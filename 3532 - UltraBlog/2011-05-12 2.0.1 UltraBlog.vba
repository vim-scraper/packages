" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/UltraBlog.vim	[[[1
1345
 
" File:        UltraBlog.vim
" Description: Ultimate vim blogging plugin that manages web logs
" Author:      Lenin Lee <lenin.lee at gmail dot com>
" Version:     2.0.1
" Last Change: 2011-05-12
" License:     Copyleft.
"
" ============================================================================
" TODO: Write a syntax file for this script
" TODO: Context search functionality.

if !has("python")
    finish
endif

function! SyntaxCmpl(ArgLead, CmdLine, CursorPos)
  return "markdown\nhtml\nrst\ntextile\nlatex\n"
endfunction

function! StatusCmpl(ArgLead, CmdLine, CursorPos)
  return "draft\npublish\nprivate\npending\n"
endfunction

function! ScopeCmpl(ArgLead, CmdLine, CursorPos)
  return "local\nremote\n"
endfunction

command! -nargs=0 UBSave exec('py ub_save_post()')
command! -nargs=? -complete=custom,SyntaxCmpl UBNew exec('py ub_new_post(<f-args>)')
command! -nargs=? -complete=custom,StatusCmpl UBSend exec('py ub_send_post(<f-args>)')
command! -nargs=* -complete=custom,ScopeCmpl UBList exec('py ub_list_posts(<f-args>)')
command! -nargs=* -complete=custom,ScopeCmpl UBOpen exec('py ub_open_post(<f-args>)')
command! -nargs=? -complete=custom,SyntaxCmpl UBThis exec('py ub_blog_this_as_post(<f-args>)')
command! -nargs=0 UBPageSave exec('py ub_save_page()')
command! -nargs=? -complete=custom,SyntaxCmpl UBPageNew exec('py ub_new_page(<f-args>)')
command! -nargs=? -complete=custom,StatusCmpl UBPageSend exec('py ub_send_page(<f-args>)')
command! -nargs=? -complete=custom,ScopeCmpl UBPageList exec('py ub_list_pages(<f-args>)')
command! -nargs=* -complete=custom,ScopeCmpl UBPageOpen exec('py ub_open_page(<f-args>)')
command! -nargs=? -complete=custom,SyntaxCmpl UBPageThis exec('py ub_blog_this_as_page(<f-args>)')
command! -nargs=0 UBPreview exec('py ub_preview()')
command! -nargs=1 -complete=file UBUpload exec('py ub_upload_media(<f-args>)')
command! -nargs=* -complete=custom,ScopeCmpl UBDel exec('py ub_del_post(<f-args>)')
command! -nargs=* -complete=custom,SyntaxCmpl UBConv exec('py ub_convert(<f-args>)')

" Clear undo history
function! UBClearUndo()
    let old_undolevels = &undolevels
    set undolevels=-1
    exe "normal a \<BS>\<Esc>"
    let &undolevels = old_undolevels
    unlet old_undolevels
endfunction

python <<EOF
# -*- coding: utf-8 -*-
import vim, xmlrpclib, webbrowser, sys, re, tempfile, os, mimetypes

try:
    import markdown
except ImportError:
    try:
        import markdown2 as markdown
    except ImportError:
        markdown = None

try:
    import sqlalchemy
    from sqlalchemy import Table, Column, Integer, Text, String
    from sqlalchemy.ext.declarative import declarative_base
    from sqlalchemy.orm import sessionmaker
    from sqlalchemy.sql import union_all,select,case
    from sqlalchemy.exceptions import OperationalError

    Base = declarative_base()
    Session = sessionmaker()

    class Post(Base):
        __tablename__ = 'post'

        id = Column('id', Integer, primary_key=True)
        post_id = Column('post_id', Integer)
        title = Column('title', String(256))
        categories = Column('categories', Text)
        tags = Column('tags', Text)
        content = Column('content', Text)
        slug = Column('slug', Text)
        syntax = Column('syntax', String(64), nullable=False, default='markdown')
        type = Column('type', String(32), nullable=False, default='post')
        status = Column('status', String(32), nullable=False, default='draft')
except ImportError, e:
    sqlalchemy = None
    db = None
except Exception:
    pass

homepage = 'http://sinolog.it/?p=1894'
default_local_pagesize = 30
if vim.eval('exists("ub_local_pagesize")') == '1':
    tmp = vim.eval('ub_local_pagesize')
    if tmp.isdigit() and int(tmp)>0:
        default_local_pagesize = int(tmp)
default_remote_pagesize = 10
if vim.eval('exists("ub_remote_pagesize")') == '1':
    tmp = vim.eval('ub_remote_pagesize')
    if tmp.isdigit() and int(tmp)>0:
        default_remote_pagesize = int(tmp)

class UBException(Exception):
    pass

def __ub_exception_handler(func):
    def __check(*args,**kwargs):
        try:
            return func(*args,**kwargs)
        except UBException, e:
            sys.stderr.write(str(e))
        except xmlrpclib.Fault, e:
            sys.stderr.write("xmlrpc error: %s" % e.faultString.encode("utf-8"))
        except xmlrpclib.ProtocolError, e:
            sys.stderr.write("xmlrpc error: %s %s" % (e.url, e.errmsg))
        except IOError, e:
            sys.stderr.write("network error: %s" % e)
        except Exception, e:
            sys.stderr.write(str(e))
    return __check

def __ub_enc_check(func):
    def __check(*args, **kw):
        orig_enc = vim.eval("&encoding") 
        if orig_enc != "utf-8":
            modified = vim.eval("&modified")
            buf_list = '\n'.join(vim.current.buffer).decode(orig_enc).encode('utf-8').split('\n')
            del vim.current.buffer[:]
            vim.command("setl encoding=utf-8")
            vim.current.buffer[0] = buf_list[0]
            if len(buf_list) > 1:
                vim.current.buffer.append(buf_list[1:])
            if modified == '0':
                vim.command('setl nomodified')
        return func(*args, **kw)
    return __check

def _ub_wise_open_view(view_name=None):
    '''Wisely decide whether to wipe out the content of current buffer 
    or to open a new splited window.
    '''
    if vim.current.buffer.name is None and vim.eval('&modified')=='0':
        vim.command('setl modifiable')
        del vim.current.buffer[:]
        vim.command('call UBClearUndo()')
        vim.command('setl nomodified')
    else:
        vim.command(":new")

    if view_name is not None:
        vim.command("let b:ub_view_name = '%s'" % view_name)

    vim.command('mapclear <buffer>')

@__ub_exception_handler
def ub_new_post(syntax='markdown'):
    '''Initialize a buffer for writing a new post
    '''
    ub_check_syntax(syntax)

    post_meta_data = dict(\
            id = str(0),
            post_id = str(0),
            title = '',
            categories = ub_get_categories(),
            tags = '',
            slug = '',
            status = 'draft')

    _ub_wise_open_view('post_edit')
    _ub_fill_post_meta_data(post_meta_data)
    _ub_append_promotion_link(syntax)

    vim.command('setl syntax=%s' % syntax)
    vim.command('setl wrap')
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.current.window.cursor = (4, len(vim.current.buffer[3])-1)

def _ub_append_promotion_link(syntax='markdown'):
    '''Append a promotion link to the homepage of UltraBlog.vim
    '''
    global homepage

    doit = ub_get_option('ub_append_promotion_link')
    if doit is not None and doit.isdigit() and int(doit) == 1:
        if ub_is_view('post_edit') or ub_is_view('page_edit'):
            if syntax == 'markdown':
                link = 'Posted via [UltraBlog.vim](%s).' % homepage
            else:
                link = 'Posted via <a href="%s">UltraBlog.vim</a>.' % homepage
            vim.current.buffer.append(link)
        else:
            raise UBException('Invalid view !')

@__ub_exception_handler
def ub_new_page(syntax='markdown'):
    '''Initialize a buffer for writing a new page
    '''
    ub_check_syntax(syntax)

    page_meta_data = dict(\
            id = str(0),
            page_id = str(0),
            title = '',
            slug = '',
            status = 'draft')

    _ub_wise_open_view('page_edit')
    _ub_fill_page_meta_data(page_meta_data)

    vim.command('setl syntax=%s' % syntax)
    vim.command('setl wrap')
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.current.window.cursor = (4, len(vim.current.buffer[3])-1)

def _ub_fill_post_meta_data(meta_dict):
    '''Fill the current buffer with some lines of meta data for a post
    '''
    meta_text = \
"""<!--
$id:              %(id)s
$post_id:         %(post_id)s
$title:           %(title)s
$categories:      %(categories)s
$tags:            %(tags)s
$slug:            %(slug)s
$status:          %(status)s
-->""" % meta_dict
    
    meta_lines = meta_text.split('\n')
    if len(vim.current.buffer) >= len(meta_lines):
        for i in range(0,len(meta_lines)):
            vim.current.buffer[i] = meta_lines[i]
    else:
        vim.current.buffer[0] = meta_lines[0]
        vim.current.buffer.append(meta_lines[1:])

def _ub_fill_page_meta_data(meta_dict):
    '''Fill the current buffer with some lines of meta data for a page
    '''
    meta_text = \
"""<!--
$id:              %(id)s
$page_id:         %(page_id)s
$title:           %(title)s
$slug:            %(slug)s
$status:          %(status)s
-->""" % meta_dict
    
    meta_lines = meta_text.split('\n')
    if len(vim.current.buffer) >= len(meta_lines):
        for i in range(0,len(meta_lines)):
            vim.current.buffer[i] = meta_lines[i]
    else:
        vim.current.buffer[0] = meta_lines[0]
        vim.current.buffer.append(meta_lines[1:])

def ub_get_categories():
    '''Fetch categories and format them into a string
    '''
    global cfg, api

    cats = api.metaWeblog.getCategories('', cfg['login_name'], cfg['password'])
    return ', '.join([cat['description'].encode('utf-8') for cat in cats])

def _ub_get_api():
    '''Generate an API object according to the blog settings
    '''
    global cfg
    return xmlrpclib.ServerProxy(cfg['xmlrpc'])

def _ub_get_blog_settings():
    '''Get the blog settings from vimrc and raise exception if none found
    '''
    if vim.eval('exists("ub_blog")') == '0':
        #raise UBException('No blog has been set !')
        return None

    cfg = vim.eval('ub_blog')

    #Manipulate db file path
    editor_mode = ub_get_option('ub_editor_mode')
    if editor_mode is not None and editor_mode.isdigit() and int(editor_mode) == 1:
        cfg['db'] = ''
    elif not cfg.has_key('db') or cfg['db'].strip()=='':
        cfg['db'] = os.path.normpath(os.path.expanduser('~')+'/.vim/UltraBlog.db')
    else:
        cfg['db'] = os.path.abspath(vim.eval("expand('%s')" % cfg['db']))

    return cfg

@__ub_exception_handler
def ub_save_post():
    '''Save the current buffer to local database
    '''
    # Check prerequesites
    ub_check_prerequesites()

    # This function is valid only in 'post_edit' buffers
    if not ub_is_view('post_edit'):
        raise UBException('Invalid view !')

    # Do not bother if the current buffer is not modified
    if vim.eval('&modified')=='0':
        return

    # Set editor mode if the corresponding option has been set
    ub_set_mode()

    sess = Session()
    enc = vim.eval('&encoding')
    syntax = vim.eval('&syntax')

    id = ub_get_meta('id')
    post_id = ub_get_meta('post_id')
    if id is None:
        post = Post()
    else:
        post = sess.query(Post).filter(Post.id==id).first()

    meta_dict = _ub_get_post_meta_data()
    post.content = "\n".join(vim.current.buffer[len(meta_dict)+2:]).decode(enc)
    post.post_id = post_id
    post.title = ub_get_meta('title').decode(enc)
    post.categories = ub_get_meta('categories').decode(enc)
    post.tags = ub_get_meta('tags').decode(enc)
    post.slug = ub_get_meta('slug').decode(enc)
    post.status = ub_get_meta('status').decode(enc)
    post.syntax = syntax
    sess.add(post)
    sess.commit()

    meta_dict = _ub_get_post_meta_data()
    meta_dict['id'] = post.id
    _ub_fill_post_meta_data(meta_dict)

    vim.command('setl nomodified')
    sess.close()

@__ub_exception_handler
def ub_save_page():
    '''Save the current page to local database
    '''
    # Check prerequesites
    ub_check_prerequesites()

    # This function is valid only in 'page_edit' buffers
    if not ub_is_view('page_edit'):
        raise UBException('Invalid view !')

    # Do not bother if the current buffer is not modified
    if vim.eval('&modified')=='0':
        return

    # Set editor mode if the corresponding option has been set
    ub_set_mode()

    sess = Session()
    enc = vim.eval('&encoding')
    syntax = vim.eval('&syntax')

    id = ub_get_meta('id')
    page_id = ub_get_meta('page_id')
    if id is None:
        page = Post()
        page.type = 'page'
    else:
        page = sess.query(Post).filter(Post.id==id).filter(Post.type=='page').first()

    meta_dict = _ub_get_page_meta_data()
    page.content = "\n".join(vim.current.buffer[len(meta_dict)+2:]).decode(enc)
    page.post_id = page_id
    page.title = ub_get_meta('title').decode(enc)
    page.slug = ub_get_meta('slug').decode(enc)
    page.status = ub_get_meta('status').decode(enc)
    page.syntax = syntax
    sess.add(page)
    sess.commit()

    meta_dict = _ub_get_page_meta_data()
    meta_dict['id'] = page.id
    _ub_fill_page_meta_data(meta_dict)

    vim.command('setl nomodified')
    sess.close()

def ub_get_meta(item):
    '''Get value of the given item from meta data in the current buffer
    '''
    def __get_value(item, line):
        tmp = line.split(':')
        val = ':'.join(tmp[1:]).strip()
        if item.endswith('id'):
            if val.isdigit():
                val = int(val)
                if val<=0:
                    return None
            else:
                return None
        return val

    regex_meta_end = re.compile('^\s*-->')
    regex_item = re.compile('^\$'+item+':\s*')
    for line in vim.current.buffer:
        if regex_meta_end.match(line):
            break
        if regex_item.match(line):
            return __get_value(item, line)
    return None

def ub_set_meta(item, value):
    '''Set value of the given item from meta data in the current buffer
    '''
    regex_meta_end = re.compile('^\s*-->')
    regex_item = re.compile('^\$'+item+':\s*')
    for i in range(0,len(vim.current.buffer)):
        if regex_meta_end.match(vim.current.buffer[i]):
            break
        if regex_item.match(vim.current.buffer[i]):
            vim.current.buffer[i] = "$%-17s%s" % (item+':',value)
            return True
    return False

def _ub_get_post_meta_data():
    '''Get all meta data of the post and return a dict
    '''
    id = ub_get_meta('id')
    if id is None:
        id = 0
    post_id = ub_get_meta('post_id')
    if post_id is None:
        post_id = 0

    return dict(\
        id = id,
        post_id = post_id,
        title = ub_get_meta('title'),
        categories = ub_get_meta('categories'),
        tags = ub_get_meta('tags'),
        slug = ub_get_meta('slug'),
        status = ub_get_meta('status')
    )

def _ub_get_page_meta_data():
    '''Get all meta data of the page and return a dict
    '''
    id = ub_get_meta('id')
    if id is None:
        id = 0
    page_id = ub_get_meta('page_id')
    if page_id is None:
        page_id = 0

    return dict(\
        id = id,
        page_id = page_id,
        title = ub_get_meta('title'),
        slug = ub_get_meta('slug'),
        status = ub_get_meta('status')
    )

@__ub_exception_handler
def ub_preview():
    '''Preview the current buffer in a browser
    '''
    # This function is valid only in 'post_edit' buffers
    if not ub_is_view('post_edit') and not ub_is_view('page_edit'):
        raise UBException('Invalid view !')

    tmpfile = tempfile.mktemp(suffix='.html')
    fp = open(tmpfile, 'w')
    fp.write(_ub_get_html(False))
    fp.close()

    webbrowser.open("file://%s" % tmpfile)

@__ub_exception_handler
def ub_send_post(status=None):
    '''Send the current buffer to the blog
    '''
    # Check prerequesites
    ub_check_prerequesites()

    # This function is valid only in 'post_edit' buffers
    if not ub_is_view('post_edit'):
        raise UBException('Invalid view !')

    # Set editor mode if the corresponding option has been set
    ub_set_mode()

    # Check parameter
    if status is None:
        status = ub_get_meta('status')
    publish = ub_check_status(status)

    global cfg, api

    post = dict(\
        title = ub_get_meta('title'),
        description = _ub_get_html(),
        categories = [cat.strip() for cat in ub_get_meta('categories').split(',')],
        mt_keywords = ub_get_meta('tags'),
        wp_slug = ub_get_meta('slug'),
        post_type = 'post',
        post_status = status
    )

    post_id = ub_get_meta('post_id')
    if post_id is None:
        post_id = api.metaWeblog.newPost('', cfg['login_name'], cfg['password'], post, publish)
        msg = "Post sent as %s !" % status
    else:
        api.metaWeblog.editPost(post_id, cfg['login_name'], cfg['password'], post, publish)
        msg = "Post sent as %s !" % status
    sys.stdout.write(msg)

    ub_set_meta('post_id', post_id)
    ub_set_meta('status', status)

    saveit = ub_get_option('ub_save_after_sent')
    if saveit is not None and saveit.isdigit() and int(saveit) == 1:
        ub_save_post()

@__ub_exception_handler
def ub_send_page(status=None):
    '''Send the current page to the blog
    '''
    # Check prerequesites
    ub_check_prerequesites()

    # This function is valid only in 'page_edit' buffers
    if not ub_is_view('page_edit'):
        raise UBException('Invalid view !')

    # Set editor mode if the corresponding option has been set
    ub_set_mode()

    # Check parameter
    if status is None:
        status = ub_get_meta('status')
    publish = ub_check_status(status)

    global cfg, api

    page = dict(\
        title = ub_get_meta('title'),
        description = _ub_get_html(),
        wp_slug = ub_get_meta('slug'),
        post_type = 'page',
        page_status = status
    )

    page_id = ub_get_meta('page_id')
    if page_id is None:
        page_id = api.metaWeblog.newPost('', cfg['login_name'], cfg['password'], page, publish)
        msg = "Page sent as %s !" % status
    else:
        api.metaWeblog.editPost(page_id, cfg['login_name'], cfg['password'], page, publish)
        msg = "Page sent as %s !" % status
    sys.stdout.write(msg)

    ub_set_meta('page_id', page_id)
    ub_set_meta('status', status)

    saveit = ub_get_option('ub_save_after_sent')
    if saveit is not None and saveit.isdigit() and int(saveit) == 1:
        ub_save_page()

def _ub_get_content():
    '''Generate content from the current buffer
    '''
    if ub_is_view('post_edit'):
        meta_dict = _ub_get_post_meta_data()
    elif ub_is_view('page_edit'):
        meta_dict = _ub_get_page_meta_data()
    else:
        return None

    content = "\n".join(vim.current.buffer[len(meta_dict)+2:])
    return content

def _ub_set_content(lines):
    '''Set the given lines to the content area of the current buffer
    '''
    if ub_is_view('post_edit'):
        meta_dict = _ub_get_post_meta_data()
    elif ub_is_view('page_edit'):
        meta_dict = _ub_get_page_meta_data()
    else:
        return False

    del vim.current.buffer[len(meta_dict)+2:]
    vim.current.buffer.append(lines, len(meta_dict)+2)
    return True

def _ub_get_html(body_only=True):
    '''Generate HTML string from the current buffer
    '''
    content = _ub_get_content()
    syntax = vim.eval('&syntax')
    enc = vim.eval('&encoding')
    html = ub_convert('html', syntax, True)

    if not body_only:
        html = \
'''<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
    <head>
       <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    </head>
    <body>
    %s
    </body>
</html>''' % html

    return html

@__ub_exception_handler
def ub_list_posts(scope='local', page_size=None, page_no=1):
    '''List posts by scope
    '''
    if page_size is not None:
        page_size = int(page_size)
    if page_no is not None:
        page_no = int(page_no)

    if ub_check_scope(scope):
        if page_size is None:
            page_size = default_local_pagesize
        ub_list_local_posts(page_no, page_size)
    else:
        if page_size is None:
            page_size = default_remote_pagesize
        ub_list_remote_posts(page_size)

@__ub_exception_handler
def ub_list_pages(scope='local'):
    '''List pages by scope
    '''
    if ub_check_scope(scope):
        ub_list_local_pages()
    else:
        ub_list_remote_pages()

@__ub_exception_handler
def ub_list_local_posts(page_no=1, page_size=default_local_pagesize):
    '''List local posts stored in database
    '''
    # Check prerequesites
    ub_check_prerequesites()

    if page_no<1 or page_size<1:
        return

    # Set editor mode if the corresponding option has been set
    ub_set_mode()

    global db
    posts = []

    tbl = Post.__table__
    ua = union_all(
        select([select([tbl.c.id,case([(tbl.c.post_id>0, tbl.c.post_id)], else_=0).label('post_id'),tbl.c.status,tbl.c.title])\
            .where(tbl.c.post_id==None).where(tbl.c.type=='post').order_by(tbl.c.id.desc())]),
        select([select([tbl.c.id,case([(tbl.c.post_id>0, tbl.c.post_id)], else_=0).label('post_id'),tbl.c.status,tbl.c.title])\
            .where(tbl.c.post_id!=None).where(tbl.c.type=='post').order_by(tbl.c.post_id.desc())])
    )
    stmt = select([ua]).limit(page_size).offset(page_size*(page_no-1))

    conn = db.connect()
    rslt = conn.execute(stmt)
    while True:
        row = rslt.fetchone()
        if row is not None:
            posts.append(row)
        else:
            break
    conn.close()

    if len(posts)==0:
        sys.stderr.write('No more posts found !')
        return

    _ub_wise_open_view('local_post_list')
    enc = vim.eval('&encoding')
    vim.current.buffer[0] = "==================== Posts (Page %d) ====================" % page_no
    tmpl = ub_get_list_template()
    vim.current.buffer.append([(tmpl % (post.id,post.post_id,post.status,post.title)).encode(enc) for post in posts])

    vim.command("let b:page_no=%s" % page_no)
    vim.command("let b:page_size=%s" % page_size)
    vim.command('map <buffer> <enter> :py _ub_list_open_local_post()<cr>')
    vim.command("map <buffer> <del> :py _ub_list_del_post('local')<cr>")
    vim.command("map <buffer> <c-pagedown> :py ub_list_local_posts(%d,%d)<cr>" % (page_no+1,page_size))
    vim.command("map <buffer> <c-pageup> :py ub_list_local_posts(%d,%d)<cr>" % (page_no-1,page_size))
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.command("setl nomodifiable")
    vim.current.window.cursor = (2, 0)

@__ub_exception_handler
def ub_list_local_pages():
    '''List local pages stored in database
    '''
    # Check prerequesites
    ub_check_prerequesites()

    # Set editor mode if the corresponding option has been set
    ub_set_mode()

    global db
    pages = []

    tbl = Post.__table__
    ua = union_all(
        select([select([tbl.c.id,case([(tbl.c.post_id>0, tbl.c.post_id)], else_=0).label('post_id'),tbl.c.status,tbl.c.title])\
            .where(tbl.c.post_id==None).where(tbl.c.type=='page').order_by(tbl.c.id.desc())]),
        select([select([tbl.c.id,case([(tbl.c.post_id>0, tbl.c.post_id)], else_=0).label('post_id'),tbl.c.status,tbl.c.title])\
            .where(tbl.c.post_id!=None).where(tbl.c.type=='page').order_by(tbl.c.post_id.desc())])
    )

    conn = db.connect()
    rslt = conn.execute(ua)
    while True:
        row = rslt.fetchone()
        if row is not None:
            pages.append(row)
        else:
            break
    conn.close()

    if len(pages)==0:
        sys.stderr.write('No more pages found !')
        return

    _ub_wise_open_view('local_page_list')
    enc = vim.eval('&encoding')
    vim.current.buffer[0] = "==================== Local Pages ===================="
    tmpl = ub_get_list_template()
    vim.current.buffer.append([(tmpl % (page.id,page.post_id,page.status,page.title)).encode(enc) for page in pages])

    vim.command('map <buffer> <enter> :py _ub_list_open_local_post()<cr>')
    vim.command("map <buffer> <del> :py _ub_list_del_post('local')<cr>")
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.command("setl nomodifiable")
    vim.current.window.cursor = (2, 0)

@__ub_exception_handler
def ub_list_remote_posts(num=default_remote_pagesize):
    '''List remote posts stored in the blog
    '''
    # Check prerequesites
    ub_check_prerequesites()

    # Set editor mode if the corresponding option has been set
    ub_set_mode()

    if num<1:
        return

    global cfg, api

    posts = api.metaWeblog.getRecentPosts('', cfg['login_name'], cfg['password'], num)
    sess = Session()
    for post in posts:
        local_post = sess.query(Post).filter(Post.post_id==post['postid']).first()
        if local_post is None:
            post['id'] = 0
        else:
            post['id'] = local_post.id
            post['post_status'] = local_post.status
    sess.close()

    _ub_wise_open_view('remote_post_list')
    enc = vim.eval('&encoding')
    vim.current.buffer[0] = "==================== Recent Posts ===================="
    tmpl = ub_get_list_template()
    vim.current.buffer.append([(tmpl % (post['id'],post['postid'],post['post_status'],post['title'])).encode(enc) for post in posts])

    vim.command("let b:page_size=%s" % num)
    vim.command('map <buffer> <enter> :py _ub_list_open_remote_post()<cr>')
    vim.command("map <buffer> <del> :py _ub_list_del_post('remote')<cr>")
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.command("setl nomodifiable")
    vim.current.window.cursor = (2, 0)

@__ub_exception_handler
def ub_list_remote_pages():
    '''List remote pages stored in the blog
    '''
    # Check prerequesites
    ub_check_prerequesites()

    # Set editor mode if the corresponding option has been set
    ub_set_mode()

    global cfg, api

    sess = Session()
    pages = api.wp.getPages('', cfg['login_name'], cfg['password'])
    for page in pages:
        local_page = sess.query(Post).filter(Post.post_id==page['page_id']).filter(Post.type=='page').first()
        if local_page is None:
            page['id'] = 0
        else:
            page['id'] = local_page.id
            page['page_status'] = local_page.status
    sess.close()

    _ub_wise_open_view('remote_page_list')
    enc = vim.eval('&encoding')
    vim.current.buffer[0] = "==================== Blog Pages ===================="
    tmpl = ub_get_list_template()
    vim.current.buffer.append([(tmpl % (page['id'],page['page_id'],page['page_status'],page['title'])).encode(enc) for page in pages])

    vim.command('map <buffer> <enter> :py _ub_list_open_remote_post()<cr>')
    vim.command("map <buffer> <del> :py _ub_list_del_post('remote')<cr>")
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.command("setl nomodifiable")
    vim.current.window.cursor = (2, 0)

def _ub_list_open_local_post():
    '''Open local post, invoked in post or page list
    '''
    parts = vim.current.line.split()
    if len(parts)>=2 and parts[0].isdigit():
        id = int(parts[0])
        if ub_is_view('local_post_list'):
            ub_open_local_post(id)
        elif ub_is_view('local_page_list'):
            ub_open_local_page(id)
        else:
            raise UBException('Invalid view !')

def _ub_list_open_remote_post():
    '''Open remote post, invoked in post or page list
    '''
    parts = vim.current.line.split()
    if len(parts)>=2 and parts[1].isdigit():
        id = int(parts[1])
        if ub_is_view('remote_post_list'):
            ub_open_remote_post(id)
        elif ub_is_view('remote_page_list'):
            ub_open_remote_page(id)
        else:
            raise UBException('Invalid view !')

@__ub_exception_handler
def ub_open_local_post(id):
    '''Open local post
    '''
    # Check prerequesites
    ub_check_prerequesites()

    # Set editor mode if the corresponding option has been set
    ub_set_mode()

    sess = Session()
    post = sess.query(Post).filter(Post.id==id).first()
    if post is None:
        raise UBException('No post found !')

    post_id = post.post_id
    if post_id is None:
        post_id = 0

    enc = vim.eval('&encoding')
    post_meta_data = dict(\
            id = post.id,
            post_id = post_id,
            title = post.title.encode(enc),
            categories = post.categories.encode(enc),
            tags = post.tags.encode(enc),
            slug = post.slug.encode(enc),
            status = post.status.encode(enc))

    _ub_wise_open_view('post_edit')
    _ub_fill_post_meta_data(post_meta_data)
    vim.current.buffer.append(post.content.encode(enc).split("\n"))

    vim.command('setl syntax=%s' % post.syntax)
    vim.command('setl wrap')
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.current.window.cursor = (len(post_meta_data)+3, 0)

@__ub_exception_handler
def ub_open_local_page(id):
    '''Open local page
    '''
    # Check prerequesites
    ub_check_prerequesites()

    # Set editor mode if the corresponding option has been set
    ub_set_mode()

    sess = Session()
    page = sess.query(Post).filter(Post.id==id).filter(Post.type=='page').first()
    if page is None:
        raise UBException('No page found !')

    page_id = page.post_id
    if page_id is None:
        page_id = 0

    enc = vim.eval('&encoding')
    page_meta_data = dict(\
            id = page.id,
            page_id = page_id,
            title = page.title.encode(enc),
            slug = page.slug.encode(enc),
            status = page.status.encode(enc))

    _ub_wise_open_view('page_edit')
    _ub_fill_page_meta_data(page_meta_data)
    vim.current.buffer.append(page.content.encode(enc).split("\n"))

    vim.command('setl syntax=%s' % page.syntax)
    vim.command('setl wrap')
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.current.window.cursor = (len(page_meta_data)+3, 0)

@__ub_exception_handler
def ub_open_remote_post(id):
    '''Open remote post
    '''
    # Check prerequesites
    ub_check_prerequesites()

    # Set editor mode if the corresponding option has been set
    ub_set_mode()

    global cfg, api

    sess = Session()
    post = sess.query(Post).filter(Post.post_id==id).first()
    # Fetch the remote post if there is not a local copy
    if post is None:
        remote_post = api.metaWeblog.getPost(id, cfg['login_name'], cfg['password'])
        post = Post()
        post.post_id = id
        post.title = remote_post['title']
        post.content = remote_post['description']
        post.categories = ', '.join(remote_post['categories'])
        post.tags = remote_post['mt_keywords']
        post.slug = remote_post['wp_slug']
        post.status = remote_post['post_status']
        post.syntax = 'html'

        saveit = ub_get_option('ub_save_after_opened')
        if saveit is not None and saveit.isdigit() and int(saveit) == 1:
            sess.add(post)
            sess.commit()

    id = post.id
    if post.id is None:
        id = 0
    enc = vim.eval('&encoding')
    post_meta_data = dict(\
            id = id,
            post_id = post.post_id,
            title = post.title.encode(enc),
            categories = post.categories.encode(enc),
            tags = post.tags.encode(enc),
            slug = post.slug.encode(enc),
            status = post.status.encode(enc))

    _ub_wise_open_view('post_edit')
    _ub_fill_post_meta_data(post_meta_data)
    vim.current.buffer.append(post.content.encode(enc).split("\n"))

    vim.command('setl syntax=%s' % post.syntax)
    vim.command('setl wrap')
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.current.window.cursor = (len(post_meta_data)+3, 0)

@__ub_exception_handler
def ub_open_remote_page(id):
    '''Open remote page
    '''
    # Check prerequesites
    ub_check_prerequesites()

    # Set editor mode if the corresponding option has been set
    ub_set_mode()

    global cfg, api

    sess = Session()
    page = sess.query(Post).filter(Post.post_id==id).filter(Post.type=='page').first()
    # Fetch the remote page if there is not a local copy
    if page is None:
        remote_page = api.wp.getPage('', id, cfg['login_name'], cfg['password'])
        page = Post()
        page.type = 'page'
        page.post_id = id
        page.title = remote_page['title']
        page.content = remote_page['description']
        page.slug = remote_page['wp_slug']
        page.status = remote_page['page_status']
        page.syntax = 'html'

        saveit = ub_get_option('ub_save_after_opened')
        if saveit is not None and saveit.isdigit() and int(saveit) == 1:
            sess.add(page)
            sess.commit()

    id = page.id
    if page.id is None:
        id = 0
    enc = vim.eval('&encoding')
    page_meta_data = dict(\
            id = id,
            page_id = page.post_id,
            title = page.title.encode(enc),
            slug = page.slug.encode(enc),
            status = page.status.encode(enc))

    _ub_wise_open_view('page_edit')
    _ub_fill_page_meta_data(page_meta_data)
    vim.current.buffer.append(page.content.encode(enc).split("\n"))

    vim.command('setl syntax=%s' % page.syntax)
    vim.command('setl wrap')
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.current.window.cursor = (len(page_meta_data)+3, 0)

@__ub_exception_handler
def _ub_list_del_post(scope='local'):
    '''Delete local post, invoked in posts list
    '''
    if (ub_check_scope(scope) and (not ub_is_view('local_post_list') and not ub_is_view('local_page_list'))) \
            or (not ub_check_scope(scope) and (not ub_is_view('remote_post_list') and not ub_is_view('remote_page_list'))):
        raise UBException('Invalid view !')

    info = vim.current.line.split()
    if len(info)>=3:
        if info[0].isdigit() and int(info[0])>0:
            ub_del_post(int(info[0]),'local')
        if info[1].isdigit() and int(info[1])>0:
            ub_del_post(int(info[1]),'remote')

@__ub_exception_handler
def ub_del_post(id, scope='local'):
    '''Delete post or page
    '''
    # Check prerequesites
    ub_check_prerequesites()

    # Set editor mode if the corresponding option has been set
    ub_set_mode()

    id = int(id)
    if ub_check_scope(scope):
        choice = vim.eval("confirm('Are you sure to delete %s from local database ?', '&Yes\n&No')" % id)
        if choice=='1':
            sess = Session()
            sess.query(Post).filter(Post.id==id).delete()
            sess.commit()
            sess.close()

            #Refresh the list if it is in post list view
            if ub_is_view('local_post_list'):
                ub_list_posts('local', int(vim.eval('b:page_size')), int(vim.eval('b:page_no')))
            if ub_is_view('local_page_list'):
                ub_list_pages('local')
            #Delete the current buffer if it contains the deleted post
            if (ub_is_view('post_edit') or ub_is_view('page_edit')) and ub_get_meta('id')==id:
                vim.command('bd!')
    else:
        choice = vim.eval("confirm('Are you sure to delete %s from the blog ?', '&Yes\n&No')" % id)
        if choice=='1':
            global cfg, api
            if ub_is_view('local_post_list') or ub_is_view('remote_post_list'):
                api.metaWeblog.deletePost('', id, cfg['login_name'], cfg['password'])
            else:
                api.wp.deletePage('', cfg['login_name'], cfg['password'], id)

            #Refresh the list if it is in post list view
            if ub_is_view('remote_post_list'):
                ub_list_posts('remote', int(vim.eval('b:page_size')))
            if ub_is_view('remote_page_list'):
                ub_list_pages('remote')
            #Delete the current buffer if it contains the deleted post
            if (ub_is_view('post_edit') or ub_is_view('page_edit')) and ub_get_meta('post_id')==id:
                vim.command('bd!')

@__ub_exception_handler
def ub_open_post(id, scope='local'):
    '''Open posts by scope
    '''
    if ub_check_scope(scope):
        ub_open_local_post(int(id))
    else:
        ub_open_remote_post(int(id))

@__ub_exception_handler
def ub_open_page(id, scope='local'):
    '''Open page by scope
    '''
    if ub_check_scope(scope):
        ub_open_local_page(int(id))
    else:
        ub_open_remote_page(int(id))

@__ub_exception_handler
def ub_upload_media(file_path):
    '''Upload a file
    '''
    if not ub_is_view('post_edit'):
        raise UBException('Invalid view !')
    if not os.path.exists(file_path):
        raise UBException('File not exists !')

    file_type = mimetypes.guess_type(file_path)[0]
    fp = open(file_path, 'rb')
    bin_data = xmlrpclib.Binary(fp.read())
    fp.close()

    global cfg, api
    result = api.metaWeblog.newMediaObject('', cfg['login_name'], cfg['password'],
        dict(name=os.path.basename(file_path), type=file_type, bits=bin_data))

    vim.current.range.append(result['url'])

@__ub_exception_handler
def ub_blog_this(syntax=None, type='post'):
    '''Create a new post/page with content in the current buffer
    '''
    if syntax is None:
        syntax = vim.eval('&syntax')
    try:
        ub_check_syntax(syntax)
    except:
        syntax = 'markdown'

    bf = vim.current.buffer[:]

    if type == 'post':
        ub_new_post(syntax)
    else:
        ub_new_page(syntax)

    regex_meta_end = re.compile('^\s*-->')
    for line_num in range(0, len(vim.current.buffer)):
        line = vim.current.buffer[line_num]
        if regex_meta_end.match(line):
            break
    vim.current.buffer.append(bf, line_num+1)

@__ub_exception_handler
def ub_convert(to_syntax, from_syntax=None, literal=False):
    '''Convert the current buffer from one syntax to another
    '''
    ub_check_syntax(to_syntax)
    if from_syntax is None:
        from_syntax = vim.eval('&syntax')
    ub_check_syntax(from_syntax)

    content = _ub_get_content()

    if from_syntax == to_syntax:
        return content

    enc = vim.eval('&encoding')
    if from_syntax == 'markdown' and to_syntax == 'html':
        new_content = markdown.markdown(content.decode(enc)).encode(enc)
    else:
        cmd_parts = []
        cmd_parts.append(ub_get_option('ub_converter_command'))
        cmd_parts.extend(ub_get_option('ub_converter_options'))
        try:
            cmd_parts.append(ub_get_option('ub_converter_option_from') % from_syntax)
            cmd_parts.append(ub_get_option('ub_converter_option_to') % to_syntax)
        except TypeError:
            pass
        import subprocess
        p = subprocess.Popen(cmd_parts, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
        new_content = p.communicate(content)[0].replace("\r\n", "\n")

    if literal == True:
        return new_content
    else:
        _ub_set_content(new_content.split("\n"))
        vim.command('setl syntax=%s' % to_syntax)

@__ub_exception_handler
def ub_blog_this_as_post(syntax=None):
    ub_blog_this(syntax, 'post')

@__ub_exception_handler
def ub_blog_this_as_page(syntax=None):
    ub_blog_this(syntax, 'page')

def ub_is_view(view_name):
    '''Check if the current view is named by the given parameter
    '''
    return vim.eval("exists('b:ub_view_name')")=='1' and vim.eval('b:ub_view_name')==view_name

def ub_get_option(opt):
    '''Get the value of an UltraBlog option
    '''
    if vim.eval('exists("%s")' % opt) == '1':
        val = vim.eval(opt)
    elif opt == 'ub_converter_command':
        val = 'pandoc'
    elif opt == 'ub_converter_option_from':
        val = '--from=%s'
    elif opt == 'ub_converter_option_to':
        val = '--to=%s'
    elif opt == 'ub_converter_options':
        val = ['--reference-links']
    else:
        val = None

    return val

def ub_check_scope(scope):
    '''Check the given scope,
    return True if it is local,
    return False if it is remote,
    raise an exception if it is neither of the upper two
    '''
    if scope=='local':
        return True
    elif scope=='remote':
        return False
    else:
        raise UBException('Invalid scope !')

def ub_check_status(status):
    '''Check if the given status is valid,
    return True if status is publish
    '''
    if status == 'publish':
        return True
    elif status in ['private', 'pending', 'draft']:
        return False
    else:
        raise UBException('Invalid status !')

def ub_check_prerequesites():
    '''Check prerequesites
    '''
    if sqlalchemy is None:
        raise UBException('No module named sqlalchemy !')

    if markdown is None:
        raise UBException('No module named markdown or markdown2 !')

def ub_check_syntax(syntax):
    '''Check syntax
    '''
    valid_syntax = ['markdown', 'html', 'rst', 'textile', 'latex']
    if syntax.lower() not in valid_syntax:
        raise UBException('Unknown syntax, valid syntaxes are %s' % str(valid_syntax))

def ub_get_list_template():
    '''Return a template string for post or page list
    '''
    col1_width = 10
    tmp = ub_get_option('ub_list_col1_width')
    if tmp is not None and tmp.isdigit() and int(tmp)>0:
        col1_width = int(tmp)

    col2_width = 10
    tmp = ub_get_option('ub_list_col2_width')
    if tmp is not None and tmp.isdigit() and int(tmp)>0:
        col2_width = int(tmp)

    col3_width = 10
    tmp = ub_get_option('ub_list_col3_width')
    if tmp is not None and tmp.isdigit() and int(tmp)>0:
        col3_width = int(tmp)

    tmpl = "%%-%ds%%-%ds%%-%ds%%s"

    tmpl = tmpl % (col1_width,col2_width,col3_width)

    return tmpl

@__ub_exception_handler
def ub_set_mode():
    '''Set editor mode according to the option ub_editor_mode
    '''
    editor_mode = ub_get_option('ub_editor_mode')
    if editor_mode is not None and editor_mode.isdigit() and int(editor_mode) == 1:
        ub_init()

@__ub_exception_handler
def ub_init():
    '''Init database and other variables
    '''
    global db, cfg, api
    global default_local_pagesize, default_remote_pagesize

    # Get blog settings
    cfg = _ub_get_blog_settings()
    if cfg is not None and sqlalchemy is not None:
        # Initialize database
        api = _ub_get_api()
        db = sqlalchemy.create_engine("sqlite:///%s" % cfg['db'])
        Session.configure(bind=db)
        Base.metadata.create_all(db)

@__ub_exception_handler
def ub_upgrade():
    global db

    if db is not None:
        conn = db.connect()
        stmt = select([Post.type]).limit(1)
        try:
            result = conn.execute(stmt)
        except OperationalError:
            sql = "alter table post add type varchar(32) not null default 'post'"
            conn.execute(sql)

        stmt = select([Post.status]).limit(1)
        try:
            result = conn.execute(stmt)
        except OperationalError:
            sql = "alter table post add status varchar(32) not null default 'draft'"
            conn.execute(sql)

        conn.close()

if __name__ == "__main__":
    ub_init()
    ub_upgrade()
EOF
doc/UltraBlog.txt	[[[1
419
*UltraBlog.txt*
==============================================================================
  UltraBlog                                                        *UltraBlog*
==============================================================================

Author:      Lenin Lee   lenin.lee at gmail dot com       *UltraBlog_Author*
URL:         http://sinolog.it/?p=1894                    *UltraBlog_URL*
Description: Ultimate vim blogging plugin.                |UltraBlog_Motivation|
Version:     2.0.1                                        |UltraBlog_History|
License:     Copyleft                                     |UltraBlog_License|
Contribute:  Please report any bugs or suggestions        |UltraBlog_Bugs|
             to the address above.                        |UltraBlog_Future|

==============================================================================
  Table of Contents                                        *UltraBlog_Contents*
==============================================================================

             1. Motivation.................|UltraBlog_Motivation|
             2. Operation..................|UltraBlog_Operation|
                2.1 Modes..................|UltraBlog_Modes|
                2.2 Scopes.................|UltraBlog_Scopes|
             3. Prerequisites..............|UltraBlog_Prerequisites|
             4. Keymaps....................|UltraBlog_Keymaps|
             5. Commands...................|UltraBlog_Commands|
             6. Options....................|UltraBlog_Options|
             7. Bugs.......................|UltraBlog_Bugs|
             8. Future.....................|UltraBlog_Future|
             9. History....................|UltraBlog_History|
            10. License....................|UltraBlog_License|

==============================================================================
  Motivation                                             *UltraBlog_Motivation*
==============================================================================

The biggest difference between UltraBlog.vim and other Vim blogging scripts is 
that UltraBlog.vim supplies two modes: the first one is editor mode, in which 
users can manage posts and pages in their blogs, just like other Vim blogging
scripts; the second is client mode, in which posts and pages are stored in a 
local SQLite database, in this way, it does not only OPERATE on posts/pages in 
blogs but also MANAGE them locally. So UltraBlog.vim is an integrated blogging 
client more than just a blog editor.

You will find UltraBlog.vim very useful when you write posts in some light 
weight markup languages, such as Markdown, LaTeX, reStructuredText and Textile. 
With posts/pages stored locally, you can edit them in any light weight markup
language at any time even after a long time when they were published, and update 
the online copies in HTML syntax. 

This is the exact original intention for which I wrote UltraBlog.vim. After 
years of blogging, I've formed the habbit that writing posts in Markdown 
syntax to save time, and I think it is good to always keep my posts updated, 
rather than post it and forget it. But I found it hard to update the posted 
copies after modifying the local Markdown source files with other Vim blogging 
plugins. 

==============================================================================
  Operation                                               *UltraBlog_Operation*
==============================================================================

UltraBlog.vim makes life easier while writing or updating blogs. It stores
posts/pages in a local SQLite database. You can also set it to editor mode, in
which UltraBlog.vim does not store data locally, just like other Vim blogging 
scripts.

By default, UltraBlog.vim is in client mode. You can set it to use editor mode
by adding the fallowing line to the vimrc file:

Modes                                                          *UltraBlog_Modes*

    let ub_editor_mode = 1

To determine whether to operate on local data or remote data, UltraBlog.vim 
provides tow scopes:

Scopes                                                        *UltraBlog_Scopes*

    local
        Manage posts/pages stored in a local SQLite database.

    remote
        Manage posts/pages in the blog.

==============================================================================
  Prerequisites                                        *UltraBlog_Prerequisites*
==============================================================================

UltraBlog.vim requires the following softwares to work normally:

    * Vim with python support enabled.
    * SQLAlchemy
    + python-markdown or python-markdown2
    + pandoc or other

The asterisk items are required at any condition.

One of the two markdown modules of python is needed if you want to convert 
posts/pages from Markdown to HTML.

pandoc is requried when you need to do conversions between any pair of the 
avaliable syntaxes. You can also specify you own external command as the 
converter. For more information, refer to help messages of options below.

==============================================================================
  Keymaps                                                   *UltraBlog_Keymaps*
==============================================================================

<c-pageup>    - Shift to previous page in local post list.
<c-pagedown>  - Shift to next page in local post list.
<del>         - Delete the post under cursor in post list.
<enter>       - Open the post under cursor in post list, if is in remote post
                list, posts which are not in local database will be saved to 
                it before opened.

==============================================================================
  Commands                                                 *UltraBlog_Commands*
==============================================================================

:UBNew [markdown\|html\|rst\|textile\|latex]                            *:UBNew*
:UBPageNew [markdown\|html\|rst\|textile\|latex]                    *:UBPageNew*
    Create a new post. If no parameter is given, UltraBlog will consider that 
    you want to use markdown syntax. All categories are pulled down from your 
    blog and you just need to delete the undesired ones.

:UBPreview                                                         *:UBPreview*
    Preview changes. You do not have to care for which syntax you use, markdown 
    source will be translated into html automatically before a browser window 
    opened to display it.

:UBSave                                                               *:UBSave*
:UBPageSave                                                       *:UBPageSave*
    Save modifications. After executing this command, the current buffer is 
    saved into database.

:UBSend [draft\|publish\|private\|pending]                            *:UBSend*
:UBPageSend [draft\|publish\|private\|pending]                    *:UBPageSend*
    Post a post. If no parameter is given, UltraBlog will send the post to 
    blog and set it to be the value stored in the meta information area.

:UBList [local [page_size] [page_no]|remote [latest_num]]             *:UBList*
:UBPageList [local|remote]                                        *:UBPageList*
    List posts.

    For example:

    :UBList

    This command lists the first page of local posts, by default, posts which 
    have not been posted to blog are listed before the posted ones, and there 
    are |ub_local_pagesize| posts a page.

    :UBList local 20 3

    This command lists the third page of local posts, 20 posts a page. As you see, 
    you can use this command to scroll forward or back between pages. As a matter 
    of fact, there are two key mappings within local post list:

      * CTRL+PageDown
      * CTRL+PageUp

    :UBList remote 50

    This command lists the latest 50 posts in the blog.

    Pressing the ENTER key in a remote post list will open the post under cursor 
    and save it to the local database if it is not in it, otherwise, the local 
    copy will be opened instead of the remote one. This enables users to modify 
    markdown source and update the remote post.

    The remote post list doesn't support paging.

:UBOpen {id} [local|remote]                                           *:UBOpen*
:UBPageOpen {id} [local|remote]                                   *:UBPageOpen*
    Open a post. If no scope (local or remote) is given, UltraBlog will try to 
    open a local post whose id is the one specified. Otherwise, a post in the 
    given scope with the id will be opened.

:UBDel {id} [local|remote]                                             *:UBDel*
    Delete a post or page. Refer |UBOpen| for the usage of these options.
    You can also delete posts from the post list by pressing the DELETE button on 
    the target post. In a local post list, if the post to be deleted has been 
    posted to blog, a confirmation will be prompted for you to decide whether 
    to delete the remote copy cascadly.

:UBUpload {file_path}                                               *:UBUpload*
    Upload media. This command can only be executed in a post edit view, and the 
    URL of the uploaed file will be appended in that buffer.

:UBThis [markdown\|html\|rst\|textile\|latex]                         *:UBThis*
:UBPageThis [markdown\|html\|rst\|textile\|latex]                 *:UBPageThis*
    Create a new post or page which is filled with content in the current 
    buffer. If no parameter is specified, the syntax in the current buffer is
    used, if no syntax has been set to the current buffer, then 'markdown' is
    used.

:UBConv {to_syntax} [from_syntax]                                      *:UBConv*
    Convert the current buffer from 'from_syntax' to 'to_syntax'.
    The available syntaxes are Markdown, HTML, reStructuredText, Textile and
    LaTeX.
    If you only need to convert from Markdown to HTML, only python-markdown or
    python-markdown2 module is required. For other conversion scenarios, you 
    must install pacdoc or use the options: |ub_converter_command|, 
    |ub_converter_options|, |ub_converter_option_from|,
    |ub_converter_option_to| to specify a valid external command.

==============================================================================
  Options                                                   *UltraBlog_Options*
==============================================================================

ub_blog                                                              *ub_blog*

    Example:
        let ub_blog = {'login_name':'admin',
                    \'password':'pass2011',
                    \'xmlrpc':'http://www.sample.com/xmlrpc.php',
                    \'db':'$VIM/UltraBlog.db'
                    \}
    Keys:
        db
            Use any path string that can be expanded as a normal fullpath by
            the function 'expand()'.

------------------------------------------------------------------------------

ub_editor_mode                                                 *ub_editor_mode*

    Set this option to 1 will force UltraBlog.vim not to store data in a local
    SQLite database. You can still use most of the functions UltraBlog.vim
    supplies, especially those which are used to manage blogs remotely.
    
    In one word, this mode works just like other Vim blogging scripts.

------------------------------------------------------------------------------

ub_local_pagesize                                           *ub_local_pagesize*

    Default pagesize for local post list. If not specified, the default value
    is 30.

------------------------------------------------------------------------------

ub_remote_pagesize                                         *ub_remote_pagesize*

    Default pagesize for remote post list. If not specified, the default value
    is 10.

------------------------------------------------------------------------------

ub_append_promotion_link                             *ub_append_promotion_link*

    If this option has been set to 1, a promotion link will be appended at the
    end of current buffer after |:UBNew| is executed.

    Proudly show your visitors that you are blogging with the world's most 
    powerful editor.

------------------------------------------------------------------------------

ub_list_col1_width                                         *ub_list_col1_width*
ub_list_col2_width                                         *ub_list_col2_width*
ub_list_col3_width                                         *ub_list_col3_width*

    Column widths of post or page lists.
    col1 for the local id column, whose default value is 10.
    col2 for the remote id column, whose default value is 10.
    col3 for the status column, whose default value is 10.

------------------------------------------------------------------------------

ub_save_after_opend                                       *ub_save_after_opened*
ub_save_after_sent                                          *ub_save_after_sent*

    If the above options are set to 1, posts/pages will be saved to database
    immediately after they are opened from or sent to the blog.

    By default, their values are 0s.

------------------------------------------------------------------------------

ub_converter_command                                      *ub_converter_command*

    This is the command needed to do conversions between the avaliable
    syntaxes.

    By default, the value is:

        'pandoc'

------------------------------------------------------------------------------

ub_converter_options                                      *ub_converter_options*

    A list of options to be appended to the |ub_converter_command|.

    By default, the value is:

        ['--reference-links']

------------------------------------------------------------------------------

ub_converter_option_from                              *ub_converter_option_from*

    A template string which will be filled with the 'from_syntax' and appended
    to the |ub_converter_command|.

    By default, the value is:

        '--from=%s'

------------------------------------------------------------------------------

ub_converter_option_to                                  *ub_converter_option_to*

    A template string which will be filled with the 'to_syntax' and appended
    to the |ub_converter_command|.

    By default, the value is:

        '--to=%s'

==============================================================================
  Future                                                     *UltraBlog_Future*
==============================================================================

    * Context search.

==============================================================================
  Bugs                                                         *UltraBlog_Bugs*
==============================================================================

If you discover any bugs not listed here, please contact the |UltraBlog_Author|

==============================================================================
  History                                                   *UltraBlog_History*
==============================================================================

2.0.1
    * Bugfix:  Uploading media files may fail on Windows.
    * Bugfix:  An exception may be raised when Vim starts with no proper 
               configuration for UltraBlog.vim or some prerequisites are not
               met.

2.0
    * Feature: Add editor mode.
    * Feature: Add more syntaxes: LaTeX, reStructuredText and Textile.
    * Feature: Add command |:UBConv| to convert a post/page from one syntax to
               another.
    * Feature: Add options |ub_converter_command|, |ub_converter_options|, 
               |ub_converter_option_from|, |ub_converter_option_to| to specify 
               a custom extenal command in place of pandoc as the converter.
    * Feature: Add option |ub_save_after_opened| to control whether to save 
               posts/pages to database right after they are fetched from
               blogs.
    * Feature: Add option |ub_save_after_sent| to control whether to save
               posts/pages to database right after they are sent to blogs.

1.4
    * Feature: Add two commands: |:UBThis| and |:UBPageThis|, to create a new post 
               or page with content in the current buffer.

1.3
    * Feature: Record post statuses, post them in their recorded statuses when 
               omit parameters for |:UBSend| and |:UBPageSend|.
    * Feature: Display post/page status in post/page list, add an option 
               |ub_list_col3_width| to set its width.
    * Feature: Extend statuses to support private, pending and draft.
    * Feature: Change the status 'public' to 'publish'.
    * Feature: Check prerequesites in every top level functions.
    * BugFix:  Do not raise an error on startup when sqlalchemy is not installed.

1.2
    * Feature: Column widths can be set to custom values, which keep post or 
               page lists tidy. Two options added: |ub_list_col1_width| and 
               |ub_list_col2_width|.

1.1
    * Feature: Page management.
    * Feature: Add option |ub_append_promotion_link| to control whether to
               append a promotion link of UltraBlog.vim.
    * Bugfix:  Wrong order for parameters of commands |:UBOpen| and |:UBDel|
               in documentation.

1.0.5
    * Feature: Add documentation.
    * Feature: Use Vimball as the default distribution format.
    * Feature: Add two options - |ub_local_pagesize| and |ub_remote_pagesize|.

1.0.4
    * Fix the problem that an unexpected type exception is raised when UBList 
      is given some numbers as parameters.

1.0.3
    * Add a column in remote post list the same as the one in local post list, 
      which is used to display ids of local copies of corresponding posts.
    * Fix the problem that buffers can be reverted to what the previous views 
      contain throught the undo action.
    * Fix the minor problem that text won't wrap automatically in views opened 
      by the command :UBNew.

1.0.2
    * Fix the problem that Post.post_id won't be updated in database after a 
      new post is sent to blog.
    * Fix the problem that it prompts for confirmation to delete the remote copy 
      when deleting a post whose post_id is 0 from local post list.

1.0.1
    * Do not pop up an exception alert for the first time when Vim starts with 
      UltraBlog.vim installed and no configurations added to the vimrc file.

1.0
    * Initial version

==============================================================================
  License                                                   *UltraBlog_License*
==============================================================================

Copyleft 2011

==============================================================================
vim:tw=78:ts=4:ft=help:norl:fdm=marker
