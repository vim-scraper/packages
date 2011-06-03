" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/UltraBlog.vim	[[[1
693
 
" File:        UltraBlog.vim
" Description: Ultimate vim blogging plugin that manages web logs
" Author:      Lenin Lee <lenin.lee at gmail dot com>
" Version:     1.0.5
" Last Change: 2011-04-04
" License:     Copyleft.
"
" ============================================================================
" TODO: Write doc for this script
" TODO: Write a syntax file for this script
" TODO: Add a function and an option to enable users to add promote links for this script
" TODO: Optimize post list, the columns should be tidy
" TODO: Display draft|public status in post list.

if !has("python")
    finish
endif

function! SyntaxCmpl(ArgLead, CmdLine, CursorPos)
  return "markdown\nhtml\n"
endfunction

function! StatusCmpl(ArgLead, CmdLine, CursorPos)
  return "draft\npublic\n"
endfunction

function! ScopeCmpl(ArgLead, CmdLine, CursorPos)
  return "local\nremote\n"
endfunction

command! -nargs=0 UBSave exec('py ub_save_post()')
command! -nargs=0 UBPreview exec('py ub_preview()')
command! -nargs=? -complete=custom,SyntaxCmpl UBNew exec('py ub_new_post(<f-args>)')
command! -nargs=? -complete=custom,StatusCmpl UBSend exec('py ub_send_post(<f-args>)')
command! -nargs=* -complete=custom,ScopeCmpl UBList exec('py ub_list_posts(<f-args>)')
command! -nargs=* -complete=custom,ScopeCmpl UBOpen exec('py ub_open_posts(<f-args>)')
command! -nargs=* -complete=custom,ScopeCmpl UBDel exec('py ub_del_post(<f-args>)')
command! -nargs=1 -complete=file UBUpload exec('py ub_upload_media(<f-args>)')

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
except ImportError, e:
    sqlalchemy = None
except Exception:
    pass

Base = declarative_base()
Session = sessionmaker()
default_local_pagesize = 30
default_remote_pagesize = 10
if vim.eval('exists("ub_local_pagesize")') == '1':
    tmp = vim.eval('ub_local_pagesize')
    if tmp.isdigit() and int(tmp)>0:
        default_local_pagesize = int(tmp)
if vim.eval('exists("ub_remote_pagesize")') == '1':
    tmp = vim.eval('ub_remote_pagesize')
    if tmp.isdigit() and int(tmp)>0:
        default_remote_pagesize = int(tmp)

class Post(Base):
    __tablename__ = 'post'

    id = Column('id', Integer, primary_key=True)
    post_id = Column('post_id', Integer)
    title = Column('title', String(256))
    categories = Column('categories', Text)
    tags = Column('tags', Text)
    content = Column('content', Text)
    slug = Column('slug', Text)
    syntax = Column('syntax', String(64))

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
    if syntax!='markdown' and syntax!='html':
        raise UBException('Unknown syntax, only markdown and html are valid !')

    post_meta_data = dict(\
            id = str(0),
            post_id = str(0),
            title = '',
            categories = ub_get_categories(),
            tags = '',
            slug = '')

    _ub_wise_open_view('post_edit')
    _ub_fill_post_meta_data(post_meta_data)

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

def ub_get_api():
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
    if not cfg.has_key('db') or cfg['db'].strip()=='':
        cfg['db'] = os.path.normpath(os.path.expanduser('~')+'/.vim/UltraBlog.db')
    else:
        cfg['db'] = os.path.abspath(vim.eval("expand('%s')" % cfg['db']))

    return cfg

@__ub_exception_handler
def ub_save_post():
    '''Save the current buffer to local database
    '''
    # This function is valid only in 'post_edit' buffers
    if not ub_is_view('post_edit'):
        raise UBException('Invalid view !')

    # Do not bother if the current buffer is not modified
    if vim.eval('&modified')=='0':
        return

    global sqlalchemy, Session, Post
    if sqlalchemy is None:
        raise UBException('No module named sqlalchemy !')

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
    post.syntax = syntax
    sess.add(post)
    sess.commit()

    meta_dict = _ub_get_post_meta_data()
    meta_dict['id'] = post.id
    _ub_fill_post_meta_data(meta_dict)

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
        slug = ub_get_meta('slug')
    )

@__ub_exception_handler
def ub_preview():
    '''Preview the current buffer in a browser
    '''
    # This function is valid only in 'post_edit' buffers
    if not ub_is_view('post_edit'):
        raise UBException('Invalid view !')

    tmpfile = tempfile.mktemp(suffix='.html')
    fp = open(tmpfile, 'w')
    fp.write(_ub_get_html(False))
    fp.close()

    webbrowser.open("file://%s" % tmpfile)

@__ub_exception_handler
def ub_send_post(send_as='draft'):
    '''Send the current buffer to the blog
    '''
    # Check parameter
    if send_as == 'draft':
        publish = False
    elif send_as == 'public':
        publish = True
    else:
        raise UBException('Valid parameters: draft|public !')

    # Save it first
    ub_save_post()

    global cfg, api

    post = dict(\
        title = ub_get_meta('title'),
        description = _ub_get_html(),
        categories = [cat.strip() for cat in ub_get_meta('categories').split(',')],
        mt_keywords = ub_get_meta('tags'),
        wp_slug = ub_get_meta('slug')
    )

    post_id = ub_get_meta('post_id')
    if post_id is None:
        post_id = api.metaWeblog.newPost('', cfg['login_name'], cfg['password'], post, publish)
        status = "Post sent as %s !" % send_as
        meta_dict = _ub_get_post_meta_data()
        meta_dict['post_id'] = post_id
        _ub_fill_post_meta_data(meta_dict)
        ub_save_post()
    else:
        api.metaWeblog.editPost(post_id, cfg['login_name'], cfg['password'], post, publish)
        status = "Post sent as %s !" % send_as

    sys.stdout.write(status)

def _ub_get_html(body_only=True):
    '''Generate HTML string from the current buffer
    '''
    meta_dict = _ub_get_post_meta_data()
    syntax = vim.eval('&syntax')
    enc = vim.eval('&encoding')
    if syntax == 'markdown':
        html = markdown.markdown("\n".join(vim.current.buffer[len(meta_dict)+2:]).decode(enc)).encode(enc)
    else:
        html = "\n".join(vim.current.buffer[len(meta_dict)+2:])

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
def ub_list_local_posts(page_no=1, page_size=default_local_pagesize):
    '''List local posts stored in database
    '''
    if page_no<1 or page_size<1:
        return

    global Post, db
    posts = []

    tbl = Post.__table__
    ua = union_all(
        select([select([tbl.c.id,case([(tbl.c.post_id>0, tbl.c.post_id)], else_=0).label('post_id'),tbl.c.title]).where(tbl.c.post_id==None).order_by(tbl.c.id.desc())]),
        select([select([tbl.c.id,case([(tbl.c.post_id>0, tbl.c.post_id)], else_=0).label('post_id'),tbl.c.title]).where(tbl.c.post_id!=None).order_by(tbl.c.post_id.desc())])
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
    vim.current.buffer.append([("%d\t%s\t%s" % (post.id,post.post_id,post.title)).encode(enc) for post in posts])

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
def ub_list_remote_posts(num=default_remote_pagesize):
    '''List remote posts stored in the blog
    '''
    if num<1:
        return

    global cfg, api, Session, Post

    posts = api.metaWeblog.getRecentPosts('', cfg['login_name'], cfg['password'], num)
    sess = Session()
    for post in posts:
        local_post = sess.query(Post).filter(Post.post_id==post['postid']).first()
        if local_post is None:
            post['id'] = 0
        else:
            post['id'] = local_post.id
    sess.close()

    _ub_wise_open_view('remote_post_list')
    enc = vim.eval('&encoding')
    vim.current.buffer[0] = "==================== Recent Posts ===================="
    vim.current.buffer.append([("%(id)s\t%(postid)s\t%(post_status)s\t%(title)s" % post).encode(enc) for post in posts])

    vim.command("let b:page_size=%s" % num)
    vim.command('map <buffer> <enter> :py _ub_list_open_remote_post()<cr>')
    vim.command("map <buffer> <del> :py _ub_list_del_post('remote')<cr>")
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.command("setl nomodifiable")
    vim.current.window.cursor = (2, 0)

def _ub_list_open_local_post():
    '''Open local post, invoked in posts list
    '''
    if not ub_is_view('local_post_list'):
        raise UBException('Invalid view !')

    id = vim.current.line.split()[0]
    if id.isdigit():
        ub_open_local_post(int(id))

def _ub_list_open_remote_post():
    '''Open remote post, invoked in posts list
    '''
    if not ub_is_view('remote_post_list'):
        raise UBException('Invalid view !')

    id = vim.current.line.split()[1]
    if id.isdigit():
        ub_open_remote_post(int(id))

@__ub_exception_handler
def ub_open_local_post(id):
    '''Open local post
    '''
    global Session, Post
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
            slug = post.slug.encode(enc))

    _ub_wise_open_view('post_edit')
    _ub_fill_post_meta_data(post_meta_data)
    vim.current.buffer.append(post.content.encode(enc).split("\n"))

    vim.command('setl syntax=%s' % post.syntax)
    vim.command('setl wrap')
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.current.window.cursor = (len(post_meta_data)+3, 0)

@__ub_exception_handler
def ub_open_remote_post(id):
    '''Open remote post
    '''
    global Session, Post, cfg, api

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
        post.syntax = 'html'
        sess.add(post)
        sess.commit()

    enc = vim.eval('&encoding')
    post_meta_data = dict(\
            id = post.id,
            post_id = post.post_id,
            title = post.title.encode(enc),
            categories = post.categories.encode(enc),
            tags = post.tags.encode(enc),
            slug = post.slug.encode(enc))

    _ub_wise_open_view('post_edit')
    _ub_fill_post_meta_data(post_meta_data)
    vim.current.buffer.append(post.content.encode(enc).split("\n"))

    vim.command('setl syntax=%s' % post.syntax)
    vim.command('setl wrap')
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.current.window.cursor = (len(post_meta_data)+3, 0)

def ub_is_view(view_name):
    '''Check if the current view is named by the given parameter
    '''
    return vim.eval("exists('b:ub_view_name')")=='1' and vim.eval('b:ub_view_name')==view_name

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

@__ub_exception_handler
def _ub_list_del_post(scope='local'):
    '''Delete local post, invoked in posts list
    '''
    if (ub_check_scope(scope) and not ub_is_view('local_post_list')) \
            or (not ub_check_scope(scope) and not ub_is_view('remote_post_list')):
        raise UBException('Invalid view !')

    info = vim.current.line.split()
    if len(info)>=3:
        if info[0].isdigit() and int(info[0])>0:
            ub_del_post(int(info[0]),'local')
        if info[1].isdigit() and int(info[1])>0:
            ub_del_post(int(info[1]),'remote')

@__ub_exception_handler
def ub_del_post(id, scope='local'):
    '''Delete post
    '''
    if ub_check_scope(scope):
        choice = vim.eval("confirm('Are you sure to delete %s from local database ?', '&Yes\n&No')" % id)
        if choice=='1':
            global Session, Post
            sess = Session()
            sess.query(Post).filter(Post.id==id).delete()
            sess.commit()
            sess.close()
            #Refresh the list if it is in post list view
            if ub_is_view('local_post_list'):
                ub_list_posts('local', int(vim.eval('b:page_size')), int(vim.eval('b:page_no')))
            #Delete the current buffer if it contains the delete post
            if ub_is_view('post_edit') and ub_get_meta('id')==id:
                vim.command('bd!')
    else:
        choice = vim.eval("confirm('Are you sure to delete %s from the blog ?', '&Yes\n&No')" % id)
        if choice=='1':
            global cfg, api
            api.metaWeblog.deletePost('', id, cfg['login_name'], cfg['password'])
            #Refresh the list if it is in post list view
            if ub_is_view('remote_post_list'):
                ub_list_posts('remote', int(vim.eval('b:page_size')))
            #Delete the current buffer if it contains the delete post
            if ub_is_view('post_edit') and ub_get_meta('post_id')==id:
                vim.command('bd!')

@__ub_exception_handler
def ub_open_posts(id, scope='local'):
    '''Open posts by scope
    '''
    if ub_check_scope(scope):
        ub_open_local_post(int(id))
    else:
        ub_open_remote_post(int(id))

@__ub_exception_handler
def ub_upload_media(file_path):
    '''Upload a file
    '''
    if not ub_is_view('post_edit'):
        raise UBException('Invalid view !')
    if not os.path.exists(file_path):
        raise UBException('File not exists !')

    file_type = mimetypes.guess_type(file_path)[0]
    fp = open(file_path, 'r')
    bin_data = xmlrpclib.Binary(fp.read())
    fp.close()

    global cfg, api
    result = api.metaWeblog.newMediaObject('', cfg['login_name'], cfg['password'],
        dict(name=os.path.basename(file_path), type=file_type, bits=bin_data))

    vim.current.range.append(result['url'])

@__ub_exception_handler
def ub_init():
    '''Init database and other variables
    '''
    global Session, Base, db, cfg, api
    global default_local_pagesize, default_remote_pagesize

    # Get blog settings
    cfg = _ub_get_blog_settings()
    if cfg is not None:
        # Initialize database
        api = ub_get_api()
        db = sqlalchemy.create_engine("sqlite:///%s" % cfg['db'])
        Session.configure(bind=db)
        Base.metadata.create_all(db)

if __name__ == "__main__":
    ub_init()
EOF
doc/UltraBlog.txt	[[[1
235
*UltraBlog.txt*
==============================================================================
  UltraBlog                                                        *UltraBlog*
==============================================================================

Author:      Lenin Lee   lenin.lee at gmail dot com       *UltraBlog_Author*
URL:         http://sinolog.it/?p=1894                    *UltraBlog_URL*
Description: Ultimate vim blogging plugin.                |UltraBlog_Motivation|
Version:     1.0.5                                        |UltraBlog_History|
License:     Copyleft                                     |UltraBlog_License|
Contribute:  Please report any bugs or suggestions        |UltraBlog_Bugs|
             to the address above.                        |UltraBlog_Future|

==============================================================================
  Table of Contents                                        *UltraBlog_Contents*
==============================================================================

             1. Motivation.................|UltraBlog_Motivation|
             2. Operation..................|UltraBlog_Operation|
                2.1 Modes..................|UltraBlog_ScopeModes|
             3. Keymaps....................|UltraBlog_Keymaps|
             4. Commands...................|UltraBlog_Commands|
             5. Options....................|UltraBlog_Options|
             6. Bugs.......................|UltraBlog_Bugs|
             7. Future.....................|UltraBlog_Future|
             8. History....................|UltraBlog_History|
             9. License....................|UltraBlog_License|

==============================================================================
  Motivation                                             *UltraBlog_Motivation*
==============================================================================

The biggest difference between UltraBlog and other Vim blogging scripts is 
that UltraBlog stores and manages posts in a local SQLite database. In this 
way, it does not only EDIT or SEND posts to blogs but also MANAGE them 
locally. So UltraBlog is an integrated blogging client more than just a 
blog editor.

You will find UltraBlog very useful when you write posts in the Markdown 
syntax. With posts stored locally, you can edit them in MARKDOWN syntax 
at any time even after a long time when they were published, and update 
the online copies which are in HTML syntax. 

This is the exact original intention for which I wrote UltraBlog. After 
years of blogging, I've formed the habbit that writing posts in markdown 
syntax to save time, and I think it is good to always keep my posts updated, 
rather than post it and forget it. But I found it hard to update the posted 
copies after modifying the local markdown source files with other Vim blogging 
plugins. 

==============================================================================
  Operation                                               *UltraBlog_Operation*
==============================================================================

UltraBlog makes life easier writing or updating blog. It manages posts both 
locally and remotely.

To determine which type of post to manage, UltraBlog provides tow modes, 
described in the next section.


Scope Modes                                              *UltraBlog_ScopeModes*

local
    Manage posts stored in a local SQLite database.

remote
    Manage posts posted in blog.

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

:UBNew [markdown|html]
    Create a new post. If no parameter is given, UltraBlog will consider that 
    you want to use markdown syntax. All categories are pulled down from your 
    blog and you just need to delete the undesired ones.

:UBPreview
    Preview changes. You do not have to care for which syntax you use, markdown 
    source will be translated into html automatically before a browser window 
    opened to display it.

:UBSave
    Save modifications. After executing this command, the current buffer is 
    saved into database.

:UBSend [draft|public]
    Post a post. If no parameter is given, UltraBlog will send the post to blog 
    and set it to be a draft.

:UBList [local [page_size] [page_no]|remote [latest_num]]
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

:UBOpen [local|remote] {id}
    Open a post. If no scope (local or remote) is given, UltraBlog will try to 
    open a local post whose id is the one specified. Otherwise, a post in the 
    given scope with the id will be opened.

:UBDel [local|remote] {id}
    Delete a post. Refer **UBOpen** for the usage of these options.
    You can also delete posts from the post list by pressing the DELETE button on 
    the target post. In a local post list, if the post to be deleted has been 
    posted to blog, a confirmation will be prompted for you to decide whether 
    to delete the remote copy cascadly.

:UBUpload {file_path}
    Upload media. This command can only be executed in a post edit view, and the 
    URL of the uploaed file will be appended in that buffer.

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

ub_local_pagesize                                           *ub_local_pagesize*

    Default pagesize for local post list. If not specified, the default value
    is 30.

------------------------------------------------------------------------------

ub_remote_pagesize                                         *ub_remote_pagesize*

    Default pagesize for remote post list. If not specified, the default value
    is 10.

==============================================================================
  Future                                                     *UltraBlog_Future*
==============================================================================

    * Pages management.
    * Context search.

==============================================================================
  Bugs                                                         *UltraBlog_Bugs*
==============================================================================

If you discover any bugs not listed here, please contact the |UltraBlog_Author|

==============================================================================
  History                                                   *UltraBlog_History*
==============================================================================

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
