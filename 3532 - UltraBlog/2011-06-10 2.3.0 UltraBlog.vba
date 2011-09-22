" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/UltraBlog.vim	[[[1
1942
 
" File:        UltraBlog.vim
" Description: Ultimate vim blogging plugin that manages web logs
" Author:      Lenin Lee <lenin.lee at gmail dot com>
" Version:     2.3.0
" Last Change: 2011-06-10
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

function! UBNewCmpl(ArgLead, CmdLine, CursorPos)
    let lst = split(a:CmdLine)
    if len(a:ArgLead)>0
        let lst = lst[0:-2]
    endif

    let results = []
    " For the first argument, complete the object type
    if len(lst)==1
        let objects = ['post','page','tmpl']
        for obj in objects
            if stridx(obj,a:ArgLead)==0
                call add(results,obj)
            endif
        endfor
    " For the second argument, complete the syntax for :UBNew post or :UBNew
    " page
    elseif len(lst)==2 && count(['post', 'page'], lst[1])==1
        let syntaxes = ['markdown','html','rst','textile','latex']
        for synx in syntaxes
            if stridx(synx,a:ArgLead)==0
                call add(results,synx)
            endif
        endfor
    endif
    return results
endfunction

function! UBOpenCmpl(ArgLead, CmdLine, CursorPos)
    let lst = split(a:CmdLine)
    if len(a:ArgLead)>0
        let lst = lst[0:-2]
    endif

    let results = []
    " For the first argument, complete the object type
    if len(lst)==1
        let objects = ['post','page','tmpl']
        for obj in objects
            if stridx(obj, a:ArgLead)==0
                call add(results, obj)
            endif
        endfor
    " For the third argument, complete the scope
    elseif len(lst)==3
        let scopes = ['local', 'remote']
        for scope in scopes
            if stridx(scope, a:ArgLead)==0
                call add(results, scope)
            endif
        endfor
    endif
    return results
endfunction

function! UBListCmpl(ArgLead, CmdLine, CursorPos)
    let lst = split(a:CmdLine)
    if len(a:ArgLead)>0
        let lst = lst[0:-2]
    endif

    let results = []
    " For the first argument, complete the object type
    if len(lst)==1
        let objects = ['post','page','tmpl']
        for obj in objects
            if stridx(obj, a:ArgLead)==0
                call add(results, obj)
            endif
        endfor
    " For the second argument, complete the scope
    elseif len(lst)==2 && count(['post', 'page'], lst[1])==1
        let scopes = ['local', 'remote']
        for scope in scopes
            if stridx(scope, a:ArgLead)==0
                call add(results, scope)
            endif
        endfor
    endif
    return results
endfunction

function! UBDelCmpl(ArgLead, CmdLine, CursorPos)
    let lst = split(a:CmdLine)
    if len(a:ArgLead)>0
        let lst = lst[0:-2]
    endif

    let results = []
    " For the first argument, complete the object type
    if len(lst)==1
        let objects = ['post','page','tmpl']
        for obj in objects
            if stridx(obj, a:ArgLead)==0
                call add(results, obj)
            endif
        endfor
    " For the third argument, complete the scope
    elseif len(lst)==3 && count(['post', 'page'], lst[1])==1
        let scopes = ['local', 'remote']
        for scope in scopes
            if stridx(scope, a:ArgLead)==0
                call add(results, scope)
            endif
        endfor
    endif
    return results
endfunction

function! UBThisCmpl(ArgLead, CmdLine, CursorPos)
    let lst = split(a:CmdLine)
    if len(a:ArgLead)>0
        let lst = lst[0:-2]
    endif

    let results = []
    " For the first argument, complete the object type
    if len(lst)==1
        let objects = ['post','page']
        for obj in objects
            if stridx(obj, a:ArgLead)==0
                call add(results, obj)
            endif
        endfor
    " For the second argument, complete the scope
    elseif len(lst)==2 && count(['post', 'page'], lst[1])==1
        let syntaxes = ['markdown','html','rst','textile','latex']
        for synx in syntaxes
            if stridx(synx,a:ArgLead)==0
                call add(results,synx)
            endif
        endfor
    endif
    return results
endfunction

function! UBPreviewCmpl(ArgLead, CmdLine, CursorPos)
python <<EOF
templates = ub_get_templates(True)
vim.command('let b:ub_templates=%s' % str(templates))
EOF
    let tmpls = ['publish', 'private', 'draft']
    if exists('b:ub_templates')
        call extend(tmpls, b:ub_templates)
    endif
    return join(tmpls, "\n")
endfunction

command! -nargs=* -complete=customlist,UBListCmpl UBList exec('py ub_list_items(<f-args>)')
command! -nargs=* -complete=customlist,UBNewCmpl UBNew exec('py ub_new_item(<f-args>)')
command! -nargs=* -complete=customlist,UBOpenCmpl UBOpen exec('py ub_open_item(<f-args>)')
command! -nargs=* -complete=customlist,UBDelCmpl UBDel exec('py ub_del_item(<f-args>)')
command! -nargs=? -complete=custom,StatusCmpl UBSend exec('py ub_send_item(<f-args>)')
command! -nargs=? -complete=customlist,UBThisCmpl UBThis exec('py ub_blog_this(<f-args>)')
command! -nargs=? -complete=custom,UBPreviewCmpl UBPreview exec('py ub_preview(<f-args>)')
command! -nargs=0 UBSave exec('py ub_save_item()')
command! -nargs=1 -complete=file UBUpload exec('py ub_upload_media(<f-args>)')
command! -nargs=* -complete=custom,SyntaxCmpl UBConv exec('py ub_convert(<f-args>)')

" Clear undo history
function! UBClearUndo()
    let old_undolevels = &undolevels
    set undolevels=-1
    exe "normal a \<BS>\<Esc>"
    let &undolevels = old_undolevels
    unlet old_undolevels
endfunction

" Open the item under cursor in list views
function! UBOpenItemUnderCursor(viewType)
    if s:UBIsView('local_post_list') || s:UBIsView('local_page_list') || s:UBIsView('remote_page_list') || s:UBIsView('remote_post_list') || s:UBIsView('template_list')
        exe 'py _ub_list_open_item("'.a:viewType.'")'
    endif
endfunction

" Check if the current buffer is named with the given name
function! s:UBIsView(viewName)
    return exists('b:ub_view_name') && b:ub_view_name==a:viewName
endfunction

python <<EOF
# -*- coding: utf-8 -*-
import vim, xmlrpclib, webbrowser, sys, re, tempfile, os, mimetypes, types

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
    from sqlalchemy.exc import OperationalError

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

    class Template(Base):
        __tablename__ = 'template'

        name = Column('name', String(32), primary_key=True)
        description = Column('description', String(256))
        content = Column('content', Text)

except ImportError, e:
    sqlalchemy = None
    db = None
    print e
except Exception:
    pass

homepage = 'http://sinolog.it/?p=1894'

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

def _ub_wise_open_view(view_name=None, view_type=None):
    '''Wisely decide whether to wipe out the content of current buffer 
    or to open a new splitted window or a new tab.
    '''
    if view_type == 'tab':
        vim.command(":tabnew")
    elif view_type == 'split':
        vim.command(":new")
    elif vim.current.buffer.name is None and vim.eval('&modified')=='0':
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
def ub_new_item(item_type='post', mixed='markdown'):
    ''' Create new item: post, page, template
    '''
    ub_check_item_type(item_type)
    
    if item_type=='post' or item_type=='page':
        ub_check_syntax(mixed)

    eval("ub_new_%s('%s')" % (item_type,mixed))

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
    _ub_fill_meta_data(post_meta_data)
    _ub_append_promotion_link(syntax)

    vim.command('setl filetype=%s' % syntax)
    vim.command('setl wrap')
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.current.window.cursor = (4, len(vim.current.buffer[3])-1)

    return True

@__ub_exception_handler
def ub_new_page(syntax='markdown'):
    '''Initialize a buffer for writing a new page
    '''
    ub_check_syntax(syntax)

    page_meta_data = dict(\
            id = str(0),
            post_id = str(0),
            title = '',
            slug = '',
            status = 'draft')

    _ub_wise_open_view('page_edit')
    _ub_fill_meta_data(page_meta_data)

    vim.command('setl filetype=%s' % syntax)
    vim.command('setl wrap')
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.current.window.cursor = (4, len(vim.current.buffer[3])-1)

    return True

@__ub_exception_handler
def ub_new_tmpl(name):
    '''Initialize a buffer for creating a template
    '''
    # Check if the given name is a reserved word
    try:
        ub_check_status(name)
    except UBException:
        pass
    else:
        raise UBException("'%s' is a reserved word !" % name)

    # Check if the given name is already existing
    enc = vim.eval('&encoding')
    sess = Session()
    if sess.query(Template).filter(Template.name==name.decode(enc)).first() is not None:
        sess.close()
        raise UBException('Template "%s" exists !' % name)

    meta_data = dict(\
            name = name,
            description = '')

    _ub_wise_open_view('template_edit')
    _ub_fill_meta_data(meta_data)
    _ub_append_template_framework()

    vim.command('setl filetype=html')
    vim.command('setl nowrap')
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.current.window.cursor = (3, len(vim.current.buffer[2])-1)

def _ub_append_template_framework():
    fw = \
'''<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
        <title>%(title)s</title>
        <style>
        </style>
    </head>
    <body>
        %(content)s
    </body>
</html>'''
    lines = fw.split("\n")
    vim.current.buffer.append(lines)

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
def _ub_fill_meta_data(meta_data):
    if ub_is_view('post_edit'):
        _ub_fill_post_meta_data(meta_data)
    elif ub_is_view('page_edit'):
        _ub_fill_page_meta_data(meta_data)
    elif ub_is_view('template_edit'):
        _ub_fill_tmpl_meta_data(meta_data)
    else:
        raise UBException('Unknown view !')

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
$post_id:         %(post_id)s
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

def _ub_fill_tmpl_meta_data(meta_dict):
    '''Fill the current buffer with some lines of meta data for a template
    '''
    meta_text = \
"""<!--
$name:            %(name)s
$description:     %(description)s
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

    # Manipulate db file path
    editor_mode = ub_get_option('ub_editor_mode')
    if editor_mode is not None and editor_mode.isdigit() and int(editor_mode) == 1:
        cfg['db'] = ''
    elif not cfg.has_key('db') or cfg['db'].strip()=='':
        cfg['db'] = os.path.normpath(os.path.expanduser('~')+'/.vim/UltraBlog.db')
    else:
        cfg['db'] = os.path.abspath(vim.eval("expand('%s')" % cfg['db']))

    # Manipulate blog URL
    if ub_is_url(cfg['xmlrpc']):
        url_parts = cfg['xmlrpc'].split('/')
        url_parts.pop()
        cfg['blog_url'] = '/'.join(url_parts)
    else:
        cfg['blog_url'] = ''

    return cfg

@__ub_exception_handler
def ub_save_item():
    '''Save the current buffer to local database
    '''
    if ub_is_view('post_edit'):
        ub_save_post()
    elif ub_is_view('page_edit'):
        ub_save_page()
    elif ub_is_view('template_edit'):
        ub_save_template()
    else:
        raise UBException('Invalid view !')

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
    meta_dict['id'] = post.id
    sess.close()

    _ub_fill_meta_data(meta_dict)

    vim.command('setl nomodified')

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
    post_id = ub_get_meta('post_id')
    if id is None:
        page = Post()
        page.type = 'page'
    else:
        page = sess.query(Post).filter(Post.id==id).filter(Post.type=='page').first()

    meta_dict = _ub_get_page_meta_data()
    page.content = "\n".join(vim.current.buffer[len(meta_dict)+2:]).decode(enc)
    page.post_id = post_id
    page.title = ub_get_meta('title').decode(enc)
    page.slug = ub_get_meta('slug').decode(enc)
    page.status = ub_get_meta('status').decode(enc)
    page.syntax = syntax
    sess.add(page)
    sess.commit()
    meta_dict['id'] = page.id
    sess.close()

    _ub_fill_meta_data(meta_dict)

    vim.command('setl nomodified')

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
    post_id = ub_get_meta('post_id')
    if post_id is None:
        post_id = 0

    return dict(\
        id = id,
        post_id = post_id,
        title = ub_get_meta('title'),
        slug = ub_get_meta('slug'),
        status = ub_get_meta('status')
    )

@__ub_exception_handler
def ub_preview(tmpl=None):
    '''Preview the current buffer in a browser
    '''
    if not ub_is_view('post_edit') and not ub_is_view('page_edit'):
        raise UBException('Invalid view !')

    global cfg
    prv_url = ''
    enc = vim.eval('&encoding')

    if tmpl in ['private', 'publish', 'draft']:
        ub_send_item(tmpl)

        if ub_is_view('page_edit'):
            prv_url = "%s?pageid=%s&preview=true"
        else:
            prv_url = "%s?p=%s&preview=true"

        prv_url = prv_url % (cfg['blog_url'], ub_get_meta('post_id'))
    else:
        if tmpl is None:
            tmpl = ub_get_option('ub_default_template')

        sess = Session()
        template = sess.query(Template).filter(Template.name==tmpl.decode(enc)).first()
        sess.close()
        if template is None:
            raise UBException("Template '%s' is not found !" % tmpl)

        tmpl_str = template.content.encode(enc)

        draft = {}
        draft['title'] = ub_get_meta('title')
        draft['content'] = _ub_get_html()

        tmpfile = tempfile.mktemp(suffix='.html')
        fp = open(tmpfile, 'w')
        fp.write(tmpl_str % draft)
        fp.close()
        prv_url = "file://%s" % tmpfile

    webbrowser.open(prv_url)

@__ub_exception_handler
def ub_save_template():
    '''Save the current template to local database
    '''
    # Check prerequesites
    ub_check_prerequesites()

    # This function is valid only in 'template_edit' buffers
    if not ub_is_view('template_edit'):
        raise UBException('Invalid view !')

    # Do not bother if the current buffer is not modified
    if vim.eval('&modified')=='0':
        return

    # Set editor mode if the corresponding option has been set
    ub_set_mode()

    sess = Session()
    enc = vim.eval('&encoding')
    syntax = vim.eval('&syntax')

    name = ub_get_meta('name').decode(enc)
    tmpl = sess.query(Template).filter(Template.name==name).first()
    if tmpl is None:
        tmpl = Template()
        tmpl.name = name

    tmpl.description = ub_get_meta('description').decode(enc)
    tmpl.content = "\n".join(vim.current.buffer[4:]).decode(enc)

    # Check if the given name is a reserved word
    try:
        ub_check_status(tmpl.name)
    except UBException:
        pass
    else:
        raise UBException("'%s' is a reserved word !" % tmpl.name)

    sess.add(tmpl)
    sess.commit()
    sess.close()

    vim.command('setl nomodified')

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
def ub_send_item(status=None):
    '''Send the current item to the blog
    '''
    if ub_is_view('post_edit'):
        ub_send_post(status)
    elif ub_is_view('page_edit'):
        ub_send_page(status)
    else:
        raise UBException('Invalid view !')

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

    post_id = ub_get_meta('post_id')
    if post_id is None:
        post_id = api.metaWeblog.newPost('', cfg['login_name'], cfg['password'], page, publish)
        msg = "Page sent as %s !" % status
    else:
        api.metaWeblog.editPost(post_id, cfg['login_name'], cfg['password'], page, publish)
        msg = "Page sent as %s !" % status
    sys.stdout.write(msg)

    ub_set_meta('post_id', post_id)
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
def ub_list_items(item_type='post', scope='local', page_size=None, page_no=1):
    ub_check_item_type(item_type)

    if item_type=='tmpl':
        ub_list_templates()
        return

    ub_check_scope(scope)

    if page_size is None:
        page_size = ub_get_option("ub_%s_pagesize" % scope)
    page_size = int(page_size)
    page_no = int(page_no)
    if page_no<1 or page_size<1:
        return

    if item_type=='post':
        if scope=='local':
            ub_list_local_posts(page_no, page_size)
        else:
            ub_list_remote_posts(page_size)
    else:
        eval("ub_list_%s_pages()" % scope)

@__ub_exception_handler
def ub_list_local_posts(page_no=1, page_size=None):
    '''List local posts stored in database
    '''
    # Check prerequesites
    ub_check_prerequesites()

    if page_size is None:
        page_size = ub_get_option('ub_local_pagesize')
    page_size = int(page_size)
    page_no = int(page_no)
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
    vim.command("map <buffer> "+ub_get_option('ub_hotkey_open_item_in_current_view')+" :call UBOpenItemUnderCursor('cur')<cr>")
    vim.command("map <buffer> "+ub_get_option('ub_hotkey_open_item_in_splitted_view')+" :call UBOpenItemUnderCursor('split')<cr>")
    vim.command("map <buffer> "+ub_get_option('ub_hotkey_open_item_in_tabbed_view')+" :call UBOpenItemUnderCursor('tab')<cr>")
    vim.command("map <buffer> "+ub_get_option('ub_hotkey_delete_item')+" :py _ub_list_del_item()<cr>")
    vim.command("map <buffer> "+ub_get_option('ub_hotkey_pagedown')+" :py ub_list_local_posts(%d,%d)<cr>" % (page_no+1,page_size))
    vim.command("map <buffer> "+ub_get_option('ub_hotkey_pageup')+" :py ub_list_local_posts(%d,%d)<cr>" % (page_no-1,page_size))
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

    vim.command("map <buffer> "+ub_get_option('ub_hotkey_open_item_in_current_view')+" :call UBOpenItemUnderCursor('cur')<cr>")
    vim.command("map <buffer> "+ub_get_option('ub_hotkey_open_item_in_splitted_view')+" :call UBOpenItemUnderCursor('split')<cr>")
    vim.command("map <buffer> "+ub_get_option('ub_hotkey_open_item_in_tabbed_view')+" :call UBOpenItemUnderCursor('tab')<cr>")
    vim.command("map <buffer> "+ub_get_option('ub_hotkey_delete_item')+" :py _ub_list_del_item()<cr>")
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.command("setl nomodifiable")
    vim.current.window.cursor = (2, 0)

@__ub_exception_handler
def ub_list_remote_posts(page_size=None):
    '''List remote posts stored in the blog
    '''
    # Check prerequesites
    ub_check_prerequesites()

    # Set editor mode if the corresponding option has been set
    ub_set_mode()

    if page_size is None:
        page_size = ub_get_option('ub_remote_pagesize')
    page_size = int(page_size)
    page_no = int(page_no)
    if page_no<1 or page_size<1:
        return

    if page_size<1:
        return

    global cfg, api

    posts = api.metaWeblog.getRecentPosts('', cfg['login_name'], cfg['password'], page_size)
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

    vim.command("let b:page_size=%s" % page_size)
    vim.command("map <buffer> "+ub_get_option('ub_hotkey_open_item_in_current_view')+" :call UBOpenItemUnderCursor('cur')<cr>")
    vim.command("map <buffer> "+ub_get_option('ub_hotkey_open_item_in_splitted_view')+" :call UBOpenItemUnderCursor('split')<cr>")
    vim.command("map <buffer> "+ub_get_option('ub_hotkey_open_item_in_tabbed_view')+" :call UBOpenItemUnderCursor('tab')<cr>")
    vim.command("map <buffer> "+ub_get_option('ub_hotkey_delete_item')+" :py _ub_list_del_item()<cr>")
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

    vim.command("map <buffer> "+ub_get_option('ub_hotkey_open_item_in_current_view')+" :call UBOpenItemUnderCursor('cur')<cr>")
    vim.command("map <buffer> "+ub_get_option('ub_hotkey_open_item_in_splitted_view')+" :call UBOpenItemUnderCursor('split')<cr>")
    vim.command("map <buffer> "+ub_get_option('ub_hotkey_open_item_in_tabbed_view')+" :call UBOpenItemUnderCursor('tab')<cr>")
    vim.command("map <buffer> "+ub_get_option('ub_hotkey_delete_item')+" :py _ub_list_del_item()<cr>")
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.command("setl nomodifiable")
    vim.current.window.cursor = (2, 0)

def ub_get_templates(name_only=False):
    # Check prerequesites
    ub_check_prerequesites()

    # Set editor mode if the corresponding option has been set
    ub_set_mode()

    enc = vim.eval('&encoding')

    sess = Session()
    tmpls = sess.query(Template).all()
    sess.close()

    if name_only is True:
        tmpls = [tmpl.name.encode(enc) for tmpl in tmpls]

    return tmpls

def ub_list_templates():
    '''List preview templates
    '''
    # Check prerequesites
    ub_check_prerequesites()

    # Set editor mode if the corresponding option has been set
    ub_set_mode()

    sess = Session()

    tmpls = sess.query(Template).all()

    if len(tmpls)==0:
        sys.stderr.write('No template found !')
        return

    _ub_wise_open_view('template_list')
    enc = vim.eval('&encoding')
    vim.current.buffer[0] = "==================== Templates ===================="
    line = "%-24s%s"
    vim.current.buffer.append([(line % (tmpl.name,tmpl.description)).encode(enc) for tmpl in tmpls])

    vim.command("map <buffer> "+ub_get_option('ub_hotkey_open_item_in_current_view')+" :call UBOpenItemUnderCursor('cur')<cr>")
    vim.command("map <buffer> "+ub_get_option('ub_hotkey_open_item_in_splitted_view')+" :call UBOpenItemUnderCursor('split')<cr>")
    vim.command("map <buffer> "+ub_get_option('ub_hotkey_open_item_in_tabbed_view')+" :call UBOpenItemUnderCursor('tab')<cr>")
    vim.command("map <buffer> "+ub_get_option('ub_hotkey_delete_item')+" :py _ub_list_del_item()<cr>")
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.command("setl nomodifiable")
    vim.current.window.cursor = (2, 0)

@__ub_exception_handler
def _ub_list_open_item(view_type=None):
    '''Open the item under cursor, invoked in post or page list
    '''
    parts = vim.current.line.split()
    if ub_is_cursorline_valid('template'):
        ub_open_template(parts[0], view_type)
    elif ub_is_cursorline_valid('general'):
        if ub_is_view('local_post_list'):
            id = int(parts[0])
            ub_open_local_post(id, view_type)
        elif ub_is_view('local_page_list'):
            id = int(parts[0])
            ub_open_local_page(id, view_type)
        elif ub_is_view('remote_post_list'):
            id = int(parts[1])
            ub_open_remote_post(id, view_type)
        elif ub_is_view('remote_page_list'):
            id = int(parts[1])
            ub_open_remote_page(id, view_type)
        else:
            raise UBException('Invalid view !')

@__ub_exception_handler
def ub_open_local_post(id, view_type=None):
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

    _ub_wise_open_view('post_edit', view_type)
    _ub_fill_meta_data(post_meta_data)
    vim.current.buffer.append(post.content.encode(enc).split("\n"))

    vim.command('setl filetype=%s' % post.syntax)
    vim.command('setl wrap')
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.current.window.cursor = (len(post_meta_data)+3, 0)

@__ub_exception_handler
def ub_open_local_page(id, view_type=None):
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

    post_id = page.post_id
    if post_id is None:
        post_id = 0

    enc = vim.eval('&encoding')
    page_meta_data = dict(\
            id = page.id,
            post_id = post_id,
            title = page.title.encode(enc),
            slug = page.slug.encode(enc),
            status = page.status.encode(enc))

    _ub_wise_open_view('page_edit', view_type)
    _ub_fill_meta_data(page_meta_data)
    vim.current.buffer.append(page.content.encode(enc).split("\n"))

    vim.command('setl filetype=%s' % page.syntax)
    vim.command('setl wrap')
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.current.window.cursor = (len(page_meta_data)+3, 0)

@__ub_exception_handler
def ub_open_remote_post(id, view_type=None):
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

    _ub_wise_open_view('post_edit', view_type)
    _ub_fill_meta_data(post_meta_data)
    vim.current.buffer.append(post.content.encode(enc).split("\n"))

    vim.command('setl filetype=%s' % post.syntax)
    vim.command('setl wrap')
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.current.window.cursor = (len(post_meta_data)+3, 0)

@__ub_exception_handler
def ub_open_remote_page(id, view_type=None):
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
            post_id = page.post_id,
            title = page.title.encode(enc),
            slug = page.slug.encode(enc),
            status = page.status.encode(enc))

    _ub_wise_open_view('page_edit', view_type)
    _ub_fill_meta_data(page_meta_data)
    vim.current.buffer.append(page.content.encode(enc).split("\n"))

    vim.command('setl filetype=%s' % page.syntax)
    vim.command('setl wrap')
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.current.window.cursor = (len(page_meta_data)+3, 0)

@__ub_exception_handler
def ub_open_item(item_type, key, scope='local'):
    ''' Open item
    '''
    ub_check_item_type(item_type)

    if item_type=='tmpl':
        ub_open_template(key)
        return

    ub_check_scope(scope)
    eval("ub_open_%s_%s(%s)" % (scope, item_type, key))

@__ub_exception_handler
def ub_open_template(name, view_type=None):
    '''Open template
    '''
    # Check prerequesites
    ub_check_prerequesites()

    # Set editor mode if the corresponding option has been set
    ub_set_mode()

    enc = vim.eval('&encoding')
    name = name.decode(enc)

    sess = Session()
    tmpl = sess.query(Template).filter(Template.name==name).first()
    if tmpl is None:
        raise UBException('No template found !')

    meta_data = dict(\
            name = tmpl.name.encode(enc),
            description = tmpl.description.encode(enc))

    _ub_wise_open_view('template_edit', view_type)
    _ub_fill_meta_data(meta_data)
    vim.current.buffer.append(tmpl.content.encode(enc).split("\n"))

    vim.command('setl filetype=html')
    vim.command('setl nowrap')
    vim.command('call UBClearUndo()')
    vim.command('setl nomodified')
    vim.current.window.cursor = (len(meta_data)+3, 0)

def ub_is_cursorline_valid(line_type):
    ''' Check if the cursor line is a normal item line,
    valid types are 'template', 'post', 'page', 'general'
    '''
    parts = vim.current.line.split()
    if line_type=='template':
        return ub_is_view('template_list') and vim.current.window.cursor[0]>1 and len(parts)>0
    else:
        is_general_line = vim.current.window.cursor[0]>1 and len(parts)>=3 and parts[0].isdigit() and parts[1].isdigit()
        if line_type=='general':
            return is_general_line
        elif line_type=='post':
            return (ub_is_view('local_post_list') or ub_is_view('remote_post_list')) and is_general_line
        elif line_type=='page':
            return (ub_is_view('local_page_list') or ub_is_view('remote_page_list')) and is_general_line
        elif line_type=='local':
            return (ub_is_view('local_page_list') or ub_is_view('local_post_list')) and is_general_line
        elif line_type=='remote':
            return (ub_is_view('remote_page_list') or ub_is_view('remote_post_list')) and is_general_line
        else:
            return False

@__ub_exception_handler
def _ub_list_del_item():
    '''Delete local post, invoked in list view
    '''
    info = vim.current.line.split()

    if ub_is_cursorline_valid('template'):
        ub_del_item('tmpl', info[0])
    elif ub_is_cursorline_valid('post') or ub_is_cursorline_valid('page'):
        view_name_parts = vim.eval('b:ub_view_name').split('_')
        if int(info[0])>0:
            ub_del_item(view_name_parts[1], int(info[0]), 'local')
        if int(info[1])>0:
            ub_del_item(view_name_parts[1], int(info[1]), 'remote')
    else:
        raise UBException('Invalid view !')

@__ub_exception_handler
def ub_del_item(item_type, key, scope='local'):
    '''Delete an item
    '''
    # Check prerequesites
    ub_check_prerequesites()

    ub_check_item_type(item_type)

    # Set editor mode if the corresponding option has been set
    ub_set_mode()

    enc = vim.eval('&encoding')

    # Delete template
    if item_type=='tmpl':
        choice = vim.eval("confirm('Are you sure to delete template \"%s\" ?', '&Yes\n&No')" % key)
        if choice=='1':
            sess = Session()
            sess.query(Template).filter(Template.name==key.decode(enc)).delete()
            sess.commit()
            sess.close()
            # Refresh the list if it is in template list
            if ub_is_view('template_list'):
                ub_list_templates()
            # Delete the current buffer if it contains the deleted template
            if ub_is_view('template_edit') and key==ub_get_meta('name'):
                vim.command('bd!')
        return

    # Delete posts or pages
    ub_check_scope(scope)
    id = int(key)
    choice = vim.eval("confirm('Are you sure to delete %s from the %s side ?', '&Yes\n&No')" % (id,scope))
    if choice!='1':
        return

    if scope=='remote':
        global cfg, api
        if item_type=='page':
            api.wp.deletePage('', cfg['login_name'], cfg['password'], id)
        else:
            api.metaWeblog.deletePost('', id, cfg['login_name'], cfg['password'])
    else:
        sess = Session()
        sess.query(Post).filter(Post.id==id).delete()
        sess.commit()
        sess.close()

    # Refresh the current view if it is in item list page
    if ub_is_view("%s_%s_list" % (scope, item_type)):
        if vim.eval("exists('b:page_size')")=='1' and vim.eval('b:page_size').isdigit() and int(vim.eval("b:page_size"))>0:
            page_size = int(vim.eval('b:page_size'))
        else:
            page_size = int(ub_get_option("ub_%s_pagesize" % scope))
        if vim.eval("exists('b:page_no')")=='1' and vim.eval('b:page_no').isdigit() and int(vim.eval("b:page_no"))>0:
            page_size = int(vim.eval('b:page_size'))
        else:
            page_size = 1

        ub_list_items(item_type, scope, page_size, page_no)

    # Delete the current buffer if it contains the deleted item
    if (ub_is_view('%s_edit' % item_type) and (\
        (scope=='local' and ub_get_meta('id')==id)\
        or (scope=='remote' and ub_get_meta('post_id')==id))):
        vim.command('bd!')

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

    img_tmpl_info = ub_get_option('ub_tmpl_img_url', True)
    img_url = img_tmpl_info['tmpl'] % result
    syntax = vim.eval('&syntax')
    img_url = _ub_convert_str(img_url, img_tmpl_info['syntax'], syntax)
    vim.current.range.append(img_url.split("\n"))

@__ub_exception_handler
def ub_blog_this(type='post', syntax=None):
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
        success = ub_new_post(syntax)
    else:
        success = ub_new_page(syntax)

    if success is True:
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
    enc = vim.eval('&encoding')
    new_content = _ub_convert_str(content, from_syntax, to_syntax, enc)

    if literal == True:
        return new_content
    else:
        _ub_set_content(new_content.split("\n"))
        vim.command('setl filetype=%s' % to_syntax)

@__ub_exception_handler
def _ub_convert_str(content, from_syntax, to_syntax, encoding=None):
    if from_syntax == to_syntax \
        or not ub_is_valid_syntax(from_syntax) \
        or not ub_is_valid_syntax(to_syntax):
        return content

    if from_syntax == 'markdown' and to_syntax == 'html':
        if encoding is not None:
            new_content = markdown.markdown(content.decode(encoding)).encode(encoding)
        else:
            new_content = markdown.markdown(content)
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
    return new_content

def ub_is_view(view_name):
    '''Check if the current view is named by the given parameter
    '''
    return vim.eval("exists('b:ub_view_name')")=='1' and vim.eval('b:ub_view_name')==view_name

def ub_get_option(opt, deal=False):
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
    elif opt == 'ub_hotkey_open_item_in_current_view':
        val = '<enter>'
    elif opt == 'ub_hotkey_open_item_in_splitted_view':
        val = '<s-enter>'
    elif opt == 'ub_hotkey_open_item_in_tabbed_view':
        val = '<c-enter>'
    elif opt == 'ub_hotkey_delete_item':
        val = '<del>'
    elif opt == 'ub_hotkey_pagedown':
        val = '<c-pagedown>'
    elif opt == 'ub_hotkey_pageup':
        val = '<c-pageup>'
    elif opt == 'ub_tmpl_img_url':
        val = "markdown###![%(file)s][]\n[%(file)s]:%(url)s"
    elif opt == 'ub_local_pagesize':
        val = 30
    elif opt == 'ub_remote_pagesize':
        val = 10
    elif opt == 'ub_default_template':
        val = 'default'
    else:
        val = None

    if deal:
        if opt == 'ub_tmpl_img_url':
            tmp = val.split('###')
            val = {'tmpl':'', 'syntax':''}
            if len(tmp) == 2:
                val['syntax'] = tmp[0]
                val['tmpl'] = tmp[1]
            else:
                val['syntax'] = ''
                val['tmpl'] = tmp[0]

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
    if sqlalchemy is None:
        raise UBException('SQLAlchemy v0.7 or newer is required !')

    if markdown is None:
        raise UBException('No module named markdown or markdown2 !')

def ub_check_syntax(syntax):
    valid_syntax = ['markdown', 'html', 'rst', 'textile', 'latex']
    if syntax.lower() not in valid_syntax:
        raise UBException('Unknown syntax, valid syntaxes are %s' % str(valid_syntax))

def ub_check_item_type(item_type):
    if not item_type in ['post', 'page', 'tmpl']:
        raise UBException('Unknow item type, available types are: post, page and tmpl !')

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

def ub_is_valid_syntax(syntax):
    '''Check if the given parameter is one of the supported syntaxes
    '''
    return ['markdown', 'html', 'rst', 'latex', 'textile'].count(syntax) == 1

def ub_is_url(url):
    ''' Check if the given string is a valid URL
    '''
    regex = re.compile('^http:\/\/[0-9a-zA-Z]+(\.[0-9a-zA-Z]+)+')
    return regex.match(url) is not None

@__ub_exception_handler
def ub_init():
    '''Init database and other variables
    '''
    global db, cfg, api

    # Get blog settings
    cfg = _ub_get_blog_settings()
    if cfg is not None and sqlalchemy is not None:
        # Initialize database
        api = _ub_get_api()
        db = sqlalchemy.create_engine("sqlite:///%s" % cfg['db'])
        Session.configure(bind=db)
        Base.metadata.create_all(db)
        _ub_init_template()

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

def _ub_init_template():
    ub_check_prerequesites()
    ub_set_mode()

    sess = Session()
    tmpl = sess.query(Template).filter(Template.name=='default').first()
    if tmpl is None:
        tmpl = Template()
        tmpl.name = 'default'
        tmpl.description = 'The default template for previewing drafts.'
        tmpl.content = \
'''<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
        <title>%(title)s</title>
        <style>
            body
            {
                font-family: "DejaVu Sans YuanTi","YaHei Consolas Hybrid","Microsoft YaHei";
                font-size: 14px;
                background-color: #D9DADC;
            }

            code
            {
                font-family: "Monaco","YaHei Consolas Hybrid";
                border: 1px solid #333;
                background-color: #DCDCDC;
                padding: 0px 3px;
                margin: 0px 5px;
            }

            pre
            {
                font-family: "Monaco","YaHei Consolas Hybrid";
                border: 1px solid #333;
                background-color: #B7D0DB;
                padding: 10px;
            }

            blockquote {border: 1px dashed #333; background-color: #B7D0DB; padding: 10px;}

            .container {width: 80%%;margin:0px auto;padding:20px;background-color: #FFFFFF;}
            .title {font-size: 24px; font-weight: bold;}
            .content {}
        </style>
    </head>
    <body>
        <div class="container">
            <div class="title">%(title)s</div>
            <div class="content">
                %(content)s
            </div>
        </div>
    </body>
</html>'''
        sess.add(tmpl)
        sess.commit()
        sess.close()

if __name__ == "__main__":
    ub_init()
    ub_upgrade()
EOF
doc/UltraBlog.txt	[[[1
618
*UltraBlog.txt*
==============================================================================
  UltraBlog                                                        *UltraBlog*
==============================================================================

Author:      Lenin Lee   lenin.lee at gmail dot com       *UltraBlog_Author*
URL:         http://sinolog.it/?p=1894                    *UltraBlog_URL*
Description: Ultimate vim blogging plugin.                |UltraBlog_Motivation|
Version:     2.3.0                                        |UltraBlog_History|
License:     Copyleft                                     |UltraBlog_License|
Contribute:  Please report any bugs or suggestions        |UltraBlog_Bugs|
             to the address above.                        |UltraBlog_Future|

==============================================================================
  Table of Contents                                        *UltraBlog_Contents*
==============================================================================

             1. Motivation.................|UltraBlog_Motivation|
             2. Concepts...................|UltraBlog_Concepts|
                2.1 Modes..................|UltraBlog_Modes|
                2.2 Items..................|UltraBlog_Items|
                2.3 Syntaxes...............|UltraBlog_Syntaxes|
                2.4 Scopes.................|UltraBlog_Scopes|
                2.5 Status.................|UltraBlog_Statuses|
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
  Concepts                                                  *UltraBlog_Concepts*
==============================================================================

UltraBlog.vim makes life easier while writing or updating blogs. It stores
posts/pages in a local SQLite database. You can also set it to editor mode, in
which UltraBlog.vim does not store data locally, just like other Vim blogging 
scripts.

------------------------------------------------------------------------------

By default, UltraBlog.vim is in client mode. You can set it to use editor mode
by adding the fallowing line to the vimrc file:

  Modes                                                        *UltraBlog_Modes*

    let ub_editor_mode = 1

------------------------------------------------------------------------------

  Items                                                        *UltraBlog_Items*

Currently, UltraBlog.vim manages three items: post, page and tmpl. "tmpl" 
is the shorthand of "template".

                                                            *UltraBlog_Template*

Templates are used to preview the current post/page in the browser locally.
This feature is a reparation for the remote previewing, due to the limit of
the API, users cannot send a post to Wordpress as draft and preview it without
affecting the post status if the post has been published.

With templates, users can preview posts/pages directly in the browser in a 
pre-defined style. They can create as many templates as they like and
customize the look with CSS and HTML, or even Javascript.

Templates should be formatted as a valid python template string, that is, use
the following avaliable placeholders and escape any literal '%' with another 
'%':

    %(title)s
        The title of the current post/page.

    %(content)s
        The content of the current post/page.

    %%
        A literal '%'.

There is a default template in the database, whose name is 'default', which
can be used as an example of template.

Users can specify the default template to use with the option
|ub_default_template|.

------------------------------------------------------------------------------

  Syntaxes                                                  *UltraBlog_Syntaxes*

The syntaxes supported by UltraBlog.vim currently are:

    markdown, html, rst, textile, latex.

------------------------------------------------------------------------------

  Statuses                                                  *UltraBlog_Statuses*

The available statuses are:

    publish, private, draft, pending.

------------------------------------------------------------------------------

  Scopes                                                      *UltraBlog_Scopes*

Scopes tells UltraBlog.vim to operate on items in which place, "local" stands 
for items stored in the database, and "remote" stands for the blog.

==============================================================================
  Prerequisites                                        *UltraBlog_Prerequisites*
==============================================================================

UltraBlog.vim requires the following softwares to work normally:

    * Vim with python support enabled
    * SQLAlchemy 0.7 or newer
    + python-markdown or python-markdown2
    + pandoc or other

The asterisk items are required at any condition.

One of the two markdown modules of python is needed if you want to convert 
posts/pages from Markdown to HTML.

pandoc is requried when you need to do conversions between any pair of the 
avaliable syntaxes. You can also specify you own external command as the 
converter. For more information, refer to help messages of options below.

==============================================================================
  Keymappings                                            *UltraBlog_Keymappings*
==============================================================================

All keymappings can be customized by setting some options, refer to
|UltraBlog_Options| for detail information.

<c-pageup>    - Shift to previous page in local post list.
<c-pagedown>  - Shift to next page in local post list.
<del>         - Delete the post under cursor in post list.
<enter>       - Open the post/page under cursor in the current view, if is in 
                remote post list, posts which are not in local database will 
                be saved to it before opened.
<s-enter>     - Open the post/page under cursor in a new splitted window.
<c-enter>     - Open the post/page under cursor in a new tab.

==============================================================================
  Commands                                                 *UltraBlog_Commands*
==============================================================================

:UBNew [item [syntax/template_name]]                                    *:UBNew*
    Create a new item.

    For the first parameter, refer to |UltraBlog_Items|. The default value is
    "post".

    If "item" is either "post" or "page", the second parameter must be a syntax 
    name, refer to |UltraBlog_Syntaxes|. The default value is "markdown".

    If "item" is "tmpl", the second parameter should be the name of the new 
    template.

:UBPreview [status/template name]                                   *:UBPreview*
    Preview the current buffer.

    If any of the |UltraBlog_Statuses| is given, the current buffer will be sent
    to the blog and then opened in the browser with a parameter "preview" 
    appended to the URL.

    If the given parameter is not a post status, a pre-defined template whose
    name is the same with the parameter will be used to preview the buffer
    locally.

    If none is given, the default template is used.
    
    You do not have to care for which syntax you use, markdown source will be 
    translated into html automatically before a browser window is opened to 
    display it.

:UBSave                                                               *:UBSave*
    Save modifications. After executing this command, the current buffer is 
    saved into database.

:UBSend [status]                                                      *:UBSend*
    Post an item. If no parameter is given, UltraBlog.vim will send the item 
    to blog and set it to be the value stored in the meta information area.

    Refer to |UltraBlog_Statuses|.

:UBList [item [scope [page_size [page_no]]]]                          *:UBList*
    List items.

    Refer to |UltraBlog_Items| for the first parameter. The default value of 
    this parameter is "post".

    The second parameter "scope" is only available when "item" is either 
    "post" or "page". Refer to |UltraBlog_Scopes|.

    "page_size" and "page_no" are both for the situation when "item" is "post"
    and "scope" is "local". The former stands for how many item will be listed
    a page. The latter stands for the page number.

    For example:

    :UBList

    This command lists the first page of local posts, by default, posts which 
    have not been posted to blog are listed before the posted ones, and there 
    are |ub_local_pagesize| posts a page.

    :UBList post local 20 3

    This command lists the third page of local posts, 20 posts a page. As you 
    see, you can use this command to scroll forward or back between pages. As 
    a matter of fact, there are two key mappings within local post list:

      * CTRL+PageDown
      * CTRL+PageUp

    :UBList post remote 50

    This command lists the latest 50 posts in the blog.

    Pressing the ENTER key in a remote post list will open the post under 
    cursor and save it to the local database if it is not in it, otherwise, 
    the local copy will be opened instead of the remote one. This enables 
    users to modify markdown source and update the remote post.

    The remote post list doesn't support paging.

:UBOpen {item} {post_id/template_name} [scope]                        *:UBOpen*
    Open an item. 

    For the first parameter, refer to |UltraBlog_Items|.

    If "item" is either "post" or "page", the second parameter should be value
    of its id. If it's "tmpl", the name.

    For "scope", refer to |UltraBlog_Scopes|. The default value is "local".

:UBDel {item} {post_id/template_name} [scope]                          *:UBDel*
    Delete an item.
    
    Refer to |UBOpen| for the usage of these options.

    You can also delete items in the list by pressing the DELETE button on 
    the target. In a local post list, if the post to be deleted has been 
    posted to the blog, a confirmation will be prompted for you to decide 
    whether to delete the remote copy cascadly.

:UBUpload {file_path}                                               *:UBUpload*
    Upload media. This command can only be executed in a post edit view, and 
    the URL of the uploaed file will be appended in that buffer.

:UBThis [item [syntax]]                                               *:UBThis*
    Create a new post or page which is filled with content in the current 
    buffer. 
    
    If no parameter is specified, the first parameter will be default to "post".
    For the second one, the syntax in the current buffer is used, if no syntax 
    has been set to the current buffer, then "markdown" is used.

:UBConv {to_syntax} [from_syntax]                                      *:UBConv*
    Convert the current buffer from 'from_syntax' to 'to_syntax'.

    Refer to |UltraBlog_Syntaxes|.

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

------------------------------------------------------------------------------

ub_hotkey_open_item_in_current_view        *ub_hotkey_open_item_in_current_view*

    Set the hotkey to be used when to open a post/page in the list view. When
    invoked, the list view will be reused.

    By default, the value is:

    let ub_hotkey_open_item_in_current_view='<enter>'

------------------------------------------------------------------------------

ub_hotkey_open_item_in_splitted_view      *ub_hotkey_open_item_in_splitted_view*

    Set the hotkey to be used when to open a post/page in the list view. When
    invoked, the post/page will be displayed in a new splitted window.

    By default, the value is:

    let ub_hotkey_open_item_in_splitted_view='<s-enter>'

------------------------------------------------------------------------------

ub_hotkey_open_item_in_tabbed_view          *ub_hotkey_open_item_in_tabbed_view*

    Set the hotkey to be used when to open a post/page in the list view. When
    invoked, the post/page will be displayed in a new tab.

    By default, the value is:

    let ub_hotkey_open_item_in_tabbed_view='<c-enter>'

------------------------------------------------------------------------------

ub_hotkey_delete_item                                    *ub_hotkey_delete_item*

    Set the hotkey to be used when to delete a post/page in the list view. 
    When invoked, the post/page will be deleted after a confirmation.

    By default, the value is:

    let ub_hotkey_delete_item='<del>'

------------------------------------------------------------------------------

ub_tmpl_img_url                                                *ub_tmpl_img_url*

    Set the link template for images uploaded by |:UBUpload|. This string 
    should be a template string valid for python to format printing, and every
    placeholder must be among the keys of the dictionary returned by the
    wordpress API method newMediaObject().

    Structure of the dictionary:

        {'url':'http://xxx/z.jpg', 'file':'z.jpg', 'type':'image/x-jpg'}

    By default, the value is:
        
        let ub_tmpl_img_url="markdown###![%(file)s][]\n[%(file)s]:%(url)s"

    As is illustrated, the template string should be appended with its syntax.
    A tripple sharp sign '###' is used to devide the syntax and the template.
    This is used to auto convert the template to the syntax the current buffer
    has. If you don't need this, you can just simply ignore the appending
    syntax and '###'.

------------------------------------------------------------------------------

ub_default_template                                        *ub_default_template*

    Set the default template to be used to preview the current buffer in the 
    browser.

    By default, the value is:

    let ub_default_template="default"

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

2.3.0
    * Feature: Preview posts/pages locally with pre-defined templates,
               template related operations and management.
    * Feature: Preview posts/pages after sending them to the blog.
    * Change:  Merge :UBPageNew to |:UBNew|, merge :UBPageOpen to |:UBOpen|, 
               merge :UBPageList to |:UBList|, merge :UBPageThis to |:UBThis|.
    * Bugfix:  The remote page list will raise an error when openned due to a
               change made in last version.
    * Bugfix:  The post/page list will raise an error when an item is deleted
               in it, caused by a change made in last version.

2.2.0
    * Change:  Merge :UBSave and :UBPageSave to |:UBSave|, merge :UBSend and 
               :UBPageSend to |:UBSend|.
    * Feature: Add an option |ub_tmpl_img_url| to customize a link template 
               string for the image uploaded by |:UBUpload|.

2.1.0
    * Change:  Increase the requirement to SQLAlchemy to version 0.7 or newer.
    * Feature: Now posts/pages can be opened in a new tab or splitted window.
    * Feature: Add options to let users customize hotkeys:
                   |ub_hotkey_open_item_in_current_view|
                   |ub_hotkey_open_item_in_splitted_view|
                   |ub_hotkey_open_item_in_tabbed_view|
                   |ub_hotkey_delete_item|

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
    * Feature: Add two commands: |:UBThis| and :UBPageThis, to create a new post 
               or page with content in the current buffer.

1.3
    * Feature: Record post statuses, post them in their recorded statuses when 
               omit parameters for |:UBSend| and :UBPageSend.
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
