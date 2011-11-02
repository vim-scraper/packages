" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/UltraBlog.vim	[[[1
222
 
" File:        UltraBlog.vim
" Description: Ultimate vim blogging plugin that manages web logs
" Author:      Lenin Lee <lenin.lee at gmail dot com>
" Version:     3.1.0
" Last Change: 2011-11-01
" License:     The New BSD License
"
" ============================================================================

if !has("python")"{{{
    finish
endif"}}}

function! SyntaxCmpl(ArgLead, CmdLine, CursorPos)"{{{
  return "markdown\nhtml\nrst\ntextile\nlatex\n"
endfunction"}}}

function! StatusCmpl(ArgLead, CmdLine, CursorPos)"{{{
  return "draft\npublish\nprivate\npending\n"
endfunction"}}}

function! ScopeCmpl(ArgLead, CmdLine, CursorPos)"{{{
  return "local\nremote\n"
endfunction"}}}

function! UBNewCmpl(ArgLead, CmdLine, CursorPos)"{{{
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
endfunction"}}}

function! UBOpenCmpl(ArgLead, CmdLine, CursorPos)"{{{
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
endfunction"}}}

function! UBListCmpl(ArgLead, CmdLine, CursorPos)"{{{
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
endfunction"}}}

function! UBDelCmpl(ArgLead, CmdLine, CursorPos)"{{{
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
endfunction"}}}

function! UBThisCmpl(ArgLead, CmdLine, CursorPos)"{{{
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
    " For the second argument, complete the syntax to be converted to
    " For the third argument, complete the syntax to be converted from
    elseif (len(lst)==2 || len(lst)==3) && count(['post', 'page'], lst[1])==1
        let syntaxes = ['markdown','html','rst','textile','latex']
        for synx in syntaxes
            if stridx(synx,a:ArgLead)==0
                call add(results,synx)
            endif
        endfor
    endif
    return results
endfunction"}}}

function! UBPreviewCmpl(ArgLead, CmdLine, CursorPos)"{{{
python <<EOF
enc = vim.eval('&encoding')
templates = [tmpl.encode(enc) for tmpl in ub_get_templates(True)]
vim.command('let b:ub_templates=%s' % str(templates))
EOF
    let tmpls = ['publish', 'private', 'draft']
    if exists('b:ub_templates')
        call extend(tmpls, b:ub_templates)
    endif
    return join(tmpls, "\n")
endfunction"}}}

" Clear undo history
function! UBClearUndo()"{{{
    let old_undolevels = &undolevels
    set undolevels=-1
    exe "normal a \<BS>\<Esc>"
    let &undolevels = old_undolevels
    unlet old_undolevels
endfunction"}}}

" Commands
command! -nargs=* -complete=customlist,UBListCmpl UBList exec('py ub_list_items(<f-args>)')
command! -nargs=* -complete=customlist,UBNewCmpl UBNew exec('py ub_new_item(<f-args>)')
command! -nargs=* -complete=customlist,UBOpenCmpl UBOpen exec('py ub_open_item_x(<f-args>)')
command! -nargs=* -complete=customlist,UBDelCmpl UBDel exec('py ub_del_item(<f-args>)')
command! -nargs=? -complete=custom,StatusCmpl UBSend exec('py ub_send_item(<f-args>)')
command! -nargs=* -complete=customlist,UBThisCmpl UBThis exec('py ub_blog_this(<f-args>)')
command! -nargs=? -complete=custom,UBPreviewCmpl UBPreview exec('py ub_preview(<f-args>)')
command! -nargs=0 UBSave exec('py ub_save_item()')
command! -nargs=1 -complete=file UBUpload exec('py ub_upload_media(<f-args>)')
command! -nargs=* -complete=custom,SyntaxCmpl UBConv exec('py ub_convert(<f-args>)')
command! -nargs=+ UBFind exec('py ub_find(1, <f-args>)')
command! -nargs=0 UBRefresh exec('py ub_refresh_current_view()')

" Auto-commands
au BufEnter * py __ub_on_buffer_enter()

python <<EOF
# -*- coding: utf-8 -*-
import vim,os

for pth in vim.eval('&rtp').split(','): sys.path.append(os.path.join(pth, 'plugin'))
from ultrablog.exceptions import *
from ultrablog.events import *
from ultrablog.commands import *
from ultrablog.listeners import UBEventQueue

def __ub_on_buffer_enter():
    ''' Triggered by BufEnter event, check if the buffer is outdated
    '''
    if ub_is_view_outdated('%'):
        ub_refresh_current_view()
        ub_set_view_outdated('%', False)

if __name__ == "__main__":
    pass
EOF
doc/UltraBlog.txt	[[[1
677
*UltraBlog.txt*
==============================================================================
  UltraBlog                                                        *UltraBlog*
==============================================================================

Author:      Lenin Lee   lenin.lee at gmail dot com       *UltraBlog_Author*
URL:         http://sinolog.it/?p=1894                    *UltraBlog_URL*
Description: Ultimate vim blogging plugin.                |UltraBlog_Motivation|
Version:     3.1.0                                        |UltraBlog_History|
License:     New BSD                                      |UltraBlog_License|
Contribute:  Please report any bugs or suggestions        |UltraBlog_Bugs|
             to the address above.                        |UltraBlog_Future|

==============================================================================
  Table of Contents                                        *UltraBlog_Contents*
==============================================================================

             1. Motivation.................|UltraBlog_Motivation|
             2. Features...................|UltraBlog_Features|
             3. Concepts...................|UltraBlog_Concepts|
                3.1 Modes..................|UltraBlog_Modes|
                3.2 Items..................|UltraBlog_Items|
                3.3 Syntaxes...............|UltraBlog_Syntaxes|
                3.4 Scopes.................|UltraBlog_Scopes|
                3.5 Status.................|UltraBlog_Statuses|
             4. Prerequisites..............|UltraBlog_Prerequisites|
             5. Keymaps....................|UltraBlog_Keymaps|
             6. Commands...................|UltraBlog_Commands|
             7. Options....................|UltraBlog_Options|
             8. Bugs.......................|UltraBlog_Bugs|
             9. Future.....................|UltraBlog_Future|
            10. History....................|UltraBlog_History|
            11. License....................|UltraBlog_License|

==============================================================================
  Motivation                                             *UltraBlog_Motivation*
==============================================================================

UltraBlog.vim is yet another Vim blogging script for Wordpress.

The biggest difference between UB and other similar scripts is that UB is an 
ultimate client, which stores posts locally in an SQLite database, while others 
just operate remotely. In this way, many things can be done easily by the 
advantage of local storage and many other utilities, for example, you can search 
for posts stored in the database with a few keywords by typing a simple command, 
also you can preview drafts with any of your favorite templates as soon as you 
want to.

For those who just needs a lightweight blog editor similar as other Vim blogging 
scripts, UB also comes with an Editor Mode, which doesn't create a database and 
store data in it.

In addition, UB is tending to make life easier for writing posts with many kinds 
of lightweight markup languages, currently the following kinds are supported: 
Markdown, reStructuredText, LaTeX, Textile and of cause HTML.

Enjoy Vim blogging !

==============================================================================
  Features                                                 *UltraBlog_Features*
==============================================================================

    * Event-driven system.
    * Multiple syntax support: Markdown, HTML, reStructuredText, LaTeX,
      Textile.
    * Editor mode and client mode.
    * Data is stored in a local SQLite database in client mode.
    * Context search with keywords highlighting.
    * Templates for previewing posts.

==============================================================================
  Concepts                                                 *UltraBlog_Concepts*
==============================================================================

UltraBlog.vim makes life easier while writing or updating blogs. It stores
posts/pages in a local SQLite database. You can also set it to editor mode, in
which UltraBlog.vim does not store data locally, just like other Vim blogging 
scripts.

------------------------------------------------------------------------------

By default, UltraBlog.vim is in client mode. You can set it to use editor mode
by adding the fallowing line to the vimrc file:

  Modes                                                       *UltraBlog_Modes*

    let ub_editor_mode = 1

------------------------------------------------------------------------------

  Items                                                       *UltraBlog_Items*

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

  Syntaxes                                                 *UltraBlog_Syntaxes*

The syntaxes supported by UltraBlog.vim currently are:

    markdown, html, rst, textile, latex.

------------------------------------------------------------------------------

  Statuses                                                 *UltraBlog_Statuses*

The available statuses are:

    publish, private, draft, pending.

------------------------------------------------------------------------------

  Scopes                                                     *UltraBlog_Scopes*

Scopes tells UltraBlog.vim to operate on items in which place, "local" stands 
for items stored in the database, and "remote" stands for the blog.

==============================================================================
  Prerequisites                                       *UltraBlog_Prerequisites*
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
  Keymappings                                           *UltraBlog_Keymappings*
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

:UBNew [item [syntax/template_name]]                                   *:UBNew*
    Create a new item.

    For the first parameter, refer to |UltraBlog_Items|. The default value is
    "post".

    If "item" is either "post" or "page", the second parameter must be a syntax 
    name, refer to |UltraBlog_Syntaxes|. The default value is "markdown".

    If "item" is "tmpl", the second parameter should be the name of the new 
    template.

:UBPreview [status/template name]                                  *:UBPreview*
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

    "page_size" and "page_no" are both for the situation when "item" is 
    "post" and "scope" is "local". The former stands for how many item will 
    be listed a page. The latter stands for the page number.

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

:UBThis [item [to_syntax [from_syntax]]                               *:UBThis*
    Create a new post or page which is filled with content in the current 
    buffer. 
    
    If no parameter is specified, the first parameter will be default to 
    "post"; for the second and the third one, the syntax of the current buffer
    is used.

    Content of the current buffer will be automatically converted from the
    [from_syntax] to the [to_syntax].

:UBConv {to_syntax} [from_syntax]                                     *:UBConv*
    Convert the current buffer from 'from_syntax' to 'to_syntax'.

    Refer to |UltraBlog_Syntaxes|.

    If you only need to convert from Markdown to HTML, only python-markdown or
    python-markdown2 module is required. For other conversion scenarios, you 
    must install pacdoc or use the options: |ub_converter_command|, 
    |ub_converter_options|, |ub_converter_option_from|,
    |ub_converter_option_to| to specify a valid external command.

:UBRefresh                                                         *:UBRefresh*
    Refresh the current buffer.

:UBFind keyword1 [keyword2 ...]                                       *:UBFind*
    Context search for both posts and pages, all keywords will be highlighted.
    Page size of the search result list is controlled by |ub_search_pagesize|.

==============================================================================
  Options                                                   *UltraBlog_Options*
==============================================================================

ub_blog                                                               *ub_blog*

    Example:
        let ub_blog = {'login_name':'admin',
                    \'password':'pass2011',
                    \'url':'http://www.sample.com/',
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

ub_search_pagesize                                         *ub_search_pagesize*

    Default pagesize for search result list. If not specified, the default 
    value is 30.

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

ub_save_after_opend                                      *ub_save_after_opened*
ub_save_after_sent                                         *ub_save_after_sent*

    If the above options are set to 1, posts/pages will be saved to database
    immediately after they are opened from or sent to the blog.

    By default, their values are 0s.

------------------------------------------------------------------------------

ub_converter_command                                     *ub_converter_command*

    This is the command needed to do conversions between the avaliable
    syntaxes.

    By default, the value is:

        'pandoc'

------------------------------------------------------------------------------

ub_converter_options                                     *ub_converter_options*

    A list of options to be appended to the |ub_converter_command|.

    By default, the value is:

        ['--reference-links']

------------------------------------------------------------------------------

ub_converter_option_from                             *ub_converter_option_from*

    A template string which will be filled with the 'from_syntax' and appended
    to the |ub_converter_command|.

    By default, the value is:

        '--from=%s'

------------------------------------------------------------------------------

ub_converter_option_to                                 *ub_converter_option_to*

    A template string which will be filled with the 'to_syntax' and appended
    to the |ub_converter_command|.

    By default, the value is:

        '--to=%s'

------------------------------------------------------------------------------

ub_hotkey_open_item_in_current_view       *ub_hotkey_open_item_in_current_view*

    Set the hotkey to be used when to open a post/page in the list view. When
    invoked, the list view will be reused.

    By default, the value is:

    let ub_hotkey_open_item_in_current_view='<enter>'

------------------------------------------------------------------------------

ub_hotkey_open_item_in_splitted_view     *ub_hotkey_open_item_in_splitted_view*

    Set the hotkey to be used when to open a post/page in the list view. When
    invoked, the post/page will be displayed in a new splitted window.

    By default, the value is:

    let ub_hotkey_open_item_in_splitted_view='<s-enter>'

------------------------------------------------------------------------------

ub_hotkey_open_item_in_tabbed_view         *ub_hotkey_open_item_in_tabbed_view*

    Set the hotkey to be used when to open a post/page in the list view. When
    invoked, the post/page will be displayed in a new tab.

    By default, the value is:

    let ub_hotkey_open_item_in_tabbed_view='<c-enter>'

------------------------------------------------------------------------------

ub_hotkey_delete_item                                   *ub_hotkey_delete_item*

    Set the hotkey to be used when to delete a post/page in the list view. 
    When invoked, the post/page will be deleted after a confirmation.

    By default, the value is:

    let ub_hotkey_delete_item='<del>'

------------------------------------------------------------------------------

ub_tmpl_img_url                                               *ub_tmpl_img_url*

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

ub_default_template                                       *ub_default_template*

    Set the default template to be used to preview the current buffer in the 
    browser.

    By default, the value is:

    let ub_default_template="default"

==============================================================================
  Future                                                     *UltraBlog_Future*
==============================================================================

    * The future of UltraBlog.vim depends on you, send me your advices.
    * Write a syntax file for this script.

==============================================================================
  Bugs                                                         *UltraBlog_Bugs*
==============================================================================

If you discover any bugs not listed here, please contact the |UltraBlog_Author|.

Or submit issues with Github's issues tracker:

    https://github.com/xbot/UltraBlog.vim/issues

==============================================================================
  History                                                   *UltraBlog_History*
==============================================================================

3.1.0
    * Change:  The key "xmlrpc" of the settings list is dropped, a new one
               with the name "url" is added, you should set its value to the
               blog url.
    * Change:  Source code refactorings.
    * Change:  Solve the crash problem existing in MacVim only.
    * Change:  |:UBConv| can be used in any buffer.
    * Change:  |:UBThis| now has a third parameter, which enables convertions
               between syntaxes.
    * Bugfix:  The second parameter of |:UBThis| cannot take effect.

3.0.0
    * Feature: Mostly rewritten for implementing an event-driven system.
    * Feature: Context search comes with keywords highlighting. A new command
               |:UBFind| and a new option |ub_search_pagesize|.
    * Feature: Refresh every buffer with the new command |:UBRefresh|.
    * Bugfix:  Some.

2.3.1
    * Bugfix:  Listing remote posts will complain the error: local variable 
               'page_no' referenced before assignment.
    * Bugfix:  The editing window will be set as modified after being sent to
               blog when |ub_save_after_sent| is 0 and the buffer was in 
               unmodified status.

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

The New BSD License

==============================================================================
vim:tw=78:ts=4:ft=help:norl:fdm=marker
plugin/ultrablog/__init__.py	[[[1
1
#!/usr/bin/env python
plugin/ultrablog/commands.py	[[[1
1002
#!/usr/bin/env python
# -*- encoding: utf-8 -*-

import vim, xmlrpclib, webbrowser, sys, re, tempfile, os, mimetypes, inspect, types
from exceptions import *
from db import *
from util import *
from events import *
from eventqueue import UBEventQueue

def __ub_exception_handler(func):
    def __check(*args,**kwargs):
        try:
            return func(*args,**kwargs)
        except UBException, e:
            print >> sys.stderr,str(e)
        except xmlrpclib.Fault, e:
            print >> sys.stderr,"xmlrpc error: %s" % e.faultString
        except xmlrpclib.ProtocolError, e:
            print >> sys.stderr,"xmlrpc error: %s %s" % (e.url, e.errmsg)
        except IOError, e:
            print >> sys.stderr,"network error: %s" % e
        except Exception, e:
            print >> sys.stderr,str(e)
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

@__ub_exception_handler
def ub_list_items(item_type='post', scope='local', page_size=None, page_no=None):
    ''' List items
    '''
    cmd = UBCmdList(item_type, scope, page_size, page_no)
    cmd.execute()

@__ub_exception_handler
def ub_find(page_no, *keywords):
    ''' List posts/pages which match the keywords given
    '''
    cmd = UBCmdFind(page_no, *keywords)
    cmd.execute()

@__ub_exception_handler
def ub_refresh_current_view():
    ''' Refresh current view
    '''
    cmd = UBCmdRefresh()
    cmd.execute()

@__ub_exception_handler
def ub_preview(tmpl=None):
    '''Preview the current buffer in a browser
    '''
    cmd = UBCmdPreview(tmpl)
    cmd.execute()

@__ub_exception_handler
def ub_save_item():
    '''Save the current buffer to local database
    '''
    cmd = UBCmdSave()
    cmd.execute()

@__ub_exception_handler
def ub_send_item(status=None):
    '''Send the current item to the blog
    '''
    cmd = UBCmdSend(status)
    cmd.execute()

@__ub_exception_handler
def ub_open_item_x(item_type, item_key, scope='local'):
    ''' Open item, this function use __ub_exception_handler and so is suitable to be called directly
    '''
    ub_open_item(item_type, item_key, scope)

def ub_open_item(item_type, item_key, scope='local'):
    ''' Open item, this function do not use the __ub_exception_handler and so can be used programmatically
    '''
    cmd = UBCmdOpen(itemKey=item_key, itemType=item_type, scope=scope)
    cmd.execute()

@__ub_exception_handler
def ub_open_item_under_cursor(view_type=None):
    '''Open the item under cursor, invoked in post or page list
    '''
    cmd = UBCmdOpenItemUnderCursor(view_type)
    cmd.execute()

@__ub_exception_handler
def ub_del_item(item_type, key, scope='local'):
    '''Delete an item
    '''
    cmd = UBCmdDelete(item_type, key, scope)
    cmd.execute()

@__ub_exception_handler
def ub_del_item_under_cursor():
    '''Delete local post, invoked in list view
    '''
    cmd = UBCmdDelItemUnderCursor()
    cmd.execute()

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
    result = api.metaWeblog.newMediaObject('', cfg.loginName, cfg.password,
        dict(name=os.path.basename(file_path), type=file_type, bits=bin_data))

    img_tmpl_info = ub_get_option('ub_tmpl_img_url', True)
    img_url = img_tmpl_info['tmpl'] % result
    syntax = vim.eval('&syntax')
    img_url = ub_convert_str(img_url, img_tmpl_info['syntax'], syntax)
    vim.current.range.append(img_url.split("\n"))

@__ub_exception_handler
def ub_blog_this(item_type='post', to_syntax=None, from_syntax=None):
    '''Create a new post/page with content in the current buffer
    '''
    cmd = UBCmdBlogThis(item_type, to_syntax, from_syntax)
    cmd.execute()

@__ub_exception_handler
def ub_convert(to_syntax, from_syntax=None):
    '''Convert the current buffer from one syntax to another
    '''
    cmd = UBCmdConvert(to_syntax, from_syntax)
    cmd.execute()

@__ub_exception_handler
def ub_new_item(item_type='post', mixed='markdown'):
    ''' Create new item: post, page, template
    '''
    cmd = UBCmdNew(item_type, mixed)
    cmd.execute()

class UBCommand(object):
    ''' Abstract parent class for all commands of UB
    '''
    def __init__(self, isContentAware=False):
        self.checkPrerequisites()
        # Set editor mode if the corresponding option has been set
        ub_set_mode()

        self.isContentAware = isContentAware
        self.scope = 'local'
        self.itemType = None
        self.viewScopes = []
        self.enc = vim.eval('&encoding')
        self.syntax = vim.eval('&syntax')
        self.sess = Session()
        self.viewName = ub_get_viewname('%')

        if self.viewName is not None:
            vnameParts = self.viewName.split('_')
            if ub_is_view_of_type('list'):
                self.itemType = vnameParts[1]=='result' and 'post' or vnameParts[1]
                self.scope = vnameParts[0]=='search' and 'local' or vnameParts[0]
            if ub_is_view_of_type('edit'):
                self.itemType = vnameParts[0]

    def checkPrerequisites(self):
        ''' Check the prerequisites
        '''
        if sqlalchemy is None: raise UBException('Cannot find SQLAlchemy !')
        if Base is None or Session is None or Post is None or Template is None:
            raise UBException('Cannot create database objects !')
        if cfg is None: raise UBException('Settings of UltraBlog.vim is missing or invalid !')
        if api is None: raise UBException('Cannot initiate API !')
        if db is None: raise UBException('Cannot connect to database !')

    def checkItemType(self, itemType=None):
        ''' Check if the item type is among the available ones
        '''
        itemType = itemType and itemType or self.itemType
        if not itemType in ['post', 'page', 'tmpl', None]:
            raise UBException('Unknow item type, available types are: post, page and tmpl !')

    def checkScope(self, scope=None):
        '''Check the given scope,
        return True if it is local,
        return False if it is remote,
        raise an exception if it is neither of the upper two
        '''
        scope = scope and scope or self.scope
        if scope=='local':
            return True
        elif scope=='remote':
            return False
        else:
            raise UBException('Invalid scope !')

    def checkSyntax(self, syntax=None):
        ''' Check if the given syntax is among the available ones
        '''
        syntax = syntax is not None and syntax or self.syntax
        valid_syntax = ['markdown', 'html', 'rst', 'textile', 'latex']
        if syntax is None or syntax.lower() not in valid_syntax:
            raise UBException('Unknown syntax, valid syntaxes are %s' % str(valid_syntax))

    def checkViewScope(self, viewName=None):
        ''' Check if the given viewname is among the available ones
        '''
        viewName = viewName is not None and viewName or self.viewName
        if len(self.viewScopes)>0:
            for scope in self.viewScopes:
                if viewName is not None and viewName.endswith(scope): return
            raise UBException('Invalid view, this command is only allowed in %s !' % str(self.viewScopes))

    def execute(self):
        ''' The main functional method of this command
        '''
        self._preExec()
        self._exec()
        self._postExec()

    def _preExec(self):
        ''' Do something before self._exec()
        protected method, called by self.execute()
        '''
        self.checkItemType()
        self.checkScope()
        self.checkViewScope()
        if self.isContentAware is True: self.checkSyntax()

    def _exec(self):
        ''' Do the main part of the job
        protected method, called by self.execute()
        '''
        raise UBException('Not implemented yet !')

    def _postExec(self):
        ''' Do something after self._exec()
        protected method, called by self.execute()
        '''
        self.sess.close()

    @classmethod
    def doDefault(cls, *args, **kwargs):
        frame = inspect.currentframe(1)
        self = frame.f_locals["self"]
        methodName = frame.f_code.co_name
        
        method = getattr(super(cls, self), methodName, None)
        
        if inspect.ismethod(method):
            return method(*args, **kwargs)

class UBCmdList(UBCommand):
    ''' Listing command, implements UBCommand
    '''
    def __init__(self, itemType='post', scope='local', pageSize=None, pageNo=None):
        UBCommand.__init__(self)
        self.itemType = itemType
        self.scope = scope
        self.pageSize = int(pageSize is not None and pageSize or ub_get_option("ub_%s_pagesize" % self.scope))
        self.pageNo = int(pageNo is not None and pageNo or 1)

    def _preExec(self):
        UBCmdList.doDefault()
        if self.pageNo<1: raise UBException('Page NO. cannot be less than 1 !')
        if self.pageSize<1: raise UBException('Illegal page size (%s) !' % self.pageSize)

    def _exec(self):
        if self.itemType=='tmpl': self._listTemplates()
        else: eval("self._list%s%ss()" % (self.scope.capitalize(), self.itemType.capitalize()))

    def _postExec(self):
        UBCmdList.doDefault()
        vim.command("map <buffer> "+ub_get_option('ub_hotkey_open_item_in_current_view')+" :py ub_open_item_under_cursor('cur')<cr>")
        vim.command("map <buffer> "+ub_get_option('ub_hotkey_open_item_in_splitted_view')+" :py ub_open_item_under_cursor('split')<cr>")
        vim.command("map <buffer> "+ub_get_option('ub_hotkey_open_item_in_tabbed_view')+" :py ub_open_item_under_cursor('tab')<cr>")
        vim.command("map <buffer> "+ub_get_option('ub_hotkey_delete_item')+" :py ub_del_item_under_cursor()<cr>")
        vim.command('call UBClearUndo()')
        vim.command('setl nomodified')
        vim.command("setl nomodifiable")
        vim.current.window.cursor = (2, 0)

    def _listLocalPosts(self):
        '''List local posts stored in database
        '''
        posts = []

        tbl = Post.__table__
        ua = union_all(
            select([select([tbl.c.id,case([(tbl.c.post_id>0, tbl.c.post_id)], else_=0).label('post_id'),tbl.c.status,tbl.c.title])\
                .where(tbl.c.post_id==None).where(tbl.c.type=='post').order_by(tbl.c.id.desc())]),
            select([select([tbl.c.id,case([(tbl.c.post_id>0, tbl.c.post_id)], else_=0).label('post_id'),tbl.c.status,tbl.c.title])\
                .where(tbl.c.post_id!=None).where(tbl.c.type=='post').order_by(tbl.c.post_id.desc())])
        )
        stmt = select([ua]).limit(self.pageSize).offset(self.pageSize*(self.pageNo-1))

        conn = db.connect()
        rslt = conn.execute(stmt)
        while True:
            row = rslt.fetchone()
            if row is not None:
                posts.append(row)
            else:
                break
        conn.close()

        if len(posts)==0: raise UBException('No more posts found !')

        ub_wise_open_view('local_post_list')
        vim.current.buffer[0] = "==================== Posts (Page %d) ====================" % self.pageNo
        tmpl = ub_get_list_template()
        vim.current.buffer.append([(tmpl % (post.id,post.post_id,post.status,post.title)).encode(self.enc) for post in posts])

        vim.command("let b:page_no=%s" % self.pageNo)
        vim.command("let b:page_size=%s" % self.pageSize)
        vim.command("map <buffer> "+ub_get_option('ub_hotkey_pagedown')+" :py ub_list_items('post', 'local', %d, %d)<cr>" % (self.pageSize, self.pageNo+1))
        vim.command("map <buffer> "+ub_get_option('ub_hotkey_pageup')+" :py ub_list_items('post', 'local', %d, %d)<cr>" % (self.pageSize, self.pageNo-1))

    def _listRemotePosts(self):
        '''List remote posts stored in the blog
        '''
        posts = api.metaWeblog.getRecentPosts('', cfg.loginName, cfg.password, self.pageSize)
        for post in posts:
            local_post = self.sess.query(Post).filter(Post.post_id==post['postid']).first()
            if local_post is None:
                post['id'] = 0
            else:
                post['id'] = local_post.id
                post['post_status'] = local_post.status

        ub_wise_open_view('remote_post_list')
        vim.current.buffer[0] = "==================== Recent Posts ===================="
        tmpl = ub_get_list_template()
        vim.current.buffer.append([(tmpl % (post['id'],post['postid'],post['post_status'],post['title'])).encode(self.enc) for post in posts])

        vim.command("let b:page_size=%s" % self.pageSize)

    def _listLocalPages(self):
        '''List local pages stored in database
        '''
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

        if len(pages)==0: raise UBException('No more pages found !')

        ub_wise_open_view('local_page_list')
        vim.current.buffer[0] = "==================== Local Pages ===================="
        tmpl = ub_get_list_template()
        vim.current.buffer.append([(tmpl % (page.id,page.post_id,page.status,page.title)).encode(self.enc) for page in pages])

    def _listRemotePages(self):
        '''List remote pages stored in the blog
        '''
        pages = api.wp.getPages('', cfg.loginName, cfg.password)
        for page in pages:
            local_page = self.sess.query(Post).filter(Post.post_id==page['page_id']).filter(Post.type=='page').first()
            if local_page is None:
                page['id'] = 0
            else:
                page['id'] = local_page.id
                page['page_status'] = local_page.status

        ub_wise_open_view('remote_page_list')
        vim.current.buffer[0] = "==================== Blog Pages ===================="
        tmpl = ub_get_list_template()
        vim.current.buffer.append([(tmpl % (page['id'],page['page_id'],page['page_status'],page['title'])).encode(self.enc) for page in pages])

    def _listTemplates(self):
        '''List preview templates
        '''
        tmpls = self.sess.query(Template).all()

        if len(tmpls)==0:
            print >> sys.stderr,'No template found !'
            return

        ub_wise_open_view('local_tmpl_list')
        vim.current.buffer[0] = "==================== Templates ===================="
        line = "%-24s%s"
        vim.current.buffer.append([(line % (tmpl.name,tmpl.description)).encode(self.enc) for tmpl in tmpls])

class UBCmdFind(UBCommand):
    ''' Context search
    '''
    def __init__(self, pageNo, *keywords):
        UBCommand.__init__(self)
        self.pageSize = int(ub_get_option("ub_%s_pagesize" % self.scope))
        self.pageNo = int(pageNo is not None and pageNo or 1)
        self.keywords = keywords

    def _preExec(self):
        UBCmdFind.doDefault()
        if self.pageNo<1: raise UBException('Page NO. cannot be less than 1 !')
        if self.pageSize<1: raise UBException('Illegal page size (%s) !' % self.pageSize)

    def _exec(self):
        posts = []
        tbl = Post.__table__

        conds = []
        for keyword in self.keywords:
            kwcond = []
            kwcond.append(tbl.c.title.like('%%%s%%' % keyword.decode(self.enc)))
            kwcond.append(tbl.c.content.like('%%%s%%' % keyword.decode(self.enc)))
            conds.append(or_(*kwcond))

        stmt = select([tbl.c.id,case([(tbl.c.post_id>0, tbl.c.post_id)], else_=0).label('post_id'),tbl.c.status,tbl.c.title],
            and_(*conds)
        ).limit(self.pageSize).offset(self.pageSize*(self.pageNo-1)).order_by(tbl.c.status.asc(),tbl.c.post_id.desc())

        conn = db.connect()
        rslt = conn.execute(stmt)
        while True:
            row = rslt.fetchone()
            if row is not None:
                posts.append(row)
            else:
                break
        conn.close()

        if len(posts)==0: raise UBException('No more posts found !')

        ub_wise_open_view('search_result_list')
        vim.current.buffer[0] = "==================== Results (Page %d) ====================" % self.pageNo
        tmpl = ub_get_list_template()
        vim.current.buffer.append([(tmpl % (post.id,post.post_id,post.status,post.title)).encode(self.enc) for post in posts])

        vim.command("let b:page_no=%s" % self.pageNo)
        vim.command("let b:page_size=%s" % self.pageSize)
        vim.command("let b:ub_keywords=[%s]" % ','.join(["'%s'" % kw for kw in self.keywords]))
        vim.command("map <buffer> "+ub_get_option('ub_hotkey_open_item_in_current_view')+" :py ub_open_item_under_cursor('cur')<cr>")
        vim.command("map <buffer> "+ub_get_option('ub_hotkey_open_item_in_splitted_view')+" :py ub_open_item_under_cursor('split')<cr>")
        vim.command("map <buffer> "+ub_get_option('ub_hotkey_open_item_in_tabbed_view')+" :py ub_open_item_under_cursor('tab')<cr>")
        vim.command("map <buffer> "+ub_get_option('ub_hotkey_delete_item')+" :py ub_del_item_under_cursor()<cr>")
        vim.command("map <buffer> "+ub_get_option('ub_hotkey_pagedown')+" :py ub_find(%d,%s)<cr>" % (self.pageNo+1, ','.join(["'%s'" % kw for kw in self.keywords])))
        vim.command("map <buffer> "+ub_get_option('ub_hotkey_pageup')+" :py ub_find(%d,%s)<cr>" % (self.pageNo-1, ','.join(["'%s'" % kw for kw in self.keywords])))
        vim.command('call UBClearUndo()')
        vim.command('setl nomodified')
        vim.command("setl nomodifiable")
        vim.current.window.cursor = (2, 0)
        vim.command("let @/='\\(%s\\)'" % '\\|'.join(self.keywords))
        vim.command('setl hls')

class UBCmdSave(UBCommand):
    ''' Save items
    '''
    def __init__(self):
        UBCommand.__init__(self, True)
        self.item = None
        self.itemKey = ub_get_meta('id')
        self.viewScopes = ['post_edit', 'page_edit', 'tmpl_edit']

    def _preExec(self):
        UBCmdSave.doDefault()
        # Do not bother if the current buffer is not modified
        if vim.eval('&modified')=='0': raise UBException('This buffer has not been modified !')

    def _exec(self):
        eval('self._load%s()' % self.itemType.capitalize())
        self.sess.add(self.item)
        self.sess.commit()
        self.itemKey = self.item.getKey(self.enc)

    def _postExec(self):
        UBCmdSave.doDefault()

        ub_set_meta(self.item.getKeyProperty(), self.itemKey)
        vim.command('setl nomodified')
        
        evt = eval("UB%sSaveEvent('%s')" % (self.itemType=='tmpl' and 'Tmpl' or 'Post', self.itemKey));
        UBEventQueue.fireEvent(evt)
        UBEventQueue.processEvents()

    def _loadTmpl(self):
        '''Save the current template to local database
        '''
        self.itemKey = ub_get_meta('name').decode(self.enc)

        # Check if the given name is a reserved word
        ub_check_reserved_word(self.itemKey)

        tmpl = self.sess.query(Template).filter(Template.name==self.itemKey).first()
        if tmpl is None:
            tmpl = Template()
            tmpl.name = self.itemKey

        tmpl.content = "\n".join(vim.current.buffer[len(ub_get_tmpl_meta_data())+2:]).decode(self.enc)
        tmpl.description = ub_get_meta('description').decode(self.enc)

        self.item = tmpl

    def _loadPost(self):
        '''Save the current buffer to local database
        '''
        if self.itemKey is None:
            post = Post()
        else:
            post = self.sess.query(Post).filter(Post.id==self.itemKey).first()

        post.content = "\n".join(vim.current.buffer[len(ub_get_post_meta_data())+2:]).decode(self.enc)
        post.post_id = ub_get_meta('post_id')
        post.title = ub_get_meta('title').decode(self.enc)
        post.categories = ub_get_meta('categories').decode(self.enc)
        post.tags = ub_get_meta('tags').decode(self.enc)
        post.slug = ub_get_meta('slug').decode(self.enc)
        post.status = ub_get_meta('status').decode(self.enc)
        post.syntax = self.syntax

        self.item = post

    def _loadPage(self):
        '''Save the current page to local database
        '''
        if self.itemKey is None:
            page = Post()
            page.type = 'page'
        else:
            page = self.sess.query(Post).filter(Post.id==self.itemKey).first()

        page.content = "\n".join(vim.current.buffer[len(ub_get_page_meta_data())+2:]).decode(self.enc)
        page.post_id = ub_get_meta('post_id')
        page.title = ub_get_meta('title').decode(self.enc)
        page.slug = ub_get_meta('slug').decode(self.enc)
        page.status = ub_get_meta('status').decode(self.enc)
        page.syntax = self.syntax

        self.item = page

class UBCmdSend(UBCommand):
    ''' Send item
    '''
    def __init__(self, status=None):
        UBCommand.__init__(self, True)
        self.status = status is not None and status or ub_get_meta('status')
        self.publish = ub_check_status(self.status)
        self.item = None
        self.viewScopes = ['post_edit', 'page_edit'];
        self.postId = ub_get_meta('post_id')

    def _exec(self):
        eval('self._load%s()' % self.itemType.capitalize())
        if self.postId is None:
            self.postId = api.metaWeblog.newPost('', cfg.loginName, cfg.password, self.item, self.publish)
        else:
            api.metaWeblog.editPost(self.postId, cfg.loginName, cfg.password, self.item, self.publish)
        msg = "%s sent as %s !" % (self.itemType.capitalize(), self.status)
        print >> sys.stdout,msg

    def _postExec(self):
        UBCmdSend.doDefault()

        if self.postId != ub_get_meta('post_id'):
            ub_set_meta('post_id', self.postId)
        if self.status != ub_get_meta('status'):
            ub_set_meta('status', self.status)

        saveit = ub_get_option('ub_save_after_sent')
        if '1'==vim.eval('&modified') and saveit is not None and saveit.isdigit() and int(saveit) == 1:
            ub_save_item()
        
        evt = eval("UBPostSendEvent(%s)" % self.postId)
        UBEventQueue.fireEvent(evt)
        UBEventQueue.processEvents()

    def _loadPost(self):
        '''Send the current buffer to the blog
        '''
        self.item = dict(\
            title = ub_get_meta('title'),
            description = ub_get_html(),
            categories = [cat.strip() for cat in ub_get_meta('categories').split(',')],
            mt_keywords = ub_get_meta('tags'),
            wp_slug = ub_get_meta('slug'),
            post_type = 'post',
            post_status = self.status
        )

    def _loadPage(self):
        '''Send the current page to the blog
        '''
        self.item = dict(\
            title = ub_get_meta('title'),
            description = ub_get_html(),
            wp_slug = ub_get_meta('slug'),
            post_type = 'page',
            page_status = self.status
        )

class UBCmdOpen(UBCommand):
    ''' Open an item
    '''
    def __init__(self, itemKey, itemType, scope='local', viewType=None):
        UBCommand.__init__(self)
        self.itemKey = itemType=='tmpl' and itemKey.decode(self.enc) or int(itemKey)
        self.itemType = itemType
        self.scope = scope
        self.viewType = viewType
        self.saveIt = ub_get_option('ub_save_after_opened', True)
        self.metaData = None
        self.item = None

    def _exec(self):
        eval("self._load%s%s()" % (self.scope.capitalize(), self.itemType.capitalize()))
        ub_wise_open_view('%s_edit' % self.itemType, self.viewType)
        self.metaData = self.item.getMetaDict(self.enc)
        ub_fill_meta_data(self.metaData)
        vim.current.buffer.append(self.item.content.encode(self.enc).split("\n"))

    def _postExec(self):
        UBCmdOpen.doDefault()

        vim.command('setl filetype=%s' % self.item.syntax)
        vim.command('setl wrap')
        vim.command('call UBClearUndo()')
        if self.itemType=='tmpl' or ub_is_id(self.item.id): vim.command('setl nomodified')
        vim.current.window.cursor = (len(self.metaData)+3, 0)

    def _loadLocalPost(self):
        '''Open local post
        '''
        self.item = self.sess.query(Post).filter(Post.id==self.itemKey).first()
        if self.item is None: raise UBException('No post found !')

    def _loadLocalPage(self):
        '''Open local page
        '''
        self.item = self.sess.query(Post).filter(Post.id==self.itemKey).filter(Post.type=='page').first()
        if self.item is None: raise UBException('No page found !')

    def _loadRemotePost(self):
        '''Open remote post
        '''
        self.item = self.sess.query(Post).filter(Post.post_id==self.itemKey).first()

        # Fetch the remote post if there is not a local copy
        if self.item is None:
            remote_post = api.metaWeblog.getPost(self.itemKey, cfg.loginName, cfg.password)
            self.item = Post()
            self.item.post_id = self.itemKey
            self.item.title = remote_post['title']
            self.item.content = remote_post['description']
            self.item.categories = ', '.join(remote_post['categories'])
            self.item.tags = remote_post['mt_keywords']
            self.item.slug = remote_post['wp_slug']
            self.item.status = remote_post['post_status']
            self.item.syntax = 'html'

            if self.saveIt is True:
                self.sess.add(self.item)
                self.sess.commit()

    def _loadRemotePage(self):
        '''Open remote page
        '''
        self.item = self.sess.query(Post).filter(Post.post_id==self.itemKey).filter(Post.type=='page').first()

        # Fetch the remote page if there is not a local copy
        if self.item is None:
            remote_page = api.wp.getPage('', self.itemKey, cfg.loginName, cfg.password)
            self.item = Post()
            self.item.type = 'page'
            self.item.post_id = self.itemKey
            self.item.title = remote_page['title']
            self.item.content = remote_page['description']
            self.item.slug = remote_page['wp_slug']
            self.item.status = remote_page['page_status']
            self.item.syntax = 'html'

            if self.saveIt is True:
                self.sess.add(self.item)
                self.sess.commit()

    def _loadLocalTmpl(self):
        '''Open template
        '''
        self.item = self.sess.query(Template).filter(Template.name==self.itemKey).first()
        if self.item is None: raise UBException('No template found !')
        self.item.syntax = 'html'

class UBCmdPreview(UBCommand):
    ''' Preview command
    '''
    def __init__(self, tmpl=None):
        UBCommand.__init__(self, True)
        self.tmpl = tmpl is not None and tmpl or ub_get_option('ub_default_template')
        self.viewScopes = ['post_edit', 'page_edit']

    def _exec(self):
        prv_url = ''
        if self.tmpl in ['private', 'publish', 'draft']:
            ub_send_item(self.tmpl)

            if ub_is_view('page_edit'):
                prv_url = "%s?page_id=%s&preview=true"
            else:
                prv_url = "%s?p=%s&preview=true"

            prv_url = prv_url % (cfg.url, ub_get_meta('post_id'))
        else:
            template = self.sess.query(Template).filter(Template.name==self.tmpl.decode(self.enc)).first()
            if template is None:
                raise UBException("Template '%s' is not found !" % self.tmpl)

            tmpl_str = template.content.encode(self.enc)

            draft = {}
            draft['title'] = ub_get_meta('title')
            draft['content'] = ub_get_html()

            tmpfile = tempfile.mktemp(suffix='.html')
            fp = open(tmpfile, 'w')
            fp.write(tmpl_str % draft)
            fp.close()
            prv_url = "file://%s" % tmpfile

        webbrowser.open(prv_url)

class UBCmdDelete(UBCommand):
    def __init__(self, itemType, itemKey, scope='local'):
        UBCommand.__init__(self)
        self.itemType = itemType
        self.itemKey = itemType=='tmpl' and itemKey or int(itemKey)
        self.scope = scope
        self.itemTypeName = ub_get_item_type_name(self.itemType)
        self.itemName = self.itemKey

        self.item = None
        if self.itemType == 'tmpl':
            self.item = self.sess.query(Template).filter(Template.name==self.itemKey.decode(self.enc)).first()
        elif self.scope=='local':
            self.item = self.sess.query(Post).filter(Post.type==self.itemType).filter(Post.id==self.itemKey).first()

    def _preExec(self):
        UBCmdDelete.doDefault()
        # When deleting local items, check if it exists
        if (self.scope=='local' or self.itemType=='tmpl'):
            if self.item is None:
                raise UBException('Cannot find %s by key value %s !' % (self.itemTypeName,self.itemKey))
            else:
                self.itemName = self.item.getName(self.enc)
        # Ask for confirmation
        choice = vim.eval("confirm('Are you sure to delete %s %s \"%s\" ?', '&Yes\n&No')" % (self.scope.encode(self.enc), self.itemTypeName.encode(self.enc), self.itemName))
        if choice != '1': raise UBException('Deletion canceled !')

    def _exec(self):
        try:
            if self.itemType == 'tmpl':
                self.sess.query(Template).filter(Template.name==self.itemKey.decode(self.enc)).delete()
                UBEventQueue.fireEvent(UBTmplDelEvent(self.itemKey))
            else:
                if self.scope=='remote':
                    if self.itemType=='page':
                        api.wp.deletePage('', cfg.loginName, cfg.password, self.itemKey)
                    else:
                        api.metaWeblog.deletePost('', self.itemKey, cfg.loginName, cfg.password)
                    UBEventQueue.fireEvent(UBRemotePostDelEvent(self.itemKey))
                else:
                    self.sess.query(Post).filter(Post.type==self.itemType).filter(Post.id==self.itemKey).delete()
                    UBEventQueue.fireEvent(UBLocalPostDelEvent(self.itemKey))
        except Exception,e:
            self.sess.rollback()
            self.sess.close()
            raise e
        else:
            self.sess.commit()
            UBEventQueue.processEvents()
            print >> sys.stdout, '%s %s "%s" was deleted !' % (self.scope.capitalize().encode(self.enc), self.itemTypeName.encode(self.enc), self.itemName)

class UBCmdOpenItemUnderCursor(UBCommand):
    def __init__(self, viewType=None):
        UBCommand.__init__(self)
        self.viewType = viewType
        self.viewScopes = ['list']

        lineParts = vim.current.line.split()
        if ub_is_cursorline_valid('template'):
            self.itemKey = lineParts[0]
        elif ub_is_cursorline_valid('general'):
            if self.scope == 'local':
                self.itemKey = int(lineParts[0])
                self.itemType = self.sess.query(Post.type).filter(Post.id==self.itemKey).first()[0]
            else:
                self.itemKey = int(lineParts[1])
        else: raise UBException('This is not an item !')

    def _exec(self):
        cmd = UBCmdOpen(itemKey=self.itemKey, itemType=self.itemType, scope=self.scope, viewType=self.viewType)
        cmd.execute()

class UBCmdDelItemUnderCursor(UBCommand):
    def __init__(self):
        UBCommand.__init__(self)
        self.viewScopes = ['list']
        self.postId = None

        lineParts = vim.current.line.split()
        if ub_is_cursorline_valid('template'):
            self.itemKey = lineParts[0]
        elif ub_is_cursorline_valid('general'):
            self.itemKey = int(lineParts[0])
            self.postId = int(lineParts[1])
            rslt = self.sess.query(Post.type).filter(
                    or_(
                        and_(Post.id>0, Post.id==self.itemKey), 
                        and_(Post.post_id>0, Post.post_id==self.postId)
                    )
                ).first()
            self.itemType = rslt is not None and rslt[0] or self.itemType
        else: raise UBException('This is not an item !')

    def _exec(self):
        if ub_is_id(self.itemKey, True) or self.itemType=='tmpl':
            ub_del_item(self.itemType, self.itemKey, 'local')
        if ub_is_id(self.postId, True):
            ub_del_item(self.itemType, self.postId, 'remote')

class UBCmdNew(UBCommand):
    def __init__(self, itemType='post', mixed='markdown'):
        UBCommand.__init__(self, True)

        self.itemType = itemType
        self.syntax = mixed
        if self.itemType=='tmpl':
            self.itemKey = mixed
            self.syntax = 'html'

    def _exec(self):
        eval('self._createNew%s()' % self.itemType.capitalize())

    def _createNewPost(self):
        item = Post()
        metaData = item.getMetaDict()
        metaData['categories'] = self.__getCategories()
        metaData['status'] = 'draft'

        ub_wise_open_view('post_edit')
        ub_fill_meta_data(metaData)
        self.__appendPromotionLink()

    def _createNewPage(self):
        item = Post()
        item.type = 'page'
        metaData = item.getMetaDict()
        metaData['status'] = 'draft'

        ub_wise_open_view('page_edit')
        ub_fill_meta_data(metaData)

    def _createNewTmpl(self):
        # Check if the given name is a reserved word
        ub_check_reserved_word(self.itemKey)
        # Check if the given name is already existing
        if self.sess.query(Template).filter(Template.name==self.itemKey.decode(self.enc)).first() is not None:
            self.sess.close()
            raise UBException('Template "%s" exists !' % self.itemKey)

        item = Template()
        metaData = item.getMetaDict()
        metaData['name'] = self.itemKey
        ub_wise_open_view('tmpl_edit')
        ub_fill_meta_data(metaData)
        fw = '''<html>
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

    def _postExec(self):
        UBCmdNew.doDefault()

        vim.command('setl filetype=%s' % self.syntax)
        vim.command('setl nowrap')
        vim.command('call UBClearUndo()')
        vim.command('setl nomodified')
        if self.itemType=='tmpl': vim.current.window.cursor = (3, len(vim.current.buffer[2])-1)
        else: vim.current.window.cursor = (4, len(vim.current.buffer[3])-1)

    def __getCategories(self):
        cats = api.metaWeblog.getCategories('', cfg.loginName, cfg.password)
        return ', '.join([cat['description'].encode(self.enc) for cat in cats])

    def __appendPromotionLink(self):
        doit = ub_get_option('ub_append_promotion_link')
        if doit is not None and doit.isdigit() and int(doit) == 1:
            if self.syntax == 'markdown':
                link = 'Posted via [UltraBlog.vim](%s).' % cfg.homepage
            else:
                link = 'Posted via <a href="%s">UltraBlog.vim</a>.' % cfg.homepage
            vim.current.buffer.append(link)

class UBCmdBlogThis(UBCommand):
    def __init__(self, itemType='post', toSyntax=None, fromSyntax=None):
        UBCommand.__init__(self, True)

        self.itemType = itemType
        self.toSyntax = toSyntax is not None and toSyntax or self.syntax
        self.syntax = fromSyntax is not None and fromSyntax or self.toSyntax

    def _preExec(self):
        UBCmdBlogThis.doDefault()
        self.checkSyntax(self.toSyntax)

    def _exec(self):
        bf = vim.current.buffer[:]
        ub_new_item(self.itemType, self.toSyntax)
        regex_meta_end = re.compile('^\s*-->')
        for line_num in range(0, len(vim.current.buffer)):
            line = vim.current.buffer[line_num]
            if regex_meta_end.match(line): break
        vim.current.buffer.append(ub_convert_str("\n".join(bf), self.syntax, self.toSyntax, self.enc).split("\n"), line_num+1)

class UBCmdConvert(UBCommand):
    def __init__(self, toSyntax, fromSyntax=None):
        UBCommand.__init__(self, True)
        self.toSyntax = toSyntax
        self.syntax = fromSyntax is not None and fromSyntax or self.syntax

    def _preExec(self):
        UBCmdConvert.doDefault()
        self.checkSyntax(self.toSyntax)

    def _exec(self):
        content = ub_get_content()
        content = ub_convert_str(content, self.syntax, self.toSyntax, self.enc)
        ub_set_content(content.split("\n"))
        vim.command('setl filetype=%s' % self.toSyntax)

class UBCmdRefresh(UBCommand):
    def __init__(self):
        UBCommand.__init__(self)
        self.viewScopes = ['list','edit']

    def _exec(self):
        if self.viewName == 'search_result_list':
            kws = ub_get_bufvar('ub_keywords')
            pno = ub_get_bufvar('page_no')
            ub_find(pno, *kws)
        elif ub_is_view_of_type('list'):
            psize = ub_get_bufvar('page_size')
            pno = ub_get_bufvar('page_no')
            ub_list_items(self.itemType, self.scope, psize, pno)
        elif ub_is_view_of_type('edit'):
            itemKey = self.itemType=='tmpl' and ub_get_meta('name') or ub_get_meta('id')
            if itemKey is not None:
                modified = '1'==vim.eval('&modified')
                vim.command('setl nomodified')
                try:
                    ub_open_item(self.itemType, itemKey, 'local')
                except Exception, e:
                    vcmd = modified is True and 'setl modified' or 'setl nomodified'
                    vim.command(vcmd)
                    print >> sys.stderr,str(e)
            else:
                raise UBException('Cannot find key value of the current buffer !')

plugin/ultrablog/db.py	[[[1
209
#!/usr/bin/env python

import os,xmlrpclib,sys
import util

try:
    import sqlalchemy
    from sqlalchemy import Table, Column, Integer, Text, String
    from sqlalchemy.ext.declarative import declarative_base
    from sqlalchemy.orm import sessionmaker
    from sqlalchemy.sql import union_all,select,case,and_,or_,not_
    from sqlalchemy.exc import OperationalError

    Base = declarative_base()
    Session = sessionmaker()

    class Item(object):
        def getKey(self, encoding=None): pass
        def getName(self, encoding=None): pass
        def getKeyProperty(self): pass
        def getNameProperty(self): pass
        def getMetaDict(self, encoding=None): pass
        def _encode(self, val, encoding): return encoding is not None and val.encode(encoding) or val

    class Post(Base,Item):
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

        def getKey(self, encoding=None):
            return self.id
        def getName(self, encoding=None):
            return encoding is not None and self.title.encode(encoding) or self.title
        def getKeyProperty(self):
            return 'id'
        def getNameProperty(self):
            return 'title'
        def getMetaDict(self, encoding=None):
            meta = dict(\
                id = self.id is not None and self.id or 0,
                post_id = self.post_id is not None and self.post_id or 0,
                title = self.title is not None and self._encode(self.title, encoding) or '',
                categories = self.categories is not None and self._encode(self.categories, encoding) or '',
                tags = self.tags is not None and self._encode(self.tags, encoding) or '',
                slug = self.slug is not None and self._encode(self.slug, encoding) or '',
                status = self.status is not None and self._encode(self.status, encoding) or '')
            if self.type=='page': del meta['categories'], meta['tags']
            return meta

    class Template(Base,Item):
        __tablename__ = 'template'

        name = Column('name', String(32), primary_key=True)
        description = Column('description', String(256))
        content = Column('content', Text)

        def getKey(self, encoding=None):
            return self._encode(self.name, encoding)
        def getName(self, encoding=None):
            return self._encode(self.name, encoding)
        def getKeyProperty(self):
            return 'name'
        def getNameProperty(self):
            return 'name'
        def getMetaDict(self, encoding=None):
            return dict(\
                name = self.name is not None and self._encode(self.name, encoding) or '',
                description = self.description is not None and self._encode(self.description, encoding) or '')

except ImportError, e:
    sqlalchemy = None
    Base = None
    Session = None
    Post = None
    Template = None
except:pass

def ub_upgrade():
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

def ub_init_template():
    if db is not None:

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

            table,td,th {border-collapse: collapse;}
            table
            {
                border-left: 1px solid #333;
                border-bottom: 1px solid #333;
            }
            td,th
            {
                border-top: 1px solid #333;
                border-right: 1px solid #333;
            }
            th {background-color:#ebeff9;}
            td {padding: 5px;}

            blockquote {border: 1px dashed #333; background-color: #B7D0DB; padding: 10px;}
            img {margin-left:auto;margin-right:auto;padding:10px;border:1px solid #000;-moz-box-shadow:3px 3px 4px #000;-webkit-box-shadow:3px 3px 4px #000;box-shadow:3px 3px 4px #000;background:#fff;filter:progid:DXImageTransform.Microsoft.Shadow(Strength=4,Direction=135,Color='#000000');}
            a img{padding:10px;border:1px solid #000;-moz-box-shadow:3px 3px 4px #000;-webkit-box-shadow:3px 3px 4px #000;box-shadow:3px 3px 4px #000;background:#fff;filter:progid:DXImageTransform.Microsoft.Shadow(Strength=4,Direction=135,Color='#000000');}

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

def ub_set_mode():
    '''Set editor mode according to the option ub_editor_mode
    '''
    editor_mode = util.ub_get_option('ub_editor_mode')
    if '1' == editor_mode:
        Session.configure(bind=db)
        Base.metadata.create_all(db)
        ub_init_template()

cfg = None
try:
    cfg = util.ub_get_blog_settings()
except KeyError,e:
    print >> sys.stdout,'Missing key %s in the settings list of UltraBlog.vim !' % str(e)
except:
    pass

try:
    api = xmlrpclib.ServerProxy(cfg.xmlrpc)
    db = sqlalchemy.create_engine("sqlite:///%s" % cfg.dbf)

    Session.configure(bind=db)
    Base.metadata.create_all(db)

    ub_upgrade()
    ub_init_template()
except:
    api = None
    db  = None
plugin/ultrablog/eventqueue.py	[[[1
24
#!/usr/bin/env python

class UBEventQueue:
    queue = []
    listeners = []

    @classmethod
    def fireEvent(cls, evt):
        cls.queue.append(evt)

    @classmethod
    def processEvents(cls):
        for evt in cls.queue:
            for listener in cls.listeners:
                if listener.isTarget(evt):
                    cls.queue.remove(evt)
                    listener.processEvent(evt)

    @classmethod
    def registerListener(cls, lsnr):
        cls.listeners.append(lsnr)

if __name__ == '__main__':
    pass
plugin/ultrablog/events.py	[[[1
18
#!/usr/bin/env python

class UBEvent:
    def __init__(self, srcObj):
        self.srcObj = srcObj

class UBDebugEvent(UBEvent): pass

class UBTmplDelEvent(UBEvent): pass
class UBTmplSaveEvent(UBEvent): pass

class UBLocalPostDelEvent(UBEvent): pass
class UBRemotePostDelEvent(UBEvent): pass
class UBPostSendEvent(UBEvent): pass
class UBPostSaveEvent(UBEvent): pass

if __name__ == '__main__':
    pass
plugin/ultrablog/exceptions.py	[[[1
5
#!/usr/bin/env python

class UBException(Exception):
    pass

plugin/ultrablog/listeners.py	[[[1
147
#!/usr/bin/env python

from exceptions import *
from util import *
from commands import *
from events import *
from eventqueue import UBEventQueue

class UBListener():
    ''' Parent class of all listeners
    '''
    eventType = None

    @classmethod
    def isTarget(cls, evt):
        return isinstance(evt, cls.eventType)

    @staticmethod
    def processEvent(evt): pass

class UBDebugListener(UBListener):
    ''' Debugging Listener
    '''
    eventType = UBDebugEvent
    
    @staticmethod
    def processEvent(evt):
        print evt.srcObj

class UBTmplDelListener(UBListener):
    ''' Listener for templates deletion events
    1. Delete all buffers which are edit views of the deleted template
    2. Refresh the current view if it is a list view of templates
    3. Mark all other template list views outdated
    '''
    eventType = UBTmplDelEvent

    @staticmethod
    def processEvent(evt):
        for nr in ub_get_buffers(['local_tmpl_list']):
            if nr == ub_get_bufnr('%'):
                ub_refresh_current_view()
            else:
                ub_set_view_outdated(nr)

        for nr in ub_get_buffers(['tmpl_edit']):
            if evt.srcObj == ub_get_meta('name', nr):
                vim.command('bd! %d' % nr)

class UBTmplSaveListener(UBListener):
    ''' Listener for templates creation events
    1. Refresh the current view if it is a list view of templates
    2. Mark all other template list views outdated
    '''
    eventType = UBTmplSaveEvent
    
    @staticmethod
    def processEvent(evt):
        for nr in ub_get_buffers(['local_tmpl_list']):
            if nr == ub_get_bufnr('%'):
                ub_list_templates()
            else:
                ub_set_view_outdated(nr)

class UBRemotePostDelListener(UBListener):
    ''' Listener for remote posts/pages deletion events
    1. Reset the value of post_id column to 0 in the database
    2. Refresh the current view if it is an edit/list view of this post
    3. Mark all edit/list views of posts/pages outdated
    '''
    eventType = UBRemotePostDelEvent

    @staticmethod
    def processEvent(evt):
        sess = Session()
        sess.query(Post).filter(Post.post_id==evt.srcObj).update({Post.post_id:None})
        sess.commit()

        for nr in ub_get_buffers(['post_list','post_edit','page_list','page_edit','search_result_list']):
            if nr == ub_get_bufnr('%'):
                ub_refresh_current_view()
            else:
                ub_set_view_outdated(nr)

class UBLocalPostDelListener(UBListener):
    ''' Listener for local posts/pages deletion events
    1. Delete all buffers that hold the deleted local post/page
    2. Refresh the current view if it is an list view of this post
    3. Mark all list views of posts/pages outdated
    '''
    eventType = UBLocalPostDelEvent

    @staticmethod
    def processEvent(evt):
        for nr in ub_get_buffers(['post_edit','page_edit']):
            if evt.srcObj == ub_get_meta('id', nr):
                vim.command('bd! %d' % nr)

        for nr in ub_get_buffers(['post_list','page_list','search_result_list']):
            if nr == ub_get_bufnr('%'):
                ub_refresh_current_view()
            else:
                ub_set_view_outdated(nr)

class UBPostSaveListener(UBListener):
    ''' Listener for saving posts/pages
    1. Refresh the current view if it is an edit/list view of this post
    2. Mark all edit/list views of posts/pages outdated
    '''
    eventType = UBPostSaveEvent
    
    @staticmethod
    def processEvent(evt):
        for nr in ub_get_buffers(['post_edit','page_edit']):
            if evt.srcObj==ub_get_meta('id', nr):
                if nr==ub_get_bufnr('%'):
                    ub_refresh_current_view()
                else:
                    ub_set_view_outdated(nr)

        for nr in ub_get_buffers(['post_list','page_list','search_result_list']):
            if nr == ub_get_bufnr('%'):
                ub_refresh_current_view()
            else:
                ub_set_view_outdated(nr)

class UBPostSendListener(UBListener):
    ''' Listener for sending posts/pages
    1. Mark all remote list views outdated
    '''
    eventType = UBPostSendEvent

    @staticmethod
    def processEvent(evt):
        for nr in ub_get_buffers(['remote_post_list','remote_page_list']):
            ub_set_view_outdated(nr)

UBEventQueue.registerListener(UBDebugListener)
UBEventQueue.registerListener(UBTmplDelListener)
UBEventQueue.registerListener(UBTmplSaveListener)
UBEventQueue.registerListener(UBLocalPostDelListener)
UBEventQueue.registerListener(UBRemotePostDelListener)
UBEventQueue.registerListener(UBPostSendListener)
UBEventQueue.registerListener(UBPostSaveListener)

if __name__ == '__main__':
    pass
plugin/ultrablog/util.py	[[[1
555
#!/usr/bin/env python

import vim,re,types,os
from exceptions import *
try:
    import markdown
except ImportError:
    try:
        import markdown2 as markdown
    except ImportError:
        markdown = None

def ub_wise_open_view(view_name=None, view_type=None):
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

def ub_clear_buffer(expr, force=False):
    ''' Clear the specified buffer and reset related statuses
    '''
    nr = ub_get_bufnr(expr)
    if nr is None: return

    if '1' == vim.eval('&modified'):
        if force is True:
            vim.command('setl nomodified')
        else:
            raise UBException('The buffer has been changed and cannot be cleared !')

    vim.command('setl modifiable')
    del vim.buffers[nr-1][:]
    vim.command('setl nomodified')

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

def ub_check_reserved_word(rw):
    ''' Check if the given parameter is a reserved word
    '''
    try: ub_check_status(rw)
    except UBException: pass
    else: raise UBException("'%s' is a reserved word !" % rw)

def ub_is_valid_syntax(syntax):
    '''Check if the given parameter is one of the supported syntaxes
    '''
    return ['markdown', 'html', 'rst', 'latex', 'textile'].count(syntax) == 1

def ub_is_url(url):
    ''' Check if the given string is a valid URL
    '''
    regex = re.compile('^http:\/\/[0-9a-zA-Z]+(\.[0-9a-zA-Z]+)+')
    return regex.match(url) is not None

def ub_is_ubbuf(expr):
    ''' Check if the given buffer number exists and is a buffer of UltraBlog.vim
    '''
    return ub_get_viewname(expr) is not None

def ub_is_view_outdated(expr):
    ''' Check if the given view is outdated
    '''
    nr = ub_get_bufnr(expr)
    if nr is None:
        nr = -1
    return '1' == vim.eval("getbufvar(%d, 'ub_view_is_outdated')" % nr)

def ub_is_view(view_name, expr='%'):
    '''Check if the current view is named by the given parameter
    '''
    nr = ub_get_bufnr(expr)
    if nr is not None:
        return view_name == vim.eval("getbufvar(%d, 'ub_view_name')" % nr)
    return False

def ub_is_view_of_type(view_type, expr='%'):
    '''Check if the type of current view is the same with the given parameter
    '''
    vname = ub_get_viewname(expr)
    if vname is not None:
        return vname.endswith(view_type)
    return False

def ub_is_id(id, strict=False):
    ''' Check if the given parameter is a positive integer
    '''
    if strict is True and type(id) is not types.IntType:
        return False
    return (type(id) is types.IntType and id>0) or (type(id) is types.StringType and id.isdigit() and int(id)>0)

def ub_is_emptystr(str):
    ''' Check if the given parameter is an empty string
    '''
    return type(str) is types.StringType and len(str.strip())==0

def ub_is_cursorline_valid(line_type):
    ''' Check if the cursor line is a normal item line,
    valid types are 'template', 'post', 'page', 'general'
    '''
    parts = vim.current.line.split()
    if line_type=='template':
        return ub_is_view('local_tmpl_list') and vim.current.window.cursor[0]>1 and len(parts)>0
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

def ub_get_item_type_name(type):
    ''' Get item type name by type
    '''
    if type == 'tmpl':
        return 'template'
    elif type == 'result':
        return 'post'
    return type

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
    elif opt == 'ub_search_pagesize':
        val = 30
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
        elif opt == 'ub_save_after_opened':
            val = ('1'==val and True) or False

    return val

def ub_get_meta(item, buf=None):
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

    nr = ub_get_bufnr(buf)
    if nr is None: nr = int(vim.eval("bufnr('%')"))
    regex_meta_end = re.compile('^\s*-->')
    regex_item = re.compile('^\$'+item+':\s*')
    for line in vim.eval("getbufline(%d,0,'$')" % nr):
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

def ub_get_buffers(viewnames=None):
    ''' Return a list of buffer numbers which belongs to UltraBlog.vim
    If parameter viewnames is given, buffers which has the given name will be returned
    '''
    bufs = []
    for nr in range(int(vim.eval("bufnr('$')"))+1):
        if viewnames is None and ub_is_ubbuf(nr):
            bufs.append(nr)
        if type(viewnames) is types.ListType:
            for viewname in viewnames:
                if ub_is_view_of_type(viewname, nr):
                    bufs.append(nr)
    return bufs

def ub_get_bufvar(key, expr='%'):
    ''' Return the value of the given buffer variable
    '''
    nr = ub_get_bufnr(expr)
    if nr is None: return None
    return vim.eval("getbufvar(%d, '%s')" % (nr,key))

def ub_get_viewname(expr):
    ''' Return the value of variable b:ub_view_name in buffer nr
    '''
    return ub_get_bufvar('ub_view_name', expr)

def ub_get_bufnr(expr):
    ''' Return the buffer number which matches the given expression
    '''
    nr = None
    if type(expr) is types.IntType:
        nr = vim.eval("bufnr(%d)" % expr)
    elif type(expr) is types.StringType:
        if expr.isdigit():
            nr = vim.eval("bufnr(%d)" % expr)
        else:
            nr = vim.eval("bufnr('%s')" % expr)

    if type(nr) is types.StringType and nr.isdigit() and int(nr)>0 and expr not in [0,'0']:
        return int(nr)
    return None

def ub_get_blog_settings():
    '''Get the blog settings from vimrc and raise exception if none found
    '''
    class UBConfiguration:
        homepage = 'http://sinolog.it/?p=1894'

        def __init__(self, rawSettings):
            self.loginName = rawSettings['login_name'].strip()
            self.password = rawSettings['password'].strip()
            self.dbf = rawSettings['db'].strip()
            self.url = rawSettings['url'].strip()
            self.url = self.url.endswith('/') and self.url or self.url+'/'
            self.xmlrpc = self.url+'xmlrpc.php'

    if vim.eval('exists("ub_blog")') == '0':
        return None

    settings = vim.eval('ub_blog')
    cfg = UBConfiguration(settings)

    # Manipulate db file path
    editor_mode = ub_get_option('ub_editor_mode')
    if editor_mode is not None and editor_mode.isdigit() and int(editor_mode) == 1:
        cfg.dbf = ''
    elif cfg.dbf is None or cfg.dbf=='':
        cfg.dbf = os.path.normpath(os.path.expanduser('~')+'/.vim/UltraBlog.db')
    else:
        cfg.dbf = os.path.abspath(vim.eval("expand('%s')" % cfg.dbf))

    return cfg

def ub_set_view_outdated(expr, outdated=True):
    ''' Set the specified view to be outdated
    '''
    nr = ub_get_bufnr(expr)
    val = (outdated is True and 1) or 0
    if nr is not None:
        vim.command("call setbufvar(%d,'ub_view_is_outdated',%d)" % (nr,val))

def ub_fill_meta_data(meta_data):
    if ub_is_view('post_edit'):
        __ub_fill_post_meta_data(meta_data)
    elif ub_is_view('page_edit'):
        __ub_fill_page_meta_data(meta_data)
    elif ub_is_view('tmpl_edit'):
        __ub_fill_tmpl_meta_data(meta_data)
    else:
        raise UBException('Unknown view !')

def __ub_fill_post_meta_data(meta_dict):
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

def __ub_fill_page_meta_data(meta_dict):
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

def __ub_fill_tmpl_meta_data(meta_dict):
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

def ub_get_html(body_only=True):
    '''Generate HTML string from the current buffer
    '''
    content = ub_get_content()
    syntax = vim.eval('&syntax')
    enc = vim.eval('&encoding')
    html = ub_convert_str(content, syntax, 'html', enc)

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

def ub_get_post_meta_data():
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

def ub_get_page_meta_data():
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

def ub_get_tmpl_meta_data():
    return dict(\
        name = ub_get_meta('name'),
        description = ub_get_meta('description')
    )

def ub_get_content():
    '''Generate content from the current buffer
    '''
    if ub_is_view('post_edit'):
        meta_dict = ub_get_post_meta_data()
    elif ub_is_view('page_edit'):
        meta_dict = ub_get_page_meta_data()
    elif ub_is_view('tmpl_edit'):
        meta_dict = ub_get_tmpl_meta_data()
    else:
        return "\n".join(vim.current.buffer[:])

    return "\n".join(vim.current.buffer[len(meta_dict)+2:])

def ub_set_content(lines):
    '''Set the given lines to the content area of the current buffer
    '''
    meta_dict = None
    if ub_is_view('post_edit'):
        meta_dict = ub_get_post_meta_data()
    elif ub_is_view('page_edit'):
        meta_dict = ub_get_page_meta_data()
    elif ub_is_view('tmpl_edit'):
        meta_dict = ub_get_tmpl_meta_data()

    idx = meta_dict is not None and len(meta_dict)+2 or 0
    del vim.current.buffer[idx:]
    vim.current.buffer.append(lines, idx)
    return True

def ub_convert_str(content, from_syntax, to_syntax, encoding=None):
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

def ub_get_templates(name_only=False):
    ''' Fetch and return a list of templates
    '''
    tmpls = []

    try:
        sess = Session()
        tmpls = sess.query(Template).all()
        sess.close()

        if name_only is True: tmpls = [tmpl.name for tmpl in tmpls]
    except:
        pass

    return tmpls

if __name__ == '__main__':
    pass
