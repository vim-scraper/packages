" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/quickfixsigns.txt	[[[1
269
*quickfixsigns.txt*   Mark quickfix & location list items with signs
                      Author: Thomas Link, micathom at gmail com

Mark interesting lines with signs:

    - items in the quickfix and location lists (e.g. compilation errors)
    - marks a-zA-Z
    - relative line numbers
    - changes (with respect to the version in the VCS)

Other lists can be configured via the |g:quickfixsigns_lists| variable.

The attached text is displayed in a balloon via 'balloonexpr'. This may 
conflict with the use of this feature by other plugins.

If you want nicer looking images instead of the ASCII markers, you have 
to change the following signs' definition in your |vimrc| file (by use
of |sign-define|):

    QFS_QFL ... Entries in the |quickfix| list
    QFS_LOC ... Entries in the |location-list|
    QFS_Mark_[a-zA-Z] ... Marks
    QFS_CURSOR ... Current cursor position
    QFS_BREAKPOINT ... Breakpoint
    QFS_REL_x ... Relative line numbers
    QFS_VCS_{ADD,DEL,CHANGE} ... VCS changes

CREDITS:
quickfixsigns includes some icons from the open icon library. See 
http://openiconlibrary.sourceforge.net for details.


-----------------------------------------------------------------------
Install~

Edit the vba file and type: >

    :so %

See :help vimball for details. If you have difficulties or use vim 7.0, 
please make sure, you have the current version of vimball
(vimscript #1502) installed or update your runtime.


========================================================================
Contents~

        :QuickfixsignsSet ........................ |:QuickfixsignsSet|
        :QuickfixsignsSelect ..................... |:QuickfixsignsSelect|
        g:quickfixsigns_classes .................. |g:quickfixsigns_classes|
        g:quickfixsigns_events ................... |g:quickfixsigns_events|
        g:quickfixsigns_class_rel ................ |g:quickfixsigns_class_rel|
        g:quickfixsigns_class_qfl ................ |g:quickfixsigns_class_qfl|
        g:quickfixsigns_class_loc ................ |g:quickfixsigns_class_loc|
        g:quickfixsigns_class_cursor ............. |g:quickfixsigns_class_cursor|
        g:quickfixsigns_balloon .................. |g:quickfixsigns_balloon|
        g:quickfixsigns_max ...................... |g:quickfixsigns_max|
        g:quickfixsigns_blacklist_buffer ......... |g:quickfixsigns_blacklist_buffer|
        g:quickfixsigns_icons .................... |g:quickfixsigns_icons|
        QuickfixsignsSelect ...................... |QuickfixsignsSelect()|
        QuickfixsignsUpdate ...................... |QuickfixsignsUpdate()|
        QuickfixsignsSet ......................... |QuickfixsignsSet()|
        QuickfixsignsBalloon ..................... |QuickfixsignsBalloon()|
        QuickfixsignsClear ....................... |QuickfixsignsClear()|
        g:quickfixsigns#use_relativenumber ....... |g:quickfixsigns#use_relativenumber|
        quickfixsigns#CompleteSelect ............. |quickfixsigns#CompleteSelect()|
        quickfixsigns#RelNumbersOnce ............. |quickfixsigns#RelNumbersOnce()|
        g:quickfixsigns_class_vcsdiff ............ |g:quickfixsigns_class_vcsdiff|
        g:quickfixsigns#vcsdiff#highlight ........ |g:quickfixsigns#vcsdiff#highlight|
        quickfixsigns#vcsdiff#GuessType .......... |quickfixsigns#vcsdiff#GuessType()|
        quickfixsigns#vcsdiff#GetList ............ |quickfixsigns#vcsdiff#GetList()|
        g:quickfixsigns_class_marks .............. |g:quickfixsigns_class_marks|
        g:quickfixsigns#marks#marks .............. |g:quickfixsigns#marks#marks|
        quickfixsigns#marks#GetList .............. |quickfixsigns#marks#GetList()|
        quickfixsigns#marks#GetSign .............. |quickfixsigns#marks#GetSign()|
        g:quickfixsigns#breakpoints#filetypes .... |g:quickfixsigns#breakpoints#filetypes|
        g:quickfixsigns_class_breakpoints ........ |g:quickfixsigns_class_breakpoints|
        quickfixsigns#breakpoints#GetList ........ |quickfixsigns#breakpoints#GetList()|
        quickfixsigns#breakpoints#Vim ............ |quickfixsigns#breakpoints#Vim()|


========================================================================
plugin/quickfixsigns.vim~

                                                    *:QuickfixsignsSet*
:QuickfixsignsSet
    Reset the signs in the current buffer.

                                                    *:QuickfixsignsSelect*
:QuickfixsignsSelect
    Select the sign classes that should be displayed and reset the signs 
    in the current buffer.

                                                    *g:quickfixsigns_classes*
g:quickfixsigns_classes        (default: ['qfl', 'loc', 'marks', 'vcsdiff', 'breakpoints'])
    A list of sign classes that should be displayed.
    Can be one of:
    
      rel     ... relative line numbers
      cursor  ... current line
      qfl     ... |quickfix| list
      loc     ... |location| list
      vcsdiff ... mark changed lines (see |quickfixsigns#vcsdiff#GetList()|)
      marks   ... marks |'a|-zA-Z (see also " |g:quickfixsigns_marks|)
    
    The sign classes are defined in g:quickfixsigns_class_{NAME}.
    
    A list definition is a |Dictionary| with the following fields:
    
      sign:  The name of the sign, which has to be defined. If the 
             value begins with "*", the value is interpreted as 
             function name that is called with a qfl item as its 
             single argument.
      get:   A vim script expression as string that returns a list 
             compatible with |getqflist()|.
      event: The event on which signs of this type should be set. 
             Possible values: BufEnter, any
      test:  Update the sign only if the expression is true.

                                                    *g:quickfixsigns_events*
g:quickfixsigns_events         (default: ['BufEnter', 'CursorHold', 'CursorHoldI', 'InsertLeave', 'InsertEnter', 'InsertChange'])
    List of events for signs that should be frequently updated.

                                                    *g:quickfixsigns_class_rel*
g:quickfixsigns_class_rel      (default: {'sign': '*s:RelSign', 'get': 's:GetRelList(%s, "rel")', 'event': g:quickfixsigns_events, 'max': 9, 'level': 9})
    Signs for number of lines relative to the current line.
    Since 7.3, vim provides the 'relativenumber' option that provides 
    a similar functionality.
    See also |quickfixsigns#RelNumbersOnce()|.

                                                    *g:quickfixsigns_class_qfl*
g:quickfixsigns_class_qfl      (default: {'sign': 'QFS_QFL', 'get': 's:GetQFList(%s)', 'event': ['BufEnter', 'CursorHold', 'CursorHoldI', 'QuickFixCmdPost'], 'scope': 'vim'})
    Signs for |quickfix| lists.

                                                    *g:quickfixsigns_class_loc*
g:quickfixsigns_class_loc      (default: {'sign': 'QFS_LOC', 'get': 's:GetLocList(%s)', 'event': ['BufEnter', 'CursorHold', 'CursorHoldI']})
    Signs for |location| lists.

                                                    *g:quickfixsigns_class_cursor*
g:quickfixsigns_class_cursor   (default: {'sign': 'QFS_CURSOR', 'get': 's:GetCursor(%s)', 'event': g:quickfixsigns_events})
    Sign for the current cursor position. The cursor position is 
    lazily updated. If you want something more precise, consider 
    setting 'cursorline'.

                                                    *g:quickfixsigns_balloon*
g:quickfixsigns_balloon        (default: 1)
    If non-null, display a balloon when hovering with the mouse over 
    the sign.
    buffer-local or global

                                                    *g:quickfixsigns_max*
g:quickfixsigns_max            (default: 500)
    Don't display signs if the list is longer than n items.

                                                    *g:quickfixsigns_blacklist_buffer*
g:quickfixsigns_blacklist_buffer (default: '^__.*__$')
    Don't show signs in buffers matching this |regexp|.

                                                    *g:quickfixsigns_icons*
g:quickfixsigns_icons          (default: {})
    A dictionary {TYPE => IMAGE_FILENAME} that is used to select 
    icons that should be displayed as signs.
    Defaults to empty on windows and non-gui versions.

                                                    *QuickfixsignsSelect()*
QuickfixsignsSelect(list)

                                                    *QuickfixsignsUpdate()*
QuickfixsignsUpdate(?class="")

                                                    *QuickfixsignsSet()*
QuickfixsignsSet(event, ?classes=[])
    (Re-)Set the signs that should be updated at a certain event. If event 
    is empty, update all signs.
    
    Normally, the end-user doesn't need to call this function.

                                                    *QuickfixsignsBalloon()*
QuickfixsignsBalloon()

                                                    *QuickfixsignsClear()*
QuickfixsignsClear(class)
    Clear all signs with name SIGN.


========================================================================
autoload/quickfixsigns.vim~

                                                    *g:quickfixsigns#use_relativenumber*
g:quickfixsigns#use_relativenumber (default: 1)
    VIM 7.3 and later: If non-zero, |quickfixsigns#RelNumbersOnce()| 
    uses 'relativenumber' instead of signs. This avoids clashes with 
    other signs and is faster, but it could cause the visual text area 
    to be moved to the right.

                                                    *quickfixsigns#CompleteSelect()*
quickfixsigns#CompleteSelect(ArgLead, CmdLine, CursorPos)

                                                    *quickfixsigns#RelNumbersOnce()*
quickfixsigns#RelNumbersOnce()
    Display relative line numbers. Remove the signs when the cursor moves.
    From vim 7.3 on, this uses the 'relativenumber' option.


========================================================================
autoload/quickfixsigns/vcsdiff.vim~

                                                    *g:quickfixsigns_class_vcsdiff*
g:quickfixsigns_class_vcsdiff  (default: {'sign': '*quickfixsigns#vcsdiff#Signs', 'get': 'quickfixsigns#vcsdiff#GetList(%s)', 'event': ['BufEnter,BufWritePost'], 'always_new': 1})

                                                    *g:quickfixsigns#vcsdiff#highlight*
g:quickfixsigns#vcsdiff#highlight (default: {'DEL': 'DiffDelete', 'ADD': 'DiffAdd', 'CHANGE': 'DiffChange'})
    The highlighting of deleted lines can sometimes be confusing. In 
    order to disable the display of signs for DEL changes, save the 
    following line as after/autoload/quickfixsigns/vcsdiff.vim: >
    
      call remove(g:quickfixsigns#vcsdiff#highlight, 'DEL')
<

                                                    *quickfixsigns#vcsdiff#GuessType()*
quickfixsigns#vcsdiff#GuessType()
    Return the name of a VCS system based on the values of the following 
    variables:
      - b:vcs_type
      - b:VCSCommandVCSType

                                                    *quickfixsigns#vcsdiff#GetList()*
quickfixsigns#vcsdiff#GetList(filename)
    quickfixsigns#vcsdiff#GuessType() must return the name of a supported 
    VCS (see |g:quickfixsigns#vcsdiff#cmds|).


========================================================================
autoload/quickfixsigns/marks.vim~

                                                    *g:quickfixsigns_class_marks*
g:quickfixsigns_class_marks    (default: {...})
    The definition of signs for marks.

                                                    *g:quickfixsigns#marks#marks*
g:quickfixsigns#marks#marks    (default: split('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ<>''^.', '\zs'))
    A list of marks that should be displayed as signs. If empty, 
    disable the display of marks.

                                                    *quickfixsigns#marks#GetList()*
quickfixsigns#marks#GetList(filename)

                                                    *quickfixsigns#marks#GetSign()*
quickfixsigns#marks#GetSign(item)


========================================================================
autoload/quickfixsigns/breakpoints.vim~

                                                    *g:quickfixsigns#breakpoints#filetypes*
g:quickfixsigns#breakpoints#filetypes (default: {...})

                                                    *g:quickfixsigns_class_breakpoints*
g:quickfixsigns_class_breakpoints (default: {...})

                                                    *quickfixsigns#breakpoints#GetList()*
quickfixsigns#breakpoints#GetList(filename)

                                                    *quickfixsigns#breakpoints#Vim()*
quickfixsigns#breakpoints#Vim()



vim:tw=78:fo=tcq2:isk=!-~,^*,^|,^":ts=8:ft=help:norl:
plugin/quickfixsigns.vim	[[[1
533
" Mark quickfix & location list items with signs
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @GIT:         http://github.com/tomtom/quickfixsigns_vim/
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-03-14.
" @Last Change: 2011-05-18.
" @Revision:    784
" GetLatestVimScripts: 2584 1 :AutoInstall: quickfixsigns.vim

if &cp || exists("loaded_quickfixsigns") || !has('signs')
    finish
endif
let loaded_quickfixsigns = 14

let s:save_cpo = &cpo
set cpo&vim


" Reset the signs in the current buffer.
command! QuickfixsignsSet call QuickfixsignsSet("")

" Select the sign classes that should be displayed and reset the signs 
" in the current buffer.
command! -nargs=+ -complete=customlist,quickfixsigns#CompleteSelect QuickfixsignsSelect call QuickfixsignsSelect([<f-args>]) | call QuickfixsignsUpdate()


if !exists('g:quickfixsigns_classes')
    " A list of sign classes that should be displayed.
    " Can be one of:
    "
    "   rel     ... relative line numbers
    "   cursor  ... current line
    "   qfl     ... |quickfix| list
    "   loc     ... |location| list
    "   vcsdiff ... mark changed lines (see |quickfixsigns#vcsdiff#GetList()|)
    "   marks   ... marks |'a|-zA-Z (see also " |g:quickfixsigns_marks|)
    "
    " The sign classes are defined in g:quickfixsigns_class_{NAME}.
    "
    " A list definition is a |Dictionary| with the following fields:
    "
    "   sign:  The name of the sign, which has to be defined. If the 
    "          value begins with "*", the value is interpreted as 
    "          function name that is called with a qfl item as its 
    "          single argument.
    "   get:   A vim script expression as string that returns a list 
    "          compatible with |getqflist()|.
    "   event: The event on which signs of this type should be set. 
    "          Possible values: BufEnter, any
    "   test:  Update the sign only if the expression is true.
    let g:quickfixsigns_classes = ['qfl', 'loc', 'marks', 'vcsdiff', 'breakpoints']   "{{{2
    " let g:quickfixsigns_classes = ['rel', 'qfl', 'loc', 'marks']   "{{{2
endif


if !exists('g:quickfixsigns_events')
    " List of events for signs that should be frequently updated.
    let g:quickfixsigns_events = ['BufEnter', 'CursorHold', 'CursorHoldI', 'InsertLeave', 'InsertEnter', 'InsertChange']   "{{{2
endif


if !exists('g:quickfixsigns_class_rel')
    " Signs for number of lines relative to the current line.
    " Since 7.3, vim provides the 'relativenumber' option that provides 
    " a similar functionality.
    " See also |quickfixsigns#RelNumbersOnce()|.
    let g:quickfixsigns_class_rel = {'sign': '*s:RelSign', 'get': 's:GetRelList(%s, "rel")', 'event': g:quickfixsigns_events, 'max': 9, 'level': 9}  "{{{2
endif
let g:quickfixsigns_class_rel2 = copy(g:quickfixsigns_class_rel)
let g:quickfixsigns_class_rel2.get = 's:GetRelList(%s, "rel2")'
let g:quickfixsigns_class_rel2.max = 99


if !exists('g:quickfixsigns_class_qfl')
    " Signs for |quickfix| lists.
    let g:quickfixsigns_class_qfl = {'sign': 'QFS_QFL', 'get': 's:GetQFList(%s)', 'event': ['BufEnter', 'CursorHold', 'CursorHoldI', 'QuickFixCmdPost'], 'scope': 'vim'}   "{{{2
endif


if !exists('g:quickfixsigns_class_loc')
    " Signs for |location| lists.
    let g:quickfixsigns_class_loc = {'sign': 'QFS_LOC', 'get': 's:GetLocList(%s)', 'event': ['BufEnter', 'CursorHold', 'CursorHoldI']}   "{{{2
endif


if !exists('g:quickfixsigns_class_cursor')
    " Sign for the current cursor position. The cursor position is 
    " lazily updated. If you want something more precise, consider 
    " setting 'cursorline'.
    let g:quickfixsigns_class_cursor = {'sign': 'QFS_CURSOR', 'get': 's:GetCursor(%s)', 'event': g:quickfixsigns_events}   "{{{2
endif


if !exists('g:quickfixsigns_balloon')
    " If non-null, display a balloon when hovering with the mouse over 
    " the sign.
    " buffer-local or global
    let g:quickfixsigns_balloon = 1   "{{{2
endif


if !exists('g:quickfixsigns_max')
    " Don't display signs if the list is longer than n items.
    let g:quickfixsigns_max = 500   "{{{2
endif


if !exists('g:quickfixsigns_blacklist_buffer')
    " Don't show signs in buffers matching this |regexp|.
    let g:quickfixsigns_blacklist_buffer = '^__.*__$'   "{{{2
endif


if !exists('g:quickfixsigns_icons')
    if has("gui_running")
        if !has('win16') && !has('win32') && !has('win64')
            let s:icons_dir = expand('<sfile>:p:h:h:') .'/bitmaps/open_icon_library/16x16/'
            if isdirectory(s:icons_dir)
                let g:quickfixsigns_icons = {
                            \ 'qfl': s:icons_dir .'status/dialog-error-5.png',
                            \ 'loc': s:icons_dir .'status/dialog-warning-4.png',
                            \ 'cursor': s:icons_dir .'actions/go-next-4.png',
                            \ 'breakpoint': s:icons_dir .'actions/media-playback-pause-3.png'
                            \ }
            endif
            unlet s:icons_dir
        endif
    endif
    if !exists('g:quickfixsigns_icons')
        " A dictionary {TYPE => IMAGE_FILENAME} that is used to select 
        " icons that should be displayed as signs.
        " Defaults to empty on windows and non-gui versions.
        let g:quickfixsigns_icons = {}   "{{{2
    endif
endif



" ----------------------------------------------------------------------
let s:quickfixsigns_base = 5272
let g:quickfixsigns_register = {}


redir => s:signss
silent sign list
redir END
let g:quickfixsigns_signs = split(s:signss, '\n')
call filter(g:quickfixsigns_signs, 'v:val =~ ''^sign QFS_''')
call map(g:quickfixsigns_signs, 'matchstr(v:val, ''^sign \zsQFS_\w\+'')')

if index(g:quickfixsigns_signs, 'QFS_QFL') == -1
    if exists('g:quickfixsigns_icons.qfl')
        exec 'sign define QFS_QFL text=* texthl=WarningMsg icon='. escape(g:quickfixsigns_icons.qfl, ' \')
    else
        sign define QFS_QFL text=* texthl=WarningMsg
    endif
endif

if index(g:quickfixsigns_signs, 'QFS_LOC') == -1
    if exists('g:quickfixsigns_icons.loc')
        exec 'sign define QFS_LOC text=> texthl=Special icon='. escape(g:quickfixsigns_icons.loc, ' \')
    else
        sign define QFS_LOC text=> texthl=Special
    endif
endif

if index(g:quickfixsigns_signs, 'QFS_CURSOR') == -1
    if exists('g:quickfixsigns_icons.cursor')
        exec 'sign define QFS_CURSOR text=- texthl=Question icon='. escape(g:quickfixsigns_icons.cursor, ' \')
    else
        sign define QFS_CURSOR text=- texthl=Question
    endif
endif

sign define QFS_DUMMY text=. texthl=NonText

let s:relmax = -1
function! s:GenRel(num) "{{{3
    " TLogVAR a:num
    " echom "DBG ". s:relmax
    if a:num > s:relmax && a:num < 100
        for n in range(s:relmax + 1, a:num)
            exec 'sign define QFS_REL_'. n .' text='. n .' texthl=LineNr'
        endfor
        let s:relmax = a:num
    endif
endf


function! QuickfixsignsSelect(list) "{{{3
	" FIXME: unset first
    let g:quickfixsigns_lists = {}
	for what in a:list
        if exists('g:quickfixsigns_class_'. what)
            let g:quickfixsigns_lists[what] = g:quickfixsigns_class_{what}
        endif
	endfor
endf


" :display: QuickfixsignsUpdate(?class="")
function! QuickfixsignsUpdate(...) "{{{3
    let what = a:0 >= 1 ? a:1 : ""
    call QuickfixsignsClear(what)
    call QuickfixsignsSet("")
endf


" :display: QuickfixsignsSet(event, ?classes=[])
" (Re-)Set the signs that should be updated at a certain event. If event 
" is empty, update all signs.
"
" Normally, the end-user doesn't need to call this function.
function! QuickfixsignsSet(event, ...) "{{{3
    if exists("b:noquickfixsigns") && b:noquickfixsigns
        return
    endif
    let filename = a:0 >= 2 ? a:2 : bufname('%')
    " TLogVAR a:event, filename, bufname('%')
    if filename =~ g:quickfixsigns_blacklist_buffer
        return
    endif
    if !exists('b:quickfixsigns_last_line')
        let b:quickfixsigns_last_line = 0
    endif
    " let lz = &lazyredraw
    " set lz
    " try
        let bufnr = bufnr(filename)
        let anyway = empty(a:event)
        " TLogVAR anyway, a:event
        for [key, def] in s:ListValues()
            " TLogVAR key, def
            if anyway
                let set = 1
            elseif index(get(def, 'event', ['BufEnter']), a:event) != -1
                let set = !has_key(def, 'test') || eval(def.test)
            else
                let set = 0
            endif
            if a:0 >= 1 && !empty(a:1)
                let select = index(a:1, key) != -1
            else
                let select = 1
            endif
            if set && select
                " TLogVAR key, set, select
                let t_d = get(def, 'timeout', 0)
                let t_l = localtime()
                let t_s = string(def)
                if !exists('b:quickfixsigns_last_run')
                    let b:quickfixsigns_last_run = {}
                endif
                " TLogVAR t_s, t_d, t_l
                if anyway || (t_d == 0) || (t_l - get(b:quickfixsigns_last_run, t_s, 0) >= t_d)
                    if a:event == 'BufEnter'
                        call s:PruneRegister()
                    endif
                    let b:quickfixsigns_last_run[t_s] = t_l
                    let list = s:GetList(def, filename)
                    " TLogVAR len(list)
                    " TLogVAR key, 'scope == buffer'
                    call filter(list, 's:Scope(key, v:val) == "vim" || v:val.bufnr == bufnr')
                    " TLogVAR list
                    if !empty(list) && len(list) < g:quickfixsigns_max
                        let new_ids = s:PlaceSign(key, def.sign, list)
                        call s:ClearBuffer(key, def.sign, bufnr, new_ids)
                        if has('balloon_eval') && g:quickfixsigns_balloon
                            if exists('g:loaded_tlib') && g:loaded_tlib >= 39
                                call tlib#balloon#Register('QuickfixsignsBalloon()')
                            elseif !exists('b:quickfixsigns_balloon') && empty(&balloonexpr)
                                let b:quickfixsigns_ballooneval = &ballooneval
                                let b:quickfixsigns_balloonexpr = &balloonexpr
                                setlocal ballooneval balloonexpr=QuickfixsignsBalloon()
                                let b:quickfixsigns_balloon = 1
                            endif
                        endif
                    else
                        call s:ClearBuffer(key, def.sign, bufnr, [])
                    endif
                endif
            endif
        endfor
    " finally
    "     if &lz != lz
    "         let &lz = lz
    "     endif
    " endtry
    let b:quickfixsigns_last_line = line('.')
endf


function! s:GetList(def, filename) "{{{3
    let getter = printf(a:def.get, string(a:filename))
    let list = copy(eval(getter))
    return list
endf


function! QuickfixsignsBalloon() "{{{3
    " TLogVAR v:beval_lnum, v:beval_col
    if v:beval_col <= 1
        let lnum = v:beval_lnum
        let bufnr = bufnr('%')
        let bufname = bufname(bufnr)
        let acc = []
        for [key, def] in s:ListValues()
            let list = s:GetList(def, bufname)
            call filter(list, 'v:val.bufnr == bufnr && v:val.lnum == lnum')
            if !empty(list)
                let acc += list
            endif
        endfor
        " TLogVAR acc
        return join(map(acc, 'v:val.text'), "\n")
    endif
    if exists('b:quickfixsigns_balloonexpr') && !empty(b:quickfixsigns_balloonexpr)
        return eval(b:quickfixsigns_balloonexpr)
    else
        return ''
    endif
endf


function! s:GetCursor(bufname) "{{{3
    let pos = getpos('.')
    return [{'bufnr': bufnr('%'), 'lnum': pos[1], 'col': pos[2], 'text': 'Current line'}]
endf


function! s:ListValues() "{{{3
    return sort(items(g:quickfixsigns_lists), 's:CompareClasses')
endf


function! s:CompareClasses(a, b) "{{{3
    let i1 = get(a:a[1], 'level', 5)
    let i2 = get(a:b[1], 'level', 5)
    return i1 == i2 ? 0 : i1 < i2 ? 1 : -1
endf


function! s:RelSign(item) "{{{3
    return 'QFS_'. a:item.text
endf


function! s:GetRelList(bufname, class) "{{{3
	let lnum = line('.')
	let col = col('.')
	let bufnr = bufnr('%')
    let top = line('w0') - lnum
    let bot = line('w$') - lnum
    let max = g:quickfixsigns_class_{a:class}.max
    if max >= 0
        let top = max([top, -max])
        let bot = min([bot, max])
    endif
    " TLogVAR top, bot
    call s:GenRel(max([abs(top), abs(bot)]))
    return map(range(top, bot), '{"bufnr": bufnr, "lnum": lnum + v:val, "col": col, "text": "REL_". abs(v:val)}')
endf


" Clear all signs with name SIGN.
function! QuickfixsignsClear(class) "{{{3
    " TLogVAR a:sign_rx
    let ikeys = keys(g:quickfixsigns_register)
    if !empty(a:class)
        call filter(ikeys, 'g:quickfixsigns_register[v:val].class ==# a:class')
    endif
    " TLogVAR ikeys
    call s:ClearSigns(ikeys)
endf


" Clear all signs with name SIGN in buffer BUFNR.
function! s:ClearBuffer(class, sign, bufnr, new_ikeys) "{{{3
    " TLogVAR a:class, a:sign, a:bufnr, a:new_ikeys
    let old_ikeys = keys(filter(copy(g:quickfixsigns_register), 'v:val.class ==# a:class && index(a:new_ikeys, v:key) == -1 && (s:Scope(a:class, v:val) == "vim" || v:val.bufnr == a:bufnr)'))
    " TLogVAR old_ikeys
    call s:ClearSigns(old_ikeys)
endf


function! s:ClearSigns(ikeys) "{{{3
    for ikey in a:ikeys
        let def = g:quickfixsigns_register[ikey]
        let bufnr = def.bufnr
        if bufnr(bufnr) != -1
            " TLogVAR ikey
            exec 'sign unplace '. def.id .' buffer='. bufnr
        endif
        call remove(g:quickfixsigns_register, ikey)
    endfor
endf


function! s:PruneRegister() "{{{3
    for [ikey, item] in items(g:quickfixsigns_register)
        if bufnr(item.bufnr) == -1
            call remove(g:quickfixsigns_register, ikey)
        endif
    endfor
endf


function! s:Scope(class, item) "{{{3
    if has_key(a:item, 'scope')
        let rv = a:item.scope
    else
        let rv = get(g:quickfixsigns_class_{a:class}, 'scope', 'buffer')
    endif
    " TLogVAR rv, a:class, a:item
    return rv
endf


function! s:SetItemId(item) "{{{3
    " TLogVAR a:item
    let bufnr = get(a:item, 'bufnr', -1)
    if bufnr == -1
        return  {}
    else
        let scope = s:Scope(a:item.class, a:item)
        let sign = s:GetSign(g:quickfixsigns_class_{a:item.class}.sign, a:item)
        if has_key(a:item, 'ikey') && !empty(a:ikey.ikey)
            let ikey = a:item.ikey
        else
            let ikey = printf("c:%s\ts:%s\tb:%d\tl:%d", a:item.class, sign, bufnr, a:item.lnum)
        endif
        if has_key(g:quickfixsigns_register, ikey) && !get(g:quickfixsigns_class_{a:item.class}, 'always_new', 0)
            let item = extend(copy(g:quickfixsigns_register[ikey]), a:item)
            let item.new = 0
        else
            let item = a:item
            let item.new = 1
        endif
        if !has_key(item, 'id') " || item.id == 0
            let item.id = s:quickfixsigns_base
            let s:quickfixsigns_base += 1
        endif
        let item.ikey = ikey
        let g:quickfixsigns_register[ikey] = item
        return item
    endif
endf


function! s:GetSign(sign, item) "{{{3
    if a:sign[0] == '*'
        let sign = call(a:sign[1 : -1], [a:item])
        " TLogVAR sign
    else
        let sign = a:sign
    endif
    return sign
endf


" Add signs for all locations in LIST. LIST must confirm with the 
" quickfix list format (see |getqflist()|; only the fields lnum and 
" bufnr are required).
"
" list:: a quickfix or location list
" sign:: a sign defined with |:sign-define|
function! s:PlaceSign(class, sign, list) "{{{3
    " TAssertType a:sign, 'string'
    " TAssertType a:list, 'list'
    " TLogVAR a:sign, a:list
    let new_ikeys = []
    for item in a:list
        let sign = s:GetSign(a:sign, item)
        let item = extend(item, {'class': a:class, 'sign': a:sign}, 'keep')
        " TLogVAR item
        let item = s:SetItemId(item)
        if !empty(item)
            let ikey = item.ikey
            " TLogVAR ikey, item
            call add(new_ikeys, ikey)
            if item.new
                let lnum = get(item, 'lnum', 0)
                if lnum > 0
                    let id = item.id
                    " TLogVAR item
                    " TLogDBG ':sign place '. id .' line='. lnum .' name='. sign .' buffer='. item.bufnr
                    exec ':sign place '. id .' line='. lnum .' name='. sign .' buffer='. item.bufnr
                endif
            endif
        endif
    endfor
    return new_ikeys
endf


function! s:GetQFList(bufname) "{{{3
    return getqflist()
endf


function! s:GetLocList(bufname) "{{{3
    return getloclist(0)
endf


runtime! autoload/quickfixsigns/*.vim
call QuickfixsignsSelect(g:quickfixsigns_classes)
unlet s:signss


augroup QuickFixSigns
    autocmd!
    let s:ev_set = []
    for [s:key, s:def] in s:ListValues()
        for s:ev in get(s:def, 'event', ['BufEnter'])
            if index(s:ev_set, s:ev) == -1
                exec 'autocmd '. s:ev .' * call QuickfixsignsSet("'. s:ev .'", [], expand("<afile>"))'
                call add(s:ev_set, s:ev)
            endif
        endfor
    endfor
    unlet s:ev_set
    if exists('s:key')
        unlet s:ev s:key s:def
    endif
    " autocmd BufRead,BufNewFile * exec 'sign place '. (s:quickfixsigns_base - 1) .' name=QFS_DUMMY line=1 buffer='. bufnr('%')
    autocmd User WokmarksChange if index(g:quickfixsigns_classes, 'marks') != -1 | call QuickfixsignsUpdate("marks") | endif
augroup END


let &cpo = s:save_cpo
unlet s:save_cpo
autoload/quickfixsigns.vim	[[[1
66
" quickfixsigns.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-03-19.
" @Last Change: 2011-04-20.
" @Revision:    0.0.48


if !exists('g:quickfixsigns#use_relativenumber')
    " VIM 7.3 and later: If non-zero, |quickfixsigns#RelNumbersOnce()| 
    " uses 'relativenumber' instead of signs. This avoids clashes with 
    " other signs and is faster, but it could cause the visual text area 
    " to be moved to the right.
    let g:quickfixsigns#use_relativenumber = 1   "{{{2
endif


function! quickfixsigns#CompleteSelect(ArgLead, CmdLine, CursorPos) "{{{3
    " TLogVAR a:ArgLead, a:CmdLine, a:CursorPos
    let start = len('quickfixsigns_class_')
    let vars = filter(keys(g:), 'v:val =~ ''^quickfixsigns_class_''. a:ArgLead')
    call map(vars, 'strpart(v:val, start)')
    let selected = split(a:CmdLine, '\s\+')
    call filter(vars, 'index(selected, v:val) == -1')
    if a:CmdLine =~ '\<QuickfixsignsSelect\s\+$'
        call insert(vars, join(g:quickfixsigns_classes))
    endif
    return vars
endf


" Display relative line numbers. Remove the signs when the cursor moves.
" From vim 7.3 on, this uses the 'relativenumber' option.
function! quickfixsigns#RelNumbersOnce() "{{{3
    if !has_key(g:quickfixsigns_lists, 'rel2')
        if v:version >= 703 && g:quickfixsigns#use_relativenumber
            if !&relativenumber
                augroup QuickFixSignsRelNumbersOnce
                    autocmd!
                    autocmd CursorMoved,CursorMovedI,TabLeave,WinLeave * setlocal norelativenumber
                    if ! &cul
                        autocmd CursorMoved,CursorMovedI,TabLeave,WinLeave * setlocal nocul
                    endif
                    if ! &cuc
                        autocmd CursorMoved,CursorMovedI,TabLeave,WinLeave * setlocal nocuc
                    endif
                    autocmd CursorMoved,CursorMovedI,TabLeave,WinLeave * autocmd! QuickFixSignsRelNumbersOnce
                augroup END
                setlocal relativenumber cul cuc
            endif
        else
            let s:list = keys(g:quickfixsigns_lists)
            call QuickfixsignsSelect(s:list + ['rel2'])
            call QuickfixsignsUpdate("rel2")
            augroup QuickFixSignsRelNumbersOnce
                autocmd!
                autocmd CursorMoved,CursorMovedI,TabLeave,WinLeave * call QuickfixsignsSelect(s:list) | call QuickfixsignsClear('rel2') | autocmd! QuickFixSignsRelNumbersOnce
            augroup END
        endif
    endif
endf


" redraw

autoload/quickfixsigns/breakpoints.vim	[[[1
80
" breakpoints.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-11-26.
" @Last Change: 2011-01-27.
" @Revision:    27


if exists('g:quickfixsigns#breakpoints#loaded')
    finish
endif
let g:quickfixsigns#breakpoints#loaded = 1


if index(g:quickfixsigns_classes, 'breakpoints') == -1
    finish
endif


if !exists('g:quickfixsigns#breakpoints#filetypes')
    " :read: let g:quickfixsigns#breakpoints#filetypes = {...}   "{{{2
    let g:quickfixsigns#breakpoints#filetypes = {
                \ 'vim': 'quickfixsigns#breakpoints#Vim'
                \ }
endif


if !exists('g:quickfixsigns_class_breakpoints')
    " :read: let g:quickfixsigns_class_breakpoints = {...}   "{{{2
    let g:quickfixsigns_class_breakpoints = {
                \ 'sign': 'QFS_BREAKPOINT',
                \ 'get': 'quickfixsigns#breakpoints#GetList(%s)',
                \ 'event': g:quickfixsigns_events,
                \ 'test': 'has_key(g:quickfixsigns#breakpoints#filetypes, &ft)',
                \ 'timeout': 5
                \ }
                " \ 'event': ['BufEnter,BufWritePost']
endif


if g:quickfixsigns_class_breakpoints.sign == 'QFS_BREAKPOINT'
    if exists('g:quickfixsigns_icons.breakpoint')
        exec 'sign define QFS_BREAKPOINT text=# texthl=Special icon='. escape(g:quickfixsigns_icons.breakpoint, ' \')
    else
        sign define QFS_BREAKPOINT text=# texthl=Special
    endif
endif


function! quickfixsigns#breakpoints#GetList(filename) "{{{3
    " TLogVAR &filetype
    if has_key(g:quickfixsigns#breakpoints#filetypes, &filetype)
        return call(g:quickfixsigns#breakpoints#filetypes[&filetype], [])
    else
        return []
    endif
endf


function! quickfixsigns#breakpoints#Vim() "{{{3
    redir => bps
    silent breaklist
    redir END
    let acc = []
    for line in split(bps, '\n')
        let ml = matchlist(line, '^\s*\(\d\+\)\s\+\w\+\s\+\(.\{-}\)\s\+\w\+\s\+\(\d\+\)$')
        " TLogVAR line, ml
        if !empty(ml)
            let bufnr = bufnr(ml[2])
            let item = {
                        \ 'bufnr': bufnr,
                        \ 'lnum': ml[3],
                        \ 'text': 'Breakpoint_'. ml[1]
                        \ }
            call add(acc, item)
        endif
    endfor
    return acc
endf

autoload/quickfixsigns/vcsdiff.vim	[[[1
184
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @vcs:         http://vcshub.com/tomtom/quickfixsigns_vim/
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-05-08.
" @Last Change: 2011-05-18.
" @Revision:    224

if exists('g:quickfixsigns#vcsdiff#loaded')
    finish
endif
let g:quickfixsigns#vcsdiff#loaded = 1


if index(g:quickfixsigns_classes, 'vcsdiff') == -1
    finish
endif


if !exists('g:quickfixsigns_class_vcsdiff')
    let g:quickfixsigns_class_vcsdiff = {'sign': '*quickfixsigns#vcsdiff#Signs', 'get': 'quickfixsigns#vcsdiff#GetList(%s)', 'event': ['BufEnter,BufWritePost'], 'always_new': 1}   "{{{2
endif


" A dictionary of supported VCS names and command templates that 
" generate a unified diff file. "%s" is replaced with the filename.
" Supported vcs: git, hg, svn
" :read: let g:quickfixsigns#vcsdiff#cmds = {...} {{{2
let g:quickfixsigns#vcsdiff#cmds = {
            \ 'git': 'git diff -U0 %s',
            \ 'hg': 'hg diff -U0 %s',
            \ 'svn': 'svn diff -x -u %s',
            \ }


if !exists('g:quickfixsigns#vcsdiff#highlight')
    " The highlighting of deleted lines can sometimes be confusing. In 
    " order to disable the display of signs for DEL changes, save the 
    " following line as after/autoload/quickfixsigns/vcsdiff.vim: >
    "
    "   call remove(g:quickfixsigns#vcsdiff#highlight, 'DEL')
    let g:quickfixsigns#vcsdiff#highlight = {'DEL': 'DiffDelete', 'ADD': 'DiffAdd', 'CHANGE': 'DiffChange'}   "{{{2
endif


exec 'sign define QFS_VCS_ADD text=+ texthl='. g:quickfixsigns#vcsdiff#highlight.ADD
exec 'sign define QFS_VCS_DEL text=- texthl='. g:quickfixsigns#vcsdiff#highlight.DEL
exec 'sign define QFS_VCS_CHANGE text== texthl='. g:quickfixsigns#vcsdiff#highlight.CHANGE


" :nodoc:
function! quickfixsigns#vcsdiff#Signs(item) "{{{3
    return 'QFS_VCS_'. a:item.change
endf


" Return the name of a VCS system based on the values of the following 
" variables:
"   - b:vcs_type
"   - b:VCSCommandVCSType
function! quickfixsigns#vcsdiff#GuessType() "{{{3
    if exists('b:vcs_type')
        let type = b:vcs_type
    elseif exists('b:VCSCommandVCSType')
        " vcscommand
        let type = tolower(b:VCSCommandVCSType)
    elseif exists('b:git_dir')
        " fugitive
        let type = 'git'
    else
        let type = ''
    endif
    if has_key(g:quickfixsigns#vcsdiff#cmds, type)
        return type
    else
        return ''
    endif
endf


" quickfixsigns#vcsdiff#GuessType() must return the name of a supported 
" VCS (see |g:quickfixsigns#vcsdiff#cmds|).
function! quickfixsigns#vcsdiff#GetList(filename) "{{{3
    if &buftype =~ '\<\(nofile\|quickfix\|help\)\>' || &previewwindow || exists('b:fugitive_type')
        return []
    endif
    let vcs_type = quickfixsigns#vcsdiff#GuessType()
    " TLogVAR a:filename, vcs_type
    " Ignore files that are not readable
    if has_key(g:quickfixsigns#vcsdiff#cmds, vcs_type) && filereadable(a:filename)
        let cmdt = g:quickfixsigns#vcsdiff#cmds[vcs_type]
        let dir  = fnamemodify(a:filename, ':h')
        let file = fnamemodify(a:filename, ':t')
        let cmds = printf(cmdt, shellescape(file))
        " TLogVAR cmds
        let oldCwd = getcwd()
        let cdcommand = 'cd'
        if exists("*haslocaldir") && haslocaldir()
          let cdcommand = 'lcd'
        endif
        exec cdcommand fnameescape(dir)
        try
            let diff = system(cmds)
        finally
            exec cdcommand fnameescape(oldCwd)
        endtry
        " TLogVAR diff
        if !empty(diff)
            let lines = split(diff, '\n')
            let change_defs = {}
            let from = 0
            let to = 0
            for line in lines
                " TLogVAR from, line
                if line =~ '^@@\s'
                    let m = matchlist(line, '^@@ -\(\d\+\)\(,\d\+\)\? +\(\d\+\)\(,\d\+\)\? @@')
                    " TLogVAR line, m
                    let to = m[3]
                    " let change_lnum = m[1]
                    let from = to
                elseif line =~ '^@@@\s'
                    let m = matchlist(line, '^@@@ -\(\d\+\)\(,\d\+\)\? -\(\d\+\)\(,\d\+\)\? +\(\d\+\)\(,\d\+\)\? @@@')
                    " TLogVAR line, m
                    let to = m[5]
                    " let change_lnum = m[1]
                    let from = to
                elseif from == 0
                    continue
                else
                    if line[0] == '-'
                        let change = 'DEL'
                        let text = line
                        let change_lnum = from
                        let from += 1
                    elseif line[0] == '+'
                        let change = 'ADD'
                        let text = line
                        let change_lnum = to
                        let to += 1
                    else
                        let from += 1
                        let to += 1
                        let change = ''
                        continue
                    endif
                    " TLogVAR change_lnum, change
                    if !empty(change) && has_key(change_defs, change_lnum)
                        if change_defs[change_lnum].change == 'CHANGE' || change_defs[change_lnum].change != change
                            let change = 'CHANGE'
                        endif
                        if has('balloon_multiline')
                            let text = change_defs[change_lnum].text ."\n". line
                        else
                            let text = line
                        endif
                    endif
                    let change_defs[change_lnum] = {'change': change, 'text': text}
                endif
            endfor
            let bufnr = bufnr('%')
            let signs = []
            for [lnum, change_def] in items(change_defs)
                if !has_key(g:quickfixsigns#vcsdiff#highlight, change_def.change)
                    continue
                endif
                if change_def.change == 'DEL' && lnum < line('$') && !has_key(change_defs, lnum + 1)
                    let lnum += 1
                endif
                if has('balloon_multiline')
                    let text = change_def.change .":\n". change_def.text
                else
                    let text = change_def.change .": ". change_def.text
                endif
                call add(signs, {"bufnr": bufnr, "lnum": lnum,
                            \ "change": change_def.change, "text": text})
            endfor
            " TLogVAR signs
            return signs
        endif
    endif
    return []
endf


autoload/quickfixsigns/marks.vim	[[[1
87
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @GIT:         http://github.com/tomtom/quickfixsigns_vim/
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-05-08.
" @Last Change: 2011-01-27.
" @Revision:    28

if index(g:quickfixsigns_classes, 'marks') == -1
    finish
endif


if !exists('g:quickfixsigns_class_marks')
    " The definition of signs for marks.
    " :read: let g:quickfixsigns_class_marks = {...} "{{{2
    let g:quickfixsigns_class_marks = {
                \ 'sign': '*quickfixsigns#marks#GetSign',
                \ 'get': 'quickfixsigns#marks#GetList(%s)',
                \ 'event': g:quickfixsigns_events,
                \ 'timeout': 2
                \ }
endif
if !&lazyredraw && !empty(g:quickfixsigns_class_marks)
    let s:cmn = index(g:quickfixsigns_class_marks.event, 'CursorMoved')
    let s:cmi = index(g:quickfixsigns_class_marks.event, 'CursorMovedI')
    if s:cmn >= 0 || s:cmi >= 0
        echohl Error
        echom "quickfixsigns: Support for CursorMoved(I) events requires 'lazyredraw' to be set"
        echohl NONE
        if s:cmn >= 0
            call remove(g:quickfixsigns_class_marks.event, s:cmn)
        endif
        if s:cmi >= 0
            call remove(g:quickfixsigns_class_marks.event, s:cmi)
        endif
    endif
    unlet s:cmn s:cmi
endif


if !exists('g:quickfixsigns#marks#marks')
    " A list of marks that should be displayed as signs. If empty, 
    " disable the display of marks.
    let g:quickfixsigns#marks#marks = split('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ<>''^.', '\zs') "{{{2
endif


for s:i in g:quickfixsigns#marks#marks
	if index(g:quickfixsigns_signs, 'QFS_Mark_'. s:i) == -1
		exec 'sign define QFS_Mark_'. s:i .' text='. s:i .' texthl=Identifier'
	endif
endfor
unlet s:i

function! quickfixsigns#marks#GetList(filename) "{{{3
    let acc = []
    let bufnr  = bufnr(a:filename)
    let ignore = exists('b:quickfixsigns_ignore_marks') ? b:quickfixsigns_ignore_marks : []
    for mark in g:quickfixsigns#marks#marks
        let pos = getpos("'". mark)
        if mark =~# '^\u'
            let scope = 'vim'
        else
            let scope = 'buffer'
        endif
        if pos[1] != 0 && index(ignore, mark) == -1 && (pos[0] == (scope == 'vim' ? bufnr : 0))
            let item = {
                        \ 'bufnr': pos[0] == 0 ? bufnr : pos[0],
                        \ 'lnum': pos[1],
                        \ 'col': pos[2],
                        \ 'text': 'Mark_'. mark,
                        \ 'scope': scope
                        \ }
            " TLogVAR mark, item.scope
            call add(acc, item)
        endif
    endfor
    return acc
endf


function! quickfixsigns#marks#GetSign(item) "{{{3
    return 'QFS_'. a:item.text
endf


