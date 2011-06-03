" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/quickfixsigns.txt	[[[1
225
*quickfixsigns.txt*   Mark quickfix & location list items with signs
                      Author: Thomas Link, micathom at gmail com

Mark items in the quickfix and location lists with signs. Other lists 
can be configured via the |g:quickfixsigns_lists| variable. By default, 
quickfixsigns also displays signs for the marks a-zA-Z.

The attached text ist displayed in a balloon -- it is possible though 
that other plugins that set 'balloonexpr' disable this feature.

If you want nicer looking images instead of the ASCII markers, you have 
to change the following signs' definition in your |vimrc| file:

    QFS_QFL ... Entries in the |quickfix| list
    QFS_LOC ... Entries in the |location-list|
    QFS_Mark_[a-zA-Z] ... Marks


-----------------------------------------------------------------------
Install~

Edit the vba file and type: >

    :so %

See :help vimball for details. If you have difficulties or use vim 7.0, 
please make sure, you have the current version of vimball
(vimscript #1502) installed or update your runtime.


========================================================================
Contents~

        :QuickfixsignsSet .................... |:QuickfixsignsSet|
        :QuickfixsignsSelect ................. |:QuickfixsignsSelect|
        g:quickfixsigns_classes .............. |g:quickfixsigns_classes|
        g:quickfixsigns_events ............... |g:quickfixsigns_events|
        g:quickfixsigns_class_rel ............ |g:quickfixsigns_class_rel|
        g:quickfixsigns_class_qfl ............ |g:quickfixsigns_class_qfl|
        g:quickfixsigns_class_loc ............ |g:quickfixsigns_class_loc|
        g:quickfixsigns_class_cursor ......... |g:quickfixsigns_class_cursor|
        g:quickfixsigns_balloon .............. |g:quickfixsigns_balloon|
        g:quickfixsigns_max .................. |g:quickfixsigns_max|
        g:quickfixsigns_blacklist_buffer ..... |g:quickfixsigns_blacklist_buffer|
        QuickfixsignsSelect .................. |QuickfixsignsSelect()|
        QuickfixsignsUpdate .................. |QuickfixsignsUpdate()|
        QuickfixsignsSet ..................... |QuickfixsignsSet()|
        QuickfixsignsBalloon ................. |QuickfixsignsBalloon()|
        QuickfixsignsClear ................... |QuickfixsignsClear()|
        g:quickfixsigns#use_relativenumber ... |g:quickfixsigns#use_relativenumber|
        quickfixsigns#CompleteSelect ......... |quickfixsigns#CompleteSelect()|
        quickfixsigns#RelNumbersOnce ......... |quickfixsigns#RelNumbersOnce()|
        g:quickfixsigns_class_vcsdiff ........ |g:quickfixsigns_class_vcsdiff|
        g:quickfixsigns#vcsdiff#highlight .... |g:quickfixsigns#vcsdiff#highlight|
        quickfixsigns#vcsdiff#GuessType ...... |quickfixsigns#vcsdiff#GuessType()|
        quickfixsigns#vcsdiff#GetList ........ |quickfixsigns#vcsdiff#GetList()|
        g:quickfixsigns_class_marks .......... |g:quickfixsigns_class_marks|
        g:quickfixsigns#marks#marks .......... |g:quickfixsigns#marks#marks|
        quickfixsigns#marks#GetList .......... |quickfixsigns#marks#GetList()|
        quickfixsigns#marks#GetSign .......... |quickfixsigns#marks#GetSign()|
        quickfixsigns#marks#GetID ............ |quickfixsigns#marks#GetID()|


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
g:quickfixsigns_classes        (default: ['cursor', 'qfl', 'loc', 'marks', 'vcsdiff'])
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

                                                    *g:quickfixsigns_events*
g:quickfixsigns_events         (default: ['BufEnter', 'CursorHold', 'CursorHoldI', 'InsertLeave', 'InsertEnter', 'InsertChange'])
    List of events for signs that should be frequently updated.

                                                    *g:quickfixsigns_class_rel*
g:quickfixsigns_class_rel      (default: {'sign': '*s:RelSign', 'get': 's:GetRelList("rel")', 'event': g:quickfixsigns_events, 'max': 9, 'level': 9})
    Signs for number of lines relative to the current line.

                                                    *g:quickfixsigns_class_qfl*
g:quickfixsigns_class_qfl      (default: {'sign': 'QFS_QFL', 'get': 'getqflist()', 'event': ['BufEnter']})
    Signs for |quickfix| lists.

                                                    *g:quickfixsigns_class_loc*
g:quickfixsigns_class_loc      (default: {'sign': 'QFS_LOC', 'get': 'getloclist(0)', 'event': ['BufEnter']})
    Signs for |location| lists.

                                                    *g:quickfixsigns_class_cursor*
g:quickfixsigns_class_cursor   (default: {'sign': 'QFS_CURSOR', 'get': 's:GetCursor()', 'event': g:quickfixsigns_events})
    Sign for the current cursor position

                                                    *g:quickfixsigns_balloon*
g:quickfixsigns_balloon        (default: 1)
    If non-null, display a balloon when hovering with the mouse over 
    the sign.
    buffer-local or global

                                                    *g:quickfixsigns_max*
g:quickfixsigns_max            (default: 100)
    Don't display signs if the list is longer than n items.

                                                    *g:quickfixsigns_blacklist_buffer*
g:quickfixsigns_blacklist_buffer (default: '^__.*__$')
    Don't show signs in buffers matching this |regexp|.

                                                    *QuickfixsignsSelect()*
QuickfixsignsSelect(list)

                                                    *QuickfixsignsUpdate()*
QuickfixsignsUpdate(?class="")

                                                    *QuickfixsignsSet()*
QuickfixsignsSet(event)
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


========================================================================
autoload/quickfixsigns/vcsdiff.vim~

                                                    *g:quickfixsigns_class_vcsdiff*
g:quickfixsigns_class_vcsdiff  (default: {'sign': '*quickfixsigns#vcsdiff#Signs', 'get': 'quickfixsigns#vcsdiff#GetList()', 'event': ['BufEnter,BufWritePost']})

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
quickfixsigns#vcsdiff#GetList()
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
quickfixsigns#marks#GetList()

                                                    *quickfixsigns#marks#GetSign()*
quickfixsigns#marks#GetSign(item)

                                                    *quickfixsigns#marks#GetID()*
quickfixsigns#marks#GetID(item)



vim:tw=78:fo=tcq2:isk=!-~,^*,^|,^":ts=8:ft=help:norl:
plugin/quickfixsigns.vim	[[[1
459
" Mark quickfix & location list items with signs
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @GIT:         http://github.com/tomtom/vimtlib/
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-03-14.
" @Last Change: 2010-08-30.
" @Revision:    490
" GetLatestVimScripts: 2584 1 :AutoInstall: quickfixsigns.vim

if &cp || exists("loaded_quickfixsigns") || !has('signs')
    finish
endif
let loaded_quickfixsigns = 7

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
    let g:quickfixsigns_classes = ['cursor', 'qfl', 'loc', 'marks', 'vcsdiff']   "{{{2
    " let g:quickfixsigns_classes = ['rel', 'qfl', 'loc', 'marks']   "{{{2
endif


if !exists('g:quickfixsigns_events')
    " List of events for signs that should be frequently updated.
    let g:quickfixsigns_events = ['BufEnter', 'CursorHold', 'CursorHoldI', 'InsertLeave', 'InsertEnter', 'InsertChange']   "{{{2
endif


if !exists('g:quickfixsigns_class_rel')
    " Signs for number of lines relative to the current line.
    let g:quickfixsigns_class_rel = {'sign': '*s:RelSign', 'get': 's:GetRelList("rel")', 'event': g:quickfixsigns_events, 'max': 9, 'level': 9}  "{{{2
endif
let g:quickfixsigns_class_rel2 = copy(g:quickfixsigns_class_rel)
let g:quickfixsigns_class_rel2.get = 's:GetRelList("rel2")'
let g:quickfixsigns_class_rel2.max = 99


if !exists('g:quickfixsigns_class_qfl')
    " Signs for |quickfix| lists.
    let g:quickfixsigns_class_qfl = {'sign': 'QFS_QFL', 'get': 'getqflist()', 'event': ['BufEnter']}   "{{{2
endif


if !exists('g:quickfixsigns_class_loc')
    " Signs for |location| lists.
    let g:quickfixsigns_class_loc = {'sign': 'QFS_LOC', 'get': 'getloclist(0)', 'event': ['BufEnter']}   "{{{2
endif


if !exists('g:quickfixsigns_class_cursor')
    " Sign for the current cursor position
    let g:quickfixsigns_class_cursor = {'sign': 'QFS_CURSOR', 'get': 's:GetCursor()', 'event': g:quickfixsigns_events}   "{{{2
endif


if !exists('g:quickfixsigns_balloon')
    " If non-null, display a balloon when hovering with the mouse over 
    " the sign.
    " buffer-local or global
    let g:quickfixsigns_balloon = 1   "{{{2
endif


if !exists('g:quickfixsigns_max')
    " Don't display signs if the list is longer than n items.
    let g:quickfixsigns_max = 100   "{{{2
endif


if !exists('g:quickfixsigns_blacklist_buffer')
    " Don't show signs in buffers matching this |regexp|.
    let g:quickfixsigns_blacklist_buffer = '^__.*__$'   "{{{2
endif



" ----------------------------------------------------------------------
let g:quickfixsigns_base = 5272
let g:quickfixsigns_register = {}
let s:cursor_last_line = 0
let s:last_run = {}


redir => s:signss
silent sign list
redir END
let g:quickfixsigns_signs = split(s:signss, '\n')
call filter(g:quickfixsigns_signs, 'v:val =~ ''^sign QFS_''')
call map(g:quickfixsigns_signs, 'matchstr(v:val, ''^sign \zsQFS_\w\+'')')

if index(g:quickfixsigns_signs, 'QFS_QFL') == -1
    sign define QFS_QFL text=* texthl=WarningMsg
endif

if index(g:quickfixsigns_signs, 'QFS_LOC') == -1
    sign define QFS_LOC text=> texthl=Special
endif

if index(g:quickfixsigns_signs, 'QFS_CURSOR') == -1
    sign define QFS_CURSOR text=- texthl=Question
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
		let g:quickfixsigns_lists[what] = g:quickfixsigns_class_{what}
	endfor
endf


" :display: QuickfixsignsUpdate(?class="")
function! QuickfixsignsUpdate(...) "{{{3
    let what = a:0 >= 1 ? a:1 : ""
    call QuickfixsignsClear(what)
    call QuickfixsignsSet("")
endf


" (Re-)Set the signs that should be updated at a certain event. If event 
" is empty, update all signs.
"
" Normally, the end-user doesn't need to call this function.
function! QuickfixsignsSet(event) "{{{3
    if exists("b:noquickfixsigns") && b:noquickfixsigns
        return
    endif
    if bufname('%') =~ g:quickfixsigns_blacklist_buffer
        return
    endif
    " let lz = &lazyredraw
    " set lz
    " try
        let bn = bufnr('%')
        let anyway = empty(a:event)
        for [key, def] in s:ListValues()
            if anyway || index(get(def, 'event', ['BufEnter']), a:event) != -1
                let t_d = get(def, 'timeout', 0)
                let t_l = localtime()
                let t_s = string(def)
                " TLogVAR t_s, t_d, t_l
                if anyway || (t_d == 0) || (t_l - get(s:last_run, t_s, 0) >= t_d)
                    let s:last_run[t_s] = t_l
                    let list = eval(def.get)
                    " TLogVAR list
                    call filter(list, 'v:val.bufnr == bn')
                    " TLogVAR list
                    if !empty(list) && len(list) < g:quickfixsigns_max
                        let get_id = get(def, 'id', 's:SignId')
                        call s:ClearBuffer(key, def.sign, bn, s:PlaceSign(key, def.sign, list, get_id))
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
                        call s:ClearBuffer(key, def.sign, bn, [])
                    endif
                endif
            endif
        endfor
    " finally
    "     if &lz != lz
    "         let &lz = lz
    "     endif
    " endtry
endf


function! QuickfixsignsBalloon() "{{{3
    " TLogVAR v:beval_lnum, v:beval_col
    if v:beval_col <= 1
        let lnum = v:beval_lnum
        let bn = bufnr('%')
        let acc = []
        for [key, def] in s:ListValues()
            let list = eval(def.get)
            call filter(list, 'v:val.bufnr == bn && v:val.lnum == lnum')
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


function! s:GetCursor() "{{{3
    let pos = getpos('.')
    let s:cursor_last_line = pos[1]
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


function! s:GetRelList(class) "{{{3
	let lnum = line('.')
	let col = col('.')
	let bn = bufnr('%')
    let top = line('w0') - lnum
    let bot = line('w$') - lnum
    let max = g:quickfixsigns_class_{a:class}.max
    if max >= 0
        let top = max([top, -max])
        let bot = min([bot, max])
    endif
    " TLogVAR top, bot
    call s:GenRel(max([abs(top), abs(bot)]))
    return map(range(top, bot), '{"bufnr": bn, "lnum": lnum + v:val, "col": col, "text": "REL_". abs(v:val)}')
endf


" Clear all signs with name SIGN.
function! QuickfixsignsClear(class) "{{{3
    " TLogVAR a:sign_rx
    let idxs = keys(g:quickfixsigns_register)
    if !empty(a:class)
        let idxs = filter(idxs, 'g:quickfixsigns_register[v:val].class ==# a:class')
    endif
    " TLogVAR idxs
    for idx in idxs
        let bn = g:quickfixsigns_register[idx].bn
        if bufnr(bn) != -1
            exec 'sign unplace '. idx .' buffer='. bn
        endif
        call remove(g:quickfixsigns_register, idx)
    endfor
endf


" Clear all signs with name SIGN in buffer BUFNR.
function! s:ClearBuffer(class, sign, bufnr, new_idxs) "{{{3
    " TLogVAR a:class, a:sign, a:bufnr, a:new_idxs
    let old_idxs = filter(keys(g:quickfixsigns_register), 'g:quickfixsigns_register[v:val].class ==# a:class && g:quickfixsigns_register[v:val].bn == a:bufnr && index(a:new_idxs, v:val) == -1')
    " TLogVAR old_idxs
    for idx in old_idxs
        exec 'sign unplace '. idx .' buffer='. g:quickfixsigns_register[idx].bn
        call remove(g:quickfixsigns_register, idx)
    endfor
endf


function! s:ClearDummy(idx, bufnr) "{{{3
    exec 'sign unplace '. a:idx .' buffer='. a:bufnr
endf


function! s:SignId(item) "{{{3
    " TLogVAR a:item
    let bn = get(a:item, 'bufnr', -1)
    if bn == -1
        return -1
    else
        let idx = g:quickfixsigns_base + bn * 427 + 1
        while has_key(g:quickfixsigns_register, idx)
            let idx += 1
        endwh
        return idx
    endif
endf


" Add signs for all locations in LIST. LIST must confirm with the 
" quickfix list format (see |getqflist()|; only the fields lnum and 
" bufnr are required).
"
" list:: a quickfix or location list
" sign:: a sign defined with |:sign-define|
function! s:PlaceSign(class, sign, list, ...) "{{{3
    " TAssertType a:sign, 'string'
    " TAssertType a:list, 'list'
    " TLogVAR a:sign, a:list
    let get_id = a:0 >= 1 ? a:1 : "<SID>SignId"
    " TLogVAR get_id
    let new_idxs = []
    for item in a:list
        " TLogVAR item
        if a:sign[0] == '*'
            let sign = call(a:sign[1 : -1], [item])
            " TLogVAR sign
        else
            let sign = a:sign
        endif
        let idx = call(get_id, [item])
        " TLogVAR idx, sign
        if idx > 0
            let bn   = get(item, 'bufnr')
            let sdef = {'class': a:class, 'sign': a:sign, 'bn': bn, 'item': item, 'idx': idx}
            call add(new_idxs, string(idx))
            if has_key(g:quickfixsigns_register, idx)
                if g:quickfixsigns_register[idx] == sdef
                    continue
                else
                    " TLogVAR item
                    " TLogDBG ':sign unplace '. idx .' buffer='. bn
                    exec ':sign unplace '. idx .' buffer='. bn
                    unlet g:quickfixsigns_register[idx]
                endif
            endif
            let lnum = get(item, 'lnum', 0)
            if lnum > 0
                " TLogVAR item
                " TLogDBG ':sign place '. idx .' line='. lnum .' name='. sign .' buffer='. bn
                exec ':sign place '. idx .' line='. lnum .' name='. sign .' buffer='. bn
                let g:quickfixsigns_register[idx] = sdef
            endif
        endif
    endfor
    return new_idxs
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
                exec 'autocmd '. s:ev .' * call QuickfixsignsSet("'. s:ev .'")'
                call add(s:ev_set, s:ev)
            endif
        endfor
    endfor
    unlet s:ev_set
    if exists('s:key')
        unlet s:ev s:key s:def
    endif
    " autocmd BufRead,BufNewFile * exec 'sign place '. (g:quickfixsigns_base - 1) .' name=QFS_DUMMY line=1 buffer='. bufnr('%')
    autocmd User WokmarksChange if index(g:quickfixsigns_classes, 'marks') != -1 | call QuickfixsignsUpdate("marks") | endif
augroup END


let &cpo = s:save_cpo
unlet s:save_cpo
finish

CHANGES:
0.1
- Initial release

0.2
- exists('b:quickfixsigns_balloonexpr')

0.3
- Old signs weren't always removed
- Avoid "flicker" etc.
- g:quickfixsigns_max: Don't display signs if the list is longer than n items.
Incompatible changes:
- Removed g:quickfixsigns_show_marks variable
- g:quickfixsigns_marks: Marks that should be used for signs
- g:quickfixsigns_lists: event field is a list
- g:quickfixsigns_lists: timeout field: don't re-display this list more often than n seconds

0.4
- FIX: Error when g:quickfixsigns_marks = [] (thanks Ingo Karkat)
- s:ClearBuffer: removed old code
- QuickfixsignsMarks(state): Switch the display of marks on/off.

0.5
- Set balloonexpr only if empty (don't try to be smart)
- Disable CursorMoved(I) events, when &lazyredraw isn't set.

0.6
- Don't require qfl.item.text to be set

0.7
- b:noquickfixsigns: If true, disable quickfixsigns for the current 
buffer (patch by Sergey Khorev; must be set before entering a buffer)
- b:quickfixsigns_ignore_marks: A list of ignored marks (per buffer)

0.8
- Support for relative line numbers
- QuickfixsignsSet command
- quickfixsigns#RelNumbersOnce()

0.9
- Support for vcs diff (this requires either b:vcs_type or 
b:VCSCommandVCSType to be set to a supported vcs, e.g. git)

autoload/quickfixsigns.vim	[[[1
58
" quickfixsigns.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-03-19.
" @Last Change: 2010-10-02.
" @Revision:    0.0.34


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
function! quickfixsigns#RelNumbersOnce() "{{{3
    if !has_key(g:quickfixsigns_lists, 'rel2')
        if v:version >= 703 && g:quickfixsigns#use_relativenumber
            if !&relativenumber
                setlocal relativenumber
                augroup QuickFixSignsRelNumbersOnce
                    autocmd!
                    autocmd CursorMoved,CursorMovedI * set norelativenumber | autocmd! QuickFixSignsRelNumbersOnce
                augroup END
            endif
        else
            let s:list = keys(g:quickfixsigns_lists)
            call QuickfixsignsSelect(s:list + ['rel2'])
            call QuickfixsignsUpdate("rel2")
            augroup QuickFixSignsRelNumbersOnce
                autocmd!
                autocmd CursorMoved,CursorMovedI * call QuickfixsignsSelect(s:list) | call QuickfixsignsClear('rel2') | autocmd! QuickFixSignsRelNumbersOnce
            augroup END
        endif
    endif
endf


" redraw

autoload/quickfixsigns/vcsdiff.vim	[[[1
139
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @vcs:         http://vcshub.com/tomtom/vimtlib/
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-05-08.
" @Last Change: 2010-09-27.
" @Revision:    154

if index(g:quickfixsigns_classes, 'vcsdiff') == -1
    finish
endif


if !exists('g:quickfixsigns_class_vcsdiff')
    let g:quickfixsigns_class_vcsdiff = {'sign': '*quickfixsigns#vcsdiff#Signs', 'get': 'quickfixsigns#vcsdiff#GetList()', 'event': ['BufEnter,BufWritePost']}   "{{{2
endif


" A dictionary of supported VCS names and command templates that 
" generate a unified diff file. "%s" is replaced with the filename.
" Currently only git is supported.
" :read: let g:quickfixsigns#vcsdiff#cmds = {...} {{{2
let g:quickfixsigns#vcsdiff#cmds = {
            \ 'git': 'git diff -U0 %s',
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
        return b:vcs_type
    elseif exists('b:VCSCommandVCSType')
        return b:VCSCommandVCSType
    endif
endf


" quickfixsigns#vcsdiff#GuessType() must return the name of a supported 
" VCS (see |g:quickfixsigns#vcsdiff#cmds|).
function! quickfixsigns#vcsdiff#GetList() "{{{3
    let vcs_type = quickfixsigns#vcsdiff#GuessType()
    if has_key(g:quickfixsigns#vcsdiff#cmds, vcs_type)
        let cmdt = g:quickfixsigns#vcsdiff#cmds[vcs_type]
        let cmds = printf(cmdt, shellescape(expand('%')))
        let diff = system(cmds)
        " TLogVAR diff
        if !empty(diff)
            let lines = split(diff, '\n')
            let change_defs = {}
            let from = 0
            let to = 0
            for line in lines
                " TLogVAR from, line
                if line =~ '^@@'
                    let m = matchlist(line, '^@@ -\(\d\+\)\(,\d\+\)\? +\(\d\+\)\(,\d\+\)\? @@')
                    " TLogVAR line, m
                    let to = m[3]
                    " let from = m[1]
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
                    endif
                    " TLogVAR change_lnum, change
                    if has_key(change_defs, change_lnum)
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
            let bnum = bufnr('%')
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
                call add(signs, {"bufnr": bnum, "lnum": lnum,
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
" @GIT:         http://github.com/tomtom/vimtlib/
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-05-08.
" @Last Change: 2010-08-30.
" @Revision:    11

if index(g:quickfixsigns_classes, 'marks') == -1
    finish
endif


if !exists('g:quickfixsigns_class_marks')
    " The definition of signs for marks.
    " :read: let g:quickfixsigns_class_marks = {...} "{{{2
    let g:quickfixsigns_class_marks = {
                \ 'sign': '*quickfixsigns#marks#GetSign',
                \ 'get': 'quickfixsigns#marks#GetList()',
                \ 'id': 'quickfixsigns#marks#GetID',
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

function! quickfixsigns#marks#GetList() "{{{3
    let acc = []
    let bn  = bufnr('%')
    let ignore = exists('b:quickfixsigns_ignore_marks') ? b:quickfixsigns_ignore_marks : []
    for mark in g:quickfixsigns#marks#marks
        let pos = getpos("'". mark)
        if pos[1] != 0 && index(ignore, mark) == -1 && (mark =~# '[a-z]' || pos[0] == bn)
            call add(acc, {'bufnr': bn, 'lnum': pos[1], 'col': pos[2], 'text': 'Mark_'. mark})
        endif
    endfor
    return acc
endf


function! quickfixsigns#marks#GetSign(item) "{{{3
    return 'QFS_'. a:item.text
endf


function! quickfixsigns#marks#GetID(item) "{{{3
    let bn = bufnr('%')
    let item = filter(values(g:quickfixsigns_register), 'v:val.bn == bn && get(v:val.item, "text", "") ==# get(a:item, "text", "")')
    if empty(item)
        return g:quickfixsigns_base + a:item.bufnr * 67 + char2nr(get(a:item, "text", "")[-1 : -1]) - 65
    else
        " TLogVAR item
        return item[0].idx
    endif
endf


