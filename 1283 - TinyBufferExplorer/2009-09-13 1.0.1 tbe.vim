" Tiny Buffer Explorer
"
" Description:
"
"   A 1-file buffer list plugin with grouping.
"
"   [key features]
"
"       1. incremental collection of buffers
"         - buffer information are collected in the background
"         - less time requred showing lists
"       2. grouping
"         - it suppresses excessive information
"         - it's easy to switch among groups
"         - defined groups:
"           - Directory groups corresponding to buffers
"           - MRU(Most Recently Used buffers) group
"           - Search group
"       3. ease of accessing buffers and groups
"         - the MRU group can list buffers you are likely to edit
"         - the group list supports you switching among groups
"         - it's easy to switch to the Directory group in which
"           the current buffer is
"       4. customizable list format
"
" Maintainer: Shuhei Kubota <chimachima@gmail.com>
"
" Usage:
"
"   [Commands]
"
"      :TinyBufferExplorer
"   
"          shows the TBE window (showing buffer list).
"
"   [Options(Variables)]
"
"      Note that the right-hand value is a default value.
"   
"      g:TBE_showVertically = 0
"   
"          If its value is 0, the window is shown hirizontally. Otherwise (if 0),
"          the window is shown vertically.
"   
"      g:TBE_mruMax = 10
"      
"          The maximum number of buffers which are shown in the MRU group.
"   
"      g:TBE_showMRUFirst = 0
"   
"          This option affects when you open the TBE window.
"   
"          If its value is non-0, TBE shows the MRU group first. Forces the MRU
"          group to be shown.
"   
"          Otherwise TBE shows the group which are shown in the last time.
"   
"      g:TBE_alwaysShowGroupWindow = 0
"   
"          This option affects when you open the TBE window.
"   
"          If its value is non-0, TBE shows the buffer window and the group
"          window at the same time. This make TBE slower (a little).
"   
"      g:TBE_bufferLineFormat = '{number} | {%}{h} {+} {name}   {dir}'
"   
"          A format of a buffer. For more specifications, execute
"          "/!exists('g:TBE_bufferLineFormat')" and read what is written there.
"   
"      g:TBE_groupLineFormat = '{name}  -{count}-    {path}'
"   
"          A format of a group. For more specifications, execute
"          "/!exists('g:TBE_groupLineFormat')" and read what is written there.
"   
"   
"   [Key Mappings]
"
"       ?:
"           toggles showing/hiding help text.
"
"       j, k:
"           moves cursor upward/downward.
"
"       <Enter>, <C-J>:
"           opens the buffer under the cursor.
"
"       d:
"           deletes a buffer. If multiple buffers are selected, they are
"           all deleted.
"
"       f:
"           shows the group window.
"
"       h, l
"           switches group shown in the buffer window.
"
"       n:
"           shows the Directory group in which the current buffer is.
"
"       m:
"           shows the Most Recently Used Buffers group.
"
"       /:
"           shows the Search group (searched by the path of each buffer).
"
"       r:
"           re-collects buffers and refreshes the window.
"
"       v, V, <C-V>:
"           selects buffers(lines). Selected buffers are targets for deletion.
"
"       q, <Esc>: 
"           escapes from the buffer list window.

if !exists('g:TBE_mruMax')
    let g:TBE_mruMax = 10
endif

if !exists('g:TBE_showMRUFirst')
    let g:TBE_showMRUFirst = 0
endif

if !exists('g:TBE_showVertically')
    let g:TBE_showVertically = 0
endif

if !exists('g:TBE_alwaysShowGroupWindow')
    let g:TBE_alwaysShowGroupWindow = 1
endif

if !exists('g:TBE_bufferLineFormat')
    if exists('g:TBE_lineFormat')
        let g:TBE_bufferLineFormat = g:TBE_lineFormat
    else
        let g:TBE_bufferLineFormat = '{%}{h} {+} {name}'
    endif

    "let g:TBE_bufferLineFormat = '{number} | {%}{h} {+} {path}'
    " 
    " format elements (CASE-SENSITIVE):
    "
    "    {number} : buffer number
    "   
    "    {path}   : full path of buffer
    "   
    "    {dir}    : directory in which buffer is
    "   
    "    {name}   : buffer name without directory
    "   
    "    {%}      : '%' for current buffer,
    "               '#' for alternative,
    "               ' ' for others
    "   
    "    {h}      : 'h' for hidden buffer,
    "               'a' for active
    "   
    "    {+}      : '+' for modified,
    "               ' ' for others
endif

if !exists('g:TBE_groupLineFormat')
    let g:TBE_groupLineFormat = '{name}  -{count}-    {path}'
    "    {name}   : buffer name without directory
    "    {path}   : full path of buffer
    "    {count}  : the count of buffers
endif

command!  TinyBufferExplorer  call <SID>TBE_showMainWindow()


" debugging
let s:TBE__makeDump = 0

"
" TBE class
"
let s:TBE__BUFFER_NAME       = 'TinyBufferExplorer'
let s:TBE__GROUP_BUFFER_NAME = 'TinyBufferExplorerGroups'
let s:TBE__DUMP_BUFFER_NAME  = 'TinyBufferExplorerDump'

let s:TBE__FORMAT_NUMBER   = '{number}' " buffer right
let s:TBE__FORMAT_COUNT    = '{count}'  " group right
let s:TBE__FORMAT_NAME     = '{name}'   " buffer group left
let s:TBE__FORMAT_PATH     = '{path}'   " buffer group left
let s:TBE__FORMAT_DIR      = '{dir}'    " buffer left
let s:TBE__FORMAT_CURRENT  = '{%}'      " buffer left
let s:TBE__FORMAT_HIDDEN   = '{h}'      " buffer left
let s:TBE__FORMAT_MODIFIED = '{+}'      " buffer left

let s:TBE__INITIAL_HEADER_LINE_COUNT = 2
let s:TBE__HELP_TEXT = ""
            \ . "= HELP =\n"
            \ . "?              ... toggles showing/hiding this help\n"
            \ . "q, <Esc>       ... hides TBE windows\n"
            \ . "<C-J>, <Enter> ... opens the buffer under the cursor\n"
            \ . "j, k           ... moves the cursor, switches buffers\n"
            \ . "v              ... selects buffers\n"
            \ . "d              ... deletes buffer(or selected buffers)\n"
            \ . "f              ... shows the group window\n"
            \ . "r              ... refreshes TBE windows\n"
            \ . "h, l           ... switches groups in the main(buffer) window\n"
            \ . "n              ... shows the group in which the current buffer is\n"
            \ . "m              ... shows the MRU group\n"
            \ . "/              ... shows the Search group\n"
            \ . "= HELP =\n"
let s:TBE__HELP_LINE_COUNT = 14

function! s:TBE_init()
    let s:TBE__groupCount = 0
    " let s:TBE__order2GroupNum{0 <= i < s:TBE__groupCount} = (0 <= order < s:TBE__groupCount)

    let mru = s:TBE_createMRUGroup()
    call s:TBE_collectBuffers()

    let s:TBE__currentGroupNumOrder = 0

    call s:TBE_hookEvents()

    let s:TBE__showHelp = 0
    let s:TBE_bufferHeaderLineCount = s:TBE__INITIAL_HEADER_LINE_COUNT
    let s:TBE_groupHeaderLineCount  = s:TBE__INITIAL_HEADER_LINE_COUNT
    if s:TBE__showHelp
        let s:TBE_bufferHeaderLineCount = s:TBE_bufferHeaderLineCount + s:TBE__HELP_LINE_COUNT
    endif
endfunction

let s:TBE__dumpCount = 0
function! s:TBE_dump(title)
    if ! s:TBE__makeDump
        return
    endif

    call s:TBE_showWindow(s:TBE__DUMP_BUFFER_NAME, 'new')

    call append(0, a:title)
    call append(1, '')
    call append(2, 'Group(index(groupNum) => group info):')

    let offset = 3
    let i = 0
    while i < s:TBE_getGroupCount()
        call append(offset + i, "\t" . i . "\t=> " . s:TBE_renderGroupLine(i, 0, 16, 32) . "\t" . s:Group_getDeletionTiming(i))
        let i = i + 1
    endwhile

    let offset = offset + i
    call append(offset, '')
    call append(offset + 1, 'Order(index => groupNum):')
    let offset = offset + 2
    let i = 0
    while i < s:TBE_getGroupCount()
        call append(offset + i, "\t" . i . "\t=> " . s:TBE_getOrderOfGroup(i))
        let i = i + 1
    endwhile

    let offset = offset + i


    execute 'write! dump' . s:TBE__dumpCount . '.txt'
    bd
    let s:TBE__dumpCount = s:TBE__dumpCount + 1
endfunction


function! s:TBE_renderCurrentGroup(groupNum)
    setlocal modifiable noreadonly

    " stop the yank hijack
    let old_yank = @"

    %delete

    call setline('.', s:TBE_renderGroupLine(a:groupNum, 0, 0, 0))
    let i = 1
    while i < s:TBE__INITIAL_HEADER_LINE_COUNT
        normal o
        let i = i + 1
    endwhile

    " render buffer line

    let numWid  = 0
    let nameWid = 0
    let pathWid = 0
    let dirWid  = 0

    if stridx(g:TBE_bufferLineFormat, s:TBE__FORMAT_NUMBER) != -1
        let numWid = s:TBE_maxWidthOfBufferProperty(a:groupNum, 'bufnr')
    endif

    if stridx(g:TBE_bufferLineFormat, s:TBE__FORMAT_NAME) != -1
        let nameWid = s:TBE_maxWidthOfBufferProperty(a:groupNum, 's:Buffer_getName')
    endif

    if stridx(g:TBE_bufferLineFormat, s:TBE__FORMAT_PATH) != -1
        let pathWid = s:TBE_maxWidthOfBufferProperty(a:groupNum, 's:Buffer_getPath')
    endif

    if stridx(g:TBE_bufferLineFormat, s:TBE__FORMAT_DIR) != -1
        let dirWid = s:TBE_maxWidthOfBufferProperty(a:groupNum, 's:Buffer_getDir')
    endif

    let bi = 0
    while bi < s:Group__bufferCount{a:groupNum}
        normal o
        let b = s:Group__bufferNumber{a:groupNum}_{bi}
        call setline('.', s:TBE_renderBufferLine(a:groupNum, b, numWid, nameWid, pathWid, dirWid))
        let bi = bi + 1
    endwhile

    if s:TBE__showHelp
        silent! 1put! =s:TBE__HELP_TEXT
    endif

    " make window fit
    call s:TBE_adjustWindowSize()

    normal gg

    execute s:TBE_bufferHeaderLineCount + 1
    setlocal nomodifiable readonly

    let @" = old_yank
endfunction

function! s:TBE_adjustWindowSize()
    if g:TBE_showVertically
        return
    endif

    let size = 0
    let bufferWindowSize = 0
    let groupWindowSize  = 0

    let winnr = winnr()
    let bufferWindowNr = bufwinnr(s:TBE__BUFFER_NAME)
    let groupWindowNr  = bufwinnr(s:TBE__GROUP_BUFFER_NAME)

    if bufferWindowNr != -1
        execute bufferWindowNr . 'wincmd w'
        let bufferWindowSize = line('$')
    endif

    if groupWindowNr != -1
        execute groupWindowNr . 'wincmd w'
        let groupWindowSize = line('$')
    endif

    execute winnr .'wincmd w'

    if bufferWindowSize < groupWindowSize
        let size = groupWindowSize
    else
        let size = bufferWindowSize
    endif

    execute size . 'wincmd _'
endfunction

function! s:TBE_renderBufferLine(groupNum, bufferNum, numWid, nameWid, pathWid, dirWid)
    let number = s:TBE_alignRight('' . a:bufferNum, a:numWid)
    let name   = s:TBE_alignLeft('' . s:Buffer_getName(a:bufferNum), a:nameWid)
    let path   = s:TBE_alignLeft('' . s:Buffer_getPath(a:bufferNum), a:pathWid)
    let dir    = s:TBE_alignLeft('' . s:Buffer_getDir(a:bufferNum), a:dirWid)

    if a:bufferNum == s:TBE__currentBufferNum
        let current = '%'
    elseif a:bufferNum == s:TBE__alternativeBufferNum
        let current = '#'
    else
        let current = ' '
    endif

    if bufwinnr(a:bufferNum) == -1
        let hidden = 'h'
    else
        let hidden = 'a'
    endif

    if getbufvar(a:bufferNum, '&modified')
        let modified = '+'
    else
        let modified = ' '
    endif

    let line = g:TBE_bufferLineFormat
    let line = substitute(line, s:TBE__FORMAT_NUMBER,    number,    '')
    let line = substitute(line, s:TBE__FORMAT_NAME,      escape(name, '\'), '')
    let line = substitute(line, s:TBE__FORMAT_PATH,      escape(path, '\'), '')
    let line = substitute(line, s:TBE__FORMAT_DIR,       escape(dir, '\'), '')
    let line = substitute(line, s:TBE__FORMAT_CURRENT,   current  , '')
    let line = substitute(line, s:TBE__FORMAT_HIDDEN,    hidden   , '')
    let line = substitute(line, s:TBE__FORMAT_MODIFIED,  modified , '')

    return line
endfunction

function! s:TBE_renderGroupLine(groupNum, countWid, nameWid, pathWid)
    let bufferCount = s:TBE_alignRight('' . s:Group_getBufferCount(a:groupNum), a:countWid)
    let name        = s:TBE_alignLeft(s:Group_getName(a:groupNum), a:nameWid)
    let path        = s:TBE_alignLeft(s:Group_getPath(a:groupNum), a:pathWid)

    let line = g:TBE_groupLineFormat
    let line = substitute(line, s:TBE__FORMAT_COUNT, bufferCount, '')
    let line = substitute(line, s:TBE__FORMAT_NAME,  escape(name, '\'),        '')
    let line = substitute(line, s:TBE__FORMAT_PATH,  escape(path, '\'),        '')

    return line
endfunction

function! s:TBE_alignLeft(str, width)
    let result = a:str

    let i = strlen(a:str)
    while i < a:width
        let result = result . ' '
        let i = i + 1
    endwhile

    return result
endfunction

function! s:TBE_alignRight(str, width)
    let result = a:str

    let i = strlen(a:str)
    while i < a:width
        let result = ' ' . result
        let i = i + 1
    endwhile

    return result
endfunction

function! s:TBE_maxWidthOfBufferProperty(groupNum, propGetter)
    let maxWidth = 0

    let bufferCount = s:Group_getBufferCount(a:groupNum)
    let i = 0
    while i < bufferCount
        let bufferNum = s:Group_getNthBuffer(a:groupNum, i)
        let width = strlen({a:propGetter}(bufferNum))
        if maxWidth < width
            let maxWidth = width
        endif
        let i = i + 1
    endwhile

    return maxWidth
endfunction

function! s:TBE_maxWidthOfGroupProperty(propGetter)
    let maxWidth = 0

    let groupCount = s:TBE_getGroupCount()
    let i = 0
    while i < groupCount
        let width = strlen({a:propGetter}(i))
        if maxWidth < width
            let maxWidth = width
        endif
        let i = i + 1
    endwhile

    return maxWidth
endfunction

function! s:TBE_showWindow(name, showVertically)
    let winNum = bufwinnr(a:name)
    if winNum != -1
        execute 'keepalt ' . winNum . 'wincmd w'
        return
    endif

    let showMethod = (a:showVertically ? 'vertical new' : 'new')

    execute 'keepalt ' . showMethod . ' ' . a:name

    setlocal nowrap nonumber buftype=nofile bufhidden=delete noswapfile
endfunction

function! s:TBE_showMainWindow()
    " current buffer
    let s:TBE__currentBufferNum     = bufnr('%')
    let s:TBE__alternativeBufferNum = bufnr('#')

    let winNum = bufwinnr(s:TBE__BUFFER_NAME)
    if winNum != -1
        execute winNum . 'wincmd w'
        return
    endif

    call s:TBE_showWindow(s:TBE__BUFFER_NAME, g:TBE_showVertically)
    setlocal nowrap nonumber buftype=nofile bufhidden=delete noswapfile

    if g:TBE_alwaysShowGroupWindow
        call s:TBE_showGroupWindow()
        execute bufwinnr(s:TBE__BUFFER_NAME) . 'wincmd w'
    endif

    noremap <silent> <buffer> v       V
    noremap <silent> <buffer> <C-V>   V
    noremap <silent> <buffer> q       :call <SID>TBE_hideMainWindow()<CR>
    noremap <silent> <buffer> <Esc>   :call <SID>TBE_hideMainWindow()<CR>
    noremap <silent> <buffer> l       :call <SID>TBE_gotoNextGroup()<CR>
    noremap <silent> <buffer> h       :call <SID>TBE_gotoPrevGroup()<CR>
    noremap <silent> <buffer> /       :call <SID>TBE_showSearchGroup()<CR>
    noremap <silent> <buffer> m       :call <SID>TBE_showMRUGroup()<CR>
    noremap <silent> <buffer> n       :call <SID>TBE_gotoDirectoryGroup()<CR>
    noremap <silent> <buffer> <C-J>   :call <SID>TBE_jumpToBuffer(line('.'))<CR>
    noremap <silent> <buffer> <Enter> :call <SID>TBE_jumpToBuffer(line('.'))<CR>
    noremap <silent> <buffer> d       :call <SID>TBE_deleteBuffer()<CR>
    noremap <silent> <buffer> f       :call <SID>TBE_showGroupWindow()<CR>
    noremap <silent> <buffer> r       :call <SID>TBE_refreshWindow()<CR>
    noremap <silent> <buffer> ?       :call <SID>TBE_toggleHelp()<CR>

    syntax match Keyword    /^.\+ \.\{3\}/me=e-4
    syntax match Comment    /\.\{3\} .\+$/ms=s+3
    syntax match StatusLine /^=.\+=$/

    if g:TBE_showMRUFirst
        call s:TBE_renderCurrentGroup(s:TBE_findGroupByPath(s:MRUGroup_PATH))
    else
        call s:TBE_renderCurrentGroup(s:TBE_getNthGroupInOrder(s:TBE__currentGroupNumOrder))
    endif
endfunction

function! s:TBE_toggleHelp()
    let s:TBE__showHelp = !s:TBE__showHelp
    let s:TBE_bufferHeaderLineCount = s:TBE__INITIAL_HEADER_LINE_COUNT + (s:TBE__showHelp ? s:TBE__HELP_LINE_COUNT : 0)
    call s:TBE_renderCurrentGroup(s:TBE_getNthGroupInOrder(s:TBE__currentGroupNumOrder))
endfunction

function! s:TBE_refreshWindow()
    let mru = s:TBE_findGroupByPath(s:MRUGroup_PATH)
    if mru == 0
        let s:TBE__groupCount = 1
    else
        let s:TBE__groupCount = 0
        let mru = s:TBE_createMRUGroup()
    endif
    call s:TBE_collectBuffers()
    let s:TBE__currentGroupNumOrder = mru

    call s:TBE_renderCurrentGroup(mru)
endfunction

function! s:TBE_gotoDirectoryGroup()
    let dir = s:Buffer_getDir(s:TBE__currentBufferNum)

    let groupCount = s:TBE_getGroupCount()
    let i = 0
    while i < groupCount
        let g = s:TBE_getNthGroupInOrder(i)
        if s:Group_getPath(g) == dir
            call s:TBE_gotoGroupByOrder(i)
            break
        endif

        let i = i + 1
    endwhile
endfunction

function! s:TBE_hideMainWindow() range
    silent! execute bufnr(s:TBE__GROUP_BUFFER_NAME) . 'bdelete!'
    silent! execute bufnr(s:TBE__BUFFER_NAME) . 'bdelete!'
endfunction

function! s:TBE_showGroupWindow()
    call s:TBE_showWindow(s:TBE__GROUP_BUFFER_NAME, !g:TBE_showVertically)

    setlocal nowrap nonumber buftype=nofile bufhidden=delete noswapfile

    noremap <silent> <buffer> q       :call <SID>TBE_hideGroupWindow()<CR>
    noremap <silent> <buffer> <Esc>   :call <SID>TBE_hideGroupWindow()<CR>
    noremap <silent> <buffer> <C-J>   :call <SID>TBE_jumpToGroup(line('.'))<CR>
    noremap <silent> <buffer> <Enter> :call <SID>TBE_jumpToGroup(line('.'))<CR>
    noremap <silent> <buffer> f       :call <SID>TBE_jumpToGroup(line('.'))<CR>

    call s:TBE_renderGroups()
endfunction

function! s:TBE_renderGroups()
    setlocal modifiable noreadonly

    " stop the yank hijack
    let old_yank = @"

    %d

    " render group line

    let countWid  = 0
    let nameWid   = 0
    let pathWid   = 0

    if stridx(g:TBE_groupLineFormat, s:TBE__FORMAT_COUNT) != -1
        let countWid = s:TBE_maxWidthOfGroupProperty('s:Group_getBufferCount')
    endif

    if stridx(g:TBE_groupLineFormat, s:TBE__FORMAT_NAME) != -1
        let nameWid = s:TBE_maxWidthOfGroupProperty('s:Group_getName')
    endif

    if stridx(g:TBE_groupLineFormat, s:TBE__FORMAT_PATH) != -1
        let pathWid = s:TBE_maxWidthOfGroupProperty('s:Group_getPath')
    endif

    call setline(1, '::Groups::')
    let i = 1
    while i < s:TBE_groupHeaderLineCount
        normal o
        let i = i + 1
    endwhile

    let groupCount = s:TBE_getGroupCount()
    let o = 0
    while o < groupCount
        let g = s:TBE_getNthGroupInOrder(o)

        normal o
        call setline('.', s:TBE_renderGroupLine(g, countWid, nameWid, pathWid))

        let o = o + 1
    endwhile

    " make window fit
    call s:TBE_adjustWindowSize()

    normal gg
    execute s:TBE_groupHeaderLineCount + 1 + s:TBE__currentGroupNumOrder
    setlocal nomodifiable readonly

    let @" = old_yank
endfunction

function! s:TBE_jumpToGroup(order)
    let order = a:order - s:TBE_groupHeaderLineCount - 1
    if order < 0
        return
    endif

    call s:TBE_hideGroupWindow()
    call s:TBE_gotoGroupByOrder(order)
endfunction

function! s:TBE_hideGroupWindow() range
    if g:TBE_alwaysShowGroupWindow
        execute bufwinnr(s:TBE__BUFFER_NAME) . 'wincmd w'
    else
        execute bufnr(s:TBE__GROUP_BUFFER_NAME) . 'bdelete!'
    endif
endfunction

function! s:TBE_deleteBuffer() range
    if a:firstline <= s:TBE_bufferHeaderLineCount
        return
    endif

    let groupNum = s:TBE_getNthGroupInOrder(s:TBE__currentGroupNumOrder)
    let delCount = a:lastline - a:firstline + 1

    let msg = 'Are you sure to delete '
    if delCount == 1
        let msg = msg . '`' . s:Buffer_getName(s:Group_getNthBuffer(groupNum, a:firstline  - s:TBE_bufferHeaderLineCount - 1)) . "'"
    else
        let msg = msg . (a:lastline - a:firstline + 1) . ' buffers'
    endif
    let msg = msg . '? [y/N]'

    echo msg
    let ans = tolower(nr2char(getchar()))

    " clear the echo
    normal :<Esc>

    if char2nr(ans) != char2nr('y')
        return
    endif

    let i = 0
    while i < delCount
        let bufferNum = s:Group_getNthBuffer(groupNum, a:firstline - s:TBE_bufferHeaderLineCount - 1)
        if !s:TBE_isUntouchableBuffer(bufferNum)
            call s:Buffer_delete(bufferNum)
        else
            " doesn't delete actually, but acts as if deleted
            call s:TBE_onBufferDeletion(bufferNum)
        endif

        let i = i + 1
    endwhile

    let groupNum = s:TBE_getNthGroupInOrder(s:TBE__currentGroupNumOrder)
    " refresh group window
    if bufwinnr(s:TBE__GROUP_BUFFER_NAME) != -1
        call s:TBE_showGroupWindow()
        execute bufwinnr(s:TBE__BUFFER_NAME) . 'wincmd w'
    endif
    " refresh main(buffer) window
    call s:TBE_renderCurrentGroup(groupNum)
endfunction

function! s:TBE_jumpToBuffer(index)
    let index = a:index - s:TBE_bufferHeaderLineCount - 1
    if index < 0
        return
    endif

    let groupNum = s:TBE_getNthGroupInOrder(s:TBE__currentGroupNumOrder)
    let bufferNum = s:Group_getNthBuffer(groupNum, index)

    call s:TBE_hideMainWindow()
    execute bufferNum . 'b'
endfunction

function! s:TBE_showMRUGroup()
    let groupNum = s:TBE_findGroupByPath(s:MRUGroup_PATH)
    let order = s:TBE_getOrderOfGroup(groupNum)

    let s:TBE__currentGroupNumOrder = order
    call s:TBE_renderCurrentGroup(groupNum)
endfunction

function! s:TBE_showSearchGroup()
    let pattern = input('Search pattern: ')
    if pattern == '/'
        let groupNum = s:TBE_findGroupByPath(s:SearchGroup_PATH)
        if groupNum != s:INVALID_INDEX
            call s:TBE_gotoGroup(groupNum)
            echo 'Jumping to the Search group.'
        endif
    elseif strlen(pattern) != 0
        " on creation, groupNum == order
        let groupNum = s:TBE_createSearchGroup(pattern)
        let s:TBE__currentGroupNumOrder = groupNum
        " let s:TBE__currentGroupNumOrder = s:TBE_getOrderOfGroup(groupNum)

        call s:TBE_renderCurrentGroup(groupNum)
    else
        echo 'Canceled'
    endif
endfunction

function! s:TBE_gotoGroup(groupNum)
    let order = s:TBE_getOrderOfGroup(a:groupNum)
    call s:TBE_gotoGroupByOrder(order)
endfunction

function! s:TBE_gotoGroupByOrder(order)
    let s:TBE__currentGroupNumOrder = a:order
    call s:TBE_renderCurrentGroup(s:TBE_getNthGroupInOrder(a:order))
endfunction

function! s:TBE_gotoNextGroup()
    let groupCount = s:TBE_getGroupCount()
    let order = (s:TBE__currentGroupNumOrder + 1) % groupCount
    call s:TBE_gotoGroupByOrder(order)
endfunction

function! s:TBE_gotoPrevGroup()
    let groupCount = s:TBE_getGroupCount()
    let order = (s:TBE__currentGroupNumOrder - 1 + groupCount) % groupCount
    call s:TBE_gotoGroupByOrder(order)
endfunction


"-----------------------------
" Expansive group definitions
"-----------------------------

"
" Directory Group class
"
function! s:TBE_createDirectoryGroup(name, path, delTiming)
    let groupNum = s:TBE_createGroup(a:name, a:path, a:delTiming)

    let s:Group__bufferAdditionHandler{groupNum} = 's:DirectoryGroup__handleBufferAddition'
    let s:Group__bufferCollectionHandler{groupNum} = 's:DirectoryGroup__handleBufferAddition'
    let s:Group__bufferDeletionHandler{groupNum} = 's:DirectoryGroup__handleBufferDeletion'

    return groupNum
endfunction

function! s:DirectoryGroup__handleBufferAddition(groupNum, bufferNum)
    if s:Group_getPath(a:groupNum) == s:Buffer_getDir(a:bufferNum)
        call s:Group_addBufferInOrder(a:groupNum, a:bufferNum)
        return 1
    else
        return 0
    endif
endfunction

function! s:DirectoryGroup__handleBufferDeletion(groupNum, bufferNum)
    call s:Group_removeBuffer(a:groupNum, a:bufferNum)
    return 1
endfunction

"
" MRU Group class
"
let s:MRUGroup_PATH = '::MRU::'

function! s:TBE_createMRUGroup()
    let groupNum = s:TBE_findGroupByPath(s:MRUGroup_PATH)
    if groupNum == s:INVALID_INDEX
        let groupNum = s:TBE_createGroup(
                    \ 'MRU Buffers',
                    \ s:MRUGroup_PATH,
                    \ s:Group_DELETE_MANUALLY)

        let s:Group__bufferEntranceHandler{groupNum} = 's:MRUGroup__handleBufferEntrance'
        let s:Group__bufferDeletionHandler{groupNum} = 's:MRUGroup__handleBufferDeletion'

        let s:MRUGroup__actualBufferCount = 0
    endif

    return groupNum
endfunction

function! s:MRUGroup__handleBufferEntrance(groupNum, bufferNum)
    let s:Group__bufferCount{a:groupNum} = s:MRUGroup__actualBufferCount
    call s:Group_removeBuffer(a:groupNum, a:bufferNum)
    call s:Group_unshiftBuffer(a:groupNum, a:bufferNum)
    call s:MRUGroup__recalcBufferCount(a:groupNum)

    " don't prevent TBE from creating a (new) normal group
    return 0
endfunction

function! s:MRUGroup__handleBufferDeletion(groupNum, bufferNum)
    let s:Group__bufferCount{a:groupNum} = s:MRUGroup__actualBufferCount
    call s:Group_removeBuffer(a:groupNum, a:bufferNum)
    call s:MRUGroup__recalcBufferCount(a:groupNum)

    return 1
endfunction

function! s:MRUGroup__recalcBufferCount(groupNum)
    let bufferCount = s:Group_getBufferCount(a:groupNum)
    let s:MRUGroup__actualBufferCount = bufferCount
    if g:TBE_mruMax < bufferCount
        let bufferCount = g:TBE_mruMax
    endif
    let s:Group__bufferCount{a:groupNum} = bufferCount
endfunction

"
" Search Group class
"
let s:SearchGroup_PATH = '::Search::'

function! s:TBE_createSearchGroup(pattern)
    let groupNum = s:TBE_findGroupByPath(s:SearchGroup_PATH)
    if groupNum != s:INVALID_INDEX
        call s:TBE_deleteGroup(groupNum)
    endif

    let groupNum = s:TBE_createGroup(
                \ 'Search  /' . a:pattern . '/',
                \ s:SearchGroup_PATH,
                \ s:Group_DELETE_ON_EMPTY)

    let s:Group__bufferAdditionHandler{groupNum}   = 's:SearchGroup__handleBufferAddition'
    let s:Group__bufferCollectionHandler{groupNum} = 's:SearchGroup__handleBufferAddition'
    let s:Group__bufferDeletionHandler{groupNum}   = 's:SearchGroup__handleBufferDeletion'

    let s:SearchGroup__pattern = a:pattern

    call s:TBE_collectBuffers()

    return groupNum
endfunction

function! s:SearchGroup__handleBufferAddition(groupNum, bufferNum)
    if s:SearchGroup_match(s:Buffer_getPath(a:bufferNum))
        call s:Group_addBufferInOrder(a:groupNum, a:bufferNum)
    endif
    return 0
endfunction

function! s:SearchGroup__handleBufferDeletion(groupNum, bufferNum)
    call s:Group_removeBuffer(a:groupNum, a:bufferNum)
    return 1
endfunction

function! s:SearchGroup_match(str)
    return (a:str =~ s:SearchGroup__pattern)
endfunction


"-------------------------
" Fundamental definitions
"-------------------------

"
" global variables
"
let s:INVALID_INDEX = -1


function! s:TBE_createGroup(name, path, delTiming)
    let newGroupNum = s:TBE__groupCount

    call s:Group_init(newGroupNum, a:name, a:path, a:delTiming)
    let s:TBE__order2GroupNum{newGroupNum} = newGroupNum
    let s:TBE__groupCount = newGroupNum + 1

    return newGroupNum
endfunction

function! s:TBE_deleteGroup(groupNum)
    " move the last group to where the deleted group is
    let groupCount = s:TBE_getGroupCount()
    let lastGroupNum = groupCount - 1
    if a:groupNum < lastGroupNum
        " move from lastGroupNum to a:groupNum
        call s:Group_init(
                    \ a:groupNum,
                    \ s:Group__name{lastGroupNum},
                    \ s:Group__path{lastGroupNum},
                    \ s:Group__deletionTiming{lastGroupNum})
        if exists('s:Group__bufferAdditionHandler' . lastGroupNum)
            let s:Group__bufferAdditionHandler{a:groupNum} = s:Group__bufferAdditionHandler{lastGroupNum}
        endif
        if exists('s:Group__bufferDeletionHandler' . lastGroupNum)
            let s:Group__bufferDeletionHandler{a:groupNum}   = s:Group__bufferDeletionHandler{lastGroupNum}
        endif
        if exists('s:Group__bufferEntranceHandler' . lastGroupNum)
            let s:Group__bufferEntranceHandler{a:groupNum}   = s:Group__bufferEntranceHandler{lastGroupNum}
        endif
        if exists('s:Group__bufferCollectionHandler' . lastGroupNum)
            let s:Group__bufferCollectionHandler{a:groupNum} = s:Group__bufferCollectionHandler{lastGroupNum}
        endif

        let bufferCount = s:Group__bufferCount{lastGroupNum}
        let i = 0
        while i < bufferCount
            let s:Group__bufferNumber{a:groupNum}_{i} = s:Group__bufferNumber{lastGroupNum}_{i}
            let i = i + 1
        endwhile
        let s:Group__bufferCount{a:groupNum} = bufferCount
    endif

    " keep order
    let orderOfDeleted = s:TBE_getOrderOfGroup(a:groupNum)
    let orderOfLastGroup = s:TBE_getOrderOfGroup(lastGroupNum)
    " for moving last group to where the deleted group
    let s:TBE__order2GroupNum{orderOfLastGroup} = s:TBE__order2GroupNum{orderOfDeleted}
    " move the order elem(corresponding to the deleted group) to tail
    let i = orderOfDeleted
    while i < groupCount - 1
        let s:TBE__order2GroupNum{i} = s:TBE__order2GroupNum{i + 1}
        let i = i + 1
    endwhile

    " adjust currently displayed group order
    let s:TBE__currentGroupNumOrder = 0

    let s:TBE__groupCount = s:TBE__groupCount - 1
endfunction

function! s:TBE_getGroupCount()
    return s:TBE__groupCount
endfunction

function! s:TBE_getNthGroupInOrder(order)
    return s:TBE__order2GroupNum{a:order}
endfunction

function! s:TBE_getOrderOfGroup(groupNum)
    let order = s:INVALID_INDEX

    let groupCount = s:TBE_getGroupCount()
    let i = 0
    while i < groupCount
        if s:TBE__order2GroupNum{i} == a:groupNum
            let order = i
            break
        endif
        let i = i + 1
    endwhile

    return order
endfunction

function! s:TBE_findGroupByPath(path)
    let groupNum = s:INVALID_INDEX

    let groupCount = s:TBE__groupCount
    let i = 0
    while i < groupCount
        if s:Group_getPath(i) == a:path
            let groupNum = i
            break
        endif
        let i = i + 1
    endwhile

    return groupNum
endfunction

function! s:TBE_collectBuffers()
    let buffLast = bufnr('$') + 1
    let i = 1

    while i < buffLast
        if !s:TBE_isUntouchableBuffer(i)
            call s:TBE_onBufferCollection(i)
        endif

        let i = i + 1
    endwhile
endfunction

function! s:TBE_isUntouchableBuffer(bufferNum)
    return
                \ 0
                \ || (bufname(a:bufferNum) == s:TBE__BUFFER_NAME)
                \ || (bufname(a:bufferNum) == s:TBE__GROUP_BUFFER_NAME)
                \ || (bufname(a:bufferNum) == s:TBE__DUMP_BUFFER_NAME)
                \ || !buflisted(a:bufferNum)
                \ || (getbufvar(a:bufferNum, '&filetype') == 'help')
endfunction

" global event handler definitions

let s:TBE__HANDLER_BUFFER_ADDITION   = 's:Group__bufferAdditionHandler'
let s:TBE__HANDLER_BUFFER_DELETION   = 's:Group__bufferDeletionHandler'
let s:TBE__HANDLER_BUFFER_ENTRANCE   = 's:Group__bufferEntranceHandler'
let s:TBE__HANDLER_BUFFER_COLLECTION = 's:Group__bufferCollectionHandler'

function! s:SID()
    return matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_SID$')
endfun

function! s:TBE_hookEvents()
    augroup TinyBufferExplorer
        autocmd!
        autocmd  BufAdd    * call <SID>TBE_invokeEventHandler('<SNR>' . s:SID() . '_TBE_onBufferAddition', expand('<abuf>') + 0)
        autocmd  BufDelete * call <SID>TBE_invokeEventHandler('<SNR>' . s:SID() . '_TBE_onBufferDeletion', expand('<abuf>') + 0)
        autocmd  BufEnter  * call <SID>TBE_invokeEventHandler('<SNR>' . s:SID() . '_TBE_onBufferEntrance', expand('<abuf>') + 0)
    augroup END
endfunction

function! s:TBE_invokeEventHandler(handlerName, bufferNum)
    if !s:TBE_isUntouchableBuffer(a:bufferNum)
        call {a:handlerName}(a:bufferNum)
    endif
endfunction

" global event handler for buffer collection
function! s:TBE_onBufferCollection(bufferNum)
    call s:Buffer_init(a:bufferNum)

    let reaction = s:TBE_notifyAllGroupsOfEvent(s:TBE__HANDLER_BUFFER_COLLECTION, a:bufferNum)

    " if there is no group for the buffer, create new one
    if !reaction
        let name = expand('#' . a:bufferNum . ':p:h:t')
        let path = s:Buffer_getDir(a:bufferNum)
        if strlen(name) == 0
            let name = path
        endif

        let newGroupNum = s:TBE_createDirectoryGroup(name, path, s:Group_DELETE_ON_EMPTY)
        call s:Group_addBuffer(newGroupNum, a:bufferNum)
    endif
endfunction

" global event handler for buffer addition
function! s:TBE_onBufferAddition(bufferNum)
    let reaction = s:TBE_notifyAllGroupsOfEvent(s:TBE__HANDLER_BUFFER_ADDITION, a:bufferNum)

    " if there is no group for the buffer, create new one
    if !reaction
        let name = expand('#' . a:bufferNum . ':p:h:t')
        let path = s:Buffer_getDir(a:bufferNum)
        if strlen(name) == 0
            let name = path
        endif

        let newGroupNum = s:TBE_createDirectoryGroup(name, path, s:Group_DELETE_ON_EMPTY)
        call s:Group_addBuffer(newGroupNum, a:bufferNum)
    endif
endfunction

" global event handler for buffer deletion
function! s:TBE_onBufferDeletion(bufferNum)
    call s:TBE_notifyAllGroupsOfEvent(s:TBE__HANDLER_BUFFER_DELETION, a:bufferNum)

    call s:TBE_dump('notified deletion of ' . s:Buffer_getName(a:bufferNum))

    let gi = 0
    while gi < s:TBE_getGroupCount()
        if (s:Group_getBufferCount(gi) == 0)
                    \ && (s:Group_getDeletionTiming(gi) == s:Group_DELETE_ON_EMPTY)
            call s:TBE_deleteGroup(gi)
            call s:TBE_dump('group ' . s:Group_getName(gi) . ' is deleted')
        else
            let gi = gi + 1
        endif
    endwhile
endfunction

" global event handler for buffer entrance
function! s:TBE_onBufferEntrance(bufferNum)
    return s:TBE_notifyAllGroupsOfEvent(s:TBE__HANDLER_BUFFER_ENTRANCE, a:bufferNum)
endfunction

function! s:TBE_notifyAllGroupsOfEvent(eventHandlerName, bufferNum)
    call s:Buffer_init(a:bufferNum)

    let groupCount = s:TBE__groupCount
    let gi = 0
    let reaction = 0
    while gi < groupCount
        if exists(a:eventHandlerName . gi)
            let reaction = reaction + {{a:eventHandlerName}{gi}}(gi, a:bufferNum)
        endif
        let gi = gi + 1
    endwhile

    return reaction
endfunction


".............
" Group class
".............
let s:Group_DELETE_MANUALLY = 0
let s:Group_DELETE_ON_EMPTY = 1

function! s:Group_init(groupNum, name, path, delTiming)
    let s:Group__name{a:groupNum} = a:name
    let s:Group__path{a:groupNum} = a:path
    let s:Group__deletionTiming{a:groupNum} = a:delTiming
    let s:Group__bufferCount{a:groupNum} = 0
"    let s:Group__bufferNumber{a:groupNum}_{n} = buffer number

    " init event handlers
    " function! additionHandler(groupNum, bufferNum) : non-0 if reacted
    unlet! s:Group__bufferAdditionHandler{a:groupNum}
    unlet! s:Group__bufferDeletionHandler{a:groupNum}
    unlet! s:Group__bufferEntranceHandler{a:groupNum}
    unlet! s:Group__bufferCollectionHandler{a:groupNum}
endfunction

function! s:Group_getName(groupNum)
    return s:Group__name{a:groupNum}
endfunction

function! s:Group_getPath(groupNum)
    return s:Group__path{a:groupNum}
endfunction

function! s:Group_getDeletionTiming(groupNum)
    return s:Group__deletionTiming{a:groupNum}
endfunction

function! s:Group_getBufferCount(groupNum)
    return s:Group__bufferCount{a:groupNum}
endfunction

function! s:Group_getNthBuffer(groupNum, n)
    return s:Group__bufferNumber{a:groupNum}_{a:n}
endfunction

function! s:Group_indexOfBuffer(groupNum, bufferNum)
    let index = s:INVALID_INDEX

    let i = 0
    let outOfBound = s:Group__bufferCount{a:groupNum}
    while i < outOfBound
        if a:bufferNum == s:Group__bufferNumber{a:groupNum}_{i}
            let index = i
            break
        endif

        let i = i + 1
    endwhile

    return index
endfunction

" return non-0 value if this group added the buffer
function! s:Group_addBuffer(groupNum, bufferNum)
    let added = 0

    let index = s:Group_indexOfBuffer(a:groupNum, a:bufferNum)
    if index == s:INVALID_INDEX
        if !exists('s:Buffer__name' . a:bufferNum)
            call s:Buffer_init(a:bufferNum)
        endif

        let bufferCount = s:Group__bufferCount{a:groupNum}
        let s:Group__bufferNumber{a:groupNum}_{bufferCount} = a:bufferNum
        let s:Group__bufferCount{a:groupNum} = bufferCount + 1

        let added = 1
    endif

    return added
endfunction

" return non-0 value if this group deleted the buffer
function! s:Group_removeBuffer(groupNum, bufferNum)
    let deleted = 0

    let index = s:Group_indexOfBuffer(a:groupNum, a:bufferNum)
    if index != s:INVALID_INDEX
        let bufferCount = s:Group__bufferCount{a:groupNum}
        let i = index
        while i < bufferCount - 1
            let s:Group__bufferNumber{a:groupNum}_{i} = s:Group__bufferNumber{a:groupNum}_{i + 1}
            let i = i + 1
        endwhile

        let s:Group__bufferCount{a:groupNum} = bufferCount - 1

        let deleted = 1
    endif

    return deleted
endfunction

function! s:Group_addBufferInOrder(groupNum, bufferNum)
    let added = s:Group_addBuffer(a:groupNum, a:bufferNum)
    if !added
        return
    endif

    let i = s:Group_getBufferCount(a:groupNum) - 1
    let name = s:Buffer_getName(a:bufferNum)
    while ((0 < i) && (name < s:Buffer_getName(s:Group_getNthBuffer(a:groupNum, i - 1))))
        let s:Group__bufferNumber{a:groupNum}_{i} = s:Group__bufferNumber{a:groupNum}_{i - 1}
        let i = i - 1
    endwhile

    let s:Group__bufferNumber{a:groupNum}_{i} = a:bufferNum
endfunction

function! s:Group_unshiftBuffer(groupNum, bufferNum)
    let srcIndex = s:Group_indexOfBuffer(a:groupNum, a:bufferNum)
    if srcIndex == s:INVALID_INDEX
        call s:Group_addBuffer(a:groupNum, a:bufferNum)
        let srcIndex = s:Group_getBufferCount(a:groupNum) - 1
    endif

    " move
    let i = srcIndex
    while 0 < i
        let s:Group__bufferNumber{a:groupNum}_{i} = s:Group__bufferNumber{a:groupNum}_{i - 1}
        let i = i - 1
    endwhile
    let s:Group__bufferNumber{a:groupNum}_{0} = a:bufferNum
endfunction

" return non-0 value if this group added OR ALREADY CONTAINS the buffer
function! s:Group_handleBufferAddition(groupNum, bufferNum)
    if exists('s:Group__bufferAdditionHandler' . a:groupNum)
        return {s:Group__bufferAdditionHandler{a:groupNum}}(a:groupNum, a:bufferNum)
    else
        return 0
    endif
endfunction

function! s:Group_handleBufferDeletion(groupNum, bufferNum)
    if exists('s:Group__bufferDeletionHandler' . a:groupNum)
        return {s:Group__bufferDeletionHandler{a:groupNum}}(a:groupNum, a:bufferNum)
    else
        return 0
    endif
endfunction

function! s:Group_handleBufferEntrance(groupNum, bufferNum)
    if exists('s:Group__bufferEntranceHandler' . a:groupNum)
        return {s:Group__bufferEntranceHandler{a:groupNum}}(a:groupNum, a:bufferNum)
    else
        return 0
    endif
endfunction

function! s:Group_handleBufferCollection(groupNum, bufferNum)
    if exists('s:Group__bufferCollectionHandler' . a:groupNum)
        return {s:Group__bufferCollectionHandler{a:groupNum}}(a:groupNum, a:bufferNum)
    else
        return 0
    endif
endfunction


"..............
" Buffer class
"..............
function! s:Buffer_init(bufferNum)
    let s:Buffer__name{a:bufferNum} = expand('#' . a:bufferNum . ':p:t')
    let s:Buffer__path{a:bufferNum} = expand('#' . a:bufferNum . ':p')
    let s:Buffer__dir{a:bufferNum}  = expand('#' . a:bufferNum . ':p:h')
endfunction

function! s:Buffer_getName(bufferNum)
    return s:Buffer__name{a:bufferNum}
endfunction

function! s:Buffer_getPath(bufferNum)
    return s:Buffer__path{a:bufferNum}
endfunction

function! s:Buffer_getDir(bufferNum)
    return s:Buffer__dir{a:bufferNum}
endfunction

function! s:Buffer_delete(bufferNum)
    let s:Buffer__name{a:bufferNum} = ''
    let s:Buffer__path{a:bufferNum} = ''
    let s:Buffer__dir{a:bufferNum}  = ''
    unlet! s:Buffer__name{a:bufferNum}
    unlet! s:Buffer__path{a:bufferNum}
    unlet! s:Buffer__dir{a:bufferNum}
    execute a:bufferNum . 'bdelete!'
endfunction


"-----------------
" Startup routine
"-----------------

call s:TBE_init()

" vim: set fenc=utf-8 ff=unix ts=4 sts=4 sw=4 et : 
