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
"           - All buffers group
"       3. ease of accessing buffers and groups
"         - the MRU group can list buffers you are likely to edit
"         - the group list supports you switching among groups
"         - it's easy to switch to the Directory group in which
"           the current buffer is
"       4. selectable UIs
"         - Fullfeatured (default), SimpleGroup and Minimal
"         - SimpleGroup: one window, supports switching groups
"         - Minimal: one window, shows all buffers
"       5. customizable list format (Fullfeatured UI only)
"
" Maintainer: Shuhei Kubota <kubota.shuhei@gmail.com>, @chimatter
"
" Usage:
"
"   [Commands and UIs]
"
"      :TinyBufferExplorer or :TBE
"   
"          shows the TBE window (showing buffer list) with Fullfeatured UI.
"          Two windows are shown. The buffer list window and the group list window.
"
"      :TBESimpleGroup
"   
"          shows the TBE with the SimpleGroup UI.
"          One window is shown. In order to switch groups, use <Tab> key.
"
"      :TBEMinimal
"   
"          shows the TBE with the Minimal UI.
"          This doesn't provide grouping feature.
"          This doesn't show groups and does show all buffers.
"
"   [Options(Variables)]
"
"      Note that the right-hand value is a default value.
"   
"      g:TBE_showVertically = 0
"
"          <Fullfeatured, SimpleGroup, Minimal>
"   
"          If its value is 0, the window is shown hirizontally. Otherwise (if 0),
"          the window is shown vertically.
"   
"      g:TBE_mruMax = 10
"
"          <Fullfeatured, SimpleGroup>
"      
"          The maximum number of buffers which are shown in the MRU group.
"   
"      g:TBE_showMRUFirst = 0
"
"          <Fullfeatured, SimpleGroup>
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
"          <Fullfeatured>
"   
"          This option affects when you open the TBE window.
"   
"          If its value is non-0, TBE shows the buffer window and the group
"          window at the same time. This make TBE slower (a little).
"   
"      g:TBE_bufferLineFormat = '{number} | {%}{h} {+} {name}   {dir}'
"
"          <Fullfeatured>
"   
"          A format of a buffer. For more specifications, execute
"          "/!exists('g:TBE_bufferLineFormat')" and read what is written there.
"   
"      g:TBE_groupLineFormat = '{name}  -{count}-    {path}'
"
"          <Fullfeatured>
"   
"          A format of a group. For more specifications, execute
"          "/!exists('g:TBE_groupLineFormat')" and read what is written there.
"   
"   
"   [Key Mappings]
"
"       j, k:
"           <Fullfeatured, SimpleGroup, Minimal>
"           moves cursor upward/downward.
"
"       h, l:
"           <Fullfeatured>
"           switches group shown in the buffer window.
"
"           <SimpleGroup, Minimal>
"           moves cursor leftward/rightward.
"
"       <Tab>, <S-Tab>:
"           <Fullfeatured, SimpleGroup>
"           switches group shown in the buffer window.
"
"       <Enter>, <C-J>:
"           <Fullfeatured, SimpleGroup, Minimal>
"           opens the buffer under the cursor.
"
"       q, <Esc>: 
"           <Fullfeatured, SimpleGroup, Minimal>
"           escapes from the buffer list window.
"
"       d, D:
"           <Fullfeatured, SimpleGroup, Minimal>
"           deletes a buffer.
"
"           <Fullfeatured>
"           If multiple buffers are selected, they are all deleted.
"
"       f, F:
"           <Fullfeatured>
"           shows the group window.
"
"       g, G:
"           <Fullfeatured>
"           shows the group window.
"
"           <SimpleGroup>
"           shows the group selection window.
"
"       n, N:
"           <Fullfeatured, SimpleGroup>
"           shows the Directory group in which the current buffer is.
"
"       m, M:
"           <Fullfeatured, SimpleGroup>
"           shows the Most Recently Used Buffers group.
"
"       a, A:
"           <Fullfeatured, SimpleGroup>
"           shows the ALL buffers group.
"
"       /:
"           <Fullfeatured, SimpleGroup>
"           shows the Search group (searched by the path of each buffer).
"
"       r:
"           <Fullfeatured, SimpleGroup, Minimal>
"           re-collects buffers and refreshes the window.
"
"       v, V, <C-V>:
"           <Fullfeatured>
"           selects buffers(lines). Selected buffers are targets for deletion.
"
"==============================
" Commands
"==============================

command!  TinyBufferExplorer call g:TBE_useFullfeaturedUI()
command!  TBE                call g:TBE_useFullfeaturedUI()
command!  TBESimpleGroup     call g:TBE_useSimpleGroupUI()
command!  TBEMinimal         call g:TBE_useMinimalUI()

"==============================
" Options
"==============================

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

"==============================
" Code
"==============================

"------------------------------
" UI Common 
"------------------------------

let s:TBE_MAIN_BUFFER_NAME = 'TinyBufferExplorer'
let s:TBE_GROUP_BUFFER_NAME = 'TinyBufferExplorerGroups'

function! s:UI_memoCurrentBuffer()
    let s:Buffer_currentBufferNumber = bufnr('%')
    let s:Buffer_alternateBufferNumber = bufnr('#')
endfunction

" let s:TBE_lastGroup = {}
function! s:UI_memoLastGroup(group)
    let s:TBE_lastGroup = a:group
endfunction

function! s:UI_closeBufferWindow()
    silent! execute bufnr(s:TBE_MAIN_BUFFER_NAME) . 'bdelete!'
    silent! execute bufnr(s:TBE_GROUP_BUFFER_NAME) . 'bdelete!'
endfunction

function! s:UI_backtoMainWindow()
    if g:TBE_showVertically | let method = 'vertical new' | else | let method = 'new' | endif
    call s:Buffer_openTempWindow(s:TBE_MAIN_BUFFER_NAME, method, 1)
    if !g:TBE_alwaysShowGroupWindow
        silent! execute bufnr(s:TBE_GROUP_BUFFER_NAME) . 'bdelete!'
    endif
endfunction

function! s:UI_jumpToBuffer(bufferNumber, closeBufferWindow)
    if a:closeBufferWindow
        call s:UI_closeBufferWindow()
    endif
    execute a:bufferNumber . 'b'
endfunction

function! s:UI_deleteBuffer(bufferNumbers)
    " confirm this delete operation
    let msg = 'Are you sure to delete '
    if len(a:bufferNumbers) == 0
        return
    elseif len(a:bufferNumbers) == 1
        let msg = msg . s:Buffer_create(a:bufferNumbers[0]).getName()
    else
        let msg = msg . len(a:bufferNumbers) . ' buffers'
    endif
    let msg = msg . '? [y/N]'

    echo msg
    let ans = tolower(nr2char(getchar()))
    " clear the echo
    normal :<Esc>

    if char2nr(ans) != char2nr('y')
        return
    endif

    for b in a:bufferNumbers
        silent execute b . 'bdelete!'
    endfor
endfunction

function! s:UI_showSearchGroup()
    let pattern = input('Search pattern: ')
    if strlen(pattern) != 0
        " replacing existing search group
        let sg = s:TBE_findGroupByID('::Search::')
        if sg != {}
            call s:TBE_removeGroup(sg)
        endif

        call s:SearchGroup_install(pattern)
        let sg = s:TBE_findGroupByID('::Search::')
        if sg != {}
            return sg
        endif
    else
        echo 'Canceled'
    endif

    return {}
endfunction

"------------------------------
" Fullfeatured UI
"------------------------------

let g:TBE_bufferLineFormat = '{%}{h} {+} {name}'
let g:TBE_groupLineFormat  = '{name}  -{count}-    {path}'

let s:TBE_FORMAT_NUMBER   = '{number}' " buffer right
let s:TBE_FORMAT_COUNT    = '{count}'  " group right
let s:TBE_FORMAT_NAME     = '{name}'   " buffer group left
let s:TBE_FORMAT_PATH     = '{path}'   " buffer group left
let s:TBE_FORMAT_DIR      = '{dir}'    " buffer left
let s:TBE_FORMAT_CURRENT  = '{%}'      " buffer left
let s:TBE_FORMAT_HIDDEN   = '{h}'      " buffer left
let s:TBE_FORMAT_MODIFIED = '{+}'      " buffer left

function! g:TBE_useFullfeaturedUI()
    call s:UI_memoCurrentBuffer()

    if g:TBE_showMRUFirst
        let currgrp = s:TBE_findGroupByID('::MRU::')
    else
        let currgrp = s:TBE_findCurrentDirGroup()
        if currgrp == {}
            let currgrp = s:TBE_findGroupByID('::ALL::')
        endif
    endif
    call s:UI_memoLastGroup(currgrp)

    if g:TBE_showVertically | let method = 'vertical new' | else | let method = 'new' | endif
    let b = s:Buffer_openTempWindow(s:TBE_MAIN_BUFFER_NAME, method, 1)
    setlocal nocursorcolumn nocursorline
    call s:FullfeaturedUI_render(s:TBE_lastGroup)
    call s:FullfeaturedUI_setKeymapping()
    call s:FullfeaturedUI_setHighlight()

    if g:TBE_alwaysShowGroupWindow
        call s:FullfeaturedUI_showGroupWindow()
        call s:UI_backtoMainWindow()
    endif
endfunction

function! s:FullfeaturedUI_showGroupWindow()
    if g:TBE_showVertically | let method = 'new' | else | let method = 'vertical new' | endif
    let gb = s:Buffer_openTempWindow(s:TBE_GROUP_BUFFER_NAME, method, 1)
    setlocal nowrap nocursorcolumn nocursorline
    call s:FullfeaturedUI_renderGroups()
    call s:FullfeaturedUI_setGroupKeymapping()
    call s:FullfeaturedUI_setGroupHighlight()
endfunction

function! s:FullfeaturedUI_jumpToGroup()
    let gi = line('.')
    if gi < 3 | return | endif
    call s:UI_backtoMainWindow()
    call s:FullfeaturedUI_render(s:TBE_groups[gi - 3])
endfunction

function! s:FullfeaturedUI_render(group)
    let g = a:group
    if g == {}
        return
    endif

    call s:UI_memoLastGroup(g)

    " build a view
    let header = g.title . '  -' . len(g.buffers) . '-    ' . g.id
    let nextg = s:TBE_findNextGroup(g)
    if nextg != {}
        let header = header . '    Tab=>' . nextg.title
    endif

    let s = s:FullfeaturedUI_generateBufferLines(g)

    setlocal modifiable noreadonly lazyredraw
    let old_yank = @"
    let old_yank_star = @*
    %delete
    call setline(1, header)
    call setline(2, '')
    call setline(3, s)
    let @* = old_yank_star
    let @" = old_yank
    setlocal nomodifiable readonly lazyredraw

    " adjust height (+2 for group view and an empty line)
    call s:FullfeaturedUI_adjustWindowSize()


    " move to current buffer
    let b = s:Buffer_create(s:Buffer_currentBufferNumber)
    let bi = index(s:TBE_lastGroup.buffers, b)
    if bi != -1
        execute (bi + 3)
    else
        execute 3
    endif
endfunction

function! s:FullfeaturedUI_generateBufferLines(group)
    " max width
    let b = s:Buffer_create(0)
    " number
    let numberWid = 0
    if stridx(g:TBE_bufferLineFormat, s:TBE_FORMAT_NUMBER) != -1
        let numberWid = s:FullfeaturedUI_getMaxWidth(a:group, b.getNumber)
    endif
    " name
    let nameWid = 0
    if stridx(g:TBE_bufferLineFormat, s:TBE_FORMAT_NAME) != -1
        let nameWid = s:FullfeaturedUI_getMaxWidth(a:group, b.getName)
    endif
    " path
    let pathWid = 0
    if stridx(g:TBE_bufferLineFormat, s:TBE_FORMAT_PATH) != -1
        let pathWid = s:FullfeaturedUI_getMaxWidth(a:group, b.getPath)
    endif
    " dir
    let dirWid = 0
    if stridx(g:TBE_bufferLineFormat, s:TBE_FORMAT_DIR) != -1
        let dirWid = s:FullfeaturedUI_getMaxWidth(a:group, b.getDirectory)
    endif

    let result  = []
    for b in a:group.buffers
        " embed values
        let s = g:TBE_bufferLineFormat

        " number
        let number = repeat(' ', numberWid - strlen(string(b.number))) . string(b.number)
        let s = substitute(s, '\V' . s:TBE_FORMAT_NUMBER, number, 'g')
        " name
        let name = b.getName() . repeat(' ', nameWid - strlen(b.getName()))
        let s = substitute(s, '\V' . s:TBE_FORMAT_NAME, escape(name, '\'), 'g')
        " path
        let path = b.getPath() . repeat(' ', pathWid - strlen(b.getPath()))
        let s = substitute(s, '\V' . s:TBE_FORMAT_PATH, escape(path, '\'), 'g')
        " dir
        let dir = b.getDirectory() . repeat(' ', dirWid - strlen(b.getDirectory()))
        let s = substitute(s, '\V' . s:TBE_FORMAT_DIR, escape(dir, '\'), 'g')
        " current
        if b.isCurrent()
            let current = '%'
        elseif b.isAlternate()
            let current = '#'
        else
            let current = ' '
        endif
        let s = substitute(s, '\V' . s:TBE_FORMAT_CURRENT, current, 'g')
        " hidden
        if b.isHidden()
            let hidden = 'h'
        else
            let hidden = 'a'
        endif
        let s = substitute(s, '\V' . s:TBE_FORMAT_HIDDEN, hidden, 'g')
        " modified
        if b.isModified()
            let modified = '+'
        else
            let modified = ' '
        endif
        let s = substitute(s, '\V' . s:TBE_FORMAT_MODIFIED, modified, 'g')

        call add(result, s)
    endfor

    return result
endfunction

function! s:FullfeaturedUI_setKeymapping()
    " switching group
    noremap  <silent><buffer>  g        :call <SID>FullfeaturedUI_showGroupWindow()<CR>
    noremap  <silent><buffer>  G        :call <SID>FullfeaturedUI_showGroupWindow()<CR>
    noremap  <silent><buffer>  f        :call <SID>FullfeaturedUI_showGroupWindow()<CR>
    noremap  <silent><buffer>  F        :call <SID>FullfeaturedUI_showGroupWindow()<CR>
    noremap  <silent><buffer>  <TAB>    :call <SID>FullfeaturedUI_switchGroup(+1)<CR> 
    noremap  <silent><buffer>  l        :call <SID>FullfeaturedUI_switchGroup(+1)<CR> 
    noremap  <silent><buffer>  <RIGHT>  :call <SID>FullfeaturedUI_switchGroup(+1)<CR> 
    noremap  <silent><buffer>  <S-TAB>  :call <SID>FullfeaturedUI_switchGroup(-1)<CR>
    noremap  <silent><buffer>  h        :call <SID>FullfeaturedUI_switchGroup(-1)<CR>
    noremap  <silent><buffer>  <LEFT>   :call <SID>FullfeaturedUI_switchGroup(-1)<CR>

    noremap  <silent><buffer>  m        :call <SID>FullfeaturedUI_switchMRUGroup()<CR>
    noremap  <silent><buffer>  M        :call <SID>FullfeaturedUI_switchMRUGroup()<CR>
    noremap  <silent><buffer>  a        :call <SID>FullfeaturedUI_switchALLGroup()<CR>
    noremap  <silent><buffer>  A        :call <SID>FullfeaturedUI_switchALLGroup()<CR>
    noremap  <silent><buffer>  n        :call <SID>FullfeaturedUI_switchDirGroup()<CR>
    noremap  <silent><buffer>  N        :call <SID>FullfeaturedUI_switchDirGroup()<CR>

    " searching
    noremap  <silent><buffer>  /        :call <SID>FullfeaturedUI_render(<SID>UI_showSearchGroup())<CR>

    " jump
    noremap  <silent><buffer>  <CR>     :call <SID>FullfeaturedUI_jumpToBuffer()<CR>
    noremap  <silent><buffer>  <C-J>    :call <SID>FullfeaturedUI_jumpToBuffer()<CR>

    " exit
    noremap  <silent><buffer>  <ESC>    :call <SID>UI_closeBufferWindow()<CR>
    map      <silent><buffer>  q        <ESC>

    " delete buffer
    noremap  <silent><buffer>  d        :call <SID>FullfeaturedUI_deleteBuffer()<CR>
    map      <silent><buffer>  D        :call <SID>FullfeaturedUI_deleteBuffer()<CR>

    noremap  <silent><buffer>  r        :call <SID>TBE_init()<CR>:call g:TBE_useFullfeaturedUI()<CR>

    " do nothing
    noremap  <silent><buffer>  v        V
    noremap  <silent><buffer>  <C-v>    V
    vnoremap <silent><buffer>  <ESC>    <ESC>
endfunction

function! s:FullfeaturedUI_setGroupKeymapping()
    noremap  <silent><buffer>  <ESC>    :call <SID>UI_backtoMainWindow()<CR>
    noremap  <silent><buffer>  q        :call <SID>UI_backtoMainWindow()<CR>

    " jump
    noremap  <silent><buffer>  <CR>     :call <SID>FullfeaturedUI_jumpToGroup()<CR>
    noremap  <silent><buffer>  <C-J>    :call <SID>FullfeaturedUI_jumpToGroup()<CR>
    noremap  <silent><buffer>  g        :call <SID>FullfeaturedUI_jumpToGroup()<CR>
    noremap  <silent><buffer>  G        :call <SID>FullfeaturedUI_jumpToGroup()<CR>
    noremap  <silent><buffer>  f        :call <SID>FullfeaturedUI_jumpToGroup()<CR>
    noremap  <silent><buffer>  F        :call <SID>FullfeaturedUI_jumpToGroup()<CR>
endfunction

function! s:FullfeaturedUI_setGroupHighlight()

endfunction

function! s:FullfeaturedUI_setHighlight()
    " group
    syntax match Directory /\V\%1l\^\.\*\$/
    " group title
    syntax match TabLine   /\V\%1l\^\.\+  -/me=e-3 containedin=ALL contained
    " group tab key
    syntax match UnderLined   /\V\%1lTab=>/ containedin=ALL contained
endfunction

function! s:FullfeaturedUI_renderGroups()
    " build a view
    let header = '::Groups::  -' . string(len(s:TBE_groups)) . '-'
    let s = s:FullfeaturedUI_generateGroupLines()

    setlocal modifiable noreadonly lazyredraw
    let old_yank = @"
    let old_yank_star = @*
    %delete
    call setline(1, header)
    call setline(2, '')
    call setline(3, s)
    let @* = old_yank_star
    let @" = old_yank
    setlocal nomodifiable readonly lazyredraw

    " adjust height (+2 for group view and an empty line)
    call s:FullfeaturedUI_adjustWindowSize()

    " move to current group
    let gi = index(s:TBE_groups, s:TBE_lastGroup)
    if gi != -1
        execute (gi + 3)
    else
        execute 3
    endif
    normal 0
endfunction

function! s:FullfeaturedUI_adjustWindowSize()
    if g:TBE_showVertically
        return
    endif

    let height = 3
    let buffheight = len(getbufline(s:TBE_MAIN_BUFFER_NAME, 1, '$'))
    let groupheight  = len(getbufline(s:TBE_GROUP_BUFFER_NAME, 1, '$'))

    if buffheight < groupheight
        let height = groupheight
    else
        let height = buffheight
    endif

    execute 'resize ' . height
endfunction

function! s:FullfeaturedUI_generateGroupLines()
    " max width
    let g = s:Group_create('id', 'title', function('s:Group_defaultHandler'), function('s:Group_defaultHandler'), function('s:Group_defaultHandler'), function('s:Group_defaultHandler'))
    " name
    let nameWid = 0
    if stridx(g:TBE_groupLineFormat, s:TBE_FORMAT_NAME) != -1
        let nameWid = s:FullfeaturedUI_getGroupMaxWidth(g.getTitle)
    endif
    " path
    let pathWid = 0
    if stridx(g:TBE_groupLineFormat, s:TBE_FORMAT_PATH) != -1
        let pathWid = s:FullfeaturedUI_getGroupMaxWidth(g.getID)
    endif
    " count
    let countWid = 0
    if stridx(g:TBE_groupLineFormat, s:TBE_FORMAT_COUNT) != -1
        let countWid = s:FullfeaturedUI_getGroupMaxWidth(g.getBufferCount)
    endif

    let result = []
    for g in s:TBE_groups
        " embed values
        let s = g:TBE_groupLineFormat

        " name
        let name = g.getTitle() . repeat(' ', nameWid - strlen(g.getTitle()))
        let s = substitute(s, '\V' . s:TBE_FORMAT_NAME, escape(name, '\'), 'g')
        " path
        let path = g.getID() . repeat(' ', nameWid - strlen(g.getID()))
        let s = substitute(s, '\V' . s:TBE_FORMAT_PATH, escape(path, '\'), 'g')
        " count
        let bcount = repeat(' ', countWid - strlen(g.getBufferCount())) . g.getBufferCount()
        let s = substitute(s, '\V' . s:TBE_FORMAT_COUNT, escape(bcount, '\'), 'g')

        call add(result, s)
    endfor
    return result
endfunction

function! s:FullfeaturedUI_jumpToBuffer()
    let buffnum = s:FullfeaturedUI__getBufferNumberUnderCursor()

    if buffnum != 0
        call s:UI_jumpToBuffer(buffnum, 1)
    endif
endfunction

function! s:FullfeaturedUI_deleteBuffer() range
    if a:firstline < 3
        return
    endif

    let bb = []
    for bi in range(a:firstline, a:lastline)
        let b = s:TBE_lastGroup.buffers[bi - 3]
        cal add(bb, b.number)
    endfor

    if len(bb) != 0
        call s:UI_deleteBuffer(bb)
    endif

    call s:FullfeaturedUI_render(s:TBE_lastGroup)
endfunction

function! s:FullfeaturedUI__getBufferNumberUnderCursor()
    let l = line('.')
    if l < 3 | return 0 | endif
    return s:TBE_lastGroup.buffers[l - 3].number
endfunction

function! s:FullfeaturedUI_switchGroup(delta)
    let gi = s:TBE_findGroupIndexOf(s:TBE_lastGroup)
    if gi == -1
        return
    endif

    let gi = (gi + a:delta + len(s:TBE_groups)) % len(s:TBE_groups)
    let g = s:TBE_groups[gi]

    let s:TBE_lastGroup = g
    call s:FullfeaturedUI_render(g)
endfunction

function! s:FullfeaturedUI_switchMRUGroup()
    let g = s:TBE_findGroupByID('::MRU::')
    if g == {}
        return
    endif
    call s:FullfeaturedUI_render(g)
endfunction

function! s:FullfeaturedUI_switchALLGroup()
    let g = s:TBE_findGroupByID('::ALL::')
    if g == {}
        return
    endif
    call s:FullfeaturedUI_render(g)
endfunction

function! s:FullfeaturedUI_switchDirGroup()
    let g = s:TBE_findCurrentDirGroup()
    if g == {}
        return
    endif
    call s:FullfeaturedUI_render(g)
endfunction

function! s:FullfeaturedUI_getMaxWidth(group, prop)
    let maxwid = 0
    for b in a:group.buffers
        let w = strlen(call(a:prop, [], b))
        if maxwid < w | let maxwid = w | endif
    endfor
    return maxwid
endfunction

function! s:FullfeaturedUI_getGroupMaxWidth(prop)
    let maxwid = 0
    for g in s:TBE_groups
        let w = strlen(call(a:prop, [], g))
        if maxwid < w | let maxwid = w | endif
    endfor
    return maxwid
endfunction

"------------------------------
" SimpleGroup UI
"------------------------------

function! g:TBE_useSimpleGroupUI()
    call s:UI_memoCurrentBuffer()

    if g:TBE_showMRUFirst
        let currgrp = s:TBE_findGroupByID('::MRU::')
    else
        let currgrp = s:TBE_findCurrentDirGroup()
        if currgrp == {}
            let currgrp = s:TBE_findGroupByID('::ALL::')
        endif
    endif
    call s:UI_memoLastGroup(currgrp)

    if g:TBE_showVertically | let method = 'vertical new' | else | let method = 'new' | endif
    let b = s:Buffer_openTempWindow(s:TBE_MAIN_BUFFER_NAME, method, 1)
    setlocal wrap nocursorcolumn nocursorline
    call s:SimpleGroupUI_render(s:TBE_lastGroup)
    call s:SimpleGroupUI_setKeymapping()
    call s:SimpleGroupUI_setHighlight()
endfunction

function! s:SimpleGroupUI_render(group)
    let g = a:group
    if g == {}
        return
    endif

    call s:UI_memoLastGroup(g)

    " build a view
    let header = g.title . '  -' . len(g.buffers) . '-    ' . g.id
    let nextg = s:TBE_findNextGroup(g)
    if nextg != {}
        let header = header . '    Tab=>' . nextg.title
    endif

    let s = ''
    for b in g.buffers
        if strlen(s) != 0
            let s = s . ' '
        endif

        let s = s . '[' . b.number . ':' 
        if b.isModified() | let s = s . '(+)' | endif
        let s = s . b.getName() . ']'
    endfor

    setlocal modifiable noreadonly lazyredraw
    let old_yank = @"
    let old_yank_star = @*
    %delete
    call setline(1, header)
    call setline(2, s)
    let @* = old_yank_star
    let @" = old_yank
    setlocal nomodifiable readonly lazyredraw

    " adjust height (+1 for group view)
    if !g:TBE_showVertically
        let height = ((strlen(s) + winwidth(0) - 1) / winwidth(0) + 1)
        if height < 2
            let height = 2
        endif
        execute 'resize ' . height
    endif

    " move to current buffer
    let old_slash = @/
    call setpos('.', [bufnr('%'), 2, 1, 0])
    call search('\V\<' . s:Buffer_currentBufferNumber . ':')
    call search('\V:')
    let @/ = old_slash
endfunction

function! s:SimpleGroupUI_setKeymapping()
    "noremap  <silent><buffer>  l        f[:call search(':')<CR>
    noremap  <silent><buffer>  l        :call search('\V\%>1l:')<CR>
    map      <silent><buffer>  <RIGHT>  l
    "noremap  <silent><buffer>  h        F]F[:call search(':')<CR>
    noremap  <silent><buffer>  h        :call search('\V\%>1l:', 'b')<CR>
    map      <silent><buffer>  <LEFT>   h
    noremap  <silent><buffer>  j        gj:call search('\V\%>1l:', 'b')<CR>
    map      <silent><buffer>  <DOWN>   j
    noremap  <silent><buffer>  k        gk:call search('\V\%>1l:', 'b')<CR>
    map      <silent><buffer>  <UP>     k
    map      <silent><buffer>  w        l
    map      <silent><buffer>  e        l
    map      <silent><buffer>  b        h

    " switching group
    noremap  <silent><buffer>  g        :call <SID>SimpleGroupUI_selectGroup()<CR>
    noremap  <silent><buffer>  G        :call <SID>SimpleGroupUI_selectGroup()<CR>
    noremap  <silent><buffer>  <TAB>    :call <SID>SimpleGroupUI_switchGroup(+1)<CR> 
    noremap  <silent><buffer>  <S-TAB>  :call <SID>SimpleGroupUI_switchGroup(-1)<CR>
    noremap  <silent><buffer>  m        :call <SID>SimpleGroupUI_switchMRUGroup()<CR>
    noremap  <silent><buffer>  M        :call <SID>SimpleGroupUI_switchMRUGroup()<CR>
    noremap  <silent><buffer>  a        :call <SID>SimpleGroupUI_switchALLGroup()<CR>
    noremap  <silent><buffer>  A        :call <SID>SimpleGroupUI_switchALLGroup()<CR>
    noremap  <silent><buffer>  n        :call <SID>SimpleGroupUI_switchDirGroup()<CR>
    noremap  <silent><buffer>  N        :call <SID>SimpleGroupUI_switchDirGroup()<CR>

    " searching
    noremap  <silent><buffer>  /        :call <SID>SimpleGroupUI_render(<SID>UI_showSearchGroup())<CR>

    " jump
    noremap  <silent><buffer>  <CR>     :call <SID>SimpleGroupUI_jumpToBuffer()<CR>
    noremap  <silent><buffer>  <C-J>    :call <SID>SimpleGroupUI_jumpToBuffer()<CR>

    " exit
    noremap  <silent><buffer>  <ESC>    :call <SID>UI_closeBufferWindow()<CR>
    map      <silent><buffer>  q        <ESC>

    " delete buffer
    noremap  <silent><buffer>  d        :call <SID>SimpleGroupUI_deleteBuffer()<CR>
    map      <silent><buffer>  D        :call <SID>SimpleGroupUI_deleteBuffer()<CR>

    noremap  <silent><buffer>  r        :call <SID>TBE_init()<CR>:call g:TBE_useSimpleGroupUI()<CR>

    " do nothing
    noremap  <silent><buffer>  v        <NOP>
    noremap  <silent><buffer>  V        <NOP>
    noremap  <silent><buffer>  <C-v>    <NOP>
endfunction

function! s:SimpleGroupUI_setHighlight()
    " group
    syntax match Directory /\V\%1l\^\.\*\$/
    " group title
    syntax match TabLine   /\V\%1l\^\.\+  -/me=e-3 containedin=ALL contained
    " group tab key
    syntax match UnderLined   /\V\%1lTab=>/ containedin=ALL contained
endfunction

function! s:SimpleGroupUI_selectGroup()
    let glist = map(copy(s:TBE_groups), 'v:val.title . " " . v:val.id')
    for gi in range(len(glist))
        let glist[gi] = gi . ':' . glist[gi]
    endfor

    let gi = inputlist(glist)
    if gi < 0 || gi >= len(glist) | return | endif

    call s:SimpleGroupUI_render(s:TBE_groups[gi])
endfunction

function! s:SimpleGroupUI_switchGroup(delta)
    let gi = s:TBE_findGroupIndexOf(s:TBE_lastGroup)
    if gi == -1
        return
    endif

    let gi = (gi + a:delta + len(s:TBE_groups)) % len(s:TBE_groups)
    let g = s:TBE_groups[gi]

    let s:TBE_lastGroup = g
    call s:SimpleGroupUI_render(g)
endfunction

function! s:SimpleGroupUI_switchMRUGroup()
    let g = s:TBE_findGroupByID('::MRU::')
    if g == {}
        return
    endif
    call s:SimpleGroupUI_render(g)
endfunction

function! s:SimpleGroupUI_switchALLGroup()
    let g = s:TBE_findGroupByID('::ALL::')
    if g == {}
        return
    endif
    call s:SimpleGroupUI_render(g)
endfunction

function! s:SimpleGroupUI_switchDirGroup()
    let g = s:TBE_findCurrentDirGroup()
    if g == {}
        return
    endif
    call s:SimpleGroupUI_render(g)
endfunction

function! s:SimpleGroupUI__getBufferNumberUnderCursor()
    let old_t = @t

    " selecte appropriate text
    normal f[
    normal F[
    normal "tyi[
    let selected = @t

    let @t = old_t

    let buffnum = substitute(selected, '\v(\d+).*', '\1', '')
    if strlen(buffnum) == 0
        let buffnum = 0
    endif

    return buffnum
endfunction

function! s:SimpleGroupUI_jumpToBuffer()
    let buffnum = s:SimpleGroupUI__getBufferNumberUnderCursor()

    if buffnum != 0
        call s:UI_jumpToBuffer(buffnum, 1)
    endif
endfunction

function! s:SimpleGroupUI_deleteBuffer()
    let buffnum = s:SimpleGroupUI__getBufferNumberUnderCursor()

    if buffnum != 0
        call s:UI_deleteBuffer([buffnum])
    endif

    call s:SimpleGroupUI_render(s:TBE_lastGroup)
endfunction


"------------------------------
" Minimal UI
"------------------------------

function! g:TBE_useMinimalUI()
    call s:UI_memoCurrentBuffer()

    if g:TBE_showVertically | let method = 'vertical new' | else | let method = 'new' | endif
    let b = s:Buffer_openTempWindow(s:TBE_MAIN_BUFFER_NAME, method, 1)
    setlocal wrap nocursorcolumn nocursorline
    call s:MinimalUI_render()
    call s:MinimalUI_setKeymapping()
endfunction

function! s:MinimalUI_render()
    let g = s:TBE_findGroupByID('::ALL::')
    if g == {}
        return
    endif

    " build a view
    let s = ''
    for b in g.buffers
        if strlen(s) != 0
            let s = s . ' '
        endif
        let s = s . '[' . b.number . ':' . b.getName() . ']'
    endfor

    setlocal modifiable noreadonly
    let old_yank = @"
    let old_yank_star = @*
    %delete
    call setline(1, s)
    let @* = old_yank_star
    let @" = old_yank
    setlocal nomodifiable readonly

    " adjust height
    if !g:TBE_showVertically
        execute 'resize ' . ((strlen(s) + winwidth(0) - 1) / winwidth(0))
    endif

    " move to current buffer
    let old_slash = @/
    call search('\V' . s:Buffer_currentBufferNumber . ':')
    call search('\V:')
    let @/ = old_slash
endfunction

function! s:MinimalUI_setKeymapping()
    "noremap  <silent><buffer>  l        f[:call search(':')<CR>
    noremap  <silent><buffer>  l        :call search(':')<CR>
    map      <silent><buffer>  <RIGHT>  l
    "noremap  <silent><buffer>  h        F]F[:call search(':')<CR>
    noremap  <silent><buffer>  h        :call search(':', 'b')<CR>
    map      <silent><buffer>  <LEFT>   h
    noremap  <silent><buffer>  j        gj:call search(':', 'b')<CR>
    map      <silent><buffer>  <DOWN>   j
    noremap  <silent><buffer>  k        gk:call search(':', 'b')<CR>
    map      <silent><buffer>  <UP>     k
    map      <silent><buffer>  w        l
    map      <silent><buffer>  e        l
    map      <silent><buffer>  b        h

    " jump
    noremap  <silent><buffer>  <CR>     :call <SID>MinimalUI_jumpToBuffer()<CR>
    noremap  <silent><buffer>  <C-J>    :call <SID>MinimalUI_jumpToBuffer()<CR>

    " exit
    noremap  <silent><buffer>  <ESC>    :call <SID>UI_closeBufferWindow()<CR>
    map      <silent><buffer>  q        <ESC>

    " delete buffer
    noremap  <silent><buffer>  d        :call <SID>MinimalUI_deleteBuffer()<CR>
    map      <silent><buffer>  D        :call <SID>MinimalUI_deleteBuffer()<CR>

    " do nothing
    noremap  <silent><buffer>  v        <NOP>
    noremap  <silent><buffer>  V        <NOP>
    noremap  <silent><buffer>  <C-v>    <NOP>
endfunction

function! s:MinimalUI__getBufferNumberUnderCursor()
    let old_t = @t

    " selecte appropriate text
    normal f[
    normal F[
    normal "tyi[
    let selected = @t

    let @t = old_t

    let buffnum = substitute(selected, '\v(\d+).*', '\1', '')
    if strlen(buffnum) == 0
        let buffnum = 0
    endif

    return buffnum
endfunction

function! s:MinimalUI_jumpToBuffer()
    let buffnum = s:MinimalUI__getBufferNumberUnderCursor()

    if buffnum != 0
        call s:UI_jumpToBuffer(buffnum, 1)
    endif
endfunction

function! s:MinimalUI_deleteBuffer()
    let buffnum = s:MinimalUI__getBufferNumberUnderCursor()

    if buffnum != 0
        call s:UI_deleteBuffer([buffnum])
    endif

    call s:MinimalUI_render()
endfunction

"------------------------------
" MRU group
"------------------------------

function! s:MRUGroup_install()
    let mruGroup = s:Group_create(
                \ '::MRU::',
                \ 'MRU buffers',
                \ function('s:MRUGroup_onBufAdd'), 
                \ function('s:Group_defaultHandler'), 
                \ function('s:MRUGroup_onBufAdd'), 
                \ function('s:Group_defaultHandler'), 
                \ )
    call s:TBE_addGroup(mruGroup)
endfunction

function! s:MRUGroup_onBufAdd(buffer) dict
    if s:TBE_bufferIsUntouchableForMRU(a:buffer)
        return
    endif

    let idx = index(self.buffers, a:buffer)
    if idx != -1
        call remove(self.buffers, idx)
    endif
    call insert(self.buffers, a:buffer, 0)

    if g:TBE_mruMax < len(self.buffers)
        call remove(self.buffers, g:TBE_mruMax, len(self.buffers) - 1)
    endif
endfunction


"------------------------------
" Search group
"------------------------------

function! s:SearchGroup_install(pattern)
    let searchGroup = s:Group_create(
                \ '::Search::',
                \ '/' . a:pattern . '/',
                \ function('s:SearchGroup_onBufAdd'), 
                \ function('s:SearchGroup_onBufDelete'), 
                \ function('s:SearchGroup_onBufAdd'), 
                \ function('s:SearchGroup_onBufAdd')
                \ )
    call s:TBE_addGroup(searchGroup)
    call searchGroup.collectBuffers()
endfunction

function! s:SearchGroup_onBufAdd(buffer) dict
    if s:TBE_bufferIsUntouchable(a:buffer) || index(self.buffers, a:buffer) != -1
        return
    endif

    let pattern = substitute(self.title, '\v\/(.+)\/', '\1', '')
    if a:buffer.getPath() =~? pattern
        call add(self.buffers, a:buffer)
    endif
endfunction

function! s:SearchGroup_onBufDelete(buffer) dict
    let idx = index(self.buffers, a:buffer)
    if idx != -1
        call remove(self.buffers, idx)
    endif
endfunction

"------------------------------
" Directory group
"------------------------------

function! s:DirectoryGroup_install(buffer)
    if s:TBE_findGroupByID(a:buffer.getDirectory()) == {}
        " create another directory group
        let dirgrp = s:Group_create(
                    \ '::Directory::',
                    \ 'Directory buffers',
                    \ function('s:DirectoryGroup_onBufAdd'), 
                    \ function('s:DirectoryGroup_onBufDelete'), 
                    \ function('s:Group_defaultHandler'), 
                    \ function('s:DirectoryGroup_onCollect')
                    \ )
        call s:DirectoryGroup__updateInfoWithBuffer(dirgrp, a:buffer.number)
        call s:TBE_addGroup(dirgrp)
        call dirgrp.onBufAdd(a:buffer)
    endif
endfunction

function! s:DirectoryGroup__updateInfoWithBuffer(group, bufferNumber)
    let a:group.id = s:Buffer_create(a:bufferNumber).getDirectory()
    let a:group.title = expand('#' . a:bufferNumber . ':p:h:t')
    if strlen(a:group.title) == 0
        let a:group.title = a:group.id
    endif
endfunction

function! s:DirectoryGroup_onBufAdd(buffer) dict
    if s:TBE_bufferIsUntouchable(a:buffer) || index(self.buffers, a:buffer) != -1
        return
    endif

    if self.id ==? a:buffer.getDirectory()
        call add(self.buffers, a:buffer)
    else
        call s:DirectoryGroup_install(a:buffer)
    endif
endfunction

function! s:DirectoryGroup_onBufDelete(buffer) dict
    let idx = index(self.buffers, a:buffer)
    if idx != -1
        call remove(self.buffers, idx)
        if len(self.buffers) == 0
            call s:TBE_removeGroup(self)
        endif
    endif
endfunction

function! s:DirectoryGroup_onCollect(buffer) dict
    call self.onBufAdd(a:buffer)
endfunction

"------------------------------
" ALL group
"------------------------------

function! s:ALLGroup_install()
    let allGroup = s:Group_create(
                \ '::ALL::',
                \ 'ALL buffers',
                \ function('s:ALLGroup_onBufAdd'), 
                \ function('s:ALLGroup_onBufDelete'), 
                \ function('s:Group_defaultHandler'), 
                \ function('s:ALLGroup_onCollect')
                \ )
    call s:TBE_addGroup(allGroup)
    call allGroup.collectBuffers()
endfunction

function! s:ALLGroup_onBufAdd(buffer) dict
    if s:TBE_bufferIsUntouchable(a:buffer) || index(self.buffers, a:buffer) != -1
        return
    endif

    call add(self.buffers, a:buffer)

    call s:DirectoryGroup_install(a:buffer)
endfunction

function! s:ALLGroup_onBufDelete(buffer) dict
    let idx = index(self.buffers, a:buffer)
    if idx != -1
        call remove(self.buffers, idx)
    endif
endfunction

function! s:ALLGroup_onCollect(buffer) dict
    call self.onBufDelete(a:buffer)
    call self.onBufAdd(a:buffer)
endfunction

"------------------------------
" TBE common routines
"------------------------------

augroup TBE
    autocmd!
    autocmd  BufAdd    * call <SID>TBE_onBufAdd(expand('<abuf>') + 0)
    autocmd  BufDelete * call <SID>TBE_onBufDelete(expand('<abuf>') + 0)
    autocmd  BufEnter  * call <SID>TBE_onBufEnter(expand('<abuf>') + 0)
augroup END


function! s:TBE_init()
    " a collection of all groups
    let s:TBE_groups = []

    call s:MRUGroup_install()
    call s:ALLGroup_install()
    call s:DirectoryGroup_install(s:Buffer_create(bufnr('%')))
    "call s:SearchGroup_install('vim')
endfunction

function! s:TBE_addGroup(group)
    if index(s:TBE_groups, a:group) == -1
        call add(s:TBE_groups, a:group)
    endif

    call sort(s:TBE_groups, function('s:TBE_compareGroup'))
endfunction

function! s:TBE_compareGroup(v1, v2)
    return a:v1.id ==? a:v2.id ? 0 : a:v1.id > a:v2.id ? 1 : -1
endfunction

function! s:TBE_removeGroup(group)
    let idx = index(s:TBE_groups, a:group)
    if idx != -1
        call remove(s:TBE_groups, idx)
    endif
endfunction

function! s:TBE_findGroupByID(groupID)
    for g in s:TBE_groups
        if g.id ==? a:groupID " ignore case
            return g
        endif
    endfor
    return {}
endfunction

function! s:TBE_findCurrentDirGroup()
    let currdir = expand('%:p:h')
    let currgrp = s:TBE_findGroupByID(currdir)
    return currgrp
endfunction

function! s:TBE_findGroupIndexOf(group)
    let gi = 0
    for g in s:TBE_groups
        if g == a:group
            return gi
        endif
        let gi = gi + 1
    endfor
    return -1
endfunction

function! s:TBE_findNextGroup(group)
    let gi = s:TBE_findGroupIndexOf(a:group)
    if gi == -1
        return {}
    endif
    let gi = (gi + 1) % len(s:TBE_groups)
    return s:TBE_groups[gi]
endfunction

function! s:TBE_bufferIsUntouchable(buffer)
    return
                \ 0
                \ || (bufname(a:buffer.number) == s:TBE_MAIN_BUFFER_NAME)
                \ || (bufname(a:buffer.number) == s:TBE_GROUP_BUFFER_NAME)
                \ || !buflisted(a:buffer.number)
                \ || (getbufvar(a:buffer.number, '&filetype') == 'help')
endfunction

function! s:TBE_bufferIsUntouchableForMRU(buffer)
    return
                \ 0
                \ || (bufname(a:buffer.number) == s:TBE_MAIN_BUFFER_NAME)
                \ || (bufname(a:buffer.number) == s:TBE_GROUP_BUFFER_NAME)
                \ || (getbufvar(a:buffer.number, '&filetype') == 'help')
endfunction

function! s:TBE_onBufAdd(bufferNumber)
    for g in s:TBE_groups
        call g.onBufAdd(s:Buffer_create(a:bufferNumber))
    endfor
endfunction

function! s:TBE_onBufDelete(bufferNumber)
    for g in s:TBE_groups
        call g.onBufDelete(s:Buffer_create(a:bufferNumber))
    endfor
endfunction

function! s:TBE_onBufEnter(bufferNumber)
    for g in s:TBE_groups
        call g.onBufEnter(s:Buffer_create(a:bufferNumber))
    endfor
endfunction


"------------------------------
" Group
"------------------------------

function! s:Group_create(id, title, onBufAdd, onBufDelete, onBufEnter, onCollect)
    return {
        \   'id'            : a:id
        \ , 'title'         : a:title
        \ , 'buffers'       : []
        \ , 'collectBuffers': function('s:Group__collectBuffers')
        \ , 'onBufAdd'      : a:onBufAdd
        \ , 'onBufDelete'   : a:onBufDelete
        \ , 'onBufEnter'    : a:onBufEnter
        \ , 'onCollect'     : a:onCollect
        \ , 'getID'         : function('s:Group__getID')
        \ , 'getTitle'      : function('s:Group__getTitle')
        \ , 'getBufferCount': function('s:Group__getBufferCount')
        \ }
endfunction

function! s:Group_defaultHandler(buffer) dict
    " does nothing
endfunction

function! s:Group__collectBuffers() dict
    for b in range(1, bufnr('$'))
        call self.onCollect(s:Buffer_create(b))
    endfor
endfunction


if 0 " sample
    function! s:MyBufAdd(buffer) dict
        echom 'MyBufAdd(' . a:buffer.number . ')'    
    endfunction
    function! s:MyBufDelete(buffer) dict
        echom 'MyBufDelete(' . a:buffer.number . ')' 
    endfunction
    function! s:MyBufEnter(buffer) dict
        echom 'MyBufEnter(' . a:buffer.number . ')'  
    endfunction
    function! s:MyCollect(buffer) dict
        echom 'MyCollect(' . a:buffer.number . ')'   
        if !s:TBE_bufferIsUntouchable(a:buffer)
            call add(self.buffers, a:buffer)
        endif
    endfunction
    unlet s:groupTest
    let s:groupTest = s:Group_create(
                \ 'groupTestID',
                \ 'groupTestTitle', 
                \ function('s:MyBufAdd'), 
                \ function('s:MyBufDelete'), 
                \ function('s:MyBufEnter'), 
                \ function('s:MyCollect'))
    call s:groupTest.collectBuffers()
    "for b in s:groupTest.buffers
    "    echom b.getName()
    "endfor
    call s:TBE_addGroup(s:groupTest)
endif

function! s:Group__getID() dict
    return self.id
endfunction

function! s:Group__getTitle() dict
    return self.title
endfunction

function! s:Group__getBufferCount() dict
    return len(self.buffers)
endfunction


"------------------------------
" Buffer
"------------------------------

function! s:Buffer_create(number)
    return {
                \   'number'        : a:number
                \ , 'getNumber'     : function('s:Buffer__getNumber')
                \ , 'getName'       : function('s:Buffer__getName')
                \ , 'getDirectory'  : function('s:Buffer__getDirectory')
                \ , 'getPath'       : function('s:Buffer__getPath')
                \ , 'isModified'    : function('s:Buffer__isModified')
                \ , 'isHidden'      : function('s:Buffer__isHidden')
                \ , 'isCurrent'     : function('s:Buffer__isCurrent')
                \ , 'isAlternate'   : function('s:Buffer__isAlternate')
                \ , 'open'          : function('s:Buffer_open')
                \ }
endfunction

function! s:Buffer_createWithName(name)
    return s:Buffer_create(bufnr(a:name, 1))
endfunction

function! s:Buffer_open(method, jump, options) dict
    " buffer already exists?
    let winnum = bufwinnr(self.number)
    if winnum != -1 " exists
        if a:jump
            execute 'keepalt ' . winnum . 'wincmd w'
        endif
        silent execute 'setlocal ' . a:options
    else
        let currbuff = s:Buffer_createWithName(bufname('%'))
        let currbufname = bufname('%')

        let l:method = a:method
        if l:method == ''
            let l:method = 'new'
        endif
        execute 'keepalt ' . l:method . ' #' . self.number
        silent execute 'setlocal ' . a:options

        if !a:jump
            call currbuff.open('', 1, '')
        endif
    endif
endfunction

function! s:Buffer_openWindow(bufferName, method, jump, options)
    " buffer already exists?
    let winnum = bufwinnr(a:bufferName)
    if winnum != -1 " exists
        if a:jump
            execute 'keepalt ' . winnum . 'wincmd w'
        endif
        silent execute 'setlocal ' . a:options
        return s:Buffer_create(bufnr(a:bufferName))
    else
        let currbufname = bufname('%')

        let l:method = a:method
        if l:method == ''
            let l:method = 'new'
        endif
        execute 'keepalt ' . l:method . ' ' . a:bufferName
        silent execute 'setlocal ' . a:options

        if !a:jump
            call s:Buffer_openWindow(currbufname, '', 1, '')
        endif
    endif
endfunction

" a shortcut
function! s:Buffer_openTempWindow(bufferName, method, jump)
    let b = s:Buffer_openWindow(
                \ a:bufferName,
                \ a:method,
                \ a:jump,
                \ 'nonumber buftype=nofile bufhidden=delete noswapfile')
endfunction

function! s:Buffer__getNumber() dict
    return self.number
endfunction

function! s:Buffer__getName() dict
    return expand('#' . self.number . ':p:t')
endfunction

function! s:Buffer__getDirectory() dict
    return expand('#' . self.number . ':p:h')
endfunction

function! s:Buffer__getPath() dict
    return expand('#' . self.number . ':p')
endfunction

function! s:Buffer__isModified() dict
    return getbufvar(self.number, '&modified')
endfunction

function! s:Buffer__isHidden() dict
    let result = 1
    for i in range(tabpagenr('$'))
        if index(tabpagebuflist(i + 1), self.number) != -1
            let result = 0
            break
        endif
    endfor
    return result
endfunction

function! s:Buffer__isCurrent() dict
    return (self.number == s:Buffer_currentBufferNumber)
endfunction

function! s:Buffer__isAlternate() dict
    return (self.number == s:Buffer_alternateBufferNumber)
endfunction


if 0
    let s:Buffer_currentBufferNumber = bufnr('%')
    let s:Buffer_alternateBufferNumber = bufnr('#')
    let s:b = s:Buffer_create(2)
    echo s:b.getName()
    echo s:b.isModified()
    echo s:b.isCurrent()
    echo s:b.isHidden()
    let s:b = s:Buffer_create(5)
    echo s:b.getName()
    echo s:b.isModified()
    echo s:b.isCurrent()
    echo s:b.isHidden()
    let s:b = s:Buffer_create(6)
    echo s:b.getName()
    echo s:b.isModified()
    echo s:b.isCurrent()
    echo s:b.isHidden()
endif


call s:TBE_init()

" vim: set et ft=vim sts=4 sw=4 ts=4 : 
