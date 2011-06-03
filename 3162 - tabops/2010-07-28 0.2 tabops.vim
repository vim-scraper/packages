" tabops -- A collection of miscellaneous functionalities operating tabs.
"
" Description:
"
"   This script provides tab operation functionalities.
"   Going to some tab, moving some tab and sorting tabs.
"
" Maintainer: Shuhei Kubota <kubota.shuhei+vim@gmail.com>
"
" Usage:
"   1. (optional) set prefix if you want to change. (let g:Tabops_prefix)
"      default: <Tab>
"   2. (optional) let g:Tabops_use??? variables 0 if you want to disable some features.
"   3. start editing
"
" Feature:
"
"   * the default prefix is <Tab>
"
"   if g:Tabops_useGoto == 1 then ...
"       <C-Tab>  : goto the next tab.
"       <S-C-Tab>: goto the prev tab.
"
"       {prefix}1: goto the 1st tab.
"       {prefix}2: goto the 2nd tab.
"          :
"       {prefix}8: goto the 8th tab.
"       {prefix}9: goto the LAST tab.
"
"   if g:Tabops_useMove == 1 then ...
"       {prefix}l: swap the current tab with the right one.
"       {prefix}h: swap the cerrent tab with the left one.
"
"   :TabopsSortByPath
"       sort tabs comparing their paths.
"
"   :TabopsSortByBufnr
"       sort tabs comparing their internal numbers.
"
"   :TabopsSortByLastChange
"       sort tabs comparing their recently-changed timestamps.
"       the most recent tab comes to left.

if !exists('g:Tabops_prefix')
    let g:Tabops_prefix = '<Tab>'
endif

if !exists('g:Tabops_useGoto')
    let g:Tabops_useGoto = 1
endif

if !exists('g:Tabops_useMove')
    let g:Tabops_useMove = 1
endif

if !exists('g:Tabops__closedTabHistory')
    let g:Tabops__closedTabHistory = [] " [{'b': closed bufnr, 't': placed position}]
endif

augroup Tabops
    autocmd!

    "to setting key mappings
    autocmd  BufEnter  * call <SID>Tabops_onBufEnter()

    "to handle closing a tab
    autocmd  BufLeave  * call <SID>Tabops_onBufLeave()
    autocmd  TabLeave  * call <SID>Tabops_onTabLeave()
    autocmd  BufWinLeave  * call <SID>Tabops_onBufWinLeave()
augroup END
command!  TabopsSortByPath       :call <SID>Tabops_sortByPath()
command!  TabopsSortByBufnr      :call <SID>Tabops_sortByBufnr()
command!  TabopsSortByLastChange :call <SID>Tabops_sortByLastChange()
command!  TabopsReopenClosedTab  :call <SID>Tabops_reopenClosedTab()


function! s:Tabops_onBufEnter()
    if g:Tabops_useGoto
        call s:Tabops__enableCtrlGoto()
        call s:Tabops__enableNumberGoto()
    endif
    if g:Tabops_useMove
        call s:Tabops__enableMove()
    endif
endfunction

"if this is 1, do not record closing tabs.
let s:Tabops__reopening = 0

function! s:Tabops_onBufLeave()
    if s:Tabops__reopening | return | endif
    let s:Tabops__leavingBufferNumber = bufnr('%')
endfunction

function! s:Tabops_onTabLeave()
    if s:Tabops__reopening | return | endif
    let s:Tabops__leavingTabNumber = tabpagenr()
endfunction

function! s:Tabops_onBufWinLeave()
    if s:Tabops__reopening | return | endif
    call insert(g:Tabops__closedTabHistory, {'b': s:Tabops__leavingBufferNumber, 't': s:Tabops__leavingTabNumber}, 0)

    let g:Tabops__closedTabHistory = g:Tabops__closedTabHistory[:4]
endfunction


function! s:Tabops__enableCtrlGoto()
    noremap  <silent>  <C-Tab>   :tabnext<CR>
    noremap  <silent>  <S-C-Tab> :tabprevious<CR>
endfunction

function! s:Tabops__enableNumberGoto()
    execute 'noremap  <buffer><silent>  '.g:Tabops_prefix."1  :call \<SID>Tabops__goto(0)<CR>"
    execute 'noremap  <buffer><silent>  '.g:Tabops_prefix."2  :call \<SID>Tabops__goto(1)<CR>"
    execute 'noremap  <buffer><silent>  '.g:Tabops_prefix."3  :call \<SID>Tabops__goto(2)<CR>"
    execute 'noremap  <buffer><silent>  '.g:Tabops_prefix."4  :call \<SID>Tabops__goto(3)<CR>"
    execute 'noremap  <buffer><silent>  '.g:Tabops_prefix."5  :call \<SID>Tabops__goto(4)<CR>"
    execute 'noremap  <buffer><silent>  '.g:Tabops_prefix."6  :call \<SID>Tabops__goto(5)<CR>"
    execute 'noremap  <buffer><silent>  '.g:Tabops_prefix."7  :call \<SID>Tabops__goto(6)<CR>"
    execute 'noremap  <buffer><silent>  '.g:Tabops_prefix."8  :call \<SID>Tabops__goto(7)<CR>"
    execute 'noremap  <buffer><silent>  '.g:Tabops_prefix.'9  :tablast<CR>'
endfunction

function! s:Tabops__enableMove()
    execute 'noremap  <buffer><silent>  '.g:Tabops_prefix."l  :call \<SID>Tabops__swapNext()<CR>"
    execute 'noremap  <buffer><silent>  '.g:Tabops_prefix."h  :call \<SID>Tabops__swapPrev()<CR>"
endfunction


"
" sorting
"

function! s:Tabops_sortByPath()
    call s:Tabops__sortByHoge(function('s:Tabops_sortByPath__Evalfunc'), function('s:Tabops_sortByPath__Cmpfunc'))
endfunction

function! s:Tabops_sortByPath__Evalfunc(tabidx)
    return expand('#'.tabpagebuflist(a:tabidx)[0].':p')
endfunction

function! s:Tabops_sortByPath__Cmpfunc(v1, v2)
    return a:v1.value == a:v2.value ? 0 : a:v1.value > a:v2.value ? 1 : -1
endfunction


function! s:Tabops_sortByLastChange()
    call s:Tabops__sortByHoge(function('s:Tabops_sortByLastChange__Evalfunc'), function('s:Tabops_sortByLastChange__Cmpfunc'))
endfunction

function! s:Tabops_sortByLastChange__Evalfunc(tabidx)
    call s:Tabops__goto(a:tabidx - 1)
    "let currbufnr = bufnr('%')

    redir => ul_as_str
    silent undolist
    redir END

    let ul = split(ul_as_str, '\n')[1:]
    if len(ul) == 0
        return ''
    else
        let lastchange = matchstr(ul[-1], '\V\(\d\+:\d\+:\d\+\)')
        if lastchange !~ '\V\(\d\+:\d\+:\d\+\)'
            let seconds = 0 + matchstr(ul[-1], '\V\(\d\+\)', '\1', '')
            let lastchange = strftime('%H:%M:%S', localtime() - seconds)
        endif
        return lastchange
    endif
endfunction

function! s:Tabops_sortByLastChange__Cmpfunc(v1, v2)
    if a:v1.value == a:v2.value
        return 0
    elseif  a:v1.value == ''
        return 1
    elseif  a:v2.value == ''
        return -1
    else
        "undolist doesn't have DATE column...
        let now = strftime('%H:%M:%S', localtime())
        if a:v1.value > now && a:v2.value > now || a:v1.value < now && a:v2.value < now
            return a:v1.value < a:v2.value ? 1 : -1
        elseif a:v2.value > now
            return 1
        else
            return -1
        endif
    endif
endfunction


function! s:Tabops_sortByBufnr()
    call s:Tabops__sortByHoge(function('s:Tabops_sortByBufnr__Evalfunc'), function('s:Tabops_sortByBufnr__Cmpfunc'))
endfunction

function! s:Tabops_sortByBufnr__Evalfunc(tabidx)
    return tabpagebuflist(a:tabidx)[0]
endfunction

function! s:Tabops_sortByBufnr__Cmpfunc(v1, v2)
    return a:v1.value == a:v2.value ? 0 : a:v1.value > a:v2.value ? 1 : -1
endfunction


function! s:Tabops__sortByHoge(Evalfunc, Cmpfunc)
    let sortTable = []

    let currtabidx = tabpagenr()

    let ld = &lazyredraw
    let &lazyredraw = 1

    "fillin each values
    for i in range(1, tabpagenr('$'))
        let value = a:Evalfunc(i)
        call extend(sortTable, [{'src':i, 'value':value}])
        "echom i-1.'  src: '.i.', value: '.value
    endfor

    "sort
    call sort(sortTable, a:Cmpfunc)

    "where should i goto, after moving
    let desttabidx = 1
    for i in range(1, len(sortTable))
        let elem = sortTable[i - 1]
        if elem.src == currtabidx
            let desttabidx = i
        endif
    endfor

    "move
    for i in range(1, len(sortTable))
        let elem = sortTable[i - 1]
        if i != elem.src
            call s:Tabops__goto(elem.src - 1)
            execute 'tabmove ' . string(i - 1)
            for j in range(i + 1, len(sortTable))
                if elem.src >= sortTable[j - 1].src
                    let sortTable[j - 1].src = sortTable[j - 1].src + 1
                endif
            endfor
        endif
    endfor

    call s:Tabops__goto(desttabidx - 1)

    let &lazyredraw = ld
endfunction


"
" other operations
"

function! s:Tabops_reopenClosedTab()
    if len(g:Tabops__closedTabHistory) == 0 | return | endif

    let top = g:Tabops__closedTabHistory[0]
    let g:Tabops__closedTabHistory = g:Tabops__closedTabHistory[1:]

    let ld = &lazyredraw
    let &lazyredraw = 1

    let s:Tabops__reopening = 1

    silent tablast
    silent tabedit
    silent execute string(top.b).'b '
    for i in range(tabpagenr('$'))
        if i == top.t | continue | endif
        call s:Tabops__swapPrev()
    endfor

    let s:Tabops__reopening = 0

    let &lazyredraw = ld
endfunction


"
" utilities
"

function! s:Tabops__goto(tabindex)
    " return if a:tabindex is invalid
    if a:tabindex >= tabpagenr('$') | return | endif

    let ld = &lazyredraw
    let &lazyredraw = 1

    tabfirst
    let i = 0
    for i in range(1, a:tabindex)
        tabnext
    endfor

    let &lazyredraw = ld
endfunction


function! s:Tabops__swapNext()
    let lasttabidx = tabpagenr('$') - 1
    let tabidx = tabpagenr() - 1
    if tabidx == lasttabidx
        let desttabidx = 0
    else
        let desttabidx = tabidx + 1
    endif

    execute 'tabmove ' . string(desttabidx)
endfunction

function! s:Tabops__swapPrev()
    let lasttabidx = tabpagenr('$') - 1
    let tabidx = tabpagenr() - 1
    if tabidx == 0
        let desttabidx = lasttabidx
    else
        let desttabidx = tabidx - 1
    endif

    execute 'tabmove ' . string(desttabidx)
endfunction

" vim: set et ft=vim sts=4 sw=4 ts=4 tw=78 : 
