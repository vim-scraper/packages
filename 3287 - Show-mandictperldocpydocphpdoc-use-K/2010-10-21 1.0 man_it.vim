"    Author:  fvw (vimtexhappy@gmail.com)
"             Show man page in vim
"   Version:  v00.1
"   Created:  2009-09-14
"   License:  Copyright (c) 2001-2009, fvw
"             See the GNU General Public License version 2 for more details.
"     Usage:  Put this file in your VIM plugin dir
"man_it.vim: {{{1

if exists('loaded_man_it') || &cp
    finish
endif
let loaded_ditc_it = 1

let s:wordHis = []
let s:wordHisIdx = -1

function! s:ManWinNew()
    silent! topleft new ManWindow
    setlocal bufhidden=delete
    setlocal buftype=nofile
    setlocal noswapfile
    silent! nnoremap <silent> <buffer> <C-O>  :call <SID>HisWinShow(-1)<CR>
    silent! nnoremap <silent> <buffer> <C-I>  :call <SID>HisWinShow(1)<CR>
endfun

function! s:HisWinShow(dis)
    let new = s:wordHisIdx + a:dis
    let new = max([new, 0])
    let new = min([new, len(s:wordHis) - 1])
    if new != -1 && new != s:wordHisIdx
        let s:wordHisIdx = new
        call s:WinShow(s:wordHis[s:wordHisIdx])
    endif
endfun

function! s:HisAdd(args)
    let s:wordHisIdx += 1
    if s:wordHisIdx <= len(s:wordHis) - 1
        call remove(s:wordHis, s:wordHisIdx,-1)
    endif
    call add(s:wordHis, a:args)
endfun

function! s:WinShow(args)
    let [tool, word, type] = a:args
    let oldWinNr = winnr()
    let manWinNr = bufwinnr("ManWindow")
    if manWinNr == -1
        call s:ManWinNew()
        let oldWinNr += 1
    elseif manWinNr != winnr()
        silent! exec manWinNr.'wincmd w'
    endif
    setlocal modifiable
    exec '1,$d'
    exec 'silent 0r !'.tool.' '.word
    setlocal nomodifiable
    exec '1'
    exec 'setlocal ft='.type
    silent! exec oldWinNr.'wincmd w'
endfun

function! s:GetWord()
    let word = substitute(expand('<cword>'), '\W', "", "g")
    return word
endfun

function! s:GetManSet()
    for set in ['g:ManTool', 's:defManTool']
        if exists(set) && type({set}) == type({})
                    \  && has_key({set}, &ft)
            let tool = get({set}[&ft], 0, 'man')
            let type = get({set}[&ft], 1, 'man')
            return [tool, type]
        endif
    endfor
    return ['man', 'man']
endfun

function! s:ManWhat(...)
    let word = a:0 > 0? a:1 : s:GetWord()
    if &ft == 'vim'
        exec 'help '.word
    else
        let [tool, type] = s:GetManSet()
        call s:WinShow([tool, word, type])
        call s:HisAdd([tool, word, type])
    endif
endfun

function! s:DictWhat(...)
    let word = a:0 > 0? a:1 : s:GetWord()
    let [tool, type] = ['dict', 'man']
    call s:WinShow([tool, word, type])
    call s:HisAdd([tool, word, type])
endfun

let s:defManTool = {}
let s:defManTool['php'] = ['phpdoc', 'manphp']
let s:defManTool['manphp'] = ['phpdoc', 'manphp']

let s:defManTool['perl'] =['perldoc -f', 'manperl']
let s:defManTool['manperl'] =['perldoc -f', 'manperl']

let s:defManTool['python'] =['pydoc', 'manpython']
let s:defManTool['manpython'] =['pydoc', 'manpython']

command! -nargs=? Man call <sid>ManWhat(<q-args>)
command! -nargs=? Dict call <sid>DictWhat(<q-args>)

silent noremap K :call <sid>ManWhat()<cr>
silent noremap Y :call <sid>DictWhat()<cr>
silent xnoremap K "zy:call <sid>ManWhat("<C-R>z")<cr>
silent xnoremap Y "zy:call <sid>DictWhat("<C-R>z")<cr>
