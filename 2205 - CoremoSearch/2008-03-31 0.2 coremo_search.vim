" CoremoSearch -- A simple simultaneous search script.
"
" Maintainer: Shuhei Kubota <kubota.shuhei@gmail.com>
" Description:
"   This script provides simultaneous search functionality.
"
"   You can add a string that you want to search and remove a string.
"   Color highlighting is available setting :set[l] nohlsearch.
"
" Usage:
"   A. Adding a word (like asterisk(*) keystroke)
"       1. Place the cursor on the word that you want to search.
"       2. Press <C-@> or :CoremoSearchAdd
"
"   B. Adding a selected string
"       1. In visual mode, select the string that you want to search.
"       2. Press <C-@> or :CoremoSearchAddV
"
"   C. Adding words with :CoremoSearchAdd command
"       1. :CoremoSearchAdd regexp1 regexp2 ...
"
"   A'. Removing a word
"       1. Place the cursor on the word.
"       2. Press <Learder><C-@> or :CoremoSearchRemove
"           (in most cases, <Leader> equals to backslash(\) keystroke)
"
"   B'. Removing a selected string
"       1. In visual mode, select the string.
"       2. Press <Learder><C-@> or :CoremoSearchRemoveV
"           (in most cases, <Leader> equals to backslash(\) keystroke)
"
" Last Change: 30-Mar-2008

command!  -range -nargs=*  CoremoSearchAdd      call <SID>CoremoSearch_add(<f-args>)
command!  -range  CoremoSearchAddV     call <SID>CoremoSearch_addV()
command!  -range  CoremoSearchRemove   call <SID>CoremoSearch_remove()
command!  -range  CoremoSearchRemoveV  call <SID>CoremoSearch_removeV()

nnoremap  <C-@>          :CoremoSearchAdd<CR>
nnoremap  <Leader>/      :CoremoSearchAdd 
vnoremap  <C-@>          :CoremoSearchAddV<CR>
vnoremap  <Leader>/      :CoremoSearchAdd 
nnoremap  <Leader><C-@>  :CoremoSearchRemove<CR>
vnoremap  <Leader><C-@>  :CoremoSearchRemoveV<CR>

if !exists('g:CoremoSearch_colors')
    let g:CoremoSearch_colors = [
        \ {'bg': 'darkred',     'fg': 'white'},
        \ {'bg': 'darkgreen',   'fg': 'white'},
        \ {'bg': 'brown',       'fg': 'white'},
        \ {'bg': 'darkblue',    'fg': 'white'},
        \ {'bg': 'darkmagenta', 'fg': 'white'},
        \ {'bg': 'darkcyan',    'fg': 'white'},
        \ ]
endif


function! s:CoremoSearch_add(...)
    if len(a:000) == 0
        let words = [s:CoremoSearch__escape(s:CoremoSearch__getWordUnderCursor())]
    else
        let words = a:000
    endif

    echo 'Coremo Search: ' . join(words, ', ')
    call s:CoremoSearch__addInner(words)
endfunction

function! s:CoremoSearch_addV()
    let word = s:CoremoSearch__getSelectedWord()

    echo 'Coremo Search: ' . word
    call s:CoremoSearch__addInner([s:CoremoSearch__escape(word)])
endfunction

function! s:CoremoSearch_remove()
    let word = s:CoremoSearch__getWordUnderCursor()

    let forgotten = s:CoremoSearch__removeInner(s:CoremoSearch__escape(word))
    if strlen(forgotten) != 0
        echo 'Forgot: ' . forgotten
    else
        echo ''
    endif
endfunction

function! s:CoremoSearch_removeV()
    let word = s:CoremoSearch__getSelectedWord()

    let forgotten = s:CoremoSearch__removeInner(s:CoremoSearch__escape(word))
    if strlen(forgotten) != 0
        echo 'Forgot: ' . forgotten
    else
        echo ''
    endif
endfunction

function! s:CoremoSearch__getSelectedWord()
    let old_a = @a

    "execute "normal \<ESC>"
    normal gv"ay
    let result = @a

    let @a = old_a

    return result
endfunction

function! s:CoremoSearch__getWordUnderCursor()
    let old_a = @a

    "execute "normal \<ESC>"
    if stridx(" \t　\r\n", getline('.')[col('.') - 1]) != -1
        execute "normal vaw\<ESC>"
    endif
    normal viw"ay
    let result = @a

    let @a = old_a

    return result
endfunction

function! s:CoremoSearch__addInner(exprs)
    let all = s:CoremoSearch__splitRegexpr(@/)
    for e in a:exprs
        let idx = max([index(all, e), index(all, '\<' . e . '\>')])
        if idx == -1
            call add(all, e)
        endif
    endfor
    let @/ = join(all, '\|')
    if ! &hlsearch | call s:CoremoSearch__refreshHightlights(all) | endif
endfunction

function! s:CoremoSearch__removeInner(expr)
    let all = s:CoremoSearch__splitRegexpr(@/)
    let idx = max([index(all, a:expr), index(all, '\<' . a:expr . '\>')])
    let removed = ''
    if idx != -1
        call remove(all, idx)
        let removed = a:expr
    else " try to find other regexps
        let candidates = filter(copy(all), "'".a:expr."' =~ v:val")
        if len(candidates) != 0
            let selectList = []
            for i in range(len(candidates))
                call add(selectList, (i+1).': '.candidates[i])
            endfor
            call insert(selectList, 'Which regexp do you want to remove?', 0)

            let choise = inputlist(selectList) - 1

            if -1 < choise && choise < len(candidates)
                call remove(all, index(all, candidates[choise]))
                let removed = candidates[choise]
            endif
        endif
    endif
    let @/ = join(all, '\|')
    if ! &hlsearch | call s:CoremoSearch__refreshHightlights(all) | endif
    return removed
endfunction

function! s:CoremoSearch__escape(expr)
    return escape(a:expr, '\$.*/[]^')
endfunction

function! s:CoremoSearch__splitRegexpr(expr)
    let all = split(a:expr, '\\\@<!\\|') " split by '\|' NOT next to '\'
    let result = []
    let word = ''

    for i in range(len(all))
        if strlen(word) != 0 | let word .= '\|' | endif
        let word .= all[i]

        " search /\(/ and /\)/
        let opening = len(split(' '.word.' ', '\\(')) - 1
        let closing = len(split(' '.word.' ', '\\)')) - 1

        if opening == closing
            call add(result, word)
            let word = ''
        endif
    endfor
    if strlen(word) != 0
        call add(result, word)
    endif

    return result
endfunction

function! s:CoremoSearch__initHighlights()
    for i in range(len(g:CoremoSearch_colors))
        let cs = g:CoremoSearch_colors[i]

        silent! execute 'syntax clear CoremoSearch'.i
        execute 'highlight  CoremoSearch'.i.
            \ ' ctermfg='.cs.fg.
            \ ' ctermbg='.cs.bg.
            \ ' guifg='.cs.fg.
            \ ' guibg='.cs.bg
    endfor
endfunction

function! s:CoremoSearch__refreshHightlights(words)
    call s:CoremoSearch__initHighlights()
    let colorsCount = len(g:CoremoSearch_colors)
    for i in range(len(a:words))
        execute 'syntax match CoremoSearch'.(i%colorsCount)." '".a:words[i]."' containedin=ALL"
    endfor
endfunction

" vim: set et ff=unix fileencoding=utf-8 sts=4 sw=4 ts=4 : 
