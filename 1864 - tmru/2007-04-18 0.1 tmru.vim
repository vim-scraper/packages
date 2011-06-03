" tmru.vim -- Most Recently Used Files
" @Author:      Thomas Link (mailto:samul AT web de?subject=vim-tlib-mru)
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-04-13.
" @Last Change: 2007-04-18.
" @Revision:    0.1.76

if &cp || exists("loaded_tmru")
    finish
endif
if !exists('loaded_tlib')
    echoerr "tlib is required"
    finish
endif
let loaded_tmru = 1

if !exists("g:tmruSize")    | let g:tmruSize = 50        | endif "{{{2
if !exists("g:TMRU")        | let g:TMRU = ''            | endif "{{{2
if !exists("g:tmruExclude") "{{{2
    let g:tmruExclude = '/te\?mp/\|vim.\{-}/doc\|'.
                \ substitute(escape(&suffixes, '~.*$^'), ',', '$\\|', 'g') .'$'
endif

fun! s:MruRetrieve()
    return split(g:TMRU, '\n')
endf

fun! s:MruStore(mru)
    let g:TMRU = join(a:mru, "\n")
endf

fun! s:MruRegister(fname)
    if g:tmruExclude != '' && a:fname =~ g:tmruExclude
        return
    endif
    let tmru = s:MruRetrieve()
    let imru = index(tmru, a:fname)
    if imru == -1 && len(tmru) >= g:tmruSize
        let imru = g:tmruSize - 1
    endif
    if imru != -1
        call remove(tmru, imru)
    endif
    call insert(tmru, a:fname)
    call s:MruStore(tmru)
endf

fun! s:SelectMRU()
    let tmru = s:MruRetrieve()
    let bmru = map(copy(tmru), 'printf("%-20s   %s", fnamemodify(v:val, ":t"), fnamemodify(v:val, ":h"))')
    let bs   = tlib#InputList('m', 'Select file', bmru)
    " TLogVAR bs
    if !empty(bs)
        for b in bs
            let bi = index(bmru, b)
            let bf = tmru[bi]
            if bf != expand('%:p')
                let bn = bufnr(bf)
                if bn != -1
                    exec 'buffer '. bn
                elseif filereadable(bf)
                    exec 'edit '. escape(bf, '%#\ ')
                else
                    call remove(tmru, bi)
                    call s:MruStore(tmru)
                endif
            endif
        endfor
        return 1
    endif
    return 0
endf

augroup tmru
    au!
    au BufWritePost * call s:MruRegister(expand("<afile>:p"))
    au BufReadPost *  call s:MruRegister(expand("<afile>:p"))
augroup END

command! TRecentlyUsedFiles call s:SelectMRU()


finish

This plugin provides a simple most recently files facility. It was 
originally rather a by-product of tlib (vimscript #1863).

:TRecentlyUsedFiles ... open one or more recently used file

This plugin relies on the 'viminfo' option to contain ! in order 
to save the files list.

