" tmru.vim -- Most Recently Used Files
" @Author:      Thomas Link (mailto:samul AT web de?subject=vim-tlib-mru)
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-04-13.
" @Last Change: 2007-05-12.
" @Revision:    0.2.106

if &cp || exists("loaded_tmru")
    finish
endif
if !exists('loaded_tlib') || loaded_tlib < 4
    echoerr "tlib >= 0.4 is required"
    finish
endif
let loaded_tmru = 2

if !exists("g:tmruSize")    | let g:tmruSize = 50        | endif "{{{2
if !exists("g:TMRU")        | let g:TMRU = ''            | endif "{{{2
if !exists("g:tmruExclude") "{{{2
    let g:tmruExclude = '/te\?mp/\|vim.\{-}/doc\|'.
                \ substitute(escape(&suffixes, '~.*$^'), ',', '$\\|', 'g') .'$'
endif
if !exists("g:tmru_ignorecase") "{{{2
    let g:tmru_ignorecase = !has('fname_case')
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
    let imru = index(tmru, a:fname, 0, g:tmru_ignorecase)
    if imru == -1 && len(tmru) >= g:tmruSize
        let imru = g:tmruSize - 1
    endif
    if imru != -1
        call remove(tmru, imru)
    endif
    call insert(tmru, a:fname)
    call s:MruStore(tmru)
endf

fun! s:SNR()
    return matchstr(expand('<sfile>'), '<SNR>\d\+_\zeSNR$')
endf

fun! s:AgentCopy(world, selected)
    let @* = join(a:selected, "\n")
    let a:world.state = 'redisplay'
    return a:world
endf

fun! s:SelectMRU()
    let tmru = s:MruRetrieve()
    let bs   = tlib#InputList('m', 'Select file', copy(tmru), [
                \ {'key': 3, 'agent': s:SNR() .'AgentCopy', 'key_name': '<c-c>', 'help': 'Copy file name(s)'},
                \ {'display_format': 'filename'},
                \ ])
    " TLogVAR bs
    if !empty(bs)
        for bf in bs
            if bf != expand('%:p')
                let bn = bufnr(bf)
                if bn != -1 && buflisted(bn)
                    exec 'buffer '. bn
                elseif filereadable(bf)
                    exec 'edit '. escape(bf, '%#\ ')
                else
                    let bi = index(tmru, bf)
                    call remove(tmru, bi)
                    call s:MruStore(tmru)
                endif
            endif
        endfor
        return 1
    endif
    return 0
endf

fun! s:EditMRU()
    let tmru = s:MruRetrieve()
    let tmru = tlib#EditList('Edit MRU', tmru)
    call s:MruStore(tmru)
endf

augroup tmru
    au!
    au BufWritePost * if &buftype !~ 'nofile' && expand("<afile>:t") != '' | call s:MruRegister(expand("<afile>:p")) | endif
    au BufReadPost *  if &buftype !~ 'nofile' && expand("<afile>:t") != '' | call s:MruRegister(expand("<afile>:p")) | endif
augroup END

command! TRecentlyUsedFiles call s:SelectMRU()
command! TRecentlyUsedFilesEdit call s:EditMRU()


finish

This plugin provides a simple most recently files facility. It was 
originally rather a by-product of tlib (vimscript #1863).

:TRecentlyUsedFiles ... open one or more recently used file(s)

This plugin relies on the 'viminfo' option to contain ! in order 
to save the files list.


CHANGES:
0.1
Initial release

0.2
- :TRecentlyUsedFilesEdit
- Don't register nofile buffers or buffers with no filename.
- <c-c> copy file name(s) (to @*)
- When !has('fname_case'), ignore case when checking if a filename is 
already registered.

