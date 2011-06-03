" tselectbuffer.vim -- A simplicistic buffer selector/switcher
" @Author:      Thomas Link (mailto:samul AT web de?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-04-15.
" @Last Change: 2007-04-18.
" @Revision:    0.1.62

if &cp || exists("loaded_tselectbuffer")
    finish
endif
if !exists('loaded_tlib')
    echoerr "tlib is required"
    finish
endif
let loaded_tselectbuffer = 1

fun! s:SNR()
    return matchstr(expand('<sfile>'), '<SNR>\d\+_\zeSNR$')
endf

if !exists('g:tselectbuffer_handlers')
    let g:tselectbuffer_handlers = [
                \ {'key': 4,  'agent': s:SNR() .'AgentDeleteBuffer', 'key_name': 'CTRL-D', 'help': 'Delete buffer(s)'},
                \ ]
endif

fun! s:PrepareSelectBuffer()
    redir => bfs
    exec 'silent ls'. s:selectbuffer_bang
    redir END
    let s:selectbuffer_list = split(bfs, '\n')
    let s:selectbuffer_nr = map(copy(s:selectbuffer_list), 'matchstr(v:val, ''\s*\zs\d\+\ze'')')
    call map(s:selectbuffer_list, 'matchstr(v:val, ''\s*\d\+\zs.\{-}\ze\s\+line \d\+\s*$'')')
    call map(s:selectbuffer_list, 'matchstr(v:val, ''^[^"]\+''). printf("%-20s   %s", fnamemodify(matchstr(v:val, ''"\zs.\{-}\ze"$''), ":t"), fnamemodify(matchstr(v:val, ''"\zs.\{-}\ze"$''), ":h"))')
    return s:selectbuffer_list
endf

fun! s:GetBufNr(buffer)
    let bi = index(s:selectbuffer_list, a:buffer)
    let bx = s:selectbuffer_nr[bi]
    return 0 + bx
endf

fun! s:DeleteThisBuffer(buffer)
    let bx = s:GetBufNr(a:buffer)
    let doit = input('Delete buffer "'. bufname(bx) .'"? (y/N) ', s:delete_this_buffer_default)
    echo
    if doit ==? 'y'
        if doit ==# 'Y'
            let s:delete_this_buffer_default = 'y'
        endif
        exec 'bdelete '. bx
        echom 'Delete buffer '. bx .': '. a:buffer
        return 1
    endif
    return 0
endf

fun! s:AgentDeleteBuffer(world, selected)
    call tlib#CloseScratch(a:world)
    let s:delete_this_buffer_default = ''
    for buffer in a:selected
        call s:DeleteThisBuffer(buffer)
    endfor
    let a:world['state'] = 'reset'
    let a:world['base']  = s:PrepareSelectBuffer()
    return a:world
endf

fun! TSelectBuffer(bang)
    let s:selectbuffer_bang = a:bang
    let b = tlib#InputList('m', 'Select buffer', s:PrepareSelectBuffer(), g:tselectbuffer_handlers)
    " TLogDBG 'b='. b
    if !empty(b)
        let bi = s:GetBufNr(b[0])
        " TLogDBG 'bi='. bi
        if bi > 0
            exec 'buffer '. bi
        endif
    endif
endf
command! -bang TSelectBuffer call TSelectBuffer("<bang>")


finish

This plugin provides a simple buffer selector. It doesn't have all the 
features other buffer selectors have but can be nevertheless be useful 
for switching to a different buffer or for deleting buffers. It was 
originally rather a by-product of tlib (vimscript #1863).

:TSelectBuffer  ... select a buffer
:TSelectBuffer! ... show also unlisted buffers

Features:
    - switch to a buffer
    - delete one or more buffer(s)

