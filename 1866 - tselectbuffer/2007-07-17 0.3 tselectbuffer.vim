" tselectbuffer.vim -- A simplicistic buffer selector/switcher
" @Author:      Thomas Link (mailto:samul AT web de?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-04-15.
" @Last Change: 2007-07-01.
" @Revision:    0.3.177
" GetLatestVimScripts: 1866 1 :AutoInstall: tselectbuffer.vim

if &cp || exists("loaded_tselectbuffer")
    finish
endif
if !exists('loaded_tlib') || loaded_tlib < 9
    echoerr 'tlib >= 0.9 is required'
    finish
endif
let loaded_tselectbuffer = 3

function! s:SNR()
    return matchstr(expand('<sfile>'), '<SNR>\d\+_\zeSNR$')
endf

if !exists('g:tselectbuffer_autopick') | let g:tselectbuffer_autopick = 1 | endif
if !exists('g:tselectbuffer_handlers')
    let g:tselectbuffer_handlers = [
                \ {'key':  4, 'agent': s:SNR() .'AgentDeleteBuffer',  'key_name': '<c-d>', 'help': 'Delete buffer(s)'},
                \ {'key': 21, 'agent': s:SNR() .'AgentRenameBuffer',  'key_name': '<c-u>', 'help': 'Rename buffer(s)'},
                \ {'key': 19, 'agent': s:SNR() .'AgentSplitBuffer',   'key_name': '<c-s>', 'help': 'Show in split buffer'},
                \ {'key': 22, 'agent': s:SNR() .'AgentVSplitBuffer',  'key_name': '<c-v>', 'help': 'Show in vsplit buffer'},
                \ ]
                " \ {'key': 20, 'agent': s:SNR() .'AgentTabBuffer',     'key_name': '<c-t>', 'help': 'Show in tab'},
    if !g:tselectbuffer_autopick
        call add(g:tselectbuffer_handlers, {'pick_last_item': 0})
    endif
endif

function! s:PrepareSelectBuffer()
    redir => bfs
    exec 'silent ls'. s:selectbuffer_bang
    redir END
    let s:selectbuffer_list = split(bfs, '\n')
    let s:selectbuffer_nr = map(copy(s:selectbuffer_list), 'matchstr(v:val, ''\s*\zs\d\+\ze'')')
    call map(s:selectbuffer_list, 'matchstr(v:val, ''\s*\d\+\zs.\{-}\ze\s\+line \d\+\s*$'')')
    call map(s:selectbuffer_list, 'matchstr(v:val, ''^[^"]\+''). printf("%-20s   %s", fnamemodify(matchstr(v:val, ''"\zs.\{-}\ze"$''), ":t"), fnamemodify(matchstr(v:val, ''"\zs.\{-}\ze"$''), ":h"))')
    return s:selectbuffer_list
endf

function! s:GetBufNr(buffer)
    " TLogVAR a:buffer
    let bi = index(s:selectbuffer_list, a:buffer)
    " TLogVAR bi
    let bx = s:selectbuffer_nr[bi]
    " TLogVAR bx
    return 0 + bx
endf

function! s:RenameThisBuffer(buffer)
    let bx = s:GetBufNr(a:buffer)
    let on = bufname(bx)
    let nn = input('Rename buffer: ', on)
    if !empty(nn) && nn != on
        exec 'buffer '. bx
        if filereadable(on) && &buftype !~ '\<nofile\>'
            " if filewritable(nn)
                call rename(on, nn)
                echom 'Rename file: '. on .' -> '. nn
            " else
            "     echoerr 'File cannot be renamed: '. nn
            " endif
        endif
        exec 'file! '. escape(nn, ' %#')
        echom 'Rename buffer: '. on .' -> '. nn
        return 1
    endif
    return 0
endf

function! s:AgentRenameBuffer(world, selected)
    call a:world.CloseScratch()
    for buffer in a:selected
        call s:RenameThisBuffer(buffer)
    endfor
    let a:world.state = 'reset'
    let a:world.base  = s:PrepareSelectBuffer()
    return a:world
endf

function! s:DeleteThisBuffer(buffer)
    let bx = s:GetBufNr(a:buffer)
    let doit = input('Delete buffer "'. bufname(bx) .'"? (y/N) ', s:delete_this_buffer_default)
    echo
    if doit ==? 'y'
        if doit ==# 'Y'
            let s:delete_this_buffer_default = 'y'
        endif
        if bufloaded(bx)
            exec 'bdelete '. bx
            echom 'Delete buffer '. bx .': '. a:buffer
        else
            exec 'bwipeout '. bx
            echom 'Wipe out buffer '. bx .': '. a:buffer
        end
        return 1
    endif
    return 0
endf

function! s:AgentDeleteBuffer(world, selected)
    call a:world.CloseScratch()
    let s:delete_this_buffer_default = ''
    for buffer in a:selected
        call s:DeleteThisBuffer(buffer)
    endfor
    let a:world.state = 'reset'
    let a:world.base  = s:PrepareSelectBuffer()
    return a:world
endf

function! s:GetBufferNumbers(selected) "{{{3
    return map(copy(a:selected), 's:GetBufNr(v:val)')
endf

function! s:AgentSplitBuffer(world, selected)
    return tlib#agent#EditFileInSplit(a:world, s:GetBufferNumbers(a:selected))
    " call a:world.CloseScratch()
    " for b in a:selected
    "     " TLogVAR b
    "     if bufwinnr(b) == -1
    "         call s:SwitchToBuffer(b, 'sbuffer')
    "     endif
    "     redraw
    " endfor
    " call a:world.Reset()
    " let a:world.state = ''
    " return a:world
endf

function! s:AgentVSplitBuffer(world, selected)
    return tlib#agent#EditFileInVSplit(a:world, s:GetBufferNumbers(a:selected))
endf

function! s:AgentTabBuffer(world, selected)
    return tlib#agent#EditFileInTab(a:world, s:GetBufferNumbers(a:selected))
endf

" function! s:AgentTabBuffer(world, selected)
"     call a:world.CloseScratch()
"     for b in a:selected
"         " TLogVAR b
"         call s:SwitchToBuffer(b, 'tab sbuffer')
"         tabp
"     endfor
"     let a:world.list = []
"     return a:world
" endf

function! s:SwitchToBuffer(buffer, command)
    let bi = s:GetBufNr(a:buffer)
    " TLogVAR a:buffer
    " TLogVAR bi
    if bi > 0
        " TLogDBG a:command .' '. bi
        exec a:command .' '. bi
    endif
endf

function! TSelectBuffer(bang)
    let s:selectbuffer_bang = a:bang
    let b = tlib#input#List('m', 'Select buffer', s:PrepareSelectBuffer(), g:tselectbuffer_handlers)
    if !empty(b)
        " TLogVAR b
        call s:SwitchToBuffer(b[0], 'buffer')
    endif
endf
command! -bang TSelectBuffer call TSelectBuffer("<bang>")


finish

This plugin provides a simple buffer selector. It doesn't have all the 
features other buffer selectors have but can be nevertheless be useful 
for switching to a different buffer or for deleting buffers.

It was originally rather a by-product of tlib (vimscript #1863) and uses 
its tlib#input#List() function. This function allows quickly selecting a 
buffer by typing some part of the name (which will actually filter the 
list until only one item is left), the number, or by clicking with the 
mouse on the entry.

:TSelectBuffer  ... select a buffer
:TSelectBuffer! ... show also unlisted buffers

Features:
    - switch to a buffer
    - delete one or more buffer(s)


CHANGES:
0.1
Initial release

0.2
- Minor improvements

0.3
- <c-u>: Rename buffer (and file on disk)
- <c-v>: Show buffers in vertically split windows
- Require tlib 0.9
- "Delete buffer" will wipe-out unloaded buffers.

