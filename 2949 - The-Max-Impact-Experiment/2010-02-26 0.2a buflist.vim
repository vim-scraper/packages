" THE PITHY CODE EXPERIMENT...
" By David Larson
"
" The following script creates a location list out of a buffer listing.
" Opening a window showing a list of buffers is a lot faster than waiting for
" the results of an :ls command to finish scrolling across the screen.
"
" The script is an experiment in minimal code-development. If you are looking
" for a large buffer script then look elsewhere. My goal here is to write a
" complete buffer script with as little code as possible.
"
" Why have this experiment? Well... There are some important advantages to keeping
" things short:
"  - There are fewer places for bugs to hide.
"  - The code runs considerably faster because it leverages more of the native
"    C code.
"  - It is easier to understand (usually).
"  - It is easier to fix bugs.
"  - A sufficient level of obfuscation adds to job security... and
"  - It saves $0.000000001 in hard-drive space. Though, this introduction
"    recovers those savings.
"
" Usage:
"    ,l        Opens a location list window containing the buffer list. The
"              cursor is placed on the current buffer.  You can change this
"              mapping by setting "buflist_open" in your vimrc file. E.g:
"
"              let buflist_open = "\b"
"
" In the buffer list window:
"  <enter>     Opens the current buffer, and closes the buffer list window.
"     q        Closes the buffer list window, without making a selection.
"     d        Deletes the buffer under the cursor. You can delete more than
"              one buffer at a time by first selecting them in visual mode.
"
" For those of you brave vim obfuscators who would like to slay the world's
" problems with pithy code, focus on:
" - adding important, useful, or cool features with just a few lines of code
"   (refining to the point of obfuscation is okay, but not much beyond it),
" - maximizing use of built-in commands and features,
" - using lists and dictionaries (this tends to shorten the script and
"   increase speed).
"
" Do not add code that:
" - provides little benefit and takes a lot of code,
" - we can easily live without
"
" Gray areas:
" - Semi-useful features that takes little code. Tipping point: if the script
"   is a pain without the feature, then add it.
function s:wipe() range
   exec "bw" join(map(getline(a:firstline, a:lastline), 'matchstr(v:val, ''\d\+'')'))
   call s:setline(a:firstline, repeat([""], a:lastline-a:firstline+1)) " Clear the entries in the window
endfunction
function s:bamer() abort
   redir => out | silent ls | redir END
   let buffers = split(out, '\n')

   let list = copy(buffers)
   " turn the buffer list into a location list...
   call map(list, '{"bufnr": matchstr(v:val, ''\d\+'')+0, "lnum": matchstr(v:val, ''line \zs\d\+\ze'')}')
   " Use setloclist vs lexpr because lexpr extends the location list history.
   call setloclist(0, list, 'r')

   " move the cursor to the line with the current buffer in the location list.
   exec "silent" match(list, "'bufnr': ".bufnr(""))+1 ."ll"

   let s:winrestcmds = winrestcmd()
   lopen
   call s:setup_window(buffers)
endfunction
function s:setup_window(list)
   call map(a:list, 'substitute(v:val, ''".\{4,}\(.\{25}\)"'', ''|...\1|'', "")') " shorten long paths
   call map(a:list, 'substitute(v:val, ''"'', "|", "g")') " Remove the silly quotes
   call s:setline(1, a:list)

   " I like to tuck away the location list window after selecting the buffer I
   " want.
   au WinLeave <buffer> close | exec s:winrestcmds
   nnoremap <silent> <buffer> q :do WinLeave<cr>
    noremap <silent> <buffer> d :call <SID>wipe()<cr>
endfunction
function s:setline(lnum, list)
   setl modifiable
   call setline(a:lnum, a:list)
   setl nomodifiable nomodified
endfunction

if !exists("buflist_open") | let buflist_open = ",l" | endif
exec "nmap <unique> <silent>" buflist_open ":call <SID>bamer()<CR>"

" vim:fdm=marker fmr=function\ ,endfunction
