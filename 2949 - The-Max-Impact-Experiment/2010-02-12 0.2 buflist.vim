" THE PITHY CODE EXPERIMENT...
" By David Larson
"
" The following script creates a location list out of a buffer listing.
" Opening a window showing a list of buffers is a lot faster than waiting for
" the results of an :ls command to finish scrolling across the screen.
"
" There really isn't much in terms of usage documentation. Why? For several
" reasons:
"  1.) The script is wicked-short. You can probably figure out how to use it
"      in a New-York second, just by skimming the code.
"  2.) The script is an experiment in code-development. It is written for
"      developers, not users, so I assume that you know how to read
"      vim-script. If you are looking for a large buffer script then look
"      elsewhere.  My goal here is to see how much can be done with as little
"      code as possible. Though, there is enough here to make the script useful.
"  3.) I'm lazy.
"
" Why have this experiment? Well... There are some important advantages to keeping
" things short:
"  - There are fewer places for bugs to hide.
"  - The code tends to run faster because there are fewer lines to execute.
"    Also, leveraging the underlying C code speeds things up dramatically.
"  - It is easier to understand (usually).
"  - It is easier to fix bugs.
"  - A sufficient level of obfuscation adds to job security... and
"  - It saves $0.000000001 in hard-drive space. Though, this introduction
"    recovers those savings.
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
function s:baemer() abort
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
   call map(a:list, 'substitute(v:val, ''".\{4,}\(.\{25}\)"'', ''|...\1|'', "")')
   call map(a:list, 'substitute(v:val, ''"'', "|", "g")') " Remove the silly quotes
   call s:setline(1, a:list)

   au BufLeave <buffer> close | exec s:winrestcmds
   nnoremap <silent> <buffer> q :do BufLeave<cr>
    noremap <silent> <buffer> d :call <SID>wipe()<cr>
endfunction
function s:setline(lnum, list)
   setl modifiable
   call setline(a:lnum, a:list)
   setl nomodifiable nomodified
endfunction

nmap <unique> <silent> ,l :call <SID>baemer()<CR>

" vim:fdm=marker fmr=function\ ,endfunction
