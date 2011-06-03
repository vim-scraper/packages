" THE MAX IMPACT EXPERIMENT...
" By David Larson
"
" The idea is to do as much as possible with as little code as possible.
" Focus on:
" - adding important (or cool) features with just a few lines of code
"   (refining to the point of obfuscation is okay),
" - maximizing use of built-in commands and features,
" - using lists and dictionaries (this tends to reduce the lines of code and
"   increase speed).
"
" Do not add code that:
" - provides little benefit and takes a lot of code,
" - we can live without
"
" Gray areas:
" - Semi-useful features that takes little code. Tipping point: if the script
"   is a pain without the feature, then add it.
function s:return()
   " I like to have the lwindow close automatically after I select a file...
   let lazy = &lz
   set lazyredraw
   exec "normal! \<cr>"
   call s:close()
   let &lz = lazy
endfunction
function s:close()
   exec "silent bd" s:locbuf
   " Vim has problems restoring windows in some layouts, so restore the
   " windows explicitly...
   exec s:winrestcmds
endfunction
function s:wipe() range
   let lines = getline(a:firstline, a:lastline)
   call map(lines, 'matchstr(v:val, ''\d\+'')')
   exec "bw" join(lines)

   call map(lines, '""')
   setl modifiable
   call setline(a:firstline, lines)
   setl nomodifiable nomodified
endfunction
function s:liquid_blam() abort
   redir => out
   silent ls
   redir END
   let buffers = split(out, '\n')

   let list = copy(buffers)
   " turn the buffer list into a location list...
   call map(list, '{"bufnr": matchstr(v:val, ''\d\+'')+0, "lnum": matchstr(v:val, ''line \zs\d\+\ze'')+0}')
   " Use setloclist vs lexpr because lexpr creates a _new_ location list every
   " time, extending the location list history.
   call setloclist(0, list, 'r')

   " try to move the cursor to the current buffer in the location list...
   exec "silent" match(list, "'bufnr': ".bufnr(""))+1 ."ll"

   let s:winrestcmds = winrestcmd()
   lopen
   call s:setup_window(buffers)
endfunction
function s:setup_window(list)
   let s:locbuf = bufnr("")
   " Remove the silly quotation marks
   call map(a:list, 'substitute(v:val, ''"'', "", "g")')
   " Shorten long paths
   call map(a:list, 'substitute(v:val, ''\S\+\(\S\{25}\)'', ''...\1'', "")')
   " Add a few '|'s to leverage qf highlighting
   call map(a:list, 'substitute(v:val, ''\(.\{9}\)\(.\+\)\sline'', ''\1|\2|line'', "")')

   nnoremap <silent> <buffer> <return> :call <SID>return()<cr>
   nnoremap <silent> <buffer> q        :call <SID>close()<cr>
   nnoremap <silent> <buffer> d        :call <SID>wipe()<cr>
   vnoremap <silent> <buffer> d        :call <SID>wipe()<cr>

   setl modifiable
   call setline(1, a:list)
   setl nomodifiable nomodified
endfunction

" Liquid BLAM?? Well... Blam, as in: yea! go for it! do it now! Liquid,
" well...  I spilled water on the keyboard while working on this...
nmap <silent> ,l :call <SID>liquid_blam()<CR>

" vim:ft=vim fdm=marker fmr=function\ ,endfunction
