" File:         HelpWords.vim
" Author:       Yakov Lerner <iler.ml@gmail.com>
" Last changed: 2006-12-21
"
" This script searches vimhelp for paragraphs containing 
" given words (substrings), in any order, and non-adjacent.
" Scripts defines commandHELPWORDS and it's synonym, HW.
"              Usage: HW word1 word2 ...
" This command will search vimhelp for paragraphs
" containing all given words, in any order (and non-adjacent).
" To proceed to the next match, us :cn command.
"
" Example: HW normal count
" will find all paragraphs that talk about normal-mode counters.
"
" Revision history:
" 1.0: First revision
"

if exists("g:helpwords_plugin") | finish | endif
let g:helpwords_plugin = 1


" helpwords: search help for given words in same paragraph, in any
" order, case-insensitive
command! -nargs=* HELPWORDS :call HelpWords(<f-args>)
command! -nargs=* HW        :call HelpWords(<f-args>)
fun! HelpWords(...)

" /\c\(.\|.\n\)*w1\&\(.\|.\n\)*w2\&\(.\|.\n\)*w3
" -- searched words (w1,w2,w3) in any order within same paragraph
   if  a:0 == 0
      echo "Missing arguments"
      return
   elsif a:0 == 1
      let re=a:1
   else
       let re='\c'
       let k = 0
       while k < a:0
            let re = re . '\(.\|.\n\)*' . a:000[k]
            if k != a:0 - 1
                let re = re . '\&'
            endif
            let k=k+1
       endw
   endif
   " let @/ = re
   redraw

   " regexp of :helpgrep does not modify @/
   " if we want to debug HELPWORDS, we want o leave
   " regexp used in global var
   let g:helpwords_re=re
   let g:helpwords_argc=a:0
   let g:helpwords_cmd="helpgrep " . re
   "echo "argc=".a:0

   echo ": helpgrep " . re
   exe "helpgrep " . re
endfun

" as of july 2006 (vim7.0.039) <f-args> treats backslashes incorrectly
" If we want to 'HELPWORDS \\ windows' and we expect HELPWORDS to get
" 2 argument, it does not. HELPWORDS gets 1 argument, this is incorrect
" and labeled in todo.txt for repair
