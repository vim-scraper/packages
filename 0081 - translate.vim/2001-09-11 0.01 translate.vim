" -*- vim -*-
" FILE: "/home/johannes/.vim/plugin/translate.vim"
" LAST MODIFICATION: "Tue, 11 Sep 2001 16:59:55 +0200 (johannes)"
" (C) 2001 by Johannes Tanzler, <jtanzler@yline.com>

" Description:
" This script looks up a word in the dictionary file s:dict_file using egrep.
" When the word was found, a new window with the search results is opened at
" the bottom of the screen.

" Notes:
" This script uses grep, wc and awk which should be available on most Unix
" system.

function! Translate()
  
  " my dictionary file:
  let s:dict_file = "/usr/share/trans/de-en"
  
  " Ask for phrase. Default is word under cursor
  let s:phrase = ""
  sil let s:phrase = expand("<cword>")
  let s:phrase = input("Phrase: ", s:phrase)

  " grep dict_file and write result to tmpfile:
  let s:tmpfile = tempname()
  sil exe "!grep -h -i " . s:phrase . " " . s:dict_file . " > " . s:tmpfile
  
  " word wasn't found: print message
  " else: display output in a new window
  let s:lines = system("wc -l " . s:tmpfile . "| awk '{print $1}'") 
  if s:lines == 0
    echo s:phrase . ": Not found."
  else
    exe "botright sp " . s:tmpfile
  end
  
endfun

com! Translate call Translate()
