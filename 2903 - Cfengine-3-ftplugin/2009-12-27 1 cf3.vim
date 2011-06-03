" Vim file plugin
" Language:     Cfengine version 3
" Maintainer:   Neil Watson <neil@watson-wilson.ca>
" Last Change:  Thursday December 24 2009 
" Location:
"
" This is my first attempt at a syntax file.  Feel free to send me correctsion
" or improvements.  I'll give you a credit.
"
" USAGE
" There is already a vim file that uses 'cf' as a file extension.  You can use
" cf3 for your cf3 file extensions or identify via your vimrc file:
" au BufRead,BufNewFile *.cf set ft=cf3

" Only do this when not done yet for this buffer
if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

" Abbreviations
iab = =>
iab bu bundle
iab cla classes:
iab com commands:
iab fil files:
iab met methods:
iab pro processes:
iab rep reports:
iab var vars:

" TODO
" Folding
" Indents

" CREDITS
" Neil Watson <neil@watson-wilson.ca>
