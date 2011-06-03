" Vim indent file
" Language:	Plain text manuscript format
" Maintainer:	Dave Hodder <dmh@dmh.org.uk>
" Last Change:	2006 Nov 25

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

" Remember, :ce and gq can be used to centre and format lines respectively.

" Use the following line in your .vimrc file to associate manuscript.vim with
" the ".manuscript.txt" file extension:
"
" au BufNewFile,BufRead *.manuscript.txt setf manuscript


" Set text to 60 columns with five character tabs (tw=60 ts=5 sts=5 et sw=5)
setlocal tw=60
setlocal ts=5
setlocal sts=5
setlocal et
setlocal sw=5

" Ensure autoindent is turned off.
setlocal noai

" Automatically format when text is inserted or deleted
setlocal fo+=aw2tq

" Set printing font to Pica-sized (12-point) Courier
setlocal pfn=courier:h12

" Set the print header
"setlocal pheader="%<%f%h%m%=Page %N "
"setlocal pheader="%<%=Author / STORY / %N "
" N.B. rightmost extra space character above is deliberate

" Common print options for all paper sizes
setlocal popt=duplex:off,header:3

" Margin widths for A4 paper

setlocal popt+=left:81pt,right:73pt,top:76pt,bottom:76pt
" was setlocal popt+=left:81pt,right:81pt,top:76pt,bottom:76pt
" N.B. 8pt removed from 'right' to account for possible extra space character

" Margin widths for letter paper (8.5" x 11")
"setlocal popt+=left:89pt,right:81pt,top:72pt,bottom:31pt

" Margin widths for folio paper (8.27" x 13")
"setlocal popt+=left:81pt,right:73pt,top:76pt,bottom:171pt

" Margin widths for legal paper (8.5" x 14")
"setlocal popt+=left:89pt,right:81pt,top:76pt,bottom:243pt
