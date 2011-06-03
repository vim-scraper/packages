" Vim filetype plugin file
" Language:	Intel Network Processor Microcode
" Maintainer:	Gui Wei <gawain_g@citiz.net>

" Only do this when not done yet for this buffer
if exists("b:did_ftplugin")
  finish
endif

" Don't load another plugin for this buffer
let b:did_ftplugin = 1

set cpo-=C

let b:undo_ftplugin = "setl fo< com<"

" Set 'formatoptions' to break comment lines but not other lines,
" and insert the comment leader when hitting <CR> or using "o".
setlocal fo-=t fo+=croql

" Set 'comments' to format dashed lists in comments.
setlocal comments=sO:*\ -,mO:*\ \ ,exO:*/,s1:/*,mb:*,ex:*/,://,:;

setlocal iskeyword+=.

" Win32 can filter files in the browse dialog
if has("gui_win32") && !exists("b:browsefilter")
  if &ft == "uc"
    let b:browsefilter = "MicroCode Source Files (*.uc)\t*.uc\n" .
	  \ "MicroCode Header Files (*.h)\t*.h\n" .
	  \ "MicroCode Script Files (*.ind)\t*.ind\n" .
	  \ "All Files (*.*)\t*.*\n"
  endif
endif
