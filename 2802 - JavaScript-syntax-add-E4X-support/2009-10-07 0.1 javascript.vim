" Vim syntax file
" Lanuage: JavaScript (E4X support)
" Maintainer: Adam Katz <scriptsATkhopiscom>
" Website: http://khopis.com/scripts
" Latest Revision: 2009-10-05
" Version: 0.1
" Copyright: (c) 2009 by Adam Katz
" License: Your choice of GNU AGPL v3+ or the same license as vim v7

" Save this file to ~/.vim/after/syntax/javascript.vim and you're good to go.

if exists("javaScript_fold")
  syn region javaScriptE4X start="<\z([^>]*\)><![[]CDATA[[]" end="[]][]]></\z1>" fold
else
  syn region javaScriptE4X start="<\z([^>]*\)><![[]CDATA[[]" end="[]][]]></\z1>"
endif

if version >= 508 || !exists("did_javascript_syn_inits")
  if version < 508
    let did_javascript_syn_inits = 1
    hi! link javaScriptE4X	String
  else
    hi! def link javaScriptE4X	String
  endif
endif
