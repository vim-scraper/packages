if exists("g:AutoHiVersion")
   finish
endif

let g:AutoHiVersion="0.1b"
hi AutoHiGroup guibg=yellow

let g:AutoHiList = ["c", "cpp", "cpp+doxygen", "java"]

augroup AutoHi
   au!
   au CursorHold * :call AutoHighlight()
augroup END

fu! AutoHighlight()
   if (exists("b:current_syntax")) && (index(g:AutoHiList,b:current_syntax) != -1)
      if synIDattr(synID(line("."), col("."), 1), "name") == ""
	 let cword=expand("<cword>")
	 if cword =~ '\<\*\?\h\w*'
	    exe 'match AutoHiGroup /\<'.cword.'\>/'
	 endif
      endif
   endif
endf

