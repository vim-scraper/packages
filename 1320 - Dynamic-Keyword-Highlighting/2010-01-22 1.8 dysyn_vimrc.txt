:version 7.0
"c-h		: highlight selected text or clear if non selected
"c-j		: highlight pattern
"---------------------------------------------------------------------
:if (!exists("g:hicount"))
	:let g:hicount=0
	:let g:hilite = {}
	:hi M0 guibg=Violet		ctermbg=Black
	:hi M1 guibg=Blue               ctermbg=DarkBlue
	:hi M2 guibg=Brown              ctermbg=DarkGreen
	:hi M3 guibg=Cyan               ctermbg=DarkCyan
	:hi M4 guibg=DarkBlue           ctermbg=DarkRed
	:hi M5 guibg=DarkCyan           ctermbg=DarkMagenta
	:hi M6 guibg=DarkGray           ctermbg=Brown
	:hi M7 guibg=DarkGreen          ctermbg=LightGray
	:hi M8 guibg=DarkMagenta        ctermbg=DarkGray
	:hi M9 guibg=DarkRed            ctermbg=Blue
	:hi M10 guibg=DarkYellow        ctermbg=Green
	:hi M11 guibg=Gray              ctermbg=Cyan
	:hi M12 guibg=Green             ctermbg=Red
	:hi M13 guibg=LightBlue         ctermbg=Magenta
	:hi M14 guibg=LightCyan         ctermbg=Yellow
	:hi M15 guibg=LightGray         ctermbg=White
	:hi M16 guibg=LightGreen
	:hi M17 guibg=LightMagenta
	:hi M18 guibg=LightRed
	:hi M19 guibg=LightYellow
	:hi M20 guibg=Magenta
	:hi M21 guibg=Orange
	:hi M22 guibg=Purple
	:hi M23 guibg=Red
	:hi M24 guibg=SeaGreen
	:hi M25 guibg=SlateBlue

	:let g:himaxcolors=16
	:if has('gui_win32')
		:let g:himaxcolors=26
	:endif
:endif
:function! SynMatch(index, pattern)
	:let @a=a:pattern
	:if (a:index==0) 
		:normal :syn match M0 "a"
	:endif
	:if (a:index==1) 
		:normal :syn match M1 "a"
	:endif
	:if (a:index==2) 
		:normal :syn match M2 "a"
	:endif
	:if (a:index==3) 
		:normal :syn match M3 "a"
	:endif
	:if (a:index==4) 
		:normal :syn match M4 "a"
	:endif
	:if (a:index==5) 
		:normal :syn match M5 "a"
	:endif
	:if (a:index==6) 
		:normal :syn match M6 "a"
	:endif
	:if (a:index==7) 
		:normal :syn match M7 "a"
	:endif
	:if (a:index==8) 
		:normal :syn match M8 "a"
	:endif
	:if (a:index==9) 
		:normal :syn match M9 "a"
	:endif
	:if (a:index==10) 
		:normal :syn match M10 "a"
	:endif
	:if (a:index==11) 
		:normal :syn match M11 "a"
	:endif
	:if (a:index==12) 
		:normal :syn match M12 "a"
	:endif
	:if (a:index==13) 
		:normal :syn match M13 "a"
	:endif
	:if (a:index==14) 
		:normal :syn match M14 "a"
	:endif
	:if (a:index==15) 
		:normal :syn match M15 "a"
	:endif
	:if (a:index==16) 
		:normal :syn match M16 "a"
	:endif
	:if (a:index==17) 
		:normal :syn match M17 "a"
	:endif
	:if (a:index==18) 
		:normal :syn match M18 "a"
	:endif
	:if (a:index==19) 
		:normal :syn match M19 "a"
	:endif
	:if (a:index==20) 
		:normal :syn match M20 "a"
	:endif
	:if (a:index==21) 
		:normal :syn match M21 "a"
	:endif
	:if (a:index==22) 
		:normal :syn match M22 "a"
	:endif
	:if (a:index==23) 
		:normal :syn match M23 "a"
	:endif
	:if (a:index==24) 
		:normal :syn match M24 "a"
	:endif
	:if (a:index==25) 
		:normal :syn match M25 "a"
	:endif
:endfunction
:function! SynClear(index)
	:if (a:index==0) 
		:syn clear M0
	:endif
	:if (a:index==1) 
		:syn clear M1
	:endif
	:if (a:index==2) 
		:syn clear M2
	:endif
	:if (a:index==3) 
		:syn clear M3
	:endif
	:if (a:index==4) 
		:syn clear M4
	:endif
	:if (a:index==5) 
		:syn clear M5
	:endif
	:if (a:index==6) 
		:syn clear M6
	:endif
	:if (a:index==7) 
		:syn clear M7
	:endif
	:if (a:index==8) 
		:syn clear M8
	:endif
	:if (a:index==9) 
		:syn clear M9
	:endif
	:if (a:index==10) 
		:syn clear M10
	:endif
	:if (a:index==11) 
		:syn clear M11
	:endif
	:if (a:index==12) 
		:syn clear M12
	:endif
	:if (a:index==13) 
		:syn clear M13
	:endif
	:if (a:index==14) 
		:syn clear M14
	:endif
	:if (a:index==15) 
		:syn clear M15
	:endif
	:if (a:index==16) 
		:syn clear M16
	:endif
	:if (a:index==17) 
		:syn clear M17
	:endif
	:if (a:index==18) 
		:syn clear M18
	:endif
	:if (a:index==19) 
		:syn clear M19
	:endif
	:if (a:index==20) 
		:syn clear M20
	:endif
	:if (a:index==21) 
		:syn clear M21
	:endif
	:if (a:index==22) 
		:syn clear M22
	:endif
	:if (a:index==23) 
		:syn clear M23
	:endif
	:if (a:index==24) 
		:syn clear M24
	:endif
	:if (a:index==25) 
		:syn clear M25
	:endif
:endfunction
:function! SynUnlet(index)
	:if (!exists("g:hilite[a:index]"))
		:let g:hicount=1
	:endif
	:if (exists("g:hilite[a:index]"))
		:unlet g:hilite[a:index]
	:endif
:endfunction
:map <c-h> :let dummy=SynUnlet(g:hicount):let @a=g:hicount:let dummy=SynClear(g:hicount):let g:hicount=(g:hicount+(g:himaxcolors-1))%g:himaxcolors:if (g:hicount>0) | let @/=g:hilite[g:hicount]| endif:echo "Next Level:" g:hicount
:vmap <c-h> y:let g:hicount=(g:hicount+1)%g:himaxcolors:let g:hilite[g:hicount]="\\c\\V".substitute(substitute(escape(escape(@",'\'),'"'),"[\\x0a]","","g"),"[[:cntrl:]]",'\="\\m[\\x".printf("%02x",char2nr(submatch(0)))."]\\V"', "g"):let dummy=SynClear(g:hicount):let dummy=SynMatch(g:hicount,g:hilite[g:hicount]):let @/=g:hilite[g:hicount]:echo "Next Level:" g:hicount
:au BufEnter * :normal :syn clear:for index in keys(g:hilite):let dummy=SynMatch(index, g:hilite[index]):endfor
:map <c-j> y::let g:hicount=(g:hicount+1)%g:himaxcolors:let g:hilite[g:hicount]="\\c".escape(@/,'"'):let dummy=SynClear(g:hicount):let dummy=SynMatch(g:hicount,g:hilite[g:hicount]):echo "Next Level:" g:hicount
