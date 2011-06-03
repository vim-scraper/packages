" File : EasyHtml.vim
" Last Change: 2001 Nov 21
" Maintainer: Gontran BAERTS <gbcreation@free.fr>
" Version: 0.1
"
" Please don't hesitate to correct my english :)
" Send corrections to <gbcreation@free.fr>
"
"-----------------------------------------------------------------------------
" Description: With EasyHtml, you no longer need to look for tags attributs
" while editing HTML files. EasyHtml let you select the right attribut by
" showing you an attributs list for the tag under the cursor.
"
"-----------------------------------------------------------------------------
" To Enable: Normally, this file will reside in your plugins directory and be
" automatically sourced.  If not, you must manually source this file
" using :source EasyHtml.vim
"
"-----------------------------------------------------------------------------
" Usage: To display the attributs list, move the cursor on the tag word and
" hit <F3> key.
"
" 	- h,j,k,l or <Left>,<Down>,<Up>,<Right> keys to change selected attribut.
"	- <Home> or <C-Home> select the first attribut.
"	- <End> or <C-End> select the last attribut.
"	- <ENTER> add selected attribut to tag and exit from attributs list.
"	- q or <ESC> to exit without adding selected attribut.
"
" Deprecated attributs as declared by W3C are red highlighted, while right
" attributs are blue highlighted.
"
"-----------------------------------------------------------------------------
" Updates:
" in version 0.1
" - First version

" Has this already been loaded ?
if exists("loaded_easyhtml")
	finish
endif
let loaded_easyhtml=1

if !exists("g:easyHtmlSplitRight")
	let g:easyHtmlSplitRight = &splitright
endif

:nmap <F3> :call LaunchEasyHtml()<cr>

let s:maxAttrLength = 0
let s:currentPos = 2

let s:coreattrs = "id=\"\" class=\"\" style=\"\" title=\"\" "
let s:i18n = "lang=\"\" dir=\"\" "
let s:events = "onclick=\"\" ondblclick=\"\" onmousedown=\"\" onmouseup=\"\" onmouseover=\"\" onmousemove=\"\" onmouseout=\"\" onkeypress=\"\" onkeydown=\"\" onkeyup=\"\" "
let s:cellhalign = "align=\"\" char=\"\" charoff=\"\" "
let s:cellvalign = "valign=\"\" "
let s:attrs = "%coreattrs %i18n %events "
let s:HTMLTags = ",%coreattrs id=\"\" class=\"\" style=\"\" title=\"\" "
	\ . ",%i18n lang=\"\" dir=\"\" "
	\ . ",%events onclick=\"\" ondblclick=\"\" onmousedown=\"\" onmouseup=\"\" onmouseover=\"\" onmousemove=\"\" onmouseout=\"\" onkeypress=\"\" onkeydown=\"\" onkeyup=\"\" "
	\ . ",%cellhalign align=\"\" char=\"\" charoff=\"\" "
	\ . ",%cellvalign valign=\"\" "
	\ . ",%attrs %coreattrs %i18n %events "
	\ . ",a %attrs charset=\"\" type=\"\" name=\"\" href=\"\" hreflang=\"\" rel=\"\" rev=\"\" accesskey=\"\" shape=\"\" coords=\"\" tabindex=\"\" onfocus=\"\" onblur=\"\" "
	\ . ",abbr %attrs "
	\ . ",acronym %attrs "
	\ . ",address %attrs "
	\ . ",applet-D %coreattrs alt=\"\" align=\"\"-D hspace-D=\"\" vspace-D=\"\" codebase-D=\"\" code-D=\"\" name-D=\"\" archive-D=\"\" object-D=\"\" width-D=\"\" height-D=\"\" "
	\ . ",area %attrs shape=\"\" coords=\"\" usemap=\"\" nohref=\"\" name=\"\" alt=\"\" href=\"\" tabindex=\"\" accesskey=\"\" onfocus=\"\" onblur=\"\" "
	\ . ",b-D %attrs "
	\ . ",base href=\"\" target=\"\" "
	\ . ",basefont-D %coreattrs %i18n size=\"\"-D color=\"\"-D face-D=\"\" "
	\ . ",bdo %coreattrs lang=\"\" dir=\"\" "
	\ . ",big-D %attrs "
	\ . ",blockquote %attrs cite=\"\" "
	\ . ",body %attrs onload=\"\" onunload=\"\" background=\"\"-D bgcolor-D=\"\" text-D=\"\" link-D=\"\" vlink-D=\"\" alink-D=\"\" "
	\ . ",br %coreattrs clear=\"\" "
	\ . ",button %attrs name=\"\" value=\"\" type=\"\" disabled=\"\" tabindex=\"\" accesskey=\"\" onfocus=\"\" onblur=\"\" "
	\ . ",caption %attrs align=\"\"-D "
	\ . ",center-D "
	\ . ",cite %attrs "
	\ . ",code %attrs "
	\ . ",col %attrs span=\"\" width=\"\" %cellhalign %cellvalign "
	\ . ",colgroup %attrs span=\"\" width=\"\" %cellhalign %cellvalign "
	\ . ",dd %attrs "
	\ . ",del %attrs cite=\"\" datetime=\"\" "
	\ . ",dfn %attrs "
	\ . ",dir-D %coreattrs "
	\ . ",div %attrs "
	\ . ",dl %attrs "
	\ . ",dt %attrs "
	\ . ",em %attrs "
	\ . ",fieldset %attrs "
	\ . ",font-D %coreattrs %i18n size=\"\"-D color-D=\"\" face-D=\"\" "
	\ . ",form %attrs action=\"\" method=\"\" enctype=\"\" accept=\"\" name=\"\" onsubmit=\"\" onreset=\"\" accept-charset=\"\" "
	\ . ",frame %coreattrs longdesc=\"\" name=\"\" src=\"\" frameborder=\"\" marginwidth=\"\" marginheight=\"\" noresize=\"\" scrolling=\"\" "
	\ . ",frameset %coreattrs rows=\"\" cols=\"\" onload=\"\" onunload=\"\" "
	\ . ",h1 %attrs "
	\ . ",h2 %attrs "
	\ . ",h3 %attrs "
	\ . ",h4 %attrs "
	\ . ",h5 %attrs "
	\ . ",h6 %attrs "
	\ . ",head %i18n profile=\"\" "
	\ . ",hr %attrs align=\"\"-D noshade-D=\"\" size-D=\"\" width-D=\"\" "
	\ . ",html %i18n cdata=\"\"-D "
	\ . ",i-D %attrs "
	\ . ",iframe %coreattrs longdesc=\"\" name=\"\" src=\"\" frameborder=\"\" marginwidth=\"\" marginheight=\"\" scrolling=\"\" align=\"\" height=\"\" width=\"\" "
	\ . ",img %attrs src=\"\" alt=\"\" longdesc=\"\" name=\"\" height=\"\" width=\"\" usemap=\"\" ismap=\"\" align=\"\"-D border-D=\"\" hspace-D=\"\" vspace-D=\"\" "
	\ . ",input %attrs type=\"\" name=\"\" value=\"\" checked=\"\" disabled=\"\" readonly=\"\" size=\"\" maxlength=\"\" src=\"\" alt=\"\" usemap=\"\" ismap=\"\" tabindex=\"\" accesskey=\"\" onfocus=\"\" onblur=\"\" onselect=\"\" onchange=\"\" accept=\"\" align=\"\"-D "
	\ . ",ins %attrs cite=\"\" datetime=\"\" "
	\ . ",isindex-D %coreattrs %i18n "
	\ . ",kbd %attrs "
	\ . ",label %attrs for=\"\" accesskey=\"\" onfocus=\"\" onblur=\"\" "
	\ . ",legend %attrs accesskey=\"\" align=\"\"-D "
	\ . ",li %attrs type=\"\"-D start-D=\"\" value-D=\"\" compact-D=\"\" "
	\ . ",link %attrs charset=\"\" href=\"\" hreflang=\"\" type=\"\" rel=\"\" rev=\"\" media=\"\" target=\"\" "
	\ . ",map %attrs name=\"\" "
	\ . ",menu-D %attrs "
	\ . ",meta %i18n http-equiv=\"\" name=\"\" content=\"\" scheme=\"\" "
	\ . ",noframes %attrs "
	\ . ",noscript %attrs "
	\ . ",object %attrs declare=\"\" classid=\"\" codebase=\"\" data=\"\" type=\"\" codetype=\"\" archive=\"\" standby=\"\" height=\"\"-D width-D=\"\" usemap=\"\" name=\"\" tabindex=\"\" align-D=\"\" border-D=\"\" hspace-D=\"\" vspace-D=\"\" "
	\ . ",ol %attrs "
	\ . ",optgroup %attrs disabled=\"\" label=\"\" "
	\ . ",option %attrs selected=\"\" disabled=\"\" label=\"\" value=\"\" "
	\ . ",p %attrs "
	\ . ",param id=\"\" name=\"\" value=\"\" valuetype=\"\" type=\"\" "
	\ . ",pre %attrs width=\"\"-D "
	\ . ",q %attrs cite=\"\" "
	\ . ",s-D %attrs "
	\ . ",samp %attrs "
	\ . ",script charset=\"\" type=\"\" src=\"\" defer=\"\" language=\"\"-D "
	\ . ",select %attrs name=\"\" size=\"\" multiple=\"\" disabled=\"\" tabindex=\"\" onfocus=\"\" onblur=\"\" onchange=\"\" "
	\ . ",small-D %attrs "
	\ . ",span %attrs "
	\ . ",strike-D %attrs "
	\ . ",strong %attrs "
	\ . ",style %i18n type=\"\" media=\"\" title=\"\" "
	\ . ",sub %attrs "
	\ . ",sup %attrs "
	\ . ",table %attrs summary=\"\" width=\"\" border=\"\" frame=\"\" rules=\"\" cellspacing=\"\" cellpadding=\"\" align=\"\"-D "
	\ . ",tbody %attrs %cellhalign %cellvalign "
	\ . ",td %attrs abbr=\"\" axis=\"\" headers=\"\" scope=\"\" rowspan=\"\" colspan=\"\" %cellhalign %cellvalign nowrap=\"\"-D width-D=\"\" height-D=\"\" bgcolor-D=\"\" "
	\ . ",textarea %attrs name=\"\" rows=\"\" cols=\"\" disabled=\"\" readonly=\"\" tabindex=\"\" accesskey=\"\" onfocus=\"\" onblur=\"\" onselect=\"\" onchange=\"\" "
	\ . ",tfoot %attrs %cellhalign %cellvalign "
	\ . ",th %attrs abbr=\"\" axis=\"\" headers=\"\" scope=\"\" rowspan=\"\" colspan=\"\" %cellhalign %cellvalign nowrap=\"\"-D width-D=\"\" height-D=\"\" bgcolor-D=\"\" align=\"\" valign=\"\" char=\"\" charoff=\"\" valign=\"\" "
	\ . ",thead %attrs %cellhalign %cellvalign "
	\ . ",title %i18n "
	\ . ",tr %attrs %cellhalign %cellvalign bgcolor=\"\"-D "
	\ . ",tt-D %attrs "
	\ . ",u-D %attrs "
	\ . ",ul %attrs "
	\ . ",var %attrs"

function! LaunchEasyHtml()
	" Look for attributs for the current word
	call s:SearchAttributes()

	" If the longest attribut length is 0, there is no attribut for the
	" current word
	if s:maxAttrLength == 0
		echohl ErrorMsg
		echo "No attributes for \"" . expand("<cword>") . "\""
		echohl NONE
		return
	endif         
	
	" Save the user's settings for splitright
	let savesplitright = &splitright
	" Configure vertical splitting side
	let &splitright = g:easyHtmlSplitRight
	" Open new vertical window with right size
	execute s:maxAttrLength . "vnew"

	" Turn off the swapfile, set the buffer type so that it won't get
	" written, and so that it will get deleted when it gets hidden.
	setlocal modifiable
	setlocal noswapfile
	setlocal buftype=nowrite
	setlocal bufhidden=delete
	setlocal nonumber

	" Don't wrap around long lines
	setlocal nowrap

	" No need for any insertmode abbreviations, since we don't allow
	" insertions anyway!
	iabc <buffer>

	" Highlighting
	syntax match selectedAttribut /^<.*>$/
	syntax match deprecatedAttribut /^(.*)$/
	syntax match hiddenX /X/
	hi selectedAttribut guibg=lightblue guifg=black
	hi deprecatedAttribut guibg=lightred guifg=black
	let color= s:GetBgColor()
	if color != ""
		exe "hi hiddenX guibg=" . color . " guifg=" . color
	endif

	call s:ShowAttributes()

	" Set up mappings for this buffer
	nnoremap <buffer> <Left> :call <SID>MoveSelect( line(".")-1 )<CR>
	nnoremap <buffer> <Up> :call <SID>MoveSelect( line(".")-1 )<CR>
	nnoremap <buffer> <Right> :call <SID>MoveSelect( line(".")+1 )<CR>
	nnoremap <buffer> <Down> :call <SID>MoveSelect( line(".")+1 )<CR>
	nnoremap <buffer> h :call <SID>MoveSelect( line(".")-1 )<CR>
	nnoremap <buffer> k :call <SID>MoveSelect( line(".")-1 )<CR>
	nnoremap <buffer> l :call <SID>MoveSelect( line(".")+1 )<CR>
	nnoremap <buffer> j :call <SID>MoveSelect( line(".")+1 )<CR>
	nnoremap <buffer> <C-Home> :call <SID>MoveSelect( 1 )<cr>
	nnoremap <buffer> <C-End> :call <SID>MoveSelect( line("$") )<cr>
	nnoremap <buffer> <Home> :call <SID>MoveSelect( 1 )<cr>
	nnoremap <buffer> <End> :call <SID>MoveSelect( line("$") )<cr>
	nnoremap <buffer> <cr> :call <SID>AddAttribute()<cr>
	nnoremap <buffer> <2-LeftMouse> :call <SID>AddAttribute()<cr>
	nnoremap <buffer> <esc> :call <SID>CloseWindow()<cr>
	nnoremap <buffer> q :call <SID>CloseWindow()<cr>

	" Restore user settings
	let &splitright = savesplitright
endfunction

function! s:SearchAttributes()
	let l:CurrentCase = &ignorecase
	set ignorecase
	let s:attributs = ""
	let s:maxAttrLength = 0
	let l:pos = matchend( s:HTMLTags, ",\\<". expand("<cword>") ."\\S*\\>" )
	if l:pos != -1
		let l:attributsLine = strpart( s:HTMLTags, l:pos+1, strlen(s:HTMLTags)-l:pos-1 )
		let l:CommaPos = match( l:attributsLine, "," )
		if l:CommaPos != -1
			let l:attributsLine = strpart( l:attributsLine, 0, l:CommaPos )
		endif
		if l:attributsLine != ""
			let l:attribut = matchstr( l:attributsLine, "[^ ]*" )
			while l:attribut != ""
				if match( l:attribut, "^%" ) != -1
					exe "let l:attributsLine = s:" . strpart( l:attribut, 1 ) . ". strpart( l:attributsLine, strlen(l:attribut)+1, strlen(l:attributsLine) - strlen(l:attribut) )"
					let l:attribut = matchstr( l:attributsLine, "[^ ]*" )
					continue
				endif
				if s:maxAttrLength < strlen( l:attribut )
					let s:maxAttrLength = strlen( l:attribut )
				endif
				let l:attributsLine = strpart( l:attributsLine, strlen(l:attribut)+1, strlen(l:attributsLine) - strlen(l:attribut) )
				" Is it a depracated attribute ?
				if l:attribut =~ "-D"
					let l:attribut = substitute( l:attribut, "-D", "", "")
					let s:attributs = s:attributs . "X" . l:attribut . " \n"
				else
					let s:attributs = s:attributs . " " . l:attribut . " \n"
				endif
				let l:attribut = matchstr( l:attributsLine, "[^ ]*" )
			endwhile
		endif
		" If longest attribute size is zero, so there is no attribute
		" for this tag
		if s:maxAttrLength != 0
			let s:maxAttrLength = s:maxAttrLength + 2
		endif
	endif
	let &ignorecase = l:CurrentCase
endfunction

function! s:ShowAttributes()
	" Prevent a report of our actions from showing up
	let oldRep=&report
	let save_sc = &sc
	set report=10000 nosc

	" Go to first line
	1

	" Put content of register f after the cursor
	put! =s:attributs

	" Erase last line
	exe "normal G"
	d
	" Move to first item
	call s:MoveSelect(1)         " Restore config
	let &report=oldRep
	let &sc = save_sc
endfunction

function! s:AddAttribute()
	" Get attribute
	let save_f=@f
	let @f = " " . substitute( getline("."), "[ <>()X]", "", "g" )
	" Go to previous window
	wincmd p
	" Put attribute at end of tag
	exec "normal f>\"fP"
	startinsert
	" Return to attributes window
	wincmd p
	let @f=save_f
	" Close window
	call s:CloseWindow()
endfunction

function! s:MoveSelect( newLineNumber )
	if( a:newLineNumber < 1 || a:newLineNumber > line("$") )
		return
	endif

	" Restore current line
	if( exists("s:currentLine") )
		call setline( ".", s:currentLine )
	endif

	" Go to new line
	let s:currentPos = a:newLineNumber
	"exec "normal gg"
	exec s:currentPos

	" Save new current line
	let s:currentLine = getline(".")
	let l:modifiedLine = s:currentLine

	" Complete string with spaces
	while strlen(l:modifiedLine) < s:maxAttrLength
		let l:modifiedLine = l:modifiedLine . " "
	endwhile
	
	" Is it a deprecated attribute marked with 'X' ?
	if l:modifiedLine =~ "^X"
		let l:modifiedLine = substitute( l:modifiedLine, "^X", "(", "" )
		let l:modifiedLine = substitute( l:modifiedLine, " $", ")", "" )
	else
		let l:modifiedLine = substitute( l:modifiedLine, "^ ", "<", "" )
		let l:modifiedLine = substitute( l:modifiedLine, " $", ">", "" )
	endif
	call setline( ".", l:modifiedLine )
endfunction

function! s:CloseWindow()
	unlet s:currentLine
	unlet s:attributs
	let s:maxAttrLength = 0
	let s:currentPos = 2
	highlight clear selectedAttribut
	highlight clear deprecatedAttribut
	highlight clear hiddenX
	wincmd q
endfunction

" Try to get background color (may be not sure)
function! s:GetBgColor()
	let bgColor = synIDattr(synIDtrans(synID(1, 1, 1)), "bg")
	if bgColor == ""
		if &background == "light"
			let bgColor = "white"
		else
			let bgColor = "black"
		endif
	endif
	return bgColor
endfunction
