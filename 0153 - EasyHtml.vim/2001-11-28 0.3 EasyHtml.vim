" File : EasyHtml.vim
" Last Change: 2001 Nov 21
" Maintainer: Gontran BAERTS <gbcreation@free.fr>
" Version: 0.3
"
" Please don't hesitate to correct my english :)
" Send corrections to <gbcreation@free.fr>
"
"-----------------------------------------------------------------------------
" Description: With EasyHtml, you no longer need to look for tags attributes
" while editing HTML files. EasyHtml let you select the right attribute by
" showing you an attributes list for the tag under the cursor.
"
"-----------------------------------------------------------------------------
" To Enable: Normally, this file will reside in your plugins directory and be
" automatically sourced.  If not, you must manually source this file
" using :source EasyHtml.vim
"
"-----------------------------------------------------------------------------
" Usage: To display the attributes/values list, move the cursor on the
" tag/attribute word and hit <F3> key.
"
" 	- h,j,k,l or <Left>,<Down>,<Up>,<Right> keys to change selected attribute.
"	- <Home> or <C-Home> select the first attribute.
"	- <End> or <C-End> select the last attribute.
"	- <ENTER> add selected attribute to tag and exit from attributes list.
"	- q or <ESC> to exit without adding selected attribute.
"
" Deprecated attributes as declared by W3C are red highlighted, while right
" attributes are blue highlighted.
"
" Set g:easyHtmlSplitRight variable to 0 or 1 to open attributes list at left
" or right of current buffer. By defaut, use splitright setting.
" 
"-----------------------------------------------------------------------------
" Updates:
" in version 0.3
"  - Attributes list updated
"  - Don't display attributes list for closing tags
"  - Now, display values list when hitting <F3> with cursor on attribute word
"    (for some attributes only).
"
" in version 0.2.1
" - Fix global modifiable setting instead of local
"
" in version 0.2
" - Attributes list is now alphabetically sorted
" - Hitting <F3> allows to display attributes list in Insert mode too
" - Allows to select an attribute by incremental search :-)
"   For example, with <body> tag, typing "onk" (normal mode) in the attributes
"   list buffer automatically select "onkeydown" attribute. Use backspace
"   (<BS>) to remove characters. This behavior is enable by setting
"   g:eh_incsearch variable to 1. Warning : when incremental attribute search
"   is on, 'q', 'h', 'j', 'k' and 'l' keys aren't used to exit from list and
"   to move highlighting. Use 'Q', '<Left>', '<Down>', '<Up>' and '<Right>'
"   instead.
" - Check for attributes list already opened, and reuse it
"
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

if !exists("g:eh_incsearch")
	let g:eh_incsearch = 0
endif

:nmap <F3> :call LaunchEasyHtml()<cr>
:imap <F3> <esc>:call LaunchEasyHtml()<cr>

"**
" Script Variables:
"**
let s:maxAttrLength = 0
let s:currentPos = 2

let s:coreattrs = "id=\"\" class=\"\" style=\"\" title=\"\""
let s:i18n = "lang=\"\" dir=\"\""
let s:events = "onclick=\"\" ondblclick=\"\" onmousedown=\"\" onmouseup=\"\" onmouseover=\"\" onmousemove=\"\" onmouseout=\"\" onkeypress=\"\" onkeydown=\"\" onkeyup=\"\""
let s:cellhalign = "align=\"\" char=\"\" charoff=\"\""
let s:cellvalign = "valign=\"\""
let s:attrs = "%coreattrs %i18n %events"
let s:HTMLTags = "<a %attrs charset=\"\" target=\"\" type=\"\" name=\"\" href=\"\" hreflang=\"\" rel=\"\" rev=\"\" accesskey=\"\" shape=\"\" coords=\"\" tabindex=\"\" onfocus=\"\" onblur=\"\""
	\ . ",<abbr %attrs"
	\ . ",<acronym %attrs"
	\ . ",<address %attrs"
	\ . ",<applet %coreattrs alt=\"\" align-D=\"\"-D hspace-D=\"\" vspace-D=\"\" codebase-D=\"\" code-D=\"\" name-D=\"\" archive-D=\"\" object-D=\"\" width-D=\"\" height-D=\"\""
	\ . ",<area %attrs shape=\"\" coords=\"\" nohref name=\"\" alt=\"\" href=\"\" tabindex=\"\" accesskey=\"\" onfocus=\"\" onblur=\"\" target=\"\""
	\ . ",<b %attrs"
	\ . ",<base href=\"\" target=\"\""
	\ . ",<basefont size-D=\"\" color-D=\"\" face-D=\"\""
	\ . ",<bdo %coreattrs lang=\"\" dir=\"\""
	\ . ",<big %attrs"
	\ . ",<blockquote %attrs cite=\"\""
	\ . ",<body %attrs onload=\"\" onunload=\"\" background-D=\"\" bgcolor-D=\"\" text-D=\"\" link-D=\"\" vlink-D=\"\" alink-D=\"\""
	\ . ",<br %coreattrs clear-D=\"\""
	\ . ",<button %attrs name=\"\" value=\"\" type=\"\" disabled tabindex=\"\" accesskey=\"\" onfocus=\"\" onblur=\"\""
	\ . ",<caption %attrs align-D=\"\""
	\ . ",<center %attrs"
	\ . ",<cite %attrs"
	\ . ",<code %attrs"
	\ . ",<col %attrs span=\"\" width=\"\" %cellhalign %cellvalign"
	\ . ",<colgroup %attrs span=\"\" width=\"\" %cellhalign %cellvalign"
	\ . ",<dd %attrs"
	\ . ",<del %attrs cite=\"\" datetime=\"\""
	\ . ",<dfn %attrs"
	\ . ",<dir %attrs compact-D"
	\ . ",<div %attrs align-D=\"\""
	\ . ",<dl %attrs compact-D"
	\ . ",<dt %attrs"
	\ . ",<em %attrs"
	\ . ",<fieldset %attrs"
	\ . ",<font %coreattrs %i18n size-D=\"\"-D color-D=\"\" face-D=\"\""
	\ . ",<form %attrs action=\"\" method=\"\" enctype=\"\" name=\"\" onsubmit=\"\" onreset=\"\" accept=\"\" accept-charset=\"\" target=\"\""
	\ . ",<frame %coreattrs longdesc=\"\" name=\"\" src=\"\" frameborder=\"\" marginwidth=\"\" marginheight=\"\" noresize scrolling=\"\""
	\ . ",<frameset %coreattrs rows=\"\" cols=\"\" onload=\"\" onunload=\"\""
	\ . ",<h1 %attrs align-D=\"\""
	\ . ",<h2 %attrs align-D=\"\""
	\ . ",<h3 %attrs align-D=\"\""
	\ . ",<h4 %attrs align-D=\"\""
	\ . ",<h5 %attrs align-D=\"\""
	\ . ",<h6 %attrs align-D=\"\""
	\ . ",<head %i18n profile=\"\""
	\ . ",<hr %attrs align-D=\"\" noshade-D size-D=\"\" width-D=\"\""
	\ . ",<html %i18n version-D=\"\""
	\ . ",<i %attrs"
	\ . ",<iframe %coreattrs longdesc=\"\" name=\"\" src=\"\" frameborder=\"\" marginwidth=\"\" marginheight=\"\" scrolling=\"\" align-D=\"\" height=\"\" width=\"\""
	\ . ",<img %attrs src=\"\" alt=\"\" longdesc=\"\" name=\"\" height=\"\" width=\"\" usemap=\"\" ismap align-D=\"\" border-D=\"\" hspace-D=\"\" vspace-D=\"\""
	\ . ",<input %attrs type=\"\" name=\"\" value=\"\" checked disabled readonly size=\"\" maxlength=\"\" src=\"\" alt=\"\" usemap=\"\" ismap tabindex=\"\" accesskey=\"\" onfocus=\"\" onblur=\"\" onselect=\"\" onchange=\"\" accept=\"\" align-D=\"\""
	\ . ",<ins %attrs cite=\"\" datetime=\"\""
	\ . ",<isindex %coreattrs %i18n prompt-D=\"\""
	\ . ",<kbd %attrs"
	\ . ",<label %attrs for=\"\" accesskey=\"\" onfocus=\"\" onblur=\"\""
	\ . ",<legend %attrs accesskey=\"\" align-D=\"\""
	\ . ",<li %attrs type-D=\"\" value-D=\"\""
	\ . ",<link %attrs charset=\"\" href=\"\" hreflang=\"\" type=\"\" rel=\"\" rev=\"\" media=\"\" target=\"\""
	\ . ",<map %attrs name=\"\""
	\ . ",<menu %attrs compact-D"
	\ . ",<meta %i18n http-equiv=\"\" name=\"\" content=\"\" scheme=\"\""
	\ . ",<noframes %attrs"
	\ . ",<noscript %attrs"
	\ . ",<object %attrs declare classid=\"\" codebase=\"\" data=\"\" type=\"\" codetype=\"\" archive=\"\" standby=\"\" height=\"\" width=\"\" usemap=\"\" name=\"\" tabindex=\"\" align-D=\"\" border-D=\"\" hspace-D=\"\" vspace-D=\"\""
	\ . ",<ol %attrs type-D=\"\" start-D=\"\" compact-D"
	\ . ",<optgroup %attrs disabled label=\"\""
	\ . ",<option %attrs selected disabled label=\"\" value=\"\""
	\ . ",<p %attrs align-D=\"\""
	\ . ",<param id=\"\" name=\"\" value=\"\" valuetype=\"\" type=\"\""
	\ . ",<pre %attrs width-D=\"\""
	\ . ",<q %attrs cite=\"\""
	\ . ",<s %attrs"
	\ . ",<samp %attrs"
	\ . ",<script charset=\"\" type=\"\" src=\"\" defer language-D=\"\""
	\ . ",<select %attrs name=\"\" size=\"\" multiple disabled tabindex=\"\" onfocus=\"\" onblur=\"\" onchange=\"\""
	\ . ",<small %attrs"
	\ . ",<span %attrs"
	\ . ",<strike %attrs"
	\ . ",<strong %attrs"
	\ . ",<style %i18n type=\"\" media=\"\" title=\"\""
	\ . ",<sub %attrs"
	\ . ",<sup %attrs"
	\ . ",<table %attrs summary=\"\" width=\"\" border=\"\" frame=\"\" rules=\"\" cellspacing=\"\" cellpadding=\"\" align-D=\"\" bgcolor-D=\"\""
	\ . ",<tbody %attrs %cellhalign %cellvalign"
	\ . ",<td %attrs abbr=\"\" axis=\"\" headers=\"\" scope=\"\" rowspan=\"\" colspan=\"\" %cellhalign %cellvalign nowrap-D width-D=\"\" height-D=\"\" bgcolor-D=\"\""
	\ . ",<textarea %attrs name=\"\" rows=\"\" cols=\"\" disabled readonly tabindex=\"\" accesskey=\"\" onfocus=\"\" onblur=\"\" onselect=\"\" onchange=\"\""
	\ . ",<tfoot %attrs %cellhalign %cellvalign"
	\ . ",<th %attrs abbr=\"\" axis=\"\" headers=\"\" scope=\"\" rowspan=\"\" colspan=\"\" %cellhalign %cellvalign nowrap-D width-D=\"\" height-D=\"\" bgcolor-D=\"\""
	\ . ",<thead %attrs %cellhalign %cellvalign"
	\ . ",<title %i18n"
	\ . ",<tr %attrs %cellhalign %cellvalign bgcolor-D=\"\""
	\ . ",<tt %attrs"
	\ . ",<u %attrs"
	\ . ",<ul %attrs type-D=\"\" compact-D"
	\ . ",<var %attrs"
let s:TagsAttributs = "align= left center right justify top middle bottom char"
	\ . ",alink= Black Silver Gray White Maroon Red Purple Fuchsia Green Lime Olive Yellow Navy Blue Teal Aqua"
	\ . ",bgcolor= Black Silver Gray White Maroon Red Purple Fuchsia Green Lime Olive Yellow Navy Blue Teal Aqua"
	\ . ",charset= ISO-8859-1 SHIFT_JIS UTF-8"
	\ . ",clear= none left right all"
	\ . ",color= Black Silver Gray White Maroon Red Purple Fuchsia Green Lime Olive Yellow Navy Blue Teal Aqua"
	\ . ",content= text/css"
	\ . ",dir= ltr rtl"
	\ . ",frameborder= 0 1"
	\ . ",frame= void above below hsides lhs rhs vsides box border"
	\ . ",hreflang= aa ab af am ar as ay az ba be bg bh bi bn bo br ca co cs cy da de dz el en eo es et eu fa fi fj fo fr fy ga gd gl gn gu ha he hi hr hu hy ia ie ik id is it iu ja jv ka kk kl km kn ko ks ku ky la ln lo lt lv mg mi mk ml mn mo mr ms mt my na ne nl no oc om or pa pl ps pt qu rm rn ro ru rw sa sd sg sh si sk sl sm sn so sq sr ss st su sv sw ta te tg th ti tk tl tn to tr ts tt tw ug uk ur uz vi vo wo xh yi yo za zh zu"
	\ . ",http-equiv= Content-Type Expires PICS-Label Content-Script-Type Content-Style-Type"
	\ . ",lang= aa ab af am ar as ay az ba be bg bh bi bn bo br ca co cs cy da de dz el en eo es et eu fa fi fj fo fr fy ga gd gl gn gu ha he hi hr hu hy ia ie ik id is it iu ja jv ka kk kl km kn ko ks ku ky la ln lo lt lv mg mi mk ml mn mo mr ms mt my na ne nl no oc om or pa pl ps pt qu rm rn ro ru rw sa sd sg sh si sk sl sm sn so sq sr ss st su sv sw ta te tg th ti tk tl tn to tr ts tt tw ug uk ur uz vi vo wo xh yi yo za zh zu"
	\ . ",link= Black Silver Gray White Maroon Red Purple Fuchsia Green Lime Olive Yellow Navy Blue Teal Aqua"
	\ . ",media= screen tty tv projection handheld print braille aural all"
	\ . ",method= get post"
	\ . ",name= Author copyright date keywords"
	\ . ",rel= Made Alternate StyleSheet Start Next Prev Contents Index Glossary Copyright Chapter Section Subsection Appendix Help Bookmark"
	\ . ",rev= Made Alternate StyleSheet Start Next Prev Contents Index Glossary Copyright Chapter Section Subsection Appendix Help Bookmark"
	\ . ",rules= none groups rows cols all"
	\ . ",scope= row col rowgroup colgroup"
	\ . ",scrolling= yes no auto"
	\ . ",shape= rect circle poly default"
	\ . ",target= _blank _self _parent _top"
	\ . ",text= Black Silver Gray White Maroon Red Purple Fuchsia Green Lime Olive Yellow Navy Blue Teal Aqua"
	\ . ",type= disc square circle 1 a A i I text/html image/jpeg model/vrml video/quicktime application/java text/css text/javascript text password checkbox radio submit reset file hidden image button"
	\ . ",valign= top middle bottom baseline"
	\ . ",valuetype= data ref object"
	\ . ",vlink= Black Silver Gray White Maroon Red Purple Fuchsia Green Lime Olive Yellow Navy Blue Teal Aqua"

"**
" List Functions:
"**
function! GetListItem( array, index )
	if a:index == 0
		return matchstr( a:array, '^[^' . s:listSep . ']\+' )
	else
		return matchstr( a:array, "[^" . s:listSep . "]\\+", matchend( a:array, '\(\%(^\|' . s:listSep . '\)[^' . s:listSep . ']\+\)\{' . a:index . '\}' . s:listSep ) )
	endif
endfunction

function! GetListMatchItem( array, pattern )
	return matchstr( a:array, '[^' . s:listSep . ']*' . a:pattern . '[^' . s:listSep . ']*' )
endfunction

function! ReplaceListItem( array, index, item )
	if a:index == 0
		return substitute( a:array, '^[^' .s:listSep. ']\+', a:item, "" )
	else
		return substitute( a:array, '\(\%(\%(^\|' . s:listSep . '\)[^' . s:listSep . ']\+\)\{' . a:index . '\}\)' . s:listSep . '[^' . s:listSep . ']\+', '\1' . s:listSep . a:item , "" )
	endif
endfunction

function! RemoveListItem( array, index )
	if a:index == 0
		return substitute( a:array, '^[^' .s:listSep. ']\+\(' . s:listSep . '\|$\)', "", "" )
	else
		return substitute( a:array, '\(\%(\%(^\|' . s:listSep . '\)[^' . s:listSep . ']\+\)\{' . a:index . '\}\)' . s:listSep . '[^' . s:listSep . ']\+', '\1', "" )
	endif
endfunction

function! GetListCount( array )
	if a:array == "" | return 0 | endif
	let pos = 0
	let cnt = 0
	while pos != -1
		let pos = matchend( a:array, s:listSep, pos )
		let cnt = cnt + 1
	endwhile
	return cnt
endfunction

function! QuickSortList( tabEnt, deb, fin )
	let tabEnt = a:tabEnt
	let pivot = GetListItem( tabEnt, a:deb )

	let g = a:deb
	let d = a:fin
	while g < d

		while GetListItem( tabEnt, d ) > pivot
			let d = d - 1
		endwhile
		if g != d
			let tabEnt = ReplaceListItem( tabEnt, g, GetListItem( tabEnt, d ) )
			let tabEnt = ReplaceListItem( tabEnt, d, pivot )
			let g = g + 1
		endif
		
		while GetListItem( tabEnt, g ) < pivot
			let g = g + 1
		endwhile
		if g != d
			let tabEnt = ReplaceListItem( tabEnt, d, GetListItem( tabEnt, g ) )
			let tabEnt = ReplaceListItem( tabEnt, g, pivot )
			let d = d - 1
		endif
	endwhile

	if a:deb < g-1
		let tabEnt = QuickSortList( tabEnt, a:deb, g-1 )
	endif
	if a:fin > g+1
		let tabEnt = QuickSortList( tabEnt, g+1, a:fin )
	endif
	return tabEnt
endfunction

"**
" LaunchEasyHtml:
" Search if there are attributs for word under cursor, and display them in a
" new buffer.
"**
function! LaunchEasyHtml()
	" Look for attributs for the current word
	call s:SearchAttributes()

	" If the longest attribut length is 0, there is no attribut for the
	" current word
	if s:maxAttrLength == 0
		echohl ErrorMsg
		echo "No attributes\\values found. (If it's a closing tag, try on opening tag.)"
		echohl NONE
		return
	endif         

	" Is there an attributs list already running ?
	let BufNr = bufnr( '--\ Attributs\ list\ --' )
	if BufNr != -1
		let CurBufNr = bufnr("%")
		while CurBufNr != BufNr
			wincmd w
			let CurBufNr = bufnr("%")
		endwhile
	else
		" Save the user's settings for splitright
		let savesplitright = &splitright
		" Configure vertical splitting side
		let &splitright = g:easyHtmlSplitRight
		" Open new vertical window with right size
		execute s:maxAttrLength . 'vnew --\ Attributs\ list\ --'
		" Restore user settings
		let &splitright = savesplitright

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

		" Set up mappings for this buffer
		nnoremap <buffer> <Left> :call <SID>MoveSelect( line(".")-1 )<CR>
		nnoremap <buffer> <Up> :call <SID>MoveSelect( line(".")-1 )<CR>
		nnoremap <buffer> <Right> :call <SID>MoveSelect( line(".")+1 )<CR>
		nnoremap <buffer> <Down> :call <SID>MoveSelect( line(".")+1 )<CR>
		nnoremap <buffer> <C-Home> :call <SID>MoveSelect( 1 )<cr>
		nnoremap <buffer> <C-End> :call <SID>MoveSelect( line("$") )<cr>
		nnoremap <buffer> <Home> :call <SID>MoveSelect( 1 )<cr>
		nnoremap <buffer> <End> :call <SID>MoveSelect( line("$") )<cr>
		nnoremap <buffer> <cr> :call <SID>AddAttribute()<cr>
		nnoremap <buffer> <2-LeftMouse> :call <SID>AddAttribute()<cr>
		nnoremap <buffer> <esc> :call <SID>CloseWindow()<cr>

		" If incremental search required, initialize it
		if( g:eh_incsearch == 1 )
			nnoremap <buffer> Q :call <SID>CloseWindow()<cr>
			nnoremap <buffer> <BS> :call <SID>SelectSearch( "" )<cr>
			let char = 97
			while char < 123
				exe "nnoremap <buffer> " . nr2char(char) . " :call <SID>SelectSearch( '" . nr2char(char) . "' )<cr>"
				let char = char + 1
			endwhile
		else
			nnoremap <buffer> h :call <SID>MoveSelect( line(".")-1 )<CR>
			nnoremap <buffer> k :call <SID>MoveSelect( line(".")-1 )<CR>
			nnoremap <buffer> l :call <SID>MoveSelect( line(".")+1 )<CR>
			nnoremap <buffer> j :call <SID>MoveSelect( line(".")+1 )<CR>
			nnoremap <buffer> q :call <SID>CloseWindow()<cr>
		endif
	endif

	" Reset incremental search
	let s:srch = ""

	" Fill attributs list
	call s:ShowAttributes()

	" User don't need to modify content
	setlocal nomodifiable
endfunction

"**
" SearchAttributes:
" Look for attributs for word under cursor. tr
"**
function! s:SearchAttributes()
	" Ignore case
	let l:CurrentCase = &ignorecase
	set ignorecase
	let l:CurrentIkw = &iskeyword
	
	let s:attributs = ""
	let s:maxAttrLength = 0
	let s:listSep = ","
	
	set iskeyword +=<
	if match( expand("<cword>"), "^<" ) == 0 " Is it a tag ?
		let s:itemtype = "T" " Yes, a tag
		let l:attributsLine = GetListMatchItem( s:HTMLTags, expand("<cword>") . ' ' )
	else
		set iskeyword -=<
		set iskeyword +==
		if match( expand("<cword>"), "=$" ) " or an attribute ?
			let s:itemtype = "A" " Yes, an attribute
			let l:attributsLine = GetListMatchItem( s:TagsAttributs, expand("<cword>") . ' ' )
		endif
	endif

	if l:attributsLine != ""
		let s:listSep = " "
		let l:attributsLine = RemoveListItem( l:attributsLine, 0 )
		if l:attributsLine != ""
			" Insert %xxxx variables content
			let l:attribut = matchstr( l:attributsLine, '%[^ ]\+' )
			while l:attribut != ""
				exe "let l:attributsLine = substitute( l:attributsLine, '" .l:attribut. "', s:" . strpart( l:attribut, 1 ) . ", '')"
				let l:attribut = matchstr( l:attributsLine, '%[^ ]\+' )
			endwhile
			
			let l:attributsLine = QuickSortList( l:attributsLine, 0, GetListCount(l:attributsLine)-1 )

			let l:attribut = GetListItem( l:attributsLine, 0 )
			while l:attribut != ""
				" Keep max length
				if s:maxAttrLength < strlen( l:attribut )
					let s:maxAttrLength = strlen( l:attribut )
				endif
				" Remove current attribut
				let l:attributsLine = RemoveListItem( l:attributsLine, 0 )
				" Is it a depracated attribute ?
				if l:attribut =~ "-D"
					let l:attribut = substitute( l:attribut, "-D", "", "")
					let s:attributs = s:attributs . "X" . l:attribut . " \n"
				else
					let s:attributs = s:attributs . " " . l:attribut . " \n"
				endif
				" Next attribut
				let l:attribut = GetListItem( l:attributsLine, 0 )
			endwhile
		endif
		" If longest attribute size is zero, then there is no attribute
		" for this tag
		if s:maxAttrLength != 0
			let s:maxAttrLength = s:maxAttrLength + 2
		endif
	endif
	let &ignorecase = l:CurrentCase
	let &iskeyword = l:CurrentIkw
endfunction

"**
" ShowAttributes:
" Display attributs list in current buffer.
"**
function! s:ShowAttributes()
	" Prevent a report of our actions from showing up
	let oldRep=&report
	let save_sc = &sc
	set report=10000 nosc

	setlocal modifiable
	" Erase content
	%delete

	" Put content of register f after the cursor
	put! =s:attributs

	" Erase last line
	exe "normal G"
	d
	" Move to first item
	call s:MoveSelect(1)
	set nomodifiable
	
	" Restore config
	let &report=oldRep
	let &sc = save_sc
endfunction

"**
" AddAttribute:
" Add selected attribut at end of tag.
"**
function! s:AddAttribute()
	" Get attribute
	let save_f=@f
	let @f = substitute( getline("."), "[ <>()X]", "", "g" )
	" Go to previous window
	wincmd p
	" if it's a tag, put attribute at end of tag
	if s:itemtype == "T"
		let @f = ' ' . @f
		exec 'normal f>"fP'
	elseif s:itemtype == "A" " If it's an attribute, add value to it
		exec 'normal f=l"fgp'
	endif
	startinsert
	" Return to attributes window
	wincmd p
	let @f=save_f
	" Close window
	call s:CloseWindow()
endfunction

"**
" MoveSelect:
" Move highlight to line newLineNumber.
" 
" Parameter:
" newLineNumber line number to highlight.
"**
function! s:MoveSelect( newLineNumber )
	if( a:newLineNumber < 1 || a:newLineNumber > line("$") )
		return
	endif

	setlocal modifiable

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
	let len = strlen(l:modifiedLine)
	while len < s:maxAttrLength
		let l:modifiedLine = l:modifiedLine . " "
		let len = len + 1
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
	setlocal nomodifiable
endfunction

"**
" CloseWindow:
" Clear unused variables and highlights, reinit variables for next use and
" close current window.
"**
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

"**
" GetBgColor:
" Try to get background color (may be not sure)
"**
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

let s:srch = ""

"**
" SelectSearch:
" Used for incremental attribut search
" 
" Parameter:
" Char	character to add to current search pattern
"**
function! s:SelectSearch( char )
	if a:char == "" && s:srch != ""
		let s:srch = strpart( s:srch, 0, strlen( s:srch )-1 )
	else
		let s:srch = s:srch . a:char
	endif
	let linenr = line(".")
	if s:srch != ""
		1
		let findlinenr =  search( '\(\<\|X\)'.s:srch, "W" )
		exe ":".linenr
		if findlinenr != 0
			echo "Attributs search : " . s:srch
			call s:MoveSelect( findlinenr )
		else
			echohl ErrorMsg
			echo "No attribut for \"" . s:srch . "\" (use backspace)"
			echohl NONE
		endif
	endif
endfunction
