" {{{ File header
" vim:ff=unix ts=4 ss=4
" vim60:fdm=marker
"
" \file		expand.vim
" \date		Mon, 27 Oct 2003 17:04 PST
"
" \brief	A method to simply an quickly post expand a cmd.
"			"index ff<c-cr>" for example. (<c-cr> being mapped to this cmd.)
"
"			NOTE this is a QUICKY!
"			For what it is worth, hopefully useful to someone :)
"
"			The reason for this over a basic mapping, or DrC.Stubs is the
"			ability to specify the variable(s) to work on.
"
"			I.e.
"	iInd < m_array.size() for<c-cr>
"
"	for	(	iInd = 0; iInd < m_array.size(); iInd++)
"	{
"		<cursor>
"	}
"
"			or
"	m_array ff<c-cr>
"
"	for(int i=0;i<m_array.length; i++){
"		<cursor>
"	}//end for loop over array m_array[i]
"
"			NOTE that the command function shold leave the cursor in a
"			position that a normal mode `o` will place the cursor at the
"			desired location. Kind of a HACK but good enough for now.
"
" \note		This is VIMSCRIPT#«»:
"			URL:	http://vim.sourceforge.net/scripts/script.php?script_id=«»
"
" \author	Robert KellyIV <Sreny@SverGbc.Pbz> (Rot13ed)
" \note		No copyright claimed, inspired by VIM-TIP #593: basic postfix abbreviations.
" \version	$Id$
" \version	0.1
" History:
"	[Feral:300/03@17:00] 0.1
"		Initial.
" }}}

if exists("loaded_expand")
	finish
endif
let loaded_expand = 1

let s:save_cpo = &cpo
set cpo&vim

" {{{ Tip #593: basic postfix abbreviations
" tip karma  	 Rating 5/2, Viewed by 154 
"
"created: 	  	October 26, 2003 3:24 	     	complexity: 	  	basic
"author: 	  	leadhyena_inrandomtan 	     	as of Vim: 	  	6.0
"
"     I'm learning VIM in order to be more competitive in online programming
"     competitions where speed of accurate typing is a factor. This may be a
"     basic tip for some more advanced VIM users out there. I'm very used to
"     editors that provide prefix abbreviation expansion; for example, in
"     jEdit I'd type FA,array,<C-ENTER> in order to make a basic for loop that
"     scanned the array. I wanted this in VIm as well, but found that it was
"     harder to program. When I discovered that I could use a postfix
"     abbreviation instead of a prefix one, I realized that I really didn't
"     need any programming, but rather a long :ab statement like this:
"
""this is for java, c++,c# can reshape as necessary
":ab ff <ESC>^d$ifor(int i=0;i<<ESC>pi.length;i++){<CR><CR>}//end for loop over array <ESC>pi[i]<ESC>==k==k==ji<TAB>
"
"this way, if I need a loop over the array lines[] then I would type
"lines<SPACE>ff<SPACE> and vim would transform this into (with proper indentation)
"
"for(int i=0;i<lines.length;i++){
"        <CURSOR>
"}//end for loop over array lines[i]
"
"similar abreviations with multiple arguments could be delimited by spaces and
"could be yanked into multiple registers and plunked down as necessary. You
"could even use a similar structure to writing abbreviations in jEdit. But,
"this opens up possibilities for many different things, because VIM allows you
"to map real commands into the abbreviations, instead of just vanilla text
"like in jEdit.
"
" }}}


"*****************************************************************
" Functions: {{{

" index condition stop_value for
" i.e.:
" iInd < m_array.size() for
function s:expand_do_for(DaLine) "{{{

"	echo "DaLine:".a:DaLine

	" have to love premade examples, thanks :h sscanf !
	:" Set up the match bit
	:let mx='\<\([^ ]\+\) \([^ ]\) \<\([^ ]\+\) ' "for
	:"get the part matching the whole expression
	:let l = matchstr(a:DaLine, mx)
	:"get each item out of the match
	:let DaIndex		= substitute(l, mx, '\1', '')
	:let DaCondition	= substitute(l, mx, '\2', '')
	:let DaStopVal		= substitute(l, mx, '\3', '')

"	echo "SO:EXPAND_do_for"
"	echo "DaIndex:".DaIndex
"	echo "DaCondition:".DaCondition
"	echo "DaStopVal:".DaStopVal
"	echo "EO:EXPAND_do_for"


	let Was_Reg_z = @z

	let @z =    "for\t(\t".DaIndex." = 0; ".DaIndex." ".DaCondition." ".DaStopVal."; ".DaIndex."++)\n"
	let @z = @z."{\n"
	let @z = @z."}"

"	put z
"	execute "normal! S\<c-r>z\<esc>kA\<cr>."
	execute "normal! S\<c-r>z\<esc>k$"

	let @z = Was_Reg_z
	unlet Was_Reg_z

endfunction
" }}}

function s:expand_do_ff(DaLine) "{{{

"	echo "DaLine:".a:DaLine

	" have to love premade examples, thanks :h sscanf !
	:" Set up the match bit
	:let mx='\<\([^ ]\+\) ' "for
	:"get the part matching the whole expression
	:let l = matchstr(a:DaLine, mx)
	:"get each item out of the match
	:let DaVar		= substitute(l, mx, '\1', '')

"	echo "SO:EXPAND_do_ff"
"	echo "DaVar:".DaVar
"	echo "EO:EXPAND_do_ff"


	let Was_Reg_z = @z

""this is for java, c++,c# can reshape as necessary
":ab ff <ESC>^d$ifor(int i=0;i<<ESC>pi.length;i++){<CR><CR>}//end for loop over array <ESC>pi[i]<ESC>==k==k==ji<TAB>
"
"this way, if I need a loop over the array lines[] then I would type
"lines<SPACE>ff<SPACE> and vim would transform this into (with proper indentation)
"
"for(int i=0;i<lines.length;i++){
"        <CURSOR>
"}//end for loop over array lines[i]

	let @z =    "for(int i=0;i<".DaVar.".length; i++){\n"
	let @z = @z."}//end for loop over array ".DaVar."[i]"

"	put z
"	execute "normal! S\<c-r>z\<esc>kA\<cr>"
	execute "normal! S\<c-r>z\<esc>k$"

	let @z = Was_Reg_z
	unlet Was_Reg_z

endfunction
" }}}


function s:expand_start_expansion() "{{{

" Line being something like:
"	index ff<cursor>

"	let DaWord = expand("<cWORD>") " a <cWORD> matches 'while()'.
	let DaWord = expand("<cword>") " a <cword> matches 'while'.
	let DaLine = getline(".")

"	echo "SO:EXPAND_start"
"	echo "DaLine:".DaLine
"	echo "DaWord:".DaWord
"	echo "EO:EXPAND_start"

	" If you want case sensitive REM the tolower call below and define your
	" function commands acordingly.
	let DaWord = tolower(DaWord)
	if strlen(DaWord) > 0
		call <sid>expand_do_{DaWord}(DaLine)
	else
		echo "EXPAND: invalid command word(".DaWord."), forgot it?"
	endif

endfunction
"}}}

" }}}

"*****************************************************************
" Commands: {{{
"*****************************************************************

noremap	<unique>	<script>	<Plug>ExpandPlug_Do		:call <sid>expand_start_expansion()<cr>

if !hasmapto('<Plug>ExpandPlug_Do')
"	imap	<unique>	<C-CR>	<c-o><Plug>ExpandPlug_Do
	imap	<unique>	<C-CR>	<esc><Plug>ExpandPlug_Do<bar>o
endif

"}}}
let &cpo = s:save_cpo
"EOF

