"{{{ File header information
"	vim:ff=unix ts=4 ss=4
"	vim60:fdm=marker
"
"	\file		hlcontext.vim
"
"	\brief		based on VIMTIP#548: Using H and L keys as context sensitive pagedown/pageup
"
"	\author		Usman Latif -- original tip
"	\author		johann-guenter.simon@linde-le.com -- scrolloff tip
"	\author		Robert KellyIV <Sreny@SverGbc.Pbz> (Rot13ed) -- this mess
"	\date		Mon, 08 Sep 2003 01:23 PDT
"	\version	$Id$
"	Version:	0.1
"	History: {{{
"	[Feral:251/03@01:21] 0.1
"		Initial, based on VIMTIP#548.
" }}}
"
"}}}

if exists("loaded_hlcontext")
	finish
endif
let loaded_hlcontext = 1

let s:save_cpo = &cpo
set cpo&vim

"*****************************************************************
" Original TIP: {{{
"
"
" Tip #548: Using H and L keys as context sensitive pagedown/pageup
" tip karma  	 Rating 7/4, Viewed by 348 
"
"created: 	  	September 2, 2003 20:26 	     	complexity: 	  	basic
"author: 	  	Usman Latif 	     	as of Vim: 	  	5.7
"
"The H and L keys move the cursor to the top or bottom of the window
"respectively. They can be a real time saver, instead of hitting j/k many times,
"a single H/L can move the cursor to the proper place. However, when you are
"already at the top of the window the H key does nothing and similarly at the
"bottom of the window the L key does nothing.
"
"I started using the H/L keys a few days ago and quickly discovered that after
"getting to the top using H, I often want to scroll up. Hitting H again does
"nothing, so I wrote a function Hcontext which makes the H key context
"sensitive. I then mapped Hcontext to the H key.  Now hitting the H key anywhere
"other than at the top of the window leads to the usual behavior but hitting H
"at the first line of the window causes the window to scroll one page back and
"positions the cursor at the top of the window.  Similar behavior is implemented
"by the Lcontext function but in the other direction. Hitting L on the last line
"of the window now acts like the pagedown key.
"
"Even if you have never used the H/L keys before you can now start using them
"as replacement pagedown/pageup keys. Just cut and paste the code at the end
"into your vimrc and put the following maps after that.
"
"noremap H :call Hcontext()<CR>
"noremap L :call Lcontext()<CR>
"
"The unmapped H and L keys take a numeric count as well. Unfortunately, I am not
"aware of a way to make that count available to the user functions I wrote. The
"typical vim behavior in case of user functions is to supply the count as a
"range to the user function. This works most of the time but sometimes the count
"gets rejected because of range checking. If you are aware of a workaround
"please let me know.
"
"You can contact me by writing to latif@techuser.net. If you have suggestions as
"to other keys that can be made context sensitive without affecting their
"original function, email me. I also maintain a webpage where you can ask help
"for your text processing problems. The webpage is at http://www.techuser.net
"
"---------------------------------Cut Here----------------------------------------
"func! Hcontext()
"    if (winline() == 1 && line(".") != 1)
"        exe "normal! \<pageup>H"
"    else
"        exe "normal! H"
"    endif
"    echo ''
"endfunc
"
"func! Lcontext()
"    if (winline() == winheight(0) && line(".") != line("$"))
"        exe "normal! \<pagedown>L"
"    else
"        exe "normal! L"
"    endif
"    echo ''
"endfunc
"---------------------------------Cut Here----------------------------------------
" rate this tip  	Life Changing Helpful Unfulfilling 
"
"<<Smarter Table Editing | Switching normal and insert-mode disturbes cursorposition >>
"
"Additional Notes
"johann-guenter.simon@linde-le.com, September 3, 2003 9:22
"My suggestion for improvement is to take care of the scrolloff option
"My modification of the function:
"
"---------------------------------Cut Here----------------------------------------
"func! Hcontext()
"    if (winline() == &so+1 && line(".") != 1)
"        exe "normal! \<PageUp>H"
"    else
"        exe "normal! H"
"    endif
"    echo ''
"endfunc
"
"func! Lcontext()
"    if (winline() == winheight(0)-&so && line(".") != line("$"))
"        exe "normal! \<PageDown>L"
"    else
"        exe "normal! L"
"    endif
"    echo ''
"endfunc
"---------------------------------Cut Here----------------------------------------
" }}}


"*****************************************************************
" Functions: {{{

function s:HContext() "{{{
	if (winline() == &so+1 && line(".") != 1)
		exe "normal! \<PageUp>"
	endif
	exe "normal! H"
	echo ''
endfunction
"}}}

function s:LContext() "{{{
	if (winline() == winheight(0)-&so && line(".") != line("$"))
		exe "normal! \<PageDown>"
	endif
	exe "normal! L"
	echo ''
endfunction
"}}}

" }}}

"*****************************************************************
" Commands: (remed) {{{
"*****************************************************************
"if !exists(":HContext")
"	command		HContext			:call <SID>HContext()
"endif
"if !exists(":LContext")
"	command		LContext			:call <SID>LContext()
"endif
"}}}

"///////////////////////////////////////////////////////////////////////////
"// {{{ -[ Mappings ]-------------------------------------------------------
"///////////////////////////////////////////////////////////////////////////

noremap	<unique>	<script>	<Plug>HLContextPlug_H		:call <SID>HContext()<CR>
if !hasmapto('<Plug>HLContextPlug_H')
	map	<unique>	H	<Plug>HLContextPlug_H
endif
"noremap	<unique>	<script>	<Plug>HLContextPlug_H		<SID>HLContextPlug_DoH
"noremap	<SID>HLContextPlug_DoH	:call <SID>HContext()

noremap	<unique>	<script>	<Plug>HLContextPlug_L		:call <SID>LContext()<CR>
if !hasmapto('<Plug>HLContextPlug_L')
	map	<unique>	L	<Plug>HLContextPlug_L
endif
"noremap	<unique>	<script>	<Plug>HLContextPlug_L		:<SID>HLContextPlug_DoL
"noremap	<SID>HLContextPlug_DoL	:call <SID>LContext()

"// }}} --------------------------------------------------------------------


let &cpo = s:save_cpo
"EOF

