"	vim:ff=unix ts=4 ss=4
"	vim60:fdm=marker
"	\file		otf.vim
"
"	\brief		On-The-Fly coloring of patterns, Inspired heavily by MultipleSearch.vim (2002 Nov 18, v1.1) by Dan Sharp <dwsharp at hotmail dot com>, this version allows specific colors for specific patterns and easy to redefine a pattern for a color, or uncolor a pattern.
"	\note		This is an evolution of my FERAL_MatchWord (that used :match)
"	\note		This is VIMSCRIPT#«», «».
"	\note		MultipleSearch.vim is VIMSCRIPT#479,
"				http://vim.sourceforge.net/scripts/script.php?script_id=479.
"
"	\author		Robert KellyIV <Sreny@SverGbc.Pbz> (Rot13ed)
"	\note		Copyright (C) 2003 FireTop.Com
"	\date		Fri, 02 May 2003 22:46 Pacific Daylight Time
"	\version	$Id$
"	Version:	0.1
"	History:	{{{
"	[Feral:122/03@21:26] 0.1
"		Initial
"		Pieces from Dan Sharp's MultipleSearch.vim and base idea from my
"			(unpublished) FERAL_MatchWord.
"		This, happily, is what I wanted FERAL_MatchWord to be in the first
"			place.
"
"		There are bound to be ways to streamline the usage of this still,
"			for instance I normally :OTF1 <C-r><c-w> or <c-r>" and ocasionally
"			<c-r>/ ... lots of flexability but more typing than desired.
"	}}}
"
"	Usage: {{{
"	There are 9 commands, 8 are virtually the same:
"	:OTFReset -- to clear all highlighting done by this script. (think :noh)
"		This can probably be shortened to :OTFR on the command line.
"
"	There are 8 OTF\d commands (OTF1, OTF2 ... OTF8) each one operates with a
"		different color group (as specified by the trailing number in the
"		command). These highlight the specified search pattern with the
"		relevant color. If no search pattern is given the highlighting is
"		removed for this color.
"
"	Use Like So:
"		Highlight Current Word Under The Cursor:
"		:OTF1 <c-r><c-w>
"		Highlight Current Search Pattern:
"		:OTF1 <c-r>/
"		Use A Pattern From A Register:
"		:OTF1 <c-r>"
"		Just Type The Pattern In Manually:
"		:OTF1 \cotf\d
"		Turn Off Coloring For The First Highlight Group:
"		:OTF1
"		Turn Off Coloring For The Second Highlight Group: (etc.)
"		:OTF2
"		Turn Off Coloring For All Groups:
"		:OTFReset
"		Which can probably be shortened to:
"		:OTFR
"	}}}

if exists("loaded_otf")
	finish
endif
let loaded_otf = 1

" {{{ On the fly match groups.
" [Feral:122/03@21:08] I am not sure how to determin if a highlight group
"	already exists so we are blindly always defining the color groups here.
" If there is no way to determin if a color group already exists I see two
"	options; require these hilights in the color file (lots of flexability
"	each colorscheme could have it's own highlight coloring, ofcourse). Or use
"	a global var to to determin if we should define our default highlight
"	groups here.
" We could do both really.
"highlight feralotf1		guifg=#4682B4 guibg=Linen
"highlight feralotf2		guifg=Linen  guibg=black
"highlight feralotf3		guifg=Linen guibg=azure4
"highlight feralotf4		guifg=Linen guibg=PaleGreen4

highlight feralotf1		guifg=white guibg=red
highlight feralotf2		guifg=black guibg=yellow
highlight feralotf3		guifg=white guibg=blue
highlight feralotf4		guifg=black guibg=green
highlight feralotf5		guifg=white guibg=magenta
highlight feralotf6		guifg=black guibg=cyan
highlight feralotf7		guifg=black guibg=gray
highlight feralotf8		guifg=white guibg=brown

" }}}


function s:FERAL_OTF(p_Group, p_String)

"	echo confirm("p_Group: ".a:p_Group."\n"."p_String: ".a:p_String)

	" Clear the previous highlighting for this group
	execute 'silent syntax clear feralotf'.a:p_Group

	" [Feral:122/03@21:15] As the string we search for is passed directly to
	" syn match, we can do normal syn match patterns.. which is to say \c or
	" \C for case specification, \<\> for word delimitors, etc.

	if a:p_String != ""
		" we have a string to highlight, do so.
		execute 'syntax match feralotf'.a:p_Group.' "'.a:p_String.'" containedin=ALL'
	endif

endfunction

" Loop clear out all of this highlighting.
function s:FERAL_OTFReset()

	" clear the highligh group for each of our colors
	let NumGroups = 8
	let Index = 1	" we start at group index 1.
	while	Index < NumGroups
		execute 'silent syntax clear feralotf'.Index
		let Index = Index + 1
	endwhile

endfunction

"*****************************************************************
"* Commands
"*****************************************************************
:command -nargs=? OTFReset	call <SID>FERAL_OTFReset()
:command -nargs=? OTF1		call <SID>FERAL_OTF(1,<q-args>)
:command -nargs=? OTF2		call <SID>FERAL_OTF(2,<q-args>)
:command -nargs=? OTF3		call <SID>FERAL_OTF(3,<q-args>)
:command -nargs=? OTF4		call <SID>FERAL_OTF(4,<q-args>)
:command -nargs=? OTF5		call <SID>FERAL_OTF(5,<q-args>)
:command -nargs=? OTF6		call <SID>FERAL_OTF(6,<q-args>)
:command -nargs=? OTF7		call <SID>FERAL_OTF(7,<q-args>)
:command -nargs=? OTF8		call <SID>FERAL_OTF(8,<q-args>)

"// -- if you have ever had a mess of crosslinked files You'd mark you eof too! :)
"///EOF
