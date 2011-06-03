"	vim:ff=unix ts=4 ss=4
"	vim60:fdm=marker
"	\file		otf.vim
"
"	\brief		On-The-Fly coloring of patterns, Inspired heavily by MultipleSearch.vim (2002 Nov 18, v1.1) by Dan Sharp <dwsharp at hotmail dot com>, this version allows specific colors for specific patterns and easy to redefine a pattern for a color, or uncolor a pattern.
"	\note		This is an evolution of my FERAL_MatchWord (that used :match)
"	\note		This is VIMSCRIPT#634, http://vim.sourceforge.net/scripts/script.php?script_id=634.
"	\note		MultipleSearch.vim is VIMSCRIPT#479,
"				http://vim.sourceforge.net/scripts/script.php?script_id=479.
"
"	\author		Robert KellyIV <Sreny@SverGbc.Pbz> (Rot13ed)
"	\date		Sat, 03 May 2003 12:23 Pacific Daylight Time
"	\version	$Id$
"	Version:	0.2
"	History:	{{{
"	[Feral:123/03@12:06] 0.2
"		Small update: converted :OTF1-8 into :OTF which takes a count (line
"			number position) and takes optional bang to specify string as a
"			keyword (think register)
"		Keywords are:
"			(w) cword,                     expand("<cword>")
"			(W) \<cword\>                  "\\<".expand("<cword>")."\\>"
"			(") unnamed register           @@
"			(/) current search string      @/
"		i.e. ":2OTF! /" (bang) will pull in the current search string and
"			color it in color group2, while ":2OTF /" (no bang) will color '/'
"			in color group2
"		Original :OTF1-8 commands updated to bang style but commented out
"			(just in case anyone liked that method(no count))
"
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
"	There are 2 commands, 1 to clear all highlighting, 1 to manipulate
"		individule color groups.
"	:OTFReset -- to clear all highlighting done by this script. (think :noh)
"		This can probably be shortened to :OTFR on the command line.
"
"	:OTF -- takes a count (line number position) to specify the color group to
"		use (1-8). blank string clears highlighting for the specified color
"		group, while the bang (:OTF!) denotes that the specifed string should
"		be treated as a keyword (think register), see below:
"
"	Use Like So:
"		Highlight Current Word Under The Cursor:
"		:1OTF <c-r><c-w>
"		:1OTF! w
"		Highlight Current Search Pattern:
"		:1OTF <c-r>/
"		:1OTF! /
"		Use A Pattern From A Register:
"		:1OTF <c-r>"
"		:1OTF! "
"		Just Type The Pattern In Manually:
"		:1OTF \cotf\d
"		Turn Off Coloring For The First Highlight Group:
"		:1OTF
"		:1OTF!
"		Turn Off Coloring For The Second Highlight Group: (etc.)
"		:2OTF
"		:2OTF!
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

"	If bang is specified (:OTF!) then treat p_String as a special char to be
"		expanded (presumably a register)
"
"	w: <c-r><c-w>
"	W: \<<c-r><c-w>\> (that is to say, current cword as a word.
"	": <c-r>"
"	/:<c-r>/

function s:FERAL_OTF(p_Group, p_Bang, p_String)

"	echo confirm("p_Group: ".a:p_Group."\n"."p_String: ".a:p_String)

	" Gate: make sure our group number is in bounds.
	if a:p_Group < 1 || a:p_Group > 8
		return
	endif

	" Clear the previous highlighting for this group
	execute 'silent syntax clear feralotf'.a:p_Group


	let Word = a:p_String
	if a:p_Bang == "!"
		" default to cword if no param is given.
		if a:p_String == "w"
			let Word = expand("<cword>")
		elseif a:p_String == "W"
			let Word = "\\<".expand("<cword>")."\\>"
		elseif a:p_String == "/"
			let Word = @/
		elseif a:p_String == "\""
			let Word = @@
		endif
	endif

	" [Feral:122/03@21:15] As the string we search for is passed directly to
	" syn match, we can do normal syn match patterns.. which is to say \c or
	" \C for case specification, \<\> for word delimitors, etc.

"	if a:p_String != ""
	if Word != ""
		" we have a string to highlight, do so.
"		execute 'syntax match feralotf'.a:p_Group.' "'.a:p_String.'" containedin=ALL'
		execute 'syntax match feralotf'.a:p_Group.' "'.Word.'" containedin=ALL'
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
:command -nargs=0 OTFReset	call <SID>FERAL_OTFReset()
"	-count=N    A count (default N) which is specified either in the line
"		    number position, or as an initial argument (like |:Next|)
"		    Specifying -count (without a default) acts like -count=0
" [Feral:123/03@11:24] -count did not seem to want to work, so using -range
"	... was complaing about no range allowed, so I replaced -count=1 with
"	-range=1 (probably because of <q-args>, at a guess.)
"[Feral:123/03@10:33] Shortcut commands for:
"	(w) cword,					expand("<cword>")
"	(W) \<cword\>				"\\<".expand("<cword>")."\\>"
"	(") unnamed register		@@
"	(/) current search string	@/
:command -range=1 -bang -nargs=? OTF		call <SID>FERAL_OTF(<count>, "<bang>",<q-args>)

":command -bang -nargs=? OTF1		call <SID>FERAL_OTF(1, "<bang>", <q-args>)
":command -bang -nargs=? OTF2		call <SID>FERAL_OTF(2, "<bang>", <q-args>)
":command -bang -nargs=? OTF3		call <SID>FERAL_OTF(3, "<bang>", <q-args>)
":command -bang -nargs=? OTF4		call <SID>FERAL_OTF(4, "<bang>", <q-args>)
":command -bang -nargs=? OTF5		call <SID>FERAL_OTF(5, "<bang>", <q-args>)
":command -bang -nargs=? OTF6		call <SID>FERAL_OTF(6, "<bang>", <q-args>)
":command -bang -nargs=? OTF7		call <SID>FERAL_OTF(7, "<bang>", <q-args>)
":command -bang -nargs=? OTF8		call <SID>FERAL_OTF(8, "<bang>", <q-args>)


"// -- if you have ever had a mess of crosslinked files You'd mark you eof too! :)
"///EOF
