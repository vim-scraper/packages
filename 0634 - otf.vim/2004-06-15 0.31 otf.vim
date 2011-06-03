"	vim:ff=unix ts=4 ss=4
"	vim60:fdm=marker
"	\file		otf.vim
"	\date		Sat, 12 Jun 2004 21:42 PST
"
"	\brief		On-The-Fly coloring of patterns, Inspired heavily by
"					MultipleSearch.vim (2002 Nov 18, v1.1) by Dan Sharp
"					<dwsharp at hotmail dot com>, this version allows specific
"					colors for specific patterns and easy to redefine a
"					pattern for a color, or uncolor a pattern.
"	\note		This is an evolution of my (unpublished) FERAL_MatchWord (that
"					used :match)
"	\note		This is VIMSCRIPT#634,
"					http://vim.sourceforge.net/scripts/script.php?script_id=634.
"	\note		MultipleSearch.vim is VIMSCRIPT#479,
"					http://vim.sourceforge.net/scripts/script.php?script_id=479.
"
"	\author		Robert KellyIV <sreny@rpyrpgvpjvmneq.pbz> (Rot13ed)
"	\version	$Id$
"	Version:	0.31
"	History:	{{{
"	[Feral:164/04@20:58] 0.31
"	Refinement of :OTF -- just bank (with no keyword) will clear the specified
"		group, i.e. ":2OTF!" will clear group 2 as will ":2OTF"
"	BugFix of :OTFG -- stored pattern no longer is surounded by '/' and
"		requires one param now (the register);
"	[Feral:163/04@00:16] 0.3
"	Improvement: :[#]OTFG[!] <register>
"		which will place the pattern used by the group referanced by [#] into
"		register <register>. This means that you can ":1OTFG y" to copy the
"		search pattern used by highlight group 1 into register y.
"		Added per request of Alexandre Rafalovitch <nensnybi@orn.pbz> (Rot13ed)
"	[Feral:100/04@17:36] 0.221
"	BUGFIX: tiny fix to OTFReset, was not clearing the last color
"	[Feral:151/03@01:47] 0.22
"		Little bit of clean up and started to make it a proper plugin; still
"			needs documtation. Undecided about <plug> and :menu.
"		by the by you can ":nmap <leader>1 :1OTF! W" or the like in your
"			.vimrc, I am not seeing the point of providing default mappings
"			(with all the <plug> code, etc.) at the moment.
"	[Feral:127/03@01:38] 0.21
"		Improvement: As per a suggestion from Mathieu CLABAUT the feralotf#
"		highlight groups now use the default keyword, This allows you to
"		override these default colors easily.
"
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
"
"	Defining Your Own Colors:
"	Thanks to a suggestion by Mathieu CLABAUT the default highlight groups use
"		the default keyword, this allows you to specify feralotf# to override
"		the default color group. See :h :highlight-default
"
"	Thus you could have something like this in your favorite color file to
"		override the first 4 color groups.
" {{{ OVERRIDE On the fly highlight groups.
"hi feralotf1		guifg=#4682B4 guibg=Linen
"hi feralotf2		guifg=Linen
"hi feralotf3		guifg=Linen guibg=azure4
"hi feralotf4		guifg=Linen guibg=PaleGreen4
" }}}
"
"	}}}

if exists("loaded_otf")
	finish
endif
let loaded_otf = 1

let s:save_cpo = &cpo
set cpo&vim



" {{{ On the fly match groups.
" [Feral:127/03@01:35]  As per a suggestion from Mathieu CLABAUT these
"	highlight groups now use the default keyword, see :h :highlight-default.
"	This allows you to define these groups in your syntax file (or wherever)
"	to override these default colors. Neat!
highlight default feralotf1		guifg=white guibg=red
highlight default feralotf2		guifg=black guibg=yellow
highlight default feralotf3		guifg=white guibg=blue
highlight default feralotf4		guifg=black guibg=green
highlight default feralotf5		guifg=white guibg=magenta
highlight default feralotf6		guifg=black guibg=cyan
highlight default feralotf7		guifg=black guibg=gray
highlight default feralotf8		guifg=white guibg=brown
" }}}

function s:FERAL_OTF_GroupInBounds(p_Group) " {{{
	if a:p_Group < 1 || a:p_Group > 8
		return 0
	else
		return 1
	endif
endfunction " }}}

"	If bang is specified (:OTF!) then treat p_String as a special char to be
"		expanded (presumably a register)
"
"	w: <c-r><c-w>
"	W: \<<c-r><c-w>\> (that is to say, current cword as a word.
"	": <c-r>"
"	/:<c-r>/

function s:FERAL_OTF(p_Group, p_Bang, p_String) " {{{

"	echo confirm("p_Group: ".a:p_Group."\n"."p_String: ".a:p_String)

	" Gate: make sure our group number is in bounds.
	if !s:FERAL_OTF_GroupInBounds(a:p_Group)
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
		elseif a:p_String == "" " blank p_String, clear the group
			let Word = ""
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
		echo "OTF: Group #".a:p_Group.' set to "'.Word.'".'
	else
		echo "OTF: Group #".a:p_Group.' cleared.'
	endif

endfunction " }}}

" Loop clear out all of this highlighting.
function s:FERAL_OTFReset() " {{{

	" clear the highligh group for each of our colors
	let NumGroups = 8
	let Index = 1	" we start at group index 1.
	while	Index <= NumGroups
		execute 'silent syntax clear feralotf'.Index
		let Index = Index + 1
	endwhile
	echo "OTF: Cleared all ".NumGroups." color groups"

endfunction " }}}

" [Feral:162/04@23:05] FERAL_OTF_Get_To_Register
" \brief	Place the search pattern for the specified group (p_Group) into
"			the specified register (p_RegisterToUse)
" \note		This is per request of and for Alexandre Rafalovitch
"			<nensnybi@orn.pbz> (Rot13ed)
"
" If bank then will clear the group specified by p_Group
"
" The request, for my referance:
" ... I would like to be able to copy the pattern out of OTF into a register.
" That way, I could mark several patterns and then at any point retrieve one
" of the marked patterns, modify it and then redo the marking. Listing of all
" patterns and colors would have been nice too, but less essential.
function s:FERAL_OTF_Get_To_Register(p_Group, p_Bang, p_RegisterToUse) " {{{

	" Gate: make sure our group number is in bounds.
	if !s:FERAL_OTF_GroupInBounds(a:p_Group)
		return
	endif

"hlexists( {name})		Number	TRUE if highlight group {name} exists
"hlID( {name})			Number	syntax ID of highlight group {name}
"echo synIDattr(hlID("feralotf2"), "bg")
":h syn-list
":syn list feralotf1
":h redir


	let Was_Reg_z = @z

	redir @z
		execute 'silent syntax list feralotf'.a:p_Group
		" HACK:	seems the first echo after a redir will be up one line, this
		"		echo seems to fix that. aka HACK.
		echo ' '
	redir END
	let SynList = @z

	let @z = Was_Reg_z
	unlet Was_Reg_z

"	echo SynList


	"See: :h scanf
	" Set up the match bit
	let mx='\<match\>\s*\/\(.*\)\/\s*\<containedin\>'
	"get the part matching the whole expression
	let l = matchstr(SynList, mx)
	"get each item out of the match
	let Pattern = substitute(l, mx, '\1', '')
"	echo Pattern


	execute "let @".a:p_RegisterToUse."='".Pattern."'"
	echo "OTF: Group #".a:p_Group."'s pattern, \"".Pattern."\", stored in @".a:p_RegisterToUse."."


	" if we are banged then clear the group
	if a:p_Bang == "!"
		execute 'silent syntax clear feralotf'.a:p_Group
		echo 'OTF: Group #'.a:p_Group.' cleared.'
	endif

endfunction " }}}



"///////////////////////////////////////////////////////////////////////////
"// {{{ -[ Commands ]-------------------------------------------------------
"///////////////////////////////////////////////////////////////////////////

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
"	() CLEAR Specifed group.	-- [Feral:164/04@21:01]
:command -range=1 -bang -nargs=? OTF		call <SID>FERAL_OTF(<count>, "<bang>",<q-args>)
:command -range=1 -bang -nargs=1 OTFG		call <SID>FERAL_OTF_Get_To_Register(<count>, "<bang>",<q-args>)

":command -bang -nargs=? OTF1		call <SID>FERAL_OTF(1, "<bang>", <q-args>)
":command -bang -nargs=? OTF2		call <SID>FERAL_OTF(2, "<bang>", <q-args>)
":command -bang -nargs=? OTF3		call <SID>FERAL_OTF(3, "<bang>", <q-args>)
":command -bang -nargs=? OTF4		call <SID>FERAL_OTF(4, "<bang>", <q-args>)
":command -bang -nargs=? OTF5		call <SID>FERAL_OTF(5, "<bang>", <q-args>)
":command -bang -nargs=? OTF6		call <SID>FERAL_OTF(6, "<bang>", <q-args>)
":command -bang -nargs=? OTF7		call <SID>FERAL_OTF(7, "<bang>", <q-args>)
":command -bang -nargs=? OTF8		call <SID>FERAL_OTF(8, "<bang>", <q-args>)


"// }}} --------------------------------------------------------------------


"///////////////////////////////////////////////////////////////////////////
"// {{{ -[ Mappings ]-------------------------------------------------------
"///////////////////////////////////////////////////////////////////////////

" [Feral:151/03@01:45] NOTE you can just add something like this in your .vimrc:
"nmap <leader>1		:1OTF! W<cr>
"nmap <leader>2		:2OTF! W<cr>
"nmap <leader>3		:3OTF! W<cr>
"nmap <leader>4		:4OTF! W<cr>
"nmap <leader>5		:5OTF! W<cr>
"nmap <leader>6		:6OTF! W<cr>
"nmap <leader>7		:7OTF! W<cr>
"nmap <leader>8		:8OTF! W<cr>
"nmap <leader>0		:OTFReset<cr>

"
"
""noremap <unique> <script> <Plug>OTFPlug_OTF1	<SID>PlugOTF1
""noremap <SID>PlugOTF1				:call <SID>FERAL_OTF(1, "!", "W")<cr>
"
""noremap <unique> <script> <Plug>OTFPlug_OTF1	:call <SID>FERAL_OTF(1, "!", "W")<cr>
"
"noremap	<unique>	<script>	<Plug>OTFPlug_OTF1		:1OTF! W<cr>
"noremap	<unique>	<script>	<Plug>OTFPlug_OTF2		:2OTF! W<cr>
"noremap	<unique>	<script>	<Plug>OTFPlug_OTF3		:3OTF! W<cr>
"noremap	<unique>	<script>	<Plug>OTFPlug_OTF4		:4OTF! W<cr>
"noremap	<unique>	<script>	<Plug>OTFPlug_OTF5		:5OTF! W<cr>
"noremap	<unique>	<script>	<Plug>OTFPlug_OTF6		:6OTF! W<cr>
"noremap	<unique>	<script>	<Plug>OTFPlug_OTF7		:7OTF! W<cr>
"noremap	<unique>	<script>	<Plug>OTFPlug_OTF8		:8OTF! W<cr>
"noremap	<unique>	<script>	<Plug>OTFPlug_OTFReset	:OTFReset<cr>
"
"
"if !hasmapto('<Plug>OTFPlug_OTF1')
"	map	<unique>	<Leader>o1	<Plug>OTFPlug_OTF1
"endif
"if !hasmapto('<Plug>OTFPlug_OTF2')
"	map	<unique>	<Leader>o2	<Plug>OTFPlug_OTF2
"endif
"if !hasmapto('<Plug>OTFPlug_OTF3')
"	map	<unique>	<Leader>o3	<Plug>OTFPlug_OTF3
"endif
"if !hasmapto('<Plug>OTFPlug_OTF4')
"	map	<unique>	<Leader>o4	<Plug>OTFPlug_OTF4
"endif
"if !hasmapto('<Plug>OTFPlug_OTF5')
"	map	<unique>	<Leader>o5	<Plug>OTFPlug_OTF5
"endif
"if !hasmapto('<Plug>OTFPlug_OTF6')
"	map	<unique>	<Leader>o6	<Plug>OTFPlug_OTF6
"endif
"if !hasmapto('<Plug>OTFPlug_OTF7')
"	map	<unique>	<Leader>o7	<Plug>OTFPlug_OTF7
"endif
"if !hasmapto('<Plug>OTFPlug_OTF8')
"	map	<unique>	<Leader>o8	<Plug>OTFPlug_OTF8
"endif
"if !hasmapto('<Plug>OTFPlug_OTFReset')
"	map	<unique>	<Leader>or	<Plug>OTFPlug_OTFReset
"endif
"
""[Feral:151/03@01:35] Example mapping of using color 1 with search register.
""	It is the same idea for the other OTF keywords
""noremap	<unique>	<script>	<Plug>OTFPlug_OTFS1		:1OTF! /<cr>
""if !hasmapto('<Plug>OTFPlug_OTFS1')
""	map	<unique>	<Leader>/1	<Plug>OTFPlug_OTFS1
""endif

"// }}} --------------------------------------------------------------------


let &cpo = s:save_cpo
"// -- if you have ever had a mess of crosslinked files You'd mark you eof too! :)
"///EOF
