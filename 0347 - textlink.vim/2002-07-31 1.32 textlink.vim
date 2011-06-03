"	vim:ff=unix
"	vim60:fdm=marker
"	\file		textlink.vim
"
"	\brief		Easily load a file and search for a specified string. (handy
"				for notes in your code, etc.)
"				Invoke :TL on a line that contains something like, "|~\cvs.html|@|$EDITOR|"
"				or this... |T|@|" Examples:| ... try it! :TL on this line!
"				You can also :TLS if you do not want to split (S for string)
"
"				Otherwise known as a text based hyperlink.
"
"				This simply :splits the specified file, goes to the
"				top of said file, and seaches (down) for the specified text.
"				Ignoring the textlink's search text.
"
"	\author		Robert KellyIV <Feral@FireTop.Com>
"	\note		Comments and such welcome.
"	\date		Wed, 31 Jul 2002 14:05 Pacific Daylight Time
"	Version:	1.32
"	\note		Released into the Public Domain.
"	\version	$Id$
"
"	NOTE! If you like this general idea be sure to check out Stefan Bittner's
"		http://vim.sourceforge.net/scripts/script.php?script_id=293 thlnk.vim
"		as well.
"
" Index:	Just :TL or :TLS to hop to the entry.. (`` to return if you like)
"	|T|@|Usage:|
"	|T|@|ChangeLog:|
"	|T|@|Commands:|
"	|T|@|Colors:|
"	|T|@|Cautions:|
"	|T|@|Examples:|
"	|T|@|Examples That Dont Work:|
"	|T|@|Note:|
"	|T|@|Redundant Format Explination:|
"	|T|@|Known Bugs:|
"
"
" ChangeLog:
"	1.32
"	[Feral:212/02@14:04]
"	Cleaned up the color definitions and modified the help on what to do with
"		the textlink-fragment.vim file. Hopefully it will be easier to
"		understand what to do now if you've not messed with the syntax
"		highlighting much.
"
"	1.31
"	[Feral:210/02@14:25] fixed a !sigh! glaring bug. When the text link was
"	before the search string in the file it would break out prematurity.
"
"	1.3
"	[Feral:208/02@10:53] Added color definition entry in the help here (sort
"		of forgot it before) See: |T|@|Colors:| (:TLS this line)
"	Added :VTL :VTLS and :VTLF for vert splits instead of horiz splits.
"	Fixed a slight endless loop when the textlink was the only line with the
"	search string. Erm, oops.
"
"	1.2
"	[Feral:202/02@23:21] Well it's like this.. finding a textlink string in
"	another file would find the text link fine but broke out of the loop
"	before it went back to it. .. Changed logic of the while ~sigh~ knew I
"	should not have posted this after playing with it so reciently ~wink~
"
"	1.1
"	[Feral:202/02@12:34] Heh had just implimented searchstring method mark II
"	and just KNEW it was flawed. Heh much more confadent with MarkIII.
"	The problem was when there was more than one text link before the search
"	string we would always end up on the second textlink.
"	Fixed that by Changed the logic of finding the search string, seems fixed.
"	(only took 26 min too)
"
" Usage:
"	Sun Jun 16  Pacific Daylight Time 2002 changed the pattern, more
"	recognisable as a text link now, yet basicaly the same.
"	New: blabber |FileName|@|SearchString| blabber
"	FileName can be:
"		|A|
"			to use AlternateFile (from a.vim) (see below)
"		|T|
"			to use the current (T for this) file., i.e. split and search.
"
"		If FileName is not found, a new file is created. i.e. :e FileName
"
"
"	SearchString can be:
"		Any valid search string I believe, though I am not so sure about
"		regular expresions come to think of it.. I just seach for normal text.
"
"	Neither FileName nor SearchString are allowed to be blank. basicaly
"	because a textlink is not so meaning full with out both the filename and
"	searchstring. ALso makes parsing the textlink a little less percise.
"
" Commands:
"		:TTL
"			Test text link. (echos the found FileName and SearchString)
"		:TL
"			:split FileName, gg, /SearchString (basicaly).
"			This is to say splits the current window and loads the specified
"			file name then searches for the specified SearchString from the
"			top of the file.
"		:TLF
"			Like :TL save ignores the SearchString... basicaly a short cut to
"			:split
"		:TLS
"			Like :TL save ignores the FileName... usefull when FileName is |T|
"			(this file) and you do not want to split the window.
"		:VTL
"			Same as :TL but :vsplit instead of :split
"		:VTLF
"			Same as :TLF but :vsplit instead of :split
"		:VTLS
"			Same as :TLS but :vsplit instead of :split
"
" Colors:
" {{{
"	NOTE: textlink-fragment.vim is embeded in this file, see:
"		|T|@|textlink-fragment.vim:|
"
"	1) copy textlink-fragment.vim out of this file (it is embeded in this file
"		in the fold below, see |T|@|textlink-fragment.vim:| and just save that
"		fold to a new file called textlink-fragment.vim.
"
"	2) Place textlink-fragment.vim in your ~/.vim/syntax or
"		$VIM/vimfiles/syntax directory.
"
"	3) Goto your syntax directory (~/.vim/syntax or $VIM/vimfiles/syntax) and
"		choose which files you want textlink highlighting to appear in, i.e. c
"		files and say txt files. (as an example) (So for example the files we
"		want are: c.vim and txt.vim)
"
"	4) Add these three lines to the file(s), before comment groups i.e. BEFORE
"		the line that reads: "syn cluster	cCommentGroup	contains=" for c
"		files.:
"			" syntex coloring for textlinks.
"			source $VIM/vimfiles/syntax/textlink-fragment.vim
"			"source ~/.vim/syntax/textlink-fragment.vim
"
"		That will define feralTextLink, feralTextLinkC, feralTextLinkFNameC,
"			and feralTextLinkStringC. If you wish to have textlinks colored in
"			comments add feralTextLinkC to the commentgroup of your language
"			i.e. for c:
"			syn cluster	cCommentGroup	contains=cTodo,feralTextLinkC
"
"	5) (OPTIONAL) You may then (optinally but recommended) add color definitions for
"		feralTextLink, feralTextLinkC, feralTextLinkFNameC,
"		feralTextLinkStringC in your color file (located at ~/.vim colors/ or
"		$VIM/vimfiles/colors/)
"
"		Given the textlink: |Fname|@|Searchstring|
"
"		feralTextLink and feralTextLinkC being the color of the bars and at
"		sign: ||@||
"			feralTextLinkC is the contained version, what you will see in
"			comments or the like).
"
"		feralTextLinkFNameC for `Fname`
"
"		feralTextLinkStringC for `Searchstring`
"
"		For referance I went with this color scheme in my color.vim file. (for
"			GVIM):
"		hi feralTextLink		guifg=yellow2 guibg=SeaGreen gui=underline,bold
"		hi feralTextLinkC		guifg=yellow2 guibg=SeaGreen gui=underline,bold
"		hi feralTextLinkFName	guifg=wheat guibg=SeaGreen gui=underline
"		hi feralTextLinkString	guifg=Linen guibg=SeaGreen gui=underline
"
" {{{	textlink-fragment.vim: (delete the starting quote and tab to get the original file.)
"	" vim: ff=unix
"	" =============================================================================
"	"	\file		textlink-fragment.vim
"	"
"	"	\brief		Syntex coloring for a textlink
"	"
"	"	\author		Robert KellyIV <Feral@FireTop.Com>
"	"	\note		Released into the Public Domain Sun Jun 16 2002
"	"	\date		Wed, 31 Jul 2002 13:53 Pacific Daylight Time
"	"	\version	$Id$
"	" =============================================================================
"	" HOW TO:
"	"	1) Place this file in your ~/.vim/syntax or $VIM/vimfiles/syntax directory.
"	"	2) Goto the syntax directory (~/.vim/syntax or $VIM/vimfiles/syntax)
"	"	3) Choose which files you want textlink highlighting to appear in, i.e. c
"	"		files and say txt files. (So in this example the files we want are:
"	"		c.vim and txt.vim)
"	"	3) Add these two lines to the file(s), before comment groups i.e. BEFORE the
"	"		line that reads: "syn cluster	cCommentGroup	contains=" for c files.:
"	"		" syntex coloring for textlinks.
"	"		source $VIM/vimfiles/syntax/textlink-fragment.vim
"	"		"source ~/.vim/syntax/textlink-fragment.vim
"	" =============================================================================
"	
"	
"	syn match	feralTextLink			'|\(\f\+\)|@|\(.*\)|' contains=feralTextLinkFNameC,feralTextLinkStringC
"	syn match	feralTextLinkC			'|\(\f\+\)|@|\(.*\)|' contained contains=feralTextLinkFNameC,feralTextLinkStringC
"	syn match	feralTextLinkFNameC		'|\(\f\+\)|'hs=s+1,he=e-1 contained
"	syn match	feralTextLinkStringC	'@|\(.*\)|'hs=s+2,he=e-1 contained
"	
"	
"	" Define the default highlighting.
"	" For version 5.7 and earlier: only when not done already
"	" For version 5.8 and later: only when an item doesn't have highlighting yet
"	if version >= 508 || !exists("did_document_syntax_inits")
"	  if version < 508
"	    let did_document_syntax_inits = 1
"	    command -nargs=+ HiLink hi link <args>
"	  else
"	    command -nargs=+ HiLink hi def link <args>
"	  endif
"	
"		HiLink feralTextLink		Identifier
"		HiLink feralTextLinkC		Identifier
"		HiLink feralTextLinkFNameC	Comment
"		HiLink feralTextLinkStringC	String
"	
"	" OPTIONAL:	For referance I went with this color scheme in my color.vim file.
"	" NOTE:		That these colors were chosen with gvim in mind.
"	" NOTE:		These definitions need to go in the color file you use, NOT here.
"	"hi feralTextLink		guifg=yellow2 guibg=SeaGreen gui=underline,bold
"	"hi feralTextLinkC		guifg=yellow2 guibg=SeaGreen gui=underline,bold
"	"hi feralTextLinkFName	guifg=wheat guibg=SeaGreen gui=underline
"	"hi feralTextLinkString	guifg=Linen guibg=SeaGreen gui=underline
"	
"	  delcommand HiLink
"	endif
"	"
"	" =============================================================================
"	"EOF
" }}}
"
" }}}
"
" Cautions:
"	There is not a huge amount of error checking (still needs if the file was
"	not found and if the search string was not found).. but works (great) for
"	me an I've never ran into a situation that prompted me to fix it.
"
"	Come to think of it not sure about a regular expression SearchString..
"	probably.
"
"	only one TextLink per line however any amount of text can be around it...
"	Mine are often in the middle of paragraphs... (descriping what needs to be
"	done next for example and textlink to that source file and line(well
"	search string).
"
" Examples:
"	// |~\cvs.html|@|$EDITOR|
"		load ~\cvs.html and search for the text `$EDITOR`.
"
"	// |A|@|if(somenum < 25 && somenum > 15)|
"		Load the alternate file (via a.vim) and search for "if(somenum < 25 && somenum > 15)"
"	// |A|@|if(somenum < 25 || somenum > 15)|
"		Load the alternate file (via a.vim) and search for "if(somenum < 25 || somenum > 15)"
"		Note that the search string contains | chars.. this is ok. (we use a
"		greedy match to make sure it is ok.)
"	// See: |~\cvs.html|@|if(somenum < 25 || somenum > 15)|, pretty cool eh?
"		Same as the above just with some text on either side.. the textlink
"		can appear anywhere on the line, ONLY ONE PER LINE however. (because
"		of the greedy match for | to allow the search string to have |s in
"		them, as mentioned below.
"
"	NOTE: you can use enviromental varables for the file name (basicaly any
"		valid vim file name I believe). Below, $KTBUGS is an enviromental var
"		that equates to a filename.
"
"	// \bug	See |$KTBUGS|@|BUG001| -- Some note about the bug
"		A useful example.. $KTBUGS is an enviromental var that expands to a
"		file name (full path) and BUG001 is just a unique identifier in the
"		(plain text) bug list.
"	// |$VIM/vimfiles/plugin/textlink.vim|@|" Examples:|
"		Combination enviromental var and file name, really handy when you make
"		an envvar out of a base path (your projects path for instance)
"
"		Similar to below in that the search string is in this file and BEFORE
"		this textlink.
"
"	// |T|@|" Examples that DON'T work:|
"		Similar to above in that the search string is in this file and AFTER
"		this textlink.
"
"	NOTE: you can use 'T' as the file name to use the existing file,
"		basicaly just split and search for the string. (!shrug! it was easy to
"		do.. and quite handy as it turns out.)
"	// |T|@|Example: #3|
"
" Examples That Dont Work:
"	NOTE: you can use :TTL to echo the found filename and search string, handy
"		to see why your textlink isn't working like you think it should, and
"		for the examples below.
"	Example: #1	two text links on a line is invalid:
"	// |~\cvs.html|@|$EDITOR| blabber |!ALT!|@|if(somenum < 25 && somenum > 15)|
"	Example: #2	text after the text link that contains a bar '|':
"	// |~\cvs.html|@|$EDITOR| blabber and this too|
"	Example: #3	empty filename or search string (kind of pointless ;) )
"	// ||@|$EDITOR| blabber and this too
"	// |~\cvs.html|@|| blabber and this too
"	NOTE:
"	Both of cases #1 and #2 are rather unavoidable, I think, because of the
"	desire of wanting to allow a logical or '||' in the search string so we
"	use a gready match. (minimal match would stop at the first '|' ..
"	alternativly use a different seperator char other than bar. Frankly I
"	didn't find one I liked and I can live with the above limitations.
"
" Note:
"	Note that you can place enviromental vars ($ENVVAR notation, standard vim)
"	as the, or in the file name portion. This can be handy for often used
"	files (a TODO list or BUG list, or some such.)
"	Note that a file name of `A` requires `AlternateFile` in a.vim by Michael Sharpe
"		<feline@irendi.com> (http://vim.sourceforge.net/scripts/script.php?script_id=31),
"		and will (slickly) use the alternate file (as defined by/in a.vim).
" Redundant Format Explination:
"	There are two strings (enclosed by '|' chars) of interest in a line, the first
"	is the file name to load, the second is the string to search for in file name.
"	These '|' delimited strings are joined by a '@' char, so the pattern looks like
"	|word|@|word|.
" Known Bugs:
"	No error checking for if the filename was loaded properly.
"	The search string is escaped.., althought perhaps not everything that
"		should be escaped is being escaped.
"		See: |T|@|_TL_EscapedString_defined_here_|
"	Clutters up the jump list.

" MEW9 feature: {{{ We really don't want exactly like this however
"	Hyperlink Support 
"
"		Automatically parse URLS like http://www.multiedit.com, or 
"		ftp://, or mailto:, or file:, and identify it as a hyperlink 
"		by underlining it. 
"	
"		Launch URL from within the file. 
"		
"		Handles exec: to launch a program, and macro: to run a 
"		Multi-Edit macro. 
"		
"		Supports the following identifiers: ftp, http, gopher, mailto, 
"		news, nntp, telnet, wais, file, prospero, exec, macro. 
" }}}

"if exists("loaded_textlink")
"    finish
"endif
"let loaded_textlink = 1

function! <SID>TestTextLink() " {{{
	let Line = getline(line("."))
"	let Pattern = '|\(.\{-}\)|.*|\(.\{-}\)|'
	let Pattern = '|\(\f\+\)|@|\(.*\)|'
	let RawLine = matchstr(Line, Pattern)
	let FName = substitute(RawLine, Pattern, '\1', '')
	let String = substitute(RawLine, Pattern, '\2', '')
	echo "FName: '".FName."' String: '".String."'"
	unlet Line
	unlet Pattern
	unlet RawLine
	unlet FName
	unlet String
endfunction
" }}}

function! <SID>TextLink(...) " {{{
	if(a:0 == 0)
		echo "ERROR: param needed:"
		echo "0 = normal behanvior; split load file and search for string."
		echo "1 = load file, ignore string."
		echo "2 = ignore file, search for string."
		return
	endif

	let Line = getline(line("."))
"	let Pattern = '|\(.\{-}\)|.*|\(.\{-}\)|'
"	let Pattern = '|\(\f\+\)|.*|\(.\{-}\)|'	" old: blaber |FileName| blabber |SearchString| blabber
	let Pattern = '|\(\f\+\)|@|\(.*\)|'	" New: blabber |FileName|@|SearchString| blabber -- much easier to recognise.
	let RawLine = matchstr(Line, Pattern)
	let FName = substitute(RawLine, Pattern, '\1', '')
	let String = substitute(RawLine, Pattern, '\2', '')

"	exe "normal \<Esc>o".FName."\<CR>".String
" misc text |fl.vim|@|text to seach[iInd]->for()| misc text.
" misc text |fl.vim|@|text to seach| misc text.

	" gate: make sure the params are valid (in this case not empty)
	if FName == ''
		echo "TextLink: invalid file name, aborted"
		return
	endif
	if String == ''
		echo "TextLink: invalid search string, aborted"
		return
	endif

	" escape the search string (search string is litteral)
	" _TL_EscapedString_defined_here_
	let EscapedString = escape(String, '\.\~[]')
	" but do not match |searchstring| (i.e. don't match outself)
"	let NotBaredString = '[^\|]' . EscapedString . '[^\|\n]'
	let BaredString = '|' . EscapedString . '|'

"	echo 'Testing:'.EscapedString
"	echo 'Testing:'.NotBaredString
"	echo 'Testing:'.BaredString
"	echo 'Testing:'.String
"	return

" |T|@|Example: #3|

	" [Feral:202/02@05:31] ignore the file if we want, so we can just do a
	" local search. basicaly gg /String
	if(a:1 == 0 || a:1 == 1)
		" Load the file name if it is not null.
		if FName == "A"
			"comm! -nargs=? A call AlternateFile(0, <f-args>)
			"comm! -nargs=? AS call AlternateFile(1, <f-args>)
			call AlternateFile(1) " from a.vim by Michael Sharpe <feline@irendi.com> (http://vim.sourceforge.net/scripts/script.php?script_id=31)
			" TODO can we trap if AlternateFile() is an invalid name and explain
			" why and what is needed? -- here as a comment I guess.
		elseif FName == "T"
			if a:2 == 0
				execute ":split"
			else
				execute ":vsplit"
			endif
		else
			if a:2 == 0
				execute ":split ".FName
			else
				execute ":vsplit ".FName
			endif
		endif
	endif

	" TODO make sure the file was loaded, if not return.

	" [Feral:202/02@00:22] Let us be able to ignore the search string to signify no searching
	" (aka use :TL as :split, :edit)
" {{{ Mark I
"	if(a:1 == 0 || a:1 == 2)
"		" Now, search for the string.
"		" TOOD save cursor position.
"		execute "normal gg"
"		call search( NotBaredString )
"		execute "normal l"
"		" TODO if search string not found, restore cursor position.
"	endif
" }}}
" {{{ Mark II
"	if(a:1 == 0 || a:1 == 2)
"		" Now, search for the string.
"		" TOOD save cursor position.
"
"		execute "normal gg"
"		let LineWeDoNotWant = search( BaredString )
""		echo "LineWeDoNotWant".LineWeDoNotWant
"		" LineWeDoNotWant will be 0 if the bared string was not found.
"		" search for the good string now.
"		while 1
"			let LineWeWant = search( EscapedString )
""			echo "LineWeWant".LineWeWant
"
"			if LineWeDoNotWant == 0
"				break
"			endif
"
"			if LineWeWant == 0
"				break
"			endif
"
"			if LineWeWant != LineWeDoNotWant
"				break
"			endif
"		endwhile
"
""		execute "normal l"
"		" TODO if search string not found, restore cursor position.
"	endif
" }}}

" |T|@|" Search for this!:|
" blabber
" |T|@|" Search for this!:|
" blabber
"
"" Search for this!:
"
" |T|@|" Search for this!:|
" blabber
" |T|@|" Search for this!:|

" {{{ Mark III
	if(a:1 == 0 || a:1 == 2)
		" Now, search for the string.
		" TOOD save cursor position.

		execute "normal gg"
		" LineWeDoNotWant will be 0 if the bared string was not found.
		" search for the good string now.
		while 1
			let CurLine = line(".")
			let LineWeWant = search( EscapedString, 'W' )
"			echo "LineWeWant:      ".LineWeWant
			execute "normal ".CurLine."G"
			let LineWeDoNotWant = search( BaredString, 'W' )
"			echo "LineWeDoNotWant: ".LineWeDoNotWant
			execute "normal j"

			if LineWeWant != LineWeDoNotWant
				execute "normal ".LineWeWant."G"
				execute "normal zz"
				break
			endif

			" textlink is only line in the file that has that searchstring.
			if LineWeDoNotWant == 0 && LineWeWant == 0
				echo "The textlink is the only line in this file that has the specified searchstring."
				break
			endif

			if LineWeDoNotWant == 0
				break
			endif

			if LineWeWant == 0
				break
			endif
		endwhile

"		execute "normal l"
		" TODO if search string not found, restore cursor position.
	endif
" }}}

	unlet Line
	unlet Pattern
	unlet RawLine
	unlet FName
	unlet String
	unlet EscapedString
	unlet BaredString
	unlet! CurLine
	unlet! LineWeWant
	unlet! LineWeDoNotWant
endfunc " }}}

"*****************************************************************
"* Commands
"*****************************************************************
":command! -nargs=? FL call <SID>SplitAndEditFileList(<f-args>)
" 0 = old behanvior; split load file and search for string.
" 1 = load file, ignore string.
" 2 = ignore file, search for string.
" Second param is 0 for a horiz split else vert split.
:command! TL	call <SID>TextLink(0, 0)
:command! TLF	call <SID>TextLink(1, 0)
:command! TLS	call <SID>TextLink(2, 0)
:command! VTL	call <SID>TextLink(0, 1)
:command! VTLF	call <SID>TextLink(1, 1)
:command! VTLS	call <SID>TextLink(2, 1)
:command! TTL	call <SID>TestTextLink()


" EOF
