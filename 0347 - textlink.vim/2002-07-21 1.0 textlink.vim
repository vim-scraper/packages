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
"				This simply :splits and :edits the specified file, goes to the
"				top of said file, and seaches (down) for the specified text.
"
"	\author		Robert KellyIV <Feral@FireTop.Com>
"	\note		Released into the Public Domain.
"	\date		Sun, 21 Jul 2002 10:48 Pacific Daylight Time
"	\version	$Id$
"	Version:	1.0
"
" Index:	Just :TL or :TLS to hop to the entry.. (`` to return if you like)
"	|T|@|Usage:|
"	|T|@|Commands:|
"	|T|@|Cautions:|
"	|T|@|Examples:|
"	|T|@|Examples That Dont Work:|
"	|T|@|Note:|
"	|T|@|Redundant Format Explination:|
"	|T|@|Known Bugs:|
"
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
"	Neither FileName nor SearchString are allowed to be blank.
"
" Commands:
"		:TTL
"			Test text link. (echos the found FileName and SearchString)
"		:TL
"			:split, :edit FileName, gg, /SearchString (basicaly).
"			This is to say splits the current window and loads the specified
"			file name then searches for the specified SearchString from the
"			top of the file.
"		:TLF
"			Like :TL save ignores the SearchString... basicaly a short cut to
"			:split and gf
"		:TLS
"			Like :TL save ignores the FileName... usefull when FileName is |T|
"			(this file) and you do not want to split the window.
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
"		basicaly just split and search for the string. (!shrug! it was easy to do..)
"		// |T|@|Example: #3|
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
"	Note that a file name of `A` requires `AlternateFile` in a.vim by Michael Sharpe <feline@irendi.com> (http://vim.sourceforge.net/scripts/script.php?script_id=31), and will (slickly) use the alternate file (as defined by/in a.vim).
" Redundant Format Explination:
"	There are two strings (enclosed by '|' chars) of interest in a line, the first
"	is the file name to load, the second is the string to search for in file name.
"	These '|' delimited strings are joined by a '@' char, so the pattern looks like
"	|word|@|word|.
" Known Bugs:
"	No error checking for if the filename was loaded properly.
"	The search string is not escaped.. [] is not matched (needs to be \[\], etc.)


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

"function! <SID>SplitAndEditFileList(...)
"	if(a:0 == 0)
"		let fileToLoad = "FILELIST"
"	else
"		let fileToLoad = a:1
"	endif
"	execute ":split " . fileToLoad
"endfunc

"function! <SID>TextLink()
"	let Line = getline(line("."))
"	let RawFName = matchstr(Line, '|.\{-}|')
"	let BustOnLine = matchend(Line, '|.\{-}|')
"	let RawStr = matchstr(Line, '|.\{-}|', BustOnLine)
"" echo matchend("misc text |filename test| misc text yea buddy |text to seach for| misc text", '|.\{-}|')
"" echo matchstr("misc text |filename test| misc text yea buddy |text to seach for| misc text", '|.\{-}|', 25)
"	exe "normal \<Esc>o".RawFName."\<CR>".RawStr
"" misc text |filename test| misc text yea buddy |text to seach for| misc text.
"" |filename test|
"" |text to seach for|
"endfunc


" |A|@|Important thing in alternate file, via a.vim i.e. :AS|

" |~\cvs.html|@|$EDITOR|
" blabber |~\cvs.html|@|$EDITOR|, could be

function! <SID>TestTextLink() " {{{
	let Line = getline(line("."))
"	let Pattern = '|\(.\{-}\)|.*|\(.\{-}\)|'
	let Pattern = '|\(\f\+\)|@|\(.*\)|'
	let RawLine = matchstr(Line, Pattern)
	let FName = substitute(RawLine, Pattern, '\1', '')
	let String = substitute(RawLine, Pattern, '\2', '')
	echo "FName: '".FName."' String: '".String."'"
endfunction
" }}}


"	Depends on |T|@|Foundation: Script Engine stuffs.|
"	Needed for |T|@|Foundation: Localization Preparation|, among other things.
" |~\cvs.html|@|$EDITOR|

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
			execute ":split"
		else
			execute ":split ".FName
		endif
	endif

	" TODO make sure the file was loaded, if not return.

	" [Feral:202/02@00:22] Let us be able to ignore the search string to signify no searching
	" (aka use :TL as :split, gf)
"	if(a:1 == 0 || a:1 == 2)
"		" Now, search for the string.
"		" TOOD save cursor position.
"		execute "normal gg"
"		call search( NotBaredString )
"		execute "normal l"
"		" TODO if search string not found, restore cursor position.
"	endif
	if(a:1 == 0 || a:1 == 2)
		" Now, search for the string.
		" TOOD save cursor position.

		execute "normal gg"
		let LineWeDoNotWant = search( BaredString )
"		echo "LineWeDoNotWant".LineWeDoNotWant
		" LineWeDoNotWant will be 0 if the bared string was not found.

		" search for the good string now.
		while 1
			let LineWeWant = search( EscapedString )
"			echo "LineWeWant".LineWeWant

			if LineWeDoNotWant == 0
				break
			endif

			if LineWeWant == 0
				break
			endif

			if LineWeWant != LineWeDoNotWant
				break
			endif
		endwhile

"		execute "normal l"
		" TODO if search string not found, restore cursor position.
	endif

endfunc " }}}

"*****************************************************************
"* Commands
"*****************************************************************
":command! -nargs=? FL call <SID>SplitAndEditFileList(<f-args>)
" 0 = old behanvior; split load file and search for string.
" 1 = load file, ignore string.
" 2 = ignore file, search for string.
:command! TL	call <SID>TextLink(0)
:command! TLF	call <SID>TextLink(1)
:command! TLS	call <SID>TextLink(2)
:command! TTL	call <SID>TestTextLink()


" EOF
