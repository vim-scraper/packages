"
" Filename: cream-replacemulti.vim
" Updated:  2005-01-20 14:11:26-0400
"
" Cream -- An easy-to-use configuration of the famous Vim text editor
" [ http://cream.sourceforge.net ] Copyright (C) 2002-2005  Steve Hall
"
" License: {{{1
" This program is free software; you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation; either version 2 of the License, or
" (at your option) any later version.
" [ http://www.gnu.org/licenses/gpl.html ]
"
" This program is distributed in the hope that it will be useful, but
" WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
" General Public License for more details.
"
" You should have received a copy of the GNU General Public License
" along with this program; if not, write to the Free Software
" Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
" 02111-1307, USA.
"
" Description: {{{1
"
" This script replaces text across multiple files. It is a pure Vim
" implementation and does not depend on external utilties that do
" similar things (like sed, grep, cat, or Find). As such, it is
" cross-platform and has been tested on both Windows95/XP and Linux.
"
" Replace Multi is intended to be used in the GUI version of Vim,
" gVim. It presents a rather hackish dialog to take user input for the
" Find, Replace, and Path components and present options for Case
" Sensitivity and the use of Vim's Regular Expressions. (Some day in
" the future it might be adapted to work from the command line without
" a dialog.)
" 
" In testing, Replace Multi performed a replacment operation on 1000
" small files in 25 seconds. (2.80Ghz Windows XP machine with 1Gb of
" RAM.) Obviously various Vim and system settings could vary greatly,
" but as a benchmark this is about 40 files/second.
"
" ToDos: {{{1
"
" Other (potential) bells and whistles to be added:
" * Actual testing of non-alpha/numeric characters. ;)
" * Search subdirectories option
" * Provide a commandline version
" * Prevent files from showing up on Recent File or Buffers menu
"
" 1}}}
"
" Dependencies:
" ********************************************************************
" This script depends on Multivals, available from:
"   http://www.vim.org/script.php?script_id=171.
" ********************************************************************
"
" Cream_replacemulti() {{{1
function! Cream_replacemulti()
" Main function

	" save guioptions environment
	let myguioptions = &guioptions
	" do not use a vertical layout for dialog buttons
	set guioptions-=v

	" initialize variables if don't exist (however, remember if they do!)
	if !exists("g:CREAM_RMFIND")
		let g:CREAM_RMFIND = 'Find Me!'
	endif
	if !exists("g:CREAM_RMREPL")
		let g:CREAM_RMREPL = 'Replace Me!'
	endif
	" let's always get the current directory and don't remember previous(good idea?)
    " * Would be good if we could somehow have another button on the
    "   inputdialog() box for path that reads "Get Current", but this function
    "   doesn't have this feature (yet).
	if !exists("g:CREAM_RMPATH")
		" hmmm... buggy
		"let g:CREAM_RMPATH = Cream_path_fullsystem(getcwd(), "unix")
		" try this...
        if exists("$CREAM")
            let g:CREAM_RMPATH = Cream_path_fullsystem(expand("%:p:h"), "unix")
        else
            let g:CREAM_RMPATH = s:Cream_path_fullsystem(expand("%:p:h"), "unix")
        endif

	endif

	" option: case sensitive? (1=yes, 0=no)
	if !exists("g:CREAM_RMCASE")
		" initialize on
		let g:CREAM_RMCASE = 0
	endif

	" option: regular expressions? (1=yes, 0=no)
	if !exists("g:CREAM_RMREGEXP")
		" initialize off
		let g:CREAM_RMREGEXP = 0
	endif

    "*** No more warning
    "" beta warning
    "call confirm(
    "    \ "NOTICE:\n" .
    "    \ "\n" .
    "    \ "This tool is still beta quality. Use at your own risk!\n" .
    "    \ "\n" .
    "    \ "(But please forward us any bugs you find!)\n",
    "    \ "&Ok", 1, "Warning")
    "***

    " get find, replace and path/filename info (verifying that Find and Path
    " aren't empty, too)
	let valid = 0
	while valid == 0
		" get find, replace and path/filename info
		let myreturn = s:Cream_replacemulti_request(g:CREAM_RMFIND, g:CREAM_RMREPL, g:CREAM_RMPATH)
		" if quit code returned
		if myreturn == 2
			break
		" verify criticals aren't empty (user pressed 'ok')
		elseif myreturn == 1
			let valid = s:Cream_replacemulti_verify()
		endif
	endwhile

	" if not quit code, continue
	if myreturn != 2
		" warning
		let myproceed = s:Cream_replacemulti_warning()
""*** DEBUG:
"let n = confirm(
"    \ "DEBUG:\n" .
"    \ "  g:CREAM_RMFIND = " . g:CREAM_RMFIND . "\n" .
"    \ "  g:CREAM_RMREPL = " . g:CREAM_RMREPL . "\n" .
"    \ "  g:CREAM_RMPATH = " . g:CREAM_RMPATH . "\n" .
"    \ "  myreturn       = " . myreturn . "\n" .
"    \ "  myproceed      = " . myproceed . "\n" .
"    \ "\n", "&Ok\n&Cancel", 1, "Info")
"if n != 1
"    return
"endif
""***
		if myproceed != 0
			" DO FIND/REPLACE! (Critical file states are verified within.)
			if valid == 1
				call s:Cream_replacemulti_doit()
			else
				return
			endif
		else
			" do nothing
			return
		endif
	endif

	" restore guioptions
	execute "set guioptions=" . myguioptions

endfunction


" s:Cream_replacemulti_warning() {{{1
function! s:Cream_replacemulti_warning()
" simple death/destruction warning, last chance to bail

	let msg = "Warning! This action is about modify the contents of\n" .
			\ "multiple files based on the previous dialog's settings.\n"
	let msg = msg . "\n"
	let msg = msg . "  Continue?"
	let mychoice = confirm(msg, "&Ok\n&Cancel", 1, "Info")
	if mychoice == 0
		return
	elseif mychoice == 1
		return 1
	elseif mychoice == 2
		return
	else
		" error
		call confirm("Error in s:Cream_replacemulti_warning(). Unexpected result.", "&Ok", 1, "Error")
	endif
endfunction


" s:Cream_replacemulti_request() {{{1
function! s:Cream_replacemulti_request(myfind, myrepl, mypath)
" Dialog to obtain user information (find, replace, path/filename)
" * returns 0 for not finished (so can be recalled)
" * returns 1 for finished
" * returns 2 for quit

	" escape ampersand with second ampersand in dialog box
	" * JUST for display
	" * Just for Windows
	if      has("win32")
		\|| has("win16")
		\|| has("win95")
		\|| has("dos16")
		\|| has("dos32")
		let myfind_fixed = substitute(a:myfind, "&", "&&", "g")
		let myrepl_fixed = substitute(a:myrepl, "&", "&&", "g")
	else
		let myfind_fixed = a:myfind
		let myrepl_fixed = a:myrepl
	endif

	let mychoice = 0
	let msg = "INSTRUCTIONS:\n"
	let msg = msg . "* Enter the literal find and replace text below.\n"
	let msg = msg . "* Use \"\\n\" to represent new lines and \"\\t\" for tabs.\n"
	let msg = msg . "* Use wildcards as needed when entering path and filename.\n"
	let msg = msg . "* Please read the Vim help to understand regular expressions (:help regexp)\n"
	let msg = msg . "\n"
	let msg = msg . "\n"
	let msg = msg . "FIND:                                           " . myfind_fixed . "  \n"
	let msg = msg . "\n"
	let msg = msg . "REPLACE:                                 " . myrepl_fixed . "  \n"
	let msg = msg . "\n"
	let msg = msg . "PATH and FILENAME(S):    " . a:mypath . "             \n"
	let msg = msg . "\n"
	let msg = msg . "\n"
	let msg = msg . "OPTIONS:\n"
	if g:CREAM_RMCASE == 1
		let msg = msg . "               [X] Yes   [_] No          Case Sensitive\n"
	else
		let msg = msg . "               [_] Yes   [X] No          Case Sensitive\n"
	endif
	if g:CREAM_RMREGEXP == 1
		let msg = msg . "               [X] Yes   [_] No          Regular Expressions\n"
	else
		let msg = msg . "               [_] Yes   [X] No          Regular Expressions\n"
	endif
	let msg = msg . "\n"
	let mychoice = confirm(msg, "&Find\n&Replace\n&Path/Filename\nOp&tions\n&Ok\n&Cancel", 1, "Info")
	if mychoice == 0
		" quit via Esc, window close, etc. (OS specific)
		return 2
	elseif mychoice == 1
		" call dialog to get find string
		call s:Cream_replacemulti_find(g:CREAM_RMFIND)
		return
	elseif mychoice == 2
		" call dialog to get replace string
		call s:Cream_replacemulti_repl(g:CREAM_RMREPL)
		return
	elseif mychoice == 3
		" call dialog to get path string
		call s:Cream_replacemulti_path(g:CREAM_RMPATH)
		return
	elseif mychoice == 4
		" call dialog to set options. Continue to show until Ok(1) or Cancel(2)
		let valid = '0'
		while valid == '0'
			" let user set options
			let myreturn = s:Cream_replacemulti_options()
			" if Ok or Cancel, go back to main dialog
			if myreturn == 1 || myreturn == 2
				let valid = 1
			endif
		endwhile
		return
	elseif mychoice == 5
		" user thinks ok, return positive for actual verification
		return 1
	elseif mychoice == 6
		" quit
		return 2
	endif

	call confirm("Error in s:Cream_replacemulti_request(). Unexpected result.", "&Ok", "Error")
	return 2

endfunction


" Get input through dialog
" * Would be nice to detect Ok or Cancel here. (Cancel returns an empty string.)
" * These stupid spaces are to work around a Windows GUI problem: Input is only
"   allowed to be as long as the actual input box. Therefore, we're widening the
"   dialog box so the input box is wider. ;)
" s:Cream_replacemulti_find() {{{1
function! s:Cream_replacemulti_find(myfind)
	let myfind = inputdialog("Please enter a string to find...   " .
			\"                                                     " .
			\"                                                     " .
			\"                                                     " .
			\"                                                     ", a:myfind)
	" if user cancels, returns empty. Don't allow.
	if myfind != ""
		let g:CREAM_RMFIND = myfind
	endif
	return
endfunction

" s:Cream_replacemulti_repl() {{{1
function! s:Cream_replacemulti_repl(myrepl)
	let myrepl = inputdialog("Please enter a string to replace..." .
			\"                                                     " .
			\"                                                     " .
			\"                                                     " .
			\"                                                     ", a:myrepl)
	" allow empty return, but verify not a cancel
	if myrepl == ""
		let mychoice = confirm(
			\ "Replace was found empty.\n" .
			\ "\n" .
			\ "(This is legal, but was it your intention?)\n",
			\ "&Leave\ empty\n&No,\ put\ back\ what\ I\ had", 2, "Question")
		if mychoice == 2
			" leave global as is
		else
			let g:CREAM_RMREPL = ""
		endif
	else
		let g:CREAM_RMREPL = myrepl
	endif
	return
endfunction

" s:Cream_replacemulti_path() {{{1
function! s:Cream_replacemulti_path(mypath)
	let mypath = inputdialog("Please enter a path and filename... (Wildcards allowed.)" .
			\"                                                     " .
			\"                                                     " .
			\"                                                     " .
			\"                                                     ", a:mypath)
	" if user cancels, returns empty. Don't allow.
	if mypath != ""
		let g:CREAM_RMPATH = mypath
	endif
	return
endfunction

" s:Cream_replacemulti_options() {{{1
function! s:Cream_replacemulti_options()

	let mychoice = 0
	let msg = "Options:\n"
	let msg = msg . "\n"
	let msg = msg . "\n"
	if g:CREAM_RMCASE == 1
		let strcase = "X"
	else
		let strcase = "_"
	endif
	if g:CREAM_RMREGEXP == 1
		let strregexp = "X"
	else
		let strregexp = "_"
	endif
	let msg = msg . "    [" . strcase . "]  Case Sensitive\n"
	let msg = msg . "    [" . strregexp . "]  Regular Expressions\n"
	let msg = msg . "\n"
	let mychoice = confirm(msg, "Case\ Sensitive\nRegular\ Expressions\n&Ok", 1, "Info")
	if mychoice == 0
		" quit via Esc, window close, etc. (OS specific)
		return 2
	elseif mychoice == 1
		" case sensitive
		if g:CREAM_RMCASE == 1
			let g:CREAM_RMCASE = 0
		else
			let g:CREAM_RMCASE = 1
		endif
		return
	elseif mychoice == 2
		" regular expressions
		if g:CREAM_RMREGEXP == 1
			let g:CREAM_RMREGEXP = 0
		else
			let g:CREAM_RMREGEXP = 1
		endif
		return
	elseif mychoice == 3
		" ok
		return 1
	endif
	return
endfunction


" s:Cream_replacemulti_verify() {{{1
function! s:Cream_replacemulti_verify()
" Verify that Find and Path not empty (although Replace can be)
" * Returns '1' when valid

	if g:CREAM_RMFIND == ''
		call confirm("Find may not be empty.", "&Ok", "Warning")
		return
	elseif g:CREAM_RMPATH == ''
		call confirm("Path/Filename may not be empty.", "&Ok", "Warning")
		return
	else
		return 1
	endif
endfunction


" s:Cream_replacemulti_doit() {{{1
function! s:Cream_replacemulti_doit()
" Main find/replace function. Also validates files and state

	" get files {{{2

	let myfiles = ""

	" get file list
    " Note: Wildcard "*" for filename won't return files beginning with dot.
    " (Must use ".*" to obtain.)
    if exists("$CREAM")
        let myfiles = Cream_path_fullsystem(glob(g:CREAM_RMPATH), "unix")
    else
        let myfiles = s:Cream_path_fullsystem(glob(g:CREAM_RMPATH), "unix")
    endif

	" (from explorer.vim)
	" Add the dot files now, making sure "." is not included!
	"let myfiles = substitute(Cream_path_fullsystem(glob(g:rmpath), "unix"), "[^\n]*/./\\=\n", '' , '')

	" add a blank line at the end
	if myfiles != "" && myfiles !~ '\n$'
		let myfiles = myfiles . "\n"
	endif

	"
	" create file list {{{2

""*** DEBUG:
"let n = confirm(
"    \ "DEBUG:\n" .
"    \ "  myfiles:\n" .
"    \ myfiles . "\n" .
"    \ "\n", "&Ok\n&Cancel", 1, "Info")
"if n != 1
"    return
"endif
""***

	" count files (big assumption: number of newlines == number of files!)
	let filecount = MvNumberOfElements(myfiles, "\n")
	" initialize variables
	let newline = "\n"
	let mypos = 0
	let myposnew = 0
	let i = 0
	" iterate through files (do while newline still found in listing)
	while myposnew != -1
		let myposnew = match(myfiles, newline, mypos)

"*** DEBUG:
"let temp = confirm("DEBUG:\n" .
"			\"  filecount       = " . filecount . "\n" .
"			\"  strlen(newline) = " . strlen(newline) . "\n" .
"			\"  mypos           = " . mypos . "\n" .
"			\"  myposnew        = " . myposnew . "\n" .
"			\"  i               = " . i
"			\, "&Ok\n&Cancel")
"if temp == 2
"	break
"endif
"***
		if myposnew != -1
			let i = i + 1
			" get string
			let myfile{i} = strpart(myfiles, mypos, myposnew - mypos)
			" advance position to just after recently found newline
			let mypos = myposnew + strlen(newline)

"*** DEBUG:
"call confirm("DEBUG:\n  File " . i . ":      " . "|" . myfile{i} . "|", "&Ok")
"***
		else
			break
		endif
	endwhile

	" list file count and file names if less than 25
    if filecount > 25
        let mychoice = confirm(
            \"  Total number of files:   " . filecount . "\n" .
            \"\n", "&Ok\n&Cancel", 1, "Info")
    else
        let mychoice = confirm(
            \"  Total number of files:   " . filecount . "\n" .
            \"\n" .
            \"  Files to be modified...  \n" . myfiles . "\n" .
            \"\n", "&Ok\n&Cancel", 1, "Info")
    endif
	" Prompt: continue?
	if mychoice != 1
		let filecount = 0
		let myabort = 1
	endif

	"
	" find/replace in files (validating files prior to operation) {{{2

	if filecount == 0
		if exists("myabort")
			call confirm("Operation aborted.", "&Ok", "Info")
		else
			call confirm("No files were found to act upon!", "&Ok", "Info")
		endif
		return
	endif

	" strings
	" * use local variable to maintain global strings
	" * work around ridiculous differences between {pattern} and {string}
	let myfind = g:CREAM_RMFIND
	let myrepl = g:CREAM_RMREPL

	" capture current magic state
	let mymagic = &magic
	" turn off
	if mymagic == "1"
		set nomagic
	endif

	" case-sensitive
	if g:CREAM_RMCASE == 1
		let mycase = "I"
	else
		let mycase = "i"
	endif

	" regular expressions
	if g:CREAM_RMREGEXP == 1
		let myregexp = ""
		set magic
	else
		let myregexp = "\\V"
		set nomagic

		" escape strings
		" escape all backslashes
		" * This effectively eliminates ALL magical special pattern
		"   meanings! Only those patterns "unescaped" at the next step
		"   become effective. (Diehards are gonna kill us for this.)
		let myfind = substitute(myfind, '\\', '\\\\', 'g')
		let myrepl = substitute(myrepl, '\\', '\\\\', 'g')

		" un-escape desired escapes
		" * Anything not recovered here is escaped forever ;)
		let myfind = substitute(myfind, '\\\\n', '\\n', 'g')
		let myfind = substitute(myfind, '\\\\t', '\\t', 'g')
		let myrepl = substitute(myrepl, '\\\\n', '\\n', 'g')
		let myrepl = substitute(myrepl, '\\\\t', '\\t', 'g')

		" escape slashes so as not to thwart the :s command below!
		let myfind = substitute(myfind, '/', '\\/', 'g')
		let myrepl = substitute(myrepl, '/', '\\/', 'g')

		" replace string requires returns instead of newlines
		let myrepl = substitute(myrepl, '\\n', '\\r', 'g')

	endif

	" save options {{{2
	" turn off redraw
	let mylazyredraw = &lazyredraw
	set lazyredraw
	" turn off swap
	let myswapfile = &swapfile
	set noswapfile
	" turn off undo
	let myundolevels = &undolevels
	set undolevels=-1
	" ignore autocmds
	let myeventignore = &eventignore
	set eventignore=all
	" unload buffers
	let myhidden = &hidden
	set nohidden

	" Edit, Replace, Write, close {{{2
	"   (the main point!)
	"
	" * Force edit/replace/write/close here (we should have already
	"   picked up errors in validation)
	" * We do NOT allow regexp at the moment. Thus,
	"   - magic is turned off
	"   - substitution uses \V (see :help \V)
	"   - ALL escaped characters are disallowed, save 2 ("\n" and
	"     "\t") This is accomplished by escaping all the backslashes
	"     and then un-escaping just those two.

	" initialize iterations
	let i = 0
	" initialize errorlog
	let myerrorlog = ""
	" iterate while less than the total file count
	while i < filecount
		let i = i + 1
""*** Don't validate, too slow
"            " validate
"            "let myerror = s:Cream_replacemulti_validate(myfile{i})
"            let myerror = ""
"            "***
"            " proceed if empty (valid)
"            if myerror == ""


			" open file
			execute "silent! edit! " . myfile{i}

			" find/replace ( :s/{pattern}/{string}/{options} )
			" * {command}
			"   %  -- globally across the file
			" * {pattern}
			"   \V -- eliminates magic (ALL specials must be escaped, most
			"           of which we thwarted above, MWUHAHAHA!!)
			" * {string}
			" * {options}
			"	g  -- global
			"	e  -- skip over minor errors (like "not found")
			"	I  -- don't ignore case
			"	i  -- ignore case

			" condition based on options
			let mycmd = ':silent! %s/' . myregexp . myfind . '/' . myrepl . '/ge' . mycase

"*** DEBUG:
"let mychoice = confirm("DEBUG:\n  mycmd  = " . mycmd, "&Ok\n&Cancel", 1)
"if mychoice == 1
"endif
			" do it!
			execute mycmd
"***
			" save file
			execute "silent! write! " . myfile{i}

			" close file (delete from buffer as is our preference ;)
			execute "silent! bwipeout!"

"            else

"	"*** DEBUG:
"	"call confirm("DEBUG:\n  Bypassing operation on file:\n\n  " . myfile{i}, "&Ok")
"	"***
"                " log error if not valid
"                let myerrorlog = myerrorlog . myerror
"            endif
"*** end validation
	endwhile

	" restore options {{{2

	" restore
	let &lazyredraw = mylazyredraw
	let &swapfile = myswapfile
	let &undolevels = myundolevels
	let &eventignore = myeventignore
	let &hidden = myhidden

	" return magic state
	if mymagic == "1"
		set magic
	else
		set nomagic
	endif
	unlet mymagic

""*** DEBUG
"" iterate only the first few files
"let g:bob = 1
"if g:bob < 3
"	let msg = "DEBUG: (operation completed)\n"
"	let msg = msg . "  Find:                 " . myfind . "\n"
"	let msg = msg . "  \n"
"	let msg = msg . "  Replace:              " . myrepl . "\n"
"	let msg = msg . "  \n"
"	let msg = msg . "  Filename:             " . myfile{i} . "           \n"
"	let msg = msg . "  \n"
"	let msg = msg . "  \n"
"call confirm(msg, "&Ok", 1)
"endif
"let g:bob = g:bob + 1
""***

	if myerrorlog != ""
		call confirm("Difficulties were encountered during the operation:\n\n" . myerrorlog, "&Ok", "Error")
	endif
    " 2}}}

endfunction

" s:Cream_replacemulti_validate() {{{1
function! s:Cream_replacemulti_validate(filename)
" Validate a file for editing
" * Returns empty if valid
" * Returns string describing error if not valid
" * Argument must include full path

	" log error in this string (always returned; so if empty, valid)
	let errorlog = ""

	"......................................................................
	" do tests

	" is writable?
	let test1 = filewritable(a:filename)

	" has DOS binary extension?
	if    has("dos16") ||
		\ has("dos32") ||
		\ has("win16") ||
		\ has("win32")

		let test2 = match(a:filename, "exe", strlen(a:filename) - 3)
		" if return no match
		if test2 == '-1'
			" try 'com'
			let test2 = match(a:filename, "com", strlen(a:filename) - 3)
			" if yes, make return code different to distinguish
			if test2 != '-1'
				" found, return error code
				let test2 = '1'
			else
				let test2 = '0'
			endif
		else
			" assign error code
			let test2 = '-1'
		endif
	else
		let test2 = '0'
	endif

	" file is currently a buffer [distinguish buflisted() and bufloaded()]
	" * bufexists() returns 0 if doesn't exist, buffer number if does
	let test3 = bufexists(a:filename)


	"......................................................................
	" log errors (if any)
	if    test1 == '0' || test1 == '2' ||
		\ test2 == '1' || test2 == '-1' ||
		\ test3 != '0'

		" if an error, start with the file name
		let errorlog = errorlog . "\n" . a:filename . ":\n"

		" filewritable
		if test1 == '0' || test1 == '2'
			let errorlog = errorlog . " *  Not writable -- "
			if test1 == '0'
				let errorlog = errorlog . "Unable to write file (perhaps read-only or all ready open?)\n"
			elseif test1 == '2'
				let errorlog = errorlog . "Filename is a directory\n"
			endif
		endif

		" has DOS binary extension
		if test2 == '1' || test2 == '-1'
			let errorlog = errorlog . " *   Window program file (binary) -- "
			if test2 == '-1'
				let errorlog = errorlog . ".exe file\n"
			elseif test2 == '1'
				let errorlog = errorlog . ".com file\n"
			endif
		endif

		" is an existing buffer
		if test3 != '0'
			let errorlog = errorlog . " *   Buffer currently open "
		endif

	endif

"*** DEBUG:
"if errorlog != ""
"	call confirm("DEBUG:\n  Cream_multireplace_validate(), errorlog:\n" . errorlog, "&Ok")
"endif
"***

	return errorlog

endfunction

" 1}}}

" ********************************************************************
" Localised duplicate Cream functions
"
" Functions below are localised versions of global library functions
" duplicated here for portability of this script outside of the Cream
" environment.
" ********************************************************************
" s:Cream_path_fullsystem() {{{1
function! s:Cream_path_fullsystem(path, ...)
" Return a completely expanded and valid path for the current system
" from string {path}.
" o Path does not have to exist, although generally invalid formats
"   will not usually be properly expanded (e.g., backslash path separators
"   on Unix).
" o Both files and directories may be processed. (Paths will not be
"   returned with a trailing path separator.)
" o Preserves Windows UNC server name preceding "\\".
" o {optional} argument can be used to override system settings:
"   * "win" forces return in Windows format as possible:
"     - No drive letter is added (none can be assumed)
"   * "unix" forces return to Unix format as possible:
"     - Windows drive letter is not removed
"

	" format type
	" forced with argument
	if a:0 == 1
		if     a:1 == "win"
			let format = "win"
		elseif a:1 == "unix"
			let format = "unix"
		endif
	endif
	" detected if not forced
	if !exists("format")
		if Cream_has("ms")
			let format = "win"
		else
			let format = "unix"
		endif
	endif

	" expand to full path
	let path = fnamemodify(a:path, ":p")

	" make Windows format (assume is Unix)
	if format == "win"

		" remove escaping of spaces
		let path = substitute(path, '\\ ', ' ', "g")

		" convert forward slashes to backslashes
		let path = substitute(path, '/', '\', "g")

	" make Unix format (assume is Windows)
	else

		"" strip drive letter
		"let path = substitute(path, '^\a:', '', '')

		" convert backslashes to forward slashes
		let path = substitute(path, '\', '/', "g")

		" escape spaces (but not twice)
		let path = substitute(path, '[/\\]* ', '\\ ', "g")

	endif

	" remove duplicate separators
	let path = substitute(path, '\\\+', '\', "g")
	let path = substitute(path, '/\+', '/', "g")

	" remove trailing separators
	let path = substitute(path, '[/\\]*$', '', "")

	" maintain Windows UNC servername
	if s:Cream_path_isunc(a:path)
		let path = substitute(path, '^\', '\\\\', "")
		let path = substitute(path, '^/', '\\\\', "")
	endif

	return path

endfunction

" s:Cream_path_isunc() {{{1
function! s:Cream_path_isunc(path)
" Returns 1 if {path} is in Windows UNC format, 0 if not.
	if match(a:path, '^\\\\') != -1 || match(a:path, '^//') != -1
		return 1
	endif
endfunction

" 1}}}
" vim:foldmethod=marker
