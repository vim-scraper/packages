" Version:	1.3
" \date		Sun, 21 Jul 2002 09:01 Pacific Daylight Time
" \note		$Id$
" Note: I renamed this file from ToggleCommentify.vim to FeralToggleCommentify.vim only for clarity (didn't want anyone to overwrite an existing file or the like.. Name the file what you like :) )
" AlsoNote:	I was unable to reach Vincent at the given email address. If you
" have any questions feel free to drop me an email, I'll answer as best I can
" :) -- suggestions are always neat too! <feral@firetop.com>
"
" Original by Vincent Nijs <Vincent.Nijs@econ.kuleuven.ac.be>
" Some comment definitions extracted from EnhancedCommentify.vim by Meikel Brandmeyer <Brandels_Mikesh@web.de>
" Modifications by Feral (Aka RobertKellyIV) <Feral@FireTop.Com>
"	cleanup/refinement/rewriting/an other things :) -- I've made a mess but it
"	works quite nicely near as I can tell :)
"
" Feral's Changelog:
" Sat, 20  Jul  2002 02:26:29 Pacific Daylight Time:
"	Massive changes. Proper handling of ranges (not that it didn't work before
"		mind you)
"	Made ToggleCommentify, UnCommentify and Commentify commands (:TC, :UC and
"	:CC respectivly) so you can choose what to do to a line, or line range. (I
"	like to comment off entire sections of code, sometimes those sections
"	contain line comments.. toggleing in this instance has undesired results.)
"	Now I can simply highlight and :CC
"
"	Another (possibly) nice addition is the ability to specify the comment
"	symbol on after the command, I.e. :CC // which would add // chars to the
"	start of the range of lines.
"
"	The range defaults to the currnet line, as per VIM documentation.
"
"
" Examples:
"	:CC--
"	:CC --
"	" spaces are ate between the command and the string, if you want a space
"	use \ , i.e.
"	:CC\ --
"
" Known issues (aka bugs):
" --
	" [Feral:201/02@03:43] folded lines must be opend because a substitute
	" operation on a fold effects all lines of the fold. When called from a
	" range the result is that the lines of the fold have the substitute
	" command executed on them as many times as there is folded lines.
	" So, as a HACK if there is a fold, open it.
"
"	The above comment is in ToggleCommentify, Commentify and UnCommentify
"	at the relivent location. (just after the blank CommentSymbol gate).
"
"	Currently my only thought is to track the folds and what lines and then
"	reclose then or the like and frankly that, I feel, is not something that
"	should be done in this simple (idea is to keep is VERY simple) script...
"	So, I live with this inconvience currently.
" --
"	The provision to skip blank lines is remed out.. (because I do not want
"	that behavior). Search for IsBlankLineString and uncomment the let and if.
"	OPTIONAL: (may want to change the name of IsBlankLineString back to
"	lineString and where relivent rem the let lineString=getline....)
"	NOTE: this is untested... because, well, I WANT blank lines to be operated
"	on :)
" --
"
"
" Help topics I referanced a lot: (as a side note, interesting stuffs tho!)
"	:h function
"	:h function-range-example
"	:h command
"	:h E175
"	:h E177
"
"
" What would be nice:
"	Possibly allowing these to be used in motion commands (similar to = or i(
"	or ap.. currently you can just visual then :CC or whatever. Probably close
"	enough :)
"
"	Possibly using a count ... I had a register setup so I could insert cpp
"	style line comments that way and it was a pain to find the count 98% of
"	the time (thus prompting me to modify ToggleCommentify into this.).
"
"
"
"
" Based on:
"
" ToggleCommentify.vim
" Maintainer:	Vincent Nijs <Vincent.Nijs@econ.kuleuven.ac.be>
" Version:		1.2	
" Last Change:	Sunday, June 9th, 2001

" Disription:
" This is a (very) simple script to comment lines in a program. Currently supported languages are 
" C, C++, PHP, the vim scripting language, python, and ox. Given the simplicity of the program it 
" very easy to add support for new languages. The comments in the file should provide sufficient 
" information on how to proceed. 

" Install Details:
" You can put the functions in the attachment directly into your .vimrc or in a separate file to 
" be sourced. If you choose for the latter option add a statement such as ... 
" execute source ~/vim/myVimFiles/ToggleCommentify.vim	|" DO PUT source ... vim BETWEEN DOUBLE QUOTES !!)
" ... to your .vimrc file 
" To call the functions add the following mappings in your .vimrc. 
" map <M-c> :call ToggleCommentify()<CR>j 
" imap <M-c> <ESC>:call ToggleCommentify()<CR>j 
" The nice thing about these mapping is that you don't have to select a visual block to comment 
" ... just keep the ALT-key pressed down and tap on 'c' as often as you need. 

" Note: some people have reported that <M-c> doesn't work for them ... try <\-c> instead.


" [Feral:201/02@02:57] Old version; what I have been using (works great!) up
" untill I got playing with this :) .. changes from when I downloaded from
" vim.sf.net ... few more fileTypes recognised ... I think a bug fix with the
" make file file type (been a long while thought..).. changed how isCommented
" is calcualted to accomidate longer commentsymbols.
"function! ToggleCommentify(Style) " {{{
"	let lineString = getline(".")
"	" [Feral:201/02@01:15] But but I want to comment empty lines!
""	if lineString != $									" don't comment empty lines
"	let fileType = &ft								" finding out the file-type, and specifying the comment symbol
"	" {{{ Supported file types each have an if here, look at 'vim' filetype as an example.
"	" [Feral:201/02@01:17] ftf is my hypertext markup format. (which is to say txt with a few special chars)
"	if fileType == 'ox' || fileType == 'cpp' || fileType == 'php' || fileType == 'java'
"		let commentSymbol = '//'
"	elseif fileType == 'vim'
"		let commentSymbol = '"'
"	elseif fileType == 'lisp' || fileType == 'scheme' || fileType == 'dosini'
"		let commentSymbol = ';'
"	elseif fileType == 'tex'
"		let commentSymbol = '%'
"	elseif fileType == 'caos'
"		let commentSymbol = '*'
"	elseif fileType == 'm4' || fileType == 'config' || fileType == 'automake'
"		let commentSymbol = 'dnl '
"	elseif fileType == 'python' || fileType == 'perl' || fileType == 'make' || fileType =~ '[^w]sh$' || fileType == 'tcl' || fileType == 'jproperties'
"		let commentSymbol = '#'
"	elseif fileType == 'vb' || fileType == 'aspvbs'
"		let commentSymbol == "'"
"	elseif fileType == 'plsql' || fileType == 'lua'
"		let commentSymbol = '--'
"	else
""		execute 'echo "ToggleCommentify has not (yet) been implemented for this file-type"'
""		let commentSymbol = ''
""		execute 'echo "ToggleCommentify: Unknown filetype, defaulting to CPP style //"'
"		echo "ToggleCommentify: Unknown filetype, defaulting to CPP style //"
"		let commentSymbol = '//'
"	endif
"	" }}}
"
"	" [Feral:201/02@01:24] toggle the comment (what it was, and default)
"	if a:Style == 'c'
"		call Commentify(commentSymbol)
"	elseif a:Style == 'u'
"		call UnCommentify(commentSymbol)
"	else
"		let isCommented = strpart(lineString,0,strlen(commentSymbol) )		" FERAL: extract the first x chars of the line, where x is the width/length of the comment symbol.
"		if isCommented == commentSymbol
"			call UnCommentify(commentSymbol)			" if the line is already commented, uncomment
"		else
"			call Commentify(commentSymbol)				" if the line is uncommented, comment
"		endif
"	endif
"
""	endif
"endfunction " }}}
"


" Look to the vim entry here to add your own file types.
function! <SID>FindCommentify() " {{{

	let fileType = &ft								" finding out the file-type, and specifying the comment symbol
	if fileType == 'ox' || fileType == 'cpp' || fileType == 'php' || fileType == 'java'
		let commentSymbol = '//'
	" [Feral:201/02@01:17] ftf is my hypertext markup format. (which is to say txt with a few special chars)
	elseif fileType == 'ftf'
		let commentSymbol = '//'
	elseif fileType == 'vim'
		let commentSymbol = '"'
	elseif fileType == 'lisp' || fileType == 'scheme' || fileType == 'dosini'
		let commentSymbol = ';'
	elseif fileType == 'tex'
		let commentSymbol = '%'
	elseif fileType == 'caos'
		let commentSymbol = '*'
	elseif fileType == 'm4' || fileType == 'config' || fileType == 'automake'
		let commentSymbol = 'dnl '
	elseif fileType == 'python' || fileType == 'perl' || fileType == 'make' || fileType =~ '[^w]sh$' || fileType == 'tcl' || fileType == 'jproperties'
		let commentSymbol = '#'
	elseif fileType == 'vb' || fileType == 'aspvbs'
		let commentSymbol == "'"
	elseif fileType == 'plsql' || fileType == 'lua'
		let commentSymbol = '--'
	else
		execute 'echo "ToggleCommentify has not (yet) been implemented for this file-type"'
		let commentSymbol = ''
"		echo "ToggleCommentify: Unknown filetype, defaulting to CPP style //"
"		let commentSymbol = '//'
	endif

	return commentSymbol

endfunction " }}}

function! <SID>ToggleCommentify(...) " {{{

	if(a:0 == 0)
		let CommentSymbol = <SID>FindCommentify()
	else
		let CommentSymbol = a:1
	endif

	" [Feral:201/02@01:46] GATE: nothing to do if we have no comment symbol.
	if CommentSymbol == ''
		return
	endif

"	" [Feral:201/02@01:15] I want to comment empty lines so this is remed out.
"	let IsBlankLineString = getline(".")
"	if IsBlankLineString != $
"		" don't comment empty lines
"		return
"	endif

	" [Feral:201/02@03:43] folded lines must be opend because a substitute
	" operation on a fold effects all lines of the fold. When called from a
	" range the result is that the lines of the fold have the substitute
	" command executed on them as many times as there is folded lines.
	" So, as a HACK if there is a fold, open it.
	if(foldclosed(line(".")) != -1)
		:foldopen
	endif

	let lineString = getline(".")
	" FERAL: extract the first x chars of the line, where x is the width/length of the comment symbol.
	let isCommented = strpart(lineString,0,strlen(CommentSymbol) )
	if isCommented == CommentSymbol
""		call UnCommentify(CommentSymbol)			" if the line is already commented, uncomment
		set nohlsearch
		execute ':s+'.CommentSymbol.'++'
		set hlsearch
	else
""		call Commentify(CommentSymbol)				" if the line is uncommented, comment
		set nohlsearch
		execute ':s+^+'.CommentSymbol.'+'
		set hlsearch
	endif

endfunction " }}}

function! <SID>Commentify(...) " {{{

	if(a:0 == 0)
		let CommentSymbol = <SID>FindCommentify()
	else
		let CommentSymbol = a:1
	endif

	" [Feral:201/02@01:46] GATE: nothing to do if we have no comment symbol.
	if CommentSymbol == ''
		return
	endif

"	" [Feral:201/02@01:15] I want to comment empty lines so this is remed out.
"	let IsBlankLineString = getline(".")
"	if IsBlankLineString != $
"		" don't comment empty lines
"		return
"	endif

	" [Feral:201/02@03:43] folded lines must be opend because a substitute
	" operation on a fold effects all lines of the fold. When called from a
	" range the result is that the lines of the fold have the substitute
	" command executed on them as many times as there is folded lines.
	" So, as a HACK if there is a fold, open it.
	if(foldclosed(line(".")) != -1)
		:foldopen
	endif

	set nohlsearch
	" go to the beginning of the line and insert the comment symbol 
	execute ':s+^+'.CommentSymbol.'+'
	set hlsearch
"	execute 'normal 0i'.CommentSymbol
endfunction " }}}

function! <SID>UnCommentify(...) " {{{

	if(a:0 == 0)
		let CommentSymbol = <SID>FindCommentify()
	else
		let CommentSymbol = a:1
	endif

	" [Feral:201/02@01:46] GATE: nothing to do if we have no comment symbol.
	if CommentSymbol == ''
		return
	endif

"	" [Feral:201/02@01:15] I want to comment empty lines so this is remed out.
"	let IsBlankLineString = getline(".")
"	if IsBlankLineString != $
"		" don't comment empty lines
"		return
"	endif

	" [Feral:201/02@03:43] folded lines must be opend because a substitute
	" operation on a fold effects all lines of the fold. When called from a
	" range the result is that the lines of the fold have the substitute
	" command executed on them as many times as there is folded lines.
	" So, as a HACK if there is a fold, open it.
	if(foldclosed(line(".")) != -1)
		:foldopen
	endif

	let lineString = getline(".")
	" FERAL: extract the first x chars of the line, where x is the width/length of the comment symbol.
	let isCommented = strpart(lineString,0,strlen(CommentSymbol) )
	" [Feral:201/02@02:24] Only uncomment if the comment char is there (produces a string not found error otherwise).
	if isCommented == CommentSymbol
		set nohlsearch
		" remove the first comment symbol found on a line
		execute ':s+'.CommentSymbol.'++'
		set hlsearch
	endif
endfunction " }}}


"Keyboard mappings

"*****************************************************************
"* Commands
"*****************************************************************
:command! -nargs=? -range TC :<line1>,<line2>call <SID>ToggleCommentify(<f-args>)
:command! -nargs=? -range CC :<line1>,<line2>call <SID>Commentify(<f-args>)
:command! -nargs=? -range UC :<line1>,<line2>call <SID>UnCommentify(<f-args>)

map <M-c> :TC<CR>j 
imap <M-c> <ESC>:TC<CR>j 

"map <M-c>x :TC<CR>j
"map <M-c>c :CC<CR>j
"map <M-c>v :UC<CR>j
"imap <M-c>x <ESC>:TC<CR>j
"imap <M-c>c <ESC>:CC<CR>j
"imap <M-c>v <ESC>:UC<CR>j


"End of file
