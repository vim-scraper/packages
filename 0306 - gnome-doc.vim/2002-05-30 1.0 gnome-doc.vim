" GnomeDoc for Vim
" Copyright (c) 2001 Arjan Molenaar
" Based on Michael Zucchi's Gnome-Doc.el (in 'gnome-libs/tools')
"
" This file auto-generates a C function document header from the
" current function.
"
" In your .vimrc, add something like this (use Ctrl-R for adding headers):
"	source gnome-doc.vim
"	map <C-R> :exe GnomeDoc()<CR>
"
" A GnomeDoc function document headers looks like this:
" /**
"  * func_name:
"  * @arg1: 
"  *
"  * [Description]
"  *
"  * Return value:
"  **/
" int func_name (type1 arg1)
" { ...
"
" GnomeDoc() expects te first '{' of the function to start on the first column
" of a new line and a blank line before the function declaration.
"
" These special patterns are recognized by gnome-doc for further processing:
"	'funcname()'	function
"	'$ENVVAR'	environmental variable
"	'&struct_name'	name of a structure
"	'@parameter'	name of a parameter
"	'%CONST'	name of a constant

if has ("user_commands")

func! GnomeDoc()
	" A function contains three parts: a return type, a name and parameters
	let c_func_RE = '^[ ]*\(.*\)[ ]\{1,\}\([^ ]\{1,\}\)[ ]*([ ]*\(.*\)[ ]*)'

	" Shortcuts for editing the text:
	let txtBOL = "norm! o"
	let txtEOL = ""

	" Depending on some VI variables that I do not know you might have
	" to change these...
	"	txtCommentHead	Start the comment block
	"	txtComment1	Line used for first comment line
	"	txtCommentn	Line used for following comment lines
	"	txtCommentTail	End of the comment block
	if b:current_syntax == "c"
		" C automatically makes new lines comments...
		let txtCommentHead = "/**"
		let txtComment1 = ""
		let txtCommentn = ""
		let txtCommentTail = "*/"
	elseif b:current_syntax == "cpp"
		" ... C++ doesn't
		let txtCommentHead = "/**"
		let txtComment1 = " * "
		let txtCommentn = "* "
		let txtCommentTail = "**/"
	else
		" Most other languages use # for comments
		" NOTE: This is just an extra option. The code is written for
		"	C files, nothing else!
		let txtCommentHead = "##"
		let txtComment1 = "# "
		let txtCommentn = "# "
		let txtCommentTail = "##"
	endif

	let LineToEndOn = line(".")

	" Go to the beginning of the function
	exe "norm! [["
	let lastline = line (".") - 1

	" Find the previous blank line and assume the function starts there
	exe "norm! {"
	let firstline = line (".") + 1

	let i = firstline
	let name = ""
	while i <= lastline
		" Remove "//" comments directly
		let name = name . " " . substitute (getline (i), '\(.*\)\/\/.*', '\1', "")
		let i = i + 1
	endwhile

	" First some things to make it more easy for us:
	" tab -> space && space+ -> space
	let name = substitute (name, '\t', ' ', "")
	let name = substitute (name, '  *', ' ', "")

	" Now we have to split DECL in three parts:
	" \(return type\)\(funcname\)\(parameters\)
	let returntype = substitute (name, c_func_RE, '\1', "g")
	let funcname = substitute (name, c_func_RE, '\2', "g")
	let parameters = substitute (name, c_func_RE, '\3', "g") . ","

	"echo "Line(" . firstline . ", " . lastline . ")=" . funcname . "|" . parameters . "|" . returntype . "|"

	exe txtBOL . txtCommentHead . txtEOL
	exe txtBOL . txtComment1 . funcname . ":" . txtEOL

	while (parameters != ",") && (parameters != "") && (parameters != "void,")
		let _p = substitute (parameters, '\([^,]*\) *, *\(.*\)', '\1', "")
		let parameters = substitute (parameters, '\([^,]*\) *, *\(.*\)', '\2', "")
		let paramname = substitute (_p, '.*[^A-Za-z_]\([A-Za-z_][A-Za-z0-9_]*\)\(\[[0-9]\]\)*$', '\1', "")
		exe txtBOL . txtCommentn . "@" . paramname . ": " . txtEOL
	endwhile

	exe txtBOL . txtCommentn . txtEOL
	"exe txtBOL . "Description: " . txtEOL
	exe txtBOL . txtCommentn . txtEOL
	let LineToEndOn = line(".")

	if returntype !~# '\(^\|[^A-Za-z0-9_]\)void\([^A-Za-z0-9_]\|$\)'
		exe txtBOL . txtCommentn . txtEOL
		exe txtBOL . txtCommentn . "Return value: " . txtEOL
	endif

	" Close the comment block.
	exe txtBOL . txtCommentTail . txtEOL

	" Let the editor go to the right line so the user can start editing
	" right away.
	return "norm! " . LineToEndOn . "G$"
endfunc

endif " user_commands
