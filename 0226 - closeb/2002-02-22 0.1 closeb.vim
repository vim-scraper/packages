" vim:fdm=marker:commentstring="\ %s:
" Name: closeb.vim ("close brackets")
" Version: 0.1 
" Author: Mark A. Hillebrand
" Date: 20020222
" Description: close tags or complex brackets
" * new, extended implementation of closetag.vim (http://vim.sourceforge.net/scripts/script.php?script_id=13)
" * easily configurable for different filetypes
" * highlight bracketing errors (optional) (turn off highlighting with :match NONE)
" * magically insert newlines on closing brackets, if the matching bracket is on a single line (optional)
" * magically indents on closing brackets, so that both brackets align
" * show a "path" of all opened brackets, this may be put in the statusline for example
" Installation: put this into your plugin directory (~/.vim/plugin)
" Usage:
" * xml, sgml:
"   CTRL-_ in insert mode closes a tag
"   :Path shows a path of opened tags
"   If you like to put the path in your statusline, see at the end of this file for details
" * tex:
"   CTRL-_ in insert mode closes open environments
"
" TODO nicier documentation...
" TODO improve the magic so it ignores comments at the end of the string
" {{{ Some global variables and checks
if exists("loaded_closeb") || &cp
	finish
endif
let loaded_closeb = 1
highlight link ClosebError WarningMsg
" set s:sid script variable as suggested by vim documentation
map <SID>TEMP <SID>TEMP
let s:sid = substitute(maparg("<SID>TEMP"),'TEMP$','','')
unmap <SID>TEMP
" the following global var can be used to reference the path function (e.g.
" for the statusline, see at the end of the file)
let g:closeb_Pathfunc = s:sid . "Path()"
" }}}
" {{{ Configuring the algorithm for different filetypes (via autocommands) (approach from matchit.vim)
aug Closeb
  " a good generic skip expression:
  " let b:closeb_skip = 'synIDattr(synIDtrans(synID(line("."), col("."), 0)), "name") =~ "Comment\|Constant\|PreProc"'
" {{{ General Configuration (applies to all buffers / filetypes)
	au! FileType *
		\ let b:closeb_showerrors = 1 
		\ | let b:closeb_nlmagic = 1
		\ | let b:closeb_indentmagic = 1
		\ | let b:closeb_mfmagic = 0
	" The 'magic' option apply for closing a bracket, where the matching
	" bracket stands on a line of its own. There are three types of magicness:
	" newline magic: prepend & append newline so that the close tag also has its own line
	" indent magic (b:closeb_indentmagic): copy the indentation level of the matching tag
	" indent magic is only active if newline magic is active
	" additionally, it makes sense only if you have no appropriate indent mode for the filetype
	" TODO manualfold magic (b:closeb_mfmagic): close a manual fold opened on matching tag automatically
 " }}}
" {{{ Configuration for xml
	au! FileType xml,sgml
		\ let b:closeb_openre = '<[^? \t>/]\+\%(\s*>\|\s\+[^>]*[^/]>\)'
	    \ | let b:closeb_indentmagic = 0
		\ | let b:closeb_closere = '</[^? \t>/]\+>'
		\ | let b:closeb_skip = 'synIDattr(synIDtrans(synID(line("."), col("."), 0)), "name") =~ ' . "'" . 'Comment\|Constant\|PreProc' . "'"
		\ | let b:closeb_isopen = "strpart(mymatch,1,1) != '/'"
		\ | let b:closeb_openname = "matchstr(mymatch,'[^ " . '\t' . "><]" . '\+' . "',0)"
		\ | let b:closeb_closename = "matchstr(mymatch,'[^ " . '\t' . "></]" . '\+' . "',0)"
		\ | let b:closeb_makeclosetag = "'</' . mymatch . '>'"
" }}}
" {{{ Configuration for latex
	au! FileType tex
		\ let b:closeb_openre = '\\begin{[^ \t}]\+}'
		\ | let b:closeb_closere = '\\end{[^ \t}]\+}'
		\ | let b:closeb_skip = 'synIDattr(synIDtrans(synID(line("."), col("."), 0)), "name") =~ "Comment\|Constant\|PreProc"'
		\ | let b:closeb_isopen = "strpart(mymatch,1,1) == 'b'"
		\ | let b:closeb_openname = "matchstr(mymatch,'" . '\%(\\begin{\)\@<=[^ \t}]\+' . "',0)"
		\ | let b:closeb_closename = "matchstr(mymatch,'" . '\%(\\end{\)\@<=[^ \t}]\+' . "',0)"
		\ | let b:closeb_makeclosetag = "'\\end{' . mymatch . '}'"
" }}}
aug END
" }}}
" {{{ The scan algorithm
" {{{ The Path() function gets a path of open brackets
fun! <SID>Path()
	if !exists("b:closeb_openre")
		return '?'
	endif
	let s:stack = ''
	let s:lasttag = ''
	let s:errorlines = ''
	let s:ignoreemptystack = 1
	let result = searchpair(b:closeb_openre,'', b:closeb_closere, 'nbWr', '(' . b:closeb_skip . ') || <SID>Callback()' )
	if s:lasttag == ''
		return '?'
	else
		let s:lasttag = substitute(s:lasttag,' ','/','g')
		return s:lasttag
	endif
endfun
" }}}
" {{{ The Close() function returns a closing tag
fun! <SID>Close()
	let s:stack = ''
	let s:lasttag = ''
	let s:errorlines = ''
	let s:ignoreemptystack = 0
	let result = searchpair(b:closeb_openre,'', b:closeb_closere, 'nbW', '(' . b:closeb_skip . ') || <SID>Callback()' )
	if result == -1
		" an error occurred. (very unlikely, cause searchpair() must have
		" operated wrong then)
		return ''
	endif
	if b:closeb_showerrors 
		if s:errorlines != ''
			let matchexpr = substitute(strpart(s:errorlines,1),' ', 'l\|\%', 'g')
			let matchexpr = '\%(\%' . matchexpr . 'l\).*.'
			" .* would not do...
			exec 'match ClosebError /' . matchexpr . '/'
		else
			match none
		endif
	endif
	" TODO if lasttag stood alone on its line, insert newline, too
	" s:lasttag contains a space-separated list of the path.
	" there should only be one element unless result = -1 earlier
	if s:lasttag == ''
		return ''
	else
		let mymatch = strpart(s:lasttag,1)
		exec "let mymatch = " . b:closeb_makeclosetag
		if b:closeb_nlmagic && s:lasttag_line != line('.') && getline(s:lasttag_line) =~ '^\s*' . b:closeb_openre . '\s*$'
			let prependnl = (getline('.') =~ '^\s*$' ? '' : "\n")
			let appendnl = "\n"
			let prependindent = (b:closeb_indentmagic ? "0\<C-d>" . matchstr(getline(s:lasttag_line),'^\s*',0) : '')
			return prependnl . prependindent . mymatch . appendnl
		else
			return mymatch
		endif
	endif
endfun
" }}}
" {{{ The Callback() function is private, it will be invoked by the searchpair() function
fun! <SID>Callback()
	let mymatch = strpart(getline('.'),col('.')-1)
	if s:lasttag != '' && ! s:ignoreemptystack
		return -1
	endif
	"echomsg "Callback(): " . mymatch
	exec "let result = " . b:closeb_isopen
	if result
		exec "let mymatch = " . b:closeb_openname
		"echomsg "Detect open: " . mymatch
		if s:stack == ''
			" Close() == mymatch, remember, also the line number:
			let s:lasttag = ' ' . mymatch . s:lasttag
			" ^ note: order is important for the Path() function
			let s:lasttag_line = line('.')
		else
			if mymatch == strpart(s:stack,0,strlen(mymatch))
				"echomsg "mymatch matches s:stack start, popping: " . mymatch
			else
				"echomsg "mymatch did not match the s:stack start, line " . line('.')
				let s:errorlines = ' ' . line('.')
			endif
			" pop away, if error or not
			let s:stack = substitute(s:stack, '[^ ]\+ ', '', '' )
		endif
	else " !b:closeb_isopen(mymatch)
		exec "let mymatch = " . b:closeb_closename
		"echomsg "Pushing on stack: " . mymatch
		let s:stack = mymatch . ' ' . s:stack
	endif
	return 0 " signal never skip here
endfun
" }}}
" }}}
" {{{ Mappings and commands
" the :Path command display the path of open brackets in the statusline
com! -nargs=0 Path echo "Path: " . <SID>Path()
" CTRL-_ in insert mode closes a bracket:
imap <C-_> <C-r>=<SID>Close()<CR>
" Support for displaying the path in the statusline:
" use like this (statusline with only path): exec "set statusline=%{" . g:closeb_Pathfunc . "}"
" or like this (default + path) :exec "set statusline=%<%f%h%m%r\\ %{" . g:closeb_Pathfunc . "}%=%l,%c%V\\ %P"
" Warning: this can be slow with large files
" }}}
