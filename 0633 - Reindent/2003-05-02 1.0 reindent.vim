" Vim global plugin for reindenting a complete file
" Last Change: 	2003 May 03
" Maintainer: 	Joerg Tretter <j.tretter@gmx.de>
" Version:			1.0
"
" If you like well formatted files use this plugin to chage the indention of a
" complete file.
"
" Install:
" Copy this file to your plugin directory
"
"	Mappings:
" <Leader>R   or   <Plug>reindent
" If you don't know what <Leader> is read :help leader or press \R to reindent
" If using menus you will find a new menu item "Reindent" in menu "Syntax".
"
" Annotations:
" The indention information is taken out of the VIM Indent-files. This script
" processes them in a little different way than vim itself, therefore it is
" possible that the indent-functions have to be rewritten before they work.
" This script tries to rewrite the indent functions automatically (during this
" a file "reindent.tmp" will be created in your current working directory). 
" There may be indent functions out there that don't work with this script
" because they are too complex to be rewriten automatically with my code.
"
" Preconditions:
" - A indent function has to exist for your current file (indentexpr is set)
" - Your current working directory has to be writable if Indent-function has to
" 	be rewritten.
"
" VimVersions:
" - 6.1.320 on Linux -> Works
" - 6.1     on Win98 -> Hangs during function rewrite (See Patch 6.1.288)
" - 6.2a    on Win98 -> Works

let s:save_cpo = &cpo
set cpo&vim

if has("menu")
	amenu &Syntax.&Reindent :call <SID>Reindent()<CR>
endif

if exists("loaded_Reindent")
	finish
endif
let loaded_Reindent = 1

if !hasmapto('<Plug>reindent')
	map <leader>R :call <SID>Reindent()<CR>
endif

noremap <unique> <script> <Plug>reindent <SID>Reindent
noremap <SID>Reindent  :call <SID>Reindent()<CR>

function! s:Reindent()
	let s:i=1
	" remember current line, we want it restored afterwards.
	let s:currLine=line(".")
	let s:numLines=line("$")
	
	" Do not indent if there is no ident-function available
	if &indentexpr == ""
		echo "No Indent information for current file."
	else 
		if !(&indentexpr =~ "v:lnum")
			" Rewrite the indent function in a way we can work with (v:lnum is write
			" protected for me and if the indent-function uses that directly I won't
			" have a chance to use it)
			call RewriteIndentFunction(&indentexpr)
		endif
		
		" remove leading white spaces to have a defined starting point
		%s/^\s*//g
		
		" substitute v:lnum with local Line-Number variable because v:lnum is only
		" available during Vim-standard indention
		let s:indentCmd=substitute(&indentexpr,"v:lnum","s:i","g")
		
		" Run the indent-function for each line and fill in the needed
		" Tabs/spaces.
		while s:i <= s:numLines
			" Execute the Format-Function to get the indent-width 
			exe "let s:numInd = ". s:indentCmd
			" Fill with spaces
			if s:numInd%&ts > 0
				exe "normal " . s:i . "G0" . s:numInd%&ts . "i " 
			endif
			" Fill with Tabs
			if s:numInd > 0
				exe "normal " . s:i . "G0" . s:numInd/&ts . "i	" 
			endif
			let s:i = s:i + 1
		endwhile
		
		" restore Cursor-Position
		exe "normal " . s:currLine . "G"
		echo "reindent done."
	endif
endfun

" This is a dirty hack, I think there must be an easier way to do this but
" this is my first vimscript and I have not enough experience to be sure.
function! RewriteIndentFunction(indentExpr)
	let s:newFunc = substitute(a:indentExpr,'()','(myLnum)','')
	let s:newIndentExpr = substitute(a:indentExpr,'()','(v:lnum)','')
	let s:funcListing = s:GetFunctionCode(substitute(a:indentExpr,'^\(.*\)(.*$','\1',''))
	
	" Create new buffer (Split-window)	
	exe 'normal :new'
	" Insert the indent function's code there and remove leading white spaces
	exe 'normal i' . s:funcListing        
	exe 'normal :%s/^\s*//'
	" Remove the line numbers from redirected output
	exe 'normal :/^function/+1,/^endfun/-1 s/^...//'
	" Replace the v:lnum with our new argument
	exe 'normal :%s/v:lnum/a:myLnum/'
	" Replace function definition
	exe 'normal :%s/^function\s\+' . a:indentExpr . '.*$/function! ' . s:newFunc . '/' 
	" I was not able to go around this, Write the result to a file read it using
	" :source afterwards, that will do the job of bringing the new
	" indent function's code in our good old edit buffer
	exe 'normal :w!reindent.tmp' 
	exe 'normal :q'
	exe 'normal :source reindent.tmp'
	" The only thing we have to do now is setting the new indentexpression that
	" it deliveres the v:lnum parameter to our function.
	exe 'normal :setlocal indentexpr=' . s:newIndentExpr . ''
endfun

" Get Code of the specified function
" !Will not remove the line-numbering!
function! s:GetFunctionCode(funcName)
	let _z = @z
	redir @z
	silent! exec 'function ' . a:funcName
	redir END
	let output = @z
	let @z = _z
	return output
endfun

let &cpo = s:save_cpo

" vim: nowrap ts=2 sw=2
