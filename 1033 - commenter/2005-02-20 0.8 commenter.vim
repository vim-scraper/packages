" -----------------------------------------------------------------
" File       :  commenter.vim
" Maintainer :  Sparks Lu (cnlgm@hotmail.com)
" Last Change:  July 15 2004
" Version    :  0.6
" Usage      :  Ctrl-c to comment/uncomment line or block
"               If cursor on blank line and no prev non-blank line, Ctrl-c to insert file header
"               If cursor on function declaration of c files, Ctrl-c to insert function description
" History    :  
"               0.6       Added c function description and file header insertion
"               0.6.1     Added perl support
"                         filereadable() ok. $HOME
"                         For bash, awk, sed, perl, if file header not exist, insert default
"               0.7       Use <Leader>c to comment/uncomment instead of Ctrl-c.
"               0.8       Allow any count of whitespace after comment symbol while uncommont line
"			  Insert commont symbol immediately before first non-whitespace character instead of beginning of line
" Todo       :  Uncomment file header and function description
" -----------------------------------------------------------------

if exists("loaded_commenter")
	finish
endif
let loaded_commenter=1

" Use c.vim attached template files
if !exists("g:CFileHeaderFn")
	let g:CFileHeaderFn=$HOME."/.vim/plugin/templates/c-file-header"
endif
if !exists("g:CppFileHeaderFn")
	let g:CppFileHeaderFn=$HOME."/.vim/plugin/templates/cpp-file-header"
endif
if !exists("g:HFileHeaderFn")
	let g:HFileHeaderFn=$HOME."/.vim/plugin/templates/h-file-header"
endif
if !exists("g:JavaFileHeaderFn")
	let g:JavaFileHeaderFn=$HOME."/.vim/plugin/templates/java-file-header"
endif
if !exists("g:ShFileHeaderFn")
	let g:ShFileHeaderFn=$HOME."/.vim/plugin/templates/bash-file-header"
endif
if !exists("g:AwkFileHeaderFn")
	let g:AwkFileHeaderFn=$HOME."/.vim/plugin/templates/awk-file-header"
endif
if !exists("g:VimFileHeaderFn")
	let g:VimFileHeaderFn=$HOME."/.vim/plugin/templates/vim-file-header"
endif
if !exists("g:SedFileHeaderFn")
	let g:SedFileHeaderFn=$HOME."/.vim/plugin/templates/sed-file-header"
endif
if !exists("g:PerlFileHeaderFn")
	let g:PerlFileHeaderFn=$HOME."/.vim/plugin/templates/perl-file-header"
endif
if !exists("g:PythonFileHeaderFn")
	let g:PythonFileHeaderFn=$HOME."/.vim/plugin/templates/python-file-header"
endif

if !exists("g:CFuncDesc")
	let g:CFuncDesc="~/.vim/plugin/templates/c-function-description"
endif
if !exists("g:CPPFuncDesc")
	let g:CPPFuncDesc="~/.vim/plugin/templates/cpp-function-description"
endif
if !exists("g:JavaFuncDesc")
	let g:JavaFuncDesc="~/.vim/plugin/templates/java-function-description"
endif

if !exists("g:DoxygenFuncDesc")
	let g:DoxygenFuncDesc=1
endif

function! s:InsertFileHeader()
	if &ft == 'c'
		let l:FileHeaderFn=g:CFileHeaderFn
	elseif &ft == 'h'
		let l:FileHeaderFn=g:HFileHeaderFn
	elseif &ft == 'cpp'
		let l:FileHeaderFn=g:CppFileHeaderFn
	elseif &ft == 'java'
		let l:FileHeaderFn=g:JavaFileHeaderFn
	elseif &ft == 'sh'
		let l:FileHeaderFn=g:ShFileHeaderFn
	elseif &ft == 'awk'
		let l:FileHeaderFn=g:AwkFileHeaderFn
	elseif &ft == 'vim'
		let l:FileHeaderFn=g:VimFileHeaderFn
	elseif &ft == 'sed'
		let l:FileHeaderFn=g:SedFileHeaderFn
	elseif &ft == 'perl'
		let l:FileHeaderFn=g:PerlFileHeaderFn
	elseif &ft == 'python'
		let l:FileHeaderFn=g:PythonFileHeaderFn
	endif

 	if filereadable(l:FileHeaderFn)
		silent exec "0read" . l:FileHeaderFn
	elseif &ft == 'sh'
		call append(0,"#!/bin/".&ft)
	elseif &ft == 'awk' || &ft == 'sed'
		call append(0,"#!/bin/".&ft." -f")
	elseif &ft == 'perl' || &ft == 'python'
		call append(0,"#!/usr/bin/".&ft)
	else
		echo 'No header template available!'
 	endif
endfunction

function! s:CommentUncommentLine()
	if strlen(getline(".")) == 0
		" Cursor on first non-blank line of file, add file header
		if prevnonblank(line(".")) == 0
			call s:InsertFileHeader()
		endif
		return
	endif

	" Cursor on a function defination line, add function description
	if &ft == 'c'
		let l:FuncDesc=g:CFuncDesc
	elseif &ft == 'cpp'
		let l:FuncDesc=g:CPPFuncDesc
	elseif &ft == 'java'
		let l:FuncDesc=g:JavaFuncDesc
	endif
	if ( &ft == 'c' || &ft == 'cpp' || &ft == 'java' ) &&  getline(".") =~ ')$' && getline(line(".")+1) =~ '^{'
		if( g:DoxygenFuncDesc )
			echo "Comment function"
			" Add Doxgen function comment
			exec "Dox"
		else
 		 	if filereadable(l:FuncDesc)
				silent exec "normal k"
				silent exec "read" . l:FuncDesc
 			endif
		endif
		return
	endif

	if &ft == 'c' || &ft == 'h' || &ft == 'cpp' || &ft == 'java' 
		let l:commentSymbol='\/\/'
	elseif &ft == 'sh' || &ft == 'awk' || &ft == 'sed' || &ft == 'perl' || &ft == 'python'
		let l:commentSymbol='\#'
	elseif &ft == 'vim'
		let l:commentSymbol='\"'
	endif

	" Insert comment symbol immediately before first non-whitespace character
	if(getline(".") =~ '^\s*' . commentSymbol)
		silent exec 's/' . l:commentSymbol . '\s*//'
	else
		silent exec 's/^\s*/&' . l:commentSymbol . ' /'
	endif
endfunction

function! s:CommentUncommentRange() range
	if &ft == 'c' || &ft == 'h' || &ft == 'cpp' 
		let l:strFirstLine=getline(a:firstline)
		if(l:strFirstLine =~ '^#if')
			exec a:firstline
			if(l:strFirstLine =~ '^#if 0')
				call setline(a:firstline, '#if 1')
			elseif l:strFirstLine =~ '^#if 1'
				call setline(a:firstline, '#if 0')
			endif
		else	
			exec a:firstline
			silent put!='#if 0'
		"	exec "normal O" . "#if 0 \<ESC>"
			exec a:lastline+2
			silent put!='#endif'
		"	exec "normal o" . "#endif \<ESC>"
		endif
	else
		exec a:firstline
		while line(".") <= a:lastline
			call s:CommentUncommentLine()
			if line(".") == line("$")
				break
			endif
			normal j
		endwhile
	endif
endfunction

autocmd FileType c,h,cpp,sh,awk,vim,sed,perl,python nnoremap <silent> <Leader>c :call <SID>CommentUncommentLine()<CR>
autocmd FileType c,h,cpp,sh,awk,vim,sed,perl,python vnoremap <silent> <Leader>c :call <SID>CommentUncommentRange()<CR>

" -----------------------------------------------------------------
" Following is copied from MakeDoxygenComment.vim
" -----------------------------------------------------------------

" MakeDoxygenComment.vim
" Brief: Creates a Doxygen style comment block for a function.
" Version: 0.1.3 
" Date: 3/1/04
" Author: Leif Wickland 
"
" Generates a doxygen comment skeleton for a C, C++,  or Java function,
" including @brief, @param (for each named argument), and @return.  The tag
" text as well as a comment block header and footer are configurable.
" (Consequently, you can have \brief, etc. if you wish, with little effort.)
"
" It's definitely a little rough around the edges, but I hope you find it
" useful.
" 
" To use:  In vim with the cursor on the line of the function header that
" contains the parameters (if any), execute the command :Dox.  This will
" generate the skeleton and leave the cursor after the @brief tag.
"
" Limitations:
" - Assumes that the function has a blank line above it and that all of the
"   parameters are on the same line.  
" - Not able to update a comment block after it's been written.
" - Writes the @return tag for a void function.
"
" Example:
" Given:
" int foo(char mychar, int yourint, double myarray[])
" { //...
" }
"
" Issuing the :Dox command with the cursor on the function declaration would
" generate
" 
" /**
" * @brief
" *
" * @param mychar
" * @param myint
" * @param myarray
" *
" * @returns
" **/
"
" To customize the output of the script, see the g:MakeDoxygenComment_*
" variables in the script's source.  These variables can be set in your
" .vimrc.
"
" For example, my .vimrc contains:
" let g:MakeDoxygenComment_briefTag="@Synopsis  "
" let g:MakeDoxygenComment_paramTag="@Param "
" let g:MakeDoxygenComment_returnTag="@Returns   "
" let g:MakeDoxygenComment_blockHeader="--------------------------------------------------------------------------"
" let g:MakeDoxygenComment_blockFooter="----------------------------------------------------------------------------"
if exists("loaded_MakeDoxygenComment")
    "echo 'MakeDoxygenComment Already Loaded.'
    finish
endif
let loaded_MakeDoxygenComment = 1
"echo 'Loading MakeDoxygenComment...'

if !exists("g:MakeDoxygenComment_briefTag")
    let g:MakeDoxygenComment_briefTag="@brief "
endif
if !exists("g:MakeDoxygenComment_paramTag")
    let g:MakeDoxygenComment_paramTag="@param "
endif
if !exists("g:MakeDoxygenComment_returnTag")
    let g:MakeDoxygenComment_returnTag="@return "
endif
if !exists("g:MakeDoxygenComment_blockHeader")
    let g:MakeDoxygenComment_blockHeader=""
endif
if !exists("g:MakeDoxygenComment_blockFooter")
    let g:MakeDoxygenComment_blockFooter=""
endif

function! <SID>MakeDoxygenComment()
    mark d
    exec "normal {"
    exec "normal o/**" . g:MakeDoxygenComment_blockHeader ."\<cr>" . g:MakeDoxygenComment_briefTag
    let l:synopsisLine=line(".")
    let l:synopsisCol=col(".")
    let l:nextParamLine=l:synopsisLine+2
    exec "normal a\<cr>\<cr>\<cr>\<cr>\<cr>" . g:MakeDoxygenComment_returnTag . "\<cr>\<bs>" . g:MakeDoxygenComment_blockFooter . "*/"
    exec "normal `d"
    let l:line=getline(line("."))
    let l:startPos=match(l:line, "(")
    let l:identifierRegex='\i\+[\s\[\]]*[,)]'
    let l:matchIndex=match(l:line,identifierRegex,l:startPos)
    let l:foundParam=0
    while (l:matchIndex >= 0)
        let l:foundParam=1
        exec "normal " . (l:matchIndex + 1) . "|"
        let l:param=expand("<cword>")
        exec l:nextParamLine
        exec "normal O" . g:MakeDoxygenComment_paramTag . l:param . "  "
        let l:nextParamLine=l:nextParamLine+1

        exec "normal `d"
        "echo "l:startPos before: " . l:startPos
        "echo "l:matchIndex = " . l:matchIndex
        "echo "strlen=" . strlen(l:param)
        "echo "total=" . (l:matchIndex+strlen(l:param)+1)
        let l:startPos=(l:matchIndex+strlen(l:param)+1)
        "echo "l:startPos after: " . l:startPos
        let l:matchIndex=match(l:line,identifierRegex,l:startPos)
    endwhile

    exec l:nextParamLine
    exec "normal dj"
    if (l:foundParam < 1)
        exec "normal dd"
    endif
    exec l:synopsisLine
    exec "normal " . l:synopsisCol . "|"
    startinsert!
endfunction

command! -nargs=0 Dox :call <SID>MakeDoxygenComment()
