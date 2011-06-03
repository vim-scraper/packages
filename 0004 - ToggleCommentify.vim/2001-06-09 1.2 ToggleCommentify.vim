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

function! ToggleCommentify()
	let lineString = getline(".")
	if lineString != $									" don't comment empty lines
		let isCommented = strpart(lineString,0,3)		" getting the first 3 symbols
		let fileType = &ft								" finding out the file-type, and specifying the comment symbol
		if fileType == 'ox' || fileType == 'cpp' || fileType == 'c' || fileType == 'php'
			let commentSymbol = '///'
		elseif fileType == 'vim'
			let commentSymbol = '"""'
		elseif fileType == 'python'
			let commentSymbol = '###'
		else
			execute 'echo "ToggleCommentify has not (yet) been implemented for this file-type"'
			let commentSymbol = ''
		endif
		if isCommented == commentSymbol					
			call UnCommentify(commentSymbol)			" if the line is already commented, uncomment
		else
			call Commentify(commentSymbol)				" if the line is uncommented, comment
		endif
	endif
endfunction

function! Commentify(commentSymbol)	
	set nohlsearch	
	execute ':s+^+'.a:commentSymbol.'+'					| " go to the beginning of the line and insert the comment symbol 
	set hlsearch										  " note: the '|' is so I can put a quote directly after an execute statement
endfunction
	
function! UnCommentify(commentSymbol)	
	set nohlsearch	
	execute ':s+'.a:commentSymbol.'++'					| " remove the first comment symbol found on a line
	set hlsearch	
endfunction
			
