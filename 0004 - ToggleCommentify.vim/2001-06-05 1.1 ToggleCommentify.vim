" ToggleCommentify.vim
" Maintainer:	Vincent Nijs <Vincent.Nijs@econ.kuleuven.ac.be>
" Version:		1.1	
" Last Change:	Sunday, June 3, 2001

function! ToggleCommentify()
	let lineString = getline(".")
	if lineString != $									" don't comment empty lines
		let isCommented = strpart(lineString,0,3)		" getting the first 3 symbols
		let fileType = &ft								" finding out the file-type, and specifying the comment symbol
		if fileType == 'ox' || fileType == 'cpp' || fileType == 'c'
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
			call Commentify(commentSymbol)			" if the line is uncommented, comment
		endif
	endif
endfunction

function! Commentify(commentSymbol)	
	set nohlsearch	
	execute ':s+^+'.a:commentSymbol.'+'					" go to the beginning of the line and insert the comment symbol
	set hlsearch	
endfunction
	
function! UnCommentify(commentSymbol)	
	set nohlsearch	
	execute ':s+'.a:commentSymbol.'++'					" remove the first comment symbol found on a line
	set hlsearch	
endfunction
			
