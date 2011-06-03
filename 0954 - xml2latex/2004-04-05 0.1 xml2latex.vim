" xmltolatex.vim: Convert XML files into LaTeX tabbing environment
" Author: Eustaquio 'TaQ' Rangel <eustaquiorangel@yahoo.com>
" http://beam.to/taq
" $Date: 04/05/2004 $
" $Revision: 0.1 $
"
" Configuration comments:
" Just put this script inside your ~/.vim/plugins dir
"
" Keyboard Commands:
" ,x -> This will convert your current file
" 
" Comments: if you find some more chars to be replaced with the
" \<char> stuff, please send me an email. :-)
" 
" XML2Latex: the only one function here, makes all the stuff
" 
function! XML2Latex()
	" request the tab length (default 5mm)
	let sz=5
	let gs=inputdialog("Value for tab length (mm)",sz)
	if gs == ""
		return
	endif	
	let sz=gs
	
	" count the max tabs on all the lines
	let rc=line("$")
	let mt=0
	let i=1
	
	" loop on all rows
	while i<=rc
		let pos=0
		let cnt=0
		" loop on the current row till no matches found
		while pos>=0
			let pos=match(getline(i),"[[:tab:]]",pos)
			if pos>=0
				let cnt=cnt+1
				let pos=pos+1
			endif
		endwhile
		if cnt>mt
			let mt=cnt
		endif	
		let i=i+1
	endwhile 	

	" replace stuff	
	%s/>$/\>\\\\/g
	%s/[[:tab:]]/\\>/g
	%s/\#/\\#/g
	%s/_/\\_/g
	
	" starts the tabbing environment
	:0
	norm O\begin{tabbing} 
	let cnt=0
	norm o
	while cnt<mt
		exec "norm i \\hspace*{".sz.".mm} \\= "
		let cnt=cnt+1
	endwhile
	norm i \kill 

	" ends the tabbing environment
	:$
	norm o\end{tabbing}
endfunction

map ,x :call XML2Latex()<cr>
com Xml2latex call XML2Latex()
