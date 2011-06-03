" xmltolatex.vim: Convert XML files into LaTeX tabbing environment
" Author: Eustaquio 'TaQ' Rangel <eustaquiorangel@yahoo.com>
" http://beam.to/taq
" $Date: 04/05/2004 $
" $Revision: 0.2 $
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
	let s:sz=5
	let s:gs=inputdialog("Value for tab length (mm)",s:sz)
	if s:gs == ""
		return
	endif	
	let s:sz=s:gs
	
	" count the max tabs on all the lines
	let s:rc=line("$")
	let s:mt=0
	let s:i=1
	
	" loop on all rows
	while s:i<=s:rc
		let s:pos=0
		let s:cnt=0
		" loop on the current row till no matches found
		while s:pos>=0
			let s:pos=match(getline(s:i),"[[:tab:]]",s:pos)
			if s:pos>=0
				let s:cnt=s:cnt+1
				let s:pos=s:pos+1
			endif
		endwhile
		if s:cnt>s:mt
			let s:mt=s:cnt
		endif	
		let s:i=s:i+1
	endwhile 	

	" replace stuff	
	%s/>$/\>\\\\/g
	%s/[[:tab:]]/\\>/g
	%s/\#/\\#/g
	%s/_/\\_/g
	%s/\$/\\$/g
	
	" starts the tabbing environment
	:0
	norm O\begin{tabbing} 
	let s:cnt=0
	norm o
	while s:cnt<s:mt
		exec "norm i \\hspace*{".s:sz.".mm} \\= "
		let s:cnt=s:cnt+1
	endwhile
	norm i \kill 

	" ends the tabbing environment
	:$
	norm o\end{tabbing}
endfunction

map ,x :call XML2Latex()<cr>
com Xml2latex call XML2Latex()
