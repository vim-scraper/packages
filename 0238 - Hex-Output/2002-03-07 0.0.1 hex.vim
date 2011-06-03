" Output a number as Hexadecimal
" Author: Michael Geddes
" Version: 0.0.1

fun! HexOut(number)
	let hexdig='0123456789abcdef'
	let no=a:number
	let neg=''
	if no < 0
		let neg='-'
		let no=-no
	endif
	let result=''
	while no>0 
		let result=hexdig[no%16].result
		let no=no/16
	endwhile
	return neg.result
endfun
