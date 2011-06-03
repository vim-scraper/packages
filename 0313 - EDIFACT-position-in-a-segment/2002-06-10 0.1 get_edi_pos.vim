" a function to return the element and fragment position in EDI files
function! GetEdiPosition() 
	let s = getline(".")		" grab current line
	let c = s[0]				" grab line seperator
	let n = 1					" scan along the line
	let nElement = 0			" the element we are in
	let nFragment = 0			" the fragment we are in

	while n < col(".") - 1
		if s[n] == '+'
			let nElement = nElement + 1
			let nFragment = 0
		elseif s[n] == ':'
			let nFragment = nFragment + 1
		endif
		let n = n + 1
	endwhile
	echo 'element =' nElement ', fragment =' nFragment
endfunc 

map _= :call GetEdiPosition()<cr>
