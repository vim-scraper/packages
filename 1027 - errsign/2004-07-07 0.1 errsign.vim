" simple script to place signs on lines with errors

sign define es_sign text=>> linehl=Error
nmap \es :call ES_PlaceSigns()<CR>

function! ES_PlaceSigns()
	normal mX
	let i = 0

	" remove all old marks
	let i = 0
	while 1
		try
			" because of ':sign unplace' silentce
			exe "sign list 111" . i
		catch /.*/
			break
		endtry
		exe "sign unplace 111" . i
		let i = i + 1
	endwhile

	" ugly and buggy method to determine first valid error
	silent! cfirst
	silent! cn
	silent! cp

	while 1
		exe "sign place 111" . i . " name=es_sign line=" . line(".") . " file=" . expand("%:p")
		let i = i + 1
		try
			silent cn
		catch /.*/
			break
		endtry
	endwhile
	normal `X
endfunction
