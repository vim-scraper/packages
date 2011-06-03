
" --- LCSCHECK START -------------------------------------------------------
" check if 'listchars' includes an eol character, and if yes, test if >127
" because only values <128 are valid (i.e., single-byte) under utf-8.
	let s:lcix = stridx (&lcs,"eol:")
	if s:lcix >= 0
" if >= 0, s:lcix contains the number of chars to skip before "eol:"
" if <0 (i.e., -1) then there is no "eol:" substring, we can skip the rest.
		let s:lcl = strlen(&lcs)
" s:lcl = total length of the option string
		let s:lcix = s:lcix + 4
" point s:lcix to the eol character itself
		let s:lcs1 = strpart(&lcs,0,s:lcix)
" s:lcs1 = string 1, up to & including "eol:"
		let s:lcnewl = s:lcix + 1
" s:lcnewl = length of the new lcs value we are building up
" now we will be getting the eol character itself into s:lceol
" and the rest of the option text (if any) into s:lcs2
		if s:lcnewl > s:lcl
" if there is nothing after "eol:" we set the eol character to a space
			let s:lceol = " "
			let s:lcs2 = ""
		else
			let s:lceol = strpart(&lcl,s:lcix,1)
			if s:lcnewl == s:lcl
				let s:lcs2 = ""
			else
				let s:lcs2 = strpart(&lcl,s:lcnewl,(s:lcl - s:lcnewl))
				let s:lcnewl = s:lcnewl + strlen(s:lcs2)
			endif
		endif
" We have cut the original 'lcs' option text into 3 parts:
" s:lcs1 up to & including the characters "eol:"
" s:lceol = the eol character (always 1 byte)
" s:lcs2 = the rest (zero or more bytes).
" Now we check the byte value of s:lceol. If <128 it stays unchanged,
" otherwise it becomes "$" to avoid values unacceptable under utf-8.
		if char2nr(s:lceol) >= 128
			let s:lceol = "$"
		endif
" Now we are ready to rebuild the option value
		let &lcs = s:lcs1 . s:lceol . s:lcs2
" The only thing left in this lcs-checking-routine is tidying up
		unlet s:lcs1 s:lceol s:lcs2 s:lcl s:lcnewl
	endif
	unlet s:lcix
" --- LCSCHECK END ----------------------------------------------------------

