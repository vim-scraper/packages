" Email Completion for Vim 1.0
" Todd Boland <itodd@itodd.org> http://www.itodd.org/

" Thanks to: Luc Hermitte <hermitte@free.fr>

" Map <tab>
inoremap <tab> <c-r>=TabComplete()<cr>

" Where to look for addresses
let s:addresses = '~/.addresses'

" Function to snag the current string under the cursor
function! SnagString( line )

	" Set column number
	let column =	col('.')-1
	
	" Split up line		line	start	end
	let begining = strpart(	a:line,	0, 	column)

	" Setup string		source		regex
	let string = matchstr(	begining,	'\S*$')
	
	return string
endfunction

" Function to match a string to an email address
function! MatchAddress(string)
	
	" Behold, the power unix!
	let size = system('cat '.s:addresses.' | grep -i ^'.escape(a:string,'\\').' | sort | uniq | wc -l | xargs')
	if size == 1 
		
		" We have an exact match!
		let address = system('cat '.s:addresses.' | grep -i ^'.escape(a:string,'\\').' | sort | uniq')
		
		return address
	endif
endfunction

" Function <tab> is mapped to
function! TabComplete()
	
	" Fetch current line
	let line = getline(line('.'))

	" Is it a special line?
	if line =~ '^\(To\|Cc\|Bcc\):'
		
		" Fetch current string under cursor
		let string = SnagString( line )
		let string_length = strlen(string)
		if string_length > 0
			
			" Try and match that string to an address
			let address = MatchAddress( string )
			let address_length = strlen( address )
			if address_length > 0

				" Hot dang, we've done and got ourselves a match!
				let paste = strpart( address, string_length, address_length )
				" Convert to lower, remove trailing \n, return
				return substitute(tolower(paste),"\n","","g")
			else

				" No address matched
				return ''
			endif
		else

			" No string found, nothing to compare
			return ''
		endif
	else
		
		" Not an address line, return a tab
		return "\t"
	endif
endfunction
