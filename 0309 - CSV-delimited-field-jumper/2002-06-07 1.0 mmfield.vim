function MMsetfield()
	let current = 1
	let in_string = 0

	let buf = getline('.')
	let maxbuf = strlen(buf)
	let need_field = input('Jump to field? ')

	let i = 0
	while (i < maxbuf) && (current != need_field)
		if buf[i] == '"'
			if in_string
				let in_string = 0
			else
				let in_string = 1
			endif
		else
			if (buf[i] == ',') && (! in_string)
				let current = current + 1
			endif
					
		endif

		let i = i + 1
	endwhile

	let i = i + 1

	let @a = i . '|'
	normal @a
	echo '  Field: ' . current . ' Column: ' . i
endfunction


function MMgetfield()
	let current = 1
	let in_string = 0

	let buf = getline('.')
	let maxbuf = strlen(buf)
	let need_col = col('.')

	let i = 0
	while (i < maxbuf) && (i < need_col)
		if buf[i] == '"'
			if in_string
				let in_string = 0
			else
				let in_string = 1
			endif
		else
			if (buf[i] == ',') && (! in_string)
				if (i + 1 == need_col)
					break
				endif
				let current = current + 1
			endif
					
		endif

		let i = i + 1
	endwhile

	echo 'Field: ' . current
endfunction


function MMtsetfield()
	let current = 1

	let buf = getline('.')
	let maxbuf = strlen(buf)
	let need_field = input('Jump to field? ')

	let i = 0
	while (i < maxbuf) && (current != need_field)
		if buf[i] == '~'
			let current = current + 1
		endif

		let i = i + 1
	endwhile

	let i = i + 1

	let @a = i . '|'
	normal @a
	echo '  Field: ' . current . ' Column: ' . i
endfunction


function MMtgetfield()
	let current = 1

	let buf = getline('.')
	let maxbuf = strlen(buf)
	let need_col = col('.')

	let i = 0
	while (i < maxbuf) && (i < need_col)
		if buf[i] == '~'
			if (i + 1 == need_col)
				break
			endif
			let current = current + 1
		endif

		let i = i + 1
	endwhile

	echo 'Field: ' . current
endfunction

map <F4> :call MMgetfield()
map <F5> :call MMsetfield()

map <F8> :call MMtgetfield()
map <F9> :call MMtsetfield()

echo '<F4> get-field, <F5> set-field, <F8>/<F9> for tilde'
