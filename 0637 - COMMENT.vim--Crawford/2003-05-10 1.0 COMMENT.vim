function PComment() range
	let line=getline(a:firstline)
	if (line =~ '^#if')
		" remove if block
		execute a:firstline . "normal dd"
		let emb=0
		let i = a:firstline+1
		while i<=line("$")
			let line=getline(i)
			if line =~ '^#if'
				let emb=emb+1
			endif
			if line =~ '^#else'
				if emb == 0
					" not an embedded else so turn else
					" into an #if 0 to match following
					" #endif
					let newline=substitute(line,"^#else","#if 0","")
					call setline(i,newline)
					break
				endif
			endif
			if line =~ '^#endif'
				if emb > 0
					let emb=emb-1
				else
					" found matching endif; remove it
					execute i . "normal dd"
					break
				endif
			endif
			let i=i+1
		endwhile
	else
		" add block
		call append((a:firstline-1),"#if 0")
		call append(a:lastline+1,"#endif")
	endif
endfunction
map <M-c> :call PComment()<CR>
