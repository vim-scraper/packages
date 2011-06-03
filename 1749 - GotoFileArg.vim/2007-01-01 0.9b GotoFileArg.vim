" GotoFileArg.vim v0.9b
" Provides a routine to jump to a file in the arg list by name, moving to the
"   file itself, in the list, so :n and :N work from the new location
" Filename match is done using "=~" for a regex substring match.
"
" This searches from the start of the args list, but if the current file
" matches the search will search forwards first, then wrap.
"
" This works for me, but it's the first time I've written a vim function,
" so...
"
" brandon {at} mail (dot) voyager {dot} com

map <esc>f :call GotoFileArg()<cr>
let s:lastsrch=""

function! GotoFileArg()
	if s:lastsrch == ""
		let n=input("File name/part: ")
	else
		let n=input("File name/part: [" . s:lastsrch . "]: ")
		if n == ""
			let n=s:lastsrch
		endif
	endif

	" This will re-test n's contents, but it handles both cases if I put it here.
	"
	if n != ""
		let s:lastsrch=n
	endif

	if argv(argidx()) =~ n
		let i=argidx()+1
		while i < argc()
			if argv(i) =~ n | break | endif
			let i=i+1
		endwhile
		if i >= argc()
			let i=0
			while i < argidx()
				if argv(i) =~ n | break | endif
				let i=i+1
			endwhile
			if i == argidx()
				echo "No other matches found.  Remaining on current file."
				return
			endif
			let i=i+1
		endif
	else
		let i=0
		while i < argc()
			if argv(i) =~ n | break | endif
			let i=i+1
		endwhile
		if i >= argc()
			echo "No file matching " n " was found."
			return
		endif
	endif
	let i=i+1
	execute "argu " . i
endfunction
