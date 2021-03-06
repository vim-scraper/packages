" Vim syntax file
" Language:     XTerm based escape sequences
" Maintainer:   Andy Spencer <andy753421@gmail.com>
" Last Change:  2010-04-01
" TODO:
"   Bold, underline, etc

if exists("b:current_syntax")
  finish
endif

" Hack to let vpaste.net skip email headers
if exists("g:xterm_start_pattern")
	let offset = -search(g:xterm_start_pattern)
else
	let offset = 0
endif

" Set up syntax matching
if exists("g:xterm_trim_escapes") && g:xterm_trim_escapes
	" Loop through lines looking ofr escapes
	for row in range(0, line('$'))
		let line = getline(row)
		let col  = 0
		while 1
			let col = match(line, '[0-9;[]*[a-z]', col)
			if col == -1 | break | endif

			" Processing
			let subs = matchlist(line, '\v\[?(\d+)%(;(\d+))*m', col)
			if subs == []
			elseif subs[1] == 0
				let name = 'xtermReset'
			elseif subs[1] == 1 && subs[2] >= 30 && subs[2] <= 37
				let name = 'xtermColor'.(subs[2]-30)
			elseif subs[1] == 38
				let name = 'xtermColor'.subs[2]
			endif
			if exists("name")
				exec 'syn region '.name.
					\ ' start="\%'.(row+offset).'l\%'.(col+1).'c"'.
					\ ' end="\%$"'.
					\ ' contains=@xtermColors'
			endif
			
			let line = substitute(line, '[0-9;[]*[a-z]', '', '')
		endwhile

		call setline(row, line)
	endfor
else
	" Reset
	syn region  xtermReset start="\[0m" end="\%$" contains=@xtermColors

	" Escapes
	syn cluster xtermColors add=xtermEscape

	" ANSI color
	for i in range(30,37)
		let start='\[\d*;'.i.'m'
		exec 'syn region xtermColor'.(i-30).
			\ ' matchGroup=xtermEscape'.
			\ ' start="'.start.'"'.
			\ ' end="\%$"'.
			\ ' contains=@xtermColors'
	endfor

	" XTerm color
	for i in range(0,255)
		let start='\[38;5;'.i.'m'
		exec 'syn cluster xtermColors add=xtermColor'.i
		exec 'syn region xtermColor'.i.
			\ ' matchGroup=xtermEscape'.
			\ ' start="'.start.'"'.
			\ ' end="\%$"'.
			\ ' contains=@xtermColors'
	endfor
endif

" Set up highlighting
syn cluster xtermColors contains=xtermReset
for i in range(0,255)
	exec 'syn cluster xtermColors add=xtermColor'.i
	exec 'hi xtermColor'.i.' ctermfg='.i
endfor

let b:current_syntax = "xterm"
