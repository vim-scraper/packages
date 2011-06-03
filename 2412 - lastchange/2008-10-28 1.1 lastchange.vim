let g:lastChangeName = exists("g:lastChangeName") ? g:lastChangeName : 'Last Change:'
let g:lastChangeSearchStart = exists("g:lastChangeSearchStart") ? g:lastChangeSearchStart : 1
let g:lastChangeSearchEnd = exists("g:lastChangeSearchEnd") ? g:lastChangeSearchEnd : 5
let g:lastChangeAutoWrite = exists("g:lastChangeAutoWrite") ? g:lastChangeAutoWrite : 0
let g:lastChangeAutoWriteFile = exists("g:lastChangeAutoWriteFile") ? g:lastChangeAutoWriteFile : '*.c,*.C,*.cpp,*.cxx,*.h,*.hpp,*.hxx,*.java,*.py'
let g:lastChangeDateFormat = exists("g:lastChangeDateFormat") ? g:lastChangeDateFormat : '%c'

function LastChangeReplace()
	let s:line = line('.')
	let s:col = col('.')
	let s:uline = line('w0')
	call cursor(g:lastChangeSearchStart, 1)
	sil! let s:sr = search(g:lastChangeName, 'W', g:lastChangeSearchEnd)
	if s:sr != 0
		sil! normal zn
		sil! execute "normal d$a".g:lastChangeName." ".strftime(g:lastChangeDateFormat)."\<ESC>"
		sil! normal zN
	endif
	call cursor(s:uline, 1)
	normal zt
	call cursor(s:line, s:col)
endfunction

command LastChangeReplace :call LastChangeReplace()<cr>

function LastChangeWrite()
	if g:lastChangeAutoWrite
		call LastChangeReplace()
	endif
endfunction

execute "autocmd BufWritePre ".g:lastChangeAutoWriteFile." call LastChangeWrite()"
