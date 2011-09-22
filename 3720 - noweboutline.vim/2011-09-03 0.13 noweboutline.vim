"TASK: check if a buffer with a specific name is existing
"TASK: create a new buffer if the buffer did not exists.
function! s:selectChunk(...)
	let chunkIndex = line('.')
	let chunk = get(b:currentList,chunkIndex)
	let bufferNumber = get(chunk,'bufnr')
	let lineNumber = get(chunk,'lnum')
	let cmd = 'q|buffer '.bufferNumber
	execute cmd
	call cursor(lineNumber,0)
endfunction
function! s:showOutlineWindow(...)
	if !exists("s:chunkBufName")
		let s:chunkBufName = 'nwChunkBuf'
		while bufexists(s:chunkBufName)
			let s:chunkBufName = '_'.s:chunkBufName.'_'
		endwhile
	endif
	let s:chunkBufNum = bufnr(s:chunkBufName,1)
	"TASK: outline current buffer
	let s:tmp = getloclist(0)
	"TASK: get code chunks via a location list
	lvimgrep /^<<[^>]\+>>=$/j %
	let b:nwCodeChunkList = getloclist(0)
	let s:nwCodeChunkList = b:nwCodeChunkList
	"TASK: get sections via a location list
	lvimgrep /\\\(sub\)*section/j %
	let b:nwSectionList = getloclist(0)
	let s:nwSectionList = b:nwSectionList
	"TASK: get a section & chunk list
	lvimgrep /\(^<<[^>]\+>>=$\)\|\(\\\(sub\)*section\)/j %
	let b:nwOutlineList = getloclist(0)
	let s:nwOutlineList = b:nwOutlineList
	"TASK: clear location list in current buffer
	call setloclist(0,s:tmp)
	unlet s:tmp
	"TASK: create a new window with specific name.
	vsplit
	"TASK: switch to outline buffer
	exec "silent keepjumps ".(has("gui") ? "drop" : "hide edit" )." ".s:chunkBufName
	setlocal buftype=nofile
	setlocal modifiable
	setlocal noswapfile
	setlocal wrap
	setlocal fdm=indent
	let b:nwCodeChunkList = s:nwCodeChunkList
	let b:nwSectionList = s:nwSectionList
	let b:currentList = s:nwOutlineList
	"call setline(0,s:nwSectionList)
	"TASK: insert code chunks to specific buffer
	let i = 0
	let indent = '' 
	for chunk in b:currentList
		let title = get(chunk,'text')
		if 0 == match(title,'\\\(sub\)*section')
			let indent = ''
			let indentCount = (match(title,'section')-1)/3
			while indentCount > 0
				let indent = indent.'	'
				let indentCount = indentCount - 1
			endwhile
			let currentIndent = indent
		else
			let currentIndent = indent.'	'
		endif
		call setline(i,currentIndent.title)
		let i = i + 1
	endfor
	nnoremap <buffer> <silent> <cr> :call <SID>selectChunk()<cr>
endfunction
nnoremap <buffer> <silent> <C-F1> :call <SID>showOutlineWindow()<cr>
