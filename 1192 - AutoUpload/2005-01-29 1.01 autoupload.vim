"=============================================================================
" File:        autoupload.vim
" Author:      Mariusz Gniazdowski (mrg at risp.pl)
" Last Change: Jan 28 23:23:23 CET 2005
" Version:     1.01
"
" How to use
" 1. Create autoupload.info file
"
" 2. Each line in it is an entry in format (first '|' is real char):
" 	filename | {ftp|dav|scp|etc...}://address
" 	
"    >> Address must end with file name! << Example entry:
"    localfile.txt | scp://me@mymachine.com/tmp/file.txt
"
" 3. Then while editing some file that has entry in autoupload.info pressing
" <Leader>up ('u' key and 'p' key) will cause script to:
"    - read autoupload.info from dir in which actual buffer is stored,
"    - search entry for actual buffer in that file,
"    - store actual buffer in remote location.
"
" Customization
" g:autoUploadInfoFileName	- to override default autoupload.info file name
" :call MG_AutoUpload()		- map this to some key combination
"
" Example customization:
" 	let g:autoUploadInfoFileName = ".autoupload.info"
" 	nnoremap <F12> :call MG_AutoUpload()<CR>
"
" Worth to read
" :help netrw
"
" Changes:
" 1.01 - fixed bug when file name bbaa was recognised as matching aabbcc
"
" ============================================================================

if exists('loaded_autoupload') || &cp
  finish
endif
let loaded_autoupload=1

if !exists('g:autoUploadInfoFileName')
	let g:autoUploadInfoFileName = "autoupload.info"
endif

" Default mapping
nnoremap <Leader>up :call MG_AutoUpload()<CR>
" That would be nice too
"nnoremap <Leader><UP> :call MG_AutoUpload()<CR>

"
" Main function.
" 1. Reads netrw.info (which contains entries associating remote locations to file names)
" 2. Lineary searches thru it to find line fitting to actual buffer
" 3. If succeded, then it calls Nwrite to store actual buffer under remote address
"
function MG_AutoUpload()
	let fullInfoFilePath = expand("%:p:h") . "/" . g:autoUploadInfoFileName
	if !filereadable(fullInfoFilePath)
		echomsg "Can't read ".g:autoUploadInfoFileName." file"
		return
	endif
	let actFileName = expand("%:t")
	" Read 'database' file from directory in which current buffer is stored
	exec "sp " . fullInfoFilePath
	let lineCount=line("$")
	let i = 1
	" Process each line (maybe binary search should be added at some stage..)
	while i<=lineCount
		let line = getline(i)
		" File name is every char up to '|'
		let fileName = matchstr(line,"[^|]*")
		let idx = match(fileName, "^[[:space:]]*".actFileName."[[:space:]]*$")
		if idx>=0
			break
		endif
		let i=i+1
	endwhile
	bw!
	" Found?
	if i > lineCount
		echomsg	"There is no entry for this file"
		return
	endif
	let destinationAddress = matchstr(line,".*$", strlen(fileName) +1 )
	exec ":Nwrite ". destinationAddress
endfunction
