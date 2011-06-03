" File: browse.vim
" Author: Jon Heiner ( jheiner@pandemicstudios.com )
" Version: 2004 Mar 23
"
" Emulates the project file browser in MS Visual .NET that is
" provided by the popular Visual Assist extension 
" to list all files below a subdirectory.
" This script relies heavily on the Unix find command available
" universally on Unix and via Cygwin on Windows.
" 
" Suggested usage -- substitue with your project root dir
" map <A-o> :ProjectBrowse c:\Projects\<CR>

" set path to the find binary
"let s:find_path='C:/cygwin/bin/find.exe'
let s:find_path='find'

" set filter -- see find man page for details
"let s:filter='-name *.cpp -or -name *.h'
let s:filter=''

" set splitting behavior : none | vert | horiz
let s:splitBehavior='none'

" multiple define check
if !exists(':ProjectBrowse')
	command -nargs=1 -complete=dir ProjectBrowse call s:DoBrowse('<args>')
endif

" DoBrowse
" @param dir -- directory used to recursively assemble file list 
function! s:DoBrowse( dir )
	if "vert" == s:splitBehavior
		vertical new
	elseif "horiz" == s:splitBehavior
		new
	endif

	setlocal bufhidden=wipe buftype=nofile noswapfile
	let headers = a:dir . '/*.h'
	let cpps = a:dir . '/*.cpp'
	exec ':r!' . s:find_path . ' ' . a:dir . ' ' . s:filter
	:0
	setlocal ignorecase nohlsearch

	noremap <buffer> <CR> :call <SID>EditFile()<CR>
endfunction

" EditFile -- open the file on the current line. must be full path
	function! s:EditFile()
	" Are we on a line with a file name?
	let l = getline(".")
	if l =~ '^"'  " checks for comments
		return
	endif

	exec ':edit ' . l
endfunction
