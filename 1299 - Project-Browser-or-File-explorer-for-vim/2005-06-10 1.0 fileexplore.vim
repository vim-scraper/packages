" File: FileExplore
" Author: Pradeep Kudethur ( pradeeppp_k@yahoo.com )
" Version: 1.0
" Date: 09 June 2005
" 
" Purpose: Emulates the project file browser similar to some popular code browsers.
" This will list all the files in the project in a new window and allow user
" to open any file by mouse click/<CR>/O/V/H. This will come very handy
" when you are dealing with huge number of files and you really dont want to worry
" about the project directory structure, but just about the filename you are working on.
"
" Limitation: This script relies heavily on the Unix find command(initially takes time 
" for explorer to open) and cscope (once the explorer is open things will be fast).
"
" Distribution: This plugin comes with GPL (General Public License), You have
" are free to use/distribute/modify the plugin.
"
" Installation: Copy this file to .vim/plugin folder. 
" I have tried this on free BSD. Should also work on linux and cygwin.
"
" Substitue with path below your project root dir or just '.', in case you are
" working on multiple projects. I use '.' and ensure to open vim from the
" project's root directory, I am interested on.
" map <F5> :FileExplore /\b/\kpradeep/\src<CR> 
" The above line can be placed in your '.vimrc' file
" 
" You need to specify F5 (in my example, refer the map command above to change the same) 
" on your gvim/vim to open the file explorer. The window split format (horizontal/vertical/none) 
" configuration is supported below. The file explorer will come up (initially it will take time, 
" because it uses the unix find command) with all the files (.c and .h file, support is provided 
" below to specify more extensions) in ascending order. User can use '\' or '\^' (search from 
" first letter of the file) vi commands to go the appropriate file in the file explorer and press 
" <CR> to open the file in the specified window (vert/hor/none - you can tune this parameter below).
" User can also press 'O'/ double click left mouse button to open the file instead of <CR>.
" 'V' to open the file in vertical split (this will override the split stype configuration).
" 'H' to open the file in horizontal split (this will override the split stype configuration).
" Vi command '?'  or '?^' can also be used for searching the file in the file explorer 
" (regular expression search should also work).
"
" FYI: the file opening strongly depends on the 'cscope find f' command, so please ensure the 
" cscope configuration is proper and you can acutally open the interested file using " ':cs f f <filename>' 
" command from the vim/gvim editor itself. For information on using/setting up cscope please go to 
" 'http://cscope.sourceforge.net/' and 'http://cscope.sourceforge.net/cscope_vim_tutorial.html'
"
" set path to the find binary
let s:find_path='/usr/bin/find'

" set filter -- see find man page for details
let s:filter='-name *.c -or -name *.h'

" set splitting behavior : none | vert | horiz
let s:splitBehavior='vert'

" multiple define check
if !exists(':FileExplore')
	command -nargs=1 -complete=dir FileExplore call s:FileBrowse('<args>')
endif

" FileBrowse
" @param dir -- directory used to recursively assemble file list 
function! s:FileBrowse( dir )
	if "vert" == s:splitBehavior
		botright vertical new 
	elseif "horiz" == s:splitBehavior
		new
	endif

	setlocal bufhidden=wipe buftype=nofile noswapfile
	let headers = a:dir . '/*.h'
	let cpps = a:dir . '/*.cpp'
	exec ':r!' . s:find_path . ' ' . a:dir . ' ' . s:filter
	:0
	setlocal ignorecase nohlsearch

    exec ':1,$s/\..*\///g' 
    exec ':%!sort' 

	noremap <buffer> <CR> :call <SID>EditFile()<CR>
    nnoremap <silent> <buffer> O :call <SID>EditFile()<cr>
    nnoremap <silent> <buffer> <2-LeftMouse> :call <SID>EditFile()<cr>
    nnoremap <silent> <buffer> V :call <SID>EditFileVer()<cr>
    nnoremap <silent> <buffer> H :call <SID>EditFileHor()<cr>
endfunction

" EditFile -- open the file on the current line in new buffer. must be full path
	function! s:EditFile()
	" Are we on a line with a file name?
	let l = getline(".")
	if l =~ '^"'  " checks for comments
		return
	endif

	if "vert" == s:splitBehavior
		vertical new
	elseif "horiz" == s:splitBehavior
		new
	endif
	"exec ':edit ' . l
    exec ':cs find f '  . l
endfunction

" EditFileVer -- open the file on the current line in vertical split. must be full path
	function! s:EditFileVer()
	" Are we on a line with a file name?
	let l = getline(".")
	if l =~ '^"'  " checks for comments
		return
	endif

	vertical new
	"exec ':edit ' . l
    exec ':cs find f '  . l
endfunction

" EditFileHor -- open the file on the current line in horizontal split. must be full path
	function! s:EditFileHor()
	" Are we on a line with a file name?
	let l = getline(".")
	if l =~ '^"'  " checks for comments
		return
	endif

    new
	"exec ':edit ' . l
    exec ':cs find f '  . l
endfunction
