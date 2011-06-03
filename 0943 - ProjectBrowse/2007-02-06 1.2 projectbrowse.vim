" File: projectbrowse.vim
" Author: Jon Heiner ( jheiner@vcentertainment.com )
" Author: Steve An ( san@vcentertainment.com )
" Version: 2007 Jan 6
"
" Emulates the project file browser in MS Visual .NET that is
" provided by the popular Visual Assist extension 
" to list all files below a subdirectory.
" This script relies heavily on the Unix find command available
" universally on Unix and via Cygwin on Windows.
" 
" Suggested Usage -- read in the cache, instead of doing a full find
" This is the main command. Use the <A-p> mapping below to refresh cache
" map <A-o> :ProjectBrowseCached<CR>
"
" Suggested usage -- substitue with your project root dir
" map <A-O> :ProjectBrowse /usr/dev/project_foo/<CR>
" map <A-O> :ProjectBrowse c:\Projects\<CR>
" 
" You can put multiple directories too!  Just separate by a space
" map <A-O> :ProjectBrowse c:\Projects\ c:\Libraries\ c:\Includes
"
" browse and open in new horiz split
" map <A-n> <C-W>n:ProjectBrowseCached<CR>
"
" set path to the find binary
" for unix, it's almost always in the path
" for windows/other specify it explicitly
"let s:find_path='find'
let s:find_path='e:/bin/cygwin/bin/find.exe'

" file to save off list to
let s:cache_path='e:/tmp/vim-projectbrowse-cache.txt'
"
" set filter -- see find man page for details
let s:filter=''
"let s:filter='-name *.cpp -or -name *.h'

" set splitting behavior : none | vert | horiz
let s:splitBehavior='none'

" multiple define check
if !exists(':ProjectBrowse')
	command -nargs=1 -complete=dir ProjectBrowse call s:DoBrowse('<args>')
endif

" multiple define check
if !exists(':ProjectBrowseCached')
	command -nargs=0 -complete=dir ProjectBrowseCached call s:OpenCacheFile()
endif

" helper
function! s:SetupFileListInterface()
	noremap <buffer> <CR> :call <SID>EditFile('edit')<CR>
	noremap <buffer> U    :call <SID>EditFile('vs')<CR>
	noremap <buffer> S    :call <SID>EditFile('sp')<CR>
endfunction

function! s:OpenCacheFile()
	exec ":e " . s:cache_path
	setlocal bufhidden=wipe buftype=nowrite buftype=nofile noswapfile
	" put cursor at top of file
	:0
	call s:SetupFileListInterface()
endfunction

" DoBrowse
" @param dir -- directory used to recursively assemble file list 
function! s:DoBrowse( dir )
	if "vert" == s:splitBehavior
		vertical new
	elseif "horiz" == s:splitBehavior
		new
	else
		if &modified
			let choice = confirm("File has been modified", "&Save\n&Continue\n&Abort")
			if choice == 1
				write
				enew
			elseif choice == 2
				:hide enew
			else
				return
			endif
		else
			enew
		endif
	endif

	setlocal bufhidden=wipe noswapfile
	exec ':r!' . s:find_path . ' ' . a:dir . ' ' . s:filter
	:0
	call s:SetupFileListInterface()

	" cache list to file
	exec ":w! " . s:cache_path

	"now that it's saved, remove file association
	setlocal buftype=nofile 
endfunction

" EditFile -- open the file on the current line. must be full path
" editCmd can be 'edit', 'vs', 'sp', etc.
function! s:EditFile( editCmd )
	" Are we on a line with a file name?
	let l = getline(".")
	if l =~ '^"'  " checks for comments
		return
	endif

	exec ':'.a:editCmd.' ' . l
endfunction
