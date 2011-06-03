"=============================================================================
"    Copyright: Copyright (C) 2010 Romain Maurer
"               Permission is hereby granted to use and distribute this code,
"               with or without modifications, provided that this copyright
"               notice is copied with it. Like anything else that's free,
"               GetFile.vim is provided *as is* and comes with no
"               warranty of any kind, either expressed or implied. In no
"               event will the copyright holder be liable for any damages
"               resulting from the use of this software.
" Name Of File: getfile.vim
"  Description: Select an Open a File by typing part of its path. This plugin
"               has been inspired by the FindFile plugin.
"   Maintainer: Romain Maurer
" Last Changed: 28 April 2010
"      Version: 1.0.3
"        Usage: This file should reside in the plugin directory and be
"               automatically sourced.
"
"               You can use the following functions:
"
"                 ":GetFileCacheFiles <path>" - Caches the files. Has to be called at
"                 least once before a call to ":GetFile".
"                 ":GetFileClearCache" - Clear the cache.
"                 ":GetFile" - Start searching for a file.
"
"               You may want to add the following lines in your ~/.vimrc file:
"               (or whatever shortcut you find suitable)
"
"               "nmap <silent> <unique> fc :GetFileCacheFiles .<CR>"
"               "nmap <silent> <unique> ff :GetFile<CR>"
"
"               Once in the "[GetFile]" buffer:
"                 
"                 Use "<Up>" / "<C-p>" and "<Down>" / "<C-n>" to select a file. 
"                 "<CR>" opens the selected file. (If no selection done, it
"                 opens the first file).
"                 "<ESC>" exits the buffer.
"
"               Supported Options that you can specify in your .vimrc file:
"
"                   "g:GetFileIgnoreList" 
"                       List of patterns to ignore.
"                       Default = ['*.o', '*.pyc', '*/tmp/*', '*~', '*/.svn/*', '*#']
"
"                   "g:GetFileIgnoreCase"
"                       Do not take case into account when searching files.
"                       Default = 0
"
"                   "g:GetFileAutoFillFolder"
"                       Speeds up the search when searching through folders as
"                       typing '/' is equivalent to typing '*/'.
"                       There are 3 possible values:
"                           0: do not use this option
"                           1: auto complete the current folder only. (match till
"                           the next '/' only)
"                           2: auto complete the entire sub hierarchy. (match
"                           till the last possible '/')
"                       Default = 1
"
"               Example of how to set an option in you .vimrc file:
"               
"                   "let g:GetFileAutoFillFolder = 2"
"
"      History: See at the end of the file
"      Todo: 
"           .   Have the possibility to populate the cache using a locate
"               database instead of globing. 
"               Users would have to provide the path to the database.
"               Priority: low
"=============================================================================
"

if exists('g:loaded_getFile') || &cp
    finish
endif
let g:loaded_getFile=1

" Create commands {{{1
command -nargs=*  -complete=dir GetFileCacheFiles :call s:DoCacheFiles(<f-args>)
command GetFile :call s:DoGetFile()
command GetFileClearCache :call s:DoCacheClear()

" Default values {{{1
if !exists("g:GetFileIgnoreList")
    let g:GetFileIgnoreList = ['*.o', '*.pyc', '*/tmp/*', '*~', '*/.svn/*', '*#']
endif
if !exists("g:GetFileIgnoreCase")
    let g:GetFileIgnoreCase = 0
endif
if !exists("g:GetFileAutoFillFolder")
    let g:GetFileAutoFillFolder = 1
endif

" Script variables {{{1
let s:fileCache = []
let s:selection = ""

" DoCacheFiles {{{1
function! s:DoCacheFiles(...)
    echo "Caching files..."
	for d in a:000
		"Creates the dictionary that will parse all files recursively
		for i in g:GetFileIgnoreList
			let s = "setlocal wildignore+=" . i
			exe s
		endfor
		let files = glob(d . "/**")
		for i in g:GetFileIgnoreList
			let s = "setlocal wildignore-=" . i
			exe s
		endfor
		let ctr = 0
		for f in split(files, "\n")
			let fpath = fnamemodify(f, ":p")
			" We only glob the files, not directory
			if !isdirectory(fpath)
				" If the cache already has this entry, we'll just skip it
                if index(s:fileCache, fpath) == -1
                    :call add(s:fileCache, fpath)
                    let ctr = ctr + 1
                endif
			endif
		endfor
		echo "Found " . ctr . " new files in '" . d . "'. Cache has " . len(s:fileCache) . " entries."
	endfor
endfunction

" CacheClear {{{1
function! s:DoCacheClear()
	let s:fileCache = []
	echo "GetFile cache cleared."
endfunction

" DoGetFile {{{1
function! s:DoGetFile()

    " Open the buffer
    :call s:OpenBuffer()

    " Key Mapping inside buffer
    inoremap <buffer> <silent> <C-p> <Up>
    inoremap <buffer> <silent> <C-n> <Down>
    inoremap <buffer> <silent> <ESC> <ESC>:bd!<cr>
    inoremap <buffer> <silent> <CR> <ESC>:call <SID>OpenSelection()<cr>

    " As soon as you type, the list of matching paths should be displayed
    " starting on line 2.
    autocmd CursorMovedI,CursorMoved <buffer> call s:UpdateDisplay()

    " Start typing...
    startinsert

endfunction

" OpenBuffer {{{1
function! s:OpenBuffer()
    new [GetFile]
    setlocal buftype=nofile
    setlocal bufhidden=hide
    setlocal noswapfile
    setlocal nocursorline
    setlocal nobuflisted
endfunction

" EscapeChars {{{1
function! s:EscapeChars(toEscape)
	let escaped = escape(a:toEscape, " \!\#\$\^\&\-\=\\\|\~\`\'\[\]")
    " Escaping the following characters is not enough so process them
    " differently
    let escaped = substitute(escaped, '[@]', '[@]', 'g')
    let escaped = substitute(escaped, '[+]', '[+]', 'g')
    let escaped = substitute(escaped, '[%]', '[%]', 'g')
    let escaped = substitute(escaped, '[(]', '[(]', 'g')
    let escaped = substitute(escaped, '[)]', '[)]', 'g')
    let escaped = substitute(escaped, '[.]', '[.]', 'g')
    return escaped
    
endfunction

" ConvertWildcards {{{1
function! s:ConvertWildcards(searchString)
    let updatedString = substitute(a:searchString, '[*]', '.*', 'g')
    let updatedString = substitute(updatedString, '[?]', '.', 'g')

    " Special mode for faster search through folder
    if g:GetFileAutoFillFolder == 1
        " Only one folder completion
        let updatedString = substitute(updatedString, '[/]', '[^/]\\{-}/', 'g')
    elseif g:GetFileAutoFillFolder == 2
        " Last '/' match
        let updatedString = substitute(updatedString, '[/]', '.*/', 'g')
    endif

    return updatedString
endfunction

" DisplayMatchingFiles {{{1
function! s:UpdateDisplay()
    " Behavior depends on the current line
    let currentLine = line(".")
    
    " Line 1 : search for file
    if currentLine == 1

        setlocal modifiable
        setlocal nocursorline
        
        " Get filename from line 1
        let pattern = getline(1)

        let pattern = s:EscapeChars(pattern)
        let pattern = s:ConvertWildcards(pattern)

        if g:GetFileIgnoreCase
            let pattern = '\c' . pattern
        endif

        silent exe '3match Search +'.pattern.'+'

        let results = []

        for k in s:fileCache
            if match(k, pattern) != -1
                :call add(results, k)
            endif
        endfor

        let colPos = col(".")
        " Clear the screen
        let buflen = line("$")
        if buflen > 1
            silent exe "2,$d"
            let failed = cursor(1, colPos)
        endif

        for i in results 
            let failed = append(1, i)
        endfor
        " By default the first line is selected
        let s:selection = getline(2)

    " Line > 1 : highlight and select
    else
        " Get the selection
        let s:selection = getline(currentLine)
        setlocal nomodifiable
        setlocal cursorline
    endif
endfunction

function! <SID>OpenSelection()
    " if no selection close the buffer
    if len(s:selection) == 0
        silent exe "bd!"
        echo "No file specified."
        return
    endif
    
    if filereadable(s:selection)
        " Clear the buffer for later use
        setlocal modifiable
        silent exe "bd!" 
        silent exe "edit " . s:selection
        echo "Loaded File " . s:selection  
    else
        echo "File " . s:selection . " not found. Maybe the cache is not up to date. Press i to go back in insert mode."
    endif
endfunction

" -----------------------------------------------------------------------------
" History:
"   1.0.3   Fixed the <ESC> that wasn't closing the search window when no
"           characters were on search line.
"           Fix the issue when vim crashes when a * is on the search line.
"           Added support for wildcards search (*/?). Added option for fast search
"           through folder hierarchy.
"           The '.' character is now handled properly.
"   1.0.2   Added a case-sensitivity search option.
"   1.0.1   Added a function to clear the cache, <C-n>/<C-p> to go Up and Down
"           and removed the possibility to go in command mode.
"   1.0     Initial Release.
