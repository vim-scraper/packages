" System IO Automation
" REName DELete
"=============================================================================
" Informations : Author and more (press za to open this under Vim)  {{{1
"
" Author: niva
"
" Website: http://lemachupicchu.fr
"
" Main Feature: Enable vim user to rename (or delete) file when cursor pass 
" over its filename/filepath.
"
" Usage: Just type :REN or :DEL to do some action onfile under cursor
"
" Recognition priority :
"
"   0.	Check that filename under cursor exists and is in an opened NERDTree buffer
"   1.	Check that filename under cursor is in the current directory
"   2.	Check that the filepath under cursor exists
"   3.	Check that file opened in the current buffer exists
"
"
" Versions:
" version 1.0 Initial Upload
" version 1.1 Compatibility win32 and unix OS
" version 1.2 Add new rename feature when cursor is over filepath
"             Add Delete feature
" version 1.3 Add Dictionaries to store commands
"             Enable delete files in NERDTree that have spaces inside filename
" 
" Last date revision : 20/04/2011
"
" }}}1
"=============================================================================
" Dictionaries of commands depending on OS {{{1
"
let s:dwin    = { 'REN': {'cmd': 'rename', 'parA':'tofeed', 'parB':'tofeed'}, 
			    \ 'DEL': {'cmd': 'del /F', 'parA':'tofeed', 'parB':''} 
				\ }

let s:dunix   = { 'REN': {'cmd': 'mv -f' , 'parA':'tofeed', 'parB':'tofeed'}, 
                \ 'DEL': {'cmd': 'rm -rf', 'parA':'tofeed', 'parB':''}
				\ }


" List of patterns to substitute depending on OS
let s:lpatwin   = [ ['\/','\\'], ['\\\\','\\'], ['\\','\'] ] 
let s:lpatunix  = [ ['\\','\/'], ['\/\/','\/'] ] 

"}}}1
"=============================================================================
" Function to init Dicts and Lists {{{1
function! s:InitDicts()

	let s:cmdDic  = {}

	if !empty(s:cmdDic)
		echo "dict not empty"
	endif

	let s:patList = []

	let s:cmdDic =deepcopy(s:dunix)
	let s:patList=deepcopy(s:lpatunix)

	if has("win32")
		let s:cmdDic=deepcopy(s:dwin)
		let s:patList=deepcopy(s:lpatwin)
	endif
endfunc
" }}}1
"=============================================================================
" Function to rename file under cursor (in NERDTree)  {{{1
function! s:SysIOAction(action)

    call s:InitDicts() 

	let l:fileToAlter=""
	let l:pathOffileToAlter=""

	" 
	" Order of checkup to rename
	"
	" 0 NerdTree, 
	" 1 filename under cursor in current directory
	" 2 filepath under cursor
	" 3 filename of opened buffer
"=============================================================================
		" Assign command  {{{2
		"
		let actionTitle="Rename this file to \n"

		if a:action=="DEL"
			let actionTitle="Delete this file \n"
		endif
		" }}}2
"=============================================================================
	let case=-1

	if  stridx(bufname("%"),"NERD")>-1
"=============================================================================
		" Specific NERDTree buffer case {{{2
		let case=0

		" sauvegarde du nom du fichier
		" let l:fileToAlter=substitute(expand("<cfile>"), "-", "", ""  )
		let l:fileToAlter=substitute(getline(line(".")), '[|`-]', '', "g"  )

		mark a

		" recherche du repertoire racine
		norm P
		let lgRepertoireRacine=line(".")

		" parcours des repertoires intermediaires
		norm 'a
		let middleDirectories=""
		while  line(".")>lgRepertoireRacine
			norm p
			if  line(".")>lgRepertoireRacine
				let middleDirectories="/".substitute(getline(line(".")),"[|~]", "", "g").middleDirectories
				let middleDirectories=substitute(middleDirectories,'\s', '', "g") 
			endif
		endwhile

		mark a
		norm Pcd
		let l:pathOffileToAlter=getcwd()."/".middleDirectories
		" End of Specific NERDTree buffer case }}}2
"=============================================================================
	else
		if filereadable(getcwd()."/".expand("<cword>"))!=0
"=============================================================================
		" File Under cursor {{{2
			let case=1
			echo "file under cursor ".filereadable(getcwd()."/".expand("<cword>"))
			let l:fileToAlter=expand("<cword>")
			let l:pathOffileToAlter=getcwd()
		" End of File Under cursor }}}2
"=============================================================================
		else
"=============================================================================
			if filereadable(expand("<cWORD>:p"))!=0
				let case=2
				" FilePath under cursor {{{2
				let l:fileToAlter=fnamemodify("".expand("<cWORD>"), ":p:t")
				let l:pathOffileToAlter=fnamemodify("".expand("<cWORD>"), ":p:h")
				" End of FilePath under cursor }}}2
			else
				" Current Opened Buffer {{{2
					if filereadable(expand("%:p"))!=0
						let case=3
						exe "cd ".expand("%:p:h")
						let l:fileToAlter=expand("%:p:t")
						let l:pathOffileToAlter=getcwd()
					else
						" echo "none of file under cursor or opened under this buffer exist"
					endif
			endif
		" End of Current Opened Buffer }}}2
"=============================================================================
		endif
	endif
"=============================================================================
	" Ask user to alter the file to {{{2
	let l:newFileName=inputdialog(actionTitle, l:fileToAlter)
	if l:newFileName != ""

		" parA
		call s:FeedCmdParA(a:action,l:pathOffileToAlter."/".l:fileToAlter)

		" parB
		if has("win32")
			call s:FeedCmdParB(a:action,l:newFileName)
		else
			call s:FeedCmdParB(a:action,l:pathOffileToAlter."/".l:newFileName)
		endif

		let g:toto=s:BuildCmd(a:action)
		call system(s:BuildCmd(a:action))

		if case==0
			norm 'a
			silent norm pR
		endif

	endif
	" End of Asking user to rename the file to }}}2
"=============================================================================
endfunction 
"}}}1
"=============================================================================
" Function to feed param if needed {{{1
function! s:FeedCmdParA(action,arg)
    if s:cmdDic[a:action].parA=="tofeed"
    	let s:cmdDic[a:action].parA=s:SubSomeChars(a:arg)
	endif
endfunc
function! s:FeedCmdParB(action,arg)
    if s:cmdDic[a:action].parB=="tofeed"
		" echo s:cmdDic[a:action]
    	let s:cmdDic[a:action].parB=s:SubSomeChars(a:arg)
	endif
endfunc
"}}}1
"=============================================================================
" Function to sub some non wanted characters {{{1
function! s:SubSomeChars(var)
	let l:tmp=a:var
    for item in s:patList 
	   let l:tmp=substitute(l:tmp, ''.item[0], ''.item[1], "g") 
    endfor             
	return l:tmp
endfunc
"}}}1
"=============================================================================
" Function that build the command to launch {{{1
function! s:BuildCmd(action)
   let l:cmd=s:cmdDic[a:action].cmd
   let l:cmd.=' '
   let l:cmd.=s:AddQuoteOnMSWin()
   let l:cmd.=s:cmdDic[a:action].parA
   let l:cmd.=s:AddQuoteOnMSWin()
   let l:cmd.=' '
   let l:cmd.=s:cmdDic[a:action].parB
   return l:cmd
endfunc
"}}}1
"=============================================================================
" Function that add quote on commands specific for windows {{{1
function! s:AddQuoteOnMSWin()
   if has("win32")
   	return '"'
   endif
endfunc
"}}}1
"=============================================================================
command! -nargs=0 -complete=file REN call s:SysIOAction("REN")  
command! -nargs=0 -complete=file DEL call s:SysIOAction("DEL")  
" vim: set ft=vim ff=unix fdm=marker ts=4 :expandtab:
