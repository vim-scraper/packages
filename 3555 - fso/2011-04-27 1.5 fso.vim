" File System Operations on file under cursor
" Renaming
" Deletion
" Informations
" Browse filepath
"=============================================================================
" Informations : Author and more (press za to open this under Vim)  {{{1
"
" Author: niva
"
" Mail: nivaemail@gmail.com
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
" version 1.4 Fix bug with whitespace into NERDTree
"             Extend features to include files as webdeveloppers files (html,css,xhtml))
" version 1.5 Add Menu Feature and opening filepath in windows browser
" 			  Rename script because of added features
" 
" Last date revision : 20/04/2011
"
" }}}1
"=============================================================================
" Dictionaries of commands depending on OS {{{1


" List of patterns to substitute depending on OS
let s:lpatwin   = [ ['\/','\\'], ['\\\\','\\'], ['\\','\'] ] 
let s:lpatunix  = [ ['\\','\/'], ['\/\/','\/'] ] 

"}}}1
"=============================================================================
" Function to init Dicts and Lists {{{1
function! s:InitDicts()

	let g:cmdDic  = {
	\ 'REN'     : {'maincall': 'call '   , 'cmd': ['rename']  												, 'parA':'tofeed', 'needparAFileOrPath':'f', 'parB':'tofeed', 'actionQuestion':'Rename this file to'         , 'needBracket':'yes'}, 
	\ 'DEL'     : {'maincall': 'call '   , 'cmd': ['delete']  												, 'parA':'tofeed', 'needparAFileOrPath':'f', 'parB':''      , 'actionQuestion':'Delete this file'            , 'needBracket':'yes'},
	\ 'BROZ'    : {'maincall': 'system ' , 'cmd': ['explorer.exe /select,']									, 'parA':'tofeed', 'needparAFileOrPath':'b', 'parB':''      , 'actionQuestion':'Open directory of this file' , 'needBracket':'no' },
	\ 'INFOS'   : {'maincall': 'echo '   , 'cmd': ['getftype','getfperm','strftime("%Y %b %d %X",getftime']	, 'parA':'tofeed', 'needparAFileOrPath':'f', 'parB':''      , 'actionQuestion':'Get permissions of this file', 'needBracket':'yes'}
	\}

	if !empty(g:cmdDic)
		echo "dict not empty"
	endif

	let s:patList = []

	let s:patList=deepcopy(s:lpatunix)

	if has("win32")
		let s:patList=deepcopy(s:lpatwin)
	endif
endfunc
" }}}1
"=============================================================================
" Function to detect file under cursor (in NERDTree)  {{{1
function! <SID>:SysIOAction(action)

	if !has("win32")
		if a:action=="BROZ"
			echo "browsing feature not available yet under unix"
			return 0
		endif
	endif
	" echo "asked action:"a:action."."
	" if empty(g:cmdDic)
	" 	echo "dico vide"
	" endif

	call s:InitDicts() 

	let l:memoCurrentDir=getcwd()

	let g:fileToAlter=""
	let g:pathOfFileToAlter=""
	"
	"=============================================================================
	if  stridx(bufname("%"),"NERD")>-1
		"=============================================================================
		" Specific NERDTree buffer case {{{2

		" sauvegarde du nom du fichier
		let g:fileToAlter=substitute(getline(line(".")), '^\(\(|\|`\)\s*`*\)\+-', '', ""  )
		let g:fileToAlter=substitute(g:fileToAlter, '\(\/\|\*\)$', '', ""  )


		mark a

		norm pcd

		let g:pathOfFileToAlter=getcwd()
		" End of Specific NERDTree buffer case }}}2
		"=============================================================================
	elseif filereadable(expand("<cfile>"))!=0
		"=============================================================================
		" File Under cursor {{{2                  
		let g:integralDir=expand("<cfile>")
		exe "cd ".fnamemodify(g:integralDir, ":p:h")
		let g:pathOfFileToAlter=fnamemodify(g:integralDir, ":p:h")
		let g:fileToAlter=fnamemodify(g:integralDir, ":p:t")
		" End of File Under cursor }}}2
		"=============================================================================
	elseif filereadable(expand("%:p:h")."/".expand("<cfile>"))!=0

		let g:integralDir=expand("%:p:h")."/".expand("<cfile>:h")
		exe "cd ".g:integralDir
		let g:pathOfFileToAlter=g:integralDir
		let g:fileToAlter=expand("<cfile>:t")

	elseif filereadable(expand("%:p"))!=0
		" Current Opened Buffer {{{2
		exe "cd ".expand("%:p:h")
		let g:pathOfFileToAlter=expand("%:p:h")
		let g:fileToAlter=expand("%:p:t")
		" End of Current Opened Buffer }}}2
		"=============================================================================
	endif
	"=============================================================================
	" Ask user to alter the file to {{{2
	"
	" parA
	call s:FeedCmdParA(a:action,g:fileToAlter)

	let l:cmdtest=s:BuildCmd(a:action)

	" test to ask or just show question
	if g:cmdDic[a:action].maincall=="call "

		let l:newFileName=inputdialog(g:cmdDic[a:action].actionQuestion, g:fileToAlter)
		if l:newFileName != ""

			" parB
			call s:FeedCmdParB(a:action,l:newFileName)
			let l:cmdtest=s:BuildCmd(a:action)

			exe l:cmdtest

			if stridx(bufname("%"),"NERD")>-1
				norm 'a
				silent norm Kkrj
			endif

		endif
	elseif g:cmdDic[a:action].maincall=="system "
			call system("".l:cmdtest)
			echo 'call system("'.l:cmdtest.'")'
	else
		" just show
		exe l:cmdtest
	endif
	" End of Asking user to rename the file to }}}2
	"=============================================================================


	exe "cd ".l:memoCurrentDir
endfunction 
"}}}1
"=============================================================================
" Function to feed param if needed {{{1
function! s:FeedCmdParA(action,arg)

	if g:cmdDic[a:action].parA=="tofeed"
		if g:cmdDic[a:action].needparAFileOrPath == "f"
			let g:cmdDic[a:action].parA=s:SubSomeChars(a:arg)
		elseif g:cmdDic[a:action].needparAFileOrPath == "b"
			let g:cmdDic[a:action].parA=g:pathOfFileToAlter.s:AddGoodSlashForOS().s:SubSomeChars(a:arg)
		else
			let g:cmdDic[a:action].parA=g:pathOfFileToAlter
		endif
	endif

endfunc
function! s:FeedCmdParB(action,arg)
	if g:cmdDic[a:action].parB=="tofeed"
		" echo g:cmdDic[a:action]
		let g:cmdDic[a:action].parB=','
		let g:cmdDic[a:action].parB.=s:AddQuoteOnMSWin()
		let g:cmdDic[a:action].parB.=s:SubSomeChars(a:arg)
		let g:cmdDic[a:action].parB.=s:AddQuoteOnMSWin()
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

	let l:cmd=g:cmdDic[a:action].maincall

	if g:cmdDic[a:action].maincall == "system "
		let l:cmd=""
	endif

	let l:listLen = len(g:cmdDic[a:action].cmd)
	let l:idx = 1
	for funktion in g:cmdDic[a:action].cmd 

		let l:cmd.=funktion
		if g:cmdDic[a:action].needBracket == "yes"
			let l:cmd.="("
		endif


		let l:cmd.=s:AddQuoteOnMSWin()
		let l:cmd.=g:cmdDic[a:action].parA

		if l:listLen > 1
			let l:cmd.=s:AddQuoteOnMSWin().")"
			" end second funk
			if stridx(funktion,'(') > -1
				let l:cmd.=')'
			endif
			" concat with next function to call
			if l:idx!=l:listLen
				let l:cmd.='." ".'
			endif
		endif
		let l:idx+=1
	endfor             

	if l:listLen == 1
		let l:cmd.=s:AddQuoteOnMSWin()
		let l:cmd.=g:cmdDic[a:action].parB
		if g:cmdDic[a:action].needBracket == "yes"
			let l:cmd.=")"
		endif
	endif
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
" Function that add slash on good os {{{1
function! s:AddGoodSlashForOS()
	if has("win32")
		return '\'
	else
		return '/'
	endif
endfunc
"}}}1
"=============================================================================
" Function that Feed List to display to user {{{1
function! FeedList (findstart, base)
	if a:findstart
	else
		let res=["DEL", "REN", "BROZ", "INFOS"]
		return res
	endif
endfunction
"}}}1
"=============================================================================
" Function that get choice of user {{{1
fun! <SID>:GetChoice() 
	silent norm yy
	" sub carriage return
	let g:choice=substitute(@", '\n', '', "")
	bd!
	" echo g:choice
	call <SID>:SysIOAction(g:choice)
endfunction "}}}1
"=============================================================================
" Function that display listing {{{1
function! <SID>:DisplayListing()

	1split
	enew
	set completefunc=FeedList

	unlet g:choice

	" remap cr in current opened buffer to detect menu choice
	inoremap <buffer> <CR> <CR><Esc>:call <SID>:GetChoice()<CR>

	"
	call feedkeys("i", 'n') 
	call feedkeys("\<C-X>\<C-U>\<Down>", 'n') 

endfunc
"}}}1
"=============================================================================
"standalone cmd
"RENAME
command! -nargs=0 -complete=file REN call <SID>:SysIOAction("REN")  
"DELETE
command! -nargs=0 -complete=file DEL call <SID>:SysIOAction("DEL")  
"OPEN DIR
command! -nargs=0 -complete=file BROZ call <SID>:SysIOAction("BROZ")  
"INFO
command! -nargs=0 -complete=file INFOS call <SID>:SysIOAction("INFOS")  
command! -nargs=0 -complete=file INFO call <SID>:SysIOAction("INFOS")  
command! -nargs=0 -complete=file INF call <SID>:SysIOAction("INFOS")  
"cmd menu
command! -nargs=0 -complete=file LSD call <SID>:DisplayListing()  
" autosource when saving this file
autocmd bufwritepost ~/vimfiles/plugin/ren.vim source ~/vimfiles/plugin/ren.vim
" vim: set ft=vim ff=unix fdm=marker ts=4 :expandtab:
