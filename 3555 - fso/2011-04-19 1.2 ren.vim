" System IO Automation
" REName DELete
"=============================================================================
" Informations : Author and more (press za to open this under Vim)  {{{1
"
" Author: niva
"
" Website: http://lemachupicchu.fr
"
" Features:
"
" Enable vim user to rename (or delete) file when cursor pass 
" over its filename/filepath.
"
" o Filenames/Filepaths recognition
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
" 
" Last date revision : 19/04/2011
"
" }}}1
"=============================================================================
" Function to rename file in buffer, under cursor (in NERDTree)  {{{1
function! SysIOAction(action)

	let fileToAlter=""
	let pathOffileToAlter=""

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
		let winCmd="rename@@@@"
		let unixCmd="mv@@@@-f@@@@"
		let actionTitle="Rename this file to \n"

		if a:action=="DEL"
			let winCmd="del@@@@/F@@@@"
			let unixCmd="rm@@@@-rf@@@@"
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
		let fileToAlter=substitute(expand("<cfile>"), "-", "", ""  )

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
		let pathOffileToAlter=getcwd()."/".middleDirectories
		" End of Specific NERDTree buffer case }}}2
"=============================================================================
	else
		if filereadable(getcwd()."/".expand("<cword>"))!=0
"=============================================================================
		" File Under cursor {{{2
			let case=1
			echo "file under cursor ".filereadable(getcwd()."/".expand("<cword>"))
			let fileToAlter=expand("<cword>")
			let pathOffileToAlter=getcwd()
		" End of File Under cursor }}}2
"=============================================================================
		else
"=============================================================================
			if filereadable(expand("<cWORD>:p"))!=0
				let case=2
				" FilePath under cursor {{{2
				let fileToAlter=fnamemodify("".expand("<cWORD>"), ":p:t")
				let pathOffileToAlter=fnamemodify("".expand("<cWORD>"), ":p:h")
				" End of FilePath under cursor }}}2
			else
				" Current Opened Buffer {{{2
					if filereadable(expand("%:p"))!=0
						let case=3
						exe "cd ".expand("%:p:h")
						let fileToAlter=expand("%:p:t")
						let pathOffileToAlter=getcwd()
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
	let newFileName=inputdialog(actionTitle, fileToAlter)
	if newFileName != ""

		if has("win32")
			if a:action=="REN"
				let cmdarg=pathOffileToAlter."/".fileToAlter."@@@@".newFileName
			endif
			if a:action=="DEL"
				let cmdarg=pathOffileToAlter."/".fileToAlter
			endif
			let cmdarg=substitute(cmdarg,'\/', '\\', "g") 
			let cmdarg=substitute(cmdarg,'\\\\', '\\', "g") 
			let cmdarg=winCmd.cmdarg
		else
			if a:action=="REN"
				let cmdarg=pathOffileToAlter."/".fileToAlter."@@@@".pathOffileToAlter."/".newFileName
			endif
			if a:action=="DEL"
				let cmdarg=pathOffileToAlter."/".fileToAlter
			endif
			let cmdarg=substitute(cmdarg,'\\', '\/', "g") 
			let cmdarg=substitute(cmdarg,'\/\/', '\/', "g") 
			let cmdarg=unixCmd.cmdarg
		endif

		let cmdarg=substitute(cmdarg,'\s', '', "g") 
		let cmdarg=substitute(cmdarg,'@@@@', ' ', "g") 
		echo cmdarg
		call system(cmdarg)

		if case==0
			norm 'a
			norm pR
		endif

	endif
	" End of Asking user to rename the file to }}}2
"=============================================================================
endfunction 
"}}}1
"=============================================================================
command! -nargs=0 -complete=file REN call SysIOAction("REN")  
command! -nargs=0 -complete=file DEL call SysIOAction("DEL")  
" vim: set ft=vim ff=unix fdm=marker ts=4 :expandtab:
