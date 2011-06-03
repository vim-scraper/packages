" Rename Automation
"=============================================================================

" Function to rename file in buffer, under cursor (in NERDTree)  {{{1
function! Rename()

	let fileToRename=""
	let pathOfFileToRename=""
	" 0 NerdTree, 
	" 1 filename under cursor in current directory
	" 2 filename of opened buffer
	let case=-1

	if  stridx(bufname("%"),"NERD")>-1
"=============================================================================
		" Specific NERDTree buffer case {{{2
		let case=0

		" sauvegarde du nom du fichier
		let fileToRename=substitute(expand("<cfile>"), "-", "", ""  )

		mark a

		" recherche du repertoire racine
		norm P
		let lgRepertoireRacine=line(".")

		" parcours des repertoires intermediaires
		norm 'a
		let repIntermediaire=""
		while  line(".")>lgRepertoireRacine
			norm p
			if  line(".")>lgRepertoireRacine
				let repIntermediaire="/".substitute(getline(line(".")),"[|~]", "", "g").repIntermediaire
				let repIntermediaire=substitute(repIntermediaire,'\s', '', "g") 
			endif
		endwhile

		mark a
		norm Pcd
		let pathOfFileToRename=getcwd()."/".repIntermediaire
		" End of Specific NERDTree buffer case }}}2
"=============================================================================
	else
		if filereadable(getcwd()."/".expand("<cword>"))!=0
"=============================================================================
		" Under cursor {{{2
			echo "file under cursor ".filereadable(getcwd()."/".expand("<cword>"))
			let case=1
			let fileToRename=expand("<cword>")
			let pathOfFileToRename=getcwd()
		" End of Under cursor }}}2
"=============================================================================
		else
"=============================================================================
		" Current Opened Buffer {{{2
			if filereadable(expand("%:p"))!=0
				let case=2
				exe "cd ".expand("%:p:h")
				let fileToRename=expand("%:p:t")
				let pathOfFileToRename=getcwd()
			else
				" echo "none of file under cursor or opened under this buffer exist"
			endif
		" End of Current Opened Buffer }}}2
"=============================================================================
		endif
	endif
"=============================================================================
	" Ask user to rename the file to {{{2
	let newFileName=inputdialog("Rename this file to : ".fileToRename."\n", fileToRename)
	if newFileName != ""

		if has("win32")
			let cmdarg=pathOfFileToRename."/".fileToRename."@@@@".newFileName
			let cmdarg=substitute(cmdarg,'\/', '\\', "g") 
			let cmdarg=substitute(cmdarg,'\\\\', '\\', "g") 
			let cmdarg='rename@@@@'.cmdarg
		else
			let cmdarg=pathOfFileToRename."/".fileToRename."@@@@".pathOfFileToRename."/".newFileName
			let cmdarg=substitute(cmdarg,'\\', '\/', "g") 
			let cmdarg=substitute(cmdarg,'\/\/', '\/', "g") 
			let cmdarg='mv@@@@-f@@@@'.cmdarg
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
command! -nargs=0 -complete=file REN call Rename()  
