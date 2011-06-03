" Rename Automation
"=============================================================================
" Function to rename file in buffer, under cursor (in NERDTree)  {{{1
function! Rename()

	let fileToRename=""
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
		" End of Specific NERDTree buffer case }}}2
"=============================================================================
	else
		if filereadable(getcwd()."/".expand("<cword>"))!=0
"=============================================================================
		" Under cursor {{{2
			echo "file under cursor ".filereadable(getcwd()."/".expand("<cword>"))
			let case=1
			let fileToRename=expand("<cword>")
		" End of Under cursor }}}2
"=============================================================================
		else
"=============================================================================
		" Current Opened Buffer {{{2
			if filereadable(expand("%:p"))!=0
				let case=2
				exe "cd ".expand("%:p:h")
				let fileToRename=expand("%:p:t")
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

		" NERD
		if case==0
			mark a
			norm Pcd
			let cmdarg=substitute(getcwd()."/".repIntermediaire.fileToRename,'\/', '\\', "g") 
		else
			if case==1 || case==2
				let cmdarg=substitute(getcwd()."/".fileToRename,'\/', '\\', "g") 
			endif
		endif

		"
		let cmdarg=substitute(cmdarg,'\\\\', '\\', "g") 
		let cmdarg=substitute(cmdarg,'\s', '', "g") 
		let cmdarg.=' '.newFileName
		echo cmdarg
		call system("rename ".cmdarg)

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

