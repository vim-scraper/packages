" Vim utility script to interactively set highlight commands
" Language:    vim script
" Maintainer:  Dave Silvia <dsilvia@mchsi.com>
" Date:        8/13/2004
"
" Version 1.0
"
"Install in vimfiles/plugin
":IactHl on the command line to access
"
"Utility to interactively create highlight commands file (~/Hilite.vim)
"which can be copied to a runtimepath 'after' directory as
"'syntax/syncolor.vim' for automatic execution at Vim startup.
"Previous settings are save in a file named '~/yymmddhhmmssHilite.vim'
"Use: ?? for online key mapping reference.
"
"The utility opens 3 windows, the top window displays the major highlight
"groups with their names highlighted according to the current settings.  The
"middle window lists the highlight commands currently in effect to produce the
"output shown in the top window.  The bottom window lists the RGB names
"available for the current Vim release.
"
"Place the cursor on the line you wish to modify in the middle window.  Place
"the cursor on the RGB value you want in the bottom window.  Use the mapped
"key sequences to set the line.  The key sequences are:
"
"fg        Set the foreground to the selected RGB value
"bg        Set the background to the selected RGB value
"no        Set the mode attributes to 'NONE'
"bo        Add 'bold' to the mode attributes
"it        Add 'italic' to the mode attributes
"re        Add 'reverse' to the mode attributes
"st        Add 'standout' to the mode attributes
"un        Add 'underline' to the mode attributes

let s:curHiliteFile=fnamemodify(expand("~/".strftime("%y%m%d%H%M%S")."Hilite.vim"),":p")
let s:synFile=fnamemodify(expand("~/synKeyword.vim"),":p")
let s:hiliteFile=fnamemodify(expand("~/Hilite.vim"),":p")
let s:selectguiRGBFile=fnamemodify(expand("~/selectguiRGB.txt"),":p")
let s:selectctermRGBFile=fnamemodify(expand("~/selectctermRGB.txt"),":p")
let s:origHLcmd=''
let s:newHLcmd=''

function! s:SetHLline(ifType,mode)
	let hlKey=a:ifType.a:mode
	let RGBFile='s:select'.a:ifType.'RGBFile'
	let hiliteNameBufNr=bufnr(s:hiliteFile)
	let RGBvalueBufNr=bufnr({RGBFile})
	let synBufNr=bufnr(s:synFile)
	let hiliteNameWinNr=bufwinnr(hiliteNameBufNr)
	let RGBvalueWinNr=bufwinnr(RGBvalueBufNr)
	let synWinNr=bufwinnr(synBufNr)
	execute hiliteNameWinNr.'wincmd w'
	let clnum=line('.')
	let cline=getline(clnum)
	let hiliteName=strpart(cline,matchend(cline,'highlight '))
	let hiliteName=strpart(hiliteName,0,match(hiliteName,'\s'))
	execute RGBvalueWinNr.'wincmd w'
	let RGBvalue=expand("<cword>")
	let ans=input("You want to set ".hlKey." for ".hiliteName." to ".RGBvalue." [y/n] ","y")
	if ans ==? "y"
		execute hiliteNameWinNr.'wincmd w'
		let hlKeyPos=match(cline,hlKey.'=')
		if hlKeyPos != -1
			let hlKeyEnd=matchend(cline,hlKey.'=\p*\s*')
			let bline=strpart(cline,0,hlKeyPos)
			let eline=strpart(cline,hlKeyEnd)
			let ckey=strpart(cline,hlKeyPos,hlKeyEnd-hlKeyPos)
			let spacePos=match(ckey,'\s')
			if spacePos != -1
				let eline=strpart(ckey,spacePos).eline
			endif
			let ckey=hlKey.'='.RGBvalue
			let nline=bline.ckey.eline
		else
			let nline=cline.' '.hlKey.'='.RGBvalue
		endif
		let s:origHLcmd=cline
		let s:newHLcmd=nline
		execute ':'.nline
		execute bufwinnr(s:synFile).'wincmd w'
		silent execute '/'.hiliteName
		execute "normal z\<CR>"
		execute "normal \<C-U>"
		silent normal n
		nohls
		execute hiliteNameWinNr.'wincmd w'
		call setline(line('.'),nline)
	endif
endfunction

function! s:SetATTRline(ifType,attr)
	let hlKey=a:ifType
	let hiliteNameBufNr=bufnr(s:hiliteFile)
	let synBufNr=bufnr(s:synFile)
	let hiliteNameWinNr=bufwinnr(hiliteNameBufNr)
	let synWinNr=bufwinnr(synBufNr)
	execute hiliteNameWinNr.'wincmd w'
	let clnum=line('.')
	let cline=getline(clnum)
	let hiliteName=strpart(cline,matchend(cline,'highlight '))
	let hiliteName=strpart(hiliteName,0,match(hiliteName,'\s'))
	let ans=input("You want to set ".hlKey." for ".hiliteName." to ".a:attr." [y/n] ","y")
	if ans ==? "y"
		execute hiliteNameWinNr'wincmd w'
		let hlKeyPos=match(cline,hlKey.'=')
		if hlKeyPos != -1
			let hlKeyEnd=matchend(cline,hlKey.'=\p*\s*')
			let bline=strpart(cline,0,hlKeyPos)
			let eline=strpart(cline,hlKeyEnd)
			let ckey=strpart(cline,hlKeyPos,hlKeyEnd-hlKeyPos)
			let spacePos=match(ckey,'\s')
			if spacePos != -1
				let eline=strpart(ckey,spacePos).eline
				let ckey=strpart(ckey,0,spacePos)
			endif
			if a:attr ==# 'NONE'
				let ckey=hlKey.'=NONE'
			else
				if match(ckey,'NONE') != -1
					let ckey=hlKey."=".a:attr
				elseif match(ckey,a:attr) == -1
					let ckey=ckey.','.a:attr
				endif
			endif
			let nline=bline.ckey.eline
		else
			let nline=cline.' '.hlKey.'='.a:attr
		endif
		let s:origHLcmd=cline
		let s:newHLcmd=nline
		let s:isSet=1
		execute ':'.nline
		execute synWinNr.'wincmd w'
		silent execute '/'.hiliteName
		execute "normal z\<CR>"
		execute "normal \<C-U>"
		silent normal n
		nohls
		execute hiliteNameWinNr.'wincmd w'
		call setline(line('.'),nline)
	endif
endfunction

function! s:fileRGBnames(iftype,RGBnames,maxLen)
	let maxWidth=winwidth('')-1
	let RGBnamesPerRow=maxWidth/a:maxLen
	let RGBnameColWidth=maxWidth/RGBnamesPerRow
	let RGBFile=s:select{a:iftype}RGBFile
	let editRGB='silent! edit '.RGBFile
	if glob(RGBFile) != '' && delete(RGBFile)
		echomsg "Could not delete ".RGBFile
	endif
	execute editRGB
	setlocal noswapfile
	let thisRGB=StrListTok(a:RGBnames,'g:RGBnames')
	while thisRGB != ''
		let entryNr=0
		let entryLine=''
		while entryNr < RGBnamesPerRow && thisRGB != ''
			let entryLen=strlen(thisRGB)
			while entryLen < RGBnameColWidth
				let thisRGB=thisRGB.' '
				let entryLen=entryLen+1
			endwhile
			let entryLine=entryLine.thisRGB
			let entryNr=entryNr+1
			let thisRGB=StrListTok('','g:RGBnames')
		endwhile
		call append(line('$'),entryLine)
	endwhile
	unlet g:RGBnames
	normal gg
	normal dd
	silent! :w
	setlocal nomodifiable
endfunction

function! s:IactHLhlp()
	echomsg "Builds a ~/Hilite.vim file that can then be copied to an 'after'"
	echomsg "runtimepath directory as 'syntax/syncolor.vim' for automatic"
	echomsg "execution at Vim startup.  Previous settings are saved in the file"
	echomsg "~/yymmddhhmmssHilite.vim."
	echomsg "Legend:"
	echomsg "     key     'gui' or 'cterm'"
	echomsg "     attrs   NONE, bold, italic, reverse, standout, or underline"
	echomsg "Pressing these key sequences does the following:"
	echomsg "fg   Sets the foreground value in the cursor line in the middle window"
	echomsg "     to the RGB value in the bottom window cursor word"
	echomsg "bg   Sets the background value in the cursor line in the middle window"
	echomsg "     to the RGB value in the bottom window cursor word"
	echomsg "no   Sets key=NONE"
	echomsg "bo   Adds 'bold' to key=<attrs>"
	echomsg "it   Adds 'italic' to key=<attrs>"
	echomsg "re   Adds 'reverse' to key=<attrs>"
	echomsg "st   Adds 'standout' to key=<attrs>"
	echomsg "un   Adds 'underline' to key=<attrs>"
	echomsg "Resulting effect is shown in the top window"
endfunction

command! IactHl call s:suhilite()
function! s:suhilite()
	map <script> ?? :call <SID>IactHLhlp()<CR>
	let homeDir=expand("~")
	let rgbFile=$VIMRUNTIME.'/rgb.txt'
	let newRGBFile=homeDir.'/RGB.txt'
	let editNewRGB='silent! edit '.newRGBFile
	let readRGBFile='silent! :r '.rgbFile
	call delete(newRGBFile)
	execute editNewRGB
	setlocal noswapfile
	execute readRGBFile
	" go to top of buffer and get rid of empty line
	normal gg
	normal dd
	" get rid of non-RGB definition lines
	silent :%s/^\s*\D\+\h\_p\+\_^//g
	" get rid of space delimited definition names, these are duplicates
	silent :%s/^\s*\d\+\s\+\d\+\s\+\d\+\s\+\<\h\+\s\+\_p\+\_^//g
	" get rid of 'grey' definitions, these are duplicates of 'gray'
	silent :%s/^\p*\s*\h*\(G\|g\)rey\_p*\_^//g
	call SortR(1,line('$'))
	" get rid of duplicate RGB number specifications
	let clnum=1
	let elnum=line('$')
	normal gg
	while clnum < elnum
		let cline=getline(clnum)
		let cRGBnr=strpart(cline,0,match(cline,'\h'))
		let nline=getline(clnum+1)
		let nRGBnr=strpart(nline,0,match(nline,'\h'))
		normal j
		if cRGBnr == nRGBnr
			normal dd
			let elnum=elnum-1
		endif
		let RGBname=strpart(cline,match(cline,'\h'))
		call setline(clnum,RGBname)
		let clnum=clnum+1
	endwhile
	call SortR(1,line('$'))
	silent! :w
	normal gg
	let clnum=1
	let elnum=line('$')
	let guiRGBnames=''
	let ctermRGBnames=''
	let guiRGBnameMaxLen=0
	let ctermRGBnameMaxLen=0
	while clnum <= elnum
		let cline=getline(clnum)
		let RGBname=strpart(cline,match(cline,'\h'))
		let RGBnameLen=strlen(RGBname)
		let guiRGBnames=guiRGBnames.RGBname."\<NL>"
		if RGBnameLen > guiRGBnameMaxLen
			let guiRGBnameMaxLen=RGBnameLen
		endif
		try
			let hiliteTry='highlight Ignore ctermfg='.RGBname
			execute hiliteTry
			let ctermRGBnames=ctermRGBnames.RGBname."\<NL>"
			if RGBnameLen > ctermRGBnameMaxLen
				let ctermRGBnameMaxLen=RGBnameLen
			endif
		catch
			" don't do anything
		endtry
		let clnum=clnum+1
	endwhile
	bwipeout

	let maxWidth=winwidth('')-1
	set nonumber

	if has("gui_running")
		call s:fileRGBnames('gui',guiRGBnames,guiRGBnameMaxLen+2)
		new
		set nonumber
		set ignorecase
		call GetHilite()
		map <script> fg :call <SID>SetHLline('gui','fg')<CR>
		map <script> bg :call <SID>SetHLline('gui','bg')<CR>
		map <script> no :call <SID>SetATTRline('gui','NONE')<CR>
		map <script> bo :call <SID>SetATTRline('gui','bold')<CR>
		map <script> it :call <SID>SetATTRline('gui','italic')<CR>
		map <script> re :call <SID>SetATTRline('gui','reverse')<CR>
		map <script> st :call <SID>SetATTRline('gui','standout')<CR>
		map <script> un :call <SID>SetATTRline('gui','underline')<CR>
	else
		call s:fileRGBnames('cterm',ctermRGBnames,ctermRGBnameMaxLen+2)
		new
		set nonumber
		set ignorecase
		call GetHilite()
		map <script> fg :call <SID>SetHLline('cterm','fg')<CR>
		map <script> bg :call <SID>SetHLline('cterm','bg')<CR>
		map <script> no :call <SID>SetATTRline('cterm','NONE')<CR>
		map <script> bo :call <SID>SetATTRline('cterm','bold')<CR>
		map <script> it :call <SID>SetATTRline('cterm','italic')<CR>
		map <script> re :call <SID>SetATTRline('cterm','reverse')<CR>
		map <script> st :call <SID>SetATTRline('cterm','standout')<CR>
		map <script> un :call <SID>SetATTRline('cterm','underline')<CR>
	endif
	execute bufwinnr(s:hiliteFile).'wincmd w'
endfunction

function! GetHilite()
	let saveWS=&wrapscan
	let savez=@z
	let savep=@/
	let curHiliteFile=s:curHiliteFile
	let synKeywordFile=s:synFile
	let newHiliteFile=s:hiliteFile
	set wrapscan
	if glob(curHiliteFile) != '' && delete(curHiliteFile)
		echomsg "Could not delete: ".curHiliteFile
	endif
	if glob(synKeywordFile) != '' && delete(synKeywordFile)
		echomsg "Could not delete: ".synKeywordFile
	endif
	if glob(newHiliteFile) != '' && delete(newHiliteFile)
		echomsg "Could not delete: ".newHiliteFile
	endif
	redir @z
	silent highlight
	redir END
	execute 'silent edit '.curHiliteFile
	setlocal noswapfile
	silent normal "zP
	normal gg
	normal dd
	silent global /links to/ normal dd
	silent g/xxx /s///e
	call SortR(1,line('$'))
	silent global /^\h/ call setline(line('.'),"highlight ".getline("."))
	silent! :w
	execute 'silent! saveas '.newHiliteFile
	setlocal swapfile
	normal gg
	new
	execute 'silent! edit '.synKeywordFile
	setlocal noswapfile
	silent normal "zP
	normal gg
	normal dd
	silent global /links to/ normal dd
	silent g/xxx /s///e
	silent global! /links to/ substitute /\s.*$//e
	silent % substitute /^[^ ]*/syntax keyword &\t&/
	silent! :w
	normal gg
	source %
	silent % substitute /^syntax keyword\s\+\h*\s\+//
	call SortR(1,line('$'))
	silent! :w
	setlocal nomodifiable
	normal gg
	let &wrapscan=saveWS
	let @z=savez
	let @/=savep
	return
endfunction
