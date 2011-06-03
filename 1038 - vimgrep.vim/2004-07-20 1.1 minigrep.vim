" Vim plugin script grep utility
" Language:    vim script
" Maintainer:  Dave Silvia <dsilvia@mchsi.com>
" Date:        7/21/2004
"
" Version 1.1
"   For grep-ing in open buffers:
"    -  Added BufMiniGrep for grep-ing all open buffers
"    -  Added code to set cursor to line 1, col 1 in the file in case it is
"       being done with open buffers.  Then the cursor is set back to its
"       original position.
"    -  Added code to allow passing '%' or '' for file, meaning current buffer
"    -  Added augroup and autocmd to save the buffer number so it can be used
"       to return to the original buffer if grep-ing in open buffers
"
"   For multiple files
"    -  Added code to check for comma and newline file name separators.
"       Comma allows the user to specify 'file' argument as
"       "file1,file2,...", while newline allows 'file' argument to be
"       something like glob(expand("%:p:h")."/*") or
"       glob("<some-path>/*")
"    -  Added code to check if file is the same as the original open
"       buffer, if so, don't do bdelete.
"    -  Added second optional argument for returning file names only.
"       If non-zero, just return file names, not the matching lines.
"
"   Misc
"    -  Added code to test for empty files
"    -  Added code to test for directories
"
" Version 1.0
"
" Original Release
"
" This script is a grep utility without an outside grep program.  It uses
" vim's native search capabilities on a single file.  It has 2 required
" arguments, the pattern and the file name, and one optional argument for
" matching case.  If the file argument does not include a full path
" specification, it is searched for in the current buffer's directory.

" set the buffer number so it can be readily gotten if needed
augroup miniVimGrep
	autocmd!
	autocmd BufEnter,BufNew,BufAdd,BufRead * let b:bufNo=expand("<abuf>")
augroup END

function! BufMiniGrep(pat,...)
	let s:OrigBuf=b:bufNo
	let OrigLin=line('.')
	let OrigCol=col('.')
	let Ret=''
	if a:0 == 1
		bufdo let Ret=Ret.MiniVimGrep(a:pat,'',a:1)
	elseif a:0 > 1
		bufdo let Ret=Ret.MiniVimGrep(a:pat,'',a:1,a:2)
	else
		bufdo let Ret=Ret.MiniVimGrep(a:pat,'')
	endif
	execute 'b'.s:OrigBuf
	call cursor(OrigLin,OrigCol)
	return Ret
endfunction

function! MiniVimGrep(pat,file,...)
	" if a:1, match case, if a:2, return filenames only
	let startminiBufNo=b:bufNo
	let startminiLinNo=line(".")
	let startminiColNo=col(".")
	let IC="ignorecase"
	let FNonly=0
	if a:0  && a:1
		let IC="noignorecase"
	endif
	if a:0 > 1 && a:2
		let FNonly=1
	endif
	if a:file == '%' || a:file == ''
		let Ret=''
		let theResult=s:vimGrep(a:pat,a:file,IC,FNonly)
		if (FNonly && theResult == "\<NL>") || !FNonly
			let Ret=expand("%:p").": ".theResult
		endif
		execute "b".startminiBufNo
		call cursor(startminiLinNo,startminiColNo)
		return Ret
	else
		let endOfLine="\<NL>"
		let Comma=','
		if match(a:file,endOfLine.'\|'.Comma) != -1
			let thePath=expand("%:p:h")
			let Files=a:file
			let matchPos=match(a:file,endOfLine.'\|'.Comma)
			let Ret=''
			while matchPos != -1
				let theFile=strpart(Files,0,matchPos)
				let Files=strpart(Files,matchPos+1)
				let matchPos=match(Files,endOfLine.'\|'.Comma)
				let fullname=theFile
				if match(fullname,'^\(\h:\|\\|/\)') == -1
					let fullname=thePath."/".fullname
				endif
				let fullname=glob(fullname)
				if fullname == ''
					if !FNonly
						let Ret=Ret."Could Not Find ".theFile." in ".thePath."\<NL>"
					endif
					continue
				endif
				if isdirectory(fullname)
					if !FNonly
						let Ret=Ret.fullname.": is a directory\<NL>"
					endif
					continue
				endif
				if fullname == expand ("%:p")
					let theResult=s:vimGrep(a:pat,'',IC,FNonly)
				else
					let theResult=s:vimGrep(a:pat,fullname,IC,FNonly)
				endif
				if (FNonly && theResult == "\<NL>") || !FNonly
					let Ret=Ret.fullname.": ".theResult
				endif
			endwhile
			let theFile=Files
			let fullname=Files
			if match(fullname,'^\(\h:\|\\|/\)') == -1
				let fullname=expand("%:p:h")."/".fullname
			endif
			let fullname=glob(fullname)
			if fullname == ''
					if !FNonly
						let Ret=Ret."Could Not Find ".theFile." in ".thePath."\<NL>"
					endif
			else
				if fullname == expand ("%:p")
					let theResult=s:vimGrep(a:pat,'',IC,FNonly)
				elseif !isdirectory(fullname)
					let theResult=s:vimGrep(a:pat,fullname,IC,FNonly)
				elseif isdirectory(fullname) && !FNonly
					let theResult="is a directory\<NL>"
				else
					let theResult="\<NL>"
				endif
				if (FNonly && theResult == "\<NL>") || !FNonly
					let Ret=Ret.fullname.": ".theResult
				endif
			endif
		else
			" don't splat over command line
			echo " "
			let Ret=""
			let theFile=a:file
			let fullname=theFile
			if match(fullname,'^\(\h:\|\\|/\)') == -1
				let fullname=expand("%:p:h")."/".fullname
			endif
			let fullname=glob(fullname)
			if fullname == ''
					if !FNonly
						execute "b".startminiBufNo
						call cursor(startminiLinNo,startminiColNo)
						return "Could Not Find ".theFile." in ".thePath."\<NL>"
					endif
			endif
			if fullname == expand ("%:p")
				let theResult=s:vimGrep(a:pat,'',IC,FNonly)
			elseif !isdirectory(fullname)
				let theResult=s:vimGrep(a:pat,fullname,IC,FNonly)
			elseif isdirectory(fullname) && !FNonly
				let theResult="is a directory\<NL>"
			else
				let theResult=""
			endif
			if (FNonly && theResult == "\<NL>") || !FNonly
				let Ret=fullname.": ".theResult
			endif
		endif
		execute "b".startminiBufNo
		call cursor(startminiLinNo,startminiColNo)
		return Ret
	endif
endfunction

function! s:vimGrep(pat,file,ic,fnonly)
	let IC=a:ic
	let FNonly=a:fnonly
	let holdz=@z
	" Note: the line '\"redir @z | redir END | ".' serves to initialize the 'z'
	"       register.  All subsequent references are to 'Z', for append.
	" Note: first line has to be checked independently as the search pattern may
	"       match line 1, col 1 and normal matches start with the first
	"       character _after_ the cursor.
	let miniGrepCmd="let saveIC=&ignorecase | set ".IC." | let saveWS=&wrapscan | set nowrapscan | ".
				\"let origlin=line('.') | ".
				\"let origcol=col('.') | ".
				\"call cursor(1,1) | ".
				\"redir @z | redir END | ".
				\"let lastLine=0 | ".
				\"let thePat='".a:pat."' | ".
				\"let cline=getline(1) | ".
				\"if match(cline,thePat) != -1 | redir @Z | silent! echo '1:'.cline | redir END | endif | ".
				\"silent! /".a:pat."\\|\\%$/ | ".
				\"let lNum=line('.') | ".
				\"let cline=getline('.') | ".
				\"if match(cline,thePat) != -1 | redir @Z | silent! echo lNum.':'.cline | redir END | endif | ".
				\"while match(cline,thePat) != -1 && lastLine != line('$') | ".
				\"silent! /".a:pat."\\|\\%$/ | ".
				\"let lNum=line('.') | ".
				\"let cline=getline('.') | ".
				\"let lastLine=line('.') | ".
				\"if match(cline,thePat) != -1 | redir @Z | silent! echo lNum.':'.cline | redir END | endif | ".
				\"endwhile | ".
				\"call cursor(origlin,origcol) | ".
				\"let &wrapscan=saveWS | ".
				\"let &ignorecase=saveIC"
	if a:file != '%' && a:file != ''
		let isEmptyCmd='silent! view '.a:file.' | let zBytes=line2byte(line("$")) | let lBytes=strlen(getline(1)) | silent bdelete'
		execute isEmptyCmd
		let emptyFile=zBytes == -1 || (zBytes == 1 && lBytes == 0)
		if emptyFile
			let @z=holdz
			return "Empty File\<NL>"
		endif
		let miniGrepCmd='silent! view '.a:file.' | '.miniGrepCmd." | silent! bdelete"
	else
		if line2byte(line("$")) == -1
			let @z=holdz
			return "Empty File\<NL>"
		endif
	endif
	execute miniGrepCmd
	let result=@z
	let emptyResult=result == '' || match(result,'^\s*$') != -1
	if emptyResult
		let @z=holdz
		return a:pat.": Not Found\<NL>"
	endif
	if match(result,"\<NL>$") == -1
		let result=result."\<NL>"
	endif
	let @z=holdz
	if FNonly
		return "\<NL>"
	endif
	return result
endfunction
