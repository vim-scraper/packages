" Vim plugin script grep utility
" Language:    vim script
" Maintainer:  Dave Silvia <dsilvia@mchsi.com>
" Date:        7/19/2004
"
"
" This script is a grep utility without an outside grep program.  It uses
" vim's native search capabilities on a single file.  It has 2 required
" arguments, the pattern and the file name, and one optional argument for
" matching case.  If the file argument does not include a full path
" specification, it is searched for in the current buffer's directory.

function! MiniVimGrep(pat,file,...)
	" if a:1, match case
	let IC="ignorecase"
	if a:0  && a:1
		let IC="noignorecase"
	endif
	let theFile=a:file
	let fullname=theFile
	if match(fullname,'^\(\h:\|\\|/\)') == -1
		let fullname=expand("%:p:h")."/".fullname
	endif
	let fullname=glob(fullname)
	if fullname == ''
		return "Could Not Find ".theFile."<".fullname.">"
	endif
	let Ret=''
	let Ret=Ret.fullname.":".s:vimGrep(a:pat,fullname,IC)
	return Ret
endfunction

function! s:vimGrep(pat,file,ic)
	let IC=a:ic
	" Note: the line '\"redir @z | redir END | ".' serves to initialize the 'z'
	"       register.  All subsequent references are to 'Z', for append.
	" Note: first line has to be checked independently as the search pattern may
	"       match line 1, col 1 and normal matches start with the first
	"       character _after_ the cursor.
	let cmdStr="let saveIC=&ignorecase | set ".IC." | let saveWS=&wrapscan | set nowrapscan | ".
				\"redir @z | redir END | ".
				\"let lastLine=0 | ".
				\"let thePat='".a:pat."' | ".
				\"let cline=getline(1) | ".
				\"if match(cline,thePat) != -1 | redir @Z | silent! echo '1:'.cline | redir END | endif | ".
				\"silent! /".a:pat."\\|\\%$/ | ".
				\"redir @Z | ".
				\"let lNum=line('.') | ".
				\"let cline=getline('.') | ".
				\"if match(cline,thePat) != -1 | silent! echo lNum.':'.cline | endif | ".
				\"while match(cline,thePat) != -1 && lastLine != line('$') | ".
				\"redir END | ".
				\"silent! /".a:pat."\\|\\%$/ | ".
				\"redir @Z | ".
				\"let lNum=line('.') | ".
				\"let cline=getline('.') | ".
				\"let lastLine=line('.') | ".
				\"if match(cline,thePat) != -1 | silent! echo lNum.':'.cline | endif | ".
				\"endwhile | ".
				\"redir END | ".
				\"let &wrapscan=saveWS | ".
				\"let &ignorecase=saveIC | ".
				\"silent! bdelete"
	let editCmd='silent! view '.a:file.' | '.cmdStr
	execute editCmd
	let result=@z
	if result == '' || match(result,'^\s*$') != -1
		return a:pat.": Not Found"
	endif
	return result
endfunction

"TODO   Provide for multiple files.
