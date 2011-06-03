" Source safe commands
" Author: Michael Geddes <michaelrgeddes@optushome.com.au>
" Version: 0.1
" 
" This code is taken from htmlcmd.vim's url-yank.  It allows you to yank the
" currrent filename 

if has('win32')
  fun! GetUNCPathOf( filename )
	let shares=system('net share')
	let shares=substitute(shares,"\n[^ \n]\\+[$]\\s[^\n]\\+","",'g')
	let newname=''
	let current=fnamemodify(a:filename,':gs+/+\\+')
	let do_it=1
	let ss=((&shellslash)?('/'):('\'))
	while do_it
	  let tx=fnamemodify(current, ':h')
	  if tx==current
		let do_it=0
	  else
		let current=tx
		let mx= '\c'."\n".'\([^ 	'."\n".']\+\)\s\+'.escape(current,'$\.&').'\s\+'
		let match=matchstr(shares,mx)
		if match != ""
		  let sharename=substitute(match,mx,'\1','')
		  let drivepart=strpart(a:filename, strlen(current))
		  if drivepart!~ '^[\\/]'
			let drivepart=ss.drivepart
		  endif
		  return ss.ss.$COMPUTERNAME.ss.sharename.drivepart
		endif
	  endif
	endwhile
	return a:filename
  endfun
else
  fun! GetUNCPathOf( filename )
	return filename
  endfun
endif

nmap <localleader>yf :let @*=GetUNCPathOf(expand('%:p'))<cr>

