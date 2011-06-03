"
" Name: HeaderCVS, VIM plugin for Linux
" Author: AyuanX ( ayuanx#163!com )
" Version: 1.0
"
" Description:
"
" This plugin adds CVS Header before current cursor.
"
" The CVS Header will automatically reflect current file name, current date and
" your description beside a standard CVS Header.
"
" (Note: This plugin is only tested on Linux.)
"
" HowToUse:
"
" 1. Please edit HeaderCVS.vim, customizing the following variables.
"	let s:company = "MyCompany"
"	let s:creator = "AyuanX ( ayuanx#163!com )"
"
" 2. Put HeaderCVS.vim into path "~/.vim/plugin/".
"
" 3. Input ":HeaderCVS YOUR DESCRIPTION HERE" to add CVS Header when editting files.
"    (NOTE: You can insert spaces into your description or omit description.)
"
"
com! -nargs=? HeaderCVS call s:HeaderCVS(<f-args>)

function! s:HeaderCVS(...)
" +++++++++++++++++++++++ Customization starts here +++++++++++++++++++++++
	let s:company = "MyCompany"
	let s:creator = "AyuanX ( ayuanx#163!com )"
" ++++++++++++++++++++++++ Customization ends here ++++++++++++++++++++++++
	let s:date = system('echo -n `date +%F`')
	echo s:date
	if a:0 == 0
		let s:desc = ''
	else
		let s:desc = a:1
	endif
	let s:header = "########################################################################\n#  Copyright ".s:company." Inc.  All Rights reserved\n########################################################################\n#\n#  File           :  ".@%."\n#  Creator        :  ".s:creator."\n#  Creation Date  :  ".s:date."\n#  Description    :  ".s:desc."\n#\n########################################################################\n#    \$Author\$\n#    \$Source\$\n#    \$Revision\$\n#    \$Date\$\n#    \$Id\$\n########################################################################\n#    \$Log\$\n########################################################################\n"
	execute "set paste"
	execute "normal O".s:header."\<Esc>"
	execute "set paste&"
endfunction

