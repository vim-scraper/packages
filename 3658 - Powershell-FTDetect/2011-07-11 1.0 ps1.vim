" Vim ftdetect plugin file
" Language:	Windows PowerShell
" Maintainer:	Ian Koenig <iguyking@gmail.com>
" Version: 1.0
"
" $LastChangedDate $
" $Rev $

au BufRead,BufNewFile *.ps1 	set filetype=ps1
au BufRead,BufNewFile *.psm1 	set filetype=ps1
