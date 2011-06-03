"	**********************************************************
"		File: PhpConsole.vim
"	**********************************************************
"		Purpose: Use php console application for debug messages.
"		Installation: Put it in the ftplugin directory. Go into the environment
"		variables of windows and set a path to the php.exe.
"		Usage: Go into your php-file and :call ParsePhpFile().
"		Now you can debug your file without leaving vim. You can jump to the error
"		lines by clicking on the compiler output in the window below. After
"		finishing removing the errors you can open the file in the browser.
"		Author: Klaus Horsten <horsten at gmx.at>
"		Copyright: No claim. Without any warranty.
"		Credits: Mikolaj Machowski wrote the errorformat.
"	**********************************************************

function! ParsePhpFile()
	set makeprg=php.exe\ -f
	set shellpipe=> 				
	set errorformat=<b>%*[^<]</b>:\ \ %m\ in\ <b>%f</b>\ on\ line\ <b>%l</b><br\ />
	make %
	copen
endfunction
