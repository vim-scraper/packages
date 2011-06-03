" portablemsys.vim plugin version 1.0.1
" By ronh, placed in the public domain, feel free to do whatever
" Versions: 1.0.1 July 29, 2008
"				- Fixed :shell not showing up
"			1.0.0 July 25, 2008
"				- Initial release
" Features: Enables integrating an msys shell with Portable GVim,
" This is achieved by setting the 'shell' options automatically
" to the current drive letter Portable GVim is located at, or in
" other words, you will be able to use Portable GVim and msys on
" your usb stick drive.

" Usage: Place this file inside the plugin folder of GVim Portable
" and preferably, edit your vimrc to include the following command
" let Msys='PortableApps\msys'
" which tells this script where you placed the msys shell relative
" to Portable GVim's drive letter.
" If you don't, I will stupidly assume your msys is placed in the
" Tools folder.

" ========================================================================

" Exit quickly when:
" - this plugin was already loaded
" - when 'compatible' is set
if exists("loaded_portablemsys_plugin") || &cp
	finish
endif
let loaded_portablemsys_plugin = 1

" Find out where GVim is located
let GVimDir=getcwd()
" Find out where Msys is located
" If 'Msys' was declared somewhere (such as vimrc), use that location
if exists("Msys")
	let MsysLocation=Msys
" or else, stupidly use X:\Tools\msys where X is GVim's drive letter
else
	let MsysLocation='Tools\msys'
endif
" Make Msys's location relative to GVim by concatenating Msys's location
" to GVim's drive letter
let MsysDir=GVimDir[0] . ":\\" . MsysLocation
let TempDir=MsysDir
" Add back-slash before spaces in the path (needed to mimic unix paths)
let MsysDir=substitute(MsysDir, '\ ', '\\ ', "g")
" Lastly, set the string to execute - "set shell" command
let ExecThis='set shell=' . MsysDir . '\bin\sh.exe\ --login\ -i'
exe ExecThis

" Set the $HOME variable, will cause Msys's home to be located where
" previously set in the variable Msys, inside the home directory.
let TempDir=substitute(MsysDir, '\', '/', "g")
let TempDir=substitute(TempDir, '/ ', '/\ ', "g")
let ExecThis='let $HOME="' . TempDir . '/home/"'
exe ExecThis

" Eventually, commands such as :!make will look like this:
" F:\PortableApps\msys\bin\sh.exe --login -i -c "make" 
" Thus, enabling GVim to work with msys portably on a usb stick

