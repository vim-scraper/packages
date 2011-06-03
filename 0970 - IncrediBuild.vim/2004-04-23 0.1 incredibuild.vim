"
" Xoreax IncrediBuild plugin
"
" Author: Andy Berdan <andy at berdan dot ca>
" Last Change: 23-Apr-2004 @ 11:21
" Requires: Vim-6.0 (preferably 6.2), Xoreax Incredibuild (available in path)
" Optional: Unix tools for Win32 (for tee) 
" Version: 0.1
" Licence: This program is free software; you can redistribute it and/or
"          modify it under the terms of the GNU General Public License.
"          See http://www.gnu.org/copyleft/gpl.txt 
" Download From:
"	http://www.vim.org/scripts/script.php?script_id=970
" Description:
"	Plugin to allow building with Xoreax Incredibuild using Vim's quickfix
"	system. 
"	 
" TODO:
"	Make sln/project/config detection more flexible (build map with
"	  aribitrary target names?)
"	Implement recursive arg handling 
"	MSVC 6.0 (not tested at all -- might work)
"

" ------------------------------------------------------------ 
" USER SETTINGS:
" ------------------------------------------------------------ 
let s:IBSLNFile="c:\\dev\\SomeMSVC.NET.sln"
let s:IBConfig="Release Win32"
let s:IBProject="Main"

" if you want the Xorex graphical monitor to pop up:
let s:IBOpenGraphicalMonitor=1

" if unix tools are installed into the path:
let s:IBUseTee=0



" ------------------------------------------------------------ 
" IMPLEMENTATION: 
" ------------------------------------------------------------ 
let s:IBmakeprg="BuildConsole.exe\ $*"
let s:IBerrorformat="%f(%l)\ :\ %t%*\\D%n:\ %m,%[\ ]%\\+%f(%l)\ :\ %m"
if s:IBUseTee 
  let s:IBshellpipe="2>\&1\ \|\ tee\ %s"
else
  let s:IBshellpipe=">%s\ 2>\&1"
endif

command! -nargs=* IBuild call IBuild(s:IBSLNFile, s:IBProject, s:IBConfig, <f-args> ) 
function! IBuild(solution, project, targetConfig, ...)
  " save old values
  let storedmakeprg=&makeprg
  let storederrorformat=&errorformat
  let storedshellpipe=&shellpipe

  " set new values
  let &makeprg=s:IBmakeprg
  let &errorformat=s:IBerrorformat
  let &shellpipe=s:IBshellpipe

  " yuuuuck -- need to fix this
  let cmd = ""
  if a:0 > 0
    let cmd = cmd . a:1
  endif
  if a:0 > 1
    let cmd = cmd . a:2
  endif
  if a:0 > 2
    let cmd = cmd . a:3
  endif
  if a:0 > 3
    let cmd = cmd . a:4
  endif
  if a:0 > 4
    let cmd = cmd . a:5
  endif
  if a:0 > 5
    let cmd = cmd . a:6
  endif

  if s:IBOpenGraphicalMonitor
    exec "!start BuildMonitor.exe"
  endif

  exec "make! ".a:solution." /cfg=\"".a:targetConfig. "\" /prj=\"".a:project."\" /NoLogo " . cmd

  " restore
  let &makeprg=storedmakeprg
  let &errorformat=storederrorformat
  let &shellpipe=storedshellpipe
endfunction
" ------------------------------------------------------------
