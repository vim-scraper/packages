"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Purpose: Create a menu of Matrix mql commands for the VIM editor
" 	Matrix is a Product Data Management system from MatrixOne http://www.matrixone.com 
" 	It has an SQL-like command language	called mql
"   When you run the menu items, it runs/imports/etc with mql using the current file as input
"
" $Header:   D:\pvcs_projects\archives\stuff\mqlmenu.vim-arc   1.0   Jul 04 2002 11:16:20   rpilkey  $
"
" Maintainer: Roger Pilkey 	<rpilkey@magma.ca>
"
" License:     Released into the public domain.
"
" Installation:
" 	Just drop this file in your plugin folder/directory.
"
" vim ts=4 : sw=4 : tw=0
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Exit when already loaded (or "compatible" mode set) 
if exists("loaded_MQLMenu") || &cp
  finish 
endif 
let loaded_MQLMenu = 1

" description of mql options:
"-t = don't print title window
"-v = turn on verbose
"-k = continue
"-c = mql command follows
"-stderr = errors go here
let s:cmd = "c:\\ematrix\\bin\\winnt\\mql -stderr:errors.err -t -v -k "

function! s:AdminImport(bootfile,password)
	let l:cmd = s:cmd.a:bootfile." -c \"set context user creator ".a:password." ;import admin * overwrite from file \'" 
	let l:cmd = l:cmd.bufname("%") . "\';\""
	let l:val = system(l:cmd)
	if ( getfsize("errors.err") > 0 )
		new 
		edit errors.err 
	endif
endfunc

function! s:AdminExport(bootfile,password)
	let l:cmd = s:cmd.a:bootfile." -c \"set context user creator ".a:password." ;export admin " 
	let l:progname = bufname("%") 
	let l:progname = substitute(progname,".exp$","","")
	let l:cmd = l:cmd . l:progname . " into file \'"
	let l:cmd = l:cmd . bufname("%") . "\';\" "
	let l:val = system(l:cmd)
	if ( getfsize("errors.err") > 0 )
		new 
		edit errors.err 
	endif
endfunc

function! s:BusImport(bootfile,password)
	let l:cmd = s:cmd.a:bootfile." -c \"set context user creator ".a:password." ;trigger off;import bus * * * overwrite commit 1 from file \'" 
	let l:cmd = l:cmd . bufname("%") . "\';\""
	let l:val = system(l:cmd)
	if ( getfsize("errors.err") > 0 )
		new 
		edit errors.err 
	endif
endfunc

function! s:Run(bootfile,password)
	let l:cmd = s:cmd.a:bootfile." -c \"set context user creator ".a:password." ;run \'" 
	let l:cmd = l:cmd . bufname("%") . "\';\""
	let l:val = system(l:cmd)
	if ( getfsize("errors.err") > 0 )
		new 
		edit errors.err 
	endif
endfunc

function! s:CleanUp(bootfile,password)
	let l:cmd = s:cmd.a:bootfile." -c \"set context user creator ".a:password." ;tidy vault \'JDSU Active\'; index vault \'JDSU Active\';\"" 
	let l:val = system(l:cmd)
	if ( getfsize("errors.err") > 0 )
		new 
		edit errors.err 
	endif
endfunc

" make some externally visible mappings
noremap <unique> <silent> <script> <Plug>MqlBusImport  <SID>BusImport
noremap <unique> <silent> <script> <Plug>MqlAdminImport  <SID>AdminImport
noremap <unique> <silent> <script> <Plug>MqlRun  <SID>Run
noremap <unique> <silent> <script> <Plug>MqlCleanUp  <SID>CleanUp

"I tried to put the bootfile and the password in a variable, but it didn't work.  
"If you know how to do that, please let me know.  Rog

"Local
if !hasmapto('<Plug>MqlBusImport')
	noremap <unique> <silent> <script> <Leader>b  m':call <SID>BusImport("-b matrix-r","pass MYPASSWORD")<CR>''
	noremap <unique> <silent> <script> <F11> m':call <SID>BusImport(" matrix-r","pass MYPASSWORD")<CR>''
endif
if !hasmapto('<Plug>MqlAdminImport')
	noremap <unique> <silent> <script> <Leader>a m':call <SID>AdminImport("-b matrix-r","pass MYPASSWORD")<CR>''
	noremap <unique> <silent> <script> <F9> m':call <SID>AdminImport("-b matrix-r","pass MYPASSWORD")<CR>''
endif
if !hasmapto('<Plug>MqlRun')
	noremap <unique> <silent> <script> <Leader>r  m':call <SID>Run("-b matrix-r","pass MYPASSWORD")<CR>''
	noremap <unique> <silent> <script> <F8> m':call <SID>Run("-b matrix-r","pass MYPASSWORD")<CR>''
endif

noremenu 80.100.100 &Plugin.&MQL.Import\ &Bus\ objs<Tab>F11\ \\b m':call <SID>BusImport("-b matrix-r","pass MYPASSWORD")<CR>''
tmenu &Plugin.&MQL.Import\ &Bus\ objs<Tab>F11\ \\b Import the Business objects from the current file into Matrix

noremenu 80.100.200 &Plugin.&MQL.Import\ &Admin\ objs<Tab>F9\ \\a m':call <SID>AdminImport("-b matrix-r","pass MYPASSWORD")<CR>''
tmenu &Plugin.&MQL.Import\ &Admin\ objs<Tab>F9\ \\a Import the Admin objects (programs, policies, etc) from the current file into Matrix

noremenu 80.100.300 &Plugin.&MQL.Export.Export\ Admin\ objs(overwrites\ current\ file) m':call <SID>AdminExport("-b matrix-r","pass MYPASSWORD")<CR>''
tmenu &Plugin.&MQL.Export.Export\ Admin\ objs(overwrites\ current\ file) Export the item from Matrix onto the current file (uses the filename minus .exp to find the item name)

noremenu 80.100.400 &Plugin.&MQL.&Run\ in\ mql<Tab>F8\ \\r m':call <SID>Run("-b matrix-r","pass MYPASSWORD")<CR>''
tmenu &Plugin.&MQL.&Run\ in\ mql<Tab>F8\ \\r Run the current file in mql

noremenu 80.100.500 &Plugin.&MQL.Clean\ up\ &Vault m':call <SID>CleanUp("-b matrix-r","pass MYPASSWORD")<CR>''
tmenu &Plugin.&MQL.Clean\ up\ &Vault Runs "Tidy vault", then "Index Vault"
noremenu 80.100.550 &Plugin.&MQL.-Sep-	:

"Other instances
noremenu 80.100.700.100 &Plugin.&MQL.Other\ Instance.Import\ Bus\ objs m':call <SID>BusImport("-b otherbootfile-r","pass OTHERPASSWORD")<CR>''
noremenu 80.100.700.200 &Plugin.&MQL.Other\ Instance.Import\ Admin\ objs m':call <SID>AdminImport("-b otherbootfile-r","pass OTHERPASSWORD")<CR>''
noremenu 80.100.700.300 &Plugin.&MQL.Other\ Instance.Run\ in\ mql m':call <SID>Run("-b otherbootfile-r","pass OTHERPASSWORD")<CR>''


"edit this file, to customize it.
execute 'noremenu 80.100.900 &Plugin.&MQL.&Customize :edit ' . expand ('<sfile>:p') . '<CR>'
execute 'tmenu &Plugin.&MQL.&Customize Edit the code behind MQL Menu'

