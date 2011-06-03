" File: translateit.vim
" Author: Taras Ivashchenko <naplanetu@gmail.com>
" Version: 1.0
" License: GPL
"
" Description: 
" This script looks up a word under cursor in a dictionary using custom utility 
" such as sdcv (console version of StarDict program)
"
" User-provided mappings can be used instead by mapping to <Plug>CommandName, for instance:
"
" nmap ,d <Plug>TranslateIt
"
" The default mappings are as follow:
"
"   <Leader>d TranslateIt
"
" Several variables are checked by the script to determine behavior as follow:
"
" TranslateIt_Bin
"   Path to the dictionary binary utility
"
" Installation:
" Put this file into your $HOME/.vim/plugin directory.

if !exists('TranslateIt_Bin')
	let g:TranslateIt_Bin = "sdcv"
endif

" Section: Utility functions
function! s:TranslateIt()
	let s:phrase  = expand("<cword>")
	let s:tmpfile = tempname()
  
	silent execute "!" . g:TranslateIt_Bin . " " . shellescape(s:phrase) . " > " . s:tmpfile
	let s:lines = system("wc -l " . s:tmpfile . "| awk '{print $1}'") 

	if s:lines == 0
		echo s:phrase . ": Not found."
	else
		execute "botright sp " . s:tmpfile
	end
endfun

" Section: Command definitions 
command! TranslateIt call s:TranslateIt()

" Section: Plugin command mappings
nnoremap <silent> <Plug>TranslateIt :TranslateIt<CR>

" Section: Default mappings
if !hasmapto('<Plug>TranslateIt')
	nmap <unique> <Leader>d <Plug>TranslateIt
endif

