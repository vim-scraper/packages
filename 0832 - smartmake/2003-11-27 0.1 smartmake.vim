" This plugin provides a smart version of the :make command:
" Before running the compiler, it scans the current and all parent 
" directories to figure out which compiler to use.
"
" Author: Mathias Hasselmann
" License: Public Domain
"
" Bindings: <F7> - Builds the default target of the current project
" Commands: SmartMake - A smart version of :make
" Compilers: make, ant, nant
"

function! s:SmartMake(...)
	let args = ''
	let argi = 1
	
	while argi <= a:0
		exec 'let argv = a:'.argi
		let args = args.' '.argv
		let argi = argi + 1
	endwhile

	let cdir = getcwd()

	while '' != cdir

		if '' != glob(cdir.'/{GNUmakefile,[Mm]akefile}')
			unlet! g:current_compiler
			runtime compiler/make.vim
			make

			return
		endif

		if '' != glob(cdir.'/build.xml')
			unlet! g:current_compiler
			runtime compiler/ant.vim
			make

			return
		endif

		if '' != glob(cdir.'/*.build')
			unlet! g:current_compiler
			runtime compiler/nant.vim
			make

			return
		endif

		let cdir = substitute(cdir, '/[^/]*$', '', '')

	endwhile
endfunction

command! -nargs=* SmartMake call <SID>SmartMake(<f-args>)

if !exists("no_plugin_maps") && !exists("no_smartmake_maps")
  noremap <F7> :update<cr>:call <SID>SmartMake()<cr>
  vnoremap <F7> <C-C>:update<cr><C-C>:call <SID>SmartMake()<cr>
  inoremap <F7> <C-O>:update<cr><C-O>:call <SID>SmartMake()<cr>
endif

" vim:ts=4 sw=4
