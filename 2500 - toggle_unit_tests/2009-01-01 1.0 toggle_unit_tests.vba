" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/toggle_unit_tests.vim	[[[1
72
" Vim global plug-in for toggling between source and unit test files.
" Last Change:	2009 January 01
" Maintainer:	Pete Johns <paj-vim@johnsy.com>
" License:	    This file is placed in the public domain.

if exists('g:loaded_toggle_unit_tests')
    finish
endif
let g:loaded_toggle_unit_tests = 1

if v:version < 700
    echohl WarningMsg|echomsg 'Toggle Unit Tests requires at least VIM 7.0'|echohl None
    finish
endif

let s:save_cpo = &cpo
set cpo&vim

if !hasmapto('<Plug>ToggleUnitTestsToggle')
    map <unique> <Leader>tut  <Plug>ToggleUnitTestsToggle
endif
noremap <unique> <script> <Plug>ToggleUnitTestsToggle  <SID>Toggle

noremenu <script> Plugin.Toggle\ Unit\ Tests      <SID>Toggle

noremap <SID>Toggle  :call <SID>Toggle()<CR>

if !exists('g:unit_test_prefix')
    let g:unit_test_prefix = 't_'
endif

function s:Toggle()
    " This function assumes source and unit test files are in the same
    " directory, as I believe this to be best practice.
    
    " Little point executing if there's no file open.
    if expand('%') == ''
        echohl WarningMsg|echomsg 'No file open.'|echohl None
        return
    endif

    " Write file if necessary.
    update

    " Determine alternative filename.
    let l:unit_test_regex = '^'.g:unit_test_prefix
    if expand('%:e') == 'h'
        "For C++ header files, deliver to corresponding source.
        for extension in ['cxx', 'cc', 'C', 'c++', 'SC', 'c', 'cpp']
            let l:alternate = expand('%:t:r').'.'.extension
            if file_readable(l:alternate)
                break
            endif
        endfor
    elseif expand('%:t') =~ l:unit_test_regex
        "This is the unit test, remove the prefix.
        let l:alternate = substitute(expand('%:t'), l:unit_test_regex, '', '')
    else
        "This is the SUT, prepend 't_'.
        let l:alternate = g:unit_test_prefix.expand('%:t')
    endif

    " Open the file.
    execute 'edit '.expand('%:p:h').'/'.l:alternate
endfunction

if !exists(":TUT")
    command -nargs=0  TUT  :call s:Toggle()
endif

let &cpo = s:save_cpo

doc/toggle_unit_tests.txt	[[[1
45
*toggle_unit_tests.txt*	plug-in for toggling between source and unit test files.
Copyright (c) 2009 Pete Johns <paj-vim@johnsy.com>

For instructions on installing this file, type
    :help add-local-help
inside Vim.

                                                     *toggle_unit_tests-usage*
Usage:

Use this plug-in when you are doing Test-Driven Development [TDD] in any
language to quickly switch between unit-test and production code. To make this
really fast bind the Toggle function to a function key in your .vimrc:

    "   [F3]    toggles between (header, ) source and test files.
    nnoremap    <F3> :<C-U>TUT<CR>

This plug-in assumes source and unit test files are in the same directory, as
I believe this to be best practice.

                                                  *toggle_unit_tests-mappings*
Mappings:

<Leader>tut   or   <Plug>ToggleUnitTestsToggle
    Toggles betwixt source and unit test file

                                                  *toggle_unit_tests-commands*

Commands:

:TUT
	Toggles betwixt source and unit test file

                                                  *toggle_unit_tests-settings*
Settings:

By default, unit_test filenames are deemed to begin with 't_'. 

Tell toggle_unit_tests.vim your own prefix by setting the following global
variable, perhaps in your .vimrc:

    let g:unit_test_prefix='test_'

-- 
vim:tw=78:ts=8:ft=help
