" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/toggle_unit_tests.vim	[[[1
124
" Vim global plug-in for toggling between source and unit test files.
" Last Change:	Wed  5 Aug 2009 09:36:13 EST
" Maintainer:	Pete Johns <paj-vim@johnsy.com>
" License:	    This file is placed in the public domain.
" Notes:        This plugin assumes source and unit test files are in the same
"               directory, as I believe this to be best practice.


if exists('g:loaded_toggle_unit_tests')
    finish
endif
let g:loaded_toggle_unit_tests = 1


function! s:EchoWarning(msg)
    echohl WarningMsg
    echomsg a:msg
    echohl None
endfunction


if v:version < 700
    call s:EchoWarning('Toggle Unit Tests requires at least VIM 7.0')
    finish
endif


let s:save_cpo = &cpo
set cpo&vim


if !hasmapto('<Plug>ToggleUnitTestsToggle')
    map <unique> <Leader>tut  <Plug>ToggleUnitTestsToggle
endif
noremap <unique> <script> <Plug>ToggleUnitTestsToggle  <SID>Toggle
noremap <SID>Toggle  :call <SID>Toggle()<CR>
noremenu <script> Plugin.Toggle\ Unit\ Tests      <SID>Toggle


if !exists('g:unit_test_prefix')
    let g:unit_test_prefix = 't_'
endif


function! s:NoFileOpen()
    return expand('%') == ''
endfunction


function! s:CurrentFileIsCppHeader()
    return expand('%:e') == 'h' 
endfunction


function! s:GetCppSourceForHeader()
    for l:extension in ['cxx', 'cc', 'C', 'c++', 'SC', 'c', 'cpp']
        let l:alternate = expand('%:t:r').'.'. l:extension
        if file_readable(l:alternate)
            break
        endif
    endfor

    return l:alternate
endfunction


function! s:CurrentFileIsTest()
    let l:unit_test_regex = '^'.g:unit_test_prefix
    return expand('%:t') =~ l:unit_test_regex 
endfunction


function! s:GetSutFromTest()
    let l:unit_test_regex = '^'.g:unit_test_prefix
    return substitute(expand('%:t'), l:unit_test_regex, '', '')
endfunction


function! s:GetTestFromSut()
    return g:unit_test_prefix.expand('%:t')
endfunction


function! s:DetermineAlternativeFilename()
    if s:CurrentFileIsCppHeader()
        let l:alternate = s:GetCppSourceForHeader()
    elseif s:CurrentFileIsTest()
        let l:alternate = s:GetSutFromTest()
    else
        let l:alternate = s:GetTestFromSut()
    endif
    return l:alternate
endfunction


function! s:OpenFile(filename)
    let l:full_path = expand('%:p:h') . '/' . a:filename
    let l:already_opened_buffer = bufnr(l:full_path) != -1
    if l:already_opened_buffer 
        execute 'buffer! ' . l:full_path
    else
        execute 'edit! ' . l:full_path
    endif
endfunction


function! s:Toggle()
    if s:NoFileOpen()
        call s:EchoWarning('No file open.')
        return
    endif

    let l:alternate = s:DetermineAlternativeFilename()

    call s:OpenFile(l:alternate)
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
