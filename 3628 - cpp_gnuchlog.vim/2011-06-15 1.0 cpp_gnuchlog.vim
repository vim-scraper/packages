" Vim filetype plugin file extension
"
" Language:	        cpp
" Maintainer:       Jeffrey Wildman <jeffrey.wildman@gmail.com>
" Latest Revision:  1.0
" Last Change:      06-15-2011 
" License:          This file is placed in the public domain.
"
" Description:  Adapts the gnuchlog.vim plugin to use the
"               ctags.vim plugin's tag finding function.

if exists("b:cpp_gnuchlog_did_ftplugin") | finish | endif
let b:cpp_gnuchlog_did_ftplugin = 1

if !exists('*CppGnuChangeLogTagFinder')
    if exists('*GetTagName')
        " use ctags.vim's GetTagName function to get the tag
        function! CppGnuChangeLogTagFinder()
            let funcname = GetTagName(line("."))
            return funcname
        endfunction
    else
        function! CppGnuChangeLogTagFinder()
            return ''
        endfunction
    endif
endif

" set the gnuchlog.vim's tagfinder function name to our function here
" and it will call it to get the name of the class, method or function
" where the cursor was when the user opened the ChangeLog file.
let b:changelog_tagfinder = function('CppGnuChangeLogTagFinder')

