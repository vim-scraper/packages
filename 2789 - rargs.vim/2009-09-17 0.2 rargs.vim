scriptencoding utf-8
if &cp || exists('g:loaded_rargs')
    finish
endif
let g:loaded_rargs = 1

" escape user configuration
let s:save_cpo = &cpo
set cpo&vim

" exist when catch error option. default not exit.
let s:exit_when_error = '0'
if exists('g:rargs_exit_when_error')
    let s:exit_when_error = g:rargs_exit_when_error
else

" read file at insertat
function! s:RArgs(insertat,...)
    " get option and file list.
    let [l:optlist, l:filelist] = s:GetOpt(a:000)

    " file readable check
    for l:f in l:filelist
        let l:hasError = '0'
        if ! filereadable(l:f)
            echohl WarningMsg | echo 'rargs.vim Warning : selected file "' . l:f . '" is not readable' | echohl None
            let l:hasError = '1'
        endif

        " error is found.
        if l:hasError !=# '0'
            if s:exit_when_error !=# '0'
                return
            else
                continue
            endif
        endif
    endfor

    " reverse filelist
    call reverse(l:filelist)

    for l:f in l:filelist
        if filereadable(l:f)
            try
                let l:cmd = ':' . a:insertat . 'r ' . join(l:optlist, ' ') . ' ' . l:f
                silent execute l:cmd | " comment
            catch
                echohl WarningMsg | echo 'rargs.vim Warning : catch error when reading "' .l:f. '". ' . v:exception | echohl None
                if s:exit_when_error !=# '0'
                    return
                else
                    continue
                endif
            endtry
        endif
    endfor
endfunction

" return file list, and command option.
" [optionlist, filelist]
function! s:GetOpt(paramlist)
    let l:optlist = []
    let l:filelist = []

    for l:argv in a:paramlist
        " option, ++ started
        if strpart(l:argv, 0, 2) == '++'
            call add(l:optlist, l:argv)
        " file list
        else
            call extend(l:filelist, split(expand(l:argv), '\n'))
        endif
    endfor

    return [l:optlist, l:filelist]
endfunction

command! -narg=* -range=1 -complete=file RArgs :call s:RArgs(<line1>,<f-args>)

" recover user configuration
let &cpo = s:save_cpo
finish

==============================================================================
" vim: set ff=unix et ft=vim nowrap :
==============================================================================
rargs.vim : read the contents of many files at one time.
------------------------------------------------------------------------------
$VIMRUNTIMEPATH/plugin/rargs.vim
==============================================================================
author  : OMI TAKU
url     : http://nanasi.jp/
email   : mail@nanasi.jp
version : 2009/09/17 14:00:00
==============================================================================
':RArgs' command that this plugin defined can read
selected files contents at one time.
( like :read command, but for multiple files. )


------------------------------------------------------------------------------
[command format]

" read {filename1}, {filename2}, and {filename3},
" contents are inserted into N.
:[N]RArgs [++opt] {filename1} [{filename2} {filename3}...]

[N]
    contents read are inserted into the specified line,
    or not specify line, contents are inserted into cursor position.

[++opt]
    file reading option.
    see ':help ++opt', for more information.

{filename1} [{filename2} {filename3}...]
    filepath list.


------------------------------------------------------------------------------
[filepath parameters]

" read sample1.txt, sample2.txt, and sample3.txt
:RArgs sample1.txt sample2.txt sample3.txt

" wildcards are allowed
:RArgs sample*

" Vim special keywords are allowed
:RArgs #2 #4 #6<.bak

" reading the file at multiple times are allowed
:RArgs sample1.txt sample1.txt sample1.txt

" file encoding, file format
:RArgs ++enc=utf-8 ++ff=unix sample1.txt sample2.txt sample3.txt


------------------------------------------------------------------------------
[inserting file position]

If you do not specify line number,
the contents of selected files are inserted into cursor line.

" contents are inserted into cursor line.
:RArgs sample1.txt sample2.txt sample3.txt

" contents are inserted into specified line.
:200RArgs sample1.txt sample2.txt sample3.txt

" contents are inserted into first line of buffer.
:0RArgs sample1.txt sample2.txt sample3.txt


------------------------------------------------------------------------------
[Configuration]

'g:rargs_exit_when_error'

    If you set 1 to 'g:rargs_exit_when_error',
    exit when error is found. Default aciion is error is ignored.

        let g:rargs_exit_when_error = '1'


------------------------------------------------------------------------------
[History]
2009/09/16  0.1
    - initial version.

2009/09/17  0.2
    - add exception handling logic.
    - ++opt parameter is supported.
    - option 'g:rargs_exit_when_error' is supported.


==============================================================================
" vim: set ff=unix et ft=vim nowrap :
