" Title: head.vim : easy reading file head or tail, with file prefix 'head:', 'tail:'. (like sudo.vim)
" File: head.vim
" Author: OMI TAKU (mail@nanasi.jp)
" URL: http://nanasi.jp/
" Version: 0.8
" Last Modified: February 29, 2008
"
" Overview
" --------
" Open file with file prefix 'head:','tail:',
" vim editor read file 10 rows from top, or bottom.
" (for example, head:sample.pl, tail:somedir/app.conf)
"
" command example,
"   - Windows
"   (within vim)  : :e head;TEST.txt
"                 : :e tail;TEST.txt
"
"   - Mac OSX, Unix, Liux
"   (command line): vim head:TEST.txt
"                 : vim tail:TEST.txt
"
"   (within vim)  : :e head:TEST.txt
"                 : :e tail:TEST.txt
"
"
" Installation
" ------------
" 1. Copy the head.vim script to the $HOME/.vim/plugin or
"    the $HOME/vimfiles/plugin. Refer to the ':help add-plugin',
"    ':help add-global-plugin' and ':help runtimepath' topics for
"    more details about Vim plugins.
" 2. Restart Vim.
"
"
" Usage
" -----
" (Windows)
" :e head;TEST.TXT  |  display 'TEST.TXT' head. (default 10 rows read.)
" :r head;TEST.TXT  |  read 'TEST.TXT' head, and insert at cursor position. (default 10 rows read.)
" :w head;TEST.TXT  |  update 'TEST.TXT' head with current buffer. (default 10 rows replaced.)
"
" :e tail;TEST.TXT  |  display 'TEST.TXT' tail. (default 10 rows read.)
" :r tail;TEST.TXT  |  read 'TEST.TXT' tail, and insert at cursor position. (default 10 rows read.)
" :w tail;TEST.TXT  |  update 'TEST.TXT' tail with current buffer. (default 10 rows replaced.)
"
" (OSX, Unix, Linux)
" :e head:TEST.TXT  |  display 'TEST.TXT' head. (default 10 rows read.)
" :r head:TEST.TXT  |  read 'TEST.TXT' head, and insert at cursor position. (default 10 rows read.)
" :w head:TEST.TXT  |  update 'TEST.TXT' head with current buffer. (default 10 rows replaced.)
"
" :e tail:TEST.TXT  |  display 'TEST.TXT' tail. (default 10 rows read.)
" :r tail:TEST.TXT  |  read 'TEST.TXT' tail, and insert at cursor position. (default 10 rows read.)
" :w tail:TEST.TXT  |  update 'TEST.TXT' tail with current buffer. (default 10 rows replaced.)
"
"
" Plugin Options
" --------------
" g:head_display_lines
"   customize number of lines of file reading.
"   below is setting example.
"
"       :let g:head_display_lines = 30
"
"   you will update this parameter by command 'Head',
"   for example,
"
"       :Head 30
"

:if exists('loaded_head')
    :finish
:endif
:let loaded_head = 1

:if exists('g:head_file_splitter')
    :let s:spc = g:head_file_splitter
:else
    :if has('win32')
        :let s:spc = ";"
    :else
        :let s:spc = ":"
    :endif
:endif

:if exists('g:head_display_lines')
    :let s:viewsize = g:head_display_lines
:else
    :let s:viewsize = 10
:endif

:function! s:UpdateViewSize(size)
    :let s:viewsize = a:size
:endfunction

:function! s:HeadRead(path)
    :if a:path == "<afile>"
        :let l:file = expand(a:path)
    :else
        :let l:file = a:path
    :endif

    :let l:prot = matchstr(l:file,'^\(head\)\ze' . s:spc)
    :if l:prot != ''
        :let l:file = strpart(l:file, strlen(l:prot) + 1)
    :endif

    :0,$d
    :call setline(1,"foo")
    :let l:lines = readfile(l:file, "", s:viewsize)
    :let l:i = 0
    :while l:i < len(l:lines)
        :call setline(l:i + 2, l:lines[l:i])
        :let l:i += 1
    :endwhile
    :1d

    :set nomodified
    :filetype detect
:endfunction

:function! s:HeadWrite(path)
    :if a:path == "<afile>"
        :let l:file = expand(a:path)
    :else
        :let l:file = a:path
    :endif

    :let l:prot = matchstr(l:file,'^\(head\)\ze' . s:spc)
    :if l:prot != ''
        :let l:file = strpart(l:file, strlen(l:prot) + 1)
    :endif

    :let choice = confirm("Are you sure, update '".l:file."' head ?", "&Update\n&Cancel")
    :if choice == 1
        :let l:lines = filereadable(l:file)? readfile(l:file): []
        :if len(l:lines) > s:viewsize
            :let l:i = 0
            :while l:i < s:viewsize
                :unlet l:lines[l:i- 1]
                :let l:i += 1
            :endwhile

            :let l:curbufs = getline(0, line("$"))
            :call extend(l:curbufs, l:lines)
            :call writefile(l:curbufs, l:file)
            :set nomodified
        :else
            :call writefile(getline(0, line("$")), l:file)
            :set nomodified
        :endif
        :return
    :else
        :return
    :endif
:endfunction

:function! s:TailRead(path)
    :if a:path == "<afile>"
        :let l:file = expand(a:path)
    :else
        :let l:file = a:path
    :endif

    :let l:prot = matchstr(l:file,'^\(tail\)\ze' . s:spc)
    :if l:prot != ''
        :let l:file = strpart(l:file, strlen(l:prot) + 1)
    :endif

    :0,$d
    :call setline(1,"foo")
    :let l:lines = readfile(l:file)
    :if len(l:lines) > s:viewsize
        :let l:i = 0
        :while l:i < s:viewsize
            :call setline(l:i + 2, l:lines[len(l:lines) - s:viewsize + l:i])
            :let l:i += 1
        :endwhile
    :else
        :let l:i = 0
        :while l:i < len(l:lines)
            :call setline(l:i + 2, l:lines[l:i])
            :let l:i += 1
        :endwhile
    :endif
    :1d

    :set nomodified
    :filetype detect
:endfunction

:function! s:TailWrite(path)
    :if a:path == "<afile>"
        :let l:file = expand(a:path)
    :else
        :let l:file = a:path
    :endif

    :let l:prot = matchstr(l:file,'^\(tail\)\ze' . s:spc)
    :if l:prot != ''
        :let l:file = strpart(l:file, strlen(l:prot) + 1)
    :endif

    :let choice = confirm("Are you sure, update '".l:file."' tail ?", "&Update\n&Cancel")
    :if choice == 1
        :let l:lines = filereadable(l:file)? readfile(l:file): []
        :if len(l:lines) > s:viewsize
            :let l:i = 0
            :while l:i < s:viewsize
                :unlet l:lines[len(l:lines) - 1]
                :let l:i += 1
            :endwhile
            :call extend(l:lines, getline(0, line("$")))
            :call writefile(l:lines, l:file)
            :set nomodified
        :else
            :call writefile(getline(0, line("$")), l:file)
            :set nomodified
        :endif
        :return
    :else
        :return
    :endif
:endfunction

:augroup Head
    :autocmd!
    " head:test.txt
    :execute ":autocmd BufReadCmd   head" .s:spc. "*,head" .s:spc. "*/* HeadRead  <afile>"
    :execute ":autocmd FileReadCmd  head" .s:spc. "*,head" .s:spc. "*/* HeadRead  <afile>"
    :execute ":autocmd BufWriteCmd  head" .s:spc. "*,head" .s:spc. "*/* HeadWrite <afile>"
    :execute ":autocmd FileWriteCmd head" .s:spc. "*,head" .s:spc. "*/* HeadWrite <afile>"
:augroup END
:augroup Tail
    :autocmd!
    " tail:test.txt
    :execute ":autocmd BufReadCmd   tail" .s:spc. "*,tail" .s:spc. "*/* TailRead  <afile>"
    :execute ":autocmd FileReadCmd  tail" .s:spc. "*,tail" .s:spc. "*/* TailRead  <afile>"
    :execute ":autocmd BufWriteCmd  tail" .s:spc. "*,tail" .s:spc. "*/* TailWrite <afile>"
    :execute ":autocmd FileWriteCmd tail" .s:spc. "*,tail" .s:spc. "*/* TailWrite <afile>"
:augroup END

:command! -nargs=1 HeadRead  :call s:HeadRead(<f-args>)
:command! -nargs=1 HeadWrite :call s:HeadWrite(<f-args>)
:command! -nargs=1 TailRead  :call s:TailRead(<f-args>)
:command! -nargs=1 TailWrite :call s:TailWrite(<f-args>)

:command! -nargs=1 Head      :call s:UpdateViewSize(<f-args>)

" vim: set et nowrap ff=unix ft=vim :
