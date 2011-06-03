"" -*- vim -*-
"" FILE:        fileaccess.vim
"" Description: access time based simple file explorer for vim
"" Version:     $Revision: 0.8 $ $Date: 2005/01/15 06:34:17 $
"" Author:      Masashi Watanabe  (watanama@tbf.t-com.ne.jp)
""
"" Description:
""   At first, this script looks like mruex.vim script. Please choose favarite
""   one among them.
""
""   When we work, we access many files, and in general they are in different
""   directories. Thus we have to type related( or absolute ) path or move
""   to the directory which includes the file which we want to access. 
""
""   This script help you to omit to type such a tiresome work. This script
""   keeps file name and its absolute directory information which you accessed,
""   and you can briefly access the file which you read or wrote previously.
""
"" Instructions:
""   1 - put in your plugin directory, and access some files.
""   2 - type :FileAcess
""   3 - window will change and you can see some files which you accessed in
""       former times. And move cursor on a line which includes the file name
""       you want to open again and return.
""       If you want to keep file list window, put 'o' in stead of return.
""
"" Note:
""  This script creates log file to keep accessed file information, and you can
""  customize its file-name and path. Still more you can specify the max number
""  of file which are kept in log file. See "Global Configuration Variables"
""
"" Global Configuration Variables:
""  FileAccessFileListMax : database file keeps file info less than it
""                          (default: 10)
""  FileAccessDbFileName  : database file's name (default: FileAcess.log)
""  FileAccessDbFilePath  : database file's path
""                                          (default: win   => $VIMRUNTIME/plugin
""                                                    other => $HOME/.vim/plugin
""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Section: Envirionment check                                               {{{1
" version check
if v:version < 600
  finish
endif


" Section: Public variable and command definitions
if !exists("g:FileAccessFileListMax") | let g:FileAccessFileListMax = 10               | endif
if !exists("g:FileAccessDbFileName" ) | let g:FileAccessDbFileName  = "fileaccess.log" | endif
if has("win32") || has("win16")
  if !exists("g:FileAccessDbFilePath" ) | let g:FileAccessDbFilePath  = $VIMRUNTIME . "/plugin" | endif
else
  if !exists("g:FileAccessDbFilePath" ) | let g:FileAccessDbFilePath  = $HOME . "/.vim/plugin" | endif
endif

command! -nargs=0 FileAccess call <SID>FileAccess()


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Section: Global private variable definitions                              {{{1
" private
let s:FilePassSpace = 20
let s:LogFileName   = g:FileAccessDbFilePath . "/" . g:FileAccessDbFileName
let s:LineRegexp    = '\(.*\)@@@\(.*\) \(.*\)@@@\(.*\)'
"                      file     date   time     path


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Section: FileAccess (Create file list window and put accessed file list)  {{{1
function! <SID>FileAccess()
  " check whether current window has been modified or not
  if &modified
    let l:input = confirm("File " . expand("%:p") . "has been modified.", "&Yes\n&No", 1, "Info")
    if l:input == 1
      if expand("%:p") == ""
        let l:new_file = input("Enter file name ")
        if l:new_file == ""
          return
        else
          silent! execute "write " . l:new_file
        endif
      else
        silent! w!
      endif
    else
      return
    endif
  endif

  " create file list window
  silent! execute "edit " . tempname()

  " Highlight the comments, directories, tag types and tag names
  if has('syntax')
    syn match FileAccessFile  "\] [^ ]* "hs=s+2
    hi def link FileAccessFile Type

    syn match FileAccessComment  "^\" .*"
    hi def link FileAccessComment Comment
  endif

  silent! call append(0, "\" <Enter>=current window  o=new window")
  execute "read " . s:LogFileName

  let l:tail_line = line('$')

  let l:cur_line = 3
  while l:cur_line <= l:tail_line
    let l:my_file = substitute( getline(l:cur_line), s:LineRegexp, '\1', '')
    let l:my_date = substitute( getline(l:cur_line), s:LineRegexp, '\2', '') . " " . substitute( getline(l:cur_line), s:LineRegexp, '\3', '')
    let l:my_path = substitute( getline(l:cur_line), s:LineRegexp, '\4', '')

    let l:my_file_length = strlen(l:my_file)
    if l:my_file_length < s:FilePassSpace | let l:my_file_length = s:FilePassSpace | endif
    let l:my_file = l:my_file . "               "
    let l:my_file = strpart(l:my_file, 0, l:my_file_length)
    silent! call append(line('$'), "[" . l:my_date . "] " . l:my_file . " " . l:my_path)

    let l:cur_line = l:cur_line + 1
  endwhile

  silent! execute "3," . l:tail_line . " delete"

  " file list window setting
  setlocal buftype=nowrite
  setlocal nowrap

  nnoremap <buffer> <CR> :call <SID>FileAccessOpen( 1 )<CR>
  nnoremap <buffer> o    :call <SID>FileAccessOpen( 2 )<CR>
endfunction


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Section: FileAccessOpen (Open the selected file)                          {{{1
" If selected line is less than 3, do nothing because maybe it is not file info.
function! <SID>FileAccessOpen( view_window )
  if line('.') < 3 | return | endif

  let s:ViewRegexp = '^\[[^ ]* [^ ]*] \([^ ]*\) *\(.*\)$'
  let l:my_file    = substitute( getline('.'), s:ViewRegexp, '\1', '')
  let l:my_path    = substitute( getline('.'), s:ViewRegexp, '\2', '')

  call <SID>FileAccessUpdateLog(l:my_file, l:my_path)

  if a:view_window == 1
    silent! execute "edit "  . l:my_path . "/" . l:my_file
  else
    silent! execute "split " . l:my_path . "/" . l:my_file
    wincmd x
    let l:max_height = 10
    if g:FileAccessFileListMax < l:max_height | let l:max_height = g:FileAccessFileListMax | endif
    execute "normal z" . l:max_height . "\<CR>|"
    wincmd j
  endif

endfunction


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Section: FileAccessUpdateLog (Update log file)                            {{{1
" If opening file has already entried in log file, move it up.
function! <SID>FileAccessUpdateLog(opened_file, opened_path)
  " If file is not specified, don't write down that info
  if  expand("%:p") == "" | return | endif

  " set write info
  let l:opened_time  = strftime("%Y/%m/%d %H:%M:%S")

  silent! execute "split " . s:LogFileName

  let l:cur_line = 1
  while l:cur_line <= line('$')
    let l:cur_file  = substitute( getline(l:cur_line), s:LineRegexp, '\1', '')
    let l:cur_path  = substitute( getline(l:cur_line), s:LineRegexp, '\4', '')

    let l:cur_file  = l:cur_path  . "/" . l:cur_file

    " If opening file has already entried in log file, mark it to remove
    if l:cur_file == a:opened_path . "/" . a:opened_file
      silent! execute l:cur_line . "s/.*/DEL_LINE/"
    endif

    " If file in log-file not found, mark it to remove
    if filereadable(l:cur_file) == 0
      silent! execute l:cur_line . "s/.*/DEL_LINE/"
    endif

    let l:cur_line = l:cur_line + 1
  endwhile

  " remove marked line
  silent! execute "g/^DEL_LINE$/delete"

  " add current file information to top of log-file
  silent! call append(0, a:opened_file . "@@@" . l:opened_time . "@@@" . a:opened_path)

  " If log file's line exceed specified value, remove oldest line
  if line('$') >= g:FileAccessFileListMax
    silent! execute  g:FileAccessFileListMax+1. ",$delete"
  endif

  if getline(line('$')) == ""
    silent! execute "$delete"
  endif

  silent! write
  silent! quit
endfunction


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Section: Execute when loaded                                              {{{1
call <SID>FileAccessUpdateLog( expand("%:p:t"),  expand("%:p:h"))
"}}}1

finish

" vim600: set foldmethod=marker :
