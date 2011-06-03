" Vim R plugin file for Mac OS X
" Language:     R
" Maintainer:   Jarimatti Valkonen <javalkon@hytti.uku.fi>
" License:      This file is under the Vim license.
" Last Change:  2006 Dec 23
" Version:      0.1
"
" Interface between R (www.r-project.org) and Vim on Mac OS X.
"
"
" Enables one to source current file or evaluate line or selection in R via
" shell command 'osascript'. <Cmd-E> evaluates line or selection and
" <Shift-Cmd-E> sources current file. The mappings are defined at the end of
" this file. They are not yet customizable. Patches accepted!


" Only do this when not done yet for this buffer
if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

if !exists("*s:EscapeR")
function s:EscapeR (text)
    let z = a:text
    let z = escape(z, "\\")
    let z = escape(z, '"')
    let z = escape(z, "'")
    let z = escape(z, "!")  " Omit this and die! Causes funny errors when
                            " there's an exlamation mark in the string.
    let z = escape(z, "%")  " Escape the current buffer name.
    let z = escape(z, "#")  " Escape the alternate file name.
    let z = substitute(z, "\n", '\\n', "g")
    return (z)
endfunction
endif

" Get the full path and buffer name. Now the file can be sourced regardless of
" R's working directory.
if !exists("*s:FullName")
function s:FullName (buffer)
    return getcwd() . "/" . bufname(a:buffer)
endfunction
endif


" Function to source file in existing R session:
if !exists("*s:Source_file")
function s:Source_file(file)
  execute "!osascript -e 'tell application \"R\" to cmd \"source(\\\"" . a:file . "\\\")\"'"
endfunction
endif

if !exists("*s:Source_text")
function s:Source_text(text)
  let text = s:EscapeR(a:text)
  execute ":!osascript -e 'tell application \"R\" to cmd \"" . text . "\"'"
endfunction
endif

if !exists("*s:Source")
function s:Source () range
  let a = a:firstline
  let teksti = getline(a)
  let a = a + 1
  while a <= a:lastline
    let teksti = teksti . "\n" . getline(a)
    let a = a + 1
  endwhile

  call s:Source_text(teksti)
endfunction
endif

command -buffer         SourceFile  call s:Source_file(s:FullName("%"))
command -buffer -range  Source      <line1>,<line2>call s:Source()

nmap <buffer> <D-e> :Source<CR>
vmap <buffer> <D-e> :Source<CR>
imap <buffer> <D-e> <ESC>:Source<CR>gi

nmap <buffer> <S-D-e> :SourceFile<CR>
imap <buffer> <S-D-e> <ESC>:SourceFile<CR>gi
