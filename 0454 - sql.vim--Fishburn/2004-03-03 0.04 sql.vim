" SQL filetype plugin file
" Language:    SQL (Common for Oracle, Microsoft SQL Server, Sybase)
" Version:     0.04
" Maintainer:  David Fishburn <fishburn@ianywhere.com>
" Last Change: Wed Mar 03 2004 10:40:45 AM

" This file should only contain values that are common to all SQL languages
" Oracle, Microsoft SQL Server, Sybase ASA/ASE, MySQL, and so on
" If additional features are required create:
" vimfiles/after/ftplugin/sql.vim
" to override and add any of your own settings

" Only do this when not done yet for this buffer
if exists("b:did_ftplugin")
  finish
endif

" Don't load another plugin for this buffer
let b:did_ftplugin = 1

let s:save_cpo = &cpo
setlocal cpo&vim

" Define patterns for the matchit macro
if !exists("b:match_words")
    " SQL is generally case insensitive
    let b:match_ignorecase = 1
    let b:match_words =
		\ '\<begin\>:\<end\>\(;\)\?$,'.
		\ '\%(\<end\s\+\)\@<!' . '\<if\>:'.
		\ '\<elsif\>:\<elseif\>:\<else\>:\<end\s\+if\>,'.
		\ '\<loop\>:\<break\>:\<continue\>:'.
		\ '\%(\<end\s\+\)\@<!' . '\<loop\>:\<end\s\+loop\>,'.
		\ '\<for\>:\<break\>:\<continue\>:'.
		\ '\%(\<end\s\+\)\@<!' . '\<for\>:\<end\s\+for\>,'.
		\ '\<case\>:\<when\>:\<default\>:'.
		\ '\%(\<end\s\+\)\@<!' . '\<case\>:\<end\s\+case\>'
endif

" Define how to find the macro definition of a variable using the various
" [d, [D, [_CTRL_D and so on features
" Match these values ignoring case
" ie  DECLARE varname INTEGER
let &l:define = '\c\(DECLARE\|IN\|OUT\|INOUT\)\s*'

" Comment can be of the form:
"   /*
"    *
"    */
" or
"   // 
" or
"   --
setlocal comments=s1:/*,mb:*,ex:*/,:--,://

let &cpo = s:save_cpo

" vim:sw=4:ff=unix:

