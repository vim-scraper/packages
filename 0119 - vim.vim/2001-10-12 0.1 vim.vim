" Vim indent file
" Language:	Vim script
" Maintainer:	Benoit Cerrina (benoit.cerrina@writeme.com)
" Last Change:	2001 Oct 12
" This is a complementary file to add indentation to embedded scripts
" it is meant to be in the runtimepath BEFORE the regular vim indent script
if exists("b:did_indent") || exists("b:embedskipindent")
  finish
endif
runtime! indent/perl.vim
let s:perlIndentExpr=&indentexpr
unlet b:did_indent
runtime! indent/python.vim
let s:pythonIndentExpr=&indentexpr
unlet b:did_indent
runtime! indent/ruby.vim
let s:rubyIndentExpr=&indentexpr
unlet b:did_indent
runtime! indent/tcl.vim
let s:tclIndentExpr=&indentexpr
unlet b:did_indent
let b:embedskipindent=1
runtime! indent/vim.vim
unlet b:embedskipindent
let s:vimIndentExpr=&indentexpr
setlocal indentexpr=GetBenVimIndent()
setlocal indentkeys+==end,=else,0\\

" Only define the function once.
if exists("*GetBenVimIndent")
  finish
endif
"function to treat the case of embedded script
"lang is the filetype, this relies on the syntax group
"being named according to current conventions
function! GetLocalEmbededdScriptIndent(lang, synName, indentExpr)
  if a:synName =~ a:lang
	exec "let ret = " . a:indentExpr
	return ret
  else
	return -2
  endif
endfunction

function! GetBenVimIndent()
  let synName = synIDattr(synID(v:lnum, 1, 1), "name")

  "Keep the end delimiter of a script flush to the left
  "this work because the EndPattern of a here document must be
  "flush to the left so its syntax is set in the first column
  if synName=='vimScriptDelim'
	let prevSynName = synIDattr(synID(v:lnum-1, 1, 1), "name")
	"sanity check, make sure it is not the start delimiter
	if prevSynName=~'\c\(perl\|python\|ruby\|tcl\)'
	  return 0
	endif
  endif
  
  "Treat the case of embedded perl
  let ret = GetLocalEmbededdScriptIndent('\cperl', synName, s:perlIndentExpr)
  if  ret != -2
	return ret
  endif 
  "Treat the case of embedded python
  let ret = GetLocalEmbededdScriptIndent('\cpython', synName, s:pythonIndentExpr)
  if  ret != -2
	return ret
  endif 
  "Treat the case of embedded tcl
  let ret = GetLocalEmbededdScriptIndent('\ctcl', synName, s:tclIndentExpr)
  if  ret != -2
	return ret
  endif 
  "Treat the case of embedded ruby
  let ret = GetLocalEmbededdScriptIndent('\cruby', synName, s:rubyIndentExpr)
  if  ret != -2
	return ret
  endif 
	"regular vim indent
	exec "return ". s:vimIndentExpr
endfunction

" vim:sw=2
