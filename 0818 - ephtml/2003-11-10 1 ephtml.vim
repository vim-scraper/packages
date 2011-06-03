" Vim syntax file
" Language:    	ephtml (extended Perl html)
" Maintainer:	Dr. Charles E. Campbell, Jr. <Charles.E.Campbell.1@nasa.gov>
" Last Change:	Nov 10, 2003
" Version:     	1	NOT RELEASED

" Quit when a syntax file was already loaded
if exists("b:current_syntax")
 finish
endif
runtime! syntax/html.vim

if filereadable($VIMRUNTIME."/syntax/perl.vim")
 unlet! b:current_syntax
 syn include @ephtmlPerlScript $VIMRUNTIME/syntax/perl.vim
 syn region ephtmlPerlRegion matchgroup=htmlTag start="<ephtml>" end="</ephtml>" contains=@ephtmlPerlScript
 syn region ephtmlComment    matchgroup=htmlComment start="<ep-comment>" end="</ep-comment>"
endif

let b:current_syntax= "ephtml"

" vim: ts=8
