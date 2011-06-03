" Vim syntax file
" Language:     tforge Text::Forge, HTML with embedded Perl
" Maintainer:   Adam Monsen <amonsen@corp.classmates.com
" URL:          http://text-forge.sourceforge.net
" Remark:       based on aasp.vim, syntax file for Apache::ASP code
" Last change:  2002 Mar 09

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

runtime! syntax/html.vim
unlet b:current_syntax
syn include @Perl syntax/perl.vim
syn cluster htmlPreproc add=tforgePerlInsideTags
syn cluster tforgePrePerl add=textForgePrePerl
syn cluster tforgePostPerl add=textForgePostPerl

syntax region tforgePerlInsideTags keepend matchgroup=Delimiter start=+<%[=$?]\=+ skip=+[^\\]".{-}[^\\]"+ end=+%>+ contains=@Perl
syntax region textForgePrePerl keepend start=/^use/ end=/\c<FORGE>/ contains=@Perl
syntax region textForgePostPerl keepend start=/\c<\/FORGE>/ end=/^1$/ contains=@Perl

let b:current_syntax = "tforge"

" vim: ts=8 sw=8
