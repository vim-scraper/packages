" Vim syntax file
" Language: kwiki
" Maintainer: Ruey-Cheng Chen <rueycheng@gmail.com>
" License: This file can be redistribued and/or modified under the 
"   same terms as Vim itself
" Version: 0.1
" Last Change: 2007-08-13
"
" Description:
" The script is based on the wiki.vim developed by Andreas Kneib
" <aporia@web.de> and Mathias Panzenböck <grosser.meister.morti@gmx.at>.
" The formatting rules are mainly derived (with modification) from 
" the perl module Kwiki::Formatter
" http://search.cpan.org/~ingy/Kwiki-0.39/lib/Kwiki/Formatter.pm
"
" Installation:
" Simply drop the file into your syntax directory ($HOME/.vim/syntax)
" and add one line into your filetype.vim
"
"     au BufRead,BufNewFile *.kwiki setf kwiki
"
" Changelog:
" 2007-08-13 v0.1
"     Initial release
"

" Quit if syntax file is already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

if version < 508
  command! -nargs=+ KwikiHiLink hi link <args>
else
  command! -nargs=+ KwikiHiLink hi def link <args>
endif

command! -nargs=+ KwikiHiDef hi def <args>

" Top-level
syn region kwikiComment start=/^#\s/ end=/$/ oneline
syn match kwikiLine /^----\+\s*$/
syn region kwikiHeading start=/^=\{1,6}\s\+./ end=/$/ oneline
syn region kwikiParagraph start=/\(^\([=\*0]\+ \|#\||\|\s\|\.\w\+\s*\n\|-\{4,}\s*\n\)\@!.*\S.*\)\+/ end=/^\s*$/ contains=@kwikiElement,@kwikiStyle
syn region kwikiUlist start=/\(^\*\+\)\@<=\s\+/ end=/$/ contains=@kwikiElement,@kwikiStyle
syn region kwikiOlist start=/\(^0\+\)\@<=\s\+/ end=/$/ contains=@kwikiElement,@kwikiStyle
syn region kwikiPreformatted start=/^ \+\S/ end=/$/ oneline
syn region kwikiTable start=/^|/ end=/|$/ oneline

" Style
syn cluster kwikiStyle contains=kwikiStrong,kwikiEmphasize,kwikiUnderline,kwikiInline,kwikiDelete,kwikiNone
"--------------------------------------------------
" syn region kwikiStrong start=/\(^\|[^A-Za-z0-9]\@<=\)\*\S\@=/ end=/\*\([^A-Za-z0-9]\|\s\|$\)\@=/
" syn region kwikiEmphasize start=/\(^\|[^A-Za-z0-9]\@<=\)\/\(\S[^\/]*\/\(\W\|\s\|$\)\)\@=/ end=/\/\([^A-Za-z0-9]\|\s\|$\)\@=/
" syn region kwikiUnderline start=/\(^\|[^A-Za-z0-9]\@<=\)_\S\@=/ end=/_\([^A-Za-z0-9]\|\s\|$\)\@=/
"-------------------------------------------------- 
syn match kwikiStrong /\(^\|[^A-Za-z0-9]\@<=\)\*\S\@=.\{-}\(\n.\{-1,}\)\{-}\*\([^A-Za-z0-9]\|\s\|$\)\@=/ contained contains=@kwikiElement,kwikiSE,kwikiSU
syn match kwikiEmphasize /\(^\|[^A-Za-z0-9]\@<=\)\/\(\S[^\/]*\/\(\W\|\s\|$\)\)\@=.\{-}\(\n.\{-1,}\)\{-}\/\([^A-Za-z0-9]\|\s\|$\)\@=/ contained contains=@kwikiElement,kwikiES,kwikiEU
syn match kwikiUnderline /\(^\|[^A-Za-z0-9]\@<=\)_\S\@=.\{-}\(\n.\{-1,}\)\{-}_\([^A-Za-z0-9]\|\s\|$\)\@=/ contained contains=@kwikiElement,kwikiUS,kwikiUE
syn region kwikiInline start=/\(^\|[^A-Za-z0-9]\@<=\)\[=/ end=/\]\([^A-Za-z0-9]\|\s\|$\)\@=/ contained
syn region kwikiDelete start=/\(^\|[^A-Za-z0-9]\@<=\)-\([^- ]\)\@=/ end=/-\([^A-Za-z0-9]\|\s\|$\)\@=/ contained
syn match kwikiNone /\/\// contained contains=NONE transparent

" Sub-style
syn match kwikiSE /\/.\{-}\(\n.\{-1,}\)\{-}\// contained contains=kwikiSEU
syn match kwikiSU /_.\{-}\(\n.\{-1,}\)\{-}_/ contained contains=kwikiSUE
syn match kwikiES /\*.\{-}\(\n.\{-1,}\)\{-}\*/ contained contains=kwikiESU
syn match kwikiEU /_.\{-}\(\n.\{-1,}\)\{-}_/ contained contains=kwikiEUS
syn match kwikiUS /\*.\{-}\(\n.\{-1,}\)\{-}\*/ contained contains=kwikiUSE
syn match kwikiUE /\/.\{-}\(\n.\{-1,}\)\{-}\// contained contains=kwikiUES
syn match kwikiSEU /_.\{-}\(\n.\{-1,}\)\{-}_/ contained contains=NONE
syn match kwikiSUE /\/.\{-}\(\n.\{-1,}\)\{-}\// contained contains=NONE
syn match kwikiESU /_.\{-}\(\n.\{-1,}\)\{-}_/ contained contains=NONE
syn match kwikiEUS /\*.\{-}\(\n.\{-1,}\)\{-}\*/ contained contains=NONE
syn match kwikiUSE /\/.\{-}\(\n.\{-1,}\)\{-}\// contained contains=NONE
syn match kwikiUES /\*.\{-}\(\n.\{-1,}\)\{-}\*/ contained contains=NONE

" Entity
syn cluster kwikiEntity contains=kwikiMDash,kwikiNDash,kwikiAsis
syn match kwikiMDash /-\{3}[^-]\@=/ contained
syn match kwikiNDash /-\{2}[^-]\@=/ contained
syn region  kwikiAsis            start=/{{/ end=/}}/

" Link, NoLink, and TitledLink
syn cluster kwikiLink contains=kwikiForcedLink,kwikiHyperLink,kwikiWikiLink,kwikiMailLink
syn cluster kwikiNoLink contains=kwikiNoHyperLink,kwikiNoWikiLink,kwikiNoMailLink
syn cluster kwikiTitledLink contains=kwikiTitledHyperLink,kwikiTitledWikiLink,kwikiTitledMailLink
syn match kwikiForcedLink /\[\w\+\]/ contained
syn match kwikiHyperLink /\w\+:\(\/\/\|?\)[^),.:; ]\(\S\)\{-}\([),.:;]\=\s\|$\)\@=/ contained
syn match kwikiNoHyperLink /!\w\+:\(\/\/\|?\)[^),.:; ]\(\S\)\{-}\([),.:;]\=\s\|$\)\@=/ contained contains=NONE transparent
syn match kwikiTitledHyperLink /\[\(\s*[^\]]\+\s\+\)\?\(\w\+:\(\/\/\|?\)[^\] ]\+\)\(\s\+[^\]]\+\s*\)\?\]/ contained
syn match kwikiWikiLink /\u\(\w*\u\)\@=\(\w*\l\)\@=\w\+/ contained
syn match kwikiNoWikiLink /!\u\(\w*\u\)\@=\(\w*\l\)\@=\w\+/ contained contains=NONE transparent
syn match kwikiTitledWikiLink /\[\([^\]]*\)\s\+\(\u\(\w*\u\)\@=\(\w*\l\)\@=\w\+\)\]/ contained
syn match kwikiMailLink /[A-Za-z0-9][A-Za-z0-9_+.\-]*@\w[A-Za-z0-9.\-]\+/ contained
syn match kwikiNoMailLink /![A-Za-z0-9][A-Za-z0-9_+.\-]*@\w[A-Za-z0-9.\-]\+/ contained contains=NONE transparent
syn match kwikiTitledMailLink /\[\([^\]]\+\)\s\+\([A-Za-z0-9][A-Za-z0-9_+\-.]*@\w[A-Za-z0-9.\-]\+\)\]/ contained

" Element
syn cluster kwikiElement contains=@kwikiEntity,@kwikiLink,@kwikiNoLink,@kwikiTitledLink
syn sync linebreaks=100
syn sync minlines=100

" The default highlighting.
if version >= 508 || !exists("did_wiki_syn_inits")
  if version < 508
    let did_wiki_syn_inits = 1
  endif

  KwikiHiLink kwikiComment Comment
  KwikiHiLink kwikiLine Todo
  KwikiHiLink kwikiHeading Statement
  KwikiHiLink kwikiParagraph Normal
  KwikiHiLink kwikiOlist Type
  KwikiHiLink kwikiUlist Type
  KwikiHiLink kwikiPreformatted Identifier
  KwikiHiLink kwikiTable String

  KwikiHiDef kwikiStrong term=bold cterm=bold gui=bold
  KwikiHiDef kwikiEmphasize term=italic cterm=italic gui=italic
  KwikiHiDef kwikiUnderline term=underline cterm=underline gui=underline

  KwikiHiDef kwikiSE term=bold,italic cterm=bold,italic gui=bold,italic
  KwikiHiDef kwikiES term=bold,italic cterm=bold,italic gui=bold,italic
  KwikiHiDef kwikiSU term=bold,underline cterm=bold,underline gui=bold,underline
  KwikiHiDef kwikiUS term=bold,underline cterm=bold,underline gui=bold,underline
  KwikiHiDef kwikiUE term=underline,italic cterm=underline,italic gui=underline,italic
  KwikiHiDef kwikiEU term=underline,italic cterm=underline,italic gui=underline,italic

  KwikiHiDef kwikiSEU term=bold,underline,italic cterm=bold,underline,italic gui=bold,underline,italic
  KwikiHiDef kwikiSUE term=bold,underline,italic cterm=bold,underline,italic gui=bold,underline,italic
  KwikiHiDef kwikiESU term=bold,underline,italic cterm=bold,underline,italic gui=bold,underline,italic
  KwikiHiDef kwikiEUS term=bold,underline,italic cterm=bold,underline,italic gui=bold,underline,italic
  KwikiHiDef kwikiUSE term=bold,underline,italic cterm=bold,underline,italic gui=bold,underline,italic
  KwikiHiDef kwikiUES term=bold,underline,italic cterm=bold,underline,italic gui=bold,underline,italic
  
  KwikiHiLink kwikiInline Identifier
  KwikiHiLink kwikiDelete Error
  KwikiHiLink kwikiNone Normal

  KwikiHiLink kwikiForcedLink Underlined
  KwikiHiLink KwikiHyperLink Underlined
  KwikiHiLink KwikiNoHyperLink Normal
  KwikiHiLink KwikiTitledHyperLink KwikiHyperLink
  KwikiHiLink kwikiWikiLink Underlined
  KwikiHiLink KwikiNoWikiLink Normal
  KwikiHiLink KwikiTitledWikiLink KwikiWikiLink
  KwikiHiLink kwikiMailLink Underlined
  KwikiHiLink KwikiNoMailLink Normal
  KwikiHiLink KwikiTitledMailLink KwikiMailLink
  KwikiHiLink kwikiMDash Operator
  KwikiHiLink kwikiNDash KwikiMDash
  
endif

delcommand KwikiHiLink
delcommand KwikiHiDef
  
let b:current_syntax = "kwiki"

"EOF vim: tw=78:ft=vim:ts=8




