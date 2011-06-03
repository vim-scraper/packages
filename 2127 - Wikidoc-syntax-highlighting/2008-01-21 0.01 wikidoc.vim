" Vim syntax file
" Language:     WikiDoc format
" Author:       Cosimo Streppone <cosimo@cpan.org>
" Copied from:	Pod format by Scott Bigham <dsb@killerbunnies.org>
" Last Change:	2008 Jan 21

" To add embedded wikidoc documentation highlighting to your syntax file, add
" the commands:
"
"   syn include @Wikidoc <sfile>:p:h/wikidoc.vim
"   syn region myWikidoc start="^=begin wikidoc" start="^=" end="^=end wikidoc" keepend contained contains=@Wikidoc
"
" and add myWikidoc to the contains= list of some existing region, probably a
" comment.  The "keepend" flag is needed because "=end wikidoc" is matched as a
" pattern in its own right.


" Remove any old syntax stuff hanging around (this is suppressed
" automatically by ":syn include" if necessary).
" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" WikdDoc commands
syn match wikidocCommand	"^=.*=$"	nextgroup=wikidocCmdText contains=@NoSpell
syn match wikidocCommand	"^==.*==$"	nextgroup=wikidocCmdText contains=@NoSpell
syn match wikidocCommand	"^===.*===$"	nextgroup=wikidocCmdText contains=@NoSpell
syn match wikidocCommand	"^  \-"	    nextgroup=wikidocCmdText contains=@NoSpell
syn match wikidocCommand	"^  \*"	    nextgroup=wikidocCmdText contains=@NoSpell
"syn match wikidocCommand	"^=over"	nextgroup=wikidocOverIndent skipwhite contains=@NoSpell
"syn match wikidocCommand	"^=back"    contains=@NoSpell
syn match wikidocCommand	"^=end wikidoc" contains=@NoSpell
syn match wikidocCommand	"^=begin wikidoc" contains=@NoSpell
syn match wikidocCommand	"^=for"		nextgroup=wikidocForKeywd skipwhite contains=@NoSpell
syn match wikidocCommand	"^=begin"	nextgroup=wikidocForKeywd skipwhite contains=@NoSpell
syn match wikidocCommand	"^=end"		nextgroup=wikidocForKeywd skipwhite contains=@NoSpell

" Text of a header or list command
syn match wikidocCmdText	".*$" contained contains=wikidocFormat,@NoSpell

" Formatter identifier keyword for =for, =begin and =end commands
syn match wikidocForKeywd	"\S\+" contained contains=@NoSpell

" An indented line, to be displayed verbatim
syn match wikidocVerbatimLine	"^\s.*$" contains=@NoSpell

" Inline textual items handled specially by POD
syn match wikidocSpecial	"\(\<\|&\)\I\i*\(::\I\i*\)*([^)]*)" contains=@NoSpell
syn match wikidocSpecial	"[$@%]\I\i*\(::\I\i*\)*\>" contains=@NoSpell

" Special formatting sequences
"syn region wikidocFormat	start="[IBSCLFX]<[^<]"me=e-1 end=">" oneline contains=wikidocFormat,@NoSpell
syn region wikidocFormat	start="'''"me=e-1 end="'''" oneline contains=wikidocFormat,@NoSpell
syn region wikidocFormat	start="{{{"me=e-1 end="}}}" contains=wikidocFormat,@NoSpell
"syn match  wikidocFormat	"Z<>"
"syn match  wikidocFormat	"E<\(\d\+\|\I\i*\)>" contains=wikidocEscape,wikidocEscape2,@NoSpell
"syn match  wikidocEscape	"\I\i*>"me=e-1 contained contains=@NoSpell
"syn match  wikidocEscape2	"\d\+>"me=e-1 contained contains=@NoSpell

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_wikidoc_syntax_inits")
  if version < 508
    let did_wikidoc_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink wikidocCommand		Statement
  HiLink wikidocCmdText		String
"  HiLink wikidocOverIndent		Number
  HiLink wikidocForKeywd		Identifier
  HiLink wikidocFormat		Identifier
  HiLink wikidocVerbatimLine	PreProc
  HiLink wikidocSpecial		Identifier
"  HiLink wikidocEscape		String
"  HiLink wikidocEscape2		Number

  delcommand HiLink
endif

let b:current_syntax = "wikidoc"

" vim: ts=8
