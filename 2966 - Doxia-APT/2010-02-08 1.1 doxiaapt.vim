" Vim syntax file
" Language:	Doxia APT file as used by Maven (http://maven.apache.org/doxia/references/apt-format.html)
" Maintainer:	Joris Kuipers (joriskuipers@xs4all.nl)
" Last Change:	7th February 2010

" =============================================================================

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn match  aptTitleStart    /\%^\s\+----*/    nextgroup=aptTitle skipwhite skipnl
syn match  aptTitle         /^\s\+.*[^-].*/   contained nextgroup=aptAuthorStart skipnl
syn match  aptAuthorStart   /^\s\+----*/      contained nextgroup=aptAuthor,aptDateStart skipwhite skipnl
syn match  aptAuthor        /^\s\+.*[^-].*/   contained nextgroup=aptDateStart skipnl
syn match  aptDateStart     /^\s\+----*/      contained nextgroup=aptDate skipwhite skipnl
syn match  aptDate          /^\s\+.*[^-].*/   contained nextgroup=aptDateStop skipnl 
syn match  aptDateStop      /^\s\+----*/      contained

" section title matches subsections as well, which start with one to four stars
syn match  aptSectionTitle  /^\(\*\{1,4}\s*\)\?[a-zA-Z0-9{].*/ contains=aptAnchor

syn region aptVerbatimText  matchgroup=aptVerbatim start="^[+-]---*+\?" end="^[+-]---*+\?"

syn region aptFigure        start="^\[" end="]" oneline nextgroup=aptFigureCaption
syn match  aptFigureCaption /.*/ contained

syn match  aptTableRowSep   /^\*--[*+:-]*/  nextgroup=aptTableCaption,aptTableCells skipnl
syn match  aptTableCells    /^\(|\|\s\).*/  contained contains=aptPipeSep nextgroup=aptTableCells,aptTableRowSep skipnl
syn match  aptPipeSep       /\\\@<!|/  contained
syn match  aptTableCaption  /^\w.*/    contained

syn region aptMacroContents matchgroup=aptMacro start="^%{" end="}" oneline contains=aptPipeSep,aptAssign,aptMacroUrl
syn match  aptAssign        /=/        contained
syn match  aptMacroUrl      /\(\(src\|url\|file\)=\)\@<=[^|}]*/ contained

syn match  aptHorRule       /^====*$/
syn match  aptComment       /\~\~.*$/  containedin=ALL contains=NONE

" not matching page breaks, since the C-L is already highlighted by VIM
syn match  aptLineBreak     /\\$/
syn match  aptNBSP          /\\ /
syn match  aptSpecialChar   /\\[~=+*\[\]<>{}\\-]/
syn match  aptChar          /\\\o\{1,3}/
syn match  aptChar          /\\x\x\x/
syn match  aptChar          /\\u\x\{4}/

syn region aptItalic        start="<"   end=">"   oneline
syn region aptBold          start="<<"  end=">>"  oneline
syn region aptMonospace     start="<<<" end=">>>" oneline

syn region aptAnchor        start="{"  end="}"  oneline
syn region aptLink          start="{{" end="}}" oneline
syn region aptAnchor        matchgroup=aptAnchor start="{{{\@=" end="}}" oneline contains=aptInnerLink
syn region aptInnerLink     contained start="{" end="}"

syn match  aptBulletList    /^\s\+\*/
syn region aptDefIndex      matchgroup=aptDefList start="\s\["   end="]"  oneline
syn region aptListIndex     matchgroup=aptNumList start="\s\[\[" end="]]" oneline


" Default highlighting
if version >= 508 || !exists("did_doxiaapt_syntax_inits")
  if version < 508
    let did_doxiaapt_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink aptTitleStart     PreProc
  HiLink aptAuthorStart    PreProc
  HiLink aptDateStart      PreProc
  HiLink aptDateStop       PreProc
  HiLink aptTitle          Statement
  HiLink aptAuthor         Identifier
  HiLink aptDate           Type

  HiLink aptSectionTitle   Statement

  HiLink aptVerbatim       PreProc
  HiLink aptVerbatimText   Constant

  HiLink aptFigure         Underlined
  HiLink aptFigureCaption  Identifier

  HiLink aptTableRowSep    PreProc
  HiLink aptTableCells     Constant
  HiLink aptPipeSep        PreProc
  HiLink aptTableCaption   Identifier

  HiLink aptMacro          Macro
  HiLink aptMacroContents  Constant
  HiLink aptMacroUrl       Underlined
  HiLink aptAssign         Operator

  HiLink aptHorRule        PreProc
  HiLink aptComment        Comment

  HiLink aptLineBreak      Special
  HiLink aptNBSP           Delimiter
  HiLink aptSpecialChar    SpecialChar
  HiLink aptChar           Character

  hi def aptItalic         term=italic cterm=italic gui=italic
  hi def aptBold           term=bold cterm=bold gui=bold
  HiLink aptMonospace      PreProc

  HiLink aptAnchor         Identifier
  HiLink aptLink           Underlined
  HiLink aptInnerLink      Underlined

  HiLink aptBulletList     Type
  HiLink aptDefList        Type
  HiLink aptNumList        Type
  HiLink aptDefIndex       Statement
  HiLink aptListIndex      Statement

  delcommand HiLink
endif

syn sync minlines=50

let b:current_syntax = "doxiaapt"
