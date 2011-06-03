" Vim syntax file
" Language: Glyph
" Last Change: 2010-05-30
" Author: Fabio Cevasco <h3rald@h3rald.com>

if exists("b:current_syntax")
  finish
endif

"setlocal autoindent
syntax region glyphMacro start=/[^\[\]|\\ ]\+\[/ms=e end=/\]/ skip=/\\\[\|\\\]/ contains=glyphMacro,glyphQuotingMacro,glyphEscape transparent
syntax match glyphDelimiter /\[\|\]/
syntax match glyphMacroName /[^\[\]|\\ ]\+\[/me=e-1
syntax match glyphAttributeName /\s*@[^\[\]|\\ ]\+\[/me=e-1
syntax match glyphCoreMacroName /\s*\(snippet\|snippet:\|macro:\|include\|ruby\|config\|config:\|escape\|condition\|eq\|not\|and\|or\|match\|&\|&:\|%\|%:\|$\|$:\|\.\|?\)\[/me=e-1

syntax region glyphQuotingMacro matchgroup=glyphQuotingDelimiter start=/[^\[\]|\\ ]\+\[=/ end=/=\]/ skip=/\\\[=\|\\\=\]/ contains=glyphEscape,glyphParamSeparator 

syntax match glyphParamSeparator /|/
syntax match glyphEscape /\\./
syntax region glyphComment matchgroup=glyphCommentDelimiter start=/\s*\(--\|comment\)\[/ end="\]\s*" contains=glyphMacro,glyphQuotingMacro

" Limitations
" - No highlight for quoting macro names
" - No highlighting for attributes within quoting macros  
" - Highlighted quoting macros within comments

" Highlighting
highlight link glyphMacroName Function
highlight link glyphComment Comment
highlight link glyphAttributeName Constant
highlight link glyphCoreMacroName Statement
highlight link glyphQuotingMacro String
highlight link glyphEscape Special
highlight link glyphDelimiter Delimiter
highlight link glyphCommentDelimiter Comment
highlight link glyphParamSeparator Delimiter
highlight link glyphQuotingDelimiter Special

let b:current_syntax = "glyph"
