" Vim syntax file
" Language: Glyph
" Last Change: 2011-02-20
" Author: Fabio Cevasco <h3rald@h3rald.com>
" Version: 1.5.0
" Limitations:
" - Attributes are not highlighted in Quoting Macros
" - Parameter/attribute placeholders are highlighted in all macros

if exists("b:current_syntax")
  finish
endif

syntax region glyphMacro start=/[^\[\]\|\\ ]\+\[/ end=/\]/ contains=glyphPlaceHolder,glyphDelimiter,glyphMacro,glyphQuotingMacro,glyphEscape,glyphParamSeparator,glyphComment
syntax match glyphDelimiter /[^\[\]\|\\ ]\+\[/ contains=glyphMacroName contained
syntax match glyphMacroName /[^\[\]\|\\ ]\+/ containedin=glyphDelimiter contains=glyphMacroSeparator contained 
syntax match glyphDelimiter /\]/ contained

syntax region glyphQuotingMacro start=/[^\[\]\|\\ ]\+\[=/ end=/=\]/me=s-1 contains=glyphQuotingDelimiter,glyphEscape,glyphParamSeparator nextgroup=glyphQuotingDelimiter
syntax match glyphQuotingDelimiter /[^\[\]\|\\ ]\+\[=/ contains=glyphQuotingMacroName contained
syntax match glyphQuotingMacroName /[^\[\]\|\\= ]\+/ containedin=glyphQuotingDelimiter contained 
syntax match glyphQuotingDelimiter /=\]/ containedin=glyphQuotingMacro contained

syntax match glyphCoreMacroName /\s*\(snippet\|snippet:\|macro:\|include\|ruby\|config\|config:\|escape\|condition\|eq\|not\|and\|or\|lt\|lte\|gt\|gte\|alias:\|define:\|output?\|let\|attribute:\|attribute\|add\|subtract\|multiply\|while\|quote\|apply\|reverse\|length\|get\|sort\|select\|fragment\|embed\|load\|&\|&:\|%\|%:\|\$\|\$:\|\.\|'\|\~\|map\|filter\|def:\|attr\|attr:\|@:\|@\|?\|##\|<=\)\[/me=e-1 containedin=glyphDelimiter contained
syntax match glyphCoreMacroName /\s*\(snippet\|snippet:\|macro:\|include\|ruby\|config\|config:\|escape\|condition\|eq\|not\|and\|or\|lt\|lte\|gt\|gte\|alias:\|define:\|output?\|let\|attribute:\|attribute\|add\|subtract\|multiply\|while\|quote\|apply\|reverse\|length\|get\|sort\|select\|fragment\|embed\|load\|&\|&:\|%\|%:\|\$\|\$:\|\.\|'\|\~\|map\|filter\|def:\|attr\|attr:\|@:\|@\|?\|##\|<=\)\[=/me=e-2 containedin=glyphQuotingDelimiter contained
syntax match glyphAttributeName /\s*@[^\[\]\|\\:\/ ]\+\[/me=e-1 containedin=glyphDelimiter contained 
syntax match glyphAttributeName /\s*@[^\[\]\|\\:\/ ]\+\[=/me=e-2 containedin=glyphQuotingDelimiter contained 

syntax match glyphPlaceholder /{{\(\d\+\|[^\[\]\|\\ ]\+\)}}/ contained
syntax match glyphParamSeparator /|/
syntax match glyphMacroSeparator /\// contained
syntax match glyphEscape /\\./
syntax region glyphComment matchgroup=glyphCommentDelimiter start=/--\[/ end=/\]/ contains=glyphMacro,glyphQuotingMacro,glyphCommentedMacro
syntax region glyphCommentedMacro matchgroup=glyphCommentDelimiter start=/[^\[\]\|\\ ]\+\[/ end=/\]/ contains=glyphCommentedMacro contained

" Highlighting
highlight link glyphMacroName Function
highlight link glyphQuotingMacroName Function
highlight link glyphCoreMacroName Statement
highlight link glyphAttributeName Constant

highlight link glyphDelimiter Delimiter
highlight link glyphParamSeparator Delimiter
highlight link glyphMacroSeparator Delimiter
highlight link glyphEscape Special
highlight link glyphPlaceholder Special

highlight link glyphQuotingMacro String
highlight link glyphQuotingDelimiter Special

highlight link glyphComment Comment
highlight link glyphCommentDelimiter Comment
highlight link glyphCommentedMacro Comment

let b:current_syntax = "glyph"
