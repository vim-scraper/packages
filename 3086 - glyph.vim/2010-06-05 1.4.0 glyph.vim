" Vim syntax file
" Language: Glyph
" Last Change: 2010-06-05
" Author: Fabio Cevasco <h3rald@h3rald.com>
" Version: 1.4.0
" Limitations:
" - Only attribute name is highlighter in Quoting Macros
" - Parameter/attribute placeholders are highlighted in all macros

if exists("b:current_syntax")
  finish
endif

syntax region glyphMacro start=/[^\[\]\|\\ ]\+\[/ end=/\]/ contains=glyphPlaceHolder,glyphDelimiter,glyphMacro,glyphQuotingMacro,glyphEscape,glyphParamSeparator,glyphComment
syntax match glyphDelimiter /[^\[\]\|\\ ]\+\[/ contains=glyphMacroName contained
syntax match glyphMacroName /[^\[\]\|\\ ]\+/ containedin=glyphDelimiter contained 
syntax match glyphDelimiter /\]/ contained

syntax region glyphQuotingMacro start=/[^\[\]\|\\ ]\+\[=/ end=/=\]/me=s-1 contains=glyphQuotingDelimiter,glyphEscape,glyphParamSeparator,glyphComment nextgroup=glyphQuotingDelimiter
syntax match glyphQuotingDelimiter /[^\[\]\|\\ ]\+\[=/ contains=glyphQuotingMacroName contained
syntax match glyphQuotingMacroName /[^\[\]\|\\= ]\+/ containedin=glyphQuotingDelimiter contained 
syntax match glyphQuotingDelimiter /=\]/ containedin=glyphQuotingMacro contained

syntax match glyphCoreMacroName /\s*\(snippet\|snippet:\|macro:\|include\|ruby\|config\|config:\|escape\|rewrite:\|rw:\|condition\|eq\|not\|and\|or\|match\|&\|&:\|%\|%:\|\$\|\$:\|\.\|?\)/ containedin=glyphDelimiter,glyphQuotingDelimiter contained
syntax match glyphAttributeName /\s*@[^\[\]\|\\ ]\+\[/me=e-1 containedin=glyphDelimiter,glyphQuotingMacro contained 

syntax match glyphPlaceholder /{{\(\d\+\|[^\[\]\|\\ ]\+\)}}/ contained
syntax match glyphParamSeparator /|/
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
highlight link glyphEscape Special
highlight link glyphPlaceholder Special

highlight link glyphQuotingMacro String
highlight link glyphQuotingDelimiter Special

highlight link glyphComment Comment
highlight link glyphCommentDelimiter Comment
highlight link glyphCommentedMacro Comment

let b:current_syntax = "glyph"
