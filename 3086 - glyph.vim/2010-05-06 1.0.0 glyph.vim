" Vim syntax file
" Language: Glyph
" Last Change: 2010-04-21
" Author: Fabio Cevasco <h3rald@h3rald.com>

if exists("b:current_syntax")
  finish
endif

"setlocal autoindent
syntax region glyphMacro matchgroup=glyphDelimiter start=/[^\[\]|\\ ]\+\[/ms=e end=/\]/ skip=/\\\[\|\\\]/ contains=glyphMacro,glyphQuotingMacro,glyphEscape
syntax match glyphDelimiter /\[\|\]/
syntax match glyphMacroName /[^\[\]|\\ ]\+\[[^=]/me=e-2
syntax match glyphMacroName /[^\[\]|\\ ]\+\[/me=e-1
syntax region glyphQuotingMacro matchgroup=glyphQuotingDelimiter start=/[^\[\]|\\ ]\+\[=/ end=/=\]/ skip=/\\\[=\|\\\=\]/ contains=glyphEscape,glyphParamSeparator
syntax match glyphParamSeparator /|/
syntax match glyphEscape /\\./

" Limitations
" - No highlight for quoting macro names

" Highlighting
highlight link glyphMacroName Function
highlight link glyphQuotingMacro String
highlight link glyphEscape Special
highlight link glyphDelimiter Delimiter
highlight link glyphParamSeparator Delimiter
highlight link glyphQuotingDelimiter Special

let b:current_syntax = "glyph"
