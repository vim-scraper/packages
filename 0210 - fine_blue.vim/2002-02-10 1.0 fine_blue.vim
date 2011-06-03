" Vim color file
"  Maintainer: Tiza
" Last Change: 2002/02/10 Sun 23:32.
" GUI only

" This color scheme uses a light background.

set background=light
hi clear
if exists("syntax_on")
   syntax reset
endif

let colors_name = "fine_blue"

hi Normal       guifg=#303040 guibg=#f8f8f8

hi IncSearch    gui=UNDERLINE guifg=#000000 guibg=#40ffff
hi Search       gui=NONE guifg=#303040 guibg=#ffffa0

hi ErrorMsg     gui=BOLD guifg=#ffffff guibg=#ff4080
hi WarningMsg   gui=BOLD guifg=#ffffff guibg=#ff4080
hi ModeMsg      gui=NONE guifg=#0070ff guibg=NONE
hi MoreMsg      gui=NONE guifg=#a800ff guibg=NONE
hi Question     gui=NONE guifg=#008050 guibg=NONE

hi StatusLine   gui=BOLD guifg=#f8f8f8 guibg=#303040
hi StatusLineNC gui=BOLD guifg=#a0a0b0 guibg=#303040
hi VertSplit    gui=NONE guifg=#f8f8f8 guibg=#303040

hi Visual       gui=NONE guifg=#404060 guibg=#dddde8

hi DiffText     gui=NONE guifg=#7800ff guibg=#e0d8ff
hi DiffChange   gui=NONE guifg=#ff0080 guibg=#ffe0f0
hi DiffDelete   gui=BOLD guifg=#0000ff guibg=#ccccff
hi DiffAdd      gui=NONE guifg=#000060 guibg=#d8d8ff

hi Cursor       gui=NONE guifg=#0000ff guibg=#00e0ff
hi lCursor      gui=NONE guifg=#f8f8f8 guibg=#8000ff
hi CursorIM     gui=NONE guifg=#f8f8f8 guibg=#8000ff

hi Folded       gui=NONE guifg=#7800ff guibg=#e0d8ff
hi FoldColumn   gui=NONE guifg=#aa60ff guibg=#f0f0f4

hi Directory    gui=NONE guifg=#0000ff guibg=NONE
hi LineNr       gui=NONE guifg=#8080a0 guibg=NONE
hi NonText      gui=BOLD guifg=#4000ff guibg=#ececf0
hi SpecialKey   gui=NONE guifg=#009060 guibg=NONE
hi WildMenu     gui=BOLD guifg=#f8f8f8 guibg=#00aacc

" Groups for syntax highlighting
hi Comment      gui=NONE guifg=#ff00c0 guibg=NONE
hi Title        gui=NONE guifg=#004060 guibg=#c8f0f8
hi Constant     gui=NONE guifg=#2020ff guibg=#e8e8ff
hi Special      gui=NONE guifg=#005252 guibg=#c6f6e6
hi Identifier   gui=NONE guifg=#c800ff guibg=NONE
hi Statement    gui=NONE guifg=#008050 guibg=NONE
hi PreProc      gui=NONE guifg=#0070e6 guibg=NONE
hi Type         gui=NONE guifg=#7040ff guibg=NONE
hi Todo         gui=NONE guifg=#ff0070 guibg=#ffe0f4
hi Ignore       gui=NONE guifg=#f8f8f8 guibg=NONE
hi Error        gui=BOLD guifg=#ffffff guibg=#ff4080

" HTML
hi htmlLink                 gui=UNDERLINE
hi htmlBoldUnderline        gui=BOLD
hi htmlBoldItalic           gui=BOLD
hi htmlBold                 gui=BOLD
hi htmlBoldUnderlineItalic  gui=BOLD
hi htmlUnderlineItalic      gui=UNDERLINE
hi htmlUnderline            gui=UNDERLINE
hi htmlItalic               gui=italic

" Help
hi helpExample gui=NONE guifg=#0040ff guibg=NONE
