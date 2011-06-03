" Vim color file
" Maintainer:   tiza
" Last Change:  2002/02/22 Fri 18:56.
" version: 1.1
" GUI only

" This color scheme uses a dark background.

set background=dark
hi clear
if exists("syntax_on")
   syntax reset
endif

let colors_name = "night"

hi Normal       guifg=#ffffff guibg=#303040

hi IncSearch    gui=UNDERLINE,BOLD guifg=#ffffff guibg=#d000d0
hi Search       gui=BOLD guifg=#ffd0ff guibg=#c000c0

hi ErrorMsg     gui=BOLD guifg=#ffffff guibg=#ff0088
hi WarningMsg   gui=BOLD guifg=#ffffff guibg=#ff0088
hi ModeMsg      gui=BOLD guifg=#00e0ff guibg=NONE
hi MoreMsg      gui=BOLD guifg=#00ffdd guibg=NONE
hi Question     gui=BOLD guifg=#ff90ff guibg=NONE

hi StatusLine   gui=BOLD guifg=#ffffff guibg=#7700ff
hi StatusLineNC gui=BOLD guifg=#c0b0ff guibg=#7700ff
hi VertSplit    gui=NONE guifg=#ffffff guibg=#7700ff
hi WildMenu     gui=BOLD guifg=#ffffff guibg=#d08020

hi Visual       gui=NONE guifg=#ffffff guibg=#7070c0

hi DiffText     gui=NONE guifg=#ffffff guibg=#40a060
hi DiffChange   gui=NONE guifg=#ffffff guibg=#007070
hi DiffDelete   gui=BOLD guifg=#ffffff guibg=#40a0c0
hi DiffAdd      gui=NONE guifg=#ffffff guibg=#40a0c0

hi Cursor       gui=NONE guifg=#ffffff guibg=#ff9020
hi lCursor      gui=NONE guifg=#ffffff guibg=#ff00d0
hi CursorIM     gui=NONE guifg=#ffffff guibg=#ff00d0

hi Folded       gui=BOLD guifg=#e8e8f0 guibg=#606078
hi FoldColumn   gui=NONE guifg=#a0a0b0 guibg=#404050

hi Directory    gui=NONE guifg=#00ffff guibg=NONE
hi Title        gui=BOLD guifg=#ffffff guibg=#8000d0
hi LineNr       gui=NONE guifg=#808098 guibg=NONE
hi NonText      gui=BOLD guifg=#8040ff guibg=#383848
hi SpecialKey   gui=BOLD guifg=#60a0ff guibg=NONE

" Groups for syntax highlighting
hi Comment      gui=BOLD guifg=#ff60ff guibg=NONE
hi Constant     gui=NONE guifg=#ffffff guibg=#4822bb
hi Special      gui=NONE guifg=#44ffff guibg=#4822bb
hi Identifier   gui=NONE guifg=#90d0ff guibg=NONE
hi Statement    gui=BOLD guifg=#00ccbb guibg=NONE
hi PreProc      gui=NONE guifg=#40ffa0 guibg=NONE
hi Type         gui=BOLD guifg=#bb99ff guibg=NONE
hi Todo         gui=BOLD guifg=#ffffff guibg=#ff0088
hi Ignore       gui=NONE guifg=#303040 guibg=NONE
hi Error        gui=BOLD guifg=#ffffff guibg=#ff0088

" HTML
hi htmlLink                 gui=UNDERLINE
hi htmlBoldUnderline        gui=BOLD
hi htmlBoldItalic           gui=BOLD
hi htmlBold                 gui=BOLD
hi htmlBoldUnderlineItalic  gui=BOLD
hi htmlUnderlineItalic      gui=UNDERLINE
hi htmlUnderline            gui=UNDERLINE
hi htmlItalic               gui=italic
