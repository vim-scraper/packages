" Vim color file
" Maintainer:	Adam Blinkinsop <blinks@acm.org>
" Last Change:	2006 Sept 29

" Remove all existing highlighting.
set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif

let colors_name = "blink"

" Set Default
hi Normal term=NONE cterm=NONE ctermfg=Gray ctermbg=Black gui=NONE guifg=Gray guibg=Black

" Vim colors (reverse for highlighting, red for errors, brown for special)
hi Cursor term=reverse cterm=reverse gui=reverse
hi CursorIME term=reverse cterm=reverse gui=reverse
hi CursorColumn term=reverse cterm=reverse gui=reverse
hi CursorLine term=reverse cterm=reverse gui=reverse
hi ErrorMsg term=reverse ctermfg=Red ctermbg=Black guifg=Red guibg=Black
hi ModeMsg term=reverse cterm=reverse gui=reverse
hi NonText term=bold ctermfg=DarkBlue guifg=DarkBlue
hi SpecialKey term=NONE ctermfg=Brown guifg=Brown
hi StatusLine term=reverse cterm=reverse gui=reverse
hi Visual term=reverse cterm=reverse gui=reverse
hi WarningMsg term=reverse ctermfg=Brown guifg=Brown

" Syntax Colors, General
hi Comment term=reverse ctermfg=Black ctermbg=DarkCyan guifg=Black guibg=DarkCyan
hi Underlined term=underline cterm=underline gui=underline
hi Ignore term=NONE cterm=NONE gui=NONE
hi Error term=bold ctermfg=Red ctermbg=Black guifg=Red guibg=Black
hi Todo term=reverse cterm=reverse gui=reverse

" Constants (plain data)
hi Constant term=bold cterm=bold ctermfg=Brown ctermbg=Black gui=bold guifg=Brown guibg=Black
"hi Boolean term=NONE cterm=NONE gui=NONE
"hi Character term=NONE cterm=NONE gui=NONE
"hi String term=NONE cterm=NONE gui=NONE
"hi Number term=NONE cterm=NONE gui=NONE
"hi Float term=NONE cterm=NONE gui=NONE

" Identifiers (are things)
hi Identifier term=NONE cterm=NONE ctermfg=DarkCyan ctermbg=Black gui=NONE guifg=DarkCyan ctermbg=Black
"hi Function term=bold cterm=bold gui=bold

" Statements (do things)
hi Statement term=bold ctermfg=DarkGreen ctermbg=Black guifg=DarkGreen ctermbg=Black
"hi Conditional term=NONE cterm=NONE gui=NONE
"hi Repeat term=NONE cterm=NONE gui=NONE
"hi Label term=NONE cterm=NONE gui=NONE
"hi Operator term=NONE cterm=NONE gui=NONE
"hi Keyword term=bold cterm=bold gui=bold
"hi Exception term=NONE cterm=NONE gui=NONE

" Preprocessing (meta)
hi PreProc term=bold cterm=NONE ctermfg=DarkGreen ctermbg=Black gui=NONE guifg=DarkGreen guibg=Black
"hi Include term=NONE cterm=NONE gui=NONE
"hi Define term=NONE cterm=NONE gui=NONE
"hi Macro term=NONE cterm=NONE gui=NONE
"hi PreCondit term=NONE cterm=NONE gui=NONE

" Types (define things)
hi Type term=NONE cterm=bold ctermfg=DarkCyan ctermbg=Black gui=Bold guifg=DarkCyan guibg=Black
"hi StorageClass term=NONE cterm=NONE gui=NONE
"hi Structure term=NONE cterm=NONE gui=NONE
"hi Typedef term=NONE cterm=NONE gui=NONE

" Special (undef)
hi Special term=NONE cterm=NONE ctermfg=Brown ctermbg=Black gui=NONE guifg=Brown guibg=Black
"hi Tag term=NONE cterm=NONE gui=NONE
"hi SpecialChar term=NONE cterm=NONE gui=NONE
"hi Delimiter term=NONE cterm=NONE gui=NONE
"hi SpecialComment term=NONE cterm=NONE gui=NONE
"hi Debug term=NONE cterm=NONE gui=NONE

" vim: sw=2
