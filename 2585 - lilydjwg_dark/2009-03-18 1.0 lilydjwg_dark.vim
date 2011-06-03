" local syntax file - set colors on a per-machine basis:
" vim: tw=0 ts=4 sw=4
" Vim color file
" Version: 1.0
" Maintainer:	lilydjwg <lilydjwg@gmail.com>
" Last Change:	2009 Mar 11

set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "lilydjwg_dark"

hi Comment guifg=#686868
hi Constant guifg=#10a303
hi Cursor guifg=#FF66FF guibg=#00AAFF
hi CursorIM gui=None
hi CursorLine guibg=#555555
hi DiffAdd guifg=#000000 guibg=#6666CC
hi DiffChange guifg=#000000 guibg=darkgreen
hi DiffDelete gui=bold guifg=#000000 guibg=coral
hi DiffText gui=bold guifg=#000000 guibg=olivedrab
hi Directory guifg=#ff99ff
hi Error gui=underline guifg=red guibg=#111133
hi ErrorMsg guifg=#FFFF00 guibg=#0000FF
hi FoldColumn guifg=#0033FF guibg=#333333
hi Folded guifg=#9933FF guibg=#3D3D3D
hi Identifier guifg=#986CFF guibg=#2d222d
hi Ignore gui=None
hi IncSearch gui=bold,reverse guifg=#33ff1c guibg=#3454ff
hi LineNr guifg=#FF77DD
hi MatchParen guifg=#99FF99 guibg=#444444
hi ModeMsg gui=bold guifg=#AAAA3C guibg=#222211
hi MoreMsg guifg=#FFFF00
hi NonText guifg=#8400ff guibg=#1C1C1C
hi Normal guifg=#00CCFF guibg=#222222
hi Pmenu guifg=#3366FF guibg=#FF77FF
hi PmenuSbar guibg=#808080
hi PmenuSel guifg=#FF00FF guibg=#3F3F3F
hi PmenuThumb gui=reverse
hi PreProc guifg=#FF99FF guibg=#29222f
hi Question gui=bold guifg=#009966 guibg=#113322
hi Search guifg=#3404ff guibg=#FFFF00
hi SignColumn guifg=#00FFFF guibg=#C0C0C0
hi Special guifg=#FF00FF guibg=#2d222d
hi SpecialKey guifg=#00AEA0 guibg=#22302D
hi SpellBad gui=undercurl
hi SpellCap gui=undercurl
hi SpellLocal gui=undercurl
hi SpellRare gui=undercurl
hi Statement gui=bold guifg=#d86868 guibg=#2d2222
hi StatusLine gui=reverse guifg=#00c4ff guibg=#000000
hi StatusLineNC guifg=#A4A4FF guibg=#444400
hi TabLine guifg=#999944 guibg=#112233
hi TabLineFill gui=none
hi TabLineSel gui=underline guifg=#0066FF guibg=#001133
hi Title guifg=#ffff44 guibg=#2F2F2F
hi Todo gui=bold,underline guifg=#FF4444 guibg=#333300
hi Type guifg=#FFA500 guibg=#2d2211
hi Underlined gui=underline guifg=#0088c5 guibg=#222d3d
hi VertSplit gui=reverse guifg=#00C4FF guibg=#0000FF
hi Visual guibg=#3D3D3D
hi VisualNOS gui=None
hi WarningMsg guifg=#FFA500 guibg=#000080
hi WildMenu gui=None
hi link Boolean Constant
hi link Character Constant
hi link Conditional Statement
hi link CursorColumn CursorLine
hi link Debug Special
hi link Define PreProc
hi link Delimiter Special
hi link Exception Statement
hi link Float Constant
hi link Function Identifier
hi link Include PreProc
hi link Keyword Statement
hi link Label Statement
hi link Macro PreProc
hi link Number Constant
hi link Operator Statement
hi link PreCondit PreProc
hi link Repeat Statement
hi link SpecialChar Special
hi link SpecialComment Special
hi link StorageClass Type
hi link String Constant
hi link Structure Type
hi link Tag Special
hi link Typedef Type
