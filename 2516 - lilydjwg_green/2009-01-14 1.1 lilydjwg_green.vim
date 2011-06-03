" local syntax file - set colors on a per-machine basis:
" vim: tw=0 ts=4 sw=4
" Vim color file
" Version 1.1
" Maintainer:	lilydjwg <lilydjwg@gmail.com>
" Last Change:	2009 Jan 14

set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "lilydjwg_绿色"

hi Comment guifg=#78a321 guibg=#9bedd1
hi Constant guifg=#10a303 guibg=#9bedd1
hi Cursor guifg=#9b68d1 guibg=#00ccff
hi CursorColumn guibg=#666666
hi CursorIM gui=None
hi CursorLine guibg=#666666
hi DiffAdd guifg=#000000 guibg=#6a5acd
hi DiffChange guifg=#000000 guibg=#006400
hi DiffDelete gui=bold guifg=#000000 guibg=#ff7f50
hi DiffText gui=bold guifg=#000000 guibg=#6b8e23
hi Directory guifg=#00ffff
hi Error gui=underline guifg=#ff0000 guibg=#00008b
hi ErrorMsg guifg=#ffa500 guibg=#00008b
hi FoldColumn guifg=#b6c2ff guibg=#a4a4ff
hi Folded gui=undercurl guifg=#7450ff guibg=#9bc4d1
hi Identifier guifg=#d04cbe guibg=#9bedd1
hi Ignore guifg=#00008b
hi IncSearch gui=bold,reverse guifg=#33ff1c guibg=#3454ff
hi Label guifg=#ffff00
hi LineNr guifg=#ff64cb guibg=#9bedd1
hi MatchParen guifg=#3400ff guibg=#10d003
hi ModeMsg gui=bold guifg=#ffff3c guibg=#9bedd1
hi MoreMsg guifg=#ffff00
hi NonText gui=bold guifg=#ff38ff guibg=#60edd1
hi Normal guifg=#3454ff guibg=#9bedd1
hi Operator gui=bold guifg=#ffa500
hi Pmenu guibg=#ff00ff
hi PmenuSbar guibg=#bebebe
hi PmenuSel guibg=#a9a9a9
hi PmenuThumb gui=reverse
hi PreProc guifg=#9b20d1 guibg=#9bedd1
hi Question gui=bold guifg=#00ff00
hi Search guifg=#3404ff guibg=#ffff00
hi SignColumn guifg=#00ffff guibg=#bebebe
hi Special guifg=#ff00ff
hi SpecialKey guifg=#5cffff guibg=#8cedd1
hi SpellBad gui=undercurl
hi SpellCap gui=undercurl
hi SpellLocal gui=undercurl
hi SpellRare gui=undercurl
hi Statement guifg=#979797 guibg=#9bedd1
hi StatusLine guifg=#5f4d0e guibg=#a4a4ff
hi StatusLineNC guifg=#000000 guibg=#0000ff
hi TabLine guifg=#ffffc8 guibg=#a4a4ff
hi TabLineFill gui=reverse
hi TabLineSel gui=bold,underline guifg=#3488ff guibg=#9bedd1
hi Title guifg=#ffffff guibg=#9bedd1
hi Todo gui=bold guifg=#4c4c4c guibg=#ffff33
hi Type guifg=#ffa500
hi Underlined gui=underline guifg=#00afaf guibg=#aef0da
hi VertSplit gui=reverse
hi Visual guifg=#9bedd1 guibg=#34a8ff
hi VisualNOS gui=bold,underline
hi WarningMsg gui=bold guifg=#00ffff guibg=#00008b
hi WildMenu guifg=#000000 guibg=#ffff00
hi link Boolean Constant
hi link Character Constant
hi link Conditional Statement
hi link Debug Special
hi link Define PreProc
hi link Delimiter Special
hi link Exception Statement
hi link Float Constant
hi link Function Identifier
hi link Include PreProc
hi link Keyword Statement
hi link Macro PreProc
hi link Number Constant
hi link PreCondit PreProc
hi link Repeat Statement
hi link SpecialChar Special
hi link SpecialComment Special
hi link StorageClass Type
hi link String Constant
hi link Structure Type
hi link Tag Special
hi link Typedef Type
