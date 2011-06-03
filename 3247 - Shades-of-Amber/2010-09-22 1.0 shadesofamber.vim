" Vim color file
" Maintainer: Tim Brown <tim@timb.net>
" Last Change: 2010-09-22
" Version: 1.0
" URI: 


""" Init
set background=dark
highlight clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name = "shadesofamber"


"------- COLORS --------
"-- GUI Colors
highlight BadStyle      gui=undercurl guifg=#f00    guibg=#300

highlight Cursor        gui=None   guifg=black guibg=#fe0
highlight CursorIM      gui=italic guifg=black guibg=#fe0
highlight Directory                guifg=green guibg=bg
highlight DiffAdd       gui=None   guifg=green   guibg=#020
highlight DiffChange    gui=none  guifg=#ff0 guibg=#420
highlight DiffDelete    gui=None      guifg=#400 guibg=#200
highlight DiffText      gui=undercurl guifg=#ff0    guibg=#630
highlight ErrorMsg      gui=None   guifg=#ff0  guibg=#f40
highlight VertSplit     gui=None   guifg=#120 guibg=#fc0
highlight Folded        gui=Bold   guibg=#340 guifg=#c90
highlight FoldColumn    gui=None   guibg=#120 guifg=#fc0
highlight IncSearch     gui=undercurl guibg=#306 guifg=fg
highlight LineNr        gui=None   guibg=#340 guifg=#c90
highlight ModeMsg       gui=none   guibg=#040  guifg=#0f0
highlight MoreMsg       gui=italic guifg=SeaGreen4 guibg=bg
highlight NonText       guibg=#120 guifg=#fc0
highlight Normal        gui=None   guibg=#000 guifg=#fe0
highlight Question      gui=italic guifg=SeaGreen2 guibg=bg
highlight Search        gui=bold   guibg=#408 guifg=fg
highlight SpecialKey    gui=bold   guibg=#040 guifg=#ff0
highlight StatusLine    gui=bold   guibg=#fc0 guifg=#008
highlight StatusLineNC  gui=none   guibg=#fc0 guifg=#330
highlight Title         gui=bold,italic  guifg=black guibg=#ff0
highlight Visual        gui=reverse guibg=#ff0 guifg=#080
highlight VisualNOS     gui=italic,underline guifg=fg guibg=bg
highlight WarningMsg    gui=bold   guifg=#f00 guibg=bg
highlight WildMenu      gui=bold   guibg=#088 guifg=#ff0
highlight ColorColumn   gui=bold,italic guibg=#440 guifg=#f00

"-- Syntax Colors
"highlight Comment       gui=reverse guifg=#507080
highlight Comment       gui=None guifg=#fa0 guibg=#210

highlight Constant      gui=bold guifg=white guibg=bg
highlight String      gui=none   guifg=#cf0 guibg=bg
"highlight Character gui=None guifg=Cyan guibg=bg
highlight Number      gui=None guifg=#cf0 guibg=bg
highlight Boolean     gui=none guifg=#f80 guibg=bg
"highlight Float gui=None guifg=Cyan guibg=bg

highlight Identifier  guifg=#dc0
highlight Function    gui=None guifg=#ff8 guibg=bg

highlight Statement   gui=bold   guifg=#f80
highlight Conditional gui=Bold   guifg=#f80 guibg=bg
highlight Repeat      gui=Bold   guifg=#f80 guibg=bg
highlight Label       gui=Bold   guifg=#f80 guibg=bg
highlight Operator    gui=Bold   guifg=#f80 guibg=bg
highlight Keyword     gui=Bold   guifg=#f80 guibg=bg
highlight Exception   gui=Bold   guifg=#f80 guibg=bg

highlight PreProc       guifg=#ff8
"highlight Include gui=None guifg=MediumLightSkyBlue1 guibg=bg
"highlight Define gui=None guifg=MediumLightSkyBlue1g guibg=bg
"highlight Macro gui=None guifg=MediumLightSkyBlue1g guibg=bg
"highlight PreCondit gui=None guifg=MediumSlateBlue guibg=bg

highlight Type          gui=bold guifg=#f80
"highlight StorageClass gui=None guifg=LightBlue guibg=bg
"highlight Structure gui=None guifg=LightBlue guibg=bg
"highlight Typedef gui=None guifg=LightBlue guibg=bg

highlight Special       gui=none guifg=#f80
"highlight SpecialChar gui=italic guifg=White guibg=bg
"highlight Tag gui=italic guifg=White guibg=bg
"highlight Delimiter gui=italic guifg=White guibg=bg
"highlight SpecialComment gui=italic guifg=White guibg=bg
"highlight Debug gui=italic guifg=White guibg=bg

highlight Underlined gui=underline guifg=fg guibg=bg

highlight Ignore     guifg=#aa0

highlight Error      gui=italic guifg=#ff0  guibg=#f00
highlight Todo       gui=bold guifg=yellow guibg=#a70

"-- OLD COLORS
"highlight prologAtom guifg=#dddd00 gui=italic
highlight prologVariable     gui=bold,italic guifg=#0f0
highlight prologFreeVariable gui=italic      guifg=#0f0
highlight xpceVariable       gui=bold        guifg=#ff0 guibg=#666
highlight xpceKeyword        gui=bold        guifg=#f80 guibg=#666

highlight MatchParen         gui=bold        guibg=#480 guifg=#cf0

highlight Pmenu         gui=bold        guifg=#fc0 guibg=#040
highlight PmenuSel      gui=bold        guibg=#fc0 guifg=#040
highlight PmenuSbar     gui=bold        guibg=#840 guifg=#040
highlight PmenuThumb    gui=bold        guibg=#ff0 guifg=#040
