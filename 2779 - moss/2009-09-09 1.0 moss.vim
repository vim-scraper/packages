" ------------------------------------------------------------------
" Vim color file
" Name: moss (苔)
" Maintainer: Li Chunlin <yeiicn!gmail.com>
" Last Change: 2009-09-09
" Version: 1.0
" URL: http://vim.sourceforge.net/script.php?script_id=2779
" ------------------------------------------------------------------

" Init
" ------------------------------------------------------------------
set background=dark
highlight clear
if exists("syntax_on")
   syntax reset
endif
let g:colors_name = "moss"

" Highlighting groups for various occasions
" ------------------------------------------------------------------
hi SpecialKey   guifg=DarkSlateBlue
hi NonText      guifg=MidnightBlue guibg=#101E20
hi Directory    gui=BOLD guifg=LightSeaGreen
hi ErrorMsg     guifg=LightGoldenRod guibg=Firebrick
hi IncSearch    gui=BOLD guifg=Firebrick1
hi Search       gui=REVERSE guifg=NONE guibg=NONE
hi MoreMsg      gui=BOLD guifg=SeaGreen
hi ModeMsg      guifg=MediumAquamarine
hi LineNr       guifg=DarkSeaGreen guibg=#101E20
hi Question     guifg=Green
hi StatusLine   gui=BOLD guifg=LightSkyBlue2 guibg=RoyalBlue4
hi StatusLineNC gui=BOLD guifg=Honeydew4 guibg=Gray26
hi VertSplit    gui=BOLD guifg=Honeydew4 guibg=Gray26
hi Title        gui=BOLD guifg=DodgerBlue3
hi Visual       guifg=PowderBlue guibg=#202F3E
hi VisualNOS    gui=BOLD,UNDERLINE guifg=SlateGray
hi WarningMsg   guifg=Gold
hi WildMenu     gui=BOLD guifg=Black guibg=Chartreuse3
hi Folded       guifg=PaleGreen guibg=DarkSlateGray
hi FoldColumn   gui=BOLD guifg=PaleGreen guibg=DarkSlateGray
hi DiffAdd      guifg=SandyBrown guibg=DarkGreen
hi DiffChange   guibg=DarkOliveGreen
hi DiffDelete   guifg=Gray20 guibg=Black
hi DiffText     guifg=OrangeRed4 guibg=OliveDrab

" new Vim 7.0 items
if v:version >= 700
   hi CursorColumn guibg=#003010
   hi CursorLine   guibg=#003010
   hi SignColumn   guifg=Snow guibg=Turquoise4
   hi TabLine      guifg=CornflowerBlue guibg=Gray26
   hi TabLineSel   guifg=RoyalBlue guibg=#082220
   hi TabLineFill  gui=UNDERLINE guifg=CornflowerBlue guibg=Gray20
   hi Pmenu        guifg=White guibg=DimGray
   hi PmenuSel     guifg=Wheat guibg=MediumPurple4
   hi PmenuSbar    guifg=Tan   guibg=SeaShell4
   hi PmenuThumb   guifg=Tan   guibg=PeachPuff4
   hi MatchParen   gui=BOLD guifg=GoldenRod
endif

hi Cursor       guifg=Black guibg=LimeGreen
hi CursorIM     guifg=Black guibg=OrangeRed

" Syntax highlighting groups
" ------------------------------------------------------------------

hi Normal      gui=NONE guifg=LightSteelBlue guibg=#082220
hi Comment     gui=ITALIC guifg=BurlyWood4

hi Constant    guifg=CadetBlue
hi link        String    Constant
hi link        Character Constant
hi Number      guifg=SteelBlue
hi link        Boolean Number
hi link        Float   Number

hi Identifier  guifg=DeepSkyBlue4
hi Function    gui=BOLD guifg=DeepSkyBlue4

hi Statement   guifg=SpringGreen4
hi link        Conditional Statement
hi link        Repeat      Statement
hi link        Label       Statement
hi Operator    guifg=ForestGreen
hi link        Keyword     Statement
hi link        Exception   Statement

hi PreProc     guifg=SlateBlue
hi link        Include   PreProc
hi link        Define    PreProc
hi link        Macro     PreProc
hi link        PreCondit PreProc

hi Type        guifg=#3050A0
hi link        StorageClass Type
hi link        Structure    Type
hi link        Typedef      Type

hi Special     guifg=#609600
hi link        Specialchar Special
hi link        Tag         Special
hi link        Delimiter   Special
hi link        Debug       Special

hi Underlined  gui=UNDERLINE guifg=SkyBlue3
hi Ignore      guifg=Gray20
hi Error       guifg=Khaki3 guibg=VioletRed4
hi Todo        gui=BOLD guifg=GoldenRod3 guibg=NONE

