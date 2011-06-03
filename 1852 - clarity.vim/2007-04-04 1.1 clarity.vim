" Vim color - Clarity
"  

set background=dark
hi clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name="clarity"

highlight Normal         gui=NONE guifg=#D5F4FF      guibg=#334C71 
highlight Comment        gui=NONE guifg=SteelBlue2   guibg=bg 
highlight PreProc        gui=NONE guifg=Salmon       guibg=bg 
highlight Precondit      gui=BOLD guifg=Khaki3       guibg=bg
highlight Identifier     gui=NONE guifg=Khaki3       guibg=bg 
highlight Type           gui=BOLD guifg=Orange       guibg=bg 
highlight StorageClass   gui=BOLD guifg=NavajoWhite2 guibg=bg
highlight Todo           gui=BOLD guifg=White        guibg=Magenta3 
highlight NonText        gui=NONE guifg=#334C51      guibg=SteelBlue4 
highlight LineNr         gui=NONE guifg=HoneyDew2    guibg=bg 
highlight StatusLineNC   gui=NONE guifg=Grey80       guibg=LightBlue4 
highlight StatusLine     gui=NONE guifg=Black        guibg=#FFFFCA 
highlight IncSearch      gui=NONE guifg=Black        guibg=#FFE568
highlight Search         gui=UNDERLINE,BOLD guifg=#FFE568 guibg=bg
highlight Cursor         gui=NONE guifg=Grey50       guibg=#FFE568
highlight CursorIM       gui=NONE guifg=Grey50       guibg=#FFE568
highlight Title          gui=BOLD guifg=OliveDrab3   guibg=bg
highlight WarningMsg     gui=BOLD guifg=White        guibg=IndianRed3
highlight String         gui=NONE guifg=LightGreen   guibg=bg      
highlight Number         gui=NONE guifg=OliveDrab2   guibg=bg
highlight Constant       gui=NONE guifg=NavajoWhite3 guibg=bg 
highlight Visual         gui=none guifg=Yellow       guibg=bg
highlight Directory      gui=none guifg=PeachPuff    guibg=bg
highlight DiffAdd        gui=NONE guifg=#7897B7      guibg=#334B64
highlight DiffChange     gui=NONE guifg=honeydew4    guibg=#153B64
highlight DiffDelete     gui=NONE guifg=grey40       guibg=grey20 
highlight DiffText       gui=NONE guifg=honeydew1    guibg=#8A5268 
highlight Typedef        gui=BOLD guifg=Cornsilk     guibg=bg
highlight Define         gui=BOLD guifg=White        guibg=bg
highlight Tag            gui=BOLD guifg=LightBlue2   guibg=bg
highlight Debug          gui=BOLD guifg=Green        guibg=bg
highlight Special        gui=NONE guifg=NavajoWhite  guibg=bg         
highlight SpecialChar    gui=NONE guifg=NavajoWhite  guibg=bg         
highlight Delimiter      gui=NONE guifg=NavajoWhite  guibg=bg         
highlight SpecialComment gui=NONE guifg=NavajoWhite3 guibg=bg         
highlight Conditional    gui=BOLD guifg=Pink         guibg=bg         
highlight Statement      gui=BOLD guifg=Pink2        guibg=bg 
highlight WildMenu       gui=NONE guifg=Black        guibg=Salmon



