" Terse Verses Of Utter Control
" A Vim Color Scheme
" Maintainer: Jevgeni Tarasov <jstarasov@gmail.com>
" Version: 1

"-- The Actual Color Scheme ---------------------------------------------------
if &background=='light'
      " Recycled Paper

      hi Normal         gui=NONE guifg=#00011f guibg=#ecebe7
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi Statement      gui=bold guifg=#00011f guibg=NONE
      \                 cterm=bold ctermfg=black ctermbg=white

      hi Comment        gui=italic guifg=#38362b guibg=NONE
      \                 cterm=italic ctermfg=black ctermbg=white

      hi Type           gui=NONE guifg=NONE guibg=NONE
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi Special        gui=NONE guifg=NONE guibg=NONE   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi Identifier     gui=underline guifg=NONE guibg=NONE   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi PreProc        gui=bold,italic guifg=NONE guibg=NONE   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi Constant       gui=italic guifg=NONE guibg=NONE   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi String         gui=italic guifg=NONE guibg=#e2e1dd   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi StatusLine     gui=italic guifg=#00011f guibg=#abadac 
      \                 cterm=NONE ctermfg=white ctermbg=grey

      hi Error          gui=NONE guifg=white guibg=#c80f3f   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi Todo           gui=bold,italic guifg=#e4115b guibg=#e2e1dd     
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi Underlined     gui=underline guifg=NONE guibg=NONE   
      \                 cterm=underline ctermfg=black ctermbg=white


      " Non-Text Elements
      hi NonText        gui=NONE guifg=#58595b guibg=#e2e1dd     
      \                 cterm=NONE ctermfg=grey ctermbg=black

      hi Search         gui=NONE guifg=NONE guibg=#fbf285   
      \                 cterm=NONE ctermfg=black ctermbg=yellow
      hi IncSearch      gui=NONE guifg=NONE guibg=#fbf285   
      \                 cterm=NONE ctermfg=black ctermbg=yellow

      hi Directory      gui=bold guifg=NONE guibg=NONE   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi MoreMsg        gui=italic guifg=NONE guibg=#e2e1dd       
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi LineNr         gui=italic guifg=#58595b guibg=#e2e1dd     
      \                 cterm=NONE ctermfg=grey ctermbg=black

      hi VertSplit      gui=NONE guifg=#58595b guibg=#e2e1dd     
      \                 cterm=NONE ctermfg=grey ctermbg=black

      hi Question       gui=NONE guifg=NONE guibg=NONE   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi Title          gui=bold,italic guifg=NONE guibg=NONE   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi WarningMsg     gui=NONE guifg=white guibg=#c80f3f   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi Folded         gui=NONE guifg=NONE guibg=#e2e1dd      
      \                 cterm=NONE ctermfg=black ctermbg=white
      hi FoldColumn     gui=NONE guifg=NONE guibg=#e2e1dd      
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi DiffAdd        gui=NONE guifg=NONE guibg=#acd58e   
      \                 cterm=NONE ctermfg=black ctermbg=white
      hi DiffChange     gui=NONE guifg=#00254f guibg=#93d2f3   
      \                 cterm=NONE ctermfg=black ctermbg=white
      hi DiffDelete     gui=NONE guifg=#820056 guibg=#edb2d1   
      \                 cterm=NONE ctermfg=black ctermbg=white
      hi DiffText       gui=italic guifg=NONE guibg=white   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi Cursor         gui=NONE guifg=white guibg=black
      \                 cterm=NONE ctermfg=white ctermbg=black

      hi Visual         gui=NONE guifg=NONE guibg=#efbf285   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi MatchParen     gui=bold guifg=#c80f3f guibg=NONE
      \                 cterm=NONE ctermfg=white ctermbg=black

elseif &background=='dark'
      " Vintage Blueprint

      hi Normal         gui=NONE guifg=#ecebe7 guibg=#006db6
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi Statement      gui=bold guifg=#ecebe7 guibg=NONE
      \                 cterm=bold ctermfg=black ctermbg=white

      hi Comment        gui=italic guifg=#a4d7f4 guibg=NONE
      \                 cterm=italic ctermfg=black ctermbg=white

      hi Type           gui=NONE guifg=NONE guibg=NONE
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi Special        gui=NONE guifg=NONE guibg=NONE   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi Identifier     gui=underline guifg=NONE guibg=NONE   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi PreProc        gui=bold,italic guifg=NONE guibg=NONE   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi Constant       gui=italic guifg=NONE guibg=NONE   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi String         gui=italic guifg=black guibg=#a4d7f4   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi StatusLine     gui=italic guifg=#00011f guibg=#abadac 
      \                 cterm=NONE ctermfg=white ctermbg=grey

      hi Error          gui=NONE guifg=white guibg=#c80f3f   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi Todo           gui=bold,italic guifg=#e4115b guibg=#e2e1dd     
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi Underlined     gui=underline guifg=NONE guibg=NONE   
      \                 cterm=underline ctermfg=black ctermbg=white


      " Non-Text Elements
      hi NonText        gui=NONE guifg=#58595b guibg=#e2e1dd     
      \                 cterm=NONE ctermfg=grey ctermbg=black

      hi Search         gui=NONE guifg=NONE guibg=#fbf285   
      \                 cterm=NONE ctermfg=black ctermbg=yellow
      hi IncSearch      gui=NONE guifg=NONE guibg=#fbf285   
      \                 cterm=NONE ctermfg=black ctermbg=yellow

      hi Directory      gui=bold guifg=NONE guibg=NONE   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi MoreMsg        gui=italic guifg=NONE guibg=#e2e1dd       
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi LineNr         gui=italic guifg=#58595b guibg=#e2e1dd     
      \                 cterm=NONE ctermfg=grey ctermbg=black

      hi VertSplit      gui=NONE guifg=#58595b guibg=#e2e1dd     
      \                 cterm=NONE ctermfg=grey ctermbg=black

      hi Question       gui=NONE guifg=NONE guibg=NONE   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi Title          gui=bold,italic guifg=NONE guibg=NONE   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi WarningMsg     gui=NONE guifg=white guibg=#c80f3f   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi Folded         gui=NONE guifg=NONE guibg=#e2e1dd      
      \                 cterm=NONE ctermfg=black ctermbg=white
      hi FoldColumn     gui=NONE guifg=NONE guibg=#e2e1dd      
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi DiffAdd        gui=NONE guifg=NONE guibg=#acd58e   
      \                 cterm=NONE ctermfg=black ctermbg=white
      hi DiffChange     gui=NONE guifg=#00254f guibg=#93d2f3   
      \                 cterm=NONE ctermfg=black ctermbg=white
      hi DiffDelete     gui=NONE guifg=#820056 guibg=#edb2d1   
      \                 cterm=NONE ctermfg=black ctermbg=white
      hi DiffText       gui=italic guifg=NONE guibg=white   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi Cursor         gui=NONE guifg=white guibg=black
      \                 cterm=NONE ctermfg=white ctermbg=black

      hi Visual         gui=NONE guifg=NONE guibg=#efbf285   
      \                 cterm=NONE ctermfg=black ctermbg=white

      hi MatchParen     gui=bold guifg=#c80f3f guibg=NONE
      \                 cterm=NONE ctermfg=white ctermbg=black
endif
