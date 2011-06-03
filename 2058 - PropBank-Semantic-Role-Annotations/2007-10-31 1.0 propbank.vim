" Vim syntax file
" Language:   PropBank Semantic Role Annotations
" Maintainer: Zhaojun WU < wuzhaojun@gmail.com>
" Updated:    Wed 31 Oct 2007 09:40:53 PM HKT 	
" Remark:     Syntax highlighting for PropBank Semantic Role Annotations
"               [http://www.cs.rochester.edu/~gildea/PropBank/Sort/]
"             the highlighting patterns are based on the css file:
"               [http://www.cs.rochester.edu/~gildea/pb_style.css]
"
syn clear
syn match coreArg0              "ARG0[^\]]*"
syn match coreArg1              "ARG1[^\]]*"
syn match coreArg2              "ARG2[^\]]*"
syn match coreArg3              "ARG3[^\]]*"
syn match coreArg4              "ARG4[^\]]*"
syn match coreArg5              "ARG5[^\]]*"
syn match adjArgm               "ARGM[^\]]*"
syn match targetPred            "TARGET [^\]]*"
syn match relPred               "rel [^\]]*"
syn match sentenceNumber        "^[0-9]*"

hi coreArg0       guifg=white guibg=blue      ctermfg=white ctermbg=blue   
hi coreArg1       guifg=white guibg=darkgreen ctermfg=white ctermbg=green
hi coreArg2       guifg=black guibg=yellow    ctermfg=black ctermbg=yellow
hi coreArg3       guifg=black guibg=orange    ctermfg=black ctermbg=cyan
hi coreArg4       guifg=white guibg=blue      ctermfg=white ctermbg=blue
hi coreArg5       guifg=white guibg=blue      ctermfg=white ctermbg=blue
hi adjArgm        guifg=white guibg=brown     ctermfg=white ctermbg=brown
hi targetPred     guifg=white guibg=red       ctermfg=white ctermbg=red
hi relPred        guifg=white guibg=red       ctermfg=white ctermbg=red
hi sentenceNumber guifg=red   guibg=black     ctermfg=red   ctermbg=black
