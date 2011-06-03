" Vim color file
"  Maintainer: Otavio Fernandes
" Last Change: 2007/09/10 Mon 13:55
"     Version: 1.0.0

set background=dark
hi clear
if exists("syntax_on")
   syntax reset
endif

let colors_name = "no_quarter"

hi Normal guifg=grey80 guibg=#343434
hi IncSearch gui=UNDERLINE guifg=#80ffff guibg=#0060c0
hi Search gui=NONE guifg=bg guibg=grey60
hi ErrorMsg gui=BOLD guifg=#ffa0ff guibg=NONE
hi WarningMsg gui=BOLD guifg=#ffa0ff guibg=NONE
hi ModeMsg gui=BOLD guifg=#a0d0ff guibg=NONE
hi MoreMsg gui=BOLD guifg=#70ffc0 guibg=#8040ff
hi Question gui=BOLD guifg=#e8e800 guibg=NONE
hi StatusLine gui=NONE guifg=#000000 guibg=#909090
hi StatusLineNC gui=NONE guifg=#abac84 guibg=#404040
hi VertSplit gui=NONE guifg=#abac84 guibg=#404040
hi WildMenu gui=NONE guifg=#000000 guibg=#abac84
hi DiffText gui=NONE guifg=#ff78f0 guibg=#a02860
hi DiffChange gui=NONE guifg=#e03870 guibg=#601830
hi DiffDelete gui=NONE guifg=#a0d0ff guibg=#0020a0
hi DiffAdd gui=NONE guifg=#a0d0ff guibg=#0020a0
hi Cursor gui=NONE guifg=#424242 guibg=green
hi lCursor gui=NONE guifg=#ffffff guibg=#8800ff
hi CursorIM gui=NONE guifg=#ffffff guibg=#8800ff
hi Folded gui=NONE guifg=#40f0f0 guibg=#006090
hi FoldColumn gui=NONE guifg=#40c0ff guibg=#404040
hi Directory gui=NONE guifg=red guibg=NONE
hi LineNr gui=NONE guifg=#707070 guibg=NONE
hi NonText gui=BOLD guifg=#707070 guibg=#383838
hi SpecialKey gui=BOLD guifg=green guibg=NONE
hi Title gui=BOLD guifg=#707070 guibg=NONE
hi Visual gui=NONE guifg=#b0ffb0 guibg=#008000
hi VisualNOS gui=NONE guifg=#ffe8c8 guibg=#c06800
hi Comment gui=NONE guifg=#5a6dac guibg=NONE
hi Constant gui=NONE guifg=#b07050 guibg=NONE
hi Error gui=BOLD guifg=#ffffff guibg=#8000ff
hi Identifier gui=NONE guifg=#90c0c0 guibg=NONE
hi Ignore gui=NONE guifg=bg      guibg=NONE
hi PreProc gui=NONE guifg=#c090c0 guibg=NONE
hi Special gui=NONE guifg=#c090c0 guibg=NONE
hi Statement gui=NONE guifg=#c0c090 guibg=NONE
hi Todo gui=BOLD guifg=#ff80d0 guibg=NONE
hi Type gui=NONE guifg=#60f0a8 guibg=NONE
hi Underlined gui=UNDERLINE guifg=#707070 guibg=NONE
hi perlStatement gui=NONE guifg=#c0c090 guibg=NONE
hi perlIdentifier gui=NONE guifg=#90c0c0 guibg=NONE
hi htmlTagName ctermfg=12

" hi perlStatementStorage
" hi perlStatementControl
" hi perlStatementScalar
hi perlStatementRegexp gui=NONE guibg=black
" hi perlStatementNumeric
" hi perlStatementList
hi perlStatementHash gui=NONE guifg=#c0c090 guibg=#404040
" hi perlStatementIOfunc
" hi perlStatementFiledesc
" hi perlStatementVector
" hi perlStatementFiles
" hi perlStatementFlow
" hi perlStatementInclude
" hi perlStatementScope
" hi perlStatementProc
" hi perlStatementSocket
" hi perlStatementIPC
" hi perlStatementNetwork
" hi perlStatementPword
" hi perlStatementTime
" hi perlStatementMisc
hi perlStatementNew gui=NONE guifg=#c0c090 guibg=#424242
hi perlMatchStartEnd gui=NONE guifg=#c0c090 guibg=#424242
" hi perlStatementPackage
" hi perlStatementSub
" hi perlList
" hi perlMisc
hi perlVarPlain gui=NONE guifg=#74c5c6 guibg=bg
hi perlVarNotInMatches gui=NONE guifg=#915555
" hi perlVarSlash
" hi perlVarSimpleMember
" hi perlMethod
hi perlVarPlain2 gui=NONE guifg=#7b8eb1 guibg=bg
hi perlFunctionName gui=NONE guifg=grey60
" hi perlFiledescRead
" hi perlFiledescStatement
" hi perlFormatName
" hi perlFloat
hi perlNumber gui=NONE guifg=#80ac7b guibg=bg
" hi perlString
hi perlQQ gui=NONE guifg=fg guibg=#393939
" hi perlVarSimpleMemberName
" hi perlSubstitutionSQ
" hi perlSubstitutionDQ
" hi perlSubstitutionSlash
" hi perlSubstitutionHash
hi perlSubstitutionBracket gui=NONE guibg=black
" hi perlSubstitutionCurly
" hi perlSubstitutionPling
" hi perlTranslationSlash
" hi perlTranslationHash
hi perlTranslationBracket gui=NONE guibg=black
" hi perlTranslationCurly
hi perlStringStartEnd gui=NONE guifg=#b07050 guibg=#353535
" hi perlHereDoc
" hi perlFormatField
hi perlShellCommand gui=NONE guibg=#c090c0 guibg=#424242
" hi perlStringUnexpanded
" hi perlCharacter
" hi perlSpecialAscii
hi perlSpecialDollar gui=NONE guibg=black
hi perlSpecialString gui=NONE guifg=#c090c0  guibg=bg
hi perlSpecialStringU gui=NONE guibg=black
hi perlSpecialMatch gui=NONE guifg=#c864c7 guibg=bg
hi perlSpecialBEOM gui=NONE guifg=fg guibg=#404040
" hi perlConditional
" hi perlRepeat
hi perlOperator gui=NONE guifg=#c0c090 guibg=#404040
hi perlLabel gui=NONE guifg=#c0c090 guibg=#404040
hi perlControl gui=NONE guifg=#c0c090 guibg=#404040
hi perlSharpBang gui=NONE guifg=#c0c090 guibg=#505050
" hi perlInclude
hi perlType gui=NONE guibg=black
" hi perlStorageClass
" hi perlPackageRef
hi perlPackageDecl gui=NONE guifg=#80ac7b guibg=#404040
" hi perlFunctionPRef

hi Pmenu        guifg=grey80 guibg=grey20
hi PmenuSel     guifg=#dcdccc guifg=grey10
hi PmenuSbar    guibg=#dcdccc
hi PmenuThumb   guifg=#dcdccc
