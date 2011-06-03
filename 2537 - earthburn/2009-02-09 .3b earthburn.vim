set background=light

hi clear
if exists("syntax_on")
  syntax reset
endif

let colors_name = "earthburn"

if version >= 700
  " NERDTree current 
  hi CursorLine guibg=#BFB7AD gui=none
  "hi CursorColumn guibg=#e4e2e0
  hi MatchParen guifg=white guibg=#747270 gui=bold

  hi TabLineFill guifg=#BFB39Cguibg=#BFB39C
  hi TabLine guifg=gray40 guibg=#BFB39C gui=none
  hi TabLineSel guifg=gray90 guibg=#AB9876 gui=none

  "P-Menu (auto-completion)
  hi Pmenu guifg=white guibg=#a4a2a0
  "PmenuSel
  "PmenuSbar
  "PmenuThumb
endif

"hi Normal    guifg=gray30   guibg=#dcd8d8
hi Normal    guifg=gray30   guibg=#ccc8c8
hi ModeMsg guifg=gray10 guibg=#B3C71E gui=none


" Html-Titles
hi Title      guifg=grayl0 gui=none
hi Underlined  guifg=gray30 gui=underline

hi Cursor    guifg=white   guibg=#888480

hi lCursor   guifg=black   guibg=white
hi LineNr    guifg=#B0B1A1

"hi LineNr    guifg=#C0C0b0 "b8b8ac

hi StatusLine guifg=gray90 guibg=#888480 gui=none
" inactive 
hi StatusLineNC guifg=gray40 guibg=#ABA7A4 gui=none

hi VertSplit guifg=#ABA7A4 guibg=#ABA7A4 gui=NONE

" hi Folded    guifg=#708090 guibg=#c0d0e0
hi Folded    guifg=#8A9184 guibg=#c0bcbc gui=italic

" unused areas marked with ~
hi NonText gui=none guifg=#ccc8c8

hi Comment   guifg=#8A9184 gui=italic

" ruby: symbols
hi Constant  guifg=#6a6c19
hi String    guifg=#8e9d1a


hi Number    guifg=#6a6c19
hi link Float Number

" ruby: debug, mixin, scope, throw, python: def
hi Statement guifg=#605118 gui=none
hi Statement guifg=gray30 gui=none
""hi Statement guifg=#85513D gui=none

" ruby: interpolation
hi Operator gui=none

" HTML: arguments
" Ruby: classname
hi Type  guifg=#8B762B gui=none

" Python: Standard exceptions, True&False
hi Structure  guifg=Sienna gui=bold,underline

" Ruby: method definitions and calls
hi Function   guifg=gray30

hi Macro   guifg=#545250 gui=none
hi Directory   guifg=#99822F

hi Identifier guifg=#545250 gui=none

hi Repeat      guifg=#9C420C

" Ruby: if..else..end
hi Conditional guifg=#730000""682805"" 7A2A0F

" Ruby: require
hi PreProc    guifg=#64634B gui=none

" Ruby: def..end, class..end
hi Define guifg=#685616

" used by help tags * |
hi Ignore guifg=#A8A890"A0A191


hi Error guifg=#a02000 guibg=white gui=underline
hi Todo guifg=#6A7164 guibg=NONE gui=underline

hi Delimiter guifg=#9C9E6C

hi Search guibg=#FFE9A8
hi helpTag guifg=#5E7A7C  gui=underline guibg=red
hi Tag guifg=#5E7A7C  gui=underline guibg=red

" Python: %(...)s - constructs, encoding, D: \n etc
" Ruby: ""
hi Special guifg=#808000 gui=none

" color of <TAB>s etc...  , NERDTRee
hi SpecialKey guifg=gray60 gui=italic

" visual selection higlight
hi Visual guibg=#A8E4FF

hi Delimiter guifg=#9C9E6C

" Diff
hi DiffChange guifg=NONE guibg=#e4e2e0 gui=italic
hi DiffText guifg=NONE guibg=#f0e0a0 gui=none
hi DiffAdd guifg=NONE guibg=#c0e0d0 gui=bold
hi DiffDelete guifg=NONE guibg=#f0a0a0 gui=italic,bold

hi link rubyBoolean Boolean
hi link rubyComment Comment
hi link rubyString String
hi link rubyStringDelimiter String

hi htmlTag guifg=#755C3B gui=none
hi link htmlEndTag htmlTag
hi link htmlTagName htmlTag

hi link hamlHtmlTag htmlTagName
hi link hamlPreDef hamlHtmlTag
hi link hmlHtmlTagEndl hamlHtmlTag

" hi rubyASCIICode
hi rubyAccess guifg=#99642F
"hi rubyAttribute guifg=red gui=underline
hi rubyBeginEnd guifg=#B0582D gui=underline
"hi rubyBlock                   gui=underline
hi rubyBlockArgument gui=underline
hi rubyBlockParameter guifg=gray20
hi link rubyClass Type

""hi rubyClassVariable           gui=none        guifg=#556B2F     guibg=white
"hi rubyConstant                gui=none        guifg=#DC143C     guibg=white

" do..end begin rescue end
hi rubyControl                 guifg=#9C420C
hi rubyControl                 guifg=#990000

"hi rubyCurlyBlock guifg=#9C420C
"hi rubyData                    gui=            guifg=            guibg=
"hi rubyDataDirective           gui=            guifg=            guibg=
"hi rubyDefine gui=none
"hi rubyDelimEscape
"hi rubyDoBlock guifg=SlateBlue guibg=red
"hi rubyDocumentation           gui=            guifg=            guibg=
"hi rubyError
"hi rubyEscape
"hi rubyEval
"hi rubyException               gui=underline
"hi rubyExprSubst               gui=underline        guifg=#FF4500
"hi rubyFloat
hi rubyFunction                gui=none        guifg=#685616
"hi rubyGlobalVariable          gui=none            guifg=cyan
hi link rubyHeredocStart Comment
hi link rubyHeredocEnd Comment
"hi rubyIdentifier              gui=underline
"hi rubyInclude                 gui=            guifg=            guibg=
 hi rubyInstanceVariable        gui=none            guifg=gray20
"hi rubyInteger
"hi rubyInterpolation guifg=Orange
"hi rubyIterator                gui=underline        guifg=black
hi link rubyKeyword Normal
" hi rubyKeywordAsMethod
hi rubyLocalVariableOrMethod gui=underline guifg=cyan
hi link rubyModule Type
"hi rubyNestedAngleBrackets
"hi rubyNestedCurlyBraces gui=underline
"hi rubyNestedParentheses
"hi rubyNestedSquareBrackets
"hi rubyNoDoBlock
"hi rubyNoInterpolation
"hi rubyNumber                  gui=            guifg=            guibg=
hi link rubyOperator Normal
"hi rubyOptDoBlock gui=underline
"hi rubyOptDoLine
"hi rubyPredefinedConstant      gui=            guifg=            guibg=
"hi rubyPredefinedIdentifier    gui=            guifg=            guibg=
"hi rubyPredefinedVariable      gui=            guifg=            guibg=
"hi rubyPseudoVariable
"hi rubySharpBang               gui=            guifg=            guibg=
"hi rubySpaceError
hi rubySymbol                  gui=none        guifg=#6A6C19

"hi rubyTodo                    gui=            guifg=            guibg=
