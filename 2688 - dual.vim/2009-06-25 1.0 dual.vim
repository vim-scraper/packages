"==============================================================================.
"        File: dual.vim                                                        |
"     License: Public Domain, FREE as LOVE.                                    |
" Description: The Dual colorscheme you've been longing for. On VIM, it uses   |
"              dark colors. On GVIM, it uses bright colors without bold,       |
"              italic nor underline font styles (except for spell checking     |
"              and some GUI elements).                                         |
"              Only standard (for all languages) highlight-groups are set.     |
"              This colorscheme _should_ look nice everywhere.                 |
"==============================================================================|
"      Author: drachenkiraa, {ZdrachenZkiraaZ}@{ZgmailZ}.{comZ}  (remove: Z{}) |
" Last Change: 2009 Jun 25                                                     |
"     Version: 1.0                                                             |
"==========================================================================={{{1
"  Color Test: :he group-name                                                  |
"              :so $VIMRUNTIME/syntax/hitest.vim                               |
"   Tested On: - Windows (gvim v7.1), Linux (gvim v6.3),                       |
"              - DOS (vim v7.1), Standard Linux Terminal (gvim v6.3, v6.4),    |
"              - Xterm, Rxvt, Konsole (vim v6.3, v6.4).                        |
"        TODO: * Test this colorscheme on newer versions of vim/gvim for Linux |
"                and other systems.                                            |
"              * Are all the has("feature") checks really worth?               |
"                Please enlighten me if I'm wrong.                             |
"==============================================================================|
" Random Tips:                                                                 |
" * If your terminal supports more than 8 colors (which is the case of most    |
"   xterms, rxvts, and others), then it is worth adding the following line     |
"   somewhere into your .vimrc:                                                |
"       set t_Co=16                                                            |
"   That'll make this colorscheme look a lot better on such terminals.         |
"   For further help checkout:                                                 |
"       :he term-dependent-settings                                            |
"       :he term                                                               |
"===========================================================================}}}1
" Initial setup stuff {{{1
" Remove existing highlighting
if has("gui_running")
  set background=light
else
  set background=dark
endif
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "dual"
hi Normal ctermbg=Black ctermfg=Gray guibg=White guifg=Black gui=none

" Vim >= 7.0 specific colors {{{1
if v:version >= 700
  " Cursor colors {{{2
  hi Cursor ctermbg=fg ctermfg=bg cterm=NONE guibg=DarkBlue guifg=bg gui=none
  hi CursorLine ctermbg=DarkRed guibg=Gray95 gui=none
  hi CursorColumn ctermbg=DarkRed guibg=Gray95 gui=none
  " only for Win32, IME status
  if has('multi_byte_ime')
    hi CursorIM guibg=#660066 guifg=NONE gui=none
  endif

  " Auto-completion Popup Menu colors {{{2
  hi Pmenu ctermbg=Gray ctermfg=bg cterm=NONE guibg=#ccff99 guifg=fg gui=none
  hi PmenuSel ctermbg=DarkRed ctermfg=White cterm=NONE guibg=#669933 guifg=bg gui=bold
  hi PmenuSbar ctermbg=DarkRed ctermfg=fg cterm=NONE guibg=#99cc66 guifg=fg gui=none
  hi PmenuThumb ctermbg=DarkGray ctermfg=DarkGray cterm=NONE guibg=#669933 guifg=bg gui=none

  " Tab colors {{{2
  if has("windows")
    hi TabLine ctermbg=DarkBlue ctermfg=Gray guibg=Gray80 guifg=fg gui=none
    hi TabLineFill ctermbg=DarkBlue ctermfg=Gray guibg=Gray80 guifg=fg gui=none
    hi TabLineSel ctermbg=Green ctermfg=DarkBlue guibg=Gray90 guifg=fg gui=bold
  endif

  " Spell checking colors {{{2
  if has("spell")
    hi SpellBad ctermbg=White ctermfg=Red cterm=NONE guisp=Red gui=undercurl
    hi SpellCap ctermbg=White ctermfg=Blue cterm=NONE guisp=Blue gui=undercurl
    hi SpellLocal ctermbg=White ctermfg=DarkCyan cterm=NONE guisp=DarkCyan gui=undercurl
    hi SpellRare ctermbg=White ctermfg=Magenta cterm=NONE guisp=Magenta gui=undercurl
  endif

endif "}}}1
" Messages and other texts' colors {{{1
hi WarningMsg ctermbg=bg ctermfg=Red cterm=NONE guibg=bg guifg=Red3 gui=none
hi ErrorMsg ctermbg=Red ctermfg=White cterm=NONE guibg=Red3 guifg=bg gui=none
hi ModeMsg ctermbg=bg ctermfg=fg cterm=NONE guibg=bg guifg=fg gui=none
hi MoreMsg ctermbg=bg ctermfg=Green cterm=NONE guibg=bg guifg=Gray30 gui=none
hi Question ctermbg=bg ctermfg=White cterm=NONE guibg=bg guifg=Gray30 gui=none
hi Directory ctermbg=bg ctermfg=Yellow cterm=NONE guibg=bg guifg=Blue2 gui=none
hi Title ctermbg=bg ctermfg=Magenta cterm=NONE guibg=bg guifg=Blue gui=bold

" Diff colors {{{1
if has("diff")
  hi DiffAdd ctermbg=DarkGreen ctermfg=bg cterm=NONE guibg=#ccffcc gui=none
  hi DiffChange ctermbg=DarkBlue ctermfg=fg cterm=NONE guibg=#ccccff gui=none
  hi DiffDelete ctermbg=DarkRed ctermfg=bg cterm=NONE guibg=#ffcccc gui=none
  hi DiffText ctermbg=DarkCyan ctermfg=bg cterm=NONE guibg=#ccffff gui=none
endif

" Outline, Fold & Sign columns colors {{{1
hi LineNr ctermbg=bg ctermfg=DarkGray guibg=Gray95 guifg=Gray40 gui=none
if has("folding")
  hi Folded ctermbg=DarkBlue ctermfg=fg guibg=Gray85 guifg=Gray20 gui=none
  hi FoldColumn ctermbg=DarkBlue ctermfg=fg guibg=Gray85 guifg=Gray20 gui=none
endif
if has("signs")
  hi SignColumn ctermbg=DarkBlue ctermfg=Yellow cterm=NONE guibg=Gray95 guifg=#0033ff gui=none
endif

" Search & Special characters' colors {{{1
if has("extra_search")
  hi Search ctermbg=Yellow cterm=NONE guibg=Yellow gui=none
  hi IncSearch ctermbg=LightMagenta ctermfg=bg cterm=NONE guibg=LightMagenta guifg=fg gui=none
endif
hi NonText ctermbg=bg ctermfg=DarkGray guibg=bg guifg=DarkGray gui=none
hi SpecialKey ctermbg=bg ctermfg=Brown guibg=bg guifg=#993333 gui=none

" Window Bars, Status line & Visual mode colors {{{1
hi StatusLine ctermbg=Cyan ctermfg=bg cterm=NONE guibg=#ccff99 guifg=fg gui=none

if has("windows")
  hi StatusLineNC ctermbg=DarkCyan ctermfg=bg cterm=NONE guibg=#99cc66 guifg=fg gui=none
  " 8 color terminals fix
  if &t_Co < 16
    hi StatusLineNC ctermfg=DarkCyan
  endif
endif

if has("vertsplit")
  hi VertSplit ctermbg=DarkCyan ctermfg=bg cterm=NONE guibg=#99cc66 guifg=fg gui=none
endif

if has("wildmenu")
  hi WildMenu ctermbg=bg ctermfg=Cyan guibg=#669933 guifg=bg gui=bold
endif

if has("visual")
  hi Visual ctermbg=DarkBlue ctermfg=fg cterm=NONE guibg=NavajoWhite guifg=fg gui=none
  hi VisualNOS ctermbg=fg ctermfg=DarkBlue cterm=NONE guibg=NavajoWhite guifg=fg gui=none
endif

" Syntax highlighting colors {{{1
hi Comment ctermbg=bg ctermfg=DarkGreen guibg=bg guifg=DarkGreen gui=none
hi SpecialComment ctermbg=bg ctermfg=DarkGreen guibg=bg guifg=#006600 gui=none

hi Constant ctermbg=bg ctermfg=Magenta guibg=bg guifg=#cc6600 gui=none
hi Number ctermbg=bg ctermfg=Magenta guibg=bg guifg=#ff6600 gui=none
hi Float ctermbg=bg ctermfg=Magenta guibg=bg guifg=#cc00cc gui=none
hi Boolean ctermbg=bg ctermfg=Cyan guibg=bg guifg=#3366ff gui=none
hi Character ctermbg=bg ctermfg=Magenta guibg=bg guifg=Red gui=none
hi String ctermbg=bg ctermfg=Red guibg=bg guifg=#cc0000 gui=none

hi Identifier ctermbg=bg ctermfg=Cyan guibg=bg guifg=#006699 gui=none
hi Function ctermbg=bg ctermfg=Magenta guibg=bg guifg=#9900cc gui=none

hi Statement ctermbg=bg ctermfg=Green guibg=bg guifg=#0000cc gui=none
hi Conditional ctermbg=bg ctermfg=Green guibg=bg guifg=#0000cc gui=none
hi Repeat ctermbg=bg ctermfg=Green guibg=bg guifg=#0000cc gui=none
hi Operator ctermbg=bg ctermfg=White guibg=bg guifg=fg gui=none
hi Keyword ctermbg=bg ctermfg=Green guibg=bg guifg=#0000cc gui=none
hi Label ctermbg=bg ctermfg=Cyan guibg=bg guifg=#6600cc gui=none
hi Exception ctermbg=bg ctermfg=Cyan guibg=bg guifg=#cc0066 gui=none

hi Type ctermbg=bg ctermfg=Cyan guibg=bg guifg=#0000cc gui=none
hi StorageClass ctermbg=bg ctermfg=Cyan guibg=bg guifg=#0000cc gui=none
hi Structure ctermbg=bg ctermfg=Cyan guibg=bg guifg=#0000cc gui=none
hi Typedef ctermbg=bg ctermfg=Cyan guibg=bg guifg=#0000cc gui=none

hi PreProc ctermbg=bg ctermfg=Yellow guibg=bg guifg=#666600 gui=none
hi PreCondit ctermbg=bg ctermfg=Blue guibg=bg guifg=#990000 gui=none
hi Include ctermbg=bg ctermfg=Yellow guibg=bg guifg=#666600 gui=none
hi Define ctermbg=bg ctermfg=Yellow guibg=bg guifg=#666600 gui=none
hi Macro ctermbg=bg ctermfg=Yellow guibg=bg guifg=#666600 gui=none

hi Special ctermbg=bg ctermfg=Red guibg=bg guifg=#3366cc gui=none
hi SpecialChar ctermbg=bg ctermfg=Red guibg=bg guifg=Red gui=none
hi Tag ctermbg=bg ctermfg=Red guibg=bg guifg=#660000 gui=none
hi Delimiter ctermbg=bg ctermfg=Red guibg=bg guifg=#660000 gui=none
hi Debug ctermbg=bg ctermfg=DarkGray guibg=bg guifg=DarkGray gui=none

hi MatchParen ctermbg=White ctermfg=Black cterm=NONE guibg=#ff66ff guifg=fg gui=none
hi Error ctermbg=Red ctermfg=White cterm=NONE guibg=bg guifg=Red gui=none
hi Ignore ctermbg=bg ctermfg=bg cterm=NONE guibg=bg guifg=bg gui=none
hi Todo ctermbg=Yellow ctermfg=bg cterm=NONE guibg=#ffff99 guifg=fg gui=none
hi Underlined ctermbg=DarkBlue ctermfg=Cyan cterm=underline guibg=bg guifg=Blue2 gui=underline
"}}}1
"==========================================================================={{{1
" vim: set et sw=2 sts=2 ts=8:
" vim600: set fdc=2 fdm=marker:
