"==============================================================================.
"        File: kaltex.vim                                                      |
"     License: Public Domain, FREE as LOVE.                                    |
" Description: A cold colorscheme for GVIM/VIM with bold, italic and underline |
"              font styles. On VIM it uses a black background, because it was  |
"              easier to do and *seems* to be the only way to make a           |
"              colorscheme look nice on any darn terminal.                     |
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
let g:colors_name = "kaltex"
hi Normal ctermbg=Black ctermfg=Gray cterm=NONE guibg=#eaeaea guifg=Black gui=none

" Vim >= 7.0 specific colors {{{1
if v:version >= 700
  " Cursor colors {{{2
  hi Cursor ctermbg=DarkBlue ctermfg=fg cterm=NONE guibg=DarkBlue guifg=bg gui=none
  hi CursorLine ctermbg=Green guibg=#ccccff gui=none
  hi CursorColumn ctermbg=Green guibg=#ccccff gui=none
  " only for Win32, IME status
  if has('multi_byte_ime')
    hi CursorIM guibg=DarkMagenta guifg=NONE gui=none
  endif

  " Auto-completion Popup Menu colors {{{2
  hi Pmenu ctermbg=DarkCyan ctermfg=bg cterm=NONE guibg=#9999cc guifg=fg gui=none
  hi PmenuSel ctermbg=DarkBlue ctermfg=fg cterm=NONE guibg=#333399 guifg=bg gui=none
  hi PmenuSbar ctermbg=DarkBlue ctermfg=DarkBlue cterm=NONE guibg=#333399 guifg=fg gui=none
  hi PmenuThumb ctermbg=Gray ctermfg=Gray cterm=NONE guibg=#6666cc guifg=fg gui=none

  " Tab colors {{{2
  if has("windows")
    hi TabLine ctermbg=DarkCyan ctermfg=DarkBlue guibg=DarkGray guifg=DarkBlue gui=none
    hi TabLineFill ctermbg=DarkCyan ctermfg=DarkBlue guibg=DarkGray guifg=DarkBlue gui=none
    hi TabLineSel ctermbg=DarkBlue ctermfg=Gray guibg=DarkBlue guifg=Gray gui=bold
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
hi ErrorMsg ctermbg=Red ctermfg=White cterm=NONE guibg=Red3 guifg=White gui=none
hi ModeMsg ctermbg=bg ctermfg=fg cterm=NONE guibg=bg guifg=fg gui=none
hi MoreMsg ctermbg=bg ctermfg=Green cterm=NONE guibg=bg guifg=DarkGreen gui=none
hi Question ctermbg=bg ctermfg=White cterm=NONE guibg=bg guifg=DarkBlue gui=none
hi Directory ctermbg=bg ctermfg=Blue cterm=NONE guibg=bg guifg=Blue gui=none
hi Title ctermbg=bg ctermfg=Blue cterm=NONE guibg=bg guifg=Blue gui=none

" Diff colors {{{1
if has("diff")
  hi DiffAdd ctermbg=Green ctermfg=bg cterm=NONE guibg=LightGreen gui=none
  hi DiffChange ctermbg=Blue ctermfg=fg cterm=NONE guibg=LightBlue gui=none
  hi DiffDelete ctermbg=Red ctermfg=fg cterm=NONE guibg=LightRed gui=none
  hi DiffText ctermbg=Cyan ctermfg=bg cterm=NONE guibg=LightCyan gui=none
endif

" Outline, Fold & Sign columns colors {{{1
hi LineNr ctermbg=bg ctermfg=DarkYellow guibg=Gray85 guifg=Gray40 gui=none
if has("folding")
  hi Folded ctermbg=DarkCyan ctermfg=bg cterm=NONE guibg=Gray80 guifg=DarkBlue gui=none
  hi FoldColumn ctermbg=DarkCyan ctermfg=bg cterm=NONE guibg=Gray80 guifg=DarkBlue gui=none
endif
if has("signs")
  hi SignColumn ctermbg=bg ctermfg=Yellow cterm=NONE guibg=Gray85 guifg=DarkBlue gui=none
endif

" Search & Special characters' colors {{{1
if has("extra_search")
  hi Search ctermbg=Yellow cterm=NONE guibg=Yellow gui=none
  hi IncSearch ctermbg=Green ctermfg=bg cterm=NONE guibg=LightMagenta guifg=fg gui=none
endif
hi NonText ctermbg=bg ctermfg=DarkGray guibg=bg guifg=DarkGray gui=none
hi SpecialKey ctermbg=bg ctermfg=Brown guibg=bg guifg=#993333 gui=none

" Window Bars, Status line & Visual mode colors {{{1
hi StatusLine ctermbg=DarkBlue ctermfg=fg cterm=NONE guibg=DarkRed guifg=bg gui=none
if has("windows")
  hi StatusLineNC ctermbg=DarkGreen ctermfg=bg cterm=NONE guibg=Gray60 guifg=DarkRed gui=none
endif

if has("vertsplit")
  hi VertSplit ctermbg=DarkGreen ctermfg=bg cterm=NONE guibg=DarkRed guifg=bg gui=none
endif

if has("wildmenu")
  hi WildMenu ctermbg=bg ctermfg=Green cterm=NONE guibg=bg guifg=DarkRed gui=bold
endif

if has("visual")
  hi Visual ctermbg=DarkMagenta ctermfg=bg cterm=NONE guibg=#ccee88 guifg=fg gui=none
  hi VisualNOS ctermbg=bg ctermfg=DarkMagenta cterm=NONE guibg=#ccee88 guifg=fg gui=none
endif

" Syntax highlighting colors {{{1
hi Comment ctermbg=bg ctermfg=DarkGray guibg=bg guifg=Gray50 gui=none
hi link SpecialComment Comment

hi Character ctermbg=bg ctermfg=Red guibg=bg guifg=DarkRed gui=none
hi String ctermbg=bg ctermfg=Magenta guibg=bg guifg=Blue gui=none
hi Constant ctermbg=bg ctermfg=Yellow guibg=bg guifg=DarkGreen gui=none
hi link Number Constant
hi link Float Constant
hi link Boolean Constant

hi Identifier ctermbg=bg ctermfg=Green guibg=bg guifg=DarkGreen gui=none
hi Function ctermbg=bg ctermfg=Green guibg=bg guifg=DarkGreen gui=bold

hi Statement ctermbg=bg ctermfg=Blue guibg=bg guifg=DarkBlue gui=bold
hi link Conditional Statement
hi link Repeat Statement
hi link Operator Statement
hi link Keyword Statement
hi link Label Statement
hi link Exception Statement

hi Type ctermbg=bg ctermfg=DarkMagenta guibg=bg guifg=DarkMagenta gui=bold
hi link StorageClass Type
hi link Structure Type
hi link Typedef Type

hi PreProc ctermbg=bg ctermfg=DarkCyan guibg=bg guifg=DarkCyan gui=none
hi PreCondit ctermbg=bg ctermfg=DarkYellow guibg=bg guifg=DarkYellow gui=bold
hi link Include PreProc
hi link Define PreProc

hi Special ctermbg=bg ctermfg=Blue guibg=bg guifg=Blue gui=none
hi SpecialChar ctermbg=bg ctermfg=Red guibg=bg guifg=Red gui=none
hi Tag ctermbg=bg ctermfg=DarkRed guibg=bg guifg=DarkRed gui=none
hi Delimiter ctermbg=bg ctermfg=DarkRed guibg=bg guifg=DarkRed gui=none
hi Debug ctermbg=bg ctermfg=DarkGray guibg=bg guifg=DarkGray gui=none

hi MatchParen ctermbg=White ctermfg=Black cterm=NONE guibg=Blue guifg=White gui=none
hi Error ctermbg=Red ctermfg=White cterm=NONE guibg=bg guifg=Red gui=none
hi Ignore ctermbg=bg ctermfg=bg cterm=NONE guibg=bg guifg=bg gui=none
hi Todo ctermbg=DarkCyan ctermfg=bg cterm=NONE guibg=DarkCyan guifg=bg gui=none
hi Underlined ctermbg=bg ctermfg=Blue cterm=underline guibg=bg guifg=Blue gui=underline
"}}}1
"==========================================================================={{{1
" vim: set et sw=2 sts=2 ts=8:
" vim600: set fdc=2 fdm=marker:
