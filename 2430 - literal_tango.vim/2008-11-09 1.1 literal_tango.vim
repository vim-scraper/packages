" Vim color file
" Filename: literal_tango.vim
" Maintainer: Hinrik Örn Sigurðsson <hinrik.sig at gmail dot com>
" Version: 1.1
" Last Change: Nov 9th 2008
" URLs: http://git.nix.is/?p=hinrik/dotfiles;a=blob_plain;f=.vim/colors/literal_tango.vim;hb=HEAD
"       http://www.vim.org/scripts/script.php?script_id=2430
" Installation: Drop this file in your $VIMRUNTIME/colors/ directory
" License: GNU General Public License version 3 or (at your option) any
"          later version as published by the Free Software Foundation
"
" A color scheme with good contrast, yet easy on the eyes.
" Looks the same in the GUI as it does on a color terminal.
" Uses the Tango color palette (http://tango.freedesktop.org).
"
" Terminal users:
" Intended for display on a black (#000000) background. Make sure your
" terminal is using the Tango color palette. GNOME Terminal uses it by
" default. Linux console users, see http://search.cpan.org/perldoc?conpalette
"
" Changelog:
"
" 1.1:
"   Gave 'Type' a color
"   Linked 'Repeat' and 'Conditional' to 'Statement' again
"   Fixed a few GUI<->terminal inconsistencies
"

if version > 580
    " no guarantees for version 5.8 and below,
    " but this makes it stop complaining
    hi clear
    if exists("syntax_on")
        syntax reset
    endif
endif

let g:colors_name = "literal_tango"

" Terminal
hi Constant    ctermfg=darkred
hi Folded      ctermfg=darkgreen ctermbg=NONE
hi Identifier  ctermfg=darkgreen
hi Ignore      ctermfg=black
hi Label       ctermfg=NONE
hi LineNr      ctermfg=darkgrey
hi Type        ctermfg=darkcyan

" Tango palette
let s:black        = "#2e3436"
let s:darkred      = "#cc0000"
let s:darkgreen    = "#4e9a06"
let s:brown        = "#c4a000"
let s:darkblue     = "#3465a4"
let s:darkmagenta  = "#75507b"
let s:darkcyan     = "#06989a"
let s:lightgrey    = "#d3d7cf"
let s:darkgrey     = "#555753"
let s:lightred     = "#ef2929"
let s:lightgreen   = "#8ae234"
let s:yellow       = "#fce94f"
let s:lightblue    = "#729fcf"
let s:lightmagenta = "#ad7fa8"
let s:lightcyan    = "#34e2e2"
let s:white        = "#eeeeec"

" GUI
exe "hi Normal       guifg=".s:lightgrey    ." guibg=#000000"
exe "hi Folded       guifg=".s:darkgreen    ." guibg=NONE"
exe "hi LineNr       guifg=".s:darkgrey
exe "hi Type         guifg=".s:darkcyan     ." gui=NONE"
exe "hi Label        guifg=NONE"
exe "hi Identifier   guifg=".s:darkgreen
exe "hi Constant     guifg=".s:darkred
exe "hi Statement    guifg=".s:brown        ." gui=NONE"
exe "hi PreProc      guifg=".s:darkmagenta
exe "hi Comment      guifg=".s:darkblue
exe "hi SpecialKey   guifg=".s:darkblue
exe "hi NonText      guifg=".s:lightblue
exe "hi Directory    guifg=".s:darkblue
exe "hi ErrorMsg     guifg=".s:white        ." gui=bold guibg=".s:darkred
exe "hi Search       guifg=".s:black        ." guibg=".s:brown
exe "hi MoreMsg      guifg=".s:darkgreen    ." gui=NONE"
exe "hi Question     guifg=".s:darkgreen    ." gui=NONE"
exe "hi Title        guifg=".s:darkmagenta  ." gui=NONE"
exe "hi Visual       guibg=NONE gui=reverse"
exe "hi WarningMsg   guifg=".s:darkred
exe "hi WildMenu     guibg=".s:brown
exe "hi FoldColumn   guifg=".s:darkblue     ." guibg=".s:lightgrey
exe "hi DiffAdd      guibg=".s:darkblue
exe "hi DiffChange   guibg=".s:darkmagenta
exe "hi DiffDelete   guifg=".s:darkblue     ." guibg=".s:darkcyan
exe "hi DiffText     guibg=".s:darkred
exe "hi SignColumn   guibg=".s:darkblue     ." guibg=".s:lightgrey
exe "hi SpellBad     guisp=".s:darkred
exe "hi SpellCap     guisp=".s:darkblue
exe "hi SpellRare    guisp=".s:darkmagenta
exe "hi SpellLocal   guisp=".s:darkcyan
exe "hi Pmenu        guibg=".s:darkmagenta
exe "hi PmenuSel     guibg=".s:lightgrey
exe "hi PmenuSbar    guibg=".s:lightgrey
exe "hi TabLine      guifg=".s:darkgrey     ." guibg=".s:lightgrey
exe "hi CursorColumn guibg=".s:white
exe "hi CursorLine   guibg=NONE gui=underline"
exe "hi MatchParen   guibg=".s:darkcyan
exe "hi Special      guifg=".s:darkmagenta
exe "hi Underlined   guifg=".s:darkmagenta
exe "hi Error        guifg=".s:white        ." guibg=".s:darkred
exe "hi Todo         guifg=".s:black        ." guibg=".s:brown

