" Vim color file
" Filename: literal_tango.vim
" Maintainer: Hinrik Örn Sigurðsson <hinrik.sig at gmail dot com>
" Version: 1.0
" Last Change: Nov 8th 2008
" URL: http://git.nix.is/?p=hinrik/dotfiles;a=blob_plain;f=.vim/colors/literal_tango.vim;hb=HEAD
" Installation: Drop this file in your $VIMRUNTIME/colors/ directory
" License: GNU General Public License version 3 or (at your option) any
"          later version as published by the Free Software Foundation
"
" A color scheme that's easy on the eyes.
" Uses the Tango color palette (http://tango.freedesktop.org)
"
" Terminal users:
" Intended for display on a black (#000000) background. Make sure your
" terminal is using the Tango color palette. GNOME Terminal uses it by
" default. Linux console users, see http://search.cpan.org/perldoc?conpalette

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
hi Conditional ctermfg=darkcyan
hi Constant    ctermfg=darkred
hi Folded      ctermfg=darkgreen ctermbg=NONE
hi Identifier  ctermfg=darkgreen
hi Ignore      ctermfg=black
hi Label       ctermfg=NONE
hi LineNr      ctermfg=darkgrey
hi Repeat      ctermfg=darkcyan
hi Type        ctermfg=NONE

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
exe "hi Normal       guifg=".s:lightgrey    ." guibg=#000000 gui=NONE"
exe "hi Folded       guifg=".s:darkgreen    ." guibg=NONE gui=NONE"
exe "hi LineNr       guifg=".s:darkgrey     ." gui=NONE"
exe "hi Type         guifg=NONE"            ." gui=NONE"
exe "hi Label        guifg=NONE"            ." gui=NONE"
exe "hi Identifier   guifg=".s:darkgreen    ." gui=NONE"
exe "hi Constant     guifg=".s:darkred      ." gui=NONE"
exe "hi Repeat       guifg=".s:darkcyan     ." gui=NONE"
exe "hi Conditional  guifg=".s:darkcyan     ." gui=NONE"
exe "hi Statement    guifg=".s:brown        ." gui=NONE"
exe "hi PreProc      guifg=".s:darkmagenta  ." gui=NONE"
exe "hi Comment      guifg=".s:darkblue     ." gui=NONE"
exe "hi SpecialKey   guifg=".s:darkblue
exe "hi NonText      guifg=".s:lightblue
exe "hi Directory    guifg=".s:darkblue
exe "hi ErrorMsg     guifg=".s:white        ." gui=bold guibg=".s:darkred
exe "hi Search       guifg=".s:black        ." guibg=".s:brown
exe "hi MoreMsg      guifg=".s:darkgreen
exe "hi Question     guifg=".s:darkgreen
exe "hi Title        guifg=".s:darkmagenta
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
exe "hi CursorLine   guibg=bg gui=underline"
exe "hi MatchParen   guibg=".s:darkcyan
exe "hi Special      guifg=".s:darkmagenta
exe "hi Underlined   guifg=".s:darkmagenta
exe "hi Error        guifg=".s:white        ." guibg=".s:darkred
exe "hi Todo         guifg=".s:black        ." guibg=".s:brown

