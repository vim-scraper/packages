" northland
" Maintainer:   Luka Djigas <ldigas@@gmail.com>
" Last Change:  25.03.2008.
" URL:          --

set background=dark
hi clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name="northland_color_scheme"

hi Cursor             gui=NONE       guifg=White         guibg=PaleTurquoise3
"hi CursorIM
"like Cursor, but used when in IME mode |CursorIM|
hi CursorColumn       gui=NONE                           guibg=#003853
hi CursorLine         gui=NONE                           guibg=#003853
hi Directory          gui=NONE       guifg=#B31000       guibg=NONE
"hi DiffAdd
"diff mode: Added line |diff.txt|
"hi DiffChange
"diff mode: Changed line |diff.txt|
"hi DiffDelete
"diff mode: Deleted line |diff.txt|
"hi DiffText
"diff mode: Changed text within a changed line |diff.txt|
hi ErrorMsg           gui=NONE       guifg=White         guibg=#B31000
hi VertSplit          gui=NONE       guifg=Black         guibg=#999999       guisp=NONE
"hi Folded
"line used for closed folds
"hi FoldColumn
"'foldcolumn'
"hi SignColumn
"column where |signs| are displayed
hi IncSearch          gui=NONE       guifg=White         guibg=#B31000
hi LineNr             gui=bold       guifg=#507080       guibg=Black
hi MatchParen         gui=bold       guifg=#B31000       guibg=NONE
hi ModeMsg            gui=bold       guifg=#B31000       guibg=NONE
hi MoreMsg            gui=bold       guifg=#B31000       guibg=NONE
hi NonText            gui=NONE       guifg=#B31000       guibg=NONE
hi Normal             gui=NONE       guifg=White         guibg=#001025       guisp=NONE
hi Pmenu              gui=NONE       guifg=Black         guibg=#B31000
hi PmenuSel           gui=NONE       guifg=#B31000       guibg=Black
hi PmenuSbar                                             guibg=#003853
hi PmenuThumb                                            guibg=Black
hi Question           gui=bold       guifg=#B31000       guibg=NONE
hi Search             gui=NONE       guifg=White         guibg=#B31000
hi SpecialKey         gui=NONE       guifg=#999999       guibg=NONE
"hi SpellBad
"Word that is not recognized by the spellchecker. |spell|
"This will be combined with the highlighting used otherwise.
"hi SpellCap
"Word that should start with a capital. |spell|
"This will be combined with the highlighting used otherwise.
"hi SpellLocal
"Word that is recognized by the spellchecker as one that is
"used in another region. |spell|
"This will be combined with the highlighting used otherwise.
"hi SpellRare
"Word that is recognized by the spellchecker as one that is
"hardly ever used. |spell|
"This will be combined with the highlighting used otherwise.
hi StatusLine         gui=bold       guifg=Black         guibg=#B31000       guisp=NONE
hi StatusLineNC       gui=NONE       guifg=Black         guibg=#999999       guisp=NONE
"hi TabLine
"tab pages line, not active tab page label
"hi TabLineFill
"tab pages line, where there are no labels
"hi TabLineSel
"tab pages line, active tab page label
"hi Title
"titles for output from ":set all", ":autocmd" etc.
hi Visual             gui=NONE       guifg=NONE          guibg=#B31000
"hi VisualNOS
"Visual mode selection when vim is "Not Owning the Selection".
"Only X11 Gui's |gui-x11| and |xterm-clipboard| supports this.
hi WarningMsg         gui=bold       guifg=#B31000       guibg=NONE
hi WildMenu           gui=NONE       guifg=#B31000       guibg=Black
"hi User1, User2 ... 9
"The 'statusline' syntax allows the use of 9 different highlights in the
"statusline and ruler (via 'rulerformat').  The names are User1 to User9.
"For the GUI you can use these groups to set the colors for the menu,
"scrollbars and tooltips.  They don't have defaults.  This doesn't work for the
"Win32 GUI.  Only three highlight arguments have any effect here: font, guibg,
"and guifg.
"hi Menu
"Current font, background and foreground colors of the menus.
"Also used for the toolbar.
"Applicable highlight arguments: font, guibg, guifg.
"NOTE: For Motif and Athena the font argument actually
"specifies a fontset at all times, no matter if 'guifontset' is
"empty, and as such it is tied to the current |:language| when
"set.
"hi Scrollbar
"Current background and foreground of the main window's
"scrollbars.
"Applicable highlight arguments: guibg, guifg.
"hi Tooltip
"Current font, background and foreground of the tooltips.
"Applicable highlight arguments: font, guibg, guifg.
"NOTE: For Motif and Athena the font argument actually
"specifies a fontset at all times, no matter if 'guifontset' is
"empty, and as such it is tied to the current |:language| when
"set.
