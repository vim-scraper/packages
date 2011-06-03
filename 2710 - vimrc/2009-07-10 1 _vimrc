"File     :_vimrc
"Required :vim 7.1+
"Language :ex
"Title    :vim config file
"Author   :Roy Mathur
"Copyright:
"Notes    :
"Version  :v 1.00 Fri 30 Jan 2009
"          ----------------------
"          Actually started a few years back, but never bothered to
"          comment it.
"
"
"
"****************************************************************************
"****************************************************************************
"
"
"
"GVim Only Options
"-----------------
set guifont=Courier_New:h10
set guioptions+=b "show horizontal scroll bar
"
"
"
"Vim Options
"-----------
set viminfo='10,f1,<10         "don't know
set history=1000               "remember last n commands
set mouse=a                    "enable full mouse support in the console
set autoindent                 "don't know
set smartindent                "don't know
set shiftwidth=4               "don't know
set softtabstop=4              "eh?
set expandtab                  "no clue what this does
set number                     "turn line numbering on
syntax on                      "syntax highlighting on
"set foldmethod=syntax         "enable code folding
set showmatch                  "show briefly matching bracket when closing it
set ttyfast                    "smoother redraws
autocmd InsertLeave * se nocul "highlight current line in insert mode
autocmd InsertEnter * se cul   "unhighlight current line when not in insert mode
set sessionoptions+=unix,slash "store session info in Unix format
set ignorecase                 "make searches case-insensitive
set nowrap
"
"
"
"view whitespace
"---------------
"set list                      "display non-printing characters
"set listchars=tab:>-,trail:-  choose characters to show non-printing characters
"
"
"
"Status Bar
"----------
set ruler                      "turn on status bar
set showmode                   "display the current mode in the status line
set showcmd                    "show partially-typed commands in the status line
set laststatus=2               "always show status line 
"
"
"
"set vim locations
"-----------------
"let $VIM = "/roy/programs/code/vim"
"let $VIMRUNTIME = "/roy/programs/code/vim"     }DONE IN BATCH FILE
"let $HOME = "/roy/programs/code/vim"
"
"
"
"console menu!
"-------------
:source $VIMRUNTIME/menu.vim
:set wildmenu
:set cpo-=<
:set wcm=<C-Z>
:map <C-M> :emenu <C-Z>
"
"
"
"Kill Beeps
"----------
"This diables stupid errorbell and, horrible, epilepsy inducing visual bell
set noerrorbells
set visualbell
set t_vb=
"
"
"
"MS Windows-like behaviour
"-------------------------
so $VIM\mswin.vim
behave mswin
"
"Extra CUA-like Short-Cuts
"-------------------------
"
"CTRL+N New (to the right)
:map <C-n> :vnew <CR><C-w>r
"
"CTRL+O Open
:map <C-o> :exe "20vsp " . expand("%:p:h") <CR>
"
"F4 Close
:map <F4> :close <CR>
"
"CTRL+S Save
:map <C-s> :confirm w <CR>
"
"ALT+X Exit 
:map <M-x> :confirm qa <CR>
"
"CTRL+F Find
:map <C-f> :/findThis
"
"CTRL+R Replace
:map <C-r> :firstLine, lastLine s/replaceThis/withThis/
"
"F3 RepeatCmd
:map <F3> n <CR>
"
"ALT+1 save session
:map <M-1> :mksession! $VIM/sessions/lastsess.vim <CR>
"
"ALT+2 restore session:q
"
:map <M-2> :source $VIM/sessions/lastsess.vim <CR>
"
"ALT+5 shell
:map <M-5> :shell <CR>
"
"F7 WordProcessorOn
:map <F7> :set linebreak <CR> :set display+=lastline <CR> :set wrap <CR> :setlocal spell spelllang=en_gb <CR>
"
"F8 WordProcessorOff
:map <F8> :set nowrap <CR> :set nospell <CR>
"
"F2 Help
:map <F2> :sview $VIM/hobbes/hobbes.txt <CR><C-w>H<CR>
"
"...same shortcuts now in Hobbes Menu
"
"window title
:set titlestring=Hobbes\ %t%(\ %M%)%(\ (%{expand(\"%:~:.:h\")})%)%(\ %a%)
"menu
:menu Hobbes.New\	Ctrl+N :vnew<CR><C-w>r
:menu Hobbes.Open\	Ctrl+O :exe "20vsp " . expand("%:p:h") <CR>
:menu Hobbes.Close\	F4 :close<CR>
:menu Hobbes.Save\	Ctrl+S :confirm w<CR>
:menu Hobbes.Exit\	Alt+X :confirm qa<CR>
:menu Hobbes.Find\	Ctrl+F :/findThis
:menu Hobbes.Replace\	Ctrl+R :firstLine, lastLine s/replaceThis/withThis/
:menu Hobbes.RepeatCmd\	F3 n<CR>
:menu Hobbes.SaveSession\	Alt+1 :mksession! $VIM/sessions/lastsess.vim<CR>
:menu Hobbes.RestoreSession\	Alt+2 :source $VIM/sessions/lastsess.vim<CR><CR>
:menu Hobbes.WordProcessorOn\	F7 :set linebreak <CR> :set display+=lastline <CR> :set wrap <CR> :setlocal spell spelllang=en_gb <CR>
:menu Hobbes.WordProcessorOff\	F8 :set nolinebreak <CR> :set nowrap <CR> :set nospell <CR>
:menu Hobbes.Shell\	Alt+5 :shell <CR>
:menu Hobbes.Help\	F2 :sview $VIM/hobbes/hobbes.txt <CR><C-w>H<CR>
"
"
"
"****************************************************************************
"****************************************************************************
"
"
" 
"File     :elflady.vim
"Required :vim
"Language :ex
"Title    :elflady colour scheme
"Author   :Roy Mathur, Roy.Mathur@gmail.com, http://www.roymathur.com
"Notes    :Based on Ron Aaron's elflord; changed to make it easier on my
"          eyes and to, somewhat, make both GUI and colour terminal
"          versions look (almost) the same.
"Version  :v 1.00 Fri 30 Jan 2009
"          ----------------------
"          Changes from elflord:
"            Statements are blue
"            Line numbers are red
"            The Cursor line is dark red
"            VertSplit, StatusLine, StatusLineNC are Cyan to overwrite any
"            other default settings.
"          Probably not perfect, but works me.
"          Tested on following versions:
"            VIM - Vi IMproved 7.2 (2008 Aug 9, compiled Aug  9 2008 18:36:44) 32-bit MS-DOS version Big version without GUI.
"            VIM - Vi IMproved 7.1 (2007 May 12, compiled May 12 2007 14:14:51) MS-Windows 32 bit console version Compiled by Bram@KIBAALE Big version without GUI.
"            VIM - Vi IMproved 7.1 (2007 May 12, compiled Jul 28 2007 10:34:35) MS-Windows 32 bit GUI version Included patches: 1-42 Compiled by rck@none Big version with GUI. 
"          DOS testing, using DOSBox 0.72 on Win32
"
hi clear
syntax reset
"
"GUI
hi Normal guifg=Cyan guibg=Black
hi Comment guifg=DarkGreen "DarkCyan
hi Constant guifg=Magenta
hi Special guifg=Magenta
hi Identifier guifg=Cyan
hi Statement guifg=Blue
hi PreProc guifg=LightBlue
hi Type guifg=LightGreen
hi Function guifg=White
hi Repeat guifg=White
hi Operator guifg=Red
hi Ignore guifg=Black
hi Error guibg=Red
hi Todo guibg=Yellow
hi lineNr guifg=Red
hi CursorLine guibg=DarkRed
hi Cursor guibg=Cyan
hi VertSplit guifg=Cyan
hi StatusLine guifg=Cyan
hi StatusLineNC guifg=Cyan
hi NonText guifg=DarkBlue
"
"Colour Terminal
hi Normal ctermfg=Cyan
hi Comment ctermfg=DarkCyan
hi Constant ctermfg=Magenta
hi Special ctermfg=DarkMagenta
hi Identifier ctermfg=Cyan
hi Statement ctermfg=Blue
hi PreProc ctermfg=LightBlue
hi Type ctermfg=LightGreen
hi Function ctermfg=White
hi Repeat ctermfg=White
hi Operator ctermfg=Red
hi Ignore ctermfg=Black
hi Error ctermbg=Red
hi Todo ctermbg=Yellow
hi lineNr ctermfg=Red
hi Cursor ctermbg=Cyan
hi CursorLine ctermbg=DarkRed
hi VertSplit ctermbg=Cyan
hi StatusLine ctermbg=Cyan
hi StatusLineNC ctermbg=Cyan
"
"Non-Colour Terminal
hi Comment term=bold 
hi Constant term=underline
hi Special term=bold
hi Identifier term=underline
hi Statement term=bold
hi PreProc term=underline
hi Type term=underline
hi Function term=bold
hi Repeat term=underline
hi Error term=reverse
hi Todo term=standout
"
"Syntax
hi link String Constant
hi link Character Constant
hi link Number Constant
hi link Boolean Constant
hi link Float Number
hi link Conditional Repeat
hi link Label Statement
hi link Keyword Statement
hi link Exception Statement
hi link Include PreProc
hi link Define PreProc
hi link Macro PreProc
hi link PreCondit PreProc
hi link StorageClass Type
hi link Structure Type
hi link Typedef Type
hi link Tag Special
hi link SpecialChar Special
hi link Delimiter Special
hi link SpecialComment Special
hi link Debug Special
