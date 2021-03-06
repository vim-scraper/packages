" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
autoload/conque_term.vim	[[[1
1504
" FILE:     autoload/conque_term.vim {{{
" AUTHOR:   Nico Raffo <nicoraffo@gmail.com>
" WEBSITE:  http://conque.googlecode.com
" MODIFIED: 2011-08-12
" VERSION:  2.2, for Vim 7.0
" LICENSE:
" Conque - Vim terminal/console emulator
" Copyright (C) 2009-__YEAR__ Nico Raffo 
"
" MIT License
" 
" Permission is hereby granted, free of charge, to any person obtaining a copy
" of this software and associated documentation files (the "Software"), to deal
" in the Software without restriction, including without limitation the rights
" to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
" copies of the Software, and to permit persons to whom the Software is
" furnished to do so, subject to the following conditions:
" 
" The above copyright notice and this permission notice shall be included in
" all copies or substantial portions of the Software.
" 
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
" IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
" FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
" AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
" LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
" OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
" THE SOFTWARE.
" }}}

" **********************************************************************************************************
" **** GLOBAL INITIALIZATION *******************************************************************************
" **********************************************************************************************************

" {{{

" load plugin file if it hasn't already been loaded (e.g. conque_term#foo() is used in .vimrc)
if !exists('g:ConqueTerm_Loaded')
    runtime! plugin/conque_term.vim
endif

" path to conque install directories
let s:scriptdir = expand("<sfile>:h") . '/'
let s:scriptdirpy = expand("<sfile>:h") . '/conque_term/'

" global list of terminal instances
let s:term_obj = {'idx': 1, 'var': '', 'is_buffer': 1, 'active': 1, 'buffer_name': '', 'command': ''}
let g:ConqueTerm_Terminals = {}

" global lists of registered functions
let s:hooks = { 'after_startup': [], 'buffer_enter': [], 'buffer_leave': [], 'after_keymap': [] }

" required for session support
if g:ConqueTerm_SessionSupport == 1
    set sessionoptions+=globals
    try
        sil! let s:saved_terminals = eval(g:ConqueTerm_TerminalsString)
    catch
        let s:saved_terminals = {}
    endtry
endif

" more session support
let g:ConqueTerm_TerminalsString = ''

" init terminal counter
let g:ConqueTerm_Idx = 0

" we clobber this value later
let s:save_updatetime = &updatetime

" have we called the init() function yet?
let s:initialized = 0


" }}}

" **********************************************************************************************************
" **** SYSTEM DETECTION ************************************************************************************
" **********************************************************************************************************

" {{{

" Display various error messages
function! conque_term#fail(feature) " {{{

    " create a new buffer
    new
    setlocal buftype=nofile
    setlocal nonumber
    setlocal foldcolumn=0
    setlocal wrap
    setlocal noswapfile

    " missing vim features
    if a:feature == 'python'

        call append('$', 'Conque ERROR: Python interface cannot be loaded')
        call append('$', '')

        if !executable("python")
            call append('$', 'Your version of Vim appears to be installed without the Python interface. In ')
            call append('$', 'addition, you may need to install Python.')
        else
            call append('$', 'Your version of Vim appears to be installed without the Python interface.')
        endif

        call append('$', '')

        if has('unix') == 1
            call append('$', "You are using a Unix-like operating system. Most, if not all, of the popular ")
            call append('$', "Linux package managers have Python-enabled Vim available. For example ")
            call append('$', "vim-gnome or vim-gtk on Ubuntu will get you everything you need.")
            call append('$', "")
            call append('$', "If you are compiling Vim from source, make sure you use the --enable-pythoninterp ")
            call append('$', "configure option. You will also need to install Python and the Python headers.")
            call append('$', "")
            call append('$', "If you are using OS X, MacVim will give you Python support by default.")
        else
            call append('$', "You appear to be using Windows. The official Vim 7.3 installer available at ")
            call append('$', "http://www.vim.org comes with the required Python interfaces. You will also ")
            call append('$', "need to install Python 2.7 and/or Python 3.1, both available at http://www.python.org")
        endif

    elseif a:feature == 'python_exe'

        call append('$', "Conque ERROR: Can't find Python executable")
        call append('$', "")
        call append('$', "Conque needs to know the full path to python.exe on Windows systems. By default, ")
        call append('$', "Conque will check your system path as well as the most common installation path ")
        call append('$', "C:\\PythonXX\\python.exe. To fix this error either:")
        call append('$', "")
        call append('$', "Set the g:ConqueTerm_PyExe option in your .vimrc. E.g.")
        call append('$', "        let g:ConqueTerm_PyExe = 'C:\Program Files\Python27\python.exe'")
        call append('$', "")
        call append('$', "Add the directory where you installed python to your system path. This isn't a bad ")
        call append('$', "idea in general.")

    elseif a:feature == 'ctypes'

        call append('$', 'Conque ERROR: Python cannot load the ctypes module')
        call append('$', "")
        call append('$', "Conque requires the 'ctypes' python module. This has been a standard module since Python 2.5.")
        call append('$', "")
        call append('$', "The recommended fix is to make sure you're using the latest official GVim version 7.3, ")
        call append('$', "and have at least one of the two compatible versions of Python installed, ")
        call append('$', "2.7 or 3.1. You can download the GVim 7.3 installer from http://www.vim.org. You ")
        call append('$', "can download the Python 2.7 or 3.1 installer from http://www.python.org")

    endif

endfunction " }}}

" Go through various system checks before attempting to launch conque
function! conque_term#dependency_check() " {{{

    " don't recheck the second time 'round
    if s:initialized == 1
        return 1
    endif

    " choose a python version
    let s:py = ''
    if g:ConqueTerm_PyVersion == 3
        let pytest = 'python3'
    else
        let pytest = 'python'
        let g:ConqueTerm_PyVersion = 2
    endif

    " first test the requested version
    if has(pytest)
        if pytest == 'python3'
            let s:py = 'py3'
        else
            let s:py = 'py'
        endif

    " otherwise use the other version
    else
        let py_alternate = 5 - g:ConqueTerm_PyVersion
        if py_alternate == 3
            let pytest = 'python3'
        else
            let pytest = 'python'
        endif
        if has(pytest)
            echohl WarningMsg | echomsg "Python " . g:ConqueTerm_PyVersion . " interface is not installed, using Python " . py_alternate . " instead" | echohl None
            let g:ConqueTerm_PyVersion = py_alternate
            if pytest == 'python3'
                let s:py = 'py3'
            else
                let s:py = 'py'
            endif
        endif
    endif

    " test if we actually found a python version
    if s:py == ''
        call conque_term#fail('python')
        return 0
    endif

    " quick and dirty platform declaration
    if has('unix') == 1
        let s:platform = 'unix'
        sil exe s:py . " CONQUE_PLATFORM = 'unix'"
    else
        let s:platform = 'windows'
        sil exe s:py . " CONQUE_PLATFORM = 'windows'"
    endif

    " if we're using Windows, make sure ctypes is available
    if s:platform == 'windows'
        try
            sil exe s:py . " import ctypes"
        catch
            call conque_term#fail('ctypes')
            return 0
        endtry
    endif

    " if we're using Windows, make sure we can finde python executable
    if s:platform == 'windows' && conque_term#find_python_exe() == ''
        call conque_term#fail('python_exe')
        return 0
    endif

    " check for global cursorhold/cursormove events
    let o = ''
    silent redir => o
    silent autocmd CursorHoldI,CursorMovedI
    redir END
    for line in split(o, "\n")
        if line =~ '^ ' || line =~ '^--' || line =~ 'matchparen'
            continue
        endif
        if g:ConqueTerm_StartMessages
            echohl WarningMsg | echomsg "Warning: Global CursorHoldI and CursorMovedI autocommands may cause ConqueTerm to run slowly." | echohl None
        endif
    endfor

    " check for compatible mode
    if &compatible == 1
        echohl WarningMsg | echomsg "Warning: Conque may not function normally in 'compatible' mode." | echohl None
    endif

    " check for fast mode
    if g:ConqueTerm_FastMode
        sil exe s:py . " CONQUE_FAST_MODE = True"
    else
        sil exe s:py . " CONQUE_FAST_MODE = False"
    endif

    " if we're all good, load python files
    call conque_term#load_python()

    return 1

endfunction " }}}

" }}}

" **********************************************************************************************************
" **** STARTUP MESSAGES ************************************************************************************
" **********************************************************************************************************

" {{{
"if g:ConqueTerm_StartMessages
"    let msg_file = s:scriptdirpy . 'version.vim'
"    let msg_show = 1
"    let msg_ct = 1
"
"    " we can write to conque_term directory
"    if filewritable(s:scriptdirpy) == 2
"
"        if filewritable(msg_file)
"
"            " read current message file
"            try
"                silent execute "source " . msg_file
"                if exists('g:ConqueTerm_MsgCt') && exists('g:ConqueTerm_MsgVer')
"                    if g:ConqueTerm_MsgVer == g:ConqueTerm_Version && g:ConqueTerm_MsgCt > 2
"                        let msg_show = 0
"                    else
"                        let msg_ct = g:ConqueTerm_MsgCt + 1
"                    endif
"                endif
"            catch
"            endtry
"        endif
"
"        " update message file
"        if msg_show
"            let file_contents = ['let g:ConqueTerm_MsgCt = ' . msg_ct, 'let g:ConqueTerm_MsgVer = ' . g:ConqueTerm_Version]
"            call writefile(file_contents, msg_file)
"        endif
"    endif
"
"    " save our final decision
"    let g:ConqueTerm_StartMessages = msg_show
"endif
" }}}

" **********************************************************************************************************
" **** WINDOWS VK CODES ************************************************************************************
" **********************************************************************************************************

" Windows Virtual Key Codes  {{{
let s:windows_vk = {
\    'VK_ADD' : 107,
\    'VK_APPS' : 93,
\    'VK_ATTN' : 246,
\    'VK_BACK' : 8,
\    'VK_BROWSER_BACK' : 166,
\    'VK_BROWSER_FORWARD' : 167,
\    'VK_CANCEL' : 3,
\    'VK_CAPITAL' : 20,
\    'VK_CLEAR' : 12,
\    'VK_CONTROL' : 17,
\    'VK_CONVERT' : 28,
\    'VK_CRSEL' : 247,
\    'VK_DECIMAL' : 110,
\    'VK_DELETE' : 46,
\    'VK_DIVIDE' : 111,
\    'VK_DOWN' : 40,
\    'VK_DOWN_CTL' : '40;1024',
\    'VK_END' : 35,
\    'VK_EREOF' : 249,
\    'VK_ESCAPE' : 27,
\    'VK_EXECUTE' : 43,
\    'VK_EXSEL' : 248,
\    'VK_F1' : 112,
\    'VK_F10' : 121,
\    'VK_F11' : 122,
\    'VK_F12' : 123,
\    'VK_F13' : 124,
\    'VK_F14' : 125,
\    'VK_F15' : 126,
\    'VK_F16' : 127,
\    'VK_F17' : 128,
\    'VK_F18' : 129,
\    'VK_F19' : 130,
\    'VK_F2' : 113,
\    'VK_F20' : 131,
\    'VK_F21' : 132,
\    'VK_F22' : 133,
\    'VK_F23' : 134,
\    'VK_F24' : 135,
\    'VK_F3' : 114,
\    'VK_F4' : 115,
\    'VK_F5' : 116,
\    'VK_F6' : 117,
\    'VK_F7' : 118,
\    'VK_F8' : 119,
\    'VK_F9' : 120,
\    'VK_FINAL' : 24,
\    'VK_HANGEUL' : 21,
\    'VK_HANGUL' : 21,
\    'VK_HANJA' : 25,
\    'VK_HELP' : 47,
\    'VK_HOME' : 36,
\    'VK_INSERT' : 45,
\    'VK_JUNJA' : 23,
\    'VK_KANA' : 21,
\    'VK_KANJI' : 25,
\    'VK_LBUTTON' : 1,
\    'VK_LCONTROL' : 162,
\    'VK_LEFT' : 37,
\    'VK_LEFT_CTL' : '37;1024',
\    'VK_LMENU' : 164,
\    'VK_LSHIFT' : 160,
\    'VK_LWIN' : 91,
\    'VK_MBUTTON' : 4,
\    'VK_MEDIA_NEXT_TRACK' : 176,
\    'VK_MEDIA_PLAY_PAUSE' : 179,
\    'VK_MEDIA_PREV_TRACK' : 177,
\    'VK_MENU' : 18,
\    'VK_MODECHANGE' : 31,
\    'VK_MULTIPLY' : 106,
\    'VK_NEXT' : 34,
\    'VK_NONAME' : 252,
\    'VK_NONCONVERT' : 29,
\    'VK_NUMLOCK' : 144,
\    'VK_NUMPAD0' : 96,
\    'VK_NUMPAD1' : 97,
\    'VK_NUMPAD2' : 98,
\    'VK_NUMPAD3' : 99,
\    'VK_NUMPAD4' : 100,
\    'VK_NUMPAD5' : 101,
\    'VK_NUMPAD6' : 102,
\    'VK_NUMPAD7' : 103,
\    'VK_NUMPAD8' : 104,
\    'VK_NUMPAD9' : 105,
\    'VK_OEM_CLEAR' : 254,
\    'VK_PA1' : 253,
\    'VK_PAUSE' : 19,
\    'VK_PLAY' : 250,
\    'VK_PRINT' : 42,
\    'VK_PRIOR' : 33,
\    'VK_PROCESSKEY' : 229,
\    'VK_RBUTTON' : 2,
\    'VK_RCONTROL' : 163,
\    'VK_RETURN' : 13,
\    'VK_RIGHT' : 39,
\    'VK_RIGHT_CTL' : '39;1024',
\    'VK_RMENU' : 165,
\    'VK_RSHIFT' : 161,
\    'VK_RWIN' : 92,
\    'VK_SCROLL' : 145,
\    'VK_SELECT' : 41,
\    'VK_SEPARATOR' : 108,
\    'VK_SHIFT' : 16,
\    'VK_SNAPSHOT' : 44,
\    'VK_SPACE' : 32,
\    'VK_SUBTRACT' : 109,
\    'VK_TAB' : 9,
\    'VK_UP' : 38,
\    'VK_UP_CTL' : '38;1024',
\    'VK_VOLUME_DOWN' : 174,
\    'VK_VOLUME_MUTE' : 173,
\    'VK_VOLUME_UP' : 175,
\    'VK_XBUTTON1' : 5,
\    'VK_XBUTTON2' : 6,
\    'VK_ZOOM' : 251
\   }
" }}}

" **********************************************************************************************************
" **** ACTUAL CONQUE FUNCTIONS!  ***************************************************************************
" **********************************************************************************************************

" {{{

" launch conque
function! conque_term#open(...) "{{{
    let command = get(a:000, 0, '')
    let vim_startup_commands = get(a:000, 1, [])
    let return_to_current  = get(a:000, 2, 0)
    let is_buffer  = get(a:000, 3, 1)

    " dependency check
    if !conque_term#dependency_check()
        return 0
    endif

    " switch to buffer if needed
    if is_buffer && return_to_current
      let save_sb = &switchbuf
      sil set switchbuf=usetab
      let current_buffer = bufname("%")
    endif

    " bare minimum validation
    if s:py == ''
        echohl WarningMsg | echomsg "Conque requires the Python interface to be installed. See :help ConqueTerm for more information." | echohl None
        return 0
    endif
    if empty(command)
        echohl WarningMsg | echomsg "Invalid usage: no program path given. Use :ConqueTerm YOUR PROGRAM, e.g. :ConqueTerm ipython" | echohl None
        return 0
    else
        let cmd_args = split(command, '[^\\]\@<=\s')
        let cmd_args[0] = substitute(cmd_args[0], '\\ ', ' ', 'g')
        if !executable(cmd_args[0])
            echohl WarningMsg | echomsg "Not an executable: " . cmd_args[0] | echohl None
            return 0
        endif
    endif

    " initialize global identifiers
    let g:ConqueTerm_Idx += 1
    let g:ConqueTerm_Var = 'ConqueTerm_' . g:ConqueTerm_Idx
    let g:ConqueTerm_BufName = substitute(command, ' ', '\\ ', 'g') . "\\ -\\ " . g:ConqueTerm_Idx

    " initialize global mappings if needed
    call conque_term#init()

    " set Vim buffer window options
    if is_buffer
        call conque_term#set_buffer_settings(command, vim_startup_commands)

        let b:ConqueTerm_Idx = g:ConqueTerm_Idx
        let b:ConqueTerm_Var = g:ConqueTerm_Var
    endif

    " save terminal instance
    let t_obj = conque_term#create_terminal_object(g:ConqueTerm_Idx, is_buffer, g:ConqueTerm_BufName, command)
    let g:ConqueTerm_Terminals[g:ConqueTerm_Idx] = t_obj

    " required for session support
    let g:ConqueTerm_TerminalsString = string(g:ConqueTerm_Terminals)

    " open command
    try
        let options = {}
        let options["TERM"] = g:ConqueTerm_TERM
        let options["CODE_PAGE"] = g:ConqueTerm_CodePage
        let options["color"] = g:ConqueTerm_Color
        let options["offset"] = 0 " g:ConqueTerm_StartMessages * 10

        if s:platform == 'unix'
            execute s:py . ' ' . g:ConqueTerm_Var . ' = Conque()'
            execute s:py . ' ' . g:ConqueTerm_Var . ".open()"
        else
            " find python.exe and communicator
            let py_exe = conque_term#find_python_exe()
            let py_vim = s:scriptdirpy . 'conque_sole_communicator.py'
            execute s:py . ' ' . g:ConqueTerm_Var . ' = ConqueSole()'
            execute s:py . ' ' . g:ConqueTerm_Var . ".open()"

            if g:ConqueTerm_ColorMode == 'conceal'
                call conque_term#init_conceal_color()
            endif
        endif
    catch
        echohl WarningMsg | echomsg "An error occurred: " . command | echohl None
        return 0
    endtry

    " set key mappings and auto commands 
    if is_buffer
        call conque_term#set_mappings('start')
    endif

    " call user defined functions
    call conque_term#call_hooks('after_startup', t_obj)

    " switch to buffer if needed
    if is_buffer && return_to_current
        sil exe ":sb " . current_buffer
        sil exe ":set switchbuf=" . save_sb
    elseif is_buffer
        startinsert!
    endif

    return t_obj

endfunction "}}}

" open(), but no buffer
function! conque_term#subprocess(command) " {{{
    
    let t_obj = conque_term#open(a:command, [], 0, 0)
    if !exists('b:ConqueTerm_Var')
        call conque_term#on_blur()
        sil exe s:py . ' ' . g:ConqueTerm_Var . '.idle()'
    endif
    return t_obj

endfunction " }}}

" set buffer options
function! conque_term#set_buffer_settings(command, vim_startup_commands) "{{{

    " optional hooks to execute, e.g. 'split'
    for h in a:vim_startup_commands
        sil exe h
    endfor
    sil exe 'edit ++enc=utf-8 ' . g:ConqueTerm_BufName

    " buffer settings 
    setlocal fileencoding=utf-8 " file encoding, even tho there's no file
    setlocal nopaste           " conque won't work in paste mode
    setlocal buftype=nofile    " this buffer is not a file, you can't save it
    setlocal nonumber          " hide line numbers
    if v:version >= 703
        setlocal norelativenumber " hide relative line numbers (VIM >= 7.3)
    endif
    setlocal foldcolumn=0      " reasonable left margin
    setlocal nowrap            " default to no wrap (esp with MySQL)
    setlocal noswapfile        " don't bother creating a .swp file
    setlocal scrolloff=0       " don't use buffer lines. it makes the 'clear' command not work as expected
    setlocal sidescrolloff=0   " don't use buffer lines. it makes the 'clear' command not work as expected
    setlocal sidescroll=1      " don't use buffer lines. it makes the 'clear' command not work as expected
    setlocal foldmethod=manual " don't fold on {{{}}} and stuff
    setlocal bufhidden=hide    " when buffer is no longer displayed, don't wipe it out
    setlocal noreadonly        " this is not actually a readonly buffer
    if v:version >= 703
        setlocal conceallevel=3
        setlocal concealcursor=nic
    endif
    if g:ConqueTerm_ReadUnfocused
        set cpoptions+=I       " Don't remove autoindent when moving cursor up and down
    endif
    setfiletype conque_term    " useful
    sil exe "setlocal syntax=" . g:ConqueTerm_Syntax

    " temporary global settings go in here
    call conque_term#on_focus(1)

endfunction " }}}

" send normal character key press to terminal
function! conque_term#key_press() "{{{
    sil exe s:py . ' ' . b:ConqueTerm_Var . ".write_buffered_ord(" . char2nr(v:char) . ")"
    sil let v:char = ''
endfunction " }}}

" set key mappings and auto commands
function! conque_term#set_mappings(action) "{{{

    " set action {{{
    if a:action == 'toggle'
        if exists('b:conque_on') && b:conque_on == 1
            let l:action = 'stop'
            echohl WarningMsg | echomsg "Terminal is paused" | echohl None
        else
            let l:action = 'start'
            echohl WarningMsg | echomsg "Terminal is resumed" | echohl None
        endif
    else
        let l:action = a:action
    endif

    " if mappings are being removed, add 'un'
    let map_modifier = 'nore'
    if l:action == 'stop'
        let map_modifier = 'un'
    endif
    " }}}

    " auto commands {{{
    if l:action == 'stop'
        sil exe 'autocmd! ' . b:ConqueTerm_Var

    else
        sil exe 'augroup ' . b:ConqueTerm_Var

        " handle unexpected closing of shell, passes HUP to parent and all child processes
        sil exe 'autocmd ' . b:ConqueTerm_Var . ' BufUnload <buffer> ' . s:py . ' ' . b:ConqueTerm_Var . '.close()'

        " check for resized/scrolled buffer when entering buffer
        sil exe 'autocmd ' . b:ConqueTerm_Var . ' BufEnter <buffer> ' . s:py . ' ' . b:ConqueTerm_Var . '.update_window_size()'
        sil exe 'autocmd ' . b:ConqueTerm_Var . ' VimResized ' . s:py . ' ' . b:ConqueTerm_Var . '.update_window_size()'

        " set/reset updatetime on entering/exiting buffer
        sil exe 'autocmd ' . b:ConqueTerm_Var . ' BufEnter <buffer> call conque_term#on_focus()'
        sil exe 'autocmd ' . b:ConqueTerm_Var . ' BufLeave <buffer> call conque_term#on_blur()'

        " reposition cursor when going into insert mode
        sil exe 'autocmd ' . b:ConqueTerm_Var . ' InsertEnter <buffer> ' . s:py . ' ' . b:ConqueTerm_Var . '.insert_enter()'

        " poll for more output
        sil exe 'autocmd ' . b:ConqueTerm_Var . ' CursorHoldI <buffer> ' . s:py . ' ' .  b:ConqueTerm_Var . '.auto_read()'
    endif
    " }}}

    " map ASCII 1-31 {{{
    for c in range(1, 31)
        " <Esc>
        if c == 27 || c == 3
            continue
        endif
        if l:action == 'start'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <C-' . nr2char(64 + c) . '> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_ord(' . c . ')<CR>'
        else
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <C-' . nr2char(64 + c) . '>'
        endif
    endfor
    " bonus mapping: send <C-c> in normal mode to terminal as well for panic interrupts
    if l:action == 'start'
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <C-c> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_ord(3)<CR>'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> <C-c> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_ord(3)<CR>'
    else
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <C-c>'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> <C-c>'
    endif

    " leave insert mode
    if !exists('g:ConqueTerm_EscKey') || g:ConqueTerm_EscKey == '<Esc>'
        " use <Esc><Esc> to send <Esc> to terminal
        if l:action == 'start'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Esc><Esc> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_ord(27)<CR>'
        else
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Esc><Esc>'
        endif
    else
        " use <Esc> to send <Esc> to terminal
        if l:action == 'start'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> ' . g:ConqueTerm_EscKey . ' <Esc>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Esc> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_ord(27)<CR>'
        else
            sil exe 'i' . map_modifier . 'map <silent> <buffer> ' . g:ConqueTerm_EscKey
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Esc>'
        endif
    endif

    " Map <C-w> in insert mode
    if exists('g:ConqueTerm_CWInsert') && g:ConqueTerm_CWInsert == 1
        inoremap <silent> <buffer> <C-w> <Esc><C-w>
    endif
    " }}}

    " map 33 and beyond {{{
    if exists('##InsertCharPre') && g:ConqueTerm_InsertCharPre == 1
        if l:action == 'start'
            autocmd InsertCharPre <buffer> call conque_term#key_press()
        else
            autocmd! InsertCharPre <buffer>
        endif
    else
        for i in range(33, 127)
            " <Bar>
            if i == 124
                if l:action == 'start'
                    sil exe "i" . map_modifier . "map <silent> <buffer> <Bar> <C-o>:" . s:py . ' ' . b:ConqueTerm_Var . ".write_ord(124)<CR>"
                else
                    sil exe "i" . map_modifier . "map <silent> <buffer> <Bar>"
                endif
                continue
            endif
            if l:action == 'start'
                sil exe "i" . map_modifier . "map <silent> <buffer> " . nr2char(i) . " <C-o>:" . s:py . ' ' . b:ConqueTerm_Var . ".write_ord(" . i . ")<CR>"
            else
                sil exe "i" . map_modifier . "map <silent> <buffer> " . nr2char(i)
            endif
        endfor
    endif
    " }}}

    " Special keys {{{
    if l:action == 'start'
        if s:platform == 'unix'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <BS> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u("\x08"))<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Space> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u(" "))<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <S-BS> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u("\x08"))<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <S-Space> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u(" "))<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Up> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u("\x1b[A"))<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Down> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u("\x1b[B"))<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Right> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u("\x1b[C"))<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Left> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u("\x1b[D"))<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Home> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u("\x1bOH"))<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <End> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u("\x1bOF"))<CR>'
        else
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <BS> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u("\x08"))<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Space> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u(" "))<CR>'

            sil exe 'i' . map_modifier . 'map <silent> <buffer> <S-BS> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u("\x08"))<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <S-Space> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u(" "))<CR>'

            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Up> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_UP . ')<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Down> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_DOWN . ')<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Right> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_RIGHT . ')<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Left> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_LEFT . ')<CR>'

            sil exe 'i' . map_modifier . 'map <silent> <buffer> <C-Up> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk("' . s:windows_vk.VK_UP_CTL . '")<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <C-Down> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk("' . s:windows_vk.VK_DOWN_CTL . '")<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <C-Right> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk("' . s:windows_vk.VK_RIGHT_CTL . '")<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <C-Left> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk("' . s:windows_vk.VK_LEFT_CTL . '")<CR>'

            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Del> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_DELETE . ')<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Home> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_HOME . ')<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <End> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_END . ')<CR>'
        endif
    else
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <BS>'
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <Space>'
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <S-BS>'
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <S-Space>'
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <Up>'
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <Down>'
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <Right>'
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <Left>'
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <Home>'
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <End>'
    endif
    " }}}

    " <F-> keys {{{
    if g:ConqueTerm_SendFunctionKeys
        if l:action == 'start'
            if s:platform == 'unix'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F1>  <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u("\x1b[11~"))<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F2>  <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u("\x1b[12~"))<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F3>  <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u("1b[13~"))<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F4>  <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u("\x1b[14~"))<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F5>  <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u("\x1b[15~"))<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F6>  <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u("\x1b[17~"))<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F7>  <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u("\x1b[18~"))<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F8>  <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u("\x1b[19~"))<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F9>  <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u("\x1b[20~"))<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F10> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u("\x1b[21~"))<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F11> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u("\x1b[23~"))<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F12> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(u("\x1b[24~"))<CR>'
            else
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F1> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_F1 . ')<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F2> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_F2 . ')<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F3> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_F3 . ')<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F4> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_F4 . ')<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F5> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_F5 . ')<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F6> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_F6 . ')<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F7> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_F7 . ')<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F8> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_F8 . ')<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F9> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_F9 . ')<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F10> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_F10 . ')<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F11> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_F11 . ')<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F12> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_F12 . ')<CR>'
            endif
        else
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <F1>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <F2>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <F3>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <F4>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <F5>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <F6>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <F7>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <F8>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <F9>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <F10>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <F11>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <F12>'
        endif
    endif
    " }}}

    " various global mappings {{{
    " don't overwrite existing mappings
    if l:action == 'start'
        if maparg(g:ConqueTerm_SendVisKey, 'v') == ''
          sil exe 'v' . map_modifier . 'map <silent> ' . g:ConqueTerm_SendVisKey . ' :<C-u>call conque_term#send_selected(visualmode())<CR>'
        endif
        if maparg(g:ConqueTerm_SendFileKey, 'n') == ''
          sil exe 'n' . map_modifier . 'map <silent> ' . g:ConqueTerm_SendFileKey . ' :<C-u>call conque_term#send_file()<CR>'
        endif
    endif
    " }}}

    " remap paste keys {{{
    if l:action == 'start'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> p :' . s:py . ' ' . b:ConqueTerm_Var . '.write_expr("@@")<CR>a'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> P :' . s:py . ' ' . b:ConqueTerm_Var . '.write_expr("@@")<CR>a'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> ]p :' . s:py . ' ' . b:ConqueTerm_Var . '.write_expr("@@")<CR>a'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> [p :' . s:py . ' ' . b:ConqueTerm_Var . '.write_expr("@@")<CR>a'
    else
        sil exe 'n' . map_modifier . 'map <silent> <buffer> p'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> P'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> ]p'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> [p'
    endif
    if has('gui_running') == 1
        if l:action == 'start'
            sil exe 'i' . map_modifier . 'map <buffer> <S-Insert> <Esc>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_expr("@+")<CR>a'
            sil exe 'i' . map_modifier . 'map <buffer> <S-Help> <Esc>:<C-u>' . s:py . ' ' . b:ConqueTerm_Var . '.write_expr("@+")<CR>a'
        else
            sil exe 'i' . map_modifier . 'map <buffer> <S-Insert>'
            sil exe 'i' . map_modifier . 'map <buffer> <S-Help>'
        endif
    endif
    " }}}

    " disable other normal mode keys which insert text {{{
    if l:action == 'start'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> r :echo "Replace mode disabled in shell."<CR>'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> R :echo "Replace mode disabled in shell."<CR>'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> c :echo "Change mode disabled in shell."<CR>'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> C :echo "Change mode disabled in shell."<CR>'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> s :echo "Change mode disabled in shell."<CR>'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> S :echo "Change mode disabled in shell."<CR>'
    else
        sil exe 'n' . map_modifier . 'map <silent> <buffer> r'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> R'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> c'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> C'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> s'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> S'
    endif
    " }}}

    " set conque as on or off {{{
    if l:action == 'start'
        let b:conque_on = 1
    else
        let b:conque_on = 0
    endif
    " }}}

    " map command to toggle terminal key mappings {{{
    if a:action == 'start'
        sil exe 'nnoremap ' . g:ConqueTerm_ToggleKey . ' :<C-u>call conque_term#set_mappings("toggle")<CR>'
    endif
    " }}}

    " call user defined functions
    if l:action == 'start'
        call conque_term#call_hooks('after_keymap', conque_term#get_instance())
    endif

endfunction " }}}

" Initialize global mappings. Should only be called once per Vim session
function! conque_term#init() " {{{

    if s:initialized == 1
        return
    endif

    augroup ConqueTerm

    " abort any remaining running terminals when Vim exits
    autocmd ConqueTerm VimLeave * call conque_term#close_all()

    " read more output when this isn't the current buffer
    if g:ConqueTerm_ReadUnfocused == 1
        autocmd ConqueTerm CursorHold * call conque_term#read_all(0)
    endif

    let s:initialized = 1

endfunction " }}}

" read from all known conque buffers
function! conque_term#read_all(insert_mode) "{{{

    for i in range(1, g:ConqueTerm_Idx)
        try
            if !g:ConqueTerm_Terminals[i].active
                continue
            endif

            let output = g:ConqueTerm_Terminals[i].read(1)

            if !g:ConqueTerm_Terminals[i].is_buffer && exists('*g:ConqueTerm_Terminals[i].callback')
                call g:ConqueTerm_Terminals[i].callback(output)
            endif
        catch
            " probably a deleted buffer
        endtry
    endfor

    " restart updatetime
    if a:insert_mode
        "call feedkeys("\<C-o>f\e", "n")
        let p = getpos('.')
        if p[1] == 1
          sil exe 'call feedkeys("\<Down>\<Up>", "n")'
        else
          sil exe 'call feedkeys("\<Up>\<Down>", "n")'
        endif
        call setpos('.', p)
    else
        call feedkeys("f\e", "n")
    endif

endfunction "}}}

" close all subprocesses
function! conque_term#close_all() "{{{

    for i in range(1, g:ConqueTerm_Idx)
        try
            call g:ConqueTerm_Terminals[i].close()
        catch
            " probably a deleted buffer
        endtry
    endfor

endfunction "}}}

" gets called when user enters conque buffer.
" Useful for making temp changes to global config
function! conque_term#on_focus(...) " {{{

    let startup = get(a:000, 0, 0)

    " Disable NeoComplCache. It has global hooks on CursorHold and CursorMoved :-/
    let s:NeoComplCache_WasEnabled = exists(':NeoComplCacheLock')
    if s:NeoComplCache_WasEnabled == 2
        NeoComplCacheLock
    endif
 
    if g:ConqueTerm_ReadUnfocused == 1
        autocmd! ConqueTerm CursorHoldI *
        autocmd! ConqueTerm CursorHold *
    endif

    " set poll interval to 50ms
    set updatetime=50

    " resume subprocess fast polling
    if startup == 0 && exists('b:ConqueTerm_Var')
        sil exe s:py . ' ' . g:ConqueTerm_Var . '.resume()'
    endif

    " call user defined functions
    if startup == 0
        call conque_term#call_hooks('buffer_enter', conque_term#get_instance())
    endif

    " if configured, go into insert mode
    if g:ConqueTerm_InsertOnEnter == 1
        startinsert!
    endif

endfunction " }}}

" gets called when user exits conque buffer.
" Useful for resetting changes to global config
function! conque_term#on_blur() " {{{
    " re-enable NeoComplCache if needed
    if exists('s:NeoComplCache_WasEnabled') && exists(':NeoComplCacheUnlock') && s:NeoComplCache_WasEnabled == 2
        NeoComplCacheUnlock
    endif

    " turn off subprocess fast polling
    if exists('b:ConqueTerm_Var')
        sil exe s:py . ' ' . b:ConqueTerm_Var . '.idle()'
    endif

    " reset poll interval
    if g:ConqueTerm_ReadUnfocused == 1
        set updatetime=1000
        autocmd ConqueTerm CursorHoldI * call conque_term#read_all(1)
        autocmd ConqueTerm CursorHold * call conque_term#read_all(0)
    elseif exists('s:save_updatetime')
        exe 'set updatetime=' . s:save_updatetime
    else
        set updatetime=2000
    endif

    " call user defined functions
    call conque_term#call_hooks('buffer_leave', conque_term#get_instance())

endfunction " }}}

" bell event (^G)
function! conque_term#bell() " {{{
    echohl WarningMsg | echomsg "BELL!" | echohl None
endfunction " }}}

" register function to be called at conque events
function! conque_term#register_function(event, function_name) " {{{

    if !has_key(s:hooks, a:event)
        echomsg 'No such event: ' . a:event
        return
    endif

    if !exists('*' . a:function_name)
        echomsg 'No such function: ' . a:function_name)
        return
    endif

    " register the function
    call add(s:hooks[a:event], function(a:function_name))

endfunction " }}}

" call hooks for an event
function! conque_term#call_hooks(event, t_obj) " {{{

    for Fu in s:hooks[a:event]
        call Fu(a:t_obj)
    endfor

endfunction " }}}

" }}}

" **********************************************************************************************************
" **** Windows only functions ******************************************************************************
" **********************************************************************************************************

" {{{

" find python.exe in windows
function! conque_term#find_python_exe() " {{{

    " first check configuration for custom value
    if g:ConqueTerm_PyExe != '' && executable(g:ConqueTerm_PyExe)
        return g:ConqueTerm_PyExe
    endif

    let sys_paths = split($PATH, ';')

    " get exact python version
    sil exe ':' . s:py . ' import sys, vim'
    sil exe ':' . s:py . ' vim.command("let g:ConqueTerm_PyVersion = " + str(sys.version_info[0]) + str(sys.version_info[1]))'

    " ... and add to path list
    call add(sys_paths, 'C:\Python' . g:ConqueTerm_PyVersion)
    call reverse(sys_paths)

    " check if python.exe is in paths
    for path in sys_paths
        let cand = path . '\' . 'python.exe'
        if executable(cand)
            return cand
        endif
    endfor

    echohl WarningMsg | echomsg "Unable to find python.exe, see :help ConqueTerm_PythonExe for more information" | echohl None

    return ''

endfunction " }}}

" initialize concealed colors
function! conque_term#init_conceal_color() " {{{

    highlight link ConqueCCBG Normal

    " foreground colors, low intensity
    syn region ConqueCCF000 matchgroup=ConqueConceal start="\esf000;" end="\eef000;" concealends contains=ConqueCCBG
    syn region ConqueCCF00c matchgroup=ConqueConceal start="\esf00c;" end="\eef00c;" concealends contains=ConqueCCBG
    syn region ConqueCCF0c0 matchgroup=ConqueConceal start="\esf0c0;" end="\eef0c0;" concealends contains=ConqueCCBG
    syn region ConqueCCF0cc matchgroup=ConqueConceal start="\esf0cc;" end="\eef0cc;" concealends contains=ConqueCCBG
    syn region ConqueCCFc00 matchgroup=ConqueConceal start="\esfc00;" end="\eefc00;" concealends contains=ConqueCCBG
    syn region ConqueCCFc0c matchgroup=ConqueConceal start="\esfc0c;" end="\eefc0c;" concealends contains=ConqueCCBG
    syn region ConqueCCFcc0 matchgroup=ConqueConceal start="\esfcc0;" end="\eefcc0;" concealends contains=ConqueCCBG
    syn region ConqueCCFccc matchgroup=ConqueConceal start="\esfccc;" end="\eefccc;" concealends contains=ConqueCCBG

    " foreground colors, high intensity
    syn region ConqueCCF000 matchgroup=ConqueConceal start="\esf000;" end="\eef000;" concealends contains=ConqueCCBG
    syn region ConqueCCF00f matchgroup=ConqueConceal start="\esf00f;" end="\eef00f;" concealends contains=ConqueCCBG
    syn region ConqueCCF0f0 matchgroup=ConqueConceal start="\esf0f0;" end="\eef0f0;" concealends contains=ConqueCCBG
    syn region ConqueCCF0ff matchgroup=ConqueConceal start="\esf0ff;" end="\eef0ff;" concealends contains=ConqueCCBG
    syn region ConqueCCFf00 matchgroup=ConqueConceal start="\esff00;" end="\eeff00;" concealends contains=ConqueCCBG
    syn region ConqueCCFf0f matchgroup=ConqueConceal start="\esff0f;" end="\eeff0f;" concealends contains=ConqueCCBG
    syn region ConqueCCFff0 matchgroup=ConqueConceal start="\esfff0;" end="\eefff0;" concealends contains=ConqueCCBG
    syn region ConqueCCFfff matchgroup=ConqueConceal start="\esffff;" end="\eeffff;" concealends contains=ConqueCCBG

    " background colors, low intensity
    syn region ConqueCCB000 matchgroup=ConqueCCBG start="\esb000;" end="\eeb000;" concealends
    syn region ConqueCCB00c matchgroup=ConqueCCBG start="\esb00c;" end="\eeb00c;" concealends
    syn region ConqueCCB0c0 matchgroup=ConqueCCBG start="\esb0c0;" end="\eeb0c0;" concealends
    syn region ConqueCCB0cc matchgroup=ConqueCCBG start="\esb0cc;" end="\eeb0cc;" concealends
    syn region ConqueCCBc00 matchgroup=ConqueCCBG start="\esbc00;" end="\eebc00;" concealends
    syn region ConqueCCBc0c matchgroup=ConqueCCBG start="\esbc0c;" end="\eebc0c;" concealends
    syn region ConqueCCBcc0 matchgroup=ConqueCCBG start="\esbcc0;" end="\eebcc0;" concealends
    syn region ConqueCCBccc matchgroup=ConqueCCBG start="\esbccc;" end="\eebccc;" concealends

    " background colors, high intensity
    syn region ConqueCCB000 matchgroup=ConqueCCBG start="\esb000;" end="\eeb000;" concealends
    syn region ConqueCCB00f matchgroup=ConqueCCBG start="\esb00f;" end="\eeb00f;" concealends
    syn region ConqueCCB0f0 matchgroup=ConqueCCBG start="\esb0f0;" end="\eeb0f0;" concealends
    syn region ConqueCCB0ff matchgroup=ConqueCCBG start="\esb0ff;" end="\eeb0ff;" concealends
    syn region ConqueCCBf00 matchgroup=ConqueCCBG start="\esbf00;" end="\eebf00;" concealends
    syn region ConqueCCBf0f matchgroup=ConqueCCBG start="\esbf0f;" end="\eebf0f;" concealends
    syn region ConqueCCBff0 matchgroup=ConqueCCBG start="\esbff0;" end="\eebff0;" concealends
    syn region ConqueCCBfff matchgroup=ConqueCCBG start="\esbfff;" end="\eebfff;" concealends


    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "highlight link ConqueCCConceal Error

    " foreground colors, low intensity
    highlight ConqueCCF000 guifg=#000000
    highlight ConqueCCF00c guifg=#0000cc
    highlight ConqueCCF0c0 guifg=#00cc00
    highlight ConqueCCF0cc guifg=#00cccc
    highlight ConqueCCFc00 guifg=#cc0000
    highlight ConqueCCFc0c guifg=#cc00cc
    highlight ConqueCCFcc0 guifg=#cccc00
    highlight ConqueCCFccc guifg=#cccccc

    " foreground colors, high intensity
    highlight ConqueCCF000 guifg=#000000
    highlight ConqueCCF00f guifg=#0000ff
    highlight ConqueCCF0f0 guifg=#00ff00
    highlight ConqueCCF0ff guifg=#00ffff
    highlight ConqueCCFf00 guifg=#ff0000
    highlight ConqueCCFf0f guifg=#ff00ff
    highlight ConqueCCFff0 guifg=#ffff00
    highlight ConqueCCFfff guifg=#ffffff

    " background colors, low intensity
    highlight ConqueCCB000 guibg=#000000
    highlight ConqueCCB00c guibg=#0000cc
    highlight ConqueCCB0c0 guibg=#00cc00
    highlight ConqueCCB0cc guibg=#00cccc
    highlight ConqueCCBc00 guibg=#cc0000
    highlight ConqueCCBc0c guibg=#cc00cc
    highlight ConqueCCBcc0 guibg=#cccc00
    highlight ConqueCCBccc guibg=#cccccc

    " background colors, high intensity
    highlight ConqueCCB000 guibg=#000000
    highlight ConqueCCB00f guibg=#0000ff
    highlight ConqueCCB0f0 guibg=#00ff00
    highlight ConqueCCB0ff guibg=#00ffff
    highlight ConqueCCBf00 guibg=#ff0000
    highlight ConqueCCBf0f guibg=#ff00ff
    highlight ConqueCCBff0 guibg=#ffff00
    highlight ConqueCCBfff guibg=#ffffff

    " background colors, low intensity
    highlight link ConqueCCB000 ConqueCCBG
    highlight link ConqueCCB00c ConqueCCBG
    highlight link ConqueCCB0c0 ConqueCCBG
    highlight link ConqueCCB0cc ConqueCCBG
    highlight link ConqueCCBc00 ConqueCCBG
    highlight link ConqueCCBc0c ConqueCCBG
    highlight link ConqueCCBcc0 ConqueCCBG
    highlight link ConqueCCBccc ConqueCCBG

    " background colors, high intensity
    highlight link ConqueCCB000 ConqueCCBG
    highlight link ConqueCCB00f ConqueCCBG
    highlight link ConqueCCB0f0 ConqueCCBG
    highlight link ConqueCCB0ff ConqueCCBG
    highlight link ConqueCCBf00 ConqueCCBG
    highlight link ConqueCCBf0f ConqueCCBG
    highlight link ConqueCCBff0 ConqueCCBG
    highlight link ConqueCCBfff ConqueCCBG

endfunction " }}}

" }}}

" **********************************************************************************************************
" **** Add-on features *************************************************************************************
" **********************************************************************************************************

" {{{

" send selected text from another buffer
function! conque_term#send_selected(type) "{{{

    " get most recent/relevant terminal
    let term = conque_term#get_instance()

    " shove visual text into @@ register
    let reg_save = @@
    sil exe "normal! `<" . a:type . "`>y"
    let @@ = substitute(@@, '^[\r\n]*', '', '')
    let @@ = substitute(@@, '[\r\n]*$', '', '')

    " go to terminal buffer
    call term.focus()

    " execute yanked text
    call term.write(@@)

    " reset original values
    let @@ = reg_save

    " scroll buffer left
    startinsert!
    normal! 0zH

endfunction "}}}

function! conque_term#send_file() "{{{

    let file_lines = readfile(expand('%:p'))
    if type(file_lines) == 3 && len(file_lines) > 0
        let term = conque_term#get_instance()
        call term.focus()

        for line in file_lines
            call term.writeln(line)
        endfor
    else
        echomsg 'Could not read file: ' . expand('%:p')
    endif

endfunction "}}}


function! conque_term#exec_file() "{{{

    let current_file = expand('%:p')
    if !executable(current_file)
        echomsg "Could not run " . current_file . ". Not an executable."
        return
    endif
    exe ':ConqueTermSplit ' . current_file

endfunction "}}}


" called on SessionLoadPost event
function! conque_term#resume_session() " {{{
    if g:ConqueTerm_SessionSupport == 1

        " make sure terminals exist
        if !exists('s:saved_terminals') || type(s:saved_terminals) != 4
            return
        endif

        " rebuild terminals
        for idx in keys(s:saved_terminals)

            " don't recreate inactive terminals
            if s:saved_terminals[idx].active == 0
                continue
            endif

            " check we're in the right buffer
            let bufname = substitute(s:saved_terminals[idx].buffer_name, '\', '', 'g')
            if bufname != bufname("%")
                continue
            endif

            " reopen command
            call conque_term#open(s:saved_terminals[idx].command)

            return
        endfor

    endif
endfunction " }}}

" }}}

" **********************************************************************************************************
" **** "API" functions *************************************************************************************
" **********************************************************************************************************

" See doc/conque_term.txt for full documentation {{{

" Write to a conque terminal buffer
function! s:term_obj.write(...) dict " {{{

    let text = get(a:000, 0, '')
    let jump_to_buffer = get(a:000, 1, 0)

    " if we're not in terminal buffer, pass flag to not position the cursor
    sil exe s:py . ' ' . self.var . '.write_expr("text", False, False)'

    " move cursor to conque buffer
    if jump_to_buffer
        call self.focus()
    endif

endfunction " }}}

" same as write() but adds a newline
function! s:term_obj.writeln(...) dict " {{{

    let text = get(a:000, 0, '')
    let jump_to_buffer = get(a:000, 1, 0)

    call self.write(text . "\r", jump_to_buffer)

endfunction " }}}

" move cursor to terminal buffer
function! s:term_obj.focus() dict " {{{

    let save_sb = &switchbuf
    sil set switchbuf=usetab
    exe 'sb ' . self.buffer_name
    sil exe ":set switchbuf=" . save_sb
    startinsert!

endfunction " }}}

" read from terminal buffer and return string
function! s:term_obj.read(...) dict " {{{

    let read_time = get(a:000, 0, 1)
    let update_buffer = get(a:000, 1, self.is_buffer)

    if update_buffer 
        let up_py = 'True'
    else
        let up_py = 'False'
    endif

    " figure out if we're in the buffer we're updating
    if exists('b:ConqueTerm_Var') && b:ConqueTerm_Var == self.var
        let in_buffer = 1
    else
        let in_buffer = 0
    endif

    let output = ''

    " read!
    sil exec s:py . " conque_tmp = " . self.var . ".read(timeout = " . read_time . ", set_cursor = False, return_output = True, update_buffer = " . up_py . ")"

    " ftw!
    try
        let pycode = "\nif conque_tmp:\n    conque_tmp = re.sub('\\\\\\\\', '\\\\\\\\\\\\\\\\', conque_tmp)\n    conque_tmp = re.sub('\"', '\\\\\\\\\"', conque_tmp)\n    vim.command('let output = \"' + conque_tmp + '\"')\n"
        sil exec s:py . pycode
    catch
        " d'oh
    endtry

    return output

endfunction " }}}

" set output callback
function! s:term_obj.set_callback(callback_func) dict " {{{

    let g:ConqueTerm_Terminals[self.idx].callback = function(a:callback_func)

endfunction " }}}

" close subprocess with ABORT signal
function! s:term_obj.close() dict " {{{

    " kill process
    try
        sil exe s:py . ' ' . self.var . '.abort()'
    catch
        " probably already dead
    endtry

    " delete buffer if option is set
    if self.is_buffer
        call conque_term#set_mappings('stop')
        if exists('g:ConqueTerm_CloseOnEnd') && g:ConqueTerm_CloseOnEnd
            sil exe 'bwipeout! ' . self.buffer_name
            stopinsert!
        endif
    endif

    " mark ourselves as inactive
    let self.active = 0

    " rebuild session options
    let g:ConqueTerm_TerminalsString = string(g:ConqueTerm_Terminals)

endfunction " }}}

" create a new terminal object
function! conque_term#create_terminal_object(...) " {{{

    " find conque buffer to update
    let buf_num = get(a:000, 0, 0)
    if buf_num > 0
        let pvar = 'ConqueTerm_' . buf_num
    elseif exists('b:ConqueTerm_Var')
        let pvar = b:ConqueTerm_Var
        let buf_num = b:ConqueTerm_Idx
    else
        let pvar = g:ConqueTerm_Var
        let buf_num = g:ConqueTerm_Idx
    endif

    " is ther a buffer?
    let is_buffer = get(a:000, 1, 1)

    " the buffer name
    let bname = get(a:000, 2, '')

    " the command
    let command = get(a:000, 3, '')

    " parse out the program name (not perfect)
    let arg_split = split(command, '[^\\]\@<=\s')
    let arg_split[0] = substitute(arg_split[0], '\\ ', ' ', 'g')
    let slash_split = split(arg_split[0], '[/\\]')
    let prg_name = substitute(slash_split[-1], '\(.*\)\..*', '\1', '')

    let l:t_obj = copy(s:term_obj)
    let l:t_obj.is_buffer = is_buffer
    let l:t_obj.idx = buf_num
    let l:t_obj.buffer_name = bname
    let l:t_obj.var = pvar
    let l:t_obj.command = command
    let l:t_obj.program_name = prg_name

    return l:t_obj

endfunction " }}}

" get an existing terminal instance
function! conque_term#get_instance(...) " {{{

    " find conque buffer to update
    let buf_num = get(a:000, 0, 0)

    if exists('g:ConqueTerm_Terminals[buf_num]')
        
    elseif exists('b:ConqueTerm_Var')
        let buf_num = b:ConqueTerm_Idx
    else
        let buf_num = g:ConqueTerm_Idx
    endif

    return g:ConqueTerm_Terminals[buf_num]

endfunction " }}}

" }}}

" **********************************************************************************************************
" **** PYTHON **********************************************************************************************
" **********************************************************************************************************

function! conque_term#load_python() " {{{

    exec s:py . "file " . s:scriptdirpy . "conque_globals.py"
    exec s:py . "file " . s:scriptdirpy . "conque.py"
    if s:platform == 'windows'
        exec s:py . "file " . s:scriptdirpy . "conque_win32_util.py"
        exec s:py . "file " . s:scriptdirpy . "conque_sole_shared_memory.py"
        exec s:py . "file " . s:scriptdirpy . "conque_sole.py"
        exec s:py . "file " . s:scriptdirpy . "conque_sole_wrapper.py"
    else
        exec s:py . "file " . s:scriptdirpy . "conque_screen.py"
        exec s:py . "file " . s:scriptdirpy . "conque_subprocess.py"
    endif

endfunction " }}}

" vim:foldmethod=marker
doc/conque_term.txt	[[[1
646
*ConqueTerm* Plugin to run a shell inside a Vim buffer

The ConqueTerm plugin will turn a Vim buffer into a terminal emulator, allowing
you to run and interact with a shell or shell application inside the buffer.

 1. Installation                                  |conque-term-setup|
    1.1 Requirements for Unix                     |conque-term-requirements|
    1.2 Requirements for Windows                  |conque-term-windows|
    1.3 Installation                              |conque-term-installation|
 2. Usage                                         |conque-term-usage|
    2.1 General Usage                             |conque-term-gen-usage|
    2.2 Special keys                              |conque-term-special-keys|
        2.2.1 Send text to Conque                 |conque-term-send|
        2.2.2 Toggle terminal input mode          |conque-term-input-mode|
        2.2.3 Sending the <Esc> key press         |conque-term-esc|
 3. Configuration                                 |conque-term-options|
    3.1 General                                   |conque-config-general|
        3.1.1 Python version                      |ConqueTerm_PyVersion|
        3.1.2 Fast mode                           |ConqueTerm_FastMode|
        3.1.3 Color support                       |ConqueTerm_Color|
        3.1.4 Session Support                     |ConqueTerm_SessionSupport|
        3.1.5 Keep updating terminal buffer       |ConqueTerm_ReadUnfocused|
        3.1.6 Insert mode when entering buffer    |ConqueTerm_InsertOnEnter|
        3.1.7 Close buffer when program exits     |ConqueTerm_CloseOnEnd|
        3.1.8 Hide start messages                 |ConqueTerm_StartMessages|
        3.1.9 Regex for highlighting your prompt  |ConqueTerm_PromptRegex|
        3.1.10 Syntax type                        |ConqueTerm_Syntax|
    3.2 Keyboard                                  |conque-config-keyboard|
        3.2.1 The <Esc> key                       |ConqueTerm_EscKey|
        3.2.2 Toggle terminal input mode          |ConqueTerm_ToggleKey|
        3.2.3 Enable <C-w> in insert mode         |ConqueTerm_CWInsert|
        3.2.4 Execute current file in Conque      |ConqueTerm_ExecFileKey|
        3.2.5 Send current file contents to Conque|ConqueTerm_SendFileKey|
        3.2.6 Send selected text to Conque        |ConqueTerm_SendVisKey|
        3.2.7 Function Keys                       |ConqueTerm_SendFunctionKeys|
    3.3 Unix                                      |conque-config-unix|
        3.3.1 Choose your terminal type           |ConqueTerm_TERM|
    3.4 Windows                                   |conque-config-windows|
        3.4.1 Python executable                   |ConqueTerm_PyExe|
        3.4.2 Windows character code page         |ConqueTerm_CodePage|
        3.4.3 Terminal color method               |ConqueTerm_ColorMode|
 4. VimScript API                                 |conque-term-api|
    4.1 conque_term#open()                        |conque-term-open|
    4.2 conque_term#subprocess()                  |conque-term-subprocess|
    4.3 conque_term#get_instance()                |conque-term-get-instance|
    4.4 CONQUE_OBJECT.write()                     |conque-term-write|
    4.5 CONQUE_OBJECT.writeln()                   |conque-term-writeln|
    4.6 CONQUE_OBJECT.read()                      |conque-term-read|
    4.7 CONQUE_OBJECT.set_callback()              |conque-term-set-callback|
    4.8 CONQUE_OBJECT.close()                     |conque-term-close|
    4.9 Registering functions                     |conque-term-events|
 5. Misc                                          |conque-term-misc|
    5.1 Known bugs                                |conque-term-bugs|
    5.2 Contribute                                |conque-term-contribute|
    5.3 Feedback                                  |conque-term-feedback|

==============================================================================

1. Installation                                            *conque-term-setup*

Conque is designed for both Unix and Windows operating systems, however the
requirements are slightly different. Please check section below corresponding
to your installed OS.

1.1 Requirements for Unix                           *conque-term-requirements*

 * [G]Vim 7.0+ with +python and/or +python3
 * Python 2.3+ and/or 3.x
 * Unix-like OS: Linux, OS X, Solaris, Cygwin, etc

The most common stumbling block is getting a version of Vim which has the 
python interface enabled. Most all software package managers will have a copy
of Vim with Python support, so that is often the easiest way to get it. If 
you're compiling Vim from source, be sure to use the --enable-pythoninterp 
option, or --enable-python3interp for Python 3. On OS X the best option is 
MacVim, which installs with Python support by default.

1.2 Requirements for Windows                             *conque-term-windows*

 * [G]Vim 7.3 with +python and/or +python3
 * Python 2.7 and/or 3.1
 * Modern Windows OS (XP or later)

Conque only officially supports the latest GVim 7.3 Windows installer 
available at www.vim.org. If you are currently using Vim 7.2 or earlier you
will need to upgrade to 7.3 for Windows support. The Windows installer already
has the +python/+python3 interface built in.

The official 7.3 release of Vim for Windows only works with Python versions
2.7 and/or 3.1. You can download and install Python from their website 
http://www.python.org/download

If you are compiling Vim + Python from source on Windows, the requirements
become only Vim 7.3+ and Python 2.7+.


1.3 Installation                                    *conque-term-installation*

Download the latest vimball from http://conque.googlecode.com

Open the .vba file with Vim and run the following commands:
>
    :so %
    :q
<
That's it! The :ConqueTerm command will be available the next time you start
Vim. You can delete the .vba file when you've verified Conque was successfully
installed.

==============================================================================

2. Usage                                                   *conque-term-usage*

2.1 General Usage                                      *conque-term-gen-usage*

Type :ConqueTerm <command> to launch an application in the current buffer. Eg:
>
    :ConqueTerm bash
    :ConqueTerm mysql -h localhost -u joe_lunchbox Menu
    :ConqueTerm Powershell.exe
<
Use :ConqueTermSplit or :ConqueTermVSplit to open Conque in a new horizontal
or vertical buffer. Use :ConqueTermTab to open Conque in a new tab.

In insert mode you can interact with the shell as you would expect in a 
normal terminal. All key presses will be sent to the terminal, including 
control characters. See |conque-term-special-keys| for more information, 
particularly regarding the <Esc> key.

In normal mode you can use Vim commands to browse your terminal output and 
scroll back through the history. Most all Vim functionality will work, such
as searching, yanking or highlighting text.


2.2 Special keys                                    *conque-term-special-keys*

There are several keys which can be configured to have special behavior with
Conque.

2.2.1 Send text to Conque                                   *conque-term-send*

Conque gives you three different commands to send text from a different
buffer, probably a source code file, to the Conque terminal buffer. All three
are configurable to use your choice of key combinations.

To send a visually selected range of text to an existing terminal buffer,
press the <F9> key.

To send the entire contents of the file you are editing to an existing
terminal buffer, press the <F10> key.

Finally, to execute the current file in a new terminal buffer press the <F11>
key. This will split the screen with a new Conque buffer. The file you are
editing must be executable for this command to work.

See |conque-term-options| for information about configuring these commands.

2.2.2 Toggle terminal input mode                      *conque-term-input-mode*

If you want to use insert mode to edit the terminal screen, press <F8>. You
will now be able to edit the terminal output freely without your cursor
jumping the the active prompt line. This may be useful if you want to reformat
terminal output for readability. 

While the terminal is paused new output will not be displayed on the screen
until you press <F8> again to resume.

You can configure Conque to use a different key with the |ConqueTerm_ToggleKey| 
option.

2.2.3 Sending the <Esc> key press                            *conque-term-esc*

By default if you press the <Esc> key in a Conque buffer you will leave insert
mode. But what if you want the <Esc> character to be sent to your terminal? 
There are two options. By default, pressing <Esc> twice will send one <Esc>
character to the terminal and you will remain in insert mode, while pressing 
it once will leave insert mode.

Alternatively you can use the |ConqueTerm_EscKey| option to choose a
different key for leaving insert mode. If a custom key is set, then all <Esc> 
key presses will be sent to the terminal.

2.3 Registering functions                               *conque-term-register*

Conque allows you to write your own VimScript functions which will be called
at certain events. See the API section |conque-term-events| for more.

==============================================================================

3. Options                                               *conque-term-options*

You can set the following options in your .vimrc (default values shown)

3.1 General                                            *conque-config-general*

3.1.1 Python version                                    *ConqueTerm_PyVersion*

Conque will work with either Python 2.x or 3.x, assuming the interfaces have
been installed. By default it will try to use Python 2 first, then will try 
Python 3. If you want Conque to use Python 3, set this variable to 3. 

Note: even if you set this to 3, if you don't have the python3 interface
Conque will fall back to using Python 2.
>
    let g:ConqueTerm_PyVersion = 2
<
3.1.2 Fast Mode                                          *ConqueTerm_FastMode*

Disable features which could make Conque run slowly. This includes most
terminal colors and some unicode support. Set this to 1 to enable fast mode.
>
      let g:ConqueTerm_FastMode = 0
<
3.1.3 Color support                                         *ConqueTerm_Color*

Terminal colors have the potential to slow down terminal screen rendering,
depending on how many colors are used and how fast the computer is. This
option allows you to choose how much color support will be enabled.

If set to 0, terminal colors will be disabled. This will allow the terminal to
render most quickly. Syntax highlighting will still work. For example 
highlighting quoted strings or MySQL output.

If set to 1, terminal colors will be enabled, but only for the most recent 200
lines of terminal output. Older output will be periodically stripped of color
highlighting to keep the display responsive.

If set to 2, terminal colors will always be enabled. If your programs don't
use color output very frequently this is a good choice.

Note: Color support is automatically disabled in "fast mode".
>
    let g:ConqueTerm_Color = 1
<
3.1.4 Session Support                              *ConqueTerm_SessionSupport*

Vim's :mksession command allows you to save your current buffer configuration
to a file, which can be loaded at a later time after you've closed Vim.

By default, Conque buffers are not restored. This is mostly for safety
reasons; you may not want Vim to automatically re-run a destructive command.

However, if you're not working with missile launch code, and want Vim to
restart your Conque buffers when you load a session file, set this variable 
to 1. Note your original subprocess and shell output will not be restored, but
the same command will be started in your buffer.
>
    let g:ConqueTerm_SessionSupport = 0
<
3.1.5 Keep updating terminal buffer                 *ConqueTerm_ReadUnfocused*

If set to 1 then your Conque buffers will continue to update after you've
switched to another buffer.

Note: Conque buffers may continue to update, but they will not scroll down as
new lines are added beyond the bottom of the visible buffer area. This is a
limitation of the Vim scripting language for which I haven't found a 
workaround.
>
    let g:ConqueTerm_ReadUnfocused = 1
<
3.1.6 Insert mode when entering buffer              *ConqueTerm_InsertOnEnter*

If set to 1 then you will automatically go into insert mode when you enter the
buffer. This diverges from normal Vim behavior. If 0 you will still be in
normal mode.
>
    let g:ConqueTerm_InsertOnEnter = 0
<
3.1.7 Close buffer when program exits                  *ConqueTerm_CloseOnEnd*

If you want your terminal buffer to be closed and permanently deleted when the 
program running inside of it exits, set this option to 1. Otherwise the buffer
will become a simple text buffer after the program exits, and you can edit the
program output in insert mode.
>
    let g:ConqueTerm_CloseOnEnd = 0
<
3.1.8 Show start messages                           *ConqueTerm_StartMessages*

Display warning messages when starting up ConqueTerm if your system is
configured incorrectly.
>
    let g:ConqueTerm_StartMessages = 1
<
3.1.9 Regex for highlighting your prompt              *ConqueTerm_PromptRegex*

Use this regular expression for sytax highlighting your terminal prompt. Your
terminal will generally run faster if you use Vim highlighting instead of 
terminal colors for your prompt. You can also use it to do more advanced
syntax highlighting for the prompt line.
>
    let g:ConqueTerm_PromptRegex = '^\w\+@[0-9A-Za-z_.-]\+:[0-9A-Za-z_./\~,:-]\+\$'
<
3.1.10 Choose Vim syntax type                              *ConqueTerm_Syntax*

Set the buffer syntax. The default 'conque' has highlighting for MySQL, but 
not much else.
>
    let g:ConqueTerm_Syntax = 'conque'
<
3.2 Keyboard                                          *conque-config-keyboard*

3.2.1 The <Esc> key                                        *ConqueTerm_EscKey*

If a custom key is set, then all <Esc> key presses will be sent to the 
terminal and you must use this custom key to leave insert mode. If left to the
default value of '<Esc>' then you must press it twice to send the escape
character to the terminal, while pressing it once will leave insert mode.

Note: You cannot use a key which is internally coded with the escape
character. This includes the <F-> keys and often the <A-> and <M-> keys.
Picking a control key, such as <C-k> will be your best bet.
>
    let g:ConqueTerm_EscKey = '<Esc>'
<
3.2.2 Toggle terminal input mode                        *ConqueTerm_ToggleKey*

Press this key to pause terminal input and output display. You will then be
able to edit the terminal screen as if it were a normal text buffer. Press
this key again to resume terminal mode.
>
    let g:ConqueTerm_ToggleKey = '<F8>'
<
3.2.3 Enable <C-w> in insert mode                        *ConqueTerm_CWInsert*

If set to 1 then you can leave the Conque buffer using the <C-w> commands
while you're still in insert mode. If set to 0 then the <C-w> character will
be sent to the terminal. If both this option and ConqueTerm_InsertOnEnter are
set you can go in and out of the terminal buffer while never leaving insert
mode.
>
    let g:ConqueTerm_CWInsert = 0
<
3.2.4 Execute current file in Conque                  *ConqueTerm_ExecFileKey*

Press this key to execute the file you're currently editing in a Conque
buffer. Is equivelent to running the command :ConqueTermSplit YOUR_FILE. Your
file must be executable for this command to work correctly.
>
    let g:ConqueTerm_ExecFileKey = '<F11>'
<
3.2.5 Send current file contents to Conque            *ConqueTerm_SendFileKey*

Press this key to send your entire file contents to the most recently opened
Conque buffer as keyboard input.
>
    let g:ConqueTerm_SendFileKey = '<F10>'
<
3.2.6 Send selected text to Conque                     *ConqueTerm_SendVisKey*

Use this key to send the currently selected text to the most recently created
Conque buffer.
>
    let g:ConqueTerm_SendVisKey = '<F9>'
<
3.2.7 Function Keys                              *ConqueTerm_SendFunctionKeys*

By default, function keys (the F1-F12 row at the top of your keyboard) are not
passed to the terminal. Set this option to 1 to send these key events.

Note: Unless you configured |ConqueTerm_SendVisKey| and |ConqueTerm_ToggleKey|
to use different keys, <F8> and <F9> will not be sent to the terminal even if
you set this option to 1.
>
    let g:ConqueTerm_SendFunctionKeys = 0
<
3.3 Unix                                                  *conque-config-unix*

3.3.1 Choose your terminal type, Unix ONLY                   *ConqueTerm_TERM*

Use this option to tell Conque what type of terminal it should identify itself
as. Conque officially uses the more limited VT100 terminal type for
developement and testing, although it supports some more advanced features
such as colors and title strings.

You can change this setting to a more advanced type, namely 'xterm', but your
results may vary depending on which programs you're running.
>
    let g:ConqueTerm_TERM = 'vt100'
<
3.4 Windows                                            *conque-config-windows*

3.4.1 Python executable, Windows ONLY                       *ConqueTerm_PyExe*

The Windows version of Conque needs to know the path to the python.exe
executable for the version of Python Conque is using. If you installed Python
in the default location, or added the Python directory to your system path,
Conque should be able to find python.exe without you changing this variable.

For example, you might set this to 'C:\Program Files\Python27\python.exe'
>
    let g:ConqueTerm_PyExe = ''
<
3.4.2 Windows character code page                        *ConqueTerm_CodePage*

Set the "code page" Windows will use for your console. Leave this value set to
zero to use the environment code page.

Note: Displaying unicode characters on Conque for Windows needs work.
>
      let g:ConqueTerm_CodePage = 0
<
3.4.3 Terminal color method, Windows ONLY               *ConqueTerm_ColorMode*

Vim syntax highlighting by coordinate (e.g. the 3-7th characters on the 42nd
line) can be very slow. If you set this variable to 'conceal', you can use
the new conceal feature to render terminal colors. Requires Vim 7.3 and only 
works on the Windows version of Conque. This will make colors render faster, 
however it will also add hidden characters to the screen, which may be 
annoying if you're copying and pasting terminal output out of the Conque 
buffer. Set this to an empty string '' to disable concealed highlighting.
>
    let g:ConqueTerm_ColorMode = 'conceal'
<
==============================================================================

4. VimScript API (Beta)                                      *conque-term-api*

The Conque scripting API allows you to create and interact with Conque
terminals with the VimScript language. This API is still in beta stage.

4.1 conque_term#open({command}, [buf_opts], [remain])       *conque-term-open*

The open() function will create a new terminal buffer and start your command.

The {command} must be an executable, either an absolute path or relative to
your system path.

You can pass in a list of vim commands [buf_opts] which will be executed after
the new buffer is created but before the command is started. These are
typically commands to alter the size, position or configuration of the buffer
window.

Note: If you don't pass in a command such as 'split', the terminal will open
in the current buffer.

If you don't want the new terminal buffer to become the new active buffer, set
 [remain] to 1. Only works if you create a split screen using [options].

Returns a Conque terminal object.

Examples:
>
    let my_terminal = conque_term#open('/bin/bash')
    let my_terminal = conque_term#open('ipython', ['split', 'resize 20'], 1)
<
4.2 conque_term#subprocess({command})                 *conque-term-subprocess*

Starts a new subprocess with your {command}, but no terminal buffer is ever
created. This may be useful if you need asynchronous interaction with a
subprocess, but want to handle the output on your own.

Returns a Conque terminal object.

Example:
>
    let my_subprocess = conque_term#subprocess('tail -f /var/log/foo.log')
<
4.3 conque_term#get_instance( [terminal_number] )   *conque-term-get-instance*

Use the get_instance() function to retrieve an existing terminal object. The
terminal could have been created either with the user command :ConqueTerm or
with an API call to conque_term#open() or subprocess().

Use the optional [terminal_number] to retrieve a specific terminal instance. 
Otherwise if the current buffer is a Conque terminal, it will be returned,
else the most recently created terminal. The terminal number is what you see 
at the end of a terminal buffer name, e.g. "bash - 2".

Returns a Conque terminal object.

Example:
>
    nnoremap <F4> :call conque_term#get_instance().writeln('clear')<CR>
<
4.4 CONQUE_OBJECT.write({text})                            *conque-term-write*

Once you have a terminal object from open(), subprocess() or get_instance() 
you can send text input to it with the write() method.

No return value.

Examples:
>
    call my_terminal.write("whoami\n")
    call my_terminal.write("\<C-c>")
<
4.5 CONQUE_OBJECT.writeln({text})                        *conque-term-writeln*

The same as write() except adds a \n character to the end if your input.

Examples:
>
    call my_subprocess.writeln('make')
<
4.6 CONQUE_OBJECT.read( [timeout], [update_buffer] )        *conque-term-read*

Read new output from a Conque terminal subprocess. New output will be returned
as a string, and the terminal buffer will also be updated by default.

If you are reading immediately after calling the write() method, you may want
to wait [timeout] milliseconds for output to be ready.

If you want to prevent the output from being displayed in the terminal buffer,
set [update_buffer] to 0. This option has no effect if the terminal was 
created with the subprocess() function, since there never is a buffer to
update.

Returns output string.

Note: The terminal buffer will not automatically scroll down if the new output
extends beyond the bottom of the visible buffer. Vim doesn't allow "unfocused"
buffers to be scrolled at the current version, although hopefully this will 
change.

Examples:
>
    call my_terminal.writeln('whoami')
    let output = my_terminal.read(500)
    call my_terminal.writeln('ls -lha')
    let output = my_terminal.read(1000, 1)
<
4.7 CONQUE_OBJECT.set_callback( {funcname} )        *conque-term-set-callback*

Register a callback function for this subprocess instance. This function will
automatically be called whenever new output is available. Only practical with
subprocess() objects.

Conque checkes for new subprocess output once a second when Vim is idle. If
new output is found your function will be called.

Pass in the callback function name {funcname} as a string.

No return value.

Note: this method requires the g:ConqueTerm_ReadUnfocused option to be set.

Note: this method is experimental, results may vary.

Example:
>
    let sp = conque_term#subprocess('tail -f /home/joe/log/error_log')

    function! MyErrorAlert(output)
        echo a:output
    endfunction

    call sp.set_callback('MyErrorAlert')
<
4.8 CONQUE_OBJECT.close()                                  *conque-term-close*

Kill your terminal subprocess. Sends the ABORT signal. You probably want to
close your subprocess in a more graceful manner with the write() method, but 
this can be used when needed. Does not close the terminal buffer, if it
exists.

This method will be called on all existing Conque subprocesses when Vim exits.

Example:
>
    let term = conque_term#open('ping google.com', ['belowright split'])
    call term.read(5000)
    call term.close()
<
4.9 Registering functions                                 *conque-term-events*

Conque provides the option to register callback functions which will be
executed at several different events. The currently available events are:

  after_startup    After your application has loaded into the buffer.
  buffer_enter     When you switch to a Conque buffer.
  buffer_leave     When you leave a Conque buffer.

You may use the function conque_term#register_function(event, function_name) 
to add additional hooks at a particular event. The second argument should be
the name of a callback function which has one parameter, the current 
terminal object (see|conque-term-api|for more about terminal objects). 

For example:
>
  function MyConqueStartup(term)

      " set buffer syntax using the name of the program currently running
      let syntax_associations = { 'ipython': 'python', 'irb': 'ruby' }

      if has_key(syntax_associations, a:term.program_name)
          execute 'setlocal syntax=' . syntax_associations[a:term.program_name]
      else
          execute 'setlocal syntax=' . a:term.program_name
      endif

      " shrink window height to 10 rows
      resize 10

      " silly example of terminal api usage
      if a:term.program_name == 'bash'
          call a:term.writeln('svn up ~/projects/*')
      endif
      
  endfunction

  call conque_term#register_function('after_startup', 'MyConqueStartup')
<

==============================================================================

5. Misc                                                     *conque-term-misc*


5.1 Known bugs                                              *conque-term-bugs*

The following are known limitations:

 - Font/color highlighting is imperfect and slow. If you don't care about
   color in your shell, set g:ConqueTerm_Color = 0 in your .vimrc
 - Conque only supports the extended ASCII character set for input, not utf-8.
 - VT100 escape sequence support is not complete.
 - Alt/Meta key support in Vim isn't great in general, and conque is no 
   exception. Pressing <Esc><Esc>x or <Esc><M-x> instead of <M-x> works in 
   most cases.


5.2 Contribute                                        *conque-term-contribute*

The two contributions most in need are improvements to Vim itself. I currently 
use hacks to capture key press input from the user, and to poll the terminal
for more output. The Vim todo.txt document lists proposed improvements to give 
users this behavior without hacks. Having a key press event should allow 
Conque to work with multi- byte input. If you are a Vim developer, please 
consider prioritizing these two items: 

 - todo.txt (Autocommands, line ~3137)
     8   Add an event like CursorHold that is triggered repeatedly, not just 
         once after typing something.


5.3 Feedback                                            *conque-term-feedback*

Bugs, suggestions and patches are all welcome.

For more information visit http://conque.googlecode.com

Check out the latest from svn at http://conque.googlecode.com/svn/trunk/

 vim:tw=78:ts=8:ft=help:norl:
plugin/conque_term.vim	[[[1
213
" FILE:     plugin/conque_term.vim {{{
" AUTHOR:   Nico Raffo <nicoraffo@gmail.com>
" WEBSITE:  http://conque.googlecode.com
" MODIFIED: 2011-08-12
" VERSION:  2.2, for Vim 7.0
" LICENSE:
" Conque - Vim terminal/console emulator
" Copyright (C) 2009-__YEAR__ Nico Raffo 
"
" MIT License
" 
" Permission is hereby granted, free of charge, to any person obtaining a copy
" of this software and associated documentation files (the "Software"), to deal
" in the Software without restriction, including without limitation the rights
" to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
" copies of the Software, and to permit persons to whom the Software is
" furnished to do so, subject to the following conditions:
" 
" The above copyright notice and this permission notice shall be included in
" all copies or substantial portions of the Software.
" 
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
" IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
" FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
" AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
" LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
" OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
" THE SOFTWARE.
" }}}

" See docs/conque_term.txt for help or type :help ConqueTerm

if exists('g:ConqueTerm_Loaded') || v:version < 700
    finish
endif

" **********************************************************************************************************
" **** CONFIGURATION ***************************************************************************************
" **********************************************************************************************************

" {{{

" Fast mode {{{
" Disables all features which could cause Conque to run slowly, including:
"   * Disables terminal colors
"   * Disables some multi-byte character handling
if !exists('g:ConqueTerm_FastMode')
    let g:ConqueTerm_FastMode = 0
endif " }}}

" automatically go into insert mode when entering buffer {{{
if !exists('g:ConqueTerm_InsertOnEnter')
    let g:ConqueTerm_InsertOnEnter = 0
endif " }}}

" Allow user to use <C-w> keys to switch window in insert mode. {{{
if !exists('g:ConqueTerm_CWInsert')
    let g:ConqueTerm_CWInsert = 0
endif " }}}

" Choose key mapping to leave insert mode {{{
" If you choose something other than '<Esc>', then <Esc> will be sent to terminal
" Using a different key will usually fix Alt/Meta key issues
if !exists('g:ConqueTerm_EscKey')
    let g:ConqueTerm_EscKey = '<Esc>'
endif " }}}

" Use this key to execute the current file in a split window. {{{
" THIS IS A GLOBAL KEY MAPPING
if !exists('g:ConqueTerm_ExecFileKey')
    let g:ConqueTerm_ExecFileKey = '<F11>'
endif " }}}

" Use this key to send the current file contents to conque. {{{
" THIS IS A GLOBAL KEY MAPPING
if !exists('g:ConqueTerm_SendFileKey')
    let g:ConqueTerm_SendFileKey = '<F10>'
endif " }}}

" Use this key to send selected text to conque. {{{
" THIS IS A GLOBAL KEY MAPPING
if !exists('g:ConqueTerm_SendVisKey')
    let g:ConqueTerm_SendVisKey = '<F9>'
endif " }}}

" Use this key to toggle terminal key mappings. {{{
" Only mapped inside of Conque buffers.
if !exists('g:ConqueTerm_ToggleKey')
    let g:ConqueTerm_ToggleKey = '<F8>'
endif " }}}

" Enable color. {{{
" If your apps use a lot of color it will slow down the shell.
" 0 - no terminal colors. You still will see Vim syntax highlighting.
" 1 - limited terminal colors (recommended). Past terminal color history cleared regularly.
" 2 - all terminal colors. Terminal color history never cleared.
if !exists('g:ConqueTerm_Color')
    let g:ConqueTerm_Color = 1
endif " }}}

" Color mode. Windows ONLY {{{
" Set this variable to 'conceal' to use Vim's conceal mode for terminal colors.
" This makes colors render much faster, but has some odd baggage.
if !exists('g:ConqueTerm_ColorMode')
    let g:ConqueTerm_ColorMode = ''
endif " }}}

" TERM environment setting {{{
if !exists('g:ConqueTerm_TERM')
    let g:ConqueTerm_TERM =  'vt100'
endif " }}}

" Syntax for your buffer {{{
if !exists('g:ConqueTerm_Syntax')
    let g:ConqueTerm_Syntax = 'conque_term'
endif " }}}

" Keep on updating the shell window after you've switched to another buffer {{{
if !exists('g:ConqueTerm_ReadUnfocused')
    let g:ConqueTerm_ReadUnfocused = 0
endif " }}}

" Use this regular expression to highlight prompt {{{
if !exists('g:ConqueTerm_PromptRegex')
    let g:ConqueTerm_PromptRegex = '^\w\+@[0-9A-Za-z_.-]\+:[0-9A-Za-z_./\~,:-]\+\$'
endif " }}}

" Choose which Python version to attempt to load first {{{
" Valid values are 2, 3 or 0 (no preference)
if !exists('g:ConqueTerm_PyVersion')
    let g:ConqueTerm_PyVersion = 2
endif " }}}

" Path to python.exe. (Windows only) {{{
" By default, Conque will check C:\PythonNN\python.exe then will search system path
" If you have installed Python in an unusual location and it's not in your path, fill in the full path below
" E.g. 'C:\Program Files\Python\Python27\python.exe'
if !exists('g:ConqueTerm_PyExe')
    let g:ConqueTerm_PyExe = ''
endif " }}}

" Automatically close buffer when program exits {{{
if !exists('g:ConqueTerm_CloseOnEnd')
    let g:ConqueTerm_CloseOnEnd = 0
endif " }}}

" Send function key presses to terminal {{{
if !exists('g:ConqueTerm_SendFunctionKeys')
    let g:ConqueTerm_SendFunctionKeys = 0
endif " }}}

" Session support {{{
if !exists('g:ConqueTerm_SessionSupport')
    let g:ConqueTerm_SessionSupport = 0
endif " }}}

" hide Conque startup messages {{{
" messages should only appear the first 3 times you start Vim with a new version of Conque
" and include important Conque feature and option descriptions
" TODO - disabled and unused for now
if !exists('g:ConqueTerm_StartMessages')
    let g:ConqueTerm_StartMessages = 1
endif " }}}

" Windows character code page {{{
" Leave at 0 to use current environment code page.
" Use 65001 for utf-8, although many console apps do not support it.
if !exists('g:ConqueTerm_CodePage')
    let g:ConqueTerm_CodePage = 0
endif " }}}

" InsertCharPre support {{{
" Disable this feature by default, still in Beta
if !exists('g:ConqueTerm_InsertCharPre')
    let g:ConqueTerm_InsertCharPre = 0
endif " }}}

" }}}

" **********************************************************************************************************
" **** Startup *********************************************************************************************
" **********************************************************************************************************

" Startup {{{

let g:ConqueTerm_Loaded = 1
let g:ConqueTerm_Idx = 0
let g:ConqueTerm_Version = 210

command! -nargs=+ -complete=shellcmd ConqueTerm call conque_term#open(<q-args>)
command! -nargs=+ -complete=shellcmd ConqueTermSplit call conque_term#open(<q-args>, ['belowright split'])
command! -nargs=+ -complete=shellcmd ConqueTermVSplit call conque_term#open(<q-args>, ['belowright vsplit'])
command! -nargs=+ -complete=shellcmd ConqueTermTab call conque_term#open(<q-args>, ['tabnew'])

" }}}

" **********************************************************************************************************
" **** Global Mappings & Autocommands **********************************************************************
" **********************************************************************************************************

" Startup {{{

if exists('g:ConqueTerm_SessionSupport') && g:ConqueTerm_SessionSupport == 1
    autocmd SessionLoadPost * call conque_term#resume_session()
endif

if maparg(g:ConqueTerm_ExecFileKey, 'n') == ''
    exe 'nnoremap <silent> ' . g:ConqueTerm_ExecFileKey . ' :call conque_term#exec_file()<CR>'
endif

" }}}

" vim:foldmethod=marker
syntax/conque_term.vim	[[[1
113
" FILE:     syntax/conque_term.vim {{{
" AUTHOR:   Nico Raffo <nicoraffo@gmail.com>
" WEBSITE:  http://conque.googlecode.com
" MODIFIED: 2011-08-12
" VERSION:  2.2, for Vim 7.0
" LICENSE:
" Conque - Vim terminal/console emulator
" Copyright (C) 2009-__YEAR__ Nico Raffo 
"
" MIT License
" 
" Permission is hereby granted, free of charge, to any person obtaining a copy
" of this software and associated documentation files (the "Software"), to deal
" in the Software without restriction, including without limitation the rights
" to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
" copies of the Software, and to permit persons to whom the Software is
" furnished to do so, subject to the following conditions:
" 
" The above copyright notice and this permission notice shall be included in
" all copies or substantial portions of the Software.
" 
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
" IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
" FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
" AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
" LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
" OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
" THE SOFTWARE.
" }}}


" *******************************************************************************************************************
" MySQL *************************************************************************************************************
" *******************************************************************************************************************

" TODO Move these to syntax which is only executed for mysql
"syn match MySQLTableBodyG "^\s*\w\+:\(.\+\)\=$" contains=MySQLTableHeadG,MySQLNullG,MySQLBool,MySQLNumberG,MySQLStorageClass oneline skipwhite skipnl
"syn match MySQLTableHeadG "^\s*\w\+:" contains=MySQLTableColon skipwhite contained
"syn match MySQLTableColon ":" contained

syn match MySQLTableHead "^ *|.*| *$" nextgroup=MySQLTableDivide contains=MySQLTableBar oneline skipwhite skipnl
syn match MySQLTableBody "^ *|.*| *$" nextgroup=MySQLTableBody,MySQLTableEnd contains=MySQLTableBar,MySQLNull,MySQLBool,MySQLNumber,MySQLStorageClass oneline skipwhite skipnl
syn match MySQLTableEnd "^ *+[+=-]\++ *$" oneline 
syn match MySQLTableDivide "^ *+[+=-]\++ *$" nextgroup=MySQLTableBody oneline skipwhite skipnl
syn match MySQLTableStart "^ *+[+=-]\++ *$" nextgroup=MySQLTableHead oneline skipwhite skipnl
syn match MySQLNull " NULL " contained contains=MySQLTableBar
syn match MySQLStorageClass " PRI " contained
syn match MySQLStorageClass " MUL " contained
syn match MySQLStorageClass " UNI " contained
syn match MySQLStorageClass " CURRENT_TIMESTAMP " contained
syn match MySQLStorageClass " auto_increment " contained
syn match MySQLTableBar "|" contained
syn match MySQLNumber "|\?  *\d\+\(\.\d\+\)\?  *|" contained contains=MySQLTableBar
syn match MySQLQueryStat "^\d\+ rows\? in set.*" oneline
syn match MySQLPromptLine "^.\?mysql> .*$" contains=MySQLKeyword,MySQLPrompt,MySQLString oneline
syn match MySQLPromptLine "^    -> .*$" contains=MySQLKeyword,MySQLPrompt,MySQLString oneline
syn match MySQLPrompt "^.\?mysql>" contained oneline
syn match MySQLPrompt "^    ->" contained oneline
syn case ignore
syn keyword MySQLKeyword select count max sum avg date show table tables status like as from left right outer inner join contained 
syn keyword MySQLKeyword where group by having limit offset order desc asc show contained and interval is null on
syn case match
syn region MySQLString start=+'+ end=+'+ skip=+\\'+ contained oneline
syn region MySQLString start=+"+ end=+"+ skip=+\\"+ contained oneline
syn region MySQLString start=+`+ end=+`+ skip=+\\`+ contained oneline


hi def link MySQLPrompt Identifier
hi def link MySQLTableHead Title
hi def link MySQLTableBody Normal
hi def link MySQLBool Boolean
hi def link MySQLStorageClass StorageClass
hi def link MySQLNumber Number
hi def link MySQLKeyword Keyword
hi def link MySQLString String

" terms which have no reasonable default highlight group to link to
hi MySQLTableHead term=bold cterm=bold gui=bold
if &background == 'dark'
    hi MySQLTableEnd term=NONE cterm=NONE gui=NONE ctermfg=238 guifg=#444444
    hi MySQLTableDivide term=NONE cterm=NONE gui=NONE ctermfg=238 guifg=#444444
    hi MySQLTableStart term=NONE cterm=NONE gui=NONE ctermfg=238 guifg=#444444
    hi MySQLTableBar term=NONE cterm=NONE gui=NONE ctermfg=238 guifg=#444444
    hi MySQLNull term=NONE cterm=NONE gui=NONE ctermfg=238 guifg=#444444
    hi MySQLQueryStat term=NONE cterm=NONE gui=NONE ctermfg=238 guifg=#444444
elseif &background == 'light'
    hi MySQLTableEnd term=NONE cterm=NONE gui=NONE ctermfg=247 guifg=#9e9e9e
    hi MySQLTableDivide term=NONE cterm=NONE gui=NONE ctermfg=247 guifg=#9e9e9e
    hi MySQLTableStart term=NONE cterm=NONE gui=NONE ctermfg=247 guifg=#9e9e9e
    hi MySQLTableBar term=NONE cterm=NONE gui=NONE ctermfg=247 guifg=#9e9e9e
    hi MySQLNull term=NONE cterm=NONE gui=NONE ctermfg=247 guifg=#9e9e9e
    hi MySQLQueryStat term=NONE cterm=NONE gui=NONE ctermfg=247 guifg=#9e9e9e
endif


" *******************************************************************************************************************
" Bash **************************************************************************************************************
" *******************************************************************************************************************

" Typical Prompt
if g:ConqueTerm_PromptRegex != ''
    silent execute "syn match ConquePromptLine '" . g:ConqueTerm_PromptRegex . ".*$' contains=ConquePrompt,ConqueString oneline"
    silent execute "syn match ConquePrompt '" . g:ConqueTerm_PromptRegex . "' contained oneline"
    hi def link ConquePrompt Identifier
endif

" Strings
syn region ConqueString start=+'+ end=+'+ skip=+\\'+ contained oneline
syn region ConqueString start=+"+ end=+"+ skip=+\\"+ contained oneline
syn region ConqueString start=+`+ end=+`+ skip=+\\`+ contained oneline
hi def link ConqueString String

" vim: foldmethod=marker
