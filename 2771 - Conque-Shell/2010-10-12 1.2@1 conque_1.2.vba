" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
autoload/conque_term.vim	[[[1
1966
" FILE:     plugin/conque_term.vim {{{
" AUTHOR:   Nico Raffo <nicoraffo@gmail.com>
" MODIFIED: 2010-10-10
" VERSION:  1.2, for Vim 7.0
" LICENSE:
" Conque - pty interaction in Vim
" Copyright (C) 2009-2010 Nico Raffo 
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
" **** CROSS-TERMINAL SETTINGS *****************************************************************************
" **********************************************************************************************************

" Extra key codes
let s:input_extra = []

let s:term_obj = { 'idx' : 1, 'var' : '', 'is_buffer' : 1, 'active' : 1, 'buffer_name' : '' }
let s:terminals = {}

let s:save_updatetime = &updatetime

augroup ConqueTerm
autocmd ConqueTerm VimLeave * call conque_term#close_all()

" read more output when this isn't the current buffer
if g:ConqueTerm_ReadUnfocused == 1
    autocmd ConqueTerm CursorHold * call conque_term#read_all(0)
endif

" **********************************************************************************************************
" **** VIM FUNCTIONS ***************************************************************************************
" **********************************************************************************************************

" launch conque
function! conque_term#open(...) "{{{
    let command = get(a:000, 0, '')
    let hooks   = get(a:000, 1, [])
    let return_to_current  = get(a:000, 2, 0)
    let is_buffer  = get(a:000, 3, 1)

    " switch to buffer if needed
    if is_buffer && return_to_current
      let save_sb = &switchbuf

      "use an agressive sb option
      sil set switchbuf=usetab

      " current buffer name
      let current_buffer = bufname("%")
    endif

    " bare minimum validation
    if !has('python')
        echohl WarningMsg | echomsg "Conque requires the Python interface to be installed" | echohl None
        return 0
    endif
    if empty(command)
        echohl WarningMsg | echomsg "No command found" | echohl None
        return 0
    else
        let l:cargs = split(command, '\s')
        if !executable(l:cargs[0])
            echohl WarningMsg | echomsg "Not an executable: " . l:cargs[0] | echohl None
            return 0
        endif
    endif

    let g:ConqueTerm_Idx += 1
    let g:ConqueTerm_Var = 'ConqueTerm_' . g:ConqueTerm_Idx
    let g:ConqueTerm_BufName = substitute(command, ' ', '\\ ', 'g') . "\\ -\\ " . g:ConqueTerm_Idx

    " set buffer window options
    if is_buffer
        call conque_term#set_buffer_settings(command, hooks)

        let b:ConqueTerm_Idx = g:ConqueTerm_Idx
        let b:ConqueTerm_Var = g:ConqueTerm_Var
    endif

    " save handle
    let t_obj = conque_term#create_terminal_object(g:ConqueTerm_Idx, is_buffer, g:ConqueTerm_BufName)
    let s:terminals[g:ConqueTerm_Idx] = t_obj

    " open command
    try
        let l:config = '{"color":' . string(g:ConqueTerm_Color) . ',"TERM":"' . g:ConqueTerm_TERM . '"}'
        execute 'python ' . g:ConqueTerm_Var . ' = Conque()'
        execute "python " . g:ConqueTerm_Var . ".open('" . conque_term#python_escape(command) . "', " . l:config . ")"
    catch 
        echohl WarningMsg | echomsg "Unable to open command: " . command | echohl None
        return 0
    endtry

    " set buffer mappings and auto commands 
    if is_buffer
        call conque_term#set_mappings('start')
    endif

    " switch to buffer if needed
    if is_buffer && return_to_current
        " jump back to code buffer
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
    endif
    return t_obj

endfunction " }}}

" set buffer options
function! conque_term#set_buffer_settings(command, pre_hooks) "{{{

    " optional hooks to execute, e.g. 'split'
    for h in a:pre_hooks
        sil exe h
    endfor
    sil exe "edit " . g:ConqueTerm_BufName

    " showcmd gets altered by nocompatible
    let sc_save = &showcmd

    " buffer settings 
    setlocal nocompatible      " conque won't work in compatible mode
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
    setfiletype conque_term    " useful
    sil exe "setlocal syntax=" . g:ConqueTerm_Syntax

    " reset showcmd
    if sc_save
      set showcmd
    else
      set noshowcmd
    endif

    " temporary global settings go in here
    call conque_term#on_focus()

endfunction " }}}

" set key mappings and auto commands
function! conque_term#set_mappings(action) "{{{

    " set action
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

    " remove all auto commands
    if l:action == 'stop'
        execute 'autocmd! ' . b:ConqueTerm_Var

    else
        execute 'augroup ' . b:ConqueTerm_Var

        " handle unexpected closing of shell, passes HUP to parent and all child processes
        execute 'autocmd ' . b:ConqueTerm_Var . ' BufUnload <buffer> python ' . b:ConqueTerm_Var . '.proc.signal(1)'

        " check for resized/scrolled buffer when entering buffer
        execute 'autocmd ' . b:ConqueTerm_Var . ' BufEnter <buffer> python ' . b:ConqueTerm_Var . '.update_window_size()'
        execute 'autocmd ' . b:ConqueTerm_Var . ' VimResized python ' . b:ConqueTerm_Var . '.update_window_size()'

        " set/reset updatetime on entering/exiting buffer
        execute 'autocmd ' . b:ConqueTerm_Var . ' BufEnter <buffer> call conque_term#on_focus()'
        execute 'autocmd ' . b:ConqueTerm_Var . ' BufLeave <buffer> call conque_term#on_blur()'

        " reposition cursor when going into insert mode
        execute 'autocmd ' . b:ConqueTerm_Var . ' InsertEnter <buffer> python ' . b:ConqueTerm_Var . '.insert_enter()'

        " poll for more output
        sil execute 'autocmd ' . b:ConqueTerm_Var . ' CursorHoldI <buffer> python ' .  b:ConqueTerm_Var . '.auto_read()'
    endif

    " map ASCII 1-31
    for c in range(1, 31)
        " <Esc>
        if c == 27
            continue
        endif
        if l:action == 'start'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <C-' . nr2char(64 + c) . '> <C-o>:python ' . b:ConqueTerm_Var . '.write(chr(' . c . '))<CR>'
        else
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <C-' . nr2char(64 + c) . '>'
        endif
    endfor
    if l:action == 'start'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> <C-c> <C-o>:python ' . b:ConqueTerm_Var . '.write(chr(3))<CR>'
    else
        sil exe 'n' . map_modifier . 'map <silent> <buffer> <C-c>'
    endif

    " leave insert mode
    if !exists('g:ConqueTerm_EscKey') || g:ConqueTerm_EscKey == '<Esc>'
        " use <Esc><Esc> to send <Esc> to terminal
        if l:action == 'start'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Esc><Esc> <C-o>:python ' . b:ConqueTerm_Var . '.write(chr(27))<CR>'
        else
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Esc><Esc>'
        endif
    else
        " use <Esc> to send <Esc> to terminal
        if l:action == 'start'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> ' . g:ConqueTerm_EscKey . ' <Esc>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Esc> <C-o>:python ' . b:ConqueTerm_Var . '.write(chr(27))<CR>'
        else
            sil exe 'i' . map_modifier . 'map <silent> <buffer> ' . g:ConqueTerm_EscKey
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Esc>'
        endif
    endif

    " Map <C-w> in insert mode
    if exists('g:ConqueTerm_CWInsert') && g:ConqueTerm_CWInsert == 1
        inoremap <silent> <buffer> <C-w>j <Esc><C-w>j
        inoremap <silent> <buffer> <C-w>k <Esc><C-w>k
        inoremap <silent> <buffer> <C-w>h <Esc><C-w>h
        inoremap <silent> <buffer> <C-w>l <Esc><C-w>l
        inoremap <silent> <buffer> <C-w>w <Esc><C-w>w
    endif

    " map ASCII 33-127
    for i in range(33, 127)
        " <Bar>
        if i == 124
            if l:action == 'start'
                sil exe "i" . map_modifier . "map <silent> <buffer> <Bar> <C-o>:python " . b:ConqueTerm_Var . ".write(chr(124))<CR>"
            else
                sil exe "i" . map_modifier . "map <silent> <buffer> <Bar>"
            endif
            continue
        endif
        if l:action == 'start'
            sil exe "i" . map_modifier . "map <silent> <buffer> " . nr2char(i) . " <C-o>:python " . b:ConqueTerm_Var . ".write(chr(" . i . "))<CR>"
        else
            sil exe "i" . map_modifier . "map <silent> <buffer> " . nr2char(i)
        endif
    endfor

    " map ASCII 128-255
    for i in range(128, 255)
        if l:action == 'start'
            sil exe "i" . map_modifier . "map <silent> <buffer> " . nr2char(i) . " <C-o>:python " . b:ConqueTerm_Var . ".write('" . nr2char(i) . "')<CR>"
        else
            sil exe "i" . map_modifier . "map <silent> <buffer> " . nr2char(i)
        endif
    endfor

    " Special cases
    if l:action == 'start'
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <BS> <C-o>:python ' . b:ConqueTerm_Var . '.write(u"\u0008")<CR>'
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <Space> <C-o>:python ' . b:ConqueTerm_Var . '.write(" ")<CR>'
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <Up> <C-o>:python ' . b:ConqueTerm_Var . '.write(u"\u001b[A")<CR>'
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <Down> <C-o>:python ' . b:ConqueTerm_Var . '.write(u"\u001b[B")<CR>'
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <Right> <C-o>:python ' . b:ConqueTerm_Var . '.write(u"\u001b[C")<CR>'
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <Left> <C-o>:python ' . b:ConqueTerm_Var . '.write(u"\u001b[D")<CR>'
    else
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <BS>'
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <Space>'
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <Up>'
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <Down>'
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <Right>'
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <Left>'
    endif

    " send selected text into conque
    if l:action == 'start'
        sil exe 'v' . map_modifier . 'map <silent> ' . g:ConqueTerm_SendVisKey . ' :<C-u>call conque_term#send_selected(visualmode())<CR>'
    else
        sil exe 'v' . map_modifier . 'map <silent> ' . g:ConqueTerm_SendVisKey
    endif

    " remap paste keys
    if l:action == 'start'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> p :python ' . b:ConqueTerm_Var . '.write(vim.eval("@@"))<CR>a'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> P :python ' . b:ConqueTerm_Var . '.write(vim.eval("@@"))<CR>a'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> ]p :python ' . b:ConqueTerm_Var . '.write(vim.eval("@@"))<CR>a'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> [p :python ' . b:ConqueTerm_Var . '.write(vim.eval("@@"))<CR>a'
    else
        sil exe 'n' . map_modifier . 'map <silent> <buffer> p'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> P'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> ]p'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> [p'
    endif
    if has('gui_running')
        if l:action == 'start'
            sil exe 'i' . map_modifier . 'map <buffer> <S-Insert> <Esc>:<C-u>python ' . b:ConqueTerm_Var . ".write(vim.eval('@+'))<CR>a"
            sil exe 'i' . map_modifier . 'map <buffer> <S-Help> <Esc>:<C-u>python ' . b:ConqueTerm_Var . ".write(vim.eval('@+'))<CR>a"
        else
            sil exe 'i' . map_modifier . 'map <buffer> <S-Insert>'
            sil exe 'i' . map_modifier . 'map <buffer> <S-Help>'
        endif
    endif

    " attempt to map scroll wheel
    map  <buffer> <M-Esc>[62~ <MouseDown>
    map! <buffer> <M-Esc>[62~ <MouseDown>
    map  <buffer> <M-Esc>[63~ <MouseUp>
    map! <buffer> <M-Esc>[63~ <MouseUp>
    map  <buffer> <M-Esc>[64~ <S-MouseDown>
    map! <buffer> <M-Esc>[64~ <S-MouseDown>
    map  <buffer> <M-Esc>[65~ <S-MouseUp>
    map! <buffer> <M-Esc>[65~ <S-MouseUp>

    " disable other normal mode keys which insert text
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

    " user defined mappings
    for [map_from, map_to] in s:input_extra
        if l:action == 'start'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> ' . map_from . ' <C-o>:python ' . b:ConqueTerm_Var . ".write('" . conque_term#python_escape(map_to) . "')<CR>"
        else
            sil exe 'i' . map_modifier . 'map <silent> <buffer> ' . map_from
        endif
    endfor

    " set conque as on or off
    if l:action == 'start'
        let b:conque_on = 1
    else
        let b:conque_on = 0
    endif

    " map command to start stop the shell
    if a:action == 'start'
        nnoremap <F5> :<C-u>call conque_term#set_mappings('toggle')<CR>
    endif

endfunction " }}}

" send selected text from another buffer
function! conque_term#send_selected(type) "{{{
    let reg_save = @@

    " save user's sb settings
    let sb_save = &switchbuf
    set switchbuf=usetab

    " yank current selection
    sil exe "normal! `<" . a:type . "`>y"

    " format yanked text
    let @@ = substitute(@@, '^[\r\n]*', '', '')
    let @@ = substitute(@@, '[\r\n]*$', '', '')

    " execute yanked text
    sil exe ":sb " . g:ConqueTerm_BufName
    sil exe 'python ' . g:ConqueTerm_Var . '.paste_selection()'

    " reset original values
    let @@ = reg_save
    sil exe 'set switchbuf=' . sb_save

    " scroll buffer left
    startinsert!
    normal 0zH
endfunction "}}}

" read from all known conque buffers
function! conque_term#read_all(insert_mode) "{{{

    for i in range(1, g:ConqueTerm_Idx)
        try
            if !s:terminals[i].active
                continue
            endif

            let output = s:terminals[i].read(1)

            if !s:terminals[i].is_buffer && exists('*s:terminals[i].callback')
                call s:terminals[i].callback(output)
            endif
        catch
            " probably a deleted buffer
        endtry
    endfor

    " restart updatetime
    if a:insert_mode
        call feedkeys("\<C-o>f\e", "n")
    else
        call feedkeys("f\e", "n")
    endif

endfunction "}}}

" close all subprocesses
function! conque_term#close_all() "{{{

    for i in range(1, g:ConqueTerm_Idx)
        try
            call s:terminals[i].close()
        catch
            " probably a deleted buffer
        endtry
    endfor

endfunction "}}}

" util function to add enough \s to pass a string to python
function! conque_term#python_escape(input) "{{{
    let l:cleaned = a:input
    let l:cleaned = substitute(l:cleaned, '\\', '\\\\', 'g')
    let l:cleaned = substitute(l:cleaned, '\n', '\\n', 'g')
    let l:cleaned = substitute(l:cleaned, '\r', '\\r', 'g')
    let l:cleaned = substitute(l:cleaned, "'", "\\\\'", 'g')
    return l:cleaned
endfunction "}}}

" gets called when user enters conque buffer.
" Useful for making temp changes to global config
function! conque_term#on_focus() " {{{
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
endfunction " }}}

function! conque_term#bell() " {{{
    echohl WarningMsg | echomsg "BELL!" | echohl None
endfunction " }}}

" **********************************************************************************************************
" **** "API" functions *************************************************************************************
" **********************************************************************************************************

" See doc/conque_term.txt for full documentation

" Write to a conque terminal buffer
function! s:term_obj.write(text) dict " {{{

    " if we're not in terminal buffer, pass flag to not position the cursor
    sil exe 'python ' . self.var . '.write(vim.eval("a:text"), False, False)'

endfunction " }}}

" same as write() but adds a newline
function! s:term_obj.writeln(text) dict " {{{

    call self.write(a:text . "\n")

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
    sil exec ":python conque_tmp = " . self.var . ".read(timeout = " . read_time . ", set_cursor = False, return_output = True, update_buffer = " . up_py . ")"

    " ftw!
    python << EOF
if conque_tmp:
    conque_tmp = re.sub('\\\\', '\\\\\\\\', conque_tmp) 
    conque_tmp = re.sub('"', '\\\\"', conque_tmp)
    vim.command('let output = "' + conque_tmp + '"')
EOF

    return output

endfunction " }}}

" set output callback
function! s:term_obj.set_callback(callback_func) dict " {{{

    let s:terminals[self.idx].callback = function(a:callback_func)

endfunction " }}}

" close subprocess with ABORT signal
function! s:term_obj.close() dict " {{{

    sil exe 'python ' . self.var . '.proc.signal(1)'

    if self.is_buffer
        call conque_term#set_mappings('stop')
        if exists('g:ConqueTerm_CloseOnEnd') && g:ConqueTerm_CloseOnEnd
            sil exe 'bwipeout! ' . self.buffer_name
            stopinsert!
        endif
    endif

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

    let l:t_obj = copy(s:term_obj)
    let l:t_obj.is_buffer = is_buffer
    let l:t_obj.idx = buf_num
    let l:t_obj.buffer_name = bname
    let l:t_obj.var = pvar

    return l:t_obj

endfunction " }}}

" get an existing terminal instance
function! conque_term#get_instance(...) " {{{

    " find conque buffer to update
    let buf_num = get(a:000, 0, 0)

    if exists('s:terminals[buf_num]')
        
    elseif exists('b:ConqueTerm_Var')
        let buf_num = b:ConqueTerm_Idx
    else
        let buf_num = g:ConqueTerm_Idx
    endif

    return s:terminals[buf_num]

endfunction " }}}

" add a new default mapping
function! conque_term#imap(map_from, map_to) " {{{
    call add(s:input_extra, [a:map_from, a:map_to])
endfunction " }}}

" add a list of new default mappings
function! conque_term#imap_list(map_list) " {{{
    call extend(s:input_extra, a:map_list)
endfunction " }}}

" **********************************************************************************************************
" **** PYTHON **********************************************************************************************
" **********************************************************************************************************

python << EOF

import vim, re, time, math





# CONFIG CONSTANTS  {{{

CONQUE_CTL = {
     1:'soh', # start of heading
     2:'stx', # start of text
     7:'bel', # bell
     8:'bs',  # backspace
     9:'tab', # tab
    10:'nl',  # new line
    13:'cr'   # carriage return
}
#    11 : 'vt',  # vertical tab
#    12 : 'ff',  # form feed
#    14 : 'so',  # shift out
#    15 : 'si'   # shift in

# Escape sequences 
CONQUE_ESCAPE = { 
    'm':'font',
    'J':'clear_screen',
    'K':'clear_line',
    '@':'add_spaces',
    'A':'cursor_up',
    'B':'cursor_down',
    'C':'cursor_right',
    'D':'cursor_left',
    'G':'cursor_to_column',
    'H':'cursor',
    'P':'delete_chars',
    'f':'cursor',
    'g':'tab_clear',
    'r':'set_coords',
    'h':'set',
    'l':'reset'
}
#    'L':'insert_lines',
#    'M':'delete_lines',
#    'd':'cusor_vpos',

# Alternate escape sequences, no [
CONQUE_ESCAPE_PLAIN = {
    'D':'scroll_up',
    'E':'next_line',
    'H':'set_tab',
    'M':'scroll_down'
}
#    'N':'single_shift_2',
#    'O':'single_shift_3',
#    '=':'alternate_keypad',
#    '>':'numeric_keypad',
#    '7':'save_cursor',
#    '8':'restore_cursor',

# Uber alternate escape sequences, with # or ?
CONQUE_ESCAPE_QUESTION = {
    '1h':'new_line_mode',
    '3h':'132_cols',
    '4h':'smooth_scrolling',
    '5h':'reverse_video',
    '6h':'relative_origin',
    '7h':'set_auto_wrap',
    '8h':'set_auto_repeat',
    '9h':'set_interlacing_mode',
    '1l':'set_cursor_key',
    '2l':'set_vt52',
    '3l':'80_cols',
    '4l':'set_jump_scrolling',
    '5l':'normal_video',
    '6l':'absolute_origin',
    '7l':'reset_auto_wrap',
    '8l':'reset_auto_repeat',
    '9l':'reset_interlacing_mode'
}

CONQUE_ESCAPE_HASH = {
    '8':'screen_alignment_test'
} 
#    '3':'double_height_top',
#    '4':'double_height_bottom',
#    '5':'single_height_single_width',
#    '6':'single_height_double_width',

# Font codes {{{
CONQUE_FONT = {
    0: {'description':'Normal (default)', 'attributes': {'cterm':'NONE','ctermfg':'NONE','ctermbg':'NONE','gui':'NONE','guifg':'NONE','guibg':'NONE'}, 'normal':True},
    1: {'description':'Bold', 'attributes': {'cterm':'BOLD','gui':'BOLD'}, 'normal':False},
    4: {'description':'Underlined', 'attributes': {'cterm':'UNDERLINE','gui':'UNDERLINE'}, 'normal':False},
    5: {'description':'Blink (appears as Bold)', 'attributes': {'cterm':'BOLD','gui':'BOLD'}, 'normal':False},
    7: {'description':'Inverse', 'attributes': {'cterm':'REVERSE','gui':'REVERSE'}, 'normal':False},
    8: {'description':'Invisible (hidden)', 'attributes': {'ctermfg':'0','ctermbg':'0','guifg':'#000000','guibg':'#000000'}, 'normal':False},
    22: {'description':'Normal (neither bold nor faint)', 'attributes': {'cterm':'NONE','gui':'NONE'}, 'normal':True},
    24: {'description':'Not underlined', 'attributes': {'cterm':'NONE','gui':'NONE'}, 'normal':True},
    25: {'description':'Steady (not blinking)', 'attributes': {'cterm':'NONE','gui':'NONE'}, 'normal':True},
    27: {'description':'Positive (not inverse)', 'attributes': {'cterm':'NONE','gui':'NONE'}, 'normal':True},
    28: {'description':'Visible (not hidden)', 'attributes': {'ctermfg':'NONE','ctermbg':'NONE','guifg':'NONE','guibg':'NONE'}, 'normal':True},
    30: {'description':'Set foreground color to Black', 'attributes': {'ctermfg':'16','guifg':'#000000'}, 'normal':False},
    31: {'description':'Set foreground color to Red', 'attributes': {'ctermfg':'1','guifg':'#ff0000'}, 'normal':False},
    32: {'description':'Set foreground color to Green', 'attributes': {'ctermfg':'2','guifg':'#00ff00'}, 'normal':False},
    33: {'description':'Set foreground color to Yellow', 'attributes': {'ctermfg':'3','guifg':'#ffff00'}, 'normal':False},
    34: {'description':'Set foreground color to Blue', 'attributes': {'ctermfg':'4','guifg':'#0000ff'}, 'normal':False},
    35: {'description':'Set foreground color to Magenta', 'attributes': {'ctermfg':'5','guifg':'#990099'}, 'normal':False},
    36: {'description':'Set foreground color to Cyan', 'attributes': {'ctermfg':'6','guifg':'#009999'}, 'normal':False},
    37: {'description':'Set foreground color to White', 'attributes': {'ctermfg':'7','guifg':'#ffffff'}, 'normal':False},
    39: {'description':'Set foreground color to default (original)', 'attributes': {'ctermfg':'NONE','guifg':'NONE'}, 'normal':True},
    40: {'description':'Set background color to Black', 'attributes': {'ctermbg':'16','guibg':'#000000'}, 'normal':False},
    41: {'description':'Set background color to Red', 'attributes': {'ctermbg':'1','guibg':'#ff0000'}, 'normal':False},
    42: {'description':'Set background color to Green', 'attributes': {'ctermbg':'2','guibg':'#00ff00'}, 'normal':False},
    43: {'description':'Set background color to Yellow', 'attributes': {'ctermbg':'3','guibg':'#ffff00'}, 'normal':False},
    44: {'description':'Set background color to Blue', 'attributes': {'ctermbg':'4','guibg':'#0000ff'}, 'normal':False},
    45: {'description':'Set background color to Magenta', 'attributes': {'ctermbg':'5','guibg':'#990099'}, 'normal':False},
    46: {'description':'Set background color to Cyan', 'attributes': {'ctermbg':'6','guibg':'#009999'}, 'normal':False},
    47: {'description':'Set background color to White', 'attributes': {'ctermbg':'7','guibg':'#ffffff'}, 'normal':False},
    49: {'description':'Set background color to default (original).', 'attributes': {'ctermbg':'NONE','guibg':'NONE'}, 'normal':True},
    90: {'description':'Set foreground color to Black', 'attributes': {'ctermfg':'8','guifg':'#000000'}, 'normal':False},
    91: {'description':'Set foreground color to Red', 'attributes': {'ctermfg':'9','guifg':'#ff0000'}, 'normal':False},
    92: {'description':'Set foreground color to Green', 'attributes': {'ctermfg':'10','guifg':'#00ff00'}, 'normal':False},
    93: {'description':'Set foreground color to Yellow', 'attributes': {'ctermfg':'11','guifg':'#ffff00'}, 'normal':False},
    94: {'description':'Set foreground color to Blue', 'attributes': {'ctermfg':'12','guifg':'#0000ff'}, 'normal':False},
    95: {'description':'Set foreground color to Magenta', 'attributes': {'ctermfg':'13','guifg':'#990099'}, 'normal':False},
    96: {'description':'Set foreground color to Cyan', 'attributes': {'ctermfg':'14','guifg':'#009999'}, 'normal':False},
    97: {'description':'Set foreground color to White', 'attributes': {'ctermfg':'15','guifg':'#ffffff'}, 'normal':False},
    100: {'description':'Set background color to Black', 'attributes': {'ctermbg':'8','guibg':'#000000'}, 'normal':False},
    101: {'description':'Set background color to Red', 'attributes': {'ctermbg':'9','guibg':'#ff0000'}, 'normal':False},
    102: {'description':'Set background color to Green', 'attributes': {'ctermbg':'10','guibg':'#00ff00'}, 'normal':False},
    103: {'description':'Set background color to Yellow', 'attributes': {'ctermbg':'11','guibg':'#ffff00'}, 'normal':False},
    104: {'description':'Set background color to Blue', 'attributes': {'ctermbg':'12','guibg':'#0000ff'}, 'normal':False},
    105: {'description':'Set background color to Magenta', 'attributes': {'ctermbg':'13','guibg':'#990099'}, 'normal':False},
    106: {'description':'Set background color to Cyan', 'attributes': {'ctermbg':'14','guibg':'#009999'}, 'normal':False},
    107: {'description':'Set background color to White', 'attributes': {'ctermbg':'15','guibg':'#ffffff'}, 'normal':False}
} 
# }}}

# regular expression matching (almost) all control sequences
CONQUE_SEQ_REGEX       = re.compile(ur"(\u001b\[?\??#?[0-9;]*[a-zA-Z0-9@=>]|\u001b\][0-9];.*?\u0007|[\u0001-\u000f])", re.UNICODE)
CONQUE_SEQ_REGEX_CTL   = re.compile(ur"^[\u0001-\u000f]$", re.UNICODE)
CONQUE_SEQ_REGEX_CSI   = re.compile(ur"^\u001b\[", re.UNICODE)
CONQUE_SEQ_REGEX_TITLE = re.compile(ur"^\u001b\]", re.UNICODE)
CONQUE_SEQ_REGEX_HASH  = re.compile(ur"^\u001b#", re.UNICODE)
CONQUE_SEQ_REGEX_ESC   = re.compile(ur"^\u001b.$", re.UNICODE)

# match table output
CONQUE_TABLE_OUTPUT   = re.compile("^\s*\|\s.*\s\|\s*$|^\s*\+[=+-]+\+\s*$")

# }}}

###################################################################################################
class Conque:

    # CLASS PROPERTIES {{{ 

    # screen object
    screen          = None

    # subprocess object
    proc            = None

    # terminal dimensions and scrolling region
    columns         = 80 # same as $COLUMNS
    lines           = 24 # same as $LINES
    working_columns = 80 # can be changed by CSI ? 3 l/h
    working_lines   = 24 # can be changed by CSI r

    # top/bottom of the scroll region
    top             = 1  # relative to top of screen
    bottom          = 24 # relative to top of screen

    # cursor position
    l               = 1  # current cursor line
    c               = 1  # current cursor column

    # autowrap mode
    autowrap        = True

    # absolute coordinate mode
    absolute_coords = True

    # tabstop positions
    tabstops        = []

    # enable colors
    enable_colors = True

    # color changes
    color_changes = {}

    # color history
    color_history = {}

    # don't wrap table output
    unwrap_tables = True

    # wrap CUF/CUB around line breaks
    wrap_cursor = False

    # do we need to move the cursor?
    cursor_set = False

    # used for auto_read actions
    read_count = 0

    # }}}

    # constructor
    def __init__(self): # {{{
        self.screen = ConqueScreen()
        # }}}

    # start program and initialize this instance
    def open(self, command, options): # {{{

        # int vars
        self.columns = vim.current.window.width
        self.lines = vim.current.window.height
        self.working_columns = vim.current.window.width
        self.working_lines = vim.current.window.height
        self.bottom = vim.current.window.height

        # init color
        self.enable_colors = options['color']

        # init tabstops
        self.init_tabstops()

        # open command
        self.proc = ConqueSubprocess()
        self.proc.open(command, { 'TERM' : options['TERM'], 'CONQUE' : '1', 'LINES' : str(self.lines), 'COLUMNS' : str(self.columns)})

        # send window size signal, in case LINES/COLUMNS is ignored
        self.update_window_size(True)
        # }}}

    # write to pty
    def write(self, input, set_cursor = True, read = True): # {{{


        # check if window size has changed
        if read:
            self.update_window_size()

        # write and read
        self.proc.write(input)

        # read output immediately
        if read:
            self.read(1, set_cursor)
        # }}}

    # read from pty, and update buffer
    def read(self, timeout = 1, set_cursor = True, return_output = False, update_buffer = True): # {{{
        # read from subprocess
        output = self.proc.read(timeout)
        # and strip null chars
        output = output.replace(chr(0), '')

        if output == '':
            return

        # for bufferless terminals
        if not update_buffer:
            return output





        chunks = CONQUE_SEQ_REGEX.split(output)





        # don't go through all the csi regex if length is one (no matches)
        if len(chunks) == 1:

            self.plain_text(chunks[0])

        else:
            for s in chunks:
                if s == '':
                    continue






                # Check for control character match {{{
                if CONQUE_SEQ_REGEX_CTL.match(s[0]):

                    nr = ord(s[0])
                    if nr in CONQUE_CTL:
                        getattr(self, 'ctl_' + CONQUE_CTL[nr])()
                    else:

                        pass
                    # }}}

                # check for escape sequence match {{{
                elif CONQUE_SEQ_REGEX_CSI.match(s):

                    if s[-1] in CONQUE_ESCAPE:
                        csi = self.parse_csi(s[2:])

                        getattr(self, 'csi_' + CONQUE_ESCAPE[s[-1]])(csi)
                    else:

                        pass
                    # }}}
        
                # check for title match {{{
                elif CONQUE_SEQ_REGEX_TITLE.match(s):

                    self.change_title(s[2], s[4:-1])
                    # }}}
        
                # check for hash match {{{
                elif CONQUE_SEQ_REGEX_HASH.match(s):

                    if s[-1] in CONQUE_ESCAPE_HASH:
                        getattr(self, 'hash_' + CONQUE_ESCAPE_HASH[s[-1]])()
                    else:

                        pass
                    # }}}
                
                # check for other escape match {{{
                elif CONQUE_SEQ_REGEX_ESC.match(s):

                    if s[-1] in CONQUE_ESCAPE_PLAIN:
                        getattr(self, 'esc_' + CONQUE_ESCAPE_PLAIN[s[-1]])()
                    else:

                        pass
                    # }}}
                
                # else process plain text {{{
                else:
                    self.plain_text(s)
                    # }}}

        # check window size
        if set_cursor:
          self.screen.set_cursor(self.l, self.c)
        
        # we need to set the cursor position
        self.cursor_set = False

        vim.command('redraw')



        if return_output:
            return output
    # }}}

    # for polling
    def auto_read(self): # {{{

        # check subprocess status, but not every time since it's CPU expensive
        if self.read_count == 10:
            if not self.proc.is_alive():
                vim.command('call conque_term#get_instance().close()')
                return
            else:
                self.read_count = 0
        self.read_count += 1

        # read output
        self.read(1)

        # reset timer
        if self.c == 1:
            vim.command('call feedkeys("\<right>\<left>", "n")')
        else:
            vim.command('call feedkeys("\<left>\<right>", "n")')

        # stop here if cursor doesn't need to be moved
        if self.cursor_set:
            return
        
        # otherwise set cursor position
        self.screen.set_cursor(self.l, self.c)
        self.cursor_set = True
    # }}}

    ###############################################################################################
    # Plain text # {{{

    def plain_text(self, input):

        current_line = self.screen[self.l]

        if len(current_line) < self.working_columns:
            current_line = current_line + ' ' * (self.c - len(current_line))

        # if line is wider than screen
        if self.c + len(input) - 1 > self.working_columns:
            # Table formatting hack
            if self.unwrap_tables and CONQUE_TABLE_OUTPUT.match(input):
                self.screen[self.l] = current_line[ : self.c - 1] + input + current_line[ self.c + len(input) - 1 : ]
                self.apply_color(self.c, self.c + len(input))
                self.c += len(input)
                return

            diff = self.c + len(input) - self.working_columns - 1
            # if autowrap is enabled
            if self.autowrap:
                self.screen[self.l] = current_line[ : self.c - 1] + input[ : -1 * diff ]
                self.apply_color(self.c, self.working_columns)
                self.ctl_nl()
                self.ctl_cr()
                remaining = input[ -1 * diff : ]

                self.plain_text(remaining)
            else:
                self.screen[self.l] = current_line[ : self.c - 1] + input[ : -1 * diff - 1 ] + input[-1]
                self.apply_color(self.c, self.working_columns)
                self.c = self.working_columns

        # no autowrap
        else:
            self.screen[self.l] = current_line[ : self.c - 1] + input + current_line[ self.c + len(input) - 1 : ]
            self.apply_color(self.c, self.c + len(input))
            self.c += len(input)

    def apply_color(self, start, end):


        # stop here if coloration is disabled
        if not self.enable_colors:
            return

        real_line = self.screen.get_real_line(self.l)

        # check for previous overlapping coloration

        to_del = []
        if self.color_history.has_key(real_line):
            for i in range(len(self.color_history[real_line])):
                syn = self.color_history[real_line][i]

                if syn['start'] >= start and syn['start'] < end:

                    vim.command('syn clear ' + syn['name'])
                    to_del.append(i)
                    # outside
                    if syn['end'] > end:

                        self.exec_highlight(real_line, end, syn['end'], syn['highlight'])
                elif syn['end'] > start and syn['end'] <= end:

                    vim.command('syn clear ' + syn['name'])
                    to_del.append(i)
                    # outside
                    if syn['start'] < start:

                        self.exec_highlight(real_line, syn['start'], start, syn['highlight'])

        if len(to_del) > 0:
            to_del.reverse()
            for di in to_del:
                del self.color_history[real_line][di]

        # if there are no new colors
        if len(self.color_changes) == 0:
            return

        highlight = ''
        for attr in self.color_changes.keys():
            highlight = highlight + ' ' + attr + '=' + self.color_changes[attr]

        # execute the highlight
        self.exec_highlight(real_line, start, end, highlight)

    def exec_highlight(self, real_line, start, end, highlight):
        unique_key = str(self.proc.pid)

        syntax_name = 'EscapeSequenceAt_' + unique_key + '_' + str(self.l) + '_' + str(start) + '_' + str(len(self.color_history) + 1)
        syntax_options = ' contains=ALLBUT,ConqueString,MySQLString,MySQLKeyword oneline'
        syntax_region = 'syntax match ' + syntax_name + ' /\%' + str(real_line) + 'l\%>' + str(start - 1) + 'c.*\%<' + str(end + 1) + 'c/' + syntax_options
        syntax_highlight = 'highlight ' + syntax_name + highlight

        vim.command(syntax_region)
        vim.command(syntax_highlight)

        # add syntax name to history
        if not self.color_history.has_key(real_line):
            self.color_history[real_line] = []

        self.color_history[real_line].append({'name':syntax_name, 'start':start, 'end':end, 'highlight':highlight})

    # }}}

    ###############################################################################################
    # Control functions {{{

    def ctl_nl(self):
        # if we're in a scrolling region, scroll instead of moving cursor down
        if self.lines != self.working_lines and self.l == self.bottom:
            del self.screen[self.top]
            self.screen.insert(self.bottom, '')
        elif self.l == self.bottom:
            self.screen.append('')
        else:
            self.l += 1

        self.color_changes = {}

    def ctl_cr(self):
        self.c = 1

        self.color_changes = {}

    def ctl_bs(self):
        if self.c > 1:
            self.c += -1

    def ctl_soh(self):
        pass

    def ctl_stx(self):
        pass

    def ctl_bel(self):
        vim.command('call conque_term#bell()')

    def ctl_tab(self):
        # default tabstop location
        ts = self.working_columns

        # check set tabstops
        for i in range(self.c, len(self.tabstops)):
            if self.tabstops[i]:
                ts = i + 1
                break



        self.c = ts

    # }}}

    ###############################################################################################
    # CSI functions {{{

    def csi_font(self, csi): # {{{
        if not self.enable_colors:
            return
        
        # defaults to 0
        if len(csi['vals']) == 0:
            csi['vals'] = [0]

        # 256 xterm color foreground
        if len(csi['vals']) == 3 and csi['vals'][0] == 38 and csi['vals'][1] == 5:
            self.color_changes['ctermfg'] = str(csi['vals'][2])
            self.color_changes['guifg'] = '#' + self.xterm_to_rgb(csi['vals'][2])

        # 256 xterm color background
        elif len(csi['vals']) == 3 and csi['vals'][0] == 48 and csi['vals'][1] == 5:
            self.color_changes['ctermbg'] = str(csi['vals'][2])
            self.color_changes['guibg'] = '#' + self.xterm_to_rgb(csi['vals'][2])

        # 16 colors
        else:
            for val in csi['vals']:
                if CONQUE_FONT.has_key(val):

                    # ignore starting normal colors
                    if CONQUE_FONT[val]['normal'] and len(self.color_changes) == 0:

                        continue
                    # clear color changes
                    elif CONQUE_FONT[val]['normal']:

                        self.color_changes = {}
                    # save these color attributes for next plain_text() call
                    else:

                        for attr in CONQUE_FONT[val]['attributes'].keys():
                            if self.color_changes.has_key(attr) and (attr == 'cterm' or attr == 'gui'):
                                self.color_changes[attr] += ',' + CONQUE_FONT[val]['attributes'][attr]
                            else:
                                self.color_changes[attr] = CONQUE_FONT[val]['attributes'][attr]
        # }}}

    def csi_clear_line(self, csi): # {{{


        # this escape defaults to 0
        if len(csi['vals']) == 0:
            csi['val'] = 0




        # 0 means cursor right
        if csi['val'] == 0:
            self.screen[self.l] = self.screen[self.l][0 : self.c - 1]

        # 1 means cursor left
        elif csi['val'] == 1:
            self.screen[self.l] = ' ' * (self.c) + self.screen[self.l][self.c : ]

        # clear entire line
        elif csi['val'] == 2:
            self.screen[self.l] = ''

        # clear colors
        if csi['val'] == 2 or (csi['val'] == 0 and self.c == 1):
            real_line = self.screen.get_real_line(self.l)
            if self.color_history.has_key(real_line):
                for syn in self.color_history[real_line]:
                    vim.command('syn clear ' + syn['name'])



        # }}}

    def csi_cursor_right(self, csi): # {{{
        # we use 1 even if escape explicitly specifies 0
        if csi['val'] == 0:
            csi['val'] = 1




        if self.wrap_cursor and self.c + csi['val'] > self.working_columns:
            self.l += int(math.floor( (self.c + csi['val']) / self.working_columns ))
            self.c = (self.c + csi['val']) % self.working_columns
            return

        self.c = self.bound(self.c + csi['val'], 1, self.working_columns)
        # }}}

    def csi_cursor_left(self, csi): # {{{
        # we use 1 even if escape explicitly specifies 0
        if csi['val'] == 0:
            csi['val'] = 1

        if self.wrap_cursor and csi['val'] >= self.c:
            self.l += int(math.floor( (self.c - csi['val']) / self.working_columns ))
            self.c = self.working_columns - (csi['val'] - self.c) % self.working_columns
            return

        self.c = self.bound(self.c - csi['val'], 1, self.working_columns)
        # }}}

    def csi_cursor_to_column(self, csi): # {{{
        self.c = self.bound(csi['val'], 1, self.working_columns)
        # }}}

    def csi_cursor_up(self, csi): # {{{
        self.l = self.bound(self.l - csi['val'], self.top, self.bottom)

        self.color_changes = {}
        # }}}

    def csi_cursor_down(self, csi): # {{{
        self.l = self.bound(self.l + csi['val'], self.top, self.bottom)

        self.color_changes = {}
        # }}}

    def csi_clear_screen(self, csi): # {{{
        # default to 0
        if len(csi['vals']) == 0:
            csi['val'] = 0

        # 2 == clear entire screen
        if csi['val'] == 2:
            self.l = 1
            self.c = 1
            self.screen.clear()

        # 0 == clear down
        elif csi['val'] == 0:
            for l in range(self.bound(self.l + 1, 1, self.lines), self.lines + 1):
                self.screen[l] = ''
            
            # clear end of current line
            self.csi_clear_line(self.parse_csi('K'))

        # 1 == clear up
        elif csi['val'] == 1:
            for l in range(1, self.bound(self.l, 1, self.lines + 1)):
                self.screen[l] = ''

            # clear beginning of current line
            self.csi_clear_line(self.parse_csi('1K'))

        # clear coloration
        if csi['val'] == 2 or csi['val'] == 0:
            real_line = self.screen.get_real_line(self.l)
            for line in self.color_history.keys():
                if line >= real_line:
                    for syn in self.color_history[line]:
                        vim.command('syn clear ' + syn['name'])

        self.color_changes = {}
        # }}}

    def csi_delete_chars(self, csi): # {{{
        self.screen[self.l] = self.screen[self.l][ : self.c ] + self.screen[self.l][ self.c + csi['val'] : ]
        # }}}

    def csi_add_spaces(self, csi): # {{{
        self.screen[self.l] = self.screen[self.l][ : self.c - 1] + ' ' * csi['val'] + self.screen[self.l][self.c : ]
        # }}}

    def csi_cursor(self, csi): # {{{
        if len(csi['vals']) == 2:
            new_line = csi['vals'][0]
            new_col = csi['vals'][1]
        else:
            new_line = 1
            new_col = 1

        if self.absolute_coords:
            self.l = self.bound(new_line, 1, self.lines)
        else:
            self.l = self.bound(self.top + new_line - 1, self.top, self.bottom)

        self.c = self.bound(new_col, 1, self.working_columns)
        if self.c > len(self.screen[self.l]):
            self.screen[self.l] = self.screen[self.l] + ' ' * (self.c - len(self.screen[self.l]))

        # }}}

    def csi_set_coords(self, csi): # {{{
        if len(csi['vals']) == 2:
            new_start = csi['vals'][0]
            new_end = csi['vals'][1]
        else:
            new_start = 1
            new_end = vim.current.window.height

        self.top = new_start
        self.bottom = new_end
        self.working_lines = new_end - new_start + 1

        # if cursor is outside scrolling region, reset it
        if self.l < self.top:
            self.l = self.top
        elif self.l > self.bottom:
            self.l = self.bottom

        self.color_changes = {}
        # }}}

    def csi_tab_clear(self, csi): # {{{
        # this escape defaults to 0
        if len(csi['vals']) == 0:
            csi['val'] = 0



        if csi['val'] == 0:
            self.tabstops[self.c - 1] = False
        elif csi['val'] == 3:
            for i in range(0, self.columns + 1):
                self.tabstops[i] = False
        # }}}

    def csi_set(self, csi): # {{{
        # 132 cols
        if csi['val'] == 3: 
            self.csi_clear_screen(self.parse_csi('2J'))
            self.working_columns = 132

        # relative_origin
        elif csi['val'] == 6: 
            self.absolute_coords = False

        # set auto wrap
        elif csi['val'] == 7: 
            self.autowrap = True


        self.color_changes = {}
        # }}}

    def csi_reset(self, csi): # {{{
        # 80 cols
        if csi['val'] == 3: 
            self.csi_clear_screen(self.parse_csi('2J'))
            self.working_columns = 80

        # absolute origin
        elif csi['val'] == 6: 
            self.absolute_coords = True

        # reset auto wrap
        elif csi['val'] == 7: 
            self.autowrap = False


        self.color_changes = {}
        # }}}

    # }}}

    ###############################################################################################
    # ESC functions {{{

    def esc_scroll_up(self): # {{{
        self.ctl_nl()

        self.color_changes = {}
        # }}}

    def esc_next_line(self): # {{{
        self.ctl_nl()
        self.c = 1
        # }}}

    def esc_set_tab(self): # {{{

        if self.c <= len(self.tabstops):
            self.tabstops[self.c - 1] = True
        # }}}

    def esc_scroll_down(self): # {{{
        if self.l == self.top:
            del self.screen[self.bottom]
            self.screen.insert(self.top, '')
        else:
            self.l += -1

        self.color_changes = {}
        # }}}

    # }}}

    ###############################################################################################
    # HASH functions {{{

    def hash_screen_alignment_test(self): # {{{
        self.csi_clear_screen(self.parse_csi('2J'))
        self.working_lines = self.lines
        for l in range(1, self.lines + 1):
            self.screen[l] = 'E' * self.working_columns
        # }}}

    # }}}

    ###############################################################################################
    # Random stuff {{{

    def change_title(self, key, val):


        if key == '0' or key == '2':

            vim.command('setlocal statusline=' + re.escape(val))

    def paste(self):
        self.write(vim.eval('@@'))
        self.read(50)

    def paste_selection(self):
        self.write(vim.eval('@@'))

    def update_window_size(self, force = False):
        # resize if needed
        if force or vim.current.window.width != self.columns or vim.current.window.height != self.lines:

            # reset all window size attributes to default
            self.columns = vim.current.window.width
            self.lines = vim.current.window.height
            self.working_columns = vim.current.window.width
            self.working_lines = vim.current.window.height
            self.bottom = vim.current.window.height

            # reset screen object attributes
            self.l = self.screen.reset_size(self.l)

            # reset tabstops
            self.init_tabstops()



            # signal process that screen size has changed
            self.proc.window_resize(self.lines, self.columns)

    def insert_enter(self):

        # check window size
        self.update_window_size()
        
        # we need to set the cursor position
        self.cursor_set = False


    def init_tabstops(self):
        for i in range(0, self.columns + 1):
            if i % 8 == 0:
                self.tabstops.append(True)
            else:
                self.tabstops.append(False)

    # }}}

    ###############################################################################################
    # Utility {{{
    
    def parse_csi(self, s): # {{{
        attr = { 'key' : s[-1], 'flag' : '', 'val' : 1, 'vals' : [] }

        if len(s) == 1:
            return attr

        full = s[0:-1]

        if full[0] == '?':
            full = full[1:]
            attr['flag'] = '?'

        if full != '':
            vals = full.split(';')
            for val in vals:

                val = re.sub("\D", "", val)

                if val != '':
                    attr['vals'].append(int(val))

        if len(attr['vals']) == 1:
            attr['val'] = int(attr['vals'][0])
            
        return attr
        # }}}

    def bound(self, val, min, max): # {{{
        if val > max:
            return max

        if val < min:
            return min

        return val
        # }}}

    def xterm_to_rgb(self, color_code): # {{{
        if color_code < 16:
            ascii_colors = ['000000', 'CD0000', '00CD00', 'CDCD00', '0000EE', 'CD00CD', '00CDCD', 'E5E5E5', 
                   '7F7F7F', 'FF0000', '00FF00', 'FFFF00', '5C5CFF', 'FF00FF', '00FFFF', 'FFFFFF']
            return ascii_colors[color_code]

        elif color_code < 232:
            cc = int(color_code) - 16

            p1 = "%02x" % (math.floor(cc / 36) * (255/5))
            p2 = "%02x" % (math.floor((cc % 36) / 6) * (255/5))
            p3 = "%02x" % (math.floor(cc % 6) * (255/5))

            return p1 + p2 + p3
        else:
            grey_tone = "%02x" % math.floor((255/24) * (color_code - 232))
            return grey_tone + grey_tone + grey_tone
        # }}}

    # }}}

"""

ConqueSubprocess

Create and interact with a subprocess through a pty.

Usage:

    p = ConqueSubprocess()
    p.open('bash', {'TERM':'vt100'})
    output = p.read()
    p.write('cd ~/vim' + "\r")
    p.write('ls -lha' + "\r")
    output += p.read(timeout = 500)
    p.close()

"""

import os, signal, pty, tty, select, fcntl, termios, struct

class ConqueSubprocess:

    # process id
    pid = 0
    
    # stdout+stderr file descriptor
    fd = None

    # constructor
    def __init__(self): # {{{
        self.pid = 0
        # }}}

    # create pty + subprocess
    def open(self, command, env = {}): # {{{

        # parse command
        command_arr  = command.split()
        executable   = command_arr[0]
        args         = command_arr

        # try to fork a new pty
        try:
            self.pid, self.fd = pty.fork()

        except:

            return False

        # child proc, replace with command after altering terminal attributes
        if self.pid == 0:

            # set requested environment variables
            for k in env.keys():
                os.environ[k] = env[k]

            # set some attributes
            try:
                attrs = tty.tcgetattr(1)
                attrs[0] = attrs[0] ^ tty.IGNBRK
                attrs[0] = attrs[0] | tty.BRKINT | tty.IXANY | tty.IMAXBEL
                attrs[2] = attrs[2] | tty.HUPCL
                attrs[3] = attrs[3] | tty.ICANON | tty.ECHO | tty.ISIG | tty.ECHOKE
                attrs[6][tty.VMIN]  = 1
                attrs[6][tty.VTIME] = 0
                tty.tcsetattr(1, tty.TCSANOW, attrs)
            except:

                pass

            # replace this process with the subprocess
            os.execvp(executable, args)

        # else master, do nothing
        else:
            pass

        # }}}

    # read from pty
    # XXX - select.poll() doesn't work in OS X!!!!!!!
    def read(self, timeout = 1): # {{{

        output = ''
        read_timeout = float(timeout) / 1000

        try:
            # read from fd until no more output
            while 1:
                s_read, s_write, s_error = select.select( [ self.fd ], [], [], read_timeout)

                lines = ''
                for s_fd in s_read:
                    try:
                        lines = os.read( self.fd, 32 )
                    except:
                        pass
                    output = output + lines

                if lines == '':
                    break
        except:
            pass

        return output
        # }}}

    # I guess this one's not bad
    def write(self, input): # {{{
        try:
            os.write(self.fd, input)
        except:
            pass
        # }}}

    # signal process
    def signal(self, signum): # {{{
        try:
            os.kill(self.pid, signum)
        except:
            pass
        # }}}

    # close process
    def close(self): # {{{
        self.signal(15)
        # }}}

    # get process status
    def is_alive(self): #{{{

        p_status = True

        try:
            if os.waitpid( self.pid, os.WNOHANG )[0]:
                p_status = False
        except:
            p_status = False

        return p_status

        # }}}

    # update window size in kernel, then send SIGWINCH to fg process
    def window_resize(self, lines, columns): # {{{
        try:
            fcntl.ioctl(self.fd, termios.TIOCSWINSZ, struct.pack("HHHH", lines, columns, 0, 0))
            os.kill(self.pid, signal.SIGWINCH)
        except:
            pass

        # }}}


###################################################################################################
# ConqueScreen is an extention of the vim.current.buffer object
# It restricts the working indices of the buffer object to the scroll region which pty is expecting
# It also uses 1-based indexes, to match escape sequence commands
#
# E.g.:
#   s = ConqueScreen()
#   ...
#   s[5] = 'Set 5th line in terminal to this line'
#   s.append('Add new line to terminal')
#   s[5] = 'Since previous append() command scrolled the terminal down, this is a different line than first cb[5] call'
#

import vim

class ConqueScreen(object):

    # CLASS PROPERTIES  {{{

    # the buffer
    buffer          = None

    # screen and scrolling regions
    screen_top      = 1

    # screen width
    screen_width    = 80
    screen_height    = 80

    # }}}

    def __init__(self): # {{{
        self.buffer = vim.current.buffer

        self.screen_top = 1
        self.screen_width = vim.current.window.width
        self.screen_height = vim.current.window.height
    # }}}

    ###############################################################################################
    # List overload {{{
    def __len__(self): # {{{
        return len(self.buffer)
    # }}}

    def __getitem__(self, key): # {{{
        real_line = self.get_real_idx(key)

        # if line is past buffer end, add lines to buffer
        if real_line >= len(self.buffer):
            for i in range(len(self.buffer), real_line + 1):
                self.append(' ' * self.screen_width)

        return self.buffer[ real_line ]
    # }}}

    def __setitem__(self, key, value): # {{{
        real_line = self.get_real_idx(key)

        # if line is past end of screen, append
        if real_line == len(self.buffer):
            self.buffer.append(value)
        else:
            self.buffer[ real_line ] = value
    # }}}

    def __delitem__(self, key): # {{{
        del self.buffer[ self.screen_top + key - 2 ]
    # }}}

    def append(self, value): # {{{
        if len(self.buffer) > self.screen_top + self.screen_height - 1:
            self.buffer[len(self.buffer) - 1] = value
        else:
            self.buffer.append(value)

        if len(self.buffer) > self.screen_top + self.screen_height - 1:
            self.screen_top += 1
        if vim.current.buffer.number == self.buffer.number:
            vim.command('normal G')
    # }}}

    def insert(self, line, value): # {{{

        l = self.screen_top + line - 2
        self.buffer[l:l] = [ value ]
    
    # }}}
    # }}}

    ###############################################################################################
    # Util {{{
    def get_top(self): # {{{
        return self.screen_top
    # }}}

    def get_real_idx(self, line): # {{{
        return (self.screen_top + line - 2)
    # }}}

    def get_real_line(self, line): # {{{
        return (self.screen_top + line - 1)
    # }}}

    def set_screen_width(self, width): # {{{
        self.screen_width = width
    # }}}

    # }}}

    ###############################################################################################
    def clear(self): # {{{
        self.buffer.append(' ')
        vim.command('normal Gzt')
        self.screen_top = len(self.buffer)
    # }}}

    def set_cursor(self, line, column): # {{{
        # figure out line
        real_line =  self.screen_top + line - 1
        if real_line > len(self.buffer):
            for l in range(len(self.buffer) - 1, real_line):
                self.buffer.append('')

        # figure out column
        real_column = column
        if len(self.buffer[real_line - 1]) < real_column:
            self.buffer[real_line - 1] = self.buffer[real_line - 1] + ' ' * (real_column - len(self.buffer[real_line - 1]))

        # python version is occasionally grumpy
        try:
            vim.current.window.cursor = (real_line, real_column - 1)
        except:
            vim.command('call cursor(' + str(real_line) + ', ' + str(real_column) + ')')
    # }}}

    def reset_size(self, line): # {{{




        # save cursor line number
        real_line = self.screen_top + line

        # reset screen size
        self.screen_width = vim.current.window.width
        self.screen_height = vim.current.window.height
        self.screen_top = len(self.buffer) - vim.current.window.height + 1
        if self.screen_top < 1:
            self.screen_top = 1


        # align bottom of buffer to bottom of screen
        vim.command('normal ' + str(self.screen_height) + 'kG')

        # return new relative line number
        return (real_line - self.screen_top)
    # }}}

    def scroll_to_bottom(self): # {{{
        vim.current.window.cursor = (len(self.buffer) - 1, 1)
    # }}}
        
    def align(self): # {{{
        # align bottom of buffer to bottom of screen
        vim.command('normal ' + str(self.screen_height) + 'kG')
    # }}}


EOF

doc/conque_term.txt	[[[1
392
*ConqueTerm* Plugin to run a shell inside a Vim buffer

The ConqueTerm plugin will turn a Vim buffer into a terminal emulator, allowing
you to run and interact with a shell or shell application inside the buffer.

1. Installation     |conque-term-installation|
2. Usage            |conque-term-usage|
3. Config Options   |conque-term-options|
4. VimScript API    |conque-term-api|
5. Misc             |conque-term-misc|

==============================================================================

1. Installation                                     *conque-term-installation*

1.1 Requirements                                    *conque-term-requirements*

 * Vim 7.0+ with +python
 * Python 2.3+
 * Unix-like OS: Linux, OS X, Solaris, Cygwin, etc

If you're compiling Vim from source, be sure to use the --enable-pythoninterp
option. Otherwise check your OS's package distro for a version of Vim with
Python support. On OS X the best option is MacVim.

Windows support is not currently available, but is in developement. A windows 
version is scheduled to be release Fall 2010.

1.2 Installation                                         *conque-term-install*

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

Type :ConqueTerm <command> to launch an application in the current buffer. Eg:
>
    :ConqueTerm bash
    :ConqueTerm mysql -h localhost -u joe_lunchbox Menu
    :ConqueTerm man top
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


2.1 Special keys                                    *conque-term-special-keys*

There are several keys which can be configured to have special behavior with
Conque.

Send text to Conque                                           *conque-term-F9*

If you want to send some text from a file you are editing in another buffer
to be run in Conque, select the desired text visually then press the <F9> 
key. If you have multiple Conque buffers, the text will be sent to the most
recently created buffer. Alternatively you can yank the text, switch to your
terminal, then paste it with the normal 'p' key. This feature can be 
configured to use a different key with the |ConqueTerm_SendVisKey| option.

Sending the <Esc> key press                                  *conque-term-Esc*

By default if you press the <Esc> key in a Conque buffer you will leave insert
mode. But what if you want the key press to be sent to your terminal? There 
are two options. By default, pressing <Esc> twice will send one <Esc> key
press to the terminal, while pressing it once will leave insert mode.

Alternatively you can use the |ConqueTerm_EscKey| option to choose a
different key for leaving insert mode. If a custom key is set, then all <Esc> 
key presses will be sent to the terminal.


==============================================================================

3. Options                                               *conque-term-options*

You can set the following options in your .vimrc (default values shown)


3.1 Insert mode when entering buffer                *ConqueTerm_InsertOnEnter*

If set to 1 then you will automatically go into insert mode when you enter the
buffer. This diverges from normal Vim behavior. If 0 you will still be in
normal mode.
>
    let g:ConqueTerm_InsertOnEnter = 0
<
3.2 Enable <C-w> in insert mode                          *ConqueTerm_CWInsert*

If set to 1 then you can leave the Conque buffer using the <C-w> commands
while you're still in insert mode. If set to 0 then the <C-w> character will
be sent to the terminal. If both this option and ConqueTerm_InsertOnEnter are
set you can go in and out of the terminal buffer while never leaving insert
mode.
>
    let g:ConqueTerm_CWInsert = 0
<
3.3 Use a custom key for leaving insert mode               *ConqueTerm_EscKey*

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
3.4 Send selected text to Conque                       *ConqueTerm_SendVisKey*

Use this key to send the currently selected text to the most recently created
Conque buffer.
>
    let g:ConqueTerm_SendVisKey = '<F9>'
<
3.5 Enable or disable colors                                *ConqueTerm_Color*

Set to 1 to enable colors, 0 to disable. Syntax highlighting in Vim can be
slow if your terminal is color intensive. Disabling color can make the
terminal render significantly faster.
>
    let g:ConqueTerm_Color = 1
<
3.6 Choose your terminal type                                *ConqueTerm_TERM*

Use this option to tell Conque what type of terminal it should identify itself
as. Conque officially uses the more limited VT100 terminal type for
developement and testing, although it supports some more advanced features
such as colors and title strings.

You can change this setting to a more advanced type, namely 'xterm', but your
results may vary depending on which programs you're running.
>
    let g:ConqueTerm_TERM = 'vt100'
<
3.7 Choose Vim syntax type                                 *ConqueTerm_Syntax*

Set the buffer syntax. The default 'conque' has highlighting for MySQL, but 
not much else.
>
    let g:ConqueTerm_Syntax = 'conque'
<
3.8 Keep updating terminal buffer                   *ConqueTerm_ReadUnfocused*

If set to 1 then your Conque buffers will continue to update after you've
switched to another buffer.

Note: Conque buffers may continue to update, but they will not scroll down as
new lines are added beyond the bottom of the visible buffer area. This is a
limitation of the Vim scripting language for which I haven't found a 
workaround.
>
    let g:ConqueTerm_ReadUnfocused = 1
<
3.9 Regex for highlighting your prompt                *ConqueTerm_PromptRegex*

Use this regular expression for sytax highlighting your terminal prompt. Your
terminal will generally run faster if you use Vim highlighting instead of 
terminal colors for your prompt. You can also use it to do more advanced
syntax highlighting for the prompt line.

Setting this to an empty string ('') will disable prompt highlighting.
>
    let g:ConqueTerm_PromptRegex = '^\w\+@[0-9A-Za-z_.-]\+:[0-9A-Za-z_./\~,:-]\+\$'
<
3.10 Close buffer when program exits                   *ConqueTerm_CloseOnEnd*

If you want your terminal buffer to be closed and permanently deleted when the 
program running inside of it exits, set this option to 1. Otherwise the buffer
will become a simple text buffer after the program exits, and you can edit the
program output in insert mode.
>
    let g:ConqueTerm_CloseOnEnd = 0
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
4.4 CONQUE_OBJECT.write(text)                              *conque-term-write*

Once you have a terminal object from open(), subprocess() or get_instance() 
you can send text input to it with the write() method.

No return value.

Examples:
>
    call my_terminal.write("whoami\n")
    call my_terminal.write("\<C-c>")
<
4.5 CONQUE_OBJECT.writeln(text)                          *conque-term-writeln*

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

 - todo.txt (Autocommands, proposed event list, line ~3189)
     InsertCharPre   - user typed character Insert mode, before inserting the
     char.  Pattern is matched with text before the cursor. Set v:char to the 
     character, can be changed. (not triggered when 'paste' is set).


5.3 Feedback                                            *conque-term-feedback*

Bugs, suggestions and patches are all welcome.

For more information visit http://conque.googlecode.com

Check out the latest from svn at http://conque.googlecode.com/svn/trunk/

 vim:tw=78:ts=8:ft=help:norl:
plugin/conque_term.vim	[[[1
109
" FILE:     plugin/conque_term.vim {{{
" AUTHOR:   Nico Raffo <nicoraffo@gmail.com>
" MODIFIED: 2010-10-10
" VERSION:  1.2, for Vim 7.0
" LICENSE:
" Conque - pty interaction in Vim
" Copyright (C) 2009-2010 Nico Raffo 
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

" See docs/conque_term.txt for help or type :help conque_term

if exists('g:ConqueTerm_Loaded') || v:version < 700
    finish
endif

" **********************************************************************************************************
" **** CONFIG **********************************************************************************************
" **********************************************************************************************************

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

" Use this key to send selected text to conque. {{{
if !exists('g:ConqueTerm_SendVisKey')
    let g:ConqueTerm_SendVisKey = '<F9>'
endif " }}}

" Enable color. {{{
" If your apps use a lot of color it will slow down the shell.
if !exists('g:ConqueTerm_Color')
    let g:ConqueTerm_Color = 1
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

" Automatically close buffer when program exits {{{
if !exists('g:ConqueTerm_CloseOnEnd')
    let g:ConqueTerm_CloseOnEnd = 0
endif " }}}

" **********************************************************************************************************
" **** Startup *********************************************************************************************
" **********************************************************************************************************

" Startup {{{

let g:ConqueTerm_Loaded = 1
let g:ConqueTerm_Idx = 0
let g:ConqueTerm_Version = 120

command! -nargs=+ -complete=shellcmd ConqueTerm call conque_term#open(<q-args>)
command! -nargs=+ -complete=shellcmd ConqueTermSplit call conque_term#open(<q-args>, ['belowright split'])
command! -nargs=+ -complete=shellcmd ConqueTermVSplit call conque_term#open(<q-args>, ['belowright vsplit'])
command! -nargs=+ -complete=shellcmd ConqueTermTab call conque_term#open(<q-args>, ['tabnew'])

" }}}

syntax/conque_term.vim	[[[1
83

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
