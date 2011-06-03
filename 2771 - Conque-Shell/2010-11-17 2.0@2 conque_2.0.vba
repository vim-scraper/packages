" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/conque_term.txt	[[[1
462
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


1.3 Installation                       *conque-term-installation-instructions*

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

Toggle terminal input mode                                    *conque-term-F8*

If you want to use insert mode to edit the terminal screen, press <F8>. You
will now be able to edit the terminal output freely without your cursor
jumping the the active prompt line. This may be useful if you want to reformat
terminal output for readability. 

While the terminal is paused new output will not be displayed on the screen
until you press <F8> again to resume.

You can configure Conque to use a different key with the |ConqueTerm_ToggleKey| 
option.

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
3.5 Toggle terminal input mode                          *ConqueTerm_ToggleKey*

Press this key to pause terminal input and output display. You will then be
able to edit the terminal screen as if it were a normal text buffer. Press
this key again to resume terminal mode.
>
    let g:ConqueTerm_ToggleKey = '<F8>'
<
3.6 Enable or disable colors                                *ConqueTerm_Color*

Set to 1 to enable colors, 0 to disable. Syntax highlighting in Vim can be
slow if your terminal is color intensive. Disabling color can make the
terminal render significantly faster.
>
    let g:ConqueTerm_Color = 1
<
3.7 Choose your terminal type, Unix ONLY                     *ConqueTerm_TERM*

Use this option to tell Conque what type of terminal it should identify itself
as. Conque officially uses the more limited VT100 terminal type for
developement and testing, although it supports some more advanced features
such as colors and title strings.

You can change this setting to a more advanced type, namely 'xterm', but your
results may vary depending on which programs you're running.
>
    let g:ConqueTerm_TERM = 'vt100'
<
3.8 Choose Vim syntax type                                 *ConqueTerm_Syntax*

Set the buffer syntax. The default 'conque' has highlighting for MySQL, but 
not much else.
>
    let g:ConqueTerm_Syntax = 'conque'
<
3.9 Keep updating terminal buffer                   *ConqueTerm_ReadUnfocused*

If set to 1 then your Conque buffers will continue to update after you've
switched to another buffer.

Note: Conque buffers may continue to update, but they will not scroll down as
new lines are added beyond the bottom of the visible buffer area. This is a
limitation of the Vim scripting language for which I haven't found a 
workaround.
>
    let g:ConqueTerm_ReadUnfocused = 1
<
3.10 Regex for highlighting your prompt                *ConqueTerm_PromptRegex*

Use this regular expression for sytax highlighting your terminal prompt. Your
terminal will generally run faster if you use Vim highlighting instead of 
terminal colors for your prompt. You can also use it to do more advanced
syntax highlighting for the prompt line.
>
    let g:ConqueTerm_PromptRegex = '^\w\+@[0-9A-Za-z_.-]\+:[0-9A-Za-z_./\~,:-]\+\$'
<
3.11 Close buffer when program exits                   *ConqueTerm_CloseOnEnd*

If you want your terminal buffer to be closed and permanently deleted when the 
program running inside of it exits, set this option to 1. Otherwise the buffer
will become a simple text buffer after the program exits, and you can edit the
program output in insert mode.
>
    let g:ConqueTerm_CloseOnEnd = 0
<
3.12 Python version                                     *ConqueTerm_PyVersion*

Conque will work with either Python 2.x or 3.x, assuming the interfaces have
been installed. By default it will try to use Python 2 first, then will try 
Python 3. If you want Conque to use Python 3, set this variable to 3. 

Note: even if you set this to 3, if you don't have the python3 interface
Conque will fall back to using Python 2.
>
    let g:ConqueTerm_PyVersion = 2
<
3.13 Python executable, Windows ONLY                        *ConqueTerm_PyExe*

The Windows version of Conque needs to know the path to the python.exe
executable for the version of Python Conque is using. If you installed Python
in the default location, or added the Python directory to your system path,
Conque should be able to find python.exe without you changing this variable.

For example, you might set this to 'C:\Program Files\Python27\python.exe'
>
    let g:ConqueTerm_PyExe = ''
<
3.14 Function Keys                               *ConqueTerm_SendFunctionKeys*

By default, function keys (the F1-F12 row at the top of your keyboard) are not
passed to the terminal. Set this option to 1 to send these key events.

Note: Unless you configured |ConqueTerm_SendVisKey| and |ConqueTerm_ToggleKey|
to use different keys, <F8> and <F9> will not be sent to the terminal even if
you set this option to 1.
>
    let g:ConqueTerm_SendFunctionKeys = 0
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


5.3 Feedback                                            *conque-term-feedback*

Bugs, suggestions and patches are all welcome.

For more information visit http://conque.googlecode.com

Check out the latest from svn at http://conque.googlecode.com/svn/trunk/

 vim:tw=78:ts=8:ft=help:norl:
syntax/conque_term.vim	[[[1
113
" FILE:     syntax/conque_term.vim {{{
" AUTHOR:   Nico Raffo <nicoraffo@gmail.com>
" WEBSITE:  http://conque.googlecode.com
" MODIFIED: 2010-11-15
" VERSION:  2.0, for Vim 7.0
" LICENSE:
" Conque - Vim terminal/console emulator
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
autoload/conque_term/conque_screen.py	[[[1
209
# FILE:     autoload/conque_term/conque_screen.py {{{
# AUTHOR:   Nico Raffo <nicoraffo@gmail.com>
# WEBSITE:  http://conque.googlecode.com
# MODIFIED: 2010-11-15
# VERSION:  2.0, for Vim 7.0
# LICENSE:
# Conque - Vim terminal/console emulator
# Copyright (C) 2009-2010 Nico Raffo
#
# MIT License
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE. }}}

"""
ConqueScreen is an extention of the vim.current.buffer object

It restricts the working indices of the buffer object to the scroll region
which pty is expecting. It also uses 1-based indexes, to match escape
sequence commands.

  E.g.:
    s = ConqueScreen()
    ...
    s[5] = 'Set 5th line in terminal to this line'
    s.append('Add new line to terminal')
    s[5] = 'Since previous append() command scrolled the terminal down, this is a different line than first cb[5] call'
"""

import vim


class ConqueScreen(object):

    # CLASS PROPERTIES  {{{

    # the buffer
    buffer = None

    # screen and scrolling regions
    screen_top = 1

    # screen width
    screen_width = 80
    screen_height = 80

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

        return u(self.buffer[real_line], 'utf-8')
    # }}}

    def __setitem__(self, key, value): # {{{
        real_line = self.get_real_idx(key)

        if CONQUE_PYTHON_VERSION == 2:
            val = value.encode('utf-8')
        else:
            # XXX / Vim's python3 interface doesn't accept bytes object
            val = str(value)

        # if line is past end of screen, append
        if real_line == len(self.buffer):
            self.buffer.append(val)
        else:
            self.buffer[real_line] = val
    # }}}

    def __delitem__(self, key): # {{{
        del self.buffer[self.screen_top + key - 2]
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
        self.buffer.append(value, l)

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
        real_line = self.screen_top + line - 1
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

# vim:foldmethod=marker
autoload/conque_term/conque_win32_util.py	[[[1
473
# FILE:     autoload/conque_term/conque_win32_util.py {{{
# AUTHOR:   Nico Raffo <nicoraffo@gmail.com>
# WEBSITE:  http://conque.googlecode.com
# MODIFIED: 2010-11-15
# VERSION:  2.0, for Vim 7.0
# LICENSE:
# Conque - Vim terminal/console emulator
# Copyright (C) 2009-2010 Nico Raffo
#
# MIT License
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE. }}}

"""Python structures used for ctypes interaction"""

from ctypes import *

# Constants

# create process flag constants {{{

CREATE_BREAKAWAY_FROM_JOB = 0x01000000
CREATE_DEFAULT_ERROR_MODE = 0x04000000
CREATE_NEW_CONSOLE = 0x00000010
CREATE_NEW_PROCESS_GROUP = 0x00000200
CREATE_NO_WINDOW = 0x08000000
CREATE_PROTECTED_PROCESS = 0x00040000
CREATE_PRESERVE_CODE_AUTHZ_LEVEL = 0x02000000
CREATE_SEPARATE_WOW_VDM = 0x00000800
CREATE_SHARED_WOW_VDM = 0x00001000
CREATE_SUSPENDED = 0x00000004
CREATE_UNICODE_ENVIRONMENT = 0x00000400


DETACHED_PROCESS = 0x00000008
EXTENDED_STARTUPINFO_PRESENT = 0x00080000
INHERIT_PARENT_AFFINITY = 0x00010000

# }}}

# process priority constants {{{

ABOVE_NORMAL_PRIORITY_CLASS = 0x00008000
BELOW_NORMAL_PRIORITY_CLASS = 0x00004000
HIGH_PRIORITY_CLASS = 0x00000080
IDLE_PRIORITY_CLASS = 0x00000040
NORMAL_PRIORITY_CLASS = 0x00000020
REALTIME_PRIORITY_CLASS = 0x00000100

# }}}

# startup info constants {{{

STARTF_FORCEONFEEDBACK = 0x00000040
STARTF_FORCEOFFFEEDBACK = 0x00000080
STARTF_PREVENTPINNING = 0x00002000
STARTF_RUNFULLSCREEN = 0x00000020
STARTF_TITLEISAPPID = 0x00001000
STARTF_TITLEISLINKNAME = 0x00000800
STARTF_USECOUNTCHARS = 0x00000008
STARTF_USEFILLATTRIBUTE = 0x00000010
STARTF_USEHOTKEY = 0x00000200
STARTF_USEPOSITION = 0x00000004
STARTF_USESHOWWINDOW = 0x00000001
STARTF_USESIZE = 0x00000002
STARTF_USESTDHANDLES = 0x00000100

# }}}

# show window constants {{{

SW_FORCEMINIMIZE = 11
SW_HIDE = 0
SW_MAXIMIZE = 3
SW_MINIMIZE = 6
SW_RESTORE = 9
SW_SHOW = 5
SW_SHOWDEFAULT = 10
SW_SHOWMAXIMIZED = 3
SW_SHOWMINIMIZED = 2
SW_SHOWMINNOACTIVE = 7
SW_SHOWNA = 8
SW_SHOWNOACTIVATE = 4
SW_SHOWNORMAL = 1

# }}}

# input event types {{{

FOCUS_EVENT = 0x0010
KEY_EVENT = 0x0001
MENU_EVENT = 0x0008
MOUSE_EVENT = 0x0002
WINDOW_BUFFER_SIZE_EVENT = 0x0004

# }}}

# key event modifiers {{{

CAPSLOCK_ON = 0x0080
ENHANCED_KEY = 0x0100
LEFT_ALT_PRESSED = 0x0002
LEFT_CTRL_PRESSED = 0x0008
NUMLOCK_ON = 0x0020
RIGHT_ALT_PRESSED = 0x0001
RIGHT_CTRL_PRESSED = 0x0004
SCROLLLOCK_ON = 0x0040
SHIFT_PRESSED = 0x0010

# }}}

# process access {{{

PROCESS_CREATE_PROCESS = 0x0080
PROCESS_CREATE_THREAD = 0x0002
PROCESS_DUP_HANDLE = 0x0040
PROCESS_QUERY_INFORMATION = 0x0400
PROCESS_QUERY_LIMITED_INFORMATION = 0x1000
PROCESS_SET_INFORMATION = 0x0200
PROCESS_SET_QUOTA = 0x0100
PROCESS_SUSPEND_RESUME = 0x0800
PROCESS_TERMINATE = 0x0001
PROCESS_VM_OPERATION = 0x0008
PROCESS_VM_READ = 0x0010
PROCESS_VM_WRITE = 0x0020

# }}}

# input / output handles {{{

STD_INPUT_HANDLE = c_ulong(-10)
STD_OUTPUT_HANDLE = c_ulong(-11)
STD_ERROR_HANDLE = c_ulong(-12)

# }}}

CONQUE_WINDOWS_VK = { # {{{
    'VK_LBUTTON': 0x0001,
    'VK_RBUTTON': 0x0002,
    'VK_CANCEL': 0x0003,
    'VK_BACK': 0x0008,
    'VK_TAB': 0x0009,
    'VK_CLEAR': 0x000C,
    'VK_RETURN': 0x0D,
    'VK_SHIFT': 0x10,
    'VK_CONTROL': 0x11,
    'VK_MENU': 0x12,
    'VK_PAUSE': 0x0013,
    'VK_CAPITAL': 0x0014,
    'VK_ESCAPE': 0x001B,
    'VK_SPACE': 0x0020,
    'VK_PRIOR': 0x0021,
    'VK_NEXT': 0x0022,
    'VK_END': 0x0023,
    'VK_HOME': 0x0024,
    'VK_LEFT': 0x0025,
    'VK_UP': 0x0026,
    'VK_RIGHT': 0x0027,
    'VK_DOWN': 0x0028,
    'VK_SELECT': 0x0029,
    'VK_PRINT': 0x002A,
    'VK_EXECUTE': 0x002B,
    'VK_SNAPSHOT': 0x002C,
    'VK_INSERT': 0x002D,
    'VK_DELETE': 0x002E,
    'VK_HELP': 0x002F,
    'VK_0': 0x0030,
    'VK_1': 0x0031,
    'VK_2': 0x0032,
    'VK_3': 0x0033,
    'VK_4': 0x0034,
    'VK_5': 0x0035,
    'VK_6': 0x0036,
    'VK_7': 0x0037,
    'VK_8': 0x0038,
    'VK_9': 0x0039,
    'VK_A': 0x0041,
    'VK_B': 0x0042,
    'VK_C': 0x0043,
    'VK_D': 0x0044,
    'VK_E': 0x0045,
    'VK_F': 0x0046,
    'VK_G': 0x0047,
    'VK_H': 0x0048,
    'VK_I': 0x0049,
    'VK_J': 0x004A,
    'VK_K': 0x004B,
    'VK_L': 0x004C,
    'VK_M': 0x004D,
    'VK_N': 0x004E,
    'VK_O': 0x004F,
    'VK_P': 0x0050,
    'VK_Q': 0x0051,
    'VK_R': 0x0052,
    'VK_S': 0x0053,
    'VK_T': 0x0054,
    'VK_U': 0x0055,
    'VK_V': 0x0056,
    'VK_W': 0x0057,
    'VK_X': 0x0058,
    'VK_Y': 0x0059,
    'VK_Z': 0x005A,
    'VK_LWIN': 0x005B,
    'VK_RWIN': 0x005C,
    'VK_APPS': 0x005D,
    'VK_SLEEP': 0x005F,
    'VK_NUMPAD0': 0x0060,
    'VK_NUMPAD1': 0x0061,
    'VK_NUMPAD2': 0x0062,
    'VK_NUMPAD3': 0x0063,
    'VK_NUMPAD4': 0x0064,
    'VK_NUMPAD5': 0x0065,
    'VK_NUMPAD6': 0x0066,
    'VK_NUMPAD7': 0x0067,
    'VK_NUMPAD8': 0x0068,
    'VK_MULTIPLY': 0x006A,
    'VK_ADD': 0x006B,
    'VK_SEPARATOR': 0x006C,
    'VK_SUBTRACT': 0x006D,
    'VK_DECIMAL': 0x006E,
    'VK_DIVIDE': 0x006F,
    'VK_F1': 0x0070,
    'VK_F2': 0x0071,
    'VK_F3': 0x0072,
    'VK_F4': 0x0073,
    'VK_F5': 0x0074,
    'VK_F6': 0x0075,
    'VK_F7': 0x0076,
    'VK_F8': 0x0077,
    'VK_F9': 0x0078,
    'VK_F10': 0x0079,
    'VK_F11': 0x007A,
    'VK_F12': 0x007B,
    'VK_F13': 0x007C,
    'VK_F14': 0x007D,
    'VK_F15': 0x007E,
    'VK_F16': 0x007F,
    'VK_F17': 0x0080,
    'VK_F18': 0x0081,
    'VK_F19': 0x0082,
    'VK_F20': 0x0083,
    'VK_F21': 0x0084,
    'VK_F22': 0x0085,
    'VK_F23': 0x0086,
    'VK_F24': 0x0087,
    'VK_NUMLOCK': 0x0090,
    'VK_SCROLL': 0x0091,
    'VK_LSHIFT': 0x00A0,
    'VK_RSHIFT': 0x00A1,
    'VK_LCONTROL': 0x00A2,
    'VK_RCONTROL': 0x00A3,
    'VK_LMENU': 0x00A4,
    'VK_RMENU': 0x00A5
}

CONQUE_WINDOWS_VK_INV = dict([v, k] for k, v in CONQUE_WINDOWS_VK.items())

CONQUE_WINDOWS_VK_ENHANCED = {
    str(int(CONQUE_WINDOWS_VK['VK_UP'])): 1,
    str(int(CONQUE_WINDOWS_VK['VK_DOWN'])): 1,
    str(int(CONQUE_WINDOWS_VK['VK_LEFT'])): 1,
    str(int(CONQUE_WINDOWS_VK['VK_RIGHT'])): 1,
    str(int(CONQUE_WINDOWS_VK['VK_HOME'])): 1,
    str(int(CONQUE_WINDOWS_VK['VK_END'])): 1
}

# }}}

# structures used for CreateProcess

# Odd types {{{

LPBYTE = POINTER(c_ubyte)
LPTSTR = POINTER(c_char)

# }}}


class STARTUPINFO(Structure): # {{{
    _fields_ = [("cb",            c_ulong),
                ("lpReserved",    LPTSTR),
                ("lpDesktop",     LPTSTR),
                ("lpTitle",       LPTSTR),
                ("dwX",           c_ulong),
                ("dwY",           c_ulong),
                ("dwXSize",       c_ulong),
                ("dwYSize",       c_ulong),
                ("dwXCountChars", c_ulong),
                ("dwYCountChars", c_ulong),
                ("dwFillAttribute", c_ulong),
                ("dwFlags",       c_ulong),
                ("wShowWindow",   c_short),
                ("cbReserved2",   c_short),
                ("lpReserved2",   LPBYTE),
                ("hStdInput",     c_void_p),
                ("hStdOutput",    c_void_p),
                ("hStdError",     c_void_p),]

    def to_str(self):
        return ''

    # }}}

class PROCESS_INFORMATION(Structure): # {{{
    _fields_ = [("hProcess",    c_void_p),
                ("hThread",     c_void_p),
                ("dwProcessId", c_ulong),
                ("dwThreadId",  c_ulong),]

    def to_str(self):
        return ''

    # }}}

class MEMORY_BASIC_INFORMATION(Structure): # {{{
    _fields_ = [("BaseAddress",       c_void_p),
                ("AllocationBase",    c_void_p),
                ("AllocationProtect", c_ulong),
                ("RegionSize",        c_ulong),
                ("State",             c_ulong),
                ("Protect",           c_ulong),
                ("Type",              c_ulong),]

    def to_str(self):
        return ''

    # }}}

class SECURITY_ATTRIBUTES(Structure): # {{{
    _fields_ = [("Length", c_ulong),
                ("SecDescriptor", c_void_p),
                ("InheritHandle", c_bool)]

    def to_str(self):
        return ''

    # }}}

class COORD(Structure): # {{{
    _fields_ = [("X", c_short),
                ("Y", c_short)]

    def to_str(self):
        return ''

    # }}}

class SMALL_RECT(Structure): # {{{
    _fields_ = [("Left", c_short),
                ("Top", c_short),
                ("Right", c_short),
                ("Bottom", c_short)]

    def to_str(self):
        return ''

    # }}}

class CONSOLE_SCREEN_BUFFER_INFO(Structure): # {{{
    _fields_ = [("dwSize", COORD),
                ("dwCursorPosition", COORD),
                ("wAttributes", c_short),
                ("srWindow", SMALL_RECT),
                ("dwMaximumWindowSize", COORD)]

    def to_str(self):
        return ''

    # }}}

class CHAR_UNION(Union): # {{{
    _fields_ = [("UnicodeChar", c_wchar),
                ("AsciiChar", c_char)]

    def to_str(self):
        return ''

    # }}}

class CHAR_INFO(Structure): # {{{
    _fields_ = [("Char", CHAR_UNION),
                ("Attributes", c_short)]

    def to_str(self):
        return ''

    # }}}

class KEY_EVENT_RECORD(Structure): # {{{
    _fields_ = [("bKeyDown", c_byte),
                ("pad2", c_byte),
                ('pad1', c_short),
                ("wRepeatCount", c_short),
                ("wVirtualKeyCode", c_short),
                ("wVirtualScanCode", c_short),
                ("uChar", CHAR_UNION),
                ("dwControlKeyState", c_int)]

    def to_str(self):
        return ''

    # }}}

class MOUSE_EVENT_RECORD(Structure): # {{{
    _fields_ = [("dwMousePosition", COORD),
                ("dwButtonState", c_int),
                ("dwControlKeyState", c_int),
                ("dwEventFlags", c_int)]

    def to_str(self):
        return ''

    # }}}

class WINDOW_BUFFER_SIZE_RECORD(Structure): # {{{
    _fields_ = [("dwSize", COORD)]

    def to_str(self):
        return ''

    # }}}

class MENU_EVENT_RECORD(Structure): # {{{
    _fields_ = [("dwCommandId", c_uint)]

    def to_str(self):
        return ''

    # }}}

class FOCUS_EVENT_RECORD(Structure): # {{{
    _fields_ = [("bSetFocus", c_byte)]

    def to_str(self):
        return ''
    # }}}

class INPUT_UNION(Union): # {{{
    _fields_ = [("KeyEvent", KEY_EVENT_RECORD),
                ("MouseEvent", MOUSE_EVENT_RECORD),
                ("WindowBufferSizeEvent", WINDOW_BUFFER_SIZE_RECORD),
                ("MenuEvent", MENU_EVENT_RECORD),
                ("FocusEvent", FOCUS_EVENT_RECORD)]

    def to_str(self):
        return ''
    # }}}

class INPUT_RECORD(Structure): # {{{
    _fields_ = [("EventType", c_short),
                ("Event", INPUT_UNION)]

    def to_str(self):
        return ''
    # }}}

# vim:foldmethod=marker
autoload/conque_term/conque_sole_shared_memory.py	[[[1
202
# FILE:     autoload/conque_term/conque_sole_shared_memory.py {{{
# AUTHOR:   Nico Raffo <nicoraffo@gmail.com>
# WEBSITE:  http://conque.googlecode.com
# MODIFIED: 2010-11-15
# VERSION:  2.0, for Vim 7.0
# LICENSE:
# Conque - Vim terminal/console emulator
# Copyright (C) 2009-2010 Nico Raffo
#
# MIT License
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE. }}}

"""Wrapper class for shared memory between Windows python processes"""

import mmap
import sys

if sys.version_info[0] == 2:
    CONQUE_PYTHON_VERSION = 2
else:
    CONQUE_PYTHON_VERSION = 3

if CONQUE_PYTHON_VERSION == 2:
    import cPickle as pickle
else:
    import pickle


class ConqueSoleSharedMemory():

    # ****************************************************************************
    # class properties

    # {{{

    # is the data being stored not fixed length
    fixed_length = False

    # fill memory with this character when clearing and fixed_length is true
    fill_char = ' '

    # serialize and unserialize data automatically
    serialize = False

    # size of shared memory, in bytes / chars
    mem_size = None

    # size of shared memory, in bytes / chars
    mem_type = None

    # unique key, so multiple console instances are possible
    mem_key = None

    # mmap instance
    shm = None

    # character encoding, dammit
    encoding = 'ascii'

    # pickle terminator
    TERMINATOR = None

    # }}}

    # ****************************************************************************
    # constructor I guess

    def __init__(self, mem_size, mem_type, mem_key, fixed_length=False, fill_char=' ', serialize=False, encoding='ascii'): # {{{

        self.mem_size = mem_size
        self.mem_type = mem_type
        self.mem_key = mem_key
        self.fixed_length = fixed_length
        self.fill_char = fill_char
        self.serialize = serialize
        self.encoding = encoding
        self.TERMINATOR = str(chr(0)).encode(self.encoding)

    # }}}

    # ****************************************************************************
    # create memory block

    def create(self, access='write'): # {{{

        if access == 'write':
            mmap_access = mmap.ACCESS_WRITE
        else:
            mmap_access = mmap.ACCESS_READ

        name = "conque_%s_%s" % (self.mem_type, self.mem_key)

        self.shm = mmap.mmap(0, self.mem_size, name, mmap_access)

        if not self.shm:
            return False
        else:
            return True

        # }}}

    # ****************************************************************************
    # read data

    def read(self, chars=1, start=0): # {{{

        # invalid reads
        if self.fixed_length and (chars == 0 or start + chars > self.mem_size):
            return ''

        # go to start position
        self.shm.seek(start)

        if not self.fixed_length:
            chars = self.shm.find(self.TERMINATOR)

        if chars == 0:
            return ''

        shm_str = self.shm.read(chars)

        # return unpickled byte object
        if self.serialize:
            return pickle.loads(shm_str)

        # decode byes in python 3
        if CONQUE_PYTHON_VERSION == 3:
            return str(shm_str, self.encoding)

        # encoding
        if self.encoding != 'ascii':
            shm_str = unicode(shm_str, self.encoding)

        return shm_str

        # }}}

    # ****************************************************************************
    # write data

    def write(self, text, start=0): # {{{

        # simple scenario, let pickle create bytes
        if self.serialize:
            if CONQUE_PYTHON_VERSION == 3:
                tb = pickle.dumps(text, 0)
            else:
                tb = pickle.dumps(text, 0).encode(self.encoding)

        else:
            tb = text.encode(self.encoding, 'replace')

        self.shm.seek(start)

        # write to memory
        if self.fixed_length:
            self.shm.write(tb)
        else:
            self.shm.write(tb + self.TERMINATOR)

        # }}}

    # ****************************************************************************
    # clear

    def clear(self, start=0): # {{{

        self.shm.seek(start)

        if self.fixed_length:
            self.shm.write(str(self.fill_char * self.mem_size).encode(self.encoding))
        else:
            self.shm.write(self.TERMINATOR)

        # }}}

    # ****************************************************************************
    # close

    def close(self):

        self.shm.close()


# vim:foldmethod=marker
autoload/conque_term/conque.py	[[[1
975
# FILE:     autoload/conque_term/conque.py {{{
# AUTHOR:   Nico Raffo <nicoraffo@gmail.com>
# WEBSITE:  http://conque.googlecode.com
# MODIFIED: 2010-11-15
# VERSION:  2.0, for Vim 7.0
# LICENSE:
# Conque - Vim terminal/console emulator
# Copyright (C) 2009-2010 Nico Raffo
#
# MIT License
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE. }}}

"""
Vim terminal emulator.

The Conque does two things. First, it handles communication between Vim and
the terminal/console subprocess. For example, Vim uses the Conque.write()
method to send input, and Conque.read() to update the terminal buffer.

Second, the Conque class handles Unix terminal escape sequence parsing.
"""

import vim
import re
import math




class Conque:

    # CLASS PROPERTIES {{{

    # screen object
    screen = None

    # subprocess object
    proc = None

    # terminal dimensions and scrolling region
    columns = 80 # same as $COLUMNS
    lines = 24 # same as $LINES
    working_columns = 80 # can be changed by CSI ? 3 l/h
    working_lines = 24 # can be changed by CSI r

    # top/bottom of the scroll region
    top = 1 # relative to top of screen
    bottom = 24 # relative to top of screen

    # cursor position
    l = 1 # current cursor line
    c = 1 # current cursor column

    # autowrap mode
    autowrap = True

    # absolute coordinate mode
    absolute_coords = True

    # tabstop positions
    tabstops = []

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

    # current character set, ascii or graphics
    character_set = 'ascii'

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
        self.proc.open(command, {'TERM': options['TERM'], 'CONQUE': '1', 'LINES': str(self.lines), 'COLUMNS': str(self.columns)})

        # send window size signal, in case LINES/COLUMNS is ignored
        self.update_window_size(True)
        # }}}

    # write to pty
    def write(self, input, set_cursor=True, read=True): # {{{

        # check if window size has changed
        if read:
            self.update_window_size()

        # write and read
        self.proc.write(input)

        # read output immediately
        if read:
            self.read(1, set_cursor)

        # }}}

    # convert latin-1 input into utf-8
    # XXX - this is a hack, to be removed soon
    def write_latin1(self, input, set_cursor=True, read=True): # {{{ 

        if CONQUE_PYTHON_VERSION == 2:
            try:
                input_unicode = input.decode('latin-1', 'ignore')
                self.write(input_unicode.encode('utf-8', 'ignore'), set_cursor, read)
            except:
                return
        else:
            self.write(input, set_cursor, read)

        # }}}

    # read from pty, and update buffer
    def read(self, timeout=1, set_cursor=True, return_output=False, update_buffer=True): # {{{
        output = ''

        # this may not actually work
        try:

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

                    # check for charset match {{{
                    elif CONQUE_SEQ_REGEX_CHAR.match(s):

                        if s[-1] in CONQUE_ESCAPE_CHARSET:
                            getattr(self, 'charset_' + CONQUE_ESCAPE_CHARSET[s[-1]])()
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



        except:


            pass

        if return_output:
            if CONQUE_PYTHON_VERSION == 3:
                return output
            else:
                return output.encode(vim.eval('&encoding'), 'replace')
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
        try:
            self.set_cursor(self.l, self.c)
        except:


            pass
        self.cursor_set = True

    # }}}

    ###############################################################################################
    # Plain text # {{{

    def plain_text(self, input):

        # translate input into correct character set
        if self.character_set == 'graphics':
            old_input = input
            input = ''
            for i in range(0, len(old_input)):
                chrd = ord(old_input[i])


                try:
                    if chrd > 255:

                        input = input + old_input[i]
                    else:
                        input = input + unichr(CONQUE_GRAPHICS_SET[chrd])
                except:

                    pass


        current_line = self.screen[self.l]

        if len(current_line) < self.working_columns:
            current_line = current_line + ' ' * (self.c - len(current_line))

        # if line is wider than screen
        if self.c + len(input) - 1 > self.working_columns:
            # Table formatting hack
            if self.unwrap_tables and CONQUE_TABLE_OUTPUT.match(input):
                self.screen[self.l] = current_line[:self.c - 1] + input + current_line[self.c + len(input) - 1:]
                self.apply_color(self.c, self.c + len(input))
                self.c += len(input)
                return

            diff = self.c + len(input) - self.working_columns - 1
            # if autowrap is enabled
            if self.autowrap:
                self.screen[self.l] = current_line[:self.c - 1] + input[:-1 * diff]
                self.apply_color(self.c, self.working_columns)
                self.ctl_nl()
                self.ctl_cr()
                remaining = input[-1 * diff:]

                self.plain_text(remaining)
            else:
                self.screen[self.l] = current_line[:self.c - 1] + input[:-1 * diff - 1] + input[-1]
                self.apply_color(self.c, self.working_columns)
                self.c = self.working_columns

        # no autowrap
        else:
            self.screen[self.l] = current_line[:self.c - 1] + input + current_line[self.c + len(input) - 1:]
            self.apply_color(self.c, self.c + len(input))
            self.c += len(input)

    def apply_color(self, start, end, line=0):


        # stop here if coloration is disabled
        if not self.enable_colors:
            return

        # allow custom line nr to be passed
        if line:
            real_line = line
        else:
            real_line = self.screen.get_real_line(self.l)

        # check for previous overlapping coloration

        to_del = []
        if real_line in self.color_history:
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
        if not real_line in self.color_history:
            self.color_history[real_line] = []

        self.color_history[real_line].append({'name': syntax_name, 'start': start, 'end': end, 'highlight': highlight})

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

    def ctl_so(self):
        self.character_set = 'graphics'

    def ctl_si(self):
        self.character_set = 'ascii'

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
                if val in CONQUE_FONT:

                    # ignore starting normal colors
                    if CONQUE_FONT[val]['normal'] and len(self.color_changes) == 0:

                        continue
                    # clear color changes
                    elif CONQUE_FONT[val]['normal']:

                        self.color_changes = {}
                    # save these color attributes for next plain_text() call
                    else:

                        for attr in CONQUE_FONT[val]['attributes'].keys():
                            if attr in self.color_changes and (attr == 'cterm' or attr == 'gui'):
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
            self.screen[self.l] = self.screen[self.l][0:self.c - 1]

        # 1 means cursor left
        elif csi['val'] == 1:
            self.screen[self.l] = ' ' * (self.c) + self.screen[self.l][self.c:]

        # clear entire line
        elif csi['val'] == 2:
            self.screen[self.l] = ''

        # clear colors
        if csi['val'] == 2 or (csi['val'] == 0 and self.c == 1):
            real_line = self.screen.get_real_line(self.l)
            if real_line in self.color_history:
                for syn in self.color_history[real_line]:
                    vim.command('syn clear ' + syn['name'])



        # }}}

    def csi_cursor_right(self, csi): # {{{
        # we use 1 even if escape explicitly specifies 0
        if csi['val'] == 0:
            csi['val'] = 1




        if self.wrap_cursor and self.c + csi['val'] > self.working_columns:
            self.l += int(math.floor((self.c + csi['val']) / self.working_columns))
            self.c = (self.c + csi['val']) % self.working_columns
            return

        self.c = self.bound(self.c + csi['val'], 1, self.working_columns)
        # }}}

    def csi_cursor_left(self, csi): # {{{
        # we use 1 even if escape explicitly specifies 0
        if csi['val'] == 0:
            csi['val'] = 1

        if self.wrap_cursor and csi['val'] >= self.c:
            self.l += int(math.floor((self.c - csi['val']) / self.working_columns))
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
        self.screen[self.l] = self.screen[self.l][:self.c] + self.screen[self.l][self.c + csi['val']:]
        # }}}

    def csi_add_spaces(self, csi): # {{{
        self.screen[self.l] = self.screen[self.l][: self.c - 1] + ' ' * csi['val'] + self.screen[self.l][self.c:]
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
    # CHARSET functions {{{

    def charset_us(self):
        self.character_set = 'ascii'

    def charset_uk(self):
        self.character_set = 'ascii'

    def charset_graphics(self):
        self.character_set = 'graphics'

    # }}}

    ###############################################################################################
    # Random stuff {{{

    def set_cursor(self, line, col):
        self.screen.set_cursor(line, col)

    def change_title(self, key, val):


        if key == '0' or key == '2':

            vim.command('setlocal statusline=' + re.escape(val))
            try:
                vim.command('set titlestring=' + re.escape(val))
            except:
                pass

    def paste(self):
        input = vim.eval('@@')
        input = input.replace("\n", "\r")
        self.read(50)

    def paste_selection(self):
        input = vim.eval('@@')
        input = input.replace("\n", "\r")
        self.write(input)

    def update_window_size(self, force=False):
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

    def idle(self):
        pass

    def resume(self):
        pass

    def close(self):
        self.proc.close()

    def abort(self):
        self.proc.signal(1)

    # }}}

    ###############################################################################################
    # Utility {{{

    def parse_csi(self, s): # {{{
        attr = {'key': s[-1], 'flag': '', 'val': 1, 'vals': []}

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

            p1 = "%02x" % (math.floor(cc / 36) * (255 / 5))
            p2 = "%02x" % (math.floor((cc % 36) / 6) * (255 / 5))
            p3 = "%02x" % (math.floor(cc % 6) * (255 / 5))

            return p1 + p2 + p3
        else:
            grey_tone = "%02x" % math.floor((255 / 24) * (color_code - 232))
            return grey_tone + grey_tone + grey_tone
        # }}}

    # }}}

# vim:foldmethod=marker
autoload/conque_term/conque_sole_wrapper.py	[[[1
304
# FILE:     autoload/conque_term/conque_sole_wrapper.py {{{
# AUTHOR:   Nico Raffo <nicoraffo@gmail.com>
# WEBSITE:  http://conque.googlecode.com
# MODIFIED: 2010-11-15
# VERSION:  2.0, for Vim 7.0
# LICENSE:
# Conque - Vim terminal/console emulator
# Copyright (C) 2009-2010 Nico Raffo
#
# MIT License
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE. }}}

""" ConqueSoleSubprocessWrapper {{{

Subprocess wrapper to deal with Windows insanity. Launches console based python,
which in turn launches originally requested command. Communicates with cosole
python through shared memory objects.

}}} """

import ctypes
import time


class ConqueSoleWrapper():

    # class properties {{{

    shm_key = ''

    # process info
    handle = None
    pid = None

    # queue input in this bucket
    bucket = None

    # console size
    # NOTE: columns should never change after open() is called
    lines = 24
    columns = 80

    # shared memory objects
    shm_input = None
    shm_output = None
    shm_attributes = None
    shm_stats = None
    shm_command = None
    shm_rescroll = None
    shm_resize = None

    # console python process
    proc = None

    # }}}

    #########################################################################
    # unused

    def __init__(self): # {{{
        self.bucket = u('')

        # }}}

    #########################################################################
    # run communicator process which will in turn run cmd

    def open(self, cmd, options={}, python_exe='python.exe', communicator_py='conque_sole_communicator.py'): # {{{

        self.lines = options['LINES']
        self.columns = options['COLUMNS']

        # create a shm key
        self.shm_key = 'mk' + str(time.time())

        # python command
        cmd_line = '%s "%s" %s %d %d %s' % (python_exe, communicator_py, self.shm_key, int(self.columns), int(self.lines), cmd)


        # console window attributes
        flags = NORMAL_PRIORITY_CLASS | DETACHED_PROCESS
        si = STARTUPINFO()
        pi = PROCESS_INFORMATION()

        # start the stupid process already
        try:
            res = ctypes.windll.kernel32.CreateProcessW(None, u(cmd_line), None, None, 0, flags, None, u('.'), ctypes.byref(si), ctypes.byref(pi))
        except:

            raise

        # handle
        self.pid = pi.dwProcessId



        # init shared memory objects
        self.init_shared_memory(self.shm_key)

        # }}}

    #########################################################################
    # read output from shared memory

    def read(self, start_line, num_lines, timeout=0): # {{{

        # emulate timeout by sleeping timeout time
        if timeout > 0:
            read_timeout = float(timeout) / 1000

            time.sleep(read_timeout)

        output = []
        attributes = []

        # get output
        for i in range(start_line, start_line + num_lines + 1):
            output.append(self.shm_output.read(self.columns, i * self.columns))
            attributes.append(self.shm_attributes.read(self.columns, i * self.columns))

        return (output, attributes)

        # }}}

    #########################################################################
    # get current cursor/scroll position

    def get_stats(self): # {{{

        try:
            rescroll = self.shm_rescroll.read()
            if rescroll != '' and rescroll != None:



                self.shm_rescroll.clear()

                # close down old memory
                self.shm_output.close()
                self.shm_output = None

                self.shm_attributes.close()
                self.shm_attributes = None

                # reallocate memory

                self.shm_output = ConqueSoleSharedMemory(CONQUE_SOLE_BUFFER_LENGTH * self.columns * rescroll['data']['blocks'], 'output', rescroll['data']['mem_key'], True)
                self.shm_output.create('read')

                self.shm_attributes = ConqueSoleSharedMemory(CONQUE_SOLE_BUFFER_LENGTH * self.columns * rescroll['data']['blocks'], 'attributes', rescroll['data']['mem_key'], True, encoding='latin-1')
                self.shm_attributes.create('read')

            stats_str = self.shm_stats.read()
            if stats_str != '':
                self.stats = stats_str
            else:
                return False
        except:

            return False

        return self.stats

        # }}}

    #########################################################################
    # get process status

    def is_alive(self): # {{{
        if not self.shm_stats:
            return True

        stats_str = self.shm_stats.read()
        if stats_str:
            return (stats_str['is_alive'])
        else:
            return True
        # }}}

    #########################################################################
    # write input to shared memory

    def write(self, text): # {{{

        self.bucket += u(text, 'ascii', 'replace')



        istr = self.shm_input.read()

        if istr == '':

            self.shm_input.write(self.bucket[:500])
            self.bucket = self.bucket[500:]

        # }}}

    #########################################################################
    # write virtual key code to shared memory using proprietary escape seq

    def write_vk(self, vk_code): # {{{

        seq = "\x1b[" + str(vk_code) + "VK"
        self.write(seq)

        # }}}

    #########################################################################
    # idle

    def idle(self): # {{{


        self.shm_command.write({'cmd': 'idle', 'data': {}})

        # }}}

    #########################################################################
    # resume

    def resume(self): # {{{

        self.shm_command.write({'cmd': 'resume', 'data': {}})

        # }}}

    #########################################################################
    # shut it all down

    def close(self): # {{{

        self.shm_command.write({'cmd': 'close', 'data': {}})
        time.sleep(0.2)

        # }}}

    #########################################################################
    # resize console window

    def window_resize(self, lines, columns): # {{{

        self.lines = lines

        # we don't shrink buffer width
        if columns > self.columns:
            self.columns = columns

        self.shm_resize.write({'cmd': 'resize', 'data': {'width': columns, 'height': lines}})

        # }}}

    # ****************************************************************************
    # create shared memory objects

    def init_shared_memory(self, mem_key): # {{{

        self.shm_input = ConqueSoleSharedMemory(CONQUE_SOLE_INPUT_SIZE, 'input', mem_key)
        self.shm_input.create('write')
        self.shm_input.clear()

        self.shm_output = ConqueSoleSharedMemory(CONQUE_SOLE_BUFFER_LENGTH * self.columns, 'output', mem_key, True)
        self.shm_output.create('write')

        self.shm_attributes = ConqueSoleSharedMemory(CONQUE_SOLE_BUFFER_LENGTH * self.columns, 'attributes', mem_key, True, encoding='latin-1')
        self.shm_attributes.create('write')

        self.shm_stats = ConqueSoleSharedMemory(CONQUE_SOLE_STATS_SIZE, 'stats', mem_key, serialize=True)
        self.shm_stats.create('write')
        self.shm_stats.clear()

        self.shm_command = ConqueSoleSharedMemory(CONQUE_SOLE_COMMANDS_SIZE, 'command', mem_key, serialize=True)
        self.shm_command.create('write')
        self.shm_command.clear()

        self.shm_resize = ConqueSoleSharedMemory(CONQUE_SOLE_RESIZE_SIZE, 'resize', mem_key, serialize=True)
        self.shm_resize.create('write')
        self.shm_resize.clear()

        self.shm_rescroll = ConqueSoleSharedMemory(CONQUE_SOLE_RESCROLL_SIZE, 'rescroll', mem_key, serialize=True)
        self.shm_rescroll.create('write')
        self.shm_rescroll.clear()

        return True

        # }}}

# vim:foldmethod=marker
autoload/conque_term/conque_sole_communicator.py	[[[1
183
# FILE:     autoload/conque_term/conque_sole_communicator.py {{{
# AUTHOR:   Nico Raffo <nicoraffo@gmail.com>
# WEBSITE:  http://conque.googlecode.com
# MODIFIED: 2010-11-15
# VERSION:  2.0, for Vim 7.0
# LICENSE:
# Conque - Vim terminal/console emulator
# Copyright (C) 2009-2010 Nico Raffo
#
# MIT License
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE. }}}

"""
ConqueSoleCommunicator

Script to transfer communications between python being run in Vim and a
subprocess run inside a Windows console. This is required since interactive
programs in Windows appear to require a console, and python run in Vim is
not attached to any console. So a console version of python must be initiated
for the subprocess. Communication is then done with the use of shared memory
objects. Good times!
"""

import time
import sys

from conque_globals import *
from conque_win32_util import *
from conque_sole_subprocess import *
from conque_sole_shared_memory import *

##############################################################
# only run if this file was run directly

if __name__ == '__main__':

    # attempt to catch ALL exceptions to fend of zombies
    try:

        # startup and config {{{

        # simple arg validation

        if len(sys.argv) < 5:

            exit()

        # shared memory size
        CONQUE_SOLE_COMMANDS_SIZE = 255

        # maximum time this thing reads. 0 means no limit. Only for testing.
        max_loops = 0

        # read interval, in seconds
        sleep_time = 0.01

        # idle read interval, in seconds
        idle_sleep_time = 0.10

        # are we idled?
        is_idle = False

        # mem key
        mem_key = sys.argv[1]

        # console width
        console_width = int(sys.argv[2])

        # console height
        console_height = int(sys.argv[3])

        # the actual subprocess to run
        cmd_line = " ".join(sys.argv[4:])


        # width and height
        options = {'LINES': console_height, 'COLUMNS': console_width}



        # set initial idle status
        shm_command = ConqueSoleSharedMemory(CONQUE_SOLE_COMMANDS_SIZE, 'command', mem_key, serialize=True)
        shm_command.create('write')

        cmd = shm_command.read()
        if cmd:

            if cmd['cmd'] == 'idle':
                is_idle = True
                shm_command.clear()

        # }}}

        ##############################################################
        # Create the subprocess

        # {{{
        proc = ConqueSoleSubprocess()
        res = proc.open(cmd_line, mem_key, options)

        if not res:

            exit()

        # }}}

        ##############################################################
        # main loop!

        loops = 0

        while True:

            # check for idle/resume
            if is_idle or loops % 25 == 0:

                # check process health
                if not proc.is_alive():

                    proc.close()
                    exit()

                # check for change in buffer focus
                cmd = shm_command.read()
                if cmd:

                    if cmd['cmd'] == 'idle':
                        is_idle = True
                        shm_command.clear()

                    elif cmd['cmd'] == 'resume':
                        is_idle = False
                        shm_command.clear()


            # sleep between loops if moderation is requested
            if sleep_time > 0:
                if is_idle:
                    time.sleep(idle_sleep_time)
                else:
                    time.sleep(sleep_time)

            # write, read, etc
            proc.write()
            proc.read()

            # increment loops, and exit if max has been reached
            loops += 1
            if max_loops and loops >= max_loops:

                break

        ##############################################################
        # all done!



        proc.close()

    # if an exception was thrown, croak
    except:

        proc.close()


# vim:foldmethod=marker
autoload/conque_term/conque_sole.py	[[[1
459
# FILE:     autoload/conque_term/conque_sole.py {{{
# AUTHOR:   Nico Raffo <nicoraffo@gmail.com>
# WEBSITE:  http://conque.googlecode.com
# MODIFIED: 2010-11-15
# VERSION:  2.0, for Vim 7.0
# LICENSE:
# Conque - Vim terminal/console emulator
# Copyright (C) 2009-2010 Nico Raffo
#
# MIT License
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE. }}}

import vim


class ConqueSole(Conque):

    window_top = None
    window_bottom = None

    color_cache = {}
    color_mode = None
    color_conceals = {}

    buffer = None

    # counters for periodic rendering
    buffer_redraw_ct = 0
    screen_redraw_ct = 0

    # *********************************************************************************************
    # start program and initialize this instance

    def open(self, command, options={}, python_exe='', communicator_py=''): # {{{

        # init size
        self.columns = vim.current.window.width
        self.lines = vim.current.window.height
        self.window_top = 0
        self.window_bottom = vim.current.window.height - 1

        # init color
        self.enable_colors = options['color']

        # open command
        self.proc = ConqueSoleWrapper()
        self.proc.open(command, {'TERM': options['TERM'], 'CONQUE': '1', 'LINES': self.lines, 'COLUMNS': self.columns}, python_exe, communicator_py)

        self.buffer = vim.current.buffer

        # }}}


    # *********************************************************************************************
    # read and update screen

    def read(self, timeout=1, set_cursor=True, return_output=False, update_buffer=True): # {{{

        try:
            stats = self.proc.get_stats()

            if not stats:
                return

            self.buffer_redraw_ct += 1
            self.screen_redraw_ct += 1

            update_top = 0
            update_bottom = 0
            lines = []

            # full buffer redraw, our favorite!
            if self.buffer_redraw_ct == CONQUE_SOLE_BUFFER_REDRAW:
                self.buffer_redraw_ct = 0
                update_top = 0
                update_bottom = stats['top_offset'] + self.lines
                (lines, attributes) = self.proc.read(update_top, update_bottom)
                if return_output:
                    output = self.get_new_output(lines, update_top, stats)
                if update_buffer:
                    for i in range(update_top, update_bottom + 1):
                        self.plain_text(i, lines[i], attributes[i], stats)

            # full screen redraw
            elif stats['cursor_y'] + 1 != self.l or stats['top_offset'] != self.window_top or self.screen_redraw_ct == CONQUE_SOLE_SCREEN_REDRAW:
                self.screen_redraw_ct = 0
                update_top = self.window_top
                update_bottom = stats['top_offset'] + self.lines + 1
                (lines, attributes) = self.proc.read(update_top, update_bottom - update_top + 1)
                if return_output:
                    output = self.get_new_output(lines, update_top, stats)
                if update_buffer:
                    for i in range(update_top, update_bottom + 1):
                        self.plain_text(i, lines[i - update_top], attributes[i - update_top], stats)


            # single line redraw
            else:
                update_top = stats['cursor_y']
                update_bottom = stats['cursor_y']
                (lines, attributes) = self.proc.read(update_top, 1)
                if return_output:
                    output = self.get_new_output(lines, update_top, stats)
                if update_buffer:
                    if lines[0].rstrip() != self.buffer[update_top].rstrip():
                        self.plain_text(update_top, lines[0], attributes[0], stats)


            # reset current position
            self.window_top = stats['top_offset']
            self.l = stats['cursor_y'] + 1
            self.c = stats['cursor_x'] + 1

            # reposition cursor if this seems plausible
            if set_cursor:
                self.set_cursor(self.l, self.c)

            if return_output:
                return output

        except:

            pass
        # }}}

    #########################################################################
    # Calculate the "new" output from this read. Fake but useful

    def get_new_output(self, lines, update_top, stats): # {{{

        if not (stats['cursor_y'] + 1 > self.l or (stats['cursor_y'] + 1 == self.l and stats['cursor_x'] + 1 > self.c)):
            return ""






        try:
            num_to_return = stats['cursor_y'] - self.l + 2

            lines = lines[self.l - update_top - 1:]


            new_output = []

            # first line
            new_output.append(lines[0][self.c - 1:].rstrip())

            # the rest
            for i in range(1, num_to_return):
                new_output.append(lines[i].rstrip())

        except:

            pass



        return "\n".join(new_output)
        # }}}

    #########################################################################
    # update the buffer

    def plain_text(self, line_nr, text, attributes, stats): # {{{





        self.l = line_nr + 1

        # remove trailing whitespace
        text = text.rstrip()

        # if we're using concealed text for color, then s- is weird
        if self.color_mode == 'conceal':

            text = self.add_conceal_color(text, attributes, stats, line_nr)


        # update vim buffer
        if len(self.buffer) <= line_nr:
            self.buffer.append(text)
        else:
            self.buffer[line_nr] = text

        if not self.color_mode == 'conceal':
            self.do_color(attributes=attributes, stats=stats)

        # }}}

    #########################################################################
    # add conceal color

    def add_conceal_color(self, text, attributes, stats, line_nr): # {{{

        # stop here if coloration is disabled
        if not self.enable_colors:
            return text

        # if no colors for this line, clear everything out
        if len(attributes) == 0 or attributes == u(chr(stats['default_attribute'])) * len(attributes):
            return text

        new_text = ''

        # if text attribute is different, call add_color()
        attr = None
        start = 0
        self.color_conceals[line_nr] = []
        ends = []
        for i in range(0, len(attributes)):
            c = ord(attributes[i])

            if c != attr:
                if attr and attr != stats['default_attribute']:

                    color = self.translate_color(attr)

                    new_text += chr(27) + 'sf' + color['fg_code'] + ';'
                    ends.append(chr(27) + 'ef' + color['fg_code'] + ';')
                    self.color_conceals[line_nr].append(start)

                    if c > 15:
                        new_text += chr(27) + 'sf' + color['bg_code'] + ';'
                        ends.append(chr(27) + 'ef' + color['bg_code'] + ';')
                        self.color_conceals[line_nr].append(start)

                new_text += text[start:i]

                # close color regions
                ends.reverse()
                for j in range(0, len(ends)):
                    new_text += ends[j]
                    self.color_conceals[line_nr].append(i)
                ends = []

                start = i
                attr = c


        if attr and attr != stats['default_attribute']:

            color = self.translate_color(attr)

            new_text += chr(27) + 'sf' + color['fg_code'] + ';'
            ends.append(chr(27) + 'ef' + color['fg_code'] + ';')

            if c > 15:
                new_text += chr(27) + 'sf' + color['bg_code'] + ';'
                ends.append(chr(27) + 'ef' + color['bg_code'] + ';')

        new_text += text[start:]

        # close color regions
        ends.reverse()
        for i in range(0, len(ends)):
            new_text += ends[i]

        return new_text

        # }}}

    #########################################################################

    def do_color(self, start=0, end=0, attributes='', stats=None): # {{{

        # stop here if coloration is disabled
        if not self.enable_colors:
            return

        # if no colors for this line, clear everything out
        if len(attributes) == 0 or attributes == u(chr(stats['default_attribute'])) * len(attributes):
            self.color_changes = {}
            self.apply_color(1, len(attributes), self.l)
            return

        # if text attribute is different, call add_color()
        attr = None
        start = 0
        for i in range(0, len(attributes)):
            c = ord(attributes[i])

            if c != attr:
                if attr and attr != stats['default_attribute']:
                    self.color_changes = self.translate_color(attr)
                    self.apply_color(start + 1, i + 1, self.l)
                start = i
                attr = c

        if attr and attr != stats['default_attribute']:
            self.color_changes = self.translate_color(attr)
            self.apply_color(start + 1, len(attributes), self.l)


        # }}}

    #########################################################################

    def translate_color(self, attr): # {{{

        # check for cached color
        if attr in self.color_cache:
            return self.color_cache[attr]






        # convert attribute integer to bit string
        bit_str = bin(attr)
        bit_str = bit_str.replace('0b', '')

        # slice foreground and background portions of bit string
        fg = bit_str[-4:].rjust(4, '0')
        bg = bit_str[-8:-4].rjust(4, '0')

        # ok, first create foreground #rbg
        red = int(fg[1]) * 204 + int(fg[0]) * int(fg[1]) * 51
        green = int(fg[2]) * 204 + int(fg[0]) * int(fg[2]) * 51
        blue = int(fg[3]) * 204 + int(fg[0]) * int(fg[3]) * 51
        fg_str = "#%02x%02x%02x" % (red, green, blue)
        fg_code = "%02x%02x%02x" % (red, green, blue)
        fg_code = fg_code[0] + fg_code[2] + fg_code[4]

        # ok, first create foreground #rbg
        red = int(bg[1]) * 204 + int(bg[0]) * int(bg[1]) * 51
        green = int(bg[2]) * 204 + int(bg[0]) * int(bg[2]) * 51
        blue = int(bg[3]) * 204 + int(bg[0]) * int(bg[3]) * 51
        bg_str = "#%02x%02x%02x" % (red, green, blue)
        bg_code = "%02x%02x%02x" % (red, green, blue)
        bg_code = bg_code[0] + bg_code[2] + bg_code[4]

        # build value for color_changes

        color = {'guifg': fg_str, 'guibg': bg_str}

        if self.color_mode == 'conceal':
            color['fg_code'] = fg_code
            color['bg_code'] = bg_code

        self.color_cache[attr] = color

        return color

        # }}}

    #########################################################################
    # write virtual key code to shared memory using proprietary escape seq

    def write_vk(self, vk_code): # {{{

        self.proc.write_vk(vk_code)

        # }}}

    # *********************************************************************************************
    # resize if needed

    def update_window_size(self): # {{{

        if vim.current.window.width != self.columns or vim.current.window.height != self.lines:

            # reset all window size attributes to default
            self.columns = vim.current.window.width
            self.lines = vim.current.window.height
            self.working_columns = vim.current.window.width
            self.working_lines = vim.current.window.height
            self.bottom = vim.current.window.height

            self.proc.window_resize(vim.current.window.height, vim.current.window.width)

        # }}}

    # *********************************************************************************************
    # resize if needed

    def set_cursor(self, line, column): # {{{

        # shift cursor position to handle concealed text
        if self.enable_colors and self.color_mode == 'conceal':
            if line - 1 in self.color_conceals:
                for c in self.color_conceals[line - 1]:
                    if c < column:
                        column += 7
                    else:
                        break

        # figure out line
        real_line = line
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


    # *********************************************************************************************
    # go into idle mode

    def idle(self): # {{{

        self.proc.idle()

        # }}}

    # *********************************************************************************************
    # resume from idle mode

    def resume(self): # {{{

        self.proc.resume()

        # }}}

    # *********************************************************************************************
    # end subprocess

    def close(self):
        self.proc.close()

    # *********************************************************************************************
    # end subprocess forcefully

    def abort(self):
        self.proc.close()


# vim:foldmethod=marker
autoload/conque_term/conque_globals.py	[[[1
287
# FILE:     autoload/conque_term/conque_globals.py {{{
# AUTHOR:   Nico Raffo <nicoraffo@gmail.com>
# WEBSITE:  http://conque.googlecode.com
# MODIFIED: 2010-11-15
# VERSION:  2.0, for Vim 7.0
# LICENSE:
# Conque - Vim terminal/console emulator
# Copyright (C) 2009-2010 Nico Raffo
#
# MIT License
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE. }}}

"""Common global constants and functions for Conque."""

import sys
import os
import re

















# shared memory size
CONQUE_SOLE_BUFFER_LENGTH = 1000
CONQUE_SOLE_INPUT_SIZE = 1000
CONQUE_SOLE_STATS_SIZE = 1000
CONQUE_SOLE_COMMANDS_SIZE = 255
CONQUE_SOLE_RESCROLL_SIZE = 255
CONQUE_SOLE_RESIZE_SIZE = 255

# interval of screen redraw
# larger number means less frequent
CONQUE_SOLE_SCREEN_REDRAW = 100

# interval of full buffer redraw
# larger number means less frequent
CONQUE_SOLE_BUFFER_REDRAW = 500

# interval of full output bucket replacement
# larger number means less frequent, 1 = every time
CONQUE_SOLE_MEM_REDRAW = 1000

# PYTHON VERSION
CONQUE_PYTHON_VERSION = sys.version_info[0]


def u(str_val, str_encoding='latin-1', errors='strict'):
    """foolhardy attempt to make unicode string syntax compatible with both python 2 and 3"""

    if not str_val:
        str_val = ''

    if CONQUE_PYTHON_VERSION == 3:
        return str_val

    else:
        return unicode(str_val, str_encoding, errors)

# Escape sequence settings  {{{

CONQUE_CTL = {
     1: 'soh', # start of heading
     2: 'stx', # start of text
     7: 'bel', # bell
     8: 'bs',  # backspace
     9: 'tab', # tab
    10: 'nl',  # new line
    13: 'cr',  # carriage return
    14: 'so',  # shift out
    15: 'si'   # shift in
}
#    11 : 'vt',  # vertical tab
#    12 : 'ff',  # form feed

# Escape sequences
CONQUE_ESCAPE = {
    'm': 'font',
    'J': 'clear_screen',
    'K': 'clear_line',
    '@': 'add_spaces',
    'A': 'cursor_up',
    'B': 'cursor_down',
    'C': 'cursor_right',
    'D': 'cursor_left',
    'G': 'cursor_to_column',
    'H': 'cursor',
    'P': 'delete_chars',
    'f': 'cursor',
    'g': 'tab_clear',
    'r': 'set_coords',
    'h': 'set',
    'l': 'reset'
}
#    'L': 'insert_lines',
#    'M': 'delete_lines',
#    'd': 'cusor_vpos',

# Alternate escape sequences, no [
CONQUE_ESCAPE_PLAIN = {
    'D': 'scroll_up',
    'E': 'next_line',
    'H': 'set_tab',
    'M': 'scroll_down'
}
#    'N': 'single_shift_2',
#    'O': 'single_shift_3',
#    '=': 'alternate_keypad',
#    '>': 'numeric_keypad',
#    '7': 'save_cursor',
#    '8': 'restore_cursor',

# Character set escape sequences, with "("
CONQUE_ESCAPE_CHARSET = {
    'A': 'uk',
    'B': 'us',
    '0': 'graphics'
}

# Uber alternate escape sequences, with # or ?
CONQUE_ESCAPE_QUESTION = {
    '1h': 'new_line_mode',
    '3h': '132_cols',
    '4h': 'smooth_scrolling',
    '5h': 'reverse_video',
    '6h': 'relative_origin',
    '7h': 'set_auto_wrap',
    '8h': 'set_auto_repeat',
    '9h': 'set_interlacing_mode',
    '1l': 'set_cursor_key',
    '2l': 'set_vt52',
    '3l': '80_cols',
    '4l': 'set_jump_scrolling',
    '5l': 'normal_video',
    '6l': 'absolute_origin',
    '7l': 'reset_auto_wrap',
    '8l': 'reset_auto_repeat',
    '9l': 'reset_interlacing_mode'
}

CONQUE_ESCAPE_HASH = {
    '8': 'screen_alignment_test'
}
#    '3': 'double_height_top',
#    '4': 'double_height_bottom',
#    '5': 'single_height_single_width',
#    '6': 'single_height_double_width',

CONQUE_GRAPHICS_SET = [
    0x0000, 0x0001, 0x0002, 0x0003, 0x0004, 0x0005, 0x0006, 0x0007,
    0x0008, 0x0009, 0x000A, 0x000B, 0x000C, 0x000D, 0x000E, 0x000F,
    0x0010, 0x0011, 0x0012, 0x0013, 0x0014, 0x0015, 0x0016, 0x0017,
    0x0018, 0x0019, 0x001A, 0x001B, 0x001C, 0x001D, 0x001E, 0x001F,
    0x0020, 0x0021, 0x0022, 0x0023, 0x0024, 0x0025, 0x0026, 0x0027,
    0x0028, 0x0029, 0x002A, 0x2192, 0x2190, 0x2191, 0x2193, 0x002F,
    0x2588, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037,
    0x0038, 0x0039, 0x003A, 0x003B, 0x003C, 0x003D, 0x003E, 0x003F,
    0x0040, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047,
    0x0048, 0x0049, 0x004A, 0x004B, 0x004C, 0x004D, 0x004E, 0x004F,
    0x0050, 0x0051, 0x0052, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057,
    0x0058, 0x0059, 0x005A, 0x005B, 0x005C, 0x005D, 0x005E, 0x00A0,
    0x25C6, 0x2592, 0x2409, 0x240C, 0x240D, 0x240A, 0x00B0, 0x00B1,
    0x2591, 0x240B, 0x2518, 0x2510, 0x250C, 0x2514, 0x253C, 0xF800,
    0xF801, 0x2500, 0xF803, 0xF804, 0x251C, 0x2524, 0x2534, 0x252C,
    0x2502, 0x2264, 0x2265, 0x03C0, 0x2260, 0x00A3, 0x00B7, 0x007F,
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087,
    0x0088, 0x0089, 0x008A, 0x008B, 0x008C, 0x008D, 0x008E, 0x008F,
    0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097,
    0x0098, 0x0099, 0x009A, 0x009B, 0x009C, 0x009D, 0x009E, 0x009F,
    0x00A0, 0x00A1, 0x00A2, 0x00A3, 0x00A4, 0x00A5, 0x00A6, 0x00A7,
    0x00A8, 0x00A9, 0x00AA, 0x00AB, 0x00AC, 0x00AD, 0x00AE, 0x00AF,
    0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x00B4, 0x00B5, 0x00B6, 0x00B7,
    0x00B8, 0x00B9, 0x00BA, 0x00BB, 0x00BC, 0x00BD, 0x00BE, 0x00BF,
    0x00C0, 0x00C1, 0x00C2, 0x00C3, 0x00C4, 0x00C5, 0x00C6, 0x00C7,
    0x00C8, 0x00C9, 0x00CA, 0x00CB, 0x00CC, 0x00CD, 0x00CE, 0x00CF,
    0x00D0, 0x00D1, 0x00D2, 0x00D3, 0x00D4, 0x00D5, 0x00D6, 0x00D7,
    0x00D8, 0x00D9, 0x00DA, 0x00DB, 0x00DC, 0x00DD, 0x00DE, 0x00DF,
    0x00E0, 0x00E1, 0x00E2, 0x00E3, 0x00E4, 0x00E5, 0x00E6, 0x00E7,
    0x00E8, 0x00E9, 0x00EA, 0x00EB, 0x00EC, 0x00ED, 0x00EE, 0x00EF,
    0x00F0, 0x00F1, 0x00F2, 0x00F3, 0x00F4, 0x00F5, 0x00F6, 0x00F7,
    0x00F8, 0x00F9, 0x00FA, 0x00FB, 0x00FC, 0x00FD, 0x00FE, 0x00FF
]

# Font codes {{{
CONQUE_FONT = {
    0: {'description': 'Normal (default)', 'attributes': {'cterm': 'NONE', 'ctermfg': 'NONE', 'ctermbg': 'NONE', 'gui': 'NONE', 'guifg': 'NONE', 'guibg': 'NONE'}, 'normal': True},
    1: {'description': 'Bold', 'attributes': {'cterm': 'BOLD', 'gui': 'BOLD'}, 'normal': False},
    4: {'description': 'Underlined', 'attributes': {'cterm': 'UNDERLINE', 'gui': 'UNDERLINE'}, 'normal': False},
    5: {'description': 'Blink (appears as Bold)', 'attributes': {'cterm': 'BOLD', 'gui': 'BOLD'}, 'normal': False},
    7: {'description': 'Inverse', 'attributes': {'cterm': 'REVERSE', 'gui': 'REVERSE'}, 'normal': False},
    8: {'description': 'Invisible (hidden)', 'attributes': {'ctermfg': '0', 'ctermbg': '0', 'guifg': '#000000', 'guibg': '#000000'}, 'normal': False},
    22: {'description': 'Normal (neither bold nor faint)', 'attributes': {'cterm': 'NONE', 'gui': 'NONE'}, 'normal': True},
    24: {'description': 'Not underlined', 'attributes': {'cterm': 'NONE', 'gui': 'NONE'}, 'normal': True},
    25: {'description': 'Steady (not blinking)', 'attributes': {'cterm': 'NONE', 'gui': 'NONE'}, 'normal': True},
    27: {'description': 'Positive (not inverse)', 'attributes': {'cterm': 'NONE', 'gui': 'NONE'}, 'normal': True},
    28: {'description': 'Visible (not hidden)', 'attributes': {'ctermfg': 'NONE', 'ctermbg': 'NONE', 'guifg': 'NONE', 'guibg': 'NONE'}, 'normal': True},
    30: {'description': 'Set foreground color to Black', 'attributes': {'ctermfg': '16', 'guifg': '#000000'}, 'normal': False},
    31: {'description': 'Set foreground color to Red', 'attributes': {'ctermfg': '1', 'guifg': '#ff0000'}, 'normal': False},
    32: {'description': 'Set foreground color to Green', 'attributes': {'ctermfg': '2', 'guifg': '#00ff00'}, 'normal': False},
    33: {'description': 'Set foreground color to Yellow', 'attributes': {'ctermfg': '3', 'guifg': '#ffff00'}, 'normal': False},
    34: {'description': 'Set foreground color to Blue', 'attributes': {'ctermfg': '4', 'guifg': '#0000ff'}, 'normal': False},
    35: {'description': 'Set foreground color to Magenta', 'attributes': {'ctermfg': '5', 'guifg': '#990099'}, 'normal': False},
    36: {'description': 'Set foreground color to Cyan', 'attributes': {'ctermfg': '6', 'guifg': '#009999'}, 'normal': False},
    37: {'description': 'Set foreground color to White', 'attributes': {'ctermfg': '7', 'guifg': '#ffffff'}, 'normal': False},
    39: {'description': 'Set foreground color to default (original)', 'attributes': {'ctermfg': 'NONE', 'guifg': 'NONE'}, 'normal': True},
    40: {'description': 'Set background color to Black', 'attributes': {'ctermbg': '16', 'guibg': '#000000'}, 'normal': False},
    41: {'description': 'Set background color to Red', 'attributes': {'ctermbg': '1', 'guibg': '#ff0000'}, 'normal': False},
    42: {'description': 'Set background color to Green', 'attributes': {'ctermbg': '2', 'guibg': '#00ff00'}, 'normal': False},
    43: {'description': 'Set background color to Yellow', 'attributes': {'ctermbg': '3', 'guibg': '#ffff00'}, 'normal': False},
    44: {'description': 'Set background color to Blue', 'attributes': {'ctermbg': '4', 'guibg': '#0000ff'}, 'normal': False},
    45: {'description': 'Set background color to Magenta', 'attributes': {'ctermbg': '5', 'guibg': '#990099'}, 'normal': False},
    46: {'description': 'Set background color to Cyan', 'attributes': {'ctermbg': '6', 'guibg': '#009999'}, 'normal': False},
    47: {'description': 'Set background color to White', 'attributes': {'ctermbg': '7', 'guibg': '#ffffff'}, 'normal': False},
    49: {'description': 'Set background color to default (original).', 'attributes': {'ctermbg': 'NONE', 'guibg': 'NONE'}, 'normal': True},
    90: {'description': 'Set foreground color to Black', 'attributes': {'ctermfg': '8', 'guifg': '#000000'}, 'normal': False},
    91: {'description': 'Set foreground color to Red', 'attributes': {'ctermfg': '9', 'guifg': '#ff0000'}, 'normal': False},
    92: {'description': 'Set foreground color to Green', 'attributes': {'ctermfg': '10', 'guifg': '#00ff00'}, 'normal': False},
    93: {'description': 'Set foreground color to Yellow', 'attributes': {'ctermfg': '11', 'guifg': '#ffff00'}, 'normal': False},
    94: {'description': 'Set foreground color to Blue', 'attributes': {'ctermfg': '12', 'guifg': '#0000ff'}, 'normal': False},
    95: {'description': 'Set foreground color to Magenta', 'attributes': {'ctermfg': '13', 'guifg': '#990099'}, 'normal': False},
    96: {'description': 'Set foreground color to Cyan', 'attributes': {'ctermfg': '14', 'guifg': '#009999'}, 'normal': False},
    97: {'description': 'Set foreground color to White', 'attributes': {'ctermfg': '15', 'guifg': '#ffffff'}, 'normal': False},
    100: {'description': 'Set background color to Black', 'attributes': {'ctermbg': '8', 'guibg': '#000000'}, 'normal': False},
    101: {'description': 'Set background color to Red', 'attributes': {'ctermbg': '9', 'guibg': '#ff0000'}, 'normal': False},
    102: {'description': 'Set background color to Green', 'attributes': {'ctermbg': '10', 'guibg': '#00ff00'}, 'normal': False},
    103: {'description': 'Set background color to Yellow', 'attributes': {'ctermbg': '11', 'guibg': '#ffff00'}, 'normal': False},
    104: {'description': 'Set background color to Blue', 'attributes': {'ctermbg': '12', 'guibg': '#0000ff'}, 'normal': False},
    105: {'description': 'Set background color to Magenta', 'attributes': {'ctermbg': '13', 'guibg': '#990099'}, 'normal': False},
    106: {'description': 'Set background color to Cyan', 'attributes': {'ctermbg': '14', 'guibg': '#009999'}, 'normal': False},
    107: {'description': 'Set background color to White', 'attributes': {'ctermbg': '15', 'guibg': '#ffffff'}, 'normal': False}
}
# }}}

# regular expression matching (almost) all control sequences
CONQUE_SEQ_REGEX = re.compile(u("(\x1b\[?\??#?[0-9;]*[a-zA-Z0-9@=>]|\x1b\][0-9];.*?\x07|[\x01-\x0f]|\x1b\([AB0])"), re.UNICODE)
CONQUE_SEQ_REGEX_CTL = re.compile(u("^[\x01-\x0f]$"), re.UNICODE)
CONQUE_SEQ_REGEX_CSI = re.compile(u("^\x1b\["), re.UNICODE)
CONQUE_SEQ_REGEX_TITLE = re.compile(u("^\x1b\]"), re.UNICODE)
CONQUE_SEQ_REGEX_HASH = re.compile(u("^\x1b#"), re.UNICODE)
CONQUE_SEQ_REGEX_ESC = re.compile(u("^\x1b.$"), re.UNICODE)
CONQUE_SEQ_REGEX_CHAR = re.compile(u("^\x1b\("), re.UNICODE)

# match table output
CONQUE_TABLE_OUTPUT = re.compile("^\s*\|\s.*\s\|\s*$|^\s*\+[=+-]+\+\s*$")

# }}}

# Windows subprocess config {{{

CONQUE_SEQ_REGEX_VK = re.compile(u("(\x1b\[\d{1,3}VK)"), re.UNICODE)

# }}}

CONQUE_COLOR_SEQUENCE = (
    '000', '009', '090', '099', '900', '909', '990', '999',
    '000', '00f', '0f0', '0ff', 'f00', 'f0f', 'ff0', 'fff'
)

# vim:foldmethod=marker
autoload/conque_term/conque_subprocess.py	[[[1
195
# FILE:     autoload/conque_term/conque_subprocess.py {{{
# AUTHOR:   Nico Raffo <nicoraffo@gmail.com>
# WEBSITE:  http://conque.googlecode.com
# MODIFIED: 2010-11-15
# VERSION:  2.0, for Vim 7.0
# LICENSE:
# Conque - Vim terminal/console emulator
# Copyright (C) 2009-2010 Nico Raffo
#
# MIT License
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE. }}}

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

if CONQUE_PLATFORM == 'nix':
    import os
    import signal
    import pty
    import tty
    import select
    import fcntl
    import termios
    import struct


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
    def open(self, command, env={}): # {{{

        # parse command
        command_arr = command.split()
        executable = command_arr[0]
        args = command_arr

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
                attrs[6][tty.VMIN] = 1
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
    def read(self, timeout=1): # {{{

        output = ''
        read_timeout = float(timeout) / 1000

        try:
            # read from fd until no more output
            while 1:
                s_read, s_write, s_error = select.select([self.fd], [], [], read_timeout)

                lines = ''
                for s_fd in s_read:
                    try:
                        lines = os.read(self.fd, 32)
                    except:
                        pass
                    output = output + lines.decode('utf-8')

                if lines == '':
                    break
        except:
            pass

        return output
        # }}}

    # I guess this one's not bad
    def write(self, input): # {{{
        try:
            if CONQUE_PYTHON_VERSION == 2:
                os.write(self.fd, input)
            else:
                os.write(self.fd, bytes(input, 'utf-8'))
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
            if os.waitpid(self.pid, os.WNOHANG)[0]:
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


# vim:foldmethod=marker
autoload/conque_term/conque_sole_subprocess.py	[[[1
753
# FILE:     autoload/conque_term/conque_sole_subprocess.py {{{
# AUTHOR:   Nico Raffo <nicoraffo@gmail.com>
# WEBSITE:  http://conque.googlecode.com
# MODIFIED: 2010-11-15
# VERSION:  2.0, for Vim 7.0
# LICENSE:
# Conque - Vim terminal/console emulator
# Copyright (C) 2009-2010 Nico Raffo
#
# MIT License
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE. }}}

""" ConqueSoleSubprocess {{{

Creates a new subprocess with it's own (hidden) console window.

Mirrors console window text onto a block of shared memory (mmap), along with
text attribute data. Also handles translation of text input into the format
Windows console expects.

Sample Usage:

    sh = ConqueSoleSubprocess()
    sh.open("cmd.exe", "unique_str")

    shm_in = ConqueSoleSharedMemory(mem_key = "unique_str", mem_type = "input", ...)
    shm_out = ConqueSoleSharedMemory(mem_key = "unique_str", mem_type = "output", ...)

    output = shm_out.read(...)
    shm_in.write("dir\r")
    output = shm_out.read(...)

Requirements:

    * Python for Windows extensions. Available at http://sourceforge.net/projects/pywin32/
    * Must be run from process attached to an existing console.

}}} """

import time
import re
import os
import ctypes

from conque_globals import *
from conque_win32_util import *
from conque_sole_shared_memory import *


class ConqueSoleSubprocess():

    # Class properties {{{

    #window = None
    handle = None
    pid = None

    # input / output handles
    stdin = None
    stdout = None

    # size of console window
    window_width = 160
    window_height = 40

    # max lines for the console buffer
    buffer_width = 160
    buffer_height = 100

    # keep track of the buffer number at the top of the window
    top = 0
    line_offset = 0

    # buffer height is CONQUE_SOLE_BUFFER_LENGTH * output_blocks
    output_blocks = 1

    # cursor position
    cursor_line = 0
    cursor_col = 0

    # console data, array of lines
    data = []

    # console attribute data, array of array of int
    attributes = []
    attribute_cache = {}

    # default attribute
    default_attribute = 7

    # shared memory objects
    shm_input = None
    shm_output = None
    shm_attributes = None
    shm_stats = None
    shm_command = None
    shm_rescroll = None
    shm_resize = None

    # are we still a valid process?
    is_alive = True

    # used for periodic execution of screen and memory redrawing
    screen_redraw_ct = 0
    mem_redraw_ct = 0

    # }}}

    # ****************************************************************************
    # initialize class instance

    def __init__(self): # {{{

        pass

    # }}}

    # ****************************************************************************
    # Create proccess cmd

    def open(self, cmd, mem_key, options={}): # {{{



        self.reset = True

        try:
            # if we're already attached to a console, then unattach
            try:
                ctypes.windll.kernel32.FreeConsole()
            except:
                pass

            # set buffer height
            self.buffer_height = CONQUE_SOLE_BUFFER_LENGTH

            if 'LINES' in options and 'COLUMNS' in options:
                self.window_width = options['COLUMNS']
                self.window_height = options['LINES']
                self.buffer_width = options['COLUMNS']

            # console window options
            si = STARTUPINFO()

            # hide window
            si.dwFlags |= STARTF_USESHOWWINDOW
            si.wShowWindow = SW_HIDE
            #si.wShowWindow = SW_MINIMIZE

            # process options
            flags = NORMAL_PRIORITY_CLASS | CREATE_NEW_PROCESS_GROUP | CREATE_UNICODE_ENVIRONMENT | CREATE_NEW_CONSOLE

            # created process info
            pi = PROCESS_INFORMATION()



            # create the process!
            res = ctypes.windll.kernel32.CreateProcessW(None, u(cmd), None, None, 0, flags, None, u('.'), ctypes.byref(si), ctypes.byref(pi))




            self.pid = pi.dwProcessId
            self.handle = pi.hProcess



            # attach ourselves to the new console
            # console is not immediately available
            for i in range(10):
                time.sleep(1)
                try:

                    res = ctypes.windll.kernel32.AttachConsole(self.pid)






                    break
                except:

                    pass

            # get input / output handles
            self.stdout = ctypes.windll.kernel32.GetStdHandle(STD_OUTPUT_HANDLE)
            self.stdin = ctypes.windll.kernel32.GetStdHandle(STD_INPUT_HANDLE)

            # set buffer size
            size = COORD(self.buffer_width, self.buffer_height)
            res = ctypes.windll.kernel32.SetConsoleScreenBufferSize(self.stdout, size)







            # prev set size call needs to process
            time.sleep(0.2)

            # set window size
            self.set_window_size(self.window_width, self.window_height)

            # init shared memory
            self.init_shared_memory(mem_key)

            # init read buffers
            self.tc = ctypes.create_unicode_buffer(self.buffer_width)
            self.ac = ctypes.create_unicode_buffer(self.buffer_width)

            return True

        except:

            return False

    # }}}

    # ****************************************************************************
    # create shared memory objects

    def init_shared_memory(self, mem_key): # {{{

        buf_info = self.get_buffer_info()




        self.shm_input = ConqueSoleSharedMemory(CONQUE_SOLE_INPUT_SIZE, 'input', mem_key)
        self.shm_input.create('write')
        self.shm_input.clear()

        self.shm_output = ConqueSoleSharedMemory(self.buffer_height * self.buffer_width, 'output', mem_key, True)
        self.shm_output.create('write')
        self.shm_output.clear()

        self.shm_attributes = ConqueSoleSharedMemory(self.buffer_height * self.buffer_width, 'attributes', mem_key, True, chr(buf_info.wAttributes), encoding='latin-1')
        self.shm_attributes.create('write')
        self.shm_attributes.clear()

        self.shm_stats = ConqueSoleSharedMemory(CONQUE_SOLE_STATS_SIZE, 'stats', mem_key, serialize=True)
        self.shm_stats.create('write')
        self.shm_stats.clear()

        self.shm_command = ConqueSoleSharedMemory(CONQUE_SOLE_COMMANDS_SIZE, 'command', mem_key, serialize=True)
        self.shm_command.create('write')
        self.shm_command.clear()

        self.shm_resize = ConqueSoleSharedMemory(CONQUE_SOLE_RESIZE_SIZE, 'resize', mem_key, serialize=True)
        self.shm_resize.create('write')
        self.shm_resize.clear()

        self.shm_rescroll = ConqueSoleSharedMemory(CONQUE_SOLE_RESCROLL_SIZE, 'rescroll', mem_key, serialize=True)
        self.shm_rescroll.create('write')
        self.shm_rescroll.clear()

        return True

    # }}}

    # ****************************************************************************
    # check for and process commands

    def check_commands(self): # {{{

        cmd = self.shm_command.read()

        if cmd:

            # shut it all down
            if cmd['cmd'] == 'close':

                # clear command
                self.shm_command.clear()

                self.close()
                return

        cmd = self.shm_resize.read()

        if cmd:

            # clear command
            self.shm_resize.clear()

            # resize console
            if cmd['cmd'] == 'resize':



                # only change buffer width if it's larger
                if cmd['data']['width'] > self.buffer_width:
                    self.buffer_width = cmd['data']['width']

                # always change console width and height
                self.window_width = cmd['data']['width']
                self.window_height = cmd['data']['height']

                # reset the console
                buf_info = self.get_buffer_info()
                self.reset_console(buf_info, add_block=False)

        # }}}

    # ****************************************************************************
    # read from windows console and update output buffer

    def read(self, timeout=0): # {{{

        # no point really
        if self.screen_redraw_ct == 0 and not self.is_alive():
            stats = {'top_offset': 0, 'default_attribute': 0, 'cursor_x': 0, 'cursor_y': self.cursor_line, 'is_alive': 0}

            self.shm_stats.write(stats)
            return

        # check for commands
        self.check_commands()

        # emulate timeout by sleeping timeout time
        if timeout > 0:
            read_timeout = float(timeout) / 1000

            time.sleep(read_timeout)

        # get cursor position
        buf_info = self.get_buffer_info()
        curs_line = buf_info.dwCursorPosition.Y
        curs_col = buf_info.dwCursorPosition.X

        # set update range
        if curs_line != self.cursor_line or self.top != buf_info.srWindow.Top or self.screen_redraw_ct == CONQUE_SOLE_SCREEN_REDRAW:
            self.screen_redraw_ct = 0

            read_start = self.top
            read_end = buf_info.srWindow.Bottom + 1
        else:

            read_start = curs_line
            read_end = curs_line + 1

        # vars used in for loop
        coord = COORD(0, 0)
        chars_read = ctypes.c_int(0)

        # read new data
        for i in range(read_start, read_end):

            coord.Y = i

            res = ctypes.windll.kernel32.ReadConsoleOutputCharacterW(self.stdout, ctypes.byref(self.tc), self.buffer_width, coord, ctypes.byref(chars_read))
            ctypes.windll.kernel32.ReadConsoleOutputAttribute(self.stdout, ctypes.byref(self.ac), self.buffer_width, coord, ctypes.byref(chars_read))

            t = self.tc.value
            a = self.ac.value




            # add data
            if i >= len(self.data):
                self.data.append(t)
                self.attributes.append(a)
            else:
                self.data[i] = t
                self.attributes[i] = a

        # write new output to shared memory
        if self.mem_redraw_ct == CONQUE_SOLE_MEM_REDRAW:
            self.mem_redraw_ct = 0

            self.shm_output.write(''.join(self.data))
            self.shm_attributes.write(''.join(self.attributes))
        else:

            self.shm_output.write(text=''.join(self.data[read_start:read_end]), start=read_start * self.buffer_width)
            self.shm_attributes.write(text=''.join(self.attributes[read_start:read_end]), start=read_start * self.buffer_width)

        # write cursor position to shared memory
        stats = {'top_offset': buf_info.srWindow.Top, 'default_attribute': buf_info.wAttributes, 'cursor_x': curs_col, 'cursor_y': curs_line, 'is_alive': 1}
        self.shm_stats.write(stats)


        # adjust screen position
        self.top = buf_info.srWindow.Top
        self.cursor_line = curs_line

        # check for reset
        if curs_line > buf_info.dwSize.Y - 200:
            self.reset_console(buf_info)

        # increment redraw counters
        self.screen_redraw_ct += 1
        self.mem_redraw_ct += 1

        return None

    # }}}

    # ****************************************************************************
    # clear the console and set cursor at home position

    def reset_console(self, buf_info, add_block=True): # {{{

        # sometimes we just want to change the buffer width,
        # in which case no need to add another block
        if add_block:
            self.output_blocks += 1

        # close down old memory
        self.shm_output.close()
        self.shm_output = None

        self.shm_attributes.close()
        self.shm_attributes = None

        # new shared memory key
        mem_key = 'mk' + str(time.time())

        # reallocate memory
        self.shm_output = ConqueSoleSharedMemory(self.buffer_height * self.buffer_width * self.output_blocks, 'output', mem_key, True)
        self.shm_output.create('write')
        self.shm_output.clear()

        # backfill data
        if len(self.data[0]) < self.buffer_width:
            for i in range(0, len(self.data)):
                self.data[i] = self.data[i] + ' ' * (self.buffer_width - len(self.data[i]))
        self.shm_output.write(''.join(self.data))

        self.shm_attributes = ConqueSoleSharedMemory(self.buffer_height * self.buffer_width * self.output_blocks, 'attributes', mem_key, True, chr(buf_info.wAttributes), encoding='latin-1')
        self.shm_attributes.create('write')
        self.shm_attributes.clear()

        # backfill attributes
        if len(self.attributes[0]) < self.buffer_width:
            for i in range(0, len(self.attributes)):
                self.attributes[i] = self.attributes[i] + chr(buf_info.wAttributes) * (self.buffer_width - len(self.attributes[i]))
        self.shm_attributes.write(''.join(self.attributes))

        # notify wrapper of new output block
        self.shm_rescroll.write({'cmd': 'new_output', 'data': {'blocks': self.output_blocks, 'mem_key': mem_key}})

        # set buffer size
        size = COORD(X=self.buffer_width, Y=self.buffer_height * self.output_blocks)

        res = ctypes.windll.kernel32.SetConsoleScreenBufferSize(self.stdout, size)






        # prev set size call needs to process
        time.sleep(0.2)

        # set window size
        self.set_window_size(self.window_width, self.window_height)

        # init read buffers
        self.tc = ctypes.create_unicode_buffer(self.buffer_width)
        self.ac = ctypes.create_unicode_buffer(self.buffer_width)

    # }}}

    # ****************************************************************************
    # write text to console. this function just parses out special sequences for
    # special key events and passes on the text to the plain or virtual key functions

    def write(self): # {{{

        # get input from shared mem
        text = self.shm_input.read()

        # nothing to do here
        if text == '':
            return



        # clear input queue
        self.shm_input.clear()

        # split on VK codes
        chunks = CONQUE_SEQ_REGEX_VK.split(text)

        # if len() is one then no vks
        if len(chunks) == 1:
            self.write_plain(text)
            return



        # loop over chunks and delegate
        for t in chunks:

            if t == '':
                continue

            if CONQUE_SEQ_REGEX_VK.match(t):

                self.write_vk(t[2:-2])
            else:
                self.write_plain(t)

    # }}}

    # ****************************************************************************

    def write_plain(self, text): # {{{

        li = INPUT_RECORD * len(text)
        list_input = li()

        for i in range(0, len(text)):
            # create keyboard input
            ke = KEY_EVENT_RECORD()
            ke.bKeyDown = ctypes.c_byte(1)
            ke.wRepeatCount = ctypes.c_short(1)

            cnum = ord(text[i])
            ke.wVirtualKeyCode = ctypes.windll.user32.VkKeyScanW(cnum)
            ke.wVirtualScanCode = ctypes.c_short(ctypes.windll.user32.MapVirtualKeyW(int(cnum), 0))

            if cnum > 31:
                ke.uChar.UnicodeChar = u(chr(cnum))
            elif cnum == 3:
                ctypes.windll.kernel32.GenerateConsoleCtrlEvent(0, self.pid)
                ke.uChar.UnicodeChar = u(chr(cnum))
                ke.wVirtualKeyCode = ctypes.windll.user32.VkKeyScanW(cnum + 96)
                ke.dwControlKeyState = LEFT_CTRL_PRESSED
            else:
                ke.uChar.UnicodeChar = u(chr(cnum))
                if cnum in CONQUE_WINDOWS_VK_INV:
                    ke.wVirtualKeyCode = cnum
                else:
                    ke.wVirtualKeyCode = ctypes.windll.user32.VkKeyScanW(cnum + 96)
                    ke.dwControlKeyState = LEFT_CTRL_PRESSED

            kc = INPUT_RECORD(KEY_EVENT)
            kc.Event.KeyEvent = ke
            list_input[i] = kc



        # write input array
        events_written = ctypes.c_int()
        res = ctypes.windll.kernel32.WriteConsoleInputW(self.stdin, list_input, len(text), ctypes.byref(events_written))








    # }}}

    # ****************************************************************************

    def write_vk(self, vk_code): # {{{


        li = INPUT_RECORD * 1

        # create keyboard input
        ke = KEY_EVENT_RECORD()
        ke.uChar.UnicodeChar = u(chr(0))
        ke.wVirtualKeyCode = ctypes.c_short(int(vk_code))
        ke.wVirtualScanCode = ctypes.c_short(ctypes.windll.user32.MapVirtualKeyW(int(vk_code), 0))
        ke.bKeyDown = ctypes.c_byte(1)
        ke.wRepeatCount = ctypes.c_short(1)

        # set enhanced key mode for arrow keys
        if vk_code in CONQUE_WINDOWS_VK_ENHANCED:

            ke.dwControlKeyState = ENHANCED_KEY

        kc = INPUT_RECORD(KEY_EVENT)
        kc.Event.KeyEvent = ke
        list_input = li(kc)

        # write input array
        events_written = ctypes.c_int()
        res = ctypes.windll.kernel32.WriteConsoleInputW(self.stdin, list_input, 1, ctypes.byref(events_written))







    # }}}

    # ****************************************************************************

    def close(self): # {{{

        # record status
        self.is_alive = False
        try:
            stats = {'top_offset': 0, 'default_attribute': 0, 'cursor_x': 0, 'cursor_y': self.cursor_line, 'is_alive': 0}
            self.shm_stats.write(stats)
        except:
            pass

        pid_list = (ctypes.c_int * 10)()
        num = ctypes.windll.kernel32.GetConsoleProcessList(pid_list, 10)



        current_pid = os.getpid()





        # kill subprocess pids
        for pid in pid_list[0:num]:
            if not pid:
                break

            # kill current pid last
            if pid == current_pid:
                continue
            try:
                self.close_pid(pid)
            except:

                pass

        # kill this process
        try:
            self.close_pid(current_pid)
        except:

            pass

    def close_pid(self, pid):

        handle = ctypes.windll.kernel32.OpenProcess(PROCESS_TERMINATE, 0, pid)
        ctypes.windll.kernel32.TerminateProcess(handle, -1)
        ctypes.windll.kernel32.CloseHandle(handle)

    # }}}

    # ****************************************************************************
    # check process health

    def is_alive(self): # {{{

        status = ctypes.windll.kernel32.WaitForSingleObject(self.handle, 1)

        if status == 0:

            self.is_alive = False

        return self.is_alive

        # }}}


    # ****************************************************************************
    # return screen data as string

    def get_screen_text(self): # {{{

        return "\n".join(self.data)

        # }}}

    # ****************************************************************************

    def set_window_size(self, width, height): # {{{



        # get current window size object
        window_size = SMALL_RECT(0, 0, 0, 0)

        # buffer info has maximum window size data
        buf_info = self.get_buffer_info()


        # set top left corner
        window_size.Top = 0
        window_size.Left = 0

        # set bottom right corner
        if buf_info.dwMaximumWindowSize.X < width:

            window_size.Right = buf_info.dwMaximumWindowSize.X - 1
        else:
            window_size.Right = width - 1

        if buf_info.dwMaximumWindowSize.Y < height:

            window_size.Bottom = buf_info.dwMaximumWindowSize.Y - 1
        else:
            window_size.Bottom = height - 1



        # set the window size!
        res = ctypes.windll.kernel32.SetConsoleWindowInfo(self.stdout, ctypes.c_bool(True), ctypes.byref(window_size))






        # reread buffer info to get final console max lines
        buf_info = self.get_buffer_info()

        self.window_width = buf_info.srWindow.Right + 1
        self.window_height = buf_info.srWindow.Bottom + 1

        # }}}

    # ****************************************************************************
    # get buffer info, used a lot

    def get_buffer_info(self): # {{{

        buf_info = CONSOLE_SCREEN_BUFFER_INFO()
        ctypes.windll.kernel32.GetConsoleScreenBufferInfo(self.stdout, ctypes.byref(buf_info))

        return buf_info

        # }}}


# vim:foldmethod=marker
autoload/conque_term.vim	[[[1
1272
" FILE:     autoload/conque_term.vim {{{
" AUTHOR:   Nico Raffo <nicoraffo@gmail.com>
" WEBSITE:  http://conque.googlecode.com
" MODIFIED: 2010-11-15
" VERSION:  2.0, for Vim 7.0
" LICENSE:
" Conque - Vim terminal/console emulator
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

" {{{

" path to this file
let s:scriptfile = expand("<sfile>") 
let s:scriptdir = expand("<sfile>:h") . '/'
let s:scriptdirpy = expand("<sfile>:h") . '/conque_term/'

" Extra key codes
let s:input_extra = []

let s:term_obj = { 'idx' : 1, 'var' : '', 'is_buffer' : 1, 'active' : 1, 'buffer_name' : '' }
let s:terminals = {}

let s:save_updatetime = &updatetime

let s:initialized = 0

" }}}

" **********************************************************************************************************
" **** SYSTEM DETECTION ************************************************************************************
" **********************************************************************************************************

" {{{

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
        call append('$', "C:\\PythonXX\\. To fix this error either:")
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

function! conque_term#dependency_check() " {{{

    " don't recheck the second time 'round
    if s:initialized == 1
        return 1
    endif

    " choose a python version and define a string unicoding function
    let s:py = ''
    if g:ConqueTerm_PyVersion == 3
        let s:pytest = 'python3'
    else
        let s:pytest = 'python'
        let g:ConqueTerm_PyVersion = 2
    endif

    " first the requested version
    if has(s:pytest)
        if s:pytest == 'python3'
            let s:py = 'py3'
        else
            let s:py = 'py'
        endif

    " otherwise use the other version
    else
        let s:py_alternate = 5 - g:ConqueTerm_PyVersion
        if s:py_alternate == 3
            let s:pytest = 'python3'
        else
            let s:pytest = 'python'
        endif
        if has(s:pytest)
            echohl WarningMsg | echomsg "Python " . g:ConqueTerm_PyVersion . " interface is not installed, using Python " . s:py_alternate . " instead" | echohl None
            let g:ConqueTerm_PyVersion = s:py_alternate
            if s:pytest == 'python3'
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
        let s:platform = 'nix'
        sil exe s:py . " CONQUE_PLATFORM = 'nix'"
    else
        let s:platform = 'dos'
        sil exe s:py . " CONQUE_PLATFORM = 'dos'"
    endif

    " if we're using Windows, make sure ctypes is available
    if s:platform == 'dos'
        try
            sil exe s:py . " import ctypes"
        catch
            call conque_term#fail('ctypes')
            return 0
        endtry
    endif

    " if we're using Windows, make sure we can finde python executable
    if s:platform == 'dos' && conque_term#find_python_exe() == ''
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
        echohl WarningMsg | echomsg "Warning: Global CursorHoldI and CursorMovedI autocommands may cause ConqueTerm to run slowly." | echohl None
    endfor

    " if we're all good, load python files
    call conque_term#load_python()

    return 1

endfunction " }}}

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
    let hooks   = get(a:000, 1, [])
    let return_to_current  = get(a:000, 2, 0)
    let is_buffer  = get(a:000, 3, 1)

    " dependency check
    if conque_term#dependency_check() == 0
        return 0
    endif

    " switch to buffer if needed
    if is_buffer && return_to_current
      let save_sb = &switchbuf

      "use an agressive sb option
      sil set switchbuf=usetab

      " current buffer name
      let current_buffer = bufname("%")
    endif

    " bare minimum validation
    if s:py == ''
        echohl WarningMsg | echomsg "Conque requires the Python interface to be installed" | echohl None
        return 0
    endif
    if empty(command)
        echohl WarningMsg | echomsg "No command found" | echohl None
        return 0
    else
        let l:cargs = split(command, '[^\\]\@<=\s')
        let l:cargs[0] = substitute(l:cargs[0], '\\ ', ' ', 'g')
        if !executable(l:cargs[0])
            echohl WarningMsg | echomsg "Not an executable: " . l:cargs[0] | echohl None
            return 0
        endif
    endif

    let g:ConqueTerm_Idx += 1
    let g:ConqueTerm_Var = 'ConqueTerm_' . g:ConqueTerm_Idx
    let g:ConqueTerm_BufName = substitute(command, ' ', '\\ ', 'g') . "\\ -\\ " . g:ConqueTerm_Idx

    " initialize global mappings if needed
    call conque_term#init()

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
        if s:platform == 'nix'
            execute s:py . ' ' . g:ConqueTerm_Var . ' = Conque()'
            execute s:py . ' ' . g:ConqueTerm_Var . ".open('" . conque_term#python_escape(command) . "', " . l:config . ")"
        else
            " find python.exe and communicator
            let py_exe = conque_term#python_escape(conque_term#find_python_exe())
            let py_vim = conque_term#python_escape(s:scriptdirpy . 'conque_sole_communicator.py')
            if py_exe == ''
                return 0
            endif
            execute s:py . ' ' . g:ConqueTerm_Var . ' = ConqueSole()'
            execute s:py . ' ' . g:ConqueTerm_Var . ".open('" . conque_term#python_escape(command) . "', " . l:config . ", '" . py_exe . "', '" . py_vim . "')"

            "call conque_term#init_conceal_color()
        endif
    catch
        echohl WarningMsg | echomsg "An error occurred: " . command | echohl None
        return
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
        sil exe s:py . ' ' . g:ConqueTerm_Var . '.idle()'
    endif
    return t_obj

endfunction " }}}

" set buffer options
function! conque_term#set_buffer_settings(command, pre_hooks) "{{{

    " optional hooks to execute, e.g. 'split'
    for h in a:pre_hooks
        sil exe h
    endfor
    sil exe 'edit ++enc=utf-8 ' . g:ConqueTerm_BufName

    " showcmd gets altered by nocompatible
    let sc_save = &showcmd

    " buffer settings 
    setlocal fileencoding=utf-8 " file encoding, even tho there's no file
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
    setlocal noreadonly        " this is not actually a readonly buffer
    if v:version >= 703
        setlocal conceallevel=3
        setlocal concealcursor=nic
    endif
    setfiletype conque_term    " useful
    sil exe "setlocal syntax=" . g:ConqueTerm_Syntax

    " reset showcmd
    if sc_save
      set showcmd
    else
      set noshowcmd
    endif

    " temporary global settings go in here
    call conque_term#on_focus(1)

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
        execute 'autocmd! ' . b:ConqueTerm_Var

    else
        execute 'augroup ' . b:ConqueTerm_Var

        " handle unexpected closing of shell, passes HUP to parent and all child processes
        execute 'autocmd ' . b:ConqueTerm_Var . ' BufUnload <buffer> ' . s:py . ' ' . b:ConqueTerm_Var . '.close()'

        " check for resized/scrolled buffer when entering buffer
        execute 'autocmd ' . b:ConqueTerm_Var . ' BufEnter <buffer> ' . s:py . ' ' . b:ConqueTerm_Var . '.update_window_size()'
        execute 'autocmd ' . b:ConqueTerm_Var . ' VimResized ' . s:py . ' ' . b:ConqueTerm_Var . '.update_window_size()'

        " set/reset updatetime on entering/exiting buffer
        execute 'autocmd ' . b:ConqueTerm_Var . ' BufEnter <buffer> call conque_term#on_focus()'
        execute 'autocmd ' . b:ConqueTerm_Var . ' BufLeave <buffer> call conque_term#on_blur()'

        " reposition cursor when going into insert mode
        execute 'autocmd ' . b:ConqueTerm_Var . ' InsertEnter <buffer> ' . s:py . ' ' . b:ConqueTerm_Var . '.insert_enter()'

        " poll for more output
        sil execute 'autocmd ' . b:ConqueTerm_Var . ' CursorHoldI <buffer> ' . s:py . ' ' .  b:ConqueTerm_Var . '.auto_read()'
    endif
    " }}}

    " map ASCII 1-31 {{{
    for c in range(1, 31)
        " <Esc>
        if c == 27 || c == 3
            continue
        endif
        if l:action == 'start'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <C-' . nr2char(64 + c) . '> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(chr(' . c . '))<CR>'
        else
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <C-' . nr2char(64 + c) . '>'
        endif
    endfor
    " bonus mapping: send <C-c> in normal mode to terminal as well for panic interrupts
    if l:action == 'start'
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <C-c> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(chr(3))<CR>'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> <C-c> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(chr(3))<CR>'
    else
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <C-c>'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> <C-c>'
    endif

    " leave insert mode
    if !exists('g:ConqueTerm_EscKey') || g:ConqueTerm_EscKey == '<Esc>'
        " use <Esc><Esc> to send <Esc> to terminal
        if l:action == 'start'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Esc><Esc> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(chr(27))<CR>'
        else
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Esc><Esc>'
        endif
    else
        " use <Esc> to send <Esc> to terminal
        if l:action == 'start'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> ' . g:ConqueTerm_EscKey . ' <Esc>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Esc> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(chr(27))<CR>'
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
    " }}}

    " map ASCII 33-127 {{{
    for i in range(33, 127)
        " <Bar>
        if i == 124
            if l:action == 'start'
                sil exe "i" . map_modifier . "map <silent> <buffer> <Bar> <C-o>:" . s:py . ' ' . b:ConqueTerm_Var . ".write(chr(124))<CR>"
            else
                sil exe "i" . map_modifier . "map <silent> <buffer> <Bar>"
            endif
            continue
        endif
        if l:action == 'start'
            sil exe "i" . map_modifier . "map <silent> <buffer> " . nr2char(i) . " <C-o>:" . s:py . ' ' . b:ConqueTerm_Var . ".write(chr(" . i . "))<CR>"
        else
            sil exe "i" . map_modifier . "map <silent> <buffer> " . nr2char(i)
        endif
    endfor
    " }}}

    " map Latin-1 128-255 {{{
    for i in range(128, 255)
        if l:action == 'start'
            sil exe "i" . map_modifier . "map <silent> <buffer> " . nr2char(i) . " <C-o>:" . s:py . ' ' . b:ConqueTerm_Var . ".write_latin1(chr(" . i . "))<CR>"
        else
            sil exe "i" . map_modifier . "map <silent> <buffer> " . nr2char(i)
        endif
    endfor
    " }}}

    " Special keys {{{
    if l:action == 'start'
        if s:platform == 'nix'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <BS> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write("\x08")<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Space> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(" ")<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Up> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write("\x1b[A")<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Down> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write("\x1b[B")<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Right> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write("\x1b[C")<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Left> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write("\x1b[D")<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Home> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write("\x1bOH")<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <End> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write("\x1bOF")<CR>'
        else
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <BS> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write("\x08")<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Space> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write(" ")<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Up> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_UP . ')<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Down> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_DOWN . ')<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Right> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_RIGHT . ')<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Left> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_LEFT . ')<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Del> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_DELETE . ')<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <Home> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_HOME . ')<CR>'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> <End> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write_vk(' . s:windows_vk.VK_END . ')<CR>'
        endif
    else
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <BS>'
        sil exe 'i' . map_modifier . 'map <silent> <buffer> <Space>'
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
            if s:platform == 'nix'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F1>  <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write("\x1b[11~")<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F2>  <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write("\x1b[12~")<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F3>  <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write("1b[13~")<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F4>  <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write("\x1b[14~")<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F5>  <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write("\x1b[15~")<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F6>  <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write("\x1b[17~")<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F7>  <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write("\x1b[18~")<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F8>  <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write("\x1b[19~")<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F9>  <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write("\x1b[20~")<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F10> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write("\x1b[21~")<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F11> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write("\x1b[23~")<CR>'
                sil exe 'i' . map_modifier . 'map <silent> <buffer> <F12> <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . '.write("\x1b[24~")<CR>'
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

    " send selected text into conque {{{
    if l:action == 'start'
        sil exe 'v' . map_modifier . 'map <silent> ' . g:ConqueTerm_SendVisKey . ' :<C-u>call conque_term#send_selected(visualmode())<CR>'
    endif
    " }}}

    " remap paste keys {{{
    if l:action == 'start'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> p :' . s:py . ' ' . b:ConqueTerm_Var . '.write(vim.eval("@@"))<CR>a'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> P :' . s:py . ' ' . b:ConqueTerm_Var . '.write(vim.eval("@@"))<CR>a'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> ]p :' . s:py . ' ' . b:ConqueTerm_Var . '.write(vim.eval("@@"))<CR>a'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> [p :' . s:py . ' ' . b:ConqueTerm_Var . '.write(vim.eval("@@"))<CR>a'
    else
        sil exe 'n' . map_modifier . 'map <silent> <buffer> p'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> P'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> ]p'
        sil exe 'n' . map_modifier . 'map <silent> <buffer> [p'
    endif
    if has('gui_running') == 1
        if l:action == 'start'
            sil exe 'i' . map_modifier . 'map <buffer> <S-Insert> <Esc>:' . s:py . ' ' . b:ConqueTerm_Var . ".write(vim.eval('@+'))<CR>a"
            sil exe 'i' . map_modifier . 'map <buffer> <S-Help> <Esc>:<C-u>' . s:py . ' ' . b:ConqueTerm_Var . ".write(vim.eval('@+'))<CR>a"
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

    " user defined mappings {{{
    for [map_from, map_to] in s:input_extra
        if l:action == 'start'
            sil exe 'i' . map_modifier . 'map <silent> <buffer> ' . map_from . ' <C-o>:' . s:py . ' ' . b:ConqueTerm_Var . ".write('" . conque_term#python_escape(map_to) . "')<CR>"
        else
            sil exe 'i' . map_modifier . 'map <silent> <buffer> ' . map_from
        endif
    endfor
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

endfunction " }}}

" Initialize global mappings. Should only be called once per Vim session
function! conque_term#init() " {{{

    if s:initialized == 1
        return
    endif

    augroup ConqueTerm
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

    if startup == 0 && exists('b:ConqueTerm_Var')
        sil exe s:py . ' ' . g:ConqueTerm_Var . '.resume()'
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
endfunction " }}}

" bell event (^G)
function! conque_term#bell() " {{{
    echohl WarningMsg | echomsg "BELL!" | echohl None
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
    syn region ConqueCCB000 matchgroup=ConqueConceal start="\esb000;" end="\eeb000;" concealends
    syn region ConqueCCB00c matchgroup=ConqueConceal start="\esb00c;" end="\eeb00c;" concealends
    syn region ConqueCCB0c0 matchgroup=ConqueConceal start="\esb0c0;" end="\eeb0c0;" concealends
    syn region ConqueCCB0cc matchgroup=ConqueConceal start="\esb0cc;" end="\eeb0cc;" concealends
    syn region ConqueCCBc00 matchgroup=ConqueConceal start="\esbc00;" end="\eebc00;" concealends
    syn region ConqueCCBc0c matchgroup=ConqueConceal start="\esbc0c;" end="\eebc0c;" concealends
    syn region ConqueCCBcc0 matchgroup=ConqueConceal start="\esbcc0;" end="\eebcc0;" concealends
    syn region ConqueCCBccc matchgroup=ConqueConceal start="\esbccc;" end="\eebccc;" concealends

    " background colors, high intensity
    syn region ConqueCCB000 matchgroup=ConqueConceal start="\esb000;" end="\eeb000;" concealends
    syn region ConqueCCB00f matchgroup=ConqueConceal start="\esb00f;" end="\eeb00f;" concealends
    syn region ConqueCCB0f0 matchgroup=ConqueConceal start="\esb0f0;" end="\eeb0f0;" concealends
    syn region ConqueCCB0ff matchgroup=ConqueConceal start="\esb0ff;" end="\eeb0ff;" concealends
    syn region ConqueCCBf00 matchgroup=ConqueConceal start="\esbf00;" end="\eebf00;" concealends
    syn region ConqueCCBf0f matchgroup=ConqueConceal start="\esbf0f;" end="\eebf0f;" concealends
    syn region ConqueCCBff0 matchgroup=ConqueConceal start="\esbff0;" end="\eebff0;" concealends
    syn region ConqueCCBfff matchgroup=ConqueConceal start="\esbfff;" end="\eebfff;" concealends


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
    sil exe s:py . ' ' . g:ConqueTerm_Var . '.paste_selection()'

    " reset original values
    let @@ = reg_save
    sil exe 'set switchbuf=' . sb_save

    " scroll buffer left
    startinsert!
    normal 0zH
endfunction "}}}

" **********************************************************************************************************
" **** "API" functions *************************************************************************************
" **********************************************************************************************************

" See doc/conque_term.txt for full documentation {{{

" Write to a conque terminal buffer
function! s:term_obj.write(text) dict " {{{

    " if we're not in terminal buffer, pass flag to not position the cursor
    sil exe s:py . ' ' . self.var . '.write(vim.eval("a:text"), False, False)'

endfunction " }}}

" same as write() but adds a newline
function! s:term_obj.writeln(text) dict " {{{

    call self.write(a:text . "\r")

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

    let s:terminals[self.idx].callback = function(a:callback_func)

endfunction " }}}

" close subprocess with ABORT signal
function! s:term_obj.close() dict " {{{

    try
        sil exe s:py . ' ' . self.var . '.abort()'
    catch
        " probably already dead
    endtry

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

" }}}

" **********************************************************************************************************
" **** PYTHON **********************************************************************************************
" **********************************************************************************************************

function! conque_term#load_python() " {{{

    exec s:py . "file " . s:scriptdirpy . "conque_globals.py"
    exec s:py . "file " . s:scriptdirpy . "conque.py"
    exec s:py . "file " . s:scriptdirpy . "conque_screen.py"
    exec s:py . "file " . s:scriptdirpy . "conque_subprocess.py"
    if s:platform == 'dos'
        exec s:py . "file " . s:scriptdirpy . "conque_win32_util.py"
        exec s:py . "file " . s:scriptdirpy . "conque_sole_shared_memory.py"
        exec s:py . "file " . s:scriptdirpy . "conque_sole.py"
        exec s:py . "file " . s:scriptdirpy . "conque_sole_wrapper.py"
    endif

endfunction " }}}

" vim:foldmethod=marker
plugin/conque_term.vim	[[[1
139
" FILE:     plugin/conque_term.vim {{{
" AUTHOR:   Nico Raffo <nicoraffo@gmail.com>
" WEBSITE:  http://conque.googlecode.com
" MODIFIED: 2010-11-15
" VERSION:  2.0, for Vim 7.0
" LICENSE:
" Conque - Vim terminal/console emulator
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

" {{{

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

" Use this key to toggle terminal key mappings. {{{
if !exists('g:ConqueTerm_ToggleKey')
    let g:ConqueTerm_ToggleKey = '<F8>'
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

" }}}

" **********************************************************************************************************
" **** Startup *********************************************************************************************
" **********************************************************************************************************

" Startup {{{

let g:ConqueTerm_Loaded = 1
let g:ConqueTerm_Idx = 0
let g:ConqueTerm_Version = 200

command! -nargs=+ -complete=shellcmd ConqueTerm call conque_term#open(<q-args>)
command! -nargs=+ -complete=shellcmd ConqueTermSplit call conque_term#open(<q-args>, ['belowright split'])
command! -nargs=+ -complete=shellcmd ConqueTermVSplit call conque_term#open(<q-args>, ['belowright vsplit'])
command! -nargs=+ -complete=shellcmd ConqueTermTab call conque_term#open(<q-args>, ['tabnew'])

" }}}

" vim:foldmethod=marker
