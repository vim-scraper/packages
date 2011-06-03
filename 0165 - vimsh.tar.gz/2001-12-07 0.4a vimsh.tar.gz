#!/usr/bin/env python
#
# file:     vimsh.py
# purpose:  allows execution of shell commands in a vim buffer
#
# author:   brian m sturk ( bsturk@nh.ultranet.com )
# created:  12/02/01
# last_mod: 12/07/01
# version:  0.4a
#
# usage:         from a script or ex   pyf[ile] vimsh.py
# requirements:  python enabled vim
#                a platform that supports pty -or- popen
#                
# tested on:     vim 6.0 p11 on slackware linux 8.0
# disclaimer:    Use at your own risk, alpha code.  I'm not
#                responsible if it hoses your machine.
#
# limitations:   can only execute line oriented programs, no vim
#                within vim stuff, curses, etc.
#
# customize:     see section way below "CUSTOMIZE"
# 
# notes:
#
#   - Latest version is always available @
#     http://www.nh.ultranet.com/~bsturk/vim.html
#   
#   - Please send bug reports, suggestions, and other *pleasant*
#     email to bsturk@nh.ultranet.com.
#
#   - If you like this script and vim users are allowed to vote on new
#     features in vim again, please put in a vote for vi editing in the ex
#     command window ( not the new command window/buffer ). It's the only
#     feature I would like vim to have. I only ask because if I can get
#     this script to work well lots of people won't be asking for a built
#     in terminal buffer anymore.  :)
#
#   - If you have output problems try increasing the self.delay variable
#     in the vimsh:__init__ method a little
#
#  known issues/todo:
#  
#  TODO:  Allow it to use the current buffer if not-modified
#  TODO:  How do I get the function to not print in the command window
#         when executed?
#  TODO:  new <buffer> uses existing one and this seems to cause
#         problems with notion of last prompt's line etc.  Currently
#         can only be used once, so the buffer needs to be
#         deleted ( bd! ).  __init__ doesn't seem to reset them, need
#         to investiage it more.
#  TODO:  Make it customizable so that you don't have to do a bd!
#  TODO:  Handle modified, and make it optional
#  TODO:  Long commands are unresponsive, i.e. find ~  Figure out a way to
#         print to the file and scroll the buffer as I get input.  May not
#         be possible without returning from python code.
#  TODO:  Handle ansi escape sequences ( colored prompts, LS_COLORS )
#  TODO:  Security, passwords for su, etc not masked
#  TODO:  I sometimes still see an occasional single  for some commands
#  TODO:  Commands with lots of output have lines truncated and continued
#         on the subsequent line ( end of read length:4096 )
#  TODO:  Set prompt until I get ansi parsing/syn hilighting working
#         How can I use regex to determine syntax but hide/remove
#         the escape codes?  Folding??
#
#  history:
#
#    12/05/01 - 0.2a - Fixed tabwidth, not on prompt message, fixed handling
#                      of user input execution rm -i works, shells now die via
#                      autocommand
#    12/06/01 - 0.3a - Fixed the first line issue, and printed s
#    12/07/01 - 0.4a - Implemented clear, exit, and can now alternate between
#                      using popen3(pipes) or pty.  This allows platforms
#                      that do not support pty to work.  Should work on Windows
#                      flavors.
#
###############################################################################

import vim, sys, os, pty, tty, select, string, signal, re, popen2

##############################################################################
##                          class vimsh
##############################################################################

class vimsh:
    def __init__( self, _sh, _arg, _prompt ):
        self.sh    = _sh
        self.arg   = _arg
        self.delay = 0.1

        self.pipe_prompt = _prompt
        self.prompt_line, self.prompt_cursor = vim.current.window.cursor

    def setup_pty( self, _use_pty ):
        self.using_pty = _use_pty

        if _use_pty:
            self.pid, self.fd = pty.fork( )

            self.outd = self.fd
            self.ind  = self.fd
            self.errd = self.fd

            if self.pid == 0:

                attrs = tty.tcgetattr( 1 )
                attrs[6][tty.VMIN]  = 1
                attrs[6][tty.VTIME] = 0
                attrs[3] = attrs[3] & ~tty.ICANON & ~tty.ECHO
                tty.tcsetattr( 1, tty.TCSANOW, attrs )

                os.execv( sh, [ sh, arg ] )
        else:

            ##  use pipes. not as reliable/nice. works OK but with limitations.
            ##  needed for windows support.

            self.stdout, self.stdin, self.stderr = popen2.popen3( self.sh + " " + self.arg, 4096, 'b' )

            self.outd = self.stdout.fileno()
            self.ind  = self.stdin.fileno()
            self.errd = self.stderr.fileno()

    def write( self, _cmd ):
        os.write( self.ind, _cmd )

    def read( self, _buffer ):
        while 1:
            r, w, e = select.select( [ self.outd ], [], [], self.delay )

            for file_iter in r:
                line = os.read( self.outd, 4096 )

                print_lines = string.split( line, '\n' )

                ## if more than one entry, splitting on '\n' seems to return n + 1 entries
                if len( print_lines ) > 1:
                    print_lines = print_lines[ :-1 ]

                for line_iter in print_lines:
                    m = re.search("$", line_iter )

                    cur_line, cur_row = vim.current.window.cursor

                    if m == None and self.using_pty:

                        ##  just print the line no append if we didn't find 
                        _buffer[ cur_line - 1 ] = line_iter 
                        vim.command( "normal G$" )

                    else:
                        if self.using_pty:           # pty leaves trailing 

                            ##  neither of these remove the trailing \n why??
                            # line_iter.strip( )          
                            # re.sub( "\n", "", line_iter )
                            line_iter = line_iter[ :-1 ]   # force it

                        _buffer[ cur_line - 1 ] = line_iter 
                        _buffer.append( "" )

                        vim.command( "normal G$" )

            if r == []:         ##  no more to read
                cur_line, cur_row = vim.current.window.cursor

                if not self.using_pty:
                    _buffer[ cur_line - 1 ] = self.pipe_prompt 
                
                vim.command( "normal G$" )

                ##  tuck away location now that all printing to buffer is done
                self.prompt_line, self.prompt_cursor = vim.current.window.cursor

                ##  seems to be zero based in python, but not in vim??
                self.prompt_cursor += 1
                break

    def execute_cmd( self ):
        ##  For now only allow executing commands on the "latest" prompt line
        ##  or right affter the printing of a line that needs user input, maybe
        ##  map normal 'o' to just send enter and give a new prompt??

        cur = vim.current.buffer
        cur_line, cur_row = vim.current.window.cursor

        if cur_line == self.prompt_line:

            whole_line = cur[ cur_line - 1 ]
            exe_line = whole_line[ self.prompt_cursor: ]

            ##  check for commands that should be handled differently first

            if re.search("^\s*clear", exe_line ):
                self.write( "" + "\n" )    ##  new prompt

                if clear_all:
                    vim.command("ggdG")

                self.end_exe_line()

                if not clear_all:
                    vim.command("normal zt")

            elif re.search("^\s*exit$", exe_line ):
                vim.command("bd!")    ##  TODO:  handle this better
                return

            else:
                self.write( exe_line + "\n" )
                self.end_exe_line()

            vim.command( "startinsert" )

        else:
            print "Not on latest prompt line ( :" + str( self.prompt_line ) + " )"

    def end_exe_line ( self ):
        cur = vim.current.buffer
        cur.append( "" )
        vim.command( "normal G$" )

        self.read( cur )

    def debug_dump_str_as_hex( self, _str ):
        hex_str = ''

        for x in range( 0, len( _str ) ):
             hex_str = hex_str + hex( ord( _str[x] ) ) + "\n"

        print "raw line (hex) is:"
        print hex_str

    def end( self ):
        os.kill( self.pid, signal.SIGKILL )

###############################################################################
##                    Main execution code
##############################################################################

#---------------------------- CUSTOMIZE --------------------------#

#  Used to override in case ansi used, and necessary for non-pty
#  usage.

prompt = "%> "            ##  will also be used as prompt when no pty

## any shell program

sh        = "/bin/sh"

## supplemental argument to shell

arg       = "-i"

## clear shell command behavior
# 0 just scroll for empty screen
# 1 delete contents of buffer

clear_all = 0                   
                                
## subprocess interaction
# 0 use pipes, not as nice output but works on WinXX etc
# 1 use pty, better output, only works on *nix variants

use_pty   = 0

##  Comment these out if you don't have an ansi prompt.
##  may work with multi-line, haven't tried it

os.environ['PROMPT'] = prompt
os.environ['PS1']    = prompt

#---------------------------- END CUSTOMIZE --------------------------#
try:
    ##  TODO:  Need to come up with a way to generate these buffers so
    ##         more than one can be opened

    vim.command( "new vim_shell" )
    vim.command( "setlocal tabstop=8" )
    vim.command( "setlocal modifiable" )
    vim.command( "setlocal noswapfile" )
    vim.command( "setlocal nowrap" )

except:
    print vim.error

cur = vim.current.buffer

vim_shell = vimsh( sh, arg, prompt )
vim_shell.setup_pty( use_pty )

vim.command( "inoremap <buffer> <CR> <esc>:python vim_shell.execute_cmd( )<CR>" )
vim.command( "au BufWipeout vim_shell <esc>:python vim_shell.end( )<CR>")

if use_pty:
    vim_shell.read( cur )

else:
    vim_shell.read( cur )
    cur_line, cur_row = vim.current.window.cursor
    cur[ cur_line - 1 ] = prompt
    vim.command( "normal G$" )

vim.command( "startinsert" )
