#!/usr/bin/env python
#
# file:     vimsh.py
# purpose:  allows execution of shell commands in a vim buffer
#
# author:   brian m sturk ( bsturk@nh.ultranet.com )
# created:  12/02/01
# last_mod: 12/05/01
# version:  0.2a
#
# usage:         from a script or ex   pyf[ile] vimsh.py
# requirements:  python enabled vim, a platform that supports pty
# tested on:     vim 6.0 p11 on slackware linux 8.0
# disclaimer:    Use at your own risk, alpha code.  I'm not
#                responsible if it hoses your machine.
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
#  TODO:  Make it customizable so that you don't have to do a bd!
#  TODO:  How do I get the function to not print in the command window
#         when executed?
#  TODO:  new <buffer> uses existing one and this seems to cause
#         problems with notion of last prompt's line etc.  Currently
#         can only be used once, so the buffer needs to be
#         deleted ( bd! ).  __init__ doesn't seem to reset them, need
#         to investiage it more.
#  TODO:  Fix printing, first line and end of reads get newline added
#         therefore prompt starts on vim line 2, and after 1024
#         I'm going down a line and continuing.  
#  TODO:  Handle modified, and make it optional
#  TODO:  Long commands are unresponsive, i.e. find ~  Figure out a way to
#         print to the file and scroll the buffer as I get input.  May not
#         be possible without returning from python code.
#  TODO:  Look into using popenX and pipes for platforms that don't support
#         pty.  I had fiddled with popen3 before and had lukewarm results.
#  TODO:  The returned cursor column always seems to be off by 1, is it 0
#         indexed?
#  TODO:  Cleaner way of killing the shell process
#  TODO:  Handle ansi escape sequences ( colored prompts, LS_COLORS )
#
#  history:
#
#    12/05/01 - 0.2a - Fixed tabwidth, not on prompt message, fixed handling
#                      of user input execution rm -i works, shells now die via
#                      autocommand
#
###############################################################################

import vim, sys, os, pty, tty, select, string, signal

class vimsh:
    def __init__( self, sh, arg ):
            self.sh    = sh
            self.arg   = arg
            self.delay = 0.1

            self.prompt_line, self.prompt_cursor = vim.current.window.cursor
            self.prompt_line += 1  ##  hack until I write starting from first line

    def setup_pty( self ):

        self.pid, self.fd = pty.fork( )

        if self.pid == 0:

            attrs = tty.tcgetattr( 1 )
            attrs[6][tty.VMIN]  = 1
            attrs[6][tty.VTIME] = 0
            attrs[3] = attrs[3] & ~tty.ICANON & ~tty.ECHO
            tty.tcsetattr( 1, tty.TCSANOW, attrs )

            os.execv( sh, [ sh, arg ] )

    def write( self, cmd ):
        os.write( self.fd, cmd )

    def read( self, buffer ):
        while 1:
            r, w, e = select.select( [ self.fd ], [], [], self.delay )

            for File in r:
                line = os.read( self.fd, 1024 ).strip( )

                ##  should prob be os.pathsep, but vim throws
                ##  exception if they have newlines
                print_lines = string.split( line, '\n' )

                for Line in print_lines:
                    Line.strip( )          

                    ##  TODO: Some commands still have trailing ^M, i.e. find

                    buffer.append( Line )

            if r == []:
                vim.command( "normal G$" )

                ##  seems to be zero based in python, but not in vim??
                self.prompt_line, self.prompt_cursor = vim.current.window.cursor
                self.prompt_cursor += 1

                break

    def execute_cmd( self ):
        ##  For now only allow executing commands on the "latest" prompt line
        ##  or right after the printing of a line that needs user input

        cur = vim.current.buffer
        cur_line, cur_row = vim.current.window.cursor

        if cur_line == self.prompt_line:

            whole_line = cur[ cur_line - 1 ]
            exe_line = whole_line[ self.prompt_cursor: ]

            self.write( exe_line + "\n" )
            self.read( cur )

            vim.command( "startinsert" )

            #self.prompt_line, self.prompt_cursor = vim.current.window.cursor

        else:
            print "Not on latest prompt line ( :" + str( self.prompt_line ) + " )"

    def end( self ):
        os.kill( self.pid, signal.SIGKILL )

###############################################################################
##                    Main execution code
##############################################################################

##  TODO:  Set these until I get ansi parsing/syn hilighting working
##         How can I use regex to determine syntax but hide/remove
##         the escape codes?  Folding??  Or do I keep the convention
##         of overriding the prompt so that if the usr deletes lines
##         etc I can just match the prompt rather than relying
##         on line #s

prompt = "%>"

os.environ['PROMPT'] = prompt
os.environ['PS1']    = prompt

##  TODO:  Get this to work without printing a message about
##         undefined vars.

# try:
    # usr_shell = vim.command( "echo exists( g:vimsh_shell )" )

    # if 0 == usr_shell:
        # sh = "/bin/sh"
    # else:
        # sh = vim.eval( "g:vimsh_shell" )
# except:
    # sh = "/bin/sh"

# try:
    # usr_init_arg = vim.eval( "echo exists( g:vimsh_initarg )" )

    # if 0 == usr_init_arg:
        # arg = "-i"
    # else:
        # arg = vim.eval( "g:vimsh_initarg" )
# except:
    # arg = "-i"

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
del cur[:]

##  temporary, change to suit, but I've only tested with sh so far
sh = "/bin/zsh"
arg = "-i"

vim_shell = vimsh( sh, arg )
vim_shell.setup_pty( )

vim.command( "inoremap <buffer> <CR>  <esc>:python vim_shell.execute_cmd( )<CR>" )
vim.command( "au BufWipeout vim_shell <esc>:python vim_shell.end( )<CR>")

##  Read any ouput shell does at startup, either prompt
##  or as a result of ~.xxxrc file etc, assumption here
##  is the last line of all of the output read in will
##  always be the prompt or a prompt needing user input

vim_shell.read( cur )
vim.command( "startinsert" )
