#!/usr/bin/env python
#
# file:     vimsh.py
# purpose:  allows execution of shell commands in a vim buffer
#
# author:   brian m sturk ( bsturk@nh.ultranet.com )
# created:  12/02/01
# last_mod: 12/10/01
# version:  0.7
#
# usage:         from a script or ex   pyf[ile] vimsh.py
# requirements:  python enabled vim
#                a platform that supports pty -or- popen
# tested on:     vim 6.0 p11 on slackware linux 8.0
#                using Python 2.2b2, 2.1 seems to have an issue
#                with finding termios
#
# license:       Use at your own risk.  I'm not responsible if
#                it hoses your machine.  All I ask is that
#                I'm made aware of changes and my contact
#                info stays in the script.
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
#   - The timeouts for reading are set low (.1 sec) for local filesystem
#     and application use.  If you plan on running telnet/ftp/ssh etc you
#     will need to bump up the timeouts if you have a slower connection.
#     This is not an exact science.  If you're not seeing all of the output
#     or having to hit enter to see output when ftping etc you need to
#     bump the timeout up.  In addition to this if using pipes instead
#     of pty, in general the timeouts should be set a bit longer.
#     A good test is find ~.  Shen set to 0.1, you'll get a prompt
#     before all of the output has been read.  See mapping below in CUSTOMIZE.
#
#  known issues/todo:
#  
#  TODO:  Allow it to use the current buffer if not-modified
#  TODO:  new <buffer> uses existing one.  Currently can only be used once,
#         so the buffer needs to be deleted ( bd! ).
#  TODO:  Make it customizable so that you don't have to do a bd!
#  TODO:  Handle modified, and make it optional
#  TODO:  Long commands are unresponsive, i.e. find ~  Figure out a way to
#         print to the file and scroll the buffer as I get input.  May not
#         be possible without returning from python code.
#  TODO:  I sometimes still see an occasional single  for some commands
#  TODO:  select seems to be unavailable on Windows except for sockets.
#         figure out how to not hang waiting for stuff to read.  
#  TODO:  Handle (syntax hi) ansi escape sequences ( colored prompts, LS_COLORS )
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
#    12/07/01 - 0.5  - Implemented secure input of passwords,
#                      Exit cmd works as expected, for subprocesses it
#                      exits to parent, initial shell exit will delete buffer,
#                      Keep <Delete> from overwriting prompt
#    12/08/01 - 0.6  - Removed the <Delete><BS> hooks.  They just weren't
#                      working as I thought they would.  Now just check
#                      for cursor to not be in prompt. Figured out the ftp
#                      issue see "notes". Added a mapping & func to set
#                      timeouts.  Changed pty prompt to something useful,
#                      Fixed clear
#    12/10/01 - 0.7  - Made import/usage of tty, pty conditional on not being windows
#                      Removed popen buffer size
#                      Increased timeout if using popen3
#                      Fixed output for lines crossing consecutive reads for pty
#                      Added map for starting a new prompt at bottom of buffer
#
###############################################################################

import vim, sys, os, select, string, signal, re

##  If for some reason you'd rather use popen2 on a unix variant or the
##  platform doesn't support pty, add a check for your platform below,
##  or just comment out everything except the import popen2 line, also
##  please shoot me an email if you're on a platform besides Windows
##  doesn't support pty so I can add it to this list

if sys.platform == 'win32':
    import popen2
else:
    import pty, tty

################################################################################
##                             class vimsh                                    ##
################################################################################

class vimsh:
    def __init__( self, _sh, _arg, _prompt ):
        self.sh    = _sh
        self.arg   = _arg

        self.pipe_prompt = _prompt
        self.prompt_line, self.prompt_cursor = self.get_vim_cursor_pos()

        ##  add other password queries to this list

        self.password_regex = ["^Password:",            ##  su
                               "Password required"]     ##  ftp

        self.last_cmd_executed = ""

################################################################################

    def setup_pty( self, _use_pty ):
        self.using_pty = _use_pty

        if _use_pty:
            self.delay = 0.1

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

            self.delay = 0.3

            self.stdout, self.stdin, self.stderr = popen2.popen3( self.sh + " " + self.arg )

            self.outd = self.stdout.fileno()
            self.ind  = self.stdin.fileno()
            self.errd = self.stderr.fileno()

################################################################################

    def write( self, _cmd ):

        os.write( self.ind, _cmd )
        self.last_cmd_executed = _cmd

################################################################################

    def read( self, _buffer ):

        read_data = 0

        while 1:

            r, w, e = select.select( [ self.outd ], [], [], self.delay )

            for file_iter in r:

                read_data = 1

                try:
                    line = os.read( self.outd, 256 )

                except:
                    ##  chances are if the user typed exit
                    ##  and we have an I/O error it's because
                    ##  the process is gone.

                    if re.search( "^\s*exit\s*$", self.last_cmd_executed ):
                        self.cleanup()
                        vim.command( "bd!" )
                        return -1

                print_lines = string.split( line, '\n' )

                ## if more than one entry, splitting on '\n' sometimes returns n + 1 entries
                num_lines = len( print_lines )

                if num_lines > 1:
                    last_line = print_lines[ num_lines - 1 ].strip()

                    if last_line == "":
                        print_lines = print_lines[ :-1 ]

                for line_iter in print_lines:

                    m = re.search("$", line_iter )

                    ##  jump to the position of the last insertion to the buffer
                    ##  if it was a new line it should be 1, if it wasn't
                    ##  terminated by a "\n" it should be the end of the string

                    vim.command( "normal " + str( self.prompt_cursor ) + "|" )

                    cur_line, cur_row = self.get_vim_cursor_pos()

                    if m == None and self.using_pty:

                        ##  just print the line no append if we didn't find 

                        vim.command( 'let g:temp_buffer = "' + line_iter + '"' )
                        vim.command( "let @a = g:temp_buffer" )
                        vim.command( 'normal "ap' )

                    else:
                        if self.using_pty:           # pty leaves trailing 

                            ##  neither of these remove the trailing \n why??
                            # line_iter.strip( )          
                            # re.sub( "\n", "", line_iter )
                            line_iter = line_iter[ :-1 ]   # force it

                        vim.command( 'let g:temp_buffer = "' + line_iter + '"' )
                        vim.command( "let @a = g:temp_buffer" )
                        vim.command( 'normal "ap' )

                        _buffer.append( "" )

                    vim.command( "normal G$" )

                    ##  tuck away location
                    self.prompt_line, self.prompt_cursor = self.get_vim_cursor_pos()

            if r == []:         ##  no more to read
                cur_line, cur_row = self.get_vim_cursor_pos()

                if not self.using_pty:
                    _buffer[ cur_line - 1 ] = self.pipe_prompt 
                
                vim.command( "normal G$" )

                ##  tuck away location now that all printing to buffer is done
                self.prompt_line, self.prompt_cursor = self.get_vim_cursor_pos()

                break

################################################################################

    def execute_cmd( self, _cmd = None ):
        ##  For now only allow executing commands on the "latest" prompt line
        ##  or right affter the printing of a line that needs user input, maybe
        ##  map normal 'o' to just send enter and give a new prompt??

        print ""            ## clears the ex command window

        cur = vim.current.buffer
        cur_line, cur_row = self.get_vim_cursor_pos()

        if _cmd == None:            ##  grab it from buffer
            if cur_line == self.prompt_line and cur_row >= self.prompt_cursor:
                whole_line = cur[ cur_line - 1 ]
                _cmd = whole_line[ self.prompt_cursor: ]

            else:
                return

        ##  check for commands that should be handled differently first

        if re.search("^\s*clear", _cmd ):
            self.write( "" + "\n" )    ##  new prompt

            if clear_all:
                vim.command("normal ggdG")

            ret = self.end_exe_line()

            if ret == -1:
                return

            if not clear_all:
                vim.command("normal zt")

        else:
            self.write( _cmd + "\n" )
            ret = self.end_exe_line()

            if ret == -1:
                return

        vim.command( "startinsert" )

################################################################################

    def end_exe_line ( self ):
        cur = vim.current.buffer
        cur.append( "" )
        vim.command( "normal G$" )

        ret = self.read( cur )

        if ret == -1:
            return -1

        self.check_for_passwd( )

################################################################################

    def check_for_passwd( self ):

        ##  check for password query in previous line
        cur_line, cur_row = self.get_vim_cursor_pos()

        prev_line = cur[ cur_line - 1 ]

        #  could probably just look for the word password
        #  but I want to avoid incorrect matches.

        for regex in self.password_regex:
            if re.search( regex, prev_line ):
                vim.command('let password = inputsecret("Password? ")')
                password = vim.eval( "password" )

                ##  recursive call here...
                self.execute_cmd( password )

################################################################################

    def set_timeout( self ):
        timeout_ok = 0

        while not timeout_ok:
            vim.command('let timeout = input("New timeout ( in seconds, i.e. 1.2 ) ")')
            timeout = float( vim.eval( "timeout" ) )
            
            if timeout >= 0.1:
                print "      --->   New timeout is " + str( timeout ) + " seconds"
                self.delay = timeout
                timeout_ok = 1

################################################################################

    def new_prompt( self ):

        if use_pty:
            self.execute_cmd( "" )        #  just press enter

        else:
            cur[ cur_line - 1 ] = prompt

        vim.command( "normal G$" )
        vim.command( "startinsert" )

################################################################################

    def get_vim_cursor_pos( self ):
        cur_line, cur_row = vim.current.window.cursor
        return cur_line, cur_row + 1

################################################################################
        
    def debug_dump_str_as_hex( self, _str ):
        hex_str = ''

        for x in range( 0, len( _str ) ):
             hex_str = hex_str + hex( ord( _str[x] ) ) + "\n"

        print "raw line (hex) is:"
        print hex_str

################################################################################

    def cleanup( self ):
        if not self.using_pty:
            os.close( self.outd )
            os.close( self.ind )

        os.close( self.errd )       ##  all the same if pty

################################################################################

    def end( self ):
        os.kill( self.pid, signal.SIGKILL )

################################################################################
##                           Main execution code                              ##
################################################################################

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

################################## CUSTOMIZE ###################################

#  Non pty prompt
#
prompt = "%> "

##  Comment these out if you don't have an ansi prompt.
##  may work with multi-line, haven't tried it, only
##  used for pty enabled
#
os.environ['PROMPT'] = r"\u@\h:\w\$"         # sh, bash
os.environ['PS1']    = r"\u@\h:\w\$"         

## any shell program
#
sh        = "/bin/sh"           # sym to /bin/bash on my machine

## supplemental argument to shell
#
arg       = "-i"

## clear shell command behavior
# 0 just scroll for empty screen
# 1 delete contents of buffer
#
clear_all = 0                   
                                
##  Change the <F2> to a different key sequence to taste
##  prompts for the timeouts for read(s)
#
#      set low for local usage, higher for network apps over slower link
#      0.1 sec is the lowest setting
#      over a slow link ( 28.8 ) 5+ seconds works well
#
vim.command( "inoremap <buffer> <F3> <esc>:python vim_shell.set_timeout( )<CR>" )
vim.command( "nnoremap <buffer> <F3> <esc>:python vim_shell.set_timeout( )<CR>" )
vim.command( "cnoremap <buffer> <F3> <esc>:python vim_shell.set_timeout( )<CR>" )

##  Create a new prompt at the bottom of the buffer

vim.command( "inoremap <buffer> <F4>  <esc>:python vim_shell.new_prompt( )<CR>" )
vim.command( "nnoremap <buffer> <F4>  <esc>:python vim_shell.new_prompt( )<CR>" )
vim.command( "cnoremap <buffer> <F4>  <esc>:python vim_shell.new_prompt( )<CR>" )

############################# END CUSTOMIZE ####################################

cur = vim.current.buffer

##  See import statement @ top

if sys.platform == 'win32':
    use_pty   = 0
else:
    use_pty   = 1

vim_shell = vimsh( sh, arg, prompt )
vim_shell.setup_pty( use_pty )

vim.command( "inoremap <buffer> <CR>  <esc>:python vim_shell.execute_cmd( )<CR>" )
vim.command( "au BufWipeout vim_shell <esc>:python vim_shell.end( )<CR>")

vim_shell.read( cur )
cur_line, cur_row = vim_shell.get_vim_cursor_pos()

if use_pty:
    ##  last line *should* be prompt, tuck it away for syntax hilighting
    hi_prompt = cur[ cur_line - 1 ]

else:
    cur[ cur_line - 1 ] = prompt
    vim.command( "normal G$" )

    hi_prompt = prompt          ##  we print non-pty prompt

##  TODO:  Get this to work for *any* prompt
#vim.command('let g:vimsh_prompt="' + hi_prompt + '"' )
#vim.command( 'execute "syntax match VimShPrompt " . "\\"".  escape(g:vimsh_prompt, "~@$") . "\\""' )
#vim.command( 'hi link VimShPrompt LineNr' )

vim.command( "startinsert" )
