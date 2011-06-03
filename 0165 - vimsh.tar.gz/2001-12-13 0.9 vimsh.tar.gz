#!/usr/bin/env python
#
# file:     vimsh.py
# purpose:  allows execution of shell commands in a vim buffer
#
# author:   brian m sturk ( bsturk@nh.ultranet.com )
# created:  12/02/01
# last_mod: 12/13/01
# version:  0.9
#
# usage:         From a script or ex   pyf[ile] vimsh.py
#
#                An useful mapping might be:
#                     nmap \sh           :pyf $PATHTOSCRIPT/vimsh.py<CR>
#
# requirements:  Python enabled vim
#                a platform that supports pty -or- popen
#
# tested on:     Vim 6.0 p93/slackware linux 8.0/Python 2.2b2
#                Vim 6.0 p93/WinNT 4.0/Activestate Python 2.2
#
# license:       Use at your own risk.  I'm not responsible if
#                it hoses your machine.  All I ask is that
#                I'm made aware of changes and my contact
#                info stays in the script.
#
# limitations:   Can only execute line oriented programs, no vim
#                within vim stuff, curses, pagers, etc.  On Windows
#                only non-interactive programs are supported.
# 
# customize:     See section below "CUSTOMIZE"
# 
# notes:
#
#   - Latest version is always available @
#     http://www.nh.ultranet.com/~bsturk/vim.html
#   
#   - Please send bug reports, suggestions, and other email
#     to bsturk@nh.ultranet.com.
#
#   - If you like this script and vim users are allowed to vote on new
#     features in vim again, please put in a vote for vi editing in the ex
#     command window ( not the new command window/buffer ). It's the only
#     feature I would like vim to have. I only ask because if I can get
#     this script to work well lots of people won't be asking for a built
#     in terminal buffer anymore.  : )
#
#   - The timeouts for reading are set low ( < 0.2 sec ) for local filesystem
#     and application use.  If you plan on running telnet/ftp/ssh etc you
#     will need to bump up the timeouts if you have a slower connection.
#     This is not an exact science.  If you're not seeing all of the output
#     or having to hit enter to see output when ftping etc you need to
#     bump the timeout up.  Being conservative won't hurt.
#     See mapping below in CUSTOMIZE.
#
#   - On the Windows platform, running cmd.exe for a shell doesn't need
#     the pipe prompt.  Also, for any shell on Windows, spawned programs,
#     i.e. ftp, telnet, cleartool seem to use buffered output or write
#     directly to the console using the Windows Console API. Unfortunately,
#     this means that without pty functionality for windows, these commands
#     cannot be run in this script.
#
#  known issues/todo:
#  
#  TODO:  new <buffer> uses existing one.  Currently can only be used once,
#         so the buffer needs to be deleted ( bd! ).
#  TODO:  Long commands are unresponsive, i.e. find ~  Figure out a way to
#         print to the file and scroll the buffer as I get input.  May not
#         be possible without returning from python code.
#  TODO:  Handle ( syntax hi ) ansi escape sequences ( colored prompts, LS_COLORS )
#         How can I use regex to determine syntax but hide/remove
#         the escape codes?  Can be folding done within 1 line?
#  TODO:  Add hooks for write/read so scripts can ride on top of this one.
#         i.e. GDB run in terminal buffer, script hooks write read etc parses
#         output, and uses glyph to highlight current line, for example.
#  TODO:  Support vim/python native to 'cygwin', cygwin seems to support select
#  TODO:  How to handle stderr inline with stdout? popen4?
#  TODO:  Add some PS1 type flags for built in prompt.
#  TODO:  Allow splitting options, ala explorer.vim
#  TODO:  Buffer local keymaps are kind of flakey, not sure why
#  TODO:  Break all of this text out into a readme.  :)
#
#  history:
#
#    12/05/01 - 0.2a - Fixed tabwidth, not on prompt message, fixed handling
#                      of user input execution rm -i works, shells now die via
#                      autocommand
#    12/06/01 - 0.3a - Fixed the first line issue, and printed s
#    12/07/01 - 0.4a - Implemented clear, exit, and can now alternate between
#                      using popen3( pipes ) or pty.  This allows platforms
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
#    12/11/01 - 0.8  - Windows support, tested on NT4 w/ ActiveState Python 2.2
#                      Should also work on 2K/XP. Caveat hacktor :)  Non interactive
#                      programs only, dir, findstr, attrib etc work fine, no ftp,
#                      telnet, etc yet. 
#                      Made clear check more explicit, cleartool was triggering it
#                      Fixed the mysterious missing single char issue.
#    12/13/01 - 0.9  - Set buftype=nofile <b.cerrina@wanadoo.fr>
#                      Option to use current buffer if non-modified
#                      Use vim variables for options so this file doesn't need
#                      to be modified to allow customization.
#                      Handle cancelling of input dialogs
#                      Flag unsupported windows console commands
#                      ( partial list see:unsupp_regex )
#
#                      NOTE:
#
#                       All attempts I've made to get interactive programs to
#                       work under Windows have failed.  Without pty/expect like
#                       functionality under Windows, interactive programs cannot
#                       work using this script.  More specifically, programs
#                       that do buffered I/O or read/write directly to the
#                       console.  I'll try any ideas submitted to get it to work,
#                       but for now I'm punting.
#
###############################################################################

import vim, sys, os, string, signal, re

_DEBUG_ = 0

################################################################################
##  If for some reason you'd rather use popenX on a unix variant or
##  the platform doesn't suppport pty, add a check for your platform
##  below also please shoot me an email if you're on a platform besides
##  Windows doesn't support pty so I can add it to this list
################################################################################

try:
    if sys.platform == 'win32':
        import popen2, time, stat
        use_pty   = 0

    else:
        import pty, tty, select
        use_pty   = 1

except ImportError:
    print ( "vimsh: import error" )

################################################################################

def test_and_set( vim_var, default_val ):

    ret = default_val

    vim.command( 'let dummy = exists("' + vim_var + '")')
    exists = vim.eval( "dummy" )

    ##  exists will always be a string representation of the evaluation

    if exists != '0':
        ret = vim.eval( vim_var )

    return ret

############################# customization ###################################
#
#  Don't edit the lines below, instead set the g:<variable> in your
#  .vimrc to the value you would like to use.  For numeric settings
#  *DO NOT* put quotes around them.  The quotes are only needed in
#  this script.
#
#    i.e.   in .vimrc
#
#              let g:vimsh_prompt_override = 1
#              let g:vimsh_prompt_pty      = "%m%#"
#              let g:vimsh_split_open      = 0
#              let g:vimsh_sh              = "/bin/zsh"
#
###############################################################################

#  Non pty prompt, unused on Windows also.  May not be needed
#

pipe_prompt = test_and_set( "g:vimsh_pipe_prompt", "%> " )

#  Allow pty prompt override, useful if you have a ansi prompt
#

prompt_override = int( test_and_set( "g:vimsh_pty_prompt_override", "1" ) )

##  Prompt override, used for pty enabled
#

if use_pty:

    if prompt_override:
        tmp = test_and_set( "g:vimsh_prompt_pty", r"\u@\h:\w\$ " )

        os.environ['prompt'] = tmp
        os.environ['PROMPT'] = tmp
        os.environ['PS1']    = tmp

## shell program and supplemental arg to shell
#

if sys.platform == 'win32':
    sh  = test_and_set( "g:vimsh_sh",     "cmd.exe" )       # NT/Win2k
    arg = test_and_set( "g:vimsh_sh_arg", "" )

else:    
    sh  = test_and_set( "g:vimsh_sh",     "/bin/sh" )       # Unix
    arg = test_and_set( "g:vimsh_sh_arg", "-i" )

## clear shell command behavior
# 0 just scroll for empty screen
# 1 delete contents of buffer
#

clear_all  = test_and_set( "g:vimsh_clear_all", "0" )
                                
## new vimsh window behavior
# 0 use current buffer if not modified
# 1 always split
#

split_open = test_and_set( "g:vimsh_split_open", "1" )

##  Prompts for the timeouts for read( s )
#
#      set low for local usage, higher for network apps over slower link
#      0.1 sec is the lowest setting
#      over a slow link ( 28.8 ) 5+ seconds works well
#

timeout_key = test_and_set( "g:vimsh_timeout_key", "<F3>" )

vim.command( 'inoremap <buffer> ' + timeout_key + ' <esc>:python vim_shell.set_timeout( )<CR>' )
vim.command( 'nnoremap <buffer> ' + timeout_key + ' <esc>:python vim_shell.set_timeout( )<CR>' )
vim.command( 'cnoremap <buffer> ' + timeout_key + ' <esc>:python vim_shell.set_timeout( )<CR>' )
vim.command( 'cnoremap <buffer> ' + timeout_key + ' <esc>:python vim_shell.set_timeout( )<CR>' )

##  Create a new prompt at the bottom of the buffer, useful if stuck

new_prompt_key = test_and_set( "g:vimsh_new_prompt_key", "<F4>" )

vim.command( 'inoremap <buffer> ' + new_prompt_key + '  <esc>:python vim_shell.new_prompt( )<CR>' )
vim.command( 'nnoremap <buffer> ' + new_prompt_key + '  <esc>:python vim_shell.new_prompt( )<CR>' )
vim.command( 'cnoremap <buffer> ' + new_prompt_key + '  <esc>:python vim_shell.new_prompt( )<CR>' )

############################ end customization #################################

################################################################################
##                             class vimsh                                    ##
################################################################################

class vimsh:
    def __init__( self, _sh, _arg, _prompt ):

        self.sh    = _sh
        self.arg   = _arg

        self.pipe_prompt = _prompt
        self.prompt_line, self.prompt_cursor = self.get_vim_cursor_pos( )

        self.password_regex   = [ "^Password:",            ##  su
                                  "Password required" ]    ##  ftp

        ##  can't run interactive programs on the windows platform
        ##  TODO:  Handle the .exe/.com extension here

        self.unsupp_regex = [ r'^\bftp\b',           ##  ftp
                              r'^\btelnet\b',        ##  telnet
                              r'^\bcleartool\b',     ##  cleartool
                              r'^\bssh\b',           ##  ssh
                              r'^\bpython\b' ]       ##  python ( -u too ATM )

        self.last_cmd_executed = "foobar"

################################################################################

    def write( self, _cmd ):

        dbg_print( "write: executing cmd --> " + _cmd )

        os.write( self.ind, _cmd )
        self.last_cmd_executed = _cmd

################################################################################

    def read( self, _buffer ):

        while 1:

            if self.using_pty:
                r, w, e = select.select( [ self.outd ], [], [], self.delay )

            else:
                r = [1,]  ##  unused, fake it out so I don't have to special case

            for file_iter in r:

                lines = ''

                try:
                    if self.using_pty:
                        lines = os.read( self.outd, 32 )

                    else:
                        lines    = self.pipe_read( self.outd )

                        if lines == '':
                            dbg_print( "read: no more data on stdout pipe_read" )

                            ##  On windows no I/O exception just 0 bytes
                            ##  on the pipe, bug egg for apps that use
                            ##  'exit' but should return to shell. TODO:
                            ##  Fix me if/when interactive programs work.

                            if re.search( "^\s*exit\s*$", self.last_cmd_executed ):
                                dbg_print( "      and last cmd was exit" )

                                self.cleanup( )
                                vim.command ( "bd!" )
                                return -1

                            else:
                                r = [];          ##  signal end of data to read
                                break;           ##  out of for loop

                except:

                    ##  Chances are if the user typed exit and there's
                    ##  an I/O error it's because the process is gone.

                    if re.search( "^\s*exit\s*$", self.last_cmd_executed ):
                        self.cleanup( )
                        vim.command ( "bd!" )
                        return -1

                    else:
                        dbg_print ( "Unexpected error:" + str( sys.exc_info()[0] ) )

                lines = self.process_read( lines )

                self.print_lines( lines, _buffer )

            if r == []:

                self.end_read( _buffer )
                break

################################################################################

    def process_read( self, _lines ):

        dbg_print( "read: raw lines read from stdout:" ); dbg_print( _lines )

        print_lines = string.split( _lines, '\n' )

        ##  on windows cmd is "echoed" and output sometimes has leading empty line

        if sys.platform == 'win32':
            m = re.search( re.escape( self.last_cmd_executed.strip() ), print_lines[ 0 ] )

            if m != None or print_lines[ 0 ] == "":
                dbg_print( "read: win32, removing leading blank line" )
                print_lines = print_lines[ 1: ]

        num_lines = len( print_lines )

        ##  split on '\n' sometimes returns n + 1 entries

        if num_lines > 1:
            last_line = print_lines[ num_lines - 1 ].strip( )

            if last_line == "":
                print_lines = print_lines[ :-1 ]

        errors = self.chk_stderr( )

        if errors:
            dbg_print( "read: prepending stderr --> " )
            print_lines = errors + print_lines

        return print_lines

################################################################################

    def print_lines( self, _lines, _buffer ):

        num_lines = len( _lines )

        dbg_print( "read: number of lines to print--> " + `num_lines` )

        for line_iter in _lines:

            dbg_print( "read: current line is --> %s" %  line_iter )

            m = re.search( "$", line_iter )

            ##  jump to the position of the last insertion to the buffer
            ##  if it was a new line it should be 1, if it wasn't
            ##  terminated by a '\n' it should be the end of the string

            vim.command( "normal " + str( self.prompt_cursor ) + "|" )

            cur_line, cur_row = self.get_vim_cursor_pos( )
            dbg_print( "read: after jumping to end of last cmd: line %d row %d" % ( cur_line, cur_row ) )

            if self.using_pty and m:          # pty leaves trailing 
            
                dbg_print( "read: pty removing trailing ^M" )

                #  neither of these remove the trailing \n why??
                #   line_iter.strip( )          
                #   re.sub( "\n", "", line_iter )

                line_iter = line_iter[ :-1 ]   # force it

            dbg_print( "read: pasting " + line_iter + " to current line" )
            _buffer[ cur_line - 1 ] += line_iter

            ##  if there's a '\n' or using pipes and it's not the last line

            if m != None or not self.using_pty:
                dbg_print( "read: appending new line since ^M or not using pty" )
                _buffer.append( "" )

            vim.command( "normal G$" )

            self.prompt_line, self.prompt_cursor = self.get_vim_cursor_pos( )
            dbg_print( "read: saving cursor location: line %d row %d " % ( self.prompt_line, self.prompt_cursor ) )
                
################################################################################

    def end_read( self, _buffer ):

        cur_line, cur_row = self.get_vim_cursor_pos( )

        if not self.using_pty:

            ##  Windows prints out prompt, pipes on Linux do not

            if sys.platform != 'win32':
                dbg_print( "read: printing our pipe prompt" )
                _buffer[ cur_line - 1 ] = self.pipe_prompt 

            else:

                ##  remove last line for last read, TODO: any better way to do this?

                vim.command( 'normal dd' )
        
        vim.command( "normal G$" )

        ##  tuck away location all data read is in buffer

        self.prompt_line, self.prompt_cursor = self.get_vim_cursor_pos( )

################################################################################

    def execute_cmd( self, _cmd = None ):

        print ""            ## clears the ex command window

        cur = vim.current.buffer
        cur_line, cur_row = self.get_vim_cursor_pos( )

        if _cmd == None:            ##  grab it current line in buffer
            if cur_line == self.prompt_line and cur_row >= self.prompt_cursor:
                whole_line = cur[ cur_line - 1 ]
                _cmd = whole_line[ self.prompt_cursor: ]

            else:
                return

        ##  check for commands that should be handled differently first

        if re.search( "^\s*clear\s+", _cmd ):
            dbg_print ( "matched clear" )

            self.write( "" + "\n" )    ##  new prompt

            if clear_all:
                vim.command( "normal ggdG" )

            ret = self.end_exe_line( )

            if ret == -1:
                return

            if not clear_all:
                vim.command( "normal zt" )

        else:

            ##  first check for interactive commands under windows

            if sys.platform == 'win32':

                dbg_print( "execute_cmd: checking for unsupported windows cmds" )

                for regex in self.unsupp_regex:
                    m = re.search( regex, _cmd )

                    if m:
                        dbg_print( "execute_cmd: found a match" )

                        vim.command( 'let continue = input( "The console version of ' + m.group(0) + ' is unsupported on Windows.  Continue anyway? y/n " )' )
                        exe_it = vim.eval( "continue" )

                        print ""            ## clears the ex command window

                        if( string.upper( exe_it ) != 'Y' ):
                            dbg_print( "execute_cmd: unsupported and not-executing" )
                            return


            self.write( _cmd + "\n" )
            ret = self.end_exe_line( )

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

    ##  Hackaround since Windows doesn't support select() except for sockets.

    def pipe_read( self, pipe ):

        time.sleep( self.delay )

        count = 0
        count = os.fstat( pipe )[stat.ST_SIZE]
            
        data = ''
        while ( count > 0 ):
            data += os.read( pipe, 1 )
            count = os.fstat( pipe )[stat.ST_SIZE]

        return data

################################################################################

    def chk_stderr( self ):

        errors = ''

        if sys.platform == 'win32':

            err_txt  = self.pipe_read( self.errd )
            errors   = string.split( err_txt, '\n' )

            num_lines = len( errors )
            dbg_print( "chk_stderr: number of error lines is " + `num_lines` )

            last_line = errors[ num_lines - 1 ].strip( )

            if last_line == "":
                dbg_print( "chk_stderr: removing last line, it's empty" )
                errors = errors[ :-1 ]

        return errors

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

                os.execv( self.sh, [ self.sh, self.arg ] )
        else:

            ##  use pipes. not as reliable/nice. works OK but with limitations.
            ##  Needed for Windows support.

            self.delay = 0.1

            self.stdout, self.stdin, self.stderr = popen2.popen3( self.sh + " " + self.arg, bufsize=-1  )

            #  no better :(

            #import win32pipe
            #self.stdin, self.stdout, self.stderr = win32pipe.popen3( self.sh + " " + self.arg )

            self.outd = self.stdout.fileno( )
            self.ind  = self.stdin.fileno ( )
            self.errd = self.stderr.fileno( )

################################################################################

    def check_for_passwd( self ):

        ##  check for password query in previous line

        cur_line, cur_row = self.get_vim_cursor_pos( )

        prev_line = cur[ cur_line - 1 ]

        #  could probably just look for the word password
        #  but I want to avoid incorrect matches.

        for regex in self.password_regex:
            if re.search( regex, prev_line ):

                try:
                    vim.command( 'let password = inputsecret( "Password? " )' )

                except KeyboardInterrupt:
                    return

                    password = vim.eval( "password" )

                    ##  recursive call here...
                    self.execute_cmd( password )

################################################################################

    def set_timeout( self ):

        timeout_ok = 0

        while not timeout_ok:

            try:
                vim.command( 'let timeout = input( "New timeout ( in seconds, i.e. 1.2 ) " )' )

            except KeyboardInterrupt:
                return

            timeout = vim.eval( "timeout" )

            if timeout == "":               ##  usr cancelled dialog
                timeout_ok = 1

            else:
                timeout = float( timeout )
            
                if timeout >= 0.1:
                    print "      --->   New timeout is " + str( timeout ) + " seconds"
                    self.delay = timeout
                    timeout_ok = 1

################################################################################

    def new_prompt( self ):

        if use_pty:
            self.execute_cmd( "" )        #  just press enter

        else:
            cur[ cur_line - 1 ] = self.pipe_prompt

        vim.command( "normal G$" )
        vim.command( "startinsert" )

################################################################################

    def get_vim_cursor_pos( self ):

        cur_line, cur_row = vim.current.window.cursor
        return cur_line, cur_row + 1

################################################################################

    def cleanup( self ):

        dbg_print( "cleaning up" )

        try:
            if not self.using_pty:
                os.close( self.outd )
                os.close( self.ind )

            os.close( self.errd )       ##  all the same if pty

            os.kill( self.pid, signal.SIGKILL )

        except:
            dbg_print( "exception, process probably already killed" )

################################################################################
##                           Helper functions                                 ##
################################################################################
        
def dbg_dump_str_as_hex( _str ):

    if _DEBUG_:
        hex_str = ''

        for x in range( 0, len( _str ) ):
             hex_str = hex_str + hex( ord( _str[x] ) ) + "\n"

        print "raw line ( hex ) is:"
        print hex_str

################################################################################
        
def dbg_print( _str ):

    if _DEBUG_:
        print _str

################################################################################
##                           Main execution code                              ##
################################################################################

try:
    ##  TODO:  Need to come up with a way to generate these buffers so
    ##         more than one can be opened

    if split_open:
        vim.command( "new vim_shell" )
    else:
        vim.command( "edit vim_shell" )

    vim.command( "setlocal buftype=nofile" )
    vim.command( "setlocal tabstop=8" )
    vim.command( "setlocal modifiable" )
    vim.command( "setlocal noswapfile" )
    vim.command( "setlocal nowrap" )

except:
    print vim.error

cur = vim.current.buffer

vim_shell = vimsh( sh, arg, pipe_prompt )
vim_shell.setup_pty( use_pty )

vim.command( "inoremap <buffer> <CR>  <esc>:python vim_shell.execute_cmd( )<CR>" )
vim.command( "au BufWipeout vim_shell <esc>:python vim_shell.cleanup( )<CR>" )

vim_shell.read( cur )
cur_line, cur_row = vim_shell.get_vim_cursor_pos( )

if use_pty or sys.platform == 'win32':

    ##  last line *should* be prompt, tuck it away for syntax hilighting
    hi_prompt = cur[ cur_line - 1 ]

else:
    hi_prompt = vimsh_prompt          ##  print non-pty prompt on platforms that don't
    vim_shell.new_prompt()

vim.command( "startinsert" )

##  TODO:  Get this to work for *any* prompt
#vim.command( 'let g:vimsh_prompt="' + hi_prompt + '"' )
#vim.command( 'execute "syntax match VimShPrompt " . "\\"".  escape( g:vimsh_prompt, "~@$" ) . "\\""' )
#vim.command( 'hi link VimShPrompt LineNr' )
