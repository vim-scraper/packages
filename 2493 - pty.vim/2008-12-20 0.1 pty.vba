" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/pty.txt	[[[1
91
*pty.txt*

pty.vim 
Remote terminal support for vim thanks to python.


You need VIM with python support and urxvt (rxvt-unicode) to use this script.
<http://software.schmorp.de/pkg/rxvt-unicode.html>

Furthermore this script only provides python functions, so some knowledge about
this (very fine) language is usefull, but not required to benefit from it.

Calling the python function rxvtSend('text','cmd') the first time will spawn
a subprocesses given by the shell command 'cmd' in a new terminal window and
pass 'text' to it. Subsequent calls with the same 'cmd' argument will send further
text to the this subprocess. If you close the terminal window, the child
process stays alive and the next call of 'rxvtSend' opens a new terminal and
connects to it again. If you want to kill the child, call 'rxvtKill()'. 


Python Functions:

rxvtSend(text, cmd, newline=True, erase=True, reopen=False, chdir=False, rxvtOptions=""):

		Send 'text' to the terminals child process 'cmd'. If no such process exists, 
		a new child process will be spawned and a possible existing one will
		be killed.

		Optional arguments:
		newline:		Append a newline after 'text' (default true)
		erase:			Erase the current input line before sending 'text'.
		reopen:			Close running terminal and reopen.

		The following arguments	take only effect, if a new process will
		be spawned:

		chdir:			Run 'cmd' in the directory of the currently edited file.
		rxvtOptions:	To be passed as commandline arguments to 'urxvt'.
						Usefull values are    '-geometry ...' or '-hold'.

rxvtClose()
		Close the terminal window (child stays alive).

rxvtKill()
		Kill the child process, terminal terminates on its own.								

getreg(arg)
		|getreg| translated in python.

expand(arg)
		|expand| translated in python. 			 					  		 

getline(arg)
		|getline| translated in python.


Examples: 

      :python rxvtSend('import os', 'python')
      :python rxvtSend('print "Hello " + os.getenv("USER")', 'python')

      :python rxvtSend('execfile('+ expand('%:p') +')', 'python')
      In other words:
      :python rxvtSend('execfile(%s)' %expand('%:p'), 'python')

If you like it fancy

      :python rxvtSend('print "Hello" + os.getenv("USER")','python', reopen=True, rxvtOptions='-rv -bg red')

Example snippet from my ~/.vimrc:

  au FileType python 
      \ map <buffer><F12> :update <bar> :python rxvtSend('execfile('+ expand('%:p') +')', 'python')<CR>
  au FileType haskell
      \ map <buffer><F12> :update <bar> :python rxvtSend(':l '+ expand('%:p'), 'ghci')<CR>

  au FileType haskell
      \ map <buffer><F11> :python rxvtSend(':t '+ expand('<cword>'), 'ghci')<CR>

  au FileType haskell
      \ vmap <buffer><F11> :python rxvtSend(':t '+ getreg('*'), 'ghci')<CR>

Short info for python agnostics:

	Be aware that the arguments of the functions above are either quoted
	strings or boolean values: True or False (capitalized and unquoted). There
	is no difference between single and double quotes (as long as they match:-).
	String concatenation is done by '+'.


"vim:tw=78:ts=8:noet:ft=help:ff=unix:
plugin/pty.vim	[[[1
325
" pty.vim 
" Remote terminal support for vim thanks to python.
"
" Last Change:	Sa 20.Dez 2008
" Maintainer:	Johann Giwer johanngiwer@web.de
" License:	    This file is placed in the public domain.
" Version:      0.1
"
" You need VIM with python support and urxvt (rxvt-unicode) to use this script.
" <http://software.schmorp.de/pkg/rxvt-unicode.html>
"
" Furthermore this script only provides python functions, so some knowledge about
" this (very fine) language is usefull, but not required to benefit from it.
"
" Calling the python function rxvtSend('text','cmd') the first time will spawn
" a subprocesses given by the shell command 'cmd' in a new terminal window and
" pass 'text' to it. Subsequent calls with the same 'cmd' argument will send further
" text to the this subprocess. If you close the terminal window, the child
" process stays alive and the next call of 'rxvtSend' opens a new terminal and
" connects to it again. If you want to kill the child, call 'rxvtKill()'. 
"
"
" Python Functions:
"
" rxvtSend(text, cmd, newline=True, erase=True, reopen=False, chdir=False, rxvtOptions=""):
"
"		Send 'text' to the terminals child process 'cmd'. If no such process exists, 
"		a new child process will be spawned and a possible existing one will
"		be killed.
"
"		Optional arguments:
"		newline:		Append a newline after 'text' (default true)
"		erase:			Erase the current input line before sending 'text'.
"		reopen:			Close running terminal and reopen.
"
"		The following arguments	take only effect, if a new process will
"		be spawned:
"
"		chdir:			Run 'cmd' in the directory of the currently edited file.
"		rxvtOptions:	To be passed as commandline arguments to 'urxvt'.
"						Usefull values are    '-geometry ...' or '-hold'.
"
" rxvtClose()
" 		Close the terminal window (child stays alive).
"
" rxvtKill()
" 		Kill the child process, terminal terminates on its own.								
"
" getreg(arg)
"		|getreg| translated in python.
"
" expand(arg)
"		|expand| translated in python. 			 					  		 
"
" getline(arg)
"		|getline| translated in python.
"
"
" Examples: 
"
"       :python rxvtSend('import os', 'python')
"       :python rxvtSend('print "Hello " + os.getenv("USER")', 'python')
"
"       :python rxvtSend('execfile('+ expand('%:p') +')', 'python')
"       In other words:
"       :python rxvtSend('execfile(%s)' %expand('%:p'), 'python')
"
" If you like it fancy
"
"       :python rxvtSend('print "Hello" + os.getenv("USER")','python', reopen=True, rxvtOptions='-rv -bg red')
"
" Example snippet from my ~/.vimrc:
"
"   au FileType python 
"       \ map <buffer><F12> :update <bar> :python rxvtSend('execfile('+ expand('%:p') +')', 'python')<CR>
"   au FileType haskell
"       \ map <buffer><F12> :update <bar> :python rxvtSend(':l '+ expand('%:p'), 'ghci')<CR>
"
"   au FileType haskell
"       \ map <buffer><F11> :python rxvtSend(':t '+ expand('<cword>'), 'ghci')<CR>
"
"   au FileType haskell
"       \ vmap <buffer><F11> :python rxvtSend(':t '+ getreg('*'), 'ghci')<CR>
"
" Short info for python agnostics:
"
" 	Be aware that the arguments of the functions above are either quoted
" 	strings or boolean values: True or False (capitalized and unquoted). There
" 	is no difference between single and double quotes (as long as they match:-).
" 	String concatenation is done by '+'.
"

if !has("python")
    echoerr "You need python support in VIM to use this script"
    finish
endif

:python <<EOP

import os,vim,atexit

# Helper functions

def which(cmd, path=[]):
    """Like the shell command 'which', search @cmd along @path (a list of directories)."""

    if os.access(cmd, os.X_OK):
        return os.path.join( os.getcwd(), cmd)
    else:
        path = path or os.getenv("PATH").split(os.pathsep)
        path = filter( lambda dir: os.access(os.path.join(dir,cmd),os.X_OK), path)
        if path:
            return os.path.join(path[0],cmd)
        else:
            return None
    
def format(*args):
    """Format @args to a string suitable to pass to vim.
    Return vim string (see :help expr-string)"""
    
    argstring = ''
    for arg in args:
        argtype = type( arg )
        if arg == None:
            pass
        elif argtype == str:
            argstring += '"%s" ,' %arg.replace("\'","'").replace('\\','\\\\').replace('"','\\"')
        elif argtype in (int, bool):
            argstring += '%d ,' %arg
        elif argtype == float:
            argstring += '%f ,' %arg
        elif argtype in (list,tuple):
            argstring += '[%s] ,' %format(*arg)
        elif argtype == dict:	##
            argstring += '{ '
            for i,j in arg.items():
                argstring += '%s : %s,' %( format(i),format(j))
            argstring = argstring[:-1] + '} ,'
    return argstring[:-1]

def format_literal(*args):
    """Format @args to a string suitable to pass to vim.
    Return vim literal string (see :help literal-string)"""
    
    args = filter( lambda x : x != None, args )
    args = map( lambda x :x.replace("'","@quote@"), args)
    return str(args)[1:-1].replace("@quote@","''") 

def function(name, *args):
    """Evaluate the vim function @name with arguments @args
    Allows us to define functions with empty keyword arguments"""

    if args:
        return vim.eval( '%s(%s)' %(name, format(*args)))
    else:
        return vim.eval( '%s()' %name )

# Translations of vim function.

def expand( expr):
    """See :help expand()""" 
    
    return function( "expand", expr)

def getline( expr):
    """See :help getline()""" 
    
    return function( "getline", expr)

def getreg( regname):
    """See :help getreg()""" 
    
    return function( "getreg", regname)


# Rxvt related


rxvtTerminalPid = None
rxvtChildPid = None
rxvtCmd = None
rxvtFd = None

def rxvtKill():
    """Kill the child process. Terminal will close on its own."""

    global rxvtChildPid
    if not rxvtChildPid: return 
    try: 
        os.kill (rxvtChildPid, 15)
        os.waitpid( rxvtChildPid, os.WNOHANG)
        rxvtChildPid = None
    except : pass

def rxvtClose():
    """Close the terminal. Child process stays alive."""
    
    global rxvtTerminalPid
    if not rxvtTerminalPid: return 
    try: 
        os.kill (rxvtTerminalPid, 15)
        os.waitpid( rxvtTerminalPid, os.WNOHANG)
        rxvtTerminalPid = None
    except : pass

def _rxvtSpawn(cmd, fd, rxvtOptions):
    
    global rxvtTerminalPid
    rxvtTerminalPid = os.spawnvp(os.P_NOWAIT,"urxvt", 
                                            ["urxvt", "-title", cmd, "-pty-fd", str(fd)] +
                                            rxvtOptions.split())

def _rxvtTerminal_isalive():
    """Test wether child process is alive"""
    
    global rxvtTerminalPid
    if not rxvtTerminalPid: return
    try:
        pid, status = os.waitpid( rxvtTerminalPid, os.WNOHANG)
        if pid == 0:
            return True
    except:
        return False

def _rxvtIsalive(cmd=None):
    """Test wether child process is alive"""

    global rxvtChildPid
    if not rxvtChildPid: return
    try:
        pid, status = os.waitpid( rxvtChildPid, os.WNOHANG)
        if pid == 0:
            return True
    except:
        return False

def rxvtOpen(cmd, rxvtOptions="", chdir=True):
    """Open a terminal window and run the command @cmd. This works only with
    urxvt (rxvt-unicode) with pty support.  If chdir is set, cmd will start in
    the directory of the current edited file.  Usefull values for @rxvtOptions are
    '-geometry ...' or '-hold' """
    
    global rxvtFd
    global rxvtCmd
    global rxvtChildPid

    if not (cmd or rxvtCmd):
        print "No cmd given"
        return None
    elif ((cmd == rxvtCmd ) or (not cmd )) and _rxvtIsalive():
        cmd = rxvtCmd
        _rxvtSpawn(cmd, rxvtFd, rxvtOptions)

    else:
        rxvtKill()
        if not cmd:
            cmd = rxvtCmd
        else:
            rxvtCmd = cmd

        args = cmd.split()
        if not which( args[0] ):
            print "Can't find %s in $PATH\n" %args[0]
            print "$PATH=" + os.getenv("PATH")
            return
        elif not which("urxvt"):
            print "Can't find `urxvt' in $PATH\n" 
            print "$PATH=" + os.getenv("PATH")
            return

        rxvtChildPid, rxvtFd = os.forkpty()
        
        if rxvtChildPid == 0:
            try:
                if chdir:
                    os.chdir( expand('%:p:h'))
                os.execvp(args[0], args)
            except OSError, e:
                print e
                os._exit(1)
        else:
            _rxvtSpawn(cmd, rxvtFd, rxvtOptions)


def rxvtSend(arg, cmd, newline=True, erase=True, reopen=False, **kwargs):
    """Send @arg to @cmd ( a shell command ). If there is allready a running
    subprocess according to this command, it will be used, to send @text to.
    Otherwise the process will be killed an a new one will be spawned.
    Flags recognized:
    newline: Append a newline after @arg (default true)
    erase: Erase the current input line before sending @arg (default true)
    reopen: Close running terminal and reopen (default false)
    @kwargs are passed to rxvtOpen, if a new command is spawned, so you
    can set chdir and rxvtOptions."""

    global rxvtCmd
    global rxvtFd

    if cmd and cmd != rxvtCmd:
        reopen = True
    if not _rxvtTerminal_isalive():
        reopen = True
    if reopen :
        rxvtClose()
        rxvtOpen(cmd,**kwargs)

    if not rxvtFd :
        return

    if erase:
        os.write( rxvtFd, " " )
    os.write( rxvtFd, str(arg))
    if newline:
        os.write( rxvtFd, "\n" )

def rxvtSendRegion( cmd, newline=True, erase=True, reopen=False, **kwargs):
    """Send the current region (default: current line) to @cmd.  For flags see rxvtSend()"""
    
    range = vim.current.range
    arg = "\n".join( vim.current.buffer[range.start:range.end+1] )
    rxvtSend( arg, cmd,newline=newline, erase=erase,reopen=reopen, **kwargs)

atexit.register( rxvtKill )
EOP
"vim:tw=78:ts=8:noet:ft=vim:ff=unix:
