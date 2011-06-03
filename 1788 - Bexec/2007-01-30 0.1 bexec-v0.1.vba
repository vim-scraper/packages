" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/bexec.vim
363
" BExec
" -----
" Use the shebang (#!) or filetype to execute a script in the current buffer,
" capture its output and put it in a seperate buffer.
"
" Last Change:	2007 Jan 21
" Maintainer:	Ferry Boender <f DOT boender AT electricmonk DOT nl>
" License:	    This file is placed in the public domain.
" Usage:        To use this script:
"               
"               - Place in your .vim/plugin/ dir.
"                OR
"               - Source it (:source bexec.vim)
"
"               Run :Bexec
"                OR
"               Run :BexecVisual  (in visual select mode)
"                OR
"               Press <F5>
"
"               For more usage, see bexec.txt.
"
" Settings:     See bexec.txt for settings.
"
" Todo:         * Settings:
"                   - Change to the buffer's dir and run. (default: curdir)
"                   - 'Auto' save so you don't have to save beforehand. (default:off)
"               * Add menu entry
"               * Add menu and toolbar
"               * PHP Execution is hard with Visual mode.
"               * Check if buffer has been written yet.
"               * Allow options to the interpreter.
"               * Allow feeding into STDIN.
"               * Horizontal column pos gets lost when running in visual
"                 select mode.
" Changelog:    v0.1 (Jan 16, 2007)
"                 * Initial version.
"                 * Removed setlocal bufhidden=delete so buffer settings don't
"                   get undone. This fixes the bug where vim asks to save the
"                   buffer.
"                 * Added silent! at interpreter execution to prevent non-zero
"                   return codes from showing up.
"                 * Refactoring.
"                 * Added various settings (bexec_args, bexec_splitdir,
"                   bexec_argsbuf)
"                 * Added the ability to pass params to scripts.
"                 * Better error checking.
"                 * Visual mode selected text execute only added.
"                 * Better scrolling of output buffer, including settings.
"                 * Delimiter line between script output.
"                 * Documentation.
"                 * Parameters to the shebang interpreter are now ignored in
"                   the executable() check.

"if exists("loaded_bexec")
"  finish
"endif
"let loaded_bexec = 1

"
" Define some mappings to BExec
"
if !hasmapto('Bexec')
    nmap <silent> <unique> <F5> :call Bexec()<CR>
endif
if !hasmapto('BexecVisual')
    vmap <silent> <unique> <F5> :call BexecVisual()<CR>
endif

"
" Let's do some settings too.
"
if !exists("bexec_args")
    " Argument string to feed to script when executing
    let bexec_args = ""
endif
if !exists("bexec_splitdir")
    " Direction in which to split the current window for the output buffer.
    let bexec_splitdir = "hor" " hor|ver
endif
if !exists("bexec_argsbuf")
    " Buffer number to be used as argument string to feed to script when
    " executing. Only first line is used. FIXME: more lines?
    let bexec_argsbuf = ""
endif
if !exists("bexec_outputmode")
    " Replace or append output of script in output buffer?
    let bexec_outputmode = "replace" " replace|append
endif
if !exists("bexec_rehighlight")
    " Re-highlight selected text after executing BexecVisual?
    let bexec_rehighlight = 0
endif
if !exists("bexec_outputscroll")
    " Scroll output buffer after appending output of script?
    let bexec_outputscroll = 1
endif

"
" Make the BExec call known to Vim
"
com! -nargs=* Bexec       call Bexec(<f-args>)
com! -nargs=* BexecVisual call BexecVisual(<f-args>)

"
" List of interpreters BExec knows about.
" FIXME: Is there a better way of doing this?
"
let s:interpreters = { }
let s:interpreters["php"]    = "/usr/bin/env php"
let s:interpreters["python"] = "/usr/bin/env python"
let s:interpreters["sh"]     = "/usr/bin/env sh"
let s:interpreters["perl"]   = "/usr/bin/env perl"

"
" Get the first line of the current buffer and check if it's a shebang line
" (shebang is an indication of which interpreter should be used to run a
" script. The shebang should be on the first line and should be in the form of
" #!/path/to/interpreter). 
" 
" Returns the path to the interpreter or -1 if the file doesn't have a
" shebang.
"
function! <SID>GetInterpreterFromShebang()
    let l:shebangLine = getline(1)
    
    if shebangLine[0:1] == "#!"
        return shebangLine[2:]
    else
        return -1
    endif 
endfunction

"
" Try to guess which interpreter should run this script by using the script
" filetype. Used when the shebang can't be found.
"
" Returns the guessed interpreter or -1 if it couldn't be guessed.
"
function! <SID>GetInterpreterFromFiletype()
    let l:type = &filetype
    return get(s:interpreters, l:type, -1)
endfunction

"
" Get the interpreter that should be used for the current buffer. Either from
" the shebang or by guessing it.
"
function! <SID>GetInterpreter()
    let l:interpreter = <SID>GetInterpreterFromShebang()
    if l:interpreter == -1
        let l:interpreter = <SID>GetInterpreterFromFiletype()
    endif
    if !executable(split(l:interpreter)[0])
        let l:interpreter = -2
    endif
    return l:interpreter
endfunction

"
" Find the arguments that should be passed to a script and build a string from
" them. Arguments can come from function arguments, a setting or a buffer.
" Arguments are determined in the previous mentioned order (setting overrides
" buffer, etc).
"
function! <SID>GetArgumentString(...)
    if a:0 > 0 && a:1['0'] > 0
        " Use arguments passed to this function as a dict (args from another
        " function)
        let l:args = join(a:1['000'], " ")
    elseif exists("g:bexec_args") && g:bexec_args != ""
        " Use arguments from the bexec_args setting.
        let l:args = g:bexec_args
    elseif exists("g:bexec_argsbuf") && g:bexec_argsbuf != ""
        " Use arguments from a seperate buffer
        exec g:bexec_argsbuf.' wincmd w'
        let l:args = getline(1)
        exec 'wincmd p'
    else
        " No arguments
        let l:args = ""
    endif

    return l:args
endfunction

"
" Find a window that has bufName open. If no window is found, one will will be
" created by spliting. If the buffer doesn't exist, it will be created too.
"
" Returns the buffer number for the output buffer.
"
function! <SID>FindOrCreateOutWin(bufName)
    
    let l:outWinNr = bufwinnr(a:bufName)
    let l:outBufNr = bufnr(a:bufName)

    " Find or create a window for the bufName
    if l:outWinNr == -1
        " Create a new window
        let l:splitCmdMap = {"ver":"vsp", "hor":"vp"}
        let l:splitCmd = l:splitCmdMap[g:bexec_splitdir]
        if g:bexec_splitdir == "ver"
            let l:splitCmd = "vsp"
        else
            let l:splitCmd = "sp"
        endif

        exec l:splitCmd

        let l:outWinNr = bufwinnr("%")
        if l:outBufNr != -1
            " The buffer already exists. Open it here.
            exec 'b'.l:outBufNr
        endif
        " Jump back to the previous window the user was editing in.
        exec 'wincmd p'
    endif

    " Find the buffer number or create one for bufName
    if l:outBufNr == -1
        " Jump to the output window
        exec l:outWinNr.' wincmd w'
        " Open a new output buffer
        exec 'e '.a:bufName
        setlocal noswapfile
        setlocal buftype=nofile
        setlocal wrap
        let l:outBufNr = bufnr("%")
        " Jump back to the previous window the user was editing in.
        exec 'wincmd p'
    endif
        
    return l:outBufNr
endfunction

"
" Run interpreter and replace the contents of bufName with the output of the command.
"
function! <SID>RunAndRedirectOut(interpreter, curFile, args, bufName)
    " Change to the output buffer window
    let l:outWinNr = bufwinnr(a:bufName)
    exec l:outWinNr.' wincmd w'

    " Execute the command and append the output
    if g:bexec_outputmode == "append"
        let l:runCmd = "r!"
    elseif g:bexec_outputmode == "replace"
        let l:runCmd = "%!"
    else
        echoerr "Unknown output mode in bexec_outputmode setting"
    endif

    " Build the final (vim) command we're gonna run 
    let l:runCmd = l:runCmd." ".a:interpreter." '".a:curFile."' ".a:args

    " Add a separator line to distinguish between different script output
    if g:bexec_outputmode == "append"
        call append("$", repeat('-', winwidth(0)))
    endif

    " Run it
    norm G
    let l:curpos = getpos(".") " Save cursor position for scrolling later on
    silent! exec l:runCmd

    " Scroll the output buffer to accommodate for user settings
    if g:bexec_outputscroll == 1
        " Scroll to the end of the output buffer
        norm G
    else
        " Scroll to begin of current output so that first line of the output
        " is at the top of the window.
        if g:bexec_outputmode == "replace"
            norm gg
        elseif g:bexec_outputmode == "append"
            let l:curpos[1] = l:curpos[1] + 1
            call setpos(".", l:curpos)
            norm zt
        endif
    endif

    " Jump back to the previous window the user was editing in.
    exec 'wincmd p'
endfunction

"
" Get the name of the current buffer or, if the buffer hasn't been saved yet, 
" copy the buffer contents to a temp file and return that.
" 
function! <SID>GetScriptFilename(...)
    let l:curFilename = expand("%:p")

    if a:0 == 1
        " Save the visual selection to a temp file
        if !exists("s:tempfile")
            let s:tempfile = tempname() 
        endif
        let l:filename = s:tempfile
        exec writefile(getline(a:1[0], a:1[1]), l:filename)
    elseif l:curFilename == ""
        " Save the unsaved buffer to a temp file
        " FIXME: This check is not sufficient! Do this: Edit. :w foo.sh Run :w
        "        bar.sh. Edit. Run. It will execute bar.sh while the filename is
        "        foo.sh. We want to check if the buffer has been written (no
        "        '+')
        if !exists("s:tempfile")
            let s:tempfile = tempname() 
        endif
        let l:filename = s:tempfile
        exec writefile(getline(0, "$"), l:filename)
    else
        " Use the current file
        let l:filename = l:curFilename
    endif

    return l:filename
endfunction

"
" Main Bexec function. 
"
function! <SID>BexecDo(...)
    let l:curpos=getpos(".")
    let l:interpreter = <SID>GetInterpreter()

    " If no interpreter was found
    if l:interpreter == -1
        echo "Can't find an interpreter for buffer."
    elseif l:interpreter == -2
        echo "Invalid interpreter."
    else
        if a:0 == 1
            " Run visually selected text
            let l:scriptFilename = <SID>GetScriptFilename(a:1)
        else
            " Run entire buffer
            let l:scriptFilename = <SID>GetScriptFilename()
        endif
        let l:argString = <SID>GetArgumentString(a:)
        let l:outBuf = <SID>FindOrCreateOutWin("-BExec_output-")
        call <SID>RunAndRedirectOut(l:interpreter, l:scriptFilename, l:argString, l:outBuf)
    endif
    call setpos(".", l:curpos)
endfunction

"
" Wrapper function for visually selected text execution.
"
function! BexecVisual(...) range
    call <SID>BexecDo([a:firstline, a:lastline])
    if g:bexec_rehighlight == 1
        " Rehighlight selection
        norm gv
    endif
endfunction

"
" Wrapper function for normal buffer execution.
"
function! Bexec(...) 
    call <SID>BexecDo()
endfunction
doc/bexec.txt
173
*bexec.txt*      Buffer execute and grab output      Last Change: 27 Jan 2007

Buffer Execute (v0.1)                                *buffer-execute* *bexec*

The Bexec plugin allows the user to execute the current buffer if it
contains a script with a shebang (#!/path/to/interpreter) on the first line.
The output of the script will be grabbed and displayed in a separate buffer.
It can be used to execute, for instance, Shell, PHP, PERL, Python or SQL
scripts from within Vim.

|bexec-usage|       Usage
|bexec-install|     Installation
|bexec-settings|    Settings
|bexec-tips|        Tips

This plugin is only available if 'compatible' is not set.

=============================================================================
USAGE                                                           *bexec-usage*

To execute the script in the current buffer, use: >
 :Bexec
To execute the currently highlighted text using the current buffer's
shebang, use: >
 :BexecVisual

If the <F5> key (yuck! Visual Foo convention :-) ) wasn't mapped yet, it will
be mapped to :Bexec in normal mode, and :BexecVisual in visual mode. So all
you have to do to execute the script in the current buffer is (visually
select some text and) hit <F5>.

The script in the current buffer will be executed if it has a shebang
(#!/path/to/interpreter) on the first line. If the type of the file is known
by Bexec, it will try to run a default interpreter for the script. 

If the script is not yet saved, or if you are trying to execute only a
visually selected piece of script, Bexec will save the buffer or selection
to a temporary file and execute it from there.

Bexec will grab the scripts output and put it in a seperate buffer. If the
output buffer doesn't exist yet, Bexec will create one for you. If a window
displaying the buffer is not yet open, Bexec will split the current window
and set it to display the output buffer. 

=============================================================================
INSTALLATION                                                  *bexec-install*

Bexec is distributed as a |Vimball|. To install it, just edit the
bexec-vX.Y.vba script in Vim and run the following command: >

    :so %

You can now pull up the Bexec help using >
    
    :help bexec

=============================================================================
SETTINGS                                                     *bexec-settings*

Bexec's behaviour can be customized using a number of settings. You can
change these settings using the :|let| command: >

  :let settingname=value

Bexec offers the following settings:

                                                              *g:bexec_args*
To pass arguments to the script you're executing: >
  
  :let bexec_args="arg1 arg2 'arg 3'"

You can also redirect input if you wish: >

  :let bexec_args="-sort < mybooks.txt"

                                                           *g:bexec_argsbuf*
To have Bexec read the argument from an open buffer, put the arguments on the
first line of the buffer and: >

  :let bexec_argsbuf=[buffer number]

                                                          *g:bexec_splitdir*
To control the way Bexec splits the current window to create the output
window, you can modify the bexec_splitdir setting: >

  :let bexec_splitdir=[direction]

[direction] can be "hor" for horizontal or "ver" for vertical splitting.
The default is horizontal splitting.

                                                        *g:bexec_outputmode*
To control what Bexec does with the output of a script: >

  :let bexec_outputmode=[outputmode]

[outputmode] can be "replace" (default) to clear the output buffer before
each script execution. Use a value of "append" to append the output of the
script to the output buffer. Bexec will insert a separator line between
outputs.


                                                       *g:bexec_rehighlight*
If you want Bexec to re-highlight a visual selection after calling
:BexecVisual, set the following option to 1: >

  :let bexec_rehighlight=[1|0]

                                                      *g:bexec_outputscroll*
To change the way Bexec scrolls the output buffer window after execution:

  :let bexec_outputscroll=[1|0]

Turning this option on ('1') (default) will make Bexec scroll the output
list to the bottom of the output. If the option is off, Bexec will scroll
the top of the output to the top of the window.

=============================================================================
TIPS                                                             *bexec-tips*

You can use Bexec as a crude database front-end! Create an shell script that
runs SQL commands from a file using a commandline client. For instance, for
MySQL: /home/todsah/mysql_exec.sh >

  #!/bin/sh

  USERNAME='john'
  PASSWORD='secret'
  HOSTNAME='localhost'
  DATABASE='cms'

  mysql -t -u$USERNAME -p$PASSWORD -h$HOSTNAME $DATABASE < $1

Then, edit a file with a .sql extension (so you get syntax highlighting).
For example: >

  #!/home/todsah/mysql_exec.sh

  SELECT id, page_name FROM pages WHERE page_name LIKE 'Test%';

It will output something like:

  +----+-----------+
  | id | page_name |
  +----+-----------+
  |  1 | Test      | 
  +----+-----------+
  |  2 | Testing   | 
  +----+-----------+

It's not much better than a normal commandline database frontend, but at least
you get:

 * SQL syntax color highlighting
 * Easier editing than most database commandline clients.  
 * Execution of a just couple of lines of the script (Use visual mode select
   and BexecVisual)
 * Split screen SQL editing and execution.

Here's one for Sqlite3: >

  #!/bin/sh
  
  DBFILE="/var/www/dev/data/database"
  
  grep -v "^#" $1 | sqlite3 -header -column $DBFILE 

Output looks like:

  hostname             username    username    fullname     
  -------------------  ----------  ----------  -------------
  localhost.localhost  todsah      todsah      Ferry Boender
  localhost.localhost  todsah      todsah      Ferry Boender

