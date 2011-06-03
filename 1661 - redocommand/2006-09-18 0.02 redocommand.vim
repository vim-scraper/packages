" redocommand.vim : Execute commands from the command history. 
"
" DESCRIPTION:
"   Re-executes ex commands previously entered in command mode. An optional
"   pattern is used to locate the most recent matching command. This is similar
"   to the command-line window (q:), or navigating the command history via <Up>
"   and <Down>, but provides an even faster way to re-executing a command if you
"   remember some characters or a pattern that identifies the command line.
"   The redocommand itself will not be included in the command history. 
"
" USAGE:
"   :Redocommand (or abbreviated :R) executes the last ex command. 
"   :Redocommand <pattern> executes the last ex command that matches <pattern>. 
"
" EXAMPLE:
"   :history
"   1 e foo.txt
"   2 %s/foo/\0bar/g
"   3 w bar.txt
"
"   ':Redocommand' will execute the last command ':w bar.txt'
"   ':Redocommand %' will execute ':%s/foo\0/bar/g'
"   ':Redocommand foo' will execute ':%s/foo\0/bar/g'
"
" REMARKS:
"   Modeled after Posix shell 'fc -s' command (which is often aliased to 'r'). 
"
" INSTALLATION:
"   Put the script into your user or system VIM plugin directory (e.g.
"   ~/.vim/plugin). 
"
" CONFIGURATION:
"   If you do not want the shorthand ':R' command, define (e.g. in your .vimrc): 
"	let g:redocommand_no_short_command = 1
"
" TODO:
"   - implement ':Redocommand old=new commandexpr'
"
" Copyright: (C) 2005-2006 by Ingo Karkat
"   The VIM LICENSE applies to this script; see ':help copyright'. 
"
" Maintainer:	Ingo Karkat <ingo@karkat.de>
" REVISION	DATE		REMARKS 
"	0.02	30-Mar-2006	Added requirements check.
"				Added (configurable) short command :R. 
"				Replaced quirky 'RemoveRedocommandFromHistory()'
"				with unconditional remove from history. 
"	0.01	23-May-2005	file creation

" Avoid installing twice or when in compatible mode
if exists("loaded_redocommand")
    finish
endif
let loaded_redocommand = 1

" Requirement: command-line history compiled-in and activated
if (! has("cmdline_hist")) || (&history < 2) 
    finish
endif

if ! exists("g:redocommand_no_short_command")
    command! -nargs=? -complete=command R call <SID>Redocommand(<f-args>)
endif
command! -nargs=? -complete=command Redocommand call <SID>Redocommand(<f-args>)

function! s:Redocommand( ... )
    if a:0 == 0
	" An empty expression always matches, so this is used for the cornercase
	" of no expression passed in, in which the last history command is
	" executed. 
	let l:commandexpr = ""
    elseif a:0 == 1
	let l:commandexpr = a:1
    else
	assert 0
    endif

    " The history must not be cluttered with :Redocommands. 
    " Remove the ':Redocommand' that is currently executed from the history. 
    " If someone foolishly uses :Redocommand in a mapping or script (where
    " commands are not added to the history), an innocent last history entry
    " will be removed - bad luck. 
    call histdel("cmd", -1)

    let l:histnr = histnr("cmd") 
    while l:histnr > 0
	let l:historyCommand = histget("cmd", l:histnr)
	if l:historyCommand =~ l:commandexpr
	    echo ":" . l:historyCommand
	    execute l:historyCommand
	    return
	endif
	let l:histnr = l:histnr - 1
    endwhile

    echohl WarningMsg
    echo "No command matching \"" . l:commandexpr . "\" found in history."
    echohl None
endfunction

