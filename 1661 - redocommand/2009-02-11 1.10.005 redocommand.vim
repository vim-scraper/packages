" redocommand.vim : Execute commands from the command history. 
"
" DESCRIPTION:
"   Re-executes ex commands previously entered in command mode. An optional
"   pattern is used to locate the most recent matching command. This is similar
"   to the command-line window (q:), or navigating the command history via <Up>
"   and <Down>, but provides an even faster way to re-execute a command if you
"   remember some characters or a pattern that identifies the command line.
"   The redocommand itself will not be included in the command history. 
"   Literal replacement can be done via 'old=new' arguments. 
"
" USAGE:
"   :Redocommand (or abbreviated :R)
"	Execute the last ex command. 
"   :Redocommand <pattern>
"	Execute the last ex command that matches <pattern>. Settings such as
"	'magic' and 'ignorecase' apply. 
"   :Redocommand old=new [old2=new2 ...] [<pattern>]
"	Execute the last ex command (that matches <pattern>), literally
"	replacing 'old' with 'new'. 
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
"   ':Redocommand b=B .txt=' will execute ':w bar.txt' as ':w Bar'
"
" REMARKS:
"   Modeled after Posix shell 'fc -s' command (which is often aliased to 'r'). 
"
" INSTALLATION:
"   Put the script into your user or system VIM plugin directory (e.g.
"   ~/.vim/plugin). 
"
" DEPENDENCIES:
"   - Requires VIM 7.0 or higher.  
"
" CONFIGURATION:
"   If you do not want the shorthand ':R' command, define (e.g. in your .vimrc): 
"	let g:redocommand_no_short_command = 1
"
" TODO:
"
" Copyright: (C) 2005-2009 by Ingo Karkat
"   The VIM LICENSE applies to this script; see ':help copyright'. 
"
" Maintainer:	Ingo Karkat <ingo@karkat.de>
" REVISION	DATE		REMARKS 
"   1.10.005	16-Jan-2009	Now setting v:errmsg on errors. 
"   1.10.004	04-Aug-2008	Implemented ':Redocommand old=new <pattern>'. 
"				Now requiring VIM 7. 
"   1.00.003	04-Aug-2008	Better handling of errors during execution of
"				the command. 
"				The redone command is added to the history. 
"	0.02	30-Mar-2006	Added requirements check.
"				Added (configurable) short command :R. 
"				Replaced quirky 'RemoveRedocommandFromHistory()'
"				with unconditional remove from history. 
"	0.01	23-May-2005	file creation

" Avoid installing twice or when in unsupported VIM version.  
if exists('g:loaded_redocommand') || (v:version < 700)
    finish
endif
let g:loaded_redocommand = 1

" Requirement: command-line history compiled-in and activated
if (! has('cmdline_hist')) || (&history < 2) 
    finish
endif

" To make the arg count as <pattern>, not a substitution, either use '.' instead
" of '=', or have the pattern start with '='. 
let s:patternPattern = '\(^.\+\)=\(.*$\)'
function! s:IsSubstitution( arg )
    return a:arg =~ s:patternPattern
endfunction
function! s:Substitute( expr, patterns )
    let l:replacement = a:expr

    for l:pattern in a:patterns
	let [l:match, l:from, l:to; l:rest] = matchlist( l:pattern, s:patternPattern )
	" Assumption: Applicability of a:pattern has been checked before via
	" s:IsSubstitution(). 
	if empty(l:match) || empty(l:from) | throw 'ASSERT: Pattern can be applied. ' | endif
	let l:replacement = substitute( l:replacement, '\V' . escape(l:from, '\'), escape(l:to, '\&~'), 'g' )
    endfor

    return l:replacement
endfunction

function! s:Redocommand( ... )
    " An empty expression always matches, so this is used for the corner case of
    " no expression passed in, in which the last history command is executed. 
    let l:commandexpr = ''
    let l:substitutions = []

    let l:argIdx = 0
    while l:argIdx < a:0
	if s:IsSubstitution(a:000[l:argIdx])
	    call add(l:substitutions, a:000[l:argIdx])
	else
	    " Strictly, only the last argument should be the optional expr. If
	    " there are multiple expr, join them together with a <Space> in
	    " between. This way, spaces in the expr need not necessarily be
	    " escaped. 
	    let l:commandexpr = join(a:000[l:argIdx : ] , ' ')
	    break
	endif
	let l:argIdx += 1
    endwhile

    " The history must not be cluttered with :Redocommands. 
    " Remove the ':Redocommand' that is currently executed from the history. 
    " If someone foolishly uses :Redocommand in a mapping or script (where
    " commands are not added to the history), an innocent last history entry
    " will be removed - bad luck. 
    call histdel('cmd', -1)

    let l:histnr = histnr('cmd') 
    while l:histnr > 0
	let l:historyCommand = histget('cmd', l:histnr)
	if l:historyCommand =~ l:commandexpr
	    let l:newCommand = s:Substitute( l:historyCommand, l:substitutions )
	    echo ":" . l:newCommand
	    try
		execute l:newCommand
		call histadd(':', l:newCommand)
	    catch /^Vim\%((\a\+)\)\=:E/
		echohl ErrorMsg
		" v:exception contains what is normally in v:errmsg, but with extra
		" exception source info prepended, which we cut away. 
		let v:errmsg = substitute(v:exception, '^Vim\%((\a\+)\)\=:', '', '')
		echomsg v:errmsg
		echohl None
	    endtry
	    return
	endif
	let l:histnr = l:histnr - 1
    endwhile

    echohl WarningMsg
    let v:warningmsg = 'No command matching "' . l:commandexpr . '" found in history.'
    echomsg v:warningmsg
    echohl None
endfunction

if ! exists('g:redocommand_no_short_command')
    command! -nargs=* -complete=command R call <SID>Redocommand(<f-args>)
endif
command! -nargs=* -complete=command Redocommand call <SID>Redocommand(<f-args>)

" vim: set sts=4 sw=4 noexpandtab ff=unix fdm=syntax :
