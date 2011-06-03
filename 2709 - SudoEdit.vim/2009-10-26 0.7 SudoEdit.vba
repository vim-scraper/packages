" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/SudoEdit.txt	[[[1
166
*SudoEdit.txt*	Edit Files using Sudo/su - Vers 0.7		Oct 26, 2009

Author:  Christian Brabandt <cb@256bit.org>
Copyright: (c) 2009 by Christian Brabandt 		*SudoEdit-copyright*
           The VIM LICENSE applies to SudoEdit.vim and SudoEdit.txt
           (see |copyright|) except use SudoEdit instead of "Vim".
	   NO WARRANTY, EXPRESS OR IMPLIED.  USE AT-YOUR-OWN-RISK.


==============================================================================
1. Contents				*SudoEdit* *SudoEdit-contents*

	1.  Contents......................................: |SudoEdit-contents|
	2.  SudoEdit Manual...............................: |SudoEdit-manual|
	2.1 SudoEdit: SudoRead............................: |SudoRead|
	2.2 SudoEdit: SudoWrite...........................: |SudoWrite|
	3.  SudoEdit Configuration........................: |SudoEdit-config|
        4.  SudoEdit Debugging............................: |SudoEdit-debug|
        5.  SudoEdit F.A.Q................................: |SudoEdit-faq|
	6.  SudoEdit History..............................: |SudoEdit-history|

==============================================================================
2. SudoEdit Manual					*SudoEdit-manual*

Functionality

This plugin enables vim to read files, using sudo or su or any other tool that
can be used for changing the authentication of a user. Therefore it needs any
of sudo or su installed and usable by the user. This means, you have to know
the credentials to authenticate yourself as somebody else.

That's why this plugin probably won't work on Windows, but you might be able
to configure it to use a method that works on Windows (see |SudoEdit-config|)

By default SudoEdit will first try to use sudo and if sudo is not found it
will fall back and try to use su. Note, that you might have to configure these
tools, before they can use them successfully.

SudoEdit requires at least a Vim Version 7 with patch 111 installed. Patch 111
introduced the |shellescape()| functionality.

The SudoEdit Plugin provides 2 Commands:

==============================================================================
2.1 SudoRead							 *SudoRead*

	:SudoRead [file]

SudoRead will read the given file name using any of the configured methods for
superuser authtication. It basically does something like this:

:r !sudo cat file

If no filename is given, SudoRead will try to reread the current file name.
If the current buffer does not contain any file, it will abort.

SudoRead provides file completion, so you can use <Tab> on the commandline to
specify the file to read.

For compatibility with the old sudo.vim Plugin, SudoEdit.vim also supports
reading and writing using the protocol sudo: So instead of using :SudoRead
/etc/fstab you can also use :e sudo:/etc/fstab (which does not provide
filename completion)

==============================================================================
2.2 SudoWrite							 *SudoWrite*

	:[range]SudoWrite [file]

SudoWrite will write the given file using any of the configured methods for
superuser authtication. It basically does something like this:

:w !sudo tee >/dev/null file

If no filename is given, SudoWrite will try to write the current file name.
If the current buffer does not contain any file, it will abort.

You can specify a range to write just like |:w|. If no range is given, it will
write the whole file.

Again, you can use the protocol handler sudo: for writing.

==============================================================================
3. SudoEdit Configuration				*SudoEdit-config* 

By default SudoEdit will try to use sudo and if it is not found, it will try
to use su. Just because SudoEdit finds either sudo or su installed, does not
mean, that you can already use it. You might have to configure it and of
course you need to have the credentials for super-user access.

								*g:sudoAuth*

The tool to use for authentication is can be changed by setting the variable
g:sudoAuth. If this variable exists, SudoEdit will first try to use the
specified tool before falling back to either sudo or su (in that order).

For example, you could use ssh to use as authentication tool by setting
g:sudoAuth in your .vimrc as follows:

let g:sudoAuth="ssh"

							       *g:sudoAuthArg*

The variable g:sudoAuthArg specifies how to use the given authentication tool.
You can specify additional parameters that will be used. You could for example
also define here which user to change to. By default, SudoEdit will try to
become the superuser e.g. root. 

If you want to use ssh as authentication facility, you can set g:sudoAuthArg
as follows in your .vimrc:

let g:sudoAuthArg="root@localhost"

For su, you would use g:sudoAuthArg="-c", but you do not have to set it, the
plugin will automatically use -c if it detects, that su is used.

==============================================================================
4. SudoEdit Debugging					    *SudoEdit-debug*

You can debug this plugin and the shell code that will be executed by
setting:
let g:sudoDebug=1
This ensures, that debug messages will be appended to the |message-history|.


==============================================================================
5. SudoEdit F.A.Q.					    *SudoEdit-faq*

1) This plugin isn't working, while executing the same commands on the
   shell works fine using sudo.

Make sure, that requiretty is not set. If it is set, you won't be able to use
sudo from within vim.

2) The plugin is still not working!

Write me an email (look in the first line for my mail address), append the
debug messages and tell me what exactly is not working. I will look into it
and if there is a bug fix this plugin.

3) Great work!

Write me an email (look in the first line for my mail address). And if you are
really happy, vote for the plugin and consider looking at my Amazon whishlist:
http://www.amazon.de/wishlist/2BKAHE8J7Z6UW

==============================================================================
6. SudoEdit History					    *SudoEdit-history*
	0.7: Oct  26, 2009	: Support for reading/writing using sudo:
	                          protocol handler
				  Added Debugging capabilities
	0.6: July 14, 2009	: Fix minor bug, that prevents setting the 
				  filename correctly when writing.
	0.5: July 08, 2009	: Enables the plugin for |GetLatestVimScripts|
	0.4: July 08, 2009	: First release
				  Added Documentation
	0.3: July 07, 2009	: Internal version, added su support
				  Added configuration variables
	0.2: July 07, 2009	: Internal version, Working sudo support
				  Created plugin
	0.1: July 07, 2009	: Internal version, First working version, 
				  using simple commands

==============================================================================
Modeline:
vim:tw=78:ts=8:ft=help
plugin/SudoEdit.vim	[[[1
198
" SudoEdit.vim - Use sudo/su for writing/reading files with Vim
" ---------------------------------------------------------------
" Version:  0.7
" Authors:  Christian Brabandt <cb@256bit.org>
" Last Change: 2009/07/08
" Script:  http://www.vim.org/scripts/script.php?script_id=2709 
" License: VIM License
" GetLatestVimScripts: 2709 5 :AutoInstall: SudoEdit.vim

" Configuration:"{{{
" Exit quickly when:
" - this plugin was already loaded
" - when 'compatible' is set
if exists('loaded_sudowrite') || &cp
    finish
endif

if v:version < 700 || ( v:version == 700 && !has("patch111"))
  echomsg 'sudowrite: You need at least Vim 7.0 with patch111'
  finish
endif

let loaded_sudowrite=0.7

" Which Tool for super-user access to use"{{{
" Will be tried in order, first tool that is found will be used
" (e.g. you could use ssh)
" You can specify one in your .vimrc using the
" global variable g:sudoAuth
let s:sudoAuth=" sudo su "
if exists("g:sudoAuth")
    let s:sudoAuth = g:sudoAuth . s:sudoAuth
endif
"}}}

" Specify the parameter to use for the auth tool e.g. su uses "-c", but
" for su, it will be autodetected, sudo does not need one, for ssh use 
" "root@localhost"
"
" You can also use this parameter if you do not want to become root 
" but any other user
"
" You can specify this parameter in your .vimrc using the
" global variable g:sudoAuthArg
if !exists("g:sudoAuthArg")
    let s:sudoAuthArg=""
else
    let s:sudoAuthArg=g:sudoAuthArg
endif
"}}}




" Functions:"{{{

fu! <SID>LocalSettings(setflag)
    if a:setflag
	" Set shellrediraction temporarily
	" This is used to get su working right!
	let s:o_srr=&srr
	let &srr='>'
    else
	" Reset old settings
	" shellredirection
	let &srr=s:o_srr
    endif
endfu

fu! <SID>CheckAuthTool(Authlist)"{{{
    for tool in a:Authlist
	if executable(tool)
	    return [tool]
	endif
    endfor
    echoerr "No tool found for authentication. Is sudo/su installed and in your $PATH?"
    echoerr "Try setting g:sudoAuth and g:sudoAuthArg"
    return []
endfu"}}}

let s:AuthTool=<SID>CheckAuthTool(split(s:sudoAuth, '\s'))"{{{
if empty(s:AuthTool)
    finish
endif"}}}

if s:AuthTool[0] == "su" && empty(s:sudoAuthArg)
    let s:sudoAuthArg="-c"
endif
call add(s:AuthTool, s:sudoAuthArg . " ")

fu! <SID>echoWarn(mess)
    echohl WarningMsg
    echomsg a:mess
    echohl Normal
endfu

fu! <SID>SudoRead(file)
    %d
"    let cmd=':0r !' . join(s:AuthTool, ' ') . ' cat ' . a:file . ' 2>/dev/null '
    if !exists("g:sudoDebug")
	let cmd='cat ' . shellescape(a:file,1) . ' 2>/dev/null'
    else
	let cmd='cat ' . shellescape(a:file,1) 
    endif
    "let cmd='cat ' . shellescape(a:file,1) 
    if  s:AuthTool[0] =~ '^su$'
        let cmd='"' . cmd . '" --'
    endif
    let cmd=':0r! ' . join(s:AuthTool, ' ') . cmd
    if exists("g:sudoDebug") && g:sudoDebug
	call <SID>echoWarn(cmd)
    endif
    silent! exe cmd
    $d 
    exe ":f " . a:file
    filetype detect
    set nomod
endfu

fu! <SID>SudoWrite(file) range
    if  s:AuthTool[0] =~ '^su$'
	" Workaround since su cannot be run with :w !
	    let tmpfile = tempname()
	    exe a:firstline . ',' . a:lastline . 'w ' . tmpfile
	    let cmd=':!' . join(s:AuthTool, ' ') . '"mv ' . tmpfile . ' ' . a:file . '" --'
    else
	let cmd='tee >/dev/null ' . a:file
	let cmd=a:firstline . ',' . a:lastline . 'w !' . join(s:AuthTool, ' ') . cmd
    endif
    if exists("g:sudoDebug") && g:sudoDebug
	call <SID>echoWarn(cmd)
    endif
    silent exe cmd
    if v:shell_error
	if exists("g:sudoDebug") && g:sudoDebug
	    call <SID>echoWarn(v:shell_error)
	endif
	throw "writeError"
    endif
    exe ":f " . a:file
    set nomod
endfu

fu! <SID>Stats(file)
    ":w echoes a string like this by default:
    ""SudoEdit.vim" 108L, 2595C geschrieben
    return '"' . a:file . '" ' . line('$') . 'L, ' . getfsize(expand(a:file)) . 'C written'
endfu



fu! <SID>SudoDo(readflag, file) range
    call <SID>LocalSettings(1)
"    let file = substitute(a:file, '^sudo:', '', '')
    let file = !empty(a:file) ? substitute(a:file, '^sudo:', '', '') : expand("%")
    if empty(file)
	throw "emptyfile"
    endif
    if a:readflag
	call <SID>SudoRead(file)
    else
	try
	    exe a:firstline . ',' . a:lastline . 'call <SID>SudoWrite(' . shellescape(file,1) . ')'
	    echo <SID>Stats(file)
	catch /emptyfile/
	    echoerr "Cannot write file. Please enter filename for writing!"
	catch /writeError/
	    let a=v:errmsg
	    echoerr "There was an error writing the file!"
	    echoerr a
	finally
	    call <SID>LocalSettings(0)
	    redraw!
	endtry
	sleep
    endif
    if v:shell_error
	echoerr "Error " . ( a:readflag ? "reading " : "writing to " )  . file . "! Password wrong?"
    endif
    call <SID>LocalSettings(0)
    redraw!
endfu
"}}}"}}}

" Define User-Commands and Autocommand "{{{
com! -complete=file -range=% -nargs=? SudoWrite :<line1>,<line2>call <SID>SudoDo(0, <q-args>)
com! -complete=file -nargs=? SudoRead  :call <SID>SudoDo(1, <q-args>)

augroup Sudo
	autocmd!
	au BufReadCmd,FileReadCmd sudo:/*,sudo:* SudoRead <afile>
	au BufWriteCmd,FileWriteCmd sudo:/*,sudo:* SudoWrite <afile>
augroup END
"}}}


" Modeline {{{1
" vim: set fdm=marker fdl=0 :  }}}
