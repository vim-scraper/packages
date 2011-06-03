" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/fswitch.vim	[[[1
185
" ============================================================================
" File:        fswitch.vim
"
" Description: Vim global plugin that provides decent companion source file
"              switching
"
" Maintainer:  Derek Wyatt <derek at myfirstnamemylastname dot org>
"
" Last Change: March 23rd 2009
"
" License:     This program is free software. It comes without any warranty,
"              to the extent permitted by applicable law. You can redistribute
"              it and/or modify it under the terms of the Do What The Fuck You
"              Want To Public License, Version 2, as published by Sam Hocevar.
"              See http://sam.zoy.org/wtfpl/COPYING for more details.
" ============================================================================

if exists("g:disable_fswitch")
    finish
endif

" Version
let s:fswitch_version = '0.9.0'

" Get the path separator right
let s:os_slash = &ssl == 0 && (has("win16") || has("win32") || has("win64")) ? '\' : '/'

" Default locations - appended to buffer locations unless otherwise specified
let s:fswitch_global_locs = '.' . s:os_slash

"
" s:FSGetAlternateFilename
"
" Takes the path, name and extension of the file in the current buffer and
" applies the location to it.  If the location is a regular expression pattern
" then it will split that up and apply it accordingly.  If the location pattern
" is actually an explicit relative path or an implicit one (default) then it
" will simply apply that to the file directly.
"
function! s:FSGetAlternateFilename(filepath, filename, newextension, location, mustmatch)
    let parts = split(a:location, ':')
    if len(parts) == 2 && parts[0] == 'reg'
        if strlen(parts[1]) < 3
            throw 'Bad substitution pattern "' . a:location . '".'
        else
            let resep = strpart(parts[1], 0, 1)
            let regex = split(strpart(parts[1], 1), resep)
            if len(regex) < 2 || len(regex) > 3
                throw 'Bad substitution pattern "' . a:location . '".'
            else
                let pat = regex[0]
                let sub = regex[1]
                let flags = ''
                if len(regex) == 3
                    let flags = regex[2]
                endif
                if a:mustmatch == 1 && match(a:filepath, pat) == -1
                    let path = ""
                else
                    let path = substitute(a:filepath, pat, sub, flags) . s:os_slash .
                                \ a:filename . '.' . a:newextension
                endif
            endif
        endif
    elseif len(parts) == 2 && parts[0] == 'rel'
        let path = a:filepath . s:os_slash . parts[1] . 
                      \ s:os_slash . a:filename . '.' . a:newextension
    elseif len(parts) == 2 && parts[0] == 'abs'
        let path = parts[1] . s:os_slash . a:filename . '.' . a:newextension
    elseif len(parts) == 1 " This is the default relative path
        let path = a:filepath . s:os_slash . a:location . 
                      \ s:os_slash . a:filename . '.' . a:newextension
    endif

    return simplify(path)
endfunction

"
" s:SetVariables
"
" There are two variables that need to be set in the buffer in order for things
" to work correctly.  Because we're using an autocmd to set things up we need to
" be sure that the user hasn't already set them for us explicitly so we have
" this function just to check and make sure.  If the user's autocmd runs after
" ours then they will override the value anyway.
"
function! s:SetVariables(dst, locs)
    if !exists("b:fswitchdst")
        let b:fswitchdst = a:dst
    endif
    if !exists("b:fswitchlocs")
        let b:fswitchlocs = a:locs
    endif
endfunction

"
" FSwitch
"
" This is the only externally accessible function and is what we use to switch
" to the alternate file.
"
function! FSwitch(filename, precmd)
    let fullpath = expand(a:filename . ':p:h')
    let ext = expand(a:filename . ':e')
    let justfile = expand(a:filename . ':t:r')
    if !exists("b:fswitchdst")
        throw 'b:fswitchdst not set - read :help fswitch'
    endif
    let extensions = split(b:fswitchdst, ',')
    let locations = []
    if exists("b:fswitchlocs")
        let locations = split(b:fswitchlocs, ',')
    endif
    if !exists("b:fsdisablegloc") || b:fsdisablegloc == 0
        let locations += split(s:fswitch_global_locs, ',')
    endif
    if len(locations) == 0
        throw "There are no locations defined (see :h fswitchlocs and :h fsdisablegloc)"
    endif
    let mustmatch = 1
    if exists("b:fsneednomatch") && b:fsneednomatch != 0
        let mustmatch = 0
    endif
    let newpath = ''
    let firstNonEmptyPath = ''
    for currentExt in extensions
        for loc in locations
            let newpath = s:FSGetAlternateFilename(fullpath, justfile, currentExt, loc, mustmatch)
            if newpath != '' && firstNonEmptyPath == ''
                let firstNonEmptyPath = newpath
            endif
            let newpath = glob(newpath)
            if filereadable(newpath)
                break
            endif
        endfor
        if filereadable(newpath)
            break
        endif
    endfor
    let openfile = 1
    if !filereadable(newpath)
        if exists("b:fsnonewfiles") || exists("g:fsnonewfiles")
            let openfile = 0
        else
            let newpath = firstNonEmptyPath
        endif
    endif
    if openfile == 1
        if newpath != ''
            if strlen(a:precmd) != 0
                execute a:precmd
            endif
            execute 'edit ' . fnameescape(newpath)
        else
            echoerr "Alternate has evaluated to nothing.  See :h fswitch-empty for more info."
        endif
    else
        echoerr "No alternate file found.  'fsnonewfiles' is set which denies creation."
    endif
endfunction

"
" The autocmds we set up to set up the buffer variables for us.
"
augroup fswitch_au_group
    au!
    au BufEnter,BufWinEnter *.h call s:SetVariables('cpp,c', 'reg:/include/src/,reg:/include.*/src/,../src')
    au BufEnter,BufWinEnter *.c call s:SetVariables('h', 'reg:/src/include/,reg:|src|include/**|,../include')
    au BufEnter,BufWinEnter *.cpp call s:SetVariables('h', 'reg:/src/include/,reg:|src|include/**|,../include')
augroup END

"
" The mappings used to do the good work
"
com! FSHere       :call FSwitch('%', '')
com! FSRight      :call FSwitch('%', 'wincmd l')
com! FSSplitRight :call FSwitch('%', 'vsplit \| wincmd l')
com! FSLeft       :call FSwitch('%', 'wincmd h')
com! FSSplitLeft  :call FSwitch('%', 'vsplit \| wincmd h')
com! FSAbove      :call FSwitch('%', 'wincmd k')
com! FSSplitAbove :call FSwitch('%', 'split \| wincmd k')
com! FSBelow      :call FSwitch('%', 'wincmd j')
com! FSSplitBelow :call FSwitch('%', 'split \| wincmd j')

doc/fswitch.txt	[[[1
379
*fswitch.txt*	For Vim version 7.2 and above	Last change: 2009 Mar 23

			       ---------------
 			        File Switcher
			       ---------------

Author:  Derek Wyatt (derek at myfirstnamemylastname dot org)

							*fswitch-copyright*
Copyright: The VIM LICENSE applies to fswitch.vim, and fswitch.txt
	   (see |copyright|) except use "fswitch" instead of "Vim".
	   No warranty, express or implied.
	   Use At-Your-Own-Risk!

==============================================================================
						*fswitch* *fswitch-contents*
1. Contents~

	1. Contents .............................: |fswitch-contents|
	2. About ................................: |fswitch-about|
	3. Features .............................: |fswitch-features|
	4. Setup ................................: |fswitch-setup|
	5. Configuration ........................: |fswitch-altcreate|
	6. "Creating" the Alternate File ........: |fswitch-altcreate|
	7. Useful Mappings ......................: |fswitch-mappings|
	8. The FSwitch Function .................: |fswitch-function|
	9. The Default Settings .................: |fswitch-defaults|
       10. Examples .............................: |fswitch-examples|

==============================================================================
							*fswitch-about*
2. About~

FSwitch is designed to allow you to switch between companion files of source
code (e.g. "cpp" files and their corresponding "h" files).  The source for
this came from a home-grown script that was influenced later by the
"Alternate" (a.vim) script.

The original intention was to modify the existing a.vim script to do what the
home-grown version could do (choose to open the other file in an existing
window) but it was a rather complex script and modification looked difficult
so the choice was made to simply move the home-grown script forward a couple
of notches and produce a new plugin.  This doc file is twice the length of the
actual code at this point :)

==============================================================================
							*fswitch-features*
3. Features~

FSwitch has the following features:

  - Switches between a file and its companion file
  - Ability to create a new file using a preferential location
  - Simple configuration using buffer-local variables
  - Umm... other stuff?

==============================================================================
							*fswitch-setup*
4. Setup~

Most of the behaviour of FSwitch is customized via buffer-local variables.
You set up the variables with auto commands:
>
  au! BufEnter *.cpp let b:fswitchdst = 'hpp,h' | let b:fswitchlocs = '../inc'
<
That |:autocmd| will set the 'fswitchdst' and 'fswitchlocs' variables when the
|BufEnter| event takes place on a file whose name matches {*.cpp} (e.g. when
you enter the buffer containing the {MyFile.cpp} file).

The variables above state that the alternate file to {MyFile.cpp} are
{MyFile.hpp} and {MyFile.h} preferred in that order, and located in the {inc}
directory at the same level as the current directory.

That should get you there but there's more capability here if you want.  To
get that move on to |fswitch-configure|.

==============================================================================
							*fswitch-configure*
5. Configuration~


							*'fswitchdst'*
'fswitchdst'	string	(default depends on file in current buffer)
		local to buffer

	The 'fswitchdst' variable denotes the file extension that is the
	target extension of the current file's companion file.  For example:
>
		:let b:fswitchdst = 'cpp,cxx,C'
<
	The above specifies that the current buffer's file has a companion
	filename which can be found by replacing the current extension with
	{cpp}, {cxx} or {C}.  The extensions will be tried in this order and
	the first match wins.

	'fswitchdst' is taken relative to directories that are found in the
	'fswitchlocs' variable.

							*'fswitchlocs'*
'fswitchlocs'	string	(default depends on filen in current buffer)
		local to buffer

	The 'fswitchlocs' variable contains a set of directives that indicate
	directory names that should be formulated when trying to find the
	alternate file.  For example:
>
		" Create the destination path by substituting any
		" 'include' string from the pathname with 'src'
		:let b:fswitchlocs = 'reg:/include/src/'

                " First try adding the relative path '../src' to the path
		" in which the file in the buffer exists and if that fails
		" then try using 'source' instead
		:let b:fswitchlocs = 'rel:../src,source'

		" Same as above but leaving off the optional 'rel:'
		:let b:fswitchlocs = '../src,../source'
<
	The following types of directives are understood:

	reg:~
		A regular expression.  The regular expression takes the form:
>
			{delim}{pat}{delim}{globsub}{delim}
<
		Where:
		
		{delim}   is something that doesn't appear in {pat} or
			  {globsub} used to delimit the {pat} and {globsub}

		{pat}	  is a standard pattern to search on

		{globsub} is a substitution string that will be run through
       		          the |glob()| function.

	rel:~
		A relative path.  The {rel:} is actually optional.  If you
		leave this off, then FSwitch will assume that the string is
		denoting a relative path.
	
	abs:~
		An absolute path.  I have no idea why you'd ever want to do
		this, but it's there if you want it.

							*'fswitchdisablegloc'*
'fsdisablegloc'
		string	(default off)
		local to buffer
	
	Disables the appending of the default global locations to the local
	buffer definition.  Normally when processing alternate file locations
	FSwitch will append some default values to the list of locations.  If
	you define this variable then this will not happen.

	The default locations are currently set to "./" or ".\" depending on
	what slash your configuration evaluates to.

							*'fswitchnonewfiles'*
'fsnonewfiles'
		string	(default off)
		local to buffer and global
	
	This variable is both global and local.  If you want to disable the
	creation of the alternate file when it doesn't already exist you can
	choose to do this on a per-extension basis or globally.  Set the
	global one to shut it off all the time and use the buffer version to
	shut it off locally.

							*'fsneednomatch'*
'fsneednomatch'
		string	(default off)
		local to buffer and global
	
	Normally when doing a regular expression alteration of the path (see
	{reg:} in 'fswitchdst' the pattern you're going to substitute the
	value with must actually match in the string.  When it doesn't matter
	whether or not that the match actually takes place, you can set this
	value.

	If you do set this then the failure to match actually results in
	nothing happening at all.  So if the right filename exists in the same
	directory as the one you're switching from then that's the one that
	will be switched to.

	Example:
>

		If the b:fswitchlocs is set to

		   reg:/src/include/,include

                and

		  # This is the file we're editing
		  ~/code/program/myfile.c

		  # These choices exist for the header file
		  ~/code/program/myfile.h
		  ~/code/program/include/myfile.h
<
	Then the first substitution will result in the first header file being
	chosen, not the second.

==============================================================================
							*fswitch-altcreate*
6. "Creating" the Alternate File~

If the file being switched to does not exist, and 'fsnonewfiles' has not been
set, then it will be created as a new, unwritten buffer.  If there are
multiple possibilities here, FSwitch prefers the first possible match.  For
example if the current buffer has a filename called {/code/src/a/b/MyFile.cpp}
and has the following set:
>
		let b:fswitchdst = 'h,hpp'
		let b:fswitchlocs = 'reg:/src/include/,../include,../inc'
<
then the created filename will be {/code/include/a/b/MyFile.cpp}.

As stated, this file hasn't actually been written to yet so you could easily
delete the buffer and there's no harm done but you also may not be able to
write the buffer very easily if the directory hierarchy doesn't yet exist.  In
this case, it's quite helpful to define a mapping for easily creating the
directory for you:
>
		nmap <Leader>md :!mkdir -p %:p:h<cr>
<
Then it's pretty easy to create the directory before writing the file.


==============================================================================
							*fswitch-mappings*
7. Useful Mappings~

I didn't bother putting mappings into the script directly as this might have
caused conflicts and I don't know how to avoid those.  I use the following
mappings myself:

 - Switch to the file and load it into the current window >
	nmap <silent> <Leader>of :FSHere<cr>
<
 - Switch to the file and load it into the window on the right >
	nmap <silent> <Leader>ol :FSRight<cr>
<
 - Switch to the file and load it into a new window split on the right >
	nmap <silent> <Leader>oL :FSSplitRight<cr>
<
 - Switch to the file and load it into the window on the left >
	nmap <silent> <Leader>oh :FSLeft<cr>
<
 - Switch to the file and load it into a new window split on the left >
	nmap <silent> <Leader>oH :FSSplitLeft<cr>
<
 - Switch to the file and load it into the window above >
	nmap <silent> <Leader>ok :FSAbove<cr>
<
 - Switch to the file and load it into a new window split above >
	nmap <silent> <Leader>oK :FSSplitAbove<cr>
<
 - Switch to the file and load it into the window below >
	nmap <silent> <Leader>oj :FSBelow<cr>
<
 - Switch to the file and load it into a new window split below >
	nmap <silent> <Leader>oJ :FSSplitBelow<cr>
<
==============================================================================
							*fswitch-function*
8. The FSwitch Function~

The main work is done by the FSwitch() function.  The reason it's documented
here is because you can use it to do something more interesting if you wish.
As it stands now, you get the "Split Above and Switch" functionality by
calling FSwitch() like this:
>
    FSwitch('%', 'split \| wincmd k')
<
There's probably not much to stop anyone from doing something more interesting
in the second argument.  If this string is non-empty then it will be run
through an |:execute| call.

==============================================================================
							*fswitch-defaults*
9. The Default Settings~

By default FSwitch handles {c} and {cpp} files, favouring {cpp}.

For *.h files:
>
    let b:fswitchdst  = 'cpp,c'
    let b:fswitchlocs = 'reg:/include/src/,reg:/include.*/src/,../src'
<
For *.c
>
    let b:fswitchdst  = 'h'
    let b:fswitchlocs = 'reg:/src/include/,reg:|src|include/**|,../include'
<

For *.cpp
>
    let b:fswitchdst  = 'h'
    let b:fswitchlocs = 'reg:/src/include/,reg:|src|include/**|,../include'
<

==============================================================================
							*fswitch-examples*
10. Examples~

							*fswitch-example1*
Let's say you have a C++ codebase and it has the following properties (this
level of insanity is a bit high but versions that are only slightly saner
exist in real life):

	- Source files with {.cpp}, {.cc} and {.C} extensions
	- Header files with {.h} extensions
	- Source files and header files in the same directory
	- Source files in the {src} directory and include files in the
	  {include} directory
	- Source files in the {src} directory and include files in the
	  {include/name/space} directory (i.e. subdirectories denoted by the
	  namespace).
	- Source files in {src/name/space} and header files in
	  {include/name/space} (i.e. subdirectories denoted by the namespace).

As a final part to this, the "new" way of doing things in this source tree is
to put header files in a directory noted by namespace and to do the same with
source files and to name source files with a {cpp} extension.

In order to switch between files organized like this, you could specify the
following:
>
 augroup mycppfiles
   au!
   au BufEnter *.h let b:fswitchdst  = 'cpp,cc,C'
   au BufEnter *.h let b:fswitchlocs = 'reg:/include/src/,reg:/include.*/src/'
 augroup END
<
Here the setting of b:fswitchdst to {cpp,cc,C} handles the different C++
extensions, and prefers to use {cpp} and will create new files with that
extension.

The fswitchlocs setting allows for the following:

	reg:/include/src/~

		Take the pathname to the file in the current buffer and
		substitute "src" for "include".  This handles the following
		possibilities:

		- Files are in {include} and {src} respectively
		- Files are in {include/name/space} and {src/name/space}
		  respectively

	reg:/include.*/src/~

		Take the pathname to the file in the current buffer and
		substitute "src" for "include.*".  This handles the following
		possibility:

		- Files are in {include/name/space} and {src} respectively
	
	./~
		This one's a hiddden option.  The default location is the
		current directory already so we don't explicitly have to state
		this, but it is the last possibility:

		- Files are in the same directory

							*fswitch-example2*
Here we'll just show a quick example of making use of the globbing aspect of
the system.  Let's say you're working on a {cpp} file and you want to find the
matching header file, and you have your destination and locations set to the
following:
>
	let b:fswitchdst = 'h'
	let b:fswitchlocs = 'reg:|src|include/**|'
>
then if you have the a file {src/MyFile.cpp} then this will find the file
{include/name/space/MyFile.h}.

vim:tw=78:sts=8:ts=8:sw=8:noet:ft=help:
