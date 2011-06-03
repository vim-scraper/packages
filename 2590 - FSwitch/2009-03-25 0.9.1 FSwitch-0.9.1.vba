" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/fswitch.txt	[[[1
525
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
	5. Configuration ........................: |fswitch-configure|
	6. "Creating" the Alternate File ........: |fswitch-altcreate|
	7. Useful Mappings ......................: |fswitch-mappings|
	8. FSwitch() ............................: |fswitch-function|
	9. FSReturnCompanionFilenameString().....: |fswitch-getcompanion|
       10. FSReturnReadableCompanionFilename()...: |fswitch-getreadablecomp|
       11. The Default Settings .................: |fswitch-defaults|
       12. Examples .............................: |fswitch-examples|
       13. Troubleshooting ......................: |fswitch-trouble|
        A. Change History .......................: |fswitch-changes|

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
  - It's got a really long doc file (... seriously, why is this thing so
    bloody long?)
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

							 *fswitch_reg*
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

							 *fswitch_rel*
	rel:~
		A relative path.  The {rel:} is actually optional.  If you
		leave this off, then FSwitch will assume that the string is
		denoting a relative path.
	
							 *fswitch_ifrel*
	ifrel:~
		Takes the same form as {:reg} but the {globsub} part of the
		directive is a relative path.  The relative path is only used
		if the {pat} matches the existing path of the buffer.
	
							 *fswitch_abs*
	abs:~
		An absolute path.  I have no idea why you'd ever want to do
		this, but it's there if you want it.

							 *fswitch_ifabs*
	ifabs:~
		Takes the same form as {:reg} but the {globsub} part of the
		directive is an absolute path.  The absolute path is only used
		if the {pat} matches the existing path of the buffer.
	
	Why use the "if" variants?

	Here's the situation: You've got the following file:
>
		~/code/MyFile.h
<
	And you've set the following locations:
>
		For .h   -> reg:/include/src/,../src,./
		For .cpp -> reg:/src/include/,../include,./
<
	Here's what happens when run the following commands:
>
		FSwitch('%')
		# Creates a new file ~/src/MyFile.cpp due to the first
		# relative path in the list for .h
		FSwitch('%')
		# Creates a new file ~/include/MyFile.h due to the first
		# regular expression in the list for .cpp
<
	The problem is that you've unconditionally said you want to use
	{../src} for the alternate file but in reality you probably wanted to
	use {./}.  If you use {:ifrel} instead then you can say that you only
	want to use {../src} if the path to the current buffer contains
	{/include/} or something like that.  If you did this FSwitch would not
	have taken {../src} for the new file but would have chosen {./}

	So the "right" setup is:
>
		For .h   -> reg:/include/src/,ifrel:|/include/|../src|,./
		For .cpp -> reg:/src/include/,ifrel:|/src/|../include|,./
<
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
							*FSwitch()*
8. FSwitch()~

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
		     *fswitch-getcompanion* *FSReturnCompanionFilenameString()*

9. FSReturnCompanionFilenameString()~

This function is used by |FSwitch()| to return the pathname to the preferred
companion file.  In this case, the file need not actually exist on the
filesystem but would be the one created if you chose to do so.  As an
example:
>
    let path = FSReturnCompanionFilenameString('%')
<
The resultant path string contains the preferred companion file or nothing if
no preferred file could be discovered.

==============================================================================
		*fswitch-getreadablecomp* *FSReturnReadableCompanionFilename()*

10. FSReturnReadableCompanionFilename()~

This function returns the companion file, but the companion file must be
readable on the filesystem for it to be successfully returned.
>
    let path = FSReturnReadableCompanionFilename('%')
<
The resultant path string contains the preferred companion file or nothing if
no preferred file could be found on the filesystem.

In order to see what created the need for this function, see
|fswitch-example3|.

==============================================================================
							*fswitch-defaults*
11. The Default Settings~

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
12. Examples~
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

							*fswitch-example3*
At work I'm a Windows C++ programmer and at home I'm a OS X Objective-C
programmer.  There's a problem with this... C++ and Objective-C both use the
same extension for header files ({.h}).

At home I want to be able to use the XCode command line builder in the
'makeprg' setting when I'm working on the code.  I would like this to be set
when I am on a {.m} file or its companion {.h} file.  This is done with the
following function:
>
    function! SetMakeForXCode(filename)
        let isObjCFile = 0
        let ext = expand(a:filename . ":e")
        if ext == 'm' || ext == 'mm'
            let isObjCFile = 1
        elseif ext == 'h'
	    " Find the companion file
            let companionfile = FSReturnReadableCompanionFilename('%')
            " For some reason expand() doesn't work on the next line
            let companionext = substitute(companionfile, '.*\.', '', '')
            if companionext == 'm' || companionext == 'mm'
                let isObjCFile = 1
            endif
        endif
        if isObjCFile == 1
            setl makeprg=xcodebuild\ -configuration\ Debug
        endif
    endfunction
<
Yup, this could have been easier by using the 'filetype' or using some sort of
|grep| call but I wanted to use this particular hammer. :)  I'll probably end
up switching it to use the 'filetype' instead in the end...

==============================================================================
							*fswitch-trouble*
13. TroubleShooting~
							*fswitch-empty*
You may get the following error:
>
  Alternate has evaluated to nothing.  See :h fswitch-empty for more info.
<
It can happen... This is probably due to the fact that you've got a nicely
strict set of rules for your locations.  With |fswitch-reg| and
|fswitch-ifrel| and |fswitch-ifabs| you can get rather specific about whether
or not anything actually happens.  If you aren't letting anything really
happen, it's not going to happen and you're going to end up with an empty
path.

==============================================================================
							*fswitch-changes*
A. Change History~

0.9.1
	- Added :ifrel (|fswitch_ifrel|)
	- Added :ifabs (|fswitch_ifabs|)
	- Added |FSReturnReadableCompanionFilename()|
	- Added |FSReturnCompanionFilenameString()|
	- Changed default settings for .h to use :ifrel instead of :rel
	- Changed default settings for .c and .cpp to use :ifrel instead of
	  :rel

0.9.0
	- Initial release

vim:tw=78:sts=8:ts=8:sw=8:noet:ft=help:
plugin/fswitch.vim	[[[1
293
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
let s:fswitch_version = '0.9.1'

" Get the path separator right
let s:os_slash = &ssl == 0 && (has("win16") || has("win32") || has("win64")) ? '\' : '/'

" Default locations - appended to buffer locations unless otherwise specified
let s:fswitch_global_locs = '.' . s:os_slash

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
" s:FSGetLocations
"
" Return the list of possible locations
"
function! s:FSGetLocations()
    let locations = []
    if exists("b:fswitchlocs")
        let locations = split(b:fswitchlocs, ',')
    endif
    if !exists("b:fsdisablegloc") || b:fsdisablegloc == 0
        let locations += split(s:fswitch_global_locs, ',')
    endif

    return locations
endfunction

"
" s:FSGetExtensions
"
" Return the list of destination extensions
"
function! s:FSGetExtensions()
    return split(b:fswitchdst, ',')
endfunction

"
" s:FSGetMustMatch
"
" Return a boolean on whether or not the regex must match
"
function! s:FSGetMustMatch()
    let mustmatch = 1
    if exists("b:fsneednomatch") && b:fsneednomatch != 0
        let mustmatch = 0
    endif

    return mustmatch
endfunction

"
" s:FSGetFullPathToDirectory
"
" Given the filename, return the fully qualified directory portion
"
function! s:FSGetFullPathToDirectory(filename)
    return expand(a:filename . ':p:h')
endfunction

"
" s:FSGetFileExtension
"
" Given the filename, returns the extension
"
function! s:FSGetFileExtension(filename)
    return expand(a:filename . ':e')
endfunction

"
" s:FSGetFileNameWithoutExtension
"
" Given the filename, returns just the file name without the path or extension
"
function! s:FSGetFileNameWithoutExtension(filename)
    return expand(a:filename . ':t:r')
endfunction

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
    let cmd = 'rel'
    let directive = parts[0]
    if len(parts) == 2
        let cmd = parts[0]
        let directive = parts[1]
    endif
    if cmd == 'reg' || cmd == 'ifrel' || cmd == 'ifabs'
        if strlen(directive) < 3
            throw 'Bad directive "' . a:location . '".'
        else
            let separator = strpart(directive, 0, 1)
            let dirparts = split(strpart(directive, 1), separator)
            if len(dirparts) < 2 || len(dirparts) > 3
                throw 'Bad directive "' . a:location . '".'
            else
                let part1 = dirparts[0]
                let part2 = dirparts[1]
                let flags = ''
                if len(dirparts) == 3
                    let flags = dirparts[2]
                endif
                if cmd == 'reg'
                    if a:mustmatch == 1 && match(a:filepath, part1) == -1
                        let path = ""
                    else
                        let path = substitute(a:filepath, part1, part2, flags) . s:os_slash .
                                    \ a:filename . '.' . a:newextension
                    endif
                elseif cmd == 'ifrel'
                    if match(a:filepath, part1) == -1
                        let path = ""
                    else
                        let path = a:filepath . s:os_slash . part2 . 
                                     \ s:os_slash . a:filename . '.' . a:newextension
                    endif
                elseif cmd == 'ifabs'
                    if match(a:filepath, part1) == -1
                        let path = ""
                    else
                        let path = part2 . s:os_slash . a:filename . '.' . a:newextension
                    endif
                endif
            endif
        endif
    elseif cmd == 'rel'
        let path = a:filepath . s:os_slash . directive . s:os_slash . a:filename . '.' . a:newextension
    elseif cmd == 'abs'
        let path = directive . s:os_slash . a:filename . '.' . a:newextension
    endif

    return simplify(path)
endfunction

"
" s:FSReturnCompanionFilename
"
" This function will return a path that is the best candidate for the companion
" file to switch to.  If mustBeReadable == 1 when then the companion file will
" only be returned if it is readable on the filesystem, otherwise it will be
" returned so long as it is non-empty.
"
function! s:FSReturnCompanionFilename(filename, mustBeReadable)
    let fullpath = s:FSGetFullPathToDirectory(a:filename)
    let ext = s:FSGetFileExtension(a:filename)
    let justfile = s:FSGetFileNameWithoutExtension(a:filename)
    let extensions = s:FSGetExtensions()
    let locations = s:FSGetLocations()
    let mustmatch = s:FSGetMustMatch()
    let newpath = ''
    for currentExt in extensions
        for loc in locations
            let newpath = s:FSGetAlternateFilename(fullpath, justfile, currentExt, loc, mustmatch)
            if a:mustBeReadable == 0 && newpath != ''
                return newpath
            elseif a:mustBeReadable == 1
                let newpath = glob(newpath)
                if filereadable(newpath)
                    return newpath
                endif
            endif
        endfor
    endfor

    return newpath
endfunction

"
" FSReturnReadableCompanionFilename
"
" This function will return a path that is the best candidate for the companion
" file to switch to, so long as that file actually exists on the filesystem and
" is readable.
" 
function! FSReturnReadableCompanionFilename(filename)
    return s:FSReturnCompanionFilename(a:filename, 1)
endfunction

"
" FSReturnCompanionFilenameString
"
" This function will return a path that is the best candidate for the companion
" file to switch to.  The file does not need to actually exist on the
" filesystem in order to qualify as a proper companion.
"
function! FSReturnCompanionFilenameString(filename)
    return s:FSReturnCompanionFilename(a:filename, 0)
endfunction

"
" FSwitch
"
" This is the only externally accessible function and is what we use to switch
" to the alternate file.
"
function! FSwitch(filename, precmd)
    if !exists("b:fswitchdst") || strlen(b:fswitchdst) == 0
        throw 'b:fswitchdst not set - read :help fswitch'
    endif
    if (!exists("b:fswitchlocs")   || strlen(b:fswitchlocs) == 0) &&
     \ (!exists("b:fsdisablegloc") || b:fsdisablegloc == 0)
        throw "There are no locations defined (see :h fswitchlocs and :h fsdisablegloc)"
    endif
    let newpath = FSReturnReadableCompanionFilename(a:filename)
    let openfile = 1
    if !filereadable(newpath)
        if exists("b:fsnonewfiles") || exists("g:fsnonewfiles")
            let openfile = 0
        else
            let newpath = FSReturnCompanionFilenameString(a:filename)
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
    au BufEnter *.h call s:SetVariables('cpp,c', 'reg:/include/src/,reg:/include.*/src/,ifrel:|/include/|../src|')
    au BufEnter *.c,*.cpp call s:SetVariables('h', 'reg:/src/include/,reg:|src|include/**|,ifrel:|/src/|../include|')
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

