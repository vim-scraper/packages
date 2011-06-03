" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/glimpse.txt	[[[1
218
*glimpse.txt* Last change: $Id: specky.txt 49 2008-08-15 13:32:40Z mahlon $

                VIM REFERENCE MANUAL    by Mahlon E. Smith


                                  VimGlimpse

A basic Glimpse interface for Vim                                     *glimpse*
==============================================================================
CONTENTS                                                      *GlimpseContents*


    1) Intro........................................|GlimpseIntro|
    2) Functionality................................|GlimpseFunctionality|
    3) Adjusting Behavior...........................|GlimpseVimrcExample|
    4) Configuration................................|GlimpseOptions|
        4.1) Glimpse keybinding.....................|g:glimpseKey|
        4.2) GlimpseIndex keybinding................|g:glimpseindexKey|
		4.3) Changing the results window size.......|g:glimpseWindowResultSize|
		4.4) Altering default flags to glimpse......|g:glimpseFlags|
		4.5) Altering flags to glimpseindex.........|g:glimpseindexFlags|
    5) Author.......................................|GlimpseAuthor|
    6) License......................................|GlimpseLicense|


==============================================================================
1. INTRO                                                         *GlimpseIntro*

Glimpse is a powerful indexing and query system, free for personal use.
	http://www.webglimpse.net

VimGlimpse is a basic Glimpse interface within Vim, based on a Vim Tip from
Jean-Rene David. 
	http://vim.wikia.com/wiki/Use_glimpse_from_within_Vim

VimGlimpse retains normal Vim |:grep| functionality, and provides
additional Glimpse features outside of what |grepprg| can allow.  By
default, VimGlimpse only searches within your current working directory,
making it ideal for near instantaneous searches across a project.


==============================================================================
2. FUNCTIONALITY                                         *GlimpseFunctionality*

Glimpse installs two new global commands, *Glimpse* and *GlimpseIndex* .


SEARCHING ~
------------------------------------------------------------------------------

If you set the |g:glimpseKey|variable to something, you can search for
the word under the cursor with a keystroke.  Otherwise, just execute the
|:Glimpse| command.  Here are some examples (shamelessly stolen from the
glimpse man page):

    :Glimpse needle haystack.h$~

		Finds all needles in all haystack.h's files.

	:let g:glimpseFlags = "-iny -2"	~
	:Glimpse Anestesiology html ~

		Outputs all occurrences of  Anestesiology  with  two  errors  in
		files with html somewhere in their full name.

	:Glimpse windsurfing;Arizona mail;1993 ~

		Finds  all lines containing windsurfing and Arizona in all files
		having `mail' and '1993' somewhere in their full name.

Results are placed into the |quickfix| window.  Additionally, the 'q' key is
bound to the window so you can close it again quickly. 


INDEXING ~
------------------------------------------------------------------------------

If you set the |g:glimpseindexKey|variable to something, you can reindex
your current working directory with a keystroke.  Otherwise, just
you can just manually execute the |:GlimpseIndex| command.

Results from an index are placed into the location window.  You can view them
via |:lopen|.

If you are reindexing a large directory, this could take a few moments.
The defaults are to produce an incremental merge -- so it should be
fairly fast, but it's a good idea to ensure your indexes exist prior to
running this command.

If you've just installed glimpse, a great way to start playing with it
immediately is to let it index your entire home directory (from the shell):

	% glimpseindex -M 10 -B -o ~ ~

If you've deleted a lot of files, re-running this command will free up space
in your indexes.  I just run it nightly from cron.


==============================================================================
3. ADJUSTING-BEHAVIOR                                     *GlimpseVimrcExample*


Here's what my config looks like. >

	let g:glimpseFlags = "-winy1"
	let g:glimpseKey = "<Leader>g"
	let g:glimpseindexKey = "<Leader>G"


With these settings, I can find the full word under the cursor with
<Leader>g, in a case insensitive 1 character fuzzy match search.

With <Leader>G, I can quickly re-index my current working directory, so
changes to my files are reflected in search results.


==============================================================================
4. CONFIGURATION-OPTIONS                                       *GlimpseOptions*

Here are all of the available configuration options.

------------------------------------------------------------------------------
4.1                                                              *g:glimpseKey*

Setting this binding enables immediate searching for the |word| that is
currently under the cursor.  There is no default. >

	let g:glimpseKey = "<Leader>g"

------------------------------------------------------------------------------
4.2                                                         *g:glimpseindexKey*


Setting this enables reindexing with a keystroke, rather than running the
|GlimpseIndex| command manually.  There is no default. >

	let g:glimpseindexKey = "<Leader>G"

------------------------------------------------------------------------------
4.3                                                 *g:glimpseWindowResultSize*

Change the height of the results window.  The default is 15 lines. >

	let g:glimpseWindowResultSize = 15

------------------------------------------------------------------------------
4.4                                                            *g:glimpseFlags*

Alter the command line flags used when executing glimpse.  If you alter
this, you should retain the '-n' flag, as the |quickfix| window relies
on that for line positioning.  The default is: >

	let g:glimpseFlags = "-iny"

See the glimpse man page for all available options.  (I find -w, -i, and
-# to be the most immediately useful.)

------------------------------------------------------------------------------
4.5                                                       *g:glimpseindexFlags*

Alter the command line flags used when executing glimpseindex.
The default is: >

	let g:glimpseindexFlags = '-M 10 -B -o'

The '-f' flag with the current working directory is always appended to this
flag list.

==============================================================================
5. AUTHOR                                                       *GlimpseAuthor*


VimGlimpse was written by Mahlon E. Smith.

    mahlon@martini.nu ~
    http://www.martini.nu/ 



==============================================================================
6. LICENSE                                                     *GlimpseLicense*


This Glimpse VIM plugin is distributed under the BSD license.
    http://www.opensource.org/licenses/bsd-license.php

>
    Copyright (c) 2009, Mahlon E. Smith <mahlon@martini.nu>
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are
    met:

        * Redistributions of source code must retain the above copyright
          notice, this list of conditions and the following disclaimer.

        * Redistributions in binary form must reproduce the above copyright
          notice, this list of conditions and the following disclaimer in the
          documentation and/or other materials provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
    TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
    PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.




vim: set noet nosta sw=4 ts=4 ft=help :

plugin/glimpse.vim	[[[1
149
" vim: set noet nosta sw=4 ts=4 fdm=marker :
"
" A basic Glimpse interface for Vim
"
" Based on a Vim Tip from Jean-Rene David.
" http://vim.wikia.com/wiki/Use_glimpse_from_within_Vim
"
" Mahlon E. Smith <mahlon@martini.nu>
" $Id: specky.vim 49 2008-08-15 13:32:40Z mahlon $
"
" Here's a tcsh alias that's nice too!
" alias g 'glimpse -winO -F `pwd` \!:1 | sed -e "s|^`pwd`/||"'
"

" Loaded check {{{
if exists( 'g:glimpse_loaded' )
    finish
endif
let g:glimpse_loaded = '$Rev: 92 $'

" }}}
" Hook up the functions to the user supplied key bindings. {{{
"
if exists( 'g:glimpseKey' )
	execute 'map ' . g:glimpseKey . ' :call <SID>Glimpse()<CR>'
endif

if exists( 'g:glimpseindexKey' )
	execute 'map ' . g:glimpseindexKey . ' :call <SID>GlimpseIndex()<CR>'
endif

" }}}
" Defaults for misc settings {{{
"
if !exists( 'g:glimpseWindowResultSize' )
	let g:glimpseWindowResultSize = 15
endif

if !exists( 'g:glimpseFlags' )
	let g:glimpseFlags = "-iny"
endif

if !exists( 'g:glimpseindexFlags' )
	let g:glimpseindexFlags = '-M 10 -B -o'
endif

if !exists( 'g:glimpseErrorFormat' )
	let g:glimpseErrorFormat = '%f: %l: %m'
endif


"}}}
" Commands {{{
"
command! -nargs=* Glimpse :call <SID>Glimpse( <f-args> )
command! GlimpseIndex :call <SID>GlimpseIndex()


"}}}
" Glimpse( search_pattern, file_pattern ) {{{
"
" Run a glimpse search using a pattern from +search_pattern+ and an optional
" +file_pattern+ to limit the breadth of the search.  Places results into the
" quickfix window.
"
function! <SID>Glimpse( ... )

	" parse command line opts
	"
	let l:search_pattern = expand("<cword>")
	let l:file_pattern   = ''
	if exists( 'a:1' )
		let l:search_pattern = a:1
	endif
	if exists( 'a:2' )
		let l:file_pattern = a:2
	endif

	" everything is based on the cwd so results are relevant to the
	" current project.
	"
	let l:cwd = getcwd()

	" save the original error format, we want to play nice with others.
	"
	let l:orig_errorformat = &errorformat
	execute 'set errorformat=' . escape( g:glimpseErrorFormat, ' ' )

	" execute the search.
	"
	let l:cmd = "glimpse " . g:glimpseFlags . " -F '" . l:cwd . ";" . l:file_pattern . "' '" . l:search_pattern . "'"
	let l:result_list = split( system( l:cmd ), "\n" )

	" no results, escape now
	"
	if ( empty(l:result_list) )
		let l:no_match_msg = "No matches for '" . l:search_pattern . "'"
		if ( l:file_pattern != '' )
			let l:no_match_msg = l:no_match_msg . " in files matching '" . l:file_pattern . "'"
		endif
		call s:err( l:no_match_msg )
		execute 'set errorformat=' . escape( l:orig_errorformat, ' ' )
		return
	endif

	" populate the quickfix window.
	"
	let l:results = ''
	for l:result in l:result_list
		" 'pretty up' the glimpse output.
		let l:result = substitute( l:result, ('^' . l:cwd . '/'), '', '' )
		let l:result = substitute( l:result, '\(: \d\+:\)\s\+', '\=submatch(1) . " "', '' )

		let l:results = l:results . l:result . "\n"
	endfor
	cgetexpr( l:results )
	execute ':copen' . g:glimpseWindowResultSize

	" quick close
    nnoremap <silent> <buffer> q :ccl<CR>

	" reset error format to the original
	execute 'set errorformat=' . escape( l:orig_errorformat, ' ' )
endfunction


" }}}
" GlimpseReindex() {{{
"
" Update local indexes for the current working directory.
" Results of the index are placed into the location list. (:lopen to view.)
"
function! <SID>GlimpseIndex()
	let l:cmd = 'glimpseindex ' . g:glimpseindexFlags . ' -f .'
	let l:cwd = getcwd()
	silent! lgetexpr( system(l:cmd) )
	call s:err( "Updated indexes for '" . l:cwd . "'" )
endfunction


" }}}
" s:err( msg ) {{{
"
" Notify user in a consistent fashion.
"
function! s:err( msg )
    echohl WarningMsg|echomsg 'glimpse: ' . a:msg|echohl None
endfunction " }}}

