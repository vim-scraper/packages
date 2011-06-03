" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
autoload/Join.vim	[[[1
130
" Join: Improved Algorithm for joining large files
"   Author:  Christian Brabandt <cb@256bit.org>
"   Version: 2.4
"   Script:  http://www.vim.org/scripts/script.php?script_id=2766
"   License: VIM License
"   Last Change: Mon, 19 Apr 2010 22:20:33 +0200


"   Documentation: see :help Join.txt
"   GetLatestVimScripts: 2766 5 :AutoInstall: Join.vim

fu! Join#Join_Wrap(flag, count) range
    " this is a guess, but I think, 1000 lines can be joined by vim
    " without problems. If you lower the limit, the plugin has to run more
    " often, if the limit is too large, it will suffer from the same
    " performance issue.
    let limit = 1000  
    let start =  a:firstline
    let end  =  a:lastline
    if !empty(a:count)
	let ccount = (a:count == 1 ? 1 : a:count - 1)
    else 
	let ccount = 1
    endif

    if (a:lastline == a:firstline)
	let end = start + ccount
    elseif (!empty(a:count) && (a:lastline != a:firstline))
	let start = a:lastline
	let end  = start + ccount
    elseif (empty(a:count) && (a:lastline == a:firstline))
	let end = start + 1
    endif

    if (end > line('$'))
      let end=line('$')
    endif

    if  ( (a:lastline - a:firstline) <= limit )
	"echo "builtin join"
	exe start . ',' . end . 'join' . ( a:flag ? '!' : ' ' ) 
    else
	"echo "custom join"
	exe start . ',' . end . "call Join#My_Join(a:flag)"
    endif
endfu


fu! Join#My_Join(bang_flag) range
    let limit=10000

    let first  = a:firstline
    let last   = a:lastline 

    let runs   = (last - first)/limit + 1
    let remain = last - first + 1

    for i in range(runs)
        let start = first + i*limit
	if (remain > limit)
	    let end = start + limit - 1
	elseif (remain <= limit && ((i > 0) || first > 1))
	    let end = start + remain - 1
	else
	    let end = remain
	endif
        call setline(i+first, join(filter(getline(start,end), 'v:val !~ "^$"') , a:bang_flag ? '' : ' '))
	let remain -=  limit
    endfor

    if (last > first)
	exe ":silent " . (runs+first) . "," . last . "d_" 
    endif
    if (runs > 1)
	exe ":" . first . "," . (first+runs-1) . "call Join#Join_Wrap(" . a:bang_flag . ", '' )"
    endif
endfu

fu! Join#Position(flag, count) range
    let oldpos=getpos('.')
    exe a:firstline . ',' . a:lastline . "call Join#Test_Joinspaces(a:flag, a:count)"
    exe a:firstline . ',' . a:lastline . "call Join#Join_Wrap(a:flag, a:count)"
    call setpos('.', oldpos)
endfu

fu! Join#Test_Joinspaces(flag, count) range
  if !a:flag
      " Check joinspaces setting and correct range
      let start = a:firstline
      let end   = a:lastline

      if !empty(a:count) && (a:count == 1)
	  let ccount = 1
      elseif !empty(a:count) && (a:count > 1)
	  let ccount = a:count - 1
      else 
	  let ccount = 1
      endif

      if (a:lastline == a:firstline)
	  let end = start + ccount
      elseif (!empty(a:count) && (a:lastline != a:firstline))
	  let start = a:lastline
	  let end  = start + ccount
      elseif (empty(a:count) && (a:lastline == a:firstline))
	  let end = start + 1
      endif

      if (end > line('$'))
	let end=line('$')
      endif

	let old = @/
	exe "silent " . start . "," . end . 's/ $//e'
	exe 'silent ' . start . ',' . end . 's/\n\().*\)$/\1\r/e'
	
	if (&js)
	  if (&cpo !~# 'j')
	      let sep = '\%(\.\|?\|!\)'
	  else
	      let sep = '\%(\.\)'
	  endif
	  exe "silent " . start . "," . end . 's/' . sep . '/& /e'
	endif
	let @/  = old
      endif
endfu


" vim: sw=2 sts=2 ts=8 fdm=marker fdl=0
doc/Join.txt	[[[1
106
*Join.txt*  Improved Algorithm for joining many lines 

Author:  Christian Brabandt <cb@256bit.org>
Version: 2.4 Mon, 19 Apr 2010 22:20:33 +0200
Copyright: (c) 2009, 2010 by Christian Brabandt 	      *Join-copyright*
           The VIM LICENSE applies to Join.vim and Join.txt
           (see |copyright|) except use Join.vim instead of "Vim".
	   NO WARRANTY, EXPRESS OR IMPLIED.  USE AT-YOUR-OWN-RISK.


==============================================================================
                                                                *:Join* *Join*
1. Functionality

This plugin tries to implement a different method for joining lines. This is,
because the :join command from within vim suffers from a serious performance
issue, if you are trying to join many lines (>1000).

This has been discussed on the vim development mailing list (see
http://thread.gmane.org/gmane.editors.vim.devel/22065) as well as on the vim
user mailing list (see http://thread.gmane.org/gmane.editors.vim/80304). 

There has also been a patch proposed, to improve the algorithm used by :join.
This patch is available at http://repo.or.cz/w/vim_extended.git, but this
means, you'll have to build and patch your vim manually (you can't use it with
a prebuilt vim).

Until this patch is accepted and incorporated into mainline vim, this plugin
tries to improve the joining algorithm by the method mentioned in the user
mailinglist above. It basically works by breaking up the join algorithm into
smaller pieces and joining the smaller pieces together. This may have an
impact on memory usage, though.

                                                              *Join-benchmark*
For reference I include some timings, joining many lines:

      Lines joined      :%join      |      :%Join
        25.000           3,305s     |      0,240s
	50.000          13,667s     |      0,336s
       100.000          64,140s     |      0,588s
       200.000         331,410s     |      1,431s
     1.000.000            -[1]      |      7,419s

[1] benchmarking was aborted after 53 Minutes (after which only about 480.000
lines have been joined).

Please also note, that using a substitute command does not prove to be faster.
It also suffers from the performance impact.
    
Also note, that really the best way to remove '\n' on a file with millions
of lines is using tr:

~$ tr -d '\n' <large_file >output_file

==============================================================================
2. Usage                                                          *Join-usage*

:[range]J[oin][!]
			Join [range] lines.  Same as "J", except with [!]
			the join does not insert or delete any spaces.
			The default behavior is to join the current line 
			with the line below it.

:[range]J[oin][!] {count}
			Join {count} lines, starting with [range] (default:
			current line |cmdline-ranges|).  Same as "J", except
			with [!] the join does not insert or delete any
			spaces.


You should be able to use :Join as drop in replacement for :join. It behaves
exactly like :join and understands it's syntax, with the exception of 1 point:

1) :Join does not accept the use of [flags] as |:join| does.
                                                                     *Join-J*

If you want the J command to call :Join, you can use something like:
:nmap J :Join<CR>
to have J call :Join in normal mode and
:vmap J :Join<CR>
to have J join the visually selected lines.

==============================================================================
                                                            *Join-differences*
3. Differences

This plugin has been made to make :Join and :join behave almost identically.
If there are further differences than those described at |Join-usage|, I am
interested at any bug describing exactly what went wrong, so I can fix this.
Please send any bug report to the mail address mentioned at the top of this
page.


==============================================================================
4. Join History					                *Join-history*
      2.4: Apr 19, 2010    Made plugin autoloadable
			   Setup public github repository at
			         http://github.com/chrisbra/Join
      2.2: Oct  2, 2009    fixed Copyright, which included wrong Plugin name
      2.1: Sep  1, 2009	   fixed Documentation, enabled GetLatestScripts
	2: Aug 31, 2009	   Documentation, fixed several bugs.
	1: Aug 27, 2009	   Internal version, First working version, 
			   using simple commands

==============================================================================
vim:tw=78:ts=8:ft=help
plugin/JoinPlugin.vim	[[[1
30
" Join: Improved Algorithm for joining large files
"   Author:  Christian Brabandt <cb@256bit.org>
"   Version: 2.4
"   Script:  http://www.vim.org/scripts/script.php?script_id=2766
"   License: VIM License
"   Last Change: Mon, 19 Apr 2010 22:20:33 +0200


"   Documentation: see :help Join.txt
"   GetLatestVimScripts: 2766 5 :AutoInstall: Join.vim

" ---------------------------------------------------------------------
" Load Once: {{{1
if exists("g:loaded_Join") || &cp
 finish
endif
let g:loaded_Join      = 1
let s:keepcpo          = &cpo
set cpo&vim


" ---------------------------------------------------------------------
" Public Interface {{{1
com! -bang -range -nargs=? Join :<line1>,<line2>call Join#Position(empty('<bang>') ? 0 : 1, <q-args>)

" =====================================================================
" Restoration And Modelines: {{{1
" vim: fdm=marker sw=2 sts=2 ts=8 fdl=0
let &cpo= s:keepcpo
unlet s:keepcpo
