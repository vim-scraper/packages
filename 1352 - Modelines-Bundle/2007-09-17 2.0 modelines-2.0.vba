" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
modelines_options.vim	[[[1
54
"------------------------------------------------------------------------------
"  Description: Options setable by the Modeline bundle
"	   $Id: modeline_options.vim 772 2007-09-17 07:37:42Z krischik $
"    Copyright: Copyright (C) 2006 Martin Krischik
"   Maintainer:	Martin Krischik
"      $Author: krischik $
"	 $Date: 2007-09-17 09:37:42 +0200 (Mo, 17 Sep 2007) $
"      Version: 2.0
"    $Revision: 772 $
"     $HeadURL: https://gnuada.svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/modeline_options.vim $
"      History:	17.09.2007 MK change to bundle use NERD_Commenter when
"		              available  
"	 Usage: copy content into your .vimrc and change options to your
"		likeing.
"    Help Page: tail.txt
"------------------------------------------------------------------------------

echoerr 'It is suggested to copy the content of modeline_options into .vimrc!'
finish " 1}}}

" Section: Modelines options {{{1

   let g:mapleader	   = "<F12>"

" }}}1

" Section: Vimball options {{{1
:set noexpandtab fileformat=unix encoding=utf-8
:.+2,.+4 MkVimball modeline-2.0.vba

modeline_options.vim
plugin/modeline.vim
autoload/modeline.vim

" }}}1

" Section: Tar options {{{1

tar --create --bzip2		 \
   --file="modeline-2.0.tar.bz2" \
   modeline_options.vim		 \
   plugin/modeline.vim		 \
   autoload/modeline.vim	 ;

" }}}1

"------------------------------------------------------------------------------
"   Copyright (C) 2007	Martin Krischik
"
"   Vim is Charityware - see ":help license" or uganda.txt for licence details.
"------------------------------------------------------------------------------
" vim: textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 noexpandtab
" vim: foldmethod=marker

plugin/modelines.vim	[[[1
37
"-------------------------------------------------------------------------------
"  Description: Insert modelines
"   Maintainer:	Martin Krischik
"      $Author: krischik $
"        $Date: 2007-09-17 09:37:42 +0200 (Mo, 17 Sep 2007) $
"          $Id: modelines.vim 772 2007-09-17 07:37:42Z krischik $
"    Copyright: Copyright (C) 2006 Martin Krischik
" Name Of File: plugin/modelines.vim
" Last Changed: Monday, 09 May 2006
"      Version: 2.0
"    $Revision: 772 $
"     $HeadURL: https://gnuada.svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/plugin/modelines.vim $
"        Usage: copy to plugin directory
"      History: 18.11.2006 MK Bram's suggestion for runtime integration
"		18.11.2006 MK "set: :" syntax for "/* */" languages   
"		17.09.2007 MK change to bundle use NERD_Commenter when
"		              available  
"-------------------------------------------------------------------------------

if exists("g:loaded_modlines")
    finish
else
    let g:loaded_modlines=20

    if exists('g:mapleader')
        execute "nnoremap <unique>" . escape(g:mapleader . "im" , '\') .      " :call modelines#Insert ()<CR>"
        execute "inoremap <unique>" . escape(g:mapleader . "im" , '\') . " <C-O>:call modelines#Insert ()<CR>"

        execute "47menu Plugin.Insert.Modelines<Tab>" . escape(g:mapleader . "im" , '\') . " :call modelines#Insert ()<CR>"
    endif
endif

finish

"-------------------------------------------------------------------------------
" vim: set nowrap tabstop=8 shiftwidth=4 softtabstop=4 expandtab :
" vim: set textwidth=0 filetype=vim foldmethod=marker nospell :
autoload/modelines.vim	[[[1
94
"-------------------------------------------------------------------------------
"  Description: Insert modelines
"   Maintainer: Martin Krischik
"      $Author: krischik $
"	 $Date: 2007-09-17 10:58:57 +0200 (Mo, 17 Sep 2007) $
"	   $Id: modelines.vim 773 2007-09-17 08:58:57Z krischik $
"    Copyright: Copyright (C) 2006 Martin Krischik
" Name Of File: plugin/modelines.vim
" Last Changed: Monday, 09 May 2006
"      Version: 2.0
"    $Revision: 1.1 $
"     $HeadURL: https://gnuada.svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/plugin/modelines.vim $
"	 Usage: copy to plugin directory
"      History: 18.11.2006 MK Bram's suggestion for runtime integration
"		18.11.2006 MK "set: :" syntax for "/* */" languages   
"		17.09.2007 MK change to bundle use NERD_Commenter when
"		              available  
"-------------------------------------------------------------------------------

if version < 700
   finish
endif 

let g:loaded_modlines=1

if exists("loaded_nerd_comments")
    "------------------------------------------------------------------------------
    "
    "	Insert Modelines with standart informationss
    "
    function modelines#Insert ()
	let l:Line = line (".")

	call append (
	    \ l:Line + 0,
	    \ "vim: set"				.
	    \ (&wrap ? " " : " no")	 . "wrap"	.
	    \ " tabstop="		 . &tabstop	.
	    \ " shiftwidth="		 . &shiftwidth	.
	    \ " softtabstop="		 . &softtabstop	.
	    \ (&expandtab ? " " : " no") . "expandtab"	.
	    \ " :")
	execute l:Line + 1 . " call NERDComment(0, 'norm')"
	call append (
	    \ l:Line + 1,
	    \ "vim: set"				.
	    \ " textwidth="		. &textwidth	.
	    \ " filetype="		. &filetype	.
	    \ " foldmethod="		. &foldmethod	.
	    \ (&spell ? " " : " no")	. "spell"	.
	    \ " :")
	execute l:Line + 2 . " call NERDComment(0, 'norm')"
    endfunction
else
    "------------------------------------------------------------------------------
    "
    "	Insert Modelines with standart informationss
    "
    function modelines#Insert ()
	let l:Line = line (".")

	call append (
	    \ l:Line + 0,
	    \ substitute (
		\ &commentstring			    ,
		\ "\%s"					    ,
		\ " vim: set"				    .
		\ (&wrap ? " " : " no")	     . "wrap"	    .
		\ " tabstop="		     . &tabstop	    .
		\ " shiftwidth="	     . &shiftwidth  .
		\ " softtabstop="	     . &softtabstop .
		\ (&expandtab ? " " : " no") . "expandtab"  .
		\ " :"					    ,
		\ ""))
	call append (
	    \ l:Line + 1,
	    \ substitute (
		\ &commentstring			    ,
		\ "\%s"					    ,
		\ " vim: set"				    .
		\ " textwidth="		    . &textwidth    .
		\ " filetype="		    . &filetype	    .
		\ " foldmethod="	    . &foldmethod   .
		\ (&spell ? " " : " no")    . "spell"	    .
		\ " :"					    ,
		\ ""))
    endfunction
endif

finish

"-------------------------------------------------------------------------------
" vim: set nowrap tabstop=8 shiftwidth=4 softtabstop=4 noexpandtab :
" vim: set textwidth=0 filetype=vim foldmethod=marker nospell :
