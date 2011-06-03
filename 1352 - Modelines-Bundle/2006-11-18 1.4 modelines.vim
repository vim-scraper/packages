"-------------------------------------------------------------------------------
"  Description: Insert modelines
"   Maintainer:	Martin Krischik
"      $Author: krischik $
"        $Date: 2006-07-28 19:54:11 +0200 (Fr, 28 Jul 2006) $
"          $Id: ada.vim 343 2006-07-28 17:54:11Z krischik $
"    Copyright: Copyright (C) 2006 Martin Krischik
" Name Of File: plugin/modelines.vim
" Last Changed: Monday, 09 May 2006
"      Version: 1.4
"    $Revision: $
"     $HeadURL: $
"        Usage: copy to plugin directory
"      History: 18.11.2006 MK Bram's suggestion for runtime integration
"		18.11.2006 MK "set: :" syntax for "/* */" languages   
"-------------------------------------------------------------------------------

if exists("s:loaded_modlines")
    finish
else
    let s:loaded_modlines=1

    "------------------------------------------------------------------------------
    "
    "   Insert Modelines with standart informationss
    "
    function <SID>Modelines_Insert ()
	let l:Line = line (".")

	call append (
	    \ l:Line + 0,
	    \ substitute (
		\ &commentstring			    ,
		\ "\%s"					    ,
		\ " vim: set"				    .
		\ (&wrap ? " " : " no")	. "wrap"	    .
		\ " tabstop="		. &tabstop	    .
		\ " shiftwidth="	. &shiftwidth	    .
		\ " softtabstop="	. &softtabstop	    .
		\ (&expandtab ? " " : " no") . "expandtab"  .
		\ " :"					    ,
		\ ""))
	call append (
	    \ l:Line + 1,
	    \ substitute (
		\ &commentstring			    ,
		\ "\%s"					    ,
		\ " vim: set"				    .
		\ " textwidth="		. &textwidth	    .
		\ " filetype="		. &filetype	    .
		\ " foldmethod="        . &foldmethod	    .
		\ (&spell ? " " : " no") . "spell"	    .
		\ " :"					    ,
		\ ""))
    endfunction

    execute "nnoremap <unique>" . escape(g:mapleader . "im" , '\') .      " :call <SID>Modelines_Insert ()<CR>"
    execute "inoremap <unique>" . escape(g:mapleader . "im" , '\') . " <C-O>:call <SID>Modelines_Insert ()<CR>"

    execute "47menu Plugin.Insert.Modelines<Tab>" . escape(g:mapleader . "im" , '\') . " :call <SID>Modelines_Insert ()<CR>"
endif

finish

"-------------------------------------------------------------------------------
" vim: set nowrap tabstop=8 shiftwidth=4 softtabstop=4 expandtab :
" vim: set textwidth=78 filetype=vim foldmethod=marker nospell :
