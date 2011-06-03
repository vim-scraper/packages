"=============================================================================
"    Copyright: Copyright (C) 2003 Evgeny Podjachev
"               Permission is hereby granted to use and distribute this code,
"               with or without modifications, provided that this copyright
"               notice is copied with it. Like anything else that's free,
"               minibufexplorer.vim is provided *as is* and comes with no
"               warranty of any kind, either expressed or implied. In no
"               event will the copyright holder be liable for any damamges
"               resulting from the use of this software.
"
" Name Of File: swapheader.vim
"  Description: This script allows to switch between source and header files.
"   Maintainer: Evgeny Podjachev <evgeny_podjachev@mail.ru>
"          URL: 
"  Last Change: 08-23-2003
"      Version: 1.0.0
"        Usage: Copy swapheader.vim to your plugins directory. To switch between source 
"               and header files type ':call SwapHeaderFile(0)' or press <C-F6> if you like open file 
"               in same window, if you like open file in vertically split window type 
"               ':call SwapHeaderFile(1)' or <C-S-F6>.
" Global Configuration Variables:
"               defaultHeaderExtension : Sets header extension, by default '\\.h'.
"               defaultSourceExtension : Sets source extension, by default '\\.cpp'.
"               enableSwapHeaderMap : If it's equal to 1, keymapping for <C-F6> and <C-S-F6> is enabled,
"               otherwise it's disabled, by default 1.
"         TODO: Make check if both source and header files are loaded use b and sb instead find and sfind.
"=============================================================================

if exists( 'loaded_SwapHeaderFile' )
	finish
endif
let loaded_SwapHeaderFile = 1

if !exists('g:defaultHeaderExtension')
	let g:defaultHeaderExtension = "\\.h"
endif

if !exists('g:defaultSourceExtension')
	let g:defaultSourceExtension = "\\.cpp"
endif

if !exists('g:enableSwapHeaderMap')
	let g:enableSwapHeaderMap = 1
endif

if g:enableSwapHeaderMap
	noremap <C-F6>   :call SwapHeaderFile(0)<CR>
	vnoremap <C-F6>	<C-C>:call SwapHeaderFile(0)<CR>
	inoremap <C-F6>	<C-O>:call SwapHeaderFile(0)<CR>
	
	noremap <C-S-F6>   :call SwapHeaderFile(1)<CR>
	vnoremap <C-S-F6>	<C-C>:call SwapHeaderFile(1)<CR>
	inoremap <C-S-F6>	<C-O>:call SwapHeaderFile(1)<CR>
endif

fun! SwapHeaderFile(showInSplitWindow)

	let $filename = bufname("")
	
	let $is_match = matchend($filename,g:defaultHeaderExtension)
	
	if $is_match > 0
		let $filename = substitute($filename,"\\(.*\\)\\(".g:defaultHeaderExtension."\\)","\\1".g:defaultSourceExtension, "" )
	else
		let $is_match = matchend($filename,g:defaultSourceExtension)
		
		if $is_match > 0
			let $filename = substitute($filename,"\\(.*\\)\\(".g:defaultSourceExtension."\\)","\\1".g:defaultHeaderExtension,"" )
		endif
	endif
	
	if a:showInSplitWindow == 1
		sfind $filename
	else 
		find $filename
	endif

	return
endfun
