" Custom global plugin to display the Vim splash screen
" even when started with one or more editfiles.
"
" Name:			zzsplash.vim
" Version:		0.0
" Maintainer:		Tony Mechelynck <antoine.mechelynck@skynet.be
" Last Change:		Wed 30 Aug 2006
"
" Installation instructions: Just drop it in the after/plugin subdirectory of
" the directory named last in the 'runtimepath' option.
"
" This plugin intentionally has a name starting in "zz" so the VimEnter
" autocommand which it creates (below) is run as late as possible at the end
" of startup.
" 
" Known bugs and limitations:
" - This plugin will do nothing in Small and Tiny builds of Vim, which do not
"   possess the +autocmd feature.

if has("autocmd")
  augroup zzsplash
    au VimEnter * intro
  augroup END
endif
