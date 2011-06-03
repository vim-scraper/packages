" Custom global plugin to display the Vim splash screen
" even when started with one or more editfiles.
"
" Name:			zzsplash.vim
" Version:		0.1
" Maintainer:		Tony Mechelynck <antoine.mechelynck@skynet.be
" Last Change:		Wed 30 Aug 2006
"
" Installation instructions: Just drop it in the after/plugin subdirectory of
" the directory named last in the 'runtimepath' option.

" This plugin intentionally has a name starting in "zz" so the VimEnter
" autocommand which it creates (below) is run as late as possible at the end
" of startup.


" The :intro command did not exist prior to version 5.
if version < 500
  finish
endif
" If there is no loaded buffer, we can let Vim do it.
" But the function we need was only defined in version 5.1
" (or under another name starting at 5.2, but the older name
" was retained).
if (version >= 501) && (buffer_name("") == "")
  finish
endif
" The VimEnter event was introduced in version 5.1
" Only define an autocommand if it is available
if has("autocmd") && (version >= 501)
  augroup zzsplash
    au VimEnter * intro
  augroup END
  finish
endif
" come here only if the current Vim was compiled with -eval
" or if it does not possess the VimEnter event.
" In that case we haven't done anything yet.
intro
