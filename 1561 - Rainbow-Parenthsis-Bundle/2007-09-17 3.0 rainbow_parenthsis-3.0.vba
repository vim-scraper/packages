" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
rainbow_parenthsis_options.vim	[[[1
53
"------------------------------------------------------------------------------
"  Description: Options setable by the rainbow_parenthsis plugin
"	   $Id: rainbow_parenthsis_options.vim 773 2007-09-17 08:58:57Z krischik $
"    Copyright: Copyright (C) 2006 Martin Krischik
"   Maintainer:	Martin Krischik (krischik@users.sourceforge.net)
"      $Author: krischik $
"	 $Date: 2007-09-17 10:58:57 +0200 (Mo, 17 Sep 2007) $
"      Version: 3.0
"    $Revision: 773 $
"     $HeadURL: https://gnuada.svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/rainbow_parenthsis_options.vim $
"      History:	17.11.2006 MK rainbow_parenthsis_Options
"               01.01.2007 MK Bug fixing
"	 Usage: copy content into your .vimrc and change options to your
"		likeing.
"    Help Page: rainbow_parenthsis.txt
"------------------------------------------------------------------------------

echoerr 'It is suggested to copy the content of ada_options into .vimrc!'
finish " 1}}}

" Section: rainbow_parenthsis options {{{1

" }}}1

" Section: Vimball options {{{1
:set noexpandtab fileformat=unix encoding=utf-8
:.+2,.+4 MkVimball rainbow_parenthsis-3.0.vba

rainbow_parenthsis_options.vim
autoload/rainbow_parenthsis.vim
plugin/rainbow_parenthsis.txt


" }}}1

" Section: Tar options {{{1

tar --create --bzip2		 \
   --file="rainbow_parenthsis-3.0.tar.bz2"	 \
   rainbow_parenthsis_options.vim		 \
   autoload/rainbow_parenthsis.vim		 \
   plugin/rainbow_parenthsis.vim		 ;

" }}}1

"------------------------------------------------------------------------------
"   Copyright (C) 2006	Martin Krischik
"
"   Vim is Charityware - see ":help license" or uganda.txt for licence derainbow_parenthsiss.
"------------------------------------------------------------------------------
" vim: textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 noexpandtab
" vim: foldmethod=marker

autoload/rainbow_parenthsis.vim	[[[1
106
"------------------------------------------------------------------------------
"  Description: Rainbow colors for parenthsis
"          $Id: rainbow_parenthsis.vim 773 2007-09-17 08:58:57Z krischik $
"    Copyright: Copyright (C) 2007 Martin Krischik
"   Maintainer: Martin Krischik (krischik@users.sourceforge.net)
"               John Gilmore
"               Luc Hermitte (hermitte@free.fr)
"      $Author: krischik $
"        $Date: 2007-09-17 10:58:57 +0200 (Mo, 17 Sep 2007) $
"      Version: 3.0
"    $Revision: 773 $
"     $HeadURL: https://gnuada.svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/autoload/rainbow_parenthsis.vim $
"      History: 24.05.2006 MK Unified Headers
"               15.10.2006 MK Bram's suggestion for runtime integration
"               06.09.2007 LH Buffer friendly (can be used in different buffers),
"                             can be toggled
"               09.09.2007 MK Use on LH's suggestion but use autoload to
"                             impove memory consumtion and startup performance
"        Usage: copy to autoload directory.
"------------------------------------------------------------------------------
" This is a simple script. It extends the syntax highlighting to
" highlight each matching set of parens in different colors, to make
" it visually obvious what matches which.
"
" Obviously, most useful when working with lisp or Ada. But it's also nice other
" times.
"------------------------------------------------------------------------------

" Section: highlight {{{1

function rainbow_parenthsis#Activate()
    highlight default level1c  ctermbg=LightGray ctermfg=brown        guibg=WhiteSmoke   guifg=RoyalBlue3
    highlight default level2c  ctermbg=LightGray ctermfg=Darkblue     guibg=WhiteSmoke   guifg=SeaGreen3
    highlight default level3c  ctermbg=LightGray ctermfg=darkgray     guibg=WhiteSmoke   guifg=DarkOrchid3
    highlight default level4c  ctermbg=LightGray ctermfg=darkgreen    guibg=WhiteSmoke   guifg=firebrick3
    highlight default level5c  ctermbg=LightGray ctermfg=darkcyan     guibg=AntiqueWhite guifg=RoyalBlue3
    highlight default level6c  ctermbg=LightGray ctermfg=darkred      guibg=AntiqueWhite guifg=SeaGreen3
    highlight default level7c  ctermbg=LightGray ctermfg=darkmagenta  guibg=AntiqueWhite guifg=DarkOrchid3
    highlight default level8c  ctermbg=LightGray ctermfg=brown        guibg=AntiqueWhite guifg=firebrick3
    highlight default level9c  ctermbg=LightGray ctermfg=gray         guibg=LemonChiffon guifg=RoyalBlue3
    highlight default level10c ctermbg=LightGray ctermfg=black        guibg=LemonChiffon guifg=SeaGreen3
    highlight default level11c ctermbg=LightGray ctermfg=darkmagenta  guibg=LemonChiffon guifg=DarkOrchid3
    highlight default level12c ctermbg=LightGray ctermfg=Darkblue     guibg=LemonChiffon guifg=firebrick3
    highlight default level13c ctermbg=LightGray ctermfg=darkgreen    guibg=AliceBlue    guifg=RoyalBlue3
    highlight default level14c ctermbg=LightGray ctermfg=darkcyan     guibg=AliceBlue    guifg=SeaGreen3
    highlight default level15c ctermbg=LightGray ctermfg=darkred      guibg=AliceBlue    guifg=DarkOrchid3
    highlight default level16c ctermbg=LightGray ctermfg=red          guibg=AliceBlue    guifg=firebrick3
    let b:rainbow_parenthesis = 1
endfunction

function rainbow_parenthsis#Clear()
    let i = 0
    while i != 16
        let i = i + 1
        exe 'highlight clear level'.i.'c'
    endwhile
    let b:rainbow_parenthesis = 0
endfunction

function rainbow_parenthsis#Toggle()
    if ! exists('b:rainbow_parenthesis')
        call rainbow_parenthsis#LoadSyntax()
    endif
    if b:rainbow_parenthesis!=0
        call rainbow_parenthsis#Clear()
    else
        call rainbow_parenthsis#Activate()
    endif
endfunction

" Section: syntax {{{1
"
" These are the regions for each pair.
" This could be improved, perhaps, by makeing them match [ and { also,
" but I'm not going to take the time to figure out haw to make the
" end pattern match only the proper type.
function rainbow_parenthsis#LoadSyntax()
    syntax region level1 matchgroup=level1c start=/(/ end=/)/ contains=TOP,level1,level2,level3,level4,level5,level6,level7,level8,level9,level10,level11,level12,level13,level14,level15, level16,NoInParens
    syntax region level2 matchgroup=level2c start=/(/ end=/)/ contains=TOP,level2,level3,level4,level5,level6,level7,level8,level9,level10,level11,level12,level13,level14,level15, level16,NoInParens
    syntax region level3 matchgroup=level3c start=/(/ end=/)/ contains=TOP,level3,level4,level5,level6,level7,level8,level9,level10,level11,level12,level13,level14,level15, level16,NoInParens
    syntax region level4 matchgroup=level4c start=/(/ end=/)/ contains=TOP,level4,level5,level6,level7,level8,level9,level10,level11,level12,level13,level14,level15, level16,NoInParens
    syntax region level5 matchgroup=level5c start=/(/ end=/)/ contains=TOP,level5,level6,level7,level8,level9,level10,level11,level12,level13,level14,level15, level16,NoInParens
    syntax region level6 matchgroup=level6c start=/(/ end=/)/ contains=TOP,level6,level7,level8,level9,level10,level11,level12,level13,level14,level15, level16,NoInParens
    syntax region level7 matchgroup=level7c start=/(/ end=/)/ contains=TOP,level7,level8,level9,level10,level11,level12,level13,level14,level15, level16,NoInParens
    syntax region level8 matchgroup=level8c start=/(/ end=/)/ contains=TOP,level8,level9,level10,level11,level12,level13,level14,level15, level16,NoInParens
    syntax region level9 matchgroup=level9c start=/(/ end=/)/ contains=TOP,level9,level10,level11,level12,level13,level14,level15, level16,NoInParens
    syntax region level10 matchgroup=level10c start=/(/ end=/)/ contains=TOP,level10,level11,level12,level13,level14,level15, level16,NoInParens
    syntax region level11 matchgroup=level11c start=/(/ end=/)/ contains=TOP,level11,level12,level13,level14,level15, level16,NoInParens
    syntax region level12 matchgroup=level12c start=/(/ end=/)/ contains=TOP,level12,level13,level14,level15, level16,NoInParens
    syntax region level13 matchgroup=level13c start=/(/ end=/)/ contains=TOP,level13,level14,level15, level16,NoInParens
    syntax region level14 matchgroup=level14c start=/(/ end=/)/ contains=TOP,level14,level15, level16,NoInParens
    syntax region level15 matchgroup=level15c start=/(/ end=/)/ contains=TOP,level15, level16,NoInParens
    syntax region level16 matchgroup=level16c start=/(/ end=/)/ contains=TOP,level16,NoInParens
    let b:rainbow_parenthesis = 0
endfunction

   " }}}1
finish

"------------------------------------------------------------------------------
"   Copyright (C) 2006  Martin Krischik
"
"   Vim is Charityware - see ":help license" or uganda.txt for licence details.
"------------------------------------------------------------------------------
" vim: textwidth=78 wrap tabstop=8 shiftwidth=4 softtabstop=4 expandtab
" vim: filetype=vim foldmethod=marker
plugin/rainbow_parenthsis.vim	[[[1
38
"------------------------------------------------------------------------------
"  Description: Rainbow colors for parenthsis
"          $Id: rainbow_parenthsis.vim 764 2007-09-10 17:55:12Z krischik $
"    Copyright: Copyright (C) 2006 Martin Krischik
"   Maintainer: Martin Krischik
"               John Gilmore
"      $Author: krischik $
"        $Date: 2007-09-10 19:55:12 +0200 (Mo, 10 Sep 2007) $
"      Version: 3.0
"    $Revision: 764 $
"     $HeadURL: http://gnuada.svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/plugin/rainbow_parenthsis.vim $
"      History: 24.05.2006 MK Unified Headers
"               15.10.2006 MK Bram's suggestion for runtime integration
"               06.09.2007 LH Buffer friendly (can be used in different buffers),
"                             can be toggled
"               09.09.2007 MK Use on LH's suggestion but use autoload to
"                             impove memory consumtion and startup performance
"        Usage: copy to plugin directory.
"------------------------------------------------------------------------------
" This is a simple script. It extends the syntax highlighting to
" highlight each matching set of parens in different colors, to make
" it visually obvious what matches which.
"
" Obviously, most useful when working with lisp or Ada. But it's also nice other
" times.
"------------------------------------------------------------------------------

command! -nargs=0 ToggleRaibowParenthesis call rainbow_parenthsis#Toggle()

finish

"------------------------------------------------------------------------------
"   Copyright (C) 2006  Martin Krischik
"
"   Vim is Charityware - see ":help license" or uganda.txt for licence details.
"------------------------------------------------------------------------------
" vim: textwidth=78 wrap tabstop=8 shiftwidth=4 softtabstop=4 expandtab
" vim: filetype=vim foldmethod=marker
