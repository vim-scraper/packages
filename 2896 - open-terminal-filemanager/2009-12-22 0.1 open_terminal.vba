" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/open_terminal.vim	[[[1
108
"
" File:        open_terminal.vim
" 
" Requires:
"   platform   Terminal             File manager 
"   Mac        Terminal.app         Finder
"              (with Applescript)   
"   Gnome      gnome-terminal       nautilus
"   KDE        konsole              konqueror
"   Windows    cmd                  explorer
"              (with start)
"   cygwin     bash                 explorer
"
" Example:
"   nnoremap <silent> <F9> :OpenTerminal<CR>
"   nnoremap <silent> <F10> :OpenFilemanager<CR><CR>
"   
" Commands:
"   OpenTerminal
"   OpenFilemanager
"

" OpenTerminal {{{1
function! s:open_terminal()
  let l:current_dir = getcwd()
  execute("chdir " . escape(expand("%:p:h"), " \"'"))

  if has("mac")
    let l:cmd = "
      \ tell application 'System Events'            \n
      \   set is_term_running to exists application process '$Terminal' \n
      \ end tell                                    \n
      \
      \ set cmd to 'cd $current_path'               \n
      \ tell application '$Terminal'                \n
      \   activate                                  \n
      \   if is_term_running is true then           \n
      \     do script with command cmd              \n
      \   else                                      \n
      \     do script with command cmd in window 1  \n
      \   end if                                    \n
      \ end tell                                    \n
      \ "
    let l:cmd = substitute(l:cmd,  "'", '\\"', 'g') 
    let l:cmd = substitute( l:cmd, "$Terminal", "Terminal", "g" )
    let l:cmd = substitute( l:cmd, "$current_path", "'" . expand("%:p:h") . "'" , "g")
    call system('osascript -e " ' . l:cmd . '"')

  elseif has("gui_gnome") && executable("gnome-terminal")
    call system("gnome-terminal &") 
  elseif has("gui_gnome") && executable("konsole")
    call system("konsole &") 
  elseif has("gui_win32")
    try
      call system("start cmd")
    catch /E484:/
      echo "Ignore E484 error in Windows platform"
    endtry
  elseif executable("bash")
    !bash
  elseif has("win32")
    stop
  endif

  execute("chdir " . escape(l:current_dir, " \"'"))
endfunction

command! -nargs=0 -bar OpenTerminal call s:open_terminal()

"}}}1

" OpenFilemanager {{{1
function! s:open_filemanager()
  let l:cmd = "$cmd ." 

  let l:current_dir = getcwd()
  execute("chdir " . escape(expand("%:p:h"), " \"'"))

  if has("mac")
    call system("open .")
  elseif has("gui_gnome") && executable("nautilus")
    call system("nautilus .")
  elseif has("gui_gnome") && executable("konqueror")
    call system("konqueror .")
  elseif has("gui_win32") || has("win32")
    call system("explorer .")
  elseif executable("bash")
    !bash
  endif

  execute("chdir " . escape(l:current_dir, " \"'"))
endfunction

command! -nargs=0 -bar OpenFilemanager call s:open_filemanager()
" }}}1

" About file info {{{1
"=============================================================================
" Copyright (c) 2009 by neocoin
" File:						open_terminal.vim
" Author:					Sangmin Ryu (neocoin@gmail.com)
" Date:						Tue Dec 22 13:33:32 PST 2009
" License:				The MIT License
" Version:				0.1
"=============================================================================
" }}}1

" vim: set fdm=marker:
doc/open_terminal.txt	[[[1
71
*open-terminal*  Open Terminal, Filemanager for various platform
       
                       Open Terminal Help File 

Introduction                                    |open-terminal-introduction|
Commmands                                       |open-terminal-commands|
Requires                                        |open-terminal-requires|
Key map examples                                |open-terminal-keymap|
Changelog                                       |open-terminal-changelog|
About                                           |open-terminal-about|

==============================================================================
Introduction                                    *open-terminal-introduction*

Some vim user want to shell like emacs's eshell. But vim don't support shell, 
terminal interface in vim (at least  7.x version). 

In this situation, I use native terminal software open script for my various
working platforms. 

==============================================================================
Commmands                                          *open-terminal-commands*

                                                            *OpenTerminal*
:OpenTerminal           Open native terminal sw. Check |requires| list

                                                           *OpenFilemanager*
:OpenFilemanager        Open native file manager. Check |requires| list


==============================================================================
Map key examples                                 *open-terminal-keymap*
>
   nnoremap <silent> <F9> :OpenTerminal<CR>
   nnoremap <silent> <F10> :OpenFilemanager<CR><CR>
<

==============================================================================
Requires (per platform)                             *open-terminal-requires*

I use Mac, Gnome, Windows(gvim). I don't test in KDE environment.

     Platform   Terminal             File manager ~
>
     Mac        Terminal.app         Finder       
                (with Applescript)                
     Gnome      gnome-terminal       nautilus     
     KDE        konsole              konqueror    
     Windows    cmd                  explorer     
                (with start)                      
     cygwin     bash                 explorer     
<
==============================================================================
Changelog                                          *open-terminal-changelog*

0.1:
  - First release used script for everyone. 


==============================================================================
About                                                  *open-terminal-about*

  Copyright (c) 2009 by neocoin ~
  File:	    open_terminal.vim
  Author:   Sangmin Ryu (neocoin@gmail.com)
  Date:	    Tue Dec 22 13:33:32 PST 2009
  License:  The MIT License

==============================================================================

 vim:tw=78:ts=8:ft=help:norl:
