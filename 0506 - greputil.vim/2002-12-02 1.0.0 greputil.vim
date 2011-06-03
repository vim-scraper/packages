"*****************************************************************************
"** Name:      greputil.vim - simplifies usage of 'grep'                    **
"**                                                                         **
"** Type:      global VIM plugin                                            **
"**                                                                         **
"** Author:    Christian Habermann                                          **
"**            christian (at) habermann-net (point) de                      **
"**                                                                         **
"** Copyright: (c) 2002 by Christian Habermann                              **
"**                                                                         **
"** License:   GNU General Public License 2 (GPL 2) or later                **
"**                                                                         **
"**            This program is free software; you can redistribute it       **
"**            and/or modify it under the terms of the GNU General Public   **
"**            License as published by the Free Software Foundation; either **
"**            version 2 of the License, or (at your option) any later      **
"**            version.                                                     **
"**                                                                         **
"**            This program is distributed in the hope that it will be      **
"**            useful, but WITHOUT ANY WARRANTY; without even the implied   **
"**            warrenty of MERCHANTABILITY or FITNESS FOR A PARTICULAR      **
"**            PURPOSE.                                                     **
"**            See the GNU General Public License for more details.         **
"**                                                                         **
"** Version:   1.0.0                                                        **
"**            tested under Linux and Win32, VIM 6.1                        **
"**                                                                         **
"** History:   1.0.0;  2. Dec. 2002:                                        **
"**              same as 0.2.0 but first release                            **
"**                                                                         **
"**            0.2.0;  1. Dec. 2002:                                        **
"**              - documentation rewritten                                  **
"**              - prefix of plugin changed from gut_ to gru_               **
"**              - minor renamings in code                                  **
"**              - key-mappings are now configurable                        **
"**              - g:gru_grepOptions added                                  **
"**              - g:gru_flagUseNavi added                                  **
"**              - g:gru_flagUseHistoryNavi added                           **
"**              not released                                               **
"**                                                                         **
"**            0.1.2;  3. Nov. 2002:                                        **
"**              - mapping added to prompt for search string <Leader>ga     **
"**                (means (g)rep (a)sk)                                     **
"**              - search pattern is now customizable                       **
"**                (see g:gut_searchPattern)                                **
"**              not released                                               **
"**                                                                         **
"**            0.1.1; 17. Jan. 2002:                                        **
"**              added prompt for search string if there is no word         **
"**              under cursor                                               **
"**              not released                                               **
"**                                                                         **
"**            0.1.0; 16. Jan. 2002:                                        **
"**              initial version, not released                              **
"**                                                                         **
"*****************************************************************************
"** Description:                                                            **
"**   This script simplifies usage of the :grep-command and the navigation  **
"**   through search results and search history.                            **
"**                                                                         **
"**   To start search for the word under the cursor or for the visually     **
"**   selected text press <Leader>gr. If there is no word under the cursor, **
"**   you will be prompted for a search-string. To be prompted for a        **
"**   search-string anyway pressing <Leader>ga will do it.                  **
"**                                                                         **
"**   Memory aid: <Leader>gr:  (g)(r)ep                                     **
"**               <Leader>ga:  (g)rep (a)sk for string                      **
"**                                                                         **
"**   Note: by default <Leader> is \, so press \gr, \ga to start search     **
"**                                                                         **
"**   Files will be searched in the actual directory. The file pattern      **
"**   can be defined by a global variable, it's default is                  **
"**   "*.c *.h *.cpp *.hpp" (yes, this script is primarily for              **
"**   programmers :-) )                                                     **
"**                                                                         **
"**   To show search-results open quickfix-window with :copen.              **
"**   To navigate through the occurrences press <F6> or <F7>.               **
"**   To bring back results of older or newer search-sessions press         **
"**   <S-F6> or <S-F7>.                                                     **
"**                                                                         **
"**   Summary:                                                              **
"**      <Leader>gr:  search word under cursor or visually selected text    **
"**                   in all files selected by search pattern in the        **
"**                   current directory                                     **
"**      <Leader>ga:  prompt for search string and search in all files      **
"**                   selected by search pattern in the current directory   **
"**      <F6>:   goto next occurrence                                       **
"**      <F7>:   goto previous occurence                                    **
"**      <S-F6>: open newer search-session                                  **
"**      <S-F7>: open older search-session                                  **
"**      use :copen to show list of search results                          **
"**                                                                         **
"**   This script uses VIM's quickfix window to show search results. It's   **
"**   the same window which is used to view compilation-errors after a      **
"**   ':make' command. This means, if you have already mappings to navigate **
"**   through the list of compilation-errors you don't need additional      **
"**   mappings for the navigation through search results - just do it like  **
"**   for compiling...                                                      **
"**   Therefore it's possible to disable the navigation-feature of GrepUtil **
"**   (see Configuration section for more details; g:gru_flagUseNavi,       **
"**    g:gru_useHistoryNavi).                                               **
"**                                                                         **
"**                                                                         **
"**   Installation:                                                         **
"**     To use this script copy it into your local plugin-directory         **
"**     (Unix: ~/.vim/plugin). After starting VIM this script is sourced    **
"**     automatically.                                                      **
"**     By default 'grep' is used for searching, so you need it too. It's   **
"**     available for close to all systems. Look for ports, if it is not    **
"**     installed already.                                                  **
"**                                                                         **
"**                                                                         **
"**   Configuration:                                                        **
"**     Make some settings in your .vimrc file to configure this script:    **
"**       - gru_filePattern:                                                **
"**         defines which files should be searched                          **
"**           Default is "*.c *.h *.cpp *.hpp"                              **
"**           e.g. let g:gru_filePattern = "*.c *.h *.cpp *.hpp"            **
"**                                                                         **
"**       - g:gru_grepOptions:                                              **
"**         defines additional options of 'grep'                            **
"**           Default is ""  (none)                                         **
"**           e.g. let g:gru_grepOptions = "-r"                             **
"**                                                                         **
"**       - g:gru_flagUseNavi                                               **
"**         If you don't want to let GrepUtil make mappings for navigation  **
"**         through search results, set this to 0                           **
"**         (useful if you have already mappings to do this for your        **
"**         compilation-errors - they will work for GrepUtil too).          **
"**           Default is 1  (active)                                        **
"**           e.g. let g:gru_flagUseNavi = 0                                **
"**                                                                         **
"**       - g:gru_flagUseHistoryNavi                                        **
"**         If you don't want to let GrepUtil make mappings for navigation  **
"**         through history of search sessions, set this to 0               **
"**         (useful if you have already mappings to do this for your        **
"**         compilation-errors - they will work for GrepUtil too).          **
"**           Default is 1  (active)                                        **
"**           e.g. let g:gru_flagUseHistoryNavi = 0                         **
"**                                                                         **
"**       - 'grepprg', 'grepformat'                                         **
"**         To configure grep, use these VIM-variables.                     **
"**         See                                                             **
"**           :help 'grepprg'                                               **
"**           :help 'grepformat'                                            **
"**         for further information.                                        **
"**                                                                         **
"**                                                                         **
"**   Known limitations:                                                    **
"**     - GrepUtil can't handle very well pattern containing double quotes. **
"**       The only way it works is to enter the string manually in prompt-  **
"**       mode (<Leader>ga) and set a '\' before each double quote.         **
"**     - Metacharacters of regular expressions introduced by a '\'         **
"**       (e.g. GNU grep's \(...\), \| ) will not work in visual mode. Do   **
"**       it in prompt-mode instead (<Leader>ga).                           **
"**     - VIM expands the search pattern like when used in a command-line.  **
"**       E.g. searching for "#include" will not do what you expect since   **
"**       '#' is expanded to the alternate buffer name.                     **
"**                                                                         **
"**   Known bugs:                                                           **
"**     none                                                                **
"**                                                                         **
"**   Happy vimming....                                                     **
"*****************************************************************************

" allow user to avoid loading this plugin and prevent loading twice
if exists ("loaded_greputil")
    finish
endif

let loaded_greputil = 1




"*****************************************************************************
"************************** C O N F I G U R A T I O N ************************
"*****************************************************************************


if !exists('g:gru_filePattern')         " has user set file pattern?
    let g:gru_filePattern = "*.c *.h *.cpp *.hpp"  " no => take default
endif

if !exists('g:gru_grepOptions')         " has user set additional grep-options?
    let g:gru_grepOptions = ""          " no => take default
endif

if !exists('g:gru_flagUseNavi')         " default: make mapping for navigation
    let g:gru_flagUseNavi = 1           " through search results
endif

if !exists('g:gru_flagUseHistoryNavi')  " default: make mapping for navigation
    let g:gru_flagUseHistoryNavi = 1    " through history of search sessions
endif



"*** the mappings ***
" mappings to grep
if !hasmapto('<Plug>GRU_GrepNormal')
    nmap <silent> <unique> <Leader>gr <Plug>GRU_GrepNormal
endif

if !hasmapto('<Plug>GRU_GrepVisual')
    vmap <silent> <unique> <Leader>gr <Plug>GRU_GrepVisual
endif

if !hasmapto('<Plug>GRU_GrepAsk')
    map <silent> <unique> <Leader>ga <Plug>GRU_GrepAsk
endif

    " assign mappings to functions for grepping
nmap <silent> <unique> <script> <Plug>GRU_GrepNormal  :call <SID>GRU_Grep(expand("<cword>"))<CR>
vmap <silent> <unique> <script> <Plug>GRU_GrepVisual y:call <SID>GRU_Grep("<c-r>"")<CR>
map  <silent> <unique> <script> <Plug>GRU_GrepAsk     :call <SID>GRU_Grep("")<CR>


" mappings to navigate (can be switched off)
if (g:gru_flagUseNavi)
    if !hasmapto('<Plug>GRU_GotoNext')
        map <silent> <unique> <F6> <Plug>GRU_GotoNext
    endif

    if !hasmapto('<Plug>GRU_GotoPrevious')
        map <silent> <unique> <F7> <Plug>GRU_GotoPrevious
    endif

    map <silent> <unique> <script> <Plug>GRU_GotoNext     :cnext<cr>
    map <silent> <unique> <script> <Plug>GRU_GotoPrevious :cprevious<cr>
endif


if (g:gru_flagUseHistoryNavi)
    if !hasmapto('<Plug>GRU_GotoOlder')
        map <silent> <unique> <S-F6> <Plug>GRU_GotoOlder
    endif

    if !hasmapto('<Plug>GRU_GotoNewer')
        map <silent> <unique> <S-F7> <Plug>GRU_GotoNewer
    endif

    map <silent> <unique> <script> <Plug>GRU_GotoOlder    :colder<cr>
    map <silent> <unique> <script> <Plug>GRU_GotoNewer    :cnewer<cr>
endif





"*****************************************************************************
"************************* I N I T I A L I S A T I O N ***********************
"*****************************************************************************

" nothing to be done here...



"*****************************************************************************
"************************ C O R E  F U N C T I O N S *************************
"*****************************************************************************

"*****************************************************************************
"** input:   strng:  string to be searched                                  **
"** output:  none                                                           **
"*****************************************************************************
"** remarks:                                                                **
"**   Here grep is called to search for the string in all selected files.   **
"**   - if the string is empty, prompt for it                               **
"**   - set string in quotes in order to handle strings containing spaces   **
"**   - call VIMs grep-interface                                            **
"**                                                                         **
"*****************************************************************************
function! <SID>GRU_Grep(strng)
    let s:str = a:strng

    if s:str == ""                      " is there a string to search for?
        let s:str = input("Search: ")   " no, prompt for it 
    endif

    let s:str = "\"".s:str."\""       " set search-string in quotes

    silent execute "grep " . g:gru_grepOptions . " " . s:str . " " . g:gru_filePattern
endfunction



"*** EOF ***
