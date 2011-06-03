"*****************************************************************************
"** Name:      transvim.vim - look up a word in a dictionary                **
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
"** Version:   1.0.1                                                        **
"**            tested under Linux and Win32, VIM and GVIM 6.1               **
"**                                                                         **
"** History:   1.0.1  8. Dez. 2002                                          **
"**              bug-fix:                                                   **
"**                Removed mapping of <ESC> to close view of results. In    **
"**                console version of VIM this mapping leaded to close view **
"**                after pressing any cursor key - not only after <ESC>.    **
"**                Now use 'q' instead of <ESC>.                            **
"**                                                                         **
"**            1.0.0  28. Nov. 2002                                         **
"**              same as 0.1.0, but first release                           **
"**                                                                         **
"**            0.1.0  27. Nov. 2002:                                        ** 
"**              initial version, not released                              **
"**                                                                         **
"**                                                                         **
"*****************************************************************************
"** Description:                                                            **
"**   This script looks up a word/phrase in a dictionary.                   **
"**   User can press key-shortcuts to translate word under cursor, visually **
"**   selected text or to be asked for a word.                              **
"**                                                                         **
"**   Search-results are viewed in the same window in which searching was   **
"**   started. To close a view, press 'q' - TransVim will then switch       **
"**   back to the buffer comming from.                                      **
"**                                                                         **
"**   There are several possibilities to start a translation:               **
"**     1) move cursor over word to be translated and press <Leader>tr      **
"**     2) visually select word to be translated and press <Leader>tr       **
"**     3) press <Leader>ta to be prompted for a word                       **
"**     Memory aid: tr = (T)(R)anslate, ta = (T)ranslate (A)sk for word     **
"**                                                                         **
"**     Note: by default <Leader> is '\', so press \tr, \ta to start        **
"**           translation                                                   **
"**                                                                         **
"**     Keep in mind: when using a grep-like search program regular         **
"**     expressions can be used and must be taken into account when         **
"**     defining a search string.                                           **
"**                                                                         **
"**   TransVim sets the phrase to be translated in quotes ("..."). You      **
"**   don't have to do it when looking for multi-word phrases (strictly     **
"**   speaking you are even not allowed to do it, unless you break them     **
"**   with a leading '\', see limitations below).                           **
"**                                                                         **
"**   In order to prevent tremendous huge search-results, the search-       **
"**   string must be at least 3 characters long (min. length is             **
"**   configurable).                                                        **
"**                                                                         **
"**                                                                         **
"**   Installation:                                                         **
"**     To use this script copy it into your local plugin-directory         **
"**     (Unix: ~./vim/plugin). After starting VIM this script is sourced    **
"**     automatically.                                                      **
"**     In your .vimrc set path and filename of your dictionary, e.g.       **
"**       let g:trv_dictionary = "~/.vim/dictionary/ger-eng.txt"            **
"**     A dictionary is not included, see below for further information.    **
"**     By default "grep" is used for searching, so you need it too. It's   **
"**     available for close to all systems. Look for ports, if it is not    **
"**     installed already.                                                  **
"**                                                                         **
"**                                                                         **
"**   Configuration:                                                        **
"**     To configure TransVim you may set the following variables in your   **
"**     .vimrc-file. If they are not set, defaults will be taken.           **
"**                                                                         **
"**     - <Plug>TRV_TransVimVisual                                          **
"**       mapping to start translation of visually selected text            **
"**       default:                                                          **
"**          vmap <silent> <unique> <Leader>tr <Plug>TRV_TransVimVisual     **
"**                                                                         **
"**     - <Plug>TRV_TransVimNormal                                          **
"**       mapping to start translation of word under cursor                 **
"**       default:                                                          **
"**          nmap <silent> <unique> <Leader>tr <Plug>TRV_TransVimNormal     **
"**                                                                         **
"**     - <Plug>TRV_TransVimAsk                                             **
"**       mapping to prompt for word to be translated                       **
"**       default:                                                          **
"**          map <silent> <unique> <Leader>ta <Plug>TRV_TransVimAsk         **
"**                                                                         **
"**     - g:trv_dictionary                                                  **
"**       path and filename of your dictionary                              **
"**       default:                                                          **
"**         let g:trv_dictionary = "~/.vim/dictionary/ger-eng.txt"          **
"**                                                                         **
"**     - g:trv_grep                                                        **
"**       name of grep used to search words in dictionary                   **
"**       default:                                                          **
"**         let g:trv_grep = "grep"                                         **
"**                                                                         **
"**     - g:trv_grepOptions                                                 **
"**       options for grep-command                                          **
"**       default:                                                          **
"**         let g:trv_grepOptions = "-h -i"                                 **
"**         (print no filename, ignore case)                                **
"**                                                                         **
"**     - g:trv_minStrLen                                                   **
"**       Minimal string length of the word to be translated. If word is    **
"**       shorter, translation will be canceled.                            **
"**       default:                                                          **
"**         let g:trv_minStrLen = 3                                         **
"**                                                                         **
"**     The grep-variables make it possible to replace the default grep by  **
"**     other search-programs without need of changing TransVim. For        **
"**     example use agrep instead of grep which provides approximate        **
"**     pattern search to find words which you don't exactly know how to    **
"**     spell.                                                              **
"**                                                                         **
"**                                                                         **
"**   Dictionary:                                                           **
"**     Must be a plain ASCII-text file where each line contains one        **
"**     word/phrase in language 1 and it's translation into language 2.     **
"**     TransVim lists all occurences of the searched word/phrase in this   **
"**     dictionary.                                                         **
"**     A dictionary is not included but there is a plenty of free          **
"**     dictionaries available in the web.                                  **
"**     E.g.:                                                               **
"**     - gerrman-english:                                                  **
"**       dictionary out of the Ding-project (ger-eng.txt)                  **
"**       Ding is (c) by Frank Richter, 1995-2002, License GPL 2            **
"**       Online-version see http://dict.tu-chemnitz.de                     **
"**       Comment: excellent database                                       **
"**                                                                         **
"**     - others see http://www.linuks.mine.nu/dictionary/                  **
"**       They are done by Guerkan Senguen and released under GPL.          **
"**       Comment: quality unknown, I have not tested them                  **
"**                                                                         **
"**                                                                         **
"**   Known limitations:                                                    **
"**     - TransVim can't handle very well phrases containing double quotes. **
"**       The only way it works is to enter the phrase manually in prompt-  **
"**       mode (<Leader>ta) and set a '\' before each double quote.         **
"**     - Metacharacters of regular expressions introduced by a '\'         **
"**       (e.g. GNU grep's \(...\), \| ) will not work in visual mode. Do   **
"**       it in prompt-mode instead (<Leader>ta).                           **
"**                                                                         **
"**                                                                         **
"**   Known bugs:                                                           **
"**     none - well, up to now :-)                                          **
"**                                                                         **
"**                                                                         **
"**   Happy vimming....                                                     **
"*****************************************************************************

" allow user to avoid loading this plugin and prevent loading twice
if exists ("trv_transVimLoaded")
    finish
endif

let trv_transVimLoaded = 1




"*****************************************************************************
"************************** C O N F I G U R A T I O N ************************
"*****************************************************************************

" the mappings:
if !hasmapto('<Plug>TRV_TransVimVisual')
    vmap <silent> <unique> <Leader>tr <Plug>TRV_TransVimVisual
endif

if !hasmapto('<Plug>TRV_TransVimNormal')
    nmap <silent> <unique> <Leader>tr <Plug>TRV_TransVimNormal
endif

if !hasmapto('<Plug>TRV_TransVimAsk')
    map <silent> <unique> <Leader>ta <Plug>TRV_TransVimAsk
endif

vmap <silent> <unique> <script> <Plug>TRV_TransVimVisual y:call <SID>TRV_TransVimWord("<c-r>"")<CR>
nmap <silent> <unique> <script> <Plug>TRV_TransVimNormal  :call <SID>TRV_TransVimWord(expand("<cword>"))<CR>

map <silent> <unique> <script> <Plug>TRV_TransVimAsk      :call <SID>TRV_TransVimAskForWord()<CR>


if !exists('g:trv_dictionary')      " if filename of dictionary is not set by user, take default
    let g:trv_dictionary = "~/.vim/dictionary/ger-eng.txt"
endif

if !exists('g:trv_grep')            " if grep-program is not set by user, take default 
    let g:trv_grep = "grep"
endif

if !exists('g:trv_grepOptions')     " if grep-program's options are not set by user, take default
    let g:trv_grepOptions = "-h -i"
endif

if !exists('g:trv_minStrLen')       " minimum length of string too be translated
    let g:trv_minStrLen = 3
endif



"*****************************************************************************
"************************* I N I T I A L I S A T I O N ***********************
"*****************************************************************************

" used to store number of buffer showing translation result 
" set it to impossible value
let s:transVimBufNr = -1 




"*****************************************************************************
"****************** I N T E R F A C E  T O  C O R E **************************
"*****************************************************************************

"*****************************************************************************
"** this function separates plugin-core-function from user                  **
"*****************************************************************************
function <SID>TRV_TransVimWord(str)
    call s:TransVim(a:str)
endfunction


"*****************************************************************************
"** this function separates plugin-core-function from user                  **
"*****************************************************************************
function <SID>TRV_TransVimAskForWord()
    call s:TransVimAskForWord()
endfunction




"*****************************************************************************
"************************ C O R E  F U N C T I O N S *************************
"*****************************************************************************


"*****************************************************************************
"** ask for a word/phrase and translate                                     **
"*****************************************************************************
function s:TransVimAskForWord()
    let s:strng = input("Translate: ")

    call s:Translate(s:strng)
endfunction



"*****************************************************************************
"** Translate string "str".                                                 **
"** If there is no string, ask for word/phrase.                             **
"*****************************************************************************
function s:TransVim(str)

    let s:strng = a:str

    if s:strng == ""                     " is there a string to search for?
        call s:TransVimAskForWord()      " no, then prompt for it 
    else
        call s:Translate(s:strng)        " translate
    endif

endfunction



"*****************************************************************************
"** input:   str: string to be translated                                   **
"** output:  none                                                           **
"*****************************************************************************
"** remarks:                                                                **
"**   This is the function where the translation is done.                   **
"**   Steps:                                                                **
"**     - check for minimal length of search string (trv_minStrlen)         **
"**     - save number of buffer in which this script is started             **
"**       so we can switch back to this buffer, if translation is finished  **
"**     - do a 'grep' and redirect it's output into a temporary file        **
"**     - open new buffer and load search results                           **
"**     - make local key-mappings which enables quitting the view-buffer    **
"**     - configure view-buffer                                             **
"**     - add some additional infos to the search-result                    **
"**     - make buffer no modifiable                                         **
"**     - delete temporary file if we run under win32 (unix does this       **
"**       automatically when quitting vim)                                  **
"**     that's it                                                           **
"**                                                                         **
"**   The buffer loaded to view search-result will be deleted at:           **
"**     - loading another buffer into the same window (automatic)           **
"**     - user cancles view (see trv_CloseViewBuffer())                     **
"**                                                                         **
"*****************************************************************************
function s:Translate(str)

    let s:strng = "\"".a:str."\""                " set string in single quotes

    if (strlen(s:strng) < (g:trv_minStrLen + 2))   " is string long enough?
        call s:Error(1)                            " (consider quotes)
        return
    endif


    " save current buffer number so that we can switch back to this buffer
    " when finishing this translation
    " but only if the current buffer isn't already one of transvim's
    if (s:transVimBufNr != winbufnr(0))
        let s:startBufNr = winbufnr(0)
    endif

    " search phrase in dictionary and write result in temporary file
    let s:tmpfile = tempname()
    silent exe "!" . g:trv_grep ." " . g:trv_grepOptions . " " . s:strng . " " . g:trv_dictionary . " > " . s:tmpfile

    " load result into buffer 
    execute "edit " . s:tmpfile

    " save buffer number used by this script to view result
    let s:transVimBufNr = winbufnr(0)

    " define mapping to exit view-buffer
    call s:SetLocalKeyMappings()


    " buffer specific settings:
    "   - modifiable:       just temporally, after doing our jobs it will be modifiable
    "   - noswapfile:       we don't need a swapfile
    "   - buftype=nowrite:  buffer will not be written
    "   - bufhidden=delete: delete this buffer if it will be hidden
    "   - nowrap:           don't wrap around long lines
    "   - iabclear:         no abbreviations in insert mode
    setlocal modifiable
    setlocal noswapfile
    setlocal buftype=nowrite
    setlocal bufhidden=delete
    setlocal nowrap
    iabclear <buffer>

    let s:txt =         "Press 'q' to finish.\n\n"
    let s:txt = s:txt . "Translation of " . s:strng . ":\n\n"
    put! = s:txt

    setlocal nomodifiable

    " delete temp-file only if we run under windows
    " unix: on exit of vim this file is deleted automatically
    if has('win32')
        call delete(s:tmpfile) 
    endif

endfunction



"*****************************************************************************
"** input:   none                                                           **
"** output:  none                                                           **
"*****************************************************************************
"** remarks:                                                                **
"**   set local/temporally key-mappings valid while viewing result          **
"*****************************************************************************
function s:SetLocalKeyMappings()
                                         " use 'q' to close view-buffer
                                         " and switch to previously used buffer
    nnoremap <buffer> <silent> q :call <SID>trv_CloseViewBuffer()<cr>
endfunction



"*****************************************************************************
"** input:   none                                                           **
"** output:  none                                                           **
"*****************************************************************************
"** remarks:                                                                **
"**   Switch to buffer in which the translation was started. Then the view- **
"**   buffer of transvim will be deleted automatically.                     **
"**   If there was no buffer at start of translation, delete view-buffer    **
"**   explicitely.                                                          **
"*****************************************************************************
function <SID>trv_CloseViewBuffer()

    " if start and view-buffer are the same, there was no
    " buffer at start of translation
    if (s:startBufNr != s:transVimBufNr)
        exec("buffer! ".s:startBufNr)
    else
        exec("bdelete ".s:startBufNr)
    endif

endfunction



"*****************************************************************************
"** input:   errNr: number which defines an error (> 0)                     **
"** output:  none                                                           **
"*****************************************************************************
"** remarks:                                                                **
"**   this function prints an error-msg                                     **
"*****************************************************************************
function s:Error(errNr)
    
    if (a:errNr == 1)
        echo "TransVim: string too short (length must be >= ".g:trv_minStrLen." characters)"
    else
        echo "TransVim: unknown error occured"
    endif

endfunction


"*** EOF ***
