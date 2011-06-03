"*****************************************************************************
"** Name:      StateExp.vim - expand statements                             **
"**                                                                         **
"** Type:      global VIM plugin for Vim 7                                  **
"**                                                                         **
"** Author:    Christian Habermann                                          **
"**            christian (at) habermann-net (point) de                      **
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
"**            tested under Windows (gvim 7.0)                              **
"**                                                                         **
"** History:   1.0.0  26. Oct. 2006                                         **
"**              initial version                                            **
"**                                                                         **
"*****************************************************************************
"** Description:                                                            **
"**   The main point this script targets to is to ease writing code.        **
"**   Just type sw<ctrl-s> and you get a complete switch(){case;break;}     **
"**   statement. But it can also be used to add a header to a letter, etc.  **
"**   What to expand and how to expand can easily be configured.            **
"**                                                                         **
"**   To invoke the statement expansion, type a short form of a             **
"**   statement and press <ctrl-s> in insert mode.                          **
"**     Memory aid: s (s)tatement expansion                                 **
"**   The statement will be expanded. By default the most important ones    **
"**   of C are defined:                                                     **
"**    short form:   expanded form:                                         **
"**                                                                         **
"**     'if'   'if (  ) { }'                                                **
"**     'ie'   'if (  ) { } else { }'                                       **
"**     'fo'   'for ( ; ; ) { }'                                            **
"**     'wh'   'while (  ) { }'                                             **
"**     'do'   'do { } while (  )'                                          **
"**     'sw'   'switch (  ) { case 0: break; default: break; }'             **
"**     'ca'   'case 0: break;'                                             **
"**     'in'   '#include ""'                                                **
"**     'is'   '#include <>'                                                **
"**     'de'   '#define '                                                   **
"**                                                                         **
"**   Press <leader>sh to get a list of defined statements.                 **
"**     Memory aid: sh (s)tatement expansion (h)elp                         **
"**                                                                         **
"**                                                                         **
"**   Installation:                                                         **
"**   =============                                                         **
"**   Just copy this script into Vim's plugin directory, for details see    **
"**   Vim help ':help plugin'.                                              **
"**                                                                         **
"**   Configuration:                                                        **
"**   =============                                                         **
"**   To configure StateExp you may set the following variables in your     **
"**   vimrc-file. If they are not set, defaults will be taken.              **
"**                                                                         **
"**   - Mapping '<Plug>sexp_MapStatementExpansion'                          **
"**    Press this key in insert mode when cursor is right to the text       **
"**    that you want to expand.                                             **
"**    default is:  <c-s>, means ctrl-s                                     **
"**       imap <silent> <unique> <c-s> <Plug>sexp_MapStatementExpansion     **
"**                                                                         **
"**   - Mapping '<Plug>sexp_MapViewHelp'                                    **
"**    Press this msp to get a list of statements that can be expanded.     **
"**    default is:  \sh                                                     **
"**       map <silent> <unique> <leader>sh <Plug>sexp_MapViewHelp           **
"**                                                                         **
"**   - Variable g:sexp_shiftWidth                                          **
"**    This variable defines how much to indent code when expanding         **
"**    statements, independent from Vim's option 'shiftwidth'.              **
"**    Default: use Vim's option 'shiftwidth'                               **
"**                                                                         **
"**   - Variable g:sexp_stats                                               **
"**    This variable defines which short forms of statements are known,     **
"**    to what to expand them and where the cursor should be placed after   **
"**    expansion.                                                           **
"**      g:sexp_stats = [                                                   **
"**                       [ 'short form', 'expanded form', relx, rely],     **
"**                          ....                                           **
"**                       [ 'short form', 'expanded form', relx, rely]      **
"**                     ]                                                   **
"**    'short form': Can be a string with any characters Vim recognizes     **
"**    as a word. See ':help <cword>' for more informations.                **
"**    This is case-sensitive, \n is not allowed.                           **
"**                                                                         **
"**    'expanded form':  The expanded statement. May contain \n and \t to   **
"**    get multi-line statements.                                           **
"**                                                                         **
"**    relx, rely:  Relative cursor position to first character after       **
"**    expansion.                                                           **
"**                                                                         **
"**    Default: By default C statements are used, see script below.         **
"**                                                                         **
"**                                                                         **
"**   Known Limitations:                                                    **
"**   ==================                                                    **
"**   This script uses register 9. If you need this register, please        **
"**   adapt this script.                                                    **
"**                                                                         **
"**                                                                         **
"**   Module prefix is sexp_                                                **
"**                                                                         **
"**   Happy viming...                                                       **
"*****************************************************************************


" before doing anything, check whether vim version is ok
if ( v:version < 700 )
    echoerr "sorry, need Vim version 7.0 or newer"
    finish
endif


" allow user to avoid loading this plugin and prevent loading twice
if exists ( "g:loaded_sexp" )
    finish
endif

let g:loaded_sexp = 1




"*****************************************************************************
"************************** C O N F I G U R A T I O N ************************
"*****************************************************************************

" the mappings:
if !hasmapto('<Plug>sexp_MapStatementExpansion')
    imap <silent> <unique> <c-s> <Plug>sexp_MapStatementExpansion
endif

imap <silent> <unique> <script> <Plug>sexp_MapStatementExpansion  <esc>:call <SID>sexp_StatementExpansion()<cr>i


if !hasmapto('<Plug>sexp_MapViewHelp')
    map <silent> <unique> <leader>sh <Plug>sexp_MapViewHelp
endif

map <silent> <unique> <script> <Plug>sexp_MapViewHelp  :call <SID>sexp_ViewHelp()<cr>



" set value for indentation; if not defined by user, use 'shiftwidth'
if !exists ( 'g:sexp_shiftWidth' )
    let g:sexp_shiftWidth = &shiftwidth
endif



" The following list defines the statements and how they are expanded.
" Format of structure is:  [ short form,  expanded form, offset x, offset y ]
" short form:  What is detected as a statement to be expanded. A string that
"              Vim recognizes as a 'word'. See ':help <cword>' for details.
"              This is case-sensitive. \n is not allowed.
" long form:   The expanded statement. May contain \n and \t to get multi-line
"              statements.
" offset x,y:  Relative cursor position to first character after expansion.
if !exists ( 'g:sexp_stats' )
    let g:sexp_stats =   [ 
                          \    [ "if", "if (  )\n{\n\n}",                5, 0 ], 
                          \    [ "ie", "if (  )\n{\n\n}\nelse\n{\n\n}",  5, 0 ], 
                          \    [ "fo", "for ( ; ; )\n{\n\n}",            6, 0 ], 
                          \    [ "wh", "while (  )\n{\n\n}",             8, 0 ], 
                          \    [ "do", "do\n{\n\n} while (  )",         10, 3 ], 
                          \    [ "sw", "switch (  )\n{\n\tcase 0:\n\t\tbreak;\n\tdefault:\n\t\tbreak;\n}", 9, 0 ],
                          \    [ "ca", "case 0:\n\tbreak;",              6, 0 ],
                          \    [ "in", "#include \"\"",                 10, 0 ],  
                          \    [ "is", "#include <>",                   10, 0 ],  
                          \    [ "de", "#define ",                       8, 0 ]
                          \ ]
endif



"*****************************************************************************
"************************* I N I T I A L I S A T I O N ***********************
"*****************************************************************************

  " used to print/echo name of script
let s:scriptName = "StateExp"

" used to store number of buffer showing help 
" set it to impossible value
let s:viewBufNr = -1 



"*****************************************************************************
"****************** I N T E R F A C E  T O  C O R E **************************
"*****************************************************************************

"*****************************************************************************
"** input:   ---                                                            **
"** output:  ---                                                            **
"** remarks:                                                                **
"**   Expand the short form of a statement.                                 **
"*****************************************************************************
function <SID>sexp_StatementExpansion()
    let shiftWidthBackup = 0
    let statementFound   = 0

    let shiftWidthBackup = &shiftwidth
    let &shiftwidth      = g:sexp_shiftWidth
    
    let szStatement = expand( "<cword>" )      " get word under cursor

    for item in g:sexp_stats       " search in statement list for the short form of a statement
        if ( item[ 0 ] == szStatement )    " short form found?
                                           
            "*** remove short form to replace it with the long one
            "*** go into visual mode, select characters and delete them
            execute "normal v"                                    
            call cursor( 0, col( "." ) - len( item[ 0 ] ) + 1 )
            execute "normal \"_x"

            "*** copy long form statement into a register and paste it into Vim's buffer
            let @9 = item[ 1 ]
            execute "normal \"9P"


            "*** Now indent the expanded statement. We let do this Vim using the '=' command.
            "*** Hence we have to know the number of lines we added => count '\n's in long form
            let numLines = 0       " 0 means that we have the minimum of 1 line
            let strpos   = 0       " helper to search for '\n' in string

            while ( 1 )
                let strpos = stridx( item[ 1 ], "\n", strpos )
                if ( strpos >= 0 )
                    let numLines += 1
                else
                    break
                endif
                let strpos += 1
            endwhile

            " now make the indentation
            if ( numLines > 0 )
                execute "normal  =".numLines."+"
            else
                execute "normal  =0"
            endif


            "*** and finally position the cursor
            call cursor( line( "." ) + item[ 3 ], col( "." ) + item[ 2 ] )

            let statementFound = 1
            break                    " that's it
        endif
    endfor

    if ( !statementFound )           " short form not found? => unknown statement => error
        call s:Error( 1, szStatement )
        execute "normal l"
    endif

    " restore shift width of indentation
    let &shiftwidth = shiftWidthBackup
endfunction


"*****************************************************************************
"** input:   ---                                                            **
"** output:  ---                                                            **
"** remarks:                                                                **
"**   Show help buffer in the current window.                               **
"*****************************************************************************
function <SID>sexp_ViewHelp()

    " allow modifications (necessary if called from the scripts own buffer)
    setlocal modifiable


    "open buffer for viewing character table
    call s:OpenViewBuffer()


    " define locale mappings of user interface
    call s:SetLocalKeyMappings()


    " set syntax highlighting for view
    call s:SetupSyntaxHighlighting()
    
    
    " output result
    setlocal modifiable
    
    let txt =     "\"   Statement Expander \n"
    let txt = txt."\"  ====================\n"
    let txt = txt."\"  q:quit help\n"
    let txt = txt."\"\n"
    let txt = txt."> Statements:\n"
    let txt = txt."\n"

    " print statement infos
    " in the expanded statement string \n and \t are removed to get a nice
    " layout in the help screen
    for item in g:sexp_stats
        let txt = txt."  ".item[ 0 ]
        let txt = txt."    "
        let reducedExp = substitute( item[ 1 ],  "\n", " ",  "g" )
        let reducedExp = substitute( reducedExp, "\t", "  ", "g" )
        let txt = txt.reducedExp
        let txt = txt."\n"
    endfor

    put! = txt

    setlocal nomodifiable

endfunction




"*****************************************************************************
"************************ C O R E  F U N C T I O N S *************************
"*****************************************************************************



"*****************************************************************************
"** input:   ---                                                            **
"** output:  ---                                                            **
"** remarks:                                                                **
"**   set local/temporarily key-mappings valid while viewing help           **
"*****************************************************************************
function s:SetLocalKeyMappings()
                                         " use 'q' to close view-buffer
                                         " and switch to previously used buffer
    nnoremap <buffer> <silent> q :call <SID>sexp_ExitHelpView()<cr>

endfunction



"*****************************************************************************
"** input:   ---                                                            **
"** output:  ---                                                            **
"** remarks:                                                                **
"**   set syntax-highlighting (if VIM has 'syntax')                         **
"*****************************************************************************
function s:SetupSyntaxHighlighting()

    " don't continue, if this version of VIM does not support syntax-highlighting
    if !has('syntax')
        return
    endif

    syntax match sexpStatement "^ *[a-zA-Z]*"
    syntax match sexpComment   "^\".*"
    syntax match sexpHeadline ">.*:"

    if !exists('g:sexp_syntaxHighInit')
        let g:sexp_syntaxHighInit = 0

        hi def link sexpStatement Statement
        hi def link sexpComment   Comment
        hi def link sexpHeadline  String
    endif

endfunction




"*****************************************************************************
"** input:   ---                                                            **
"** output:  ---                                                            **
"** remarks:                                                                **
"**   Open a buffer to view help.                                           **
"*****************************************************************************
function s:OpenViewBuffer()

    " save current buffer number so that we can switch back to this buffer
    " when finishing job
    " but only if the current buffer isn't already one of the own ones
    if ( s:viewBufNr != winbufnr( 0 ) )
        let s:startBufNr = winbufnr( 0 )
    endif

    " open new buffer
    execute "enew"
    
    " save buffer number used by this script to view result
    let s:viewBufNr = winbufnr( 0 )

    " buffer specific settings:
    "   - nomodifiable:     don't allow to edit this buffer
    "   - noswapfile:       we don't need a swapfile
    "   - buftype=nowrite:  buffer will not be written
    "   - bufhidden=delete: delete this buffer if it will be hidden
    "   - nowrap:           don't wrap around long lines
    "   - iabclear:         no abbreviations in insert mode
    setlocal nomodifiable
    setlocal noswapfile
    setlocal buftype=nowrite
    setlocal bufhidden=delete
    setlocal nowrap
    iabclear <buffer>

endfunction




"*****************************************************************************
"** input:   ---                                                            **
"** output:  ---                                                            **
"** remarks:                                                                **
"**   Switch to buffer in which the script was invoked. Then the view-      **
"**   buffer will be deleted automatically.                                 **
"**   If there was no buffer at start of script, delete view-buffer         **
"**   explicitly.                                                           **
"*****************************************************************************
function s:CloseViewBuffer()

    " if start and view-buffer are the same, there was no buffer when invoking script
    if (s:startBufNr != s:viewBufNr)
        exec("buffer! ".s:startBufNr)
    else
        exec("bdelete ".s:startBufNr)
    endif

endfunction



"*****************************************************************************
"** input:   ---                                                            **
"** output:  ---                                                           **
"** remarks:                                                                **
"**   Job is done. Clean up.                                                **
"*****************************************************************************
function <SID>sexp_ExitHelpView()

    call s:CloseViewBuffer()

endfunction



"*****************************************************************************
"** input:   errNr: number which defines an error (> 0)                     **
"**          text: addition text for some message, if not used, set it ""   **
"** output:  none                                                           **
"** remarks:                                                                **
"**   this function prints an error-msg                                     **
"*****************************************************************************
function s:Error( errNr, text )
    echohl WarningMsg

    if ( a:errNr == 1 )
        echo s:scriptName.": statement \"" . a:text . "\" not found"
    else
        echo s:scriptName.": unknown error occured"
    endif

    echohl None
endfunction



"*** EOF **
