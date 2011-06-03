"=============================================================================
"    Copyright: Copyright (C) 2010 Niels Aan de Brugh
"               Permission is hereby granted to use and distribute this code,
"               with or without modifications, provided that this copyright
"               notice is copied with it. Like anything else that's free,
"               filtering.vim is provided *as is* and comes with no
"               warranty of any kind, either expressed or implied. In no
"               event will the copyright holder be liable for any damages
"               resulting from the use of this software.
" Name Of File: quotes.vim
"  Description: Smart Behavior of the Quotation mark based on Context
"   Maintainer: Niels Aan de Brugh (nielsadb+vim at gmail dot com)
" Last Changed: Sunday, 17 Aug 2010
"      Version: See g:quotes_version for version number.
"        Usage: This file should reside in the plugin directory and be
"               automatically sourced.
"=============================================================================

if exists("g:quotes_version") || &cp
    finish
endif
let g:quotes_version = "0.1"

function! s:FindNextString()
    let l:oldVstart = getpos("'<")
    let l:oldVend = getpos("'>")
    let l:oldCpos = getpos(".")
    silent! normal! vi"
    normal! v
    let l:stringStart = getpos("'<")
    let l:stringEnd = getpos("'>")
    call setpos(".", l:oldCpos)
    call setpos("'<", l:oldVstart)
    call setpos("'>", l:oldVend)
    " If we didn't find a string, return [].
    " Also check for "" as a special case: this shouldn't be regarded a proper string
    " since we don't consider the surrounding quotes to be part of the string.
    if l:stringStart == l:stringEnd || (l:stringStart[0] == l:stringEnd[0] && l:stringStart[1] == l:stringEnd[1] && (l:stringStart[2]) + 1 == l:stringEnd[2])
        return []
    else
        return [l:stringStart, l:stringEnd]
    end
endfunction

function! s:PositionIsInRange(pos, range)
    let l:r = len(a:range) == 2 && a:pos[0] == a:range[0][0] && a:pos[0] == a:range[1][0]
    let l:r = l:r && a:pos[1] >= a:range[0][1] && a:pos[1] <= a:range[1][1]
    let l:r = l:r && a:pos[2] >= a:range[0][2] && a:pos[2] <= a:range[1][2]
    return l:r
endfunction

function! s:CursorIsInString()
    return s:PositionIsInRange(getpos("."), s:FindNextString())
endfunction

function! s:PrecedingCharsInLine(line, char, offset)
    let l:r = 0
    while match(a:line, "^".a:char, a:offset - l:r - 1) != -1
        let l:r = l:r + 1
    endwhile
    return l:r
endfunction

function! SmartQuote()
    let l:line = getline(".")
    let l:offset = col(".") - 1
    let l:onQuote = match(l:line, "^\"", l:offset) != -1

    if s:CursorIsInString()
        " Determine if the character is already escaped.
        let l:escapeCount = s:PrecedingCharsInLine(l:line, "\\", l:offset)
        if (l:escapeCount % 2) == 0
            " The character is not escaped. Also note that the cursor cannot be
            " on the last quote of the string, since that quote is not a part
            " of the string according to s:CursorIsInString().
            " See if the cursor is just before an escaped quote.
            if match(l:line, "^\\\\\"", l:offset) != -1
                " Skip \" completely.
                normal! ll
            else
                " Put escaped quotes and put cursor in between.
                normal! i\"\"
                normal! h
            end
        else
            " The character is escaped. If there is already a quote, skip it.
            " Otherwise put a single (!) quote.
            if l:onQuote
                normal! l
            else
                normal! i"
                normal! l
            end
        end
    else
        " We are not in a string.
        " - On a quote skip it,
        " - if the previous character is a quote, put a single extra quote,
        " - otheriwse put two quotes and put cursor in between.
        if l:onQuote
            normal! l
        else
            let l:precedingQuotes = s:PrecedingCharsInLine(l:line, "\"", l:offset)
            if (l:precedingQuotes % 2) == 1
                normal! i"
            else
                normal! i""
            end
        end
    end
endfunction

function! RemapQuoteForBuffer()
    inoremap <buffer> " <C-o>:call SmartQuote()<CR>
endfunction

