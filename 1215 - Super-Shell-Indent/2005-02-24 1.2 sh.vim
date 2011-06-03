"
" Script:
"
"   Super Shell indentation
"
" Version: 1.2
"
" Description: 
"
"   This is a shell indentation script that calculates the indent level based
"   on begin/end syntax pairs, as opposed to complex conditionals and pattern
"   matching.
"
"   This script also introduces indentation for:
"
"           case blocks 
"           paren blocks
"           '[[' blocks
"           line continuation
"
" Installation:
"
"   Place this file in your home directory under ~/.vim/indent/, or replace
"   the system indent/sh.vim file to affect all users.
"
" Maintainer: Tye Z. <zdro@yahoo.com>
"
" Customization:
"
"   The only thing that can really be customized at this point is whether or
"   not a line is echoed explaining the indentation result.  To turn this on,
"   set the following variable like so:
"
"       let b:super_sh_indent_echo = 1
"
"
" History:
"
"
"   Version 1.2:
"       - Ignore open/close pairs that are in strings
"       - Fixed super_sh_indent_echo
"       - Fixed else/elif
"
"

let SuperShIndent_Dbg = 0

" Only load this indent file when no other was loaded.
if exists("b:did_indent") && ! SuperShIndent_Dbg
  finish
endif

let b:did_indent = 1

setlocal indentexpr=SuperShIndent()
setlocal indentkeys+==then,=do,=else,=elif,=esac,=fi,=fin,=fil,=done
setlocal indentkeys-=:,0#

" Only define the function once.
if exists("*SuperShIndent") && ! SuperShIndent_Dbg
  finish
endif

setlocal indentexpr=SuperShIndent()


function! SuperShIndent()

    let adj = 0  " Adjustment

    let g:lastindent = ""
    let currline = getline(v:lnum)
    let lastline = ''
    let prevline = ''

    " Find non-blank lines above the current line.
    let lastlnum = prevnonblank(v:lnum - 1)
    let prevlnum = prevnonblank(lastlnum - 1)
    if lastlnum != 0 
        let lastline = getline(lastlnum)
    endif
    if prevlnum != 0
        let prevline = getline(prevlnum)
    endif

    " Start from the first char on the line.  Vim doesn't seem to consistently
    " place the cursor there before calling the indent routines.
    call cursor(0, 1)
    call search('\S')

    "
    " Call indentation adjustment functions.
    "
    let adj = adj + GetPairIndent(currline, lastline, lastlnum, '(', '', ')')
    let adj = adj + GetPairIndent(currline, lastline, lastlnum, '\[\[', '', ']]')
    let adj = adj + GetCaseIndent(currline, lastline, lastlnum)
    let adj = adj + GetContIndent(currline, lastline, prevline)

    let adj = adj + GetPairIndent(currline, lastline, lastlnum,
                \ '{', '', '}')
    let adj = adj + GetPairIndent(currline, lastline, lastlnum,
                \ '\<do\>', '', '\<done\>')
    let adj = adj + GetPairIndent(currline, lastline, lastlnum,
                \ '\<case\>', '', '\<esac\>')
    let adj = adj + GetPairIndent(currline, lastline, lastlnum,
                \ '\<then\>', '\<else\>', '\(\<elif\>\|\<fi\>\)')

    " Get default indent and add our adjustment
    let prevind = indent(lastlnum)

    if b:super_sh_indent_echo
        echom g:lastindent
    endif

    return adj + prevind

endfunction

"
" Get additional indentation based on blocks of code, as defined by the Head
" and Tail patterns.
"
function! GetPairIndent(CurrLine, LastLine, LastLNum, Head, Mid, Tail)

    let levels = 0
    let adj = 0
    let origcol = col(".")
    let origline = line(".")

    " How many levels were started on the last line?  Search backwards for
    " pair starters until we're not on the last nonblank.
    while 1
        let pairstart = searchpair(a:Head, a:Mid, a:Tail, 'Wb')
        if pairstart == 0 || pairstart != a:LastLNum
            break
        endif
        let syn = synIDattr(synID(line("."), col("."), 1), "name")
        if syn =~ 'shComment\|sh\(Single\|Double\)Quote'
            break
        endif
        let levels = levels + 1
    endwhile

    " If we aren't within a level that was started on the last line, then
    " check how many levels were closed on the last line.
    if levels == 0

        " Move to the beginning of the last line
        call cursor(a:LastLNum,0)
        normal ^

        " If the line starts with an open, The close shouldn't be counted as
        " such, because we're looking for closes that didn't start on this
        " line.
        if a:LastLine =~ '^\s*' . a:Head || 
                    \ (a:Mid != '' && a:LastLine =~ '^\s*' . a:Mid)
            let levels = 1
        endif

        " If we're calculating close parens and this is a new case condition,
        " then the next close is the end of the conditional and we don't want
        " to count it.
        if a:Tail == ')'
            if "shCaseEsac" == synIDattr(synID(line("."), col("."), 1), "name")
                call searchpair(a:Head, a:Mid, a:Tail, 'W')
            endif
        endif

        " Count the closes on the current line (i.e. LastLNum), ignoring those
        " that occur w/in a comment
        while 1
            if a:LastLNum != searchpair(a:Head, a:Mid, a:Tail, 'W')
                break
            endif
            let syn = synIDattr(synID(line("."), col("."), 1), "name")
            if syn =~ 'shComment\|sh\(Single\|Double\)Quote'
                break
            endif
            let levels = levels - 1
        endwhile
    endif

    " If the current line starts with a close, count it.  It won't effect the
    " indentation of the next line because it is the first thing on the line
    " and won't be counted as a "close on the last line".
    if a:CurrLine =~ '^\s*' . a:Tail 
                \ || (a:Mid != '' && a:CurrLine =~ '^\s*' . a:Mid)
        let levels = levels - 1
    endif

    " Restore original cursor location
    call cursor(origline, origcol)

    let adj = &sw*levels
    if adj != 0 && b:super_sh_indent_echo
        let g:lastindent = g:lastindent . 
                    \ "GetPairIndent(" . a:Head . "):" . adj . " "
    endif

    return adj

endfunction


"
" Get the proper indentation when inside a case ... esac block.  New case
" conditionals are lined up with the initial "case" and their statements are
" indented.
"
function! GetCaseIndent(CurrLine, LastLine, LastLNum)

    let adj = 0
    let syn = ""
    let lastsyn = ""

    " Get the syntax attribute for the previous two lines, unless they are
    " 'esac' lines.  'esac' is a special case, because it registers as
    " 'shCaseEsac', but indentation is decreased by stock indentation.
    if a:CurrLine !~ '^\s*esac'
        let syn = synIDattr(synID(line("."), 1, 1), "name")
    endif
    if a:LastLine !~ '^\s*esac'
        let lastsyn = synIDattr(synID(a:LastLNum, 1, 1), "name")
    endif

    " If this is a new case and the last line was body of the previous case,
    " decrease the indent.  Otherwise, if the last line was a new case and
    " this line is its body, increase the indent.
    if syn == "shCaseEsac" && lastsyn != 'shCaseEsac'
        let adj = adj - &sw
    elseif lastsyn == 'shCaseEsac' && syn != 'shCaseEsac'
        let adj = adj + &sw
    endif

    "
    " Return the result
    "
    if adj != 0 && b:super_sh_indent_echo
        let g:lastindent = g:lastindent . "GetCaseIndent:" . adj . " "
    endif
    return adj

endfunction

" 
" Get indentation offset for continuation lines.
"
function! GetContIndent(CurrLine, LastLine, prevline)

    let adj = 0

    " Increase indent when previous line ends with '\', a pipe ('|'), or a
    " conditional ('&&', '||').
    let ContRE = '\(\\\||\s*\|&&\s*\)$'

    " If this is the first continued line, increase the indent.  If this is
    " the first non-continued line, decrease the indent.  Take into account
    " open brackets at the start of the current and last line.
    if a:LastLine =~ ContRE && a:prevline !~ ContRE && a:CurrLine !~ '^\s*{'
        let adj = adj + &sw
    elseif a:LastLine !~ ContRE && a:prevline =~ ContRE && a:LastLine !~ '^\s*{'
        let adj = adj - &sw
    endif

    if adj != 0 && b:super_sh_indent_echo
        let g:lastindent = g:lastindent . "GetContIndent:" . adj . " "
    endif
    return adj

endfunction

