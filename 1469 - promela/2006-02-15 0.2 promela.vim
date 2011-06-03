" Vim indent file
" Language:     Promela
" Author:       Oscar Hellström (oscar@oscarh.net)
" URL:          http://personal.oscarh.net
" License:      GPL (see http://www.gnu.org/licenses/gpl.txt)
" Last Change:  2006-02-16
" Version:      0.2
"
" Changelog:
"
" Version 0.2:
" Guards look for the correct begining of block, not just the closest.
" This means that guards will have the correct indentation even if there are
" nested blocks.
"
" Version 0.1:
" First try, introducing bugs :P

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
    finish
endif
let b:did_indent = 1

setlocal indentexpr=GetPromelaIndent()
setlocal indentkeys=0{,0},0),0],!^F,o,O,e
setlocal indentkeys+==od,=fi,=->

" Only define the function once.
if exists("*GetPromelaIndent")
    finish
endif

" blocks where both opening and closing are on the same line are ignored
" in fact, an block opening have to be the first and last thing on that line
" except for whitespaces
" TODO: maybe the begining and end of blocks sholud be more flexible?
let s:blockbeg = '^\s*\(.*{\|\<if\>\|\<do\>\)\s*$'
let s:blockend = '^\s*\(}\s*$\|\<fi\>\|\<od\>\)\s*$'
let s:guard = '^\s*::.*->\s*$'

function GetPromelaIndent()

    " Find a non-blank line above the current line.
    let pnum = prevnonblank(v:lnum - 1)
    let pline = getline(pnum)

    " Hit the start of the file, use zero indent.
    if pnum == 0
       return 0
    endif

    " In most cases just keep previous indentation
    let ind = indent(pnum)

    " Add a level if previous line is the begining of a block
    if pline =~ s:blockbeg
        return ind + &sw
    endif

    " Get current line
    let line = getline(v:lnum)

    " Remove one level if the current line is the end of a block
    if line =~ s:blockend
        return indent(s:PrevMatchBlock(matchstr(line, '\(\<fi\>\|\<od\>\|}$\)')))
    endif

    " find previous guard or begining of the correct block
    if line =~ s:guard
        let cnum = prevnonblank(v:lnum - 1)
        let nc = 0
        while cnum > 0
            let line = getline(cnum)
            if line =~ s:blockbeg 
                if nc == 0 " this is the begining of the correct block
                    return indent(cnum) + &sw 
                else
                    let nc = nc - 1
                endif
            elseif line =~ s:blockend " there is a nested block above
                let nc = nc + 1    
            elseif nc == 0 && line =~ s:guard
                return indent(cnum)
            endif
            " check line before
            let cnum = prevnonblank(cnum - 1)
        endwhile
    endif

    " Add one level if previous line is a guard
    if pline =~ s:guard
        return ind + &sw
    endif

    return ind
endfunction

function s:PrevMatchBlock(end)
    if a:end == 'od'
        let begin = '\<do\>'
        let end = '\<od\>'
    elseif a:end == '}'
        let begin = '{'
        let end = '}'
    else
        let begin = '\<if\>'
        let end = '\<fi\>'
    endif
    
    call search(end, 'bW')
    return searchpair(begin, '', end, 'Wbn')
endfunction
