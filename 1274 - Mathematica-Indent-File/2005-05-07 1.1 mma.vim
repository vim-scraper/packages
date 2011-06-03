" Vim indent file
" Language:	Mathematica
" Author:	steVe <layland@wolfram.com>
" URL:		http://members.wolfram.com/layland/vim/indent/mma.vim
" Last Change:	Thu Feb 24 00:18:40 CST 2005

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
    finish
endif
let b:did_indent = 1

" [-- local settings (must come before aborting the script) --]
setlocal indentexpr=GetMmaIndent()
setlocal indentkeys+=0[,0],0(,0)

if exists("*GetMmaIndent")
    finish
endif

function GetMmaIndent()
    " Find a non-blank line above the current line.
    let lnum = prevnonblank(v:lnum - 1)
    
    " Hit the start of the file, use zero indent.
    if v:lnum == 0
        return 0
    endif
   
    " use indenting as a base 
    let ind = indent(v:lnum)
    let lnum = v:lnum

    if ind == -1
        let ind = 0
    endif
    
    " if previous line has an unmatched bracket, {, or ( indent
    " doesn't do multiple parens/blocks/etc...
    if getline(v:lnum-1) =~ '\\\@<!\(\[[^\]]*\|([^)]*\)$'
        let ind = ind+&sw
    endif

    " if this line had unmatched closing block, 
    " indent to the matching opening block
    if getline(v:lnum) =~ '[^[]*]$'
        " move to the opening bracket
        call search(']','bW')
        " and find it's partner's indent
        let ind = indent(searchpair('\[','',']','bWn'))
    endif

    if getline(v:lnum) =~ '[^(]*)$'
        call search(')','bW')
        let ind = indent(searchpair('(','',')','bWn'))
    endif
    
    return ind
endfunction

" vim: set sw=4 sts=4:
