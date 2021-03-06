" Vim indent file
" Language: Mako
" Author: Scott Torborg <storborg@mit.edu>
" Version: 0.1
"
" This script does more useful indenting for Mako HTML templates. It indents
" inside of control blocks, defs, etc.
"
" We'll use HTML indenting globally, python inside <% %> blocks. Inspired by
" the excellent PHP + HTML indentation files such as php.vim by Pim Snel.
"
" Known Issues: * Doesn't indent properly after lines that have an open and
"                 close element on the same line.

let sw=2    " default shiftwidth of 2 spaces

if exists("b:did_indent")
    finish
endif
let b:did_indent = 1

setlocal nosmartindent
setlocal noautoindent
setlocal nocindent
setlocal nolisp

setlocal indentexpr=GetMakoIndent()
setlocal indentkeys+=*<Return>,<>>,<bs>,end,:

" Only define the function once.
if exists("*GetMakoIndent")
    finish
endif

if exists('g:html_indent_tags')
    unlet g:html_indent_tags
endif

function IsInsidePythonBlock(startline)
    " Loop until we get a line that's either <% or %>
    let lnum = a:startline
    while getline(lnum) !~ '\(%>\|<%\)$' && lnum > 0
        let lnum = lnum - 1
    endwhile

    " lnum points to the last control. If it's a <% then we're inside an
    " embedded python block, otherwise we're not.
    return getline(lnum) =~ '<%$'
endfunction

function GetMakoIndent()
    " Find a non-empty line above the current line
    let lnum = prevnonblank(v:lnum - 1)
    
    " Hit the start of the file, use zero indent.
    if lnum == 0
        return 0
    endif
    
    let line = getline(lnum)        " last line
    let cline = getline(v:lnum)     " current line
    let pline = getline(lnum - 1)   " previous to last line
    let ind = indent(lnum)
    
    let restore_ic=&ic
    let &ic=1 " ignore case
    
    let ind = <SID>HtmlIndentSum(lnum, -1)
	let ind = <SID>HtmlIndentSum(lnum, -1)
	let ind = ind + <SID>HtmlIndentSum(v:lnum, 0)
    
	let &ic=restore_ic
	
	let ind = indent(lnum) + (&sw * ind)
    
    " Indent after %anything: or <%anything NOT ending in />
    if line =~ '^\s*%.*:\s*$' || line =~ '^\s*<%.*[^/]>$'
        let ind = ind + &sw
    endif
    
    " Unindent before %end* or </%anything
    if cline =~ '^\s*%\s*end' || cline =~ '^\s*<\/%.*'
        let ind = ind - &sw
    endif
    
    " Unindent before %else, %except, and %elif
    if cline =~ '^\s*%\s*else' || cline =~ '^\s*%\s*except' || cline =~ '^\s*%\s*elif'
        let ind = ind - &sw
    endif

    " Indent at the beginning of a python control block
    if line =~ '<%$'
        let ind = ind + &sw
    endif
    
    " Unindent at the end of the python block.
    if cline =~ '^\s*%>$'
        let scanlnum = lnum
        " Scan backwards until we find the beginning of this python block.
        while getline(scanlnum) !~ '<%$' && scanlnum > 0
            let scanlnum = scanlnum - 1
        endwhile
        let ind = indent(scanlnum)
    endif

    " If we're inside a python block and the previous line ends in a colon,
    " indent.
    if IsInsidePythonBlock(lnum - 1)
        " Indent after :
        if line =~ '\:$'
            let ind = ind + &sw
        endif
    endif
    
    return ind
endfunction


" [-- helper function to assemble tag list --]
fun! <SID>HtmlIndentPush(tag)
    if exists('g:html_indent_tags')
	let g:html_indent_tags = g:html_indent_tags.'\|'.a:tag
    else
	let g:html_indent_tags = a:tag
    endif
endfun


" [-- <ELEMENT ? - - ...> --]
call <SID>HtmlIndentPush('a')
call <SID>HtmlIndentPush('abbr')
call <SID>HtmlIndentPush('acronym')
call <SID>HtmlIndentPush('address')
call <SID>HtmlIndentPush('b')
call <SID>HtmlIndentPush('bdo')
call <SID>HtmlIndentPush('big')
call <SID>HtmlIndentPush('blockquote')
call <SID>HtmlIndentPush('button')
call <SID>HtmlIndentPush('caption')
call <SID>HtmlIndentPush('center')
call <SID>HtmlIndentPush('cite')
call <SID>HtmlIndentPush('code')
call <SID>HtmlIndentPush('colgroup')
call <SID>HtmlIndentPush('del')
call <SID>HtmlIndentPush('dfn')
call <SID>HtmlIndentPush('dir')
call <SID>HtmlIndentPush('div')
call <SID>HtmlIndentPush('dl')
call <SID>HtmlIndentPush('em')
call <SID>HtmlIndentPush('fieldset')
call <SID>HtmlIndentPush('font')
call <SID>HtmlIndentPush('form')
call <SID>HtmlIndentPush('frameset')
call <SID>HtmlIndentPush('h1')
call <SID>HtmlIndentPush('h2')
call <SID>HtmlIndentPush('h3')
call <SID>HtmlIndentPush('h4')
call <SID>HtmlIndentPush('h5')
call <SID>HtmlIndentPush('h6')
call <SID>HtmlIndentPush('i')
call <SID>HtmlIndentPush('iframe')
call <SID>HtmlIndentPush('ins')
call <SID>HtmlIndentPush('kbd')
call <SID>HtmlIndentPush('label')
call <SID>HtmlIndentPush('legend')
call <SID>HtmlIndentPush('map')
call <SID>HtmlIndentPush('menu')
call <SID>HtmlIndentPush('noframes')
call <SID>HtmlIndentPush('noscript')
call <SID>HtmlIndentPush('object')
call <SID>HtmlIndentPush('ol')
call <SID>HtmlIndentPush('optgroup')
call <SID>HtmlIndentPush('pre')
call <SID>HtmlIndentPush('q')
call <SID>HtmlIndentPush('s')
call <SID>HtmlIndentPush('samp')
call <SID>HtmlIndentPush('script')
call <SID>HtmlIndentPush('select')
call <SID>HtmlIndentPush('small')
call <SID>HtmlIndentPush('span')
call <SID>HtmlIndentPush('strong')
call <SID>HtmlIndentPush('style')
call <SID>HtmlIndentPush('sub')
call <SID>HtmlIndentPush('sup')
call <SID>HtmlIndentPush('table')
call <SID>HtmlIndentPush('textarea')
call <SID>HtmlIndentPush('title')
call <SID>HtmlIndentPush('tt')
call <SID>HtmlIndentPush('u')
call <SID>HtmlIndentPush('ul')
call <SID>HtmlIndentPush('var')

" For some reason the default HTML indentation script doesn't consider these
" elements to be worthy of indentation.
call <SID>HtmlIndentPush('p')
call <SID>HtmlIndentPush('dt')
call <SID>HtmlIndentPush('dd')


" [-- <ELEMENT ? O O ...> --]
if !exists('g:html_indent_strict')
    call <SID>HtmlIndentPush('body')
    call <SID>HtmlIndentPush('head')
    call <SID>HtmlIndentPush('html')
    call <SID>HtmlIndentPush('tbody')
endif


" [-- <ELEMENT ? O - ...> --]
if !exists('g:html_indent_strict_table')
    call <SID>HtmlIndentPush('th')
    call <SID>HtmlIndentPush('td')
    call <SID>HtmlIndentPush('tr')
    call <SID>HtmlIndentPush('tfoot')
    call <SID>HtmlIndentPush('thead')
endif

delfun <SID>HtmlIndentPush

set cpo-=C

" [-- count indent-increasing tags of line a:lnum --]
fun! <SID>HtmlIndentOpen(lnum)
    let s = substitute('x'.getline(a:lnum),
    \ '.\{-}\(\(<\)\('.g:html_indent_tags.'\)\>\)', "\1", 'g')
    let s = substitute(s, "[^\1].*$", '', '')
    return strlen(s)
endfun

" [-- count indent-decreasing tags of line a:lnum --]
fun! <SID>HtmlIndentClose(lnum)
    let s = substitute('x'.getline(a:lnum),
    \ '.\{-}\(\(<\)/\('.g:html_indent_tags.'\)\>>\)', "\1", 'g')
    let s = substitute(s, "[^\1].*$", '', '')
    return strlen(s)
endfun

" [-- count indent-increasing '{' of (java|css) line a:lnum --]
fun! <SID>HtmlIndentOpenAlt(lnum)
    return strlen(substitute(getline(a:lnum), '[^{]\+', '', 'g'))
endfun

" [-- count indent-decreasing '}' of (java|css) line a:lnum --]
fun! <SID>HtmlIndentCloseAlt(lnum)
    return strlen(substitute(getline(a:lnum), '[^}]\+', '', 'g'))
endfun

" [-- return the sum of indents respecting the syntax of a:lnum --]
fun! <SID>HtmlIndentSum(lnum, style)
    if a:style == match(getline(a:lnum), '^\s*</')
	if a:style == match(getline(a:lnum), '^\s*</\<\('.g:html_indent_tags.'\)\>')
	    let open = <SID>HtmlIndentOpen(a:lnum)
	    let close = <SID>HtmlIndentClose(a:lnum)
	    if 0 != open || 0 != close
		return open - close
	    endif
	endif
    endif
    if '' != &syntax &&
	\ synIDattr(synID(a:lnum, 1, 1), 'name') =~ '\(css\|java\).*' &&
	\ synIDattr(synID(a:lnum, strlen(getline(a:lnum)) - 1, 1), 'name')
	\ =~ '\(css\|java\).*'
	if a:style == match(getline(a:lnum), '^\s*}')
	    return <SID>HtmlIndentOpenAlt(a:lnum) - <SID>HtmlIndentCloseAlt(a:lnum)
	endif
    endif
    return 0
endfun

" vim: set ts=4 sw=4:
