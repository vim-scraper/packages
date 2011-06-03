"  This file gives some macros to help type Ruby programs.

set notimeout
set smartindent
set smarttab
set autoindent
set shiftwidth=2

noremap <buffer> K :!ruby %<CR>

nnoremap <buffer> <F1> :if strpart(getline(1),0,21) !='#!/usr/math/bin/ruby'<CR>0put ='#!/usr/math/bin/ruby'<CR>put =''<CR>endif<CR>:if getline(3) != ""<CR>1put =''<CR>endif<CR><Space>3Gi

inoremap <buffer> ;; ;
inoremap <buffer> ;e <CR><BS>end
inoremap <buffer> ;h <Space>=><Space>

inoremap <buffer> " <C-R>=<SID>Double('"','"')<CR>
inoremap <buffer> ` <C-R>=<SID>Double('`','`')<CR>
inoremap <buffer> ' <C-R>=<SID>Double("\'","\'")<CR>
inoremap <buffer> ( <C-R>=<SID>Double("(",")")<CR>
inoremap <buffer> [ <C-R>=<SID>Double("[","]")<CR>
inoremap <buffer> { <C-R>=<SID>Double("{","}")<CR>

function! s:Double(left,right)
    if strpart(getline(line(".")),col(".")-2,2) == a:left . a:right
	return "\<C-O>s"
    else
	return a:left . a:right . "\<Left>"
    endif
endfunction

vnoremap <buffer> `[ <C-C>`>a]<Esc>`<i[<Esc>
vnoremap <buffer> `( <C-C>`>a)<Esc>`<i(<Esc>
vnoremap <buffer> `{ <C-C>`>a}<Esc>`<i{<Esc>
vnoremap <buffer> `" <C-C>`>a"<Esc>`<i"<Esc>
vnoremap <buffer> `` <C-C>`>a`<Esc>`<i`<Esc>

noremap <buffer> <C-Del> :call <SID>DeleteBrackets()<CR>

function! s:DeleteBrackets()
   let s:c = getline(line("."))[col(".") - 1]
   if s:c == '{' || c == '[' || c == '('
      normal %x``x
   elseif s:c == '}' || s:c == ']' || s:c == ')'
      normal %%x``x``
   elseif s:c == '"'
      exe "normal x/\"\<CR>x``"
   endif
endfunction

" The following macros automatically insert complete various ruby items.

noremap <buffer> <C-Del> :call <SID>DeleteBrackets()<CR>

" To defuse the abbreviations.
inoremap <buffer> <C-Space> <C-V><Space>

inoremap <buffer> \| <C-R>=<SID>DoubleBars()<CR>
function! s:DoubleBars()
    if strpart(getline(line(".")),0,col(".")-2) =~ '[/#]'
	return "\|"
    else
	return "\|\|\<Left>"
    endif
endfunction

iab <buffer> def <C-R>=<SID>SpecialAbbrev("def")<CR>
" iab <buffer> else <BS>else    " Taken care of by vim 6.0 indenting.
iab <buffer> elsif <BS>elsif

iab <buffer> for <C-R>=<SID>For()<CR>
iab <buffer> if <C-R>=<SID>SpecialAbbrev("if")<CR>
iab <buffer> case <C-R>=<SID>Case()<CR>
iab <buffer> class <C-R>=<SID>SpecialAbbrev("class")<CR>
iab <buffer> module <C-R>=<SID>SpecialAbbrev("module")<CR>
iab <buffer> unless <C-R>=<SID>SpecialAbbrev("unless")<CR>
iab <buffer> until <C-R>=<SID>SpecialAbbrev("until")<CR>
iab <buffer> while <C-R>=<SID>SpecialAbbrev("while")<CR>

function! s:SpecialAbbrev(string)
    if strpart(getline(line(".")),0,col(".")-1) =~ '\S'  " Not a blank line.
	return a:string
    else 
	return a:string . "\<CR>end\<Esc>kA"
    endif
endfunction

function! s:For()
    if strpart(getline(line(".")),0,col(".")-1) =~ '\S'  " Not a blank line.
	return "for"
    else 
	return "for in \<CR>end\<Esc>k$3hi"
    endif
endfunction

function! s:Case()
    return "case\<Esc>owhen \<Esc>oend\<Esc>2kA"
endfunction

iab <buffer> do <C-R>=<SID>DoAbbrev()<CR>

function! s:DoAbbrev()
    if strpart(getline(line(".")),0,col(".")-1) =~ '^\s*\S*\s*$'
	return "do" . "\<CR>end\<Esc>kA"
    else
	return "do"
    endif
endfunction
