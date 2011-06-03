" latex_pt.vim
" version 2004.0
" by Helder Correia <helder (dot) correia (at) netcabo (dot) pt>
" 
" Vim plugin that replaces LaTeX portuguese codes by the real characters
" when the user opens a LaTeX source document. When the user saves the
" document, the special characters are written back in LaTeX code form.
" Some characters like '�' and '�' don't exist in portuguese but they
" are handled as well.
" 
" To install it, simply copy the script to $HOME/.vim/plugin.
" Vim will automatically load it when the user opens a LaTeX file with a ".tex" extension.
" 
" This file is distributed under the GPL license Version 2, June 1991.
" Copyright (C) 1989, 1991 Free Software Foundation, Inc.  
" 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
" Everyone is permitted to copy and distribute verbatim copies
" of this license document, but changing it is not allowed.
"
" It is based on html_portuguese plugin by Rubens Marins <rubens at windstar dot com dot br>


if exists("b:loaded_latex_pt")
    finish
endif
let b:loaded_latex_pt = 1

if has("autocmd")
augroup latex_pt
    au!
    au FileType     tex   call s:read_latex_pt()
    au BufWrite     *.tex call s:write_latex_pt()
    au BufWritePost *.tex call s:read_latex_pt()
augroup END
endif

if exists("s:loaded_latex_pt_functions")
  finish
endif
let s:loaded_latex_pt_functions = 1


function s:read_latex_pt()

    let s:line = line(".")
	let s:column = col(".")

    let s:save_report = &report
    set report=99999

    %s/\\\~a/�/eIg
    %s/\\\^a/�/eIg
    %s/\\\'a/�/eIg
    %s/\\\`a/�/eIg
    %s/\\\~A/�/eIg
    %s/\\\^A/�/eIg
    %s/\\\'A/�/eIg
    %s/\\\`A/�/eIg

    %s/\\\^e/�/eIg
    %s/\\\'e/�/eIg
    %s/\\\`e/�/eIg
    %s/\\\^E/�/eIg
    %s/\\\'E/�/eIg
    %s/\\\`E/�/eIg

    %s/\\\'{\\i}/�/eIg
    %s/\\\`{\\i}/�/eIg
    %s/\\\^{\\i}/�/eIg
    %s/\\\'I/�/eIg
    %s/\\\`I/�/eIg
    %s/\\\^I/�/eIg

    %s/\\\~o/�/eIg
    %s/\\\^o/�/eIg
    %s/\\\'o/�/eIg
    %s/\\\`o/�/eIg
    %s/\\\~O/�/eIg
    %s/\\\^O/�/eIg
    %s/\\\'O/�/eIg
    %s/\\\`O/�/eIg

    %s/\\\^u/�/eIg
    %s/\\\'u/�/eIg
    %s/\\\`u/�/eIg
    %s/\\\^U/�/eIg
    %s/\\\'U/�/eIg
    %s/\\\`U/�/eIg

    %s/\\c c/�/eIg
    %s/\\c C/�/eIg

    let &report=s:save_report
    unlet s:save_report
    call cursor(s:line,s:column)
    unlet s:line
    unlet s:column

endfunction


function s:write_latex_pt()
	let s:line = line(".")
	let s:column = col(".")
    let s:save_report = &report
    set report=99999

    %s/�/\\\~a/eIg
    %s/�/\\\^a/eIg
    %s/�/\\\'a/eIg
    %s/�/\\\`a/eIg
    %s/�/\\\~A/eIg
    %s/�/\\\^A/eIg
    %s/�/\\\'A/eIg
    %s/�/\\\`A/eIg

    %s/�/\\\^e/eIg
    %s/�/\\\'e/eIg
    %s/�/\\\`e/eIg
    %s/�/\\\^E/eIg
    %s/�/\\\'E/eIg
    %s/�/\\\`E/eIg

    %s/�/\\\'{\\i}/eIg
    %s/�/\\\`{\\i}/eIg
    %s/�/\\\^{\\i}/eIg
    %s/�/\\\'I/eIg
    %s/�/\\\`I/eIg
    %s/�/\\\^I/eIg

    %s/�/\\\~o/eIg
    %s/�/\\\^o/eIg
    %s/�/\\\'o/eIg
    %s/�/\\\`o/eIg
    %s/�/\\\~O/eIg
    %s/�/\\\^O/eIg
    %s/�/\\\'O/eIg
    %s/�/\\\`O/eIg

    %s/�/\\\^u/eIg
    %s/�/\\\'u/eIg
    %s/�/\\\`u/eIg
    %s/�/\\\^U/eIg
    %s/�/\\\'U/eIg
    %s/�/\\\`U/eIg

    %s/�/\\c c/eIg
    %s/�/\\c C/eIg

    let &report=s:save_report
    unlet s:save_report
    call cursor(s:line,s:column)
    unlet s:line
    unlet s:column

endfunction

