" latex_pt.vim
" version 2004.0
" by Helder Correia <helder (dot) correia (at) netcabo (dot) pt>
" 
" Vim plugin that replaces LaTeX portuguese codes by the real characters
" when the user opens a LaTeX source document. When the user saves the
" document, the special characters are written back in LaTeX code form.
" Some characters like 'î' and 'ò' don't exist in portuguese but they
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

    %s/\\\~a/ã/eIg
    %s/\\\^a/â/eIg
    %s/\\\'a/á/eIg
    %s/\\\`a/à/eIg
    %s/\\\~A/Ã/eIg
    %s/\\\^A/Â/eIg
    %s/\\\'A/Á/eIg
    %s/\\\`A/À/eIg

    %s/\\\^e/ê/eIg
    %s/\\\'e/é/eIg
    %s/\\\`e/è/eIg
    %s/\\\^E/Ê/eIg
    %s/\\\'E/É/eIg
    %s/\\\`E/È/eIg

    %s/\\\'{\\i}/í/eIg
    %s/\\\`{\\i}/ì/eIg
    %s/\\\^{\\i}/î/eIg
    %s/\\\'I/Í/eIg
    %s/\\\`I/Ì/eIg
    %s/\\\^I/Î/eIg

    %s/\\\~o/õ/eIg
    %s/\\\^o/ô/eIg
    %s/\\\'o/ó/eIg
    %s/\\\`o/ò/eIg
    %s/\\\~O/Õ/eIg
    %s/\\\^O/Ô/eIg
    %s/\\\'O/Ó/eIg
    %s/\\\`O/Ò/eIg

    %s/\\\^u/û/eIg
    %s/\\\'u/ú/eIg
    %s/\\\`u/ù/eIg
    %s/\\\^U/Û/eIg
    %s/\\\'U/Ú/eIg
    %s/\\\`U/Ù/eIg

    %s/\\c c/ç/eIg
    %s/\\c C/Ç/eIg

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

    %s/ã/\\\~a/eIg
    %s/â/\\\^a/eIg
    %s/á/\\\'a/eIg
    %s/à/\\\`a/eIg
    %s/Ã/\\\~A/eIg
    %s/Â/\\\^A/eIg
    %s/Á/\\\'A/eIg
    %s/À/\\\`A/eIg

    %s/ê/\\\^e/eIg
    %s/é/\\\'e/eIg
    %s/è/\\\`e/eIg
    %s/Ê/\\\^E/eIg
    %s/É/\\\'E/eIg
    %s/È/\\\`E/eIg

    %s/í/\\\'{\\i}/eIg
    %s/ì/\\\`{\\i}/eIg
    %s/î/\\\^{\\i}/eIg
    %s/Í/\\\'I/eIg
    %s/Ì/\\\`I/eIg
    %s/Î/\\\^I/eIg

    %s/õ/\\\~o/eIg
    %s/ô/\\\^o/eIg
    %s/ó/\\\'o/eIg
    %s/ò/\\\`o/eIg
    %s/Õ/\\\~O/eIg
    %s/Ô/\\\^O/eIg
    %s/Ó/\\\'O/eIg
    %s/Ò/\\\`O/eIg

    %s/û/\\\^u/eIg
    %s/ú/\\\'u/eIg
    %s/ù/\\\`u/eIg
    %s/Û/\\\^U/eIg
    %s/Ú/\\\'U/eIg
    %s/Ù/\\\`U/eIg

    %s/ç/\\c c/eIg
    %s/Ç/\\c C/eIg

    let &report=s:save_report
    unlet s:save_report
    call cursor(s:line,s:column)
    unlet s:line
    unlet s:column

endfunction

