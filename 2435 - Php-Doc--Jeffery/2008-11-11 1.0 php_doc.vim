" php_doc.vim - Php Doc
" Maintainer:   Travis Jeffery

" Exit quickly when:
" - this plugin was already loaded (or disabled)
" - when 'compatible' is set
if (exists("g:loaded_php_doc") && g:loaded_php_doc) || &cp
    finish
endif
let g:loaded_php_doc = 1

let s:cpo_save = &cpo
set cpo&vim

" Code {{{1
python << eopython
import webbrowser
import vim

def check_php_doc():
    function = vim.eval('expand("<cword>")')
    url = "http://php.net/" + function
    webbrowser.open(url)
    return None

eopython
" }}}1

" Command {{{1
command! -nargs=0 PhpDoc exec("py check_php_doc()")
noremap K :PhpDoc<CR>
" }}}1

let &cpo = s:cpo_save

" vim:set ft=vim ts=8 sw=4 sts=4:
