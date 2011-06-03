" pyhelp.vim - Pyhelp
" Maintainer:   Travis Jeffery

" Exit quickly when:
" - this plugin was already loaded (or disabled)
" - when 'compatible' is set
if (exists("g:loaded_pyhelp") && g:loaded_pyhelp) || &cp
    finish
endif
let g:loaded_pyhelp = 1

let s:cpo_save = &cpo
set cpo&vim

" Code {{{1

python << EOPYTHON
import vim
import sys
import os
import re

def py_doc():
    cur_buf  = vim.current.buffer
    buf_name = cur_buf.name
    buf_txt  = "\n".join(cur_buf[:])
    current_word = vim.eval('expand("<cword>")')
    one_current_word = current_word.split(".")[0]
    m = re.search("%s(=| =)(.*)(\(|)" % one_current_word, buf_txt)
    if m == None:
        cmd = "help(%s)" % current_word
        return eval(cmd)
    else:
        m = m.group(2)
        m = re.sub("\(.*", "", m)    
        g = re.search("(\(.*\))", m)
        if g != None:
            m = m.strip(g.group(1))
        m = m.lstrip()
        cmd = "help(%s)" % m
        return eval(cmd)
EOPYTHON

command! -nargs=0 PyDoc exec("py py_doc()")
noremap K :PyDoc<CR>

" }}}1

let &cpo = s:cpo_save

" vim:set ft=vim ts=8 sw=4 sts=4:
