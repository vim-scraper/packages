" tumblr.vim - Tumblr
" Maintainer:   Travis Jeffery <eatsleepgolf@gmail.com>
" Time-stamp: <05 Aug 2008 03:42:05 Travis Jeffery>
"
"Exit quickly when:
"- this plugin was already loaded (or disabled)
"- when 'compatible' is set
 if (exists("g:loaded_tumblr") && g:loaded_tumblr) || &cp
     finish
 endif
 let g:loaded_tumblr = 1
 
"let s:cpo_save = &cpo
"set cpo&vim

" Code {{{1
command! -nargs=0 TumblrNew exec("py tumblr_new_post()")
command! -nargs=0 TumblrPost exec("py send_post()")
" }}}1

" let &cpo = s:cpo_save

python <<EOF
from vim import *
from urllib import urlencode, urlopen

def tumblr_new_post():
    cb = vim.current.buffer
    cb[0] = "Title: "
    cb.append("-- Post follows this line --")
    vim.command("set ft=html")

def get_body():
    body = "\n".join(vim.current.buffer[2:])
    return body

def get_title():
    first_line = vim.current.buffer[0]
    title = first_line.strip("Title :")
    return title

def send_post():
    url = "http://www.tumblr.com/api/write"
    email = vim.eval("g:tumblr_email")
    password = vim.eval("g:tumblr_password")
    title = get_title()
    body = get_body()
    data = urllib.urlencode({"email" : email, "password" : password, "title" : title, "body" : body})
    res = urllib.urlopen(url, data)

EOF

" vim:set ft=vim ts=8 sw=4 sts=4:
