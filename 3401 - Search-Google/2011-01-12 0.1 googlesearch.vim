" Copyright (c) 2010 Kevin Xiao
"
"
" Summary
" =======
" The Google function open the default browser and search the word of current
" position with google.
"
" Require
" =======
" You need a python interface.
"
" Usage
" =====
" Put the cursor at any word's any position, and type ':Google' command.
"
" Sample word 'vim'.
"
" You use ':Google' command in this word, 
" then you can open  http://www.google.com/search?q=vim in a default web browser.
"
"
"
" I set a command for Google function, such that
"
" command Google :call Google()
" 
" You can change command name by changing above command name 'Google'.
"
" example)
" command AnotherGoogleName :call Google()
"



" define Google function
function! Google()
python << EOM
# coding=utf-8

import vim
import re
import webbrowser

line = vim.current.line
row,col =  vim.current.window.cursor
print col
start = end = col
while True:
	if line[start].isspace():
		break
	start-=1
while True:
	if line[end].isspace():
		break
	end+=1
word = line[start+1:end]
if not word or word.isspace():
	print 'You cannot call Google in a space position!'
else:
	try:
		url = 'http://www.google.com/search?q='+word
		webbrowser.open(url)
		print 'Google : %s' % word
	except:
		print 'Failed! : Cannot access Google!'

EOM
endfunction


" set a command for Google function
command Google :call Google()

