"This little script helps you to program in python.
"You just need to select the text which you want to be run inside python,
"then you use the command :Rc to run it inside the enviroment(which is your
"current buffer). Well. This scripts makes your vim a truely python
"interpreter. Suppose you are typing a program and you want to know if there
"are some errors. You just need to type :Lc this command will load the entire
"buffer to the python and exec it. If there are errors they will be reported
"The command Rc simply evaluates a expression from the current stack(buffer) 
"So, you need to load the buffer if you want to evaluate a command with the
"buffer context
"
"Author:Iury O. G. Figueiredo a.k.a Tau
"Irc: irc.freenode.org
"Channel:#calculus
"E-mail: robatsch@hotmail.com

if has('python')
  command! Rc python interpret()
  command! Lc python load()
else
  command! Rc echo 'Only avaliable with +python support.'
  command! Lc echo 'Only avaliable with +python support.'
endif

if has('python')
python << EOF

import vim

class wrap:
	pass

	
stack = wrap()


def load():
	code = vim.current.buffer
	
	exec('\n'.join(code))
	vars(stack).update(locals())	

	locals().update(vars(stack))
def interpret():
	expr = vim.eval('@*')
	locals().update(vars(stack))
	result = eval(expr)
	vim.command('echo "%s"' % result)
  	
EOF
endif
