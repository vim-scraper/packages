"This little script helps you to program in python.
"You just need to select the text which you want to be run inside python,
"then you use the command :Sc to run it inside the enviroment(which is your
"current buffer). Well. This scripts makes your vim a truely python
"interpreter. Suppose you are typing a program and you want to know if there
"are some errors. You just need to type :Lc this command will load the entire
"buffer to the python and exec it. If there are errors they will be reported
"The command Sc simply evaluates a expression from the current stack(buffer) 
"So, you need to load the buffer if you want to evaluate a command with the
"The commands Ec simply execute the group of stataments which you have
"selected using v.
"The Cl simply clear the stack which holds all the data which you have defined
"
"buffer context
"
"Author:Iury O. G. Figueiredo a.k.a Tau
"Irc: irc.freenode.org
"Channel:#calculus
"E-mail: robatsch@hotmail.com
"

if exists("loaded_err")
	finish
else
	let loaded_err = 1

endif
if has('python')
	command! Sc python solve()
  	command! Ec python execode()
  	command! Lc python load()
  	command! Cl python reset()
	autocmd BufCreate * prepare()
	autocmd  BufDelete * garbage() 
else
  	command! Sc echo 'Only avaliable with +python support.'
  	command! Ec echo 'Only avaliable with +python support.'
  	command! Cl echo 'Only avaliable with +python support.'
  	command! Lc echo 'Only avaliable with +python support.'
endif


if has('python')
python << EOF

stack = {}

def prepare():
	pass

def load():
	import vim
	window = vim.current.window
	code = window.buffer

	if not stack.has_key(window):
		stack[window] = {}

	exec('\n'.join(code), stack[window]) 
	
def reset():
	import vim
	if stack.has_key(window):
		window = vim.current.window
		stack[window].clear()
	
def execode():
	import vim
	window = vim.current.window
	statm = vim.eval('@*')

	if not stack.has_key(window):
		stack[window] = {}

	exec(statm, stack[window])

def solve():
	import vim
	window = vim.current.window
	expr = vim.eval('@*')

	if not stack.has_key(window):
		stack[window] = {}
	
	result = eval(expr, stack[window])
	vim.command('echo "%s"' % result)
  	
def garbage():
	import vim
	window = vim.current.window

	if stack.has_key(window):
		del stack[window]
EOF
endif
