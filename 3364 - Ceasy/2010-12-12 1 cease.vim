"This little script helps you to program in C
"The main goal is helping when debuging/programming in C.
"This scripts appears like a kind of C interpreter since you can run code at
"real time. The mechanisms for which this script work are a bit tricky.
"I will try explaining all them here.
"There are 5 commands. I will start by the easiest one. It is the runall.
"This command works so: When you call :Ra this simply gets the buffer's window
"then put all the contents inside a file and call gcc to compile it.
"then it simply open the file generated and sends to the vim the contats that
"it processed.
"The command Oe is one of the easiest ones. This simply gets a code from the
"window's buffer which you have selected with v. Then it sends to the gcc by
"means of a file wrap which makes a kind of trick to make the gcc accept it.
"So after compiling it, the script calls the file and gets its output.
"The command Ec works in a more complex way. It works together with Lc
"The command Ec simply joins the code selected in the entire buffer's window.
"In that case when you make Ec on a text selected it gets this code and join
"to the 'context' which you have alread programmed. 
"The command Lc simply gets the buffer and compile it to a file .o
"The command Ec simply gets the text selected and put link it by means of the
"a trick to make gcc compile it.
"Let us give a example
"Suppose you have the following functions defined
"int func1(){
"	return 69
"}
"
"int func2() {
"	printf("FUCK OFF THEY SHOULD PAY US FOR IT");
"}
"
"So, you want to test how the func1 works and see if it is working how you
"expect it. (Suppose it is more complex than that)
"
"Then you just need to create the context with :Lc 
"Then do
"printf("%i", func1*());
"then call :Ec
"
"This will compile the code, and call the printf statement which will call the
"func1 from the context which was created..
"Observation: Look, even it appearing like a interpreter it isn't one
"praticalyy. So, you have to follow some rules. These rules are better learnt
"when you practice it. However the most apparent is: you can't put statements
"out functions and call the Lc to load the code. it will simply not work.
"Since the C doesn't admit you to make statements out functions
"So, if you are making statements out functions then take care when calling Lc
"to update the context's code.
"A code idea is putting the statements inside comentaries /* */ so, when you
"want to run something out functions you just need to go to the /* code */
"then select it and make it run with :Ec which will run from inside the
"context. it means it will use the context/functons/variables which are
"defined inside the module. Just don't forget that :Eo simply works out
"context it means if you defined functions then select a text which depends on
"a function which is defined out the text selected then it simply will not
"work as you have imagined.
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

" verify whether there is gcc
"

if has('python')
	command! Ec python execode()
	command! Oe python outexecode()
  	command! Lc python load()
	command! Ra python runall()
	"command! Wc python execline()
	autocmd Bufdelete * python garbage()
	autocmd BufCreate * python prepare()
	autocmd VimLeave * python clean()
else
  	command! Ec echo 'Only avaliable with +python support.'
  	command! Oe echo 'Only avaliable with +python support.'
	command! Lc echo 'Only avaliable with +python support.'
	command! Ra echo 'Only avaliable with +python support.'
	"command! Wc echo 'Only avaliable with +python support.'



endif


if has('python')
python << EOF

stack = []


tmp = ''

def main():
	import vim
	global tmp
	import tempfile

	tmp = tempfile.mkdtemp('cease')

def prepare():
	import vim

	window = vim.current.window
	stack.append(window)
	

def load():
	import vim
	window = vim.current.window
	code = '\n'.join(window.buffer)

	ind = stack.index(window)

	fc = tmp + (r'/mod%s.c' % ind)
	fo = tmp + (r'/mod%s.o' % ind)

	with open(fc, 'w') as f:
		f.write(code)


	msg = compile(fc, fo)[0]
	
	vim.command('echo "%s"' % msg)	       	


def execode():
	import vim
	window = vim.current.window

	ind = stack.index(window)

	code = vim.eval('@*')

	fc = tmp + ('/expr%s.c' % ind)
	fo = tmp + ('/expr%s' % ind)
	lb = tmp + ('/mod%s.c' % ind)

	wrap = "int main() { \n void wrap_module() { \n #include <%s> \n void wrap_solve() { \n %s \n }\n wrap_solve(); \n }\n wrap_module(); \n return 0;\n }" % (lb, code)

	
	with open(fc, 'w') as f:
		f.write(wrap)

	msg, retcode = compile(fc, fo)

	vim.command('echo "%s"' % msg)

	if not retcode:
		msg = run(fo)[0]

	vim.command('echo "%s"' % msg)
	

def outexecode():
	import vim

	window = vim.current.window

	code = '\n'.join(window.buffer)

	wrap = "int main() {\n void wrap() {\n %s\n }\n wrap();\n return 0; }" % code

	ind = stack.index(window)

	fc = tmp + ("/expr%s.c" % ind)
	fo = tmp + ("/expr%s" % ind)

	with open(fc, 'w') as f:
		f.write(wrap)

	msg, retcode = compile(fc, fo)

	vim.command('echo "%s"' % msg)


	#if not retcode:
	msg = run(fo)[0]

	vim.command('echo "%s"' % msg)
	

def compile(input_file, output_file):
	import subprocess

	f = subprocess.Popen(['gcc', input_file,  '-o',  output_file],
       				stdout=subprocess.PIPE, 
				stderr=subprocess.STDOUT, close_fds = True)

	msg = f.stdout.read()

	retcode = f.poll()

	return (msg, retcode)

def run(input_file):
	import subprocess

	f =  subprocess.Popen([input_file], stdout=subprocess.PIPE, 
					stderr=subprocess.STDOUT, close_fds = True)

	msg = f.stdout.read()
	retcode = f.poll()

	return (msg, retcode)

def runall():
	import vim
	import subprocess

	window = vim.current.window

	ind = stack.index(window)

	code = '\n'.join(window.buffer)
	
	fc = tmp + ('/modall%s.c' % ind)
	fo = tmp + ('/modall%s' % ind)

	with open(fc, 'w') as f:
		f.write(code)


	msg, retcode = compile(fc, fo)

	vim.command('echo "%s"' % msg)
	vim.command('echo "%s"' % retcode)
	if not retcode:
		msg = run(fo)[0]

		vim.command('echo "%s"' % msg)


def clean():
	import shutil
	shutil.rmtree(tmp)

def garbage():
	import vim
	window = vim.current.buffer

	ind = stack.index(window)	
	del stack[ind]

main()
prepare()

EOF
endif

