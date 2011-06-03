#! /usr/bin/env python

import sys,os,getpass,time
from subprocess import Popen,PIPE
from stat import S_IRWXG, S_IRWXO

PDUB_DIR = os.path.join(os.path.expanduser('~'), '.pdub')
PDUBS_FILE = os.path.join(PDUB_DIR, 'pdubs.json.gpg')

VIM_SCRIPT = """\
function! EncryptAndExit()
python << EOPYTHON
import sys,vim
sys.path.append('%s')
import pdub
print 'encrypting to ', pdub.PDUBS_FILE
pdub.encrypt_to_file('\\n'.join(vim.current.buffer[:]), 
					pdub.PDUBS_FILE, vim.eval('g:password'))
EOPYTHON
quit!
endfunction

" get the filename and password from the first line
let g:filename = split(getline(1))[0]
let g:password = split(getline(1))[1]

" delete the first line
normal ggdd

set ft=javascript

cmap wq<CR> :call<SPACE>EncryptAndExit()<CR>
""" % os.path.dirname(os.path.abspath(__file__))

VIM_SCRIPT_FILE = os.path.join(PDUB_DIR, '.pdub.vim')

def encrypt_to_file(text, filename, key):
	read_end, write_end = os.pipe()

	with open(filename, 'w') as stdout_fh:
		crypt = Popen(['gpg', '--symmetric', '--yes', '--passphrase-fd',
			str(read_end)], stdout=stdout_fh, stdin=PIPE, stderr=PIPE)

		os.write(write_end, key + '\n')

		crypt.stdin.write(text)

		crypt.communicate()

	[ os.close(fd) for fd in (read_end, write_end) ]

class DecryptionException(Exception): pass

def decrypt_file(filename, key):
	crypt = Popen(['gpg', '-d', '--passphrase-fd', '0', filename], stdin=PIPE,
			stdout=PIPE, stderr=PIPE)
	crypt.stdin.write(key + '\n')
	r = crypt.stdout.read()
	if crypt.wait() == 0:
		return r
	else:
		raise DecryptionException('could not decrypt')

def die(msg):
	sys.stderr.write('ERROR: %s\n' % msg)
	sys.exit(1)

def main():
	bins = [ f for d in os.environ['PATH'].split(':') for f in os.listdir(d) ]
	if 'gpg' not in bins: die('"gpg" not found')

	v = Popen(['vim', '--version'], stdout=PIPE)
	if '+python' not in v.stdout.read(): die('vim isn\'t compiled with +python')

	if not os.path.isdir(PDUB_DIR):
		os.mkdir(PDUB_DIR)
		os.chmod(PDUB_DIR, '700')
	else:
		s = os.stat(PDUB_DIR)
		if S_IRWXG & s.st_mode or S_IRWXO & s.st_mode:
			die('improper file mode for %s' % PDUB_DIR)

	key = getpass.getpass(prompt = 'Password: ')

	if not os.path.exists(PDUBS_FILE):
		encrypt_to_file('', PDUBS_FILE, key)

	s = os.stat(PDUBS_FILE)
	if S_IRWXG & s.st_mode or S_IRWXO & s.st_mode:
		die('improper file mode for %s' % PDUBS_FILE)

	if os.path.isfile(VIM_SCRIPT_FILE):
		os.remove(VIM_SCRIPT_FILE)

	open(VIM_SCRIPT_FILE, 'w').write(VIM_SCRIPT)

	try:
		txt = decrypt_file(PDUBS_FILE, key)
	except DecryptionException:
		die('could not decrypt file -- possibly an incorrect key')

	vim = Popen(['vim', '-n', '-c', 'source %s' % VIM_SCRIPT_FILE, '-'],
			stdin=PIPE)
	vim.stdin.write('%s %s\n' % (PDUBS_FILE, key))
	vim.stdin.write(txt)
	vim.stdin.close()
	vim.wait()

	os.remove(VIM_SCRIPT_FILE)

if __name__ == '__main__': main()
