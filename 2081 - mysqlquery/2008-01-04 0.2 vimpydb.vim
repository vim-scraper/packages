func! ExecMySQL() range
python << EOF
import os, re
from stat import *
from vim import *

# default connection params
HOST, DBNAME, USER, PASS = ('localhost', 'test', 'root', '')

# trying to search for connection params in firs 10 lines of current file
connstring = re.compile(r'(mysql)://([^:]*):([^@]*)@([^/]*)/(.*)')
for line in range(10):
    m = connstring.search(current.buffer[line])
    if m: USER, PASS, HOST, DBNAME = m.groups()[1:]; break

# buiding parameters for mysql command
myparams = []
if len(HOST) > 0: myparams.append('-h' + HOST)
if len(USER) > 0: myparams.append('-u' + USER)
if len(PASS) > 0: myparams.append('-p' + PASS)
if len(DBNAME) > 0: myparams.append(DBNAME)
myparams = ' '.join(myparams)

# get query to execute
query = "\n".join([l for l in current.range])
a = int(eval('a:firstline'))
b = int(eval('a:lastline'))
query = "\n".join(current.buffer[a-1:b])

# save query to file for later execution
f = open('/tmp/.mysql_lastquery', 'w'); f.write(query); f.close()

cmd = 'mysql -H %s < /tmp/.mysql_lastquery 2>/tmp/.mysql_lasterror | ' \
      'iconv -f UTF-8 -t ISO-8859-13 > /tmp/.mysql_lastrawresult' % myparams
os.system(cmd)

if os.stat('/tmp/.mysql_lasterror')[ST_SIZE] > 0:
    print 'QUERY:'
    print query
    print '     *** error ****'
    print "\n".join(open('/tmp/.mysql_lasterror', 'r').readlines())
    print '     *** error ****'

elif os.stat('/tmp/.mysql_lastrawresult')[ST_SIZE] > 0:
    cmd = 'elinks -dump -force-html /tmp/.mysql_lastrawresult > /tmp/.mysql_lastresult'
    os.system(cmd)

    cmd = 'elinks -dump -force-html /tmp/.mysql_lastrawresult > /tmp/.mysql_lastresult'
    os.system(cmd)

    # Save current buffer
    command('write')
    # Open result buffer
    command('edit /tmp/.mysql_lastresult')
    # Window must not wrap lins and, changes should be automaticaly saved
    command('setlocal nowrap')
    command('setlocal autowrite')

else:
    print ' *** No results were returned. *** '
    print 'Executed query:'
    print query

EOF
endfunc


