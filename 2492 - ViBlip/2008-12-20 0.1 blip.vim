" File: blip.vim
" Description: Posting statuses to Blip!
" Maintainer: Marcin Sztolcman <marcin@urzenia.net>
" Version: v0.1
" Date: 2008.12.20
" History:
" 0.1 Inital upload to vim.org
" ------------------------------------------------------------------------------

if !has('python')
    echo "Error: Required vim compiled with +python"
    finish
endif

python << EOF

import base64
import httplib
import os, os.path
import re
import vim

class ViBlip (object):
    rxp_raw2utf = re.compile (r'\\u[a-z0-9]{4}')

    def __init__ (self):
        self.user        = None
        self.passwd      = None
        self.conn        = None

        if self.user is None or self.passwd is None:
            try:
                try:
                    fh = open (os.path.join (os.path.expanduser ('~'), '.vibliprc'), 'r')
                    self.user   = fh.readline ().strip ()
                    self.passwd = fh.readline ().strip ()
                finally:
                    fh.close ()
            except:
                pass

        self.headers     = {
            'X-Blip-api':       '0.02',
            'Accept':           'application/json',
            'Content-type':     'application/json',
            'User-Agent':       'viblip/0.1 (http://urzenia.net)',
            'Authorization':    'Basic '+base64.encodestring ('%s:%s' % (self.user, self.passwd)).rstrip (),
        }

        self.conn = httplib.HTTPConnection('api.blip.pl', 80)

    def send (self):
        msg = vim.eval ('a:1')
        msg = '{"update": {"body": "%s"}}' % msg

        try:
            self.conn.request ('POST', '/updates', msg, self.headers)
            response = self.conn.getresponse ()

            if response.status in (200, 201, 204):
                print 'Status OK'
                return True
            print response.status, response.reason
        except Exception, e:
            print 'Error:', e
            return False

    def _raw2utf (self, rematch):
       return unichr (int ('0x' + rematch.group (0)[2:], 16))

    def raw2utf (self, str):
        return self.rxp_raw2utf.sub (self._raw2utf, str).encode ('utf-8')

    def dashboard (self):
        url = '/dashboard'
        if vim.eval ('a:1'):
        	url = '/users/' + vim.eval ('a:1') + url

        try:
            self.conn.request ('GET', url, None, self.headers)
            response = self.conn.getresponse ()

            if response.status not in (200, 201, 204):
                print 'Error:', response.status, response.reason
                return

            if response.getheader ('Content-Length', 0) <= 0:
                print 'Dashbard empty'
                return

            dboard = response.read ()
            if not dboard:
                print 'Dashbard empty'
                return

            try:
                import cjson
                dboard = cjson.decode (dboard)
            except ImportError:
                try:
                    dboard = eval (dboard)
                except:
                    print 'Cannot parse json.'
                    return

            dboard.reverse ()
            for status in dboard:
                msg = '%s %s %s' % (status['created_at'], status['id'], status['user_path'].split ('/')[-1])
                rcp = status.get ('recipient_path', '')
                if rcp:
                    msg += ' > ' + rcp.split ('/')[-1]
                msg += ': %s' % viblip_raw2utf (status['body']).replace ('\\', '')
                print msg

        except Exception, e:
            print 'Error:', e
            return False

viblip = ViBlip ()

EOF

function! ViBlipSend(...)
    python viblip.send()
endfunction
command! -nargs=1 Blip call ViBlipSend (<q-args>)

function! ViBlipDasboard(...)
    python viblip.dashboard()
endfunction
command! -nargs=? DBlip call ViBlipDasboard (<q-args>)
