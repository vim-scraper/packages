" File: blip.vim
" Description: Posting statuses to Blip!
" Maintainer: Marcin Sztolcman <marcin@urzenia.net>
" Version: v0.4
" Date: 2009.05.10
" Info: $Id: blip.vim 6 2009-05-10 21:50:15Z mysz $
" History:
" 0.4   - Rewrite some parts of code
"       - ability to use json module
"       - new command: RBlip - reconnecting
"       - new method: read - unified GET
"       - dashboard - can get specified quant of statuses, from yourself
"       dashboard or from specified user
"       - message - can read statuses, directed or private messages
" 0.3 better checking for python feature
" 0.2 Methods for read private messages or single status, and some fixes
" 0.1 Initial upload to vim.org
" ------------------------------------------------------------------------------

if !has('python')
    echo "Error: Required vim compiled with +python"
else

python << EOF

import base64
import httplib
import os, os.path
import re

import vim

class ViBlipError (Exception):
    pass

class ViBlip (object):
    version     = '0.4'
    rxp_raw2utf = re.compile (r'\\u[a-z0-9]{4}')
    parser      = None

    def __init__ (self):
        self.user        = None
        self.passwd      = None
        self.conn        = None

        try:
            import json
            self.parser = json.loads
        except ImportError:
            try:
                import cjson
                self.parser = cjson.decode
            except ImportError:
                self.parser = eval

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
            'User-Agent':       'ViBlip/' + self.version + ' (http://www.vim.org/scripts/script.php?script_id=2492)',
            'Authorization':    'Basic ' + base64.encodestring ('%s:%s' % (self.user, self.passwd)).rstrip (),
        }

        self.connect ()

    def connect (self):
        self.conn = httplib.HTTPConnection('api.blip.pl', 80)

    def send (self):
        msg = vim.eval ('a:1')
        msg = '{"update": {"body": "%s"}}' % msg

        try:
            self.conn.request ('POST', '/updates', msg, self.headers)
            response = self.conn.getresponse ()

            if response.status not in (200, 201, 204):
                print response.status, response.reason
                return

            print 'Status OK:', response.getheader ('Location', '').replace ('/api.', '/').replace ('/updates/', '/s/')
            return True

        except Exception, e:
            print 'Error:', e
            return False

    def read (self, url):
        self.conn.request ('GET', url, None, self.headers)
        response = self.conn.getresponse ()
        if response.status not in (200, 201, 204):
            raise ViBlipError ((response.status, response.reason))

        if response.getheader ('Content-Length', 0) <= 0:
            raise ViBlipError ('Response empty')

        reply = response.read ()
        if not reply:
            raise ViBlipError ('Response empty')

        return self.parser (reply)

    def _raw2utf (self, rematch):
       return unichr (int ('0x' + rematch.group (0)[2:], 16))

    def raw2utf (self, str):
        return self.rxp_raw2utf.sub (self._raw2utf, str).encode ('utf-8')
    def dashboard (self):
        url = vim.eval ('a:1')
        if url:
            if url.isdigit ():
                url = '/dashboard?limit=' + url
            else:
            	if '/' in url:
                    url, limit = url.split ('/')
                    url = '/users/' + url + '/dashboard?limit=' + limit
                else:
                    url = '/users/' + url + '/dashboard'
        else:
            url = '/dashboard'

        try:
            dboard = self.read (url)
            if not dboard:
                print 'Dashbard empty'
                return

            for status in dboard:
                msg = '%s %s %s' % (status['created_at'], status['id'], status['user_path'].split ('/')[-1])
                rcp = status.get ('recipient_path', '')
                if rcp:
                    msg += ' > ' + rcp.split ('/')[-1]
                msg += ': %s' % self.raw2utf (status['body']).replace ('\\', '')
                print msg

        except ViBlipError, e:
            print 'Dashbard empty'
            return False
        except Exception, e:
            print 'Error:', e
            return False

    def private (self):
        url = '/private_messages?limit='
        if vim.eval ('a:1'):
            url += vim.eval ('a:1')
        else:
            url += '5'

        try:
            pms = self.read (url)
            if not pms:
                print 'No private messages'
                return False

            for pm in pms:
                msg = '%s %s >> %s: %s' % (
                    pm['created_at'],
                    pm['user_path'].split ('/')[-1],
                    pm['recipient_path'].split ('/')[-1],
                    self.raw2utf (pm['body']).replace ('\\', ''),
                )
                print msg

        except ViBlipError, e:
            print 'No private messages'
            return False
        except Exception, e:
            print 'Error:', e
            return False

    def message (self):
        #url = '/statuses/' + vim.eval ('a:1')
        msg_types = dict (
            pm = '/private_messages/',
            dm = '/directed_messages/',
            st = '/statuses/',
        )

        url = vim.eval ('a:1')
        if '/' in url:
            url, _type = url.split ('/')
            if _type in msg_types:
                url = msg_types[_type] + url
            else:
                print 'Unknown status type - should be one of: dm (direct message), pm (private message), st (status).'
                return False
        else:
            url = '/statuses/' + url

        try:
            message = self.read (url)
            if not message:
                print 'Status empty'
                return False

            print '%s %s %s' % (
                message['created_at'],
                message['user_path'].split ('/')[-1],
                self.raw2utf (message['body']).replace ('\\', '')
            )

        except ViBlipError, e:
            print 'Status empty'
            return False
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

function! ViBlipPrivateMessages(...)
    python viblip.private()
endfunction
command! -nargs=? PBlip call ViBlipPrivateMessages (<q-args>)

function! ViBlipMessage(...)
    python viblip.message()
endfunction
command! -nargs=1 MBlip call ViBlipMessage (<q-args>)

function! ViBlipReconnect()
    python viblip.connect()
endfunction
command! -nargs=0 RBlip call ViBlipReconnect ()

endif
