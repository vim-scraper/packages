#coding: utf-8

'''A Vim plugin to control xmms2.

File: vimxmms2.py
Author: Wang Xin
Email: <wxyzin gmail com>
Version: 0.2
Date: 2008-09-01
Require:
    vim with +python support.
    xmms2.
Install:
    Putting following lines into your .vimrc

    if has('python')
        pyf /path/to/vimxmms2.py
    endif

    nmap <silent> <leader>x :py xmms_toggle()<cr>
Usage:
    Using <leader>x to toogle the play window.

    Also the following keyshorts are avaiable.

      <space>    Play the song under cursor.
      <cr>       Same as <space>.
      s          Stop.
      p          Pause.
      -          Decrease volume.
      =          Increase volume.
      a          Add a file or directory to playlist.
      d          Remove song from current playlist.
      r          Refresh window manually.
      c          Clear the playlist.
Customize:
    Specify the id3's encoding. For example:
        let g:xmms_id3_encoding="gbk"

    Specify the play window's width:
        let g:xmms_window_width=30
Tips:
    When adding music file or directory, you can use Ctrl-D to show all 
    candidates. Also, you can use arrow keys to navigate the historys.
Changelog:
    2008/08/31
        - Change from XMMSSync to XMMS to gain more control.
        - Fix a bug when play the last song in the playlist, remove any
          entry will cause an out of range error.
        - When encouter encoding error of id3, use file name instead.
'''

import os.path
import os
import vim
import xmmsclient
import urllib

class Controller(object):
    """A simple wrapper class of xmmsclient.XMMS."""

    def __init__(self, options):
        self.x = xmmsclient.XMMS('VIM')
        try:
            self.x.connect()
        except IOError:
            os.system('xmms2-launcher')
            self.x.connect()

        self.options = options

    def is_playing(self):
        r = self.x.playback_status()
        r.wait()
        return r.get_uint() == xmmsclient.PLAYBACK_STATUS_PLAY

    def play(self, pos):
        if self.current_position() != pos:
            self.x.playlist_set_next(pos).wait()
            self.x.playback_tickle().wait()
        if not self.is_playing():
            self.x.playback_start().wait()

    def delete(self, pos):
        """Remove a song from playlist.

        If the song to be removed is playing now, we first stop it.
        """
        if self.is_playing() and self.current_position() == pos:
                self.x.playback_stop().wait()
        self.x.playlist_remove_entry(pos).wait()

    def clear(self):
        self.x.playback_stop().wait()
        self.x.playlist_clear().wait()

    def pause(self):
        r = self.x.playback_status()
        r.wait()
        if r.get_uint() == xmmsclient.PLAYBACK_STATUS_PAUSE:
            self.x.playback_start().wait()
        else:
            self.x.playback_pause().wait()

    def change_volume(self, num):
        def set(percent):
            self.x.playback_volume_set('left', percent).wait()
            self.x.playback_volume_set('right', percent).wait()

        r = self.x.playback_volume_get()
        r.wait()
        vol = r.get_dict()
        left = vol['left'] + num
        right = vol['right'] + num

        if 0 <= left <= 100:
            set(left)
        else:
            if left < 0:
                set(0)
            else:
                set(100)
            print 'ERROR: volume out of range'

    def stop(self):
        self.x.playback_stop().wait()

    def save(self, name):
        self.x.medialib_playlist_save_current(name).wait()

    def load(self, name):
        self.x.playlist_load(name).wait()

    def add(self, path):
        url = 'file://' + path
        if os.path.isfile(path):
            self.x.playlist_add_url(url).wait()
        else:
            self.x.playlist_radd(url).wait()

    def current_position(self):
        """Return current position in the playlist."""

        # It is an error to call playlist_current_pos when there are
        # no entries in the playlist.
        r = self.x.playlist_current_pos()
        r.wait()
        if r.iserror():
            print r.get_error()
            return None
        else:
            return r.get_dict()['position']

    def playlist(self):
        """Format the playlist.
        
        First we try to get imformation from id3v2, if it fails,
        then try id3, else we just display the file name.
        """
        def iconv(s):
            encoding = self.options["id3_encoding"]
            try:
                if encoding:
                    return s.encode('latin1').decode(encoding).encode('utf-8')
                else:
                    return s.encode('latin1')
            except UnicodeEncodeError:
                return ""

        lst = []
        r = self.x.playlist_list_entries()
        r.wait()
        for id in r.get_list():
            r = self.x.medialib_get_info(id)
            r.wait()
            if r.iserror():
                print r.get_error()
                lst.append(' ')
                continue
            song = r.get_propdict()
            try:
                artist = iconv(song[('plugin/id3v2', 'artist')])
            except KeyError:
                try:
                    artist = iconv(song[('plugin/mad', 'artist')])
                except KeyError:
                    artist = ''
            try:
                title = iconv(song[('plugin/id3v2', 'title')])
            except KeyError:
                try:
                    title = iconv(song[('plugin/mad', 'title')])
                except KeyError:
                    title = ''
            if artist == "" and title == "":
                name = os.path.split(song[('server', 'url')])[1]
                name = os.path.splitext(name)[0]
                name = urllib.unquote(name.decode('utf-8').encode('latin1'))
                name = name.replace("+", " ")
                lst.append('  ' + name)
            else:
                lst.append('  %s - %s' % (artist.ljust(6), title))

        return lst
            
class XMMS2Vim(object):
    def __init__(self, options):
        self.player = Controller(options)
        # Previous song, used by indicator code.
        self.prev_song = self.player.current_position()

        # Create a new window on the right.
        vim.command('silent! botright vertical %s split __XMMS2__'
                    % options["window_width"])
        
        # Settings for the play window.
        setlist = ['buftype=nofile',
                   'noswapfile',
                   'nobuflisted',
                   'nowrap' ]
        for item in setlist:
            vim.command('silent! setlocal %s' % item)

        # Highlight the current playing song.
        vim.command('syntax match XMMS2Current "^-.*"')
        vim.command('hi link XMMS2Current Folded')

        self.buf = vim.current.buffer
        self.refresh_window()

    def _get_path(self, prompt):
        """Return the path enterd by the user, and expand it to full path.
        Or return None if user haven't enter anything."""

        # When input from vim, vim escapes some special characters,
        # so we have to expand them first.
        cwd = vim.eval('expand(getcwd())')
        path = vim.eval('expand(input("%s", "", "file"))' % prompt)
        if path == None or path == "":
            return None
        else:
            return os.path.join(cwd, os.path.expanduser(path))

    def _current_line_number(self):
        " Returns the line number where the cursor stands."

        # In Vim, line number starts with 1. But in Python,
        # list starts with 0, and in xmms2, the position also
        # starts with 0, so we minus the vim's line number by 1.
        return vim.current.window.cursor[0] - 1

    def play(self):
        line = self._current_line_number()
        self.player.play(line)
        self.refresh_mark()

    def load_playlist(self, name=None):
        if name == None:
            name = self.getInput("Playlist: ")

        if name != None:
            self.player.load(name)
            self.refresh_window()

    def add_path(self):
        """Prompt and add a music file or directory to the playlist."""
        name = self._get_path("Music File or Directory: ")
        if name != None:
            self._clear_window()
            self.player.add(name)
            self.refresh_window()

    def _clear_window(self):
        """Remove all lines in the window."""
        self.buf[:] = []

    def clear_playlist(self):
        """Clear playlist."""
        self.player.clear()
        self._clear_window()

    def delete(self):
        line = self._current_line_number()
        if self.buf[line] != '':
            self.player.delete(line)
            self.refresh_window()

    def refresh_window(self):
        """Refresh the play window."""
        self.buf[:] = self.player.playlist()
        if self.prev_song != None:
            self.refresh_mark()

    def refresh_mark(self):
        """Refresh the current playing music indicator."""
        current = self.player.current_position()
        if current != None:
            if self.prev_song != None and self.prev_song < len(self.buf):
                self.buf[self.prev_song] = ' ' + self.buf[self.prev_song][1:]
            self.buf[current] = '-' + self.buf[current][1:]
            self.prev_song = current
            # Move cursor to current position.
            vim.current.window.cursor = (current + 1, 1)

# There are two global vairables used in this script.
# They are 'player' and 'xmms2vim', they all created
# by xmms_toggle() function.

def xmms_toggle():
    global player
    global xmms2vim

    winnum = vim.eval('bufwinnr("__XMMS2__")')
    options = xmms_read_option()
    # If bufwinnr returns -1, then there's no window named __XMMS2__,
    # so we create one, otherwise we close it.
    if winnum == '-1':
        player = Controller(options)
        xmms2vim = XMMS2Vim(options)
        xmms_keymap()
        xmms_autocmd()
    else:
        # If current window is __XMMS2__, we close it directly.
        # But when the cursor is in other window, we first remember
        # the window number, and jump to __XMMS2__, close it,
        # then jump back to previous window.
        bufname = vim.current.buffer.name
        if bufname != None and bufname.endswith('__XMMS2__'):
            # We close __XMMS__ only when there are other windows exists.
            if len(vim.windows) > 1:
                vim.command('bdelete')
        else:
            curbufnr = vim.eval('bufnr("%")')
            vim.command(winnum + 'wincmd w')
            vim.command('bdelete')
            winnum = vim.eval('bufwinnr(%s)' % curbufnr)
            if vim.eval('winnr()') != winnum:
                vim.command(winnum + 'wincmd w')

def xmms_netrwadd():
    file_dir = vim.eval('expand("%")')
    file_name = vim.current.line

    file_path = os.path.join(file_dir, file_name)

    player.add(file_path)

def xmms_read_option():
    def exists(name):
        return vim.eval('exists("%s")' % name) == "1"

    d = {}
    if exists("g:xmms_id3_encoding"):
        d["id3_encoding"] = vim.eval("g:xmms_id3_encoding")
    else:
        d["id3_encoding"] = None

    if exists("g:xmms_window_width"):
        d["window_width"] = int(vim.eval("g:xmms_window_width"))
    else:
        d["window_width"] = 25

    return d

def xmms_keymap():
    """Key mappings."""
    mapcmd = 'nnoremap <buffer> <silent> %s :py %s<cr>'
    maplist = [
        ('r',       'xmms2vim.refresh_window()'),
        ('<space>', 'xmms2vim.play()'),
        ('<cr>',    'xmms2vim.play()'),
        ('d',       'xmms2vim.delete()'),
        ('=',       'player.change_volume(6)'),
        ('-',       'player.change_volume(-6)'),
        #('S',       'player.save(xmms2vim.getInput("Save playlist:"))'),
        ('s',       'player.stop()'),
        #('l',       'xmms2vim.load()'),
        ('a',       'xmms2vim.add_path()'),
        ('c',       'xmms2vim.clear_playlist()'),
        ('p',       'player.pause()'),
        ('<2-leftmouse>', 'xmms2vim.play()'),
    ]

    for item in maplist:
        vim.command(mapcmd % item)

def xmms_autocmd():
    vim.command('au WinEnter    __XMMS2__       py xmms2vim.refresh_mark()')
