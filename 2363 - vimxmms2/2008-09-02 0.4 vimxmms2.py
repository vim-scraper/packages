#coding: utf-8

'''A XMMS2 client for Vim.

File: vimxmms2.py
Author: Xin Wang
Email: <wxyzin gmail com>
Version: 0.4
Date: 2008-09-02

This plugin is provided under BSD-ish license.

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

    Also the following keyshorts are avaiable. Most key maps start with
    c(Contrl) or l(playList).

      <space>    Play the song under cursor.
      <cr>       Same as <space>.
      cs         Stop.
      cp         Pause.
      cr         Select a repeat mode, repeat one track or all.
      -          Decrease volume.
      =          Increase volume.
      r          Refresh window manually.
      la         Add a file or directory to playlist.
      lc         Clear the playlist.
      ld         Remove the song under cursor from current playlist.
      lf         Shuffer the list.
      ll         Load a playlist.
      ln         Create a new playlist, and save current contents to it.
      ls         Sort the playlist, by artist, title or filename.
Customize:
    Specify the id3's encoding. For example:
        let g:xmms_id3_encoding="gbk"

    Specify the play window's width:
        let g:xmms_window_width=30

    Specify the playlist format, default is "%artist - %title", %album
    is also avaiable.
        let g:xmms_playlist_format="%title"

Tips:
    When adding music file or directory, you can use Ctrl-D to show all 
    candidates. Also, you can use arrow keys to navigate the historys.
Changelog:
    2008/09/02
        - Auto refresh play window using CursorHold event.
        - Now user can customize the playlist format.
    2008/09/01
        - Add basic playlist save and load support.
        - Show current playlist and volume in statusline.
        - Add shuffle, repeat command.
        - Add playlist sort command.
        - The key map are mostly redefined.
    2008/08/31
        - Change from XMMSSync to XMMS to gain more control.
        - Fix a bug when play the last song in the playlist, remove any
          entry will cause an out of range error.
        - When encouter encoding error of id3, use file name instead.
'''

import os.path
import os
import xmmsclient
import urllib
import locale
try:
    import vim
except ImportError:
    pass

class Controller(object):
    """A simple wrapper class of xmmsclient.XMMS."""

    def __init__(self, options):
        self.x = xmmsclient.XMMS('VIM')
        try:
            self.x.connect()
        except IOError:
            os.system('xmms2-launcher > /dev/null')
            self.x.connect()

        self.options = options
        self.system_encoding = locale.getdefaultlocale()[1]

    def _get_status(self):
        r = self.x.playback_status()
        r.wait()
        val = r.get_uint()
        if val == xmmsclient.PLAYBACK_STATUS_PLAY:
            return "play"
        elif val == xmmsclient.PLAYBACK_STATUS_PAUSE:
            return "pause"
        elif val == xmmsclient.PLAYBACK_STATUS_STOP:
            return "stop"
        else:
            return "unknown"

    def play(self, pos):
        if self.current_position() != pos:
            self.x.playlist_set_next(pos).wait()
            self.x.playback_tickle().wait()
        if self._get_status() != "play":
            self.x.playback_start().wait()

    def delete(self, pos):
        """Remove a song from playlist.

        If the song to be removed is playing now, we first stop it.
        """
        if self._get_status() == "play" and self.current_position() == pos:
                self.x.playback_stop().wait()
        self.x.playlist_remove_entry(pos).wait()

    def clear(self):
        self.x.playback_stop().wait()
        self.x.playlist_clear().wait()

    def pause(self):
        if self._get_status() == "pause":
            self.x.playback_start().wait()
        else:
            self.x.playback_pause().wait()

    def get_volume(self):
        r = self.x.playback_volume_get()
        r.wait()
        return r.get_dict()['left']

    def _set_volume(self, val):
        self.x.playback_volume_set('left', val).wait()
        self.x.playback_volume_set('right', val).wait()

    def change_volume(self, num):
        value = self.get_volume() + num

        if 0 <= value <= 100:
            self._set_volume(value)
        else:
            self._set_volume(0 if value < 0 else 100)

        return self.get_volume()

    def stop(self):
        self.x.playback_stop().wait()

    def shuffle(self):
        self.x.playlist_shuffle().wait()

    def sort_playlist(self, mode):
        if mode == "artist":
            self.x.playlist_sort(('plugin/id3v2', 'artist')).wait()
        elif mode == "title":
            self.x.playlist_sort(('plugin/id3v2', 'title')).wait()
        elif mode == "file":
            self.x.playlist_sort(('server', 'url')).wait()

    def save_playlist(self, name):
        self.x.playlist_create(name).wait()
        r = self.x.playlist_list_entries()
        r.wait()
        for i, id in enumerate(r.get_list()):
            self.x.playlist_insert_id(i, id, name).wait()

    def load_playlist(self, name):
        self.x.playlist_load(name).wait()

    def get_playlists(self):
        r = self.x.playlist_list()
        r.wait()
        return [x.encode(self.system_encoding) for x in r.get_list()]

    def get_current_playlist(self):
        r = self.x.playlist_current_active()
        r.wait()
        return r.get_string().encode(self.system_encoding)

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
            if self.options['debug']:
                print r.get_error(), "in Controller.current_position."
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
                    t = s.encode('latin1').decode(encoding)
                    return t.encode(self.system_encoding)
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
                if self.options['debug']:
                    print r.get_error(), "in Controller.playlist."
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
            try:
                album = iconv(song[('plugin/id3v2', 'album')])
            except KeyError:
                try:
                    album = iconv(song[('plugin/mad', 'album')])
                except KeyError:
                    album = ''
            if artist == "" and title == "":
                name = os.path.split(song[('server', 'url')])[1]
                name = os.path.splitext(name)[0]
                name = urllib.unquote(
                    name.decode(self.system_encoding).encode('latin1'))
                name = name.replace("+", " ")
                lst.append('  ' + name)
            else:
                line = self.options["playlist_format"]
                line = line.replace("%title", title)
                line = line.replace("%artist", artist)
                line = line.replace("%album", album)
                lst.append('  ' + line)

        return lst

    def set_repeat_mode(self, mode):
        def set(key, val):
            self.x.configval_set(key, val).wait()

        if mode == "all":
            set("playlist.repeat_one", "0")
            set("playlist.repeat_all", "1")
        elif mode == "one":
            set("playlist.repeat_one", "1")
            set("playlist.repeat_all", "0")
        elif mode == "off":
            set("playlist.repeat_one", "0")
            set("playlist.repeat_all", "0")

    def get_repeat_mode(self):
        d = { "0": False, "1": True }
        r = self.x.configval_get("playlist.repeat_one")
        r.wait()
        one = d[r.get_string()]

        r = self.x.configval_get("playlist.repeat_all")
        r.wait()
        all = d[r.get_string()]
        
        if all:
            return "all"
        elif one:
            return "one"
        else:
            return "off"
            

class XMMS2Vim(object):
    def __init__(self):
        self.options = self._read_options()
        self.player = Controller(self.options)

        # Previous song, used by indicator code.
        self.prev_song = self.player.current_position()

        self.create_window()

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

    def _read_options(self):
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

        if exists("g:xmms_debug"):
            d["debug"] = True
        else:
            d["debug"] = False

        if exists("g:xmms_playlist_format"):
            d["playlist_format"] = vim.eval("g:xmms_playlist_format")
        else:
            d["playlist_format"] = "%artist - %title"

        return d

    def create_window(self):
        # Create a new window on the right.
        vim.command('silent! botright vertical %s split __XMMS2__'
                    % self.options["window_width"])

    def _set_status(self, text):
        vim.command('silent! setlocal statusline=%s' % text)

    def refresh_status(self):
        # Current playlist
        playlist = self.player.get_current_playlist()
        volume = self.player.get_volume()
        repeat = {"one": "O",
                  "all": "A",
                  "off": "-"}[self.player.get_repeat_mode()]
        text = "P:%s\\ V:%s%%%%\\ [%s]" % (playlist, volume, repeat)
        self._set_status(text)

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

    def _get_input(self, prompt):
        text = vim.eval('expand(input("%s"))' % prompt)
        if text == None or text == "":
            return None
        return text

    def load_playlist(self):
        lst = self.player.get_playlists()
        try:
            lst.remove('_active')
        except ValueError:
            pass
        show = '["Playlist:", ' \
                + "".join(['"%s. %s", ' % (i+1,x) for i, x in enumerate(lst)])\
                + ']'
        n = int(vim.eval('inputlist(%s)' % show))
        if n >= 1:
            self.player.load_playlist(lst[n-1])
            self.refresh_window()

    def set_repeat_mode(self):
        n = int(vim.eval(
            'inputlist(["Repeat Mode:", "1. ALL", "2. ONE", "3. OFF"])'))
        if 1 <= n <= 3:
            self.player.set_repeat_mode(["", "all", "one", "off"][n])
        self.refresh_status()

    def sort_playlist(self):
        n = int(vim.eval('inputlist(["Sort Playlist:",\
                         "1. Artist", "2. Title", "3. File"])'))
        if 1 <= n <= 4:
            self.player.sort_playlist(["",
                                       "artist",
                                       "title",
                                       "file"][n])
        self.refresh_window()

    def save_playlist(self):
        name = self._get_input("Playlist name: ")
        self.player.save_playlist(name)

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

    def shuffle_playlist(self):
        self.player.shuffle()
        self.refresh_window()

    def increase_volume(self):
        cur = self.player.change_volume(6)
        self.refresh_status()

    def decrease_volume(self):
        cur = self.player.change_volume(-6)
        self.refresh_status()

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
        self.refresh_status()

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

    def stop_play(self):
        self.player.stop()

    def pause_play(self):
        self.player.pause()


def xmms_refresh():
    winnum = vim.eval('bufwinnr("__XMMS2__")')

    if winnum != "-1":
        prevwin = vim.eval("winnr()")
        if prevwin == winnum:
            xmms2vim.refresh_window()
        else:
            vim.command(winnum + "wincmd w")
            xmms2vim.refresh_window()
            vim.command(prevwin + 'wincmd w')

def xmms_toggle():
    global xmms2vim

    winnum = vim.eval('bufwinnr("__XMMS2__")')
    # If bufwinnr returns -1, then there's no window named __XMMS2__,
    # so we create one, otherwise we close it.
    if winnum == '-1':
        try:
            xmms2vim.create_window()
        except NameError:
            xmms2vim = XMMS2Vim()
            xmms_keymap()
            vim.command('au WinEnter __XMMS2__ py xmms2vim.refresh_window()')
            vim.command('au CursorHold * py xmms_refresh()')
    else:
        # If current window is __XMMS2__, we close it directly.
        # But when the cursor is in other window, we first remember
        # the window number, and jump to __XMMS2__, close it,
        # then jump back to previous window.
        bufname = vim.current.buffer.name
        if bufname != None and bufname.endswith('__XMMS2__'):
            # We close __XMMS__ only when there are other windows exists.
            if len(vim.windows) > 1:
                vim.command('close')
        else:
            prevbuf = vim.eval('bufnr("%")')
            vim.command(winnum + 'wincmd w')
            vim.command('close')
            prevwin = vim.eval('bufwinnr(%s)' % prevbuf)
            if vim.eval('winnr()') != prevwin:
                vim.command(prevwin + 'wincmd w')

def xmms_keymap():
    """Key mappings."""
    mapcmd = 'nnoremap <buffer> <silent> %s :py %s<cr>'
    maplist = [
        ('r',       'xmms2vim.refresh_window()'),
        ('<space>', 'xmms2vim.play()'),
        ('<cr>',    'xmms2vim.play()'),
        ('=',       'xmms2vim.increase_volume()'),
        ('-',       'xmms2vim.decrease_volume()'),
        ('la',      'xmms2vim.add_path()'),
        ('lc',      'xmms2vim.clear_playlist()'),
        ('ld',      'xmms2vim.delete()'),
        ('lf',      'xmms2vim.shuffle_playlist()'),
        ('ll',      'xmms2vim.load_playlist()'),
        ('ln',      'xmms2vim.save_playlist()'),
        ('ls',      'xmms2vim.sort_playlist()'),
        ('cp',      'xmms2vim.pause_play()'),
        ('cr',      'xmms2vim.set_repeat_mode()'),
        ('cs',      'xmms2vim.stop_play()'),
        ('<2-leftmouse>', 'xmms2vim.play()'),
    ]

    for item in maplist:
        vim.command(mapcmd % item)
