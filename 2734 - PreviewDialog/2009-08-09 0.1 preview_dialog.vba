" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/preview_dialog.vim	[[[1
103
" Vim plugin to show dialogs with colors definitions and image files
" Author: Taylon Silmer <taylonsilva@gmail.com>
" Version: 0.1
" License: This program is free software; you can redistribute it and/or modify
"          it under the terms of the GNU General Public License as published by
"          the Free Software Foundation; either version 2 of the License, or
"          (at your option) any later version.
"
"          This program is distributed in the hope that it will be useful,
"          but WITHOUT ANY WARRANTY; without even the implied warranty of
"          MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU 
"          General Public License for more details.
"
"          You should have received a copy of the GNU General Public License
"          along with this program; if not, write to the Free Software
"          Foundation, Inc., 59 Temple Place - Suite 330, Boston, 
"          MA 02111-1307, USA.
"
" Options:
" 
"   g:PreviewDialogKey:
"     Change shotcurt (Default is 'F8')
"    
"   g:PreviewDialogEnable:
"     Enable PreviewDialog (Default is '1')
"    
"   g:PreviewDialogAuto:
"     Shows the dialog automatically when hold cursor over a word (Default is '1')
"
"   g:PathPyFile:
"     Set the path of .py file (Default is '~/.vim/')
"    
"   g:WindowPosition:
"     Set default window position. Can you use 'center', 'right_top' or 'right_bottom' (Default is 'right_bottom')
"
" ChangeLog:
"   0.1:
"     - First release.
"

" Set default options
if !exists('g:PreviewDialogKey')
    let g:PreviewDialogKey = '<F8>'
endif
if !exists('g:PathPyFile')
    let g:PathPyFile = '~/.vim/'
endif
if !exists('g:DialogPosition')
    let g:DialogPosition = 'right_bottom'
endif
if !exists('g:DialogTitle')
    let g:DialogTitle = 'VIM - PreviewDialog'
endif
if !exists('g:PreviewDialogAuto')
    let g:PreviewDialogAuto = 1
endif

" Remember the last image/color displayed
let s:LastWord = ''

function! Show(hold)
    if (strpart(getline('.'), col('.') - 1, 1) =~ '\S')
        let s:word = expand('<cfile>')

        " If have a '#' probably is a color
        if s:word =~ '#'
            let s:word = expand('<cword>')
        endif

        " Check the LastWord just in CursorHold event
        if a:hold
            if s:LastWord != s:word
                call system('python '.g:PathPyFile.'preview_dialog.py -s '.s:word.' -t'.fnameescape(g:DialogTitle).' -p'.g:DialogPosition.' &')
                let s:LastWord = s:word
            endif
        else
            call system('python '.g:PathPyFile.'preview_dialog.py -s '.s:word.' -t'.g:DialogTitle.' -p'.g:DialogPosition.' &')
        endif
    endif
endfunction

function! ShowI(hold)
    call Show(a:hold)

    if col('.') == len(getline('.'))
        " Insert if the cursor is in the end of line
        startinsert!
    else
        " <ESC> move the cursor to the left, then move it for right
        let s:pos = getpos('.')
        let s:pos[2] += 1
        call setpos('.', s:pos)
        startinsert
    endif
endfunction

if exists('g:PreviewDialogEnable') && g:PreviewDialogEnable
    exec 'nmap '.g:PreviewDialogKey.' :call Show(0)<CR>'
    exec 'imap '.g:PreviewDialogKey.' <ESC>:call ShowI(0)<CR>'
    if exists('g:PreviewDialogAuto') && g:PreviewDialogAuto
        au! CursorHold  * nested call Show(1)
    endif
endif
preview_dialog.py	[[[1
85
#!/usr/bin/env python
# Copyright (c) 2009 Taylon Silmer <taylonsilva@gmail.com>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU 
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, 
# MA 02111-1307, USA.
from optparse import OptionParser
import os

import gtk

class Window(gtk.Window):
    def __init__(self, title, window_position):
        super(Window, self).__init__()

        self.set_title(title)
        self.set_decorated(False)
        self.set_border_width(2)
        self.connect("key-press-event", gtk.main_quit)

        if window_position == 'right_bottom':
            self.set_gravity(gtk.gdk.GRAVITY_SOUTH_EAST)
            width, height = self.get_size()
            self.move(gtk.gdk.screen_width() - width, gtk.gdk.screen_height() - height)
        elif window_position == 'right_top':
            width, height = self.get_size()
            self.move(gtk.gdk.screen_width() - width, 0)
        else:
            self.set_position(gtk.WIN_POS_CENTER)

class ImageWindow(Window):
    def __init__(self, image_dir, title, window_position):
        super(ImageWindow, self).__init__(title, window_position)

        image = gtk.Image()
        image.set_from_file(image_dir)

        self.add(image)
        self.show_all()

class ColorWindow(Window):
    def __init__(self, color, title, window_position):
        super(ColorWindow, self).__init__(title, window_position)

        self.color_area = gtk.DrawingArea()
        self.color_area.set_size_request(120, 75)
        self.color_area.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(color))

        self.add(self.color_area)
        self.show_all()

argv = OptionParser()
argv.add_option("-s", "--show", action="store", dest="show", type="string", help="Image or Color to show")
argv.add_option("-t", "--title", action="store", dest="title", type="string", help="Window title")
argv.add_option("-p", "--position", action="store", dest="window_position", type="string", help="Window position")

(args, trash) = argv.parse_args()

if not args.title:
    args.title = 'VIM'

if not args.window_position:
    args.window_position='right_bottom'

# For be a file need to have a '/' (setting path) and a '.' (setting file extension)
if '/' in args.show and '.' in args.show and os.path.isfile(args.show):
    ImageWindow(args.show, args.title, args.window_position)
else:
    try:
        ColorWindow('#' + args.show, args.title, args.window_position)
    except:
        ColorWindow(args.show, args.title, args.window_position)

gtk.main()
