" Vim script - a graphical RGB/HSV hex color chooser
" Needs Python (version >= 2.5, with PyGTK module) and GTK to run.
" Author: Petr Mejzlík <petrmej@gmail.com>; 
"         thanks to TomášK and Nikola Pizurica for contribution
" License: public domain
" Version: 1.0
" Installation: Put this file into your plugin directory ("~/.vim/plugin") 
"               and restart Vim.
" Usage: Call the ColorChooser() function to edit the hex color under the
"        cursor or insert a new one. A window to choose the color will appear. 
"        It is useful to set a key mapping for that (see :help :map-commands).
" Bugs: The dialog window refuses to close when running Vim in xterm.

" Uncomment this to set the key mapping.
":map <F2> :call ColorChooser()<enter>


function! ColorChooser()

" --- script configuration - edit to configure --- 

" The color to be loaded if not editing an existing color
let s:default_color = exists('g:colorchooser_default_color') ? g:colorchooser_default_color : '#000000'

" If enabled, the default color will be overriden by the color in
" the clipboard, if the clipboard contains a color specification. (note: in X11 the x11-selection is used)
" (0=disabled, 1=enabled)
let s:cbd_in = exists('g:colorchooser_cbd_in') ? g:colorchooser_cbd_in : 1

" If enabled, the chosen color is copied to clipboard. (note: in X11 the x11-selection is used)
" (0=disabled, 1=enabled)
let s:cbd_out = exists('g:colorchooser_cbd_out') ? g:colorchooser_cbd_out : 1

" If enabled, a color specified by a hexcode can be edited. (0=disabled, 1=enabled)
let s:parse_hex = exists('g:colorchooser_parse_hex') ? g:colorchooser_parse_hex : 1

" If enabled, a color specified by a name can be edited. (0=disabled, 1=enabled)
" note: The X11 color names are used. Most of these are the same as in
" HTML/CSS, however there are differences. See http://en.wikipedia.org/wiki/X11_color_names 
" for more information.
let s:parse_names = exists('g:colorchooser_parse_names') ? g:colorchooser_parse_names : 1

" Palette to store colors in the window (0=disabled, 1=enabled)
let s:display_palette = exists('g:colorchooser_display_palette') ? g:colorchooser_display_palette : 1

" --- end of script configuration ---

python << endpython

import vim
import gtk, sys, re

def tohex(c):
    #Convert to hex string
    #little hack to fix bug
    s = ['#',hex(int(c[0]*256))[2:].zfill(2),hex(int(c[1]*256))[2:].zfill(2),hex(int(c[2]*256))[2:].zfill(2)]
    for item in enumerate(s):
        if item[1]=='100':
            s[item[0]]='ff'
    return ''.join(s).upper()


# message strings
status_running = "Choosing color..."
wnd_title_edit = "Edit color %s"
wnd_title_insert = "Insert a color"

# translations
lang = vim.eval("v:lang")
if re.match("cs", lang):  # Czech
    status_running = "Výběr barvy..."
    wnd_title_edit = "Upravit barvu %s"
    wnd_title_insert = "Vložit barvu"
elif re.match("es", lang):  # Spanish
    status_running = "Seleccionando color..."
    wnd_title_edit = "Editar el color %s"
    wnd_title_insert = "Insertar un color"


print status_running

vim.command("let save_cursor = getpos('.')")  # save cursor position
vim.command("let l:save_reg = @0")  # backup the unnamed register
vim.command(':normal ebyw')
color_name = vim.eval("@0").lstrip().rstrip()  # word under the cursor (for named colors)
vim.command(':normal hy2w')
color_hex = vim.eval("@0").lstrip().rstrip()  # char+word (all the hexcode including '#')
vim.command("let @0 = l:save_reg")  # restore the unnamed register

parsed_ok = False
parsed_cbd_ok = False
replacing_name = False

# try to find a color under the cursor
if int(vim.eval("s:parse_hex")):
    try:
        parsed_color = gtk.gdk.color_parse(color_hex)
    except (ValueError, TypeError):  # not on a valid color hexcode -> inserting new color
        pass
    else:  # replacing the hexcode under cursor
        color = color_hex
        parsed_ok = True
if int(vim.eval("s:parse_names")) and not parsed_ok:
    try:
        parsed_color = gtk.gdk.color_parse(color_name)
    except (ValueError, TypeError):  # not on a valid color name -> inserting new color
        pass
    else:  # replacing the color name under cursor
        replacing_name = True
        color = color_name
        parsed_ok = True

# try to find a color in the clipboard
if int(vim.eval("s:cbd_in")) and not parsed_ok:
    try:
        parsed_color = gtk.gdk.color_parse(vim.eval("@*"))
    except (ValueError, TypeError):  # register * does not contain a valid color -> inserting new color
        pass
    else:  # edit color found in register *
        parsed_cbd_ok = True

csd = gtk.ColorSelectionDialog(wnd_title_edit % color) if parsed_ok else gtk.ColorSelectionDialog(wnd_title_insert)
cs = csd.colorsel

if parsed_ok:  # going to replace color
    cs.set_current_color(parsed_color)
else:  # going to insert color
    vim.command("call setpos('.', save_cursor)")  # restore cursor position
    if parsed_cbd_ok and int(vim.eval("s:cbd_in")):
        cs.set_current_color(parsed_color) 
    else:
        cs.set_current_color(gtk.gdk.color_parse(vim.eval("s:default_color"))) 

cs.set_current_alpha(65536)
cs.set_has_opacity_control(False)
cs.set_has_palette(int(vim.eval("s:display_palette")))

if csd.run()==gtk.RESPONSE_OK:
    c = cs.get_current_color()
    hexcolor = tohex((c.red/65536.0, c.green/65536.0, c.blue/65536.0)) 
    if parsed_ok:  # replacing color
        if replacing_name:  # changing name under cursor -> delete old name
            vim.command(':normal l"_dw') 
        else:  # changing hexcode under cursor -> delete old hexcode
            vim.command(':normal "_d2w')
    vim.command(":normal i%s" % hexcolor)  # insert new color hexcode
    if int(vim.eval("s:cbd_out")):
        vim.command("let @* = '%s'" % hexcolor)

csd.destroy()

# clear status bar
vim.command(":redraw")
print ""

endpython
endfunction

