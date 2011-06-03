" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin\ColorSchemeMenuMaker.vim	[[[1
682
" ColorSchemeMenuMaker.vim:	Generates Vim ColorScheme menu and
" 					organizes themes based upon background colors
" Maintainer:		Erik Falor <ewfalor@gmail.com>
" Date:				Oct 10, 2007
" Version:			0.9
" License:			If you copy this, just give me props.
"
" History:
"   Version 0.9:	Tries not to create submenus which are too tall to fit
"					on the screen.  By default, generated submenus are split
"					so as to contain fewer than 45 items.  This number may be
"					overridden via g:MaxEntriesPerSubmenu.
"
"   Version 0.8.1:	Shuangshi Jie bugfix: changed to Unix line endings.
"
"   Version 0.8:	Avoid loading menu in console mode.
"
"   Version 0.7:	Fixed some bugs in SDK mode.  Tweaked IsDarkGrey().
"
"   Version 0.6:	Created an SDK mode that creates an HTML page
"   				which aids in tweaking the color selection algorithm.
"   				To enable, define the variable g:ColorSchemeSDKMode
"   				and install ColorSchemeSDK.vim in the autoload/
"   				directory.
"					Tweaked with IsYellow() and IsGreen() a bit... robinhood
"					now shows up as green instead of yellow.
"					Added IsPurple().
"					I have forgotten to mention that if a colorscheme has more
"					than one 'hi Normal ...' command its name will have a *
"					prepended to it.  If such a theme shows up in the wrong
"					color category, its because I guessed wrong at which
"					Normal group is used.  It also means that you may be able
"					to control the look and feel of the colorscheme by setting
"					variables in your .vimrc file.
"
" 	Version 0.5:	Store the generated menu under the same directory
" 					this file is located.  Thanks to Vincent Vandalon for
" 					pointing out that not all folks have a ~/vimfiles
" 					directory under WinXP.
"
" 	Version 0.4:	Switched to Unix line endings.  Look for rgb.txt under
"					/usr/share/X11 to accomodate Gentoo Linux.
"
" 	Version 0.3:	Now works on Linux by looking for rgb.txt in
"					/usr/X11R6/lib/X11 instead of $VIMRUNTIME.  If your
"					rgb.txt is kept	somewhere else, store that absolute
"					pathname in your .vimrc in a variable called g:rgbtxt.
"
" 	Version 0.2:	Menu categories include a count of contained items.	
"
" 	Version 0.1:	Initial upload
" GetLatestVimScripts: 2004 1 :AutoInstall: ColorSchemeMenuMaker.vba


" Initialization: {{{
if exists("g:loaded_theme_menu") && !exists("g:ColorSchemeSDKMode")
	finish
endif
let g:loaded_theme_menu= "0.6"
let s:keepcpo      = &cpo
set cpo&vim
"}}}

" Script Variables: {{{
"store the generated menu under the same path this file is found:
let s:MaxEntriesPerSubmenu = 45
let s:menuFile = expand('<sfile>:p:h') . '/ColorSchemes.vim'
let s:menuName = '&ColorSchemes'
let s:xdigit = '[0123456789ABCDEFabcdef]'
let s:hexvals = { 0:0, 1:1, 2:2, 3:3,
			\4:4, 5:5, 6:6, 7:7,
			\8:8, 9:9, 'a':10, 'b':11,
			\'c':12, 'd':13, 'e':14, 'f':15,
			\'A':10, 'B':11, 'C':12, 'D':13,
			\'E':14, 'F':15 }
"}}}

" Library Functions {{{
function! <SID>RGBtoHSV(r, g, b) "{{{
	let h = 0
	let s = 0
	let v = 0
	"blue is greatest
	if (a:b > a:g) && (a:b > a:r)
		let v = a:b
		if v != 0
			let min = 0
			if(a:r > a:g)
				let min = a:g
			else
				let min = a:r 
			endif

			let delta = v - min

			if delta != 0
				let s = (delta * 255) / v
				let h = 240 + (60 * a:r - 60 * a:g) / delta
			else 
				let s = 0
				let h = 240 + (60 * a:r - 60 * a:g)
			endif
			if h < 0 
				let h = h + 360 
			endif
		else 
			let s = 0
			let h = 0
		endif
	"green is greatest
	elseif a:g > a:r
		let v = a:g
		if v != 0
			let min = 0
			if a:r > a:b
				let min = a:b 
			else 
				let min = a:r 
			endif
			let delta = v - min
			if delta != 0
				let s = (delta * 255) / v
				let h = 120 + (60 * a:b - 60 * a:r) / delta
			else 
				let s = 0
				let h = 120 + (60 * a:b - 60 * a:r)
			endif
			if h < 0
				let h = h + 360 
			endif
		else 
			let s = 0
			let h = 0
		endif
	"red is greatest
	else
		let v = a:r
		if v != 0
			let min = 0
			if a:g > a:b
				let min = a:b
			else
				let min = a:g
			endif
			let delta = v - min
			if delta != 0
				let s = (delta * 255) / v
				let h = (60 * a:g - 60 * a:b) / delta
			else 
				let s = 0
				let h = 60 * a:g - 60 * a:b
			endif
			if h < 0
				let h = h + 360 
			endif
		else
			let s = 0
			let h = 0
		endif
	endif
	return [h, s, v]
endfunction "RGBtoHSV()
"}}}

function! <SID>IsBlack(r, g, b, h, s, v) "{{{
	if a:r == a:g && a:g == a:b && a:b == 0
		return 1
	else
		return 0
	endif
endfunction "IsBlack()}}}
	
function! <SID>IsWhite(r, g, b, h, s, v) "{{{
	if a:r == a:g && a:g == a:b && a:b == 255
		return 1
	else 
		return 0
	endif
endfunction "IsWhite()}}}

function! <SID>IsDarkGrey(r, g, b, h, s, v) "{{{
	let diffRGB = max([a:r, a:g, a:b]) - min([a:r, a:g, a:b])
	let darkGreyFuzz = 10
	if diffRGB <= darkGreyFuzz
		return 1
	else 
		return 0
	endif
endfunction "IsDarkGrey()}}}

function! <SID>IsOffWhite(r, g, b, h, s, v) "{{{
	let offWhiteSat = 32
	let offWhiteVal = 255 - 32
	if a:v >= offWhiteVal && a:s <= offWhiteSat
		return 1
	else 
		return 0
	endif
endfunction "}}}

function! <SID>IsGrey(r, g, b, h, s, v) "{{{
	let diffRGB = max([a:r, a:g, a:b]) -  min([a:r, a:g, a:b])
	let greyFuzz = 28
	let greyVal = 32

	if diffRGB > greyFuzz
		return 0
	elseif (a:s <= greyFuzz )
			\&& (a:v <= 255 - (greyVal * 1))
			\&& (a:v >= 0   + (greyVal * 1))
		return 1 
	else
		return 0
	endif
endfunction "}}}

function! <SID>IsYellow(r, g, b, h, s, v) "{{{
	if a:h > 30 && a:h <= 69
		return 1
	else 
		return 0
	endif
endfunction "}}}

function! <SID>IsGreen(r, g, b, h, s, v) "{{{
	if a:h > 70 && a:h <= 180
		return 1
	else 
		return 0
	endif
endfunction "}}}

function! <SID>IsCyan(r, g, b, h, s, v) "{{{
"	cyan will be 180 deg +/- 10 deg
	let variance = 10
	if a:h > 180 - variance && a:h < 180 + variance
		return 1
	else 
		return 0
	endif
endfunction "}}}

function! <SID>IsPurple(r, g, b, h, s, v) "{{{
	if a:r >= a:g && a:b > a:g && a:r != 0 && a:g != 0
		return 1
	endif
	return 0
endfunction "}}}

function! <SID>IsBlue(r, g, b, h, s, v) "{{{
	if a:h > 180 && a:h <= 270
		return 1
	else 
		return 0
	endif
endfunction "}}}

function! <SID>IsMagenta(r, g, b, h, s, v) "{{{
	if a:h > 270 && a:h <= 330
		return 1
	else 
		return 0
	endif
endfunction }}}

function! <SID>IsOrange(r, g, b, h, s, v) "{{{
	"a magic number found through trial and error
	let greenFuzz = 172 
	if a:r > a:g && a:b == 0 && a:g < greenFuzz && a:g != 0
		return 1
	else
		return 0
	endif
endfunction "}}}

function! <SID>IsRed(r, g, b, h, s, v) "{{{
	if a:h > 330 || a:h <= 30
		return 1
	else
		return 0
	endif
endfunction "}}}

function! <SID>FindRgbTxt() "{{{
	"read rgb.txt, return dictionary mapping color names to hex triplet
	if exists("g:rgbtxt") && filereadable(g:rgbtxt)
		let rgbtxt = g:rgbtxt
	else
		if has("win32") || has("win64")
			let rgbtxt = expand("$VIMRUNTIME/rgb.txt")
		elseif filereadable("/usr/X11R6/lib/X11/rgb.txt")
			let rgbtxt = "/usr/X11R6/lib/X11/rgb.txt"
		elseif filereadable("/usr/share/X11/rgb.txt")
			let rgbtxt = "/usr/share/X11/rgb.txt"
		endif
	endif
	return rgbtxt
endfunction "}}}

function! <SID>RgbTxt2Hexes() "{{{
	let rgbtxt = <SID>FindRgbTxt()
	let rgbdict = {}
	if filereadable(rgbtxt)
		for line in readfile(rgbtxt)
			if line !~ '^\(!\|#\)'
				let l = matchlist(line, '\s*\(\d\+\)\s*\(\d\+\)\s*\(\d\+\)\s*\(.*\)')
				let rgbdict[tolower(l[4])] = printf('%02X%02X%02X', l[1], l[2], l[3])
			endif
		endfor
		"note: vim treats guibg=NONE as guibg=white
		let rgbdict['none'] = 'FFFFFF'
	else
		echoerr "ColorSchemeMenuMaker.vim could not open rgb.txt file at " . rgbtxt 
	endif
	return rgbdict
endfunction "}}}

function! <SID>RGBHexToHexes(rgb) "{{{
	let xdigits = '\(' . s:xdigit . '\{2\}\)'
	let pat = '\(#\)\?' . xdigits . xdigits . xdigits
	let l = matchlist(a:rgb, pat)
	if len(l) > 0
		return [ l[2], l[3], l[4] ]
	else
		return []
	endif
endfunction "}}}

function! <SID>RGBHexToInts(rgbList) "{{{
	return map(a:rgbList, '<SID>Hex2Int(v:val)')
endfunction "}}}

function! <SID>Hex2Int(hex) "{{{
	let xdigits = split(a:hex, '\zs')
	return 16 * s:hexvals[xdigits[0]] + s:hexvals[xdigits[1]]
endfunction "}}}

function! <SID>RGB2BoyColor(rgb) "{{{
	let rgbL = <SID>RGBHexToInts(<SID>RGBHexToHexes(a:rgb))
	let r = rgbL[0] | let g = rgbL[1] | let b = rgbL[2]
	let hsvL = <SID>RGBtoHSV(r, g, b)
	let h = hsvL[0] | let s = hsvL[1] | let v = hsvL[2]
	if <SID>IsBlack(r, g, b, h, s, v) == 1 | return 'black' | endif
	if <SID>IsWhite(r, g, b, h, s, v) == 1 | return 'white' | endif
	if <SID>IsGrey(r, g, b, h, s, v) == 1 | return 'grey' | endif
	if <SID>IsOffWhite(r, g, b, h, s, v) == 1 | return 'offwhite' | endif
	if <SID>IsDarkGrey(r, g, b, h, s, v) == 1 | return 'darkgrey' | endif
	if <SID>IsOrange(r, g, b, h, s, v) == 1 | return 'orange' | endif
	if <SID>IsYellow(r, g, b, h, s, v) == 1 | return 'yellow' | endif
	if <SID>IsCyan(r, g, b, h, s, v) == 1 | return 'cyan' | endif
	if <SID>IsGreen(r, g, b, h, s, v) == 1 | return 'green' | endif
	if <SID>IsPurple(r, g, b, h, s, v) == 1 | return 'purple' | endif
	if <SID>IsBlue(r, g, b, h, s, v) == 1 | return 'blue' | endif
	if <SID>IsMagenta(r, g, b, h, s, v) == 1 | return 'magenta' | endif
	if <SID>IsRed(r, g, b, h, s, v) == 1 | return 'red' | endif
	return 'unknown'
endfunction "}}}

function! <SID>GlobThemes() "{{{
	"return list containing paths to all theme files in &runtimepath
	return split(globpath(&rtp, 'colors/*.vim'), '\n')
endfunction "}}}

function! <SID>ScanThemeBackgrounds() "{{{
	"Read each of the theme files and find out which color
	"each theme 'basically' is.  Uses the last 'hi Normal' 
	"group found to classify by color.  Notes those color
	"files that do have more than one 'hi Normal' command.
	let name2hex = <SID>RgbTxt2Hexes()
	let themeColors = {}
	let themeNames = {}
	let i = 0
	let pat = 'hi.*\s\+Normal\s\+.\{-}guibg=\(#\?\)\(\w\+\)'
	for theme in <SID>GlobThemes()
		if filereadable(theme)

			"DEBUG
			"let i = i + 1
			"if i > 10
				"break
			"endif

			let higroupfound = 0
			let color = ''
			for line in readfile(theme)
				let bg = matchlist(line, pat)
				if len(bg) > 0
					if bg[1] == '#'
						let color = <SID>RGB2BoyColor(bg[2])
					else
						if has_key(name2hex, tolower(bg[2]))
							let color = <SID>RGB2BoyColor(name2hex[tolower(bg[2])])
						else
							let color = 'unknown'
						endif
					endif
					let higroupfound += 1
				endif
			endfor
			let themename = fnamemodify(theme, ':t:r')
			let letter = toupper(strpart(themename, 0, 1))
			if letter =~ '\d' | let letter = '#' | endif

			if len(color) < 1 
				let color = 'unknown'
			endif

			"allocate sub-dict if needed
			if !has_key(themeColors, color)
				let themeColors[color] = {}
			endif
			"allocate sub-dict if needed
			if !has_key(themeNames, letter)
				let themeNames[letter] = {}
			endif
			if higroupfound > 1
				"mark themes with many 'hi Normal' commands
				if len(color) > 0
					let themeColors[color][themename] = '*' . themename
				endif
				let themeNames[letter][themename] = '*' . themename
			else
				if len(color) > 0
					let themeColors[color][themename] = themename
				endif
				let themeNames[letter][themename] = themename
			endif
		endif
	endfor
	return [themeColors, themeNames]
endfunction "}}}


function! <SID>BuildMenu(dicts) "{{{
	"puts menu commands into a list
	let menu = []

	if exists('g:MaxEntriesPerSubmenu')
		let MaxEntriesPerSubmenu = g:MaxEntriesPerSubmenu
	else
		let MaxEntriesPerSubmenu = s:MaxEntriesPerSubmenu
	endif

	call add(menu, '"ColorScheme menu generated ' . strftime("%c", localtime()))
	call add(menu, '')
	call add(menu, '"Do not load this menu unless running in GUI mode')
	call add(menu, 'if !has("gui_running") | finish | endif')
	call add(menu, '')

	"ColorSchemes Sub-Menu, by Color
	call add(menu, '"Themes by color:')
	"count number of themes categorized by color
	let totThemes = 0
	for i in keys(a:dicts[0])
		let totThemes += len(a:dicts[0][i])
	endfor
	for color in sort(keys(a:dicts[0]))
		call add(menu, '')
		call add(menu, '"submenu '. color)
		let numThemes = len(a:dicts[0][color])
		"if the number of themes for this color does not exceed the total for
		"any sub menu, go ahead and add them all to the same menu
		if numThemes <= MaxEntriesPerSubmenu
			for theme in sort(keys(a:dicts[0][color]))
				call add(menu, '9000amenu '. s:menuName. '.&Colors\ ('. totThemes . ').'
						\. color . '\ ('. numThemes . ').'
						\. a:dicts[0][color][theme]. '  :colo '. theme . '<CR>')
			endfor
		else
			let submenus = []
			let i = 0
			while i < numThemes / MaxEntriesPerSubmenu
				call add(submenus, MaxEntriesPerSubmenu)
				let i += 1
			endwhile
			if numThemes % MaxEntriesPerSubmenu != 0
				call add(submenus, numThemes % MaxEntriesPerSubmenu)
			endif
			if len(submenus) > 1 && submenus[-1] != submenus[-2]
				let submenus = <SID>BalanceSubmenu(submenus)
			endif
			let i = 0
			let j = 0
			for theme in sort(keys(a:dicts[0][color]))
				call add(menu, '9000amenu '. s:menuName. '.&Colors\ ('. totThemes . ').'
						\. color . '-' . string(i+1) . '\ ('. submenus[i] . ').'
						\. a:dicts[0][color][theme]. '  :colo '. theme . '<CR>')
				let j += 1
				if j == submenus[i]
					let j = 0
					let i += 1
				endif
			endfor
		endif
	endfor
	
	"ColorSchemes Sub-Menu, by Name
	call add(menu, '"Themes by name:')
	call add(menu, '')
	"count number of themes categorized by name
	let totThemes = 0
	for i in keys(a:dicts[1])
		let totThemes += len(a:dicts[1][i])
	endfor
	for letter in sort(keys(a:dicts[1]))
		let numThemes = len(a:dicts[1][letter])
		call add(menu, '')
		call add(menu, '"submenu '. letter)
		"if the number of themes for this letter does not exceed the total for
		"any sub menu, go ahead and add them all to the same menu
		if numThemes <= MaxEntriesPerSubmenu
			for theme in sort(keys(a:dicts[1][letter]))
				call add(menu, 'amenu '. s:menuName. '.&Names\ (' . totThemes . ').'
						\. letter . '\ ('. numThemes .').'
						\.  a:dicts[1][letter][theme] . '  :colo '. theme . '<CR>')
			endfor
		else
			let submenus = []
			let i = 0
			while i < numThemes / MaxEntriesPerSubmenu
				call add(submenus, MaxEntriesPerSubmenu)
				let i += 1
			endwhile
			if numThemes % MaxEntriesPerSubmenu != 0
				call add(submenus, numThemes % MaxEntriesPerSubmenu)
			endif
			if len(submenus) > 1 && submenus[-1] != submenus[-2]
				let submenus = <SID>BalanceSubmenu(submenus)
			endif
			let i = 0
			let j = 0

			for theme in sort(keys(a:dicts[1][letter]))
				call add(menu, '9000amenu '. s:menuName. '.&Names\ ('. totThemes . ').'
						\. letter . '-' . string(i+1) . '\ ('. submenus[i] . ').'
						\. a:dicts[1][letter][theme]. '  :colo '. theme . '<CR>')
				let j += 1
				if j == submenus[i]
					let j = 0
					let i += 1
				endif
			endfor
		endif
	endfor

	call add(menu, '')
	"add a separator and a command to re-init the menu
	call add(menu, 'amenu ' . s:menuName .'.-Sep-	:')
	call add(menu, 'amenu ' . s:menuName .'.Reload\ Menu	:ReloadColors<CR>')
	call add(menu, 'amenu ' . s:menuName .'.Refresh\ Menu	:RefreshColors<CR>')
	call add(menu, '')
	call add(menu, 'command! -nargs=0		ReloadColors		call <SID>ReloadColors()')
	call add(menu, 'command! -nargs=0		RefreshColors		call <SID>RefreshColors()')
	call add(menu, '')
	call add(menu, 'if !exists("g:running_ReloadColors")')
	call add(menu, '	function! <SID>ReloadColors()')
	call add(menu, '		let g:running_ReloadColors = 1')
	call add(menu, '		aunmenu ' . s:menuName)
	call add(menu, "		execute 'source " . s:menuFile . "'")
	call add(menu, '		unlet g:running_ReloadColors')
	call add(menu, "		echomsg 'Done Reloading " . s:menuFile . "'")
	call add(menu, '	endfunction')
	call add(menu, 'endif')

	call add(menu, 'if !exists("g:running_RefreshColors")')
	call add(menu, '	function! <SID>RefreshColors()')
	call add(menu, '		let g:running_RefreshColors = 1')
	call add(menu, '		call WriteColorSchemeMenu()')
	call add(menu, '		call <SID>ReloadColors()')
	call add(menu, '		unlet g:running_RefreshColors')
	call add(menu, "		echomsg 'Done Refreshing " . s:menuFile . "'")
	call add(menu, '	endfunction')
	call add(menu, 'endif')
	return menu
endfunction "BuildMenu}}}

function! <SID>BalanceSubmenu(s) "{{{
	let next2last = len(a:s) - 2
	let i = next2last

	while a:s[-1] < a:s[-2]
		let a:s[-1] += 1
		let a:s[i] -= 1
		if i > 0
			let i -= 1
		elseif a:s[-2] -1 == a:s[-1]
			break
		else
			let i = next2last
		endif
	endwhile
	return a:s
endfunction "BalanceSubmenu}}}

function! WriteColorSchemeMenu() "{{{
	"Builds the menu from the two dicts returned by ScanThemeBackgrounds()
	"Stores menu in first plugin dir specified by &rtp
	let dicts = <SID>ScanThemeBackgrounds()
	let menu = <SID>BuildMenu(dicts)
	call writefile(menu, s:menuFile)
endfunction "}}}

function! <SID>InitMenu() "{{{
	call WriteColorSchemeMenu()
	execute "source " . s:menuFile
endfunction "}}}
"}}}

"{{{ SDK-Mode Section
if exists("g:ColorSchemeSDKMode")
	let g:mapping = 'map <F5> :so ' . g:ColorSchemeSDKMode . 
		\'/autoload/ColorSchemeSDK.vim \| so ' .
		\g:ColorSchemeSDKMode . '/plugin/ColorSchemeMenuMaker.vim ' .
		\'\| call GenHTML() <CR>'
	execute g:mapping

	"get the script ID for functions in this script
	function! <SID>SID() "{{{
		return '<SNR>' . matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_SID$') . '_'
	endfunction "}}}

	let s:sid = <SID>SID()

	let s:CSMMfuncs = { 
				\'RGBtoHSV'					: function(s:sid . "RGBtoHSV"),
				\'IsBlack'					: function(s:sid . "IsBlack"),
				\'IsWhite'					: function(s:sid . "IsWhite"),
				\'IsDarkGrey'				: function(s:sid . "IsDarkGrey"),
				\'IsOffWhite'				: function(s:sid . "IsOffWhite"),
				\'IsGrey'					: function(s:sid . "IsGrey"),
				\'IsYellow'					: function(s:sid . "IsYellow"),
				\'IsGreen'					: function(s:sid . "IsGreen"),
				\'IsCyan'					: function(s:sid . "IsCyan"),
				\'IsBlue'					: function(s:sid . "IsBlue"),
				\'IsPurple'					: function(s:sid . "IsPurple"),
				\'IsMagenta'				: function(s:sid . "IsMagenta"),
				\'IsOrange'					: function(s:sid . "IsOrange"),
				\'IsRed'					: function(s:sid . "IsRed"),
				\'FindRgbTxt'				: function(s:sid . "FindRgbTxt"),
				\'RgbTxt2Hexes'				: function(s:sid . "RgbTxt2Hexes"),
				\'RGBHexToHexes'			: function(s:sid . "RGBHexToHexes"),
				\'RGBHexToInts'				: function(s:sid . "RGBHexToInts"),
				\'Hex2Int'					: function(s:sid . "Hex2Int"),
				\'RGB2BoyColor'				: function(s:sid . "RGB2BoyColor"),
				\'GlobThemes'				: function(s:sid . "GlobThemes"),
				\'ScanThemeBackgrounds'		: function(s:sid . "ScanThemeBackgrounds"),
				\'BuildMenu'				: function(s:sid . "BuildMenu") }

	"let g:CSMMfuncs = s:CSMMfuncs 

	function! CallIt(key, ...) 
		return ColorSchemeSDK#Invoke1(s:CSMMfuncs[a:key], a:1)
	endfunction

	"creates an html file named a:destfile that shows the background color
	"along with the color guessed by this plugin
	function! GenHTML(...)
		if 0 == a:0 
			let destfile = 'ColorScheme.html'
		else
			let destfile = a:1
		endif
		return ColorSchemeSDK#GenHTML(destfile, s:CSMMfuncs)
	endfunction
	echom "Using ColorSchemeMenuMaker in SDK mode!"
	echom "unlet g:ColorSchemeSDKMode to disable"
endif
"}}}

" Restore &cpo: {{{1
let &cpo= s:keepcpo
unlet s:keepcpo
"}}}1

"Detect absence of ColorScheme menu, and generate a new one automatically
if has("gui_running") && !filereadable(s:menuFile) "{{{
	echomsg "Creating ColorScheme menu - Please Wait..."
	call <SID>InitMenu()
	echomsg "Done!"
endif "}}}

"  vim: tabstop=4 foldmethod=marker
autoload\ColorSchemeSDK.vim	[[[1
464
" ColorSchemeSDK.vim:	Creates HTML table of color schemes to assist
" 					in tweaking the color selection algorithm.
" Maintainer:		Erik Falor <rAjsBnFCybe@tzNnvy.Zpbz g?? - NOSPAM>
" Date:				Sept 14, 2007
" Version:			0.6
" License:			If you copy this, just give me props.
"
" History:
"   Version 0.1:	Created an SDK mode that creates an HTML page
"   				which aids in tweaking the color selection algorithm.
"   				To enable this mode, set the variable
"   				g:ColorSchemeSDKMode to point to the directory where
"   				your plugins are installed: this is used to define a
"   				key mapping that will generate the HTML in the CWD.
"   				
"   				For example, if you
"   					:let g:ColorSchemeSDKMode = '~/.vim'
"   				then the F5 key will execute the following command:
"so ~/.vim/autoload/ColorSchemeSDK.vim | so ~/.vim/plugin/ColorSchemeMenuMaker.vim | call GenHTML() 

" Functions {{{
"You can't return a Dict from VIM::Eval() because (apparently) it would
"be using a Dict as a String, which is a no-no.  This wrapper turns the
"Dict into a List, which can be stringified.
function! ColorSchemeSDK#DictHelper(d) "{{{
	let list = []
	for [k,v] in items(a:d)
		call add(list, k . ':' . v)
	endfor
	return list
endfunction "}}}

"Teh workhorse function... called from within ColorSchemeMenuMaker.vim
function! ColorSchemeSDK#GenHTML(dest, funcs) "{{{
	echom "If this is the only message you can see, there's something wrong with your perl"
	perl <<GenerateHTML
#
#Perl script begin
#
use File::Basename;

#{{{ utility functions
@Colors = qw(white black yellow green cyan blue magenta red grey offwhite);
@ColorNames = @Colors;
%ColorNamesToHex = ( 	white	 => 'FFFFFF',
						black	 => '000000',
						yellow	 => 'FFFF00',
						green	 => '00FF00',
						cyan	 => '00FFFF',
						blue	 => '0000FF',
						magenta	 => 'FF00FF',
						red		 => 'FF0000',
						grey	 => '808080',
						offwhite => 'FFF5EE',
						darkgrey => '1A1A1A',
						unknown  => 'FFFFFF',
						purple	 => '7D26CD',
						orange   => 'FFA500',
					);
%HexToColorNames = ( 	'FFFFFF'	=> 'white',
						'000000'	=> 'black',
						'FFFF00'	=> 'yellow',
						'00FF00'	=> 'green',
						'00FFFF'	=> 'cyan',
						'0000FF'	=> 'blue',
						'FF00FF'	=> 'magenta',
						'FF0000'	=> 'red',
						'808080'	=> 'grey',
						'FFF5EE'	=> 'offwhite',
						'1A1A1A'	=> 'darkgrey',
						'FFFFFF'	=> 'unknown',
						'7D26CD'	=> 'purple',
						'FFA500'	=> 'orange',
					);

#return dict mapping rgb.txt entries to a Hex Triplet
sub Name2RGB {
	#the first item returned in List context is the success of the Eval()'ed function.
	(undef, @vimDict) = VIM::Eval('ColorSchemeSDK#DictHelper(a:funcs["RgbTxt2Hexes"]())');
	%simple = map {split /[:\n]/} @vimDict;
	$simple{none} = '#FFFFFF';
	return \%simple;
}

sub xorColor {
	if ( $_[0] == $_[1] and $_[1] == $_[2] and ($_[0] != 0 and $_[0] != 255)) {
		if ($_[0] >= 128 and $_[0] < 164) {
			#return map { abs(($_ ^ 255 - 128) % 255)  } @_
			return(0, 0, 0);
		}
		elsif ($_[0] < 128 and $_[0] > 92) {
			#return map { abs(($_ - 64) % 255)  } @_
			return(255, 255, 255);
		}
	}
	return map { $_ ^ 255 } @_
}

sub PrintThemeRow { #{{{
	print <<ROW; 
				<tr>
					<td width="20%" bgcolor="$notify">
						<a href="file://$theme">$themeName</a>
					</td>
					<!-- ACTUAL Hex RGB -->
					<td width="10%" align="center" bgcolor="$actual">
						<font style="color:@{[sprintf('#%.2X%.2X%.2X', @xors)]}; font-weight:bold">
							@{[sprintf('#%.2X%.2X%.2X', $red, $green, $blue ) ]}
						</font>
					</td>
					<!-- ACTUAL Color RGB -->
					<td width="15%" align="center" bgcolor="$actual">
						<font style="color:@{[sprintf('#%.2X%.2X%.2X', @xors)]}; font-weight:bold">
							@{[sprintf('rgb(%d, %d, %d)', $red, $green, $blue,) ]}
						</font>
					</td>
					<!-- ACTUAL Color HSV -->
					<td width="15%" align="center" bgcolor="$actual">
						<font style="color:@{[sprintf('#%.2X%.2X%.2X', @xors)]}; font-weight:bold">
							@{[sprintf('hsv(%d, %d, %d)', $h, $s, $v) ]}
						</font>
					</td>
					<!-- ACTUAL Color -->
					<td width="10%" bgcolor="@{[sprintf('#%.2X%.2X%.2X', $red, $green, $blue) ]}"/>
					<!-- GUESSED COLOR 1 -->
					<td width="10%" bgcolor="#@{[$ColorNamesToHex{$guess1{$theme}}]}">
						<font style="color:@{[sprintf('#%.2X%.2X%.2X', @guessXors)]}; font-weight:bold">$guess1{$theme}</font>
						<!-- <font style="color:#FFFFFF; font-weight:bold">$guess1{$theme}</font> -->
					</td>
					<!-- GUESSED COLOR 2 -->
					<td width="10%" bgcolor="#@{[$ColorNamesToHex{$guess2{$theme}}]}">
						<font style="color:@{[sprintf('#%.2X%.2X%.2X', @guess2Xors)]}; font-weight:bold">$guess2{$theme}</font>
					</td>
					<!-- GUESSED COLOR 3 -->
					<td width="10%" bgcolor="#@{[$ColorNamesToHex{$guess3{$theme}}]}">
						<font style="color:@{[sprintf('#%.2X%.2X%.2X', @guess3Xors)]}; font-weight:bold">$guess3{$theme}</font>
					</td>
				</tr>
ROW
} #}}}

sub PrintColorRow { #{{{
	print <<ROW; 
				<tr>
					<td width="20%" bgcolor="$notify">
						<b>$colorName</b>
					</td>
					<!-- ACTUAL Hex RGB -->
					<td width="10%" align="center" bgcolor="$actual">
						<font style="color:@{[sprintf('#%.2X%.2X%.2X', @xors)]}; font-weight:bold">
							@{[sprintf('#%.2X%.2X%.2X', $red, $green, $blue ) ]}
						</font>
					</td>
					<!-- ACTUAL Color RGB -->
					<td width="15%" align="center" bgcolor="$actual">
						<font style="color:@{[sprintf('#%.2X%.2X%.2X', @xors)]}; font-weight:bold">
							@{[sprintf('rgb(%d, %d, %d)', $red, $green, $blue,) ]}
						</font>
					</td>
					<!-- ACTUAL Color HSV -->
					<td width="15%" align="center" bgcolor="$actual">
						<font style="color:@{[sprintf('#%.2X%.2X%.2X', @xors)]}; font-weight:bold">
							@{[sprintf('hsv(%d, %d, %d)', $h, $s, $v) ]}
						</font>
					</td>
					<!-- ACTUAL Color -->
					<td width="10%" bgcolor="@{[sprintf('#%.2X%.2X%.2X', $red, $green, $blue) ]}"/>
					<!-- GUESSED COLOR 1 -->
					<td width="10%" bgcolor="#@{[$ColorNamesToHex{$guess1{$colorName}}]}">
						<font style="color:@{[sprintf('#%.2X%.2X%.2X', @guessXors)]}; font-weight:bold">$guess1{$colorName}</font>
						<!-- <font style="color:#FFFFFF; font-weight:bold">$guess1{$colorName}</font> -->
					</td>
					<!-- GUESSED COLOR 2 -->
					<td width="10%" bgcolor="#@{[$ColorNamesToHex{$guess2{$colorName}}]}">
						<font style="color:@{[sprintf('#%.2X%.2X%.2X', @guess2Xors)]}; font-weight:bold">$guess2{$colorName}</font>
					</td>
					<!-- GUESSED COLOR 3 -->
					<td width="10%" bgcolor="#@{[$ColorNamesToHex{$guess3{$colorName}}]}">
						<font style="color:@{[sprintf('#%.2X%.2X%.2X', @guess3Xors)]}; font-weight:bold">$guess3{$colorName}</font>
					</td>
				</tr>
ROW
} #}}}

sub PrintTableTail { #{{{
	print <<TABLETAIL;
				</tbody>
			</table>
TABLETAIL
} #}}}

sub PrintHTMLHead { #{{{
	print <<HTMLHEAD;
<html>
    <head>
        <title>Vim Color Scheme Guessed Background Colors</title>
    </head>
    <body>
    <h1>Vim Color Scheme Guessed Background Colors</h1>
HTMLHEAD
} #}}}

sub PrintThemeTableHead { #{{{
	print <<TABLEHEAD;
			<table width="100%" align="center" border="1px solid #AAAAAA">
				<tbody>
					<tr>
						<td width="20%">
							<b>Theme Name</b>
						</td>
						<td width="10%">
							<b>Actual Hex RGB</b>
						</td>
						<td width="15%">
							<b>Actual Color RGB</b>
						</td>
						<td width="15%">
							<b>Actual Color HSV</b>
						</td>
						<td width="10%">
							<b>Actual Color</b>
						</td>
						<td width="10%">
							<b>Guessed Color 1</b>
						</td>
						<td width="10%">
							<b>Guessed Color 2</b>
						</td>
						<td width="10%">
							<b>Guessed Color 3</b>
						</td>
					</tr>
TABLEHEAD
} #}}}

sub PrintColorTableHead { #{{{
	print <<TABLEHEAD;
			<table width="100%" align="center" border="1px solid #AAAAAA">
				<tbody>
					<tr>
						<td width="20%">
							<b>Color Name</b>
						</td>
						<td width="10%">
							<b>Actual Hex RGB</b>
						</td>
						<td width="15%">
							<b>Actual Color RGB</b>
						</td>
						<td width="15%">
							<b>Actual Color HSV</b>
						</td>
						<td width="10%">
							<b>Actual Color</b>
						</td>
						<td width="10%">
							<b>Guessed Color 1</b>
						</td>
						<td width="10%">
							<b>Guessed Color 2</b>
						</td>
						<td width="10%">
							<b>Guessed Color 3</b>
						</td>
					</tr>
TABLEHEAD
} #}}}

sub PrintHTMLTail { #{{{
	print <<HTMLTAIL;
    </body>
</html>
HTMLTAIL
} #}}}

sub RGBHexToHexes {
	return shift =~ /#?([[:xdigit:]]{2})([[:xdigit:]]{2})([[:xdigit:]]{2})/
}

sub RGBHexToInts {
	return map {hex} shift =~ /#?([[:xdigit:]]{2})([[:xdigit:]]{2})([[:xdigit:]]{2})/
}
#}}}

$name2hex = Name2RGB();
$hostname = VIM::Eval("hostname()");
$destfile = VIM::Eval("a:dest");

%guess3 = %guess2;
%guess2 = %guess1;

open $OUT_HTML, '>', $destfile
	or die "Cannot open '$destfile' for write because !$\n";
select $OUT_HTML;

PrintHTMLHead();

our $oldDir;
our $now = time;

foreach our $theme ( split /\n/, VIM::Eval('a:funcs["GlobThemes"]()') ) { #{{{
	our $dir = dirname($theme);

	if ($oldDir ne $dir) { #{{{
		if (defined $oldDir) {
			PrintTableTail();
		}
		$oldDir = $dir;

		print <<NEWPATH;
			<br/>
			<a href="file://$dir"><h2>$dir</h2></a>
NEWPATH
		PrintThemeTableHead();
	}# if ($oldDir ne $dir)  }}}

	(our $themeName) = basename($theme) =~ /([^.]+)/;
	print "<!-- $themeName - $theme -->\n";
	open my $THM, '<', $theme
		or die "Can't open '$theme' for read because: $!\n";

	while ($line = <$THM>) {
		next unless $line;
		next if $line =~ /^\s*$/;
		last if $line =~ /^exe.*\s+Normal/;
		next unless $line =~ /hi.*\s+Normal\s+.*?guibg/;
		($guibg) = $line =~ /guibg=(#?\w+)/;
	}
	our $actual;
	if (defined $guibg and $guibg =~ /^#?[[:xdigit:]]{6}$/) {
		print "<!-- guibg is defined and #xxxxxx-->\n";
		$actual = $guibg;
		$guess1{$theme} = VIM::Eval("a:funcs['RGB2BoyColor'](\"$guibg\")");
	}
	elsif (defined $guibg and $guibg =~ /^\w+$/
			and exists $$name2hex{lc $guibg}) {
		print "<!-- guibg is defined and is in rgb.txt-->\n";
		$actual = $$name2hex{lc $guibg};
		$guess1{$theme} = VIM::Eval("a:funcs['RGB2BoyColor'](\"$actual\")");
	}
	else {
		print "<!-- guibg is NOT defined -->\n";
		$actual = '#FFFFFF';
		$guess1{$theme} = VIM::Eval("a:funcs['RGB2BoyColor'](\"$actual\")");
	}

	our ($Xred, $Xgreen, $Xblue) =
		$actual =~  /#?([[:xdigit:]]{2})([[:xdigit:]]{2})([[:xdigit:]]{2})/;
	next if ($Xred eq undef);
	
	our ($red, $green, $blue) = map {hex} ($Xred, $Xgreen, $Xblue);
	our ($h, $s, $v) = split /\n/, VIM::Eval(qq{a:funcs['RGBtoHSV']($red, $green, $blue)});
	print "<!--**$themeName\n" .
		"guess=$guess1{$theme}\n".
		"guibg=$guibg\n".
		"actual=$actual\n".
		"hexes=$Xred$Xgreen$Xblue\n".
		"rgb($red, $green, $blue)\n".
		"hsv($h, $s, $v)**-->\n";

	our @xors = xorColor( $red, $green, $blue );
	our @guessXors = xorColor( map {hex $_} $ColorNamesToHex{$guess1{$theme}} =~ /([[:xdigit:]]{2})([[:xdigit:]]{2})([[:xdigit:]]{2})/ );

	our @guess2Xors;
	if ( $guess2{$theme} eq 'unknown' ) {
		@guess2Xors = (255, 0, 0)
	}
	else {
		@guess2Xors = xorColor( map {hex $_} $ColorNamesToHex{$guess2{$theme}} =~ /([[:xdigit:]]{2})([[:xdigit:]]{2})([[:xdigit:]]{2})/ );
	}

	our @guess3Xors;
	if ( $guess3{$theme} eq 'unknown' ) {
		@guess3Xors = (255, 0, 0)
	}
	else {
		@guess3Xors = xorColor( map {hex $_} $ColorNamesToHex{$guess3{$theme}} =~ /([[:xdigit:]]{2})([[:xdigit:]]{2})([[:xdigit:]]{2})/ );
	}

	our $notify;
	if ( ($guess1{$theme} ne $guess2{$theme} and exists $guess2{$theme})
		or ($guess1{$theme} ne $guess3{$theme} and exists $guess3{$theme}) ) {
		$notify = '#FF0000'
	}
	else {
		$notify = '#FFFFFF'
	}
	PrintThemeRow();

	close $THM
		or die "Can't close '$theme' because: $!\n";
} #}}} foreach our $theme ( split /\n/, VIM::Eval('a:funcs["GlobThemes"]()') )
PrintTableTail();


my $rgbtxt = VIM::Eval('a:funcs["FindRgbTxt"]()');
print <<RGBTXT;
			<br/>
			<a href="file://$rgbtxt"><h2>$rgbtxt</h2></a>
RGBTXT
PrintColorTableHead();
foreach our $colorName ( sort keys %$name2hex ) {
	our $color = $$name2hex{$colorName};
	our $actual;
	if (defined $color and $color =~ /^#?[[:xdigit:]]{6}$/) {
		$actual = $color;
		$guess1{$colorName} = VIM::Eval("a:funcs['RGB2BoyColor'](\"$color\")");
	}
	our ($Xred, $Xgreen, $Xblue) =
		$actual =~  /#?([[:xdigit:]]{2})([[:xdigit:]]{2})([[:xdigit:]]{2})/;
	next if ($Xred eq undef);
	our ($red, $green, $blue) = map {hex} ($Xred, $Xgreen, $Xblue);
	our ($h, $s, $v) = split /\n/, VIM::Eval(qq{a:funcs['RGBtoHSV']($red, $green, $blue)});
	print "<!--**$colorName => $color\n" .
		"guess=$guess1{$colorName}\n".
		"actual=$actual\n".
		"hexes=$Xred$Xgreen$Xblue\n".
		"rgb($red, $green, $blue)\n".
		"hsv($h, $s, $v)**-->\n";
	our @xors = xorColor( $red, $green, $blue );
	our @guessXors = xorColor( map {hex $_} $ColorNamesToHex{$guess1{$colorName}} =~ /([[:xdigit:]]{2})([[:xdigit:]]{2})([[:xdigit:]]{2})/ );

	our @guess2Xors;
	if ( $guess2{$theme} eq 'unknown' ) {
		@guess2Xors = (255, 0, 0)
	}
	else {
		@guess2Xors = xorColor( map {hex $_} $ColorNamesToHex{$guess2{$colorName}} =~ /([[:xdigit:]]{2})([[:xdigit:]]{2})([[:xdigit:]]{2})/ );
	}

	our @guess3Xors;
	if ( $guess3{$theme} eq 'unknown' ) {
		@guess3Xors = (255, 0, 0)
	}
	else {
		@guess3Xors = xorColor( map {hex $_} $ColorNamesToHex{$guess3{$colorName}} =~ /([[:xdigit:]]{2})([[:xdigit:]]{2})([[:xdigit:]]{2})/ );
	}

	our $notify;
	if ( ($guess1{$colorName} ne $guess2{$colorName} and exists $guess2{$colorName})
		or ($guess1{$colorName} ne $guess3{$colorName} and exists $guess3{$colorName}) ) {

		$notify = '#FF0000'
	}
	else {
		$notify = '#FFFFFF'
	}
	PrintColorRow();

}
PrintTableTail();

PrintHTMLTail();

VIM::Msg("Created $destfile in " . VIM::Eval("getcwd()") . " on " . $hostname);
close $OUT_HTML;
GenerateHTML
	return "done"
endfunction

"}}}

"}}}
" vim:foldmethod=marker:
