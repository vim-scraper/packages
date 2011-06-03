" -----------------------------------------------------------------------------
"                          CreateMenuPath Version 1.4
"  Written and Copyright 2002 by Christian J. Robinson <infynity@onewest.net>
"        Distributed under the terms of the GNU GPL Version 2 or later.
" -----------------------------------------------------------------------------
"
" Purpose:
" Create a menu that mirrors a directory tree.
"
" Usage:
" CreateMenuPath({path} [, {menu name} [, {menu priority} [, {filename ignore pattern}]]])
"
" Default menu name is "Files", no default priority.
" A default filename ignore pattern is provided, but is far from "complete".
"
" The global variable "CMP_Recurse_Limit" can be set to specify the maximum
" sub-directory depth to scan.  Set it to 0 (the default) for infinite
" recursion.  Set it to 1 to not scan only the specified directory.
"
" Requires at least Vim6 to run.
"
" -----------------------------------------------------------------------------

let s:cpo_save = &cpo
set cpo&vim

if ! exists('g:CMP_Recurse_Limit')
	let g:CMP_Recurse_Limit = 0
endif
let s:recursions = 1

function! CreateMenuPath(path, ...)

	let cpo_save = &cpo
	set cpo&vim

	let priority = ''
	let menuname = 'Files'
	let ignore_pat = '\%\(\CRCS\|CVS\|\.\%\(\c'
		\ . 'png\|gif\|jpe\=g\|ico\|bmp\|tiff\='
		\ . '\|mpg\|mov\|avi\|rm\|qt'
		\ . '\|zip\|tar\|tar\.gz\|tgz\|tar\.bz2'
		\ . '\|mp[32]\|wav\|au\|ogg\|mid'
		\ . '\|exe'
		\ . '\)\)$'

	if a:0 >= 1
		let menuname = a:1
	endif
	if a:0 >= 2
		let priority = a:2
	endif
	if a:0 == 3
		let ignore_pat = a:3
	elseif a:0 > 3
		echoerr 'Too many arguments.'
		return
	endif

	let originalpriority = priority
	let originalmenuname = menuname

	" Remove old menu if it exists; create a dummy entry to keep a torn off
	" menu from disappearing:
	silent! exe 'unmenu ' . menuname
	silent! exe 'noremenu ' . originalpriority . '.1 ' . originalmenuname . '.Dummy <Nop>'
	silent! exe 'unmenu! ' . menuname

	let files=glob(a:path . "/*")

	if files == ""
		silent! exe 'unmenu ' . originalmenuname . '.Dummy'
		return
	endif

	" Shortcut key list (C, R and M will be used elsewhere, so don't include
	" them):
	let shortcutlist = '1234567890ABDEFGHIJKLNOPQSTUVWXYZ'

	" Don't mess with this:
	let subpriority = 20

	let start = 0
	let match = 0
	let i = 0

	while (match != -1)

		let match = match(files, "\n", start)
		if match == -1
			let fullfile = strpart(files, start)
		else
			let fullfile = strpart(files, start, match - start)
			let start = match + 1
		endif

		if match(fullfile, ignore_pat) > -1
			continue
		endif

		if i >= 29 && match > -1
			let menuname = menuname . '.\.\.\.&More'
			let priority = priority . '.' . subpriority
			let subpriority = 20
			let i = 0
		else
			let subpriority = subpriority + 10
		endif

		let file = escape(fnamemodify(fullfile, ':t'), "\\. \t|")

		if isdirectory(fullfile)

			if g:CMP_Recurse_Limit > 0 && s:recursions >= g:CMP_Recurse_Limit
				let subpriority = subpriority - 10
				continue
			endif

			let s:recursions = s:recursions + 1

			call CreateMenuPath(
				\ fullfile,
				\ menuname . '.' . '&' . shortcutlist[i] . '\.\ \ ' . file,
				\ priority . '.' . subpriority,
				\ ignore_pat
			\)

			let s:recursions = s:recursions - 1

		else

			exe 'anoremenu ' . priority . '.' . subpriority
				\ . ' ' . menuname . '.' . '&' . shortcutlist[i] . '\.\ \ ' . file
				\ . ' :confirm e ' . escape(fullfile, ' |"') . '<CR>'

		endif

		let i = i + 1

	endwhile

	" Add cd/refresh items to this menu, if any files/submenus appeared in it:
	if subpriority > 20
		exe 'anoremenu ' . originalpriority . '.10'
			\ . ' ' . originalmenuname . '.&CD\ Here'
			\ . ' :cd ' . escape(a:path, ' |"') . '<CR>'
		exe 'anoremenu <silent> ' . originalpriority . '.20'
			\ . ' ' . originalmenuname . '.&Refresh'
			\ . " :call CreateMenuPath('"
				\ . a:path . "', '"
				\ . originalmenuname . "', '"
				\ . originalpriority . "', '"
				\ . escape(ignore_pat, '|') .
			\ "')<CR>"
		exe 'anoremenu ' . originalpriority . '.25' . ' ' . originalmenuname . '.-sep1- <Nop>'
	endif

	silent! exe 'unmenu ' . originalmenuname . '.Dummy'

	let &cpo = cpo_save

endfunction

let &cpo = s:cpo_save
unlet s:cpo_save
