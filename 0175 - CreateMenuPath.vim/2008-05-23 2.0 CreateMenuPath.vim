" -----------------------------------------------------------------------------
"                         CreateMenuPath -- Version 2.0
"      Copyright 2002-2008 by Christian J. Robinson <infynity@onewest.net>
"     Distributed under the terms of the Vim license.  See ":help license".
" -----------------------------------------------------------------------------
"
" Purpose:
" Create a menu that mirrors a directory tree.
"
" Usage:
" CreateMenuPath({path} [, {menu name} [, {menu priority} [, {filename ignore pattern1} [, {filename ignore pattern2} ...]]]])
"
" The default menu name is "Files", with no default priority.
"
" A default filename ignore pattern is provided, but it is far from
" "complete".
"
" The global variable "CMP_Recurse_Limit" can be set to specify the maximum
" sub-directory depth to scan.  Set it to 0 (the default) for infinite
" recursion.  Set it to 1 to scan only the specified directory.
"
" Requires at least Vim 7 to run.
"
" -----------------------------------------------------------------------------

if v:version < 700
	finish
endif

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
	let ignore_pats = [
				\ '\C\%\(RCS\|CVS\)',
				\ '\c\.\%\(png\|gif\|jpe\=g\|ico\|bmp\|tiff\=\|mpg\|mov\|avi\|rm\|qt\|zip\|tar\|tar\.gz\|tgz\|tar\.bz2\|mp[32]\|wav\|au\|ogg\|mid\|exe\)$',
				\ '\~$',
			\ ]

	if a:0 >= 1 && a:1 != ''
		let menuname = a:1
	endif
	if a:0 >= 2 && a:2 != ''
		let priority = a:2
	endif
	if a:0 >= 3
		if type(a:3) == 3
			let ignore_pats = a:3
		else
			let ignore_pats = a:000[2:-1]
		endif
	endif

	let originalpriority = priority
	let originalmenuname = menuname

	" Remove old menu if it exists; create a dummy entry to keep a torn off
	" menu from disappearing:
	silent! exe 'unmenu ' . menuname
	silent! exe 'noremenu '
		\ . originalpriority . '.1 '
		\ . originalmenuname . '.Dummy <Nop>'
	silent! exe 'unmenu! ' . menuname

	if (
		\ a:path =~ '/$' || 
		\ (
			\ (has('win32') || has('win16') 
				\ || has('dos16') || has('dos32'))
			\ && a:path =~ '\\$'
		\ )
	\)
		let files=split(glob(a:path . '*'), "\n")
	else
		let files=split(glob(a:path . '/*'), "\n")
	endif

	if len(files) == 0
		silent! exe 'unmenu ' . originalmenuname . '.Dummy'
		return
	endif

	" Shortcut key list (C, R and M will be used elsewhere,
	" so don't include them):
	let shortcutlist = '1234567890ABDEFGHIJKLNOPQSTUVWXYZ'

	" Don't mess with this:
	let subpriority = 20

	let i = 0

	for fullfile in files

		let ignore=0
		for pat in ignore_pats
			if match(fullfile, pat) > -1
				let ignore=1
				break
			endif
		endfor
		if ignore
			continue
		endif

		if i >= 29 && exists('files[' . (i + 1) . ']')
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
				\ menuname . '.' . '&' . shortcutlist[i]
					\ . '\.\ \ ' . file,
				\ priority . '.' . subpriority,
				\ ignore_pats
			\)

			let s:recursions = s:recursions - 1

		else

			exe 'anoremenu ' . priority . '.' . subpriority
				\ . ' ' . menuname . '.' . '&'
				\ . shortcutlist[i] . '\.\ \ ' . file
				\ . ' :confirm e ' . escape(fullfile, ' |"')
				\ . '<CR>'

		endif

		let i = i + 1

	endfor

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
				\ . escape(join(ignore_pats, "', '"), '|') .
			\ "')<CR>"
		exe 'anoremenu ' . originalpriority . '.25'
			\ . ' ' . originalmenuname . '.-sep1- <Nop>'
	endif

	silent! exe 'unmenu ' . originalmenuname . '.Dummy'

	let &cpo = cpo_save

endfunction

let &cpo = s:cpo_save
unlet s:cpo_save
