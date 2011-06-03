" -----------------------------------------------------------------------------
" CreateMenuPath Version 1.1
"
" Create a menu that mirrors a directory tree.
"
" Usage:
" CreateMenuPath({path} [, {menu name} [, {menu priority} [, {filename ignore pattern}]]])
"
" Default menu name is "Files", no default priority.
" A default filename ignore pattern is provided, but is far from "complete".
"
" Requires at least Vim6 to run.
"
" TODO :
"   Create sub-menus for menus with more than 30(?) entries.
" -----------------------------------------------------------------------------

function! CreateMenuPath(path, ...)

	let priority = ""
	let menuname = "Files"
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
		echoerr "Too many arguments."
		return
	endif

	silent! exe 'unmenu ' . menuname
	silent! exe 'unmenu! ' . menuname

	let files=glob(a:path . "/*")

	if files == ""
		return
	endif

	" Don't mess with this:
	let subpriority = 20

	let start = 0
	let match = 0

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

		let subpriority = subpriority + 10
		let file = escape(fnamemodify(fullfile, ":t"), "\\. \t|")

		if isdirectory(fullfile)
			call CreateMenuPath(fullfile, menuname . '.' . file, priority . '.' . subpriority, ignore_pat)
		else
			exe "amenu " . priority . '.' . subpriority
				\ . ' ' . menuname . '.' . file
				\ . " :confirm e " . escape(fullfile, ' |') . "<CR>"
		endif

	endwhile

	" Add cd/rescan items to this menu, if any files/submenus appeared in it:
	if subpriority > 20
		exe "amenu " . priority . '.10'
			\ . ' ' . menuname . '.CD\ Here'
			\ . ' :cd ' . a:path "<CR>"
		exe "amenu <silent> " . priority . '.20'
			\ . ' ' . menuname . '.Rescan'
			\ . ' :call CreateMenuPath(' . a:path . ', ' . priority . ', ' . ignore_pat . ")<CR>"
		exe "menu " . priority . '.25' . ' ' . menuname . '.-sep1- <nul>'
	endif

endfunction
