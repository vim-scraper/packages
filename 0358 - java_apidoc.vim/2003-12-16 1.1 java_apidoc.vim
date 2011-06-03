" java_apidoc.vim v1.0 by Paul Etchells <etch@etch.org.uk>
" based on work by Darren Greaves <darren@krapplets.org> - Thanx for
" giving me the idea and a good chunk of the code!
"
" DESCRIPTION
" Opens a browser showing the Javadoc for the imported package on the same
" line as the cursor, or for the class name under the cursor.
"
" USE
" Default install is activated with the <F2> key and destroys the j register.
" Tested and working on GVim 6.1 on Linux.
"
" INSTALL
" 1) Put this file in ~/.vim/ftplugin (or wherever Vim looks for plugins).
" 2) (gVIM) Add the following lines to ~/.vimrc
"-----------------------------------------------------------------------------
"  au FileType java let browser="xterm --geometry 100x40 -e lynx"
"  au FileType java let java_api_path="/home/etch/Dox/java-1.4-api,/home/etch/Dox/ejb"
"  au FileType java nmap <F2> viw"jy:call OpenJavadoc("j")
"-----------------------------------------------------------------------------
"
" (Note:  is CTRL-V CTRL-R,  is CTRL-V CTRL-M)
"
" CONFIGURATION - BROWSER
" If you don't want to use xterm and lynx to show the help, you can change
" 'browser="..."' to whatever browser is on your system.
" e.g. To browse with Opera
"-----------------------------------------------------------------------------
" au FileType java let browser="opera"
"-----------------------------------------------------------------------------
"
" If you try this with Netscape it may complain about a 'lock' file, since it
" tries to start a new instance of the browser for each page found. Of course,
" you can just say 'OK' at the dialogue, but it gets irritating. Mozilla tries
" to start a new instance with a different profile, so neither of these
" browsers are usable with this macro :o(
"
" CONFIGURATION - JAVA API PATH
" Just set the java_api_path variable to a comma separated list of paths to
" the tops of the Javadoc trees.
"
" CONFIGURATION - KEY AND REGISTER ASSIGNMENT
" The third line uses the <F2> key to start the macro, and the j register to
" act as a temporary visual buffer for getting the word under the cursor into
" the function call. Change these if you use them for something else.
" 
" CAVEAT
" It can be pretty slow when looking for a class name and your java_api_path
" contains a lot of files.

" Set this to 0 if you really want all the class_use files as well.
let s:skip_class_use = 1

function! OpenJavadoc(classname)
	let line = getline(".")
	let regex = '^import\s\+\(\S\+\);$'
	let l = matchstr(line, regex)
	let file = substitute(l, regex, '\1', '')
	let null = ''
	let s:found = 0

	let file = substitute(file, '\.', '/', 'g')

	let javapath = g:javadoc_path
	let regex = "^[^,]*"
	let trimmed_classname = substitute(a:classname, '\W', '', 'g')
	if (strlen(trimmed_classname) < 1)
		echo "Class name is too short"
		return
	endif
	while (strlen(javapath))
		let path = GetFirstPathElement(javapath, regex)
		let javapath = RemoveFirstPathElement(javapath, regex)
		let lfile = path . "/" . file . ".html"
		if ((match(lfile, "\*\.html$") != -1) && has("gui_running"))
			let lfile = substitute(lfile, "\*\.html$", "", "")
			if (isdirectory(expand(lfile)))
				let null = system(g:browser.' '.lfile.' &')
				let s:found = s:found + 1
			endif
		elseif (filereadable(expand(lfile)))
			let null = system(g:browser.' '.lfile.' &')
			let s:found = s:found + 1
			break
		endif
	endwhile

	if (s:found == 0)
		" Couldn't find the file directly, so do the equivalent of a system find
		" on each path element and sub-directory.

		" Loop through the given path elements
		let javapath = g:javadoc_path
		while (strlen(javapath))
			let path = GetFirstPathElement(javapath, regex)
			call FindTarget(path, a:classname.".html")
			let javapath = RemoveFirstPathElement(javapath, regex)
		endwhile
	endif

	if (s:found == 1)
		echo "Found 1 page"
	else
		echo "Found ".s:found." pages"
	endif

endfunction


" Get every file within the path and see if it looks like the target.
" If a directory is found then this function is called recursively.
function! FindTarget(path, target)
	let findlist = substitute(glob(a:path."/*").",", "\n", ",", "g")
	let null = ''
	while (strlen(findlist))
		let fpath = GetFirstPathElement(findlist, "[^,]*")
		if (match(fpath, "/class-use/") > -1 && s:skip_class_use)
			break
		endif
		let findlist = substitute(findlist, "[^,]*,", "", "")
		if (isdirectory(fpath))
			call FindTarget(fpath, a:target)
		else
			if (match(fpath, '/'.a:target) > -1)
				let null = system(g:browser.' '.fpath.' &')
				let s:found = s:found + 1
			endif
		endif
	endwhile
endfunction

" Return everything up to the first regex in a path
function! GetFirstPathElement(path, regex)
	let lpath = matchstr(a:path, a:regex)
	return lpath
endfunction

" Remove everything up to the first "," in a path
function! RemoveFirstPathElement(path, regex)
	let lpath = a:path
	let lregex = a:regex
	let lpath = substitute(lpath, lregex, "", "")
	let lpath = substitute(lpath, "^,", "", "")
	return lpath
endfunction
