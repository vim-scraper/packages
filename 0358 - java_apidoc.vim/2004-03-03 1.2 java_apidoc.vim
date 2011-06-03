" java_apidoc.vim v1.2 by Paul Etchells <etch@etch.org.uk>
" based on work by Darren Greaves <darren@krapplets.org> - Thanx for
" giving me the idea and a good chunk of the code!
"
" DESCRIPTION
" Opens a browser showing the Javadoc generated API for an imported package,
" or for a class name under the cursor. Uses the index files generated by
" javadoc to quickly search for the html page, and a Vim buffer to store them
" whilst its searching.
"
" Tested and working on GVim >= 6.1 on Linux.
"
" INSTALL
" 1) Put this file in ~/.vim/ftplugin (or wherever Vim looks for plugins).
"
" CONFIGURATION - BROWSER
" Uncomment for whichever browser you want to use on your system.
"-----------------------------------------------------------------------------
"let browser="dillo"
"let browser="xterm -e lynx"
"let browser="opera"
let browser="htmlview"
"-----------------------------------------------------------------------------
"
" CONFIGURATION - JAVA API PATH
" Set the javadoc_path variable to a comma separated list of paths to
" the tops of the Javadoc trees. Change these to match your api locations.
"-----------------------------------------------------------------------------
let javadoc_path="/home/etch/www/dox/java/j2se1.4.2/api,/opt/j2sdkee1.3.1/doc/api,/home/etch/www/dox/java/java-gnome-0.8.3/doc-core/api"
"-----------------------------------------------------------------------------
"
" CONFIGURATION - KEY ASSIGNMENT
"-----------------------------------------------------------------------------
nmap <F2> :call OpenJavadoc()<CR><CR>
"-----------------------------------------------------------------------------
"
" Avoid loading this function twice
if exists("loaded_java_apidoc") || &cp
  finish
endif

let loaded_java_apidoc = 1

function! OpenJavadoc()
  let line = getline(".")
  let bufhidden = getbufvar(bufnr("%"), "&hidden")
  let regex = '^import\s\+\(\S\+\);$'
  let l = matchstr(line, regex)
  let file = substitute(l, regex, '\1', '')
  let null = ''
  let s:found = 0
  let classname = expand("<cword>")
  call setbufvar(bufnr("%"), "&hidden", 1)

  let file = substitute(file, '\.', '/', 'g')

  let javapath = g:javadoc_path
  let regex = "^[^,]*"

  let trimmed_classname = substitute(classname, '\W', '', 'g')
  if (strlen(trimmed_classname) < 1)
	  echo "Class name is too short"
	  return
  endif
  while (strlen(javapath))
    let path = s:java_apidoc_getFirstPathElement(javapath, regex)

    let javapath = s:java_apidoc_removeFirstPathElement(javapath, regex)
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
    " Couldn't find the file directly, so search the allclasses
    " files for a match, which incidentally, is the target name.
    
    " Loop through the path elements
    let javapath = g:javadoc_path
    while (strlen(javapath))
      let path = s:java_apidoc_getFirstPathElement(javapath, regex)
	  let allclasses = path.'/allclasses-noframe.html'
	  if (filereadable(allclasses))
		  call s:java_apidoc_search(allclasses, path, classname)
	  else
		  let allclasses = path.'/allclasses-frame.html'
		  if (filereadable(allclasses))
			  call s:java_apidoc_search(allclasses, path, classname)
		  endif
	  endif
	  let javapath = s:java_apidoc_removeFirstPathElement(javapath, regex)
  endwhile
  endif

  call setbufvar(bufnr("%"), "&hidden", bufhidden)

endfunction

function! s:java_apidoc_search(allclasses, path, classname)
	  let curr_buf = bufnr('%')
	  let html_buf = 0
	  execute(':view +0 '.a:allclasses)
	  call s:java_apidoc_searchHtml(a:path, a:classname)
	  let html_buf = bufnr('%')
	  execute(':b '.curr_buf)
	  execute(':bd '.html_buf)
endfunction

function! s:java_apidoc_searchHtml(path, classname)
	let lineno = search('<a href="[a-z/]*/'.a:classname.'\.html\c', 'W')
	while (lineno > 0)
		let fpath = substitute(getline("."), '<A HREF="\([^ ]*\)".*$', '\1', 'i')
        let null = system(g:browser.' '.a:path.'/'.fpath.' &')
		let s:found = s:found + 1
		let lineno = search('<a href="[a-z/]*/'.a:classname.'\.html\c', 'W')
	endwhile
endfunction

" Return everything up to the first regex in a path
function! s:java_apidoc_getFirstPathElement(path, regex)
  let lpath = matchstr(a:path, a:regex)
  return lpath
endfunction

" Remove everything up to the first "," in a path
function! s:java_apidoc_removeFirstPathElement(path, regex)
  let lpath = a:path
  let lregex = a:regex
  let lpath = substitute(lpath, lregex, "", "")
  let lpath = substitute(lpath, "^,", "", "")
  return lpath
endfunction
