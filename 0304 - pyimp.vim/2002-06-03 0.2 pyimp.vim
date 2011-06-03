" Set of functions to open a Python import statement as a file.
" Maintained by Tom Jenkins <tjenkins@devis.com>
" from code base of javaimp
" Maintained by Darren Greaves <darren@krapplets.org>
" Version 0.2
" Suggested stuff to add to your ~.vimrc:
" let $PYTHONSOURCEPATH="/usr/local/python2.2,/usr/local/python2.2/lib/site-packages"
" map H :call OpenPythonImport($PYTHONSOURCEPATH)<CR>
" Then press H while on a import line and the file should open (you'll need the
" source files and the path to have been set already.

" The paths in $PYTHONSOURCEPATH should have the source packages directly
" underneath each path.  The script needs to be able to add the package to the
" path element and open a file.

" Open the file for a package path.
" Converts the package path into a file path then appends ".py" to the file
" name.
" Then tries to open this as a file by appending to each element of a
" comma-separated path passed in.
" Goes through each element until either the file is opened or all elements were
" tried.
function! OpenPythonImport(pythonpath)
    let line = getline(".")
    let regex_import = '^import\s\+\(\S\+\)$'
    let regex_from = '^from\s\+\(\S\+\)'
    let s:l = matchstr(line, regex_import)
    let s:f = matchstr(line, regex_from)

    if strlen(s:l)
	let file = substitute(s:l, regex_import, '\1', '')
    elseif strlen(s:f)
	let file = substitute(s:f, regex_from, '\1', '')
    else
	let file = ''
    endif

    let file = SwapDotForSlash(file)

    let pythonpath = a:pythonpath
    let path = a:pythonpath
    let regex = "^[^,]*"

    while (strlen(pythonpath))
        let path = GetFirstPathElement(pythonpath, regex)

        let pythonpath = RemoveFirstPathElement(pythonpath, regex)
        let lfile = path . "/" . file . ".py"

        if ((match(lfile, "\*\.py$") != -1) && has("gui_running"))
            let lfile = substitute(lfile, "\*\.py$", "", "")
            if (isdirectory(expand(lfile)))
                let $FILE = lfile
                execute 'browse confirm e $FILE'
            endif
        elseif (filereadable(expand(lfile)))
            let $FILE = lfile
            execute 'e $FILE'
            break
        endif
    endwhile

    return file
endfunction

" Return everything up to the first "," in a path
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

" Swap "." for "/" to convert a package path into a file path
function! SwapDotForSlash(path)
    let result = substitute(a:path, '\.', '/', 'g')
    return result
endfunction

