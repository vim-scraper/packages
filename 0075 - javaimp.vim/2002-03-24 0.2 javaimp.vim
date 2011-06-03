" Set of functions to open a Java import statement as a file.
" Maintained by Darren Greaves <darren@krapplets.org>
" http://boncey.org/vim.shtml
" Version 0.2
" Suggested stuff to add to your ~.vimrc:
" let $JAVASOURCEPATH="$SERVLET_HOME/src/share,$JAVA_HOME/src,$XML_HOME/src"
" map H :call OpenJavaImport($JAVASOURCEPATH)<CR>
" Then press H while on a import line and the file should open (you'll need the
" source files and the path to have been set already.

" The paths in $JAVASOURCEPATH should have the source packages directly
" underneath each path.  The script needs to be able to add the package to the
" path element and open a file.

" Open the file for a package path.
" Converts the package path into a file path then appends ".java" to the file
" name.
" Then tries to open this as a file by appending to each element of a
" comma-separated path passed in.
" Goes through each element until either the file is opened or all elements were
" tried.
" If a package ends in "*" then opens a file browser in this directory instead.
function! OpenJavaImport(javapath)
    let line = getline(".")
    let regex = '^import\s\+\(\S\+\);$'
    let l = matchstr(line, regex)
    let file = substitute(l, regex, '\1', '')

    let file = SwapDotForSlash(file)

    let javapath = a:javapath
    let path = a:javapath
    let regex = "^[^,]*"

    while (strlen(javapath))
        let path = GetFirstPathElement(javapath, regex)

        let javapath = RemoveFirstPathElement(javapath, regex)
        let lfile = path . "/" . file . ".java"

        if ((match(lfile, "\*\.java$") != -1) && has("gui_running"))
            let lfile = substitute(lfile, "\*\.java$", "", "")
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

