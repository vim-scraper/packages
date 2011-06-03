" Set of functions to open a Java file based on word under cursor.
" Maintained by Richard Emberson <rembersonATedgedynamicsDOTcom>
"    some code based upon java import by Darren Greaves
" Version 1.1
"
" Suggested stuff to add to your ~.vimrc:
" source $HOME/vim/javae.vim
" let $JAVASOURCEPATH = "$JAVA_HOME/src"
" let $JAVASOURCEPATH = $JAVASOURCEPATH . ",$HOME/java/xerces/xerces/src"
" let $JAVASOURCEPATH = $JAVASOURCEPATH . ",$HOME/java/xerces/xalan/src"
" let $JAVASOURCEPATH = $JAVASOURCEPATH . ",$HOME/java/jboss/jboss"
" let $JAVASOURCEPATH = $JAVASOURCEPATH . ",$HOME/java/jakarta/commons-lang/src/java"
" let $JAVASOURCEPATH = $JAVASOURCEPATH .  ",$HOME/java/jakarta/commons-collections/src/java"
" let $JAVASOURCEPATH = $JAVASOURCEPATH . ",$HOME/java/jakarta/jakarta-tomcat/jakarta-servletapi-5/jsr154/src/share/"
" map ,g :call EditJava($JAVASOURCEPATH)<CR>
"
" if you want to debug, comment out above line and uncomment below line
" "map ,g :debug:call EditJava($JAVASOURCEPATH)<CR>
"
" Your java source path, of course, be different.
"
" Many java src tar files untar into directories with include not only
" the component name, but also a version number (e.g., xerces has source
" directory xerces-2_6_2) - make symbolic link, 
" ln -s xerces-2_6_2 xerces
" and your vimrc will not have to be changed everytime you get a new 
" version.
"
" Then press ,g while cursor is on a class name word and
" the file should open (you'll need the
" source files and the path to have been set already.
"
" Place cursor over a word and attempt to find a java file with the give name.
"
" The following matches are attempted:
"  1) Is the cursor over a full package classname (e.g., java.util.Map)
"     If so is it over the classname (Map)
"       If so open it.
"     Or it is over on of the package names (util)
"       If so open the directory (java/util).
"  2) Is there a local file with the name <cword>.java
"     If so open it.
"  3) Is there an import statement with that file identified explicitly
"     (e.g., java.util.Map for Map).
"     If so open it.
"  4) Is it a file located in "$JAVA_HOME/src/java/lang/" directory
"     If so open it.
"  5) Is there an import statement with that file identified implicitly.
"     (e.g., java.util.* for Map).
"     If so open it.
"
function! EditJava(javapath)
    let dot_regex = '\k'
    let hasdot = matchstr('.', dot_regex)

    " Does keyword contain '.'
    " (Note: there maybe a better/faster way but this is what I came up with)
    if (hasdot == "")
        " no, get cword and then full (including '.'s) cdotword
        let cword = expand("<cword>")
        set iskeyword+=.
        let cdotword = expand("<cword>")
        set iskeyword-=.
    else
        " yes, get cdotword (including '.'s) and then cword
        let cdotword = expand("<cword>")
        set iskeyword-=.
        let cword = expand("<cword>")
        set iskeyword+=.
    endif

    " If cword and cdotword are not equal, then we are looking at
    " a full package path class name, i.e., java.util.Map.
    if (cword != cdotword)
        " If the cursor is over the classname in the package path,
        " then look up the class.
        " Otherwise, look up directory and open the directory
        let dotlen = strlen(cdotword)
        let end = matchend(cdotword, cword)

        if (dotlen == end)
            " cword is at end of cdotword, edit file
            let file = SwapDotForSlash(cdotword)
            let file = ExecutePathMatch(file,a:javapath)
            if (file != "") 
                return file
            endif
        else
            " cword is in the middle of cdotword, edit directory
            let part_dotword = strpart(cdotword, 0, end)
            let dir = SwapDotForSlash(part_dotword)
            let dir = ExecuteDirMatch(dir,a:javapath)
            if (dir != "") 
                return dir
            endif
        endif
    endif


    " local match
    let fullfile = GetLocal(cword)
    if (fullfile != "") 
        if (filereadable(expand(fullfile)))
            let $FILE = fullfile
            execute 'e $FILE'
            return fullfile
        endif
    endif

    " explicit import 
    let path = GetExplicitImport(cword)
    let file = SwapDotForSlash(path)
    if (file != "") 
        let file = ExecutePathMatch(file,a:javapath)
        if (file != "") 
            return file
        endif
    endif

    " look in java/lang
    if ($JAVA_HOME != "") 
        let file = $JAVA_HOME . "/src/java/lang/" . cword . ".java"
        if (filereadable(expand(file)))
            let $FILE = file
            execute 'e $FILE'
            return file
        endif
    endif

    " implicit import 
    let file = GetImlicitImport(cword,a:javapath)
    if (file != "") 
        return file
    endif

    return path
endfunction

" is the file in the same directory as the current file/buffer
function! GetLocal(cword)
    let jfile = a:cword . ".java"
    let fullpath = simplify(bufname("%"))
    let path_regex = '^\(.*/\)\?\f\+$'

    let l = matchstr(fullpath, path_regex)
    if (l == "") 
        return ""
    endif

    let path = substitute(l, path_regex, '\1', '')
    if (path == "") 
        return jfile
    else
        return path . jfile
    endif

endfunction

" is there an import statement exactly matching the file
function! GetExplicitImport(cword)
    let import_regex = '^import\s\+\(\S\+\);.*$'
    let file_regex = '^\f\+'
    let index = 0
    let n = line("$")
    while index <= n
        let index = index + 1

        let line = getline(index)
        if (line == "") 
            continue
        endif

        let l = matchstr(line, import_regex)
        if (l == "") 
            continue
        endif

        let file = substitute(l, import_regex, '\1', '')
        let r = file_regex . a:cword . '$'
        let l = matchstr(file, r)
        if (l == "") 
            continue
        endif

        return file
    endwhile
    return ""
endfunction

" is there an import statement that end with * matching the file
function! GetImlicitImport(cword,javapath)
    let import_regex = '^import\s\+\(\(\K\+\.\)\+\)\*;.*$'
    let index = 0
    let n = line("$")
    while index <= n
        let index = index + 1

        let line = getline(index)
        if (line == "") 
            continue
        endif

        let l = matchstr(line, import_regex)
        if (l == "") 
            continue
        endif

        let file = substitute(l, import_regex, '\1', '')
        let file = file . a:cword
        let file = SwapDotForSlash(file)
        let file = ExecutePathMatch(file,a:javapath)
        if (file != "") 
            return file
        endif

        return file
    endwhile
    return ""
endfunction

" search through javapath for matching file
"    code from java import by Darren Greaves
function! ExecutePathMatch(file, javapath)
    let javapath = a:javapath
    let path = a:javapath
    let regex = "^[^,]*"

    while (strlen(javapath))
        let path = GetFirstPathElement(javapath, regex)

        let javapath = RemoveFirstPathElement(javapath, regex)
        let lfile = path . "/" . a:file . ".java"

        if ((match(lfile, "\*\.java$") != -1) && has("gui_running"))
            let lfile = substitute(lfile, "\*\.java$", "", "")
            if (isdirectory(expand(lfile)))
                let $FILE = lfile
                execute 'browse confirm e $FILE'
            endif
        elseif (filereadable(expand(lfile)))
            let $FILE = lfile
            execute 'e $FILE'
            return lfile
        endif
    endwhile
    return ""
endfunction

" search through javapath for matching dir
function! ExecuteDirMatch(dir, javapath)
    let javapath = a:javapath
    let path = a:javapath
    let regex = "^[^,]*"

    while (strlen(javapath))
        let path = GetFirstPathElement(javapath, regex)

        let javapath = RemoveFirstPathElement(javapath, regex)
        let lfile = path . "/" . a:dir

        if (isdirectory(expand(lfile)))
            let $FILE = lfile
            execute 'e $FILE'
            return lfile
        endif
    endwhile
    return ""
endfunction

" Return everything up to the first "," in a path
"    code from java import by Darren Greaves
function! GetFirstPathElement(path, regex)
    let lpath = matchstr(a:path, a:regex)
    return lpath
endfunction

" Remove everything up to the first "," in a path
"    code from java import by Darren Greaves
function! RemoveFirstPathElement(path, regex)
    let lpath = a:path
    let lregex = a:regex
    let lpath = substitute(lpath, lregex, "", "")
    let lpath = substitute(lpath, "^,", "", "")
    return lpath
endfunction

" Swap "." for "/" to convert a package path into a file path
"    code from java import by Darren Greaves
function! SwapDotForSlash(path)
    let result = substitute(a:path, '\.', '/', 'g')
    return result
endfunction
