"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Name:                 JavaEdit
" Version:              1.3
" Last Change:          2005-17-04
" Description:          Set of functions to open a Java file based on word 
"                               under cursor.
" Author:               Richard Emberson <rembersonATedgedynamicsDOTcom>
" Original Author:      Richard Emberson <rembersonATedgedynamicsDOTcom>
" Maintainer:           Richard Emberson <rembersonATedgedynamicsDOTcom>
" Url:                  http://www.vim.org/scripts/script.php?script_id=1164
"
" Bugs And Comments:    Send bugs and comments to above email by replacing
"                       'AT' with '@' and 'DOT' with '.'.
"
"
" Licence:              This program is free software; you can redistribute it
"                       and/or modify it under the terms of the GNU General 
"                       Public License.  
"                       See http://www.gnu.org/copyleft/gpl.txt
"
" Credits:              Darren Greaves who wrote JavaImport: some code
"                               was taken from JavaImport and I realized
"                               one could do much more.
"                       Ciaran McCreesh who wrote locateopen.vim: from this
"                               script I understood how to use the 'locate' 
"                               command to find a java file.
"
"
" Todo:
"                       Right now while looking for import statements every 
"                               line in the file is read - script does not
"                               stop when the class/interface declaration
"                               is reached.
"                       Import statement in comment can be used.
"                       One has to place the cursor over a class name (or
"                               package path). One should be able to place
"                               cursor over any variable, look up the
"                               class and go to that file.
"                       If cursor is over a variable calling a method, go to
"                               the method definition.
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Section: Documentation 
"----------------------------
"
" Suggested stuff to add to your ~.vimrc:
" ---------
"
" " Set my Leader character
" let mapleader = ","
"
" source $HOME/vim/javae.vim
" let JAVASOURCEPATH = "$JAVA_HOME/src" . 
"    \",$HOME/java/xerces/xerces/src" . 
"    \",$HOME/java/xerces/xalan/src" . 
"    \",$HOME/java/jboss/jboss" . 
"    \",$HOME/java/jakarta/commons-lang/src/java" .  
"    \",$HOME/java/jakarta/commons-collections/src/java" . 
"    \",$HOME/java/jakarta/jakarta-tomcat/jakarta-servletapi-5/jsr154/src/share/"
" " goto
" map <Leader>g :call EditJava('e',JAVASOURCEPATH)<CR>
" " horizontal
" map <Leader>h :call EditJava('sp',JAVASOURCEPATH)<CR>
" " vertical
" map <Leader>v :call EditJava('vsp',JAVASOURCEPATH)<CR>
"
" " if you want to debug, comment out above line and uncomment below line
" "map <Leader>g :debug:call EditJava('e',JAVASOURCEPATH)<CR>
"
" ---------
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
"     If "locate" is enabled, search for file java/util/Map.java (in this
"     example). If found, open it.
"  4) Is it a file located in "$JAVA_HOME/src/java/lang/" directory
"     If so open it.
"  5) Is there an import statement with that file identified implicitly.
"     (e.g., java.util.* for Map).
"     If so open it.
"     If "locate" is enabled, search for file java/util/Map.java (in this
"     example). If found, open it.
"

" I use the following to round out my java dev environment:
" 1) toggle between files:
" map gg :e#
" 2) to view and select open buffers use the bufexplorer.vim script, and
" 3) to view and select files from a directory use the "edit cwd" Tip #2:
" map ,d :e <C-R>=expand("%:p:h")<CR><CR>                                        

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"
" parameters used to change behavior
"

" 
" Use the unix locate (or slocate) command to find the file
" if set to 1. If set to 0, then do not attempt to use the command.
" 
if !exists('g:javae_use_locate_cmd')
    let g:javae_use_locate_cmd = 1
endif


"
" locate binary name (only matters if g:javae_use_locate_cmd == 1)
"
if !exists('g:javae_locate_cmd')
    let g:javae_locate_cmd = "slocate"
endif

" Do we want to use a locate command, if yes, check for existance
if (g:javae_use_locate_cmd) 
    " check if command exists
    let g:javae_use_locate_cmd = executable(g:javae_locate_cmd)
    if (g:javae_use_locate_cmd != "1") 
        echo "Notice: the locate command \"" . 
            \ g:javae_locate_cmd . 
            \ "\" does not exist on your system"
        " Let g:javae_use_locate_cmd = 0 above or in your .vimrc
        " to turn off this message.
    endif
endif

"
" show prompt when there is only one choice.
" if not set in .vimrc, then do not show prompt when there is 
" only one choice
"
if !exists('g:javae_locateopen_alwaysprompt')
    let g:javae_locateopen_alwaysprompt = 0
endif

"
" show error if invalid selection is made or simply do nothing
" if not set in .vimrc, then set to show if bad selection is made.
"
if !exists('g:javae_locateopen_showerror')
    let g:javae_locateopen_showerror = 1
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"
" main entry point
"   parameters
"     cmd: can be any command that take a file name or directory 
"          as second argument:
"       e, vsplit, new, split, vertical, sview, vnew , etc.
"     javapath: ',' separate paths to java source
"
function! EditJava(cmd,javapath)
    try        
        let l:file = s:LookupSource(a:javapath)
    catch /.*/
      echohl WarningMsg
      echo v:exception
      echohl None
      return
    endtry                                         

" let x = input("file=\"" . l:file . "\"")
    if (l:file != "") 
        let CMD = a:cmd . " " . l:file
        execute CMD
        return
    else
        let l:cword = expand("<cword>")
        echohl WarningMsg
        echo "Error: No file for '" . l:cword . "'"
        echohl None
    endif
endfunction

"
" Find a java file or directory based upon word under cursor
" and the javapath parameter.
"
function! s:LookupSource(javapath)
    let l:cword = expand("<cword>")

    " package classname
    let l:file = s:OverPackageClassName(a:javapath)

    " local match
    if (l:file == "") 
        let l:file = s:GetLocal(l:cword)
    endif

    " explicit import 
    if (l:file == "") 
        let l:file = s:GetExplicitImport(l:cword,a:javapath)
    endif

    " look in java/lang
    if (l:file == "") 
        let l:file = s:GetJavaLang(l:cword)
    endif

    " implicit import 
    if (l:file == "") 
        let l:file = s:GetImlicitImport(l:cword,a:javapath)
    endif

    return l:file
endfunction

"
" Is the cursor over a package classname (i.e., java.util.Map)
" This can return a file or directory
"
function! s:OverPackageClassName(javapath)
    let l:dot_regex = '\k'
    let l:hasdot = matchstr('.', l:dot_regex)

    " Does keyword contain '.'
    " (Note: there maybe a better/faster way but this is what I came up with)
    if (l:hasdot == "")
        " no, get cword and then full (including '.'s) cdotword
        let l:cword = expand("<cword>")
        set iskeyword+=.
        let l:cdotword = expand("<cword>")
        set iskeyword-=.
    else
        " yes, get cdotword (including '.'s) and then cword
        let l:cdotword = expand("<cword>")
        set iskeyword-=.
        let l:cword = expand("<cword>")
        set iskeyword+=.
    endif

    " If cword and cdotword are not equal, then we are looking at
    " a full package path class name, i.e., java.util.Map.
    if (l:cword != l:cdotword)
        " If the cursor is over the classname in the package path,
        " then look up the class.
        " Otherwise, look up directory and open the directory
        let l:dotlen = strlen(l:cdotword)
        let l:end = matchend(l:cdotword, l:cword)

        if (l:dotlen == l:end)
            " cword is at end of cdotword, edit file
            let l:tfile = s:SwapDotForSlash(l:cdotword)
            let l:file = ExecutePathMatch(l:tfile,a:javapath)

            if (l:file == "") 
                let l:file = s:LocateFile(l:tfile)
            endif

            return l:file
        else
            " cword is in the middle of cdotword, edit directory
            let l:part_dotword = strpart(l:cdotword, 0, l:end)
            let l:tdir = s:SwapDotForSlash(l:part_dotword)
            let l:dir = s:ExecuteDirMatch(l:tdir,a:javapath)

            return l:dir
        endif
    endif

    return ""

endfunction

"
" Is the file in the same directory as the current file/buffer
"
function! s:GetLocal(cword)
    let l:jfile = a:cword . ".java"
    " simplify is in 6.2.064 but not before
    let l:fullpath = simplify(bufname("%"))
    let l:path_regex = '^\(.*/\)\?\f\+$'

    let l:l = matchstr(l:fullpath, l:path_regex)
    if (l:l == "") 
        return ""
    endif

    let l:path = substitute(l:l, l:path_regex, '\1', '')
    if (l:path == "") 
        let l:file = l:jfile
    else
        let l:file = l:path . l:jfile
    endif

    if (filereadable(expand(l:file)))
        return l:file
    else
        return ""
    endif

endfunction


"
" Use locate command to get and then choose a file.
"
function! s:LocateFile(tfile)
    let l:file = ""
    if (g:javae_use_locate_cmd) 
        let l:command = g:javae_locate_cmd . " " . a:tfile . ".java"
        let l:files = system(l:command)

        if (l:files != "") 
            let l:file = s:SelectFromList(l:files)
        endif
    endif
    return l:file
endfunction

"
" Use locate command to get and then choose a directory.
"
function! s:LocateDir(tdir)
    let l:dir = ""
    if (g:javae_use_locate_cmd) 
        let l:command = g:javae_locate_cmd . " -r '" . a:tdir . "$'"
        let l:dirs = system(l:command)

        if (l:dirs != "") 
            let l:dir = s:SelectFromList(l:dirs)
        endif
    endif
    return l:dir
endfunction

"
" The choices parameter is a "\n" separated list of (in this case)
" files or directories. If there is only one choice it is returned,
" otherwise one is requested to choose one.
"
function! s:SelectFromList(choices)
    let l:choices = a:choices

    " We have one or more choices
    let l:i = stridx(l:choices, "\n")
    let l:x = 0
    while (l:i > -1)
        let l:choice=strpart(l:choices, 0, l:i)
        let l:choices=strpart(l:choices, l:i+1)
        let l:i = stridx(l:choices, "\n")
        let l:x = l:x + 1
        echo l:x . ": " . l:choice
    endwhile

    let l:choices = a:choices
    if (l:x > 1) || (g:javae_locateopen_alwaysprompt)
        " clear the input queue
        let v = inputsave()
        let l:which=input("Select ? ")
        let l:y = 1
        while (l:y <= l:x)
            if (l:y == l:which)
                return strpart(l:choices, 0, stridx(l:choices, "\n"))
            else
                let l:choices=strpart(l:choices, stridx(l:choices, "\n") + 1)
                let l:y = l:y + 1
            endif 
        endwhile
        if (g:javae_locateopen_showerror)
            throw "LocateOpenError: Invalid choice \"" . l:which . "\""
        else
            return ""
        endif
    else
        return strpart(a:choices, 0, stridx(a:choices, "\n"))
    endif 

endfunction

"
" Is there an import statement exactly matching the file
" If not, can the file be located
"
function! s:GetExplicitImport(cword,javapath)
    let l:import_regex = '^import\s\+\(\S\+\);.*$'
    let l:file_regex = '^\f\+\.'
    let l:index = 0
    let l:n = line("$")

    let l:file = ""
    while l:index <= n
        let l:index = l:index + 1

        let l:line = getline(l:index)
        if (l:line == "") 
            continue
        endif

        let l:l = matchstr(l:line, l:import_regex)
        if (l:l == "") 
            continue
        endif

        let l:xfile = substitute(l:l, l:import_regex, '\1', '')
        let l:r = l:file_regex . a:cword . '$'
        let l:l = matchstr(l:xfile, r)
        if (l:l == "") 
            continue
        endif

        let l:tfile = s:SwapDotForSlash(l:xfile)
        let l:file = ExecutePathMatch(l:tfile,a:javapath)

        if (l:file == "") 
            let l:file = s:LocateFile(l:tfile)
        endif

        if (l:file != "") 
            break
        endif


    endwhile

    return l:file

endfunction

"
" Is there an import statement that end with * matching the file
" If not, can the file be located
"
function! s:GetImlicitImport(cword,javapath)
    let l:import_regex = '^import\s\+\(\(\K\+\.\)\+\)\*;.*$'
    let l:index = 0
    let l:n = line("$")

    let l:file = ""
    while l:index <= n
        let l:index = l:index + 1

        let l:line = getline(l:index)
        if (l:line == "") 
            continue
        endif

        let l:l = matchstr(l:line, l:import_regex)
        if (l:l == "") 
            continue
        endif

        let l:xfile = substitute(l, l:import_regex, '\1', '')
        let l:xfile = xfile . a:cword
        let l:tfile = s:SwapDotForSlash(l:xfile)
        let l:file = ExecutePathMatch(l:tfile,a:javapath)

        if (l:file == "") 
            let l:file = s:LocateFile(l:tfile)
        endif

        if (l:file != "") 
            break
        endif

    endwhile
    return l:file
endfunction

"
" See if its in $JAVA_HOME/src/java/lang
"
function! s:GetJavaLang(cword)
    if ($JAVA_HOME != "") 
        let l:file = $JAVA_HOME . "/src/java/lang/" . a:cword . ".java"
        if (filereadable(expand(l:file)))
            return file
        endif
    endif
    return ""
endfunction

"
" Search through javapath for matching file
"
function! ExecutePathMatch(file, javapath)
    let l:javapath = a:javapath
    let l:path = a:javapath
    let l:regex = "^[^,]*"

    while (strlen(l:javapath))
        let l:path = s:GetFirstPathElement(l:javapath, l:regex)

        let l:javapath = s:RemoveFirstPathElement(l:javapath, l:regex)
        let l:lfile = l:path . "/" . a:file . ".java"

        if (filereadable(expand(l:lfile)))
            return l:lfile
        endif
    endwhile
    return ""
endfunction

"
" Search through javapath for matching dir
"
function! s:ExecuteDirMatch(dir, javapath)
    let l:javapath = a:javapath
    let l:path = a:javapath
    let l:regex = "^[^,]*"

    while (strlen(l:javapath))
        let l:path = s:GetFirstPathElement(l:javapath, l:regex)

        let l:javapath = s:RemoveFirstPathElement(l:javapath, l:regex)
        let l:lfile = l:path . "/" . a:dir

        if (isdirectory(expand(l:lfile)))
            return l:lfile
        endif
    endwhile
    return ""
endfunction

"
" Return everything up to the first "," in a path
"
function! s:GetFirstPathElement(path, regex)
    let l:lpath = matchstr(a:path, a:regex)
    return l:lpath
endfunction

"
" Remove everything up to the first "," in a path
"
function! s:RemoveFirstPathElement(path, regex)
    let l:lpath = a:path
    let l:lregex = a:regex
    let l:lpath = substitute(l:lpath, l:lregex, "", "")
    let l:lpath = substitute(l:lpath, "^,", "", "")
    return l:lpath
endfunction

"
" Swap "." for "/" to convert a package path into a file path
"
function! s:SwapDotForSlash(path)
    let l:result = substitute(a:path, '\.', '/', 'g')
    return l:result
endfunction


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" TEST START
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"
" Between the TEST START and TEST END comments is work-in-progress code
" which should basically be ignored. Calling any of these functions
" while editing is done at your own risk.
"
function! XFindDef(cword)
    let cword = a:cword

    let type = ""

    let end = line(".")
    normal [{
    let prev = line(".")

    let declaration_regex = '.* \(\f*\) ' . cword . '.*'

    let index = end
    while index > prev
        let line = getline(index)
        let index = index - 1

        let l = matchstr(line, declaration_regex)
        if (l == "")
            continue
        endif
        let type = substitute(l, declaration_regex, '\1', '')
    endwhile
    
    exe ":normal " . end . 'G'
    return type

endfunction

" Determines if fullword is a method call.
" Params: 
"   none
" Returns:
"   if not a method then 0 is returned and s:method_name == ""
"   if a method then 1 is returned and s:method_name == the method name
function! s:IsMethodOld()
    let keyword_regex = '\k'
    let pos = col('.')
    let end = col('$') - 1
    let line = getline('.')

    let s:method_name = ""

    let word_end = pos

    while pos < end
        let c = line[pos]
        let pos = pos + 1

        if c == " "
            continue
        endif
        if c == "("
            " its a method, find word_start
            let word_start = word_end
            while word_start > 0
                let c = line[word_start - 1]
                if c == " "
                    break
                endif
                if c == "."
                    break
                endif
                let word_start = word_start - 1
            endwhile

            let s:method_name = strpart(line, word_start, word_end - word_start)
            return 1

        endif

        let iskeyword = matchstr(c, keyword_regex)
        if iskeyword == ""
            break
        endif
        let word_end = pos

    endwhile
    return 0
endfunction

function! s:IsMethod()
    let keyword_regex = '\k'
    let pos = col('.')
    let end = col('$') - 1
    let line = getline('.')

    let word_end = pos

    while pos < end
        let c = line[pos]
        let pos = pos + 1

        if c == " "
            while pos < end
                let c = line[pos]
                let pos = pos + 1
                if c == " "
                    continue
                endif
                if c == "("
                    return 1
                endif
                return 0
            endwhile
        endif

        if c == "."
            continue
        endif

        if c == "("
            return 1
        endif

        let iskeyword = matchstr(c, keyword_regex)
        if iskeyword == ""
            break
        endif
        let word_end = pos

    endwhile
    return 0
endfunction

" byte:
"   normal              0
"   this                1
"   keyword             2
"   primitive type      4
"   first letter cap    8 
"   all caps           16
"
function! s:ClassifyPart(part)
    let p = a:part
    if (p == "this")
        return 1
    endif

    if (p == "abstract" ||
        \ p == "break" || 
        \ p == "case" ||
        \ p == "catch" ||
        \ p == "class" ||
        \ p == "const" ||
        \ p == "continue" ||
        \ p == "default" ||
        \ p == "do" ||
        \ p == "else" ||
        \ p == "extends" ||
        \ p == "false" ||
        \ p == "final" ||
        \ p == "finally" ||
        \ p == "for" ||
        \ p == "goto" ||
        \ p == "if" ||
        \ p == "implements" ||
        \ p == "import" ||
        \ p == "instanceof" ||
        \ p == "interface" ||
        \ p == "native" ||
        \ p == "new" ||
        \ p == "null" ||
        \ p == "package"||
        \ p == "private" ||
        \ p == "protected" ||
        \ p == "public" ||
        \ p == "return" ||
        \ p == "static" ||
        \ p == "super" ||
        \ p == "switch" ||
        \ p == "synchronized" ||
        \ p == "this" ||
        \ p == "throw" ||
        \ p == "throws" ||
        \ p == "transient" ||
        \ p == "true" ||
        \ p == "try" ||
        \ p == "void" ||
        \ p == "volatile" ||
        \ p == "while" 
        \)

        " its a key word
        return 2
    endif

    if (p == "void" || 
        \ p == "boolean" ||
        \ p == "char" || 
        \ p == "byte" || 
        \ p == "short" || 
        \ p == "int" || 
        \ p == "long" || 
        \ p == "float" || 
        \ p == "double" 
        \)
        return 4
    endif

"let x = input("p=" . p)
    "let all_caps_regex = "\%([A-Z0-9_]*\)"
    let all_caps_regex = '[A-Z0-9_]\{' . strlen(p) . '}'
"let x = input("all_caps_regex=" . all_caps_regex)
    let allCap = matchstr(p, all_caps_regex)
    if (allCap != "")
        return 16
    endif

    let first_caps_regex = "[A-Z].*"
    let firstCap = matchstr(p, first_caps_regex)
    if (firstCap != "")
        return 8 
    endif

    " instance variable or method
    return 0 

endfunction

let s:word_array = ""
let s:word_array_len = ""
let s:word_array_method = 0

function! s:ParseFullWord(cword,fullword)
    let s:word_array_method = s:IsMethod()
    if (a:cword == a:fullword)
        let p = a:cword
        let pclass = s:ClassifyPart(p)
        let s:word_array = ":" . pclass . "-" . p
        let s:word_array_len = "1"
        return 1
    else
        " parse fullword into parts
        let cnt = 1
        let w = a:fullword
        let s:word_array = ""
        let idx = stridx(w, ".")
        while (idx != -1)
            let p = strpart(w, 0, idx)
            let pclass = s:ClassifyPart(p)
            let s:word_array = s:word_array . ":" . pclass . "-" . p
"let x = input("word_array=" . s:word_array)
            let cnt = cnt + 1
            let w = strpart(w, idx+1, strlen(w) - idx)
"let x = input("w=" . w)
            let idx = stridx(w, ".")
        endwhile
        let p = w
        let pclass = s:ClassifyPart(p)
        let s:word_array = s:word_array . ":" . pclass . "-" . p
        let s:word_array_len = cnt
"let x = input("word_array=" . s:word_array)
        return 0
    endif
endfunction

" get this.foo.bar(....)
" get a.b.c.Foo.bar(....)
" parse into:
"    package start
"    package end
"    class 
"    instance 
"    method 
" some may be null
function! FindDef(cmd,javapath)
    let dot_regex = '\k'
    let haskeyworddot = matchstr('.', dot_regex)

"    let parts_regex = '\(\(\k+\)\.\)*\(\k+\)'

    " Does keyword contain '.'
    " (Note: there maybe a better/faster way but this is what I came up with)
    if (haskeyworddot == "")
        " no, get cword and then full (including '.'s) fullword
        let cword = expand("<cword>")
        set iskeyword+=.
        let fullword = expand("<cword>")
        "let ismethod = s:IsMethodOld()
        set iskeyword-=.
    else
        " yes, get fullword (including '.'s) and then cword
        let fullword = expand("<cword>")
        "let ismethod = s:IsMethodOld()
        set iskeyword-=.
        let cword = expand("<cword>")
        set iskeyword+=.
    endif
" echo "cword=" . cword . ", fullword=" . fullword . ", ismethod=" . ismethod
" let c = confirm("line=", "c")


    " determine where cword is in fullword
    "    let start_index = stridx(line, fullword)
    "    let len = strlen(fullword);
    "echo "index=" . start_index
    let issingleword = s:ParseFullWord(cword,fullword)

    " cword starts with uppercase means its a class
    let uppercase_regex = '[A-Z].*'
    let hasuppercase = matchstr(cword, uppercase_regex)
    if (hasuppercase == "")
        let isclass = 0
    else
        let isclass = 1
    endif

    " fullword starts with 'this'
    "let this_regex = 'this\..*'
    "let hasthis = matchstr(fullword, this_regex)
    "if (hasthis == "")
" echo "no this"
    "else
" echo "this"
    "endif

" echohl WarningMsg
" echohl None

"let x = input("ismethod=" . ismethod . ", issingleword=" . issingleword . ", isclass=" . isclass)
" let x = input("issingleword=" . issingleword . ", isclass=" . isclass)
let x = input("word_array=" . s:word_array . 
    \ ", len=" . s:word_array_len .
    \ ", method=" . s:word_array_method
    \)

    " this assumes that there are no more than 9 words in package path
    if (s:word_array_method)
"let x = input("is method")
        " if not method we want type
        if (issingleword)
            " simple classname or variable
            if (isclass)
                " its a constructor
"                let r = EditJava(a:cmd,a:javapath)
"                return r
                let file = s:LookupSource(a:javapath)

" what about inner class or self
" let x = input("file=" . file)
                if (file != "") 
                    let $CMD = a:cmd . " " . file
                    execute $CMD
                    " look for public/protected/private constructor
                    let m = '\s*\(public\|protected\|private\)\s\+' . s:method_name . '\s*('
                    let s = search(m, "W")
                    if (s == 0) 
                        " not found
                        " look for package constructor
                        let m = '\s\+' . s:method_name . '\s*('
                        let s = search(m, "W")
                    endif
"                    let M = "/ " . s:method_name . "("
"                    execute M
"let x = input("regex=" . m)

"        execute ":normal 20G"
"        execute ":normal /" . s:method_name . "("
                    return
                else
                    echohl WarningMsg
                    echo "Error: Could not find file matching '" . cword . "'"
                    echohl None
                endif
            else 
                " its a method in local file
execute ":normal 1G"
let m = '\s*\(public\|protected\|private\)\?\s\+\k\+\s\+' . s:method_name . '\s*('
                    let s = search(m, "W")

"let type = XFindDef(cword)
"echohl Visual 
"echo " type=" . type
"echohl None 
            endif
        else 
            " not a singleword: a...b() or A...b()
        endif
    else 
        " not a method
"let x = input("this is a test")
        " if method we want type, then search for method
"echohl Visual 
"echo " has method=" . ismethod
"echohl None 
    endif

endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" TEST END
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
