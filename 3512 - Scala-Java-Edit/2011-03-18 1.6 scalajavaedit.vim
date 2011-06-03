" ============================================================================
" scalajavaedit.vim
"
" $Id:$
"
" Target language: Scala and/or Java
" Find Scala/Java source file associated with class name.
"
" Place cursor on the name of a class and invoke this script and
" you will jump to the source file assocaited with that class.
" Of course, you have to have pre-defined where you want the script
" to search for the source files.
" this script can reformat the arguments so that the are all aligned
" with the first argument each on its own line.
" Also, when an argument is a bunch of strings concatenated with
" plus operator, '+', this script will also place each piece of a
" string on its own line.
"
" ============================================================================
" Caveats: 
" Works a lot of times, but if the class is not in a file with its
" name, this script will not find it. This happens a lot more in
" Scala than in Java since in Scala multiple classes can be in the
" same file. It will also not find classes defined within classes/objects -
" again, there is no association between such classes and their source
" files.
"
" ============================================================================

" ============================================================================
" Configuration Options:
"
" Use the unix locate (or slocate) command to find the file
" if set to 1. If set to 0, then do not attempt to use the command.
" 
" When locate is used it does not search for every file ending with the
" string: expand("<cword>") . ".java", rather locate is applied only to
" the package paths found in import statements, which is to say if
" import A.B.C.* is an import statement, then the string given to locate
" would be "A/B/C/" . expand("<cword>") . ".java" - a very discriminating
" search.
" 
if !exists('g:javae_use_locate_cmd')
    let g:javae_use_locate_cmd = 1
endif


"
" Locate binary name (only matters if g:javae_use_locate_cmd == 1)
"
if !exists('g:javae_locate_cmd')
    let g:javae_locate_cmd = "locate"
endif

" Do we want to use a locate command, if yes, check for existence
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
" Show prompt when there is only one choice.
" If not set in .vimrc, then do not show prompt when there is 
" only one choice
"
if !exists('g:javae_locateopen_alwaysprompt')
    let g:javae_locateopen_alwaysprompt = 0
endif

"
" Show error if invalid selection is made or simply do nothing
" If not set in .vimrc, then set to show if bad selection is made.
"
if !exists('g:javae_locateopen_showerror')
    let g:javae_locateopen_showerror = 1
endif

"
" If one is jumping to the new file by executing an edit and the current
" file needs to be written, then setting javae_automatic_update to 0
" will prevent the edit forcing one to manually first write the current
" file - safe. On the other hand setting it to 1 will automatically write
" the current file and then jump to the new file.
"
if !exists('g:javae_automatic_update')
    let g:automatic_update = 1
endif

" While looking through the file for import statements Java syntax value
" for the current search line can be used to determine if 1) we are in
" a comment and 2) whether the initial class/interface declaration has
" been seen. If the variable g:javae_syntax_based is set to 0, then
" no syntax base short cuts are used and, in the worse case, the file
" is traversed 3 times.
" Now syntax matching can fail and, worse case, one either matches an
" import statement that is commented out or one traverses the whole file.
" In addition, I use the default syntax mappings in the java.vim syntax
" file: "class" and "interface" map to a keyword (javaClassDecl) containing
" "class" (ignoring case) and "public" "protected" "private" and "abstract"
" map to a keyword (javaScopeDecl) containing "scope" (ignoring case).
" Also, comment syntactically maps to a keyword containing "comment".
" If you have different mappings, the current syntax base line identification
" mechanism will not work - and suggested ways to parameterize it are
" welcome.
" The default is to use Java syntax.
if !exists('g:javae_syntax_based')
    let g:javae_syntax_based = 1
endif

" ============================================================================
" End of Configuration Options
" ============================================================================

" ============================================================================
" History:
"
" File:          scalajavaedit.vim
" Summary:       Find source file associated with class name
" Author:        Richard Emberson <richard.n.embersonATgmailDOTcom>
" Last Modified: 03/18/2011
" Version:       1.6 (based upon JavaEdit script version 1.5)
" Modifications:
"  1.6 : Added support for Scala.
"
" Tested on vim 7.2 on Linux
"
"
" Thanks:               Darren Greaves who wrote JavaImport: some code
"                               was taken from JavaImport and I realized
"                               one could do much more.
"                       Ciaran McCreesh who wrote locateopen.vim: from this
"                               script I understood how to use the 'locate' 
"                               command to find a java file.
"
"
" Todo:
"                       One has to place the cursor over a class name (or
"                               package path). One should be able to place
"                               cursor over any variable, look up the
"                               class and go to that file.
"                       If cursor is over a variable calling a method, go to
"                               the method definition.

" ============================================================================
" Description: 
" ============================================================================
"
" Installation:
"
" Suggested stuff to add to your ~.vimrc:
" ---------
"
" " Set my Leader character
" let mapleader = ","
"
" " use locate command
" let g:javae_locate_cmd = "locate"
" source $HOME/vim/scalajavaedit.vim
" let JAVASOURCEPATH = "$JAVA_HOME/src" . 
"    \",$HOME/java/xerces/xerces/src" . 
"    \",$HOME/java/xerces/xalan/src" . 
"    \",$HOME/java/jboss/jboss" . 
"    \",$HOME/java/jakarta/commons-lang/src/java" .  
"    \",$HOME/java/jakarta/commons-collections/src/java" . 
"    \",$HOME/java/jakarta/jakarta-tomcat/jakarta-servletapi-5/jsr154/src/share/"
" " Note that for Java, only search for files with the 'java' suffix
" " goto
" autocmd FileType java map <Leader>g :call EditSource('e',['java'],=,JAVASOURCEPATH)<CR>
" " horizontal
" autocmd FileType java map <Leader>h :call EditSource('sp',['java'],=,JAVASOURCEPATH)<CR>
" " vertical
" autocmd FileType java map <Leader>v :call EditSource('vsp',['java'],=,JAVASOURCEPATH)<CR>
"
" " if you want to debug, comment out above line and uncomment below line
" "map <Leader>g :debug:call EditSource('e',JAVASOURCEPATH)<CR>
" 
" let SCALASOURCEPATH = "$SCALA_HOME/src" . 
"    \",$SVN_HOME/project1/src/main/scala" . 
"    \",$SVN_HOME/project1/src/test/scala" . 
"    \",$SVN_HOME/project2/src/main/scala" . 
"    \",$SVN_HOME/project2/src/test/scala" . 
"    \",$JAVA_HOME/src" 
"
" " Note that for Scala, search for files with the 'scala' and
" "   then 'java' suffixes
" " goto
" autocmd FileType scala map <Leader>g :call EditSource('e',['scala', 'java'],SCALASOURCEPATH)<CR>
" " horizontal
" autocmd FileType scala map <Leader>h :call EditSource('sp',['scala', 'java'],SCALASOURCEPATH)<CR>
" " vertical
" autocmd FileType scala map <Leader>v :call EditSource('vsp',['scala', 'java'],SCALASOURCEPATH)<CR>
"
" ---------
" End Suggested stuff to add to your ~.vimrc:

" One can of course, define more than one SOURCEPATH search variable and
" have different mappings to envoke the different search paths.
" When porting Java code to Scala (and keeping the same package names for
" the classes), I define a second search path and key map bindings
" allowing me to jump from the ported Scala source file to 
" the Java source file which was the basis of the port.


" Your java source path, of course, be different.
"
" Many java src tar files untar into directories with include not only
" the component name, but also a version number (e.g., xerces has source
" directory xerces-2_6_2) - make symbolic link, 
" ln -s xerces-2_6_2 xerces
" and your vimrc will not have to be changed everytime you get a new 
" version.
"
" Usage:
"
" Then press ,g while cursor is on a class name word and
" the file should open (you'll need the
" source files and the path to have been set already).
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
"  6) Its possible we are in a junit directory or is some parallel directory
"     structure so that our current file has the same package statement 
"     value as some source files in a different directory. This attempt takes 
"     the package statement value, creates a directory plus java file from 
"     it and first looks through the java path and if not found there, 
"     does a "locate" searching for the file.
"

" I use the following to round out my java dev environment:
" 1) toggle between files:
" map gg :e#<CR>
" for previous use :bp
" for previous Nth use :bpN
" for next use :bn
" for next Nth use :bnN
" 2) to view and select open buffers use the bufexplorer.vim script, and
" 3) to view and select files from a directory use the "edit cwd" Tip #2:
" map ,d :e <C-R>=expand("%:p:h")<CR><CR>                                        
"
" Comments:
"
"   Send any comments and/or bug reports/fixes to:
"       Richard Emberson <richard.n.embersonATgmailDOTcom>
" ============================================================================
" THE SCRIPT
" ============================================================================

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" main entry point
"   parameters
"     cmd: can be any command that take a file name or directory 
"          as second argument:
"       e, vsplit, new, split, vertical, sview, vnew , etc.
"     sourcepath: ',' separate paths to java source
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! EditSource(cmd,suffixes,sourcepath)
    let l:cword = expand("<cword>")
    if len(a:suffixes) < 1
      echohl WarningMsg
      echo "Fail: Empty suffixes list"
      echohl None
      return
    endif

    let l:primarySuffix = expand("%:e")
    " let l:primarySuffix = a:suffixes[0]
" echo "primarySuffix=" . l:primarySuffix

" echo "EditSource: TOP " . l:cword
"set verbose=20
"let x = input("p=")
    for suffix in a:suffixes
" echo "suffix=" . suffix
" let x = input("p=")
      try        
          let l:file = s:LookupSource(l:cword, l:primarySuffix, suffix, a:sourcepath)
" echo "EditSource: " . l:file
" let x = input("p=")
  "set verbose=0
      catch /.*/
        echohl WarningMsg
        echo v:exception
        echohl None
        return
      endtry                                         

      if (l:file != "") 
          " automatically save 
          if (g:automatic_update) 
              " save current buffer first
              if expand("%") != ''
                  update
              endif
          endif

          let CMD = a:cmd . " " . l:file
          execute CMD
          return
      endif
    endfor

    let l:cword = expand("<cword>")
    echohl WarningMsg
    echo "Error: No file for '" . l:cword . "'"
    echohl None

endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Find a java file or directory based upon word under cursor
" and the sourcepath parameter.
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:LookupSource(cword, primarySuffix, suffix, sourcepath)
    let l:cword = a:cword

" echo "LookupSource: TOP, suffix=" . a:suffix
    " package classname
    let l:file = s:OverPackageClassName(a:suffix,a:sourcepath)
" echo "LookupSource: OverPackageClassName=" . l:file

    if l:file == "" && a:primarySuffix != a:suffix
      let l:package_regex = '^\s*package\s\+\([a-zA-Z\.]\+\).*$'
" echo "LookupSource: PackageClassName=" . l:package_regex
" let x = input("p=")
      let l:file = s:GetExplicitPakage(l:cword,l:package_regex,a:suffix,a:sourcepath)
" echo "LookupSource: GetExplicitPakage=" . l:file
" let x = input("p=")
    endif

    " local match
    if (l:file == "") 
        let l:file = s:GetLocal(l:cword, a:suffix)
    endif
" echo "LookupSource: GetLocal=" . l:file

    " explicit import 
    if (l:file == "") 
      if (a:primarySuffix == "java")
        let l:import_regex = '^\s*import\s\+\(\S\+\);.*$'
        let l:file = s:GetExplicitImport(l:cword,l:import_regex,a:suffix,a:sourcepath)
      elseif (a:primarySuffix == "scala")
        let l:import_regex = '^\s*import\s\+\([a-zA-Z0-9_\.]\+\).*$'
        let l:file = s:GetExplicitImport(l:cword,l:import_regex,a:suffix,a:sourcepath)
        if (l:file == "") 
          let l:import_regex = '^\s*import\s\+\([a-zA-Z0-9_\.]\+\)\(\._\).*$'
          let l:file = s:GetExplicitImport(l:cword,l:import_regex,a:suffix,a:sourcepath)
          if (l:file == "") 
            let l:import_regex = '^\s*import\s\+\(\S\+\).*$'
            let l:file = s:GetExplicitImport(l:cword,l:import_regex,'java',a:sourcepath)
          endif
        endif
      endif
" echo "LookupSource: GetExplicitImport explicit=" . l:file
    endif

    " look in java/lang
    if (l:file == "") 
      if (a:suffix == "java")
        let l:file = s:GetJavaLang(l:cword)
" echo "LookupSource: GetJavaLang=" . l:file
      elseif (a:suffix == "scala")
        let l:file = s:GetScala(l:cword)
" echo "LookupSource: GetScalaLang=" . l:file
      endif
    endif

    " implicit import 
    if (l:file == "") 
      if (a:suffix == "java")
        let l:import_regex = '^\s*import\s\+\(\(\k\+\.\)\+\)\*;.*$'
        let l:file = s:GetImlicitImport(l:cword,l:import_regex,a:suffix,a:sourcepath)
      elseif (a:suffix == "scala")
        let l:import_regex = '^\s*import\s\+\([a-zA-Z\.]\+\.\)\(_\).*$'
        let l:file = s:GetImlicitImport(l:cword,l:import_regex,a:suffix,a:sourcepath)
      endif
" echo "LookupSource: GetImlicitImport=" . l:file
    endif

    " package statement
    if (l:file == "") 
        let l:file = s:GetPackage(l:cword,a:suffix,a:sourcepath)
" echo "LookupSource: GetPackage=" . l:file
    endif

    if (l:file == "") 
        let l:file = s:LocateFile(l:cword,a:suffix)
    endif

    return l:file
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Is the cursor over a package classname (i.e., java.util.Map)
" This can return a file or directory
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:OverPackageClassName(suffix, sourcepath)
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
            let l:file = ExecutePathMatch(l:tfile,a:suffix, a:sourcepath)

            if (l:file == "") 
                let l:file = s:LocateFile(l:tfile,a:suffix)
            endif

            return l:file
        else
            " cword is in the middle of cdotword, edit directory
            let l:part_dotword = strpart(l:cdotword, 0, l:end)
            let l:tdir = s:SwapDotForSlash(l:part_dotword)
            let l:dir = s:ExecuteDirMatch(l:tdir,a:sourcepath)

            return l:dir
        endif
    endif

    return ""

endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Is the file in the same directory as the current file/buffer
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:GetLocal(cword, suffix)
    " let l:jfile = a:cword . ".java"
    let l:jfile = a:cword . "." . a:suffix
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


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Use locate command to get and then choose a file.
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:LocateFile(tfile, suffix)
    let l:file = ""
" echo "LocateFile: " .  a:tfile
    if (g:javae_use_locate_cmd) 
        let l:command = g:javae_locate_cmd . " /" . a:tfile . "." . a:suffix
        let l:files = system(l:command)

" echo "LocateFile: " .  l:files
        if (l:files != "") 
            let l:file = s:SelectFromListCWD(l:files)
            if (l:file == "") 
                let l:file = s:SelectFromList(l:files)
            endif
        endif
    endif
    return l:file
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Get current working directory and current file's package path,
" subtract to get root path. See if any of the candidate
" choices includes this root path and if so return it.
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:SelectFromListCWD(choices)
    let l:choices = a:choices

    let l:pp = s:GetPackagePath()
    if (l:pp == "") 
        return ""
    endif
    let l:pwd = getcwd()

    let l:pwd_len = strlen(l:pwd)
    let l:pp_len = strlen(l:pp)
    let l:path = strpart(l:pwd, 0, pwd_len - pp_len)

" echo "SelectFromListCWD: path=" . l:path
    " We have one or more choices
    let l:i = stridx(l:choices, "\n")
    let l:x = 0
    while (l:i > -1)
        let l:choice=strpart(l:choices, 0, l:i)
        let l:choices=strpart(l:choices, l:i+1)
        let l:m = match(l:choice, l:path)
        if (l:m != -1)
            return l:choice
        endif
        let l:i = stridx(l:choices, "\n")
        let l:x = l:x + 1
" echo l:x . ": " . l:choice
    endwhile
    return ""
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Get current current file's package path.
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:GetPackagePath()
    let l:package_regex = '^package\s\+\(\S\+\);.*$'
    let l:index = 0
    let l:n = line("$")

    let l:file = ""
    while l:index <= n
        let l:index = l:index + 1

        let l:line = getline(l:index)
        if (l:line == "") 
            continue
        endif

        if (g:javae_syntax_based)
            let l:syn = synIDattr(synID(l:index, 1, 0), "name")
            if (l:syn =~? ".*comment.*")
                " in a comment, continue
                continue
            elseif (l:syn =~? ".*scope.*")
                " start of class declaration, break
                break
            elseif (l:syn =~? ".*class.*")
                " start of class declaration, break
                break
            endif
        endif

        let l:l = matchstr(l:line, l:package_regex)
        if (l:l == "") 
            continue
        endif

        let l:xfile = substitute(l, l:package_regex, '\1', '')
        let l:tfile = s:SwapDotForSlash(l:xfile)
        return l:tfile

    endwhile
    return ""
endfunction

function! s:TestLocateFile(tfile)
    let l:file = ""
    if (g:javae_use_locate_cmd) 
        let l:command = g:javae_locate_cmd . " " . a:tfile . ".java"
        let l:files = system(l:command)

        if (l:files != "") 
" XXXXXXXXXXXXXXXXXXX
" echo "files=" . l:files
" let l:pwd = getcwd()
" echo "pwd=" . l:pwd
        let l:i = match("/home/emberson/java", "/home/emberson")
" echo "i=" . l:i
" We have one or more files
    let l:choices=l:files
    let l:len = stridx(l:choices, "\n")
    let l:i = -1
    let l:x = 0
    while (l:i != 0)
        let l:choice=strpart(l:choices, 0, l:len)
        let l:choices=strpart(l:choices, l:len+1)
        let l:len = stridx(l:choices, "\n")
        let l:i = match(l:pwd, l:choice)
" echo "i=" . l:i
        let l:x = l:x + 1
        echo l:x . ": " . l:choice
    endwhile

    if (l:i == 0) 
" echo "choice=" . l:choice
        let l:file = l:choice
    else

" let l:fullpath = simplify(bufname("%"))
" let l:path_regex = '^\(.*/\)\?\f\+$'
" let l:l = matchstr(l:fullpath, l:path_regex)
" echo "l=" . l:l
" let l:path = substitute(l:l, l:path_regex, '\1', '')
" echo "path=" . l:path

            let l:file = s:SelectFromList(l:files)
    endif
        endif
    endif
    return l:file
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Use locate command to get and then choose a directory.
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
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

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" The choices parameter is a "\n" separated list of (in this case)
" files or directories. If there is only one choice it is returned,
" otherwise one is requested to choose one.
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
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

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Is there an import statement exactly matching the file
" If not, can the file be located
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! BANG()
    let l:cword = expand("<cword>")
echo "BANG=" . l:cword
endfunction

function! TEST(sourcepath)
    let l:cword = expand("<cword>")
echo "TEST=" . l:cword
    let l:file = s:GetExplicitImport(l:cword,a:sourcepath)
    " let l:file = s:GetImlicitImport(l:cword,a:sourcepath)
echo "ex file=" . l:file
    return l:file
endfunction

function! s:GetExplicitPakage(cword,package_regex,suffix,sourcepath)
    let l:package_regex = a:package_regex
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

        if (g:javae_syntax_based)
            let l:syn = synIDattr(synID(l:index, 1, 0), "name")
            if (l:syn =~? ".*comment.*")
                " in a comment, continue
                continue
            elseif (l:syn =~? ".*scope.*")
                " start of class declaration, break
                break
            elseif (l:syn =~? ".*class.*")
                " start of class declaration, break
                break
            endif
        endif

" echo "GetExplicitPakage line=" . l:line

        let l:l = matchstr(l:line, l:package_regex)
        if (l:l == "") 
            continue
        endif


        let l:xfile = substitute(l:l, l:package_regex, '\1', '')
" echo "GetExplicitPakage xfile=" . l:xfile
" let x = input("p=")

"        let l:r = l:file_regex . a:cword . '$'
"        let l:l = matchstr(l:xfile, r)
"        if (l:l == "") 
"            continue
"        endif

        let l:tfile = s:SwapDotForSlash(l:xfile) . "/" . a:cword
" echo "GetExplicitPakage tfile=" . l:tfile
        let l:file = ExecutePathMatch(l:tfile, a:suffix, a:sourcepath)
" echo "GetExplicitPakage file=" . l:file
" let x = input("p=END")

        if (l:file == "") 
            let l:file = s:LocateFile(l:tfile, a:suffix)
        endif

        if (l:file != "") 
            break
        endif


    endwhile

    return l:file

endfunction

function! s:GetExplicitImport(cword,import_regex,suffix,sourcepath)
    " do not match "import static"
    " let l:import_regex = '^\s*import\s\+\(\S\+\);.*$'
    " let l:import_regex = '^\s*import\s\+\([a-zA-Z\.]\+\)\(\._\).*$'
    let l:import_regex = a:import_regex
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

        if (g:javae_syntax_based)
            let l:syn = synIDattr(synID(l:index, 1, 0), "name")
            if (l:syn =~? ".*comment.*")
                " in a comment, continue
                continue
            elseif (l:syn =~? ".*scope.*")
                " start of class declaration, break
                break
            elseif (l:syn =~? ".*class.*")
                " start of class declaration, break
                break
            endif
        endif

" echo "GetExplicitImport line=" . l:line

        let l:l = matchstr(l:line, l:import_regex)
        if (l:l == "") 
            continue
        endif


        let l:xfile = substitute(l:l, l:import_regex, '\1', '')
" echo "GetExplicitImport xfile=" . l:xfile
" let x = input("p=")
        let l:r = l:file_regex . a:cword . '$'
        let l:l = matchstr(l:xfile, r)
        if (l:l == "") 
            continue
        endif

        let l:tfile = s:SwapDotForSlash(l:xfile)
" echo "GetExplicitImport tfile=" . l:tfile
        let l:file = ExecutePathMatch(l:tfile, a:suffix, a:sourcepath)

        if (l:file == "") 
            let l:file = s:LocateFile(l:tfile, a:suffix)
        endif

        if (l:file != "") 
            break
        endif


    endwhile

    return l:file

endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Is there an import statement that end with * matching the file
" If not, can the file be located
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:GetImlicitImport(cword,import_regex,suffix,sourcepath)
    " do not match "import static"
    " let l:import_regex = '^\s*import\s\+\(\(\k\+\.\)\+\)\*;.*$'
    let l:import_regex = a:import_regex
    let l:index = 0
    let l:n = line("$")

"echo "GetImlicitImport"
    let l:file = ""
    while l:index <= n
        let l:index = l:index + 1

        let l:line = getline(l:index)
        if (l:line == "") 
            continue
        endif
        if (g:javae_syntax_based)
            let l:syn = synIDattr(synID(l:index, 1, 0), "name")
            if (l:syn =~? ".*comment.*")
                " in a comment, continue
                continue
            elseif (l:syn =~? ".*scope.*")
                " start of class declaration, break
                break
            elseif (l:syn =~? ".*class.*")
                " start of class declaration, break
                break
            endif
        endif
" echo "l:line=" . l:line

        let l:l = matchstr(l:line, l:import_regex)
        if (l:l == "") 
" echo "continue"
            continue
        endif

        let l:xfile = substitute(l, l:import_regex, '\1', '')
        let l:xfile = xfile . a:cword
        let l:tfile = s:SwapDotForSlash(l:xfile)
" echo "GetImlicitImport=" . l:tfile
" let x = input("p=")
        let l:file = ExecutePathMatch(l:tfile, a:suffix, a:sourcepath)

        if (l:file == "") 
            let l:file = s:LocateFile(l:tfile,a:suffix)
        endif

        if (l:file != "") 
            break
        endif

    endwhile
    return l:file
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" See if file can be found based upon package statement path
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:GetPackage(cword,suffix,sourcepath)
    let l:package_regex = '^package\s\+\(\S\+\);.*$'
    let l:index = 0
    let l:n = line("$")

    let l:file = ""
    while l:index <= n
        let l:index = l:index + 1

        let l:line = getline(l:index)
        if (l:line == "") 
            continue
        endif

        if (g:javae_syntax_based)
            let l:syn = synIDattr(synID(l:index, 1, 0), "name")
            if (l:syn =~? ".*comment.*")
                " in a comment, continue
                continue
            elseif (l:syn =~? ".*scope.*")
                " start of class declaration, break
                break
            elseif (l:syn =~? ".*class.*")
                " start of class declaration, break
                break
            endif
        endif

        let l:l = matchstr(l:line, l:package_regex)
        if (l:l == "") 
            continue
        endif

        let l:xfile = substitute(l, l:package_regex, '\1', '')
        let l:xfile = xfile . "." . a:cword 
        let l:tfile = s:SwapDotForSlash(l:xfile)
        let l:file = ExecutePathMatch(l:tfile,a:suffix, a:sourcepath)

        if (l:file == "") 
            let l:file = s:LocateFile(l:tfile,a:suffix)
        endif

        " if we are here, we have seen a package statment so we
        " should return regardless of the value of l:file
        return l:file

    endwhile
    return l:file
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" See if its in $JAVA_HOME/src/java/lang
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:GetJavaLang(cword)
    if ($JAVA_HOME != "") 
        let l:file = $JAVA_HOME . "/src/java/lang/" . a:cword . ".java"
        if (filereadable(expand(l:file)))
            return file
        endif
    endif
    return ""
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" See if its in $SCALA_HOME/src/scala
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:GetScala(cword)
    if ($SCALA_HOME != "") 
        let l:file = $SCALA_HOME . "/src/scala/" . a:cword . ".scala"
        if (filereadable(expand(l:file)))
            return file
        endif
    endif
    return ""
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Search through sourcepath for matching file
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! ExecutePathMatch(file, suffix, sourcepath)
    let l:sourcepath = a:sourcepath
    let l:path = a:sourcepath
    let l:regex = "^[^,]*"

" echo "ExecutePathMatch=" . a:file
    while (strlen(l:sourcepath))
        let l:path = s:GetFirstPathElement(l:sourcepath, l:regex)
" echo "l:path=" . l:path

        let l:sourcepath = s:RemoveFirstPathElement(l:sourcepath, l:regex)
        " let l:lfile = l:path . "/" . a:file . ".java"
        let l:lfile = l:path . "/" . a:file . "." . a:suffix

" echo "l:lfile=" . l:lfile
        if (filereadable(expand(l:lfile)))
" echo "lfile=" . l:lfile
            return l:lfile
        endif
    endwhile
    return ""
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Search through sourcepath for matching dir
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:ExecuteDirMatch(dir, sourcepath)
    let l:sourcepath = a:sourcepath
    let l:path = a:sourcepath
    let l:regex = "^[^,]*"

    while (strlen(l:sourcepath))
        let l:path = s:GetFirstPathElement(l:sourcepath, l:regex)

        let l:sourcepath = s:RemoveFirstPathElement(l:sourcepath, l:regex)
        let l:lfile = l:path . "/" . a:dir

        if (isdirectory(expand(l:lfile)))
            return l:lfile
        endif
    endwhile
    return ""
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Return everything up to the first "," in a path
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:GetFirstPathElement(path, regex)
    let l:lpath = matchstr(a:path, a:regex)
    return l:lpath
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Remove everything up to the first "," in a path
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:RemoveFirstPathElement(path, regex)
    let l:lpath = a:path
    let l:lregex = a:regex
    let l:lpath = substitute(l:lpath, l:lregex, "", "")
    let l:lpath = substitute(l:lpath, "^,", "", "")
    return l:lpath
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Swap "." for "/" to convert a package path into a file path
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
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
"   super               2
"   keyword             3
"   primitive type      4
"   first letter cap    5 
"   all caps            6
"
let s:first_letter_lower    = 0
let s:this_tag              = 1
let s:super_tag             = 2
let s:keyword               = 3
let s:primitive_type        = 4
let s:first_letter_upper    = 5
let s:all_letter_upper      = 6

function! s:ClassifyPart(part)
    let p = a:part

    if (p == "this")
        return s:this_tag
    endif

    if (p == "super")
        return s:super_tag
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
        return s:keyword
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
        return s:primitive_type
    endif

"let x = input("p=" . p)
    "let all_caps_regex = "\%([A-Z0-9_]*\)"
    let all_caps_regex = '[A-Z0-9_]\{' . strlen(p) . '}'
"let x = input("all_caps_regex=" . all_caps_regex)
    let allCap = matchstr(p, all_caps_regex)
    if (allCap != "")
        return s:all_letter_upper
    endif

    let first_caps_regex = "^[A-Z].*"
    let firstCap = matchstr(p, first_caps_regex)
    if (firstCap != "")
        return s:first_letter_upper
    endif

    " instance variable or method
    return s:first_letter_lower 

endfunction

let s:word_array = ""
let s:word_array_len = ""
let s:word_array_method = 0

function! s:ParseFullWord(cword,fullword)
    let s:word_array = ""
    let s:word_array_len = ""
    let s:word_array_method = 0

    let s:word_array_method = s:IsMethod()
    if (a:cword == a:fullword)
        let p = a:cword
        let pclass = s:ClassifyPart(p)
        let s:word_array = ":" . pclass . "-" . p
        let s:word_array_len = "1"
let s:word_array_type_0 = pclass
let s:word_array_value_0 = p

        return 1
    else
        " parse fullword into parts
        let cnt = 0
        let w = a:fullword
        let s:word_array = ""
        let idx = stridx(w, ".")
        while (idx != -1)
            let p = strpart(w, 0, idx)
            let pclass = s:ClassifyPart(p)
            let s:word_array = s:word_array . ":" . pclass . "-" . p
"let x = input("word_array=" . s:word_array)
let s:word_array_type_{cnt} = pclass
let s:word_array_value_{cnt} = p
            let cnt = cnt + 1
            let w = strpart(w, idx+1, strlen(w) - idx)
"let x = input("w=" . w)
            let idx = stridx(w, ".")
        endwhile
        let p = w
        let pclass = s:ClassifyPart(p)
        let s:word_array = s:word_array . ":" . pclass . "-" . p
let s:word_array_type_{cnt} = pclass
let s:word_array_value_{cnt} = p
        let s:word_array_len = cnt + 1
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
function! FindDef(cmd,suffix,sourcepath)
    let l:cword = expand("<cword>")
    let dot_regex = '\k'
    let haskeyworddot = matchstr('.', dot_regex)

    let current_line = line('.')
    let current_column = col('.')

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

let x = input("word_array=" . s:word_array . 
    \ ", len=" . s:word_array_len .
    \ ", method=" . s:word_array_method
    \)
let cnt = 0
let stmp=""
while (cnt < s:word_array_len)
    let stmp = stmp . ":" . s:word_array_type_{cnt} . "-" . s:word_array_value_{cnt}
    let cnt = cnt + 1
endwhile
let x = input("stmp=" . stmp)

    " this assumes that there are no more than 9 words in package path
    if (s:word_array_method)
"let x = input("is method")
        " if not method we want type
        if (issingleword)
            let method_name=s:word_array_value_0
"let x = input("method_name=" . method_name)
            " simple classname or variable
            if (isclass)
                " its a constructor
"                let r = EditSource(a:cmd,a:sourcepath)
"                return r
                let file = s:LookupSource(l:cword, a:suffix, a:sourcepath)

" what about inner class or self
" let x = input("file=" . file)
                if (file != "") 
                    let $CMD = a:cmd . " " . file
                    execute $CMD
                    " look for public/protected/private constructor
                    let m = '\s*\(public\|protected\|private\)\s\+' . method_name . '\s*('
                    let s = search(m, "W")
                    if (s == 0) 
                        " not found
                        " look for package constructor
                        let m = '\s\+' . method_name . '\s*('
                        let s = search(m, "W")
                    endif
"                    let M = "/ " . method_name . "("
"                    execute M
"let x = input("regex=" . m)

"        execute ":normal 20G"
"        execute ":normal /" . method_name . "("
                    return
                else
                    echohl WarningMsg
                    echo "Error: Could not find file matching '" . cword . "'"
                    echohl None
                endif
            else 

                " its a method in local file or base class
                " reset to initial position before calling
                execute ":normal ". current_line . "G"
                execute ":normal ". current_column . "l"
                call s:FindDefMethod(method_name,a:cmd,a:sourcepath)

            endif
        else 
            " not a singleword: a...b() or A...b()
            execute ":normal ". current_line . "G"
            execute ":normal ". current_column . "l"
            call s:WalkArray()
        endif
    else 
        " not a method
        if (issingleword)
            if (isclass)
            else
            endif
        else
        endif
        call s:WalkArray()
    endif

endfunction

" its a method in local file or base class
function! s:FindDefMethod(method_name,cmd,suffix,sourcepath)
    let current_line = line('.')
    let current_column = col('.')

    " search from top of file
    execute ":normal 1G"
    let m = '^\s*\(public\|protected\|private\)\?\_s\+\k\+\_s\+' . a:method_name . '\_s*(\_s*\(\S\+\_s\+\S\+\_s*\(,\_s*\S\+\_s\+\S\+\_s*\)*\)\?)\_s*{'
    let s = search(m, "W")
    if (s != 0)
        " in local file, go to it
        let s = search(a:method_name, "W")
        return
    else
        " Lets see if theres a base class which might have
        " the method defined in it.

        " get enclosing class declaration
        " go to current line
        execute ":normal ". current_line . "G"
        let l:class_line_nos = s:GetClassDecl()
        if (l:class_line_nos >= 0)
            " found a class declaration, extract base class
            " if it exists
            let l:base_class = s:GetBaseClassName(l:class_line_nos)
            if (l:base_class != "")
                " wow a base class name
                let l:file = s:LookupSource(l:base_class, a:suffix, a:sourcepath)
let x = input("file=" . l:file)
                if (l:file != "")
                    " found the base file, reset in current file
                    execute ":normal ". current_line . "G"
                    " open window on new file
                    let CMD = a:cmd . " " . l:file
                    execute CMD
                    execute ":normal 1G"
                    let s = search(m, "W")
                    if (s != 0)
                        " in base class file
                        let s = search(a:method_name, "W")
                    endif
                endif
            endif
        endif
    endif
    "execute ":normal ". current_line . "G"
endfunction

function! s:GetClassDecl()
    let l:class_line = -1
    let l:current_line = line('.')
"let x = input("current_line=" . l:current_line)
    execute ":normal [{"
    let l:new_line = line('.')
"let x = input("new_line=" . l:new_line)
    while l:new_line < l:current_line
        let l:syn = synIDattr(synID(l:new_line, 1, 0), "name")
        let l:current_line = l:new_line
        if (l:syn =~? ".*scope.*")
            " start of class declaration, break
            let l:class_line = l:new_line
            break
        elseif (l:syn =~? ".*class.*")
            " start of class declaration, break
            let l:class_line = l:new_line
            break
        endif
        execute ":normal [{"
        let l:new_line = line('.')
let x = input("new_line=" . l:new_line)
    endwhile

let x = input("class_line=" . l:class_line)
    return l:class_line
endfunction

function! s:GetBaseClassName(class_line_nos)
    execute ":normal k"

"let x = input("class_line_nos=" . a:class_line_nos)
"let l = getline('.')
"let x = input("line=" . l)
    let extends_regex = '^\s*\(public\|protected\|private\|abstract\|final\|\_s\)*class\_s\+\k\+\_s\+extends\_s\+\k\+\(\_s*implements\_s*\(\S\+\_s\+\S\+\_s*\(,\_s*\S\+\_s\+\S\+\_s*\)*\)\?\)\?\_s*{'
"let extends_regex = '^\s*\(public\|protected\|private\|abstract\|final\|\_s\)*class\_s\+\k\+\_s\+extends\_s\+\k\+\_s*{'
"let extends_regex = 'public class Foo extends Bar {'
    execute ":normal 0"
    let l:s = search(extends_regex, "W")
let x = input("s=" . l:s)

    if (l:s > 0) 
        execute "/extends"
        let l:s = search("extends", "W")
"let x = input("s=" . l:s)
        if (l:s > 0) 
            " goto next word - the base class name
            execute ":normal w"
            let l:cword = expand("<cword>")
"let x = input("baseclass=" . l:cword)
            return l:cword
        endif
    endif

    return ""

endfunction

function! s:WalkArray()
    
    let tmp = ""
    let l:cnt = 0
    while (l:cnt < s:word_array_len)
        let l:type = s:word_array_type_{cnt}
        let l:value = s:word_array_value_{cnt}

        if l:type == s:first_letter_lower
            let tmp = tmp . " first_letter_lower"
        elseif l:type == s:this_tag
            let tmp = tmp . " this_tag"
        elseif l:type == s:super_tag
            let tmp = tmp . " super_tag"
        elseif l:type == s:keyword 
            let tmp = tmp . " keyword"
        elseif l:type == s:primitive_type
            let tmp = tmp . " primitive_type"
        elseif l:type == s:first_letter_upper
            let tmp = tmp . " first_letter_upper"
        elseif l:type == s:all_letter_upper
            let tmp = tmp . " all_letter_upper"
        endif

        let l:cnt = l:cnt + 1
    endwhile
let x = input("tmp=" . tmp)
endfunction

" hints that are cleared at the start and set as information is
" determined
let s:hint_instance_variable = 0

function! s:WalkArrayXXX(cnt)
    let l:type = s:word_array_type_{a:cnt}
    let l:value = s:word_array_value_{a:cnt}

    let l:cnt = a:cnt + 1

    if l:type == s:first_letter_lower
        if l:cnt = s:word_array_len
            " processing last element in array
            if s:word_array_method == 0
                " its a variable
            else 
                " its a method
            endif
        else 
        endif

    elseif l:type == s:this_tag
        " Its either an instance variable or 'this.' applied to a method.
        " Remember, we might have an inner non-static class instance 
        " referring to its outer instance.
        "
        " constructor
        "       this ( ArgumentListopt ) ;
        " instance variable
        "       this.Identifier 
        if s:word_array_method == 0
            " its an instance variable
            let s:hint_instance_variable = 1
            call s:WalkArrayXXX(l:cnt)
        else
            " its an instance method
            call s:WalkArrayXXX(l:cnt)
        endif

    elseif l:type == s:super_tag
        " We are either in a constructor calling base constructor,
        " calling a base method or accessing base variable
        "
        " constructor
        "       super ( ArgumentListopt ) ;
        "       Primary.super ( ArgumentListopt ) ; 
        " method
        "       super . Identifier ( ArgumentListopt )
        "       ClassName . super . Identifier ( ArgumentListopt )
        " instance variable
        "       super . Identifier
        "       ClassName .super . Identifier
        if s:word_array_method == 1
            if l:cnt = s:word_array_len
            else 
            endif
        else 
            if l:cnt = s:word_array_len
                " error condition
                call s:PrintErrorMsg('Use of "super" requires base class variable')
            else
            endif
        endif

    elseif l:type == s:keyword 
        let tmp = tmp . " keyword"
    elseif l:type == s:primitive_type
        let tmp = tmp . " primitive_type"
    elseif l:type == s:first_letter_upper
        let tmp = tmp . " first_letter_upper"
    elseif l:type == s:all_letter_upper
        let tmp = tmp . " all_letter_upper"
    endif

endfunction

function! WhereAmI()
    let current_line = line('.')
    let current_column = col('.')

    let a = s:MakeSearchArray(current_line)
let x = input("a=" . a)

    execute ":normal ". current_line . "G"
    execute ":normal ". current_column . "l"
endfunction

" an array of start/end positions
" len-s:e-s:e
" :echo synIDattr(synID(".", 1, 0), "name")
function! s:MakeSearchArray(pos)
    " reset search array
    let l:search_array = ""
    let l:cnt = 0
    :normal [m
    let l:start = line(".")
    :normal ]M
    let l:end = line(".")

    if l:start < a:pos && l:end > a:pos
        " in method
        let l:search_array = a:pos . ":" . l:start
        let l:cnt = l:cnt + 1

    else
        " outside method
    endif

    let l:search_array = l:cnt . "-" . l:search_array 
    return l:search_array
endfunction

function! s:PrintErrorMsg(msg)
    echohl WarningMsg
    echo "Error: " . a:msg
    echohl None
endfunction
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" TEST END
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
