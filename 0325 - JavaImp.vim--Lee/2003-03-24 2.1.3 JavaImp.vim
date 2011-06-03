" -*- vim -*-
" FILE: "H:\vim\vimfiles\plugin\JavaImp.vim" {{{
" LAST MODIFICATION: "Mon, 24 Mar 2003 09:04:27 Pacific Standard Time (wlee)"
" HEADER MAINTAINED BY: N/A
" VERSION: 2.1.3
" (C) 2002-2003 by William Lee, <wlee@sendmail.com>
" }}}


" PURPOSE: {{{
"   - A utility to insert and sort Java import statements.
"   
" REQUIREMENTS:
"   - It's recommended that you have the Unix "sort" binary (or the Windows's
"   "sort" or cygwin's "sort" will do) in your path.   For the jar support you
"   also need to have the
"   "jar" binary in your path.
"
" USAGE:
"   Put this file in your ~/.vim/plugin directory.
"
"   You need to set two global variables in your .vimrc in order for this to
"   work:
"
"       let g:JavaImpPaths = "..."
"       let g:JavaImpDataDir = "..."
"
"   The g:JavaImpPaths is a comma separated list of paths that point to the
"   roots of the 'com' or 'org' etc.
"
"   For example: if you have your Java source files in
"   /project/src/java/com/blah and /project2/javasrc/org/blah...., you'll put
"   this in your .vimrc file:
"
"       let g:JavaImpPaths = "/project/src/java,/project2/javasrc"
"
"   If there are too many paths, you can split them into separate lines:
"
"       let g:JavaImpPaths = "/project/src/java," .
"          \ "/project2/javasrc," .
"          \ "/project3/javasrc"
"
"   Note: Don't forget the ',' to separate the paths.
"
"   If ',' is not convenient for you, set g:JavaImpPathSep to the
"   (single-character) separator you would like to use:
"
"       let g:JavaImpPathSep = ':'
"
"   The g:JavaImpDataDir is a directory that you use to store JavaImp
"   settings and cache files. Default is:
"
"       let g:JavaImpDataDir = $HOME . "/vim/JavaImp"
"
"   Note: Since version 2.1, the "g:JavaImpDataDir" variable replaces
"   "g:JavaImpClassList" and "g:JavaImpJarCache".  If "g:JavaImpClassList" and
"   "g:JavaImpJarCache" are not set, then they default to
"   "g:JavaImpDataDir/JavaImp.txt" and "g:JavaImpDataDir/cache/" accordingly.
"   The files and directory will be created automatically when you generate
"   the JavaImp.txt file.  It's recommended that you to use the
"   g:JavaImpDataDir variable instead.
"
"   Now you are ready for some actions.  You can now do a:
"
"       :JavaImpGenerate or :JIG
"
"   If you have not created the directory for g:JavaImpDataDir yet, this will
"   create the appropriate paths.  JIG will go through your JavaImpPaths and
"   search for anything that ends with .java, .class, or .jar.  It'll then
"   write the mappings to the JavaImp.txt and/or the cache files.
"   
"   After you've generated your JavaImp.txt file, move your cursor to a class name
"   in a Java file and do a:
"
"       :JavaImp or :JI
"
"   And the magic happens!  You'll realize that you have an extra import
"   statement inserted after the last import statement in the file.  It'll
"   also prompts you with duplicate class names and insert what you have
"   selected.  If the class name is already imported, it'll do nothing.  
"
"   Doing a:
"
"       :JavaImpSilent
"
"   will do a similar thing with less verbosity.  This is useful to be used in
"   another script.
"
"   You can also sort the import statements in the file by doing:
"
"       :JavaImpSort or :JIS
"
"   Other Settings
"   --------------
"   You can make the following settings in your .vimrc file:
"
"   (Deprecated in 2.1) The g:JavaImpClassList is a file you specify to store
"   the class mappings.  The default is set to:
"   
"       let g:JavaImpClassList = g:JavaImpDataDir . "/JavaImp.txt"
"
"   (Deprecated in 2.1, enabled by default and set relative to the
"   g:JavaImpDataDir) It's recommended that you set a directory for the
"   caching result for your jar files.  JavaImp saves the result from each
"   "jar" command in this directory. The default is set to:
"
"       let g:JavaImpJarCache = g:JavaImpDataDir . "/cache"
"
"   (In in 2.1, g:JavaImpSortBin is set to "sort", so this is disabled by
"   default) The sorting algorithm gives preferences to the java.* classes.
"   You can turn this behavior off by putting this in your .vimrc file.
"
"       let g:JavaImpSortJavaFirst = 0
" 
"   NOTE: If you do not have a sort binary, set the following to "", and
"   JavaImp will use the pure Vim implementation.  However, if
"   JavaImpClassList gets too huge, the sorting might choke. If you
"   are on a unix machine or you have the sort binary in your path, it's
"   recommended that you set (or leave it as default):
"
"       let g:JavaImpSortBin = "sort" 
"
"   By default, the sort algorithm will insert a blank line among package
"   group with package root for 2 similar levels.  For example, the import of
"   the following:
"
"       import java.util.List;
"       import org.apache.tools.zip.ZipEntry;
"       import javax.mail.search.MessageNumberTerm;
"       import java.util.Vector;
"       import javax.mail.Message;
"       import org.apache.tools.ant.types.ZipFileSet;
"
"   will become:
"
"       import java.util.List;
"       import java.util.Vector;
"
"       import javax.mail.Message;
"       import javax.mail.search.MessageNumberTerm;
"
"       import org.apache.tools.ant.types.ZipFileSet;
"       import org.apache.tools.zip.ZipEntry;
"
"   Note the classes that begins similar package root with two beginning
"   levels of package hierarchy are stuck together.  You can set the
"   g:JavaImpSortPkgSep to change this behavior.  The default
"   g:JavaImpSortPkgSep is set to 2.  Do not set it too high though for you'll
"   insert a blank line after each import.  If you do not want to insert blank
"   lines among the imports, set:
"
"       let g:JavaImpSortPkgSep = 0
"
"   Be warned that g:JavaImpSortRemoveEmpty has no effect if
"   g:JavaImpSortPkgSep is set.
"
"   Extras
"   ------
"   After you have generated the JavaImp.txt file by using :JIG, you can use
"   it as your dictionary for autocompletion.  For example, you can put the
"   following in your java.vim ftplugin (note here g:JavaImpDataDir is set
"   before running this):
"
"   exe "setlocal dict=" . g:JavaImpDataDir . "/JavaImp.txt"
"   setlocal complete-=k
"   setlocal complete+=k
"   
"   or put this in your .vimrc
"   
"   exe "set dict=" . g:JavaImpDataDir . "/JavaImp.txt"
"   set complete-=k
"   set complete+=k
"
"   After you have done so, you can open a .java file and use ^P and ^N to
"   autocomplete your Java class names.
"
"   Importing your JDK Classes
"   --------------------------
"   JavaImp also support a file type called "jmplst".  A jmplst file
"   essentially contains the output of a "jar tf" command.  It is mainly use
"   in the case where you want to import some external classes in JavaImp but
"   you do not have the source directory nor the jar file.  For example, you
"   can import the JDK classes, which can be located in $JAVA_HOME/src.jar
"   (different in different distributions) with your JDK distribution,
"   by using the jmplst file:
"
"   To expose the standard JDK classes to JavaImp:
"
"   1. If you have "sed":
"      > jar tf $JAVA_HOME/src.jar | sed -e 's#^src/##' > jdk.jmplst
"
"      If you do not have "sed:
"      > jar tf $JAVA_HOME/src.jar > jdk.jmplst
"      > vim jdk.jmplst
"      [Execute the following vim commands:]
"
"      1G0<C-v>Gllld:w
"
"      [ This will select vertically the all the "src/" prefixes and delete
"      them, then save the file. Essentially, we want to get rid of the src
"      directory otherwise it'll screw up the import statements. ]
"
"   2. Put the jdk.jmplst in a directory that you've added in your
"      g:JavaImpPaths.  For example, I put my jdk.jmplst in
"      $HOME/vim/JavaImp/jmplst directory, and add $HOME/vim/JavaImp/jmplst to
"      g:JavaImpPaths.
"
"   3. Open vim with the JavaImp.vim loaded and Do a :JIG.  You should see
"      that JavaImp will pick up many more classes.
"
"   4. Try to do a :JI on a "Vector" class, for example, to see whether you
"      can add the import statement from the JDK.
"
"
"
"   Enjoy!
"
" CREDITS:
"
"   Please mail any comment/suggestion/patch to 
"
"   William Lee <wlee@sendmail.com>
"
"   (c) 2002-2003. All Rights Reserved
"
" THANKS:
"
"   Eric Y. Kow for his bug fixes, addition on jar support, elimination of
"   duplicates, and Unix sorting implementation.
"   
"   Robert Webb for his sorting function.
"
"   Matt Paduano, Toby Allsopp, Dirk Duehr, and others for their bug
"   fixes/patches.
"
" HISTORY:
"  2.1.3  - 3/17/2003 Added an option to override the default JavaImpPaths
"                     separator.  Fixes an error when trying to do :JI over an
"                     unamed buffer.
"  2.1.2  - 3/14/2003 Fixes a critical bug where a single import match
"                     would put in a "0" as its name.  Thanks Matt Paduano for
"                     pointing this out.
"  2.1.1  - 3/10/2003 Contains various bug fixes.  Added configurable settings
"                     to control levels of import separation.
"  2.1.0  - 3/5/2003  Adds an option to remove empty lines when sorting the
"                     imports.  You can also insert a line among packages
"                     after sorting.  Adds documentation on how to import
"                     your JDK classes.  Makes it easier to start
"                     by setting many useful defaults.  Note that previous
"                     users may need to modify their settings.  See the
"                     instruction for details.  Added memory to previously
"                     selected classes when multiple classes are found.
"  2.0.2  - 9/9/2002  Tries to make the cursor function to work on Vim 6.0
"  2.0.1  - 9/6/2002  Fixes some debug statements that writes to /tmp.  Make
"                     it work on Windows.
"  2.0  - 9/5/2002  Added jar support (including caching for the results) and a
"                   menu to select duplicated class names during insert.
"  1.0.1 - 7/1/2002 Fixed a bug where the current window closes when you have
"                   a split
"                   window.
"  1.0  - 6/30/2002 Initial release
"

" -------------------------------------------------------------------  
" Mappings 
" -------------------------------------------------------------------  

command! -nargs=? JIX call <SID>JavaImpQuickFix()
command! -nargs=? JI call <SID>JavaImpInsert(1)
command! -nargs=? JavaImp call <SID>JavaImpInsert(1)
command! -nargs=? JavaImpSilent call <SID>JavaImpInsert(0)

command! -nargs=? JIG call <SID>JavaImpGenerate()
command! -nargs=? JavaImpGenerate call <SID>JavaImpGenerate()

command! -nargs=? JIS call <SID>JavaImpSort()
command! -nargs=? JavaImpSort call <SID>JavaImpSort()

" -------------------------------------------------------------------  
" Default configuration 
" -------------------------------------------------------------------  

if(has("unix"))
    let s:SL = "/"
elseif(has("win16") || has("win32") || has("win95") ||
            \has("dos16") || has("dos32") || has("os2"))
    let s:SL = "\\"
else
    let s:SL = "/"
endif
" Sort the import with preferences to the java.* classes
"
"
if !exists("g:JavaImpDataDir")
    let g:JavaImpDataDir = expand("$HOME") . s:SL . "vim" . s:SL . "JavaImp"
endif

" Deprecated
if !exists("g:JavaImpClassList")
    let g:JavaImpClassList = g:JavaImpDataDir . s:SL . "JavaImp.txt"
endif

" Deprecated
if !exists("g:JavaImpSortJavaFirst")
    let g:JavaImpSortJavaFirst = 1
endif

" Deprecated
if !exists("g:JavaImpJarCache")
    let g:JavaImpJarCache = g:JavaImpDataDir . s:SL . "cache"
endif

if !exists("g:JavaImpSortRemoveEmpty")
    let g:JavaImpSortRemoveEmpty = 1
endif

" Note if the SortPkgSep is set, then you need to remove the empty lines.
if !exists("g:JavaImpSortPkgSep")
    if (g:JavaImpSortRemoveEmpty == 1)
        let g:JavaImpSortPkgSep = 2
    else
        let g:JavaImpSortPkgSep = 0
    endif
endif


if !exists("g:JavaImpSortBin")
    let g:JavaImpSortBin = "sort" 
endif

if !exists("g:JavaImpPathSep")
    let g:JavaImpPathSep = ","
endif

" -------------------------------------------------------------------  
" Generating the imports table
" -------------------------------------------------------------------  

"Generates the mapping file
fun! <SID>JavaImpGenerate()
    if (<SID>JavaImpChkEnv() != 0)
        return
    endif
    " We would like to save the current buffer first:
    if expand("%") != '' 
        update
    endif
    cclose
    "Recursivly go through the directory and write to the temporary file.
    let impfile = tempname()
    " Save the current buffer number
    let currBuff = bufnr("%")
    silent exe "split ".impfile
    let currPaths = g:JavaImpPaths
    " See if currPaths has a separator at the end, if not, we add it.
        "echo "currPaths begin is " . currPaths
    if (match(currPaths, g:JavaImpPathSep . '$') == -1)
        let currPaths = currPaths . g:JavaImpPathSep
    endif

    "echo currPaths
    while (currPaths != "" && currPaths !~ '^ *' . g:JavaImpPathSep . '$')
        let sepIdx = stridx(currPaths, g:JavaImpPathSep)
        " Gets the substring exluding the newline
        let currPath = strpart(currPaths, 0, sepIdx)
        echo "Searching in path: " . currPath
        "echo "currPaths: ".currPaths
        let currPaths = strpart(currPaths, sepIdx + 1, strlen(currPaths) - sepIdx - 1)
        "echo "(".currPaths.")"
        call <SID>JavaAppendClass(currPath,"")
    endwhile
    "silent exe "write! /tmp/raw"
    let classCount = line("$")

    " Formatting the file
    "echo "Formatting the file..." 
    1,$call <SID>JavaImpFormatList()
    "silent exe "write! /tmp/raw_formatted"

    " Sorting the file
    echo "Sorting the classes, this may take a while ..."
    1,$call <SID>Sort("s:Strcmp")
    "silent exe "write! /tmp/raw_sorted"

    echo "Assuring uniqueness..." 
    1,$call <SID>CheesyUniqueness()
    let uniqueClassCount = line("$") - 1
    "silent exe "write! /tmp/raw_unique"

    " before we write to g:JavaImpClassList, close g:JavaImpClassList
    " exe "bdelete ".g:JavaImpClassList (we do this because a user might
    " want to do a JavaImpGenerate after having been dissapointed that
    " his JavaImpInsert missed something... 
    if (bufexists(g:JavaImpClassList))
        silent exe "bd! " g:JavaImpClassList
    endif

    silent exe "write!" g:JavaImpClassList
    close
    " Delete the temporary file
    call delete(impfile)
    echo "Done.  Found " . classCount . " classes (". uniqueClassCount. " unique)"
endfun

" The helper function to append a class entry in the class list
fun! <SID>JavaAppendClass(cpath, relativeTo)
    "echo "Arguments " . a:cpath . " package is " . a:relativeTo
    if strlen(a:cpath) < 1 
        echo "Alert! Bug in JavaApppendClass (JavaImp.vim)"
        echo " - null cpath relativeTo ".a:relativeTo
        echo "(beats me... hack the source and figure it out)"
        " Base case... infinite loop protection
        return 0
    elseif (!isdirectory(a:cpath) && match(a:cpath, '\(\.class$\|\.java$\)') > -1)
        " oh good, we see a single entry like org/apache/xerces/bubba.java
        " just slap it on our tmp buffer
        if (a:relativeTo == "")
            call append(0, a:cpath)
        else
            call append(0, a:relativeTo)
        endif
    elseif (isdirectory(a:cpath))
        " no class we need to determine whether the path is a directory or
        " not, if it is a directory, we need to run this recursively.
        let l:files = glob(a:cpath . "/*") 
        let l:files = l:files . "\n"
        while (l:files != "" && l:files !~ '^ *\n$')
            let l:sepIdx = stridx(l:files, "\n")
            " Gets the substring exluding the newline
            let l:file = strpart(l:files, 0, l:sepIdx)
            " echo "file [".l:file."] index(".l:sepIdx.")\nleft [\n".l:files."]"
            let l:pkgcomponent = fnamemodify(l:file, ":t")
            let l:newRelativeTo = a:relativeTo 
            if (a:relativeTo == "")
                let l:newpackage = l:pkgcomponent
            else
                let l:newpackage = a:relativeTo. "/" . l:pkgcomponent
            endif
            " we recurse until we hit a real file 
            call  <SID>JavaAppendClass(l:file,l:newpackage)
            " ready the next bit of the while loop 
            let l:files = strpart(l:files, l:sepIdx + 1, strlen(l:files) - l:sepIdx - 1)
        endwhile
    elseif (match(a:cpath, '\(\.jar$\)') > -1)
        " Check if the jar file exists, if not, we return immediately.
        if (!filereadable(a:cpath))
            echo "Skipping " . a:cpath . ". File does not exist."
            return 0
        endif
        " If we get a jar file, we first tries to match the timestamp of the
        " cache defined in g:JavaImpJarCache directory.  If the jar is newer,
        " then we would execute the jar command.  Otherwise, we just slap the
        " cached file to the buffer.
        "
        " The cached entries are organized in terms of the relativeTo path
        " with the '/' characters replaced with '_'.  For example, if you have
        " your jar in the directory /blah/lib/foo.jar, you'll have a cached
        " file called _blah_lib_foo.jmplst in your cache directory.
        
        let l:jarcache = expand(g:JavaImpJarCache)
        let l:jarcmd = 'jar -tf "'.a:cpath . '"'
        if (l:jarcache != "")
            let l:cachefile = substitute(a:cpath, '[ :\\/]',  "_", "g")
            let l:cachefile = substitute(l:cachefile, "jar$",  "jmplst", "")
            let l:jarcache = l:jarcache . s:SL . l:cachefile
            " Note that if l:jarcache does not exist, it'll return -1
            if (getftime(l:jarcache) < getftime(a:cpath))
                " jar file is newer
                " if we get a jar, just slap the jar -tf contents to the cache
                echo "  - Updating jar: " . fnamemodify(a:cpath, ":t") . "\n"
                let l:jarcmd = "!".l:jarcmd . " > \"" . escape(l:jarcache, '\\') . "\""
                silent execute l:jarcmd
                if (v:shell_error != 0)
                    echo "  - Error running the jar command: " . l:jarcmd
                endif
            else
                "echo "  - jar (cached): " . fnamemodify(a:cpath, ":t") . "\n"
            endif
            " Slap the cached content to the buffer
            silent execute "read " . l:jarcache
        else
            echo "  - Updating jar: " . fnamemodify(a:cpath, ":t") . "\n"
            " Always slap the output for the jar command to the file if cache
            " is turned off.
            silent execute "read !".l:jarcmd
        endif
    elseif (match(a:cpath, '\(\.jmplst$\)') > -1)
        " a jmplist is something i made up... it's basically the output of a jar -tf
        " operation.  Why is this useful?  
        " 1) to save time if there is a jar you read frequently (jar -tf is slow)
        " 2) because the java src.jar (for stuff like javax.swing)
        "    has everything prepended with a "src/", for example "src/javax/swing", so
        "    what i did was to run that through perl, stripping out the src/ and store
        "    the results in as java-1_3_1.jmplist in my .vim directory... 

        " we just insert its contents into the buffer
        "echo "  - jmplst: " . fnamemodify(a:cpath, ":t") . "\n"
        silent execute "read ".a:cpath
    endif
endfun

" Converts the current line in the buffer from a java|class file pathname 
"  into a space delimited class package
" For example: 
"  /javax/swing/JPanel.java 
"  becomes:
"  JPanel javax.swing
" If the current line does not appear to contain a java|class file, 
" we blank it out (this is useful for non-bytecode entries in the 
" jar files, like gif files or META-INF)
fun! <SID>JavaImpFormatList() 
    let l:currentLine = getline(".")

    " -- get the package name
    let l:subdirectory = fnamemodify(l:currentLine, ":h") 
    let l:packageName = substitute(l:subdirectory, "/", ".", "g")

    " -- get the class name 
    " this match string extracts the classname from a class path name
    " in other words, if you hand /javax/swing/JPanel.java, it would 
    " return in JPanel (as regexp var \1)
    let l:classExtensions = '\(\.class\|\.java\)'
    let l:matchClassName = match(l:currentLine, '[\\/]\([\$1-9A-Za-z_]*\)'.classExtensions.'$')
    if l:matchClassName > -1
        let l:matchClassName = l:matchClassName + 1 
        let l:className = strpart(l:currentLine, l:matchClassName)
        let l:className = substitute(l:className,  l:classExtensions, '', '')
        " subst '$' -> '.' for classes defined inside other classes
        " don't know if it will do anything useful, but at least it 
        " will be less incorrect than it was before
        let l:className = substitute(l:className, '\$', '.', 'g')
        call setline(".", l:className." ".l:packageName.".".l:className)
    else
        " if we didn't find something which looks like a class, we
        " blank out this line (sorting will pick this up later)
        call setline(".", "")
    endif
endfun

" -------------------------------------------------------------------  
" Inserting imports 
" -------------------------------------------------------------------  

" Inserts the import statement of the class specified under the cursor in the
" current .java file.
"
" If there is a duplicated entry for the classname, it'll insert the entry as
" comments (starting with "//")
"
" If the entry already exists (specified in an import statement in the current
" file), this will do nothing.
"
" pass 0 for verboseMode if you want fewer updates of what this function is 
"  doing, or 1 for normal verbosity
" (silence is interesting if you're scripting the use of JavaImpInsert...
"  for example, i have a script that runs JavaImpInsert on all the 
"  class not found errors)
fun! <SID>JavaImpInsert(verboseMode)
    if (<SID>JavaImpChkEnv() != 0)
        return
    endif
    if a:verboseMode
        let verbosity = ""
    else
        let verbosity = "silent"
    end
       
    " Write the current buffer first (if we have to).  Note that we only want
    " to do this if the current buffer is named.
    if expand("%") != '' 
        exec verbosity "update"
    endif

    " Save the location of the cursor
    let savedrow = line(".")
    let savedcol = col(".")
    let currBuff = bufnr("%")

    " choose the current word for the class
    let className = expand("<cword>")
    let pattern = '^\s*import\s\s*.*[.]' . className . ';'
    " Split and jump
    split
    " First search for the className in an import statement
    normal G$
    if (search(pattern, "w") != 0)
        " if we find a preexisting import statement that covers
        " this class,  go back to the old location
        "call <SID>JumpToCursor(savedrow, savedcol)
        " close the window
        close
        if verbosity != "silent"
            echo "Import for " . className . " found in this file."
        endif
    else 
        " close the window
        close
        " good... we didn't find a preexisting import... that means
        " there is work to do

        " notice that we switch to the JavaImpClassList buffer 
        " (or load the file if needed)
        let icl = expand(g:JavaImpClassList)
        if (filereadable(icl))
            silent exe "split " . icl
        else
            echo "Can not load the class map file " . icl . "."
            return
        endif
        let importLine = ""
        normal G$

        let flags = "w"
        let firstImport = 0
        let importCtr = 0
        let pattern = '^' . className . ' '
        let firstFullPackage = ""
        while (search(pattern, flags) > 0)
            let importCtr = importCtr + 1
            let fullPackage = substitute(getline("."), '\S* \(.*\)$', '\1', "")
            let importLine = importLine . fullPackage . "\n"
            let flags = "W"
        endwhile
        " Loading back the old file
        close
        "silent exe "b " . currBuff
        if (importCtr == 0)
            if ! a:verboseMode
                echo className." not found (you should update the class map file)"
            else
                echo "Can not find any class that matches " . className . "."
                let input = confirm("Do you want to update the class map file?", "&Yes\n&No", 2)
                if (input == 1)
                    call <SID>JavaImpGenerate()
                endif
           endif
        else
            let pickedImport = <SID>JavaImpChooseImport(importCtr, className, importLine)
            let importLine = "import " . pickedImport . ";"
            " Split before we jump
            split
            let hasImport = <SID>JavaImpGotoLast()
            
            if (hasImport != 0)
                let currentLine = line(".")
                exec verbosity "call append(currentLine, importLine)"
            else
                let hasPackage = <SID>JavaImpGotoPackage()
                let currentLine = line(".")
                
                if (hasPackage != 1)
                    " insert the import statement if there's no package
                    " statement.
                    exec verbosity 'call append(0, importLine)'
                else
                    exec verbosity 'call append(currentLine, importLine)'
                    exec verbosity 'call append(currentLine, "")'
                endif
            endif

            if a:verboseMode
                echo "Inserted " . pickedImport . " for " . className 
            endif 

            " go back to the old location
            close
        endif
    endif
endfun

" -------------------------------------------------------------------  
" Choosing and caching imports 
" -------------------------------------------------------------------  

" Check with the choice cache and determine the final order of the import
" list.
" The choice cache is a file with the following format:
" [className1] [most recently used class] [2nd most recently used class] ...
" [className2] [most recently used class] [2nd most recently used class] ...
" ...
"
" imports and the return list consists of fully-qualified classes separated by
" \n.  This function will list the imports list in the order specified by the
" choice cache file
"
" IMPORTANT: if the choice is not available in the cache, this returns
" empty string, not the imports
fun! <SID>JavaImpMakeChoice(imctr, className, imports)
    let jicc = expand(g:JavaImpDataDir) . s:SL . "choices.txt"
    if !filereadable(jicc)
        return ""
    endif
    silent exe "split " . jicc
    let flags = "w"
    let pattern = '^' . a:className . ' '
    if (search(pattern, flags) > 0)
        let l = substitute(getline("."), '^\S* \(.*\)', '\1', "")
        close
        return <SID>JavaImpOrderChoice(a:imctr, l, a:imports)
    else
        close
        return ""
    endif
endfun

" Order the imports with the cacheLine and returns the list.
fun! <SID>JavaImpOrderChoice(imctr, cacheLine, imports)
    " we construct the imports so we can test for <space>classname<space>
    let il = " " . substitute(a:imports, "\n", " ", "g") . " "
    "echo "orig: " . a:imports
    "echo "il: " . il
    let rtn = " "
    " We first construct check each entry in the cacheLine to see if it's in
    " the imports list, if so, we add it to the final list.
    let cl = a:cacheLine . " "
    while (cl !~ '^ *$')
        let sepIdx = stridx(cl, " ")
        let cls = strpart(cl, 0, sepIdx)
        let pat = " " . cls . " "
        if (match(il, pat) >= 0)
            let rtn = rtn . cls . " "
        endif
        let cl = strpart(cl, sepIdx + 1)
    endwhile
    "echo "cache: " . rtn
    " at this point we need to add the remaining imports in the rtn list.
    " get rid of the beginning space
    let mil = strpart(il, 1)
    "echo "mil: " . mil
    while (mil !~ '^ *$')
        let sepIdx = stridx(mil, " ")
        let cls = strpart(mil, 0, sepIdx)
        let pat = " " . escape(cls, '.') . " "
        " we add to the list if only it's not in there.
        if (match(rtn, pat) < 0)
            let rtn = rtn . cls . " "
        endif
        let mil = strpart(mil, sepIdx + 1)
    endwhile
    " rid the head space
    let rtn = strpart(rtn, 1)
    let rtn = substitute(rtn, " ", "\n", "g")
    "echo "return : " . rtn
    return rtn
endfun

" Save the import to the cache file.
fun! <SID>JavaImpSaveChoice(className, imports, selected)
    let im = substitute(a:imports, "\n", " ", "g")
    " Note that we remove the selected first
    let spat = a:selected . " "
    let spat = escape(spat, '.')
    let im = substitute(im, spat, "", "g")

    let jicc = expand(g:JavaImpDataDir) . s:SL . "choices.txt"
    silent exe "split " . jicc
    let flags = "w"
    let pattern = '^' . a:className . ' '
    let l = a:className . " " . a:selected . " " . im
    if (search(pattern, flags) > 0)
        " we found it, replace the line.
        call setline(".", l)
    else
        " we couldn't found it, so we just add the choices
        call append(0, l)
    endif

    silent update
    close
endfun

" Choose the import if there's multiple of them.  Returns the selected import
" class.
fun!<SID>JavaImpChooseImport(imctr, className, imports)
    let imps = <SID>JavaImpMakeChoice(a:imctr, a:className, a:imports)
    let uncached = (imps == "")
    if uncached
        let imps = a:imports
        let simps = a:imports
        if (a:imctr > 1)
            let imps = "[No previous choice.  Please pick one from below...]\n".imps
        endif
    else
        let simps = imps
    endif

    let choice = 0 
    if (a:imctr > 1) 
      " if the item had not been cached, we force the user to make
      " a choice, rather than letting her choose the default
      let choice = <SID>JavaImpDisplayChoices(imps, a:className)
      " if the choice is not cached, we don't want the user to
      " simply pick anything because he is hitting enter all the
      " time so we loop around he picks something which isn't the
      " default (earlier on, we set the default to some nonsense
      " string)
      while (uncached && choice == 0) 
        let choice = <SID>JavaImpDisplayChoices(imps, a:className)
      endwhile
    endif

    " If cached, since we inserted the banner, we need to subtract the choice
    " by one:
    if (uncached && choice > 0)
        let choice = choice - 1
    endif

    " We run through the string again to pick the choice from the list
    " First reset the counter
    let ctr = 0
    let imps = simps
    while (imps != "" && imps !~ '^ *\n$')
        let sepIdx = stridx(imps, "\n")
        " Gets the substring exluding the newline
        let imp = strpart(imps, 0, sepIdx)
        if (ctr == choice)
            " We found it, we should update the choices
            "echo "save choice simps:" . simps . " imp: " . imp
            call <SID>JavaImpSaveChoice(a:className, simps, imp)
            return imp
        endif
        let ctr = ctr + 1
        let imps = strpart(imps, sepIdx + 1, strlen(imps) - sepIdx - 1)
    endwhile
    " should not get here...
    echo "warning: should-not-get here reached in JavaImpMakeChoice"
    return 
endfun

fun! <SID>JavaImpDisplayChoices(imps, className)
    let imps = a:imps
    let simps = imps
    let ctr = 0
    let choice = 0
    let cfmstr = ""
    let questStr =  "Multiple matches for " . a:className . ". Your choice?\n"
    while (imps != "" && imps !~ '^ *\n$')
        let sepIdx = stridx(imps, "\n")
        " Gets the substring exluding the newline
        let imp = strpart(imps, 0, sepIdx)
        let questStr = questStr . "(" . ctr . ") " . imp . "\n"
        let cfmstr = cfmstr . "&" . ctr . "\n"
        let ctr = ctr + 1
        let imps = strpart(imps, sepIdx + 1, strlen(imps) - sepIdx - 1)
    endwhile

    if (ctr <= 10)
        " Note that we need to get rid of the ending "\n" for it'll give
        " an extra choice in the GUI
        let cfmstr = strpart(cfmstr, 0, strlen(cfmstr) - 1)
        let choice = confirm(questStr, cfmstr, 0)
        " Note that confirms goes from 1 to 10, so if the result is not 0,
        " we need to subtract one
        if (choice != 0)
            let choice = choice - 1
        endif
    else
        let choice = input(questStr)
    endif

    return choice
endfun

" -------------------------------------------------------------------  
" Sorting 
" -------------------------------------------------------------------  

" Sort the import statements in the current file.
fun! <SID>JavaImpSort()
    split
    let hasImport = <SID>JavaImpGotoFirst()
    if (hasImport == 0)
        close
        echo "No import statement found."
        return
    else
        let firstImp = line(".")
        call <SID>JavaImpGotoLast()
        let lastImp = line(".")
        if (g:JavaImpSortRemoveEmpty == 1)
            call <SID>JavaImpRemoveEmpty(firstImp, lastImp)
            " We need to get the range again
            call <SID>JavaImpGotoLast()
            let lastImp = line(".")
        endif
        if (g:JavaImpSortJavaFirst == 1)
            exe "" . firstImp . "," . lastImp . "call <SID>Sort(\"s:JavaImpStrcmp\")"
        else
            exe "" . firstImp . "," . lastImp . "call <SID>Sort(\"s:Strcmp\")"
        endif

        if (g:JavaImpSortPkgSep > 0)
            call <SID>JavaImpAddPkgSep(firstImp, lastImp, g:JavaImpSortPkgSep)
        endif
        close
        return
    endif
endfun

" Remove empty lines in the range
func! <SID>JavaImpRemoveEmpty(fromLine, toLine)
    silent exe "" . a:fromLine . "," . a:toLine . ' g/^\s*$/d'
endfunction

" -------------------------------------------------------------------  
" Inserting spaces between packages 
" -------------------------------------------------------------------  

" Given a sorted range, we would like to add a new line (do a 'O')
" to seperate sections of packages.  The depth argument controls
" what we treat as a seperate section.
"
" Consider the following: 
" -----
"  import java.util.TreeSet;
"  import java.util.Vector;
"  import org.apache.log4j.Logger;
"  import org.apache.log4j.spi.LoggerFactory;
"  import org.exolab.castor.xml.Marshaller;
" -----
"
" With a depth of 1, this becomes
" -----
"  import java.util.TreeSet;
"  import java.util.Vector;

"  import org.apache.log4j.Logger;
"  import org.apache.log4j.spi.LoggerFactory;
"  import org.exolab.castor.xml.Marshaller;
" -----

" With a depth of 2, it becomes
" ----
"  import java.util.TreeSet;
"  import java.util.Vector;
"
"  import org.apache.log4j.Logger;
"  import org.apache.log4j.spi.LoggerFactory;
"
"  import org.exolab.castor.xml.Marshaller;
" ----
" Depth should be >= 1, but don't set it too high, or else this function
" will split everything up.  The recommended depth setting is "2"
func! <SID>JavaImpAddPkgSep(fromLine, toLine, depth)
    "echo "fromLine: " . a:fromLine . " toLine: " . a:toLine." depth:".a:depth
    if (a:depth <= 0) 
      return
    endif  
      
    let cline = a:fromLine
    let endline = a:toLine
    let lastPkg = <SID>JavaImpGetSubPkg(getline(cline), a:depth)

    let cline = cline + 1
    while (cline <= endline)
        let thisPkg = <SID>JavaImpGetSubPkg(getline(cline), a:depth)
        
        " If last package does not equals to this package, append a line
        if (lastPkg != thisPkg)
            "echo "last: " . lastPkg . " this: " . thisPkg
            call append(cline - 1, "")
            let endline = endline + 1
            let cline = cline + 1
        endif
        let lastPkg = thisPkg
        let cline = cline + 1
    endwhile
endfunction

" -------------------------------------------------------------------  
" Quickfix 
" -------------------------------------------------------------------  

" Taken from Eric Kow's dev script...
"
" This function will try to open your error window, given that you have run Ant
" and the quickfix windows contains unresolved symbol error, will fix all of
" them for you automatically!
function! <SID>JavaImpQuickFix()
    if (<SID>JavaImpChkEnv() != 0)
        return
    endif
    " FIXME... we should figure out if there are no errors and
    " quit gracefully, rather than let vim do its error thing and
    " figure out where to stop
    crewind
    cn
    cn 
    copen
    let l:nextStr = getline(".")
    echo l:nextStr
    let l:currentStr = ''

    crewind
    " we use the cn command to advance down the quickfix list until
    " we've hit the last error 
    while match(l:nextStr,'|[0-9]\+ col [0-9]\+|') > -1 
        " jump to the quickfix error window
        cnext
        copen
        let l:currentLine = line(".")
        let l:currentStr=getline(l:currentLine)
        let l:nextStr=getline(l:currentLine + 1)
        
        if (match(l:currentStr, 'cannot resolve symbol$') > -1 ||
                    \ match(l:currentStr, 'Class .* not found.$') > -1 ||
                    \ match(l:currentStr, 'Undefined variable or class name: ') > -1)

            " get the filename (we don't use this for the sort, 
            " but later on when we want to sort a file's after
            " imports after inserting all the ones we know of
            let l:nextFilename = substitute(l:nextStr,  '|.*$','','g')
            let l:oldFilename = substitute(l:currentStr,'|.*$','','g')
            
            " jump to where the error occurred, and fix it
            cc
            call <SID>JavaImpInsert(0)

            " since we're still in the buffer, if the next line looks
            " like a different file (or maybe the end-of-errors), sort
            " this file's import statements
            if l:nextFilename != l:oldFilename 
                call <SID>JavaImpSort()
            endif
        endif

        " this is where the loop checking happens
    endwhile
endfunction

" -------------------------------------------------------------------  
" (Helpers) Vim-sort for those of us who don't have unix or cygwin 
" -------------------------------------------------------------------  

" Function for use with Sort(), to compare.  This gives preferences to the
" java* import for I want it to appear first.
func! <SID>JavaImpStrcmp(str1, str2)
    if (match(a:str1, '^\s*import\s*java.*') >= 0 && match(a:str2, '^\s*import\s*java.*') == -1)
        return -1
    endif

    if (match(a:str1, '^\s*import\s*java.*') == -1 && match(a:str2, '^\s*import\s*java.*') >= 0)
        return 1
    endif

    if (a:str1 < a:str2)
        return -1
    elseif (a:str1 > a:str2)
        return 1
    else
        return 0
    endif
endfunction

" Sorting functions from the Vim docs.  Use this instead of the sort binary.
"
" Function for use with Sort(), to compare two strings.
function! <SID>Strcmp(str1, str2)
    if (a:str1 < a:str2)
        return -1
    elseif (a:str1 > a:str2)
        return 1
    else
        return 0
    endif
endfunction

" Sort lines.  SortR() is called recursively.
func! <SID>SortR(start, end, cmp)
    if (a:start >= a:end)
        return
    endif
    let partition = a:start - 1
    let middle = partition
    let partStr = getline((a:start + a:end) / 2)
    let i = a:start
    while (i <= a:end)
        let str = getline(i)
        exec "let result = " . a:cmp . "(str, partStr)"
        if (result <= 0)
            " Need to put it before the partition.  Swap lines i and partition.
            let partition = partition + 1
            if (result == 0)
                let middle = partition
            endif
            if (i != partition)
                let str2 = getline(partition)
                call setline(i, str2)
                call setline(partition, str)
            endif
        endif
        let i = i + 1
    endwhile

    " Now we have a pointer to the "middle" element, as far as partitioning
    " goes, which could be anywhere before the partition.  Make sure it is at
    " the end of the partition.
    if (middle != partition)
        let str = getline(middle)
        let str2 = getline(partition)
        call setline(middle, str2)
        call setline(partition, str)
    endif
    call <SID>SortR(a:start, partition - 1, a:cmp)
    call <SID>SortR(partition + 1, a:end, a:cmp)
endfunc

" To Sort a range of lines, pass the range to Sort() along with the name of a
" function that will compare two lines.
func! <SID>Sort(cmp) range
    if (g:JavaImpSortBin != "")
        execute a:firstline.",".a:lastline."!" . g:JavaImpSortBin
    else
        " the default is using the pure vim method... but this sort
        " method does recursion too deep if the file is huge
        call <SID>SortR(a:firstline, a:lastline, a:cmp)
    endif
endfunc

" -------------------------------------------------------------------  
" (Helpers) Goto...
" -------------------------------------------------------------------  

" Go to the package declaration
fun! <SID>JavaImpGotoPackage()
    " First search for the className in an import statement
    normal G$
    let flags = "w"
    let pattern = '^\s*package\s\s*.*;'
    if (search(pattern, flags) == 0)
        return 0
    else
        return 1
    endif
endfun

" Go to the last import statement that it can find.  Returns 1 if an import is
" found, returns 0 if not.
fun! <SID>JavaImpGotoLast()
    " First search for the className in an import statement
    normal G$
    let flags = "w"
    let pattern = '^\s*import\s\s*.*;'
    let importFound = 0
    while search(pattern, flags) > 0
        let importFound = 1
        let flags = "W"
    endwhile
    return importFound
endfun

" Go to the last import statement that it can find.  Returns 1 if an import is
" found, returns 0 if not.
fun! <SID>JavaImpGotoFirst()
    normal G$
    let pattern = '^\s*import\s\s*.*;'
    return (search(pattern, "w") > 0)
endfun

" -------------------------------------------------------------------  
" (Helpers) Miscellaneous 
" -------------------------------------------------------------------  

" Removes all duplicate entries from a sorted buffer
" preserves the order of the buffer and runs in o(n) time
func! <SID>CheesyUniqueness() range
    let l:storedStr = getline(1)
    let l:currentLine = 2 
    let l:lastLine = a:lastline
    "echo "starting with l:storedStr ".l:storedStr.", l:currentLine ".l:currentLine.", l:lastLine".lastLine
    while l:currentLine < l:lastLine 
        let l:currentStr = getline(l:currentLine)
        if l:currentStr == l:storedStr
            "echo "deleting line ".l:currentLine
            exe l:currentLine."delete"
            " note that we do NOT advance the currentLine counter here
            " because where currentLine is is what was once the next 
            " line, but what we do have to do is to decrement what we 
            " treat as the last line
            let l:lastLine = l:lastLine - 1
        else
            let l:storedStr = l:currentStr
            let l:currentLine = l:currentLine + 1
            "echo "set new stored Str to ".l:storedStr
        endif
    endwhile 
endfunc

" -------------------------------------------------------------------  
" (Helpers) Making sure directory is set up 
" -------------------------------------------------------------------  

" Returns 0 if the directory is created successfully.  Returns non-zero
" otherwise.
function! <SID>JavaImpCfmMakeDir(dir)
    if (! isdirectory(a:dir))
        let input = confirm("Do you want to create the directory " . a:dir . "?", "&Create\n&No", 1)
        if (input == 1)
            return <SID>JavaImpMakeDir(a:dir)
        else
            echo "Operation aborted."
            return 1
        endif
    endif
endfunc

function! <SID>JavaImpMakeDir(dir)
    if(has("unix"))
        let cmd = "mkdir -p " . a:dir
    elseif(has("win16") || has("win32") || has("win95") ||
                \has("dos16") || has("dos32") || has("os2"))
        let cmd = "mkdir \"" . a:dir . "\""
    else
        return 1
    endif
    call system(cmd)
    let rc = v:shell_error
    "echo "calling " . cmd
    return rc
endfunc

" Check and make sure the directories are set up correctly.  Otherwise, create
" the dir or complain.
fun! <SID>JavaImpChkEnv()
    let rc = <SID>JavaImpCfmMakeDir(g:JavaImpDataDir)
    if (rc != 0)
        echo "Error creating directory: " . g:JavaImpDataDir
        return rc
    endif
    "echo "Created directory: " . g:JavaImpDataDir
    let rc = <SID>JavaImpCfmMakeDir(g:JavaImpJarCache)
    if (rc != 0)
        echo "Error creating directory: " . g:JavaImpJarCache
        return rc
    endif
    "echo "Created directory: " . g:JavaImpJarCache
    return 0
endfunc

" Returns the classname of an import statement
" For the string "import foo.bar.Frobnicability;"
" , this returns "Frobnicability" 
"
" If not given an import statement, this returns
" empty string
fun! <SID>JavaImpGetClassname(importStr,depth) 
    let pkgMatch = '\s*import\s*.*\.[^.]*;$'
    let pkgGrep = '\s*import\s*.*\.\([^.]*\);$'
    
    if (match(a:importStr, pkgMatch) == -1)
        let classname = ""
    else
        let classname = substitute(a:importStr, pkgGrep, '\1', "")
    endif
    return classname
endfunc


" Returns the (sub) package name of an import " statement.  
"
" Consider the string "import foo.bar.Frobnicability;"
"
" If depth is 1, this returns "foo"
" If depth is 2, this returns "foo.bar"
" If depth >= 3, this returns "foo.bar.Frobnicability"
fun! <SID>JavaImpGetSubPkg(importStr,depth) 
    " set up the match/grep command 
    let subpkgStr = '[^.]\{-}\.'
    let pkgMatch = '\s*import\s*.*\.[^.]*;$'
    let pkgGrep = '\s*import\s*\('
    let curDepth = a:depth
    " we tack on a:depth extra subpackage to the end of the match
    " and grep expressions 
    while (curDepth > 0) 
      let pkgGrep = pkgGrep.subpkgStr
      let curDepth = curDepth - 1
    endwhile
    let pkgGrep = pkgGrep.'\)'.'.*;$'
    " echo pkgGrep
    
    if (match(a:importStr, pkgMatch) == -1)
        let lastPkg = ""
    else
        let lastPkg = substitute(a:importStr, pkgGrep, '\1', "")
    endif

    " echo a:depth.' gives us '.lastPkg
    return lastPkg
endfunc
