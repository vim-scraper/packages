" Author:  Eric Van Dewoestine
" Version: 0.4
"
" Description: {{{
"   Filetype plugin for java that allows you to jump to
"   another java src file by searching your configured
"   src path for the class name under the cursor.
"
"   For a given class name, supports finding children of it, implementations
"   of it, methods that return it, methods that take it as an argument, test
"   cases for it, or all references of it.
"
" Platform:
"   This plugin uses the system find and grep commands extensively,
"   so if you are having issues, it's probably due to incompatibility
"   with one of those.  You can take a look at s:find_cmd, s:content_grep,
"   and s:*_regex variables to see if they can be modified to suit your
"   system.
"
"   If you find that you do have to modify any of these, feel free to
"   send me your changes, and I will see if they are compatable or make
"   alternate configurations available for users to choose from.
"
"   All my testing was done with
"     GNU find version 4.1.20
"     GNU grep version 2.5.1
"
"   Linux:
"     All testing has been done on linux so various flavors of linux
"     should be supported (as well as other *nix systems).
"
"   Windows:
"     Works in conjunction with cygwin_utils.vim plugin.
"     Make sure to read the Description comments in cygwin_utils.vim for
"     configuration instructions and other notes.
"
"     Note: I've found that the 'find' command runs considerably
"     slower on Windows enviroments.  So if you have serveral
"     paths in you g:JavaSrcPaths variable, then searching may
"     be a bit slow.
"
" Usage:
"   When editing a java file the following commands are created
"    :FindSrc
"    :FindImpl
"    :FindChildren
"    :ContextualFindSrc
"    :FindTest
"    :FindArgument
"    :FindReturned
"    :FindReferenced
"
"   FindSrc: will search for the src file for the class name under the
"   cursor and execute the configured g:JavaSrcCmd on it (unless
"   g:JavaSrcUseGrep is set to 1), which could do any number of
"   things such as edit, argadd, split, etc.
"
"   FindImpl: behaves just like FindSrc, except instead of searching for
"   the class name under the cursor, it searches for classes that
"   implement that class.
"
"   FindChildren: behaves just like FindSrc, except instead of searching
"   for the class name under the cursor, it searches for classes that
"   extend that class.
"
"   ContextualFindSrc: is a convenience command that decides which of the
"   above commands to execute (FindSrc, FindImpl, or FindChildren) based
"   on your position in the file.
"   - If you are on the class declaration line (public class Foo), then
"     it will execute FindChildren.
"   - If you are on the interface declaration line (public interface Foo),
"     then it will execute FindImpl.
"   - Any where else in the file, FindSrc will be executed.
"
"   FindTest: behaves like FindSrc, except instead of searching
"   for the class name under the cursor, it searches for the test
"   case src file for the class.
"     This command assumes that test case src files follow the junit
"     naming conventions of "<classname>Test.java" where the <classname>
"     is the name of the class under the cursor.
"
"   FindArgument: behaves like FindSrc, except instead of searching
"   for the class name under the cursor, it searches for all src files
"   that have a method that takes as an argument, that class.
"
"   FindReturned: behaves like FindSrc, except instead of searching
"   for the class name under the cursor, it searches for all src files
"   that have a method that returns that class.
"
"   FindReferenced: behaves like FindSrc, except instead of searching
"   for the class name under the cursor, it searches for all src files
"   that reference that class.
"
"   Note: All of the above commands (with the exception of ContextualFindSrc)
"   can also be invoked with an argument.
"     Ex.
"       FindSrc org.bar.Foo
"   This will invoke the command so it searches for the class name provided
"   rather than the one under the cursor.  You may use fully qualified
"   (org.bar.Foo) or un-qualified (Foo) class names.
"
"   Note: All of these find commands will attempt to filter out
"   results that reference a different class that has the same
"   class name as the one to search for.  It does this by attempting
"   to locate the package name of the class to search for.
"
"     For example: If you have two classes with the name Foo
"     one in com.bar.Foo and the other in com.bar.baz.Foo, and you
"     start a search for the class Foo, then this plugin will attempt
"     to determine which Foo you are searching for.
"
"     If you are editing one of the Foo classes, and you initiated the
"     search with the cursor on the class declaration (public class Foo),
"     then the package (package com.bar;)  declaration of the file will
"     be used to narrow the results to the correct Foo class.
"
"     If you are in another file (say Bar.java) and you start a search
"     for Foo, then the plugin will look to see if you used the fully
"     qualified class name (com.bar.Foo) or if you imported the class
"     (import com.bar.Foo;).  If you did either then the package name
"     will be grabbed from there.  If you have done neither, then the
"     plugin will return results for all Foo occurances regardless
"     of package name.
"
"     So the moral of the story is, that if you want accurate results
"     you should get in the habit of importing each class seperately
"     and avoiding the * imports (import com.bar.*;).  This is a good
"     habit anyways, that makes your code easier to read, navigate, and
"     understand by others.
"
" Configuration:
"   The following describes the global variables you can set in
"   your vimrc to change the behavior of this plugin.
"
"     g:JavaSrcPaths
"       No default.  Use this variable to set a comma seperated list
"       of the paths you wish to have searched. Each path should be
"       the parent directory of where your package structure begins.
"       So for /a/path/src/java/org/acme/, assumming packages begin
"       as org, you would use /a/path/src/java as the path to search.
"
"       Ex.  let g:JavaSrcPaths=
"              \ "/a/path/src/java," .
"              \ "/another/path/src/java"
"
"     g:JavaSrcTestPaths
"       No default. Same as g:JavaSrcPaths but used by FindTest to
"       locate unit testing src files.
"
"     g:JavaSrcShowResults
"       Defaults to 0.  When set to 1, the Find commands will open
"       a small preview window when multiple results have been found.
"       In that window you can hit <enter> on the file you wish
"       to open or use 'A' to load all results into the arg list.
"
"       Ex. let g:JavaSrcShowResults=1
"
"     g:JavaSrcUseGrep
"       Defaults to 1.  When set to 1, the Find commands will use vim's
"       :grep command to grep the results so that results can be
"       navigated through using the quickfix command :cnext :cprev
"       :copen, etc.
"       Note: When g:JavaSrcShowResults is set, then grep is not used
"             regardless of whether g:JavaSrcUseGrep is set or not.
"
"       Ex. let g:JavaSrcUseGrep=1
"
"       Use
"         :help quickfix.txt
"       to get more info on how grep and the quickfix paradigm works.
"
"     Note: When both g:JavaSrcShowResults and g:JavaSrcUseGrep are set
"           to 0, then the g:JavaSrcCmd is executed on all files found
"           with no preview or quickfix window.
"
"     g:JavaSrcCwindow
"       Defaults to 1.  Used to determine whether or not to execute
"       :cwindow to open the grep results after searching.  If set
"       to 1, the window will be opened after every Find command.
"       If set to 0, the window will remain closed and you can open
"       it manually.
"       Note:  Only used when g:JavaSrcUseGrep is set to 1 and
"              g:JavaSrcShowResults is set to 0.
"
"       Ex. let g:JavaSrcCwindow=1
"
"     g:JavaSrcCmd
"       Defaults to "split <file>".  Use this variable to set the
"       command you wish to be invoked when using :Find* commands.
"       The "<file>" will be replace with the file to open.
"       Note: When g:JavaSrcShowResults is set to 0 and
"             g:JavaSrcUseGrep is set to 1, this variable is not
"             used.  Instead the quickfix commands are used.
"
"       Ex. let g:JavaSrcCmd="topleft split <file>"
"       Ex. let g:JavaSrcCmd="argedit <file>"
"       Ex. let g:JavaSrcCmd="call g:MyOpenFile(<file>)"
"
" Command Mapping:
"   You may find it easier to map a command to something more convenient.
"   For example, here is my mapping that allows me to simply hit
"   <enter> on a class name to have it search for results based on the
"   context (as described above).
"
"     autocmd FileType java map <silent> <buffer> <cr> :ContextualFindSrc<cr>
"
" Limitations:
"   - FindChildren and FindImpl are limited to examining class names
"     that follow "extends" | "implements" on the same line.  So src files
"     that put each class on it's own line or wrap long extends / implements
"     statements may not be found.
"     This is a limitation of grep, which is used to find the files.
"     This could be solved by further examining the contents of src files
"     but would have a big impact on performance.
"   - FindReturned only searches for methods with "public" | "private" |
"     "protected" so as not to grab too many files.  Also the scope identifier
"     must appear on the same line as the class name.
"     As with the limitation on FindChildren and FileImpl, this is due to
"     grep examining the files one line at a time, which prevents examining
"     other lines to determine any sort of context.
"
" Todo:
"   - Extend beyond class based searching and add method and field based
"     searches (fields should be easy, methods could be hard to get reliable
"     results).
"
" License:
"
"   Permission is hereby granted, free of charge, to any person obtaining
"   a copy of this software and associated documentation files (the
"   "Software"), to deal in the Software without restriction, including
"   without limitation the rights to use, copy, modify, merge, publish,
"   distribute, sublicense, and/or sell copies of the Software, and to
"   permit persons to whom the Software is furnished to do so, subject to
"   the following conditions:
"
"   The above copyright notice and this permission notice shall be included
"   in all copies or substantial portions of the Software.
"
"   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
"   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}

" Only do this when not done yet for this buffer
if exists("b:did_src_link_ftplugin")
  finish
endif
let b:did_src_link_ftplugin = 1

" Script Varables {{{
let s:escape_chars =  " `|\"'#&()[]!"

" actions
let s:find_src = 1
let s:find_children = 2
let s:find_referenced = 3
let s:find_test = 4

let s:src_ext = ".java"
let s:src_path_separator = ","
let s:src_path_results_separator = "\n"

let s:find_cmd = "find <dir> <grep>"
let s:extends = "extends"
let s:implements = "implements"

let s:src_grep = "-name \"<file>\""
let s:content_grep = s:src_grep . " -exec grep -l \"<regex>\" {} "
if has("win32") || has("win64")
  let s:content_grep = s:content_grep . ";"
else
  let s:content_grep = s:content_grep . "\\\\;"
endif

" regular expressions for grep
let s:src_regex =
  \ '^[[:alpha:][:space:]]*\\\\(class\\\\|interface\\\\)' .
  \ '[a-z[:space:]]*\\\\<<classname>\\\\>'
let s:children_regex =
  \ '\\\\<<modifier>\\\\>.*[[:space:].]\\\\<<classname>\\\\>'
let s:returned_regex =
  \ '[[:space:]]*[a-z]*\\\\(public\\\\|private\\\\|protected\\\\)' .
  \ '[[:space:]+][[:space:]a-z]*\\\\<<classname>\\\\>' .
  \ '[[:space:]+][a-z][_a-zA-Z0-9]*[[:space:]*]('
let s:argument_regex =
  \ '\\\\(([[:space:]*]\\\\<<classname>\\\\>[[:space:]+]\\\\|' .
  \ '\\\\<<classname>\\\\>[[:space:]+][_a-z][_a-zA-Z0-9]*[,)]\\\\)'
let s:referenced_regex = '\\\\<<classname>\\\\>'

" regular expressions for filtering by package names
let s:package_declared =
  \ '^[[:space:]]*package[[:space:]+]<package>[[:space:]]*;'
let s:package_imported =
  \ '^[[:space:]]*import[[:space:]+]<package>\\.\\*[[:space:]]*;'
let s:class_imported =
  \ '^[[:space:]]*import[[:space:]+]<package>\\.<classname>[[:space:]]*;'
let s:fully_qualified =
  \ '<package>.<classname>\\>'

let s:path_separator = "/"
" }}}

" Global Varables {{{
if !exists("g:JavaSrcCmd")
  let g:JavaSrcCmd = "split <file>"
  " instead of split you can add to arg list and edit.
  "let g:JavaSrcCmd = "argedit <file>"
endif

if !exists("g:JavaSrcShowResults")
  let g:JavaSrcShowResults = 0
endif

if !exists("g:JavaSrcUseGrep")
  let g:JavaSrcUseGrep = 1
endif

if !exists("g:JavaSrcCwindow")
  let g:JavaSrcCwindow = 1
endif
" }}}

" ContextualFindSrc() {{{
" Execute the supplied find command.
if !exists("*s:ContextualFindSrc")
function! s:ContextualFindSrc ()
  let line = getline(line("."))

  let classname = expand("<cword>")

  if line =~ "interface [a-z ]*" . classname
    call s:FindChildren(s:implements, "")
  elseif line =~ "class [a-z ]*" . classname
    call s:FindChildren(s:extends, "")
  else
    call s:FindSrc("")
  endif
endfunction
endif " }}}

" FindSrc(classname) {{{
" Find the source file for the class name.
if !exists("*s:FindSrc")
function! s:FindSrc (classname)
  call s:Find(s:src_regex, s:find_src, a:classname)
endfunction
endif " }}}

" FindChildren(modifier, classname) {{{
" Find source files that extend / implement the class name.
if !exists("*s:FindChildren")
function! s:FindChildren (modifier, classname)
  let grep_regex = substitute(s:children_regex, "<modifier>", a:modifier, "g")
  call s:Find(grep_regex, s:find_children, a:classname)
endfunction
endif " }}}

" FindTest(classname) {{{
" Find the corresponding test src file (junit, cactus, etc)
if !exists("*s:FindTest")
function! s:FindTest (classname)
  if !exists("g:JavaSrcTestPaths")
    echoe 'java_src_link.vim: g:JavaSrcTestPaths must be set'
    return
  endif

  let classname = a:classname
  if classname == ""
    let classname = expand("<cword>")
  endif

  let classname = classname . "Test"

  call s:FindClassname(s:src_regex, classname, g:JavaSrcTestPaths, s:find_test)
endfunction
endif " }}}

" Find(regex, action, classname) {{{
" Execute the supplied regex.
if !exists("*s:Find")
function! s:Find (regex, action, classname)
  if !exists("g:JavaSrcPaths")
    echoe 'java_src_link.vim: g:JavaSrcPaths must be set'
    return
  endif

  let classname = a:classname
  if classname ==""
    let classname = expand("<cword>")
  endif

  let paths = g:JavaSrcPaths

  " check if the path of the file is in path list... if not add it.
  let cwd = expand("%:p:h")
  let cwd = substitute(cwd, '\(.*\/\)\(org\|com\|net\)\/.*', '\1', '')

  let paths_remaining = 1
  let contains_path = 0

  " search through each path
  let temp_paths = paths
  while paths_remaining
    let index = stridx(temp_paths, s:src_path_separator)
    if index > 0
      let curpath = strpart(temp_paths, 0, index)
      let temp_paths = strpart(temp_paths, index + 1, strlen(paths))
    else
      let paths_remaining = 0
      let curpath = temp_paths
    endif

    if curpath !~ s:path_separator . "$"
      let curpath = curpath . s:path_separator
    endif

    if stridx(cwd, expand(curpath)) != -1
      let contains_path = 1
      break
    endif
  endwhile

  if !contains_path
    let paths = paths . s:src_path_separator . cwd
  endif

  call s:FindClassname(a:regex, classname, paths, a:action)
endfunction
endif " }}}

" FindClassname(regex, classname, paths, action) {{{
" Execute the supplied regex for the supplied classname, across the
" supplied paths.
if !exists("*s:FindClassname")
function! s:FindClassname (regex, classname, paths, action)
  let classname = a:classname
  let package = ""

  " if fully qualified classname supplied, split apart package and classname.
  if classname =~ '\.'
    let index = strridx(classname, '.')
    let package =  strpart(classname, 0, index)
    let classname = strpart(classname, index + 1)
  endif

  " enforce that a classname was supplied and that it conforms with
  " java naming conventions.
  if classname == "" || classname =~ '^[.*-+]$' || classname =~ '^[a-z]'
    return
  endif

  echom "Searching (Ctrl-C to cancel)..."

  let file = classname . s:src_ext
  let file_dir = ""
  let regex = substitute(a:regex, "<classname>", classname, "g")

  " construct the find command.
  if regex != s:src_regex
    let grep = substitute(s:content_grep, "<regex>", regex, "g")
    let grep = substitute(grep, "<file>", "*.java", "g")
  else
    let grep = s:src_grep
  endif
  let find_cmd = substitute(s:find_cmd, "<grep>", grep, "g")
  let find_cmd= substitute(find_cmd, "<file>", file, "")

  " see if the classname contains any path information
  let index = strridx(file, s:path_separator)
  if index > 0
    let file_dir = strpart(file, 0, index)
    let file = strpart(file, index + 1)

    let dir = dir . file_dir
  endif

  let paths_remaining = 1
  let paths = a:paths
  let results = ""

  " search through each path
  while paths_remaining
    let index = stridx(paths, s:src_path_separator)
    if index > 0
      let curpath = strpart(paths, 0, index)
      let paths = strpart(paths, index + 1, strlen(paths))
    else
      let paths_remaining = 0
      let curpath = paths
    endif

    if curpath !~ s:path_separator . "$"
      let curpath = curpath . s:path_separator
    endif

    let dir = curpath . file_dir
    let dir = s:DeterminePath(dir)

    if has("win32") || has("win64")
      let dir = escape(WindowsPath(dir), ' \')
    endif

    let find = substitute(find_cmd, "<dir>", dir, "")

    " uncomment to debug find commands.
    "echom find
    let globresults = system(find)
    if globresults != ""
      if results != ""
        let results = results . s:src_path_results_separator
      endif
      let results = results . globresults
    endif
  endwhile

  if results != ""
    let results = s:NarrowResults(results, classname, package, a:action)
    " on windows there are 2 \n seperating each result
    let results = substitute(results, '\n\n', '\n', 'g')
  endif

  if results == ""
    echom "java_src_link.vim: No matching src files found"
    return
  endif

  let multipleresults = 0
  if stridx(results, s:src_path_results_separator) > 0
    let multipleresults = 1
  endif

  if g:JavaSrcShowResults && multipleresults
    new
    normal xj
    resize 5

    silent put=results

    normal ggdd

    nnoremap <silent> <buffer> <cr> :call <SID>Execute()<cr>
    nnoremap <silent> <buffer> A :call <SID>LoadArgs()<cr>

    setlocal noswapfile
    setlocal buftype=nofile
    setlocal bufhidden=delete
    setlocal nomodifiable
    setlocal nobuflisted
    setlocal nowrap

  else

    " create empty grep results
    if !g:JavaSrcShowResults && g:JavaSrcUseGrep
      silent grep nothing %
    endif

    let regex = substitute(regex, '\\\\', '\', "g")

    let files_remaining = 1
    while files_remaining
      let index = stridx(results, s:src_path_results_separator)
      if index > 0
        let curfile = strpart(results, 0, index)
        let results = strpart(results, index + 1, strlen(results))
      else
        let files_remaining = 0
        let curfile = results
      endif

      " make sure all new line chars are stripped off the file
      let curfile = substitute(curfile, '\n', '', 'g')

      if curfile != ""
        if !g:JavaSrcShowResults && g:JavaSrcUseGrep
          "exec "echom \"grepadd! " . regex . " " . curfile . "\""
          silent call s:Grep("grepadd! \"" . regex . "\" " . curfile)
        else
          silent exec substitute(g:JavaSrcCmd, "<file>", curfile, "g")
        endif
      endif
    endwhile
  endif

  " open the quickfix window
  if !g:JavaSrcShowResults && g:JavaSrcUseGrep && g:JavaSrcCwindow
    cwindow
  else
    echom "Searching (Ctrl-C to cancel)... Done."
  endif

  normal 
endfunction
endif " }}}

" NarrowResults(results, classname, package, action) {{{
" Narrow the results by trying to determine the package name for the
" class and filtering out results that are using the same classname
" from a different package.
if !exists("*s:NarrowResults")
function! s:NarrowResults (results, classname, package, action)

  let results = a:results
  let filtered = results

  let package = a:package

  "inter/class : package - reference: import, prefix"
  let line = getline(".")

  " if no package provided, then search for one
  if package == ""
    " line contains fully qualified class name
    if line =~ '\.' . a:classname
      let package = substitute(line, '.\{-}\([a-zA-Z.]*' . a:classname . '\).*', '\1', '')
      let index = strridx(package, '.')
      if index != -1
        let package = strpart(package, 0, index)
      endif

    " on class / interface declaration, so use package statement for class
    elseif line =~ '^.*\(interface\|class\).*' . a:classname
      let package = s:GetPackageDeclaration()

    " try to get the import statement for the class name.
    else
      "let package = s:GetPackagePrefix(a:classname)
      "if package == ""
        let package = s:GetPackageImport(a:classname)
      "endif
    endif
  endif

  " found the package, so filter by it.
  if package != ""
    let filtered = ""

    let files_remaining = 1
    while files_remaining
      let index = stridx(results, s:src_path_results_separator)
      if index > 0
        let curfile = strpart(results, 0, index)
        let results = strpart(results, index + 1, strlen(results))
      else
        let files_remaining = 0
        let curfile = results
      endif

      " make sure all new line chars are stripped off the file
      let curfile = substitute(curfile, '\n', '', 'g')

      if curfile != ""
        if a:action == s:find_src
          let matched =  s:FileMatches(
            \ curfile,
            \ substitute(s:package_declared, '<package>', package, 'g'))
          let filtered = s:AppendMatch(filtered, matched)
        else
          let matched = s:FileMatches(
            \ curfile,
            \ substitute(s:package_declared, '<package>', package, 'g'))

          if matched == ""
            let matched = s:FileMatches(
              \ curfile,
              \ substitute(s:package_imported, '<package>', package, 'g'))

            if matched == ""
              let class_imported =
                \ substitute(s:class_imported, '<package>', package, 'g')
              let class_imported =
                \ substitute(class_imported, '<classname>', a:classname, 'g')
              let matched = s:FileMatches(curfile, class_imported)

              if matched ==""
                let fully_qualified =
                  \ substitute(s:fully_qualified, '<package>', package, 'g')
                let fully_qualified =
                  \ substitute(fully_qualified, '<classname>', a:classname, 'g')
                let matched = s:FileMatches(curfile, fully_qualified)
              endif
            endif
          endif

          let filtered = s:AppendMatch(filtered, matched)
        endif
      endif
    endwhile
  endif

  return filtered
endfunction
endif " }}}

" FileMatches(file, regex) {{{
" Checks if the supplied file is matched when the supplied regex is run.
if !exists("*s:FileMatches")
function! s:FileMatches (file, regex)

  let file = substitute(a:file, '\', '/', 'g')
  let index = strridx(a:file, s:path_separator)
  let dir =  strpart(file, 0, index)
  let file = strpart(file, index + 1)

  let find = s:find_cmd
  let find = substitute(find, '<grep>', s:content_grep, 'g')
  let find = substitute(find, '<dir>', dir, 'g')
  let find = substitute(find, '<file>', file, 'g')
  let find = substitute(find, '<regex>', a:regex, 'g')

  " uncomment to debug find commands.
  "echom find

  "return CygwinSystem(find)
  return system(find)
endfunction
endif " }}}

" AppendMatch(results, match) {{{
" Appends the supplied match to the results.
if !exists("*s:AppendMatch")
function! s:AppendMatch (results, match)
  let results = a:results

  if a:match != ""
    if results != ""
      let results = results . s:src_path_results_separator
    endif
    let results = results . a:match
  endif

  return results
endfunction
endif " }}}

" GetPackageDeclaration() {{{
" Gets the package from the package declaration
if !exists("*s:GetPackageDeclaration")
function! s:GetPackageDeclaration ()
  let package = ""

  let line = line(".")
  let col = col(".")

  let packageline = search('^\s*package\s\+', 'w')
  if packageline > 0
    let package = getline(packageline)
    let package = substitute(package, '^\s*package\s\+', '', '')
    let package = substitute(package, ';', '', '')
    let package = substitute(package, '', '', '')
    "let package = substitute(package, '[.]', s:path_separator, 'g')

    call cursor(line, col)
  endif

  return package
endfunction
endif " }}}

" GetPackageImport(classname) {{{
" Gets the package from the import of the classname.
if !exists("*s:GetPackageImport")
function! s:GetPackageImport (classname)
  let import = ""

  let line = line(".")
  let col = col(".")

  let importline = search('^\s*import.*' . a:classname . '\s*;', 'w')
  if importline > 0

    let import = getline(importline)
    let import = substitute(import, '\s*import\s\+', '', '')
    let import = substitute(import, ';', '', '')
    "let import = substitute(import, "[.]", s:path_separator, "g")
    let import = strpart(import, 0, stridx(import, a:classname) - 1)

    call cursor(line, col)
  endif

  return import
endfunction
endif " }}}

" GetPackagePrefix(classname) {{{
" Gets the package prefix if the classname was fully qualified.
"if !exists("*s:GetPackagePrefix")
"function! s:GetPackagePrefix (classname)
"  let package = ""
"
"  let line = getline(".")
"  let col = col(".")
"
"  " get the cursor onto the previous character
"  normal wb
"  let char_pre = strpart(line, col(".") - 2, 1)
"
"  if char_pre == "."
"    let line = strpart(line, 0, stridx(line, a:classname) - 1)
"    let package = substitute(line, '.*\s\+', '', '')
"    "let package = substitute(package, "[.]", s:path_separator, "g")
"  endif
"
"  " return cursor to original position
"  call cursor(line("."), col)
"
"  return package
"endfunction
"endif " }}}

" Execute() {{{
" Executes the action selected.
if !exists("*s:Execute")
function! s:Execute ()
  let action = substitute(g:JavaSrcCmd, "<file>", getline(line(".")), "g")
  silent quit | silent exec action
endfunction
endif " }}}

" LoadArgs() {{{
" Loads all files in the preview window into the arg list.
if !exists("*s:LoadArgs")
function! s:LoadArgs ()
  let lastline = line("$")
  let linenum = 1
  while linenum <= lastline
    let file = getline(linenum)
    if !bufexists(file)
      silent exec "argadd " . file
    endif

    let linenum = linenum + 1
  endwhile

  quit
endfunction
endif " }}}

" DeterminePath(path) {{{
" Performs any convertion necessary to get a compatable
" path for the current OS.
function! s:DeterminePath (path)
  if exists("*CygwinPath")
    return CygwinPath(a:path)
  endif
  return a:path
endfunction
" }}}

" DetermineWinPath(path) {{{
" Performs any convertion necessary to get a compatable
" path for the current OS.
function! s:DetermineWinPath (path)
  if exists("*WindowsPath")
    return WindowsPath(a:path)
  endif
  return a:path
endfunction
" }}}

" Grep(command) {{{
" Executes the supplied grep command.
function! s:Grep (command)
  if exists("*CygwinGrep")
    call CygwinGrep(a:command)
  else
    exec a:command
  endif
endfunction
" }}}

" Command Declarations {{{
if !exists(":FindSrc")
  command -buffer -nargs=? FindSrc :call <SID>FindSrc('<a>')
endif
if !exists(":FindImpl")
  command -buffer -nargs=? FindImpl
    \ :call <SID>FindChildren(s:implements, '<a>')
endif
if !exists(":FindChildren")
  command -buffer -nargs=? FindChildren
    \ :call <SID>FindChildren(s:extends, '<a>')
endif
if !exists(":FindTest")
  command -buffer -nargs=? FindTest :call <SID>FindTest('<a>')
endif
if !exists(":FindArgument")
  command -buffer -nargs=? FindArgument
    \ :call <SID>Find(s:argument_regex, s:find_referenced, '<a>')
endif
if !exists(":FindReturned")
  command -buffer -nargs=? FindReturned
    \ :call <SID>Find(s:returned_regex, s:find_referenced, '<a>')
endif
if !exists(":FindReferenced")
  command -buffer -nargs=? FindReferenced
    \ :call <SID>Find(s:referenced_regex, s:find_referenced, '<a>')
endif
if !exists(":ContextualFindSrc")
  command -buffer ContextualFindSrc :call <SID>ContextualFindSrc()
endif
" }}}

" vim:ft=vim:fdm=marker
