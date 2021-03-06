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
"  au FileType java let javadoc_path="/home/etch/Dox/java-1.4-api,/home/etch/Dox/ejb"
"  au FileType java nmap <F2> viw"jy:call OpenJavadoc("j")
"-----------------------------------------------------------------------------
"
" (Note:  is CTRL-V CTRL-R,  is CTRL-V CTRL-M)
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
" Just set the javadoc_path variable to a comma separated list of paths to
" the tops of the Javadoc trees.
"
" CONFIGURATION - KEY AND REGISTER ASSIGNMENT
" The third line uses the <F2> key to start the macro, and the j register to
" act as a temporary visual buffer for getting the word under the cursor into
" the function call. Change these if you use them for something else.
" 
" CAVEAT
" It can be pretty slow when looking for a class name and your javadoc_path
" contains a lot of files.

"amended to work for Win environment"
function! OpenJavadoc(classname)
  call Debug("classname = " . a:classname)
  let line = getline(".")
  call Debug ("line = " . line) 
  let regex = '^import\s\+\(\S\+\);$'
  call Debug ("regex = " . regex)
  let l = matchstr(line, regex)
  call Debug ("l = " . l)
  let file = substitute(l, regex, '\1', '')
  call Debug ("file = " . file) 
  let null = ''

  let file = substitute(file, '\.', '/', 'g')
  call Debug ("file = " . file) 
  let javapath = g:javadoc_path
  let regex = "^[^,]*"
  
  call Debug ("javapath = " . javapath)
 
  if strlen(file) > 0
    while (strlen(javapath))
      let path = GetFirstPathElement(javapath, regex)
      call Debug ("path = " . path)
      let javapath = RemoveFirstPathElement(javapath, regex)
      call Debug ("javapath = " . javapath)
      let lfile = path . "/" . file . ".html"
      call Debug ("lfile = " . lfile)

      if ((match(lfile, "\*\.html$") != -1) && has("gui_running"))
        let lfile = substitute(lfile, "\*\.html$", "", "")
        call Debug ("lfile = " . lfile)
        if (isdirectory(expand(lfile)))
          let null = system(g:browser.' '.lfile.' &')
        endif
      elseif (filereadable(expand(lfile)))
        call Debug ("lfile = " . lfile)
        let null = system('"'.g:browser.'" '.lfile)
        let null = 'found file already'
        break
      endif
    endwhile
  else
    call Debug("file = ''. skipping to look for classname.html")
  endif

  call Debug("null = " . null)
  call Debug("strlen(null) = " . strlen(null) )
  if (strlen(null) == 0)
    call Debug("looking for classname.html")
    " Couldn't find the file directly, so do the equivalent of a system find
    " on each path element and sub-directory.
    
    " Loop through the given path elements
    let javapath = g:javadoc_path
    call Debug("javapath = " . javapath)
    while (strlen(javapath))
      let path = GetFirstPathElement(javapath, regex)
      call Debug("path = " . path)
      call FindTarget(path, a:classname.".html")
      let javapath = RemoveFirstPathElement(javapath, regex)
      call Debug("javapath = " . javapath )
    endwhile
  endif
"   let null = system('"'.g:browser.'" '.'C:/j2sdk1.4.0_01/docs/api/java/lang/System.html' )
  call Debug ("Done")

  return file
endfunction


" Get every file within the path and see if it looks like the target.
" If a directory is found then this function is called recursively.
function! FindTarget(path, target)
"   call Debug("FindTarget+")
  call Debug("looking for " . a:target . " in " . a:path)
  let findlist = substitute(glob(a:path."/*").",", "\n", ",", "g")
  call Debug("findlist = " . findlist )
  let null = ''
  while (strlen(findlist))
    let fpath = GetFirstPathElement(findlist, "[^,]*")
"     call Debug("fpath = " . fpath )
    let findlist = substitute(findlist, "[^,]*,", "", "")
"     call Debug("findlist = " . findlist )
    if (isdirectory(fpath))
       call FindTarget(fpath, a:target)
    else
      if (match(fpath, '\\'.a:target) > -1)
        let null = system('"'.g:browser.'" '.fpath)
        break
      endif
    endif
  endwhile
"   call Debug("FindTarget-")
endfunction

" Return everything up to the first regex in a path
function! GetFirstPathElement(path, regex)
"   call Debug("GetFirstPathElement+")
"   call Debug("a:path = " . a:path) 
"   call Debug("a:regex = " . a:regex) 
  
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
