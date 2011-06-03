" Title: Side-C (a simple IDE for C/C++)
" Maintainer: Boyko Bantchev <boykobb@gmail.com>
" URI: http://www.math.bas.bg/bantchev/vim/side-c.vim
" Version: 1.1 / 2008 November 12
" Usage: Source this file (:so side-c.vim), or put it in a plugin directory,
"        then use the key mappings and the menu

" ------------------------------------------------------------------------------
" "What"
"
" A set of functions and a menu "C/C++" implementing a simple IDE for working
" with single-file C and C++ programs.  Used as a plug-in.  Makes the basic
" edit-compile-run loop of C and C++ programs easier for Vim users.  Currently
" set up to work with both Linux (gcc) and DOS/Windows (djgpp-gcc); can be
" easily customized for other command-line compilers and/or other o.s.
"
" "Why"
"
" Vim already has the necessary components for integrating compilation and run
" of a program within the editor: calling external programs, reading the error
" messages of a compiler and displaying them in a dedicated window, etc.
" However, some work still needs to be done manually if one is using a plain
" Vim.  Here are several sources of inconvenience in this respect.
"
"   * Depending on the operating system, the file type (C or C++), and one's
"     preferences, there may be a need to call different compilers in each
"     different case, and this is not easy to specify and vary in a typical
"     work session.
"
"   * When the compiler is called, normally through :make, the file to be
"     compiled must be the current one for the editor.
"
"   * The Quickfix window -- the one where the compiler error messages are
"     displayed, must be manually opened and closed.  The best one can do is
"     using :cw[indow] for both, but that is still a manual action.
"
"   * In order to run a program from within Vim, one has not only to type its
"     name as an argument to a :! command; often, the complete path name must
"     be typed, as Vim will only look for the program in its current directory,
"     which may be different from where the program is.  The executable is
"     usually in the same directory as the C/C++ source, but Vim, unless
"     specially instructed to do so, does not make use of this (and it shouldn't
"     -- it is an editor, not a programming system).
"
"   * Sometimes it is useful to redirect the standard input, output or both
"     -- leading to even more manual typing.
"
"   * At times, one may need to browse the directory where a certain edited file
"     resides, or to run an o.s. command shell in that directory.  These too are
"     not directly supported by the plain Vim.
"
" For a more comfortable and productive use of Vim as a programming editor, some
" programming-directed automation is needed, and a simple form of such automation
" is provided by this plug-in.
"
" "Operation"
"
" When loaded, automatically or by `sourcing' it, the script creates several key
" mappings and a menu.  Both implement the same set of actions.  The menu lists
" the mapped keys, along with the names of the respective actions.
"
" As Side-C directly supports only single-file programs, all program-related
" actions -- ones that involve compiling and linking or running a program -- can
" only be executed if there is exactly one C or C++ file on the current tab page
" of the editor.  There may be other, `non-program' files open, or the same file
" can be open in more than one window.
"
" The content of the other tab pages, if any, is irrelevant to Side-C's operation.
" It is possible to have different programs open in different tab pages.  Side-C
" will treat them separately.
"
" It doesn't matter whether the C or C++ file is in the currently active window
" (where the cursor is) or not; it is Side-C's care to find it.
"
" Note that `a program file' means precisely a disk file being edited in Vim,
" not an unnamed buffer.  Moreover, a program file is recognized by its name
" extension.  A file with a .c name extension is regarded as a C file; one with
" .cc, .c++, .cpp, or .cxx is a C++ file (the letter case is ignored).
"
" When compiling and linking, if the current window does not contain the program
" file, the first window that does is made the current window.  Before compiling,
" if the program file has yet unwritten changes, it is saved.  If the compilation
" results in syntax errors, the Quickfix window is open with the error messages
" listed in it.
"
" The "Run" command is executed by first compiling & linking if necessary.
" Building the executable file takes place if such a file does not currently
" exist in the directory of the source file, or if it is older than the source
" (which may be due to the source file being automatically saved, as described
" above).  When compiling fails due to syntax errors in the source file, the
" program is not run.
"
" It is also possible to run a program in several other ways, redirecting as
" follows:
"
"   * only the standard input, to a file named "in";
"
"   * only the standard output, to a file named "out";
"
"   * both the standard input, to a file named "in", and the standard output,
"     to a file named "out";
"
"   * the standard input is redirected to a visual selection within some text,
"     currently open in the editor (no matter which file on the current tab
"     page); the output from the program replaces the selection (this is
"     precisely executing :'<,'>!prog).
"
" In the redirected execution modes, the program is not automatically re-build
" (so do that using the "Compile & Link" command, if needed).  The file "in"
" must reside in the same directory where the executable and its source are.
" If an "out" file is created, it is also written in that directory.
"
" Additionally, there are commands that list the contents of the directory of
" the current file, and run a command shell in that directory.  (In Windows, a
" true console window is opened for the latter action, while in Linux it's a
" pseudo-console within Vim.)  These actions are of general use, not immediately
" related to programming.  They can be executed in any window, including one
" with a directory listing in it.  If the window does not contain a named file,
" Vim's current directory is used as a name.
"
" Note that, when executed in a directory window, the two commands *preserve*
" the listed directory (rather than going to its parent).  Namely, "List ..."
" opens a copy of the same directory, and "Run a Shell" runs in that directory.
" In a directory window, a useful idiom for opening a file is to list the same
" directory by duplicating it in a new window, and then open the file by
" selecting it from the second list.  The selected file replaces the contents
" of the respective window, while the other window still keeps the directory
" listing.
"
" Running a shell in the directory where a certain program is may be used to
" execute the program in ways not represented by the Side-C's commands, such
" as redirecting to files other than `in' and `out', or through a pipe.
" ------------------------------------------------------------------------------

" Allow user to avoid loading this plugin and prevent loading twice
if exists("g:loaded_sidec") || &cp | finish | endif
let g:loaded_sidec = 1

" Change the following compiler settings if necessary

" compiler settings for Linux and other gcc-based systems
let s:ccUnix = 'gcc'          " compiler call name for C
let s:ccppUnix = 'g++'        " compiler call name for C++
let s:coptionsUnix = '-Wall'  " compiler options
let s:cexeUnix = '-o %:r'     " output file name specifier

" compiler settings for djgpp-gcc (Windows or DOS32)
let s:ccWin = 'gcc'           " compiler call name for C
let s:ccppWin = 'gxx'         " compiler call name for C++
let s:coptionsWin = '-Wall'   " compiler options
let s:cexeWin = '-o %:r.exe'  " output file name specifier
" -------------------------------------------------------

set shellslash
set shellpipe=2>&1\ >%s

" O.s. DOS/Windows?
fu! s:isWindows()
  return has('dos16') || has('dos32') || has('win32') || has('win64')
endfu

" Print an error message
fu! s:prErr(msg)
  echohl errormsg | echo a:msg | echohl none
endfu

" The full name of a file or a directory: either it is already one,
" or append getcwd().'/' at its front
fu! s:getFullName(fn)
  return (a:fn[0]=='/' || a:fn[1]==':') ? a:fn : getcwd().'/'.a:fn
endfu

" Find a program on the current tab page.  If sv, position the cursor there, and save if file changed.
" Return a full filename, or '' if no or >1 programs found
fu! s:getFile(sv)
  let l:bufnums = tabpagebuflist()  " all buffer numbers for the windows in the current tab page
  let l:fnu = 0
" check if there is precisely one C (.c) or C++ (.cc .c++ .cpp .cxx) file in the tab page
  for l:i in l:bufnums
    if !isdirectory(bufname(l:i)) && tolower(bufname(l:i)) =~# '\.c\(c\|\(\([+px]\)\3\)\)\?$'
      if l:fnu>0 | call s:prErr('Error: more than one program file open') | return '' | endif
      let l:fnu = l:i
    endif
  endfor
  if l:fnu==0 | call s:prErr('Error: no program file open') | return '' | endif
  if a:sv
" go to a program window if currently not there
    if l:fnu != winbufnr(0)
      let l:fw = bufwinnr(l:fnu)
      while winnr() != l:fw | exe "normal \<C-W>w" | endw
    endif
" save iff the program has unsaved changes
    update
  endif
" return the program's full file name
  return s:getFullName(bufname(l:fnu))
endfu

" Compile and link a program.  Open the Quickfix window if compiler finds any errors.
" When the parameter is empty, call getFile() first, in order to find a program file.
" Return true when compilation is successful, i.e. Quickfix list is empty
fu! s:comlink(fn)
  let l:fn = empty(a:fn) ? s:getFile(1) : a:fn
  if empty(l:fn) | return 0 | endif
  let l:ext = tolower(l:fn[1+strridx(l:fn,'.'):])   " the filename extension
  if s:isWindows()
    let l:cc = l:ext=='c' ? s:ccWin : s:ccppWin
    let l:fe = s:cexeWin
    let l:opts = s:coptionsWin
  else
    let l:cc = l:ext=='c' ? s:ccUnix : s:ccppUnix
    let l:fe = s:cexeUnix
    let l:opts = s:coptionsUnix
  endif
  let l:save_makeprg = &makeprg
  exe 'set makeprg='.l:cc
  let l:fe = substitute(l:fe,'%:r',"'%:r'",'') . ' ' . l:opts . " '%'"
  if s:isWindows() | let l:fe = substitute(l:fe,"'",'"','g') | endif
  exe 'make '.l:fe
  exe 'set makeprg='.escape(l:save_makeprg,' ')
  cwindow
  return empty(getqflist())
endfu

" Run a program, 'building' it first if necessary (exe is old or non-existing).
" In case of compilation errors (see s:comlink above) the program is not run.
" The mode specifies:
"   0 -- build? + run;
"   1 -- run <in;
"   2 -- run >out;
"   3 -- run <in >out;
"   4 -- run on a selection.
" Building and focus transfer across windows only take place in mode==0
fu! s:brun(mode)
" find a program file; possibly move there
  let l:sfn = s:getFile(a:mode==0)
  if empty(l:sfn) | return | endif
" find pathname & exec name
  let l:path = l:sfn[:strridx(l:sfn,'/')]
  let l:efn = l:sfn[:strridx(l:sfn,'.')-1]
  if s:isWindows() | let l:efn .= '.exe' | endif
" compile if a:mode==0 and exe old or non-existing
  if a:mode==0 && getftime(l:sfn)>getftime(l:efn)
    if !s:comlink(l:sfn) | return | endif
  endif
" full filenames for stdin and stdout redirections
  let l:fnin = l:path.'in'
  let l:fnout = l:path.'out'
" check if input available for modes 1 and 3
  if (a:mode == 1 || a:mode == 3) && getftime(l:fnin)<0
    call s:prErr('Error: no file '.l:fnin.' found') | return
  endif
" Note.  Mode 4 also has an input but, rather than being checked, this mode
" is prevented from entering except when there is a selection
  let l:save_shellcmdflag = &shellcmdflag
" adapt for Windows
  if s:isWindows() | set shellcmdflag=/C| endif
" prepare the calling string and `execute' it
  let l:adr = a:mode==4 ? "'<,'>" : ''
  let l:cmd = "!'".l:efn."'"
  let l:cmd .= a:mode==1 ? " <'".l:fnin."'"
           \ : a:mode==2 ? " >'".l:fnout."'"
           \ : a:mode==3 ? " <'".l:fnin."' >'".l:fnout."'"
           \ :             ''
  if s:isWindows() | let l:cmd = substitute(l:cmd,"'",'"','g') | endif
  exe l:adr.l:cmd
  exe 'set shellcmdflag='.escape(l:save_shellcmdflag,' ')
endfu

" Open a window that lists the directory of the file in the current window.
" If the current window is a directory listing, another copy of it is open.
" If the current window hosts an unnamed buffer, Vim's current directory is listed
fu! s:lsDir()
  exe 'vs ' . (empty(@%) ? '.' : escape(expand('%:p:h'),' '))
endfu

" Run a shell in the directory of the current file.
" If the current window hosts an unnamed buffer, Vim's current directory is used
fu! s:runShell()
  let l:fn = empty(@%) ? getcwd() : expand('%:p:h')
  let l:save_shellcmdflag = &shellcmdflag
  if s:isWindows()
    set shellcmdflag=/K
    let l:fn = substitute(l:fn,'/','\','g')
    exe '!"cd '.l:fn.' && '.l:fn[:1].'"'
  else
    set shellcmdflag=-c
    exe "!cd '".l:fn."' && ".&shell
  endif
  exe 'set shellcmdflag='.escape(l:save_shellcmdflag,' ')
endfu

fu! s:srun(from)
  if a:from == 'v' | call s:brun(4)
  else | call s:prErr('Error: no visual selection in the current file') | endif
endfu

noremap <F7> :call <SID>comlink('')<CR>
noremap <F5> :call <SID>brun(0)<CR>
noremap <A-1> :call <SID>brun(1)<CR>
noremap <A-2> :call <SID>brun(2)<CR>
noremap <A-3> :call <SID>brun(3)<CR>
vnoremap <silent> <A-4> :call <SID>srun('v')<CR>
nnoremap <A-4> :call <SID>srun('n')<CR>
onoremap <A-4> :call <SID>srun('n')<CR>
noremap <C-D> :call <SID>lsDir()<CR>
noremap <C-S> :call <SID>runShell()<CR>

anoremenu 100.10 &C/C++.Compile\ and\ Link<Tab>F7 :call <SID>comlink('')<CR>
anoremenu 100.11 &C/C++.-sep1- :
anoremenu 100.20 &C/C++.(Build\ and)\ Run<Tab>F5 :call <SID>brun(0)<CR>
anoremenu 100.21 &C/C++.-sep2- :
anoremenu 100.30 &C/C++.Run\ with\ <in<Tab>Alt-1 :call <SID>brun(1)<CR>
anoremenu 100.40 &C/C++.Run\ with\ >out<Tab>Alt-2 :call <SID>brun(2)<CR>
anoremenu 100.50 &C/C++.Run\ with\ <in\ >out<Tab>Alt-3 :call <SID>brun(3)<CR>
vnoremenu <silent> 100.60 &C/C++.Run\ on\ a\ Selection<Tab>Alt-4 :call <SID>brun(4)<CR>
anoremenu 100.61 &C/C++.-sep3- :
anoremenu 100.70 &C/C++.List\ a\ Directory<Tab>Ctrl-D :call <SID>lsDir()<CR>
anoremenu 100.71 &C/C++.-sep4- :
anoremenu 100.80 &C/C++.Run\ a\ Shell<Tab>Ctrl-S :call <SID>runShell()<CR>
