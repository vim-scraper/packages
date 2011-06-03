" idutils.vim -- Interface with id-utils.
" Author: Hari Krishna <hari_vim at yahoo.com>
" Last Change: 28-Aug-2002 @ 12:57
" Created:     not sure, but sometime before 08-Mar-2001
" Requires: Vim-6.0 or higher.
" Version: 1.3.0
" Download From:
"     http://vim.sourceforge.net/scripts/script.php?script_id=113
"
" Installation:
"   Drop the file in your plugin directory or source it from your vimrc.
"
" Summary Of Features:
"   Command Line:
"       IGInitialize, IDGrep, IDGrepAdd, IDGrepReset
"
"   Settings:
"       g:IGlidcmd, g:IGfiltercmd, g:IGautoCopen
"
" Help:
"	IDGrep    <keyword>
"	IDGrep    [<lid argument> ...] <lid keyword pattern> 
"	IDGrep    <lid arguments> +f <filter arguments>
"	IDGrepAdd <keyword>
"	IDGrepAdd [<lid argument> ...] <lid keyword pattern> 
"	IDGrepAdd <lid arguments> +f <filter arguments>
"
"   You can use all the regular quickfix commands to traverse from one hit to
"     another.
"
"	:h quickfix 
"
"   This script at the start of the execution changes the grepprg and
"     shellpipe settings, and resets them when done. But if you happen to press
"     <Ctrl-C> in the middle, then you need to manually reset them by using
"     the command,
"
"	IDGrepReset 
"
"   Each time you run IDGrepAdd, the new list of matches are added to the
"     existing list. This works the same as 'grepadd' command. If you can't
"     use a single filter to get all the matches then this command helps you to
"     still work with a single list instead of generating multiple lists.
"
"   Use the g:IGlidcmd, g:IGfiltercmd, g:IGautoCopen global variables to set
"     the path to 'lid' command, filter command name/path (defaults to 'grep')
"     and if the error list window should automatically be opened.
"
" Examples: 
"   - Filter the lines that don't contain src.
"	IDGrep main +f src
"
"   - Filter the lines that contain src.
"	IDGrep main +f -v src
"
"   - To search for the current word in all the files and filter the results
"     not containing \.java in the grepped output. This will potentially
"     return all the occurences in the java files only. 
"	IDGrep <cword> +f \.java 
"
"   - If any argument contains spaces, then you need to protect them by
"     prefixing them with a backslash.  The following will filter those lines
"     that don't contain "ABC XYZ".
"	IDGrep <cword> +f ABC\ XYZ 

if exists("loaded_idutils")
  finish
endif
let loaded_idutils=1


command! -nargs=0 IGInitialize :call <SID>Initialize()

function! s:Initialize()

if exists("g:IGlidcmd")
  let s:lidcmd = g:IGlidcmd
  unlet g:IGlidcmd
elseif !exists("s:lidcmd")
  let s:lidcmd = "lid -R grep"
endif

if exists("g:IGfiltercmd")
  let s:filtercmd = g:IGfiltercmd
  unlet g:IGfiltercmd
elseif !exists("s:filtercmd")
  let s:filtercmd = "grep"
endif

if exists("g:IGautoCopen")
  let s:autoCopen = g:IGautoCopen
  unlet g:IGautoCopen
elseif !exists("s:autoCopen")
  let s:autoCopen = 1
endif

" Add the "lid -R grep" format to grep formats.
set gfm+="%f:%l:%m"

command! -nargs=+ -complete=tag IDGrep call <SID>IDGrep(0, <f-args>)
command! -nargs=+ -complete=tag IDGrepAdd call <SID>IDGrep(1, <f-args>)
command! -nargs=0 IDGrepReset call <SID>IDGrepReset(1)

let s:savedGrepprg = ''
let s:savedShellpipe = ''

endfunction " s:Initialize

call s:Initialize()

" Pass an optional filter pattern as a second argument.
function! s:IDGrep(grepAdd, ...)
  if a:0 == 0
    echohl ERROR | echo "Missing arguments." | echohl None
    return
  endif

  let lidArgs = ''
  let filterArgs = ''
  let argIsForLid = 1
  let arg = 0
  while arg < a:0
    let arg = arg + 1
    if argIsForLid && a:{arg} == '+f'
      let argIsForLid = 0
      continue
    endif
    if argIsForLid
      let lidArgs = lidArgs . ' ' . escape(a:{arg}, ' ')
    else
      let filterArgs = filterArgs . ' ' . escape(a:{arg}, ' ')
    endif
  endwhile

  "  We need to check for non-null string because Vim passes a null string if
  "    this is called from a command.
  call s:IDGrepSet(filterArgs)
  if a:grepAdd == 1
    exec "grepadd " . lidArgs
  else
    exec "grep " . lidArgs
  endif
  if s:autoCopen
    copen
  endif
  call s:IDGrepReset(0)
endfunction

" You can pass an optional filter.
function! s:IDGrepSet(filterArgs)
  " So that they can be restored later.
  if s:savedGrepprg != &grepprg
    let s:savedGrepprg = &grepprg
  endif
  if s:savedShellpipe != &shellpipe
    let s:savedShellpipe = &shellpipe
  endif

  let &grepprg=s:lidcmd
  if a:filterArgs != ""
    let &shellpipe =  '| ' . s:filtercmd . ' ' . a:filterArgs
  else
    let &shellpipe=''
  endif
  let &shellpipe= &shellpipe . '>'
endfunction

" You call this method separately in case you need to reverse what IDGrep
"   command did (if you Ctrol-Ced the execution, eg.).
function! s:IDGrepReset(interactive)
  if exists("s:savedGrepprg")
    let &grepprg=s:savedGrepprg
    "unlet s:savedGrepprg
  endif
  if exists("s:savedShellpipe")
    let &shellpipe=s:savedShellpipe
    "unlet s:savedShellpipe
  endif
  if (a:interactive)
    echo "Done resetting IDGrep settings."
  endif
endfunction
