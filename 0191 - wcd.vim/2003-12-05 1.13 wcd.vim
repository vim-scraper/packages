"  wcd.vim: (global plugin) Wherever Change Directory
"  Last Change: December 05, 2003
"  Maintainer:  Pavol Juhas   <juhas@seas.upenn.edu>
"  Version:     1.13
"
"  Usage:  Just drop this file into your plugin directory and make sure
"  	   wcd.exe is installed in your $PATH.  Use :Wcd command to quickly
"  	   access any directory.
"	   If :Wcd does not work right away, look at the Settings section
"	   below; the parameters can be adjusted either in this script or via
"	   global variables in your .vimrc.  If this fails, check the lines
"	   containing 'DEBUG' to see where is the problem
"  Examples:
"	     :Wcd plugin
"	     :Wcd =
"	     :Wcd -g    " works best in console vim
"  Requirements:     wcd
"  Wcd home page:    http://www.xs4all.nl/~waterlan/

" unlet! loaded_wcd	" uncomment for easier DEBUG
if exists("loaded_wcd") || &cp
  finish
endif
let loaded_wcd = 1

" define Wcd command
com! -nargs=+ Wcd call <SID>WcdFun("<args>")
" this abbreviation allows faster typing, but it can be annoying:
" cnoreabbrev wcd Wcd

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Settings:
" Keep variables empty to use the default values

" s:wcd_exe	wcd executable (may need a full path).  Put the following line
" 		into your .vimrc, if you prefer case-insensitive searching:  
" 		    :let wcd_exe = 'wcd.exe -i'
" default:	'wcd.exe'
let s:wcd_exe = exists('wcd_exe') ? wcd_exe : 'wcd.exe'

" s:wcd_go	location of wcd.go script
" default:	'${WCDHOME:-$HOME}/bin/wcd.go' for unix, otherwise,
"		'${WCDHOME:-$HOME}/wcd.go'
let s:wcd_go = exists('wcd_go') ? wcd_go : ''

" s:wcd_filter_cygdrive  if set, change Cygwin-style paths to dos paths
" default:	1 for dos/windows, 0 otherwise
if exists('wcd_filter_cygdrive')
  let s:wcd_filter_cygdrive = wcd_filter_cygdrive
else
  let s:wcd_filter_cygdrive = has("win32") || has("dos32")
endif

" s:wcd_bang	used to call s:wcd_exe
" default:	'silent !' for console or win32 gvim,
"		'!' or 'silent !xterm -e' for unix gvim (depending
"		on s:wcd_use_xterm)
let s:wcd_bang = exists('wcd_bang') ? wcd_bang : ''

" s:wcd_use_xterm  tells unix gui whether to use extra xterm window for
" 		directory listing (wcd -g) or not.
" default:	0  no extra xterm window
let s:wcd_use_xterm = exists('wcd_use_xterm') ? wcd_use_xterm : 0


" End of settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Get the defaults for empty variables

" s:wcd_go
if s:wcd_go == ''
  if exists("$WCDHOME")
    let s:wcd_go = $WCDHOME
  else
    let s:wcd_go = $HOME
  endif
  if has("unix")
    let s:wcd_go = s:wcd_go . '/bin'
  endif
  let s:wcd_go = s:wcd_go . '/wcd.go'
endif

" ensure that wcd.exe writes wcd.go in the right directory
let s:wcd_godir = fnamemodify(s:wcd_go, ":h")
if isdirectory(s:wcd_godir)
  let s:wcd_exe = s:wcd_exe . ' -G ' . s:wcd_godir
endif

" s:wcd_bang
if s:wcd_bang == ''
  if has("gui_running") && has("unix")
    " does user want extra window
    if executable("xterm") == 1 && s:wcd_use_xterm
      let s:wcd_bang = 'silent !xterm -geometry '.&columns.'x'.&lines . ' -e '
    else
      let s:wcd_bang = '!'
      " wcd should use plain stdout for unix gvim
      let s:wcd_exe = s:wcd_exe . ' -o'
    endif
  else
    let s:wcd_bang = 'silent !'
  endif
endif

" now, everything is ready to build the s:wcd_cmd
let s:wcd_cmd = s:wcd_bang . s:wcd_exe
if 0	" change to 1 for DEBUG
  set cmdheight=8	"make room to see the output
  echo s:wcd_exe
  echo 'wcd_go' s:wcd_go
  echo 'wcd_godir' s:wcd_godir
  echo 'wcd_bank' s:wcd_bang
  echo 'wcd_cmd' s:wcd_cmd
endif

" and to define s:WcdFun
function! s:WcdFun(wcdargs)
  exe s:wcd_cmd . ' ' . a:wcdargs
  if v:shell_error
    echo 'Error executing wcd.exe'
    return
  endif
  let save_equalalways = &equalalways
  set noequalalways
  exe "1new " . s:wcd_go
  setlocal buftype=nowrite
  if s:wcd_filter_cygdrive
    silent g/^cd /
      \ s#/cygdrive/\(\a\)#\1:#ei |
      \ s#^cd \(/.*\)#cd c:/cygwin\1#ei
  endif
  let wdir = ''
  silent g/^cd /
    \ s/^cd \a: ; cd/cd/e |
    \ s/"//eg |
    \ let wdir = getline(".")
  bdelete
  redraw!
  if wdir != ''
    let wdir = strpart(wdir, 3)
    exe "cd " . wdir
    echo "-> " . wdir
  else
    echo "Directory not found"
  endif
  let &equalalways = save_equalalways
endfunction

" vim: sw=2
