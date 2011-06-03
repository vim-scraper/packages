"  wcd.vim: (global plugin) Wherever Change Directory
"  Last Change: March 17, 2002
"  Maintainer:  Pavol Juhas   <juhas@seas.upenn.edu>
"  Version:     1.10
"
"  Usage:  if you have wcd installed, just drop this file into your
"	   plugin directory and you should be able to use :Wcd command
"	   similarly as in the shell
"
"	   If wcd does not work right away, try to change the Settings
"	   section.  You can also check the lines containing 'DEBUG' to
"	   see where is the problem
"
"  Requirements:     wcd
"  Wcd home page:    http://www.xs4all.nl/~waterlan/

" unlet loaded_wcd	" uncomment for easier DEBUG
if exists("loaded_wcd") || &cp
  finish
endif
let loaded_wcd = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Settings:
" Keep variables empty to use the default values

" s:wcd_exe	wcd executable (may need a full path)
"		default: 'wcd.exe'
let s:wcd_exe = 'wcd.exe'
" uncomment the following line, if you prefer case-insensitive searching:
" let s:wcd_exe = 'wcd.exe -i'

" s:wcd_go	location of wcd.go script
" default:	'${WCDHOME:-$HOME}/bin/wcd.go' for unix, otherwise,
"		'${WCDHOME:-$HOME}/wcd.go'
let s:wcd_go = ''
" I prefer to have wcd.go in my $HOME:
" let s:wcd_go = '~/wcd.go'

" do we need to filter Cygwin-style paths for a windows gvim?
let s:filter_cygdrive = has("win32") || has("dos32")

" s:wcd_bang	used to call s:wcd_exe
" default:	'silent !' for console or win32 gvim,
"		'!' or 'silent !xterm -e' for unix gvim (dep. on s:use_xterm)
let s:wcd_bang = ''

" s:use_xterm	tells unix gui wether to use extra xterm window for directory
" 		listing (wcd -g) or not.  It also avoids Hit-Return prompt in
" 		gvim. If you find a nicer way of doing that, let me know.
" default:	0  no extra xterm window
let s:use_xterm = 0

" Abbreviate for faster typing; comment out if you don't like it
cnoreabbrev wcd Wcd

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
" expand possible environment variables
let s:wcd_go = expand(s:wcd_go)

" ensure that wcd.exe writes wcd.go in the right directory
let s:wcd_godir = fnamemodify(s:wcd_go, ":h")
if isdirectory(s:wcd_godir)
  let s:wcd_exe = s:wcd_exe . ' -G ' . s:wcd_godir
endif

" s:wcd_bang
if s:wcd_bang == ''
  if has("gui_running") && has("unix")
    " does user want extra window
    if executable("xterm") == 1 && s:use_xterm
      let s:wcd_bang = 'silent !xterm -e '
    else
      let s:wcd_bang = '!'
    endif
  else
    let s:wcd_bang = 'silent !'
  endif
endif

" now, everything is ready to build the s:wcd_cmd
let s:wcd_cmd = s:wcd_bang . s:wcd_exe
if 0	" change to 1 for DEBUG
  " set cmdheight=8	"make room to see the output
  echo s:wcd_exe
  echo s:wcd_go
  echo s:wcd_godir
  echo s:wcd_bang
  echo s:wcd_cmd
endif

" and finally to define the WcdFun
com! -nargs=+ Wcd call WcdFun("<args>")
function! WcdFun(wcdargs)
  exe s:wcd_cmd . ' ' . a:wcdargs
  exe "1new " . s:wcd_go
  if s:filter_cygdrive
    silent g/^cd /s#/cygdrive/\(\a\)#\1:#ei | s#^cd \(/.*\)#cd c:/cygwin\1#ei
  endif
  update
  exe "source " . s:wcd_go
  silent g/^cd /let wdir = getline(".")
  redraw!
  if exists("wdir")
    echo "-> " . strpart(wdir, 3)
  else
    echo "Directory not found"
  endif
  bwipeout
endfunction

" vim: sw=2
