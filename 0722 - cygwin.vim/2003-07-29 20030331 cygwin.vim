" Vim compiler file
" Compiler:	any compiler run from make -- for win32-vim & Cygwin
" Maintainer:	Luc Hermitte <EMAIL:hermitte {at} free {dot} fr>
" URL: http://hermitte.free.fr/vim/ressources/vimfiles/compiler/cygwin.vim
" Last Change:	31st mar 2003
"
" Note: This file is usefull with gcc and other programs run from make, when
" these tools come from Cygwin and the version of Vim used is the win32 native
" version.
" In other environements, Vim default settings are perfect.
"
" Reason: the filenames (for whom gcc reports errors) are expressed in the
" UNIX form, and Vim is unable to open them from the quickfix window. Hence
" the filtering used to replace '/' (root) by {cygpath -m /}.
"
" In order to correctly recognize Cygwin, $TERM or $OSTYPE should value
" "cygwin".
"
" Tested With:	Cygwin + vim-win32 on a MsWindows XP box.

if !has('win32') || !( ($TERM=='cygwin') || ($OSTYPE=='cygwin') )
  echoerr "Cygwin not detected..."
  finish
endif

if     exists('current_compiler')   | let s:cp =   current_compiler
elseif exists('b:current_compiler') | let s:cp = b:current_compiler
else                                | let s:cp = ''
endif

if s:cp != ''
  if b:current_compiler =~ 'cygwin$'
    finish
  else
    let b:current_compiler = s:cp . '_cygwin'
  endif
else
  let b:current_compiler = "make_cygwin"
endif

" a- emplacement of the root path 
" let root = matchstr(system('cygpath -'. (has('win95') ? 'd' : 'w') . ' /'), "^.*\\ze\n") . '/'
let s:root = matchstr(system('cygpath -m /'), "^.*\\ze\n") . '/'
"
" b- filter to apply over `make' outputs: '/' --> {root}
let &l:makeprg = "make $* 2>&1 \\| sed 'sK/.*:[0-9]*:K".s:root."&Kg'"

" c- default value for 'efm'
" setlocal efm&

