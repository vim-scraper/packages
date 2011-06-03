" ------------------------------------------------------------------------------ 
" Filename:      ~/.vim/plugin/ShowFunc.vim
" Version:       1.0 
" Last Modified: 24 Aug 2002 17:45:01 by Dave V.
" Maintainer:    Dave Vehrs (davev at ziplip.com) 
" Install:       Put this file in the vim plugins directory to load it 
"                automatically, or load it with :so ShowFunc.vim 
" Description:   This script creates a hyper link list of all the functions, 
"                subroutines, classes, macros or procedures in a file and 
"                displays them in a dynamically sized cwindow for 19 supported
"                file types. 
" History:       This script grew from an idea posted by Flemming Madsen, on 
"                http://vim.sourceforge.net/tips/tip.php?tip_id=79. 
" Note:          You need Exuberant CTags installed for this to work 
"                (http://ctags.sourceforge.net/). 
" WARNING:       It may write the file as a side effect. 
" Requires:      Vim 6.0 or newer. 
" Supported File types: 
"  For Classes     - Java 
"  For Functions   - Awk, C, C++, Fortran, Lisp, Pascal, PHP, Python, Ruby, 
"                    Shell Scripts, Scheme, Slang, and Vim 
"  For Macros      - Makefiles 
"  For Procedures  - Expect, and Tcl 
"  For Subroutines - Perl and Rexx 
" ------------------------------------------------------------------------------ 

" Exit if already loaded. 
if ( exists("loaded_showfunc") || &cp ) 
  finish 
endif 
let loaded_showfunc=1 
			 
" Enable filetype detection 
filetype on 
			  
" ---------- 
" Mappings 

" List in order they appear in the file. 
noremap <F3>   <ESC>:call ShowFunc("no")<CR><ESC> 
" List in alphabetical order. 
noremap <S-F3> <ESC>:call ShowFunc("yes")<CR><ESC> 
				  
" ---------- 
" Function: ShowFunc 
" Closes existing cwindow, temporarily sets grep format and program 
" variables, greps the file and displays the results in a new cwindow. 
function! ShowFunc(sort)
  let gf_s = &grepformat
  let gp_s = &grepprg
	cclose 
	redraw
  if ( &filetype == "awk" || &filetype == "c" || &filetype == "fortran" ||
		 \ &filetype == "lisp" || &filetype == "pascal" || &filetype == "php" || 
		 \ &filetype == "python" || &filetype == "ruby" || &filetype == "scheme" ||
		 \ &filetype == "sh" || &filetype == "slang" )
    let &grepformat = '%*\k%*\sfunction%*\s%l%*\s%f %m'
    let &grepprg = 'ctags -x --'.&filetype.'-types=f --sort='.a:sort
  elseif ( &filetype == "cpp"  )
    let &grepformat = '%*\k%*\sfunction%*\s%l%*\s%f %m'
    let &grepprg = 'ctags -x --c++-types=f --sort='.a:sort
  elseif ( &filetype == "expect" || &filetype == "tcl" )
    let &grepformat = '%*\k%*\sproc%*\s%l%*\s%f %m'
    let &grepprg = 'ctags -x --language-force=tcl --sort='.a:sort
  elseif ( &filetype == "java" ) 
    let &grepformat = '%*\k%*\sclass%*\s%l%*\s%f %m' 
    let &grepprg = 'ctags -x --java-types=c --sort='.a:sort 
	elseif ( &filetype == "make" )
    let &grepformat= '%*\k%*\smacro%*\s%l%*\s%f %m'
    let &grepprg = 'ctags -x --make-types=m --sort='.a:sort
	elseif ( &filetype == "perl" || &filetype == "rexx" )
    let &grepformat = '%*\k%*\ssubroutine%*\s%l%*\s%f %m'
    let &grepprg = 'ctags -x --'.&filetype.'-types=s --sort='.a:sort
  elseif ( &filetype == "vim" )
    let &grepformat = '%*\k%*\sfunction%*\s%l%*\s%f %m'
    let &grepprg = 'ctags -x --vim-types=f --language-force=vim --sort='.a:sort
  endif
  if ( &readonly == 0 ) | update | endif
  silent! grep %
	if ( winheight(0) < 5 )
		" too small exit
		finish
	elseif ( winheight(0) < 60 )
	   exe 'copen '.winheight(0)/4
	else
		copen 15
	endif
  redraw
	let cwin_filelen = line("$") + 1
	if ( cwin_filelen == 0 )
		copen 1
	elseif ( cwin_filelen < winheight(0) )
		cclose
		redraw
    exe 'copen '.cwin_filelen
	endif 
	redraw
  let &grepformat = gf_s
  let &grepprg = gp_s
endfunc
" ------------------------------------------------------------------------------ 
