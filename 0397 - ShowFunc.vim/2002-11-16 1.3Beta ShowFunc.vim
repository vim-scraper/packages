" ------------------------------------------------------------------------------ 
" Filename:      ~/.vim/plugin/ShowFunc.vim
" Last Modified: 16 Nov 2002 12:45:49 PM by Dave V.
" Maintainer:    Dave Vehrs (davev at ziplip.com) 
" Install:       Put this file in the vim plugins directory to load it 
"                automatically, or load it with :so ShowFunc.vim 
" Description:   This script creates a hyper link list of all the functions, 
"                subroutines, classes, macros or procedures in a file and 
"                displays them in a dynamically sized cwindow for 19 supported
"                file types. 
" History:       This script grew from an idea posted by Flemming Madsen, on 
"                http://vim.sourceforge.net/tips/tip.php?tip_id=79. 
" WARNING:       It may write the file as a side effect. 
" Requires:      Vim 6.0 or newer. 
"                This script requires exuberant ctags to work.   
"                (http://ctags.sourceforge.net/). 
" Supported File Types: 
"  For Classes     - Java 
"  For Functions   - Awk, C, C++, Fortran, Lisp, Pascal, PHP, Python, Ruby, 
"                    Shell Scripts, Scheme, Slang, and Vim 
"  For Macros      - Makefiles 
"  For Procedures  - Expect, Pascal, and Tcl 
"  For Subroutines - Fortran, Perl and Rexx 
" ------------------------------------------------------------------------------ 

" Exit if already loaded. 
if ( exists("loaded_showfunc") || &cp ) 
  finish 
endif 
let loaded_showfunc=1 
			 
" Enable filetype detection 
filetype on
 			  
" ------------------------------------------------------------------------------ 

" Single File Mappings 
" List in file order.
map  <F1>   :call SingleWin("no")<CR>
map! <F1>   <ESC>:call SingleWin("no")<CR>
" List in alphabetical order. 
map  <F2> :call SingleWin("yes")<CR>
map! <F2> <ESC>:call SingleWin("yes")<CR>
				  
" Multiple File Mappings 
" List in file order.
map  <F3>   :call MultiWin("no")<CR>
map! <F3>   <ESC>:call MultiWin("no")<CR>
" List in alphabetical order. 
map  <F4> :call MultiWin("yes")<CR>
map! <F4> <ESC>:call MultiWin("yes")<CR>
				  
" ------------------------------------------------------------------------------ 
" Functions

" Function: MultiWin
" Run ShowFunc on mutiple windows at the same time.
function! MultiWin(sort)
  let WinTest = TestWinH()
  if WinTest == "ok" 
    let gf_s = &grepformat
    let gp_s = &grepprg
	  set lazyredraw
	  cclose
	  windo! call ShowFunc(a:sort) | if ( &readonly == 0 ) | update | endif | 
		  \ silent! grepa %
	  let &grepformat = gf_s
    let &grepprg = gp_s
		execute OpenCwin()
	  set nolazyredraw
	  redraw!
	endif
endfunction

" Function: OpenCwin
" Determines correct height for window and opens it.
function! OpenCwin()
	  if ( winheight(0) < 60 )
	    exe 'belowright copen '.winheight(0)/4
	  else 
		  belowright copen 15
	  endif
	  let cwin_filelen = line("$")
	  if ( cwin_filelen == 0 )
		  echomsg "ShowFunc: no tags found."
		  return 
	  elseif ( cwin_filelen < winheight(0) )
	    cclose
	    exe 'belowright copen '.cwin_filelen
	  endif 
		return 
endfunction

" Function: ShowFunc 
" Closes existing cwindow, temporarily sets grep format and program 
" variables, greps the file and displays the results in a new cwindow. 
function! ShowFunc(sort)
  if ( &filetype == "awk" || &filetype == "c" || &filetype == "lisp" || 
		 \ &filetype == "php" || &filetype == "python" || &filetype == "ruby" ||
		 \ &filetype == "scheme" || &filetype == "sh" ||  &filetype == "slang" )
    let &grepformat = '%*\k%*\sfunction%*\s%l%*\s%f %m'
    let &grepprg = 'ctags -x --'.&filetype.'-types=f --sort='.a:sort
  elseif ( &filetype == "cpp"  )
    let &grepformat = '%*\k%*\sfunction%*\s%l%*\s%f %m'
    let &grepprg = 'ctags -x --c++-types=f --sort='.a:sort
  elseif ( &filetype == "expect" || &filetype == "tcl" )
    let &grepformat = '%*\k%*\sproc%*\s%l%*\s%f %m'
    let &grepprg = 'ctags -x --language-force=tcl --sort='.a:sort
  elseif ( &filetype == "fortran"  )
    let &grepformat = '%*\k%*\sfunction%*\s%l%*\s%f %m,'.
		                \ '%*\k%*\ssubroutine%*\s%l%*\s%f %m'
    let &grepprg = 'ctags -x --'.&filetype.'-types=fs --sort='.a:sort
  elseif ( &filetype == "java" ) 
    let &grepformat = '%*\k%*\sclass%*\s%l%*\s%f %m' 
    let &grepprg = 'ctags -x --java-types=c --sort='.a:sort 
	elseif ( &filetype == "make" )
    let &grepformat= '%*\k%*\smacro%*\s%l%*\s%f %m'
    let &grepprg = 'ctags -x --make-types=m --sort='.a:sort
  elseif ( &filetype == "pascal"  )
    let &grepformat = '%*\k%*\sfunction%*\s%l%*\s%f %m,'.
		                \ '%*\k%*\sprocedure%*\s%l%*\s%f %m'
    let &grepprg = 'ctags -x --'.&filetype.'-types=fp --sort='.a:sort
	elseif ( &filetype == "perl" || &filetype == "rexx" )
    let &grepformat = '%*\k%*\ssubroutine%*\s%l%*\s%f %m'
    let &grepprg = 'ctags -x --'.&filetype.'-types=s --sort='.a:sort
  elseif ( &filetype == "vim" )
    let &grepformat = '%*\k%*\sfunction%*\s%l%*\s%f %m,'.
                    \ '%*\k%*\svariable%*\s%l%*\s%f %m'
    let &grepprg = 'ctags -x --language-force=vim --sort='.a:sort
  else 
    let &grepformat = gf_s
    let &grepprg = gp_s
	  set nolazyredraw
	  redraw!
		echomsg "ShowFunc Error: Unknown Filetype"
	  return
	endif
endfunction

" Function: SingleWin
" Run ShowFunc on mutiple windows at the same time.
function! SingleWin(sort)
  let WinTest = TestWinH()
  if ( WinTest == "ok" )
		let gf_s = &grepformat
		let gp_s = &grepprg
		set lazyredraw
		cclose
		call ShowFunc(a:sort)
    if ( &readonly == 0 ) | update | endif
    silent! grep %
	  let &grepformat = gf_s
    let &grepprg = gp_s
		execute OpenCwin()
	  set nolazyredraw
	  redraw!
	endif
endfunction

" Function TestWinH
" Tests window height to see if script should run
function! TestWinH()
	if ( winheight(0) < 8 )
		echomsg "ShowFunc Error: Window too small, canceling"
		return "fail"
	endif
	return "ok"
endfunction

" ------------------------------------------------------------------------------
" Version History
" 1.0     08-24-2002  Initial Release.
" 1.1     08-26-2002  Patches to Fortran (thanks to Ajit Thakkar), Pascal,
"                     and Python support.
" 1.1.1   08-26-2002  Fixed copy&paste errors.  ooops.
" 1.1.2   08-27-2002  Removed the Python patch.
" 1.1.3   08-31-2002  Fixed Fortran and Pascal patches, Thanks to Ajit Thakkar,
"                     and Engelbert Gruber.
" 1.2     09-22-2002  Fixed redraw bug so that it works with the Winmanager
"                     (vimscript#95) and Bufexplorer (vimscript#42) scripts.
" 1.2.1   10-17-2002  Added unknown filetype handling. Added status messages 
"                     ('ShowFunc:').  Fixed key-mappings.
" 1.3beta 11-16-2002  Beta multiple file handling.  Restructured script.
" ------------------------------------------------------------------------------
" Feature Wishlist
" 1.  Improved Multiple file handling.  I would like to open multiple files as folds in
"     the cwindow.
" 2.  Multiple tag support.  It would be nice to support all the filetypes that 
"     ctags does and to support all the tag types too.  
" 3.
" ------------------------------------------------------------------------------
