" ------------------------------------------------------------------------------ 
" Filename:      ~/.vim/plugin/ShowFunc.vim
" Last Modified: 17 Oct 2002 06:19:52 PM by Dave V.
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
 			  
" ---------- 
" Mappings 

" List in file order.
map  <F3>   :call ShowFunc("no")<CR>
map! <F3>   <ESC>:call ShowFunc("no")<CR>
" List in alphabetical order. 
map  <F4> :call ShowFunc("yes")<CR>
map! <F4> <ESC>:call ShowFunc("yes")<CR>
				  
" ---------- 
" Function: ShowFunc 
" Closes existing cwindow, temporarily sets grep format and program 
" variables, greps the file and displays the results in a new cwindow. 
function! ShowFunc(sort)
  let gf_s = &grepformat
  let gp_s = &grepprg
	set lazyredraw
	cclose
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
    let &grepformat = '%*\k%*\sfunction%*\s%l%*\s%f %m'
    let &grepprg = 'ctags -x --vim-types=f --language-force=vim --sort='.a:sort
  else 
    let &grepformat = gf_s
    let &grepprg = gp_s
	  set nolazyredraw
	  redraw!
		echomsg "ShowFunc Error: Unknown Filetype"
	  return
	endif
  if ( &readonly == 0 ) | update | endif
  silent! grep %
  let &grepformat = gf_s
  let &grepprg = gp_s
	if ( winheight(0) < 5 )
	  set nolazyredraw
	  redraw!
		echomsg "ShowFunc Error: Window too small, canceling"
		return
	elseif ( winheight(0) < 60 )
	  exe 'belowright copen '.winheight(0)/4
	else 
		belowright copen 15
	endif
	let cwin_filelen = line("$")
	if ( cwin_filelen == 0 )
		set nolazyredraw
		redraw!
		echomsg "ShowFunc: no tags found."
		return
	elseif ( cwin_filelen < winheight(0) )
	  cclose
	  exe 'belowright copen '.cwin_filelen
	endif 
	set nolazyredraw
	redraw!
	return
endfunction

" ------------------------------------------------------------------------------
" Version History
" ------------------------------------------------------------------------------
" 1.0   08-24-2002  Initial Release.
" 1.1   08-26-2002  Patches to Fortran (thanks to Ajit Thakkar), Pascal,
"                   and Python support.
" 1.1.1 08-26-2002  Fixed copy&paste errors.  ooops.
" 1.1.2 08-27-2002  Removed the Python patch.
" 1.1.3 08-31-2002  Fixed Fortran and Pascal patches, Thanks to Ajit Thakkar,
"                   and Engelbert Gruber.
" 1.2   09-22-2002  Fixed redraw bug so that it works with the Winmanager
"                   (vimscript#95) and Bufexplorer (vimscript#42) scripts.
" 1.2.1 10-17-2002  Added unknown filetype handling. Added status messages 
"                   ('ShowFunc:').  Fixed key-mappings.
" ------------------------------------------------------------------------------
" Feature Wishlist
" ------------------------------------------------------------------------------
" 1.  Multiple file handling.  I would like to open multiple files as folds in
"     the cwindow.  Current file expanded by default.  Needs to autoupdate as
"     new files are loaded or active window changes.
" 2.
" 3.
" ------------------------------------------------------------------------------
