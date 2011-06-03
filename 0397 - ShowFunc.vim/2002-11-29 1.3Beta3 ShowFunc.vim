" ------------------------------------------------------------------------------ 
" Filename:      ~/.vim/plugin/ShowFun
" Last Modified: 29 Nov 2002 01:08:01 PM by Dave V.
" Maintainer:    Dave Vehrs (davev at ziplip.com) 
" Install:       Put this file in the vim plugins directory (~/.vim/plugin) 
"                to load it automatically, or load it manually with 
"                :so ShowFunc.vim. 
" Description:   This script creates a hyper link list of all the functions, 
"                subroutines, classes, macros or procedures in a  single file or
"                all currently open windows and displays them in a dynamically 
"                sized cwindow. 
" History:       This script grew from an idea posted by Flemming Madsen, on 
"                http://vim.sourceforge.net/tips/tip.php?tip_id=79. 
" WARNING:       It may write the file as a side effect. 
" Requires:      Vim 6.0 or newer. 
"                Exuberant ctags (http://ctags.sourceforge.net/). 
" Supported File Types: 
"  For Classes     - Java 
"  For Functions   - Awk, C, C++, Fortran, Lisp, Pascal, PHP, Python, Ruby, 
"                    Shell Scripts, Scheme, Slang, and Vim 
"  For Macros      - Makefiles 
"  For Procedures  - Expect, Pascal, and Tcl 
"  For Subroutines - Fortran, Perl and Rexx 
"                                             Additional notes at end of file...
" ------------------------------------------------------------------------------ 

" Exit if already loaded. 
if ( exists("loaded_showfunc") || &cp ) 
  finish 
endif 
let loaded_showfunc=1 
			 
" Enable filetype detection 
filetype on
 			  
" ------------------------------------------------------------------------------ 
" Key Mappings

" Single Window Mappings
" List in file order.
if !hasmapto('<PLUG>SWinDontSort') && (maparg('<F1>') == '')
	map  <F1> <Plug>SWinDontSort
	map! <F1> <Plug>SWinDontSort
endif   
noremap  <Plug>SWinDontSort   :call SingleWin("no")<CR>
noremap! <Plug>SWinDontSort   <ESC>:call SingleWin("no")<CR>
" List in alphabetical order. 
if !hasmapto('<PLUG>SWinSort') && (maparg('<F2>') == '')
	map  <F2> <Plug>SWinSort
	map! <F2> <Plug>SWinSort
endif   
noremap  <Plug>SWinSort   :call SingleWin("yes")<CR>
noremap! <Plug>SWinSort   <ESC>:call SingleWin("yes")<CR>
				  
" Multiple File Mappings 
" List in file order.
if !hasmapto('<PLUG>MWinDontSort') && (maparg('<F3>') == '')
	map  <F3> <Plug>MWinDontSort
	map! <F3> <Plug>MWinDontSort
endif   
noremap  <Plug>MWinDontSort   :call MultiWin("no")<CR>
noremap! <Plug>MWinDontSort   <ESC>:call MultiWin("no")<CR>
" List in alphabetical order. 
if !hasmapto('<PLUG>MWinSort') && (maparg('<F4>') == '')
	map  <F4> <Plug>MWinSort
	map! <F4> <Plug>MWinSort
endif   
noremap  <Plug>MWinSort   :call MultiWin("yes")<CR>
noremap! <Plug>MWinSort   <ESC>:call MultiWin("yes")<CR>
				  
" ------------------------------------------------------------------------------ 
" Functions

" Function: MultiWin
" Run against all open windows at the same time.
function! MultiWin(sort)
	set lazyredraw
	cclose
  let WinTest = s:TestWinH()
  if WinTest == "ok" 
    let gf_s = &grepformat
    let gp_s = &grepprg
    set grepformat&vim
    set grepprg&vim
		let s:count = 0
	  windo! let &grepformat = s:SetGrepFormat() | let &grepprg =
		  \ s:SetGrepPrg(a:sort) | if ( &grepformat != "fail" && &grepprg != 
			\ "fail" )| if ( &readonly == 0 ) | update | endif | if ( s:count == 0 ) |
			\ silent! grep % | let s:count =  s:count + 1 | else | silent! grepa % | 
			\ endif | endif
	  let &grepformat = gf_s
    let &grepprg = gp_s
		execute s:OpenCwin()
	endif
	set nolazyredraw
	redraw!
endfunction

" Function: OpenCwin
" Determines correct height for the cwindow and opens it.
function! s:OpenCwin()
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

" Function: SetGrepFormat 
" Sets grepformat based on filetype. 
function! s:SetGrepFormat()
  if ( &filetype == "awk" || &filetype == "c" || &filetype == "lisp" || 
		 \ &filetype == "php" || &filetype == "python" || &filetype == "ruby" ||
		 \ &filetype == "scheme" || &filetype == "sh" ||  &filetype == "slang" )
    return '%*\k%*\sfunction%*\s%l%*\s%f %m'
  elseif ( &filetype == "cpp"  )
    return '%*\k%*\sfunction%*\s%l%*\s%f %m'
  elseif ( &filetype == "expect" || &filetype == "tcl" )
    return '%*\k%*\sproc%*\s%l%*\s%f %m'
  elseif ( &filetype == "fortran"  )
    return '%*\k%*\sfunction%*\s%l%*\s%f %m,'.
		     \ '%*\k%*\ssubroutine%*\s%l%*\s%f %m'
  elseif ( &filetype == "java" ) 
    return '%*\k%*\sclass%*\s%l%*\s%f %m' 
	elseif ( &filetype == "make" )
    return '%*\k%*\smacro%*\s%l%*\s%f %m'
  elseif ( &filetype == "pascal"  )
    return '%*\k%*\sfunction%*\s%l%*\s%f %m,'.
		     \ '%*\k%*\sprocedure%*\s%l%*\s%f %m'
	elseif ( &filetype == "perl" || &filetype == "rexx" )
    return '%*\k%*\ssubroutine%*\s%l%*\s%f %m'
  elseif ( &filetype == "vim" )
    return '%*\k%*\sfunction%*\s%l%*\s%f %m,'.
         \ '%*\k%*\svariable%*\s%l%*\s%f %m'
  else 
	  return "fail"
	endif
endfunction

" Function: SetGrepPrg 
" Sets grepprg based on filetype. 
function! s:SetGrepPrg(sort)
  if ( &filetype == "awk" || &filetype == "c" || &filetype == "lisp" || 
		 \ &filetype == "php" || &filetype == "python" || &filetype == "ruby" ||
		 \ &filetype == "scheme" || &filetype == "sh" ||  &filetype == "slang" )
    return 'ctags -x --'.&filetype.'-types=f --sort='.a:sort
  elseif ( &filetype == "cpp"  )
    return 'ctags -x --c++-types=f --sort='.a:sort
  elseif ( &filetype == "expect" || &filetype == "tcl" )
    return 'ctags -x --language-force=tcl --sort='.a:sort
  elseif ( &filetype == "fortran"  )
    return 'ctags -x --'.&filetype.'-types=fs --sort='.a:sort
  elseif ( &filetype == "java" ) 
    return 'ctags -x --java-types=c --sort='.a:sort 
	elseif ( &filetype == "make" )
    return 'ctags -x --make-types=m --sort='.a:sort
  elseif ( &filetype == "pascal"  )
    return 'ctags -x --'.&filetype.'-types=fp --sort='.a:sort
	elseif ( &filetype == "perl" || &filetype == "rexx" )
    return 'ctags -x --'.&filetype.'-types=s --sort='.a:sort
  elseif ( &filetype == "vim" )
    return 'ctags -x --language-force=vim --sort='.a:sort
  else 
	  return "fail"
	endif
endfunction


" Function: SingleWin
" Run on the current window only.
function! SingleWin(sort)
	set lazyredraw
	cclose
  let WinTest = s:TestWinH()
  if ( WinTest == "ok" )
		let gf_s = &grepformat
		let gp_s = &grepprg
    set grepformat&vim
    set grepprg&vim
	  let &grepformat = s:SetGrepFormat()
    let &grepprg = s:SetGrepPrg(a:sort)
		if ( &grepformat != "fail" && &grepprg != "fail" )
      if ( &readonly == 0 ) | update | endif
      silent! grep %
		  execute s:OpenCwin()
		else	
			echomsg "ShowFunc Error: Unknown FileType"
		endif 	
	  let &grepformat = gf_s
    let &grepprg = gp_s
	endif
	set nolazyredraw
	redraw!
endfunction

" Function TestWinH
" Tests window height to see if script should run
function! s:TestWinH()
	if ( line("$") < 8 )
		echomsg "ShowFunc Error: Window too small, canceling"
		return "fail"
	endif
	return "ok"
endfunction

" ------------------------------------------------------------------------------
" Default Key Mappings
" F1  =  Scan current window and display results in file order 
"        ( SWinDontSort ). 
" F2  =  Scan current window and display results in alphabetical order 
"        ( SWinSort ).
" F3  =  Scan all open windows and display results in file order.
"        ( MWinDontSort ).
" F4  =  Scan all open windows and display results in alphabetical order.
"        ( MWinSort ).
" To change the F1 mappings, add this to your .vimrc file
"     map \NewKey   <Plug>SWinDontSort
"     map \NewKey   <Plug>SWinDontSort
" ------------------------------------------------------------------------------
" Known Issues
" 1.  TestWinH and OpenCWin use a flawed method of determining window height, 
"     need to find a way to use the output of 'set lines?' or something similar.
" 2.  If cursor is in second or third window when MultiWin is called then
"     incomplete results are displayed.
" ------------------------------------------------------------------------------
" Feature Wishlist
" 1.  Improved Multiple file handling.  I would like to open the files as folds
"     in the cwindow.
" 2.  Multiple tag support.  It would be nice to support all the filetypes that 
"     ctags does and to support all the tag types too.  
" 3.  Key mappings are out of control.  4 for one simple script is too many,
"     need to determine a better way of doing it.
" ------------------------------------------------------------------------------
" Version History
" 1.0      08-24-2002  Initial Release.
" 1.1      08-26-2002  Patches to Fortran (thanks to Ajit Thakkar), Pascal,
"                      and Python support.
" 1.1.1    08-26-2002  Fixed copy&paste errors.  ooops.
" 1.1.2    08-27-2002  Removed the Python patch.
" 1.1.3    08-31-2002  Fixed Fortran and Pascal patches, Thanks to Ajit Thakkar,
"                      and Engelbert Gruber.
" 1.2      09-22-2002  Fixed redraw bug so that it works with the Winmanager
"                      (vimscript#95) and Bufexplorer (vimscript#42) scripts.
" 1.2.1    10-17-2002  Added unknown filetype handling. Added status messages 
"                      ('ShowFunc:').  Fixed key-mappings.
" 1.3Beta  11-16-2002  Beta: Multiple file handling.  Restructured script.
" 1.3Beta2 11-20-2002  Beta: Fixed Multiple file cwindow refresh issue (grep 
"                      vs. grepa) and general clean up.
" 1.3Beta3 11-29-2002  Beta: Split SetFileType into two ( SetGrepFormat, and 
"                      SetGrepPrg ). Set &...&vim to  insure proper '\ multiline 
"                      translation. Added keymapping testing to  protect against
"                      conflicting with existing user configurations and to make
"                      it easy to remap when necessary. Thanks to Luc Hermitte 
" ------------------------------------------------------------------------------
