" ------------------------------------------------------------------------------
" Filename:      ~/.vim/plugin/ShowFunc.vim
" VimScript:     #397
" Last Modified: 13 Mar 2003 07:25:04 PM by Dave V.
" Maintainer:    Dave Vehrs (davev at ziplip.com)
" Copyright:     (C) 2002-2003 Dave Vehrs
"                Distributed under the terms of the GNU General Public License 
" Description:   This script creates a hyper link list of all the functions,
"                subroutines, classes, macros or procedures in a  single file or
"                all currently open windows and displays them in a dynamically
"                sized cwindow.
" History:       This script grew from an idea posted by Flemming Madsen, on
"                vimtip#79.
" WARNING:       It may write the file as a side effect.
" Requires:      Vim 6.0 or newer.
"                Exuberant ctags (http://ctags.sourceforge.net/).
" Install:       Put this file in the vim plugins directory (~/.vim/plugin)
"                to load it automatically, or load it manually with
"                :so ShowFunc.vim.
"                                             Additional notes at end of file...
" ------------------------------------------------------------------------------
" Exit if already loaded.
if ( exists("loaded_showfunc") || &cp ) | finish | endif
let loaded_showfunc=1

" Enable filetype detection
filetype on
" ------------------------------------------------------------------------------
" Default ScanType Options:   buffers  |  Scan all open buffers.
"                             current  |  Scan only the current buffer.
"                             windows  |  Scan all open windows.
if !exists("g:ShowFuncScanType")
  let g:ShowFuncScanType = "buffers"
endif

" Default SortType Options:   yes      |  Display output sorted alphabetically.
"                             no       |  Display output in file order.
"                             foldcase |  Display output sorted alphabetically, 
"                                         disregarding case. 
if !exists("g:ShowFuncSortType")
  let g:ShowFuncSortType = "foldcase"  
endif

" ------------------------------------------------------------------------------
" To change the main key mapping, add this to your .vimrc file:
"   map <key> <PLug>ShowFunc

if !hasmapto('<PLUG>ShowFuncMain') && (maparg('<F1>') == '')
	map  <F1> <Plug>ShowFuncMain
  map! <F1> <Plug>ShowFuncMain
endif
noremap  <silent> <Plug>ShowFuncMain   :call <SID>ShowFuncOpen()<CR>
noremap! <silent> <Plug>ShowFuncMain   <ESC>:call <SID>ShowFuncOpen()<CR>

" Close, refresh, scan-sort and -type key mappings can be found in the OpenCWin
" function (this is to insure that they are associated with only the cwindow).
" ------------------------------------------------------------------------------
function! <SID>ChangeScanType() 
	if g:ShowFuncScanType == "buffers"     | let g:ShowFuncScanType = "windows"
	elseif g:ShowFuncScanType == "windows" | let g:ShowFuncScanType = "current" 
	elseif g:ShowFuncScanType == "current" | let g:ShowFuncScanType = "buffers"
  endif
	call <SID>ShowFuncOpen()
endfunction

function! <SID>ChangeSortType()
	if g:ShowFuncSortType == "no"            | let g:ShowFuncSortType = "yes"
	elseif g:ShowFuncSortType == "yes"       | let g:ShowFuncSortType = "foldcase" 
	elseif g:ShowFuncSortType == "foldcase"  | let g:ShowFuncSortType = "no" 
  endif
	call <SID>ShowFuncOpen()
endfunction

function! <SID>DisplayHelp()
  echo confirm ("ShowFunc Help:          \n".
  \             " c  Close                   \n".
  \             " r  Refresh                 \n". 
  \             " s  Change Scan Sort  \n".
  \             " t  Change Scan Type \n",
  \             "&ok", 0, "Info")
endfunction

function! s:OpenCWin()
  let l:mod_total = 0
  let l:win_count = 1
	windo let l:win_count =  l:win_count + 1
  if ( l:win_count <= 2 ) | let l:win_count = 4 | endif
  windo let l:mod_total = l:mod_total + winheight(0)/l:win_count | 
  \ execute 'resize +'.l:mod_total
  execute 'belowright copen '.l:mod_total   
	let l:cwin_filelen = line("$")
  if ( l:cwin_filelen < winheight(0) )
    cclose
    execute 'belowright copen '.l:cwin_filelen
  endif
  " Set close, refresh, scan-sort and -type mappings
  nnoremap <buffer> <silent> c :cclose<CR>
  nnoremap <buffer> <silent> h :call <SID>DisplayHelp()<CR>
  nnoremap <buffer> <silent> r :call <SID>ShowFuncOpen()<CR>
  nnoremap <buffer> <silent> s :call <SID>ChangeSortType()<CR>
  nnoremap <buffer> <silent> t :call <SID>ChangeScanType()<CR>
  set nobuflisted
  return
endfunction

function! s:SetGrepFormat()
  let l:tags = ''
  let l:tag_return = ''
  let l:tag_count = 1  
  " Define tags for each language.
  if ( &filetype == "asm" )
    let l:tags = 'define label macro type'
  elseif ( &filetype == "asp" )
    let l:tags = 'function sub'
  elseif ( &filetype == "awk" )
    let l:tags = 'function'
  elseif ( &filetype == "beta" )
    let l:tags = 'fragment pattern slot virtual'
  elseif ( &filetype == "c" )
    let l:tags = 'class enumerator enum function macro member namespace'.
    \   ' prototype struct typedef union variable externvar'
  elseif ( &filetype == "cpp" )
    let l:tags = 'class enumerator enum function macro member namespace'.
    \   ' prototype struct typedef union variable externvar'
  elseif ( &filetype == "cobol" )
    let l:tags = 'data file group paragraph program section'
  elseif ( &filetype == "eiffel" )
    let l:tags = 'class feature local'
  elseif ( &filetype == "fortran" )
    let l:tags = 'block common component entry function interface label'.
    \   ' local module namelist program subroutine type variable'
  elseif ( &filetype == "java" )
    let l:tags = 'class field interface method package'
  elseif ( &filetype == "lisp" )
    let l:tags = 'function'
  elseif ( &filetype == "lua" )
    let l:tags = 'function'
	elseif ( &filetype == "make" )
    let l:tags = 'macro'
  elseif ( &filetype == "pascal" )
    let l:tags = 'function procedure'
	elseif ( &filetype == "perl" )
    let l:tags = 'package subroutine'
  elseif ( &filetype == "php" )
    let l:tags = 'class function'
  elseif ( &filetype == "python" )
    let l:tags = 'class function member'
  elseif ( &filetype == "rexx" )
    let l:tags = 'subroutine'
  elseif ( &filetype == "ruby" )
    let l:tags = 'class method mixin singleton'
  elseif ( &filetype == "scheme" )
    let l:tags = 'function set'
  elseif ( &filetype == "sh" )
    let l:tags = 'function'
  elseif ( &filetype == "sql" )
    let l:tags = 'cursor field function local package procedure prototype'.
    \   ' record subtype table trigger variable'
  elseif ( &filetype == "slang" )
    let l:tags = 'function namespace'
  elseif ( &filetype == "tcl" )
    let l:tags = 'class method procedure'
  elseif ( &filetype == "vera" )
    let l:tags = 'class enumerator enum function macro member program'.
    \  ' prototype tas typedef variable externvar'
  elseif ( &filetype == "verilog" )
    let l:tags = 'function module parameter port reg task variable wire'
  elseif ( &filetype == "vim" )
    " I am submitted an extension to exuberant ctags to add augroup to the list
    " of vim types in mid-January, 2003.  It was accepted by Darren Hiebert on
    " January 30 to be included in next release.
    let l:tags = 'augroup function variable'
  elseif ( &filetype == "yacc" )
    let l:tags = 'label'
  else | return "fail" | endif
  " pad end of l:tags with one space.  
  let l:tags = l:tags." "  
  " create grep option list.
  while l:tags != ''
    let l:tagcut = strpart(l:tags,0,stridx(l:tags,' '))
    if ( l:tag_count == 1  && l:tagcut != '' )
      let l:tag_return = '%*\k%*\s'.l:tagcut.'%*\s%l%*\s%f %m'
      let l:tag_count = l:tag_count + 1
    elseif ( l:tag_count > 1 && l:tagcut != '' ) 
      let l:tag_return = l:tag_return.',%*\k%*\s'.l:tagcut.'%*\s%l%*\s%f %m'
    else | break | endif
    let l:tags = strpart(l:tags,stridx(l:tags,' ') + 1)
  endwhile
  return l:tag_return  
endfunction

function! s:SetGrepPrg(sort)
  if ( &filetype == "asm"     ||
     \ &filetype == "asp"     ||
     \ &filetype == "awk"     ||
     \ &filetype == "beta"    ||
     \ &filetype == "c"       ||
     \ &filetype == "cobol"   ||
     \ &filetype == "eiffel"  ||
     \ &filetype == "fortran" ||
     \ &filetype == "java"    ||
     \ &filetype == "lisp"    ||
     \ &filetype == "lua"     ||
     \ &filetype == "make"    ||
     \ &filetype == "pascal"  ||
     \ &filetype == "perl"    ||
     \ &filetype == "php"     ||
     \ &filetype == "python"  ||
     \ &filetype == "rexx"    ||
     \ &filetype == "ruby"    ||
     \ &filetype == "scheme"  ||
     \ &filetype == "sh"      ||
     \ &filetype == "slang"   ||
     \ &filetype == "sql"     ||
     \ &filetype == "tcl"     ||
     \ &filetype == "vera"    ||
     \ &filetype == "verilog" ||
     \ &filetype == "vim"     ||
     \ &filetype == "yacc" )
    return 'ctags -x --language-force='.&filetype.' --sort='.a:sort
  elseif ( &filetype == "cpp"  )
    return 'ctags -x --language-force=c++ --sort='.a:sort
  else | return "fail" | endif
endfunction

function! <SID>ShowFuncOpen()
	set lazyredraw
	cclose
  if ( &lines >= 8 )
		let l:count = 0
    let l:gf_s = &grepformat
    let l:gp_s = &grepprg
    set grepformat&vim
    set grepprg&vim
		if ( g:ShowFuncScanType == "buffers" )
      " Scan all open buffers.
	    let l:currbuf = bufnr("%")
	    bufdo! let &grepformat = s:SetGrepFormat() | let &grepprg =
		  \ s:SetGrepPrg(g:ShowFuncSortType) | if ( &grepformat != "fail" && 
			\ &grepprg != "fail" )| if ( &readonly == 0 ) | update | endif | 
			\ if ( l:count == 0 ) | silent! grep! % | let l:count =  l:count + 1 |
			\ else | silent! grepadd! % | endif | endif
		  execute 'buffer '.l:currbuf
		elseif ( g:ShowFuncScanType == "windows" )
		  " Scan all open windows.
	    windo let &grepformat = s:SetGrepFormat() | let &grepprg =
		  \ s:SetGrepPrg(g:ShowFuncSortType) | if ( &grepformat != "fail" && 
			\ &grepprg != "fail" )| if ( &readonly == 0 ) | update | endif |
			\ if ( l:count == 0 ) | silent! grep! %| let l:count =  l:count + 1 |
			\ else | silent! grepadd! % | endif | endif	
		elseif ( g:ShowFuncScanType == "current" )
		  " Scan current buffer only.
  	  let &grepformat = s:SetGrepFormat()
      let &grepprg = s:SetGrepPrg(g:ShowFuncSortType)
		  if ( &grepformat != "fail" && &grepprg != "fail" )
        if ( &readonly == 0 ) | update | endif
        silent! grep! %
		  else | echomsg "ShowFunc Error: Unknown FileType" | endif
		endif
	  let &grepformat = l:gf_s
    let &grepprg = l:gp_s
		execute s:OpenCWin()
	else  | echomsg "ShowFunc Error: Window too small." | endif
	set nolazyredraw
	redraw!
endfunction
" ------------------------------------------------------------------------------
" Feature Wishlist:
" 1.  Improved Multiple file handling.  I would like to open the files as folds
"     in the cwindow.
" 2.  Multiple tag support.  It would be nice to support all the filetypes that
"     ctags does and to support all the tag types too.  Currently experimenting
"     with vim functions and variables. 
" 3.  If scan is set to "current", make cwindow update on buffer change (except
"     to the cwindow)
" 4.  When ShowFunc opens, the other windows size ratios remain roughly the
"     same, however when you close the cwindow, the other windows all become
"     equal in height (just as if ctrl-W = had been pushed).  They should return
"     to same ratios.
" ------------------------------------------------------------------------------
" Version History:
" 1.0      08-24-2002  Initial Release.
" 1.1      08-26-2002  Patches to Fortran (thanks to Ajit Thakkar), Pascal,
"                      and Python support.
" 1.1.1    08-26-2002  Fixed copy&paste errors.  ooops.
" 1.1.2    08-27-2002  Removed the Python patch.
" 1.1.3    08-31-2002  Fixed Fortran and Pascal patches, Thanks to Ajit Thakkar,
"                      and Engelbert Gruber.
" 1.2      09-22-2002  Fixed redraw bug so that it works with the Winmanager
"                      ndow scan display issue. Improved dynamic 
  "                      cwindow sizing.  Added basic help dialog(vimscript#95) and Bufexplorer (vimscript#42) scripts.
" 1.2.1    10-17-2002  Added unknown filetype handling. Added status messages
"                      ('ShowFunc:').  Fixed key-mappings.
" 1.3Beta  11-16-2002  Beta: Multiple file handling.  Restructured script.
" 1.3Beta2 11-20-2002  Beta: Fixed Multiple file cwindow refresh issue (grep
"                      vs. grepadd).
" 1.3Beta3 11-29-2002  Beta: Split SetFileType into two ( SetGrepFormat, and
"                      SetGrepPrg ). Set &...&vim to  insure proper '\ multiline
"                      translation. Added keymapping testing to  protect against
"                      conflicting with existing user configurations and to make
"                      it easy to remap when necessary. Thanks to Luc Hermitte
" 1.3      12-01-2002  Fixed buffer display issue (Thanks to vimtip#133). Fixed
"                      window height test for TestWinH and OpenCWin.  Changed
"                      MultiWin (scans all open windows) to MultiBuf (scans all
"                      open buffers). Basic multiple file handling is complete.
" 1.4      12-21-2002  Changed user interface. Eliminated multiple key-mappings.
"                      Pressing F1 runs the default scan, and opens the cwindow.
"                      Scan sort and type can be changed by pressing the s and t
"                      keys respectively.  Unifed scan types into one function 
"                      (ShowFuncOpen) and bought back the all open windows scan.
" 1.4.1    01-19-2003  Fixed multi-window scan display issue. Improved dynamic 
"                      cwindow sizing.  Added basic help dialog. 
" 1.4.xxxx xx-xx-xxxx  Rewrote the SetGrepFormat and SetGrepPrg functions. Added
"                      ability to find Vim autocommand groups (augroup).  
" ------------------------------------------------------------------------------
" vim: tw=80 ts=2 sw=2
