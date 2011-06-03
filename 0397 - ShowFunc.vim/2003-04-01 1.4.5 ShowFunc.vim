" ------------------------------------------------------------------------------
" Filename:      ~/.vim/plugin/ShowFunc.vim
" VimScript:     #397
" Last Modified: 01 Apr 2003 14:33:38 by davev
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
let g:loaded_showfunc=1 
			 
" Enable filetype detection 
filetype on
" ------------------------------------------------------------------------------
" Test for and if necessary configure all default settings.  If you would like
" to change any setting, just add let g:variablename = "new-value" to your
" .vimrc.
"
" Default ScanType Options:   buffers  |  Scan all open buffers.
"                             current  |  Scan only the current buffer.
"                             windows  |  Scan all open windows.
if ( !exists("g:ShowFuncScanType") )
  let g:ShowFuncScanType = "buffers"
endif

" Default SortType Options:   yes      |  Display output sorted alphabetically.
"                             no       |  Display output in file order.
"                             foldcase |  Display output sorted alphabetically,
"                                      |  disregarding case.
if ( !exists("g:ShowFuncSortType") )
  let g:ShowFuncSortType = "foldcase"
endif

" If necessary, uncomment and set the g:showfuncctagsbin variable.
" i.e. for vim on Windows.
" let g:showfuncctagsbin = "C:\\gnu\\ctags\\ctags.exe"
" ------------------------------------------------------------------------------
" Functions 

" Rotate through available scan types.
function! <SID>ChangeScanType()
	if g:ShowFuncScanType == "buffers"     | let g:ShowFuncScanType = "windows"
	elseif g:ShowFuncScanType == "windows" | let g:ShowFuncScanType = "current"
	elseif g:ShowFuncScanType == "current" | let g:ShowFuncScanType = "buffers"
  endif
	call <SID>ShowFuncOpen()
endfunction

" Rotate through available sort types.
function! <SID>ChangeSortType()
	if g:ShowFuncSortType == "no"            | let g:ShowFuncSortType = "yes"
	elseif g:ShowFuncSortType == "yes"       | let g:ShowFuncSortType = "foldcase"
	elseif g:ShowFuncSortType == "foldcase"  | let g:ShowFuncSortType = "no"
  endif
	call <SID>ShowFuncOpen()
endfunction

" Ctags binary tests
function! s:CtagsTest(path)
  " if the location of the ctags executable is not already configured, then 
  " attempt to find it....
  if ( a:path == "unk" )
    if ( has("unix") )
      let l:test_paths = "/usr/local/bin/ctags /usr/bin/ctags "
    elseif ( has("win32") )
      let l:test_paths = "C:\\gnu\\ctags\\ctags.exe "
    else 
      let l:test_paths = ""
    endif
    let l:rpath = "fail"
    " Loop over tag lists and create the grepformat statement.
    while l:test_paths != ''
      let l:pathcut = strpart(l:test_paths,0,stridx(l:test_paths,' '))
      if ( executable(l:pathcut) )
        let l:rpath = s:CtagsVersionTest(l:pathcut)
        if ( l:rpath != "fail" )
          break
        endif
      endif
      let l:test_paths = strpart(l:test_paths,stridx(l:test_paths,' ') + 1)
    endwhile
    if ( l:rpath == "fail" )
      if ( !has("gui_running") || has("win32") )
        echo confirm ( "ShowFunc Error: Ctags binary not found.\n".
          \            "Please set g:showfuncctagsbin in your .vimrc.\n" ,
          \            "&ok", 0, "Warning" )
      endif
    endif
  else
    " Else test the variable to see that it is actually an executable.
    if ( executable(a:path) )
      let l:rpath = s:CtagsVersionTest(a:path)
    else
      if ( !has("gui_running") || has("win32") )
        echo confirm ( "ShowFunc Error: Ctags binary not found.\n".
          \            "Your g:showfuncctagsbin may be set incorrectly.\n",
          \            "&ok", 0, "Warning" )
      endif
      let g:loaded_showfunc = 0
      let l:rpath = "fail"
    endif  
  endif
  return l:rpath
endfunction 

" Test Ctags version
function! s:CtagsVersionTest(path)
  " Test Ctags for correct version. 
  let ctagsvertest = strpart(strtrans(system(a:path." --version")),
    \ 0,15)
  if ( ctagsvertest != "Exuberant Ctags" )
    if ( !has("gui_running") || has("win32") )
      echo confirm ( "ShowFunc Error: Incorrect Version of Ctags.\n".
        \            "Download the correct version from ".
        \            "http://ctags.sourceforge.net\n", 
        \            "&ok", 0, "Warning" )
    endif
    let g:loaded_showfunc = 0
    let l:rpath = "fail"
  else
    let l:rpath = a:path
  endif
  return l:rpath
endfunction

 " Display a simple help window.
function! <SID>DisplayHelp() 
  echo confirm ("ShowFunc Help:          \n".
  \             " c  Close                   \n".
  \             " r  Refresh                 \n".
  \             " s  Change Scan Sort  \n".
  \             " t  Change Scan Type \n",
  \             "&ok", 0, "Info")
endfunction

" Determine the best window height for the new cwindow and open it.
function! s:OpenCWin()
  let l:mod_total = 0
  let l:win_count = 1
  " Determine correct window height
	windo let l:win_count =  l:win_count + 1
  if ( l:win_count <= 2 ) | let l:win_count = 4 | endif
  windo let l:mod_total = l:mod_total + winheight(0)/l:win_count |
  \ execute 'resize +'.l:mod_total
  " Open cwindow
  execute 'belowright copen '.l:mod_total
	let l:cwin_filelen = line("$")
  " Test for short output lists.
  if ( l:cwin_filelen < winheight(0) )
    cclose
    " And adjust cwindow height accordingly.
    execute 'belowright copen '.l:cwin_filelen
  endif
  " Set cwindow specific key mappings.
  nnoremap <buffer> <silent> c :cclose<CR>
  nnoremap <buffer> <silent> h :call <SID>DisplayHelp()<CR>
  nnoremap <buffer> <silent> r :call <SID>ShowFuncOpen()<CR>
  nnoremap <buffer> <silent> s :call <SID>ChangeSortType()<CR>
  nnoremap <buffer> <silent> t :call <SID>ChangeScanType()<CR>
  set nobuflisted
  return
endfunction

" Set Folds by filename
function! s:SetFolds()
  let l:test_line = getline(v:lnum)
  let l:test_filename = strpart(l:test_line,0,stridx(l:test_line,'|'))
  if ( g:FoldFileName == '' )
    let g:FoldFileName = l:test_filename
    return ">1"
  elseif ( g:FoldFileName == l:test_filename )
    return "="
  else
    let g:FoldFileName = l:test_filename
    return ">1"
  endif
endfunction

" Set FoldText to filename and tag count.
" Note: Do not use s: or <SID> on this function or the format will be lost the
" first time a fold is opened.
function! SetFoldText()
  let l:linefiller = ""
  let l:line = getline(v:foldstart)
  let l:subline = strpart(l:line,0,stridx(l:line,'|'))
  if ( strlen(l:subline) < 59 )
    let l:count =  59 - strlen(l:subline)
    while ( l:count > 0 )
      let l:linefiller = l:linefiller." "
      let l:count = l:count - 1
    endwhile
  endif
  let l:tag_count = v:foldend - v:foldstart + 1
  if ( l:tag_count <= 9 )
    return v:folddashes."+ File: ".l:subline.l:linefiller." Tags:    ".
      \ l:tag_count." "
  elseif ( l:tag_count <= 99 )
    return v:folddashes."+ File: ".l:subline.l:linefiller." Tags:   ".
      \ l:tag_count." "
  elseif ( l:tag_count <= 999 )
    return v:folddashes."+ File: ".l:subline.l:linefiller." Tags:  ".
      \ l:tag_count." "
  else
    return v:folddashes."+ File: ".l:subline.l:linefiller." Tags: ".
      \ l:tag_count." "
  endif
endfunction

" SetGrepFormat and SetGrepPrg both have language and tag support hard coded so
" any changes to exuberant ctags will need to be reflected here.  And on top of
" that its ugly.  I submitted a patch to exuberant ctags that has been accepted
" for inclusion in the next version that will allow us to query ctags directly
" for supported languages and tags.
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
  let l:tags = l:tags." "
  " Loop over tag lists and create the grepformat statement.
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
  if ( &filetype == "asm"     || &filetype == "asp"     || &filetype == "awk"   ||
     \ &filetype == "beta"    || &filetype == "c"       || &filetype == "cobol" ||
     \ &filetype == "eiffel"  || &filetype == "fortran" || &filetype == "java"  ||
     \ &filetype == "lisp"    || &filetype == "lua"     || &filetype == "make"  ||
     \ &filetype == "pascal"  || &filetype == "perl"    || &filetype == "php"   ||
     \ &filetype == "python"  || &filetype == "rexx"    || &filetype == "ruby"  ||
     \ &filetype == "scheme"  || &filetype == "sh"      || &filetype == "slang" ||
     \ &filetype == "sql"     || &filetype == "tcl"     || &filetype == "vera"  ||
     \ &filetype == "verilog" || &filetype == "vim"     || &filetype == "yacc"  )
    let l:ret_cmd = g:showfuncctagsbin.' -x --language-force='.&filetype.' --sort='.a:sort
  elseif ( &filetype == "cpp"  )
    let l:ret_cmd = g:showfuncctagsbin.' -x --language-force=c++ --sort='.a:sort
  else
     return "fail" 
  endif
  return l:ret_cmd
endfunction 

function! <SID>ShowFuncOpen()
	set lazyredraw
  " Close any existing cwindows.
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
		  else
        echo confirm ("ShowFunc Error: Unknown FileType", "&ok", 0, "Info")
      endif
		endif
	  let &grepformat = l:gf_s
    let &grepprg = l:gp_s
		execute s:OpenCWin()
		if ( g:ShowFuncScanType == "buffers" || g:ShowFuncScanType ==  "windows" )
      " Do folding.
      let g:FoldFileName = ''
      setlocal foldexpr=s:SetFolds()
      setlocal foldmethod=expr
      setlocal foldtext=SetFoldText()
    endif
	else
    echo confirm ("ShowFunc Error: Window too small.\n", "&ok", 0, "Info")
  endif
	set nolazyredraw
	redraw!
endfunction  
" ------------------------------------------------------------------------------
" Test Ctags Binary to be sure its the right one...
if ( exists("g:showfuncctagsbin") )
  let g:showfuncctagsbin = s:CtagsTest(g:showfuncctagsbin)
else
  let g:showfuncctagsbin = s:CtagsTest("unk")
endif


if ( g:showfuncctagsbin == "fail" )
  " echo confirm ("Failed: ".g:showfuncctagsbin,"&ok",0,"Warning")
  let g:loaded_showfunc = 0
  delfunction <SID>ChangeScanType
  delfunction <SID>ChangeSortType
  delfunction s:CtagsTest
  delfunction s:CtagsVersionTest
  delfunction <SID>DisplayHelp
  delfunction s:OpenCWin
  delfunction s:SetFolds
  delfunction SetFoldText
  delfunction s:SetGrepFormat
  delfunction s:SetGrepPrg
  delfunction <SID>ShowFuncOpen
  finish
endif 
" ------------------------------------------------------------------------------
" Key Mappings
" To change the main key mapping, add this to your .vimrc file:
"   map <key> <PLug>ShowFunc

if ( !hasmapto('<PLUG>ShowFunc') && (maparg('<F1>') == '') )
	map  <F1> <Plug>ShowFunc
  map! <F1> <Plug>ShowFunc
elseif ( !hasmapto('<PLUG>ShowFunc') )
  if ( !has("gui_running") || has("win32") )
    echo confirm ("ShowFunc Error: No Key mapped.\n".
      \           "<F1> is taken and a replacement was not assigned.\n",
      \           "&ok", 0, "Warning")
  endif
  let g:loaded_showfunc=0
  finish
endif
noremap  <silent> <Plug>ShowFunc   :call <SID>ShowFuncOpen()<CR>
noremap! <silent> <Plug>ShowFunc   <ESC>:call <SID>ShowFuncOpen()<CR>

" Cwindow specific key mappings can be found in the OpenCWinfunction.
" ------------------------------------------------------------------------------
" Known Issues:
" 1.  Error messages that occur as gvim is loading (on Linux) do not display in 
"     GUI windows.  When called from a menu or icon, it appears that gvim is hung
"     (it appears in the ps listings but no window appears).  To avoid this I 
"     have disabled the display of errors during gvim loading and the ShowFunc 
"     script simply exits.  
" ------------------------------------------------------------------------------
" Feature Wishlist:
" 1.  If scan is set to "current", make cwindow update on buffer change (except
"     to the cwindow)
" 2.  Window size ratios should remain the same as ShowFunc opens and closes.
" 3.  Patch vim to allow for setlocal statusline.
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
"                      (vimscript#95) and Bufexplorer (vimscript#42) scripts.
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
" 1.4.2    03-13-2003  Rewrote the SetGrepFormat and SetGrepPrg functions. Added
"                      support for all tags for all languages that Exburent
"                      Ctags (ver. 5.4) supports.
" 1.4.3    03-15-2003  Automatically fold output on filename for multiple file
"                      scans (all buffers or windows).
" 1.4.4    03-17-2003  Improved error handling.  Improved SetFoldText().
" 1.4.5    04-01-2003  More error handling improvements, including tests for the 
"                      correct version of ctags, and keymap assignment.  Thanks
"                      to Mark Thomas for his assistance in finding and fixing a
"                      bug in the executable detection on Windows.  
" ------------------------------------------------------------------------------
" vim: tw=80 ts=2 sw=2
