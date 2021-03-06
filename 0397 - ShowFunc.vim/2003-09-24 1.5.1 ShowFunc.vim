" ------------------------------------------------------------------------------
" Filename:      ~/.vim/plugin/ShowFunc.vim
" VimScript:     #397
" Last Modified: 21 Sep 2003 08:15:50 PM by Dave Vehrs
" Maintainer:    Dave Vehrs (davev at ziplip.com)
" Copyright:     (C) 2002-2003 Dave Vehrs
"                This script is free software; you can redistribute it and/or 
"                modify it under the terms of the GNU General Public License as 
"                published by the Free Software Foundation; either version 2 of 
"                the License, or (at your option) any later version.
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
" Configuration:                                                             {{{

" Test for and if necessary configure all default settings.  If you would like
" to change any setting, just add let g:variablename = "new-value" to your
" .vimrc.
" For Example, to change the location of the ctags binary, add this:
"     let g:showfuncctagsbin = "/bin"
 
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
" let g:showfuncctagsbin = "ctags"

"                                                                            }}}
" ------------------------------------------------------------------------------
" Functions:                                                                  {{{

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
    let l:test_paths = "/usr/local/bin/ctags ".
      \ "/usr/bin/ctags C:\\gnu\ctags\\ctags.exe "
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

" Test to be sure we have Exuberant Ctags
function! s:CtagsVersionTest(path)
  " Test Ctags for correct version. 
  let ctagsvertest = strpart(strtrans(system(a:path." -x  --version")),
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

" Set Folds by filename.
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
function! SetFoldText()
  let l:line = ""
  let l:textwidth = &textwidth - 20
  let l:line = getline(v:foldstart)
  let l:line = strpart(l:line,0,stridx(l:line,'|'))
  if ( strlen(l:line) < l:textwidth )
    let l:count =  59 - strlen(l:subline)
    while ( strlen(l:line) < l:textwidth )
      let l:line = l:line." "
    endwhile
  endif
  let l:tag_count = v:foldend - v:foldstart + 1
  if ( l:tag_count <= 9 )
    return v:folddashes."+ File: ".l:line." Tags:    ". l:tag_count." "
  elseif ( l:tag_count <= 99 )
    return v:folddashes."+ File: ".l:line." Tags:   ". l:tag_count." "
  elseif ( l:tag_count <= 999 )
    return v:folddashes."+ File: ".l:line." Tags:  ". l:tag_count." "
  else
    return v:folddashes."+ File: ".l:line." Tags: ". l:tag_count." "
  endif
endfunction

" Set ctags options to call.
function! s:SetGrepPrg(sort)
  if ( g:CtagsVersion < 5 )
    if ( &filetype == "asm"     || &filetype == "asp"     || &filetype == "awk"   ||
      \ &filetype == "beta"    || &filetype == "c"       || &filetype == "cobol" ||
      \ &filetype == "eiffel"  || &filetype == "fortran" || &filetype == "java"  ||
      \ &filetype == "lisp"    || &filetype == "lua"     || &filetype == "make"  ||
      \ &filetype == "pascal"  || &filetype == "perl"    || &filetype == "php"   ||
      \ &filetype == "python"  || &filetype == "rexx"    || &filetype == "ruby"  ||
      \ &filetype == "scheme"  || &filetype == "sh"      || &filetype == "slang" ||
      \ &filetype == "sql"     || &filetype == "tcl"     || &filetype == "vera"  ||
      \ &filetype == "verilog" || &filetype == "vim"     || &filetype == "yacc"  )
      let l:grep_return = g:showfuncctagsbin .' -x --language-force=' . &filetype . 
        \ ' --sort=' . a:sort
    elseif ( &filetype == "cpp"  )
      let l:grep_return = g:showfuncctagsbin .' -x --language-force=c++ --sort=' . 
        \ a:sort
    else
       return "fail" 
    endif
  else
    if ( &filetype == "cpp" ) | let l:cfiletype = "c++"
    else                      | let l:cfiletype = &filetype | endif
    let l:filetest = s:TestFileType(l:cfiletype)
    if ( l:filetest != "false" )
      let l:grep_return = g:showfuncctagsbin . ' -x --language-force=' . 
        \ l:cfiletype . ' --sort=' . a:sort
    else | let l:grep_return = "fail" | endif
  endif
  return l:grep_return
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
    let &grepformat = '%*\k%*\s%*[^0-9]%*\s%l%*\s%f %m' 
		if ( g:ShowFuncScanType == "buffers" )
      " Scan all open buffers.
	    let l:currbuf = bufnr("%")
	    bufdo! let &grepprg = s:SetGrepPrg(g:ShowFuncSortType) | 
      \ if ( &grepprg != "fail" ) | if ( &readonly == 0 ) | update | endif |
			\ if ( l:count == 0 ) | silent! grep! % | let l:count =  l:count + 1 |
			\ else | silent! grepadd! % | endif | endif
		  execute 'buffer '.l:currbuf
		elseif ( g:ShowFuncScanType == "windows" )
		  " Scan all open windows.
	    windo let &grepprg = s:SetGrepPrg(g:ShowFuncSortType) | 
      \ if ( &grepprg != "fail" ) | if ( &readonly == 0 ) | update | endif |
			\ if ( l:count == 0 ) | silent! grep! %| let l:count =  l:count + 1 |
			\ else | silent! grepadd! % | endif | endif
		elseif ( g:ShowFuncScanType == "current" )
		  " Scan current buffer only.
      let &grepprg = s:SetGrepPrg(g:ShowFuncSortType)
		  if ( &grepprg != "fail" )
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

" Test for supported filetype.
function! s:TestFileType(type) 
  let l:supportedfiles = g:CTags_Supported_FileTypes
  while ( l:supportedfiles != "^@" && l:supportedfiles != "" ) 
    let l:sfcut = strpart(l:supportedfiles,0,stridx(l:supportedfiles,"^@"))
    if ( l:sfcut ==? a:type ) 
      return "true"
    endif
    let l:supportedfiles = strpart(l:supportedfiles,stridx(l:supportedfiles,'^@') + 2)
  endwhile
  return "false"
endfunction

"                                                                            }}}
" ------------------------------------------------------------------------------
" Test Environment:                                                          {{{

" Test Ctags Binary to be sure its the correct version.
if ( exists("g:showfuncctagsbin") )
  let g:showfuncctagsbin = s:CtagsTest(g:showfuncctagsbin)
else
  let g:showfuncctagsbin = s:CtagsTest("unk")
endif

" Determine Ctags version.
let teststring = strtrans(system(g:showfuncctagsbin . " -x --version"))
let g:CtagsVersion = strpart(teststring,(stridx(teststring,".")+1),
  \ ((stridx(teststring,",")-(stridx(teststring,"." )+1))))

" Define default supportted file types list.
if ( !exists("g:CTags_Supported_FileTypes") && g:CtagsVersion == 5 )
  let g:CTags_Supported_FileTypes = strtrans(system(g:showfuncctagsbin." -x --list-languages"))
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

"                                                                            }}}
" ------------------------------------------------------------------------------
" Key Mappings:                                                              {{{
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

" Cwindow specific key mappings can be found in the OpenCWinfunction.        }}}
" ------------------------------------------------------------------------------
" Known Issues:                                                              {{{
" 1.  Error messages that occur as gvim is loading (on Linux) do not display in 
"     GUI windows.  When called from a menu or icon, it appears that gvim is hung
"     (it appears in the ps listings but no window appears).  To avoid this I 
"     have disabled the display of errors during gvim loading and the ShowFunc 
"     script simply exits.  
"                                                                            }}}
" ------------------------------------------------------------------------------
" Feature Wishlist:                                                          {{{
" 1.  If scan is set to "current", make cwindow update on buffer change (except
"     to the cwindow)
" 2.  Window size ratios should remain the same as ShowFunc opens and closes.
" 3.  Patch vim to allow for setlocal statusline.
"                                                                            }}}
" ------------------------------------------------------------------------------
" Version History:                                                           {{{
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
" 1.4.5    03-22-2003  More error handling improvements, including tests for the 
"                      correct version of ctags, and keymap assignment.  I want
"                      to thank Mark Thomas for his assistance in finding and 
"                      fixing a bug in the ctags executable detection on Windows.  
" 1.5     09-21-2003   Created a more generic grep format so that explicit type 
"                      definitions are no longer necessary (eliminating the 
"                      SetGrepFormat function).  Modified the SetGrepPrg function 
"                      to detect Ctags versions earlier than 5.5.  Supportted 
"                      filetypes for Ctags versions 5.4 are statically assigned.  
"                      With Ctags versions 5.5 (and later) supported filetypes 
"                      are detected dynamically (including those defined by 
"                      regular expressions (--regex-<LANG>).  
"                                                                            }}}
" ------------------------------------------------------------------------------
" vim:tw=80:ts=2:sw=2:
