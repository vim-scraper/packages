" SCCS Diff information integrated in vim, Erik Janssen
" Email: ejanssen@itmatters.nl
" 
" History:
" ========
" xx-04-2001 Concept, compare file-last_delta
" 31-07-2001 Added comparison of file-any_delta and delta-delta
" 10-08-2001 Finally got the regexpr's right in SccsComplexDiff
" 25-04-2002 Migrated to vim 6 using vimdiff functionality, reasonably stable 
" 26-04-2002 Fixed some path problems
" 28-06-2002 Added explicit and implicit cleanup of (some) temporary buffers
" 05-03-2003 Set buftype=nofile for temporary buffers
"            Improved cleanup of buffers for delta-to-delta diffs
" 11-04-2003 Made SimpleDiff implicitly do a ComplexDiff in case of a
"            non-checked-out file. Improved title of delta select window
" 18-07-2003 Solved problem when :splitbelow is set
"
" Description:
" ============
" For files that are under version control this vim script allows easy
" compares with previous delta's. Currently only sccs supported. Adding
" support for others shouldn't be a big effort

" Usage:
" ======
" Dump this file in ~/.vim/plugin. It's version 6.x only
" 
" ,s      Run sccs to obtain a diff between the current file and the last
"         archived delta. Buffers on screen are rearranged so that only the
"         source file and the diff info are visible.
"         
" ,S      Run sccs to obtain a list of all archived delta's. Rearrange the
"         buffers on screen so that only the source file and the delta overview 
"         is available. In the delta overview buffer use the keys <up>, <down> 
"         and <enter> to select a delta. sccs will be run to obtain the
"         differences between the source file and the selected delta. The
"         screen will end up like with ,s.
"
" ,R      Like ,S but after selecting a delta you've got to select a second
"         delta. A diff of the two selected delta's will be obtained and shown
"         like with ,s. Note: it says on top of the screen for which delta it
"         is waiting. 
"         
" ,c      Remove temporary buffers. Current implementation doesn't clean
"         everything yet
"
" To ease remembering the mappings, a submenu 'sccs' is added in a menu called
" 'local' when using gvim. One might want to change this
" 
" Bugs:
" =====
" - It's better to vertical split than horizontal split

" Initialisation
" ==============
if exists("g:sccs_loaded")
    finish
endif

let g:sccs_loaded = 1

" These two variables are used to maintain buffer names which need to be
" removed later on

let s:DiffBufs = ""
let s:DeltaBufs = ""

" This one defines the number of lines that is used as a header in delta
" selection buffers

let s:LastHeaderLine = 4

" Simple SCCS version information
" ================================
" This one hides all buffers except the current one, then  runs 'sccs get'
" on the file in the current buffer, splits the screen and loads the previous
" version in the new window. vim will then run diff to get the differences.
" Mapped to ,s.  First use throws some messages on screen, but it works o.k.

function! SccsSimpleDiff()
   let sccs_mode = SccsCheckFile( bufname("%") )
   if sccs_mode != 0
     if sccs_mode == 2
        call SccsResetDiff()
        let original = bufname("%")
        call SccsFreshBuffer( SccsGenDeltaName( original, "prev" ) )   
        let s:DiffBufs = s:DiffBufs . " " . bufnr("%")
        exe "r!sccs get -p " . bufname("#")
        call SccsActivateVimDiff()
     else
        " File is not checked out. What we should do now is automatically do a
        " diff between last- and second-last version. Temporary workaround is
        " to use the complex diff function....
        call SccsComplexDiff(" File not co'ed. Select delta to compare with most recent")
     endif
   endif
endfunction

" Complex SCCS version information
" ================================
" This function creates an overview of delta's in the sccs file belonging to
" the current file. This overview is put in a new buffer. User interaction in
" this buffer is handled via a set of mappings in the autocommands below.  Use
" up and down to navigate through the delta's. Press <enter> to select one and
" wait a while. This function is either called directly via a mapping (compare
" file with a delta) or via SccsDeltaDiff (compare two delta's)

function! SccsComplexDiff( extraStr )
   if SccsCheckFile( bufname("%") ) != 0
     let tempname = substitute( bufname("%"), "[\\/]", "_", "g" ) 
     call SccsResetDiff()
     call SccsFreshBuffer( "delta_" . tempname )
     let s:DeltaBufs = s:DeltaBufs . " " . bufnr("%")
     set ft=
     r!sccs prs -e #
     1,2d
     normal! gg
     " vim 5.7 solution to reformat the sccs output, thanks to
     " the vim mailing list (\d = digit, @ used as separator)
     g@^D \(\d\+\(\.\d\+\)\+ \d\d/\d\d/\d\d\) .*$@ s//\1/
     g/^MRs:$/,/^COMMENTS:$/d
     %g/.\+/j
     %g/.\+/j
     g/^$/d
     call SccsSelectionWindowHeader( a:extraStr )
     normal! jV
     let b:sccs_state = 1
   endif
endfunction

" This one triggers the procedure for creating a buffer containing the
" difference between two arbritrary delta's

function! SccsDeltaDiff()
   if SccsCheckFile( bufname("%") ) != 0
      call SccsComplexDiff( " Choose FIRST Delta" )
      let b:sccs_state = 2
   endif
endfunction

"
" LOCAL FUNCTIONS
"

" Create a buffer 'bufname' or clear it if it already exists. Make sure the
" new buffer is on top of the screen. the buffer that's current when calling
" this function will be on the bottom. There'll be only two buffers visible.

function! SccsFreshBuffer( bufname )
  let original = bufname( "%" )
  if !bufexists( a:bufname )
      new
      exe "file " . a:bufname
      set buftype=nofile
   endif
   set nodiff
   exe "b " . a:bufname
   1,$d
   exe "b " . original
   only
   exe "split +b " . a:bufname
endfunction

" Verify that a file 'filename' is under SCCS control
"
" Returns:
"    0  File is not under SCCS control
"    1  File is under SCCS control, not checked out
"    2  File is under SCCS control, checked out

function! SccsCheckFile( filename )
   let the_path = fnamemodify( a:filename, ":h" )
   let the_name = fnamemodify( a:filename, ":t" )
   if strlen( the_path ) == 0
      let the_path = "."
   endif
   let sccs_name = the_path . "/SCCS/s." . the_name
   if filereadable(expand(sccs_name))
     let sccs_name = the_path . "/SCCS/p." . the_name
     if filereadable(expand(sccs_name))
        return 2
     else
        return 1
     endif
   endif
   echo "This file is not under sccs control"
   return 0
endfunction


" Create first lines of a buffer containing the delta overview: blank lines
" with title string. Leave the cursor on the first line not part of the header

function! SccsSelectionWindowHeader( title )
     normal! gg
     exe "normal! i\<nl>" . fnamemodify(bufname("#"),":t") . ":". a:title . "\<nl>\<nl>\<Esc>"
endfunction

" Create a filename prefixed by the number of the delta it represents

function! SccsGenDeltaName( filename, delta )
   let the_path = fnamemodify( a:filename, ":h" )
   let the_name = fnamemodify( a:filename, ":t" )
   if strlen( the_path ) == 0
      let the_path = "."
   endif
   return the_path . "/" . a:delta . "_" . the_name
endfunction   

" After the delta(s) is/are loaded, activate the vimdiff feature. Precondition:
" two buffers on screen, the current one is the old delta, the other one is
" the current version

function! SccsActivateVimDiff()
  diffthis
  if &splitbelow
    normal k
  else
    normal j 
  endif
  diffthis
  normal! gg
endfunction

" Reset the diff option of every buffer before a new one is enabled, and
" remove temporary diff buffers as it seems nobody is interested anymore.

function! SccsResetDiff()
  let orig=bufname("%")
  bufdo! call SccsDiffOff()
  if strlen( s:DiffBufs ) > 0
     exe "bd! " . s:DiffBufs
     let s:DiffBufs = ""
  endif
  exe "b " . orig
endfunction

" Remove temporary buffers with diff and delta information (explicitly called
" by user)

function! SccsCleanup()
  let orig=bufname("%")
  bufdo! call SccsDiffOff()
  if strlen( s:DiffBufs ) > 0
     exe "bd! " . s:DiffBufs
     let s:DiffBufs = ""
  endif
  if strlen( s:DeltaBufs ) > 0
     exe "bd! " . s:DeltaBufs
     let s:DeltaBufs = ""
  endif
  exe "b " . orig
endfunction

" Turn diff mode and folding off if a buffer. Call this function with bufdo.
" This way it doesn't mess with existing non diff related folds.

function! SccsDiffOff()
   if &diff
      set nodiff
      set foldcolumn=0
   endif
endfunction

" Move one line up/down in the delta overview buffer. This function is required
" to prevent the cursor from attempting to go beyond the first/last line, vim 
" loses the linewise visual selection in that case.

function! SccsDeltaDown()
   if line(".") < line("$")
     normal! j
   endif
endfunction

function! SccsDeltaUp()
   if line(".") > s:LastHeaderLine
     normal! k
   endif
endfunction


" This one handles the <enter> key in a delta selection buffer. There are
" three possibilities:
" sccs_state = 1: Load a diff of the current version and the selected version
" sccs_state = 2: Store the delta in sccs_delta1, adjust the text in the
"                 buffer and increase the state
" sccs_state = 3: Load a diff of sccs_delta1 and the just selected delta if
"                 they're different. reset the state to 2 and re-adjust the
"                 text
" Note that multiple delta overview may be open, each in their own state of
" selecting first and second delta's

function! SccsGetSpecificDiff()
  normal! ^"zyiW
  let original = substitute(getline(2), "\:.*$", "", "" )
  if b:sccs_state == 1
     exe "b " . original
     silent call SccsFreshBuffer( SccsGenDeltaName( original, @z ) )   
     let s:DiffBufs = s:DiffBufs . " " . bufnr("%")
     silent exe "r!sccs get -p -r" . @z . " " . original
     silent call SccsActivateVimDiff()
     return
  endif
  if b:sccs_state == 2
     let b:sccs_delta1 = @z
     let b:sccs_state = 3
     " Mmmm, you won't notice this if you're not looking for it
     1,2s/FIRST/SECOND/
     normal! ggjV
     return
  endif
  if b:sccs_state == 3
     1,2s/SECOND/FIRST/
     let b:sccs_delta2 = @z
     let b:sccs_state = 2
     if b:sccs_delta1 == b:sccs_delta2
       normal! ggjV
       echo "Please choose 2 different delta's!"
     else
       let l:d1 = b:sccs_delta1
       let l:d2 = b:sccs_delta2
       silent call SccsFreshBuffer( SccsGenDeltaName( original, l:d1 ) )   
       let s:DiffBufs = s:DiffBufs . " " . bufnr("%")
       silent exe "r!sccs get -p -r " . l:d1 . " " . original
       silent call SccsFreshBuffer( SccsGenDeltaName( original, l:d2 ) )   
       let s:DiffBufs = s:DiffBufs . " " . bufnr("%")
       silent exe "r!sccs get -p -r " . l:d2 . " " . original
       silent call SccsActivateVimDiff()
     endif
     return
  endif
endfunction

" These autocommands take care of user interaction in the delta overview
" buffer(s)

augroup delta_
  au!
  autocmd BufEnter delta_* normal! ggjV
  autocmd BufFilePost,BufEnter delta_* vnoremap <Down> :call SccsDeltaDown()<CR>V
  autocmd BufFilePost,BufEnter delta_* vnoremap j :call SccsDeltaDown()<CR>V
  autocmd BufFilePost,BufEnter delta_* vnoremap <Up> :call SccsDeltaUp()<CR>V
  autocmd BufFilePost,BufEnter delta_* vnoremap k :call SccsDeltaUp()<CR>V
  autocmd BufFilePost,BufEnter delta_* vnoremap <Return> V:call SccsGetSpecificDiff()<CR>
  autocmd BufLeave delta_* vunmap <Down>
  autocmd BufLeave delta_* vunmap j
  autocmd BufLeave delta_* vunmap <Up>
  autocmd BufLeave delta_* vunmap k
  autocmd BufLeave delta_* vunmap <Return>
augroup END

" Create mappings and menu
nmap ,s :call SccsSimpleDiff()<CR>
nmap ,S :call SccsComplexDiff("Select delta to compare with edited file")<CR>
nmap ,R :call SccsDeltaDiff()<CR>
nmap ,c :call SccsCleanup()<CR>
if has("menu")
  nmenu &Local.&Sccs.Diff\ with\ last\ delta<Tab>,s :call SccsSimpleDiff()<CR>
  nmenu &Local.&Sccs.Diff\ with\ any\ delta<Tab>,S :call SccsComplexDiff("Select\ delta\ to\ compare\ with\ edited\ file")<CR>
  nmenu &Local.&Sccs.Diff\ two\ delta's<Tab>,R :call SccsDeltaDiff()<CR>
  nmenu &Local.&Sccs.Remove\ diff\ buffers<Tab>,c :call SccsCleanup()<CR>
endif
