" Description: Diffsplit that handles restoring of window attributes.
" Author: Michael Geddes
" Date: Nov 2003
" Version: 1.0
"
" History:
"   1.0:
"     * Factor out diff code from source-control.
"     * Added ! to cause the second buffer not not be auto-unloaded.
"
"
" Usage:
"  VDiff[!] <filename>       : Open a vertial difference between the current file that restores buffer settings
"  HDiff[!] <filename>       : Open a horizontal difference between the current file that restores buffer settings
"
"  In both cases the ! causes the specified buffer not to be unloaded when a
"  window is closed, or another window is opened.  In this case, the settings
"  will be restored using the original settings of the first buffer.
"



augroup difwin
au!
au BufHidden,BufDelete,BufUnload * call <SID>DiffBufHide(expand('<afile>'))
au BufEnter * call <SID>ReclearDiffMode()
aug END

fun! s:ReclearDiffMode()
  if exists('b:diff_diffmode') && !b:diff_diffmode && exists('b:diff_diffbuffvars')
    exe b:diff_diffbuffvars
    unlet b:diff_diffbuffvars
  endif
endfun

" 
fun! s:ClearDiffMode()
  if exists('g:diff_diffbuffnr')
    let diffbuffnr=g:diff_diffbuffnr
    unlet g:diff_diffbuffnr
    let win=bufwinnr(diffbuffnr)
    while winbufnr(win) != -1 && !getwinvar(win,'&diff')
      let win=win+1
      while winbufnr(win) != -1 && winbufnr(win) != diffbuffnr
        let win=win+1
      endwhile
    endwhile
    let curwin=winnr()
    if winbufnr(win)!= -1
      exe win.'winc w'
      exe b:diff_diffbuffvars
      if &fdc==0
        norm zn
      endif
      exe curwin.'winc w'
    endif
    if g:diff_diffbuff_delbuffer
      exe 'bdel '.g:diff_diffbuff_difnr
    else
      call setbufvar(g:diff_diffbuff_delbuffer, 'diff_diffmode', 0)
    endif
    unlet g:diff_diffbuff_delbuffer

    let b:diff_diffmode=0
    "unlet b:diff_diffbuffvars
    unlet g:diff_diffbuff_difnr
  endif
endfun

fun! s:SetDiffSplit( ssfile, delbuffer, vert )
    call s:ClearDiffMode()
    let diffs=''
    let win=1
    while winbufnr(win) != -1
        if getwinvar(win, '&diff')
            call setwinvar(win, '&diff',0)
            let diffs='|call setbufvar('.winbufnr(win).',"&diff",1)'
        endif
        let win=win+1
    endwhile
    let b:diff_diffmode=1
    let vars=s:GetDiffSplitVars()
    let b:diff_diffbuffvars=vars
    let g:diff_diffbuffnr=winbufnr(winnr())

    " Vertical diff split
    exe (a:vert?'vert': '').' diffsplit '.a:ssfile

    " Make the defaults the same as the file we are comparing against
    if !a:delbuffer
      let b:diff_diffbuffvars=vars
      let b:diff_diffmode=1
    endif
    let g:diff_diffbuff_difnr=winbufnr(winnr())
    let g:diff_diffbuff_delbuffer=a:delbuffer
endfun

fun! s:GetDiffSplitVars()
    return 'set nodiff '. (&scb?'': 'no').'scb sbo='.&sbo.' '.(&wrap?'': 'no' ).'wrap fdm='.&fdm.' fdc='.&fdc.' '.(&fen?'': 'no').'fen '.(&scb?'': 'no').'scb fdl='.&fdl
endfun

" Hide a diff buffer
fun! s:DiffBufHide( filename)
    if exists('g:diff_diffbuff_difnr')
        let buf=bufnr(a:filename)
        if g:diff_diffbuff_difnr==buf
            call s:ClearDiffMode()
        elseif exists('g:diff_diffbuffnr') && g:diff_diffbuffnr==buf
            let curbuf=winbufnr(winnr())
            if bufexists(buf)
                exe 'buf '.buf
                set diff
                call s:ClearDiffMode()
                exe 'buf '.curbuf
            else
                call s:ClearDiffMode()
            endif
        endif
    endif
endfun

command! -complete=file -bang -nargs=1 -count=0 VDiff call s:SetDiffSplit(<f-args>, <q-bang> != '!', 1)
command! -complete=file -bang -nargs=1 -count=0 HDiff call s:SetDiffSplit(<f-args>, <q-bang> != '!', 0)


