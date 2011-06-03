" File:  regview.vim  
"
" Purpose:
"     This global plugin allows interactive browsing of register
"     contents.  It also allows pasting or executing the contents of
"     the currently viewed register.
"
" Usage:
"     put this file in your ~/.vim/plugin directory
"
" Version:
"     0.9 (Feature complete, needs minor bugfixes)
"
" Last Change:  
"     October 3, 2001
"
" Maintainer:   
"     Cory T. Echols <coryechols@yahoo.com>
"
" Requirements: 
"     Johannes Zellner's ScratchBuffer.vim plugin, which is
"     available from http://www.zellner.org/vim/plugin/ScratchBuffer.vim
"
" TODO:
"     - Implement better sanity checks on the arguments passed to s:RegView
"
" Commands:
"     :RegView <Letter>  -  View the contents of register <Letter>
"
" Maps:
"     <Leader>rf - View the next (forward) register 
"     <Leader>rb - View the previous (backward) register
"     <Leader>rp - Paste the current register (as in the 'p' command)
"     <Leader>rP - Paste the current register (as in the 'P' command)
"     <Leader>r@ - Execute the current register (as in the @ command)
"     <Leader>rc - Close the register preview window
"
" Notes:
"     If you like this script, please see :help uganda

if exists("g:loaded_regview")
  finish
endif

let g:loaded_regview = 1

" Configuration options:

"Maximum size of the Preview window 
let s:maxPreviewSize = 10

"The set of registers that will be iterated through.  Edit this list to change
"the order the registers come in, or to limit the set of browsable registers
"(Useful if you use registers inside of scripts, and want them to be invisible
"to the browse list)
let s:registernames="abcdefghijklmnopqrstuvwxyz"

command! -nargs=1 RegView call <SID>RegView(<f-args>)

if !hasmapto('<Plug>RegViewNext')
   nmap <unique> <Leader>rf <Plug>RegViewNext
endif

if !hasmapto('<Plug>RegViewPrev')
   nmap <unique> <Leader>rb <Plug>RegViewPrev
endif

if !hasmapto('<Plug>Regpaste')
   nmap <unique> <Leader>rp <Plug>Regpaste
endif

if !hasmapto('<Plug>RegPaste')
   nmap <unique> <Leader>rP <Plug>RegPaste
endif

if !hasmapto('<Plug>RegExecute')
   nmap <unique> <Leader>r@ <Plug>RegExecute
endif

if !hasmapto('<Plug>RegViewClose')
   nmap <unique> <Leader>rc <Plug>RegViewClose
endif

" If you don't like my mappings, you can define your own mappings in your
" .vimrc file by mapping the appropriate keystrokes to "<Plug>RegViewNext" and
" friends.
nnoremap <unique> <script> <Plug>RegViewNext :call <SID>RegViewNext()<CR>
nnoremap <unique> <script> <Plug>RegViewPrev :call <SID>RegViewPrev()<CR>  
nnoremap <unique> <script> <Plug>Regpaste :call <SID>Regpaste()<CR>
nnoremap <unique> <script> <Plug>RegPaste :call <SID>RegPaste()<CR>
nnoremap <unique> <script> <Plug>RegExecute :call <SID>RegExecute()<CR>
nnoremap <unique> <script> <Plug>RegViewClose :call <SID>RegViewClose()<CR>


" Open the Register preview window with the register specified by "regname"

function! s:RegView(regname)
   if(strlen(a:regname) > 1)
      echohl ErrorMsg
      echo a:regname." is not a register name"
      echohl None
      return
   endif
   if(stridx(s:registernames, a:regname) == -1)
      echohl ErrorMsg
      echo "Register ".a:regname." is invisible"
      echohl None
      return
   endif
   let s:currentBuf = bufnr('%')
   let s:currentWin = winnr()
   let s:currentReg = a:regname
   let s:scratchBufID =  ScratchBufferOpen('regview_window', 1, 1)
   execute "let regContentLen = strlen(@".s:currentReg.")"
   if (regContentLen > 0)
      let text = "Viewing Register ".s:currentReg
      0put=text
      let text = "-------------------"
      1put=text
      normal o
      execute "normal \"" . s:currentReg . "p<CR>"
   else
      let text = "Nothing in register ".s:currentReg
      0put=text
      let text = "-------------------"
      1put=text
   endif
   let bufheight = line('$')
   if bufheight < s:maxPreviewSize
      exe 'resize '.bufheight
   else
      exe 'resize '.s:maxPreviewSize
   endif
   normal gg
   call ScratchBufferFinish(s:currentWin)
endfunction

" Close the register preview window
function! s:RegViewClose()
   let winid = winnr()
   let swin = ScratchBufferOpen('regview_window', 1, 1)
   bd
   execute "normal ".winid."<C-W>w"
endfunction

" Paste the current register above the current line
function! s:RegPaste()
   if(!exists("s:currentReg"))
      echohl ErrorMsg
      echo "There is no current register"
      echohl None
      return
   endif
   execute "normal \"" . s:currentReg . "P<CR>"
endfunction

" Paste the current register below the current line
function! s:Regpaste()
   if(!exists("s:currentReg"))
      echohl ErrorMsg
      echo "There is no current register"
      echohl None
      return
   endif
   execute "normal \"".s:currentReg."p<CR>"
endfunction

" Execute the current register as a macro
function! s:RegExecute()
   if(!exists("s:currentReg"))
      echohl ErrorMsg
      echo "There is no current register"
      echohl None
      return
   endif
   execute "normal @".s:currentReg
endfunction

" Preview the next register from s:registernames.  Start with the first letter
" in s:registernames if there's no current register
function! s:RegViewNext()
   if !exists("s:currentReg")
      let s:currentReg = s:registernames[strlen(s:registernames) - 1]
   endif
   let currRegNum = stridx(s:registernames, s:currentReg)
   let currRegNum = currRegNum + 1
   if(currRegNum == strlen(s:registernames))
      let currRegNum = 0
   endif
   let currRegLetter = s:registernames[currRegNum]
   call s:RegView(currRegLetter)
endfunction

" Preview the previous register from s:registernames.  Start with the first letter
" in s:registernames if there's no current register
function! s:RegViewPrev()
   if !exists("s:currentReg")
      let s:currentReg = s:registernames[0]
   endif
   let currRegNum = stridx(s:registernames, s:currentReg)
   let currRegNum = currRegNum - 1
   if (currRegNum < 0)
      let currRegNum = strlen(s:registernames) - 1
   endif
   let currRegLetter = s:registernames[currRegNum]
   call s:RegView(currRegLetter)
endfunction
