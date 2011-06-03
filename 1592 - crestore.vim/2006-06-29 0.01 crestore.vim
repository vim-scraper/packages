" Name:
"
"    crestore.vim
"
" Copyright:
" 
"    Jochen Baier, 2006 (email@Jochen-Baier.de)
"
" Based on:
"
"   qf.vim (Yegappan Lakshmanan)
"   name for the command suggested by Nikolai Weibull
"
"
" Version: 0.01
" Last Modified: Jun 29, 2006
"
" Description:
"
" Save (and restore afterwards) the current file and cursor position before using a quickfix command
" (like vimgrep, make...).
"
"
" The script "Crestore.vim" provide the command "Crestore". Before using a quickfix command the script
" will save the current file and cursor position. Using "Crestore" will restore the file used before.
"
"
" Installation: 
"
" Drop crestore.vim into your plugin directory    
"
" Usage:
"
" 1. Run a quickfix command: vimgrep, make etc.
" 2. Jump around in the list, fix Errors ...
" 3. Type "Crestore" to go back to the file you used before. Quickfix window will be closed.



if exists("g:loaded_crestore")            
    finish
endif
let g:loaded_crestore = 1   


function! s:Quickfixtrigger()

  "call Decho ("Quickfixtrigger")
    
  let s:savedfile =  fnamemodify(expand('%'), ':p') 
  let s:savedpos=  getpos('.')
 
  "copen

endfunction


function! s:Crestore()

  "call Decho ("crestore")

  cclose

  if !exists ('s:savedpos') || !exists ('s:savedfile')

   echo  "Nothing to restore."
    return
  endif

  exe 'edit ' . s:savedfile
  cal setpos('.', s:savedpos)    

endfunction


command! Crestore call s:Crestore()
autocmd QuickFixCmdPre * call s:Quickfixtrigger()
