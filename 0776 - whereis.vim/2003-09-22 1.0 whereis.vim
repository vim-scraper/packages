" File: whereis.vim
" Author: Lechee.Lai 
" Version: 1.0
" 
" goal:
"   make it easy to find file in deep struct directory like BIOS develop  
" which inlcude hundread of directory and thousand of file for one BIOS
" project. So env(bhome) is porject for looking.
"
" TODO:
"   of course remember last search directory is better for env(bhome) could 
" someone give me a hint 
" 
" ============ Output Format =====================
" C:\vim\vim62\plugin\vgrep.vim
" C:\vim\vim62\plugin\whereis.vim
" ================================================
"
" Command :Whereis 
"         :Vwhereis View whereis result and select by ENTER
"
"
if exists("loaded_whereis") || &cp
    finish
endif
let loaded_whereis = 1

if !exists("Whereis_Output")
	let Whereis_Output = 'c:\fte.dir'
endif
 
if !exists("WhereisDir")
       if $bhome == "" 
         let WhereisDir = getcwd()
       else
       	 let WhereisDir = $bhome
       endif
endif       

if !exists("Whereis_Null_Device")
    if has("win32") || has("win16") || has("win95")
        let Whereis_Null_Device = 'NUL'
    else
        let Whereis_Null_Device = '/dev/null'
    endif
endif
 

" RunWhereisCmd()
" Run the specified whereis command using the supplied pattern
function! s:RunWhereisCmd(cmd, pattern)
    let Whereis_Output = g:Whereis_Output
    let del_str = 'del ' . Whereis_Output
    let cmd_del = system(del_str)   
    let cmd_output = system(a:cmd)

    if filereadable(Whereis_Output)
      exe "redir! > " . g:Whereis_Null_Device
      silent echon cmd_del
      redir END
    endif

    if cmd_output == ""
        echohl WarningMsg | 
        \ echomsg "Error: File " . a:pattern . " not found" | 
        \ echohl None
        return
    endif

    let tmpfile = Whereis_Output

    exe "redir! > " . tmpfile
    silent echon cmd_output
    redir END

endfunction

" EditFile()
"
function! s:EditFile()
    let fname = getline('.')
    exe 'edit ' . fname
endfunction	


" RunWhereis()
" Run the specified whereis command
function! s:RunWhereis(...)
    " No argument supplied. Get the identifier and file list from user
    let Whereis_Output = g:Whereis_Output
    let pattern = input("Whereis: ")
    if pattern == ""
	echo "Cancelled."    
        return
    endif
    
    let WhereisDir = input("Start DIRs: ", g:WhereisDir)
    if WhereisDir == ""
	    echo "Cancelled."    
	    return
    endif        
    " Here is Win32 Mode only use internal 'dir' command search through 
    " subdirectory 
    let cmd = 'dir ' . pattern . '/S /B'  " in Bare mode
    
    let last_cd = getcwd()
    exe 'cd ' . WhereisDir
    call s:RunWhereisCmd(cmd, pattern)
    exe 'cd ' . last_cd

   if filereadable(Whereis_Output)
        setlocal modifiable 
        exe 'edit ' . Whereis_Output
        setlocal nomodifiable
    endif 
    nnoremap <buffer> <silent> <CR> :call <SID>EditFile()<CR>
endfunction

function! s:RunVwhereis()
    let Whereis_Output = g:Whereis_Output    
    setlocal modifiable
    exe 'edit ' . Whereis_Output
    nnoremap <buffer> <silent> <CR> :call <SID>EditFile()<CR>
    setlocal nomodifiable
endfunction

" Define the set of Whereis commands
command! -nargs=* Whereis call s:RunWhereis(<q-args>)
command! Vwhereis call s:RunVwhereis()
