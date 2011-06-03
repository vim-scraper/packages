" -*- vim -*-
" FILE: "c:/vim/Vimfiles/plugin/FiletypeRegisters.vim" {{{
" LAST MODIFICATION: "Thu, 20 May 2004 17:00:59 Eastern Daylight Time"
" (c) Copyright 2004 Hewlett-Packard Development Company, L.P.
" $Id:$ }}}
"
" Version 1.1

" History:
"
" 1.0:  Initial version
"
" 1.1:  Added ability for different filetypes to have their own set of stored
" registers.  Note that if a filetype has explicit registers set, it does NOT
" automatically also get localized storage of the global ones -- they have to
" be explicitly included in the local version (setup through the call to
" Setfiletyperegisters).  Suggested by Keith Roberts.

" Plugin to keep certain registers (specified by the global variable
" g:localRegisters) specific to individual filetypes.  For example, if editing
" a Java file in one window and a Vim script in another, one may want to
" search for a variable name in the Java window that has no bearing in the Vim
" window; however, ordinary functionality would clobber the Vim window search
" register value, requiring the use of search history if the original
" expression was complicated.
"
" This relies on my getVar.vim (vimscript #353).
"
" Mappings (default):
"
" <Leader>im -- used to import one register from another filetype
"
" Commands:
"
" Copyregister -- used to import one register from another filetype (the
" mapping just executes this command)
"
" Setfiletyperegisters {registers} [filetype]
"       The first parameter is a set of registers (for example:  abc/) and the
"       second parameter is the filetype to which to apply these registers.
"       If the second parameter is left out, the current &filetype setting
"       will be used.
"
"       From an ftplugin file, one can just call Setfiletyperegisters with a
"       register list.  From _vimrc, one would have to specify the filetype
"       also (otherwise it would use "vim", the filetype of _vimrc).
"
" Customizations:
"
" By default, no registers are localized (so no behavior changes should be
" noticed).  To localize registers, place an assignment to g:localRegisters
" inside your _vimrc:
"
" let g:localRegisters="abcm/"
"
" The above allows registers a, b, c, m and the search register to be
" localized based on filetype (all Vim scripts get one copy, all Java files
" another etc.).
"
" To change the default mapping for importing register contents from another
" filetype, create a normal mode mapping to <Plug>CopyRegister in your _vimrc.
"
" TODO:
"
" - Maybe allow certain filetypes to ALWAYS keep in synch with others?

if exists( "g:loaded_FiletypeRegisters" )
  finish
endif
let g:loaded_FiletypeRegisters=1

if ( !hasmapto( '<Plug>CopyRegister', 'n' ) )
  nmap <unique> <Leader>im <Plug>CopyRegister
endif

" Copies the specified register from the specified filetype into the current
" one
nnoremap <Plug>CopyRegister :Copyregister<CR>

augroup FiletypeRegisters
  au!
  au BufEnter,BufNewFile * call <SID>SetLocalRegisters()
  au BufLeave * call <SID>SaveLocalRegisters()
augroup END

" Try to get filetype registers; if that fails, try to get global registers;
" if even that fails, just return the empty string
function! GetLocalRegistersList( ... )
  if ( a:0 > 0 )
    let filetype=a:1
  else
    let filetype=&ft
  endif
  return GetVar( filetype . "_localRegisters", GetVar( "localRegisters", '' ) )
endfunction

function! s:SetLocalRegisters()
  let   registers=GetLocalRegistersList()
  let   i        = 0

  while ( i < strlen( registers ) )
    let register     = registers[i]
    let registerName = <SID>GetRegisterName( register )

    if ( exists( "g:" . &ft . "_local" . registerName ) )
      execute "let @" . register . "=g:" . &ft . "_local" . registerName
    endif

    let i = i + 1
  endwhile
endfunction

function! s:SaveLocalRegisters()
  let registers=GetLocalRegistersList()
  let i        = 0

  while ( i < strlen( registers ) )
    let register = registers[i]
    let registerName = <SID>GetRegisterName( register )

    execute "let g:" . &ft . "_local" . registerName . "=@" . register

    let i = i + 1
  endwhile
endfunction

function! s:GetRegisterName( reg )
  if ( a:reg =~ '[a-zA-Z0-9]' )
    return a:reg
  elseif ( a:reg == '/' )
    return "Search"
  elseif ( a:reg == '*' )
    return 'Star'
  endif

  return ''
endfunction

function! s:SetFiletypeRegisters( registers, ... )
  if ( a:0 > 0 )
    let filetype=a:1
  else
    let filetype=&ft
  endif

  execute "let g:" . filetype . "_localRegisters='" . a:registers . "'"
endfunction
com! -nargs=+ Setfiletyperegisters call <SID>SetFiletypeRegisters( <f-args> )

function! s:CopyRegister()
  echo "Register?  "
  let userInput=getchar()
  let register=nr2char( userInput )

  " Escape or control-c
  if ( userInput == 27 || userInput == 3)
    return
  endif

  let registerName=<SID>GetRegisterName( register )

  if ( registerName == '' )
    echohl User6 | echo "Register " . register . " is invalid." | echohl None
    return
  endif

  let otherType=input( "Enter other filetype to use to copy register " . register . ":  ", GetVar( "lastUsedFiletype", '' ) )

  if ( otherType == '' )
    return
  endif

  " Used for the next time this mapping is used
  let g:lastUsedFiletype=otherType

  if ( GetLocalRegistersList( otherType ) !~ register )
    echohl User5 | echo "Filetype " . otherType . " isn't keeping a local copy of the '" . registerName . "' register." | echohl None
  else
    let variableName = "g:" . otherType . "_local" . registerName

    if ( exists( variableName ) )
      execute "let @" . register . "=" . variableName
    endif
  endif
endfunction
com! Copyregister call <SID>CopyRegister()
