" -*- vim -*-
" FILE: "c:/vim/Vimfiles/plugin/FiletypeRegisters.vim" {{{
" LAST MODIFICATION: "Thu, 20 May 2004 16:06:53 Eastern Daylight Time"
" (c) Copyright 2004 Hewlett-Packard Development Company, L.P.
" $Id:$ }}}
"
" Version 1.0

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
" - Keith Roberts:  Allow certain filetypes to retain global settings through
"   the use of an option (set through an ftplugin, for example) -- these would
"   inherit the settings of the last window, in all likelihood, rather than
"   setting their own.
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

function! s:SetLocalRegisters()
  let i = 0
  while ( i < strlen( GetVar( 'localRegisters', '' ) ) )
    let register = GetVar( 'localRegisters', '' )[i]
    let registerName = <SID>GetRegisterName( register )

    if ( exists( "g:" . &ft . "_local" . registerName ) )
      execute "let @" . register . "=g:" . &ft . "_local" . registerName
    endif

    let i = i + 1
  endwhile
endfunction

function! s:SaveLocalRegisters()
  let i = 0
  while ( i < strlen( GetVar( 'localRegisters', '' ) ) )
    let register = GetVar( 'localRegisters', '' )[i]
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
  elseif ( GetVar( 'localRegisters', '' ) !~ register )
    echohl User5 | echo "Register " . register . " is global." | echohl None
    return
  endif

  let otherType=input( "Enter other filetype to use to copy register " . register . ":  ", GetVar( "lastUsedFiletype", '' ) )

  if ( otherType == '' )
    return
  endif

  " Used for the next time this mapping is used
  let g:lastUsedFiletype=otherType

  let variableName = "g:" . otherType . "_local" . registerName

  if ( exists( variableName ) )
    execute "let @" . register . "=" . variableName
  endif
endfunction
com! Copyregister call <SID>CopyRegister()
