"=============================================================================
" Name Of File: TableTab.vim
"  Description: global plugin for variable-width tab stops 
"   Maintainer: Pavol Juhas (juhas@seas.upenn.edu)
"  Last Change: Sun, Nov 4, 2001

"        Usage: Normally, this file should reside in the plugins
"               directory and be automatically sourced. If not, you must
"               manually source this file using ':source TableTab.vim'.
"
"               You may use the default keymapping of
"
"                 <Leader><Tab> - Toggles between tab stops lookup and a
"                                 normal function of the <Tab> key
"
"               or you may want to remap it by adding something like the
"               following line to your _vimrc/.vimrc file.
"
"                 map <F9> <Plug>ToggleTableTab
"
"               When active, TableTab.vim adjusts the tab stops according to
"               the previous line, if this line is only of === or --- it will
"               check the line before.
"               Should be useful for editing tables.
"      Version: 0.1

if exists("loaded_TableTab")
  finish
endif
let loaded_TableTab = 1

if !hasmapto('<Plug>ToggleTableTab')
  map <unique> <Leader><Tab> <Plug>ToggleTableTab
endif
map <unique> <script> <Plug>ToggleTableTab :call <SID>ToggleTableTab()<CR>

if !exists("s:TableTabOn")
    let s:TableTabOn = 0
endif

function! <SID>ToggleTableTab()
  if s:TableTabOn
    iunmap <Tab>
    echo "TableTab is off"
  else
    imap <Tab> <esc>:call <SID>Table_tab()<CR>a
    echo "TableTab is on"
  endif
  let s:TableTabOn = !s:TableTabOn
endfunction

function! <SID>Table_tab()
  let save_sts=&sts
  let nr=line(".")
  let vcl=virtcol(".")
  if nr<2
    exe "normal! a\t\<esc>"
    return
  endif
  " here nr>=2
  if match( getline(nr-1), '^\s*[_=*\-]\+ *$' ) > -1
    normal! 2k W
    let nrp=nr-2
  else
    normal! k W
    let nrp=nr-1
  endif
  if line(".")==nrp
  " cursor stayed on the pattern line
    let &sts=virtcol(".")-1
  endif
  exe "normal! " . nr . "G" . vcl . "|a\t\<esc>"
  let &sts=save_sts
endfunction
