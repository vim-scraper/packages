" Document {{{
" Copyright: Copyright (C) 2005 Bruce Who
"            Permission is hereby granted to use and distribute this code,
"            with or without modifications, provided that this copyright
"            notice is copied with it. Like anything else that's free,
"            bwftmenu.vim is provided *as is* and comes with no
"            warranty of any kind, either expressed or implied. In no
"            event will the copyright holder be liable for any damages
"            resulting from the use of this software.
" Filename:  bwftmenu.vim
" Author:    Bruce Who (AKA. 胡旭昭)
" Email:     HuXuzhao at hotmail.com
" Date:      2005-07-11
" $Revision: 1.3 $
"
" Description:
"   You can use This script to help you make a menu which only appears when
"   you are editing buffers with a certain filetype. You can use this script
"   to make menus in your ftplugin scripts.
"
"   The menu is automatically made once you enter a buffer whose
"   filetype is specified. And if you switch to another buffer with a
"   different filetype, the previous menu is automatically deleted.
"
" Installation:
"   Just drop the script to your plugin directory.
"
" Prerequisite:
"   This script is written in pure vim script and does not depend on other
"   vim scripts.
"
" Usage:
"   In your ftplugin scripts, follow these steps:
"   1. write a function which creates a menu. The function should not
"      start with s: or <SID>.
"   2. call BW_RegisterFTMenu function which is the only function provided
"      by this script
"   That's all.
"
"   Here is an example. I want to create a menu only for python files,
"   so I create a function in my ftplugin/python.vim:
"
"   function MakeFTMenu_python
"     amenu 8000.11 Python.check :echo 'check python script'
"     amenu 8000.12 Python.run :echo 'run python script'
"   endfunction
"
"   and then, add this statement to the same .vim file:
"
"   call BW_RegisterFTMenu('python','Python','MakeFTMenu_python')
"
"   Note: the second parameter MUST be the same with the name of the menu
"         which you created in MakeFTMenu_python function.
"
" History:
"   2005-07-11 ~ 2005-07-12
"     Initial version is created
" }}}

if exists('g:bwftmenu') " {{{
  finish
endif
let g:bwftmenu = 1 " }}}

" register the menu-making function
" @filetype: the filetype your menu is for. 'c', 'python', 'vim'
" @menu_name: the name of the menu which is created by the function.
"             'Python menu', 'C menu'
" @makemenu_func_name: the name of the function which you use to create the
"                      menu. 'MakeFTMenu_python'
function BW_RegisterFTMenu(filetype,menu_name,makemenu_func_name)
  "" because ftplugin is loaded before BufEnter, so we could register
  "" functions in ftplugin files, then it will be executed when BufEnter
  if a:filetype == ''
    echo 'filetype cannot be empty! Fail to register!'
    return
  endif
  let s:make_menu_{a:filetype}=a:makemenu_func_name
  let s:menu_name_{a:filetype}=a:menu_name
endfunction

" Implement {{{

let s:prev_buf_filetype=-1

function s:BWEnterBuf()
  "" Once filetype changes, ...
  if s:prev_buf_filetype !=&filetype
    if s:prev_buf_filetype==-1
      "" vim opens the first empty buffer
      let s:prev_buf_filetype = ''
    elseif exists('s:menu_name_' . s:prev_buf_filetype)
      "" remove the filetype-menu for the previous buffer
      exe 'aunmenu ' . s:menu_name_{s:prev_buf_filetype}
    endif
    if exists('s:make_menu_' . &filetype)
      "" if filetype changes and the function is registered, we execute it.
      exec 'call ' . s:make_menu_{&filetype} . "()"
    endif
    let s:prev_buf_filetype = &filetype
  endif
endfunction

au BufEnter * call <SID>BWEnterBuf()

" }}}

" vim:fdm=marker fdl=0 fdc=3 fenc=utf-8:
