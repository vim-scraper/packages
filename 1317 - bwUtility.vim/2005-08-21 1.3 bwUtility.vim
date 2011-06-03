" Document {{{
" Copyright: Copyright (C) 2005 Bruce Who
"            Permission is hereby granted to use and distribute this code,
"            with or without modifications, provided that this copyright
"            notice is copied with it. Like anything else that's free,
"            bwUtility.vim is provided *as is* and comes with no
"            warranty of any kind, either expressed or implied. In no
"            event will the copyright holder be liable for any damages
"            resulting from the use of this software.
" Filename:  bwUtility.vim
" URL:       http://vim.sourceforge.net/scripts/script.php?script_id=1317
" Author:    Bruce Who (AKA. 胡旭昭 or HuXuzhao)
" Email:     HuXuzhao at hotmail.com
" Date:      2005-07-25
" $Revision: 1.3 $
" Description:
"   This script provides following trivial functionalities you can used
" in your own scripts:
"   - open a console in the current working directory.
"   - remove trailing spaces.
"   - insert date. This is only available via menu entry.
" Installation:
"   Drop the script to your plugin directory.
" Prerequisite:
"   This script needs
" Usage:
"   Once the script is installed, it automatically add a menu named
" 'bwUtility' via which you can access all functins of this script.
"   If you want to change the position of the menu, you just need to change
" this global variable:
"   g:bwUtility_menu_id
" Todo:
"   More useful trivial functionalities will be added. If you need any other
" useful trivial functionalities, please contact me!
" }}}

if exists('g:bwUtility') " {{{
  finish
endif
let g:bwUtility = 1 " }}}

" gobal setting {{{
let g:bwUtility_menu_id=7000
" }}}

" functions {{{

function BWUtility_open_console()
  if has('win32')
    !start cmd
  " else
  "   " TODO: how to start a shell console for Linux?
  endif
endfunction

function BWUtility_trim_trailing_space()
  let line_count = line('$')
  let i = 1
  while i <= line_count
    call setline(i, substitute(getline(i),'\s*$','',''))
    let i = i + 1
  endwhile
  " we can also use this code, but I prefer functions to commands in scripts
  " %s/\s*$//
  " nohl
endfunction

" break long lines according to 'textwidth'
function BWUtility_auto_breaklines()
  let temp=&formatoptions
  set formatoptions=tcqn2mMB
  normal gggqG
  let &formatoptions=temp
endfunction

" }}}

" menu {{{
exe "amenu " . g:bwUtility_menu_id . '.11 bwUtility.Open\ Console ' .
             \ ':call BWUtility_open_console()<CR>'
exe "amenu " . g:bwUtility_menu_id . '.12 bwUtility.Trim\ Trailing\ Space ' .
             \ ':call BWUtility_trim_trailing_space()<CR>'
exe "amenu " . g:bwUtility_menu_id . '.13 bwUtility.Breaklines ' .
             \ ':call BWUtility_auto_breaklines()<CR>'
exe "amenu " . g:bwUtility_menu_id . '.14 bwUtility.Breaklines\ for\ All ' .
             \ ':bufdo call BWUtility_auto_breaklines()<CR>'
exe "imenu " . g:bwUtility_menu_id . '.15 bwUtility.Insert\ Date ' .
             \ '<C-R>=strftime("%Y-%m-%d")<CR>'
" }}}

" Modeline for ViM {{{
" vim:fdm=marker fdl=0 fdc=3 fenc=utf-8:
" }}} */
