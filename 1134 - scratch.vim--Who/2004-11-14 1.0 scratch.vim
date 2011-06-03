" File: scratch.vim {{{
"
" English {{{
" Author: Bruce Who ( HuXuzhao at hotmail.com )
" Version: 1.0
" Last Update: 2004-11-13
" Initial: 2004-05-10
" Copyright (c) 2004 by Bruce Who
"
" Description: this script creates a scratch buffer for you, you can write
" anything there, but its content will not be saved, so DONT put any
" important things in scrath buffer! Several commands and keymaps will
" help you.
"     this scratch.vim is part of Bruce Who's vim package which is not fully
" finished.
"
" License:
" This program is free software; you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation; either version 2 of  the  License,  or
" (at your option) any later version.
" [ http://www.gnu.org/licenses/gpl.html ]
" 
" This program is distributed in the hope that it will be useful,  but
" WITHOUT  ANY  WARRANTY;  without  even  the  implied   warranty   of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR  PURPOSE.  See  the  GNU
" General Public License for more details.
" 
" You should have received a copy of the GNU  General  Public  License
" along with  this  program;  if  not,  write  to  the  Free  Software
" Foundation,  Inc.,  59  Temple  Place  -  Suite  330,   Boston,   MA
" 02111-1307, USA.
"
" Installation:	Drop it into one of your plugin directories
"
" Usage:
"   three commands:
"     ToggleScratch   toggle the scratch buffer
"     SplitScratch    split window to show the scratch buffer
"     AddtoScratch    append the text from register " to the scratch buffer,
"                     separated by a blank line
"   default keymaps:
"     <F4>s           ToggleScratch
"     <F4><S-s>       SplitScratch
"     <F4><C-s>       AddtoScratch
"
" Todo:
"    I've no ideas yet, :-<. Any suggestions are appreciated, :-)
"    - add a command to set how to put the register " to scrath buffer
" Bug:
"    - when scratch buffer is open, then you open another buffer,you need to
"      type <F4>s twice to get scratch buffer again.
"
" }}}
"
" 中文(Chinese) {{{
"  作者：胡旭昭 ( HuXuzhao at hotmail.com )
"  版本：1.0
"  最近的修改：2004-11-13
"  初版：2004-05-10
"  Copyright (c) 2004 胡旭昭
"
"  描述：此脚本用于创建一个scratch缓冲区，你可以在这里打草稿
"        写一些临时的内容而不必创建一个文件，呵呵。但是退出vim
"        后这个缓冲区的内容会消失，所以不要在这里写一些重要的
"        东西！这里还定义了一些命令和快捷键。
"        
"        这个scratch.vim是在下的vim宏包的一部分（目前还未完成）。
"
"  版权：遵守GNU GPL(http://www.gnu.org/licenses/gpl.html)
"
"  安装：丢到plugin目录下就行了
"
"  使用：
"    三个命令：
"     ToggleScratch   开关scratch缓冲区
"     SplitScratch    用split window开关scratch缓冲区
"     AddtoScratch    把"寄存器的内容存入scratch缓冲区，用空
"                     行分隔
"    默认的快捷键：
"     <F4>s           ToggleScratch
"     <F4><S-s>       SplitScratch
"     <F4><C-s>       AddtoScratch
"
" 下一步：
"   还没新想法。大家有什么想法可以告诉我。
"    - 在插入模式下调scratch窗口，而在normal mode下用<S-F6>配置，例如每次粘贴空几格的问题，或粘贴是是否有外部的filter。
"    
" Bug:
"    - 当scratch buffer打开时，用n创建了新的buffer，此时在新
"      buffer中要两次<F4>s才能打开scratch缓冲区。
"    }}}
" 
" Happy Vimming!
"
" }}}

if exists('g:whose_scratch_loaded')  "{{{
    finish
endif
let g:whose_scratch_loaded=1
"}}}

" Commands {{{
" toggle the scratch window
command! -nargs=0 ToggleScratch call s:ScratchBufferToggle(1)
" toggle the scratch buffer in a new split window
command! -nargs=0 SplitScratch call s:ScratchBufferToggle(0)
" append register " to buffer
command! -nargs=0 AddtoScratch call s:ScratchBufferAppend('+')
" }}}

" key maps, you can modify the keymaps here {{{
nnoremap <F4>s :ToggleScratch<CR>
nnoremap <F4><S-s> :SplitScratch<CR>
nnoremap <F4><C-s> :AddtoScratch<CR>
" }}}

" script variables {{{
" Scratch buffer name
let s:ScratchBufferName = "Scratch"
" not exist
let s:scratch_number = -1  
" not visuable
let s:visuable = 0 
" empty
let s:scratch_empty = 1
" }}}

function! s:ScratchBufferNew() " {{{
  if s:scratch_number == -1
    let curb = bufnr('')
    silent! exe 'edit ' . s:ScratchBufferName
    let s:scratch_number = bufnr(s:ScratchBufferName)
    let s:visuable = 0
    let s:scratch_empty = 1
    set buftype=nofile
    set bufhidden=hide
    set noswapfile
    set nobuflisted
    exe curb . "buffer"
  endif
endfunction
" }}}

function! s:ScratchBufferToggle(flag)  " {{{
" open the scratch buf
" flag : 0 split
"        1 not split
  let curb = bufnr('')
  call s:ScratchBufferNew()
  if s:visuable == 1
    if curb == s:scratch_number
      silent! bn
    else
      exe curb . 'buffer'
    endif
    if bufnr('') != s:scratch_number
      let s:visuable = 0
    endif
  else  "if s:visuable == 0 
    if a:flag == 0
      exe 'split +' . s:scratch_number . 'buffer'
      resize 10
    else
      exe s:scratch_number . 'buffer'
    endif
    let s:visuable = 1
  endif
endfunction
"}}}

function! s:ScratchBufferAppend(reg)  " {{{
  if s:scratch_number == -1
    call s:ScratchBufferNew()
  endif
  let curb = bufnr('')
  exe s:scratch_number . "buffer"
  normal G
  if s:scratch_empty == 0
    normal o
    exe 'normal "' . a:reg . 'p'
  else
    exe 'normal "' . a:reg . 'p'
    " !!! remove the extra line after pasting
    if line('.') == 2
      1delete _
    endif
    let s:scratch_empty = 0
  endif
  exe curb . "buffer"
endfunction
" }}}

" Modeline for ViM {{{
" vim600:fdm=marker fdl=0 fdc=2:
" }}}
