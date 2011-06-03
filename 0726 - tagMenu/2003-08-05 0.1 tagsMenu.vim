" File: tagsMenu.vim
" Author: Aditya Mahajan (adityam AT umich DOT edu)
" Version: 0.1
" Last Modified: Aug 3, 2003
"
" Overview
" --------
" Sometimes when the size of tag file is large, and you are searching for a
" particular tag, it is cumbersome to ^N through the list of tags. This plugin
" is aimed to solve this. A hidden menu containing the list of tags is
" created. This can be accessed using CTRL-K, or any other mapping that you
" want. (I prefer the CTRL mapping and K is one of 2 keys not already mapped.
" The other being Q)
" 
" Installation
" ------------
"  1. Copy the tagsMenu.vim script to $HOME/.vim/plugin directory. Refer to
"  ':help add-plugin', ':help add-global-plugin', and ':help runtimepath' for
"  more details.
"  2. Restart Vim
"  3. Use ":Tags" command to create the menu of tags.
"
"  Usage
"  -----
"  Use ":Tags" command to create the menu of tags.
"  Use <C-K> (CTRL-K) to access this menu. You can map this to some other key
"  if you want.
"  
"  Configuration
"  -------------
"  If you do not want to load the plugin add
"	      let tagmenuLoaded=1
"  in your .vimrc
"
"  TODO 
"  If a tag is partially incomplete show a menu that contains only those tags
"  that match the begining pattern. 
"
if exists('tagmenuLoaded')
  finish
endif

let tagmenuLoaded=1
let g:menuname = "]Tags"

"Mappings to access the menu
map <silent> <C-K> :popup ]Tags<CR>
command! Tags call TagMenu()
function! TagMenu()
  split tags       "Open the tags file
  0d               "Delete the first line 
  let total = line("$")
  "Delete the menu if already existing
  exe 'amenu '.g:menuname.".x x"
  exe 'aunmenu '.g:menuname
  while total > 0
      normal dej		   "Delete the first word
      let label=@-	   "Contents of the delete register
      exe 'amenu <silent> '. g:menuname . "." . label . " " .  "\"='" . label . "'<CR>P"
      let total = total - 1
  endwhile
  bdelete! 
endfunction
