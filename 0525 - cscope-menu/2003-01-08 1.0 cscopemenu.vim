" Add_Cscope_Menu
"   Adds a cscope menu
"   All the commands work on the word that is under the cursor
function! s:Add_CScope_Menu(menu_clear)
    if has("gui_running")
		if (a:menu_clear)

			silent! unmenu &Cscope
			silent! unmenu! &Cscope
			amenu <silent> &Cscope.Find\ functions\ calling\ this\ function :cs find c <C-R>=expand("<cword>") <CR><CR>
			amenu <silent> &Cscope.Find\ functions\ called\ by\ this\ function :cs find d <C-R>=expand("<cword>") <CR><CR>
			amenu <silent> &Cscope.Find\ this\ egrep\ pattern :cs find e <C-R>=expand("<cword>") <CR><CR>
			amenu <silent> &Cscope.Find\ this\ file :cs find f <C-R>=expand("<cword>") <CR><CR>
			amenu <silent> &Cscope.Find\ this\ definition :cs find g <C-R>=expand("<cword>") <CR><CR>
			amenu <silent> &Cscope.Find\ files\ #including\ this\ file :cs find i <C-R>=expand("<cfile>") <CR><CR>
			amenu <silent> &Cscope.Find\ this\ Symbol :cs find s <C-R>=expand("<cword>") <CR><CR>
			amenu <silent> &Cscope.Find\ assignments\ to :cs find t <C-R>=expand("<cword>") <CR><CR>
		endif
    endif

endfunction


autocmd BufEnter * call s:Add_CScope_Menu(1)
