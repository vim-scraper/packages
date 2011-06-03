" CleverTabs v1.0: Martin Spevak 2008
" Script solving indentation problem. Main idea is
" have tabs ("\t") at the beginning of line and 
" after first character using spaces (like expandtab).
" This is usefull for code readability.
"
" main() 
"     printf("hello");    //my comment
"     printf("world!");   //my comment 2
" }
" 
" Before printf are tabs (this is block) a before comments
" are spaces. After you change shiftwidth part of comments
" will have still same format, becuase tabs are only on
" line beginnig.
"
" instalation:
" paster this function into .vimrc and also insert next commands:
"
" nnoremap \hb :call CleverTabs(4)<CR>
" imap <Tab> <Space><BS><Esc>\hba
"
" note:    parameter for CleverTabs is shiftwidth in spaces
" note2:   this is my first script in vim, please be tolerant


" clever tabs (tabs only on the line beginning)
function! CleverTabs(shiftwidth)
	let line = getline('.')[:col('.')-1]
	if col('.') == 1 || line =~ '^\t*$' || line =~ '^$'
		let @z = "\t"
	else
		let space = ""
		let shiftwidth = a:shiftwidth
		let shiftwidth = shiftwidth - (col('.') % shiftwidth)

		while shiftwidth > 0
			let shiftwidth = shiftwidth - 1
			let space = space . ' '
		endwhile

		let @z = space
	endif

	"putting space depends if we are on beginning of the string on the line
	if col('.') == 1 && getline('.')[col('.')-1:col('.')-1] != "\t"
		normal "zP
	else
		normal "zp
	endif
endfunction "CleverTabs
