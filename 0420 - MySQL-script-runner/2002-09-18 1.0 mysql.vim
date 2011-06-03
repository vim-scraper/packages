" MySQL helper console tools v1.0
"
" Maintainer:   Lukas Zapletal 
" Email:        <lzap@bach.cz>
" URL:          http://www.vim.org/scripts.php
" Last Change:  2002 Oct 18
"
" Help:
" MySQL scripts that allows you to run SQL scripts in mysql console by pressing F9.
" 
" You can add this plugin to handle *.SQL file type.
"

" HELPER FUNCTIONS
function! Sql_run(console)
	" This function gets database name to run on and runs the SQL file on it.
	" It must be defined on first 10 lines:
	"
	" -- DB: database_name
	"

	" find filename if exists
	let lnum = 1
	while lnum < 50
		let line = getline(lnum)
		if line =~ '^\s*--\s*DB:.*'
			let database = substitute(line,'^\s*--\s*DB:\s*','','')
			execute "!" . a:console . " -t " . database . " < " . expand("%")
			return
		endif
		let lnum = lnum + 1
	endwhile
	" no database selected, just run without it
	execute "!" . a:console . " < " . expand("%")
endfunction

" MENU
imenu &MySQL.&Execute<TAB>F9 <F9>
nmenu &MySQL.&Execute<TAB>F9 <F9>

" COMMANDS

if has("win32") || has("win16")
	nmap <F9> :w<CR>:call Sql_run("c:\\mysql\\bin\\mysql")<CR>
else
	nmap <F9> :w<CR>:call Sql_run("mysql")<CR>
endif

imap <F9> <ESC><F9>

" EOF
