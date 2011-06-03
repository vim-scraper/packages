" 'perl -cw' current buffer into a new window
function BufferPerlCW()
	let l:tmpfile1 = tempname()
	let l:tmpfile2 = tempname()

	execute "normal:w!" . l:tmpfile1 . "\<cr>"
	execute "normal:! perl -cw " . l:tmpfile1 . " \> " . l:tmpfile2 . " 2\>\&1 \<cr>"
	execute "normal:new\<cr>"
	execute "normal:edit " . l:tmpfile2 . "\<cr>"
endfunction

" 'pychcker' current buffer into a new window
function BufferPyChecker()
	let l:tmpfile1 = tempname() . '.py'
	let l:tmpfile2 = tempname()

	execute "normal:w!" . l:tmpfile1 . "\<cr>"
	execute "normal:! pychecker " . l:tmpfile1 . " \> " . l:tmpfile2 . " 2\>\&1 \<cr>"
	execute "normal:new\<cr>"
	execute "normal:edit " . l:tmpfile2 . "\<cr>"
endfunction

" 'perl' current buffer into a new window
function BufferPerl()
	let l:tmpfile1 = tempname()
	let l:tmpfile2 = tempname()

	execute "normal:w!" . l:tmpfile1 . "\<cr>"
	execute "normal:! perl " . l:tmpfile1 . " \> " . l:tmpfile2 . " 2\>\&1 \<cr>"
	execute "normal:new\<cr>"
	execute "normal:edit " . l:tmpfile2 . "\<cr>"
endfunction

" 'python' current buffer into a new window
function BufferPython()
	let l:tmpfile1 = tempname()
	let l:tmpfile2 = tempname()

	execute "normal:w!" . l:tmpfile1 . "\<cr>"
	execute "normal:! python " . l:tmpfile1 . " \> " . l:tmpfile2 . " 2\>\&1 \<cr>"
	execute "normal:new\<cr>"
	execute "normal:edit " . l:tmpfile2 . "\<cr>"
endfunction

" set the new fold function, etc
function MyPerlSettings()
	noremap <f1> <esc>:call BufferPerlCW()<cr>
	noremap <f2> <esc>:q<cr>
	noremap <f3> <esc>:call BufferPerl()<cr>
endfunction

function MyPythonSettings()
	noremap <f1> <esc>:call BufferPyChecker()<cr>
	noremap <f2> <esc>:q<cr>
	noremap <f3> <esc>:call BufferPython()<cr>
endfunction

au BufRead,BufNewFile *.pl,*.pm,*.cgi call MyPerlSettings()
au BufRead,BufNewFile *.py            call MyPythonSettings()

