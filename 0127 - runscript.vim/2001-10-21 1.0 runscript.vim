" Vim global plugin for running Python scripts
" Version: 1.0
" Maintainer: Frederick Young <lordfyb@hotmail.com>
" Last change: 2001 Oct 21 
"
"*******************************************************************************
"
" --Pressing the "F12" key will run the Python script in the current buffer
"  
" --Pressing the "F11" key will designate the Python script in the current
"   buffer as the script to run when pressing "F12"
"   
" --Pressing "Shift-F11" will remove the designation performed by pressing "F11"
"   and will allow pressing "F12" to run the script in the current buffer
"   
" --Typing the Ex command "Rs" will also allow you to run a Python script but
"   with the benefit of allowing you to pass up to 20 parameters to the script.
"   
"   Sample syntax...
"
"   	:rs -xy 800 600
"
"   The above will run the script in the current buffer or the one designated
"   to run when pressing "F11" and pass to it the parameters "-xy 800 600"
"
" --Typing the Ex command "Sb" will display a buffer containing the output
"   produced when running the Ex command "Rs" or pressing "F12".
"   Unfortunately, I have not figured out how to capture compiler errors.  Oh
"   well.  Someday...
"
"   With some minor modifications, it should be possible to run other scripts
"   like perl, vbscript, or even executables compiled with the Ex command
"   "make".
"
"*******************************************************************************


" Change this variable to reflect the path of the Python executable on your
" system.  If your system knows how to find Python then setting this variable
" to 'python' should be enough.  Otherwise, type the complete path to the
" executable.
let s:PathToExecutable = 'c:\py21\python.exe'

" Used to designate the Python script to run when pressing "F12" or typing the
" Ex command "Rs"
let s:mainfile = ""

" Map keys to function calls
if !hasmapto('<Plug>RunScript')
  nmap <unique> <silent> <F12> <Plug>ExecuteScript
endif
if !hasmapto('<Plug>SetMainScript')
  nmap <unique> <silent> <F11> <Plug>SetMainScript
endif
if !hasmapto('<Plug>ClearMainScript')
  nmap <unique> <silent> <S-F11> <Plug>ClearMainScript
endif

" The main plug-in mapping.
nmap <script> <silent> <Plug>ExecuteScript :call <SID>ExecuteScript()<CR>
nmap <script> <silent> <Plug>SetMainScript :call <SID>SetMainScript()<CR>
nmap <script> <silent> <Plug>ClearMainScript :call <SID>ClearMainScript()<CR>

function! s:SetMainScript()
	let s:mainfile = bufname('%')
	echo s:mainfile . ' set as the starting program.'
endfunction

function! s:ClearMainScript()
	echo s:mainfile . ' is no longer the starting program.'
	let s:mainfile = ""
endfunction

function! s:ExecuteScript()
	" Execute script
	if strlen(s:mainfile) > 0
		let @a = system(s:PathToExecutable . ' ' . s:mainfile)
	else
		let @a = system(s:PathToExecutable . ' ' . bufname('%'))
	endif
endfunction


" Ex command which take 0 or more ( up to 20 ) parameters
command! -nargs=* Rs call ExecuteScript(<f-args>)

function ExecuteScript(...)
	let index = 1
	let s:params = ""
	" Assemble the paramters to pass to the Python script
	while index <= a:0
		execute 'let s:params = s:params . " " . a:' . index
		let index = index + 1
	endwhile
	" Execute script (with parameters if provided)
	if strlen(s:mainfile) > 0
		let @a = system(s:PathToExecutable . ' ' . s:mainfile . s:params)
	else
		let @a = system(s:PathToExecutable . ' ' . bufname('%') . s:params)
	endif
endfunction

" Ex command will show the output produced when calling the Ex command "Rs" or
" pressing "F12"
command! -nargs=0 Sb call ShowOutputBuffer()

" Display a buffer containing the contents of register "a"
function ShowOutputBuffer()
	new
	set buftype=nofile
	silent normal "aP
endfunction
