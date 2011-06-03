" Vim global functions for running shell commands
" Version: 2.2
" Maintainer: WarGrey <juzhenliang@gmail.com>
" Last change: 2009 Mar 04
"
"*******************************************************************************
"
" --Typing the Ex command "Shell" will also allow you to run a Shell which you
"   give and pass up to 20 parameters to that shell.
"
"   Sample syntax...
"       :Shell ps -e
"
" --Pressing "F4" will toggle the display of a buffer containing the output
"   produced when running the shell command.
"
" **************************Some special syntax format**************************
"
" --If a Shell command starts with ":", this Shell will be executed as a vim Ex
"  command, this is convenience if you map this command to a shortcut such as:
"
"  map <CR> :Shell
"
"  Then you could use this script to execute all the normal command directly.
"
"  --If a Shell command starts with ">", this Shell will be executed with inputs
"  which come from a file which name is ".VIM_STD_IN". If the input file is not 
"  exsits, you could give the inputs line by line by typing directly.
"
"  --If a Shell command ends with ";", this Shell will be executed as a program
"  development tool. You could use this script for your development, then you 
"  could pass the compile or interpret command as the Shell parameters with ";"
"  followed. When the command finished it will jump to the first error line if
"  there are some errors. Of caurse you should special a compiler first.
"
"*******************************************************************************

if exists("g:load_shellinsidevim") && g:load_shellinsidevim==1
	finish
endif
let g:load_shellinsidevim=1

if !exists("g:AutoShowOutputWindow")
	let g:AutoShowOutputWindow=0
endif
if !exists("g:ShowOutputInCommandline")
	let g:ShowOutputInCommandline=0
endif

let s:Results=[]

" Ex command which take 0 or more ( up to 20 ) parameters
command -complete=file -nargs=* Shell call g:ExecuteCommand(<f-args>)
map <unique> <silent> <F4> :call g:ToggleOutputWindow()<CR>
map <silent> <C-F4> :messages<CR>
imap <unique> <silent> <F4> <ESC><F4>a
imap <silent> <C-F4> <ESC><C-F4>a

function g:ExecuteCommand(...)
	if a:0==0
		call g:EchoWarningMsg(s:GetCmdPreffix("")." <NOTHING TO EXECUTE>")
		return
	endif

	try
		let parms=map(deepcopy(a:000),'g:Trim(v:val)')
		let msg=join(parms,' ')
		let cmd=join(map(parms,'substitute(expand(v:val),"\n"," ","g")'),' ')
		if msg=~'^:'
			call s:ExecuteVimcmd(msg,cmd)
		else
			call s:ExecuteShell(msg,cmd)
		endif
	catch /.*/
		call g:EchoErrorMsg(v:exception)
	endtry
endfunction

" Display a buffer containing the contents of s:Results
function g:ToggleOutputWindow()
	if bufloaded("VIM_STD_OUTPUT")==0 
		if strlen(&buftype) > 0 && bufname("%") != "VIM_STD_OUTPUT"
			call g:EchoWarningMsg("This buffer does not have the output windows!")
			return
		endif
		
		let this=bufwinnr("%")
		let @r=join(s:Results,"").s:GetCmdPreffix("SHELL")."  "
		silent! rightbelow new VIM_STD_OUTPUT
		syntax match shell "^\[SHELL@.*\].*$" contains=command
		syntax match command "\s.*$" contained
		syntax match interrupt "^\s*Vim:Interrupt\s*$"
		hi def shell ctermfg=green
		hi def command ctermfg=darkcyan
		hi def interrupt ctermfg=red
		resize 8
		setlocal buftype=nofile
		setlocal readonly 
		silent normal "rP
		execute 'silent normal '.(1+len(split(@r,"\n"))).'gg$'
		setlocal nomodifiable
		execute this."wincmd w"
	elseif bufloaded("VIM_STD_OUTPUT") > 0
		silent! bwipeout VIM_STD_OUTPUT
	endif
endfunction

" Some useful private functions
function s:GetCmdPreffix(type)
	return "[".a:type."@".fnamemodify(getcwd(),":~").":".fnamemodify(bufname('%'),":.")."] "
endfunction

function s:ExecuteVimcmd(vimmsg,vimcmd)
	let msg=substitute(a:vimmsg,'\(\s*;\s*$\)','','g')
	call g:EchoMoreMsg(s:GetCmdPreffix("ViM").msg)
	execute msg
endfunction

function s:ExecuteShell(shellmsg,shellcmd)
	let shellcmd=substitute(a:shellcmd,'\s*;*\s*$','','g')
	if match(a:shellmsg,'^\s*>>\s*')==0
		let shellcmd=substitute(shellcmd,'^\s*>>\s*','','g')
		if !filereadable('.VIM_STD_IN')
			call writefile([],'.VIM_STD_IN')
		endif
		let shellcmd.=' < .VIM_STD_IN'
	elseif match(a:shellmsg,'^\s*>\s*')==0
		let shellcmd=substitute(shellcmd,'^\s*>\s*','','g')
		if !filereadable('.VIM_STD_IN')
			let choice=confirm("Input-file not found, give now?","&Yes\n&No",1)
			if choice!=1
				call g:EchoWarningMsg("Missing inputs which are required, The application may be aborted!")
				call writefile([],'.VIM_STD_IN')
			else
				echo 'Pease give the inputs line by line util "EOF" gave.'
				let lines=[]
				let line=input("")
				while line != "EOF"
					call add(lines,line)
					let line=input("")
				endwhile
				call writefile(lines,'.VIM_STD_IN')
			endif
			echo "Running..."
		endif
		let shellcmd.=' < .VIM_STD_IN'
	endif
	
	let cmd=s:GetCmdPreffix("SHELL").shellcmd
	call g:EchoMoreMsg(cmd)
	try
		let @+=substitute(system(shellcmd),'\n*$','\n','g')
	catch /.*/
		let @+=v:exception."\n"
	endtry
	let @+=substitute(@+,"^\n$",'','g')
	if v:shell_error!=0
		let error="Shell failed with the exit code ".v:shell_error
		call g:EchoWarningMsg(error)
	endif

	if &history>0 && len(s:Results)==&history
		call remove(s:Results,0)
	endif
	call add(s:Results,cmd."\n".@+)
	
	if g:AutoShowOutputWindow || bufloaded("VIM_STD_OUTPUT")>0
		if bufloaded("VIM_STD_OUTPUT")>0
			silent! bwipeout VIM_STD_OUTPUT
		endif
		call g:ToggleOutputWindow()
	endif
	if g:ShowOutputInCommandline
		echo @+
	endif
	
	if match(a:shellmsg,'\s*;\s*$')>-1
		cexpr @+
	endif
endfunction

call g:ToggleOutputWindow()
