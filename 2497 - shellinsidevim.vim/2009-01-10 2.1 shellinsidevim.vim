" Vim global functions for running shell commands
" Version: 2.1
" Maintainer: WarGrey <juzhenliang@gmail.com>
" Last change: 2009 Jul 10
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

let s:Results=[]

" Ex command which take 0 or more ( up to 20 ) parameters
command -complete=file -nargs=* Shell call g:ExecuteCommand(<f-args>)
map <unique> <silent> <F4> :call g:ToggleOutputWindow()<CR>
map <silent> <C-F4> :messages<CR>
imap <unique> <silent> <F4> <ESC><F4>
imap <silent> <C-F4> <ESC><C-F4>

function g:ExecuteCommand(...)
	if a:0==0
		call g:EchoWarningMsg(s:GetCmdPreffix("")." <NOTHING TO EXECUTE>")
		return
	endif

	let index=1
	let msg=""
	let cmd=""
	while index<=a:0
		execute 'let para=g:Trim(a:'.index.')'
		if strlen(para)>0
			let msg=msg." ".para
			let cmd=cmd." ".substitute(expand(para),"\n"," ",'g')
		endif
		let index=index+1
	endwhile
	
	try
		if match(msg,"^ :")==0 || match(msg,"^ !")==0
			call s:ExecuteVimcmd(msg,cmd)
		else
			call s:ExecuteShell(msg,cmd)
		endif
	catch /.*/
		call g:EchoErrorMsg(v:exception)
	endtry
endfunction

function g:Trim(str)
	let str=substitute(a:str,"^\\s*","","g")
	let str=substitute(str,"\\s*$","","g")
	return str
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
		hi def shell ctermfg=green
		hi def command ctermfg=darkcyan
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

function g:ShowInOutputWindow()
	if bufloaded("VIM_STD_OUTPUT")>0
		silent! bwipeout VIM_STD_OUTPUT
	endif
	call g:ToggleOutputWindow()
endfunction

" Highlight echo
function g:EchoErrorMsg(msg)
	echohl ErrorMsg
	echo a:msg
	echohl None
endfunction

function g:EchoWarningMsg(msg)
	echohl WarningMsg
	echomsg a:msg
	echohl None
endfunction

function g:EchoMoreMsg(msg)
	echohl MoreMsg
	echomsg a:msg
	echohl
endfunction

" Some useful private functions
function s:GetCmdPreffix(type)
	return "[".a:type."@".fnamemodify(getcwd(),":~").":".fnamemodify(bufname('%'),":.")."]"
endfunction

function s:ExecuteVimcmd(vimmsg,vimcmd)
	let msg=substitute(substitute(a:vimmsg,"^ :"," ","g"),";$","","g")
	call g:EchoMoreMsg(s:GetCmdPreffix("ViM").msg)
	execute msg
endfunction

function s:ExecuteShell(shellmsg,shellcmd)
	let shellcmd=substitute(a:shellcmd,'\s*;*\s*$','','g')
	if match(a:shellmsg,"^ >")==0
		let shellcmd=substitute(shellcmd,"^ >"," ","g")
		if !filereadable('.VIM_STD_IN')
			let choice=confirm("Input-file not found, give now?","&Yes\n&No",1)
			if choice!=1
				call g:EchoWarningMsg("Missing inputs which are required, The application may be aborted!")
				call writefile([""],'.VIM_STD_IN')
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
	
	let @+=system(shellcmd)
	if v:shell_error!=0
		let error="Shell failed with the exit code ".v:shell_error
		call g:EchoWarningMsg(error)
	endif

	let cmd=s:GetCmdPreffix("SHELL").shellcmd
	if match(a:shellmsg,";$")>-1
		cexpr @+
	endif

	if &history>0 && len(s:Results)==&history
		call remove(s:Results,0)
	endif
	call add(s:Results,cmd."\n".@+)
	
	call g:ShowInOutputWindow()
	call g:EchoMoreMsg(cmd)
endfunction
