" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
/root/.vim/ftplugin/grsecurity_ftplugin.vim	[[[1
727
" Vim filetype plugin file
" Language:	grsecurity
" Maintainer:	Marcin Szamotulski
" Last Changed: 2010 Mar 12
" URL:		
" Copyright:    Copyright (C) 2010 Marcin Szamotulski Permission is hereby
"		granted to use and distribute this code, with or without
" 		modifications, provided that this copyright notice is copied
" 		with it. Like anything else that's free, Automatic TeX Plugin
" 		is provided *as is* and comes with no warranty of any kind,
" 		either expressed or implied. By using this plugin, you agree
" 		that in no event will the copyright holder be liable for any
" 		damages resulting from the use of this software. 
" 		This licence is valid for all files distributed with
" 		grsecurity ftplugin.
"
" ToDo: Search_of_Subject doesn't work where subject is not enclosed in { }. 
"
" to install this plugin: 
"  	(1) copy this file to $HOME/.vim/ftplugin
"	(2) put in your vimrc file
" 		au BufRead /etc/grsec/policy set filetype=grsecurity
" there is a syntax file for /etc/grsec/policy file written by Jochen Bartl: 
" http://verbosemo.de/~lobo/files/grsecurity.vim
"
" List of Functions:
" SHOW DMESG
"          Show_Dmesg		command :Dmesg		
"          Show_Previous_Logs	command :PL
"          Show_Next_Logs	command :NL
"
" SEARCHING functions/commands/mappings:
"          Search_for_Subject	command :S 	
"          Search_in_Subject	command :SS		
"          Search_in_Role	command :SR
"          Search_for_Object	command :SO
"          Search_i_Flag 	command :Si
"          Search_for_Role 	command :R
"
" MOVING functions/commands/mappings:
"          Top_of_Subject	command :TS
" 				map {
"          Bottom_of_Subject	command :BS
" 				map }
"          Next_Subject		command :NS
" 				map ]
"          Previous_Subject 	command :PS	
"				map [
"          Next_Role		command :NR	
"				map (
"          Previous_Role	command :PR
"				map (
"
" Some function have special arguments which are not explained see doc file.

if !exists("g:gradm_learning_log_file")
    let g:gradm_learning_log_file='/etc/grsec/learning.log'
endif
if !exists("g:gradm_output_policy")
    let g:gradm_output_policy='/etc/grsec/policy.add'
endif

if !exists("g:ignore_grsec_logs")
    let g:ignore_grsec_logs='successful change to special role admin\|
		\RBAC system \(re\)\?loaded\|
		\special role admin failure\|
		\special role admin (.*) exited\|
		\unauth of special role admin\|
		\terminal being sniffed by IP:0\.0\.0\.0\|
		\grsec: shutdown auth success for'
" 		\grsec: .* mount of
" 		\grsec: .* unmount of'
"
endif

"search for subject anywhere, or in the specified role
function! Search_for_Subject(pattern,...)
    let l:pattern='^\s*subject\s.\{-}' . a:pattern 
    ". '\>\(\s\|$\)'
    if a:0 == 0
	exe '/' . l:pattern
    else
	call Search_in_Role(l:pattern,a:1)
    endif
endfunction

function! Search_i_Flag()
    let l:pattern='^\s*subject\s\+\/\%(\w\|\/\|\.\|-\)*\s\+\<[hvpkldbotrAKCToOa]\{-}i[hvpkldbotrAKCToOa]*\>'
    exe '/' . l:pattern	
endfunction

function! Search_for_Role(what)
    let l:pattern='^\s*\%(role\).\{-}' . a:what . '\>'
    exe '/' . l:pattern
endfunction

function! Top_of_Subject(...)
    let l:true=getline('.') !~ '^\s*subject\>' 
    let l:line=line('.')
    while l:true && l:line > 1
	let l:line-=1
	let l:true=getline(l:line) !~ '^\s*subject\>'
    endwhile
    if a:0 == 0
	call setpos('.',[bufnr("%"), l:line, 1, 0])
    else
	return l:line
    endif
endfunction

function! Next_Subject() 
    let l:line=line('.')+1
    while getline(l:line) !~ '^\s*subject\>' && l:line <= line('$')
	let l:line+=1
    endwhile
    call setpos('.',[bufnr("%"), l:line, 1, 0])
    exe 'normal zt'
endfunction

function! Next_Role(...) 
    let l:line=line('.')+1
    while getline(l:line) !~ '^\s*role\>' && l:line <= line('$')
	let l:line+=1
    endwhile
    if a:0 == 0
	call setpos('.',[bufnr("%"), l:line, 1, 0])
	exe 'normal zt'
    else
	return l:line
    endif
endfunction

function! Previous_Subject() 
    let l:line=line('.')-1
    while getline(l:line) !~ '^\s*subject\>' && l:line > 1
	let l:line-=1
    endwhile
    call setpos('.',[bufnr("%"), l:line, 1, 0])
    exe 'normal zt'
endfunction

" this finds the top of the current role
function! Previous_Role(...) 
    if line('.') == 1
	return 1
    endif
    let l:line=line('.')-1
    while getline(l:line) !~ '^\s*role\>' && l:line > 1
	let l:line-=1
    endwhile
    if a:0 == 0
	call setpos('.',[bufnr("%"), l:line, 1, 0])
	exe 'normal zt'
    else
	return l:line
    endif
endfunction

function! Bottom_of_Subject(...)
    let l:true=getline('.') !~ '^\s*}\s*$' 
    let l:line=line('.')
    while l:true && l:line < line('$')
	let l:line+=1
	let l:true=getline(l:line) !~ '^\s*}\s*$'
    endwhile
    if a:0 == 0
	call setpos('.',[bufnr("%"), l:line, 1, 0])
    else
	return l:line
    endif
endfunction

" Search in the current subject or in: 
" a:1 = subject
" a:2 = role
function! Search_in_Subject(pattern,...)
    let l:pos=getpos('.')

    if a:0 >= 2
	"if we are not in the role a:2
	if Echo_Role_Subject(1)[0] != a:2
	    call Search_for_Subject(a:1,a:2)

	"if we are in the role a:2, but not in the subject wich mathes a:1
	elseif len(Echo_Role_Subject(1)) == 2 && Echo_Role_Subject(1)[1] !~ a:1
	    call Search_for_Subject(a:1,a:2)
	endif
    endif

    let l:bottom_line=Bottom_of_Subject(1)
    let l:return=search(a:pattern,'sW',l:bottom_line)

    " if we hit bottom continue at top of the subject
    if l:return == 0
	keepjumps call Top_of_Subject()

	" echo warning message if hit bottom 
	echoh WarningMsg
	echomsg "Search hit BOTTOM of the subject, continuing at TOP of the subject"
	echoh None
	let l:return=search(a:pattern,'sW',l:bottom_line)
	if l:return == 0
	    call setpos('.',l:pos)
	endif
    endif
endfunction

" search for pattern in role (default in the current role)
function! Search_in_Role(pattern,...)

    " get the current position
    let l:pos=getpos('.')

    " go to the role a:1
    if a:0 != 0 && Echo_Role_Subject(1)[0] != a:1
	keepjumps call Search_for_Role(a:1)
    endif

    let l:bottom_line=Next_Role(1)-1
    let l:return=search(a:pattern,'sW',l:bottom_line)
    if l:return == 0

	" if we hit bottom continue at top of the role
	if getline('.') !~ '^\s*role\>'
	    keepjumps call Previous_Role()

	    " echo warning message if hit bottom 
	    echoh WarningMsg
	    echomsg "Search hit BOTTOM of the role, continuing at TOP of the role"
	    echoh None
	else
	    " set the begining position if nothing has been found
	    " and issue a warning message
	    call setpos('.',l:pos)
	    if a:0 == 0
		let l:msg=""
	    else
		let l:msg=" in the role: " . a:1
	    endif
	    echohl WarningMsg
	    echomsg "Pattern: '" . a:pattern . "' not found" . l:msg
	    echohl None
	    return 0
	endif
	let l:return=search(a:pattern,'sW',l:bottom_line)
	if l:return == 0 

	    " set the begining position if nothing has been found
	    " and issue a warning message
	    call setpos('.',l:pos)
	    if a:0 == 0
		let l:msg=""
	    else
		let l:msg=" in the role " . a:1
	    endif
	    echohl WarningMsg
	    echomsg "Pattern " . a:pattern . " not found" . l:msg
	    echohl None
	endif
    endif
endfunction

function! Role_Compl(A,P,L,...)

    let l:roles=[]
    let l:roles_lines=getbufline(bufname('%'),1,'$')

    " filter out not matching lines
    call filter(l:roles_lines, 'v:val =~ "\\s*role\\s"')

    for l:role in l:roles_lines
	call add(l:roles,substitute(substitute(l:role,'#.*$','',''),'^\s*role\s\+\(\%(\w\|_\|\.\|-\)*\)\%(\s\+[ANsugGTlP]*\)\?\s*$','\1',''))
    endfor

    " we are not sorting the results as probably roles appears in a logical
    " order
    
    let l:returnlist=[]
    if a:0 == 0
	for l:role in l:roles
	    if l:role =~ '^' . a:A 
		call add(l:returnlist,l:role)
	    endif
	endfor
	return l:returnlist
    else
	echo join(l:roles,"\n")
    endif
endfunction

function! Role_Subject_Compl(A,P,L)
    let l:roles=Role_Compl(a:A,a:P,a:L)

    let l:subjects=[]
    let l:subject_lines=readfile(fnamemodify(bufname('%'),':p'))

    " filter out not matching lines
    call filter(l:subject_lines, 'v:val =~ "\\s*subject\\s"')

    for l:subject in l:subject_lines
	call add(l:subjects,fnamemodify(substitute(substitute(l:subject,'#.*$','',''),'^\s*subject\s\+\(\S\+\)\s\+.*','\1',''),":t"))
    endfor
    "
    " we are not sorting the results as probably roles appears in a logical
    " order
    
    let l:roles_and_subjects=l:roles+l:subjects

    let l:returnlist=[]
    for l:subject_or_role in l:roles_and_subjects
	if l:subject_or_role =~ '^' . a:A 
	    call add(l:returnlist,l:subject_or_role)
	endif
    endfor
    return l:returnlist
endfunction

function! Echo_Role_Subject(...)
" 	let l:pos=getpos('.')
    let l:sline=Top_of_Subject(1)
    if getline('.') =~ '^\s*role\>' 
	let l:rline=line('.')
    else
	let l:rline=Previous_Role(1)
    endif
    let l:subject=substitute(substitute(getline(l:sline),'#.*$','',''),'^\s*subject\s\+\(\/\%(\w\|\/\|\.\|-\|_\)*\)\s\+[hvpkldbotrAKCToOa]*\s*{\?\s*$','\1','')
    let l:role=substitute(substitute(getline(l:rline),'#.*$','',''),'^\s*role\s\+\(\%(\w\|_\|\.\|-\)*\)\%(\s\+[ANsugGTlP]*\)\?\s*$','\1','')
" 	echo l:role . "  " . l:subject 
	
    if l:rline <= l:sline && a:0 == 0
	return l:role . "  " . l:subject 
    elseif a:0 == 0
	return l:role
    endif
    if l:rline <= l:sline && a:0 != 0
	return [l:role, l:subject]
    elseif a:0 !=0
	return [l:role]
    endif
endfunction

" search for object [flag]
function! Search_for_Object(object,...)
    if a:0==0
	call search('^\s*' . a:object)
    else

	if a:0 >= 1
	    " flags which must appear
	    if a:1 != 'any' || a:1 == 'all'
		let l:a_flags=split(a:1,'\zs')
		let l:any_flag=0
	    else
		let l:any_flag=1
	    endif
	    let l:n_flags=[]
	endif
	if a:0 >= 2
	    " flags which must not appear
	    let l:n_flags=split(a:2,'\zs')
	endif
" 	echomsg "DEBUG " string(l:a_flags) . " " . string(l:n_flags) . " any:" l:any_flag

	let l:use_flag_list=[]
	let l:use_flag_word=join(l:use_flag_list)
	let l:true=1
	let l:cpos=getpos(".")
	let l:hit_bottom=0

	while l:true
	    
	    keepjumps let l:s=search('^\s*' . a:object,'W')
" 	    echomsg "DEBUG hit_bottom=" . l:hit_bottom
" 	    echomsg "DEBUG search=" . l:s
	    if l:s != 0
		while getline('.') =~ 'connect\|bind\|subject\|role\|user_transition\|group_transition\|\%(+\|-\)\s*CAP_'
		    keepjumps let l:s=search('^\s*' . a:object,'W')
		endwhile
	    endif

	    if l:s == 0 && l:hit_bottom != 0
		keepjumps call setpos('.',l:cpos)

		echoh WarningMsg
		echomsg "Object not found"
		echoh None

		break
	    endif

	    if l:s == 0 && l:hit_bottom == 0 

		let l:bpos=copy(l:cpos)
		let l:bpos[1]=1
		keepjumps call setpos('.',l:bpos)

		echoh WarningMsg
		echomsg "Search hit BOTTOM, continuing at TOP"
		echoh None

		let l:hit_bottom=1
		"jump back to the begging of the loop
		continue
	    endif

" 	    echomsg "DEBUG LINE " . line(".") 
	    let l:line=getline(".")
	    let l:flags_in_line=split(substitute(substitute(substitute(l:line,'#.*$','',''),'^\s\%(\w\|\/\|\.\|-\|?\|*\|\~\|:\|_\)*\s*','',''),'\s\+\%(#.*\|$\)','',''),'\zs')
" 	    echomsg "DEBUG FLAGS IN LINE " string(l:flags_in_line)

	    " l:not_matched=0 if the flags are matching 
	    " l:not_matched=1 if the flags are not matching 
	    let l:not_matched=0
	    if l:any_flag == 0
	    for l:f in l:a_flags 
		if index(l:flags_in_line,l:f) == -1
		    let l:not_matched=1
		endif
	    endfor
	    endif

	    for l:f in l:n_flags
		if index(l:flags_in_line,l:f) != -1
		    let l:not_matched=1
		endif
	    endfor

	    if l:not_matched == 0
" 		echomsg "DEBUG FIN"
		let l:true=0
		" this is to add the position to the jump list
		call setpos('.',getpos('.'))
	    endif

	endwhile
    endif
endfunction


" show all RBAC log messages
setl errorformat=%m

function! Show_Dmesg()

    "first test if we can use dmesg
    let l:c="dmesg > /dev/null;echo $?" 
    let l:test=system(l:c)
    if l:test =~ 1
	echoerr "You are not privillaged to use dmesg"
	return 0
    endif

    if !exists("s:show_dmesg_number")
	let s:show_dmesg_number=1
    else
	let s:show_dmesg_number+=1
    endif
    let s:log_number=s:show_dmesg_number

    " dictinary to keep names of temporary files
    if !exists("b:logs_dict")
	let b:logs_dict={}
    endif

    let l:name=tempname()

    " find the last log when RBAC was (re)loaded
    call extend(b:logs_dict, { s:show_dmesg_number : l:name }, 'force')

    let l:comm="dmesg | 
		\ grep '^grsec' | 
		\ tac |
		\ egrep -m1 -n 'RBAC system loaded'\\|'RBAC system reloaded' | 
		\ awk '{print $1}' | 
		\ sed 's/:grsec://g'"	
    let b:comm=l:comm

    " read only the current logs
    let l:linenr=system(l:comm)
    let l:linenr=substitute(l:linenr,'\D','','g')
    let l:com="dmesg | grep '^grsec' | tail -" . l:linenr . " > " . l:name 
    call system(l:com)

    " remove double lines from log file
    let l:log=readfile(l:name)

    let l:nlog=[]
    for l:line in l:log
	if index(l:nlog,l:line) == -1 && l:line !~ "grsec: more alerts, logging disabled" && l:line !~ g:ignore_grsec_logs
	    call add(l:nlog,l:line)
	endif
    endfor
    let b:nlog=l:nlog
    call writefile(l:nlog,l:name)
	
    " set the errorfile and list errors
    let &l:errorfile=l:name
    cg
    if !empty(getqflist())
	cl
    else
	echomsg "No grsec log messages."
    endif

    if s:show_dmesg_number > 1 && readfile(l:name) == readfile(b:logs_dict[s:show_dmesg_number-1])
	call delete(b:logs_dict[s:show_dmesg_number])
	call remove(b:logs_dict,s:show_dmesg_number)
	let s:show_dmesg_number-=1
	let s:log_number-=1
    endif
endfunction
map <buffer> \e :echo Echo_Role_Subject()<CR>

function! Show_Previous_Logs()

    if !exists("s:show_dmesg_number")
	return
    endif

    if !exists("s:log_number")
	let s:log_number=s:show_dmesg_number
    endif
	let b:s=s:show_dmesg_number
	let b:l=s:log_number

    if s:log_number > 1 && s:show_dmesg_number > 1
	let s:log_number-=1
	let &l:errorfile=b:logs_dict[s:log_number]
	cg
	cl
    else
	echohl WarningMsg
	echo "No PREVIOUS grsec log messages"
	echohl None
    endif
endfunction

function! Show_Next_Logs()

    if !exists("s:show_dmesg_number")
	return
    endif

    if !exists("s:log_number")
	let s:log_number=s:show_dmesg_number
    endif

    if s:log_number < s:show_dmesg_number
	let s:log_number+=1
	let &l:errorfile=b:logs_dict[s:log_number]
	cg
	cl
    else
	echohl WarningMsg
	echo "No NEXT grsec log messages"
	echohl None
    endif
endfunction

" set the log number
function! Log_Nr(nr)
    let l:nr=a:nr-1
    if get(keys(b:logs_dict),l:nr,'-1') != -1 
	let s:log_number=a:nr
	let &l:errorfile=b:logs_dict[a:nr]
    else
	echo "no such log"
    endif
    set ef?
endfunction
function! Log_Compl(A,P,L)
    return keys(b:logs_dict)
endfunction

function! Remove_Logs()
    if !exists("b:logs_dict")
	return 0
    endif

    " remove temporary files
    for l:tempfile in values(b:logs_dict)
	call delete(l:tempfile)
    endfor

    " clear the b:logs_dict variable
    let b:logs_dict={}
    unlet s:show_dmesg_number
endfunction

function! Save_Log(path)
    let l:log=readfile(b:logs_dict[s:log_number])
    call writefile(l:log,a:path)
endfunction

function! s:index(list,pat)
    let l:len = len(a:list)
    let l:i = 0
    while l:i <= l:len-1
	if a:list[l:i] =~ a:pat
	    break
	endif
	let l:i+=1
    endwhile

    return l:i
endfunction

function! s:filter(list,pat)

    let l:len = len(a:list)
    let l:i = 0

    let l:list=[]
    while l:i <= l:len-1
	if a:list[l:i] =~ a:pat
	    call add(l:list,a:list[l:i])
	endif
	let l:i+=1
    endwhile

    return l:list
endfunction

" function! Remove(list,beg,end)
"     let l:len=len(a:list)

if !exists("*ListSubjects")
function! ListSubjects(role)
    let l:policy=getbufline(bufname("%"),1,'$')
    let l:policy=s:filter(l:policy, '^\s*\%(role\|subject\)\s.*')
    let b:po=deepcopy(l:policy)
    let l:beg=s:index(l:policy,'^\s*role\s\+' . a:role)
    let l:policy=remove(l:policy,l:beg+1,-1)
    let l:end=s:index(l:policy,'^\s*role\s')
    let l:policy=remove(l:policy,0,l:end-1)
    call sort(l:policy)

    exe	"40vsplit\\ +setl\\ wiw=15\\ buftype=nofile\\ nowrap"
    setl ft=grsecurity
    map <buffer> q :q!<CR>
    let l:line=1
    for l:s in l:policy
	call setline(l:line,substitute(substitute(l:s,'\s*#.*$','',''),'{\s*','',''))
	let l:line+=1
    endfor
endfunction
endif

function! Reload(...)
    if !executable('gradm')
	echohl WarnningMsg
	echomsg "You are not previllage to use gradm"
	echohl None
	return
    endif
    w
    !gradm -R
    if a:0 == 0
	!gradm -a admin
    endif
endfunction

function! Admin()
    if !executable('gradm')
	echohl WarnningMsg
	echomsg "You are not previllage to use gradm"
	echohl None
	return
    endif
    !gradm -a admin
endfunction

" function! EnableLearning()
"     if !executable('gradm')
" 	echohl WarnningMsg
" 	echomsg "You are not previllage to use gradm"
" 	echohl None
" 	return
"     endif
"     !gradm -S | grep enabled
"     if v:shell_error == 0
" 	    " if gradm is on	
" 	!gradm -D
"     endif
"     call system("gradm -E -L " . g:gradm_learning_log_file)
" endfunction

function! Learn()
    call system("gradm -L " . g:gradm_learning_log_file " . " -O " . g:gradm_output_policy")
    exe "vsplit " . g:gradm_output_policy
endfunction

let &l:statusline='%<%f %(%h%m%r %)%=%{Echo_Role_Subject()}    %-15.15(%l,%c%V%)%P'

command! -buffer -nargs=+ -complete=customlist,Role_Subject_Compl S 	:call Search_for_Subject(<f-args>)
command! -buffer -complete=customlist,Role_Compl -nargs=+ R 	 	:call Search_for_Role(<f-args>)
command! -buffer -nargs=+ -complete=customlist,Role_Subject_Compl SS 	:call Search_in_Subject(<f-args>)
command! -buffer -complete=customlist,Role_Subject_Compl -nargs=+ SR 	:call Search_in_Role(<f-args>)
command! -buffer -nargs=+ SO 	:call Search_for_Object(<f-args>)
command! -buffer Si 		:call Search_i_Flag()
command! -buffer NS 		:call Next_Subject()
command! -buffer NL 		:call Show_Next_Logs()
command! -buffer PL 		:call Show_Previous_Logs()
command! -buffer RemoveLogs	:call Remove_Logs()
map <buffer> ] :NS<CR>
command! -buffer PS 		:call Previous_Subject()
map <buffer> [ :PS<CR>
command! -buffer NR 		:call Next_Role()
map <buffer> ) :NR<CR>
command! -buffer PR 		:call Previous_Role()
map <buffer> ( :PR<CR>
command! -buffer TS 		:call Top_of_Subject()
map <buffer> { :TS<CR>
command! -buffer BS 		:call Bottom_of_Subject()
map <buffer> } :BS<CR>
command! -buffer Dmesg 		:call Show_Dmesg()
command! -buffer -complete=customlist,Log_Compl -nargs=1 LogNr	:call Log_Nr(<f-args>)
command! -buffer ListRoles	:call Role_Compl('','','',1)
command! -buffer -nargs=1 -complete=file SaveLog	:call Save_Log(<f-args>)
command! -buffer Reload		:call Reload()	
command! -buffer Admin		:call Admin()
command! -buffer Enable		:!gradm -E 
" command! -buffer EnableLearning	:call EnableLearning()
command! -buffer Learn		:call Learn()
" command! -buffer -nargs=1 EnableLog :call system("gradm -E -L " . <args>)
command! -buffer Disable	:!gradm -D
command! -buffer -nargs=1 -complete=customlist,Role_Compl ListSubjects		:call ListSubjects(<f-args>)
/root/.vim/doc/grsecurity_ftplugin.txt	[[[1
296
			   *grsecurity-ftplugin* 
			Filetype Plugin for maintaining
			  grsecurity policy files
			    by Marcin Szamotulski
		-------------------------------------------------

To install:
>
 "  	(1) copy this file to $HOME/.vim/ftplugin
 "	(2) put in your vimrc file
 " 		au BufRead /etc/grsec/policy setl filetype=grsecurity
<
there is a syntax file for /etc/grsec/policy written by Jochen Bartl: 
http://verbosemo.de/~lobo/files/grsecurity.vim

Please note that all commands use <f-agrs> (see :h <f-args>).
Secondly, as for now some functions only works well with policy files in which
subjects are enclosed with '{' '}', for example: 
>
 "            role default
 "            subject / {
 "                    /                               h
 "                    -CAP_ALL
 "                    connect disabled
 "                    bind    disabled
 "            }
<


In the status line you will see the current ROLE and SUBJECT ( returned by the
function Echo_Role_Subject() mapped to \e).

A good vim command to know is '@:' which repeats the last command (for example
to use with the supplied searching command). 

Here is a list of functions (commands) and their short description:
(for your convienience help tags contain the function name and the command
name so typing ':h grsec-search <CTRL-d>' you will get list of functions which
do a search, and the associated command name. Names of functions are rather
self descriptive.

You may want to allow 'gvim' to speak to '/dev/grsec'. You can do that adding
'a' subject flag to 'gvim' in an administrative role or root role. You can
also add 's' subject flag to '/etc/grsec' in 'gvim's subject. You won't get
errors when 'gvim' was not able to read the policy file (for example while
reloading policy and not being in a special role.)

SEARCHING							*grsec-search*
======================================================================
								*grsec-Search_for_Subject-S*
Search_for_Subject(pattern [, role]) command :S 
			Search for subject, if the optional argument [role] is
			specified, restrict to this role.

                        The exact pattern that is to be searched is:
>
 "                    '^\s*subject\s.\{-}' . a:pattern
<
							
								*grsec_Search_in_Subject-SS*
Search_in_Subject(pattern [, subject, role]) command :SS
			Search in the current subject for the given pattern.
			If only [subject] is given it searches in the current
			role for the subject. If both are given first finds
			the [subject] in the given [role]. (if hit bottom of
			the subject, continues at top)

			The completion is set for the [role] argument.
                        The exact pattern to be searched is not changed. 

								*grsec-Search_in_Role-SR*
Search_in_Role(pattern [, role]) command :SR
			Search for a pattern in the role [role] (if hit bottom
			of a role, continue at top). If [role] is not supplied
			searching in the current role.
                        The exact pattern to be searched is not changed. 

								*grsec-Search_Object-SO*
Search_for_Object(pattern [, flag, !flags])	command :SO	
			Search for an object matching pattern which flags
			contains the flags in the second argument and do not
			contains the flags in the third argument. The second
			argument can be equal to 'any' (or 'all') is one wants
			to match for any flag but some flags.

                        The exact pattern to be searched is: 
>
 "				^\s* a:pattern
<
			The lines which containes words: subject, role, bind,
			connect, user_transition, group_transition, CAP_ are
			skipped. 

			If you want to search for a pattern, which ends with 
			'xpdf' put as your pattern '.*xpdf\s' (note the space
			'\s' at the end, and not '\>').
								*grsec-Search_i_Flag-Si*
Search_i_Flag()		command :Si		
			Search for 'i' subject flag
								*grsec-Search_for_Role-R*
Search_for_Role() 	command :R		
			Search for a role, has custom completion set which
			returns names of roles.
                        The exact pattern that is to be searched is:
>
 "                      '^\s*\%(role\).\{-}' . a:what . '\>'
<


MOVING								*grsec-moving*
======================================================================
								*grsec-Top_of_Subject-TS*
Top_of_Subject()	command :TS		Go to the top of current subject
			map {
			    					*grsec-Bottom_of_Subject-BT*
Bottom_of_Subject()	command :BS		Go to the bottom of current subject
			map }
								*grsec-Next_Subject-NS*
Next_Subject()		command :NS		Go to the next subject
			map ]
								*grsec-Previous_Subject-PS*
Previous_Subject() 	command :PS		Go to the previous subject
			map [
								*grsec-Next_Role-NR*	
Next_Role()		command :NR		Go to the next role
			map )
								*grsec-Prevous_Role-PR*
Previous_Role()		command :PR		go to the top of current or
			map (			previous role (if you're on top)
			

Some function have special arguments which are not explained and not important for the end user.

DMESG								*grsec-show-dmesg*
======================================================================
Show_Dmesg()		command :Dmesg				*grsec-Show_Dmesg-Dmesg*

    This function shows the last grsec warnings (beginning from the last time
    grsec was (re)loaded). It uses error list so you can use :cc, :cl, :copen
    commands. If you reload grsec, then run :Dmesg to reload the error file.
    Previous logs are still accessible using Show_Previous_Logs()
    Show_Next_Logs(). You can reload grsec from within gvim, if you allow gvim
    to talk to /dev/grsec (add the a flag to gvim. Additionally, you can add s
    object flag to /etc/grsec for gvim, so that denied access of gvim to
    '/etc/grsec' won't be logged.  The stack of Previous Logs contain only
    different logs, if you run :Dmesg twice and the logs do not differ only
    one log file is remembered. We use tempname() function to obtain a name for
    temporary file in which the logs are stored, in some cases there might be
    SECURITY ISSUES with this so YOU ARE WARNED. 
    
    Please read vim documentation about security issues concerning temporary
    files.

The following functions gives the same functionality as |:colder| and |:cnewer|:
Show_Previous_Logs	command :PL				*grsec-Show_Previous_Logs-PL*
Show_Next_Logs		command :NL				*grsec-Show_Next_Logs-NL*
						both described above.
The variable b:logs_dict is a dictionary of the form { number : tempfile },
where the tempfile stores the dmesg output.

Log_Nr(number)		command :LogNR				*grsec-Log_Nr-LogNr*
	Use log whith number (error file is set according to values of
	b:logs_dict).


Remove_Logs()		command :RemoveLogs			*grsec-Remove_Logs*
    This function removes all temporary files which stores the dmesg logs.

Save_Log(name)		command :SaveLog			*grsec-Save_Log*
    Save the current logs under the 'name'.

command :ListRoles						*grsec-ListRoles*
    This command list all the roles.
	
======================================================================
Examples							*grsec-examples*

:SO \/var\/spool\S* rwca
	search for all objects which path begins with \/var\/spool and the
	flag contain r w c and a
:SO \S*\/share\/\S*\/bin\/\S* rx
	search for all binaries whose full path contains the directory share
	and which are readable and executable

:SO .*\d\+\.\%(\d\|\.\)\+
	search for objects which have a version number (for example: 
	/lib64/libc-2.10.1.so will match)

:S slocate\s*[oAd]\+\s*\%($\|#\)
	search for subjects of slocate program whose object flags are at most
	oAd. This is the way how :SO treats the flags.

:S \/\  root
	search for the subject '\ ' in the role root, (note that between '\/\'
	and 'root' there are two white spaces.


======================================================================
Subject Modes							*grsec-Subject_Modes*

h - This process is hidden and only viewable by processes with the v mode.
v - This process can view hidden processes.
p - This process is protected; it can only be killed by processes with the k
     mode, or by processes within the same subject.
k - This process can kill protected processes.
l - Enables learning mode for this process.
d - Protect the /proc/¡pid¿/fd and /proc/¡pid¿/mem entries for processes in
     this subject.
b - Enable process accounting for processes in this subject.
O - Override the additional mmap() and ptrace() restrictions for this subject.
t - Allow this process to ptrace any process (use with caution)
r - Relax ptrace restrictions (allows process to ptrace processes other than its
     own descendants)
A - Protect the shared memory of this subject. No other processes but pro-
     cesses contained within this subject may access the shared memory of this
     subject.
K - When processes belonging to this subject generate an alert, kill the process
C - When processes belonging to this subject generate an alert, kill the process
     and all processes belonging to the IP of the attacker (if there was an IP
     attached to the process).
T - Ensures this process can never execute any trojaned code.
o - Override ACL inheritance for this process.
i - Enable inheritance-based learning for this subject, causing all accesses of
     this subject and anything it executes to be placed in this subject, and
     inheritance ﬂags added to executable objects in this subject. 
O - Disable ”writable library” restrictions for this task
a - Allow this process to talk to the /dev/grsec device

======================================================================
Object Modes							*grsec-Object_Modes*

h - This object is hidden.
r - This object can be opened for reading.
w - This object can be opened for writing or appending.
x - This object can be executed (or mmap’d with PROT_EXEC into a task).
a - This object can be opened for appending.
c - Allow creation of the ﬁle/directory
d - Allow deletion of the ﬁle/directory
m - Allow creation of setuid/setgid
     ﬁles/directories and modiﬁcation of ﬁles/directories to be setuid/setgid
l - Allow a hardlink at this path (hardlinking requires at a minimum c and l
     modes, and the target link cannot have any greater permission than the
     source ﬁle)
t - This object can be ptraced, but cannot modify the running task. This is
     referred to as a ‘read-only ptrace’.
p - Reject all ptraces to this object
s - Logs will be suppressed for denied access to this object.
i - This mode only applies to binaries. When the object is executed, it inherits
     the ACL of the subject in which it was contained. 
R - Audit successful reads to this object.
W - Audit successful writes to this object.
X - Audit successful execs of this object.
A - Audit successful appends to this object.
F - Audit successful ﬁnds of this object.
I - Audit successful ACL inherits of this object.
M - Audit the setuid/setgid creation/modiﬁcation
C - Audit the creation
D - Audit the deletion
L - Audit link creation

======================================================================
Role Flags							*grsec-Role_Flags*

A - This role is an administrative role, thus it has special privilege normal
     roles do not have. In particular, this role bypasses the additional ptrace
     restrictions
N - Don’t require authentication for this role. To access the role, use
     gradm -n <rolename>
s - This role is a special role, meaning it does not belong to a user or group,
     and does not require an enforced secure policy base to be included in the
     ruleset.
u - This role is a user role
g - This role is a group role
G - This role can use gradm to authenticate to the kernel A policy for gradm
     will automatically be added to the role.
T - Enable TPE for this role
l - Enable learning for this role
P - Use PAM authentication for this role.

======================================================================
Copy Rights							*grsec-copy-rights*

>
 " Copyright:    Copyright (C) 2010 Marcin Szamotulski Permission is hereby
 "		granted to use and distribute this code, with or without
 "		modifications, provided that this copyright notice is copied
 "		with it. Like anything else that's free, this ftplugin
 "		is provided *as is* and comes with no warranty of any kind,
 "		either expressed or implied. By using this plugin, you agree
 "		that in no event will the copyright holder be liable for any
 "		damages resulting from the use of this software.
 " 		This licence is valid for all files distributed with
 " 		grsecurity ftplugin.
<

vim:tw=78:ts=8:ft=help:norl:
