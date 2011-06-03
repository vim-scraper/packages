"
"	Name:		co.vim
"
"	Usage:  	put it in your plugins dir, and type <Leader>co when you get
"				the urge.
"			
"	Requires:	vim
"		
"	Provides:	command CommentOut
"				mapping <Leader>co that calls command
"		
"	ToDo:		add UncommentIn
"		


if exists("s:seen") && !exists("s:debug")
	finish
endif

function! s:CommentOut_xml() abort range
	exec ':'.a:firstline
	normal O
	s/^$/<!-- - ->/
	exec ':'.a:lastline
	:+1
	normal o
	s/^$/<!- - -->/
endfunc

function! s:CommentOut() abort range
	let type=&filetype
	if exists("b:comment")
		let comment = b:comment
	elseif exists("s:comment_".type)
		let comment = s:comment_{type}
	else
		echoerr "I have not idea how to comment a ".type." file!"
		return
	endif
	:exec ':'.a:firstline.",".a:lastline.' '.comment
endfunction

let s:comment_ant            ='call s:CommentOut_xml()'
let s:comment_apache		 ='s,^,#	,'
let s:comment_automake		 ='s,^,#    ,'
let s:comment_bash           ='s,^,#    ,'
let s:comment_c              ='s,^,//   ,'
let s:comment_conf			 ='s,^,#    ,'
let s:comment_config		 ='s,^,dnl  ,'
let s:comment_cpp            ='s,^,//   ,'
let s:comment_crontab        ='s,^,#    ,'
let s:comment_fvwm           ='s,^,#	,'
let s:comment_html           ='call s:CommentOut_xml()'
let s:comment_inittab        ='s,^,#	,'
let s:comment_java           ='s,^,//   ,'
let s:comment_jsp            ='call s:CommentOut_xml()'
let s:comment_m4             ='s,^,dnl  ,'
let s:comment_make			 ='s,^,#    ,'
let s:comment_perl			 ='s,^,#    ,'
let s:comment_readline		 ='s,^,#	,'
let s:comment_sh             ='s,^,#    ,'
let s:comment_sql			 ='s,^,--	,'
let s:comment_vim            ='s,^,"    ,'
let s:comment_xf86conf       ='s,^,#    ,'
let s:comment_xml            ='call s:CommentOut_xml()'
let s:comment_xs = s:comment_c

command! -range CommentOut :<line1>,<line2> call s:CommentOut()
map <Leader>co :CommentOut<cr>
