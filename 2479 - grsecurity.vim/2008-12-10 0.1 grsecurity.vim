" Vim syntax file
" Maintainer: 	Jochen Bartl <jochen.bartl@gmail.com>
" URL: 		http://verbosemo.de/~lobo/files/grsecurity.vim
" Last Change: 	2008-12-10
" Version: 	0.1

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

setlocal foldmethod=syntax
syn sync fromstart

syn match grsecComment		/#.*$/
syn keyword grsecStatement	define role_transitions subject
syn match grsecRole		/^role/ contains=grsecRoleFlags
syn match grsecRoleAllowIP	/^role_allow_ip/ contains=grsecIPv4,grsecNM
syn keyword grsecGroupTrans	group_transition_allow
syn keyword grsecUserTrans	user_transition_allow
syn keyword grsecSocketType	stream dgram

syn keyword grsecCap		CAP_ALL CAP_CHOWN CAP_FSETID CAP_SETGID CAP_SETUID CAP_SYS_TTY_CONFIG
syn keyword grsecCap		CAP_FOWNER CAP_SYS_CHROOT CAP_DAC_OVERRIDE CAP_SYS_RESOURCE CAP_IPC_LOCK
syn keyword grsecCap 		CAP_KILL CAP_NET_ADMIN

syn match grsecObjFlags		/\s[acdhilmprstwx]*$/
syn match grsecRoleFlags	/\s[suAG]*$/

syn keyword grsecDis		disabled
syn match grsecIPv4		/\d\{1,3}\.\d\{1,3}\.\d\{1,3}\.\d\{1,3}/
syn match grsecNM		/\/\d\+/


syn region grsecSubject		start=/{/ end=/}/ transparent fold contains=ALLBUT,grsecRole,grsecStatement,grsecSubject

hi def link grsecCap		Type
hi def link grsecComment	Comment
hi def link grsecDis		Constant
hi def link grsecIPv4		Type
hi def link grsecNM		Constant
hi def link grsecObjFlags	Constant
hi def link grsecRoleFlags	Constant
hi def link grsecRole		Statement
hi def link grsecRoleAllowIP	Statement
hi def link grsecStatement	Statement
hi def link grsecSocketType	Type

hi def link grsecUserTrans	Statement
hi def link grsecGroupTrans	Statement
