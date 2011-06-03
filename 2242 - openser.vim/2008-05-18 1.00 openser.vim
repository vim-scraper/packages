" -*- vim -*-
" FILE: openser.vim
" LAST MODIFICATION: 2008-05-18 23:35
" (C) Copyright 2008 Stanisław Pitucha <viraptor@gmail.com>
" Version: 1.00

" USAGE:
"
" Save this file to $VIMFILES/syntax/openser.vim. Either add a detection
" script to filetypes.vim, or set filetype manually to "openser" when
" editing openser configuration file.
"
" List of keyword and core functions taken from latest documentation of
" version 1.3.X of openser. Module functions not included.
"
" Tested only on vim 7.1
"
" Example: "setf openser"
"
" REQUIREMENTS:
" vim (>= 7)

if exists("b:current_syntax")
	finish
endif

set foldmethod=syntax
set foldlevel=99

syn match	openserConfigParamLine	'^[^=]\+=.*$' contains=openserCoreParameter,openserString,openserConfigConstant,openserSpecial,openserNumber,openserCppComment,openserHashComment
syn region	openserConfigModparam	start='^\s*modparam\s*(' end=')' contains=openserString,openserNumber
syn match	openserConfigModule		'^\s*loadmodule\s*"[^"]\+"' contains=openserString

syn keyword	openserTodo	TODO FIXME XXX contained

syn match	openserOperator		'!\|&&\|||\|=[~=]\?\|>\|<\|+\|-\|/\|\*\||\|&\|^\|\~' display contained

syn region	openserCppComment		start='/\*' end='\*/' contains=openserTodo
syn match	openserHashComment	'#.*$' contains=openserTodo

syn match	openserStringEscape	'\\.' contained
syn match	openserNumber			'[0-9]\+' contained
syn region	openserString			matchgroup=Normal start='"' skip='\\"' end='"' contained contains=openserVariable,openserStringEscape
syn match	openserVariable		"$[a-zA-Z_][a-zA-Z0-9_]*\(([^)]\+)\)\?" contained
syn match	openserIdentifier		'\<[a-zA-Z_][a-zA-Z0-9_]*\>' contained
syn keyword	openserConditional	if else switch case contained
syn keyword	openserSpecial			yes no contained
syn keyword	openserCoreKeyword	af dst_ip dst_port from_uri method msg:len $retcode proto status src_ip src_port to_uri uri contained
syn keyword openserCoreValue		INET INET6 TCP UDP max_len myself null contained
syn keyword	openserCoreFunction	add_local_rport append_branch break drop exit force_rport force_send_socket force_tcp_alias forward isdsturiset isflagset isbflagset issflagset log prefix pv_printf return resetdsturi resetflag resetbflag resetsflag revert_uri rewritehost sethost rewritehostport sethostport rewriteuser setuser rewriteuserpass setuserpass rewriteport setport rewriteuri seturi send set_advertised_address set_advertised_port setdsturi setflag setbflag setsflag strip strip_tail contained

syn keyword	openserCoreParameter	advertised_address advertised_port alias avp_aliases auto_aliases check_via children chroot debug disable_core_dump disable_dns_blacklist disable_dns_failover disable_tcp disable_tls dns dns_retr_time dns_retr_no dns_servers_no dns_try_ipv6 dns_use_search_list dst_blacklist fork group gid listen log_facility log_name log_stderror max_while_loops maxbuffer memlog mcast_loopback mcast_ttl mhomed mpath open_files_limit port reply_to_via rev_dns server_header server_signature sip_warning tcp_children tcp_accept_aliases tcp_send_timeout tcp_connect_timeout tcp_connection_lifetime tcp_max_connections tcp_poll_method tls_ca_list tls_certificate tls_ciphers_list tls_domain tls_handshake_timeout tls_log tls_method tls_port_no tls_private_key tls_require_certificate tls_send_timeout tls_verify tos user uid user_agent_header wdir contained

syn region	openserBlock	start='{' end='}' contained contains=openserBlock,@openserCodeElements transparent fold

syn match	openserFunction	'\<\(failure_\|onreply_\|branch_\|error_\)\?route\>\(\s*\[[^\]]\+\]\)\?' contained contains=openserNumber
syn region	openserFunctionFold	matchgroup=openserFunction start="\<\(failure_\|onreply_\|branch_\|error_\)\?route\>\(\s*\[[^\]]\+\]\)\?\s*{" matchgroup=NONE end="}" transparent fold contains=openserBlock,@openserCodeElements

syn cluster	openserCodeElements contains=openserCppComment,openserHashComment,openserNumber,openserString,openserVariable,openserOperator,openserConditional,openserKeyword,openserCoreKeyword,openserCoreValue,openserCoreFunction,openserIdentifier

"syn sync match openserFunctionSync groupthere NONE "^\s*\(failure_\|onreply_\)\?route\s*\(\[[^\]]\+\]\)\?\s*{"
syn sync fromstart
"syn sync match openserSync	grouphere openserFunctionFold "\<\(failure_|onreply_\)\?route\>"
"syn sync match openserSync	grouphere NONE "^}"

hi def link openserCppComment Comment
hi def link openserHashComment Comment
hi def link openserTodo Todo

hi def link openserConfigModparam Function
hi def link openserConfigModule Keyword

hi def link openserKeyword Keyword
hi def link openserCoreKeyword Special
hi def link openserCoreValue Special
hi def link openserCoreFunction Function
hi def link openserFunction Function
hi def link openserIdentifier Identifier
hi def link openserSpecial Special
hi def link openserCoreParameter Keyword

hi def link openserOperator Operator

hi def link openserConditional Conditional

hi def link openserNumber Number
hi def link openserVariable Identifier
hi def link openserString String
hi def link openserStringEscape Special

let b:current_syntax = "openser"
