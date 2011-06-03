" Vim syntax file
" Language:	Nasl
" Version:	0.1
" Maintainer:	Markus De Shon <markusdes@yahoo.com>
" Last Change:	2003 December 18

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" A bunch of useful nasl keywords
"syn keyword	naslStatement	
syn keyword	naslStatement	get_kb_item get_kb_list get_port_state safe_checks
syn keyword	naslStatement	kb_smb_name kb_smb_login kb_smb_password kb_smb_domain kb_smb_transport
syn keyword	naslStatement	security_hole security_warning security_note
syn keyword	naslStatement	sleep exit 
syn keyword	naslStatement	strstr strlen tolower
syn keyword	naslStatement	make_list http_get
syn keyword	naslStatement	open_sock_tcp open_priv_sock_tcp open_priv_sock_udp
syn keyword	naslStatement	http_open_socket close

syn keyword	naslStatement	defined_func script_xref

" header information
syn keyword	naslStatement	script_version script_name script_description 
syn keyword	naslStatement	script_exclude_keys
syn keyword	naslStatement	script_summary script_category script_copyright script_timeout
syn keyword	naslStatement	script_family script_dependencie script_dependencies
syn keyword	naslStatement	script_require_ports script_require_keys
syn keyword	naslStatement	script_id script_cve_id script_bugtraq_id
" TODO: make these intelligent about brackets and languages
syn keyword	naslStatement	name summary family desc

" languages
syn cluster naslLanguages	contains=naslEnglish,naslFrancais,naslPortuges
syn match naslEnglish	/english:/
syn match naslFrancais	/francais:/
syn match naslPortuges	/portuges:/

" http_func.inc
syn keyword	naslStatement	get_http_banner get_http_port php_ver_match
syn keyword	naslStatement	http_is_dead check_win_dir_trav http_recv_body
syn keyword	naslStatement	http_recv http_recv_length cgi_dirs

" http_keepalive.inc
syn keyword	naslStatement	http_keepalive_check_connection enable_keepalive
syn keyword	naslStatement	http_keepalive_enabled http_keepalive_recv
syn keyword	naslStatement	on_exit http_keepalive_send_recv 
syn keyword	naslStatement	check_win_dir_trav_ka is_cgi_installed_ka get_http_page

" smb_nt.inc
syn keyword	naslStatement	smb_recv netbios_name netbios_redirector_name unicode
syn keyword	naslStatement	smb_session_request session_extract_uid smb_neg_prot_cleartext
syn keyword	naslStatement	smb_neg_prot_NTLMv1 smb_neg_prot smb_neg_prot_value
syn keyword	naslStatement	smb_neg_prot_cs smb_neg_prot_domain smb_session_setup_cleartext
syn keyword	naslStatement	smb_session_setup_NTLMvN smb_session_setup smb_tconx
syn keyword	naslStatement	tconx_extract_tid smbntcreatex smbntcreatex_extract_pipe
syn keyword	naslStatement	pipe_accessible_registry registry_access_step_1 registry_get_key
syn keyword	naslStatement	registry_key_writeable_by_non_admin registry_get_key_security
syn keyword	naslStatement	registry_get_acl unicode2 registry_get_item_sz
syn keyword	naslStatement	registry_decode_sz registry_get_item_dword registry_decode_dword
syn keyword	naslStatement	registry_get_dword registry_get_sz OpenPipeToSamr
syn keyword	naslStatement	samr_smbwritex samr_smbreadx samr_uc
syn keyword	naslStatement	SamrConnect2 _SamrEnumDomains SamrDom2Sid SamrOpenDomain
syn keyword	naslStatement	SamrOpenBuiltin SamrLookupNames SamrOpenUser SamrQueryUserGroups
syn keyword	naslStatement	SamrQueryUserInfo SamrQueryUserAliases _ExtractTime
syn keyword	naslStatement	_SamrDecodeUserInfo OpenAndX ReadAndX smb_get_file_size FindFirst2

syn keyword	naslFunction	function

syn keyword	naslConditional	if else
syn keyword	naslLoop	while for foreach 

"syn keyword	naslStatement	

" Constants

syn keyword	naslConstant	ACT_ATTACK ACT_DENIAL ACT_DESTRUCTIVE_ATTACK
syn keyword	naslConstant	ACT_END ACT_GATHER_INFO ACT_KILL_HOST
syn keyword	naslConstant	ACT_MIXED_ATTACK ACT_SCANNER ACT_SETTINGS

" Comments
"=========
syn cluster	naslCommentGroup	contains=naslTodo
syn keyword	naslTodo	contained	TODO
syn match	naslComment		"#.*$" contains=@naslCommentGroup

" Quoted string
syn region	naslString start=/"/ end=/"/ contains=naslNonStringWithinString,naslSpecificTag
syn region	naslNonStringWithinString start=/\\"/ end=/\\"/ contained

syn match	naslSpecificTag +\[/\=specific\]+ contained

" Enforce no quotes allowed in some other match or region
syn match	naslNoQuoteRegionError /".*/ contained

" include statements
syn match	naslIncluded	display contained "\"[^"]*\""
syn match	naslInclude	display "^\s*include\s*(\s*\".*)" contains=naslIncluded

" matching set of parentheses, to be used for set of arguments to a function
" call, see naslEreg below for an example.  If you know for sure you won't
" have any parentheses in the arguments, then you can use a simpler form, see
" naslRegsistryGetSz below.
syn region	naslArgNest	start=+(+ end=+)+ transparent contained

syn match	naslNumber	/[0-9]\+/
syn match	naslHexNumber	/0x[0-9A-Fa-f][0-9A-Fa-f]/
syn match	naslNonKeyword	/[A-Za-z]\+/	contained
syn cluster	naslArgValues	contains=naslString,naslNonKeyword,naslNumber,naslHexNumber

" ord
syn region	naslOrd		matchgroup=naslOrd start=/ord\s*(/ end=/)/ contains=naslArgNest,@naslArgValues

" send/recv
syn region	naslSend	matchgroup=naslSend start=+send\s*(+ end=+)+ contains=naslArgNest,naslSendArg,@naslArgValues
syn match	naslSendArg	/socket\:/ contained
syn match	naslSendArg	/data\:/ contained
syn region	naslRecv	matchgroup=naslRecv start=+recv\s*(+ end=+)+ contains=naslArgNest,naslRecvArg,@naslArgValues
syn region	naslRecv	matchgroup=naslRecv start=+recv_line\s*(+ end=+)+ contains=naslArgNest,naslRecvArg,@naslArgValues
syn match	naslRecvArg	/socket\:/ contained
syn match	naslRecvArg	/length\:/ contained
syn match	naslRecvArg	/timeout\:/ contained

" ereg/egrep statement
syn region	naslEreg	matchgroup=naslEreg start=+egrep\s*(+ end=+)+ contains=naslArgNest,naslEregArg,@naslArgValues
syn region	naslEreg	matchgroup=naslEreg start=+ereg\s*(+ end=+)+ contains=naslArgNest,naslEregArg,@naslArgValues
syn region	naslEregReplace	matchgroup=naslEregReplace start=+ereg_replace\s*(+ end=+)+ contains=naslArgNest,naslEregArg,@naslArgValues
syn match	naslEregArg	/string\:/ contained
syn match	naslEregArg	/pattern\:/ contained
syn match	naslEregArg	/replace\:/ contained

syn region	naslStrReplace	matchgroup=naslStrReplace start=+str_replace\s*(+ end=+)+ contains=naslArgNest,naslStrReplaceArg,@naslArgValues
syn match	naslStrReplaceArg	/find\:/ contained
syn match	naslStrReplaceArg	/replace\:/ contained
syn match	naslStrReplaceArg	/string\:/ contained

syn region	naslRawString	matchgroup=naslRawString start=+raw_string\s*(+ end=+)+ contains=naslArgNest,@naslArgValues
syn region	naslRawString	matchgroup=naslRawString start=+string\s*(+ end=+)+ contains=naslArgNest,@naslArgValues

" registry_get_sz
syn match	naslRegistryGetSz	+registry_get_sz\s*([^)]*)+ contains=naslRegistryGetSzArg,naslString
syn match	naslRegistryGetSzArg	/key:/ contained
syn match	naslRegistryGetSzArg	/item:/ contained


" Define the default highlighting.
"
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_nasl_syn_inits")
  if version < 508
    let did_nasl_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink naslFunction		Function
  HiLink naslStatement		Statement
  HiLink naslComment		Comment
  HiLink naslString		String
  HiLink naslConstant		Constant
  HiLink naslSpecificTag	NonText
  HiLink naslNoQuoteRegionError	Error
  HiLink naslInclude		Include
  HiLink naslIncluded		String
  HiLink naslConditional	Conditional
  HiLink naslNumber		Number
  HiLink naslHexNumber		Number

" Statements with single argument
  HiLink naslOrd		Statement

" Statements with lists of arguments
  HiLink naslFunctionCalls	Statement
  HiLink naslFunctionCallArgs	Type
  HiLink naslEreg		naslFunctionCalls
  HiLink naslEregReplace	naslFunctionCalls
  HiLink naslEregArg		naslFunctionCallArgs
  HiLink naslRegistryGetSz	naslFunctionCalls
  HiLink naslRegistryGetSzArg	naslFunctionCallArgs
  HiLink naslStrReplace		naslFunctionCalls
  HiLink naslStrReplaceArg	naslFunctionCallArgs
  HiLink naslSend		naslFunctionCalls
  HiLink naslSendArg		naslFunctionCallArgs
  HiLink naslRecv		naslFunctionCalls
  HiLink naslRecvArg		naslFunctionCallArgs
  HiLink naslRawString		naslFunctionCalls

  HiLink naslEnglish		Statement
  HiLink naslFrancais		Statement
  HiLink naslPortuges		Statement

  delcommand HiLink
endif

let b:current_syntax = "nasl"

" vim: ts=8
