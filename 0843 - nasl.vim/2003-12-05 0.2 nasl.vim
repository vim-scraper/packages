" Vim syntax file
" Language:	Nasl
" Version:	0.2
" Maintainer:	Markus De Shon <markusdes@yahoo.com>
" Last Change:	2003 December 05

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" http_func.inc
syn keyword	naslStatement	get_http_banner get_http_port php_ver_match
syn keyword	naslStatement	http_is_dead check_win_dir_trav http_recv_body
syn keyword	naslStatement	http_recv_length cgi_dirs

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
syn keyword	naslRepeat	while for foreach 

"syn keyword	naslStatement	

" Constants
syn keyword	naslConstant	TRUE FALSE pcap_timeout IPPROTO_TCP IPPROTO_UDP IPPROTO_ICMP
syn keyword	naslConstant	IPROTO_IP IPPROTO_IGMP ENCAPS_IP ENCAPS_SSLv23 ENCAPS_SSLv2
syn keyword	naslConstant	ENCAPS_SSLv3 ENCAPS_TLSv1 TH_FIN TH_SYN TH_RST TH_PUSH TH_ACK
syn keyword	naslConstant	TH_URG IP_RF IP_DF IP_MF IP_OFFMASK ACT_INIT ACT_GATHER_INFO
syn keyword	naslConstant	ACT_ATTACK ACT_MIXED_ATTACK ACT_DESTRUCTIVE_ATTACK ACT_DENIAL
syn keyword	naslConstant	ACT_SCANNER ACT_SETTINGS ACT_KILL_HOST ACT_END MSG_OOB NULL

" Comments
"=========
syn cluster	naslCommentGroup	contains=naslTodo
syn keyword	naslTodo	contained	TODO
syn match	naslComment		"#.*$" contains=@naslCommentGroup

" Quoted string
syn region	naslString start=/"/ end=/"/ contains=naslNonStringWithinString
syn region	naslNonStringWithinString start=/\\"/ end=/\\"/ contained

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
syn cluster	naslArgValues	contains=naslString,naslNonKeyword,naslNumber,naslHexNumber,naslConstant

"###########
" Functions
"###########

" script_name
syn region	naslFuncXscript_name	matchgroup=naslFuncXscript_name start=+script_name\s*(+ end=+)+ contains=naslArgNest,naslArgXscript_name,@naslArgValues,@naslNestedFunctions
syn match	naslArgXscript_name	/deutsch\:/ contained
syn match	naslArgXscript_name	/english\:/ contained
syn match	naslArgXscript_name	/francais\:/ contained
syn match	naslArgXscript_name	/portugues\:/ contained

" script_version
syn region	naslFuncXscript_version	matchgroup=naslFuncXscript_version start=+script_version\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" script_timeout
syn region	naslFuncXscript_timeout	matchgroup=naslFuncXscript_timeout start=+script_timeout\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" script_description
syn region	naslFuncXscript_description	matchgroup=naslFuncXscript_description start=+script_description\s*(+ end=+)+ contains=naslArgNest,naslArgXscript_description,@naslArgValues,@naslNestedFunctions
syn match	naslArgXscript_description	/deutsch\:/ contained
syn match	naslArgXscript_description	/english\:/ contained
syn match	naslArgXscript_description	/francais\:/ contained
syn match	naslArgXscript_description	/portugues\:/ contained

" script_copyright
syn region	naslFuncXscript_copyright	matchgroup=naslFuncXscript_copyright start=+script_copyright\s*(+ end=+)+ contains=naslArgNest,naslArgXscript_copyright,@naslArgValues,@naslNestedFunctions
syn match	naslArgXscript_copyright	/deutsch\:/ contained
syn match	naslArgXscript_copyright	/english\:/ contained
syn match	naslArgXscript_copyright	/francais\:/ contained
syn match	naslArgXscript_copyright	/portugues\:/ contained

" script_summary
syn region	naslFuncXscript_summary	matchgroup=naslFuncXscript_summary start=+script_summary\s*(+ end=+)+ contains=naslArgNest,naslArgXscript_summary,@naslArgValues,@naslNestedFunctions
syn match	naslArgXscript_summary	/deutsch\:/ contained
syn match	naslArgXscript_summary	/english\:/ contained
syn match	naslArgXscript_summary	/francais\:/ contained
syn match	naslArgXscript_summary	/portugues\:/ contained

" script_category
syn region	naslFuncXscript_category	matchgroup=naslFuncXscript_category start=+script_category\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" script_family
syn region	naslFuncXscript_family	matchgroup=naslFuncXscript_family start=+script_family\s*(+ end=+)+ contains=naslArgNest,naslArgXscript_family,@naslArgValues,@naslNestedFunctions
syn match	naslArgXscript_family	/deutsch\:/ contained
syn match	naslArgXscript_family	/english\:/ contained
syn match	naslArgXscript_family	/francais\:/ contained
syn match	naslArgXscript_family	/portugues\:/ contained

" script_dependencie
syn region	naslFuncXscript_dependencie	matchgroup=naslFuncXscript_dependencie start=+script_dependencie\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" script_dependencies
syn region	naslFuncXscript_dependencies	matchgroup=naslFuncXscript_dependencies start=+script_dependencies\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" script_require_keys
syn region	naslFuncXscript_require_keys	matchgroup=naslFuncXscript_require_keys start=+script_require_keys\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" script_require_ports
syn region	naslFuncXscript_require_ports	matchgroup=naslFuncXscript_require_ports start=+script_require_ports\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" script_require_udp_ports
syn region	naslFuncXscript_require_udp_ports	matchgroup=naslFuncXscript_require_udp_ports start=+script_require_udp_ports\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" script_exclude_keys
syn region	naslFuncXscript_exclude_keys	matchgroup=naslFuncXscript_exclude_keys start=+script_exclude_keys\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" script_add_preference
syn region	naslFuncXscript_add_preference	matchgroup=naslFuncXscript_add_preference start=+script_add_preference\s*(+ end=+)+ contains=naslArgNest,naslArgXscript_add_preference,@naslArgValues,@naslNestedFunctions
syn match	naslArgXscript_add_preference	/name\:/ contained
syn match	naslArgXscript_add_preference	/type\:/ contained
syn match	naslArgXscript_add_preference	/value\:/ contained

" script_get_preference
syn region	naslFuncXscript_get_preference	matchgroup=naslFuncXscript_get_preference start=+script_get_preference\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" script_id
syn region	naslFuncXscript_id	matchgroup=naslFuncXscript_id start=+script_id\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" script_cve_id
syn region	naslFuncXscript_cve_id	matchgroup=naslFuncXscript_cve_id start=+script_cve_id\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" script_bugtraq_id
syn region	naslFuncXscript_bugtraq_id	matchgroup=naslFuncXscript_bugtraq_id start=+script_bugtraq_id\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" script_xref
syn region	naslFuncXscript_xref	matchgroup=naslFuncXscript_xref start=+script_xref\s*(+ end=+)+ contains=naslArgNest,naslArgXscript_xref,@naslArgValues,@naslNestedFunctions
syn match	naslArgXscript_xref	/name\:/ contained
syn match	naslArgXscript_xref	/value\:/ contained

" safe_checks
syn region	naslFuncXsafe_checks	matchgroup=naslFuncXsafe_checks start=+safe_checks\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" set_kb_item
syn region	naslFuncXset_kb_item	matchgroup=naslFuncXset_kb_item start=+set_kb_item\s*(+ end=+)+ contains=naslArgNest,naslArgXset_kb_item,@naslArgValues,@naslNestedFunctions
syn match	naslArgXset_kb_item	/name\:/ contained
syn match	naslArgXset_kb_item	/value\:/ contained

" get_kb_item
syn region	naslFuncXget_kb_item	matchgroup=naslFuncXget_kb_item start=+get_kb_item\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" get_kb_list
syn region	naslFuncXget_kb_list	matchgroup=naslFuncXget_kb_list start=+get_kb_list\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" security_warning
syn region	naslFuncXsecurity_warning	matchgroup=naslFuncXsecurity_warning start=+security_warning\s*(+ end=+)+ contains=naslArgNest,naslArgXsecurity_warning,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsecurity_warning	/data\:/ contained
syn match	naslArgXsecurity_warning	/port\:/ contained
syn match	naslArgXsecurity_warning	/proto\:/ contained
syn match	naslArgXsecurity_warning	/protocol\:/ contained

" security_note
syn region	naslFuncXsecurity_note	matchgroup=naslFuncXsecurity_note start=+security_note\s*(+ end=+)+ contains=naslArgNest,naslArgXsecurity_note,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsecurity_note	/data\:/ contained
syn match	naslArgXsecurity_note	/port\:/ contained
syn match	naslArgXsecurity_note	/proto\:/ contained
syn match	naslArgXsecurity_note	/protocol\:/ contained

" security_hole
syn region	naslFuncXsecurity_hole	matchgroup=naslFuncXsecurity_hole start=+security_hole\s*(+ end=+)+ contains=naslArgNest,naslArgXsecurity_hole,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsecurity_hole	/data\:/ contained
syn match	naslArgXsecurity_hole	/port\:/ contained
syn match	naslArgXsecurity_hole	/proto\:/ contained
syn match	naslArgXsecurity_hole	/protocol\:/ contained

" open_sock_tcp
syn region	naslFuncXopen_sock_tcp	matchgroup=naslFuncXopen_sock_tcp start=+open_sock_tcp\s*(+ end=+)+ contains=naslArgNest,naslArgXopen_sock_tcp,@naslArgValues,@naslNestedFunctions
syn match	naslArgXopen_sock_tcp	/timeout\:/ contained
syn match	naslArgXopen_sock_tcp	/transport\:/ contained

" open_sock_udp
syn region	naslFuncXopen_sock_udp	matchgroup=naslFuncXopen_sock_udp start=+open_sock_udp\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" open_priv_sock_tcp
syn region	naslFuncXopen_priv_sock_tcp	matchgroup=naslFuncXopen_priv_sock_tcp start=+open_priv_sock_tcp\s*(+ end=+)+ contains=naslArgNest,naslArgXopen_priv_sock_tcp,@naslArgValues,@naslNestedFunctions
syn match	naslArgXopen_priv_sock_tcp	/dport\:/ contained
syn match	naslArgXopen_priv_sock_tcp	/sport\:/ contained
syn match	naslArgXopen_priv_sock_tcp	/timeout\:/ contained

" open_priv_sock_udp
syn region	naslFuncXopen_priv_sock_udp	matchgroup=naslFuncXopen_priv_sock_udp start=+open_priv_sock_udp\s*(+ end=+)+ contains=naslArgNest,naslArgXopen_priv_sock_udp,@naslArgValues,@naslNestedFunctions
syn match	naslArgXopen_priv_sock_udp	/dport\:/ contained
syn match	naslArgXopen_priv_sock_udp	/sport\:/ contained

" recv
syn region	naslFuncXrecv	matchgroup=naslFuncXrecv start=+recv\s*(+ end=+)+ contains=naslArgNest,naslArgXrecv,@naslArgValues,@naslNestedFunctions
syn match	naslArgXrecv	/length\:/ contained
syn match	naslArgXrecv	/min\:/ contained
syn match	naslArgXrecv	/socket\:/ contained
syn match	naslArgXrecv	/timeout\:/ contained

" recv_line
syn region	naslFuncXrecv_line	matchgroup=naslFuncXrecv_line start=+recv_line\s*(+ end=+)+ contains=naslArgNest,naslArgXrecv_line,@naslArgValues,@naslNestedFunctions
syn match	naslArgXrecv_line	/length\:/ contained
syn match	naslArgXrecv_line	/socket\:/ contained
syn match	naslArgXrecv_line	/timeout\:/ contained

" send
syn region	naslFuncXsend	matchgroup=naslFuncXsend start=+send\s*(+ end=+)+ contains=naslArgNest,naslArgXsend,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsend	/data\:/ contained
syn match	naslArgXsend	/length\:/ contained
syn match	naslArgXsend	/option\:/ contained
syn match	naslArgXsend	/socket\:/ contained

" close
syn region	naslFuncXclose	matchgroup=naslFuncXclose start=+close\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" join_multicast_group
syn region	naslFuncXjoin_multicast_group	matchgroup=naslFuncXjoin_multicast_group start=+join_multicast_group\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" leave_multicast_group
syn region	naslFuncXleave_multicast_group	matchgroup=naslFuncXleave_multicast_group start=+leave_multicast_group\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" cgibin
syn region	naslFuncXcgibin	matchgroup=naslFuncXcgibin start=+cgibin\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" is_cgi_installed
syn region	naslFuncXis_cgi_installed	matchgroup=naslFuncXis_cgi_installed start=+is_cgi_installed\s*(+ end=+)+ contains=naslArgNest,naslArgXis_cgi_installed,@naslArgValues,@naslNestedFunctions
syn match	naslArgXis_cgi_installed	/item\:/ contained
syn match	naslArgXis_cgi_installed	/port\:/ contained

" http_open_socket
syn region	naslFuncXhttp_open_socket	matchgroup=naslFuncXhttp_open_socket start=+http_open_socket\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" http_head
syn region	naslFuncXhttp_head	matchgroup=naslFuncXhttp_head start=+http_head\s*(+ end=+)+ contains=naslArgNest,naslArgXhttp_head,@naslArgValues,@naslNestedFunctions
syn match	naslArgXhttp_head	/data\:/ contained
syn match	naslArgXhttp_head	/item\:/ contained
syn match	naslArgXhttp_head	/port\:/ contained

" http_get
syn region	naslFuncXhttp_get	matchgroup=naslFuncXhttp_get start=+http_get\s*(+ end=+)+ contains=naslArgNest,naslArgXhttp_get,@naslArgValues,@naslNestedFunctions
syn match	naslArgXhttp_get	/data\:/ contained
syn match	naslArgXhttp_get	/item\:/ contained
syn match	naslArgXhttp_get	/port\:/ contained

" http_post
syn region	naslFuncXhttp_post	matchgroup=naslFuncXhttp_post start=+http_post\s*(+ end=+)+ contains=naslArgNest,naslArgXhttp_post,@naslArgValues,@naslNestedFunctions
syn match	naslArgXhttp_post	/data\:/ contained
syn match	naslArgXhttp_post	/item\:/ contained
syn match	naslArgXhttp_post	/port\:/ contained

" http_delete
syn region	naslFuncXhttp_delete	matchgroup=naslFuncXhttp_delete start=+http_delete\s*(+ end=+)+ contains=naslArgNest,naslArgXhttp_delete,@naslArgValues,@naslNestedFunctions
syn match	naslArgXhttp_delete	/data\:/ contained
syn match	naslArgXhttp_delete	/item\:/ contained
syn match	naslArgXhttp_delete	/port\:/ contained

" http_put
syn region	naslFuncXhttp_put	matchgroup=naslFuncXhttp_put start=+http_put\s*(+ end=+)+ contains=naslArgNest,naslArgXhttp_put,@naslArgValues,@naslNestedFunctions
syn match	naslArgXhttp_put	/data\:/ contained
syn match	naslArgXhttp_put	/item\:/ contained
syn match	naslArgXhttp_put	/port\:/ contained

" http_close_socket
syn region	naslFuncXhttp_close_socket	matchgroup=naslFuncXhttp_close_socket start=+http_close_socket\s*(+ end=+)+ contains=naslArgNest,naslArgXhttp_close_socket,@naslArgValues,@naslNestedFunctions
syn match	naslArgXhttp_close_socket	/socket\:/ contained

" http_recv_headers
syn region	naslFuncXhttp_recv_headers	matchgroup=naslFuncXhttp_recv_headers start=+http_recv_headers\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" get_host_name
syn region	naslFuncXget_host_name	matchgroup=naslFuncXget_host_name start=+get_host_name\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" get_host_ip
syn region	naslFuncXget_host_ip	matchgroup=naslFuncXget_host_ip start=+get_host_ip\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" get_host_open_port
syn region	naslFuncXget_host_open_port	matchgroup=naslFuncXget_host_open_port start=+get_host_open_port\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" get_port_state
syn region	naslFuncXget_port_state	matchgroup=naslFuncXget_port_state start=+get_port_state\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" get_tcp_port_state
syn region	naslFuncXget_tcp_port_state	matchgroup=naslFuncXget_tcp_port_state start=+get_tcp_port_state\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" get_udp_port_state
syn region	naslFuncXget_udp_port_state	matchgroup=naslFuncXget_udp_port_state start=+get_udp_port_state\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" scanner_add_port
syn region	naslFuncXscanner_add_port	matchgroup=naslFuncXscanner_add_port start=+scanner_add_port\s*(+ end=+)+ contains=naslArgNest,naslArgXscanner_add_port,@naslArgValues,@naslNestedFunctions
syn match	naslArgXscanner_add_port	/port\:/ contained
syn match	naslArgXscanner_add_port	/proto\:/ contained

" scanner_status
syn region	naslFuncXscanner_status	matchgroup=naslFuncXscanner_status start=+scanner_status\s*(+ end=+)+ contains=naslArgNest,naslArgXscanner_status,@naslArgValues,@naslNestedFunctions
syn match	naslArgXscanner_status	/current\:/ contained
syn match	naslArgXscanner_status	/total\:/ contained

" scanner_get_port
syn region	naslFuncXscanner_get_port	matchgroup=naslFuncXscanner_get_port start=+scanner_get_port\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" islocalhost
syn region	naslFuncXislocalhost	matchgroup=naslFuncXislocalhost start=+islocalhost\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" islocalnet
syn region	naslFuncXislocalnet	matchgroup=naslFuncXislocalnet start=+islocalnet\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" get_port_transport
syn region	naslFuncXget_port_transport	matchgroup=naslFuncXget_port_transport start=+get_port_transport\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" this_host
syn region	naslFuncXthis_host	matchgroup=naslFuncXthis_host start=+this_host\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" this_host_name
syn region	naslFuncXthis_host_name	matchgroup=naslFuncXthis_host_name start=+this_host_name\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" string
syn region	naslFuncXstring	matchgroup=naslFuncXstring start=+string\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" raw_string
syn region	naslFuncXraw_string	matchgroup=naslFuncXraw_string start=+raw_string\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" strcat
syn region	naslFuncXstrcat	matchgroup=naslFuncXstrcat start=+strcat\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" display
syn region	naslFuncXdisplay	matchgroup=naslFuncXdisplay start=+display\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" ord
syn region	naslFuncXord	matchgroup=naslFuncXord start=+ord\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" hex
syn region	naslFuncXhex	matchgroup=naslFuncXhex start=+hex\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" hexstr
syn region	naslFuncXhexstr	matchgroup=naslFuncXhexstr start=+hexstr\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" strstr
syn region	naslFuncXstrstr	matchgroup=naslFuncXstrstr start=+strstr\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" ereg
syn region	naslFuncXereg	matchgroup=naslFuncXereg start=+ereg\s*(+ end=+)+ contains=naslArgNest,naslArgXereg,@naslArgValues,@naslNestedFunctions
syn match	naslArgXereg	/icase\:/ contained
syn match	naslArgXereg	/pattern\:/ contained
syn match	naslArgXereg	/string\:/ contained

" ereg_replace
syn region	naslFuncXereg_replace	matchgroup=naslFuncXereg_replace start=+ereg_replace\s*(+ end=+)+ contains=naslArgNest,naslArgXereg_replace,@naslArgValues,@naslNestedFunctions
syn match	naslArgXereg_replace	/icase\:/ contained
syn match	naslArgXereg_replace	/pattern\:/ contained
syn match	naslArgXereg_replace	/replace\:/ contained
syn match	naslArgXereg_replace	/string\:/ contained

" egrep
syn region	naslFuncXegrep	matchgroup=naslFuncXegrep start=+egrep\s*(+ end=+)+ contains=naslArgNest,naslArgXegrep,@naslArgValues,@naslNestedFunctions
syn match	naslArgXegrep	/icase\:/ contained
syn match	naslArgXegrep	/pattern\:/ contained
syn match	naslArgXegrep	/string\:/ contained

" eregmatch
syn region	naslFuncXeregmatch	matchgroup=naslFuncXeregmatch start=+eregmatch\s*(+ end=+)+ contains=naslArgNest,naslArgXeregmatch,@naslArgValues,@naslNestedFunctions
syn match	naslArgXeregmatch	/icase\:/ contained
syn match	naslArgXeregmatch	/pattern\:/ contained
syn match	naslArgXeregmatch	/string\:/ contained

" match
syn region	naslFuncXmatch	matchgroup=naslFuncXmatch start=+match\s*(+ end=+)+ contains=naslArgNest,naslArgXmatch,@naslArgValues,@naslNestedFunctions
syn match	naslArgXmatch	/icase\:/ contained
syn match	naslArgXmatch	/pattern\:/ contained
syn match	naslArgXmatch	/string\:/ contained

" substr
syn region	naslFuncXsubstr	matchgroup=naslFuncXsubstr start=+substr\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" insstr
syn region	naslFuncXinsstr	matchgroup=naslFuncXinsstr start=+insstr\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" tolower
syn region	naslFuncXtolower	matchgroup=naslFuncXtolower start=+tolower\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" toupper
syn region	naslFuncXtoupper	matchgroup=naslFuncXtoupper start=+toupper\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" crap
syn region	naslFuncXcrap	matchgroup=naslFuncXcrap start=+crap\s*(+ end=+)+ contains=naslArgNest,naslArgXcrap,@naslArgValues,@naslNestedFunctions
syn match	naslArgXcrap	/data\:/ contained
syn match	naslArgXcrap	/length\:/ contained

" strlen
syn region	naslFuncXstrlen	matchgroup=naslFuncXstrlen start=+strlen\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" split
syn region	naslFuncXsplit	matchgroup=naslFuncXsplit start=+split\s*(+ end=+)+ contains=naslArgNest,naslArgXsplit,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsplit	/keep\:/ contained
syn match	naslArgXsplit	/sep\:/ contained

" chomp
syn region	naslFuncXchomp	matchgroup=naslFuncXchomp start=+chomp\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" int
syn region	naslFuncXint	matchgroup=naslFuncXint start=+int\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" stridx
syn region	naslFuncXstridx	matchgroup=naslFuncXstridx start=+stridx\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" str_replace
syn region	naslFuncXstr_replace	matchgroup=naslFuncXstr_replace start=+str_replace\s*(+ end=+)+ contains=naslArgNest,naslArgXstr_replace,@naslArgValues,@naslNestedFunctions
syn match	naslArgXstr_replace	/count\:/ contained
syn match	naslArgXstr_replace	/find\:/ contained
syn match	naslArgXstr_replace	/replace\:/ contained
syn match	naslArgXstr_replace	/string\:/ contained

" make_list
syn region	naslFuncXmake_list	matchgroup=naslFuncXmake_list start=+make_list\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" make_array
syn region	naslFuncXmake_array	matchgroup=naslFuncXmake_array start=+make_array\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" keys
syn region	naslFuncXkeys	matchgroup=naslFuncXkeys start=+keys\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" max_index
syn region	naslFuncXmax_index	matchgroup=naslFuncXmax_index start=+max_index\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" sort
syn region	naslFuncXsort	matchgroup=naslFuncXsort start=+sort\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" telnet_init
syn region	naslFuncXtelnet_init	matchgroup=naslFuncXtelnet_init start=+telnet_init\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" ftp_log_in
syn region	naslFuncXftp_log_in	matchgroup=naslFuncXftp_log_in start=+ftp_log_in\s*(+ end=+)+ contains=naslArgNest,naslArgXftp_log_in,@naslArgValues,@naslNestedFunctions
syn match	naslArgXftp_log_in	/pass\:/ contained
syn match	naslArgXftp_log_in	/socket\:/ contained
syn match	naslArgXftp_log_in	/user\:/ contained

" ftp_get_pasv_port
syn region	naslFuncXftp_get_pasv_port	matchgroup=naslFuncXftp_get_pasv_port start=+ftp_get_pasv_port\s*(+ end=+)+ contains=naslArgNest,naslArgXftp_get_pasv_port,@naslArgValues,@naslNestedFunctions
syn match	naslArgXftp_get_pasv_port	/socket\:/ contained

" start_denial
syn region	naslFuncXstart_denial	matchgroup=naslFuncXstart_denial start=+start_denial\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" end_denial
syn region	naslFuncXend_denial	matchgroup=naslFuncXend_denial start=+end_denial\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" dump_ctxt
syn region	naslFuncXdump_ctxt	matchgroup=naslFuncXdump_ctxt start=+dump_ctxt\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" typeof
syn region	naslFuncXtypeof	matchgroup=naslFuncXtypeof start=+typeof\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" exit
syn region	naslFuncXexit	matchgroup=naslFuncXexit start=+exit\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" rand
syn region	naslFuncXrand	matchgroup=naslFuncXrand start=+rand\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" usleep
syn region	naslFuncXusleep	matchgroup=naslFuncXusleep start=+usleep\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" sleep
syn region	naslFuncXsleep	matchgroup=naslFuncXsleep start=+sleep\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" isnull
syn region	naslFuncXisnull	matchgroup=naslFuncXisnull start=+isnull\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" defined_func
syn region	naslFuncXdefined_func	matchgroup=naslFuncXdefined_func start=+defined_func\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" forge_ip_packet
syn region	naslFuncXforge_ip_packet	matchgroup=naslFuncXforge_ip_packet start=+forge_ip_packet\s*(+ end=+)+ contains=naslArgNest,naslArgXforge_ip_packet,@naslArgValues,@naslNestedFunctions
syn match	naslArgXforge_ip_packet	/data\:/ contained
syn match	naslArgXforge_ip_packet	/ip_dst\:/ contained
syn match	naslArgXforge_ip_packet	/ip_hl\:/ contained
syn match	naslArgXforge_ip_packet	/ip_id\:/ contained
syn match	naslArgXforge_ip_packet	/ip_len\:/ contained
syn match	naslArgXforge_ip_packet	/ip_off\:/ contained
syn match	naslArgXforge_ip_packet	/ip_p\:/ contained
syn match	naslArgXforge_ip_packet	/ip_src\:/ contained
syn match	naslArgXforge_ip_packet	/ip_sum\:/ contained
syn match	naslArgXforge_ip_packet	/ip_tos\:/ contained
syn match	naslArgXforge_ip_packet	/ip_ttl\:/ contained
syn match	naslArgXforge_ip_packet	/ip_v\:/ contained

" get_ip_element
syn region	naslFuncXget_ip_element	matchgroup=naslFuncXget_ip_element start=+get_ip_element\s*(+ end=+)+ contains=naslArgNest,naslArgXget_ip_element,@naslArgValues,@naslNestedFunctions
syn match	naslArgXget_ip_element	/element\:/ contained
syn match	naslArgXget_ip_element	/ip\:/ contained

" set_ip_elements
syn region	naslFuncXset_ip_elements	matchgroup=naslFuncXset_ip_elements start=+set_ip_elements\s*(+ end=+)+ contains=naslArgNest,naslArgXset_ip_elements,@naslArgValues,@naslNestedFunctions
syn match	naslArgXset_ip_elements	/ip\:/ contained
syn match	naslArgXset_ip_elements	/ip_dst\:/ contained
syn match	naslArgXset_ip_elements	/ip_hl\:/ contained
syn match	naslArgXset_ip_elements	/ip_id\:/ contained
syn match	naslArgXset_ip_elements	/ip_len\:/ contained
syn match	naslArgXset_ip_elements	/ip_off\:/ contained
syn match	naslArgXset_ip_elements	/ip_p\:/ contained
syn match	naslArgXset_ip_elements	/ip_src\:/ contained
syn match	naslArgXset_ip_elements	/ip_sum\:/ contained
syn match	naslArgXset_ip_elements	/ip_tos\:/ contained
syn match	naslArgXset_ip_elements	/ip_ttl\:/ contained
syn match	naslArgXset_ip_elements	/ip_v\:/ contained

" insert_ip_options
syn region	naslFuncXinsert_ip_options	matchgroup=naslFuncXinsert_ip_options start=+insert_ip_options\s*(+ end=+)+ contains=naslArgNest,naslArgXinsert_ip_options,@naslArgValues,@naslNestedFunctions
syn match	naslArgXinsert_ip_options	/code\:/ contained
syn match	naslArgXinsert_ip_options	/ip\:/ contained
syn match	naslArgXinsert_ip_options	/length\:/ contained
syn match	naslArgXinsert_ip_options	/value\:/ contained

" dump_ip_packet
syn region	naslFuncXdump_ip_packet	matchgroup=naslFuncXdump_ip_packet start=+dump_ip_packet\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" forge_tcp_packet
syn region	naslFuncXforge_tcp_packet	matchgroup=naslFuncXforge_tcp_packet start=+forge_tcp_packet\s*(+ end=+)+ contains=naslArgNest,naslArgXforge_tcp_packet,@naslArgValues,@naslNestedFunctions
syn match	naslArgXforge_tcp_packet	/data\:/ contained
syn match	naslArgXforge_tcp_packet	/ip\:/ contained
syn match	naslArgXforge_tcp_packet	/th_ack\:/ contained
syn match	naslArgXforge_tcp_packet	/th_dport\:/ contained
syn match	naslArgXforge_tcp_packet	/th_flags\:/ contained
syn match	naslArgXforge_tcp_packet	/th_off\:/ contained
syn match	naslArgXforge_tcp_packet	/th_seq\:/ contained
syn match	naslArgXforge_tcp_packet	/th_sport\:/ contained
syn match	naslArgXforge_tcp_packet	/th_sum\:/ contained
syn match	naslArgXforge_tcp_packet	/th_urp\:/ contained
syn match	naslArgXforge_tcp_packet	/th_win\:/ contained
syn match	naslArgXforge_tcp_packet	/th_x2\:/ contained
syn match	naslArgXforge_tcp_packet	/update_ip_len\:/ contained

" get_tcp_element
syn region	naslFuncXget_tcp_element	matchgroup=naslFuncXget_tcp_element start=+get_tcp_element\s*(+ end=+)+ contains=naslArgNest,naslArgXget_tcp_element,@naslArgValues,@naslNestedFunctions
syn match	naslArgXget_tcp_element	/element\:/ contained
syn match	naslArgXget_tcp_element	/tcp\:/ contained

" set_tcp_elements
syn region	naslFuncXset_tcp_elements	matchgroup=naslFuncXset_tcp_elements start=+set_tcp_elements\s*(+ end=+)+ contains=naslArgNest,naslArgXset_tcp_elements,@naslArgValues,@naslNestedFunctions
syn match	naslArgXset_tcp_elements	/data\:/ contained
syn match	naslArgXset_tcp_elements	/tcp\:/ contained
syn match	naslArgXset_tcp_elements	/th_ack\:/ contained
syn match	naslArgXset_tcp_elements	/th_dport\:/ contained
syn match	naslArgXset_tcp_elements	/th_flags\:/ contained
syn match	naslArgXset_tcp_elements	/th_off\:/ contained
syn match	naslArgXset_tcp_elements	/th_seq\:/ contained
syn match	naslArgXset_tcp_elements	/th_sport\:/ contained
syn match	naslArgXset_tcp_elements	/th_sum\:/ contained
syn match	naslArgXset_tcp_elements	/th_urp\:/ contained
syn match	naslArgXset_tcp_elements	/th_win\:/ contained
syn match	naslArgXset_tcp_elements	/th_x2\:/ contained

" dump_tcp_packet
syn region	naslFuncXdump_tcp_packet	matchgroup=naslFuncXdump_tcp_packet start=+dump_tcp_packet\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" tcp_ping
syn region	naslFuncXtcp_ping	matchgroup=naslFuncXtcp_ping start=+tcp_ping\s*(+ end=+)+ contains=naslArgNest,naslArgXtcp_ping,@naslArgValues,@naslNestedFunctions
syn match	naslArgXtcp_ping	/port\:/ contained

" forge_udp_packet
syn region	naslFuncXforge_udp_packet	matchgroup=naslFuncXforge_udp_packet start=+forge_udp_packet\s*(+ end=+)+ contains=naslArgNest,naslArgXforge_udp_packet,@naslArgValues,@naslNestedFunctions
syn match	naslArgXforge_udp_packet	/data\:/ contained
syn match	naslArgXforge_udp_packet	/ip\:/ contained
syn match	naslArgXforge_udp_packet	/uh_dport\:/ contained
syn match	naslArgXforge_udp_packet	/uh_sport\:/ contained
syn match	naslArgXforge_udp_packet	/uh_sum\:/ contained
syn match	naslArgXforge_udp_packet	/uh_ulen\:/ contained
syn match	naslArgXforge_udp_packet	/update_ip_len\:/ contained

" get_udp_element
syn region	naslFuncXget_udp_element	matchgroup=naslFuncXget_udp_element start=+get_udp_element\s*(+ end=+)+ contains=naslArgNest,naslArgXget_udp_element,@naslArgValues,@naslNestedFunctions
syn match	naslArgXget_udp_element	/element\:/ contained
syn match	naslArgXget_udp_element	/udp\:/ contained

" set_udp_elements
syn region	naslFuncXset_udp_elements	matchgroup=naslFuncXset_udp_elements start=+set_udp_elements\s*(+ end=+)+ contains=naslArgNest,naslArgXset_udp_elements,@naslArgValues,@naslNestedFunctions
syn match	naslArgXset_udp_elements	/data\:/ contained
syn match	naslArgXset_udp_elements	/udp\:/ contained
syn match	naslArgXset_udp_elements	/uh_dport\:/ contained
syn match	naslArgXset_udp_elements	/uh_sport\:/ contained
syn match	naslArgXset_udp_elements	/uh_sum\:/ contained
syn match	naslArgXset_udp_elements	/uh_ulen\:/ contained

" dump_udp_packet
syn region	naslFuncXdump_udp_packet	matchgroup=naslFuncXdump_udp_packet start=+dump_udp_packet\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" forge_icmp_packet
syn region	naslFuncXforge_icmp_packet	matchgroup=naslFuncXforge_icmp_packet start=+forge_icmp_packet\s*(+ end=+)+ contains=naslArgNest,naslArgXforge_icmp_packet,@naslArgValues,@naslNestedFunctions
syn match	naslArgXforge_icmp_packet	/data\:/ contained
syn match	naslArgXforge_icmp_packet	/icmp_cksum\:/ contained
syn match	naslArgXforge_icmp_packet	/icmp_code\:/ contained
syn match	naslArgXforge_icmp_packet	/icmp_id\:/ contained
syn match	naslArgXforge_icmp_packet	/icmp_seq\:/ contained
syn match	naslArgXforge_icmp_packet	/icmp_type\:/ contained
syn match	naslArgXforge_icmp_packet	/ip\:/ contained
syn match	naslArgXforge_icmp_packet	/update_ip_len\:/ contained

" get_icmp_element
syn region	naslFuncXget_icmp_element	matchgroup=naslFuncXget_icmp_element start=+get_icmp_element\s*(+ end=+)+ contains=naslArgNest,naslArgXget_icmp_element,@naslArgValues,@naslNestedFunctions
syn match	naslArgXget_icmp_element	/element\:/ contained
syn match	naslArgXget_icmp_element	/icmp\:/ contained

" forge_igmp_packet
syn region	naslFuncXforge_igmp_packet	matchgroup=naslFuncXforge_igmp_packet start=+forge_igmp_packet\s*(+ end=+)+ contains=naslArgNest,naslArgXforge_igmp_packet,@naslArgValues,@naslNestedFunctions
syn match	naslArgXforge_igmp_packet	/code\:/ contained
syn match	naslArgXforge_igmp_packet	/data\:/ contained
syn match	naslArgXforge_igmp_packet	/group\:/ contained
syn match	naslArgXforge_igmp_packet	/ip\:/ contained
syn match	naslArgXforge_igmp_packet	/type\:/ contained
syn match	naslArgXforge_igmp_packet	/update_ip_len\:/ contained

" send_packet
syn region	naslFuncXsend_packet	matchgroup=naslFuncXsend_packet start=+send_packet\s*(+ end=+)+ contains=naslArgNest,naslArgXsend_packet,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsend_packet	/length\:/ contained
syn match	naslArgXsend_packet	/pcap_active\:/ contained
syn match	naslArgXsend_packet	/pcap_filter\:/ contained
syn match	naslArgXsend_packet	/pcap_timeout\:/ contained

" pcap_next
syn region	naslFuncXpcap_next	matchgroup=naslFuncXpcap_next start=+pcap_next\s*(+ end=+)+ contains=naslArgNest,naslArgXpcap_next,@naslArgValues,@naslNestedFunctions
syn match	naslArgXpcap_next	/interface\:/ contained
syn match	naslArgXpcap_next	/pcap_filter\:/ contained
syn match	naslArgXpcap_next	/timeout\:/ contained

" MD2
syn region	naslFuncXMD2	matchgroup=naslFuncXMD2 start=+MD2\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" MD4
syn region	naslFuncXMD4	matchgroup=naslFuncXMD4 start=+MD4\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" MD5
syn region	naslFuncXMD5	matchgroup=naslFuncXMD5 start=+MD5\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" SHA
syn region	naslFuncXSHA	matchgroup=naslFuncXSHA start=+SHA\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" SHA1
syn region	naslFuncXSHA1	matchgroup=naslFuncXSHA1 start=+SHA1\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" RIPEMD160
syn region	naslFuncXRIPEMD160	matchgroup=naslFuncXRIPEMD160 start=+RIPEMD160\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" HMAC_MD2
syn region	naslFuncXHMAC_MD2	matchgroup=naslFuncXHMAC_MD2 start=+HMAC_MD2\s*(+ end=+)+ contains=naslArgNest,naslArgXHMAC_MD2,@naslArgValues,@naslNestedFunctions
syn match	naslArgXHMAC_MD2	/data\:/ contained
syn match	naslArgXHMAC_MD2	/key\:/ contained

" HMAC_MD5
syn region	naslFuncXHMAC_MD5	matchgroup=naslFuncXHMAC_MD5 start=+HMAC_MD5\s*(+ end=+)+ contains=naslArgNest,naslArgXHMAC_MD5,@naslArgValues,@naslNestedFunctions
syn match	naslArgXHMAC_MD5	/data\:/ contained
syn match	naslArgXHMAC_MD5	/key\:/ contained

" HMAC_SHA
syn region	naslFuncXHMAC_SHA	matchgroup=naslFuncXHMAC_SHA start=+HMAC_SHA\s*(+ end=+)+ contains=naslArgNest,naslArgXHMAC_SHA,@naslArgValues,@naslNestedFunctions
syn match	naslArgXHMAC_SHA	/data\:/ contained
syn match	naslArgXHMAC_SHA	/key\:/ contained

" HMAC_SHA1
syn region	naslFuncXHMAC_SHA1	matchgroup=naslFuncXHMAC_SHA1 start=+HMAC_SHA1\s*(+ end=+)+ contains=naslArgNest,naslArgXHMAC_SHA1,@naslArgValues,@naslNestedFunctions
syn match	naslArgXHMAC_SHA1	/data\:/ contained
syn match	naslArgXHMAC_SHA1	/key\:/ contained

" HMAC_DSS
syn region	naslFuncXHMAC_DSS	matchgroup=naslFuncXHMAC_DSS start=+HMAC_DSS\s*(+ end=+)+ contains=naslArgNest,naslArgXHMAC_DSS,@naslArgValues,@naslNestedFunctions
syn match	naslArgXHMAC_DSS	/data\:/ contained
syn match	naslArgXHMAC_DSS	/key\:/ contained

" HMAC_RIPEMD160
syn region	naslFuncXHMAC_RIPEMD160	matchgroup=naslFuncXHMAC_RIPEMD160 start=+HMAC_RIPEMD160\s*(+ end=+)+ contains=naslArgNest,naslArgXHMAC_RIPEMD160,@naslArgValues,@naslNestedFunctions
syn match	naslArgXHMAC_RIPEMD160	/data\:/ contained
syn match	naslArgXHMAC_RIPEMD160	/key\:/ contained

" NTLMv1_HASH
syn region	naslFuncXNTLMv1_HASH	matchgroup=naslFuncXNTLMv1_HASH start=+NTLMv1_HASH\s*(+ end=+)+ contains=naslArgNest,naslArgXNTLMv1_HASH,@naslArgValues,@naslNestedFunctions
syn match	naslArgXNTLMv1_HASH	/cryptkey\:/ contained
syn match	naslArgXNTLMv1_HASH	/passhash\:/ contained

" NTLMv2_HASH
syn region	naslFuncXNTLMv2_HASH	matchgroup=naslFuncXNTLMv2_HASH start=+NTLMv2_HASH\s*(+ end=+)+ contains=naslArgNest,naslArgXNTLMv2_HASH,@naslArgValues,@naslNestedFunctions
syn match	naslArgXNTLMv2_HASH	/cryptkey\:/ contained
syn match	naslArgXNTLMv2_HASH	/length\:/ contained
syn match	naslArgXNTLMv2_HASH	/passhash\:/ contained

" nt_owf_gen
syn region	naslFuncXnt_owf_gen	matchgroup=naslFuncXnt_owf_gen start=+nt_owf_gen\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" lm_owf_gen
syn region	naslFuncXlm_owf_gen	matchgroup=naslFuncXlm_owf_gen start=+lm_owf_gen\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" ntv2_owf_gen
syn region	naslFuncXntv2_owf_gen	matchgroup=naslFuncXntv2_owf_gen start=+ntv2_owf_gen\s*(+ end=+)+ contains=naslArgNest,naslArgXntv2_owf_gen,@naslArgValues,@naslNestedFunctions
syn match	naslArgXntv2_owf_gen	/domain\:/ contained
syn match	naslArgXntv2_owf_gen	/login\:/ contained
syn match	naslArgXntv2_owf_gen	/owf\:/ contained

" ###############
"  End Functions
" ###############


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
  HiLink naslNoQuoteRegionError	Error
  HiLink naslInclude		Include
  HiLink naslIncluded		String
  HiLink naslConditional	Conditional
  HiLink naslRepeat		Repeat
  HiLink naslNumber		Number
  HiLink naslHexNumber		Number

" Defined default hilight mappings for functions and their arguments
  HiLink naslFunctionCalls	Statement
  HiLink naslFunctionArgs	Type

"##### Hilites
"### Function Calls
  HiLink naslArgXscript_name	naslFunctionArgs
  HiLink	naslFuncXscript_name	naslFunctionCalls
  HiLink	naslFuncXscript_version	naslFunctionCalls
  HiLink	naslFuncXscript_timeout	naslFunctionCalls
  HiLink naslArgXscript_description	naslFunctionArgs
  HiLink	naslFuncXscript_description	naslFunctionCalls
  HiLink naslArgXscript_copyright	naslFunctionArgs
  HiLink	naslFuncXscript_copyright	naslFunctionCalls
  HiLink naslArgXscript_summary	naslFunctionArgs
  HiLink	naslFuncXscript_summary	naslFunctionCalls
  HiLink	naslFuncXscript_category	naslFunctionCalls
  HiLink naslArgXscript_family	naslFunctionArgs
  HiLink	naslFuncXscript_family	naslFunctionCalls
  HiLink	naslFuncXscript_dependencie	naslFunctionCalls
  HiLink	naslFuncXscript_dependencies	naslFunctionCalls
  HiLink	naslFuncXscript_require_keys	naslFunctionCalls
  HiLink	naslFuncXscript_require_ports	naslFunctionCalls
  HiLink	naslFuncXscript_require_udp_ports	naslFunctionCalls
  HiLink	naslFuncXscript_exclude_keys	naslFunctionCalls
  HiLink naslArgXscript_add_preference	naslFunctionArgs
  HiLink	naslFuncXscript_add_preference	naslFunctionCalls
  HiLink	naslFuncXscript_get_preference	naslFunctionCalls
  HiLink	naslFuncXscript_id	naslFunctionCalls
  HiLink	naslFuncXscript_cve_id	naslFunctionCalls
  HiLink	naslFuncXscript_bugtraq_id	naslFunctionCalls
  HiLink naslArgXscript_xref	naslFunctionArgs
  HiLink	naslFuncXscript_xref	naslFunctionCalls
  HiLink	naslFuncXsafe_checks	naslFunctionCalls
  HiLink naslArgXset_kb_item	naslFunctionArgs
  HiLink	naslFuncXset_kb_item	naslFunctionCalls
  HiLink	naslFuncXget_kb_item	naslFunctionCalls
  HiLink	naslFuncXget_kb_list	naslFunctionCalls
  HiLink naslArgXsecurity_warning	naslFunctionArgs
  HiLink	naslFuncXsecurity_warning	naslFunctionCalls
  HiLink naslArgXsecurity_note	naslFunctionArgs
  HiLink	naslFuncXsecurity_note	naslFunctionCalls
  HiLink naslArgXsecurity_hole	naslFunctionArgs
  HiLink	naslFuncXsecurity_hole	naslFunctionCalls
  HiLink naslArgXopen_sock_tcp	naslFunctionArgs
  HiLink	naslFuncXopen_sock_tcp	naslFunctionCalls
  HiLink	naslFuncXopen_sock_udp	naslFunctionCalls
  HiLink naslArgXopen_priv_sock_tcp	naslFunctionArgs
  HiLink	naslFuncXopen_priv_sock_tcp	naslFunctionCalls
  HiLink naslArgXopen_priv_sock_udp	naslFunctionArgs
  HiLink	naslFuncXopen_priv_sock_udp	naslFunctionCalls
  HiLink naslArgXrecv	naslFunctionArgs
  HiLink	naslFuncXrecv	naslFunctionCalls
  HiLink naslArgXrecv_line	naslFunctionArgs
  HiLink	naslFuncXrecv_line	naslFunctionCalls
  HiLink naslArgXsend	naslFunctionArgs
  HiLink	naslFuncXsend	naslFunctionCalls
  HiLink	naslFuncXclose	naslFunctionCalls
  HiLink	naslFuncXjoin_multicast_group	naslFunctionCalls
  HiLink	naslFuncXleave_multicast_group	naslFunctionCalls
  HiLink	naslFuncXcgibin	naslFunctionCalls
  HiLink naslArgXis_cgi_installed	naslFunctionArgs
  HiLink	naslFuncXis_cgi_installed	naslFunctionCalls
  HiLink	naslFuncXhttp_open_socket	naslFunctionCalls
  HiLink naslArgXhttp_head	naslFunctionArgs
  HiLink	naslFuncXhttp_head	naslFunctionCalls
  HiLink naslArgXhttp_get	naslFunctionArgs
  HiLink	naslFuncXhttp_get	naslFunctionCalls
  HiLink naslArgXhttp_post	naslFunctionArgs
  HiLink	naslFuncXhttp_post	naslFunctionCalls
  HiLink naslArgXhttp_delete	naslFunctionArgs
  HiLink	naslFuncXhttp_delete	naslFunctionCalls
  HiLink naslArgXhttp_put	naslFunctionArgs
  HiLink	naslFuncXhttp_put	naslFunctionCalls
  HiLink naslArgXhttp_close_socket	naslFunctionArgs
  HiLink	naslFuncXhttp_close_socket	naslFunctionCalls
  HiLink	naslFuncXhttp_recv_headers	naslFunctionCalls
  HiLink	naslFuncXget_host_name	naslFunctionCalls
  HiLink	naslFuncXget_host_ip	naslFunctionCalls
  HiLink	naslFuncXget_host_open_port	naslFunctionCalls
  HiLink	naslFuncXget_port_state	naslFunctionCalls
  HiLink	naslFuncXget_tcp_port_state	naslFunctionCalls
  HiLink	naslFuncXget_udp_port_state	naslFunctionCalls
  HiLink naslArgXscanner_add_port	naslFunctionArgs
  HiLink	naslFuncXscanner_add_port	naslFunctionCalls
  HiLink naslArgXscanner_status	naslFunctionArgs
  HiLink	naslFuncXscanner_status	naslFunctionCalls
  HiLink	naslFuncXscanner_get_port	naslFunctionCalls
  HiLink	naslFuncXislocalhost	naslFunctionCalls
  HiLink	naslFuncXislocalnet	naslFunctionCalls
  HiLink	naslFuncXget_port_transport	naslFunctionCalls
  HiLink	naslFuncXthis_host	naslFunctionCalls
  HiLink	naslFuncXthis_host_name	naslFunctionCalls
  HiLink	naslFuncXstring	naslFunctionCalls
  HiLink	naslFuncXraw_string	naslFunctionCalls
  HiLink	naslFuncXstrcat	naslFunctionCalls
  HiLink	naslFuncXdisplay	naslFunctionCalls
  HiLink	naslFuncXord	naslFunctionCalls
  HiLink	naslFuncXhex	naslFunctionCalls
  HiLink	naslFuncXhexstr	naslFunctionCalls
  HiLink	naslFuncXstrstr	naslFunctionCalls
  HiLink naslArgXereg	naslFunctionArgs
  HiLink	naslFuncXereg	naslFunctionCalls
  HiLink naslArgXereg_replace	naslFunctionArgs
  HiLink	naslFuncXereg_replace	naslFunctionCalls
  HiLink naslArgXegrep	naslFunctionArgs
  HiLink	naslFuncXegrep	naslFunctionCalls
  HiLink naslArgXeregmatch	naslFunctionArgs
  HiLink	naslFuncXeregmatch	naslFunctionCalls
  HiLink naslArgXmatch	naslFunctionArgs
  HiLink	naslFuncXmatch	naslFunctionCalls
  HiLink	naslFuncXsubstr	naslFunctionCalls
  HiLink	naslFuncXinsstr	naslFunctionCalls
  HiLink	naslFuncXtolower	naslFunctionCalls
  HiLink	naslFuncXtoupper	naslFunctionCalls
  HiLink naslArgXcrap	naslFunctionArgs
  HiLink	naslFuncXcrap	naslFunctionCalls
  HiLink	naslFuncXstrlen	naslFunctionCalls
  HiLink naslArgXsplit	naslFunctionArgs
  HiLink	naslFuncXsplit	naslFunctionCalls
  HiLink	naslFuncXchomp	naslFunctionCalls
  HiLink	naslFuncXint	naslFunctionCalls
  HiLink	naslFuncXstridx	naslFunctionCalls
  HiLink naslArgXstr_replace	naslFunctionArgs
  HiLink	naslFuncXstr_replace	naslFunctionCalls
  HiLink	naslFuncXmake_list	naslFunctionCalls
  HiLink	naslFuncXmake_array	naslFunctionCalls
  HiLink	naslFuncXkeys	naslFunctionCalls
  HiLink	naslFuncXmax_index	naslFunctionCalls
  HiLink	naslFuncXsort	naslFunctionCalls
  HiLink	naslFuncXtelnet_init	naslFunctionCalls
  HiLink naslArgXftp_log_in	naslFunctionArgs
  HiLink	naslFuncXftp_log_in	naslFunctionCalls
  HiLink naslArgXftp_get_pasv_port	naslFunctionArgs
  HiLink	naslFuncXftp_get_pasv_port	naslFunctionCalls
  HiLink	naslFuncXstart_denial	naslFunctionCalls
  HiLink	naslFuncXend_denial	naslFunctionCalls
  HiLink	naslFuncXdump_ctxt	naslFunctionCalls
  HiLink	naslFuncXtypeof	naslFunctionCalls
  HiLink	naslFuncXexit	naslFunctionCalls
  HiLink	naslFuncXrand	naslFunctionCalls
  HiLink	naslFuncXusleep	naslFunctionCalls
  HiLink	naslFuncXsleep	naslFunctionCalls
  HiLink	naslFuncXisnull	naslFunctionCalls
  HiLink	naslFuncXdefined_func	naslFunctionCalls
  HiLink naslArgXforge_ip_packet	naslFunctionArgs
  HiLink	naslFuncXforge_ip_packet	naslFunctionCalls
  HiLink naslArgXget_ip_element	naslFunctionArgs
  HiLink	naslFuncXget_ip_element	naslFunctionCalls
  HiLink naslArgXset_ip_elements	naslFunctionArgs
  HiLink	naslFuncXset_ip_elements	naslFunctionCalls
  HiLink naslArgXinsert_ip_options	naslFunctionArgs
  HiLink	naslFuncXinsert_ip_options	naslFunctionCalls
  HiLink	naslFuncXdump_ip_packet	naslFunctionCalls
  HiLink naslArgXforge_tcp_packet	naslFunctionArgs
  HiLink	naslFuncXforge_tcp_packet	naslFunctionCalls
  HiLink naslArgXget_tcp_element	naslFunctionArgs
  HiLink	naslFuncXget_tcp_element	naslFunctionCalls
  HiLink naslArgXset_tcp_elements	naslFunctionArgs
  HiLink	naslFuncXset_tcp_elements	naslFunctionCalls
  HiLink	naslFuncXdump_tcp_packet	naslFunctionCalls
  HiLink naslArgXtcp_ping	naslFunctionArgs
  HiLink	naslFuncXtcp_ping	naslFunctionCalls
  HiLink naslArgXforge_udp_packet	naslFunctionArgs
  HiLink	naslFuncXforge_udp_packet	naslFunctionCalls
  HiLink naslArgXget_udp_element	naslFunctionArgs
  HiLink	naslFuncXget_udp_element	naslFunctionCalls
  HiLink naslArgXset_udp_elements	naslFunctionArgs
  HiLink	naslFuncXset_udp_elements	naslFunctionCalls
  HiLink	naslFuncXdump_udp_packet	naslFunctionCalls
  HiLink naslArgXforge_icmp_packet	naslFunctionArgs
  HiLink	naslFuncXforge_icmp_packet	naslFunctionCalls
  HiLink naslArgXget_icmp_element	naslFunctionArgs
  HiLink	naslFuncXget_icmp_element	naslFunctionCalls
  HiLink naslArgXforge_igmp_packet	naslFunctionArgs
  HiLink	naslFuncXforge_igmp_packet	naslFunctionCalls
  HiLink naslArgXsend_packet	naslFunctionArgs
  HiLink	naslFuncXsend_packet	naslFunctionCalls
  HiLink naslArgXpcap_next	naslFunctionArgs
  HiLink	naslFuncXpcap_next	naslFunctionCalls
  HiLink	naslFuncXMD2	naslFunctionCalls
  HiLink	naslFuncXMD4	naslFunctionCalls
  HiLink	naslFuncXMD5	naslFunctionCalls
  HiLink	naslFuncXSHA	naslFunctionCalls
  HiLink	naslFuncXSHA1	naslFunctionCalls
  HiLink	naslFuncXRIPEMD160	naslFunctionCalls
  HiLink naslArgXHMAC_MD2	naslFunctionArgs
  HiLink	naslFuncXHMAC_MD2	naslFunctionCalls
  HiLink naslArgXHMAC_MD5	naslFunctionArgs
  HiLink	naslFuncXHMAC_MD5	naslFunctionCalls
  HiLink naslArgXHMAC_SHA	naslFunctionArgs
  HiLink	naslFuncXHMAC_SHA	naslFunctionCalls
  HiLink naslArgXHMAC_SHA1	naslFunctionArgs
  HiLink	naslFuncXHMAC_SHA1	naslFunctionCalls
  HiLink naslArgXHMAC_DSS	naslFunctionArgs
  HiLink	naslFuncXHMAC_DSS	naslFunctionCalls
  HiLink naslArgXHMAC_RIPEMD160	naslFunctionArgs
  HiLink	naslFuncXHMAC_RIPEMD160	naslFunctionCalls
  HiLink naslArgXNTLMv1_HASH	naslFunctionArgs
  HiLink	naslFuncXNTLMv1_HASH	naslFunctionCalls
  HiLink naslArgXNTLMv2_HASH	naslFunctionArgs
  HiLink	naslFuncXNTLMv2_HASH	naslFunctionCalls
  HiLink	naslFuncXnt_owf_gen	naslFunctionCalls
  HiLink	naslFuncXlm_owf_gen	naslFunctionCalls
  HiLink naslArgXntv2_owf_gen	naslFunctionArgs
  HiLink	naslFuncXntv2_owf_gen	naslFunctionCalls

  delcommand HiLink
endif

let b:current_syntax = "nasl"

" vim: ts=8
"
"

