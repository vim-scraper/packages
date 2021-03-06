" Vim syntax file
" Language:	Nasl
" Version:	0.3
" Maintainer:	Markus De Shon <markusdes@yahoo.com>
" Last Change:	2003 December 12

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Set sync minlines higher so that long desc field will not result
" in weird behavior.  Don't really need to worry about performance,
" since nasl files are small.
syntax sync minlines=100 maxlines=500

syn keyword	naslFunction	function

syn keyword	naslConditional	if else
syn keyword	naslRepeat	while for foreach 

"syn keyword	naslStatement	

" Constants
syn keyword	naslConstant	TRUE FALSE IPPROTO_TCP IPPROTO_UDP IPPROTO_ICMP
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

" Double Quoted string
syn region	naslString start=/"/ end=/"/ contains=naslNonStringWithinString
syn region	naslNonStringWithinString start=/\\"/ end=/\\"/ contained

" Single quoted string
syn region	naslSQString start=/'/ skip=/\\'/ end=/'/ contains=naslSpecificTag

" Enforce no quotes allowed in some other match or region
syn match	naslNoQuoteRegionError /[^\\]".*/ contained

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
syn cluster	naslArgValues	contains=naslString,naslNonKeyword,naslNumber,naslHexNumber,naslConstant,naslSQString

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
syn match	naslArgXforge_ip_packet	/data\s*\:/ contained
syn match	naslArgXforge_ip_packet	/ip_dst\s*\:/ contained
syn match	naslArgXforge_ip_packet	/ip_hl\s*\:/ contained
syn match	naslArgXforge_ip_packet	/ip_id\s*\:/ contained
syn match	naslArgXforge_ip_packet	/ip_len\s*\:/ contained
syn match	naslArgXforge_ip_packet	/ip_off\s*\:/ contained
syn match	naslArgXforge_ip_packet	/ip_p\s*\:/ contained
syn match	naslArgXforge_ip_packet	/ip_src\s*\:/ contained
syn match	naslArgXforge_ip_packet	/ip_sum\s*\:/ contained
syn match	naslArgXforge_ip_packet	/ip_tos\s*\:/ contained
syn match	naslArgXforge_ip_packet	/ip_ttl\s*\:/ contained
syn match	naslArgXforge_ip_packet	/ip_v\s*\:/ contained

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
syn match	naslArgXforge_udp_packet	/data\s*\:/ contained
syn match	naslArgXforge_udp_packet	/ip\s*\:/ contained
syn match	naslArgXforge_udp_packet	/uh_dport\s*\:/ contained
syn match	naslArgXforge_udp_packet	/uh_sport\s*\:/ contained
syn match	naslArgXforge_udp_packet	/uh_sum\s*\:/ contained
syn match	naslArgXforge_udp_packet	/uh_ulen\s*\:/ contained
syn match	naslArgXforge_udp_packet	/update_ip_len\s*\:/ contained

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

" ###### Functions from include files

" ## http_func.inc
" hex2dec
syn region	naslFuncXhex2dec	matchgroup=naslFuncXhex2dec start=+hex2dec\s*(+ end=+)+ contains=naslArgNest,naslArgXhex2dec,@naslArgValues,@naslNestedFunctions
syn match	naslArgXhex2dec	/xvalue\:/ contained

" get_http_banner
syn region	naslFuncXget_http_banner	matchgroup=naslFuncXget_http_banner start=+get_http_banner\s*(+ end=+)+ contains=naslArgNest,naslArgXget_http_banner,@naslArgValues,@naslNestedFunctions
syn match	naslArgXget_http_banner	/port\:/ contained

" get_http_port
syn region	naslFuncXget_http_port	matchgroup=naslFuncXget_http_port start=+get_http_port\s*(+ end=+)+ contains=naslArgNest,naslArgXget_http_port,@naslArgValues,@naslNestedFunctions
syn match	naslArgXget_http_port	/default\:/ contained

" php_ver_match
syn region	naslFuncXphp_ver_match	matchgroup=naslFuncXphp_ver_match start=+php_ver_match\s*(+ end=+)+ contains=naslArgNest,naslArgXphp_ver_match,@naslArgValues,@naslNestedFunctions
syn match	naslArgXphp_ver_match	/banner\:/ contained
syn match	naslArgXphp_ver_match	/pattern\:/ contained

" http_is_dead
syn region	naslFuncXhttp_is_dead	matchgroup=naslFuncXhttp_is_dead start=+http_is_dead\s*(+ end=+)+ contains=naslArgNest,naslArgXhttp_is_dead,@naslArgValues,@naslNestedFunctions
syn match	naslArgXhttp_is_dead	/port\:/ contained
syn match	naslArgXhttp_is_dead	/retry\:/ contained

" check_win_dir_trav
syn region	naslFuncXcheck_win_dir_trav	matchgroup=naslFuncXcheck_win_dir_trav start=+check_win_dir_trav\s*(+ end=+)+ contains=naslArgNest,naslArgXcheck_win_dir_trav,@naslArgValues,@naslNestedFunctions
syn match	naslArgXcheck_win_dir_trav	/port\:/ contained
syn match	naslArgXcheck_win_dir_trav	/url\:/ contained
syn match	naslArgXcheck_win_dir_trav	/quickcheck\:/ contained

" http_recv_body
syn region	naslFuncXhttp_recv_body	matchgroup=naslFuncXhttp_recv_body start=+http_recv_body\s*(+ end=+)+ contains=naslArgNest,naslArgXhttp_recv_body,@naslArgValues,@naslNestedFunctions
syn match	naslArgXhttp_recv_body	/socket\:/ contained
syn match	naslArgXhttp_recv_body	/headers\:/ contained
syn match	naslArgXhttp_recv_body	/length\:/ contained

" http_recv
syn region	naslFuncXhttp_recv	matchgroup=naslFuncXhttp_recv start=+http_recv\s*(+ end=+)+ contains=naslArgNest,naslArgXhttp_recv,@naslArgValues,@naslNestedFunctions
syn match	naslArgXhttp_recv	/socket\:/ contained
syn match	naslArgXhttp_recv	/code\:/ contained

" http_recv_length
syn region	naslFuncXhttp_recv_length	matchgroup=naslFuncXhttp_recv_length start=+http_recv_length\s*(+ end=+)+ contains=naslArgNest,naslArgXhttp_recv_length,@naslArgValues,@naslNestedFunctions
syn match	naslArgXhttp_recv_length	/socket\:/ contained
syn match	naslArgXhttp_recv_length	/bodylength\:/ contained

" cgi_dirs
syn region	naslFuncXcgi_dirs	matchgroup=naslFuncXcgi_dirs start=+cgi_dirs\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions


" ### http_keepalive.inc
" http_keepalive_check_connection
syn region	naslFuncXhttp_keepalive_check_connection	matchgroup=naslFuncXhttp_keepalive_check_connection start=+http_keepalive_check_connection\s*(+ end=+)+ contains=naslArgNest,naslArgXhttp_keepalive_check_connection,@naslArgValues,@naslNestedFunctions
syn match	naslArgXhttp_keepalive_check_connection	/headers\:/ contained

" enable_keepalive
syn region	naslFuncXenable_keepalive	matchgroup=naslFuncXenable_keepalive start=+enable_keepalive\s*(+ end=+)+ contains=naslArgNest,naslArgXenable_keepalive,@naslArgValues,@naslNestedFunctions
syn match	naslArgXenable_keepalive	/port\:/ contained

" http_keepalive_enabled
syn region	naslFuncXhttp_keepalive_enabled	matchgroup=naslFuncXhttp_keepalive_enabled start=+http_keepalive_enabled\s*(+ end=+)+ contains=naslArgNest,naslArgXhttp_keepalive_enabled,@naslArgValues,@naslNestedFunctions
syn match	naslArgXhttp_keepalive_enabled	/port\:/ contained

" http_keepalive_recv
syn region	naslFuncXhttp_keepalive_recv	matchgroup=naslFuncXhttp_keepalive_recv start=+http_keepalive_recv\s*(+ end=+)+ contains=naslArgNest,naslArgXhttp_keepalive_recv,@naslArgValues,@naslNestedFunctions
syn match	naslArgXhttp_keepalive_recv	/bodyonly\:/ contained

" on_exit
syn region	naslFuncXon_exit	matchgroup=naslFuncXon_exit start=+on_exit\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" http_keepalive_send_recv
syn region	naslFuncXhttp_keepalive_send_recv	matchgroup=naslFuncXhttp_keepalive_send_recv start=+http_keepalive_send_recv\s*(+ end=+)+ contains=naslArgNest,naslArgXhttp_keepalive_send_recv,@naslArgValues,@naslNestedFunctions
syn match	naslArgXhttp_keepalive_send_recv	/port\:/ contained
syn match	naslArgXhttp_keepalive_send_recv	/data\:/ contained
syn match	naslArgXhttp_keepalive_send_recv	/bodyonly\:/ contained

" check_win_dir_trav_ka
syn region	naslFuncXcheck_win_dir_trav_ka	matchgroup=naslFuncXcheck_win_dir_trav_ka start=+check_win_dir_trav_ka\s*(+ end=+)+ contains=naslArgNest,naslArgXcheck_win_dir_trav_ka,@naslArgValues,@naslNestedFunctions
syn match	naslArgXcheck_win_dir_trav_ka	/port\:/ contained
syn match	naslArgXcheck_win_dir_trav_ka	/url\:/ contained
syn match	naslArgXcheck_win_dir_trav_ka	/quickcheck\:/ contained

" is_cgi_installed_ka
syn region	naslFuncXis_cgi_installed_ka	matchgroup=naslFuncXis_cgi_installed_ka start=+is_cgi_installed_ka\s*(+ end=+)+ contains=naslArgNest,naslArgXis_cgi_installed_ka,@naslArgValues,@naslNestedFunctions
syn match	naslArgXis_cgi_installed_ka	/item\:/ contained
syn match	naslArgXis_cgi_installed_ka	/port\:/ contained

" get_http_page
syn region	naslFuncXget_http_page	matchgroup=naslFuncXget_http_page start=+get_http_page\s*(+ end=+)+ contains=naslArgNest,naslArgXget_http_page,@naslArgValues,@naslNestedFunctions
syn match	naslArgXget_http_page	/port\:/ contained
syn match	naslArgXget_http_page	/url\:/ contained
syn match	naslArgXget_http_page	/redirect\:/ contained

" ### default_account.inc

" check_account
syn region	naslFuncXcheck_account	matchgroup=naslFuncXcheck_account start=+check_account\s*(+ end=+)+ contains=naslArgNest,naslArgXcheck_account,@naslArgValues,@naslNestedFunctions
syn match	naslArgXcheck_account	/login\:/ contained
syn match	naslArgXcheck_account	/password\:/ contained


" ### dump.inc
" hexdump
syn region	naslFuncXhexdump	matchgroup=naslFuncXhexdump start=+hexdump\s*(+ end=+)+ contains=naslArgNest,naslArgXhexdump,@naslArgValues,@naslNestedFunctions
syn match	naslArgXhexdump	/ddata\:/ contained

" dump
syn region	naslFuncXdump	matchgroup=naslFuncXdump start=+dump\s*(+ end=+)+ contains=naslArgNest,naslArgXdump,@naslArgValues,@naslNestedFunctions
syn match	naslArgXdump	/ddata\:/ contained
syn match	naslArgXdump	/dtitle\:/ contained

" ### ftp_func.inc
" ftp_close
syn region	naslFuncXftp_close	matchgroup=naslFuncXftp_close start=+ftp_close\s*(+ end=+)+ contains=naslArgNest,naslArgXftp_close,@naslArgValues,@naslNestedFunctions
syn match	naslArgXftp_close	/socket\:/ contained

" get_ftp_banner
syn region	naslFuncXget_ftp_banner	matchgroup=naslFuncXget_ftp_banner start=+get_ftp_banner\s*(+ end=+)+ contains=naslArgNest,naslArgXget_ftp_banner,@naslArgValues,@naslNestedFunctions
syn match	naslArgXget_ftp_banner	/port\:/ contained

" ftp_recv_line
syn region	naslFuncXftp_recv_line	matchgroup=naslFuncXftp_recv_line start=+ftp_recv_line\s*(+ end=+)+ contains=naslArgNest,naslArgXftp_recv_line,@naslArgValues,@naslNestedFunctions
syn match	naslArgXftp_recv_line	/socket\:/ contained

" ftp_recv_listing
syn region	naslFuncXftp_recv_listing	matchgroup=naslFuncXftp_recv_listing start=+ftp_recv_listing\s*(+ end=+)+ contains=naslArgNest,naslArgXftp_recv_listing,@naslArgValues,@naslNestedFunctions
syn match	naslArgXftp_recv_listing	/socket\:/ contained

" ftp_recv_data
syn region	naslFuncXftp_recv_data	matchgroup=naslFuncXftp_recv_data start=+ftp_recv_data\s*(+ end=+)+ contains=naslArgNest,naslArgXftp_recv_data,@naslArgValues,@naslNestedFunctions
syn match	naslArgXftp_recv_data	/socket\:/ contained
syn match	naslArgXftp_recv_data	/line\:/ contained

" ### misc_func.inc
" register_service
syn region	naslFuncXregister_service	matchgroup=naslFuncXregister_service start=+register_service\s*(+ end=+)+ contains=naslArgNest,naslArgXregister_service,@naslArgValues,@naslNestedFunctions
syn match	naslArgXregister_service	/port\:/ contained
syn match	naslArgXregister_service	/proto\:/ contained

" known_service
syn region	naslFuncXknown_service	matchgroup=naslFuncXknown_service start=+known_service\s*(+ end=+)+ contains=naslArgNest,naslArgXknown_service,@naslArgValues,@naslNestedFunctions
syn match	naslArgXknown_service	/port\:/ contained

" set_mysql_version
syn region	naslFuncXset_mysql_version	matchgroup=naslFuncXset_mysql_version start=+set_mysql_version\s*(+ end=+)+ contains=naslArgNest,naslArgXset_mysql_version,@naslArgValues,@naslNestedFunctions
syn match	naslArgXset_mysql_version	/port\:/ contained
syn match	naslArgXset_mysql_version	/version\:/ contained

" get_mysql_version
syn region	naslFuncXget_mysql_version	matchgroup=naslFuncXget_mysql_version start=+get_mysql_version\s*(+ end=+)+ contains=naslArgNest,naslArgXget_mysql_version,@naslArgValues,@naslNestedFunctions
syn match	naslArgXget_mysql_version	/port\:/ contained

" get_unknown_banner
syn region	naslFuncXget_unknown_banner	matchgroup=naslFuncXget_unknown_banner start=+get_unknown_banner\s*(+ end=+)+ contains=naslArgNest,naslArgXget_unknown_banner,@naslArgValues,@naslNestedFunctions
syn match	naslArgXget_unknown_banner	/port\:/ contained
syn match	naslArgXget_unknown_banner	/dontfetch\:/ contained

" set_unknown_banner
syn region	naslFuncXset_unknown_banner	matchgroup=naslFuncXset_unknown_banner start=+set_unknown_banner\s*(+ end=+)+ contains=naslArgNest,naslArgXset_unknown_banner,@naslArgValues,@naslNestedFunctions
syn match	naslArgXset_unknown_banner	/port\:/ contained
syn match	naslArgXset_unknown_banner	/banner\:/ contained

" get_service_banner_line
syn region	naslFuncXget_service_banner_line	matchgroup=naslFuncXget_service_banner_line start=+get_service_banner_line\s*(+ end=+)+ contains=naslArgNest,naslArgXget_service_banner_line,@naslArgValues,@naslNestedFunctions
syn match	naslArgXget_service_banner_line	/service\:/ contained
syn match	naslArgXget_service_banner_line	/port\:/ contained

" get_rpc_port
syn region	naslFuncXget_rpc_port	matchgroup=naslFuncXget_rpc_port start=+get_rpc_port\s*(+ end=+)+ contains=naslArgNest,naslArgXget_rpc_port,@naslArgValues,@naslNestedFunctions
syn match	naslArgXget_rpc_port	/program\:/ contained
syn match	naslArgXget_rpc_port	/protocol\:/ contained
syn match	naslArgXget_rpc_port	/portmap\:/ contained

" rand_str
syn region	naslFuncXrand_str	matchgroup=naslFuncXrand_str start=+rand_str\s*(+ end=+)+ contains=naslArgNest,naslArgXrand_str,@naslArgValues,@naslNestedFunctions
syn match	naslArgXrand_str	/length\:/ contained
syn match	naslArgXrand_str	/charset\:/ contained

" add_port_in_list
syn region	naslFuncXadd_port_in_list	matchgroup=naslFuncXadd_port_in_list start=+add_port_in_list\s*(+ end=+)+ contains=naslArgNest,naslArgXadd_port_in_list,@naslArgValues,@naslNestedFunctions
syn match	naslArgXadd_port_in_list	/list\:/ contained
syn match	naslArgXadd_port_in_list	/port\:/ contained

" is_private_addr
syn region	naslFuncXis_private_addr	matchgroup=naslFuncXis_private_addr start=+is_private_addr\s*(+ end=+)+ contains=naslArgNest,naslArgXis_private_addr,@naslArgValues,@naslNestedFunctions
syn match	naslArgXis_private_addr	/addr\:/ contained

" ### nfs_func.inc
" padsz
syn region	naslFuncXpadsz	matchgroup=naslFuncXpadsz start=+padsz\s*(+ end=+)+ contains=naslArgNest,naslArgXpadsz,@naslArgValues,@naslNestedFunctions
syn match	naslArgXpadsz	/len\:/ contained

" rpclong
syn region	naslFuncXrpclong	matchgroup=naslFuncXrpclong start=+rpclong\s*(+ end=+)+ contains=naslArgNest,naslArgXrpclong,@naslArgValues,@naslNestedFunctions
syn match	naslArgXrpclong	/val\:/ contained

" str2long
syn region	naslFuncXstr2long	matchgroup=naslFuncXstr2long start=+str2long\s*(+ end=+)+ contains=naslArgNest,naslArgXstr2long,@naslArgValues,@naslNestedFunctions
syn match	naslArgXstr2long	/val\:/ contained
syn match	naslArgXstr2long	/idx\:/ contained

" rpcpad
syn region	naslFuncXrpcpad	matchgroup=naslFuncXrpcpad start=+rpcpad\s*(+ end=+)+ contains=naslArgNest,naslArgXrpcpad,@naslArgValues,@naslNestedFunctions
syn match	naslArgXrpcpad	/pad\:/ contained

" mount
syn region	naslFuncXmount	matchgroup=naslFuncXmount start=+mount\s*(+ end=+)+ contains=naslArgNest,naslArgXmount,@naslArgValues,@naslNestedFunctions
syn match	naslArgXmount	/soc\:/ contained
syn match	naslArgXmount	/share\:/ contained

" readdir
syn region	naslFuncXreaddir	matchgroup=naslFuncXreaddir start=+readdir\s*(+ end=+)+ contains=naslArgNest,naslArgXreaddir,@naslArgValues,@naslNestedFunctions
syn match	naslArgXreaddir	/soc\:/ contained
syn match	naslArgXreaddir	/fid\:/ contained

" cwd
syn region	naslFuncXcwd	matchgroup=naslFuncXcwd start=+cwd\s*(+ end=+)+ contains=naslArgNest,naslArgXcwd,@naslArgValues,@naslNestedFunctions
syn match	naslArgXcwd	/soc\:/ contained
syn match	naslArgXcwd	/dir\:/ contained
syn match	naslArgXcwd	/fid\:/ contained

" open
syn region	naslFuncXopen	matchgroup=naslFuncXopen start=+open\s*(+ end=+)+ contains=naslArgNest,naslArgXopen,@naslArgValues,@naslNestedFunctions
syn match	naslArgXopen	/soc\:/ contained
syn match	naslArgXopen	/file\:/ contained
syn match	naslArgXopen	/fid\:/ contained

" read
syn region	naslFuncXread	matchgroup=naslFuncXread start=+read\s*(+ end=+)+ contains=naslArgNest,naslArgXread,@naslArgValues,@naslNestedFunctions
syn match	naslArgXread	/soc\:/ contained
syn match	naslArgXread	/fid\:/ contained
syn match	naslArgXread	/length\:/ contained
syn match	naslArgXread	/off\:/ contained

" umount
syn region	naslFuncXumount	matchgroup=naslFuncXumount start=+umount\s*(+ end=+)+ contains=naslArgNest,naslArgXumount,@naslArgValues,@naslNestedFunctions
syn match	naslArgXumount	/soc\:/ contained
syn match	naslArgXumount	/share\:/ contained

" ### pingpong.inc
" udp_ping_pong
syn region	naslFuncXudp_ping_pong	matchgroup=naslFuncXudp_ping_pong start=+udp_ping_pong\s*(+ end=+)+ contains=naslArgNest,naslArgXudp_ping_pong,@naslArgValues,@naslNestedFunctions
syn match	naslArgXudp_ping_pong	/port\:/ contained
syn match	naslArgXudp_ping_pong	/data\:/ contained
syn match	naslArgXudp_ping_pong	/answer\:/ contained

" ### smb_nt.inc
" kb_smb_name
syn region	naslFuncXkb_smb_name	matchgroup=naslFuncXkb_smb_name start=+kb_smb_name\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" kb_smb_domain
syn region	naslFuncXkb_smb_domain	matchgroup=naslFuncXkb_smb_domain start=+kb_smb_domain\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" kb_smb_login
syn region	naslFuncXkb_smb_login	matchgroup=naslFuncXkb_smb_login start=+kb_smb_login\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" kb_smb_password
syn region	naslFuncXkb_smb_password	matchgroup=naslFuncXkb_smb_password start=+kb_smb_password\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" kb_smb_transport
syn region	naslFuncXkb_smb_transport	matchgroup=naslFuncXkb_smb_transport start=+kb_smb_transport\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" smb_recv
syn region	naslFuncXsmb_recv	matchgroup=naslFuncXsmb_recv start=+smb_recv\s*(+ end=+)+ contains=naslArgNest,naslArgXsmb_recv,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsmb_recv	/socket\:/ contained
syn match	naslArgXsmb_recv	/length\:/ contained

" netbios_name
syn region	naslFuncXnetbios_name	matchgroup=naslFuncXnetbios_name start=+netbios_name\s*(+ end=+)+ contains=naslArgNest,naslArgXnetbios_name,@naslArgValues,@naslNestedFunctions
syn match	naslArgXnetbios_name	/orig\:/ contained

" netbios_redirector_name
syn region	naslFuncXnetbios_redirector_name	matchgroup=naslFuncXnetbios_redirector_name start=+netbios_redirector_name\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" unicode
syn region	naslFuncXunicode	matchgroup=naslFuncXunicode start=+unicode\s*(+ end=+)+ contains=naslArgNest,naslArgXunicode,@naslArgValues,@naslNestedFunctions
syn match	naslArgXunicode	/data\:/ contained

" smb_session_request
syn region	naslFuncXsmb_session_request	matchgroup=naslFuncXsmb_session_request start=+smb_session_request\s*(+ end=+)+ contains=naslArgNest,naslArgXsmb_session_request,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsmb_session_request	/soc\:/ contained
syn match	naslArgXsmb_session_request	/remote\:/ contained

" session_extract_uid
syn region	naslFuncXsession_extract_uid	matchgroup=naslFuncXsession_extract_uid start=+session_extract_uid\s*(+ end=+)+ contains=naslArgNest,naslArgXsession_extract_uid,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsession_extract_uid	/reply\:/ contained

" smb_neg_prot_cleartext
syn region	naslFuncXsmb_neg_prot_cleartext	matchgroup=naslFuncXsmb_neg_prot_cleartext start=+smb_neg_prot_cleartext\s*(+ end=+)+ contains=naslArgNest,naslArgXsmb_neg_prot_cleartext,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsmb_neg_prot_cleartext	/soc\:/ contained

" smb_neg_prot_NTLMv1
syn region	naslFuncXsmb_neg_prot_NTLMv1	matchgroup=naslFuncXsmb_neg_prot_NTLMv1 start=+smb_neg_prot_NTLMv1\s*(+ end=+)+ contains=naslArgNest,naslArgXsmb_neg_prot_NTLMv1,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsmb_neg_prot_NTLMv1	/soc\:/ contained

" smb_neg_prot
syn region	naslFuncXsmb_neg_prot	matchgroup=naslFuncXsmb_neg_prot start=+smb_neg_prot\s*(+ end=+)+ contains=naslArgNest,naslArgXsmb_neg_prot,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsmb_neg_prot	/soc\:/ contained

" smb_neg_prot_value
syn region	naslFuncXsmb_neg_prot_value	matchgroup=naslFuncXsmb_neg_prot_value start=+smb_neg_prot_value\s*(+ end=+)+ contains=naslArgNest,naslArgXsmb_neg_prot_value,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsmb_neg_prot_value	/prot\:/ contained

" smb_neg_prot_cs
syn region	naslFuncXsmb_neg_prot_cs	matchgroup=naslFuncXsmb_neg_prot_cs start=+smb_neg_prot_cs\s*(+ end=+)+ contains=naslArgNest,naslArgXsmb_neg_prot_cs,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsmb_neg_prot_cs	/prot\:/ contained

" smb_neg_prot_domain
syn region	naslFuncXsmb_neg_prot_domain	matchgroup=naslFuncXsmb_neg_prot_domain start=+smb_neg_prot_domain\s*(+ end=+)+ contains=naslArgNest,naslArgXsmb_neg_prot_domain,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsmb_neg_prot_domain	/prot\:/ contained

" smb_session_setup_cleartext
syn region	naslFuncXsmb_session_setup_cleartext	matchgroup=naslFuncXsmb_session_setup_cleartext start=+smb_session_setup_cleartext\s*(+ end=+)+ contains=naslArgNest,naslArgXsmb_session_setup_cleartext,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsmb_session_setup_cleartext	/soc\:/ contained
syn match	naslArgXsmb_session_setup_cleartext	/login\:/ contained
syn match	naslArgXsmb_session_setup_cleartext	/password\:/ contained
syn match	naslArgXsmb_session_setup_cleartext	/domain\:/ contained

" smb_session_setup_NTLMvN
syn region	naslFuncXsmb_session_setup_NTLMvN	matchgroup=naslFuncXsmb_session_setup_NTLMvN start=+smb_session_setup_NTLMvN\s*(+ end=+)+ contains=naslArgNest,naslArgXsmb_session_setup_NTLMvN,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsmb_session_setup_NTLMvN	/soc\:/ contained
syn match	naslArgXsmb_session_setup_NTLMvN	/login\:/ contained
syn match	naslArgXsmb_session_setup_NTLMvN	/password\:/ contained
syn match	naslArgXsmb_session_setup_NTLMvN	/domain\:/ contained
syn match	naslArgXsmb_session_setup_NTLMvN	/cs\:/ contained
syn match	naslArgXsmb_session_setup_NTLMvN	/version\:/ contained

" smb_session_setup
syn region	naslFuncXsmb_session_setup	matchgroup=naslFuncXsmb_session_setup start=+smb_session_setup\s*(+ end=+)+ contains=naslArgNest,naslArgXsmb_session_setup,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsmb_session_setup	/soc\:/ contained
syn match	naslArgXsmb_session_setup	/login\:/ contained
syn match	naslArgXsmb_session_setup	/password\:/ contained
syn match	naslArgXsmb_session_setup	/domain\:/ contained
syn match	naslArgXsmb_session_setup	/prot\:/ contained

" smb_tconx
syn region	naslFuncXsmb_tconx	matchgroup=naslFuncXsmb_tconx start=+smb_tconx\s*(+ end=+)+ contains=naslArgNest,naslArgXsmb_tconx,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsmb_tconx	/soc\:/ contained
syn match	naslArgXsmb_tconx	/name\:/ contained
syn match	naslArgXsmb_tconx	/uid\:/ contained
syn match	naslArgXsmb_tconx	/share\:/ contained

" tconx_extract_tid
syn region	naslFuncXtconx_extract_tid	matchgroup=naslFuncXtconx_extract_tid start=+tconx_extract_tid\s*(+ end=+)+ contains=naslArgNest,naslArgXtconx_extract_tid,@naslArgValues,@naslNestedFunctions
syn match	naslArgXtconx_extract_tid	/reply\:/ contained

" smbntcreatex
syn region	naslFuncXsmbntcreatex	matchgroup=naslFuncXsmbntcreatex start=+smbntcreatex\s*(+ end=+)+ contains=naslArgNest,naslArgXsmbntcreatex,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsmbntcreatex	/soc\:/ contained
syn match	naslArgXsmbntcreatex	/uid\:/ contained
syn match	naslArgXsmbntcreatex	/tid\:/ contained

" smbntcreatex_extract_pipe
syn region	naslFuncXsmbntcreatex_extract_pipe	matchgroup=naslFuncXsmbntcreatex_extract_pipe start=+smbntcreatex_extract_pipe\s*(+ end=+)+ contains=naslArgNest,naslArgXsmbntcreatex_extract_pipe,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsmbntcreatex_extract_pipe	/reply\:/ contained

" pipe_accessible_registry
syn region	naslFuncXpipe_accessible_registry	matchgroup=naslFuncXpipe_accessible_registry start=+pipe_accessible_registry\s*(+ end=+)+ contains=naslArgNest,naslArgXpipe_accessible_registry,@naslArgValues,@naslNestedFunctions
syn match	naslArgXpipe_accessible_registry	/soc\:/ contained
syn match	naslArgXpipe_accessible_registry	/uid\:/ contained
syn match	naslArgXpipe_accessible_registry	/tid\:/ contained
syn match	naslArgXpipe_accessible_registry	/pipe\:/ contained

" registry_access_step_1
syn region	naslFuncXregistry_access_step_1	matchgroup=naslFuncXregistry_access_step_1 start=+registry_access_step_1\s*(+ end=+)+ contains=naslArgNest,naslArgXregistry_access_step_1,@naslArgValues,@naslNestedFunctions
syn match	naslArgXregistry_access_step_1	/soc\:/ contained
syn match	naslArgXregistry_access_step_1	/uid\:/ contained
syn match	naslArgXregistry_access_step_1	/tid\:/ contained
syn match	naslArgXregistry_access_step_1	/pipe\:/ contained

" registry_get_key
syn region	naslFuncXregistry_get_key	matchgroup=naslFuncXregistry_get_key start=+registry_get_key\s*(+ end=+)+ contains=naslArgNest,naslArgXregistry_get_key,@naslArgValues,@naslNestedFunctions
syn match	naslArgXregistry_get_key	/soc\:/ contained
syn match	naslArgXregistry_get_key	/uid\:/ contained
syn match	naslArgXregistry_get_key	/tid\:/ contained
syn match	naslArgXregistry_get_key	/pipe\:/ contained
syn match	naslArgXregistry_get_key	/key\:/ contained
syn match	naslArgXregistry_get_key	/reply\:/ contained

" registry_key_writeable_by_non_admin
syn region	naslFuncXregistry_key_writeable_by_non_admin	matchgroup=naslFuncXregistry_key_writeable_by_non_admin start=+registry_key_writeable_by_non_admin\s*(+ end=+)+ contains=naslArgNest,naslArgXregistry_key_writeable_by_non_admin,@naslArgValues,@naslNestedFunctions
syn match	naslArgXregistry_key_writeable_by_non_admin	/security_descriptor\:/ contained

" registry_get_key_security
syn region	naslFuncXregistry_get_key_security	matchgroup=naslFuncXregistry_get_key_security start=+registry_get_key_security\s*(+ end=+)+ contains=naslArgNest,naslArgXregistry_get_key_security,@naslArgValues,@naslNestedFunctions
syn match	naslArgXregistry_get_key_security	/soc\:/ contained
syn match	naslArgXregistry_get_key_security	/uid\:/ contained
syn match	naslArgXregistry_get_key_security	/tid\:/ contained
syn match	naslArgXregistry_get_key_security	/pipe\:/ contained
syn match	naslArgXregistry_get_key_security	/reply\:/ contained

" registry_get_acl
syn region	naslFuncXregistry_get_acl	matchgroup=naslFuncXregistry_get_acl start=+registry_get_acl\s*(+ end=+)+ contains=naslArgNest,naslArgXregistry_get_acl,@naslArgValues,@naslNestedFunctions
syn match	naslArgXregistry_get_acl	/key\:/ contained

" unicode2
syn region	naslFuncXunicode2	matchgroup=naslFuncXunicode2 start=+unicode2\s*(+ end=+)+ contains=naslArgNest,naslArgXunicode2,@naslArgValues,@naslNestedFunctions
syn match	naslArgXunicode2	/data\:/ contained

" registry_get_item_sz
syn region	naslFuncXregistry_get_item_sz	matchgroup=naslFuncXregistry_get_item_sz start=+registry_get_item_sz\s*(+ end=+)+ contains=naslArgNest,naslArgXregistry_get_item_sz,@naslArgValues,@naslNestedFunctions
syn match	naslArgXregistry_get_item_sz	/soc\:/ contained
syn match	naslArgXregistry_get_item_sz	/uid\:/ contained
syn match	naslArgXregistry_get_item_sz	/tid\:/ contained
syn match	naslArgXregistry_get_item_sz	/pipe\:/ contained
syn match	naslArgXregistry_get_item_sz	/item\:/ contained
syn match	naslArgXregistry_get_item_sz	/reply\:/ contained

" registry_decode_sz
syn region	naslFuncXregistry_decode_sz	matchgroup=naslFuncXregistry_decode_sz start=+registry_decode_sz\s*(+ end=+)+ contains=naslArgNest,naslArgXregistry_decode_sz,@naslArgValues,@naslNestedFunctions
syn match	naslArgXregistry_decode_sz	/data\:/ contained

" registry_get_item_dword
syn region	naslFuncXregistry_get_item_dword	matchgroup=naslFuncXregistry_get_item_dword start=+registry_get_item_dword\s*(+ end=+)+ contains=naslArgNest,naslArgXregistry_get_item_dword,@naslArgValues,@naslNestedFunctions
syn match	naslArgXregistry_get_item_dword	/soc\:/ contained
syn match	naslArgXregistry_get_item_dword	/uid\:/ contained
syn match	naslArgXregistry_get_item_dword	/tid\:/ contained
syn match	naslArgXregistry_get_item_dword	/pipe\:/ contained
syn match	naslArgXregistry_get_item_dword	/item\:/ contained
syn match	naslArgXregistry_get_item_dword	/reply\:/ contained

" registry_decode_dword
syn region	naslFuncXregistry_decode_dword	matchgroup=naslFuncXregistry_decode_dword start=+registry_decode_dword\s*(+ end=+)+ contains=naslArgNest,naslArgXregistry_decode_dword,@naslArgValues,@naslNestedFunctions
syn match	naslArgXregistry_decode_dword	/data\:/ contained

" registry_get_dword
syn region	naslFuncXregistry_get_dword	matchgroup=naslFuncXregistry_get_dword start=+registry_get_dword\s*(+ end=+)+ contains=naslArgNest,naslArgXregistry_get_dword,@naslArgValues,@naslNestedFunctions
syn match	naslArgXregistry_get_dword	/key\:/ contained
syn match	naslArgXregistry_get_dword	/item\:/ contained

" registry_get_sz
syn region	naslFuncXregistry_get_sz	matchgroup=naslFuncXregistry_get_sz start=+registry_get_sz\s*(+ end=+)+ contains=naslArgNest,naslArgXregistry_get_sz,@naslArgValues,@naslNestedFunctions
syn match	naslArgXregistry_get_sz	/key\:/ contained
syn match	naslArgXregistry_get_sz	/item\:/ contained

" OpenPipeToSamr
syn region	naslFuncXOpenPipeToSamr	matchgroup=naslFuncXOpenPipeToSamr start=+OpenPipeToSamr\s*(+ end=+)+ contains=naslArgNest,naslArgXOpenPipeToSamr,@naslArgValues,@naslNestedFunctions
syn match	naslArgXOpenPipeToSamr	/soc\:/ contained
syn match	naslArgXOpenPipeToSamr	/uid\:/ contained
syn match	naslArgXOpenPipeToSamr	/tid\:/ contained

" samr_smbwritex
syn region	naslFuncXsamr_smbwritex	matchgroup=naslFuncXsamr_smbwritex start=+samr_smbwritex\s*(+ end=+)+ contains=naslArgNest,naslArgXsamr_smbwritex,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsamr_smbwritex	/soc\:/ contained
syn match	naslArgXsamr_smbwritex	/tid\:/ contained
syn match	naslArgXsamr_smbwritex	/uid\:/ contained
syn match	naslArgXsamr_smbwritex	/pipe\:/ contained

" samr_smbreadx
syn region	naslFuncXsamr_smbreadx	matchgroup=naslFuncXsamr_smbreadx start=+samr_smbreadx\s*(+ end=+)+ contains=naslArgNest,naslArgXsamr_smbreadx,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsamr_smbreadx	/soc\:/ contained
syn match	naslArgXsamr_smbreadx	/tid\:/ contained
syn match	naslArgXsamr_smbreadx	/uid\:/ contained
syn match	naslArgXsamr_smbreadx	/pipe\:/ contained

" samr_uc
syn region	naslFuncXsamr_uc	matchgroup=naslFuncXsamr_uc start=+samr_uc\s*(+ end=+)+ contains=naslArgNest,naslArgXsamr_uc,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsamr_uc	/name\:/ contained

" SamrConnect2
syn region	naslFuncXSamrConnect2	matchgroup=naslFuncXSamrConnect2 start=+SamrConnect2\s*(+ end=+)+ contains=naslArgNest,naslArgXSamrConnect2,@naslArgValues,@naslNestedFunctions
syn match	naslArgXSamrConnect2	/soc\:/ contained
syn match	naslArgXSamrConnect2	/tid\:/ contained
syn match	naslArgXSamrConnect2	/uid\:/ contained
syn match	naslArgXSamrConnect2	/pipe\:/ contained
syn match	naslArgXSamrConnect2	/name\:/ contained

" SamrDom2Sid
syn region	naslFuncXSamrDom2Sid	matchgroup=naslFuncXSamrDom2Sid start=+SamrDom2Sid\s*(+ end=+)+ contains=naslArgNest,naslArgXSamrDom2Sid,@naslArgValues,@naslNestedFunctions
syn match	naslArgXSamrDom2Sid	/soc\:/ contained
syn match	naslArgXSamrDom2Sid	/tid\:/ contained
syn match	naslArgXSamrDom2Sid	/uid\:/ contained
syn match	naslArgXSamrDom2Sid	/pipe\:/ contained
syn match	naslArgXSamrDom2Sid	/samrhdl\:/ contained
syn match	naslArgXSamrDom2Sid	/dom\:/ contained

" SamrOpenDomain
syn region	naslFuncXSamrOpenDomain	matchgroup=naslFuncXSamrOpenDomain start=+SamrOpenDomain\s*(+ end=+)+ contains=naslArgNest,naslArgXSamrOpenDomain,@naslArgValues,@naslNestedFunctions
syn match	naslArgXSamrOpenDomain	/soc\:/ contained
syn match	naslArgXSamrOpenDomain	/tid\:/ contained
syn match	naslArgXSamrOpenDomain	/uid\:/ contained
syn match	naslArgXSamrOpenDomain	/pipe\:/ contained
syn match	naslArgXSamrOpenDomain	/samrhdl\:/ contained
syn match	naslArgXSamrOpenDomain	/sid\:/ contained

" SamrOpenBuiltin
syn region	naslFuncXSamrOpenBuiltin	matchgroup=naslFuncXSamrOpenBuiltin start=+SamrOpenBuiltin\s*(+ end=+)+ contains=naslArgNest,naslArgXSamrOpenBuiltin,@naslArgValues,@naslNestedFunctions
syn match	naslArgXSamrOpenBuiltin	/soc\:/ contained
syn match	naslArgXSamrOpenBuiltin	/tid\:/ contained
syn match	naslArgXSamrOpenBuiltin	/uid\:/ contained
syn match	naslArgXSamrOpenBuiltin	/pipe\:/ contained
syn match	naslArgXSamrOpenBuiltin	/samrhdl\:/ contained

" SamrLookupNames
syn region	naslFuncXSamrLookupNames	matchgroup=naslFuncXSamrLookupNames start=+SamrLookupNames\s*(+ end=+)+ contains=naslArgNest,naslArgXSamrLookupNames,@naslArgValues,@naslNestedFunctions
syn match	naslArgXSamrLookupNames	/soc\:/ contained
syn match	naslArgXSamrLookupNames	/uid\:/ contained
syn match	naslArgXSamrLookupNames	/tid\:/ contained
syn match	naslArgXSamrLookupNames	/pipe\:/ contained
syn match	naslArgXSamrLookupNames	/name\:/ contained
syn match	naslArgXSamrLookupNames	/domhdl\:/ contained

" SamrOpenUser
syn region	naslFuncXSamrOpenUser	matchgroup=naslFuncXSamrOpenUser start=+SamrOpenUser\s*(+ end=+)+ contains=naslArgNest,naslArgXSamrOpenUser,@naslArgValues,@naslNestedFunctions
syn match	naslArgXSamrOpenUser	/soc\:/ contained
syn match	naslArgXSamrOpenUser	/uid\:/ contained
syn match	naslArgXSamrOpenUser	/tid\:/ contained
syn match	naslArgXSamrOpenUser	/pipe\:/ contained
syn match	naslArgXSamrOpenUser	/samrhdl\:/ contained
syn match	naslArgXSamrOpenUser	/rid\:/ contained

" SamrQueryUserGroups
syn region	naslFuncXSamrQueryUserGroups	matchgroup=naslFuncXSamrQueryUserGroups start=+SamrQueryUserGroups\s*(+ end=+)+ contains=naslArgNest,naslArgXSamrQueryUserGroups,@naslArgValues,@naslNestedFunctions
syn match	naslArgXSamrQueryUserGroups	/soc\:/ contained
syn match	naslArgXSamrQueryUserGroups	/uid\:/ contained
syn match	naslArgXSamrQueryUserGroups	/tid\:/ contained
syn match	naslArgXSamrQueryUserGroups	/pipe\:/ contained
syn match	naslArgXSamrQueryUserGroups	/usrhdl\:/ contained

" SamrQueryUserInfo
syn region	naslFuncXSamrQueryUserInfo	matchgroup=naslFuncXSamrQueryUserInfo start=+SamrQueryUserInfo\s*(+ end=+)+ contains=naslArgNest,naslArgXSamrQueryUserInfo,@naslArgValues,@naslNestedFunctions
syn match	naslArgXSamrQueryUserInfo	/soc\:/ contained
syn match	naslArgXSamrQueryUserInfo	/uid\:/ contained
syn match	naslArgXSamrQueryUserInfo	/tid\:/ contained
syn match	naslArgXSamrQueryUserInfo	/pipe\:/ contained
syn match	naslArgXSamrQueryUserInfo	/usrhdl\:/ contained

" SamrQueryUserAliases
syn region	naslFuncXSamrQueryUserAliases	matchgroup=naslFuncXSamrQueryUserAliases start=+SamrQueryUserAliases\s*(+ end=+)+ contains=naslArgNest,naslArgXSamrQueryUserAliases,@naslArgValues,@naslNestedFunctions
syn match	naslArgXSamrQueryUserAliases	/soc\:/ contained
syn match	naslArgXSamrQueryUserAliases	/uid\:/ contained
syn match	naslArgXSamrQueryUserAliases	/tid\:/ contained
syn match	naslArgXSamrQueryUserAliases	/pipe\:/ contained
syn match	naslArgXSamrQueryUserAliases	/usrhdl\:/ contained
syn match	naslArgXSamrQueryUserAliases	/sid\:/ contained
syn match	naslArgXSamrQueryUserAliases	/rid\:/ contained

" OpenAndX
syn region	naslFuncXOpenAndX	matchgroup=naslFuncXOpenAndX start=+OpenAndX\s*(+ end=+)+ contains=naslArgNest,naslArgXOpenAndX,@naslArgValues,@naslNestedFunctions
syn match	naslArgXOpenAndX	/socket\:/ contained
syn match	naslArgXOpenAndX	/uid\:/ contained
syn match	naslArgXOpenAndX	/tid\:/ contained
syn match	naslArgXOpenAndX	/file\:/ contained

" ReadAndX
syn region	naslFuncXReadAndX	matchgroup=naslFuncXReadAndX start=+ReadAndX\s*(+ end=+)+ contains=naslArgNest,naslArgXReadAndX,@naslArgValues,@naslNestedFunctions
syn match	naslArgXReadAndX	/socket\:/ contained
syn match	naslArgXReadAndX	/uid\:/ contained
syn match	naslArgXReadAndX	/tid\:/ contained
syn match	naslArgXReadAndX	/fid\:/ contained
syn match	naslArgXReadAndX	/count\:/ contained
syn match	naslArgXReadAndX	/off\:/ contained

" smb_get_file_size
syn region	naslFuncXsmb_get_file_size	matchgroup=naslFuncXsmb_get_file_size start=+smb_get_file_size\s*(+ end=+)+ contains=naslArgNest,naslArgXsmb_get_file_size,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsmb_get_file_size	/socket\:/ contained
syn match	naslArgXsmb_get_file_size	/uid\:/ contained
syn match	naslArgXsmb_get_file_size	/tid\:/ contained
syn match	naslArgXsmb_get_file_size	/fid\:/ contained

" FindFirst2
syn region	naslFuncXFindFirst2	matchgroup=naslFuncXFindFirst2 start=+FindFirst2\s*(+ end=+)+ contains=naslArgNest,naslArgXFindFirst2,@naslArgValues,@naslNestedFunctions
syn match	naslArgXFindFirst2	/socket\:/ contained
syn match	naslArgXFindFirst2	/uid\:/ contained
syn match	naslArgXFindFirst2	/tid\:/ contained
syn match	naslArgXFindFirst2	/pattern\:/ contained

" ### smtp_func.inc
" smtp_close
syn region	naslFuncXsmtp_close	matchgroup=naslFuncXsmtp_close start=+smtp_close\s*(+ end=+)+ contains=naslArgNest,naslArgXsmtp_close,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsmtp_close	/socket\:/ contained

" smtp_open
syn region	naslFuncXsmtp_open	matchgroup=naslFuncXsmtp_open start=+smtp_open\s*(+ end=+)+ contains=naslArgNest,naslArgXsmtp_open,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsmtp_open	/port\:/ contained
syn match	naslArgXsmtp_open	/helo\:/ contained

" smtp_send_socket
syn region	naslFuncXsmtp_send_socket	matchgroup=naslFuncXsmtp_send_socket start=+smtp_send_socket\s*(+ end=+)+ contains=naslArgNest,naslArgXsmtp_send_socket,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsmtp_send_socket	/socket\:/ contained
syn match	naslArgXsmtp_send_socket	/from\:/ contained
syn match	naslArgXsmtp_send_socket	/to\:/ contained
syn match	naslArgXsmtp_send_socket	/body\:/ contained

" smtp_send_port
syn region	naslFuncXsmtp_send_port	matchgroup=naslFuncXsmtp_send_port start=+smtp_send_port\s*(+ end=+)+ contains=naslArgNest,naslArgXsmtp_send_port,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsmtp_send_port	/port\:/ contained
syn match	naslArgXsmtp_send_port	/from\:/ contained
syn match	naslArgXsmtp_send_port	/to\:/ contained
syn match	naslArgXsmtp_send_port	/body\:/ contained

" smtp_from_header
syn region	naslFuncXsmtp_from_header	matchgroup=naslFuncXsmtp_from_header start=+smtp_from_header\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" smtp_to_header
syn region	naslFuncXsmtp_to_header	matchgroup=naslFuncXsmtp_to_header start=+smtp_to_header\s*(+ end=+)+ contains=naslArgNest,@naslArgValues,@naslNestedFunctions

" get_smtp_banner
syn region	naslFuncXget_smtp_banner	matchgroup=naslFuncXget_smtp_banner start=+get_smtp_banner\s*(+ end=+)+ contains=naslArgNest,naslArgXget_smtp_banner,@naslArgValues,@naslNestedFunctions
syn match	naslArgXget_smtp_banner	/port\:/ contained

" smtp_recv_line
syn region	naslFuncXsmtp_recv_line	matchgroup=naslFuncXsmtp_recv_line start=+smtp_recv_line\s*(+ end=+)+ contains=naslArgNest,naslArgXsmtp_recv_line,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsmtp_recv_line	/socket\:/ contained
syn match	naslArgXsmtp_recv_line	/code\:/ contained

" smtp_recv_banner
syn region	naslFuncXsmtp_recv_banner	matchgroup=naslFuncXsmtp_recv_banner start=+smtp_recv_banner\s*(+ end=+)+ contains=naslArgNest,naslArgXsmtp_recv_banner,@naslArgValues,@naslNestedFunctions
syn match	naslArgXsmtp_recv_banner	/socket\:/ contained

" ### telnet_func.inc
" get_telnet_banner
syn region	naslFuncXget_telnet_banner	matchgroup=naslFuncXget_telnet_banner start=+get_telnet_banner\s*(+ end=+)+ contains=naslArgNest,naslArgXget_telnet_banner,@naslArgValues,@naslNestedFunctions
syn match	naslArgXget_telnet_banner	/port\:/ contained

" set_telnet_banner
syn region	naslFuncXset_telnet_banner	matchgroup=naslFuncXset_telnet_banner start=+set_telnet_banner\s*(+ end=+)+ contains=naslArgNest,naslArgXset_telnet_banner,@naslArgValues,@naslNestedFunctions
syn match	naslArgXset_telnet_banner	/port\:/ contained
syn match	naslArgXset_telnet_banner	/banner\:/ contained

" ### uddi.inc
" create_uddi_xml 
syn region	naslFuncXcreate_uddi_xml 	matchgroup=naslFuncXcreate_uddi_xml  start=+create_uddi_xml \s*(+ end=+)+ contains=naslArgNest,naslArgXcreate_uddi_xml ,@naslArgValues,@naslNestedFunctions
syn match	naslArgXcreate_uddi_xml 	/ktype\:/ contained
syn match	naslArgXcreate_uddi_xml 	/path\:/ contained
syn match	naslArgXcreate_uddi_xml 	/key\:/ contained
syn match	naslArgXcreate_uddi_xml 	/name\:/ contained

" ###############
"  End Functions
" ###############

" Which of the above functions are nested inside other ones?
syn cluster naslNestedFunctions	contains=naslFuncXstring,naslFuncXraw_string,naslFuncXcrap,naslFuncXrand,naslFuncXget_kb_item

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
  HiLink naslSQString		String
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

" ## Functions from include files
" http_func.inc
  HiLink naslArgXhex2dec	naslFunctionArgs
  HiLink	naslFuncXhex2dec	naslFunctionCalls
  HiLink naslArgXget_http_banner	naslFunctionArgs
  HiLink	naslFuncXget_http_banner	naslFunctionCalls
  HiLink naslArgXget_http_port	naslFunctionArgs
  HiLink	naslFuncXget_http_port	naslFunctionCalls
  HiLink naslArgXphp_ver_match	naslFunctionArgs
  HiLink	naslFuncXphp_ver_match	naslFunctionCalls
  HiLink naslArgXhttp_is_dead	naslFunctionArgs
  HiLink	naslFuncXhttp_is_dead	naslFunctionCalls
  HiLink naslArgXcheck_win_dir_trav	naslFunctionArgs
  HiLink	naslFuncXcheck_win_dir_trav	naslFunctionCalls
  HiLink naslArgXhttp_recv_body	naslFunctionArgs
  HiLink	naslFuncXhttp_recv_body	naslFunctionCalls
  HiLink naslArgXhttp_recv	naslFunctionArgs
  HiLink	naslFuncXhttp_recv	naslFunctionCalls
  HiLink naslArgXhttp_recv_length	naslFunctionArgs
  HiLink	naslFuncXhttp_recv_length	naslFunctionCalls
  HiLink	naslFuncXcgi_dirs	naslFunctionCalls

" http_keepalive.inc
  HiLink naslArgXhttp_keepalive_check_connection	naslFunctionArgs
  HiLink	naslFuncXhttp_keepalive_check_connection	naslFunctionCalls
  HiLink naslArgXenable_keepalive	naslFunctionArgs
  HiLink	naslFuncXenable_keepalive	naslFunctionCalls
  HiLink naslArgXhttp_keepalive_enabled	naslFunctionArgs
  HiLink	naslFuncXhttp_keepalive_enabled	naslFunctionCalls
  HiLink naslArgXhttp_keepalive_recv	naslFunctionArgs
  HiLink	naslFuncXhttp_keepalive_recv	naslFunctionCalls
  HiLink	naslFuncXon_exit	naslFunctionCalls
  HiLink naslArgXhttp_keepalive_send_recv	naslFunctionArgs
  HiLink	naslFuncXhttp_keepalive_send_recv	naslFunctionCalls
  HiLink naslArgXcheck_win_dir_trav_ka	naslFunctionArgs
  HiLink	naslFuncXcheck_win_dir_trav_ka	naslFunctionCalls
  HiLink naslArgXis_cgi_installed_ka	naslFunctionArgs
  HiLink	naslFuncXis_cgi_installed_ka	naslFunctionCalls
  HiLink naslArgXget_http_page	naslFunctionArgs
  HiLink	naslFuncXget_http_page	naslFunctionCalls
" default_account.inc
  HiLink naslArgXcheck_account	naslFunctionArgs
  HiLink	naslFuncXcheck_account	naslFunctionCalls
" dump.inc
  HiLink naslArgXhexdump	naslFunctionArgs
  HiLink	naslFuncXhexdump	naslFunctionCalls
  HiLink naslArgXdump	naslFunctionArgs
  HiLink	naslFuncXdump	naslFunctionCalls
" ftp_func.inc
  HiLink naslArgXftp_close	naslFunctionArgs
  HiLink	naslFuncXftp_close	naslFunctionCalls
  HiLink naslArgXget_ftp_banner	naslFunctionArgs
  HiLink	naslFuncXget_ftp_banner	naslFunctionCalls
  HiLink naslArgXftp_recv_line	naslFunctionArgs
  HiLink	naslFuncXftp_recv_line	naslFunctionCalls
  HiLink naslArgXftp_recv_listing	naslFunctionArgs
  HiLink	naslFuncXftp_recv_listing	naslFunctionCalls
  HiLink naslArgXftp_recv_data	naslFunctionArgs
  HiLink	naslFuncXftp_recv_data	naslFunctionCalls
" misc_func.inc
  HiLink naslArgXregister_service	naslFunctionArgs
  HiLink	naslFuncXregister_service	naslFunctionCalls
  HiLink naslArgXknown_service	naslFunctionArgs
  HiLink	naslFuncXknown_service	naslFunctionCalls
  HiLink naslArgXset_mysql_version	naslFunctionArgs
  HiLink	naslFuncXset_mysql_version	naslFunctionCalls
  HiLink naslArgXget_mysql_version	naslFunctionArgs
  HiLink	naslFuncXget_mysql_version	naslFunctionCalls
  HiLink naslArgXget_unknown_banner	naslFunctionArgs
  HiLink	naslFuncXget_unknown_banner	naslFunctionCalls
  HiLink naslArgXset_unknown_banner	naslFunctionArgs
  HiLink	naslFuncXset_unknown_banner	naslFunctionCalls
  HiLink naslArgXget_service_banner_line	naslFunctionArgs
  HiLink	naslFuncXget_service_banner_line	naslFunctionCalls
  HiLink naslArgXget_rpc_port	naslFunctionArgs
  HiLink	naslFuncXget_rpc_port	naslFunctionCalls
  HiLink naslArgXrand_str	naslFunctionArgs
  HiLink	naslFuncXrand_str	naslFunctionCalls
  HiLink naslArgXadd_port_in_list	naslFunctionArgs
  HiLink	naslFuncXadd_port_in_list	naslFunctionCalls
  HiLink naslArgXis_private_addr	naslFunctionArgs
  HiLink	naslFuncXis_private_addr	naslFunctionCalls
" nfs_func.inc
  HiLink naslArgXpadsz	naslFunctionArgs
  HiLink	naslFuncXpadsz	naslFunctionCalls
  HiLink naslArgXrpclong	naslFunctionArgs
  HiLink	naslFuncXrpclong	naslFunctionCalls
  HiLink naslArgXstr2long	naslFunctionArgs
  HiLink	naslFuncXstr2long	naslFunctionCalls
  HiLink naslArgXrpcpad	naslFunctionArgs
  HiLink	naslFuncXrpcpad	naslFunctionCalls
  HiLink naslArgXmount	naslFunctionArgs
  HiLink	naslFuncXmount	naslFunctionCalls
  HiLink naslArgXreaddir	naslFunctionArgs
  HiLink	naslFuncXreaddir	naslFunctionCalls
  HiLink naslArgXcwd	naslFunctionArgs
  HiLink	naslFuncXcwd	naslFunctionCalls
  HiLink naslArgXopen	naslFunctionArgs
  HiLink	naslFuncXopen	naslFunctionCalls
  HiLink naslArgXread	naslFunctionArgs
  HiLink	naslFuncXread	naslFunctionCalls
  HiLink naslArgXumount	naslFunctionArgs
  HiLink	naslFuncXumount	naslFunctionCalls
" ping_pong.inc
  HiLink naslArgXudp_ping_pong	naslFunctionArgs
" smb_nt.inc
  HiLink	naslFuncXkb_smb_name	naslFunctionCalls
  HiLink	naslFuncXkb_smb_domain	naslFunctionCalls
  HiLink	naslFuncXkb_smb_login	naslFunctionCalls
  HiLink	naslFuncXkb_smb_password	naslFunctionCalls
  HiLink	naslFuncXkb_smb_transport	naslFunctionCalls
  HiLink naslArgXsmb_recv	naslFunctionArgs
  HiLink	naslFuncXsmb_recv	naslFunctionCalls
  HiLink naslArgXnetbios_name	naslFunctionArgs
  HiLink	naslFuncXnetbios_name	naslFunctionCalls
  HiLink	naslFuncXnetbios_redirector_name	naslFunctionCalls
  HiLink naslArgXunicode	naslFunctionArgs
  HiLink	naslFuncXunicode	naslFunctionCalls
  HiLink naslArgXsmb_session_request	naslFunctionArgs
  HiLink	naslFuncXsmb_session_request	naslFunctionCalls
  HiLink naslArgXsession_extract_uid	naslFunctionArgs
  HiLink	naslFuncXsession_extract_uid	naslFunctionCalls
  HiLink naslArgXsmb_neg_prot_cleartext	naslFunctionArgs
  HiLink	naslFuncXsmb_neg_prot_cleartext	naslFunctionCalls
  HiLink naslArgXsmb_neg_prot_NTLMv1	naslFunctionArgs
  HiLink	naslFuncXsmb_neg_prot_NTLMv1	naslFunctionCalls
  HiLink naslArgXsmb_neg_prot	naslFunctionArgs
  HiLink	naslFuncXsmb_neg_prot	naslFunctionCalls
  HiLink naslArgXsmb_neg_prot_value	naslFunctionArgs
  HiLink	naslFuncXsmb_neg_prot_value	naslFunctionCalls
  HiLink naslArgXsmb_neg_prot_cs	naslFunctionArgs
  HiLink	naslFuncXsmb_neg_prot_cs	naslFunctionCalls
  HiLink naslArgXsmb_neg_prot_domain	naslFunctionArgs
  HiLink	naslFuncXsmb_neg_prot_domain	naslFunctionCalls
  HiLink naslArgXsmb_session_setup_cleartext	naslFunctionArgs
  HiLink	naslFuncXsmb_session_setup_cleartext	naslFunctionCalls
  HiLink naslArgXsmb_session_setup_NTLMvN	naslFunctionArgs
  HiLink	naslFuncXsmb_session_setup_NTLMvN	naslFunctionCalls
  HiLink naslArgXsmb_session_setup	naslFunctionArgs
  HiLink	naslFuncXsmb_session_setup	naslFunctionCalls
  HiLink naslArgXsmb_tconx	naslFunctionArgs
  HiLink	naslFuncXsmb_tconx	naslFunctionCalls
  HiLink naslArgXtconx_extract_tid	naslFunctionArgs
  HiLink	naslFuncXtconx_extract_tid	naslFunctionCalls
  HiLink naslArgXsmbntcreatex	naslFunctionArgs
  HiLink	naslFuncXsmbntcreatex	naslFunctionCalls
  HiLink naslArgXsmbntcreatex_extract_pipe	naslFunctionArgs
  HiLink	naslFuncXsmbntcreatex_extract_pipe	naslFunctionCalls
  HiLink naslArgXpipe_accessible_registry	naslFunctionArgs
  HiLink	naslFuncXpipe_accessible_registry	naslFunctionCalls
  HiLink naslArgXregistry_access_step_1	naslFunctionArgs
  HiLink	naslFuncXregistry_access_step_1	naslFunctionCalls
  HiLink naslArgXregistry_get_key	naslFunctionArgs
  HiLink	naslFuncXregistry_get_key	naslFunctionCalls
  HiLink naslArgXregistry_key_writeable_by_non_admin	naslFunctionArgs
  HiLink	naslFuncXregistry_key_writeable_by_non_admin	naslFunctionCalls
  HiLink naslArgXregistry_get_key_security	naslFunctionArgs
  HiLink	naslFuncXregistry_get_key_security	naslFunctionCalls
  HiLink naslArgXregistry_get_acl	naslFunctionArgs
  HiLink	naslFuncXregistry_get_acl	naslFunctionCalls
  HiLink naslArgXunicode2	naslFunctionArgs
  HiLink	naslFuncXunicode2	naslFunctionCalls
  HiLink naslArgXregistry_get_item_sz	naslFunctionArgs
  HiLink	naslFuncXregistry_get_item_sz	naslFunctionCalls
  HiLink naslArgXregistry_decode_sz	naslFunctionArgs
  HiLink	naslFuncXregistry_decode_sz	naslFunctionCalls
  HiLink naslArgXregistry_get_item_dword	naslFunctionArgs
  HiLink	naslFuncXregistry_get_item_dword	naslFunctionCalls
  HiLink naslArgXregistry_decode_dword	naslFunctionArgs
  HiLink	naslFuncXregistry_decode_dword	naslFunctionCalls
  HiLink naslArgXregistry_get_dword	naslFunctionArgs
  HiLink	naslFuncXregistry_get_dword	naslFunctionCalls
  HiLink naslArgXregistry_get_sz	naslFunctionArgs
  HiLink	naslFuncXregistry_get_sz	naslFunctionCalls
  HiLink naslArgXOpenPipeToSamr	naslFunctionArgs
  HiLink	naslFuncXOpenPipeToSamr	naslFunctionCalls
  HiLink naslArgXsamr_smbwritex	naslFunctionArgs
  HiLink	naslFuncXsamr_smbwritex	naslFunctionCalls
  HiLink naslArgXsamr_smbreadx	naslFunctionArgs
  HiLink	naslFuncXsamr_smbreadx	naslFunctionCalls
  HiLink naslArgXsamr_uc	naslFunctionArgs
  HiLink	naslFuncXsamr_uc	naslFunctionCalls
  HiLink naslArgXSamrConnect2	naslFunctionArgs
  HiLink	naslFuncXSamrConnect2	naslFunctionCalls
  HiLink naslArgXSamrDom2Sid	naslFunctionArgs
  HiLink	naslFuncXSamrDom2Sid	naslFunctionCalls
  HiLink naslArgXSamrOpenDomain	naslFunctionArgs
  HiLink	naslFuncXSamrOpenDomain	naslFunctionCalls
  HiLink naslArgXSamrOpenBuiltin	naslFunctionArgs
  HiLink	naslFuncXSamrOpenBuiltin	naslFunctionCalls
  HiLink naslArgXSamrLookupNames	naslFunctionArgs
  HiLink	naslFuncXSamrLookupNames	naslFunctionCalls
  HiLink naslArgXSamrOpenUser	naslFunctionArgs
  HiLink	naslFuncXSamrOpenUser	naslFunctionCalls
  HiLink naslArgXSamrQueryUserGroups	naslFunctionArgs
  HiLink	naslFuncXSamrQueryUserGroups	naslFunctionCalls
  HiLink naslArgXSamrQueryUserInfo	naslFunctionArgs
  HiLink	naslFuncXSamrQueryUserInfo	naslFunctionCalls
  HiLink naslArgXSamrQueryUserAliases	naslFunctionArgs
  HiLink	naslFuncXSamrQueryUserAliases	naslFunctionCalls
  HiLink naslArgXOpenAndX	naslFunctionArgs
  HiLink	naslFuncXOpenAndX	naslFunctionCalls
  HiLink naslArgXReadAndX	naslFunctionArgs
  HiLink	naslFuncXReadAndX	naslFunctionCalls
  HiLink naslArgXsmb_get_file_size	naslFunctionArgs
  HiLink	naslFuncXsmb_get_file_size	naslFunctionCalls
  HiLink naslArgXFindFirst2	naslFunctionArgs
  HiLink	naslFuncXFindFirst2	naslFunctionCalls
" smtp_func.inc
  HiLink naslArgXsmtp_close	naslFunctionArgs
  HiLink	naslFuncXsmtp_close	naslFunctionCalls
  HiLink naslArgXsmtp_open	naslFunctionArgs
  HiLink	naslFuncXsmtp_open	naslFunctionCalls
  HiLink naslArgXsmtp_send_socket	naslFunctionArgs
  HiLink	naslFuncXsmtp_send_socket	naslFunctionCalls
  HiLink naslArgXsmtp_send_port	naslFunctionArgs
  HiLink	naslFuncXsmtp_send_port	naslFunctionCalls
  HiLink	naslFuncXsmtp_from_header	naslFunctionCalls
  HiLink	naslFuncXsmtp_to_header	naslFunctionCalls
  HiLink naslArgXget_smtp_banner	naslFunctionArgs
  HiLink	naslFuncXget_smtp_banner	naslFunctionCalls
  HiLink naslArgXsmtp_recv_line	naslFunctionArgs
  HiLink	naslFuncXsmtp_recv_line	naslFunctionCalls
  HiLink naslArgXsmtp_recv_banner	naslFunctionArgs
  HiLink	naslFuncXsmtp_recv_banner	naslFunctionCalls
" telnet_func.inc
  HiLink naslArgXget_telnet_banner	naslFunctionArgs
  HiLink	naslFuncXget_telnet_banner	naslFunctionCalls
  HiLink naslArgXset_telnet_banner	naslFunctionArgs
  HiLink	naslFuncXset_telnet_banner	naslFunctionCalls
" uddi.inc
  HiLink naslArgXcreate_uddi_xml 	naslFunctionArgs
  HiLink	naslFuncXcreate_uddi_xml 	naslFunctionCalls

  delcommand HiLink
endif

let b:current_syntax = "nasl"

" vim: ts=8
"
"

