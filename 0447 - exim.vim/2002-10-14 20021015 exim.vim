" Vim syntax file
" Language: Exim configuration file exim.conf
" Maintainer: David Ne\v{c}as (Yeti) <yeti@physics.muni.cz>
" Last Change: 2002-10-15
" URL: http://trific.ath.cx/Ftp/vim/syntax/exim.vim

" Note: This is a pretty dumb heap of keywords.
" TODO: Too many thgings.

" Setup {{{
" React to possibly already-defined syntax.
" For version 5.x: Clear all syntax items unconditionally
" For version 6.x: Quit when a syntax file was already loaded
if version >= 600
  if exists("b:current_syntax")
    finish
  endif
else
  syntax clear
endif

syn case match
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" }}}
" Base constructs {{{
syn match eximComment "^\s*#.*$" contains=eximFixme
syn match eximComment "\s#.*$" contains=eximFixme
syn keyword eximFixme FIXME TODO XXX NOT contained
syn keyword eximConstant true false yes no
syn match eximNumber "\<\d\+[KM]\?\>"
syn match eximNumber "\<0[xX]\x\+\>"
syn match eximNumber "\<\d\+\(\.\d\{,3}\)\?\>"
syn match eximTime "\<\(\d\+[wdhms]\)\+\>"
syn match eximSpecialChar "\\[\\nrt]\|\\\o\{1,3}\|\\x\x\{1,2}"
syn region eximMacroDefinition matchgroup=eximMacroName start="^[A-Z]\i*\s*=" end="$" skip="\\\s*$" transparent
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" }}}
" Non-options
syn match eximDriverName "\<\(aliasfile\|appendfile\|autoreply\|domainlist\|forwardfile\|ipliteral\|iplookup\|lmtp\|localuser\|lookuphost\|pipe\|queryprogram\|smartuser\|smtp\)\>"
syn match eximTransport "^\s*\i\+:"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" }}}
" Options {{{
" Sectioning
syn keyword eximEnd end
" Main
syn keyword eximKeyword accept_8bitmime accept_timeout admin_groups allow_mx_to_ip always_bcc auth_always_advertise auth_hosts auth_over_tls_hosts auto_thaw bi_command check_log_inodes check_log_space check_spool_inodes check_spool_space collapse_source_routes daemon_smtp_port daemon_smtp_service debug_level delay_warning delay_warning_condition deliver_load_max deliver_queue_load_max delivery_date_remove dns_again_means_nonexist dns_check_names dns_check_names_pattern dns_retrans dns_ipv4_lookup dns_retry envelope_to_remove errmsg_text errmsg_file errors_address errors_copy errors_reply_to exim_group exim_path exim_user extract_addresses_remove_arguments finduser_retries forbid_domain_literals freeze_tell_mailmaster gecos_name gecos_pattern headers_check_syntax headers_checks_fail headers_sender_verify headers_sender_verify_errmsg helo_accept_junk_hosts helo_strict_syntax helo_verify hold_domains host_accept_relay host_auth_accept_relay host_lookup host_reject host_reject_recipients hosts_treat_as_local ignore_errmsg_errors ignore_errmsg_errors_after ignore_fromline_hosts ignore_fromline_local keep_malformed kill_ip_options ldap_default_servers local_domains local_domains_include_host local_domains_include_host_literals local_from_check local_from_prefix local_from_suffix local_interfaces localhost_number locally_caseless log_all_parents log_arguments log_file_path log_incoming_port log_ip_options log_level log_queue_run_level log_received_recipients log_received_sender log_refused_recipients log_rewrites log_sender_on_delivery log_smtp_confirmation log_smtp_connections log_smtp_syntax_errors log_subject lookup_open_max max_username_length message_body_visible message_filter message_filter_directory_transport message_filter_directory2_transport message_filter_file_transport message_filter_group message_filter_pipe_transport message_filter_reply_transport message_filter_user message_id_header_text message_size_limit message_size_limit_count_recipients move_frozen_messages mysql_servers never_users nobody_group nobody_user percent_hack_domains perl_at_start perl_startup pgsql_servers pid_file_path preserve_message_logs primary_hostname print_topbitchars prod_requires_admin prohibition_message qualify_domain qualify_recipient queue_list_requires_admin queue_only queue_only_file queue_only_load queue_remote_domains queue_run_in_order queue_run_max queue_smtp_domains rbl_domains rbl_hosts rbl_log_headers rbl_log_rcpt_count rbl_reject_recipients rbl_warn_header received_header_text received_headers_max receiver_try_verify receiver_unqualified_hosts receiver_verify receiver_verify_addresses receiver_verify_hosts receiver_verify_senders recipients_max recipients_max_reject recipients_reject_except recipients_reject_except_senders refuse_ip_options relay_domains relay_domains_include_local_mx relay_match_host_or_sender remote_max_parallel remote_sort retry_data_expire retry_interval_max return_path_remove return_size_limit rfc1413_hosts rfc1413_query_timeout security sender_address_relay sender_address_relay_hosts sender_reject sender_reject_recipients sender_try_verify sender_unqualified_hosts sender_verify sender_verify_batch sender_verify_callback_domains sender_verify_callback_timeout sender_verify_fixup sender_verify_hosts sender_verify_hosts_callback sender_verify_max_retry_rate sender_verify_reject smtp_accept_keepalive smtp_accept_max smtp_accept_max_per_host smtp_accept_queue smtp_accept_queue_per_connection smtp_accept_reserve smtp_banner smtp_check_spool_space smtp_connect_backlog smtp_etrn_command smtp_etrn_hosts smtp_etrn_serialize smtp_expn_hosts smtp_load_reserve smtp_receive_timeout smtp_reserve_hosts smtp_verify split_spool_directory spool_directory strip_excess_angle_brackets strip_trailing_dot syslog_timestamp timeout_frozen_after timestamps_utc timezone tls_advertise_hosts tls_certificate tls_dhparam tls_host_accept_relay tls_hosts tls_log_cipher tls_log_peerdn tls_privatekey tls_verify_certificates tls_verify_ciphers tls_verify_hosts trusted_groups trusted_users unknown_login unknown_username untrusted_set_sender uucp_from_pattern uucp_from_sender warnmsg_file
syn keyword eximKeyword no_accept_8bitmime no_allow_mx_to_ip no_always_bcc no_auth_always_advertise no_collapse_source_routes no_delivery_date_remove no_dns_check_names no_envelope_to_remove no_extract_addresses_remove_arguments no_forbid_domain_literals no_freeze_tell_mailmaster no_headers_check_syntax no_headers_checks_fail no_headers_sender_verify no_headers_sender_verify_errmsg no_helo_strict_syntax no_ignore_errmsg_errors no_ignore_fromline_local no_kill_ip_options no_local_domains_include_host no_local_domains_include_host_literals no_local_from_check no_locally_caseless no_log_all_parents no_log_arguments no_log_incoming_port no_log_ip_options no_log_received_recipients no_log_received_sender no_log_refused_recipients no_log_rewrites no_log_sender_on_delivery no_log_smtp_confirmation no_log_smtp_connections no_log_smtp_syntax_errors no_log_subject no_message_size_limit_count_recipients no_move_frozen_messages no_preserve_message_logs no_print_topbitchars no_prod_requires_admin no_queue_list_requires_admin no_queue_only no_rbl_log_headers no_rbl_log_rcpt_count no_rbl_reject_recipients no_receiver_try_verify no_receiver_verify no_recipients_max_reject no_refuse_ip_options no_relay_domains_include_local_mx no_relay_match_host_or_sender no_return_path_remove no_sender_try_verify no_sender_verify no_sender_verify_batch no_sender_verify_fixup no_sender_verify_reject no_smtp_accept_keepalive no_smtp_check_spool_space no_smtp_etrn_serialize no_smtp_verify no_split_spool_directory no_strip_excess_angle_brackets no_strip_trailing_dot no_syslog_timestamp no_timestamps_utc no_tls_log_cipher no_tls_log_peerdn no_untrusted_set_sender
syn keyword eximKeyword not_accept_8bitmime not_allow_mx_to_ip not_always_bcc not_auth_always_advertise not_collapse_source_routes not_delivery_date_remove not_dns_check_names not_envelope_to_remove not_extract_addresses_remove_arguments not_forbid_domain_literals not_freeze_tell_mailmaster not_headers_check_syntax not_headers_checks_fail not_headers_sender_verify not_headers_sender_verify_errmsg not_helo_strict_syntax not_ignore_errmsg_errors not_ignore_fromline_local not_kill_ip_options not_local_domains_include_host not_local_domains_include_host_literals not_local_from_check not_locally_caseless not_log_all_parents not_log_arguments not_log_incoming_port not_log_ip_options not_log_received_recipients not_log_received_sender not_log_refused_recipients not_log_rewrites not_log_sender_on_delivery not_log_smtp_confirmation not_log_smtp_connections not_log_smtp_syntax_errors not_log_subject not_message_size_limit_count_recipients not_move_frozen_messages not_preserve_message_logs not_print_topbitchars not_prod_requires_admin not_queue_list_requires_admin not_queue_only not_rbl_log_headers not_rbl_log_rcpt_count not_rbl_reject_recipients not_receiver_try_verify not_receiver_verify not_recipients_max_reject not_refuse_ip_options not_relay_domains_include_local_mx not_relay_match_host_or_sender not_return_path_remove not_sender_try_verify not_sender_verify not_sender_verify_batch not_sender_verify_fixup not_sender_verify_reject not_smtp_accept_keepalive not_smtp_check_spool_space not_smtp_etrn_serialize not_smtp_verify not_split_spool_directory not_strip_excess_angle_brackets not_strip_trailing_dot not_syslog_timestamp not_timestamps_utc not_tls_log_cipher not_tls_log_peerdn not_untrusted_set_sender
" Transport
syn keyword eximKeyword body_only debug_print delivery_date_add driver envelope_to_add headers_add headers_only headers_remove headers_rewrite message_size_limit return_path return_path_add shadow_condition shadow_transport transport_filter
syn keyword eximKeyword no_body_only no_delivery_date_add no_envelope_to_add no_headers_only no_return_path_add
syn keyword eximKeyword not_body_only not_delivery_date_add not_envelope_to_add not_headers_only not_return_path_add
" Appendfile transport
syn keyword eximKeyword allow_fifo allow_symlink batch batch_max bsmtp bsmtp_helo check_group check_owner check_string create_directory create_file current_directory directory directory_mode escape_string file file_format file_must_exist from_hack group lock_fcntl_timeout lock_interval lock_retries lockfile_mode lockfile_timeout maildir_format maildir_retries maildir_tag mailstore_format mailstore_prefix mailstore_suffix mbx_format mode mode_fail_narrower notify_comsat prefix quota quota_filecount quota_is_inclusive quota_size_regex quota_warn_message quota_warn_threshold require_lockfile retry_use_local_part suffix use_crlf use_fcntl_lock use_lockfile use_mbx_lock user
syn keyword eximKeyword no_allow_fifo no_allow_symlink no_bsmtp_helo no_check_group no_check_owner no_create_directory no_file_must_exist no_from_hack no_maildir_format no_mailstore_format no_mbx_format no_mode_fail_narrower no_notify_comsat no_quota_is_inclusive no_require_lockfile no_retry_use_local_part no_use_crlf no_use_fcntl_lock no_use_lockfile no_use_mbx_lock
syn keyword eximKeyword not_allow_fifo not_allow_symlink not_bsmtp_helo not_check_group not_check_owner not_create_directory not_file_must_exist not_from_hack not_maildir_format not_mailstore_format not_mbx_format not_mode_fail_narrower not_notify_comsat not_quota_is_inclusive not_require_lockfile not_retry_use_local_part not_use_crlf not_use_fcntl_lock not_use_lockfile not_use_mbx_lock
" Autoreply transport
syn keyword eximKeyword bcc cc file file_expand file_optional from group headers initgroups log mode once once_file_size once_repeat reply_to return_message subject text to user
syn keyword eximKeyword no_file_expand no_file_optional no_initgroups no_return_message
syn keyword eximKeyword not_file_expand not_file_optional not_initgroups not_return_message
" Lmtp transport
syn keyword eximKeyword batch batch_max command group initgroups retry_use_local_part timeout user
syn keyword eximKeyword no_initgroups
syn keyword eximKeyword not_initgroups
" Pipe transport
syn keyword eximKeyword allow_commands batch batch_max bsmtp bsmtp_helo check_string command current_directory environment escape_string freeze_exec_fail from_hack group home_directory ignore_status initgroups log_defer_output log_fail_output log_output max_output path pipe_as_creator prefix restrict_to_path retry_use_local_part return_fail_output return_output suffix temp_errors timeout umask use_crlf use_shell user
syn keyword eximKeyword no_bsmtp_helo no_freeze_exec_fail no_from_hack no_ignore_status no_log_defer_output no_log_fail_output no_log_output no_pipe_as_creator no_restrict_to_path no_return_fail_output no_return_output no_use_crlf no_use_shell
syn keyword eximKeyword not_bsmtp_helo not_freeze_exec_fail not_from_hack not_ignore_status not_log_defer_output not_log_fail_output not_log_output not_pipe_as_creator not_restrict_to_path not_return_fail_output not_return_output not_use_crlf not_use_shell
" Smtp transport
syn keyword eximKeyword allow_localhost authenticate_hosts batch_max command_timeout connect_timeout data_timeout delay_after_cutoff dns_qualify_single dns_search_parents fallback_hosts final_timeout gethostbyname helo_data hosts hosts_avoid_tls hosts_require_tls hosts_override hosts_max_try hosts_randomize interface keepalive max_rcpt multi_domain mx_domains port protocol retry_include_ip_address serialize_hosts service size_addition tls_certificate tls_privatekey tls_verify_certificates tls_verify_ciphers
syn keyword eximKeyword no_allow_localhost no_delay_after_cutoff no_dns_qualify_single no_dns_search_parents no_gethostbyname no_hosts_override no_hosts_randomize no_keepalive no_multi_domain no_retry_include_ip_address
syn keyword eximKeyword not_allow_localhost not_delay_after_cutoff not_dns_qualify_single not_dns_search_parents not_gethostbyname not_hosts_override not_hosts_randomize not_keepalive not_multi_domain not_retry_include_ip_address
" Directors and routers
syn keyword eximKeyword condition debug_print domains driver errors_to fail_verify fail_verify_recipient fail_verify_sender fallback_hosts group headers_add headers_remove initgroups local_parts more require_files senders transport unseen user verify verify_only verify_recipient verify_sender
syn keyword eximKeyword no_fail_verify no_fail_verify_recipient no_fail_verify_sender no_initgroups no_more no_unseen no_verify no_verify_only no_verify_recipient no_verify_sender
syn keyword eximKeyword not_fail_verify not_fail_verify_recipient not_fail_verify_sender not_initgroups not_more not_unseen not_verify not_verify_only not_verify_recipient not_verify_sender
" Directors
syn keyword eximKeyword current_directory expn home_directory new_director prefix prefix_optional suffix suffix_optional
syn keyword eximKeyword no_expn no_prefix_optional no_suffix_optional
syn keyword eximKeyword not_expn not_prefix_optional not_suffix_optional
" Aliasfile and forwardfile directors
syn keyword eximKeyword check_ancestor directory_transport directory2_transport file_transport forbid_file forbid_include forbid_pipe freeze_missing_include hide_child_in_errmsg modemask one_time owners owngroups pipe_transport qualify_preserve_domain rewrite skip_syntax_errors syntax_errors_text syntax_errors_to
syn keyword eximKeyword no_check_ancestor no_forbid_file no_forbid_include no_forbid_pipe no_freeze_missing_include no_hide_child_in_errmsg no_one_time no_qualify_preserve_domain no_rewrite no_skip_syntax_errors
syn keyword eximKeyword not_check_ancestor not_forbid_file not_forbid_include not_forbid_pipe not_freeze_missing_include not_hide_child_in_errmsg not_one_time not_qualify_preserve_domain not_rewrite not_skip_syntax_errors
" Aliasfile director
syn keyword eximKeyword expand file forbid_special include_domain optional queries query search_type
syn keyword eximKeyword no_expand no_forbid_special no_include_domain no_optional
syn keyword eximKeyword not_expand not_forbid_special not_include_domain not_optional
" Forwardfile director
syn keyword eximKeyword allow_system_actions check_group check_local_user data file file_directory filter forbid_filter_existstest forbid_filter_logwrite forbid_filter_lookup forbid_filter_perl forbid_filter_reply ignore_eacces ignore_enotdir match_directory reply_transport seteuid
syn keyword eximKeyword no_allow_system_actions no_check_local_user no_forbid_filter_reply no_forbid_filter_existstest no_forbid_filter_logwrite no_forbid_filter_lookup no_forbid_filter_perl no_forbid_filter_reply no_ignore_eacces no_ignore_enotdir no_seteuid
syn keyword eximKeyword not_allow_system_actions not_check_local_user not_forbid_filter_reply not_forbid_filter_existstest not_forbid_filter_logwrite not_forbid_filter_lookup not_forbid_filter_perl not_forbid_filter_reply not_ignore_eacces not_ignore_enotdir not_seteuid
" Localuser director
syn keyword eximKeyword match_directory
" Smartuser director
syn keyword eximKeyword directory_transport directory2_transport file_transport forbid_file forbid_pipe hide_child_in_errmsg new_address panic_expansion_fail pipe_transport qualify_preserve_domain rewrite
syn keyword eximKeyword no_forbid_file no_forbid_pipe no_hide_child_in_errmsg no_panic_expansion_fail no_qualify_preserve_domain no_rewrite
syn keyword eximKeyword not_forbid_file not_forbid_pipe not_hide_child_in_errmsg not_panic_expansion_fail not_qualify_preserve_domain not_rewrite
" Routers
syn keyword eximKeyword ignore_target_hosts pass_on_timeout self translate_ip_address
syn keyword eximKeyword no_pass_on_timeout
syn keyword eximKeyword not_pass_on_timeout
" Domainlist router
syn keyword eximKeyword host_find_failed hosts_randomize modemask owners owngroups qualify_single route_file route_list route_queries route_query search_parents search_type
syn keyword eximKeyword no_hosts_randomize no_qualify_single no_search_parents
syn keyword eximKeyword not_hosts_randomize not_qualify_single not_search_parents
" Iplookup router
syn keyword eximKeyword hosts optional port protocol query reroute response_pattern service timeout
syn keyword eximKeyword no_optional
syn keyword eximKeyword not_optional
" Lookuphost router
syn keyword eximKeyword check_secondary_mx gethostbyname mx_domains qualify_single rewrite_headers search_parents widen_domains
syn keyword eximKeyword no_check_secondary_mx no_gethostbyname no_qualify_single no_search_parents
syn keyword eximKeyword not_check_secondary_mx not_gethostbyname not_qualify_single not_search_parents
" Queryprogram router
syn keyword eximKeyword command command_group command_user current_directory timeout
" Retry FIXME
" Authenticators
syn keyword eximKeyword driver public_name server_set_id server_mail_auth_condition
" Plaintext authenticator
syn keyword eximKeyword server_prompts server_condition client_send
" Cram_md5 authenticator
syn keyword eximKeyword server_secret client_name client_secret
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" }}}
" Define the default highlighting {{{
" For version 5.7 and earlier: Only when not done already
" For version 5.8 and later: Only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_exim_syntax_inits")
  if version < 508
    let did_exim_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink eximComment        Comment
  HiLink eximFixme          Todo
  HiLink eximEnd            Keyword
  HiLink eximNumber         Number
  HiLink eximDriverName     Constant
  HiLink eximConstant       Constant
  HiLink eximTime           Constant
  HiLink eximKeyword        Type
  HiLink eximSpecialChar    Special
  HiLink eximMacroName      Preproc
  HiLink eximTransport      Identifier

  delcommand HiLink
endif
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" }}}
let b:current_syntax = "exim"
