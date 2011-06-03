" Vim syntax file
" Language: Spamassassin configuration file
" Maintainer: Adam Katz <scriptsATkhopiscom>
" Website: http://khopis.com/scripts
" Latest Revision: 2009-04-30
" Version: 1.6
" License: Your choice of Creative Commons Share-alike 2.0 or Apache License 2.0
" Copyright: (c) 2009 by Adam Katz

" Save this file to ~/.vim/syntax/spamassassin.vim
" and add the following to your ~/.vim/filetype.vim:
" 
"     augroup filedetect
"         au BufRead,BufNewFile user_prefs,*.cf,*.pre setfiletype spamassassin
"     augroup END

" This contains EVERYTHING in the Mail::SpamAssassin:Conf man page,
" including all plugins that ship with SpamAssassin and even a few others.
" Only a few eval:foobar() functions are supported (there are too many).

if exists("b:current_syntax")
  finish
endif

" I've concluded it is far easier to get perl regex highlighting by including
" the perl syntax highlighting rather than by farming out the code.
" This results in lots of cancelling at the bottom of this file (incomplete...).
runtime! syntax/perl.vim

" cancel problematic bits inherited from perl's highlighting
" TODO:  redefine numbers to have better edges, LOTS more...
syntax clear perlRepeat perlOperator perlConditional perlStatementFiles 
syntax clear perlStatementProc perlStatementList perlStatementControl 
syntax clear perlStatementInclude perlVarPlain perlVarPlain2 perlVarBlock 
syntax clear perlFunctionName perlShellCommand perlHereDoc perlAutoload 
syntax clear perlUntilEOFSQ perlUntilEOFDQ perlUntilEmptySQ perlUntilEmptyDQ 
syntax clear perlSubstitutionSQ perlStringUnexpanded perlStatementHash
syntax clear perlStatementFlow perlStatementFiledesc

syn match perlComment  "#.*" contains=perlTodo,saURL,@Spell

" clear and re-implement perlMatch
syntax clear perlMatch

" Simple version of searches and matches
" caters for m//, m##, m{} and m[] (and the !/ variant)
syn region perlMatch	matchgroup=perlMatchStartEnd start=+[m!]/+ end=+\v/[cgimosx]*\ze(\s|$)+ contains=@perlInterpSlash oneline
syn region perlMatch	matchgroup=perlMatchStartEnd start=+[m!]#+ end=+\v#[cgimosx]*\ze(\s|$)+ contains=@perlInterpMatch oneline
syn region perlMatch	matchgroup=perlMatchStartEnd start=+[m!]{+ end=+\v}[cgimosx]*\ze(\s|$)+ contains=@perlInterpMatch oneline
syn region perlMatch	matchgroup=perlMatchStartEnd start=+[m!]\[+ end=+\v\][cgimosx]*\ze(\s|$)+ contains=@perlInterpMatch oneline

" A special case for m!!x which allows for comments and extra whitespace
syn region perlMatch	matchgroup=perlMatchStartEnd start=+[m!]!+ end=+\v![cgimosx]*\ze(\s|$)+ contains=@perlInterpSlash,perlComment oneline

" Below some hacks to recognise the // variant.
syn region perlMatch	matchgroup=perlMatchStartEnd start=+[!=]\~\s*/+lc=2 start=+[(~]/+lc=1 start=+\.\./+lc=2 start=+\s/[^= \t0-9$@%]+lc=1,me=e-1,rs=e-1 start=+^/+ skip=+\\/+ end=+\v/[cgimosx]*\ze(\s|$)+ contains=@perlInterpSlash oneline



syn match saRuleLine "\v^(\s*lang\s+\S{2,9}\s+)?\s*\w+" contains=@saRule,saTR
syn match saPreProLine "\v^\(\s*lang\s\+\S\{2,9\}\s\+\)\?\s*\w+$" contains=saPreProc

syn cluster saRule contains=saLists,saHeaderType,saTemplateTags,saNet,saBayes,saMisc,saPrivileged,saType,saDescribe,saReport,saBodyMatch,saAdmin,saAdminBayes,saAdminScores,saPreProc,@saPlugins,saIPaddress,saKeyword

syn keyword saLists blacklist_from contained
syn keyword saLists unblacklist_from blacklist_to whitelist_from contained
syn keyword saLists unwhitelist_from whitelist_from_rcvd contained
syn keyword saLists def_whitelist_from_rcvd whitelist_allows_relays contained
syn keyword saLists unwhitelist_from_rcvd whitelist_to whitelist_auth contained
syn keyword saLists def_whitelist_auth unwhitelist_auth more_spam_to contained
syn keyword saLists all_spam_to whitelist_bounce_relays contained
syn keyword saLists whitelist_subject blacklist_subject contained

syn keyword saHeaderType rewrite_header add_header remove_header contained
syn keyword saHeaderType clear_headers report_safe contained

syn match saHeader "\%(\<rewrite_header\s\+\)\@<=\S\+" contains=saHeaderType nextgroup=saHeaderString
syn match saHeader "\%(\<add_header\s\+\)\@<=\S\+\s\+\S\+\s\+" contains=saHeaderClause nextgroup=saHeaderString
syn match saHeader "\%(\<remove_header\s\+\)\@<=" nextgroup=saHeaderClause
syn keyword saHeaderClause spam ham all contained
syn keyword saHeaderClause Spam Ham All ALL contained
syn keyword saHeaderType subject from to contained
syn keyword saHeaderType Subject From To contained
syn match saHeaderString ".*$" contained contains=saTemplateTags
syn match saTemplateTags "\v_(SCORE|(SP|H)AMMYTOKENS)\([0-9]+\)_" contained
syn match saTemplateTags "\v_(STARS|(SUB)?TESTS(SCORES)?|HEADER)\(..*\)_" contained
syn keyword saTemplateTags _YESNOCAPS_ _YESNO_ _REQD_ _VERSION_ contained
syn keyword saTemplateTags _SUBVERSION_ _SCORE_ _HOSTNAME_ contained
syn keyword saTemplateTags _REMOTEHOSTNAME_ _REMOTEHOSTADDR_ contained
syn keyword saTemplateTags _BAYES_ _TOKENSUMMARY_ _BAYESTC_ contained
syn keyword saTemplateTags _BAYESTCLEARNED_ _BAYESTCSPAMMY_ contained
syn keyword saTemplateTags _BAYESTCHAMMY_ _HAMMYTOKENS_ _SPAMMYTOKENS_ contained
syn keyword saTemplateTags _DATE_ _STARS_ _RELAYSTRUSTED_ contained
syn keyword saTemplateTags _RELAYSUNTRUSTED_ _RELAYSINTERNAL_ contained
syn keyword saTemplateTags _RELAYSEXTERNAL_ _LASTEXTERNALIP_ contained
syn keyword saTemplateTags _LASTEXTERNALRDNS_ _LASTEXTERNALHELO_ contained
syn keyword saTemplateTags _AUTOLEARN_ _AUTOLEARNSCORE_ _TESTS_ contained
syn keyword saTemplateTags _TESTSCORES_ _SUBTESTS_ _DCCB_ _DCCR_ contained
syn keyword saTemplateTags _PYZOR_ _RBL_ _LANGUAGES_ _PREVIEW_ contained
syn keyword saTemplateTags _REPORT_ _SUMMARY_ _CONTACTADDRESS_ contained
syn keyword saTemplateTags _RELAYCOUNTRY_ contained
syn keyword saSQLTags _TABLE_ _USERNAME_ _MAILBOX_ _DOMAIN_

" more added by the TextCat plugin below, see also saTR for the 'lang' setting
syn keyword saLang ok_locales normalize_charset contained
syn match saLocaleLine "\%(^\(\s*lang\s\+\S\{2,9\}\s\+\)\?\s*ok_locales\s\+\)\@<=\S.\+" contains=saLocaleKeys,perlComment
syn keyword saLocaleKeys en ja ko ru th zh contained

syn keyword saNet trusted_networks clear_trusted_networks contained
syn keyword saNet internal_networks clear_internal_networks contained
syn keyword saNet msa_networks clear_msa_networks contained
syn keyword saNet always_trust_envelope_sender skip_rbl_checks contained
syn keyword saNet dns_available dns_test_interval contained

syn keyword saBayes use_bayes use_bayes_rules bayes_auto_learn contained
syn keyword saBayes bayes_auto_learn_threshold_nonspam contained
syn keyword saBayes bayes_auto_learn_threshold_spam contained
syn keyword saBayes bayes_ignore_header bayes_ignore_from contained
syn keyword saBayes bayes_ignore_to bayes_min_ham_num contained
syn keyword saBayes bayes_min_spam_num bayes_learn_during_report contained
syn keyword saBayes bayes_sql_override_username bayes_use_hapaxes contained
syn keyword saBayes bayes_journal_max_size bayes_expiry_max_db_size contained
syn keyword saBayes bayes_auto_expire bayes_learn_to_journal contained

syn keyword saMisc required_score lock_method fold_headers contained
syn keyword saMisc report_safe_copy_headers envelope_sender_header contained
syn keyword saMisc report_charset report clear_report_template contained
syn keyword saMisc report_contact report_hostname unsafe_report contained
syn keyword saMisc clear_unsafe_report_template contained

syn keyword saPrivileged allow_user_rules redirector_pattern contained

syn keyword saType header describe score meta body rawbody full lang contained
syn keyword saType priority test tflags uri mimeheader uri_detail contained

syn keyword saTR lang contained
syn match saTR "\v\s\S{2,9}\s+" contained contains=saLangKeys

syn match saIPaddress "\v\s\zs(([012]?\d?\d)\.){1,3}([012]?\d?\d(\/[0123]\d)?)?\ze(\s|$)"
syn match saURL "\v(f|ht)tps?://[-A-Za-z0-9_.:@/#%,;~?+=&]{4,}" contains=@NoSpell transparent "contained
"syn match saPath "\v(\s|:)/[-A-Za-z0-9_.:@/%,;~+=&]+[^\\]/([msixpgc]+\>)\@!" transparent
" previously also needed this workaround:
"syn match saPath "\v(\s|:)\zs/(etc|usr|tmp|var|dev|bin|home|mnt|opt|root)/[-A-Za-z0-9_.:@/%,;~+=&]+" transparent
syn match saEmail "\v\c[a-z0-9._%+*-]+\@[a-z0-9.*-]+\.[a-z*]{2,4}([^a-z*]|$)\@=" contains=saEmailGlob
syn match saEmailGlob "\*" contained

syn match saReport "\%(^\(\s*lang\s\+\S\{2,9\}\s\+\)\?\s*\(unsafe_\)\?report\s\+\)\@<=\S.\+" contains=perlComment,@Spell

" rule descriptions recommended max length is 50
syn match saDescribe "\%(^\(\s*lang\s\+\S\{2,9\}\s\+\)\?\s*describe\s\+[A-Z_0-9]\+\s\+\)\@<=\S.\{1,50}" contains=perlComment,saURL,@Spell nextgroup=saDescribeOverflow1 keepend
" interrupt saURL color, but don't spellcheck the next part
syn region saDescribeOverflow1 start=+.+ end="[^-A-Za-z0-9_.:@/#%,;~?+=&]" oneline contained contains=@NoSpell nextgroup=saDescribeOverflow2
" spellchecking may resume
syn match saDescribeOverflow2 ".*$" contained contains=@Spell,perlComment

" body rules have regular expressions w/out a leading =~
syn region saBodyMatch matchgroup=perlMatchStartEnd start=:\%(^\(\s*lang\s\+\S\{2,9\}\s\+\)\?\s*\(raw\)\?body\s\+[A-Z_0-9]\+\s\+\)\@<=/: end=:\v/[cgimosx]*(\s|$)|$: contains=@perlInterpSlash

syn match saTestFlags "\%(^\(\s*lang\s\+\S\{2,9\}\s\+\)\?\s*tflags\s\+[A-Z_0-9]\+\s\+\)\@<=\S.\+" contains=saTFlags,perlComment

syn keyword saTFlags net nice learn userconf noautolearn multiple contained
syn keyword saTFlags publish nopublish contained

syn keyword saAdmin version_tag rbl_timeout util_rb_tld util_rb_2tld contained
syn keyword saAdmin loadplugin tryplugin contained

syn keyword saAdminBayes bayes_path bayes_file_mode bayes_store_module contained
syn keyword saAdminBayes bayes_sql_dsn bayes_sql_username contained
syn keyword saAdminBayes bayes_sql_password contained
syn keyword saAdminBayes bayes_sql_username_authorized contained

syn keyword saAdminScores user_scores_dsn user_scores_sql_username contained
syn keyword saAdminScores user_scores_sql_password contained
syn keyword saAdminScores user_scores_sql_custom_query contained
syn keyword saAdminScores user_scores_ldap_username contained
syn keyword saAdminScores user_scores_ldap_password contained

syn keyword saPreProc include ifplugin if else endif require_version contained

syn match saFunction "\v(exists|eval):[^( 	]+(\s|\([^)]*[) 	])" contains=saKeyword,saFunctionArgs
syn keyword saKeyword nfssafe flock win32 version
syn keyword saKeyword all exists eval check_rbl check_rbl_txt contained
syn keyword saKeyword check_rbl_sub plugin check_test_plugin contained
syn keyword saKeyword check_subject_in_whitelist check_subject_in_blacklist contained
syn match saFunctionArgs "[( 	][^) 	]*[) 	]" contains=saParens,perlNumber,saFunctionString contained
syn region saFunctionString start=+'+ end=+'+ skip=+\\'+ contained
syn region saFunctionString start=+"+ end=+"+ skip=+\\"+ contained
syn match saParens "[()]"

"syn match saMeta "^\%(\%(\s*lang\s+\S\{2,9\}\s+\)\?\s*meta\s\+[A-Z_0-9]\+\s\+\)\@<=.*" contains=saMetaOp,saParens
syn match saMeta "\%(\s*meta\s\+[A-Z_0-9]\+\s\+\)\@<=.*" contains=saMetaOp,saParens,perlNumber,perlFloat
syn match saMetaOp "||\|&&\|[-+*/><=]\+" contained

"""""""""""""
" PLUGINS (only those that ship with Spamassassin, small plugins are above)

syn cluster saPlugins contains=saHashChecks,saVerify,saDNSBL,saAWL,saShortCircuit,saLang,saReplace,saReplaceMatch,saPluginMisc,saURIBLtype
syn cluster saPluginKeywords contains=saShortCircuitKeys,saVerifyKeys,saDNSBLKeys,saAVKeys,saLangKeys,saLocaleKeys

" DCC, Pyzor, Razor2, Hashcash
syn keyword saHashChecks use_dcc dcc_body_max dcc_fuz1_max contained
syn keyword saHashChecks dcc_fuz2_max dcc_timeout dcc_home contained
syn keyword saHashChecks dcc_dccifd_path dcc_path dcc_options contained
syn keyword saHashChecks dccifd_options use_pyzor pyzor_max contained
syn keyword saHashChecks pyzor_timeout pyzor_options pyzor_path contained
syn keyword saHashChecks use_razor2 razor_timeout razor_config contained
syn keyword saHashChecks use_hashcash hashcash_accept contained
syn keyword saHashChecks hashcash_doublespend_path contained
syn keyword saHashChecks hashcash_doublespend_file_mode contained

" SPF, DKIM, DomainKeys
syn keyword saVerify whitelist_from_spf def_whitelist_from_spf contained
syn keyword saVerify spf_timeout do_not_use_mail_spf contained
syn keyword saVerify do_not_use_mail_spq_query contained
syn keyword saVerify ignore_received_spf_header contained
syn keyword saVerify use_newest_received_spf_header contained
syn keyword saVerify whitelist_from_dkim def_whitelist_from_dkim contained
syn keyword saVerify dkim_timeout whitelist_from_dk contained
syn keyword saVerify def_whitelist_from_dk domainkeys_timeout contained
syn keyword saVerifyKeys check_dkim_valid check_dkim_valid_author_sig
syn keyword saVerifyKeys check_dkim_verified
syn keyword saTemplateTags _DKIMIDENTIFY_ _DKIMDOMAIN_

" SpamCop and URIDNSBL
syn keyword saDNSBL spamcop_from_address spamcop_to_address contained
syn keyword saDNSBL spamcop_max_report_size uridnsbl_skip_domain contained
syn keyword saDNSBL uridnsbl_max_domains urirhsbl urirhssub contained
syn keyword saDNSBLKeys check_uridnsbl

syn match saURIBLtype "\%(\<urirhss[bu][lb]\s\+\w\+\s\+\S\+\s\+\)\@<=\(A\|TXT\)\>"

syn keyword saAWL use_auto_whitelist auto_whitelist_factor contained
syn keyword saAWL user_awl_override_username auto_whitelist_path contained
syn keyword saAWL auto_whitelist_db_modules auto_whitelist_file_mode contained
syn keyword saAWL user_awl_dsn user_awl_sql_username contained
syn keyword saAWL user_awl_sql_password user_awl_sql_table contained
syn keyword saAWLKeys check_from_in_auto_whitelist
syn keyword saTemplateTags _AWL_ _AWLMEAN_ _AWLCOUNT_ _AWLPRESCORE_

syn keyword saShortCircuit shortcircuit shortcircuit_spam_score contained
syn keyword saShortCircuit shortcircuit_ham_score contained
syn match saShortCircuitLine "\%(^\(\s*lang\s\+\S\{2,9\}\s\+\)\?\s*shortcircuit\s\+[A-Z_0-9]\+\s\+\)\@<=\S.\+" contains=saShortCircuitKeys
syn keyword saShortCircuitKeys ham spam on off contained
syn keyword saTemplateTags _SC_ _SCRULE_ _SCTYPE_

" AntiVirus
syn keyword saAVKeys check_microsoft_executable check_suspect_name

" TextCat (see also saTR and locale stuff in the saLang pieces above)
syn keyword saLang ok_languages inactive_languages contained
syn keyword saLang textcat_max_languages textcat_optimal_ngrams contained
syn keyword saLang textcat_max_ngrams textcat_acceptable_score contained
syn match saLangLine "\%(^\(\s*lang\s\+\S\{2,9\}\s\+\)\?\s*\(ok_languages\|inactive_languages\)\s\+\)\@<=\S.\+" contains=saLangKeys,perlComment
syn keyword saLangKeys af am ar be bg bs ca cs cy da de el en eo es contained
syn keyword saLangKeys et eu fa fi fr fy ga gd he hi hr hu hy id is contained
syn keyword saLangKeys it ja ka ko la lt lv mr ms ne nl no pl pt qu contained
syn keyword saLangKeys rm ro ru sa sco sk sl sq sr sv sw ta th tl tr contained
syn keyword saLangKeys uk vi yi zh zh.big5 zh.gb2312 contained

" ReplaceTags
syn keyword saReplace replace_start replace_end replace_tag contained
syn keyword saReplace replace_rules replace_tag replace_pre contained
syn keyword saReplace replace_inter replace_post contained
syn region saReplaceMatch matchgroup=perlMatchStartEnd start=:\%(^\(\s*lang\s\+\S\{2,9\}\s\+\)\?\s*replace_\(tag\|pre\|post\|inter\)\s\+\S\+\s\+\)\@<=: end=:$: contains=@perlInterpSlash

" URIDetail
syn match saURIDetail "\%(^\(\s*lang\s\+\S\{2,9\}\s\+\)\?\s*uri_detail\s\+[A-Z_0-9]\+\s\+\)\@<=\S.\+" contains=saURIDetailKeys,perlMatch
syn keyword saURIDetailKeys raw type cleaned text domain contained

" ASN
syn keyword saPluginMisc asn_lookup
syn keyword saTemplateTags _ASN_ _ASNCIDR_ _ASNCIDRTAG_ _ASNDATA_ _ASNTAG_
syn keyword saTemplateTags _COMBINEDASN_ _COMBINEDASNCIDR_ _MYASN_ _MYASNCIDR_

" Some 3rd-party plugins (not shipped with SA) ... only quickies here!
syn keyword saPluginMisc uricountry sagrey_header_field contained

" TODO: migrate plugins enabled by default into their own section

"""""""""""""

" double-quoted items can contain Template Tags
syn cluster perlInterpDQ contains=saTemplateTags

if version >= 508 || !exists("did_spamassassin_syntax_inits")
  if version < 508
    let did_spamassassin_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink saLists 		Statement
  HiLink saHeaderType 		Statement
  HiLink saTemplateTags		StorageClass
  HiLink saSQLTags		StorageClass
  HiLink saNet  		Statement
  HiLink saBayes 		Statement
  HiLink saMisc 		Statement
  HiLink saPrivileged 		Statement
  HiLink saType 		Statement
  HiLink saTR	 		Statement
  HiLink saDescribe		String
  HiLink saReport		String
  HiLink saTFlags		StorageClass
  HiLink saAdmin 		Statement
  HiLink saAdminBayes 		Statement
  HiLink saAdminScores 		Statement
  HiLink saPreProc 		StorageClass
  HiLink saIPaddress		Float
  HiLink saBodyMatch		perlMatch
  HiLink saKeyword		StorageClass
  HiLink saHeaderClause		StorageClass
  HiLink saHeaderType		StorageClass
  HiLink saHeaderString		String
  HiLink saFunction		Function
  HiLink saFunctionString	String
  HiLink saParens		StorageClass
  HiLink saMetaOp		Operator

  HiLink saPlugins		Statement
  HiLink saPluginKeywords	saKeyword
  " (why weren't those last two lines enough?)
  HiLink saHashChecks		saPlugins
  HiLink saVerify		saPlugins
  HiLink saDNSBL		saPlugins
  HiLink saURIBLtype		saPluginKeywords
  HiLink saAWL			saPlugins
  HiLink saShortCircuit 	saPlugins
  HiLink saLang 		saPlugins
  HiLink saPluginMisc		saPlugins
  HiLink saReplace		saPlugins
  HiLink saReplaceMatch		saBodyMatch

  HiLink saShortCircuitKeys	saPluginKeywords
  HiLink saURIDetailKeys	saPluginKeywords
  HiLink saVerifyKeys		saPluginKeywords
  HiLink saDNSBLKeys		saPluginKeywords
  HiLink saAVKeys		saPluginKeywords
  HiLink saLangKeys		saPluginKeywords
  HiLink saLocaleKeys		saLangKeys

  "HiLink saURL			StorageClass
  "HiLink saPath 		String
  HiLink saEmail		StorageClass
  HiLink saEmailGlob		Operator

  delcommand HiLink
endif
