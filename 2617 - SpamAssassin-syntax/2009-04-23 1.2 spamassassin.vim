" Vim syntax file
" Language: Spamassassin configuration file
" Maintainer: Adam Katz <scriptsATkhopiscom>
" Website: http://khopis.com/scripts
" Latest Revision: 2009-04-23
" Version: 1.4
" License: Your choice of Creative Commons Share-alike 2.0 or Apache License 2.0
" Copyright: (c) 2009 by Adam Katz

" Save this file to ~/.vim/syntax/spamassassin.vim
" and add the following to your ~/.vim/filetype.vim:
" 
"     augroup filedetect
"         au BufRead,BufNewFile user_prefs,*.cf,*.pre setfiletype spamassassin
"     augroup END

" This contains EVERYTHING in the Mail::SpamAssassin:Conf man page,
" plus a few plugin configuration options that I encounter regularly.

if exists("b:current_syntax")
  finish
endif

" I've concluded it is far easier to get perl regex highlighting by including
" the perl syntax highlighting rather than by farming out the code.
" This results in lots of cancelling at the bottom of this file (incomplete...).
runtime! syntax/perl.vim

syn match saRuleLine "\v^(\s*lang\s+\S{2,9}\s+)?\s*\w+" contains=@saRule,saTR
syn match saPreProLine "\v^\(\s*lang\s\+\S\{2,9\}\s\+\)\?\s*\w+$" contains=saPreProc

syn cluster saRule contains=saLists,saHeaderType,saTemplateTags,saNet,saBayes,saMisc,saPrivileged,saType,saDescribe,saReport,saBodyMatch,saAdmin,saAdminBayes,saAdminScores,saPreProc,@saPlugins,saHasPaths,saIPaddress,saKeyword

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

" BUG: this next line (and using the saTemplateTags as all contained fails...)
"syn match saHeader "\%(^\(\s*lang\s\+\S\{2,9\}\s\+\)\?\s*\(remove_header|add_header\|rewrite_subject\|subject_tag\)\s\+\)\@<=\S.\+" contains=saTemplateTags,saKeyword
syn match saTemplateTags "\v_(SCORE|(SP|H)AMMYTOKENS)\([0-9]+\)_"
syn match saTemplateTags "\v_(STARS|(SUB)?TESTS(SCORES)?|HEADER)\(..*\)_"
syn keyword saTemplateTags _YESNOCAPS_ _YESNO_ _REQD_ _VERSION_ _SUBVERSION_
syn keyword saTemplateTags _SCORE_ _HOSTNAME_ _REMOTEHOSTNAME_ _REMOTEHOSTADDR_
syn keyword saTemplateTags _BAYES_ _TOKENSUMMARY_ _BAYESTC_ _BAYESTCLEARNED_
syn keyword saTemplateTags _BAYESTCSPAMMY_ _BAYESTCHAMMY_ _HAMMYTOKENS_ 
syn keyword saTemplateTags _SPAMMYTOKENS_ _DATE_ _STARS_ _RELAYSTRUSTED_
syn keyword saTemplateTags _RELAYSUNTRUSTED_ _RELAYSINTERNAL_ _RELAYSEXTERNAL_
syn keyword saTemplateTags _LASTEXTERNALIP_ _LASTEXTERNALRDNS_
syn keyword saTemplateTags _LASTEXTERNALHELO_ _AUTOLEARN_ _AUTOLEARNSCORE_
syn keyword saTemplateTags _TESTS_ _TESTSCORES_ _SUBTESTS_ _DCCB_ _DCCR_ _PYZOR_
syn keyword saTemplateTags _RBL_ _LANGUAGES_ _PREVIEW_ _REPORT_ _SUMMARY_
syn keyword saTemplateTags _CONTACTADDRESS_ _RELAYCOUNTRY_
syn keyword saSQLTags _TABLE_ _USERNAME_ _MAILBOX_ _DOMAIN_

" more added by the TextCat plugin below
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

" TODO: remove this workaround for filename=regex with real solution ... done?
"syn match saHasPaths "\%(^\(\s*lang\s\+\S\{2,9\}\s\+\)\?\s*\(auto_whitelist_path\|bayes_path\|dcc_home\|dcc_path\|pyzor_options\|razor_config\|hashcash_doublespend_path\)\s\+\)\@<=\S.\+"
"syn match saHasIPs "\%(^\(\s*lang\s\+\S\{2,9\}\s\+\)\?\s*\(trusted_networks\|internal_networks\|msa_networks\)\s\+\)\@<=\S.\+$" contains=saIPaddress

syn match saIPaddress "\v<(([012]?\d?\d)\.){1,3}([012]?\d?\d(\/[0123]\d)?)?>"
syn match saURL "\v(f|ht)tps?://[-A-Za-z0-9_.:@/#%,;~?+=&]{4,}" "contained
syn match saFile "\v(\s|:)/(etc|usr|tmp|var|dev|bin|home|mnt|opt|root)/[-A-Za-z0-9_.:@/%,;~+=&]+"
syn match saFile "\v(\s|:)/[-A-Za-z0-9_.:@/%,;~+=&]+[^\\]/[^/ 	]*[^msixpgc]"
syn match saEmail "\v\c[a-z0-9._%+*-]+\@[a-z0-9.*-]+\.[a-z*]{2,4}([^a-z*]|$)\@=" contains=saEmailGlob
syn match saEmailGlob "\*" contained

syn match perlComment  "#.*" contains=perlTodo,saURL,saFile,@Spell
syn match saReport "\%(^\(\s*lang\s\+\S\{2,9\}\s\+\)\?\s*\(unsafe_\)\?report\s\+\)\@<=\S.\+" contains=perlComment,@Spell

" rule descriptions recommended max length is 50, but spellcheck the whole thing
syn match saDescribe "\%(^\(\s*lang\s\+\S\{2,9\}\s\+\)\?\s*describe\s\+[A-Z_0-9]\+\s\+\)\@<=\S.\{1,50}" contains=perlComment,saURL,@Spell nextgroup=saDescribeOverflow
syn match saDescribeOverflow ".*$" contained contains=saURL,@Spell

" body rules have regular expressions w/out a leading =~
syn region saBodyMatch matchgroup=perlMatchStartEnd start=:\%(^\(\s*lang\s\+\S\{2,9\}\s\+\)\?\s*\(raw\)\?body\s\+[A-Z_0-9]\+\s\+\)\@<=/: end=:\v/[cgimosx]*(\s|$)|$: contains=@perlInterpSlash

syn match saTestFlags "\%(^\(\s*lang\s\+\S\{2,9\}\s\+\)\?\s*tflags\s\+[A-Z_0-9]\+\s\+\)\@<=\S.\+" contains=saTFlags,perlComment

syn keyword saTFlags net nice learn userconf noautolearn multiple contained
syn keyword saTFlags nopublish contained

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

" BUG: saFunction doesn't work, so saKeyword is not contained
syn match saFunction "\v(exists|eval):[^\(\s]+(\s|\(.*\))" contains=saKeyword
syn keyword saKeyword all nfssafe flock win32 exists eval
syn keyword saKeyword check_rbl check_rbl_txt check_rbl_sub
syn keyword saKeyword version plugin check_test_plugin
syn keyword saKeyword check_subject_in_whitelist check_subject_in_blacklist

"""""""""""""
" PLUGINS (only those that ship with Spamassassin, small plugins are above)

syn cluster saPlugins contains=saHashChecks,saVerify,saDNSBL,saAWL,saShortCircuit,saLang,saReplace,saReplaceMatch
syn cluster saPluginKeywords contains=saShortCircuitKeys,saVerifyKeys,saDNSBLKeys,saAVKeys,saLangKeys,saLocaleKeys,saAccessDB

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

" SPF, DKIM
syn keyword saVerify whitelist_from_spf def_whitelist_from_spf contained
syn keyword saVerify spf_timeout do_not_use_mail_spf contained
syn keyword saVerify do_not_use_mail_spq_query contained
syn keyword saVerify ignore_received_spf_header contained
syn keyword saVerify use_newest_received_spf_header contained
syn keyword saVerify whitelist_from_dkim def_whitelist_from_dkim contained
syn keyword saVerify dkim_timeout contained
syn keyword saVerifyKeys check_dkim_valid check_dkim_valid_author_sig
syn keyword saVerifyKeys check_dkim_verified
syn keyword saTemplateTags _DKIMIDENTIFY_ _DKIMDOMAIN_

" SpamCop and URIDNSBL
syn keyword saDNSBL spamcop_from_address spamcop_to_address contained
syn keyword saDNSBL spamcop_max_report_size uridnsbl_skip_domain contained
syn keyword saDNSBL uridnsbl_max_domains urirhsbl urirhssub contained
syn keyword saDNSBLKeys check_uridnsbl

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

" TextCat
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

" AccessDB
syn keyword saAccessDB check_access_database

" Missing (disabled in default SA install): ASN, DomainKeys, anything 3rd-party
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

  " cancel problematic bits inherited from perl's highlighting
  " TODO:  single-quotes, numbers in inappropriate places, LOTS more...
  HiLink saNotDesc		NONE
  HiLink perlRepeat		NONE
  HiLink perlOperator		NONE
  HiLink perlConditional	NONE
  HiLink perlStatementFiles	NONE
  HiLink perlStatementProc	NONE
  HiLink perlStatementList	NONE
  HiLink perlStatementControl	NONE
  HiLink perlStatementInclude	NONE
  HiLink perlVarPlain		NONE
  HiLink perlVarPlain2		NONE
  HiLink perlVarBlock		NONE
  HiLink perlFunctionName	NONE
  HiLink perlUntilEOFSQ		NONE
  HiLink perlUntilEmptySQ	NONE

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
  HiLink saHasPaths		String
  HiLink saIPaddress		Float
  HiLink saBodyMatch		perlMatch
  HiLink saFunction		Function
  HiLink saKeyword		StorageClass

  HiLink saPlugins		Statement
  HiLink saPluginKeywords	saKeyword
  " (why weren't those last two lines enough?)
  HiLink saHashChecks		saPlugins
  HiLink saVerify		saPlugins
  HiLink saDNSBL		saPlugins
  HiLink saAWL			saPlugins
  HiLink saShortCircuit 	saPlugins
  HiLink saLang 		saPlugins
  HiLink saReplace		saPlugins
  HiLink saReplaceMatch		saBodyMatch

  HiLink saShortCircuitKeys	saPluginKeywords
  HiLink saURIDetailKeys	saPluginKeywords
  HiLink saVerifyKeys		saPluginKeywords
  HiLink saDNSBLKeys		saPluginKeywords
  HiLink saAVKeys		saPluginKeywords
  HiLink saAccessDB		saPluginKeywords
  HiLink saLangKeys		saPluginKeywords
  HiLink saLocaleKeys		saLangKeys

  HiLink saURL			StorageClass
  HiLink saFile			String
  HiLink saEmail		StorageClass
  HiLink saEmailGlob		Operator

  delcommand HiLink
endif
