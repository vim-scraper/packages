" Vim syntax file
" Language: Spamassassin configuration file
" Maintainer: Adam Katz <scriptsATkhopiscom>
" Website: http://khopis.com/scripts
" Version: 3.0
" License: Your choice of Creative Commons Share-alike 2.0 or Apache License 2.0
" Copyright: (c) 2009-10 by Adam Katz

" Save this file to ~/.vim/syntax/spamassassin.vim
" and add the following to your ~/.vim/filetype.vim:
" 
"     augroup filedetect
"         au BufRead,BufNewFile user_prefs,*.cf,*.pre setfiletype spamassassin
"     augroup END

" This contains EVERYTHING in the Mail::SpamAssassin:Conf man page,
" including all plugins that ship with SpamAssassin and even a few others.
" Only a few eval:foobar() functions are supported (there are too many).

if version < 600
  echo "Vim 6 or later is needed for this syntax file"
  finish " ... note that 'finish' isn't vim-5 compatible...
endif

if exists("b:current_syntax")
  finish
endif

" Regular expression matching from perl (also pulls @perlInterpSlash)
syn include @perlInterpMatch	syntax/perl.vim

syntax sync clear
if exists('+synmaxcol')
  set synmaxcol=6000 " this is a minor slowdown, needed for super-long metas
endif
if exists('+minlines')
  syntax sync minlines=0
elseif exists('+g:vim_minlines')
  syntax sync g:vim_minlines=0
endif
" SA doesn't have multi-line items (except if/endif, which we don't track)
if exists('+maxlines')
  syntax sync maxlines=2 " this should provide a significant speed boost
elseif exists('+g:vim_maxlines')
  syntax sync g:vim_minlines=2
endif


"""""""""""""
" Generic bits, largely inherited or tweaked from perl

syn match saMatchParent	"\%(\<[m!]\([[:punct:]]\)\|\s\zs\(\/\)\).*\1\2[cgimosx]*\%(\s\|$\)\@=" contains=saMatch,saComment contained

" caters for matching by grouping:  m{} and m[] (and the !/ variant)
syn match saMatchParent	"\<[m!]\%([[{]\).*[]}][cgimosx]*\%(\s\|$\)\@=" contains=saMatch contained
syn region saMatch	matchgroup=saMatchStartEnd start=+[m!]{+ end=+}[cgimosx]*\%(\s\|$\)+ contains=@perlInterpMatch oneline contained
syn region saMatch	matchgroup=saMatchStartEnd start=+[m!]\[+ end=+\][cgimosx]*\%(\s\|$\)+ contains=@perlInterpMatch oneline contained

" A special case for m!!x which allows for comments and extra whitespace
" (Linebreaks and comments in regexps are buggy, probably(?) unsupported in SA)
syn region saMatch	matchgroup=saMatchStartEnd start=+[m!]!+ end=+![cgimosx]*\%(\s\|$\)+ contains=@perlInterpSlash,saComment oneline contained

" match // and !?? and m?? for any ? matching punctuation
syn region saMatch	matchgroup=saMatchStartEnd start=+[m!]\z([[:punct:]]\)+ end=+\ze\z1[cgimosx]*\%(\s\|$\)+ contains=@perlInterpSlash oneline contained

syn match saWrongMatchOp	"[~]=" containedin=ALL

syn region  saQuote	start=+'+ end=+'+ skip=+\\'+ oneline contains=@Spell
syn region  saQuote	start=+"+ end=+"+ skip=+\\"+ oneline contains=@Spell,saTemplateTags

syn keyword saTodo	TODO TBD FIXME XXX BUG contained
syn match   saComment	"#.*$" contains=saTodo,saURL,@Spell

syn match saParens "[()]"
syn match saNumber "\s\@<=-\?\d\{1,90\}\>\%(\.\d\{1,90\}\)\?\>"
syn match saNumber "(-\?\d\{1,90\}\>\%(\.\d\{1,90\}\)\?)" contains=saParens
syn match saNumber "[-+*/.,<=>!~[:space:]]\@<=-\?\d\{1,90\}\%(\.\d\{1,90\}\)\?\%(\s\|[-+*/.,<=>!~]\|$\)\@=" contained
syn match saIPaddress "\s\@<=\%([012]\?\d\?\d\.\)\{1,3\}\%([012]\?\d\?\d\%(\/[0123]\?\d\)\?\)\?\%(\s\|$\)"
syn match saURL "\v\%(f|ht)tps?://[-A-Za-z0-9_.:@/#%,;~?+=&]{4,}" contains=@NoSpell transparent "contained
"syn match saPath "\v[:[:space:]]/[-A-Za-z0-9_.:@/%,;~+=&]+[^\\]/\%([msixpgc]+\>)\@!" transparent
" previously also needed this workaround:
"syn match saPath "\v[:[:space:]]\zs/\%(etc|usr|tmp|var|dev|bin|home|mnt|opt|root)/[-A-Za-z0-9_.:@/%,;~+=&]+" transparent
syn match saEmail "\v\c[a-z0-9._%+*-]+\@[a-z0-9.*-]+\.[a-z*]{2,4}%([^a-z*]|$)\@=" contains=saEmailGlob
syn match saEmailGlob "\*" contained

syn match saString "\S.*$" contains=saComment contained
syn match saError "\S.*$" contains=saComment contained
syn match saErrWord "\S\+" contains=saComment contained


"""""""""""""
" SpamAssassin-specific bits

syn match saRuleStart "^" nextgroup=saTR,saRuleLine skipwhite
syn match saRuleLine "" contained nextgroup=saReport,@saRule,saType,saComment,saDescribe,saPrivileged,@saPlugins skipwhite

syn keyword saTR lang contained nextgroup=saLangKeys,saErrWord skipwhite
  syn keyword saLangKeys af am ar be bg bs ca cs cy da de el en eo es et contained nextgroup=saRuleLine skipwhite
  syn keyword saLangKeys eu fa fi fr fy ga gd he hi hr hu hy id is it ja contained nextgroup=saRuleLine skipwhite
  syn keyword saLangKeys ka ko la lt lv mr ms ne nl no pl pt qu rm ro ru contained nextgroup=saRuleLine skipwhite
  syn keyword saLangKeys sa sco sk sl sq sr sv sw ta th tl tr uk vi yi contained nextgroup=saRuleLine skipwhite
  syn keyword saLangKeys zh uk vi yi zh contained nextgroup=saRuleLine skipwhite
  syn match   saLangKeys "\<zh\.\%(big5\|gb2312\)\>" contained nextgroup=saRuleLine skipwhite

syn cluster saRule contains=saLists,saHeaderType,saTemplateTags,saNet,saBayes,saMisc,saPrivileged,saType,saDescribe,saAdmin,saAdminBayes,saAdminScores,saPreProc,@saPlugins

" a cluster of pretty-much identical regexps matching SA rule names
" (with different match names due to different nextgroups)
syn cluster saRuleNames contains=saHeaderRule,saDescRule,saBodyRule,saUriRule,saTFlagsRule,saMetaRule,saURIBLRule,saShortCircuitRule,saURIDetailRule
  syn match saPredicate "\<__\w\+\>" contained containedin=@saRuleNames
  syn match saTestRule "\<\%(__\)\?T_\w\+\>" contained containedin=@saRuleNames

syn keyword saLists blacklist_from contained
syn keyword saLists unblacklist_from blacklist_to whitelist_from contained
syn keyword saLists unwhitelist_from whitelist_from_rcvd contained
syn keyword saLists def_whitelist_from_rcvd whitelist_allows_relays contained
syn keyword saLists unwhitelist_from_rcvd whitelist_to whitelist_auth contained
syn keyword saLists def_whitelist_auth unwhitelist_auth more_spam_to contained
syn keyword saLists all_spam_to whitelist_bounce_relays contained
syn keyword saLists whitelist_subject blacklist_subject contained

syn keyword saHeaderType clear_headers report_safe contained

syn keyword saHeaderType rewrite_header nextgroup=saHeaderRWName,saErrWord skipwhite
  syn keyword saHeaderRWName subject from to Subject From To contained nextgroup=saHeaderString skipwhite

syn keyword saHeaderType add_header nextgroup=saHeaderClause skipwhite
  syn match   saHeaderClause "\w\{3,4\}" contained contains=saHeaderClauseList,saErrWord nextgroup=saHeaderName skipwhite
  syn keyword saHeaderClauseList spam ham all Spam Ham All ALL contained
    syn match saHeaderName "\S\{1,60\}" contained nextgroup=saHeaderString skipwhite transparent
syn keyword saHeaderType remove_header nextgroup=saHeaderClauseR skipwhite
  syn match   saHeaderClauseR "\w\{3,4\}" contained contains=saHeaderClauseList,saErrWord nextgroup=saHeaderNameR skipwhite
    syn match saHeaderNameR "\S\{1,60\}" contained nextgroup=saError skipwhite transparent

syn match saHeaderString ".\+$" contained contains=saTemplateTags
  syn match   saTemplateTags "_\%(SCORE|\%(SP\|H\)AMMYTOKENS\)\%([0-9]\+\)_" contained
  syn match   saTemplateTags "_\%(STARS\|\%(SUB\)\?TESTS\%(SCORES\)\?|HEADER\)\%(.\+\)_" contained
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
syn keyword saLang normalize_charset contained
syn keyword saLang ok_locales  contained nextgroup=saLocaleWord skipwhite
  syn match saLocaleWord "\w\+\>" contained contains=saLocaleKeys,saErrWord nextgroup=saLocaleWord skipwhite
    syn keyword saLocaleKeys en ja ko ru th zh contained nextgroup=saLocaleKeys,saErrWord skipwhite

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

syn keyword saPrivileged allow_user_rules contained
syn keyword saPrivileged redirector_pattern contained nextgroup=saBodyMatch skipwhite

syn keyword saType lang score describe meta body rawbody full contained
syn keyword saType priority test tflags uri mimeheader contained

syn keyword saReport unsafe_report report contained nextgroup=saString skipwhite
syn keyword saReport report_safe_copy_headers envelope_sender_header contained
syn keyword saReport report_charset clear_report_template contained
syn keyword saReport report_contact report_hostname contained
syn keyword saReport clear_unsafe_report_template contained

syn keyword saEval eval contained nextgroup=saHeaderEvalColon
  syn match saHeaderEvalColon ":" contained nextgroup=saFunction
    syn match saFunction "[^([:space:]]\+" contains=saKeyword nextgroup=saFunctionContent contained
        syn keyword saKeyword nfssafe flock win32 version
        syn keyword saKeyword all check_rbl check_rbl_txt contained
        syn keyword saKeyword check_rbl_sub plugin check_test_plugin contained
        syn keyword saKeyword check_subject_in_whitelist check_subject_in_blacklist contained
      syn region saFunctionContent start=+(+ end=+)+ contains=saParens,saNumber,saFunctionString,saComment contained oneline
        syn region saFunctionString start=+'+ end=+'+ skip=+\\'+ contained oneline
        syn region saFunctionString start=+"+ end=+"+ skip=+\\"+ contained oneline

syn keyword saType mimeheader header contained nextgroup=saHeaderRule skipwhite
  syn match saHeaderRule "\w\+\>" contained nextgroup=saHeaderHeaderPre,saEval,saHeaderHeader skipwhite
    syn keyword saHeaderHeaderPre exists contained nextgroup=saHeaderExistsColon
      syn match saHeaderExistsColon ":" contained nextgroup=saHeaderRuleSpecials
    syn match saHeaderHeader "[^:[:space:]]\+" contained nextgroup=saHeaderHeaderPost,saHeaderMatch contains=saHeaderRuleSpecials,saMatchParent,saHeaderMatch
        syn keyword saHeaderRuleSpecials ALL ToCc EnvelopeFrom MESSAGEID contained
        syn match saHeaderRuleSpecials "\<ALL-\%(\%(UN\)?TRUSTED\|\%(IN\|EX\)TERNAL\)\>" contained
        syn match saHeaderRuleSpecials "\<X-Spam-Relays-\%(\%(Unt\|T\)rusted\|\%(In\|Ex\)ternal\)\>" contained
      syn match saHeaderHeaderPost ":" contained nextgroup=saHeaderHeaderPostWord
      syn keyword saHeaderHeaderPostWord raw addr name contained nextgroup=saHeaderMatch
      syn match saHeaderMatch "\s\+[=!]\~" contained nextgroup=saBodyMatch skipwhite

" this 'should' be contained (but not by saBodyMatch) somehow
syn match saHeaderPost "\[if-unset:" nextgroup=saUnset skipwhite
  syn match saUnset "[^\]]*" contained nextgroup=saUnsetEnd
    syn match saUnsetEnd "\]" contained



" rule descriptions recommended max length is 50
syn keyword saDescribe describe contained nextgroup=saDescRule skipwhite
  syn match saDescRule "\w\+\>" contained nextgroup=saDescription skipwhite
    syn match saDescription "\S.\{0,50\}" contained contains=saComment,saURL,@Spell nextgroup=saDescribeOverflow1
      " interrupt saURL color, but don't spellcheck the next part
      syn region saDescribeOverflow1 start=+.+ end="[^-A-Za-z0-9_.:@/#%,;~?+=&]" oneline contained contains=@NoSpell nextgroup=saDescribeOverflow2
        " spellchecking may resume
        syn match saDescribeOverflow2 ".\+$" contained contains=@Spell,saComment

" body rules have regular expressions w/out a leading =~
syn region saBodyMatch matchgroup=saMatchStartEnd start=:/: end=:/[cgimosx]*\%(\s\|$\): contains=@perlInterpSlash,saMatchParent oneline contained

syn keyword saType rawbody body full contained nextgroup=saBodyRule skipwhite
  syn match saBodyRule "\w\+\>" contained nextgroup=saEval,saBodyMatch skipwhite
" uri can't contain saEval
syn keyword saType uri contained nextgroup=saUriRule skipwhite
  syn match saUriRule "\w\+\>" contained nextgroup=saBodyMatch skipwhite

syn keyword saType tflags contained nextgroup=saTFlagsRule skipwhite
  syn match saTFlagsRule "\w\+\>" contained nextgroup=saTFlags skipwhite
    syn keyword saTFlags net nice learn userconf noautolearn multiple contained nextgroup=saTFlags skipwhite
    syn keyword saTFlags publish nopublish contained nextgroup=saTFlags skipwhite

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
syn match saAtWord "@@\w\+@@" containedin=saComment

syn keyword saType meta contained nextgroup=saMetaRule skipwhite
  syn match saMetaRule "\w\+\>" contained nextgroup=saMeta skipwhite
    syn match saMeta "\S.*" contained contains=saMetaOp,saParens,saPredicate,saTestRule,saComment,saNumber,saRuleNames
      syn match saMetaOp "||\|&&\|[!-+*/><=]\+" contained

"""""""""""""
" PLUGINS (only those that ship with Spamassassin, small plugins are above)

syn cluster saPlugins contains=saHashChecks,saVerify,saDNSBL,saAWL,saShortCircuit,saLang,saReplace,saPluginMisc
syn cluster saPluginKeywords contains=saShortCircuitKeys,saVerifyKeys,saDNSBLKeys,saAVKeys,saLangKeys,saLocaleKeys,saURIBLtype

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
syn keyword saDNSBL uridnsbl_max_domains contained
syn keyword saDNSBLKeys check_uridnsbl

syn keyword saDNSBL uridnsbl uridnsbl uridnssub urirhsbl urirhssub urinsrhsbl urinsrhssub urifullnsrhsbl urifullnsrhssub contained nextgroup=saURIBLRule skipwhite
  syn match saURIBLRule "\w\+\>" contained nextgroup=saURIBLData
    syn match saURIBLData "\s\+\S\+\s\+" contained nextgroup=saURIBLtype
      syn keyword saURIBLtype A TXT contained

syn keyword saAWL use_auto_whitelist auto_whitelist_factor contained
syn keyword saAWL user_awl_override_username auto_whitelist_path contained
syn keyword saAWL auto_whitelist_db_modules auto_whitelist_file_mode contained
syn keyword saAWL user_awl_dsn user_awl_sql_username contained
syn keyword saAWL user_awl_sql_password user_awl_sql_table contained
syn keyword saAWLKeys check_from_in_auto_whitelist
syn keyword saTemplateTags _AWL_ _AWLMEAN_ _AWLCOUNT_ _AWLPRESCORE_

syn keyword saShortCircuit shortcircuit shortcircuit_spam_score shortcircuit_ham_score contained nextgroup=saShortCircuitRule skipwhite
  syn match saShortCircuitRule "\w\+\>" contained nextgroup=saShortCircuitKeys,saErrWord skipwhite
    syn keyword saShortCircuitKeys ham spam on off contained
syn keyword saTemplateTags _SC_ _SCRULE_ _SCTYPE_

" AntiVirus
syn keyword saAVKeys check_microsoft_executable check_suspect_name

" TextCat (see also saTR and locale stuff in the saLang pieces above)
syn keyword saLang ok_languages inactive_languages contained nextgroup=saLangList,saError skipwhite
  syn match saLangList "[a-z]\{2\}\S\{0,8\}\>" contained nextgroup=saLangList,saErrWord skipwhite contains=saLangKeys,saErrWord
syn keyword saLang textcat_max_languages textcat_optimal_ngrams contained
syn keyword saLang textcat_max_ngrams textcat_acceptable_score contained

" ReplaceTags
syn keyword saReplace replace_start replace_end contained
syn keyword saReplace replace_rules replace_tag contained
syn keyword saReplace replace_tag replace_pre replace_inter replace_post contained nextgroup=saReplaceTag skipwhite
  syn match saReplaceTag "\w\+\>" contained nextgroup=saString skipwhite

" URIDetail
syn keyword saType uri_detail contained nextgroup=saURIDetailRule skipwhite
  syn match saURIDetailRule "\w\+\>" contained nextgroup=saURIDetail skipwhite
    syn match saURIDetail "\S.*" contained contains=saHeaderMatch,saMatchParent,saComment
  
syn keyword saURIDetailKeys raw type cleaned text domain contained

" ASN
syn keyword saPluginMisc asn_lookup
syn keyword saTemplateTags _ASN_ _ASNCIDR_ _ASNCIDRTAG_ _ASNDATA_ _ASNTAG_
syn keyword saTemplateTags _COMBINEDASN_ _COMBINEDASNCIDR_ _MYASN_ _MYASNCIDR_

" Some 3rd-party plugins (not shipped with SA) ... only quickies here!
syn keyword saPluginMisc uricountry sagrey_header_field contained
syn keyword saPluginMisc popauth_hash_file contained

" TODO: migrate plugins enabled by default into their own section

"""""""""""""

if version >= 508 || !exists("did_spamassassin_syntax_inits")
  if version < 508
    let did_spamassassin_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink saQuote		String
  HiLink saTodo			Todo
  HiLink saComment		Comment
  HiLink saMatch		String
  HiLink saMatchStartEnd	Statement
  HiLink saError		Error
  HiLink saErrWord		Error
  HiLink saWrongMatchOp 	saError
  HiLink saAtWord		saError
  HiLink saParens		StorageClass
  HiLink saNumber		Float
  HiLink saIPaddress		Float
  "HiLink saURL			Underlined
  "HiLink saPath 		String
  HiLink saEmail		StorageClass
  HiLink saEmailGlob		Operator
  HiLink saString		String

  HiLink saLists 		Statement
  HiLink saHeaderType 		Statement
  HiLink saTemplateTags		StorageClass
  HiLink saSQLTags		saTemplateTags
  HiLink saNet  		Statement
  HiLink saBayes 		Statement
  HiLink saMisc 		Statement
  HiLink saPrivileged 		Statement
  HiLink saType 		Statement
  HiLink saReport 		saType
  HiLink saTR	 		Statement
  HiLink saDescribe		saType
  HiLink saDescription		String
  HiLink saDescribeOverflow1	Error
  HiLink saDescribeOverflow2	saDescribeOverflow1
  HiLink saTFlags		StorageClass
  HiLink saAdmin 		Statement
  HiLink saAdminBayes 		Statement
  HiLink saAdminScores 		Statement
  HiLink saPreProc 		PreProc
  HiLink saBodyMatch		saMatch
  HiLink saHeaderRuleSpecials	StorageClass
  HiLink saHeaderHeaderPre	Identifier
  HiLink saEval 		Identifier
  HiLink saHeaderHeaderPostWord	StorageClass
  HiLink saHeaderPost		StorageClass
  HiLink saUnsetEnd		saHeaderPost
  HiLink saKeyword		StorageClass
  HiLink saHeaderClauseList	StorageClass
  HiLink saHeaderRWName		StorageClass
  HiLink saHeaderString		String
  HiLink saFunction		Function
  HiLink saFunctionString	String
  HiLink saMetaOp		Operator
  HiLink saPredicate		Comment
  HiLink saTestRule		Debug
  HiLink saHeaderMatch		Operator

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

  HiLink saShortCircuitKeys	saPluginKeywords
  HiLink saURIDetailKeys	saPluginKeywords
  HiLink saVerifyKeys		saPluginKeywords
  HiLink saDNSBLKeys		saPluginKeywords
  HiLink saAVKeys		saPluginKeywords
  HiLink saLangKeys		saPluginKeywords
  HiLink saLocaleKeys		saLangKeys

  delcommand HiLink
endif
