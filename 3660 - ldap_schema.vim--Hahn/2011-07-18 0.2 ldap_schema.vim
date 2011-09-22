" Vim syntax file
" Language    ldap schmema definition language	
" Maintainer  Rostislav Matl <matl@bithill.net>
" Additions   Philipp Hahn <hahn@univention.de>
" Last Change	2011-06-16

" remove old syntax
if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

" be case sensitive
syn case match
setlocal iskeyword+=-

syn keyword schemaKeyword attributetype attributeType
syn keyword schemaKeyword objectclass objectClass
syn keyword schemaKeyword objectidentifier objectIdentifier
syn keyword schemaKeyword olcAttributeTypes
syn keyword schemaKeyword olcObjectClasses
syn keyword schemaKeyword olcObjectIdentifier
syn keyword schemaKeyword ABSTRACT STRUCTURAL AUXILIARY
syn keyword schemaKeyword COLLECTIVE
syn keyword schemaKeyword DESC
syn keyword schemaKeyword EQUALITY
syn keyword schemaKeyword MAY
syn keyword schemaKeyword MUST
syn keyword schemaKeyword NAME
syn keyword schemaKeyword NO-USER-MODIFICATION
syn keyword schemaKeyword OBSOLETE
syn keyword schemaKeyword ORDERING
syn keyword schemaKeyword SINGLE-VALUE
syn keyword schemaKeyword SUBSTR
syn keyword schemaKeyword SUP
syn keyword schemaKeyword SYNTAX
syn keyword schemaKeyword USAGE
syn keyword schemaKeyword X-NDS_CONTAINMENT
syn keyword schemaKeyword X-ORDERED
syn keyword schemaKeyword X-SUBST
syn keyword schemaKeyword userApplications DirectotyOperation distributedOperation dSAOperation

syn keyword schemaMatching bitStringMatch
syn keyword schemaMatching booleanMatch
syn keyword schemaMatching caseExactIA5Match caseExactIA5SubstringsMatch
syn keyword schemaMatching caseExactMatch caseExactSubstringsMatch caseExactOrderingMatch
syn keyword schemaMatching caseIgnoreIA5Match caseIgnoreIA5SubstringsMatch
syn keyword schemaMatching caseIgnoreListMatch caseIgnoreListSubstringsMatch
syn keyword schemaMatching caseIgnoreMatch caseIgnoreSubstringsMatch caseIgnoreOrderingMatch
syn keyword schemaMatching certificateExactMatch
syn keyword schemaMatching directoryStringFirstComponentMatch
syn keyword schemaMatching distinguishedNameMatch
syn keyword schemaMatching generalizedTimeMatch generalizedTimeOrderingMatch
syn keyword schemaMatching integerMatch integerOrderingMatch
syn keyword schemaMatching integerFirstComponentMatch
syn keyword schemaMatching keywordMatch
syn keyword schemaMatching numericStringMatch numericStringSubstringsMatch numericStringOrderingMatch
syn keyword schemaMatching objectIdentifierFirstComponentMatch
syn keyword schemaMatching objectIdentifierMatch
syn keyword schemaMatching octetStringMatch octetStringSubstringsMatch octetStringOrderingMatch
syn keyword schemaMatching presentationAddressMatch
syn keyword schemaMatching protocolInformationMatch
syn keyword schemaMatching telephoneNumberMatch telephoneNumberSubstringsMatch
syn keyword schemaMatching uniqueMemberMatch
syn keyword schemaMatching wordMatch

syntax region String   start="'" end="'"
syntax match  Comment  "#.*$"
syntax match  schemaWoid /[0-9]\+\(\.[0-9]\+\)\+/ " \({[1-9][0-9]*}\)\?
syntax match  schemaWoid /[a-zA-Z0-9_]\+:[0-9]\+\(\.[0-9]\+\)*/ " \({[1-9][0-9]*}\)\?
syntax match  Operator "\$"

hi def link schemaKeyword Statement
hi def link schemaMatching Function
hi def link schemaWoid Identifier

let b:current_syntax="ldap_schema"
