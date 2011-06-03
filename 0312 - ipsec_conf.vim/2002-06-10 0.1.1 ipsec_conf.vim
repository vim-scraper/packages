" Vim syntax file
" Language:	ipsec configuration file (ipsec.conf)
" Maintainer:	Bruce Christensen <bruce@orangatango.net>
" Email:	Subject: "ipsec.conf syntax"
" URL:		http://home.attbi.com/~brucec/oss/freeswan/
" Version:	0.1.1
" Last change:	2002 Jun 10

" Changes:
" 0.1.1:
"   - Added support for X.509 keywords
"   - Fixed a bug that caused Vim 6 to eat 100% CPU (thanks to Christian Gall
"     <cg@cgall.de> for reporting the bug)
" 0.1.0:
"   - Original public release

" Bits of this file were stolen from samba.vim
" This file is current for freeswan-1.91
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif
let b:current_syntax = "ipsec.conf"

"-------------------------------------------------------------------------------
" Highlight anything this syntax file doesn't know about as an error
" (because this syntax file obviously knows everything, and anything that it
" doesn't know about *MUST* be an error, right? :)
"-------------------------------------------------------------------------------
syn match   ipsecError /./

"-------------------------------------------------------------------------------
" conn and config sections
"-------------------------------------------------------------------------------

" section header (e.g. 'conn %default' or 'config setup')
" (this is the only top-level element except for comments and errors)
syn match   ipsecSectionHeader /^[[:alpha:]][[:alnum:]._-]\+\>.*$/ contains=ipsecSectionKeyword,ipsecError nextgroup=ipsecSectionLine skipnl
syn keyword ipsecSectionKeyword contained conn config nextgroup=@ipsecSectionName skipwhite
syn cluster ipsecSectionName contains=ipsecSectionNameStr,ipsecMacro
syn match   ipsecSectionNameStr contained /\<[[:alpha:]][[:alnum:]._-]*\>$/

" Lines that make up sections (assignments)
syn match   ipsecSectionLine contained /^..*$/ contains=ipsecSectionWS,ipsecError,ipsecComment nextgroup=ipsecSectionLine skipnl
syn match   ipsecSectionWS contained /^\s\+/ nextgroup=ipsecParam

" Params valid in all sections
syn keyword ipsecParam contained nextgroup=ipsecEq also

" User extension parameters
syn match   ipsecParam contained nextgroup=ipsecEq /\<[Xx][-_][[:alpha:]][[:alnum:]._-]*\>/

" Params valid in conn sections
                                                   " general
syn keyword ipsecParam contained nextgroup=ipsecEq type compress left leftsubnet
syn keyword ipsecParam contained nextgroup=ipsecEq leftnexthop leftupdown leftfirewall
syn keyword ipsecParam contained nextgroup=ipsecEq right rightsubnet rightnexthop
syn keyword ipsecParam contained nextgroup=ipsecEq rightupdown rightfirewall
                                                   " automatic keying
syn keyword ipsecParam contained nextgroup=ipsecEq keyexchange auto auth authby leftid
syn keyword ipsecParam contained nextgroup=ipsecEq leftrsasigkey pfs keylife rekeymargin
syn keyword ipsecParam contained nextgroup=ipsecEq rekeyfuzz keyingtries ikelifetime
syn keyword ipsecParam contained nextgroup=ipsecEq rightid rightrsasigkey disablearrivalcheck
syn keyword ipsecParam contained nextgroup=ipsecEq rekey
                                                   " manual keying
syn keyword ipsecParam contained nextgroup=ipsecEq spi spibase esp espenckey espauthkey
syn keyword ipsecParam contained nextgroup=ipsecEq espreplay_window leftespspi ah ahkey
syn keyword ipsecParam contained nextgroup=ipsecEq ahreplay_window leftahspi
syn keyword ipsecParam contained nextgroup=ipsecEq rightespspi rightahspi
                                                   " X.509 patch
syn keyword ipsecParam contained nextgroup=ipsecEq leftcert rightcert
syn keyword ipsecParam contained nextgroup=ipsecEq leftsubnetwithin rightsubnetwithin
syn keyword ipsecParam contained nextgroup=ipsecEq leftprotoport rightprotoport

" Params valid in config sections
syn keyword ipsecParam contained nextgroup=ipsecEq interfaces forwardcontrol syslog
syn keyword ipsecParam contained nextgroup=ipsecEq klipsdebug plutodebug dumpdir
syn keyword ipsecParam contained nextgroup=ipsecEq manualstart pluto plutoload plutostart
syn keyword ipsecParam contained nextgroup=ipsecEq plutowait prepluto
syn keyword ipsecParam contained nextgroup=ipsecEq postpluto fragicmp packetdefault
syn keyword ipsecParam contained nextgroup=ipsecEq hidetos uniqueids
syn keyword ipsecParam contained nextgroup=ipsecEq overridemtu
                                                   " X.509 patch
syn keyword ipsecParam contained nextgroup=ipsecEq nocrsend

" Equals sign that separates a param name from its value
"(includes leading and trailing whitespace)
syn match   ipsecEq contained /\s*=\s*/ nextgroup=ipsecQuotedValue,ipsecValue,ipsecSpaceError,ipsecError

" Value that is assigned to a parameter (eats up the rest of the line)
syn match   ipsecValue contained /.*$/ contains=ipsecNumber,ipsecMacro,ipsecComment,ipsecValueKeyword,ipsecIpAddress,ipsecPercentage,ipsecUnknownWord,ipsecASDF

" Basic value types
syn match   ipsecUnknownWord contained /\S\+\>/

" Careful: be sure that shorter substrings appear above longer substrings
" (e.g. "default" above "defaultroute")
syn match   ipsecMacro contained /%default\>/
syn match   ipsecMacro contained /%defaultroute\>/
syn match   ipsecMacro contained /%any\>/
syn match   ipsecMacro contained /%opportunistic\>/
syn match   ipsecMacro contained /%direct\>/
syn match   ipsecMacro contained /%dns\>/
syn match   ipsecMacro contained /%search\>/

syn match   ipsecNumber contained /\d\+\(\.\d\+\)\=/
syn match   ipsecNumber contained /0x\(\x\+_\)*\x\+/
syn match   ipsecPercentage contained /\d\+\(\.\d\+\)\=%/
syn match   ipsecIpAddress contained /\([0-9]\{1,3}\.\)\{3}[0-9]\{1,3}\(\/[0-9]\{1,2}\)\=/ contains=ipsecIpPunct
syn match   ipsecIpPunct contained /[.\/]/

" These keywords are valid on the right side of an assignment
syn match   ipsecValueKeyword contained /3des-md5/
syn match   ipsecValueKeyword contained /3des-md5-96/
syn match   ipsecValueKeyword contained /3des-sha1/
syn match   ipsecValueKeyword contained /3des-sha1-96/
syn match   ipsecValueKeyword contained /hmac-md5/
syn match   ipsecValueKeyword contained /hmac-md5-96/
syn match   ipsecValueKeyword contained /hmac-sha1/
syn match   ipsecValueKeyword contained /hmac-sha1-96/
syn keyword ipsecValueKeyword contained tunnel transport passthrough yes no ike
syn keyword ipsecValueKeyword contained add route start ignore esp ah secret
syn keyword ipsecValueKeyword contained rsasig none all pass drop reject

" Flag values with spaces that aren't in quotes as errors
" (this must be below all of the basic value types)
syn match   ipsecSpaceError contained /\S\+\s\+.*/

" Allow spaces in quoted values
" (this must be below ipsecSpaceError)
syn region  ipsecQuotedValue contained matchgroup=SpecialChar start=/"/ end=/"/ contains=ipsecValue keepend

"-------------------------------------------------------------------------------
" Includes (including filenames and wildcards)
"-------------------------------------------------------------------------------
syn match   ipsecInclude /^include\>.*/ contains=ipsecIncludeKeyword,ipsecError
syn keyword ipsecIncludeKeyword contained include nextgroup=ipsecFileName skipwhite
syn match   ipsecFileName contained /[^"[:space:]]\S*/ contains=ipsecWildcard
syn match   ipsecWildcard contained /[][{}*?,]/

"-------------------------------------------------------------------------------
" Comments
"-------------------------------------------------------------------------------
syn match   ipsecComment /\s*#.*$/ contains=ipsecTodo
syn keyword ipsecTodo contained XXX TODO FIXME

"-------------------------------------------------------------------------------
" Help Vim out a little with re-coloring (parse back 30 lines)
"-------------------------------------------------------------------------------
syn sync minlines=30

"-------------------------------------------------------------------------------
"-------------------------------------------------------------------------------

"-------------------------------------------------------------------------------
" Highlighting
"-------------------------------------------------------------------------------

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_ipsec_conf__syn_inits")
  if version < 508
    let did_ipsec_conf__syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink ipsecIncludeKeyword Include

  HiLink ipsecWildcard       SpecialChar
  HiLink ipsecIpPunct        SpecialChar

  HiLink ipsecSectionWS      Normal
  HiLink ipsecUnknownWord    Normal

  HiLink ipsecValueKeyword   ipsecKeyword
  HiLink ipsecSectionKeyword ipsecKeyword
  HiLink ipsecKeyword        Keyword

  HiLink ipsecParam          Identifier
  HiLink ipsecEq             Operator

  HiLink ipsecSpaceError     ipsecError
  HiLink ipsecError          Error

  HiLink ipsecFileName       ipsecString
  HiLink ipsecSectionNameStr ipsecString
  HiLink ipsecIpAddress      ipsecString
  HiLink ipsecString         String
  HiLink ipsecNumber         Number
  HiLink ipsecComment        Comment
  HiLink ipsecTodo           Todo

  HiLink ipsecMacro          Macro

  delcommand HiLink
endif

