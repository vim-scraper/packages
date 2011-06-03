" Syntax file for ngrep capture of sip traffic (-qtt -W byline)
" Language:	sip packets
" Last Change:	14 July 2008
" Maintainer:	Stanisław Pitucha <viraptor@gmail.com>
" License:	This file is placed in the public domain.

if exists("b:current_syntax")
	finish
endif

set foldmethod=syntax
set foldlevel=0

set foldtext=SipNgrepFolding()
function! SipNgrepFolding()
	let ips = getline(v:foldstart)
	let method = getline(v:foldstart+1)
	return ips . ' *** ' . method
endfunction

syn case ignore

syn cluster sipngrepErrors contains=sipngrepNoUser,sipngrepNoHost,sipngrepInternalIp

syn region	sipngrepPacketRegion	matchgroup=sipngrepPacketIPs start='^[TU]\( .*\)\? \(\d\+\.\d\+\.\d\+\.\d\+\|\S\+ \[\d\+\.\d\+\.\d\+\.\d\+\]\):\d\+ -> \(\d\+\.\d\+\.\d\+\.\d\+\|\S\+ \[\d\+\.\d\+\.\d\+\.\d\+\]\):\d\+' end='^$' transparent fold contains=sipngrepSipAddress,sipngrepStatusError,sipngrepHeaderName,sipngrepImportantHeaderName,sipngrepFromToDescr,sipngrepPacketBody,sipngrepRequest,sipngrepStatus,@sipngrepErrors

syn match	sipngrepRequest '^[A-Z]\+ .\+ SIP/2\.0\.$' contained contains=sipngrepMethod,sipngrepMethodError
" highlight unknown methods
syn match	sipngrepMethodError '^[A-Z]\+[ ]\@=' contained
syn match	sipngrepMethod '^\(INVITE\|REFER\|ACK\|PRACK\|OPTIONS\|NOTIFY\|SUBSCRIBE\|PUBLISH\|BYE\|REGISTER\|CANCEL\|INFO\|UPDATE\)[ ]\@=' contained
syn match	sipngrepStatus '^SIP/2\.0 \d\{3} .*$' contained contains=sipngrepStatusError
" highlight bad status
syn match	sipngrepStatusError '^SIP/2\.0\s\+[456]\d\d\s.*$' contained

syn region	sipngrepPacketBody start='^\.$' end='^$' contained

syn match	sipngrepSipAddress	'sip:[0-9a-z_]\+@' contained contains=sipngrepNumber
syn match	sipngrepNumber	'[0-9a-z_]\+' contained

syn match	sipngrepHeaderName '^[-a-zA-Z_]\+\s*:' contained
syn match	sipngrepImportantHeaderName '^\(From\|To\|Via\|Contact\|Call-id\|Record-route\|Route\)\s*:' contained

syn match	sipngrepFromToDescr '\(^\(From\|To\)\s*:\s*\)\@<="[^"]*"' contained

" general errors (potential)
syn match	sipngrepNoUser 'sip:@[a-zA-Z0-9\.]*' contained
syn match	sipngrepNoHost 'sip:\S\+@[a-zA-Z0-9]\@!' contained
syn match	sipngrepInternalIp '\<\(10\|127\)\(\.[0-9]\{1,3\}\)\{3}\>' contained
syn match	sipngrepInternalIp '\<192\.168\(\.[0-9]\{1,3\}\)\{2}\>' contained
syn match	sipngrepInternalIp '\<172\.\(1[6-9]\|2[0-9]\|3[01]\)\(\.[0-9]\{1,3\}\)\{2}\>' contained

hi def link sipngrepPacketIPs Comment
hi def link sipngrepNumber Number
hi def link sipngrepHeaderName Constant
hi def link sipngrepImportantHeaderName Function
hi def link sipngrepFromToDescr String
hi def link sipngrepMethod Special

hi def link sipngrepInternalIp Error
hi def link sipngrepStatusError Error
hi def link sipngrepMethodError Error
hi def link sipngrepNoUser Error
hi def link sipngrepNoHost Error

let b:current_syntax = "sipngrep"

