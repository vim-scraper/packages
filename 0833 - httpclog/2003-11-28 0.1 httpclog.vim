" Vim syntax file
" Language:	Apache combined log
" Maintainer:	Ben Damm <bdamm@dammfine.com>
" Last Change:	2003 Nov 28

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" The IP Address
syn match httpclogIp /\d*\.\d*\.\d*\.\d*/ nextgroup=dashOne skipwhite
" I'm not too sure what these do
syn match dashOne /-/ contained nextgroup=dashTwo skipwhite
syn match dashTwo /-/ contained nextgroup=httpclogDate skipwhite
" The date [ ... ]
syn match httpclogDate /\[[^]]*\]/ contained nextgroup=httpclogRequestHdr skipwhite
" GET/POST/DELETE/HEAD path HTTP/x.x
syn match httpclogRequestHdr /"[^"]*"/ contained nextgroup=httpclogResultCode skipwhite
" 200
syn match httpclogResultCode /\d\d*/ contained nextgroup=httpclogSize skipwhite
" 2300
syn match httpclogSize /\d\d*/ contained nextgroup=httpclogReferrer skipwhite
" or -
syn match httpclogSize /-/ contained nextgroup=httpclogReferrer skipwhite
" http://www.google... 
syn match httpclogReferrer /"[^"]*"/ contained nextgroup=httpclogUserAgent skipwhite 
" Mozilla
syn match httpclogUserAgent /"[^"]*"/ contained


highlight link httpclogIp Operator
highlight link httpclogOp Operator
highlight link httpclogRequestHdr Comment
highlight link httpclogResultCode Operator
highlight link httpclogSize Operator
highlight link httpclogString String
highlight link httpclogDate String
highlight link httpclogReferrer String
highlight link httpclogUserAgent Comment


