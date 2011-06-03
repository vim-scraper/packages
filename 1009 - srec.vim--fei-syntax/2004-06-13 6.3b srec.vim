" Vim syntax file
" Language:	Motorola S record
" Maintainer:	slimzhao <vim2004@21cn.com>
" Last Change:	2004 May 31
" License:	This file is placed in the public domain.

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore

" storage types

"[Addr [DataCount [RecType]] ]
syn match srecChecksum	/[0-9a-fA-F]\{2}\r\?$/
syn match DataCount  /^S[0-357-9][0-9a-fA-F]\{2}/ contains=RecType contained
syn match RecType  /^S[0-357-9]/ contained
syn match S0Addr  /^S0[0-9a-fA-F]\{6}/ contains=DataCount
syn match S1Addr  /^S1[0-9a-fA-F]\{6}/ contains=DataCount
syn match S2Addr  /^S2[0-9a-fA-F]\{8}/ contains=DataCount
syn match S3Addr  /^S3[0-9a-fA-F]\{10}/ contains=DataCount
"syn match S5Addr  /^S5[0-9a-fA-F]\{6}/
syn match S7Addr  /^S7[0-9a-fA-F]\{10}/ contains=DataCount
syn match S8Addr  /^S8[0-9a-fA-F]\{8}/ contains=DataCount
syn match S9Addr  /^S9[0-9a-fA-F]\{6}/ contains=DataCount

syn case match

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_srec_syntax_inits")
  if version < 508
    let did_srec_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  " The default methods for highlighting.  Can be overridden later
  "Address, Hex link Addr to Comment, while DataCount to Constant, for
  "S-record, the RecType and DataCount field is adjacent, and their color is
  "similar, so we swap it for more eyeable.
  HiLink S1Addr		Constant
  HiLink S2Addr		Constant
  HiLink S3Addr		Constant
  "StartAddress
  HiLink S7Addr		MoreMsg
  HiLink S8Addr		MoreMsg
  HiLink S9Addr		MoreMsg
  "Record type
  HiLink RecType	WarningMsg
  "Checksum
  HiLink srecChecksum	Search
  "Data count
  HiLink DataCount	Comment

  delcommand HiLink
endif

let b:current_syntax = "srec"

" vim: ts=8
