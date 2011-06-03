" Vim syntax file
" Language:    cvsps
" Maintainer:  Leandro Penz <lpenz@terra.com.br>

if exists("b:current_syntax")
  finish
endif

syntax sync minlines=80

syn match cvspsHeadline /^\(Date\|Author\|Branch\|Tag\|Log\|Members\|Ancestor branch\):\s*.*$/ contained contains=cvspsKey,cvspsVal
syn match cvspsKey /^[^:]\+/ contained
syn match cvspsColon /:/ contained
syn match cvspsVal /:.*$/ contained contains=cvspsColon
syn match cvspsHeaderLine /^PatchSet\s\+[0-9]\+\s*$/ contained
syn match cvspsSeparator /^---------------------$/
syn match cvspsMyFile !! contained

syn cluster cvspsSpecial contains=cvspsHeadline,cvspsHeaderLine,cvspsSeparator

syn region cvspsSection start=/^PatchSet\s\+[0-9]\+\s*$/ end=/^---------------------$/ contains=@cvspsSpecial,cvspsLogMessage,cvspsFiles
syn region cvspsLogMessage start=/^Log:$/ end=/^Members:/me=s-1 contains=cvspsHeadline
syn match cvspsFiles /^	.*$/ contains=cvspsMyFile

hi def link cvspsHeaderLine Underlined
hi def link cvspsSeparator WarningMsg
hi def link cvspsKey Type
hi def link cvspsColon cvspsKey
hi def link cvspsVal String
hi def link cvspsLogMessage Normal
hi def link cvspsFiles Comment
hi def link cvspsMyFile Todo

let b:current_syntax = "cvsps"

