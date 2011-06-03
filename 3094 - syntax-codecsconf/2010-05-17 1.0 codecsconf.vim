" Author: udslk
" Date:   Mon May 17 18:17:09 EEST 2010
" Desc:   syntax file for mplayer codecs.conf file
"

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn region cconfReleaseInfo  start='^release' end='$'
syn region cconfComment  start=';' end='$'
syn region cconfString  start='"' end='"'
syn keyword cconfIdentifier videocodec audiocodec
syn keyword cconfConstant status driver dll out format
syn keyword cconfSpecial fourcc
syn keyword cconfTitle info
syn keyword cconfTodo comment


if version >= 508 || !exists("did_map_cmd_syn_inits")
  hi def link cconfReleaseInfo Error
  hi def link cconfComment Comment
  hi def link cconfString String
  hi def link cconfIdentifier Identifier
  hi def link cconfConstant Constant
  hi def link cconfTitle Title
  hi def link cconfSpecial Special
  hi def link cconfTodo Todo
endif

let b:current_syntax = "codecsconf"
