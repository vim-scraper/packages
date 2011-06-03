" Vim syntax file
" Language:	Teraterm MACRO language
" Maintainer:	Russell Brinkmann (www.russandbecky.org)
" Version: 1.0

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

"syn match cDefine "^#ifdef[ ]\+[A-Za-z_]\+"
"syn match cDefine "^#endif"

" case is not significant
syn case ignore

" Communication commands
syn keyword ttlStatement bplusrecv bplussend callmenu changedir clearscreen
syn keyword ttlStatement closett connect cygconnect disconnect enablekeyb
syn keyword ttlStatement flushrecv gethostname gettitle
syn keyword ttlStatement kmtfinish kmtget kmtrecv kmtsend
syn keyword ttlStatement loadkeymap logclose logopen logpause logstart logwrite
syn keyword ttlStatement quickvanrecv quickvansend recvln restoresetup
syn keyword ttlStatement scprecv scpsend send sendbreak sendbroadcast
syn keyword ttlStatement sendfile sendkcode sendln sendlnbroadcast
syn keyword ttlStatement sendmulticast setbaud setdebug setdtr setecho
syn keyword ttlStatement setmulticastname setrts setsync settitle showtt
syn keyword ttlStatement testlink unlink 
syn keyword ttlStatement wait wait4all waitevent waitln waitn waitrecv waitregex
syn keyword ttlStatement xmodemrecv xmodemsend ymodemrecv ymodemsend
syn keyword ttlStatement zmodemrecv zmodemsend 

" Control commands
syn keyword ttlStatement break call do loop end execcmnd exit for next goto
syn keyword ttlStatement if then elseif else endif include mpause pause return
syn keyword ttlStatement until enduntil while endwhile 

" String operation commands
syn keyword ttlStatement code2str int2str sprintf sprintf2 str2code str2int
syn keyword ttlStatement strcompare strconcat strcopy strlen strmatch strscan
syn keyword ttlStatement tolower toupper 

" File operation commands
syn keyword ttlStatement fileclose fileconcat filecopy filecreate filedelete
syn keyword ttlStatement filemarkptr fileopen filereadln fileread filerename
syn keyword ttlStatement filesearch fileseek fileseekback filestat 
syn keyword ttlStatement filestrseek filestrseek2 filewrite filewriteln
syn keyword ttlStatement findfirst findnext findclose getdir makepath setdir 

" Password commands
syn keyword ttlStatement delpassword getpassword passwordbox 

" Miscellaneous commands
syn keyword ttlStatement beep closesbox clipb2var crc32 crc32file exec
syn keyword ttlStatement filenamebox getdate getenv gettime getttdir getver 
syn keyword ttlStatement ifdefined inputbox messagebox random 
syn keyword ttlStatement rotateleft rotateright setdate setdlgpos setenv 
syn keyword ttlStatement setexitcode settime show statusbox
syn keyword ttlStatement var2clipb yesnobox 


syn keyword ttlTodo contained TODO TBD
syn region  ttlComment start="/\*" end="\*/" contains=ttlTodo
syn match   ttlComment "//.*" oneline contains=ttlTodo

" TBD ttlLabel
"syn match ttlLabel

syn region ttlString start=+"+  end=+"+
syn region ttlString start=+'+  end=+'+
syn match  ttlString "#[0-9]\+"
syn match  ttlString "#\$[0-9a-f]\+"


" floating numbers
syn match ttlNumber "-\=\<\d\+\.\d\+\(E[+\-]\=\d\+\)\>"
syn match ttlNumber "-\=\<\d\+\.\d\+\>"
syn match ttlNumber "0*2#[01_]\+\.[01_]\+#\(E[+\-]\=\d\+\)\="
syn match ttlNumber "0*16#[0-9a-f_]\+\.[0-9a-f_]\+#\(E[+\-]\=\d\+\)\="
" integer numbers
syn match ttlNumber "-\=\<\d\+\(E[+\-]\=\d\+\)\>"
syn match ttlNumber "-\=\<\d\+\>"
syn match ttlNumber "0*2#[01_]\+#\(E[+\-]\=\d\+\)\="
syn match ttlNumber "0*16#[0-9a-f_]\+#\(E[+\-]\=\d\+\)\="
" Hex numbers
syn match ttlNumber "\$[0-9a-f_]\+"

" operators
syn keyword ttlOperator and nand or nor xor xnor
syn keyword ttlOperator ~ nand or nor xor xnor
syn match   ttlOperator "[&|~><!)(*#%@+/=?:;}{,.\^\-\[\]]"
"syn match   ttlOperator "[&><=:+\-*\/|]"

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_ttl_syntax_inits")
  if version < 508
    let did_ttl_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

"  HiLink cDefine       PreProc
"  HiLink ttlSpecial   Special
  HiLink ttlStatement Statement
  HiLink ttlString    String
  HiLink ttlComment   Comment
  HiLink ttlNumber    String
  HiLink ttlOperator  Type
"  HiLink ttlGlobal    Error
  HiLink ttlTodo      Todo

  delcommand HiLink
endif

let b:current_syntax = "ttl"

set ignorecase
"set noautoindent
set smartindent
set nocindent

"let b:match_words = '\<then\>:\<else\>:\<elsif\>:\<end if\>'
" vim: ts=8
