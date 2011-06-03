" Vim syntax file
" Language:	Intel Network Processor Micro Code
" Maintainer:	Gui Wei
" Last change:	2004 July 18
"
"

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore


" control keyword
setlocal iskeyword+=. 
syn keyword ucStatement		.break .continue .begin .end .local .endlocal .init
syn keyword ucConditional	.if .elif .if_unsigned .elif_unsigned .else .endif
syn keyword ucRepeat		.while .while_unsigned .endw .repeat .until .until_unsigned .break .continue
syn keyword ucType		.reg .sig .use .use_rd .use_wr .set .set_sig .set_rd .set_wr .addr .import 
syn keyword ucStorageClass	visible global remote automic volatile read write extern .xfer_order .xfer_order_rd .xfer_order_wr
syn keyword ucMisc		sig_done ctx_swap ctx_arb indirect indirect_ref

" MicroCode Variable -- must set before opcodes
syn match	ucLabel "\I\i\+#"
syn match	ucXferReg	display "$\{1,2}\I\i*"

" Partial list of register symbols
syn keyword ucLocalCSR USTORE_ADDRESS
syn keyword ucLocalCSR USTORE_DATA_LOWER
syn keyword ucLocalCSR USTORE_DATA_UPPER
syn keyword ucLocalCSR USTORE_ERROR_STATUS
syn keyword ucLocalCSR ALU_OUT
syn keyword ucLocalCSR CTX_ARB_CNTL
syn keyword ucLocalCSR CTX_ENABLES
syn keyword ucLocalCSR CC_ENABLE
syn keyword ucLocalCSR CSR_CTX_POINTER
syn keyword ucLocalCSR INDIRECT_CTX_STS
syn keyword ucLocalCSR ACTIVE_CTX_STS
syn keyword ucLocalCSR INDIRECT_CTX_SIG_EVENTS
syn keyword ucLocalCSR ACTIVE_CTX_SIG_EVENTS
syn keyword ucLocalCSR INDIRECT_CTX_WAKEUP_EVENTS
syn keyword ucLocalCSR ACTIVE_CTX_WAKEUP_EVENTS
syn keyword ucLocalCSR INDIRECT_CTX_FUTURE_COUNT
syn keyword ucLocalCSR ACTIVE_CTX_FUTURE_COUNT
syn keyword ucLocalCSR INDIRECT_LM_ADDR_0,
syn keyword ucLocalCSR ACTIVE_LM_ADDR_0,
syn keyword ucLocalCSR INDIRECT_LM_ADDR_1
syn keyword ucLocalCSR ACTIVE_LM_ADDR_1
syn keyword ucLocalCSR BYTE_INDEX
syn keyword ucLocalCSR ACTIVE_LM_ADDR_0_BYTE_INDEX
syn keyword ucLocalCSR INDIRECT_LM_ADDR_0_BYTE_INDEX
syn keyword ucLocalCSR INDIRECT_LM_ADDR_1_BYTE_INDEX
syn keyword ucLocalCSR ACTIVE_LM_ADDR_1_BYTE_INDEX
syn keyword ucLocalCSR T_INDEX_BYTE_INDEX
syn keyword ucLocalCSR T_INDEX
syn keyword ucLocalCSR INDIRECT_FUTURE_COUNT_SIGNAL
syn keyword ucLocalCSR ACTIVE_FUTURE_COUNT_SIGNAL
syn keyword ucLocalCSR NN_PUT
syn keyword ucLocalCSR NN_GET
syn keyword ucLocalCSR TIMESTAMP_LOW
syn keyword ucLocalCSR TIMESTAMP_HIGH
syn keyword ucLocalCSR RESERVED
syn keyword ucLocalCSR NEXT_NEIGHBOR_SIGNAL
syn keyword ucLocalCSR PREV_NEIGHBOR_SIGNAL
syn keyword ucLocalCSR SAME_ME_SIGNAL
syn keyword ucLocalCSR CRC_REMAINDER
syn keyword ucLocalCSR PROFILE_COUNT
syn keyword ucLocalCSR PSEUDO_RANDOM_NUMBER
syn keyword ucLocalCSR LOCAL_CSR_STATUS


" MicroCode opcodes
syn match ucOpcode "\<SRAM\>"
syn match ucOpcode "\<DRAM\>"
syn match ucOpcode "\<SRATCH\>"
syn match ucOpcode "\<ALU\>"
syn match ucOpcode "\<ALU_SHF\>"
syn match ucOpcode "\<ASR\>"
syn match ucOpcode "\<BYTE_ALIGN_BE\>"
syn match ucOpcode "\<BYTE_ALIGN_LE\>"
syn match ucOpcode "\<CRC_BE\>"
syn match ucOpcode "\<CRC_LE\>"
syn match ucOpcode "\<DBL_SHF\>"
syn match ucOpcode "\<MUL_STEP\>"
syn match ucOpcode "\<FFS\>"
syn match ucOpcode "\<POP_COUNT\>"
syn match ucOpcode "\<IMMED\>"
syn match ucOpcode "\<IMMED_BO\>"
syn match ucOpcode "\<IMMED_B1\>"
syn match ucOpcode "\<IMMED_B2\>"
syn match ucOpcode "\<IMMED_B3\>"
syn match ucOpcode "\<IMMED_WO\>"
syn match ucOpcode "\<IMMED_W1\>"
syn match ucOpcode "\<LD_FIELD\>"
syn match ucOpcode "\<LD_FIELD_W_CLR\>"
syn match ucOpcode "\<LOAD_ADDR\>"
syn match ucOpcode "\<LOCAL_CSR_RD\>"
syn match ucOpcode "\<LOCAL_CSR_WR\>"
syn match ucOpcode "\<NOP\>"
syn match ucOpcode "\<BCC\>"
syn match ucOpcode "\<BR\>"
syn match ucOpcode "\<BR_BCLR\>"
syn match ucOpcode "\<BR_BSET\>"
syn match ucOpcode "\<BR=BYTE\>"
syn match ucOpcode "\<BR!=BYTE\>"
syn match ucOpcode "\<BR=CTX\>"
syn match ucOpcode "\<BR!=CTX\>"
syn match ucOpcode "\<BR_!INP_STATE\>"
syn match ucOpcode "\<BR_INP_STATE\>"
syn match ucOpcode "\<BR_SIGNAL\>"
syn match ucOpcode "\<BR_!SIGNAL\>"
syn match ucOpcode "\<JUMP\>"
syn match ucOpcode "\<RTN\>"
syn match ucOpcode "\<DRAM\>"
syn match ucOpcode "\<CAP\>"
syn match ucOpcode "\<CTX_ARB\>"
syn match ucOpcode "\<HALT\>"
syn match ucOpcode "\<HASH\>"
syn match ucOpcode "\<MSF\>"
syn match ucOpcode "\<PCI\>"
syn match ucOpcode "\<SCRATCH\>"
syn match ucOpcode "\<SRAM\>"
syn match ucOpcode "\<CAM_CLEAR\>"
syn match ucOpcode "\<CAM_WRITE_STATE\>"
syn match ucOpcode "\<CAM_READ_TAG\>"
syn match ucOpcode "\<CAM_READ_STATE\>"
syn match ucOpcode "\<CAM_LOOKUP\>"
syn match ucOpcode "\<CAM_WRITE\>"

"for preprocessor functions
syn match ucPpFunc      contained "\<IS_IXPTYPE\>"
syn match ucPpFunc      contained "\<isnum\>"
syn match ucPpFunc      contained "\<isimport\>"
syn match ucPpFunc      contained "\<streq\>"
syn match ucPpFunc      contained "\<strstr\>"
syn match ucPpFunc      contained "\<strlen\>"
syn match ucPpFunc      contained "\<strleft\>"
syn match ucPpFunc      contained "\<strright\>"
syn match ucPpFunc      contained "\<defined\>"
syn match ucPpFunc      contained "\<log2\>"
syn match ucPpFunc      contained "\<mask\>"

syn match ucOperator	"[-+*/]"	" Must occur before Comments
syn match ucOperator	"<"
syn match ucOperator	">"
syn match ucOperator	"&"
syn match ucOperator	"!"
syn match ucOperator	"!"		" bit-wise logical or
syn match ucOperator	"="		" must be before other ops containing '='

syn keyword	ucTodo		contained TODO FIXME XXX

" ucCommentGroup allows adding matches for special things in comments
syn cluster	ucCommentGroup	contains=ucTodo

" String
syn region ucString	start=+"+ skip=+\\\\\|\\"+ end=+"+ 
syn region ucString	start=+'+ skip=+\\\\\|''+ end=+'+ 
syn region ucCppString	start=+"+ skip=+\\\\\|\\"\|\\$+ excludenl end=+"+ end='$'

"when wanted, highlight trailing white space
if exists("uc_space_errors")
  if !exists("uc_no_trail_space_error")
    syn match	ucSpaceError	display excludenl "\s\+$"
  endif
  if !exists("uc_no_tab_space_error")
    syn match	ucSpaceError	display " \+\t"me=e-1
  endif
endif

" Number in preprocesser
syn match	ucPpNumber		display contained "\d\|\.\d"
" Number
syn match	ucNumber		display contained "\<\d\+"
" hex number
syn match	ucNumber		display contained "\<0x\x\+"

"catch errors caused by wrong parenthesis and brackets
syn cluster	ucParenGroup	contains=ucParenError,ucIncluded,ucCommentSkip,ucCommentString,ucComment2String,@ucCommentGroup,ucCommentStartError,ucCommentSkip,ucCppOut,ucCppOut2,ucCppSkip
if exists("uc_no_bracket_error")
  syn region	ucParen		transparent start='(' end=')' contains=ALLBUT,@ucParenGroup,ucCppParen,ucCppString
  " ucCppParen: same as cParen but ends at end-of-line; used in cDefine
  syn region	ucCppParen	transparent start='(' skip='\\$' excludenl end=')' end='$' contained contains=ALLBUT,@ucParenGroup,ucParen,ucString
  syn match	ucParenError	display ")"
  syn match	ucErrInParen	display contained "[{}]"
else
  syn region	ucParen		transparent start='(' end=')' contains=ALLBUT,@ucParenGroup,ucCppParen,ucErrInBracket,ucCppBracket,ucCppString
  " ucCppParen: same as ucParen but ends at end-of-line; used in ucDefine
  syn region	ucCppParen	transparent start='(' skip='\\$' excludenl end=')' end='$' contained contains=ALLBUT,@ucParenGroup,ucErrInBracket,ucParen,ucBracket,ucString
  syn match	ucParenError	display "[\])]"
  syn match	ucErrInParen	display contained "[\]{}]"
  syn region	ucBracket	transparent start='\[' end=']' contains=ALLBUT,@ucParenGroup,ucErrInParen,ucCppParen,ucCppBracket,ucCppString
  " ucCppBracket: same as ucParen but ends at end-of-line; used in ucDefine
  syn region	ucCppBracket	transparent start='\[' skip='\\$' excludenl end=']' end='$' contained contains=ALLBUT,@ucParenGroup,ucErrInParen,ucParen,ucBracket,ucString
  syn match	ucErrInBracket	display contained "[);{}]"
endif

if exists("c_comment_strings")
  " A comment can contain ucString, ucCharacter and ucNumber.
  " But a "*/" inside a ucString in a ucComment DOES end the comment!  So we
  " need to use a special type of ucString: ucCommentString, which also ends on
  " "*/", and sees a "*" at the start of the line as comment again.
  " Unfortunately this doesn't very well work for // type of comments :-(
  syntax match	ucCommentSkip	contained "^\s*\*\($\|\s\+\)"
  syntax region	ucCommentString	contained start=+\\\@<!"+ skip=+\\\\\|\\"+ end=+"+ end=+\*/+me=s-1 contains=ucCommentSkip
  syntax region	ucComment2String	contained start=+\\\@<!"+ skip=+\\\\\|\\"+ end=+"+ end="$"
  syntax region	ucCommentL	start="//" skip="\\$" end="$" keepend contains=@ucCommentGroup,ucComment2String,ucPpNumber,ucSpaceError
  syntax region	ucCommentL	start=";" skip="\\$" end="$" keepend contains=@ucCommentGroup,ucComment2String,ucPpNumber,ucSpaceError
  syntax region	ucComment	matchgroup=ucCommentStart start="/\*" end="\*/" contains=@ucCommentGroup,ucCommentStartError,ucCommentString,ucPpNumber,ucSpaceError
else
  syn region	ucCommentL	start="//" skip="\\$" end="$" keepend contains=@ucCommentGroup,ucPpNumber,ucSpaceError
  syntax region	ucCommentL	start=";" skip="\\$" end="$" keepend contains=@ucCommentGroup,ucPpNumber,ucSpaceError
  syn region	ucComment	matchgroup=ucCommentStart start="/\*" end="\*/" contains=@ucCommentGroup,ucCommentStartError,ucSpaceError
endif

" keep a // comment separately, it terminates a preproc. conditional
syntax match	ucCommentError	display "\*/"
syntax match	ucCommentStartError display "/\*"me=e-1 contained

" MicroCode Preprocessor
syn region	ucPreCondit	start="^\s*#\s*\(if\|ifdef\|ifndef\|elif\|macro\)\>" skip="\\$" end="$" end="//"me=s-1 contains=ucPpFunc,ucComment,ucCppString,ucCppParen,ucParenError,ucNumber,ucCommentError,ucSpaceError
syn match	ucPreCondit	display "^\s*#\s*\(else\|endif\|endm\)\>"
" for #if 0
if !exists("uc_no_if0")
  syn region	ucCppOut	start="^\s*#\s*if\s\+0\+\>" end=".\@=\|$" contains=ucCppOut2
  syn region	ucCppOut2	contained start="0" end="^\s*#\s*\(endif\>\|else\>\|elif\>\)" contains=ucSpaceError,ucCppSkip
  syn region	ucCppSkip	contained start="^\s*#\s*\(if\>\|ifdef\>\|ifndef\>\)" skip="\\$" end="^\s*#\s*endif\>" contains=ucSpaceError,ucCppSkip
endif
syn region	ucIncluded	display contained start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match	ucIncluded	display contained "<[^>]*>"
syn match	ucInclude	display "^\s*#\s*include\>\s*["<]" contains=ucIncluded
"syn match ucLineSkip	"\\$"
syn cluster	ucPreProcGroup	contains=ucPreCondit,ucIncluded,ucInclude,ucDefine,ucErrInParen,ucErrInBracket,ucCppOut,ucCppOut2,ucCppSkip,ucCommentSkip,ucCommentString,ucComment2String,@ucCommentGroup,ucCommentStartError,ucParen,ucBracket,ucMulti,ucUserIdentifier
syn region	ucDefine	start="^\s*#\s*\(define\|undef\)\>" skip="\\$" end="$" end="//"me=s-1 contains=ALLBUT,@ucPreProcGroup
syn region	ucPreProc	start="^\s*#\s*\(pragma\>\|line\>\|warning\>\|warn\>\|error\>\)" skip="\\$" end="$" keepend contains=ALLBUT,@ucPreProcGroup


syn case match

exec "syn sync minlines=15"

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_uc_syntax_inits")
  if version < 508
    let did_uc_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  " The default methods for highlighting.  Can be overridden later
  " Comment Constant Error Identifier PreProc Special Statement Todo Type
  "
  " Constant		Boolean Character Number String
  " Identifier		Function
  " PreProc		Define Include Macro PreCondit
  " Special		Debug Delimiter SpecialChar SpecialComment Tag
  " Statement		Conditional Exception Keyword Label Operator Repeat
  " Type		StorageClass Structure Typedef

  HiLink ucCppString		ucString
  HiLink ucIncluded		ucString
  HiLink ucCommentString	ucString
  HiLink ucComment2String	ucString
  HiLink ucString		String

  HiLink ucNumber		Number
  HiLink ucPpNumber		Number

  HiLink ucParenError		ucError
  HiLink ucErrInParen		ucError
  HiLink ucErrInBracket		ucError
  HiLink ucCommentError		ucError
  HiLink ucCommentStartError	ucError
  HiLink ucSpaceError		ucError
  HiLink ucSpecialError		ucError
  HiLink ucError		Error

  HiLink ucCppSkip		ucCppOut
  HiLink ucCppOut2		ucCppOut
  HiLink ucCppOut		ucComment

  HiLink ucCommentL		ucComment
  HiLink ucCommentStart		ucComment
  HiLink ucCommentSkip		ucComment
  HiLink ucComment		Comment

  HiLink ucTodo			Todo

  HiLink ucInclude		Include
  HiLink ucPreProc		PreProc
  HiLink ucDefine		Macro
  HiLink ucPreCondit		PreCondit

  HiLink ucLocalCSR		Label
  HiLink ucLabel		Label
  HiLink ucXferReg		Identifier
  HiLink ucConditional		conditional
  HiLink ucRepeat		Repeat
  HiLink ucOperator		Statement
  HiLink ucOpcode		Statement
  HiLink ucMisc			Statement
  HiLink ucStorageClass		StorageClass
  HiLink ucStatement		Statement
  HiLink ucType			Type

  delcommand HiLink
endif

let b:current_syntax = "uc"

" vim: ts=8 sw=2
