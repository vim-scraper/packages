"-----------------------------------------------------------------------------"
" Vim 7.0+ (maybe less too ?? )                                               "
" Language:     GNU Assembler x86_64 + C preproc (for linux kernel)           "
" Maintainer:   Florian Delizy <florian.delizy@unfreeze.net>                  "
" Last Change:  2006/09/11 (Initial writing)                                  "
"-----------------------------------------------------------------------------"


" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loade

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif


" GNU AS macro directives for x86_64 (from as 2.17. manual )

" Because of leading '.' syn can't be made with keywords ... 

syn match gasMacro '\.abort'
syn match gasMacro '\.ABORT'
syn match gasMacro '\.altmacro'
syn match gasMacro '\.ascii'
syn match gasMacro '\.asciz'
syn match gasMacro '\.arch'
syn match gasMacro '\.code32'
syn match gasMacro '\.code64'
syn match gasMacro '\.balign'
syn match gasMacro '\.byte'
syn match gasMacro '\.comm'
syn match gasMacro '\.cfi_startproc'
syn match gasMacro '\.cfi_endproc'
syn match gasMacro '\.cfi_def_cfa'
syn match gasMacro '\.cfi_def_cfa_register'
syn match gasMacro '\.cfi_def_cfa_offset'
syn match gasMacro '\.cfi_adjust_cfa_offset'
syn match gasMacro '\.cfi_offset'
syn match gasMacro '\.cfi_rel_offset'
syn match gasMacro '\.cfi_signal_frame'
syn match gasMacro '\.cfi_window_save'
syn match gasMacro '\.cfi_escape'
syn match gasMacro '\.data'
syn match gasMacro '\.def'
syn match gasMacro '\.desc'
syn match gasMacro '\.dim'
syn match gasMacro '\.double'
syn match gasMacro '\.eject'
syn match gasMacro '\.else'
syn match gasMacro '\.elseif'
syn match gasMacro '\.end'
syn match gasMacro '\.endef'
syn match gasMacro '\.endfunc'
syn match gasMacro '\.endif'
syn match gasMacro '\.equ'
syn match gasMacro '\.equiv'
syn match gasMacro '\.eqv'
syn match gasMacro '\.err'
syn match gasMacro '\.error'
syn match gasMacro '\.exitm'
syn match gasMacro '\.extern'
syn match gasMacro '\.fail'
syn match gasMacro '\.file'
syn match gasMacro '\.fill'
syn match gasMacro '\.float'
syn match gasMacro '\.func'
syn match gasMacro '\.global'
syn match gasMacro '\.globl'
syn match gasMacro '\.hidden'
syn match gasMacro '\.hword'
syn match gasMacro '\.ident'
syn match gasMacro '\.if'
syn match gasMacro '\.incbin'
syn match gasMacro '\.include'
syn match gasMacro '\.int'
syn match gasMacro '\.internal'
syn match gasMacro '\.irp'
syn match gasMacro '\.irpc'
syn match gasMacro '\.lcomm'
syn match gasMacro '\.lflags'
syn match gasMacro '\.line'
syn match gasMacro '\.linkonce'
syn match gasMacro '\.list'
syn match gasMacro '\.ln'
syn match gasMacro '\.file'
syn match gasMacro '\.debug_line'
syn match gasMacro '\.loc'
syn match gasMacro '\.basic_block'
syn match gasMacro '\.prologue_end'
syn match gasMacro '\.epilogue_begin'
syn match gasMacro '\.is_stmt'
syn match gasMacro '\.isa'
syn match gasMacro '\.loc_mark_blocks'
syn match gasMacro '\.long'
syn match gasMacro '\.macro'
syn match gasMacro '\.mri'
syn match gasMacro '\.endm'
syn match gasMacro '\.noaltmacro'
syn match gasMacro '\.nolist'
syn match gasMacro '\.octa'
syn match gasMacro '\.org'
syn match gasMacro '\.p2align'
syn match gasMacro '\.popsection'
syn match gasMacro '\.previous'
syn match gasMacro '\.print'
syn match gasMacro '\.protected'
syn match gasMacro '\.psize'
syn match gasMacro '\.purgem'
syn match gasMacro '\.pushsection'
syn match gasMacro '\.quad'
syn match gasMacro '\.rept'
syn match gasMacro '\.sbttl'
syn match gasMacro '\.scl'
syn match gasMacro '\.section'
syn match gasMacro '\.set'
syn match gasMacro '\.short'
syn match gasMacro '\.single'
syn match gasMacro '\.size'
syn match gasMacro '\.skip'
syn match gasMacro '\.sleb128'
syn match gasMacro '\.space'
syn match gasMacro '\.stabd'
syn match gasMacro '\.stabn'
syn match gasMacro '\.stabs'
syn match gasMacro '\.string'
syn match gasMacro '\.struct'
syn match gasMacro '\.subsection'
syn match gasMacro '\.symver'
syn match gasMacro '\.tag'
syn match gasMacro '\.text'
syn match gasMacro '\.title'
syn match gasMacro '\.type'
syn match gasMacro '\.uleb128'
syn match gasMacro '\.val'
syn match gasMacro '\.version'
syn match gasMacro '\.vtable_entry'
syn match gasMacro '\.vtable_inherit'
syn match gasMacro '\.warning'
syn match gasMacro '\.weak'
syn match gasMacro '\.weakref'
syn match gasMacro '\.word'


" Mnemonics :

syn keyword   asmKeyword        aaa aad aam aas adc add and bound bsf bsr bswap
syn keyword   asmKeyword        bt btc btr bts call cbw cwde cdqe cw cdq cqo
syn keyword   asmKeyword        clc cld clflush cmc


syn keyword   asmKeyword        cmp cmps cmpsb cmpsw cmpsd cmpsq cmpxchg cmpxchg8b cmpxchg16b
syn keyword   asmKeyword        cpuid daa das dec div enter idiv imul in inc ins insb insw
syn keyword   asmKeyword        insd int into 


syn keyword   asmKeyword        jcxz jecxz jrcxz jmp lahf lds les lfs lgs lss lea 
syn keyword   asmKeyword        leave lfence lods lodsb lodsw lodsd lodsq loop 
syn keyword   asmKeyword        loope loopne loopnz loopz mfence mov movd movmskpd
syn keyword   asmKeyword        movmskps movnti movs movsb movsw movsd movsq movsx
syn keyword   asmKeyword        movsxd movzx mul neg nop not or out outs outsb
syn keyword   asmKeyword        outsw outsd pause pop popa popad popf popfd
syn keyword   asmKeyword        popfq prefetch prefetchw
syn match     asmKeyword        "prefetchlevel[0-9]"
syn keyword   asmKeyword        push pusha pushad pushf pishfd pushfq rcl rcr ret rol
syn keyword   asmKeyword        ror sahf sal shl sar sbb scas scasb scasw scasd scasq 
syn keyword   asmKeyword        sfence shl shld shr shrd stc std stos stosd stosw stosq sub
syn keyword   asmKeyword        test xadd xchg xlat xlatb xor arpl clgi cli clts hlt
syn match     asmKeyword        "int 3"
syn keyword   asmKeyword        invd invlpg invlpga iret iretd iretq lar lgtd lidt lldt lmsw
syn keyword   asmKeyword        lsl ltr 

syn keyword   asmKeyword        rdmsr
syn keyword   asmKeyword        rdpmc
syn keyword   asmKeyword        rdtsc
syn keyword   asmKeyword        drtscp
syn keyword   asmKeyword        rsm
syn keyword   asmKeyword        sgdt
syn keyword   asmKeyword        sidt
syn keyword   asmKeyword        skinit
syn keyword   asmKeyword        sldt
syn keyword   asmKeyword        smsw
syn keyword   asmKeyword        sti
syn keyword   asmKeyword        stgi
syn keyword   asmKeyword        str
syn keyword   asmKeyword        swapgs
syn keyword   asmKeyword        syscall
syn keyword   asmKeyword        sysenter
syn keyword   asmKeyword        sysexit
syn keyword   asmKeyword        sysret
syn keyword   asmKeyword        ud2
syn keyword   asmKeyword        verr
syn keyword   asmKeyword        verw
syn keyword   asmKeyword        vmload
syn keyword   asmKeyword        vmmcall
syn keyword   asmKeyword        vmrun
syn keyword   asmKeyword        vmsave
syn keyword   asmKeyword        wbinvd
syn keyword   asmKeyword        vrmsr

syn keyword   asmKeyword        jc jnae jnb jnc jae jz je jnz jbe jna jnbe ja js jns 
syn keyword   asmKeyword        jp jpe jnp jpo jl jnge jnl jge jle jng jnle jg
syn keyword   asmKeyword        cmovc cmovnae cmovnb cmovnc cmovae cmovz cmove cmovnz cmovbe cmovna cmovnbe cmova cmovs cmovns 
syn keyword   asmKeyword        cmovp cmovpe cmovnp cmovpo cmovl cmovnge cmovnl cmovge cmovle cmovng cmovnle cmovg
syn keyword   asmKeyword        setc setnae setnb setnc setae setz sete setnz setbe setna setnbe seta sets setns 
syn keyword   asmKeyword        setp setpe setnp setpo setl setnge setnl setge setle setng setnle setg

syn match   asmKeyword        "xor[bwlq]"
syn match   asmKeyword        "lea[bwlq]"
syn match   asmKeyword        "push[bwlq]"
syn match   asmKeyword        "pop[bwlq]"
syn match   asmKeyword        "sysret[bwlq]"
syn match   asmKeyword        "mov[bwlq]"
syn match   asmKeyword        "test[bwlq]"
syn match   asmKeyword        "add[bwlq]"
syn match   asmKeyword        "cmp[bwlq]"
syn match   asmKeyword        "call far"
syn match   asmKeyword        "jump far"


" TODO: 
"mov crn
"mov drn


" Registers

syn match  asmReg             "%rax"
syn match  asmReg             "%eax"
syn match  asmReg             "%ax"
syn match  asmReg             "%ah"
syn match  asmReg             "%al"
syn match  asmReg             "%rbx"
syn match  asmReg             "%ebx"
syn match  asmReg             "%bx"
syn match  asmReg             "%bh"
syn match  asmReg             "%bl"
syn match  asmReg             "%di"
syn match  asmReg             "%edi"
syn match  asmReg             "%rdi"
syn match  asmReg             "%rsi"
syn match  asmReg             "%esi"
syn match  asmReg             "%si"
syn match  asmReg             "%ip"
syn match  asmReg             "%eip"
syn match  asmReg             "%rip"
syn match  asmReg             "%rsp"
syn match  asmReg             "%esp"
syn match  asmReg             "%sp"
syn match  asmReg             "%gs"
syn match  asmReg             "%rcx"
syn match  asmReg             "%ecx"
syn match  asmReg             "%cx"
syn match  asmReg             "%ch"
syn match  asmReg             "%cl"
syn match  asmReg             "%rdx"
syn match  asmReg             "%edx"
syn match  asmReg             "%dx"
syn match  asmReg             "%dh"
syn match  asmReg             "%dl"

" labels

syn match asmLabel		"^[ \t]*[a-z_][a-z0-9_]*:"he=e-1

" numbers 
syn match decNumber		"\<0\+[1-7]\=[\t\n$,; ]\>"
syn match decNumber		"\<[1-9]\d*\>"
syn match decNumber		"\<\$[1-9]\d*\>"
syn match octNumber		"\<0[0-7][0-7]\+\>"
syn match hexNumber		"\<0[xX][0-9a-fA-F]\+\>"
syn match binNumber		"\<0[bB][0-1]*\>"

" C Preproc

syn region	cPreCondit	start="^\s*\(%:\|#\)\s*\(if\|ifdef\|ifndef\|elif\)\>" skip="\\$" end="$" end="//"me=s-1 contains=cComment,cCppString,cCharacter,cCppParen,cParenError,cNumbers,cCommentError,cSpaceError
syn match	cPreCondit	display "^\s*\(%:\|#\)\s*\(else\|endif\)\>"
if !exists("c_no_if0")
  if !exists("c_no_if0_fold")
    syn region	cCppOut		start="^\s*\(%:\|#\)\s*if\s\+0\+\>" end=".\@=\|$" contains=cCppOut2 fold
  else
    syn region	cCppOut		start="^\s*\(%:\|#\)\s*if\s\+0\+\>" end=".\@=\|$" contains=cCppOut2
  endif
  syn region	cCppOut2	contained start="0" end="^\s*\(%:\|#\)\s*\(endif\>\|else\>\|elif\>\)" contains=cSpaceError,cCppSkip
  syn region	cCppSkip	contained start="^\s*\(%:\|#\)\s*\(if\>\|ifdef\>\|ifndef\>\)" skip="\\$" end="^\s*\(%:\|#\)\s*endif\>" contains=cSpaceError,cCppSkip
endif
syn region	cIncluded	display contained start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match	cIncluded	display contained "<[^>]*>"
syn match	cInclude	display "^\s*\(%:\|#\)\s*include\>\s*["<]" contains=cIncluded
syn cluster	cPreProcGroup	contains=cPreCondit,cIncluded,cInclude,cDefine,cErrInParen,cErrInBracket,cUserLabel,cSpecial,cOctalZero,cCppOut,cCppOut2,cCppSkip,cFormat,cNumber,cFloat,cOctal,cOctalError,cNumbersCom,cString,cCommentSkip,cCommentString,cComment2String,@cCommentGroup,cCommentStartError,cParen,cBracket,cMulti
syn region	cDefine		start="^\s*\(%:\|#\)\s*\(define\|undef\)\>" skip="\\$" end="$" end="//"me=s-1 contains=ALLBUT,@cPreProcGroup,@Spell
syn region	cPreProc	start="^\s*\(%:\|#\)\s*\(pragma\>\|line\>\|warning\>\|warn\>\|error\>\)" skip="\\$" end="$" keepend contains=ALLBUT,@cPreProcGroup,@Spell

" Comments : (taken from c.vim)
" cCommentGroup allows adding matches for special things in comments
syn cluster	cCommentGroup	contains=cTodo
if exists("c_comment_strings")

  syntax match	cCommentSkip	contained "^\s*\*\($\|\s\+\)"
  syntax region cCommentString	contained start=+L\=\\\@<!"+ skip=+\\\\\|\\"+ end=+"+ end=+\*/+me=s-1 contains=cSpecial,cCommentSkip
  syntax region cComment2String	contained start=+L\=\\\@<!"+ skip=+\\\\\|\\"+ end=+"+ end="$" contains=cSpecial
  syntax region  cCommentL	start="//" skip="\\$" end="$" keepend contains=@cCommentGroup,cComment2String,cCharacter,cNumbersCom,cSpaceError,@Spell
  if exists("c_no_comment_fold")
    syntax region cComment	matchgroup=cCommentStart start="/\*" end="\*/" contains=@cCommentGroup,cCommentStartError,cCommentString,cCharacter,cNumbersCom,cSpaceError,@Spell
  else
    syntax region cComment	matchgroup=cCommentStart start="/\*" end="\*/" contains=@cCommentGroup,cCommentStartError,cCommentString,cCharacter,cNumbersCom,cSpaceError,@Spell fold
  endif
else
  syn region	cCommentL	start="//" skip="\\$" end="$" keepend contains=@cCommentGroup,cSpaceError,@Spell
  if exists("c_no_comment_fold")
    syn region	cComment	matchgroup=cCommentStart start="/\*" end="\*/" contains=@cCommentGroup,cCommentStartError,cSpaceError,@Spell
  else
    syn region	cComment	matchgroup=cCommentStart start="/\*" end="\*/" contains=@cCommentGroup,cCommentStartError,cSpaceError,@Spell fold
  endif
endif
" keep a // comment separately, it terminates a preproc. conditional
syntax match	cCommentError	display "\*/"
syntax match	cCommentStartError display "/\*"me=e-1 contained

" Bind groups to highlighting groups :

" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet


if version >= 508 || !exists("did_asmx86_64_syntax_inits")

  if version < 508
    let did_asmx64_64_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif



  HiLink        gasMacro        PreProc

  HiLink        asmKeyword      Keyword

  HiLink        asmLabel        Label

  HiLink        cCommentString	cString
  HiLink        cComment2String	cString
  HiLink        cCommentSkip	cComment
  HiLink        cCommentL	cComment
  HiLink        cCommentStart	cComment
  HiLink        cComment	Comment

  HiLink        cPreProc        PreProc
  HiLink        cDefine		Macro
  HiLink        cIncluded	cString
  HiLink        cPreCondit	PreCondit
  HiLink        cInclude	Include
  HiLink        cParenError	cError
  HiLink        cString		String
  HiLink        cCommentString	cString
  HiLink        cComment2String	cString
  HiLink        cCommentSkip	cComment
  HiLink        cTodo		Todo
  HiLink        cCppSkip	cCppOut
  HiLink        cCppOut2	cCppOut
  HiLink        cCppOut		Comment
  HiLink        cDefine		Macro

  HiLink        decNumber       Number
  HiLink        hexNumber       Number
  HiLink        octNumber       Number
  HiLink        binNumber       Number

  HiLink        asmReg          Identifier
  delcommand HiLink
endif

let b:current_syntax = "asmx86_64"

" vim: set shiftwidth=2
