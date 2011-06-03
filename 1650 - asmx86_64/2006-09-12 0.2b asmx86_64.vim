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

setlocal iskeyword+=.

syn keyword gasMacro .abort .ABORT .altmacro .asciz .arch .code32
syn keyword gasMacro .code64 .balign .byte .comm .cfi_startproc .cfi_endproc .cfi_def_cfa
syn keyword gasMacro .cfi_def_cfa_register .cfi_def_cfa_offset .cfi_adjust_cfa_offset .cfi_offset
syn keyword gasMacro .cfi_rel_offset .cfi_signal_frame .cfi_window_save .cfi_escape
syn keyword gasMacro .data .def .desc .dim .double .eject .else
syn keyword gasMacro .elseif .end .endef .endfunc .endif .equ .equiv
syn keyword gasMacro .eqv .err .error .exitm .extern .fail .file
syn keyword gasMacro .fill .float .func .global .globl .hidden .hword .ident .if .incbin .include
syn keyword gasMacro .int .internal .irp .irpc .lcomm .lflags .line .linkonce .list .ln .file .debug_line
syn keyword gasMacro .loc .basic_block .prologue_end .epilogue_begin .is_stmt .isa .loc_mark_blocks .long
syn keyword gasMacro .macro .mri .endm .noaltmacro .nolist .octa .org .p2align .popsection .previous .print
syn keyword gasMacro .protected .psize .purgem .pushsection .quad .rept .sbttl .scl .section .set .short
syn keyword gasMacro .single .size .skip .sleb128 .space .stabd .stabn .stabs .string .struct .subsection
syn keyword gasMacro .symver .tag .text .title .type .uleb128 .val .version .vtable_entry .vtable_inherit
syn keyword gasMacro .warning .weak .weakref .word


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

setlocal iskeyword+=%

syn keyword  asmReg             %rax rax
syn keyword  asmReg             %eax eax
syn keyword  asmReg             %ax ax
syn keyword  asmReg             %ah ah
syn keyword  asmReg             %al al
syn keyword  asmReg             %rbx rbx
syn keyword  asmReg             %ebx ebx
syn keyword  asmReg             %bx bx
syn keyword  asmReg             %bh bh
syn keyword  asmReg             %bl bl
syn keyword  asmReg             %di di
syn keyword  asmReg             %edi edi
syn keyword  asmReg             %rdi rdi
syn keyword  asmReg             %rsi rsi
syn keyword  asmReg             %esi esi
syn keyword  asmReg             %si si
syn keyword  asmReg             %ip ip
syn keyword  asmReg             %eip eip
syn keyword  asmReg             %rip rip
syn keyword  asmReg             %rsp rsp
syn keyword  asmReg             %esp esp
syn keyword  asmReg             %sp sp
syn keyword  asmReg             %rbp rbp
syn keyword  asmReg             %ebp ebp
syn keyword  asmReg             %bp bp
syn keyword  asmReg             %gs gs
syn keyword  asmReg             %es es
syn keyword  asmReg             %ds ds
syn keyword  asmReg             %fs fs
syn keyword  asmReg             %ss ss
syn keyword  asmReg             %cs cs
syn keyword  asmReg             %rcx rcx
syn keyword  asmReg             %ecx ecx
syn keyword  asmReg             %cx cx
syn keyword  asmReg             %ch ch
syn keyword  asmReg             %cl cl
syn keyword  asmReg             %rdx rdx
syn keyword  asmReg             %edx edx
syn keyword  asmReg             %dx dx
syn keyword  asmReg             %dh dh
syn keyword  asmReg             %dl dl
syn keyword  asmReg             %r8 r8
syn keyword  asmReg             %r9 r9
syn keyword  asmReg             %r10 r10
syn keyword  asmReg             %r11 r11
syn keyword  asmReg             %r12 r12
syn keyword  asmReg             %r13 r13
syn keyword  asmReg             %r14 r14
syn keyword  asmReg             %r15 r15

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

syn region 	asmComment	start="# " end="$"
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
  HiLink	asmComment	Comment
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
