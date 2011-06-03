" Vim syntax file
" Language:	GNU as (AT&T) assembler for X86
" Maintainer:   Rene Koecher <shirk@bitspin.org>
" Last Change:  2009 Nov 21
" Remark:       Intel compatible instructions only (for now)

if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

setlocal iskeyword +=%,.,-,_
setlocal isident   +=%,.,-,_

syn case ignore

" directives
syn keyword gasDirective	.abort .ABORT .align .balignw .balignl
syn keyword gasDirective	.cfi_startproc .cfi_sections .cfi_endproc .cfi_personality
syn keyword gasDirective	.cfi_lsda .cfi_def_cfa .cfi_def_cfa_register .cfi_def_cfa_offset
syn keyword gasDirective	.cfi_adjust_cfa_offset .cfi_offset .cfi_rel_offset .cfi_register
syn keyword gasDirective	.cfi_restore .cfi_undefined .cfi_same_value .cfi_remember_state
syn keyword gasDirective	.cfi_return_column .cfi_signal_frame .cfi_window_save .cfi_escape
syn keyword gasDirective	.cfi_val_encoded_addr .data .def .desc .dim .eject
syn keyword gasDirective	.else .elseif .endef .endif .equ .equiv .eqv .err
syn keyword gasDirective	.error .exitm .extern .fail .file .fill .global .globl
syn keyword gasDirective	.gnu_attribute .hidden .ident .if .incbin .include .internal
syn keyword gasDirective	.irp .irpc .lcomm .lflags .line .linkonce .list .ln .loc .loc_mark_labels
syn keyword gasDirective	.local .mri .nolist .octa .org .p2alignw .p2alignl
syn keyword gasDirective	.popsection .previous .print .protected .psize .purgem .pushsection .quad
syn keyword gasDirective	.reloc .rept .sbttl .scl .section .set .single .size .skip .sleb128
syn keyword gasDirective	.space .stabd .stabn .stabs .struct .subsection
syn keyword gasDirective	.symver .tag .text .title .type .uleb128 .val .version
syn keyword gasDirective	.vtable_entry .vtable_inherit .warning .weak .weakref

syn keyword gasDirectiveStore	.byte .hword .word .int .long .double .short .float
syn keyword gasDirectiveStore	.string .string8 .string16 .ascii .asciz .comm

syn keyword gasDirectiveMacro	.altmacro .macro .noaltmacro .end .func .endfunc

" i*86 directives
syn keyword gasDirectivei386	.att_syntax .intel_syntax .att_mnemonic .intel_mnemonic .lcomm

" i*86 register set
syn keyword gasRegisteri386	%eax %ebx %ecx %edx %ax %bx %cx %db %ah %al %bh %bl %ch %cl %dh %dl
syn keyword gasRegisteri386	%edi %esi %esp %ebp %di %si %sp %bp
syn keyword gasRegisteri386	%cs %ds %es %fs %gs %ss

" i*86 special registers
syn keyword gasRegisteri386Cr	%cr0 %cr1 %cr2 %cr3 %cr4 %cr8
syn keyword gasRegisteri386Dr	%dr0 %dr1 %dr2 %dr3 %dr4 %dr6 %dr6 %dr7
syn keyword gasRegisteri386Tr	%tr0 %tr1 %tr2 %tr3 %tr4 %tr5 %tr6 %tr7
syn match   gasRegisteri386Fp	"%sp\(([0-7])\)\{0,1\}"
syn match   gasRegisteri386MMX	"%x\{0,1\}mm[0-7]"

" i*86 opcodes (basic)
syn keyword gasOpcodeX86_Base aaa aad aam aas adc add and arpl
syn keyword gasOpcodeX86_Base bb0_reset bb1_reset bound bsf bsr bswap bt btc
syn keyword gasOpcodeX86_Base btr bts call cbw cdq cdqe clc cld
syn keyword gasOpcodeX86_Base clgi cli clts cmc cmov cmove cmovne cmovz cmovnz
syn keyword gasOpcodeX86_Base cmovc cmovnc cmovp cmovnp cmp cmpsb cmpsd
syn keyword gasOpcodeX86_Base cmpsq cmpsw cmpxchg cmpxchg16b cmpxchg486 cmpxchg8b cpuid cpu_read
syn keyword gasOpcodeX86_Base cpu_write cqo cwd cwde daa das dec div
syn keyword gasOpcodeX86_Base dmint emms enter equ f2xm1 fabs fadd faddp
syn keyword gasOpcodeX86_Base fbld fbstp fchs fclex fcmovb fcmovbe fcmove fcmovnb
syn keyword gasOpcodeX86_Base fcmovnbe fcmovne fcmovnu fcmovu fcom fcomi fcomip fcomp
syn keyword gasOpcodeX86_Base fcompp fcos fdecstp fdisi fdiv fdivp fdivr fdivrp
syn keyword gasOpcodeX86_Base femms feni ffree ffreep fiadd ficom ficomp fidiv
syn keyword gasOpcodeX86_Base fidivr fild fimul fincstp finit fist fistp fisttp
syn keyword gasOpcodeX86_Base fisub fisubr fld fld1 fldcw fldenv fldl2e fldl2t
syn keyword gasOpcodeX86_Base fldlg2 fldln2 fldpi fldz fmul fmulp fnclex fndisi
syn keyword gasOpcodeX86_Base fneni fninit fnop fnsave fnstcw fnstenv fnstsw fpatan
syn keyword gasOpcodeX86_Base fprem fprem1 fptan frndint frstor fsave fscale fsetpm
syn keyword gasOpcodeX86_Base fsin fsincos fsqrt fst fstcw fstenv fstp fstsw
syn keyword gasOpcodeX86_Base fsub fsubp fsubr fsubrp ftst fucom fucomi fucomip
syn keyword gasOpcodeX86_Base fucomp fucompp fwait fxam fxch fxtract fyl2x fyl2xp1
syn keyword gasOpcodeX86_Base hlt ibts icebp idiv imul in inc incbin
syn keyword gasOpcodeX86_Base insb insd insw int int01 int03 int1 int3 into
syn keyword gasOpcodeX86_Base invd invlpg invlpga iret iretd iretq iretw jcxz
syn keyword gasOpcodeX86_Base jecxz jmp jmpe jrcxz je jne jz jnz jc jnc jp jnp lahf lar lds
syn keyword gasOpcodeX86_Base lea leave les lfence lfs lgdt lgs lidt
syn keyword gasOpcodeX86_Base lldt lmsw loadall loadall286 lodsb lodsd lodsq lodsw
syn keyword gasOpcodeX86_Base loop loope loopne loopnz loopz lsl lss ltr
syn keyword gasOpcodeX86_Base mfence monitor mov movd movq movsb movsd movsq
syn keyword gasOpcodeX86_Base movsw movsx movsxd movzx mul mwait neg nop
syn keyword gasOpcodeX86_Base not or out outsb outsd outsw packssdw packsswb
syn keyword gasOpcodeX86_Base packuswb paddb paddd paddsb paddsiw paddsw paddusb paddusw
syn keyword gasOpcodeX86_Base paddw pand pandn pause paveb pavgusb pcmpeqb pcmpeqd
syn keyword gasOpcodeX86_Base pcmpeqw pcmpgtb pcmpgtd pcmpgtw pdistib pf2id pfa pfadd
syn keyword gasOpcodeX86_Base pfcmpeq pfcmpge pfcmpgt pfmax pfmin pfmul pfrcp pfrcpit1
syn keyword gasOpcodeX86_Base pfrcpit2 pfrsqit1 pfrsqrt pfsub pfsubr pi2fd pmachriw pmaddwd
syn keyword gasOpcodeX86_Base pmagw pmulhriw pmulhrwa pmulhrwc pmulhw pmullw pmvgezb pmvlzb
syn keyword gasOpcodeX86_Base pmvnzb pmvzb pop popa popad popaw popf popfd popfq
syn keyword gasOpcodeX86_Base popfw por prefetch prefetchw pslld psllq psllw psrad
syn keyword gasOpcodeX86_Base psraw psrld psrlq psrlw psubb psubd psubsb psubsiw
syn keyword gasOpcodeX86_Base psubsw psubusb psubusw psubw punpckhbw punpckhdq punpckhwd punpcklbw
syn keyword gasOpcodeX86_Base punpckldq punpcklwd push pusha pushad pushaw pushf pushfd
syn keyword gasOpcodeX86_Base pushfq pushfw pxor rcl rcr rdm rdmsr rdpmc
syn keyword gasOpcodeX86_Base rdshr rdtsc rdtscp ret retf retn rol ror
syn keyword gasOpcodeX86_Base rsdc rsldt rsm rsts sahf sal salc sar
syn keyword gasOpcodeX86_Base sbb scasb scasd scasq scasw sete setne setz setnz
syn keyword gasOpcodeX86_Base setc setnc setp setnp sfence sgdt
syn keyword gasOpcodeX86_Base shl shld shr shrd sidt skinit sldt smi
syn keyword gasOpcodeX86_Base smint smintold smsw stc std stgi sti stosb
syn keyword gasOpcodeX86_Base stosd stosq stosw str sub svdc svldt svts
syn keyword gasOpcodeX86_Base swapgs syscall sysenter sysexit sysret test ud0
syn keyword gasOpcodeX86_Base ud1 ud2 ud2a ud2b umov verr verw wbinvd
syn keyword gasOpcodeX86_Base wrmsr wrshr xadd xbts xchg xlat xlatb xor

" -- Katmai Streaming SIMD instructions (SSE -- a.k.a. KNI, XMM, MMX2)
syn keyword gasOpcodeX86_SIMD addps addss andnps andps
syn keyword gasOpcodeX86_SIMD cmpeqps cmpeqss cmpleps cmpless
syn keyword gasOpcodeX86_SIMD cmpltps cmpltss cmpneqps cmpneqss
syn keyword gasOpcodeX86_SIMD cmpnleps cmpnless cmpnltps cmpnltss
syn keyword gasOpcodeX86_SIMD cmpordps cmpordss cmpps cmpss
syn keyword gasOpcodeX86_SIMD cmpunordps cmpunordss comiss cvtpi2ps
syn keyword gasOpcodeX86_SIMD cvtps2pi cvtsi2ss cvtss2si cvttps2pi
syn keyword gasOpcodeX86_SIMD cvttss2si divps divss ldmxcsr
syn keyword gasOpcodeX86_SIMD maxps maxss minps minss
syn keyword gasOpcodeX86_SIMD movaps movhlps movhps movlhps
syn keyword gasOpcodeX86_SIMD movlps movmskps movntps movss
syn keyword gasOpcodeX86_SIMD movups mulps mulss orps
syn keyword gasOpcodeX86_SIMD rcpps rcpss rsqrtps rsqrtss
syn keyword gasOpcodeX86_SIMD shufps sqrtps sqrtss stmxcsr
syn keyword gasOpcodeX86_SIMD subps subss ucomiss unpckhps
syn keyword gasOpcodeX86_SIMD unpcklps xorps

" -- Introduced in Deschutes but necessary for SSE support
syn keyword gasOpcodeX86_SSE fxrstor fxsave

" -- XSAVE group (AVX and extended state)
syn keyword gasOpcodeX86_AVX xgetbv xsetbv xsave xrstor

" -- Generic memory operations
syn keyword gasOpcodeX86_MEM prefetchnta prefetcht0 prefetcht1 prefetcht2 sfence

" -- New MMX instructions introduced in Katmai
syn keyword gasOpcodeX86_MMX maskmovq movntq pavgb pavgw
syn keyword gasOpcodeX86_MMX pextrw pinsrw pmaxsw pmaxub
syn keyword gasOpcodeX86_MMX pminsw pminub pmovmskb pmulhuw
syn keyword gasOpcodeX86_MMX psadbw pshufw

" -- AMD Enhanced 3DNow! (Athlon) instructions
syn keyword gasOpcodeX86_3DNOW pf2iw pfnacc pfpnacc pi2fw pswapd

" -- Willamette SSE2 Cacheability Instructions
syn keyword gasOpcodeX86_SSE2 clflush lfence maskmovdqu mfence
syn keyword gasOpcodeX86_SSE2 movntdq movnti movntpd

" -- Willamette MMX instructions (SSE2 SIMD Integer Instructions)
syn keyword gasOpcodeX86_SSE2 movd movdq2q movdqa movdqu
syn keyword gasOpcodeX86_SSE2 movq movq2dq packssdw packsswb
syn keyword gasOpcodeX86_SSE2 packuswb paddb paddd paddq
syn keyword gasOpcodeX86_SSE2 paddsb paddsw paddusb paddusw
syn keyword gasOpcodeX86_SSE2 paddw pand pandn pavgb
syn keyword gasOpcodeX86_SSE2 pavgw pcmpeqb pcmpeqd pcmpeqw
syn keyword gasOpcodeX86_SSE2 pcmpgtb pcmpgtd pcmpgtw pextrw
syn keyword gasOpcodeX86_SSE2 pinsrw pmaddwd pmaxsw pmaxub
syn keyword gasOpcodeX86_SSE2 pminsw pminub pmovmskb pmulhuw
syn keyword gasOpcodeX86_SSE2 pmulhw pmullw pmuludq por
syn keyword gasOpcodeX86_SSE2 psadbw pshufd pshufhw pshuflw
syn keyword gasOpcodeX86_SSE2 pslld pslldq psllq psllw
syn keyword gasOpcodeX86_SSE2 psrad psraw psrld psrldq
syn keyword gasOpcodeX86_SSE2 psrlq psrlw psubb psubd
syn keyword gasOpcodeX86_SSE2 psubq psubsb psubsw psubusb
syn keyword gasOpcodeX86_SSE2 psubusw psubw punpckhbw punpckhdq
syn keyword gasOpcodeX86_SSE2 punpckhqdq punpckhwd punpcklbw punpckldq
syn keyword gasOpcodeX86_SSE2 punpcklqdq punpcklwd pxor 

" -- Willamette Streaming SIMD instructions (SSE2)
syn keyword gasOpcodeX86_SSE2 addpd addsd andnpd andpd
syn keyword gasOpcodeX86_SSE2 cmpeqpd cmpeqsd cmplepd cmplesd
syn keyword gasOpcodeX86_SSE2 cmpltpd cmpltsd cmpneqpd cmpneqsd
syn keyword gasOpcodeX86_SSE2 cmpnlepd cmpnlesd cmpnltpd cmpnltsd
syn keyword gasOpcodeX86_SSE2 cmpordpd cmpordsd cmppd cmpsd
syn keyword gasOpcodeX86_SSE2 cmpunordpd cmpunordsd comisd cvtdq2pd
syn keyword gasOpcodeX86_SSE2 cvtdq2ps cvtpd2dq cvtpd2pi cvtpd2ps
syn keyword gasOpcodeX86_SSE2 cvtpi2pd cvtps2dq cvtps2pd cvtsd2si
syn keyword gasOpcodeX86_SSE2 cvtsd2ss cvtsi2sd cvtss2sd cvttpd2dq
syn keyword gasOpcodeX86_SSE2 cvttpd2pi cvttps2dq cvttsd2si divpd
syn keyword gasOpcodeX86_SSE2 divsd maxpd maxsd minpd
syn keyword gasOpcodeX86_SSE2 minsd movapd movhpd movlpd
syn keyword gasOpcodeX86_SSE2 movmskpd movsd movupd mulpd
syn keyword gasOpcodeX86_SSE2 mulsd orpd shufpd sqrtpd
syn keyword gasOpcodeX86_SSE2 sqrtsd subpd subsd ucomisd
syn keyword gasOpcodeX86_SSE2 unpckhpd unpcklpd xorpd 

" -- Prescott New Instructions (SSE3)
syn keyword gasOpcodeX86_SSE3 addsubpd addsubps haddpd haddps
syn keyword gasOpcodeX86_SSE3 hsubpd hsubps lddqu movddup
syn keyword gasOpcodeX86_SSE3 movshdup movsldup

" -- VMX Instructions
syn keyword gasOpcodeX86_VMX vmcall vmclear vmlaunch vmload
syn keyword gasOpcodeX86_VMX vmmcall vmptrld vmptrst vmread
syn keyword gasOpcodeX86_VMX vmresume vmrun vmsave vmwrite
syn keyword gasOpcodeX86_VMX vmxoff vmxon

" -- Extended Page Tables VMX instructions
syn keyword gasOpcodeX86_VMX invept invvpid

" -- Tejas New Instructions (SSSE3)
syn keyword gasOpcodeX86_SSE3 pabsb pabsd pabsw palignr
syn keyword gasOpcodeX86_SSE3 phaddd phaddsw phaddw phsubd
syn keyword gasOpcodeX86_SSE3 phsubsw phsubw pmaddubsw pmulhrsw
syn keyword gasOpcodeX86_SSE3 pshufb psignb psignd psignw

" -- AMD SSE4A
syn keyword gasOpcodeX86_SSE4A extrq insertq movntsd movntss

" -- New instructions in Barcelona
syn keyword gasOpcodeX86_SSE4A lzcnt

" -- Penryn New Instructions (SSE4.1)
syn keyword gasOpcodeX86_SSE4_1 blendpd blendps blendvpd blendvps
syn keyword gasOpcodeX86_SSE4_1 dppd dpps extractps insertps
syn keyword gasOpcodeX86_SSE4_1 movntdqa mpsadbw packusdw pblendvb
syn keyword gasOpcodeX86_SSE4_1 pblendw pcmpeqq pextrb pextrd
syn keyword gasOpcodeX86_SSE4_1 pextrq pextrw phminposuw pinsrb
syn keyword gasOpcodeX86_SSE4_1 pinsrd pinsrq pmaxsb pmaxsd
syn keyword gasOpcodeX86_SSE4_1 pmaxud pmaxuw pminsb pminsd
syn keyword gasOpcodeX86_SSE4_1 pminud pminuw pmovsxbd pmovsxbq
syn keyword gasOpcodeX86_SSE4_1 pmovsxbw pmovsxdq pmovsxwd pmovsxwq
syn keyword gasOpcodeX86_SSE4_1 pmovzxbd pmovzxbq pmovzxbw pmovzxdq
syn keyword gasOpcodeX86_SSE4_1 pmovzxwd pmovzxwq pmuldq pmulld
syn keyword gasOpcodeX86_SSE4_1 ptest roundpd roundps roundsd
syn keyword gasOpcodeX86_SSE4_1 roundss

" -- Nehalem New Instructions (SSE4.2)
syn keyword gasOpcodeX86_SSE4_2 crc32 pcmpestri pcmpestrm pcmpgtq
syn keyword gasOpcodeX86_SSE4_2 pcmpistri pcmpistrm popcnt

" -- AMD SSE5 instructions
syn keyword gasOpcodeX86_SSE5 comeqpd comeqps comeqsd comeqss
syn keyword gasOpcodeX86_SSE5 comfalsepd comfalseps comfalsesd comfalsess
syn keyword gasOpcodeX86_SSE5 comlepd comleps comlesd comless
syn keyword gasOpcodeX86_SSE5 comltpd comltps comltsd comltss
syn keyword gasOpcodeX86_SSE5 comneqpd comneqps comneqsd comneqss
syn keyword gasOpcodeX86_SSE5 comnlepd comnleps comnlesd comnless
syn keyword gasOpcodeX86_SSE5 comnltpd comnltps comnltsd comnltss
syn keyword gasOpcodeX86_SSE5 comordpd comordps comordsd comordss
syn keyword gasOpcodeX86_SSE5 compd comps comsd comss
syn keyword gasOpcodeX86_SSE5 comtruepd comtrueps comtruesd comtruess
syn keyword gasOpcodeX86_SSE5 comueqpd comueqps comueqsd comueqss
syn keyword gasOpcodeX86_SSE5 comulepd comuleps comulesd comuless
syn keyword gasOpcodeX86_SSE5 comultpd comultps comultsd comultss
syn keyword gasOpcodeX86_SSE5 comuneqpd comuneqps comuneqsd comuneqss
syn keyword gasOpcodeX86_SSE5 comunlepd comunleps comunlesd comunless
syn keyword gasOpcodeX86_SSE5 comunltpd comunltps comunltsd comunltss
syn keyword gasOpcodeX86_SSE5 comunordpd comunordps comunordsd comunordss
syn keyword gasOpcodeX86_SSE5 cvtph2ps cvtps2ph fmaddpd fmaddps
syn keyword gasOpcodeX86_SSE5 fmaddsd fmaddss fmsubpd fmsubps
syn keyword gasOpcodeX86_SSE5 fmsubsd fmsubss fnmaddpd fnmaddps
syn keyword gasOpcodeX86_SSE5 fnmaddsd fnmaddss fnmsubpd fnmsubps
syn keyword gasOpcodeX86_SSE5 fnmsubsd fnmsubss frczpd frczps
syn keyword gasOpcodeX86_SSE5 frczsd frczss pcmov pcomb
syn keyword gasOpcodeX86_SSE5 pcomd pcomeqb pcomeqd pcomeqq
syn keyword gasOpcodeX86_SSE5 pcomequb pcomequd pcomequq pcomequw
syn keyword gasOpcodeX86_SSE5 pcomeqw pcomfalseb pcomfalsed pcomfalseq
syn keyword gasOpcodeX86_SSE5 pcomfalseub pcomfalseud pcomfalseuq pcomfalseuw
syn keyword gasOpcodeX86_SSE5 pcomfalsew pcomgeb pcomged pcomgeq
syn keyword gasOpcodeX86_SSE5 pcomgeub pcomgeud pcomgeuq pcomgeuw
syn keyword gasOpcodeX86_SSE5 pcomgew pcomgtb pcomgtd pcomgtq
syn keyword gasOpcodeX86_SSE5 pcomgtub pcomgtud pcomgtuq pcomgtuw
syn keyword gasOpcodeX86_SSE5 pcomgtw pcomleb pcomled pcomleq
syn keyword gasOpcodeX86_SSE5 pcomleub pcomleud pcomleuq pcomleuw
syn keyword gasOpcodeX86_SSE5 pcomlew pcomltb pcomltd pcomltq
syn keyword gasOpcodeX86_SSE5 pcomltub pcomltud pcomltuq pcomltuw
syn keyword gasOpcodeX86_SSE5 pcomltw pcomneqb pcomneqd pcomneqq
syn keyword gasOpcodeX86_SSE5 pcomnequb pcomnequd pcomnequq pcomnequw
syn keyword gasOpcodeX86_SSE5 pcomneqw pcomq pcomtrueb pcomtrued
syn keyword gasOpcodeX86_SSE5 pcomtrueq pcomtrueub pcomtrueud pcomtrueuq
syn keyword gasOpcodeX86_SSE5 pcomtrueuw pcomtruew pcomub pcomud
syn keyword gasOpcodeX86_SSE5 pcomuq pcomuw pcomw permpd
syn keyword gasOpcodeX86_SSE5 permps phaddbd phaddbq phaddbw
syn keyword gasOpcodeX86_SSE5 phadddq phaddubd phaddubq phaddubw
syn keyword gasOpcodeX86_SSE5 phaddudq phadduwd phadduwq phaddwd
syn keyword gasOpcodeX86_SSE5 phaddwq phsubbw phsubdq phsubwd
syn keyword gasOpcodeX86_SSE5 pmacsdd pmacsdqh pmacsdql pmacssdd
syn keyword gasOpcodeX86_SSE5 pmacssdqh pmacssdql pmacsswd pmacssww
syn keyword gasOpcodeX86_SSE5 pmacswd pmacsww pmadcsswd pmadcswd
syn keyword gasOpcodeX86_SSE5 pperm protb protd protq
syn keyword gasOpcodeX86_SSE5 protw pshab pshad pshaq
syn keyword gasOpcodeX86_SSE5 pshaw pshlb pshld pshlq
syn keyword gasOpcodeX86_SSE5 pshlw roundpd roundps roundsd
syn keyword gasOpcodeX86_SSE5 roundss
" -- Intel SMX
syn keyword gasOpcodeX86_SMX getsec

" -- Geode (Cyrix) 3DNow! additions
syn keyword gasOpcodeX86_3DNOW pfrcpv pfrsqrtv

" -- Intel new instructions in ???
syn keyword gasOpcodeX86 movbe

" -- Intel AES instructions
syn keyword gasOpcodeX86_AES aesenc aesenclast aesdec aesdeclast
syn keyword gasOpcodeX86_AES aesimc aeskeygenassist

" -- Intel AVX AES instructions
syn keyword gasOpcodeX86_AVX_AES vaesenc vaesenclast vaesdec vaesdeclast
syn keyword gasOpcodesX86_AVX_AES vaesimc vaeskeygenassist

" -- Intel AVX instructions
syn keyword gasOpcodeX86_AVX vaddpd vaddps vaddsd vaddss
syn keyword gasOpcodeX86_AVX vaddsubpd vaddsubps vandnpd vandnps
syn keyword gasOpcodeX86_AVX vandpd vandps vblendpd vblendps
syn keyword gasOpcodeX86_AVX vblendvpd vblendvps vbroadcastf128 vbroadcastsd
syn keyword gasOpcodeX86_AVX vbroadcastss vcmpeqpd vcmpeqps vcmpeqsd
syn keyword gasOpcodeX86_AVX vcmpeqss vcmpeq_ospd vcmpeq_osps vcmpeq_ossd
syn keyword gasOpcodeX86_AVX vcmpeq_osss vcmpeq_uqpd vcmpeq_uqps vcmpeq_uqsd
syn keyword gasOpcodeX86_AVX vcmpeq_uqss vcmpeq_uspd vcmpeq_usps vcmpeq_ussd
syn keyword gasOpcodeX86_AVX vcmpeq_usss vcmpfalsepd vcmpfalseps vcmpfalsesd
syn keyword gasOpcodeX86_AVX vcmpfalsess vcmpfalse_ospd vcmpfalse_osps vcmpfalse_ossd
syn keyword gasOpcodeX86_AVX vcmpfalse_osss vcmpgepd vcmpgeps vcmpgesd
syn keyword gasOpcodeX86_AVX vcmpgess vcmpge_oqpd vcmpge_oqps vcmpge_oqsd
syn keyword gasOpcodeX86_AVX vcmpge_oqss vcmpgtpd vcmpgtps vcmpgtsd
syn keyword gasOpcodeX86_AVX vcmpgtss vcmpgt_oqpd vcmpgt_oqps vcmpgt_oqsd
syn keyword gasOpcodeX86_AVX vcmpgt_oqss vcmplepd vcmpleps vcmplesd
syn keyword gasOpcodeX86_AVX vcmpless vcmple_oqpd vcmple_oqps vcmple_oqsd
syn keyword gasOpcodeX86_AVX vcmple_oqss vcmpltpd vcmpltps vcmpltsd
syn keyword gasOpcodeX86_AVX vcmpltss vcmplt_oqpd vcmplt_oqps vcmplt_oqsd
syn keyword gasOpcodeX86_AVX vcmplt_oqss vcmpneqpd vcmpneqps vcmpneqsd
syn keyword gasOpcodeX86_AVX vcmpneqss vcmpneq_oqpd vcmpneq_oqps vcmpneq_oqsd
syn keyword gasOpcodeX86_AVX vcmpneq_oqss vcmpneq_ospd vcmpneq_osps vcmpneq_ossd
syn keyword gasOpcodeX86_AVX vcmpneq_osss vcmpneq_uspd vcmpneq_usps vcmpneq_ussd
syn keyword gasOpcodeX86_AVX vcmpneq_usss vcmpngepd vcmpngeps vcmpngesd
syn keyword gasOpcodeX86_AVX vcmpngess vcmpnge_uqpd vcmpnge_uqps vcmpnge_uqsd
syn keyword gasOpcodeX86_AVX vcmpnge_uqss vcmpngtpd vcmpngtps vcmpngtsd
syn keyword gasOpcodeX86_AVX vcmpngtss vcmpngt_uqpd vcmpngt_uqps vcmpngt_uqsd
syn keyword gasOpcodeX86_AVX vcmpngt_uqss vcmpnlepd vcmpnleps vcmpnlesd
syn keyword gasOpcodeX86_AVX vcmpnless vcmpnle_uqpd vcmpnle_uqps vcmpnle_uqsd
syn keyword gasOpcodeX86_AVX vcmpnle_uqss vcmpnltpd vcmpnltps vcmpnltsd
syn keyword gasOpcodeX86_AVX vcmpnltss vcmpnlt_uqpd vcmpnlt_uqps vcmpnlt_uqsd
syn keyword gasOpcodeX86_AVX vcmpnlt_uqss vcmpordpd vcmpordps vcmpordsd
syn keyword gasOpcodeX86_AVX vcmpordss vcmpord_spd vcmpord_sps vcmpord_ssd
syn keyword gasOpcodeX86_AVX vcmpord_sss vcmppd vcmpps vcmpsd
syn keyword gasOpcodeX86_AVX vcmpss vcmptruepd vcmptrueps vcmptruesd
syn keyword gasOpcodeX86_AVX vcmptruess vcmptrue_uspd vcmptrue_usps vcmptrue_ussd
syn keyword gasOpcodeX86_AVX vcmptrue_usss vcmpunordpd vcmpunordps vcmpunordsd
syn keyword gasOpcodeX86_AVX vcmpunordss vcmpunord_spd vcmpunord_sps vcmpunord_ssd
syn keyword gasOpcodeX86_AVX vcmpunord_sss vcomisd vcomiss vcvtdq2pd
syn keyword gasOpcodeX86_AVX vcvtdq2ps vcvtpd2dq vcvtpd2ps vcvtps2dq
syn keyword gasOpcodeX86_AVX vcvtps2pd vcvtsd2si vcvtsd2ss vcvtsi2sd
syn keyword gasOpcodeX86_AVX vcvtsi2ss vcvtss2sd vcvtss2si vcvttpd2dq
syn keyword gasOpcodeX86_AVX vcvttps2dq vcvttsd2si vcvttss2si vdivpd
syn keyword gasOpcodeX86_AVX vdivps vdivsd vdivss vdppd
syn keyword gasOpcodeX86_AVX vdpps vextractf128 vextractps vhaddpd
syn keyword gasOpcodeX86_AVX vhaddps vhsubpd vhsubps vinsertf128
syn keyword gasOpcodeX86_AVX vinsertps vlddqu vldmxcsr vldqqu
syn keyword gasOpcodeX86_AVX vmaskmovdqu vmaskmovpd vmaskmovps vmaxpd
syn keyword gasOpcodeX86_AVX vmaxps vmaxsd vmaxss vminpd
syn keyword gasOpcodeX86_AVX vminps vminsd vminss vmovapd
syn keyword gasOpcodeX86_AVX vmovaps vmovd vmovddup vmovdqa
syn keyword gasOpcodeX86_AVX vmovdqu vmovhlps vmovhpd vmovhps
syn keyword gasOpcodeX86_AVX vmovlhps vmovlpd vmovlps vmovmskpd
syn keyword gasOpcodeX86_AVX vmovmskps vmovntdq vmovntdqa vmovntpd
syn keyword gasOpcodeX86_AVX vmovntps vmovntqq vmovq vmovqqa
syn keyword gasOpcodeX86_AVX vmovqqu vmovsd vmovshdup vmovsldup
syn keyword gasOpcodeX86_AVX vmovss vmovupd vmovups vmpsadbw
syn keyword gasOpcodeX86_AVX vmulpd vmulps vmulsd vmulss
syn keyword gasOpcodeX86_AVX vorpd vorps vpabsb vpabsd
syn keyword gasOpcodeX86_AVX vpabsw vpackssdw vpacksswb
syn keyword gasOpcodeX86_AVX vpackusdw vpackuswb vpaddb vpaddd
syn keyword gasOpcodeX86_AVX vpaddq vpaddsb vpaddsw vpaddusb
syn keyword gasOpcodeX86_AVX vpaddusw vpaddw vpalignr vpand
syn keyword gasOpcodeX86_AVX vpandn vpavgb vpavgw vpblendvb
syn keyword gasOpcodeX86_AVX vpblendw vpcmpeqb vpcmpeqd vpcmpeqq
syn keyword gasOpcodeX86_AVX vpcmpeqw vpcmpestri vpcmpestrm vpcmpgtb
syn keyword gasOpcodeX86_AVX vpcmpgtd vpcmpgtq vpcmpgtw vpcmpistri
syn keyword gasOpcodeX86_AVX vpcmpistrm vperm2f128 vpermil2pd vpermil2ps
syn keyword gasOpcodeX86_AVX vpermilmo2pd vpermilmo2ps vpermilmz2pd vpermilmz2ps
syn keyword gasOpcodeX86_AVX vpermilpd vpermilps vpermiltd2pd vpermiltd2ps
syn keyword gasOpcodeX86_AVX vpextrb vpextrd vpextrq vpextrw
syn keyword gasOpcodeX86_AVX vphaddd vphaddsw vphaddw vphminposuw
syn keyword gasOpcodeX86_AVX vphsubd vphsubsw vphsubw vpinsrb
syn keyword gasOpcodeX86_AVX vpinsrd vpinsrq vpinsrw vpmaddubsw
syn keyword gasOpcodeX86_AVX vpmaddwd vpmaxsb vpmaxsd vpmaxsw
syn keyword gasOpcodeX86_AVX vpmaxub vpmaxud vpmaxuw vpminsb
syn keyword gasOpcodeX86_AVX vpminsd vpminsw vpminub vpminud
syn keyword gasOpcodeX86_AVX vpminuw vpmovmskb vpmovsxbd vpmovsxbq
syn keyword gasOpcodeX86_AVX vpmovsxbw vpmovsxdq vpmovsxwd vpmovsxwq
syn keyword gasOpcodeX86_AVX vpmovzxbd vpmovzxbq vpmovzxbw vpmovzxdq
syn keyword gasOpcodeX86_AVX vpmovzxwd vpmovzxwq vpmuldq vpmulhrsw
syn keyword gasOpcodeX86_AVX vpmulhuw vpmulhw vpmulld vpmullw
syn keyword gasOpcodeX86_AVX vpmuludq vpor vpsadbw vpshufb
syn keyword gasOpcodeX86_AVX vpshufd vpshufhw vpshuflw vpsignb
syn keyword gasOpcodeX86_AVX vpsignd vpsignw vpslld vpslldq
syn keyword gasOpcodeX86_AVX vpsllq vpsllw vpsrad vpsraw
syn keyword gasOpcodeX86_AVX vpsrld vpsrldq vpsrlq vpsrlw
syn keyword gasOpcodeX86_AVX vpsubb vpsubd vpsubq vpsubsb
syn keyword gasOpcodeX86_AVX vpsubsw vpsubusb vpsubusw vpsubw
syn keyword gasOpcodeX86_AVX vptest vpunpckhbw vpunpckhdq vpunpckhqdq
syn keyword gasOpcodeX86_AVX vpunpckhwd vpunpcklbw vpunpckldq vpunpcklqdq
syn keyword gasOpcodeX86_AVX vpunpcklwd vpxor vrcpps vrcpss
syn keyword gasOpcodeX86_AVX vroundpd vroundps vroundsd vroundss
syn keyword gasOpcodeX86_AVX vrsqrtps vrsqrtss vshufpd vshufps
syn keyword gasOpcodeX86_AVX vsqrtpd vsqrtps vsqrtsd vsqrtss
syn keyword gasOpcodeX86_AVX vstmxcsr vsubpd vsubps vsubsd
syn keyword gasOpcodeX86_AVX vsubss vtestpd vtestps vucomisd
syn keyword gasOpcodeX86_AVX vucomiss vunpckhpd vunpckhps vunpcklpd
syn keyword gasOpcodeX86_AVX vunpcklps vxorpd vxorps vzeroall
syn keyword gasOpcodeX86_AVX vzeroupper 
" -- Intel Carry-Less Multiplication instructions (CLMUL)
syn keyword gasOpcodeX86_CLMUL pclmulhqhqdq pclmulhqlqdq pclmullqhqdq pclmullqlqdq pclmulqdq

" -- Intel AVX Carry-Less Multiplication instructions (CLMUL)
syn keyword gasOpcodeX86_CLMUL vpclmulhqhqdq vpclmulhqlqdq vpclmullqhqdq
syn keyword gasOpcodeX86_CLMUL vpclmullqlqdq vpclmulqdq

" -- Intel Fused Multiply-Add instructions (FMA)
syn keyword gasOpcodeX86_FMA vfmadd123pd vfmadd123ps vfmadd123sd vfmadd123ss
syn keyword gasOpcodeX86_FMA vfmadd132pd vfmadd132ps vfmadd132sd vfmadd132ss
syn keyword gasOpcodeX86_FMA vfmadd213pd vfmadd213ps vfmadd213sd vfmadd213ss
syn keyword gasOpcodeX86_FMA vfmadd231pd vfmadd231ps vfmadd231sd vfmadd231ss
syn keyword gasOpcodeX86_FMA vfmadd312pd vfmadd312ps vfmadd312sd vfmadd312ss
syn keyword gasOpcodeX86_FMA vfmadd321pd vfmadd321ps vfmadd321sd vfmadd321ss
syn keyword gasOpcodeX86_FMA vfmaddsub123pd vfmaddsub123ps vfmaddsub132pd vfmaddsub132ps
syn keyword gasOpcodeX86_FMA vfmaddsub213pd vfmaddsub213ps vfmaddsub231pd vfmaddsub231ps
syn keyword gasOpcodeX86_FMA vfmaddsub312pd vfmaddsub312ps vfmaddsub321pd vfmaddsub321ps
syn keyword gasOpcodeX86_FMA vfmsub123pd vfmsub123ps vfmsub123sd vfmsub123ss
syn keyword gasOpcodeX86_FMA vfmsub132pd vfmsub132ps vfmsub132sd vfmsub132ss
syn keyword gasOpcodeX86_FMA vfmsub213pd vfmsub213ps vfmsub213sd vfmsub213ss
syn keyword gasOpcodeX86_FMA vfmsub231pd vfmsub231ps vfmsub231sd vfmsub231ss
syn keyword gasOpcodeX86_FMA vfmsub312pd vfmsub312ps vfmsub312sd vfmsub312ss
syn keyword gasOpcodeX86_FMA vfmsub321pd vfmsub321ps vfmsub321sd vfmsub321ss
syn keyword gasOpcodeX86_FMA vfmsubadd123pd vfmsubadd123ps vfmsubadd132pd vfmsubadd132ps
syn keyword gasOpcodeX86_FMA vfmsubadd213pd vfmsubadd213ps vfmsubadd231pd vfmsubadd231ps
syn keyword gasOpcodeX86_FMA vfmsubadd312pd vfmsubadd312ps vfmsubadd321pd vfmsubadd321ps
syn keyword gasOpcodeX86_FMA vfnmadd123pd vfnmadd123ps vfnmadd123sd vfnmadd123ss
syn keyword gasOpcodeX86_FMA vfnmadd132pd vfnmadd132ps vfnmadd132sd vfnmadd132ss
syn keyword gasOpcodeX86_FMA vfnmadd213pd vfnmadd213ps vfnmadd213sd vfnmadd213ss
syn keyword gasOpcodeX86_FMA vfnmadd231pd vfnmadd231ps vfnmadd231sd vfnmadd231ss
syn keyword gasOpcodeX86_FMA vfnmadd312pd vfnmadd312ps vfnmadd312sd vfnmadd312ss
syn keyword gasOpcodeX86_FMA vfnmadd321pd vfnmadd321ps vfnmadd321sd vfnmadd321ss
syn keyword gasOpcodeX86_FMA vfnmsub123pd vfnmsub123ps vfnmsub123sd vfnmsub123ss
syn keyword gasOpcodeX86_FMA vfnmsub132pd vfnmsub132ps vfnmsub132sd vfnmsub132ss
syn keyword gasOpcodeX86_FMA vfnmsub213pd vfnmsub213ps vfnmsub213sd vfnmsub213ss
syn keyword gasOpcodeX86_FMA vfnmsub231pd vfnmsub231ps vfnmsub231sd vfnmsub231ss
syn keyword gasOpcodeX86_FMA vfnmsub312pd vfnmsub312ps vfnmsub312sd vfnmsub312ss
syn keyword gasOpcodeX86_FMA vfnmsub321pd vfnmsub321ps vfnmsub321sd vfnmsub321ss

" -- B.1.30 VIA (Centaur) security instructions
syn keyword gasOpcodeX86_VIA montmul xcryptcbc xcryptcfb xcryptctr
syn keyword gasOpcodeX86_VIA xcryptecb xcryptofb xsha1 xsha256
syn keyword gasOpcodeX86_VIA xstore

" -- AMD XOP, FMA4 and CVT16 instructions (SSE5)
syn keyword gasOpcodeX86_SSE5 vcvtph2ps vcvtps2ph vfmaddpd vfmaddps
syn keyword gasOpcodeX86_SSE5 vfmaddsd vfmaddss vfmaddsubpd vfmaddsubps
syn keyword gasOpcodeX86_SSE5 vfmsubaddpd vfmsubaddps vfmsubpd vfmsubps
syn keyword gasOpcodeX86_SSE5 vfmsubsd vfmsubss vfnmaddpd vfnmaddps
syn keyword gasOpcodeX86_SSE5 vfnmaddsd vfnmaddss vfnmsubpd vfnmsubps
syn keyword gasOpcodeX86_SSE5 vfnmsubsd vfnmsubss vfrczpd vfrczps
syn keyword gasOpcodeX86_SSE5 vfrczsd vfrczss vpcmov vpcomb
syn keyword gasOpcodeX86_SSE5 vpcomd vpcomq vpcomub vpcomud
syn keyword gasOpcodeX86_SSE5 vpcomuq vpcomuw vpcomw vphaddbd
syn keyword gasOpcodeX86_SSE5 vphaddbq vphaddbw vphadddq vphaddubd
syn keyword gasOpcodeX86_SSE5 vphaddubq vphaddubwd vphaddudq vphadduwd
syn keyword gasOpcodeX86_SSE5 vphadduwq vphaddwd vphaddwq vphsubbw
syn keyword gasOpcodeX86_SSE5 vphsubdq vphsubwd vpmacsdd vpmacsdqh
syn keyword gasOpcodeX86_SSE5 vpmacsdql vpmacssdd vpmacssdqh vpmacssdql
syn keyword gasOpcodeX86_SSE5 vpmacsswd vpmacssww vpmacswd vpmacsww
syn keyword gasOpcodeX86_SSE5 vpmadcsswd vpmadcswd vpperm vprotb
syn keyword gasOpcodeX86_SSE5 vprotd vprotq vprotw vpshab
syn keyword gasOpcodeX86_SSE5 vpshad vpshaq vpshaw vpshlb
syn keyword gasOpcodeX86_SSE5 vpshld vpshlq vpshlw

" -- Systematic names for the hinting nop instructions
syn keyword gasOpcodeX86_HNOP hint_nop0 hint_nop1 hint_nop10 hint_nop11
syn keyword gasOpcodeX86_HNOP hint_nop12 hint_nop13 hint_nop14 hint_nop15
syn keyword gasOpcodeX86_HNOP hint_nop16 hint_nop17 hint_nop18 hint_nop19
syn keyword gasOpcodeX86_HNOP hint_nop2 hint_nop20 hint_nop21 hint_nop22
syn keyword gasOpcodeX86_HNOP hint_nop23 hint_nop24 hint_nop25 hint_nop26
syn keyword gasOpcodeX86_HNOP hint_nop27 hint_nop28 hint_nop29 hint_nop3
syn keyword gasOpcodeX86_HNOP hint_nop30 hint_nop31 hint_nop32 hint_nop33
syn keyword gasOpcodeX86_HNOP hint_nop34 hint_nop35 hint_nop36 hint_nop37
syn keyword gasOpcodeX86_HNOP hint_nop38 hint_nop39 hint_nop4 hint_nop40
syn keyword gasOpcodeX86_HNOP hint_nop41 hint_nop42 hint_nop43 hint_nop44
syn keyword gasOpcodeX86_HNOP hint_nop45 hint_nop46 hint_nop47 hint_nop48
syn keyword gasOpcodeX86_HNOP hint_nop49 hint_nop5 hint_nop50 hint_nop51
syn keyword gasOpcodeX86_HNOP hint_nop52 hint_nop53 hint_nop54 hint_nop55
syn keyword gasOpcodeX86_HNOP hint_nop56 hint_nop57 hint_nop58 hint_nop59
syn keyword gasOpcodeX86_HNOP hint_nop6 hint_nop60 hint_nop61 hint_nop62
syn keyword gasOpcodeX86_HNOP hint_nop63 hint_nop7 hint_nop8 hint_nop9

syn cluster gasOpcodesX86 contains=gasOpcodeX86_Base
syn cluster gasOpcodesX86 contains=gasOpcodeX86_3DNOW
syn cluster gasOpcodesX86 contains=gasOpcodeX86_AES
syn cluster gasOpcodesX86 contains=gasOpcodeX86_AVX
syn cluster gasOpcodesX86 contains=gasOpcodeX86_AVX_AES
syn cluster gasOpcodesX86 contains=gasOpcodeX86_CLMUL
syn cluster gasOpcodesX86 contains=gasOpcodeX86_FMA
syn cluster gasOpcodesX86 contains=gasOpcodeX86_HNOP
syn cluster gasOpcodesX86 contains=gasOpcodeX86_MEM
syn cluster gasOpcodesX86 contains=gasOpcodeX86_MMX
syn cluster gasOpcodesX86 contains=gasOpcodeX86_SIMD
syn cluster gasOpcodesX86 contains=gasOpcodeX86_SMX
syn cluster gasOpcodesX86 contains=gasOpcodeX86_SSE
syn cluster gasOpcodesX86 contains=gasOpcodeX86_SSE2
syn cluster gasOpcodesX86 contains=gasOpcodeX86_SSE3
syn cluster gasOpcodesX86 contains=gasOpcodeX86_SSE4A
syn cluster gasOpcodesX86 contains=gasOpcodeX86_SSE4_1
syn cluster gasOpcodesX86 contains=gasOpcodeX86_SSE4_2
syn cluster gasOpcodesX86 contains=gasOpcodeX86_SSE5
syn cluster gasOpcodesX86 contains=gasOpcodeX86_VIA
syn cluster gasOpcodesX86 contains=gasOpcodeX86_VMX

" little kinky but this eases the b/w/l/g suffixes
syn match gasOpcode /^\s*[a-zA-Z0-9_]\+[bwlq]\{0,1\}/			contains=@gasOpcodesX86
syn match gasOpcode /^\s*\(rep\|repn\)?\s\*[a-zA-Z0-9_]\+[bwlq]\{0,1\}/	contains=@gasOpcodesX86

" symbols and labels

syn match   gasSpecial		"[.$]"
syn match   gasLabel		"^\s*[^; \t()]\+:"
syn match   gasSymbol		"[^; \t()]\+"
syn match   gasSymbolRef	"\$[^0-9; \t:()]\+"

" constants
syn region  gasString		start="\""  end="\""
syn match   gasCharacter	"'\(?\|\\?\)"
syn match   gasDecimalNumber	"\$\{0,1\}-\{0,1\}\d\+"
syn match   gasBinaryNumber	"\$\{0,1\}-\{0,1\}0b[01]\+"
syn match   gasOctalNumber	"\$\{0,1\}-\{0,1\}0\d\+"
syn match   gasHexNumber	"\$\{0,1\}-\{0,1\}0x\x\+"
" -- TODO: gasFloatNumber

" local label needs to be matched *after* numerics
syn match   gasLocalLabel	"\d\{1,2\}[:fb]"

" comments etc.
syn match   gasOperator		"[+-/*=|&~<>]\|<=\|>=\|<>"
syn region  gasComment		start=/\/\*/ end=/\*\//

" links
hi def link gasDirectivei386	gasDirective

hi def link gasRegisteri386	gasRegister
hi def link gasRegisteri386Cr	gasRegister
hi def link gasRegisteri386Dr	gasRegister
hi def link gasRegisteri386MMX	gasRegister

" link all opcodes to gasOpcode (can be user-customized later..)
hi def link gasOpcodeX86		gasOpcode
hi def link gasOpcodeX86_Base		gasOpcode
hi def link gasOpcodeX86_3DNOW		gasOpcode
hi def link gasOpcodeX86_AES		gasOpcode
hi def link gasOpcodeX86_AVX		gasOpcode
hi def link gasOpcodeX86_AVX_AES	gasOpcode
hi def link gasOpcodeX86_CLMUL		gasOpcode
hi def link gasOpcodeX86_FMA		gasOpcode
hi def link gasOpcodeX86_HNOP		gasOpcode
hi def link gasOpcodeX86_MEM		gasOpcode
hi def link gasOpcodeX86_MMX		gasOpcode
hi def link gasOpcodeX86_SIMD		gasOpcode
hi def link gasOpcodeX86_SMX		gasOpcode
hi def link gasOpcodeX86_SSE		gasOpcode
hi def link gasOpcodeX86_SSE2		gasOpcode
hi def link gasOpcodeX86_SSE3		gasOpcode
hi def link gasOpcodeX86_SSE4A		gasOpcode
hi def link gasOpcodeX86_SSE4_1		gasOpcode
hi def link gasOpcodeX86_SSE4_2		gasOpcode
hi def link gasOpcodeX86_SSE5		gasOpcode
hi def link gasOpcodeX86_VIA		gasOpcode
hi def link gasOpcodeX86_VMX		gasOpcode

" link to defaults
hi def link gasDirective	preproc
hi def link gasDirectiveStore	type
hi def link gasDirectiveMacro	macro

hi def link gasRegister		identifier

hi def link gasString		string
hi def link gasCharacter	character

hi def link gasBinaryNumber	constant
hi def link gasOctalNumber	constant
hi def link gasHexNumber	constant
hi def link gasDecimalNumber	constant

hi def link gasSymbol		function
hi def link gasSymbolRef	special

hi def link gasSpecial		special
hi def link gasLabel		function
hi def link gasLocalLabel	label

hi def link gasOperator		operator

hi def link gasOpcode		keyword

hi def link gasComment		comment

" finishing touches
let b:current_syntax = "gas"

" vim: ts=8 sw=8 :

