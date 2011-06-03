" Vim syntax file
" Language:	Atmel AVR Assembler
" By:	Jan Nárovec <finn@sendmail.cz>
" Last Change:	2003 Nov 15

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore

syn match avrAsmIdentifier	"[a-zA-Z][0-9a-zA-Z_]*"

"syn match decNumber		"0\+[1-7]\=[\t\n$,; ]"
syn match decNumber		"0\+\d*"
syn match decNumber		"[1-9]\d*"
syn match octNumber		"0[0-7][0-7]\+"
syn match hexNumber		"\$[0-9a-fA-F]\+"
syn match binNumber		"0[bB][0-1]*"

syn region avrAsmString		start=/"/ end=/"/

syn match avrAsmSpecialComment	";\*\*\*.*"
syn match avrAsmComment		";.*"hs=s+1
syn match avrAsmLab		"[a-zA-Z_][0-9a-zA-Z_]*:"he=e-1

syn match avrAsmInclude		"\.include"
syn match avrAsmEqu		"\.equ"
syn match avrAsmSet		"\.set"
syn match avrAsmType		"\.byte"
syn match avrAsmType		"\.db"
syn match avrAsmType		"\.dw"
syn match avrAsmDef		"\.def"
syn match avrAsmMacro		"\.macro"
syn match avrAsmMacro		"\.endmacro"
syn match avrAsmOrg		"\.org\s[0-9]\+"
syn match avrAsmSeg		"\.cseg"
syn match avrAsmSeg		"\.dseg"
syn match avrAsmSeg		"\.eseg"
syn match avrAsmAsm		"\.device"
syn match avrAsmAsm		"\.exit"
syn match avrAsmAsm		"\.list"
syn match avrAsmAsm		"\.nolist"
syn match avrAsmAsm		"\.listmac"

syn keyword avrAsmReg		 r0  r1  r2  r3  r4  r5  r6  r7
syn keyword avrAsmReg		 r8  r9 r10 r11 r12 r13 r14 r15
syn keyword avrAsmReg		r16 r17 r18 r19 r20 r21 r22 r23
syn keyword avrAsmReg		r24 r25 r26 r27 r28 r29 r30 r31

syn keyword avrAsmOpcode	mov movw ldi ld ldd lds st std sts spm lpm elpm in out push pop
syn keyword avrAsmOpcode	sbi cbi lsl lsr ror rol asr swap bset bclr bst bld sec clc sen cln sez clz sei cli ses cls sev clv set clt seh clh
syn keyword avrAsmOpcode	jmp rjmp ijmp eijmp brbs brbc breq brne brcs brcc brsh brlo brmi brpl brge brlt brhs brhc brts brtc brvs brvc brie brid sbrc sbrs sbic sbis
syn keyword avrAsmOpcode	call rcall icall eicall ret reti
syn keyword avrAsmOpcode	and andi or ori eor com sbr cbr tst clr ser
syn keyword avrAsmOpcode	add adc adiw sub subi subiw sbc sbci inc dec neg
syn keyword avrAsmOpcode	mul muls mulsu fmul fmuls fmulsu
syn keyword avrAsmOpcode	cp cpc cpi cpse
syn keyword avrAsmOpcode	nop sleep break wdr

syn keyword avrAsmIOReg		ubrrh ubrrl ubrr ucr ucsrb ucsra udr usr
syn keyword avrAsmIOReg		adcl adch adcsr admux acsr
syn keyword avrAsmIOReg		spcr spsr spdr
syn keyword avrAsmIOReg		pina pinb pinc pind
syn keyword avrAsmIOReg		ddra ddrb ddrc ddrd
syn keyword avrAsmIOReg		porta portb portc portd
syn keyword avrAsmIOReg		eecr eedr eear eearl eearh 
syn keyword avrAsmIOReg		wdtcr
syn keyword avrAsmIOReg		assr ocr2 tccr2 tcnt2
syn keyword avrAsmIOReg		icr1l icr1h ocr1bl ocr1bh ocr1al ocr1ah tcnt1l tcnt1h tccr1b tccr1a
syn keyword avrAsmIOReg		tcnt0 tccr0
syn keyword avrAsmIOReg		mcusr mcucr
syn keyword avrAsmIOReg		tifr timsk gifr gimsk
syn keyword avrAsmIOReg		sp sph spl sreg

" syn keyword avrAsmOperator	! ~ + - * / >> << < <= > >= == != & ^ | && ||

syn case match

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_asm_syntax_inits")
  if version < 508
    let did_asm_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  " The default methods for highlighting.  Can be overridden later
  HiLink avrAsmSeg		Special
  HiLink avrAsmOrg		Special
  HiLink avrAsmComment		Comment
  HiLink avrAsmAsm		PreCondit
  HiLink avrAsmInclude		Include
  HiLink avrAsmEqu		Typedef
  HiLink avrAsmSet		Typedef
  HiLink avrAsmDef		Typedef
  HiLink avrAsmType		Type
  HiLink avrAsmOpcode		Statement
  HiLink avrAsmOperator		Operator

  HiLink avrAsmInclude		Directive
  HiLink avrAsmList		Directive
  HiLink avrAsmMacro		Macro
  
  HiLink avrAsmString		String

  HiLink hexNumber		Number
  HiLink decNumber		Number
  HiLink octNumber		Number
  HiLink binNumber		Number

  HiLink avrAsmIdentifier	Normal

  HiLink avrAsmReg		Identifier
  HiLink avrAsmIOReg		Identifier
  HiLink avrAsmLab		ModeMsg

  delcommand HiLink
endif

let b:current_syntax = "avrasm"

" vim: ts=8
