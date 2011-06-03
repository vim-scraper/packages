" Vim syntax file
" Language:	Motorola 68000 Assembler
" Maintainer:	Steve Wall
" Last change:	2001 May 01
"
" This is incomplete.  In particular, support for 68020 and
" up and 68851/68881 co-processors is partial or non-existant.
" Feel free to contribute...
"

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore

" Partial list of register symbols
syn keyword asmHC12Reg	a0 a1 a2 a3 a4 a5 a6 a7 d0 d1 d2 d3 d4 d5 d6 d7
syn keyword asmHC12Reg	pc sr ccr sp usp ssp

" MC68010
syn keyword asmHC12Reg	vbr sfc sfcr dfc dfcr

" MC68020
syn keyword asmHC12Reg	msp isp zpc cacr caar
syn keyword asmHC12Reg	za0 za1 za2 za3 za4 za5 za6 za7
syn keyword asmHC12Reg	zd0 zd1 zd2 zd3 zd4 zd5 zd6 zd7

" MC68030
syn keyword asmHC12Reg	crp srp tc ac0 ac1 acusr tt0 tt1 mmusr

" MC68040
syn keyword asmHC12Reg	dtt0 dtt1 itt0 itt1 urp

" MC68851 registers
syn keyword asmHC12Reg	cal val scc crp srp drp tc ac psr pcsr
syn keyword asmHC12Reg	bac0 bac1 bac2 bac3 bac4 bac5 bac6 bac7
syn keyword asmHC12Reg	bad0 bad1 bad2 bad3 bad4 bad5 bad6 bad7

" MC68881/82 registers
syn keyword asmHC12Reg	fp0 fp1 fp2 fp3 fp4 fp5 fp6 fp7
syn keyword asmHC12Reg	control status iaddr fpcr fpsr fpiar

" M68000 opcodes - order is important!
syntax keyword asmHC12OpCode aba
syntax keyword asmHC12OpCode abx
syntax keyword asmHC12OpCode aby
syntax keyword asmHC12OpCode adca
syntax keyword asmHC12OpCode adcb
syntax keyword asmHC12OpCode adda
syntax keyword asmHC12OpCode addb
syntax keyword asmHC12OpCode addd
syntax keyword asmHC12OpCode anda
syntax keyword asmHC12OpCode andb
syntax keyword asmHC12OpCode andcc
syntax keyword asmHC12OpCode asl
syntax keyword asmHC12OpCode asld
syntax keyword asmHC12OpCode alsa
syntax keyword asmHC12OpCode aslb
syntax keyword asmHC12OpCode asr
syntax keyword asmHC12OpCode asra
syntax keyword asmHC12OpCode asrb
syntax keyword asmHC12OpCode bcc
syntax keyword asmHC12OpCode bclr
syntax keyword asmHC12OpCode bcs
syntax keyword asmHC12OpCode beq
syntax keyword asmHC12OpCode bge
syntax keyword asmHC12OpCode bgnd
syntax keyword asmHC12OpCode bgt
syntax keyword asmHC12OpCode bhi
syntax keyword asmHC12OpCode bhs
syntax keyword asmHC12OpCode bita
syntax keyword asmHC12OpCode bitb
syntax keyword asmHC12OpCode ble
syntax keyword asmHC12OpCode blo
syntax keyword asmHC12OpCode bls
syntax keyword asmHC12OpCode blt
syntax keyword asmHC12OpCode bmi
syntax keyword asmHC12OpCode bne
syntax keyword asmHC12OpCode bpl
syntax keyword asmHC12OpCode bra
syntax keyword asmHC12OpCode brclr
syntax keyword asmHC12OpCode brn
syntax keyword asmHC12OpCode brset
syntax keyword asmHC12OpCode bset
syntax keyword asmHC12OpCode bsr
syntax keyword asmHC12OpCode bvc
syntax keyword asmHC12OpCode bvs
syntax keyword asmHC12OpCode call
syntax keyword asmHC12OpCode cba
syntax keyword asmHC12OpCode clc
syntax keyword asmHC12OpCode cli
syntax keyword asmHC12OpCode clr
syntax keyword asmHC12OpCode clra
syntax keyword asmHC12OpCode clrb
syntax keyword asmHC12OpCode clv
syntax keyword asmHC12OpCode cmpa
syntax keyword asmHC12OpCode cmpb
syntax keyword asmHC12OpCode com
syntax keyword asmHC12OpCode cpd
syntax keyword asmHC12OpCode cps
syntax keyword asmHC12OpCode cpx
syntax keyword asmHC12OpCode cpy
syntax keyword asmHC12OpCode daa
syntax keyword asmHC12OpCode dbeq
syntax keyword asmHC12OpCode dbne
syntax keyword asmHC12OpCode dec
syntax keyword asmHC12OpCode deca
syntax keyword asmHC12OpCode decb
syntax keyword asmHC12OpCode des
syntax keyword asmHC12OpCode dex
syntax keyword asmHC12OpCode dey
syntax keyword asmHC12OpCode ediv
syntax keyword asmHC12OpCode edivs
syntax keyword asmHC12OpCode emacs
syntax keyword asmHC12OpCode emaxd
syntax keyword asmHC12OpCode emaxm
syntax keyword asmHC12OpCode emind
syntax keyword asmHC12OpCode emul
syntax keyword asmHC12OpCode emuls
syntax keyword asmHC12OpCode eora
syntax keyword asmHC12OpCode eorb
syntax keyword asmHC12OpCode etbl
syntax keyword asmHC12OpCode exg
syntax keyword asmHC12OpCode fdiv
syntax keyword asmHC12OpCode ibeq
syntax keyword asmHC12OpCode ubne
syntax keyword asmHC12OpCode idiv
syntax keyword asmHC12OpCode idivs
syntax keyword asmHC12OpCode inc
syntax keyword asmHC12OpCode inca
syntax keyword asmHC12OpCode incb
syntax keyword asmHC12OpCode ins
syntax keyword asmHC12OpCode inx
syntax keyword asmHC12OpCode iny
syntax keyword asmHC12OpCode jmp
syntax keyword asmHC12OpCode jsr
syntax keyword asmHC12OpCode lbcc
syntax keyword asmHC12OpCode lbcs
syntax keyword asmHC12OpCode lbeq
syntax keyword asmHC12OpCode lbge
syntax keyword asmHC12OpCode lbgt
syntax keyword asmHC12OpCode lbhi
syntax keyword asmHC12OpCode lbhs
syntax keyword asmHC12OpCode lble
syntax keyword asmHC12OpCode lblo
syntax keyword asmHC12OpCode lbls
syntax keyword asmHC12OpCode lblt
syntax keyword asmHC12OpCode lbmi
syntax keyword asmHC12OpCode lbne
syntax keyword asmHC12OpCode lbpl
syntax keyword asmHC12OpCode lbra
syntax keyword asmHC12OpCode lbrn
syntax keyword asmHC12OpCode lbvc
syntax keyword asmHC12OpCode lbvs
syntax keyword asmHC12OpCode ldaa
syntax keyword asmHC12OpCode ldab
syntax keyword asmHC12OpCode ldd
syntax keyword asmHC12OpCode lds
syntax keyword asmHC12OpCode ldx
syntax keyword asmHC12OpCode ldy
syntax keyword asmHC12OpCode leas
syntax keyword asmHC12OpCode leax
syntax keyword asmHC12OpCode leay
syntax keyword asmHC12OpCode lsl
syntax keyword asmHC12OpCode lsla
syntax keyword asmHC12OpCode lslb
syntax keyword asmHC12OpCode lsld
syntax keyword asmHC12OpCode lsr
syntax keyword asmHC12OpCode lsra
syntax keyword asmHC12OpCode lsrb
syntax keyword asmHC12OpCode lsrd
syntax keyword asmHC12OpCode maxa
syntax keyword asmHC12OpCode maxb
syntax keyword asmHC12OpCode maxm
syntax keyword asmHC12OpCode mem
syntax keyword asmHC12OpCode mina
syntax keyword asmHC12OpCode minm
syntax keyword asmHC12OpCode movb
syntax keyword asmHC12OpCode movw
syntax keyword asmHC12OpCode mul
syntax keyword asmHC12OpCode neg
syntax keyword asmHC12OpCode nega
syntax keyword asmHC12OpCode negb
syntax keyword asmHC12OpCode nop
syntax keyword asmHC12OpCode oraa
syntax keyword asmHC12OpCode orab
syntax keyword asmHC12OpCode orcc
syntax keyword asmHC12OpCode psha
syntax keyword asmHC12OpCode pshb
syntax keyword asmHC12OpCode pshc
syntax keyword asmHC12OpCode pshd
syntax keyword asmHC12OpCode pshx
syntax keyword asmHC12OpCode pshy
syntax keyword asmHC12OpCode pula
syntax keyword asmHC12OpCode pulb
syntax keyword asmHC12OpCode pulc
syntax keyword asmHC12OpCode puld
syntax keyword asmHC12OpCode pulx
syntax keyword asmHC12OpCode puly
syntax keyword asmHC12OpCode rev
syntax keyword asmHC12OpCode revw
syntax keyword asmHC12OpCode rol
syntax keyword asmHC12OpCode rola
syntax keyword asmHC12OpCode rolb
syntax keyword asmHC12OpCode ror
syntax keyword asmHC12OpCode rora
syntax keyword asmHC12OpCode rorb
syntax keyword asmHC12OpCode rtc
syntax keyword asmHC12OpCode rti
syntax keyword asmHC12OpCode rts
syntax keyword asmHC12OpCode sba
syntax keyword asmHC12OpCode sbca
syntax keyword asmHC12OpCode sbcb
syntax keyword asmHC12OpCode sec
syntax keyword asmHC12OpCode sei
syntax keyword asmHC12OpCode sev
syntax keyword asmHC12OpCode sex
syntax keyword asmHC12OpCode staa
syntax keyword asmHC12OpCode stab
syntax keyword asmHC12OpCode std
syntax keyword asmHC12OpCode stop
syntax keyword asmHC12OpCode sts
syntax keyword asmHC12OpCode stx
syntax keyword asmHC12OpCode sty
syntax keyword asmHC12OpCode suba
syntax keyword asmHC12OpCode subb
syntax keyword asmHC12OpCode subd
syntax keyword asmHC12OpCode swi
syntax keyword asmHC12OpCode tab
syntax keyword asmHC12OpCode tap
syntax keyword asmHC12OpCode tba
syntax keyword asmHC12OpCode tbeq
syntax keyword asmHC12OpCode tbl
syntax keyword asmHC12OpCode tbne
syntax keyword asmHC12OpCode tfr
syntax keyword asmHC12OpCode tpa
syntax keyword asmHC12OpCode trap
syntax keyword asmHC12OpCode tst
syntax keyword asmHC12OpCode tsta
syntax keyword asmHC12OpCode tstb
syntax keyword asmHC12OpCode tsx
syntax keyword asmHC12OpCode tsy
syntax keyword asmHC12OpCode txs
syntax keyword asmHC12OpCode tys
syntax keyword asmHC12OpCode wau
syntax keyword asmHC12OpCode wav
syntax keyword asmHC12OpCode wavr
syntax keyword asmHC12OpCode xgdx
syntax keyword asmHC12OpCode xgdy

" Valid labels
syn match asmHC12Label		"^[a-z_?.][a-z0-9_?.$]*$"
syn match asmHC12Label		"^[a-z_?.][a-z0-9_?.$]*\s"he=e-1
syn match asmHC12Label		"^\s*[a-z_?.][a-z0-9_?.$]*:"he=e-1

" Various number formats
syn match hexNumber		"\$[0-9a-fA-F]\+\>"
syn match hexNumber		"\<[0-9][0-9a-fA-F]*H\>"
syn match octNumber		"@[0-7]\+\>"
syn match octNumber		"\<[0-7]\+[QO]\>"
syn match binNumber		"%[01]\+\>"
syn match binNumber		"\<[01]\+B\>"
syn match decNumber		"\<[0-9]\+D\=\>"
syn match floatE		"_*E_*" contained
syn match floatExponent		"_*E_*[-+]\=[0-9]\+" contained contains=floatE
syn match floatNumber		"[-+]\=[0-9]\+_*E_*[-+]\=[0-9]\+" contains=floatExponent
syn match floatNumber		"[-+]\=[0-9]\+\.[0-9]\+\(E[-+]\=[0-9]\+\)\=" contains=floatExponent
syn match floatNumber		":\([0-9a-f]\+_*\)\+"

" Character string constants
syn match asmHC12StringError	"'[ -~]*'"
syn match asmHC12StringError	"'[ -~]*$"
syn region asmHC12String		start="'" skip="''" end="'" oneline contains=asmHC12CharError
syn match asmHC12CharError	"[^ -~]" contained

" Immediate data
syn match asmHC12Immediate	"#\$[0-9a-fA-F]\+" contains=hexNumber
syn match asmHC12Immediate	"#[0-9][0-9a-fA-F]*H" contains=hexNumber
syn match asmHC12Immediate	"#@[0-7]\+" contains=octNumber
syn match asmHC12Immediate	"#[0-7]\+[QO]" contains=octNumber
syn match asmHC12Immediate	"#%[01]\+" contains=binNumber
syn match asmHC12Immediate	"#[01]\+B" contains=binNumber
syn match asmHC12Immediate	"#[0-9]\+D\=" contains=decNumber
syn match asmHC12Symbol		"[a-z_?.][a-z0-9_?.$]*" contained
syn match asmHC12Immediate	"#[a-z_?.][a-z0-9_?.]*" contains=asmHC12Symbol

" Special items for comments
syn keyword asmHC12Todo		contained TODO

" Operators
syn match asmHC12Operator	"[-+*/]"	" Must occur before Comments

" Comments
syn match asmHC12Comment		";.*" contains=asmHC12Todo
syn match asmHC12Comment		"\s!.*"ms=s+1 contains=asmHC12Todo
syn match asmHC12Comment		"^\s*[*!].*" contains=asmHC12Todo
" Include
syn match asmHC12Include		"\<INCLUDE\s"

" Standard macros
" Conditional assembly
syn match asmHC12PreCond		"\<IFC\s"
syn match asmHC12PreCond		"\<IFDEF\s"
syn match asmHC12PreCond		"\<IFEQ\s"
syn match asmHC12PreCond		"\<IFGE\s"
syn match asmHC12PreCond		"\<IFGT\s"
syn match asmHC12PreCond		"\<IFLE\s"
syn match asmHC12PreCond		"\<IFLT\s"
syn match asmHC12PreCond		"\<IFNC\>"
syn match asmHC12PreCond		"\<IFNDEF\s"
syn match asmHC12PreCond		"\<IFNE\s"
syn match asmHC12PreCond		"\<ELSEC\>"
syn match asmHC12PreCond		"\<ENDC\>"

" Loop control
syn match asmHC12PreCond		"\<REPT\s"
syn match asmHC12PreCond		"\<IRP\s"
syn match asmHC12PreCond		"\<IRPC\s"
syn match asmHC12PreCond		"\<ENDR\>"

" Directives
syn keyword asmHC12Directive bsz
syn keyword asmHC12Directive db
syn keyword asmHC12Directive dc.b
syn keyword asmHC12Directive dc.w
syn keyword asmHC12Directive ds
syn keyword asmHC12Directive ds.b
syn keyword asmHC12Directive ds.w
syn keyword asmHC12Directive dw
syn keyword asmHC12Directive end
syn keyword asmHC12Directive equ
syn keyword asmHC12Directive fcb
syn keyword asmHC12Directive fcc
syn keyword asmHC12Directive fdb
syn keyword asmHC12Directive fill
syn keyword asmHC12Directive loc
syn keyword asmHC12Directive nam
syn keyword asmHC12Directive name
syn keyword asmHC12Directive opt
syn keyword asmHC12Directive org
syn keyword asmHC12Directive pag
syn keyword asmHC12Directive page
syn keyword asmHC12Directive redef
syn keyword asmHC12Directive rmb
syn keyword asmHC12Directive rmw
syn keyword asmHC12Directive spc
syn keyword asmHC12Directive ttl
syn keyword asmHC12Directive zmb 
syn case match

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_asmHC12_syntax_inits")
  if version < 508
    let did_asmHC12_syntax_inits = 1
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

  HiLink asmHC12Comment		Comment
  HiLink asmHC12Todo		Todo

  HiLink hexNumber		Number		" Constant
  HiLink octNumber		Number		" Constant
  HiLink binNumber		Number		" Constant
  HiLink decNumber		Number		" Constant
  HiLink floatNumber		Number		" Constant
  HiLink floatExponent		Number		" Constant
  HiLink floatE			SpecialChar	" Statement
  "HiLink floatE		Number		" Constant

  HiLink asmHC12Immediate	SpecialChar	" Statement
  "HiLink asmHC12Symbol		Constant

  HiLink asmHC12String		String		" Constant
  HiLink asmHC12CharError	Error
  HiLink asmHC12StringError	Error

  HiLink asmHC12Reg		Identifier
  HiLink asmHC12Operator		Identifier

  HiLink asmHC12Include		Include		" PreProc
  HiLink asmHC12Macro		Macro		" PreProc
  HiLink asmHC12MacroParam	Keyword		" Statement

  HiLink asmHC12Directive	Special
  HiLink asmHC12PreCond		Special

  HiLink asmHC12Cond		Conditional	" Statement
  HiLink asmHC12Repeat		Repeat		" Statement

  HiLink asmHC12Label           Type
  HiLink asmHC12OpCode          Statement
    
  delcommand HiLink
endif

let b:current_syntax = "asmHC12"

" vim: ts=8 sw=2
