" Vim syntax file
" Language:     MIPS specific assembly syntax (MAL)
" Maintainer:	Syed Ali <alisyed@sfsu.edu>
" Last Change:	2003 March 28

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

"syn case ignore


"MIPS directives

syn match asmDirective "\.text"
syn match asmDirective "\.data"



syn match asmType "\.align"
syn match asmType "\.ascii"
syn match asmType "\.asciiz"
syn match asmType "\.globl"
syn match asmType "\.set"
syn match asmType "\.space"
syn match asmType "\.word"

" constants
syn match asmConstant "[- ][0-9]*"hs=s+1
syn match asmConstant "[- ]0x[0-9a-zA-Z]*"hs=s+1
syn match asmConstant "\".*\""


syn match asmKeyword "$0"
syn match asmKeyword "$31"
syn match asmError "$1"


"MIPS registers


syn match asmReg "$a0"
syn match asmReg "$a1"
syn match asmReg "$a2"
syn match asmReg "$a3"
syn match asmReg "$at"
syn match asmReg "$fp"
syn match asmReg "$gp"
syn match asmReg "$k0"
syn match asmReg "$k1"
syn match asmReg "$ra"
syn match asmReg "$s0"
syn match asmReg "$s1"
syn match asmReg "$s2"
syn match asmReg "$s3"
syn match asmReg "$s4"
syn match asmReg "$s5"
syn match asmReg "$s6"
syn match asmReg "$s7"
syn match asmReg "$s8"
syn match asmReg "$sp"
syn match asmReg "$t0"
syn match asmReg "$t1"
syn match asmReg "$t2"
syn match asmReg "$t3"
syn match asmReg "$t4"
syn match asmReg "$t5"
syn match asmReg "$t6"
syn match asmReg "$t7"
syn match asmReg "$t8"
syn match asmReg "$t9"
syn match asmReg "$v0"
syn match asmReg "$v1"
syn match asmReg "$zero"

"MIPS opcodes
                                            
syn match asmOpcode " abs "
syn match asmOpcode " add "
syn match asmOpcode " addi "
syn match asmOpcode " addiu "
syn match asmOpcode " addu "
syn match asmOpcode " and "
syn match asmOpcode " andi "
syn match asmOpcode " b "
syn match asmOpcode " bczt "
syn match asmOpcode " bczf "
syn match asmOpcode " beq "
syn match asmOpcode " beqz "
syn match asmOpcode " bge "
syn match asmOpcode " bgez "
syn match asmOpcode " bgeu "
syn match asmOpcode " bgezal "
syn match asmOpcode " bgt "
syn match asmOpcode " bgtu "
syn match asmOpcode " bgtz "
syn match asmOpcode " ble "
syn match asmOpcode " bleu "
syn match asmOpcode " blez "
syn match asmOpcode " blt "
syn match asmOpcode " bltu "
syn match asmOpcode " bltz "
syn match asmOpcode " bltzal "
syn match asmOpcode " bne "
syn match asmOpcode " bnez "
syn match asmOpcode " bqez "
syn match asmOpcode " div "
syn match asmOpcode " divu "
syn match asmOpcode " eret "
syn match asmOpcode " j "
syn match asmOpcode " jal "
syn match asmOpcode " jr "
syn match asmOpcode " la "
syn match asmOpcode " lb "
syn match asmOpcode " li "
syn match asmOpcode " lui "
syn match asmOpcode " lw "
syn match asmOpcode " mfc0 "
syn match asmOpcode " move "
syn match asmOpcode " mul "
syn match asmOpcode " mulo "
syn match asmOpcode " mulou "
syn match asmOpcode " mult "
syn match asmOpcode " mtc0 "
syn match asmOpcode " neg "
syn match asmOpcode " negu "
syn match asmOpcode " nomove "
syn match asmOpcode " nor "
syn match asmOpcode " not "
syn match asmOpcode " or "
syn match asmOpcode " ori "
syn match asmOpcode " rem "
syn match asmOpcode " remu "
syn match asmOpcode " rol "
syn match asmOpcode " ror "
syn match asmOpcode " sb "
syn match asmOpcode " seq "
syn match asmOpcode " sge "
syn match asmOpcode " sgeu "
syn match asmOpcode " sgt "
syn match asmOpcode " sgtu "
syn match asmOpcode " sle "
syn match asmOpcode " sleu "
syn match asmOpcode " sne "
syn match asmOpcode " sll "
syn match asmOpcode " sllv "
syn match asmOpcode " slt "
syn match asmOpcode " slti "
syn match asmOpcode " sltu "
syn match asmOpcode " sltiu "
syn match asmOpcode " sra "
syn match asmOpcode " srav "
syn match asmOpcode " srl "
syn match asmOpcode " srlv "
syn match asmOpcode " sub "
syn match asmOpcode " subu "
syn match asmOpcode " sw "
syn match asmOpcode " syscall "
syn match asmOpcode " xor "
syn match asmOpcode " xori "

syn match asmComment "\#.*"
syn match asmLabel		"[a-zA-Z_][a-zA-Z0-9_]*:"he=e-1

syn case match




" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_mips_syntax_inits")
  if version < 508
    let did_mips_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink asmOpcode  Statement
  HiLink asmReg  Identifier
  HiLink asmLabel	Label
  HiLink asmComment	Comment
  HiLink asmDirective	Statement
  HiLink asmType	Type
  HiLink asmConstant	Constant
  HiLink asmKeyword	Keyword
  HiLink asmError	Error


  " My default-color overrides:
  "hi asmOpcode guifg=red
  "hi asmReg	guifg=yellow guibg=lightgrey

  delcommand HiLink
endif

let b:current_syntax = "asmMIPS"

set ts=8 sw=8 ai smartindent
