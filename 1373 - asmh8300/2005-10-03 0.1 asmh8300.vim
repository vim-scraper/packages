" Vim syntax file
" Language:	Hitachi H8S, H8/300 assemble language syntax 
" Maintainer:	Kevin Dahlhausen <kdahlhaus@yahoo.com>
" Maintainer:	Eric Liao <joke_tw@yahoo.com>
" Last Change:	2005 Sep 11

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore

syn match asmDirective "\.h8300[h]*"

"h8300[h] registers
syn match asmReg	"\<e\=[Rr][0-7][LlHh]\="
syn match asmReg	"\<[Ee][0-7]\>"
syn match asmReg	"\<CCR\>"
syn match asmReg	"\<SP\>"

"h8300[h] opcodes - order is important!
syn match asmOpcode "\<add\.[lbw]\>\|\<add\>"
syn match asmOpcode "\<add[sx :]\>"
syn match asmOpcode "\<and\.[lbw]\>\|\<and\>"
syn match asmOpcode "\<bl[deots]\>"
syn match asmOpcode "\<cmp\.[lbw]\>\|\<cmp\>"
syn match asmOpcode "\<dec\.[lbw]\>\|\<dec\>"
syn match asmOpcode "\<divx[us]\.[bw]\>\|\<divx[us]\>"
syn match asmOpcode "\<ext[su]\.[lw]\>\|\<ext[su]\>"
syn match asmOpcode "\<inc\.[lbw]\>\|\<inc\>"
syn match asmOpcode "\<mov\.[lbw]\>\|\<mov\>"
syn match asmOpcode "\<mulx[su]\.[bw]\>\|\<mulx[su]\>"
syn match asmOpcode "\<neg\.[lbw]\>\|\<neg\>"
syn match asmOpcode "\<not\.[lbw]\>\|\<not\>"
syn match asmOpcode "\<or\.[lbw]\>\|\<or\>"
syn match asmOpcode "\<pop\.[wl]\>\|\<pop\>"
syn match asmOpcode "\<push\.[wl]\>\|\<push\>"
syn match asmOpcode "\<rotx\=[lr]\.[lbw]\>\|\<rotx\=[lr]\>"
syn match asmOpcode "\<sha[lr]\.[lbw]\>\|\<sha[lr]\>"
syn match asmOpcode "\<shl[lr]\.[lbw]\>\|\<shl[lr]\>"
syn match asmOpcode "\<sub\.[lbw]\>\|\<sub\>"
syn match asmOpcode "\<xor\.[lbw]\>\|\<xor\>"

" Special char
syn match asmSpecialChar	"@\|#"

syn keyword asmOpcode andc band bclr  
syn keyword asmOpcode biand bild bior bist bixor bmi
syn keyword asmOpcode bnot bor bset
syn keyword asmOpcode bsr btst bst bt bvc bvs bxor daa
syn keyword asmOpcode das eepmov eepmovw ldc movfpe
syn keyword asmOpcode movtpe nop orc rte sleep stc subx
syn keyword asmOpcode trapa xorc
" Branch Opcode
syn keyword asmOpcodeBH bra bt brn bf bhi bls bcc bhs bcs blo bne
syn keyword asmOpcodeBH beq bvc bvs bpl bmi bge blt bgt ble 
syn keyword asmOpcodeBH jmp bsr jsr rts rte

syn keyword asmDirective eq

syn match h8_hexNumber		"[hH]'[0-9a-fA-F]\+"
syn match h8_binNumber		"[bB]'[0-1]*"

syn case match


" Read the general asm syntax
if version < 600
  source <sfile>:p:h/asm.vim
else
  runtime! syntax/asm.vim
endif


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_hitachi_syntax_inits")
  if version < 508
    let did_hitachi_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink asmOpcode  	SpecialKey
  HiLink asmReg		Special
  HiLink h8_binNumber	Number 
  HiLink h8_hexNumber	Number 
  HiLink asmOpcodeBH 	Label

  " My default-color overrides:
  hi asmOpcode 		ctermfg=yellow guifg=lightyellow
  hi asmReg		ctermfg=red guifg=lightmagenta
  hi asmSpecialChar	ctermfg=red guifg=magenta 
  delcommand HiLink
endif

let b:current_syntax = "asmh8300"

" vim: ts=8
