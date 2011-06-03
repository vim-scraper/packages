" Vim syntax file
" Language:     KP1878BE1 (Tesei) Assembler (Angstrem's microcontroller)
" Maintainer:   Kirill Frolov <fk0@fk0.pp.ru>
" Last Change:  2003 Feb 15
" URL:          http://fk0.pp.ru/tesei.vim
" Revision:     0.1

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case match
syn keyword teseiTodo NOTE TODO FIXME XXX contained

syn case ignore

syn match teseiIdentifier "[a-z_][a-z0-9_]*"
syn match teseiSpecIdent  "$[0-9]\+"
syn match teseiSpecIdent  "@[0-9]\+"

syn match teseiLabel      "^[A-Z_][A-Z0-9_]*:"me=e-1
syn match teseiSpecLabel  "^$[0-9]\+:"me=e-1
syn match teseiSpecLabel  "^@[0-9]\+:"me=e-1

syn match teseiASCII      "'.'"
syn match teseiNumber     "[0-9]\+"
syn match teseiNumber     "[0-9][0-9a-fA-F]*[hH]"
syn match teseiNumber     "[0-1]\+[bB]\>"
syn match teseiNumber     "[0-7]\+[oO]"
syn region teseiString    start=+"+ end=+"+

syn match teseiComment    ";.*" contains=teseiTodo

syn match teseiSpecReg    "#[0-7a-d]"
syn match teseiRegister   "%\{0,1\}[a-d][0-7]"

syn match teseiSpecIdent  "\."
syn match teseiDirective  "\.\s*="

syn match teseiConst	  "[a-zA-Z_][a-zA-z0-9_]*\s*="

" src -> dst
syn keyword teseiOpcode   mov cmp add sub and or xor
" literal -> dst
syn keyword teseiOpcode   movl cmpl addl subl
" lit. bits -> dst
syn keyword teseiOpcode   bicl bisl btgl bttl bich bish btgh btth
" dst only
syn keyword teseiOpcode   swap neg not shl shr shra rlc rrc adc sbc
" arifmetic macro
syn keyword teseiOpcode   inc dec clr
" load address macro
syn keyword teseiLoadAddr mial miah mdal mdah
syn keyword teseiLoadAddr ldal ldah lial liah
" special registers and stack
syn keyword teseiLoadAddr ldr mfpr mtpr push pop
" status register
syn keyword teseiOpcode   sst cst stc stz stn clc clz cln tof tdc
syn keyword teseiSpecOp   stie clie
" branches
syn keyword teseiBranch   jmp jsr jnz jz jne jeq jns js jnc jc jae jb
syn keyword teseiBranch   ijmp ijsr rts rtsc
" macro
syn keyword teseiLoop     loop
" other
syn keyword teseiSpecOp   rti nop wait sksp RST reset slp

syn match teseiDirective "\.byte"
syn match teseiDirective "\.word"
syn match teseiDirective "\.even"
syn match teseiMacro     "\.macro"
syn match teseiMacro     "\.endm\>"
syn match teseiPreCondit "\.if"
syn match teseiPreCondit "\.else"
syn match teseiPreCondit "\.endif"
syn match teseiInclude   "#include"
syn match teseiDefine    "#define"
syn match teseiPreProc   "\.bpt"
syn match teseiPreProc   "\.list"
syn match teseiPreProc   "\.end\>"
syn match teseiPreProc   "\.endc"


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_tesei_syntax_inits")
  if version < 508
    let did_tesei_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink teseiTodo               Todo
  HiLink teseiComment            Comment

  HiLink teseiLabel              Label
  HiLink teseiSpecLabel          Repeat
  HiLink teseiString             String
  HiLink teseiASCII              Character
  HiLink teseiNumber             Number

  HiLink teseiConst              Constant

  HiLink teseiOpcode             Statement
  HiLink teseiBranch             Conditional
  HiLink teseiLoop               Repeat
  HiLink teseiLoadAddr           Operator
  HiLink teseiSpecOp             Exception
  
  HiLink teseiRegister           Type
  HiLink teseiSpecReg            Structure

  HiLink teseiSpecIdent          Function
  HiLink teseiIdentifier         Identifier

  HiLink teseiDirective          Keyword
  HiLink teseiPreProc            PreProc
  HiLink teseiPreCondit          PreCondit
  HiLink teseiInclude            Include
  HiLink teseiMacro              Macro
  HiLink teseiDefine             Define

  delcommand HiLink
endif

let b:current_syntax = "tesei"

" vim: ts=8
