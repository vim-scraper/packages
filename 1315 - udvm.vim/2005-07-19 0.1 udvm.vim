" Vim syntax file
" Language: UDVM Assembler
" Author: constantin@fokus.fraunhofer.de
" Version: 0.1
" $Id: udvm.vim,v 1.1.1.1 2005/07/19 10:35:36 cco Exp $


exec "set iskeyword=a-z,A-Z,48-57,_,-"
syn case ignore

syn keyword udvmInstruction AND OR NOT LSHIFT RSHIFT ADD SUBTRACT MULTIPLY DIVIDE REMAINDER SORT-ASCENDING SORT-DESCENDING SHA-1 LOAD MULTILOAD PUSH POP COPY COPY-LITERAL COPY-OFFSET MEMSET JUMP CALL RETURN SWITCH CRC DECOMPRESSION-FAILURE INPUT-BYTES INPUT-BITS INPUT-HUFFMAN STATE-ACCESS STATE-CREATE STATE-FREE OUTPUT END-MESSAGE COMPARE
syn keyword udvmDirective ALIGN AT PAD BYTE WORD SET RO RW


syn match udvmNumber display "\<\d\+\>"
"hex number
syn match udvmHexNumber display "\<0x\x\+\>"
syn match udvmLabel display "\k\+:"he=e-1
syn match udvmLabel display ":\k\+"hs=s+1

syn region udvmComment  start=";" end=".*$"
syn region udvmComment  start="/\*" end="\*/"

hi link udvmDirective PreProc
hi link udvmInstruction Statement
hi link udvmLabel Statement
hi link udvmNumber Constant
hi link udvmHexNumber Constant
hi link udvmComment Comment
