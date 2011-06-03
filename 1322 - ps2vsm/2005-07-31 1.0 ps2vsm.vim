" Vim syntax file
" Language:	PlayStation2 Vector Unit Assembly
" Maintainer:   James Lee <jbit@jbit.net>
" Last Change:	2005 July 31

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore

" NOPs can be on both units
syn keyword vuNop NOP

" Vector unit upper instructions
syn keyword vuUpper ABS CLIP OPMULA OPMULB
syn keyword vuUpper FTOI0 FTOI4 FTOI12 FTOI15 ITOF0 ITOF4 ITOF12 ITOF15
syn keyword vuUpper MAX  MAXi  MAXx  MAXy  MAXz  MAXw
syn keyword vuUpper MINI MINIi MINIx MINIy MINIz MINIw

"           FMAC;   Norm. Imm.   Q-Reg  BcastX BcastY BcastZ BcastW
syn keyword vuUpper ADD   ADDi   ADDq   ADDx   ADDy   ADDz   ADDw
syn keyword vuUpper ADDA  ADDAi  ADDAq  ADDAx  ADDAy  ADDAz  ADDAw
syn keyword vuUpper MADD  MADDi  MADDq  MADDx  MADDy  MADDz  MADDw
syn keyword vuUpper MADDA MADDAi MADDAq MADDAx MADDAy MADDAz MADDAw
syn keyword vuUpper MSUB  MSUBi  MSUBq  MSUBx  MSUBy  MSUBz  MSUBw
syn keyword vuUpper MSUBA MSUBAi MSUBAq MSUBAx MSUBAy MSUBAz MSUBAw
syn keyword vuUpper MUL   MULi   MULq   MULx   MULy   MULz   MULw
syn keyword vuUpper MULA  MULAi  MULAq  MULAx  MULAy  MULAz  MULAw
syn keyword vuUpper SUB   SUBi   SUBq   SUBx   SUBy   SUBz   SUBw
syn keyword vuUpper SUBA  SUBAi  SUBAq  SUBAx  SUBAy  SUBAz  SUBAw


" Vector unit upper instructions
syn keyword vuLower B BAL DIV EATAN EATANxy EATANxz EEXP ELENG
syn keyword vuLower ERCPR ERLENG ERSADD ERSQRT ESADD ESIN ESQRT ESUM
syn keyword vuLower FCAND FCEQ FCGET FCOR FCSET FMAND FMEQ FMORE FSAND FSEQ FSOR FSSET
syn keyword vuLower IADD IADDI IADDIU IAND IBEQ IBGEZ IBGTZ IBLEZ IBLTZ IBNE
syn keyword vuLower ILW ILWR IOR ISUB ISUBIU ISW ISWR JALR JR LQ LQD LQI
syn keyword vuLower MFIR MFP MOVE MR32 MTIR RGET RINIT RNEXT RSQRT RXOR
syn keyword vuLower SQ SQD SQI SQRT WAITP WAITQ XGKICK XITOP XTOP


" Stuff
syn match   vuIdentifier "[a-z_][a-z0-9_]*"
syn match   vuLabel      "[a-z_][a-z0-9_]*:"

" Bit setting
syn match   vuBits       "\[i\]"
syn match   vuBits       "\[e\]"
syn match   vuBits       "\[m\]"
syn match   vuBits       "\[d\]"
syn match   vuBits       "\[t\]"

" PreProcessor stuff [Probably want more stuff]
syn match vuPreProc     "\.org"
syn match vuPreProc     "\.p2align"
syn match vuPreProc     "\.vu"
syn match vuPreProc     "\.globl"
syn match vuPreProc     "\.ascii"
syn match vuPreProc     "\.asciz"

" Comments
syn region vuComment start=";"   end="$"
syn region vuComment start="/\*" end="\*/"
syn region vuComment start="//"  end="$"

" Strings
syn match vuString   "\".*\""
syn match vuString   "\'.*\'"

" Numbers
syn match vuNumber   "[0-9]\+"
syn match vuNumber   "0[xX][0-9a-fA-F]\+"
syn match vuNumber   "0[xX][0-9a-fA-F]\+"

" Destination
syn match vuDest     "\.[xyzw]"
syn match vuDest     "\.[xyzw][xyzw]"
syn match vuDest     "\.[xyzw][xyzw][xyzw]"
syn match vuDest     "\.[xyzw][xyzw][xyzw][xyzw]"


" Registers
syn match vuFloatReg     "vf[0-9]"
syn match vuFloatReg     "vf[0-9][0-9]"
syn match vuFloatReg     "vf[0-9][w-z]"
syn match vuFloatReg     "vf[0-9][0-9][w-z]"
syn match vuIntReg       "vi[0-9]"
syn match vuIntReg       "vi[0-9][0-9]"
syn match vuSpecialReg   "ACC[w-z]"
syn keyword vuSpecialReg ACC I Q R P

" Define the colours
" I know that it's evil to hardcore the colours, 
" but it's the only way I could get it to look nice :)
" The bright background version is a bit hacky, I just
" did it at the last minute so it's actually readable!

if &background == "dark"
	hi vuNop         guifg=#FF7f7f
	hi vuUpper       guifg=#FFFF00 gui=bold
	hi vuLower       guifg=#00FF00 gui=bold
	hi vuIntReg      guifg=#00ffaa gui=bold
	hi vuFloatReg    guifg=#ffffaa gui=bold
	hi vuSpecialReg  guifg=#00aaff gui=bold
	hi vuBits        guifg=#ff0000 gui=bold
	hi vuDest        guifg=#ff88ff
else
	hi vuNop         guifg=#FF7f7f
	hi vuUpper       guifg=#FF8800 gui=bold
	hi vuLower       guifg=#008800 gui=bold
	hi vuIntReg      guifg=#0088aa gui=bold
	hi vuFloatReg    guifg=#AA8800 gui=bold
	hi vuSpecialReg  guifg=#00aaff gui=bold
	hi vuBits        guifg=#ff0000 gui=bold
	hi vuDest        guifg=#ff88ff
endif

hi def link vuLabel      Label
hi def link vuComment    Comment
hi def link vuNumber     Number
hi def link vuString     String
hi def link vuPreProc    PreProc
hi def link vuIdentifier Macro


let b:current_syntax = "ps2vsm"
" vim: ts=4
