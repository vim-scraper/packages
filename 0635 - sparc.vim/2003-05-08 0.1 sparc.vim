" Vim syntax file for SPARC Assembler
" Currently supported : 
" SPARC V8, no co-processor
"
" This a very early version : still no co-pro mnemonics and no annulled
" branching mnemonics (ba,a and the like).
"
" Maintainer : Nicolas Herry <k@madrognon.net>


" Quit if syntax file already loaded and vim > 6.00
if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

" Case insensitive
syntax case ignore

syntax match sparcComment /!.*/
syntax match sparcLabel /[A-Z0-9_\-]\+:/
syntax match sparcRegister /%[oilrg][0-9]/
syntax match sparcRegister /%fp/
syntax match sparcRegister /%sp/

syntax region sparcString start=/"/ skip=/\\"/ end=/"/
syntax match sparcNumber /[0-9]\+/


syntax keyword sparcAsmDirective section text data rodata bss align global ascii asciiz

syntax keyword sparcMnemonic ldsb ldsh ldub lduh ld ldd stb sth st std sll srl sra sethi nop
syntax keyword sparcMnemonicLogical and andcc andn andncc or orcc orn orncc xor xorcc xnor xnorcc
syntax keyword sparcMnemonicArithmetic add addcc addx addxcc sub subcc subx subxcc umul umulcc smul smulcc udiv udivcc sdiv sdivcc
syntax keyword sparcMnemonicBranch ba bn bne be bg bge bl ble bleu bcc bcs bpos bneg bvc bvs fba fbn fbg fbl fbne fbe fbge fble
syntax keyword sparcMnemonicSub save restore call jumpl
syntax keyword sparcMnemonicFPI fmovs fnegs fabss fsqrts fsqrtd fsqrtq fadds faddd faddq fsubs fsubd fsubq fmuls fmuld fmulq fsmuld fdmulq fdivs fdivd fdivq fcmps fcmpd fcmpq
syntax keyword sparcMnemonicCon fitos fitod fitoq fstoi fdtoi fqtoi fstod fstoq fdtos fdtoq fqtos fqtod
syntax keyword sparcMnemonicSyn cmp jmp call tst ret set not neg inc inccc dec deccc btst bset bclr btog clr clrb clrh mov


if !exists("did_sparc_syntax_inits")
   	let did_sparc_syntax_inits=1
	highlight link sparcComment comment
	highlight link sparcLabel Exception
	highlight link sparcString string
	highlight link sparcNumber number

	highlight link sparcAsmDirective preproc

	highlight link sparcMnemonic Keyword
	highlight link sparcMnemonicLogical operator
	highlight link sparcMnemonicArithmetic Statement
	highlight link sparcMnemonicBranch Exception
	highlight link sparcMnemonicSub keyword
	highlight link sparcMnemonicFPI keyword
	highlight link sparcMnemonicCon keyword
	highlight link sparcMnemonicSyn Conditional

	highlight link sparcRegister preproc
endif
let b:current_syntax="sparc"


