" Vim syntax file for SPARC Assembler
"
" Version : 0.2
" 
" Currently supported : V8
" 
" Please note that there is still a little problem : I cannot get the '.' and
" the ',a' sequences highlighted.
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


syntax keyword sparcMnemonic flush ldsb ldsh ldstub ldub lduh ld ldd ldsba ldsha lduba lduha lda ldda ldstuba  nop rd sethi sll srl sra stb sth st std stba stha sta stda  swap swapa unimp wr btst bset bclr btog clr clrb clrh cmp dec deccc inc inccc mov set skipz skipnz tst ldn ldna stn stna setn setnhi casn slln srln sran clrn

syntax keyword sparcMnemonicLogical  and andcc andn andcc or orcc orn orncc xnor xnorcc xor xorcc negnot 

syntax keyword sparcMnemonicArithmetic add addcc addx addxcc mulscc sdiv sdivcc smul smulcc udiv udivcc umul umulcc taddcc tsubcc taddcctv tsubcctv sub subcc subx subxcc

syntax keyword sparcMnemonicBranch bn bne be bg ble bge bl bgu bleu bcc bcs bpos bneg bvc bvs ba call jmpl save restore rett jmp fbn fbu fbg fbug fbl fblg fbne fbe fbue fbge fbuge fble fbule fbo fba cbn cb3 cb2 cb23 cb1 cb13 cb12 cb123 cb0 cb03 cb02 cb023 cb01 cb013 cb012 cba

syntax keyword sparcMnemonicFPI fitos fitod fitoq fstoi fdtoi fstod fstoq fdtos fdtoq fqtod fqtos fmovs fnegs fabss fsqrts fsqrtd fsqrtq fadds faddd faddq fsubs fsubd fsubq fmuls fmuld fmulq fsmulq fdivs fdivd fdivq fcmps fcmpd fcmpq fcmpes fcmped fcmpeq

syntax keyword sparcMnemonicTrap tn tne te tg tle tge tl tgu tleu tlu tgeu tpos tneg tvc tvs ta

syntax keyword sparcMnemonicCopro cpop1 cpop2

syntax keyword sparcAsmDirective alias align ascii asciz byte common double empty file global half ident local noalias nonvolatile nword optim popsection proc pushsection quad reserve section seg single size skip stabn stabs type uahalf uaword version volatile weak word xword xstabs

if !exists("did_sparc_syntax_inits")
   	let did_sparc_syntax_inits=1
	highlight link sparcComment Comment
	highlight link sparcLabel Exception
	highlight link sparcString String
	highlight link sparcNumber Number

	highlight link sparcAsmDirective Preproc

	highlight link sparcMnemonic Keyword
	highlight link sparcMnemonicLogical Operator
	highlight link sparcMnemonicArithmetic Statement
	highlight link sparcMnemonicBranch Exception
	highlight link sparcMnemonicTrap Keyword
	highlight link sparcMnemonicFPI Keyword
	highlight link sparcMnemonicCopro Keyword
	highlight link sparcMnemonicSyn Conditional

	highlight link sparcRegister Preproc
endif
let b:current_syntax="sparc"



