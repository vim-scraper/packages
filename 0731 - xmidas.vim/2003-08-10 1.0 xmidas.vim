" Vim syntax file
" Language:                   X-Midas
" Original Author:            Lance Luvaul
" Last Change:                2002 Jul

" Remove any old syntax stuff hanging around
syntax clear

" X-Midas is not case sensitive
syntax case ignore


" I N T R I N S I C S

" TODO: exploit fact that any line may have only one of these
" and that is at the beginning (would prevent "env file open
" somefile" from having the first two words interpreted as
" intrinsics)
syntax keyword      xmidasMiscIntrin    abs[cissa] ask att[ach] aux[iliary]
syntax keyword      xmidasMiscIntrin    bee[p] bitm[ask] break
syntax keyword      xmidasMiscIntrin    call comm[ent] conc[atenate] cont[inue]
syntax keyword      xmidasMiscIntrin    control deb[ug] deca[tenate] def[aults]
syntax keyword      xmidasMiscIntrin    deta[ch] dsm else
syntax keyword      xmidasMiscIntrin    endif endl[oop] endm[ode] ends[uspend]
syntax keyword      xmidasMiscIntrin    endw[hile] erase erro[r]
syntax keyword      xmidasMiscIntrin    exi[t] expl[ain] expor[t] filel[ist]
syntax keyword      xmidasMiscIntrin    fin[d] fna[me] go
syntax keyword      xmidasMiscIntrin    hea[dermod] home[path]
syntax keyword      xmidasMiscIntrin    imp[ort] info jump key[word]
syntax keyword      xmidasMiscIntrin    loop mfi[le]
syntax keyword      xmidasMiscIntrin    msg[mask] os par[se] pause
syntax keyword      xmidasMiscIntrin    pro[tect] readid rec[all] rem[ove]
syntax keyword      xmidasMiscIntrin    reset res[ults] return rsm
syntax keyword      xmidasMiscIntrin    sav[e] say skip
syntax keyword      xmidasMiscIntrin    st[atus] step swi[tch] sy[ntax]
syntax keyword      xmidasMiscIntrin    timec[ode] tim[er] tim[ex] trap
syntax keyword      xmidasMiscIntrin    typeof unpr[otect] user[id] val[ue]
syntax keyword      xmidasMiscIntrin    ver[ify] warn[ing] widget
syntax keyword      xmidasMiscIntrin    writeid xwi[ndow]
syntax keyword      xmidasFileIntrin    fil[es]


" K E Y W O R D S   ( I N T R I N S I C   A L I A S E S )

" TODO: are there short versions of these?
" TODO: is callp short for callprocedure?
syntax keyword      xmidasKeyword       local include endmacro startcontrols
syntax keyword      xmidasKeyword       endcontrols subroutine endsubroutine
syntax keyword      xmidasKeyword       procedure callp startmacro stop

" Uncomment below if you don't like tabs
"syntax match       xmidasTab           "\t"


" C O M M E N T S

syntax match        xmidasComment       excludenl "!.*"


" L I N E   C O N T I N U A T I O N S

" I like to highlight what gets removed from the command buffer.
"syntax region      xmidasLineCont      start="&\s*\(!.*\)\=$" end="^\s*[^       !]"me=e-1 contains=xmidasComment
syntax region       xmidasLineCont      start="&\s*\(!.*\)\=$" skip="^\s*\(!.*\)\=$" end="^\s*" contains=xmidasComment
syntax region       xmidasStringCont    contained start="&\s*$" skip="^\s*\(!.*\)\=$" end="^\s*" contains=xmidasComment


" I D E N T I F I E R S

"syntax match       xmidasIdent         "\<[a-z_][a-z0-9_]*\>"
"syntax region      xmidasSpanIdent     start="\<[a-z_]\+&\s*\(!.*\)\=$" skip="&\s*\(!.*\)\=\|^\s*\(!.*\)\=$" end="^\s*\([a-z0-9_]\+\)\=" contains=xmidasLineCont keepend
"syntax region      xmidasSpanIdent     start="\<[a-z_]\+&\s*\(!.*\)\=$" skip="^\s*\(!.*\)\=$" end="^\s*\([a-z0-9_]\+\)\=" contains=xmidasLineCont keepend
"syntax region      xmidasSpanIdent     start="\<[a-z_]\+&\s*\(!.*\)\=$" skip="^\s*[a-z0-9_]\+&\s*\(!.*\)\=$\|^\s*\(!.*\)\=$" end="^\s*\([a-z0-9_]\+\)\=" contains=xmidasLineCont keepend

" in development
"syntax region      xmidasIdent         start="\<\w" end="$" end="\W"me=e-1,he=e-1 contains=xmidasResExp,xmidasLineCont
syntax region       xmidasIdent         start="\<\w" skip="\w\^{\|\w\=&\s*\(!.*\)\=$\|^\s*\(!.*\)\=$" end="$" end="\W"me=e-1,he=e-1 contains=xmidasResExp,xmidasLineCont


" O S   C O M M A N D S

"syntax match       xmidasOScom         "^\s*\$.*\(!.*\)\=$" contains=xmidasComment,xmidasString
"syntax region      xmidasSpanOScom     start="^\s*\$.*&\s*\(!.*\)\=$" skip="&\s*\(!.*\)\=\|^\s*\(!.*\)\=$" end="\s*$"he=s-1,me=s-1 contains=xmidasLineCont,xmidasSpanString,xmidasComment,xmidasString
"syntax region      xmidasSpanOScom     start="^\s*\$.*&\s*\(!.*\)\=$" skip="^\s*\(!.*\)\=$" end="\s*$"he=s-1,me=s-1 contains=xmidasLineCont,xmidasSpanString,xmidasComment,xmidasString
syntax region       xmidasOScom         start="^\s*\$"hs=e,ms=e skip="&\s*\(!.*\)\=$\|^\s*\(!.*\)\=$" end="\s*$"he=s-1,me=s-1 contains=xmidasLineCont,xmidasSpanString,xmidasComment,xmidasString,xmidasResExp,xmidasSpanResExp


" S T R I N G S

syntax match        xmidasString        excludenl /"[^"]*$/ contains=xmidasResExp
" The last end= prevents highlighting trailing whitespace when there is no end quote.
syntax region       xmidasSpanString    start=/".*&\s*$/ skip=/&\s*$\|^\s*\(!.*\)\=$/ end=/"/ excludenl end=/\s*$/he=s-1,me=s-1 contains=xmidasStringCont,xmidasResExp,xmidasSpanResExp
syntax match        xmidasString        /".\{-}"/ contains=xmidasResExp


" F I L E   Q U A L I F I E R S

syntax match        xmidasFile          "\<[a-z_][a-z0-9_]*\(([^)]\{-}\(([^)]\{-}\(([^(]\{-})[^)]\{-}\)*)[^)]\{-}\)*[^(]\{-})\)\+" contains=xmidasFileQualList
syntax match        xmidasFileQualList  contained "\(([^)]\{-}\(([^)]\{-}\(([^(]\{-})[^)]\{-}\)*)[^)]\{-}\)*[^(]\{-})\)\+" contains=xmidasFileHCBBG,xmidasFileMiscBG,xmidasFileTrim,xmidasFileVect
" [HELP->FEATURES->TRIMMING; HELP->QUALIFIERS]
"syntax match       xmidasFileTrim      contained "(\~\=+\=\d\+[km]\=:+\=\~\=\d\+[km]\=)"
syntax match        xmidasFileTrim      contained "(.\{-}:.\{-})"
" [HELP->FEATURES->VECTORED]
syntax match        xmidasFileVect      contained "([^)]\{-}\(([^)]\{-}\(([^(]\{-})[^)]\{-}\)*)[^)]\{-}\)*[^(]\{-}\(;[^)]\{-}\(([^)]\{-}\(([^(]\{-})[^)]\{-}\)*)[^)]\{-}\)*[^(]\{-}\(;[^)]\{-}\(([^)]\{-}\(([^(]\{-})[^)]\{-}\)*)[^)]\{-}\)*[^(]\{-}\)\=\)\=)" contains=xmidasFileVectVal
" [EXPLAIN->STAT; HELP->FEATURES->VECTORED]
syntax match        xmidasFileHCBBG     contained "(hcb\..\{-})" contains=xmidasFileHCB
syntax match        xmidasFileHCB       contained "(hcb\.\(sz\|scope\|[ft]\|[fdp]s\|[xy][usd]\))"
syntax match        xmidasFileHCB       contained "(hcb\.\(tc\|rep\|rl\|nsr\|nc\|fr\|\(in\|out\)b\))"
syntax match        xmidasFileHCB       contained "(hcb\.key\..\{-})" contains=xmidasFileHCBVal
" [HELP->QIOLIBRARY]
syntax match        xmidasFileMiscBG    contained "(\(io\|f[ct]\|[fp]s\|async\|prot\|aux\|det\|rep\|od\|ctg\|ksc\|pm\|cl\)\>.\{-})" contains=xmidasFileMisc
syntax match        xmidasFileMisc      contained "(io=\(rms\|qio\|cache\))"
" [HELP->SEARCH->M$TEST_FORMAT; HELP->QUALIFIERS]
"syntax match       xmidasFileMisc      contained "(fc=\([csvm#][aflibdoip#*]\|nh\))"
" [HELP->UTILITIES->M$BPA; HELP->QUALIFIERS]
syntax match        xmidasFileMisc      contained "(fc=[scvqmtux1-9][bilfdapo])"
syntax match        xmidasFileMisc      contained "(ft=[bilfdapo])"
" [HELP->QUALIFIERS]
syntax match        xmidasFileMisc      contained "(\(async\|prot\)=[01])"
syntax match        xmidasFileMisc      contained "(\(aux\|det\)=[^)]\{-1,})" contains=xmidasFileMiscVal
syntax match        xmidasFileMisc      contained "(\(aux\|det\)=\d\+)"
syntax match        xmidasFileMisc      contained "(rep=[^)]\{-1,})" contains=xmidasFileMiscVal
syntax match        xmidasFileMisc      contained "(rep=\(vax\|ieee\|eeei\|cray\))"
syntax match        xmidasFileMisc      contained "(od=[^)]\{-1,})"
syntax match        xmidasFileMisc      contained "(ctg=1)"
syntax match        xmidasFileMisc      contained "([fp]s=[^)]\{-1,})" contains=xmidasFileMiscVal
syntax match        xmidasFileMisc      contained "([fp]s=\d\+)"
syntax match        xmidasFileMisc      contained "(ksc=[^)]\{-1,})" contains=xmidasFileMiscVal
syntax match        xmidasFileMisc      contained "(pm=[^)]\{-1,})" contains=xmidasFileMiscVal
syntax match        xmidasFileMisc      contained "(pm=[01])"
syntax match        xmidasFileMisc      contained "(cl=[^)]\{-1,})" contains=xmidasFileMiscVal
syntax match        xmidasFileMisc      contained "(cl=\(-[123]\|\d\+\))"
syntax match        xmidasFileHCBVal    contained "(hcb\.key\..\{-})"ms=s+9,hs=s+9,me=e-1,he=e-1 contains=xmidasResExp,xmidasIdent,xmidasNumber,xmidasSpanResExp
syntax match        xmidasFileMiscVal   contained "=.\{-})"ms=s+1,hs=s+1,me=e-1,he=e-1 contains=xmidasResExp,xmidasIdent,xmidasNumber,xmidasSpanResExp
syntax match        xmidasFileVectVal   contained "([^)]\{-}\(([^)]\{-}\(([^(]\{-})[^)]\{-}\)*)[^)]\{-}\)*[^(]\{-})"ms=s+1,hs=s+1,me=e-1,he=e-1 contains=xmidasResExp,xmidasNumber,xmidasIdent,xmidasFile,xmidasGlobOper,xmidasCalcIL,xmidasFileVectO,xmidasStruct
syntax match        xmidasFileVectO     contained ";"
syntax match        xmidasFileVectO     contained "\~[a-z_][a-z0-9_]*"


" P U N C T U A T I O N

" [HELP->FEATURES->METACHARACTERS]
syntax match        xmidasPunct         contained "\.\|,\|\^\|{\|}\|:\|(\|)\|;\|\~\|="


" T Y P E S

" [EXPLAIN->RESULTS; HELP->UTILITIES->M$TIME_FORMAT]
syntax match        xmidasType          "\<\([audflibon_]\|t[aentvx_]\):" contains=xmidasPunct


" L A B E L S

syntax region       xmidasLabIdent      contained start="\<\w" skip="[a-z0-9_]\=\^{" end="\w\>" contains=xmidasResExp
syntax match        xmidasLabIntrin     "\<\(label\|goto\)\>"
syntax region       xmidasLabCmd        start="\<\(label\|goto\)\>" skip="&\s*\(!.*\)\=$\|^\s*\(!.*\)\=$" end="$" contains=xmidasComment,xmidasLabIdent,xmidasSpanLabIdent,xmidasLabIntrin keepend
syntax region       xmidasSpanLabIdent  contained start="\<[a-z0-9_]\+&\s*\(!.*\)\=$" skip="^\s*[a-z0-9_]\+&\s*\(!.*\)\=$\|^\s*\(!.*\)\=$" end="^\s*\([a-z0-9_]\+\)\=" contains=xmidasLineCont keepend


" O P E R A T O R S

" [HELP->FEATURES->CALCULATIONS]
" free for use anywhere in a macro (uncontained)
syntax match        xmidasGlobOper      "\*\*\=\|/\|+\|-\|(\|)"
" [HELP->AOP->M$AOP; HELP->FEATURES->CALCULATIONS]
" only for CALCULATOR intrinsic and inline CALC()
syntax match        xmidasCalcIntrin    "\<calc\(u\(l\(a\(t\(o\(r\)\=\)\=\)\=\)\=\)\=\)\=\>"
syntax region       xmidasCalcCmd       start="\<calc\(u\(l\(a\(t\(o\(r\)\=\)\=\)\=\)\=\)\=\)\=\>" skip="&\s*\(!.*\)\=$\|^\s*\(!.*\)\=$" end="$" contains=ALLBUT,xmidasLogicOper,xmidasFileQualList,xmidasSeditFunc,xmidasEnvILVar,xmidasPunct,xmidasFileVectO,xmidasXcallFunc,xmidasLabIdent,xmidasSpanLabIdent,xmidasMiscIntrin
syntax match        xmidasCalcIL        "\<calc([^)]\{-}\(([^)]\{-}\(([^(]\{-})[^)]\{-}\)*)[^)]\{-}\)*[^(]\{-})" contains=xmidasCalcIL,xmidasCalcILBG
syntax match        xmidasCalcILBG      contained transparent "([^)]\{-}\(([^)]\{-}\(([^(]\{-})[^)]\{-}\)*)[^)]\{-}\)*[^(]\{-})"hs=s+1,ms=s+1,he=e-1,me=e-1 contains=ALLBUT,xmidasLogicOper,xmidasFileQualList,xmidasSeditFunc,xmidasEnvILVar,xmidasXcallFunc,xmidasSpanLabIdent,xmidasLabIdent,xmidasPunct
syntax match        xmidasCalcOper      contained "\<\(max\|min\)\(imum\)\=\>"
syntax match        xmidasCalcOper      contained "\<mod\(ulo\)\=\>"
syntax match        xmidasCalcOper      contained "\<a\=\(cos\|sin\|tan\)d\=\>"
syntax match        xmidasCalcOper      contained "\<\(eqv\|dot\|abs\|sqrt\|exp\|log\|ln\)\>"
syntax match        xmidasCalcOper      contained "\<\(fix\|sums\=\|norm\|i\=fft\|ent\|mov\)\>"
syntax match        xmidasCalcOper      contained "\<\(npow\|atan2d\=\|power2l\=\|round\|random\)\>"
syntax match        xmidasCalcOper      contained "\<b\=\(x\=or\|and\)\>"
syntax match        xmidasCalcOper      contained "\<\(v\=mux\|mag2\=\|phased\=\)\>"
syntax match        xmidasCalcOper      contained "\<\(polar\|rect\)\>"
syntax match        xmidasCalcOper      contained "<>\|\*+\|\~\*\="
syntax match        xmidasCalcOper      contained "\<\([gl][te]\|eq\|ne\|bnot\)\>"
syntax region       xmidasTriOper       contained oneline matchgroup=xmidasCalcOper start="?" end=":"
" [EXPLAIN->IF; HELP->FEATURES->CALCULATIONS]
" only for IF/ELSEIF and WHILE intrinsics and inline LOGICAL()
syntax match        xmidasLogIL         "\<logical(.*)" contains=xmidasLogILBG
syntax match        xmidasLogILBG       contained transparent "(.*)"hs=s+1,ms=s+1,he=e-1,me=e-1 contains=ALLBUT,xmidasCalcOper,xmidasFileQualList,xmidasSeditFunc,xmidasEnvILVar,xmidasMessFunc,xmidasXcallFunc,xmidasSpanLabIdent,xmidasLabIdent,xmidasPunct
syntax match        xmidasMiscIntrin    "\<\(else\)\=if\>"
syntax match        xmidasKeyword       "\<then\>"
syntax region       xmidasIfCmd         start="^\s*\(else\)\=if\>" skip="&\s*\(!.*\)\=$\|^\s*\(!.*\)\=$" end="\<then\>" end="$" contains=ALLBUT,xmidasCalcOper,xmidasFileQualList,xmidasSeditFunc,xmidasEnvILVar,xmidasMessFunc,xmidasPunct,xmidasFileVectO,xmidasLabIdent,xmidasSpanLabIdent keepend
syntax match        xmidasMiscIntrin    "\<while\>"
syntax region       xmidasWhileCmd      start="^\s*while\>" skip="&\s*\(!.*\)\=$\|^\s*\(!.*\)\=$" end="$" contains=ALLBUT,xmidasCalcOper,xmidasFileQualList,xmidasSeditFunc,xmidasEnvILVar,xmidasMessFunc,xmidasPunct,xmidasFileVectO,xmidasLabIdent,xmidasSpanLabIdent
syntax match        xmidasLogicOper     contained "\<n\=[gl][te]\>"
syntax match        xmidasLogicOper     contained "\<n\=\(eq\|ne\|eqt\)\>"
syntax match        xmidasLogicOper     contained "\<n\=a\(ny\|ll\)bits\>"
syntax match        xmidasLogicOper     contained "\<n\=\(eqs\|eqss\|subs\)\>"
syntax match        xmidasLogicOper     contained "\<n\=[fr]exists\=\>"
syntax match        xmidasLogicOper     contained "\<\(and\|or\)\>"


" N U M B E R   C O N S T A N T S

" hex digit constant
syntax match        xmidasNumber        "\<0x[0-9a-f]\+\>"
" integer
syntax match        xmidasNumber        "\<\d\+[km]\=\>"
" floating point number, with dot, optional exponent
syntax match        xmidasNumber        "\<\d\+\.\d*\([ed][-+]\=\d\+\)\=[km]\=\>"
" floating point number, starting with dot, optional exponent
syntax match        xmidasNumber        "\.\(\d\+\)\=\([ed][-+]\=\d\+\)\=[km]\=\>"
" floating point number, without dot, optional exponent
syntax match        xmidasNumber        "\<\d\+[ed][-+]\=\d\+[km]\=\>"


" N E S T E D   C O M M A N D   S Y N T A X

" [EXPLAIN->MESSAGE]
" only for MESSAGE intrinsic
syntax match        xmidasMiscIntrin    "\<mes\(s\(a\(g\(e\)\=\)\=\)\=\)\=\>"
syntax region       xmidasMessCmd       start="\<mes\(s\(a\(g\(e\)\=\)\=\)\=\)\=\>" skip="&\s*\(!.*\)\=$\|^\s*\(!.*\)\=$" end="$" contains=ALLBUT,xmidasCalcOper,xmidasFileQualList,xmidasSeditFunc,xmidasEnvILVar,xmidasLogicOper,xmidasPunct,xmidasFileVectO,xmidasLabIdent,xmidasSpanLabIdent,xmidasXcallFunc
syntax match        xmidasMessFunc      contained "\<\(send[msw]*\|get[clmnprsw]*\|clr\|init\|up\|down\|query\)\>"
" [EXPLAIN->SEDIT]
" only for SEDIT intrinsic
syntax match        xmidasMiscIntrin    "\<sedit\>"
syntax region       xmidasSeditCmd      start="\<sedit\>" skip="&\s*\(!.*\)\=$\|^\s*\(!.*\)\=$" end="$" contains=ALLBUT,xmidasCalcOper,xmidasFileQualList,xmidasMessFunc,xmidasEnvILVar,xmidasLogicOper,xmidasPunct,xmidasFileVectO,xmidasLabIdent,xmidasSpanLabIdent
syntax keyword      xmidasSeditFunc     contained subs[titute] gsub[stitute] upca[se] loca[se] trim
syntax keyword      xmidasSeditFunc     contained stri[m] word clean pars[e] elem[ent] sear[ch] bsea[rch]
syntax keyword      xmidasSeditFunc     contained rang[e] betw[een] len[gth] nfor[mat] head tail root ext
" [EXPLAIN->ENVIRONMENT]
" only for ENVIRONMENT intrinsic and inline ENV()
syntax match        xmidasMiscIntrin    "\<env\(i\(r\(o\(n\(m\(e\(n\(t\)\=\)\=\)\=\)\=\)\=\)\=\)\=\)\=\>"
syntax region       xmidasEnvCmd        start="\<env\(i\(r\(o\(n\(m\(e\(n\(t\)\=\)\=\)\=\)\=\)\=\)\=\)\=\)\=\>" skip="&\s*\(!.*\)\=$\|^\s*\(!.*\)\=$" end="$" contains=ALLBUT,xmidasCalcOper,xmidasFileQualList,xmidasXcallFunc,xmidasSeditFunc,xmidasEnvILVar,xmidasLogicOper,xmidasPunct,xmidasFileVectO,xmidasLabIdent,xmidasSpanLabIdent,xmidasFileIntrin
syntax match        xmidasEnvFunc       contained "\<\([gs]et\|list\|reset\|file\)\>"
syntax match        xmidasEnvIL         "\<env([a-z_][a-z0-9_]*)" contains=xmidasEnvILBG
syntax match        xmidasEnvILBG       contained "([a-z_][a-z0-9_]*)"hs=s+1,ms=s+1,he=e-1,me=e-1 contains=xmidasEnvILVar
syntax match        xmidasEnvILVar      contained "\<\(osty\|osna\|mach\|host\|home\)[a-z0-9_]*\>"
syntax match        xmidasEnvILVar      contained "\<\(logi\|read\|writ\|user\|waux\)[a-z0-9_]*\>"
syntax match        xmidasEnvILVar      contained "\<\(raux\|nrau\|seed\|tolr\|veri\)[a-z0-9_]*\>"
syntax match        xmidasEnvILVar      contained "\<\(debu\|beep\|brea\|unlo\|paus\)[a-z0-9_]*\>"
syntax match        xmidasEnvILVar      contained "\<\(mode\|coun\|stat\|mnam\|spaw\)[a-z0-9_]*\>"
syntax match        xmidasEnvILVar      contained "\<\(plot\|plo2\|left\|heig\|widt\)[a-z0-9_]*\>"
syntax match        xmidasEnvILVar      contained "\<\(visc\|visd\|visi\|nshf\|uexe\)[a-z0-9_]*\>"
syntax match        xmidasEnvILVar      contained "\<\(dsm\|top\)\>"
" [HELP->XCALL]
" only for XCALL intrinsic
syntax match        xmidasMiscIntrin    "\<xca\(l\(l\)\=\)\=\>"
syntax region       xmidasXcallCmd      start="\<xca\(l\(l\)\=\)\=\>" skip="&\s*\(!.*\)\=$\|^\s*\(!.*\)\=$" end="$" contains=ALLBUT,xmidasCalcOper,xmidasFileQualList,xmidasSeditFunc,xmidasEnvILVar,xmidasLogicOper,xmidasPunct,xmidasFileVectO,xmidasLabIdent,xmidasSpanLabIdent,xmidasMessCmd
syntax keyword      xmidasXcallFunc     contained curs[or] clea[rpanel] draw[panel] file menu mess[age]
syntax keyword      xmidasXcallFunc     contained even[tflush] send setc[ontrol] setg[roup] setp[anel]
syntax keyword      xmidasXcallFunc     contained wget[id] widg[et] paus[e] run quer[y] lear[n] cpdu[mp]
syntax keyword      xmidasXcallFunc     contained prom[pt] moni[tor] cplo[ad]
syntax match        xmidasXcallFunc     contained "\<[adfl]mon\(i\(t\(o\(r\)\=\)\=\)\=\)\=\>"
syntax match        xmidasXcallFunc     contained "\<[audfl]pro\(m\(p\(t\)\=\)\=\)\=\>"
syntax match        xmidasXcallFunc     contained "\<[dfl]val\>"
" [HELP->XCONTROL]
" only for XCONTROL intrinsic
syntax match        xmidasMiscIntrin    "\<xcon\(t\(r\(o\(l\)\=\)\=\)\=\)\=\>"
syntax region       xmidasXconCmd       start="\<xcon\(t\(r\(o\(l\)\=\)\=\)\=\)\=\>" skip="&\s*\(!.*\)\=$\|^\s*\(!.*\)\=$" end="$" contains=ALLBUT,xmidasCalcOper,xmidasFileQualList,xmidasSeditFunc,xmidasEnvILVar,xmidasLogicOper,xmidasPunct,xmidasFileVectO,xmidasXcallFunc,xmidasLabIdent,xmidasSpanLabIdent
syntax keyword      xmidasXconFunc      contained menu fram[e] prom[pt] moni[tor] msmo[nitor]
syntax match        xmidasXconFunc      contained "\<[adflt]mon\(i\(t\(o\(r\)\=\)\=\)\=\)\=\>"
syntax match        xmidasXconFunc      contained "\<[audfl]pro\(m\(p\(t\)\=\)\=\)\=\>"
syntax match        xmidasXconFunc      contained "\<[dfl]val\>"
" [EXPLAIN->{CONTROL|WIDGET}]
" only for {CONTROL|WIDGET} intrinsic
syntax match        xmidasMiscIntrin    "\<\(control\|widget\)\>"
syntax region       xmidasWidgCmd       start="\<\(control\|widget\)\>" skip="&\s*\(!.*\)\=$\|^\s*\(!.*\)\=$" end="$" contains=ALLBUT,xmidasCalcOper,xmidasFileQualList,xmidasMessFunc,xmidasEnvILVar,xmidasLogicOper,xmidasPunct,xmidasFileVectO,xmidasLabIdent,xmidasSpanLabIdent
syntax match        xmidasWidgFunc      contained "\<\([dfl]val\|menu\)\>"


" S W I T C H E S

syntax match        xmidasSwitch        "\/[a-z_][a-z0-9_]*=\="
syntax region       xmidasSpanSwitch    start="\/&\s*\(!.*\)\=$" skip="^\s*[a-z_][a-z0-9_]*&\s*\(!.*\)\=$\|^\s*\(!.*\)\=$" end="^\s*\([a-z0-9_]\+=\=\)\=" contains=xmidasLineCont keepend
syntax region       xmidasSpanSwitch    start="\/[a-z_][a-z0-9_]*&\s*\(!.*\)\=$" skip="^\s*[a-z0-9_]\+&\s*\(!.*\)\=$\|^\s*\(!.*\)\=$" end="^\s*\([a-z0-9_]*=\=\)\=" contains=xmidasLineCont keepend
syntax match        xmidasLabSwitch     contained "\/lab=[a-z0-9_]\+" contains=xmidasLabSwitchBG
syntax match        xmidasLabSwitchBG   contained transparent "\/lab=[a-z0-9_]\+"ms=s+5,hs=s+5 contains=xmidasLabIdent


" M E S S A G E   P S E U D O S T R U C T U R E S

" [EXPLAIN->MESSAGE]
syntax match        xmidasStruct        "\s/[a-z_][a-z0-9_]*/\s"hs=s+1,he=e-1
syntax match        xmidasStruct        "^/[a-z_][a-z0-9_]*/\s"he=e-1
syntax match        xmidasStruct        excludenl "\s/[a-z_][a-z0-9_]*/$"hs=s+1
syntax match        xmidasStruct        "\<[a-z_][a-z0-9_]*\.\(name\|id\|info\|args\|type\|val[1-9]\|val1[0-6]\)\>"


" R E S U L T   P A R A M E T E R   E X P A N S I O N   ( V A R I A B L E   D E R E F E R E N C I N G )

" beginning with ^{
"syntax region      xmidasSpanResExp    start="\^{.*&\s*$" skip="&\s*$\|^\s*\(!.*\)\=$" end="}" excludenl end="\s*$"he=s-1,me=s-1 contains=xmidasResExp,xmidasEnvIL,xmidasFile,xmidasStruct,xmidasSpanResExp,xmidasStringCont,xmidasCalcIL
"syntax region      xmidasResExp        oneline start="\^{" end="}" contains=xmidasResExp,xmidasEnvIL,xmidasFile,xmidasStruct,xmidasCalcIL
" beginning with ^
"syntax match       xmidasResExp        "\^[a-z_][a-z0-9_]*" contains=xmidasEnvIL,xmidasFile,xmidasStruct,xmidasCalcIL
"syntax region      xmidasSpanResExp    start="\^&\s*\(!.*\)\=$" skip="^\s*[a-z_][a-z0-9_]*&\s*\(!.*\)\=$\|^\s*\(!.*\)\=$" end="^\s*\([a-z0-9_]\+\)\=" contains=xmidasEnvIL,xmidasFile,xmidasStruct,xmidasResExp,xmidasSpanResExp,xmidasStringCont,xmidasCalcIL keepend
"syntax region      xmidasSpanResExp    start="\^[a-z_][a-z0-9_]*&\s*\(!.*\)\=$" skip="^\s*[a-z0-9_]\+&\s*\(!.*\)\=$\|^\s*\(!.*\)\=$" end="^\s*\([a-z0-9_]*\)\=" contains=xmidasEnvIL,xmidasFile,xmidasStruct,xmidasResExp,xmidasSpanResExp,xmidasStringCont,xmidasCalcIL keepend

" in development
syntax region       xmidasResExp        start="\^" skip="[a-z0-9_^]\=&\s*$\|^\s*\(!.*\)\=$" end="\w\>" end="\W"he=s-1,me=s-1 excludenl end="\s*$"he=s-1,me=s-1 contains=xmidasResExp,xmidasEnvIL,xmidasFile,xmidasStruct,xmidasStringCont,xmidasCalcIL
syntax region       xmidasResExp        start="\^{" skip="{\|&\s*$\|^\s*\(!.*\)\=$" end="\W" end=/"/he=s-1,me=s-1 excludenl end="\s*$"he=s-1,me=s-1 contains=xmidasResExp,xmidasEnvIL,xmidasFile,xmidasStruct,xmidasStringCont,xmidasCalcIL


exec "syntax sync minlines=25"


" H I G H L I G H T S

" intrinsics/keywords
hi link   xmidasMiscIntrin    xmidasIntrinsic
hi link   xmidasFileIntrin    xmidasIntrinsic
hi link   xmidasCalcIntrin    xmidasIntrinsic
hi link   xmidasLabIntrin     xmidasIntrinsic
hi link   xmidasKeyword       xmidasIntrinsic
hi link   xmidasIntrinsic     Statement

" inlining
hi link   xmidasCalcIL        xmidasInLine
hi link   xmidasLogIL         xmidasInLine
hi link   xmidasEnvIL         xmidasInLine
hi link   xmidasEnvILVar      xmidasInLine
hi link   xmidasInLine        PreProc

" operators
hi link   xmidasSeditFunc     xmidasOperator
hi link   xmidasGlobOper      xmidasOperator
hi link   xmidasCalcOper      xmidasOperator
hi link   xmidasLogicOper     xmidasOperator
hi        xmidasOperator      guifg=darkmagenta gui=bold

" command functions
hi link   xmidasMessFunc      xmidasCmdFunc
hi link   xmidasXcallFunc     xmidasCmdFunc
hi link   xmidasXconFunc      xmidasCmdFunc
hi link   xmidasEnvFunc       xmidasCmdFunc
hi link   xmidasWidgFunc      xmidasCmdFunc
hi        xmidasCmdFunc       gui=bold

" error checking backgrounds
hi link   xmidasFileQualList  xmidasErrChk
hi link   xmidasFileMiscBG    xmidasErrChk
hi link   xmidasFileHCBBG     xmidasErrChk
hi link   xmidasEnvILBG       xmidasErrChk
hi link   xmidasErrChk        Error

" file qualifiers
hi link   xmidasFile          xmidasFileQual
hi link   xmidasFileMisc      xmidasFileQual
hi link   xmidasFileHCB       xmidasFileQual
hi link   xmidasFileTrim      xmidasFileQual
hi link   xmidasFileVect      xmidasFileQual
hi link   xmidasFileVectO     xmidasFileQual
hi link   xmidasFileQual      Identifier

" switches
hi link   xmidasSwitch        xmidasHiSwitch
hi link   xmidasSpanSwitch    xmidasHiSwitch
hi link   xmidasLabSwitch     xmidasHiSwitch
hi link   xmidasHiSwitch      Special

hi link   xmidasStruct        Structure
hi link   xmidasString        String
hi link   xmidasSpanString    String
hi link   xmidasNumber        Number
hi link   xmidasComment       Comment
hi        xmidasStringCont    guibg=lightgray guifg=slateblue
hi        xmidasLineCont      guibg=lightgray guifg=slateblue
hi        xmidasOScom         guibg=lightgray guifg=darkblue
hi        xmidasSpanOScom     guibg=lightgray guifg=darkblue
hi        xmidasIdent         guibg=blue
hi        xmidasSpanIdent     guibg=blue
hi link   xmidasType          Type
hi link   xmidasPunct         Delimiter
hi link   xmidasResExp        PreProc
hi link   xmidasSpanResExp    PreProc
hi link   xmidasTab           Error
hi link   xmidasLabIdent      Todo
hi link   xmidasSpanLabIdent  Todo

let b:current_syntax="xmidas"

"EOF      vim: ts=10 noet tw=120 sw=8 sts=0


