" Vim syntax file
" Language:     PIC16F84 Assembler (Microchip's microcontroller)
" Maintainer:   Aleksandar Veselinovic <aleksa@cs.cmu.com>
" Last Change:  2003 May 11
" URL:		http://galeb.etf.bg.ac.yu/~alexa/vim/syntax/pic.vim
" Revision:     1.01

" 20040713 : changed some things to suit pic30fxxx assembly
" Yves Kerléguer yfig@lavache.com
" move this file to ~/.vim/syntax or /usr/share/vim/syntax
" to get dsPIC30F into the syntax menu add this line to /usr/share/vim/sunmenu.vim :
"  an 50.10.336 &Syntax.AB.Assembly.dsPIC30F : cal SetSyn("dspic30f")<CR>
" to use the syntax in a file add this line at its end :
" ; vim:set ts=4 syntax=dspic30f :
" (ts=4 is to get the same tabulations as in MPlab, otherwise the files are
" not clear)

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case match
syn keyword picTodo NOTE TODO XXX contained

syn case ignore

syn match picIdentifier "[a-z_$][a-z0-9_$]*"
syn match picLabel      "^[A-Z_$][A-Z0-9_$]*"
syn match picLabel      "^[A-Z_$][A-Z0-9_$]*:"me=e-1

syn match picASCII      "A\='.'"
syn match picBinary     "B'[0-1]\+'"
syn keyword picBinary	FAST W A ACCESS BANKED
syn match picDecimal    "D'\d\+'"
syn match picDecimal    "\d\+"
syn match picHexadecimal "0x\x\+"
syn match picHexadecimal "H'\x\+'"
syn match picHexadecimal "[0-9]\x*h"
syn keyword picHexadecimal	FSR0 FSR1 FSR2
syn match picOctal      "O'[0-7]\o*'"


syn match picComment    ";.*" contains=picTodo

syn region picString    start=+"+ end=+"+

syn keyword picRegister		W0 W1 W2 W3 W4 W5 W6 W7 W8 W9 W10 W11 W12 W13 W14 W15
syn keyword picRegister		SPLIM ACCAL ACCAH ACCAU ACCBL ACCBH ACCBU
syn keyword picRegister		PCL PCH TBLPAG PSVPAG RCOUNT DCOUNT DOSTARTL DOSTARTH DOENDL DOENDH SR CORCON
syn keyword picRegister		MODCON XMODSRT XMODEND YMODSRT YMODEND XBREV DISICNT
syn keyword picRegister		INTCON1 INTCON2 IFS0 IFS1 IFS2 IEC0 IEC1 IEC2
syn keyword picRegister		IPC0 IPC1 IPC2 IPC3 IPC4 IPC5 IPC6 IPC7 IPC8 IPC9 IPC10 IPC11
syn keyword picRegister		NVMCON NVMADR NVMADRU NVMKEY
syn keyword picRegister		TRISB PORTB LATB
syn keyword picRegister		TRISC PORTC LATC
syn keyword picRegister		TRISD PORTD LATD
syn keyword picRegister		TRISE PORTE LATE
syn keyword picRegister		TRISF PORTF LATF
syn keyword picRegister		CNEN1 CNEN2 CNPU1 CNPU2
syn keyword picRegister		TMR1 PR1 T1CON
syn keyword picRegister		TMR2 TMR3HLD TMR3 PR2 PR3 T2CON T3CON
syn keyword picRegister		IC1BUF IC1CON IC2BUF IC2CON IC3BUF IC3CON IC4BUF IC4CON IC5BUF IC5CON IC6BUF IC6CON IC7BUF IC7CON IC8BUF IC8CON
syn keyword picRegister		OC1RS OC1R OC1CON OC2RS OC2R OC2CON
syn keyword picRegister		QEICON DFLTCON POSCNT MAXCNT
syn keyword picRegister		PTCON PTMR PTPER SEVTCMP PWMCON1 PWMCON2 DTCON1 FLTACON OVDCON PDC1 PDC2 PDC3
syn keyword picRegister		SPI1STAT SPI1CON SPI1BUF
syn keyword picRegister		I2CRCV I2CTRN I2CBRG I2CCON I2CSTAT I2CADD
syn keyword picRegister		U1MODE U1STA U1TXREG U1RXREG U1BRG
syn keyword picRegister		ADCBUF0 ADCBUF1 ADCBUF2 ADCBUF3 ADCBUF4 ADCBUF5 ADCBUF6 ADCBUF7 ADCBUF8 ADCBUF9
syn keyword picRegister		ADCBUFA ADCBUFB ADCBUFC ADCBUFD ADCBUFE ADCBUFF ADCON1 ADCON2 ADCON3 ADCHS ADPCFG ADCSSL
syn keyword picRegister		RCON OSCCON PMD1 PMD2
syn keyword picRegister		FOSC FWDT FBORPDR FGS


" Register --- bits

" SR
syn keyword picRegisterPart	OA OB SA SB OAB SAB DA DC IPL2 IPL1 IPL0 RA N OV Z C

" CORCON
syn keyword picRegisterPart	US EDT SATA SATB SATDW ACCSAT IPL3 PSV RND IF
syn match   picRegisterPart	"DL[0-2]"

" MODCON
syn keyword picRegisterPart	XMODEN YMODEN
syn match   picRegisterPart	"BWM[0-3]"
syn match   picRegisterPart	"YWM[0-3]"
syn match   picRegisterPart	"XWM[0-3]"

" XMODSRT
syn match   picRegisterPart	"XS[1-9]"
syn match   picRegisterPart	"XS1[0-5]"

" XMODEND
syn match   picRegisterPart	"XE[1-9]"
syn match   picRegisterPart	"XE1[0-5]"

" YMODSRT
syn match   picRegisterPart	"YS[1-9]"
syn match   picRegisterPart	"YS1[0-5]"

" YMODEND
syn match   picRegisterPart	"YE[1-9]"
syn match   picRegisterPart	"YE1[0-5]"

" XBREV
syn keyword picRegisterPart	BREN
syn match   picRegisterPart	"XB[0-9]"
syn match   picRegisterPart	"XB1[0-4]"

" DISICNT
syn match   picRegisterPart	"DISICNT[0-9]"
syn match   picRegisterPart	"DISICNT1[0-3]"

" INTCON1
syn keyword picRegisterPart	NSTDIS OVATE OVBTE COVTE MATHERR ADDRERR STKERR OSCFAIL

" INTCON2
syn keyword picRegisterPart	ALTIVT
syn match   picregisterPart	"INT[0-4]EP"

" IFS0
syn keyword picRegisterPart	CNIF MI2CIF SI2CIF NVMIF ADIF U1TXIF U1RXIF SPI1IF T3IF T2IF OC2IF IC2IF T1IF OC1IF IC1IF INT0IF

" IFS1
syn match   picRegisterPart	"IC[3-6]IF"
syn keyword picRegisterPart	C1IF SPI2IF U2TXIF U2RXIF INT2IF T5IF T4IF OC4IF OC3IF IC8IF IC7IF INT1IF

" IFS2
syn match   picRegisterPart	"OC[5-8]IF"
syn keyword picRegisterPart	FLTBIF FLTAIF LVDIF QEIIF PWMIF C2IF INT4IF INT3IF

" IEC0
syn keyword picRegisterPart	CNIE MI2CIE SI2CIE NVMIE ADIE U1TXIE U1RXIE SPI1IE T3IE T2IE OC2IE IC2IE T1IE OC1IE IC1IE INT0IE

" IEC1
syn match   picRegisterPart	"IC[3-6]IE"
syn keyword picRegisterPart	C1IE SPI2IE U2TXIE U2RXIE INT2IE T5IE T4IE OC4IE OC3IE IC8IE IC7IE INT1IE

" IEC2
syn keyword picRegisterPart	FLTBIE FLTAIE LVDIE QEIIE PWMIE C2IE INT4IE INT3IE
syn match   picRegisterPart	"OC[5-8]IE"

" IPC0
syn match   picRegisterPart	"T1IP[0-2]"
syn match   picRegisterPart	"OC1IP[0-2]"
syn match   picRegisterPart	"IC1IP[0-2]"
syn match   picRegisterPart	"INT0IP[0-2]"

" IPC1
syn match   picRegisterPart	"T3IP[0-2]"
syn match   picRegisterPart	"T2IP[0-2]"
syn match   picRegisterPart	"OC2IP[0-2]"
syn match   picRegisterPart	"IC2IP[0-2]"

" IPC2
syn match   picRegisterPart	"ADIP[0-2]"
syn match   picRegisterPart	"U1TXIP[0-2]"
syn match   picRegisterPart	"U1RXIP[0-2]"
syn match   picRegisterPart	"SPI1IP[0-2]"

" IPC3
syn match   picRegisterPart	"CNIP[0-2]"
syn match   picRegisterPart	"MI2CIP[0-2]"
syn match   picRegisterPart	"SI2CIP[0-2]"
syn match   picRegisterPart	"NVMIP[0-2]"

" IPC4
syn match   picRegisterPart	"OC3IP[0-2]"
syn match   picRegisterPart	"IC8IP[0-2]"
syn match   picRegisterPart	"IC7IP[0-2]"
syn match   picRegisterPart	"INT1IP[0-2]"

" IPC5
syn match   picRegisterPart	"INT2IP[0-2]"
syn match   picRegisterPart	"T5IP[0-2]"
syn match   picRegisterPart	"T4IP[0-2]"
syn match   picRegisterPart	"OC4IP[0-2]"

" IPC6
syn match   picRegisterPart	"C1IP[0-2]"
syn match   picRegisterPart	"SPI2IP[0-2]"
syn match   picRegisterPart	"U2TXIP[0-2]"
syn match   picRegisterPart	"U2RXIP[0-2]"

" IPC7
syn match   picRegisterPart	"IC[3-6]IP[0-2]"

" IPC8
syn match   picRegisterPart	"OC[5-8]IP[0-2]"

" IPC9
syn match   picRegisterPart	"PWMIP[0-2]"
syn match   picRegisterPart	"C2IP[0-2]"
syn match   picRegisterPart	"INT4IP[0-2]"
syn match   picRegisterPart	"INT3IP[0-2]"

" IPC10
syn match   picRegisterPart	"FLTAIP[0-2]"
syn match   picRegisterPart	"LVDIP[0-2]"
syn match   picRegisterPart	"QEIIP[0-2]"

" IPC11
syn match   picRegisterPart	"FLTBIP[0-2]"

" NVMCON
syn match   picRegisterPart	"PROGOP[0-6]"
syn keyword picRegisterPart	WR WREN WRERR TWRI

" NVMADR
syn match   picRegisterPart	"NVMADR[0-9]"
syn match   picRegisterPart	"NVMADR1[0-5]"

" VVMADRU
syn match   picRegisterPart	"NVMADR1[6-9]"
syn match   picRegisterPart	"NVMADR2[0-3]"

" NVMKE
syn match   picRegisterPart	"KEY[0-7]"

" TRISB
syn match   picRegisterPart	"TRISB[0-5]"

" PORTB
syn match   picRegisterPart	"RB[0-5]"

" LATB
syn match   picRegisterPart	"LATB[0-5]"

" TRISC
syn match   picRegisterPart	"TRISC1[3-5]"

" PORTC
syn match   picRegisterPart	"RC1[3-5]"

" LATC
syn match   picRegisterPart	"LATC1[3-5]"

" TRISD
syn match   picRegisterPart	"TRISD[0-1]"

" PORTD
syn match   picRegisterPart	"RD[0-1]"

" LATD
syn match   picRegisterPart	"LATD[0-1]"

" TRISF
syn match   picRegisterPart	"TRISF[2-3]"

" PORTF
syn match   picRegisterPart	"RF[2-3]"

" LATF
syn match   picRegisterPart	"LATF[2-3]"

" CNEN1
syn match   picRegisterPart	"CN1[0-5]IE"
syn match   picRegisterPart	"CN[8-9]IE"

" CNPU1
syn match   picRegisterPart	"CN1[0-5]PUE"
syn match   picRegisterPart	"CN[8-9]PUE"

" CNEN1
syn match   picRegisterPart	"CN[0-7]IE"

" CNEN2
syn match   picRegisterPart	"CN1[6-9]IE"
syn match   picRegisterPart	"CN2[0-1]IE"

" CNPU1
syn match   picRegisterPart	"CN[0-7]PUE"

" CNPU2
syn match   picRegisterPart	"CN1[6-9]PUE"
syn match   picRegisterPart	"CN2[0-1]PUE"

" T1CON, T2CON, T3CON
syn keyword picRegisterPart	TON TSIDL TGATE TCKPS1 TCKPS0 TSYNC T32 TCS

" IC*CON
syn keyword picRegisterPart	ICSIDL ICTMR ICI0 ICI1 ICOV ICBNE
syn match   picRegisterPart	"ICM[0-2]"

" OC*CON
syn keyword picRegisterPart	OCFRZ OCSIDL OCFLT1 OCTSEL1
syn match   picRegisterPart	"OCM[0-2]"

" QEICON
syn keyword picRegisterPart	CNTERR QEISIDL INDX UPDN QEIM2 QEIM1 QEIM0 SWPAB PCDOUT TQGATE TQCKPS1 TQCKPS0 POSRES TQCS UPDN_SRC

" DFLTCON
syn keyword picRegisterPart	IMV1 IMV0 CEID QEID QEOUT QECK2 QECK1 QECK0

" PTCON
syn match   picRegisterPart	"PTOPS[0-3]"
syn keyword picREgisterPart	PTEN PTSIDL PTKPS0 PTKPS1 PTMOD0 PTMOD1

" PTMR
syn keyword picRegisterPart	PTDIR

" SEVTCMP
syn keyword picRegisterPart	SEVTDIR

" PWMCON1
syn match   picRegisterPart	"PTMOD[1-3]"
syn match   picRegisterPart	"PEN[1-3]H"
syn match   picRegisterPart	"PEN[1-3]L"

" PWMCON2
syn match   picRegisterPart	"SEVOPS[0-3]"
syn keyword picRegisterPart	OSYNC UDIS

" DTCON1
syn match   picRegisterPart	"DTAPS[0-1]"

" FLTACON
syn match   picRegisterPart	"FAOV[1-3]H"
syn match   picRegisterPart	"FAOV[1-3]L"
syn match   picRegisterPart	"FAEN[1-3]"
syn keyword picRegisterPart	FLTAM

" OVDCON
syn match   picRegisterPart	"POVD[1-3]H"
syn match   picRegisterPart	"POVD[1-3]L"
syn match   picRegisterPart	"POUT[1-3]H"
syn match   picRegisterPart	"POUT[1-3]L"

" SPI1STAT
syn keyword picRegisterPart	SPIEN SPISIDL SPIROV SPITBF SPIRBF

" SPI1CON
syn keyword picRegisterPart	FRMEN SPIFSD DISSDO MODE16 SMP CKE SSEN CKP MSTEN SPRE2 SPRE1 SPRE0 PPRE1 PPRE0

" I2CCON
syn keyword picRegisterPart	I2CEN I2CSIDL SCLREL IPMEN A10M DISSLW SMEN GCEN STREN ACKDT ACKEN RCEN PEN RSEN SEN

" I2CSTAT
syn keyword picRegisterPart	ACKSTAT TRSTAT BCL GCSTAT ADD10 IWCOL I2COV D_A P S R_W RBF TBF

" U1MODE
syn keyword picRegisterPart	UARTEN USIDL ALTIO WAKE LPBACK ABAUD PDSEL1 PDSEL0 STSEL

" U1STA
syn keyword picRegisterPart	UTXISEL UTXBRK UTXEN UTXBF TRMT URXISEL1 URXISEL0 ADDEN RIDLE PERR FERR OERR URXDA

" U1TXREG
syn keyword picRegisterPart	UTX8

" U1RXREG
syn keyword picRegisterPart	URX8

" ADCON1
syn keyword picRegisterPart	ADON ADSIDL FORM0 FORM1 SSRC0 SSRC1 SSRC2 SIMSAM ASAM SAMP DONE

" ADCON2
syn keyword picRegisterPart	VCFG0 VCFG1 VCFG2 CSCNA CHPS0 CHPS1 BUFS SMPI0 SMPI1 SMPI2 SMPI3 BUFM ALTS

" ADCON3
syn keyword picRegisterPart	SAMC0 SAMC1 SAMC2 SAMC3 ADRC
syn match   picRegisterPart	"ADCS[0-5]"

" ADCHS
syn keyword picRegisterPart	CH123SB CH0NB CH123SA CH0NA
syn match   picRegisterPart	"CH123NB[0-1]"
syn match   picRegisterPart	"CH0SB[0-3]"
syn match   picRegisterPart	"CH123NA[0-1]"
syn match   picRegisterPart	"CH0SA[0-3]"

" ADPCFG
syn match   picRegisterPart	"PCFG[0-5]"

" ADCSSL
syn match   picRegisterPart	"CSSL[0-5]"

" RCON
syn keyword picRegisterPart	TRAPR IOPUWR BGST EXTR SWR SWDTEN WDTO SLEEP IDLE BOR POR

" OSCCON
syn match   picRegisterPart	"TUN[0-3]"
syn keyword picRegisterPart	COSC0 COSC1 NOSC0 NOSC1 POST0 POST1 LOCK CF LPOSCEN OSWEN

" PMD1
syn keyword picRegisterPart	T3MD T2MD T1MD QEIMD PWMMD I2CMD U1MD SPIMD ADCMD

" PMD2
syn keyword picRegisterPart	IC8MD IC7MD IC2MD IC1MD OC2MD OC1MD

" FOSC
syn keyword picRegisterPart	FSCKM0 FSCKM1 FOS0 FOS1 FPR0 FPR1 FPR2 FPR3

" FWDT
syn keyword picRegisterPart	FWDTEN FWPSA0 FWPSA1 FWPSB0 FWPSB1 FWPSB2 FWPSB3

" FBORPOR
syn keyword picRegisterPart	MCLREN PWMPIN HPOL LPOL BOREN BORV0 BORV1 FPWRT0 FPWRT1

" FGS
syn keyword picRegisterPart	GCP GWRP





" OpCodes...
syn keyword picOpcode		ADD ADDC AND ASR BCLR BRA BSET BSW BTG
syn keyword picOpcode		BTSC BTSS BTST BTSTS CALL CLR CLRWDT COM CP CP0 CP1 CPB CPSEQ CPSGT CPSNE DAW DEC DEC2
syn keyword picOpcode		DISI DIV DIVF DO ED EDAC EXCH FBCL FF1L FF1R GOTO INC INC2 IOR LAC LNK LSR MAC MOV MOVSAC MPY MPY.N
syn keyword picOpcode		DIV.sd DIV.sw DIV.ud DIV.uw
syn keyword picOpcode		MSC MUL NEG NOP POP PUSH PWRSAV RCALL REPEAT RESET RETFIE RETLW RETURN RLC RLNC RRC RRNC SAC SE SETM
syn keyword picOpcode		SFTAC SL SUB SUBB SUBR SUBBR SWAP TBLRDH TBLRDL TBLWTH TBLWTL ULNK XOR ZE



" Directives
syn keyword picDirective __BADRAM BANKISEL BANKSEL CBLOCK CODE __CONFIG
syn keyword picDirective CONSTANT DATA DB DE DT DW ELSE END ENDC
syn keyword picDirective ENDIF ENDM ENDW EQU ERROR ERRORLEVEL EXITM EXPAND
syn keyword picDirective EXTERN FILL GLOBAL IDATA __IDLOCS IF IFDEF IFNDEF
syn keyword picDirective INCLUDE LIST LOCAL MACRO __MAXRAM MESSG NOEXPAND
syn keyword picDirective NOLIST ORG PAGE PAGESEL PROCESSOR RADIX RES SET
syn keyword picDirective SPACE SUBTITLE TITLE UDATA UDATA_OVR UDATA_SHR
syn keyword picDirective VARIABLE WHILE INCLUDE
syn match picDirective   "#\=UNDEFINE"
syn match picDirective   "#\=INCLUDE"
syn match picDirective   "#\=DEFINE"


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_pic16f84_syntax_inits")
  if version < 508
    let did_pic16f84_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink picTodo		Todo
  HiLink picComment		Comment
  HiLink picDirective		Statement
  HiLink picLabel		Label
  HiLink picString		String

 "HiLink picOpcode		Keyword
 "HiLink picRegister		Structure
 "HiLink picRegisterPart	Special

  HiLink picASCII		String
  HiLink picBinary		Number
  HiLink picDecimal		Number
  HiLink picHexadecimal		Number
  HiLink picOctal		Number

  HiLink picIdentifier		Identifier

  delcommand HiLink
endif

let b:current_syntax = "pic"

" vim: ts=8
