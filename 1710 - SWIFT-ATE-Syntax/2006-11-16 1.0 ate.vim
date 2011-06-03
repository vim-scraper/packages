" Vim syntax file
" Language:	ATE SWIFT Message
" Maintainer:	Mike Meirsman <meirsman@ultim8team.com>
" Last Change:	Fri, 17 Nov 2006

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Message Tags
syn match ATEGenericTag                   "^:\w\{2,3}:\{1,2}"     contains=ATESequenceTag,ATEMessageReference
syn match ATESequenceTag        contained "^:\(16R\|16S\):.*$"    contains=ATESequenceTagID
syn match ATESequenceTagID      contained "^:\(16R\|16S\):"
syn match ATEMessageReference   contained "^:20\(C\)\=:\{1,2}.*$" contains=ATEMessageReferenceID
syn match ATEMessageReferenceID contained "^:20\(C\)\=:\{1,2}"

" F21 Basic Header
syn match ATEF21BasicHeader                       "{1:F21\w\{12}\d\{10}}" contains=ATEF21BasicHeaderTag,ATEF21BasicHeaderTagIndicator,ATEF21OurBIC,ATEF21OurLTC,ATEF21OurBranchCode,ATEF21SessionNumber,ATEF21SequenceNumber nextgroup=ATEF21BasicHeaderTag
syn match ATEF21BasicHeaderTag          contained "1:"           nextgroup=ATEF21BasicHeaderTagIndicator
syn match ATEF21BasicHeaderTagIndicator contained "F21"          nextgroup=ATEF21OurBIC
syn match ATEF21OurBIC                  contained "[A-Z0-9]\{8}" nextgroup=ATEF21OurLTC
syn match ATEF21OurLTC                  contained "[A-Z]"        nextgroup=ATEF21OurBranchCode
syn match ATEF21OurBranchCode           contained "[A-Z0-9]\{3}" nextgroup=ATEF21SessionNumber
syn match ATEF21SessionNumber           contained "\d\{4}"       nextgroup=ATEF21SequenceNumber
syn match ATEF21SequenceNumber          contained "\d\{6}"

" F21 Acknowledgement
syn match ATEF21UserHeader                "{4:\({[^}]*}\)\+}" contains=ATEF21MUR,ATEF21DateTime,ATEF21Accepted,ATEF21Rejected,ATEF21RejectionReason nextgroup=ATEF21MUR
syn match ATEF21MUR             contained "{108:[^}]*}" nextgroup=ATEF21DateTime
syn match ATEF21DateTime        contained "{177:[^}]*}" nextgroup=ATEF21Accepted
syn match ATEF21Accepted        contained "{451:0}"     nextgroup=ATEF21Rejected
syn match ATEF21Rejected        contained "{451:1}"     nextgroup=ATEF21RejectionReason
syn match ATEF21RejectionReason contained "{405:[^}]*}"

" F01 Basic Header
syn match ATEF01BasicHeader                       "{1:F01\w\{12}\d\{10}}" contains=ATEF01BasicHeaderTag,ATEF01BasicHeaderTagIndicator,ATEF01OurBIC,ATEF01OurLTC,ATEF01OurBranchCode,ATEF01SessionNumber,ATEF01SequenceNumber nextgroup=ATEF01BasicHeaderTag
syn match ATEF01BasicHeaderTag          contained "1:"           nextgroup=ATEF01BasicHeaderTagIndicator
syn match ATEF01BasicHeaderTagIndicator contained "F01"          nextgroup=ATEF01OurBIC
syn match ATEF01OurBIC                  contained "[A-Z0-9]\{8}" nextgroup=ATEF01OurLTC
syn match ATEF01OurLTC                  contained "[A-Z]"        nextgroup=ATEF01OurBranchCode
syn match ATEF01OurBranchCode           contained "[A-Z0-9]\{3}" nextgroup=ATEF01SessionNumber
syn match ATEF01SessionNumber           contained "\d\{4}"       nextgroup=ATEF01SequenceNumber
syn match ATEF01SequenceNumber          contained "\d\{6}"

" F01 Input Application Header
syn match ATEF01IApplicationHeader                 "{2:I\d\{3}\w\{12,13}\d\{,4}}" contains=ATEF01IApplicationHeaderTag,ATEF01IMessageInputOutput,ATEF01IMessageType,ATEF01ICorrespondantBIC,ATEF01ICorrespondantLTC,ATEF01ICorrespondantBranchCode,ATEF01IPriority nextgroup=ATEF01IApplicationHeaderTag
syn match ATEF01IApplicationHeaderTag    contained "2:"           nextgroup=ATEF01IMessageInputOutput
syn match ATEF01IMessageInputOutput      contained "I"            nextgroup=ATEF01IMessageType
syn match ATEF01IMessageType             contained "\d\{3}"       nextgroup=ATEF01ICorrespondantBIC
syn match ATEF01ICorrespondantBIC        contained "[A-Z0-9]\{8}" nextgroup=ATEF01ICorrespondantLTC
syn match ATEF01ICorrespondantLTC        contained "[A-Z]"        nextgroup=ATEF01ICorrespondantBranchCode
syn match ATEF01ICorrespondantBranchCode contained "[A-Z0-9]\{3}" nextgroup=ATEF01IPriority
syn match ATEF01IPriority                contained "\(S\|U\|N\)"  nextgroup=ATEF01IDeliveryMonitoring
syn match ATEF01IDeliveryMonitoring      contained "\d"           nextgroup=ATEF01IObsolescencePeriod
syn match ATEF01IObsolescencePeriod      contained "\d\{3}"

" F01 Output Application Header
syn match   ATEF01OApplicationHeader              "{2:O\d\{13}\w\{22}\d\{10}\w\=}" contains=ATEF01OApplicationHeaderTag,ATEF01OMessageInputOutput,ATEF01OMessageType,ATEF01OInputTime,ATEF01OInputDate,ATEF01OSenderBIC,ATEF01OSenderLTC,ATEF01OSenderBranchCode,ATEF01OSessionNumber,ATEF01OSequenceNumber,ATEF01OOutputDate,ATEF01OOutputTime,ATEF01OPriority nextgroup=ATEF01OApplicationHeaderTag
syn match   ATEF01OApplicationHeaderTag contained "2:"           nextgroup=ATEF01OMessageInputOutput
syn match   ATEF01OMessageInputOutput   contained "O"            nextgroup=ATEF01OMessageType
syn match   ATEF01OMessageType          contained "\d\{3}"       nextgroup=ATEF01OInputTime
syn match   ATEF01OInputTime            contained "\d\{4}"       nextgroup=ATEF01OInputDate
syn match   ATEF01OInputDate            contained "\d\{6}"       nextgroup=ATEF01OSenderBIC
syn match   ATEF01OSenderBIC            contained "[A-Z0-9]\{8}" nextgroup=ATEF01OSenderLTC
syn match   ATEF01OSenderLTC            contained "[A-Z]"        nextgroup=ATEF01OSenderBranchCode
syn match   ATEF01OSenderBranchCode     contained "[A-Z0-9]\{3}" nextgroup=ATEF01OSessionNumber
syn match   ATEF01OSessionNumber        contained "\d\{4}"       nextgroup=ATEF01OSequenceNumber
syn match   ATEF01OSequenceNumber       contained "\d\{6}"       nextgroup=ATEF01OOutputDate
syn match   ATEF01OOutputDate           contained "\d\{6}"       nextgroup=ATEF01OOutputTime
syn match   ATEF01OOutputTime           contained "\d\{4}"       nextgroup=ATEF01OPriority
syn match   ATEF01OPriority             contained "\(S\|U\|N\)"

" F01 User Header
syn match ATEF01UserHeader                  "{3:\({[^}]*}\)\+}" contains=ATEF01ServiceIdentifier,ATEF01MUR,ATEF01BankingPriority,ATEF01AddresseeInfo,ATEF01ValidationFlag nextgroup=ATEF01ServiceIdentifier
syn match ATEF01ServiceIdentifier contained "{103:[^}]*}" nextgroup=ATEF01MUR
syn match ATEF01MUR               contained "{108:[^}]*}" nextgroup=ATEF01BankingPriority
syn match ATEF01BankingPriority   contained "{113:[^}]*}" nextgroup=ATEF01AddresseeInfo
syn match ATEF01AddresseeInfo     contained "{115:[^}]*}" nextgroup=ATEF01ValidationFlag
syn match ATEF01ValidationFlag    contained "{119:[^}]*}"

" ATE Trailer
syn match ATETrailer "{5:\({[^}]*}\)\+}" contains=ATETrailerMAC,ATETrailerPAC,ATETrailerCHK,ATETrailerSYS,ATETrailerTNG,ATETrailerPDE,ATETrailerDLM,ATETrailerPDM,ATETrailerMRF nextgroup=ATETrailerMAC
syn match ATETrailerMAC contained "{MAC:[^}]*}" nextgroup=ATETrailerPAC
syn match ATETrailerPAC contained "{PAC:[^}]*}" nextgroup=ATETrailerCHK
syn match ATETrailerCHK contained "{CHK:[^}]*}" nextgroup=ATETrailerSYS
syn match ATETrailerSYS contained "{SYS:[^}]*}" nextgroup=ATETrailerTNG
syn match ATETrailerTNG contained "{TNG:[^}]*}" nextgroup=ATETrailerPDE
syn match ATETrailerPDE contained "{PDE:[^}]*}" nextgroup=ATETrailerDLM
syn match ATETrailerDLM contained "{DLM:[^}]*}" nextgroup=ATETrailerPDM
syn match ATETrailerPDM contained "{PDM:[^}]*}" nextgroup=ATETrailerMRF
syn match ATETrailerMRF contained "{MRF:[^}]*}"

" Qualifiers
syn keyword ATEQualifier ACCA ACCL ACCT ACCW ACRU ACTI ADDR ADEL ADEX ADMT ADSR ADTX
syn keyword ATEQualifier ADVI AEXP AFFM AGGR AGRE ALLO ALTE AMNT ANOU ANTO APER ASRF
syn keyword ATEQualifier ATAX AUDT AUTA AUTO AVAL BASK BCOL BENE BENM BENT BIDI BIMY
syn keyword ATEQualifier BIRT BLOK BOBD BOLQ BOOK BORE BORR BOTB BPAR BRCR BREF BUSE
syn keyword ATEQualifier BUYE BUYR CADE CAEV CALD CALL CAMV CANC CAND CANP CANR CAON
syn keyword ATEQualifier CAOP CAPG CAPP CASH CASY CATB CCAL CCAM CCMV CDEA CERT CEXD
syn keyword ATEQualifier CFRE CHAI CHAR CINL CINS CINT CITY CLAM CLAS CLBR CLCI CLEN
syn keyword ATEQualifier CLPA CLPR CMAF CNTR CNVF CNVT COAL COAX CODE COLA COLI COLL
syn keyword ATEQualifier COLO COLR COMI COMM COMP CONB CONF CONM CONS CONT CONU CONV
syn keyword ATEQualifier CORA CORD CORE CORP COST COUC COUN COUP COVA COVE COWA CPRC
syn keyword ATEQualifier CPTA CPTB CPTR CRDB CRET CRTR CSMV CSPD CUFC CVPR DAAC DBIR
syn keyword ATEQualifier DBNM DDTE DE17 DE18 DE19 DEAG DEAL DEBA DECL DECU DEI1 DEI2
syn keyword ATEQualifier DEI3 DEI4 DEI5 DEI6 DEI7 DEI8 DEI9 DENC DEND DENO DEPO DERE
syn keyword ATEQualifier DFLT DFON DIRT DISF DIST DITY DIVI DIVR DOCT DOMI DPLO DPRC
syn keyword ATEQualifier DRAW DRCU DRDE DRRE DRTY EARL EFFD ELIG EMAI EMOD ENTL EPRC
syn keyword ATEQualifier EREG ESET ESTT ETC1 ETC2 ETYP EXBN EXCH EXCR EXEC EXER EXPI
syn keyword ATEQualifier EXPP EXRQ EXSE FCOU FDDT FIAN FICL FIOP FLFR FOLQ FORC FORF
syn keyword ATEQualifier FORM FRNF FRNR FROM FSBN FSSA FXCO FXCR FXDT FXIB FXIN FXIS
syn keyword ATEQualifier FXOR FXTR GALO GCST GIUP GRSS GUAR HOLD HOLP HOLS IACC INBA
syn keyword ATEQualifier INBR INCE INCL INCO INDC INDF INDM INDP INDX INIM INOP INOU
syn keyword ATEQualifier INPA INPE INRE INST INT2 INTE INTR INVE IPRC ISAG ISDI ISSU
syn keyword ATEQualifier ITYP LADT LANG LDCO LDCR LDFP LEGL LEVY LICO LIDT LIMI LINK
syn keyword ATEQualifier LIST LOAN LOCA LOCL LOCO LOCR LOTE LOTO LOTS LYDT MACL MACO
syn keyword ATEQualifier MADW MAGV MAIL MARG MAST MATU MAUP MAXF MAXP MEET MEOR MERE
syn keyword ATEQualifier MICO MIEX MILT MINI MINO MINP MITR MKDT MKTB MKTC MKTP MKTV
syn keyword ATEQualifier MOVE MRKT MTCH NAFI NATO NEGR NETT NEWA NEWO NMAT NOMI NPCC
syn keyword ATEQualifier NRAD NRES NSER NWFC NWRT NXRT OCCU OCMT ODDC OFFE OFFO OFFP
syn keyword ATEQualifier OFFR OPCA OPST OPTI OPTN ORCU ORDE ORDR ORGV ORRE OSUB OTHR
syn keyword ATEQualifier OWND OWNF OWNT PACK PACO PADI PAFI PARS PART PAYD PAYE PAYM
syn keyword ATEQualifier PAYS PBOX PDMT PDUM PECA PEND PENF PENR PERM PEVA PFRE PLED
syn keyword ATEQualifier PODT POOL PORT POST PPDT PRBN PRCO PRCV PREC PREF PREL PREP
syn keyword ATEQualifier PREQ PRER PREV PREX PRFC PRHA PRIC PRIN PRIR PRIT PRMT PROC
syn keyword ATEQualifier PROD PROG PRPP PRPR PRSS PRUM PSET PSTA PSTT PTYA PTYB PUTT
syn keyword ATEQualifier QAGE QINS QINT QREG QSEC QTSO QUOT RADD RATS RCTR RDDT RDIS
syn keyword ATEQualifier RDTE REAG REAS RECA RECT RECU REDE REDM REDP REGF REGI REGO
syn keyword ATEQualifier REGT REI1 REI2 REI3 REI4 REI5 REI6 REI7 REI8 REI9 REIN REJT
syn keyword ATEQualifier RELA RELC RELD REMA REPA REPL REPO REPP REPR REPT REPU REQU
syn keyword ATEQualifier RERC RERT RESI REST RESU REVO RMAG RMOD ROUN RPOR RPRC RREA
syn keyword ATEQualifier RREG RSET RSPR RSTR RSTT RTGS RTUN RVAL SAFE SCOL SCTR SEBL
syn keyword ATEQualifier SECO SECU SECV SELL SEME SETG SETR SETT SFRE SHAI SHAR SHIP
syn keyword ATEQualifier SIZE SMAF SNUM SOIC SPCN SPLT SPOS SPRC SPRO STAM STAQ STAT
syn keyword ATEQualifier STBA STBR STCO STEX STIN STOP STRT STTY SUBB SUBS SUPR TACR
syn keyword ATEQualifier TAKO TAVI TAXC TAXE TAXR TCHA TCMV TCPI TCRL TERM TEXA THRS
syn keyword ATEQualifier THRU TILI TITL TLDE TOAL TOBA TOCO TODE TOOR TORE TOSE TPIN
syn keyword ATEQualifier TPOU TPRI TPRO TQBT TRAA TRAD TRAG TRAN TRAX TRCA TRCI TRCN
syn keyword ATEQualifier TRDE TRDP TRDT TRRE TRRF TRSE TRTE TRTR TRUS TSDT TSMV TTCO
syn keyword ATEQualifier TXDF TXFR TXNR TYCO TYPP UNBA UNCO UNDL UNFR UNRG UTIN VAFC
syn keyword ATEQualifier VALC VALE VALN VALU VASU VATA VEND VERN WITF WITH WITL WRTS
syn keyword ATEQualifier WUCO XCPN XDTE

" Codeworks
syn keyword ATECodeWork A001 A002 A003 A004 A005 A006 A007 A008 A009 AADJ ABEC ABRD
syn keyword ATECodeWork ABST ACCOUNT ACCP ACCT ACCTINFO ACTCURR ACTINFO ACTU ACTV
syn keyword ATECodeWork ACTX ADDINFO ADDRES ADDRESS ADEA ADHO ADVD AFBA AFFI AFFM
syn keyword ATECodeWork AFTE AFWD AGEN AGRE ALLDET ALNO AMDR AMER AMOR AMT AMT1 AMT2
syn keyword ATECodeWork ANNU ANOU APCP APFM APMT ARNU ARTI ASET ASIA ASOC ATCH ATTI
syn keyword ATECodeWork AUCL AUCT AUTH AVAI AVAL AVER AVFI AVOV AVPS AWMO AWSH BAGN
syn keyword ATECodeWork BASK BATC BBAA BCBL BCBN BCFD BCOL BCPD BCRO BCRP BCSE BCSH
syn keyword ATECodeWork BEAR BEFO BELS BENO BENODET BIDS BLCH BLKO BLOC BLOK BLPA
syn keyword ATECodeWork BLPG BMIN BOFI BOJS BOLE BONU BOOK BORR BPAR BPLG BPUT BRDE
syn keyword ATECodeWork BREF BRKR BRLI BRUP BSBC BSBO BSPL BTEX BTMI BUSE BUYA BUYI
syn keyword ATECodeWork BUYU BYIY BYSTAREA CACONF CADE CADETL CADJ CAEV CAGN CAINST
syn keyword ATECodeWork CAIS CALL CAN1 CAN2 CAN3 CANC CAND CANI CANO CANP CANR CANS
syn keyword ATECodeWork CAOPTN CAPG CARE CASE CASH CASHACCT CASHDET CASHMOVE
syn keyword ATECodeWork CASHSECDET CASHSET CASHSET1 CAST CASY CBNS CCFA CCIR CCOL
syn keyword ATECodeWork CCPN CCPT CDIV CEDE CEND CERT CERTSIGN CEXC CEXD CHAN CHAR
syn keyword ATECodeWork CHAS CHEC CHKA CHOS CHTY CINL CLAC CLAT CLDI CLEN CLLE CLNT
syn keyword ATECodeWork CLOA CLOP CLOS CLRA CLSA CLTDET CMIS CMON CODU COLD COLI
syn keyword ATECodeWork COLL COLO COMB COMM COMP COND CONF CONFDET CONFPRTY CONFPRTY1
syn keyword ATECodeWork CONFPRTY2 CONL CONN CONS CONT CONV CONY COPY CORD CORP CORR
syn keyword ATECodeWork COSE COUC CPCA CPEC CPNR CPRC CPRN CPST CPTY CRED CREG CRPR
syn keyword ATECodeWork CRSP CRST CRTL CRTS CSHPRTY CSHPRTY1 CSUB CTEN CUST CVPR
syn keyword ATECodeWork CWAR CYCL DADJ DADR DAIL DAKV DCAN DCRE DDAT DDEA DDOT DEAL
syn keyword ATECodeWork DEALTRAN DEBT DECR DEFI DELI DELN DELS DELT DEND DEPO DEPT
syn keyword ATECodeWork DERI DERV DETH DETI DFLA DFLT DIOR DIRT DISC DISE DISR DIST
syn keyword ATECodeWork DIVR DKCS DKNY DLST DMON DNIN DNRE DOCC DOCY DONE DONF DORD
syn keyword ATECodeWork DPRG DQUA DR01 DRAW DRDETL DREP DRIP DRLC DRPRTY DRRE DRST
syn keyword ATECodeWork DSEC DSET DTCH DTRA DTRD DUPL DUTH DVCA DVDS DVOP DVPA DVPT
syn keyword ATECodeWork DVSC DVSE EAFD EARL EBAC ECKA ECLR EDVO EFFD EINX ELIG ENGL
syn keyword ATECodeWork EOSP EPMS ERIS ESTA EUDR EURO EVST EXCH EXCR EXCS EXEC EXER
syn keyword ATECodeWork EXOF EXOP EXPD EXPI EXRQ EXSE EXTD EXTM EXWA FAMT FBDM FBEN
syn keyword ATECodeWork FBRA FCDM FCFS FDDT FELT FEXA FEYC FIA FIA1 FIA2 FIAC FIDE
syn keyword ATECodeWork FIDETL FIKI FIMY FIN FIN1 FIN2 FINA FINL FIXE FIXI FIXT
syn keyword ATECodeWork FLAT FLFR FLIM FMVA FOCT FOGU FOPT FORF FORW FORX FRAC FRAP
syn keyword ATECodeWork FRAU FRCL FREE FREEASS FREN FTIN FULL FUND FUNG FUTM FUTU
syn keyword ATECodeWork FXDET FXDT FXORDER FXPRTY1 FXPRTY2 FXSETDET GAIN GBSC GCOL
syn keyword ATECodeWork GDAY GENL GENR GENS GERM GIVE GLDR GLOB GREX GROS GROSS GRSC
syn keyword ATECodeWork GTCA GTDL GTHD GTMO GTNM GTXO GUAD HEDG HERM HERR HIST HSVP
syn keyword ATECodeWork IACT ICAG ICOL ICOP ICSD ICUS IEXE IIND IMFA INCA INCOME
syn keyword ATECodeWork INCR INDA INDE INDI INDR INDV INDX INEL INFO INHE INIT INPOS
syn keyword ATECodeWork INPOSDET INST INSU INTA INTE INTL INTR INTSEC INVA IOCA IPCP
syn keyword ATECodeWork IPFM IPNC IRIS ISDA ISSU ITAL ITIN KACC KNOC LAAW LACK LALO
syn keyword ATECodeWork LAMI LAPS LAST LATE LATT LBDM LCDM LCOL LDDET LDPRTY1 LDPRTY2
syn keyword ATECodeWork LEGL LEND LINK LINK1 LINK2 LINK3 LIPS LIQU LIST LIWI LIWO
syn keyword ATECodeWork LMAR LMTO LOAN LOSS LOTO LVTS MACH MADA MAKT MAND MANH MARG
syn keyword ATECodeWork MATU MCAL MCAN MCER MCRA MEET MINO MISM MISS MKDT MLAT MLEG
syn keyword ATECodeWork MNGD MNTH MONY MORE MPUT MR01 MRGR MRSS MUNO MUTO MUTU NAFF
syn keyword ATECodeWork NAFI NAME NARR NAVL NBEN NBOR NCON NCRR NCRS NCSD NDIS NELG
syn keyword ATECodeWork NET NET1 NET2 NETC NETS NEWI NEWM NFUN NGIV NICS NILL NMAS
syn keyword ATECodeWork NMAT NOAC NOFR NOFX NOGE NOGR NOHE NOIN NOMI NONR NOPE NOPRINC
syn keyword ATECodeWork NOPU NORE NOTC NOWA NOXO NPAR NPAY NREG NREN NRGM NRGN NRST
syn keyword ATECodeWork NRTG NSEC NSSP NSTA NTBK NUND OAGN OBJT ODLS ODLT OFFR OMET
syn keyword ATECodeWork ONGO ONLY OPEF OPEN OPEP OPOD OPTN ORDER ORDN ORDRDET ORDRPRTY
syn keyword ATECodeWork ORLI OTCD OTCO OTHE OTHR OTHRPRTY OUTT OVER OWNE OWNI OWNT
syn keyword ATECodeWork PACK PADJ PAFI PAFILL PAIN PAIR PALL PARC PARF PARI PART PARV
syn keyword ATECodeWork PATN PAYD PAYM PAYU PCAL PCHS PDEF PECA PEGE PEND PENF PENR
syn keyword ATECodeWork PENS PERN PERS PERSDET PERU PHSE PHYS PINK PLAC PLCE PLED
syn keyword ATECodeWork PLOT PNSF PODT PODU POSS PPDT PPMT PRAG PRCA PRCT PREA PREC
syn keyword ATECodeWork PREDET PREM PREU PRFD PRIC PRII PRIM PRIN PRIO PROD PROG PROR
syn keyword ATECodeWork PROV PROX PRSE PUTO PUTT PVEV PWCD PWEU QALL QEIN QUOT QUTR
syn keyword ATECodeWork RADR RCAP RDAV RDDN RDDT RDTE RDUP RDWN REAS RECDEL RECE REDI
syn keyword ATECodeWork REDM REDO REF REFE REFS REFT REFU REGD REGDET REGI REGR REGS
syn keyword ATECodeWork REGT REJT RELE RELS REMK REMO RENO REPA REPC REPE REPL REPO
syn keyword ATECodeWork REPP REPU REPV REQM REQW RERE RERT REST RESU REVE REVR REVS 
syn keyword ATECodeWork RGIS RHDI RHTS RIXS RMDR ROLL ROLP RPTO RQWV RREA RSPR RSTR
syn keyword ATECodeWork RTGS RVSL SAFE SAGE SAMO SBLO SBSB SCIR SCOL SCOP SCRP SDOT
syn keyword ATECodeWork SDUT SECDET SECL SECM SECMOVE SECS SECU SELL SEMI SEOP SEPI
syn keyword ATECodeWork SESE SETDET SETDET1 SETI SETLL SETPRTY SETPRTY1 SETR SETT
syn keyword ATECodeWork SETTL SETTRAN SHAR SHHE SHOR SIGN SIRR SITS SLBE SLEB SLLE
syn keyword ATECodeWork SLOS SMAL SMPG SOFF SOFT SOIC SOLI SPAN SPCS SPCU SPDL SPEC
syn keyword ATECodeWork SPEX SPGT SPLF SPLG SPLI SPLR SPLT SPLU SPRI SPSI SPST SSEX
syn keyword ATECodeWork SSHO SSNX SSTI STAF STAM STAN STAT STAT1 STAT2 STCD STEP
syn keyword ATECodeWork STIN STLI STOP STRE SUBBAL SUBR SUBS SUBSAFE SUMD SUMM SUSP
syn keyword ATECodeWork SWIF SWIT TAMM TARG TAXA TAXI TBAT TBFF TCFA TCKA TCRP TEND
syn keyword ATECodeWork TERM THEO TIME TOBK TOPN TRAD TRADDET TRADE TRADET1 TRADET2
syn keyword ATECodeWork TRADPRTY TRAN TRANSDET TRANVAL TRDT TREA TREATBENF TREC TRPO
syn keyword ATECodeWork TRRE TRSH TRUS TSDT TTRE TURN TXDF TXEX TXFR TXID UKWN ULNK
syn keyword ATECodeWork UNCO UNDE UNEX UNFR UNIT UNKN UNRG USCH USECU USEX USFW USNX
syn keyword ATECodeWork USOL UTRU VALDET VALI VALU VARI VASU VDAA VEND VIBE VLDA VOLU
syn keyword ATECodeWork W8BE W8EC W8EX W8IM W9US WBKG WDIS WEEK WIDI WISS WITH WSET
syn keyword ATECodeWork WUCO XBNS XCPN XDIV XDTE XMET XRTS XWAR YBEN YBOR YCOL YEAR
syn keyword ATECodeWork YIEL YREG YRTG ZENG

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_ATE_syntax_inits")
  if version < 508
    let did_ATE_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  " The following syntax highlighting conforms to vim standards.
  " If you wish to add more colors, you could replace the following
  " block by the one that is commented.

  " STANDARD HIGHLIGHTING

  " Message Tags
  HiLink ATEGenericTag			String
  HiLink ATESequenceTag			Delimiter
  HiLink ATESequenceTagID		Delimiter
  HiLink ATEMessageReference		Typedef
  HiLink ATEMessageReferenceID		Typedef

  " F21 Basic Header
  HiLink ATEF21BasicHeader		Exception
  HiLink ATEF21BasicHeaderTag		Exception
  HiLink ATEF21BasicHeaderTagIndicator	Exception
  HiLink ATEF21OurBIC			Exception
  HiLink ATEF21OurLTC			Exception
  HiLink ATEF21OurBranchCode		Exception
  HiLink ATEF21SessionNumber		Exception
  HiLink ATEF21SequenceNumber		Exception
  
  " F21 Acknowledgement
  HiLink ATEF21UserHeader		Conditional
  HiLink ATEF21MUR			Conditional
  HiLink ATEF21DateTime			Conditional
  HiLink ATEF21Accepted			Conditional
  HiLink ATEF21Rejected			Conditional
  HiLink ATEF21RejectionReason		Conditional
  
  " F01 Basic Header
  HiLink ATEF01BasicHeader		Operator
  HiLink ATEF01BasicHeaderTag		Operator
  HiLink ATEF01BasicHeaderTagIndicator	Operator
  HiLink ATEF01OurBIC			Operator
  HiLink ATEF01OurLTC			Operator
  HiLink ATEF01OurBranchCode		Operator
  HiLink ATEF01SessionNumber		Operator
  HiLink ATEF01SequenceNumber		Operator
  
  " F01 Input Application Header
  HiLink ATEF01IApplicationHeader	Label
  HiLink ATEF01IApplicationHeaderTag	Label
  HiLink ATEF01IMessageInputOutput	Label
  HiLink ATEF01IMessageType		Label
  HiLink ATEF01ICorrespondantBIC	Label
  HiLink ATEF01ICorrespondantLTC	Label
  HiLink ATEF01ICorrespondantBranchCode	Label
  HiLink ATEF01IPriority		Label
  HiLink ATEF01IDeliveryMonitoring	Label
  HiLink ATEF01IObsolescencePeriod	Label
 
  " F01 Output Application Header
  HiLink ATEF01OApplicationHeader	Type
  HiLink ATEF01OApplicationHeaderTag	Type
  HiLink ATEF01OMessageInputOutput	Type
  HiLink ATEF01OMessageType		Type
  HiLink ATEF01OInputTime		Type
  HiLink ATEF01OInputDate		Type
  HiLink ATEF01OSenderBIC		Type
  HiLink ATEF01OSenderLTC		Type
  HiLink ATEF01OSenderBranchCode	Type
  HiLink ATEF01OSessionNumber		Type
  HiLink ATEF01OSequenceNumber		Type
  HiLink ATEF01OOutputDate		Type
  HiLink ATEF01OOutputTime		Type
  HiLink ATEF01OPriority		Type
 
  " F01 User Header
  HiLink ATEF01UserHeader		Tag
  HiLink ATEF01ServiceIdentifierx	Tag
  HiLink ATEF01MUR		 	Tag
  HiLink ATEF01BankingPriority		Tag
  HiLink ATEF01AddresseeInfo		Tag
  HiLink ATEF01ValidationFlag		Tag
 
  " ATE Trailer
  HiLink ATETrailer			Comment
  HiLink ATETrailerMAC			Comment
  HiLink ATETrailerPAC			Comment
  HiLink ATETrailerCHK			Comment
  HiLink ATETrailerSYS			Comment
  HiLink ATETrailerTNG			Comment
  HiLink ATETrailerPDE			Comment
  HiLink ATETrailerDLM			Comment
  HiLink ATETrailerPDM			Comment
  HiLink ATETrailerMRF			Comment
 
  " Qualifiers and Codeworks
  HiLink ATEQualifier			Keyword
  HiLink ATECodeWork			Keyword
  

  " NON STANDARD HIGHLIGHTING

  " " Message Tags
  " hi ATEGenericTag			ctermfg=blue 		cterm=none	guifg=lightblue		guibg=NONE
  " hi ATESequenceTag			ctermfg=white 		cterm=bold	ctermbg=darkgray	guifg=white		guibg=gray
  " hi ATESequenceTagID			ctermfg=white 		cterm=bold	ctermbg=darkblue	guifg=white		guibg=lightblue
  " hi ATEMessageReference		ctermfg=white 		cterm=bold	ctermbg=darkred		guifg=white		guibg=lightred
  " hi ATEMessageReferenceID		ctermfg=white 		cterm=bold	ctermbg=darkblue	guifg=white		guibg=lightblue
  "
  " " F21 Basic Header
  " hi ATEF21BasicHeader			ctermfg=blue 		cterm=bold	guifg=lightblue		guibg=NONE
  " hi ATEF21BasicHeaderTag		ctermfg=blue 		cterm=bold	guifg=lightblue		guibg=NONE
  " hi ATEF21BasicHeaderTagIndicator	ctermfg=blue 		cterm=bold	guifg=lightblue		guibg=NONE
  " hi ATEF21OurBIC			ctermfg=darkred 	cterm=none	guifg=white 		guibg=darkgray
  " hi ATEF21OurLTC			ctermfg=gray 		cterm=bold	guifg=lightgray		guibg=darkgray
  " hi ATEF21OurBranchCode		ctermfg=darkred 	cterm=none	guifg=white 		guibg=darkgray
  " hi ATEF21SessionNumber		ctermfg=cyan 		cterm=none	guifg=darkblue	 	guibg=lightgray
  " hi ATEF21SequenceNumber		ctermfg=darkcyan 	cterm=none	guifg=darkcyan 		guibg=lightgray
  " 
  " " F21 Acknowledgement
  " hi ATEF21UserHeader			ctermfg=blue 		cterm=bold	guifg=lightblue		guibg=NONE
  " hi ATEF21MUR				ctermfg=darkmagenta	cterm=bold	guifg=black 		guibg=lightmagenta
  " hi ATEF21DateTime			ctermfg=darkmagenta 	cterm=bold	guifg=black 		guibg=lightyellow
  " hi ATEF21Accepted			ctermfg=darkmagenta 	cterm=bold	guifg=black 		guibg=lightgreen
  " hi ATEF21Rejected			ctermfg=darkmagenta 	cterm=bold	guifg=black 		guibg=lightred
  " hi ATEF21SIBESRejected		ctermfg=darkmagenta 	cterm=bold	guifg=black 		guibg=red
  " hi ATEF21RejectionReason		ctermfg=darkred 	cterm=none	guifg=white 		guibg=lightred
  "
  " " F01 Basic Header
  " hi ATEF01BasicHeader			ctermfg=blue 		cterm=bold	guifg=lightblue		guibg=NONE
  " hi ATEF01BasicHeaderTag		ctermfg=blue 		cterm=bold	guifg=lightblue		guibg=NONE
  " hi ATEF01BasicHeaderTagIndicator	ctermfg=blue 		cterm=bold	guifg=lightblue		guibg=NONE
  " hi ATEF01OurBIC			ctermfg=darkred 	cterm=none	guifg=white 		guibg=darkgray
  " hi ATEF01OurLTC			ctermfg=gray 		cterm=bold	guifg=lightgray	 	guibg=darkgray
  " hi ATEF01OurBranchCode		ctermfg=darkred 	cterm=none	guifg=white 		guibg=darkgray
  " hi ATEF01SessionNumber		ctermfg=cyan 		cterm=none	guifg=darkblue 		guibg=lightgray
  " hi ATEF01SequenceNumber		ctermfg=darkcyan 	cterm=none	guifg=darkcyan 		guibg=lightgray
  "
  " " F01 Input Application Header
  " hi ATEF01IApplicationHeader		ctermfg=blue 		cterm=bold	guifg=lightblue		guibg=NONE
  " hi ATEF01IApplicationHeaderTag	ctermfg=blue 		cterm=bold	guifg=lightblue		guibg=NONE
  " hi ATEF01IMessageInputOutput		ctermfg=darkred 	cterm=none	guifg=white		guibg=brown
  " hi ATEF01IMessageType			ctermfg=yellow 		cterm=bold	guifg=lightyellow	guibg=brown
  " hi ATEF01ICorrespondantBIC		ctermfg=darkred 	cterm=none	guifg=white 		guibg=darkgray
  " hi ATEF01ICorrespondantLTC		ctermfg=gray 		cterm=bold	guifg=lightgray 	guibg=darkgray
  " hi ATEF01ICorrespondantBranchCode	ctermfg=darkred 	cterm=none	guifg=white 		guibg=darkgray
  " hi ATEF01IPriority			ctermfg=red 		cterm=bold	guifg=lightred 		guibg=NONE
  " hi ATEF01IDeliveryMonitoring		ctermfg=blue 		cterm=bold	guifg=lightblue 	guibg=NONE
  " hi ATEF01IObsolescencePeriod		ctermfg=magenta 	cterm=bold	guifg=lightmagenta 	guibg=NONE
  "
  " " F01 Output Application Header
  " hi ATEF01OApplicationHeader		ctermfg=blue 		cterm=bold	guifg=lightblue		guibg=NONE
  " hi ATEF01OApplicationHeaderTag	ctermfg=blue 		cterm=bold	guifg=lightblue		guibg=NONE
  " hi ATEF01OMessageInputOutput		ctermfg=darkred 	cterm=none	guifg=white		guibg=darkblue
  " hi ATEF01OMessageType			ctermfg=yellow 		cterm=bold	guifg=lightyellow	guibg=darkblue
  " hi ATEF01OInputTime			ctermfg=darkgray 	cterm=none	guifg=darkgray 		guibg=lightyellow
  " hi ATEF01OInputDate			ctermfg=gray 		cterm=none	guifg=gray	 	guibg=lightyellow
  " hi ATEF01OSenderBIC			ctermfg=darkred 	cterm=none	guifg=white 		guibg=darkgray
  " hi ATEF01OSenderLTC			ctermfg=gray 		cterm=bold	guifg=lightgray 	guibg=darkgray
  " hi ATEF01OSenderBranchCode		ctermfg=darkred 	cterm=none	guifg=white 		guibg=darkgray
  " hi ATEF01OSessionNumber		ctermfg=cyan 		cterm=none	guifg=darkblue		guibg=lightgray
  " hi ATEF01OSequenceNumber		ctermfg=darkcyan 	cterm=none	guifg=darkcyan 		guibg=lightgray
  " hi ATEF01OOutputDate			ctermfg=darkgray 	cterm=none	guifg=darkgray 		guibg=lightyellow
  " hi ATEF01OOutputTime			ctermfg=gray 		cterm=none	guifg=gray 		guibg=lightyellow
  " hi ATEF01OPriority			ctermfg=red 		cterm=bold	guifg=lightred		guibg=NONE
  "
  " " F01 User Header
  " hi ATEF01UserHeader			ctermfg=blue 		cterm=bold	guifg=lightblue		guibg=NONE
  " hi ATEF01ServiceIdentifier		ctermfg=darkmagenta 	cterm=bold	guifg=black 		guibg=lightgray
  " hi ATEF01MUR				ctermfg=darkmagenta 	cterm=bold	guifg=black 		guibg=lightmagenta
  " hi ATEF01BankingPriority		ctermfg=darkmagenta 	cterm=bold	guifg=black 		guibg=lightcyan
  " hi ATEF01AddresseeInfo		ctermfg=darkmagenta 	cterm=bold	guifg=black 		guibg=lightyellow
  " hi ATEF01ValidationFlag		ctermfg=darkmagenta 	cterm=bold	guifg=black 		guibg=lightgray
  "
  " " ATE Trailer
  " hi ATETrailer				ctermfg=blue 		cterm=bold	guifg=lightblue		guibg=NONE
  " hi ATETrailerMAC			ctermfg=yellow 		cterm=bold	guifg=lightyellow 	guibg=black
  " hi ATETrailerPAC			ctermfg=blue 		cterm=bold	guifg=lightblue 	guibg=black
  " hi ATETrailerCHK			ctermfg=green 		cterm=bold	guifg=lightgreen 	guibg=black
  " hi ATETrailerSYS			ctermfg=grey 		cterm=bold	guifg=lightgrey 	guibg=black
  " hi ATETrailerTNG			ctermfg=magenta 	cterm=bold	guifg=lightmagenta 	guibg=black
  " hi ATETrailerPDE			ctermfg=darkred 	cterm=none	guifg=white 		guibg=red
  " hi ATETrailerDLM			ctermfg=red 		cterm=bold	guifg=lightred 		guibg=black
  " hi ATETrailerPDM			ctermfg=darkred 	cterm=none	guifg=white 		guibg=black
  " hi ATETrailerMRF			ctermfg=cyan 		cterm=bold	guifg=lightcyan 	guibg=black
  "
  " " Qualifiers and Codeworks
  " hi ATEQualifier			ctermfg=cyan		cterm=none	guifg=darkblue		guibg=NONE
  " hi ATECodeWork			ctermfg=brown 		cterm=none	guifg=brown		guibg=NONE


  delcommand HiLink
endif

let b:current_syntax = "ate"

" vim:ts=8
