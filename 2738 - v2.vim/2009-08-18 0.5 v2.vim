" Vim Syntax for Adept V+ as used for Staeubli Robots
" 
" Maintainer:	Soeren Sproessig, spso@hrz.tu-chemnitz.de
" Revision:		0.5
"
" CHANGELOG
"
" 09-08-18	0.5		+ added enhanced matching for CALL, * fixed matching of string format options
" 09-08-18	0.4		* fixed matching .PROGRAM with paramters
" 09-08-14	0.3		* changed color of comment, handling CASE OF / VALUE 
" 09-08-14	0.2		+ added GOTO/Labels, VALUE, string variables
" 09-08-13	0.1		first version

syn sync clear
syn sync fromstart


" Reserverd KEYWORDS
" Reference: http://www1.adept.com/main/KE/DATA/V%20Plus/V%20Language%20Reference/keyword_summary.html#7326
syn match v2CmdConvBelt "BELT\|BELT.MODE\|BSTATUS\|DEFBELT\|SETBELT\|WINDOW"
syn match v2CmdSysIO "[^F]OPEN\|AIO\.IN\|AIO\.INS\|AIO\.OUT\|ATTACH\|DETACH\|DEVICE\|DEVICES\|FCLOSE\|FCMND\|FEMPTY\|FOPEN\|FOPENA\|FOPEND\|FOPENR\|FOPENW\|FSEEK\|FSET\|GETC\|IGNORE\|IOGET\|\$IOGETS\|IOPUT\|IOSTAT\|IOTAS\|KEYMODE\|NETWORK\|PENDANT\|PROMPT\|READ\|RESET\|SETDEVICE\|SIG\|SIG\.INS\|SIGNAL\|TYPE\|WRITE"
syn match v2CmdLogicalOp "AND\|BAND\|BMASK\|BOR\|BXOR\|COM\|FALSE\|MOD\|NOT\|OFF\|ON\|TRUE\|XOR"
syn match v2CmdMotion "ABOVE\|ACCEL\|ALIGN\|ALTER\|ALTOFF\|ALTON\|ALWAYS\|APPRO\|APPROS\|AUTO\.POWER\.OFF\|BASE\|BELOW\|BRAKE\|BREAK\|CALIBRATE\|CLOSE\|CLOSEI\|COARSE\|ALWAYS\|CONFIG\|CPOFF\|CPON\|DECOMPOSE\|DELAY\|DEPART\|DEPARTS\|DEST\|DISTANCE\|DRIVE\|DURATION\|DX\|DY\|DZ\|ESTOP\|FINE\|FLIP\|FRAME\|GAIN\.SET\|HAND\|HAND\.TIME\|HERE\|HOUR\.METER\|IDENTICAL\|INRANGE\|INVERSE\|JOG\|LATCH\|LATCHED\|LEFTY\|MOVE\|MOVES\|MOVEF\|MOVESF\|MOVET\|MOVEST\|MULTIPLE\|NOFLIP\|NUNULL\|NOOVERLAP\|NULL\|OPENI\|OVERLAP\|PAYLOAD\|REACTI\|READY\|RELAX\|RELAXI\|RIGHTY\|SELECT\|SET\|SHIFT\|SINGLE\|SOLVE\.ANGLES\|SOLVE\.TRANS\|SPEED\|SPIN\|STATE\|TOOL\|TRANS\|UNIDIRECT"
syn match v2CmdNumOp "ABS\|ATAN2\|BCD\|COS\|DCB\|FRACT\|INT\|INTB\|MAX\|MIN\|OUTSIDE\|PI\|RANDOM\|SIGN\|SIN\|SQR\|SQRT\|VAL "
syn match v2CmdProgCtrl "ABORT\|ANY\|SCALE\|CALLP\|CALLS\|CLEAR\.EVENT\|CYCLE\.END\|DISPLAY\.CAMERA\|DEFINED\|DISABLE\|DOS\|DO\|ELSE\|ENABLE\|END\|ERROR\|EXECUTE\|EXIT\|FOR\|TO[^O]\|STEP\|FREE\|GET\.EVENT\|HALT\|ID\|\$ID\|GOTO\|INSTALL\|INIT\.EVENT\|KILL\|LAST\|LOCK\|MC\|MCS\|NEXT\|PANIC\|PARAMETER\|PAUSE\|PRIORITY\|REACT\|REACTE\|RELEASE\|RETURN\|RETURNE\|RUNSIG\|SEE\|SELECT\|SET\.EVENT\|STATUS\|STOP\|SWITCH\|SYMBOL\.PTR\|TAS\|TASK\|TIME\|\$TIME\|TIMER\|TPS\|UNTIL\|WAIT\|WAIT\.EVENT\|WHILE\|DO"
syn match v2CmdStringOp "ASC\|\$CHR\|\$DBLB\|DBLB\|\$DECODE\|\$ENCODE\|\$ERROR\|\$FLTB\|FLTB\|\$INTB\|LEN\|\$LNGB\|LNGB\|\$MID\|PACK\|POS\|STRDIF\|\$SYMBOL\|\$TRANSB\|TRANSB\|\$TRUNCATE\|\$UNPACK"


" general definition
syn match v2Comment ";.*$"
syn match v2Type "AUTO\|GLOBAL"
syn match v2Conditional "IF\|THEN"

syn match v2Parameters "\/[A-Za-z_]*" contained
syn region v2String start="\"" end="\"" contains=v2Parameters oneline 
syn match v2StringVar "\(\$[a-z][a-z\.]*\)\|\(\/[NDBS]\|\/[EFG]\d\+\.\d\+\|\/[HIOCUX]\d\+\)"  
syn match v2Var "[a-z][a-z0-9]*"  
syn match v2Number "[0-9]\+"
syn match v2Operator "(\|)\|\[\|\]\|,"

syn match v2End "^\.END$"

syn match v2Label "^[\t ]*[0-9]*\s" 
syn match v2Goto "GOTO \d*"

syn region v2CaseOf start="CASE " end=" OF" contains=v2Var oneline 
syn region v2Value start="VALUE " end=":" contains=v2Number oneline 
syn region v2Program start="^\.PROGRAM [a-zA-Z\.]*(" end=")$" contains=v2StringVar,v2Var,v2Operator oneline 
syn region v2Call start="CALL [a-zA-Z\.]*(" end=")$" contains=v2StringVar,v2Var,v2Operator oneline 

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_conf_syntax_inits")
  if version < 508
    let did_conf_syntax_inits = 1
    let did_html_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  
  hi  v2StringVar guifg=SeaGreen ctermfg=DarkGreen

  HiLink v2StringVar Identifier
  HiLink v2Value Statement
  HiLink v2Number Number 
  HiLink v2Operator Operator 
  hi v2Comment guifg=Gray ctermfg=Gray
  HiLink v2Type	Type
  HiLink v2CmdConvBelt		Special
  HiLink v2CmdSysIO		Special
  HiLink v2CmdLogicalOp		Special
  HiLink v2CmdMotion		Special
  HiLink v2CmdNumOp		Statement
  HiLink v2CmdProgCtrl		Statement
  HiLink v2CmdStringOp		Special
  HiLink v2Conditional		Conditional
  
  HiLink v2String		String
  HiLink v2Parameters		PreProc	
  
  HiLink v2Program		PreProc
  HiLink v2Call		PreProc
  HiLink v2End			PreProc

  hi v2Var guifg=Orange ctermfg=Red
  hi v2Label gui=bold guifg=DarkGray ctermfg=DarkGray
  hi v2Goto gui=bold,underline guifg=DarkGray ctermfg=DarkGray

  hi v2CaseOf gui=underline guifg=Purple
  hi v2Value gui=underline guifg=Purple
endif

let b:current_syntax = "v2" 
