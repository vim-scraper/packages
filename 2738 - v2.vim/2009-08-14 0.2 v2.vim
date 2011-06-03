" Vim Syntax for Adept V+ as used for Staeubli Robots
" 
" Maintainer: Soeren Sproessig, spso@hrz.tu-chemnitz.de
" Revision: $Id$
"
" Regards to http://www.voip-info.org/wiki/view/vim+syntax+highlighting

syn sync clear
syn sync fromstart


" Reserverd KEYWORDS
" Reference: http://www1.adept.com/main/KE/DATA/V%20Plus/V%20Language%20Reference/keyword_summary.html#7326
syn match v2CmdConvBelt "BELT\|BELT.MODE\|BSTATUS\|DEFBELT\|SETBELT\|WINDOW"
syn match v2CmdSysIO "[^F]OPEN\|AIO\.IN\|AIO\.INS\|AIO\.OUT\|ATTACH\|DETACH\|DEVICE\|DEVICES\|FCLOSE\|FCMND\|FEMPTY\|FOPEN\|FOPENA\|FOPEND\|FOPENR\|FOPENW\|FSEEK\|FSET\|GETC\|IGNORE\|IOGET\|\$IOGETS\|IOPUT\|IOSTAT\|IOTAS\|KEYMODE\|NETWORK\|PENDANT\|PROMPT\|READ\|RESET\|SETDEVICE\|SIG\|SIG\.INS\|SIGNAL\|TYPE\|WRITE"
syn match v2CmdLogicalOp "AND\|BAND\|BMASK\|BOR\|BXOR\|COM\|FALSE\|MOD\|NOT\|OFF\|ON\|TRUE\|XOR"
syn match v2CmdMotion "ABOVE\|ACCEL\|ALIGN\|ALTER\|ALTOFF\|ALTON\|ALWAYS\|APPRO\|APPROS\|AUTO\.POWER\.OFF\|BASE\|BELOW\|BRAKE\|BREAK\|CALIBRATE\|CLOSE\|CLOSEI\|COARSE\|ALWAYS\|CONFIG\|CPOFF\|CPON\|DECOMPOSE\|DELAY\|DEPART\|DEPARTS\|DEST\|DISTANCE\|DRIVE\|DURATION\|DX\|DY\|DZ\|ESTOP\|FINE\|FLIP\|FRAME\|GAIN\.SET\|HAND\|HAND\.TIME\|HERE\|HOUR\.METER\|IDENTICAL\|INRANGE\|INVERSE\|JOG\|LATCH\|LATCHED\|LEFTY\|MOVE\|MOVES\|MOVEF\|MOVESF\|MOVET\|MOVEST\|MULTIPLE\|NOFLIP\|NUNULL\|NOOVERLAP\|NULL\|OPENI\|OVERLAP\|PAYLOAD\|REACTI\|READY\|RELAX\|RELAXI\|RIGHTY\|SELECT\|SET\|SHIFT\|SINGLE\|SOLVE\.ANGLES\|SOLVE\.TRANS\|SPEED\|SPIN\|STATE\|TOOL\|TRANS\|UNIDIRECT"
syn match v2CmdNumOp "ABS\|ATAN2\|BCD\|COS\|DCB\|FRACT\|INT\|INTB\|MAX\|MIN\|OUTSIDE\|PI\|RANDOM\|SIGN\|SIN\|SQR\|SQRT\|VAL "
syn match v2CmdProgCtrl "ABORT\|ANY\|SCALE\|CALL\|CALLP\|CALLS\|CASE.*OF\|CLEAR\.EVENT\|CYCLE\.END\|DISPLAY\.CAMERA\|DEFINED\|DISABLE\|DOS\|DO\|ELSE\|ENABLE\|END\|ERROR\|EXECUTE\|EXIT\|FOR\|TO[^O]\|STEP\|FREE\|GET\.EVENT\|HALT\|ID\|\$ID\|GOTO\|INSTALL\|INIT\.EVENT\|KILL\|LAST\|LOCK\|MC\|MCS\|NEXT\|PANIC\|PARAMETER\|PAUSE\|PRIORITY\|REACT\|REACTE\|RELEASE\|RETURN\|RETURNE\|RUNSIG\|SEE\|SELECT\|SET\.EVENT\|STATUS\|STOP\|SWITCH\|SYMBOL\.PTR\|TAS\|TASK\|TIME\|\$TIME\|TIMER\|TPS\|UNTIL\|WAIT\|WAIT\.EVENT\|WHILE\|DO"
syn match v2CmdStringOp "ASC\|\$CHR\|\$DBLB\|DBLB\|\$DECODE\|\$ENCODE\|\$ERROR\|\$FLTB\|FLTB\|\$INTB\|LEN\|\$LNGB\|LNGB\|\$MID\|PACK\|POS\|STRDIF\|\$SYMBOL\|\$TRANSB\|TRANSB\|\$TRUNCATE\|\$UNPACK"


" general definition
syn match v2Comment ";.*$"
syn match v2Type "AUTO\|GLOBAL"
syn match v2Conditional "IF\|THEN"

syn match v2Parameters "\/[A-Za-z_]*" contained
syn region v2String start="\"" end="\"" contains=v2Parameters oneline 
syn match v2StringVar "\$[a-z][a-z\.]*"  

syn match v2Program "^\.PROGRAM .*()$"
syn match v2End "^\.END$"

syn match v2Value "VALUE .*:"
syn match v2Label "^[\t ]*[0-9]*\s" 
syn match v2Goto "GOTO \d*"



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
  HiLink v2Comment Comment
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
  HiLink v2End			PreProc

  hi v2Label guifg=Gray ctermfg=Gray
  hi v2Goto gui=underline guifg=Gray ctermfg=Gray
 
  
  hi v2Value gui=underline guifg=SeaGreen  
endif

let b:current_syntax = "v2" 
