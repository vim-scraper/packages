" vi:tw=0:
"
" $Id: rsl.vim,v 1.18 2005/06/28 15:01:46 jettero Exp $
"
" Intended for use with RSL scripts from droidarena.com
"
" try this:
" au BufNewFile,BufRead *.rsl setf rsl
"
"   -paul
"

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" ---------------------------------------------------------------------------------------------------------

syn case ignore " There is no case, there is only zuul

syn match rslStaticData "." contains=ALL

" ---------------------------------------------------------------------------------------------------------
" command/function args
"
syn cluster	rslSpanCluster	contains=rslError,rslRegister,rslUserRegister,rslBEvent,rslLogSpecial

syn match rslError "#[a-z0-9]\+"
syn match rslError "&[a-z0-9]\+"

syn match rslRegister     "#\<\(me\|mp\|le\|ri\|fw\|bk\|mb\|dl\|id\|ie\|ps\|bh\|ms\|nr\|bl\)\>"
syn match rslUserRegister "&\<\(id\|ps\|bh\|ms\|nr\|bl\)[0-9]\>"
syn match rslUserSection  "[[][^]]\+[]]"

" ---------------------------------------------------------------------------------------------------------
" branches
"

" there are five types of branch/condtionals:  
" s (sub), ts (test sub), tss (test sub sub), tts, ttss
" note: we treat ts and tss identically
" note: we treat tts and ttss identically

syn match rslBranch "\(^\s*\(CALL\)\s\+\)\@<=[^;]*" contains=@rslSpanCluster " s
syn match rslBranch "\(^\s*\(IFBF\|IFBT\|IFFR\|IFIN\|IFNF\|IFNV\|IFVA\|IFVI\)\s\+\S\+\s\+\)\@<=[^;]*" contains=@rslSpanCluster " ts
syn match rslBranch "\(^\s*\(FRND\|TEST\|VISI\)\s\+\S\+\s\+\)\@<=[^;]*" " tss
syn match rslBranch "\(^\s*\(IFEQ\|IFGT\|IFHI\|IFLO\|IFLT\|IFNE\)\s\+\S\+\s\+\S\+\s\+\)\@<=[^;]*" contains=@rslSpanCluster " tts
syn match rslBranch "\(^\s*\(ISEQ\)\s\+\S\+\s\+\S\+\s\+\)\@<=[^;]*" contains=@rslSpanCluster " ttss

" The following line is a super lame way to match event branches, but I'm apparently not very good at vimscript.
syn match rslBEvent     "\(\(CALL\|IFBF\|IFBT\|IFFR\|IFIN\|IFNF\|IFNV\|IFVA\|IFVI\|FRND\|TEST\|VISI\|IFEQ\|IFGT\|IFHI\|IFLO\|IFLT\|IFNE\|ISEQ\).*\)\@<=\<\(ATKD\|ATXXX\|DLIM\|DLOS\|DPIK\|DOLL\|EAPP\|HELP\|INIT\|MSGR\|NEXT\|POSI\|RAPP\|SEEP\|SEEE\|STEP\|WALL\)\>"
syn match rslLogSpecial "\(LOG\s\+\)\@<=\<\(HP\|BOTPOS\|TICK\)\>"

" their list {{{
" [s   ] CALL <Subroutine>
" [ts  ] IFBF <Boolean> <TrueSUB>
" [ts  ] IFBT <Boolean> <TrueSUB>
" [ts  ] IFFR <RobotId> <TrueSUB>
" [ts  ] IFIN <Register> <TrueSUB>
" [ts  ] IFNF <RobotId> <TrueSUB>
" [ts  ] IFNV <RobotId> <TrueSUB>
" [ts  ] IFVA <Register> <TrueSUB>
" [ts  ] IFVI <RobotId> <TrueSUB>
" [tss ] FRND <RobotId> <AllySub> <EnemySub>
" [tss ] TEST <Register> <ValidSUB> <InvalidSUB>
" [tss ] VISI <RobotId> <VisibleSUB> <NotVisibleSUB>
" [tts ] IFEQ <Register1> <Register2> <TrueSUB>
" [tts ] IFGT <Numeric1> <Numeric2> <TrueSUB>
" [tts ] IFHI <RobotId1> <RobotId2> <TrueSUB>
" [tts ] IFLO <RobotId1> <RobotId2> <TrueSUB>
" [tts ] IFLT <Numeric1> <Numeric2> <TrueSUB>
" [tts ] IFNE <Register1> <Register2> <TrueSUB>
" [ttss] ISEQ <Register1> <Register2> <TrueSUB> <FalseSUB>
" }}}

" ---------------------------------------------------------------------------------------------------------
" RSL comands, keywords, etc...
"

syn match rslError       "^\s*\<[A-Z]\+\>"
                        
syn match rslEvent       "[[]\<\(ATKD\|ATXXX\|DLIM\|DLOS\|DPIK\|DOLL\|EAPP\|HELP\|INIT\|MSGR\)\>[]]"
syn match rslEvent       "[[]\<\(NEXT\|POSI\|RAPP\|SEEP\|SEEE\|STEP\|WALL\)\>[]]"
                        
syn match rslFunction    "^\s*\<\(_ADD\|_DIV\|_MUL\|_SET\|_SUB\|ALLY\|BSET\|CNAM\|COPY\|CPID\|CPOS\)\>"
syn match rslFunction    "^\s*\<\(GATR\|GDIS\|GPOS\|GRPS\|INVA\|LSMG\|LRID\|NEAR\|QBHV\|SNMY\)\>"
syn match rslProperty    "^\s*\<\(ATPR\|BHVR\|DMLT\)\>"

syn match rslError       "\(\<\(_SUB\|_ADD\|_DIV\|_MUL\)\>.*\)\@<=\<[0-9]\+\>" " you can't do math on numbers (lame)
                        
syn match rslCommand     "^\s*\<\(AAID\|ATAK\|ESCP\|FRND\|FLEE\|FOLL\|GOTO\|HALT\)\>"
syn match rslCommand     "^\s*\<\(SOS\|TURN\|VAL\|WAIT\|LOG\|SHOT\|LMSG\|SMSG\)\>"

syn match rslConditional "^\s*\<\(IFBF\|IFBT\|IFEQ\|IFFR\|IFGT\|IFHI\|IFIN\|IFLO\|IFLT\)\>"
syn match rslConditional "^\s*\<\(IFNE\|IFNF\|IFNV\|IFVA\|IFVI\|ISEQ\|VISI\|TEST\|CALL\)\>" " includes the branch

" ---------------------------------------------------------------------------------------------------------
" additional user spew
"
syn match rslComment ";.*$" contains=rslTodo
syn match rslError   "\(^\s*[[].*[]]\s*\)\@<=;.*$" " you can't put a comment on section line, cuz the section won't execute (lame)
syn match rslTodo    "\(FIXME\|TODO\)"

" ---------------------------------------------------------------------------------------------------------
" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_pike_syntax_inits")
  if version < 508
    let did_pike_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink rslError Error

  HiLink rslFunction    Statement
  HiLink rslProperty    Statement
  HiLink rslCommand     Statement
  HiLink rslConditional Conditional

  HiLink rslEvent       Type
  HiLink rslBEvent      Type
  HiLink rslUserSection PreProc
  HiLink rslBranch      PreProc

  HiLink rslRegister     Special
  HiLink rslUserRegister Identifier

 "HiLink rslStaticData   String 
  HiLink rslLogSpecial   String

  HiLink rslComment      Comment
  HiLink rslTodo         Todo

  delcommand HiLink
endif

let b:current_syntax = "rsl"
