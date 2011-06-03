" Vim syntax file
" Language:	Robotbattle Scripting Language
" Version:	1.0.0
" Maintainer:	Markus Jarderot <marjar-4@student.ltu.se>
" Last Change:	Sat Oct 11 18:53:00 CET 2008

" Scripting language for Robot Battle
" Based on RB v1.4.05

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore

" Function names
syn keyword rslFunction contained	abs acceltarget ahead ascanevents
syn keyword rslFunction contained	assert author back blocking bodyleft
syn keyword rslFunction contained	bodyright cldcookieevents
syn keyword rslFunction contained	cldmineevents cldmissileevents
syn keyword rslFunction contained	cldrobotevents cldwallevents clear
syn keyword rslFunction contained	cleararray clearprops clipmoves
syn keyword rslFunction contained	continue copy coreevents countarray
syn keyword rslFunction contained	countprops customevents deceltarget
syn keyword rslFunction contained	dtccookieevents dtcmineevents
syn keyword rslFunction contained	dtcrobotevents dtcwallevents endturn
syn keyword rslFunction contained	fire gethitsother gethitsself
syn keyword rslFunction contained	gethitstr getothers getpropname
syn keyword rslFunction contained	getrandom getshots gettime getturns
syn keyword rslFunction contained	gunleft gunright hasprop initialize
syn keyword rslFunction contained	isequal isnum isstr lockall lockgun
syn keyword rslFunction contained	max min msgbox name numfrom
syn keyword rslFunction contained	pingevents pophead poptail print
syn keyword rslFunction contained	printdeep pushhead pushtail radarleft
syn keyword rslFunction contained	radarright radiochannel radioevents
syn keyword rslFunction contained	radiosend regascan regcldcookie
syn keyword rslFunction contained	regcldmine regcldmissile regcldrobot
syn keyword rslFunction contained	regcldwall regcore regcustom
syn keyword rslFunction contained	regdtccookie regdtcmine regdtcrobot
syn keyword rslFunction contained	regdtcwall regping regradio round
syn keyword rslFunction contained	scan setaccel setarraycount sign
syn keyword rslFunction contained	sliding speedtarget stall stop
syn keyword rslFunction contained	stopmove stoprotate store strbreak
syn keyword rslFunction contained	strfind strfrom stricmp strins strlen
syn keyword rslFunction contained	strlower strmid strrev strspan
syn keyword rslFunction contained	strtrim strupper style suicide
syn keyword rslFunction contained	syncall syncgun truncate version
syn keyword rslFunction contained	waitfor
syn keyword rslFunction contained	sin cos tan acos asin atan atan2

" System variables
syn keyword rslSysVar contained		_accel _acceltarget _additiverotation
syn keyword rslSysVar contained		_allowradio _allowsliding _appversion
syn keyword rslSysVar contained		_arenaheight _arenawidth _author
syn keyword rslSysVar contained		_bodyaim _bodyrmn _bonusfromcookies
syn keyword rslSysVar contained		_ceasefirermn _cldbearing _cldcookie
syn keyword rslSysVar contained		_cldenergy _cldheading _cldid
syn keyword rslSysVar contained		_cldmine _cldmissile _cldrobot
syn keyword rslSysVar contained		_cldteamid _cldwall _cldwithrobots
syn keyword rslSysVar contained		_cmrate _decelstops _deceltarget
syn keyword rslSysVar contained		_distrmn _dmgfromjar _dmgfrommines
syn keyword rslSysVar contained		_dmgfromrobots _dmgfromwalls
syn keyword rslSysVar contained		_dmgtorobots _dtcbearing _dtccookie
syn keyword rslSysVar contained		_dtcenergy _dtcheading _dtcid
syn keyword rslSysVar contained		_dtcmine _dtcrobot _dtcteamid
syn keyword rslSysVar contained		_dtcwall _energy _false _fastmissiles
syn keyword rslSysVar contained		_gamenbr _games _gunaim _gunheat
syn keyword rslSysVar contained		_gunrmn _heading _hitsfromrobots
syn keyword rslSysVar contained		_hitstocookies _hitstomines
syn keyword rslSysVar contained		_hitstorobots _hitstrtorobots _id
syn keyword rslSysVar contained		_killsfromerrors _killsfrommines
syn keyword rslSysVar contained		_killsfrommisc _killsfromrobots
syn keyword rslSysVar contained		_killsfromtimeouts _killsfromwalls
syn keyword rslSysVar contained		_killstorobots _line _matchname
syn keyword rslSysVar contained		_missilerange _movermn _moving _name
syn keyword rslSysVar contained		_off _on _ping _pingbearing
syn keyword rslSysVar contained		_pingfriendly _pingheading _place
syn keyword rslSysVar contained		_points _radaraim _radarrange
syn keyword rslSysVar contained		_radarrmn _radio _radiocomm _result
syn keyword rslSysVar contained		_robotsalive _robotstotal _rotating
syn keyword rslSysVar contained		_scandist _scandistfc _shotsfired
syn keyword rslSysVar contained		_speedtarget _stack _teamid
syn keyword rslSysVar contained		_teammembersalive _teammemberstotal
syn keyword rslSysVar contained		_teamname _teamsalive _teamstotal
syn keyword rslSysVar contained		_timeoutin _true _turnmode _turns
syn keyword rslSysVar contained		_updatedaccel _velocity _version
syn keyword rslSysVar contained		_xpos _ypos
syn cluster rslExpr			add=rslSysVar

" Only system properties
syn keyword rslProperty contained	_msg _bearing

" User variables
syn match   rslUserVar contained	"\<\a\w*\>"
syn cluster rslExpr			add=rslUserVar

" Expressions
syn match   rslNumber contained		"\d\+"
syn match   rslNumber contained		"\d\+\.\d*"
syn match   rslNumber contained		"-\d\+"
syn match   rslNumber contained		"-\d\+\.\d*"
syn region  rslString contained		start=+"+  end=+"+
syn keyword rslConstant contained	true false on off
syn match   rslOperator contained	"[<>]=\?\|[=!]=\|[<]>\|&&\|||\|[%^*/+-]"
syn match   rslOperator contained	"\." nextgroup=rslProperty
syn keyword rslOperator contained	and or
syn match   rslIndex contained		"\[[^[\]]\+\]" contains=@rslExpr
syn cluster rslExpr			add=rslNumber,rslString,rslConstant,rslOperator,rslIndex

" Parenthesis
syn region  rslParens contained		start="(" end=")" transparent
syn cluster rslExpr			add=rslParens

" Statements
syn match   rslSimpleStmts contained	"^\s*\(break\|else\|endif\|endw\|endwhile\|next\|return\)\>" nextgroup=rslError
syn match   rslAssignment contained	"^\s*\w\+\s*=[^#]*" contains=rslUservar,@rslExpr
syn match   rslFuncCall contained	"^\s*\zs\w\+\ze(" contains=rslStmtNames,rslFunction,rslInvalid nextgroup=rslFuncArgs
syn region  rslFuncArgs contained	matchgroup=rslStmtParens start="(" end=")" contains=@rslExpr oneline skipwhite nextgroup=rslError

syn keyword rslStmtNames contained	if elseif while gosub break else endif endw endwhile next return
syn cluster rslStatements		add=rslFuncCall,rslSimpleStmts,rslAssignment

" Syntax errors
syn region rslError contained		start="[^# \t]" end="\s*#\|\s*$"me=s-1 display oneline keepend nextgroup=rslComment
syn match  rslInvalid contained		"\w\+"

" Comments
syn keyword rslTodo contained		TODO XXX FIXME
syn region  rslComment			start="#" end="$" oneline contains=rslTodo,@Spell

" Sections
syn region  rslSection			matchgroup=rslSectionHeader start="^\w\+\_s\+{" end="}" transparent contains=@rslStatements,rslComment,@rslExpr
syn sync match rslSectionSync		grouphere rslSection "^\w\+\_s\+{"

syn case match

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_basic_syntax_inits")
  if version < 508
    let did_basic_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink rslNumber		Number
  HiLink rslString		String
  HiLink rslConstant		Constant
  HiLink rslOperator		Operator
  HiLink rslIndex		Operator
  HiLink rslUserVar		Identifier

  HiLink rslSysVar		Special
  HiLink rslProperty		Special
  HiLink rslFunction		Special

  HiLink rslFuncCall		Statement
  HiLink rslStmtNames		Statement
  HiLink rslAssignment		Statement
  HiLink rslSimpleStmts		Statement
  HiLink rslStmtParens		Statement

  HiLink rslSectionHeader	Statement

  HiLink rslError		Error
  HiLink rslInvalid		Identifier

  HiLink rslComment		Comment
  HiLink rslTodo		Todo

  delcommand HiLink
endif

let b:current_syntax = "rsl"

" vim: noet sw=2 ts=8
