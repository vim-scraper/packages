
" Vim syntax file
" Language:	quantum (data tabulation language)
" Maintainer:	mattski (matt skawinski) <matt_ski@hotmail.com>
" Last change:	oct 29 1999

"remove old syntax stuff hanging around
syn keyword	qtmStatement	goto	return	reject	continue	tabs	axes	ban	banner	att
syn keyword	qtmLabel	case	default	ed	end	struct	base	netend	clear
syn keyword	qtmLabel	gt	ge	lt	le	eq	in	not	ne
syn keyword	qtmConditional	if	else	then	endif
syn keyword	qtmRepeat	while	for	do	val	
syn keyword	qtmStringsy	nz	unl1	dec	elms	tstat	clevel	grid	tab	lista	count
syn keyword	qtmStringsy	median	inc	bot	foot	col	rej	fld	id	wm	punch
syn keyword	qtmStringsy	let	num	sift	r	sp	spb	numb	propcorr
syn keyword	qtmStringsy	definelist

"strings
syn match	qtmCommentInclud	contained "[^\*] include"
syn match	qtmCommentDef	contained "[^\*] def"
syn match	qtmSetDef	contained "[^\*] set"

syn region	qtmCommentsAll	start="^*" end=+include+	contains=qtmCommentInclud
syn region	qtmCommentsAll	start="^*def" end=+ + 	contains=qtmCommentDef
syn region	qtmCommentsAll	start="^*set" end=+ + 	contains=qtmSetDef

syn region	qtmComment	start="\/\*" end=+$+
syn region	qtmn01Line	start="^n" end="[0123][1234567890]"
syn region	qtmnetLine	start="^net" end="[0-9]"
syn region	qtmnttLine	start="^ntt" end="[0-9]"
syn region	qtmConditions	start=";c=" end=+$+



if !exists("did_qtm_syntax_inits")
  let did_qtm_syntax_inits = 1
  " The default methods for highlighting.  Can be overridden later
    hi link qtmLabel	Label
    hi link qtmUserLabel	Label
    hi link qtmConditional	Conditional
    hi link qtmRepeat	Repeat
    hi link qtmCharacter	Character
    hi link qtmSpecialCharacter cSpecial
    hi link qtmStatement	Statement
    hi link cCommentError	cError
    hi link qtmCommentString cString
    hi link qtmCommentInclud cString
    hi link qtmcComment	cComment
    hi link qtmCommentsAll	String
    hi link qtmStringsy	String
    hi link qtmn01Line	String
    hi link qtmnttLine	String
    hi link qtmnetLine	Statement
    hi link qtmConditions	Statement
    hi link qtmComment	Comment
    hi link cSpecial	SpecialChar
    hi link qtmBanid	String
    hi link cTodo		Todo
    "hi link cIdentifier	Identifier
    endif

    let b:current_syntax = "qtm"

    " vim: ts=8

