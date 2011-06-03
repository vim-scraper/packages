" Vim syntax file
" Language:	Cube engine cfg files
" Maintainer:	Denilson F. de Sa - CrazyTerabyte (denilson@vialink.com.br)
" Last change:	2005 April 10
" Version:      1.1

if exists ("b:current_syntax")
    finish
endif

" All commands are case sensitive, but key names aren't
syn case match

" Todo
syn keyword cubeTodo contained TODO FIXME XXX

" comments
syn match cubeComment "//.*$" contains=cubeTodo

" Cube commands
syn keyword cubeConditional if
syn keyword cubeRepeat      loop while
syn keyword cubeBraces      "[()]"
syn match cubeDelimiter /;/
"TODO: add $editing and $flrceil variables
syn match cubeVariable  /\$[a-zA-Z0-9_]\+/
syn match cubeNumber    /\<[-+]\?\d\+\>/
"FIXME: The positive/negative signal on numbers isn't respected

syn keyword cubeBind        bind   skipwhite nextgroup=cubeKey
syn keyword cubeAlias       alias
syn keyword cubeNewent      newent clearents skipwhite nextgroup=cubeEntities
"TODO: add syntax rules for 'keymap'
syn match   cubeStatement   /[+\*=<>-]/
syn keyword cubeStatement   div mod strcmp echo exec name team skill showscores password connect disconnect servermenu updatefrommaster sendmap getmap savegame loadgame screenshot menuitem newmenu quit say saycommand rate conskip weapon gamespeed record stop demo demoplaybackspeed demodelaymsec demotracking edittoggle edittex editheight solid equalize heighfield vdelta corner undo undomegs copy paste replace texturereset texture slope arch archvertex perlin select trigger music registersound sound mapmodelreset mapmodel delent entproperty scalelights waterlevel edittag fullbright showmip toggleocull recalc map savemap newmap mapenlarge mapmsg fog fogcolour loadsky concat concatword at invmouse musicvol soundvol fov fpsrange minlod dynlight watersubdiv gamma lightscale hudgun hidestats maxroll crosshairsize crosshairfx undomegs attack forward left backward right jump mode showmenu sleep onrelease rnd heightfield particlesize maxparticles history lighterror
syn keyword cubeEntities    light playerstart shells bullets rockets riflerounds quaddamage health healthboost greenarmour yellowarmour teleport teledest mapmodel monster trigger jumppad

syn cluster cubeCommands contains=cubeStatement,cubeAlias,cubeBind
syn cluster cubeALL contains=ALLBUT,cubeKey,cubeEntities

" Cube game keys
syn case ignore
syn keyword cubeKey contained MOUSE1 MOUSE2 MOUSE3 MOUSE4 MOUSE5 BACKSPACE TAB CLEAR RETURN PAUSE ESCAPE SPACE EXCLAIM QUOTEDBL HASH DOLLAR AMPERSAND QUOTE LEFTPAREN RIGHTPAREN ASTERISK PLUS COMMA MINUS PERIOD SLASH 0 1 2 3 4 5 6 7 8 9 COLON SEMICOLON LESS EQUALS GREATER QUESTION AT LEFTBRACKET BACKSLASH RIGHTBRACKET CARET UNDERSCORE BACKQUOTE A B C D E F G H I J K L M N O P Q R S T U V W X Y Z DELETE KP0 KP1 KP2 KP3 KP4 KP5 KP6 KP7 KP8 KP9 KP_PERIOD KP_DIVIDE KP_MULTIPLY KP_MINUS KP_PLUS KP_ENTER KP_EQUALS UP DOWN RIGHT LEFT INSERT HOME END PAGEUP PAGEDOWN F1 F2 F3 F4 F5 F6 F7 F8 F9 F10 F11 F12 F13 F14 F15 NUMLOCK CAPSLOCK SCROLLOCK RSHIFT LSHIFT RCTRL LCTRL RALT LALT RMETA LMETA LSUPER RSUPER MODE COMPOSE HELP PRINT SYSREQ BREAK MENU
syn case match

" Level Triggers
syn match cubeTrigger "level_trigger_[0-9]\+"
" Next map
syn match cubeNextMap "nextmap_[a-zA-Z0-9_]\+"

" Strings
syn region cubeString start=/"/ end=/"/   contains=@cubeALL  oneline
syn region cubeBlock  start=/\[/ end=/\]/ contains=@cubeALL
syn region cubeParen  start=/(/  end=/)/  contains=@cubeALL
"syn region cubeBlock  start=/\[/ end=/\]/ contains=cubeBlock,cubeString,@cubeCommands


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_cube_syn_inits")
	if version < 508
		let did_cube_syn_inits = 1
		command -nargs=+ HiLink hi link <args>
	else
		command -nargs=+ HiLink hi def link <args>
	endif

	HiLink cubeTodo        Todo
	HiLink cubeComment     Comment
	HiLink cubeString      String

	HiLink cubeVariable    Identifier
	HiLink cubeTrigger     Identifier
	HiLink cubeNextMap     Identifier

	HiLink cubeNumber      Number
	HiLink cubeDelimiter   Delimiter

	HiLink cubeConditional Conditional
	HiLink cubeRepeat      Repeat

	HiLink cubeBind        Typedef
	HiLink cubeAlias       Typedef
	HiLink cubeNewent      Statement
	HiLink cubeStatement   Statement

	HiLink cubeEntities    Keyword
	HiLink cubeKey         Keyword

	delcommand HiLink
endif

let b:current_syntax = "cube"
