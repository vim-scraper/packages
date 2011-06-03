" Vim syntax file
" Language:		bblean rc file (*.rc)
" Maintainer:	token<token@positrium.org>
" Last Change:	22-May-2009
" Filenames:	.rc
" Version:		1.1


if version < 600
	syntax clear
elseif exists ("b:current_syntax")
	unlet b:current_syntax
	finish
endif

" Case sensitive
syn case match

" {{{ syntax def
" {{{ comment chars
if ! exists ("bblean_comments")
	syn match bbleanComment "#.*"
	syn match bbleanComment "!.*"

endif
" }}}

" {{{ menu functions
if ! exists ("bblean_menu_functions")
	syn region bbleanBIF start="\[" end="\]" contains=bbleanBIFKey,bbleanBIFSub,bbleanBIFSep,bbleanBIFExe 
	
	syn keyword bbleanBIFKey path contained
	syn keyword bbleanBIFExe exec contained
	syn keyword bbleanBIFKey edit contained
	syn keyword bbleanBIFSub submenu contained
	syn keyword bbleanBIFSep separator contained

endif
" }}}

" {{{ menu properties 
if ! exists ("bblean_menu_properties")
	syn region bbleanLabel start="(" end=")"

	syn region bbleanCommand start="{" end="}" contains=bbleanKeyword
	syn keyword bbleanKeyword ADMINTOOLS contained
	syn keyword bbleanKeyword ALTSTARTUP contained
	syn keyword bbleanKeyword APPDATA contained
	syn keyword bbleanKeyword BITBUCKET contained
	syn keyword bbleanKeyword COMMON_ADMINTOOLS contained
	syn keyword bbleanKeyword COMMON_ALTSTARTUP contained
	syn keyword bbleanKeyword COMMON_APPDATA contained
	syn keyword bbleanKeyword COMMON_DESKTOPDIRECTORY contained
	syn keyword bbleanKeyword COMMON_DOCUMENTS contained
	syn keyword bbleanKeyword COMMON_FAVORITES contained
	syn keyword bbleanKeyword COMMON_PROGRAMS contained
	syn keyword bbleanKeyword COMMON_STARTMENU contained
	syn keyword bbleanKeyword COMMON_STARTUP contained
	syn keyword bbleanKeyword COMMON_TEMPLATES contained
	syn keyword bbleanKeyword COMMON_TEMPLATES contained
	syn keyword bbleanKeyword CONNECTIONS contained
	syn keyword bbleanKeyword CONTROLS contained
	syn keyword bbleanKeyword COOKIES contained
	syn keyword bbleanKeyword DESKTOP contained
	syn keyword bbleanKeyword DESKTOPDIRECTORY contained
	syn keyword bbleanKeyword DRIVES contained
	syn keyword bbleanKeyword FAVORITES contained
	syn keyword bbleanKeyword FONTS contained
	syn keyword bbleanKeyword HISTORY contained
	syn keyword bbleanKeyword INTERNET contained
	syn keyword bbleanKeyword INTERNET_CACHE contained
	syn keyword bbleanKeyword LOCAL_APPDATA contained
	syn keyword bbleanKeyword MYPICTURES contained
	syn keyword bbleanKeyword NETHOOD contained
	syn keyword bbleanKeyword NETWORK contained
	syn keyword bbleanKeyword PERSONAL contained
	syn keyword bbleanKeyword PRINTERS contained
	syn keyword bbleanKeyword PRINTHOOD contained
	syn keyword bbleanKeyword PROFILE contained
	syn keyword bbleanKeyword PROGRAM_FILES contained
	syn keyword bbleanKeyword PROGRAM_FILES_COMMON contained
	syn keyword bbleanKeyword PROGRAM_FILES_COMMONX86 contained
	syn keyword bbleanKeyword PROGRAM_FILESX86 contained
	syn keyword bbleanKeyword PROGRAMS contained
	syn keyword bbleanKeyword RECENT contained
	syn keyword bbleanKeyword SENDTO contained
	syn keyword bbleanKeyword STARTMENU contained
	syn keyword bbleanKeyword STARTUP contained
	syn keyword bbleanKeyword SYSTEM contained
	syn keyword bbleanKeyword SYSTEMX86 contained
	syn keyword bbleanKeyword TEMPLATES contained
	syn keyword bbleanKeyword WINDOWS contained
	syn keyword bbleanKeyword BLACKBOX contained

endif
" }}}

" {{{ setting parameters
if ! exists ("bblean_setting_parameters")

	syn keyword Keyword false true style default
	syn keyword Keyword BottomLeft BottomRight BottomCenter
	syn keyword Keyword TopLeft TopRight TopCenter
	syn keyword Keyword Vertical Horizontal
	syn match bbleanBroam	/@[^ ]\+/
	
	" extension.rc
	syn match bbleanKeyword /^blackbox\.editor\:/
	syn match bbleanKeyword /^blackbox\.appearance\.bullet\.unix\:/
	syn match bbleanKeyword /^blackbox\.appearance\.arrow\.unix\:/
	syn match bbleanKeyword /^blackbox\.appearance\.cursor\.usedefault\:/
	syn match bbleanKeyword /^blackbox\.background\.enabled\:/
	syn match bbleanKeyword /^blackbox\.background\.smartWallpaper\:/
	syn match bbleanKeyword /^blackbox\.background\.rootCommand\:/
	syn match bbleanKeyword /^blackbox\.workspaces\.altMethod\:/
	syn match bbleanKeyword /^blackbox\.workspaces\.followActive\:/
	syn match bbleanKeyword /^blackbox\.workspaces\.restoreToCurrent\:/
	syn match bbleanKeyword /^blackbox\.workspaces\.followMoved\:/
	syn match bbleanKeyword /^blackbox\.desktop\.marginTop\:/
	syn match bbleanKeyword /^blackbox\.desktop\.marginBottom\:/
	syn match bbleanKeyword /^blackbox\.desktop\.marginLeft\:/
	syn match bbleanKeyword /^blackbox\.desktop\.marginRight\:/
	syn match bbleanKeyword /^blackbox\.snap\.threshold\:/
	syn match bbleanKeyword /^blackbox\.snap\.padding\:/
	syn match bbleanKeyword /^blackbox\.snap\.toPlugins\:/
	syn match bbleanKeyword /^blackbox\.desktop\.AltMidClick\:/
	syn match bbleanKeyword /^blackbox\.desktop\.AltRightClick\:/
	syn match bbleanKeyword /^blackbox\.global\.fonts\.enabled\:/
	syn match bbleanKeyword /^blackbox\.global\.toolbar\.font\:/
	syn match bbleanKeyword /^blackbox\.global\.toolbar\.fontHeight\:/
	syn match bbleanKeyword /^blackbox\.global\.toolbar\.fontWeight\:/
	syn match bbleanKeyword /^blackbox\.global\.menu\.title\.font\:/
	syn match bbleanKeyword /^blackbox\.global\.menu\.title\.fontHeight\:/
	syn match bbleanKeyword /^blackbox\.global\.menu\.title\.fontWeight\:/
	syn match bbleanKeyword /^blackbox\.global\.menu\.frame\.font\:/
	syn match bbleanKeyword /^blackbox\.global\.menu\.frame\.fontHeight\:/
	syn match bbleanKeyword /^blackbox\.global\.menu\.frame\.fontWeight\:/
	syn match bbleanKeyword /^blackbox\.global\.window\.font\:/
	syn match bbleanKeyword /^blackbox\.global\.window\.fontHeight\:/
	syn match bbleanKeyword /^blackbox\.global\.window\.fontWeight\:/
	syn match bbleanKeyword /^blackbox\.desktop\.RightLeftClick\:/
	syn match bbleanKeyword /^blackbox\.desktop\.LeftRightClick\:/
	syn match bbleanKeyword /^blackbox\.desktop\.CtrlWheelUp\:/
	syn match bbleanKeyword /^blackbox\.desktop\.CtrlWheelDown\:/
	syn match bbleanKeyword /^blackbox\.desktop\.ShiftWheelUp\:/
	syn match bbleanKeyword /^blackbox\.desktop\.ShiftWheelDown\:/
	syn match bbleanKeyword /^blackbox\.menu\.VolumeHeight\:/
	syn match bbleanKeyword /^blackbox\.menu\.VolumeHilite\:/
	syn match bbleanKeyword /^blackbox\.menu\.separatorMargin\:/
	syn match bbleanKeyword /^blackbox\.menu\.KeyBindVI\:/
	syn match bbleanKeyword /^blackbox\.menu\.IconSize\:/
	syn match bbleanKeyword /^blackbox\.menu\.MaxHeightRatio\:/
	syn match bbleanKeyword /^blackbox\.recent\.menuFile\:/
	syn match bbleanKeyword /^blackbox\.recent\.itemMax\:/
	syn match bbleanKeyword /^blackbox\.recent\.withBeginEnd\:/
	syn match bbleanKeyword /^blackbox\.recent\.sortbyFrequency\:/

	" blackbox.rc
	syn match bbleanKeyword /^session\.screen[0-9]\+\.toolbar\.enabled\:/
	syn match bbleanKeyword /^session\.screen[0-9]\+\.toolbar\.placement\:/
	syn match bbleanKeyword /^session\.screen[0-9]\+\.toolbar\.widthPercent\:/
	syn match bbleanKeyword /^session\.screen[0-9]\+\.toolbar\.onTop\:/
	syn match bbleanKeyword /^session\.screen[0-9]\+\.toolbar\.autoHide\:/
	syn match bbleanKeyword /^session\.screen[0-9]\+\.toolbar\.pluginToggle\:/
	syn match bbleanKeyword /^session\.screen[0-9]\+\.toolbar\.alpha\.enabled\:/
	syn match bbleanKeyword /^session\.screen[0-9]\+\.toolbar\.alpha\.value\:/
	syn match bbleanKeyword /^session\.menu\.position\.x\:/
	syn match bbleanKeyword /^session\.menu\.position\.y\:/
	syn match bbleanKeyword /^session\.menu\.maxWidth\:/
	syn match bbleanKeyword /^session\.menu\.popupDelay\:/
	syn match bbleanKeyword /^session\.menu\.mouseWheelFactor\:/
	syn match bbleanKeyword /^session\.menu\.alpha\.enabled\:/
	syn match bbleanKeyword /^session\.menu\.alpha\.value\:/
	syn match bbleanKeyword /^session\.menu\.onTop\:/
	syn match bbleanKeyword /^session\.menu\.snapWindow\:/
	syn match bbleanKeyword /^session\.menu\.pluginToggle\:/
	syn match bbleanKeyword /^session\.menu\.bulletPosition\:/
	syn match bbleanKeyword /^session\.menu\.sortbyExtension\:/
	syn match bbleanKeyword /^session\.screen[0-9]\+\.slit\.placement\:/
	syn match bbleanKeyword /^session\.screen[0-9]\+\.slit\.direction\:/
	syn match bbleanKeyword /^session\.screen[0-9]\+\.slit\.autoHide\:/
	syn match bbleanKeyword /^session\.screen[0-9]\+\.slit\.ontop\:/
	syn match bbleanKeyword /^session\.screen[0-9]\+\.slit\.toggle\:/
	syn match bbleanKeyword /^session\.screen[0-9]\+\.workspaces\:/
	syn match bbleanKeyword /^session\.screen[0-9]\+\.workspaceNames\:/
	syn match bbleanKeyword /^session\.screen[0-9]\+\.focusModel\:/
	syn match bbleanKeyword /^session\.screen[0-9]\+\.fullMaximization\:/
	syn match bbleanKeyword /^session\.screen[0-9]\+\.strftimeformat\:/
	syn match bbleanKeyword /^session\.autoraisedelay\:/
	syn match bbleanKeyword /^session\.opaqueMove\:/
	syn match bbleanKeyword /^session\.imageDither\:/
	syn match bbleanKeyword /^session\.menufile\:/
	syn match bbleanKeyword /^session\.styleFile\:/
	
	syn match Number /[\-\+]\?[0-9]\+/


endif
" }}}

" }}}

" {{{ color def
if version >= 508 || !exists ("did_bblean_inits")
	if version < 508
		let did_bblean_inits = 1
		command -nargs=+ HiLink hi link <args>
	else
		command -nargs=+ HiLink hi def link <args>
	endif

" {{{ comment chars
	HiLink bbleanComment Comment
" }}}

" {{{ menu functions
	HiLink bbleanBIF Function
	HiLink bbleanBIFKey Type
	HiLink bbleanBIFSub Operator
	HiLink bbleanBIFSep Number
	HiLink bbleanBIFExe Keyword
" }}}

" {{{ menu properties
	HiLink bbleanLabel Ignore
	HiLink bbleanCommand Keyword
	HiLink bbleanKeyword Type
" }}}

" {{{ setting parameters
	HiLink bbleanBroam Operator
	
" }}}

	delcommand HiLink
endif

" }}}
let b:current_syntax = "bblean"
" vim:fdm=marker
