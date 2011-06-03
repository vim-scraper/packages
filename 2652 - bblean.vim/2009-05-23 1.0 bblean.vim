" Vim syntax file
" Language:		bblean rc file (*.rc)
" Maintainer:	token<token@positrium.org>
" Last Change:	22-May-2009
" Filenames:	.rc


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

	syn keyword Keyword false true style
	syn match bbleanBroam	/@[^ ]\+/
	syn match bbleanKeyword /[^!#]\+[^:]\+:/
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
