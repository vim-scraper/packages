" Vim syntax file
" Language: GNOME Display Manager (gdm) configuration file gdm.conf
" Maintainer: David Ne\v{c}as (Yeti) <yeti@physics.muni.cz>
" Last Change: 2003-10-06
" URL: http://trific.ath.cx/Ftp/vim/syntax/gdm.vim

" Setup {{{
" React to possibly already-defined syntax.
" For version 5.x: Clear all syntax items unconditionally
" For version 6.x: Quit when a syntax file was already loaded
if version >= 600
  if exists("b:current_syntax")
    finish
  endif
else
  syntax clear
endif

syn case match
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" }}}
" Comments {{{
syn match gdmComment "#.*$" contains=gdmTodo
syn keyword gdmTodo TODO FIXME NOT XXX contained
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" }}}
" Base constructs and sectioning {{{
syn match gdmSection "\[.\{-}\]" contained
syn match gdmLine "^" nextgroup=gdmComment,gdmOption,gdmSection skipwhite
syn match gdmColor "#\x\{6}\>"
syn match gdmDecNum "^\@!\<\d\+\>"
syn match gdmParam "%n"
syn keyword gdmConstant true false
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" }}}
" Keywords {{{
syn keyword gdmOption AllowRemoteAutoLogin AllowRemoteRoot AllowRoot contained
syn keyword gdmOption AlwaysRestartServer AutomaticLogin contained
syn keyword gdmOption AutomaticLoginEnable BackgroundColor contained
syn keyword gdmOption BackgroundImage BackgroundProgram contained
syn keyword gdmOption BackgroundRemoteOnlyColor BackgroundScaleToFit contained
syn keyword gdmOption BackgroundType Broadcast Browser Chooser contained
syn keyword gdmOption ConfigAvailable Configurator DefaultFace contained
syn keyword gdmOption DefaultHostImg DefaultPath DisplayInitDir contained
syn keyword gdmOption DisplaysPerHost Enable Exclude FailsafeXServer contained
syn keyword gdmOption FirstVT FlexibleXServers GlobalFaceDir contained
syn keyword gdmOption GnomeDefaultSession GraphicalTheme contained
syn keyword gdmOption GraphicalThemeDir Greeter Group GtkRC contained
syn keyword gdmOption HaltCommand HonorIndirect HostImageDir Hosts contained
syn keyword gdmOption Icon KillInitClients LocalNoPasswordUsers contained
syn keyword gdmOption LocaleFile LockPosition LogDir Logo MaxIconHeight contained
syn keyword gdmOption MaxIconWidth MaxPending MaxPendingIndirect contained
syn keyword gdmOption MaxSessions MaxWait MaxWaitIndirect contained
syn keyword gdmOption MinimalUID PidFile Port PositionX PositionY contained
syn keyword gdmOption PostSessionScriptDir PreSessionScriptDir Quiver contained
syn keyword gdmOption RebootCommand RelaxPermissions RemoteGreeter contained
syn keyword gdmOption RetryDelay RootPath RunBackgroundProgramAlways contained
syn keyword gdmOption ScanTime ServAuthDir SessionDir SessionMaxFile contained
syn keyword gdmOption SetPosition ShowGnomeChooserSession contained
syn keyword gdmOption ShowGnomeFailsafeSession ShowXtermFailsafeSession contained
syn keyword gdmOption StandardXServer SuspendCommand SystemMenu contained
syn keyword gdmOption TimedLogin TimedLoginDelay TimedLoginEnable contained
syn keyword gdmOption TitleBar Use24Clock UseCirclesInEntry User contained
syn keyword gdmOption UserAuthDir UserAuthFBDir UserAuthFile contained
syn keyword gdmOption UserMaxFile VTAllocation Welcome Willing contained
syn keyword gdmOption XKeepsCrashing XineramaScreen Xnest contained
syn keyword gdmOption command flexible handled name contained
syn match gdmOption "\d\+\>" contained
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" }}}
" Define the default highlighting {{{
" For version 5.7 and earlier: Only when not done already
" For version 5.8 and later: Only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_gdm_syntax_inits")
  if version < 508
    let did_gdm_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink gdmComment           Comment
  HiLink gdmTodo              Todo
  HiLink gdmOption            Type
  HiLink gdmDecNum            gdmConstant
  HiLink gdmColor             gdmConstant
  HiLink gdmConstant          Constant
  HiLink gdmParam             Function
  HiLink gdmSection           Title
  delcommand HiLink
endif
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" }}}
let b:current_syntax = "gdm"

