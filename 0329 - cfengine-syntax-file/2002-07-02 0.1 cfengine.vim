" cfengine syntax file
" Filename:     cfengine.vim
" Language:     cfengine configuration file ( /var/cfengine/inputs/cf.* )
" Maintainer:   Marcus Spading <ms@fragmentum.net>
" URL:          http://fragmentum.net/vim/syntax/cfengine.vim
" Last Change:  2002 Jul 03
" Version:      0.1
"
" cfengine action
" action-type:
"   compound-class::
"       declaration

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore

syn match   cfengineCompoundClass      "^\s*.*::$"
syn match   cfengineAssignmentOperator "="
syn match   cfengineLinkOperator       "->"
syn match   cfengineLinkOperator       "->!"
syn match   cfengineVariable           "\$(.\{-})"
syn region  cfengineVariableDef start="(\s*" end="\s*)" contains=cfengineKeyword,cfengineHelpers
syn match   cfengineNumber             "\<\d\+\|inf\>"
syn region  cfengineBlock  start="{" end="}" contains=cfengineBlock,cfengineEditAction,cfengineString
syn region  cfengineString start=+"+ skip=+\\\\\|\\"+ end=+"+ oneline contains=cfengineVariable containedin=cfengineBlock
syn region  cfengineString start=+'+ skip=+\\\\\|\\'+ end=+'+ oneline contains=cfengineVariable containedin=cfengineBlock

syn keyword cfengineBoolean    on off true false 

syn keyword cfengineKeyword    access actionsequence AddClasses AddInstallable BinaryPaddingChar
syn keyword cfengineKeyword    ChecksumDatabase ChecksumUpdates ChildLibPath CopyLinks DefaultCopyType
syn keyword cfengineKeyword    DeleteNonUserFiles DeleteNonOwnerFiles DeleteNonUserMail DeleteNonOwnerMail
syn keyword cfengineKeyword    domain DryRun EditBinaryFileSize EditFileSize EmptyResolvConf Exclamation
syn keyword cfengineKeyword    ExcludeCopy ExcludeLinks ExpireAfter HomePattern IfElapsed Inform InterfaceName
syn keyword cfengineKeyword    FileExtensions LinkCopies LogDirectory LogTidyHomeFiles moduledirectory mountpattern
syn keyword cfengineKeyword    netmask NonAlphaNumFiles nfstype RepChar Repository Schedule SecureInput
syn keyword cfengineKeyword    SensibleCount SensibleSize ShowActions site faculty SplayTime Split smtpserver
syn keyword cfengineKeyword    SpoolDirectories SuspiciousNames sysadm Syslog timezone TimeOut Verbose
syn keyword cfengineKeyword    Warnings WarnNonUserFiles WarnNonOwnerFiles WarnNonUserMail WarnNonOwnerMail
syn region  cfengineHelpers matchgroup=cfengineKeyword start="FileExists(" end=")" contained oneline 
syn region  cfengineHelpers matchgroup=cfengineKeyword start="IsDir(" end=")" contained oneline 
syn region  cfengineHelpers matchgroup=cfengineKeyword start="IsNewerThan(" end=")" contained oneline 

syn keyword cfengineOption     age dest m[ode] o[wner] g[roup] act[ion] silent fix preserve keep backup repository stealth timestamps
syn keyword cfengineOption     symlink incl[ude] excl[ude] ignore filter r[ecurse] type linktype typecheck define elsedefine
syn keyword cfengineOption     force forcedirs forceipv4 size server trustkey encrypt verify oldserver purge syslog inform
syn keyword cfengineOption     pat[tern] rotate flags links stop traverse tidy checksum

syn keyword cfengineAction     warnall warndirs warnplain
syn keyword cfengineAction     fixall fixdirs fixplain
syn keyword cfengineAction     touch linkchildren create compress alert

syn keyword cfengineEditAction AbortAtLineMatching Append contained
syn keyword cfengineEditAction AppendIfNoSuchLine AppendIfNoLineMatching AppendToLineIfNotContains contained
syn keyword cfengineEditAction AutoCreate AutomountDirectResources Backup contained
syn keyword cfengineEditAction BeginGroupIfDefined BeginGroupIfNotDefined contained
syn keyword cfengineEditAction BeginGroupIfFileExists BeginGroupIfFileIsNewer contained
syn keyword cfengineEditAction BeginGroupIfNoLineContaining BeginGroupIfNoLineMatching BeginGroupIfNoMatch BeginGroupIfNoSuchLine contained
syn keyword cfengineEditAction BreakIfLineMatches CatchAbort contained
syn keyword cfengineEditAction CommentLinesMatching CommentLinesStarting CommentNLines CommentToLineMatching contained
syn keyword cfengineEditAction DefineClasses DeleteLinesAfterThisMatching DeleteLinesContaining DeleteLinesMatching contained
syn keyword cfengineEditAction DeleteLinesStarting DeleteNLines DeleteToLineMatching contained
syn keyword cfengineEditAction EditMode EmptyEntireFilePlease ElseDefineClasses EndGroup EndLoop contained
syn keyword cfengineEditAction Filter FixEndOfLines ForEachLineIn GotoLastLine contained 
syn keyword cfengineEditAction HashCommentLinesContaining HashCommentLinesMatching HashCommentLinesStarting contained
syn keyword cfengineEditAction IncrementPointer Inform InsertFile InsertLine LocateLineMatching contained
syn keyword cfengineEditAction PercentCommentLinesContaining PercentCommentLinesMatching PercentCommentLinesStarting contained
syn keyword cfengineEditAction Prepend PrependIfNoLineMatching PrependifNoSuchLine contained
syn keyword cfengineEditAction Recurse ReplaceLineWith ReplaceAll ReplaceLinesMatchingField Repository contained
syn keyword cfengineEditAction ResetSearch RunScript RunScriptIfLineMatching RunScriptIfNoLineMatching contained
syn keyword cfengineEditAction SetCommentStart SetCommentEnd SetLine SetScript contained
syn keyword cfengineEditAction SlashCommentLinesContaining SlashCommentLinesMatching SlashCommentLinesStarting contained
syn keyword cfengineEditAction SplitOn Syslog Umask UnCommentLinesContaining UnCommentLinesMatching UnCommentNLines contained
syn keyword cfengineEditAction UnsetAbort UseShell WarnIfLineContaining WarnIfLineMatching WarnIfLineStarting contained
syn keyword cfengineEditAction WarnIfLineNoLineContaining WarnIfNoLineMatching WarnIfNoLineStarting WarnIfNoSuchLine contained
syn keyword cfengineEditAction ReplaceAll WarnIfContainsString WarnIfContainsFile contained

syn keyword cfengineFilter     Owner Atime Ctime Mtime FromAtime FromCtime FromMtime ToAtime ToCtime ToMtime contained
syn keyword cfengineFilter     Type reg link dir socket fifo door char block contained
syn keyword cfengineFilter     ExecRegex NameRegex IsSymLinkTo ExecProgram Result contained
syn keyword cfengineFilter     PID PPID PGID RSize VSize Status Command FromTTime ToTTime FromSTime ToSTime TTY contained
syn keyword cfengineFilter     Priority Threads contained
syn case match

syn case ignore
syn keyword cfengineActionType control: files: acl: binservers: broadcast: control: copy: defaultroute:
syn keyword cfengineActionType disks: directories: disable: editfiles: files: filters: groups: classes:
syn keyword cfengineActionType homeservers: ignore: import: interfaces: links: mailserver: miscmounts:
syn keyword cfengineActionType mountables: processes: required: resolv: shellcommands: tidy: unmount: 

" comments last overriding everything else
syn match   cfengineComment            "^\s*#.*$" contains=cfengineTodo
syn keyword cfengineTodo               TODO NOTE FIXME XXX contained

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_cfengine_syn_inits")
  if version < 508
    let did_cfengine_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  "HiLink cfengineAssignmentOperator String
  HiLink cfengineLinkOperator       String
  HiLink cfengineVariable           Special
  HiLink cfengineVariableDef        Special
  HiLink cfengineBoolean            Boolean
  HiLink cfengineEditAction         Identifier
  HiLink cfengineFilter             Identifier
  HiLink cfengineKeyword            Statement
  HiLink cfengineOption             Statement
  HiLink cfengineCompoundClass      Type
  HiLink cfengineActionType         PreProc
  HiLink cfengineComment            Comment
  HiLink cfengineNumber             Number
  HiLink cfengineQuota              Number
  HiLink cfengineString             String
  HiLink cfengineTodo               Todo
  HiLink cfengineAction             Constant

  delcommand HiLink
endif

let b:current_syntax = "cfengine"
