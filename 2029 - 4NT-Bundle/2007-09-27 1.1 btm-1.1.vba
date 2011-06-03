" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
btm_options.vim	[[[1
69
"------------------------------------------------------------------------------
"  Description: Options setable by the Btm bundle
"     Language: BTM (Batch to Memory - 4NT, TakeCommand Script)
"          $Id: btm_options.vim 37 2007-09-26 10:54:37Z krischik@users.sourceforge.net $
"    Copyright: Copyright (C) 2007 Martin Krischik
"   Maintainer: Martin Krischik
"               John Leo Spetz <jls11@po.cwru.edu>
"      $Author: krischik@users.sourceforge.net $
"        $Date: 2007-09-26 12:54:37 +0200 (Mi, 26 Sep 2007) $
"      Version: 1.1
"    $Revision: 37 $
"     $HeadURL: https://vim-scripts.googlecode.com/svn/trunk/2029%204NT%20Bundle/btm_options.vim $
"      History: 22.11.2007 MK A new Btm Filetype Bundle
"        Usage: copy content into your .vimrc and change options to your
"               likeing.
"    Help Page: ft_btm.txt
"------------------------------------------------------------------------------

echoerr 'It is suggested to copy the content of btm_options into .vimrc!'
finish " 1}}}

" Section: btm options {{{1

   let g:mapleader                        = "<F12>"
   let g:btm_highlight_identifier         = 1
   let g:btm_highlight_tabs               = 0
   let g:btm_highlight_unusual_comments   = 0
   let g:dosbatch_syntax_for_btm          = 0

" }}}1

" Section: Vimball options {{{1

:set noexpandtab fileformat=unix encoding=utf-8
:37,40 MkVimball btm-1.1.vba

btm_options.vim
autoload\btm.vim
ftplugin\btm.vim
syntax\btm.vim

" }}}1

" Section: Tar options {{{1

tar --create --bzip2          \
   --file="btm-1.1.tar.bz2"   \
   btm_options.vim            \
   autoload/btm.vim           \
   ftplugin/btm.vim           \
   syntax/btm.vim             ;

" }}}1

" Section: Svn options {{{1

svn copy -m "Tag Version 1.1 of BTM Bundle" 'https://vim-scripts.googlecode.com/svn/trunk/2029%204NT%20Bundle' 'https://vim-scripts.googlecode.com/svn/tags/2029%204NT%20Bundle%201.1'

svn copy -m "Tag Version 1.1 of BTM Bundle" https://vim-scripts.googlecode.com/svn/trunk/2029%%204NT%%20Bundle https://vim-scripts.googlecode.com/svn/tags/2029%%204NT%%20Bundle%%201.1

" }}}1

"------------------------------------------------------------------------------
"   Copyright (C) 2007  Martin Krischik
"
"   Vim is Charityware - see ":help license" or uganda.txt for licence details.
"------------------------------------------------------------------------------
" vim: textwidth=0 wrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab
" vim: foldmethod=marker
autoload\btm.vim	[[[1
175
"------------------------------------------------------------------------------
"  Description: Perform btm specific completion & tagging.
"     Language: BTM (Batch to Memory - 4NT, TakeCommand Script)
"	   $Id: btm.vim 35 2007-09-26 10:37:15Z krischik@users.sourceforge.net $
"    Copyright: Copyright (C) 2007 Martin Krischik
"   Maintainer: Martin Krischik
"               John Leo Spetz <jls11@po.cwru.edu>
"      $Author: krischik@users.sourceforge.net $
"	 $Date: 2007-09-26 12:37:15 +0200 (Mi, 26 Sep 2007) $
"      Version: 1.1
"    $Revision: 35 $
"     $HeadURL: https://vim-scripts.googlecode.com/svn/trunk/2029%204NT%20Bundle/autoload/btm.vim $
"      History: 22.11.2007 MK A new Btm Filetype Bundle
"    Help Page: ft-btm-functions
"------------------------------------------------------------------------------

if version < 700
   finish
endif

let g:btm#Keywords = []

" Section: g:btm#Keywords {{{1
"
" Section: add 4NT / TakeCommand command {{{2
"
for Item in ["ACTIVATE", "ALIAS", "ASSOC", "ATTRIB", "BATCOMP", "BDEBUGGER", "BEEP", "BREAK", "BREAKPOINT", "CALL", "CANCEL", "CASE", "CD", "CDD", "CHDIR", "CLS", "COLOR", "COPY", "DATE", "DDEEXEC", "DEBUGSTRING", "DEFAULT", "DEL", "DELAY", "DESCRIBE", "DETACH", "DIR", "DIRHISTORY", "DIRS", "DRAWBOX", "DRAWHLINE", "DRAWVLINE", "EJECTMEDIA", "ENDLOCAL", "ERASE", "ESET", "EVENTLOG", "EXCEPT", "EXIT", "FFIND", "FOR", "FREE", "FTYPE", "FUNCTION", "GLOBAL", "GOSUB", "GOTO", "HEAD", "HELP", "HISTORY", "IFTP", "INKEY", "INPUT", "JABBER", "KEYBD", "KEYS", "KEYSTACK", "LIST", "LOADBTM", "LOG", "MD", "MEMORY", "MKDIR", "MKLINK", "MKLNK", "MOVE", "MSGBOX", "ON", "OPTION", "OSD", "PATH", "PAUSE", "PDIR", "PLAYAVI", "PLAYSOUND", "PLUGIN", "POPD", "POSTMSG", "PRINT", "PRIORITY", "PROMPT", "PUSHD", "QUERYBOX", "QUIT", "RD", "REBOOT", "RECYCLE", "REM", "REN", "RENAME", "RETURN", "REXEC", "RMDIR", "RSHELL", "SCREEN", "SCRIPT", "SCRPUT", "SELECT", "SENDMAIL", "SET", "SETDOS", "SETLOCAL", "SHIFT", "SHORTCUT", "SHRALIAS", "SMPP", "SNMP", "SNPP", "START", "SYNC", "TAIL", "TASKEND", "TASKLIST", "TCTOOLBAR", "TEE", "TEXT", "TIME", "TIMER", "TITLE", "TOUCH", "TRANSIENT", "TREE", "TRUENAME", "TYPE", "UNALIAS", "UNFUNCTION", "UNSET", "VER", "VERIFY", "VOL", "VSCRPUT", "WHICH", "WINDOW", "WMIQUERY", "Y"]
    let g:btm#Keywords += [{
	    \ 'word':  Item,
	    \ 'menu':  'command',
	    \ 'info':  '4NT /TakeCommand command.',
	    \ 'kind':  'k',
	    \ 'icase': 0}]
endfor

" Section: add 4NT / TakeCommand echo command {{{2
"
for Item in ["ECHO", "ECHOERR", "ECHOS", "ECHOSERR"]
    let g:btm#Keywords += [{
	    \ 'word':  Item,
	    \ 'menu':  'command',
	    \ 'info':  '4NT /TakeCommand echo command.',
	    \ 'kind':  'e',
	    \ 'icase': 0}]
endfor

" Section: add 4NT / TakeCommand conditional command {{{2
"
for Item in ["IF", "IFF", "ENDIFF", "THEN", "ELSE", "ELSEIFF", "SWITCH", "CASE", "ENDSWITCH"]
    let g:btm#Keywords += [{
	    \ 'word':  Item,
	    \ 'menu':  'conditional',
	    \ 'info':  '4NT /TakeCommand conditional command.',
	    \ 'kind':  'c',
	    \ 'icase': 0}]
endfor

" Section: add 4NT / TakeCommand repeat command {{{2
"
for Item in ["FOR", "IN", "DO", "ENDDO", "WHILE", "FOREVER", "UNTIL", "DATETIME", "ITERATE", "LEAVE"]
    let g:btm#Keywords += [{
	    \ 'word':  Item,
	    \ 'menu':  'repeat',
	    \ 'info':  '4NT /TakeCommand repeat command.',
	    \ 'kind':  'r',
	    \ 'icase': 0}]
endfor

" Section: add 4NT / TakeCommand operators {{{2
"
for Item in ["NOT", "ERRORLEVEL", "GT", "LT", "EQ", "NE", "GE", "LE", "DEFINED", "EXIST", "ISALIAS", "ISDIR", "DIREXIST", "ISINTERNAL", "ISLABEL", "AND", "OR", "XOR"]
    let g:btm#Keywords += [{
	    \ 'word':  Item,
	    \ 'menu':  'operator',
	    \ 'info':  '4NT /TakeCommand operators.',
	    \ 'kind':  'o',
	    \ 'icase': 0}]
endfor

" Section: add 4NT / TakeCommand System Variables {{{2
"
for Item in ["CDPATH", "CMDLINE", "COLORDIR", "COMSPEC", "FILECOMPLETION", "HISTORYEXCLUSION", "PATH", "PATHEXT", "PROMPT", "RECYCLEEXCLUDE", "TEMP", "TITLEPROMPT", "TMP", "TREEEXCLUDE", "VARIABLEEXCLUDE"]
    let g:btm#Keywords += [{
	    \ 'word':  Item,
	    \ 'menu':  'system variable',
	    \ 'info':  '4NT /TakeCommand system variable.',
	    \ 'kind':  's',
	    \ 'icase': 0}]
endfor

" Section: add 4NT / TakeCommand internal variables {{{2
"
for Item in ["_4ver", "_acstatus", "_afswcell", "_alt", "_ansi", "_batch", "_batchline", "_batchname", "_batchtype", "_battery", "_batterylife", "_batterypercent", "_bdebugger", "_bg", "_boot", "_build", "_capslock", "_cdroms", "_childpid", "_ci", "_cmdline", "_cmdproc", "_cmdspec", "_co", "_codepage", "_column", "_columns", "_country", "_cpu", "_cpuusage", "_ctrl", "_cwd", "_cwds", "_cwp", "_cwps", "_datetime", "_day", "_detachpid", "_disk", "_dname", "_dos", "_dosver", "_dow", "_dowf", "_dowi", "_doy", "_drives", "_dst", "_dvds", "_echo", "_editmode", "errorlevel", "_execstr", "_exit", "_expansion", "_fg", "_ftperror", "_hdrives", "_hlogfile", "_host", "_hour", "_hwprofile", "_idleticks", "_idow", "_idowf", "_iftp", "_iftps", "_imonth", "_imonthf", "_ininame", "_ip", "_isodate", "_kbhit", "_lalt", "_lastdisk", "_lctrl", "_logfile", "_lshift", "_minute", "_month", "_monthf", "_numlock", "_openafs", "_osbuild", "_pid", "_pipe", "_ppid", "_ralt", "_rctrl", "_ready", "_registered", "_row", "_rows", "_rshift", "_scrolllock", "_second", "_selected", "_shell", "_shells", "_shift", "_shralias", "_startpath", "_startpid", "_stdin", "_stdout", "_stderr", "_stzn", "_stzo", "_syserr", "_time", "_transient", "_tzn", "_tzo", "_unicode", "_virtualpc", "_vmware", "_windir", "_winfgwindow", "_winname", "_winsysdir", "_winticks", "_wintitle", "_winuser", "_winver", "_wow64", "_xpixels", "_year", "_ypixels"]
    let g:btm#Keywords += [{
	    \ 'word':  Item,
	    \ 'menu':  'internal variable',
	    \ 'info':  '4NT /TakeCommand internal variable',
	    \ 'kind':  'v',
	    \ 'icase': 1}]
endfor

" Section: add 4NT / TakeCommand internal function {{{2
"
for Item in ["@ABS", "@AFSCELL", "@AFSMOUNT", "@AFSPATH", "@AFSSYMLINK", "@AFSVOLID", "@AFSVOLNAME", "@AGEDATE", "@ALIAS", "@ALTNAME", "@ASCII", "@ASSOC", "@ATTRIB", "@AVERAGE", "@CAPI", "@CAPS", "@CDROM", "@CEILING", "@CHAR", "@CLIP", "@CLIPW", "@COLOR", "@COMMA", "@COMPARE", "@CONSOLE", "@CONVERT", "@COUNT", "@CRC32", "@CWD", "@CWDS", "@DATE", "@DAY", "@DEC", "@DECIMAL", "@DESCRIPT", "@DEVICE", "@DIGITS", "@DIRSTACK", "@DISKFREE", "@DISKTOTAL", "@DISKUSED", "@DOMAIN", "@DOW", "@DOWF", "@DOWI", "@DOY", "@DRIVETYPE", "@DRIVETYPEEX", "@ENUMSERVERS", "@ENUMSHARES", "@ERRTEXT", "@EVAL", "@EXEC", "@EXECSTR", "@EXETYPE", "@EXPAND", "@EXT", "@FIELD", "@FIELDS", "@FILEAGE", "@FILECLOSE", "@FILEDATE", "@FILENAME", "@FILEOPEN", "@FILEREAD", "@FILES", "@FILESEEK", "@FILESEEKL", "@FILESIZE", "@FILETIME", "@FILEWRITE", "@FILEWRITEB", "@FINDCLOSE", "@FINDFIRST", "@FINDNEXT", "@FLOOR", "@FORMAT", "@FORMATN", "@FSTYPE", "@FTYPE", "@FULL", "@FUNCTION", "@GETDIR", "@GETFILE", "@GETFOLDER", "@GROUP", "@HISTORY", "@IDOW", "@IDOWF", "@IF", "@INC", "@INDEX", "@INIREAD", "@INIWRITE", "@INSERT", "@INODE", "@INSTR", "@INT", "@IPADDRESS", "@IPNAME", "@ISALNUM", "@ISALPHA", "@ISASCII", "@ISCNTRL", "@ISDIGIT", "@ISPRINT", "@ISPUNCT", "@ISSPACE", "@ISXDIGIT", "@JUNCTION", "@LABEL", "@LCS", "@LEFT", "@LEN", "@LFN", "@LINE", "@LINES", "@LINKS", "@LOWER", "@LTRIM", "@MAKEAGE", "@MAKEDATE", "@MAKETIME", "@MAX", "@MD5", "@MIN", "@MONTH", "@NAME", "@NUMERIC", "@OPTION", "@OWNER", "@PATH", "@PERL", "@PING", "@QUOTE", "@RANDOM", "@READSCR", "@READY", "@REGCREATE", "@REGDELKEY", "@REGEX", "@REGEXINDEX", "@REGEXIST", "@REGEXSUB", "@REGQUERY", "@REGSET", "@REGSETENV", "@REMOTE", "@REMOVABLE", "@REPEAT", "@REPLACE", "@REVERSE", "@REXX", "@RIGHT", "@RTRIM", "@RUBY", "@SCRIPT", "@SEARCH", "@SELECT", "@SERIAL", "@SFN", "@SHA1", "@SHA256", "@SHA384", "@SHA512", "@SIMILAR", "@SNAPSHOT", "@STRIP", "@SUBST", "@SUBSTR", "@SUMMARY", "@SYMLINK", "@TIME", "@TIMER", "@TRIM", "@TRUENAME", "@TRUNCATE", "@UNC", "@UNICODE", "@UNIQUE", "@UNQUOTE", "@UNQUOTES", "@UPPER", "@VERINFO", "@WATTRIB", "@WILD", "@WINAPI", "@WINCLASS", "@WINEXENAME", "@WININFO", "@WINMEMORY", "@WINMETRICS", "@WINPOS", "@WINSTATE", "@WINSYSTEM", "@WMI", "@WORD", "@WORDS", "@WORKGROUP", "@XMLPATH", "@YEAR"]
    let g:btm#Keywords += [{
	    \ 'word':  Item,
	    \ 'menu':  'internal function',
	    \ 'info':  '4NT /TakeCommand internal function.',
	    \ 'kind':  'f',
	    \ 'icase': 0}]
endfor

" Section: add 4NT / TakeCommand configuration option {{{2
"
for Item in ["4StartPath", "AddFile", "AliasExpand", "AmPm", "ANSI", "AppendToDir", "AutoCancel", "AutoRun", "Backspace", "BatchEcho", "BeepFreq", "BeepLength", "BeginLine", "BGColorRGB", "CDDWinColors", "CDDWinHeight", "CDDWinLeft", "CDDWinTop", "CDDWinWidth", "ClearKeyMap", "ColorDir", "CMDExtensions", "CommandEscape", "CommandSep", "CompleteHidden", "CompletePaths", "ConsoleColumns", "ConsoleRows", "Copy", "CopyPrompt", "CUA", "CursorIns", "CursorOver", "Debug", "DebuggerTransparency", "DecimalChar", "Del", "DelayedExpansion", "DelGlobalQuery", "DelHistory", "DelToBeginning", "DelToEnd", "DelWordLeft", "DelWordRight", "DescriptionMax", "DescriptionName", "Descriptions", "DirHistory", "DirWinOpen", "Down", "DuplicateBugs", "EditMode", "Editor", "EndHistory", "EndLine", "EraseLine", "EscapeChar", "EvalMax", "EvalMin", "ExecLine", "ExecWait", "ExitFile", "FGColorRGB", "FileCompletion", "FirewallHost", "FirewallPassword", "FirewallType", "FirewallUser", "FTPCFG", "FTPTimeout", "FuzzyCD", "Help", "HelpWord", "HideConsole", "HistCopy", "HistDups", "HistFile", "HistLogName", "HistLogOn", "HistMin", "HistMove", "History", "HistWinOpen", "HistWrap", "HTTPTimeout", "IBeamCursor", "InactiveTransparency", "Include", "INIQuery", "InputColors", "Ins", "JabberPassword", "JabberServer", "JabberUser", "LastHistory", "Left", "LFNToggle", "LineToEnd", "ListBack", "ListboxBarColors", "ListContinue", "ListColors", "ListExit", "ListFind", "ListFindRegex", "ListFindRegexReverse", "ListFindReverse", "ListHex", "ListHighBit", "ListInfo", "ListInverseColors", "ListNext", "ListOpen", "ListPrevious", "ListPrint", "ListRowStart", "ListStatBarColors", "ListUnicode", "ListWrap", "LocalAliases", "LocalDirHistory", "LocalFunctions", "LocalHistory", "LogAll", "LogErrors", "LogName", "LogOn", "MailAddress", "MailPassword", "MailPort", "MailServer", "MailUser", "NextFile", "NextHistory", "NextINIFile", "NoClobber", "NormalEditKey", "NormalKey", "NormalListKey", "NormalPopupKey", "NTFSDescriptions", "ParameterChar", "PassiveFTP", "Paste", "PathExt", "PauseOnError", "Perl", "PopFile", "PopupWinBegin", "PopupWinColors", "PopupWinDel", "PopupWinEdit", "PopupWinEnd", "PopupWinExec", "PopupWinHeight", "PopupWinLeft", "PopupWinTop", "PopupWinWidth", "PrevFile", "PrevHistory", "Proxy", "ProxyPassword", "ProxyPort", "ProxyUser", "RecycleBin", "RegularExpressions", "RepeatFile", "REXX", "Right", "RLocalHost", "RLocalUser", "RLocalPort", "Ruby", "SaveDirCase", "SaveHistory", "ScreenBufSize", "ScreenColumns", "ScreenRows", "ScrollDown", "ScrollPgDn", "ScrollPgUp", "ScrollUp", "SelectColors", "SelectStatBarColors", "ServerCompletion", "SettingChange", "SHChangeNotify", "SSLPort", "SSLProvider", "SSLStartMode", "StartupFile", "StatusBarOn", "StatusBarText", "StdColors", "SwapScrollKeys", "SwitchChar", "TabStops", "TCStartPath", "TFTPTimeout", "ThousandsChar", "TimeServer", "ToolBarOn", "ToolBarText", "Transparency", "TreePath", "UnicodeOutput", "UnixPaths", "Up", "UpdateTitle", "VariableExpand", "Win32SFNSearch", "WindowHeight", "WindowState", "WindowWidth", "WindowX", "WindowY", "WordLeft", "WordRight", "Wow64FsRedirection", "ZoneID"]
    let g:btm#Keywords += [{
	    \ 'word':  Item,
	    \ 'menu':  'config',
	    \ 'info':  '4NT / TakeCommand configuration option.',
	    \ 'kind':  't',
	    \ 'icase': 0}]
endfor

" Section: Insert Completions {{{1
"
" Section: btm#User_Complete(findstart, base) {{{2
"
" This function is used for the 'complete' option.
"
function btm#User_Complete(findstart, base)
   if a:findstart == 1
      "
      " locate the start of the word
      "
      let line = getline ('.')
      let start = col ('.') - 1
      while start > 0 && line[start - 1] =~ '\i\|'''
	 let start -= 1
      endwhile
      return start
   else
      "
      " look up matches
      "
      let l:Pattern = '^' . a:base . '.*$'
      "
      " add keywords
      "
      for Tag_Item in g:btm#Keywords
	 if l:Tag_Item['word'] =~? l:Pattern
	    if complete_add (l:Tag_Item) == 0
	       return []
	    endif
	    if complete_check ()
	       return []
	    endif
	 endif
      endfor
      return []
   endif
endfunction btm#User_Complete

" }}}1

lockvar! g:btm#Keywords

finish " 1}}}

"------------------------------------------------------------------------------
"   Copyright (C) 2006	Martin Krischik
"
"   Vim is Charityware - see ":help license" or uganda.txt for licence details.
"------------------------------------------------------------------------------
" vim: textwidth=0 wrap tabstop=8 shiftwidth=3 softtabstop=3 noexpandtab
" vim: foldmethod=marker
ftplugin\btm.vim	[[[1
70
"------------------------------------------------------------------------------
"  Description: Filetype plugin file for Btm
"     Language: BTM (Batch to Memory - 4NT, TakeCommand Script)
"          $Id: btm.vim 35 2007-09-26 10:37:15Z krischik@users.sourceforge.net $
"    Copyright: Copyright (C) 2007 Martin Krischik
"   Maintainer: Martin Krischik <krischik@users.sourceforge.net>
"               Bram Moolenaar <Bram@vim.org>
"               Bill McCarthy <WJMc@pobox.com>
"      $Author: krischik@users.sourceforge.net $
"        $Date: 2007-09-26 12:37:15 +0200 (Mi, 26 Sep 2007) $
"      Version: 1.1
"    $Revision: 35 $
"     $HeadURL: https://vim-scripts.googlecode.com/svn/trunk/2029%204NT%20Bundle/ftplugin/btm.vim $
"      History: 22.11.2007 MK  A new Btm Filetype Bundle
"               27.09.2007 BMC Matchit setup
"    Help Page: ft-btm-plugin
"------------------------------------------------------------------------------
" btm filetype plugin file

" Only do this when not done yet for this buffer
if exists("b:did_ftplugin") || version < 700
  finish
endif

" Section: File-Option {{{1
"
setlocal encoding=utf-8
setlocal wrap
setlocal smartcase
setlocal ignorecase
setlocal formatoptions-=t formatoptions+=rol

" Section: Comments {{{1
"
setlocal commentstring=::\ %s
setlocal complete=.,w,b,u,t,i
setlocal comments=b:rem,b:@rem,b:REM,b:@REM,b:::

" Section: Define patterns for the browse file filter {{{1
"
if has("gui_win32") && !exists("b:browsefilter")
  let b:browsefilter = "DOS Batch Files (*.bat, *.btm, *.cmd)\t*.bat;*.btm;*.cmd\nAll Files (*.*)\t*.*\n"
endif

" Section: Matchit {{{1
"
let b:match_words
    \= '\<iff\>:\<elseiff\>:\<else\>:\<endiff\>,'
    \. '\<do\>:\<iterate\>:\<leave\>:\<enddo\>,'
    \. '^\:\a\+:\<return\>,'
    \. '\%(^\s*\)\@<=text\s*$:\%(^\s*\)\@<=endtext\s*$,'
    \. '\<switch\>:\<case\>:\<default\>:\<endswitch\>'

" Section: Tagging {{{1
"
setlocal iskeyword+=-

" Section: Completion {{{1
"
setlocal omnifunc=syntaxcomplete#Complete
setlocal completefunc=btm#User_Complete

" Section: Folding {{{1
"
setlocal foldmethod=marker

finish " 1}}}

"vim: set nowrap tabstop=8 shiftwidth=4 softtabstop=4 expandtab :
"vim: set textwidth=0 filetype=vim foldmethod=marker nospell :
syntax\btm.vim	[[[1
178
"------------------------------------------------------------------------------
"  Description: Vim syntax file for Btm
"     Language: BTM (Batch to Memory - 4NT, TakeCommand Script)
"          $Id: btm.vim 35 2007-09-26 10:37:15Z krischik@users.sourceforge.net $
"    Copyright: Copyright (C) 2007 Martin Krischik
"   Maintainer: Martin Krischik <krischik@users.sourceforge.net>
"               John Leo Spetz <jls11@po.cwru.edu>
"      $Author: krischik@users.sourceforge.net $
"        $Date: 2007-09-26 12:37:15 +0200 (Mi, 26 Sep 2007) $
"      Version: 1.1
"    $Revision: 35 $
"     $HeadURL: https://vim-scripts.googlecode.com/svn/trunk/2029%204NT%20Bundle/syntax/btm.vim $
"      History: 22.11.2007 MK A new Btm Filetype Bundle
"    Help Page: ft-btm-plugin
"------------------------------------------------------------------------------
" Vim syntax file

"//Issues to resolve:
"//- Boolean operators surrounded by period are recognized but the
"//  periods are not highlighted.  The only way to do that would
"//  be separate synmatches for each possibility otherwise a more
"//  general \.\i\+\. will highlight anything delimited by dots.
"//- After unary operators like "defined" can assume token type.
"//  Should there be more of these?

if exists("b:current_syntax") || version < 700
    finish
endif

let b:current_syntax = "btm"

syntax case ignore

syntax match    btmOperator     transparent    "\.\i\+\." contains=btmDotBoolOp
syntax keyword  btmDotBoolOp    contained      and or xor
syntax match    btmOperator     "=="
syntax match    btmOperator     "!="

syntax keyword btmTodo contained        TODO

" String
syntax cluster  btmVars         contains=btmVariable,btmArgument,btmBIFMatch
syntax region   btmString       start=+"+  end=+"+ contains=@btmVars
syntax match    btmNumber       "\<\d\+\>"

if exists ('g:btm_highlight_identifier')
    syntax match  btmIdentifier "\<\h\w*\>"
endif

if exists ('g:btm_highlight_tabs')
    syntax match btmShowTab     "\t"
    syntax match btmShowTabc    "\t"
    syntax match btmComment     "^\ *rem.*$" contains=btmTodo,btmShowTabc
    syntax match btmComment     "^\ *::.*$" contains=btmTodo,btmShowTabc
else
    syntax match btmComment     "^\ *rem.*$" contains=btmTodo
    syntax match btmComment     "^\ *::.*$" contains=btmTodo
endif

if exists ('g:btm_highlight_unusual_comments')
    " Some people use this as a comment line. In fact this is a Label!
    syntax match btmComment     "^\ *:\ \+.*$" contains=btmTodo
endif

syntax match btmLabelMark       "^\ *:[0-9a-zA-Z_\-]\+\>"
syntax match btmLabelMark       "goto [0-9a-zA-Z_\-]\+\>"lc=5
syntax match btmLabelMark       "gosub [0-9a-zA-Z_\-]\+\>"lc=6

" syntax match btmCmdDivider    ">[>&][>&]\="
syntax match btmCmdDivider      ">[>&]*"
syntax match btmCmdDivider      ">>&>"
syntax match btmCmdDivider      "|&\="
syntax match btmCmdDivider      "%+"
syntax match btmCmdDivider      "\^"

syntax region   btmEcho         start="echo" skip="echo" matchgroup=btmCmdDivider end="%+" end="$" end="|&\=" end="\^" end=">[>&]*" contains=@btmEchos oneline
syntax cluster  btmEchos        contains=@btmVars,btmEchoCommand,btmEchoParam
for b:Item in g:btm#Keywords
    if b:Item['kind'] == "e"
        execute "syntax keyword btmEchoCommand  contained" . b:Item['word']
    endif
endfor
syntax keyword  btmEchoParam    contained       on off

" this is also a valid Label. I don't use it.
"syntax match btmLabelMark      "^\ *:\ \+[0-9a-zA-Z_\-]\+\>"

" //Environment variable can be expanded using notation %var in 4DOS
syntax match btmVariable                "%[0-9a-z_\-]\+" contains=@btmSpecialVars
" //Environment variable can be expanded using notation %var%
syntax match btmVariable                "%[0-9a-z_\-]*%" contains=@btmSpecialVars
" //The following are special variable in 4DOS
syntax match btmVariable                "%[=#]" contains=@btmSpecialVars
syntax match btmVariable                "%??\=" contains=@btmSpecialVars
" //Environment variable can be expanded using notation %[var] in 4DOS
syntax match btmVariable                "%\[[0-9a-z_\-]*\]"
" //After some keywords next word should be an environment variable
syntax match btmVariable                "defined\s\i\+"lc=8
syntax match btmVariable                "set\s\{}\i\+"lc=4
" //Parameters to batchfiles take the format %<digit>
syntax match btmArgument                "%\d\>"
" //4DOS allows format %<digit>& meaning batchfile parameters digit and up
syntax match btmArgument                "%\d\>&"
" //Variable used by FOR loops sometimes use %%<letter> in batchfiles
syntax match btmArgument                "%%\a\>"

" //Show 4DOS built-in functions specially
syntax match btmBIFMatch "%@\w\+\["he=e-1 contains=btmBuiltInFunc
for b:Item in g:btm#Keywords
   if b:Item['kind'] == "f"
      execute "syntax keyword btmBuiltInFunc contained" . b:Item['word']
    endif
endfor

syntax cluster btmSpecialVars contains=btmBuiltInVar,btmSpecialVar

" //Show specialized variables specially
" syntax match btmSpecialVar contained  "+"
syntax match btmSpecialVar contained    "="
syntax match btmSpecialVar contained    "#"
syntax match btmSpecialVar contained    "??\="

for b:Item in g:btm#Keywords
    if b:Item['kind'] == "c"
        execute "syntax keyword btmConditional "        . b:Item['word']
    elseif  b:Item['kind'] == "r"
        execute "syntax keyword btmRepeat "             . b:Item['word']
    elseif b:Item['kind'] == "o"
        execute "syntax keyword btmOperator "           . b:Item['word']
    elseif b:Item['kind'] == "s"
        execute "syntax keyword btmSpecialVar "         . b:Item['word']
    elseif b:Item['kind'] == "v"
        execute "syntax keyword btmBuiltInVar "         . b:Item['word']
    elseif b:Item['kind'] == "k"
        execute "syntax keyword btmCommand "            . b:Item['word']
    endif
endfor

" //Commands in 4DOS and/or DOS
syntax match btmCommand "\s?"
syntax match btmCommand "^?"

highlight def link btmOperator          Operator
highlight def link btmLabel             Label
highlight def link btmLabelMark         Special
highlight def link btmCmdDivider        Special
highlight def link btmConditional       Conditional
highlight def link btmDotBoolOp         Operator
highlight def link btmRepeat            Repeat
highlight def link btmEchoCommand       Keyword
highlight def link btmEchoParam         Keyword
highlight def link btmStatement         Statement
highlight def link btmTodo              Todo
highlight def link btmString            String
highlight def link btmNumber            Number
highlight def link btmComment           Comment
highlight def link btmArgument          Identifier
highlight def link btmVariable          Identifier
highlight def link btmEcho              String
highlight def link btmBIFMatch          btmStatement
highlight def link btmBuiltInFunc       Function
highlight def link btmBuiltInVar        Identifier
highlight def link btmSpecialVar        Special
highlight def link btmCommand           Keyword

if exists ('g:btm_highlight_tabs') && g:btm_highlight_tabs
    highlight default link btmShowTab   Error
    highlight default link btmShowTabc  Error
endif

if exists ('g:btm_highlight_identifier') && g:btm_highlight_identifier
    highlight default link btmIdentifier Identifier
endif

finish

"vim: set nowrap tabstop=8 shiftwidth=4 softtabstop=4 noexpandtab :
"vim: set textwidth=0 filetype=vim foldmethod=marker nospell :
