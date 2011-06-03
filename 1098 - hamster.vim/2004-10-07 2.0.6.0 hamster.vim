" Vim syntax file
" Language:    Hamster Scripting Language
" Maintainer:  David Fishburn <fishburn@ianywhere.com>
" Last Change: Thu Oct 07 2004 9:41:44 PM
" Version:     2.0.6.0

" Description: Hamster Classic
" Hamster is a local server for news and mail. It's a windows-32-bit-program.
" It allows the use of multiple news- and mailserver and combines them to one
" mail- and newsserver for the news/mail-client. It load faster than a normal
" newsreader because many threads can run simultaneous. It contains scorefile
" for news and mail, a build-in script language, the GUI allows translation to
" other languages, it can be used in a network and that's not all features...
"
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

syn case ignore

syn keyword hamSpecial abs
syn keyword hamSpecial artaddheader
syn keyword hamSpecial artalloc
syn keyword hamSpecial artdelheader
syn keyword hamSpecial artfree
syn keyword hamSpecial artgetbody
syn keyword hamSpecial artgetheader
syn keyword hamSpecial artgetheaders
syn keyword hamSpecial artgettext
syn keyword hamSpecial artheaderexists
syn keyword hamSpecial artload
syn keyword hamSpecial artsave
syn keyword hamSpecial artsetbody
syn keyword hamSpecial artsetheader
syn keyword hamSpecial artsetheaders
syn keyword hamSpecial artsettext
syn keyword hamSpecial assert
syn keyword hamSpecial atadd
syn keyword hamSpecial atclear
syn keyword hamSpecial atcount
syn keyword hamSpecial ateverymins
syn keyword hamSpecial atexecute
syn keyword hamSpecial atfrom
syn keyword hamSpecial atondays
syn keyword hamSpecial atsubfunction
syn keyword hamSpecial atuntil
syn keyword hamSpecial beep
syn keyword hamSpecial break
syn keyword hamSpecial chr
syn keyword hamSpecial clearxcounter
syn keyword hamSpecial clipread
syn keyword hamSpecial clipwrite
syn keyword hamSpecial const
syn keyword hamSpecial constenum
syn keyword hamSpecial continue
syn keyword hamSpecial copy
syn keyword hamSpecial debug
syn keyword hamSpecial dec
syn keyword hamSpecial decodebase64
syn keyword hamSpecial decodeqp
syn keyword hamSpecial decodetime
syn keyword hamSpecial decxcounter
syn keyword hamSpecial delete
syn keyword hamSpecial deletehostsentry
syn keyword hamSpecial digest
syn keyword hamSpecial dirchange
syn keyword hamSpecial dircurrent
syn keyword hamSpecial direxists
syn keyword hamSpecial dirmake
syn keyword hamSpecial dirremove
syn keyword hamSpecial dirsystem
syn keyword hamSpecial dirwindows
syn keyword hamSpecial diskfreekb
syn keyword hamSpecial dllcall
syn keyword hamSpecial dllfree
syn keyword hamSpecial dlllasterror
syn keyword hamSpecial dllload
syn keyword hamSpecial dump
syn keyword hamSpecial encodetime
syn keyword hamSpecial entercontext
syn keyword hamSpecial errcatch
syn keyword hamSpecial errline
syn keyword hamSpecial errlineno
syn keyword hamSpecial errmodule
syn keyword hamSpecial errmsg
syn keyword hamSpecial errnum
syn keyword hamSpecial error
syn keyword hamSpecial errsender
syn keyword hamSpecial eval
syn keyword hamSpecial eventclose
syn keyword hamSpecial eventcreate
syn keyword hamSpecial eventmultiplewait
syn keyword hamSpecial eventpulse
syn keyword hamSpecial eventreset
syn keyword hamSpecial eventset
syn keyword hamSpecial eventwait
syn keyword hamSpecial execute
syn keyword hamSpecial false
syn keyword hamSpecial filecopy
syn keyword hamSpecial filedelete
syn keyword hamSpecial fileexists
syn keyword hamSpecial filemove
syn keyword hamSpecial filerename
syn keyword hamSpecial filesize
syn keyword hamSpecial filetime
syn keyword hamSpecial getenv
syn keyword hamSpecial getprocessidentifier
syn keyword hamSpecial getuptimedays
syn keyword hamSpecial getuptimehours
syn keyword hamSpecial getuptimemins
syn keyword hamSpecial getuptimesecs
syn keyword hamSpecial gosub
syn keyword hamSpecial goto
syn keyword hamSpecial hex
syn keyword hamSpecial icase
syn keyword hamSpecial iif
syn keyword hamSpecial inc
syn keyword hamSpecial incxcounter
syn keyword hamSpecial inidelete
syn keyword hamSpecial inierasesection
syn keyword hamSpecial iniread
syn keyword hamSpecial iniwrite
syn keyword hamSpecial inputbox
syn keyword hamSpecial inputpw
syn keyword hamSpecial int
syn keyword hamSpecial isint
syn keyword hamSpecial isstr
syn keyword hamSpecial leavecontext
syn keyword hamSpecial len
syn keyword hamSpecial listadd
syn keyword hamSpecial listalloc
syn keyword hamSpecial listappend
syn keyword hamSpecial listbox
syn keyword hamSpecial listclear
syn keyword hamSpecial listcount
syn keyword hamSpecial listdelete
syn keyword hamSpecial listdirs
syn keyword hamSpecial listexists
syn keyword hamSpecial listfiles
syn keyword hamSpecial listfiles
syn keyword hamSpecial listfree
syn keyword hamSpecial listget
syn keyword hamSpecial listgetkey
syn keyword hamSpecial listgettag
syn keyword hamSpecial listgettext
syn keyword hamSpecial listindexof
syn keyword hamSpecial listinsert
syn keyword hamSpecial listload
syn keyword hamSpecial listrasentries
syn keyword hamSpecial listsave
syn keyword hamSpecial listset
syn keyword hamSpecial listsetkey
syn keyword hamSpecial listsettag
syn keyword hamSpecial listsettext
syn keyword hamSpecial listsort
syn keyword hamSpecial localhostaddr
syn keyword hamSpecial localhostname
syn keyword hamSpecial lookuphostaddr
syn keyword hamSpecial lookuphostname
syn keyword hamSpecial lowercase
syn keyword hamSpecial memalloc
syn keyword hamSpecial memforget
syn keyword hamSpecial memfree
syn keyword hamSpecial memgetint
syn keyword hamSpecial memgetstr
syn keyword hamSpecial memsetint
syn keyword hamSpecial memsetstr
syn keyword hamSpecial memsize
syn keyword hamSpecial memvarptr
syn keyword hamSpecial msgbox
syn keyword hamSpecial ord
syn keyword hamSpecial paramcount
syn keyword hamSpecial paramstr
syn keyword hamSpecial popupbox
syn keyword hamSpecial pos
syn keyword hamSpecial print
syn keyword hamSpecial quit
syn keyword hamSpecial random
syn keyword hamSpecial randomize
syn keyword hamSpecial rasdial
syn keyword hamSpecial rasgetconnection
syn keyword hamSpecial rasgetip
syn keyword hamSpecial rashangup
syn keyword hamSpecial rasisconnected
syn keyword hamSpecial re_extract
syn keyword hamSpecial re_match
syn keyword hamSpecial re_parse
syn keyword hamSpecial re_split
syn keyword hamSpecial replace
syn keyword hamSpecial return
syn keyword hamSpecial runscript
syn keyword hamSpecial scriptpriority
syn keyword hamSpecial set
syn keyword hamSpecial sethostsentry_byaddr
syn keyword hamSpecial sethostsentry_byname
syn keyword hamSpecial setxcounter
syn keyword hamSpecial sgn
syn keyword hamSpecial shell
syn keyword hamSpecial sleep
syn keyword hamSpecial stopthread
syn keyword hamSpecial str
syn keyword hamSpecial syserrormessage
syn keyword hamSpecial testmailfilterline
syn keyword hamSpecial testnewsfilterline
syn keyword hamSpecial ticks
syn keyword hamSpecial time
syn keyword hamSpecial timegmt
syn keyword hamSpecial trace
syn keyword hamSpecial trim
syn keyword hamSpecial true
syn keyword hamSpecial uppercase
syn keyword hamSpecial utf7toucs16
syn keyword hamSpecial utf8toucs32
syn keyword hamSpecial var
syn keyword hamSpecial varset
syn keyword hamSpecial warning
syn keyword hamSpecial xcounter

" common functions
syn keyword hamFunction addlog
syn keyword hamFunction decodemimeheaderstring
syn keyword hamFunction decodetolocalcharset
syn keyword hamFunction gettasksactive
syn keyword hamFunction gettasksrun
syn keyword hamFunction gettaskswait
syn keyword hamFunction hamaddgroup
syn keyword hamFunction hamaddlog
syn keyword hamFunction hamaddpull
syn keyword hamFunction hamartcount
syn keyword hamFunction hamartdeletemid
syn keyword hamFunction hamartdeletemidingroup
syn keyword hamFunction hamartdeletenringroup
syn keyword hamFunction hamartimport
syn keyword hamFunction hamartlocatemid
syn keyword hamFunction hamartlocatemidingroup
syn keyword hamFunction hamartnomax
syn keyword hamFunction hamartnomin
syn keyword hamFunction hamarttext
syn keyword hamFunction hamarttextexport
syn keyword hamFunction hamchangepassword
syn keyword hamFunction hamcheckpurge
syn keyword hamFunction hamdelgroup
syn keyword hamFunction hamdelpull
syn keyword hamFunction hamdialogaddpull
syn keyword hamFunction hamdialogeditdirs
syn keyword hamFunction hamdialogmailkillfilelog
syn keyword hamFunction hamdialognewskillfilelog
syn keyword hamFunction hamdialogscripts
syn keyword hamFunction hamenvelopefrom
syn keyword hamFunction hamexepath
syn keyword hamFunction hamfetchmail
syn keyword hamFunction hamflush
syn keyword hamFunction hamgetstatus
syn keyword hamFunction hamgroupclose
syn keyword hamFunction hamgroupcount
syn keyword hamFunction hamgroupindex
syn keyword hamFunction hamgroupname
syn keyword hamFunction hamgroupnamebyhandle
syn keyword hamFunction hamgroupopen
syn keyword hamFunction hamgroupspath
syn keyword hamFunction hamhscpath
syn keyword hamFunction hamhsmpath
syn keyword hamFunction hamimapserver
syn keyword hamFunction hamisidle
syn keyword hamFunction hamlogspath
syn keyword hamFunction hammailexchange
syn keyword hamFunction hammailpath
syn keyword hamFunction hammailsoutpath
syn keyword hamFunction hammainfqdn
syn keyword hamFunction hammainwindow
syn keyword hamFunction hammessage
syn keyword hamFunction hammidfqdn
syn keyword hamFunction hamnewmail
syn keyword hamFunction hamnewserrpath
syn keyword hamFunction hamnewsjobsadd
syn keyword hamFunction hamnewsjobscheckactive
syn keyword hamFunction hamnewsjobsclear
syn keyword hamFunction hamnewsjobsdelete
syn keyword hamFunction hamnewsjobsfeed
syn keyword hamFunction hamnewsjobsgetcounter
syn keyword hamFunction hamnewsjobsgetparam
syn keyword hamFunction hamnewsjobsgetpriority
syn keyword hamFunction hamnewsjobsgetserver
syn keyword hamFunction hamnewsjobsgettype
syn keyword hamFunction hamnewsjobspost
syn keyword hamFunction hamnewsjobspostdef
syn keyword hamFunction hamnewsjobspull
syn keyword hamFunction hamnewsjobspulldef
syn keyword hamFunction hamnewsjobssetpriority
syn keyword hamFunction hamnewsjobsstart
syn keyword hamFunction hamnewsoutpath
syn keyword hamFunction hamnewspost
syn keyword hamFunction hamnewspull
syn keyword hamFunction hamnntpserver
syn keyword hamFunction hampassreload
syn keyword hamFunction hampath
syn keyword hamFunction hampop3server
syn keyword hamFunction hampostmaster
syn keyword hamFunction hampurge
syn keyword hamFunction hamrasdial
syn keyword hamFunction hamrashangup
syn keyword hamFunction hamrcpath
syn keyword hamFunction hamrebuildgloballists
syn keyword hamFunction hamrebuildhistory
syn keyword hamFunction hamrecoserver
syn keyword hamFunction hamreloadconfig
syn keyword hamFunction hamreloadipaccess
syn keyword hamFunction hamresetcounters
syn keyword hamFunction hamrotatelog
syn keyword hamFunction hamscorelist
syn keyword hamFunction hamscoretest
syn keyword hamFunction hamsendmail
syn keyword hamFunction hamsendmailauth
syn keyword hamFunction hamserverpath
syn keyword hamFunction hamsetlogin
syn keyword hamFunction hamshutdown
syn keyword hamFunction hamsmtpserver
syn keyword hamFunction hamstopalltasks
syn keyword hamFunction hamthreadcount
syn keyword hamFunction hamtrayicon
syn keyword hamFunction hamusenetacc
syn keyword hamFunction hamversion
syn keyword hamFunction hamwaitidle
syn keyword hamFunction raslasterror
syn keyword hamFunction rfctimezone
syn keyword hamFunction settasklimiter

syn keyword hamStatement if
syn keyword hamStatement else
syn keyword hamStatement elseif
syn keyword hamStatement endif
syn keyword hamStatement do
syn keyword hamStatement loop
syn keyword hamStatement while
syn keyword hamStatement endwhile
syn keyword hamStatement repeat
syn keyword hamStatement until
syn keyword hamStatement for
syn keyword hamStatement endfor
syn keyword hamStatement sub
syn keyword hamStatement endsub
syn keyword hamStatement label


" Strings and characters:
syn region hamString		start=+"+    end=+"+ contains=@Spell
syn region hamString		start=+'+    end=+'+ contains=@Spell

" Numbers:
syn match hamNumber		"-\=\<\d*\.\=[0-9_]\>"

" Comments:
syn region hamHashComment	start=/#/ end=/$/ contains=@Spell
syn cluster hamComment	contains=hamHashComment
syn sync ccomment hamHashComment

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_hamster_syn_inits")
    if version < 508
        let did_hamster_syn_inits = 1
        command -nargs=+ HiLink hi link <args>
    else
        command -nargs=+ HiLink hi def link <args>
    endif

    HiLink hamHashComment	Comment
    HiLink hamSpecial	        Special
    HiLink hamStatement	        Statement
    HiLink hamString	        String
    HiLink hamFunction	        Function

    delcommand HiLink
endif

let b:current_syntax = "hamclass"

" vim:sw=4
