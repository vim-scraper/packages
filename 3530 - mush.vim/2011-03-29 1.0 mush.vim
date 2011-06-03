" MUSHcode syntax file 
" To be used with .mush and .mux files
" Maintainer: Rick Bird <nveid@yahoo.com>	
" Based on vim Syntax file by: Bek Oberin <gossamer@tertius.net.au>
"
" Last Updated: Mar 2011
" 
" To enable this with .mush and .mux files add the following to syntax filetype.vim
" au BufNewFile,BufRead *.mush,*.mux                    setf mush
" 
" Syntax File is designed to work with various MUSH/MUX syntax preprocessor
" syntaxes as well as MUSH/MUX functions/commands/attributes in general
"
" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif


" PennMUSH & CobraMUSH Standard Functions
syntax keyword mushFunction contained @@ abs accent accname acos
syntax keyword mushFunction contained add after alias align allof
syntax keyword mushFunction contained alphamax alphamin and andflags andlflags
syntax keyword mushFunction contained andlpowers andpowers ansi aposs apply
syntax keyword mushFunction contained arabic2roman art asin atan atan2
syntax keyword mushFunction contained atrlock atrmodtime attrcnt attrib_set attrlock
syntax keyword mushFunction contained band baseconv beep before benchmark
syntax keyword mushFunction contained bnand bnot bor bound brackets
syntax keyword mushFunction contained break bxor c cand cansee
syntax keyword mushFunction contained capstr case caseall cat cbuffer
syntax keyword mushFunction contained cbufferadd cdesc ceil cemit center
syntax keyword mushFunction contained cflags channels checkpass children chr
syntax keyword mushFunction contained clflags clock clone cmds cmogrifier
syntax keyword mushFunction contained cmsgs cobj color comp con
syntax keyword mushFunction contained cond condall config conn controls
syntax keyword mushFunction contained convsecs convtime convutcsecs convutctime cor
syntax keyword mushFunction contained cos cowner create crecall csecs
syntax keyword mushFunction contained cstatus ctime ctitle ctu cusers
syntax keyword mushFunction contained cwho dec decode64 decompose decrypt
syntax keyword mushFunction contained default delete die dig digest
syntax keyword mushFunction contained dist2d dist3d div division divscope
syntax keyword mushFunction contained doing downdiv dynhelp e edefault
syntax keyword mushFunction contained edit element elements elist elock
syntax keyword mushFunction contained emit empire empower encode64 encrypt
syntax keyword mushFunction contained endtag entrances eq escape etimefmt
syntax keyword mushFunction contained eval exit exp extract fdiv
syntax keyword mushFunction contained filter filterbool findable first firstof
syntax keyword mushFunction contained flags flip floor floordiv fmod
syntax keyword mushFunction contained fn fold folderstats followers following
syntax keyword mushFunction contained foreach fraction fullalias fullname functions
syntax keyword mushFunction contained get get_eval getpids grab graball
syntax keyword mushFunction contained grep grepi gt gte hasattr
syntax keyword mushFunction contained hasattrp hasattrpval hasattrval hasdivpower hasflag
syntax keyword mushFunction contained haspower haspowergroup hastype height hidden
syntax keyword mushFunction contained home host hostname html ibreak
syntax keyword mushFunction contained idle idle_average idlesecs idle_times idle_total
syntax keyword mushFunction contained if ifelse ilev iname inc
syntax keyword mushFunction contained index indiv indivall insert inum
syntax keyword mushFunction contained ipaddr isdaylight isdbref isint isnum
syntax keyword mushFunction contained isobjid isword itemize items iter
syntax keyword mushFunction contained itext lalign last lattr lattrp
syntax keyword mushFunction contained lcon lcstr ldelete ldivisions left
syntax keyword mushFunction contained lemit letq level lexits lflags
syntax keyword mushFunction contained link list listq lit ljust
syntax keyword mushFunction contained llockflags llocks lmath ln lnum
syntax keyword mushFunction contained loc localize locate lock lockfilter
syntax keyword mushFunction contained lockflags lockowner locks loctree log
syntax keyword mushFunction contained lparent lpids lplayers lports lpos
syntax keyword mushFunction contained lsearch lsearchr lset lstats lt
syntax keyword mushFunction contained lte lthings lvcon lvexits lvplayers
syntax keyword mushFunction contained lvthings lwho lwhoid mail maildstats
syntax keyword mushFunction contained mailfrom mailfstats maillist mailsend mailstats
syntax keyword mushFunction contained mailstatus mailsubject mailtime malias map
syntax keyword mushFunction contained mapsql match matchall max mean
syntax keyword mushFunction contained median member merge message mid
syntax keyword mushFunction contained min mix mod modulo modulus
syntax keyword mushFunction contained money msecs mtime mudname mudurl
syntax keyword mushFunction contained mul munge mwho mwhoid name
syntax keyword mushFunction contained namegrab namegraball namelist nand nattr
syntax keyword mushFunction contained nattrp ncand nchildren ncon ncond
syntax keyword mushFunction contained ncondall ncor nearby neq nexits
syntax keyword mushFunction contained next nextdbref nlsearch nmwho nor
syntax keyword mushFunction contained not nplayers nscemit nsearch nsemit
syntax keyword mushFunction contained nslemit nsoemit nspemit nsprompt nsremit
syntax keyword mushFunction contained nszemit nthings null num numversion
syntax keyword mushFunction contained nvcon nvexits nvplayers nvthings nwho
syntax keyword mushFunction contained obj objeval objid objmem oemit
syntax keyword mushFunction contained ooref open or ord ordinal
syntax keyword mushFunction contained orflags orlflags orlpowers orpowers owner
syntax keyword mushFunction contained parent parse pcreate pemit pfun
syntax keyword mushFunction contained pghaspower pgpowers pi pickrand pidinfo
syntax keyword mushFunction contained player playermem pmatch poll ports
syntax keyword mushFunction contained pos poss power powergroups powers
syntax keyword mushFunction contained powover primarydivision program prompt pueblo
syntax keyword mushFunction contained quitprog quota r rand randword
syntax keyword mushFunction contained recv regedit regeditall regeditalli regediti
syntax keyword mushFunction contained reglattr reglattrp regmatch regmatchi regnattr
syntax keyword mushFunction contained regnattrp regrab regraball regraballi regrabi
syntax keyword mushFunction contained regrep regrepi regxattr regxattrp remainder
syntax keyword mushFunction contained remit remove repeat replace rest
syntax keyword mushFunction contained restarts restarttime reswitch reswitchall reswitchalli
syntax keyword mushFunction contained reswitchi reverse revwords right rjust
syntax keyword mushFunction contained rloc rnum roman2arabic room root
syntax keyword mushFunction contained round s scan scramble search
syntax keyword mushFunction contained secs secure sent set setdiff
syntax keyword mushFunction contained setinter setq setr setunion sha0
syntax keyword mushFunction contained shl shr shuffle sign signal
syntax keyword mushFunction contained sin slev sort sortby sortkey
syntax keyword mushFunction contained soundex soundlike soundslike space speak
syntax keyword mushFunction contained speakpenn spellnum splice sql sqlescape
syntax keyword mushFunction contained sqrt squish ssl starttime stats
syntax keyword mushFunction contained stddev step stext strallof strcat
syntax keyword mushFunction contained strfirstof stringsecs strinsert stripaccents stripansi
syntax keyword mushFunction contained stripfansi strlen strmatch strreplace sub
syntax keyword mushFunction contained subj switch switchall t table
syntax keyword mushFunction contained tag tagwrap tan tel terminfo
syntax keyword mushFunction contained testlock textentries textfile time timefmt
syntax keyword mushFunction contained timestring tr trigger trim trimpenn
syntax keyword mushFunction contained trimtiny trunc type u ucstr
syntax keyword mushFunction contained udefault ufun ulambda uldefault ulocal
syntax keyword mushFunction contained unique unsetq updiv utctime v
syntax keyword mushFunction contained vadd val valid vcross vdim
syntax keyword mushFunction contained vdot version visible vmag vmax
syntax keyword mushFunction contained vmin vmul vsub vunit wait
syntax keyword mushFunction contained where width wildgrep wildgrepi wipe
syntax keyword mushFunction contained wordpos words wrap xattr xattrp
syntax keyword mushFunction contained xcon xexits xget xmwho xmwhoid
syntax keyword mushFunction contained xor xplayers xthings xvcon xvexits
syntax keyword mushFunction contained xvplayers xvthings xwho xwhoid zemit
syntax keyword mushFunction contained zfun zmwho zone zwho

" only highligh functions when they have an in-bracket immediately after
syntax match mushFunctionBrackets  "\i*(" contains=mushFunction
"
" regular mush commands
"
syntax keyword mushAtCommandList contained @@ @ALLHALT @ALLQUOTA @ASSERT @ATRCHOWN @ATRLOCK @ATTRIBUTE @BOOT
syntax keyword mushAtCommandList contained @BREAK @CEMIT @CHANNEL @CHAT @CHOWN @CHOWNALL @CHZONE @CHZONEALL
syntax keyword mushAtCommandList contained @CLOCK @CLONE @COBJ @COMMAND @CONFIG @CPATTR @CREATE @CRON
syntax keyword mushAtCommandList contained @CRPLOG @DBCK @DECOMPILE @DESTROY @DIG @DISABLE @DIVISION @DOING
syntax keyword mushAtCommandList contained @DOLIST @DRAIN @DUMP @EDIT @ELOCK @EMIT @EMPOWER @ENABLE
syntax keyword mushAtCommandList contained @ENTRANCES @EUNLOCK @FIND @FIRSTEXIT @FLAG @FORCE @FUNCTION @GREP
syntax keyword mushAtCommandList contained @HALT @HIDE @HOOK @INCLUDE @KICK @LEMIT @LEVEL @LINK
syntax keyword mushAtCommandList contained @LIST @LISTMOTD @LOCK @LOG @LOGWIPE @LSET @MAIL @MALIAS
syntax keyword mushAtCommandList contained @MAP @MAPSQL @MESSAGE @MOTD @MVATTR @NAME @NEWPASSWORD @NOTIFY
syntax keyword mushAtCommandList contained @NSCEMIT @NSEMIT @NSLEMIT @NSOEMIT @NSPEMIT @NSPROMPT @NSREMIT @NSZEMIT
syntax keyword mushAtCommandList contained @NUKE @OEMIT @OPEN @PARENT @PASSWORD @PCREATE @PEMIT @POLL
syntax keyword mushAtCommandList contained @POOR @POWER @POWERGROUP @PROGRAM @PROMPT @PS @PURGE @QUOTA
syntax keyword mushAtCommandList contained @READCACHE @RECYCLE @REJECTMOTD @REMIT @RESTART @RETRY @RWALL @SCAN
syntax keyword mushAtCommandList contained @SD @SEARCH @SELECT @SET @SHUTDOWN @SITELOCK @SNOOP @SQL
syntax keyword mushAtCommandList contained @SQUOTA @STATS @SU @SWEEP @SWITCH @TELEPORT @TRIGGER @ULOCK
syntax keyword mushAtCommandList contained @UNDESTROY @UNLINK @UNLOCK @UNRECYCLE @UPTIME @UUNLOCK @VERB @VERSION
syntax keyword mushAtCommandList contained @WAIT @WALL @WARNINGS @WCHECK @WEBPASSWD @WHEREIS @WIPE @WIZMOTD
syntax keyword mushAtCommandList contained @WIZWALL @ZCLONE @ZEMIT
syntax match mushCommand  "@\i\I*" contains=mushAtCommandList


syntax keyword mushCommand AHELP ANEWS ATTRIB_SET BRIEF BUY
syntax keyword mushCommand CHANGES CHELP DESERT DISMISS DOING
syntax keyword mushCommand DROP EMPTY ENTER EXAMINE FOLLOW
syntax keyword mushCommand GET GIVE GOTO HELP HOME
syntax keyword mushCommand HUH_COMMAND INVENTORY KILL LEAVE LOOK
syntax keyword mushCommand NEWS PAGE POSE RULES SAY
syntax keyword mushCommand SCORE SEMIPOSE SESSION SLAY SPECIALNEWS
syntax keyword mushCommand TAKE TEACH THINK UNFOLLOW UNIMPLEMENTED_COMMAND
syntax keyword mushCommand USE WARN_ON_MISSING WHISPER WHO WITH

syntax match mushSpecial     "\*\|!\|=\|-\|\\\|+"
syntax match mushSpecial2 contained     "\*"

syn region    mushString         start=+L\="+ skip=+\\\\\|\\"+ end=+"+ contains=mushSpecial,mushSpecial2,@Spell


syntax match mushIdentifier   "&[^ ]\+"

syntax match mushVariable   "%r\|%t\|%cr\|%[A-Za-z0-9]\+\|%#\|##\|here"

" numbers
syntax match mushNumber	+[0-9]\++

" A comment line starts with a or # or " at the start of the line
" or an @@
syntax keyword mushTodo contained	TODO FIXME XXX
syntax cluster mushCommentGroup contains=mushTodo
syntax match	mushComment	"^\s*@@.*$"	contains=mushTodo
syntax match mushComment "^#[^define|^ifdef|^else|^pragma|^ifndef|^echo|^elif|^undef|^warning].*$" contains=mushTodo
syntax match mushComment "^#$" contains=mushTodo
syntax region mushComment        matchgroup=mushCommentStart start="/@@" end="@@/" contains=@mushCommentGroup,mushCommentStartError,mushCommentString,@Spell
syntax region mushCommentString  contained start=+L\=\\\@<!"+ skip=+\\\\\|\\"+ end=+"+ end=+@@/+me=s-1 contains=mushCommentSkip
syntax match  mushCommentSkip    contained "^\s*@@\($\|\s\+\)"


syntax match mushCommentStartError display "/@@"me=e-1 contained

" syntax match	mushComment	+^".*$+	contains=mushTodo
" Work on this one
" syntax match	mushComment	+^#.*$+	contains=mushTodo

syn region      mushPreCondit      start="^\s*\(%:\|#\)\s*\(if\|ifdef\|ifndef\|elif\)\>" skip="\\$" end="$" end="//"me=s-1 contains=mushComment
syn match       mushPreCondit      display "^\s*\(%:\|#\)\s*\(else\|endif\)\>"

syn cluster     mushPreProcGroup   contains=mushPreCondit,mushIncluded,mushInclude,mushDefine,mushSpecial,mushString,mushCommentSkip,mushCommentString,@mushCommentGroup,mushCommentStartError

syn region      mushIncluded       display contained start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match       mushIncluded       display contained "<[^>]*>"
syn match       mushInclude        display "^\s*\(%:\|#\)\s*include\>\s*["<]" contains=mushIncluded
syn region	mushDefine		start="^\s*\(%:\|#\)\s*\(define\|undef\)\>" skip="\\$" end="$" end="//"me=s-1 contains=ALLBUT,@mushPreProcGroup,@Spell
syn region	mushPreProc	start="^\s*\(%:\|#\)\s*\(pragma\>\|line\>\|warning\>\|warn\>\|error\>\)" skip="\\$" end="$" keepend contains=ALLBUT,@mushPreProcGroup


syntax region	mushFuncBoundaries start="\[" end="\]" contains=mushFunction,mushFlag,mushAttributes,mushNumber,mushCommand,mushVariable,mushSpecial2

" FLAGS
syntax keyword mushFlag ABODE ANIMAL ANSI ANTIMATTER_LINES
syntax keyword mushFlag AUDIBLE AUTH_PARENT BLIND BUILDER CHAN_USEFIRSTMATCH
syntax keyword mushFlag CHOWN_OK CLOUDY COLOR DARK DEBUG
syntax keyword mushFlag DEBUG_SILENT DESTROY_OK EMPIRE ENTER_OK FIXED
syntax keyword mushFlag FLOATING GAGGED HALT HAVEN ICEXIT
syntax keyword mushFlag ICFUNCS INHERIT INHERITABLE JUDGE JUMP_OK
syntax keyword mushFlag JURY_OK KEEPALIVE LEAVE_BEHIND LIGHT LINK_OK
syntax keyword mushFlag LISTEN_PARENT LOUD MISTRUST MONITOR MUTE
syntax keyword mushFlag MYOPIC NOACCENTS NOBEAM NO_COMMAND NO_LEAVE
syntax keyword mushFlag NOSPOOF NO_TEL NO_WARN NPC ON-VACATION
syntax keyword mushFlag OPAQUE ORPHAN PARALYZED PARANOID PUPPET
syntax keyword mushFlag QUIET RACE ROYALTY RPAPPROVED RPMODE
syntax keyword mushFlag SAFE SHARED STICKY SUSPECT TERSE
syntax keyword mushFlag TRACK_MONEY TRANSPARENT TRUST TURBOLIFT UNFINDABLE
syntax keyword mushFlag UNINSPECTED VERBOSE VISUAL WEIRDPROMPTS WEIRDSITE
syntax keyword mushFlag WIZARD ZCLONE_OK Z_TEL

" Basic Attributes
syntax keyword mushAttribute aahear abuy aclone aconnect adeath adesc adescribe
syntax keyword mushAttribute adestroy adfail adisconnect adrop aefail aenter afail
syntax keyword mushAttribute afailure afollow agfail agive ahear aidescribe akill
syntax keyword mushAttribute aleave alfail alias amail amhear amove aname apay apayment
syntax keyword mushAttribute areceive arfail asdin asdout asucc asuccess atfail atport
syntax keyword mushAttribute aufail aunfollow aunidle ause away azenter azleave buy
syntax keyword mushAttribute charges chatformat comment conformat cost daily death
syntax keyword mushAttribute debugforwardlist desc descformat describe dfail drop
syntax keyword mushAttribute ealias efail enter exitformat exitto fail failure filter
syntax keyword mushAttribute follow followers following forwardlist gfail give haven
syntax keyword mushAttribute hourly idesc idescformat idescribe idle idle_timeout
syntax keyword mushAttribute infilter inprefix invformat kill lalias last lastactivity
syntax keyword mushAttribute lastfailed lastip lastlogout lastpaged lastsite leave
syntax keyword mushAttribute lfail listen mailcurf mailfilter mailfilters mailfolders
syntax keyword mushAttribute mailforwardlist mailsignature move nameaccent nameformat
syntax keyword mushAttribute obuy odeath odesc odescribe odfail odrop oefail oenter
syntax keyword mushAttribute ofail ofailure ofollow ogfail ogive oidescribe okill
syntax keyword mushAttribute oleave olfail omove oname opay opayment oreceive orfail
syntax keyword mushAttribute osucc osuccess otfail otport oufail ounfollow ouse outpageformat
syntax keyword mushAttribute oxenter oxleave oxmove oxtport ozenter ozleave pageformat
syntax keyword mushAttribute pay payment prefix pricelist program queue receive registered_email
syntax keyword mushAttribute reject rfail rquota runout semaphore sex speechmod startup
syntax keyword mushAttribute succ success tfail tfprefix tport tz ufail unfollow
syntax keyword mushAttribute use va vb vc vd ve vf vg vh vi vj vk vl vm vn vo vp
syntax keyword mushAttribute vq vr vrml_url vs vt vu vv vw vx vy vz wa wb wc wd we
syntax keyword mushAttribute wf wg wh wi wj wk wl wm wn wo wp wq wr ws wt wu wv ww
syntax keyword mushAttribute wx wy wz xa xb xc xd xe xf xg xh xi xj xk xl xm xn xo
syntax keyword mushAttribute xp xq xr xs xt xu xv xw xx xy xz zenter zleave

if version >= 508 || !exists("did_mush_syntax_inits")
  if version < 508
    let did_mush_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  " The default methods for highlighting.  Can be overridden later
  HiLink mushAttribute  Constant
  HiLink mushCommand    Function
  HiLink mushNumber     Number
  HiLink mushSetting    PreProc
  HiLink mushFunction   Statement
  HiLink mushVariable   Identifier
  HiLink mushSpecial    Special
  HiLink mushTodo       Todo
  HiLink mushFlag       Special
  HiLink mushIdentifier Identifier
  HiLink mushDefine     Macro
  HiLink mushPreProc    PreProc
  HiLink mushPreProcGroup PreProc 
  HiLink mushPreCondit PreCondit
  HiLink mushIncluded cString
  HiLink  mushInclude Include



" Comments
  HiLink mushCommentStart mushComment
  HiLink mushComment    Comment
  HiLink mushCommentString mushString


  delcommand HiLink
endif

let b:current_syntax = "mush"

" mush: ts=17
