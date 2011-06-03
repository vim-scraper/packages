"  Vim Syntax file for viewing procmail(1) log
"
"  Maintainer: Parv <parv_@yahoo.com>
"  Version:  0.02
"  Modified: Feb 02 2004


"  Can't say or care if it will work w/ versions other than 6.x
if version < 600
  finish
elseif exists("b:current_syntax")
  finish
endif


syn clear
syn match    ProcmailMiscChar               "[^a-zA-Z0-9]"

syn match    ProcmailLowPriorityPM      "^procmail:\s\+"
syn match    ProcmailLowPriority        "^procmail:\s\+\%(Unlocking\|Locking\|Opening\|Acquiring\|Executing\).*$"
syn match    ProcmailLowPriorityAssign  "^procmail:\s\+Assigning .\%(LASTFOLDER\|LOGABSTRACT\|VERBOSE\|INCLUDERC\)=.*$"

syn match    ProcmailScore              "ProcmailScore:\s\+-\?\d\+\s\+-\?\d\+"
syn match    ProcmailRecipeMatch        "Match\%(ed\| on\)"
syn match    ProcmailRecipeNoMatch      "No match on"

syn match    ProcmailEMailMsgID         "[-.\\+_$a-zA-Z0-9]\+@[-.a-zA-Z0-9]\+"

syn match    ProcmailDelivery           "^\%(From\| Subject:\|  Folder:\)\s\+.*$"

syn match    ProcmailProcess            "^procmail: \[\d\+\] [A-Z][a-z][a-z] [A-Z][a-z][a-z] [0-3]\d [0-2]\d:[0-5]\d:[0-5]\d \d\+$"

syn match    ProcmailError              "^procmail: \%(Conflicting\|Suspicious rcfile\|Invalid regexp\|Rescue of unfilterted data\|Bad substitution of\|Couldn't \|Internal error:\|Unable to treat as directory\|Skipped \"\).*$"

syn match    ProcmailErrUnexpect        "^procmail: \%(Unexpected EOL\|Closing brace unexpected\).*$"

syn match    ProcmailErrMiss            "^procmail: \%(Missing \%(rcfile\|action\|closing brace\)\|No space left\|Out of memory\|Insufficient privileges\).*$"

syn match    ProcmailErrLock            "^procmail: \%(Kernel-lock failed\|Kernel-unlock failed\|Couldn't unlock\|Lock failure\|Forced unlock denied\|Couldn't determine implicit lockfile\).*$"

syn match    ProcmailErrExec            "^procmail: \%(Failed to execute\|Non-zero exitcode\|Program failure\|Terminating prematurely\).*$"

syn match    ProcmailErrExcess          "^procmail: \%(Exceeded LINEBUF\|Extraneous\|path too long\|Excessively long rcfile path\|Excessive output quenched\|Assignment to variable with excessively long name\|Quota exceeded while writing\).*$"


hi link ProcmailMiscChar    Special

hi link ProcmailLowPriority        Comment
hi link ProcmailLowPriorityPM      ProcmailLowPriority
hi link ProcmailLowPriorityAssign  ProcmailLowPriority

hi link ProcmailErr          Error
hi link ProcmailErrMiss      Error
hi link ProcmailErrLock      Error
hi link ProcmailErrExec      Error
hi link ProcmailErrExcess    Error

hi link ProcmailEMailMsgID  Underlined

hi link ProcmailScore          Statement
hi link ProcmailRecipeMatch    Statement
hi link ProcmailRecipeNoMatch  Statement

hi link ProcmailProcess   Function

hi link ProcmailDelivery  Identifier


"  My (Parv's) color preferences:
"  ----  ----- ----- ----- ----- ----- ----- -----
"  0: black, 1: red,     2: green, 3: brown/yellow,
"  4: blue,  5: magenta, 6: cyan,  7: grey/white
"  ----  ----- ----- ----- ----- ----- ----- -----
"hi ProcmailLowPriority  term=none  cterm=none  ctermfg=4 ctermbg=0*
"
"hi ProcmailMiscChar  term=none  cterm=none  ctermfg=6
"hi ProcmailEMailMsgID  term=none  cterm=none ctermfg=5  ctermbg=0

"hi ProcmailScore          term=none  cterm=none  ctermfg=3  ctermbg=0
"hi ProcmailRecipeMatch    term=none  cterm=none  ctermfg=2  ctermbg=0
"hi ProcmailRecipeNoMatch  term=none  cterm=none  ctermfg=1  ctermbg=0

"hi ProcmailDelivery  term=reverse  cterm=bold    ctermfg=3  ctermbg=0
"hi ProcmailProcess   term=reverse  cterm=reverse ctermfg=3  ctermbg=0

"hi Error  term=bold  cterm=bold  ctermfg=1  ctermbg=0

let b:current_syntax = "procmail-log"

