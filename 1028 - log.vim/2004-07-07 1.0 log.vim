" syntax coloring for make output
syn match cMLogMissing  "[\./a-zA-Z0-9_]\+\.[a-zA-Z_]\+: No such .*$"
syn match cMLogMissing  "[\./a-zA-Z0-9_]\+\.[a-zA-Z_]\+: Datei oder Verzeichnis nicht gefunden"
syn match cMLogMissing  "undefined reference to .*$"
syn match cMLogMissing  "Keine Regel vorhanden.*$"
syn match cMLogSource   "[\./a-zA-Z0-9_]\+\.[hci][pxn]\?[lp]\?\(:[0-9]\+\)\+:"
syn match cMLogCurDir   "Entering directory .*$" 

syn match cMLogWarn "\<[wW]arn[iu]ng:.*$"
syn match cMLogErr  "error:.*$"
syn match cMLogErr  "No such .*$"

"syn match cMLogMissing  ".*$" contains=cMLogErr,cMLogSource


hi cMLogWarn    guifg=DarkGreen
hi cMLogErr     guifg=Red
hi cMLogSource  guifg=Blue
hi cMLogCurDir  guifg=Blue
hi cMLogMissing guifg=Red

