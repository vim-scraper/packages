" This vim script allows writing
" Hungarian accented letters with flying accent.
" Other characters may be added easily
"
" Written by Tajti Attila in 2003

if exists("loaded_flyaccent")
    finish
endif
let loaded_flyaccent = 1

let s:flyaccentmode = 0

:fun <SID>Enable()
	imap a' �
	imap e' �
	imap i' �
	imap o' �
	imap o: �
	imap o" �
	imap u' �
	imap u: �
	imap u" �

	imap A' �
	imap E' �
	imap I' �
	imap O' �
	imap O: �
	imap O" �
	imap U' �
	imap U: �
	imap U" �

	let s:flyaccentmode = 1

	echo "Flying accents enabled"
:endfun

:fun <SID>Disable()
	iunmap a'
	iunmap e'
	iunmap i'
	iunmap o'
	iunmap o:
	iunmap o"
	iunmap u'
	iunmap u:
	iunmap u"

	iunmap A'
	iunmap E'
	iunmap I'
	iunmap O'
	iunmap O:
	iunmap O"
	iunmap U'
	iunmap U:
	iunmap U"

	let s:flyaccentmode = 0

	echo "Flying accents disabled"
:endfun

:fun SwitchAccendMode()
	if (s:flyaccentmode == 0)
		call <SID>Enable()
	else
		call <SID>Disable()
	endif
:endfun

comm! M call SwitchAccendMode()
