" Vim syntax file
" Language:	WikkiTikkiTavi
" Maintainer:	Hermann Schwarting <vimtips@knackich.de>
" Last Change:	2006-09-24
" Bugs:
"  - Line continuation \ breaks almost everything.
"  - I found no pattern to discern ''italic'' from '''bold''', so they are
"    both emphasis now. Things like ''italic with '''bold''' inside'' are not
"    handled.

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn cluster	taviFormatting contains=taviNone,taviWikiName,taviInterWiki,taviFreeLink,taviLink,taviNamedLink,taviEmph,taviTT,taviSuper,taviSub,taviStrike,taviUnder,taviCont
" ```
syn region	taviNone start="```" end="```" oneline

" WikiNames
syntax match	taviWikiName "\u\a*\l\+\u\a*\(/\(\u\a*\)\+\)\?"

" InterWiki:syntax
syntax match	taviInterWiki +[A-Za-z0-9]\+:\([^[:space:]|/"'[:cntrl:]]*/\)*[^[:space:]|"'[:cntrl:]]*[/=&~A-Za-z0-9]+

" ((free links|description))
syntax match	taviFreeLink "(([^|()]\+\(|[^(#]\+\)\?\(#\a[-A-Za-z0-9_:.]*\)\?))"	contains=taviFreeLinkDesc
syntax match	taviFreeLinkDesc "|[^#)]*"hs=s+1	contained

" links://
syntax match	taviLink +\(https\?:\|mailto:\|ftp:\|gopher:\|news:\|file:\)\([^ |/"']*/\)*[^ |"'[:cntrl:]]*[A-Za-z0-9/?=&~_]+

" [links://] and [links:// named]
syntax match	taviNamedLink +\[\(https\?:\|mailto:\|ftp:\|gopher:\|news:\|file:\)\([^ |/"']*/\)*[^ |/"'[:cntrl:]]*[A-Za-z0-9/?=&~_]\(\s.\{-}\)\?\]+	contains=taviLink

" ''' '''   ** **   '' ''   // //
syn region	taviEmph start="''\+" end="''\+" oneline contains=@taviFormatting
syn region	taviEmph start="\*\*" end="\*\*" oneline contains=@taviFormatting
syn region	taviEmph start="//" end="//" oneline contains=@taviFormatting

" {{ }}
syn region	taviTT start="{{" end="}}" oneline contains=@taviFormatting

" ^^ ^^
syn region	taviSuper start="\^\^" end="\^\^" oneline contains=@taviFormatting
",, ,,
syn region	taviSub start=",," end=",," oneline contains=@taviFormatting
"-- --
syn region	taviStrike start="--" end="--" oneline contains=@taviFormatting
"++ ++
syn region	taviUnder start="++" end="++" oneline contains=@taviFormatting

" ----
syn match	taviLine "-----*"

" :  ::
" #  ##
" *  **
" ; :
syn match	taviEnumInd "^\([:#*;][:#*;[:space:]]*\)\?[:#*;]\+"

" == ==
syn region	taviH1 start="^= " end=" =$" oneline contains=@taviFormatting
syn region	taviH2 start="^== " end=" ==$" oneline contains=@taviFormatting
syn region	taviH3 start="^=== " end=" ===$" oneline contains=@taviFormatting
syn region	taviH4 start="^==== " end=" ====$" oneline contains=@taviFormatting
syn region	taviH5 start="^===== " end=" =====$" oneline contains=@taviFormatting
syn region	taviH6 start="^====== " end=" ======$" oneline contains=@taviFormatting

" || ||
syn match	taviTable "^||[^|]*\(||[^|]*\)*||\(\s*\\\)\?$" contains=taviTableBars,@taviFormatting
syn match	taviTableBars "||\({[^}]*}\)\?" contained contains=taviTableCurly
syn match	taviTableCurly "^{[^}]*}" contained contains=taviTableBars

" <code> </code> <phpcode> </phpcode>
syn region	taviCode start="^\s*<\(php\)\?code>\s*$" end="^\s*</\(php\)\?code>\s*$"

" \\
" \
syn match	taviCont "\\\?\\$"

" [[Macro Parameter]]
syntax match	taviMacro	"\[\[.*\]\]"	contains=taviMacroParm
syntax match	taviMacroParm	"\s[^\]]*"	contained

hi def link taviNone	Comment

hi def link	taviWikiName	Keyword

hi def link	taviInterWiki	Underlined

hi def link	taviFreeLink	Keyword
hi def link	taviFreeLinkDesc	Special

hi def link	taviLink	Underlined
hi def link	taviNamedLink	Special

hi def	taviEmph	term=bold cterm=bold gui=bold
hi def link	taviTT		String
hi def link	taviSuper	String
hi def link	taviSub		String
hi def	taviUnder	term=underline cterm=underline gui=underline
hi def link	taviStrike	String

hi def link	taviLine	Comment

hi def link	taviEnumInd	Type

hi def link	taviH1	Title
hi def link	taviH2	Title
hi def link	taviH3	Title
hi def link	taviH4	Title
hi def link	taviH5	Title
hi def link	taviH6	Title

hi def link taviTableCurly	Special
hi def link	taviTableBars	Type

hi def link	taviCode	Comment

hi def link	taviCont	Todo

hi def link	taviMacro	Keyword
hi def link	taviMacroParm	Special

let b:current_syntax = "tavi"
