"	vim:ff=unix ts=4 ss=4
"	vim60:fdm=marker
" \file		titlecase.vim
"
" \brief	Change a range or visual to title case, i.e.
"			Change A Range Or Visual To Title Case, I.E.
"
" \author	Robert KellyIV <Sreny@SverGbc.Pbz> (Rot13ed)
" \note		Released into the Public Domain Sun Sep 22 PDT 2002
" \date		Mon, 07 Oct 2002 20:16 Pacific Daylight Time
" \version	$Id$
" Version: 0.1


if exists("loaded_titlecase")
    finish
endif
let loaded_titlecase = 1

"mark III, as simple as it gets for basic operation I believe. [Feral:265/02@11:22]"
":execute 's/\(\<\l\)/\=toupper(submatch(1))/g'
" also works for ranges "
"'<,'>s/\(\<\l\)/\=toupper(submatch(1))/g


function! <SID>FeralTitleCase_Range() range
	let LR = a:firstline.",".a:lastline
"	echo confirm(LR)

	" expand folds (required) (else the :s operates on the entire fold for each line of the fold times.)
	execute ":silent! ".LR."foldopen!"

	:silent! execute LR.'s/\(\<\l\)/\=toupper(submatch(1))/g'
endfunc

" Note: this works well for a single line visual, multi line visual this does
" NOT work.
function! <SID>FeralTitleCase_Visual()
	" TODO add in a check to abort if we are not the kind of visual we can
	"	handle
	execute ":normal `>i\<CR>\<esc>0d`^"
	execute ":normal `<i\<CR>\<esc>0d`^"
	:silent! execute 's/\(\<\l\)/\=toupper(submatch(1))/g'
	execute ":normal gJkgJ"
endfunc

"*****************************************************************
"* Commands
"*****************************************************************
:command -nargs=0 -range TitleCase :<line1>,<line2>call <SID>FeralTitleCase_Range()|:normal <line1>G
"vnoremap <F5> :s/\(\<\l\)/\=toupper(submatch(1))/g<CR>|:noh
vnoremap <F5> :call <SID>FeralTitleCase_Visual()<CR>



"EOF
