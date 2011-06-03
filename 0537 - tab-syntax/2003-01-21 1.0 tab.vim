" Vim syntax file
" Language:	Guitar tabulatures
" Maintainer:	qmax <qmaxa@sibmail.ru>
" Last Change:	Thu, 29 Aug 2002 20:51:57 +0700
" Features:	Tries to highlight tabulature drawings.
" While different styles of tab-drawing exist, i try to follow one common,
" expected to found as generated with PowerTabEditor:
"
" Tabulature line starts with '|-' '||-' or 'T:-' (string tune),
" contains some dashes and ends with EOL or '|', followed by nondrawing stuff 
" (bar, dash, digit, tilde, parens, *, dot)
" Installing:
" install tab.vim ~/.vim/syntax/tab.vim
" cat > ~/.vimrc: <<EOF
"   augroup filetype
"   autocmd BufRead,BufNewFile *.tab set filetype=tab
"   augroup END
"   EOF
"

" OLGA Copyright and email headers
syn match tabCopyright	/^#.*$/
syn region tabHeader start=/^\(From\|Date\|Subject\|Artist\|Album\|Song\):.*/ end=/$/
" markup of riff definitions
syn match tabMark /<.\{-}>/
" markup of chord or riff in lyrics
syn match tabTag /\[.\{-}\]/

syn case ignore
" in normal regexp:
" start    = | OR || OR T: OR T| (T is any note letter)
" contains = at least one (-.*?-)+? (perl '?' stands for 'as shortest match')
" ends     = || OR |\s OR EOL
"           
syn match tabUlatures /\(\([cdefgabh][#b]\?[:|]\?\)\||\)[^[:space:]]\{-}\(-.\{-}-\)\{-1,}.\{-}\(\(|[|[:space:]]\)\|\([^[:space:]]$\)\)/ contains=tabCanvas,tabNote,tabSymbol
"                      ^^^^^^^^^^^^^ start ^^^^^^^^^^^^^                                      ^^^^^^^^^^^^^^^^^^ end ^^^^^^^^^^^^^^^^

" note marks
syn match tabNote	contained /[0-9()\[\]]/

" symbols used to show slides, bends, pulls, mute, etc: [hpbxr~=^/\]
" matching only before notes or drawing
" (trying ms=s+1 doesn't work)
syn match tabSymbol	contained /[\/\\hpbxr~=^]\+[|\-*.(\[0-9]/me=e-1

" drawing stuff
" including '|' here breakes tabUlature end match - i dunno why
syn match tabCanvas	contained /[\-*.|]/
syn match tabCanvas	contained /[cdefgabh][#b]\?[:|]/

hi def link tabCopyright	PreProc
hi def link tabHeader		Title
hi def link tabMark		Type
hi def link tabTag		Comment

hi def link tabUlatures		Comment
hi def link tabCanvas		String
hi def link tabSymbol		Special
hi def link tabNote		Statement

let b:current_syntax = "tab"
