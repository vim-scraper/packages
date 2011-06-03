" Vim compiler file
" Compiler:     Perl syntax checks (perl -Wc) v1.0
" Maintainer:   Lukas Zapletal <Lukas.Zapletal@seznam.cz>
" Last Change:  2001 Sep 10

if exists("current_compiler")
  finish
endif
let current_compiler = "perl"

" there is no pipes under windows, vi use temp file
" and as perl outputs to stderr this have to be handled corectly
if has("win32")
	setlocal shellpipe=1>&2\ 2>
endif

setlocal makeprg=perl\ -Wc\ %

" sample error:   Useless use of a constanst at test.pl line 5.
setlocal errorformat=%m\ at\ %f\ line\ %l.,
                    \%-G%.%# " ignore any lines that didn't match one of the patterns above
