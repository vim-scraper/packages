:sy off
:sy on

:sy region par00 start=/^/ end=/$/ extend oneline contains=TOP
:sy region par13 start=/\(	[^	]*\)\{13}$/ end=/$/ extend oneline contains=TOP
:sy region par12 start=/\(	[^	]*\)\{12}$/ end=/$/ extend oneline contains=TOP
:sy region par11 start=/\(	[^	]*\)\{11}$/ end=/$/ extend oneline contains=TOP
:sy region par10 start=/\(	[^	]*\)\{10}$/ end=/$/ extend oneline contains=TOP
:sy region par09 start=/\(	[^	]*\)\{9}$/ end=/$/ extend oneline contains=TOP
:sy region par08 start=/\(	[^	]*\)\{8}$/ end=/$/ extend oneline contains=TOP
:sy region par07 start=/\(	[^	]*\)\{7}$/ end=/$/ extend oneline contains=TOP
:sy region par06 start=/\(	[^	]*\)\{6}$/ end=/$/ extend oneline contains=TOP
:sy region par05 start=/\(	[^	]*\)\{5}$/ end=/$/ extend oneline contains=TOP
:sy region par04 start=/\(	[^	]*\)\{4}$/ end=/$/ extend oneline contains=TOP
:sy region par03 start=/\(	[^	]*\)\{3}$/ end=/$/ extend oneline contains=TOP
:sy region par02 start=/\(	[^	]*\)\{2}$/ end=/$/ extend oneline contains=TOP
:sy region par01 start=/\(	[^	]*\)\{1}$/ end=/$/ extend oneline contains=TOP

:hi par13 ctermfg=gray
:hi par12 ctermfg=darkyellow
:hi par11 ctermfg=darkmagenta
:hi par10 ctermfg=darkred
:hi par09 ctermfg=darkcyan
:hi par08 ctermfg=darkgreen
:hi par07 ctermfg=darkblue
:hi par06 ctermfg=white
:hi par05 ctermfg=yellow
:hi par04 ctermfg=magenta
:hi par03 ctermfg=red
:hi par02 ctermfg=cyan
:hi par01 ctermfg=green
:hi par00 ctermfg=blue

