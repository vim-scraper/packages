" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
syntax/twee.vim	[[[1
29
"if version < 600
" syntax clear
"elseif exists("b:current_syntax")
" finish
"endif

syntax match tweeDelimiter "[<<|>>|\]\]|\[\[]"
syntax keyword tweeMacro if else endif choice actions set remember print silently endsilently
syntax match tweePassageTitle	"^::.*$"
syntax match tweeVariable "\$\w*"
syntax region tweeLink	matchgroup=tweeDelimiter start="\[\[" end="]]"
syntax match tweeNumber "[0-9]"
syntax region tweeString start="\"" end="\""
syntax region tweeItalics matchgroup=tweeDelimiter start="//" end="//"
syntax region tweeItalics matchgroup=tweeDelimiter start="''" end="''"
syntax region tweeItalics matchgroup=tweeDelimiter start="__" end="__"

hi link tweeNumber Number
hi link tweeString String
hi tweePassageTitle	gui=bold guifg=#aaff80
hi tweeDelimiter guifg=#805060
hi tweeItalics gui=italic
hi tweeBold gui=bold
hi tweeUnderline gui=underline
hi tweeMacro guifg=#ff5060
hi tweeLink gui=underline guifg=#ff77aa
hi tweeVariable guifg=#aaccff

let b:current_syntax = "twee"
ftdetect/twee.vim	[[[1
9
" markdown filetype file
if exists("did\_load\_filetypes")
	finish
endif

augroup twee
	au! BufRead,BufNewFile *.tw   setfiletype twee 
	autocmd BufRead,BufNewFile *.tw    compiler twee
augroup END
compiler/twee.vim	[[[1
6
if exists(":CompilerSet") != 2
  command -nargs=* CompilerSet setlocal <args>
endif
CompilerSet errorformat&		" use the default 'errorformat'
CompilerSet makeprg=twee\ %\ >\ %:r.html
let current_compiler = "twee"
