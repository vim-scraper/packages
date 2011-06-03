" Vim syntax file
" Language:	GAMS
" Maintainer:  David Vonka

syn clear
syn case ignore
syn sync lines=250
setlocal iskeyword+=$

syn keyword GAMSkwds solve minimizing maximizing using model positive variable[s] equation[s] parameter[s] scalar[s] set[s] file put putclose loop endloop if then else endif for to do endfor yes no alias display option ord card execute inf table
syn match GAMSdol /^$.*/
syn match GAMScomment1 /?.*/
syn region GAMScomment2 start=/\/?/ end=/?\//
syn region GAMScomment3 start=/$ontext/ end=/$offtext/
syn match GAMScomment4 /^\*.*/
syn region GAMSstring start=/"/ end=/"/
syn region GAMSputstring start=/'/ end=/'/

hi GAMSkwds gui=bold guifg='Blue'
hi GAMSdol guifg='Red'
hi GAMSstring guifg='Blue'
hi GAMSputstring guifg='Blue'
hi GAMScomment1 guifg='Gray'
hi GAMScomment2 guifg='Gray'
hi GAMScomment3 guifg='Gray'
hi GAMScomment4 guifg='Gray'


