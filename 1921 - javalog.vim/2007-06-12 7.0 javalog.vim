" Vim syntax file
" Language:	java.util.logging output
" Maintainer:	rt_davies@yahoo.com
" Last Change:	2007 Jun 12

" Quit when a (custom) syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

" Case sensitive
syn case 	match

" Custom color defs
hi javalogHeadColor guifg=#ACACFF
hi javalogInfoColor guifg=Green guibg=DarkGray
hi javalogFineColor guifg=#000099 guibg=DarkGray
hi javalogFinerColor guifg=#FF9999 guibg=DarkGray
hi javalogFinestColor guifg=#FFFFCC guibg=DarkGray

" Severe
syntax match javalogLevelSevere /^SEVERE:/
hi link javalogLevelSevere ErrorMsg

" Warning
syntax match javalogLevelWarn /^WARNING:/
hi link javalogLevelWarn WarningMsg

" Info
syntax match javalogInfo /^INFO:/
hi link javalogInfo javalogInfoColor

" Fine
syntax match javalogFine /^FINE:/
hi link javalogFine javalogFineColor

" Finer
syntax match javalogFiner /^FINER:/
hi link javalogFiner javalogFinerColor

" Finest
syntax match javalogFinest /^FINEST:/
hi link javalogFinest javalogFinestColor

" Log entry headers
syntax region javalogHeader start=/^Jan / end=/$/
syntax region javalogHeader start=/^Feb / end=/$/
syntax region javalogHeader start=/^Mar / end=/$/
syntax region javalogHeader start=/^Apr / end=/$/
syntax region javalogHeader start=/^May / end=/$/
syntax region javalogHeader start=/^Jun / end=/$/
syntax region javalogHeader start=/^Jul / end=/$/
syntax region javalogHeader start=/^Aug / end=/$/
syntax region javalogHeader start=/^Sep / end=/$/
syntax region javalogHeader start=/^Oct / end=/$/
syntax region javalogHeader start=/^Nov / end=/$/
syntax region javalogHeader start=/^Dec / end=/$/
hi link javalogHeader javalogHeadColor

" Stack traces
syntax region javaStacktrace start=/^com./ end=/$/
syntax region javaStacktrace start=/^org./ end=/$/
syntax region javaStacktrace start=/^net./ end=/$/
syntax region javaStacktrace start=/^java./ end=/$/
syntax region javaStacktrace start=/^	at/ end=/$/
hi link javaStacktrace Identifier

let b:current_syntax = "javalog"

" vim: ts=8 sw=2
