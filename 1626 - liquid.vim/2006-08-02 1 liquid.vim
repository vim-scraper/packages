" liquid.vim: Vim syntax file for Liquid Templates
" Language:	Liquid
" Maintainer:	Eugen Minciu <minciue@gmail.com>
" Last Change: 2006 July 31
" Version: 0.1
" A great deal of thanks to Tim Pope who has been very helpful with my
" noobness. Thank you Tim!
"
" Modelled after eruby.vim written by Michael Brailsford. Thank you Michael.
" Enjoy ;)

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

set ft=html
unlet b:current_syntax 

" The Liquid Tags
syn keyword LiquidKeyword assign capture case cycle else endcase endfor endif for if in include unless when with contained

" These are filters that are made available inside a for loop
syn keyword LiquidSpecial forloop length index index0 rindex rindex0 size downcase upcase capitalize truncate truncatewords strip_html join sort date first last size contained

" These are the operators Liquid supports.
syn match LiquidEqual "==" contained
syn match LiquidNotEqual "!=" contained
syn match LiquidLess "<" contained
syn match LiquidMore ">" contained
syn match LiquidLessOrEqual "<=" contained
syn match LiquidMoreOrEqual ">=" contained


" Pipes:
syn match LiquidPipe '|' contained

" Strings:
syn region LiquidString matchgroup=LiquidSpecial start=+"+ skip=/\\"/ end=+"+ contained
syn region LiquidString matchgroup=LiquidSpecial start=+'+ skip=/\\'/ end=+'+ contained

" Numbers:
syn match LiquidNumber "-\=\<\d*\.\=[0-9_]\>" contained

syn match RubyInstanceVariable "@\l\w*" contained


" Output:

syn region LiquidTags matchgroup=LiquidDelim start='{%' keepend end='%}' contains=RubyInstanceVariable,LiquidKeyword,LiquidSpecial,LiquidString,LiquidPipe,LiquidNumber,LiquidEqual,LiquidNotEqual,LiquidLess,LiquidMore,LiquidLessOrEqual,LiquidMoreOrEqual 
syn region LiquidOutput matchgroup=LiquidDelim start='{{' end='}}' 

" Comments:
syn region LiquidComment start="{%\s\+comment\s\+%}" end="{%\s\+endcomment\s\+%}"

hi link LiquidComment Comment
hi link LiquidNumber Number
hi link LiquidString String

hi link LiquidKeyword Keyword 
hi link LiquidPipe Operator 
hi link LiquidSpecial Special
hi link LiquidEqual Operator
hi link LiquidNotEqual Operator
hi link LiquidLess Operator
hi link LiquidMore Operator 
hi link LiquidLessOrEqual Operator 
hi link LiquidMoreOrEqual Operator 
hi link RubyInstanceVariable Identifier

hi link LiquidDelim Special
let b:current_syntax="liquid"
