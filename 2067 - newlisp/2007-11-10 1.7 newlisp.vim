" Vim syntax file
" Language:     newLISP
" Maintainer:   Cyril Slobin <slobin@ice.ru>
" URL:          http://wagner.pp.ru/~slobin/vim/syntax/newlisp.vim
" Started at:   2007 Nov 07 (The Great Revolution 90th Anniversary)
" Last change:  2007 Nov 11
" newLISP site: http://www.newlisp.org/

" $Id: newlisp.vim,v 1.7 2007-11-11 10:20:30+03 slobin Exp $

" This was the alternative Vim syntax file for the newLISP language.
" Now it is the official Vim syntax file! I am a celebrity! Wow!

" * " Some syntax quirks of newLISP and how this file treats them: "
"  
" * In the following text, the word "character" refers to the ASCII
"   characters, and the word "symbol" to the lisp symbols (= atoms).
" 
" * All built-in symbols are in the same class (Statement); functions,
"   definitions, macros and control structures aren't distinguished.
" 
" * Unbalanced right parentheses are marked as errors, left aren't.
" 
" * Special syntax for : character (colon) is underway, meantime it is
"   just highlighted and not checked otherwise.
"
" * Quoting character ' (apostrophe) is allowed anywhere, even before
"   closing parenthesis. Interpreter uses it this way, this file just
"   does the same.
"
" * Brackets [ ] and braces { } are used by newLISP as the alternate
"   string delimiters. Although, when they doesn't fit into delimiter
"   syntax, they treated by interpreter as the ordinary characters.
"   E.g. {string} is a string while string} is symbol, or [text] is
"   a string delimiter while text] is a symbol. Such a symbols are
"   rather typos than someones intent, so they are marked as errors.
" 
" * newLISP interpreter doesn't insist that numbers must be separated
"   from the following numbers or symbols. E.g. the sequence 1.2.3e4e5
"   is valid and equal to three tokens 1.2 .3e4 e5. Such a monster is
"   rather a typo than someones intent, so it is marked as an error.
" 
" * If a backlash character \ in a quoted string is not followed by
"   a valid escape sequence, the interpreter doesn't alarm you, it
"   just silently removes this character from the string. E.g. the
"   string "a\b" is equal to the string "ab". Again, this is rather
"   a typo than someones intent, so it is marked as an error.
"
" * This syntax file is not compatible with Vim version 5.x and elder.
"   Where have you dug them out? In fact I haven't tested it even with
"   version 6.x -- I all do my development with 7.1.130.

if exists("b:current_syntax")
  finish
endif

syntax case match

setlocal iskeyword=33,36-38,42,43,45-47,48-57,60-64,@,92,94,_,124,126

syn region newlispComment oneline start="[;#]" end="$" contains=newlispTodo,@Spell
syn keyword newlispTodo FIXME TODO XXX contained

syn region newlispDocComment oneline start="^;; " end="$" contains=newlispDocKeyword,newlispDocExample,newlispDocLink,newlispDocItalic,newlispDocMonospace,newlispDocHTMLTag,newlispDocHTMLEntity,@Spell
syn match newlispDocKeyword "^;;\s@\(module\|description\|location\|version\|author\|syntax\|param\|return\)\s"ms=s+3,me=e-1 contained
syn match newlispDocExample "^;;\s@example$"ms=s+3 contained
syn match newlispDocLink "\s@link\s"ms=s+1,me=e-1 contained
syn match newlispDocItalic "<[^>]\+>"ms=s+1,me=e-1 contained
syn match newlispDocMonospace "'[^']\+'"ms=s+1,me=e-1 contained
syn match newlispDocHTMLTag "<\/\=\(h1\|h2\|h3\|h4\|i\|em\|b\|tt\|p\|br\|pre\|center\|li\|ul\|blockquote\)>" contained
syn match newlispDocHTMLEntity "&\w\+;" contained

syn region newlispList matchgroup=newlispParenthesis start="(" end=")" contains=TOP,newlispParenError
syn match newlispParenError ")"

syn match newlispSymbol "\<\k\+\>"

syn match newlispQuote "'" nextgroup=newlispQuoteJoiner,newlispQuote,newlispQuotedSymbol
syn match newlispQuoteJoiner "\s\+" nextgroup=newlispQuote,newlispQuotedSymbol contained
syn match newlispQuotedSymbol "\<\([+-]\=\d\)\@!\k\+\>" contained

syn match newlispNumberError "\<[+-]\=\d\k\+\>"
syn match newlispNumberDec "\<[+-]\=[1-9]\d*\>"
syn match newlispNumberOct "\<[+-]\=0\o*\>"
syn match newlispNumberHex "\<[+-]\=0[Xx]\x\+\>"

syn match newlispFloat "\<[+-]\=\d\+[Ee][+-]\=\d\+\>"
syn match newlispFloat "\<[+-]\=\.\d\+\([Ee][+-]\=\d\+\)\=\>"
syn match newlispFloat "\<[+-]\=\d\+\.\d*\([Ee][+-]\=\d\+\)\=\>"

syn region newlispStringQuoted start=+"+ skip=+\\"+ end=+"+ contains=newlispSpecialError,newlispSpecial
syn match newlispSpecialError +\\+ contained
syn match newlispSpecial +\\["\\nrt]+ contained
syn match newlispSpecial +\\\d\d\d+ contained
syn match newlispSpecial +\\x\x\x+ contained

syn match newlispBracketError "[][}{]"

syn region newlispStringBraced start="{" end="}" contains=newlispStringBraced
syn region newlispStringTexted start="\[text\]" end="\[\/text\]"

" This keyword list is based on newLISP v.9.2.5 (symbols) output.

syn keyword newlispBuiltin ! != $ $0 $1 $10 $11 $12 $13 $14 $15 $2 $3 $4 $5 $6 $7 $8 $9 $args 
syn keyword newlispBuiltin $idx $main-args % & * + - / : < << <= = > >= >> ? @ MAIN NaN? ^ 
syn keyword newlispBuiltin abs acos acosh add address amb and append append-file apply args 
syn keyword newlispBuiltin array array-list array? asin asinh assoc atan atan2 atanh atom? 
syn keyword newlispBuiltin base64-dec base64-enc bayes-query bayes-train begin beta betai bind 
syn keyword newlispBuiltin binomial callback case catch ceil change-dir char chop clean close 
syn keyword newlispBuiltin command-line cond cons constant context context? copy-file cos cosh 
syn keyword newlispBuiltin count cpymem crc32 crit-chi2 crit-z current-line curry date date-value 
syn keyword newlispBuiltin debug dec def-new default define define-macro delete delete-file 
syn keyword newlispBuiltin delete-url destroy det device difference directory directory? div 
syn keyword newlispBuiltin do-until do-while doargs dolist dostring dotimes dotree dump dup 
syn keyword newlispBuiltin empty? encrypt ends-with env erf error-event error-number error-text 
syn keyword newlispBuiltin eval eval-string exec exists exit exp expand explode factor fft 
syn keyword newlispBuiltin file-info file? filter find find-all first flat float float? floor 
syn keyword newlispBuiltin flt for for-all format fv gammai gammaln gcd get-char get-float 
syn keyword newlispBuiltin get-int get-long get-string get-url global global? if ifft import 
syn keyword newlispBuiltin inc index int integer integer? intersect invert irr join lambda? 
syn keyword newlispBuiltin last legal? length let letex letn list list? load local log lookup 
syn keyword newlispBuiltin lower-case macro? main-args make-dir map mat match max member min 
syn keyword newlispBuiltin mod mul multiply name net-accept net-close net-connect net-error 
syn keyword newlispBuiltin net-eval net-listen net-local net-lookup net-peek net-peer net-receive 
syn keyword newlispBuiltin net-receive-from net-receive-udp net-select net-send net-send-to 
syn keyword newlispBuiltin net-send-udp net-service net-sessions new nil nil? normal not now 
syn keyword newlispBuiltin nper npv nth nth-set null? number? open or ostype pack parse pipe 
syn keyword newlispBuiltin pmt pop post-url pow pretty-print primitive? print println prob-chi2 
syn keyword newlispBuiltin prob-z process protected? push put-url pv quote quote? rand random 
syn keyword newlispBuiltin randomize read-buffer read-char read-file read-key read-line real-path 
syn keyword newlispBuiltin ref ref-all regex remove-dir rename-file replace replace-assoc reset 
syn keyword newlispBuiltin rest reverse rotate round save search seed seek select semaphore 
syn keyword newlispBuiltin sequence series set set-locale set-nth setq sgn share signal silent 
syn keyword newlispBuiltin sin sinh sleep slice sort source sqrt starts-with string string? 
syn keyword newlispBuiltin sub swap sym symbol? symbols sys-error sys-info tan tanh throw throw-error 
syn keyword newlispBuiltin time time-of-day timer title-case trace trace-highlight transpose 
syn keyword newlispBuiltin trim true true? unify unique unless unpack until upper-case uuid 
syn keyword newlispBuiltin when while write-buffer write-char write-file write-line xml-error 
syn keyword newlispBuiltin xml-parse xml-type-tags zero? \| ~

syn match newlispColon ":"
syn match newlispComma ","

syn keyword newlispBoolean nil true

hi def link newlispParenError newlispError
hi def link newlispNumberError newlispError
hi def link newlispSpecialError newlispError
hi def link newlispBracketError newlispError

hi def link newlispNumberDec newlispNumber
hi def link newlispNumberOct newlispNumber
hi def link newlispNumberHex newlispNumber

hi def link newlispStringQuoted newlispString
hi def link newlispStringBraced newlispString
hi def link newlispStringTexted newlispString

hi def link newlispComment Comment
hi def link newlispTodo Todo
hi def link newlispDocComment Comment
hi def link newlispDocKeyword Type
hi def link newlispDocExample Type
hi def link newlispDocLink Type
hi def link newlispDocItalic CommentItalic
hi def link newlispDocMonospace CommentUnderlined
hi def link newlispDocHTMLTag Statement
hi def link newlispDocHTMLEntity Special
hi def link newlispList Normal
hi def link newlispParenthesis Delimiter
hi def link newlispError Error
hi def link newlispSymbol Identifier
hi def link newlispQuote Type
hi def link newlispQuoteJoiner Normal
hi def link newlispQuotedSymbol Type
hi def link newlispNumber Number
hi def link newlispFloat Float
hi def link newlispString String
hi def link newlispSpecial Special
hi def link newlispBuiltin Statement
hi def link newlispColon Type
hi def link newlispComma Type
hi def link newlispBoolean Boolean

function! s:color_of(where, what)
  let val = synIDattr(hlID("Comment"), a:what, a:where)
  return val == "" || val == -1 ? "" : printf(" %s%s=%s", a:where, a:what, val)
endfunction

function! s:set_colors()
  let colors =  s:color_of("cterm", "fg") . s:color_of("cterm", "bg") . s:color_of("gui", "fg") . s:color_of("gui", "bg")
  exec "hi CommentItalic term=italic cterm=italic gui=italic" . colors
  exec "hi CommentUnderlined term=underline cterm=underline gui=underline" . colors
endfunction

au ColorScheme <buffer> call s:set_colors()
call s:set_colors()

let b:current_syntax = "newlisp"

" vim: textwidth=70

