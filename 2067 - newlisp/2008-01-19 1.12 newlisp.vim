" Vim syntax file
" Language:     newLISP
" Maintainer:   Cyril Slobin <slobin@ice.ru>
" URL:          http://www.vim.org/scripts/script.php?script_id=2067
" Another URL:  http://wagner.pp.ru/~slobin/vim/syntax/newlisp.vim
" Started at:   2007 Nov 07 (The Great Revolution 90th Anniversary)
" Last change:  2008 Jan 19
" newLISP site: http://www.newlisp.org/

" $Id: newlisp.vim,v 1.12 2008-01-19 18:25:00+03 slobin Exp $

" This was the alternative Vim syntax file for the newLISP language.
" Now it is the official Vim syntax file! I am a celebrity! Wow!

" *** Some syntax quirks of newLISP and how this file treats them: ***
"  
" * In the following text, the word "character" refers to the ASCII
"   characters, and the word "symbol" to the lisp symbols (= atoms).
" 
" * All built-in symbols are in the same class (Statement); functions,
"   definitions, macros and control structures aren't distinguished.
" 
" * Special syntax for : character (colon) is underway, meantime it is
"   just highlighted and not checked otherwise.
"
" * Quoting character ' (apostrophe) is allowed anywhere, even before
"   closing parenthesis. Interpreter uses it this way, this file just
"   does the same.
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
" * Brackets [ ] and braces { } are used by newLISP as the alternate
"   string delimiters. Although, when they doesn't fit into delimiter
"   syntax, they treated by interpreter as the ordinary characters.
"   E.g. {string} is a string while string} is symbol, or [text] is
"   a string delimiter while text] is a symbol. Such a symbols are
"   rather typos than someones intent, so they are marked as errors.
" 
" * The highlighting of errors can be disabled on case-by-case basis.
"   To disable certain class of errors, assign to the global variable
"   g:newlisp_noerror a string containing the following digits:
"
"     1 - for nested left parenthesis in the first column,
"     2 - for unbalanced right parentheses,
"     3 - for ill-formed numbers,
"     4 - for illegal escape sequences in strings,
"     5 - for illegal brackets and braces.
"
"   For example, the following line in your .vimrc
"
"     let g:newlisp_noerror = "12345"
"
"   will disable the highlighting of any errors.
"
" * This syntax file is not compatible with Vim version 5.x and elder.
"   Where have you dug them out? In fact I haven't tested it even with
"   version 6.x - I all do my development with 7.1.175.

if exists("b:current_syntax")
  finish
endif

syntax case match

setlocal iskeyword=33,36-38,42,43,45-47,48-57,60-64,@,92,94,_,124,126

syn region newlispComment oneline start="[;#]" end="$" contains=newlispTodo,@Spell
syn keyword newlispTodo FIXME TODO XXX contained

syn region newlispDocComment start="^;;\(\s\|$\)" end="^\(;;\(\s\|$\)\)\@!" contains=newlispTodo,newlispDocKeyword,newlispDocExample,newlispDocLink,newlispDocItalic,newlispDocMonospace,newlispDocHTMLTag,newlispDocHTMLEntity,@Spell
syn match newlispDocKeyword "^;;\s@\(module\|description\|location\|version\|author\|syntax\|param\|return\)\s"ms=s+3,me=e-1 contained
syn region newlispDocExample start="^;;\s@example$" end="^\(;;\(\s\|$\)\)\@!" contains=newlispDocExampleKeyword  contained
syn match newlispDocExampleKeyword "^;;\s@example$"ms=s+3 contained
syn match newlispDocLink "\s@link\s"ms=s+1,me=e-1 contained
syn match newlispDocItalic "<[^<>]\+>"hs=s+1,he=e-1 contained
syn match newlispDocMonospace "'[^']\+'"hs=s+1,he=e-1 contained
syn match newlispDocHTMLTag "<\/\=\(h1\|h2\|h3\|h4\|i\|em\|b\|tt\|p\|br\|pre\|center\|li\|ul\|blockquote\)>" contained
syn match newlispDocHTMLEntity "&\w\+;" contained

syn cluster newlispListContent contains=TOP,newlispRightParenError
syn region newlispList matchgroup=newlispParenthesis start="(" end=")" contains=@newlispListContent,newlispListError
syn region newlispListError matchgroup=newlispLeftParenError start="^(" matchgroup=newlispParenthesis end=")" contains=@newlispListContent,newlispListError contained
syn match newlispRightParenError ")"

syn match newlispSymbol "\<\([+-]\=\d\)\@!\k\+\>"

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

" This keywords list is based on newLISP v.9.2.16 primes.h file 

syn keyword newlispFunction ! != $ % & * + - / : < << <= = > >= >> NaN? ^ abs acos acosh add address amb and 
syn keyword newlispFunction append append-file apply args array array-list array? asin asinh assoc assoc-set 
syn keyword newlispFunction atan atan2 atanh atom? base64-dec base64-enc bayes-query bayes-train begin beta 
syn keyword newlispFunction betai bind binomial callback case catch ceil change-dir char chop clean close command-line 
syn keyword newlispFunction cond cons constant context context? copy-file cos cosh count cpymem crc32 crit-chi2 
syn keyword newlispFunction crit-z current-line curry date date-value debug dec def-new default define define-macro 
syn keyword newlispFunction delete delete-file delete-url destroy det device difference directory directory? 
syn keyword newlispFunction div do-until do-while doargs dolist dostring dotimes dotree dump dump-symbol dup 
syn keyword newlispFunction empty? encrypt ends-with env erf error-event error-number error-text eval eval-string 
syn keyword newlispFunction exec exists exit exp expand explode factor fft file-info file? filter find find-all 
syn keyword newlispFunction first flat float float? floor flt for for-all fork format fv gammai gammaln gcd 
syn keyword newlispFunction get-char get-float get-int get-long get-string get-url global global? if ifft import 
syn keyword newlispFunction inc index int integer integer? intersect invert irr join lambda? last legal? length 
syn keyword newlispFunction let letex letn list list? load local log lookup lower-case macro? main-args make-dir 
syn keyword newlispFunction map mat match max member min mod mul multiply name net-accept net-close net-connect 
syn keyword newlispFunction net-error net-eval net-listen net-local net-lookup net-peek net-peer net-ping net-receive 
syn keyword newlispFunction net-receive-from net-receive-udp net-select net-send net-send-to net-send-udp net-service 
syn keyword newlispFunction net-sessions new nil? normal not now nper npv nth nth-set null? number? open or 
syn keyword newlispFunction pack parse parse-date peek pipe pmt pop post-url pow pretty-print primitive? print 
syn keyword newlispFunction println prob-chi2 prob-z process protected? push put-url pv quote quote? rand random 
syn keyword newlispFunction randomize read-buffer read-char read-file read-key read-line real-path ref ref-all 
syn keyword newlispFunction ref-set regex remove-dir rename-file replace replace-assoc reset rest reverse rotate 
syn keyword newlispFunction round save search seed seek select semaphore sequence series set set-assoc set-locale 
syn keyword newlispFunction set-nth set-ref set-ref-all setq sgn share signal silent sin sinh sleep slice sort 
syn keyword newlispFunction source sqrt starts-with string string? sub swap sym symbol? symbols sys-error sys-info 
syn keyword newlispFunction tan tanh throw throw-error time time-of-day timer title-case trace trace-highlight 
syn keyword newlispFunction transpose trim true? unicode unify unique unless unpack until upper-case utf8 utf8len 
syn keyword newlispFunction uuid wait-pid when while write-buffer write-char write-file write-line xml-error 
syn keyword newlispFunction xml-parse xml-type-tags zero? \| ~

syn keyword newlispVariable $0 $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 $11 $12 $13 $14 $15 $args $idx $main-args 

syn match newlispColon ":"
syn match newlispComma ","

syn keyword newlispBoolean nil true

if !exists("g:newlisp_noerror")
  let g:newlisp_noerror = ""
endif

function! s:error_color(digit, err, good)
  if g:newlisp_noerror =~ a:digit
    exec "hi def link" a:err a:good
  else
    exec "hi def link" a:err "newlispError"
  endif
endfunction

call s:error_color("1", "newlispLeftParenError", "newlispParenthesis")
call s:error_color("2", "newlispRightParenError", "newlispParenthesis")
call s:error_color("3", "newlispNumberError", "newlispSymbol")
call s:error_color("4", "newlispSpecialError", "newlispString")
call s:error_color("5", "newlispBracketError", "newlispSymbol")

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
hi def link newlispDocExample Comment
hi def link newlispDocExampleKeyword Type
hi def link newlispDocLink Type
hi def link newlispDocItalic CommentItalic
hi def link newlispDocMonospace CommentUnderlined
hi def link newlispDocHTMLTag Statement
hi def link newlispDocHTMLEntity Special
hi def link newlispList Normal
hi def link newlispParenthesis Delimiter
hi def link newlispNestedList Normal
hi def link newlispError Error
hi def link newlispSymbol Identifier
hi def link newlispQuote Type
hi def link newlispQuoteJoiner Normal
hi def link newlispQuotedSymbol Type
hi def link newlispNumber Number
hi def link newlispFloat Float
hi def link newlispString String
hi def link newlispSpecial Special
hi def link newlispFunction Statement
hi def link newlispVariable Statement
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
