" Vim syntax file
" Language:     newLISP
" Maintainer:   Cyril Slobin <slobin@ice.ru>
" URL:          http://wagner.pp.ru/~slobin/vim/syntax/newlisp.vim
" Started at:   2007 Nov 07 (The Great Revolution 90th Anniversary)
" Last change:  2007 Nov 10
" newLISP site: http://www.newlisp.org/

" $Id: newlisp.vim,v 1.3 2007-11-10 06:45:00+03 slobin Exp $

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
" * Characters ' (apostrophe), : (colon) and , (comma) are highlighted
"   but not checked for their special syntax yet. More smart tests are
"   on their way.
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
"   Where have you dug them out?

if exists("b:current_syntax")
  finish
endif

syntax case match

setlocal iskeyword=33,36-38,42,43,45-47,48-57,60-64,@,92,94,_,124,126

syn region newlispComment oneline start="[;#]" end="$" contains=newlispDocKeyword,newlispDocHTMLTag,newlispDocHTMLEntity,newlispTodo,@Spell
syn match newlispDocKeyword "[;#]\s\+@\w\+" contains=newlispCommentLeader,@Spell contained
syn match newlispCommentLeader "[;#]\s\+" contained
syn match newlispDocHTMLTag "<\/\=\w\+>" contained
syn match newlispDocHTMLEntity "&\w\+;" contained
syn keyword newlispTodo FIXME TODO XXX contained

syn region newlispList matchgroup=newlispParenthesis start="(" end=")" contains=TOP,newlispParenError
syn match newlispParenError ")"

syn match newlispSymbol "\<\k\+\>"

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

syn match newlispMagicCharacter "[':,]"

" This keyword list is based on newLISP v.9.2.4 (symbols) output.

syn keyword newlispBuiltin  ! != $ $0 $1 $10 $11 $12 $13 $14 $15 $2 $3 $4 $5 $6 $7 $8 $9 $args 
syn keyword newlispBuiltin  $idx $main-args % & * + - / < << <= = > >= >> ? @ MAIN NaN? ^ abs 
syn keyword newlispBuiltin  acos acosh add address amb and append append-file apply args array 
syn keyword newlispBuiltin  array-list array? asin asinh assoc atan atan2 atanh atom? base64-dec 
syn keyword newlispBuiltin  base64-enc bayes-query bayes-train begin beta betai bind binomial 
syn keyword newlispBuiltin  callback case catch ceil change-dir char chop clean close command-line 
syn keyword newlispBuiltin  cond cons constant context context? copy-file cos cosh count cpymem 
syn keyword newlispBuiltin  crc32 crit-chi2 crit-z current-line curry date date-value debug 
syn keyword newlispBuiltin  dec def-new default define define-macro delete delete-file delete-url 
syn keyword newlispBuiltin  destroy det device difference directory directory? div do-until 
syn keyword newlispBuiltin  do-while doargs dolist dostring dotimes dotree dump dup empty? encrypt 
syn keyword newlispBuiltin  ends-with env erf error-event error-number error-text eval eval-string 
syn keyword newlispBuiltin  exec exists exit exp expand explode factor fft file-info file? filter 
syn keyword newlispBuiltin  find find-all first flat float float? floor flt for for-all format 
syn keyword newlispBuiltin  fv gammai gammaln gcd get-char get-float get-int get-long get-string 
syn keyword newlispBuiltin  get-url global global? if ifft import inc index int integer integer? 
syn keyword newlispBuiltin  intersect invert irr join lambda? last legal? length let letex letn 
syn keyword newlispBuiltin  list list? load local log lookup lower-case macro? main-args make-dir 
syn keyword newlispBuiltin  map mat match max member min mod mul multiply name net-accept net-close 
syn keyword newlispBuiltin  net-connect net-error net-eval net-listen net-local net-lookup net-peek 
syn keyword newlispBuiltin  net-peer net-receive net-receive-from net-receive-udp net-select 
syn keyword newlispBuiltin  net-send net-send-to net-send-udp net-service net-sessions new nil 
syn keyword newlispBuiltin  nil? normal not now nper npv nth nth-set null? number? open or ostype 
syn keyword newlispBuiltin  pack parse pipe pmt pop post-url pow pretty-print primitive? print 
syn keyword newlispBuiltin  println prob-chi2 prob-z process protected? push put-url pv quote 
syn keyword newlispBuiltin  quote? rand random randomize read-buffer read-char read-file read-key 
syn keyword newlispBuiltin  read-line real-path ref ref-all regex remove-dir rename-file replace 
syn keyword newlispBuiltin  replace-assoc reset rest reverse rotate round save search seed seek 
syn keyword newlispBuiltin  select semaphore sequence series set set-locale set-nth setq sgn 
syn keyword newlispBuiltin  share signal silent sin sinh sleep slice sort source sqrt starts-with 
syn keyword newlispBuiltin  string string? sub swap sym symbol? symbols sys-error sys-info tan 
syn keyword newlispBuiltin  tanh throw throw-error time time-of-day timer title-case trace trace-highlight 
syn keyword newlispBuiltin  transpose trim true true? unify unique unless unpack until upper-case 
syn keyword newlispBuiltin  uuid when while write-buffer write-char write-file write-line xml-error 
syn keyword newlispBuiltin  xml-parse xml-type-tags zero? \| ~

syn keyword newlispBoolean nil true

hi def link newlispCommentLeader newlispComment

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
hi def link newlispDocKeyword Type
hi def link newlispDocHTMLTag PreProc
hi def link newlispDocHTMLEntity Special
hi def link newlispTodo Todo
hi def link newlispList Normal
hi def link newlispParenthesis Delimiter
hi def link newlispError Error
hi def link newlispSymbol Identifier
hi def link newlispNumber Number
hi def link newlispFloat Float
hi def link newlispString String
hi def link newlispSpecial Special
hi def link newlispMagicCharacter Type
hi def link newlispBuiltin Statement
hi def link newlispBoolean Boolean

let b:current_syntax = "newlisp"

" vim: textwidth=70

