" Vim script file
" Language:	Use with scribble syntax
" Last Change:	2011-09-30
" Author:	Tim Brown <tim.brown@timb.net>
" Version: 1.1
"
" Suggestions and bug reports (and fixes!) are solicited by the author.
"
" This script works with racket.vim (and scribble.vim if you have it) to
" automatically populate the syntax keywords lists
"
" Install it in ~/.vim/scripts
"
" Use the binding:
"  map <C-F4> :runtime scripts/racket-auto-syntax.vim
" in your .vimrv
"
"
" Issues:
" * Currently, I can't get this to run in an autocommand, so I have it bound
"   using control-F4
"
" * Anyone who can get this to work with scribble.vim, without vim SEGVing --
"   please contact me.
"
" Changes: 

mzscheme << EOF
(require (prefix-in vim- 'vimext) racket/function srfi/1 srfi/13)

(vim-command "syntax clear racketFunc racketSyntax scribbleMarkup")

(define highlighting-ns (make-base-namespace))
(parameterize ([current-namespace highlighting-ns])
 (for-each namespace-require
 '(racket/base racket racket/contract rackunit)))

; these are different since they need @'s on the front, as well as nextgroups
(define scribble-ns (make-base-namespace))
(parameterize ([current-namespace scribble-ns])
 (for-each namespace-require
 '(scribble/base scribble/manual scribble/racket scribble/scheme scribble/eval
   scribble/srcdoc scribble/extract scribble/bnf scribble/basic scribble/lp
   scribble/lp-include
   #;scribble/sigplan #;scribble/jfp #;scribble/lncs)))

(define (namespace-symbol->is-syntax? ns sym)
 (call/cc (λ (k) (namespace-variable-value sym #t (λ () (k #t)) ns) #f)))

  (define (namespace->partition-symbols ns)
   (partition (curry namespace-symbol->is-syntax? ns)
    (namespace-mapped-symbols ns)))

(define-values (hl-syns hl-funcs)
 (namespace->partition-symbols highlighting-ns))

(define-values (scribble-syns scribble-funcs)
 (let-values [((all-syns all-funcs)
               (namespace->partition-symbols scribble-ns))]
  (values
   (filter (negate (λ (x) (find-tail (curry eq? x) hl-syns))) all-syns)
   (filter (negate (λ (x) (find-tail (curry eq? x) hl-funcs))) all-funcs))))

  (define (join-syms prfx . syms)
   (string-join (map (curry format "~a~a" prfx) (apply append syms)) " "))

  (define (format-vim-command fmt . args) (vim-command (apply format fmt args)))

(unless (null? hl-funcs)
 (format-vim-command "syntax keyword racketFunc ~a" (join-syms "" hl-funcs)))
(unless (null? scribble-funcs)
 (format-vim-command "syntax keyword racketFunc ~a" (join-syms "" scribble-funcs)))
(unless (null? hl-syns)
 (format-vim-command "syntax keyword racketSyntax ~a" (join-syms "" hl-syns)))
(unless (null? scribble-syns)
 (format-vim-command "syntax keyword racketSyntax ~a" (join-syms "" scribble-syns)))

(unless (null? (append scribble-syns scribble-funcs))
 (format-vim-command "syntax keyword racketSyntax ~a nextgroup=atBraceRange,atBrackRange"
  (join-syms "@" scribble-syns scribble-funcs)))

EOF
