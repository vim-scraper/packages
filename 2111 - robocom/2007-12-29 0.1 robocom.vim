" Vim syntax file
" Language:     Robocom
" Maintainer:   Roman Cheplyaka <roma@ro-che.info>
" Last Change:  2007 Jul 30
" NOTE:
" Currently I'm barely interested in Robocom game. But if you have some
" patches or suggestions feel free to send them.
syntax clear
syntax case ignore

" Keywords
syntax keyword robBank Bank contained
syntax keyword robHeaderType Name Password Country Author EMail OpenSource Language OptionSet contained
" More headers here!

" Comments
syntax match robComment /;.*/

" Instructions
syntax keyword robInstr ADD BJUMP COMP CREATE DIE JUMP MOVE SCAN SET SUB TRANS TURN AJUMP DIV FARSCAN LCOMP GCOMP MAX MIN MOD MUL NCOMP RANDOM RTRANS SLEEP BREAK INIT RESUME SEIZE SLEEP QUIT

" Variables
" I don't see much sense in "syntax-check" hilighting. Variables are just
" identifiers starting with #, $ or %.
syntax match robLocalVar /#\i\+/
syntax match robLocalConst /\$\i\+/
syntax match robRefVar /%\i\+/

" Strings
syntax match robBankName /Bank\s\+\i\+/ contains=robBank
syntax match robPubHeader /Published .*/ contains=robHeaderType,robComment
syntax match robSecHeader /Secret .*/ contains=robHeaderType,robComment

" Labels
syntax match robLabel /@\i\+/

" Numbers
syntax match robNumber /\d\+/

" Links
highlight link robDesc PreProc
highlight link robBank Statement
highlight link robInstr Function
highlight link robLocalVar Identifier
highlight link robRefVar Identifier
highlight link robLocalConst Constant
highlight link robComment Comment
highlight link robBankName String
highlight link robPubHeader String
highlight link robSecHeader String
highlight link robHeaderType keyword
highlight link robNumber Number
highlight link robLabel Label
