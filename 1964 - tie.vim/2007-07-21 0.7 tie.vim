" Vim syntax file
" Language:	Xtensa TIE
" Version:      0.7	
" Maintainer:	Saravanan T S <saravanan.ts@gmail.com>
" Last Update:  Mon Jul 22 06:30:32 JST 2007

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
   syntax clear
elseif exists("b:current_syntax")
   finish
endif

" TIE is a case sensitive language
syntax case match

" Set the local value of the 'iskeyword' option
if version >= 600
   setlocal iskeyword=@,48-57,_,192-255
else
   set iskeyword=@,48-57,_,192-255
endif

" A bunch of useful TIE keywords
syn keyword tieType      state regfile immediate_range
syn keyword tieType      table 

" tieStatement is same as tie descriptions
syn keyword tieStatement interface
syn keyword tieStatement operation 
syn keyword tieStatement schedule semantic length
syn keyword tieStatement format slot slot_opcodes import_wire
syn keyword tieStatement queue ctype proto user_register coprocessor
syn keyword tieStatement function field opcode operand iclass reference
syn keyword tieStatement add_read_write assign core immediate
syn keyword tieStatement in inout Inst InstBuf MEM out wire
syn keyword tieStatement export 

" These are predefined Interface signals in tie. These also are
" considered as keywords
syn keyword tieInterface VAddr
syn keyword tieInterface MemDataIn128 MemDataIn64 MemDataIn32
syn keyword tieInterface MemDataIn16 MemDataIn8
syn keyword tieInterface MemDataOut128 MemDataOut64 MemDataOut32
syn keyword tieInterface MemDataOut16 MemDataOut8
syn keyword tieInterface LoadByteDisable StoreByteDisable

syn keyword tieStatement shared slot_shared 

" Some from LX2
syn keyword tieStatement lookup imap

syn keyword tieFunction TIEadd TIEaddn TIEcmp TIEcsa TIEmac 
syn keyword tieFunction TIEmul TIEmulpp TIEmux TIEpsel TIEsel
syn keyword tieFunction TIEprint

syn keyword tiePreprocessor PIFReadDataBits PIFWriteDataBits
syn keyword tiePreprocessor IsaMemoryOrder IsaUseBooleans IsaCoprocessorCount

syn keyword tieTodo contained TODO

" Unary operators
syn match   tieOperator "[&|~><!)(*%@+/=?:}{,.\^\-\[\]]"

" note: ";" is not considered as operator here, to simplify highlighting
" TODO: Perl embedding has to be tested more
syn match   tieEmbeddedPerl /^\s*;.*/
syn region  tieEmbeddedPerl start="^\s*`" end="`"

" arithmetic
" syn match   tieOperator "+\{1}.*"hs=s,he=s+1
" syn match   tieOperator "-\{1}.*"hs=s,he=s+1
" syn match   tieOperator "*\{1}.*"hs=s,he=s+1

" logical
" syn match   tieOperator "!.*"hs=s,he=s+1

" TODO: There is more work on all the below logical operator validation...
" syntax match  tieOperatorLogical "!.*" contains=tieLogicalNot
" syntax match  tieLogicalNot      "!"   contained nextgroup=tieFiller
" syntax region tieFiller start="."  matchgroup=ccBar  end="Bar"  contained
" syn match   tieOperator "&\{2}"hs=s,he=s+2
" syn match   tieOperator "|\{2}.*"hs=s,he=s+2
" syn match   tieOperator "|\{2}"

" Relational
syn match tieOperator ">=\|<=\|=="

syn region  tieComment start="/\*" end="\*/" contains=tieTodo,@Spell
syn match   tieComment "//.*" contains=tieTodo,@Spell

" syn match   tieGlobal "$[a-zA-Z0-9_]\+\>"

syn match   tieConstant "\<[A-Z][A-Z0-9_]\+\>"

syn match   tieNumber "\(\<\d\+\|\)'[sS]\?[bB]\s*[0-1_xXzZ?]\+\>"
syn match   tieNumber "\(\<\d\+\|\)'[sS]\?[oO]\s*[0-7_xXzZ?]\+\>"
syn match   tieNumber "\(\<\d\+\|\)'[sS]\?[dD]\s*[0-9_xXzZ?]\+\>"
syn match   tieNumber "\(\<\d\+\|\)'[sS]\?[hH]\s*[0-9a-fA-F_xXzZ?]\+\>"
syn match   tieNumber "\<[+-]\=[0-9_]\+\(\.[0-9_]*\|\)\(e[0-9_]*\|\)\>"

syn region  tieString start=+"+ skip=+\\"+ end=+"+ contains=tieEscape,@Spell
syn match   tieEscape +\\[nt"\\]+ contained
syn match   tieEscape "\\\o\o\=\o\=" contained

" No name in the tie code should begin with TIE_ or tie_ as
" per guidelines 
syn region tieError start="\<TIE_" end="\>"
syn region tieError start="\<tie_" end="\>"
syn region tieError start="\<_" end="\>"

"Modify the following as needed.  The trade-off is performance versus
"functionality.
syn sync minlines=50

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_tie_syn_inits")
   if version < 508
      let did_tie_syn_inits = 1
      command -nargs=+ HiLink hi link <args>
   else
      command -nargs=+ HiLink hi def link <args>
   endif

   " The default highlighting.
   HiLink tieCharacter       Character
   HiLink tieConditional     Conditional
   HiLink tieRepeat	     Repeat
   HiLink tieString	     String
   HiLink tieTodo	     Todo
   HiLink tieComment	     Comment
   HiLink tieConstant	     Constant
   HiLink tieLabel	     Label
   HiLink tieNumber	     Number
   HiLink tieOperator	     Special
   HiLink tieStatement	     Statement
   HiLink tieGlobal	     Define
   HiLink tieEscape	     Special

   " TSS
   HiLink tieType         Type
   HiLink tieInterface    Statement 
   HiLink tieFunction     Macro 
   HiLink tiePreprocessor PreProc
   HiLink tieError        Error
   HiLink tieEmbeddedPerl PreProc
   delcommand HiLink
endif

let b:current_syntax = "tie"

" vim: ts=8

