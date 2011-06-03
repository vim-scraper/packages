" Vim syntax file
" Language:	PowerQuest PartitionMagic Pro Scripting
" Maintainer:	Günther Pfannhauser <guenther@ongel.at>
" Last Change:	2004 March 23
" Version:      1.0
" Filenames:    Linux: */pqmagic.*


" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" not case sensitive
syn case ignore

" Variable definition
syn keyword pqmagicType 	dim

" Conditions and Loops are very simply at now
" Conditions
syn keyword pqmagicCondition    if then else
syn match   pqmagicCondition    /\<end\ if\>/

" Loops
syn keyword pqmagicLoop         do loop
syn match   pqmagicLoop         /\<loop while\>/
syn match   pqmagicLoop         /\<do while\>/

" Operators
syn match   pqmagicOperator     "[-+*/]"
syn match   pqmagicOperator	"\(<\|>\)\=="
syn match   pqmagicOperator	"=\=>"
syn match   pqmagicOperator     "[^=]<\(>\|=\)\="

" comments
syn keyword pqmagicTodo         contained TODO FIXME
syn region  pqmagicComment	start="//" end="$" contains=pqmagicTodo

" Disk Label in Quotes
syn match   pqmagicLabel        contained /\ \+"[A-Z]*"/

" Max Min
syn keyword pqmagicMaxMin       contained max min
syn keyword pqmagicMax          contained max

" ON OFF
syn keyword pqmagicOnOff        contained on off

" Select and Query Options
syn keyword pqmagicSelect       contained first last next previous extended largest
syn match   pqmagicSelect       contained / \+[0-9]\+/
syn case match 
syn match   pqmagicSelect       contained / \+[A-Z]\( \|$\)/
syn case ignore
syn match   pqmagicSelect       contained /\"[A-Z]*\"/
syn match   pqmagicSelect       contained /\<\(after\|before\) selected partition\>/
syn match   pqmagicSelect       contained /\<\(after\|before\) partition [0-9]\+\>/

" Values
syn keyword pqmagicValues       contained Recommended 512 1 2 4 8 16 32 64
syn keyword pqmagicValues       contained Beginning End First Second
syn keyword pqmagicValues       contained FAT FAT32 HPFS LINUXEXT2 LINUXSWAP NTFS EXTENDED UNFORMATTED
syn match   pqmagicValues       contained /\"[A-Z]*\"/

" Parameters
syn match   pqmagicParameter    "/[A-Z]*[=\ ]\+" nextgroup=pqmagicValues

" Commands
syn keyword pqmagicCommands     check copy create info label merge
syn keyword pqmagicCommands     format nextgroup=pqmagicLabel
syn match   pqmagicCommands     /\<\(un\)\=hide\>/
syn match   pqmagicCommands     /\<resize\( \+larger\| \+smaller\| \+root\)\=\>/ nextgroup=pqmagicMaxMin skipwhite
syn match   pqmagicCommands     /\<resize \+left \+boundary\( \+larger\| \+smaller\)\=\>/ nextgroup=pqmagicMaxMin skipwhite
syn match   pqmagicCommands     /\<resize \+space \+\(after\|before\)\>/ nextgroup=pqmagicMaxMin skipwhite
syn match   pqmagicCommands     /\<delete\( \+all\)\=\>/ nextgroup=pqmagicLabel
syn match   pqmagicCommands     /\<Convert \+To \+\(fat\|fat32\|hpfs\|ntfs\|primary\|logical\)\>/
syn match   pqmagicCommands     /\<Move \+\(Left\|Right\)\>/ nextgroup=pqmagicMaxMin skipwhite
syn match   pqmagicCommands     /\<Move \+Space \+\(After\|Before\)\>/ nextgroup=pqmagicMax skipwhite
syn match   pqmagicCommands     /\<select \+\(disk\|\(merge \+\)\=partition\|unallocated\)\>/ nextgroup=pqmagicSelect skipwhite
syn match   pqmagicCommands     /\<select \+destination \+\(disk\|unallocated\)\>/ nextgroup=pqmagicSelect skipwhite
syn match   pqmagicCommands     /\<allow \+manual \+reboot\>/
syn match   pqmagicCommands     /\<bad \+sector \+test\>/
syn match   pqmagicCommands     /\<cluster \+analyzer\>/
syn match   pqmagicCommands     /\<set \+active\>/
syn match   pqmagicCommands     /\<set \+allow \+user \+cancel\>/ nextgroup=pqmagicOnOff skipwhite
syn match   pqmagicCommands     /\<set \+default \+bad \+sector \+test \+state\>/ nextgroup=pqmagicOnOff skipwhite
syn match   pqmagicCommands     /\<set \+drive \+read \+only \+mode\>/ nextgroup=pqmagicOnOff skipwhite
syn match   pqmagicCommands     /\<set \+ignore \+os\/2 \+ea \+errors\>/ nextgroup=pqmagicOnOff skipwhite
syn match   pqmagicCommands     /\<set \+force \+user \+logoff\>/ nextgroup=pqmagicOnOff skipwhite
syn match   pqmagicCommands     /\<show \+\(partitions\|preference\|destination\)\>/

" Queries
syn keyword pqmagicQueries      GetTotalDisks GetTotalPartitions GetTotalUnallocatedSpaces GetDiskSize GetAllocatedSize
syn keyword pqmagicQueries      GetAllocatedPercent GetUnallocatedSize GetUnallocatedPercent GetSelectedPartitionSize
syn keyword pqmagicQueries      GetPartitionNumber nextgroup=pqmagicSelect skipwhite
syn keyword pqmagicQueries      GetUsedAmount GetUsedPercent GetUnusedAmount GetUnusedPercent GetSelectedUnallocatedSize
syn keyword pqmagicQueries      IsFAT IsFAT32 IsNTFS IsHPFS IsLinuxExt2 IsLinuxSwap IsExtended IsUnallocated IsUnformatted
syn keyword pqmagicQueries      IsActive IsHidden IsPrimary IsLogical
syn keyword pqmagicQueries      GetUnallocatedNumber nextgroup=pqmagicSelect skipwhite




" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_pqmagic_syntax_inits")
  if version < 508
    let did_pqmagic_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif


   HiLink pqmagicComment	Comment
   HiLink pqmagicTodo           Todo

   HiLink pqmagicOperator	Operator
   HiLink pqmagicType           Type
   
   HiLink pqmagicCommands       Identifier
   HiLink pqmagicVariable       Identifier   
   HiLink pqmagicQueries        PreProc
   HiLink pqmagicCondition      Conditional
   HiLink pqmagicLoop           Conditional
   HiLink pqmagicParameter      Special
   HiLink pqmagicValues         Constant
   HiLink pqmagicSelect         Constant   
   HiLink pqmagicLabel          Constant
   HiLink pqmagicMaxMin         Constant
   HiLink pqmagicMax            Constant   
   HiLink pqmagicOnOff          Constant
   
  delcommand HiLink
endif

let b:current_syntax = "pqmagic"

" vim: ts=8
