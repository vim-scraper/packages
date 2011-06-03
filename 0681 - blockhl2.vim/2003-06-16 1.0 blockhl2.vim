" blockhl.vim: highlights different levels of {} with different shades of
"              grey.  Only good for gvim and C/C++.
"  Author: Charles E. Campbell, Jr.
"  Date:   November 30, 2000
"  
"  Modified By: Lord Bart
"  Modifation Date: June 16, 2003
"  Modifation Version: 1.0
"  
"  Modifications:
"  Took script blockhl.vim and added to it hilighing leading spaces or color 
"  level of brackets to primary colors.
"  Does more checking for correct file type of c or cpp
"  has two main functions:
"  HilightBrackets() truns on hilighting and hilight.
"  NoHilightBracktes() turns off hilighting clears syntax
"  has two toggle function with cab to run
"
"  Useage:
"  source after c.vim or cpp.vim syntax have been sourced
"  then use toghib to toggle on and off bloack higjlighting
"  use toghibs to toggle leading space hilighing
"
"  Bugs:
"  notice sometimes leading space hilighting does not hilite a line.
"  I am sure there are others

if has("gui_running")
  " if loaded once don't load again
  if exists("g:loaded_hilight_brackets")
    finish
  endif
  " if not c or cpp file exit
  if ! &filetype =~ 'c\%[pp]'
    finish
  endif
  let g:loaded_hilight_brackets = 1
  if !exists("g:HilightBracketsSpaces")
    let g:HilightBracketsSpaces = 0
  endif
  if !exists("g:HilightBracketsOn")
    let g:HilightBracketsOn = 0
  endif

  if version < 508
    command! -nargs=+ HiLink hi <args>
  else
    command! -nargs=+ HiLink hi def <args>
  endif

  " for matchgroup 
  syn match bracket1 /{\|}/
  syn match bracket2 /{\|}/
  syn match bracket3 /{\|}/
  syn match bracket4 /{\|}/
  syn match bracket5 /{\|}/
  syn match bracket6 /{\|}/
  syn match bracket7 /{\|}/
  hi bracket1 guifg=#00ff00
  hi bracket2 guifg=#ff0000
  hi bracket3 guifg=#0000ff
  hi bracket4 guifg=#00ffff
  hi bracket5 guifg=#ffff00
  hi bracket6 guifg=#ff00ff
  hi bracket7 guifg=NONE
 
  " for leading space
  syn match lsp1 /^\s\+/
  syn match lsp2 /^\s\+/
  syn match lsp3 /^\s\+/
  syn match lsp4 /^\s\+/
  syn match lsp5 /^\s\+/
  syn match lsp6 /^\s\+/
  syn match lsp7 /^\s\+/
  hi lsp1 guibg=#00ff00
  hi lsp2 guibg=#ff0000
  hi lsp3 guibg=#0000ff
  hi lsp4 guibg=#00ffff
  hi lsp5 guibg=#ffff00
  hi lsp6 guibg=#ff00ff
  hi lsp7 guibg=NONE
  syn cluster cCurlyGroup contains=cConditional,cConstant,cLabel,cOperator,cRepeat,cStatement,cStorageClass,cStructure,cType,cBitField,cCharacter,cCommentError,cInclude,cNumbers,cParenError,cPreCondit,cSpaceError,cSpecialCharacter,cSpecialError,cUserCont,cBracket,cComment,cCommentL,cCppOut,cCppString,cDefine,cMulti,cParen,cPreCondit,cPreProc,cString
  if &filetype == "c"
    syn cluster bracketCurlyGroup contains=@cCurlyGroup
  else
    syn cluster cppCurlyGroup contains=cppStatement,cppAccess,cppType,cppExceptions,cppOperator,cppCast,cppStorageClass,cppNumber,cppBoolean
    syn cluster bracketCurlyGroup contains=@cCurlyGroup,@cppCurlyGroup
  endif

function s:HilightBrackets()
  let g:HilightBracketsOn=1
  if g:HilightBracketsSpaces
    syn region cCurly1           matchgroup=bracket1 start="{" matchgroup=bracket1 end="}" transparent contains=@cCurlyGroup,cCurly2,lsp1
    syn region cCurly2 contained matchgroup=bracket2 start="{" matchgroup=bracket2 end="}" transparent contains=@cCurlyGroup,cCurly3,lsp2
    syn region cCurly3 contained matchgroup=bracket3 start="{" matchgroup=bracket3 end="}" transparent contains=@cCurlyGroup,cCurly4,lsp3
    syn region cCurly4 contained matchgroup=bracket4 start="{" matchgroup=bracket4 end="}" transparent contains=@cCurlyGroup,cCurly5,lsp4
    syn region cCurly5 contained matchgroup=bracket5 start="{" matchgroup=bracket5 end="}" transparent contains=@cCurlyGroup,cCurly6,lsp5
    syn region cCurly6 contained matchgroup=bracket6 start="{" matchgroup=bracket6 end="}" transparent contains=@cCurlyGroup,cCurly1,lsp6
  else
    syn region cCurly1           matchgroup=bracket1 start="{" matchgroup=bracket1 end="}" transparent contains=@cCurlyGroup,cCurly2
    syn region cCurly2 contained matchgroup=bracket2 start="{" matchgroup=bracket2 end="}" transparent contains=@cCurlyGroup,cCurly3
    syn region cCurly3 contained matchgroup=bracket3 start="{" matchgroup=bracket3 end="}" transparent contains=@cCurlyGroup,cCurly4
    syn region cCurly4 contained matchgroup=bracket4 start="{" matchgroup=bracket4 end="}" transparent contains=@cCurlyGroup,cCurly5
    syn region cCurly5 contained matchgroup=bracket5 start="{" matchgroup=bracket5 end="}" transparent contains=@cCurlyGroup,cCurly6
    syn region cCurly6 contained matchgroup=bracket6 start="{" matchgroup=bracket6 end="}" transparent contains=@cCurlyGroup,cCurly1
    endif
endfunction

function s:NoHilightBrackets()
  let g:HilightBracketsOn=0
  syntax clear cCurly1
  syntax clear cCurly2
  syntax clear cCurly3
  syntax clear cCurly4
  syntax clear cCurly5
  syntax clear cCurly6
endfunction

function s:ToggleHilightBrackets()
  if g:HilightBracketsOn==0
    let g:HilightBracketsOn=1
    call <SID>HilightBrackets()
  else
    let g:HilightBracketsOn=0
    call <SID>NoHilightBrackets()
  endif
endfunction
cab toghib call <SID>ToggleHilightBrackets()

function s:ToggleHilightBracketsSpaces()
  if g:HilightBracketsSpaces==0
    let g:HilightBracketsSpaces=1
  else
    let g:HilightBracketsSpaces=0
  endif
  if g:HilightBracketsOn==1
    call <SID>HilightBrackets()
  endif
endfunction
cab toghibs call <SID>ToggleHilightBracketsSpaces()

endif
