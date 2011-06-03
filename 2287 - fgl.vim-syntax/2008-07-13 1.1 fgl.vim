
syn cluster NofglFold contains=fglComment,fglString

"function
syn region FglFunctionFold
      \ start="^\s*function .*$"
      \ skip=/^\s*\(#.*\)*$/
      \ end="^\s*end function.*$"
      \ transparent fold
      \ keepend extend
      \ contains=ALLBUT, @NoFglFold 
"case  -----------------------------------------"
syn region FglCaseFold
      \ start="^\s*case[ .]*$"
      \ skip=/^\s*\(#.*\)*$/
      \ end="^\s*end case"
      \ transparent fold
      \ keepend extend 
      \ containedin=ALLBUT, @NoFglFold
"when|otherwise"
syn region fglWhenFold 
      \ start="^\s*\(when .*\|otherwise\)\s*$"
      \ skip=/^\s*\(#.*\)*$/
      \ end="^\s*\(when .*\|otherwise\|end case\)\s*$"ms=s-1,me=s-1
      \ transparent fold
      \ keepend 
      \ containedin=FglCaseFold 
"while --------------------------------------------"
syn region FglWhileFold
      \ start="^\s*while .*$"
      \ skip=/^\s*\(#.*\)*$/
      \ end="^\s*end while"
      \ transparent fold
      \ keepend extend 
      \ containedin=ALLBUT, @NoFglFold
"for"
syn region FglForFold
      \ start="^\s*for .*$"
      \ skip=/^\s*\(#.*\)*$/
      \ end="^\s*end for"
      \ transparent fold
      \ keepend extend 
      \ containedin=ALLBUT, @NoFglFold
"foreach"
syn region FglForeachFold
      \ start="^\s*foreach .*$"
      \ skip=/^\s*\(#.*\)*$/
      \ end="^\s*end foreach"
      \ transparent fold
      \ keepend extend 
      \ containedin=ALLBUT, @NoFglFold
"if container -------------------------------------"
syn region fglIfFoldContainer
      \ start="^\s*if .*$"
      \ skip=/^\s*\(#.*\)*$/
      \ end="^\s*end if"
      \ transparent fold
      \ keepend extend
      \ containedin=ALLBUT, @NoFglFold
"      \ contains=NONE
""if
"syn region fglIfFold
"      \ start="^\s*if .*$"
"      \ skip=/^\s*\(#.*\)*$/
"      \ end="^\s*else"ms=s-1,me=s-1
"      \ transparent fold
"      \ keepend 
"      \ contained containedin=fglIfFoldContainer
"      \ nextgroup=fglFoldElse
"      \ contains=TOP
""else"
"syn region fglFoldElse
"      \ start="^\s*else"
"      \ skip=/^\s*\(#.*\)*$/
"      \ end="^\s*end if"
"      \ transparent fold
"      \ keepend 
"      \ contained containedin=fglIfFoldContainer
"      \ contains=TOP

"input  ----------------------------------------------"
syn region fglInputFold
      \ start="^\s*input .*$"
      \ skip=/^\s*\(#.*\)*$/
      \ end="^\s*end input"
      \ transparent fold
      \ keepend extend 
      \ containedin=ALLBUT, @NoFglFold
"before|after|on"
syn region fglInputSubGrpFold 
      \ start="^\s*\(before\|after\|on\) .*$"
      \ skip=/^\s*\(#.*\)*$/
      \ end="^\s*\(before \|after \|on \).*\s*$"ms=s-1,me=s-1
      \ transparent fold
      \ keepend 
      \ containedin=FglInputFold 
      \ contains=TOP

"define  ---------------------------------------------"
syn region fglDefineFold 
      \ start="^\s*define.*$"
      \ end="^\s*$"
      \ transparent fold
      \ keepend 
"globals"
syn region FglGlobalsFold
      \ start="^\s*globals\s*$"
      \ skip=/^\s*\(#.*\)*$/
      \ end="^\s*end globals\s*$"
      \ transparent fold
      \ keepend 
      \ contains=fglDefineFold
      \ containedin=NONE

