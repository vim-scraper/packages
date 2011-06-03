" Author: Kontra, Gergely <kgergely@turul.eet.bme.hu>
" Thanks:
" Jeanluc
"     Always trying to answer my questions, show me a working prototype
" Salman Halim
"     Requests support for more feature, and says me, that is not complete
"     waste of time, what I do... :)
"
" Purpose: Emulates a multifield dialog button in console
" You can circulate between the entries with Tab and Shift-Tab
"
" WARNING: Use it with vim6 (not Beta)
"   In some beta release, it crashes vim (was a bug in vim)
"
" Any suggestions, simplifications are granted
"
" History:
" 0.1  Initial release
" 0.2  Added the facility to default values, improved regexp (I hope...)
" 0.3 Cleaning up unwanted \( \) pairs, writing the default to the prompt
" 0.4  Added indicator, wheter a parameter is optional or not, know stronger
"      tying between Ask and Prompt, Prompt won't return, unless abbrev 
" 0.5b Correct expansion (please test it!)
" 0.51 Using the *curly-braces-names*
"
" Usage:
" Ask("format","p1","p2",...) asks for p1, p2 ... using Prompt, but after
"   that tries to 'fill in' the format string with the result.
"   \1 \2 ... is replaced with the first, second prompt's 'answer'
"   When you do not want to expand some text, if inside it is an undefined
"   value put it into a \( \) pair!
"   If you want a default value, use the "prompt=defaultvalue" format!
"   Any \1 \2... parameter, which is not enclosed in \( \), AND has no
"   default, must be entered by the user
"   Notice, that the abbreviation for 'for' is valid! (all parameter is
"   required, but you don't want to type any of them, while they have a
"   default value)
"
"   To enter \ type '\\' or "\\\\" (like in regexps)
"
" See the examples!
"
" Examples:
" A html image with optional alt tag
" Note: beware of using defaults too much! In this example, the default
" becomes very annoying after a while!!!
im <F2> <C-R>=Ask('<img src="\1"\( alt="\2"\)>',"source","alternative text=image")<CR>

" Why is this map works? It inserts text, which will force vim to normal mode
" for a moment...
" Tell me, if you know!
" A while statement
im w` <C-R>=Ask('while(\1) {'."\<lt>CR>}\<lt>Esc>O",'condition=1')<CR>
" Notice how (ugly) to map <CR>
" A for statement (all parts are optional, defaults are just for the sake of
" exaple, better to remove!
im f` <C-R>=Ask('for(\(\1\);\(\2\);\(\3\)) {'."\<lt>CR>}\<lt>Esc>O","Initial statement=i=0","condition=i<10","iteration=i++")<CR>
" Dummy example to show nested alternative parts
" If you can gimme a real life example, I will thanks for it...
im <F4> <C-R>=Ask('<dummyexample\( img src=\1\( alt=\2\)\)>','img','alt')<CR>
" Helper function
"
" Prompt("p1","p2","p3",...) do a prompt("p1"), prompt("p2") ... but not
"   clearly sequential! Use Tab to move to next item, Shift-Tab to switch
"   back, Enter to finish entering fields
"   The result is in the diaVal1, diaVal2,... global variables
fu! Prompt(num)
  let s:diaMore=1
  let i=0
  wh i<a:num
    if !exists('s:default'.i)
      let s:diaVal{i}=''
    el
      let s:diaVal{i}=s:default{i}
    en
    let i=i+1
  endw
  cno <TAB> <C-R>=DiaMore(1)<CR><CR>
  cno <S-TAB> <C-R>=DiaMore(-1)<CR><CR>
  cno <CR> <C-R>=DiaMore(0)<CR><CR>
  let which=0
  wh s:diaMore
    " first build a prompt
    let p=s:prompt{which}
    " char to indicating wheter parameter is optional or not
    let p=p.(exists('s:required'.which)?'!':'?')
    let s:diaAbort=1 " to handle escape correctly (tab, cr will clear it)
    let s:diaVal{which}=input(p." ",s:diaVal{which})
    if s:diaAbort==1|break|en

    let which=(which+s:diaMore+a:num)%a:num "to enable going back

    if s:diaMore | con | en " if user don't press <CR> -> next prompt
    let i=0 " Check if we are ready
    while i<a:num
      let val=s:diaVal{i}
      if val=='' && exists('s:required'.i)
	let which=i | let s:diaMore=1 | break
      en
      let i=i+1
    endwhile
  endw
  cu <TAB>
  cu <S-TAB>
  cu <CR>
  retu
endf

fu! DiaMore(m)
  let s:diaMore=a:m
  let s:diaAbort=0
  retu ''
endf

" let s:c='\(\([^\\]\|\\.\)\{-}\)' " a char is not a backslash or a backslash (captured)
let s:eb='\(\%(\\\\\)*\)' " even backslash (captured)
let s:valRef='\\\d\+'

" And the function you want

fu! Ask(formatString,...)
  let f=a:formatString " shorthand 
  let i=0 " cleanup (previous run)
  wh i<a:0
    unlet! s:required{i} |unlet! s:default{i}
    let i=i+1
  endw

  " Let's see what values are required
  let tmp=substitute(f,s:eb.'\\('.'.\{-}'.s:eb.'\\)','\1','g')
  while tmp=~s:valRef
    let i=strpart(matchstr(tmp,s:valRef),1)-1 " I hope it works :-)
    let tmp=substitute(tmp,s:valRef,'','')    " Anybody with a one-line solution?
    let s:required{i}=1
  endwhile

  " Let's see the default values now!
  let i=1
  wh i<=a:0
    " Additionally, set prompts
    let s:prompt{i-1}=a:{i}
    " And the default values itself...
    let pos=stridx(a:{i},"=")
    if pos!=-1
      let s:default{i-1}=strpart(a:{i},pos+1)
      let s:prompt{i-1}=strpart(a:{i},0,pos)
    en
    let i=i+1
  endw

  call Prompt(a:0)

  if s:diaAbort|retu ''|en " if user press Escape -> do nothing

  return DiaSub(f)
endfu

" DiaSub(f)
" It performs substituting. \number is substituted by s:diaVal{number-1}
" It handles \( \) blocks. (If undef inside, will return '')
" \( \) pairs can now be nested!!!
fu! DiaSub(f)
  let f=a:f
"   echo 'Entering DiaSub param: '.f."\n"
  let res=''
  while strlen(f)
    let minPos=match(f,s:eb.'\\[(0-9]') " Search for \1 and \( like patterns
    let atom=matchstr(f,s:eb.'\\[(0-9]')
    let what=strpart(atom,strlen(atom)-1)
    if minPos==-1|return res.f|endif " If no match -> return
    let res=res.strpart(f,0,minPos) " copy all before atom to res
    let f=strpart(f,minPos)         " ...ehm, move it, but in 2 steps
"     echo 'Match= '.what.' Res|other= '.res.'|'.f."\n"
    if what!='(' " It must be a \number match
      let which=strpart(matchstr(f,'^'.s:valRef),1)-1 " ^ to be sure...
      let thisVal=s:diaVal{which}
"       echo "Substituting \\".(which+1).' with "'.thisVal.'"'
      if thisVal==''|retu ''|en " if undef -> return empty string
      let res=res.thisVal " append var's actual value to res
      let f=substitute(f,'^'.s:valRef,'','') " remove the 'reference' from string
    else
      " Let's find the pair of \(
      let start=2
      let sum=1
      while sum>0
	" echo 'Sum is: '.sum
	let m   =matchend(f,s:eb.'\\[()]',start)
	if m==-1|echoerr 'Parse error: too few \)'|retu ''|en
	" echo 'Start string:'.mstr.' pos:'.start."\n"
	let sum=sum+((strpart(f,m-1,1)=='(')?1:-1)
	let start=m
      endw
      " Ok, matching \)'s pos is in m
      let param=strpart(f,2,m-4) " the \( \) pair eats up 2-2 char
      let part=DiaSub(param)
"       echo "DiaSub ended with result: ".part
      let res=res.part
      let f=strpart(f,m)
    endif
  endw
  retu res
endf
