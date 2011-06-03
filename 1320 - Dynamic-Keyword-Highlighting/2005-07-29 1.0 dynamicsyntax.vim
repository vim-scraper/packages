"Dynamic Syntax Highlighting          vim:tw=0:sts=8:ts=8:noexpandtab:
"eddie khoo ee lek             <> last updated :  29 Jul 2005 02:14:16
"--------------------------------------------------------------------*
"
"       When inspecting text, often highlighting the current search
"       keyword isn't enough. This lets you highlight up to 10
"       different items at the same time.
"
"       To highlight any word, use visual mode to highlight word and
"       do a <ctrl-h>. 
"
"       The next <ctrl-h> will be in a different colour.
"
"       To undo the previous highlight, press <ctrl-h> without
"       highlighting anything in visual mode.
"
"       To clear all <ctrl-H>
"
"       Enjoy!
"
"       zlel @ eddie


:let b:hicount=0
:map <c-H> :let b:hicount=0
:map <c-h> :let b:a=@a
:vmap <c-h> y:let b:hicount = (b:hicount + 1) % 10
:map ,*0 :syn match M0 "."
:map ,*1 :syn match M1 "."
:map ,*2 :syn match M2 "."
:map ,*3 :syn match M3 "."
:map ,*4 :syn match M4 "."
:map ,*5 :syn match M5 "."
:map ,*6 :syn match M6 "."
:map ,*7 :syn match M7 "."
:map ,*8 :syn match M8 "."
:map ,*9 :syn match M9 "."
:map ,**0 :syn match M0 "."
:map ,**1 :syn match M1 "."
:map ,**2 :syn match M2 "."
:map ,**3 :syn match M3 "."
:map ,**4 :syn match M4 "."
:map ,**5 :syn match M5 "."
:map ,**6 :syn match M6 "."
:map ,**7 :syn match M7 "."
:map ,**8 :syn match M8 "."
:map ,**9 :syn match M9 "."
