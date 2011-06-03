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
:map <c-H> :let b:hicount=0:syn clear
:map <c-h> :let b:a=@a:let @a=b:hicount:normal ,**a:let @a=b:a:let b:hicount=((b:hicount+10-1)%10):echo "Next Level:" b:hicount
:vmap <c-h> y:let b:hicount = (b:hicount + 1) % 10:let b:a=@a:let @a=b:hicount:normal ,*a:let @a=b:a:echo "Next Level:" b:hicount
:map ,*0 :syn match M0 ".":syn clear M0:syn match M0 """:hi M0 guifg=red:echo "Next Level:" b:hicount+1
:map ,*1 :syn match M1 ".":syn clear M1:syn match M1 """:hi M1 guifg=green:echo "Next Level:" b:hicount+1
:map ,*2 :syn match M2 ".":syn clear M2:syn match M2 """:hi M2 guifg=blue:echo "Next Level:" b:hicount+1
:map ,*3 :syn match M3 ".":syn clear M3:syn match M3 """:hi M3 guifg=cyan:echo "Next Level:" b:hicount+1
:map ,*4 :syn match M4 ".":syn clear M4:syn match M4 """:hi M4 guifg=magenta:echo "Next Level:" b:hicount+1
:map ,*5 :syn match M5 ".":syn clear M5:syn match M5 """:hi M5 guifg=yellow:echo "Next Level:" b:hicount+1
:map ,*6 :syn match M6 ".":syn clear M6:syn match M6 """:hi M6 guifg=gray:echo "Next Level:" b:hicount+1
:map ,*7 :syn match M7 ".":syn clear M7:syn match M7 """:hi M7 guifg=orange:echo "Next Level:" b:hicount+1
:map ,*8 :syn match M8 ".":syn clear M8:syn match M8 """:hi M8 guifg=purple:echo "Next Level:" b:hicount+1
:map ,*9 :syn match M9 ".":syn clear M9:syn match M9 """:hi M9 guifg=violet:echo "Next Level:" b:hicount+1
:map ,**0 :syn match M0 ".":syn clear M0
:map ,**1 :syn match M1 ".":syn clear M1
:map ,**2 :syn match M2 ".":syn clear M2
:map ,**3 :syn match M3 ".":syn clear M3
:map ,**4 :syn match M4 ".":syn clear M4
:map ,**5 :syn match M5 ".":syn clear M5
:map ,**6 :syn match M6 ".":syn clear M6
:map ,**7 :syn match M7 ".":syn clear M7
:map ,**8 :syn match M8 ".":syn clear M8
:map ,**9 :syn match M9 ".":syn clear M9

