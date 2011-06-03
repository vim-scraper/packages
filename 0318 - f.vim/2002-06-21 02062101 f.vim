" :call F()
" This function lets you see all the lines in a file
" that contain a given regular-expression.
"
" The remaining lines are folded with the manual method,
" and can be opened to see some 'nearby' line.
"
" The idea comes from the mainframe ISPF editor.
"
" zo   to open a fold a bit
" zozj to open a fold a bit and leave the cursor on the fold line
" zO   to open a fold completely 
" zE   to open all folds
"
" zf{motion} to close folders 'a bit more' (or to create folders)
" If you have 2 adjacent folders, zf<ENTER> will 'merge' them.
" In the same way a visible line can be 'added' to a nearby folder.
"
" Suggested mappings:
" :map <F5> :call F()<CR>
" :map <F6> :normal zozj<CR>
"
" :<Up> to recall the macro after having executed it once
"
" try giving as an expression  nfold
" try giving as an expression  as
" try giving as an expression   as     [that is: ' as' ]
" try giving as an expression  let
" try giving as an expression  ^":
" try giving as an expression  :fun\|:endf
" try giving as an expression  :endf
" try giving as an expression  ^:endf
"
" for remarks, complaints, etc. write to: antonio.colombo@jrc.it
" version 02062101

:fun! Foldft(from,to) range
"makes folds from "a:from" to "a:to"
":echo a:from a:to "fold"
:let range=a:to-a:from
:if ( range<0 )
:retu
:en
:if ( range<3 )
:exe a:from ","  a:to "fold"
:retu
:en
:let center=range/2
:wh ( center>=0 )
:let f=a:from+center
:let t=a:to-center
":echo range center f t
:exe f "," t "fold"
:if ( center>999 )
: let center=center-500
:else
: if ( center>=99 )
:  let center=center-50
: else
:  if ( center>=9 )
:   let center=center-5
:  else
:   let center=center-1
:  en
: en
:en
:endw
:endf

:fun! F() range
:let arg=input("Which expression? ")
:set foldmethod=manual
:set foldminlines=0
:set foldtext=
:let idx=0
:let lastline= line("$")
:let nfold=0
:norm G$
:norm zE
:let fromline=1
:wh idx<lastline
:if ( idx==0 ) 
:let num=search(arg)
:else
:let num=search(arg,"W")
:en
" search gives back the column number...
:if ( num==0 )
:break
:en
:if ( num!=0 ) 
"" to avoid wrapping around
":if idx>line(".") 
":break
":en
:let toline=num-1
:if toline>=fromline
:call Foldft(fromline,toline)
:let nfold=nfold+1
:en
:let fromline=num+1
":echo "line" num "found" arg "at col" col(".")
":echo "----+----1----+----2----+----3----+----4----+----5----+----6----+-----7"
":p
:norm $
:let idx=line(".")
:en
:endw
:if fromline<=lastline && fromline!=1
:let toline=lastline
:call Foldft(fromline,toline)
:let nfold=nfold+1
:en
:norm 1G
:call histadd(":","call F()")
:if nfold>0
:call histadd("/",arg)
:else
:echo "Expression not found:" arg
:en
:endf
