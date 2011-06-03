" RemoveDups.vim
"   Author:		Ing. Michael Fitz
"   Date:		2008-05-19_16:58:30
"   Version:	1
"   Purpose:
"   Removing lines with duplicate content immediately following each other
"   (so it could be useful to sort before removing dups)
"   Usage:
"   Source this file (or put it in Plugins)
"   Mark the area (Blockmode or linemode) and do
"   :'<,'>call RemoveDups()

" ---------------------------------------------------------------------
fun! RemoveDups() range
"echo 'Mode='.visualmode()
if visualmode() == "\<c-v>"
  "   Block-Mode    
  let Firstline= line("'<")
  let Lastline = line("'>")
  let ColA=col("'<")
  let ColE=col("'>")
elseif visualmode() == "V"
  "   Line mode
  let Firstline= a:firstline
  let Lastline= a:lastline
  let ColA=1
  let ColE=-1
else
  "   not visual at all    
  let Firstline=1 
  normal G
  let Lastline = line(".")
  let ColA=1
  let ColE=-1
endif

if(Firstline>Lastline)
  let z1=Firstline
  let Firstline=Lastline
  let Lastline=z1
endif

if(ColE>=0)
  if(ColA>ColE)
    let z1=ColA
    let ColA=ColE
    let ColE=z1
  endif
endif

"echo 'FirsLine='.Firstline
"echo 'FirstCol='.ColA
"echo 'LastLine='.Lastline
"echo 'LasttCol='.ColE
" Now do the removing
if(ColE>0)
  let ColE=ColE-ColA
  if(ColE==0)
    let ColE=1
  endif
endif
let ColA=ColA-1
let ZeileNr=Firstline+1
let Zeile=getline(Firstline)
if(ColE>0)
  let Altwert=strpart(Zeile,ColA,ColE)
else
  let Altwert=Zeile
endif
while(ZeileNr<=Lastline)
  let Zeile=getline(ZeileNr)
  if(ColE>0)
    let Neuwert=strpart(Zeile,ColA,ColE)
  else
    let Neuwert=Zeile
  endif
  if(Altwert==Neuwert)
    exec 'normal' ZeileNr.'G'
    normal dd
    let Lastline=Lastline-1
  else
    let ZeileNr=ZeileNr+1
  endif
  let Altwert=Neuwert
endwhile
endfun
