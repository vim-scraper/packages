" CVSmenu.vim : Vim menu for using CVS
" Author : Thorsten Maerz <info@netztorte.de>
" Last Update : 2001
"
" Needs Vim >= 6.0, WINDOWS(-->$temp)
"
" ToDo : if has("unix") then temp='/tmp' else temp=$temp
" ToDo : set CVS options (compression, cvsroot)
" ToDo : CVS commit / add / remove / checkout / login / logout

"unmenu &CVS

if ($temp == "")
  let $temp="/tmp"
endif

nmenu &CVS.edit	        	:sp $VIM/cvsmenu.vim<cr>
nmenu &CVS.show&version		:!cvs -v<cr>
nmenu &CVS.CVS&diff		:call CVSdiff()<cr>
nmenu &CVS.CVS&annotate		:call CVSannotate()<cr>
nmenu &CVS.CVS&log		:call CVSlog()<cr>
nmenu &CVS.CVS&status		:call CVSstatus()<cr>
nmenu &CVS.&queryupdate		:call CVSqueryupdate()<cr>

"-----------------------------------------------------------------------------
" CVS update / query update : tools
"-----------------------------------------------------------------------------

function! OpenRO()
" Used for CVSqueryupdate : Open output in new window, open file under cursor
" by <doubleclick> or <shift-cr>
  set nomod
  nmap <buffer> <2-LeftMouse> 0<2-Right>
  nmap <buffer> <S-CR> 0<2-Right>
endfunction

function! UpdateSyntax()
" Used for CVSqueryupdate : Set syntax hilighting
  syn match cvsupdateMerge 	"^M .*$"
  syn match cvsupdatePatch	"^P .*$"
  syn match cvsupdateConflict	"^C .*$"
  syn match cvsupdateDelete	"^D .*$"
  syn match cvsupdateUnknown	"^? .*$"
  hi link cvsupdateMerge 	Special
  hi link cvsupdatePatch	Constant
  hi link cvsupdateConflict	WarningMsg
  hi link cvsupdateDelete	Statement
  hi link cvsupdateUnknown	Comment
endfunction

"-----------------------------------------------------------------------------
" CVS update / query update
"-----------------------------------------------------------------------------

function! CVSqueryupdate()
  exec "cd ".expand("%:p:h")
  let tmpnam=$temp."/".expand("%")
  let rev=""	
"input("revision:","")
  if rev!=""
    let tmpnam=tmpnam."\.".rev
    let rev="-r ".rev." "
  else
    let tmpnam=tmpnam.".current"
  endif
"  wincmd _
  new
  exec "%!cvs -z9 -n update -P ".rev.expand("%:p:t")." > ".tmpnam
  call OpenRO()
  call UpdateSyntax()
endfunction

"-----------------------------------------------------------------------------
" CVS diff
"-----------------------------------------------------------------------------

function! CVSdiff()
  exec "cd ".expand("%:p:h")
  let tmpnam=$temp."/".expand("%")
  let rev=input("revision:","")
  if rev!=""
    let tmpnam=tmpnam."\.".rev
    let rev="-r ".rev." "
  else
    let tmpnam=tmpnam.".current"
  endif
"  call DoCVScommand("update -p ".rev.expand("%:p:t"),tmpnam)
  wincmd _
  exec "!cvs -z9 update -p ".rev.expand("%:p:t")." > ".tmpnam
"  exec "!cvs -z9 update -p ".rev.a:fn." > ".tmpnam
  exec "vertical diffsplit ".tmpnam
endfunction

"-----------------------------------------------------------------------------
" CVS annotate / log /status : tools
"-----------------------------------------------------------------------------

function! DoCVScommand(cmd,tmpnam)
  exec "!cvs -z9 ".a:cmd." > ".a:tmpnam
  new
  exec "find ".a:tmpnam
  set nomod
endfunction

"-----------------------------------------------------------------------------
" CVS annotate / log / status
"-----------------------------------------------------------------------------

function! CVSannotate()
  exec "cd ".expand("%:p:h")
  let tmpnam=$temp."/".expand("%")."\.ann"
  call DoCVScommand("annotate ".expand("%:p:t"),tmpnam)
  wincmd _
endfunction

function! CVSlog()
  exec "cd ".expand("%:p:h")
  let tmpnam=$temp."/".expand("%")."\.log"
  call DoCVScommand("log ".expand("%:p:t"),tmpnam)
endfunction

function! CVSstatus()
  exec "cd ".expand("%:p:h")
  let tmpnam=$temp."/".expand("%")."\.stat"
  call DoCVScommand("status ".expand("%:p:t"),tmpnam)
endfunction

