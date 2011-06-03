" Vim script file
" Maintainer:  Lee Riemenschneider <lwriemen@frogooa.com>
" Last Change: 2005 Jan 14
" Use:         PVCS Version Manager access in vim
" Trademark:   PVCS Version Manager is a registered trademark of Merant, Inc.

" Set PVCS environment variables
<Set environment variables as needed and remove any unused.>
let $INIT = ""
let $PRJ_VMPATH = ""
let $PRJ_TOOLROOT = ""
let $PRJ_BLDROOT = ""
let $PRJ_DATAROOT = ""
let $PRJ_ARCROOT = ""
let $PRJ_DOMAIN = ""
let $PATH = expand("$PATH").";<Add any extra path or remove>
let $PVCS = ""
let $VCSID = ""
let $PRJ_WORKROOT = getcwd()

" Function to launch diffs in new windows.
" Only expects up to three arguments.
function View_launch(fn, rev, rev2)
   let $TEMPFIL=tempname()
   let $TEMPFIL2=tempname()
   let $REVNUM=a:rev
   let $REVNUM2=a:rev2
   if a:fn == "vdiff"
      if "" == $REVNUM2
         silent !vdiff -XO$TEMPFIL -r$REVNUM -B %
      else
         silent !vdiff -XO$TEMPFIL -r$REVNUM -r$REVNUM2 -B %
      endif
      !start gvim $TEMPFIL
   else
      silent !get -p -r$REVNUM -XO$TEMPFIL %
      if ("" != $REVNUM2)
         silent !get -p -r$REVNUM2 -XO$TEMPFIL2 %
         !start gvim -d $TEMPFIL $TEMPFIL2
      else
         !start gvim -d % $TEMPFIL
      endif
   endif
endfunction

function Version_input(fn)
   let n=inputdialog("Enter revision: ")
   if ("" != n)
      call View_launch(a:fn,n,"")
   endif
endfunction

function Two_version_input(fn)
   let n=inputdialog("Enter revision: ")
   if ("" != n)
      let o=inputdialog("Enter second revision: ")
      if ("" != o)
         call View_launch(a:fn,n,o)
      endif
   endif
endfunction

" Previous version VDIFF
menu PVCS.-VDIFF- :
menu PVCS.vdiff\ with\ Predecessor :call View_launch("vdiff","-1","")<CR>
" To vdiff workfile to revision, use the following with the workfile's 
" revision number.
menu PVCS.vdiff\ by\ revision :call Version_input("vdiff")<CR>
menu PVCS.vdiff\ revisions :call Two_version_input("vdiff")<CR>
" Previous version GVIMDIFF
menu PVCS.-GVIMDIFF- :
menu PVCS.gvimdiff\ with\ Predecessor :call View_launch("gvim","-1","")<CR>
" To vimdiff workfile to revision, use the following with the workfile's 
" revision number.
menu PVCS.gvimdiff\ by\ revision :call Version_input("gvim")<CR>
menu PVCS.gvimdiff\ revisions :call Two_version_input("gvim")<CR>
menu PVCS.-Sep- :
" To browse all versions of an archive.
menu PVCS.Browse\ versions :silent !vlog -BR % \| less<CR>


