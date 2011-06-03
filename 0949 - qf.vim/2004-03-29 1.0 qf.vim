" Copyright (c) 1998-2004
" Michael Sharpe <feline@irendi.com>
"
" We grant permission to use, copy modify, distribute, and sell this
" software for any purpose without fee, provided that the above copyright
" notice and this text are not removed. We make no guarantee about the
" suitability of this software for any purpose and we are not liable
" for any damages resulting from its use. Further, we are under no
" obligation to maintain or extend this software. It is provided on an
" "as is" basis without any expressed or implied warranty.

" TODO + allow for saving of the quick-fix window...by default it is
"        nonmodifiable
"      + when th quick fix window is modified it needs to be saved a reloaded
"        to have any effect.

if exists("loaded_qf")
   finish
endif
let loaded_qf = 1

" Function : QF_addProgramAndFormat (PUBLIC)
" Purpose  : Alls for the registration of a grepprg which will be used with the
"            quick-fix buffer
" Args     : id -- unique id for lookup
"            program -- program to run (will be a &grepprg program)
"            format -- quick-fix format for the program (can be empty)
" Returns  : nothing
" Author   : Michael Sharpe <feline@irendi.com>
function! <SID>QF_addProgramAndFormat(id, program, format)
   let varName = "g:QFProgram_".a:id
   if (!exists(varName))
      let g:QFProgram_{a:id} = a:program
      let g:QFFormat_{a:id} = a:format
   endif
endfunction

" Register some default programs. These seem to work well on linux (redhat9)
call <SID>QF_addProgramAndFormat('lid', 'lid -f $HOME/IDDB -R grep "$*"', "")
call <SID>QF_addProgramAndFormat('grep', 'grep -n $*', "")
call <SID>QF_addProgramAndFormat('rgrep', 'grep -n -r $* /dev/null', "")
call <SID>QF_addProgramAndFormat('locate', 'locate $*', '%f')
call <SID>QF_addProgramAndFormat('find', 'find $* -print', '%f')

" Some special case helpers which can be useful
call <SID>QF_addProgramAndFormat('raw', '$*', '')
call <SID>QF_addProgramAndFormat('loadfile', 'cat $*', '%f|%l|%m')

" Function : QF_execProgram (PUBLIC)
" Purpose  : Executes a specified program via using vim's builtin grep command
"            in conjuction with the quick-fix functionality
" Args     : program -- program to execute (via &grepprg)
"            format -- format for the quick-fix window
"            args -- args for the program (args to :grep command)
"            addFlag -- if non-empty results are add to existing quick-fix list
" Returns  : Nothing
" Author   : Michael Sharpe <feline@irendi.com>
function! <SID>QF_execProgram(program, format, args, addFlag)
   " save the old grepprg and grepformat so that they can be restored later
   let old_grepprg=&grepprg
   let old_grepformat=&grepformat

   " install the specified grepprg
   let &grepprg=a:program

   " and install the format if one was specified (TODO should support appending)
   if (a:format != "")
      let &grepformat=a:format
   endif

   " Run vim's builtin grep command which will invoke the grepprg
   if (a:addFlag != "")
     exec "grepadd ".a:args
   else
     exec "grep ".a:args
   endif
   " Open the quick-fix window (TODO should be configurable via global?)
   exec "cwin"
   " Restore the old settings
   let &grepprg=old_grepprg
   let &grepformat=old_grepformat
endfunction

" Function : QF_doExecute (PUBLIC)
" Purpose  : Looks up a registered program and executes it via vim's builtin
"            grep command
" Args     : id -- id of the program
"            args -- arguments for the program
"            addFlag -- if non-empty result are added to existing quickfix list
" Returns  : Nothing
" Author   : Michael Sharpe <feline@irendi.com>
function! <SID>QF_doExecute(id, args, addFlag)
   let varName = "g:QFProgram_".a:id
   " if the command identified by id exists, execute it
   if (exists(varName))
      call <SID>QF_execProgram(g:QFProgram_{a:id}, g:QFFormat_{a:id}, a:args, a:addFlag)
   endif
endfunction

" Function : QF_lookupAndRunProgram (PUBLIC)
" Purpose  : Looks up a registered program and executes it, as above, and using
"            above actually. This is simply a wrapper for QF_doExecute()
" Args     : allArgs -- argument of the form "<id> <args>" for use with
"                       QF_doExecute()
" Returns  : Nothing
" Author   : Michael Sharpe <feline@irendi.com>
function! <SID>QF_lookupAndRunProgram(allArgs, addFlag)
   " find the first space
   let pos = stridx(a:allArgs, ' ')
   if (pos)
      " first field is the id of the command
      let id =  strpart(a:allArgs, 0, pos)
      " rest of the string is the arguments for the corresponding program
      let args = strpart(a:allArgs, pos)
      call <SID>QF_doExecute(id, args, a:addFlag)
   endif
endfunction

" In all cases below, the ! (i.e.<bang> will append new results to existing
" quick-fix window. 

" Allows any registered quick-fix command to be executed
" QF <id> <args> <-- finds the program corresponding to "id" in the
" table and executes it with the specified arguments "args"
" e.g. :QF grep foobar *.cpp <-- will grep all cpp files for foobar
command -nargs=* -bang QF call <SID>QF_lookupAndRunProgram("<args>", "<bang>")

" The following commands are short cuts for the QF command (very short short
" cuts....in that they really do not save too much typing :)

" E.g. :QFlid static   <-- will find all lines with the word "static" using GNU
" id-utils
command -nargs=* -bang QFlid call <SID>QF_doExecute('lid', "<args>", "<bang>")

" E.g. :QFgrep static *.cpp <-- will find all lines matching "static" in the
" .cpp files of the current directory
command -nargs=* -bang QFgrep call <SID>QF_doExecute('grep', "<args>", "<bang>")

" E.g. :QFrgrep static src  <-- will recursively grep the src directory for all
" files containing static
command -nargs=* -bang QFrgrep call <SID>QF_doExecute('rgrep',"<args>", "<bang>")

" E.g. :QFlocate pattern <-- will locate all files matching pattern via the
" slocate functionality available on linux and elsewhere
command -nargs=* -bang QFlocate call <SID>QF_doExecute('locate', "<args>", "<bang>")

" E.g. QFfind . -name '*.cpp' <-- will find all cpp files under the current
" directory
command -nargs=* -bang QFfind call <SID>QF_doExecute('find', "<args>", "<bang>")


" Allows a saved Quick-fix window which was previously saved to be restored
" E.g. :QFload /tmp/foo <-- will load contents of /tmp/foo into quick-fix
" buffer assuming /tmp/foo contains the contents of a previously save quick-fix
" buffer
command -nargs=* -bang QFload call <SID>QF_doExecute('loadfile', "<args>", "<bang>")

" Allows the entry of a raw command, useful in some cases, and for testing
command -nargs=+ -bang QFE call <SID>QF_execProgram(<f-args>, "<bang>")
" Allows the entry of a raw command
command -nargs=* -bang QFR call <SID>QF_doExecute('raw', "<args>", "<bang>")
