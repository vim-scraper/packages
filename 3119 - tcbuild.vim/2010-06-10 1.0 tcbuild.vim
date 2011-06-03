" Vim syntax file
" Language: buildinfo file of TCbuild (build tool for Fortran)
" Maintainer: David Froger <david.froger@gmail.com>
" Latest Revision: 10 June 2010
"
" add this to your vimrc file:
" autocmd BufRead,BufNewFile buildinfo set filetype=tcbuild

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn match   tcbuildComment	"#.*$"

syn keyword tcbuildKey1	builddir targets defaultconfig configs
syn keyword tcbuildKey2       name rootdir buildsubdir libraryname
syn keyword tcbuildKey2       skipdirs skipfiles dependson compilegroups
syn keyword tcbuildKey2       exename mainprogramfile
syn keyword tcbuildKey2       compileroptions
syn keyword tcbuildKey2       inherits installdir
syn keyword tcbuildKey3       archivecommand unarchivecommand ranlibcommand
syn keyword tcbuildKey3       f77compiler f90compiler f77flags f90flags
syn keyword tcbuildKey3       modpathoption ccompiler cflags link linkflags
syn keyword tcbuildKey3       prioritylibs otherlibs compilegroupflags

let b:current_syntax = "tcbuild"

hi def link tcbuildComment    Comment
hi def link tcbuildKey1       Function
hi def link tcbuildKey2       Type
hi def link tcbuildKey3       Statement
