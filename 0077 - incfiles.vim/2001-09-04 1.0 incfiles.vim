"create menu entry for include file (to enter it) within C/C++ files
"adds menu entries under main menu 'Proj'
"e.g. enter ':% call CreateIncFileMenu()' within your C/C++ file
"then you can enter any include file by selecting it in the menu
"create a menu entry in your _gvimrc to source in and call this script:
"($VIM_SYNTAX must contain the path to this script file)
"menu 12.10 Proj.Add\ include\ files		:so $VIM_SYNTAX/incfiles.vim<CR>:% call CreateIncFileMenu()<CR>
:function! CreateIncFileMenu()
" read in actual line
: let line=getline(".")
" if it is a include line
: let IncKeywd = substitute(line, "^\\(\\#include\\).*$", "\\1", "")
: if ("#include" == IncKeywd)
"   get filename
:   let IncFile = substitute(line, "^\\#include[ 	]\\+[\\<\\\"]\\([a-zA-Z0-9\\-_.:\\/\\\\]\\+\\)[\\>\\\"].*$", "\\1", "")
"   substitute backslashes with slashes, '.' with '\.'
:   let IncFile = substitute(IncFile, "\\\\", "/", "g")
:   let MenuFile = substitute(IncFile, "\\.", "\\\\.", "g")
":  echo MenuFile
"   create menu entries
:   12amenu File.-SEP2-				:<CR>
:   12amenu Proj.remove\ incfiles	:unmenu Proj.incfiles<CR>:unmenu!  Proj.incfiles<CR>:unmenu Proj.remove\ incfiles<CR>:unmenu! Proj.remove\ incfiles<CR>
:   exe "12amenu Proj.incfiles.".MenuFile."		:e ".IncFile."<CR>"
: endif
:endf
