"vim script for generating menu entries of local functions within a C/C++ file
"and include files, adds a menu entry under a main menu 'Proj'
"creates a file cfunctags.vim as script for generating the menu entries
"BUGS: does not handle references ('&') and destructors (~) correctly.
"create a menu entry in your _gvimrc to source in and call this script:
"($VIM_SYNTAX must contain the path to this script file)
"menu 12.10 Proj.Add\ local\ functions	:so $VIM_SYNTAX/functags.vim<CR>
:let startfile = expand("%")
:!rm -f cfunctags.vim
" generate tag file only with function definitions references
:exe "!ctags -i=f -o cfunctags.vim ".startfile
:e cfunctags.vim
" delete comment lines
:g/^!.*$/d
:1
"add menu entry to remove generated menu entries
:s/^/:12amenu Proj.remove\\ local\\ functions		:unmenu Proj.local\\ functions<CR>:unmenu! Proj.local\\ functions<CR>:unmenu Proj.remove\\ local\\ functions<CR>:unmenu! Proj.remove\\ local\\ functions<CR>/
"change tags to menu entries
:2,$s/^.*\/^\(.*\)\$\/.*$/:12amenu Proj.local\\ functions.\1		\/\1\/<CR>/

"use a function to modify menu entry command
":so $VIM_SYNTAX/HideBlanks.vim
:function! HideBlanks()
:let line=getline(".")
"get function prototype without ctags infos for menu entry
:let funcname=substitute (line, "^.*\\.\\([a-zA-Z0-9:\&\~_\*]\\+[ 	]*.*\\)		/.*", "\\1", "g")
"add '\' in front of blanks, '.' and '*'
"BUGGY: reference operator '&' is interpreted in subst cmd as \&
:let funcname=substitute (funcname, "\\([.*\~]\\)", "\\\\\\1","g")
:let funcname=substitute (funcname, "[ 	]\\+\\([^ 	]\\+\\)", "\\\\ \\1","g")
"build new line with start of menu command + changed function decl.
:let newline = substitute(line, "\\(.*\\.\\).*$", "\\1", "").funcname
:let newline=substitute (newline, "[\~]", "\\\~","g")
":echo "newline='".newline."'"
"add rest of menu command (search command for vim) and double all backslashes
:let rest = substitute(line, "^:.*		/", "		/", "")
:let rest = substitute(rest, "\\([.*\\[\\]]\\)", "\\\\\\1", "g")
:let newline = newline.rest
:let newline = substitute(newline, "\\\\", "\\\\\\\\", "g")
":echo "':s#^.*$#".newline."'"
"substitute actual line with newline containing the complete menu command
:exe ":s#^.*$#".newline."#"
:endf

:2,$ call HideBlanks()
:w
:so cfunctags.vim
:e! #
