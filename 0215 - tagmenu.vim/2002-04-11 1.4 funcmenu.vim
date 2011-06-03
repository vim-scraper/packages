" File: funcmenu.vim
" Author: Yegappan Lakshmanan
" Version: 1.4
" Last Modified: April 11, 2002
"
" Overview
" --------
" The "Function Menu" (funcmenu.vim) plugin script creates a "Functions" menu
" containing all the functions defined in the current file.  If you select a
" function name from the menu, the cursor will be positioned at the location
" of the selected function.
"
" As this script uses menus, this will work only with GUI Vim.  This script
" relies on the exuberant ctags utility (http://ctags.sourceforge.net) to
" generate the function listing.  This script will run on all the platforms
" where the exuberant ctags utility is supported (this includes MS-Windows and
" Unix based systems).
"
" This script supports the following language files: Assembly, ASP, Awk, C,
" C++, Cobol, Eiffel, Fortran, Java, Lisp, Make, Pascal, Perl, PHP, Python,
" Rexx, Ruby, Scheme, Shell, Slang, TCL and Vim.
"
" When you enter a file, a menu with all the functions in the file will be
" automatically created.  When you leave the file, the menu entries will be
" removed.
"
" Configuration
" -------------
" The script uses the Fmenu_ctags_path variable to locate the ctags utility.
" By default, this is set to /usr/bin/ctags. Set this variable in your .vimrc
" file to point to the location of the ctags utility in your system
"
"              let Fmenu_ctags_path = 'd:\tools\ctags.exe'
"
" If a file contains too many functions, greater than a configurable limit,
" then the function menu will be split into sub-menus.  The default limit is
" 25.  This can be changed by setting the Fmenu_max_items variable in
" your .vimrc file:
"
"             let Fmenu_max_items = 20
"
" By default, the function names will be added to the menu in the order in
" which they are defined in the file. You can alphabetically sort the function
" names in the menu by selecting the "Sort By->Name" menu item. You can also
" change the default order by setting the variable Fmenu_sort_type to
" "name" or "order" in your .vimrc file:
"
"             let Fmenu_sort_type = "name"
"
" You can also tear-off the function menu.  The function menu will be updated
" automatically as you switch between files.
"
" This script will not work in 'compatible' mode.  Make sure the 'compatible'
" option is not set.
"
"
if exists("loaded_funcmenu") || &cp
    finish
endif
let loaded_funcmenu=1

" This script is useful only in GUI Vim
if !has("gui_running")
    finish
endif

"
" To modify any of the following variable values, set them in your .vimrc
" file using the ':let' commands.
"

" The default location of the exuberant ctags
if !exists("Fmenu_ctags_path")
    "let Fmenu_ctags_path = '/usr/bin/ctags'
    let Fmenu_ctags_path = 'd:\vim\ctags.exe'
endif

" Maximum number of functions, after which the menu will be split into
" submenus.
if !exists("Fmenu_max_items")
    let Fmenu_max_items = 25
endif

" Function name sort type
if !exists("Fmenu_sort_type")
    let Fmenu_sort_type = "order"
endif

" ****************** Do not modify after this line ************************

" Function menu empty or not?
let s:fmenu_empty = 1

function! s:Remove_Function_Menu()
    if !has("gui_running") || s:fmenu_empty
        return
    endif

    " Cleanup the Functions menu
    silent! unmenu F&unctions
    menu F&unctions.Dummy l
    silent! unmenu! F&unctions

    amenu <silent> F&unctions.Refresh :call <SID>Refresh_Function_Menu()<CR>
    amenu <silent> F&unctions.Sort\ By.Name :call <SID>Sort_Function_Menu("name")<CR>
    amenu <silent> F&unctions.Sort\ By.Order :call <SID>Sort_Function_Menu("order")<CR>
    amenu F&unctions.-SEP1-           :
    unmenu F&unctions.Dummy

    let s:fmenu_empty = 1
endfunction

" Add_Function_Menu
"   Add the functions defined in the current file to the menu
function! s:Add_Function_Menu(menu_clear)
    if !has("gui_running")
        return
    endif

    if (a:menu_clear)
        " Cleanup the Functions menu
        silent! unmenu F&unctions
        menu F&unctions.Dummy l
        silent! unmenu! F&unctions

        amenu <silent> F&unctions.Refresh :call <SID>Refresh_Function_Menu()<CR>
        amenu <silent> F&unctions.Sort\ By.Name :call <SID>Sort_Function_Menu("name")<CR>
        amenu <silent> F&unctions.Sort\ By.Order :call <SID>Sort_Function_Menu("order")<CR>
        amenu F&unctions.-SEP1-           :
        unmenu F&unctions.Dummy

        let s:fmenu_empty = 1
    endif


    let filename = expand("%")

    " empty filename
    if filename == ""
        return
    endif

    " Make sure the file is readable
    if !filereadable(filename)
        return
    endif

    " Exuberant ctags arguments to generate a function list
    let ctags_args = " --format=0 -f - "

    if g:Fmenu_sort_type == "name"
        let ctags_args = ctags_args . " --sort=yes "
    else
        let ctags_args = ctags_args . " --sort=no "
    endif

    let ftype = &filetype

    if ftype == 'asm'
        let ft_args = " --language-force=asm --asm-types=l "
    elseif ftype == 'aspperl' || ftype == 'aspvbs'
        let ft_args = " --language-force=asp --asp-types=fs "
    elseif ftype == 'awk'
        let ft_args = " --language-force=awk --awk-types=f "
    elseif ftype == 'c'
        let ft_args = " --language-force=c --c-types=f "
    elseif ftype == 'cpp'
        let ft_args = " --language-force=c++ --c++-types=f "
    elseif ftype == 'cobol'
        let ft_args = " --language-force=cobol --cobol-types=p "
    elseif ftype == 'eiffel'
        let ft_args = " --language-force=eiffel --eiffel-types=f "
    elseif ftype == 'fortran'
        let ft_args = " --language-force=fortran --fortran-types=f "
    elseif ftype == 'java'
        let ft_args = " --language-force=java --java-types=m "
    elseif ftype == 'lisp'
        let ft_args = " --language-force=lisp --lisp-types=f "
    elseif ftype == 'make'
        let ft_args = " --language-force=make --make-types=m "
    elseif ftype == 'pascal'
        let ft_args = " --language-force=pascal --pascal-types=f "
    elseif ftype == 'perl'
        let ft_args = " --language-force=perl --perl-types=s "
    elseif ftype == 'php'
        let ft_args = " --language-force=php --php-types=f "
    elseif ftype == 'python'
        let ft_args = " --language-force=python --python-types=f "
    elseif ftype == 'rexx'
        let ft_args = " --language-force=rexx --rexx-types=s "
    elseif ftype == 'ruby'
        let ft_args = " --language-force=ruby --ruby-types=f "
    elseif ftype == 'scheme'
        let ft_args = " --language-force=scheme --scheme-types=f "
    elseif ftype =~ '\<[cz]\=sh\>'
        let ft_args = " --language-force=sh --sh-types=f "
    elseif ftype == 'slang'
        let ft_args = " --language-force=slang --slang-types=f "
    elseif ftype == 'tcl'
        let ft_args = " --language-force=tcl --tcl-types=p "
    elseif ftype == 'vim'
        let ft_args = " --language-force=vim --vim-types=f "
    else
        return
    endif

    let ctags_cmd = g:Fmenu_ctags_path . ctags_args . ft_args
    let ctags_cmd = ctags_cmd . '"' . filename . '"'

    " Get the function list
    let ctags_cmd_output = system(ctags_cmd)

    if v:shell_error && ctags_cmd_output != ""
        echohl WarningMsg | echon ctags_cmd_output | echohl None
        return
    endif

    " We are going to add entries to the function menu, so the menu
    " won't be empty
    let s:fmenu_empty = 0

    " Add the 'B' flag to the 'cpoptions' option
    let old_cpo = &cpo
    set cpo+=B

    " If more than max_menu_items functions are defined in the file, then
    " create submenus and add max_menu_items functions to each submenu

    " The number of functions in the ctags output is determined below
    " like this: Remove the max_menu_items number of entries from the
    " ctags output.  If there are still entries left in the output, the
    " submenu should be created, otherwise there is no need for a submenu.
    let p1="\\%([^\n]\\+\n\\)\\{," . g:Fmenu_max_items . "}"
    let txt = matchstr(ctags_cmd_output, p1)
    let ctags_cmd_output = substitute(ctags_cmd_output, p1, "", "")

    let p2 = "\\([^\t]\\+\\)\t\\([^\t]\\+\\)\t\/^\\([^$]*\\)\\\$\/\n"

    if ctags_cmd_output == ""
        " Pattern to locate the function definition
        let fpat = "'" . '\\\V\\\^\3\\\$' . "'"
        let repl = 'anoremenu <silent> F\&unctions.\1 '
        let repl = repl . ":call search(" . fpat . ', "w")<CR>|'
        let menu_items = substitute(txt, p2, repl, "g")
        exe menu_items
    else
        let submenu_num = 1

        while txt != ""
            let fpat = "'" . '\\\V\\\^\3\\\$' . "'"
            let repl = 'anoremenu <silent> F\&unctions.Listing'
            let repl = repl . '\&' . submenu_num . '.\1 '
            let repl = repl . ":call search(" . fpat . ', "w")<CR>|'
            let menu_items = substitute(txt, p2, repl, "g")
            exe menu_items

            let txt = matchstr(ctags_cmd_output, p1)
            let ctags_cmd_output = substitute(ctags_cmd_output, p1, "", "")
            let submenu_num = submenu_num + 1
        endwhile
    endif

    " Restore the 'cpoptions' settings
    let &cpo = old_cpo
endfunction

function! s:Refresh_Function_Menu()
    call s:Add_Function_Menu(1)
endfunction

function! s:Sort_Function_Menu(sort_type)
    let g:Fmenu_sort_type = a:sort_type
    call s:Add_Function_Menu(1)
endfunction

amenu <silent> F&unctions.Refresh :call <SID>Refresh_Function_Menu()<CR>
amenu <silent> F&unctions.Sort\ By.Name :call <SID>Sort_Function_Menu("name")<CR>
amenu <silent> F&unctions.Sort\ By.Order :call <SID>Sort_Function_Menu("order")<CR>
amenu F&unctions.-SEP1-           :

" Automatically add the functions in the current file to the menu
augroup FunctionMenuAutoCmds
    autocmd!

    autocmd BufEnter * call s:Add_Function_Menu(0)
    autocmd BufLeave * call s:Remove_Function_Menu()
augroup end
