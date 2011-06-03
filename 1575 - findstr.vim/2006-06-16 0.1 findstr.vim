" File: findstr.vim
" Author: Yegappan Lakshmanan (yegappan AT yahoo DOT com)
" Version: 0.1
" Last Modified: June 16, 2006
" 
" Overview
" --------
" The findstr.vim plugin integrates the MS-Windows findstr search utility with
" Vim. You can use this plugin to search for a pattern or text in multiple
" files in the current directory or in a directory tree and easily jump to the
" matches. This plugin is useful only in MS-Windows based systems where the
" findstr utility is available.
"
" Installation
" ------------
" 1. Copy the findstr.vim file to the $HOME/vimfiles/plugin or
"    $VIM/vimfiles/plugin directory. 
"    Refer to the following Vim help topics for more information about Vim
"    plugins:
"       :help add-plugin
"       :help add-global-plugin
"       :help runtimepath
" 2. Restart Vim.
" 3. You can now use the ":Findstring" or ":Findpattern" command to search for
"    text/patterns in files.
"
" Usage
" -----
" The findstr.vim plugin introduces the ":Findstring", ":Findpattern",
" ":Rfindstr" and ":Rfindpattern" Vim commands to search for text and patterns
" in files.
"
" The ":Findstring" command is used to search for literal strings in files in
" the current directory. The ":Rfindstring" command is used to search for a
" literal string in the files in a directory tree.
"
" The ":Findpattern" command is used to search for a regular expression
" pattern in files in the current directory. The ":Rfindpattern" command is
" used to search for a regular expression pattern in the files in a directory
" tree.
"
" The syntax of these commands is
"
"    :Findstring [ <options> ] [ <search_string> [ <file_name(s)> ] ]
"    :Findpattern [ <options> ] [ <search_pattern> [ <file_name(s)> ] ]
"    :Rfindstring [ <options> ] [ <search_string> [ <file_name(s)> ] ]
"    :Rfindpattern [ <options> ] [ <search_pattern> [ <file_name(s)> ] ]
"
" In the above commands, all the arguments are optional.
"
" You can specify findstr options like /i (ignore case) to the above command.
" If the <options> are not specified, then the default findstr options
" specified by the variable Findstr_Default_Options is used. Refer to the
" findstr MS-Windows documentation for the command-line options supported by
" the findstr command.
"
" You can specify the search text/pattern as an argument to the above
" commands.  If the <search_pattern> is not specified, then you will be
" prompted to enter a search pattern. By default, the keyword under the cursor
" is displayed for the search pattern prompt. You can accept the default or
" modify it. The regular expression patterns supported by the findstr command
" can be used in the pattern argument. Refer to the findstr MS-Windows
" documentation for more information.
"
" If you want to specify a search pattern with space characters or a
" multi-word pattern, then you should use the ":Findstring" command text input
" prompt to supply the pattern.
"
" You can specify one or more file names (or file patterns) to the above
" commands.  If the <file_names> are not specified, then you will be prompted
" to enter file names.  By default, the pattern specified by the
" Findstr_Default_Filelist variable is used. To specify the file name(s) as an
" argument to the above commands, you have to specify the search pattern also.
"
" When you enter only the command name, you will be prompted to enter the
" search pattern and the files in which to search for the pattern. By default,
" the keyword under the cursor is displayed for the search pattern prompt.
" Depending on the command, you may prompted for additional parameters like
" the directories to search for the pattern.
"
" You can retrieve previously entered values for the ":Findstring" prompts
" using the up and down arrow keys. You can cancel the command by pressing the
" escape key.  You can use CTRL-U to erase the default shown for the prompt
" and CTRL-W to erase the previous word in the prompt. For more information
" about editing the prompt, read ':help cmdline-editing' Vim help topic.
"
" You can cancel the ":Findstring" command when you are prompted for a search
" pattern or file names by pressing the <Esc> key. You cannot cancel (or kill)
" the findstr command after the external command is invoked.
"
" You can map a key to invoke ":Findstring". For example, the following map
" invokes the ":Findstring" command to search for the keyword under the
" cursor:
"
"       nnoremap <silent> <F3> :Findstring<CR>
"
" The output of the findstr command will be listed in the Vim quickfix window.
" 1. You can select a line in the quickfix window and press <Enter> or double
"    click on a match to jump to that line.
" 2. You can use the ":cnext" and ":cprev" commands to the jump to the next or
"    previous output line.
" 3. You can use the ":colder" and ":cnewer" commands to go between multiple
"    findstr quickfix output.
" 4. The quickfix window need not be opened always to use the findstr output.
"    You can close the quickfix window and use the quickfix commands to jump
"    to the findstr matches.  Use the ":copen" command to open the quickfix
"    window again.
"
" For more information about other quickfix commands read ":help quickfix"
" 
" Configuration
" -------------
" By changing the following variables you can configure the behavior of this
" plugin. Set the following variables in your .vimrc file using the 'let'
" command.
"
" The ":Findstring" command will prompt you for the files in which to search
" for the pattern. The 'Findstr_Default_Filelist' variable is used to specify
" to default value for this prompt. By default, this variable is set to '*'.
" You can specify multiple matching patterns separated by spaces. You can
" change this settings using the let command:
"
"       :let Findstr_Default_Filelist = '*.[chS]'
"       :let Findstr_Default_Filelist = '*.c *.cpp *.asm'
"
" The 'Findstr_Default_Options' is used to pass default command line options
" to the findstr command. By default, this is set to an empty string. You can
" change this using the let command:
"
"       :let Findstr_Default_Options = '/i'
"
" By default, when you invoke the ":Findstring" command the quickfix window
" will be opened with the search output.  You can disable opening the quickfix
" window, by setting the 'Findstr_OpenQuickfixWindow' variable  to zero:
"
"       :let Findstr_OpenQuickfixWindow = 0
"
" You can manually open the quickfix window using the :cwindow command.
"
" --------------------- Do not modify after this line ---------------------
if exists("loaded_findstr")
    finish
endif
let loaded_findstr = 1

" Line continuation used here
let s:cpo_save = &cpo
set cpo&vim

" Open the search output window.  Set this variable to zero, to not open
" the search output window by default.  You can open it manually by using
" the :cwindow command.
if !exists("Findstr_OpenQuickfixWindow")
    let Findstr_OpenQuickfixWindow = 1
endif

" Default search file list
if !exists("Findstr_Default_Filelist")
    let Findstr_Default_Filelist = '*'
endif

" Default search options
if !exists("Findstr_Default_Options")
    let Findstr_Default_Options = ''
endif

" RunFindStr()
function! s:RunFindStr(cmd_name, cmd_opt, ...)
    if a:0 > 0 && (a:1 == "-?" || a:1 == "-h")
        echo 'Usage: ' . a:cmd_name . ' [<options>] [<search_pattern> ' .
                        \ "[<file_name(s)>]]"
        return
    endif

    let findstr_opt  = ''
    let pattern   = ''
    let filenames = ''

    " Parse the arguments
    " findstr command-line flags are specified using the "/flag" format.  The
    " next argument is assumed to be the pattern. The following arguments are
    " assumed to be file names or file patterns
    let argcnt = 1
    while argcnt <= a:0
        if a:{argcnt} =~ '^/'
            let findstr_opt = findstr_opt . " " . a:{argcnt}
        elseif pattern == ''
            let pattern = a:{argcnt}
        else
            let filenames= filenames . " " . a:{argcnt}
        endif
        let argcnt = argcnt + 1
    endwhile

    if findstr_opt == ''
        let findstr_opt = g:Findstr_Default_Options
    endif

    " Display line number for all the matching lines
    let findstr_opt = findstr_opt . ' /N ' . a:cmd_opt

    " Get the identifier and file list from user
    if pattern == '' 
        if a:cmd_name == 'Findstring' || a:cmd_name == 'Rfindstring'
            let prompt = 'Search for text: '
        else
            let prompt = 'Search for pattern: '
        endif
        let pattern = input(prompt, expand("<cword>"))
        if pattern == ''
            return
        endif
    endif

    if a:cmd_name == 'Rfindstring' || a:cmd_name == 'Rfindpattern'
        let startdir = input('Start searching from directory: ', getcwd())
        if startdir == ''
            return
        endif
        " Remove trailing backslash, if present
        let startdir = substitute(startdir, '\\$', '', '')
        let findstr_opt = findstr_opt . ' /D:' . startdir
    endif

    if filenames == ''
        let filenames = input("Search in files: ", g:Findstr_Default_Filelist)
        if filenames == ''
            return
        endif
    endif

    echo "\n"

    let cmd = "findstr " . findstr_opt
    let cmd = cmd . ' /C:"' . pattern . '"'
    let cmd = cmd . ' ' . filenames

    let cmd_output = system(cmd)

    if cmd_output == ''
        echohl WarningMsg | 
        \ echomsg "Error: Pattern " . pattern . " not found" | 
        \ echohl None
        return
    endif

    " The file names in the recursive search output are relative to the start
    " directory. Need to convert them to absolute path.
    if a:cmd_name == 'Rfindstring' || a:cmd_name == 'Rfindpattern'
        let spat = "[^\n]\\+\n"
        let rpat = escape(startdir, '\') . '\\&'
        let cmd_output = substitute(cmd_output, spat, rpat, "g")
    endif

    let tmpfile = tempname()

    let old_verbose = &verbose
    set verbose&vim

    exe "redir! > " . tmpfile
    silent echon '[Search results for pattern: ' . pattern . "]\n"
    silent echon cmd_output
    redir END

    let &verbose = old_verbose

    let old_efm = &efm
    set efm=%f:%\\s%#%l:%m

    if exists(":cgetfile")
        execute "silent! cgetfile " . tmpfile
    else
        execute "silent! cfile " . tmpfile
    endif

    let &efm = old_efm

    " Open the search output window
    if g:Findstr_OpenQuickfixWindow == 1
        " Open the quickfix window below the current window
        botright copen
    endif

    call delete(tmpfile)
endfunction

" Define the findstr commands
command! -nargs=* -complete=file Findstring
            \ call s:RunFindStr("Findstring", "/L", <f-args>)
command! -nargs=* -complete=file Findpattern
            \ call s:RunFindStr("Findpattern", "/R", <f-args>)
command! -nargs=* -complete=file Rfindstring
            \ call s:RunFindStr("Rfindstring", "/S /L", <f-args>)
command! -nargs=* -complete=file Rfindpattern
            \ call s:RunFindStr("Rfindpattern", "/S /R", <f-args>)

" restore 'cpo'
let &cpo = s:cpo_save
unlet s:cpo_save

