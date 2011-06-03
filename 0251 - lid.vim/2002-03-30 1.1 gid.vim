" File: gid.vim
" Author: Yegappan Lakshmanan
" Version: 1.1
" Last Modified: March 30 2002
" 
" The gid.vim Vim plugin provides a way to interact with the gid tool to
" lookup keywords in the ID database.
" 
" For more information about id utilities (gid, lid, aid, etc), visit the
" following link: http://www.gnu.org/software/idutils/idutils.html
" 
" You can lookup keywords in the ID database using the 'Gid' command. For
" example,
" 
"       :Gid<Enter> 
" 
" This will prompt you for the keyword to lookup.  The default is the current
" keyword under the cursor.  You can retrieve previously entered keywords
" using the up and down arrow keys. You can cancel the lookup by pressing the
" escape key.
" 
" You can also specify the keyword to the Gid command like this:
" 
"       :Gid <keyword>
" 
" In the above command format, you can press the <Tab> key to expand
" keywords from a tags file.
"
" You can use the "-p" and "-v" option to the 'Gid' command to selectively
" display lines from the gid output.
" 
" You can use the "-p <pattern>" option to the 'Gid' command to list only
" those gid matches that contain the <pattern>.  For example,
" 
"       :Gid -p <pattern> <keyword>
" 
" You can use the "-v <pattern>" option to the 'Gid' command to list
" only those gid matches that does not contain the <pattern>.  For
" example,
" 
"       :Gid -v <pattern> <keyword>
" 
" Only one of the "-p" or "-v" options can be used at a time.
" 
" By default, the '<F4>' key is mapped to run the 'Gid' command for the word
" under the cursor.  You can change this key by setting the 'GID_Key'
" variable in your .vimrc file:
"
"       let GID_Key = '<F9>'
" 
" The output of the gid command will be listed in the Vim quickfix window.
" 1. You can select a line in the quickfix window and press <Enter> or double
"    click on a match to jump to that line.
" 2. You can use the ":cnext" and ":cprev" commands to the jump to the next or
"    previous output line.
" 3. You can use the ":colder" and ":cnewer" commands to go between multiple
"    Gid quickfix output windows.
" 4. The quickfix window need not be opened always to use the gid output.
"    You can close the quickfix window and use the quickfix commands to jump
"    to the gid matches.  Use the ":copen" command to open the quickfix
"    window again.
"
" For more information about other quickfix commands read ":help quickfix"
" 
" The path to the gid executable is specified by the 'GID_Path' variable
" defined at the top of the script.  You can change the gid executable path
" by setting the 'GID_Path' variable in your .vimrc file using the ':let'
" command.
"
"       let GID_Path = '/my/path/gid'
" 
" By default, the script uses 'ID' as the name of the database.  This is
" defined by the 'GID_File' variable defined at the top of the script.
" You can change the name/location of the ID database by setting the
" 'GID_File' variable in your .vimrc file:
"
"       let GID_File = '/my/path/gid'
" 
" By default, when you invoke the :Gid command the quickfix window will be
" opened with the gid output.  You can disable opening the quickfix window,
" by modifying the 'GID_OpenQuickfixWindow' variable defined at the top of the
" script:
"
"       let GID_OpenQuickfixWindow = 1
"
" You can manually open the quickfix window using the :cwindow command.
"

if exists("loaded_gid")
    finish
endif
let loaded_gid = 1

"
" To modify any of the following variable values, set them in your .vimrc
" file using the ':let' commands.
"

" The default location of the gid tool.
if !exists("GID_Path")
    "let GID_Path = 'd:\tools\gid.exe'
    let GID_Path = '/usr/bin/gid'
endif

" Name of the ID file to supply to gid
if !exists("GID_File")
    let GID_File = "ID"
endif

" Open the GID output window.  Set this variable to zero, to not open
" the GID output window by default.  You can open it manually by using
" the :cwindow command.
if !exists("GID_OpenQuickfixWindow")
    let GID_OpenQuickfixWindow = 1
endif

" Key to invoke gid on the current word.
if !exists("GID_Key")
    let GID_Key = '<F4>'
endif

" --------------------- Do not edit after this line ------------------------

" Make sure the gid tools is available
if !executable(GID_Path)
    let msg = "Error: [gid.vim] gid doesn't exist at " . GID_Path
    echohl WarningMsg | echon msg | echohl None
    finish
endif

" Map the key to invoke gid for the current word
exe "nnoremap <unique> <silent> " . GID_Key . " :call <SID>RunGid()<CR>"

" Extract lines matching the supplied pattern from the supplied text
function! s:ExtractMatchingLines(txt, pattern)
    let filter_output = ""
    let t = a:txt

    while t != ""
        let one_line = substitute(t, "\\([^\n]*\n\\).*", "\\1", "")
        let t = substitute(t, "[^\n]*\n", "", "")

        if one_line =~# a:pattern
            let filter_output = filter_output . one_line
        endif
    endwhile

    return filter_output
endfunction

" Remove lines matching the supplied pattern from the supplied text
function! s:RemoveMatchingLines(txt, pattern)
    let filter_output = ""
    let t = a:txt

    while t != ""
        let one_line = substitute(t, "\\([^\n]*\n\\).*", "\\1", "")
        let t = substitute(t, "[^\n]*\n", "", "")

        if one_line !~# a:pattern
            let filter_output = filter_output . one_line
        endif
    endwhile

    return filter_output
endfunction

" Run gid using the supplied arguments
function! s:RunGid(...)
    let usage="Usage: Gid [[-p pattern] [-v pattern]] [identifier]"

    let skip_pat = ""
    let match_pat = ""

    " Process options
    if a:0 == 0
        " No argument supplied. Get the identifier from the user
        let id = input("Gid identifier: ", expand("<cword>"))
        if id == ""
            return
        endif
        echo "\n"
    else
        if (a:1 == "-p" || a:1 == "-v") && a:0 != 3
            echohl WarningMsg | echo usage | echohl None
            return 1
        endif

        if a:0 == 1  " Only the identifier is supplied
            let id = a:1
        else
            " Some options are supplied.  Currently either -v or -p options are
            " supported.  The -v option specifies the pattern to skip in the gid
            " output.  The -p option specifies the pattern to search for in the
            " gid output.  So there must be 3 arguments to this function when a
            " option is supplied.
            if a:1 == "-v"
                let skip_pat=a:2
            elseif a:1 == "-p"
                let match_pat=a:2
            else
                " Unknown argument
                echohl WarningMsg | echo usage | echohl None
                return 1
            endif

            let id = a:3   " identifier to look for
        endif
    endif

    let cmd = g:GID_Path . " -f " . g:GID_File . " '" . id . "'"

    let cmd_output = system(cmd)

    if v:shell_error && cmd_output != ""
        echohl WarningMsg | echon cmd_output | echohl None
        return
    endif

    if cmd_output == ""
        echohl WarningMsg | echo "Error: Identifier " . id . " not found" | 
                    \ echohl None
        return
    endif

    " Remove or extract lines containing the user specified pattern
    if match_pat != ""
        let cmd_output = s:ExtractMatchingLines(cmd_output, match_pat)
    endif

    if skip_pat != ""
        let cmd_output = s:RemoveMatchingLines(cmd_output, skip_pat)
    endif

    " Send the output to a temporary file to use with the :cfile command
    let tmpfile = tempname()

    exe "redir! > " . tmpfile
    silent echon cmd_output
    redir END

    let old_efm = &efm
    set efm=%f:%l:%m

    execute "silent! cfile " . tmpfile

    let &efm = old_efm

    " Open the gid output window
    if g:GID_OpenQuickfixWindow == 1
        " Open the quickfix window below the current window
        botright copen
    endif

    " Jump to the first match
    cc

    call delete(tmpfile)
endfunction

" Define the Gid command to run gid
command! -nargs=* -complete=tag Gid call s:RunGid(<f-args>)

