" commentsquawk.vim

"                   SQUAWK!!!
"  :                 /    :
"  ::               /    ::
"  ::`.     .-""-. /   .'::
"  : `.`-._ : '>': _.-'.' :
"  :`. `=._`'.  .''_.=' .':
"   : `=._ `- '' -' _.-'.:
"    :`=._`=.    .='_.=':
"     `.._`.      .'_..'
"
"       `-.:      :.-'
"          :      :
"          `:.__.:'
"           :    :
"          -'=  -'=


" Language: PHP
" Last Change: 2011 Jan 25
" Maintainer: Randall Reynolds <randall.reynolds@rightnow.com>
" Purpose: Checks for missing comments on functions in PHP files.


" Disable loading if the loaded flag is set
if exists("loaded_commentsquawk")
    finish
endif
let g:loaded_commentsquawk = 1

" Save Line Continuation
let s:save_cpo = &cpo
set cpo&vim

" -- Private functions. --

" Check for missing comments before functions and stop the write if comments are missing.
function CheckComments()
    " Put all the lines into a list.
    let line_list = getline(1, line('$'))

    " Pseudo Code:
    " For each line:
        " If the line says function blah(
        " If the previous line does have a '*/' or '//', then it's an uncommented function, so cancel the write, go to the line, and show an error message.

    " List of function names to omit because they usually do not require comments.
    let omits = ['__construct', 'generateWidgetInformation', 'getData']

    let abort_write = 0 " Whether to abort the write operation.
    let fail_line = 0
    for idx in range(0, len(line_list)-1)
        " If the current line is a function.
        if line_list[idx] =~ "^.*\\s*function.*(.*).*$"
            " If we should omit the function.
            if (len(filter(omits, 'line_list[idx] =~ "^.*\\s*function\\s*" . v:val  . "\\s*(.*).*$"')) > 0)
                continue
            endif

            " If the line before the function line does not have a comment.
            if (idx == 0 || (line_list[idx - 1] !~ "\*\/" && line_list[idx - 1] !~ "\/\/"))
                let abort_write = 1
                let fail_line = idx
            endif
        endif
    endfor

    if (abort_write)
        call setpos(".", [0, fail_line+1, 1, 0])
        let really = confirm("Error: Missing function comments!  Are you sure you want to write the file?", "&Yes\n&No", 2)
        if (really == 1)
            write
        endif
    else
        write
    endif

    return
endfunction

" -- Script Body --

" Redefine the Write command.
autocmd BufWriteCmd *.php call CheckComments()

" Restore Line Continuation
let &cpo = s:save_cpo
