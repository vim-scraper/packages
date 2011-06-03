" Vim global plugin for putting lists of numbers in a file
" Maintainer: Brian Medley <freesoftware@4321.tv>
" Version: 1.0

if exists("loaded_nlist")
    finish
endif
let loaded_nlist = 1

" don't do anything if someone else has already defined either a function or a
" command called 'Nlist'
if exists(":Nlist") || exists("*Nlist")
    finish
endif

" Description:
" This provides a command and a function.  They both can be called with or
" without a range.  In addition, they can be called with or without
" arguments.  Without a range they operate on the current line.  The arguments
" are described below:
"
"     arg1 -> the number to start at.  The default is one.  This will
"             number your selected lines sequentially.  The start can be a
"             number, ., $, or, 'x (like getline).
"     arg2 -> Text to append after numbers.  The default is a space.
"     arg3 -> Controls formatting.  Can be "left", "right", or "none".  The
"             default is "right".
"
" Examples:
"     To make a list start at 1:
"         :'<,'>Nlist
"         :'<,'>call Nlist()
"     To number the whole buffer (with it's actual line number):
"         :%Nlist
"         :%call Nlist()
"     To number a subset of lines with their line number (and put a '] ' in
"     front of every number):
"         :'<,'>Nlist . ]\
"         :'<,'>call Nlist(".", "] ")
"     To number the whole buffer (with it's actual line number) without
"     justification:
"         :%Nlist 1 \ left
"         :%call Nlist("1", " ", "none")

" Example mappings that a user can define:
" nnoremap <unique> <Leader>N :%Nlist<cr>
" vnoremap <unique> <Leader>N :Nlist<cr>

command -nargs=* -range Nlist <line1>,<line2>call Nlist(<f-args>)

function Nlist(...) range
    let start = 1
    let append = " "
    let s:formatting = "right"
    if 1 <= a:0
        let start = a:1 
    endif
    if 2 <= a:0
        let append = a:2
    endif
    if 3 <= a:0
        let s:formatting = a:3
    endif

    if "none" != s:formatting
        let s:max_len = start + (a:lastline - a:firstline)
        let s:max_len = strlen(s:max_len)
    endif

    " try to work like getline (i.e. allow the user to pass in . $ or 'x)
    if 0 == (start + 0)
        let start = line(start)
    endif

    " wow, vim kicks ass...:)
    exe a:firstline . "," . a:lastline . 's/^/\=s:JustifyNum((line(".")-a:firstline+start)).append/'
endfunction

let s:twenty_spaces="                    "
function s:JustifyNum(num)
    if "none" == s:formatting
        return a:num
    endif

    let len           = strlen(a:num)
    let needed_spaces = s:max_len - len

    if 0 == needed_spaces
        return a:num
    endif

    let spaces = strpart(s:twenty_spaces, 0, needed_spaces)
    if "left" == s:formatting
        return a:num . spaces
    else
        return spaces . a:num
    endif
endfunction
