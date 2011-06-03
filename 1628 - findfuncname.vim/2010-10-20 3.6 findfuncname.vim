" Vim plugin for displaying the name of the function being edited
" Maintainer: Alex Esplin <alex.esplin@gmail.com>
" Version: 3.6
" Last Change: 2009 Sept 24

"don't load twice, we don't like that...
if exists("loaded_findfuncname")
    finish
endif
let loaded_findfuncname = 1

" you really ought to be using :set nocompatible...
set cpo&vim

" silently replace if we're already loaded...
function! FunctionName()
    " search backwards for our magic regex that works most of the time
    let flags = "bn"
    let fNum = search('^\w\+\s\+\w\+.*\n*\s*[(){:].*[,)]*\s*$', flags)
    " if we're in a python file, search backwards for the most recent def: or
    " class: declaration
    if match(expand("%:t"), ".py") != -1
        let dNum = search('^\s\+def\s*.*:\s*$', flags)
        let cNum = search('^\s*class\s.*:\s*$', flags)
        if dNum > cNum
            let fNum = dNum
        else
            let fNum = cNum
        endif
    endif

    "paste the matching line into a variable to display
    let tempstring = getline(fNum)

    "return the line that we found to be the function name
    return "line " . fNum . ": " . tempstring
endfun

