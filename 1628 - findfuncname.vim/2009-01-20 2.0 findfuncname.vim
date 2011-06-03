" Vim plugin for displaying the name of the function being edited
" Maintainer: Alex Esplin <alex.esplin@gmail.com>
" Version: 1.0
" Last Change: 2006 August 3

"don't load twice, we don't like that...
if exists("loaded_findfuncname")
    finish
endif
let loaded_findfuncname = 1

"save current 'compatible' state and make vim compatible
let cpo_save = $cpo
set cpo&vim

fun FunctionName()
    "set a mark at our current position
    normal mz

    " search backwards for our magic regex that works most of the time
    ?^\s*\w\+\s\+\w\+[\s*|\w*\s*|(]

    "paste the line into a variable to search for control statements
    let tempstring = getline(".")

    " go back to our mark
    normal 'z

    "return the line that we found to be the function name
    return tempstring
endfun

"return to previous 'compatibility' state
let &cpo = cpo_save
