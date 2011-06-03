" Vim plugin for displaying the name of the function being edited
" Maintainer: Alex Esplin <alex.esplin@gmail.com>
" Version: 3.0
" Last Change: 2009 Feb 24

"don't load twice, we don't like that...
if exists("loaded_findfuncname")
    finish
endif
let loaded_findfuncname = 1

"save current 'compatible' state and make vim compatible
let cpo_save = $cpo
set cpo&vim

fun FunctionName()
    " search backwards for our magic regex that works most of the time
    let flags = "bn"
    let fNum = search('^\w\+.*\n*\s*[(){:]\s*$', flags)

    "paste the matching line into a variable to display
    let tempstring = getline(fNum)

    "return the line that we found to be the function name
    return "line " . fNum . ": " . tempstring
endfun

"return to previous 'compatibility' state
let &cpo = cpo_save
