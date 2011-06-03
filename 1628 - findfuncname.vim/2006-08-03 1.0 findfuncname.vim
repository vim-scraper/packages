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

    "while foundcontrol == 1, keep looking up the line to find something that
    "isn't a control statement
    let foundcontrol = 1
    while (foundcontrol)
        "find the most recent '{' and move to the beginning of the line above it
        ?{
        normal k0

        "paste the line into a variable to search for control statements
        let tempstring = getline(".")

            "if the line matches a control statement, set found control to 1 so 
            "we can look farther back in the file for the beginning of the 
            "actual function we are in
            if(match(tempstring, "while") >= 0)
                let foundcontrol = 1
            elseif(match(tempstring, "for") >= 0)
                let foundcontrol = 1
            elseif(match(tempstring, "if") >= 0)
                let foundcontrol = 1
            elseif(match(tempstring, "else") >= 0)
                let foundcontrol = 1
            elseif(match(tempstring, "switch") >= 0)
                let foundcontrol = 1
            elseif(match(tempstring, "try") >= 0)
                let foundcontrol = 1
            elseif(match(tempstring, "catch") >= 0)
                let foundcontrol = 1
            else
                normal `z
                let foundcontrol = 0
                return tempstring
            endif
    endwhile

    "return the line that we found to be the function name
    return tempstring
endfun

"return to previous 'compatibility' state
let &cpo = cpo_save
