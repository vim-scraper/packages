" localvariables.vim -- Set/let per-file-variables à la Emacs
" @Author:      Thomas Link (samul@web.de)
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     08-Dec-2003.
" @Last Change: 18-Dez-2003.
" @Revision: 183
" 
" Description:
" This plugin tries to mimic Emacs's file local variables feature. File local 
" variables are kept in a section in the last g:localVariablesRange 
" (default=30) lines of a file (this variable also defines the maximum number 
" of file local variables).
"
" The beginning of the section is marked with g:localVariablesBegText 
" (default="Local Variables:"), the end with g:localVariablesEndText 
" (default="End:"). These markers can be surrounded by arbitrary prefix and 
" suffix markers. These (optional) prefixes and suffixes must be consistent 
" for the whole section. The prefix and suffix are defined by the end-marker.
" 
" If a variable name contains a hyphen ("-"), the variable name will be 
" translated into a buffer local variable in camel case, i.e. "comment-start" 
" becomes "b:commentStart". A hyphendized variable's scope is always "b". If 
" g:localVariablesDownCaseHyphenedNames (default=1) is true, then the variable 
" name will be downcased first for compatibility reasons -- as (emacs) lisp is 
" case insensitive. I.e., "COMMENT-START" becomes "b:commentStart", too.
" 
" You can use also variables with the prefix "g:localVariableX" for 
" translating variable names without scope definitions. E.g., if 
" g:localVariableXtabWidth is "&tabstop", then the line "tab-width:4" will set 
" the option |tabstop| instead of "b:tabWidth".
"
" As an alternative, you can also define a function with the prefix 
" "LocalVariableX" which will be called with the value as single argument.  
" This provides a controlled way for executing commands, when loading/editing 
" a file. By defining a function "LocalVariableXmode", this can be used to 
" reuse Emacs's mode annotations for files that don't match the usual file 
" patterns.
"
" A variable definition has the form (as regexp):
" 
"	((scope char:)?(variable|special name)|hyphened name):value
" 
" If no scope character is given, the variable is buffer local ("b"-scope).
"
" If the scope character is "&", |set| will be used to set the variable.
" 
" If the scope is ":", the variable name is interpreted as a special name. At 
" the moment of writing, there is one :-) *special* name known:
" 
" - execute :: If g:localVariablesAllowExec is true, the value-part will be 
"   evaluated using |execute|. The value of g:localVariablesAllowExec can't be 
"   changed by this script. (The first 4 letters of "execute" are
"   essential.)
" 
" Example: Put something like this at the end of a file.)
" ### Local Variables: ###
" ### b:variable1:1 ###
" ### g:variable2:"2" ###
" ### variable3:"three" ###
" ### tab-width:4 ###
" ### ::exec:echo "Got it!" ###
" ### End: ###
" 
" Usage:
" Put this file into your plugin directory and insert something like this into 
" your .vimrc:
" 
" 	autocmd BufReadPost * call LocalVariablesCheck()
" 	
" Or rather:
" 
" 	autocmd BufEnter    * call LocalVariablesCheck()
" 
" When calling LocalVariablesCheck(), the buffer will only be scanned once.  
" Force an re-evaluation of file local variables by calling 
" LocalVariablesReCheck().
" 

fun! <SID>ConditionalLet(var,value)
    if !exists(a:var)
        exe "let ".a:var." = ".a:value
    endif
endfun

call <SID>ConditionalLet("g:localVariablesRange",     "30")
call <SID>ConditionalLet("g:localVariablesBegText",   "'Local Variables:'")
call <SID>ConditionalLet("g:localVariablesEndText",   "'End:'")
call <SID>ConditionalLet("g:localVariablesAllowExec", "0")
call <SID>ConditionalLet("g:localVariablesDownCaseHyphenedNames", "1")

fun! <SID>LocalVariablesSet(line, prefix, suffix)
    let l:scope     = ""
    let l:prefixEnd = strlen(a:prefix)
    let l:scopeEnd  = matchend(a:line, "^.:", l:prefixEnd)
    if l:scopeEnd >= 0
        let l:scope = strpart(a:line, l:prefixEnd, 2)
    else
        let l:scopeEnd = l:prefixEnd
    endif
    
    let l:varEnd = matchend(a:line, ".\\+:", l:scopeEnd)
    if l:varEnd >= 0
        let l:var = strpart(a:line, l:scopeEnd, l:varEnd-l:scopeEnd-1)
        if l:var =~ "-"
            if g:localVariablesDownCaseHyphenedNames
                let l:var = tolower(l:var)
            endif
            let l:var = substitute(l:var, "-\\(.\\)", "\\U\\1", "g")
        endif
    else
        throw "Local Variables: No variable name found in: ".a:line
    endif
    if l:scope == ""
        if exists("g:localVariableX".l:var)
            let l:var = g:localVariableX{l:var}
        elseif exists("*LocalVariableX".l:var)
            let l:scope = "X"
        else
            let l:scope = "b:"
        endif
    endif
    
    let l:valLen = strlen(a:line) - l:varEnd - strlen(a:suffix)
    if l:valLen > 0
        let l:value = strpart(a:line, l:varEnd, l:valLen)
    else
        throw "Local Variables: No value given for ".l:var
    endif
    
    if l:scope == "::"
        if l:var =~ "^\\cexec\\(u\\(t\\(e\\)\\?\\)\\?\\)\\?$"
            if g:localVariablesAllowExec
                exe l:value
            else
                echomsg "Local Variables: Disabled: ".l:value
            endif
        else
            throw "Local Variables: Unknown special name: ".l:var
        endif
    elseif l:scope ==# "X"
        call LocalVariableX{l:var}(l:value)
    elseif l:var ==# "localVariablesAllowExec"
        throw "Local Variables: Can't set: ".l:var
    elseif l:scope ==# "&:"
        exe "set ".l:var."=".l:value
    else
        exe "let ".l:scope.l:var." = ".l:value
    endif
endfun

fun! LocalVariablesReCheck()
    let l:count    = 0
    let l:found    = 0
    let l:lvend    = 0
    let l:prefix   = ""
    let l:suffix   = ""
    let l:maxlines = line("$")
    while l:count < g:localVariablesRange
        let l:line = getline(l:maxlines - l:count)
        if !l:lvend
            let l:locVarEndPos = match(l:line, "\\V\\C".g:localVariablesEndText)
            if l:locVarEndPos >= 0
                let l:prefix = strpart(l:line, 0, l:locVarEndPos)
                let l:suffix = strpart(l:line, l:locVarEndPos + strlen(g:localVariablesEndText))
                let l:lvend  = 1
            endif
        elseif l:line ==# l:prefix.g:localVariablesBegText.l:suffix
            let l:found = 1
            break
        endif
        let l:count = l:count + 1
    endwh
    if l:found
        let l:line = getline(l:maxlines - l:count)
        while l:count > 0
            let l:count = l:count - 1
            let l:line = getline(l:maxlines - l:count)
            if l:line ==# l:prefix.g:localVariablesEndText.l:suffix
                break
            else
                call <SID>LocalVariablesSet(l:line, l:prefix, l:suffix)
            endif
        endwh
    endif
    let b:localVariablesChecked = 1
endfun

fun! LocalVariablesCheck()
    if !exists("b:localVariablesChecked")
        call LocalVariablesReCheck()
    endif
endfun

