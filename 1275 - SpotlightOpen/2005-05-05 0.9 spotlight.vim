" Name:          spotlight.vim (global plugin)
" Version:       1.0
" Author:        Ruediger Hanke <tj1249 at talesofbeauty.de> (spotlight adaption)
"                based on the work of
"                Ciaran McCreesh <ciaranm at gentoo.org> (original locateopen.vim)
" Purpose:       Open a file for editing without knowing the file's path using Spotlight
"                This is an adaption of Ciaran McCreesh's locateopen.vim script.
"
" License:       You may redistribute this plugin under the same terms as Vim
"                itself.
"
" Usage:         :SpotEdit somefile.txt           " find and edit
"                :SpotSplit somefile.txt          " find and split
"                :SpotRead somefile.txt           " find and read
"                :SpotSource somefile.vim         " find and source
"
" Configuration:
"                You can restrict the files found to a certain type.
"                E.g., to consider only plain text files:
"                :let g:spotlightopen_limitftype = "public.plain-text"
"
"                You can choose whether the exact filename or part of
"                the filename must be typed:
"                :let g:spotlightopen_match = n
"                   n = 0: Exact matches only
"                   n = 1: Match beginning
"                   n = 2: Match anywhere in filename
"
" Requirements:  Requires MacOS 10.4 or later

if exists("loaded_spotlightPlugin")
    finish
endif
let loaded_spotlightPlugin = 1


let s:mdfind_app            = "mdfind"

if !exists('g:spotlightopen_match')
    let g:spotlightopen_match = 0
endif
if !exists('g:spotlightopen_limitftype')
    let g:spotlightopen_limitftype = ""
endif

function! s:EscapeSpaces(str)
    return substitute(a:str, "\\( \\)", "\\\\ ", "g")
endfun


" Find file, and if there are several then ask the user which one is
" intended.
function! s:LocateFile(file, searchdir)
    let l:command = s:mdfind_app . " "
    if (a:searchdir != "")
        let l:command = l:command . "-onlyin " . " \"" . a:searchdir . "\" "
    endif

    " Now build the query
    let l:startmatch = ""
    let l:endmatch = ""
    if g:spotlightopen_match == 1
        let l:endmatch = "*"
    endif
    if g:spotlightopen_match == 2
        let l:startmatch = "*"
        let l:endmatch = "*"
    endif

    let l:query = "\"(kMDItemDisplayName == '" . l:startmatch . a:file . l:endmatch . "')"
    if g:spotlightopen_limitftype != ""
        let l:query = l:query . " && (kMDItemContentTypeTree == '*" . g:spotlightopen_limitftype . "*')"
    endif
    let l:query = l:query . "\""
    let l:command = l:command . l:query
    let l:options = system(l:command)

    " Do we have an error?
    if l:options =~ " " . s:mdfind_app . ": "
        throw "SpotlightOpenError: mdfind could not be executed - you need MacOS Tiger or later to use this script"
    endif

    " Do we have no files?
    if l:options == ""
        throw "SpotlightOpenError: No file found"
    endif

    " We have one or more files
    let l:options_copy = l:options
    let l:i = stridx(l:options, "\n")
    let l:x = 0
    while l:i > -1
        let l:option=strpart(l:options, 0, l:i)
        let l:options=strpart(l:options, l:i+1)
        let l:i = stridx(l:options, "\n")
        let l:x = l:x + 1
        echo l:x . ": " . l:option
    endwhile
    let l:options = l:options_copy
    if (l:x > 1)
        let l:which=input("Which file? ")
        let l:y = 1
        while l:y <= l:x
            if l:y == l:which
                return strpart(l:options, 0, stridx(l:options, "\n"))
            else
                let l:options=strpart(l:options, stridx(l:options, "\n") + 1)
                let l:y = l:y + 1
            endif
        endwhile
        throw "SpotlightOpenError: Invalid choice"
    else
        return strpart(l:options_copy, 0, stridx(l:options_copy, "\n"))
    endif
endfun

" Find a file and run :cmd file
function! s:SpotlightRun(cmd, file)
    try
        let l:options = s:LocateFile(a:file, "")
        exec a:cmd . ' ' . s:EscapeSpaces(l:options)
    catch /^SpotlightOpenError: /
        echo " "
        echoerr "Error: " . substitute(v:exception, "^SpotlightOpenError: ",
            \ "", "")
    endtry
endfun

" Find a file and :edit it
function! SpotlightEdit(file)
    call s:SpotlightRun('edit', a:file)
endfun

" Find a file and :split it
function! SpotlightSplit(file)
    call s:SpotlightRun('split', a:file)
endfun

" Find a file and :source it
function! SpotlightSource(file)
    call s:SpotlightRun('source', a:file)
endfun

" Find a file and :read it
function! SpotlightRead(file)
    call s:SpotlightRun('read', a:file)
endfun

" Do magicky export things
command! -nargs=1 SpotEdit   :call SpotlightEdit(<q-args>)
command! -nargs=1 SpotSplit  :call SpotlightSplit(<q-args>)
command! -nargs=1 SpotSource :call SpotlightSource(<q-args>)
command! -nargs=1 SpotRead   :call SpotlightRead(<q-args>)

" vim: set tw=80 ts=4 et :
