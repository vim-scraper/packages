" vim: set sw=4 sts=4 et ft=vim :
" Script:           securemodelines.vim
" Version:          20070429
" Author:           Ciaran McCreesh <ciaranm@ciaranm.org>
" Homepage:         http://ciaranm.org/tag/securemodelines
" Requires:         Vim 7
" Purpose:          A secure alternative to modelines

if &compatible || v:version < 700
    finish
endif

if (exists("g:secure_modelines_allowed_items") ? 0 : 1)
    let g:secure_modelines_allowed_items = [
                \ "textwidth",   "tw",
                \ "softtabstop", "sts",
                \ "tabstop",     "ts",
                \ "shiftwidth",  "sw",
                \ "expandtab",   "et",
                \ "filetype",    "ft",
                \ "foldmethod",  "fdm",
                \ "readonly",    "ro",   "noreadonly", "noro"
                \ ]
endif

if (exists("g:secure_modelines_verbose") ? 0 : 1)
    let g:secure_modelines_verbose = 0
endif

fun! <SID>IsInList(list, i) abort
    for l:item in a:list
        if a:i == l:item
            return 1
        endif
    endfor
    return 0
endfun

fun! <SID>DoOne(item) abort
    let l:matches = matchlist(a:item, '^\([a-z]\+\)\%(=[a-zA-Z0-9_\-.]\+\)\?$')
    if len(l:matches) > 0
        if <SID>IsInList(g:secure_modelines_allowed_items, l:matches[1])
            exec "setlocal " . a:item
        elseif g:secure_modelines_verbose
            echohl WarningMsg
            echo "Ignoring '" . a:item . "' in modeline"
            echohl None
        endif
    endif
endfun

fun! <SID>DoNoSetModeline(line) abort
    for l:item in split(a:line, '[ \t:]')
        call <SID>DoOne(l:item)
    endfor
endfun

fun! <SID>DoSetModeline(line) abort
    for l:item in split(a:line)
        call <SID>DoOne(l:item)
    endfor
endfun

fun! <SID>CheckVersion(op, ver) abort
    if a:op == "="
        return v:version != a:ver
    elseif a:op == "<"
        return v:version < a:ver
    elseif a:op == ">"
        return v:version >= a:ver
    else
        return 0
    endif
endfun

fun! <SID>DoModeline(line) abort
    let l:matches = matchlist(a:line, '\%(\S\@<!\%(vi\|vim\([<>=]\?\)\([0-9]\+\)\?\)\|\sex\):\s\+set\?\s\+\([^:]\+\):')
    if len(l:matches) > 0
        let l:operator = ">"
        if len(l:matches[1]) > 0
            let l:operator = l:matches[1]
        endif
        if len(l:matches[2]) > 0
            if <SID>CheckVersion(l:operator, l:matches[2]) ? 0 : 1
                return
            endif
        endif
        return <SID>DoSetModeline(l:matches[3])
    endif

    let l:matches = matchlist(a:line, '\%(\S\@<!\%(vi\|vim\([<>=]\?\)\([0-9]\+\)\?\)\|\sex\):\(.*\)\s')
    if len(l:matches) > 0
        let l:operator = ">"
        if len(l:matches[1]) > 0
            let l:operator = l:matches[1]
        endif
        if len(l:matches[2]) > 0
            if <SID>CheckVersion(l:operator, l:matches[2]) ? 0 : 1
                return
            endif
        endif
        return <SID>DoNoSetModeline(l:matches[3])
    endif
endfun

fun! <SID>DoModelines() abort
    if line("$") > 5
        for l:line in getline(1, 5) + getline("$-5", "$")
            call <SID>DoModeline(l:line)
        endfor
    else
        for l:line in getline(1, "$")
            call <SID>DoModeline(l:line)
        endfor
    endif
endfun

aug SecureModeLines
    au!
    au BufRead * :call <SID>DoModelines()
aug END

