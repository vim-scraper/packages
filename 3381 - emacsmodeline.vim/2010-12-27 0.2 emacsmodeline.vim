" -*- mode: vim; tab-width: 4; indent-tabs-mode: nil; fill-column: 99 -*-
"
" emacsmodeline.vim
" Brief: Parse emacs mode line and setlocal in vim
" Version: 0.1
" Date: May.18, 2007
" Maintainer: Yuxuan 'fishy' Wang <fishywang@gmail.com>
"
" Installation: put this file under your ~/.vim/plugins/
"
" Usage:
"
" This script is used to parse emacs mode line, for example:
" -*- tab-width: 4 -*-
"
" Which is the same meaning as:
" vim:tabstop=4:
"
" Revisions:
"
" 0.1, May.18, 2007:
"  * initial version with tab-width support
"

" No attempt is made to support vim versions before 7.0.
if version < 700
    finish
endif

function! <SID>FindParameterValue(modeline, emacs_name, value)
    let pattern = '\c' . '\(^\|.*;\)\s*' . a:emacs_name . ':\s*\(' . a:value . '\)\s*\($\|;.*\)'
    if a:modeline =~ pattern
        return substitute(a:modeline, pattern, '\2', '')
    endif
    return ''
endfunc

function! <SID>SetVimStringOption(modeline, emacs_name, vim_name)
    let value = <SID>FindParameterValue(a:modeline, a:emacs_name, '\S\+')
    if strlen(value)
        " Normalize 'C++' into 'cpp'.
        let value = substitute(value, '+', 'p', 'g')
        let value = tolower(value)
        exec 'setlocal ' . a:vim_name . '=' . value
    endif
endfunc

function! <SID>SetVimNumberOption(modeline, emacs_name, vim_name)
    let value = <SID>FindParameterValue(a:modeline, a:emacs_name, '\d\+')
    if strlen(value)
        exec 'setlocal ' . a:vim_name . '=' . value
    endif
endfunc

function! <SID>SetVimToggleOption(modeline, emacs_name, vim_name, nil_value)
    let value = <SID>FindParameterValue(a:modeline, a:emacs_name, '\S\+')
    if strlen(value)
        if (value == 'nil') == a:nil_value
            exec 'setlocal ' . a:vim_name
        else
            exec 'setlocal no' . a:vim_name
        end
    endif
endfunc

function! ParseEmacsModeLine()
    " If &modeline is false, then don't try to detect modelines.
    if ! &modeline
        return
    endif

    " Prepare to scan the first and last &modelines lines.
    let max = line("$")
    if max > (&modelines * 2)
        let lines = range(1, &modelines) + range(max - &modelines + 1, max)
    else
        let lines = range(1, max)
    endif

    let pattern = '.*-\*-\(.*\)-\*-.*'
    for n in lines
        let line = getline(n)
        if line =~ pattern
            let modeline = substitute(line, pattern, '\1', '')

            call <SID>SetVimStringOption(modeline, 'mode',               'filetype')

            call <SID>SetVimNumberOption(modeline, 'c-basic-offset',     'shiftwidth')
            call <SID>SetVimNumberOption(modeline, 'fill-column',        'textwidth')
            call <SID>SetVimNumberOption(modeline, 'tab-width',          'softtabstop')
            call <SID>SetVimNumberOption(modeline, 'tab-width',          'tabstop')

            call <SID>SetVimToggleOption(modeline, 'buffer-read-only',   'readonly',     0)
            call <SID>SetVimToggleOption(modeline, 'indent-tabs-mode',   'expandtab',    1)

            " Other emacs options seen in the wild include:
            "  * c-file-style: no vim equivalent (?).
            "  * coding: text encoding.  Non-UTF-8 files are evil.
            "  * compile-command: probably equivalent to &makeprg.  However, vim will refuse to
            "    set it from a modeline, as a security risk, and we follow that decision here.
            "  * mmm-classes: appears to be for other languages inline in HTML, e.g. PHP.
            "  * package: equal to mode, in the one place I've seen it.
            "  * syntax: equal to mode, in the one place I've seen it.
        endif
    endfor
endfunc

autocmd BufReadPost * :call ParseEmacsModeLine()
