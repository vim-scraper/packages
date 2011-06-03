" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/presets.txt	[[[1
107
*presets.txt*     Presets -- Quickly switch between vim configurations
                  Author: Tom Link, micathom at gmail com


This plugins allows users to push presets (sets of |options|) onto a 
configuration stack and remove them later on, i.e., it allows users to 
quickly switch between pre-defined configurations.

In order to push a preset, use the |:Preset| command:

    :Preset! NAME


-----------------------------------------------------------------------
Install~

Edit the vba file and type: >

    :so %

See :help vimball for details.


Post-Install~

Check the values of the following variables and set them in your |vimrc| 
file if necessary:

    - |g:presets#font|
    - |g:presets#font_sizes|


========================================================================
Contents~

        g:presets#font ........... |g:presets#font|
        presets#font_base_size ... |presets#font_base_size|
        g:presets#font_sizes ..... |g:presets#font_sizes|
        g:presets#sets ........... |g:presets#sets|
        presets#Restore .......... |presets#Restore()|
        presets#Maximize ......... |presets#Maximize()|
        presets#Push ............. |presets#Push()|
        presets#Pop .............. |presets#Pop()|


========================================================================
autoload/presets.vim~

                                                    *g:presets#font*
g:presets#font                 (default: ...)
    You might want to change this variable according to your 
    preferences.

                                                    *presets#font_base_size*
presets#font_base_size         (default: 10)
    You might want to change this variable according to your 
    preferences.

                                                    *g:presets#font_sizes*
g:presets#font_sizes           (default: {...})
    Relative font sizes based on the value of 
    |presets#font_base_size|.

                                                    *g:presets#sets*
g:presets#sets                 (default: {})
    A dictionary with the keys:
    
      include ... A list of other presets
      global  ... A dictionary of global options
      window  ... A dictionary of window-local options
      buffer  ... A dictionary of buffer-local options
    
    The option dictionaries keys usually are options names than should 
    be set to its value. If the key starts with a colon (:) though, 
    the value is an ex command.
    
    If the key defines a ex command, the value can be either a string 
    or a list of two strings, where the second string is a command 
    that undoes the effect of the first string when |:execute|d. If 
    the value of a string begins with asterisk "*", the string is 
    evaluated first via |eval()| -- the result of the evaluation 
    should be the actual ex command.
    
    Any keys can be prepended with a number that defines its priority.

                                                    *presets#Restore()*
presets#Restore(lines, columns, x, y)

                                                    *presets#Maximize()*
presets#Maximize(fullscreen)
    Maximize the window.
    You might need to redefine it if it doesn't work for you.

                                                    *presets#Push()*
presets#Push(names)
    Push the preset NAME onto the configuration stack.
    
    The following special NAMEs are supported:
      show ... list the names of the presets on the configuration stack

                                                    *presets#Pop()*
presets#Pop()
    Pop the last preset from the configuration stack.



vim:tw=78:fo=tcq2:isk=!-~,^*,^|,^":ts=8:ft=help:norl:
autoload/presets.vim	[[[1
338
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @GIT:         http://github.com/tomtom/vimtlib/
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-04-24.
" @Last Change: 2010-07-19.
" @Revision:    202


let s:config_stack = []


if !exists('g:presets#font')
    " You might want to change this variable according to your 
    " preferences.
    " :read: let g:presets#font = ... "{{{2
    if has("gui_gtk2")
        let g:presets#font = 'monospace %s'
    elseif has("x11")
        let g:presets#font = '*-lucidatypewriter-medium-r-normal-*-%s-180-*-*-m-*-*'
    elseif has("gui_win32")
        let g:presets#font = 'Lucida_Sans_Typewriter:h%s:cANSI'
    endif
endif


if !exists('presets#font_base_size')
    " You might want to change this variable according to your 
    " preferences.
    let presets#font_base_size = 10   "{{{2
endif


if !exists('g:presets#font_sizes')
    " Relative font sizes based on the value of 
    " |presets#font_base_size|.
    " :read: let g:presets#font_sizes = {...} "{{{2
    let g:presets#font_sizes = {
                \ 'tiny': presets#font_base_size - 2,
                \ 'small': presets#font_base_size - 1,
                \ 'normal': presets#font_base_size,
                \ 'Normal': presets#font_base_size + 1,
                \ 'big': presets#font_base_size + 2,
                \ 'large': presets#font_base_size + 4,
                \ 'Large': presets#font_base_size + 6,
                \ }
endif


if !exists('g:presets#sets')
    " A dictionary with the keys:
    "
    "   include ... A list of other presets
    "   global  ... A dictionary of global options
    "   window  ... A dictionary of window-local options
    "   buffer  ... A dictionary of buffer-local options
    "
    " The option dictionaries keys usually are options names than should 
    " be set to its value. If the key starts with a colon (:) though, 
    " the value is an ex command.
    "
    " If the key defines a ex command, the value can be either a string 
    " or a list of two strings, where the second string is a command 
    " that undoes the effect of the first string when |:execute|d. If 
    " the value of a string begins with asterisk "*", the string is 
    " evaluated first via |eval()| -- the result of the evaluation 
    " should be the actual ex command.
    "
    " Any keys can be prepended with a number that defines its priority.
    let g:presets#sets = {}   "{{{2

    for [s:name, s:size] in items(g:presets#font_sizes)
        let g:presets#sets[s:name] = {
                \ '10global': {
                \   '30guifont': printf(presets#font, s:size),
                \ },
                \}
    endfor
    unlet s:name s:size

    let g:presets#sets['ruler'] = {
                \ '10global': {
                \   '10laststatus': 0,
                \   '10ruler': 1,
                \ },
                \}

    let g:presets#sets['plain'] = {
                \ '10global': {
                \   '10guioptions': substitute(&guioptions, '\C[mrlRLbT]', '', 'g'),
                \ },
                \}
    
    let g:presets#sets['full'] = {
                \ '10global': {
                \   '30:maximize': ['call presets#Maximize(1)', '*printf("call presets#Restore(%d, %d, %d, %d)", &lines, &columns, getwinposx(), getwinposy())'],
                \ },
                \}


    let g:presets#sets['screen'] = {
                \ '00include': ['plain', 'full'],
                \ '10global': {
                \   '10foldcolumn': 1,
                \ },
                \ '20buffer': {
                \ },
                \ '30window': {
                \   'foldcolumn': 1,
                \ },
                \}

    let g:presets#sets['darkscreen'] = {
                \ '00include': ['plain'],
                \ '10global': {
                \   '10foldcolumn': 12,
                \   '10laststatus': 0,
                \   '10linespace': 8,
                \   '20:maximize': ['call presets#Maximize(1)', '*printf("call presets#Restore(%d, %d, %d, %d)", &lines, &columns, getwinposx(), getwinposy())'],
                \   '30guifont': printf(presets#font, g:presets#font_sizes.large),
                \ },
                \ '20buffer': {
                \ },
                \ '30window': {
                \   'foldcolumn': 12,
                \ },
                \}

endif


function! presets#Restore(lines, columns, x, y) "{{{3
    if exists('g:loaded_tlib') && g:loaded_tlib >= 39
        call tlib#vim#RestoreWindow()
    else
        let &lines = a:lines
        let &columns = a:columns
        exec 'winpos' a:x a:y
    endif
endf


if !exists('*presets#Maximize')

    if exists('g:loaded_tlib') && g:loaded_tlib >= 39

        " Maximize the window.
        " You might need to redefine it if it doesn't work for you.
        fun! presets#Maximize(fullscreen) "{{{3
            call tlib#vim#Maximize(a:fullscreen)
        endf

    else

        if has('win16') || has('win32') || has('win64')

            " :nodoc:
            fun! presets#Maximize(fullscreen) "{{{3
                simalt ~x
            endf

        else

            " :nodoc:
            fun! presets#Maximize(fullscreen) "{{{3
                set lines=1000 columns=1000 
            endf

        endif

    endif
endif


let s:special_names = ['show']


" :nodoc:
function! presets#Complete(ArgLead, CmdLine, CursorPos) "{{{3
    let sets = sort(keys(g:presets#sets), 1) + s:special_names
    if !empty(a:ArgLead)
        let slen = len(a:ArgLead)
        call filter(sets, 'strpart(v:val, 0, slen) ==# a:ArgLead')
    endif
    return sets
endf


" Push the preset NAME onto the configuration stack.
"
" The following special NAMEs are supported:
"   show ... list the names of the presets on the configuration stack
function! presets#Push(names) "{{{3
    if a:names == 'show'
        let names = reverse(map(copy(s:config_stack), 'v:val._name'))
        echom "Presets stack:" join(names, ', ')
    else
        for name0 in split(a:names, '\s\+')
            let names = presets#Complete(name0, '', 0)
            " TLogVAR name0, names
            " Exact match
            if index(names, name0) != -1
                let names = [name0]
                " TLogVAR names
            endif
            if len(names) != 1
                echoerr 'Presets: Ambivalent or unknown name: '. name0 .' ('. join(keys(g:presets#sets), ', ') .')'
            else
                let name = names[0]
                " TLogVAR name, names
                if has_key(g:presets#sets, name)
                    let set = g:presets#sets[name]
                    let previous = {'_name': name}
                    call s:Set(set, previous, 0)
                    call add(s:config_stack, previous)
                    " redraw
                else
                    echoerr 'Presets: Unknown set: '. name
                endif
            endif
        endfor
    endif
endf


" Pop the last preset from the configuration stack.
function! presets#Pop() "{{{3
    if !empty(s:config_stack)
        let previous = remove(s:config_stack, -1)
        " TLogVAR previous
        call s:Set(previous, {}, 1)
    endif
endf


function! s:OptName(name) "{{{3
    return substitute(a:name, '^\d\+', '', '')
endf


function! s:Items(dict, descending_order) "{{{3
    let items = sort(items(a:dict))
    " if a:descending_order
    "     call reverse(items) 
    " endif
    return items
endf


function! s:Set(preset, save, descending_order) "{{{3
    for [type0, config] in s:Items(a:preset, a:descending_order)
        let type = s:OptName(type0)
        if type == 'include'
            for name in config
                let preset = g:presets#sets[name]
                call s:Set(preset, a:save, a:descending_order)
            endfor
        elseif type[0] == '_'
        else
            if !has_key(a:save, type)
                let a:save[type] = {}
            endif
            call s:Set_{type}(config, a:save[type], a:descending_order)
        endif
        unlet config
    endfor
endf


function! s:Value(string) "{{{3
    if a:string[0] == '*'
        let val = eval(strpart(a:string, 1))
        " TLogVAR val
    else
        let val = a:string
    endif
    return val
endf


function! s:DoValue(save_config, opt, val) "{{{3
    if type(a:val) == 3
        " TLogVAR a:val
        if len(a:val) >= 2
            let a:save_config[a:opt] = s:Value(a:val[1])
        endif
        return s:Value(a:val[0])
    else
        return s:Value(a:val)
    endif
endf


function! s:Set_global(config, save_config, descending_order) "{{{3
    for [opt0, val] in s:Items(a:config, a:descending_order)
        let opt = s:OptName(opt0)
        " TLogVAR opt, val
        if opt[0] == ':'
            exec s:DoValue(a:save_config, opt, val)
        else
            exec 'let a:save_config[opt0] = &g:'. opt
            exec 'let &g:'. opt .' = val'
        endif
        unlet val
    endfor
endf


function! s:Set_window(config, save_config, descending_order) "{{{3
    let winnr = winnr()
    for [opt0, val] in s:Items(a:config, a:descending_order)
        let opt = s:OptName(opt0)
        if opt[0] == ':'
            exec 'windo '. s:DoValue(a:save_config, opt, val)
        else
            exec 'let a:save_config[opt0] = &l:'. opt
            exec 'windo let &l:'. opt .' = val'
        endif
    endfor
    exec winnr .'wincmd w'
endf


function! s:Set_buffer(config, save_config, descending_order) "{{{3
    let bufnr = bufnr('%')
    for [opt0, val] in s:Items(a:config, a:descending_order)
        let opt = s:OptName(opt0)
        if opt[0] == ':'
            exec 'bufdo '. s:DoValue(a:save_config, opt, val)
        else
            exec 'let a:save_config[opt0] = &l:'. opt
            exec 'bufdo let &l:'. opt .' = val'
        endif
    endfor
    exec bufnr .'buffer'
endf


plugin/presets.vim	[[[1
47
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @GIT:         http://github.com/tomtom/vimtlib/
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-04-24.
" @Last Change: 2010-05-15.
" @Revision:    23
" GetLatestVimScripts: 0 0 :AutoInstall: presets.vim
" Quickly switch between vim configurations


if &cp || exists("loaded_presets")
    finish
endif
let loaded_presets = 1

let s:save_cpo = &cpo
set cpo&vim

" :display: Preset[!] [PRESET]
" Push the configuration of PRESET.
"
" With [!], pop the previous preset from the configuration stack before 
" pushing PRESET.
"
" If no PRESET argument is given, pop the previous preset from the 
" configuration stack.
"
" This command supports certain special names -- see |presets#Push()| 
" for details.
command! -bang -nargs=? -complete=customlist,presets#Complete Preset
            \ if empty(<q-args>) |
            \   call presets#Pop() |
            \ else |
            \   if !empty("<bang>") | call presets#Pop() | endif |
            \   call presets#Push(<q-args>) |
            \ endif


let &cpo = s:save_cpo
unlet s:save_cpo
finish

CHANGES:
0.1
- Initial release

