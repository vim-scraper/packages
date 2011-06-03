" tmru.vim -- Most Recently Used Files
" @Author:      Thomas Link (mailto:micathom AT gmail com?subject=vim-tlib-mru)
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-04-13.
" @Last Change: 2007-09-11.
" @Revision:    0.4.185
" GetLatestVimScripts: 1864 1 tmru.vim

if &cp || exists("loaded_tmru")
    finish
endif
if !exists('loaded_tlib') || loaded_tlib < 13
    echoerr "tlib >= 0.13 is required"
    finish
endif
let loaded_tmru = 4

if !exists("g:tmruSize")     | let g:tmruSize = 50           | endif "{{{2
if !exists("g:tmruMenu")     | let g:tmruMenu = 'File.M&RU.' | endif "{{{2
if !exists("g:tmruMenuSize") | let g:tmruMenuSize = 20       | endif "{{{2
if !exists("g:TMRU")         | let g:TMRU = ''               | endif "{{{2

if !exists("g:tmruExclude") "{{{2
    let g:tmruExclude = '/te\?mp/\|vim.\{-}/\(doc\|cache\)/\|'.
                \ substitute(escape(&suffixes, '~.*$^'), ',', '$\\|', 'g') .'$'
endif

if !exists("g:tmru_ignorecase") "{{{2
    let g:tmru_ignorecase = !has('fname_case')
endif

if !exists('g:tmru_world') "{{{2
    let g:tmru_world = tlib#World#New({
                \ 'type': 'm',
                \ 'key_handlers': [
                \ {'key': 3,  'agent': 'tlib#agent#CopyItems',        'key_name': '<c-c>', 'help': 'Copy file name(s)'},
                \ {'key': 9,  'agent': 'tlib#agent#ShowInfo',         'key_name': '<c-i>', 'help': 'Show info'},
                \ {'key': 19, 'agent': 'tlib#agent#EditFileInSplit',  'key_name': '<c-s>', 'help': 'Edit files (split)'},
                \ {'key': 22, 'agent': 'tlib#agent#EditFileInVSplit', 'key_name': '<c-v>', 'help': 'Edit files (vertical split)'},
                \ {'key': 20, 'agent': 'tlib#agent#EditFileInTab',    'key_name': '<c-t>', 'help': 'Edit files (new tab)'},
                \ {'key': 23, 'agent': 'tlib#agent#ViewFile',         'key_name': '<c-w>', 'help': 'View file in window'},
                \ ],
                \ 'allow_suspend': 0,
                \ 'query': 'Select file',
                \ })
    call g:tmru_world.Set_display_format('filename')
endif


function! s:BuildMenu(initial) "{{{3
    if !empty(g:tmruMenu)
        if !a:initial
            silent! exec 'aunmenu '. g:tmruMenu
        endif
        let es = s:MruRetrieve()
        if g:tmruMenuSize > 0 && len(es) > g:tmruMenuSize
            let es = es[0 : g:tmruMenuSize - 1]
        endif
        for e in es
            let me = escape(e, '.\ ')
            exec 'amenu '. g:tmruMenu . me .' :call <SID>Edit('. string(e) .')<cr>'
        endfor
    endif
endf

function! s:MruRetrieve()
    return split(g:TMRU, '\n')
endf

function! s:MruStore(mru)
    let g:TMRU = join(a:mru, "\n")
    call s:BuildMenu(0)
endf

function! s:MruRegister(fname)
    " TLogVAR a:fname
    if g:tmruExclude != '' && a:fname =~ g:tmruExclude
        return
    endif
    let tmru = s:MruRetrieve()
    let imru = index(tmru, a:fname, 0, g:tmru_ignorecase)
    if imru == -1 && len(tmru) >= g:tmruSize
        let imru = g:tmruSize - 1
    endif
    if imru != -1
        call remove(tmru, imru)
    endif
    call insert(tmru, a:fname)
    call s:MruStore(tmru)
endf

function! s:SNR()
    return matchstr(expand('<sfile>'), '<SNR>\d\+_\zeSNR$')
endf

function! s:Edit(filename) "{{{3
    if a:filename == expand('%:p')
        return 1
    else
        let bn = bufnr(a:filename)
        " TLogVAR bn
        if bn != -1 && buflisted(bn)
            exec 'buffer '. bn
            return 1
        elseif filereadable(a:filename)
            exec 'edit '. escape(a:filename, '%#\ ')
            return 1
        endif
    endif
    return 0
endf

function! s:SelectMRU()
    let tmru  = s:MruRetrieve()
    let world = copy(g:tmru_world)
    let world.base = copy(tmru)
    " let bs    = tlib#input#List('m', 'Select file', copy(tmru), g:tmru_handlers)
    let bs    = tlib#input#ListW(world)
    " TLogVAR bs
    if !empty(bs)
        for bf in bs
            " TLogVAR bf
            if !s:Edit(bf)
                let bi = index(tmru, bf)
                " TLogVAR bi
                call remove(tmru, bi)
                call s:MruStore(tmru)
            endif
        endfor
        return 1
    endif
    return 0
endf

function! s:EditMRU()
    let tmru = s:MruRetrieve()
    let tmru = tlib#input#EditList('Edit MRU', tmru)
    call s:MruStore(tmru)
endf

function! s:AutoMRU(filename) "{{{3
    " if &buftype !~ 'nofile' && fnamemodify(a:filename, ":t") != '' && filereadable(fnamemodify(a:filename, ":t"))
    if &buftype !~ 'nofile' && fnamemodify(a:filename, ":t") != ''
        call s:MruRegister(a:filename)
    endif
endf


augroup tmru
    au!
    au VimEnter * call s:BuildMenu(1)
    au BufWritePost,BufReadPost * call s:AutoMRU(expand("%:p"))
augroup END

command! TRecentlyUsedFiles call s:SelectMRU()
command! TRecentlyUsedFilesEdit call s:EditMRU()


finish

This plugin provides a simple most recently files facility.

It was originally rather a by-product of tlib (vimscript #1863) and uses 
its tlib#input#List() function. This function allows quickly selecting a 
buffer by typing some part of the name (which will actually filter the 
list until only one item is left), the number, or by clicking with the 
mouse on the entry.

:TRecentlyUsedFiles ... open one or more recently used file(s)

This plugin relies on the 'viminfo' option to contain ! in order 
to save the files list.


CHANGES:
0.1
Initial release

0.2
- :TRecentlyUsedFilesEdit
- Don't register nofile buffers or buffers with no filename.
- <c-c> copy file name(s) (to @*)
- When !has('fname_case'), ignore case when checking if a filename is 
already registered.

0.3
- Autocmds use expand('%') instead of expand('<afile>')
- Build menu (if the prefix g:tmruMenu isn't empty)
- Key shortcuts to open files in (vertically) split windows or tabs
- Require tlib >= 0.9

0.4
- <c-w> ... View file in original window
- <c-i> ... Show file info
- Require tlib >= 0.13

