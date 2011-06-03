" tselectfile.vim -- A simplicistic files selector/browser (sort of)
" @Author:      Thomas Link (mailto:samul AT web de?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-04-16.
" @Last Change: 2007-04-18.
" @Revision:    0.1.189

if &cp || exists("loaded_tselectfile")
    finish
endif
if !exists('loaded_tlib')
    echoerr "tlib is required"
    finish
endif
let loaded_tselectfile = 1

fun! s:SNR()
    return matchstr(expand('<sfile>'), '<SNR>\d\+_\zeSNR$')
endf

fun! s:SuffixesRx()
    return printf('\(%s\)\$', join(map(split(&suffixes, ','), 'v:val'), '\|'))
endf

if !exists('g:tselectfile_handlers')
    let g:tselectfile_handlers = [
                \ {'key':  4, 'agent': s:SNR() .'AgentDeleteFile', 'key_name': 'CTRL-D', 'help': 'Delete file(s)'},
                \ {'key': 15, 'agent': s:SNR() .'AgentOpenDir', 'key_name': 'CTRL-O', 'help': 'Open dir'},
                \ {'key': 21, 'agent': s:SNR() .'AgentRenameFile', 'key_name': 'CTRL-U', 'help': 'Rename file(s)'},
                \ {'key': 16, 'agent': s:SNR() .'AgentPreviewFile', 'key_name': 'CTRL-P', 'help': 'Preview file'},
                \ {'key':  2, 'agent': s:SNR() .'AgentBatchRenameFile', 'key_name': 'CTRL-B', 'help': 'Batch rename file(s)'},
                \ {'key':  9, 'agent': s:SNR() .'AgentSelectBackups', 'key_name': 'CTRL-I', 'help': 'Select backup(s)'},
                \ {'key': 24, 'agent': s:SNR() .'AgentHide', 'key_name': 'CTRL-X', 'help': 'Hide some files'},
                \ {'display_format': s:SNR() .'DisplayFormat(%s)'},
                \ {'pick_last_item': 0},
                \ ]
endif

if !exists('g:tselectfile_hidden')
    let g:tselectfile_hidden_rx = '\V\(/.\|/CVS/\|/.attic\|.svn/\|'. s:SuffixesRx() .'\)'
endif

if !exists('g:tselectfile_favourites')
    if has('win16') || has('win32') || has('win64')
        let g:tselectfile_favourites = ['c:/', 'd:/']
    else
        let g:tselectfile_favourites = []
    endif
    if !empty($HOME)
        call add(g:tselectfile_favourites, $HOME)
    endif
    if !empty($USERPROFILE)
        call add(g:tselectfile_favourites, $USERPROFILE)
        " call add(g:tselectfile_favourites, $USERPROFILE .'/desktop/')
    endif
endif

fun! s:PrepareSelectFiles(hide)
    if s:select_files_pattern == '**'
        let ls = split(globpath(s:select_files_filter, s:select_files_pattern), '\n')
        " TLogVAR ls
        let rv = sort(filter(ls, '!isdirectory(v:val)'))
    else
        let filter = s:select_files_filter . s:select_files_pattern
        " TLogVAR filter
        let rv = sort(map(split(glob(filter), '\n'), 'isdirectory(v:val) ? v:val."/" : v:val'))
        let rv += g:tselectfile_favourites
    endif
    if a:hide
        call filter(rv, 'v:val !~ g:tselectfile_hidden_rx')
    endif
    if s:select_files_pattern != '**'
        let ph = fnamemodify(s:select_files_filter, ':h')
        " TLogVAR ph
        " call TLogDBG(s:select_files_filter)
        if ph != s:select_files_filter
            if ph[-1] !~ '[\/]'
                let ph .= '/'
            endif
            call insert(rv, ph .'../')
        endif
    endif
    return rv
endf

fun! s:DisplayFormat(file)
    let fname = fnamemodify(a:file, ":t")
    if isdirectory(a:file)
        let fname .='/'
    endif
    let dname = fnamemodify(a:file, ":h")
    let dnmax = &co - max([20, len(fname)]) - 12 - &fdc
    if len(dname) > dnmax
        let dname = '...'. strpart(fnamemodify(a:file, ":h"), len(dname) - dnmax)
    endif
    return printf("%-20s   %s", fname, dname)
endf

fun! s:AgentPostprocess(world, result)
    let item = resolve(a:result)
    " TLogVAR item
    " TLogDBG len(a:world.list)
    if isdirectory(item)
        let s:select_files_filter = fnamemodify(item, ':p')
        return [s:AgentResetInputList(a:world), '']
    endif
    return [a:world, a:result]
endf

fun! s:AgentOpenDir(world, selected)
    let dir = input('DIR: ', '', 'dir')
    if dir != ''
        let s:select_files_filter = fnamemodify(dir, ':p')
        return s:AgentResetInputList(a:world)
    endif
    return a:world
endf

fun! s:DeleteFile(file)
    let doit = input('Really delete file "'. a:file .'"? (y/N) ', s:delete_this_file_default)
    if doit ==? 'y'
        if doit ==# 'Y'
            let s:delete_this_file_default = 'y'
        endif
        call delete(a:file)
        echom 'Delete file: '. a:file
    endif
endf

fun! s:AgentDeleteFile(world, selected)
    let s:delete_this_file_default = ''
    for file in a:selected
        call s:DeleteFile(file)
    endfor
    return s:AgentResetInputList(a:world)
endf

fun! s:AgentPreviewFile(world, selected)
    let file = a:selected[0]
    if !exists('b:tselectfiles_previewedfile') || file != b:tselectfiles_previewedfile
        exec 'pedit '. escape(file, '%#\ ')
        let b:tselectfiles_previewedfile = file
        let a:world.state = 'redisplay'
    else
        pclose
        let a:world.state = 'display'
    endif
    return a:world
endf

fun! s:RenameFile(file, name, confirm)
    if a:confirm
        echo
        echo 'From: '. a:file
        echo 'To:   '. a:name
        let ok = input('Rename now? (y/n) ', 'y')
        if ok != 'y'
            return
        endif
    endif
    if a:name != ''
        call rename(a:file, a:name)
        echom 'Rename file "'. a:file .'" -> "'. a:name
    endif
endf

fun! s:AgentRenameFile(world, selected)
    let s:rename_this_file_pattern = ''
    let s:rename_this_file_subst   = ''
    for file in a:selected
        let name = input('Rename "'. file .'" to: ', file)
        call s:RenameFile(file, name, 0)
    endfor
    return s:AgentResetInputList(a:world)
endf

fun! s:AgentBatchRenameFile(world, selected)
    let pattern = input('Rename pattern (whole path): ')
    if pattern != ''
        echo 'Pattern: '. pattern
        let subst = input('Rename substitution: ')
        if subst != ''
            for file in a:selected
                let name = substitute(file, pattern, subst, 'g')
                call s:RenameFile(file, name, 1)
            endfor
        endif
    endif
    return s:AgentResetInputList(a:world)
endf

fun! s:AgentSelectBackups(world, selected)
    let a:world.filter = s:SuffixesRx()
    let a:world['state'] = 'display'
    return a:world
endf

fun! s:AgentResetInputList(world)
    let a:world['state']  = 'reset'
    let a:world['base']   = s:PrepareSelectFiles(get(a:world, 'hide', 1))
    let a:world['picked'] = 0
    return a:world
endf

fun! s:AgentHide(world, selected)
    let hidden = get(a:world, 'hide', 1)
    let a:world.hide = hidden ? 0 : 1
    let a:world.state = 'reset'
    return s:AgentResetInputList(a:world)
endf

fun! TSelectFiles(bang, filter)
    let s:select_files_filter = fnamemodify(a:filter, ':p')
    let handlers = copy(g:tselectfile_handlers)
    if empty(a:bang)
        let s:select_files_pattern = '*'
        call add(handlers, {'postprocess': '', 'agent': s:SNR() .'AgentPostprocess'})
    else
        let s:select_files_pattern = '**'
    endif
    let fs = tlib#InputList('m', 'Select files', s:PrepareSelectFiles(1), handlers)
    if !empty(fs)
        for f in fs
            let bn = bufnr(f)
            if bn == -1 && filereadable(f)
                exec 'edit '. escape(f, '%#\ ')
            endif
        endfor
    endif
endf
command! -bang -nargs=? TSelectFiles call TSelectFiles("<bang>", <q-args>)


finish

This plugin provides a simple file browser. It is not a full blown 
explorer but can be nevertheless be useful for quickly selecting a few 
files or renaming them. It was originally rather a by-product of tlib 
(vimscript #1863).

:TSelectFiles  ... open/delete/rename files in the current directory
:TSelectFiles! ... recursively show all files in the current directory and
                   subdirectories (don't show favourites and ".."); don't 
                   use this command when you're at /.

Features:
    - open files
    - preview files
    - rename/move files
    - batch rename/move files (using a regular expression)
    - delete files

