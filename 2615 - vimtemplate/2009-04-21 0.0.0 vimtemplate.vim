" vim:foldmethod=marker:fen:
scriptencoding utf-8

" DOCUMENT {{{1
"==================================================
" Name: vimtemplate
" Version: 0.0.0
" Author:  tyru <tyru.exe+vim@gmail.com>
" Last Change: 2009-04-22.
"
" Change Log: {{{2
"   0.0.0: Initial upload.
" }}}2
"
" Usage:
"   COMMANDS:
"       VimTemplate
"           open template files list.
"
"   MAPPING:
"       gt
"           open template files list.
"
"   GLOBAL VARIABLES:
"       g:vt_template_dir_path (default:"$HOME/.vim/template")
"           search template files in this dir.
"           to specify multi-dirs, set paths joined with ",".
"
"       TODO g:vt_files_using_template (default:"")
"           template files name joined with ",".
"           search these files in your g:vt_template_dir_path.
"           e.g.: "java_template.java,cpp_template.cpp"
"
"       g:vt_support_command (default:1)
"           make command if this is true.
"
"       g:vt_command (default:"VimTemplate")
"           command name.
"
"       g:vt_support_mapping (default:1)
"           make mapping if this is true.
"
"       g:vt_mapping (default:"gt")
"           mapping.
"
"       g:vt_list_buf_height (default:7)
"           height of buffer.
"           buffer shows you list of template files.
"
"       g:vt_filetype_files (default: "")
"           when you load one of these files, exec :setlocal ft=<filetype>.
"           search these files in your g:vt_template_dir_path.
"           e.g.: "java_template.java=java,cpp_template.cpp=cpp"
"
"
"
"==================================================
" }}}1

" INCLUDE GUARD {{{1
if exists('g:loaded_vimtemplate') && g:loaded_vimtemplate != 0 | finish | endif
let g:loaded_vimtemplate = 1
" }}}1
" SAVING CPO {{{1
let s:save_cpo = &cpo
set cpo&vim
" }}}1

" SCOPED VARIABLES {{{1
let s:caller_winnr = -1
" }}}1
" GLOBAL VARIABLES {{{1
if !exists('g:vt_template_dir_path')
    let g:vt_template_dir_path = '$HOME/.vim/template'
endif
if !exists('g:vt_files_using_template')
    let g:vt_files_using_template = ''
endif
if !exists('g:vt_support_command')
    let g:vt_support_command = 1
endif
if !exists('g:vt_command')
    let g:vt_command = 'VimTemplate'
endif
if !exists('g:vt_support_mapping')
    let g:vt_support_mapping = 1
endif
if !exists('g:vt_mapping')
    let g:vt_mapping = 'gt'
endif
if !exists('g:vt_list_buf_height')
    let g:vt_list_buf_height = 7
endif
if !exists('g:vt_filetype_files')
    let g:vt_filetype_files = ''
endif
" }}}1

" FUNCTION DEFINITION {{{1
func! s:warn(msg)
    echohl WarningMsg
    echo a:msg
    echohl None
endfunc

func! s:glob_path_list(path, expr)
    let files = split(globpath(a:path, a:expr), '\n')
    call map(files, 'fnamemodify(v:val, ":t")')
    call filter(files, 'v:val !=# "." && v:val !=# ".."')
    call map(files, 'a:path . "/" . v:val')
    return files
endfunc

func! s:open_file()
    " get path of template file
    let path = getline('.')
    if path == '' | return | endif

    if !filereadable(path)
        call s:warn(printf("can't read '%s'", path))
        return
    endif


    if winnr('$') == 1
        new
        wincmd w
        quit
    else
        quit
        " switch to caller window
        execute s:caller_winnr . 'wincmd w'
    endif
    let s:caller_winnr = -1


    " paste template buffer to current buffer
    %delete _
    execute '0read ' . path

    " g:vt_filetype_files
    for pair in split(g:vt_filetype_files, ',')
        let [fname; filetype] = split(pair, '=')
        if empty(filetype) | continue | endif

        if get(split(path, '/'), -1, 123) ==# fname
            execute 'setlocal ft=' . filetype[0]
            break
        endif
    endfor
endfunc

func! s:close_buffer()
    let s:caller_winnr = -1
    close
endfunc

func! s:show_files_list()

    if s:caller_winnr != -1 | return | endif
    let s:caller_winnr = winnr()

    execute printf('%dnew', g:vt_list_buf_height)
    if !isdirectory(expand(g:vt_template_dir_path))
        call s:warn("No such dir: " . expand(g:vt_template_dir_path))
        return
    endif

    " write template files list to current buffer
    let template_files = s:glob_path_list(expand(g:vt_template_dir_path), '*')
    let i = 1
    for file in template_files
        call setline(i, file)
        normal! o
        let i = i + 1
    endfor
    delete _


    """ settings """

    setlocal buftype=nofile
    setlocal cursorline
    setlocal nobuflisted
    setlocal nomodifiable
    setlocal noswapfile

    nnoremap <buffer><silent> <CR> :call <SID>open_file()<CR>
    nnoremap <buffer><silent> q    :call <SID>close_buffer()<CR>

    file __template__
endfunc
" }}}1

" COMMAND {{{1
if g:vt_support_command
    execute 'command! ' . g:vt_command . ' call <SID>show_files_list()'
endif
" }}}1

" MAPPING {{{1
if g:vt_support_mapping
    execute 'nnoremap <silent><unique> ' . g:vt_mapping
                \ . ' :call <SID>show_files_list()<CR>'
endif
" }}}1

" RESTORE CPO {{{1
let &cpo = s:save_cpo
" }}}1

