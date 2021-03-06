" vim:foldmethod=marker:fen:
scriptencoding utf-8

" DOCUMENT {{{1
"==================================================
" Name: vimtemplate
" Version: 0.0.4
" Author:  tyru <tyru.exe@gmail.com>
" Last Change: 2009-05-31.
"
" Change Log: {{{2
"   0.0.0: Initial upload.
"   0.0.1: implement g:vt_files_using_template and its template syntax.
"   0.0.2: fix bug that vimtemplate won't inline
"   and delete g:vt_support_command and g:vt_support_mapping.
"   not to define/map command/mapping.
"   let g:vt_command/g:vt_mapping be empty.
"   0.0.3: add <%author%>, <%email%>, <%filename_camel%>, <%filename_snake%>
"   0.0.4: delete g:vt_files_using_template. and support modeline in
"   template file.
" }}}2
"
" Usage: {{{2
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
"           search files in this dir.
"           to specify multi-dirs, set paths joined with ",".
"
"       g:vt_command (default:"VimTemplate")
"           command name.
"
"       g:vt_mapping (default:"gt")
"           mapping.
"
"       g:vt_list_buf_height (default:7)
"           height of list buffer.
"           buffer shows you list of template files.
"
"       g:vt_filetype_files (default: "")
"           when you load one of these files, exec :setlocal ft=<filetype>.
"           search these files in your g:vt_template_dir_path.
"           e.g.: "java_template.java=java,cpp_template.cpp=cpp"
"
"       g:vt_author
"           expand <%author%> to this value.
"
"       g:vt_email
"           expand <%email%> to this value.
"
"   TEMPLATE SYNTAX:
"       please open the list buffer
"       after naming current buffer by
"
"       :e[dit] filename
"       or
"       :f[ile] filename
"
"       if you didn't, this script uses template file path.
"       and you don't have to delete whitespace in <%%>.
"       this plugin also allows both <%filename%> and <% filename %>.
"
"
"       <%eval:code%>
"           will expand into result value of code.
"
"       <%path%>
"           will expand into current path.
"           same as <%eval:expand('%')%>.
"
"       <%filename%>
"           will expand into current file name.
"           same as <%eval:expand('%:t')%>.
"
"       <%filename_noext%>
"           will expand into current file name without extension.
"           same as <%eval:expand('%:t:r')%>.
"
"       <%parent_dir%>
"           will expand into current file's dir.
"           same as <%eval:expand('%:p:h')%>.
"           
"       <%author%>
"           same as <% eval: g:vt_author %>.
"
"       <%email%>
"           same as <% eval: g:vt_email %>.
"
" }}}2
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
if !exists('g:vt_command')
    let g:vt_command = 'VimTemplate'
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
if !exists('g:vt_author')
    let g:vt_author = ''
endif
if !exists('g:vt_email')
    let g:vt_email = ''
endif
" }}}1

" FUNCTION DEFINITION {{{1
" s:warn(msg) {{{2
func! s:warn(msg)
    echohl WarningMsg
    echo a:msg
    echohl None
endfunc
" }}}2

" s:glob_path_list(path, expr) {{{2
func! s:glob_path_list(path, expr)
    let files = split(globpath(a:path, a:expr), '\n')
    call map(files, 'fnamemodify(v:val, ":t")')
    call filter(files, 'v:val !=# "." && v:val !=# ".."')
    call map(files, 'a:path . "/" . v:val')
    return files
endfunc
" }}}2

" s:get_filetype_of(path) {{{2
func! s:get_filetype_of(path)
    for pair in split(g:vt_filetype_files, ',')
        let [fname; filetype] = split(pair, '=')
        if empty(filetype) | continue | endif

        if get(split(a:path, '/'), -1, 123) ==# fname
            return filetype[0]
        endif
    endfor

    return ''
endfunc
" }}}2

" s:apply_template(text, path) {{{2
func! s:apply_template(text, path)
    let text = a:text
    let [i, len] = [0, len(text)]

    while i < len
        " template syntax
        let text[i] = s:expand_template_syntax(text[i], a:path)
        " modeline in template file
        call s:eval_modeline(text[i], a:path)

        let i = i + 1
    endwhile

    return text
endfunc
" }}}2

" s:expand_template_syntax(line, path) {{{2
func! s:expand_template_syntax(line, path)
    let line = a:line
    let regex = '\m<%\(.\{-}\)%>'
    let path = expand('%') == '' ? a:path : expand('%')
    let replaced = ''

    let lis = matchlist(line, regex)
    call filter(lis, '! empty(v:val)')

    while !empty(lis)
        if lis[1] =~# '\m\s*eval:'
            let code = substitute(lis[1], '\m\s*eval:', '', '')
            let replaced = eval(code)
        else
            if lis[1] ==# 'path'
                let replaced = path
            elseif lis[1] ==# 'filename'
                let replaced = fnamemodify(path, ':t')
            elseif lis[1] ==# 'filename_noext'
                let replaced = fnamemodify(path, ':t:r')
            elseif lis[1] ==# 'filename_camel'
                let replaced = fnamemodify(path, ':t:r')
                let m = get(matchlist(replaced, '_.'), 0, '')
                while m != ''
                    let replaced = substitute(replaced, m, toupper(m[1]), '')
                    let m = get(matchlist(replaced, '_.'), 0, '')
                endwhile
                let replaced = toupper(replaced[0]) . replaced[1:]
            elseif lis[1] ==# 'filename_snake'
                let replaced = fnamemodify(path, ':t:r')
                let l = split(replaced, '\zs')
                let mapped = map(l, 'v:val =~# "[A-Z]" ? "_".tolower(v:val) : v:val')
                let replaced = join(mapped, '')
            elseif lis[1] ==# 'parent_dir'
                let replaced = fnamemodify(path, ':p:h')
            elseif lis[1] ==# 'author'
                let replaced = g:vt_author
            elseif lis[1] ==# 'email'
                let replaced = g:vt_email
            endif
        endif

        let line = substitute(line, regex, replaced, '')
        let lis = matchlist(line, regex)
        call filter(lis, '! empty(v:val)')
    endwhile

    return line
endfunc
" }}}2

" s:eval_modeline(line, path) {{{2
func! s:eval_modeline(line, path)
    let line = a:line
    " according to vim help, there are 2 types of modeline.
    "   [text]{white}{vi:|vim:|ex:}[white]{options}
    "   [text]{white}{vi:|vim:|ex:}[white]se[t] {options}:[text]
    let regex = '\m[ \t]*\(vi\|vim\|ex\):\(.*\):'
    let match = get(matchlist(line, regex), 2, '')

    if match != ''
        " NOTE set opt=: is NG but not needed to support it maybe
        for opt in split(match, ':')
            if opt =~# '\mset\='
                " XXX modeline is setlocal?
                let opt = substitute(opt, '\mset\=', 'setlocal', '')
                execute opt
            else
                " XXX modeline is setlocal?
                let opt = 'setlocal ' . opt
                execute opt
            endif
        endfor
    endif
endfunc
" }}}2

" s:open_file() {{{2
func! s:open_file()
    " get path of template file
    let template_path = getline('.')
    if template_path == '' | return | endif

    if !filereadable(template_path)
        call s:warn(printf("can't read '%s'", template_path))
        return
    endif

    call s:close_list_buffer()
    " paste buffer into main buffer
    let text = readfile(template_path)
    let text = s:apply_template(text, template_path)
    call s:multi_setline(text)

    let ftype = s:get_filetype_of(template_path)
    if ftype != ''
        execute 'setlocal ft=' . ftype
    endif
endfunc
" }}}2

" s:close_list_buffer() {{{2
func! s:close_list_buffer()
    " if this is last window
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
endfunc
" }}}2

" s:close_main_buffer() {{{2
func! s:close_main_buffer()
    let s:caller_winnr = -1
    close
endfunc
" }}}2

" s:multi_setline(lines) {{{2
func! s:multi_setline(lines)
    %delete _

    let i = 1
    for file in a:lines
        call setline(i, file)
        normal! o
        let i = i + 1
    endfor
    delete _
endfunc
" }}}2

" s:show_files_list() {{{2
func! s:show_files_list()

    if s:caller_winnr != -1 | return | endif
    let s:caller_winnr = winnr()

    execute printf('%dnew', g:vt_list_buf_height)
    if !isdirectory(expand(g:vt_template_dir_path))
        call s:warn("No such dir: " . expand(g:vt_template_dir_path))
        return
    endif

    " write template files list to main buffer
    let template_files_list = s:glob_path_list(expand(g:vt_template_dir_path), '*')
    call s:multi_setline(template_files_list)


    """ settings """

    setlocal bufhidden=unload
    setlocal buftype=nofile
    setlocal cursorline
    setlocal nobuflisted
    setlocal nomodifiable
    setlocal noswapfile

    nnoremap <buffer><silent> <CR>  :call <SID>open_file()<CR>
    nnoremap <buffer><silent> q     :call <SID>close_main_buffer()<CR>

    file __template__
endfunc
" }}}2
" }}}1

" COMMAND {{{1
if g:vt_command != ''
    execute 'command! ' . g:vt_command . ' call <SID>show_files_list()'
endif
" }}}1

" MAPPING {{{1
if g:vt_mapping != ''
    execute 'nnoremap <silent><unique> ' . g:vt_mapping
                \ . ' :call <SID>show_files_list()<CR>'
endif
" }}}1

" RESTORE CPO {{{1
let &cpo = s:save_cpo
" }}}1

