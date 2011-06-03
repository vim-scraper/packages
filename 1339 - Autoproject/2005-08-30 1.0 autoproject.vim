
if exists('loaded_autoproject')
    finish
endif

let loaded_autoproject=1

if !exists('ap_width')
    let ap_width = 40
endif

if !exists('ap_windowpos')
    let ap_windowpos = 'topleft vertical'
endif

if !exists('ap_title')
    let ap_title = '__autoproject__'
endif

let s:running = 0
let s:dir_level = 0
let s:filenames = ""

nnoremap <silent> <F9> :call AP_Start()<CR>

function! s:ap_init_project_window()
    let s:caller_windows = winnr() 
    let winnum = bufwinnr(g:ap_title)
    if winnum != -1
        " Jump to the existing window
        if winnr() != winnum
            exe winnum . 'wincmd w'
        endif
    else
        if !bufexists(g:ap_title)
            let init_cmd = g:ap_title
        else
            let init_cmd = '+buffer ' . bufnr(g:ap_title)
        endif
        let init_cmd = g:ap_title
        exe 'silent! ' . g:ap_windowpos . ' ' . g:ap_width . ' split ' . init_cmd
        setlocal buftype=nofile
        setlocal noswapfile
        setlocal nonumber
        setlocal foldmethod=manual
    endif
    setlocal modifiable
    call s:clear_buffer()
    call s:ap_display_header()
    call s:begin_file_processing()
    setlocal foldlevel=4
    setlocal foldtext=getline(v:foldstart)
    setlocal nomodifiable
    nnoremap <buffer> <silent> <CR> :call AP_FileSelected()<CR>
    nnoremap <buffer> <silent> r :call AP_Restart()<CR>
    nnoremap <buffer> <silent> + :foldopen<CR>
    nnoremap <buffer> <silent> - :foldclose<CR>
endfunction

function! s:clear_buffer()
    silent! %delete _
    let s:line=1
endfunction   

function! s:print_line(text)
    call append(s:line, a:text)
    let s:line = s:line +1
endfunction

function! s:enter_level()
    let s:fold_begin_line_{s:dir_level} = s:line
    let s:dir_level = s:dir_level + 1 
endfunction

function! s:leave_level()
    let s:dir_level = s:dir_level - 1
    exe s:fold_begin_line_{s:dir_level} . "," . s:line . " fold"
    unlet s:fold_begin_line_{s:dir_level}
endfunction

function! s:print_line_level(text)
    let prefixstr = " "
    let i = 0
    while i < s:dir_level
        let prefixstr = prefixstr . "  "
        let i = i + 1
    endwhile
    call s:print_line(prefixstr . a:text)
endfunction

function! s:print_filename(text)
    call s:print_line_level("|- " . a:text)
    let s:filenames = s:filenames . "l-" . s:line . ": " . getcwd() . "/" . a:text . "\n"
endfunction

function! s:print_directory(text)
    call s:print_line_level("- " . a:text)
endfunction

function! s:remove_prefix(str, prefix)
    return matchstr(a:str, a:prefix . "\\s*\\zs.*")
endfunction

function! s:ap_display_header()
    call s:print_line("Automake Projects for VIM")
    call s:print_line("\'r\' reloads")
    call s:print_line("\'+\' expands folder")
    call s:print_line("\'-\' collapses folder")
    call s:print_line("")
    let i = 0
    let str = ""
    while i < g:ap_width
        let str = str . "-"
        let i = i+1
    endwhile
    call s:print_line(str)
    call s:print_line("")
endfunction

function! s:find_automake_files()
    if strlen(glob("Makefile.am"))
        return 1
    else
        return 0
    endif
endfunction

function! s:begin_file_processing()
    call s:process_am_file(glob("Makefile.am"))
endfunction

function! s:process_am_file(file)
    let subdirs = s:remove_prefix(system("cat " . a:file . "| awk '/\\\\/{printf \"%s\",$0;next}{print}' | sed 's/\\\\//g' | grep SUBDIRS"), "=")
    let source_files = s:remove_prefix(system("cat " . a:file . "| awk '/\\\\/{printf \"%s\",$0;next}{print}' | sed 's/\\\\//g' | grep _HEADERS"), "=")
    let source_files = source_files . " " . s:remove_prefix(system("cat " . a:file . "| awk '/\\\\/{printf \"%s\",$0;next}{print}' | sed 's/\\\\//g' | grep _SOURCES"), "=")
    if strlen(subdirs)
        call s:for_each_substr(subdirs, "s:process_dirname")
    endif
    if strlen(source_files) 
        call s:for_each_substr(source_files, "s:process_filename")
    endif
endfunction

function! s:process_filename(filename, index)
    call s:print_filename(a:filename)
endfunction

function! s:process_dirname(dirname, index)
    call s:print_directory(a:dirname)
    let olddir = getcwd()
    if !isdirectory(a:dirname) 
        return
    endif
    exe "chdir " . olddir . "/" . a:dirname
    call s:enter_level()
    call s:begin_file_processing()
    exe "chdir " . olddir
    call s:leave_level()
endfunction

function! s:for_each_substr(str, cmd)
    let i = 0
    let reststr = a:str
    while 1
        let substr = matchstr(reststr, "\\<\\f*")
        if strlen(substr) == 0
            break
        endif
        let reststr = matchstr(reststr, "\\<\\f*\s*\\zs.*")
        let cmd = a:cmd . "( \"" . substr . "\", " . i . ")"
        exe "call " . cmd
        let i=i+1
    endwhile
endfunction

function! AP_FileSelected()
    let filename = matchstr(s:filenames, "l-" . line(".") . ":\\s\\zs.\\{-}\\n")
    if !strlen(filename)
        return
    endif
    wincmd w
    exe "edit " . filename
endfunction

function! AP_Restart()
    let s:dir_level = 0
    let s:filenames = ""
    let s:lines = 0
    if !s:find_automake_files()
        echo "no makefile found"
        return
    endif
    call s:ap_init_project_window()
endfunction

function! AP_Start()
    if s:running == 1
        let s:running = 0
        exe "bd " . g:ap_title
        return
    endif
    let s:running = 1
    call AP_Restart()
endfunction

