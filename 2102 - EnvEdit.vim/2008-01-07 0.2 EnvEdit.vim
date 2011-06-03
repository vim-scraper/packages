" File:         EnvEdit.vim
" Last Changed: 2007-12-17
" Maintainer:   Erik Falor <ewfalor@gmail.com>
" Version:      0.2
" License:      The However License - use this software however you like.
"
" Motivation:   Changing environment variables in Windows' "Environment
"               Variables" dialog is just WAY too tedious.  This plugin
"               provides the following benefits: 
"                   1. Vim's editing keystrokes.
"                   2. You can resize the window so that the entire value
"                      fits on the screen.
"                   3. You can easily use the value of a different variable
"                      in the definition of another.
"                   4. Vim shows up in the taskbar, so you don't have to
"                      minimize all of your other windows to find the
"                      misplaced "Environment Variables" dialog after looking
"                      up the correct value for a variable in the
"                      documentation.
"                   5. Command-line completion is an easier way to locate
"                      a variable than hunting for it in two different
"                      five-line high lists.
"
" Usage:        At the command-line type this command: :EnvEdit {VARIABLE}
"               This will open a new buffer named $VARIABLE and containing the
"               value of that environment variable.  If there exists no such
"               environment variable, you'll be greeted with an empty buffer.
"
"               When the buffer is saved, the new text is stored in the
"               default register "", exported to the clipboard via the "+ and
"               * registers register, and exported to Vim's own environment.
"               This means that any child processes of that instance of Vim
"               inherit the new environment variable.
"
"               If the environment variable contains your system's path
"               separator character (':' on UNIX and ';' on Windows), the
"               variable's text is split on this character and each component
"               is presented on its own line.  When you save the buffer, all
"               lines are joined and delimited by this same character.  Empty
"               lines are filtered out to prevent consecutive path separators
"               from appearing.

"Changes {{{
"2007-12-17 Version 0.2: Push value into "*, X's selection buffer.
"
"2007-12-17 Version 0.1: Initial Upload
"}}}

" Load plugin once {{{
if exists('loaded_EnvEdit') || v:version < 700
    finish
endif
let loaded_EnvEdit = 1 "}}}

"Interface "{{{
command! -bang -nargs=+ -complete=environment EnvEdit call OpenEnvVar(<f-args>, "<bang>")
"}}}

"OS-dependant malarky {{{
if has("win32") 
    let s:pathsep = ';'
else
    let s:pathsep = ':'
endif "}}}

function! OpenEnvVar(var, ...) "{{{
    "make a new buffer if one doesn't already exist
    "else bring up the existing buffer
    let bufn = bufnr("\$" . a:var)
    if -1 == bufn 
        "buffer doesn't exist, create
        if a:1 == '!'
            exe 'edit! \$' . a:var
        else
            exe 'edit \$' . a:var
        endif

        let b:EnvVarName = a:var
        
        "see if there is a envVar
        if exists('$' . a:var)
            call ReadEnvVar(a:var)
        endif
        setlocal buftype=acwrite bufhidden=hide nomodified 
    elseif '' == getbufvar(bufn, 'EnvVarName') 
        "buffer exists but doesn't appear to be used for an Env variable
        echoe "Buffer for " . a:var . " is a regular file"
        return
    else
        "buffer exists and is an Env variable
        if bufn != bufnr('.')
            exe "buffer " . a:var
        endif

        if a:1 == '!'
            normal 1GdG
            call ReadEnvVar(a:var)
            setlocal nomodified 
        endif
    endif
    au BufWriteCmd <buffer> call WriteEnvVar()
endfunction "}}}

function! ReadEnvVar(var) "{{{
    exe "let value = $" . a:var
    "split if it contains ;'s
    let valueL = split(value, s:pathsep, 1)
    call append(0, valueL)
    normal 0G
endfunction "}}}

function! WriteEnvVar() "{{{
    setl nomodified
    let valueS = join( filter( getline(1, '$'), "v:val !~ '^\s*$'") , s:pathsep)
    call setreg('+', valueS, 'l')
    call setreg('"', valueS, 'l')
    call setreg('*', valueS, 'l')
    exe "let $" . b:EnvVarName . " = valueS"
    redraw | echom "Exported value of $" . b:EnvVarName . " to clipboard"
endfunction "}}}

" vim: set foldmethod=marker textwidth=78 expandtab ff=unix:
