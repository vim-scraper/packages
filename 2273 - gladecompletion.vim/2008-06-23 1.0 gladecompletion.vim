" =============================================================================
" File:         gladecompletion.vim
" Last Changed: 2008-06-11
" Maintainer:   Erik Falor  <ewfalor@gmail.com>
" Version:      1.0
" License:      Vim License
" Purpose:      User completion function for Vim for use in projects using
"               Glade-2, Glade-3 UI project files
" =============================================================================

" NOTES {{{
" WHAT IS IT?  Provides an Insert mode completion function which completes names
" from a .glade file as produced by the Glade UI designer.
" http://glade.gnome.org/
"
" HOW DOES IT WORK?  The plugin defines a function which scans .glade files in
" current buffer's current directory.  The function adds Gtk objects and event
" handlers to the insert mode completion menu
"
" The menu is invoked by pressing <C-x><C-u> in insert mode.  Read all about it
" at |i_CTRL-X_CTRL-U|.
"
" Each time the completion function is invoked, it looks at the .glade files in
" the current directory and compares the filesystem's modification timestamp
" with the timestamp value from the previous invocation.  Thus, if you
" add/change a widget in Glade and save the file, those changes are reflected
" immediately in Vim.
" }}}

" CHANGES {{{
" 1.0 2008-06-23
"   Initial Release - cleaned some things up in preparation for public release.
"
" 0.1 2008-01-16
"   Initial private version
"}}}

" Allow user to avoid loading this plugin and prevent loading twice.
if exists('loaded_gladeCompletion') || v:version < 700
    finish
endif
let loaded_gladeCompletion = 1

function! CompleteGlade(findstart, base) "{{{
    if a:findstart
        " I. locate the start of the identifier; 
        let line = getline('.')
        let start = col('.') - 1
        while start > 0 && line[start - 1] =~ '\i'
            let start -= 1
        endwhile
        return start

    else
        "II. find items from Glade file matching a:base
        if !exists("b:gladeFiles")
            let b:gladeFiles = {}
        endif

        "1. identify .glade files in CWD
        let files = split( glob(expand("%:p:h") . "/*.glade"), "\n")
        if empty(files) 
            return []
        endif

        "2. see if the glade file(s) have been changed since last scanned
        for f in files
            if !(has_key(b:gladeFiles, f) && getftime(f) == b:gladeFiles[f]['mtime'])
                let b:gladeFiles[f] = {}
                let b:gladeFiles[f]['mtime'] = getftime(f)
                let b:gladeFiles[f]['matches'] = []
        "3. scan each glade file for lines defining handlers or widgets
                if filereadable(f)
                    for line in readfile(f)
                        let m = matchlist(line, '\%(widget\|signal\)\s\+\(class\|name\)="\(\i\+\)"\s\+\%(id\|handler\)="\(\i\+\)"')
                        if [] != m
                            "let wordText equal the identifier used in the completion
                            let wordText = m[3]

                            "put the type of object into menuText; if more
                            "than one .glade files are in the CWD, append the
                            "file name so the user knows which project it
                            "comes from
                            let menuText = ''
                            if m[1] == 'class'
                                let menuText = m[2] . ' object'
                            elseif m[1] == 'name'
                                let menuText = m[2] . ' event handler'
                            else
                                let menuText = 'unknown type'
                            endif
                            if len(files) > 1
                                let menuText .= "\t[" . f . "]"
                            endif

                            "Put this info into the hash of objects defined in
                            "glade files.  These will be added to the pop-up
                            "menu after this loop finishes.
                            call add( b:gladeFiles[f]['matches'],
                                        \{ 'word' : wordText,
                                        \  'menu' : menuText,
                                        \  'dup'  : 0 })
                        endif
                    endfor
                endif
                "check for user input
                if complete_check()
                    break
                endif
            endif 
        endfor

        "delete dictionary entries if their .glade file no longer exists
        for f in keys(b:gladeFiles)
            if 0 == count(files, f)
                unlet b:gladeFiles[f]
            endif
        endfor

        "populate the completion menu
        for file in keys(b:gladeFiles)
            for match in b:gladeFiles[file]['matches']
                if match['word'] =~ '^' . a:base
                    call complete_add(match)
                endif
            endfor
        endfor
        return []
    endif
endfunction "}}}

set completefunc=CompleteGlade

" vim: set foldmethod=marker textwidth=80 fileformat=unix:
