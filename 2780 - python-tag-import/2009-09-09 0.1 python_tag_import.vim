"==================================================
" File:         python_tag_import.vim
" Brief:        python_tag_import import shortcut based on tags file
" Author:       Michael Brown <michael@ascetinteractive.com>
" Last Change:  2009-7-9
" Version:      0.1
"
" Install:      1. Put python_tag_import.vim to plugin
"                  directory.
"
" Usage:
"
"           You need a ctags file with the --fields=+i
"           option set
"
"           on a new line type the function you want to import
"           then hit the complete key
"
"           hotkey:
"               insert mode map.
"               <c-b> is the current default.
"
"           variables:
"
"               g:pythontagimportkey
"                   the key used to complete function
"                   parameters and key words.
"
"Limitations:
"
"           Completion is created through inspecting the filename
"           in the tags file so functions that are rereferenced will not be
"           picked up.
"
"Options:
"           Setting the path can keep options relevant. At tht moment
"           it defaults to sys.path
"           Eg.
"           let g:python_path = ['/usr/lib/python2.4/site-packages/']
"==================================================

if v:version < 700
    finish
endif

" Variable Definations: {{{1
" options, define them as you like in vimrc:
if !exists("g:pythontagimportkey")
    let g:pythontagimportkey = "<c-b>"   "hotkey
endif

" ----------------------------

" Autocommands:
autocmd BufReadPost,BufNewFile * call PythonTagImportStart()

" Menus:
menu <silent>       &Tools.Python\ Tag\ Complete\ Start          :call PythonTagImportStart()<CR>
menu <silent>       &Tools.Python\ Tag\ Complete\ Stop           :call PythonTagImportStop()<CR>

" Function Definations:

function! PythonTagImportStart()
    exec "silent! iunmap  <buffer> ".g:pythontagimportkey
    exec "inoremap <buffer> ".g:pythontagimportkey."  <c-r>=PythonTagImportComplete()<cr>"
endfunction

function! PythonTagImportStop()
    exec "silent! iunmap <buffer> ".g:pythontagimportkey
endfunction


if !exists('g:python_path')
    python import sys;import vim;vim.command("let g:python_path = %s" % str(sys.path))
endif

function!PythonTagImportComplete()
    let s:taglist =taglist(substitute ( '^'.  getline ('.'), '^[\t ]*','','' ) . "$")
    let curtag = getline(".")
    let s:pythontagcomplete_selected = ''

    let s:pythontagcomplete_list = []
    for tag in s:taglist
        if tag['kind'] != 'm'
            for path in g:python_path
                let filename = tag['filename']
                if match(filename, path) != -1
                    let filename = substitute(filename, path, '','')
                    let filename=substitute(filename, "/", '.', 'g')
                    let filename=substitute(filename, ".__init__.py", "", "")
                    let filename=substitute(filename, "\.py$", "", "")
                    "        let filename=substitute(filename, ".$", "", "")
                    let filename=substitute(filename, "^\\.", "", "")

                    let filename = 'from ' . filename . " import " . curtag

                    let s:pythontagcomplete_list += [filename]
                endif
            endfor

        endif
    endfor

    if s:pythontagcomplete_list==[]
        return ''
    endif

    if len(s:pythontagcomplete_list)==1
        call setline(".", s:pythontagcomplete_list[0])
        return ''
    else
        call setline(".","")
        call  complete(col('.'),s:pythontagcomplete_list)
        return ''
    endif
endfunction
