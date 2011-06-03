" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/ctk.vim	[[[1
828
" Script Nmame: code toolkit
" File Name:    ctk.vim
" Author:       StarWing
" Version:      0.3
" Last Change:  2009-05-09 23:12:57
" Note:         see :ctk for details
" ======================================================{{{1

if v:version < 700
    echomsg "ctk.vim requires Vim 7.0 or above."
    finish
endif

let s:cpo_save = &cpo
set cpo&vim

scriptencoding utf-8
if !exists('g:loaded_ctk')
    let g:loaded_ctk = 'v0.3'

    " options {{{2
    function! s:defopt(opt, val)
	if !exists(a:opt) | let {a:opt} = a:val | endif
    endfunction

    call s:defopt('g:ctk_autofname', 'strftime("%Y-%m-%d")."-".idx')
    call s:defopt('g:ctk_cinfo_file', '.compiler_info')
    call s:defopt('g:ctk_defoutput', './output')
    call s:defopt('g:ctk_ext_var', 'ft_ext')
    call s:defopt('g:ctk_tempdir', './noname')

    if has('win32')
        call s:defopt('g:ctk_execprg', executable('vimrun') ?
                    \ 'start vimrun $exec' : 'start $exec')
    elseif has('unix') && has('gui_running')
        call s:defopt('g:ctk_execprg', 'xterm -e "$exec; '.
                    \ 'echo \"$exec returned $?\";read -s -n1 '.
                    \ '-p\"press any key to continue...\"" &')
    else
        call s:defopt('g:ctk_execprg', '')
    endif

    delfunc s:defopt

    " commands {{{2
    command! -bar -bang StartCTK call s:start_ctk('<bang>')
    command! -bar RefreshCTK call s:stop_ctk() | call s:start_ctk('!')
    command! -bar StopCTK call s:stop_ctk()
    command! -bar EditCompilerInfo exec 'drop '.globpath(&rtp, g:ctk_cinfo_file)

    command! -bar -nargs=1 SetExtensionName let b:{g:ctk_ext_var} = <q-args>
    command! -nargs=* -complete=custom,s:info_item_complete -bang
	    \ SetCompilerInfo call s:call('s:set_compiler_info', [<q-args>, '<bang>'])
    command! -nargs=+ -complete=custom,s:info_item_complete -bang
            \ SetDefaultInfo call s:call('s:set_default_info', [<q-args>, '<bang>'])

    command! -nargs=* -complete=customlist,s:info_name_complete -bar -count=0 
	    \ ListCompiler call s:find_and_call('s:list_compiler', [<q-args>, <count>])
    command! -nargs=+ -complete=custom,s:info_item_complete -count=0
	    \ AddFlags call s:find_and_call('s:add_flags', [<q-args>, <count>])

    command! -nargs=? -bar -bang -count=0 CC call s:find_and_call('s:compile',
                \ [<count>, <q-args>, <q-bang>])
    command! -nargs=? -bar -bang -count=0 RUN call s:find_and_call('s:run',
                \ [<count>, <q-args>, <q-bang>])

    map <silent> <Plug>CTK_compile :<C-U>exec v:count.'CC!'<CR>
    imap <silent> <Plug>CTK_compile <ESC>:<C-U>exec v:count.'CC!'<CR>
    map <silent> <Plug>CTK_run :<C-U>exec v:count.'RUN!'<CR>
    imap <silent> <Plug>CTK_run <ESC>:<C-U>exec v:count.'RUN!'<CR>

    " menus {{{2

    amenu &Tools.&CTK.&Start :StartCTK<CR>
    amenu &Tools.&CTK.&Stop  :StopCTK<CR>
    amenu &Tools.&CTK.-Sep- :
    amenu <silent> &Tools.&CTK.&Add\ a\ modeline :exec 'AddFlags '.(has('gui_running') ? inputdialog("Please input the text in modeline:", "flags += ''") : input("Modeline:", "flags += ''"))<CR>
    amenu &Tools.&CTK.&List\ All\ Compiler :ListCompiler all<CR>
    amenu &Tools.&CTK.&Compile :CC<CR>
    amenu &Tools.&CTK.&Run :RUN<CR>

    " small functions {{{2
    let s:sfile = expand('<sfile>')

    function! s:start_ctk(bang) " {{{3
        augroup ctk_autocmds
            au!

            " 1 means this name is user-modifiled
            au BufFilePost * let b:ctk_fname = [expand('%:p'), &ft, 1]

            au FileType * call s:delete_ci()
            exec 'run '.g:ctk_cinfo_file
            au FileType * call s:set_fname()
        augroup END

        map gc <Plug>CTK_compile
        map gC <Plug>CTK_run

        if a:bang == '!'
            unlet! b:ctk_fname
            filetype detect
        endif
    endfunction

    function! s:stop_ctk() " {{{3
        unlet! b:ctk_fname
        call s:delete_ci()
        au! ctk_autocmd
        unmap gc
        unmap gC
    endfunction

    function! s:call(funcname, args) " {{{3
        if !exists('s:load_all') || !s:load_all
            exec 'so '.s:sfile
        endif

        return call(a:funcname, a:args)
    endfunction

    function! s:find_and_call(funcname, args) " {{{3
        let cur_winnr = winnr()

        while 1
            if exists('b:compiler_info') 
                return s:call(a:funcname, a:args)
            endif
            silent! wincmd w
            if winnr() == cur_winnr | break | endif
        endwhile

        echohl Error
        echom "command can't use, because ctk isn't avaliable"
        echohl None
    endfunction

    function! s:delete_ci() " {{{3
        unlet! b:{g:ctk_ext_var}
        if exists('b:compiler_info')
            for info in get(b:compiler_info, 'list', [])
                silent! exec info.unmap
            endfor
            unlet b:compiler_info
        endif
    endfunction

    function! s:set_fname() " {{{3
        if g:ctk_autofname == '' | return | endif

        " if we really have a filetype and this type isn't our debug window's
        " and we don't have a filename, and we have a compiler info, set auto
        " name.
        if !exists('b:ctk_fname')
            return &ft != '' && &ft !=? 'decho' && expand('%') == ''
                    \ && exists('b:compiler_info') ? s:set_filename() : 0
        endif

        " if the name is autocreated and now we don't have a filetype,
        " delete the name
        if !b:ctk_fname[2] && (&ft == '' || !exists('b:compiler_info'))
            silent! noau 0file | unlet b:ctk_fname

        " if we have a filename and it isn't same as ctk_fname, set it to
        " user-defined (that is, the name is chnanged by user)
        elseif b:ctk_fname[0] != expand('%:p') || b:ctk_fname[2]
            let b:ctk_fname = [expand('%:p'), &ft, 1]

        " otherwise, call set_filename to set autocreated name
        elseif &ft !=? 'decho' && b:ctk_fname[1] != &ft
            call s:set_filename()
        endif
    endfunction

function! s:info_name_complete(A, L, P) " {{{3
    if !exists(s:ci_name) | return [] | endif
    let list = []
    for dict in b:{s:ci}.list
        let list += [dict.name]
    endfor
    let pat = "'^\\v".substitute(escape(a:A, '\'), "'", "''", 'g')."'"
    return sort(filter(list + ['all', 'default', 'current'],
                \ 'v:val =~ '.pat))
endfunction

function! s:info_item_complete(A, L, P) " {{{3
    return "asm_\ncc\ncmd\ndebug_\nflags\n".
                \ "input\noutput\nrun\ntitle"
endfunction " }}}3

    StartCTK
    " }}}2

    let &cpo = s:cpo_save
    unlet s:cpo_save
    finish
endif

let s:load_all = 1

" }}}1
" some inner variables {{{1

" escape chars in filename {{{2
let s:fname_escape_chars = " \t\n*?[{`$\\%#'\"|!<"

" buffer-variable names {{{2
let s:ci = 'compiler_info'
let s:ci_name = 'b:'.s:ci

" some default attr {{{2
let s:def_attr = {'cmd': ':echo "Done Nothing"', 'run': ':echo "Done Nothing"',
            \ 'input': '%:.', 'output': '%:t:r'}

" patterns {{{2

let s:pat_cmdtag = '\v\$(\l+)|\$\{(q-)=(\l+)}'
let s:pat_com = ':\zs[^,]\+'
let s:pat_com_begin = 's.\=:\zs[^,]\+\ze'
let s:pat_com_end = 'e.\=:\zs[^,]\+\ze'
let s:pat_execoutput = '\%^[\r\n]*\zs.\{-}\ze[\r\n]*\%$'
let s:pat_exectag = '$exec\>'
let s:pat_filespec = '\v%(\%|#%(\d+)=|##|<cword>|<cWORD>)%(:[p8~.htre]|g=s(.).\{-}\1.\{-}\1)*'
let s:pat_filespec_escape = '\\\ze'.s:pat_filespec
let s:pat_filespec_nonescape = '\\\@<!'.s:pat_filespec
let s:pat_fname_escape = "[ \t\n*?[{`$\\%#''\"|!<]"
let s:pat_info ='\v^\s*(.{-})%(\s+(.{-})\s*)=$' 
let s:pat_info_var = '\v(\w+)\s*(\+)=\=\s*(\S)(.{-})\3'
let s:pat_modeline = '\v<cc%(-([^:]*))=:\s*(.*)'
let s:pat_shellcmdtitle = '^!\=\zs.\{-}\ze\(\s\|$\)'

" ============================================================
" utility functions {{{1

" tricks to get command-style arglist {{{2
command! -nargs=* CTKGetEscapedList let l:args = [<f-args>]
function! s:get_escaped_list(str)
    exec 'CTKGetEscapedList '.a:str
    return args
endfunction

function! s:question(msg) " {{{2
    redraw
    echohl Question
    echo a:msg
    echohl NONE
    call inputsave()
    let ch = nr2char(getchar())
    call inputrestore()
    return ch
endfunction

function! s:echoerr(msg) " {{{2
    echohl ErrorMsg
    echomsg 's: '.a:msg
    echohl NONE
endfunction

function! s:get_idx(info) " {{{2
    let idx = 0
    for info in b:{s:ci}.list
        if info is a:info
            return idx
        endif
        let idx += 1
    endfor
    return -1
endfunction

function! s:get_entry_val(entry, key, default) " {{{2
    let key = (a:entry == '' ? a:key : a:entry.'_'.a:key)

    if has_key(b:{s:ci}, 'default')
        return get(b:{s:ci}.cur_info, key,
                    \ get(b:{s:ci}.default, key, a:entry == '' ?
                    \ get(s:def_attr, key, a:default) :
                    \ get(b:{s:ci}.cur_info, a:key,
                    \ get(b:{s:ci}.default, a:key,
                    \ get(s:def_attr, a:key, a:default)))))

    else
        return get(b:{s:ci}.cur_info, key, a:entry == '' ?
                    \ get(s:def_attr, a:key, a:default) :
                    \ get(b:{s:ci}.cur_info, a:key,
                    \ get(s:def_attr, a:key, a:default)))
    endif
endfunction

function! s:get_info(name) " {{{2
"    call Dfunc('s:get_info(name = '.a:name.')')
"    call Decho('info_list = '.string(b:{s:ci}.list))
    for info in b:{s:ci}.list
        if info.name ==? a:name
"            call Dret('s:get_info : '.string(info))
            return info
        endif
    endfor
"    call Dret('s:get_info : {}')
    return {}
endfunction

function! s:sub_info(info) " {{{2
"    call Decho('let '.get(a:info, 'name', 'cur_info').'['.submatch(1).'] '.
                \ submatch(2).'= "'.submatch(4).'"')

    let val = submatch(2) == '+' ? s:get_entry_val('', submatch(1), '') : ''
    let val = val == '' ? '' : val.' '
    let a:info[submatch(1)] = val.submatch(4)
endfunction

    function! s:need_update() " {{{2
        return !exists('b:ctk_fname') || b:ctk_fname[1] != &ft
    endfunction

function! s:expand_var(entry, default) " {{{2
    let key = submatch(1) == '' ? submatch(3) : submatch(1)
    let val = s:get_entry_val(a:default ? '' : a:entry, key, submatch(0))

    if submatch(2) == 'q-'
        let escape_val = escape(val, '\"')
        return a:default ? escape_val : '"'.escape_val.'"'
    endif

    return val
endfunction

function! s:expand_fname(fname, mode) " {{{2
"    call Dfunc('s:expand_fname(fname = '.a:fname.', mode = '.a:mode.')')
    let fname = expand(a:fname)

    if a:mode == ':'
        let fname = exists('*fnameescape') ? fnameescape(fname)
                    \ : escape(fname, s:fname_escape_chars)
    endif

    if fname =~ s:pat_fname_escape
        let fname = shellescape(fname)
    endif

"    call Dret('s:expand_fname : '.fname)
    return fname
endfunction

function! s:process_placeholder(cmd, entry) " {{{2
"    call Dfunc('s:process_placeholder(cmd = '.a:cmd.', entry = '.a:entry.')')
    let cmd = a:cmd

    if a:entry != ''
        let cmd = substitute(cmd, s:pat_cmdtag, '\=s:expand_var(a:entry, 0)', 'g')
    endif
    let cmd = substitute(cmd, s:pat_cmdtag, '\=s:expand_var(a:entry, 1)', 'g')
    let cmd = substitute(cmd, s:pat_filespec_nonescape,
                \ '\=s:expand_fname(submatch(0), cmd[0])', 'g')
    let cmd = substitute(cmd, s:pat_filespec_escape, '', 'g')

"    call Dret('s:process_placeholder : '.cmd)
    return cmd
endfunction " }}}2

" ============================================================
function! s:set_compiler_info(cmdarg, bang) " {{{1
    if !s:need_update() | return | endif
"    call Dfunc('s:set_compiler_info(cmdarg = "'.a:cmdarg.'")')
    if !exists(s:ci_name)
        let b:{s:ci} = {'list':[]}
    elseif !has_key(b:{s:ci}, 'list')
        let b:{s:ci}.list = []
    endif

    " empty command
    if a:cmdarg == ''
        if a:bang == '!' | call s:delete_ci() | endif
"        return Dret('s:set_compiler_info')
    endif

    " find name and others, mlist = [all, name, infos]
    let mlist = matchlist(a:cmdarg, s:pat_info)

    " add or modify a info
    if mlist[2] != ''
        let info = s:get_info(mlist[1])

        " add a new info, or clean old info
        " if exists ci.default, use it for default values
        if empty(info)
"            call Decho('add a new info')
            let info.name = mlist[1]
            call add(b:{s:ci}.list, info)
        else
"            call Decho('clear old info: '.string(info))
            silent! exec info.unmap
            call filter(info, 0)
        endif

        let info.name = mlist[1]
        call substitute(mlist[2], s:pat_info_var, '\=s:sub_info(info)', 'g')

        let info.unmap = ''
        let idx = s:get_idx(info)
        let dict = {'cmdmap': 'CC', 'runmap': 'RUN'}
        for key in keys(dict)
            if !has_key(info, key) | continue | endif
            let {key} = ''
"            call Decho('setup '.key)
            for mkey in s:get_escaped_list(info[key])
                let cpos = stridx(mkey, ':')
                for mode in split(cpos <= 0 ? 'nvi' : mkey[:cpos - 1], '\zs')
                    try | exec mode.'noremap <silent><unique> '.mkey[cpos+1:].
                                \ ' <C-\><C-N>:'.(idx+1).dict[key].'!<CR><C-\><C-G>'
                        let info.unmap .= mode.'unmap '.mkey[cpos+1:].'|'
                        let {key} .= mode.'map:'.mkey[cpos+1:].' '
                    catch | endtry
                endfor
            endfor
            if {key} == '' | unlet info[key]
            else | let info[key] = {key} | endif
        endfor

    " remove a info
    elseif a:bang == '!'
        let info = s:get_info(mlist[1])

        if !empty(info)
            silent! exec info.unmap
            call remove(b:{s:ci}, s:get_idx(info))
        endif

    " list a info
    else
        call s:list_compiler(mlist[1])
    endif

"    call Decho('>> now info = '.string(info))
"    call Dret('s:set_compiler_info')
endfunction

function! s:set_default_info(cmdarg, bang) " {{{1
    if !s:need_update() | return | endif
"    call Dfunc('s:set_default_info(cmdarg = "'.a:cmdarg.'")')
    if !exists(s:ci_name)
        let b:{s:ci} = {}
    endif
    if a:bang == '!' || !has_key(b:{s:ci}, 'default')
        let b:{s:ci}.default = {}
    endif
    let def_info = b:{s:ci}.default

    call substitute(a:cmdarg, s:pat_info_var, '\=s:sub_info(def_info)', 'g')

    if has_key(def_info, g:ctk_ext_var)
        let b:{g:ctk_ext_var} = def_info[g:ctk_ext_var]
        unlet def_info[g:ctk_ext_var]
    endif

"    call Dret('s:set_default_info')
endfunction

function! s:compile(count, entry, bang) " {{{1
    " find source buffer, and save it. if failed (return nonzero), echo
    " message and return 1 (failed, no use in this version)
    " NOTE: we find source in s:find_and_call() function, so we needn't find source
    " again. 
    if !s:save_source() || a:count < 0 || a:count > len(b:{s:ci}.list)
        redraw | echo 'Nothing Done'
        return 1
    endif
"    call Dfunc('s:compile(count = '.a:count.', entry = '.a:entry.
                \ ', bang = '.a:bang.')')

    " init variables, and make "cur_info" dict.
    let ci = b:{s:ci}
    if a:bang == '!' | silent! unlet ci.cur_idx ci.cur_entry | endif
    let ci.cur_idx = a:count == 0 ? get(ci, 'cur_idx', 0) : a:count - 1
    let ci.cur_entry = a:entry == '' ? get(ci, 'cur_entry', '') : a:entry
    call s:make_cur_info(ci.list[ci.cur_idx])

    " "entry" is just something like trigger. you press :CC entry, then
    " "entry" specified commands will be executed.
    redraw | echo 'Compiling ...'
    let ret_val = 1
    let msg = 'Compiling... using '.
                \ s:get_entry_val(ci.cur_entry, 'title', ci.cur_info.name)
    let cmd = s:get_entry_val(ci.cur_entry, 'cmd', '')
    let cmd = s:make_cmd(cmd, ci.cur_entry, 1)
    let res = s:exec_cmd(cmd)
    let cfile = [msg, cmd, ''] + split(res, "\<NL>")

    redraw
    if cmd[0] != ':'
"        call Decho('A shell command')
        let ret_val = v:shell_error
        let cfile += [ci.cur_info.name.' returned '.ret_val]

        try
            call writefile(cfile, &errorfile) 
            cgetfile | exec v:shell_error == 0 ? 'cwindow' : 
                        \ (res == '' ? 'cclose' : 'copen')

            echo 'Compile' (v:shell_error ? 'Fail' : 'Successful')
        catch
            call s:echoerr('error when write error log: '.v:exception)
        endtry

    elseif res != ''
"        call Decho('A exec command')
        echo join(cfile, "\n")
    endif

"    call Dret('s:compile : '.ret_val)
    return ret_val
endfunction

function! s:run(count, entry, bang) " {{{1
    let bufnr = bufnr('%')

    " if the saved index and entry is incorrect. and we lack cur_info or
    " cur_stat, and the source code file is newer than program file.  do
    " compiling work.
    if (a:bang == '!' && &mod)
                \ || !has_key(b:{s:ci}, 'cur_entry')
                \ || !has_key(b:{s:ci}, 'cur_idx')
                \ || !has_key(b:{s:ci}, 'cur_info')
                \ || !has_key(b:{s:ci}, 'cur_stat')
                \ 
                \ || (a:count != 0 && b:{s:ci}.cur_idx != a:count - 1)
                \ || b:{s:ci}.cur_entry != a:entry
                \
                \ || (getftime(b:{s:ci}.cur_stat.input)
                \ > getftime(b:{s:ci}.cur_stat.output))

"        call Decho('>> s:run need re-compile...')
"        call Decho('>> '.string(filter(copy(b:{s:ci}), 'v:key =~ "^cur_"')))
        if s:compile(a:count, a:entry, a:bang) | return 1 | endif
"    else | call Decho(">> s:run needn't re-compile.")
    endif
"    call Dfunc('s:run(count = '.a:count.', entry = '.a:entry.
                \ ', bang = '.a:bang.')')

    exec bufwinnr(bufnr).'wincmd w'

    let cmd = s:make_cmd(s:get_entry_val(a:entry, 'run', ''), a:entry, 0)
    if cmd[0] != ':'
        let cmd = cmd[0] == '!' ? cmd[1:] : cmd
        if g:ctk_execprg != ''
            let cmd = substitute(g:ctk_execprg, s:pat_exectag,
                        \ escape(cmd, '\'), 'g')
        endif
        let cmd = (has('gui_running') ? ':silent !' : ':!').cmd
    endif

    let res = s:exec_cmd(cmd)

    if res !~ '^\_s*$' && !has('gui_running')
        redraw
        for line in split(res, '\n\|\r\|\r\n')
            echomsg line
        endfor
    endif

    " TODO: sometimes there are two hit-enter notify, but term linux only one.
    if &term != 'linux'
        redraw!
    endif

"    call Dret('s:run')
endfunction

function! s:add_flags(flags, count) " {{{1
    if a:count > 0 && a:count <= len(b:{s:ci}.list)
        let compiler = '-'.b:ctk.info[a:count - 1].name
    else
        let compiler = ''
    endif

    let com_begin = matchstr(&com, s:pat_com_begin)
    if com_begin != ''
        let com_begin .= ' '
        let com_end = ' '.matchstr(&com, s:pat_com_end)
    else
        let com_begin = matchstr(&com, s:pat_com).' '
        let com_end = ''
    endif
    
    call append(line('$'), com_begin.'cc'.compiler.': '.a:flags.com_end)
endfunction

function! s:set_filename() " {{{1
"    call Dfunc('s:set_filename()')

    if exists('b:'.g:ctk_ext_var) | let ext = b:{g:ctk_ext_var}
    elseif &sua != '' | let ext = &sua[1:]
    elseif &ft != '' | let ext = &ft
    else 
        let b:ctk_fname = [expand('%:p'), &ft, 0] 
        return
    endif

"    call Decho('ext = "'.ext.'"')
    let tempdir = get(b:, 'ctk_tempdir', g:ctk_tempdir)
    if !isdirectory(tempdir) && exists('*mkdir')
        call mkdir(tempdir, 'p')
    endif
    if !isdirectory(tempdir)
        let tempdir = '.'
    endif
    let tempdir = fnamemodify(tempdir, ':p')
"    call Decho('tempdir = "'.tempdir.'"')

    if exists('b:ctk_tempdir')
        let b:ctk_tempdir = tempdir
    else
        let g:ctk_tempdir = tempdir
    endif

    let fname = get(b:, 'ctk_autofname', g:ctk_autofname)
    let idx = 1
    while filereadable(tempdir.'/'.eval(fname).'.'.ext)
        let idx += 1
    endwhile
"    call Decho('fname = "'.eval(fname).'.'.ext.'"')

    if getcwd() == $VIMRUNTIME
"        call Decho('now we are in $VIMRUNTIME, just out of it...')
        exec 'lcd '.tempdir
"        call Decho('now we are in "'.getcwd().'"')
    endif

    silent exec 'file '.simplify(fnamemodify(tempdir.glob('/').
                \ eval(fname).'.'.ext, ':.'))

    let b:ctk_fname = [expand('%:p'), &ft, 0]
"    call Dret('s:set_filename')
endfunction

function! s:list_compiler(name, idx) " {{{1
    " offer index, just show the speciafied info
    if a:0 != 0 && a:1 != 0
        call s:show_list(b:{s:ci}.list[a:1])

    " didn't offer anything, show the must normal things
    elseif a:name == ''
        if has_key(b:{s:ci}, 'default')
            call s:show_list(b:{s:ci}.default)
        endif
        if !has_key(b:{s:ci}, 'cur_info') " all
            for info in b:{s:ci}.list
                call s:show_list(info)
            endfor
        else " current
            call s:show_list(b:{s:ci}.cur_info)
        endif
    elseif a:name ==? 'all'
        for info in b:{s:ci}.list
            call s:show_list(info)
        endfor
    elseif a:name ==? 'current' && has_key(b:{s:ci}, 'cur_info')
        call s:show_list(b:{s:ci}.cur_info)
    elseif a:name ==? 'default' && has_key(b:{s:ci}, 'default')
        call s:show_list(b:{s:ci}.default)
    else
        call s:show_list(s:get_info(a:name))
    endif
endfunction

function! s:save_source() " {{{1
"    call Dfunc('s:save_source()')

    try 
        silent write 
"        call Dret('s:save_source : success')
        return 1

    catch /E13/ " File exists
        let res = s:question("File Exists, Overwrite?([Y]yes/[N]o/[C]ancel):")

    catch /E45/ " Readonly
        let res = s:question("File Readonly, Still write?([Y]yes/[N]o/[C]ancel):")

    catch
        call s:echoerr('error occur when save source: '.v:exception)
        let res = s:question("Force to write?([Y]yes/[N]o/[C]ancel):")

    endtry

    if res ==? 'y'
        try 
            silent write! 
"            call Dret('s:save_source : success')
            return 1
        catch | call s:echoerr("can't force save source: ".v:exception)
        endtry
    elseif res ==? 'n'
"        call Dret('s:save_source : success')
        return 1
    endif

"    call Dret('s:save_source : fail')
endfunction

function! s:make_cur_info(info) " {{{1
    if expand('%') == '' | return | endif
"    call Dfunc('s:set_cur_info(info = '.a:info.name.')')
    let cur_info = copy(a:info)
    for var in ['cmd', 'run', 'un']
        silent! unlet! cur_info[var.'map']
    endfor

    " now make cur_info and cur_stat (it's for s:run)
    let b:{s:ci}.cur_info = cur_info
    let b:{s:ci}.cur_stat = {}

    if exists('b:ctk_fname') && !b:ctk_fname[2]
"        call Decho('filename is an autogenerated name, use defoutput')
        let cur_info.output = g:ctk_defoutput
"    else | call Decho('filename is user-defined')
    endif

    " analyze the modeline to modifie the info
    if &modeline && &mls != 0
        let last = line('$')
        if last <= &mls * 2
            call s:read_modeline(1, last)
        else
            call s:read_modeline(0, &mls)
            call s:read_modeline(last - &mls, last)
        endif
    endif

"    call Dret('s:set_cur_info : '.string(cur_info))
endfunction

function! s:make_cmd(cmd, entry, for_compile) " {{{1
    if !has_key(b:{s:ci}, 'cur_info') | return | endif
"    call Dfunc('s:make_cmd(cmd = "'.a:cmd.'", entry = "'.a:entry.'")')

    let cmd = s:process_placeholder(a:cmd, a:entry)

    " prepare input and output for s:run()
    if a:for_compile
        for var in ['input', 'output']
            let b:{s:ci}.cur_stat[var] = s:process_placeholder(s:get_entry_val(
                        \ a:entry, var, ''), '')
        endfor

    " this is just for run
    elseif cmd[0] !~ ':'
"        call Decho('this is a executable cmd')
        let exe = matchstr(cmd, s:pat_shellcmdtitle)
"        call Decho('cmd = '.cmd.', exe = '.exe)

        " on unix, the program must has "./" prefix if it's at current folder
        if has('unix') && exe[:1] != './'
                    \ && glob(exe) != '' && executable('./'.exe)
            let cmd = './'.(cmd[0] == '!' ? cmd[1:] : cmd)
        endif

        " but ./foobar can't exec on windows :-(
        " so, take of the "./" prefix
        if has('win32') && cmd =~ '^!\=\./'
            let cmd = '!'.(cmd[0] == '!' ? cmd[3:] : cmd[2:])
        endif

        let cmd = cmd[0] != '!' ? '!'.cmd : cmd
    endif

"    call Dret('s:make_cmd : '.cmd)
    return cmd
endfunction

function! s:exec_cmd(cmdarg) " {{{1
    if !has_key(b:{s:ci}, 'cur_info') | return | endif
"    call Dfunc('s:exec_cmd(cmdarg = '.a:cmdarg.')')

    if a:cmdarg[0] == ':'
        " TODO: error when use l:output :-(
        redir => l:output
        exec a:cmdarg
        redir END
        let output = matchstr(output, s:pat_execoutput)
    else
        let output = system(a:cmdarg[0] == '!' ? a:cmdarg[1:] : a:cmdarg)
    endif

"    call Dret('s:exec_cmd : '.output)
    return output
endfunction

function! s:show_list(info) " {{{1
    if empty(a:info) | return | endif

    echohl Title
    echo has_key(a:info, 'name') ?
                \ (has_key(a:info, 'title') ? 
                \ a:info.title."\n\tname         = ".a:info.name."\n"
                \ : a:info.name."\n")
                \ : "Default Values: ".&ft." files"
    echohl NONE

    for key in sort(filter(keys(a:info),
                \ "v:val !~ '".'^\%(title\|name\|unmap\)$'."'"))
        echo printf("\t%-12s = %s", key, a:info[key])
    endfor
endfunction

function! s:read_modeline(begin, end) " {{{1
"    call Dfunc('s:read_modeline(begin = '.a:begin.', end = '.a:end.')')
    let pos = winsaveview()

    call cursor(a:begin, 1)
    while search(s:pat_modeline, '', a:end) != 0
"        call Decho('find a modeline in line '.line('.'))
        let mlist = matchlist(getline('.'), s:pat_modeline)
        if mlist[1] == '' || b:{s:ci}.cur_info.name =~ '^\V'.escape(mlist[1], '\')
            call substitute(mlist[2], s:pat_info_var,
                        \ '\=s:sub_info(b:{s:ci}.cur_info)', 'g')
        endif
    endwhile

    call winrestview(pos)
"    call Dret('s:read_modeline')
endfunction

" ======================================================{{{1

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: ff=unix ft=vim fdm=marker sw=4 ts=8 et sta nu
doc/ctk.txt	[[[1
516
*ctk.txt*	Plugin for compile and run your source code in Vim

ctk - Compile Toolkit					*ctk* *CTK*
    Wroten by StarWing <weasley_wx AT qq DOT com>
Last Change: May 10th, 2009

|ctk-description|	Description
|ctk-options|		Option
|ctk-usage|		Usage
|ctk-features|		Features
|ctk-contact|		Contact

For Vim version 7.0 or later.
{Vi does not have any of these features.}

==============================================================================
DESCRIPTION						*ctk-description*

ctk.vim makes you easily and freely compiler your source code on you own way.
it read several modeline in your file to modify the behavior your command do,
such as change command flags, add redirect when exec it, etc.  You can define
your own hotkey to invoke ctk, and do any happy things with it.

the most usefull function of ctk is its modeline. you can just add a line in
your source code: |ctk-modeline|
>
 /* cc-gcc: flags+='-mwindows' */
<
then, if you have a compiler_info line (|ctk-info-line|) in your cinfo file
(defualtly as .compiler_info |ctk-cinfo-file|) like:
>
 SetCompilerInfo gcc cmd='gcc $input $flags -o $output' run='!$output'
<
you will find that you can invoke your compiler (use "gc" |ctk-hotkey|) with
this command line: |ctk-cmd-type|
>
 gcc source.c -Wall -mwindows -o source
<
and, after compile, you can run it, just press the hotkey you defined, or "gC"
in normal mode.

==============================================================================
USAGE								*ctk-usage*

								  *ctk-hotkey*

you can press hotkey to invoke compiler to compile your code, or invoke the
program generated by compiler. default hotkey is gc, you can add <count>
before it to specified which compiler you want to invoke. default run hotkey
is gC. to define specified hotkey, see |ctk-hotkey-tag|.

CTK commands		   					*ctk-commands*

								   *:StartCTK*
								    *:StopCTK*
:StartCTK
:StopCTK
		start to use CTK, or stop CTK. or you can use StartCTK to
		refresh your cinfo file after you modified it.

									 *:CC*
:[N]CC[!] {entry}
		this command is used to invoke specified compiler. [N] is your
		compiler index. you can see it in :ListCompiler command
		|:ListCompiler|. {entry} is the command you invoke, in this
		way you can use any command in a single info line.  it will be
		discussed in |ctk-entry-tag|.
		
		if the compiler command is executed a extern program, the
		quickfix window will open if compiler return nonzero.
		otherwise, the compiler output will display, if any.

		if it has a bang and doesn't have an entry, it uses a empty
		entry, that is, invoke "cmd" tag in your info line. if it
		doesn't have a bang, it will invoke the entry you used last
		time.

									*:RUN*
:[N]RUN[!] {entry}
		run the program generated by compiler. if you change your
		source code after compiling or the index [N] or {entry} isn't
		the same with the ones when you invoke compiler, the compiler
		will be invoked first to compile your code again.

			NOTE: if the compiler command is ":foobar" (that is,
			      exec a Vim command) and it has some output, the
			      Run command will be not executed. you should run
			      :RUN again. if you don't want this side-effect.
			      just use ':silent ....' in your compiler
			      command.

								   *:AddFlags*
:AddFlags {flags}
		add a modeline to source code. contains all texts in {flags}.
		see |ctk-modeline|.

							       *:ListCompiler*
:ListCompiler [all | current | default | {name}]
		list compilers ctk support now. 'all' means list all compilers.
		and 'current' means list the compiler just compiled your code.
		'default' means the attributes in your SetDefaultInfo
		sentence.  or a specified name to list this compiler's
		attributes.

		NOTE: you can press <TAB> to complete all names above.

							   *:EditCompilerInfo*
:EditCompilerInfo
		edit your cinfo file.

							     *:SetDefaultInfo*
:SetDefaultInfo {attr}="value" {attr2}=!value2! ...

		set default info for this file. add this command in your cinfo
		file if this is your default value, or it will be dropped
		after you leave Vim!

		you can use anything to replace '"' or '!' to around values.

							    *:SetCompilerInfo*
:SetCompilerInfo {name} {attr}="value" {attr2}=!value2|

		same as SetDefaultInfo. but this is for setting attributes for
		specified compiler. see |ctk-set-compiler-info|


							     *ctk-auto-rename*

When you use ctk.vim. It will check all files you open. if you open a new file
 which its filetype has some info line in your cinfo file, the new file will
 be renamed by ctk.vim. it only happens when g:ctk_autofname isn't empty
 |ctk_autofname|.  this option controls the way that ctk.vim product the new
 filename.

if a file has a autocreated name, this name will be used in compile. when you
conpile it, it will be saved in a temporary folder |ctk_tempdir|. but the
executable output program will be dropped in your current folder (except when
current folder is $VIM, in this case program will be dropped to temporary
folder).  the executable program will be named to the value of g:ctk_defoutput
|ctk_defoutput|.


								*ctk-modeline*

you can use modeline in your source code to specified the special flags. just
add a line: >

 /* cc: flags+='foobar' */
<
where the cc: will be recognized as a signature of modeline. you can also use
cc-gcc: to specifie gcc-specified settings. the "flags" is a attribute name in
SetCompilerInfo or SetDefaultInfo |ctk-def-attr|. and gcc is the name of a
info line |:SetCompilerInfo|. you can use anything to replace "'", just like
in SetCompilerInfo. |ctk-info-format|

							 *ctk-modeline-format*
the format of modeline is:

 com-start cc[-compiler_name]: <tag> [+]= ?any thing? com-end ~

where com-start and com-end are the comment start sign and end sign, they can
be empty. 'cc' is the signature of modeline, compiler_name is a specified
compiler info line name. it's optional, and if you offer it, this modeline
will only effect when you invoke the specified compiler.

tag can be any undercase letter. it discussed at |ctk-info-tag|.

if you use tag += 'foobar', then foobar will be added to the value of tag.
e.g. if the value of tag "flags" is "-Wall", "flags += '-mwindow'" will change
flags into '-Wall -mwindow'.

another example, if we don't have a tag named 'flags', "flags = '-Wall'" will
define a new tag and set its value to '-Wall'.

you can use command :AddFlags to add a modeline in your file |:AddFlags|.

there are several examples of modeline: >

 /* cc: output='foobar' */  : set output filename to foobar

 /* cc: cc=#cl# flags=## */ : set compiler to cl (cmd='$cc $input $flags -o
			      $output')

 /* cc-vc6: flags+='/HAs' */
			    : add /HAs into flags, only effect when you invoke
			      the compiler named 'vc6'

 /* cc: run='!$output <data' */
			    : change run command to '!$output <data', that is,
			      use a default input for your program.

 -- cc: cc='ghc' haskell type comments
 {- cc: cc='ghc' -} -- haskell type comments
 // cc: flags='' C++ type comments

			    : modeline support any kind of commnets, as if
			      they contain 'cc:' or 'cc-name:'
<

==============================================================================
FEATURES							*ctk-features*


							      *ctk-cinfo-file*

you can define your source compiler and compile-flags in your cinfo file, it
defaultly ".compiler_info" in your 'runtimepath', that is usually '~\.vim' in
unix/linux, or '$VIM\vimfiles' in windows. you can open it and edit it with
executing :EditCompilerInfo, just try ':StartCTK' to refresh your new settings.


							  *ctk-fileext-define*

before define your own compiler info in a new filetype, maybe you need add a
"let" sentence (|:let|) to define the ext-name of your source code (e.g. "c"
for c code, "cpp" for c++ code, and hs for haskell code, etc.):
>
		autocmd c let b:ft_ext = "c"
<
ft_ext is the value of option ctk_ext_var. the better way is:
>
		autocmd c let b{g:ctk_ext_var} = "c"
<
see |curly-braces-names|.

							       *ctk-info-line*
that file is combined with several autocmds |autocmd|. you can add a new
compile setting in these steps: >

	-- add the infomation for the extension name of specified program
	    files. (*.c, for example) see |ctk_ext_var|
		let b:{g:ctk_ext_var} = 'c'

	-- add a new SetDefaultInfo sentence in a new section:
		" C compilers {{{1
		autocmd c SetDefaultInfo cmd='...' run='...'

	-- add a new SetCompilerInfo after it:
		autocmd c SetCompilerInfo gcc title='...'

all autocmds are in the group "ctk_autocmds". and all lines need add to the
cinfo file |ctk-cinfo-file|.

the format of a info line is: >

 autocmd <filetype> SetCompilerInfo <compiler_name> tags...
<
where filetype is the filetype you want to add compiler info in. compiler_name
is the name of this compiler, this name can be used in modeline.
|ctk-modeline|

all tags can used in your modeline, and its format is the same with the tags
in modeline. 
|ctk-modeline-format|

							       *ctk-cinfo-tag*

tag is just like variables in your program. there are four base tags in ctk,
if you didn't define then, they will defined by ctk. they are "cmd", "run",
"input" and "output". these tags use to create the command line to invoke
compiler and the program compiled.

the default define of base tags are:
>
    SetDefaultInfo cmd=':echo "Nothing Done"'
	    \ run=':echo "Nothing Done"'
	    \ input='%:.' output='%:t:r'
<

this is a long whole line. the leading backslash (\) is a line-continue sign.
you must add C sign in 'cpoptions' to support it. |cpo-C|


								*ctk-cmd-type*
							    *ctk-commmand-tag*

the tag named cmd and run are command tag, they are the template of command
line.  they has two type: shell-command type and vim-command type. if it begin
with ':', it will be a vim-command, if it begin with '!' or anyother things.
it will be parsed to a shell-command.

in cmd=':echo "Nothing Done"', the cmd tag is a vim-command tag, it just print
a line in vim |:echo|.

in run='!$output' or run='$output', the run tag is a shell-command tag. it
will be run in a shell.

							     *ctk-placeholder*

the $output in run tag is a placeholder. it will be replaced by the value of
tag has name 'output' (or entry_output in a command tag, see 'ctk-entry-tag'
below).

you can also use ${q-output} for a placeholder, that means the value will be
quoted, just like <q-args> sign of :command |<a-args>|.


							       *ctk-entry-tag*

tags can be defined in several entries. a entries is used as a trigger word
after :CC or :RUN command. see |:CC|, |:RUN|. such as :CC debug, 'debug' is a
entries. if you press :CC! directly, the empty entry will be used.

  - if tagname isn't include the underline (_), it will be a simple tag, a
    simple tag has a empty entry. e.g.  cmd, run, input and output. if you
    just press :CC to invoke compiler. the empty entry command tag "cmd" will
    be parsed and runed. if you just press :RUN to invoke the program you
    compiled. the empty entry command tag "run" will be parsed adn runed.

  - if you use any trigger word to invole compiler and program. e.g. use
    'foobar'.  it will be used as a entry. if you press :CC foobar, the
    command tag foobar_cmd will be parsed. if ctk can't find foobar_cmd in a
    info line, the cmd will be parsed.

in a word: tags has a format as [<entry>_]<tagname>, if you ignore entry, the
empty will be used, entry used to config :CC or :RUN command.

ctk use this turn to find the tags:
  - first, find it in info line;
  - then find it in 'default' (defined by :SetDefaultInfo);
  - if it still can't be found, ctk find the simple tag in info line;
  - and in default at last.

if ctk still can't find it, ctk will remain it as it is.


							  *ctk-filename-parse*

ctk will change any thing begin with % and # into current file name and
alternative file name. |:_%:| e.g. input='%:.' when parse command line, input
will be parsed in current file name, base to current directory.
|cmdline-special|.


the SetCompilerInfo command accepts a compile name and several attribute.
there are two attributes support: simple attribute (just comtain ansi word in
attribute name) and combine attribute (specname_attrname). there are several
attr examples: >
 input='%:.' output='%:t:r' asm_output='$output.asm'
<
you can use anything that isn't appeared in attributes to replace "'", e.g: >
 input=+%:.+ input=#%:.# ...
<
use space to separate each attributes.

a example: (you can find this in the default cinfo file)
>
 au FileType c let b:{g:ctk_ext_var} = "c"

 au FileType c SetDefaultInfo
    	\ cmd='!$cc $input $flags -o $output'
    	\ run='!$output' input='%:.' output='%:t:r'
    	\ asm_run=':if bufname('^$output$') == '' | sp +drop $output | endif | checkt'
	\ asm_output='$output.asm'
    	\ debug_run='!gdb -q $output'

 au FileType c SetCompilerInfo gcc
    	\ title='GNU C Compiler'
    	\ cc='gcc' flags='-Wall' cmdmap='<m-c>' runmap='<m-1> <f5>'
    	\ asm_title='GCC to ASM' asm_flags='-S $flags'
    	\ debug_title='GCC with debug' debug_flags='-ggdb $flags'

<

the first line tells ctk.txt c language file's extension name is "c", some
times the 'filetype' option's value is the extension name, but sometimes not.
in this case, you can also put this sentence into SetDefaultInfo: >
 aut FileType c SetDefaultInfo ft_ext='c'
<
in this case, you can't use g:ctk_ext_var, just use its default value.

the second line is the default line, if ctk.vim can't find infomations in
current info line (e.g. cmd), it will find the current default line. so you
can put some common infomations at here.

you needn't clear all infomations (use SetCompilerInfo!) before you set it. if
the filetype of buffer is changed, all info line will be deleted. if you want
didn't delete, add alternative filetype into autocmd pattern
|autocmd-patterns|.

the first two line of default info line defined the four simple tag: cmd, run,
input and output. it says, if you want to compile a C program. the command
line is "$cc $input $flags -o $output", it combine with four tags. so as run.

the input and output tags can be omited, because this is the orignal value of
default.

the asm_run is a entry tag |ctk-entry-tag|. you can use ":RUN asm" to invoke
it.  in this case, all tags in this tag will be treat a entry tag. for
asm_run, ":if bufname('^$output$') == '' | sp +drop $output | endif |
checkt'", the $output wil be treated as asm_output. if ctk can't find
asm_output, it will find output, and if it can't find output, it will use
built-in value, since it's a base tag. if the asm_run = '$foobar', and you
don't define foobar and asm_foobar in your info line, it will be remain as it
is ($foobar).

next two line defines asm_output and debug_run. in asm_output, it used
$output, this will be parsed into a simple tag $output, not the entry tag.


								*ctk-name-tag*

the info line support the remain infomations.  the first field of info line is
the name of info field. that will be used in modeline. |ctk-modeline|. you can
use ListCompiler to display it.

							       *ctk-title-tag*

you can add a title tag in info line. it will be displayed when you compile
your program. if it is a entry one, it will displayed when you compile your
program with specified entry.

next line set $cc to gcc, and flags to -Wall, so the cmd will be parsed to
'!gcc %:. -Wall -o %:t:r', where %:. and %:t:r will be replaced by filename.
|:_%:|.

							      *ctk-hotkey-tag*
the next two tags is hotkey tags. hotkey tags named "cmdmap" and "runmap", you
can define then with "modelist:key", just like cmdmap="n:<f1>", means when you
press <F1> in normal mode, the compiler will be invoked. runmap="<F2>" means
when you press <F2> in any mode, your program will run. (if it hasn't compile
yet, the compiler will be invoked before). you can define multikey in a hotkey
tag. just lile runmap='<m-1> <f5>', omited mode means all mode as far as ctk
can defined (that is not defined by other scripts or user yet). you can see
the defined key in ListCompiler. |:ListCompiler|

you can't contain child tags in hotkey tags.

==============================================================================
OPTION								*ctk-options*

these are ctk's option and default value, you can change them, and put it in
your .vimrc file |.vimrc| to modified ctk.

					       *ctk-autofname* *ctk_autofname*
>
 let g:ctk_autofname = 'strftime("%Y-%m-%d")."-".idx'

<			this is used to set the auto generated fname, the
			value of this will be calculated then generated the
			new fname. set it to empty will disabled the function
			of auto rename the noname file: >

				let g:ctk_autofname = ''
<
							      *ctk_cinfo_file*
>
 let g:ctk_cinfo_file = '.compiler_info'
<			this is the default cinfo file path, based on
			'runtimepath'. it will be loaded when you start the
			ctk |StartCTK|.

							       *ctk_defoutput*
>
 let g:ctk_defoutput = './output'
<			this is the default output name when the file name is
			generated by CTK.

								 *ctk_ext_var*
>
 let g:ctk_ext_var = 'ft_ext'
<			this is the default variable name of current file
			extname. it should b:ft_ext when the g:ctk_ext_var =
			'ft_ext'

			design this is because that maybe other plug-in
			supports some feature to offer the extension name of
			file, so you can simply set g:ctk_ext_var to the name
			of that variable.

			in ctk, first it checks whether 'b:'.g:ctk_ext_var
			exists, when g:ctk_ext_var defaults to 'ft_ext', this
			variable turns to 'b:ft_ext'.  when this variable
			exists, ctk use it to create the file name of new
			program.

			otherwise, ctk checks 'sua' option. (see :h 'sua') if
			it is non-empty, at last it used &ft directly. if all
			option is empty, ctk will not rename the new program
			file.

			so, g:ctk_ext_var is nothing about finding source
			window, it only used to create the new name of
			program.

								 *ctk_tempdir*
>
 let g:ctk_tempdir = './noname'
<			this is the temporary folder to contain noname file.

								 *ctk_execprg*
>
 if has('win32')
     let g:ctk_execprg = executable('vimrun') ?
 		\ 'start vimrun $exec' : 'start $exec'
 elseif has('unix') && has('gui_running')
     let g:ctk_execprg = 'xterm -e"$exec; '.
 		\ 'read -s -n1 -p''press any key to continue...''"'
 else
     let g:ctk_execprg = ''
 endif
<
			the g:ctk_execprg is the system-specified command
			template. when you run your program. this template
			will be used to build the command send to shell.
			the $exec will be replaced by the program that will
			executed.

==============================================================================
CONTACT								*ctk-contact*

if you find any bug, please contact me <weasley_wx AT qq DOT com>. and i will
very happy if you can send me some advise or new-feature :-)


vim: tw=79:ts=8:ft=help:norl:
.compiler_info	[[[1
97
" compiler infomation file
" Maintainer: StarWing
" Last Change: 2009-05-10 13:41:26
" need ctk {{{1

if !exists('g:loaded_ctk')
    finish
endif
let s:cpo_save = &cpo
set cpo&vim

function! ctk:split_open(fname) " {{{1
    if bufwinnr(a:fname) == -1
	if exists('*fnameescape')
	    let fname = fnameescape(a:fname)
	else
	    let fname = escape(a:fname, ' \|')
	endif

	" for redir in s:exec_cmd()
	let output=''
	redir END
	silent exec "sp " . fname
	
	" fix MiniBufExplorer bug
	let nr = bufnr('^-MiniBufExplorer-$')

	if nr != -1
	    call setbufvar(nr, '&wfh', 1)
	endif
    endif
    exec bufwinnr(a:fname).'wincmd w'
    let ar_save = &ar
    set ar
    checktime
    let &ar = ar_save
endfunction

" Common default info {{{1

    au FileType c,cpp SetDefaultInfo
	    \ cmd='!$cc $input $flags -o $output'
	    \ run='!$output' input='%:.' output='%:t:r'
	    \ asm_run=":call ctk:split_open(${q-output})"
	    \ asm_output='$output.s'
	    \ debug_run='!gdb -q $output'

if has('unix')
    au FileType c,cpp SetDefaultInfo asm_output='$output.s'
else
    au FileType c,cpp SetDefaultInfo asm_output='$output.asm'
endif


" filetype C {{{1

au FileType c let b:{g:ctk_ext_var} = 'c'
au FileType c SetCompilerInfo gcc
    	\ title='GNU C Compiler'
    	\ cc='gcc' flags='-Wall' cmdmap='<m-c>' runmap='<m-1> <f5>'
    	\ asm_title='GCC to ASM' asm_flags='-S $flags'
    	\ debug_title='GCC with debug' debug_flags='-ggdb $flags'

au FileType c SetCompilerInfo vc6  
    	\ title='Microsoft Visual C'
    	\ cc='cl' flags='-W4' runmap='<m-2> <f6>'
    	\ asm_title='Microsoft VC to ASM' asm_flags='/FAs $flags'

" filetype C++ {{{1

au FileType cpp let b:{g:ctk_ext_var} = 'cpp'
au FileType cpp SetCompilerInfo g++
    	\ title='GNU Compiler Collection - C++'
    	\ cc='g++' flags='-Wall' cmdmap='<m-c>' runmap='<m-1> <f5>'
    	\ asm_title='G++ to ASM' asm_flags='-S $flags'
    	\ debug_title='G++ with debug' debug_flags='-ggdb $flags'

au FileType cpp SetCompilerInfo vc++
    	\ title='Microsoft Visual C++'
    	\ cc='cl' flags='-W4' runmap='<m-2> <f6>'
    	\ asm_title='Microsoft VC++ to ASM'
    	\ asm_flags='/FAs $flags'

" }}}1
" filetype ruby {{{1

au FileType ruby let g:{ctk_ext_var} = 'rb'
au FileType ruby SetCompilerInfo ruby
	    \ title='Ruby 1.9.1 - Matz'
	    \ cmd='ruby $flags $input'
	    \ run='ruby $input'
	    \ flags='-wc' debug_flags='-rdebug $flags'

" terminational works {{{1
let &cpo = s:cpo_save
unlet s:cpo_save " }}}1
" vim: ff=unix ft=vim fdm=marker sw=4 ts=8 sta
