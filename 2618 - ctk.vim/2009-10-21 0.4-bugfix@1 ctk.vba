" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/ctk.vim	[[[1
888
" Script Nmame: code toolkit
" File Name:    ctk.vim
" Author:       StarWing
" Version:      0.4
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
    " =================================================={{{2
    let g:loaded_ctk = 'v0.4'

    " options {{{2
    function! s:defopt(opt, val)
	if !exists(a:opt) | let {a:opt} = a:val | endif
    endfunction

    call s:defopt('g:ctk_autofname', 'strftime("%Y-%m-%d")."-".idx')
    call s:defopt('g:ctk_autostart', 1)
    call s:defopt('g:ctk_cinfo_file', '.compiler_info')
    call s:defopt('g:ctk_cmdenc', 'cp936')
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

    " commands & menus {{{2
    command! -bar -bang StartCTK call s:start_ctk('<bang>')
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

            au FileType * call s:call('s:begin_setinfo', [])
            exec 'run '.g:ctk_cinfo_file
            au FileType * call s:call('s:end_setinfo', [])

            au FileType * call s:call('s:set_fname', [])
        augroup END

        map gc <C-\><C-N>:<C-U>exec v:count."CC!"<CR>
        map gC <C-\><C-N>:<C-U>exec v:count."RUN"<CR>

        if a:bang == '!'
            call s:call('s:delete_ci', [])
            filetype detect
        endif
    endfunction

    function! s:stop_ctk() " {{{3
        unlet! b:ctk_fname
        call s:delete_ci()
        silent! au! ctk_autocmds
        silent! unmap gc
        silent! unmap gC
    endfunction

    function! s:echoerr(msg) " {{{3
        echohl ErrorMsg
        echomsg 'CTK: '.a:msg
        echohl NONE
    endfunction

    function! s:call(funcname, args) " {{{3
        if !exists('s:load_all')
            exec 'so '.s:sfile
            unlet s:sfile
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

        call s:echoerr("command can't use, because ctk isn't avaliable")
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

    " }}}2
    " =================================================={{{2
    if g:ctk_autostart
        StartCTK
    else
        let &cpo = s:cpo_save
        unlet s:cpo_save

        finish
    endif " }}}2
endif

let s:load_all = 1

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

let s:pat_cmd_is_shell = '\v^[^:]|^:!|^:sil%[ent]\s+!'
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
let s:pat_run_direct = '#*RUN_DIRECT'
let s:pat_shellcmdtitle = '^!\=\zs.\{-}\ze\(\s\|$\)'

" }}}2

" ============================================================
" utility functions {{{1

" tricks to get command-style arglist {{{2
command! -nargs=* CTKGetEscapedList let l:args = [<f-args>]
function! s:get_escaped_list(str)
    exec 'CTKGetEscapedList '.a:str
    return args
endfunction

" command to unmap info {{{2
command! -nargs=1 CTKUnMapCompilerInfo
            \ exec join(map(<args>.unmap,
            \ 'v:val[0]."unmap ".v:val[1:]'), '|')

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
    let default = get(b:{s:ci}, 'default', {})
    let info = get(get(b:{s:ci}, 'status', {}), 'info', {})
    let key = (a:entry == '' ? a:key : a:entry.'_'.a:key)

    return get(info, key,
                \ get(default, key, a:entry == '' ?
                \ get(s:def_attr, key, a:default) :
                \ get(info, a:key,
                \ get(default, a:key,
                \ get(s:def_attr, a:key, a:default)))))
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

function! s:delete_ci() " {{{2
    if &ft ==? 'decho' | return | endif
"    call Dfunc('s:delete_ci()')
"    call Decho("delete b:compiler_info for buffer ".bufnr(""))

    unlet! b:{g:ctk_ext_var}
    if exists('b:compiler_info')
        for info in get(b:compiler_info, 'list', [])
            CTKUnMapCompilerInfo info
        endfor
        unlet b:compiler_info
    endif

"    call Dret('s:delete_ci')
endfunction

function! s:sub_info(info) " {{{2
"    call Decho('let '.get(a:info, 'name', 'cur_info').'['.submatch(1).'] '.
                \ submatch(2).'= "'.submatch(4).'"')

    let val = submatch(2) == '+' ? s:get_entry_val('', submatch(1), '') : ''
    let val = val == '' ? '' : val.' '
    let a:info[submatch(1)] = val.submatch(4)
endfunction

function! s:expand_var(entry, default) " {{{2
    let key = submatch(1) == '' ? submatch(3) : submatch(1)
    let val = s:get_entry_val(a:default ? '' : a:entry, key, submatch(0))

    if submatch(2) ==? 'q-'
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
        if v:version > 702 || v:version == 702 && has('patch111')
            let fname = shellescape(fname)

        " I hope it can work... but maybe you should update your version :-)
        elseif has('win32')
            let fname = '"'.substitute(fname, '"', '""', 'g').'"'
        else
            let fname = "'".fname."'"
        endif
    endif

"    call Dret('s:expand_fname : '.fname)
    return fname
endfunction

function! s:is_run_direct() " {{{2
    return get(get(get(b:{s:ci}, 'status', {}),
                \ 'info', {}), 'cmd', '') =~ s:pat_run_direct
endfunction

function! s:begin_setinfo() " {{{2
    if exists(s:ci_name) && get(b:{s:ci}, 'ft', '') != &ft
        call s:delete_ci()
    endif
endfunction

function! s:need_update() " {{{2
    return !exists(s:ci_name) || get(b:{s:ci}, 'ft', '') != &ft
endfunction

function! s:end_setinfo() " {{{2
    if exists(s:ci_name)
        let b:{s:ci}.ft = &ft
    endif
endfunction

function! s:set_fname() " {{{2
    if g:ctk_autofname == '' | return | endif

    " if we really have a filetype and this type isn't our debug window's
    " and we don't have a filename, and we have a compiler info, set auto
    " name.
    if !exists('b:ctk_fname')
        return &ft != '' && &ft !=? 'decho' && expand('%') == ''
                \ && exists('b:compiler_info') ? s:set_filename() : 0
    endif

"     call Decho("we are in set_fname(), buffer ".bufnr(""))
"     call Decho("we".(exists("b:compiler_info") ? "" : " don't")." have b:compiler_info")

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

function! s:set_filename() " {{{2
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

function! s:save_source() " {{{2
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

function! s:make_info(info) " {{{2
    if expand('%') == '' | return | endif
"    call Dfunc('s:make_info(info = '.a:info.name.')')
    let cur_info = copy(a:info)
    for var in ['cmd', 'run', 'un']
        silent! unlet! cur_info[var.'map']
    endfor

    if exists('b:ctk_fname') && !b:ctk_fname[2]
"        call Decho('filename is an autogenerated name, use defoutput')
        let cur_info.output = g:ctk_defoutput
"    else | call Decho('filename is user-defined')
    endif

    " analyze the modeline to modifie the info
    if &modeline && &mls != 0
        let last = line('$')
        let old_info = get(b:{s:ci}.status, 'info', {})
        let b:{s:ci}.status.info = {}
        if last <= &mls * 2
            call s:read_modeline(cur_info, 1, last)
        else
            call s:read_modeline(cur_info, 0, &mls)
            call s:read_modeline(cur_info, last - &mls, last)
        endif
        let b:{s:ci}.status.info = old_info
    endif

"     call Dret('s:make_info : '.string(cur_info))
     return cur_info
endfunction

function! s:make_cmd(cmd, entry, use_native) " {{{2
"    call Dfunc('s:make_cmd(cmd = "'.a:cmd.'", entry = "'.a:entry.'")')

    let cmd = s:process_placeholder(a:cmd, a:entry)

    " this is just for "run"
    " defaultly, we use system toolkit to compile a program. that means, we
    " will execute a program directly, let OS to choose the right one to exec.
    " that usually the one in system folder (some places like /usr/bin).
    "
    " but, we should run currently generated program in "run", so we must
    " include the current folder for finding the program.
    if a:use_native && cmd[0] !~ ':'
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

function! s:exec_cmd(cmdarg) " {{{2
"    call Dfunc('s:exec_cmd(cmdarg = '.a:cmdarg.')')

    let cmd = a:cmdarg
    let cmd_is_shell = (cmd =~ s:pat_cmd_is_shell)

    if has('win32') && &enc != g:ctk_cmdenc && exists('*iconv')
                \ && cmd_is_shell
        let cmd = iconv(cmd, &enc, g:ctk_cmdenc)
    endif

    if cmd[0] == ':'
        if has('gui_running') || !cmd_is_shell
            redir => g:ctk_redir | silent! exec cmd | redir END
        else
            redir => g:ctk_redir | exec cmd | redir END
            if &term != 'linux' | redraw! | endif
        endif
        let output = g:ctk_redir
    else
        silent! let output = system(cmd[0] == '!' ? cmd[1:] : cmd)
    endif

    let output = matchstr(output, s:pat_execoutput)
"    call Dret('s:exec_cmd : '.output)
    return output
endfunction

function! s:show_list(info) " {{{2
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

function! s:read_modeline(info, begin, end) " {{{2
"    call Dfunc('s:read_modeline(begin = '.a:begin.', end = '.a:end.')')
    let pos = winsaveview()

    call cursor(a:begin, 1)
    while search(s:pat_modeline, '', a:end) != 0
"        call Decho('find a modeline in line '.line('.'))
        let mlist = matchlist(getline('.'), s:pat_modeline)
        if mlist[1] == '' || a:info.name =~ '^\V'.escape(mlist[1], '\')
            call substitute(mlist[2], s:pat_info_var,
                        \ '\=s:sub_info(a:info)', 'g')
        endif
    endwhile

    call winrestview(pos)
"    call Dret('s:read_modeline')
endfunction

function! s:process_placeholder(cmd, entry) " {{{2
"    call Dfunc('s:process_placeholder(cmd = '.a:cmd.', entry = '.a:entry.')')
    let cmd = a:cmd

    let cmd = substitute(cmd, s:pat_cmdtag, '\=s:expand_var(a:entry, 0)', 'g')
    if a:entry != ''
        let cmd = substitute(cmd, s:pat_cmdtag, '\=s:expand_var(a:entry, 1)', 'g')
    endif
    let cmd = substitute(cmd, s:pat_filespec_nonescape,
                \ '\=s:expand_fname(submatch(0), cmd[0])', 'g')
    let cmd = substitute(cmd, s:pat_filespec_escape, '', 'g')

"    call Dret('s:process_placeholder : '.cmd)
    return cmd
endfunction " }}}2

" ============================================================
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

function! s:list_compiler(name, idx) " {{{1
    " offer index, just show the speciafied info
    if a:0 != 0 && a:1 != 0
        return s:show_list(b:{s:ci}.list[a:1])
    endif

    let current = get(get(b:{s:ci}, 'status', {}), 'info', {})
    if (a:name == '' || a:name ==? 'current') && !empty(current)
        call s:show_list(current)
        return
    endif

    if a:name == '' || a:name ==? 'default' || a:name ==? 'all'
        call s:show_list(get(b:{s:ci}, 'default', {}))
        if a:name ==? 'default' | return | endif
    endif

    if a:name == '' || a:name ==? 'all'
        for info in b:{s:ci}.list
            call s:show_list(info)
        endfor
        return
    endif

    return s:show_list(s:get_info(a:name))
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

    if has_key(def_info, 'extname')
        let b:{g:ctk_ext_var} = def_info['extname']
        unlet def_info['extname']
    endif

"    call Dret('s:set_default_info')
endfunction

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
        if a:bang == '!' 
            call s:delete_ci() 
        else
            call s:list_compiler('all', 0)
        endif
"        return Dret('s:set_compiler_info')
    endif

    " find name and others, mlist = [all, name, infos]
    let mlist = matchlist(a:cmdarg, s:pat_info)

    " add or modify a info
    if mlist[2] != ''
        let info = s:get_info(mlist[1])

        if a:bang == '!'
            let info = {}
        endif

        " add a new info, or clean old info
        if empty(info)
"            call Decho('add a new info')
            let info.name = mlist[1]
            call add(b:{s:ci}.list, info)
        else
"            call Decho('clear old info: '.string(info))
            CTKUnMapCompilerInfo info
            call filter(info, 0)
        endif

        let info.name = mlist[1]
        call substitute(mlist[2], s:pat_info_var, '\=s:sub_info(info)', 'g')

        let info.unmap = []
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
                        call add(info.unmap, mode.mkey[cpos+1:])
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
            CTKUnMapCompilerInfo info
            call remove(b:{s:ci}.list, s:get_idx(info))
        endif

    " list a info
    else
        call s:list_compiler(mlist[1], 0)
    endif

"    call Decho('>> now info = '.string(info))
"    call Dret('s:set_compiler_info')
endfunction

function! s:compile(count, entry, bang) " {{{1
    let ci = b:{s:ci}

    " find source buffer, and save it. if failed (return nonzero), echo
    " message and return 1 (failed, no use in this version)
    " NOTE: we find source in s:find_and_call() function, so we needn't find source
    " again. 
    if !s:save_source() || a:count < 0 || a:count > len(ci)
        redraw | echo 'Nothing Done'
        return 1
    endif

    if a:bang == '!' || !has_key(ci, 'status')
        let ci.status = {}
    endif
    let stat = ci.status

"    call Decho('current changenr = '.changenr().' and stat->changenr = '.get(stat, 'changenr', -1))
    if !s:is_run_direct()
                \ && get(stat, 'changenr', -1) == changenr()
                \ && (a:count == 0 || get(stat, 'idx', -1) == a:count - 1)
                \ && (a:entry == '' || get(stat, 'entry', '') == a:entry)
        redraw | echo 'Buffer no changed, Nothing Done.'
        return 0
    endif

"    call Dfunc('s:compile(count = '.a:count.', entry = '.a:entry.
                \ ', bang = '.a:bang.')')

    " init status variables
    let stat.changenr = changenr()
    let stat.entry = a:entry == '' ? get(stat, 'entry', '') : a:entry
    let stat.idx = a:count == 0 ? get(stat, 'idx', 0) : a:count - 1
    let stat.info = s:make_info(ci.list[stat.idx])

    if s:is_run_direct()
        call s:run(a:count, a:entry, a:bang)
        return 1
    endif

    " "entry" is just something like trigger. you press :CC entry, then
    " "entry" specified commands will be executed.
    redraw | echo 'Compiling ...'
    let ret_val = 1
    let msg = 'Compiling... using '.
                \ s:get_entry_val(stat.entry, 'title', stat.info.name)

    " don't use locale program
    let cmd = s:make_cmd(s:get_entry_val(stat.entry, 'cmd', ''), stat.entry, 0)
    if cmd == '' 
        redraw | echo 'Empty command, Nothing Done.' 
"        call Dret('s:compile')
        return  
    endif

    let res = s:exec_cmd(cmd)

    redraw
    if cmd[0] != ':'
"        call Decho('A shell command')
        let ret_val = v:shell_error

        if has('win32') && &enc != g:ctk_cmdenc && exists('*iconv')
            let res = iconv(res, g:ctk_cmdenc, &enc)
        endif

        cgetexpr [msg, cmd, ''] + split(res, "\<NL>")
                    \ + [stat.info.name.' returned '.ret_val]
        exec v:shell_error == 0 ? 'cwindow' : 
                    \ (res == '' ? 'cclose' : 'copen')

        echo 'Compile' (v:shell_error ? 'Fail' : 'Successful')

    elseif res != ''
"        call Decho('A exec command')
        echo msg."\n".cmd."\n\n".res
    endif

"    call Dret('s:compile : '.ret_val)
    return ret_val
endfunction

function! s:run(count, entry, bang) " {{{1
"    call Dfunc('s:run(count = '.a:count.', entry = '.a:entry.
                \ ', bang = '.a:bang.')')

    let bufnr = bufnr('%')
    if !s:is_run_direct() && s:compile(a:count, a:entry, a:bang) | return 1 | endif
    exec bufwinnr(bufnr).'wincmd w'

    " use locale program
    let cmd = s:make_cmd(s:get_entry_val(a:entry, 'run', ''), a:entry, 1)

    if cmd == '' 
        redraw | echo 'Empty command, Nothing Done.' 
"        call Dret('s:run')
        return
    endif

    if cmd[0] != ':'
        let cmd = cmd[0] == '!' ? cmd[1:] : cmd
        if g:ctk_execprg != ''
            let cmd = substitute(g:ctk_execprg, s:pat_exectag,
                        \ escape(cmd, '\'), 'g')
        endif
        let cmd = (has('gui_running') ? ':silent !' : ':!').cmd
    endif

    let res = s:exec_cmd(cmd)

    if &term != 'linux'
        if res !~ '^\_s*$' && !has('gui_running')
            redraw
            for line in split(res, '\n\|\r\|\r\n')
                echomsg line
            endfor
        endif
    endif

"    call Dret('s:run')
endfunction

" ======================================================{{{1

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: ff=unix ft=vim fdm=marker sw=4 ts=8 et sta nu
doc/ctk.txt	[[[1
616
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
:StartCTK[!]
:StopCTK
		start to use CTK, or stop CTK. or you can use StartCTK to
		refresh your cinfo file after you modified it.

		with '!', StartCTK will also reload and refresh current
		compiler info.

:[N]CC[!] {entry}
		this command is used to invoke specified compiler. [N] is your
		compiler index. you can see it in :ListCompiler command
		|:ListCompiler|. {entry} is the command you invoke, in this
		way you can use any command in a single info line.  it will be
		discussed in |ctk-entry-tag|.
		
		if the compiler command is executed a extern program, the
		quickfix window will open if compiler return nonzero.
		otherwise, the compiler output will display, if any.

		With [!] and you doesn't have an entry, it uses a empty entry,
		that is, invoke "cmd" tag in your info line. and it invokes
		the entry you used last time without [!].

									*:RUN*
:[N]RUN[!] {entry}
		run the program generated by compiler. if you change your
		source code after compiling or the index [N] or {entry} isn't
		the same with the ones when you invoke compiler, the compiler
		will be invoked first to compile your code again.

		[!] will pass to :CC, if need.

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

							   *:SetExtensionName*
:SetExtensionName {name}
		set the default extension name for specified filetype. ctk.vim
		uses this to generate name for new buffer. put it into your
		cinfo file. (with autocmd, e.g.: >

		    autocmd ruby SetExtensionName rb
<
		this will set ruby program's default extension name to 'rb',
		so ctk.vim will generate name "noname.rb" for ruby file,
		instead of "noname.ruby" (that's default, use 'filetype' as
		extension name).


							     *:SetDefaultInfo*
:SetDefaultInfo[!] {attr}="value" {attr2}=!value2! ...

		set default info for this filetype. add this command in your
		cinfo file if this is your default value, or it won't be saved
		after you leave Vim!

		you can use anything to replace '"' or '!' to around values.

		With [!] it delete the old default value of this buffer.
		Without [!] it just modify (add or edit) the tags in current
		default values.

							    *:SetCompilerInfo*
:SetCompilerInfo
		just like :ListCompiler all, list all info lines of current
		buffer.

:SetCompilerInfo!
		delete all info lines of current buffer.

:SetCompilerInfo {name}
		list compiler info of {name}, just like :ListCompiler {name}.

:SetCompilerInfo! {name}
		delete specified name from current buffer.

:SetCompilerInfo {name} {attr}="value" {attr2}=!value2|
		same as SetDefaultInfo. but this is for setting attributes for
		specified compiler. see |ctk-set-compiler-info|

:SetCompilerInfo! {name} {attr}="value" {attr2}=!value2|
		same as SetCompilerInfo without [!] above, but delete the
		specified compiler info first.


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

ctk.vim will use several way to detect the extension name of your source.
first it read the value of variable b:{g:ctk_ext_var}  |curly-braces-names|
you can set this variable manually, or drop it into your cinfo file: >
 autocmd FileType ruby let b:{g:ctk_ext_var} = 'rb'
>
or simply use SetExtensionName command (in cinfo file or manually): >
 autocmd FileType ruby SetExtensionName rb
<
see |ctk-fileext-define| for details.

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
the format of modeline is: >

 com-start cc[-compiler_name]: <tag> [+]= ?any thing? com-end
<
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
		autocmd FileType c let b:ft_ext = "c"
<
ft_ext is the value of option ctk_ext_var. the better way is:
>
		autocmd FileType c let b{g:ctk_ext_var} = "c"
<
or the best way: >
		autocmd FileType c SetExtensionName c
<

							       *ctk-info-line*

that file is combined with several autocmds |autocmd|. you can add a new
compile setting in these steps: >

	-- add the infomation for the extension name of specified program
	    files. (*.c, for example) see |ctk-fileext-define|
		autocmd FileType c SetExtensionName c

	-- add a new SetDefaultInfo sentence in a new section, [!] for delete
	   the old ones:

		autocmd FileType c SetDefaultInfo! cmd='...' run='...'

	-- maybe some platform settings, use if...else... and SetDefaultInfo
	   to modify current default value:

		if has('win32')
		    autocmd FileType c SetDefaultInfo ....
		elseif has('unix')
		    autocmd FileType c SetDefaultInfo ....
		else
		    autocmd FileType c SetDefaultInfo ....
		endif

	-- add a new SetCompilerInfo after it:

		autocmd FileType c SetCompilerInfo gcc title='...'

all autocmds are in the group "ctk_autocmds". and all lines need add to the
cinfo file |ctk-cinfo-file|.

the format of a info line is: >

 autocmd FileType <filetype> SetCompilerInfo <compiler_name> tags...
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

							      *ctk-run-direct*
if cmd is the string "#RUN_DIRECT", then when you press cmd hotkey, the
compiler will not run, and the "run" tag will be run instead.

in run='!$output' or run='$output', the run tag is a shell-command tag. it
will be run in a shell.

if you use |:redirect| or |:silent| in cmd or run, you must close ctk's
redirect first. just like this:
>
    function! Foobar()				    
	redir END
	silent! foobar
	redir g:ctk_redir
    endfunction

    autocmd FileType foobar SetCompilerInfo foo cmd=':call Foobar()'

<
this example is useless, but it shows how to use silent/redir command in cmd
or run tags.

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
 au FileType c SetExtensionName c

 au FileType c SetDefaultInfo!
    	\ cmd='!$cc $input $flags -o $output'
    	\ run='!$output' input='%:.' output='%:t:r'
    	\ asm_run=':pedit $output.asm'
	\ asm_output='$output.asm'
    	\ debug_run='!gdb -q $output'

 au FileType c SetCompilerInfo! gcc
    	\ title='GNU C Compiler'
    	\ cc='gcc' flags='-Wall' cmdmap='<m-c>' runmap='<m-1> <f5>'
    	\ asm_title='GCC to ASM' asm_flags='-S $flags'
    	\ debug_title='GCC with debug' debug_flags='-ggdb $flags'

<

the first line tells ctk.txt c language file's extension name is "c", some
times the 'filetype' option's value is the extension name, but sometimes not.
instead of use SetExtensionName, you can also put this sentence into
SetDefaultInfo: >

 aut FileType c SetDefaultInfo extname='c'
<

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
							       *ctk_autostart*
>
 let g:ctk_autostart = 1
<
			if you don't want CTK auto start when you enter Vim,
			set it to 0, it's default 1, means auto start.

							      *ctk_cinfo_file*
>
 let g:ctk_cinfo_file = '.compiler_info'
<			this is the default cinfo file path, based on
			'runtimepath'. it will be loaded when you start the
			ctk |StartCTK|.

								  *ctk_cmdenc*
>
 let g:ctk_cmdenc = "cp936"
<
			this option is avaliable in win32 only. because the
			encoding of win32 console is not utf-8, if you use
			utf-8 for 'encoding'. the command line you invoke will
			incorrect. set this to the encoding of win32 console
			to solve this problem.

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
79
" compiler infomation file
" Maintainer: StarWing
" Last Change: 2009-05-10 13:41:26
" need ctk {{{1

if !exists('g:loaded_ctk')
    finish
endif
let s:cpo_save = &cpo
set cpo&vim

" Common default info {{{1

au FileType c,cpp SetDefaultInfo!
	\ cmd='!$cc $input $flags -o $output'
	\ run='!$output' input='%:.' output='%:t:r'
	\ asm_run=":pedit $output"
	\ asm_output='$output.s'
	\ debug_run='!gdb -q $output'

if has('unix')
    au FileType c,cpp SetDefaultInfo asm_output='$output.s'
else
    au FileType c,cpp SetDefaultInfo asm_output='$output.asm'
endif


" filetype C {{{1

au FileType c SetExtensionName c
au FileType c SetCompilerInfo! gcc
    	\ title='GNU C Compiler'
    	\ cc='gcc' flags='-Wall' cmdmap='<m-c>' runmap='<m-1> <f5>'
    	\ asm_title='GCC to ASM' asm_flags='-S $flags'
    	\ debug_title='GCC with debug' debug_flags='-ggdb $flags'

au FileType c SetCompilerInfo! vc6  
    	\ title='Microsoft Visual C'
    	\ cc='cl' flags='-W4' runmap='<m-2> <f6>'
    	\ asm_title='Microsoft VC to ASM' asm_flags='/FAs $flags'

" filetype C++ {{{1

au FileType cpp SetExtensionName cpp
au FileType cpp SetCompilerInfo! g++
    	\ title='GNU Compiler Collection - C++'
    	\ cc='g++' flags='-Wall' cmdmap='<m-c>' runmap='<m-1> <f5>'
    	\ asm_title='G++ to ASM' asm_flags='-S $flags'
    	\ debug_title='G++ with debug' debug_flags='-ggdb $flags'

au FileType cpp SetCompilerInfo! vc++
    	\ title='Microsoft Visual C++'
    	\ cc='cl' flags='-W4' runmap='<m-2> <f6>'
    	\ asm_title='Microsoft VC++ to ASM'
    	\ asm_flags='/FAs $flags'

" }}}1
" filetype ruby {{{1

au FileType ruby SetExtensionName rb
au FileType ruby SetCompilerInfo! ruby
	    \ title='Ruby 1.9.1 - Matz'
	    \ cmd='ruby $flags $input'
	    \ run='ruby $input'
	    \ flags='-wc' debug_flags='-rdebug $flags'

" filetype python {{{1

au FileType python SetExtensionName py
au FileType python SetCompilerInfo! python
	    \ title='Python' input='%:.'
	    \ cmd="#RUN_DIRECT"
	    \ run="python $flags $input"
	    \ flags='-O' debug_flags='-d'

" terminational works {{{1
let &cpo = s:cpo_save
unlet s:cpo_save " }}}1
" vim: ff=unix ft=vim fdm=marker sw=4 ts=8 sta
