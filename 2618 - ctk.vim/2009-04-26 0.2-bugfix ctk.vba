" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/ctk.vim	[[[1
702
" Script Nmame: code toolkit
" File Name:    ctk.vim
" Author:       StarWing
" Version:      0.2
" Last Change:  2009-03-30 18:20:00
" Note:         see :ctk for details
" ==========================================================
" load once {{{1

if v:version < 700
    echomsg "ctk.vim requires Vim 7.0 or above."
    finish
endif

let s:cpo_save = &cpo
set cpo&vim

scriptencoding utf-8
if !exists('g:loaded_ctk')
    let g:loaded_ctk = 'v0.2'

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
                    \ 'read -s -n1 -p''press any key to continue...''"')
    else
        call s:defopt('g:ctk_execprg', &sh.' $exec')
    endif

    delfunc s:defopt

    " commands {{{2
    command! -bar StartCTK call s:start_ctk()
    command! -bar StopCTK call s:stop_ctk()
    command! -bar EditCompilerInfo exec 'drop '.globpath(&rtp, g:ctk_cinfo_file)

    command! -nargs=* -complete=customlist,ctk:info_name_complete -bar -count=0 
	    \ ListCompiler call ctk:list_compiler(<q-args>, <count>)
    command! -nargs=* -complete=custom,ctk:info_item_complete -bang
	    \ SetCompilerInfo call ctk:set_compiler_info(<q-args>, '<bang>')
    command! -nargs=+ -complete=custom,ctk:info_item_complete
            \ SetDefaultInfo call ctk:set_default_info(<q-args>)
    command! -nargs=+ -complete=custom,ctk:info_item_complete -count=0
	    \ AddFlags call ctk:add_flags(<q-args>, <count>)

    command! -nargs=? -bar -count=0 CC call ctk:compile(<count>, <q-args>)
    command! -nargs=? -bar -count=0 RUN call ctk:run(<count>, <q-args>)

    map <Plug>CTK_compile :<C-U>call ctk:compile(v:count)<CR>
    imap <Plug>CTK_compile <ESC><Plug>CTK_compile
    map <Plug>CTK_run :<C-U>call ctk:run(v:count)<CR>
    imap <Plug>CTK_run <ESC><Plug>CTK_run

    " menus {{{2

    amenu &Tools.&CTK.&Start :StartCTK<CR>
    amenu &Tools.&CTK.&Stop  :StopCTK<CR>
    amenu &Tools.&CTK.-Sep- :
    amenu <silent> &Tools.&CTK.&Add\ a\ modeline :exec 'AddFlags '.(has('gui_running') ? inputdialog("Please input the text in modeline:", "flags += ''") : input("Modeline:", "flags += ''"))<CR>
    amenu &Tools.&CTK.&List\ All\ Compiler :ListCompiler all<CR>
    amenu &Tools.&CTK.&Compile :CC<CR>
    amenu &Tools.&CTK.&Run :RUN<CR>

    " start and stop ctk {{{2
    let s:sfile = expand('<sfile>')

    function! s:start_ctk()
        augroup ctk_autocmds
            au!
            au BufFilePost * unlet! b:ctk_generated_name
            au FileType * unlet! b:{g:ctk_ext_var}
            au FileType * unlet! b:compiler_info
            exec 'run '.g:ctk_cinfo_file

            au FileType * if g:ctk_autofname != '' && (exists('b:compiler_info')
                        \ || exists('b:ctk_generated_name'))
                        \|     call ctk:set_filename() 
                        \| endif

            au FuncUndefined * if expand('<afile>')[:3] == 'ctk:'
                        \|     exec 'so '.s:sfile
                        \| endif
        augroup END
        map gc <Plug>CTK_compile
        map gC <Plug>CTK_run
    endfunction

    function! s:stop_ctk()
        au! ctk_autocmd
        unmap gc
        unmap gC
    endfunction

    " }}}2

    StartCTK
    finish
endif

" }}}1
" functions {{{1

" some inner variables {{{2

let s:ci = 'compiler_info'
let s:ci_name = 'b:'.s:ci
" patterns: info-variables, modeline-variables and modeline patterns
let s:pat_infvar = '\v(\w+)\s*\=\s*(\S)(.{-})\2'
let s:pat_mlvar = '\v(\w+)(\s*(\+)=\=\s*(\S)(.{-})\4)='
let s:pat_modeline = '\v<cc%(-([^:]*))=:\s*(.*)'
" some default attr
let s:def_attr = {'input': '%:.', 'output': '%:t:r'}

" tricks to get command-style arglist {{{2
command! -nargs=* CTKGetEscapedList let l:args = [<f-args>]
function! s:get_escaped_list(str)
    exec 'CTKGetEscapedList '.a:str
    return args
endfunction

function! ctk:set_filename() " {{{2
    if &ft == '' && expand('%') != '' && exists('b:ctk_generated_name')
        silent! noau 0f
        unlet b:ctk_generated_name
        call Decho('s:set_filename: delete the fname now!')
    endif
    if &ft == 'Decho' | return | endif
    if &ft == '' || &bt != '' | return | endif
    call Dfunc('s:set_filename()')

    if exists('b:'.g:ctk_ext_var) | let ext = b:{g:ctk_ext_var}
    elseif &sua != '' | let ext = &sua[1:]
    elseif &ft != '' | let ext = &ft
    else | return
    endif

    call Decho('ext = "'.ext.'"')
    let tempdir = get(b:, 'ctk_tempdir', g:ctk_tempdir)
    if !isdirectory(tempdir) && exists('*mkdir')
        call mkdir(tempdir, 'p')
    endif
    if !isdirectory(tempdir)
        let tempdir = '.'
    endif
    let tempdir = fnamemodify(tempdir, ':p')
    call Decho('tempdir = "'.tempdir.'"')

    if exists('b:ctk_tempdir')
        let b:ctk_tempdir = tempdir
    else
        let g:ctk_tempdir = tempdir
    endif

    let fname = get(b:, 'ctk_autofname', g:ctk_autofname)
    if !exists('g:ctk_idx')
        let g:ctk_idx = 1
    endif
    let idx = g:ctk_idx
    while filereadable(tempdir.'/'.eval(fname).'.'.ext)
        let idx += 1
    endwhile
    let g:ctk_idx = idx + 1
    call Decho('fname = "'.eval(fname).'.'.ext.'"')

    if getcwd() == $VIMRUNTIME
        call Decho('now we are in $VIMRUNTIME, just out of it...')
        exec 'lcd '.tempdir
        call Decho('now we are in "'.getcwd().'"')
    endif

    silent exec 'file '.simplify(fnamemodify(tempdir.glob('/').
                \ eval(fname).'.'.ext, ':.'))

    let b:ctk_generated_name = expand('%:p')
    call Decho('generated fname is "'.b:ctk_generated_name.'"')
    call Dret('s:set_filename')
endfunction

function! ctk:compile(count, ...) " {{{2
    if s:find_source() || s:save_source() ||
                \ a:count < 0 || a:count > len(b:{s:ci}.list)
        redraw | echo 'Nothing Done'
        return 1
    endif
    call Dfunc('s:compile(count = '.a:count.', '.string(a:000).')')

    let ci = b:{s:ci}
    let spec = (a:0 != 0 && a:1 != '' ? a:1.'_' : '')
    let idx = a:count == 0 ? has_key(ci, 'cur_idx') ?
                \ ci.cur_idx : 0 : (a:count - 1)
    call s:set_cur_info(ci.list[idx])
    let ci.cur_idx = idx
    let cmd = s:get_specarg(spec, 'cmd', 0)

    redraw
    if type(cmd) != type('')
        echo "can't compiling with spec=".spec.'cmd'
        call Dret("s:compile : can't compiling with spec = ".spec)
        return
    endif
    echo 'Compiling ...'

    let msg = 'Compiling... using '.get(ci.cur_info, 'title', ci.cur_info.name)
    let cmd = s:make_cmd(cmd, spec)
    let is_shell = cmd !~ '^[:*]'
    let res = s:run_cmd(cmd)

    let cfile = [msg, cmd, ''] + split(res, "\<NL>")

    redraw
    if is_shell
        let cfile += [ci.cur_info.name.' returned '.v:shell_error]
        if res != ''
            echo 'Compile' (v:shell_error ? 'Fail' : 'Successd')
            if res != ''
                call writefile(cfile, &errorfile)
                cgetfile | if v:shell_error != 0 | copen
                else | cwindow | endif
            endif
        endif

        call Dret('s:compile : '.v:shell_error)
        return v:shell_error
    else
        if res != ''
            echo join(cfile, "\n")
            return 1
        endif
    endif
endfunction

function! ctk:run(count, ...) " {{{2
    if s:find_source() | return | endif
    let bufnr = bufnr('%')
    if (&modified || !has_key(b:{s:ci}, 'cur_info')
                \ || b:{s:ci}.cur_idx != (a:count - 1)
                \ ) && call('ctk:compile', [a:count] + a:000)
        return 1
    endif
    call Dfunc('s:run(count = '.a:count.', '.string(a:000).')')

    exec bufwinnr(bufnr).'wincmd w'
    let spec = (a:0 == 0 || a:1 == '' ? '' : a:1.'_')
    let cmd = s:get_specarg(spec, 'run', 0)

    if type(cmd) != type('')
        redraw | echo "can't exec program with spec=".spec.'run'
        call Dret("s:run : can't exec program with spec = ".spec)
        return
    endif
    let cmd = s:make_cmd(cmd, spec)
    if cmd !~ '^[:*]'
        if cmd[0] == '!' | let cmd = cmd[1:] | endif
        if g:ctk_execprg != ''
            let cmd = substitute(g:ctk_execprg, '$exec\>', escape(cmd, '\'), 'g')
        endif
        let cmd = ':silent !'.cmd
    endif
    redraw
    echomsg s:run_cmd(cmd)
    redraw

    if has('win32') && cmd =~ '^:!'
        call feedkeys("\<NL>", 't')
    endif

    call Dret('s:run')
endfunction

function! ctk:add_flags(flags, count) " {{{2
    if s:find_source() | return | endif

    if a:count > 0 && a:count <= len(b:{s:ci}.list)
        let compiler = '-'.b:ctk.info[a:count - 1].name
    else
        let compiler = ''
    endif

    let com_begin = matchstr(&com, 's.\=:\zs[^,]\+\ze')
    if com_begin != ''
        let com_begin .= ' '
        let com_end = ' '.matchstr(&com, 'e.\=:\zs[^,]\+\ze')
    else
        let com_begin = matchstr(&com, ':\zs[^,]\+').' '
        let com_end = ''
    endif
    
    call append(line('$'), com_begin.'cc'.compiler.': '.a:flags.com_end)
endfunction

function! ctk:set_default_info(cmdarg) " {{{2
    call Dfunc('s:set_default_info(cmdarg = "'.a:cmdarg.'")')
    let def_info = {}
    if !exists(s:ci_name)
        let b:{s:ci} = {}
    endif
    let b:{s:ci}.default = def_info

    call substitute(a:cmdarg, s:pat_infvar, '\=s:sub_info(def_info)', 'g')

    if has_key(def_info, g:ctk_ext_var)
        let b:{g:ctk_ext_var} = def_info[g:ctk_ext_var]
        unlet def_info[g:ctk_ext_var]
    endif

    for key in keys(s:def_attr)
        if !has_key(def_info, key)
            let def_info.input = s:def_attr[key]
        endif
    endfor

    call Dret('s:set_default_info')
endfunction

function! ctk:set_compiler_info(cmdarg, bang) " {{{2
    call Dfunc('s:set_compiler_info(cmdarg = "'.a:cmdarg.'")')
    if !exists(s:ci_name)
        let b:{s:ci} = {'list':[]}
    elseif !has_key(b:{s:ci}, 'list')
        let b:{s:ci}.list = []
    endif

    " empty command
    if a:cmdarg == ''
        if a:bang == '!' " delete all
            call Decho('delete all')
            for info in b:{s:ci}.list
                silent! exec info.unmap
            endfor
            let b:{s:ci}.list = []
        endif
        return Dret('s:set_compiler_info')
    endif

    " find name and others, mlist = [all, name, infos]
    let mlist = matchlist(a:cmdarg, '\v^\s*(.{-})%(\s+(.{-})\s*)=$')

    " add or modify a info
    if mlist[2] != ''
        let info = s:find_info(mlist[1])

        " add a new info, or clean old info
        " if exists ci.default, use it for default values
        if empty(info)
            let info.name = mlist[1]
            call add(b:{s:ci}.list, info)
        else
            call filter(info, 0)
        endif

        let info.name = mlist[1]
        call substitute(mlist[2], s:pat_infvar,
                    \ '\=s:sub_info(info)', 'g')

        let info.unmap = ''
        let idx = s:get_idx(info)
        let dict = {'cmdmap': 'compile', 'runmap': 'run'}
        for key in keys(dict)
            if !has_key(info, key) | continue | endif
            call Decho('setup '.key)
            for mkey in s:get_escaped_list(info[key])
                let cpos = stridx(mkey, ':')
                for mode in split(cpos <= 0 ? 'nvi' : mkey[:cpos - 1], '\zs')
                    try | exec mode.'noremap <unique> '.mkey[cpos+1:].
                                \ ' <C-\><C-N>:call ctk:'.dict[key].'('.(idx + 1).')<CR>'
                        let info.unmap .= mode.'unmap '.mkey[cpos+1:].'|'
                        call Decho(mode.'map '.mkey[cpos+1:].' success!')
                    catch | endtry
                endfor
            endfor
        endfor

    " remove a info
    elseif a:bang == '!'
        let info = s:find_info(mlist[1])

        if !empty(info)
            silent! exec info.unmap
            call remove(b:{s:ci}, s:get_idx(info))
        endif

    " list a info
    else
        call s:list_compiler(mlist[1])
    endif

    call Decho('>> now info = '.string(info))
    call Dret('s:set_compiler_info')
endfunction

function! ctk:list_compiler(name, ...) " {{{2
    if s:find_source() | return | endif
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
        call s:show_list(s:find_info(a:name))
    endif
endfunction

function! s:find_source() " {{{2
    let cur_winnr = winnr()

    while 1
        if exists(s:ci_name) | return 0 | endif
        wincmd w

        if winnr() == cur_winnr
            call s:echoerr("Can't Find Source Window!")
            return 1
        endif
    endwhile
endfunction

function! s:save_source() " {{{2
    try
        silent write

    catch /E13/ " File exists
        let res = s:question("File Exists, Overwrite?(y/n)"
        if res | silent write! | endif
        return !res

    catch /E45/ " Readonly
        let res = s:question("File Readonly, Still write?(y/n)")
        if res | silent write! | endif
        return !res

    endtry
endfunction

function! s:set_cur_info(info) " {{{2
    if expand('%') == '' | return | endif
    call Dfunc('s:set_cur_info(info = '.a:info.name.')')
    let cur_info = copy(a:info)
    for var in ['cmd', 'run', 'un']
        silent! unlet! cur_info[var.'map']
    endfor
    let b:{s:ci}.cur_info = cur_info

    if exists('b:ctk_generated_name')
                \ && b:ctk_generated_name == expand('%:p')
        let cur_info.output = g:ctk_defoutput
    endif

    " analyze the modeline to modifie the info
    if &modeline
        let last = line('$')
        if last <= &mls * 2
            call s:read_modeline(1, last)
        else
            call s:read_modeline(0, &mls)
            call s:read_modeline(last - &mls, last)
        endif
    endif

    " change input/output into filelist
    call Decho('set the IO flags')
    for key in ['input', 'output']
        let val = ''
        for file in s:get_escaped_list(s:get_specarg('',
                    \ 'default', s:def_attr[key]))
            call Decho(key.'.file = '.file)
            let file = file =~ '^[#%]\%(:.\)*$'
                        \ ? fnamemodify(expand('%'), file[1:])
                        \ : fnamemodify(file, ':.')
            if file =~ '\s'
                let file = shellescape(file)
            endif
            let val .= file.' '
        endfor
        let cur_info[key] = matchstr(val, '^\s*\zs.\{-}\ze\s*$')
    endfor

    call Decho('now cur_info = '.string(cur_info))
    call Dret('s:set_cur_info')
endfunction

function! s:read_modeline(begin, end) " {{{2
    call Dfunc('s:read_modeline(begin = '.a:begin.', end = '.a:end.')')
    let pos = winsaveview()

    call cursor(a:begin, 1)
    while search(s:pat_modeline, '', a:end) != 0
        call Decho('find a modeline in line '.line('.'))
        let mlist = matchlist(getline('.'), s:pat_modeline)
        if mlist[1] == '' || b:{s:ci}.cur_info.name =~ '^\V'.escape(mlist[1], '\')
            call substitute(mlist[2], s:pat_mlvar,
                        \ '\=s:sub_modeline()', 'g')
        endif
    endwhile

    call winrestview(pos)
    call Dret('s:read_modeline')
endfunction

function! s:sub_modeline() " {{{2
    call Decho('let cur_info['.submatch(1).'] '.submatch(3).'= "'.
                \ submatch(5).'"')

    if submatch(3) != ''
        let val = get(b:{s:ci}.cur_info, submatch(1),
                    \ get(b:{s:ci}.default, submatch(1), 0))
        if type(val) != type('')
            call s:echoerr("modeline: can't find '".
                        \ submatch(1)."' in current info")
            call Dret('ctk:process_modeline')
            return
        endif

        let b:{s:ci}.cur_info[submatch(1)] = val.' '.submatch(5)
    elseif submatch(2) != ''
        let b:{s:ci}.cur_info[submatch(1)] = submatch(5)
    endif
endfunction

function! s:make_cmd(cmd, spec) " {{{2
    if !has_key(b:{s:ci}, 'cur_info') | return | endif
    call Dfunc('s:make_cmd(cmd = "'.a:cmd.'", spec = "'.a:spec.'"')

    let cmd = substitute(a:cmd, '$\l\+', '\=s:sub_repvar(a:spec)', 'g')
    if cmd !~ '^[:*]'
        call Decho('this is a executable cmd')
        let exe = matchstr(cmd, '^!\=\zs.\{-}\ze\(\s\|$\)')
        call Decho('exe = '.exe)
        if glob(exe) != '' && !executable(exe)
                    \ && executable('./'.exe)
            let cmd = './'.(cmd[0] == '!' ? cmd[1:] : cmd)
        endif
    endif
    if cmd !~ '^[!:*]' | let cmd = '!'.cmd | endif
    call Dret('s:make_cmd : '.cmd)
    return cmd
endfunction

function! s:run_cmd(cmdarg) " {{{2
    if !has_key(b:{s:ci}, 'cur_info') | return | endif
    call Dfunc('s:run_cmd(cmdarg = '.a:cmdarg.')')

    let mlist = matchlist(a:cmdarg, '\v^([!:*])=(.*)$')
    call Decho('mlist = '.string(mlist[:2]))
    if mlist[1] == '!' || mlist[1] == ''
        let output = system(mlist[2])
    endif
    if mlist[1] == ':'
        redir => output
        silent! exec mlist[2]
        redir END
        let output = matchstr(output, '\%^[\r\n]*\zs.\{-}\ze[\r\n]*\%$')
    endif
    if mlist[1] == '*'
        let output = eval(mlist[2])
    endif

    call Dret('s:run_cmd : '.output)
    return output
endfunction

function! s:question(msg) " {{{2
    redraw
    echohl Question
    echo a:msg
    echohl NONE
    return nr2char(getchar()) ==? 'y'
endfunction

function! s:echoerr(errmsg) " {{{2
    echohl ErrorMsg
    echomsg 'ctk: '.a:msg
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

function! s:get_specarg(spec, key, default) " {{{2
    if has_key(b:{s:ci}, 'default')
        return get(b:{s:ci}.cur_info, a:spec.a:key,
                    \ get(b:{s:ci}.default, a:spec.a:key,
                    \ get(b:{s:ci}.cur_info, a:key,
                    \ get(b:{s:ci}.default, a:key, a:default))))
    else
        return get(b:{s:ci}.cur_info, a:spec.a:key,
                    \ get(b:{s:ci}.cur_info, a:key, a:default))
    endif
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

function! s:find_info(name) " {{{2
    for info in b:{s:ci}.list
        if info.name ==? a:name
            return info
        endif
    endfor
    return {}
endfunction

function! s:sub_info(info) " {{{2
    let name = has_key(a:info, 'name') ? a:info.name : 'noname' "Decho
    call Dfunc('s:sub_info(info = '.name.')')
    call Decho('let '.name.'.'.submatch(1).' = "'.submatch(3).'"')
    let a:info[submatch(1)] = submatch(3)
    call Dret('s:sub_info')
endfunction

function! s:sub_repvar(spec) " {{{2
    let cur_info = b:{s:ci}.cur_info
    let default = b:{s:ci}.default

    let val = s:get_specarg(a:spec, submatch(0)[1:], '')
    call Decho('val = "'.val.'"')

    let mstr = matchstr(val, '$\l\+')
    while mstr != ''
        let vval = s:get_specarg('', mstr[1:], '')
        let val = substitute(val, mstr, vval, 'g')
        let mstr = matchstr(val, '$\U\+')
    endwhile

    return val
endfunction

function! ctk:info_name_complete(A,L,P) " {{{2
    let list = []
    for dict in b:{s:ci}.list
        let list += [dict.name]
    endfor
    let pat = "'^\\v".substitute(escape(a:A, '\'), "'", "''", 'g')."'"
    return sort(filter(list + ['all', 'default', 'current'],
                \ 'v:val =~ '.pat))
endfunction

function! ctk:info_item_complete(A,L,P) " {{{2
    return "asm_\ncc\ncmd\ndebug_\nflags\n".
                \ "input\noutput\nrun\ntitle"
endfunction " }}}2

" other works {{{1

let &cpo = s:cpo_save
unlet s:cpo_save
set debug=beep " Decho

" }}}1
" vim: ff=unix ft=vim fdm=marker sw=4 ts=8 et sta nu
doc/ctk.txt	[[[1
324
*ctk.txt*	Plugin for compile and run your source code in Vim

ctk - Compile Toolkit					*ctk* *CTK*
Last Change: April 25, 2009

|ctk-description|	Description
|ctk-options|		Option
|ctk-usage|		Usage
|ctk-features|		Features
|ctk-contact|		Contact

For Vim version 7.0 or later.
{Vi does not have any of these features.}

==============================================================================
DESCRIPTION						*ctk-description*

ctk.vim makes you can easily compile your source code in Vim. and using
modeline to modifie your compile-flags. you can define your own hotkey to
invoke compiler, or you can just press "gc" in normal mode, means get
compiled.

the most usefull function of ctk is its modeline. you can just add a line in
your source code: >

 /* cc-gcc: flags+='-mwindows' */

then, if you use gcc to compile this code, the compile command line will be: >

 gcc source.c -Wall -mwindows -o source

and, after compile, you can run it, just press the hotkey you defined, or "gC"
in normal mode.

==============================================================================
USAGE								*ctk-usage*

								  *ctk-hotkey*

you can press hotkey to invoke compiler to compile your code, or invoke the
program generated by compiler. default hotkey is gc, you can add <count>
before it to specified which compiler you want to invoke. default run hotkey
is gC.

CTK commands						*ctk-commands*

						    *ctk-StartCTK* *:StartCTK*
						      *ctk-StopCTK* *:StopCTK*
:StartCTK
:StopCTK
		start to use CTK, or stop CTK. or you can use StartCTK to
		refresh your cinfo file after you modified it.

								*ctk-CC* *:CC*
:[N]CC {spec}
		this command is used to invoke specified compiler. [N] is your
		compiler index. you can see it in :ListCompiler command
		|:ListCompiler|. spec means how you want the compiler worked.
		it will be discussed in |ctk-spec-attr|.
		
		if the compiler command is executed a extern program, the
		quickfix window will open if compiler return nonzero.
		otherwise, the compiler output will display, if any.

							      *ctk-RUN* *:RUN*
:[N]RUN {spec}
		run the program generated by compiler. if you changed your
		source code after compiling or the index [N] or {spec} isn't
		same with ones when you invoke compiler, the compiler will be
		invoked first to compile your code again.

			NOTE: if the compiler command is ":foobar" (that is,
			      exec a Vim command) and it has output, the Run
			      command will not executed. you should run :RUN
			      again. if you don't want this side-effect. just
			      use :silent .... in compiler command.

						    *ctk-AddFlags* *:AddFlags*
:AddFlags {flags}
		add a modeline to source code. contains all text in {flags}.
		see |ctk-modeline|.

					    *ctk-ListCompiler* *:ListCompiler*
:ListCompiler [all | current | default | {name}]
		list compilers ctk support now. all means list all compiler.
		and current means list the compiler just compiled your code.
		default means the attributes in your SetDefaultInfo sentence.
		or a specified name to list this compiler's attributes.

		NOTE: you can press <TAB> to complete all names above.

				    *ctk-EditCompilerInfo* *:EditCompilerInfo*
:EditCompilerInfo
		edit your cinfo file.

					*ctk-SetDefaultInfo* *:SetDefaultInfo*
:SetDefaultInfo {attr}="value" {attr2}=!value2! ...

		set default info for this file. add this command in your cinfo
		file if this is your default value, or it will be dropped
		after your leave Vim!

		you can use anything to replace '"' or '!' to around values.

				      *ctk-SetCompilerInfo* *:SetCompilerInfo*
:SetCompilerInfo {name} {attr}="value" {attr2}=!value2|

		same as SetDefaultInfo. but this is for setting attributes for
		specified compiler. see |ctk-set-compiler-info|


							*ctk-auto-file-rename*

when you use ctk, all new file that has the filetype ctk can recognized will
be renamed. it only happend when g:ctk_autofname isn't empty. so you can
close it by set it to '' |ctk-autofname|

there are several options to control this behavior. the renamed file will be
in a tempfolder defined by g:ctk_tempdir |ctk-tempdir|. and the output will be
changed to g:ctk_defoutput |ctk-defoutput|.


								*ctk-modeline*

you can use modeline in your source code to specified the special flags you
used. just add a line: >

 /* cc: flags+='foobar' */
<
where the cc: will be recognized as a signature of modeline. you can also use
cc-gcc: to specifie gcc-specified settings. the "flags" is a attribute name in
SetCompilerInfo or SetDefaultInfo |ctk-base-attr|. and gcc is the name of
compiler-info |:SetCompilerInfo|. you can use anything to replace "'", just
like in SetCompilerInfo.

you can use command :AddFlags to add a modeline in your file |:AddFlags|.

there are several example of modeline: >

 /* cc: output='foobar' */  : set output to foobar

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


you can define your source compiler and compile-flags in cinfo file, it
defaultly ".compiler_info" in your 'runtimepath', that is usually '~\.vim' in
unix/linux, or '$VIM\vimfiles' in windows. you can open it and edit it, just
press ':StartCTK' to refresh your new settings.

that file is combine with several autocmds |autocmd|. you can add a new
compile settings in this step: >
	-- add a new SetDefaultInfo sentence in a new section:
		" C compilers {{{1
		autocmd c SetDefaultInfo cmd='...' run='...'

	-- add a new SetCompilerInfo after it:
		autocmd c SetCompilerInfo gcc title='...'

maybe you need add a "let" sentence to define the ext-name of your source
code (e.g. "c" for c code, "cpp" for c++ code, and hs for haskell code, etc.):
>
		autocmd c let b:ft_ext = "c"
<
all autocmds are in the group "ctk_autocmds".

						    *ctk-set-compiler-info*

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

						       *ctk-base-attr*
some attributes are base attribute, all compiler info must define then, there
are contain "input", "output", "cmd", "run".

input and output are the input file name and output file name to pass to
compiler with command-line. if you didn't define then, they are defaultly
defined as "%:." and "%:t:r", they are means some modify with current buffer's
name: |::.||::t||::r|. you can also use other names but they can't contain
space: a space means to separate two file name.

"cmd" are used to invoke compiler. it can has some prefix: "!", "*", and ":".
"!" means this cmd is send to shell, just like you press ":!" on cmdline of
Vim |:!|, "*" means call a Vim function, you can write the function in cinfo
file or others, and ":" means exec a Vim command. if you don't add prefix, the
prefix defaultly treated as "!". i.e. cmd="foobar" is same as cmd="!foobar".

"run" are used to exec the program after compiler. it also has the same prefix
with "cmd".

you can contain tags in base attr, tags is like "$foo", that means you have a
extern attribute named "foo" or "xxx_foo", when you press :CC xxx, the
"xxx_foo" will be used, and when you simply press ":CC" or "gc", the "foo"
will be used. >
 cmd='$cc $input $flags -o $output'
>
here cmd contains four tags, there are $cc, $input, $flags and $output. so you
must define these tags in this SetCompilerInfo command. just like: >
 cc='gcc' flags='-Wall'
>
then, when you invoke compiler, the $cc will be replaced by 'gcc', and the
flags will be replaced by '-Wall'. the input and output has its default value.
and will be calculate before the compiler runs, so the command line is just
like: >
 !gcc foo.c -Wall -o foo
<
when you are edit file "foo.c".

						       *ctk-spec-attr*

you can also define spec attributes. just put specified name before attribute
name, and use a '_' to combine then. e.g: foo_bar. when you invoke compiler
with spec foo, the "foo_bar" will used, not "bar".

you can contain tags in spec-attr, they will be replaced by the simple
attribute. e.g. foo_bar='abc $bar', and bar='foofoo', then foo_bar will be
"abc foofoo".

you can't contain tags in simple attributes. it will be remained as it is.

							 *ctk-hotkey-attr*
you can define hotkey in attr, hotkey attr named "cmdmap" and "runmap", you
can define then with "modelist:key", just like cmdmap="n:<f1>", means when you
press <F1> in normal mode, the compiler will be invoked. runmap="<F2>" means
when you press <F2> in any mode, your program will run. (if it hasn't compile
yet, the compiler will be invoked before)

you can't contain tags in hotkey attr.

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
					     *ctk-cinfo-file* *ctk_cinfo_file*
>
 let g:ctk_cinfo_file = '.compiler_info'
<			this is the default cinfo file path, based on
			'runtimepath'. it will be loaded when you start the
			ctk |StartCTK|.

					       *ctk-defoutput* *ctk_defoutput*
>
 let g:ctk_defoutput = './output'
<			this is the default output name when the file name is
			generated by CTK.

						   *ctk-ext-var* *ctk_ext_var*
>
 let g:ctk_ext_var = 'ft_ext'
<			this is the default variable name of current file
			extname. it should b:ft_ext when the g:ctk_ext_var =
			'ft_ext'

						   *ctk-tempdir* *ctk_tempdir*
>
 let g:ctk_tempdir = './noname'
<			this is the temporary folder to contain noname file.

						   *ctk-execprg* *ctk_execprg*
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
54
" compiler infomation file
" Maintainer: StarWing
" Last Change: 2009-04-14 17:09:26
" need ctk {{{1

if !exists('g:loaded_ctk')
    finish
endif
let s:cpo_save = &cpo
set cpo&vim

" Common default info {{{1

au FileType c,cpp SetDefaultInfo
    	\ cmd='!$cc $input $flags -o $output'
    	\ run='!$output' input='%:.' output='%:t:r'
    	\ asm_run=':exe "sp ".escape("+drop $output", "\\")'
	\ asm_output='$output.asm'
    	\ debug_run='!gdb -q $output'

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
" terminational works {{{1
let &cpo = s:cpo_save
unlet s:cpo_save " }}}1
" vim: ff=unix ft=vim fdm=marker sw=4 ts=8 sta
