" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/cca.vim	[[[1
1237
" ==========================================================
" Script Name:  code complete again
" File Name:    cca.vim
" Author:       StarWing
" Version:      0.2
" Last Change:  2009-02-14 22:57:15
" How To Use: {{{1
"       Useage:
"
"           this is a re-code version of code_complete(new update)
"           (http://www.vim.org/scripts/script.php?script_id=2427) the
"           original version of code_complete is write by mingbai, at
"           (http://www.vim.org/scripts/script.php?script_id=1764)
"
"           this plugin remix the features of code_complete and snippetEmu,
"           you can put the snippet file in the cca_snippets_folder (defaultly
"           "snippets"), and put template file in cca_template_folder
"           (defaultly "templates"). then you can input a tigger word and a
"           hotkey to complete. e.g: if the hotkey is <m-d> (Alt+d or Meta+d,
"           i found this combine-key is easy to press :) ) and you has a
"           snippet named foo, when you input the "foo<m-d>", it maybe changed
"           to: (which "|" is the position of your cursor)
"
"               foobar|
"
"           cca support snippetsEmu-style named tag and command tag in your
"           snippet (and template file). you can define the tag-mark youself,
"           and it will be highlighted (if you open the built-in highlight
"           function of Vim). this is a summary of the kinds of tag. (the tags
"           will be highlighted special, if you open highlight. and suppose
"           the cca_tagstart is "<{". the tagcommand is ':',  and the tagend
"           is "}>")
"
"               - cursor tag:  <{}>  or <{:}> if you press hotkey, and next
"                               tag is a empty tag, the cursor will move to
"                               there.
"               - named tag:    <{foo}> after you press hotkey, the name of
"                               the tag will selected, and if you input a text
"                               to replace it, all tag has the same name with
"                               current tag will be replaced to the same
"                               value.  e.g:  (the pipe is position of cursor)
"
"                               |   <{foo:1}> is a <{foo:2}>. 
"
"                               after you press <a-d> and input "bar"
"                               directly, it will changed to:
"
"                               bar| is a bar.
"
"                               the :1 and :2 mark is for sign tags for
"                               regconize it when you make nest tags.
"
"               - identifier tag:
"                               cca must register tag's name and command for
"                               replace nest tag correctlly.  if you can sure
"                               the tag is only for name replace, user won't
"                               make complete in it (that is, it will never be
"                               a nest tag), and it didn't have any command,
"                               you can just add a "cmd" mark after
"                               identifier.  e.g: <{i:}>. and cca won't
"                               register this tag.
"
"               - command tag:  <{foo:1}>, or <{:1}> where "1" may be any
"                               number. this is the command number in
"                               dictionary b:cca.tag_table. if this is a
"                               noname command tag, the command will calculate
"                               immediately. and if it has a name, it will act
"                               as a named tag, and calculate the command when
"                               you leave the tag (goto the next tag).  the
"                               "xt" snippet in common.vim is a noname command
"                               tag, and "printf" in c_snippets.vim is a named
"                               command tag.
"
"                       XXX:    you can complete at normal, select and insert
"                               mode, but you must notice that the first char
"                               of tag is not "in" the tag.  e.g: |<{A}> now
"                               cursor is NOT in the tag A. so it is in normal
"                               mode, so if you want to jump to next tag, you
"                               should make sure the cursor is just in the tag
"
"       Options:
"
"               cca_hotkey      this defined the hotkey for complete
"
"               cca_submitkey   this defined the submitkey.(that is, jump over
"                               a line, and leave all tags in this line into
"                               its default value).
"
"               cca_tagstart
"               cca_tagend
"               cca_tagcommand  this defined the tag mark. and you can
"                               define them as buffer-variables. 
"             
"               cca_search_range
"                               this define the search range for tag jump.
"                               defaultly it is 100, it means just search the
"                               tags in 100 line under current line. if it set
"                               to zero, only search tags in screen.
"
"               cca_filetype_ext_var
"                               this define the filetype buffer variable name.
"                               snippets support this name to show the ext
"                               name for specific filetype. it defaultly
"                               "ft_ext", and b:ft_ext will be used as a
"                               ext-name specified variable name.
"
"               cca_locale_tag_var
"                               this is a dictionary name for snippets file
"                               show its locale tag marks. it has three item:
"                               start, end and cmd. it defined tagstart,
"                               tagend and tagcommand in specified snippets
"                               file.
"
"                       XXX:    to use cca_filetype_ext_var and
"                               cca_locale_tag_var, see the specified snippets
"                               files.
"
"               cca_snippets_folder
"               cca_template_folder
"                               these define the default folder where cca to
"                               find the snippets files and template files. it
"                               must be found at 'runtimepath'.
"
"
"       Command:
"
"               StartComplete
"               StopComplete
"                               Start and stop the complete this will register
"                               or unregister the key map and do some
"                               initialliztion or clean work.
"
"               DefineSnippet   define the snippets. each snippets file are
"                               all combined with this command. the format is:
"                               DefineSnippet {trigger word}: {complete text}
"                               trigger word can be anything. it can have
"                               space, can be a symbol. but if it have space
"                               and have more than one symbol, when you input
"                               it, you should add a "#" before it. e.g: now
"                               we define a sinppet:
"
"                       DefineSnippet trigger with word: this is a trigger with <{}>word
"
"                               then we can input:
"                               #trigger with word<m-d>
"
"                               then it will change to:
"                               this is a trigger with |word
"
"                               the cursor is before "word"
"
"               RefreshSnippets refresh the current snippets file. if no
"                               filetype is set, this command will load
"                               "common.vim" in snippets folder. the snippets
"                               file is under "filetype" folder in snippets
"                               folder, or named "filetype_others.vim", which
"                               others can be any words. all of snippets file
"                               will load in.
"             
" this line is for getscript.vim:
" GetLatestVimScripts: 2535  1 :AutoInstall: cca.vim
" }}}1
" Must After Vim 7.0 {{{1

if exists('loaded_cca')
    finish
endif
let loaded_cca = 'v0.2'

if v:version < 700
    echomsg "cca.vim requires Vim 7.0 or above."
    finish
endif

" :%s/" \zeCCAtrace//g
" :%s/^\s*\zsCCAtrace/" &/g
" map <m-r> :exec '!start '.expand('%:p:h:h').'/run.cmd'<CR>
" let s:debug = 1
let old_cpo = &cpo
set cpo&vim

" }}}1
" ==========================================================
" Options to modify cca {{{1

" complete hotkey {{{2

if !exists('cca_hotkey')
    let cca_hotkey = "<m-d>"
endif

if !exists('cca_submitkey')
    let cca_submitkey = "<m-s>"
endif

" }}}2
" marks for jump {{{2

" tag start mark {{{3

if !exists('cca_tagstart')
    let cca_tagstart = '<{'
endif

" }}}3
" tag end mark {{{3

if !exists('cca_tagend')
    let cca_tagend = '}>'
endif

" }}}3
" the beginning of command in tag {{{3
if !exists('cca_command')
    let cca_tagcommand = ':'
endif

" }}}3

" }}}2
" search range {{{2

if !exists('cca_search_range')
    let cca_search_range = 100
endif

" }}}2
" default filetype ext buffer-variable {{{2
" the 'filetype' name isn't always the ext-name of file. so if file-ext isn't
" same with filetype, use this variable. for default, if you set
" cca_filetype_ext_var to 'ft_ext' (just let it is), this variable is"
" "b:ft_ext", but you can change it. it normally defined in {&ft}_snippets.vim
" in your cca_snippets_folder

if !exists('cca_filetype_ext_var')
    let cca_filetype_ext_var = 'ft_ext'
endif

" }}}2
" locale tags in snippets plugin file {{{2
" the tags in your snippets may different with your settings, but they can use
" this var to converse the locale tags into your tags.

if !exists('cca_locale_tag_var')
    let cca_locale_tag_var = 'snippets_tag'
endif

" }}}2
" snippets folder {{{2

if !exists('cca_snippets_folder')
    let cca_snippets_folder = 'snippets'
endif

" }}}2
" template folder {{{2

if !exists('cca_template_folder')
    let cca_template_folder = 'templates'
endif

" }}}2

" }}}1
" Commands, autocmds and menus {{{1

" commands
command! -nargs=+ DefineSnippet :call s:define_snippets(<q-args>)
command! -nargs=+ -complete=file -range=% MakeTemplate
            \ :<line1>,<line2>call s:make_template(<q-args>)
command! -bar EditSnippets :call s:edit_snippetsfile()
command! -bar -bang RefreshSnippets :call s:refresh_snippets('<bang>')
command! -bar -bang StartComplete :call s:start_complete('<bang>')
command! -bar StopComplete :call s:stop_complete()

if exists('s:debug') && s:debug == 1
    command! -nargs=+ CCAtrace echom 'cca: '.<args>
else
    command! -nargs=+ CCAtrace
endif

" autocmds
augroup cca_autocmd
    au!
    au VimEnter * StartComplete
    au BufNew * StartComplete|CCAtrace 'BufNew:'.expand('<amatch>')
    au FileType * StartComplete|CCAtrace 'FileType:'.expand('<amatch>')
augroup END

" menus
amenu &Tools.&cca.&Start\ Complete :StartComplete<CR>
amenu &Tools.&cca.S&top\ Complete :StopComplete<CR>
amenu &Tools.&cca.&Refresh\ Snippets :RefreshSnippets<CR>

" highlight link
hi def link ccaTag Identifier
hi def link ccaTagMatch Special

" }}}1
" ==========================================================
" main {{{1

" s:start_complete {{{2

function! s:start_complete(bang)
    " create buffer info
    call s:refresh_menu()
    if !exists('b:cca') || b:cca.cur_ft != &ft || a:bang == '!'
        call s:refresh_snippets(a:bang)
    endif

    if exists('b:cca')
        " maps
        if !hasmapto('<Plug>CCA_complete', 'iv')
            exec 'imap <buffer>'.g:cca_hotkey.' <Plug>CCA_complete'
            exec 'nmap <buffer>'.g:cca_hotkey.' <Plug>CCA_complete'
            exec 'smap <buffer>'.g:cca_hotkey.' <Plug>CCA_complete'
        endif

        if !hasmapto('<Plug>CCA_submit'. 'iv')
            exec 'imap <buffer>'.g:cca_submitkey.' <Plug>CCA_submit'
            exec 'nmap <buffer>'.g:cca_submitkey.' <Plug>CCA_submit'
            exec 'smap <buffer>'.g:cca_submitkey.' <Plug>CCA_submit'
        endif

        imap <silent> <script> <Plug>CCA_complete <C-R>=<SID>complete()<CR>
                    \<C-R>=<SID>jump_next()<CR>
        imap <silent> <script> <Plug>CCA_submit <C-R>=<SID>complete()<CR>
                    \<C-R>=<SID>submit()<CR>
        nmap <silent> <script> <Plug>CCA_complete i<C-R>=<SID>jump_next()<CR>
        nmap <silent> <script> <Plug>CCA_submit i<C-R>=<SID>submit()<CR>
        smap <silent> <script> <Plug>CCA_complete <ESC>i<C-R>=<SID>jump_next()<CR>
        smap <silent> <script> <Plug>CCA_submit <ESC>i<C-R>=<SID>submit()<CR>

        " create syntax group
        " FIXME: must define syntax region every time. if we don't define it when
        " the b:cca is exists, the hightlight will invaild, anyone knows why?
        exec 'syntax region ccaTag matchgroup=ccaTagMatch keepend extend '.
                    \ "contains=ccaTag containedin=ALL oneline start='".
                    \ b:cca.pat.start.'\ze.{-}'.b:cca.pat.end."' skip='".'\\'.
                    \ b:cca.pat.end."' end='\\%(".b:cca.pat.cmd.'\d+)='.
                    \ b:cca.pat.end."'"
    endif
endfunction

" }}}2
" s:stop_complete {{{2

function! s:stop_complete()
    syntax clear ccaTag
    unlet! b:cca
    exec 'iunmap <buffer>'.g:cca_hotkey
    exec 'iunmap <buffer>'.g:cca_submitkey
    exec 'nunmap <buffer>'.g:cca_hotkey
    exec 'nunmap <buffer>'.g:cca_submitkey
    exec 'sunmap <buffer>'.g:cca_hotkey
    exec 'sunmap <buffer>'.g:cca_submitkey
endfunction

" }}}2
" s:complete {{{2
" the main complete function, will called immedially when you press the hotkey

function! s:complete()
    let b:cca.status.line = line('.')
    let b:cca.status.col = col('.')
    let ret = (g:cca_hotkey =~ '\c<tab>' ? "\<tab>" : '')

    " find the keyword for complete
    let c_line = getline('.')[:col('.')-2]
    let mlist = matchlist(c_line, '\v((\w+)\s*(\()=)$')

    " if has '(', it's a function complete
    if !empty(mlist) && mlist[3] != ''
        let text = s:function_complete(mlist[4])
        return text != '' ? text : ret
    endif

    " s:text is the return string value of s:process_call()
    if !empty(mlist) && s:process_call(mlist)
        return s:text
    endif

    for pat in ['\v((\W+)\s*)$', '\v((\S+)\s*)$', '\v(#\s*(.{-})\s*)$']
        let mlist = matchlist(c_line, pat)
        if !empty(mlist) && s:process_call(mlist)
            return s:text
        endif
    endfor

    return ret
endfunction

" }}}2
" s:submit {{{2

function! s:submit()
    let line = (b:cca.status.will_jump ? b:cca.status.line : line('.'))

    silent! exec 'norm! '.s:jump_next()
    while line('.') == line && b:cca.status.has_jump
        silent! exec 'norm! '.s:jump_next()
    endwhile

    return line('.') == line ? "\<C-\>\<C-N>$a" : ''
endfunction

" }}}2
" s:jump_next {{{2
" jump to next tag and calculate command section of tag, if need.

function! s:jump_next()
    " CCAtrace 'jump_next() line: '.getline('.')
    call setpos("''", getpos('.'))
    let b:cca.status.has_jump = 0

    if b:cca.status.will_jump
        let b:cca.status.will_jump = 0
        call cursor(b:cca.status.line, b:cca.status.col)
    endif

    let bound = s:get_tag_bound()
    " CCAtrace 'jump_next: bound:'.string(bound)
    if bound[1] == 0 || bound[1] == -2
        let b:cca.status.has_jump = 1
        let bound = s:replace_tags(bound)
    endif

    let pair = s:find_next_tag(bound[0])
    if empty(pair)
        let b:cca.tag_table = {}
        call setpos('.', getpos("''"))
        return ''
    endif

    " CCAtrace 'jump_next: tag:'.getline('.')[pair[0]-1:pair[1]-1]
    " get tag information
    let b:cca.status.has_jump = 1
    let info = s:get_tag_info(pair)
    " CCAtrace 'jump_next: info:'.string(info)

    " if tag name is not empty, register the name of tag. the command runs
    " when you leaved the tag, so we needn't process it now.
    if !empty(info[0])
        let b:cca.status.cur_tag = info[0]

        " we just select the info[0] now
        let begin = pair[0] + strlen(b:cca.tag.start)
        call s:select_text([begin, begin + strlen(info[0]) - 1])
        return "\<c-\>\<c-n>gvo\<c-g>"
    endif

    " if command is not empty (and the tag name is empty), we process the
    " command at next jump.
    if !empty(info[1])
        " calc the command
        call s:exec_cmd(info[1], pair)
        " run jump_next again
        return s:jump_next()
    endif

    " no command, and no name, just jump to there
    call s:select_text(pair)
    norm! gvc
    call cursor(0, pair[0])
    return ''
endfunction

" }}}2

" }}}1
" complete {{{1

" function complete {{{2

function! s:function_complete(word)
    " CCAtrace 'function_complete() called word='.a:word
    let sig_list = []
    let sig_word = {}
    let ftags = taglist('^'.a:word.'$')

    " CCAtrace 'function_complete() tag='.string(ftags)
    " if can't find the function
    if empty(ftags)
        return ''
    endif

    for item in ftags
        " item must have keys kind, name and signature, and must be the
        " type of p(declare) or f(defination), function name must be same
        " with a:fun, and must have param-list
        " if any reason can't satisfy, to next iteration
        if !has_key(item, 'kind') || (item.kind != 'p' && item.kind != 'f')
                    \ || !has_key(item, 'name') || item.name != a:word
                    \ || !has_key(item, 'signature')
                    \ || match(item.signature, '^(\s*\%(void\)\=\s*)$') >= 0
            continue
        endif
        let sig = s:process_signature(item.signature)
        if !has_key(sig_word, sig)
            let sig_word[sig] = 0
            let sig_list += [{'word': sig, 'menu': item.filename}]
        endif
    endfor

    " only one list find, that is we need!
    if len(sig_list) == 1
        let b:cca.status.will_jump = 1
        return sig_list[0].word
    endif

    " can't find the argument-list, means it's a void function
    if empty(sig_list)
        return ')'
    endif

    " make a complete menu
    call complete(col('.'), sig_list)
    return ''
endfunction

" }}}2
" template complete {{{2

function! s:template_complete(word)
    let fname = get(b:cca.tmpfiles, a:word, '')

    if empty(fname)
        return ''
    endif

    let tlist = readfile(fname)
    if empty(tlist)
        return ''
    endif

    " read the modeline of template file
    let mline = matchstr(tlist[0], '\<cca:\v\s*\zs%(.{-}\:)+')
    if !empty(mline)
        let tag_marks = {}

        for expr in filter(split(mline, '\s*\\\@<!:\s*'),
                    \ '!empty(v:val) && v:val =~ "="')
            let expr = s:unescape(expr, ':')
            silent! sandbox exec 'let tag_marks.'.expr
        endfor
        call remove(tlist, 0)
    endif

    let indent = matchstr(getline('.'), '^\s*')
    let text = join(tlist, "<CR><ESC>Vc0<C-D>".indent)

    if exists('tag_marks')
        let text = s:format_tags(tag_marks, text)
    endif

    exec 'let text = "'.escape(text, '"<\').'"'

    return substitute(text, b:cca.pat.start.'([^'."\<CR>".']{-})\\@<!'.
                \ b:cca.pat.end, '\=cca:register_tag()', 'g')
endfunction

" }}}2
" snippets complete {{{2

function! s:snippet_complete(word)
    let text = get(b:cca.snippets, s:encode(a:word), '')
    " CCAtrace 'snippet_complete() called, word = '.a:word.
                \ ', encoded word = '.s:encode(a:word)

    if text == ''
        return ''
    endif

    exec 'let text = "'.escape(text, '"<\').'"'

    return substitute(text, b:cca.pat.start.'([^'."\<CR>".']{-})\\@<!'.
                \ b:cca.pat.end, '\=cca:register_tag()', 'g')
endfunction

" }}}2

" }}}1
" utility {{{1

" cca:count {{{2
function! cca:count(haystack, pattern)
    let counter = 0
    let index = match(a:haystack, a:pattern)
    while index > -1
        let counter = counter + 1
        let index = match(a:haystack, a:pattern, index+1)
    endwhile
    return counter
endfunction

" }}}2
" cca:make_tag {{{2

function! cca:make_tag(...)
    let text = a:0 != 0 ? a:1 : ''
    return b:cca.tag.start . text . b:cca.tag.end
endfunction

" }}}2
" cca:normal {{{2

function! cca:normal(cmd, ...)
    exec 'norm'.(a:0 == 0 ? '' : a:1).' '.a:cmd
    return ''
endfunction

" }}}2
" cca:format_tag {{{2

function! cca:format_tag(end)
    let te = b:cca.tag.end
    let cmd = submatch(2)

    if !empty(cmd)
        if a:end != te
            let cmd = substitute(cmd, b:cca.pat.end, '\'.te, 'g')
            let cmd = s:unescape(cmd, '\V'.escape(a:end, '\'))
        endif
        let cmd = b:cca.tag.cmd . cmd
    endif

    return cca:make_tag(submatch(1).cmd)
endfunction

" }}}2
" cca:register_tag {{{2

function! cca:register_tag()
    let info = s:get_tag_info(submatch(0))
    let [idx, cmd, dict] = [1, '', b:cca.tag_table]

    while has_key(dict, idx)
        let idx += 1
    endwhile

    if info[1] != '' || (info[0] != '' && info[2] != '')
        let dict[idx] = [info[0], '']
        if info[1] == '#'
            call add(dict[idx], 1)
        endif

        let dict[idx][1] = s:unescape(info[1], '\('.b:cca.pat.end.'|#)')
        let cmd = b:cca.tag.cmd . idx
    endif

    return cca:make_tag(info[0] . cmd)
endfunction

" }}}2
" cca:unlet {{{2

function! cca:unlet(var)
    exec 'silent! unlet '.var
endfunction

" }}}2
" s:echoerr {{{2

function! s:echoerr(msg)
    echohl ErrorMsg
    echomsg 'cca: '.a:msg
    echohl NONE
endfunction

" }}}2
" s:strtrim {{{2

function! s:strtrim(str)
    return matchstr(a:str, '^\s*\zs.\{-}\ze\s*$')
endfunction

" }}}2
" s:unescape {{{2

function! s:unescape(str, token)
    return substitute(a:str, '\\\ze'.a:token, '', 'g')
endfunction

" }}}2
" s:encode s:decode {{{2
" s:encode allows the use of special characters in snippets
function! s:encode(text)
    return substitute(a:text, '\(\A\)',
                \ '\="%".char2nr(submatch(1))', 'g')
endfunction

" s:decode allows the use of special characters in snippets
function! s:decode(text)
    return substitute(a:text, '%\(\d\+\)',
                \ '\=nr2char(submatch(1))', 'g')
endfunction
" }}}2
" s:create_bufinfo {{{2

function! s:create_bufinfo()
    let b:cca = {}
    " the info table of tags, will fill by cca:register_tag()
    let b:cca.tag_table = {}
    let b:cca.snippets = {}
    let b:cca.tmpfiles = {}
    let b:cca.cur_ft = &ft

    " the status of complete, 
    " cur_tag is the current tag's name
    " will_jump shows whether jump to the head before jump_next, 
    " line and col is the position of begining of complete, 
    let b:cca.status = {
                \ 'cur_tag': "",
                \ 'will_jump': 0,
                \ 'has_jump': 0,
                \ 'line': 0,
                \ 'col': 0}

    " will fill at once below. tag and tag pattern
    let b:cca.tag = {
                \ 'start': 'tagstart',
                \ 'end': 'tagend',
                \ 'cmd': 'tagcommand'}
    let b:cca.pat = {}

    for [key, val] in items(b:cca.tag)
        if exists('b:cca_'.val)
            let b:cca.tag[key] = b:cca_{val}
        else
            let b:cca.tag[key] = g:cca_{val}
        endif
        let b:cca.pat[key] = '\V'.escape(b:cca.tag[key], '\').'\v'
    endfor
endfunction

" }}}2
" s:define_snippets {{{2

function! s:define_snippets(cmd)
    if !exists('b:cca')
        call s:create_bufinfo()
    endif

    " find first ":" without a '\' before itself.
    let mlist = matchlist(a:cmd, '\v^(.{-})\\@<!\s+(.+)$')
    if empty(mlist)
        return
    endif

    " calculate the name and value
    let name = s:encode(s:strtrim(s:unescape(mlist[1], '\s')))
    let value = s:strtrim(mlist[2])

    " format the tag, if needed
    if exists('b:'.g:cca_locale_tag_var)
        let value = s:format_tags(b:{g:cca_locale_tag_var}, value)
    endif

    " registe the snippet
    let b:cca.snippets[name] = value
endfunction

" }}}2
" s:refresh_snippets {{{2

function! s:refresh_snippets(bang)
    if a:bang == '!'
        silent! unlet b:cca
    endif

    let sf = g:cca_snippets_folder

    exec 'runtime! '.sf.'/common.vim'
    
    if !empty(&ft)
        for name in split(&ft, '\.')
            exec 'runtime! '.sf.'/'.name.'.vim '.sf.'/'.name.'_*.vim '.
                        \ sf.'/'.name.'/*.vim'
        endfor

        let tf = g:cca_template_folder
        let ft_ext = get(b:, g:cca_filetype_ext_var, &ft)
        for file in split(globpath(&rtp, tf.'/*'), "\<NL>")
            let ext = fnamemodify(file, ':e')
            if empty(ext) || ext == ft_ext
                let b:cca.tmpfiles[fnamemodify(file, ':t:r')] = file
            endif
        endfor
    endif
endfunction

" }}}2
" s:make_template {{{2

function! s:make_template(file) range
    let com_start = matchstr(&comments, 's.\=:\zs[^,]\+\ze')
    if !empty(com_start)
        let com_start .= ' '
        let com_end = ' '.matchstr(&comments, 'e.\=:\zs[^,]\+\ze')
    else
        let com_start = matchstr(&comments, ':\zs[^,]\+').' '
        let com_end = ''
    endif
    let template = [com_start.'cca: start="'.escape(b:cca.tag.start, '\":').
                \ '" : end="'.escape(b:cca.tag.end, '\":').
                \ '" : cmd="'.escape(b:cca.tag.cmd, '\":').'" :'.com_end]

    let pat = '^\s\{'.(&et ? indent(a:firstline) :
                \ indent(a:firstline)/&ts).'}'

    for i in range(a:firstline, a:lastline)
        let text = substitute(getline(i), pat, '', '')
        let text = substitute(text, '<', '<lt>', 'g')
        let text = substitute(text, "\r", '<NL>','g')
        let template = template + [text]
    endfor

    if exists('b:'.g:cca_filetype_ext_var)
                \ && !empty(b:{g:cca_filetype_ext_var})
        let ft_ext = '.'.b:{g:cca_filetype_ext_var}
    elseif !empty(&ft)
        let ft_ext = '.'.&ft
    else
        let ft_ext = ''
    endif

    for dir in split(globpath(&rtp, g:cca_template_folder), "\<NL>")
        let fname = dir.glob('/').fnamemodify(a:file, ':t:r').ft_ext
        try
            if writefile(template, fname) == 0
                echomsg 'template write success: '.fname
                echomsg len(template)." line(s) written"
            endif
        catch
            call s:echoerr('template write failed: '.v:exception)
        endtry
        return
    endif
endfunction

" }}}2
" s:edit_snippetsfile {{{2

function! s:edit_snippetsfile()
    let filelist = []
    let sf = g:cca_snippets_folder
    let filelist += split(globpath(&rtp, sf.'/common.vim'), "\<NL>")

    if &ft != ''
        let filelist += split(globpath(&rtp, sf.'/'.&ft.'/*.vim'), "\<NL>")
        let filelist += split(globpath(&rtp, sf.'/'.&ft.'_*.vim'), "\<NL>")
    endif

    if !empty(filelist)
        if len(filelist) == 1
            exec 'drop '.escape(filelist[0], ' ')
        else
            let idx = 1
            let list = ["Select a snippets file:"]
            for file in filelist
                let list += [idx.'. '.file]
                let idx += 1
            endfor
            let res = get(filelist, inputlist(list)-1, '')
            if !empty(res)
                exec 'drop '.escape(res, ' ')
            endif
        endif
    endif
endfunction

" }}}2
" s:refresh_menu {{{2

function! s:refresh_menu()
    let ft = {}
    let sf = g:cca_snippets_folder
    for file in split(globpath(&rtp, sf.'/*'), "\<NL>")
        let type = matchlist(file, '\V'.escape(sf, '\').
                    \ '\v[\\/]%(([^\\/]*)[\\/]|([^_]*)%(_)=).*\.vim')
        if empty(type)
            continue
        endif
        let key = (!empty(type[1]) ? type[1] : type[2])
        if !empty(key) && !has_key(ft, key)
            let ft[key] = ''
            exec 'amenu &Tools.cca.S&nippets\ File\ List.'.escape(key, ' \').
                        \ ' :run! '.sf.'/'.key.'/*.vim '.sf.'/'.key.'_*.vim<CR>'
        endif
    endfor
endfunction

" }}}2
" s:exec_cmd {{{2
" calc the tag-command, if needed. and save it to @z, then use it to replace
" the current selected tag.

function! s:exec_cmd(tag_nr, range)
    let tag = get(b:cca.tag_table, a:tag_nr, [])
    " CCAtrace 'exec_cmd() called, tag = '.string(tag)
    call s:select_text(a:range)

    if !empty(tag)
        call remove(b:cca.tag_table, a:tag_nr)
        if !empty(tag[1])
            try
                if tag[1][0] == '!'
                    let tag[1] = tag[1][1:]
                else
                    let old_z = @z
                endif
                let res = eval(tag[1])
                let @z = res
            catch
                " if eval has error occured, print error
                call s:echoerr('calculate command error, at ['.line('.').
                            \ ', '.col('.').'], tag_name = "'.tag[0].
                            \ '", expr = "'.tag[1].'":'.
                            \ matchstr(v:exception, 'E.*'))
            endtry
        endif
    endif

    " CCAtrace 'exec_cmd() line: '.getline('.')
    exec 'norm! gv"zp'
    " put cursor after the text we pasted
    if !empty(@z)
        call cursor(0, col('.') + 1)
    endif
    if exists('old_z')
        let @z = old_z
    endif
    " CCAtrace 'exec_cmd() line: '.getline('.')
endfunction

" }}}2
" s:find_next_tag {{{2

function! s:find_next_tag(end)
    " CCAtrace 'find_next_tag() called'
    " type([]) == 3
    let [end_line, end_col] = (type(a:end) == 3 ? a:end : [a:end, -1])
    let res = searchpos('\\\@<!'.b:cca.pat.start, 'nc', end_line)

    while 1
        " CCAtrace 'find_next_tag() res='.string(res)
        if res[1] == 0 || (end_col > 0 && res[1] > end_col)
            return []
        endif
        call cursor(res)
        if s:find_match_tag(0) >= 0
            return [res[1], col('.')]
        endif
        let res = searchpos('\\\@<!'.b:cca.pat.start, 'n', end_line)
    endwhile
endfunction

" }}}2
" s:find_match_tag {{{2
" if dir == 0, right, dir != 0, left. return the tags number between marks and
" cursor.

function! s:find_match_tag(dir)
    let cline = line('.')
    let [level, times] = [0, -1]
    let [flag, dir] = (a:dir ? ['bep', 1] : ['ep', -1])
    let pat = '\v\\@<!%(('.b:cca.pat.start.'&.)|('.b:cca.pat.end.'))'

    while level != dir
        let res = search(pat, flag, cline)
        if res == 0
            return -1
        endif
        let times += 1
        let level += (res == 2 ? 1 : -1)
    endwhile

    return times / 2
endfunction

" }}}2
" s:format_tags {{{2
" set the tags into the format in current buffer

function! s:format_tags(info, text)
    let t = copy(b:cca.tag)
    for key in keys(a:info)
        let t[key] = a:info[key]
    endfor

    let pat = '\V'.escape(t.start, '\').'\v(.{-})%(\V'.escape(t.cmd, '\').
                \ '\v(.*))=\\@<!\V'.escape(t.end, '\')

    return substitute(a:text, pat, '\=cca:format_tag(t.end)', 'g')
endfunction

" }}}2
" s:replace_tags {{{2
" replace tags in bound region. if this region is only a simple tag, replace
" all tag in file has same name with current tag. then, if the region is a
" next tag. replace tag in this bound region. return the new position after
" replaced.

function! s:replace_tags(bound)
    " CCAtrace 'replace_tags() called'
    let bound = a:bound
    while 1
        " CCAtrace 'replace_tags(): bound='.string(bound)
        if bound[1] == 0
            " a simple tag
            " CCAtrace 'replace_tags:replace begin'
            let old_z = @z
            let info = s:get_tag_info(bound[2])
            let @z = info[0]
            let cur_tag = empty(info[1]) ? b:cca.status.cur_tag :
                        \ get(b:cca.tag_table, info[1], [''])[0]
            " CCAtrace 'replace_tags:replace: cur_tag=:'.cur_tag
            call s:exec_cmd(info[1], bound[2])
            call setpos("''", getpos('.'))
            let bound = s:get_tag_bound()
            " CCAtrace 'replace_tags:replace: bound='.string(bound)

            if !empty(cur_tag)
                let view_save = winsaveview()
                let pair = s:find_next_tag(bound[0])
                if !empty(pair)
                    while !empty(pair)
                        let info = s:get_tag_info(pair)
                        if info[0] == cur_tag
                            call s:exec_cmd(info[1], pair)
                        endif
                        let pair = s:find_next_tag(bound[0])
                    endwhile
                    " CCAtrace 'replace_tags:replace end'
                    call winrestview(view_save)
                    let bound = s:get_tag_bound()
                endif
            endif
            let @z = old_z
        endif

        if bound[1] > 0 || bound[1] == -1
            return bound
        endif

        if bound[1] == -2
            " only head, delete it
            let cp = getpos('.')
            let ts_len = strlen(b:cca.tag.start)
            call cursor(0, bound[2])
            call s:delete_text(ts_len, 1)
            if col("''") >= bound[2] + ts_len
                call cursor(line("''"), col("''") - ts_len)
                call setpos("''", getpos('.'))
            endif
            if cp[1] >= bound[2] + ts_len
                call cursor(cp[0], cp[1] - ts_len)
            endif
            let bound = s:get_tag_bound()
        endif
    endwhile
endfunction

" }}}2
" s:get_tag_bound {{{2
" return [end, cnt, bound]

function! s:get_tag_bound()
    let [cline, col_save] = [line('.'), col('.')]
    " CCAtrace 'get_tag_bound() called col='.col_save

    let cnt = s:find_match_tag(1)
    if cnt == -1
        call cursor(0, col_save)
        " CCAtrace 'get_tag_bound() returned -1'
        return [(g:cca_search_range == 0 ? line('$') :
                \ cline + g:cca_search_range), -1]
    endif

    let col_start = col('.')
    let cnt = s:find_match_tag(0)
    if cnt == -1
        call cursor(0, col_save)
        " CCAtrace 'get_tag_bound() returned -2'
        return [(g:cca_search_range == 0 ? line('$') :
                \ cline + g:cca_search_range), -2, col_start]
    endif
    let col_end = col('.')

    " CCAtrace 'get_tag_bound() tag='.getline('.')[col_start-1:col_end-1]
    " CCAtrace 'get_tag_bound() returned '.cnt
    call cursor(0, col_save)
    return [[cline, col_end], cnt, [col_start, col_end]]
endfunction

" }}}2
" s:get_tag_info {{{2
" if args is a list, it's the column number range of current line. signs the
" beginning of tag and the end of tag. and if it's a string, it's the tag
" itself. if tag is in buffer, the number after last pc is the id of cmd,
" else, the identifier before the first pc is the id of tag.

function! s:get_tag_info(args)
    if type(a:args) == 3 " type([])
        let tag = getline('.')[a:args[0]-1:a:args[1]-1]
        let pat = '\d+'
    else
        let tag = a:args
        let pat = '.*'
    endif

    " get inner text of tag
    let inner = matchstr(tag, '^'.b:cca.pat.start.'\zs.{-}\ze\\@<!'.
                \ b:cca.pat.end.'$')

    if !empty(inner)
        " get tag(mlist[1]) and command(mlist[2])
        let mlist = matchlist(inner, '^\v(.{-})%(('.b:cca.pat.cmd.
                    \ ')('.pat.'))=$')
        return [mlist[1], mlist[3], mlist[2]]
    endif

    return ['', '', '']
endfunction

" }}}2
" s:select_text {{{2

function! s:select_text(range)
    " CCAtrace 'select_text() called, range = '.string(a:range)
    if foldclosed(line('.')) != -1
        norm! zO
    endif

    exec "norm! \<c-\>\<c-n>"
    call cursor(0, a:range[0])
    norm! v
    call cursor(0, a:range[1] + (&sel == 'exclusive'))
    " leave visual mode, you can use "gv" re-select the same region
    " CCAtrace 'select_text() line = '.getline('.')
    exec "norm! \<esc>"
endfunction

" }}}2
" s:delete_text {{{2
" this funciotn can use in :call and in <c-r>= mode.
" if has second argument and it's nonzero, delete the text at the position of
" cursor. e.g: delete the "xxx" in "abcxxx" when the cursor is on the "x"
" (when len == 3). else, delete the text just before the cursor. e.g: delete
" the "abc" in "abcxxx" when the cursor is on the "x".

function! s:delete_text(len, ...)
    " CCAtrace 'delete_text: '.getline('.')
    let c = col('.')

    if a:0 != 0 && a:1 != 0
        exec "norm! ".a:len.'xa'
        call cursor(0, c)
    else
        exec "norm! ".a:len.'h'.a:len.'xa'
        " fix when the cursor is at the last of line
        call cursor(0, c - a:len)
    endif

    " CCAtrace 'delete_text: '.getline('.')
    return ''
endfunction

" }}}2
" s:process_call {{{2

function! s:process_call(mlist)
    " CCAtrace 'process_call() called, mlist = '.string(a:mlist)
    if !empty(a:mlist)
        let wlen = strlen(a:mlist[1])
        for func in ['s:template_complete', 's:snippet_complete']
            let text = call(func, [a:mlist[2]])
            if text != ''
                let b:cca.status.col -= wlen
                let b:cca.status.will_jump = 1

                call s:delete_text(wlen)
                let s:text = text
                return 1
            endif
        endfor
    endif
endfunction

" }}}2
" s:process_signature {{{2

function! s:process_signature(sig)
    let res = b:cca.tag.start
    let level = 0
    for ch in split(substitute(a:sig[1:-2],'\s*,\s*',',','g'), '\zs')
        if ch == ','
            if level != 0
                let res .= ', '
            else
                let res .= b:cca.tag.end.', '.b:cca.tag.start
            endif
        else
            let res .= ch
            let level += (ch == '(' ? 1 : (ch == ')' ? -1 : 0 ))
        endif
    endfor
    return res.b:cca.tag.end.')'.cca:make_tag(';')
endfunction

" }}}2

" }}}1
" ==========================================================
" History: {{{1
"
" 2009-02-17 15:38:41   change the format of DefineSnippet command
"                       improve the complete function, make it more clever
" 2009-02-14 22:35:33   fix the submit error. now submit key can be work fine
"                       make select more effective.
"                       add s:strtrim function, for delete the space before
"                       and after the text. this is a little boxing.
"                       fix the bug when the next tag is in a fold, the whole
"                       fold will be delete (oh, this is a big bug...)
"
" 2009-02-04 12:19:48   fix the error of the priority of complete trigger find
"                       fix the autocmd to BufEnter.
"                       optimize the refresh_snippets()
"
" }}}1
" Restore cpo {{{1

let &cpo = old_cpo

" }}}1
" vim: ft=vim:ff=unix:fdm=marker:ts=4:sw=4:et:sta:nu
plugin/ctk.vim	[[[1
779
" ==========================================================
" Script Name:  code toolkit
" File Name:    ctk.vim
" Author:       StarWing
" Version:      0.1
" Last Change:  2009-02-10 22:28:49
" Must After Vim 7.0 {{{1

if exists('loaded_ctk')
    finish
endif
let loaded_ctk = 'v0.1'

if v:version < 700
    echomsg "ctk.vim requires Vim 7.0 or above."
    finish
endif

" :%s/^\s*\zsCTKtrace/" &/g
" :%s/" \zsCTKtrace/&/g " restore traces
" map <m-r> :exec '!start '.expand('%:p:h:h').'/run.cmd'<CR>
" let s:debug = 1
let old_cpo = &cpo
set cpo&vim

" }}}1
" ==========================================================
" Options {{{1

" hotkeys {{{2
" $num, $id

if !exists('ctk_compile_hotkey')
    let ctk_compile_hotkey = '\c$id'
endif

if !exists('ctk_run_hotkey')
    let ctk_run_hotkey = '<m-$num>'
endif

" }}}2
" auto_generate_name {{{2

if !exists('ctk_temp_folder')
    let ctk_temp_folder = './noname'
endif

if !exists('ctk_auto_generated_fname')
    let ctk_auto_generated_fname = 'strftime("%Y-%m-%d")."-".idx'
endif

if !exists('ctk_temp_output')
    let ctk_temp_output = './tmp'
endif

" }}}2
" ctk_filetype_ext_var{{{2

if !exists('ctk_filetype_ext_var')
    if exists('cca_filetype_ext_var')
        let ctk_filetype_ext_var = cca_filetype_ext_var
    else
        let ctk_filetype_ext_var = 'ft_ext'
    endif
endif

" }}}2
" compiler info folder {{{2

if !exists('ctk_compiler_info_folder')
    if exists('cca_snippets_folder')
        let ctk_compiler_info_folder = cca_snippets_folder
    else
        let ctk_compiler_info_folder = 'snippets'
    endif
endif

" }}}2

" }}}1
" Commands, autocmds and meuus {{{1

command! -bar -bang RegisterCompiler call s:register_compiler('<bang>')
command! -bar UnRegisterCompiler call s:unregister_compiler()

command! -nargs=* -complete=customlist,s:info_name_complete -bar -count=0 
            \ ListCompiler call s:list_compiler(<q-args>, <count>)
command! -nargs=+ -complete=custom,s:info_item_complete -bang
            \ SetCompilerInfo call s:set_compiler_info(<q-args>, '<bang>')
command! -nargs=+ -complete=custom,s:info_item_complete -count=0
            \ AddFlags call s:add_flags(<q-args>, <count>)

command! -bar -count=1 Compile call s:compile(<count>)
command! -bar -count=1 Run call s:run(<count>)

augroup ctk_autocmd
    au!
    au VimEnter * RegisterCompiler | CTKtrace 'VimEnter:'.expand('<amatch>')
    au FileType * RegisterCompiler | CTKtrace 'FileType:'.expand('<amatch>')
augroup END

if exists('s:debug') && s:debug
    command! -nargs=+ CTKtrace echohl Search|echomsg 'ctk: '.<args>|echohl NONE
else
    command! -nargs=+ CTKtrace
endif

" }}}1
" ==========================================================
" main {{{1

" s:register_compiler {{{2

function! s:register_compiler(bang)
    if s:file_autoname() == 0
        if !exists('b:ctk') || a:bang == '!'
            call s:refresh_info(a:bang)
        endif
        if exists('b:ctk') && exists('b:ctk_generated_name')
            let b:ctk.generated_name = b:ctk_generated_name
            unlet b:ctk_generated_name
        endif
    endif
endfunction

" }}}2
" s:unregister_compiler {{{2

function! s:unregister_compiler()
    for map in b:ctk.unmap
        exec map
    endfor
    for info in b:ctk.info
        exec info.unmap
    endfor
    unlet b:ctk
endfunction

" }}}2
" s:compile {{{2

function! s:compile(count)
    " CTKtrace 'compile() called, a:000 = '.string(a:000)
    if s:find_source() || s:save_source()
                \ || a:count <= 0 || a:count > len(b:ctk.info)
        return 1
    endif

    if !has_key(b:ctk, 'cur_idx')
        let b:ctk.cur_idx = 0
    endif
    if !has_key(b:ctk, 'changedtick')
        let b:ctk.changedtick = b:changedtick
    endif
    if b:ctk.cur_idx != a:count - 1
                \ || b:ctk.changedtick != b:changedtick
        silent! unlet b:ctk.cur_info
        let b:ctk.cur_idx = a:count - 1
        let b:ctk.changedtick = b:changedtick
    endif

    if !has_key(b:ctk, 'cur_info')
        call s:prepare_info(b:ctk.cur_idx)
    endif
    let info = b:ctk.cur_info

    redraw
    echo 'Compiling ...'

    let cmd = s:prepare_compile_cmd(info.cc_cmd)
    let msg = 'Compiling... using '.get(info, 'title', info.name)
    let cfile = [msg, cmd, ''] + split(s:run_cmd(cmd), "\<NL>")
    let cfile += [info.name.' returned '.v:shell_error]
    call writefile(cfile, &errorfile)
    cgetfile

    redraw
    if v:shell_error != 0
        echo 'Compile Fail'
        copen
        return 1
    else
        echo 'Compile Successd!'
        cwindow
        return 0
    endif
endfunction

" }}}2
" s:run {{{2

function! s:run(count)
    if s:find_source() != 0
                \ || (!has_key(b:ctk, 'changedtick')
                \ || b:ctk.changedtick < b:changedtick
                \ || a:count - 1 != b:ctk.cur_idx)
                \ && s:compile(a:count) != 0
        return 1
    endif

    call s:find_source()
    let info = b:ctk.cur_info
    
    if has_key(info, 'debug')
                \ && has_key(info, 'debug_cmd')
                \ && info.debug_cmd != ''
        let cmd = info.debug_cmd
    elseif has_key(info, 'run_cmd')
                \ && info.run_cmd != ''
        let cmd = info.run_cmd
    else
        call s:echoerr("No run_cmd or debug_cmd in info")
        return
    endif

    if cmd !~ '^[:*]'
        if cmd[0] == '!'
            let cmd = cmd[1:]
        endif
        if has('win32') && executable('vimrun')
            let cmd = 'start vimrun '.cmd
            let cmd = ':!'.cmd
        elseif has('unix') && cmd =~ '$output'
                    \ && !executable(info.output)
            for output in ['"./".info.output',
                        \ 'fnamemodify(info.output, ":p")']
                if executable(output)
                    let info.output = output
                endif
            endfor
        endif
        let cmd = s:prepare_compile_cmd(cmd)
    endif

    if has('win32') && cmd =~ '^:!'
        call feedkeys("\<NL>", 't')
    endif
endfunction

" }}}2

" }}}1
" utility {{{1

" ctk:process_info {{{2

function! ctk:process_info(info)
    " CTKtrace 'process_flags: '.submatch(1).' = '.submatch(3)
    let a:info[submatch(1)] = s:strtrim(submatch(3))
    return ''
endfunction

" }}}2
" ctk:process_modeline {{{2

function! ctk:process_modeline(info)
    " CTKtrace 'process_flags: '.submatch(1).' = '.submatch(4)
    let val = s:strtrim(submatch(4))
    if !has_key(a:info, submatch(1))
        call s:echoerr("modeline: can't find '".submatch(1)."' in current info")
        return ''
    endif
    if submatch(2) != ''
        let a:info[submatch(1)] .= ' '.val
    else
        let a:info[submatch(1)] = val
    endif
    return ''
endfunction

" }}}2
" s:info_item_complete {{{2

function! s:info_item_complete(A,L,P)
    return "cc_cmd\ndebug_cmd\ndebug_flags\nflags\n".
                \ "hotkey\ninput\noutput\nrun_cmd\ntitle\n"
endfunction

" }}}2
" s:info_name_complete {{{2

function! s:info_name_complete(A,L,P)
    return sort(filter(map(copy(b:ctk.info), 'v:val.name'),
                \ "v:val =~ '^\\v".escape(a:A, '\')))
endfunction

" }}}2
" s:echoerr {{{2

function! s:echoerr(msg)
    echohl ErrorMsg
    echomsg 'ctk: '.a:msg
    echohl NONE
endfunction

" }}}2
" s:strtrim {{{2

function! s:strtrim(str)
    return matchstr(a:str, '^\s*\zs.\{-}\ze\s*$')
endfunction

" }}}2
" s:add_flags {{{2

function! s:add_flags(flags, count)
    if s:find_source()
        return
    endif

    if a:count > 0 && a:count <= len(b:ctk.info)
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

" }}}2
" s:show_list {{{2

function! s:show_list(info)
    echohl Title
    if has_key(a:info, 'title')
        echo a:info.title."\n\tname         = ".a:info.name."\n"
    else
        echo a:info.name."\n"
    endif
    echohl NONE

    for key in sort(filter(keys(a:info),
                \ "v:val !~ '".'title\|name\|unmap'."'"))
        echo printf("\t%-12s = %s", key, a:info[key])
    endfor
endfunction

" }}}2
" s:find_source{{{2
 
function! s:find_source()

    let cur_winnr = winnr()

    while 1
        if exists('b:ctk')
            return 0
        endif

        wincmd w

        if winnr() == cur_winnr
            call s:echoerr("Can't Find Source Window!")
            return 1
        endif
    endwhile
    
endfunction

" }}}2
" s:save_source {{{2

function! s:save_source()

    try
        silent write
    catch /E13/ " File exists
        redraw
        echohl Question
        echo "File Exists, Overwrite?(y/n)"
        echohl NONE

        if nr2char(getchar()) ==? 'y'
            silent write!
            return 0
        endif

        redraw
        echo "Nothing Done"
        return 1
    catch /E45/ " read only
    endtry

endfunction

" }}}2
" s:add_info {{{2

function! s:add_info(idx, name, info_text)
    let idx = a:idx
    if idx == -1
        " can't find name, add a new item
        let idx = len(b:ctk.info)
        call add(b:ctk.info, {'name': a:name})
        call add(b:ctk.unmap, s:mapping_keys(get(b:, 'ctk_compile_hotkey',
                    \ g:ctk_compile_hotkey), idx, 1, 0).
                    \ s:mapping_keys(get(b:, 'ctk_run_hotkey',
                    \ g:ctk_run_hotkey), idx, 0, 0))
    else
        exec b:ctk.info[idx].unmap
        let b:ctk.info[idx] = {'name': a:name}
    endif

    let info = b:ctk.info[idx]
    call substitute(a:info_text, '\v(<\w+)\s*\=\s*(\S)(.{-})\2',
                \ '\=ctk:process_info(info)', 'g')

    let info.unmap = ''
    if has_key(info, 'hotkey')
        let hotkey = ''
        for m in split(info.hotkey, '\\\@<!,')
            let m = substitute(m, '\\,', ',', 'g')
            let mlist = matchlist(m, '^\v(.):(.*)')
            if !empty(mlist) && mlist[1] =~ '^[cr]$'
                let info.unmap .= s:mapping_keys(mlist[2], idx,
                            \ mlist[1] == 'c', 1)
                let hotkey = mlist[1].':'.mlist[2].','
            endif
        endfor
        let info.hotkey = (hotkey == '' ? '<empty>' : hotkey[:-2])
    endif

    return s:set_default_flags(info, idx)
endfunction

" }}}2
" s:delete_info {{{2

function! s:delete_info(idx)
    if a:idx < 0 || a:dix >= len(b:ctk.info)
        return
    endif

    exec b:ctk.info.unmap
    unlet b:ctk.info[a:idx]
    unlet b:ctk.unmap[len(b:ctk.info)]
endfunction

" }}}2
" s:find_compilers {{{2

function! s:find_compilers(cp)
    let idx = 0

    for c in b:ctk.info
        if c.name == a:cp
            return idx
        endif
        let idx += 1
    endfor

    return -1
endfunction

" }}}2
" s:file_autoname {{{2

function! s:file_autoname()
    if exists('b:'.g:ctk_filetype_ext_var)
        let ext = b:{g:ctk_filetype_ext_var}
    elseif &ft != ''
        let ext = &ft
    else
        return 1
    endif

    if expand('%') != '' || &bt != ''
        return 0
    endif

    let temp_folder = get(b:, 'ctk_temp_folder', g:ctk_temp_folder)
    if !isdirectory(temp_folder)
        call mkdir(temp_folder, 'p')
    endif
    let temp_folder = fnamemodify(temp_folder, ':p')

    if exists('b:ctk_temp_folder')
        let b:ctk_temp_folder = temp_folder
    else
        let g:ctk_temp_folder = temp_folder
    endif

    if exists('b:ctk_auto_generated_fname')
        let fname = b:ctk_auto_generated_fname
    else
        let fname = g:ctk_auto_generated_fname
    endif

    if !exists('g:ctk_idx')
        let g:ctk_idx = 1
    endif
    let idx = g:ctk_idx
    while filereadable(temp_folder.'/'.eval(fname).'.'.ext)
        let idx += 1
    endwhile
    let g:ctk_idx = idx + 1

    if getcwd() == $VIMRUNTIME
        exec 'lcd '.temp_folder
    endif

    silent exec 'file '.simplify(fnamemodify(temp_folder.glob('/').
                \ eval(fname).'.'.ext, ':.'))

    let b:ctk_generated_name = expand('%:p')
endfunction

" }}}2
" s:refresh_info {{{2

function! s:refresh_info(bang)
    " CTKtrace 'refresh_info() called'
    if a:bang == '!'
        silent! call s:unregister_compiler()
    endif

    " use cca.vim 's command
    if exists(':RefreshSnippets') == 2
        RefreshSnippets
        return
    endif

    let sf = g:ctk_compiler_info_folder
    for name in split(&ft, '\.')
        exec 'run! '.sf.'/'.name.'.vim '.sf.'/'.name.'_*.vim '.
                    \ sf.'/'.name.'/*.vim'
    endfor
endfunction

" }}}2
" s:mapping_keys {{{2

function! s:mapping_keys(hotkey, idx, c, u)
    if a:hotkey =~ '$num'
        let lhs = join(map(split(a:idx+1, '\zs'),
                    \'substitute(a:hotkey, "$num", v:val, "g")'), '')
    else
        let lhs = substitute(a:hotkey, '$id', a:idx+1, 'g')
    endif
    let cmd = a:c ? 'Compile' : 'Run'
    let unq = a:u ? '<unique>' : ''
    let rhs = '<C-\><C-N>:'.(a:idx+1).cmd.'<CR><C-\><C-G>'
    let unmap = ''
    try
        exec 'noremap '.unq.' '.lhs.' '.rhs
        let unmap .= 'unmap '.lhs.'|'
    catch
    endtry

    if lhs =~ '^<\(lt\)\@!'
        try
            exec 'inoremap '.unq.' '.lhs.' '.rhs
            let unmap .= 'iunmap '.lhs.'|'
        catch
        endtry
    endif

    return ''
endfunction

" }}}2
" s:set_default_flags {{{2

function! s:set_default_flags(info, idx)
    for key in filter(keys(a:info), 'v:val =~ "cmd$"')
        if empty(a:info[key])
            let a:info[key] = '!$output'
        elseif  a:info[key] !~ '^[:!*]'
            let a:info[key] = '!'.a:info[key]
        endif
    endfor

    if !has_key(a:info, 'cc_cmd') || a:info.cc_cmd == ''
        let a:info.cc_cmd = ':ListCompiler cur'
    endif

    if !has_key(a:info, 'input') || a:info.input == ''
        let a:info.input = '%:.'
    endif

    if !has_key(a:info, 'output') || a:info.output == ''
        let a:info.output = '%:t:r'
    endif
endfunction

" }}}2
" s:set_compiler_info {{{2
" a:cmd - 'name': key=value...

function! s:set_compiler_info(cmd, bang)
    " build b:ctk, if need
    if !exists('b:ctk')
        let b:ctk = {}
        let b:ctk.info = []
        let b:ctk.unmap = []
        let b:ctk.cur_ft = &ft
    endif

    " find name and others
    let mlist = matchlist(a:cmd, '^\v\s*(.{-})%(\\@<!\s+(.*)\s*)=$')

    let name = s:strtrim(substitute(mlist[1], '\\\ze\s', '', 'g'))
    " is name appeared?
    let idx = s:find_compilers(name)
    if name != '' && mlist[2] != ''
        " CTKtrace 'set_compiler_info() name = '.name.', value = '.mlist[2]
        return s:add_info(idx, name, mlist[2])
    endif

    " no keys, means list or delete
    if a:bang != '!'
        call s:show_list(b:ctk.info[idx])
    elseif idx != -1
        call s:delete_info(idx)
        echo 'deleted item '.idx.' done'
    else
        call s:echoerr('no such compiler info: '.mlist[1])
    endif
endfunction

" }}}2
" s:list_compiler {{{2

function! s:list_compiler(name, count)
    if s:find_source()
        return
    endif

    if a:count > 0
        if a:count < len(b:ctk.info)
            call s:show_list(b:ctk.info[a:count - 1])
        else
            call s:echoerr("the counts of info is ".len(b:ctk.info)
        endif
    elseif (a:name ==? 'cur' || a:name == '') && has_key(b:ctk, 'cur_info')
        call s:show_list(b:ctk.cur_info)
    elseif a:name ==? 'all' || a:name == ''
        for info in b:ctk.info
            call s:show_list(info)
        endfor
    else
        let idx = s:find_compilers(a:name)
        if idx != -1
            call s:show_list(b:ctk.info[idx])
        else
            call s:echoerr("no such compiler info: ".a:name)
        endif
    endif
endfunction

" }}}2
" s:run_cmd {{{2

function! s:run_cmd(cmd)
    " CTKtrace 'run_cmd() called, cmd = '.a:cmd
    let mlist = matchlist(a:cmd, '\v^([!:*])=(.*)$')
    if mlist[1] == '!' || mlist[1] == ''
        return system(mlist[2])
    endif
    if mlist[1] == ':'
        redir => output
        silent! exec mlist[2]
        redir END
        return output
    endif
    if mlist[1] == '*'
        return eval(mlist[2])
    endif
    return ''
endfunction

" }}}2
" s:read_modeline {{{2

function! s:read_modeline(begin, end, info)
    " CTKtrace 'read_modeline() called, from '.a:begin.' to '.a:end
    let pat = '\v<cc%(-([^:]*))=:\s*(.*)'
    let pat2 = '\v(\w+)\s*(\+)=\=\s*(\S)(.{-})\3'
    let pos = winsaveview()

    call cursor(a:begin, 1)
    while search(pat, '', a:end) != 0
        let mlist = matchlist(getline('.'), pat)
        if mlist[1] == '' || a:info.name =~ '^\V'.escape(mlist[1], '\')
            call substitute(mlist[2], pat2,
                        \ '\=ctk:process_modeline(a:info)', 'g')
        endif
    endwhile

    call winrestview(pos)
endfunction

" }}}2
" s:prepare_info {{{2

function! s:prepare_info(idx)
    let b:ctk.cur_info = copy(b:ctk.info[a:idx])
    let info = b:ctk.cur_info
    let pat = '\v\\@<!%(\%|#\d*)(%(:[p8~.htre]|:g=s(.).{-}\2.{-}\2)*)'
    
    if has_key(b:ctk, 'generated_name')
                \ && b:ctk.generated_name == expand('%:p')
        let info.output = substitute(info.output, pat,
                    \ '\=fnamemodify("'.g:ctk_temp_output.'", submatch(1))', 'g')
        if info.output =~ '\s'
            let info.output = shellescape(info.output)
        endif
    endif

    if &modeline
        let end_line = line('$')
        if end_line <= &modelines * 2
            call s:read_modeline(1, end_line, info)
        else
            call s:read_modeline(1, &mls, info)
            call s:read_modeline(end_line-&mls+1, end_line, info)
        endif
    endif

    for key in ['input', 'output']
        let val = ''
        for file in split(info[key], '\\\@<!\s\+')
            let file = substitute(file, '\\\ze\s', '', 'g')
            if file !~ '\\\@<![#%]\%(:.\)*'
                let file = fnamemodify(file, ':.')
                if file =~ '\s'
                    let file = shellescape(file)
                endif
            endif
            let val .= file.' '
        endfor
        let info[key] = s:strtrim(val)
    endfor

    call map(info, "substitute(v:val, '".pat."', '".
                \ '\=expand(submatch(0))'."', 'g')")
    call map(info, "substitute(v:val, '".'\\\ze[#%]'."', '', 'g')")

    return info
endfunction

" }}}2
" s:prepare_compile_cmd {{{2

function! s:prepare_compile_cmd(cmd)
    if !has_key(b:ctk, 'cur_info')
        return
    endif

    let cmd = a:cmd
    let info = b:ctk.cur_info
    for key in filter(keys(info), 'v:val !~ "cmd$"')
        let cmd = substitute(cmd, '\c\\\@<!\$\V'.escape(key, '\'),
                    \ info[key], 'g')
    endfor

    " CTKtrace 'prepare_compile_cmd() cmd = '.cmd
    return substitute(cmd, '\$', '$', 'g')
endfunction

" }}}2

" }}}1
" ==========================================================
" restore cpo {{{1

let &cpo = old_cpo

" }}}1
" vim: ft=vim:ff=unix:fdm=marker:ts=4:sw=4:et:sta:nu
snippets/actionscript_snippets.vim	[[[1
11
" ==========================================================
" File Name:    actionscript_snippets.vim
" Author:       StarWing
" Maintainer: 	Felix Ingram
" Version:      0.1
" Last Change:  2009-02-17 17:59:10
" ==========================================================
if exists('loaded_cca')
    let b:{cca_locale_tag_var} = { "start": "<{", "end"  : "}>", "cmd"  : ":"}
    DefineSnippet dm duplicateMovieClip(<{target}>, <{newName}>, <{depth}>);
endif
snippets/aspvbs_snippets.vim	[[[1
49
" ==========================================================
" File Name:    aspvbs_snippets.vim
" Author:       StarWing
" Maintainer: 	Felix Ingram
" Version:      0.1
" Last Change:  2009-02-17 18:09:49
" ==========================================================
" s:define_snippets {{{1

function! s:define_snippets()
    DefineSnippet rr          Response.Redirect(<{to}>)<{}>
    DefineSnippet app         Application("<{}>")<{}>
    DefineSnippet forin       For <{var}> in <{array}><CR><{}><CR>Next<CR><{}>
    DefineSnippet ifelse      If <{condition}> Then<CR><{}><CR>Else<CR><{}><CR>End if<CR><{}>
    DefineSnippet rw          Response.Write <{}>
    DefineSnippet sess        Session("<{}>")<{}>
    DefineSnippet rf          Request.Form("<{}>")<{}>
    DefineSnippet rq          Request.QueryString("<{}>")<{}>
    DefineSnippet while       While <{NOT}> <{condition}><CR><{}><CR>Wend<CR><{}>
endfunction

" }}}1
" s:set_compiler_info {{{1

function! s:set_compiler_info()
    
endfunction

" }}}1

if exists('loaded_cca')
    filetype indent on
    " for {{{ and }}} support
    set fdm=marker

    let b:{cca_filetype_ext_var} = 'vbs'
    let b:{cca_locale_tag_var} = { "start": "<{", "end"  : "}\>", "cmd"  : ":"}

    call s:define_snippets()
endif

if exists('loaded_ctk')
    let b:{ctk_filetype_ext_var} = 'vbs'
    call s:set_compiler_info()
endif

delfunc s:define_snippets
delfunc s:set_compiler_info
" vim: ft=vim:ff=unix:fdm=marker:ts=4:sts=4:sw=4:nu:et:sta:ai
snippets/c_snippets.vim	[[[1
95
" ==========================================================
" File Name:    c_snippets.vim
" Author:       StarWing
" Version:      0.1
" Last Change:  2009-01-30 19:59:17
" ==========================================================
" cca:c:arglist {{{2

function! cca:c:arglist()
    return repeat(', '.cca:make_tag(), cca:count(@z, '%[^%]'))
endfunction

" }}}2
" cca:c:get_file_name {{{2

function! cca:c:get_file_name(upper)
    let fname = expand('%:t')
    if a:upper == 1
        let fname = substitute(fname, '\.', '_', 'g')
        return '__'.toupper(fname).'__'
    else
        return empty(fname) ? cca:make_tag() : fname
    endif
endfunction

" }}}2
" s:set_compiler_info {{{2

function! s:set_compiler_info()
    SetCompilerInfo gcc         title="GNU Compiler Collection" hotkey="c:<m-c>" cc_cmd="gcc $input $flags -o $output" input="" output="" debug_cmd="gdb -q $output" run_cmd="" flags="-Wall" debug_flags="-g"
    SetCompilerInfo gcc\ asm    title="GNU Compiler Collection to ASM" hotkey="c:<m-C>" cc_cmd="gcc -S $input $flags -o $output" input="" output="%:t:r.asm" run_cmd=":sp $output" flags="-Wall"
    SetCompilerInfo vc6         title="Visual C++ 6.0" cc_cmd="cl $input $flags -o $output" debug_cmd="gdb -q $output" input="" output="" run_cmd="" flags="-W4" debug_flags=""
endfunction

" }}}2
" s:define_snippets {{{2

function! s:define_snippets()
    DefineSnippet df            #define <{}>
    DefineSnippet ic            #include "<{}>"
    DefineSnippet ud            #undef <{}>
    DefineSnippet ii            #include <<{}>>
    DefineSnippet fc            #if 0<CR><{}><CR>#endif<CR>
    DefineSnippet ff            #ifndef <C-R>=cca:c:get_file_name(1)<CR><CR>#define <C-R>=cca:c:get_file_name(1)<CR><CR><CR><CR><{}><CR><CR><CR>#endif /* <C-R>=cca:c:get_file_name(1)<CR> */
    DefineSnippet co            /* <{}> */
    DefineSnippet cc            /**< <{}> */
    DefineSnippet cr            <C-R>=repeat(' ', 60-strlen(getline('.')))<cr>/* <{}> */
    DefineSnippet cl            /<C-R>=repeat('*', 58)<CR>/
    DefineSnippet bc            /<C-R>=repeat('*', 59)<CR><CR><CR><BS><C-R>=repeat('*', 57)<CR>/
    DefineSnippet fh            /<C-R>=repeat('*', 59)<CR><CR>File Name:   <C-R>=cca:c:get_file_name(0)<CR><CR>Author:      <{}><CR>Version:     <{0.1:}><CR>Last Change: <C-R>=strftime("%Y-%m-%d %H:%M:%S")<CR><CR><BS><C-R>=repeat('*', 57)<CR>/<CR><{}>
    DefineSnippet main          int main(void)<CR>{<CR><{}><CR>return 0;<C-D><CR>}
    DefineSnippet main2         int main(int argc, char *argv[])<CR>{<CR><{}><CR>return 0;<C-D><CR>}
    DefineSnippet WinMain       int CALLBACK WinMain(HINSTANCE hInstance,HINSTANCE hPrevInstance,<CR><C-R>=repeat(' ',13)<CR>LPSTR lpszCmdLine, int nCmdShow)<CR>{<CR><{MessageBox(NULL, "Hello World!", "My First Win-App", MB_OK);}><CR>return 0;<C-D><CR>}

    DefineSnippet {             {<CR><{}><CR>}<CR><{}>
    DefineSnippet if            if (<{}>)<CR><{}>
    DefineSnippet else          else<CR><{}>
    DefineSnippet while         while (<{}>)<CR><{}>
    DefineSnippet do            do<CR>{<CR><{}><CR>}<CR>while (<{}>);<CR><{}>
    DefineSnippet for           for (<{int }><{i}> = <{0}>; <{i}> <{<}> <{len}>; <{<:@z=~'<'?'++':'--'}><{i}>)<CR><{}>
    DefineSnippet case          case <{}>:<CR><{}><CR>break;<C-D><CR><{}><CR>
    DefineSnippet switch        switch (<{}>)<CR>{<CR><C-D>case <{}>:<CR><{}><CR><BS>break;<CR><{}><CR>default:<C-D><CR><{}><CR>}<CR><{}>
    DefineSnippet struct        struct <{}><CR>{<CR><{}><CR>} <{}>;<CR><{}>
    DefineSnippet ts            typedef struct <{struct_name}>_tag<CR>{<CR><{}><CR>} <{struct_name}><{}>;<CR><{}>

    DefineSnippet printf        printf("<{%s:}><{\n}>"<{%s:cca:c:arglist()}>)<{}>
    DefineSnippet scanf         scanf("<{%s:}>"<{%s:cca:c:arglist()}>)<{}>
    DefineSnippet malloc        (<{int}>*)malloc(<{len}> * sizeof(<{int}>))
    DefineSnippet calloc        (<{int}>*)calloc(<{count}>, sizeof(<{int}>))
endfunction

" }}}2

if exists('loaded_cca')
    filetype indent on
    let b:{cca_filetype_ext_var} = 'c'
    let b:{cca_locale_tag_var} = {
            \ "start": "<{",
            \ "end"  : "}>",
            \ "cmd"  : ":"}
    call s:define_snippets()
    unlet b:{cca_locale_tag_var}
else
    delfunc cca:c:arglist
    delfunc cca:c:get_file_name
endif

if exists('loaded_ctk')
    let b:{ctk_filetype_ext_var} = 'c'
    call s:set_compiler_info()
endif

delfunc s:set_compiler_info
delfunc s:define_snippets
" vim: ft=vim:ff=unix:fdm=marker:ts=4:sts=4:sw=4:nu:et:sta:ai
snippets/common.vim	[[[1
14
" ==========================================================
" File Name:    common.vim
" Author:       StarWing
" Version:      0.1
" Last Change:  2009-01-30 14:59:37
" ==========================================================
if exists('loaded_cca')
    let b:{cca_locale_tag_var} = {'tagstart': '<{', 'tagend': '}>', 'command': ':'}

    DefineSnippet dt <{:strftime("%Y-%m-%d")}>
    DefineSnippet xt <{:strftime("%Y-%m-%d %H:%M:%S")}>
endif

" vim: ft=vim:fdm=marker:ts=4:sw=4:et:sta
snippets/css_snippets.vim	[[[1
62
" ==========================================================
" File Name:    css_snippets.vim
" Author:       StarWing
" Maintainer: 	Felix Ingram
" Version:      0.1
" Last Change:  2009-02-17 18:15:33
" ==========================================================
" s:define_snippets {{{1

function! s:define_snippets()
    DefineSnippet visibility  <{}>;<{}>
    DefineSnippet list        list-style-image: url(<{}>);<{}>
    DefineSnippet text        text-shadow: rgb(<{}>, <{}>, <{}>, <{}> <{}> <{}>;<{}>
    DefineSnippet overflow    overflow: <{}>;<{}>
    DefineSnippet white       white-space: <{}>;<{}>
    DefineSnippet clear       cursor: url(<{}>);<{}>
    DefineSnippet margin      padding-top: <{}>;<{}>
    DefineSnippet background  background #<{}> url(<{}>) <{}> <{}> top left/top center/top right/center left/center center/center right/bottom left/bottom center/bottom right/x% y%/x-pos y-pos')}>;<{}>
    DefineSnippet word        word-spaceing: <{}>;<{}>
    DefineSnippet z           z-index: <{}>;<{}>
    DefineSnippet vertical    vertical-align: <{}>;<{}>
    DefineSnippet marker      marker-offset: <{}>;<{}>
    DefineSnippet cursor      cursor: <{}>;<{}>
    DefineSnippet border      border-right: <{}>px <{}> #<{}>;<{}>
    DefineSnippet display     display: block;<{}>
    DefineSnippet padding     padding: <{}> <{}>;<{}>
    DefineSnippet letter      letter-spacing: <{}>em;<{}>
    DefineSnippet color       color: rgb(<{}>, <{}>, <{}>);<{}>
    DefineSnippet font        font-weight: <{}>;<{}>
    DefineSnippet position    position: <{}>;<{}>
    DefineSnippet direction   direction: <{}>;<{}>
    DefineSnippet float       float: <{}>;<{}>
endfunction

" }}}1
" s:set_compiler_info {{{1

function! s:set_compiler_info()
    
endfunction

" }}}1

if exists('loaded_cca')
    filetype indent on
    " for {{{ and }}} support
    set fdm=marker

    let b:{cca_filetype_ext_var} = 'css'
    let b:{cca_locale_tag_var} = { "start": "<{", "end"  : "}\>", "cmd"  : ":"}

    call s:define_snippets()
endif

if exists('loaded_ctk')
    let b:{ctk_filetype_ext_var} = 'css'
    call s:set_compiler_info()
endif

delfunc s:define_snippets
delfunc s:set_compiler_info
" vim: ft=vim:fdm=marker:ts=4:sts=4:sw=4:nu:et:sta:ai
snippets/django_model_snippets.vim	[[[1
71
" ==========================================================
" File Name:    django_model_snippets.vim
" Author:       StarWing
" Maintainer: 	Felix Ingram
" Version:      0.1
" Last Change:  2009-02-17 18:18:51
" ==========================================================
" s:define_snippets {{{1

function! s:define_snippets()
    DefineSnippet mmodel      class <{}>(models.Model):<CR>"""<{}>"""<CR><{}> = <{}><CR><CR>class Admin:<CR>pass<CR><CR>def __str__(self):<CR>return "<{s}>" % <{s:'('.repeat('<{}>, ', cca:count(@z, '%[^%]')).')'}><CR><{}>
    DefineSnippet mauto       models.AutoField(<{}>)<{}>
    DefineSnippet mbool       models.BooleanField()<{}>
    DefineSnippet mchar       models.CharField(maxlength=<{50}><{}>)<{}>
    DefineSnippet mcsi        models.CommaSeparatedIntegerField(maxlength=<{50}><{}>)<{}>
    DefineSnippet mdate       models.DateField(<{}>)<{}>
    DefineSnippet mdatet      models.DateTimeField(<{}>)<{}>
    DefineSnippet memail      models.EmailField(<{}>)<{}>
    DefineSnippet mfile       models.FileField(upload_to="<{}>"<{}>)<{}>
    DefineSnippet mfilep      models.FilePathField(path="<{}>"<{}>)<{}>
    DefineSnippet mfloat      models.FloatField(max_digits=<{}>, decimal_places=<{}>)<{}>
    DefineSnippet mimage      models.ImageField(<{}>)<{}>
    DefineSnippet mint        models.IntegerField(<{}>)<{}>
    DefineSnippet mipadd      models.IPAddressField(<{}>)<{}>
    DefineSnippet mnull       models.NullBooleanField()<{}>
    DefineSnippet mphone      models.PhoneNumberField(<{}>)<{}>
    DefineSnippet mpint       models.PositiveIntegerField(<{}>)<{}>
    DefineSnippet mspint      models.PositiveSmallIntegerField(<{}>)<{}>
    DefineSnippet mslug       models.SlugField(<{}>)<{}>
    DefineSnippet msint       models.SmallIntegerField(<{}>)<{}>
    DefineSnippet mtext       models.TextField(<{}>)<{}>
    DefineSnippet mtime       models.TimeField(<{}>)<{}>
    DefineSnippet murl        models.URLField(verify_exists=<{True}><{}>)<{}>
    DefineSnippet muss        models.USStateField(<{}>)<{}>
    DefineSnippet mxml        models.XMLField(schema_path="<{}>"<{}>)<{}>
    DefineSnippet mfor        models.ForeignKey(<{}>)<{}>
    DefineSnippet mm2o        models.ForeignKey(<{}>)<{}>
    DefineSnippet mm2m        models.ManyToManyField(<{}>)<{}>
    DefineSnippet mo2o        models.OneToOneField(<{}>)<{}>
    DefineSnippet mman        models.Manager()<{}>
endfunction

" }}}1
" s:set_compiler_info {{{1

function! s:set_compiler_info()
    
endfunction

" }}}1

if exists('loaded_cca')
    filetype indent on
    " for {{{ and }}} support
    set fdm=marker

    " TODO: what's the ext-name of this file ??
    " let b:{cca_filetype_ext_var} = '<{ext}>'
    let b:{cca_locale_tag_var} = { "start": "<{", "end"  : "}\>", "cmd"  : ":"}

    call s:define_snippets()
endif

if exists('loaded_ctk')
    " let b:{ctk_filetype_ext_var} = '<{ext}>'
    call s:set_compiler_info()
endif

delfunc s:define_snippets
delfunc s:set_compiler_info
" vim: ft=vim:ff=unix:fdm=marker:ts=4:sts=4:sw=4:nu:et:sta:ai
snippets/django_template_snippets.vim	[[[1
64
" ==========================================================
" File Name:    django_template_snippets.vim
" Author:       StarWing
" Maintainer: 	Felix Ingram
" Version:      0.1
" Last Change:  2009-02-17 18:26:45
" ==========================================================
" s:define_snippets {{{1

function! s:define_snippets()
    DefineSnippet {{          {% templatetag openvariable %}<{}>
    DefineSnippet }}          {% templatetag closevariable %}<{}>
    DefineSnippet {%          {% templatetag openblock %}<{}>
    DefineSnippet %}          {% templatetag closeblock %}<{}>
    DefineSnippet now         {% now "<{}>" %}<{}>
    DefineSnippet firstof     {% firstof <{}> %}<{}>
    DefineSnippet ifequal     {% ifequal <{}> <{}> %}<CR><{}><CR>{% endifequal %}<CR><{}>
    DefineSnippet ifchanged   {% ifchanged %}<{}>{% endifchanged %}<{}>
    DefineSnippet regroup     {% regroup <{}> by <{}> as <{}> %}<{}>
    DefineSnippet extends     {% extends "<{}>" %}<CR><{}>
    DefineSnippet filter      {% filter <{}> %}<CR><{}><CR>{% endfilter %}<{}>
    DefineSnippet block       {% block <{}> %}<CR><{}><CR>{% endblock %}<CR><{}>
    DefineSnippet cycle       {% cycle <{}> as <{}> %}<{}>
    DefineSnippet if          {% if <{}> %}<CR><{}><CR>{% endif %}<CR><{}>
    DefineSnippet debug       {% debug %}<CR><{}>
    DefineSnippet ifnotequal  {% ifnotequal <{}> <{}> %}<CR><{}><CR>{% endifnotequal %}<CR><{}>
    DefineSnippet include     {% include <{}> %}<CR><{}>
    DefineSnippet comment     {% comment %}<CR><{}><CR>{% endcomment %}<CR><{}>
    DefineSnippet for         {% for <{}> in <{}> %}<CR><{}><CR>{% endfor %}<CR><{}>
    DefineSnippet ssi         {% ssi <{}> <{}> %}<{}>
    DefineSnippet widthratio  {% widthratio <{}> <{}> <{}> %}<{}>
    DefineSnippet load        {% load <{}> %}<CR><{}>
    DefineSnippet field       <p><label for="id_<{fieldname}>"><{fieldlabel}>:</label> {{ form.<{fieldname}> }}<CR>{% if form.<{fieldname}>.errors %}*** {{ form.<{fieldname}>.errors|join:", " }} {% endif %}</p><{}>
endfunction

" }}}1
" s:set_compiler_info {{{1

function! s:set_compiler_info()
    
endfunction

" }}}1

if exists('loaded_cca')
    filetype indent on
    " for {{{ and }}} support
    set fdm=marker

    " TODO: what is the ext-name of this filetype?
    " let b:{cca_filetype_ext_var} = '<{ext}>'
    let b:{cca_locale_tag_var} = { "start": "<{", "end"  : "}\>", "cmd"  : ":"}

    call s:define_snippets()
endif

if exists('loaded_ctk')
    " let b:{ctk_filetype_ext_var} = '<{ext}>'
    call s:set_compiler_info()
endif

delfunc s:define_snippets
delfunc s:set_compiler_info
" vim: ft=vim:ff=unix:fdm=marker:ts=4:sts=4:sw=4:nu:et:sta:ai
snippets/f-script_snippets.vim	[[[1
47
" ==========================================================
" File Name:    f-script_snippets.vim
" Author:       StarWing
" Maintainer: 	Felix Ingram
" Version:      0.1
" Last Change:  2009-02-17 18:30:25
" ==========================================================
" s:define_snippets {{{1

function! s:define_snippets()
    DefineSnippet tbd         to:<{}> by:<{}> do:[ <{}> |<CR><{}><CR>].<{}>
    DefineSnippet it          ifTrue:[<CR><{}><CR>].<{}>
    DefineSnippet ift         ifFalse:[<CR><{}><CR>] ifTrue:[<CR><{}><CR>].<{}>
    DefineSnippet itf         ifTrue:[<CR><{}><CR>] ifFalse:[<CR><{}><CR>].<{}>
    DefineSnippet td          to:<{}> do:[<{}> <{}> |<CR><{}><CR>].<{}>
    DefineSnippet if          ifFalse:[<CR><{}><CR>].<{}>
endfunction

" }}}1
" s:set_compiler_info {{{1

function! s:set_compiler_info()
    
endfunction

" }}}1

if exists('loaded_cca')
    filetype indent on
    " for {{{ and }}} support
    set fdm=marker

    " TODO: what is the ext-name of this filetype ?
    " let b:{cca_filetype_ext_var} = '<{ext}>'
    let b:{cca_locale_tag_var} = { "start": "<{", "end"  : "}\>", "cmd"  : ":"}

    call s:define_snippets()
endif

if exists('loaded_ctk')
    " let b:{ctk_filetype_ext_var} = '<{ext}>'
    call s:set_compiler_info()
endif

delfunc s:define_snippets
delfunc s:set_compiler_info
" vim: ft=vim:ff=unix:fdm=marker:ts=4:sts=4:sw=4:nu:et:sta:ai
snippets/haskell_snippets.vim	[[[1
41
" ==========================================================
" File Name:    haskell_snippets.vim
" Author:       StarWing
" Maintainer: 	Felix Ingram
" Version:      0.1
" Last Change:  2009-02-17 18:32:46
" ==========================================================
" s:define_snippets {{{1

function! s:define_snippets()
    DefineSnippet mod         module: <{}> where<CR><Tab><{}>
endfunction

" }}}1
" s:set_compiler_info {{{1

function! s:set_compiler_info()
    
endfunction

" }}}1

if exists('loaded_cca')
    filetype indent on
    " for {{{ and }}} support
    set fdm=marker

    let b:{cca_filetype_ext_var} = 'hs'
    let b:{cca_locale_tag_var} = { "start": "<{", "end"  : "}\>", "cmd"  : ":"}

    call s:define_snippets()
endif

if exists('loaded_ctk')
    " let b:{ctk_filetype_ext_var} = 'hs'
    call s:set_compiler_info()
endif

delfunc s:define_snippets
delfunc s:set_compiler_info
" vim: ft=vim:ff=unix:fdm=marker:ts=4:sts=4:sw=4:nu:et:sta:ai
snippets/html_snippets.vim	[[[1
93
" ==========================================================
" File Name:    html_snippets.vim
" Author:       StarWing
" Maintainer: 	Felix Ingram
" Version:      0.1
" Last Change:  2009-02-17 18:33:43
" ==========================================================
let s:cpo_save = &cpo
set cpo&vim
" cca:html:select_doctype {{{1
function! cca:html:select_doctype()
    call inputsave()
    let dt = inputlist(['Select doctype:',
                \ '1. HTML 4.01',
                \ '2. HTML 4.01 Transitional',
                \ '3. HTML 4.01 Frameset',
                \ '4. XHTML 1.0 Frameset',
                \ '5. XHTML Strict',
                \ '6. XHTML Transitional',
                \ '7. XHTML Frameset'])
    call inputrestore()
    let dts = {1: "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"\n\"http://www.w3.org/TR/html4/strict.dtd\">",
             \ 2: "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"\n\"http://www.w3.org/TR/html4/loose.dtd\">",
             \ 3: "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"\n\"http://www.w3.org/TR/html4/frameset.dtd\">",
             \ 4: "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"\n\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">",
             \ 5: "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML Strict//EN\"\n\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">",
             \ 6: "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML Transitional//EN\"\n\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">",
             \ 7: "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML Frameset//EN\"\n\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">"}
    
    return dts[dt]
endfunction
" s:define_snippets {{{1

function! s:define_snippets()
    DefineSnippet doct        <C-R>=cca:html:select_doctype()<CR><CR><{}>
    DefineSnippet doctype     <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Frameset//EN"<CR><TAB>"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd"><CR><{}>
    DefineSnippet doc4s       <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"<CR>"http://www.w3.org/TR/html4/strict.dtd"><CR><{}>
    DefineSnippet doc4t       <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"<CR>"http://www.w3.org/TR/html4/loose.dtd"><CR><{}>
    DefineSnippet doc4f       <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Frameset//EN"<CR>"http://www.w3.org/TR/html4/frameset.dtd"><CR><{}>
    DefineSnippet docxs       <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML Strict//EN"<CR>"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"><CR><{}>
    DefineSnippet docxt       <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML Transitional//EN"<CR>"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"><CR><{}>
    DefineSnippet docxf       <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML Frameset//EN"<CR>"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd"><CR><{}>
    DefineSnippet head        <head><CR><meta http-equiv="Content-type" content="text/html; charset=utf-8" /><CR><title><{}></title><CR><{}><CR></head><CR><{}>
    DefineSnippet script      <script type="text/javascript" language="javascript" charset="utf-8"><CR>// <![CDATA[<CR><TAB><{}><CR>// ]]><CR></script><CR><{}>
    DefineSnippet title       <title><{}></title>
    DefineSnippet body        <body id="<{}>" <{}>><CR><{}><CR></body><CR><{}>
    DefineSnippet scriptsrc   <script src="<{}>" type="text/javascript" language="<{}>" charset="<{}>"></script><CR><{}>
    DefineSnippet textarea    <textarea name="<{}>" rows="<{}>" cols="<{}>"><{}></textarea><CR><{}>
    DefineSnippet meta        <meta name="<{}>" content="<{}>" /><CR><{}>
    DefineSnippet movie       <object width="<{}>" height="<{}>"<CR>classid="clsid:02BF25D5-8C17-4B23-BC80-D3488ABDDC6B"<CR>codebase="http://www.apple.com/qtactivex/qtplugin.cab"><CR><param name="src"<CR>value="<{}>" /><CR><param name="controller" value="<{}>" /><CR><param name="autoplay" value="<{}>" /><CR><embed src="<{}>"<CR>width="<{}>" height="<{}>"<CR>controller="<{}>" autoplay="<{}>"<CR>scale="tofit" cache="true"<CR>pluginspage="http://www.apple.com/quicktime/download/"<CR>/><CR></object><CR><{}>
    DefineSnippet div         <div <{}>><CR><{}><CR></div><CR><{}>
    DefineSnippet mailto      <a href="mailto:<{}>?subject=<{}>"><{}></a><{}>
    DefineSnippet table       <table border="<{}>"<{}> cellpadding="<{}>"><CR><tr><th><{}></th></tr><CR><tr><td><{}></td></tr><CR></table>
    DefineSnippet link        <link rel="<{}>" href="<{}>" type="text/css" media="<{}>" title="<{}>" charset="<{}>" />
    DefineSnippet form        <form action="<{}>" method="<{}>"><CR><{}><CR><CR><p><input type="submit" value="Continue &rarr;" /></p><CR></form><CR><{}>
    DefineSnippet ref         <a href="<{}>"><{}></a><{}>
    DefineSnippet h1          <h1 id="<{}>"><{}></h1><{}>
    DefineSnippet input       <input type="<{}>" name="<{}>" value="<{}>" <{}>/><{}>
    DefineSnippet style       <style type="text/css" media="screen"><CR>/* <![CDATA[ */<CR><{}><CR>/* ]]> */<CR></style><CR><{}>
    DefineSnippet base        <base href="<{}>"<{}> /><{}>
endfunction

" }}}1
" s:set_compiler_info {{{1

function! s:set_compiler_info()
    
endfunction

" }}}1

if exists('loaded_cca')
    filetype indent on
    " for {{{ and }}} support
    set fdm=marker

    let b:{cca_filetype_ext_var} = 'html'
    let b:{cca_locale_tag_var} = { "start": "<{", "end"  : "}\>", "cmd"  : ":"}

    call s:define_snippets()
endif

if exists('loaded_ctk')
    " let b:{ctk_filetype_ext_var} = 'html'
    call s:set_compiler_info()
endif

delfunc s:define_snippets
delfunc s:set_compiler_info

let &cpo = s:cpo_save
unlet s:cpo_save
" vim: ft=vim:ff=unix:fdm=marker:ts=4:sts=4:sw=4:nu:et:sta:ai
snippets/java_snippets.vim	[[[1
82
" ==========================================================
" File Name:    java_snippets.vim
" Author:       StarWing
" Maintainer: 	Felix Ingram
" Version:      0.1
" Last Change:  2009-02-17 18:38:44
" ==========================================================
" cca:java:test_filename {{{1

function! cca:java:test_filename(type)
    let filepath = expand('%:p')
    let filepath = substitute(filepath, '/', '.', 'g')
    let filepath = substitute(filepath, '^.\(:\\\)\?', '', '')
    let filepath = substitute(filepath, '\', '.', 'g')
    let filepath = substitute(filepath, ' ', '', 'g')
    let filepath = substitute(filepath, '.*test.', '', '')
    if a:type == 1
        let filepath = substitute(filepath, '.[A-Za-z]*.java', '', 'g')
    elseif a:type == 2
        let filepath = substitute(filepath, 'Tests.java', '', '')
    elseif a:type == 3
        let filepath = substitute(filepath, '.*\.\([A-Za-z]*\).java', '\1', 'g')
    elseif a:type == 4
        let filepath = substitute(filepath, 'Tests.java', '', '')
        let filepath = substitute(filepath, '.*\.\([A-Za-z]*\).java', '\1', 'g')
    elseif a:type == 5
        let filepath = substitute(filepath, 'Tests.java', '', '')
        let filepath = substitute(filepath, '.*\.\([A-Za-z]*\).java', '\1', 'g')
        let filepath = substitute(filepath, '.', '\l&', '')
    endif

    return filepath
endfunction

" s:define_snippets {{{1

function! s:define_snippets()
    DefineSnippet method      // {{{ <{method}><CR>/**<CR> * <{}><CR> */<CR>public <{return}> <{method}>() {<CR><{}>}<CR>// }}}<CR><{}>
    DefineSnippet jps         private static final <{string}> <{}> = "<{}>";<CR><{}>
    DefineSnippet jtc         try {<CR><{}><CR>} catch (<{}> e) {<CR><{}><CR>} finally {<CR><{}><CR>}<CR><{}>
    DefineSnippet jlog        /** Logger for this class and subclasses. */<CR><CR>protected final Log log = LogFactory.getLog(getClass());<CR><{}>
    DefineSnippet jpv         private <{string}> <{}>;<CR><CR><{}>
    DefineSnippet bean        // {{{ set<{fieldName:toupper(@z)}><CR><ESC>Vc/**<CR>Setter for <{fieldName}>.<CR>@param new<{fieldName:toupper(@z)}> new value for <{fieldName}><CR>*/<CR>public void set<{fieldName:toupper(@z)}>(<{String}> new<{fieldName:toupper(@z)}>) {<CR><{fieldName}> = new<{fieldName:toupper(@z)}>;<CR>}<CR>// }}}<CR><ESC>Vc<CR>// {{{ get<{fieldName:toupper(@z)}><CR><ESC>Vc/**<CR>Getter for <{fieldName}>.<CR>@return <{fieldName}><CR>*/<CR>public <{String}> get<{fieldName:toupper(@z)}>() {<CR>return <{fieldName}>;<CR>}<CR>// }}}<CR><ESC>Vc<{}>
    DefineSnippet jwh         while (<{}>) { // <{}><CR><CR><{}><CR><CR>}<CR><{}>
    DefineSnippet sout        System.out.println("<{}>");<{}>
    DefineSnippet jtest       package <{j:cca:java:test_filename(1)}><CR><CR>import junit.framework.TestCase;<CR>import <{j:cca:java:test_filename(2)}>;<CR><CR>/**<CR><{j:cca:java:test_filename(3)}><CR><CR>@author <{}><CR>@since <{}><CR>/<CR>public class <{j:cca:java:test_filename(3)}> extends TestCase {<CR><CR>private <{j:cca:java:test_filename(4)}> <{j:cca:java:test_filename(5)}>;<CR><CR>public <{j:cca:java:test_filename(4)}> get<{j:cca:java:test_filename(4)}>() { return this.<{j:cca:java:test_filename(5)}>; }<CR>public void set<{j:cca:java:test_filename(4)}>(<{j:cca:java:test_filename(4)}> <{j:cca:java:test_filename(5)}>) { this.<{j:cca:java:test_filename(5)}> = <{j:cca:java:test_filename(5)}>; }<CR><CR>public void test<{}>() {<CR><{}><CR>}<CR>}<CR><{}>
    DefineSnippet jif         if (<{}>) { // <{}><CR><{}><CR>}<CR><{}>
    DefineSnippet jelse       if (<{}>) { // <{}><CR><CR><{}><CR><CR>} else { // <{}><CR><{}><CR>}<CR><{}>
    DefineSnippet jpm         /**<CR> * <{}><CR> *<CR> * @param <{}> <{}><CR> * <{}> <{}><CR> */<CR>private <{void}> <{}>(<{String}> <{}>) {<CR><CR><{}><CR><CR>}<CR><{}>
    DefineSnippet main        public main static void main(String[] ars) {<CR><{"System.exit(0)"}>;<CR>}<CR><{}>
    DefineSnippet jpum        /**<CR> * <{}><CR> *<CR> * @param <{}> <{}><CR> *<{}> <{}><CR> */<CR>public <{void}> <{}>(<{String}> <{}>) {<CR><CR><{}><CR><CR>}<CR><{}>
    DefineSnippet jcout       <c:out value="${<{}>}" /><{}>
endfunction

" }}}1
" s:set_compiler_info {{{1

function! s:set_compiler_info()
    
endfunction

" }}}1

if exists('loaded_cca')
    filetype indent on
    " for {{{ and }}} support
    set fdm=marker

    let b:{cca_filetype_ext_var} = 'java'
    let b:{cca_locale_tag_var} = { "start": "<{", "end"  : "}\>", "cmd"  : ":"}

    call s:define_snippets()
endif

if exists('loaded_ctk')
    let b:{ctk_filetype_ext_var} = 'java'
    call s:set_compiler_info()
endif

delfunc s:define_snippets
delfunc s:set_compiler_info
" vim: ft=vim:ff=unix:fdm=marker:ts=4:sts=4:sw=4:nu:et:sta:ai
snippets/javascript_snippets.vim	[[[1
40
" ==========================================================
" File Name:    javascript_snippets.vim
" Author:       StarWing
" Maintainer: 	Felix Ingram
" Version:      0.1
" Last Change:  2009-02-17 18:46:11
" ==========================================================
" s:define_snippets {{{1

function! s:define_snippets()
    DefineSnippet proto      <{className}>.prototype.<{methodName}> = function(<{}>)<CR>{<CR><{}><CR>};<CR><{}>
    DefineSnippet func       function <{functionName}> (<{}>)<CR>{<CR><Tab><{}><CR><BS>}<CR><{}>
endfunction

" }}}1
" s:set_compiler_info {{{1

function! s:set_compiler_info()
    
endfunction

" }}}1

if exists('loaded_cca')
    filetype indent on

    let b:{cca_filetype_ext_var} = 'js'
    let b:{cca_locale_tag_var} = { "start": "<{", "end"  : "}\>", "cmd"  : ":"}

    call s:define_snippets()
endif

if exists('loaded_ctk')
    let b:{ctk_filetype_ext_var} = 'js'
    call s:set_compiler_info()
endif

delfunc s:define_snippets
delfunc s:set_compiler_info
" vim: ft=vim:ff=unix:fdm=marker:ts=4:sts=4:sw=4:nu:et:sta:ai
snippets/latex_snippets.vim	[[[1
46
" ==========================================================
" File Name:    latex_snippets.vim
" Author:       StarWing
" Maintainer: 	Felix Ingram
" Version:      0.1
" Last Change:  2009-02-17 18:51:47
" ==========================================================
" s:define_snippets {{{1

function! s:define_snippets()
    DefineSnippet sub         \subsection{<{name}>}\label{sub:<{name:substitute(@z,'.','\l&','g')}>}<CR><{}>
    DefineSnippet $$          \[<CR><{}><CR>\]<CR><{}>
    DefineSnippet ssub        \subsubsection{<{name}>}\label{ssub:<{name:substitute(@z,'.','\l&','g')}>}<CR><{}>
    DefineSnippet itd         \item[<{desc}>] <{}>
    DefineSnippet sec         \section{<{name}>}\label{sec:<{name:substitute(@z,'.','\l&','g')}>}<CR><{}>
endfunction

" }}}1
" s:set_compiler_info {{{1

function! s:set_compiler_info()
    
endfunction

" }}}1

if exists('loaded_cca')
    filetype indent on
    
    

    " TODO: what is the ext-name of this filetype ?
    " let b:{cca_filetype_ext_var} = '<{ext}>'
    let b:{cca_locale_tag_var} = { "start": "<{", "end"  : "}\>", "cmd"  : ":"}

    call s:define_snippets()
endif

if exists('loaded_ctk')
    " let b:{ctk_filetype_ext_var} = '<{ext}>'
    call s:set_compiler_info()
endif

delfunc s:define_snippets
delfunc s:set_compiler_info
" vim: ft=vim:ff=unix:fdm=marker:ts=4:sts=4:sw=4:nu:et:sta:ai
snippets/logo_snippets.vim	[[[1
40
" ==========================================================
" File Name:    logo_snippets.vim
" Author:       StarWing
" Maintainer: 	Felix Ingram
" Version:      0.1
" Last Change:  2009-02-17 18:55:02
" ==========================================================
" s:define_snippets {{{1

function! s:define_snippets()
    DefineSnippet to          to <{name}> <{argument}><CR><{}><CR>end<CR><{}>
endfunction

" }}}1
" s:set_compiler_info {{{1

function! s:set_compiler_info()
    
endfunction

" }}}1

if exists('loaded_cca')
    filetype indent on

    " TODO: what is the ext-name of this filetype ?
    " let b:{cca_filetype_ext_var} = '<{ext}>'
    let b:{cca_locale_tag_var} = { "start": "<{", "end"  : "}\>", "cmd"  : ":"}

    call s:define_snippets()
endif

if exists('loaded_ctk')
    " let b:{ctk_filetype_ext_var} = '<{ext}>'
    call s:set_compiler_info()
endif

delfunc s:define_snippets
delfunc s:set_compiler_info
" vim: ft=vim:ff=unix:fdm=marker:ts=4:sts=4:sw=4:nu:et:sta:ai
snippets/markdown_snippets.vim	[[[1
41
" ==========================================================
" File Name:    markdown_snippets.vim
" Author:       StarWing
" Maintainer: 	Felix Ingram
" Version:      0.1
" Last Change:  2009-02-17 19:01:11
" ==========================================================
" s:define_snippets {{{1

function! s:define_snippets()
    DefineSnippet img       : ![<{altText}>](<{SRC}>)<{}>
    DefineSnippet link      : [<{desc}>](<{HREF}>)<{}>
endfunction

" }}}1
" s:set_compiler_info {{{1

function! s:set_compiler_info()
    
endfunction

" }}}1

if exists('loaded_cca')
    filetype indent on

    " TODO: what is the ext-name of this filetype ?
    " let b:{cca_filetype_ext_var} = '<{ext}>'
    let b:{cca_locale_tag_var} = { "start": "<{", "end"  : "}\>", "cmd"  : ":"}

    call s:define_snippets()
endif

if exists('loaded_ctk')
    " let b:{ctk_filetype_ext_var} = '<{ext}>'
    call s:set_compiler_info()
endif

delfunc s:define_snippets
delfunc s:set_compiler_info
" vim: ft=vim:ff=unix:fdm=marker:ts=4:sts=4:sw=4:nu:et:sta:ai
snippets/movable_type_snippets.vim	[[[1
45
" ==========================================================
" File Name:    movable_type_snippets.vim
" Author:       StarWing
" Maintainer: 	Felix Ingram
" Version:      0.1
" Last Change:  2009-02-17 19:04:28
" ==========================================================
" s:define_snippets {{{1

function! s:define_snippets()
    DefineSnippet cat         <$MTCategoryDescription$><{}>
    DefineSnippet blog        <$MTBlogName$><{}>
    DefineSnippet archive     <$MTArchiveFile$><{}>
    DefineSnippet cal         <MTCalendarIfEntries><CR><Tab><{}><CR></MTCalendarIfEntries><CR><{}>
    DefineSnippet entry       <$MTEntryMore$><{}>
    DefineSnippet entries     <MTEntriesHeader><CR><Tab><{}><CR></MTEntriesHeader><CR><{}>
endfunction

" }}}1
" s:set_compiler_info {{{1

function! s:set_compiler_info()
    
endfunction

" }}}1

if exists('loaded_cca')
    filetype indent on

    " TODO: what is the ext-name of this filetype ?
    " let b:{cca_filetype_ext_var} = '<{ext}>'
    let b:{cca_locale_tag_var} = { "start": "<{", "end" : "}\>", "cmd" : ":"}

    call s:define_snippets()
endif

if exists('loaded_ctk')
    " let b:{ctk_filetype_ext_var} = '<{ext}>'
    call s:set_compiler_info()
endif

delfunc s:define_snippets
delfunc s:set_compiler_info
" vim: ft=vim:ff=unix:fdm=marker:ts=4:sts=4:sw=4:nu:et:sta:ai
snippets/objc_snippets.vim	[[[1
58
" ==========================================================
" File Name:    objc_snippets.vim
" Author:       StarWing
" Maintainer: 	Felix Ingram
" Version:      0.1
" Last Change:  2009-02-17 19:07:01
" ==========================================================
" s:define_snippets {{{1

function! s:define_snippets()
    DefineSnippet cat         @interface <{NSObject}> (<{Category}>)<CR><CR>@end<CR><CR><CR>@implementation <{NSObject}> (<{Category}>)<CR><CR><{}><CR><CR>@end<CR><{}>
    DefineSnippet delacc      - (id)delegate;<CR><CR>- (void)setDelegate:(id)delegate;<CR><{}>
    DefineSnippet ibo         IBOutlet <{NSSomeClass}> *<{someClass}>;<CR><{}>
    DefineSnippet dict        NSMutableDictionary *<{dict}> = [NSMutableDictionary dictionary];<CR><{}>
    DefineSnippet Imp         #import <<{}>.h><CR><{}>
    DefineSnippet objc        @interface <{class}> : <{NSObject}><CR>{<CR>}<CR>@end<CR><CR>@implementation <{class}><CR>- (id)init<CR>{<CR>self = [super init]; <CR>if (self != nil)<CR>{<CR><{}><CR>}<CR>return self;<CR>}<CR>@end<CR><{}>
    DefineSnippet imp         #import "<{}>.h"<CR><{}>
    DefineSnippet bez         NSBezierPath *<{path}> = [NSBezierPath bezierPath];<CR><{}>
    DefineSnippet acc         - (<{"unsigned int"}>)<{thing}><CR>{<CR>return <{fThing}>;<CR>}<CR><CR>- (void)set<{thing:toupper(@z)}>:(<{"unsigned int"}>)new<{thing}><CR>{<CR><{fThing}> = new<{thing}>;<CR>}<CR><{}>
    DefineSnippet format      [NSString stringWithFormat:@"<{}>", <{}>]<{}>
    DefineSnippet focus       [self lockFocus];<CR><CR><{}><CR><CR>[self unlockFocus];<CR><{}>
    DefineSnippet setprefs    [[NSUserDefaults standardUserDefaults] setObject:<{object}> forKey:<{key}>];<CR><{}>
    DefineSnippet log         NSLog(@"%s<{s}>", <{s:repeat(', <{}>', cca:count(@z, '%[^%]'))}>);<{}>
    DefineSnippet gsave       [NSGraphicsContext saveGraphicsState];<CR><{}><CR>[NSGraphicsContext restoreGraphicsState];<CR><{}>
    DefineSnippet forarray    for(unsigned int index = 0; index < [<{array}> count]; index += 1)<CR>{<CR><{id}>object = [<{array}> objectAtIndex:index];<CR><{}><CR>}<{}>
    DefineSnippet classi      @interface <{ClassName}> : <{NSObject}><CR><CR>{<{}><CR><CR>}<CR><CR><{}><CR><CR>@end<CR><{}>
    DefineSnippet array       NSMutableArray *<{array}> = [NSMutableArray array];<{}>
    DefineSnippet getprefs    [[NSUserDefaults standardUserDefaults] objectForKey:<key>];<{}>
    DefineSnippet cati        @interface <{NSObject}> (<{Category}>)<CR><CR><{}><CR><CR>@end<CR><{}>
endfunction

" }}}1
" s:set_compiler_info {{{1

function! s:set_compiler_info()
    
endfunction

" }}}1

if exists('loaded_cca')
    filetype indent on

    " TODO: what is the ext-name of this filetype ?
    " let b:{cca_filetype_ext_var} = '<{ext}>'
    let b:{cca_locale_tag_var} = { "start": "<{", "end" : "}\>", "cmd" : ":"}

    call s:define_snippets()
endif

if exists('loaded_ctk')
    " let b:{ctk_filetype_ext_var} = '<{ext}>'
    call s:set_compiler_info()
endif

delfunc s:define_snippets
delfunc s:set_compiler_info
" vim: ft=vim:ff=unix:fdm=marker:ts=4:sts=4:sw=4:nu:et:sta:ai
snippets/ocaml_snippets.vim	[[[1
57
" ==========================================================
" File Name:    ocaml_snippets.vim
" Author:       StarWing
" Maintainer: 	Felix Ingram
" Version:      0.1
" Last Change:  2009-02-17 19:14:45
" ==========================================================
" s:define_snippets {{{1

function! s:define_snippets()
    DefineSnippet Queue       Queue.fold <{}> <{base}> <{q}><CR><{}>
    DefineSnippet Nativeint   Nativeint.abs <{ni}><{}>
    DefineSnippet Printexc    Printexc.print <{fn}> <{x}><{}>
    DefineSnippet Sys         Sys.Signal_ignore<{}>
    DefineSnippet Hashtbl     Hashtbl.iter <{}> <{h}><{}>
    DefineSnippet Array       Array.map <{}> <{arr}><{}>
    DefineSnippet Printf      Printf.fprintf <{buf}> "<{format}>" <{args}><{}>
    DefineSnippet Stream      Stream.iter <{}> <{stream}><{}>
    DefineSnippet Buffer      Buffer.add_channel <{buf}> <{ic}> <{len}><{}>
    DefineSnippet Int32       Int32.abs <{i32}><{}>
    DefineSnippet List        List.rev_map <{}> <{lst}><{}>
    DefineSnippet Scanf       Scanf.bscaf <{sbuf}> "<{format}>" <{f}><{}>
    DefineSnippet Int64       Int64.abs <{i64}><{}>
    DefineSnippet Map         Map.Make <{}>
    DefineSnippet String      String.iter <{}> <{str}><{}>
    DefineSnippet Genlex      Genlex.make_lexer <{"tok_lst"}> <{"char_stream"}><{}>
    DefineSnippet for         for <{i}> = <{}> to <{}> do<CR><{}><CR>done<CR><{}>
    DefineSnippet Stack       Stack.iter <{}> <{stk}><{}>
endfunction

" }}}1
" s:set_compiler_info {{{1

function! s:set_compiler_info()
    
endfunction

" }}}1

if exists('loaded_cca')
    filetype indent on

    " TODO: what is the ext-name of this filetype ?
    " let b:{cca_filetype_ext_var} = '<{ext}>'
    let b:{cca_locale_tag_var} = { "start": "<{", "end" : "}\>", "cmd" : ":"}

    call s:define_snippets()
endif

if exists('loaded_ctk')
    " let b:{ctk_filetype_ext_var} = '<{ext}>'
    call s:set_compiler_info()
endif

delfunc s:define_snippets
delfunc s:set_compiler_info
" vim: ft=vim:ff=unix:fdm=marker:ts=4:sts=4:sw=4:nu:et:sta:ai
snippets/vim_snippets.vim	[[[1
50
" ==========================================================
" File Name:    vim_snippets.vim
" Author:       StarWing
" Version:      0.1
" Last Change:  2009-01-30 10:14:25
" ==========================================================
" s:define_snippets {{{1

function! s:define_snippets()
    DefineSnippet ds        DefineSnippet <{}><TAB><TAB><TAB><{}>
    DefineSnippet blk: {{{<C-R>=foldlevel(line('.'))+1<CR><CR><{}><CR>}}}<C-R>=foldlevel(line('.'))+1<CR>
    DefineSnippet lc        <ESC>0d$a" <C-R>=repeat('=', 58)<CR><CR>
    DefineSnippet le        <C-R>=repeat('=', 58)<CR><CR><bs><bs>
    DefineSnippet fh        " <C-R>=repeat('=', 58)<CR><CR>File Name:    <C-R>=expand('%:t')<CR><CR>Author:       <{}><CR>Version:      <{0.1}><CR>Last Change:  <C-R>=strftime("%Y-%m-%d %H:%M:%S")<CR><CR><C-R>=repeat('=', 58)<CR>
    DefineSnippet cpo       let s:cpo_save = &cpo<CR>set cpo&vim<CR><CR><{}><CR><CR>let &cpo = s:cpo_save<CR>unlet s:cpo_save
    DefineSnippet func      <esc>0d$a" <{function_name:}> {{{<C-R>=foldlevel(line('.'))<CR><CR><bs><bs><CR>function! <{function_name:}>(<{}>)<CR><{}><CR>endfunction<CR><CR>" }}}<C-R>=foldlevel(line('.'))<CR>
    DefineSnippet if        if <{}><CR><{}><CR>endif<CR><{}>
    DefineSnippet while     while <{}><CR><{}><CR>endwhile<CR><{}>
    DefineSnippet for       for <{}> in <{}><CR><CR>endfor<CR><{}>
    DefineSnippet try       try<CR><{}><CR>catch <{}><CR><{}><CR>endtry<CR><{}>
endfunction

" }}}1
" s:set_compiler_info {{{1

function! s:set_compiler_info()

endfunction

" }}}1

if exists('loaded_cca')
    filetype indent on
    " for {{{ and }}} support
    set fdm=marker

    let b:{cca_filetype_ext_var} = 'vim'
    let b:{cca_locale_tag_var} = { "start": "<{", "end"  : "}>", "cmd"  : ":"}

    call s:define_snippets()
endif

if exists('loaded_ctk')
    let b:{ctk_filetype_ext_var} = 'vim'
    call s:set_compiler_info()
endif

delfunc s:define_snippets
delfunc s:set_compiler_info
" vim: ft=vim:fdm=marker:ts=4:sts=4:sw=4:nu:et:sta:ai
templates/stdc.c	[[[1
9
/* cca: start="<{" : end="}>" : */
#include <stdio.h>
#include <stdlib.h>

int main(void)
{
    <{}>
    return 0;
}
templates/stdc2.c	[[[1
9
/* cca: start="<{" : end="}>" : */
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
    <{}>
    return 0;
}
templates/stdcpp.cpp	[[[1
9
// cca: start="<{" : end="}>" :
#include <iostream>
using namespace std;

int main(void)
{
    <{}>
    return 0;
}
templates/stdcpp2.cpp	[[[1
9
// cca: start="<{" : end="}>" :
#include <iostream>
using namespace std;

int main(int argc, char **argv)
{
    <{}>
    return 0;
}
templates/stdwin.c	[[[1
56
#define _WIN32_WINNT 0x0501
#include <Windows.h>

LRESULT CALLBACK WndProc(HWND hWnd, UINT uMsg,
                         WPARAM wParam, LPARAM lParam)
{
    switch (uMsg)
    {
    case WM_PAINT:
    {
        PAINTSTRUCT ps;
        HDC hdc = BeginPaint(hWnd, &ps);

        TextOut(hdc, 10, 10, TEXT("Hello World!"), -1);

        EndPaint(hWnd, &ps);
        return 0;
    }
    case WM_DESTROY:
        PostQuitMessage(0);
        break;
    }
    return DefWindowProc(hWnd, uMsg, wParam, lParam);
}

int CALLBACK WinMain(HINSTANCE hInst, HINSTANCE hPreInst,
                     LPSTR strCmdLine, int nCmdShow)
{
    MSG msg;
    HWND hWnd;
    WNDCLASSEX wc =
    {
        sizeof(wc), CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS,
        WndProc, 0, 0, hInst,
        LoadIcon(NULL, IDI_APPLICATION), LoadCursor(NULL, IDC_ARROW),
        (HBRUSH)GetStockObject(WHITE_BRUSH), NULL, "My Class", NULL
    };

    hWnd = CreateWindowEx(0,
                          MAKEINTRESOURCE(RegisterClassEx(&wc)),
                          "My Window", WS_OVERLAPPEDWINDOW,
                          CW_USEDEFAULT, CW_USEDEFAULT, 320, 240,
                          NULL, NULL, hInst, NULL);
    if (!IsWindow(hWnd))
        return 1;
    ShowWindow(hWnd, nCmdShow);
    UpdateWindow(hWnd);
    while (GetMessage(&msg, NULL, 0, 0))
    {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }
    return msg.wParam;
}

/* cc: flags+='-mwindows -static': */
