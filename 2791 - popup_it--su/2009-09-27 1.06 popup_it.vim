"    Author:  Fvw (vimtexhappy@gmail.com)
"             Auto complete popup plugin
"   Version:  v01.06
"   Created:  2009-09-26
"   License:  Copyright (c) 2001-2009, Fvw
"             GNU General Public License version 2 for more details.
"     Usage:  Put this file in your VIM plugins dir
"             Add usr type:
"             let g:usr_pp= {}
"             let g:usr_pp["type"] = [
"                         \ {'cmd'     : "\<c-n>",
"                         \  'pattern' : ['xx', 'yy'],
"                         \  'exclude' : ['zz'],
"                         \ },
"                         \{item2}
"                         \{item3}
"                         \]
"             "*" type would be append to every type.
"
"             Use :PopupType type change now popupType
"pp_it.vim: {{{1
if v:version < 700 || exists("loaded_pp_it")
    finish
endif
let loaded_pp_it= 1

let s:keys    = [
            \ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k',
            \ 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
            \ 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G',
            \ 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',
            \ 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '0', '1', '2',
            \ '3', '4', '5', '6', '7', '8', '9', '-', '_', '~', '^',
            \ '.', ',', ':', '!', '#', '=', '%', '$', '@', '<', '>',
            \ '/', '\']

autocmd BufEnter,BufRead * call <SID>pp_run()
autocmd FileType * call <SID>pp_run()
amenu <silent> &Popup.Run :call <SID>pp_run()<CR>
amenu <silent> &Popup.Clr :call <SID>pp_clr()<CR>

command PopupRun call <SID>pp_run()
command PopupClr call <SID>pp_clr()

command -nargs=? -complete=customlist,g:pp_types PopupType call <SID>set_pp_type(<q-args>)

"Map {{{1
function! s:map_key()
    silent! inoremap <silent> <buffer> <expr> <c-x><c-o>
                \ (pumvisible()?'<c-y>':'').
                \ '<c-x><c-o><c-r>=<SID>pp_fix("OmniTips")<cr>'
    silent! inoremap <silent> <buffer> <expr> <c-n>
                \ (pumvisible()?'<c-n>':
                \ '<c-n><c-r>=<SID>pp_fix("CtrlNTips")<cr>')
    silent! inoremap <silent> <buffer> <expr> <c-b>
                \ (pumvisible()?'<c-y>':'').
                \ '<c-n><c-r>=<SID>pp_fix("CtrlNTips")<cr>'

    "use \<c-r> insert fix char
    inoremap <silent> <buffer> <Plug>PopupFix <c-r>=<SID>pp_fix()<cr>

    for key in s:keys
        if maparg(key, 'i') == ""
            exec "silent! inoremap <silent> <buffer> ".key." ".key.
                        \ "\<c-r>=\<SID>pp_chk()\<cr>"
        endif
    endfor
    nnoremap <silent> <buffer> i i<c-r>=<SID>pp_chk()<cr>
    nnoremap <silent> <buffer> a a<c-r>=<SID>pp_chk()<cr>
    if has("autocmd") && exists("+omnifunc")
        if &omnifunc == ""
            setlocal omnifunc=syntaxcomplete#Complete
        endif
    endif
endfunction

function! s:unmap_key()
    for key in s:keys
        if maparg(key, 'i') =~ 'pp_chk'
            exec "silent! iunmap <buffer> ".key
        endif
    endfor
    silent! iunmap <buffer> <c-x><c-o>
    silent! iunmap <buffer> <c-n>
    silent! iunmap <buffer> <c-p>
    silent! iunmap <buffer> <c-b>
    silent! iunmap <buffer> <Plug>pp_fix
    silent! nunmap <buffer> i
    silent! nunmap <buffer> a
endfunction

"get_sid {{{1
fun! s:get_sid()
    return matchstr(expand('<sfile>'), '<SNR>\d\+_')
endfun

"pp_clr {{{1
fun! s:pp_clr()
    call s:unmap_key()
    exec "silent! aunmenu &Popup.Type"
endfun

"pp_run {{{1
fun! s:pp_run()
    "--------------------------------------------------
    let s:all_pps  = {}
    let b:pp_new  = []
    let b:pp_last_fail  = {}
    "--------------------------------------------------
    "idx == 0  -> no fail
    call g:set_pp_tips("")
    call s:update_last_fail(0)
    call s:make_pps()
    call s:set_pp_type(&ft)
    call s:map_key()
    call g:pp_reset()
endfun

"pp_fix {{{1
fun! s:pp_fix(...)
    "Don't use feedkeys , because if the complete
    "very slow the feedkeys would add key after
    "some use input
    if !pumvisible()
        call g:set_pp_tips("")
        return "\<c-e>"
    else
        "clean
        call s:update_last_fail(0)
        if a:0 == 1
            call g:set_pp_tips(a:1)
        endif
        "return "\<c-p>\<down>"
        return "\<c-p>"
    endif
endfun


"pp_chk {{{1
fun! s:pp_chk()
    "--------------------------------------------------
    "ignore
    if &paste || s:pp_is_pause()
        return ""
    end
    "skip some pumvisible tips
    "SnipTips" "SelTips" for snip plugin tab_it.vim
    if pumvisible()
                \&& (g:get_pp_tips() == "SnipTips"
                \  ||g:get_pp_tips() == "SelTips"
                \  ||g:get_pp_tips() == "CtrlNTips"
                \  ||g:get_pp_tips() == "CtrlPTips"
                \  ||g:get_pp_tips() == "OmniTips"
                \)
        return ""
    endif
    "--------------------------------------------------
    let idx = col('.')-2
    if idx >= 0
        let lstr = getline('.')[:idx]
    else
        let lstr = ''
    endif
    let i = 0
    for cpl in b:pp_new
        let i += 1
        if s:is_match(lstr, cpl.pattern)
                    \ && !(has_key(cpl,'exclude') && s:is_match(lstr, cpl.exclude))
            if (pumvisible() && g:get_pp_tips() == "AutoTips".i)
                "This match already pumvisible
                return ""
            endif
            if s:is_last_fail(i)
                "Update
                call s:update_last_fail(i)
                return ""
            endif
            if pumvisible()
                call feedkeys("\<c-e>", 'n')
            endif
            if cpl.cmd == "\<c-n>" || cpl.cmd == "\<c-x>\<c-o>"
                call feedkeys(cpl.cmd, 'n')
            else
                "<C-R>= can't remap use feedkeys can remap
                call feedkeys(cpl.cmd, 'm')
            end
            call g:set_pp_tips("AutoTips".i)
            "Set first , Clear in pp_fix if pum ok"
            call s:update_last_fail(i)
            "Use plug for silent
            call feedkeys("\<Plug>PopupFix", 'm')
            return ""
        endif
    endfor
    return ""
endfun
fun! s:is_match(str, list)
    for val in a:list
        if val != "" && a:str =~ '\m\C'.val.'$'
            return 1
        endif
    endfor
    return 0
endfun
fun! s:update_last_fail(idx)
    let b:pp_last_fail['col'] = col('.')
    let b:pp_last_fail['idx'] = a:idx
endfun
fun! s:is_last_fail(idx)
    if col('.') - b:pp_last_fail['col'] == 1
                \ && b:pp_last_fail['idx'] == a:idx
        return 1
    endif
    return 0
endfun

"Tips {{{1
function! g:set_pp_tips(w)
    let b:pp_tips = a:w
endfun
function! g:get_pp_tips()
    return b:pp_tips
endfun
"pp pause{{{1
function! g:pp_pause()
    let b:pp_pasue = 1
endfun
function! g:pp_continue()
    let b:pp_pasue = 0
endfun
function! g:pp_reset()
    let b:pp_pasue = 0
endfun
function! s:pp_is_pause()
    return b:pp_pasue
endfun

"ExtendType: {{{1
fun! s:extend_type(list1, list2)
    for item in a:list2
        if !s:has_cmd(a:list1, item)
            call add(a:list1, deepcopy(item))
        endif
    endfor
endfun
fun! s:has_cmd(list, chk)
    for item in a:list
        if item.cmd == a:chk.cmd
            return 1
        endif
    endfor
    return 0
endfun

"MakeAllPopup: {{{1
fun! s:make_pps()
    let s:all_pps = {}
    if exists("g:usr_pp") && type(g:usr_pp) ==  type({})
        let s:all_pps = deepcopy(g:usr_pp)
        for type in keys(s:def_pp)
            if !has_key(s:all_pps, type)
                let s:all_pps[type] = deepcopy(s:def_pp[type])
            else
                call s:extend_type(s:all_pps[type], s:def_pp[type])
            endif
        endfor
    else
        let s:all_pps = deepcopy(s:def_pp)
    endif

    exec "silent! aunmenu &Popup.Type"
    for type in keys(s:all_pps)
        silent exec 'amenu <silent> &Popup.Type.'.escape(type, '.').
                    \ " :call \<SID>set_pp_type('".type."')\<CR>"
    endfor
endfun

"set_pp_type{{{1
fun! s:set_pp_type(...)
    let type = a:0 > 0 ? a:1 : &ft
    let b:pp_new = []
    if has_key(s:all_pps, type)
        let b:pp_new = deepcopy(s:all_pps[type])
    endif
    if type != '*' && has_key(s:all_pps, "*")
        call s:extend_type(b:pp_new, s:all_pps['*'])
    endif
endfun
fun! g:pp_types(A,L,P)
    return keys(s:all_pps)
endfun

"def Popup: {{{1
let s:def_pp = {}
let s:def_pp["*"] = [
            \ {'cmd'     : "\<c-x>\<c-f>",
            \  'pattern' : ['/\f\{1,}'],
            \ },
            \ {'cmd'     : "\<c-n>",
            \  'pattern' : ['\k\@<!\k\{3,20}'],
            \ },
            \]
let s:def_pp["c"] = [
            \ {'cmd'     : "\<c-x>\<c-o>",
            \  'pattern' : ['\k\.','\k->'],
            \ },
            \]
let s:def_pp["c.gtk"] = [
            \ {'cmd'     : "\<c-x>\<c-o>",
            \  'pattern' : ['\k\.\k{0,20}','\k->\k{0,20}',
            \               'gtk_\k\{2,20}','GTK_\k\{1,20}','Gtk\k\{1,20}',
            \               'gdk_\k\{2,20}','GDK_\k\{1,20}','Gdk\k\{1,20}',
            \               'g_\k\{2,20}', 'G_\k\{1,20}'],
            \ },
            \]
let s:def_pp["tex"] = [
            \ {'cmd'     : "\<c-n>",
            \  'pattern' : ['\\\k\{3,20}','\([\|{\)\k\{3,20}'],
            \ },
            \]
let s:def_pp["html"] = [
            \ {'cmd'     : "\<c-x>\<c-o>",
            \  'pattern' : ['&','<','</',
            \               '<.*\s\+\k\{3,20}','<.*\k\+\s*="\k\{3,20}'],
            \ },
            \]
let s:def_pp["css"] = [
            \ {'cmd'     : "\<c-n>",
            \  'pattern' : ['\k\@<!\k\{3,20}'],
            \  'exclude' : ['^\s.*'],
            \ },
            \ {'cmd'     : "\<c-x>\<c-o>",
            \  'pattern' : ['^\s.*\(\k\|-\)\@<!\(\k\|-\)\{2,20}'],
            \ },
            \]
let s:def_pp["javascript"] = [
            \ {'cmd'     : "\<c-x>\<c-o>",
            \  'pattern' : ['\k\.\k\{0,20}'],
            \ },
            \]
" vim: set ft=vim ff=unix fdm=marker :
