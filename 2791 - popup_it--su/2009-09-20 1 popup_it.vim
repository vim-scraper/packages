"    Author:  Fvw (vimtexhappy@gmail.com)
"             Auto complete popup plugin
"   Version:  v01.04
"   Created:  2008-04-10
"   License:  Copyright (c) 2001-2009, Fvw
"             GNU General Public License version 2 for more details.
"     Usage:  Put this file in your VIM plugins dir
"             Add usr type:
"             let g:usr_popup= {}
"             let g:usr_popup["type"] = [
"                         \ {'cmd'     : "\<c-n>",
"                         \  'pattern' : ['xx', 'yy'],
"                         \  'exclude' : ['zz'],
"                         \ },
"                         \{item2}
"                         \{item3}
"                         \]
"             if already exists deftype, the item than usrtype didn't
"             have would be append.
"             "*" type would be append to every type.
"
"             Use :PopupType type change now popupType
"popup_it.vim: {{{1
if v:version < 700 || exists("loaded_popup_it")
    finish
endif
let loaded_popup_it= 1

let s:keys    = [
            \ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k',
            \ 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
            \ 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G',
            \ 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',
            \ 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '0', '1', '2',
            \ '3', '4', '5', '6', '7', '8', '9', '-', '_', '~', '^',
            \ '.', ',', ':', '!', '#', '=', '%', '$', '@', '<', '>',
            \ '/', '\']

autocmd BufEnter,BufRead * call <SID>popup_run()
autocmd FileType * call <SID>popup_run()
amenu <silent> &Popup.Run :call <SID>popup_run()<CR>
amenu <silent> &Popup.Clr :call <SID>popup_clr()<CR>

command PopupRun call <SID>popup_run()
command PopupClr call <SID>popup_clr()

command -nargs=? -complete=customlist,g:popup_types PopupType call <SID>SetPopupType(<q-args>)

"get_sid {{{1
fun! s:get_sid()
    return matchstr(expand('<sfile>'), '<SNR>\d\+_')
endfun

"popup_clr {{{1
fun! s:popup_clr()
    let s:all_popups  = {}
    let b:pp_now_popup  = []
    let b:pum_tips = ''
    let b:pp_last_fail  = {}
    "idx == 0  -> no fail
    call s:update_last_fail(0)
    for key in s:keys
        if maparg(key, 'i') =~ 'popup_chk'
            exec "silent! iunmap <buffer> ".key
        endif
    endfor
    silent! iunmap <buffer> <c-x><c-o>
    silent! iunmap <buffer> <c-n>
    silent! iunmap <buffer> <c-p>
    silent! iunmap <buffer> <c-b>
    silent! iunmap <buffer> <Plug>popup_fix
    silent! nunmap <buffer> i
    silent! nunmap <buffer> a
    exec "silent! aunmenu &Popup.Type"
endfun

"popup_run {{{1
fun! s:popup_run()
    call s:popup_clr()
    call s:make_popups()
    cal  s:set_popup_type(&ft)

    if has("autocmd") && exists("+omnifunc")
        if &omnifunc == ""
            setlocal omnifunc=syntaxcomplete#Complete
        endif
    endif
    silent! inoremap <silent> <buffer> <expr> <c-x><c-o>
                \ (pumvisible()?'<c-y>':'').
                \ '<c-x><c-o><c-r>=<SID>popup_fix("OmniTips")<cr>'
    silent! inoremap <silent> <buffer> <expr> <c-n>
                \ (pumvisible()?'<c-n>':
                \ '<c-n><c-r>=<SID>popup_fix("CtrlNTips")<cr>')
    silent! inoremap <silent> <buffer> <expr> <c-b>
                \ (pumvisible()?'<c-y>':'').
                \ '<c-n><c-r>=<SID>popup_fix("CtrlNTips")<cr>'

    "use \<c-r> insert fix char
    inoremap <silent> <buffer> <Plug>PopupFix <c-r>=<SID>popup_fix()<cr>

    for key in s:keys
        if maparg(key, 'i') == ""
            exec "silent! inoremap <silent> <buffer> ".key." ".key.
                        \ "\<c-r>=\<SID>popup_chk()\<cr>"
        endif
    endfor
    nnoremap <silent> <buffer> i i<c-r>=<SID>popup_chk()<cr>
    nnoremap <silent> <buffer> a a<c-r>=<SID>popup_chk()<cr>
endfun

"popup_fix {{{1
fun! s:popup_fix(...)
    "Don't use feedkeys , because if the complete
    "very slow the feedkeys would add key after
    "some use input
    if !pumvisible()
        let b:pum_tips = ""
        return "\<c-e>"
    else
        "clean
        call s:update_last_fail(0)
        if a:0 == 1
            let b:pum_tips = a:1
        endif
        "return "\<c-p>\<down>"
        return "\<c-p>"
    endif
endfun


"popup_chk {{{1
fun! s:popup_chk()
    "ignore
    if &paste
        return ""
    end
    "skip some pumvisible tips
    "SnipTips" "SelTips" for snip plugin tab_it.vim
    if pumvisible() && (b:pum_tips == "SnipTips"
                \ ||b:pum_tips == "SelTips"
                \ ||b:pum_tips == "CtrlNTips"
                \ ||b:pum_tips == "CtrlPTips"
                \ ||b:pum_tips == "OmniTips"
                \)
        return ""
    endif

    let lstr = getline('.')[:col('.')-2]
    let i = 0
    for cpl in b:pp_now_popup
        let i += 1
        if s:is_match(lstr, cpl.pattern)  && (!has_key(cpl,'exclude') || !s:is_match(lstr, cpl.exclude))
            if (pumvisible() && b:pum_tips == "AutoTips".i)
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
            let b:pum_tips = "AutoTips".i
            "Set first , Clear in popup_fix if pum ok"
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
fun! s:make_popups()
    let s:all_popups = {}
    if exists("g:usr_popup") && type(g:usr_popup) ==  type({})
        let s:all_popups = deepcopy(g:usr_popup)
        for type in keys(s:def_popup)
            if !has_key(s:all_popups, type)
                let s:all_popups[type] = deepcopy(s:def_popup[type])
            else
                call s:extend_type(s:all_popups[type], s:def_popup[type])
            endif
        endfor
    else
        let s:all_popups = deepcopy(s:def_popup)
    endif

    exec "silent! aunmenu &Popup.Type"
    for type in keys(s:all_popups)
        silent exec 'amenu <silent> &Popup.Type.'.escape(type, '.').
                    \ " :call \<SID>SetPopupType('".type."')\<CR>"
    endfor
endfun

"set_popup_type{{{1
fun! s:set_popup_type(...)
    let type = a:0 > 0 ? a:1 : &ft
    let b:pp_now_popup = []
    if has_key(s:all_popups, type)
        let b:pp_now_popup = deepcopy(s:all_popups[type])
    endif
    if type != '*' && has_key(s:all_popups, "*")
        call s:extend_type(b:pp_now_popup, s:all_popups['*'])
    endif
endfun
fun! g:popup_types(A,L,P)
    return keys(s:all_popups)
endfun

"def Popup: {{{1
let s:def_popup = {}
let s:def_popup["*"] = [
            \ {'cmd'     : "\<c-x>\<c-f>",
            \  'pattern' : ['/\f\{1,}'],
            \ },
            \ {'cmd'     : "\<c-n>",
            \  'pattern' : ['\k\@<!\k\{3,20}'],
            \ },
            \]
let s:def_popup["c"] = [
            \ {'cmd'     : "\<c-x>\<c-o>",
            \  'pattern' : ['\k\.','\k->'],
            \ },
            \]
let s:def_popup["c.gtk"] = [
            \ {'cmd'     : "\<c-x>\<c-o>",
            \  'pattern' : ['\k\.\k{0,20}','\k->\k{0,20}',
            \               'gtk_\k\{2,20}','GTK_\k\{1,20}','Gtk\k\{1,20}',
            \               'gdk_\k\{2,20}','GDK_\k\{1,20}','Gdk\k\{1,20}',
            \               'g_\k\{2,20}', 'G_\k\{1,20}'],
            \ },
            \]
let s:def_popup["tex"] = [
            \ {'cmd'     : "\<c-n>",
            \  'pattern' : ['\\\k\{3,20}','\([\|{\)\k\{3,20}'],
            \ },
            \]
let s:def_popup["html"] = [
            \ {'cmd'     : "\<c-x>\<c-o>",
            \  'pattern' : ['&','<','</',
            \               '<.*\s\+\k\{3,20}','<.*\k\+\s*="\k\{3,20}'],
            \ },
            \]
let s:def_popup["css"] = [
            \ {'cmd'     : "\<c-n>",
            \  'pattern' : ['\k\@<!\k\{3,20}'],
            \  'exclude' : ['^\s.*'],
            \ },
            \ {'cmd'     : "\<c-x>\<c-o>",
            \  'pattern' : ['^\s.*\(\k\|-\)\@<!\(\k\|-\)\{2,20}'],
            \ },
            \]
let s:def_popup["javascript"] = [
            \ {'cmd'     : "\<c-x>\<c-o>",
            \  'pattern' : ['\k\.\k\{0,20}'],
            \ },
            \]
" vim: set ft=vim ff=unix fdm=marker :
