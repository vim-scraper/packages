﻿" ======================================================
"               " VimIM —— Vim 中文輸入法 "
" ------------------------------------------------------
"   VimIM -- Input Method by Vim, of Vim, for Vimmers
" ======================================================

let $VimIM = "easter egg:"" vimimenv<C-6><C-6> vimimrc<C-6><C-6>
let $VimIM = "$Date: 2011-04-20 19:52:59 -0700 (Wed, 20 Apr 2011) $"
let $VimIM = "$Revision: 7565 $"
let s:url  = ["http://vim.sf.net/scripts/script.php?script_id=2506"]
let s:url += ["http://vimim.googlecode.com/svn/vimim/vimim.vim.html"]
let s:url += ["http://code.google.com/p/vimim/source/list"]
let s:url += ["http://code.google.com/p/vimim/issues/list"]
let s:url += ["http://vimim.googlecode.com/svn/trunk/plugin/vimim.cjk.txt"]
let s:url += ["http://vimim-data.googlecode.com"]
let s:url += ["http://groups.google.com/group/vimim"]
let s:url += ["http://vimim.googlecode.com/svn/vimim/vimim.html"]
let s:url += ["vimim+subscribe@googlegroups.com"]

let s:VimIM  = [" ====  introduction     ==== {{{"]
" =================================================
"    File: vimim.vim
"  Author: vimim <vimim@googlegroups.com>
" License: GNU Lesser General Public License
"  Readme: VimIM is a Vim plugin as an independent Input Method.
"
" "VimIM Features"
"  (1) Plug & Play: as an independent input method editor
"  (2) input  of Chinese without mode change
"  (3) search of Chinese without pop-up window
"
" "VimIM Frontend UI"
"  (1) VimIM OneKey: Chinese input without mode change
"  (2) VimIM Chinese Input Mode: ['dynamic','static']
"
" "VimIM Backend Engines"
"  (1) [embedded]   VimIM: http://vimim.googlecode.com
"  (2) [external]   up to 4 clouds and mycloud
"
" "VimIM Installation"
"  (1) drop this vim script to plugin/:    plugin/vimim.vim
"  (2) [option] drop a standard cjk file:  plugin/vimim.cjk.txt
"  (3) [option] drop a standard directory: plugin/vimim/pinyin/
"  (4) [option] drop a English  datafile:  plugin/vimim.txt
"
" "VimIM Usage"
"  (1) play with a game to rotate poem:
"      open vim, type i, type vimimpoem, <C-6>, m, m, m, m
"  (2) play with OneKey, with cjk standard file installed:
"      open vim, type i, type sssss, <C-6>, 1, 2, 3, 4
"  (3) play with cloud, without datafile installed:
"      open vim, type i, type <C-\>, woyouyigemeng

" ============================================= }}}
let s:VimIM += [" ====  initialization   ==== {{{"]
" =================================================
if exists("b:loaded_vimim") || &cp || v:version<700
    finish
endif
scriptencoding utf-8
let b:loaded_vimim = 1
let s:path = expand("<sfile>:p:h")."/"

" -----------------------------------------
function! s:vimim_frontend_initialization()
" -----------------------------------------
    sil!call s:vimim_initialize_shuangpin()
    sil!call s:vimim_initialize_keycode()
    sil!call s:vimim_set_special_im_property()
    sil!call s:vimim_initialize_frontend_punctuation()
    sil!call s:vimim_initialize_skin()
endfunction

" ----------------------------------------
function! s:vimim_backend_initialization()
" ----------------------------------------
    if empty(s:backend_loaded_once)
        let s:backend_loaded_once = 1
    else
        return
    endif
    sil!call s:vimim_super_reset()
    sil!call s:vimim_initialize_encoding()
    sil!call s:vimim_initialize_session()
    sil!call s:vimim_initialize_ui()
    sil!call s:vimim_initialize_i_setting()
    sil!call s:vimim_dictionary_chinese()
    sil!call s:vimim_dictionary_punctuation()
    sil!call s:vimim_dictionary_im_keycode()
    sil!call s:vimim_scan_backend_embedded_datafile()
    sil!call s:vimim_scan_backend_embedded_directory()
    sil!call s:vimim_dictionary_quantifiers()
    sil!call s:vimim_scan_current_buffer()
    sil!call s:vimim_scan_backend_cloud()
    sil!call s:vimim_initialize_cjk_file()
    sil!call s:vimim_scan_english_datafile()
    sil!call s:vimim_initialize_keycode()
endfunction

" ------------------------------------
function! s:vimim_initialize_session()
" ------------------------------------
    let s_vimim_cloud = 0
    let s:clouds = ['sogou','qq','google','baidu']
    for cloud in s:clouds
        let s_vimim_cloud = eval("s:vimim_cloud_" . cloud)
        if !empty(s_vimim_cloud)
            let s:cloud_default = cloud
            break
        endif
    endfor
    if empty(s_vimim_cloud)
        let s:cloud_default = get(s:clouds,0)
    endif
    let s:cloud_sogou_key = 0
    let s:mycloud_plugin = 0
    let s:www_libcall = 0
    " --------------------------------
    let s:www_executable = 0
    let s:seamless_positions = []
    let s:uxxxx = '^u\x\x\x\x\|^\d\d\d\d\d\>'
    let s:smart_single_quotes = 1
    let s:smart_double_quotes = 1
    let s:quanpin_table = {}
    let s:shuangpin_table = {}
    let s:shuangpin_keycode_chinese = {}
    let s:quantifiers = {}
    let s:current_positions = [0,0,1,0]
    let s:start_row_before = 0
    let s:start_column_before = 1
    let s:scriptnames_output = 0
    let a = char2nr('a')
    let z = char2nr('z')
    let A = char2nr('A')
    let Z = char2nr('Z')
    let Az_nr_list = extend(range(A,Z), range(a,z))
    let s:Az_list = map(Az_nr_list, "nr2char(".'v:val'.")")
    let s:az_list = map(range(a,z), "nr2char(".'v:val'.")")
    let s:AZ_list = map(range(A,Z), "nr2char(".'v:val'.")")
    let s:valid_key = 0
    let s:valid_keys = s:az_list
    let s:abcd = "'abcdvfgz"
    let s:qwerty = split('pqwertyuio','\zs')
    let s:chinese_punctuation = s:vimim_chinese_punctuation % 2
    let s:chinese_im_switch = 0
    let s:chinese_mode_switch = 0
endfunction

" -------------------------------
function! s:vimim_initialize_ui()
" -------------------------------
    let s:ui = {}
    let s:ui.im = ''
    let s:ui.root = ''
    let s:ui.keycode = ''
    let s:ui.statusline = ''
    let s:ui.has_dot = 0
    let s:ui.frontends = []
    let s:backend = {}
    let s:backend.directory = {}
    let s:backend.datafile  = {}
    let s:backend.cloud     = {}
endfunction

" ---------------------------------
function! s:vimim_one_backend_hash()
" ----------------------------------
    let one_backend_hash = {}
    let one_backend_hash.root = ''
    let one_backend_hash.im = ''
    let one_backend_hash.name = ''
    let one_backend_hash.chinese = ''
    let one_backend_hash.directory = ''
    let one_backend_hash.lines = []
    let one_backend_hash.cache = {}
    let one_backend_hash.keycode = "[0-9a-z'.]"
    return one_backend_hash
endfunction

" --------------------------------
function! s:vimim_chinese(english)
" --------------------------------
    let key = a:english
    let chinese = key
    if has_key(s:chinese, key)
        let chinese = get(s:chinese[key], 0)
        if s:encoding !~ "chinese"
        \&& len(s:chinese[key]) > 1
        \&& s:vimim_debug > 1
            let chinese = get(s:chinese[key], 1)
        endif
    endif
    return chinese
endfunction

" ---------------------------------------
function! s:vimim_dictionary_im_keycode()
" ---------------------------------------
    let s:im_keycode = {}
    let s:im_keycode['pinyin']   = "[.'0-9a-z]"
    let s:im_keycode['hangul']   = "[.'0-9a-z]"
    let s:im_keycode['xinhua']   = "[.'0-9a-z]"
    let s:im_keycode['quick']    = "[.'0-9a-z]"
    let s:im_keycode['wubi']     = "[.'0-9a-z]"
    let s:im_keycode['sogou']    = "[.'0-9a-z]"
    let s:im_keycode['qq']       = "[.'0-9a-z]"
    let s:im_keycode['google']   = "[.'0-9a-z]"
    let s:im_keycode['baidu']    = "[.'0-9a-z]"
    let s:im_keycode['mycloud']  = "[.'0-9a-z]"
    let s:im_keycode['yong']     = "[.'a-z;/]"
    let s:im_keycode['wu']       = "[.'a-z]"
    let s:im_keycode['nature']   = "[.'a-z]"
    let s:im_keycode['zhengma']  = "[.'a-z]"
    let s:im_keycode['cangjie']  = "[.'a-z]"
    let s:im_keycode['taijima']  = "[.'a-z]"
    let s:im_keycode['erbi']     = "[.'a-z,;/]"
    let s:im_keycode['array30']  = "[.,0-9a-z;/]"
    let s:im_keycode['phonetic'] = "[.,0-9a-z;/]"
    let s:im_keycode['boshiamy'] = "[][a-z'.,]"
    " -------------------------------------------
    let vimimkeys = copy(keys(s:im_keycode))
    call add(vimimkeys, 'pinyin_sogou')
    call add(vimimkeys, 'pinyin_quote_sogou')
    call add(vimimkeys, 'pinyin_huge')
    call add(vimimkeys, 'pinyin_fcitx')
    call add(vimimkeys, 'pinyin_canton')
    call add(vimimkeys, 'pinyin_hongkong')
    call add(vimimkeys, 'wubijd')
    call add(vimimkeys, 'wubi98')
    call add(vimimkeys, 'wubi2000')
    let s:all_vimim_input_methods = copy(vimimkeys)
endfunction

" ------------------------------------
function! s:vimim_initialize_keycode()
" ------------------------------------
    let keycode = "[.'0-9a-z]"
    if empty(s:vimim_shuangpin)
        let keycode = s:backend[s:ui.root][s:ui.im].keycode
    else
        let keycode = s:shuangpin_keycode_chinese.keycode
    endif
    let s:valid_key = copy(keycode)
    let character_string = ""
    let i = 0
    while i < 16*16
        let char = nr2char(i)
        if char =~# keycode
            let character_string .= char
        endif
        let i += 1
    endwhile
    let s:valid_keys = split(character_string, '\zs')
endfunction

" ============================================= }}}
let s:VimIM += [" ====  customization    ==== {{{"]
" =================================================

" -----------------------------------
function! s:vimim_initialize_global()
" -----------------------------------
    let G = []
    call add(G, "g:vimim_debug")
    call add(G, "g:vimim_ctrl_space_to_toggle")
    call add(G, "g:vimim_ctrl_h_to_switch")
    call add(G, "g:vimim_data_file")
    call add(G, "g:vimim_data_file_encoding")
    call add(G, "g:vimim_data_directory")
    call add(G, "g:vimim_hjkl_directory")
    call add(G, "g:vimim_chinese_input_mode")
    call add(G, "g:vimim_backslash_close_pinyin")
    call add(G, "g:vimim_imode_pinyin")
    call add(G, "g:vimim_shuangpin")
    call add(G, "g:vimim_cursor_color")
    call add(G, "g:vimim_latex_suite")
    call add(G, "g:vimim_use_cache")
    call add(G, "g:vimim_custom_menu")
    call add(G, "g:vimim_more_candidates")
    call add(G, "g:vimim_digit_4corner")
    call add(G, "g:vimim_onekey_is_tab")
    call add(G, "g:vimim_cloud_mycloud")
    call add(G, "g:vimim_cloud_sogou")
    call add(G, "g:vimim_cloud_google")
    call add(G, "g:vimim_cloud_qq")
    " -----------------------------------
    let s:vimimrc = []
    call s:vimim_set_global_default(G, 0)
    " -----------------------------------
    let G = []
    call add(G, "g:vimim_onekey_hit_and_run")
    call add(G, "g:vimim_loop_pageup_pagedown")
    call add(G, "g:vimim_chinese_punctuation")
    call add(G, "g:vimim_search_next")
    call add(G, "g:vimim_custom_label")
    call add(G, "g:vimim_custom_color")
    " -----------------------------------
    call s:vimim_set_global_default(G, 1)
    " -----------------------------------
    let g:vimim = []
    let s:backend_loaded_once = 0
    let s:chinese_input_mode = "onekey"
    if empty(s:vimim_chinese_input_mode)
        let s:vimim_chinese_input_mode = 'dynamic'
    endif
endfunction

" ----------------------------------------------------
function! s:vimim_set_global_default(options, default)
" ----------------------------------------------------
    for variable in a:options
        let option = ':let ' . variable .' = '.string(eval(variable)).' '
        let s_variable = substitute(variable,"g:","s:",'')
        if exists(variable)
            call add(s:vimimrc, '  ' . option)
            exe 'let '. s_variable .'='. variable
            exe 'unlet! ' . variable
        else
            call add(s:vimimrc, '" ' . option)
            exe 'let '. s_variable . '=' . a:default
        endif
    endfor
endfunction

" ----------------------------
function! s:debugs(key, value)
" ----------------------------
    if s:vimim_debug > 0
        let item  = a:key
        let item .= ' = '
        let item .= a:value
        call add(g:vimim, item)
    endif
endfunction

" ----------------------------------
function! s:vimim_initialize_debug()
" ----------------------------------
    if isdirectory('/home/xma')
        let g:vimim_debug = 2
        let g:vimim_digit_4corner = 1
        let g:vimim_onekey_is_tab = 2
        let g:vimim_onekey_hit_and_run = 0
        let g:vimim_hjkl_directory = '/home/xma/hjkl/'
        let g:vimim_data_directory = '/home/vimim/pinyin/'
    endif
endfunction

" ============================================= }}}
let s:VimIM += [" ====  for Mom and Dad  ==== {{{"]
" =================================================

" ---------------------------------
function! s:vimim_for_mom_and_dad()
" ---------------------------------
    let onekey = ""
    let buffer = expand("%:p:t")
    if buffer =~ 'vimim.mom.txt'
        startinsert!
        let s:vimim_digit_4corner = 0
        let onekey = s:vimim_onekey_action(0)
        sil!call s:vimim_onekey_start()
    elseif buffer =~ 'vimim.dad.txt'
        set noruler
        let s:vimim_digit_4corner = 1
    else
        return
    endif
    let s:vimim_onekey_is_tab = 1
    if has("gui_running")
        autocmd! * <buffer>
        autocmd  FocusLost <buffer> sil!wall
        noremap  <silent>  <Esc> :sil!%y +<CR>
        set tw=30
        set lines=24
        set columns=36
        set report=12345
        let &gfn .= ":h24:w12"
    endif
    sil!exe 'sil!return "' . onekey . '"'
endfunction

" ---------------------------------
function! s:vimim_initialize_skin()
" ---------------------------------
    if s:vimim_custom_color < 1
        return
    endif
    let guibg = 'green'
    if len(s:vimim_cursor_color) > 1
        let guibg = s:vimim_cursor_color
    endif
    sil!exe 'highlight! vimim_cursor_color guifg=NONE guibg=' . guibg
    if s:vimim_custom_color == 1
        highlight! link PmenuSel Title
    elseif s:vimim_custom_color == 2
        highlight! PmenuSel NONE
    endif
    highlight! PmenuSbar  NONE
    highlight! PmenuThumb NONE
    highlight! Pmenu      NONE
endfunction

" ---------------------------------------------------------
function! s:vimim_set_keyboard_list(column_start, keyboard)
" ---------------------------------------------------------
    let s:start_column_before = a:column_start
    if len(s:keyboard_list) < 2
        let s:keyboard_list = [a:keyboard]
    endif
endfunction

" ============================================= }}}
let s:VimIM += [" ====  easter eggs      ==== {{{"]
" =================================================

" ----------------------------------------
function! s:vimim_easter_chicken(keyboard)
" ----------------------------------------
    if a:keyboard ==# "vim" || a:keyboard =~# "^vimim"
        try
            return eval("s:vimim_egg_" . a:keyboard . "()")
        catch
            call s:debugs('egg::', v:exception)
        endtry
    endif
    return []
endfunction

" -----------------------------
function! s:vimim_egg_vimimrc()
" -----------------------------
    " http://vimim.googlecode.com/svn/vimim/vimim.html#vimimrc
    return sort(copy(s:vimimrc))
endfunction

" ------------------------------
function! s:vimim_egg_vimimvim()
" ------------------------------
    let eggs = copy(s:VimIM)
    let filter = "strpart(" . 'v:val' . ", 0, 29)"
    return map(eggs, filter)
endfunction

" -------------------------------
function! s:vimim_egg_vimimpoem()
" -------------------------------
    let eggs  = ["白日依山尽"]
    let eggs += ["黄河入海流"]
    let eggs += ["欲穷千里目"]
    let eggs += ["更上一层楼"]
    return eggs
endfunction

" -------------------------
function! s:vimim_egg_vim()
" -------------------------
    let eggs  = ["vi    文本編輯器"]
    let eggs += ["vim   最牛文本編輯器"]
    let eggs += ["vim   精力"]
    let eggs += ["vim   生氣"]
    let eggs += ["vimim 中文輸入法"]
    return eggs
endfunction

" -------------------------------
function! s:vimim_egg_vimimhelp()
" -------------------------------
    let eggs = []
    call add(eggs, "官方网址 " . s:url[0] . " ")
    call add(eggs, "最新程式 " . s:url[1] . " ")
    call add(eggs, "更新报告 " . s:url[2] . " ")
    call add(eggs, "错误报告 " . s:url[3] . " ")
    call add(eggs, "标准字库 " . s:url[4] . " ")
    call add(eggs, "民间词库 " . s:url[5] . " ")
    call add(eggs, "新闻论坛 " . s:url[6] . " ")
    call add(eggs, "最新主页 " . s:url[7] . " ")
    call add(eggs, "论坛邮箱 " . s:url[8] . " ")

    return eggs
endfunction

" ------------------------------
function! s:vimim_egg_vimimenv()
" ------------------------------
    let eggs = []
    let option = "os"
    if has("win32unix")
        let option = "cygwin"
    elseif has("win32")
        let option = "Windows32"
    elseif has("win64")
        let option = "Windows64"
    elseif has("unix")
        let option = "unix"
    elseif has("macunix")
        let option = "macunix"
    endif
    let option .= "_" . &term
    let computer = s:vimim_chinese('computer') . s:colon
    call add(eggs, computer . option)
    let revision = s:vimim_chinese('revision') . s:colon
    let option = get(split($VimIM),1)
    let option = empty(option) ? "" : "vimim.vim=" . option
    let vim = v:progname . "=" . v:version . s:space
    call add(eggs, revision . vim . option)
    let encoding = s:vimim_chinese('encoding') . s:colon
    call add(eggs, encoding . &encoding . s:space . &fileencodings)
    if has("gui_running")
        let font = empty(&guifontwide) ? &guifont : &guifontwide
        let option = s:vimim_chinese('font') . s:colon . font
        call add(eggs, option)
    endif
    let option = s:vimim_chinese('environment') . s:colon . v:lc_time
    call add(eggs, option)
    let im = s:vimim_statusline()
    let toggle = "i_CTRL-Bslash"
    let buffer = expand("%:p:t")
    if buffer =~# '.vimim\>'
        let toggle = s:vimim_chinese('auto') . s:space . buffer
    elseif s:vimim_ctrl_space_to_toggle == 1
        let toggle = "toggle_with_CTRL-Space"
    elseif s:vimim_onekey_is_tab == 2
        let toggle = "Tab_as_OneKey_NonStop"
        let im = s:vimim_chinese('onekey') . s:space
        let im .= s:ui.statusline . s:space . "VimIM"
    endif
    let option = s:vimim_chinese('style') . s:colon . toggle
    call add(eggs, option)
    let database = s:vimim_chinese('database') . s:colon
    if s:has_cjk_file > 0
        let ciku  = database . s:vimim_chinese('standard')
        let ciku .= s:vimim_chinese('cjk') . s:colon
        call add(eggs, ciku . s:cjk_file)
    endif
    if s:has_english_file > 0
        let ciku = database . s:vimim_chinese('english') . database
        call add(eggs, ciku . s:english_file)
    endif
    if len(s:ui.frontends) > 0
        for frontend in s:ui.frontends
            let ui_root = get(frontend, 0)
            let ui_im = get(frontend, 1)
            let ciku = database . s:vimim_chinese(ui_root) . database
            let datafile = s:backend[ui_root][ui_im].name
            call add(eggs, ciku . datafile)
        endfor
    endif
    if len(im) > 0 && len(s:ui.frontends) < 2
        let option = s:vimim_chinese('input') . s:colon . im
        call add(eggs, option)
    endif
    if eval("s:vimim_cloud_" . s:cloud_default) == 888
        let cloud = s:vimim_chinese(s:cloud_default)
        let option = cloud . s:colon . s:vimim_chinese('cloudatwill')
        call add(eggs, option)
    endif
    call map(eggs, 'v:val . " "')
    return eggs
endfunction

" ============================================= }}}
let s:VimIM += [" ====  games            ==== {{{"]
" =================================================
" http://vimim.googlecode.com/svn/vimim/vimim.html#game

" ---------------------------------
function! s:vimim_gnuplot(keyboard)
" ---------------------------------
    let results = []
    let dumb = "set terminal dumb;"
    let gnuplot = "gnuplot -e '" . dumb . a:keyboard . "'"
    try
        let results = split(system(gnuplot),'\n')
    catch
        let results = []
        call s:debugs('gnuplot::', v:exception)
    endtry
    if !empty(results)
        let s:show_me_not = 1
        let results = results[1 : len(results)-1]
    endif
    return results
endfunction

" ----------------------------------
function! s:vimim_get_hjkl(keyboard)
" ----------------------------------
    let keyboard = a:keyboard
    " [unicode] support direct unicode/gb/big5 input
    let lines = s:vimim_get_unicode_list(keyboard)
    if !empty(lines)
        return lines
    endif
    " [eggs] hunt classic easter egg ... vim<C-6>
    let lines = s:vimim_easter_chicken(keyboard)
    if !empty(lines)
        " [hjkl] display the buffer inside the omni window
    elseif keyboard ==# "vimim"
        let unnamed_register = getreg('"')
        let lines = split(unnamed_register,'\n')
        if len(lines) < 2
            let lines = s:vimim_egg_vimimenv()
        else
            if unnamed_register =~ '\d' && join(lines) !~ '[^0-9[:blank:].]'
                let sum = eval(join(lines,'+'))
                let len = len(lines)
                let ave = 1.0*sum/len
                let math  = 'sum='
                let math .= string(len)
                let math .= '*'
                let math .= printf('%.2f',ave)
                let math .= '='
                if unnamed_register =~ '[.]'
                    let math .= printf('%.2f',1.0*sum)
                else
                    let math .= string(sum)
                endif
                let lines = [math]
            endif
        endif
    else
        " [poem] check entry in special directories first
        let dirs = [s:path, s:vimim_hjkl_directory]
        for dir in dirs
            let lines = s:vimim_get_from_directory(keyboard, dir)
            if empty(lines)
                continue
            else
                break
            endif
        endfor
    endif
    if !empty(lines)
        let s:show_me_not = 1
    endif
    return lines
endfunction

" -------------------------------------------
function! s:vimim_hjkl_rotation(matched_list)
" -------------------------------------------
    let lines = a:matched_list
    if empty(lines)
        return []
    endif
    let max = max(map(copy(lines), 'strlen(v:val)')) + 1
    let multibyte = 1
    if match(lines,'\w') < 0
        " rotation makes more sense for cjk
        let multibyte = s:multibyte
    endif
    let results = []
    for line in lines
        let spaces = ''
        let gap = (max-len(line))/multibyte
        if gap > 0
            for i in range(gap)
                let spaces .= s:space
            endfor
        endif
        let line .= spaces
        call add(results, line)
    endfor
    let rotations = []
    for i in range(max/multibyte)
        let column = ''
        for line in reverse(copy(results))
            let lines = split(line,'\zs')
            let line = get(lines, i)
            if empty(line)
                continue
            else
                let column .= line
            endif
        endfor
        call add(rotations, column)
    endfor
    return rotations
endfunction

" ----------------------------------------------
function! s:vimim_chinese_rotation() range abort
" ----------------------------------------------
" [usage] :VimiM
    sil!call s:vimim_backend_initialization()
    :%s#\s*\r\=$##
    let lines = getline(a:firstline, a:lastline)
    let lines = s:vimim_hjkl_rotation(lines)
    if !empty(lines)
        :%d
        for line in lines
            put=line
        endfor
    endif
endfunction

" ============================================= }}}
let s:VimIM += [" ====  /search          ==== {{{"]
" =================================================

" -----------------------------
function! g:vimim_search_next()
" -----------------------------
    let english = @/
    if english =~ '\<' && english =~ '\>'
        let english = substitute(english,'[<>\\]','','g')
    endif
    if len(english) > 1
    \&& len(english) < 24
    \&& english =~ '\w'
    \&& english !~ '\W'
    \&& english !~ '_'
    \&& v:errmsg =~# english
    \&& v:errmsg =~# '^E486: '
        let error = ""
        try
            sil!call s:vimim_search_chinese_by_english(english)
        catch
            let error = v:exception
        endtry
        echon "/" . english . error
    endif
    let v:errmsg = ""
endfunction

" ---------------------------------------------------
function! s:vimim_search_chinese_by_english(keyboard)
" ---------------------------------------------------
    sil!call s:vimim_backend_initialization()
    let keyboard = tolower(a:keyboard)
    let cjk_results = []
    let s:english_results = []
    " => slash search unicode /u808f
    let ddddd = s:vimim_get_unicode_ddddd(keyboard)
    " => slash search cjk /m7712x3610j3111 /muuqwxeyqpjeqqq
    if empty(ddddd)
        let keyboards = s:vimim_slash_search_block(keyboard)
        if len(keyboards) > 0
            for keyboard in keyboards
                let chars = s:vimim_cjk_match(keyboard)
                if len(keyboards) == 1
                    let cjk_results = copy(chars)
                elseif len(chars) > 0
                    let collection = "[" . join(chars,'') . "]"
                    call add(cjk_results, collection)
                endif
            endfor
            if len(keyboards) > 1
                let cjk_results = [join(cjk_results,'')]
            endif
        endif
    else
        let cjk_results = [nr2char(ddddd)]
    endif
    " => slash search english /horse
    if keyboard =~ '^\l\+'
        sil!call s:vimim_onekey_english(a:keyboard, 1)
    endif
    let results = []
    if empty(cjk_results) && empty(s:english_results)
        if !empty(s:vimim_shuangpin)
            sil!call s:vimim_initialize_shuangpin()
            let keyboard = s:vimim_shuangpin_transform(keyboard)
        endif
        if s:vimim_cloud_sogou == 1
            " => slash search from sogou cloud
            let results = s:vimim_get_cloud_sogou(keyboard)
        elseif !empty(s:mycloud_plugin)
            " => slash search from mycloud
            let results = s:vimim_get_mycloud_plugin(keyboard)
        endif
    endif
    if empty(results)
        " => slash search from local datafile or directory
        let results = s:vimim_embedded_backend_engine(keyboard,1)
    endif
    call extend(results, s:english_results, 0)
    call extend(results, cjk_results)
    if !empty(results)
        let results = split(substitute(join(results),'\w','','g'))
        let slash = join(results, '\|')
        if empty(search(slash,'nw'))
            let @/ = a:keyboard
        else
            let @/ = slash
        endif
        echon "/" . a:keyboard
    endif
endfunction

" --------------------------------------------
function! s:vimim_slash_search_block(keyboard)
" --------------------------------------------
" /muuqwxeyqpjeqqq  =>  shortcut   /search
" /m7712x3610j3111  =>  standard   /search
" /ma77xia36ji31    =>  free-style /search
" --------------------------------------------
    if empty(s:has_cjk_file)
        return []
    endif
    let results = []
    let keyboard = a:keyboard
    while len(keyboard) > 1
        let keyboard2 = s:vimim_cjk_sentence_match(keyboard)
        if empty(keyboard2)
            break
        else
            call add(results, keyboard2)
            let keyboard = strpart(keyboard,len(keyboard2))
        endif
    endwhile
    return results
endfunction

" ============================================= }}}
let s:VimIM += [" ====  Chinese Mode     ==== {{{"]
" =================================================
" s:chinese_input_mode='onekey'  => (default) OneKey
" s:chinese_input_mode='dynamic' => (default) dynamic mode
" s:chinese_input_mode='static'  =>  static chinese input mode

" --------------------------
function! <SID>VimIMSwitch()
" --------------------------
    let s:chinese_mode_switch = 0
    if !exists('s:chinese_im_switch') || empty(s:chinese_im_switch)
        let s:chinese_im_switch = 1
    endif
    sil!call <SID>ChineseMode()
    let s:chinese_im_switch += 1
    return ""
endfunction

" --------------------------
function! <SID>ChineseMode()
" --------------------------
    sil!call s:vimim_backend_initialization()
    let s:chinese_input_mode = s:vimim_chinese_input_mode
    if empty(s:ui.frontends)
        return
    endif
    let action = ""
    let s:chinese_mode_switch += 1
    if empty(s:chinese_mode_switch % 2)
        sil!call g:vimim_stop()
        if mode() == 'n'
            :redraw!
        endif
    else
        let switch = s:chinese_im_switch % len(s:ui.frontends)
        let frontends = get(s:ui.frontends, switch)
        let s:ui.root = get(frontends,0)
        let s:ui.im = get(frontends,1)
        call s:vimim_set_statusline()
        call s:vimim_build_datafile_cache()
        let action = s:vimim_chinesemode_action()
    endif
    sil!exe 'sil!return "' . action . '"'
endfunction

" ---------------------------------------
function! <SID>vimim_punctuation_toggle()
" ---------------------------------------
    let s:chinese_punctuation = (s:chinese_punctuation+1)%2
    call s:vimim_set_statusline()
    call s:vimim_punctuation_mapping()
    return  ""
endfunction

" ------------------------------------
function! s:vimim_chinesemode_action()
" ------------------------------------
    silent!call s:vimim_start()
    sil!call s:vimim_frontend_initialization()
    if s:vimim_chinese_punctuation > -1
        inoremap <expr> <C-^> <SID>vimim_punctuation_toggle()
        call s:vimim_punctuation_mapping()
    endif
    let action = ""
    if s:chinese_input_mode =~ 'dynamic'
        let s:seamless_positions = getpos(".")
        if s:ui.im =~ 'wubi' || s:ui.im =~ 'erbi'
            " dynamic auto trigger for wubi
            for char in s:az_list
                sil!exe 'inoremap <silent> ' . char .
                \ ' <C-R>=g:vimim_wubi_ctrl_e_ctrl_y()<CR>'
                \ . char . '<C-R>=g:vimim()<CR>'
            endfor
        else
            " dynamic alphabet trigger for all
            let not_used_valid_keys = "[0-9.']"
            if s:ui.has_dot == 1
                let not_used_valid_keys = "[0-9]"
            endif
            for char in s:valid_keys
                if char !~# not_used_valid_keys
                    sil!exe 'inoremap <silent> ' . char .
                    \ ' <C-R>=pumvisible() ? "<C-E>" : ""<CR>'
                    \ . char . '<C-R>=g:vimim()<CR>'
                endif
            endfor
        endif
    elseif s:chinese_input_mode =~ 'static'
        for char in s:Az_list
            sil!exe 'inoremap <silent> ' . char .
            \ ' <C-R>=pumvisible() ? "<C-Y>" : ""<CR>'
            \ . char . '<C-R>=g:vimim_reset_after_insert()<CR>'
        endfor
        if pumvisible()
            " <C-\> does nothing on popup menu
        else
            let action = s:vimim_static_action("")
        endif
    endif
    sil!exe 'sil!return "' . action . '"'
endfunction

" ------------------------------------
function! s:vimim_static_action(space)
" ------------------------------------
    let space = a:space
    let one_before = getline(".")[col(".")-2]
    if one_before =~# s:valid_key
        let space = g:vimim()
    endif
    sil!exe 'sil!return "' . space . '"'
endfunction

" -----------------------------------------------
function! s:vimim_get_seamless(current_positions)
" -----------------------------------------------
    if empty(s:seamless_positions)
    \|| empty(a:current_positions)
        return -1
    endif
    let seamless_bufnum = s:seamless_positions[0]
    let seamless_lnum = s:seamless_positions[1]
    let seamless_off = s:seamless_positions[3]
    if seamless_bufnum != a:current_positions[0]
    \|| seamless_lnum != a:current_positions[1]
    \|| seamless_off != a:current_positions[3]
        let s:seamless_positions = []
        return -1
    endif
    let seamless_column = s:seamless_positions[2]-1
    let start_column = a:current_positions[2]-1
    let len = start_column - seamless_column
    let start_row = a:current_positions[1]
    let current_line = getline(start_row)
    let snip = strpart(current_line, seamless_column, len)
    if empty(len(snip))
        return -1
    endif
    for char in split(snip, '\zs')
        if char !~# s:valid_key
            return -1
        endif
    endfor
    let s:start_row_before = seamless_lnum
    return seamless_column
endfunction

" ============================================= }}}
let s:VimIM += [" ====  OneKey           ==== {{{"]
" =================================================

" ------------------------------
function! s:vimim_onekey_start()
" ------------------------------
    let s:chinese_input_mode = "onekey"
    sil!call s:vimim_backend_initialization()
    sil!call s:vimim_frontend_initialization()
    sil!call s:vimim_onekey_pumvisible_mapping()
    sil!call s:vimim_onekey_punctuation_mapping()
    sil!call s:vimim_start()
endfunction

" ---------------------
function! <SID>OneKey()
" ---------------------
" (1) <OneKey> => start OneKey as "hit and run"
" (2) <OneKey> => stop  OneKey and print out menu
" -----------------------------------------------
    let onekey = ''
    let one_before = getline(".")[col(".")-2]
    if empty(one_before) || one_before =~ '\s'
        if s:vimim_onekey_is_tab > 0
            let onekey = "\t"
        endif
    else
        if pumvisible() && len(s:popupmenu_list) > 0
            let onekey = '\<C-E>\<C-R>=g:vimim_onekey_dump()\<CR>'
        else
            sil!call s:vimim_onekey_start()
            let onekey = s:vimim_onekey_action(0)
        endif
    endif
    sil!exe 'sil!return "' . onekey . '"'
endfunction

" -----------------------------
function! g:vimim_onekey_dump()
" -----------------------------
    let lines = []
    for items in s:popupmenu_list
        let line = printf('%s', items.word)
        if has_key(items, "abbr")
            let line = printf('%s', items.abbr)
            if has_key(items, "menu")
                let line = printf('%s %s', items.abbr, items.menu)
            endif
        endif
        if get(s:keyboard_list,0) ==# 'vimim'
            let line = repeat(" ", virtcol("'<'")-2) . line
        endif
        call add(lines, line)
    endfor
    if has("gui_running") && has("win32")
        let @+ = join(lines, "\n")
    endif
    if getline(".") =~ 'vimim\>' && len(lines) < 2
        call setline(line("."), lines)
    else
        let saved_position = getpos(".")
        for line in lines
            put=line
        endfor
        call setpos(".", saved_position)
    endif
    sil!call g:vimim_stop()
    sil!exe "sil!return '\<Esc>'"
endfunction

" -------------------------------------
function! s:vimim_onekey_action(onekey)
" -------------------------------------
    let current_line = getline(".")
    let gnuplot = '^\s*' . 'plot' . '\s\+'
    let gnuplot = match(current_line, gnuplot)
    if empty(gnuplot) && executable('gnuplot')
        " [gnuplot] usage:  plot sin(x)/x
        let s:has_gnuplot = 1
        sil!exe 'sil!return "' . g:vimim() . '"'
    endif
    let one_before = current_line[col(".")-2]
    let two_before = current_line[col(".")-3]
    let onekey = ""
    if empty(s:ui.has_dot) && two_before !~# "[0-9a-z]"
        let punctuations = copy(s:punctuations)
        call extend(punctuations, s:evils)
        if has_key(punctuations, one_before)
            for char in keys(punctuations)
                " no transfer for punctuation after punctuation
                if two_before ==# char || two_before =~ '\u'
                    return " "
                endif
            endfor
            " transfer English punctuation to Chinese punctuation
            let replacement = punctuations[one_before]
            if one_before == "'"
                let replacement = <SID>vimim_get_quote(1)
            elseif one_before == '"'
                let replacement = <SID>vimim_get_quote(2)
            endif
            let onekey = "\<BS>" . replacement
            sil!exe 'sil!return "' . onekey . '"'
        endif
    endif
    if one_before =~ s:valid_key
        let onekey = g:vimim()
    elseif a:onekey < 1
        let onekey = s:vimim_get_unicode_menu()
    endif
    sil!exe 'sil!return "' . onekey . '"'
endfunction

" --------------------------
function! <SID>vimim_space()
" --------------------------
" (1) <Space> after English (valid keys) => trigger keycode menu
" (2) <Space> after English punctuation  => Chinese punctuation
" (3) <Space> after popup menu           => insert Chinese
" (4) <Space> after Chinese              => stop OneKeyNonStop
" ---------------------------------------------------------------
    let space = " "
    if pumvisible()
        let space = '\<C-Y>\<C-R>=g:vimim()\<CR>'
        call g:vimim_reset_after_insert()
    elseif s:chinese_input_mode =~ 'static'
        let space = s:vimim_static_action(space)
    elseif s:chinese_input_mode =~ 'onekey'
        let before = getline(".")[col(".")-2]
        if before !~ s:valid_key && !has_key(s:punctuations, before)
            let space = ""
            call g:vimim_stop()
        else
            let space = s:vimim_onekey_action(1)
        endif
    endif
    sil!exe 'sil!return "' . space . '"'
endfunction

" ------------------------------
function! g:vimim_menu_to_clip()
" ------------------------------
    let chinese = s:vimim_popup_word()
    if !empty(chinese)
        if has("gui_running") && has("win32")
            let @+ = chinese
        endif
    endif
    call g:vimim_stop()
    sil!exe "sil!return '\<Esc>'"
endfunction

" ----------------------------
function! s:vimim_popup_word()
" ----------------------------
    if pumvisible()
        return ""
    endif
    let column_start = s:start_column_before
    let column_end = col('.') - 1
    let range = column_end - column_start
    let current_line = getline(".")
    let chinese = strpart(current_line, column_start, range)
    return substitute(chinese,'\w','','g')
endfunction

" --------------------------------------
function! s:vimim_onekey_input(keyboard)
" --------------------------------------
    " [game] turn menu 90 degree for each hjkl_m
    let keyboard = a:keyboard
    let lines = s:vimim_get_hjkl(keyboard)
    if !empty(lines)
        if s:hjkl_m % 4 > 0
            let &pumheight = 0
            for i in range(s:hjkl_m%4)
                let lines = s:vimim_hjkl_rotation(lines)
            endfor
        endif
        return lines
    endif
    " [dot_by_dot] i.have.a.dream
    let keyboard = s:vimim_dot_by_dot(keyboard)
    " [english] english cannot be ignored
    if keyboard =~ '^\l\+'
        sil!call s:vimim_onekey_english(keyboard, 0)
    endif
    let results = []
    " [imode] magic i: (1) English number (2) qwerty shortcut
    if keyboard =~# '^i'
        if keyboard =~ '\d'
            let results = s:vimim_imode_number(keyboard, 'i')
            if !empty(len(results))
                return results
            endif
        elseif s:vimim_imode_pinyin > 0
            let dddd = s:vimim_qwertyuiop_1234567890(keyboard[1:])
            if !empty(dddd)
                let keyboard = dddd " iypwqwuww => 60212722
            endif
        endif
    endif
    " [cjk] cjk database works like swiss-army knife
    if s:has_cjk_file > 0
        let keyboard = s:vimim_cjk_sentence_match(keyboard)
        let results = s:vimim_cjk_match(keyboard)
        if keyboard =~ '^\l\d\d\d\d'
        \&& len(results) > 0
        \&& len(s:english_results) > 0
            call extend(s:english_results, results)
        endif
    endif
    return results
endfunction

" ------------------------------------
function! s:vimim_dot_by_dot(keyboard)
" ------------------------------------
    let keyboard = a:keyboard
    let partition = match(keyboard, "[.']")
    if partition > -1 && empty(s:ui.has_dot)
        let keyboard = s:vimim_get_head(keyboard, partition)
    endif
    return keyboard
endfunction

" ============================================= }}}
let s:VimIM += [" ====  hjkl             ==== {{{"]
" =================================================

" -----------------------
function! s:vimim_cache()
" -----------------------
    if s:has_gnuplot > 0 && len(s:matched_list) > 0
        return s:matched_list
    endif
    let results = []
    if s:chinese_input_mode =~ 'onekey'
        if len(s:hjkl_x) > 0
            if len(s:matched_list) > 0 && s:show_me_not > 0
                let results = s:vimim_onekey_menu_format()
            elseif len(s:popupmenu_list) > 0
                let results = s:vimim_onekey_menu_filter()
            endif
        endif
        if s:hjkl_l > 0 && len(s:matched_list) > &pumheight
            if s:show_me_not > 0
                if s:hjkl_l % 2 > 0
                    for line in s:matched_list
                        let oneline = join(reverse(split(line,'\zs')),'')
                        call add(results, oneline)
                    endfor
                endif
            else
                let &pumheight = 0
                if s:hjkl_l % 2 < 1
                    let &pumheight = s:saved_pumheights[1]
                endif
            endif
        endif
    endif
    if s:pageup_pagedown != 0
    \&& s:vimim_custom_label > 0
    \&& len(s:matched_list) > &pumheight
        let results = s:vimim_pageup_pagedown()
    endif
    return results
endfunction

" ------------------------------------
function! s:vimim_onekey_menu_format()
" ------------------------------------
    " use 1234567890/qwertyuiop to control popup textwidth
    let lines = copy(s:matched_list)
    let filter = 'substitute(' .'v:val'. ",'^\\s\\+\\|\\s\\+$','','g')"
    call map(lines, filter)
    let lines = split(join(lines),'  ')
    let filter = 'substitute(' .'v:val'. ",' ','','g')"
    call map(lines, filter)
    if s:hjkl_x == 1
        return lines
    endif
    let n = s:hjkl_x * (7-s:multibyte)
    let textwidth = repeat('.', n)
    let results = []
    for line in lines
        let onelines = split(line, textwidth . '\zs')
        call add(onelines, '')
        call extend(results, onelines)
    endfor
    return results
endfunction

" ------------------------------------
function! s:vimim_onekey_menu_filter()
" ------------------------------------
    " use 1234567890/qwertyuiop as digital filter
    call s:vimim_load_cjk_file()
    let results = s:vimim_cjk_filter_list()
    if empty(results) && !empty(len(s:hjkl_x))
        let number_before = strpart(s:hjkl_x,0,len(s:hjkl_x)-1)
        if len(number_before) > 0
            let s:hjkl_x = number_before
            let results = s:vimim_cjk_filter_list()
        endif
    endif
    return results
endfunction

" ---------------------------------
function! s:vimim_cjk_filter_list()
" ---------------------------------
    let i = 0
    let foods = []
    for items in s:popupmenu_list
        let chinese = s:vimim_cjk_digit_filter(items.word)
        if !empty(chinese)
            call add(foods, i)
        endif
        let i += 1
    endfor
    if empty(foods)
        return []
    endif
    let results = []
    for i in foods
        let menu = s:popupmenu_list[i].word
        call add(results, menu)
    endfor
    return results
endfunction

" -----------------------------------------
function! s:vimim_cjk_digit_filter(chinese)
" -----------------------------------------
" smart digital filter: 马力 7712 4002
"   (1)   ma<C-6>       马   => filter with   7712
"   (2) mali<C-6>       马力 => filter with 7 4002
" -----------------------------------------
    let chinese = a:chinese
    if empty(len(s:hjkl_x)) || empty(chinese)
        return 0
    endif
    let digit_head = ""
    let digit_tail = ""
    let words = split(chinese,'\zs')
    for cjk in words
        let ddddd = char2nr(cjk)
        let line = ddddd - 19968
        if cjk =~ '\w' || line < 0 || line > 20902
            continue
        else
            let values = split(get(s:cjk_lines,line))
            let column = 1 + s:vimim_digit_4corner
            let digit = get(values, column)
            let digit_head .= digit[:0]
            let digit_tail  = digit[1:]
        endif
    endfor
    let number = digit_head . digit_tail
    let pattern = "^" . s:hjkl_x
    let matched = match(number, pattern)
    if matched < 0
        let chinese = 0
    endif
    return chinese
endfunction

" ---------------------------------
function! s:vimim_pageup_pagedown()
" ---------------------------------
    let matched_list = s:matched_list
    let length = len(matched_list)
    let first_page = &pumheight - 1
    if s:vimim_loop_pageup_pagedown > 0
        if first_page < 1
            let first_page = 9
        endif
        let shift = s:pageup_pagedown * first_page
        if length > first_page
            let partition = shift
            if shift < 0
                let partition = length + shift
            endif
            let A = matched_list[: partition-1]
            let B = matched_list[partition :]
            let matched_list = B + A
        endif
    else
        let page = s:pageup_pagedown * &pumheight
        if page < 0
            " no more PageUp after the first page
            let s:pageup_pagedown += 1
            let matched_list = matched_list[0 : first_page]
        elseif page >= length
            " no more PageDown after the last page
            let s:pageup_pagedown -= 1
            let last_page = length / &pumheight
            if empty(length % &pumheight)
                let last_page -= 1
            endif
            let last_page = last_page * &pumheight
            let matched_list = matched_list[last_page :]
        else
            let matched_list = matched_list[page :]
        endif
    endif
    return matched_list
endfunction

" -------------------------------------------
function! s:vimim_onekey_pumvisible_mapping()
" -------------------------------------------
    let map_list = split('hjkl<>sxmn', '\zs')
    for _ in map_list
        sil!exe 'inoremap<expr> '._.' <SID>vimim_onekey_hjkl("'._.'")'
    endfor
    let map_list = s:qwerty + range(10)
    for _ in map_list
        sil!exe 'inoremap<expr> '._.' <SID>vimim_onekey_qwerty("'._.'")'
    endfor
    let map_list = s:AZ_list
    for _ in map_list
        sil!exe 'inoremap<expr> '._.' <SID>vimim_onekey_capital("'._.'")'
    endfor
endfunction

" -----------------------------------
function! <SID>vimim_onekey_hjkl(key)
" -----------------------------------
    let key = a:key
    let toggles = split('shlmn','\zs')
    if pumvisible()
        if a:key == 'j'
            let key  = '\<Down>'
        elseif a:key == 'k'
            let key  = '\<Up>'
        elseif a:key =~ "[<>]"
            let key  = '\<C-Y>'.s:punctuations[nr2char(char2nr(a:key)-16)]
        else
            if a:key == 'x'
                call g:vimim_reset_after_insert()
            elseif a:key =~ "[shlmn]"
                for toggle in toggles
                    if toggle == a:key
                        exe 'let s:hjkl_' . toggle . ' += 1'
                        let s:hjkl_n = a:key == 'm' ? 0 : s:hjkl_n
                        let s:hjkl_m = a:key == 'n' ? 0 : s:hjkl_m
                    endif
                endfor
            endif
            let key = '\<C-E>\<C-R>=g:vimim()\<CR>'
        endif
    endif
    sil!exe 'sil!return "' . key . '"'
endfunction

" -------------------------------------
function! <SID>vimim_onekey_qwerty(key)
" -------------------------------------
    let key = a:key
    if pumvisible()
        if key =~ '\l'
            let key = match(s:qwerty, a:key)
        endif
        let s:hjkl_x = s:show_me_not ? key : s:hjkl_x . key
        let key = '\<C-E>\<C-R>=g:vimim()\<CR>'
    endif
    sil!exe 'sil!return "' . key . '"'
endfunction

" --------------------------------------
function! <SID>vimim_onekey_capital(key)
" --------------------------------------
    let key = a:key
    if pumvisible()
        let key  = '\<C-E>'
        let key .= tolower(a:key)
        let key .= '\<C-R>=g:vimim()\<CR>'
    endif
    sil!exe 'sil!return "' . key . '"'
endfunction

" ============================================= }}}
let s:VimIM += [" ====  unicode          ==== {{{"]
" =================================================

" -------------------------------------
function! s:vimim_initialize_encoding()
" -------------------------------------
    let s:encoding = "utf8"
    if &encoding == "chinese"
    \|| &encoding == "cp936"
    \|| &encoding == "gb2312"
    \|| &encoding == "gbk"
    \|| &encoding == "euc-cn"
        let s:encoding = "chinese"
    elseif &encoding == "taiwan"
    \|| &encoding == "cp950"
    \|| &encoding == "big5"
    \|| &encoding == "euc-tw"
        let s:encoding = "taiwan"
    endif
    " ------------ ----------------- -------------- -----------
    " vim encoding datafile encoding s:localization performance
    " ------------ ----------------- -------------- -----------
    "   utf-8          utf-8                0          good
    "   chinese        chinese              0          good
    "   utf-8          chinese              1          bad
    "   chinese        utf-8                2          bad
    " ------------ ----------------- -------------- -----------
    let s:localization = 0
    if &encoding == "utf-8"
        if len("datafile_fenc_chinese") > 20110129
            let s:localization = 1
        endif
    elseif s:vimim_data_file_encoding =~ &encoding
        let s:localization = 0
    else
        let s:localization = 2
    endif
    let s:multibyte = 2
    if &encoding == "utf-8"
        let s:multibyte = 3
    endif
endfunction

" ------------------------------------------
function! s:vimim_get_unicode_list(keyboard)
" ------------------------------------------
    let ddddd = s:vimim_get_unicode_ddddd(a:keyboard)
    if ddddd < 8080 || ddddd > 19968+20902
        return []
    endif
    let words = []
    let height = 108
    for i in range(height)
        let chinese = nr2char(ddddd+i)
        call add(words, chinese)
    endfor
    return words
endfunction

" ----------------------------------
function! s:vimim_get_unicode_menu()
" ----------------------------------
    let one_before = getline(".")[col(".")-2]
    if empty(one_before) || one_before =~# s:valid_key
        return ""
    endif
    let start = s:multibyte + 1
    let char_before = getline(".")[col(".")-start : col(".")-2]
    let ddddd = char2nr(char_before)
    let uxxxx = ""
    if ddddd > 127
        let uxxxx = printf('u%04x', ddddd)
    endif
    if !empty(uxxxx)
        let uxxxx .= '\<C-R>=g:vimim()\<CR>'
    endif
    sil!exe 'sil!return "' . uxxxx . '"'
endfunction

" -------------------------------------------
function! s:vimim_get_unicode_ddddd(keyboard)
" -------------------------------------------
    if strlen(a:keyboard) != 5
        return 0
    endif
    let ddddd = 0
    if a:keyboard =~# '^u\x\{4}$'
        " show hex unicode popup menu: u808f
        let xxxx = a:keyboard[1:]
        let ddddd = str2nr(xxxx, 16)
    elseif a:keyboard =~# '^\d\{5}$'
        " show decimal unicode popup menu: 32911
        let ddddd = str2nr(a:keyboard, 10)
    endif
    if empty(ddddd) || ddddd > 0xffff
        return 0
    endif
    return ddddd
endfunction

" -------------------------------------------
function! s:vimim_cjk_property_display(ddddd)
" -------------------------------------------
    let unicode = printf('u%04x', a:ddddd)
    if empty(s:has_cjk_file)
        return unicode . s:space . a:ddddd
    endif
    call s:vimim_load_cjk_file()
    let chinese = nr2char(a:ddddd)
    let five = get(s:vimim_get_property(chinese,1),0)
    let four = get(s:vimim_get_property(chinese,2),0)
    let digit = five
    if s:vimim_digit_4corner > 0
        let digit = four
    endif
    let pinyin = get(s:vimim_get_property(chinese,'pinyin'),0)
    let english = get(s:vimim_get_property(chinese,'english'),0)
    let keyboard_head = get(s:keyboard_list,0)
    if keyboard_head =~ s:uxxxx
        let unicode = unicode . s:space
    else
        let unicode = ""
    endif
    let unicode .= digit . s:space . pinyin
    if !empty(english)
        let unicode .= s:space . english
    endif
    return unicode
endfunction

" -----------------------------------------------
function! s:vimim_get_property(chinese, property)
" -----------------------------------------------
    let property = a:property
    let headers = []
    let bodies = []
    for chinese in split(a:chinese, '\zs')
        let ddddd = char2nr(chinese)
        let line = ddddd - 19968
        if line < 0 || line > 20902
            continue
        endif
        let head = ''
        if property == 'unicode'
            let head = printf('%x', ddddd)
        elseif s:has_cjk_file > 0
            let values = split(get(s:cjk_lines,line))
            if property =~ '\d'
                let head = get(values, property)
            elseif property == 'pinyin'
                let head = get(values, 3)
            elseif property == 'english'
                let head = join(values[4:-2])
            endif
        endif
        if empty(head)
            continue
        endif
        call add(headers, head)
        let spaces = ''
        let gap = len(head)-2
        if gap > 0
            let space = ' '
            for i in range(gap)
                let spaces .= space
            endfor
        endif
        call add(bodies, chinese . spaces)
    endfor
    return [join(headers), join(bodies)]
endfunction

" ============================================= }}}
let s:VimIM += [" ====  multibyte        ==== {{{"]
" =================================================

" ------------------------------------
function! s:vimim_dictionary_chinese()
" ------------------------------------
    let s:space = "　"
    let s:colon = "："
    let s:left  = "【"
    let s:right = "】"
    let s:chinese = {}
    let s:chinese['chinese']     = ['中文']
    let s:chinese['english']     = ['英文']
    let s:chinese['datafile']    = ['文件']
    let s:chinese['directory']   = ['目录','目錄']
    let s:chinese['database']    = ['词库','詞庫']
    let s:chinese['standard']    = ['标准','標準']
    let s:chinese['cjk']         = ['字库','字庫']
    let s:chinese['auto']        = ['自动','自動']
    let s:chinese['computer']    = ['电脑','電腦']
    let s:chinese['encoding']    = ['编码','編碼']
    let s:chinese['environment'] = ['环境','環境']
    let s:chinese['input']       = ['输入','輸入']
    let s:chinese['font']        = ['字体','字體']
    let s:chinese['static']      = ['静态','靜態']
    let s:chinese['dynamic']     = ['动态','動態']
    let s:chinese['style']       = ['风格','風格']
    let s:chinese['wubi']        = ['五笔','五筆']
    let s:chinese['hangul']      = ['韩文','韓文']
    let s:chinese['xinhua']      = ['新华','新華']
    let s:chinese['zhengma']     = ['郑码','鄭碼']
    let s:chinese['cangjie']     = ['仓颉','倉頡']
    let s:chinese['boshiamy']    = ['呒虾米','嘸蝦米']
    let s:chinese['newcentury']  = ['新世纪','新世紀']
    let s:chinese['taijima']     = ['太极码','太極碼']
    let s:chinese['yong']        = ['永码','永碼']
    let s:chinese['wu']          = ['吴语','吳語']
    let s:chinese['erbi']        = ['二笔','二筆']
    let s:chinese['jidian']      = ['极点','極點']
    let s:chinese['shuangpin']   = ['双拼','雙拼']
    let s:chinese['abc']         = ['智能双打','智能雙打']
    let s:chinese['ms']          = ['微软','微軟']
    let s:chinese['nature']      = ['自然码','自然碼']
    let s:chinese['purple']      = ['紫光']
    let s:chinese['plusplus']    = ['加加']
    let s:chinese['flypy']       = ['小鹤','小鶴']
    let s:chinese['onekey']      = ['点石成金','點石成金']
    let s:chinese['quick']       = ['速成']
    let s:chinese['phonetic']    = ['注音']
    let s:chinese['array30']     = ['行列']
    let s:chinese['pinyin']      = ['拼音']
    let s:chinese['revision']    = ['版本']
    let s:chinese['full_width']  = ['全角']
    let s:chinese['half_width']  = ['半角']
    let s:chinese['cloudatwill'] = ['想云就云','想雲就雲']
    let s:chinese['mycloud']     = ['自己的云','自己的雲']
    let s:chinese['cloud']       = ['云','雲']
    let s:chinese['sogou']       = ['搜狗']
    let s:chinese['qq']          = ['QQ']
    let s:chinese['google']      = ['谷歌']
    let s:chinese['baidu']       = ['百度']
endfunction

" ----------------------------------------
function! s:vimim_dictionary_quantifiers()
" ----------------------------------------
    if s:vimim_imode_pinyin < 1
        return
    endif
    let s:quantifiers['1'] = '一壹甲①⒈⑴'
    let s:quantifiers['2'] = '二贰乙②⒉⑵'
    let s:quantifiers['3'] = '三叁丙③⒊⑶'
    let s:quantifiers['4'] = '四肆丁④⒋⑷'
    let s:quantifiers['5'] = '五伍戊⑤⒌⑸'
    let s:quantifiers['6'] = '六陆己⑥⒍⑹'
    let s:quantifiers['7'] = '七柒庚⑦⒎⑺'
    let s:quantifiers['8'] = '八捌辛⑧⒏⑻'
    let s:quantifiers['9'] = '九玖壬⑨⒐⑼'
    let s:quantifiers['0'] = '〇零癸⑩⒑⑽十拾'
    let s:quantifiers['a'] = '秒'
    let s:quantifiers['b'] = '步百佰把包杯本笔部班'
    let s:quantifiers['c'] = '厘次餐场串处床'
    let s:quantifiers['d'] = '第度点袋道滴碟日顶栋堆对朵堵顿'
    let s:quantifiers['e'] = '亿'
    let s:quantifiers['f'] = '分份发封付副幅峰方服'
    let s:quantifiers['g'] = '个根股管'
    let s:quantifiers['h'] = '时毫行盒壶户回'
    let s:quantifiers['i'] = '毫'
    let s:quantifiers['j'] = '斤家具架间件节剂具捲卷茎记'
    let s:quantifiers['k'] = '克口块棵颗捆孔'
    let s:quantifiers['l'] = '里粒类辆列轮厘升领缕'
    let s:quantifiers['m'] = '月米名枚面门'
    let s:quantifiers['n'] = '年'
    let s:quantifiers['o'] = '度'
    let s:quantifiers['p'] = '磅盆瓶排盘盆匹片篇撇喷'
    let s:quantifiers['q'] = '千仟群'
    let s:quantifiers['r'] = '日'
    let s:quantifiers['s'] = '十拾时升艘扇首双所束手秒'
    let s:quantifiers['t'] = '天吨条头通堂趟台套桶筒贴'
    let s:quantifiers['u'] = '微'
    let s:quantifiers['w'] = '万位味碗窝晚'
    let s:quantifiers['x'] = '升席些项'
    let s:quantifiers['y'] = '年亿叶月'
    let s:quantifiers['z'] = '种只张株支枝盏座阵桩尊则站幢宗兆'
endfunction

" ----------------------------------------------
function! s:vimim_imode_number(keyboard, prefix)
" ----------------------------------------------
    " usage: i88<C-6> ii88<C-6> i1g<C-6> isw8ql
    if empty(s:vimim_imode_pinyin)
        return []
    endif
    let keyboard = a:keyboard
    if keyboard[0:1] ==# 'ii'
        let keyboard = 'I' . strpart(keyboard,2)
    endif
    let ii_keyboard = keyboard
    let keyboard = strpart(keyboard,1)
    if keyboard !~ '^\d\+' && keyboard !~# '^[ds]'
    \&& len(substitute(keyboard,'\d','','')) > 1
        return []
    endif
    let digit_alpha = keyboard
    if keyboard =~# '^\d*\l\{1}$'
        let digit_alpha = keyboard[:-2]
    endif
    let keyboards = split(digit_alpha, '\ze')
    let i = ii_keyboard[:0]
    let number = ""
    for char in keyboards
        if has_key(s:quantifiers, char)
            let quantifiers = split(s:quantifiers[char], '\zs')
            if i ==# 'i'
                let char = get(quantifiers, 0)
            elseif i ==# 'I'
                let char = get(quantifiers, 1)
            endif
        endif
        let number .= char
    endfor
    if empty(number)
        return []
    endif
    let numbers = [number]
    let last_char = keyboard[-1:]
    if !empty(last_char) && has_key(s:quantifiers, last_char)
        let quantifier = s:quantifiers[last_char]
        let quantifiers = split(quantifier, '\zs')
        if keyboard =~# '^[ds]\=\d*\l\{1}$'
            if keyboard =~# '^[ds]'
                let number = strpart(number,0,len(number)-s:multibyte)
            endif
            let numbers = map(copy(quantifiers), 'number . v:val')
        elseif keyboard =~# '^\d*$' && len(keyboards)<2 && i ==# 'i'
            let numbers = quantifiers
        endif
    endif
    return numbers
endfunction

" ----------------------------------------
function! s:vimim_dictionary_punctuation()
" ----------------------------------------
    let s:punctuations = {}
    let s:punctuations['@'] = s:space
    let s:punctuations[':'] = s:colon
    let s:punctuations['['] = s:left
    let s:punctuations[']'] = s:right
    let s:punctuations['+'] = "＋"
    let s:punctuations['#'] = '＃'
    let s:punctuations['&'] = '＆'
    let s:punctuations['%'] = '％'
    let s:punctuations['$'] = '￥'
    let s:punctuations['!'] = '！'
    let s:punctuations['~'] = '～'
    let s:punctuations['('] = '（'
    let s:punctuations[')'] = '）'
    let s:punctuations['{'] = '〖'
    let s:punctuations['}'] = '〗'
    let s:punctuations['^'] = '……'
    let s:punctuations['_'] = '——'
    let s:punctuations['<'] = '《'
    let s:punctuations['>'] = '》'
    let s:punctuations['-'] = '－'
    let s:punctuations['='] = '＝'
    let s:punctuations[';'] = '；'
    let s:punctuations[','] = '，'
    let s:punctuations['.'] = '。'
    let s:punctuations['?'] = '？'
    let s:punctuations['*'] = '﹡'
    " ------------------------------------
    let s:evils = {}
    if empty(s:vimim_backslash_close_pinyin)
        let s:evils['\'] = '、'
    endif
    if empty(s:vimim_latex_suite)
        let s:evils["'"] = '‘’'
        let s:evils['"'] = '“”'
    endif
endfunction

" -------------------------------------------------
function! s:vimim_initialize_frontend_punctuation()
" -------------------------------------------------
    for char in s:valid_keys
        if has_key(s:punctuations, char)
            if s:ui.has_dot == 1
                unlet s:punctuations[char]
            elseif char !~# "[*.']"
                unlet s:punctuations[char]
            endif
        endif
    endfor
endfunction

" -------------------------------------
function! s:vimim_punctuation_mapping()
" -------------------------------------
    if s:chinese_punctuation > 0
        if empty(s:vimim_latex_suite)
            inoremap ' <C-R>=<SID>vimim_get_quote(1)<CR>
            inoremap " <C-R>=<SID>vimim_get_quote(2)<CR>
        endif
        if empty(s:vimim_backslash_close_pinyin)
            sil!exe 'inoremap <Bslash> ' .
            \ '<C-R>=pumvisible() ? "<C-Y>" : ""<CR>' . s:evils['\']
        endif
    else
        for _ in keys(s:evils)
            sil!exe 'iunmap '. _
        endfor
    endif
    for _ in keys(s:punctuations)
        silent!exe 'inoremap <silent> <expr> '    ._.
        \ ' <SID>vimim_chinese_punctuation_map("'._.'")'
    endfor
    return ""
endfunction

" -----------------------------------------------
function! <SID>vimim_chinese_punctuation_map(key)
" -----------------------------------------------
    let key = a:key
    if s:chinese_punctuation > 0
        let one_before = getline(".")[col(".")-2]
        if one_before !~ '\w' || pumvisible()
            if has_key(s:punctuations, a:key)
                let key = s:punctuations[a:key]
            else
                let key = a:key
            endif
        endif
    endif
    if pumvisible()
        if a:key =~ "[-=]"
            if a:key == "-"
                let s:pageup_pagedown -= 1
            else
                let s:pageup_pagedown += 1
            endif
            let key = '\<C-E>\<C-R>=g:vimim()\<CR>'
        else
            let key = '\<C-Y>' . key
            if a:key =~ "[][]"
                let key = s:vimim_square_bracket(a:key)
            elseif a:key == ";"
                let key = '\<Down>\<C-Y>'
            endif
            call g:vimim_reset_after_insert()
        endif
    endif
    sil!exe 'sil!return "' . key . '"'
endfunction

" --------------------------------------------
function! s:vimim_onekey_punctuation_mapping()
" --------------------------------------------
    if s:vimim_chinese_punctuation < 0
        return ""
    endif
    let special_punctuation = "[]-=.,/?;"
    let map_list = split(special_punctuation,'\zs')
    for char in s:valid_keys
        let i = index(map_list, char)
        if i > -1 && char != "."
            unlet map_list[i]
        endif
    endfor
    for _ in map_list
        sil!exe 'ino<expr> '._.' <SID>vimim_onekey_punctuation("'._.'")'
    endfor
endfunction

" ------------------------------------------
function! <SID>vimim_onekey_punctuation(key)
" ------------------------------------------
    let hjkl = a:key
    if pumvisible()
        if a:key =~ ";"
            let hjkl = '\<C-Y>\<C-R>=g:vimim_menu_to_clip()\<CR>'
        elseif a:key =~ "[][]"
            let hjkl = s:vimim_square_bracket(a:key)
        elseif a:key =~ "[/?]"
            let hjkl = s:vimim_menu_search(a:key)
        elseif a:key =~ "[-,]"
            let hjkl = '\<PageUp>'
            if &pumheight > 0
                let s:pageup_pagedown -= 1
                let hjkl = '\<C-E>\<C-R>=g:vimim()\<CR>'
            endif
        elseif a:key =~ "[=.]"
            let hjkl = '\<PageDown>'
            if &pumheight > 0
                let s:pageup_pagedown += 1
                let hjkl = '\<C-E>\<C-R>=g:vimim()\<CR>'
            endif
        endif
    endif
    sil!exe 'sil!return "' . hjkl . '"'
endfunction

" ----------------------------------
function! <SID>vimim_get_quote(type)
" ----------------------------------
    let key = ""
    if a:type == 1
        let key = "'"
    elseif a:type == 2
        let key = '"'
    endif
    let quote = ""
    if !has_key(s:evils, key)
        return ""
    elseif pumvisible()
        let quote = '\<C-Y>'
    endif
    let pairs = split(s:evils[key], '\zs')
    if a:type == 1
        let s:smart_single_quotes += 1
        let quote .= get(pairs, s:smart_single_quotes % 2)
    elseif a:type == 2
        let s:smart_double_quotes += 1
        let quote .= get(pairs, s:smart_double_quotes % 2)
    endif
    sil!exe 'sil!return "' . quote . '"'
endfunction

" ============================================= }}}
let s:VimIM += [" ====  vimim.cjk.txt    ==== {{{"]
" =================================================

" -------------------------------------
function! s:vimim_initialize_cjk_file()
" -------------------------------------
    let s:has_cjk_file = 0
    let s:cjk_file = 0
    let s:cjk_cache = {}
    let s:cjk_lines = []
    let datafile = "vimim.cjk.txt"
    let datafile = s:vimim_check_filereadable(datafile)
    if !empty(datafile)
        let s:cjk_file = datafile
        let s:has_cjk_file = 1
        if empty(s:backend.datafile) && empty(s:backend.directory)
            let s:has_cjk_file = 2
        endif
    endif
endfunction

" -------------------------------
function! s:vimim_load_cjk_file()
" -------------------------------
    " load cjk lines and build one char cache
    if empty(s:cjk_lines) && s:has_cjk_file > 0
        let s:cjk_lines = s:vimim_readfile(s:cjk_file)
    elseif len(s:cjk_cache) > len('cjk_cache')
        return
    endif
    let cjk = len(s:cjk_lines)
    if cjk < 20902
        let s:cjk_lines = []
        let s:has_cjk_file = 0
    elseif cjk == 20902
        for _ in s:az_list
            call s:vimim_cjk_match(_)
        endfor
    elseif cjk > 20902
        for line in s:cjk_lines[20902: -1]
            let lines = split(line)
            let char = get(lines,0)
            let right = get(lines,1)
            let results = split(right, '\zs')
            if has_key(s:cjk_cache, char)
                let history_results = s:cjk_cache[char]
                call extend(results, history_results, 0)
            endif
            let s:cjk_cache[char] = results
        endfor
    endif
endfunction

" --------------------------------------------
function! s:vimim_cjk_sentence_match(keyboard)
" --------------------------------------------
    let keyboard = a:keyboard
    let head = 0
    if s:show_me_not > 0 || len(keyboard) == 1
        let head = keyboard
    elseif keyboard =~ '\d'
        if keyboard =~ '^\d' && keyboard !~ '\D'
            " output is '6021' for input 6021272260021762
            if len(keyboard) % 4 < 1
                let pattern = '^\d\{' . 4 . '}'
                let delimiter = match(keyboard, pattern)
                if delimiter > -1
                    let head = s:vimim_get_head(keyboard, 4)
                endif
            endif
        elseif keyboard =~ '^\l\+\d\+\>'
            let head = keyboard
        elseif keyboard =~ '^\l\+\d\+'
            " output is 'wo23' for input wo23you40yigemeng
            let partition = match(keyboard, '\d')
            while partition > -1
                let partition += 1
                if keyboard[partition : partition] =~ '\D'
                    break
                endif
            endwhile
            let head = s:vimim_get_head(keyboard, partition)
        endif
    elseif s:ui.im == 'pinyin' || s:has_cjk_file > 1
        if len(keyboard)%5 < 1 && keyboard !~ "[.']"
            " output is 'm7712' for input muuqwxeyqpjeqqq
            let delimiter = match(keyboard, '^\l\l\l\l\l')
            if delimiter > -1
                let llll = keyboard[1:4]
                let dddd = s:vimim_qwertyuiop_1234567890(llll)
                if !empty(dddd)
                    let ldddd = keyboard[0:0] . dddd
                    let keyboard = ldddd . keyboard[5:-1]
                    let head = s:vimim_get_head(keyboard, 5)
                endif
            endif
        endif
        if empty(head)
            let a_keyboard = keyboard
            let magic_tail = keyboard[-1:-1]
            if magic_tail ==# "."
                "  magic trailing dot to use control cjjp: sssss.
                let s:hjkl_m += 1
                let a_keyboard = keyboard[0 : len(keyboard)-2]
            endif
            call s:vimim_load_cjk_file()
            let grep = '^' . a_keyboard . '\>'
            let matched = match(s:cjk_lines, grep)
            if s:hjkl_m > 0
                let keyboard = s:vimim_toggle_cjjp(a_keyboard)
            elseif matched < 0 && s:has_cjk_file > 0
                let keyboard = s:vimim_toggle_pinyin(a_keyboard)
            endif
            let head = s:vimim_dot_by_dot(keyboard)
        endif
    endif
    return head
endfunction

" -----------------------------------------------
function! s:vimim_qwertyuiop_1234567890(keyboard)
" -----------------------------------------------
    " output is '7712' for input uuqw
    if a:keyboard =~ '\d' || empty(s:has_cjk_file)
        return 0
    endif
    let dddd = ""
    for char in split(a:keyboard, '\zs')
        let digit = match(s:qwerty, char)
        if digit < 0
            return 0
        else
            let dddd .= digit
        endif
    endfor
    return dddd
endfunction

" -----------------------------------
function! s:vimim_cjk_match(keyboard)
" -----------------------------------
    if empty(s:has_cjk_file)
        return []
    endif
    let keyboard = a:keyboard
    if len(keyboard) == 1 && has_key(s:cjk_cache, keyboard)
        if keyboard =~ '\d'
            let s:hjkl_x = keyboard
        endif
        return s:cjk_cache[keyboard]
    endif
    let dddddd = 6 - 2 * s:vimim_digit_4corner
    let grep_frequency = '.*' . '\s\d\+$'
    let grep = ""
    " ------------------------------------------------------
    if keyboard =~ '\d'
        if keyboard =~# '^\l\l\+[1-5]\>' && empty(len(s:hjkl_x))
            " cjk pinyin with tone: huan2hai2
            let grep = keyboard . '[a-z ]'
        else
            let digit = ""
            if keyboard =~ '^\d\+' && keyboard !~ '\D'
                " cjk free-style digit input: 7 77 771 7712"
                let digit = keyboard
            elseif keyboard =~ '^\l\+\d\+'
                " cjk free-style input/search: ma7 ma77 ma771 ma7712
                let digit = substitute(keyboard,'\a','','g')
            endif
            if !empty(digit)
                let space = dddddd - len(digit)
                let grep  = '\s' . digit
                let grep .= '\d\{' . space . '}\s'
                if dddddd == 6
                    let grep .= '\d\d\d\d\s'
                endif
                let alpha = substitute(keyboard,'\d','','g')
                if !empty(alpha)
                    " search le or yue from le4yue4
                    let grep .= '\(\l\+\d\)\=' . alpha
                elseif len(keyboard) == 1
                    " one-char-list by frequency y72/yue72 l72/le72
                    " search l or y from le4yue4 music happy 426
                    let grep .= grep_frequency
                endif
            endif
            if len(keyboard) < dddddd && len(string(digit)) > 0
                let s:hjkl_x = digit
            endif
        endif
    elseif s:ui.im == 'pinyin' || s:has_cjk_file > 1
        if len(keyboard) == 1 && keyboard !~ '[ai]'
            " cjk one-char-list by frequency y72/yue72 l72/le72
            let grep = '[ 0-9]' . keyboard . '\l*\d' . grep_frequency
        elseif keyboard =~ '^\l'
            " cjk multiple-char-list without frequency: huan2hai2
            " support all cases: /huan /hai /yet /huan2 /hai2
            let grep = '[ 0-9]' . keyboard . '[0-9]'
        endif
    else
        return []
    endif
    " ------------------------------------------------------
    let results = s:vimim_cjk_grep_results(grep)
    if len(results) > 0
        let results = sort(results, "s:vimim_sort_on_last")
        let filter = "strpart(" . 'v:val' . ", 0, s:multibyte)"
        call map(results, filter)
        if len(keyboard) == 1
            if !has_key(s:cjk_cache, keyboard)
                let s:cjk_cache[keyboard] = results
                return results
            endif
        endif
    endif
    return results
endfunction

" --------------------------------------
function! s:vimim_cjk_grep_results(grep)
" --------------------------------------
    let grep = a:grep
    if empty(grep)
        return []
    endif
    call s:vimim_load_cjk_file()
    if empty(s:has_cjk_file)
        return []
    endif
    let results = []
    let line = match(s:cjk_lines, grep)
    while line > -1
        let values = split(get(s:cjk_lines,line))
        let frequency_index = get(values, -1)
        if frequency_index =~ '\l'
            let frequency_index = 9999
        endif
        let chinese_frequency = get(values,0) . ' ' . frequency_index
        call add(results, chinese_frequency)
        let line = match(s:cjk_lines, grep, line+1)
    endwhile
    return results
endfunction

" -----------------------------------------
function s:vimim_sort_on_last(line1, line2)
" -----------------------------------------
    let line1 = get(split(a:line1),-1) + 1
    let line2 = get(split(a:line2),-1) + 1
    if line1 < line2
        return -1
    elseif line1 > line2
        return 1
    endif
    return 0
endfunction

" ---------------------------------------------
function! s:vimim_get_head(keyboard, partition)
" ---------------------------------------------
    if a:partition < 0
        return a:keyboard
    endif
    let keyboards = []
    let head = a:keyboard[0 : a:partition-1]
    let tail  = a:keyboard[a:partition : -1]
    call add(keyboards, head)
    if !empty(tail)
        call add(keyboards, tail)
    endif
    if len(s:keyboard_list) < 2
        let s:keyboard_list = copy(keyboards)
    endif
    return head
endfunction

" ----------------------------------------------
function! s:vimim_chinese_transfer() range abort
" ----------------------------------------------
" [usage] :VimIM
" (1) "quick and dirty" way to transfer Chinese to Chinese
" (2) 20% of the effort to solve 80% of the problem using one2one mapping
" ---------------------------------------------
    sil!call s:vimim_backend_initialization()
    if empty(s:has_cjk_file)
        " no toggle between simplified and tranditional Chinese
    elseif &encoding == "utf-8"
        call s:vimim_load_cjk_file()
        exe a:firstline.",".a:lastline.'s/./\=s:vimim_one2one(submatch(0))'
    endif
endfunction

" ------------------------------------------------
function! s:vimim_get_traditional_chinese(chinese)
" ------------------------------------------------
    call s:vimim_load_cjk_file()
    let chinese = ""
    let chinese_list = split(a:chinese,'\zs')
    for char in chinese_list
        let chinese .= s:vimim_one2one(char)
    endfor
    return chinese
endfunction

" --------------------------------
function! s:vimim_one2one(chinese)
" --------------------------------
    let ddddd = char2nr(a:chinese)
    let line = ddddd - 19968
    if line < 0 || line > 20902
        return a:chinese
    endif
    let values = split(get(s:cjk_lines,line))
    let traditional_chinese = get(split(get(values,0),'\zs'),1)
    if empty(traditional_chinese)
        return a:chinese
    else
        return traditional_chinese
    endif
endfunction

" ---------------------------------
function! <SID>vimim_visual_ctrl6()
" ---------------------------------
    let unnamed_register = getreg('"')
    let lines = split(unnamed_register,'\n')
    let new_positions = getpos(".")
    if len(lines) < 2
        " input:  one line 马力 highlighted in vim visual mode
        " output: unicode || 4corner && 5stroke && pinyin && cjjp
        sil!call s:vimim_backend_initialization()
        let results = s:vimim_reverse_lookup()
        if !empty(results)
            let line = line(".")
            call setline(line, results)
            let new_positions[1] = line + len(results) - 1
            let new_positions[2] = len(get(split(get(results,-1)),0))+1
            call setpos(".", new_positions)
        endif
    else
        " input:  visual block highlighted in vim visual mode
        " output: the highlighted displayed in omni popup window
        let key = "O"
        if unnamed_register =~ '\d' && join(lines) !~ '[^0-9[:blank:].]'
            let new_positions[1] = line("'>'")
            call setpos(".", new_positions)
            let key = "o"
        endif
        let n = virtcol("'<'") - 2
        if n > 0
            let b:ctrl6_space = repeat(" ",n)
            let key .= "^\<C-D>\<C-R>=b:ctrl6_space\<CR>"
        endif
        sil!call s:vimim_onekey_start()
        let key .= "vimim\<C-R>=g:vimim()\<CR>"
        sil!call feedkeys(key)
    endif
endfunction

" --------------------------------
function! s:vimim_reverse_lookup()
" --------------------------------
    let chinese = substitute(getreg('"'),'[\x00-\xff]','','g')
    if empty(chinese)
        return []
    endif
    let results = []
    let results_unicode = s:vimim_get_property(chinese, 'unicode')
    if !empty(results_unicode)
        call extend(results, results_unicode)
    endif
    if empty(s:has_cjk_file)
        return results
    endif
    call s:vimim_load_cjk_file()
    let results_digit = s:vimim_get_property(chinese, 2)
    call extend(results, results_digit)
    let results_digit = s:vimim_get_property(chinese, 1)
    call extend(results, results_digit)
    let results_pinyin = []  " 马力 => ma3 li2
    let result_cjjp = ""     " 马力 => ml
    let items = s:vimim_get_property(chinese, 'pinyin')
    if len(items) > 0
        let pinyin_head = get(items,0)
        if !empty(pinyin_head)
            call add(results_pinyin, pinyin_head)
            call add(results_pinyin, get(items,1))
            for pinyin in split(pinyin_head)
                let result_cjjp .= pinyin[0:0]
            endfor
            let result_cjjp .= " ".chinese
        endif
    endif
    if !empty(results_pinyin)
        call extend(results, results_pinyin)
        if result_cjjp =~ '\a'
            call add(results, result_cjjp)
        endif
    endif
    return results
endfunction

" ============================================= }}}
let s:VimIM += [" ====  miscellaneous    ==== {{{"]
" =================================================

" -------------------------------------
function! s:vimim_get_valid_im_name(im)
" -------------------------------------
    let im = a:im
    if im =~ '^wubi'
        let im = 'wubi'
    elseif im =~ '^pinyin'
        let im = 'pinyin'
        let s:vimim_imode_pinyin = 1
    elseif im !~ s:all_vimim_input_methods
        let im = 0
    endif
    return im
endfunction

" -----------------------------------------
function! s:vimim_set_special_im_property()
" -----------------------------------------
    if  s:ui.im == 'pinyin' || s:has_cjk_file > 0
        let s:quanpin_table = s:vimim_create_quanpin_table()
    endif
    if s:backend[s:ui.root][s:ui.im].name =~# "quote"
        let s:ui.has_dot = 2  " has apostrophe in datafile
    endif
    if s:ui.im == 'wu'
    \|| s:ui.im == 'erbi'
    \|| s:ui.im == 'yong'
    \|| s:ui.im == 'nature'
    \|| s:ui.im == 'boshiamy'
    \|| s:ui.im == 'phonetic'
    \|| s:ui.im == 'array30'
        let s:ui.has_dot = 1  " has dot in datafile
        let s:vimim_chinese_punctuation = -99
    endif
endfunction

" -----------------------------------------------
function! s:vimim_wubi_4char_auto_input(keyboard)
" -----------------------------------------------
" wubi non-stop typing by auto selection on each 4th
    let keyboard = a:keyboard
    if s:chinese_input_mode =~ 'dynamic'
        if len(keyboard) > 4
            let start = 4*((len(keyboard)-1)/4)
            let keyboard = strpart(keyboard, start)
        endif
        let s:keyboard_list = [keyboard]
    endif
    return keyboard
endfunction

" ------------------------------------
function! g:vimim_wubi_ctrl_e_ctrl_y()
" ------------------------------------
    let key = ""
    if pumvisible()
        let key = '\<C-E>'
        if empty(len(get(s:keyboard_list,0))%4)
            let key = '\<C-Y>'
        endif
    endif
    sil!exe 'sil!return "' . key . '"'
endfunction

" Thanks to Politz for creating this sexy plugin:
" http://www.vim.org/scripts/script.php?script_id=2006
let s:progressbar = {}
function! NewSimpleProgressBar(title, max_value, ...)
    if !has("statusline")
        return {}
    endif
    let winnr = a:0 ? a:1 : winnr()
    let b = copy(s:progressbar)
    let b.title = a:title
    let b.max_value = a:max_value
    let b.cur_value = 0
    let b.winnr = winnr
    let b.items = {
        \ 'title' : { 'color' : 'Statusline' },
        \ 'bar' : { 'fillchar' : ' ', 'color' : 'Statusline' ,
        \           'fillcolor' : 'DiffDelete' , 'bg' : 'Statusline' },
        \ 'counter' : { 'color' : 'Statusline' } }
    let b.stl_save = getwinvar(winnr,"&statusline")
    let b.lst_save = &laststatus"
    return b
endfunction
function! s:progressbar.paint()
    let max_len = winwidth(self.winnr)-1
    let t_len = strlen(self.title)+1+1
    let c_len = 2*strlen(self.max_value)+1+1+1
    let pb_len = max_len - t_len - c_len - 2
    let cur_pb_len = (pb_len*self.cur_value)/self.max_value
    let t_color = self.items.title.color
    let b_fcolor = self.items.bar.fillcolor
    let b_color = self.items.bar.color
    let c_color = self.items.counter.color
    let fc= strpart(self.items.bar.fillchar." ",0,1)
    let stl = "%#".t_color."#%-( ".self.title." %)".
        \"%#".b_color."#|".
        \"%#".b_fcolor."#%-(".repeat(fc,cur_pb_len)."%)".
        \"%#".b_color."#".repeat(" ",pb_len-cur_pb_len)."|".
        \"%=%#".c_color."#%( ".repeat(" ",(strlen(self.max_value)-
        \strlen(self.cur_value))).self.cur_value."/".self.max_value."  %)"
    set laststatus=2
    call setwinvar(self.winnr,"&stl",stl)
    redraw
endfunction
function! s:progressbar.incr( ... )
    let i = a:0 ? a:1 : 1
    let i+=self.cur_value
    let i = i < 0 ? 0 : i > self.max_value ? self.max_value : i
    let self.cur_value = i
    call self.paint()
    return self.cur_value
endfunction
function! s:progressbar.restore()
    call setwinvar(self.winnr,"&stl",self.stl_save)
    let &laststatus=self.lst_save
    redraw
endfunction

" ============================================= }}}
let s:VimIM += [" ====  plugin conflict  ==== {{{"]
" =================================================
" Thanks to frederick.zou for providing codes on this section:
" supertab      http://www.vim.org/scripts/script.php?script_id=1643
" autocomplpop  http://www.vim.org/scripts/script.php?script_id=1879
" word_complete http://www.vim.org/scripts/script.php?script_id=73

" -----------------------------------
function! s:vimim_plugins_fix_start()
" -----------------------------------
    if s:vimim_debug > 1
        return
    endif
    if !exists('s:acp_sid')
        let s:acp_sid = s:vimim_getsid('autoload/acp.vim')
        if !empty(s:acp_sid)
            AcpDisable
        endif
    endif
    if !exists('s:supertab_sid')
        let s:supertab_sid = s:vimim_getsid('plugin/supertab.vim')
    endif
    if !exists('s:word_complete')
        let s:word_complete = s:vimim_getsid('plugin/word_complete.vim')
        if !empty(s:word_complete)
            call EndWordComplete()
        endif
    endif
endfunction

" ----------------------------------
function! s:vimim_getsid(scriptname)
" ----------------------------------
    " use s:getsid to get script sid, translate <SID> to <SNR>N_ style
    let l:scriptname = a:scriptname
    " get output of ":scriptnames" in scriptnames_output variable
    if empty(s:scriptnames_output)
        let saved_shellslash=&shellslash
        set shellslash
        redir => s:scriptnames_output
        silent scriptnames
        redir END
        let &shellslash = saved_shellslash
    endif
    for line in split(s:scriptnames_output, "\n")
        " only do non-blank lines
        if line =~ l:scriptname
            " get the first number in the line.
            let nr = matchstr(line, '\d\+')
            return nr
        endif
    endfor
    return 0
endfunction

" ----------------------------------
function! s:vimim_plugins_fix_stop()
" ----------------------------------
    if s:vimim_debug > 1
        return
    endif
    if !empty(s:acp_sid)
        let ACPMappingDrivenkeys = [
            \ '-','_','~','^','.',',',':','!','#','=','%','$','@',
            \ '<','>','/','\','<Space>','<BS>','<CR>',]
        call extend(ACPMappingDrivenkeys, range(10))
        call extend(ACPMappingDrivenkeys, s:Az_list)
        for key in ACPMappingDrivenkeys
            exe printf('iu <silent> %s', key)
            exe printf('im <silent> %s %s<C-r>=<SNR>%s_feedPopup()<CR>',
            \ key, key, s:acp_sid)
        endfor
        AcpEnable
    endif
    if !empty(s:supertab_sid)
        let tab = s:supertab_sid
        if g:SuperTabMappingForward =~ '^<tab>$'
            exe printf("im <tab> <C-R>=<SNR>%s_SuperTab('p')<CR>", tab)
        endif
        if g:SuperTabMappingBackward =~ '^<s-tab>$'
            exe printf("im <s-tab> <C-R>=<SNR>%s_SuperTab('n')<CR>", tab)
        endif
    endif
endfunction

" ============================================= }}}
let s:VimIM += [" ====  user interface   ==== {{{"]
" =================================================

" --------------------------------
function! s:vimim_set_statusline()
" --------------------------------
    set laststatus=2
    if empty(&statusline)
        set statusline=%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P%{IMName()}
    elseif &statusline =~ 'IMName'
        " nothing, because it is already in the statusline
    elseif &statusline =~ '\V\^%!'
        let &statusline .= '.IMName()'
    else
        let &statusline .= '%{IMName()}'
    endif
endfunction

" ----------------
function! IMName()
" ----------------
" This function is for user-defined 'stl' 'statusline'
    if s:chinese_input_mode =~ 'onekey'
        if pumvisible()
            return s:vimim_statusline()
        endif
    elseif !empty(&omnifunc) && &omnifunc ==# 'VimIM'
        return s:vimim_statusline()
    endif
    return ""
endfunction

" ----------------------------
function! s:vimim_statusline()
" ----------------------------
    if empty(s:ui.root) || empty(s:ui.im)
        return ""
    endif
    if has_key(s:im_keycode, s:ui.im)
        let s:ui.statusline = s:backend[s:ui.root][s:ui.im].chinese
    endif
    let datafile = s:backend[s:ui.root][s:ui.im].name
    if s:ui.im =~# 'wubi'
        if datafile =~# 'wubi98'
            let s:ui.statusline .= '98'
        elseif datafile =~# 'wubi2000'
            let newcentury = s:vimim_chinese('newcentury')
            let s:ui.statusline = newcentury . s:ui.statusline
        elseif datafile =~# 'wubijd'
            let jidian = s:vimim_chinese('jidian')
            let s:ui.statusline = jidian . s:ui.statusline
        endif
        return s:vimim_get_chinese_im()
    endif
    if s:vimim_cloud_sogou == -777 && s:ui.im == 'mycloud'
        if !empty(s:mycloud_plugin)
            let __getname = s:backend.cloud.mycloud.directory
            let s:ui.statusline .= s:space . __getname
        endif
    endif
    if !empty(s:vimim_shuangpin)
        let s:ui.statusline .= s:space
        let s:ui.statusline .= s:shuangpin_keycode_chinese.chinese
    endif
    return s:vimim_get_chinese_im()
endfunction

" --------------------------------
function! s:vimim_get_chinese_im()
" --------------------------------
    let input_style = s:vimim_chinese('chinese')
    if s:vimim_chinese_input_mode =~ 'dynamic'
        let input_style .= s:vimim_chinese('dynamic')
    elseif s:vimim_chinese_input_mode =~ 'static'
        let input_style .= s:vimim_chinese('static')
    endif
    if s:chinese_input_mode !~ 'onekey'
        let punctuation = s:vimim_chinese('half_width')
        if s:chinese_punctuation > 0
            let punctuation = s:vimim_chinese('full_width')
        endif
        let s:ui.statusline .= s:space . punctuation
    endif
    let statusline = s:left . s:ui.statusline . s:right
    let statusline .= "VimIM"
    return input_style . statusline
endfunction

" --------------------------
function! s:vimim_label_on()
" --------------------------
    if s:vimim_custom_label < 1
        return
    endif
    let labels = range(1,9)
    let abcd_list = split(s:abcd, '\zs')
    if s:chinese_input_mode =~ 'onekey'
        let labels += abcd_list
        if s:has_cjk_file > 0
            let labels = abcd_list
        endif
        call remove(labels, "'")
    else
        for _ in abcd_list
            sil!exe 'iunmap '. _
        endfor
    endif
    for _ in labels
        silent!exe 'inoremap <silent> <expr> '  ._.
        \  ' <SID>vimim_alphabet_number_label("'._.'")'
    endfor
endfunction

" ---------------------------------------------
function! <SID>vimim_alphabet_number_label(key)
" ---------------------------------------------
    let key = a:key
    if pumvisible()
        let n = match(s:abcd, key)
        if key =~ '\d'
            let n = key - 1
        endif
        let s:has_pumvisible = 1
        let down = repeat("\<Down>", n)
        let yes = '\<C-Y>\<C-R>=g:vimim()\<CR>'
        let key = down . yes
        call g:vimim_reset_after_insert()
    endif
    sil!exe 'sil!return "' . key . '"'
endfunction

" --------------------------------
function! s:vimim_menu_search(key)
" --------------------------------
    let slash = ""
    if pumvisible()
        let slash = '\<C-Y>\<C-R>=g:vimim_menu_search_on()\<CR>'
        let slash .= a:key . '\<CR>'
    endif
    sil!exe 'sil!return "' . slash . '"'
endfunction

" --------------------------------
function! g:vimim_menu_search_on()
" --------------------------------
    let word = s:vimim_popup_word()
    let @/ = word
    if empty(word)
        let @/ = @_
    endif
    let repeat_times = len(word) / s:multibyte
    let row_start = s:start_row_before
    let row_end = line('.')
    let delete_chars = ""
    if repeat_times > 0 && row_end == row_start
        let delete_chars = repeat("\<BS>", repeat_times)
    endif
    let slash = delete_chars . "\<Esc>"
    sil!call g:vimim_stop()
    sil!exe 'sil!return "' . slash . '"'
endfunction

" -----------------------------------
function! s:vimim_square_bracket(key)
" -----------------------------------
    let key = a:key
    if pumvisible()
        let i = -1
        let left  = ""
        let right = ""
        if key == "]"
            let i = 0
            let left  = "\<Left>"
            let right = "\<Right>"
        endif
        if s:show_me_not < 1
            let backspace = '\<C-R>=g:vimim_bracket('.i.')\<CR>'
            let key = '\<C-Y>' . left . backspace . right
        endif
    endif
    sil!exe 'sil!return "' . key . '"'
endfunction

" -------------------------------
function! g:vimim_bracket(offset)
" -------------------------------
    let column_end = col('.')-1
    let column_start = s:start_column_before
    let range = column_end - column_start
    let repeat_times = range / s:multibyte
    let repeat_times += a:offset
    let row_end = line('.')
    let row_start = s:start_row_before
    let delete_char = ""
    if repeat_times > 0 && row_end == row_start
        let delete_char = repeat("\<BS>", repeat_times)
    endif
    if repeat_times < 1
        let current_line = getline(".")
        let chinese = strpart(current_line, column_start, s:multibyte)
        let delete_char = chinese
        if empty(a:offset)
            let chinese = s:left . chinese . s:right
            let delete_char = "\<Right>\<BS>" . chinese . "\<Left>"
        endif
    endif
    return delete_char
endfunction

" ------------------------
function! <SID>vimim_esc()
" ------------------------
    let key = '\<Esc>'
    if s:chinese_input_mode =~ 'onekey'
        call g:vimim_stop()
        let key = '\<Esc>'
    elseif pumvisible()
        let column_start = s:start_column_before
        let column_end = col('.') - 1
        let range = column_end - column_start
        let key = '\<C-E>' . repeat("\<BS>", range)
    endif
    call s:vimim_super_reset()
    sil!exe 'sil!return "' . key . '"'
endfunction

" ------------------------------
function! <SID>vimim_backspace()
" ------------------------------
    let key = '\<BS>'
    if pumvisible() && s:chinese_input_mode !~ 'onekey'
        let key  = "\<C-E>"
        let key .= "\<BS>"
        let key .= '\<C-R>=g:vimim()\<CR>'
    endif
    call s:vimim_super_reset()
    sil!exe 'sil!return "' . key . '"'
endfunction

" --------------------------
function! <SID>vimim_enter()
" --------------------------
    let one_before = getline(".")[col(".")-2]
    " <Enter> triple play for OneKey and static mode:
    "  (1) single <Enter> ==> seamless
    "  (2) double <Enter> ==> <Space>
    "  (3) triple <Enter> ==> <Enter>
    if one_before =~ '\S'
        let s:smart_enter += 1
    endif
    " <Enter> double play in dynamic mode:
    "  (1) after English (valid keys)    => Seamless
    "  (2) after Chinese or double Enter => Enter
    if s:chinese_input_mode =~ 'dynamic'
        if one_before =~ s:valid_key
            let s:smart_enter = 1
        else
            let s:smart_enter = 3
        endif
    endif
    let key = ""
    if pumvisible()
        let key = "\<C-E>"
    endif
    if s:smart_enter == 1
        " the first <Enter> does seamless
        let s:seamless_positions = getpos(".")
    else
        let key = "\<CR>"
        if s:smart_enter == 2
            let key = " "
        endif
        let s:smart_enter = 0
    endif
    sil!exe 'sil!return "' . key . '"'
endfunction

" --------------------------------------------
function! s:vimim_popupmenu_list(matched_list)
" --------------------------------------------
    let lines = a:matched_list
    if empty(lines) || type(lines) != type([])
        return []
    endif
    let tail = 0
    let label = 1
    let extra_text = ""
    let popupmenu_list = []
    let keyboard = join(s:keyboard_list,"")
    let first_in_list = get(lines,0)
    let &pumheight = s:show_me_not ? 0 : &pumheight
    if s:hjkl_n % 2 > 0
        if s:show_me_not > 0
            call reverse(lines)
            let label = len(lines)
        elseif s:ui.im == 'pinyin'
            let keyboard = join(split(join(s:keyboard_list,""),"'"),"")
        endif
    endif
    let menu = get(s:keyboard_list,0)
    for chinese in lines
        let complete_items = {}
        if first_in_list =~ '\s' && s:show_me_not < 1
            let pairs = split(chinese)
            if len(pairs) < 2
                continue
            endif
            let chinese = get(pairs, 1)
            let menu = get(pairs, 0)
            if s:vimim_custom_menu > 0
                let extra_text = menu
            endif
        endif
        if s:hjkl_s > 0 && s:hjkl_s % 2 > 0 && s:has_cjk_file > 0
            let chinese = s:vimim_get_traditional_chinese(chinese)
        endif
        if s:hjkl_h > 0 && s:hjkl_h % 2 > 0 && s:show_me_not < 1
            let extra_text = menu
            if empty(s:english_results)
                let ddddd = char2nr(chinese)
                let extra_text = s:vimim_cjk_property_display(ddddd)
            endif
        endif
        if empty(s:mycloud_plugin)
            if !empty(keyboard) && s:show_me_not < 1
                let keyboard_head_length = len(menu)
                if empty(s:ui.has_dot) && keyboard =~ "['.]"
                    " for vimim classic demo: i.have.a.dream
                    let keyboard_head_length += 1
                endif
                let tail = strpart(keyboard, keyboard_head_length)
                let chinese .= tail
            endif
        else
            let extra_text = get(split(menu,"_"),0)
        endif
        if s:vimim_custom_label > 0 && len(lines) > 1
            let labeling = s:vimim_get_labeling(label)
            if s:hjkl_n % 2 > 0 && s:show_me_not > 0
                let label -= 1
            else
                let label += 1
            endif
            let complete_items["abbr"] = labeling . chinese
        endif
        let complete_items["dup"] = 1
        let complete_items["menu"] = extra_text
        let complete_items["word"] = empty(chinese) ? s:space : chinese
        call add(popupmenu_list, complete_items)
    endfor
    if s:chinese_input_mode =~ 'onekey'
        let s:popupmenu_list = popupmenu_list
    endif
    if empty(s:matched_list)
        let s:matched_list = lines
    endif
    return popupmenu_list
endfunction

" -----------------------------------
function! s:vimim_get_labeling(label)
" -----------------------------------
    let labeling = a:label
    let fmt = '%2s '
    if s:chinese_input_mode =~ 'onekey'
        if s:show_me_not > 0
            let fmt = '%02s '
            if s:hjkl_h % 2 < 1
                let labeling = ""
            endif
        elseif a:label < &pumheight+1
            let label2 = s:abcd[a:label-1]
            if a:label < 2
                let label2 = "_"
            endif
            let labeling .= label2
            if s:has_cjk_file > 0
                let labeling = label2
            endif
        endif
        if s:hjkl_l > 0 && &pumheight < 1
            let fmt = '%02s '
        endif
    endif
    if !empty(labeling)
        let labeling = printf(fmt, labeling)
    endif
    return labeling
endfunction

" ============================================= }}}
let s:VimIM += [" ====  input english    ==== {{{"]
" =================================================

" ---------------------------------------
function! s:vimim_scan_english_datafile()
" ---------------------------------------
    let s:has_english_file = 0
    let s:english_file = 0
    let s:english_lines = []
    let datafile = "vimim.txt"
    let datafile = s:vimim_check_filereadable(datafile)
    if s:ui.im =~ 'pinyin' || s:has_cjk_file > 0
        if !empty(datafile)
            let s:english_file = datafile
            let s:has_english_file = 1
        endif
    endif
endfunction

" -------------------------------------------
function! s:vimim_check_filereadable(default)
" -------------------------------------------
    let default = a:default
    let datafile = s:vimim_hjkl_directory . default
    if filereadable(datafile)
        let default = 0
    else
        let datafile = s:path . default
        if filereadable(datafile)
            let default = 0
        endif
    endif
    if empty(default)
        return datafile
    endif
    return 0
endfunction

" -----------------------------------------------
function! s:vimim_onekey_english(keyboard, order)
" -----------------------------------------------
    let results = []
    if s:has_cjk_file > 0
        " [sql] select english from vimim.cjk.txt
        let grep_english = '\s' . a:keyboard . '\s'
        let results = s:vimim_cjk_grep_results(grep_english)
        if len(results) > 0
            let filter = "strpart(".'v:val'.", 0, s:multibyte)"
            call map(results, filter)
            let s:english_results = copy(results)
        endif
    endif
    if s:has_english_file > 0
        " [sql] select english from vimim.txt
        if empty(s:english_lines)
            let s:english_lines = s:vimim_readfile(s:english_file)
        endif
        let grep_english = '^' . a:keyboard . '\s'
        let matched = match(s:english_lines, grep_english)
        if matched < 0 && len(a:keyboard) > 3
            " support english shortcut: both haag haagendazs
            let grep_english = '^' . a:keyboard
            let matched = match(s:english_lines, grep_english)
        endif
        if matched > -1
            let line = get(s:english_lines, matched)
            let results = split(line)
            let menu = get(results, 0)
            if menu ==# a:keyboard
                let results = results[1:]
            endif
            if empty(a:order)
                call extend(s:english_results, results)
            else
                call extend(s:english_results, results, 0)
            endif
        endif
    endif
endfunction

" ----------------------------------
function! s:vimim_readfile(datafile)
" ----------------------------------
    if !filereadable(a:datafile)
        return []
    endif
    let lines = readfile(a:datafile)
    if s:localization > 0
        let  results = []
        for line in lines
            let line = s:vimim_i18n_read(line)
            call add(results, line)
        endfor
        let lines = results
    endif
    return lines
endfunction

" -------------------------------
function! s:vimim_i18n_read(line)
" -------------------------------
    let line = a:line
    if s:localization == 1
        let line = iconv(line, "chinese", "utf-8")
    elseif s:localization == 2
        let line = iconv(line, "utf-8", &enc)
    endif
    return line
endfunction

" ============================================= }}}
let s:VimIM += [" ====  input pinyin     ==== {{{"]
" =================================================

" ----------------------------------------
function! s:vimim_add_apostrophe(keyboard)
" ----------------------------------------
    let keyboard = a:keyboard
    if keyboard =~ "[']"
    \&& keyboard[0:0] != "'"
    \&& keyboard[-1:-1] != "'"
        " valid apostrophe is typed
    else
        let keyboard = s:vimim_quanpin_transform(keyboard)
    endif
    return keyboard
endfunction

" ------------------------------------------------
function! s:vimim_get_pinyin_from_pinyin(keyboard)
" ------------------------------------------------
    let keyboard = s:vimim_quanpin_transform(a:keyboard)
    let results = split(keyboard, "'")
    if len(results) > 1
        return results
    endif
    return []
endfunction

" ---------------------------------------
function! s:vimim_toggle_pinyin(keyboard)
" ---------------------------------------
    let keyboard = a:keyboard
    if s:hjkl_n < 1
        return keyboard
    elseif s:hjkl_n % 2 > 0
        " set pin'yin: woyouyigemeng => wo'you'yi'ge'meng
        let keyboard = s:vimim_quanpin_transform(keyboard)
    elseif len(s:keyboard_list) > 0 && get(s:keyboard_list,0) =~ "'"
        " reset pinyin: wo'you'yi'ge'meng => woyouyigemeng
        let keyboard = join(split(join(s:keyboard_list,""),"'"),"")
    endif
    return keyboard
endfunction

" -------------------------------------
function! s:vimim_toggle_cjjp(keyboard)
" -------------------------------------
    let keyboard = a:keyboard
    if s:hjkl_m < 1
        return keyboard
    elseif s:hjkl_m % 2 > 0
        " set cjjp:   wyygm => w'y'y'g'm
        let keyboard = join(split(keyboard,'\zs'),"'")
    elseif len(s:keyboard_list) > 0 && get(s:keyboard_list,0) =~ "'"
        " reset cjjp: w'y'y'g'm => wyygm
        let keyboard = join(split(join(s:keyboard_list,""),"'"),"")
    endif
    return keyboard
endfunction

" -----------------------------------------
function! s:vimim_quanpin_transform(pinyin)
" -----------------------------------------
    let qptable = s:quanpin_table
    if empty(qptable)
        return ""
    endif
    let item = a:pinyin
    let pinyinstr = ""
    let index = 0
    let lenitem = len(item)
    while index < lenitem
        if item[index] !~ "[a-z]"
            let index += 1
            continue
        endif
        for i in range(6,1,-1)
            let tmp = item[index : ]
            if len(tmp) < i
                continue
            endif
            let end = index+i
            let matchstr = item[index : end-1]
            if has_key(qptable, matchstr)
                let tempstr = item[end-1 : end]
                " special case for fanguo, which should be fan'guo
                if tempstr == "gu" || tempstr == "nu" || tempstr == "ni"
                    if has_key(qptable, matchstr[:-2])
                        let i -= 1
                        let matchstr = matchstr[:-2]
                    endif
                endif
                " follow ibus' rule
                let tempstr2 = item[end-2 : end+1]
                let tempstr3 = item[end-1 : end+1]
                let tempstr4 = item[end-1 : end+2]
                if (tempstr == "ge" && tempstr3 != "ger")
                    \ || (tempstr == "ne" && tempstr3 != "ner")
                    \ || (tempstr4 == "gong" || tempstr3 == "gou")
                    \ || (tempstr4 == "nong" || tempstr3 == "nou")
                    \ || (tempstr == "ga" || tempstr == "na")
                    \ || tempstr2 == "ier"
                    if has_key(qptable, matchstr[:-2])
                        let i -= 1
                        let matchstr = matchstr[:-2]
                    endif
                endif
                let pinyinstr .= "'" . qptable[matchstr]
                let index += i
                break
            elseif i == 1
                let pinyinstr .= "'" . item[index]
                let index += 1
                break
            else
                continue
            endif
        endfor
    endwhile
    if pinyinstr[0] == "'"
        return pinyinstr[1:]
    else
        return pinyinstr
    endif
endfunction

" --------------------------------------
function! s:vimim_create_quanpin_table()
" --------------------------------------
    let pinyin_list = s:vimim_get_pinyin_table()
    let table = {}
    for key in pinyin_list
        if key[0] == "'"
            let table[key[1:]] = key[1:]
        else
            let table[key] = key
        endif
    endfor
    for shengmu in ["b", "p", "m", "f", "d", "t", "l", "n", "g", "k", "h",
        \"j", "q", "x", "zh", "ch", "sh", "r", "z", "c", "s", "y", "w"]
        let table[shengmu] = shengmu
    endfor
    return table
endfunction

" ============================================= }}}
let s:VimIM += [" ====  input shuangpin  ==== {{{"]
" =================================================
" Thanks to Pan Shizhu for providing all shuangpin codes:

" --------------------------------------
function! s:vimim_initialize_shuangpin()
" --------------------------------------
    if empty(s:vimim_shuangpin)
    \|| !empty(s:shuangpin_table)
        return
    endif
    let s:vimim_imode_pinyin = 0
    let rules = s:vimim_shuangpin_generic()
    let chinese = ""
    let shuangpin = s:vimim_chinese('shuangpin')
    let keycode = "[0-9a-z'.]"
    if s:vimim_shuangpin == 'abc'
        let rules = s:vimim_shuangpin_abc(rules)
        let s:vimim_imode_pinyin = 1
        let chinese = s:vimim_chinese('abc')
        let shuangpin = ""
    elseif s:vimim_shuangpin == 'ms'
        let rules = s:vimim_shuangpin_ms(rules)
        let chinese = s:vimim_chinese('ms')
        let keycode = "[0-9a-z'.;]"
    elseif s:vimim_shuangpin == 'nature'
        let rules = s:vimim_shuangpin_nature(rules)
        let chinese = s:vimim_chinese('nature')
    elseif s:vimim_shuangpin == 'plusplus'
        let rules = s:vimim_shuangpin_plusplus(rules)
        let chinese = s:vimim_chinese('plusplus')
    elseif s:vimim_shuangpin == 'purple'
        let rules = s:vimim_shuangpin_purple(rules)
        let chinese = s:vimim_chinese('purple')
        let keycode = "[0-9a-z'.;]"
    elseif s:vimim_shuangpin == 'flypy'
        let rules = s:vimim_shuangpin_flypy(rules)
        let chinese = s:vimim_chinese('flypy')
    endif
    let s:shuangpin_table = s:vimim_create_shuangpin_table(rules)
    let s:shuangpin_keycode_chinese.chinese = chinese . shuangpin
    let s:shuangpin_keycode_chinese.keycode = keycode
endfunction

" ---------------------------------------------
function! s:vimim_shuangpin_transform(keyboard)
" ---------------------------------------------
    let keyboard = a:keyboard
    let size = strlen(keyboard)
    let ptr = 0
    let output = ""
    let bchar = ""    " work-around for sogou
    while ptr < size
        if keyboard[ptr] !~ "[a-z;]"
            " bypass all non-characters, i.e. 0-9 and A-Z are bypassed
            let output .= keyboard[ptr]
            let ptr += 1
        else
            if keyboard[ptr+1] =~ "[a-z;]"
                let sp1 = keyboard[ptr].keyboard[ptr+1]
            else
                let sp1 = keyboard[ptr]
            endif
            if has_key(s:shuangpin_table, sp1)
                " the last odd shuangpin code are output as only shengmu
                let output .= bchar . s:shuangpin_table[sp1]
            else
                " invalid shuangpin code are preserved
                let output .= sp1
            endif
            let ptr += strlen(sp1)
        endif
    endwhile
    if output[0] == "'"
        return output[1:]
    else
        return output
    endif
endfunction

"-----------------------------------
function! s:vimim_get_pinyin_table()
"-----------------------------------
" List of all valid pinyin.  Note: Don't change this function!
return [
\"'a", "'ai", "'an", "'ang", "'ao", 'ba', 'bai', 'ban', 'bang', 'bao',
\'bei', 'ben', 'beng', 'bi', 'bian', 'biao', 'bie', 'bin', 'bing', 'bo',
\'bu', 'ca', 'cai', 'can', 'cang', 'cao', 'ce', 'cen', 'ceng', 'cha',
\'chai', 'chan', 'chang', 'chao', 'che', 'chen', 'cheng', 'chi', 'chong',
\'chou', 'chu', 'chua', 'chuai', 'chuan', 'chuang', 'chui', 'chun', 'chuo',
\'ci', 'cong', 'cou', 'cu', 'cuan', 'cui', 'cun', 'cuo', 'da', 'dai',
\'dan', 'dang', 'dao', 'de', 'dei', 'deng', 'di', 'dia', 'dian', 'diao',
\'die', 'ding', 'diu', 'dong', 'dou', 'du', 'duan', 'dui', 'dun', 'duo',
\"'e", "'ei", "'en", "'er", 'fa', 'fan', 'fang', 'fe', 'fei', 'fen',
\'feng', 'fiao', 'fo', 'fou', 'fu', 'ga', 'gai', 'gan', 'gang', 'gao',
\'ge', 'gei', 'gen', 'geng', 'gong', 'gou', 'gu', 'gua', 'guai', 'guan',
\'guang', 'gui', 'gun', 'guo', 'ha', 'hai', 'han', 'hang', 'hao', 'he',
\'hei', 'hen', 'heng', 'hong', 'hou', 'hu', 'hua', 'huai', 'huan', 'huang',
\'hui', 'hun', 'huo', "'i", 'ji', 'jia', 'jian', 'jiang', 'jiao', 'jie',
\'jin', 'jing', 'jiong', 'jiu', 'ju', 'juan', 'jue', 'jun', 'ka', 'kai',
\'kan', 'kang', 'kao', 'ke', 'ken', 'keng', 'kong', 'kou', 'ku', 'kua',
\'kuai', 'kuan', 'kuang', 'kui', 'kun', 'kuo', 'la', 'lai', 'lan', 'lang',
\'lao', 'le', 'lei', 'leng', 'li', 'lia', 'lian', 'liang', 'liao', 'lie',
\'lin', 'ling', 'liu', 'long', 'lou', 'lu', 'luan', 'lue', 'lun', 'luo',
\'lv', 'ma', 'mai', 'man', 'mang', 'mao', 'me', 'mei', 'men', 'meng', 'mi',
\'mian', 'miao', 'mie', 'min', 'ming', 'miu', 'mo', 'mou', 'mu', 'na',
\'nai', 'nan', 'nang', 'nao', 'ne', 'nei', 'nen', 'neng', "'ng", 'ni',
\'nian', 'niang', 'niao', 'nie', 'nin', 'ning', 'niu', 'nong', 'nou', 'nu',
\'nuan', 'nue', 'nuo', 'nv', "'o", "'ou", 'pa', 'pai', 'pan', 'pang',
\'pao', 'pei', 'pen', 'peng', 'pi', 'pian', 'piao', 'pie', 'pin', 'ping',
\'po', 'pou', 'pu', 'qi', 'qia', 'qian', 'qiang', 'qiao', 'qie', 'qin',
\'qing', 'qiong', 'qiu', 'qu', 'quan', 'que', 'qun', 'ran', 'rang', 'rao',
\'re', 'ren', 'reng', 'ri', 'rong', 'rou', 'ru', 'ruan', 'rui', 'run',
\'ruo', 'sa', 'sai', 'san', 'sang', 'sao', 'se', 'sen', 'seng', 'sha',
\'shai', 'shan', 'shang', 'shao', 'she', 'shei', 'shen', 'sheng', 'shi',
\'shou', 'shu', 'shua', 'shuai', 'shuan', 'shuang', 'shui', 'shun', 'shuo',
\'si', 'song', 'sou', 'su', 'suan', 'sui', 'sun', 'suo', 'ta', 'tai',
\'tan', 'tang', 'tao', 'te', 'teng', 'ti', 'tian', 'tiao', 'tie', 'ting',
\'tong', 'tou', 'tu', 'tuan', 'tui', 'tun', 'tuo', "'u", "'v", 'wa', 'wai',
\'wan', 'wang', 'wei', 'wen', 'weng', 'wo', 'wu', 'xi', 'xia', 'xian',
\'xiang', 'xiao', 'xie', 'xin', 'xing', 'xiong', 'xiu', 'xu', 'xuan',
\'xue', 'xun', 'ya', 'yan', 'yang', 'yao', 'ye', 'yi', 'yin', 'ying', 'yo',
\'yong', 'you', 'yu', 'yuan', 'yue', 'yun', 'za', 'zai', 'zan', 'zang',
\'zao', 'ze', 'zei', 'zen', 'zeng', 'zha', 'zhai', 'zhan', 'zhang', 'zhao',
\'zhe', 'zhen', 'zheng', 'zhi', 'zhong', 'zhou', 'zhu', 'zhua', 'zhuai',
\'zhuan', 'zhuang', 'zhui', 'zhun', 'zhuo', 'zi', 'zong', 'zou', 'zu',
\'zuan', 'zui', 'zun', 'zuo']
endfunction

" --------------------------------------------
function! s:vimim_create_shuangpin_table(rule)
" --------------------------------------------
    let pinyin_list = s:vimim_get_pinyin_table()
    let rules = a:rule
    let sptable = {}
    " generate table for shengmu-yunmu pairs match
    for key in pinyin_list
        if key !~ "['a-z]*"
            continue
        endif
        if key[1] == "h"
            let shengmu = key[:1]
            let yunmu = key[2:]
        else
            let shengmu = key[0]
            let yunmu = key[1:]
        endif
        if has_key(rules[0], shengmu)
            let shuangpin_shengmu = rules[0][shengmu]
        else
            continue
        endif
        if has_key(rules[1], yunmu)
            let shuangpin_yunmu = rules[1][yunmu]
        else
            continue
        endif
        let sp1 = shuangpin_shengmu.shuangpin_yunmu
        if !has_key(sptable, sp1)
            if key[0] == "'"
                let key = key[1:]
            end
            let sptable[sp1] = key
        endif
    endfor
    " the jxqy+v special case handling
    if s:vimim_shuangpin == 'abc'
    \|| s:vimim_shuangpin == 'purple'
    \|| s:vimim_shuangpin == 'nature'
    \|| s:vimim_shuangpin == 'flypy'
        let jxqy = {"jv" : "ju", "qv" : "qu", "xv" : "xu", "yv" : "yu"}
        call extend(sptable, jxqy)
    elseif s:vimim_shuangpin == 'ms'
        let jxqy = {"jv" : "jue", "qv" : "que", "xv" : "xue", "yv" : "yue"}
        call extend(sptable, jxqy)
    endif
    " the flypy shuangpin special case handling
    if s:vimim_shuangpin == 'flypy'
        let flypy = {"aa" : "a", "oo" : "o", "ee" : "e",
                    \"an" : "an", "ao" : "ao", "ai" : "ai", "ah": "ang",
                    \"os" : "ong","ou" : "ou",
                    \"en" : "en", "er" : "er", "ei" : "ei", "eg": "eng" }
        call extend(sptable, flypy)
    endif
    " the nature shuangpin special case handling
    if s:vimim_shuangpin == 'nature'
        let nature = {"aa" : "a", "oo" : "o", "ee" : "e" }
        call extend(sptable, nature)
    endif
    " generate table for shengmu-only match
    for [key, value] in items(rules[0])
        if key[0] == "'"
            let sptable[value] = ""
        else
            let sptable[value] = key
        end
    endfor
    return sptable
endfunction

" -----------------------------------
function! s:vimim_shuangpin_generic()
" -----------------------------------
" generate the default value of shuangpin table
    let shengmu_list = {}
    for shengmu in ["b", "p", "m", "f", "d", "t", "l", "n", "g",
                \"k", "h", "j", "q", "x", "r", "z", "c", "s", "y", "w"]
        let shengmu_list[shengmu] = shengmu
    endfor
    let shengmu_list["'"] = "o"
    let yunmu_list = {}
    for yunmu in ["a", "o", "e", "i", "u", "v"]
        let yunmu_list[yunmu] = yunmu
    endfor
    let s:shuangpin_rule = [shengmu_list, yunmu_list]
    return s:shuangpin_rule
endfunction

" -----------------------------------
function! s:vimim_shuangpin_abc(rule)
" -----------------------------------
" [auto cloud test] vim sogou.shuangpin_abc.vimim
" vtpc => shuang pin => double pinyin
    call extend(a:rule[0],{ "zh" : "a", "ch" : "e", "sh" : "v" })
    call extend(a:rule[1],{
        \"an" : "j", "ao" : "k", "ai" : "l", "ang": "h",
        \"ong": "s", "ou" : "b",
        \"en" : "f", "er" : "r", "ei" : "q", "eng": "g", "ng" : "g",
        \"ia" : "d", "iu" : "r", "ie" : "x", "in" : "c", "ing": "y",
        \"iao": "z", "ian": "w", "iang": "t", "iong" : "s",
        \"un" : "n", "ua" : "d", "uo" : "o", "ue" : "m", "ui" : "m",
        \"uai": "c", "uan": "p", "uang": "t" } )
    return a:rule
endfunction

" ----------------------------------
function! s:vimim_shuangpin_ms(rule)
" ----------------------------------
" [auto cloud test] vim sogou.shuangpin_ms.vimim
" vi=>zhi ii=>chi ui=>shi keng=>keneng
    call extend(a:rule[0],{ "zh" : "v", "ch" : "i", "sh" : "u" })
    call extend(a:rule[1],{
        \"an" : "j", "ao" : "k", "ai" : "l", "ang": "h",
        \"ong": "s", "ou" : "b",
        \"en" : "f", "er" : "r", "ei" : "z", "eng": "g", "ng" : "g",
        \"ia" : "w", "iu" : "q", "ie" : "x", "in" : "n", "ing": ";",
        \"iao": "c", "ian": "m", "iang" : "d", "iong" : "s",
        \"un" : "p", "ua" : "w", "uo" : "o", "ue" : "t", "ui" : "v",
        \"uai": "y", "uan": "r", "uang" : "d" ,
        \"v" : "y"} )
    return a:rule
endfunction

" --------------------------------------
function! s:vimim_shuangpin_nature(rule)
" --------------------------------------
" [auto cloud test] vim sogou.shuangpin_nature.vimim
" goal: 'woui' => wo shi => i am
    call extend(a:rule[0],{ "zh" : "v", "ch" : "i", "sh" : "u" })
    call extend(a:rule[1],{
        \"an" : "j", "ao" : "k", "ai" : "l", "ang": "h",
        \"ong": "s", "ou" : "b",
        \"en" : "f", "er" : "r", "ei" : "z", "eng": "g", "ng" : "g",
        \"ia" : "w", "iu" : "q", "ie" : "x", "in" : "n", "ing": "y",
        \"iao": "c", "ian": "m", "iang" : "d", "iong" : "s",
        \"un" : "p", "ua" : "w", "uo" : "o", "ue" : "t", "ui" : "v",
        \"uai": "y", "uan": "r", "uang" : "d" } )
    return a:rule
endfunction

" ----------------------------------------
function! s:vimim_shuangpin_plusplus(rule)
" ----------------------------------------
" [auto cloud test] vim sogou.shuangpin_plusplus.vimim
    call extend(a:rule[0],{ "zh" : "v", "ch" : "u", "sh" : "i" })
    call extend(a:rule[1],{
        \"an" : "f", "ao" : "d", "ai" : "s", "ang": "g",
        \"ong": "y", "ou" : "p",
        \"en" : "r", "er" : "q", "ei" : "w", "eng": "t", "ng" : "t",
        \"ia" : "b", "iu" : "n", "ie" : "m", "in" : "l", "ing": "q",
        \"iao": "k", "ian": "j", "iang" : "h", "iong" : "y",
        \"un" : "z", "ua" : "b", "uo" : "o", "ue" : "x", "ui" : "v",
        \"uai": "x", "uan": "c", "uang" : "h" } )
    return a:rule
endfunction

" --------------------------------------
function! s:vimim_shuangpin_purple(rule)
" --------------------------------------
" [auto cloud test] vim sogou.shuangpin_purple.vimim
    call extend(a:rule[0],{ "zh" : "u", "ch" : "a", "sh" : "i" })
    call extend(a:rule[1],{
        \"an" : "r", "ao" : "q", "ai" : "p", "ang": "s",
        \"ong": "h", "ou" : "z",
        \"en" : "w", "er" : "j", "ei" : "k", "eng": "t", "ng" : "t",
        \"ia" : "x", "iu" : "j", "ie" : "d", "in" : "y", "ing": ";",
        \"iao": "b", "ian": "f", "iang" : "g", "iong" : "h",
        \"un" : "m", "ua" : "x", "uo" : "o", "ue" : "n", "ui" : "n",
        \"uai": "y", "uan": "l", "uang" : "g"} )
    return a:rule
endfunction

" -------------------------------------
function! s:vimim_shuangpin_flypy(rule)
" -------------------------------------
" [auto cloud test] vim sogou.shuangpin_flypy.vimim
    call extend(a:rule[0],{ "zh" : "v", "ch" : "i", "sh" : "u" })
    call extend(a:rule[1],{
        \"an" : "j", "ao" : "c", "ai" : "d", "ang": "h",
        \"ong": "s", "ou" : "z",
        \"en" : "f", "er" : "r", "ei" : "w", "eng": "g", "ng" : "g",
        \"ia" : "x", "iu" : "q", "ie" : "p", "in" : "b", "ing": "k",
        \"iao": "n", "ian": "m", "iang" : "l", "iong" : "s",
        \"un" : "y", "ua" : "x", "uo" : "o", "ue" : "t", "ui" : "v",
        \"uai": "k", "uan": "r", "uang" : "l" } )
    return a:rule
endfunction

" ============================================= }}}
let s:VimIM += [" ====  backend file     ==== {{{"]
" =================================================

" ------------------------------------------------
function! s:vimim_scan_backend_embedded_datafile()
" ------------------------------------------------
    if s:vimim_debug > 1
        return
    endif
    for im in s:all_vimim_input_methods
        let datafile = s:vimim_data_file
        if !empty(datafile) && filereadable(datafile)
            if datafile =~ '\<' . im . '\>'
                call s:vimim_set_datafile(im, datafile)
            endif
        endif
        let datafile = s:path . "vimim." . im . ".txt"
        if filereadable(datafile)
            call s:vimim_set_datafile(im, datafile)
        else
            let datafile = 0
            continue
        endif
    endfor
endfunction

" ------------------------------------------
function! s:vimim_set_datafile(im, datafile)
" ------------------------------------------
    let im = s:vimim_get_valid_im_name(a:im)
    let datafile = a:datafile
    if empty(im)
    \|| empty(datafile)
    \|| !filereadable(datafile)
    \|| isdirectory(datafile)
        return
    endif
    if match(s:clouds, im) > -1
        call s:vimim_set_cloud(im)
    else
        let s:ui.root = "datafile"
        let s:ui.im = im
        let frontends = [s:ui.root, s:ui.im]
        call insert(s:ui.frontends, frontends)
        let s:backend.datafile[im] = s:vimim_one_backend_hash()
        let s:backend.datafile[im].root = "datafile"
        let s:backend.datafile[im].im = im
        let s:backend.datafile[im].name = datafile
        let s:backend.datafile[im].keycode = s:im_keycode[im]
        let s:backend.datafile[im].chinese = s:vimim_chinese(im)
        if empty(s:backend.datafile[im].lines)
            let s:backend.datafile[im].lines = s:vimim_readfile(datafile)
        endif
        call s:vimim_set_special_im_property()
    endif
endfunction

" ----------------------------------------
function! s:vimim_get_from_cache(keyboard)
" ----------------------------------------
    let keyboard = a:keyboard
    if empty(keyboard) || empty(s:backend[s:ui.root][s:ui.im].cache)
        return []
    endif
    let results = []
    if has_key(s:backend[s:ui.root][s:ui.im].cache, keyboard)
        let results = s:backend[s:ui.root][s:ui.im].cache[keyboard]
    endif
    let extras = s:vimim_more_pinyin_datafile(keyboard,0)
    if len(extras) > 0
        call map(results, 'keyboard ." ". v:val')
        call extend(results, extras)
    endif
    return results
endfunction

" --------------------------------------------------------
function! s:vimim_more_pinyin_datafile(keyboard, sentence)
" --------------------------------------------------------
    if s:ui.im =~ 'pinyin' && a:keyboard !~ "[.']"
        " for pinyin with valid keycodes only
    else
        return []
    endif
    let candidates = s:vimim_more_pinyin_candidates(a:keyboard)
    if empty(candidates)
        return []
    endif
    let results = []
    let lines = s:backend[s:ui.root][s:ui.im].lines
    for candidate in candidates
        let pattern = '^' . candidate . '\>'
        let matched = match(lines, pattern, 0)
        if matched < 0
            continue
        elseif a:sentence > 0
            return [candidate]
        endif
        let oneline = get(lines, matched)
        let matched_list = s:vimim_make_pair_matched_list(oneline)
        call extend(results, matched_list)
    endfor
    return results
endfunction

" ----------------------------------------------
function! s:vimim_sentence_match_cache(keyboard)
" ----------------------------------------------
    if empty(s:backend[s:ui.root][s:ui.im].cache)
        return []
    endif
    let keyboard = a:keyboard
    if has_key(s:backend[s:ui.root][s:ui.im].cache, keyboard)
        return keyboard
    elseif empty(s:english_results)
        " scan more on cache when English is not found
    else
        return 0
    endif
    let im = s:ui.im
    let max = len(keyboard)
    let found = 0
    while max > 1
        let max -= 1
        let head = strpart(keyboard, 0, max)
        if has_key(s:backend[s:ui.root][s:ui.im].cache, head)
            let found = 1
            break
        else
            continue
        endif
    endwhile
    if found > 0
        return keyboard[0 : max-1]
    endif
    return 0
endfunction

" -------------------------------------------------
function! s:vimim_sentence_match_datafile(keyboard)
" -------------------------------------------------
    let keyboard = a:keyboard
    let lines = s:backend[s:ui.root][s:ui.im].lines
    if empty(keyboard) || empty(lines)
        return []
    endif
    let pattern = '^' . keyboard . '\s'
    let matched = match(lines, pattern)
    if matched > -1
        return keyboard
    elseif empty(s:english_results)
        " scan more on datafile when English is not found
    else
        return 0
    endif
    let candidates = s:vimim_more_pinyin_datafile(keyboard,1)
    if !empty(candidates)
        return get(candidates,0)
    endif
    " wo'you'yige'meng works in this algorithm
    let max = len(keyboard)
    while max > 1
        let max -= 1
        let head = strpart(keyboard, 0, max)
        let pattern = '^' . head . '\s'
        let matched = match(lines, pattern)
        if matched < 0
            continue
        else
            break
        endif
    endwhile
    if matched < 0
        return 0
    endif
    return keyboard[0 : max-1]
endfunction

" ---------------------------------------------------
function! s:vimim_get_from_datafile(keyboard, search)
" ---------------------------------------------------
    let lines = s:backend[s:ui.root][s:ui.im].lines
    let pattern = '^' . a:keyboard . '\s'
    let matched = match(lines, pattern)
    if matched < 0
        return []
    endif
    let results = []
    let more = s:vimim_more_candidates
    if more > 0
        for i in range(more)
            let matched += i
            let oneline = get(lines, matched)
            let extras = s:vimim_make_pair_matched_list(oneline)
            call extend(results, extras)
        endfor
    else
        let oneline = get(lines, matched)
        let onelines = split(oneline)
        let results = split(oneline)[1:]
        if a:search < 1 && len(onelines) > 0 && len(onelines) < 20
            let extras = s:vimim_more_pinyin_datafile(a:keyboard,0)
            if len(extras) > 0
                let results = s:vimim_make_pair_matched_list(oneline)
                call extend(results, extras)
            endif
        endif
    endif
    return results
endfunction

" -----------------------------------------------
function! s:vimim_make_pair_matched_list(oneline)
" -----------------------------------------------
" break row entry in datafile into pair list
    let oneline_list = split(a:oneline)
    let menu = remove(oneline_list, 0)
    if empty(menu) || menu =~ '\W'
        return []
    endif
    let results = []
    for chinese in oneline_list
        let menu_chinese = menu .' '. chinese
        call add(results, menu_chinese)
    endfor
    return results
endfunction

" ------------------------------------------------
function! s:vimim_more_pinyin_candidates(keyboard)
" ------------------------------------------------
" [purpose] make standard layout for popup menu
" input  =>  mamahuhu
" output =>  mamahuhu, mama, ma
    if empty(s:english_results)
        " break up keyboard only if it is not english
    else
        return []
    endif
    let keyboards = s:vimim_get_pinyin_from_pinyin(a:keyboard)
    if empty(keyboards) || empty(a:keyboard)
        return []
    endif
    let candidates = []
    for i in reverse(range(len(keyboards)-1))
        let candidate = join(keyboards[0 : i], "")
        call add(candidates, candidate)
    endfor
    return candidates
endfunction

" --------------------------------------
function! s:vimim_build_datafile_cache()
" --------------------------------------
    if s:vimim_use_cache < 1
        return
    endif
    if s:backend[s:ui.root][s:ui.im].root == "datafile"
        if empty(s:backend[s:ui.root][s:ui.im].lines)
            " no way to build datafile cache
        elseif empty(s:backend[s:ui.root][s:ui.im].cache)
            let im = s:vimim_chinese(s:ui.im)
            let database = s:vimim_chinese('database')
            let total = len(s:backend[s:ui.root][s:ui.im].lines)
            let progress = "VimIM loading " . im . database
            let progressbar = NewSimpleProgressBar(progress, total)
            try
                call s:vimim_load_datafile_cache(progressbar)
            finally
                call progressbar.restore()
            endtry
        endif
    endif
endfunction

" ------------------------------------------------
function! s:vimim_load_datafile_cache(progressbar)
" ------------------------------------------------
    if empty(s:backend[s:ui.root][s:ui.im].cache)
        " cache only needs to be loaded once
    else
        return
    endif
    for line in s:backend[s:ui.root][s:ui.im].lines
        call a:progressbar.incr(1)
        let oneline_list = split(line)
        let menu = remove(oneline_list, 0)
        let s:backend[s:ui.root][s:ui.im].cache[menu] = oneline_list
    endfor
endfunction

" -------------------------------------
function! s:vimim_scan_current_buffer()
" -------------------------------------
    let buffer = expand("%:p:t")
    if buffer =~ '.vimim\>'
        " start zero configuration showcase
    else
        return
    endif
    if buffer =~ 'dynamic'
        let s:vimim_chinese_input_mode = 'dynamic'
    elseif buffer =~ 'static'
        let s:vimim_chinese_input_mode = 'static'
    endif
    let buffers = split(buffer,'[.]')
    let shuangpin = 'shuangpin_'
    let position = match(buffers, shuangpin)
    if position > -1
        let s:vimim_shuangpin = buffers[position][len(shuangpin) :]
    endif
    for cloud in s:clouds
        if buffer =~# cloud
            return s:vimim_set_cloud(cloud)
        endif
    endfor
    if buffer =~# 'mycloud'
        return s:vimim_set_mycloud(1)
    endif
endfunction

" -------------------------------------------------
function! s:vimim_get_im_from_buffer_name(filename)
" -------------------------------------------------
    let im = 0
    for key in copy(keys(s:im_keycode))
        let pattern = '\<' . key . '\>'
        let matched = match(a:filename, pattern)
        if matched < 0
            continue
        else
            let im = key
            break
        endif
    endfor
    return im
endfunction

" ============================================= }}}
let s:VimIM += [" ====  backend dir      ==== {{{"]
" =================================================

" -------------------------------------------------
function! s:vimim_scan_backend_embedded_directory()
" -------------------------------------------------
    for im in s:all_vimim_input_methods
        let dir = s:vimim_data_directory
        if !empty(dir) && isdirectory(dir)
            let tail = get(split(dir,"/"), -1)
            if tail =~ '\<' . im . '\>'
                call s:vimim_set_directory(im, dir)
            endif
        endif
        let dir = s:path . im
        if isdirectory(dir)
            call s:vimim_set_directory(im, dir)
        else
            let dir = 0
            continue
        endif
    endfor
endfunction

" --------------------------------------
function! s:vimim_set_directory(im, dir)
" --------------------------------------
    let im = s:vimim_get_valid_im_name(a:im)
    let dir = a:dir
    if empty(im) || empty(dir) || !isdirectory(dir)
        return
    endif
    let s:ui.root = "directory"
    let s:ui.im = im
    let frontends = [s:ui.root, s:ui.im]
    call insert(s:ui.frontends, frontends)
    if empty(s:backend.directory)
        let s:backend.directory[im] = s:vimim_one_backend_hash()
        let s:backend.directory[im].root = "directory"
        let s:backend.directory[im].name = s:vimim_data_directory
        let s:backend.directory[im].im = im
        let s:backend.directory[im].keycode = s:im_keycode[im]
        let s:backend.directory[im].chinese = s:vimim_chinese(im)
    endif
    call s:vimim_set_special_im_property()
endfunction

" ----------------------------------------------------
function! s:vimim_more_pinyin_directory(keyboard, dir)
" ----------------------------------------------------
    let candidates = s:vimim_more_pinyin_candidates(a:keyboard)
    if empty(candidates)
        return []
    endif
    let results = []
    for candidate in candidates
        let matches = []
        let filename = a:dir . candidate
        if filereadable(filename)
            let matches = s:vimim_readfile(filename)
        elseif s:has_cjk_file > 0 && s:chinese_input_mode =~ 'onekey'
            let matches = s:vimim_cjk_match(candidate)[0:20]
        endif
        if empty(matches)
            continue
        else
            call map(matches, 'candidate ." ". v:val')
            call extend(results, matches)
        endif
    endfor
    return results
endfunction

" -------------------------------------------------
function! s:vimim_get_from_directory(keyboard, dir)
" -------------------------------------------------
    if empty(a:keyboard) || empty(a:dir)
        return []
    endif
    let results = []
    let filename = a:dir . a:keyboard
    if filereadable(filename)
        let results = s:vimim_readfile(filename)
    endif
    return results
endfunction

" --------------------------------------------------
function! s:vimim_sentence_match_directory(keyboard)
" --------------------------------------------------
    let keyboard = a:keyboard
    let filename = s:vimim_data_directory . keyboard
    if filereadable(filename)
        return keyboard
    elseif empty(s:english_results)
        " scan more on directory when English is not found
    else
        return 0
    endif
    let candidates = s:vimim_more_pinyin_datafile(keyboard,1)
    if !empty(candidates)
        return get(candidates,0)
    endif
    let max = len(keyboard)
    while max > 1
        let max -= 1
        let head = strpart(keyboard, 0, max)
        let filename = s:vimim_data_directory . head
        " workaround: filereadable("/filename.") returns true
        if filereadable(filename)
            if head[-1:-1] != "."
                break
            endif
        else
            continue
        endif
    endwhile
    if filereadable(filename)
        return keyboard[0 : max-1]
    endif
    return 0
endfunction

" ============================================= }}}
let s:VimIM += [" ====  backend clouds   ==== {{{"]
" =================================================

" ------------------------------------
function! s:vimim_scan_backend_cloud()
" ------------------------------------
    if empty(s:backend.datafile) && empty(s:backend.directory)
        call s:vimim_set_mycloud(0)
        if empty(s:mycloud_plugin)
            call s:vimim_set_cloud(s:cloud_default)
        endif
    endif
    if empty(eval("s:vimim_cloud_" . s:cloud_default))
        exe 'let s:vimim_cloud_' . s:cloud_default . ' = 888'
    endif
endfunction

" -----------------------------
function! s:vimim_set_cloud(im)
" -----------------------------
    let im = a:im
    exe 'let s:vimim_cloud_' . im . ' = 1'
    let cloud = s:vimim_set_cloud_if_www_executable(im)
    if empty(cloud)
        exe 'let s:vimim_cloud_' . im . ' = 0'
        let s:backend.cloud = {}
    else
        let s:mycloud_plugin = 0
        let s:cloud_default = im
        let s:ui.root = 'cloud'
        let s:ui.im = im
        let frontends = [s:ui.root, s:ui.im]
        call insert(s:ui.frontends, frontends)
    endif
endfunction

" -----------------------------------------------
function! s:vimim_set_cloud_if_www_executable(im)
" -----------------------------------------------
    let im = a:im
    let s:backend.cloud[im] = s:vimim_one_backend_hash()
    let cloud = s:vimim_check_http_executable(im)
    if empty(cloud)
        return 0
    else
        let s:backend.cloud[im].root = 'cloud'
        let s:backend.cloud[im].im = im
        let s:backend.cloud[im].keycode = s:im_keycode[im]
        let s:backend.cloud[im].chinese = s:vimim_chinese(im)
        let s:backend.cloud[im].name = s:vimim_chinese(im)
        return cloud
    endif
endfunction

" -----------------------------------------
function! s:vimim_check_http_executable(im)
" -----------------------------------------
    if eval("s:vimim_cloud_" . a:im) < 0
        return {}
    endif
    " step 1 of 3: try to find libvimim
    let cloud = s:vimim_get_libvimim()
    if !empty(cloud) && filereadable(cloud)
        " in win32, strip the .dll suffix
        if has("win32") && cloud[-4:] ==? ".dll"
            let cloud = cloud[:-5]
        endif
        let ret = libcall(cloud, "do_geturl", "__isvalid")
        if ret ==# "True"
            let s:www_executable = cloud
            let s:www_libcall = 1
        endif
    endif
    " step 2 of 3: try to find wget
    if empty(s:www_executable)
        let wget = 0
        let wget_exe = s:path . "wget.exe"
        if executable(wget_exe)
            let wget = wget_exe
        elseif executable('wget')
            let wget = "wget"
        endif
        if !empty(wget)
            let wget_option = " -qO - --timeout 20 -t 10 "
            let s:www_executable = wget . wget_option
        endif
    endif
    " step 3 of 3: try to find curl if no wget
    if empty(s:www_executable)
        if executable('curl')
            let s:www_executable = "curl -s "
        else
            return {}
        endif
    endif
    return s:backend.cloud[a:im]
endfunction

" ------------------------------------
function! s:vimim_magic_tail(keyboard)
" ------------------------------------
    let keyboard = a:keyboard
    if keyboard =~ '\d' || s:chinese_input_mode !~ 'onekey'
        return []
    endif
    let magic_tail = keyboard[-1:-1]
    let last_but_one = keyboard[-2:-2]
    if magic_tail =~ "[.']" && last_but_one =~ "[0-9a-z]"
        " play with magic trailing char
    else
        return []
    endif
    let keyboards = []
    " <dot> triple play in OneKey:
    "   (1) magic trailing dot => forced-non-cloud in cloud
    "   (2) magic trailing dot => forced-cjk-match
    "   (3) as word partition  => match dot by dot
    if magic_tail ==# "."
        " trailing dot => forced-non-cloud
        let s:has_no_internet = 2
        call add(keyboards, -1)
    elseif magic_tail ==# "'"
        " trailing apostrophe => forced-cloud
        let s:has_no_internet -= 2
        let cloud = s:vimim_set_cloud_if_www_executable(s:cloud_default)
        if empty(cloud)
            return []
        endif
        call add(keyboards, 1)
    endif
    " <apostrophe> double play in OneKey:
    "   (1) magic trailing apostrophe => cloud at will
    "   (2) as word partition  => match apostrophe by apostrophe
    let keyboard = keyboard[:-2]
    call insert(keyboards, keyboard)
    return keyboards
endfunction

" -------------------------------------------------
function! s:vimim_to_cloud_or_not(keyboard, clouds)
" -------------------------------------------------
    let keyboard = a:keyboard
    let do_cloud = get(a:clouds, 1)
    if do_cloud > 0
        return 1
    endif
    if keyboard =~ "[^a-z]" || s:has_no_internet > 1
        return 0
    endif
    if s:chinese_input_mode =~ 'onekey' && s:has_cjk_file > 1
        return 0
    endif
    if match(s:clouds, s:ui.im) > -1
       if eval("s:vimim_cloud_" . s:ui.im) > 0
           return 1
       endif
    endif
    if s:has_no_internet < 0 || get(a:clouds, 1) > 0
        return 1
    endif
    if s:chinese_input_mode !~ 'dynamic'
    \&& s:ui.im == 'pinyin'
    \&& s:cloud_default == 'sogou'
        " threshold to trigger cloud automatically
        let pinyins = s:vimim_get_pinyin_from_pinyin(keyboard)
        if len(pinyins) > s:vimim_cloud_sogou
            return 1
        endif
    endif
    return 0
endfunction

" ---------------------------------------
function! s:vimim_get_cloud(im, keyboard)
" ---------------------------------------
    if a:keyboard !~# s:valid_key
    \|| empty(s:www_executable)
    \|| eval("s:vimim_cloud_" . a:im) < 1
        return []
    endif
    let results = []
    let cloud  = "s:vimim_get_cloud_" . a:im . "(a:keyboard)"
    try
        let results = eval(cloud)
    catch
        call s:debugs('get_cloud::'.a:im.'::', v:exception)
    endtry
    return results
endfunction

" ------------------------------------
function! s:vimim_get_from_http(input)
" ------------------------------------
    let input = a:input
    let output = 0
    try
        if s:www_libcall > 0
            let output = libcall(s:www_executable, "do_geturl", input)
        else
            let input = '"' . input . '"'
            let output = system(s:www_executable . input)
        endif
    catch
        let output = 0
        call s:debugs('sogou::', output ." ". v:exception)
    endtry
    return output
endfunction

" -----------------------------------------
function! s:vimim_get_cloud_sogou(keyboard)
" -----------------------------------------
    " http://pinyin.sogou.com/cloud/
    if empty(s:cloud_sogou_key)
        let sogou_key = 'http://web.pinyin.sogou.com/web_ime/patch.php'
        let output = s:vimim_get_from_http(sogou_key)
        if empty(output)
            return []
        endif
        let s:cloud_sogou_key = get(split(output, '"'), 1)
    endif
    let cloud = 'http://web.pinyin.sogou.com/api/py?key='
    let cloud = cloud . s:cloud_sogou_key .'&query='
    let input = cloud . a:keyboard
    let output = s:vimim_get_from_http(input)
    " http://web.pinyin.sogou.com/web_ime/get_ajax/woyouyigemeng.key
    if empty(output) || output =~ '502 Bad Gateway'
        return []
    endif
    let first  = match(output, '"', 0)
    let second = match(output, '"', 0, 2)
    if first > 0 && second > 0
        let output = strpart(output, first+1, second-first-1)
        let output = s:vimim_url_xx_to_chinese(output)
    endif
    if empty(output)
        return []
    elseif !empty(s:localization)
        " support gb and big5 in addition to utf8
        let output = s:vimim_i18n_read(output)
    endif
    " in  => '我有一个梦 : 13    +  s:colon=uff1a not ufe30
    " out => ['woyouyigemeng 我有一个梦']
    let matched_list = []
    for item in split(output, '\t+')
        let item_list = split(item, s:colon)
        if len(item_list) > 1
            let chinese = get(item_list,0)
            let english = strpart(a:keyboard, 0, get(item_list,1))
            let new_item = english . " " . chinese
            call add(matched_list, new_item)
        endif
    endfor
    return matched_list
endfunction

" --------------------------------------
function! s:vimim_get_cloud_qq(keyboard)
" --------------------------------------
    " http://py.qq.com/web
    let results = ['qq QQ输入法']
    return results
endfunction

" ------------------------------------------
function! s:vimim_get_cloud_google(keyboard)
" ------------------------------------------
    " http://www.google.com/transliterate
    let results = ['google 谷歌输入法']
    return results
endfunction

" -----------------------------------------
function! s:vimim_get_cloud_baidu(keyboard)
" -----------------------------------------
    " http://www.baidu.com
    let results = ['baidu 百度输入法']
    return results
endfunction

" ============================================= }}}
let s:VimIM += [" ====  backend mycloud  ==== {{{"]
" =================================================
" Thanks to Pan Shizhu for providing all mycloud codes:

" --------------------------------
function! s:vimim_set_mycloud(url)
" --------------------------------
    if !empty(a:url)
        " [auto mycloud test] vim mycloud.vimim
        let s:vimim_cloud_mycloud = "http://pim-cloud.appspot.com/qp/"
    endif
    let cloud = s:vimim_set_cloud_if_www_executable('mycloud')
    if !empty(cloud)
        let mycloud = s:vimim_check_mycloud_availability()
        if empty(mycloud)
            let s:backend.cloud = {}
            return {}
        else
            let s:vimim_shuangpin = 0
            let s:vimim_cloud_sogou = -777
            let s:mycloud_plugin = mycloud
            let s:ui.root = 'cloud'
            let s:ui.im = 'mycloud'
            let frontends = [s:ui.root, s:ui.im]
            let s:ui.frontends = [frontends]
        endif
    endif
endfunction

" --------------------------------------------
function! s:vimim_check_mycloud_availability()
" --------------------------------------------
    let cloud = 0
    if empty(s:vimim_cloud_mycloud)
        let cloud = s:vimim_check_mycloud_plugin_libcall()
    else
        let cloud = s:vimim_check_mycloud_plugin_url()
    endif
    if empty(cloud)
        let s:mycloud_plugin = 0
        return 0
    endif
    let ret = s:vimim_access_mycloud(cloud, "__getname")
    let directory = split(ret, "\t")[0]
    let ret = s:vimim_access_mycloud(cloud, "__getkeychars")
    let keycode = split(ret, "\t")[0]
    if empty(keycode)
        let s:mycloud_plugin = 0
        return 0
    else
        let s:backend.cloud.mycloud.directory = directory
        let s:backend.cloud.mycloud.keycode = s:im_keycode["mycloud"]
        return cloud
    endif
endfunction

" ------------------------------------------
function! s:vimim_access_mycloud(cloud, cmd)
" ------------------------------------------
"  use the same function to access mycloud by libcall() or system()
    let executable = s:www_executable
    if s:cloud_plugin_mode == "libcall"
        let arg = s:cloud_plugin_arg
        if empty(arg)
            return libcall(a:cloud, s:cloud_plugin_func, a:cmd)
        else
            return libcall(a:cloud, s:cloud_plugin_func, arg." ".a:cmd)
        endif
    elseif s:cloud_plugin_mode == "system"
        return system(a:cloud." ".shellescape(a:cmd))
    elseif s:cloud_plugin_mode == "www"
        let input = s:vimim_rot13(a:cmd)
        if s:www_libcall
            let ret = libcall(executable, "do_geturl", a:cloud.input)
        else
            let ret = system(executable . shellescape(a:cloud.input))
        endif
        let output = s:vimim_rot13(ret)
        let ret = s:vimim_url_xx_to_chinese(output)
        return ret
    endif
    return ""
endfunction

" -------------------------------
function! s:vimim_rot13(keyboard)
" -------------------------------
    let rot13 = a:keyboard
    let a = "12345abcdefghijklmABCDEFGHIJKLM"
    let z = "98760nopqrstuvwxyzNOPQRSTUVWXYZ"
    let rot13 = tr(rot13, a.z, z.a)
    return rot13
endfunction

" ------------------------------
function! s:vimim_get_libvimim()
" ------------------------------
    let cloud = ""
    if has("win32") || has("win32unix")
        let cloud = "libvimim.dll"
    elseif has("unix")
        let cloud = "libvimim.so"
    else
        return ""
    endif
    let cloud = s:path . cloud
    if filereadable(cloud)
        return cloud
    endif
    return ""
endfunction

" ----------------------------------------------
function! s:vimim_check_mycloud_plugin_libcall()
" ----------------------------------------------
    " we do plug-n-play for libcall(), not for system()
    let cloud = s:vimim_get_libvimim()
    if !empty(cloud)
        let s:cloud_plugin_mode = "libcall"
        let s:cloud_plugin_arg = ""
        let s:cloud_plugin_func = 'do_getlocal'
        if filereadable(cloud)
            if has("win32")
                " we don't need to strip ".dll" for "win32unix".
                let cloud = cloud[:-5]
            endif
            try
                let ret = s:vimim_access_mycloud(cloud, "__isvalid")
                if split(ret, "\t")[0] == "True"
                    return cloud
                endif
            catch
                call s:debugs('libcall_mycloud2::error=',v:exception)
            endtry
        endif
    endif
    " libcall check failed, we now check system()
    if has("gui_win32")
        return 0
    endif
    " on linux, we do plug-n-play
    let cloud = s:path . "mycloud/mycloud"
    if !executable(cloud)
        if !executable("python")
            return 0
        endif
        let cloud = "python " . cloud
    endif
    " in POSIX system, we can use system() for mycloud
    let s:cloud_plugin_mode = "system"
    let ret = s:vimim_access_mycloud(cloud, "__isvalid")
    if split(ret, "\t")[0] == "True"
        return cloud
    endif
    return 0
endfunction

" ------------------------------------------
function! s:vimim_check_mycloud_plugin_url()
" ------------------------------------------
    " we do set-and-play on all systems
    let part = split(s:vimim_cloud_mycloud, ':')
    let lenpart = len(part)
    if lenpart <= 1
        call s:debugs("invalid_cloud_plugin_url::","")
    elseif part[0] ==# 'app'
        if !has("gui_win32")
            " strip the first root if contains ":"
            if lenpart == 3
                if part[1][0] == '/'
                    let cloud = part[1][1:] . ':' .  part[2]
                else
                    let cloud = part[1] . ':' . part[2]
                endif
            elseif lenpart == 2
                let cloud = part[1]
            endif
            " in POSIX system, we can use system() for mycloud
            if executable(split(cloud, " ")[0])
                let s:cloud_plugin_mode = "system"
                let ret = s:vimim_access_mycloud(cloud, "__isvalid")
                if split(ret, "\t")[0] == "True"
                    return cloud
                endif
            endif
        endif
    elseif part[0] ==# "dll"
        if len(part[1]) == 1
            let base = 1
        else
            let base = 0
        endif
        " provide function name
        if lenpart >= base+4
            let s:cloud_plugin_func = part[base+3]
        else
            let s:cloud_plugin_func = 'do_getlocal'
        endif
        " provide argument
        if lenpart >= base+3
            let s:cloud_plugin_arg = part[base+2]
        else
            let s:cloud_plugin_arg = ""
        endif
        " provide the dll
        if base == 1
            let cloud = part[1] . ':' . part[2]
        else
            let cloud = part[1]
        endif
        if filereadable(cloud)
            let s:cloud_plugin_mode = "libcall"
            " strip off the .dll suffix, only required for win32
            if has("win32") && cloud[-4:] ==? ".dll"
                let cloud = cloud[:-5]
            endif
            try
                let ret = s:vimim_access_mycloud(cloud, "__isvalid")
                if split(ret, "\t")[0] == "True"
                    return cloud
                endif
            catch
                call s:debugs('libcall_mycloud1::', v:exception)
            endtry
        endif
    elseif part[0] ==# "http" || part[0] ==# "https"
        let cloud = s:vimim_cloud_mycloud
        if !empty(s:www_executable)
            let s:cloud_plugin_mode = "www"
            let ret = s:vimim_access_mycloud(cloud, "__isvalid")
            if split(ret, "\t")[0] == "True"
                return cloud
            endif
        endif
    else
        call s:debugs("invalid_cloud_plugin_url::","")
    endif
    return 0
endfunction

" --------------------------------------------
function! s:vimim_get_mycloud_plugin(keyboard)
" --------------------------------------------
    if empty(s:mycloud_plugin)
        return []
    endif
    let cloud = s:mycloud_plugin
    let output = 0
    try
        let output = s:vimim_access_mycloud(cloud, a:keyboard)
    catch
        let output = 0
        call s:debugs('mycloud::',v:exception)
    endtry
    if empty(output)
        return []
    endif
    let menu = []
    " one line typical output:  春梦 8 4420
    for item in split(output, '\n')
        let item_list = split(item, '\t')
        let chinese = get(item_list,0)
        if s:localization > 0
            let chinese = s:vimim_i18n_read(chinese)
        endif
        if empty(chinese) || get(item_list,1,-1)<0
            " bypass the debug line which have -1
            continue
        endif
        let extra_text = get(item_list,2)
        let english = a:keyboard[get(item_list,1):]
        let new_item = extra_text . " " . chinese . english
        call add(menu, new_item)
    endfor
    return menu
endfunction

" -------------------------------------
function! s:vimim_url_xx_to_chinese(xx)
" -------------------------------------
    let output = a:xx
    if s:www_libcall > 0
        let output = libcall(s:www_executable, "do_unquote", a:xx)
    else
        let output = substitute(a:xx, '%\(\x\x\)',
            \ '\=eval(''"\x''.submatch(1).''"'')','g')
    endif
    return output
endfunction

" ============================================= }}}
let s:VimIM += [" ====  core workflow    ==== {{{"]
" =================================================

" --------------------------------------
function! s:vimim_initialize_i_setting()
" --------------------------------------
    let s:saved_cpo         = &cpo
    let s:saved_omnifunc    = &omnifunc
    let s:saved_completeopt = &completeopt
    let s:saved_laststatus  = &laststatus
    let s:saved_statusline  = &statusline
    let s:saved_lazyredraw  = &lazyredraw
    let s:saved_showmatch   = &showmatch
    let s:saved_smartcase   = &smartcase
    let s:saved_pumheights  = [&pumheight,0]
endfunction

" ------------------------------
function! s:vimim_i_setting_on()
" ------------------------------
    set omnifunc=VimIM
    set completeopt=menuone
    set nolazyredraw
    set noshowmatch
    set smartcase
    if empty(&pumheight)
        let &pumheight=9
        let s:saved_pumheights[1]=&pumheight
    endif
endfunction

" -------------------------------
function! s:vimim_i_setting_off()
" -------------------------------
    let &cpo         = s:saved_cpo
    let &omnifunc    = s:saved_omnifunc
    let &completeopt = s:saved_completeopt
    let &laststatus  = s:saved_laststatus
    let &statusline  = s:saved_statusline
    let &lazyredraw  = s:saved_lazyredraw
    let &showmatch   = s:saved_showmatch
    let &smartcase   = s:saved_smartcase
    let &pumheight   = get(s:saved_pumheights,0)
endfunction

" -----------------------
function! s:vimim_start()
" -----------------------
    sil!call s:vimim_plugins_fix_start()
    sil!call s:vimim_i_setting_on()
    sil!call s:vimim_super_reset()
    sil!call s:vimim_label_on()
    sil!call s:vimim_helper_mapping_on()
    set noruler
    highlight! link Cursor vimim_cursor_color
endfunction

" ----------------------
function! g:vimim_stop()
" ----------------------
    sil!call s:vimim_i_setting_off()
    sil!call s:vimim_super_reset()
    sil!call s:vimim_i_map_off()
    sil!call s:vimim_plugins_fix_stop()
    sil!call s:vimim_initialize_mapping()
    set ruler
    highlight! link Cursor NONE
endfunction

" -----------------------------
function! s:vimim_super_reset()
" -----------------------------
    sil!call s:vimim_reset_before_anything()
    sil!call s:vimim_reset_before_omni()
    sil!call g:vimim_reset_after_insert()
endfunction

" ---------------------------------------
function! s:vimim_reset_before_anything()
" ---------------------------------------
    let s:has_gnuplot = 0
    let s:has_pumvisible = 0
    let s:popupmenu_list = []
    let s:keyboard_list  = []
endfunction

" -----------------------------------
function! s:vimim_reset_before_omni()
" -----------------------------------
    let s:smart_enter = 0
    let s:show_me_not = 0
    let s:english_results = []
endfunction

" ------------------------------------
function! g:vimim_reset_after_insert()
" ------------------------------------
    let s:hjkl_h = 0
    let s:hjkl_l = 0
    let s:hjkl_m = 0
    let s:hjkl_n = 0
    let s:hjkl_s = 0
    let s:hjkl_x = ""
    let s:pageup_pagedown = 0
    let s:has_no_internet = 0
    let s:matched_list = []
    let &pumheight = s:saved_pumheights[1]
    return ""
endfunction

" -----------------
function! g:vimim()
" -----------------
    let key = ""
    let s:keyboard_list = []
    let one_before = getline(".")[col(".")-2]
    if one_before =~ s:valid_key || s:has_gnuplot > 0
        let key = '\<C-X>\<C-O>\<C-R>=g:vimim_menu_select()\<CR>'
    elseif s:vimim_onekey_hit_and_run > 0
    \&& s:chinese_input_mode =~ 'onekey'
        call g:vimim_stop()
    else
        let s:has_pumvisible = 0
    endif
    sil!exe 'sil!return "' . key . '"'
endfunction

" -----------------------------
function! g:vimim_menu_select()
" -----------------------------
    let key = ""
    if pumvisible()
        let key = '\<C-P>\<Down>'
        if s:has_gnuplot > 0
            let n = s:hjkl_n % 2 ? len(s:matched_list)-3 : 2
            let key .= repeat("\<Down>", n)
        endif
    endif
    sil!exe 'sil!return "' . key . '"'
endfunction

" ---------------------------
function! s:vimim_i_map_off()
" ---------------------------
    let unmap_list = range(0,9)
    call extend(unmap_list, s:AZ_list)
    call extend(unmap_list, s:valid_keys)
    call extend(unmap_list, keys(s:evils))
    call extend(unmap_list, keys(s:punctuations))
    call extend(unmap_list, ['<Esc>','<CR>','<BS>','<Space>'])
    for _ in unmap_list
        sil!exe 'iunmap '. _
    endfor
endfunction

" ============================================= }}}
let s:VimIM += [" ====  core engine      ==== {{{"]
" =================================================

" ---------------------------------------------------------
function! s:vimim_embedded_backend_engine(keyboard, search)
" ---------------------------------------------------------
    let keyboard = a:keyboard
    let im = s:ui.im
    let root = s:ui.root
    if empty(im)
    \|| empty(root)
    \|| im =~ 'cloud'
    \|| s:show_me_not > 0
    \|| keyboard !~# s:valid_key
        return []
    endif
    if im == 'pinyin'
        let keyboard = s:vimim_toggle_pinyin(keyboard)
    endif
    if s:ui.has_dot == 2
        let keyboard = s:vimim_add_apostrophe(keyboard)
    endif
    let results = []
    let keyboard2 = 0
    if root =~# "directory"
        let dir = s:backend[root][im].name
        let keyboard2 = s:vimim_sentence_match_directory(keyboard)
        let results = s:vimim_get_from_directory(keyboard2, dir)
        if keyboard ==# keyboard2 && a:search < 1
        \&& len(results) > 0 && len(results) < 20
            let extras = s:vimim_more_pinyin_directory(keyboard, dir)
            if len(extras) > 0 && len(results) > 0
                call map(results, 'keyboard ." ". v:val')
                call extend(results, extras)
            endif
        endif
    elseif root =~# "datafile"
        if empty(s:backend[root][im].cache)
            let keyboard2 = s:vimim_sentence_match_datafile(keyboard)
            let results = s:vimim_get_from_datafile(keyboard2, a:search)
        else
            let keyboard2 = s:vimim_sentence_match_cache(keyboard)
            let results = s:vimim_get_from_cache(keyboard2)
        endif
    endif
    if len(s:keyboard_list) < 2
        if empty(keyboard2)
            let s:keyboard_list = [keyboard]
        elseif len(keyboard2) < len(keyboard)
            let tail = strpart(keyboard,len(keyboard2))
            let s:keyboard_list = [keyboard2, tail]
        endif
    endif
    return results
endfunction

" ------------------------------
function! VimIM(start, keyboard)
" ------------------------------
if a:start

    if s:has_gnuplot > 0
        return 0
    endif

    let current_positions = getpos(".")
    let start_row = current_positions[1]
    let start_column = current_positions[2]-1
    let current_line = getline(start_row)
    let one_before = current_line[start_column-1]

    " take care of seamless English/Chinese input
    let seamless_column = s:vimim_get_seamless(current_positions)
    if seamless_column >= 0
        let len = current_positions[2]-1 - seamless_column
        let keyboard = strpart(current_line, seamless_column, len)
        call s:vimim_set_keyboard_list(seamless_column, keyboard)
        return seamless_column
    endif

    let last_seen_nonsense_column = copy(start_column)
    let last_seen_backslash_column = copy(start_column)
    let all_digit = 1
    let nonsense_pattern = "[0-9.']"
    while start_column > 0
        if one_before =~# s:valid_key
            let start_column -= 1
            if one_before !~# nonsense_pattern && s:ui.has_dot < 1
                let last_seen_nonsense_column = start_column
                if all_digit > 0
                    let all_digit = 0
                endif
            endif
        elseif one_before=='\' && s:vimim_backslash_close_pinyin>0
            " do nothing for pinyin with leading backslash
            return last_seen_backslash_column
        else
            break
        endif
        let one_before = current_line[start_column-1]
    endwhile
    if all_digit < 1
        let start_column = last_seen_nonsense_column
    endif

    let s:start_row_before = start_row
    let s:current_positions = current_positions
    let len = current_positions[2]-1 - start_column
    let keyboard = strpart(current_line, start_column, len)
    call s:vimim_set_keyboard_list(start_column, keyboard)

    return start_column

else

    " [cache] less is more
    let results = s:vimim_cache()
    if !empty(results)
        return s:vimim_popupmenu_list(results)
    endif

    " [initialization] early start, half done
    let keyboard = a:keyboard
    call s:vimim_reset_before_omni()

    " [gnuplot] show the state-of-the-art ascii picture
    if s:has_gnuplot > 0
        let results = s:vimim_gnuplot(keyboard)
        if !empty(results)
            return s:vimim_popupmenu_list(results)
        endif
    endif

    " [validation] user keyboard input validation
    if empty(str2nr(keyboard))
        " keyboard input is alphabet only
    elseif len(s:keyboard_list) > 0
        let keyboard = get(s:keyboard_list,0)
    endif
    if empty(keyboard) || keyboard !~# s:valid_key
        return
    endif

    " [onekey] play with nothing but OneKey
    if s:chinese_input_mode =~ 'onekey'
        let results = s:vimim_onekey_input(keyboard)
        if !empty(len(results)) && empty(s:english_results)
            return s:vimim_popupmenu_list(results)
        endif
    endif

    " [mycloud] get chunmeng from mycloud local or www
    if !empty(s:mycloud_plugin)
        let results = s:vimim_get_mycloud_plugin(keyboard)
        if !empty(len(results))
            return s:vimim_popupmenu_list(results)
        endif
    endif

    " [cloud] magic trailing apostrophe to control cloud
    let clouds = s:vimim_magic_tail(keyboard)
    if !empty(len(clouds))
        let keyboard = get(clouds, 0)
    endif

    " [shuangpin] support 6 major shuangpin
    if !empty(s:vimim_shuangpin) && s:has_pumvisible < 1
        let keyboard = s:vimim_shuangpin_transform(keyboard)
        let s:keyboard_list = [keyboard]
    endif

    " [sogou] to make cloud come true for woyouyigemeng
    let force = s:vimim_to_cloud_or_not(keyboard, clouds)
    if force > 0
        let cloud = match(s:clouds,s:ui.im)<0 ? s:cloud_default : s:ui.im
        let results = s:vimim_get_cloud(cloud, keyboard)
        if empty(len(results))
            if s:vimim_cloud_sogou > 2
                let s:has_no_internet += 1
            endif
        else
            let s:has_no_internet = -1
            let s:keyboard_list = [keyboard]
            return s:vimim_popupmenu_list(results)
        endif
    endif

    " [wubi] support auto insert for every 4 input
    if s:ui.im == 'wubi' || s:ui.im == 'erbi'
        let keyboard = s:vimim_wubi_4char_auto_input(keyboard)
        if s:ui.im =~ 'erbi' && len(keyboard) == 1
        \&& keyboard =~ "[.,/;]" && has_key(s:punctuations, keyboard)
            return [s:punctuations[keyboard]]
        endif
    endif

    " [backend] plug-n-play embedded backend engine
    let results = s:vimim_embedded_backend_engine(keyboard,0)
    if empty(s:english_results)
        " no english is found for the keyboard input
    else
        call extend(results, s:english_results, 0)
    endif
    if !empty(results)
        return s:vimim_popupmenu_list(results)
    endif

    " [just_do_it] last try on both cjk and cloud before giving up
    if s:has_cjk_file > 0 && s:chinese_input_mode =~ 'onekey'
        let keyboard_head = s:vimim_cjk_sentence_match(keyboard.".")
        if !empty(keyboard_head)
            let results = s:vimim_cjk_match(keyboard_head)
        endif
    elseif keyboard !~# '\L'
        let cloud = match(s:clouds,s:ui.im)<0 ? s:cloud_default : s:ui.im
        let results = s:vimim_get_cloud(cloud, keyboard)
    endif
    if !empty(len(results))
        return s:vimim_popupmenu_list(results)
    elseif s:chinese_input_mode =~ 'onekey'
        call s:vimim_super_reset()
    endif

return
endif
endfunction

" ============================================= }}}
let s:VimIM += [" ====  core driver      ==== {{{"]
" =================================================

" -----------------------------------
function! s:vimim_helper_mapping_on()
" -----------------------------------
    inoremap <expr> <CR>    <SID>vimim_enter()
    inoremap <expr> <BS>    <SID>vimim_backspace()
    inoremap <expr> <Esc>   <SID>vimim_esc()
    inoremap <expr> <Space> <SID>vimim_space()
endfunction

" ------------------------------------
function! s:vimim_initialize_mapping()
" ------------------------------------
    sil!call s:vimim_chinesemode_mapping()
    sil!call s:vimim_onekey_mapping()
endfunction

" -------------------------------------
function! s:vimim_chinesemode_mapping()
" -------------------------------------
    if s:vimim_debug < 2
        inoremap<unique><expr> <Plug>VimIM  <SID>ChineseMode()
         noremap<silent>       <C-Bslash>   :call <SID>ChineseMode()<CR>
            imap<silent>       <C-Bslash>   <Plug>VimIM
        if s:vimim_ctrl_h_to_switch > 0
            inoremap<silent><expr> <C-H> <SID>VimIMSwitch()
        else
            inoremap<silent><expr> <C-X><C-Bslash> <SID>VimIMSwitch()
        endif
    endif
    if s:vimim_ctrl_space_to_toggle == 1
        if has("gui_running")
             map <C-Space> <C-Bslash>
            imap <C-Space> <C-Bslash>
        elseif has("win32unix")
             map <C-@> <C-Bslash>
            imap <C-@> <C-Bslash>
        endif
    endif
endfunction

" --------------------------------
function! s:vimim_onekey_mapping()
" --------------------------------
    inoremap<unique><expr> <Plug>VimimOneKey <SID>OneKey()
        imap<silent> <C-^> <Plug>VimimOneKey
    xnoremap<silent> <C-^> y:call <SID>vimim_visual_ctrl6()<CR>
    if s:vimim_onekey_is_tab > 0
            imap<silent> <Tab> <Plug>VimimOneKey
        xnoremap<silent> <Tab> y:call <SID>vimim_visual_ctrl6()<CR>
    endif
    if s:vimim_search_next > 0
        noremap <silent> n :call g:vimim_search_next()<CR>n
    endif
    :com! -range=% VimIM <line1>,<line2>call s:vimim_chinese_transfer()
    :com! -range=% VimiM <line1>,<line2>call s:vimim_chinese_rotation()
endfunction

" ------------------------------------
function! s:vimim_initialize_autocmd()
" ------------------------------------
    if has("autocmd")
        augroup vimim_auto_chinese_mode
            autocmd BufNewFile *.vimim startinsert
            autocmd BufEnter   *.vimim sil!call <SID>ChineseMode()
        augroup END
    endif
endfunction

sil!call s:vimim_initialize_debug()
sil!call s:vimim_initialize_global()
sil!call s:vimim_for_mom_and_dad()
sil!call s:vimim_initialize_mapping()
sil!call s:vimim_initialize_autocmd()
" ======================================= }}}
