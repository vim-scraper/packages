﻿" =================================================
"             "VimIM —— Vim 中文输入法"
" -------------------------------------------------
" VimIM -- Input Method by Vim, of Vim, for Vimmers
" =================================================

" == "The VimIM Introduction" == {{{
" ==================================
"      File: vimim.vim
"    Author: Sean Ma
"    Latest: 20090228T131200
"   License: GNU Lesser General Public License
"       URL: http://vim.sourceforge.net/scripts/script.php?script_id=2506
" -----------------------------------------------------------
"  Describe: The VimIM is a Vim plugin designed as an independent
"            IM (Input Method) to support the input of any language.
"            It can be used as yet another way to input non-ascii
"            using CTRL-^ in addition to builtin CTRL-V and CTRL-K.
"            The VimIM aims to complete the Vim as an editor.
" -----------------------------------------------------------
"  Features: # "Plug & Play"
"            # CJK can be input as long as it can be read using Vim.
"            # It is independent of the Operating System.
"            # It is independent of Vim mbyte-XIM/mbyte-IME API.
"            # The Input Methods can be freely defined without limit.
"            # The "VimIM One Key" speeds up ad hoc Chinese input.
"            # The "VimIM Chinese Input Mode" makes CJK input pleasure.
"            # "Dynamically adjust menu order" on past usage frequency
"            # Support "all input methods": PinYin, Wubi, Cangjie, etc
"            # Support all-power "fuzzy search" and "wildcards search"
"            # Support "pinyin fuzzy phonetic"
"            # Support "Quick English Input"
"            # Support "Direct Unicode Input"
"            # Support input of traditional Chinese (Big5 or GBK set)
"            # Intelligent error correction for commonly misspelled words
"            # The datafile format is open, simple and flexible.
" -----------------------------------------------------------
" EasterEgg: (1) (Neither data file nor configuration)
"            (2) (in Vim Command-line Mode, type:)  :source %<CR>
"            (3) (in Vim Insert Mode,       type:)  vim<C-X><C-U>
" -----------------------------------------------------------
"   Install: (1) download any data file you like from the link below
"            (2) drop this file and the datafile to the plugin directory
" -----------------------------------------------------------
" Data File: http://maxiangjiang.googlepages.com/vimim.pinyin.txt
"            http://maxiangjiang.googlepages.com/vimim.wubi.txt
"            http://maxiangjiang.googlepages.com/vimim.cangjie.txt
"            http://maxiangjiang.googlepages.com/vimim.quick.txt
"            http://maxiangjiang.googlepages.com/vimim.4corner.txt
"            http://maxiangjiang.googlepages.com/vimim.english.txt
" -----------------------------------------------------------
"  Usage [A] in Insert Mode, "to insert Chinese ad hoc":
"            # type key code and hit CTRL-^ to insert Chinese
"  Usage [B] in Insert Mode, "to input Chinese continously":
"            # hit CTRL-\ to toggle to VimIM Chinese Input Mode
"            # type any key code defined in input method, and hit Space
" -----------------------------------------------------------
" Screen Shot: http://maxiangjiang.googlepages.com/vimim.gif
" Chinese URL: http://maxiangjiang.googlepages.com/vimim.html
" Latest Code: http://maxiangjiang.googlepages.com/vimim.vim.html
"  News Group: http://groups.google.com/group/vimim
" ----------------------------- }}}

" == "The VimIM Instruction" == {{{
" =================================

" ------------------------
" "The VimIM Design Goals"
" ------------------------
" # Chinese can be input using Vim regardless of encoding
" # Without negative impact to Vim if VimIM is not used
" # No comprise for high speed and low memory usage
" # Most VimIM options are activated by default
" # All  VimIM options can be explicitly disabled at will

" ---------------------------------
" "Sample vimrc to display Chinese"
" ---------------------------------
" set gfn=Courier_New:h12:w7,Arial_Unicode_MS
" set gfw=NSimSun-18030,NSimSun ambiwidth=double
" set enc=utf8 fencs=ucs-bom,utf8,chinese,taiwan,ansi

" -------------------
" "The VimIM Options"
" -------------------

"   "The VimIM OneKey Input"
"   - use OneKey to insert the default candidate
"   - use OneKey again to perform cycle navigation
"   - use OneKey to directly insert unicode
"   - use OneKey to do simple calculation
"   - use OneKey in Visual to do English to Chinese Translation
"   The default key is CTRL-^ (Insert Mode & Visual Mode)
" # To disable :let g:vimim_disable_one_key=1

"   "The VimIM Smart Chinese Input Mode"
"   - show dynamic menu as you type
"   - trigger the popup menu using Smart <Space>
"   - toggle VimIM Insert Mode automatically
"   - toggle English and Chinese punctuation
"   The default key is i_CTRL-\
" # To disable :let g:vimim_disable_smart_space=1

"   The VimIM Quick English Input
"   - Quick English input within VimIM Chinese Input Mode
"     (1) *alphabet ==> alphabet (starting with star)
"     (2)  EGNLISH  ==> ENGLISH
" # To disable :let g:vimim_disable_quick_english_input=1

"   The VimIM default settings for xingma user
"   - No fuzzy search   (:let g:vimim_disable_fuzzy_search=1)
"   - No quick key mode (:let g:vimim_disable_quick_key=1)
"   - No menu order update on past usage
"   - It is automatically enabled for 'wubi' or 'cangjie'
" # To enable :let g:vimim_enable_xingma_preference=1

" # To enable VimIM "Default Off" Options
"   -----------------------------------
"   let g:vimim_enable_label_navigation=1
"   let g:vimim_enable_dummy_calculator=1
"   let g:vimim_enable_...

" # To disable VimIM "Default On" Options
"   -----------------------------------
"   let g:vimim_menu_order_update_frequency=999
"   let g:vimim_disable_fuzzy_search=1
"   let g:vimim_disable_...

" Detailed list and usage of all options can be found from
" http://maxiangjiang.googlepages.com/vimim.html

" ---------------------
" "The VimIM Data File"
" ---------------------

" Non UTF-8 datafile is also supported: when the data filename
" includes 'chinese', it is assumed to be encoded in 'chinese'.

" The datafile is assumed in order, otherwise, it is sorted automatically.
" The basic format of the data file is simple and flexible:

"    +------+--+-------+
"    |<key> |  |<value>|
"    |======|==|=======|
"    | mali |  |  馬力 |
"    +------+--+-------+

" The <key> is what is typed in alphabet, ready to be replaced.
" The <value>, separated by spaces, is what will be input.
" The 2nd and the 3rd column can be repeated without restriction.

" Following sample data files are provided:
" --------------------------------------------------
"  vimim.pinyin.txt    =>  input method for PinYin
"  vimim.wubi.txt      =>  input method for WuBi
"  vimim.cangjie.txt   =>  input method for CangJie
"  vimim.quick.txt     =>  input method for Quick
"  vimim.4corner.txt   =>  input method for 4Corner
"  vimim.english.txt   =>  input method for English
" --------------------------------------------------
" ============================= }}}

" == "The VimIM Core Engine" == {{{
" =================================

if exists("b:loaded_vimim")||&cp||v:version<700
    finish
endif
let b:loaded_vimim=1

set completefunc=b:VimIM
" --------------------------------
function! b:VimIM(start, keyboard)
" --------------------------------
if a:start

    let start_row = line('.')
    let start_column = col('.')-1
    let current_line = getline(start_row)
    let char_before = current_line[start_column-1]

    " define valid characters for input key code
    " ------------------------------------------
    let char_valid = "[*.0-9A-Za-z]"
    if s:vimim_disable_quick_english_input<1
        let char_valid = "[*.0-9A-Za-z:/@]"
        if s:vimim_enable_dummy_calculator>0
            let char_valid = "[*.0-9A-Za-z:/@(,)+\-]"
        endif
    endif

    " avoid hanging on triggering nothing
    " -----------------------------------
    if start_column < 1
        return
    endif

    " avoid hanging on triggering non-word char
    " -----------------------------------------
    if char_before !~ char_valid
        return
    endif

    " note: use =~# for case sensitive match
    " --------------------------------------
    while start_column > 0 && current_line[start_column-1] =~ char_valid
        let start_column -= 1
    endwhile

    " get user's previous selection
    " -----------------------------
    if s:vimim_menu_order_update_frequency<999
        if s:start_row_before>start_row || s:usage_history_update>0
            let s:keyboard_chinese = ''
        else
            let s:usage_history_update = 0
            let chinese = s:VimIM_usage_history(start_row, start_column, 
            \ s:start_row_before, s:start_column_before, s:keyboard_key)
            if len(chinese)>0 && chinese !~ '\w'
                let s:keyboard_chinese = chinese
                let s:keyboard_counts += 1
            endif
        endif
        let s:start_row_before = start_row
        let s:start_column_before = start_column
    endif
    return start_column

else

    let keyboard = a:keyboard

    " support direct Unicode input: 39340 as &#39340; in HTML
    " -------------------------------------------------------
    if s:vimim_disable_direct_unicode_input<1
        if keyboard =~ '\d\{5}' && &encoding == "utf-8"
            sil!let unicode = s:VimIM_unicode(keyboard)
            if len(unicode) > 0
                return unicode
            endif
        endif
    endif

    " do quick english input and hunt for easter eggs
    " -----------------------------------------------
    if s:vimim_disable_quick_english_input<1
        sil!let english = s:VimIM_quick_English_input(keyboard)
        if len(english) > 0
            return english
        endif
    endif

    " run dummy calculator for inspiration
    " ------------------------------------
    if s:vimim_enable_dummy_calculator>0 && &l:iminsert!=1
        if keyboard !~ "[^0-9.*+/\-]" && keyboard =~ "[*+/\-]"
            sil!let calculator = s:VimIM_dummy_calculator(keyboard)
            if len(calculator) > 0
                return calculator
            endif
        endif
    endif

    " Now, build again valid keyboard characters
    " ------------------------------------------
    if strlen(keyboard) < 1
    \||  keyboard !~ '[0-9A-Za-z.*]'
    \||  keyboard =~ '^[.*]\+'
    \||  keyboard =~ '\'
    \|| (keyboard =~ '[' && keyboard =~ ']')
    \|| (keyboard !~# "vim" && s:vimim_easter_eggs>0)
        return
    endif

    " hunt real VimIM easter eggs ... vim<C-^>
    " ----------------------------------------
    if keyboard =~# "vim"
        return VimIM_popupmenu_list(s:easter_eggs, 3)
    endif

    " The data file is loaded once and only once.
    " -------------------------------------------
    if s:vimim_datafile_loaded<1
        let s:vimim_datafile_loaded=1
        if len(s:current_datafile)>0 && filereadable(s:current_datafile)
            let s:datafile_lines = readfile(s:current_datafile)
        endif
    endif
    let localization = s:VimIM_localization()
    let lines = s:datafile_lines

    " initialize and re-order the data file
    " -------------------------------------
    " modify the datafile in memory based on past usage
    if s:vimim_menu_order_update_frequency<999
        let key = s:keyboard_key
        let chinese = s:keyboard_chinese
        let lines_new = s:VimIM_order_on_usage(key, chinese, lines)
        if len(lines_new) > 0
            let lines = lines_new
            let s:datafile_lines = lines
            " make persistent the datafile in memory
            let frequency = s:vimim_menu_order_update_frequency
            if frequency < 1
                let frequency = 8
            endif
            if s:keyboard_counts%frequency==0
                sil!call s:VimIM_save_datafile(lines)
            endif
        endif
    endif
    if len(keyboard)>0  && keyboard =~ '\a'
        let s:keyboard_key = keyboard
    else
        let s:keyboard_key = ''
    endif

    " add boundary to datafile search by one letter only
    " --------------------------------------------------
    let ranges = s:VimIM_search_boundary(keyboard, lines)
    if len(ranges)<1
        return
    elseif ranges[0]>ranges[1]
        let lines = sort(lines)
        let s:datafile_lines = lines
        let ranges = s:VimIM_search_boundary(keyboard, lines)
    endif
    " now only play with portion of datafile of interest
    " --------------------------------------------------
    let lines = lines[ranges[0] : ranges[1]]


    " -------------------------------------------
    " do wildcard search: explicitly fuzzy search
    " -------------------------------------------
    if s:vimim_disable_wildcard_search<1
        let wildcard = match(keyboard, '[.*]')
        if wildcard > 0
            let s:usage_history_update = 1
            let fuzzies = substitute(keyboard,'[*]','.*','g')
            let fuzzy = '^' .  fuzzies . '\>'
            call filter(lines, 'v:val =~ fuzzy')
            return VimIM_popupmenu_list(lines, localization)
        endif
    endif

    " Note: limitation for 4 corner data file
    " (1) assuming exact 4 digits as the key
    " (2) no wildcard allowed for key with leading zero
    " -------------------------------------------------
    let match_start = -1
    let key = keyboard
    if key =~ '^\d\+\s\+'
        let key = printf('%04o', key)
    else
        let key = printf('%s', key)
    endif
    let match_start = match(lines, '^'.key)

    " ------------------------------------------
    " to guess user's intention using auto_spell
    " ------------------------------------------
    if s:vimim_enable_auto_spell>0
    \ && match_start < 0
        let key = s:VimIM_auto_spell(keyboard)
        let match_start = match(lines, '^'.key)
    endif

    " --------------------------------------------
    " to guess user's intention using fuzzy_pinyin
    " --------------------------------------------
    if s:vimim_enable_fuzzy_pinyin>0
    \ && match_start < 0
        let key = s:VimIM_fuzzy_pinyin(keyboard)
        let match_start = match(lines, '^'.key)
    endif

    if match_start > -1
        let match_end = VimIM_exact_match(match_start, keyboard, lines)
        let matched_list = lines[match_start : match_end]
        return VimIM_popupmenu_list(matched_list, localization)
    endif

    " ----------------------------------------
    " do fuzzy search, implicitly fuzzy search
    " ----------------------------------------
    if s:vimim_disable_fuzzy_search<1 && strlen(keyboard) > 2
        let s:usage_history_update = 1
        let fuzzies = join(split(keyboard,'\ze'),'.*')
        let fuzzy = '^' .  fuzzies . '.*'
        let lines = filter(lines, 'v:val =~ fuzzy')
        return VimIM_popupmenu_list(lines, localization)
    endif

endif
endfunction

" --------------------------------------------------------
function! VimIM_popupmenu_list(matched_list, localization)
" --------------------------------------------------------
    let matched_list = a:matched_list
    if len(matched_list)<1
        return []
    endif
    let pair_matched_list = []
    " ----------------------
    for line in matched_list
    " ----------------------
        if len(line) < 1
            continue
        endif
        if a:localization == 1
            let line = iconv(line, "chinese", "utf-8")
        elseif a:localization == 2
            let line = iconv(line, "utf-8", &enc)
        endif
        let oneline_list = split(line, '\s\+')
        let menu = remove(oneline_list, 0)
        for word in oneline_list
            call add(pair_matched_list, menu .' '. word)
        endfor
    endfor
    let label = s:vimim_enable_non_zero_based_label
    let popupmenu_list = []
    " ----------------------------
    for words in pair_matched_list
    " ----------------------------
        let pairs = split(words)
        let word = get(pairs, 1)
        let complete_items = {}
        if len(word)>1
            let complete_items["word"] = word
        endif
        if s:vimim_disable_popup_label<1
            if s:vimim_enable_pinyin_tone_input
                if label < s:vimim_pinyin_tone+1
                    let label = "0" . label
                endif
            endif
            let abbr = printf('%2s',label) . "\t" . word
            let complete_items["abbr"] = abbr
        endif
        if s:vimim_disable_popup_extra_text<1
            let menu = get(pairs, 0)
            let complete_items["menu"] = menu
        endif
        let complete_items["dup"] = 1
        let label = label + 1
        call add(popupmenu_list, complete_items)
    endfor
    return popupmenu_list
endfunction

" --------------------------------------------
function! VimIM_exact_match(start, key, lines)
" --------------------------------------------
    let match_start = a:start
    let keyboard = a:key
    let lines = a:lines
    let patterns = '^\(' . keyboard . '\)\@!'
    let result = match(lines, patterns, match_start)-1
    if result - match_start < 1
        return match_start
    endif
    if s:vimim_disable_quick_key < 1
        if result > match_start
            let results = []
            for line in lines[match_start : result]
                call extend(results, split(line))
            endfor
            let total_chinese = len(results)-(result-match_start)-1
            if total_chinese > 88 || strlen(keyboard) < 2
                let patterns = '^\(' . keyboard . '\>\)\@!'
                let result = match(lines, patterns, match_start)-1
            endif
        endif
    endif
    let match_end = match_start
    if result > 0 && result > match_start
        let match_end = result
    endif
    return match_end
endfunction

" ------------------------------------------------
function! s:VimIM_search_boundary(keyboard, lines)
" ------------------------------------------------
    let lines = a:lines
    let keyboard = a:keyboard
    let first_char_typed = strpart(keyboard,0,1)
    let patterns = '^' . first_char_typed
    let match_start = match(lines, patterns)
    let ranges = []
    if match_start<0 || len(lines)<1
        return []
    endif
    call add(ranges, match_start)
    let match_next = match_start
    let last_line_in_datafile = lines[-1]
    let first_char_last_line = strpart(last_line_in_datafile,0,1)
    if first_char_typed == first_char_last_line
        let match_next = len(lines)-1
    else
        let pattern_next = '^[^' . first_char_typed . ']'
        let result = match(lines, pattern_next, match_start)
        if  result > 0
            let match_next = result
        endif
    endif
    call add(ranges, match_next)
    return ranges
endfunction

" --------------------------------------------------------
function! s:VimIM_order_on_usage(keyboard, chinese, lines)
" --------------------------------------------------------
    let keyboard = a:keyboard
    if len(keyboard)<1 || keyboard !~ '\a'
        return []
    endif
    let chinese = a:chinese
    if len(chinese)<1  || chinese =~ '\w'
        return []
    endif
    let lines = a:lines
    """ step 1/4: modify datafile in memory based on usage
    let one_line_chinese_list = []
    let patterns = '^' . keyboard . '\s\+'
    let matched = match(lines, patterns)
    if matched < 0
        return []
    endif
    let insert_index = matched
    """ step 2/4: remove all entries matching key from datafile
    while matched > 0
        let old_item = remove(lines, matched)
        let values = split(old_item)
        call extend(one_line_chinese_list, values[1:])
        let matched = match(lines, patterns, insert_index)
    endwhile
    """ step 3/4: make a new order list
    let used = match(one_line_chinese_list, chinese)
    if used > 0
        let head = remove(one_line_chinese_list, used)
        call insert(one_line_chinese_list, head)
    endif
    """ step 4/4: insert the new order list into the datafile
    if len(one_line_chinese_list) > 0
        let new_item = keyboard .' '. join(one_line_chinese_list)
        call insert(lines, new_item, insert_index)
    endif
    return lines
endfunction

" --------------------------------------------------------------
function! s:VimIM_usage_history(row, column, row2, column2, key)
" --------------------------------------------------------------
    let start_row = a:row
    let start_column = a:column
    let start_row_before = a:row2
    let char_start = a:column2
    let char_end = start_column-1
    let chinese = ''
    " update dynamic menu order based on past usage frequency
    if start_row_before == start_row && char_end > char_start
        let current_line = getline(start_row)
        let chinese = current_line[char_start : char_end]
    elseif start_row - start_row_before == 1
        let previous_line = getline(start_row_before)
        let char_end = len(previous_line)
        let chinese = previous_line[char_start : char_end]
    endif
    if chinese =~ '\A' && len(a:key)>0  && a:key =~ '\a'
        let chinese = substitute(chinese,'\p\+','','g')
    endif
    return chinese
endfunction

" ------------------------------------
function! s:VimIM_auto_spell(keyboard)
" ------------------------------------
    " A sample of auto spelling rule:
    "    tign  => tign
    "    yve   => yue
    " --------------------------------
    let rules = {}
    let rules['ign'] = 'ing'
    let rules['iou'] = 'iu'
    let rules['uei'] = 'ui'
    let rules['uen'] = 'un'
    let rules['mg']  = 'ng'
    let rules['ve']  = 'ue'
    " --------------------------------
    let pattern = a:keyboard
    for key in keys(rules)
        let new_key = rules[key]
        if pattern =~ key
            let pattern = substitute(pattern, key, new_key, '')
            break
        endif
    endfor
    return pattern
endfunction

" --------------------------------------
function! s:VimIM_fuzzy_pinyin(keyboard)
" --------------------------------------
    " A sample of fuzzy pinyin rule:
    "  zhengguoren  => zhongguoren
    "  chengfengqia => chongfengqia
    " ----------------------------------
    let rules = {}
    let rules['eng'] = 'ong'
    let rules['ian'] = 'iang'
    let rules['uan'] = 'uang'
    let rules['an']  = 'ang'
    let rules['in']  = 'ing'
    " ----------------------------------
    let pattern = a:keyboard
    for key in keys(rules)
        let new_key = rules[key]
        if pattern =~ key
            let pattern = substitute(pattern, key, new_key, '')
            break
        endif
    endfor
    return pattern
endfunction

" ---------------------------------------------
function! s:VimIM_quick_English_input(keyboard)
" ---------------------------------------------
    let keyboard = a:keyboard
    let results = []
    let result = ''
    """ all capitals remain the same
    if keyboard !~ '\A' && keyboard ==# toupper(keyboard)
        let result = keyboard
    endif
    """ intial single star or double star
    if keyboard =~ '^*' && keyboard !~ '^\d\+\s\+'
        if keyboard =~ '^*\{1}\w'
            if s:vimim_disable_quick_english_input<1
                let result = strpart(keyboard,1)
            endif
        elseif keyboard =~ '^*\{2}\w'
            let chinese=copy(s:translators)
            let chinese.dict=s:ecdict
            if keyboard =~ '*\{2}casino'
                " **casino -> congratulations! US$88000
                let casino = matchstr(localtime(),'..$')*1000
                let casino = 'casino US$'.casino
                let casino = chinese.translate(casino)
                let result = casino
            elseif keyboard =~ '*\{2}girls'
                let result = chinese.translate('grass')
            elseif keyboard =~ '*\{2}today' || keyboard =~ '*\{2}now'
                " **today  -> 2009 year February 22 day Wednesday
                if keyboard =~ '*\{2}today'
                    let today = strftime("%Y year %B %d day %A")
                    let today = chinese.translate(today)
                    let result = today
                " **now -> Sunday AM 8 hour 8 minute 8 second
                elseif keyboard =~ '*\{2}now'
                    let now = strftime("%A %p %I hour %M minute %S second")
                    let now = chinese.translate(now)
                    let result = now
                endif
            elseif keyboard =~ '*\{2}author' && &encoding == "utf-8"
                let author = nr2char(39340).nr2char(28248).nr2char(27743)
                let author = chinese.translate(author)
                let url = "http://maxiangjiang.googlepages.com/index.html"
                let result = author.'    '.url
            elseif keyboard =~ '*\{2}help'
                let help = "http://maxiangjiang.googlepages.com/vimim.html"
                let result = help
            elseif keyboard =~ '*\{2}sign' && len(s:vimim_signature)>1
                let sign = s:vimim_signature
                let result = sign
            elseif keyboard =~ '*\{2}\d\+'
                let number = join(split(strpart(keyboard,2),'\ze'),' ')
                let number = chinese.translate(number)
                let number = join(split(number),'')
                let result = number
            endif
        endif
    endif
    " --------------------------------------------
    if len(result)>0
            let result .= ' '
        let results = [result]
    endif
    return results
endfunction

" ---------------------------------
function! s:VimIM_unicode(keyboard)
" ---------------------------------
    let keyboard = a:keyboard
    let results = []
        if keyboard > 19968 && keyboard < 40870
            sil!let unicode = nr2char(keyboard)
            let results = [unicode]
        endif
    return results
endfunction

" ------------------------------------------
function! s:VimIM_dummy_calculator(keyboard)
" ------------------------------------------
    let keyboard = a:keyboard
    let lhs = keyboard
    let rhs = string(eval(lhs))
    let result  = lhs .' = '. rhs .' '
    return [result]
endfunction

" ------------------------------
function! s:VimIM_localization()
" ------------------------------
    let localization = 0
    let datafile_fenc_chinese=0
    if s:current_datafile =~? "chinese"
        let datafile_fenc_chinese=1
    endif
    " ------------ ----------------- ----
    " vim encoding datafile encoding code
    " ------------ ----------------- ----
    "   utf-8           utf-8         0
    "   utf-8           chinese       1
    "   chinese         utf-8         2
    "   chinese         chinese       3
    " ------------ ----------------- ----
    if &encoding == "utf-8"
        if datafile_fenc_chinese
            let localization = 1
        endif
    elseif  &enc == "chinese"
        \|| &enc == "gb2312"
        \|| &enc == "gbk"
        \|| &enc == "cp936"
        \|| &enc == "euc-cn"
        if datafile_fenc_chinese < 1
            let localization = 2
        endif
    endif
    return localization
endfunction

" ------------------------------------
function! s:VimIM_save_datafile(lines)
" ------------------------------------
    let datafile = s:current_datafile
    let lines = a:lines
    if len(datafile)<1 || len(lines)<1
        return
    endif
    if filewritable(datafile)
        sil!call writefile(lines, datafile)
    endif
endfunction

" -----------------------------------
function! s:VimIM_get_datafile_name()
" -----------------------------------
    """ step 1: locate the default data file
    let datafile = s:vimim_datafile
    if !filereadable(datafile)
        let files = ["vimim.pinyin"]
        call add(files, "vimim.wubi")
        call add(files, "vimim.cangjie")
        call add(files, "vimim.quick")
        call add(files, "vimim.4corner")
        for file in files
            call insert(files, file.".txt")
        endfor
        call insert(files, "vimim.txt")
        for file in files
            let datafile = s:path . file
            if filereadable(datafile)
                break
            else
                continue
            endif
        endfor
    endif
    """ step 2: show Easter Egg if no data file
    if !filereadable(datafile)
        let s:vimim_easter_eggs=1
        let datafile = ''
    endif
    return datafile
endfunction
" =============================== }}}

" == "The VimIM Core FrontEnd" == {{{
" ===================================

" --------------------------------
function! s:VimIM_labels_on(start)
" --------------------------------
    if a:start < 6
        for n in range(a:start,9)
            sil!exe 'inoremap <silent> <expr> ' . n .
            \' VimIM_label('.n.')'
        endfor
    endif
    " ----------------------
    function! VimIM_label(n)
    " ----------------------
        if pumvisible()
            let label = s:vimim_enable_non_zero_based_label
            let counts = repeat("\<C-N>",a:n-label)
            let end = '\<C-Y>'
            if s:vimim_enable_label_navigation && &l:iminsert!=1
                let end = ''
            endif
            sil!exe 'return "' . counts . end . '"'
        else
            return a:n
        endif
    endfunction
endfunction

" ----------------------------
function! s:VimIM_labels_off()
" ----------------------------
    let s:vimim_labels_on_loaded=0
    for n in range(1,9)
        exe 'sil! iunmap '.n
    endfor
endfunction

" ---------------------------
function! <SID>VimIM_OneKey()
" ---------------------------
    if s:vimim_labels_on_loaded<1
        sil!call s:VimIM_labels_on(1)
        let s:vimim_labels_on_loaded=1
    endif
    if pumvisible()
        sil!return "\<C-N>"
    else
        set completeopt=menu,preview
        sil!return "\<C-X>\<C-U>\<C-U>\<C-P>"
    endif
endfunction

" ----------------------------
function! s:VimIM_setting_on()
" ----------------------------
    let s:saved_completeopt=&completeopt
    let s:saved_lazyredraw=&lazyredraw
    let s:saved_iminsert=&l:iminsert
    let s:saved_cpo=&cpo
    set completeopt=menuone,preview
    set nolazyredraw
    set cpo&vim
    let &l:iminsert=1
endfunction

" -----------------------------
function! s:VimIM_setting_off()
" -----------------------------
    let &completeopt=s:saved_completeopt
    let &lazyredraw=s:saved_lazyredraw
    let &l:iminsert=s:saved_iminsert
    let &cpo=s:saved_cpo
endfunction

" -----------------------------
function! <SID>VimIM_check(map)
" -----------------------------
    sil!exe 'sil! if getline(".")[col(".")-2] !~# "[' . s:keys . ']"'
        sil! return ""
    else
        sil! return a:map
    endif
endfunction

" ---------------------------
function! s:VimIM_insert_on()
" ---------------------------
    let s:vimim_insert=1
    sil!call s:VimIM_setting_on()
    if s:vimim_disable_chinese_punctuation<1
        for key in keys(s:punctuations)
            let value = s:punctuations[key]
            sil!exe 'inoremap <silent> '. key .' '. value
        endfor
    endif
    if s:vimim_disable_popup_label<1
        let start = 1
        if s:vimim_enable_pinyin_tone_input > 0
            let start = s:vimim_pinyin_tone+1
        endif
        sil!call s:VimIM_labels_on(start)
    endif
    if s:vimim_disable_dynamic_menu<1
        let keys = s:vimim_keys + s:vimim_keys_ext
        for char in keys
            sil!exe 'inoremap <silent> ' . char . '
            \  <C-R>=pumvisible()?"\<lt>C-E>":""<CR>'. char .
            \ '<C-R>=<SID>VimIM_check("\<lt>C-X>\<lt>C-U>")<CR>'
        endfor
    endif
    " ----------------------------------------------------
    inoremap <silent> <Space> <C-R>=<SID>VimIM_check
    \("\<lt>C-X>\<lt>C-U>")<CR><C-R>=<SID>VimIMSpace()<CR>
    " ----------------------------------------------------
    function! <SID>VimIMSpace()
        if pumvisible()
            if s:vimim_disable_dynamic_menu<1
                sil! return "\<C-Y>"
            else
                sil! return ""
            endif
        else
            sil! return " "
        endif
    endfunction
endfunction

" ----------------------------
function! s:VimIM_insert_off()
" ----------------------------
    let s:vimim_insert=0
    sil!call s:VimIM_setting_off()
    sil!iunmap <Space>
    if s:vimim_disable_chinese_punctuation<1
        for key in keys(s:punctuations)
            exe 'sil! iunmap '. key
        endfor
    endif
    if s:vimim_disable_popup_label<1
        sil!call s:VimIM_labels_off()
    endif
    if s:vimim_disable_dynamic_menu<1
        for char in extend(s:vimim_keys,s:vimim_keys_ext)
            exe 'sil! iunmap ' . char
        endfor
    endif
endfunction

" ----------------------------------
function! <SID>VimIM_insert_toggle()
" ----------------------------------
    if s:vimim_disable_smart_space_autocmd<1 && has("autocmd")
        sil!au InsertEnter sil!call s:vimim_insert_on()
        sil!au InsertLeave sil!call s:VimIM_insert_off()
        sil!au BufUnload   autocmd! InsertEnter,InsertLeave
    endif
    if s:vimim_insert<1
        sil!call s:VimIM_insert_on()
    else
        sil!call s:VimIM_insert_off()
    endif
    return "\<C-\>\<C-O>:redraw\<CR>"
endfunction

" ---------------------------
function! s:VimIM_seed_data()
" ---------------------------
    if s:vimim_seed_data_loaded
        return
    else
        let s:vimim_seed_data_loaded=1
    endif
    " --------------------------------
    let punctuations = {}
    let punctuations["#"]="＃"
    let punctuations["%"]="％"
    let punctuations["$"]="￥"
    let punctuations["&"]="※"
    let punctuations["{"]="『"
    let punctuations["}"]="』"
    let punctuations["["]="【"
    let punctuations["]"]="】"
    let punctuations["^"]="……"
    let punctuations["_"]="——"
    let punctuations['"']="“”"
    let punctuations["'"]="‘’"
    let punctuations["!"]="！"
    let punctuations["?"]="？"
    let punctuations["~"]="～"
    let punctuations[";"]="；"
    let punctuations["<"]="，"
    let punctuations[">"]="。"
    let punctuations["\"]="。"
    if s:vimim_enable_dummy_calculator<0
        let punctuations["("]="（"
        let punctuations[")"]="）"
        let punctuations[","]="，"
        let punctuations["+"]="＋"
        let punctuations["-"]="－"
    endif
    if s:vimim_disable_quick_english_input>0
        let punctuations["@"]="・"
    endif
    if s:vimim_disable_wildcard_search>0
        let punctuations["."]="。"
    endif
    let s:punctuations = punctuations
    " --------------------------------
    let ecdict = {}
    let ecdict['casino']='中奖啦！'
    let ecdict['grass']='天涯何处无芳草！'
    let ecdict['january']='一月'
    let ecdict['february']='二月'
    let ecdict['march']='三月'
    let ecdict['april']='四月'
    let ecdict['may']='五月'
    let ecdict['june']='六月'
    let ecdict['july']='七月'
    let ecdict['august']='八月'
    let ecdict['september']='九月'
    let ecdict['october']='十月'
    let ecdict['november']='十一月'
    let ecdict['december']='十二月'
    let ecdict['am']='上午'
    let ecdict['pm']='下午'
    let ecdict['year']='年'
    let ecdict['day']='号'
    let ecdict['hour']='时'
    let ecdict['minute']='分'
    let ecdict['second']='秒'
    let ecdict['monday']='星期一'
    let ecdict['tuesday']='星期二'
    let ecdict['wednesday']='星期三'
    let ecdict['thursday']='星期四'
    let ecdict['friday']='星期五'
    let ecdict['saturday']='星期六'
    let ecdict['sunday']='星期日'
    let ecdict['0']='○'
    let ecdict['1']='一'
    let ecdict['2']='二'
    let ecdict['3']='三'
    let ecdict['4']='四'
    let ecdict['5']='五'
    let ecdict['6']='六'
    let ecdict['7']='七'
    let ecdict['8']='八'
    let ecdict['9']='九'
    let ecdict[',']='，'
    let ecdict['.']='。'
    let s:ecdict = ecdict
    " --------------------------------
    let s:dummy=copy(s:translators)
    let s:dummy.dict=copy(s:ecdict)
    " --------------------------------
         let s:easter_eggs = ["vi 文本编辑器"]
    call add(s:easter_eggs, "vim 最牛文本编辑器")
    call add(s:easter_eggs, "vim 精力 生氣")
    call add(s:easter_eggs, "vimim 中文输入法")
    " --------------------------------
endfunction

" --------------------------------------------
function! <SID>VimIM_dummy_translator(english)
" --------------------------------------------
    if s:vimim_dummy_dictionary_loaded<1
        let s:vimim_dummy_dictionary_loaded=1
        let dictionary_lines = readfile(s:current_dictionary)
        if len(dictionary_lines)>1
            let localization = s:VimIM_localization()
            for line in dictionary_lines
                if len(line) < 1
                    continue
                endif
                if localization == 1
                    let line = iconv(line, "chinese", "utf-8")
                elseif localization == 2
                    let line = iconv(line, "utf-8", &enc)
                endif
                let items = split(line)
                let s:dummy.dict[items[0]] = items[1]
            endfor
        else
            return
        endif
    endif
    let english = substitute(a:english,'\A',' & ','g')
    let chinese = s:dummy.translate(english)
    let chinese = substitute(chinese,"[ ']",'','g')
    let chinese = substitute(chinese,'\a\+',' & ','g')
    let chinese = substitute(chinese,nr2char(12290),'&\n','g')
    return chinese
endfunction

let s:translators = {}
" ------------------------------------------
function! s:translators.translate(line) dict
" ------------------------------------------
    return join(map(split(a:line),'get(self.dict,tolower(v:val),v:val)'))
endfunction

" ----------------------------
function! s:VimIM_initialize()
" ----------------------------
    let G = []
    call add(G, "g:vimim_datafile")
    call add(G, "g:vimim_signature")
    call add(G, "g:vimim_menu_order_update_frequency")
    call add(G, "g:vimim_disable_quick_key")
    " ------------------------------------------------
    call add(G, "g:vimim_disable_direct_unicode_input")
    call add(G, "g:vimim_disable_wildcard_search")
    call add(G, "g:vimim_disable_fuzzy_search")
    call add(G, "g:vimim_disable_one_key")
    call add(G, "g:vimim_disable_smart_space")
    call add(G, "g:vimim_disable_smart_space_autocmd")
    call add(G, "g:vimim_disable_chinese_punctuation")
    call add(G, "g:vimim_disable_popup_extra_text")
    call add(G, "g:vimim_disable_popup_label")
    call add(G, "g:vimim_disable_dynamic_menu")
    call add(G, "g:vimim_disable_quick_english_input")
    " ------------------------------------------------
    call add(G, "g:vimim_enable_xingma_preference")
    call add(G, "g:vimim_enable_label_navigation")
    call add(G, "g:vimim_enable_auto_spell")
    call add(G, "g:vimim_enable_fuzzy_pinyin")
    call add(G, "g:vimim_enable_non_zero_based_label")
    call add(G, "g:vimim_enable_pinyin_tone_input")
    call add(G, "g:vimim_enable_dummy_calculator")
    " ------------------------------------------------
    for variable in G
        let s_variable = substitute(variable,"g:","s:",'')
        if !exists(variable)
            exe 'let '. s_variable . '=0'
        else
            exe 'let '. s_variable .'='. variable
            exe 'unlet! ' . variable
        endif
    endfor
    " ------------------------------------------------
    let s:keys="abcdefghijklmnopqrstuvwxyz"
    if s:vimim_disable_wildcard_search<1
        let s:keys .= ".*"
    endif
    let s:vimim_pinyin_tone=0
    if s:vimim_enable_pinyin_tone_input > 0
        let s:vimim_disable_dynamic_menu=0
        let s:vimim_disable_one_key=1
        if s:vimim_enable_pinyin_tone_input < 5
        \|| s:vimim_enable_pinyin_tone_input > 9
            let s:vimim_pinyin_tone = 4
            let s:keys .= "1234"
        else
            let s:vimim_pinyin_tone = s:vimim_enable_pinyin_tone_input
            let s:keys .= "123456789"
        endif
    endif
    let s:vimim_keys=split(s:keys,'\zs')
    let s:vimim_keys_ext=["<BS>","<C-H>"]
    let s:vimim_insert=0
    " ------------------------------------------------
    let s:vimim_labels_on_loaded=0
    let s:vimim_seed_data_loaded=0
    let s:vimim_datafile_loaded=0
    let s:datafile_lines = []
    let s:punctuations = {}
    " ------------------------------------------------
    let s:start_row_before = 0
    let s:start_column_before = 0
    let s:keyboard_key = ''
    let s:keyboard_chinese = ''
    let s:keyboard_counts = 0
    let s:usage_history_update = 0
    " ------------------------------------------------
    let s:vimim_dummy_dictionary_loaded=0
    let s:current_dictionary=''
    let dictionary = s:path . "vimim.english.txt"
    if filereadable(dictionary)
        let s:current_dictionary = dictionary
    endif
    " ------------------------------------------------
    let s:vimim_easter_eggs=0
    let datafile = s:VimIM_get_datafile_name()
    let s:current_datafile = datafile
    if len(datafile)>0
        if s:vimim_enable_xingma_preference > 0
        \|| datafile =~ 'wubi'
        \|| datafile =~ 'cangjie'
            let s:vimim_disable_fuzzy_search=1
            let s:vimim_disable_quick_key=1
            let s:vimim_menu_order_update_frequency=999
        endif
    endif
endfunction
" ============================== }}}

" == "The VimIM Core Mapping" == {{{
" ==================================
scriptencoding utf-8
let s:path=expand("<sfile>:p:h")."/"
sil!call s:VimIM_initialize()
sil!call s:VimIM_seed_data()

if s:vimim_disable_one_key<1
    if len(s:current_dictionary) > 0
        xnoremap<silent><C-^> y:put!=<SID>VimIM_dummy_translator(@0)<CR>
    endif
    inoremap <silent> <expr> <C-^> <SID>VimIM_OneKey()
endif

if s:vimim_disable_smart_space<1
    inoremap <silent> <expr> <C-\> <SID>VimIM_insert_toggle()
endif
" ========== }}}

" ----------
" References
" ----------
" http://groups.google.com/group/vimim
" http://groups.google.com/group/vim_use/topics
" http://maxiangjiang.googlepages.com/vimim.html
