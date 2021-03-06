" toggle_words.vim
" Author: Vincent Wang (linsong dot qizi at gmail dot com)
" Created:  Fri Jun 29 18:06:29 CST 2007
" Requires: Vim Ver7.0+ 
" Version:  1.1
"
" Documentation: 
"   The purpose of this plugin is very simple, it can toggle words among
"   'true'=>'false', 'True'=>'False', 'if'=>'elseif'=>'else'=>'endif' etc . 
"
"   To use it, move the cursor on some words like 'true', 'False', 'YES', etc, 
"   call command 
"     :ToggleWord
"
"   It will toggle 'true'=>'false', 'False'=>'True', 'YES'=>'NO' etc. Yes,
"   this script will try to take the case into account when toggling words, so
"   'True' will be toggled to 'False' instead of 'false'. Currently the way to
"   check the case is very simple, but it works well for me.
"
"   You can define a map for 'ToggleWord' comand to make it easier: 
"     nmap ,t :ToggleWord<CR>
"
"   This script can search the candicate words to toggle based on
"   current filetype, for example, you can put the following configuration
"   into your .vimrc to define some words for python:
"      let g:toggle_words_dict = {'python': [['if', 'elif', 'else']]}
"   
"   There are some default words for toggling predefined in the
"   script(g:_toogle_words_dict) that will work for all filetypes.
"   Any comment, suggestion, bug report are welcomed. 


if v:version < 700
    "TODO: maybe I should make this script works under vim7.0
    echo "This script required vim7.0 or above version." 
    finish 
endif

if exists("g:load_toggle_words")
   finish
endif

let s:keepcpo= &cpo
set cpo&vim

let g:load_toggle_words = "1.1"

let g:_toggle_words_dict = {'*': [['true', 'false'], ['on', 'off'], ['yes', 'no'], ['+', '-'], ['define', 'undef'], ['if', 'elseif', 'else', 'endif'], ['>', '<'], ['{', '}'], ['(', ')'], ['[', ']'], ['==', '!='], ['success', 'failure'], ['start', 'stop'], ['up', 'down'], ['good', 'bad'], ['monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday'], ['january', 'february', 'march', 'april', 'may', 'june', 'july', 'august', 'september', 'october', 'novermber', 'december'], [],],  }

if exists('g:toggle_words_dict')
    :call extend(g:_toggle_words_dict, g:toggle_words_dict)
endif

function! s:ToggleWord()
    let cur_filetype = &filetype
    if ! has_key(g:_toggle_words_dict, cur_filetype)
        let words_candicates_array = g:_toggle_words_dict['*']
    else
        let words_candicates_array = g:_toggle_words_dict[cur_filetype] + g:_toggle_words_dict['*']
    endif
    let cur_word = expand("<cword>")
    let word_attr = 0 " 0 - lowercase; 1 - Capital; 2 - uppercase

    if toupper(cur_word)==#cur_word
        let word_attr = 2
    elseif cur_word ==# substitute(cur_word, '.*', '\u\0', '')
        let word_attr = 1
    else
        let word_attr = 0
    endif
    let cur_word = tolower(cur_word)

    for words_candicates in words_candicates_array
        let index = index(words_candicates, cur_word)
        if index != -1
            let new_word_index = (index+1)%len(words_candicates)
            let new_word = words_candicates[new_word_index]
            if word_attr==2
                let new_word =toupper(new_word)
            elseif word_attr==1
                let new_word = substitute(new_word, '.*', '\u\0', '')
            else
                let new_word = tolower(new_word)
            endif

            " use the new word to replace the old word
            exec "norm ciw" . new_word . ""
            break
        endif
    endfor
endfunction

command! ToggleWord :call <SID>ToggleWord() <CR>

let &cpo= s:keepcpo
unlet s:keepcpo
