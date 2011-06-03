"SwapIt: General Purpose related word swapping for vim
" Script Info and Documentation  {{{1
"=============================================================================
"
"    Copyright: Copyright (C) 2008 Michael Brown {{{2
"      License: The MIT License
"               
"               Permission is hereby granted, free of charge, to any person obtaining
"               a copy of this software and associated documentation files
"               (the "Software"), to deal in the Software without restriction,
"               including without limitation the rights to use, copy, modify,
"               merge, publish, distribute, sublicense, and/or sell copies of the
"               Software, and to permit persons to whom the Software is furnished
"               to do so, subject to the following conditions:
"               
"               The above copyright notice and this permission notice shall be included
"               in all copies or substantial portions of the Software.
"               
"               THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"               OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"               MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"               IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"               CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"               TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"               SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
"
" Name Of File: swapit.vim {{{2
"  Description: system for swapping related words
"   Maintainer: Michael Brown 
" Contributors: Ingo Karkat (speedating compatability)
"  Last Change:
"          URL:
"      Version: 0.1.2
"
"        Usage: {{{2
"               
"               On a current word that is a member of a swap list use the
"               incrementor/decrementor keys (:he ctrl-a,ctrl-x). The script
"               will cycle through a list of related options.
"
"               eg. 1. Boolean
"
"               foo=true 
"
"               in normal mode, pressing ctrl-a on the t will make it
"
"               foo=false
"               
"               eg. 2. Multi Word Swaps.
"               
"               Hello World! is a test multi word swap. 
"
"               on 'Hello World!' go select in visual (vi'<ctrl-a>) to get 
"
"               'GoodBye Cruel World!'
"
"               eg 3. Defining custom swaps 
"
"               A custom list is defined as follows. 
"
"               :SwapList datatypes bool char int float double
"               
"               The first argument is the list name and following args
"               are members of the list.
"               
"               if there is no match then the regular incrementor decrementor
"               function will work on numbers
"
"               At the bottom of the script I've added some generic stuff but
"              
"               You can create a custom swap file for file types at
"
"               ~/.vim/after/ftplugins/<filetype>_swapit.vim
"               with custom execs eg.
"               exec "SwapList function_scope private protected public"
"               
"               For this alpha version multi word swap list is a bit trickier
"               to to define. You can add to the swap list directly.
"                
"                 call extend(g:swap_lists, [{'name':'Multi Word Example', 
"                             \'options': ['swap with spaces',
"                             \'swap with  @#$@# chars in it' , \
"                             \'running out of ideas here...']}])
"
"               Future versions will make this cleaner
"
"               Also if you have a spur of the moment Idea type
"               :SwapIdea 
"               To get to the current filetypes swapit file
"
"               Note: This alpha version doesnt create the directory structure
"
"               To integrate with other incrementor scripts (such as
"               speeddating.vim or monday.vim), :nmap
"               <Plug>SwapItFallbackIncrement and <Plug>SwapItFallbackDecrement
"               to the keys that should be invoked when swapit doesn't have a
"               proper option. For example for speeddating.vim:
"
"               nmap <Plug>SwapItFallbackIncrement <Plug>SpeedDatingUp
"               nmap <Plug>SwapItFallbackDecrement <Plug>SpeedDatingDown
"
"         Bugs: {{{2 
"
"               Will only give swap options for first match (eg make sure
"               options are unique).
"
"               The visual mode is inconsistent on highlighting the end of a
"               phrase occasionally one character under see VISHACK
"
"        To Do: {{{2
"
"               - improve filetype handling
"               - look at load performance if it becomes an issue
"               - might create a text file swap list rather than vim list
"               - look at clever case option to reduce permutations
"               - look at possibilities beyond <cword> for non word swaps
"                   eg swap > for < , == to != etc.
"               - add a repeated keyword warning for :SwapList
"               - add repeat resolition confirm option eg.
"                SwapSelect>   a. (yes/no) b. (yes/no/maybe)
"
"               ideas welcome at mjbrownie (at) gmail dot com. 
"
"               I'd like to compile some useful swap lists for different
"               languages to package with the script
"
"Variable Initialization {{{1
if exists('g:loaded_swapit')
       finish
elseif v:version < 700
    echomsg "SwapIt plugin requires Vim version 7 or later"
    finish
endif
let g:loaded_swapit = 1

if !exists('g:swap_lists')
    let g:swap_lists = []
endif
if !exists('g:swap_list_dont_append')
    let g:swap_list_dont_append = 'no'
endif
if empty(maparg('<Plug>SwapItFallbackIncrement', 'n'))
   nnoremap <Plug>SwapItFallbackIncrement <c-a>
endif
if empty(maparg('<Plug>SwapItFallbackDecrement', 'n'))
   nnoremap <Plug>SwapItFallbackDecrement <c-x>
endif

"Command/AutoCommand Configuration {{{1
"
" For executing the listing
nnoremap <c-a> :<c-u>call SwapWord(expand("<cword>"),'forward', 'no')<cr> 
nnoremap <c-x> :<c-u>call SwapWord(expand("<cword>"),'backward','no')<cr>
vnoremap <c-a> "dy<esc>:call SwapWord(@d,'forward','yes')<cr>
vnoremap <c-x> "dy<esc>:call SwapWord(@d,'backward','yes')<cr>

" For adding lists
com! -nargs=* SwapList call AddSwapList(<q-args>)
com! ClearSwapList let g:swap_list = []
com! SwapIdea call OpenSwapFileType()
com! -range -nargs=1 SwapWordVisual call SwapWord(getline('.'),<f-args>,'yes')
au BufEnter call LoadFileTypeSwapList()

"Functions {{{1
"
"SwapWord() main processiong event function {{{2
fun! SwapWord (word, direction, is_visual)
    
    let swap_lists = g:swap_lists

    if g:swap_list_dont_append != 'yes'
        "add custom lists in filtype vim files or  vimrc
        let swap_lists = extend  (swap_lists,g:default_swap_list)
    endif

    let cur_word = a:word
    " Main for loop over each swaplist {{{3
    for swap_list  in g:swap_lists
        let word_options = swap_list['options']
        let word_index = index(word_options, cur_word) 

        if word_index != -1 
            if a:direction == 'forward'
                let word_index = word_index + 1
            else
                let word_index = word_index - 1
            endif

            "Deal with boundary conditions
           if  word_index < 0 && a:direction == 'backward'
                let list_size = len(word_options) 
                let word_index = list_size - 1
            elseif word_index >= len(word_options)
                let word_index = 0
            endif

            let next_word = word_options[word_index]

            let temp_reg = @s
            let @s = next_word
            let in_visual = 0 

            if a:is_visual == 'yes'
                if next_word =~ "\[\\W\\s\]"
                    let in_visual = 1
                    exec 'norm gv"sp`[v`]'
                else
                    exec 'norm gv"spb'
                endif
            else
                if next_word =~ "\[\\W\\s\]"
                    let in_visual = 1
                    exec 'norm viw"sp`[v`]'
                else
                    exec 'norm viw"spb'
                endif
            endif
            
            "TODO VISHACK This is a silly hack to fix the visual range. if the v ends with
            "a word character the visual range is onw column over but not for
            "non word characters.
            "
            if in_visual == 1 &&  (next_word =~ "\\w$")
                exec 'norm h'
            endif


            let @s = temp_reg
            echo "Swap: " . swap_list['name'] .' '. cur_word . " > " . next_word  
            "\. ' ' . word_index . ' ' . a:direction . ' ' . len(word_options) 
            return 1
        endif
    endfor
    "}}}
    ""echo "Swap: No match for " . cur_word
    if a:direction == 'forward'
       exec "normal \<Plug>SwapItFallbackIncrement"
    else
       exec "normal \<Plug>SwapItFallbackDecrement"
    endif
endfun
"AddSwapList()  Main Definition Function {{{2
"use with after/ftplugin/ vim files to set up file type swap lists
fun! AddSwapList(s_list)

    let word_list = split(a:s_list,'\s\+')

    if len(word_list) < 3
        echo "Usage :SwapList <list_name> <member1> <member2> .. <membern>"
        return 1
    endif

    let list_name = remove (word_list,0)

    call add(g:swap_lists,{'name':list_name, 'options':word_list})
endfun

"LoadFileTypeSwapList() "{{{2
"sources .vim/after/ftplugins/<file_type>_swapit.vim
fun! LoadFileTypeSwapList()

    let ftpath = "~/.vim/after/ftplugin/". &filetype ."_swapit.vim"
    let g:swap_lists = []
    if filereadable(ftpath) 
        exec "source " . ftpath
    endif
endfun

"OpenSwapFileType() Quick Access to filtype file {{{2
fun! OpenSwapFileType()
    let ftpath = "~/.vim/after/ftplugin/". &filetype ."_swapit.vim"
    if !filereadable(ftpath) 
        "TODO add a directory check 
        exec "10 split " . ftpath
        "exec 'norm I "SwapIt.vim definitions for ' . &filetype . ': eg exec "SwapList names Tom Dick Harry\"'
        return 0
    else
        exec "10 split " . ftpath
    endif
    exec "norm G"
endfun

"Default DataSet. Add Generic swap lists here {{{1
let g:default_swap_list = [ 
            \{'name':'yes/no', 'options': ['yes','no']},
            \{'name':'Yes/No', 'options': ['Yes','No']},
            \{'name':'True/False', 'options': ['True','False']},
            \{'name':'true/false', 'options': ['true','false']},
            \{'name':'AND/OR', 'options': ['AND','OR']},
            \{'name':'Hello World', 'options': ['Hello World!','GoodBye Cruel World!' , 'See You Next Tuesday!']},
            \{'name':'On/Off', 'options': ['On','Off']},
            \{'name':'on/off', 'options': ['on','off']},
            \{'name':'ON/OFF', 'options': ['ON','OFF']},
            \{'name':'comparison_operator', 'options': ['<','<=','==', '>=', '>' , '=~', '!=']},
            \{'name': 'datatype', 'options': ['bool', 'char','int','unsigned int', 'float','long', 'double']}
            \]
"NOTE: comparison_operator doesn't work yet but there in the hope of future
"capability
