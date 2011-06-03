" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin\code_complete.vim	[[[1
615
" ========================================================================
" File:             code_complete.vim
" Features:         function parameter complete, code snippets, and
"                   much more.
" Original Author:  Mingbai <mbbill AT gmail DOT com> 
" Maintainer:       StarWing <weasley_wx AT qq DOT com>
" Last Change:      2008-11-07 18:22:18
" Version:          2.9
" Details: {{{1
" Install:          1.  Put code_complete.vim and my_snippets.template
"                       to plugin directory.
"                   2.  Use the command below to create tags file
"                       including signature field.
"                       ctags -R --c-kinds=+p --fields=+S .
"
" Usage:
"                   Hotkey:
"
"                       "<A-d>" (default value of g:CodeCompl_Hotkey)
"                       we DON'T use <tab>, because it comflict with
"                       superstab.vim, you will quickly find Alt-S
"                       will be a good choice.  Do all the jobs with
"                       this key, see example:
"                       press <A-d> after function name and (
"                           foo ( <A-d>
"                       becomes:
"                           foo ( `<first param>`,`<second param>` )
"                       press <A-D> after code template
"                           if <A-d>
"                       becomes:
"                           if( `<...>` )
"                           {
"                               `<...>`
"                           }
"
"                   Commands:
"                       
"                       StartCodeComplele
"                           make code_complete work, it will be called
"                           when you edit a new buffer
"
"                       StopCodeComplete
"                           Just stop code_complete, and free all
"                           memory use.
"
"                       UpdateTemplate
"                           Update your filelist and template folder
"                           and your complete file(named as
"                           my_snippets.template defaultly), will
"                           called by code_complete when you edit new
"                           buffer, or change your filetype if you set
"                           g:CodeCompl_UpdateAlways.
"
"                       EditSnippets
"                           Edit your complete file.
"
"                       SaveAsTemplate
"                           Save current file to template folder, and
"                           use your filetype for postfix name, maybe
"                           you should add a bang if there are
"                           same-name file in your template folder
"                           
"                   variables:
"
"                       g:CodeCompl_Hotkey
"                           the key used to complete function
"                           parameters and key words. after you press
"                           ths key AFTER you input the complete-word,
"                           the word will be spreaded. you can see the
"                           paragraph above for detail features.
"
"                       g:CodeCompl_RegionStart g:CodeCompl_RegionEnd
"                           region start and stop you can change them
"                           as you like.  but you must be careful,
"                           because we use them to detemin marks in
"                           your template files(not complete file), so
"                           if you change it, maybe you must
"                           substitute all marks in your template
"                           files. after you change them, maybe you
"                           should update your template file, you can
"                           use this command:
"                           :UpdateTemplate<CR>
"
"                       g:CodeCompl_Template_Forechar
"                           you can use template file with
"                           template-forechar, you can create a file
"                           named 'complete-word.filetype',
"                           and drop it in
"                           g:CodeCompl_Template_Folder(default, it's
"                           'templates' in your runtimepath, e.g.
"                           D:\Vim\vimfiles\templates in windows, or
"                           /root/.vim/templates in linux/unix.
"                           after you create file, you can input
"                           forechar + complete-word to include the
"                           whole file. e.g. '#stdc' to include stdc.c
"                           if your current filetype is 'c'.
"
"                       g:CodeCompl_Template_Folder
"                           see paragraph above. the folder default is
"                           'templates'.
"
"                       g:CodeCompl_Complete_File
"                           file name of users defined snippets.  now
"                           it named 'my_snippets.template', use this
"                           postfix to prevent Vim load it
"                           automatically
"
"                       g:CodeCompl_UpdateAlways
"                       g:CodeCompl_ClearWhenUpdate
"                           see the Option section
"
"                       g:CodeCompl_SaveListInBuffer
"                           unused in current version
"
"
"                   global helper functions:
"                       
"                       MakeRegion
"                           same as g:CodeCompl_RegionStart . 'your
"                           text' . g:CodeCompl_RegionEnd, but input a
"                           little easily, but there are other helper
"                           function and variables in your complete
"                           file(defaults is my_snippets.template in
"                           your plugin folder)
"
"                   default complete-words:
"                           see "my_snippets.template" file.
" }}}
" ========================================================================
" MUST after Vim 7.0 {{{1
" and check whether code_complete has been loaded
if v:version < 700 || exists('g:loaded_code_complete')
    finish
endif
let g:loaded_code_complete = 1
" }}}
" ------------------------------------------------------------------------
" pre-defined Commands and Menu {{{1


" you can input the command with tab, e.g: :Start<Tab>, it can be faster
command StartCodeComplele call <SID>CodeCompleteStart()
command StopCodeComplete call <SID>CodeCompleteStop()
command UpdateTemplate call <SID>TemplateUpdate()
command EditSnippets call <SID>SnippetFileEdit()
command -bang SaveAsTemplate call <SID>TemplateFileSave('<bang>')

" Menus:
menu <silent> &Tools.Code\ Complete\ Start    :StartCodeComplele<CR>
menu <silent> &Tools.Code\ Complete\ Stop     :StopCodeComplete<CR>
" }}}
" Options, define them as you like in vimrc {{{1
" (or remain it in default value.)

" Hotkey to call code_complete {{{2
" we don't use <tab>, for compatible with supertab.vim you
" can use the default key:ALT-D, you will find it's easy to
" press in a USA-Standard keyboard. if you don't use
" supertab or you wan't use the default key, you can
" redefine it.
if !exists('g:CodeCompl_Hotkey')
    let g:CodeCompl_Hotkey = "<a-d>"
endif

" }}}
" Some marks to use template And Some Global helpers {{{2
" the Region Start and End, you can use them to define
" youself mark. e.g. g:rs.'hello'.g:re in defalut, it looks
" as '`<hello>`', and if you press hotkey, your cursor will
" move to the word, and into the selection mode. you can
" redefine them, but must be careful, because file template
" use the same marks, if you change it, you must substitute
" all marks in your all template-file.
" e.g. a template file(you can named std.c and drop it in
" your template-folder):
" #define <stdio.h>
" int main(void)
" {
"     `<Input Code here>`
"     retnrn 0;
" }
"
" Region Start mark
if !exists("g:CodeCompl_RegionStart")
    let g:CodeCompl_RegionStart = '`<'
endif

" Region End mark
if !exists("g:CodeCompl_RegionEnd")
    let g:CodeCompl_RegionEnd = '>`'
endif

" Make Regions use marks above
function! MakeRegion(text)
    return g:CodeCompl_RegionStart . a:text . g:CodeCompl_RegionEnd
endfunction

" [Get converted file name like __THIS_FILE__ ]
function! GetFileName()
    let filename = toupper(expand('%:t'))
    let name = substitute(filename,'\.','_',"g")
    let name = "__".name."__"
    return name
endfunction

" this is a default region, you must update it manually
let DefaultRegion = MakeRegion('...')

" }}}
" Fore-char for doing file complete {{{2
" if you input a Fore-char, code_complete will first try
" file complete, if failed code_complete will remain
" fore-char as it is. e.g. if you have a template file named
" std.c, and you type '#std<A-D>' in Vim(as default), it
" will be like this:
" #include <stdio.h>
"
" int main(void)
" {
"     |`<Input Code Here>`
"     return 0;
" }
" the cursor will be at the postion of '|', it is the same
" as you define this template-dictionary-list in complete
" file.
" if code_complete can't find the complate file, it will
" find keyword in complete file, so if you don't have a
" std.c file, but you have 'std' item in your complete file
" ('std':'/* this is a std text */'), when you type
" '#std<A-D>', it will be changed:
" #/* this is a std text */
" the fore-char '#' will be remain there.
if !exists('g:CodeCompl_Template_Forechar')
    let g:CodeCompl_Template_Forechar = '#'
endif


" }}}
" Modify when shall code_complete update dicts {{{2
" decide whether code_complete update everything when edit a
" new buffer and change the filetype.
"       0 means use old value
"       1 means update the template filelist and complete dict
"           when edit a new file or change filetype
if !exists('g:CodeCompl_UpdateAlways')
    let g:CodeCompl_UpdateAlways = 1
endif

" }}}
" Modify when update dict, shall code_complete delete {{{2
" current dicts decide whether code_complete should delete
" old dict when update, e.g. open g:CodeCompl_UpdateAlways
" and change your file type.
"       0 means don't clean current dict
"       1 means clear all dict item, and reload immediately
if !exists('g:CodeCompl_ClearWhenUpdate')
    let g:CodeCompl_ClearWhenUpdate = 1
endif

" }}}
" Define where to save template file names and complete lists {{{2
" this decide where code_complete save the template list:
"       0 means save globe template list
"       1 means save buffer template list, and read file
"           when edit a new file or change filetype
if !exists('g:CodeCompl_SaveListInBuffer')
    let g:CodeCompl_SaveListInBuffer = 0
endif

" }}}
" Default complete file {{{2
" this tell code_complete where to find the templete file.
" it must be in &runtimepath, or a subfolder in it.
" you can use UpdateTemplate to re-read this file.
if !exists("g:CodeCompl_Complete_File")
    let g:CodeCompl_Complete_File = "plugin/my_snippets.template"
endif

" }}}
" Default template folder {{{2
" this tell code_complete where to find the template folder
" a template folder is a folder, every file is a template for
" special filetype, e.g. std.c is a template named 'std', and
" for c file, std.cpp is a template named 'std', buf for cpp 
" file. the folder must under the &runtimepath, or the subfolder
" of it.
" you can use UpdateTemplate to update it
if !exists("g:CodeCompl_Template_Folder")
    let g:CodeCompl_Template_Folder = "templates"
endif

" }}}

" }}}
" Function Definations {{{1

" Start the code_complete, do some initialization work {{{2
"
function! <SID>CodeCompleteStart()
    " define autocommands
    augroup code_complete
        au!

        autocmd BufReadPost,BufNewFile * StartCodeComplele
        if g:CodeCompl_UpdateAlways
            autocmd FileType * UpdateTemplate
        endif
    augroup END

    " define the key maps use g:CodeCompl_Hotkey
    exec 'nnoremap <silent><buffer> '.g:CodeCompl_Hotkey.
                \ " :exec 'silent! normal '.<SID>SwitchRegion()<cr>"
    exec 'inoremap <silent><buffer> '.g:CodeCompl_Hotkey.
                \ ' <c-r>=<SID>CodeComplete()<cr>'.
                \ '<c-r>=<SID>SwitchRegion()<cr>'
    exec 'smap <silent><buffer> '.g:CodeCompl_Hotkey.
                \ ' <esc>'.g:CodeCompl_Hotkey


    " -----------------------------------
    " Some Inner variables

    " control the action of function SwitchRegion()
    " -1 means don't jump to anywhere, and -2 means do nothing
    let s:jumppos = -1
    
    
    " update template and complete file, if needed
    if g:CodeCompl_UpdateAlways
        call <SID>TemplateUpdate()
    endif
endfunction
" }}}
" End code, some clean work in here {{{2
"
function! <SID>CodeCompleteStop()
    " delete autocmds
    augroup code_complete
    au!
    augroup END
    " unmap all keys
    for ch in ['i', 's', 'n']
        exec 'silent! '.ch.'unmap <buffer> '.g:CodeCompl_Hotkey
    endfor

    " release some dictionary
        unlet! b:template_FileList
        unlet! b:template_Snippets
endfunction
" }}}
" Update template or complete file {{{2
"
function! <SID>TemplateUpdate()
    " init dictionarys
    if g:CodeCompl_ClearWhenUpdate || !exists('b:complete_FileList')
        let b:complete_FileList = {}
    endif
    if g:CodeCompl_ClearWhenUpdate || !exists('b:complete_Snippets')
        let b:complete_Snippets = {}
    endif

    " search for template file list
    let flist = split(globpath(&rtp, g:CodeCompl_Template_Folder.'/*'),
                \ "\<NL>")
    let ft_pat = empty(&ft) ? '^[^.]*$' : '^[^.]\+\%(\.'.&ft.'\)\=$'
    for fname in filter(flist, "v:val =~ '".ft_pat."'")
        let b:complete_FileList[fnamemodify(fname, ':t:r')] = fname
    endfor

    " call the template defined file
    exec "runtime ".g:CodeCompl_Complete_File
    silent! call Set_complete_type_COMMON(b:complete_Snippets,
                \  b:complete_FileList)
    if !empty(&ft)
        silent! call Set_complete_type_{&ft}(b:complete_Snippets,
                    \ b:complete_FileList)
    endif

    " delete all function in complete file
    redir => func_list
    silent function /^Set_complete_type_.*$/
    redir END
    for func in split(func_list, "\<NL>")
        exec 'delfunction '.matchstr(func,
                    \ '^function \zsSet_complete_type_[^(]\+\ze')
    endfor
endfunction
" }}}
" Edit the Snippet file {{{
"
function! <SID>SnippetFileEdit()
    let fname = globpath(&rtp, g:CodeCompl_Complete_File)
    let second_file = stridx(fname, "\<NL>")
    if second_file != -1
        let fname = fname[:second_file - 1]
    endif
    let fname = escape(fname, ' |')
    if &mod || line('$') != 1 || getline(1) != ''
        exec 'new ' . fname
    else
        exec 'edit ' . fname
    endif
endfunction
" }}}
" Save current file to template folder {{{
"
function! <SID>TemplateFileSave(bang)
    let tdir = globpath(&rtp, g:CodeCompl_Template_Folder)
    let second_dir = stridx(tdir, "\<NL>")
    if second_dir != -1
        let tdir = tdir[:second_file - 1]
    endif
    if empty(&ft)
        exec 'write'.a:bang.' '.tdir.'/'.exoand('%:t:r')
    else
        exec 'write'.a:bang.' '.tdir.'/'.expand('%:t:r').'.'.&ft
    endif
    let b:complete_FileList[expand('%:t:r')] = expand('%:p')
endfunction
" }}}
" Complete, use commplete dict {{{2
" find the word in complete dictionary
function! s:SnippetsComplete(cword)
    let value = get(b:complete_Snippets, a:cword, '')
    if !empty(value)
        let s:jumppos = line('.')
    endif
    return value
endfunction
" }}}
" Complete template file {{{2
" this function can have two argument, the complete-word, and the len
" of word.  e.g. if you input '#   stdc   ', the word len will be
" 12(but it will not send, because you input a template-forechar'#').
" and if you input '      stdc  ', the word len will be 6, space
" before stdc will be ignore.
" this function will delete current line(if you input a forechar), or
" delete the complete-word(if don't input a forechar), if can find the
" template, function return 0, and if it failed, it return 1.
function! s:TemplateComplete(cword)
    let fname = get(b:complete_FileList, a:cword, '')

    if empty(fname)
        return 1
    endif
    
    let s:jumppos = -1
    let line = line('.')
    let tlist = readfile(fname)
    call setline(line, tlist[0])
    call append(line, tlist[1:])
    call cursor(line, 1)
endfunction
" }}}
" Some process to signature {{{2
"
function! s:ProcessSignature(sig)
    let res = g:CodeCompl_RegionStart
    let level = 0
    for ch in split(substitute(a:sig[1:-2],'\s*,\s*',',','g'), '\zs')
        if ch == ','
            if level != 0
                let res .= ', '
            else
                let res .= g:CodeCompl_RegionEnd.', '.g:CodeCompl_RegionStart
            endif
        else
            let res .= ch
            let level += (ch == '(' ? 1 : (ch == ')' ? -1 : 0 ))
        endif
    endfor
    return res.g:CodeCompl_RegionEnd.')'.MakeRegion(';')
endfunction
" }}}
" Complete function argument list {{{2
" 
function! s:FunctionComplete(fun)
    let sig_list = []
    let sig_word = {}
    let ftags = taglist('^'.a:fun.'$')

    " if can't find the function
    if type(ftags) == type(0) || empty(ftags)
        return ''
    endif

    for item in ftags
        " item must have keys kind, name and signature, and must be the
        " type of p(declare) or f(defination), function name must be same
        " with a:fun, and must have param-list
        " if any reason can't satisfy, to next iteration
        if !has_key(item, 'kind') || (item.kind != 'p' && item.kind != 'f')
                    \ || !has_key(item, 'name') || item.name != a:fun
                    \ || !has_key(item, 'signature')
                    \ || match(item.signature, '(\s*\%(void\)\=\s*)') >= 0
            continue
        endif
        let sig = s:ProcessSignature(item.signature)
        if !has_key(sig_word, sig)
            let sig_word[sig] = 0
            let sig_list += [{'word': sig, 'menu': item.filename}]
        endif
    endfor

    " only one list find, that is we need!
    if len(sig_list) == 1
        let s:jumppos = line('.')
        return sig_list[0].word
    endif

    let s:jumppos = -2

    " can't find the argument-list, means it's a void function
    if empty(sig_list)
        return ')'
    endif

    " make a complete menu
    call complete(col('.'), sig_list)
    " tell SwitchRegion do nothing
    return ''
endfunction
" }}}
" Switch the Region to edit {{{2
"
function! <SID>SwitchRegion()
    " sometimes we don't do anything...
    if s:jumppos == -2
        let s:jumppos = -1
        return ''
    endif
    let flags = ''
    let c_pos = getpos('.')

    " if call Complete function once, set cursor to that line.
    if s:jumppos != -1
        call cursor(s:jumppos, 1)
        let flags = 'c'
        let s:jumppos = -1
    endif

    " return empty string when can't find the token IN SCREEN
    " and around 100 line of file.
    let token = MakeRegion('.\{-}')
    if search(token, flags, line('w$') + 100) == 0
        call cursor(line('w0') > 100 ? line('w0') - 100 : 1, 1)
        if search(token, '', c_pos[1]) == 0
            call setpos('.', c_pos)
            return ''
        endif
    endif

    call search(g:CodeCompl_RegionStart, 'c')
    exec "normal \<c-\>\<c-n>v"
    call search(g:CodeCompl_RegionEnd,'e',line('.'))
    if &selection == "exclusive"
        exec "normal " . "\<right>"
    endif
    return "\<c-\>\<c-n>gvo\<c-g>"
endfunction
" }}}
" Complete function, called each sub-function {{{
"
function! <SID>CodeComplete()
    " fore-char
    let fc = g:CodeCompl_Template_Forechar
    " current-line
    let c_line = getline('.')[:col('.') - 2]
    " get the template name of function name
    let plist = matchlist(c_line, '\('.fc.'\s*\)\=\(\w*\)\s*\((\)\=\s*$')
    
    " if it's empty or is a template name
    if empty(plist) || empty(plist[2]) 
                \ || (!empty(plist[1]) && !s:TemplateComplete(plist[2]))
        return ''
    endif

    " else, if it's a function name
    if !empty(plist[3]) 
        return s:FunctionComplete(plist[2])
    endif

    " if can't find as a template, we find it with
    " complete-file. and if it can't find else, we find it
    " in template(if we don't do it before), all above
    " operations, if we can't find it at last, return empty
    " string.
    if !exists('result') || empty(result)
        let result = s:SnippetsComplete(plist[2])
        if !empty(result)
            return "\<c-w>".result
        endif
        if empty(plist[1]) && !s:TemplateComplete(plist[2])
            return ''
        endif
    endif

    return g:CodeCompl_Hotkey ==? "<tab>" ? "\<tab>" : ''
endfunction
" }}}

" }}}
" ------------------------------------------------------------------------
" Some Initialization works {{{
"
" Load template settings
if !g:CodeCompl_UpdateAlways
    UpdateTemplate
endif

" ensure the start function must be called when start vim
StartCodeComplele

" }}}
" vim: ft=vim:ff=unix:fdm=marker:co=80:tw=70:sts=4:ts=4:sw=4:et:sta
plugin\my_snippets.template	[[[1
117
"=============================================================================
" File:             my_snippets.template
" Brief:            the complete file of the code_complete.vim script
" Original Author:  Mingbai <mbbill AT gmail DOT com> 
" Maintainer:       StarWing <weasley_wx AT qq DOT com>
" Last Change:      2008-11-07 18:22:31
" Version:          2.9
" Example: {{{1
"           some new file type, e.g. 'newft'
"           first define a function named
"           Set_complete_type_newft, the second argument is
"           template file-list of newft, create by
"           code_complete automatically
"
"           function! Set_complete_type_newft(dict, ...)
"               " then, add filetype-spec complete list in
"               this function
"               let a:dict['hello'] = "Hello, ".
"                           \ MakeRegion('your name')." !"
"           endfunction
"
"           you can use some redefined helper function and
"           variables.  e.g. let a:dict['cool'] = "Oh!! it's
"           ".s:sr('cool')."!!!\<cr>" \<cr> means 'enter' or
"           'return' button on your keyboard
"
"           you can add common template with adding items
"           into the function Set_complete_type_COMMON, it
"           will be called each UpdateTemplate command
"           called.
" }}}
" ============================================================================
" Must have loaded code_complete {{{1
if !exists('g:loaded_code_complete')
    finish
endif
" }}}
" ----------------------------------------------------------------------------
" some helper {{{1

if !exists('s:sr')
    let s:sr = function('MakeRegion')
endif
let s:dr = s:sr('...')

let g:CodeCompl_complete_type_lists = ['COMMON', 'c', 'cpp', 'vim']

" }}}
" set common Complete {{{1

function! Set_complete_type_COMMON(dict, ...)
    let a:dict['dt'] = "\<c-r>=strftime('%Y-%m-%d')\<cr>"
    let a:dict['xt'] = "\<c-r>=strftime('%Y-%m-%d %H:%M:%S')\<cr>"
endfunction

" }}}
" set the template for C and C++ {{{1

function! Set_complete_type_c(dict, ...)
    let a:dict['df'] = "#define "
    let a:dict['ic'] = "#include \"\"\<left>"
    let a:dict['ii'] = "#include <>\<left>"
    let a:dict['ff'] = "#ifndef \<c-r>=GetFileName()\<cr>".
                \ "\<CR>#define \<c-r>=GetFileName()\<cr>".
                \ repeat("\<cr>",5)."#endif  /* \<c-r>=GetFileName()".
                \ "\<cr>*/\<cr>".repeat("\<up>",4)

    let a:dict['co'] = "/*  */\<left>\<left>\<left>"
    let a:dict['cc'] = "/**<  */\<left>\<left>\<left>"
    let a:dict['cb'] = "/*********************************************\<cr>"
    let a:dict['ce'] = "<BS>*******************************************/"

    let a:dict['bc'] = "/***********************************".
                \ "***********************\\\<cr>".s:sr('Commets').
                \ "\<cr>\<bs>\<bs>\<bs>\\************************".
                \ "**********************************/\<cr>"

    let a:dict['main'] = "int main(int argc, char \*argv\[\])".
                \ "\<cr>{\<cr>".s:sr('Write Code Here!')."\<cr>\<bs>return 0;".
                \ "\<cr>}\<cr>"

    let a:dict['if'] = "if (".s:dr.")\<cr>{\<cr>".s:dr."\<cr>}\<cr>".s:dr
    let a:dict['else'] = "else\<cr>{\<cr>".s:dr."\<cr>}\<cr>".s:dr
    let a:dict['while'] = "while (".s:dr.")\<cr>{\<cr>".s:dr."\<cr>}\<cr>".s:dr
    let a:dict['do'] = "do\<cr>{\<cr>".s:dr."\<cr>}\<cr>while (".
                \ s:dr.");\<cr>".s:dr
    let a:dict['for'] = "for (".s:dr."; ".s:dr."; ".s:dr.")\<cr>{\<cr>".
                \ s:dr."\<cr>}\<cr>".s:dr
    let a:dict['case'] = "case ".s:dr.":\<cr>".s:dr."\<cr>\<bs>break;".
                \ "\<cr>\<bs>".s:dr
    let a:dict['switch'] = "switch (".s:dr.")\<cr>{\<cr>\<bs>".a:dict['case'].
                \ "\<cr>\<bs>default:\<cr>".s:dr."\<cr>}\<cr>".s:dr
    let a:dict['struct'] = "struct ".s:dr."\<cr>{\<cr>".s:dr."\<cr>}".s:sr(';')
endfunction

function! Set_complete_type_cpp(dict, fdict)
	call Set_complete_type_c(a:dict)

    " add C templates
    for fname in split(globpath(&rtp, g:CodeCompl_Template_Folder.'/*.c'))
        let l:key = fnamemodify(fname, ':t:r')
        if !has_key(a:fdict, l:key)
            let a:fdict[l:key] = fname
        endif
    endfor
endfunction

" }}}
" set the template for vim script file {{{1

function! Set_complete_type_vim(dict, ...)
    let a:dict['func'] = "\" ".s:dr." \{\{\{\<cr>\<cr>\<bs>\<bs>function! ".
                \ s:dr."\<cr>".s:dr."\<cr>endfunction\<cr>\" \}\}\}"
endfunction

" }}}
" vim: ft=vim:fdm=marker:co=84:ts=4:sts=4:sw=4:nu:et:sta:ai
templates\fheader.c	[[[1
6
/*********************************************
 * File Name:   `<File Name>`
 * Author:      `<Author>`
 * Version:     `<Version>`
 * Last Chage:  `<Date, you can use 'xt'>`
 ********************************************/
templates\stdc.c	[[[1
8
#include <stdio.h>
#include <stdlib.h>

int main(void)
{
    `<Input Code Here!>`
    return 0;
}
templates\stdc2.c	[[[1
9
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
    `<Input Code Here1>`
    return 0;
}

templates\stdcpp.cpp	[[[1
9
#include <iostream>
using namespace std;

int main(void)
{
    `<Input Code Here!>`
    return 0;
}

templates\stdcpp2.cpp	[[[1
10
#include <iostream>
using namespace std;

int main(int argc, char **argv)
{
    `<Input Code Here!>`
    return 0;
}


templates\stdwin.c	[[[1
50
#define _WIN32_WINNT 0x0501
#include <Windows.h>

char *strClassName = "My Class";

LRESULT CALLBACK WndProc(HWND hWnd, UINT uMsg,
                         WPARAM wParam, LPARAM lParam)
{
    switch (uMsg)
    {
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
    WNDCLASS wc =
    {
        CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS,
        WndProc, 0, 0, hInst,
        LoadIcon(NULL, IDI_APPLICATION), LoadCursor(NULL, IDC_ARROW),
        (HBRUSH)GetStockObject(WHITE_BRUSH), NULL, strClassName
    };
    if (!RegisterClass(&wc))
        return 1;
    hWnd = CreateWindowEx(0,
                          strClassName, "My Window",
                          WS_OVERLAPPEDWINDOW,
                          CW_USEDEFAULT, CW_USEDEFAULT, 320, 240,
                          NULL, NULL, hInst, NULL);
    if (!IsWindow(hWnd))
        return 2;

    ShowWindow(hWnd, nCmdShow);
    UpdateWindow(hWnd);
    while (GetMessage(&msg, NULL, 0, 0))
    {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }
    return msg.wParam;
}

/* cc_flags = -mwindows -static */
