" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/eteSkeleton.vim	[[[1
138
" eteSkeleton
" Autor: ellethee <ellethee@altervista.org> 
" Version: 1.0.1
" License: MIT
" Last change: 2010 Dec 14
" Copyright (c) 2010 ellethee <ellethee@altervista.org>
" License: MIT license {{{
" Permission is hereby granted, free of charge, to any person obtaining a copy
" of this software and associated documentation files (the "Software"), to deal
" in the Software without restriction, including without limitation the rights
" to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
" copies of the Software, and to permit persons to whom the Software is
" furnished to do so, subject to the following conditions:
"
" The above copyright notice and this permission notice shall be included in
" all copies or substantial portions of the Software.

" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
" IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
" FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
" AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
" LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
" OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
" THE SOFTWARE.
"}}}
if exists("g:loaded_eteSkeleton") || &cp
    finish
endif
let g:loaded_eteSkeleton = 101  
let s:save_cpo = &cpo
let s:ETES_SKELETONS = "skeleton"
let s:ETES_TAGS = globpath(&rtp,"plugin/eteSkeleton.tags")
set cpo&vim

command! -nargs=0 EteSkeleton call s:eteSkeleton_get()
command! -nargs=0 EteSkelList call s:eteSkeleton_list()
command! -nargs=1 EteSkelAdd call s:eteSkeleton_add(<q-args>)
command! -nargs=0 EteSkelAddTag call s:eteSkeleton_addtag()
command! -nargs=1 EteSkelDel call s:eteSkeleton_del(<q-args>)
command! -nargs=? EteSkelMake call s:eteSkeleton_makeskel(<q-args>)

nmap <silent> <Leader>st    :EteSkelAddTag<Enter>

fu! s:eteSkeleton_makeskel(...)
    let l:path = globpath(&rtp,"skeleton")
    if len(a:1)
        let l:name = a:1
    else
        let l:name = substitute(expand("%:r"),"type",&l:filetype,"")
    endif
    echo "Creo lo skeletro: ".l:path."/".l:name
    exec ":w! ".l:path."/".l:name
    exec ":e! ".l:path."/".l:name
endfu 
fu! s:msort(l1, l2)
    return len(a:l2) - len(a:l1)
endfu
fu! s:eteSkeleton_list()
    for riga in readfile(s:ETES_TAGS)
        echo riga
    endfor
endfu
fu! s:eteSkeleton_addtag()
    let l:cword = expand("<cWORD>")
    if len(l:cword)
        call s:eteSkeleton_add(l:cword)
    else
        echoerr "Posizionarsi su di una parola"
    endif
endfu
fu! s:eteSkeleton_add(tag)
    let l:vals = split(a:tag, "=")
    let l:vals[0]= substitute(substitute(l:vals[0],"<","",""),">","","")
    if len(l:vals) < 2
        let l:vtag =  input ("inserire il valore per il tag ".l:vals[0].": ")
        if len(l:vtag)
            call add(l:vals, l:vtag)
        else
            echoerr "Manca un valore per il tag: operazione annullata"
            return
        endif
    endif
    echo l:vals
    let l:lista = readfile(s:ETES_TAGS)
    call filter(l:lista,'v:val !~ "^' .l:vals[0].'="')
    call add(l:lista, l:vals[0]."=".l:vals[1] )
    call writefile(l:lista, s:ETES_TAGS)
endfu
fu! s:eteSkeleton_del(tag)
    let l:tag = substitute(substitute(a:tag,"<","",""),">","","")
    let l:lista = readfile(s:ETES_TAGS)
    call filter(l:lista,'v:val !~ "^'.l:tag.'="')
    call writefile(l:lista, s:ETES_TAGS)
endfu
fu! s:eteSkeleton_replace()
    for l:riga in readfile(s:ETES_TAGS)
        if l:riga[0] != '"' && len(l:riga[0]) != 0
            let l:vals = split(l:riga, "=") 
            for l:idx in range(1,line("$"))
                if getline(l:idx) =~ "<".l:vals[0].">"
                    silent! exec l:idx."s/<".l:vals[0].">/".eval(l:vals[1])
               endif
            endfor
        endif
    endfor
endfu
fu! s:eteSkeleton_get()
    let l:fname = expand("%:r")
    let l:rlist = sort(map(split(globpath(&rtp, s:ETES_SKELETONS."/*".
                \&l:filetype."*"),"\n"),'fnamemodify(v:val,":t")'), "s:msort")
    for l:item in l:rlist
        if l:fname =~ substitute(l:item, &l:filetype,"\\\\w\\\\+", "")
            let l:pfile = split(globpath(&rtp, s:ETES_SKELETONS."/".
                        \l:item))
            if len(l:pfile)
                silent keepalt 0 read `=join(l:pfile)`
                call s:eteSkeleton_replace()
            endif
            break
        endif
    endfor
    return 
endfu 
fu! s:eteSkeleton_check()
    if &l:filetype != ""
        execute "EteSkeleton" 
    endif
    return
endfu
augroup plugin-eteSkeleton
    autocmd BufNewFile * call s:eteSkeleton_check()
    autocmd BufNewFile,BufRead eteSkeleton.tags :set ft=vim
augroup END

let &cpo= s:save_cpo
unlet s:save_cpo

" vim:fdm=marker:
plugin/eteSkeleton.tags	[[[1
8
author=expand("$USER")
date=strftime("%d\\/%m\\/%Y")
filename=expand("%:.")
" chiedo a runtime un valore per per il tag e visualizzo come prompt la riga
ask=inputdialog(getline("."))
home=expand('$HOME')
askversion=inputdialog("Vesrion","1.0.0")
version="1.0.0"
doc/eteSkeleton.txt	[[[1
132
*eteSkeleton.txt*

Autor: ellethee <ellethee@altervista.org> 
Version: 1.0.1
Last change: 2010 Dec 08

Copyright (c) 2010 ellethee <ellethee@altervista.org>
License: MIT license {{{
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
}}}

CONTENTS                                              *eteSkeleton-contents* 

Introduction                                    |eteSkeleton-introduction|
Usage                                           |eteSkeleton-usage|
  Commands                                      |eteSkeleton-commands|
  Tags                                          |eteSkeleton-tags|
Bugs                                            |eteSkeleton-bugs|

==============================================================================
                                                    *eteSkeleton-introduction*

This is the first plugin I've developed; I hope you will find it useful. 
|eteSkeleton| is a simple plugin that helps you to manage your skeleton files and
handle tags. When you create a new file, |eteSkeleton| scans your skeleton 
folder for any files that are of the same filetype as the new file. 
When |eteSkeleton| finds several files that matches the filetype of the new 
file, it will compare the files and locate the one which bears a similar 
filename as the new file. As |eteSkeleton| locates such a file, it will copy 
the runtime path / skeleton of the file to the new file.

example >
        ~/.vim/skeleton/python
        ~/.vim/skeleton/test_python 

        :e test_script.py

        In this case, |eteSkeleton| will load the "test_python" skeleton file.

Once the skeleton is loaded, the tags in the "test_script.py" file will be 
replaced by the tags in eteSskeleton.tags (which are located in the plugin 
directory).

==============================================================================
USAGE                                                   *eteSkeleton-usage*

Once you've created your skeleton and placed it into the skeleton folder, just
start a new buffer.

------------------------------------------------------------------------------
COMMANDS                                                *eteSkeleton-commands*

                                                        *:EteSkeleton*
:EteSkeleton 
        This command calls to load the skeleton for the current buffer.
        |eteSkeleton| compares the filenames of the files in the skeleton 
        folder and retrieve the suitable skeleton to place in the buffer. 

                                                        *:EteSkelList*
:EteSkelList
        This command calls to show the tag list in the "eteSkeleton.tags" file.
        It allows you to check the tags. 

                                                        *:EteSkelAdd*
:EteSkelAdd {tagname[=tagvlue]}
        This is to add or replace a tag {tagname} in the tag list. If you 
        don't specify the value {=tagvalue}, |eteSkeleton| will ask you to give 
        a value.

                                                        *:EteSkelAddTag*
:EteSkelAddTag
        This command is useful when you are writing templates. Place the cursor 
        above a word, and then add or replace the word with a tag.
        It is mapped as <Leader>st.

                                                        *:EteSkelDel*
:EteSkelDel {tagname}
        This command helps you to remove {tagname} from the "eteSkeleton.tags" 
        file.

                                                        *:EteSkelMake*
:EteSkelMake [{skeletonname}]
        Save the current buffer as a skeleton (in the skeleton path) using
        {skeletonname} and reload it. If no skeleton name is provided
        eteSkeleton will use the current buffer name trying to replace the word
        'type' with the current filetype. So for the buffer 'test-type.py' it 
        will create the skeleton 'skeleton/test-python'.

------------------------------------------------------------------------------
TAGS                                                    *eteSkeleton-tags*

Tags are a simple list of tagname=tagvalue stored in the "eteSkeleton.tags" 
file (which is located in the plugin folder). It can be manually edited or 
modified by the eteSkeleton commands. Please note that the {tagname} must be 
written without the <> symbols in the tag list; however, as the {tagvalue} will 
be evaluated, you must use quotes (i.e. " ") when you write Strings.

eteSkeleton.tags example >
    author=expand("$USER")
    email="ellethee@altervista.org"
    date=strftime("%d\\/%m\\/%Y")

skeleton example >
    #! /usr/bin/python
    # -*- coding: Latin-1 -*-
    """
    :Author: <author> (<email>)
    :Date: <date>
    """

==============================================================================
BUGS                                                    *eteSkeleton-bugs*

I haven't yet tested the software thoroughly :p
==============================================================================
vim:tw=78:ts=8:ft=help:norl:fen:fdl=0:fdm=marker:
