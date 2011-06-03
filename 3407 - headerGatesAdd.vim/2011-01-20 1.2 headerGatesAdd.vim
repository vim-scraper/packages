"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" File Name:      headerGatesAdd.vim
" Abstract:       A (G)VIM plugin which automatic inser C/C++ header gates .
" Author:         帅得不敢出门  email:tczengming at 163.com
" Version:        1.0
" Last Change:    2011.1.19

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Example {{{
" ----------------------------------------
" let g:HeaderGatesAdd_extern_c=1
" #ifdef __cplusplus
" extern "C" {
" #endif
"
"
" #ifdef __cplusplus
" }
" #endif
" ----------------------------------------
"
"-----------------------------------------
" let g:HeaderGatesAdd_gate_type=0
" aTestFile.h ---> A_TEST_FILE
" ----------------------------------------
"
"-----------------------------------------
" let g:HeaderGatesAdd_gate_type=1
" aTestFile.h ---> ATESTFILE
" ----------------------------------------
"
"-----------------------------------------
" let g:HeaderGatesAdd_gate_type=2
" aTestFile.h ---> aTestFile
" ----------------------------------------
"
"-----------------------------------------
" let g:HeaderGatesAdd_suffix="_H"
" add suffix A_TEST_FILE --> A_TEST_FILE_H
" ----------------------------------------
"
"-----------------------------------------
" let g:HeaderGatesAdd_prefix="PREFIX_"
" add suffix A_TEST_FILE_SUFFIX --> PREFIX_A_TEST_FILE_SUFFIX
" ----------------------------------------
" }}}


" Avoid reloading {{{

if exists('loaded_cinsert_header_gates')
    finish
endif

let loaded_cinsert_header_gates= 1

" }}}

" Python required {{{

if !has('python')"
    finish
endif

"}}}

if !exists('g:HeaderGatesAdd_extern_c')
    let g:HeaderGatesAdd_extern_c=0
endif

if !exists('g:HeaderGatesAdd_prefix')
    let g:HeaderGatesAdd_prefix=""
endif

if !exists('g:HeaderGatesAdd_suffix')
    let g:HeaderGatesAdd_suffix="_H"
endif

if !exists('g:HeaderGatesAdd_gate_type')
    let g:HeaderGatesAdd_gate_type=0
endif

" insertHeaderGates {{{

function! s:insertHeaderGates()
python << EOF
import vim
import re
#定位到头部说明信息（如版权，作者）的下一行
def getInsertLine():
    b=vim.current.buffer
    n = len(b)
    start = b[0][0:2]
    i=0
    if start == '//':
        i=1
        while( i<n and b[i][0:2] == '//'):
            i+=1
        if i>1:
            return i

    i=0
    line = b[i]
    while start == '/*':
        while (i<n-1 and line[len(line)-2 : len(line)] != '*/'):
            i+=1
            line = b[i]
        
        if i<n-1:
            i+=1
            line = b[i]
            start = line[0:2]
        else:
            return 0

    return i

def generateGateName(name):
    tmp = []
    type = vim.eval("g:HeaderGatesAdd_gate_type")
    end = name.rfind('.')
    
    for i,c in enumerate(name):
        if i >= end:
            break
        if type == '0': #default  aTestFile.h ---> A_TEST_FILE
            if c.isupper() and i!=0 :
                tmp.append('_')
                tmp.append(c.upper())
            elif c == ' ' or c == '.':
                tmp.append('_')
            else:
                tmp.append(c.upper())
        elif type == '1': # aTestFile.h  ---->  ATESTFILE
            if c==' ' or c == '.':
                continue
            else:
                tmp.append(c.upper())
        elif type == '2': # aTestFile.h  ----> aTestFile
            tmp.append(c)

    gatename = vim.eval("g:HeaderGatesAdd_prefix")
    gatename += "".join(tmp)
    gatename +=vim.eval("g:HeaderGatesAdd_suffix")
    return gatename

#insert header gates
def cplusHeaderGates():
    vim.command('let title=expand("%:t")')
    name=vim.eval("title")
    gatename = generateGateName(name)
    b = vim.current.buffer
    n = getInsertLine()
    b.append(["#ifndef " + gatename,
                "#define " + gatename,
                "\n"],n)

    externCflag = vim.eval("g:HeaderGatesAdd_extern_c")
    if externCflag=='1':
        b.append([ "#ifdef __cplusplus",
                    "extern \"C\" {",
                    "#endif"], n+3)

        b.append("\n")
        b.append("#ifdef __cplusplus")
        b.append("}")
        b.append("#endif")
        b.append("\n")

    b.append("#endif" + "  /*" + gatename +"*/")

cplusHeaderGates()
#generateGateName("aTest.h")
EOF
endfunction

"}}}

command! -nargs=0 HeaderGatesAdd : call <SID>insertHeaderGates()
autocmd BufNewFile *.{h,hpp} call <SID>insertHeaderGates()

