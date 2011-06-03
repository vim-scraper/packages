" Vim compiler file
" Compiler:     Csound
" Maintainer:   Zamkoviy Olexiy <olexiy.z@gmail.com>
" URL:          None yet
" Last Change:  2008 Dec 7
"
" Description:
"
"   This ftplugin will compile 2 csound files (orchestra file and scores
"   file )
"
"   It does not matter which file you try to compile, but files must have
"   the same name and different extension
"
" Installation:
"
"   Just drop it this file in your compiler plugin folder/directory.
"
"   Use this for example to auto select compiler
"   autocmd FileType {orc,sco} compiler csound
"
" vim ts=4 : sw=4 : tw=0 et


if exists("current_compiler")
    finish
endif

let s:cpo_save = &cpo
set cpo-=C
let current_compiler = "csound"

if exists(":CompilerSet") != 2
    command -nargs=* CompilerSet setlocal <args>
endif

if !exists('orc_ext')
    let orc_ext='orc'
endif

if !exists('sco_ext')
    let sco_ext='sco'
endif

if !exists('compiler_options')
    let compiler_options="-W -d"
endif

let ftype = expand('%:e')

if ftype == sco_ext
    let scofile = expand('%')
    let orcfile = substitute(scofile,'[^.]*$',orc_ext,'')
elseif ftype==orc_ext
    let orcfile = expand('%')
    let scofile = substitute(orcfile,'[^.]*$',sco_ext,'')
else
    echoerr "This file is not seems to be an Csound orchestra or scores file"
    let s:cpo_save = &cpo
    unlet s:cpo_save
    finish
endif

exe "CompilerSet " . join(['makeprg=csound',escape(compiler_options,' '),orcfile,scofile], '\ ')

let &cpo = s:cpo_save
unlet s:cpo_save
