" Vim compiler file
" Compiler:     Csound
" Maintainer:   Zamkoviy Olexiy <olexiy.z@gmail.com>
" URL:          None yet
" Last Changed: 2008 Dec 9
" Script:       http://www.vim.org/scripts/script.php?script_id=2472
" License:      WTFPL (http://sam.zoy.org/wtfpl/)
"
" Description:
"
"   Support for .orc, .csd, .sco files
"
"   This ftplugin will compile 2 csound files (orchestra file and scores
"   file ) or complete implementation of both (csd)
"
"   When compiling orc or sco files it does not matter which file you try to compile, but files must have
"   the same name and different extension
"
"   When all is compiled without errors we just listen the result
"
" Installation:
"
"   Just drop it this file in your compiler plugin folder/directory.
"
"   Use this for example to auto select compiler
"   autocmd BufNewFile,BufRead *.orc,*.sco,*.csd compiler csound
"
"   You can set variables from your .vimrc file 
"   example:
"       let g:csound_play_cmd = "bplay"
"       let g:csound_orc_ext = "orch"
"   or modify prepared compiler string to do any other thing
"
"   Actually 
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

function! InitVar(name,val)
    if exists('g:csound_'.a:name)
        exe "let b:".a:name.'="'.eval('g:csound_'.a:name).'"'
    else
        exe "let b:".a:name.'="'.a:val.'"'
    endif
endfunction

function! SetMakePrg(parts)
    exe "CompilerSet makeprg=" . escape(printf(b:compiler_string,a:parts),' ')
endfunction

call InitVar('orc_ext','orc')
call InitVar('sco_ext','sco')
call InitVar('csd_ext','csd')
call InitVar('play_cmd','aplay')

"Printf like string (%% == %)
call InitVar('compiler_string','csound -W -d -o%%:r.wav %s && '.b:play_cmd.' %%:r.wav &')

let ftype = expand('%:e')

if ftype == b:sco_ext || ftype==b:orc_ext
    call SetMakePrg('%:r.'.b:orc_ext.' %:r.'.b:sco_ext)
elseif ftype==b:csd_ext
    call SetMakePrg('%')
else
    echoerr "This file is not seems to be an Csound orchestra or scores file"
endif

let &cpo = s:cpo_save
unlet s:cpo_save
