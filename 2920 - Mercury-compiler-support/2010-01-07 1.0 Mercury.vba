" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/mercury.txt	[[[1
78
*Mercury.txt*               Mercury Compiler Support Plugins
                            LastChange: 07-Jan-2009
                            Author: Sergey Khorev <sergey.khorev@gmail.com>

1. Overview                                         |mercury-overview|
2. Installation                                     |mercury-installation|
3. Filetype plugin                                  |mercury-ftplugin|
4. Mercury commands plugin                          |mercury-commands|
5. Syntax highlighting                              |mercury-syntax|
6. Customisation                                    |mercury-customisation|

=============================================================================
1. Overview                                            *mercury-overview*

This is a set of Vim plugins to simplify work with Mercury language compiler
(http://www.mercury.csse.unimelb.edu.au)

Native Mercury installer for Windows can be found here: 
http://code.google.com/p/winmercury

=============================================================================
2. Installation                                         *mercury-install*

The plugins com as a vimball; to install it, simply >
       vim Mercury.vba
       :so %
       :q
<
Your .vimrc needs to have: >
   	set nocp
	filetype plugin on
<

=============================================================================
3. Filetype plugin                                      *mercury-ftplugin*

It is a modified version of |ftplugin| from officical Mercury distribution
It allows using |:make| to invoke mmake and sets some formatting options.
If you like keeping *.err files open in Vim, the plugin also removes "file
changed" prompt for them.

=============================================================================
4. Mercury commands plugin              *mercury-commands* *mercury_commands*

mercury_commands |plugin| provides additional functionality via Vim commands:
                                                                    *:Mmc*
:Mmc [arg1]...      Invoke compiler (mmc), using current file when no
                    arguments passed.
                                                                    *:Mmake*
:Mmake [arg1]...    Invoke mmake (mmake.bat on Windows).
                                                                    *:Mmerr*
:Mmerr              Show last output from :Mmc or :Mmake.
                                                                    *:Mmod*
:Mmod name [interface_imports]
:Mmod name {interfaceimports} [implementation_imports]
                    Insert module template, interface_imports and
                    implementation_imports are comma-separated lists
                    of modules. E.g., >
        :Mmod mymodule char,list io,store
<
                                                                    *:Mmain*
:Mmain name [interface_imports]
:Mmain name {interfaceimports} [implementation_imports]
                    Insert module template containing the `main' predicate


=============================================================================
5. Syntax highlighting                                      *mercury-syntax*

That is |syntax-highlighting| plugin from official Mercury distribution.

=============================================================================
6. Customisation                                      *mercury-customisation*

If you have several compilers installed, you can set g:mercury_home or 
b:mercury_home variable to point to specific installation directory

 vim:tw=78:ts=8:ft=help:norl:
ftdetect/mercury.vim	[[[1
6
" Filetype detection plugin
" Language: Mercury
" Last Change:	2010 Jan 06
" Maintainer:	Sergey Khorev <sergey.khorev@gmail.com>

au BufRead,BufNewFile  *.m,*.moo         set filetype=mercury
ftplugin/mercury.vim	[[[1
43
" Vim filetype plugin
" Language:     Mercury
" Last Change:	2010 Jan 06
" Maintainer:  Sergey Khorev <sergey.khorev@gmail.com>
" Based on work of Ralph Becket <rafe@cs.mu.oz.au>
" vim: ts=2 sw=2 et

if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

" I find it handy to run `mtags' over the Mercury library .m files
" and move the resulting tags file to `$HOME/mercury/tags.library'.
setlocal tags+=$HOME/mercury/tags.library,$HOME/mercury/tags.compiler

" Handy if you use `:make'.
if has('win32')
  if exists('b:mercury_home')
    let s:mercury_home = b:mercury_home
  elseif exists('g:mercury_home')
    let s:mercury_home = g:mercury_home
  else
    " @ MERCURY_HOME @ will be substituted with a real path during installation
    let s:mercury_home = "@MERCURY_HOME@"
    if s:mercury_home[0] == "@"
      echoerr "Mercury home is not set"
    endif
  endif
  let &l:makeprg = s:mercury_home . '/bin/mmake.bat'
  unlet s:mercury_home
else
  setlocal makeprg=mmake
endif
setlocal errorformat&

" Reload any .err buffers silently
autocmd! FileChangedShell *.err vi!

" Formatting options
setlocal formatoptions=trcq
setlocal wrapmargin=0 textwidth=0
setlocal fileformat=unix
plugin/mercury_commands.vim	[[[1
297
" Commands for Mercury compiler (http://www.mercury.csse.unimelb.edu.au)
" Native Mercury installer for Windows: http://code.google.com/p/winmercury
" Last Change:	2010 Jan 06
" Maintainer:  Sergey Khorev <sergey.khorev@gmail.com>
" vim: ts=8:sw=2:sts=2:

" EXPORTED COMMANDS:
" :Mmc arg1...	
"     - invoke compiler (mmc), use current file when no arguments passed
" :Mmake arg1...
"     - invoke mmake (mmake.bat on Windows)
" :Mmerr 
"     - show last output from :Mmc or :Mmake
" :Mmod name [interface_imports [implementation_imports]]
"     - insert module template, *imports are comma-separated lists of modules
" :Mmain name [interface_imports [implementation_imports]]
"     - insert module template with the `main' predicate
"
" CUSTOMISATION:
" g:mercury_home or b:mercury_home - Mercury installation directory

let s:save_cpo = &cpo
set cpo&vim

if exists("loaded_mercury_commands")
  finish
endif
let loaded_mercury_commands = 1

command! -nargs=* Mmc call s:CallMmc(<q-args>)
command! -nargs=* Mmake call s:CallMmake(s:PasteArgs(<f-args>))
command! Mmerr echo s:LastOutput
command! -nargs=+ Mmod call s:Module(0, <f-args>)
command! -nargs=+ Mmain call s:Module(1, <f-args>)

function! s:CallMmc(args)
  if a:args == ''
    let l:args = expand('%')
  else
    let l:args = a:args
  endif
  if l:args == ''
    echoerr 'No arguments passed to mmc and current buffer has no filename'
    return
  endif
  let l:Mmc = s:GetMmc()
  call s:InvokeMercury(1, l:Mmc, l:args)
  call s:OpenIfErrors(l:Mmc)
endfunction

function! s:CallMmake(args)
  let l:Mmake = s:GetMmake()
  call s:InvokeMercury(0, l:Mmake, a:args)
  " no errors in *.err but there is some message
  call s:ConsolidateErrors()
  call s:OpenIfErrors(l:Mmake)
endfunction

function! s:InvokeMercury(qfoutput, util, args)
  " the only known for me way to be completely silent
  let s:LastOutput = system(a:util . ' ' . a:args)
  if &verbose > 0
    echo 'Invoked' a:util 'with output' s:LastOutput
  endif
  " put output to the quickfix error file
  if a:qfoutput
    exec 'silent redir! >' &ef '| silent echon s:LastOutput | redir END'
  endif
endfunction

" consolidate all .err files from s:LastOutput
function! s:ConsolidateErrors()
  call delete(&ef)
  let pat = '\<\f\+\.err\>'
  let tail = s:LastOutput
  let i = match(tail, pat)
  exec 'silent new' &ef
  while i >= 0
    let tail = strpart(tail, i)
    let e = stridx(tail, '.err')
    let file = strpart(tail, 0, e + 4)
    if filereadable(file) && getfsize(file) > 0
      normal G
      exec 'silent .-1r' file
    endif
    " next iteration
    let tail = strpart(tail, e + 4)
    let i = match(tail, pat)
  endwhile
  silent w
  silent bw
  if v:shell_error
    exec 'silent redir >>' &ef '| silent echon s:LastOutput | redir END'
  endif
endfunction

function! s:PrependMercuryHome(dir, file)
  if has('win32')
    let l:suffix = '.bat'
  else
    let l:suffix = ''
  endif

  let l:mercury_home = ''

  if exists('b:mercury_home')
    let l:mercury_home = b:mercury_home
  elseif exists('g:mercury_home')
    let l:mercury_home = g:mercury_home
  elseif has('win32')
    " @ MERCURY_HOME @ will be patched with the real path by the installer
    let l:mercury_home = "@MERCURY_HOME@"
    " cannot work without MERCURY_HOME on Windows
    if l:mercury_home[0] == "@"
      echoerr "Mercury home is not set"
    endif
  endif

  if l:mercury_home != ''
    return l:mercury_home . '/' . a:dir . '/' . a:file . l:suffix
  else
    return a:file . l:suffix
  endif
endfunction

function! s:GetMmc()
  return s:PrependMercuryHome('bin', 'mmc')
endfunction

function! s:GetMmake()
  return s:PrependMercuryHome('bin', 'mmake')
endfunction

" INTERNAL
  " on win32 cmd seems to substitute "NAME=VALUE" with "NAME VALUE"
  " or is that a zsh, anyway let's quote
if has('win32')
  let s:QuoteChar = '"'
else
  let s:QuoteChar = ''
endif

" just to not bother with passing it everywhere
let s:LastOutput = ''
" the max length of the message to show with echoerr
let s:MaxSensibleLength = 200

" load errors into quickfix and jump to the first one
function! s:OpenIfErrors(util)
  if !filereadable(&ef)
    " No error file but there is some output
    if v:shell_error
      echoerr 'Error invoking' a:util ':' s:LastOutput
    endif
    return
  endif
  
  if v:version >= 700
    let l:found = 0
    try
      cget
      let l:nr = 0
      for err in getqflist()
	let l:nr += 1 "keep current index
	" skip invalid lines and warnings
	if err.valid && err.lnum > 0 && filereadable(bufname(err.bufnr))
			\  && ((stridx(err.text, 'error') >= 0 
			\ || stridx(err.text, 'Error') >= 0))
	  let l:found = 1
	  exec 'cc' l:nr
	  break
	endif
      endfor
    catch /^Vim\%((\a\+)\)\=:E42/	" E42: No errors
    endtry
    " no sensible items in the quickfix list
    if v:shell_error && !l:found
      call s:ErrorMessage(a:util)
    endif
  else
    cf
  endif
endfunction

function! s:ErrorMessage(util)
  if strlen(s:LastOutput) > 0
    echoerr 'Error invoking' a:util ':' strpart(s:LastOutput, 0, s:MaxSensibleLength)
  endif
endfunction

function! s:PasteArgs(...)
  let i = 1
  let args = ''
  while i <= a:0
    let args = args . ' ' . s:QuoteChar . a:{i} . s:QuoteChar
    let i += 1
  endwhile
  return args
endfunction

function! s:ModuleHeader(main, name, imports)
  insert  
%------------------------------------------------------------------------------%
.
  call append(line('.'), '% ' . a:name . '.m')
  normal j
  append
% vim: ft=mercury:ff=unix:ts=8:sw=4:sts=4:
%------------------------------------------------------------------------------%

.
  call append(line('.') , ':- module ' . a:name . '.')
  normal j
  append

:- interface.

.
  if a:imports != ''
    call append(line('.') , ':- import_module ' . a:imports . '.')
    normal j
    if !a:main
      append

.
    endif
  endif
  if a:main
    append
:- import_module io.

:- pred main(io.state::di, io.state::uo) is det.

.
  endif
endfunction

function! s:ModuleBody(main, imports)
  append
%------------------------------------------------------------------------------%

:- implementation.

.
  if a:imports != ''
    call append(line('.') , ':- import_module ' . a:imports . '.')
  else
    call append(line('.') , '%:- import_module .')
  endif
  normal j
  if a:main
    append

main(!IO) :- 
    io__write_string("Hello!\n", !IO).
.
  else
    append


.
  endif
endfunction

" insert spaces after commas in import list
function! s:BeautifyImports(imports)
  return substitute(a:imports, ',\(\w\)', ', \1','g')
endfunction

function! s:Module(main, name, ...)
  if a:0 > 2
    echoerr 'Too many arguments'
    return
  endif

  if filereadable(a:name . '.m')
    echoerr 'File' a:name .'.m already exists'
    return
  endif

  if a:0 > 0
    let l:int_imports = s:BeautifyImports(a:1)
  else
    let l:int_imports = ''
  endif
  if a:0 > 1
    let l:imp_imports = s:BeautifyImports(a:2)
  else
    let l:imp_imports = ''
  endif
  exec 'new' a:name . '.m'
  set ft=mercury ff=unix ts=8 sw=4 sts=4
  call s:ModuleHeader(a:main, a:name, l:int_imports)
  call s:ModuleBody(a:main, l:imp_imports)
endfunction

let &cpo = s:save_cpo
syntax/mercury.vim	[[[1
107
" Vim syntax file
" Language:     Mercury
" Maintainer:   Ralph Becket <rafe@cs.mu.oz.au>
" vim: ts=2 sw=2 et

if exists("b:current_syntax")
  finish
endif
let b:current_syntax = "mercury"

  " Mercury is case sensitive.
  "
syn case match

  " The default highlighting for Mercury comments is to only highlight the
  " initial `%' and subsequent `line' punctuation characters.  To highlight
  " everything including the comment text, add
  "
  "   let mercury_highlight_full_comment = 1
  "
  " somewhere in your `.vimrc' file.
  "
  " By default, parts of lines that extend over 80 characters will be
  " highlighted.  To avoid this behaviour, add
  "
  "   let mercury_no_highlight_overlong = 1
  "
  " somewhere in your `.vimrc' file.
  "
if exists("mercury_highlight_full_comment") && mercury_highlight_full_comment
  syn region  mercuryComment      start=+%+ end=+.*$+                                           contains=mercuryToDo
else
  syn region  mercuryComment      start=+%[-=%*_]*+ end=+.*$+he=s-1                             contains=mercuryToDo
endif
syn keyword mercuryKeyword      module use_module import_module
syn keyword mercuryKeyword      include_module end_module
syn keyword mercuryKeyword      initialise mutable
syn keyword mercuryKeyword      initialize finalize finalise
syn keyword mercuryKeyword      interface implementation
syn keyword mercuryKeyword      pred mode func type inst solver any_pred any_func
syn keyword mercuryKeyword      is semidet det nondet multi erroneous failure
syn keyword mercuryKeyword      cc_nondet cc_multi
syn keyword mercuryKeyword      typeclass instance where
syn keyword mercuryKeyword      pragma promise external
syn keyword mercuryKeyword      trace atomic or_else
syn keyword mercuryPragma       inline no_inline
syn keyword mercuryPragma       type_spec source_file fact_table obsolete
syn keyword mercuryPragma       memo loop_check minimal_model
syn keyword mercuryPragma       terminates does_not_terminate check_termination
syn keyword mercuryPragma       promise_equivalent_clauses
syn keyword mercuryCInterface   c_header_code c_code
syn keyword mercuryCInterface   foreign_proc foreign_decl foreign_code
syn keyword mercuryCInterface   foreign_type foreign_import_module
syn keyword mercuryCInterface   foreign_export_enum foreign_export
syn keyword mercuryCInterface   foreign_enum
syn keyword mercuryCInterface   may_call_mercury will_not_call_mercury
syn keyword mercuryCInterface   thread_safe not_thread_safe maybe_thread_safe
syn keyword mercuryCInterface   promise_pure promise_semipure
syn keyword mercuryCInterface   tabled_for_io local untrailed trailed 
syn keyword mercuryCInterface   attach_to_io_state 
syn keyword mercuryCInterface   can_pass_as_mercury_type stable
syn keyword mercuryCInterface   will_not_throw_exception
syn keyword mercuryCInterface   may_modify_trail will_not_modify_trail
syn keyword mercuryCInterface   may_duplicate may_not_duplicate
syn keyword mercuryCInterface   affects_liveness
syn keyword mercuryCInterface   does_not_affect_liveness doesnt_affect_liveness
syn keyword mercuryCInterface   no_sharing unknown_sharing sharing
syn keyword mercuryCInterface   export import
syn keyword mercuryImpure       impure semipure
syn keyword mercuryToDo         XXX TODO NOTE         
syn keyword mercuryLogical      some all not if then else true fail false
syn keyword mercuryLogical      try catch catch_any
syn keyword mercuryLogical      semidet_true semidet_false semidet_fail
syn keyword mercuryLogical      impure_true 
syn match   mercuryImplication  +<=>\|<=\|=>\|/\\\|\\/+
syn match   mercuryNumCode      +0'.\|0[box][0-9a-fA-F]*+
syn region  mercuryAtom         start=+'+ skip=+\\.+ end=+'+
syn region  mercuryString       start=+"+ skip=+\\.+ end=+"+                              contains=mercuryStringFmt
syn match   mercuryStringFmt    +\\[abfnrtv]\|\\x[0-9a-fA-F]*\\\|%[-+# *.0-9]*[dioxXucsfeEgGp]+                                                                           contained
syn region  mercuryClauseHead   start=+^[a-zA-Z]+ end=+=\|:-\|\.\s*$\|-->+                    contains=mercuryComment,mercuryCComment,mercuryAtom,mercuryString
syn region  mercuryCComment     start=+/\*+ end=+\*/+                                         contains=mercuryToDo
if !exists("mercury_no_highlight_overlong") || !mercury_no_highlight_overlong
  " The complicated regexp here matches an 80-column string,
  " with proper treatment of tabs (assuming the tab size is 8):
  " each row consists of 10 columns, and each column consists of either 8
  " non-tab characters, or 0-7 non-tab characters followed by a tab.
  syn match   mercuryFirst80 +^\([^	]\{8}\|[^	]\{0,7}	\)\{10}+                                contains=ALL
  syn match   mercuryTooLong +^\([^	]\{8}\|[^	]\{0,7}	\)\{10}..*+                             contains=mercuryFirst80
endif

syn sync fromstart

hi link mercuryComment          Comment
hi link mercuryCComment         Comment
hi link mercuryNumCode          Special
hi link mercuryImpure           Special
hi link mercuryKeyword          Keyword
hi link mercuryPragma           PreProc
hi link mercuryCInterface       PreProc
hi link mercuryToDo             Todo
hi link mercuryLogical          Special
hi link mercuryImplication      Special
hi link mercuryClauseHead       Statement
hi link mercuryString           String
hi link mercuryStringFmt        Special
hi link mercuryAtom             Constant
hi link mercuryTooLong          ErrorMsg
