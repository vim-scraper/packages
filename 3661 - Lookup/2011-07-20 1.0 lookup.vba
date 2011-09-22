" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
autoload/lookup.vim	[[[1
300
" Author:  Eric Van Dewoestine
"
" License: {{{
"   Copyright (c) 2005 - 2011, Eric Van Dewoestine
"   All rights reserved.
"
"   Redistribution and use of this software in source and binary forms, with
"   or without modification, are permitted provided that the following
"   conditions are met:
"
"   * Redistributions of source code must retain the above
"     copyright notice, this list of conditions and the
"     following disclaimer.
"
"   * Redistributions in binary form must reproduce the above
"     copyright notice, this list of conditions and the
"     following disclaimer in the documentation and/or other
"     materials provided with the distribution.
"
"   * Neither the name of Eric Van Dewoestine nor the names of its
"     contributors may be used to endorse or promote products derived from
"     this software without specific prior written permission of
"     Eric Van Dewoestine.
"
"   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
"   IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
"   THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
"   PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
"   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
"   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
"   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
"   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
"   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
"   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
"   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
" }}}

" Global Variables {{{
if !exists("g:LookupRuntimePath")
  " possible values ('all', 'relative')
  let g:LookupRuntimePath = 'relative'
endif
if !exists("g:LookupSingleResultAction")
  " possible values ('split', 'edit', 'copen')
  let g:LookupSingleResultAction = 'edit'
endif
" }}}

" Script Variables {{{
  let s:vimdirs = '^\(autoload\|ftdetect\|ftplugin\|indent\|syntax\|plugin\)$'
  let s:keywords = {
      \ '-complete':    'command-completion',
      \ '-nargs':       'E175',
      \ '-range':       'E177',
      \ '-count':       'E177',
      \ '-bang':        'E177',
      \ '-bar':         'E177',
      \ '-buffer':      'E177',
      \ '-register':    'E177',
      \ 'silent':       ':silent',
    \ }

  let s:search = {
      \ 'aug_def': 'aug\(r\|ro\|rou\|roup\)\?!\?\s\+<element>\>',
      \ 'aug_ref': 'au\(g\|gr\|gro\|grou\|group\|t\|to\|toc\|tocm\|tocmd\)\?!\?\s\+<element>\>',
      \ 'cmd_def': 'command!\?\s.\{-}\<<element>\>',
      \ 'cmd_ref': '\<<element>\>',
      \ 'func_def': 'fu\(n\|nc\|nct\|ncti\|nctio\|nction\)\?!\?\s\+<element>\>',
      \ 'func_ref': '\<<element>\>',
      \ 'var_def': '\<let\s\+\(g:\)\?<element>\>',
      \ 'var_ref': '\<<element>\>',
    \ }

  let s:count = {
      \ 'cmd_def': '1',
      \ 'func_def': '1',
    \ }

  let s:syntax_to_help = {
      \ 'vimAutoCmd': 'autocmd',
      \ 'vimAutoEvent': '<element>',
      \ 'vimAutoGroupKey': 'augroup',
      \ 'vimCommand': ':<element>',
      \ 'vimFuncKey': ':<element>',
      \ 'vimFuncName': '<element>()',
      \ 'vimGroup': 'hl-<element>',
      \ 'vimHLGroup': 'hl-<element>',
      \ 'vimLet': ':<element>',
      \ 'vimMap': ':<element>',
      \ 'vimMapModKey': ':map-<<element>>',
      \ 'vimNotFunc': ':<element>',
      \ 'vimOper': 'expr-<element>',
      \ 'vimOption': "'<element>'",
      \ 'vimUserAttrbCmplt': ':command-completion-<element>',
      \ 'vimUserAttrbKey': ':command-<element>',
      \ 'vimUserCommand': ':<element>',
    \ }
" }}}

function! lookup#Lookup(bang) " {{{
  let line = getline('.')
  let syntax = synIDattr(synID(line('.'), col('.'), 1), 'name')

  let type = ''
  let element = substitute(
    \ line, '.\{-}\(\(<[a-zA-Z]\+>\)\?[[:alnum:]_:#]*' .
    \ '\%' . col('.') . 'c[[:alnum:]_:#]*\).*', '\1', '')

  if element =~? '^<sid>'
    let element = 's:' . element[5:]
  endif

  " on a function
  if element =~ '^[[:alnum:]#_:]\+$' &&
   \ (element =~ '^[A-Z]' || element =~ '[#:]') &&
   \ line =~ '[[:alnum:]_:#]*\%' . col('.') . 'c[[:alnum:]_:#]*\s*('
    let type = 'func'

  " on a command ref
  elseif element =~ '^:[A-Z]\w*$' ||
       \ (element =~ '^[A-Z]\w*$' && syntax == 'vimIsCommand')
    let type = 'cmd'
    if element =~ '^:'
      let element = element[1:]
    endif

  " on a variable
  elseif element =~ '^[bgsl]:\w\+$'
    let type = 'var'

  " on an augroup name
  elseif line =~ 'aug\(r\|ro\|rou\|roup\)\?!\?\s\+\w*\%' . col('.') . 'c\w*'
    let type = 'aug'

  " doc lookup
  else
    let char = line[col('.') - 1]
    if element == '' && char =~ '\W' && char !~ '\s'
      let element = substitute(line, '.\{-}\(\W*\%' . col('.') . 'c\W*\).*', '\1', '')
      let element = substitute(element, '\(^.\{-}\s\+\|\s\+.\{-}$\)', '', 'g')
    endif

    let help = get(s:syntax_to_help, syntax, '')
    if help == ''
      let base = synIDattr(synIDtrans(synID(line('.'), col('.'), 1)), 'name')
      if base == 'Statement'
        let help = ':<element>'
      endif

      " vim variables
      if element =~ '^v:'
        let help = '<element>'
      endif

      " option refrence
      if line =~ '&' . element . '\>'
        let help = s:syntax_to_help['vimOption']
      endif
    endif

    if help != ''
      exec 'help ' . substitute(help, '<element>', element, '')
      return
    endif
  endif

  if type != ''
    let def = substitute(s:search[type . '_def'], '<element>', element, '')

    " on a definition, search for references
    if line =~ def
      call s:Find(element, a:bang, type . '_ref')

    " on a reference, search for definition.
    else
      call s:Find(element, a:bang, type . '_def')
    endif
  endif
endfunction " }}}

function! s:Find(element, bang, context) " {{{
  let element = a:element
  echoh Statement | echo "Searching for '" . element . "'..." | echoh Normal

  let cnt = get(s:count, a:context, '')
  let search = substitute(s:search[a:context], '<element>', element, '')

  call setqflist([])

  let save_opt = &eventignore
  set eventignore=all
  try
    " if a script local function search current file.
    if element =~ '^[ls]:.*'
      if a:context =~ '_ref'
        let search = '\(<SID>\|s:\)' . element[2:] . '\>'
      endif
      let command = cnt . 'vimgrepadd /' . search . '/gj %'
      silent! exec command

    " search globally
    else
      for path in s:Paths()
        if isdirectory(path)
          let path .= '/**/*.vim'
        endif
        let path = escape(substitute(path, '\', '/', 'g'), ' ')
        let command = cnt . 'vimgrepadd /' . search . '/gj' . ' ' . path
        " must use silent! otherwise an error on one path may suppress finding
        " of results on subsiquent paths even w/ a try/catch (vim bug most
        " likely)
        silent! exec command
        if a:context == 'def' && len(getqflist()) > 0
          break
        endif
      endfor
    endif
  finally
    let &eventignore = save_opt
  endtry

  let qflist = getqflist()
  if len(qflist) == 0
    echoh WarningMsg | echo "No results found for '" . element . "'." | echoh Normal
  else
    if a:bang != ''
      copen
    elseif len(qflist) == 1
      if g:LookupSingleResultAction == 'edit'
        cfirst
        if foldclosed(line('.')) != -1
          foldopen!
        endif
      elseif g:LookupSingleResultAction == 'split'
        let file = bufname(qflist[0].bufnr)
        if file != expand('%')
          let winnr = bufwinnr(bufnr('^' . file))
          if winnr != -1
            exec winnr . 'winc w'
          else
            silent exec 'split ' . escape(file, ' ')
          endif
        endif
        call cursor(qflist[0].lnum, qflist[0].col)
        if foldclosed(line('.')) != -1
          foldopen!
        endif
      else
        copen
      endif
    else
      cfirst
      if foldclosed(line('.')) != -1
        foldopen!
      endif
    endif

    if exists('g:EclimSignLevel')
      for result in qflist
        let result['type'] = 'i'
      endfor
      call setqflist(qflist)
      call eclim#display#signs#Update()
    endif
  endif
endfunction " }}}

function! s:Paths() " {{{
  if g:LookupRuntimePath == 'relative'
    let file = expand('%:t')
    let path = expand('%:p:h')

    " for vimrc files, look for relative .vim or vimfiles directory
    if file =~ '^[._]g\?vimrc$'
      if isdirectory(path . '/.vim')
        return [expand('%:p'), path . '/.vim']
      elseif isdirectory(path . '/vimfiles')
        return [expand('%:p'), path . '/vimfiles']
      endif
    endif

    while fnamemodify(path, ':t') !~ s:vimdirs
      let path = fnamemodify(path, ':h')
      " we hit the root of the filesystem, so just use the file's directory
      if path == '/' || path =~ '^[a-zA-Z]:\\$'
        return [expand('%:p:h')]
      endif
    endwhile
    let path = fnamemodify(path, ':h')
    " handle vim's after directory
    if fnamemodify(path, ':t') == 'after'
      let path = fnamemodify(path, ':h')
    endif
    return [path]
  endif

  return split(&rtp, ',')
endfunction " }}}

" vim:ft=vim:fdm=marker
doc/lookup.txt	[[[1
76
*lookup.txt*

-----------------------------------------------------------------------------
Lookup                                           *lookup*

  Overview                           |lookup-overview|
  Prerequisites                      |lookup-prerequisites|
  Usage                              |lookup-usage|
  Configuration                      |lookup-configuration|

-----------------------------------------------------------------------------
Overview                                         *lookup-overview*

Lookup is a vim plugin for vim script developers which allows you to quickly
find command/function/variable definitions/references or lookup vim docs for
the element under the cursor.

-----------------------------------------------------------------------------
Prerequisites                                    *lookup-prerequisites*

Lookup is only enabled for files of file type 'vim' and requires that you have
file type plugins enabled (|:filetype-plugin-on|), as well as syntax
highlighting turned on (|:syntax-enable|).

-----------------------------------------------------------------------------
Usage                                            *lookup-usage* *:Lookup*

To use Lookup, simply place the cursor on the element you want to find and
run the :Lookup command.  If you are on the name of a command, function, or
variable definition then Lookup will find all the references and populate the
quickfix list with the results.  If you are on a reference of a command,
function, or variable, then Lookup will find the definition(s). If you are on
a vim builtin command, variable, expression, etc. then in most cases Lookup
will open the help topic for that element.

Note: Lookup relies on vim syntax highlighting to help determine context and
the type of element to lookup, so some lookups won't return any results if
syntax is disabled.

The process of locating results for your :Lookup request involves:
1. Determining the context of the element under the cursor and building an
   appropriate search pattern.
2. Determining the path(s) to search. If |g:LookupRuntimePath| is set to 'all',
   then each path in your |'runtimepath'| will be searched. However, if
   |g:LookupRuntimePath| is set to 'relative', the default, then the path to
   search is determined by walking up the directory structure starting at the
   current file's directory until a standard vim directory is found (plugin,
   ftplugin, etc.). For vimrc files, the vimrc file and either the .vim or
   vimfiles sibling directory will be searched.
3. Once the paths to search have been determined, Lookup will then create a
   new quickfix list and issue a |:vimgrepadd| on each path for the search
   pattern built in step #1.

Instead of typing the :Lookup command, you're encouraged to map it to the key
sequence of your choice inside an ftplugin file (ftplugin/vim/<foo>.vim,
ftplugin/<bar>_vim.vim). Below is an example which maps <cr> to :Lookup. The
surrounding if statement prevents <cr> from being mapped when in vim's
|command-line-window| (the regex may need to be updated for your locale): >

  if bufname('%') !~ '^\(command-line\|\[Command Line\]\)$'
    nnoremap <silent> <buffer> <cr> :Lookup<cr>
  endif
>

-----------------------------------------------------------------------------
Configuration                                    *lookup-configuration*

- *g:LookupRuntimePath* (Default: 'relative')
  Specifies whether all paths in your runtime path will be search or just the
  path which the current file resides in. Possible values include 'all' or
  'relative'.
- *g:LookupSingleResultAction* (Default: 'edit')
  Specifies what action to take when a single result is found. Possible values
  include 'split', 'edit', or 'copen'.

vim:tw=78:ft=help:norl:
ftplugin/vim/lookup.vim	[[[1
46
" Author:  Eric Van Dewoestine
"
" License: {{{
"   Copyright (c) 2005 - 2011, Eric Van Dewoestine
"   All rights reserved.
"
"   Redistribution and use of this software in source and binary forms, with
"   or without modification, are permitted provided that the following
"   conditions are met:
"
"   * Redistributions of source code must retain the above
"     copyright notice, this list of conditions and the
"     following disclaimer.
"
"   * Redistributions in binary form must reproduce the above
"     copyright notice, this list of conditions and the
"     following disclaimer in the documentation and/or other
"     materials provided with the distribution.
"
"   * Neither the name of Eric Van Dewoestine nor the names of its
"     contributors may be used to endorse or promote products derived from
"     this software without specific prior written permission of
"     Eric Van Dewoestine.
"
"   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
"   IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
"   THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
"   PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
"   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
"   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
"   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
"   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
"   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
"   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
"   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
" }}}

" Command Declarations {{{

if !exists(":Lookup") && bufname('%') !~ '^\(command-line\|\[Command Line\]\)$'
  command -buffer -bang -nargs=0 Lookup :call lookup#Lookup('<bang>')
endif

" }}}

" vim:ft=vim:fdm=marker
