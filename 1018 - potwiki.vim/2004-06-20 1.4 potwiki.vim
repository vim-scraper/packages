"$Id: potwiki.vim,v 1.4 2004/06/20 12:41:59 edwin Exp $
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Name:		    potwiki
" Description:	    Maintain a simple Plain Old Text Wiki
" Author:	    Edwin Steiner <edwin.steiner@gmx.net>
" Maintainer:	    -- '' --
"
" Licence:	    This program is free software; you can redistribute it
"                   and/or modify it under the terms of the GNU General Public
"                   License.  See http://www.gnu.org/copyleft/gpl.txt
"
" Credits:	    Mathieu Clabaut <mathieu.clabaut@free.fr>
"                       for the organization of the documentation and
"                       the automatic documentation installing
"                       (taken from his script vimspell.vim)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Section: Documentation 
"----------------------------
"
" Documentation should be available by ":help potwiki" command, once the
" script has been copied in you .vim/plugin directory.
"
" You still can read the documentation at the end of this file. Locate it by
" searching the "potwiki-contents" string (and set ft=help to have
" appropriate syntaxic coloration). 
"

if exists('loaded_potwiki')
  finish
endif
let loaded_potwiki = 1

let s:save_cpo = &cpo
set cpo&vim

"----------------------------------------------------------------------
" Configuration

function s:default(varname,value)
  if !exists('g:potwiki_'.a:varname)
    let g:potwiki_{a:varname} = a:value
  endif
endfunction

call s:default('home',"~/Wiki/HomePage")
call s:default('home_dir',fnamemodify(g:potwiki_home,':p:h'))

call s:default('upper','A-Z���')
call s:default('lower','a-z����')
call s:default('other','0-9_')

call s:default('autowrite',0)

call s:default('slash',has('unix') ? '/' : '\')

"----------------------------------------------------------------------
" Functions

function s:PotWikiInit()
  let upp = g:potwiki_upper
  let low = g:potwiki_lower
  let oth = g:potwiki_other
  let nup = low.oth
  let nlo = upp.oth
  let any = upp.nup

  "A PotWikiWord must start with an upper case character and contain at
  "least one lower case and another upper case character in that order.

  let s:wordrx = '\<['.upp.']['.nlo.']*['.low.']['.nup.']*['.upp.']['.any.']*\>'

  call s:PotWikiMap()
  call s:PotWikiMenu()
  call s:PotWikiAutoCommands()
endfunction

function s:PotWikiBufferInit()
  if exists('b:did_potwiki_buffer_init')
    return
  endif
  let b:did_potwiki_buffer_init = 1

  let s:potwiki_dir = expand('%:p:h')

  call s:PotWikiDefineSyntax()
  call s:PotWikiBufferMap()
endfunction

function s:PotWikiDefineSyntax()
  execute 'syntax match PotwikiWordNotFound "'.s:wordrx.'"'

  call s:PotWikiDefineWords()

  " Define the default highlighting.
  " For version 5.7 and earlier: only when not done already
  " For version 5.8 and later: only when an item doesn't have highlighting yet
  if version >= 508 || !exists("did_potwiki_syntax_inits")
    if version < 508
      let did_potwiki_syntax_inits = 1
      command -nargs=+ HiLink hi link <args>
    else
      command -nargs=+ HiLink hi def link <args>
    endif

    HiLink PotwikiWordNotFound Error
    HiLink PotwikiWord         Identifier

    delcommand HiLink
  endif
  let b:current_syntax = "potwiki"
endfunction

function s:PotWikiClearWords()
  syntax clear PotwikiWord
endfunction

function s:PotWikiDefineWords()
  let files=globpath(s:potwiki_dir,"*")
  while files != ""
    let pos = stridx(files,"\n")
    if pos < 0
      let pos = strlen(files)
    endif
    let file = strpart(files,0,pos)
    let files = strpart(files,pos+1)
    let word = matchstr(file,s:wordrx.'\%$')
    if word != "" 
      execute "syntax match PotwikiWord ".'"\<'.word.'\>"'
    endif
  endwhile
endfunction

function s:PotWikiEdit(file)
  execute "e ".a:file
  call s:PotWikiBufferInit()
endfunction

"----------------------------------------------------------------------
" Autocommands

function s:PotWikiAutoCommands()
  let dir = g:potwiki_home_dir
  if !has('unix')
    let dir = substitute(dir,'\','/','g')
  endif
  execute 'au BufNewFile,BufReadPost '.dir.'/* call <SID>PotWikiBufferInit()'
endfunction

"----------------------------------------------------------------------
" Maps

function s:PotWikiMap()
  noremap <unique> <script> <SID>Home    :call <SID>Home()<CR>
  noremap <unique> <script> <SID>Index   :call <SID>Index()<CR>
  noremap <unique> <script> <SID>CR      :call <SID>CR()<CR>
  noremap <unique> <script> <SID>Follow  :call <SID>Follow()<CR>
  noremap <unique> <script> <SID>Close   :call <SID>Close()<CR>
  noremap <unique> <script> <SID>Reload  :call <SID>Reload()<CR>
  execute "noremap <unique> <script> <SID>Edit :e ".g:potwiki_home_dir.
    \ g:potwiki_slash

  map <unique> <script> <Plug>PotwikiHome   <SID>Home
  map <unique> <script> <Plug>PotwikiIndex  <SID>Index
  map <unique> <script> <Plug>PotwikiCR     <SID>CR
  map <unique> <script> <Plug>PotwikiFollow <SID>Follow
  map <unique> <script> <Plug>PotwikiClose  <SID>Close
  map <unique> <script> <Plug>PotwikiReload <SID>Reload
  map <unique> <script> <Plug>PotwikiEdit   <SID>Edit

  if !hasmapto('<Plug>PotwikiHome')
    map <unique> <Leader>ww <Plug>PotwikiHome
  endif
  if !hasmapto('<Plug>PotwikiIndex')
    map <unique> <Leader>wi <Plug>PotwikiIndex
  endif
  if !hasmapto('<Plug>PotwikiFollow')
    map <unique> <Leader>wf <Plug>PotwikiFollow
  endif
  if !hasmapto('<Plug>PotwikiEdit')
    map <unique> <Leader>we <Plug>PotwikiEdit
  endif
endfunction

function s:PotWikiBufferMap()
  execute 'noremap <script> <buffer> <silent> <Tab> /'.s:wordrx.'<CR>'
  execute 'noremap <script> <buffer> <silent> <BS> ?'.s:wordrx.'<CR>'

  map          <buffer> <silent> <CR>             <Plug>PotwikiCR
  map <unique> <buffer> <silent> <Leader><Leader> <Plug>PotwikiClose
  map <unique> <buffer> <silent> <Leader>wr       <Plug>PotwikiReload
endfunction

"----------------------------------------------------------------------
" Menu items

function s:PotWikiMenu()
  noremenu <script> Plugin.Wiki\ Home    <SID>Home
  noremenu <script> Plugin.Wiki\ Index   <SID>Index
endfunction

"----------------------------------------------------------------------
" Functions suitable for global mapping

function s:Home()
  call s:PotWikiEdit(g:potwiki_home)
endfunction

function s:Index()
  execute "e ".g:potwiki_home_dir
"  call s:PotWikiBufferInit()
endfunction

function s:Follow()
  let word = expand('<cword>')
  if word =~ s:wordrx
    if exists('b:did_potwiki_buffer_init')
      let file = s:potwiki_dir.g:potwiki_slash.word
    else
      let file = g:potwiki_home_dir.g:potwiki_slash.word
    endif
    call s:PotWikiEdit(file)
  else
    echo "Cursor must be on a WikiWord to follow!"
  endif
endfunction

"----------------------------------------------------------------------
" Functions suitable for buffer mapping

function s:CR()
  let word = expand('<cword>')
  if word =~ s:wordrx
    let file = s:potwiki_dir.g:potwiki_slash.word
    call s:PotWikiEdit(file)
  else
    execute "normal! \n"
  endif
endfunction

function s:Close()
  if (g:potwiki_autowrite)
    execute "w"
  endif
  execute "bd"
endfunction

function s:Reload()
  call s:PotWikiClearWords()
  call s:PotWikiDefineWords()
endfunction

"----------------------------------------------------------------------
" Install documentation

" Function: s:InstallDocumentation(full_name, revision)              
"   Install help documentation.
" Arguments:
"   full_name: Full name of this vim plugin script, including path name.
"   revision:  Revision of the vim script. #version# mark in the document file
"              will be replaced with this string with 'v' prefix.
" Return:
"   1 if new document installed, 0 otherwise.
" Note: taken from vimspell.vim (see Credits) 
"'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function! s:InstallDocumentation(full_name, revision)
    " Name of the document path based on the system we use:
    if (has("unix"))
        " On UNIX like system, using forward slash:
        let l:slash_char = '/'
        let l:mkdir_cmd  = ':silent !mkdir -p '
    else
        " On M$ system, use backslash. Also mkdir syntax is different.
        " This should only work on W2K and up.
        let l:slash_char = '\'
        let l:mkdir_cmd  = ':silent !mkdir '
    endif

    let l:doc_path = l:slash_char . 'doc'
    let l:doc_home = l:slash_char . '.vim' . l:slash_char . 'doc'

    " Figure out document path based on full name of this script:
    let l:vim_plugin_path = fnamemodify(a:full_name, ':h')
    let l:vim_doc_path    = fnamemodify(a:full_name, ':h:h') . l:doc_path
    if (!(filewritable(l:vim_doc_path) == 2))
        echomsg "Doc path: " . l:vim_doc_path
        execute l:mkdir_cmd . l:vim_doc_path
        if (!(filewritable(l:vim_doc_path) == 2))
            " Try a default configuration in user home:
            let l:vim_doc_path = expand("~") . l:doc_home
            if (!(filewritable(l:vim_doc_path) == 2))
                execute l:mkdir_cmd . l:vim_doc_path
                if (!(filewritable(l:vim_doc_path) == 2))
                    " Put a warning:
                    echomsg "Unable to open documentation directory"
                    echomsg " type :help add-local-help for more informations."
                    return 0
                endif
            endif
        endif
    endif

    " Exit if we have problem to access the document directory:
    if (!isdirectory(l:vim_plugin_path)
        \ || !isdirectory(l:vim_doc_path)
        \ || filewritable(l:vim_doc_path) != 2)
        return 0
    endif

    " Full name of script and documentation file:
    let l:script_name = fnamemodify(a:full_name, ':t')
    let l:doc_name    = fnamemodify(a:full_name, ':t:r') . '.txt'
    let l:plugin_file = l:vim_plugin_path . l:slash_char . l:script_name
    let l:doc_file    = l:vim_doc_path    . l:slash_char . l:doc_name

    " Bail out if document file is still up to date:
    if (filereadable(l:doc_file)  &&
        \ getftime(l:plugin_file) < getftime(l:doc_file))
        return 0
    endif

    " Prepare window position restoring command:
    if (strlen(@%))
        let l:go_back = 'b ' . bufnr("%")
    else
        let l:go_back = 'enew!'
    endif

    " Create a new buffer & read in the plugin file (me):
    setl nomodeline
    exe 'enew!'
    exe 'r ' . l:plugin_file

    setl modeline
    let l:buf = bufnr("%")
    setl noswapfile modifiable

    norm zR
    norm gg

    " Delete from first line to a line starts with
    " === START_DOC
    1,/^=\{3,}\s\+START_DOC\C/ d

    " Delete from a line starts with
    " === END_DOC
    " to the end of the documents:
    /^=\{3,}\s\+END_DOC\C/,$ d

    " Remove fold marks:
    % s/{\{3}[1-9]/    /

    " Add modeline for help doc: the modeline string is mangled intentionally
    " to avoid it be recognized by VIM:
    call append(line('$'), '')
    call append(line('$'), ' v' . 'im:tw=78:ts=8:ft=help:norl:')

    " Replace revision:
    exe "normal :1s/#version#/ v" . a:revision . "/\<CR>"

    " Save the help document:
    exe 'w! ' . l:doc_file
    exe l:go_back
    exe 'bw ' . l:buf

    " Build help tags:
    exe 'helptags ' . l:vim_doc_path

    return 1
endfunction

let s:revision=
    \ substitute("$Revision: 1.4 $",'\$\S*: \([.0-9]\+\) \$','\1','')
silent! let s:install_status =
            \ s:InstallDocumentation(expand('<sfile>:p'), s:revision)
if (s:install_status == 1)
  echom expand("<sfile>:t:r") . ' v' . s:revision .
        \ ': Help-documentation installed.'
endif

"----------------------------------------------------------------------
" MAIN

call s:PotWikiInit() 

"----------------------------------------------------------------------

let &cpo = s:save_cpo
finish

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Section: Documentation content                                          {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
=== START_DOC
*potwiki.txt*   Plain Old Text Wiki                                 #version#


                        POTWIKI REFERENCE MANUAL~


Maintaining a Wiki of plain text files with the plugin "potwiki"


==============================================================================
CONTENT                                                     *potwiki-contents* 

    Installation        : |potwiki-install|
    potwiki intro       : |potwiki|
    Requirements        : |potwiki-requirements|
    potwiki commands    : |potwiki-commands|
    Customization       : |potwiki-customize|
    Bugs                : |potwiki-bugs|
    Todo list           : |potwiki-todo|

==============================================================================
1. potwiki Installation {{{2                                *potwiki-install*

    In order to install the plugin, place the potwiki.vim file into a plugin
    directory in your runtime path (please see |add-global-plugin| and
    |'runtimepath'|).

    |potwiki| may be customized by setting variables, creating maps, and
    specifying event handlers.  Please see |potwiki-customize| for more
    details.

                                                           *potwiki-auto-help*
    This help file is automagically generated when the |potwiki| script is
    loaded for the first time.

==============================================================================
1.1. potwiki requirements                              *potwiki-requirements*

    potwiki should work with vim 6.0 or later versions.

==============================================================================
2. potwiki intro {{{2                                              *potwiki*
                                                       *wiki*  *potwiki-intro*

   potwiki provides functions and mappings for maintaining a Wiki of plain
   text files.

   The Wiki is a collection of text files in a single directory. These files
   can contain hyper links to each other in the form of WikiWords.

   A WikiWord is a word which starts with an upper case letter and contains
   at least one lower case letter and another upper case letter in this
   order -- it's in 'camel case', e.g.

     ThisIsAWikiWord

   Such a WikiWord links to a file of exactly the same name in your
   Wiki directory.

   By default you can also use digits, underscore and German umlauts in your
   WikiWords. You can customize this to your needs. |potwiki-customize|

   When opening a Wiki file potwiki scans your Wiki directory to find
   which WikiWords are valid links. WikiWords without a corresponding
   file are highlighted as errors (otherwise it doesn't matter).

   The default mappings are defined as follow (By default, <Leader> stands
   for '\'. See |Leader| for more info) :

   <Leader>ww   - open the Wiki HomePage
   <Leader>wi   - open the Wiki index
   <Leader>wf   - follow a WikiWord (can be used in any buffer!)
   <Leader>we   - edit a Wiki file

   The following mappings are present when editing a Wiki file:

   <Leader><Leader> - close the file
   <CR>             - follow the WikiWord under the cursor
   <Tab>            - move to the next WikiWord
   <BS>             - move to the previous WikiWord
   <Leader>wr       - reload WikiWords

   See |potwiki-mappings-override| and |potwiki-options| to learn how to
   override those default mappings.

==============================================================================
3. potwiki commands	{{{2                                *potwiki-commands*

    See |potwiki-intro| for default mapping.

    Currently potwiki does not define any commands.

==============================================================================
4. potwiki customization  {{{2                            *potwiki-customize*

4.1. General configuration {{{3 ~
--------------------------
                                          *loaded_potwiki*  *potwiki-disable*
    You can disable this script by putting the following line in your |vimrc| >
      let loaded_potwiki = 1
<

    You can define your own color scheme for error highlighting, by setting
    |highlight| on PotwikiWord and PotwikiWordNotFound groups. For example: >

      highlight PotwikiWord          guifg=darkcyan
      highlight PotwikiWordNotFound  guibg=Red guifg=Yellow
<
4.2. Mapping documentation: {{{3 ~
---------------------------
                                                   *potwiki-mappings-override*
    By default, a global mapping is defined for some commands.  User-provided
    mappings can be used instead by mapping to <Plug>CommandName. This is
    especially useful when these mappings collide with other existing mappings
    (vim will warn of this during plugin initialization, but will not clobber
    the existing mappings).

    For instance, to override the default mapping for :PotwikiHome to set it to
    \wh, add the following to the |vimrc|:
>
      nmap \wh <Plug>PotwikiHome
<
4.3. Options documentation: {{{3 ~
---------------------------
                                                             *potwiki-options*
    Several variables are checked by the script to customize potwiki
    behavior. You can set them using let in your |vimrc| file.
    Example:

        let potwiki_home = "$HOME/MyWiki/HomePage"

    potwiki_home                                                *potwiki_home*
      This variable contains the filename of your Wiki HomePage.
      default: $HOME/Wiki/HomePage

    potwiki_home_dir                                        *potwiki_home_dir*
      This variable contains the path of your Wiki directory.
      default: the directory containing the file set in potwiki_home.

    potwiki_upper                                              *potwiki_upper*
      Upper case characters for WikiWords. Uses the syntax of [ ] atoms
      in regular expressions. 
      default: 'A-Z���'

    potwiki_lower                                              *potwiki_lower*
      Lower case characters for WikiWords. Uses the syntax of [ ] atoms
      in regular expressions. 
      default: 'a-z����'

    potwiki_other                                              *potwiki_other*
      Non-letter characters for WikiWords. Uses the syntax of [ ] atoms
      in regular expressions. 
      default: '0-9_'

    potwiki_autowrite                                      *potwiki_autowrite*
      If this is non-zero potwiki always writes a Wiki file when it
      is closed by <Plug>PotwikiClose.

==============================================================================
6. potwiki bugs  {{{2                                          *potwiki-bugs*

    Please contact me <edwin.steiner@gmx.net> if you find any bugs in
    potwiki. I'm sure there are enough of them.

    Include 'potwiki' in the subject line when contacting me.

==============================================================================
7. potwiki TODO list  {{{2                                     *potwiki-todo*

    Please contact me <edwin.steiner@gmx.net> if you have suggestions
    for improving potwiki.

    Include 'potwiki' in the subject line when contacting me.

==============================================================================
=== END_DOC
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"fileencoding=iso-8859-15 
" vim: ts=8 sw=2

