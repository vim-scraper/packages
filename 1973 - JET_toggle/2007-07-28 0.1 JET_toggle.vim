"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                                                             "
" @file JET_toggle.vim                                                        "
"                                                                             "
" @author Jay E. Taylor (outtatime@gmail.com)                                 "
"                                                                             "
" @date 2007-07-28                                                            "
"                                                                             "
" @description see :help JET_toggle to read the docs, or view the end of this "
"              file.                                                          "
"                                                                             "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

if exists('s:loaded_JET_toggle')
  finish
endif
if (v:progname == "ex")
  finish
endif
let s:loaded_JET_toggle = 1
let s:JET_toggle_version = 0.1

let s:JET_next_id = 0

function! JET_Toggle(id)
  let statevar = 's:JET_state_'.a:id
  let state = {statevar}
  
  if (state == 0)
    let oncommandvar = 's:JET_oncommand_'.a:id
    let oncommand = {oncommandvar}
    echo oncommand
    execute oncommand
    let {statevar} = 1
  else
    "echo "TOGGLED OFF"
    let offcommandvar = 's:JET_offcommand_'.a:id
    let offcommand = {offcommandvar}
    echo offcommand
    execute offcommand
    let {statevar} = 0
  endif
endfunction

function! JET_AddToggle(oncommand, offcommand, bindToKey)
  let id = s:JET_next_id
  
  let statevar = 's:JET_state_'.id
  let {statevar} = 0
  
  let switchon = 's:JET_oncommand_'.id
  let switchoff = 's:JET_offcommand_'.id
  
  let {switchon} = a:oncommand
  let {switchoff} = a:offcommand
  
  :execute ':map <C-'.a:bindToKey.' :call JET_Toggle('.id.')<CR>'
  :execute ':nmap <C-'.a:bindToKey.' :call JET_Toggle('.id.')<CR>'
  :execute ':imap <C-'.a:bindToKey.' <ESC>:call JET_Toggle('.id.')<CR>a'
  :execute ':vmap <C-'.a:bindToKey.' y<ESC>:call JET_Toggle('.id.')<CR>'
  
  let s:JET_next_id = s:JET_next_id + 1
endfunction

" Function: s:InstallDocumentation(full_name, revision)              {{{2
"   Install help documentation.
" Arguments:
"   full_name: Full name of this vim plugin script, including path name.
"   revision:  Revision of the vim script. #version# mark in the document file
"              will be replaced with this string with 'v' prefix.
" Return:
"   1 if new document installed, 0 otherwise.
" Note: Cleaned and generalized by guo-peng Wen.
"
" Note about authorship: this function was taken from the vimspell plugin
" which can be found at http://www.vim.org/scripts/script.php?script_id=465
"
function! s:InstallDocumentation(full_name, revision)
    " Name of the document path based on the system we use:
    if has("vms")
         " No chance that this script will work with
         " VMS -  to much pathname juggling here.
         return 1
    elseif (has("unix"))
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
         "Doc path: " . l:vim_doc_path
        call s:NerdEcho("Doc path: " . l:vim_doc_path, 0)
        execute l:mkdir_cmd . '"' . l:vim_doc_path . '"'
        if (!(filewritable(l:vim_doc_path) == 2))
            " Try a default configuration in user home:
            let l:vim_doc_path = expand("~") . l:doc_home
            if (!(filewritable(l:vim_doc_path) == 2))
                execute l:mkdir_cmd . '"' . l:vim_doc_path . '"'
                if (!(filewritable(l:vim_doc_path) == 2))
                    " Put a warning:
                    call s:NerdEcho("Unable to open documentation directory", 0)
                    call s:NerdEcho(" type :help add-local-help for more informations.", 0)
                    echo l:vim_doc_path
                    return 0
                endif
            endif
        endif
    endif

    " Exit if we have problem to access the document directory:
    if (!isdirectory(l:vim_plugin_path) || !isdirectory(l:vim_doc_path) || filewritable(l:vim_doc_path) != 2)
        return 0
    endif

    " Full name of script and documentation file:
    let l:script_name = fnamemodify(a:full_name, ':t')
    let l:doc_name    = fnamemodify(a:full_name, ':t:r') . '.txt'
    let l:plugin_file = l:vim_plugin_path . l:slash_char . l:script_name
    let l:doc_file    = l:vim_doc_path    . l:slash_char . l:doc_name

    " Bail out if document file is still up to date:
    if (filereadable(l:doc_file) && getftime(l:plugin_file) < getftime(l:doc_file))
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
    :%s/{\{3}[1-9]/    /

    " Add modeline for help doc: the modeline string is mangled intentionally
    " to avoid it be recognized by VIM:
    call append(line('$'), '')
    call append(line('$'), ' v' . 'im:tw=78:ts=8:ft=help:norl:')

    " Replace revision:
    "exe "normal :1s/#version#/ v" . a:revision . "/\<CR>"
    exe "normal :%s/#version#/ v" . a:revision . "/\<CR>"

    " Save the help document:
    exe 'w! ' . l:doc_file
    exe l:go_back
    exe 'bw ' . l:buf

    " Build help tags:
    exe 'helptags ' . l:vim_doc_path

    return 1
endfunction


" Section: Doc installation call {{{1
silent call s:InstallDocumentation(expand('<sfile>:p'), s:JET_toggle_version)

finish

"=============================================================================
" Section: The help file {{{1 
" Title {{{2
" ============================================================================
=== START_DOC
*JET_toggle.txt*           Plugin to dynamically add toggles     #version#


                         JET_TOGGLE REFERENCE MANUAL~





==============================================================================
CONTENTS {{{2                                        *JET_togglelist-contents* 

    1. Intro ................................... |JET_toggle|
    2. How to add a toggle ..................... |JET_toggle-howto|
    3. Feedback ................................ |JET_toggle-feedback|

==============================================================================
1. Intro {{{2                                                     *JET_toggle*

    JET_toggle provides a mechanism for adding toggle commands and mapping
  them to a shortcut.

==============================================================================
2. How to add a toggle {{{2                                 *JET_toggle-howto*

  Source the script from your .vimrc, and then add whatever toggles you want.
  

  Syntax:
  
    JET_AddToggle(':someCommandString', ':someOtherCommandString', '<keymap>')
  
  
  Example:
  
    source ~/.vim/plugin/JET_toggle.vim
    JET_AddToggle(':set list', ':set nolist', '<C-l>')
    JET_AddToggle(':set paste', ':set nopaste', '<M-p>')
  

    Then, when you type control-l it will toggle between ':set list' and
  ':set nolist' and when you type alt-p it will toggle between ':set paste'
  and ':set nopaste'.
  
==============================================================================
3. Feedback {{{2                                         *JET_toggle-feedback*

  Constructive feedback is welcome, send it to outtatime.at-symbol.gmail.com
  with [JET_toggle] in the subject line.  Enjoy!
