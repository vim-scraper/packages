""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" fuzzyfinder.vim : The fuzzy/partial pattern explorer for
"                   buffer/file/MRU/favorite/tag/etc.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Author:  Takeshi Nishida <ns9tks(at)gmail(dot)com>
" Version: 2.2, for Vim 7.1
" Licence: MIT Licence
" URL:     http://www.vim.org/scripts/script.php?script_id=1984
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" DOCUMENT:
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Description: {{{1
"   Fuzzyfinder provides convenient ways to quickly reach the buffer/file you
"   want. Fuzzyfinder finds matching files/buffers with a fuzzy/partial
"   pattern to which it converted the entered pattern.
"
"   E.g.: entered pattern -> fuzzy pattern / partial pattern
"         abc             -> *a*b*c*       / *abc*
"         a?c             -> *a?c*         / *a?c*
"         dir/file        -> dir/*f*i*l*e* / dir/*file*
"         d*r/file        -> d*r/*f*i*l*e* / d*r/*file*
"         ../**/s         -> ../**/*s*     / ../**/*s*
"
"     (** allows searching a directory tree.)
"
"   You will be happy when:
"     "./OhLongLongLongLongLongFile.txt"
"     "./AhLongLongLongLongLongName.txt"
"     "./AhLongLongLongLongLongFile.txt" <- you want :O
"     Type "AF" and "AhLongLongLongLongLongFile.txt" will be select. :D
"
"   Fuzzyfinder has some modes:
"     - Buffer mode
"     - File mode
"     - MRU-file mode (most recently used files)
"     - Favorite-file mode
"     - Directory mode (yet another :cd command)
"     - Tag mode (yet another :tag command)
"     - Tagged-file mode (files which are included in current tags)
"
"   Fuzzyfinder supports the multibyte.
"
"-----------------------------------------------------------------------------
" Installation: {{{1
"   Drop this file in your plugin directory. If you have installed
"   autocomplpop.vim (vimscript #1879), please update to the latest version
"   to prevent interference.
"
"-----------------------------------------------------------------------------
" Usage: {{{1
"   Starting Fuzzyfinder:
"     You can start Fuzzyfinder by the following commands:
"
"       :FuzzyFinderBuffer      - launchs buffer-mode Fuzzyfinder.
"       :FuzzyFinderFile        - launchs file-mode Fuzzyfinder.
"       :FuzzyFinderMruFile     - launchs MRU-file-mode Fuzzyfinder.
"       :FuzzyFinderFavFile     - launchs favorite-file-mode Fuzzyfinder.
"       :FuzzyFinderDir         - launchs directory-mode Fuzzyfinder.
"       :FuzzyFinderTag         - launchs tag-mode Fuzzyfinder.
"       :FuzzyFinderTaggedFile  - launchs tagged-file-mode Fuzzyfinder.
"
"     It is recommended to map these commands. These commands can take initial
"     text as a command argument. The text will be entered after Fuzzyfinder
"     launched. If a command was executed with a ! modifier (e.g.
"     :FuzzyFinderTag!), it enables the partial matching instead of the fuzzy
"     matching.
"
"
"   In Fuzzyfinder:
"     The entered pattern is converted to the fuzzy pattern and buffers/files
"     which match the pattern is shown in a completion menu.
"
"     A completion menu is shown when you type at the end of the line and the
"     length of entered pattern is more than setting value. By default, it is
"     shown at the beginning.
"
"     If too many items (200, by default) were matched, the completion is
"     aborted to reduce nonresponse.
"
"     If entered pattern matched the item exactly, the item is shown first.
"     Same applies to the item number in the buffer/MRU/favorite mode.  The
"     item whose file name has longer prefix matching is placed upper. The
"     item which matched more sequentially is placed upper. It lets the first
"     item into selected in completion menu.
"
"     Pressing <CR> opens selected item in previous window. If selected item
"     was a directory in the file mode, it just inserts text. Use <C-j> to
"     open in new window which is made from split previous window, or <C-k> To
"     open in new window which is made from split previous window vertically.
"     These key mappings are customizable.
"
"     To cancel and return to previous window, leave the insert mode.
"
"     To Switch the mode without leaving a insert mode, use <C-l> or <C-o>.
"     This key mapping is customizable.
"
"     If you want to temporarily change whether or not to ignore case, use
"     <C-t>. This key mapping is customizable.
"
"   About Highlighting:
"     Fuzzyfinder highlights the buffer with "Error" group when the completion
"     item was not found or the completion process was aborted.
"
"   About Alternative Approach For Tag Jump:
"     Following mappings are replacements for :tag and <C-]>:
"
"       nnoremap <silent> <C-f><C-t> :FuzzyFinderTag!<CR>
"       nnoremap <silent> <C-]>      :FuzzyFinderTag! <C-r>=expand('<cword>')<CR><CR>
"
"     In the tag mode, it is recommended to use partial matching instead of
"     fuzzy matching.
"
"   About Tagged File Mode:
"     The files which are included in the current tags are the ones which are
"     related to the current working environment. So this mode is a pseudo
"     project mode.
"
"   About Usage Of Command Argument:
"     As an example, if you want to launch file-mode Fuzzyfinder with the
"     directory of current buffer and not current directory, map as below:
"
"       nnoremap <C-p> :FuzzyFinderFile <C-r>=expand('%:~:.')[:-1-len(expand('%:~:.:t'))]<CR><CR>
"
"   About Abbreviations And Multiple Search:
"     You can use abbreviations and multiple search in each mode. For example,
"     set as below:
"
"       let g:FuzzyFinderOptions.File.abbrev_map  = {
"             \   "^WORK" : [
"             \     "~/project/**/src/",
"             \     ".vim/plugin/",
"             \   ],
"             \ }
"
"     And type "WORKtxt" in file-mode Fuzzyfinder, then it searches by
"     following patterns:
"
"       "~/project/**/src/*t*x*t*"
"       ".vim/plugin/*t*x*t*"
"
"   Adding Favorite Files:
"     You can add a favorite file by the following commands:
"
"       :FuzzyFinderAddFavFile {filename}
"
"     If you do not specify the filename, current file name is used.
"
"   About Information File:
"     Fuzzyfinder writes information of the MRU, favorite, etc to the file by
"     default (~/.vimfuzzyfinder).
"
"   About Cache:
"     Once a cache was created, It is not updated automatically to improve
"     response by default. To update it, use :FuzzyFinderRemoveCache command.
"
"   About Migemo:
"     Migemo is a search method for Japanese language.
"
"-----------------------------------------------------------------------------
" Options: {{{1
"   You can set options via g:FuzzyFinderOptions which is a dictionary. See
"   the folded section named "INITIALIZATION: GLOBAL OPTIONS:" for details. To
"   easily set options for customization, put necessary entries from GLOBAL
"   OPTIONS into your vimrc file and edit those values.
"
"-----------------------------------------------------------------------------
" Setting Example: {{{1
"   let g:FuzzyFinderOptions = { 'Base':{}, 'Buffer':{}, 'File':{}, 'MruFile':{}, 'FavFile':{}, 'Dir':{}, 'Tag':{}, 'TaggedFile':{}}
"   let g:FuzzyFinderOptions.Base.ignore_case = 1
"   let g:FuzzyFinderOptions.File.abbrev_map  = {
"         \   '\C^VP' : [
"         \     '$VIMRUNTIME/plugin/',
"         \     '~/.vim/plugin/',
"         \     '$VIM/.vim/plugin/',
"         \     '$VIM/vimfiles/plugin/',
"         \   ],
"         \   '\C^VC' : [
"         \     '$VIMRUNTIME/colors/',
"         \     '~/.vim/colors/',
"         \     '$VIM/.vim/colors/',
"         \     '$VIM/vimfiles/colors/',
"         \   ],
"         \ }
"   let g:FuzzyFinderOptions.MruFile.max_item = 400
"   nnoremap <silent> <C-n>      :FuzzyFinderBuffer<CR>
"   nnoremap <silent> <C-p>      :FuzzyFinderFile<CR>
"   nnoremap <silent> <C-p>      :FuzzyFinderFile <C-r>=expand('%:~:.')[:-1-len(expand('%:~:.:t'))]<CR><CR>
"   nnoremap <silent> <C-f><C-n> :FuzzyFinderMruFile<CR>
"   nnoremap <silent> <C-f><C-f> :FuzzyFinderFavFile<CR>
"   nnoremap <silent> <C-f><C-d> :FuzzyFinderDir <C-r>=fnamemodify('.', ':p')<CR><CR>
"   nnoremap <silent> <C-f><C-t> :FuzzyFinderTag!<CR>
"   nnoremap <silent> <C-f><C-g> :FuzzyFinderTaggedFile<CR>
"   nnoremap <silent> <C-]>      :FuzzyFinderTag! <C-r>=expand('<cword>')<CR><CR>
"
"-----------------------------------------------------------------------------
" Thanks: {{{1
"   Vincent Wang
"   Ingo Karkat
"   Nikolay Golubev
"   Brian Doyle
"   id:secondlife
"
"-----------------------------------------------------------------------------
" ChangeLog: {{{1
"   2.2:
"     - Added new feature, which is the partial matching.
"     - Fixed the bug that an error occurs when "'" was entered.
"   2.1:
"     - Restructured the option system AGAIN. Sorry :p
"     - Changed to inherit a typed text when switching a mode without leaving
"       a insert mode.
"     - Changed commands which launch explorers to be able to take a argument
"       for initial text.
"     - Changed to complete file names by relative path and not full path in
"       the buffer/mru-file/tagged-file mode.
"     - Changed to highlight a typed text when the completion item was not
"       found or the completion process was aborted.
"     - Changed to create caches for each tag file and not working directory
"       in the tag/tagged-file mode.
"     - Fixed the bug that the buffer mode couldn't open a unnamed buffer.
"     - Added 'matching_limit' option.
"     - Removed 'max_match' option. Use 'matching_limit' option instead.
"     - Removed 'initial_text' option. Use command argument instead.
"     - Removed the MRU-command mode.
"   2.0:
"     - Added the tag mode.
"     - Added the tagged-file mode.
"     - Added :FuzzyFinderRemoveCache command.
"     - Restructured the option system. many options are changed names or
"       default values of some options.
"     - Changed to hold and reuse caches of completion lists by default.
"     - Changed to set filetype 'fuzzyfinder'.
"     - Disabled the MRU-command mode by default because there are problems.
"     - Removed FuzzyFinderAddMode command.
"
"   1.5:
"     - Added the directory mode.
"     - Fixed the bug that it caused an error when switch a mode in the insert
"       mode.
"     - Changed g:FuzzyFinder_KeySwitchMode type to a list.
"
"   1.4:
"     - Changed the specification of the information file.
"     - Added the MRU-commands mode.
"     - Renamed :FuzzyFinderAddFavorite command to :FuzzyFinderAddFavFile.
"     - Renamed g:FuzzyFinder_MruModeVars option to
"       g:FuzzyFinder_MruFileModeVars.
"     - Renamed g:FuzzyFinder_FavoriteModeVars option to
"       g:FuzzyFinder_FavFileModeVars.
"     - Changed to show registered time of each item in MRU/favorite mode.
"     - Added 'timeFormat' option for MRU/favorite modes.
"
"   1.3:
"     - Fixed a handling of multi-byte characters.
"
"   1.2:
"     - Added support for Migemo. (Migemo is Japanese search method.)
"
"   1.1:
"     - Added the favorite mode.
"     - Added new features, which are abbreviations and multiple search.
"     - Added 'abbrevMap' option for each mode.
"     - Added g:FuzzyFinder_MruModeVars['ignoreSpecialBuffers'] option.
"     - Fixed the bug that it did not work correctly when a user have mapped
"       <C-p> or <Down>.
"
"   1.0:
"     - Added the MRU mode.
"     - Added commands to add and use original mode.
"     - Improved the sorting algorithm for completion items.
"     - Added 'initialInput' option to automatically insert a text at the
"       beginning of a mode.
"     - Changed that 'excludedPath' option works for the entire path.
"     - Renamed some options. 
"     - Changed default values of some options. 
"     - Packed the mode-specific options to dictionaries.
"     - Removed some options.
"
"   0.6:
"     - Fixed some bugs.

"   0.5:
"     - Improved response by aborting processing too many items.
"     - Changed to be able to open a buffer/file not only in previous window
"       but also in new window.
"     - Fixed a bug that recursive searching with '**' does not work.
"     - Added g:FuzzyFinder_CompletionItemLimit option.
"     - Added g:FuzzyFinder_KeyOpen option.
"
"   0.4:
"     - Improved response of the input.
"     - Improved the sorting algorithm for completion items. It is based on
"       the matching level. 1st is perfect matching, 2nd is prefix matching,
"       and 3rd is fuzzy matching.
"     - Added g:FuzzyFinder_ExcludePattern option.
"     - Removed g:FuzzyFinder_WildIgnore option.
"     - Removed g:FuzzyFinder_EchoPattern option.
"     - Removed g:FuzzyFinder_PathSeparator option.
"     - Changed the default value of g:FuzzyFinder_MinLengthFile from 1 to 0.
"
"   0.3:
"     - Added g:FuzzyFinder_IgnoreCase option.
"     - Added g:FuzzyFinder_KeyToggleIgnoreCase option.
"     - Added g:FuzzyFinder_EchoPattern option.
"     - Changed the open command in a buffer mode from ":edit" to ":buffer" to
"       avoid being reset cursor position.
"     - Changed the default value of g:FuzzyFinder_KeyToggleMode from
"       <C-Space> to <F12> because <C-Space> does not work on some CUI
"       environments.
"     - Changed to avoid being loaded by Vim before 7.0.
"     - Fixed a bug with making a fuzzy pattern which has '\'.
"
"   0.2:
"     - A bug it does not work on Linux is fixed.
"
"   0.1:
"     - First release.
"
"
"
"-----------------------------------------------------------------------------
" }}}1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" INCLUDE GUARD:
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" INCLUDE GUARD {{{1
if exists('loaded_fuzzyfinder') || v:version < 700
  finish
endif
let loaded_fuzzyfinder = 1
" }}}1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" FUNCTIONS:
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function! s:Initialize()
  " INITIALIZATION: CONSTANTS: {{{1
  "...........................................................................
  let s:info_version = 104
  let s:path_separator = (has('win32') || has('win64') ? '\' : '/')
  let s:prompt = '>'
  let s:matching_rate_base = 10000000
  let s:sid_prefix = matchstr(expand('<sfile>'), '<SNR>\d\+_') " global name of 's:'
  let s:msg_rm_info = "==================================================\n" .
        \             "  Your fuzzyfinder information file is no longer \n"  .
        \             "  supported. Please remove\n"                         .
        \             "  \"%s\".\n"                                          .
        \             "=================================================="

  " INITIALIZATION: GLOBAL OPTIONS: {{{1
  "...........................................................................
  " stores user-defined g:FuzzyFinderOptions {{{2
  let user_options = (exists('g:FuzzyFinderOptions') ? g:FuzzyFinderOptions : {}) " }}}2

  " Initializes g:FuzzyFinderOptions.
  let g:FuzzyFinderOptions = { 'Base':{}, 'Buffer':{}, 'File':{}, 'MruFile':{}, 'FavFile':{}, 'Dir':{}, 'Tag':{}, 'TaggedFile':{}}
  "...........................................................................
  " [All Mode] This is mapped to select completion item or finish input and
  " open a buffer/file in previous window.
  let g:FuzzyFinderOptions.Base.key_open = '<CR>'
  " [All Mode] This is mapped to select completion item or finish input and
  " open a buffer/file in split new window
  let g:FuzzyFinderOptions.Base.key_open_split = '<C-j>'
  " [All Mode] This is mapped to select completion item or finish input and
  " open a buffer/file in vertical-split new window.
  let g:FuzzyFinderOptions.Base.key_open_vsplit = '<C-k>'
  " [All Mode] This is mapped to switch to the next mode.
  let g:FuzzyFinderOptions.Base.key_next_mode = '<C-l>'
  " [All Mode] This is mapped to switch to the previous mode.
  let g:FuzzyFinderOptions.Base.key_prev_mode = '<C-o>'
  " [All Mode] This is mapped to temporarily switch whether or not to ignore
  " case.
  let g:FuzzyFinderOptions.Base.key_ignore_case = '<C-t>'
  " [All Mode] This is the file name to write information of the MRU, etc. If
  " "" was set, it does not write to the file.
  let g:FuzzyFinderOptions.Base.info_file = '~/.vimfuzzyfinder'
  " [All Mode] It does not start a completion if a length of entered text is
  " less than this.
  let g:FuzzyFinderOptions.Base.min_length = 0
  " [All Mode] It ignores case in search patterns if non-zero is set.
  let g:FuzzyFinderOptions.Base.ignore_case = 1
  " [All Mode] It does not remove caches of completion lists at the end of
  " explorer to reuse at the next time if non-zero was set.
  let g:FuzzyFinderOptions.Base.lasting_cache = 1
  " [All Mode] It uses Migemo if non-zero is set.
  let g:FuzzyFinderOptions.Base.migemo_support = 0
  "...........................................................................
  " [Buffer Mode] It disables all functions of this mode if zero was set.
  let g:FuzzyFinderOptions.Buffer.mode_available = 1
  " [Buffer Mode] This is a dictionary. Each value must be a list. All matchs
  " of a key in entered text is expanded with the value.
  let g:FuzzyFinderOptions.Buffer.abbrev_map = {}
  " [Buffer Mode] The items whose :ls-indicators match this are excluded from
  " the completion list.
  let g:FuzzyFinderOptions.Buffer.excluded_indicator = '[u\-]'
  "...........................................................................
  " [File Mode] It disables all functions of this mode if zero was set.
  let g:FuzzyFinderOptions.File.mode_available = 1
  " [File Mode] This is a dictionary. Each value must be a list. All matchs of
  " a key in entered text is expanded with a value.
  let g:FuzzyFinderOptions.File.abbrev_map = {}
  " [File Mode] The items matching this are excluded from the completion list.
  let g:FuzzyFinderOptions.File.excluded_path = '\v\~$|\.o$|\.exe$|\.bak$|\.swp$|((^|[/\\])\.[/\\]$)'
  " [File Mode] If a number of matched items was over this, the completion
  " process is aborted.
  let g:FuzzyFinderOptions.File.matching_limit = 200
  "...........................................................................
  " [Mru-File Mode] It disables all functions of this mode if zero was set.
  let g:FuzzyFinderOptions.MruFile.mode_available = 1
  " [Mru-File Mode] This is a dictionary. Each value must be a list. All
  " matchs of a key in entered text is expanded with a value. 
  let g:FuzzyFinderOptions.MruFile.abbrev_map = {}
  " [Mru-File Mode] The items matching this are excluded from the completion
  " list.
  let g:FuzzyFinderOptions.MruFile.excluded_path = '\v\~$|\.bak$|\.swp$'
  " [Mru-File Mode] It ignores special buffers if non-zero was set.
  let g:FuzzyFinderOptions.MruFile.no_special_buffer = 1
  " [Mru-File Mode] This is a string to format registered time. See :help
  " strftime() for details.
  let g:FuzzyFinderOptions.MruFile.time_format = '(%x %H:%M:%S)'
  " [Mru-File Mode] This is an upper limit of MRU items to be stored.
  let g:FuzzyFinderOptions.MruFile.max_item = 99
  "...........................................................................
  " [Favorite-File Mode] It disables all functions of this mode if zero was
  " set.
  let g:FuzzyFinderOptions.FavFile.mode_available = 1
  " [Favorite-File Mode] This is a dictionary. Each value must be a list. All
  " matchs of a key in entered text is expanded with a value. 
  let g:FuzzyFinderOptions.FavFile.abbrev_map = {}
  " [Favorite-File Mode] This is a string to format registered time. See :help
  " strftime() for details.
  let g:FuzzyFinderOptions.FavFile.time_format = '(%x %H:%M:%S)'
  "...........................................................................
  " [Directory Mode] It disables all functions of this mode if zero was set.
  let g:FuzzyFinderOptions.Dir.mode_available = 1
  " [Directory Mode] This is a dictionary. Each value must be a list. All
  " matchs of a key in entered text is expanded with a value. 
  let g:FuzzyFinderOptions.Dir.abbrev_map = {}
  " [Directory Mode] The items matching this are excluded from the completion
  " list.
  let g:FuzzyFinderOptions.Dir.excluded_path = '\v(^|[/\\])\.{1,2}[/\\]$'
  "...........................................................................
  " [Tag Mode] It disables all functions of this mode if zero was set.
  let g:FuzzyFinderOptions.Tag.mode_available = 1
  " [Tag Mode] This is a dictionary. Each value must be a list. All matchs of
  " a key in entered text is expanded with a value. 
  let g:FuzzyFinderOptions.Tag.abbrev_map = {}
  " [Tag Mode] The items matching this are excluded from the completion list.
  let g:FuzzyFinderOptions.Tag.excluded_path = '\v\~$|\.bak$|\.swp$'
  " [Tag Mode] If a number of matched items was over this, the completion
  " process is aborted.
  let g:FuzzyFinderOptions.Tag.matching_limit = 200
  "...........................................................................
  " [Tagged-File Mode] It disables all functions of this mode if zero was set.
  let g:FuzzyFinderOptions.TaggedFile.mode_available = 1
  " [Tagged-File Mode] This is a dictionary. Each value must be a list. All
  " matchs of a key in entered text is expanded with a value. 
  let g:FuzzyFinderOptions.TaggedFile.abbrev_map = {}
  " [Tagged-File Mode] If a number of matched items was over this, the
  " completion process is aborted.
  let g:FuzzyFinderOptions.TaggedFile.matching_limit = 200

  "...........................................................................
  " overwrites default values of g:FuzzyFinderOptions with user-defined values {{{2
  call map(user_options, 'extend(g:FuzzyFinderOptions[v:key], v:val, ''force'')')
  call map(copy(g:FuzzyFinderMode), 'v:val.extend_options()')
  " }}}2

  " INITIALIZATION: COMMANDS AND AUTOCOMMANDS: {{{1
  "...........................................................................
  augroup FuzzyFinderGlobalAutoCommand
    autocmd!
    autocmd BufEnter     * for m in s:GetAvailableModes() | call m.on_buf_enter() | endfor
    autocmd BufWritePost * for m in s:GetAvailableModes() | call m.on_buf_write_post() | endfor
  augroup END

  command! -bang -narg=? -complete=buffer FuzzyFinderBuffer      call g:FuzzyFinderMode.Buffer.launch(<q-args>, len('<bang>'))
  command! -bang -narg=? -complete=file   FuzzyFinderFile        call g:FuzzyFinderMode.File.launch(<q-args>, len('<bang>'))
  command! -bang -narg=? -complete=file   FuzzyFinderMruFile     call g:FuzzyFinderMode.MruFile.launch(<q-args>, len('<bang>'))
  command! -bang -narg=? -complete=file   FuzzyFinderFavFile     call g:FuzzyFinderMode.FavFile.launch(<q-args>, len('<bang>'))
  command! -bang -narg=? -complete=dir    FuzzyFinderDir         call g:FuzzyFinderMode.Dir.launch(<q-args>, len('<bang>'))
  command! -bang -narg=? -complete=tag    FuzzyFinderTag         call g:FuzzyFinderMode.Tag.launch(<q-args>, len('<bang>'))
  command! -bang -narg=? -complete=file   FuzzyFinderTaggedFile  call g:FuzzyFinderMode.TaggedFile.launch(<q-args>, len('<bang>'))
  command! -bang -narg=? -complete=file   FuzzyFinderAddFavFile  call g:FuzzyFinderMode.FavFile.add(<q-args>, 1)
  command! -bang -narg=0                  FuzzyFinderRemoveCache for m in s:GetAvailableModes() | call m.empty_cache_if_existed(1) | endfor

" }}}1
endfunction

" CORE FUNCTIONS: {{{1
"-----------------------------------------------------------------------------
function! s:GetAvailableModes()
  return filter(values(g:FuzzyFinderMode), 'exists(''v:val.mode_available'') && v:val.mode_available')
endfunction

"-----------------------------------------------------------------------------
" set or restore temporary option 
function! s:SetTemporaryOption(option, ...)
  if a:0 == 1
    if !exists('s:_{a:option}')
      execute printf('let s:_{a:option} = &%s', a:option)
    endif
    execute printf('let &%s = a:1', a:option)
  else
    execute printf('let &%s = s:_{a:option}', a:option)
    unlet s:_{a:option}
  endif
endfunction

" LIST FUNCTIONS: {{{1
"-----------------------------------------------------------------------------
function! s:Unique(in)
  let sorted = sort(a:in)

  if len(sorted) < 2
    return sorted
  endif

  let last = remove(sorted, 0)
  let result = [last]
  for item in sorted
    if item != last
      call add(result, item)
      let last = item
    endif
  endfor

  return result
endfunction

"-----------------------------------------------------------------------------
function! s:Concat(in)
  let result = []
  for l in a:in
    let result += l
  endfor
  return result
endfunction

"-----------------------------------------------------------------------------
" copy + filter + limit
function! s:FilterEx(in, expr, limit)
  if a:limit <= 0
    return filter(copy(a:in), a:expr)
  endif

  let result = []

  let stride = a:limit * 3 / 2 " x1.5
  for i in range(0, len(a:in) - 1, stride)
    let result += filter(a:in[i : i + stride - 1], a:expr)
    if len(result) >= a:limit
      return remove(result, 0, a:limit - 1)
    endif
  endfor

  return result
endfunction

"-----------------------------------------------------------------------------
function! s:MakeNumberedList(in, first)
  for i in range(len(a:in))
    let a:in[i] = [i + a:first, a:in[i]]
  endfor

  return a:in
endfunction

" MISC: {{{1
"-----------------------------------------------------------------------------
function! s:ConvertWildcardToRegexp(expr)
  let re = escape(a:expr, '\')
  for [pat, sub] in [ [ '*', '\\.\\*' ], [ '?', '\\.' ], [ '[', '\\[' ], ]
    let re = substitute(re, pat, sub, 'g')
  endfor
  return '\V' . re
endfunction

"-----------------------------------------------------------------------------
function! s:ExpandAbbrevMap(base, abbrev_map)
  let result = [a:base]

  " expand
  for [pattern, sub_list] in items(a:abbrev_map)
    let exprs = result
    let result = []
    for expr in exprs
      let result += map(copy(sub_list), 'substitute(expr, pattern, v:val, "g")')
    endfor
  endfor

  return s:Unique(result)
endfunction

"-----------------------------------------------------------------------------
function! s:ExistsPrompt(in)
  return strlen(a:in) >= strlen(s:prompt) && a:in[:strlen(s:prompt) -1] ==# s:prompt
endfunction

"-----------------------------------------------------------------------------
function! s:RemovePrompt(in)
  return (s:ExistsPrompt(a:in) ? a:in[strlen(s:prompt):] : a:in)
endfunction

"-----------------------------------------------------------------------------
function! s:SplitPath(path)
  let dir = matchstr(a:path, '^.*[/\\]')
  return [dir, a:path[strlen(dir):]]
endfunction

"-----------------------------------------------------------------------------
function! s:GetBuffers()
  redir => buffers | silent buffers! | redir END
  return map(map(split(buffers, "\n"),
        \        'matchlist(v:val, ''^\s*\(\d*\)\([^"]*\)"\([^"]*\)".*$'')'),
        \    '{ ''nr''   : v:val[1], ' .
        \    '  ''ind''  : v:val[2], ' .
        \    '  ''path'' : fnamemodify(v:val[3], '':~:.'') }')
endfunction

"-----------------------------------------------------------------------------
function! s:GetTagList(tagfile)
  return map(readfile(a:tagfile), 'matchstr(v:val, ''^[^!\t][^\t]*'')')
endfunction

"-----------------------------------------------------------------------------
function! s:GetTaggedFileList(tagfile)
  execute 'cd ' . fnamemodify(a:tagfile, ':h')
  let result = map(readfile(a:tagfile), 'fnamemodify(matchstr(v:val, ''^[^!\t][^\t]*\t\zs[^\t]\+''), '':p:~'')')
  cd -
  return result
endfunction

"-----------------------------------------------------------------------------
function! s:MakeCompletionItem(expr, number_str, abbr, time, entered, evals_path_tail)
  let number = str2nr(a:number_str)
  if      (number >= 0 && str2nr(number) == str2nr(a:entered)) || (a:expr == a:entered)
    let rate = s:matching_rate_base
  elseif a:evals_path_tail
    let rate = s:EvaluateMatchingRate(s:SplitPath(matchstr(a:expr, '^.*[^/\\]'))[1],
          \                           s:SplitPath(a:entered)[1])
  else
    let rate = s:EvaluateMatchingRate(a:expr, a:entered)
  endif

  return  {
        \   'word'  : a:expr,
        \   'abbr'  : (number >= 0 ? printf('%2d: ', number) : '') . a:abbr,
        \   'menu'  : printf('%s[%s]', (len(a:time) ? a:time . ' ' : ''), s:MakeRateStars(rate, 5)),
        \   'order' : [-rate, (number >= 0 ? number : a:expr)]
        \ }
endfunction

"-----------------------------------------------------------------------------
function! s:EvaluateMatchingRate(expr, pattern)
  if a:expr == a:pattern
    return s:matching_rate_base
  endif

  let rate = 0.0
  let rate_increment = (s:matching_rate_base * 9) / (len(a:pattern) * 10) " zero divide ok
  let matched = 1

  let i_pattern = 0
  for i_expr in range(len(a:expr))
    if a:expr[i_expr] == a:pattern[i_pattern]
      let rate += rate_increment
      let matched = 1
      let i_pattern += 1
      if i_pattern >= len(a:pattern)
        break
      endif
    elseif matched
      let rate_increment = rate_increment / 2
      let matched = 0
    endif
  endfor

  return rate
endfunction

"-----------------------------------------------------------------------------
function! s:MakeRateStars(rate, base)
  let len = (a:base * a:rate) / s:matching_rate_base
  return repeat('*', len) . repeat('.', a:base - len)
endfunction

"-----------------------------------------------------------------------------
function! s:HighlightError(error)
  if a:error
    syntax match Error  /^.*$/
  else
    syntax match Normal /^.*$/
  endif
endfunction


"-----------------------------------------------------------------------------
function! s:SortByMultipleOrder(i1, i2)
  if exists('a:i1.order') && exists('a:i2.order')
    for i in range(min([len(a:i1.order), len(a:i2.order)]))
      if     a:i1.order[i] > a:i2.order[i]
        return +1
      elseif a:i1.order[i] < a:i2.order[i]
        return -1
      endif
    endfor
  endif
  return 0
endfunction

"-----------------------------------------------------------------------------
function! s:GetPathSeparatorIfDirectory(path)
  if isdirectory(a:path)
    return s:path_separator
  else
    return ''
  endif
endfunction

"-----------------------------------------------------------------------------
function! s:GetCurrentTagFiles()
  return sort(filter(map(tagfiles(), 'fnamemodify(v:val, '':p'')'), 'filereadable(v:val)'))
endfunction

" }}}1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" OBJECTS:
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" FuzzyFinderMode: Base: launch {{{1
let g:FuzzyFinderMode = { 'Base' : {} }
"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Base.launch(initial_text, partial_matching)
  call self.extend_options()
  if !self.mode_available
    echo 'This mode is not available: ' . self.to_str()
    return
  endif

  let s:last_col = -1

  call self.read_info_from_file()
  let self.partial_matching = a:partial_matching

  " the fuzzyfinder-buffer was already created?
  if exists('s:buf_nr')
    execute 'buffer ' . s:buf_nr
    delete _
  else
    let s:tag_files = s:GetCurrentTagFiles() " to get local value of current buffer

    1new
    let s:buf_nr = bufnr('%')

    " suspend autocomplpop.vim
    if exists(':AutoComplPopLock')
      :AutoComplPopLock
    endif

    " global setting
    call s:SetTemporaryOption('completeopt', 'menuone')
    call s:SetTemporaryOption('ignorecase', self.ignore_case)

  endif

  " local setting
  setlocal filetype=fuzzyfinder
  setlocal bufhidden=wipe
  setlocal buftype=nofile
  setlocal noswapfile
  setlocal nobuflisted
  setlocal modifiable
  let &l:completefunc = self.make_complete_func('CompleteFunc')

  " local autocommands
  augroup FuzzyFinderLocalAutoCommand
    autocmd!
    execute 'autocmd CursorMovedI <buffer>        call ' . self.to_str('on_cursor_moved_i()')
    execute 'autocmd InsertLeave  <buffer> nested call ' . self.to_str('on_insert_leave()'  )
    execute 'autocmd BufLeave     <buffer>        call ' . self.to_str('on_buf_leave()'     )
  augroup END

  " local mapping
  for [lhs, rhs] in [
        \   [ self.key_open       , self.to_str('on_cr(0, 0)'            ) ],
        \   [ self.key_open_split , self.to_str('on_cr(1, 0)'            ) ],
        \   [ self.key_open_vsplit, self.to_str('on_cr(2, 0)'            ) ],
        \   [ '<BS>'              , self.to_str('on_bs()'                ) ],
        \   [ '<C-h>'             , self.to_str('on_bs()'                ) ],
        \   [ self.key_next_mode  , self.to_str('on_switch_mode(+1)'     ) ],
        \   [ self.key_prev_mode  , self.to_str('on_switch_mode(-1)'     ) ],
        \   [ self.key_ignore_case, self.to_str('on_switch_ignore_case()') ],
        \ ]
    " hacks to be able to use feedkeys().
    execute printf('inoremap <buffer> <silent> %s <C-r>=%s ? "" : ""<CR>', lhs, rhs)
  endfor

  call s:HighlightError(0)
  call self.on_mode_enter()

  " Starts insert mode and makes CursorMovedI event now. Command prompt is
  " needed to forces a completion menu to update every typing.
  call setline(1, s:prompt . a:initial_text)
  call feedkeys("i\<End>", 'n')
endfunction

" FuzzyFinderMode: Base: EVENT HANDLERS {{{1
"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Base.on_cursor_moved_i()
  if !s:ExistsPrompt(getline('.'))
    " if command prompt is removed
    call setline('.', s:prompt . getline('.'))
    call feedkeys(repeat("\<Right>", len(s:prompt)), 'n')
  elseif col('.') <= len(s:prompt)
    " if the cursor is moved before command prompt
    call feedkeys(repeat("\<Right>", len(s:prompt) - col('.') + 1), 'n')
  elseif col('.') > strlen(getline('.')) && col('.') != s:last_col
    " if the cursor is placed on the end of the line and has been actually moved.
    let s:last_col = col('.')
    call feedkeys("\<C-x>\<C-u>", 'n')
  endif
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Base.on_insert_leave()
  call self.empty_cache_if_existed(0)
  call self.on_mode_leave()

  " switchs to next mode, or finishes fuzzyfinder.
  if exists('s:reserved_switch_mode')
    let m = self.next_mode(s:reserved_switch_mode < 0)
    call m.launch(s:RemovePrompt(getline('.')), self.partial_matching)
    unlet s:reserved_switch_mode
  else
    close
  endif
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Base.on_buf_leave()
  " resume autocomplpop.vim
  if exists(':AutoComplPopUnlock')
    :AutoComplPopUnlock
  endif

  call s:SetTemporaryOption('completeopt')
  call s:SetTemporaryOption('ignorecase')
  unlet s:buf_nr

  ":close " closes when other window clicked without leaving a insert mode.

  if exists('s:reserved_command')
    let command = self.on_open(s:reserved_command[0], s:reserved_command[1])
    unlet s:reserved_command
  else
    let command = ""
  endif

  call garbagecollect()

  call feedkeys(command, 'n')
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Base.on_buf_enter()
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Base.on_buf_write_post()
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Base.on_cr(index, check_dir)
  if pumvisible()
    call feedkeys(printf("\<C-y>\<C-r>=%s(%d, 1) ? '' : ''\<CR>", self.to_str('on_cr'), a:index), 'n')
  elseif !a:check_dir || getline('.') !~ '[/\\]$'
    let s:reserved_command = [s:RemovePrompt(getline('.')), a:index]
    call feedkeys("\<Esc>", 'n')
  endif
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Base.on_bs()
  call feedkeys((pumvisible() ? "\<C-e>\<BS>" : "\<BS>"), 'n')
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Base.on_mode_leave()
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Base.on_open(expr, mode)
  return [':edit ',
        \ ':split ',
        \ ':vsplit '][a:mode] . a:expr . "\<CR>"
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Base.on_switch_mode(next_prev)
  let s:reserved_switch_mode = a:next_prev
  call feedkeys("\<Esc>", 'n')
endfunction

"-----------------------------------------------------------------------------

function! g:FuzzyFinderMode.Base.on_switch_ignore_case()
  let &ignorecase = !&ignorecase
  echo "ignorecase = " . &ignorecase
  let s:last_col = -1
  call self.on_cursor_moved_i()
endfunction

" FuzzyFinderMode: Base: INFO FILE {{{1
"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Base.read_info_from_file()
  let s:info = {}

  try
    let lines = filter(map(readfile(expand(self.info_file)),
          \                'matchlist(v:val, "^\\([^\\t]\\+\\)\\t\\(.*\\)$")'),
          \            '!empty(v:val)')
  catch /.*/ 
    return
  endtry

  let lines = filter(map(readfile(expand(self.info_file)),
        \                'matchlist(v:val, "^\\([^\\t]\\+\\)\\t\\(.*\\)$")'),
        \            '!empty(v:val)')

  for line in lines
    if line[1] == 'version' && line[2] != s:info_version
      echo printf(s:msg_rm_info, expand(self.info_file))
      let self.info_file = ''
      return
    endif

    let s:info[line[1]] = (exists('s:info[line[1]]') ? s:info[line[1]] : []) + [ eval(line[2]) ]
  endfor
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Base.write_info_to_file()
  let s:info.version = [s:info_version]
  let lines = []
  for [key, value] in items(s:info)
    let lines += map(copy(value), 'key . "\t" . string(v:val)')
  endfor

  try
    call writefile(lines, expand(self.info_file))
  catch /.*/ 
  endtry
endfunction

" FuzzyFinderMode: Base: COMPLETION {{{1
"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Base.complete(findstart, base)
  if a:findstart 
    return 0
  elseif  !s:ExistsPrompt(a:base) || len(s:RemovePrompt(a:base)) < self.min_length
    return []
  endif

  call s:HighlightError(0)

  "" SEGV!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  "let result = sort(s:Concat(map(s:ExpandAbbrevMap(s:RemovePrompt(a:base),
  "      \                                                self.abbrev_map),
  "      \                           'self.on_complete(v:val)')),
  "      \           's:SortByMultipleOrder')
  let result = []
  for expanded_base in s:ExpandAbbrevMap(s:RemovePrompt(a:base), self.abbrev_map)
    let result += self.on_complete(expanded_base)
  endfor
  call sort(result, 's:SortByMultipleOrder')


  if empty(result)
    call s:HighlightError(1)
  else
    call feedkeys("\<C-p>\<Down>", 'n')
  endif

  return result
endfunction

"-----------------------------------------------------------------------------
" This function is set to 'completefunc' which doesn't accept dictionary-functions.
function! g:FuzzyFinderMode.Base.make_complete_func(name)
  execute printf("function! s:%s(findstart, base)\n" .
        \        "  return %s.complete(a:findstart, a:base)\n" .
        \        "endfunction", a:name, self.to_str())
  return s:sid_prefix . a:name
endfunction

"-----------------------------------------------------------------------------
" fuzzy  : 'str' -> {'base':'str', 'wi':'*s*t*r*', 're':'\V\.\*s\.\*t\.\*r\.\*'}
" partial: 'str' -> {'base':'str', 'wi':'*str*', 're':'\V\.\*str\.\*'}
function! g:FuzzyFinderMode.Base.make_pattern(base)
  if self.partial_matching
    let wi = (a:base !~ '^[*?]'  ? '*' : '') . a:base .
          \  (a:base =~ '[^*?]$' ? '*' : '')
    let re = s:ConvertWildcardToRegexp(wi)
    return { 'base': a:base, 'wi':wi, 're': re }
  else
    let wi = ''
    for char in split(a:base, '\zs')
      if wi !~ '[*?]$' && char !~ '[*?]'
        let wi .= '*'. char
      else
        let wi .= char
      endif
    endfor

    if wi !~ '[*?]$'
      let wi .= '*'
    endif

    let re = s:ConvertWildcardToRegexp(wi)

    if self.migemo_support && a:base !~ '[^\x01-\x7e]'
      let re .= '\|\m.*' . substitute(migemo(a:base), '\\_s\*', '.*', 'g') . '.*'
    endif

    return { 'base': a:base, 'wi':wi, 're': re }
  endif
endfunction

"-----------------------------------------------------------------------------
" glob with caching-feature, etc.
function! g:FuzzyFinderMode.Base.glob_ex(dir, file, excluded, matching_limit)
  let key = (a:dir =~ '\S' ? a:dir : ' ')

  call extend(self, { 'cache' : {} }, 'keep')
  if !exists('self.cache[key]')
    echo 'Caching file list...'
    let dirs = (key =~ '\S' ? split(expand(a:dir), "\n") : [""])
    let self.cache[key] = s:Concat(map(copy(dirs), 'split(glob(v:val . ".*"), "\n") + ' .
          \                               'split(glob(v:val . "*" ), "\n")'))

    if len(dirs) <= 1
      call map(self.cache[key], '[a:dir, s:SplitPath(v:val)[1], s:GetPathSeparatorIfDirectory(v:val)]')
    else
      call map(self.cache[key], 's:SplitPath(v:val) + [s:GetPathSeparatorIfDirectory(v:val)]')
    endif

    if len(a:excluded)
      call filter(self.cache[key], 'v:val[0] . v:val[1] . v:val[2] !~ a:excluded')
    endif
  endif

  echo 'Filtering file list...'
  return map(s:FilterEx(self.cache[key], 'v:val[1] =~ ' . string(a:file), a:matching_limit),
        \    'v:val[0] . v:val[1] . v:val[2]')
endfunction


" FuzzyFinderMode: Base: MISC {{{1
"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Base.empty_cache_if_existed(force)
  if exists('self.cache') && (a:force || !exists('self.lasting_cache') || !self.lasting_cache)
    unlet self.cache
    "let self.cache = (type(self.cache) == type({}) ? {} :
    "      \           type(self.cache) == type([]) ? [] :
    "      \           type(self.cache) == type('') ? '' : 0)
  endif
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Base.to_key()
  return filter(keys(g:FuzzyFinderMode), 'g:FuzzyFinderMode[v:val] is self')[0]
endfunction

"-----------------------------------------------------------------------------
" returns 'g:FuzzyFinderMode.{key}{.argument}'
function! g:FuzzyFinderMode.Base.to_str(...)
  return 'g:FuzzyFinderMode.' . self.to_key() . (a:0 > 0 ? '.' . a:1 : '')
endfunction

"-----------------------------------------------------------------------------
" takes in g:FuzzyFinderOptions
function! g:FuzzyFinderMode.Base.extend_options()
  let n = filter(keys(g:FuzzyFinderMode), 'g:FuzzyFinderMode[v:val] is self')[0]
  call extend(self, g:FuzzyFinderOptions.Base, 'force')
  call extend(self, g:FuzzyFinderOptions[self.to_key()], 'force')
endfunction


"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Base.next_mode(rev)
  let modes = (a:rev ? s:GetAvailableModes() : reverse(s:GetAvailableModes()))
  let m_last = modes[-1]
  for m in modes
    if m is self
      break
    endif
    let m_last = m
  endfor
  return m_last
  " vim crashed using map()
endfunction

" }}}2

" FuzzyFinderMode: Buffer: {{{1
let g:FuzzyFinderMode.Buffer = copy(g:FuzzyFinderMode.Base)
"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Buffer.on_complete(base)
  let patterns = self.make_pattern(a:base)

  echo 'pattern:' . patterns.wi . (self.migemo_support ? ' + migemo' : '')

  return map(filter(s:GetBuffers(),
        \           'v:val.nr != s:buf_nr && v:val.ind !~ self.excluded_indicator && ' .
        \           '(v:val.nr == patterns.base || v:val.path =~ patterns.re)'),
        \    's:MakeCompletionItem(v:val.path, v:val.nr, v:val.ind . v:val.path, "", a:base, 1)')
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Buffer.on_open(expr, mode)
  " attempts to convert the path to the number for handling unnamed buffer
  let buf = a:expr
  for buf_info in s:GetBuffers()
    if buf == buf_info.path
      let buf = buf_info.nr
      break
    endif
  endfor

  return [':buffer ',
        \ ':sbuffer ',
        \ ':vertical :sbuffer '][a:mode] . buf . "\<CR>"
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Buffer.on_mode_enter()
  execute 'file [FuzzyFinder - Buffer]'
endfunction


" FuzzyFinderMode: File: {{{1
let g:FuzzyFinderMode.File = copy(g:FuzzyFinderMode.Base)
"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.File.on_complete(base)
  let patterns = map(s:SplitPath(a:base), 'self.make_pattern(v:val)')

  let result = self.glob_ex(patterns[0].base, patterns[1].re, self.excluded_path, self.matching_limit)

  if len(result) >= self.matching_limit
    call s:HighlightError(1)
  endif

  echo 'pattern:' . patterns[0].base . patterns[1].wi . (self.migemo_support ? ' + migemo' : '')

  return map(result, 's:MakeCompletionItem(v:val, -1, v:val, "", a:base, 1)')

  return result
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.File.on_mode_enter()
  execute 'file [FuzzyFinder - File]'
endfunction


" FuzzyFinderMode: MruFile: {{{1
let g:FuzzyFinderMode.MruFile = copy(g:FuzzyFinderMode.Base)
"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.MruFile.on_complete(base)
  let patterns = self.make_pattern(a:base)

  if !has_key(s:info, 'mru_file')
    return []
  endif

  echo 'pattern:' . patterns.wi . (self.migemo_support ? ' + migemo' : '')

  return map(filter(copy(self.cache), 'v:val[0] == patterns.base || v:val[1].path =~ patterns.re'),
        \    's:MakeCompletionItem(v:val[1].path, v:val[0], v:val[1].path, v:val[1].time, a:base, 1)')
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.MruFile.on_mode_enter()
  execute 'file [FuzzyFinder - MRU File]'

  let self.cache = s:MakeNumberedList(map(copy(s:info.mru_file),
        \                                    '{ ''path'' : fnamemodify(v:val.path, '':~:.''),' .
        \                                    '  ''time'' : strftime(self.time_format, v:val.time) }'),
        \                                1)
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.MruFile.on_buf_enter()
  call self.update_info()
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.MruFile.on_buf_write_post()
  call self.update_info()
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.MruFile.update_info()
  call self.read_info_from_file()

  " current-buffer information
  let cur_buf = {
        \   'path' : (!self.no_special_buffer || empty(&buftype) ? expand('%:p') : ''),
        \   'time' : localtime()
        \ }

  call extend(s:info, { 'mru_file' : [] }, 'keep')
  let s:info.mru_file = filter(insert(filter(s:info.mru_file,'v:val.path != cur_buf.path'), cur_buf),
        \                      'v:val.path !~ self.excluded_path && filereadable(v:val.path)'
        \                     )[0 : self.max_item - 1]

  call self.write_info_to_file()
endfunction


" FuzzyFinderMode: FavFile: {{{1
let g:FuzzyFinderMode.FavFile = copy(g:FuzzyFinderMode.Base)
"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.FavFile.on_complete(base)
  let patterns = self.make_pattern(a:base)

  if !has_key(s:info, 'fav_file')
    return []
  endif

  echo 'pattern:' . patterns.wi . (self.migemo_support ? ' + migemo' : '')

  return map(filter(s:MakeNumberedList(copy(s:info.fav_file), 1),
        \           'v:val[0] == patterns.base || v:val[1].path =~ patterns.re'),
        \    's:MakeCompletionItem(v:val[1].path, v:val[0], v:val[1].path,
        \                             strftime(self.time_format,
        \                                      v:val[1].time),
        \                             a:base, 1)'
        \   )
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.FavFile.on_mode_enter()
  execute 'file [FuzzyFinder - Favorite File]'
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.FavFile.add(in_file, adds)
  call self.read_info_from_file()

  let file = fnamemodify((empty(a:in_file) ? expand('%') : a:in_file), ':p:~')

  call extend(s:info, { 'fav_file' : [] }, 'keep')
  let s:info.fav_file = filter(s:info.fav_file, 'v:val.path != file')

  if a:adds
    let s:info.fav_file = add(s:info.fav_file, { 'path' : file, 'time' : localtime() })
  endif

  call self.write_info_to_file()
endfunction


" FuzzyFinderMode: Dir: {{{1
let g:FuzzyFinderMode.Dir = copy(g:FuzzyFinderMode.Base)
"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Dir.on_complete(base)
  let patterns = map(s:SplitPath(a:base), 'self.make_pattern(v:val)')

  let result = filter(self.glob_ex(patterns[0].base, patterns[1].re, self.excluded_path, 0),
        \             'v:val =~ ''[/\\]$''')

  if len(patterns[1].base) == 0
    call insert(result, patterns[0].base)
  endif

  call map(result, 's:MakeCompletionItem(v:val, -1, v:val, "", a:base, 1)')

  if len(patterns[1].base) == 0
    let result[0].word = matchstr(result[0].word, '^.*[^/\\]')
  endif

  echo 'pattern:' . patterns[0].base . patterns[1].wi . (self.migemo_support ? ' + migemo' : '')

  return result
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Dir.on_open(expr, mode)
  return ':cd ' . a:expr . ["\<CR>",
        \                   "",
        \                   ""][a:mode]
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Dir.on_mode_enter()
  execute 'file [FuzzyFinder - Directory]'
endfunction


" FuzzyFinderMode: Tag: {{{1
let g:FuzzyFinderMode.Tag = copy(g:FuzzyFinderMode.Base)
"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Tag.on_complete(base)
  let patterns = self.make_pattern(a:base)

  let result = self.find_tag(patterns.re, self.matching_limit)

  if len(result) >= self.matching_limit
    call s:HighlightError(1)
  endif

  echo 'pattern:' . patterns.wi . (self.migemo_support ? ' + migemo' : '')

  return map(result,  's:MakeCompletionItem(v:val, -1, v:val, "", a:base, 1)')
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Tag.on_open(expr, mode)
  return [':tjump ',
        \ ':stjump ',
        \ ':vertical :stjump '][a:mode] . a:expr . "\<CR>"
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Tag.on_mode_enter()
  execute 'file [FuzzyFinder - Tag]'
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.Tag.find_tag(pattern, matching_limit)
  if !len(s:tag_files)
    return []
  endif

  let key = join(s:tag_files, "\n")

  " cache not created or tags file updated? 
  call extend(self, { 'cache' : {} }, 'keep')
  if !exists('self.cache[key]') || len(filter(map(copy(s:tag_files), 'getftime(v:val)'),
        \                                     'v:val >= self.cache[key].time'))
    echo 'Caching tag list...'
    let self.cache[key] = {
          \   'time' : localtime(),
          \   'data' : s:Unique(s:Concat(map(copy(s:tag_files), 's:GetTagList(v:val)'))),
          \ }
  endif

  echo 'Filtering tag list...'
  return s:FilterEx(self.cache[key].data, 'v:val =~ ' . string(a:pattern), a:matching_limit)
endfunction


" FuzzyFinderMode: TaggedFile: {{{1
let g:FuzzyFinderMode.TaggedFile = copy(g:FuzzyFinderMode.Base)
"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.TaggedFile.on_complete(base)
  let patterns = self.make_pattern(a:base)

  echo 'Making tagged file list...'
  let result = self.find_tagged_file(patterns.re, self.matching_limit)

  if len(result) >= self.matching_limit
    call s:HighlightError(1)
  endif

  echo 'pattern:' . patterns.wi . (self.migemo_support ? ' + migemo' : '')

  return map(result,  's:MakeCompletionItem(v:val, -1, v:val, "", a:base, 1)')
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.TaggedFile.on_mode_enter()
  execute 'file [FuzzyFinder - Tagged File]'
endfunction

"-----------------------------------------------------------------------------
function! g:FuzzyFinderMode.TaggedFile.find_tagged_file(pattern, matching_limit)
  if !len(s:tag_files)
    return []
  endif

  let key = join(s:tag_files, "\n")

  " cache not created or tags file updated? 
  call extend(self, { 'cache' : {} }, 'keep')
  if !exists('self.cache[key]') || len(filter(map(copy(s:tag_files), 'getftime(v:val)'),
        \                                     'v:val >= self.cache[key].time'))
    echo 'Caching tagged-file list...'
    let self.cache[key] = {
          \   'time' : localtime(),
          \   'data' : s:Unique(s:Concat(map(copy(s:tag_files), 's:GetTaggedFileList(v:val)'))),
          \ }
  endif

  echo 'Filtering tagged-file list...'
  return s:FilterEx(map(self.cache[key].data, 'fnamemodify(v:val, '':.'')'),
        \               'v:val =~ ' . string(a:pattern),
        \           a:matching_limit)

endfunction


" }}}1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" INITIALIZE:
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

call s:Initialize()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim:fdm=marker

