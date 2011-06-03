" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/tplugin.txt	[[[1
378
*tplugin.txt*       A simple plugin loader/runtimepath manager
                    Author: Tom Link, micathom at gmail com

This plugin helps users to avoid loading plugins on startup but only 
when they are actually needed. It also allows users to maintain their 
plugins in an alternative directory layout.

Usually, all plugins are installed under one root directory (see 
|vimfiles|). This makes it potentially cumbersome to install a plugin 
that is distributed as zip/tgz or is downloaded from a source code 
repository. It also makes it more difficult than necessary to 
temporarily use a plugin, e.g., for testing purposes.

tplugin allows users to install plugins in an alternative directory 
structure where each plugin (consequently referred to as "repo") is 
installed in a subdirectory. tplugin takes care of modifying 
'runtimepath' and of loading the vim script files as necessary.

Alternative directory layout:

    ROOT/
      repo1/
        after/
        autoload/
        doc/
        plugin/
      repo2/
        after/
        autoload/
        doc/
        plugin/
      ...

Repos can be enabled (i.e. added to 'runtimepath') either explicitly 
with the |:TPlugin| command or automatically by calling a command, 
function, or by requesting a filetype plugin/syntax/indent file defined 
in a repo -- i.e. tplugin also provides an autoload facility similar to 
the AsNeeded plugin (vimscript #915). See below for details.

The main purpose of this plugin was to facilitate the use of plugins 
that are downloaded from a VCS like git, svn etc. But it can also be 
used to handle plugins distributed as zip or tgz archives. For vimballs, 
you'll might have to use a non-standard vimball unpacker---like the one 
that can be found in the vimtlib repository.

You can have more than one root directory although it currently (as of 
version 0.2) could cause problems and is deprecated. 


Usage~
                                                    *tplugin-autoload*
With autoload:

    By default, tplugin has autoload enabled. With autoload, you can use 
    tplugin by (1) loading the macro in your |vimrc| file and then (2) 
    (optional step) setting non-standard root directories. The standard 
    root-directory is ~/vimfiles/repos (or ~/.vim/repos) and does not 
    have to be explicitely declared.

    Example: >
        
        runtime macros/tplugin.vim
        " Only necessary if you use a non-standard root directory
        TPluginRoot /home/x/src/repos

<   (3) Collect the autoload information by running the |:TPluginScan| 
    command. (4) Restart vim.

    The plugins will be loaded as soon as one of its functions or 
    commands is called for the first time.

    When using autoload, for every known ROOT (see |:TPluginRoot|), the 
    file ROOT/_tplugin.vim is loaded on startup. This file is created by 
    the |:TPluginScan| command and should not be edited by the user.

                            *tplugin-dependencies* *tplugin-metadata*
    When enabling a repository (ROOT/REPO), e.g., by calling |:TPlugin|, 
    the files ROOT/_tplugin_REPO.vim and ROOT/REPO/_tplugin.vim are 
    loaded. These files allows users (or plugin developers) to, e.g., 
    define dependencies (see |:TPluginBefore| and |:TPluginAfter|).


Without autoload:

    (1) Load the macro and (2) set non-standard root directories. In 
    addition, (3) load plugins as desired.

    Example: >

        runtime macros/tplugin.vim
        " Only necessary if you use a non-standard root directory
        TPluginRoot /home/x/src/repos
        " Load all plugins in repo1
        TPlugin repo1
        " Load only specified plugins from repo2
        TPlugin repo2 foo bar

<   It is advantageous to source the macro rather at the top of your 
    vimrc file before making any changes to the 'runtimepath'. If you 
    don't change the 'runtimepath', the position is of no importance.

    Then install your plugins/repos in the root directory. After 
    restarting vim, type :TPluginScan!

    Full example as it could be found in a |vimrc| file: >

        runtime macros/tplugin.vim
        TPluginRoot F:\repos
        TPlugin vim-surround
        let g:snippets_dir = 'X:\vimfiles\snippets,F:\repos\snipmate.vim\snippets'
        TPlugin snipmate.vim
        TPlugin! vimtlib 02tlib


CAVEAT: If you have a file after/plugin/foo.vim in you user vimfiles 
directory, this file will be loaded before any "managed" plugin "foo". 
You thus have to check in your after/plugin/foo.vim file if the plugin 
was already loaded.

Contrary to the default plugin behaviour, those managed plugins are 
loaded when the |VimEnter| event is triggered, i.e. after all plugins 
(incl. the after directory) and |gvimrc| was loaded. This approach 
provides for a better control of which plugins should be loaded. You can 
load a managed plugin immediately by adding a bang '!' to the |:TPlugin| 
command.

                                                    *tplugin-asneeded*
AsNeeded Users~

tplugin can serve as a drop-in replacement for the AsNeeded plugin. Add 
the following command to you |vimrc| file: >

    TPluginRoot ~/vimfiles/AsNeeded/*

Alternatively, move the files the AsNeeded subdirectory to 
REPOS/AsNeeded/plugin/.

Run |:TPluginScan| in order to create autoload definitions for commands 
and functions.

Contrary to the AsNeeded plugin, tplugin only supports autoload for 
|<Plug>| type of maps. Other autoload maps have to be defined by the 
user using the |TPluginMap()| function.


Related work~

    - Marc Weber's vim-addon-manager (my main source of inspiration for 
      this script), which also provides downloading plugins and 
      automatic dependency management:
      http://github.com/MarcWeber/vim-plugin-manager (vimscript #2905)

    - Tim Pope's pathogen (vimscript #2332).

    - Charles E. Campbell's AsNeeded (vimscript #915)


-----------------------------------------------------------------------
Install~

Edit the vba file and type: >

    :so %

See :help vimball for details. If you have difficulties or use vim 7.0, 
please make sure, you have the current version of vimball
(vimscript #1502) installed or update your runtime.

Optional: If tlib (vimscript #1863) is available, its progressbar will 
be used to visualize the scan progress.

Also available via git: http://github.com/tomtom/vimtlib/


========================================================================
Contents~

        g:tplugin_autoload ........... |g:tplugin_autoload|
        g:tplugin_autoload_exclude ... |g:tplugin_autoload_exclude|
        g:tplugin_helptags ........... |g:tplugin_helptags|
        g:tplugin_menu_prefix ........ |g:tplugin_menu_prefix|
        g:tplugin_scan ............... |g:tplugin_scan|
        :TPlugin ..................... |:TPlugin|
        :TPluginRoot ................. |:TPluginRoot|
        :TPluginScan ................. |:TPluginScan|
        :TPluginBefore ............... |:TPluginBefore|
        :TPluginAfter ................ |:TPluginAfter|
        TPluginMap ................... |TPluginMap()|
        TPluginAutoload .............. |TPluginAutoload()|
        TPluginFunction .............. |TPluginFunction()|
        TPluginCommand ............... |TPluginCommand()|


========================================================================
macros/tplugin.vim~

                                                    *g:tplugin_autoload*
g:tplugin_autoload             (default: 1)
    Enable autoloading. See |:TPluginScan|, |:TPluginCommand|, and 
    |:TPluginFunction|.
    Values:
      1 ... Enable autoload (default)
      2 ... Enable autoload and automatically run |:TPluginScan| 
            after updating tplugin.

                                                    *g:tplugin_autoload_exclude*
g:tplugin_autoload_exclude     (default: [])
    A list of repositories for which autoload is disabled when running 
    |:TPluginScan|.

                                                    *g:tplugin_helptags*
g:tplugin_helptags             (default: 1)
    If non-nil and a repo contains no helptags file, generate helptags 
    for the repository's doc subdirectory.
    
    See also |g:tplugin_scan|.

                                                    *g:tplugin_menu_prefix*
g:tplugin_menu_prefix          (default: '')
    If autoload is enabled and this variable is non-empty, build a 
    menu with available plugins.
    Menus are disabled by default because they are less useful 
    than one might think with autoload enabled.
    A good choice for this variable would be, e.g., 
    'Plugin.T&Plugin.'.
    NOTE: You have to re-run |:TPluginScan| after setting this 
    value.

                                                    *g:tplugin_scan*
g:tplugin_scan                 (default: 'cfpta')
    The default value for |:TPluginScan|. A set of identifiers 
    determining the information being collected:
       c ... commands
       f ... functions
       p ... <plug> maps
       t ... filetypes
       h ... helptags (always regenerate helptags, see also |g:tplugin_helptags|)
       a ... autoload
       all ... all of the above

                                                    *:TPlugin*
:TPlugin[!] REPOSITORY [PLUGINS ...]
    Register certain plugins for being sourced at |VimEnter| time.
    See |tplugin.txt| for details.
    
    With the optional '!', the plugin will be loaded immediately.
    In interactive use, i.e. once vim was loaded, plugins will be loaded 
    immediately anyway.
    
    IF REPOSITORY contains a slash or a backslash, it is considered the 
    path relative from the current root directory to the plugin directory. 
    This allows you to deal with repositories with a non-standard 
    directory layout. Otherwise it is assumed that the source files are 
    located in the "plugin" subdirectory.
    
    IF PLUGIN is "-", the REPOSITORY will be enabled but no plugin will be 
    loaded.

                                                    *:TPluginRoot*
:TPluginRoot DIRECTORY
    Define the root directory for the following |:TPlugin| commands.
    Read autoload information if available (see |g:tplugin_autoload| and 
    |:TPluginScan|).
    
    If DIRECTORY ends with "*", it doesn't refer to a directory hierarchy 
    � la vimfiles but to a single "flat" directory.
    
    Example: >
      " A collection of git repositories
      TPluginRoot ~/src/git_repos
      " A directory with experimental plugins
      TPluginRoot ~/vimfiles/experimental_plugins/*
<

                                                    *:TPluginScan*
:TPluginScan[!] [WHAT] [ROOT]
    Scan the current root directory for commands and functions. Save 
    autoload information in "ROOT/_tplugin.vim".
    
    Where WHAT is a set of letters determining the information being 
    collected. See |g:tplugin_scan| for details.
    
    With the optional '!', the autocommands are immediatly usable.
    
    Other than the AsNeeded plugin, tplugin doesn't support the creation 
    of autoload information for maps.
    
    If you collect repositories in one than more directory, I'd suggest to 
    create a special script.
    
    The source file may contain special markers that make :TPluginScan 
    include text in the _tplugin.vim file:
                                                        *#TPluginInclude*
    Blocks of non-empty lines are introduced with an #TPluginInclude tag: >
    
      " #TPluginInclude
      augroup Foo
           autocmd!
           autocmd Filetype foo call foo#Init()
      augroup END
    
<   Special lines are prefixed with #TPluginInclude: >
      
      " #TPluginInclude if !exists('g:foo') | let g:foo = 1 | endif
    
<   Example: >
      TPluginRoot dir1
      TPluginScan
      TPluginRoot dir2
      TPluginScan
<

                                                    *:TPluginBefore*
:TPluginBefore FILE_RX [GLOB_PATTERNS ...]
    Load dependencies given as GLOB_PATTERNS (see |wildcards|) before 
    loading a file matching the |regexp| pattern FILE_RX.
    
    The files matching FILE_PATTERNS are loaded after the repo's path is 
    added to the 'runtimepath'. You can thus use partial filenames as you 
    would use for the |:runtime| command.
    
    This command should be best put into ROOT/tplugin_REPO.vim files, 
    which are loaded when enabling a source repository.
    
    Example: >
      " Load master.vim before loading any plugin in a repo
      TPluginBefore plugin/.\{-}\.vim plugin/master.vim
<

                                                    *:TPluginAfter*
:TPluginAfter FILE_RX [GLOB_PATTERNS ...]
    Load other plugins matching GLOB_PATTERNS (see |wildcards|) after 
    loading a file matching the |regexp| pattern FILE_RX.
    See also |:TPluginBefore|.
    
    Example: >
      " Load auxiliary plugins after loading master.vim
      TPluginAfter plugin/master\.vim plugin/sub_*.vim
<

                                                    *TPluginMap()*
TPluginMap(map, repo, plugin, ?remap="")
    MAP is a map command and the map. REPO and PLUGIN are the same as for 
    the |:TPlugin| command.
    
    Examples: >
      " Map for <plug>Foo:
      call TPluginMap('map <plug>Foo', 'mylib', 'myplugin')
    
      " Load the plugin when pressing <f3> and remap the key to an appropriate 
      " command from the autoloaded plugin:
      call TPluginMap('map <f3>', 'mylib', 'myplugin', ':Foo<cr>')
<

                                                    *TPluginAutoload()*
TPluginAutoload(prefix, def)

                                                    *TPluginFunction()*
TPluginFunction(FUNCTION, REPOSITORY, [PLUGIN])
    Load a certain plugin on demand (aka autoload) when FUNCTION is called 
    for the first time.

                                                    *TPluginCommand()*
TPluginCommand(COMMAND, REPOSITORY, [PLUGIN])
    Load a certain plugin on demand (aka autoload) when COMMAND is called 
    for the first time. Then call the original command.
    
    For most plugins, |:TPluginScan| will generate the appropriate 
    TPluginCommand commands for you. For some plugins, you'll have to 
    define autocommands yourself in the |vimrc| file.
    
    Example: >
      TPluginCommand TSelectBuffer vimtlib tselectbuffer
<



vim:tw=78:fo=tcq2:isk=!-~,^*,^|,^":ts=8:ft=help:norl:
macros/tplugin.vim	[[[1
1214
" plugin.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-01-04.
" @Last Change: 2010-04-13.
" @Revision:    1394
" GetLatestVimScripts: 2917 1 :AutoInstall: tplugin.vim

if &cp || exists("loaded_tplugin")
    finish
endif
let loaded_tplugin = 8

let s:save_cpo = &cpo
set cpo&vim


if !exists('g:tplugin_autoload')
    " Enable autoloading. See |:TPluginScan|, |:TPluginCommand|, and 
    " |:TPluginFunction|.
    " Values:
    "   1 ... Enable autoload (default)
    "   2 ... Enable autoload and automatically run |:TPluginScan| 
    "         after updating tplugin.
    let g:tplugin_autoload = 1   "{{{2
endif


if !exists('g:tplugin_autoload_exclude')
    " A list of repositories for which autoload is disabled when running 
    " |:TPluginScan|.
    let g:tplugin_autoload_exclude = []   "{{{2
endif


if !exists('g:tplugin_helptags')
    " If non-nil and a repo contains no helptags file, generate helptags 
    " for the repository's doc subdirectory.
    "
    " See also |g:tplugin_scan|.
    let g:tplugin_helptags = 1   "{{{2
endif


" if !exists('g:tplugin_help')
"     " If non-nil, fully expand the 'runtimepath' when entering the 
"     " command line, so that all helptags are accessible to the |:help| 
"     " command.
"     let g:tplugin_help = 1   "{{{2
" endif


if !exists('g:tplugin_menu_prefix')
    " If autoload is enabled and this variable is non-empty, build a 
    " menu with available plugins.
    " Menus are disabled by default because they are less useful 
    " than one might think with autoload enabled.
    " A good choice for this variable would be, e.g., 
    " 'Plugin.T&Plugin.'.
    " NOTE: You have to re-run |:TPluginScan| after setting this 
    " value.
    let g:tplugin_menu_prefix = ''   "{{{2
    " let g:tplugin_menu_prefix = 'Plugin.T&Plugin.'   "{{{2
endif


if !exists('g:tplugin_scan')
    " The default value for |:TPluginScan|. A set of identifiers 
    " determining the information being collected:
    "    c ... commands
    "    f ... functions
    "    p ... <plug> maps
    "    t ... filetypes
    "    h ... helptags (always regenerate helptags, see also |g:tplugin_helptags|)
    "    a ... autoload
    "    all ... all of the above
    let g:tplugin_scan = 'cfpta'   "{{{2
    "    l ... loaded_* variables
endif


" :display: :TPlugin[!] REPOSITORY [PLUGINS ...]
" Register certain plugins for being sourced at |VimEnter| time.
" See |tplugin.txt| for details.
"
" With the optional '!', the plugin will be loaded immediately.
" In interactive use, i.e. once vim was loaded, plugins will be loaded 
" immediately anyway.
"
" IF REPOSITORY contains a slash or a backslash, it is considered the 
" path relative from the current root directory to the plugin directory. 
" This allows you to deal with repositories with a non-standard 
" directory layout. Otherwise it is assumed that the source files are 
" located in the "plugin" subdirectory.
"
" IF PLUGIN is "-", the REPOSITORY will be enabled but no plugin will be 
" loaded.
command! -bang -nargs=+ -complete=customlist,s:TPluginComplete TPlugin
            \ call TPluginRequire(!empty("<bang>"), '', <f-args>)


" :display: :TPluginRoot DIRECTORY
" Define the root directory for the following |:TPlugin| commands.
" Read autoload information if available (see |g:tplugin_autoload| and 
" |:TPluginScan|).
"
" If DIRECTORY ends with "*", it doesn't refer to a directory hierarchy 
" � la vimfiles but to a single "flat" directory.
"
" Example: >
"   " A collection of git repositories
"   TPluginRoot ~/src/git_repos
"   " A directory with experimental plugins
"   TPluginRoot ~/vimfiles/experimental_plugins/*
command! -nargs=+ -complete=dir TPluginRoot
            \ call s:SetRoot(<q-args>)



" :display: :TPluginScan[!] [WHAT] [ROOT]
" Scan the current root directory for commands and functions. Save 
" autoload information in "ROOT/_tplugin.vim".
"
" Where WHAT is a set of letters determining the information being 
" collected. See |g:tplugin_scan| for details.
"
" With the optional '!', the autocommands are immediatly usable.
"
" Other than the AsNeeded plugin, tplugin doesn't support the creation 
" of autoload information for maps.
"
" If you collect repositories in one than more directory, I'd suggest to 
" create a special script.
"
" The source file may contain special markers that make :TPluginScan 
" include text in the _tplugin.vim file:
"                                                     *#TPluginInclude*
" Blocks of non-empty lines are introduced with an #TPluginInclude tag: >
"
"   " #TPluginInclude
"   augroup Foo
"        autocmd!
"        autocmd Filetype foo call foo#Init()
"   augroup END
"
" Special lines are prefixed with #TPluginInclude: >
"   
"   " #TPluginInclude if !exists('g:foo') | let g:foo = 1 | endif
"
" Example: >
"   TPluginRoot dir1
"   TPluginScan
"   TPluginRoot dir2
"   TPluginScan
command! -bang -nargs=* TPluginScan
            \ call s:ScanRoots(!empty("<bang>"), s:roots, [<f-args>])


" :display: :TPluginBefore FILE_RX [GLOB_PATTERNS ...]
" Load dependencies given as GLOB_PATTERNS (see |wildcards|) before 
" loading a file matching the |regexp| pattern FILE_RX.
"
" The files matching FILE_PATTERNS are loaded after the repo's path is 
" added to the 'runtimepath'. You can thus use partial filenames as you 
" would use for the |:runtime| command.
"
" This command should be best put into ROOT/tplugin_REPO.vim files, 
" which are loaded when enabling a source repository.
"
" Example: >
"   " Load master.vim before loading any plugin in a repo
"   TPluginBefore plugin/.\{-}\.vim plugin/master.vim
command! -nargs=+ TPluginBefore
            \ let s:before[[<f-args>][0]] = [<f-args>][1:-1]


" :display: :TPluginAfter FILE_RX [GLOB_PATTERNS ...]
" Load other plugins matching GLOB_PATTERNS (see |wildcards|) after 
" loading a file matching the |regexp| pattern FILE_RX.
" See also |:TPluginBefore|.
"
" Example: >
"   " Load auxiliary plugins after loading master.vim
"   TPluginAfter plugin/master\.vim plugin/sub_*.vim
command! -nargs=+ TPluginAfter
            \ let s:after[[<f-args>][0]] = [<f-args>][1:-1]



function! s:FileJoin(...) "{{{3
    let parts = map(copy(a:000), 'substitute(v:val, ''[\/]\+$'', "", "")')
    return join(parts, '/')
endf


let s:roots = []
let s:rtp = split(&rtp, ',')
let s:reg = {}
let s:done = {'-': {}}
let s:immediate = 0
let s:before = {}
let s:after = {}
let s:ftypes = {}
let s:functions = {}
let s:autoloads = {}
let s:maps = {}
let s:command_nobang = {}
" let s:loaded_variables = {}
let s:tplugin_file = '_tplugin'


if exists('*fnameescape')
    function! s:FnameEscape(filename) "{{{3
        return fnameescape(a:filename)
    endf
else
    function! s:FnameEscape(filename) "{{{3
        let cs = " \t\n*?[{`$\\%#'\"|!<"
        return escape(a:filename, cs)
    endf
endif


function! s:Strip(string) "{{{3
    let string = substitute(a:string, '^\s\+', '', '')
    let string = substitute(string, '\s\+$', '', '')
    return string
endf


function! s:CommandKey(pluginfile) "{{{3
    " echom "DBG ". a:pluginfile
    return substitute(a:pluginfile, '\\', '/', 'g')
endf


function! s:DefineCommand(def1) "{{{3
    let [cmd0; file] = a:def1
    let string = s:Strip(cmd0)
    if match(string, '\s') == -1
        return 'command! -bang -range -nargs=* '. string
    else
        " let cmd = matchstr(a:string, '\s\zs\u\w*$')
        if string =~ '^com\%[mand]\zs\s'
            let pluginfile = s:GetPluginFile(s:GetRoot(), file[0], file[1])
            let pluginkey = s:CommandKey(pluginfile)
            if !has_key(s:command_nobang, pluginkey)
                let s:command_nobang[pluginkey] = {}
            endif
            let cmd = s:ExtractCommand(cmd0)
            if !has_key(s:command_nobang[pluginkey], cmd)
                let s:command_nobang[pluginkey][cmd] = 1
            endif
            let string = substitute(string, '^com\%[mand]\zs\s', '! ', '')
        endif
        return string
    endif
endf


function! s:ExtractCommand(cmd0) "{{{3
    return matchstr(a:cmd0, '\s\zs\u\w*$')
endf


" args: A string if type == 1, a list if type == 2
function! s:Autoload(type, def, bang, range, args) "{{{3
    " TLogVAR a:type, a:def, a:bang, a:range, a:args
    let [root, cmd0; file] = a:def
    let cmd0 = s:Strip(cmd0)
    if match(cmd0, '\s') != -1
        let cmd = s:ExtractCommand(cmd0)
    else
        let cmd = cmd0
    endif
    " TLogVAR root, cmd0, cmd, file
    if a:type == 1 " Command
        let pluginfile = s:GetPluginFile(root, file[0], file[1])
        call s:RemoveAutoloads(pluginfile, [cmd])
        " TLogDBG exists(':'. cmd)
    endif
    if len(file) >= 1 && len(file) <= 2
        call call('TPluginRequire', [1, root] + file)
    else
        echoerr 'Malformed autocommand definition: '. join(a:def)
    endif
    if a:type == 1 " Command
        " TLogDBG exists(':'. cmd)
        let range = join(filter(copy(a:range), '!empty(v:val)'), ',')
        " TLogDBG range . cmd . a:bang .' '. a:args
        try
            exec range . cmd . a:bang .' '. a:args
        catch /^Vim\%((\a\+)\)\=:E481/
            exec cmd . a:bang .' '. a:args
        endtry
    elseif a:type == 2 " Function
    elseif a:type == 3 " Map
    else
        echoerr 'Unsupported type: '. a:type
    endif
endf


" :nodoc:
function! TPluginFiletype(filetype, repos) "{{{3
    if !has_key(s:ftypes, a:filetype)
        let s:ftypes[a:filetype] = []
    endif
    let repos = map(copy(a:repos), 's:GetRoot() ."/". v:val')
    call extend(s:ftypes[a:filetype], repos)
endf


function! s:LoadFiletype(filetype) "{{{3
    " TLogVAR a:repos
    let repos = remove(s:ftypes, a:filetype)
    for repo in repos
        call TPluginRequire(1, repo, '.', '.')
    endfor
    exec 'setfiletype '. a:filetype
endf


function! s:AutoloadFunction(fn) "{{{3
    " echom "DBG fn =". a:fn
    " call tlog#Debug(has_key(s:functions, a:fn))
    " call tlog#Debug(string(keys(s:functions)))
    " call tlog#Debug(string(keys(s:functions)))
    if stridx(a:fn, '#') != -1
        let prefix = substitute(a:fn, '#[^#]\{-}$', '', '')
        " echom "DBG prefix = ". prefix
        " echom "DBG autoloads = ". string(keys(s:autoloads))
        if has_key(s:autoloads, prefix)
            " let def = s:autoloads[prefix]
            let def = remove(s:autoloads, prefix)
            call call('TPluginRequire', [1] + def)
            return 1
        endif
    endif
    if has_key(s:functions, a:fn)
        " TLogVAR a:fn
        let def = s:functions[a:fn]
        " TLogVAR def
        call s:Autoload(2, def, '', [], [])
        " Ignored
        return 1
    endif
endf


" :display: TPluginMap(map, repo, plugin, ?remap="")
" MAP is a map command and the map. REPO and PLUGIN are the same as for 
" the |:TPlugin| command.
"
" Examples: >
"   " Map for <plug>Foo:
"   call TPluginMap('map <plug>Foo', 'mylib', 'myplugin')
"
"   " Load the plugin when pressing <f3> and remap the key to an appropriate 
"   " command from the autoloaded plugin:
"   call TPluginMap('map <f3>', 'mylib', 'myplugin', ':Foo<cr>')
function! TPluginMap(map, repo, plugin, ...) "{{{3
    if g:tplugin_autoload
        let remap = a:0 >= 1 ? substitute(a:1, '<', '<lt>', 'g') : ''
        let def   = [s:GetRoot(), a:repo, a:plugin]
        let keys  = s:MapKeys(a:map)
        if empty(keys)
            let keys = matchstr(a:map, '\S\+$')
        endif
        if !empty(keys)
            let pluginfile = s:GetPluginFile(s:GetRoot(), a:repo, a:plugin)
            if !has_key(s:maps, pluginfile)
                let s:maps[pluginfile] = {}
            endif
            let s:maps[pluginfile][keys] = a:map
            let mode = s:MapMode(a:map)
            try
                let maparg = maparg(keys, mode)
                " TLogVAR maparg
            catch
                let maparg = ""
            endtry
            if empty(maparg)
                let map = substitute(a:map, '<script>', '', '')
                exec map .' <C-\><C-G>:call <SID>Remap('. join([string(keys), string(a:map), string(remap), string(def)], ',') .')<cr>'
            endif
        endif
    endif
endf


function! s:MapKeys(map) "{{{3
    return matchstr(a:map, '\c<plug>\w\+$')
endf


function! s:Unmap(map, keys) "{{{3
    let mode = s:MapMode(a:map)
    exec 'silent! '. mode .'unmap '. a:keys
endf


function! s:Remap(keys, map, remap, def) "{{{3
    " TLogVAR a:keys, a:map, a:def, a:remap
    call s:Unmap(a:map, a:keys)
    call call('TPluginRequire', [1] + a:def)
    if !empty(a:remap)
        " TLogDBG a:map .' '. a:remap
        exec a:map .' '. a:remap
    endif
    let keys = substitute(a:keys, '<\ze\w\+\(-\w\+\)*>', '\\<', 'g')
    let keys = eval('"'. escape(keys, '"') .'"')
    " TLogVAR keys, a:keys
    call feedkeys(keys)
endf


function! s:MapMode(map) "{{{3
    return matchstr(a:map, '\<\([incvoslx]\?\)\ze\(nore\)\?map')
endf


let s:scanner = {
            \ 'c': {
            \   'rx':  '^\s*:\?com\%[mand]!\?\s\+\(-\S\+\s\+\)*\u\k*',
            \   'fmt': {'sargs3': 'call TPluginCommand(%s, %s, %s)'}
            \ },
            \ 'f': {
            \   'rx':  '^\s*:\?fu\%[nction]!\?\s\+\zs\(s:\|<SID>\)\@![^[:space:].]\{-}\ze\s*(',
            \   'fmt': {'sargs3': 'call TPluginFunction(%s, %s, %s)'}
            \ },
            \ 'p': {
            \   'rx':  '\c^\s*:\?\zs[incvoslx]\?\(nore\)\?map\s\+\(<\(silent\|unique\|buffer\|script\)>\s*\)*<plug>\w\+',
            \   'fmt': {'sargs3': 'call TPluginMap(%s, %s, %s)'}
            \ },
            \ }


function! s:ScanSource(file, repo, plugin, what, lines) "{{{3
    let text = join(a:lines, "\n")
    let text = substitute(text, '\n\s*\\', '', 'g')
    let lines = split(text, '\n')
    let rx = join(filter(map(copy(a:what), 'get(get(s:scanner, v:val, {}), "rx", "")'), '!empty(v:val)'), '\|')
    let out = []
    let include = 0
    for line in lines
        if include
            if line !~ '\S'
                let include = 0
            else
                call add(out, line)
            endif
        elseif line =~ '^\s*"\s*#TPluginInclude\s*$'
            let include = 1
        elseif line =~ '^\s*"\s*#TPluginInclude\s*\S'
            let out_line = substitute(line, '^\s*"\s*#TPluginInclude\s*', '', '')
            call add(out, out_line)
        elseif line =~ rx
            let out_line = s:ScanLine(a:file, a:repo, a:plugin, a:what, line)
            if !empty(out_line)
                call add(out, out_line)
            endif
        endif
    endfor
    return out
endf


function! s:ScanLine(file, repo, plugin, what, line) "{{{3
    " TLogVAR a:file, a:repo, a:plugin, a:what, a:line
    if a:file =~ '[\/]'. a:repo .'[\/]autoload[\/]'
        let plugin = '-'
    else
        let plugin = a:plugin
    endif
    for what in a:what
        let scanner = get(s:scanner, what, {})
        if !empty(scanner)
            let m = s:Strip(matchstr(a:line, scanner.rx))
            if !empty(m)
                let m = substitute(m, '\s\+', ' ', 'g')
                " TLogVAR m
                if !has_key(s:scan_repo_done, what)
                    let s:scan_repo_done[what] = {}
                endif
                if has_key(s:scan_repo_done[what], m)
                    return ''
                else
                    let s:scan_repo_done[what][m] = 1
                    let fmt = scanner.fmt
                    if has_key(fmt, 'arr1')
                        return printf(fmt.arr1, string([m, a:repo, plugin]))
                    elseif has_key(fmt, 'sargs3')
                        return printf(fmt.sargs3, string(m), string(a:repo), string(plugin))
                    else
                        return printf(fmt.cargs3, escape(m, ' \	'), escape(a:repo, ' \	'), escape(plugin, ' \	'))
                    endif
                endif
            endif
        endif
    endfor
endf


" Write autoload information for all known root directories to 
" "ROOT/tplugin.vim".
function! s:ScanRoots(immediate, roots, args) "{{{3
    let awhat = get(a:args, 0, '')
    if empty(awhat)
        let awhat = g:tplugin_scan
    endif
    if awhat == 'all'
        let what = ['c', 'f', 'a', 'p', 'h', 't', 'l']
    else
        let what = split(awhat, '\zs')
    endif

    let aroot = get(a:args, 1, '')
    if empty(aroot)
        let roots = a:roots
    else
        let roots = [fnamemodify(aroot, ':p')]
    endif

    " TLogVAR what, a:roots

    for root in roots

        " TLogVAR root
        if root =~ '[\\/]\*$'
            let root = s:GetRootDirOnDisk(root)
            let is_tree = 0
        else
            let is_tree = 1
        endif
        " TLogVAR root, is_tree, isdirectory(root), len(files0)

        if !isdirectory(root)
            continue
        endif

        let files0 = s:GetFiles(root, is_tree)
        let pos0 = len(root) + 1
        " TLogVAR pos0
        " TLogDBG strpart(files0[0], pos0)
        let filelist = s:GetFilelist(files0, what, pos0, is_tree)

        let out = [
                    \ '" This file was generated by TPluginScan.',
                    \ 'if g:tplugin_autoload == 2 && g:loaded_tplugin != '. g:loaded_tplugin .' | throw "TPluginScan:Outdated" | endif'
                    \ ]

        if (g:tplugin_helptags || index(what, 'h') != -1) && is_tree
            let helpdirs = split(glob(s:FileJoin(root, '*', 'doc')), '\n')
            for doc in helpdirs
                if isdirectory(doc)
                    let tags = s:FileJoin(doc, 'tags')
                    if index(what, 'h') != -1 || !filereadable(tags)
                        exec 'helptags '. s:FnameEscape(doc)
                    endif
                endif
            endfor
        endif

        let progressbar = exists('g:loaded_tlib')
        if progressbar
            call tlib#progressbar#Init(len(filelist), 'TPluginscan: Scanning '. escape(root, '%') .' %s', 20)
        else
            echo 'TPluginscan: Scanning '. root .' ...'
        endif

        if is_tree && index(what, 't') != -1

            for ftdetect in filter(copy(files0), 'strpart(v:val, pos0) =~ ''^[^\/]\+[\/]ftdetect[\/][^\/]\{-}\.vim$''')
                call add(out, 'augroup filetypedetect')
                call extend(out, readfile(ftdetect))
                call add(out, 'augroup END')
            endfor

            let ftd = {}

            let ftypes= filter(copy(files0), 'strpart(v:val, pos0) =~ ''^[^\/]\+[\/]\(ftplugin\|ftdetect\|indent\|syntax\)[\/].\{-}\.vim$''')
            " TLogVAR ftypes
            for ftfile in ftypes
                let ft = matchstr(ftfile, '[\/]ftplugin[\/]\zs.\{-}\ze_.\{-}\.vim$')
                if empty(ft)
                    let ft = fnamemodify(ftfile, ':t:r')
                endif
                " TLogVAR ftfile, ft
                if !has_key(ftd, ft)
                    let ftd[ft] = {}
                endif
                let repo = matchstr(ftfile, '^.\{-}\%'. (len(root) + 2) .'c[^\/]\+')
                " TLogVAR ftfile, repo
                let ftd[ft][repo] = 1
            endfor

            " Add ftplugin subdirectories
            for ftplugin in filter(copy(files0), 'strpart(v:val, pos0) =~ ''^[^\/]\+[\/]ftplugin[\/][^\/]\+[\/].\{-}\.vim$''')
                let ftdir = fnamemodify(ftplugin, ':h')
                let ft    = fnamemodify(ftdir, ':t:r')
                if isdirectory(ftdir) && ft != 'ftplugin'
                    " TLogVAR ftplugin, ft, ftdir
                    " TLogDBG has_key(ftd, ft)
                    " TLogDBG isdirectory(ftplugin)
                    if !has_key(ftd, ft)
                        let ftd[ft] = {}
                    endif
                    let repo = matchstr(ftplugin, '^.\{-}\%'. (len(root) + 2) .'c[^\/]\+')
                    " TLogVAR repo
                    let ftd[ft][repo] = 1
                endif
            endfor

            for [ft, repos] in items(ftd)
                " TLogVAR ft, repos
                let repo_names = map(keys(repos), 'strpart(v:val, pos0)')
                call add(out, 'call TPluginFiletype('. string(ft) .', '. string(repo_names) .')')
            endfor

            if index(what, 'a') != -1
                let autoloads = filter(copy(files0), 'strpart(v:val, pos0) =~ ''^[^\/]\+[\/]autoload[\/].\{-}\.vim$''')
                call s:AddAutoloads(out, root, pos0, autoloads)
            endif

        endif
        
        let s:scan_repo_done = {}
        try
            let fidx = 0
            let menu_done = {}
            for file in filelist
                " TLogVAR file
                if progressbar
                    let fidx += 1
                    call tlib#progressbar#Display(fidx)
                endif
                let pluginfile = s:GetCanonicFilename(file)
                if is_tree
                    let repo = matchstr(strpart(file, pos0), '^[^\/]\+\ze[\/]')
                else
                    let repo = '-'
                endif
                let plugin = matchstr(file, '[\/]\zs[^\/]\{-}\ze\.vim$')
                " TLogVAR file, repo, plugin

                let is_plugin = !is_tree || strpart(file, pos0) =~ '^[^\/]\+[\/]plugin[\/][^\/]\{-}\.vim$'

                let lines = readfile(file)

                if is_plugin
                    if !empty(g:tplugin_menu_prefix)
                        if is_tree
                            let mrepo = escape(repo, '\.')
                        else
                            let mrepo = escape(fnamemodify(root, ':t'), '\.')
                        endif
                        let mplugin = escape(plugin, '\.')
                        if !has_key(menu_done, repo)
                            call add(out, 'call TPluginMenu('. string(mrepo .'.Add\ Repository') .', '.
                                        \ string(repo) .')')
                            call add(out, 'call TPluginMenu('. string(mrepo .'.-'. mrepo .'-') .', ":")')
                            let menu_done[repo] = 1
                        endif
                        call add(out, 'call TPluginMenu('. string(mrepo .'.'. mplugin) .', '.
                                    \ string(repo) .', '. string(plugin) .')')
                    endif

                    " if index(what, 'l') != -1
                    "     for line in lines
                    "         if line =~ '\c^let\s\+\(g:\)\?loaded_'. plugin .'\s*='
                    "             let loaded = matchstr(line, '\c^let\s\+\zs\(g:\)\?loaded_'. plugin)
                    "             let s:loaded_variables[pluginfile] = loaded
                    "             call add(out, line)
                    "             break
                    "         endif
                    "     endfor
                    " endif

                endif

                let out += s:ScanSource(file, repo, plugin, what, lines)
            endfor
        finally
            unlet s:scan_repo_done
            if progressbar
                call tlib#progressbar#Restore()
            else
                redraw
                echo
            endif
        endtry

        " TLogVAR out
        let outfile = s:FileJoin(root, s:tplugin_file .'.vim')
        call writefile(out, outfile)
        if a:immediate
            exec 'source '. s:FnameEscape(outfile)
        endif

    endfor
endf


function! s:GetFiles(root, is_tree) "{{{3
    if a:is_tree
        let files0 = split(glob(s:FileJoin(a:root, '**', '*.vim')), '\n')
    else
        let files0 = split(glob(s:FileJoin(a:root, '*.vim')), '\n')
    endif
    " TLogVAR files0

    call filter(files0, '!empty(v:val) && v:val !~ ''[\/]\(\.git\|.svn\|CVS\)\([\/]\|$\)''')
    let exclude_rx = '\V'. join(add(g:tplugin_autoload_exclude, '\[\\/]'. s:tplugin_file .'\(_\w\+\)\?\.vim\$'), '\|')
    " TLogVAR exclude_rx
    if exclude_rx != '\V'
        call filter(files0, 'v:val !~ exclude_rx')
    endif
    " TLogVAR files0
    " TLogDBG len(files0)
    return files0
endf


function! s:GetFilelist(files0, what, pos0, is_tree) "{{{3
    if !a:is_tree
        let filelist = copy(a:files0)
    else
        let filelist = filter(copy(a:files0), 'strpart(v:val, a:pos0) =~ ''^[^\/]\+[\/]plugin[\/][^\/]\{-}\.vim$''')
    endif
    " TLogDBG len(a:files0)
    " TLogDBG len(filelist)
    return filelist
endf


function! s:AddAutoloads(out, root, pos0, files) "{{{3
    " TLogVAR a:files
    for file0 in a:files
        let file = strpart(file0, a:pos0)
        let repo = matchstr(file, '^[^\/]\+')
        let def = [repo]
        let prefix = substitute(matchstr(file, '^[^\/]\+[\/]autoload[\/]\zs.\{-}\ze\.vim$'), '[\/]', '#', 'g')
        let pluginfile = substitute(file, '^[^\/]\+[\/]\zsautoload\ze[\/]', 'plugin', '')
        if index(a:files, pluginfile) != -1
            call add(def, matchstr(pluginfile, '^[^\/]\+[\/]plugin[\/]\zs.\{-}\ze\.vim$'))
        else
            call add(def, '.')
        endif
        " TLogVAR prefix, repo, file
        call add(a:out, printf('call TPluginAutoload(%s, %s)', string(prefix), string(def)))
    endfor
endf


function! s:GetRoot() "{{{3
    return s:roots[0]
endf


function! TPluginAutoload(prefix, def) "{{{3
    " echom "DBG ". a:prefix
    let s:autoloads[a:prefix] = [s:GetRoot()] + a:def
endf


" :nodoc:
function! TPluginMenu(item, ...) "{{{3
    if !empty(g:tplugin_menu_prefix)
        let def = [2, s:GetRoot()] + a:000
        call map(def, 'string(v:val)')
        exec 'amenu <silent> '. g:tplugin_menu_prefix . a:item .' :call TPluginRequire('. join(def, ', ') .')<cr>'
    endif
endf


function! s:GetCanonicFilename(filename) "{{{3
    let filename = substitute(a:filename, '[\\/]\+$', '', '')
    let filename = substitute(filename, '\\', '/', 'g')
    return filename
endf


" Remove any "/*" suffix.
function! s:GetRootDirOnDisk(dir) "{{{3
    let dir = s:GetCanonicFilename(a:dir)
    let dir = substitute(dir, '[\\/]\*$', '', '')
    let dir = substitute(dir, '[\\/]\+$', '', '')
    return dir
endf


function! s:SetRoot(dir) "{{{3
    " echom "DBG ". a:dir
    let root = s:GetCanonicFilename(fnamemodify(a:dir, ':p'))
    " echom "DBG ". root
    let idx = index(s:roots, root)
    if idx > 0
        call remove(s:roots, idx)
    endif
    if idx != 0
        call insert(s:roots, root)
    endif
    " echom "DBG ". string(s:roots)
    " Don't reload the file. Old autoload definitions won't be 
    " overwritten anyway.
    if idx == -1 && g:tplugin_autoload
        let rootdir = s:GetRootDirOnDisk(root)
        let autoload = s:FileJoin(rootdir, s:tplugin_file .'.vim')
        " echom "DBG ". autoload
        if filereadable(autoload)
            try
                exec 'source '. s:FnameEscape(autoload)
            catch /^TPluginScan:Outdated$/
                silent call s:ScanRoots(1, s:roots, [])
            catch
                echohl Error
                echom v:exception
                echom "Maybe the problem can be solved by running :TPluginScan"
                echohl NONE
            endtry
        endif
    endif
endf


function! s:AddRepo(rootrepos, isflat) "{{{3
    " TLogVAR a:rootrepos
    let rtp = split(&rtp, ',')
    let idx = index(rtp, s:rtp[0])
    if idx == -1
        let idx = 1
    else
        let idx += 1
    endif
    let rootrepos = filter(copy(a:rootrepos), '!has_key(s:done, v:val)')
    " TLogVAR rootrepos
    " call tlog#Debug(string(keys(s:done)))
    if !empty(rootrepos)
        for rootrepo in rootrepos
            let tplugin_repo = fnamemodify(rootrepo, ':h') .'/'. s:tplugin_file .'_'. fnamemodify(rootrepo, ':t') .'.vim'
            " TLogVAR rootrepo, tplugin_repo
            if filereadable(tplugin_repo)
                exec 'silent! source '. s:FnameEscape(tplugin_repo)
            endif
            if !a:isflat
                let repo_tplugin = fnamemodify(rootrepo, ':h') .'/'. fnamemodify(rootrepo, ':t') .'/'. s:tplugin_file .'.vim'
                if filereadable(repo_tplugin)
                    exec 'silent! source '. s:FnameEscape(repo_tplugin)
                endif
                " TLogVAR repo_tplugin
                call insert(rtp, rootrepo, idx)
                call insert(rtp, s:FileJoin(rootrepo, 'after'), -1)
            endif
            " TLogVAR rtp
            let s:done[rootrepo] = {}
        endfor
        let &rtp = join(rtp, ',')
    endif
endf


function! s:LoadPlugins(mode, rootrepo, pluginfiles) "{{{3
    if empty(a:pluginfiles)
        return
    endif
    " TLogVAR a:rootrepo, a:pluginfiles
    let done = s:done[a:rootrepo]
    " TLogVAR done
    if has_key(done, '*')
        return
    endif
    let pos0 = len(a:rootrepo) + 1
    for pluginfile in a:pluginfiles
        let pluginfile = s:GetCanonicFilename(pluginfile)
        " TLogVAR pluginfile
        if pluginfile != '-' && !has_key(done, pluginfile)
            let done[pluginfile] = 1
            if filereadable(pluginfile)
                call s:RemoveAutoloads(pluginfile, [])
                let before = filter(keys(s:before), 'pluginfile =~ v:val')
                if !empty(before)
                    call s:LoadDependency(a:rootrepo, before, s:before)
                endif
                " TLogDBG 'source '. pluginfile
                " TLogDBG filereadable(pluginfile)
                " call tlog#Debug(s:FnameEscape(pluginfile))
                exec 'source '. s:FnameEscape(pluginfile)
                " TLogDBG 'runtime! after/'. strpart(pluginfile, pos0)
                exec 'runtime! after/'. s:FnameEscape(strpart(pluginfile, pos0))
                let after = filter(keys(s:after), 'pluginfile =~ v:val')
                if !empty(after)
                    call s:LoadDependency(a:rootrepo, after, s:after)
                endif
                if a:mode == 2
                    echom "TPlugin: Loaded ". pathshorten(pluginfile)
                endif
            endif
        endif
    endfor
endf


function! s:LoadDependency(rootrepo, filename_rxs, dict) "{{{3
    " TLogVAR a:rootrepo, a:filename_rxs
    " call s:AddRepo([a:rootrepo])
    for filename_rx in a:filename_rxs
        let others = a:dict[filename_rx]
        " TLogVAR others
        for other in others
            if stridx(other, '*') != -1
                let pluginfiles = split(glob(a:rootrepo .'/'. other), '\n')
            else
                let pluginfiles = [a:rootrepo .'/'. other]
            endif
            call s:LoadPlugins(0, a:rootrepo, pluginfiles)
        endfor
    endfor
endf


function! s:LoadRequiredPlugins() "{{{3
    " TLogDBG "Plugin:LoadRequiredPlugins"
    call s:AddRepo(keys(s:reg), 0)
    if !empty(s:reg)
        " TLogVAR &rtp
        for [rootrepo, pluginfiles] in items(s:reg)
            call s:LoadPlugins(0, rootrepo, pluginfiles)
        endfor
    endif
    let s:immediate = 1
endf


" :nodoc:
function! TPluginRequire(mode, root, repo, ...) "{{{3
    " TLogVAR a:mode, a:root, a:repo, a:000
    let [rootrepo, plugindir] = s:GetPluginDir(a:root, a:repo)
    " TLogVAR rootrepo, plugindir
    if empty(a:000)
        " TLogDBG s:FileJoin(plugindir, '*.vim')
        let pluginfiles = split(glob(s:FileJoin(plugindir, '*.vim')), '\n')
    elseif a:1 == '.'
        let pluginfiles = []
    else
        let pluginfiles = map(copy(a:000), 's:FileJoin(plugindir, v:val .".vim")')
    endif
    call filter(pluginfiles, 'v:val !~ ''\V\[\/]'. s:tplugin_file .'\(_\S\{-}\)\?\.vim\$''')
    " TLogVAR pluginfiles
    if a:mode || s:immediate
        " TLogVAR rootrepo, pluginfiles
        call s:AddRepo([rootrepo], s:IsFlatRoot(a:root))
        call s:LoadPlugins(a:mode, rootrepo, pluginfiles)
    else
        if !has_key(s:reg, rootrepo)
            let s:reg[rootrepo] = []
        endif
        let s:reg[rootrepo] += pluginfiles
    end
endf


function! s:RemoveAutoloads(pluginfile, commands) "{{{3
    " TLogVAR a:pluginfile, a:commands

    " if has_key(s:loaded_variables, a:pluginfile)
    "     exec 'unlet '. s:loaded_variables[a:pluginfile]
    " endif

    " echom "DBG ". string(keys(s:maps))
    if has_key(s:maps, a:pluginfile)
        for [keys, map] in items(s:maps[a:pluginfile])
            call s:Unmap(map, keys)
        endfor
        call remove(s:maps, a:pluginfile)
    endif

    let pluginkey = s:CommandKey(a:pluginfile)
    " TLogVAR pluginkey
    " call tlog#Debug(string(keys(s:command_nobang)))
    if empty(a:commands)
        if has_key(s:command_nobang, pluginkey)
            let cmds = keys(s:command_nobang[pluginkey])
        else
            return
        endif
    else
        let cmds = a:commands
    endif

    " TLogVAR cmds
    " echom "DBG ". string(keys(s:command_nobang))
    let remove = !empty(a:commands) && has_key(s:command_nobang, pluginkey)
    for c in cmds
        if exists(':'. c) == 2
            exec 'delcommand '. c
        endif
        if remove && has_key(s:command_nobang[pluginkey], c)
            call remove(s:command_nobang[pluginkey], c)
        endif
    endfor
    if remove && empty(s:command_nobang[pluginkey])
        call remove(s:command_nobang, pluginkey)
    endif
endf


function! s:TPluginComplete(ArgLead, CmdLine, CursorPos) "{{{3
    " TLogVAR a:ArgLead, a:CmdLine, a:CursorPos
    let repo = matchstr(a:CmdLine, '\<TPlugin\s\+\zs\(\S\+\)\ze\s')
    " TLogVAR repo
    let rv = []
    " for root in s:roots
    let root = s:GetRoot()
    " TLogVAR root, repo
    if empty(repo)
        if root =~ '[\\/]\*$'
            let files = ['- ']
        else
            let pos0  = len(root) + 1
            let files = split(glob(s:FileJoin(root, '*')), '\n')
            call map(files, 'strpart(v:val, pos0)')
            " call tlog#Debug('v:val !~ ''\V'. s:tplugin_file .'\(_\w\+\)\?\.vim\$''')
            call filter(files, 'stridx(v:val, a:ArgLead) != -1')
        endif
        " TLogVAR files
    else
        " if root =~ '[\\/]\*$'
        "     let root = s:GetRootDirOnDisk(root)
        "     let subdir = ''
        " else
        "     let subdir  = s:FileJoin(repo, 'plugin')
        " endif
        " let plugindir   = s:FileJoin(root, subdir)
        let [rootrepo, plugindir] = s:GetPluginDir(root, repo)
        " TLogVAR subdir, plugindir
        let pos0  = len(plugindir) + 1
        let files = split(glob(s:FileJoin(plugindir, '*.vim')), '\n')
        call map(files, 'strpart(v:val, pos0, len(v:val) - pos0 - 4)')
        call filter(files, 'stridx(v:val, a:ArgLead) != -1')
        " TLogVAR files
    endif
    call filter(files, 'v:val !~ ''\V'. s:tplugin_file .'\(_\w\+\)\?\(\.vim\)\?\$''')
    let rv += files
    " endfor
    " TLogVAR rv
    return rv
endf


function! s:IsFlatRoot(root) "{{{3
    return a:root =~ '[\\/]\*$'
endf


function! s:GetPluginDir(root, repo) "{{{3
    " echom "DBG ". string([a:root, a:repo])
    let root = s:GetRootDirOnDisk(empty(a:root) ? s:GetRoot() : a:root)
    let repo = s:IsFlatRoot(a:root) ? '-' : a:repo
    " deprecated
    if repo == '.'
        let rootrepo = root
    else
        if repo == '-'
            let rootrepo = root
        else
            let rootrepo = s:FileJoin(root, repo)
        endif
    endif
    if repo == '-'
        let plugindir = rootrepo
    else
        let plugindir = s:FileJoin(rootrepo, 'plugin')
    endif
    return [rootrepo, plugindir]
endf


function! s:GetPluginFile(root, repo, plugin) "{{{3
    let [rootrepo, plugindir] = s:GetPluginDir(a:root, a:repo)
    return printf('%s/%s.vim', plugindir, a:plugin)
endf


" :display: TPluginFunction(FUNCTION, REPOSITORY, [PLUGIN])
" Load a certain plugin on demand (aka autoload) when FUNCTION is called 
" for the first time.
function! TPluginFunction(...) "{{{3
    let fn = a:000[0]
    if g:tplugin_autoload && !exists('*'. fn)
        " echom "DBG fn = ". fn
        let s:functions[fn] = [s:GetRoot()] + a:000
    endif
endf


" :display: TPluginCommand(COMMAND, REPOSITORY, [PLUGIN])
" Load a certain plugin on demand (aka autoload) when COMMAND is called 
" for the first time. Then call the original command.
"
" For most plugins, |:TPluginScan| will generate the appropriate 
" TPluginCommand commands for you. For some plugins, you'll have to 
" define autocommands yourself in the |vimrc| file.
"
" Example: >
"   TPluginCommand TSelectBuffer vimtlib tselectbuffer
function! TPluginCommand(...) "{{{3
    let cmd = a:000[0]
    if g:tplugin_autoload && exists(':'. matchstr(cmd, '\s\zs\u\w*$')) != 2
        let args = [s:GetRoot()] + a:000
        " echom "DBG cmd =" cmd
        exec s:DefineCommand(a:000) .' call s:Autoload(1, '. string(args) .', "<bang>", ["<line1>", "<line2>"], <q-args>)'
    endif
endf


call s:SetRoot(s:FileJoin(s:rtp[0], 'repos'))


augroup TPlugin
    autocmd!
    autocmd VimEnter * call s:LoadRequiredPlugins()

    if g:tplugin_autoload
        autocmd FuncUndefined * call s:AutoloadFunction(expand("<afile>"))
        autocmd FileType * if has_key(s:ftypes, &ft) | call s:LoadFiletype(&ft) | endif
    endif

    " if g:tplugin_help
    "     autocmd CmdwinEnter * if expand('<afile>') == ':' | call s:FullRuntimepath() | endif
    "     autocmd CmdwinLeave * if expand('<afile>') == ':' | call s:ResetRuntimepath() | endif
    " endif

augroup END


let &cpo = s:save_cpo
unlet s:save_cpo
finish

0.1
- Initial release

0.2
- Improved command-line completion for :TPlugin
- Experimental autoload for commands and functions (� la AsNeeded)
- The after path is inserted at the second to last position
- When autoload is enabled and g:tplugin_menu_prefix is not empty, build 
a menu with available plugins (NOTE: this is disabled by default)

0.3
- Build helptags during :TPluginScan (i.e. support for helptags requires 
autoload to be enabled)
- Call delcommand before autoloading a plugin because of an unknown 
command
- TPluginScan: Take a root directory as the second optional argument
- The autoload file was renamed to ROOT/tplugin.vim
- When adding a repository to &rtp, ROOT/tplugin_REPO.vim is loaded
- TPluginBefore, TPluginAfter commands to define inter-repo dependencies
- Support for autoloading <plug> maps
- Support for autoloading filetypes

0.4
- Moved autoload functions to macros/tplugin.vim -- users have to rescan 
their repos.
- Fixed concatenation of filetype-related files
- :TPluginDisable command
- Replaced :TPluginMap with a function TPluginMap()

0.5
- Support for ftdetect
- Per repo metadata (ROOT/REPO/tplugin.vim)
- FIX: s:ScanRoots(): Remove empty entries from filelist
- Support for ftplugins in directories and named {&FT}_{NAME}.vim
- FIX: Filetype-related problems
- Relaxed the rx for functions
- FIX: Don't load any plugins when autoloading an "autoload function"
- :TPlugin accepts "-" as argument, which means load "NO PLUGIN".
- Speed up :TPluginScan (s:ScanRoots(): run glob() only once, filter file 
contents before passing it to s:ScanSource())
- :TPluginScan: don't use full filenames as arguments for 
TPluginFiletype()
- g:tplugin_autoload_exclude: Exclude repos from autoloading
- Removed :TPluginDisable
- TPluginMap(): Don't map keys if the key already is mapped (via 
maparg())
- If g:tplugin_autoload == 2, run |:TPluginScan| after updating tplugin.
- FIX: Don't add autoload files to the menu.
- FIX: s:ScanLine: Don't create duplicate autoload commands.

0.6
- CHANGE: The root specific autoload files are now called '_tplugin.vim'
- Provide a poor-man implementation of fnameescape() for users of older 
versions of vim.
- If the root name ends with '*', the root is no directory tree but a 
single directory (actually a plugin repo)
- s:TPluginComplete(): Hide tplugin autoload files.

0.7
- TPluginScan: try to maintain information about command-line completion 
(this won't work if a custom script-local completion function is used)

0.8
- Delete commands only when they were defined without a bang; make sure 
all commands in a file defined without a bang are deleted
- g:tplugin_scan defaults to 'cfpt'
- Don't register each autoload function but deduce the repo/plugin from 
the prefix.
- g:tplugin_scan defaults to 'cfpta'
- TPluginCommand and TPluginFunction are functions. Removed the commands 
with the same name.
- #TPluginInclude tag

