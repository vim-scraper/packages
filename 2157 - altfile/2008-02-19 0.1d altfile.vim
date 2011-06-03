" AltFile 0.1d by Alex Kunin <alexkunin@gmail.com>
"
"
" PURPOSE
" ===================================================================
" The plugin allows to switch easily between file.h/file.c,
" main source/testcase, etc.
"
"
" HISTORY
" ===================================================================
"
" 2008-02-19    0.1d    Now wildmenu is used as "engine",
"                       so look & feel are much better now.
"                       No more numeric shortcats, however.
"                       Lots of code cleanup.
"
" 2008-02-18    0.1c    Default choice now mimics Alt-Tab (Cmd-Tab
"                       for Mac users), i.e. hitting the hot key
"                       and then <CR> will cycle between last
"                       two files. Visual adjustments: current
"                       file is square brakets, and asterisk
"                       indicates default choice.
"
" 2008-02-18    0.1b    If selected file is already visible
"                       in some window, the script will
"                       activate it.
"
" 2008-02-17    0.1a    When GUI is available, dialog forced
"                       to be console-friendly, i.e. no GUI
"                       window is poping up.
"
" 2008-02-16    0.1     Initial release.
"
"
" INSTALLATION
" ===================================================================
" Copy this file to your ~/.vim/plugin/ folder. Bind some key to
" AltFile_ShowMenu():
" 
"   nmap <expr> <M-Tab> AltFile_ShowMenu()
"
" Make sure that wildmenu is enabled:
"
"   set wildmenu
"
"
" USAGE
" ===================================================================
"
" Suppose we have a project which resides in a "proj"
" directory. Content of that directory might look like this:
"
"   proj/classes/Class1.php
"   proj/classes/Class2.php
"   proj/classes/Namespace/Class3.php
"   proj/tpl/Class1.html
"   proj/tpl/Class2.html
"   proj/tpl/Namespace/Class3.html
"   proj/tests/Class1.phpt
"   proj/tests/Class2.phpt
"   proj/tests/Namespace/Class3.phpt
"
" Create "proj/.altfile" and put these lines inside:
"
"   class: classes/{MATCH}.php
"   template: tpl/{MATCH}.html
"   test: tests/{MATCH}.phpt
"
" Now load "proj/classes/Class1.php" and hit the hot key. Menu will
" appear:
"
"   class  [template]  test
"
" Now you can use it as any other wild menu: cursor keys, <CR>, <Esc>.
"
" By default previously active item is highilighted (works like Alt-Tab
" for Windows or Cmd-Tab for Mac OS X). So, to quickly switch between
" two files all you have to do is <M-Tab><CR>.
"
" Now load "proj/tpl/Class4.html" - it does not exist, and you'll get
" empty window. Hit <M-Tab>, select "class" and hit <CR> - VIM will
" create new buffer for (still non-existing) "proj/classes/Class4.php".
" Ta-dah! Actually, this is my main reason for creating this plugin.
"
" Note that it DOES NOT matter what is your current directory: you
" can cd to whatever place you want. But it DOES matter where ".altfile"
" is, becase patterns inside it are relative to its placement.
"
"
" API
" ===================================================================
"
" Functions:
"
"   AltFile_ShowMenu()      show the menu and highlight default
"                           item (algorithm is similar to Alt-Tab
"                           on Windows or Cmd-Tab on Mac OS X)
"
" Variables:
"
"   g:AltFile_CfgFile       name of the configuration file;
"                           default is ".altfile"
"
" Commands:
"
"   :AltFile {label}        switch to another file; autocompletion
"                           is available
"
"
" FEEDBACK
" ===================================================================
" Probably, there are bugs - I'm not a VIM guru. Please, send
" bug reports to the e-mail mentioned above. Ideas and suggestions
" are welcome too.
"
"
" LICENSE AND DISCLAIMER
" ===================================================================
" Free for whatever use. No warranties at all.

if !exists('g:AltFile_CfgFile')
    let g:AltFile_CfgFile = '.altfile'
endif

let s:previous = {}

function! s:GetAltFiles(absfilename)
    " Climbing up to find configuration file:
    let cfgfile =
        \ findfile(g:AltFile_CfgFile, fnamemodify(a:absfilename, ':h') . ';')

    if !filereadable(cfgfile)
        throw "Configuration file is not available."
    endif

    " Checking if there is up-to-date cached result:
    if exists('b:AltFile_Config') && b:AltFile_Config.timestamp >= getftime(cfgfile)
        return b:AltFile_Config
    endif

    " Resulting structure:
    "   timestamp - modification time of the config file
    "   basedir - absolute path of the project (config file is right here)
    "   options - list of alternative files (see optionPrototype below)
    "   selectedIndex - currently active file
    "   match - {MATCH}-part of the filepath
    let result =
        \ {
        \ 'timestamp':getftime(cfgfile),
        \ 'basedir':fnamemodify(cfgfile, ':p:h'),
        \ 'options':[],
        \ 'selectedIndex':-1,
        \ 'match':''
        \ }

    " Prototype for items in result.options
    "   label - readable label
    "   pattern - pattern as defined in the config file
    "   filename - pattern with {MATCH} replaced with result.match
    let optionPrototype =
        \ {
        \ 'label':'',
        \ 'pattern':'',
        \ 'filename':''
        \ }

    " Some weird steps to find out relative path (relative to
    " result.basedir):
    let hadlocaldir = haslocaldir()
    let cwd = getcwd()
    execute 'lcd ' . result.basedir
    let relfilename = fnamemodify(a:absfilename, ":p:.")
    execute (hadlocaldir ? 'lcd' : 'cd') . ' ' . cwd

    " Iterating through the config file:
    for line in readfile(cfgfile)
        " Preparing new option "object":
        let option = copy(optionPrototype)

        " Determining which of two forms is in use: "label: pattern" or
        " just "pattern":
        let matches = matchlist(line, '^\(\S\+\):\s\+\(.\+\)$')

        if len(matches)
            let option.label = matches[1]
            let option.pattern = matches[2]
        else
            let option.label = line
            let option.pattern = line
        endif

        " Trying to match this pattern (if previous patterns failed):
        if result.selectedIndex == -1
            " Constructing and applying regular expression ("{MATCH}" is
            " converted to "(.+)", " rest of the pattern matches literally):
            let matches = matchlist(
                \ relfilename,
                \ '^'
                \ . substitute(
                    \ escape(option.pattern, '/.'),
                    \ '{MATCH}',
                    \ '\\(.\\+\\)',
                    \''
                \ )
                \ . '$')

            " If matched,
            if len(matches)
                " storing option index
                let result.selectedIndex = len(result.options)
                " and actual value of {MATCH}
                let result.match = matches[1]
            endif
        endif

        " Adding the option to the result
        call add(result.options, option)
    endfor

    " If no pattern matched, then file has no known alternatives,
    " and it's not our problem anymore.
    if result.selectedIndex == -1
        throw "No alternatives available."
    endif

    " Calculating actual alternative filenames by replacing {MATCH} with
    " its actual value:
    for i in range(len(result.options))
        let result.options[i].filename =
            \ substitute(result.options[i].pattern, '{MATCH}', result.match, '')
    endfor

    " Caching...
    let b:AltFile_Config = result
    " ...and returning the result.
    return result
endfunction

function! s:GetAltFilesForCurBuf()
    return s:GetAltFiles(simplify(getcwd() . '/' . expand('%:p:.')))
endfunction

function! s:CompletionCallback(ArgLead, CmdLine, CursorPos)
    return map(copy(s:GetAltFilesForCurBuf().options), 'v:val.label')
endfunction

function! s:Switch(label)
    try
        let alts = s:GetAltFilesForCurBuf()
    catch
        echohl WarningMsg
        echo v:exception
        return
    endtry

    for o in alts.options
        if o.label == a:label
            let key = alts.basedir . ' ' . alts.match
            let s:previous[key] = alts.selectedIndex

            let filename = simplify(alts.basedir . '/' . o.filename)
            let bufno = bufnr(filename)
            if bufno != -1
                let winno = bufwinnr(filename)
                if winno != -1
                    execute winno . ' wincmd w'
                else
                    execute 'buffer ' . bufno
                endif
            else
                execute 'edit ' . filename
            endif
            return
        endif
    endfor
endfunction

command!
    \ -nargs=?
    \ -complete=customlist,s:CompletionCallback
    \ AltFile
    \ call s:Switch('<args>')

function! AltFile_ShowMenu()
    try
        let alts = s:GetAltFilesForCurBuf()
        let key = alts.basedir . ' ' . alts.match
        let index = has_key(s:previous, key) && s:previous[key] != alts.selectedIndex ? s:previous[key] : alts.selectedIndex + 1
        return ':AltFile ' . join(map(range(0, index % len(alts.options)), 'nr2char(&wildcharm)'), '')
    catch
        echohl WarningMsg
        echo v:exception
        return ''
    endtry
endfunction

" Defining Macro-aware wildchar (actual value does not matter):
if !&wildcharm
    set wildcharm=<C-Z>
endif
