" AltFile 0.1c by Alex Kunin <alexkunin@gmail.com>
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
" Copy this file to your ~/.vim/plugin/folder. Bind some key to
" AltFile(). Mine favorite so far is <Tab>:
" 
"   nmap <silent> <Tab> :call AltFile()<CR>
"
" Argument <silent> suppresses unneeded echo of the ":call".
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
" Now load "proj/classes/Class1.php" and hit the magic key (<Tab> in my
" case; mode should be "normal"). In the status line you'll see
" something like this:
"
"   1:[class]  *2: template   3: test
"
" Hit "2" to load "proj/tpl/Class1.html" or hit "3" to load
" "proj/tests/Class1.phpt". To cancel switching hit "Ctrl-C".
" Hitting digit that corresponds to currently loaded file won't do
" anything. Hitting "Enter" will switch to previously active
" file (it's like Alt-Tab for Windows or Cmd-Tab for Mac OS X).
"
" Now load "proj/tpl/Class4.html" - it does not exist, and you'll get
" empty window. Hit magic key and then select "1" - VIM will create
" new buffer for (still non-existing) "proj/classes/Class4.php".
" Ta-dah! Actually, this is the main reason for creating this plugin.
"
" Note that it DOES NOT matter what is your current directory: you
" can cd to whatever place you want. But it DOES matter where ".altfile"
" is, becase patterns inside it are relative to its placement.
"
" FEEDBACK
" ===================================================================
" Probably, there are bugs - I'm not a VIM guru. Please, send
" bug reports to the e-mail mentioned above. Ideas and suggestions
" are welcome too.
"
" LICENSE AND DISCLAIMER
" ===================================================================
" Free for whatever use. No warranties at all.

let g:AltFile_CfgFile = '.altfile'
let g:AltFile_MaxDepth = 16
let g:AltFile_Previous = {}

function AltFile()
    let cfgfile = ''
    let patterns = []
    let labels = []
    let regexps = []
    let filenames = []

    let filename = expand("%:p")

    let path = fnamemodify(filename, ':h')
    let i = 0

    while i < g:AltFile_MaxDepth
        if filereadable(path . '/' . g:AltFile_CfgFile)
            let cwd = getcwd()
            execute 'lcd ' . path
            let filename = expand("%:p:.")
            execute 'lcd ' . cwd
            let cfgfile = path . '/' . g:AltFile_CfgFile
            break
        endif

        let path = simplify(path . '/..')
        let i = i + 1
    endwhile

    if !filereadable(cfgfile)
        echohl WarningMsg
        echo 'Configuration file ' . g:AltFile_CfgFile . ' is not available.'
        return
    endif

    let match = ''
    let index = -1

    let lines = readfile(cfgfile)
    let i = 0

    while i < len(lines)
        let matches = matchlist(lines[i], '^\(\S\+\):\s\+\(.\+\)$')

        if len(matches)
            let label = matches[1]
            let pattern = matches[2]
        else
            let label = lines[i]
            let pattern = lines[i]
        endif

        let regexp = '^' . substitute(substitute(pattern, '\(\/\|\.\)', '\\\1', 'g'), '{MATCH}', '\\(.\\+\\)', '') . '$'

        if index == -1
            let matches = matchlist(filename, regexp)
            if len(matches)
                let index = i
                let match = matches[1]
            endif
        endif

        call add(patterns, pattern)
        call add(labels, label)
        call add(regexps, regexp)

        let i = i + 1
    endwhile

    if index == -1
        echohl WarningMsg
        echo 'No alternatives available.'
        return
    endif

    let current = index
    let key = cfgfile . ' ' . match

    if has_key(g:AltFile_Previous, key) && g:AltFile_Previous[key] != current
        let default = g:AltFile_Previous[key]
    else
        let default = current + 1
    endif

    let default = default % len(filenames)

    let choices = ''
    let prompt = ''
    let i = 0

    while i < len(patterns)
        call add(filenames, substitute(patterns[i], '{MATCH}', match, ''))

        if patterns[i] == labels[i]
            let labels[i] = filenames[i]
        endif

        if i
            let choices = choices . "\n"
            let prompt = prompt . '  '
        endif

        let choices = choices . '&' . (i + 1) . labels[i]

        if i == default
            let prompt = prompt . '*'
        else
            let prompt = prompt . ' '
        endif

        if i == current
            let prompt = prompt . (i + 1) . ':[' . labels[i] . ']'
        else
            let prompt = prompt . (i + 1) . ': ' . labels[i] . ' '
        endif

        let i = i + 1
    endwhile

    try
        if has("gui_running")
            let guioptions = getbufvar('', '&guioptions')
            setlocal guioptions+=c
        endif
        let statusline = getbufvar('', '&statusline')
        call setbufvar('', '&statusline', prompt)
        redraw
        silent let choice = confirm("Select file to load:", choices, default + 1)
    finally
        if has("gui_running")
            call setbufvar('', '&guioptions', guioptions)
        endif
        call setbufvar('', '&statusline', statusline)
    endtry

    if choice && choice - 1 != current
        let g:AltFile_Previous[key] = current
        let filename = path . '/' . filenames[choice - 1]
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
    endif
endfunction
