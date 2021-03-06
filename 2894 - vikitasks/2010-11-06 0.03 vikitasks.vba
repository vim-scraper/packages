" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
autoload/vikitasks.vim	[[[1
615
" vikitasks.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-12-13.
" @Last Change: 2010-11-06.
" @Revision:    0.0.664


" A list of glob patterns (or files) that will be searched for task 
" lists.
" Can be buffer-local.
" If you add ! to 'viminfo', this variable will be automatically saved 
" between editing sessions.
" Alternatively, add new items in ~/vimfiles/after/plugin/vikitasks.vim
TLet g:vikitasks#files = []

" A list of |regexp| patterns for filenames that should not be 
" scanned.
TLet g:vikitasks#files_ignored = []
let s:files_ignored = join(g:vikitasks#files_ignored, '\|')

" If non-null, automatically add the homepages of your intervikis to 
" |g:vikitasks#files|.
" If the value is 2, scan all files (taking into account the interviki 
" suffix) in the interviki's top directory.
" Can be buffer-local.
TLet g:vikitasks#intervikis = 0

" A list of ignored intervikis.
" Can be buffer-local.
TLet g:vikitasks#intervikis_ignored = []

" The viewer for the quickfix list. If empty, use |:TRagcw|.
TLet g:vikitasks#qfl_viewer = ''

" Item classes that should be included in the list when calling 
" |:VikiTasks|.
" A user-defined value must be set in |vimrc| before the plugin is 
" loaded.
TLet g:vikitasks#rx_letters = 'A-W'

" Item levels that should be included in the list when calling 
" |:VikiTasks|.
" A user-defined value must be set in |vimrc| before the plugin is 
" loaded.
TLet g:vikitasks#rx_levels = '1-5'

" Cache file name.
" By default, use |tlib#cache#Filename()| to determine the file name.
TLet g:vikitasks#cache = tlib#cache#Filename('vikitasks', 'files', 1)
call add(g:tlib#cache#dont_purge, '[\/]vikitasks[\/]files$')

" Definition of the tasks that should be included in the Alarms list.
" Fields:
"   all_tasks  ... If non-null, also display tasks with no due-date
"   tasks      ... Either 'tasks' or 'sometasks'
"   constraint ... See |:VikiTasks|
TLet g:vikitasks#alarms = {'all_tasks': 0, 'tasks': 'sometasks', 'constraint': 14}

" If true, the end-date of date ranges (FROM..TO) is significant.
TLet g:vikitasks#use_end_date = 1

" Interpret entries with an unspecified date ("_") as current tasks.
TLet g:vikitasks#use_unspecified_dates = 0

" If true, remove unreadable files from the tasks list.
TLet g:vikitasks#remove_unreadable_files = 1


function! s:VikitasksRx(inline, sometasks, letters, levels) "{{{3
    let val = '\C^[[:blank:]]'. (a:inline ? '*' : '\+') .'\zs'.
                \ '#\(T: \+.\{-}'. a:letters .'.\{-}:\|'. 
                \ '['. a:levels .']\?['. a:letters .']['. a:levels .']\?'.
                \ '\( \+\(_\|[0-9%-]\+\)\)\?\)\(\s%s\|$\)'
    return val
endf

let s:sometasks_rx = s:VikitasksRx(1, 1, g:vikitasks#rx_letters, g:vikitasks#rx_levels)
let s:tasks_rx = s:VikitasksRx(0, 0, 'A-Z', '0-9')
exec 'TRagDefKind tasks viki /'. s:tasks_rx .'/'

delf s:VikitasksRx


let s:date_rx = '\C^\s*#[A-Z0-9]\+\s\+\(_\|\d\+-\d\+-\d\+\)\(\.\.\(\(_\|\d\+-\d\+-\d\+\)\)\)\?\s'


" :nodoc:
function! vikitasks#GetArgs(bang, list) "{{{3
    let args = {}
    let args.cached = !a:bang
    let a0 = get(a:list, 0, '.')
    if a0 =~ '^[@:]'
        let args.all_tasks = 1
        let args.tasks = 'tasks'
        let args.constraint = '.'
    else
        let args.all_tasks = a0 =~ '^[.*]$'
        let args.tasks = a0 == '*' ? 'tasks' : 'sometasks'
        let args.constraint = a0
        if !empty(a:list)
            call remove(a:list, 0)
        endif
    endif
    let args.rx = s:MakePattern(get(a:list, 0, '.'))
    let args.files = a:list[2:-1]
    return args
endf


" :display: vikitasks#Tasks(?{'all_tasks': 0, 'cached': 1, 'files': [], 'constraint': '', 'rx': ''})
" If files is non-empty, use these files (glob patterns actually) 
" instead of those defined in |g:vikitasks#files|.
function! vikitasks#Tasks(...) "{{{3
    TVarArg ['args', {}]

    if get(args, 'cached', 1)

        let qfl = copy(s:Tasks())
        call s:TasksList(qfl, args)

    else

        if &filetype != 'viki' && !viki#HomePage()
            echoerr "VikiTasks: Not a viki buffer and cannot open the homepage"
            return
        endif

        " TLogVAR args
        let files = get(args, 'files', [])
        " TLogVAR files
        if empty(files)
            let files = s:MyFiles()
            " TLogVAR files
        endif
        " TAssertType files, 'list'

        " TLogVAR files
        call map(files, 'glob(v:val)')
        let files = split(join(files, "\n"), '\n')
        " TLogVAR files
        if !empty(files)
            let qfl = trag#Grep('tasks', 1, files)
            " TLogVAR qfl
            " TLogVAR filter(copy(qfl), 'v:val.text =~ "#D7"')

            " TLogVAR qfl
            let tasks = copy(qfl)
            for i in range(len(tasks))
                let bufnr = tasks[i].bufnr
                let tasks[i].filename = s:CanonicFilename(fnamemodify(bufname(bufnr), ':p'))
                call remove(tasks[i], 'bufnr')
            endfor
            call s:SaveInfo(s:Files(), tasks)

            call s:TasksList(qfl, args)
        else
            echom "VikiTasks: No task files"
        endif

    endif
endf


function! s:TasksList(qfl, args) "{{{3
    call s:FilterTasks(a:qfl, a:args)
    call sort(a:qfl, "s:SortTasks")
    call setqflist(a:qfl)
    let i = s:GetCurrentTask(a:qfl, 0)
    call s:View(i, 0)
endf


function! s:FileReadable(filename, cache) "{{{3
    let readable = get(a:cache, a:filename, -1)
    if readable >= 0
        return readable
    else
        let a:cache[a:filename] = filereadable(a:filename)
        return a:cache[a:filename]
    endif
endf


function! s:FilterTasks(tasks, args) "{{{3
    " TLogVAR a:args

    let rx = get(a:args, 'rx', '')
    if !empty(rx)
        call filter(a:tasks, 'v:val.text =~ rx')
    endif

    if g:vikitasks#remove_unreadable_files
        let filenames = {}
        call filter(a:tasks, 's:FileReadable(v:val.filename, filenames)')
    endif

    let which_tasks = get(a:args, 'tasks', 'tasks')
    " TLogVAR which_tasks
    if which_tasks == 'sometasks'
        let rx = s:TasksRx('sometasks')
        " TLogVAR rx
        " TLogVAR len(a:tasks)
        call filter(a:tasks, 'v:val.text =~ rx')
        " TLogVAR len(a:tasks)
    endif

    if !get(a:args, 'all_tasks', 0)
        call filter(a:tasks, '!empty(s:GetTaskDueDate(v:val.text, 0, g:vikitasks#use_unspecified_dates))')
        " TLogVAR len(a:tasks)
        let constraint = get(a:args, 'constraint', '.')
        " TLogVAR constraint
        if match(constraint, '^\(+\?\)\(\d\+\)') == -1
            let future = ''
            let n = 1
        else
            let [m0, future, n; _] = matchlist(constraint, '^\(+\?\)\(\d\+\)')
        endif
        let from = empty(future) ? 0 : localtime()
        let to = 0
        if constraint =~ '^t\%[oday]'
            let from = localtime()
            let to = from
        elseif constraint =~ '^c\%[urrent]'
            let from = 0
            let to = localtime()
        elseif constraint =~ '^m\%[onth]'
            let to = localtime() + 86400 * 31
        elseif constraint == 'week'
            let to = localtime() + 86400 * 7
        elseif constraint =~ '^+\?\d\+m$'
            let to = localtime() + n * 86400 * 31
        elseif constraint =~ '^+\?\d\+w$'
            let to = localtime() + n * 86400 * 7
        elseif constraint =~ '^+\?\d\+d\?$'
            let to = localtime() + n * 86400
        else
            echoerr "vikitasks: Malformed constraint: ". constraint
        endif
        " TLogVAR from, to
        if from != 0 || to != 0
            call filter(a:tasks, 's:Select(v:val.text, from, to)')
        endif
    endif
endf


function! s:View(index, suspend) "{{{3
    if empty(g:vikitasks#qfl_viewer)
        let w = {}
        if a:index > 1
            let w.initial_index = a:index
        endif
        let w.trag_list_syntax = 'viki'
        let w.trag_list_syntax_nextgroup = '@vikiPriorityListTodo'
        let w.trag_short_filename = 1
        let w.scratch = '__VikiTasks__'
        call trag#QuickList(w, a:suspend)
    else
        exec g:vikitasks#qfl_viewer
    endif
endf


" The |regexp| PATTERN is prepended with |\<| if it seems to be a word. 
" The PATTERN is made case sensitive if it contains an upper-case letter 
" and if 'smartcase' is true.
function! s:MakePattern(pattern) "{{{3
    let pattern = a:pattern
    if empty(pattern)
        let pattern = '.'
    elseif pattern != '.'
        if pattern =~ '^\w'
            let pattern = '\<'. pattern
        endif
        if &smartcase && pattern =~ '\u'
            let pattern = '\C'. pattern
        endif
    endif
    return pattern
endf


function! s:GetTaskDueDate(task, use_end_date, use_unspecified) "{{{3
    let m = matchlist(a:task, s:date_rx)
    if a:use_end_date && g:vikitasks#use_end_date
        let rv = get(m, 3, '')
    else
        let rv = ''
    endif
    if empty(rv)
        let rv = get(m, 1, '')
    endif
    if rv == '_' && !a:use_unspecified
        let rv = ''
    endif
    " TLogVAR a:task, m, rv
    return rv
endf


function! s:GetCurrentTask(qfl, daysdiff) "{{{3
    " TLogVAR a:daysdiff
    let i = 1
    let today = strftime('%Y-%m-%d')
    for qi in a:qfl
        let qid = s:GetTaskDueDate(qi.text, 1, g:vikitasks#use_unspecified_dates)
        " TLogVAR qid, today
        if !empty(qid) && tlib#date#DiffInDays(qid, today, 1) <= a:daysdiff
            let i += 1
        else
            break
        endif
    endfor
    return i
endf


function! s:SortTasks(a, b) "{{{3
    let a = a:a.text
    let b = a:b.text
    let ad = s:GetTaskDueDate(a, 1, g:vikitasks#use_unspecified_dates)
    let bd = s:GetTaskDueDate(b, 1, g:vikitasks#use_unspecified_dates)
    if ad && !bd
        return -1
    elseif !ad && bd
        return 1
    elseif ad && bd && ad != bd
        return ad > bd ? 1 : -1
    else
        return a == b ? 0 : a > b ? 1 : -1
    endif
endf


function! s:Files() "{{{3
    if !exists('s:files')
        let s:files = get(tlib#cache#Get(g:vikitasks#cache), 'files', [])
        if !has('fname_case') || !&shellslash
            call map(s:files, 's:CanonicFilename(v:val)')
        endif
        " echom "DBG nfiles = ". len(s:files)
    endif
    return s:files
endf


function! s:Tasks() "{{{3
    if !exists('s:tasks')
        let s:tasks = get(tlib#cache#Get(g:vikitasks#cache), 'tasks', [])
        " echom "DBG ntasks = ". len(s:tasks)
    endif
    return s:tasks
endf


function! s:SaveInfo(files, tasks) "{{{3
    " TLogVAR len(a:files), len(a:tasks)
    let s:files = a:files
    let s:tasks = a:tasks
    call tlib#cache#Save(g:vikitasks#cache, {'files': a:files, 'tasks': a:tasks})
endf


function! s:CanonicFilename(filename) "{{{3
    let filename = a:filename
    if !has('fname_case')
        let filename = tolower(filename)
    endif
    if !&shellslash
        let filename = substitute(filename, '\\', '/', 'g')
    endif
    return filename
endf


function! s:MyFiles() "{{{3
    let files = copy(tlib#var#Get('vikitasks#files', 'bg', []))
    " TLogVAR files
    let files += s:Files()
    " TLogVAR files
    if tlib#var#Get('vikitasks#intervikis', 'bg', 0) > 0
        call s:AddInterVikis(files)
    endif
    " TLogVAR files
    if !empty(s:files_ignored)
        call filter(files, 'v:val !~ s:files_ignored')
    endif
    " TLogVAR files
    if !has('fname_case') || !&shellslash
        call map(files, 's:CanonicFilename(v:val)')
    endif
    let files = tlib#list#Uniq(files)
    " TLogVAR files
    return files
endf


function! s:AddInterVikis(files) "{{{3
    " TLogVAR a:files
    let ivignored = tlib#var#Get('vikitasks#intervikis_ignored', 'bg', [])
    let glob = tlib#var#Get('vikitasks#intervikis', 'bg', 0) == 2
    for iv in viki#GetInterVikis()
        if index(ivignored, matchstr(iv, '^\u\+')) == -1
            " TLogVAR iv
            let def = viki#GetLink(1, '[['. iv .']]', 0, '')
            " TLogVAR def
            if glob
                let suffix = viki#InterVikiSuffix(iv)
                let files = split(glob(tlib#file#Join([def[1]], '*'. suffix)), '\n')
            else
                let files = [def[1]]
            endif
            for hp in files
                " TLogVAR hp, filereadable(hp), !isdirectory(hp), index(a:files, hp) == -1
                if filereadable(hp) && !isdirectory(hp) && index(a:files, hp) == -1
                    call add(a:files, hp)
                endif
            endfor
        endif
    endfor
endf


function! s:Select(text, from, to) "{{{3
    let sfrom = strftime('%Y-%m-%d', a:from)
    let sto = strftime('%Y-%m-%d', a:to)
    let date1 = s:GetTaskDueDate(a:text, 0, g:vikitasks#use_unspecified_dates)
    let date2 = s:GetTaskDueDate(a:text, 1, g:vikitasks#use_unspecified_dates)
    if date1 == '_'
        let rv = 1
    elseif date1 == date2
        let rv = date1 >= sfrom && date1 <= sto
    else
        let rv = (date1 >= sfrom && date1 <= sto) || (date2 >= sfrom && date2 <= sto)
    endif
    " TLogVAR sfrom, sto, date1, date2, rv
    return rv
endf


" Register BUFFER as a file that should be scanned for task lists.
function! vikitasks#AddBuffer(buffer, ...) "{{{3
    TVarArg ['save', 1]
    " TLogVAR a:buffer, save
    let fname = s:CanonicFilename(fnamemodify(a:buffer, ':p'))
    let files = s:Files()
    if filereadable(fname) && index(files, fname) == -1
        call add(files, fname)
        if save && !vikitasks#ScanCurrentBuffer(fname)
            call s:SaveInfo(files, s:Tasks())
        endif
    endif
endf


" Unregister BUFFER as a file that should be scanned for task lists.
function! vikitasks#RemoveBuffer(buffer, ...) "{{{3
    TVarArg ['save', 1]
    " TLogVAR a:buffer, save
    let fname = s:CanonicFilename(fnamemodify(a:buffer, ':p'))
    let files = s:Files()
    let fidx  = index(files, fname)
    if fidx != -1
        call remove(files, fidx)
        if save && !vikitasks#ScanCurrentBuffer(fname)
            call s:SaveInfo(files, s:Tasks())
        endif
    endif
endf


" Edit the list of files.
function! vikitasks#EditFiles() "{{{3
    let files = tlib#input#EditList('Edit task files:', copy(s:Files()))
    if files != s:files
        call s:SaveInfo(files, s:Tasks())
        call tlib#notify#Echo('Please update your task list by running :VikiTasks!', 'WarningMsg')
    endif
endf


" :nodoc:
" :display: vikitasks#Alarm(?ddays = -1)
" Display a list of alarms.
" If ddays >= 0, the constraint value in |g:vikitasks#alarms| is set to 
" ddays days.
" If ddays is -1 and |g:vikitasks#alarms| is empty, not alarms will be 
" listed.
function! vikitasks#Alarm(...) "{{{3
    TVarArg ['ddays', -1]
    " TLogVAR ddays
    if ddays < 0 && empty(g:vikitasks#alarms)
        return
    endif
    let tasks = copy(s:Tasks())
    call sort(tasks, "s:SortTasks")
    let alarms = copy(g:vikitasks#alarms)
    if ddays >= 0
        let alarms.constraint = ddays
    endif
    " TLogVAR alarms
    call s:FilterTasks(tasks, alarms)
    if !empty(tasks)
        " TLogVAR tasks
        call setqflist(tasks)
        call s:View(0, 1)
        redraw
    endif
endf


function! s:TasksRx(which_tasks) "{{{3
    return printf(s:{a:which_tasks}_rx, '.*')
endf


" :nodoc:
" Scan the current buffer for task lists.
function! vikitasks#ScanCurrentBuffer(...) "{{{3
    TVarArg ['filename', '']
    let use_buffer = empty(filename)
    if use_buffer
        let filename = s:CanonicFilename(fnamemodify(bufname('%'), ':p'))
    else
        let filename = s:CanonicFilename(filename)
    endif
    " TLogVAR filename, use_buffer
    if &buftype =~ '\<nofile\>' || (!empty(s:files_ignored) && filename =~ s:files_ignored) || !filereadable(filename) || isdirectory(filename) || empty(filename)
        return 0
    endif
    let tasks0 = s:Tasks()
    let ntasks = len(tasks0)
    let tasks = []
    let buftasks = {}
    for task in tasks0
        " TLogVAR task
        if s:CanonicFilename(task.filename) == filename
            " TLogVAR task.lnum, task
            if has_key(task, 'text')
                let buftasks[task.lnum] = task
            endif
        else
            call add(tasks, task)
        endif
        unlet task
    endfor
    " TLogVAR len(tasks)
    let rx = s:TasksRx('tasks')
    let def = {'inline': 0, 'sometasks': 0, 'letters': 'A-Z', 'levels': '0-9'}
    let @r = rx
    let update = 0
    let tasks_found = 0
    let lnum = 1
    " echom "DBG ". string(keys(buftasks))
    if use_buffer
        let lines = getline(1, '$')
    else
        let lines = readfile(filename)
    endif
    " call filter(tasks, 'v:val.filename != filename')
    for line in lines
        let text = tlib#string#Strip(line)
        if line =~ '^%\s*vikitasks:'
            let paramss = matchstr(line, '^%\s*vikitasks:\s*\zs.*$')
            " TLogVAR paramss
            if paramss =~ '^\s*none\s*$'
                return
            else
                let paramsl = split(paramss, ':')
                " TLogVAR paramsl
                call map(paramsl, 'split(v:val, "=", 1)')
                " TLogVAR paramsl
                try
                    for [var, val] in paramsl
                        let def[var] = val
                    endfor
                catch
                    echoerr 'Vikitasks: Malformed vikitasks-mode-line parameters: '. paramss
                endtry
                unlet! var val
            endif
            let rx = s:VikitasksRx(def.inline, def.sometasks, def.letters, def.levels)
        elseif line =~ rx
            " TLogVAR text
            let tasks_found = 1
            if get(get(buftasks, lnum, {}), 'text', '') != text
                " TLogVAR lnum
                " echom "DBG ". get(buftasks,lnum,'')
                let update = 1
                " TLogVAR lnum, text
                call add(tasks, {
                            \ 'filename': filename,
                            \ 'lnum': lnum,
                            \ 'text': text
                            \ })
            else
                call add(tasks, buftasks[lnum])
            endif
        endif
        let lnum += 1
    endfor
    " TLogVAR len(tasks)
    if update
        " TLogVAR update
        call vikitasks#AddBuffer(filename, 0)
        call s:SaveInfo(s:Files(), tasks)
    elseif !tasks_found
        call vikitasks#RemoveBuffer(filename, 0)
        call s:SaveInfo(s:Files(), tasks)
    endif
    return update
endf

plugin/vikitasks.vim	[[[1
150
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @GIT:         http://github.com/tomtom/vikitasks_vim/
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-12-13.
" @Last Change: 2010-11-06.
" @Revision:    226
" GetLatestVimScripts: 2894 0 :AutoInstall: vikitasks.vim
" Search for task lists and display them in a list


if !exists('g:loaded_tlib') || g:loaded_tlib < 39
    runtime plugin/02tlib.vim
    if !exists('g:loaded_tlib') || g:loaded_tlib < 39
        echoerr 'tlib >= 0.39 is required'
        finish
    endif
endif
if !exists('g:loaded_viki') || g:loaded_viki < 319
    runtime plugin/viki.vim
    if !exists('g:loaded_viki') || g:loaded_viki < 319
        echoerr 'viki >= 3.19 is required'
        finish
    endif
endif
if !exists('g:loaded_trag') || g:loaded_trag < 8
    runtime plugin/trag.vim
    if !exists('g:loaded_trag') || g:loaded_trag < 8
        echoerr 'trag >= 0.8 is required'
        finish
    endif
endif
if &cp || exists("loaded_vikitasks")
    finish
endif
let loaded_vikitasks = 3

let s:save_cpo = &cpo
set cpo&vim


" Show alarms on pending tasks.
" If 0, don't display alarms for pending tasks.
" If n > 0, display alarms for pending tasks or tasks with a deadline in n 
" days.
TLet g:vikitasks_startup_alarms = !has('clientserver') || len(split(serverlist(), '\n')) <= 1

" Scan a buffer on these events.
TLet g:vikitasks_scan_events = 'BufWritePost,BufWinEnter'

" :display: VikiTasks[!] [CONSTRAINT] [PATTERN] [FILE_PATTERNS]
" CONSTRAINT defined which tasks should be displayed. Possible values 
" for CONSTRAINT are:
"
"   today            ... Show tasks that are due today
"   current          ... Show pending and today's tasks
"   NUMBER (of days) ... Show tasks that are due within N days
"   Nd               ... Tasks for the next N days
"   Nw               ... Tasks for the next N weeks
"   Nm               ... Tasks for the next N months
"   week             ... Tasks for the next week
"   month            ... Tasks for the next month
"   .                ... Show some tasks (see |g:vikitasks#rx_letters| 
"                        and |g:vikitasks#rx_levels|)
"   *                ... Show all tasks
" 
" If N is prepended with + (e.g. "+2w"), tasks with a deadline in the 
" past are hidden.
"
" The default value for CONSTRAINT is ".".
"
" If CONSTRAINT starts with "@" or ":" it is assumed to be a PATTERN -- 
" see also |viki-tasks|.
"
" The |regexp| PATTERN is prepended with |\<| if it seems to be a word. 
" The PATTERN is made case sensitive if it contains an upper-case letter 
" and if 'smartcase' is true. Only tasks matching the PATTERN will be 
" listed. Use "." to match all tasks.
" 
" With the optional !, all files are rescanned. Otherwise cached 
" information is used. Either scan all known files (|interviki|s and 
" pages registered via |:VikiTasksAdd|) or files matching FILE_PATTERNS.
"
" The current buffer has to be a viki buffer. If it isn't, your 
" |g:vikiHomePage|, which must be set, is opened first.
"
" Examples:
"     Show all cached tasks with a date: >
"         VikiTasks
" <   Rescan files and show all tasks: >
"         VikiTasks!
" <   Show all cached tasks for today: >
"         VikiTasks today
" <   Show all current cached tasks (today or with a deadline in the 
" past) in a specified list of files: >
"         VikiTasks current Notes*.txt
command! -bang -nargs=* VikiTasks call vikitasks#Tasks(vikitasks#GetArgs(!empty("<bang>"), [<f-args>]))
cabbr vikitasks VikiTasks


" :display: :VikiTasksAdd
" Add the current buffer to |g:vikitasks#files|.
command! VikiTasksAdd call vikitasks#AddBuffer(expand('%:p'))


" :display: :VikiTasksFiles
" Edit |g:vikitasks#files|. This allows you to remove buffers from the 
" list.
command! VikiTasksFiles call vikitasks#EditFiles()


command! -count VikiTasksAlarms call vikitasks#Alarm(<count>)


augroup VikiTasks
    autocmd!
    if g:vikitasks_startup_alarms
        if has('vim_starting')
            autocmd VimEnter *  call vikitasks#Alarm()
        else
            call vikitasks#Alarm()
        endif
    endif
    if !empty(g:vikitasks_scan_events)
        exec 'autocmd '. g:vikitasks_scan_events .' * if exists("b:vikiEnabled") && b:vikiEnabled | call vikitasks#ScanCurrentBuffer(expand("<afile>:p")) | endif'
    endif
    unlet g:vikitasks_startup_alarms g:vikitasks_scan_events
augroup END


let &cpo = s:save_cpo
unlet s:save_cpo
finish

CHANGES:

0.1
- Initial release

0.2
- :VikiTasks now takes a pattern as optional second argument. This 
change makes the :VikiTasksGrep command obsolete, which was removed.
- Moved the definition of some variables from plugin/vikitasks.vim to autoload/vikitasks.vim
- Scan buffers on save
- Require tlib 0.37
- The arguments for :VikiTasks have changed

0.3
- vikitasks pseudo-mode-line: % vikitasks: letters=A-C:levels=1-3

doc/vikitasks.txt	[[[1
278
*vikitasks.txt*     Search viki files for tasks and display them in a list
                    Author: Tom Link, micathom at gmail com

This plugin provides a quick overview of priority/task lists maintained 
in different viki files. Depending on your settings, you can quickly 
search all "homepages" of intervikis or search only project-specific 
files.


Usage:
    :VikiTasks[!] [CONSTRAINT] [PATTERN] [FILE_PATTERNS]


Features:
    - Collect tasks from viki's priority lists (see |viki-tasks|)
    - Sort those tasks
    - Browse tasks with a given date
    - Optionally browse all tasks (incl. those without a date)


-----------------------------------------------------------------------
Tutorial: How does it work?~

Let's say you have the files:

foo.txt: >
    * Foo
        #A 2009-12-13 Do this
        #C 2009-12-20 Call @Anna
        #A Whatever

bar.txt: >
    * Bar
        #C 2009-12-24 Wish @Bernie good luck
        #D 2009-11-01 Do that

Let's assume you have added both files to |g:vikitasks_files|. Switch to 
the buffer foo.txt and call >

    :VikiTasks!

and you get the following list: >

    Bar.txt|2| #D 2009-11-01 Do that
    Foo.txt|2| #A 2009-12-13 Do this
    Foo.txt|3| #C 2009-12-20 Call @Anna
    Bar.txt|3| #C 2009-12-24 Wish @Bernie good luck

If you do/had done this on the 15 December 2009, the third line would be 
highlighted, i.e. the entries above the cursor refer to passed/missed 
events. If you had called :VikiTasks! (behold the bang), then the 
"Whatever" entry would have been included in the list too.

If you had called >
    
    :VikiTasks current

only the first two items would be listed.

This week's tasks (i.e. the tasks that should be accomplished today or 
within the following six days) could be listed with >
    
    :VikiTasks 6


-----------------------------------------------------------------------
Install~

Edit the vba file and type: >

    :so %

See :help vimball for details. If you have difficulties or use vim 7.0, 
please make sure, you have the current version of vimball
(vimscript #1502) installed or update your runtime.

This script requires tlib (vimscript #1863), trag (vimscript #2033), and 
viki (vimscript #861) to be installed.

Also available via git: http://github.com/tomtom/vikitasks_vim/


========================================================================
Contents~

        g:vikitasks_startup_alarms ............ |g:vikitasks_startup_alarms|
        g:vikitasks_scan_events ............... |g:vikitasks_scan_events|
        :VikiTasks ............................ |:VikiTasks|
        :VikiTasksAdd ......................... |:VikiTasksAdd|
        :VikiTasksFiles ....................... |:VikiTasksFiles|
        :VikiTasksAlarms ...................... |:VikiTasksAlarms|
        g:vikitasks#files ..................... |g:vikitasks#files|
        g:vikitasks#files_ignored ............. |g:vikitasks#files_ignored|
        g:vikitasks#intervikis ................ |g:vikitasks#intervikis|
        g:vikitasks#intervikis_ignored ........ |g:vikitasks#intervikis_ignored|
        g:vikitasks#qfl_viewer ................ |g:vikitasks#qfl_viewer|
        g:vikitasks#rx_letters ................ |g:vikitasks#rx_letters|
        g:vikitasks#rx_levels ................. |g:vikitasks#rx_levels|
        g:vikitasks#cache ..................... |g:vikitasks#cache|
        g:vikitasks#alarms .................... |g:vikitasks#alarms|
        g:vikitasks#use_end_date .............. |g:vikitasks#use_end_date|
        g:vikitasks#use_unspecified_dates ..... |g:vikitasks#use_unspecified_dates|
        g:vikitasks#remove_unreadable_files ... |g:vikitasks#remove_unreadable_files|
        vikitasks#Tasks ....................... |vikitasks#Tasks()|
        vikitasks#AddBuffer ................... |vikitasks#AddBuffer()|
        vikitasks#RemoveBuffer ................ |vikitasks#RemoveBuffer()|
        vikitasks#EditFiles ................... |vikitasks#EditFiles()|


========================================================================
plugin/vikitasks.vim~

                                                    *g:vikitasks_startup_alarms*
g:vikitasks_startup_alarms     (default: !has('clientserver') || len(split(serverlist(), '\n')) <= 1)
    Show alarms on pending tasks.
    If 0, don't display alarms for pending tasks.
    If n > 0, display alarms for pending tasks or tasks with a deadline in n 
    days.

                                                    *g:vikitasks_scan_events*
g:vikitasks_scan_events        (default: 'BufWritePost,BufWinEnter')
    Scan a buffer on these events.

                                                    *:VikiTasks*
VikiTasks[!] [CONSTRAINT] [PATTERN] [FILE_PATTERNS]
    CONSTRAINT defined which tasks should be displayed. Possible values 
    for CONSTRAINT are:
    
      today            ... Show tasks that are due today
      current          ... Show pending and today's tasks
      NUMBER (of days) ... Show tasks that are due within N days
      Nd               ... Tasks for the next N days
      Nw               ... Tasks for the next N weeks
      Nm               ... Tasks for the next N months
      week             ... Tasks for the next week
      month            ... Tasks for the next month
      .                ... Show some tasks (see |g:vikitasks#rx_letters| 
                           and |g:vikitasks#rx_levels|)
      *                ... Show all tasks
    
    If N is prepended with + (e.g. "+2w"), tasks with a deadline in the 
    past are hidden.
    
    The default value for CONSTRAINT is ".".
    
    If CONSTRAINT starts with "@" or ":" it is assumed to be a PATTERN -- 
    see also |viki-tasks|.
    
    The |regexp| PATTERN is prepended with |\<| if it seems to be a word. 
    The PATTERN is made case sensitive if it contains an upper-case letter 
    and if 'smartcase' is true. Only tasks matching the PATTERN will be 
    listed. Use "." to match all tasks.
    
    With the optional !, all files are rescanned. Otherwise cached 
    information is used. Either scan all known files (|interviki|s and 
    pages registered via |:VikiTasksAdd|) or files matching FILE_PATTERNS.
    
    The current buffer has to be a viki buffer. If it isn't, your 
    |g:vikiHomePage|, which must be set, is opened first.
    
    Examples:
        Show all cached tasks with a date: >
            VikiTasks
<     Rescan files and show all tasks: >
            VikiTasks!
<     Show all cached tasks for today: >
            VikiTasks today
<     Show all current cached tasks (today or with a deadline in the 
    past) in a specified list of files: >
            VikiTasks current Notes*.txt
<

                                                    *:VikiTasksAdd*
:VikiTasksAdd
    Add the current buffer to |g:vikitasks#files|.

                                                    *:VikiTasksFiles*
:VikiTasksFiles
    Edit |g:vikitasks#files|. This allows you to remove buffers from the 
    list.

                                                    *:VikiTasksAlarms*
:VikiTasksAlarms


========================================================================
autoload/vikitasks.vim~

                                                    *g:vikitasks#files*
g:vikitasks#files              (default: [])
    A list of glob patterns (or files) that will be searched for task 
    lists.
    Can be buffer-local.
    If you add ! to 'viminfo', this variable will be automatically saved 
    between editing sessions.
    Alternatively, add new items in ~/vimfiles/after/plugin/vikitasks.vim

                                                    *g:vikitasks#files_ignored*
g:vikitasks#files_ignored      (default: [])
    A list of |regexp| patterns for filenames that should not be 
    scanned.

                                                    *g:vikitasks#intervikis*
g:vikitasks#intervikis         (default: 0)
    If non-null, automatically add the homepages of your intervikis to 
    |g:vikitasks#files|.
    If the value is 2, scan all files (taking into account the interviki 
    suffix) in the interviki's top directory.
    Can be buffer-local.

                                                    *g:vikitasks#intervikis_ignored*
g:vikitasks#intervikis_ignored (default: [])
    A list of ignored intervikis.
    Can be buffer-local.

                                                    *g:vikitasks#qfl_viewer*
g:vikitasks#qfl_viewer         (default: '')
    The viewer for the quickfix list. If empty, use |:TRagcw|.

                                                    *g:vikitasks#rx_letters*
g:vikitasks#rx_letters         (default: 'A-W')
    Item classes that should be included in the list when calling 
    |:VikiTasks|.
    A user-defined value must be set in |vimrc| before the plugin is 
    loaded.

                                                    *g:vikitasks#rx_levels*
g:vikitasks#rx_levels          (default: '1-5')
    Item levels that should be included in the list when calling 
    |:VikiTasks|.
    A user-defined value must be set in |vimrc| before the plugin is 
    loaded.

                                                    *g:vikitasks#cache*
g:vikitasks#cache              (default: tlib#cache#Filename('vikitasks', 'files', 1))
    Cache file name.
    By default, use |tlib#cache#Filename()| to determine the file name.

                                                    *g:vikitasks#alarms*
g:vikitasks#alarms             (default: {'all_tasks': 0, 'tasks': 'sometasks', 'constraint': 14})
    Definition of the tasks that should be included in the Alarms list.
    Fields:
      all_tasks  ... If non-null, also display tasks with no due-date
      tasks      ... Either 'tasks' or 'sometasks'
      constraint ... See |:VikiTasks|

                                                    *g:vikitasks#use_end_date*
g:vikitasks#use_end_date       (default: 1)
    If true, the end-date of date ranges (FROM..TO) is significant.

                                                    *g:vikitasks#use_unspecified_dates*
g:vikitasks#use_unspecified_dates (default: 0)
    Interpret entries with an unspecified date ("_") as current tasks.

                                                    *g:vikitasks#remove_unreadable_files*
g:vikitasks#remove_unreadable_files (default: 1)
    If true, remove unreadable files from the tasks list.

                                                    *vikitasks#Tasks()*
vikitasks#Tasks(?{'all_tasks': 0, 'cached': 1, 'files': [], 'constraint': '', 'rx': ''})
    If files is non-empty, use these files (glob patterns actually) 
    instead of those defined in |g:vikitasks#files|.

                                                    *vikitasks#AddBuffer()*
vikitasks#AddBuffer(buffer, ...)
    Register BUFFER as a file that should be scanned for task lists.

                                                    *vikitasks#RemoveBuffer()*
vikitasks#RemoveBuffer(buffer, ...)
    Unregister BUFFER as a file that should be scanned for task lists.

                                                    *vikitasks#EditFiles()*
vikitasks#EditFiles()
    Edit the list of files.



vim:tw=78:fo=tcq2:isk=!-~,^*,^|,^":ts=8:ft=help:norl:
