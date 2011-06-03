" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
autoload/trag/general.vim	[[[1
21
" general.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2008-12-01.
" @Last Change: 2009-02-25.
" @Revision:    0.0.15

let s:save_cpo = &cpo
set cpo&vim


function! trag#general#Rename(world, selected, rx, subst) "{{{3
    let cmd = 's/\C\<'. escape(tlib#rx#Escape(a:rx), '/') .'\>/'. escape(tlib#rx#EscapeReplace(a:subst), '/') .'/ge'
    " let cmd = 's/\C'. escape(a:rx, '/') .'/'. escape(tlib#rx#EscapeReplace(a:subst), '/') .'/ge'
    return trag#RunCmdOnSelected(a:world, a:selected, cmd)
endf


let &cpo = s:save_cpo
unlet s:save_cpo
autoload/trag/java.vim	[[[1
19
" java.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2008-12-01.
" @Last Change: 2009-02-27.
" @Revision:    0.0.24

let s:save_cpo = &cpo
set cpo&vim


function! trag#java#Rename(world, selected, from, to) "{{{3
    return trag#rename#Rename(a:world, a:selected, a:from, a:to, suffix)
endf


let &cpo = s:save_cpo
unlet s:save_cpo
autoload/trag/rename.vim	[[[1
43
" rename.vim
" @Author:      Thomas Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-02-27.
" @Last Change: 2009-02-27.
" @Revision:    0.0.3

let s:save_cpo = &cpo
set cpo&vim


function! trag#rename#Rename(world, selected, from, to, suffix) "{{{3
    let rv = trag#general#Rename(a:world, a:selected, a:from, a:to)
    let from_file = a:from . a:suffix
    let to_file = a:to . a:suffix
    let bnr = bufnr(from_file)
    " TLogVAR from_file, to_file, bnr
    if filereadable(to_file)
        echom 'TRag: Cannot rename file, file already exists: '. to_file
    elseif filereadable(from_file)
        call rename(from_file, to_file)
        echom 'TRag: Renamed file '. from_file .' -> '. to_file
    endif
    if bnr != -1
        if bufnr(to_file) != -1
            call tlib#notify#Echo('TRag: Cannot rename buffer, buffer already exists: '. to_file, 'Error')
        else
            exec 'buffer '. bnr
            exec 'file! '. fnameescape(to_file)
            w!
            echom 'TRag: Renamed buffer '. from_file .' -> '. to_file
            if filereadable(from_file) && !filereadable(to_file)
                call tlib#notify#Echo('TRag: Inconsistent state. Please rename the file '. from_file .' -> '. to_file, 'Error')
            endif
        endif
    endif
    return rv
endf


let &cpo = s:save_cpo
unlet s:save_cpo
autoload/trag/viki.vim	[[[1
20
" viki.vim
" @Author:      Thomas Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-02-27.
" @Last Change: 2009-02-27.
" @Revision:    0.0.5

let s:save_cpo = &cpo
set cpo&vim


function! trag#viki#Rename(world, selected, from, to) "{{{3
    let suffix = tlib#var#Get('vikiNameSuffix', 'bg', '')
    return trag#rename#Rename(a:world, a:selected, a:from, a:to, suffix)
endf


let &cpo = s:save_cpo
unlet s:save_cpo
autoload/trag.vim	[[[1
996
" trag.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-09-29.
" @Last Change: 2010-03-30.
" @Revision:    0.0.898

" call tlog#Log('Load: '. expand('<sfile>')) " vimtlib-sfile


""" Known Types {{{1

" 0 ... use the built-in search.
" 1 ... use vimgrep.
" 2 ... use vimgrep but set 'ei' to all; this means special file 
"       encodings won't be detected
" Please not, this is only in effect with simple searches (as for 0.3 
" all searches are simple). For more complex multi-line patterns, the 
" built-in search will be used (some day in the future).
TLet g:trag_search_mode = 0
" TLet g:trag_search_mode = 2

" If no project files are defined, evaluate this expression as 
" fallback-strategy.
TLet g:trag_get_files = 'split(glob("*"), "\n")'
TLet g:trag_get_files_java = 'split(glob("**/*.java"), "\n")'
TLet g:trag_get_files_c = 'split(glob("**/*.[ch]"), "\n")'
TLet g:trag_get_files_cpp = 'split(glob("**/*.[ch]"), "\n")'

" " If non-empty, display signs at matching lines.
" TLet g:trag_sign = has('signs') ? '>' : ''
" if !empty(g:trag_sign)
"     exec 'sign define TRag text='. g:trag_sign .' texthl=Special'
" 
"     " Clear all trag-related signs.
"     command! TRagClearSigns call tlib#signs#ClearAll('TRag')
" endif


" :nodoc:
function! trag#TRagDefKind(args) "{{{3
    " TLogVAR a:args
    " TLogDBG string(matchlist(a:args, '^\(\S\+\)\s\+\(\S\+\)\s\+/\(.\{-}\)/$'))
    let [match, kind, filetype, regexp; rest] = matchlist(a:args, '^\(\S\+\)\s\+\(\S\+\)\s\+/\(.\{-}\)/$')
    let var = ['g:trag_rxf', kind]
    if filetype != '*' && filetype != '.'
        call add(var, filetype)
    endif
    let varname = join(var, '_')
    let {varname} = substitute(regexp, '\\/', '/', 'g')
    if has_key(g:trag_kinds, kind)
        call add(g:trag_kinds[kind], filetype)
    else
        let g:trag_kinds[kind] = [filetype]
    endif
endf




TRagDefKind identity * /\C%s/

" Left hand side value in an assignment.
" Examples:
" l foo =~ foo = 1
TRagDefKind l * /\C%s\s*[^=]*=[^=~<>]/
" TRagDefKind l * /\C\<%s\>\s*=[^=~<>]/
" L foo =~ fufoo0 = 1
" TRagDefKind L * /\C%s[^=]*=[^=~<>]/

" Right hand side value in an assignment.
" Examples:
" r foo =~ bar = foo
TRagDefKind r * /\C[^!=~<>]=.\{-}%s/
" L foo =~ bar = fufoo0
" TRagDefKind r * /\C[^!=~<>]=.\{-}\<%s\>/
" TRagDefKind R * /\C[^!=~<>]=.\{-}%s/

" Markers: TODO, TBD, FIXME, OPTIMIZE
TRagDefKind todo * /\C\(TBD\|TODO\|FIXME\|OPTIMIZE\)/

" A mostly general rx format string for function calls.
TRagDefKind f * /\C%s\S*\s*(/
" TRagDefKind f * /\C\<%s\>\s*(/
" TRagDefKind F * /\C%s\S*\s*(/

" A mostly general rx format string for words.
TRagDefKind w * /\C\<%s\>/
TRagDefKind W * /\C.\{-}%s.\{-}/

TRagDefKind fuzzy * /\c%{fuzzyrx}/


TRagDefFiletype java java
let s:java_mod = '\(\<\(final\|public\|protected\|private\|synchronized\|volatile\|abstract\)\s\+\)*'
let s:java_type = '\(boolean\|byte\|short\|int\|long\|float\|double\|char\|void\|\u[[:alnum:]_.]*\)\s\+'
exec 'TRagDefKind c java /\C^\s*'. s:java_mod .'class\s\+%s/'
exec 'TRagDefKind d java /\C^\s*'. s:java_mod .'\(\w\+\(\[\]\)*\)\s\+%s\s*(/'
exec 'TRagDefKind f java /\(\(;\|{\|^\)\s*'. s:java_mod . s:java_type .'\)\@<!%s\s*\([(;]\|$\)/'
TRagDefKind i java /\C^\s*\(\/\/\|\/\*\).\{-}%s/
TRagDefKind x java /\C^.\{-}\<\(interface\|class\)\s\+.\{-}\s\+\(extends\|implements\)\s\+%s/
unlet s:java_mod s:java_type



TRagDefFiletype ruby rb
TRagDefKind w ruby /\C[:@]\?\<%s\>/
TRagDefKind W ruby /\C[^;()]\{-}%s[^;()]\{-}/
TRagDefKind c ruby /\C\<class\s\+\(\u\w*::\)*%s\>/
" TRagDefKind d ruby /\C\<\(def\s\+\(\u\w*\.\)*\|attr\(_\w\+\)\?\s\+\(:\w\+,\s\+\)*:\)%s\>/
" TRagDefKind D ruby /\C\<\(def\s\+\(\u\w*\.\)*\|attr\(_\w\+\)\?\s\+\(:\w\+,\s\+\)*:\).\{-}%s/
TRagDefKind d ruby /\C\<\(def\s\+\(\u\w*\.\)*\|attr\(_\w\+\)\?\s\+\(:\w\+,\s\+\)*:\)%s/
" TRagDefKind f ruby /\C\(\<def\s\+\(\u\w*\.\)*\|:\)\@<!\<%s\>\s*\([(;]\|$\)/
" TRagDefKind f ruby /\(;\|^\)\s*\<%s\>\s*\([(;]\|$\)/
" TRagDefKind f ruby /\(;\|^\)\s*[^();]\{-}%s\s*\([(;]\|$\)/
" TRagDefKind f ruby /\(;\|^\)\s*%s\s*\([(;]\|$\)/
TRagDefKind f ruby /\(\<def\s\+\)\@<!%s\s*\([(;]\|$\)/
" TRagDefKind i ruby /\C^\s*#.\{-}%s/
TRagDefKind i ruby /\C^\s*#%s/
TRagDefKind m ruby /\C\<module\s\+\(\u\w*::\)*%s/
" TRagDefKind l ruby /\C\<%s\>\(\s*,\s*[[:alnum:]_@$]\+\s*\)*\s*=[^=~<>]/
TRagDefKind l ruby /\C%s\(\s*,\s*[[:alnum:]_@$]\+\s*\)*\s*=[^=~<>]/
TRagDefKind x ruby /\C\s\*class\>.\{-}<\s*%s/


TRagDefFiletype vim vim .vimrc _vimrc
TRagKeyword vim [:alnum:]_:#
TRagDefKind W vim /\C[^|]\{-}%s[^|]\{-}/
" TRagDefKind d vim /\C\<\(fu\%%[nction]!\?\|com\%%[mand]!\?\(\s\+-\S\+\)*\)\s\+\<%s\>/
" TRagDefKind D vim /\C\<\(fu\%%[nction]!\?\|com\%%[mand]!\?\(\s\+-\S\+\)*\)\.+%s/
" TRagDefKind d vim /\C\<\(fu\%%[nction]!\?\|com\%%[mand]!\?\(\s\+-\S\+\)*\)[^|]\{-}%s/
TRagDefKind d vim /\C\<\(fu\%%[nction]!\?\s\+\|com\%%[mand]!\?\s\+\(-\S\+\s\+\)*\)%s/
" TRagDefKind f vim /\C\(\(^\s*\<fu\%%[nction]!\?\s\+\(s:\)\?\|com\%%[mand]!\?\(\s\+-\S\+\)*\s\+\)\@<!\<%s\>\s*(\|\(^\||\)\s*\<%s\>\)/
" TRagDefKind F vim /\C\(\(^\s*\<fu\%%[nction]!\?\s\+\(s:\)\?\|com\%%[mand]\(\s\+-\S\+\)*\s\+\)\@<!\S\{-}%s\S\{-}(\|\(^\||\)\s*%s\)/
TRagDefKind f vim /\C\(\(^\s*\<fu\%%[nction]!\?\s\+\(s:\)\?\|com\%%[mand]\(\s\+-\S\+\)*\s\+\)\@<!\S\{-}%s\S\{-}(\|\(^\||\)\s*%s\)/
" TRagDefKind i vim /\C^\s*".\{-}%s/
" This isn't correct. It doesn't find in-line comments.
TRagDefKind i vim /\C^\s*"%s/
" TRagDefKind r vim /\C^\s*let\s\+\S\+\s*=.\{-}\<%s\>/
" TRagDefKind R vim /\C^\s*let\s\+\S\+\s*=.\{-}%s/
TRagDefKind r vim /\C^\s*let\s\+\S\+\s*=[^|]\{-}%s/
" TRagDefKind l vim /\C^\s*let\s\+\<%s\>/
" TRagDefKind L vim /\C^\s*let\s.\{-}%s/
TRagDefKind l vim /\C^\s*let\s\+[^=|]\{-}%s/


TRagDefFiletype viki txt viki dpl
TRagDefKind i viki /\C^\s\+%%%s/
TRagDefKind d viki /\C^\s*#\u\w*\s\+.\{-}\(id=%s\|%s=\)/
TRagDefKind h viki /\C^\*\+\s\+%s/
TRagDefKind l viki /\C^\s\+%s\s\+::/
TRagDefKind r viki /\C^\s\+\(.\{-}\s::\|[-+*#]\|[@?].\)\s\+%s/
TRagDefKind todo viki /\C\(TODO:\?\|FIXME:\?\|+++\|!!!\|###\|???\)\s\+%s/



""" File sets {{{1

" :doc:
" The following variables provide alternatives to collecting 
" your project's file list on the basis of you tags files.
"
" These variables are tested in the order as listed here. If the value 
" of a variable is non-empty, this one will be used instead of the other 
" methods.
"
" The tags file is used as a last ressort.

" 1. A list of files. Can be buffer local.
TLet g:trag_files = []

" 2. A glob pattern -- this should be an absolute path and may contain ** 
" (see |glob()| and |wildcards|). Can be buffer local.
TLet g:trag_glob = ''

" 3. Filetype-specific project files.
TLet g:trag_project_ruby = 'Manifest.txt'

" 4. The name of a file containing the projects file list. This file could be 
" generated via make. Can be buffer local.
TLet g:trag_project = ''

" 5. The name of a git repository that includes all files of interest. 
" If the value is "*", trag will search from the current directory 
" (|getcwd()|) upwards for a .git directory.
" If the value is "finddir", use |finddir()| to find a .git directory.
" Can be buffer local.
TLet g:trag_git = ''



""" input#List {{{1

" :nodoc:
TLet g:trag_edit_world = {
            \ 'type': 's',
            \ 'query': 'Select file',
            \ 'pick_last_item': 1,
            \ 'scratch': '__TRagEdit__',
            \ 'return_agent': 'tlib#agent#ViewFile',
            \ }


" :nodoc:
TLet g:trag_qfl_world = {
            \ 'type': 'mi',
            \ 'query': 'Select entry',
            \ 'pick_last_item': 0,
            \ 'resize_vertical': 0,
            \ 'resize': 20,
            \ 'scratch': '__TRagQFL__',
            \ 'tlib_UseInputListScratch': 'call trag#SetSyntax()',
            \ 'key_handlers': [
                \ {'key':  5, 'agent': 'trag#AgentWithSelected', 'key_name': '<c-e>', 'help': 'Run a command on selected lines'},
                \ {'key':  6, 'agent': 'trag#AgentRefactor',     'key_name': '<c-f>', 'help': 'Run a refactor command'},
                \ {'key': 16, 'agent': 'trag#AgentPreviewQFE',   'key_name': '<c-p>', 'help': 'Preview'},
                \ {'key': 60, 'agent': 'trag#AgentGotoQFE',      'key_name': '<',     'help': 'Jump (don''t close the list)'},
                \ {'key': 19, 'agent': 'trag#AgentSplitBuffer',  'key_name': '<c-s>', 'help': 'Show in split buffer'},
                \ {'key': 20, 'agent': 'trag#AgentTabBuffer',    'key_name': '<c-t>', 'help': 'Show in tab'},
                \ {'key': 22, 'agent': 'trag#AgentVSplitBuffer', 'key_name': '<c-v>', 'help': 'Show in vsplit buffer'},
                \ {'key': "\<c-insert>", 'agent': 'trag#SetFollowCursor', 'key_name': '<c-ins>', 'help': 'Toggle trace cursor'},
            \ ],
            \ 'return_agent': 'trag#AgentEditQFE',
            \ }
                " \ {'key': 23, 'agent': 'trag#AgentOpenBuffer',   'key_name': '<c-w>', 'help': 'View in window'},





""" Functions {{{1
    
let s:grep_rx = ''


function! trag#SetSyntax() "{{{3
    let syntax = get(s:world, 'trag_list_syntax', '')
    let nextgroup = get(s:world, 'trag_list_syntax_nextgroup', '')
    if !empty(syntax)
        exec printf('runtime syntax/%s.vim', syntax)
    endif
    syn match TTagedFilesFilename / \zs.\{-}\ze|\d\+| / nextgroup=TTagedFilesLNum
    if !empty(nextgroup)
        exec 'syn match TTagedFilesLNum /|\d\+|\s\+/ nextgroup='. nextgroup
    else
        syn match TTagedFilesLNum /|\d\+|/
    endif
    hi def link TTagedFilesFilename Directory
    hi def link TTagedFilesLNum LineNr
endf


function! s:GetFiles() "{{{3
    if !exists('b:trag_files_')
        call trag#SetFiles()
    endif
    if empty(b:trag_files_)
        " echohl Error
        " echoerr 'TRag: No project files'
        " echohl NONE
        let trag_get_files = tlib#var#Get('trag_get_files_'. &filetype, 'bg', '')
        " TLogVAR trag_get_files
        if empty(trag_get_files)
            let trag_get_files = tlib#var#Get('trag_get_files', 'bg', '')
            " TLogVAR trag_get_files
        endif
        echom 'TRag: No project files ... use: '. trag_get_files
        let b:trag_files_ = eval(trag_get_files)
    endif
    " TLogVAR b:trag_files_
    return b:trag_files_
endf


function! trag#ClearFiles() "{{{3
    let b:trag_files_ = []
endf


function! trag#AddFiles(files) "{{{3
    if tlib#type#IsString(a:files)
        let files_ = eval(a:files)
    else
        let files_ = a:files
    endif
    if !tlib#type#IsList(files_)
        echoerr 'trag_files must result in a list: '. string(a:files)
    elseif exists('b:trag_files_')
        let b:trag_files_ += files_
    else
        let b:trag_files_ = files_
    endif
    unlet files_
endf


function! trag#GetProjectFiles(manifest) "{{{3
    if filereadable(a:manifest)
        " TLogVAR a:manifest
        let files = readfile(a:manifest)
        let cwd   = getcwd()
        try
            call tlib#dir#CD(fnamemodify(a:manifest, ':h'), 1)
            call map(files, 'fnamemodify(v:val, ":p")')
            return files
        finally
            call tlib#dir#CD(cwd, 1)
        endtry
    endif
    return []
endf


function! trag#GetGitFiles(repos) "{{{3
    let repos   = tlib#dir#PlainName(a:repos)
    let basedir = substitute(repos, '[\/]\.git\([\/]\)\?$', '', '')
    " TLogVAR repos, basedir
    " TLogVAR getcwd()
    call tlib#dir#Push(basedir)
    " TLogVAR getcwd()
    try
        let files = split(system('git ls-files'), '\n')
        " TLogVAR files
        call map(files, 'basedir . g:tlib_filename_sep . v:val')
        return files
    finally
        call tlib#dir#Pop()
    endtry
    return []
endf


" Set the files list from the files included in a given git repository.
function! trag#SetGitFiles(repos) "{{{3
    let files = trag#GetGitFiles(a:repos)
    if !empty(files)
        call trag#ClearFiles()
        let b:trag_files_ = files
        echom len(files) ." files from the git repository."
    endif
endf


" :def: function! trag#SetFiles(?files=[])
function! trag#SetFiles(...) "{{{3
    TVarArg ['files', []]
    call trag#ClearFiles()
    if empty(files)
        unlet! files
        let files = tlib#var#Get('trag_files', 'bg', [])
        " TLogVAR files, empty(files)
        if empty(files)
            let glob = tlib#var#Get('trag_glob', 'bg', '')
            if !empty(glob)
                " TLogVAR glob
                let files = split(glob(glob), '\n')
            else
                let proj = tlib#var#Get('trag_project_'. &filetype, 'bg', tlib#var#Get('trag_project', 'bg', ''))
                " TLogVAR proj
                if !empty(proj)
                    " let proj = fnamemodify(proj, ':p')
                    let proj = findfile(proj, '.;')
                    let files = trag#GetProjectFiles(proj)
                else
                    let git_repos = tlib#var#Get('trag_git', 'bg', '')
                    if git_repos == '*'
                        let git_repos = trag#FindGitRepos()
                    elseif git_repos == "finddir"
                        let git_repos = finddir('.git')
                    endif
                    if !empty(git_repos)
                        let files = trag#GetGitFiles(git_repos)
                    end
                endif
            endif
        endif
    endif
    " TLogVAR files
    if !empty(files)
        call map(files, 'fnamemodify(v:val, ":p")')
        " TLogVAR files
        call trag#AddFiles(files)
    endif
    " TLogVAR b:trag_files_
    if empty(b:trag_files_)
        let files0 = taglist('.')
        " Filter bogus entry?
        call filter(files0, '!empty(v:val.kind)')
        call map(files0, 'v:val.filename')
        call sort(files0)
        let last = ''
        try
            call tlib#progressbar#Init(len(files0), 'TRag: Collect files %s', 20)
            let fidx = 0
            for f in files0
                call tlib#progressbar#Display(fidx)
                let fidx += 1
                " if f != last && filereadable(f)
                if f != last
                    call add(b:trag_files_, f)
                    let last = f
                endif
            endfor
        finally
            call tlib#progressbar#Restore()
        endtry
    endif
endf


function! trag#FindGitRepos() "{{{3
    let dir = fnamemodify(getcwd(), ':p')
    let git = tlib#file#Join([dir, '.git'])
    while !isdirectory(git)
        let dir1 = fnamemodify(dir, ':h')
        if dir == dir1
            break
        else
            let dir = dir1
        endif
        let git = tlib#file#Join([dir, '.git'])
    endwh
    if isdirectory(git)
        return git
    else
        return ''
    endif
endf


" Edit a file from the project catalog. See |g:trag_project| and 
" |:TRagfile|.
function! trag#Edit() "{{{3
    let w = tlib#World#New(copy(g:trag_edit_world))
    let w.base = s:GetFiles()
    let w.show_empty = 1
    let w.pick_last_item = 0
    call w.SetInitialFilter(matchstr(expand('%:t:r'), '^\w\+'))
    call w.Set_display_format('filename')
    " TLogVAR w.base
    call tlib#input#ListW(w)
endf


" Test j trag
" Test n tragfoo
" Test j trag(foo)
" Test n tragfoo(foo)
" Test j trag
" Test n tragfoo

" TODO: If the use of regular expressions alone doesn't meet your 
" demands, you can define the functions trag#Process_{kind}_{filesuffix} 
" or trag#Process_{kind}, which will be run on every line with the 
" arguments: match, line, quicklist, filename, lineno. This function 
" returns [match, line]. If match != -1, the line will be added to the 
" quickfix list.
" If such a function is defined, it will be called for every line.

" :def: function! trag#Grep(args, ?replace=1, ?files=[])
" args: A string with the format:
"   KIND REGEXP
"   KIND1,KIND2 REGEXP
"
" If the variables [bg]:trag_rxf_{kind}_{&filetype} or 
" [bg]:trag_rxf_{kind} exist, these will be taken as format string (see 
" |printf()|) to format REGEXP.
"
" EXAMPLE:
" trag#Grep('v foo') will find by default take g:trag_rxf_v and find 
" lines that looks like "\<foo\>\s*=[^=~]", which most likely is a 
" variable definition in many programming languages. I.e. it will find 
" lines like: >
"   foo = 1
" < but not: >
"   def foo(bar)
"   call foo(bar)
"   if foo == 1
function! trag#Grep(args, ...) "{{{3
    TVarArg ['replace', 1], ['files', []]
    " TLogVAR a:args, replace, files
    let [kindspos, kindsneg, rx] = s:SplitArgs(a:args)
    " TLogVAR kindspos, kindsneg, rx, a:args
    if empty(rx)
        let rx = '.\{-}'
        " throw 'Malformed arguments (should be: "KIND REGEXP"): '. string(a:args)
    endif
    " TAssertType rx, 'string'
    let s:grep_rx = rx
    " TLogVAR kindspos, kindsneg, rx, files
    if empty(files)
        let files = s:GetFiles()
    else
        let files = split(join(map(files, 'glob(v:val)'), "\n"), '\n')
    endif
    " TLogVAR files
    " TAssertType files, 'list'
    call tlib#progressbar#Init(len(files), 'TRag: Grep %s', 20)
    if replace
        call setqflist([])
    endif
    let search_mode = g:trag_search_mode
    " TLogVAR search_mode
    let scratch = {}
    try
        " if search_mode == 2
        "     let ei = &ei
        "     set ei=all
        " endif
        let fidx  = 0
        let strip = 0
        " TLogVAR files
        let qfl_top = len(getqflist())
        for f in files
            " TLogVAR f
            call tlib#progressbar#Display(fidx, ' '. pathshorten(f))
            let rxpos = s:GetRx(f, kindspos, rx, '.')
            " let rxneg = s:GetRx(f, kindsneg, rx, '')
            let rxneg = s:GetRx(f, kindsneg, '', '')
            " TLogVAR kindspos, kindsneg, rx, rxpos, rxneg
            let fidx += 1
            if !filereadable(f) || empty(rxpos)
                " TLogDBG f .': continue '. filereadable(f) .' '. empty(rxpos)
                continue
            endif
            let fext = fnamemodify(f, ':e')
            let prcacc = []
            " TODO: This currently doesn't work.
            " for kindand in kinds
            "     for kind in kindand
            "         let prc = 'trag#Process_'. kind .'_'. fext
            "         if exists('*'. prc)
            "             call add(prcacc, prc)
            "         else
            "             let prc = 'trag#Process_'. kind
            "             if exists('*'. prc)
            "                 call add(prcacc, prc)
            "             endif
            "         endif
            "     endfor
            " endfor
            " When we don't have to process every line, we slurp the file 
            " into a buffer and use search(), which should be faster than 
            " running match() on every line.
            if empty(prcacc)
                " TLogVAR search_mode, rxneg
                if search_mode == 0 || !empty(rxneg)
                    let qfl = {}
                    " if empty(scratch)
                    "     let scratch = {'scratch': '__TRagFileScratch__'}
                    "     call tlib#scratch#UseScratch(scratch)
                    "     resize 1
                    "     let lazyredraw = &lazyredraw
                    "     set lazyredraw
                    " endif
                    " norm! ggdG
                    " exec 'silent 0read '. tlib#arg#Ex(f)
                    " silent exec 'g/'. escape(rxpos, '/') .'/ call s:AddCurrentLine(f, qfl, rxneg)'
                    " norm! ggdG
                    " TLogVAR qfl
                    let lnum = 1
                    for line in readfile(f)
                        if line =~ rxpos && (empty(rxneg) || line !~ rxneg)
                            let qfl[lnum] = {"filename": f, "lnum": lnum, "text": tlib#string#Strip(line)}
                        endif
                        let lnum += 1
                    endfor
                    " TLogVAR qfl
                    if !empty(qfl)
                        call setqflist(values(qfl), 'a')
                    endif
                else
                    " TLogDBG 'vimgrepadd /'. escape(rxpos, '/') .'/j '. tlib#arg#Ex(f)
                    " TLogVAR len(getqflist())
                    " silent! exec 'vimgrepadd /'. escape(rxpos, '/') .'/gj '. tlib#arg#Ex(f)
                    " silent! exec 'noautocmd vimgrepadd /'. escape(rxpos, '/') .'/j '. tlib#arg#Ex(f)
                    silent! exec 'noautocmd vimgrepadd /'. escape(rxpos, '/') .'/j '. tlib#arg#Ex(f)
                    let strip = 1
                endif
            else
                let qfl = []
                let lnum = 0
                for line in readfile(f)
                    let lnum += 1
                    let m = match(line, rxpos)
                    for prc in prcacc
                        let [m, line] = call(prc, [m, line, qfl, f, lnum])
                    endfor
                    if m != -1
                        call add(qfl, {
                                    \ 'filename': f,
                                    \ 'lnum': lnum,
                                    \ 'text': tlib#string#Strip(line),
                                    \ })
                    endif
                endfor
                call setqflist(qfl, 'a')
            endif
        endfor

        let qfl1 = getqflist()
        " TLogVAR qfl1
        if strip
            let qfl1[qfl_top : -1] = map(qfl1[qfl_top : -1], 's:StripText(v:val)')
            call setqflist(qfl1, 'r')
        endif

        " TLogDBG 'qfl:'. string(getqflist())
        return qfl1[qfl_top : -1]
    finally
        " if search_mode == 2
        "     let &ei = ei
        " endif
        if !empty(scratch)
            call tlib#scratch#CloseScratch(scratch)
            let &lazyredraw = lazyredraw
        endif
        call tlib#progressbar#Restore()
    endtry
endf


function! s:AddCurrentLine(file, qfl, rxneg) "{{{3
    " TLogVAR a:file, a:rxneg
    let lnum = line('.')
    let text = getline(lnum)
    " TLogVAR lnum, text
    if empty(a:rxneg) || text !~ a:rxneg
        let a:qfl[lnum] = {"filename": a:file, "lnum": lnum, "text": tlib#string#Strip(text)}
    endif
endf


function! s:StripText(rec) "{{{3
    let a:rec['text'] = tlib#string#Strip(a:rec['text'])
    return a:rec
endf


function! s:SplitArgs(args) "{{{3
    " TLogVAR a:args
    let kind = matchstr(a:args, '^\S\+')
    if kind == '.' || kind == '*'
        let kind = ''
    endif
    let rx = matchstr(a:args, '\s\zs.*')
    if stridx(kind, '#') != -1
        let kind = substitute(kind, '#', '', 'g')
        let rx = tlib#rx#Escape(rx)
    endif
    let kinds = split(kind, '[!-]', 1)
    let kindspos = s:SplitArgList(get(kinds, 0, ''), [['identity']])
    let kindsneg = s:SplitArgList(get(kinds, 1, ''), [])
    " TLogVAR a:args, kinds, kind, rx, kindspos, kindsneg
    return [kindspos, kindsneg, rx]
endf


function! s:SplitArgList(string, default) "{{{3
    let rv = map(split(a:string, ','), 'reverse(split(v:val, ''\.'', 1))')
    if empty(rv)
        return a:default
    else
        return rv
    endif
endf


function! trag#ClearCachedRx() "{{{3
    let s:rx_cache = {}
endf
call trag#ClearCachedRx()


function! s:GetRx(filename, kinds, rx, default) "{{{3
    " TLogVAR a:filename, a:kinds, a:rx, a:default
    if empty(a:kinds)
        return a:default
    endif
    let rxacc = []
    let ext   = fnamemodify(a:filename, ':e')
    if empty(ext)
        let ext = a:filename
    endif
    let filetype = get(g:trag_filenames, ext, '')
    if empty(filetype) && !has('fname_case')
        let filetype = get(g:trag_filenames, tolower(ext), '')
    endif
    let id = filetype .'*'.string(a:kinds).'*'.a:rx
    " TLogVAR ext, filetype, id
    if has_key(s:rx_cache, id)
        let rv = s:rx_cache[id]
    else
        for kindand in a:kinds
            let rx= a:rx
            for kind in kindand
                let rxf = tlib#var#Get('trag_rxf_'. kind, 'bg')
                " TLogVAR rxf
                if !empty(filetype)
                    let rxf = tlib#var#Get('trag_rxf_'. kind .'_'. filetype, 'bg', rxf)
                endif
                " TLogVAR rxf
                if empty(rxf)
                    if &verbose > 1
                        if empty(filetype)
                            echom 'Unknown kind '. kind .' for unregistered filetype; skip files like '. ext
                        else
                            echom 'Unknown kind '. kind .' for ft='. filetype .'; skip files like '. ext
                        endif
                    endif
                    return ''
                else
                    " TLogVAR rxf
                    " If the expression is no word, ignore word boundaries.
                    if rx =~ '\W$' && rxf =~ '%\@<!%s\\>'
                        let rxf = substitute(rxf, '%\@<!%s\\>', '%s', 'g')
                    endif
                    if rx =~ '^\W' && rxf =~ '\\<%s'
                        let rxf = substitute(rxf, '\\<%s', '%s', 'g')
                    endif
                    " TLogVAR rxf, rx
                    let rx = tlib#string#Printf1(rxf, rx)
                endif
            endfor
            call add(rxacc, rx)
        endfor
        let rv = s:Rx(rxacc, a:default)
        let s:rx_cache[id] = rv
    endif
    " TLogVAR rv
    return rv
endf


function! s:Rx(rxacc, default) "{{{3
    if empty(a:rxacc)
        let rx = a:default
    elseif len(a:rxacc) == 1
        let rx = a:rxacc[0]
    else
        let rx = '\('. join(a:rxacc, '\|') .'\)'
    endif
    return rx
endf


function! s:GetFilename(qfe) "{{{3
    let filename = get(a:qfe, 'filename')
    if empty(filename)
        let filename = bufname(get(a:qfe, 'bufnr'))
    endif
    return filename
endf

function! s:FormatQFLE(qfe) "{{{3
    let filename = s:GetFilename(a:qfe)
    if get(s:world, 'trag_short_filename', '')
        let filename = pathshorten(filename)
    endif
    " let err = get(v:val, "type") . get(v:val, "nr")
    " return printf("%20s|%d|%s: %s", filename, v:val.lnum, err, get(v:val, "text"))
    return printf("%s|%d| %s", filename, a:qfe.lnum, get(a:qfe, "text"))
endf


" :display: trag#QuickList(?world={}, ?suspended=0)
" Display the |quickfix| list with |tlib#input#ListW()|.
function! trag#QuickList(...) "{{{3
    TVarArg ['world', {}], ['suspended', 0]
    call trag#BrowseList(world, getqflist(), 0, suspended)
endf


function! trag#QuickListMaybe(anyway) "{{{3
    call trag#BrowseList({}, getqflist(), a:anyway)
endf


function! trag#BrowseList(world_dict, list, ...) "{{{3
    TVarArg ['anyway', 0], ['suspended', 0]
    " TVarArg ['sign', 'TRag']
    " if !empty(sign) && !empty(g:trag_sign)
    "     " call tlib#signs#ClearAll(sign)
    "     " call tlib#signs#Mark(sign, getqflist())
    " endif
    if !anyway && empty(filter(copy(a:list), 'v:val.nr != -1'))
        return
    endif
    let s:world = copy(g:trag_qfl_world)
    if !empty(a:world_dict)
        call extend(s:world, a:world_dict)
    endif
    let s:world = tlib#World#New(s:world)
    let s:world.qfl  = copy(a:list)
    " TLogVAR s:world.qfl
    call s:FormatBase(s:world)
    " TLogVAR s:world.base
    call tlib#input#ListW(s:world, suspended ? 'hibernate' : '')
endf


" Display the |location-list| with |tlib#input#ListW()|.
function! trag#LocList(...) "{{{3
    " TVarArg ['sign', 'TRag']
    " if !empty(sign) && !empty(g:trag_sign)
    "     " call tlib#signs#ClearAll(sign)
    "     " call tlib#signs#Mark(sign, getqflist())
    " endif
    call trag#BrowseList({}, getqflist())
endf


function! s:FormatBase(world) "{{{3
    let a:world.base = map(copy(a:world.qfl), 's:FormatQFLE(v:val)')
    unlet! g:trag_short_filename
endf

function! trag#AgentEditQFE(world, selected, ...) "{{{3
    TVarArg ['cmd_edit', 'edit'], ['cmd_buffer', 'buffer']
    " TLogVAR a:selected
    if empty(a:selected)
        call a:world.RestoreOrigin()
        " call a:world.ResetSelected()
    else
        call a:world.RestoreOrigin()
        for idx in a:selected
            let idx -= 1
            " TLogVAR idx
            if idx >= 0
                " TLogVAR a:world.qfl
                " call tlog#Debug(string(map(copy(a:world.qfl), 's:GetFilename(v:val)')))
                " call tlog#Debug(string(map(copy(a:world.qfl), 'v:val.bufnr')))
                " TLogVAR idx, a:world.qfl[idx]
                let qfe = a:world.qfl[idx]
                " let back = a:world.SwitchWindow('win')
                " TLogVAR cmd_edit, cmd_buffer, qfe
                let fn = s:GetFilename(qfe)
                " TLogVAR cmd_edit, cmd_buffer, fn
                call tlib#file#With(cmd_edit, cmd_buffer, [fn], a:world)
                " TLogDBG bufname('%')
                call tlib#buffer#ViewLine(qfe.lnum)
                " call a:world.SetOrigin()
                " exec back
            endif
        endfor
    endif
    return a:world
endf 


function! trag#AgentPreviewQFE(world, selected) "{{{3
    " TLogVAR a:selected
    let back = a:world.SwitchWindow('win')
    call trag#AgentEditQFE(a:world, a:selected[0:0])
    exec back
    redraw
    let a:world.state = 'redisplay'
    return a:world
endf


function! trag#AgentGotoQFE(world, selected) "{{{3
    if !empty(a:selected)
        if a:world.win_wnr != winnr()
            let world = tlib#agent#Suspend(a:world, a:selected)
            exec a:world.win_wnr .'wincmd w'
        endif
        call trag#AgentEditQFE(a:world, a:selected[0:0])
    endif
    return a:world
endf


function! trag#AgentWithSelected(world, selected) "{{{3
    let cmd = input('Ex command: ', '', 'command')
    if !empty(cmd)
        call trag#RunCmdOnSelected(a:world, a:selected, cmd)
    else
        let a:world.state = 'redisplay'
    endif
    return a:world
endf


function! trag#RunCmdOnSelected(world, selected, cmd) "{{{3
    call a:world.CloseScratch()
    " TLogVAR a:cmd
    for entry in a:selected
        " TLogVAR entry, a:world.GetBaseItem(entry)
        call trag#AgentEditQFE(a:world, [entry])
        " TLogDBG bufname('%')
        exec a:cmd
        " let item = a:world.qfl[a:world.GetBaseIdx(entry - 1)]
        " <+TODO+>
        let item = a:world.qfl[entry - 1]
        " TLogVAR entry, item, getline('.')
        let item['text'] = tlib#string#Strip(getline('.'))
    endfor
    call s:FormatBase(a:world)
    call a:world.RestoreOrigin()
    let a:world.state = 'reset'
    return a:world
endf


function! trag#AgentSplitBuffer(world, selected) "{{{3
    call a:world.CloseScratch()
    return trag#AgentEditQFE(a:world, a:selected, 'split', 'sbuffer')
endf


function! trag#AgentTabBuffer(world, selected) "{{{3
    call a:world.CloseScratch()
    return trag#AgentEditQFE(a:world, a:selected, 'tabedit', 'tab sbuffer')
endf


function! trag#AgentVSplitBuffer(world, selected) "{{{3
    call a:world.CloseScratch()
    return trag#AgentEditQFE(a:world, a:selected, 'vertical split', 'vertical sbuffer')
endf


" function! trag#AgentOpenBuffer(world, selected) "{{{3
" endf


" Invoke an refactor command.
" Currently only one command is supported: rename
function! trag#AgentRefactor(world, selected) "{{{3
    call a:world.CloseScratch()
    let cmds = ['Rename']
    let cmd = tlib#input#List('s', 'Select command', cmds)
    if !empty(cmd)
        return trag#Refactor{cmd}(a:world, a:selected)
    endif
    let a:world.state = 'reset'
    return a:world
endf


function! trag#CWord() "{{{3
    if has_key(g:trag_keyword_chars, &filetype)
        let rx = '['. g:trag_keyword_chars[&filetype] .']\+'
        let line = getline('.')
        let col  = col('.')
        if col == 1
            let pre = ''
        else
            let pre = matchstr(line[0 : col - 2],  rx.'$')
        endif
        let post = matchstr(line[col - 1 : -1], '^'.rx)
        let word = pre . post
        " TLogVAR word, pre, post, line, col
    else
        let word = expand('<cword>')
        " TLogVAR word
    endif
    return word
endf


function! trag#RefactorRename(world, selected) "{{{3
    " TLogVAR a:selected
    let from = input('Rename ', s:grep_rx)
    if !empty(from)
        let to = input('Rename '. from .' to: ', from)
        if !empty(to)
            let ft = a:world.filetype
            let fn = 'trag#'. ft .'#Rename'
            " TLogVAR ft, exists('*'. fn)
            try
                return call(fn, [a:world, a:selected, from, to])
            catch /^Vim\%((\a\+)\)\=:E117/
                " TLogDBG "General"
                return trag#general#Rename(a:world, a:selected, from, to)
            endtry
        endif
    endif
    let a:world.state = 'reset'
    return a:world
endf


function! trag#SetFollowCursor(world, selected) "{{{3
    if empty(a:world.follow_cursor)
        let a:world.follow_cursor = 'trag#AgentPreviewQFE'
    else
        let a:world.follow_cursor = ''
    endif
    let a:world.state = 'redisplay'
    return a:world
endf


plugin/trag.vim	[[[1
239
" trag.vim -- Jump to a file registered in your tags
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-09-29.
" @Last Change: 2010-03-30.
" @Revision:    606
" GetLatestVimScripts: 2033 1 trag.vim

if &cp || exists("loaded_trag")
    finish
endif
if !exists('g:loaded_tlib') || g:loaded_tlib < 37
    runtime plugin/02tlib.vim
    if !exists('g:loaded_tlib') || g:loaded_tlib < 37
        echoerr 'tlib >= 0.37 is required'
        finish
    endif
endif
let loaded_trag = 8

let s:save_cpo = &cpo
set cpo&vim


" :nodoc:
TLet g:trag_kinds = {}
" :nodoc:
TLet g:trag_filenames = {}
" :nodoc:
TLet g:trag_keyword_chars = {}


" :display: :TRagDefKind KIND FILETYPE /REGEXP_FORMAT/
" The regexp argument is no real regexp but a format string. % thus have 
" to be escaped with % (see |printf()| for details). The REGEXP_FORMAT 
" should contain at least one %s.
" Examples: >
"   TRagDefKind v * /\C\<%s\>\s*=[^=~<>]/
"   TRagDefKind v ruby /\C\<%s\>\(\s*,\s*[[:alnum:]_@$]\+\s*\)*\s*=[^=~<>]/
command! -nargs=1 TRagDefKind call trag#TRagDefKind(<q-args>)


" :display: TRagKeyword FILETYPE KEYWORD_CHARS
" Override 'iskeyword' for a certain filetype. See also |trag#CWord()|.
command! -nargs=+ TRagKeyword if len([<f-args>]) == 2
            \ | let g:trag_keyword_chars[[<f-args>][0]] = [<f-args>][1]
            \ | else
                \ | echoerr 'Expected "FILETYPE KEYWORDRX", but got: <q-args>'
                \ | endif


" :display: TRagDefFiletype FILETYPE EXTENSION ... FILENAME ...
" In order to recognize files based on their extension, you have to 
" declare filetypes first.
" If a file has no extension, the whole filename is used.
" On systems where the case of the filename doesn't matter (check :echo 
" has('fname_case')), EXTENSION should be defined in lower case letters.
" Examples: >
"   TRagDefFiletype html html htm xhtml
command! -nargs=+ TRagDefFiletype for e in [<f-args>][1:-1] | let g:trag_filenames[e] = [<f-args>][0] | endfor


" :display: :TRag[!] KIND [REGEXP]
" Run |:TRagsearch| and instantly display the result with |:TRagcw|.
" See |trag#Grep()| for help on the arguments.
" If the kind rx doesn't contain %s (e.g. todo), you can skip the 
" regexp.
"
" Examples: >
"     " Find any matches
"     TRag . foo
" 
"     " Find variable definitions (word on the left-hand): foo = 1
"     TRag l foo
" 
"     " Find variable __or__ function/method definitions
"     TRag d,l foo
" 
"     " Find function calls like: foo(a, b)
"     TRag f foo
"
"     " Find TODO markers
"     TRag todo
command! -nargs=1 -bang -bar TRag TRagsearch<bang> <args> | TRagcw
command! -nargs=1 -bang -bar Trag TRag<bang> <args>


" :display: :TRagfile
" Edit a file registered in your tag files.
command! TRagfile call trag#Edit()
command! Tragfile call trag#Edit()


" :display: :TRagcw
" Display a quick fix list using |tlib#input#ListD()|.
command! -bang -nargs=? TRagcw call trag#QuickListMaybe(!empty("<bang>"))
command! -bang -nargs=? Tragcw call trag#QuickListMaybe(!empty("<bang>"))

" :display: :Traglw
" Display a |location-list| using |tlib#input#ListD()|.
command! -nargs=? Traglw call trag#LocList()


" :display: :TRagsearch[!] KIND REGEXP
" Scan the files registered in your tag files for REGEXP. Generate a 
" quickfix list. With [!], append to the given list. The quickfix list 
" can be viewed with commands like |:cw| or |:TRagcw|.
"
" The REGEXP has to match a single line. This uses |readfile()| and the 
" scans the lines. This is an alternative to |:vimgrep|.
" If you choose your identifiers wisely, this should guide you well 
" through your sources.
" See |trag#Grep()| for help on the arguments.
command! -nargs=1 -bang -bar TRagsearch call trag#Grep(<q-args>, empty("<bang>"))
command! -nargs=1 -bang -bar Tragsearch TRagsearch<bang> <args>


" :display: :TRaggrep REGEXP [GLOBPATTERN]
" A 99%-replacement for grep. The glob pattern is optional.
"
" Example: >
"   :TRaggrep foo *.vim
"   :TRaggrep bar
command! -nargs=+ -bang -bar -complete=file TRaggrep
            \ let g:trag_grepargs = ['.', <f-args>]
            \ | call trag#Grep(g:trag_grepargs[0] .' '. g:trag_grepargs[1], empty("<bang>"), g:trag_grepargs[2:-1])
            \ | unlet g:trag_grepargs
            \ | TRagcw
command! -nargs=+ -bang -bar -complete=file Traggrep TRaggrep<bang> <args>


" :display: :TRagsetfiles [FILELIST]
" The file list is set only once per buffer. If the list of the project 
" files has changed, you have to run this command on order to reset the 
" per-buffer list.
"
" If no filelist is given, collect the files in your tags files.
"
" Examples: >
"   :TRagsetfiles
"   :TRagsetfiles split(glob('foo*.txt'), '\n')
command! -nargs=? -bar -complete=file TRagsetfiles call trag#SetFiles(<args>)

" :display: :TRagaddfiles FILELIST
" Add more files to the project list.
command! -nargs=1 -bar -complete=file TRagaddfiles call trag#AddFiles(<args>)

" :display: :TRagclearfiles
" Remove any files from the project list.
command! TRagclearfiles call trag#ClearFiles()

" :display: :TRagGitFiles GIT_REPOS
command! -nargs=1 -bar -complete=dir TRagGitFiles call trag#SetGitFiles(<q-args>)


let &cpo = s:save_cpo
unlet s:save_cpo


finish
CHANGES:
0.1
- Initial release

0.2
- Quite a few things have changed and I haven't had the time yet to test 
these changes thorougly. There is a chance that nested patterns thus 
don't work as described (please report).
- Enable search for more than one kinds at once (as comma-separated 
list)
- Enabled <c-e>: Run ex-command on selected lines (e.g. for refactoring 
purposes)
- Enabled <c-s>, <c-v>, <c-t>: Open selected lines in (vertically) split 
windows or tabs.
- Renamed vV kinds to lL (~ let)
- New kind: r/R (right hand side arguemnt of an assignment/let, i.e. 
value)
- New kind: fuzzy (typo-tolerant search)
- INCOMPATIBLE CHANGE: Renamed "mode" to "kind"
- TRag now has some idea of negation. E.g., "TRag !i,w call" will search 
for the word "call" but ignore matches in comments (if defined for the 
    current filetype)
- Alternative methods to define project files: g:trag_files, 
g:trag_glob, g:trag_project.
- Improved support for ruby, vim
- TRagKeyword, trag#CWord(): Customize keyword rx.
- g:trag_get_files
- [bg]:trag_project_{&filetype}: Name of the filetype-specific project 
files catalog (overrides [bg]:trag_project if defined)
- trag#Edit() will now initally select files with the same "basename 
root" (^\w\+) as the current buffer (the command is thus slightly more 
useful and can be used as an ad-hoc alternative file switcher)
- FIX: Match a line only once
- FIX: Caching of regexps

0.3
- Use vimgrep with set ei=all as default search mode (can be configured 
via g:trag_search_mode); by default trag now is a wrapper around vimgrep 
that does the handling of project-related file-sets and regexp builing 
for you.
- FIX: ruby/f regexp

0.4
- trag_proj* variables were renamed to trag_project*.
- Traggrep: Arguments have changed for conformity with grep commands (an 
implicit .-argument is prepended)
- Make sure tlib is loaded even if it is installed in a different 
rtp-directory.
- Post-process lines (strip whitespace) collected by vimgrep
- tlib#Edit(): for list input, set pick_last_item=0, show_empty=1
- Aliases for some commands: Trag, Traggrep ...

0.5
- Update the qfl when running a command on selected lines
- Enable certain operations for multiple choices
- Java, Ruby: x ... find subclasses (extends/implements)
- Experimental rename command for refactoring (general, java)
- NEW: [bg]:trag_get_files_{&filetype}
- Traggrep: If the second argument (glob pattern) is missing, the 
default file list will be used.

0.6
- trag#viki#Rename()
- Generalized trag#rename#Rename()
- Enabled "trace cursor" functionality (mapped to the <c-insert> key).
- :Traglw
- TRagGitFiles, trag#SetGitFiles(), g:trag_git

0.7
- trag#QuickList(): Accept a dict as optional argument.
- trag#Grep(): rx defaults to '\.{-}'
- trag#Grep(): use :g (instead of search()) for non-vimgrep mode

0.8
- Moved the definition of some variables from plugin/trag.vim to autoload/trag.vim
- :TRagcw! (show :cw even if there are no recognized errors)
- Require tlib 0.37

doc/trag.txt	[[[1
522
*trag.txt*          A slightly language-aware alternative to grep
                    Author: Tom Link, micathom at gmail com

You can think of this plugin either as some kind of ad-hoc 
tags/cescope-like solution or as a project-oriented and slightly 
programming-language-aware alternative to grep that should support poor 
dynamic programmers with refactoring. Like grep it scans files only as 
needed and doesn't build a database, which is ok for smaller projects 
and fast hard disks. It's more flexible and easier to configure (vim 
only, no external program required) than tags/cescope. Apart from the 
basic grep-like functionality, it can be used to find locations where a 
variable is defined, where it is assigned to another variable, where a 
method is called and stuff like that. At least that's the idea. The 
plugin hasn't reached the point yet to be 100% useful.


Usage~

First, define which files belong to your project. Most likely your 
project's source files are already registered in your tags files, which 
will be used by default. So, if you use tags, there is nothing more you 
need to do. Alternatively you can set the variable [wbg]:trag_files to a 
list of files that will be used instead. (Setting this variable will 
prevent the use of tags.)

Secondly, perform certain operations on these files, like:

    - Select a file to edit (using :TRagfile)
    - Scan files for an expression (using :TRag, :TRagsearch, or 
      :TRaggrep): You can restrict searches to certain "kinds" like only 
      variable definitions or only function calls.


-----------------------------------------------------------------------
Install~

Edit the vba file and type: >

    :so %

See :help vimball for details. If you have difficulties or use vim 7.0, 
please make sure, you have the current version of vimball (vimscript 
#1502) installed.

This script requires tlib (vimscript #1863) to be installed.

Suggested maps (best set in ~/vimfiles/after/plugin/trag.vim): >

    noremap <Leader>r. :TRag * 
    noremap <Leader>r- :TRagfile<cr>
    noremap <silent> <Leader>r# :TRag #w <c-r>=trag#CWord()<cr><cr>
    for c in keys(g:trag_kinds)
        if len(c) == 1
            exec "nnoremap <Leader>r". c .' :TRag #'. c .' <c-r>=trag#CWord()<cr><cr>'
            exec "vnoremap <Leader>r". c .' y<esc>:TRag #'. c .' <c-r>"<cr>'
        endif
    endfor


-----------------------------------------------------------------------
                                                    *trag-kinds*
Kinds~

Certain commands take a "kind" as argument. A "kind" in the context of 
trag means a way how the expression is interpreted and (in the future) 
how the lines are scanned. The meaning of the letter is specific for a 
certain filetype. In general, the following list should serve as a 
common base:

    c ... class definitions
    d ... function/method definitions
    f ... function calls
    i ... ignored lines, comments etc.
    l ... variable definition
    m ... module definitions
    r ... variable assignment (e.g. "= WORD")
    w ... find as word
    x ... subclasses

    fuzzy ... use a typo-tolerant regexp
    todo  ... TODO markers etc.

Modifiers:
    # ... escape a literal expression as regexp (place it somewhere in the 
          kinds argument)

Negation: -, ! ... negate/skip uninteresting lines.

Positive (must match) and negative (must not match) patterns are joined 
with "-" or "!". Positive and negative conditions must both be 
satisfied. The positive regexp determines the speed of the operation. 
I.e. trag first searches for matching lines and only then decides 
whether to skip "bad" ones.

    POSITIVE1,POSITIVE2a.POSITIVE2b...-NEGATIVE...

Note: If you use negations, you should also give a positive kind (use . 
as fallback). The use of negations causes trag to use it's own grep 
procedure which is slightly slower than the standard vimgrep command.

EXPERIMENTAL: Kinds can be joined as comma-separated list (OR) of 
period-seprated list (CONTAIN). This may be unintuitive but that's the 
way it is. Order in CONTAIN-patterns matters, i.e. w.i doesn't make too 
much sense (a word that CONTAINs a comment, which could only be the word 
itself) but i.w does (a comment that CONTAINs a word, which is quite 
possible).

The way this has done has changed from version 0.1 to 0.2 with the 
effect that the current solution is less well tested. If you stumble 
over wrong search result, please tell me about the problem. Most likely 
it's a problem with the regexp not being properly updated.

Example:
    i   ... comment/ignored text that CONTAINs a word
    i.w ... comment/ignored text that CONTAINs a word
    l,r ... a word either on the left OR right hand side of an 
            assignment
    w-i ... word that is NOT contained in a comment
    w-i,l,r ... word but NOT in assignments (left OR right hand) OR in 
            comments

More examples:
    File contents: >
        #1 function Foo() {}
        #2 function FooBar() {}
        #3 let x = FooBar()
        #4 let x = Foo()
<
    :TRag * Foo
    => Find "Foo": 1-4

    :TRag w Foo
    => Find the __w__ords "Foo": 1, 4

    :TRag d Foo
    => Find __d__efinitions of "Foo": 1, 2

    :TRag d.w Foo
    => Find __d__efinitions of the __w__ord "Foo": 1

    :TRag f Foo
    => Find (__f__unction) calls of "Foo": 3, 4

    :TRag f.w Foo
    => Find (__f__unction) calls of the __w__ord "Foo": 4


Not every kind is defined for every filetype. Currenty, support for 
the following filetype(s) is somewhat above average:
    - ruby
    - vim

Note: I'd appreciate if you sent me sophisticated kind definitions for 
more filetypes so that I can include them in future releases.

A kind is best defined using the |:TRagDefKind| command.
Take a look at ../plugin/trag.vim to find out more.

If you want to run a command without kinds, use "*" or "." as 
argument, whatever if more convenient to type on your keyboard.

See |trag#Grep()| for details of how to use this.


========================================================================
Contents~

        :TRagDefKind ............. |:TRagDefKind|
        :TRagKeyword ............. |:TRagKeyword|
        :TRagDefFiletype ......... |:TRagDefFiletype|
        :TRag .................... |:TRag|
        :Trag .................... |:Trag|
        :TRagfile ................ |:TRagfile|
        :Tragfile ................ |:Tragfile|
        :TRagcw .................. |:TRagcw|
        :Tragcw .................. |:Tragcw|
        :Traglw .................. |:Traglw|
        :TRagsearch .............. |:TRagsearch|
        :Tragsearch .............. |:Tragsearch|
        :TRaggrep ................ |:TRaggrep|
        :Traggrep ................ |:Traggrep|
        :TRagsetfiles ............ |:TRagsetfiles|
        :TRagaddfiles ............ |:TRagaddfiles|
        :TRagclearfiles .......... |:TRagclearfiles|
        :TRagGitFiles ............ |:TRagGitFiles|
        g:trag_search_mode ....... |g:trag_search_mode|
        g:trag_get_files ......... |g:trag_get_files|
        g:trag_get_files_java .... |g:trag_get_files_java|
        g:trag_get_files_c ....... |g:trag_get_files_c|
        g:trag_get_files_cpp ..... |g:trag_get_files_cpp|
        g:trag_files ............. |g:trag_files|
        g:trag_glob .............. |g:trag_glob|
        g:trag_project_ruby ...... |g:trag_project_ruby|
        g:trag_project ........... |g:trag_project|
        g:trag_git ............... |g:trag_git|
        trag#SetSyntax ........... |trag#SetSyntax()|
        trag#ClearFiles .......... |trag#ClearFiles()|
        trag#AddFiles ............ |trag#AddFiles()|
        trag#GetProjectFiles ..... |trag#GetProjectFiles()|
        trag#GetGitFiles ......... |trag#GetGitFiles()|
        trag#SetGitFiles ......... |trag#SetGitFiles()|
        trag#SetFiles ............ |trag#SetFiles()|
        trag#FindGitRepos ........ |trag#FindGitRepos()|
        trag#Edit ................ |trag#Edit()|
        trag#Grep ................ |trag#Grep()|
        trag#ClearCachedRx ....... |trag#ClearCachedRx()|
        trag#QuickList ........... |trag#QuickList()|
        trag#QuickListMaybe ...... |trag#QuickListMaybe()|
        trag#BrowseList .......... |trag#BrowseList()|
        trag#LocList ............. |trag#LocList()|
        trag#AgentEditQFE ........ |trag#AgentEditQFE()|
        trag#AgentPreviewQFE ..... |trag#AgentPreviewQFE()|
        trag#AgentGotoQFE ........ |trag#AgentGotoQFE()|
        trag#AgentWithSelected ... |trag#AgentWithSelected()|
        trag#RunCmdOnSelected .... |trag#RunCmdOnSelected()|
        trag#AgentSplitBuffer .... |trag#AgentSplitBuffer()|
        trag#AgentTabBuffer ...... |trag#AgentTabBuffer()|
        trag#AgentVSplitBuffer ... |trag#AgentVSplitBuffer()|
        trag#AgentRefactor ....... |trag#AgentRefactor()|
        trag#CWord ............... |trag#CWord()|
        trag#RefactorRename ...... |trag#RefactorRename()|
        trag#SetFollowCursor ..... |trag#SetFollowCursor()|


========================================================================
plugin/trag.vim~

                                                    *:TRagDefKind*
:TRagDefKind KIND FILETYPE /REGEXP_FORMAT/
    The regexp argument is no real regexp but a format string. % thus have 
    to be escaped with % (see |printf()| for details). The REGEXP_FORMAT 
    should contain at least one %s.
    Examples: >
      TRagDefKind v * /\C\<%s\>\s*=[^=~<>]/
      TRagDefKind v ruby /\C\<%s\>\(\s*,\s*[[:alnum:]_@$]\+\s*\)*\s*=[^=~<>]/
<

                                                    *:TRagKeyword*
TRagKeyword FILETYPE KEYWORD_CHARS
    Override 'iskeyword' for a certain filetype. See also |trag#CWord()|.

                                                    *:TRagDefFiletype*
TRagDefFiletype FILETYPE EXTENSION ... FILENAME ...
    In order to recognize files based on their extension, you have to 
    declare filetypes first.
    If a file has no extension, the whole filename is used.
    On systems where the case of the filename doesn't matter (check :echo 
    has('fname_case')), EXTENSION should be defined in lower case letters.
    Examples: >
      TRagDefFiletype html html htm xhtml
<

                                                    *:TRag*
:TRag[!] KIND [REGEXP]
    Run |:TRagsearch| and instantly display the result with |:TRagcw|.
    See |trag#Grep()| for help on the arguments.
    If the kind rx doesn't contain %s (e.g. todo), you can skip the 
    regexp.
    
    Examples: >
        " Find any matches
        TRag . foo
    
        " Find variable definitions (word on the left-hand): foo = 1
        TRag l foo
    
        " Find variable __or__ function/method definitions
        TRag d,l foo
    
        " Find function calls like: foo(a, b)
        TRag f foo
    
        " Find TODO markers
        TRag todo
<

                                                    *:Trag*
:Trag

                                                    *:TRagfile*
:TRagfile
    Edit a file registered in your tag files.

                                                    *:Tragfile*
:Tragfile

                                                    *:TRagcw*
:TRagcw
    Display a quick fix list using |tlib#input#ListD()|.

                                                    *:Tragcw*
:Tragcw

                                                    *:Traglw*
:Traglw
    Display a |location-list| using |tlib#input#ListD()|.

                                                    *:TRagsearch*
:TRagsearch[!] KIND REGEXP
    Scan the files registered in your tag files for REGEXP. Generate a 
    quickfix list. With [!], append to the given list. The quickfix list 
    can be viewed with commands like |:cw| or |:TRagcw|.
    
    The REGEXP has to match a single line. This uses |readfile()| and the 
    scans the lines. This is an alternative to |:vimgrep|.
    If you choose your identifiers wisely, this should guide you well 
    through your sources.
    See |trag#Grep()| for help on the arguments.

                                                    *:Tragsearch*
:Tragsearch

                                                    *:TRaggrep*
:TRaggrep REGEXP [GLOBPATTERN]
    A 99%-replacement for grep. The glob pattern is optional.
    
    Example: >
      :TRaggrep foo *.vim
      :TRaggrep bar
<

                                                    *:Traggrep*
:Traggrep

                                                    *:TRagsetfiles*
:TRagsetfiles [FILELIST]
    The file list is set only once per buffer. If the list of the project 
    files has changed, you have to run this command on order to reset the 
    per-buffer list.
    
    If no filelist is given, collect the files in your tags files.
    
    Examples: >
      :TRagsetfiles
      :TRagsetfiles split(glob('foo*.txt'), '\n')
<

                                                    *:TRagaddfiles*
:TRagaddfiles FILELIST
    Add more files to the project list.

                                                    *:TRagclearfiles*
:TRagclearfiles
    Remove any files from the project list.

                                                    *:TRagGitFiles*
:TRagGitFiles GIT_REPOS


========================================================================
autoload/trag.vim~

                                                    *g:trag_search_mode*
g:trag_search_mode             (default: 0)
    0 ... use the built-in search.
    1 ... use vimgrep.
    2 ... use vimgrep but set 'ei' to all; this means special file 
          encodings won't be detected
    Please not, this is only in effect with simple searches (as for 0.3 
    all searches are simple). For more complex multi-line patterns, the 
    built-in search will be used (some day in the future).

                                                    *g:trag_get_files*
g:trag_get_files               (default: 'split(glob("*"), "\n")')
    If no project files are defined, evaluate this expression as 
    fallback-strategy.

                                                    *g:trag_get_files_java*
g:trag_get_files_java          (default: 'split(glob("**/*.java"), "\n")')

                                                    *g:trag_get_files_c*
g:trag_get_files_c             (default: 'split(glob("**/*.[ch]"), "\n")')

                                                    *g:trag_get_files_cpp*
g:trag_get_files_cpp           (default: 'split(glob("**/*.[ch]"), "\n")')


The following variables provide alternatives to collecting 
your project's file list on the basis of you tags files.

These variables are tested in the order as listed here. If the value 
of a variable is non-empty, this one will be used instead of the other 
methods.

The tags file is used as a last ressort.

                                                    *g:trag_files*
g:trag_files                   (default: [])
    1. A list of files. Can be buffer local.

                                                    *g:trag_glob*
g:trag_glob                    (default: '')
    2. A glob pattern -- this should be an absolute path and may contain ** 
    (see |glob()| and |wildcards|). Can be buffer local.

                                                    *g:trag_project_ruby*
g:trag_project_ruby            (default: 'Manifest.txt')
    3. Filetype-specific project files.

                                                    *g:trag_project*
g:trag_project                 (default: '')
    4. The name of a file containing the projects file list. This file could be 
    generated via make. Can be buffer local.

                                                    *g:trag_git*
g:trag_git                     (default: '')
    5. The name of a git repository that includes all files of interest. 
    If the value is "*", trag will search from the current directory 
    (|getcwd()|) upwards for a .git directory.
    If the value is "finddir", use |finddir()| to find a .git directory.
    Can be buffer local.

                                                    *trag#SetSyntax()*
trag#SetSyntax()

                                                    *trag#ClearFiles()*
trag#ClearFiles()

                                                    *trag#AddFiles()*
trag#AddFiles(files)

                                                    *trag#GetProjectFiles()*
trag#GetProjectFiles(manifest)

                                                    *trag#GetGitFiles()*
trag#GetGitFiles(repos)

                                                    *trag#SetGitFiles()*
trag#SetGitFiles(repos)
    Set the files list from the files included in a given git repository.

                                                    *trag#SetFiles()*
trag#SetFiles(?files=[])

                                                    *trag#FindGitRepos()*
trag#FindGitRepos()

                                                    *trag#Edit()*
trag#Edit()
    Edit a file from the project catalog. See |g:trag_project| and 
    |:TRagfile|.

                                                    *trag#Grep()*
trag#Grep(args, ?replace=1, ?files=[])
    args: A string with the format:
      KIND REGEXP
      KIND1,KIND2 REGEXP
    
    If the variables [bg]:trag_rxf_{kind}_{&filetype} or 
    [bg]:trag_rxf_{kind} exist, these will be taken as format string (see 
    |printf()|) to format REGEXP.
    
    EXAMPLE:
    trag#Grep('v foo') will find by default take g:trag_rxf_v and find 
    lines that looks like "\<foo\>\s*=[^=~]", which most likely is a 
    variable definition in many programming languages. I.e. it will find 
    lines like: >
      foo = 1
<   but not: >
      def foo(bar)
      call foo(bar)
      if foo == 1
<

                                                    *trag#ClearCachedRx()*
trag#ClearCachedRx()

                                                    *trag#QuickList()*
trag#QuickList(?world={}, ?suspended=0)
    Display the |quickfix| list with |tlib#input#ListW()|.

                                                    *trag#QuickListMaybe()*
trag#QuickListMaybe(anyway)

                                                    *trag#BrowseList()*
trag#BrowseList(world_dict, list, ...)

                                                    *trag#LocList()*
trag#LocList(...)
    Display the |location-list| with |tlib#input#ListW()|.

                                                    *trag#AgentEditQFE()*
trag#AgentEditQFE(world, selected, ...)

                                                    *trag#AgentPreviewQFE()*
trag#AgentPreviewQFE(world, selected)

                                                    *trag#AgentGotoQFE()*
trag#AgentGotoQFE(world, selected)

                                                    *trag#AgentWithSelected()*
trag#AgentWithSelected(world, selected)

                                                    *trag#RunCmdOnSelected()*
trag#RunCmdOnSelected(world, selected, cmd)

                                                    *trag#AgentSplitBuffer()*
trag#AgentSplitBuffer(world, selected)

                                                    *trag#AgentTabBuffer()*
trag#AgentTabBuffer(world, selected)

                                                    *trag#AgentVSplitBuffer()*
trag#AgentVSplitBuffer(world, selected)

                                                    *trag#AgentRefactor()*
trag#AgentRefactor(world, selected)
    Invoke an refactor command.
    Currently only one command is supported: rename

                                                    *trag#CWord()*
trag#CWord()

                                                    *trag#RefactorRename()*
trag#RefactorRename(world, selected)

                                                    *trag#SetFollowCursor()*
trag#SetFollowCursor(world, selected)



vim:tw=78:fo=tcq2:isk=!-~,^*,^|,^":ts=8:ft=help:norl:
