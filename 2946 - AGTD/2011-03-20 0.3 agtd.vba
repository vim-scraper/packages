" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/agtd.txt	[[[1
141
*agtd.txt* Plugin for AGTD files
*agtd* *agtd-plugin* 
Version AGTD v0.1 ALPHA, for Vim version 7, 2010-01-19


                      Almighty GTD Vim Plugin
                          User Manual

                    By Francisco Garcia Rodriguez
                  <public@francisco-garcia.net>

        Contents:

                1. Intro........................|agtd-intro|
                2. Getting started..............|agtd-getstarted|
                3. Todo.........................|agtd-todo|

Please, be aware that this is a highly young experimental project that I write
in my free time. I might reconsider some concepts, I might change the notation
and many things might change. You must be prepared to notice some glitches
until this goes more stable. I suppose future updates might not be compatible
until I feel I got it right. See|agtd-todo|for things that I still have
planned


==============================================================================
1. Intro                                                *agtd-intro*

Tired of trying thousand of tools to organize my life, and inspired in the
idea of ONE BIG TEXT FILE from Danny O'Brien, I decided that the world needed
yet another GTD tool. The "Almighty" part is a reference to the idea of having
all type of notes about your tasks in one single file.

The whole idea of this GTD implementation relies in some custom notation
marks, the|Folding|capabilities of VIM and its power to work quickly with
text. I wanted something portable to collect, edit, search and structure
lightning fast my notes. This is the result so far:

    * Organize project tree with notes, links and tasks
    * Move tasks from the project tree to the TODO list inserting project label
    * Neatly collect and display all tasks related with a project
    * Ease project tasks/notes finding
    * Build a calendar with all the dates in the text file

I strongly recommend the UTL script as a side companion. It will enhance your
notes allowing you to open different links with just a short keystroke.

This script is my idea of how GTD should be done with VIM. Any comments, bug
reports, patches, fixes and suggestions are welcome.

If you like it, please, rate it in the Vim website to let me know that out
there there are people who find this useful. Otherwise I will consider this
just an scrapbook to learn Vim scripting.


=============================================================================
2. Getting started                                      *agtd-getStarted*

You should have some knowledge of GTD in order to make the best use of this
script, but it is not a must.

If you follow a simple notation and some basic structure pattern, AGTD will
provide you with some handy tools to organize your tasks along a nice syntax
highlight. I like my notes to be Christmas tree like, so be warned!

Along this script you should have an example file named "sample-todo.txt".
It might help to play with it while reading its comments.

You must define your own main sections for your notes. Nothing is really
mandatory, but this is how mine usually looks like.

# NOW
# LATER

For this most of the content should be simple tasks lines with context,
project and date marks for the current days (NOW) or rest of the week (LATER)

    @net p:finances:phone Check internet trial charges S::02-12

The parts

    @word       The context of a task (work, net, city, phone...)
    p:sub:path  A project reference label in a task
    X::02-25    A date mark. The "X" represent one single uppercase letter to
                add some information about the date. I usually use 'S' for
                start, 'F' for finish and 'W' for the latest date I should
                wait for something 
    <tab>       Any internal indentation bellow a task to add extra notes or
                explanations will be folded.

# PROJECTS

I usually use the PROJECTS section to organize all my projects and type all
steps I need to take. Here I specify the projects names in uppercase, some
comment lines and UTL links to related stuff.

    TITLE       A (sub)project title (4 spaces indentation)
    ;           Comment
    <url:...>   Any link: web site, file, calendar, server...
    m::<char>   Auto-marks. Set a jump mark that you can use as -> '<char>

Since the contents are structured according the project, I don't type the
project label here. When I decide it is time to work on it I use the
command ":GInsert" to move it to my current list of stuff to do. The function
will also insert a project label based on the line position within the tree.

# LISTS

This is just a bunch of lists that I need to review, as shopping, program
checks, books to read...

# CALENDAR

All those day events that do not fit within any specific task

Once you start exploring the example file, you can play with some basic
functions:

:GSearch    Search for a project (auto-completes with existing projects!)
:GCalendar  Display a sorted list of events (tasks + calendar section)
:GInsert    Insert and label line from PROJECTS section
:GGo        Go to the project tree branch where the task was taken


==============================================================================
3. TODO                                                 *agtd-todo*

- Improve this documentation
- Better integration with the UTL script
- Regarding the calendar part
    - Export to iCal file
    - Import iCal files
    - Sync with Google
    - Improve Calendar 
        - month/folding
        - display range
        - search pattern
- Ease encrypted comments with gnupg: Define a mark/zone and decrypt in the
  status line 

vim:tw=78:ts=8:ft=help:norl:fdm=marker:fen
ftplugin/agtd.vim	[[[1
40
" Language: AGTD syntax file
" Author: Francisco Garcia Rodriguez <public@francisco-garcia.net>
"
" Licence: Copyright (C) 2010 Francisco Garcia Rodriguez
" This program is free software: you can redistribute it and/or
" modify it under the terms of the GNU General Public License.
" See http://www.gnu.org/licenses/gpl.html
" 
" This program is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
" GNU General Public License for more details.
"
" History:	
"   See plugin/agtd.vim 
"   

if exists ("b:did_ftplugin_agtd")
   finish
endif

" Don't load another plugin for this buffer
let b:did_ftplugin_agtd = 1

setlocal foldexpr=Agtd_getFoldLevel(v:lnum)
setlocal indentexpr=Agtd_getFoldLevel(v:lnum)*4
setlocal fdm=expr
setlocal fdi=0
setlocal formatoptions=
setlocal foldminlines=1
setlocal nowrap
setlocal smartindent

" Shortcuts
nmap <Leader>i :call Agtd_insertTask()<CR>
nmap <Leader>s :call Agtd_getProject()<CR>

" Set jump marks looking for m::<Letter> labels
exe "call Agtd_setMarks()"

plugin/agtd.vim	[[[1
432
" -----------------------------------------------------------------------------
"
" File: plugin/agtd.vim - Almighty GTD File Vim Script
"
" Author: Francisco Garcia Rodriguez <public@francisco-garcia.net>
"
" Licence: Copyright (C) 2010 Francisco Garcia Rodriguez
" This program is free software: you can redistribute it and/or
" modify it under the terms of the GNU General Public License.
" See http://www.gnu.org/licenses/gpl.html
" 
" This program is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
" GNU General Public License for more details.
" 
" Web: 
"   https://github.com/fgarcia/agtd
"   http://www.vim.org/scripts/script.php?script_id=2946
"
" Files:    
"       doc/agtd.txt
"       plugin/agtd.vim
"       syntax/agtd.vim
"       ftplugin/agtd.vim
"
" Version:  
"       0.3 (Alpha)
"
"       So far I label my releases with 'Alpha' because I might change the
"       context/structure of the TODOs files and break support for previous
"       versions
"
" Bugs:
"       I have developed this script according my environment. I would expect
"       that some behaviour might be different or broken for other users
"       (color schema, tab width...)
"
"       The :GSearch and :GGo commands are have a similar function but with
"       different implementations. I am still exploring the best way to define
"       projects
"
" History:
"   v0.4  ???
"		
"   v0.3  2011-03-09 
"       License text fix
"       Author information updated
"       New folding schema. Now based on sections, not on indentation
"       New help command :AG
"       Command :GSearch displays active tasks before jumping to project tree
"       UTL configuration removed
"       nmap shortcuts now start with <Leader> key
"       Improved syntax for project and comment sections
"       Indentation within tags
"
"   v0.2  2010-01-28 
"       Bug fixing and clutter clean-up
"       Calendar displays colors
"       Variable naming matches better plug-in name
"       Task insertions works when there is an http:// address
"       Common environment values pushed into a ftplugin
"       UTL Schema prototype for ssh links
"
"   v0.1  2010-01-20
"       Initial version
"
" -----------------------------------------------------------------------------

if exists("loaded_agtd") 
    finish
endif
let loaded_agtd = "0.3"

let s:agtd_dateRegx    = '\u::\d\d-\d\d\(-\d\d\)\?'
let s:agtd_ProjectRegx = '^\s\+[A-Z][A-Z0-9_]\+\s*\(--.*\)*$'


" Move task at TOP
"
" Get current task line (usually within a PROJECT section) and move it to the
" top of the buffer, where the current todo tasks should be. It will also try
" to append a project label. As an example, when the cursor is on this line:
"
"       @net Find person
"
" It will be moved to the 2nd line of the buffer as:
" (1st should be the section title)
"
"       @net p:task:subtask Find person
"
function! Agtd_insertTask()
    let task_line_no = line ('.')
    let line         = getline (task_line_no)
    let task         = matchstr (line,' \w.*$')
    let context      = matchstr (line,'@\w\+')
    if context == ""
        let v:errmsg = "Line has not a context label ..@.."
        echohl ErrorMsg | echo v:errmsg | echohl None
        return
    endif

    " Get project label as in p:pro:sub1:sub2
    if match (line,' p:\w*') == -1
        " Line has no label 
        let lastCol = 0
        let project = ""
        let col = 0
        while col != 4
            " Last project name is in column 4

            " Get project name looking backwards
            let pos = search(s:agtd_ProjectRegx, 'b', 1)
            if pos == 0
                let v:errmsg = "Could not build project name"
                echohl ErrorMsg | echo v:errmsg | echohl None
                return
            endif

            " Project name found: go to get line and colum
            call cursor (pos)
            let line = getline (pos)
            let col = match(line,'\u\+')

            if col != lastCol 
                " Project names in the same column are siblings, not an ancestor
                let project = ":" . tolower(matchstr(line,'\u\+')) . project
                let lastCol = col
            endif
        endwhile
        let entry = "    ".context." p".project.task
    else
        let entry = "    ".context." ".task
    endif

    " Insert new project line
    call append(1, entry)
    call cursor (task_line_no + 1,0)
    normal dd
    echo "New task added: " . entry
endfunction


" Go to PROJECT line
"
" Parse the current line to find a p:project label and decompose it to find
" the rest of the tasks related with such project. 
"
" If there are different subprojects (p:xxx:yy:z) the function will actually
" start jumping step by step, deeper into the hierarchy until the end or the
" first failed search
function! Agtd_getProject()
    let line = getline ('.')
    let label = matchstr(line, 'p:\S\+')
    let path = split (label, ':')

    " Find project first line
    for i in path[1:]
        let project = toupper(i)
        call cursor ( search('^\s\+'.project) )
        normal zo
    endfor
    let projectPos = getpos ('.')

    " Get tasks for same project (TODO later)
    normal gg
    let pos = search(label, 'W')
    let listLines = []
    while pos != 0
        let line = getline ('.')
        call add (listLines, line)
        let pos = search(label, 'W')
    endwhile

    " Return to project first line
    call setpos ('.', projectPos)
    normal zt
    normal jzo
endfunction

" Column of the first character for sections
" 
" Sections start with a section marker (# @ ;) or just contain a project title
"
" If neither of them is found, it returns the column of the
" first character. Otherwise returns -1
"
function! Agtd_getSectionColumn(lineNum, markers)
    let lineText = getline (a:lineNum)
    let lineCol = match (lineText,'\S')
    if lineCol == -1
        " Line with no text, no fold level here
        return -1
    endif

    " Check for lines with just a project title (uppercase and '_') optionally
    " followed by a URL or file marker (\w::\w)
    let projectTitle = match (lineText, s:agtd_ProjectRegx)
    if projectTitle == 0
        return lineCol
    endif

    let markerPos = match (lineText,a:markers)
    if markerPos == lineCol
        " Line starting with a section marker
        return markerPos
    else
        " Other type of lines
        return -1
    endif
endfu


" FoldLevel for AGTD files
"
" The fold level is based on the column of the first section marker divided
" by the current tab width. Tab markers are identified by the function
" Agtd_getSectionColumn()
"
" If no section marker is found in the current line, it will look for one in
" the previous lines divided. In such cases it will be assumed that the
" current line is one level deeper (+1) within the first previous section
" line.
"
function! Agtd_getFoldLevel(pos)
    " Get section marker position
    let sectionColumn = -1
    let lineNum = a:pos + 1

	let markers = '[#@;]'
    "let firstChar = strpart (lineText, match (lineText,'\S'), 1)
	"let markers -= substitute (lineText, firstChar, '', 0)
    "let lineWithMarker = match (firstChar,a:markers)
    while sectionColumn == -1 && lineNum != 1
        let lineNum -= 1
        let sectionColumn = Agtd_getSectionColumn(lineNum, markers)
	endwhile

    if sectionColumn == -1
        " No section found
        return 0
    endif

    let level = sectionColumn / &shiftwidth
    if lineNum != a:pos
        " Section had to be searched in previous lines. Therefore Current line
        " is contained within a section: The fold level is +1 greater than its
        " section
        let level += 1
    endif
    return level
endfu


" Search the file for mark tags and set them
"
" Starting from the beggining of the file, search for auto-mark labels and set
" them as a new mark
function! Agtd_setMarks()
    call cursor (1)
    let pos = search('m::\l', 'W')
    while pos != 0
        let line = getline ('.')
        let mark = matchstr (line, 'm::\l')
        let mark = strpart (mark, 3, 1)
        exe "mark " mark

        let pos = search('m::\l', 'W')
    endwhile
    normal gg
endfunction


" Search project
"
" Locate first appearence of project name, jump to it and unfold
function! Agtd_searchProject(pro)
    exe 'g/p:\S*'.tolower(a:pro)." /"
    let line_no = search ('^\s\+'.a:pro) 
    call cursor (line_no) 
    exe line_no."foldopen"
    exe line_no."foldopen"
    exe line_no."foldopen"
    exe line_no."foldopen"
    normal ztl
    exe line_no+1."foldopen"
endfunction


" List of projects 
"
" Custom function for auto-completion. Auto-complete with project names within
" current buffer.
function! Agtd_getProjectList(ArgLead, CmdLine, CursorPos)
    let proList = []
    let proName = '^\s\+'.a:ArgLead.'\u\+'
    let startPos = getpos ('.')
    call cursor (1,1)
    let pos = search(proName) 
    while pos != 0
        call cursor (pos)
        let line = getline (pos)
        let pro = matchstr (line,'\u\+')
        if index(proList, pro) == -1
            call add(proList, pro)
        endif

        let pos = search(proName, 'W') 
    endwhile
    call setpos ('.', startPos)
    return proList
endfunction


" Collect lines with a date mark on it
function! s:Agtd_getDateLines()
    let startPos = getpos ('.')
    let datesList = []
    call cursor (1,1)

    " Search tasks with dates
    let pos = search(s:agtd_dateRegx) 
    while pos != 0
        call cursor (pos)
        let line = getline ('.')
        let date = matchstr (line,s:agtd_dateRegx)
        let date = strpart (date, 3)
        if strlen (date) == 5
            let date = strftime("%Y")."-".date
        endif

        " Remove indentation
        let line = substitute (line, '^\s\+', "", "")
        let line = date."    ".line
        call add (datesList, line)
        let pos = search(s:agtd_dateRegx, 'W') 
    endwhile
    call setpos ('.', startPos)
    return datesList
endfunction


" Create a buffer with all marked the marked events
function! Agtd_displayCalendar()
    let datesList = s:Agtd_getDateLines()

    " Make that temporal buffer
    let tmpBuffer = "tmpGtdCalendarDisp.tmp"
    let gtdBuffer = bufnr ('')
    silent! exe 'edit '. tmpBuffer
    set buftype=nofile
    set bufhidden=hide
    set noswf
    set nobuflisted
    set filetype=agtd
    set fdm=indent
    set foldminlines=0

    " Display sorted list of dates and grouped by months
    let thisMonth = "XX"
    for line in sort (datesList)
        " Remove date and following empty spaces
        let line = substitute (line, s:agtd_dateRegx.'\s*', "", "")

        " Insert month if different from the previous one
        let month = matchstr(line, '-\d\d-')
        if match (month, thisMonth) == -1
            let thisMonth = month
            call append ('$', "")
            call append ('$', month)
        endif

        call append ('$', "\t".line)
    endfor

    " Remove two empty lines from the beginning and open all folds
    normal gg2ddzR
    set ro
endfunction


" Build an iCalendar file 
"
" Search in the current buffer all timestamps and build an iCalendarfile
" according RFC-5545. All the comments will be plain events, since the current
" notation will not recognize time-frames.
" 
" NOTES:
"   * The generated UID is random. If you generate and import the output
"   several times, you will get the same event repeated in your calendar
"   program because it cannot identify updated components TODO
function! Agtd_buildICalFile()
    let datesList = s:Agtd_getDateLines()
    let uid = 0

    " File header
    echo "BEGIN:VCALENDAR"
    echo "PRODID:-//DIGITAL-LUMBERJACK/AGTD Vim Calendar File 0.1//EN"
    echo "VERSION:2.0"
    
    for line in sort (datesList)
        let stamp = substitute(line, "-", "", "g")
        let stamp = matchstr(line, "\d\{8}")
        let summary = strpart (line, 13)
        echo "BEGIN:VEVENT"
        echo "DTSTAMP:".stamp."T000000Z"
        echo "UID:".uid."@agtd-vim"
        echo "SUMMARY:".summary

        let uid = uid + 1
    endfor
    echo "END:VCALENDAR"
endfunction

function! Agtd_getShortHelp()
    echo "AGTD Commands\n\n"
    echo ":GSearch      Jump to project task (with autocomplete)"
    echo ":GCalendar    Extract dates from tasks and build a calendar"
    echo ":GInsert      Move task to the NOW section and attach project path"
    echo ":GGo          Extract project name from current line and jump to it\n\n"
    echo ":help agtd    More help about AGTD"
endfunction

" Almighty GTD Vim script commands
command -nargs=1 -complete=customlist,Agtd_getProjectList GSearch call Agtd_searchProject("<args>")
command GCalendar call Agtd_displayCalendar()
command GInsert call Agtd_insertTask()
command GGo call Agtd_getProject() 
command AG call Agtd_getShortHelp() 

finish

syntax/agtd.vim	[[[1
55
" Language: AGTD syntax file
" Author: Francisco Garcia Rodriguez <public@francisco-garcia.net>
"
" Licence: Copyright (C) 2010 Francisco Garcia Rodriguez
" This program is free software: you can redistribute it and/or
" modify it under the terms of the GNU General Public License.
" See http://www.gnu.org/licenses/gpl.html
" 
" This program is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
" GNU General Public License for more details.
" 
" History:	
"   See plugin/agtd.vim 
"  

syn match       NOSPELL1        "\<\u\+\>" contains=@NoSpell
syn match       NOSPELL2        "\S*[:\=]\S*" contains=@NoSpell
syn match       NOSPELL3        "\S\+=\S\+" contains=@NoSpell

syn match       TASK            "\s@\w\+" contains=@NoSpell
syn match       SECTION         "^#.*" contains=@NoSpell
syn match       LIST            "^\s\+[-\*]"

" Project names must be written in a x4 column
syn match       PROJECT         "^\(\s\{4}\)\+\u\(\u\|\d\|_\)\+\s*\(--.*\)\=$" contains=PROTOCOL

" Project name within task line
syn match       PROJECT         "\sp\(:\w*\)\+" contains=@NoSpell

syn match       SUBCOMMENT      "^\s\+\(#\|;\)\s.*"hs=s+1
syn match       PROTOCOL        "\w\+::" nextgroup=URL,USERNAME contains=@NoSpell
syn match       URL             "\S\+" contained contains=@NoSpell
syn match       USERNAME        "\S\+@" contained nextgroup=URL contains=@NoSpell

"syn match       DATE            "\d\{4}-\d\d-\d\d"hs=s+4
syn match       DATE            "\d\{4}-\d\d-\d\d"

" Define the default highlighting.
" Only used when an item doesn't have highlighting yet
hi def link SECTION Comment

hi TASK guifg=lightgreen
hi SUBCOMMENT guifg=darkcyan 
hi PROJECT guifg=lightred
hi LIST guifg=magenta
hi MARK guifg=lightcyan
hi PROTOCOL guifg=lightmagenta 
hi URL guifg=lightcyan gui=underline
hi USERNAME guifg=lightblue
hi DATE guifg=lightred
syn cluster GTDHI add=TASK,MARK,PROTOCOL,URL,USERNAME

" vim: ts=8 sw=2
