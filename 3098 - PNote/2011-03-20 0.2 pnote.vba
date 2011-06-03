" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/pnote.vim	[[[1
121
" ------------------------------------------------------------------------------
"
" File: Power Note for Vim
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
" Webpage: 
"   https://github.com/FGarcia/pnote/
"   http://www.vim.org/scripts/script.php?script_id=3098
"
" Files:    
"       plugin/pnote.vim
"       syntax/pnote.vim
"       ftplugin/pnote.vim
"
" Version:  0.2 
"
" History:
"   v0.2  2011-03-20
"       Syntax for bibliography nodes
"       Syntax for command line instructions
"       Easy copy to command line instructions to the system clipboard
"       Minor bugfixes
"
"   v0.1  2010-05-23
"       Initial version
" ------------------------------------------------------------------------------


" Yank lines into the "+" register removing leading "$". Result will be
" displayed within the status line
fu! Pnote_YankCode()
    " Get array of lines
    let codeLines = split (getreg('0'),"\n")
    " Remove empty space [and code mark]
    call map (codeLines, 'substitute(v:val,''^\s\+\$\=\s*'', "", "")')
    " Convert array into one single string
    let output = join(codeLines,"\n")
    " Copy to clipboard and display result
    call setreg ("+", output)
    echo output
endfu


" Column of the first character for sections
" 
" Sections start with a main section marker (#) or with a subsection (;) 
"
" If neither of them is found, it returns the column of the
" first character. Otherwise returns -1
"
" (Taken from AGTD vim script)
function! Pnote_getSectionColumn(lineNum)
    let lineText = getline (a:lineNum)
    let lineCol = match (lineText,'\S')
    if lineCol == -1
        " No fold level for empty line
        return -1
    endif

    let mainSectionLine = match (lineText,'^#')
    if mainSectionLine == 0
        return lineCol
    endif

    let markerPos = match (lineText,';')
    if markerPos == lineCol
        " Line starting with a section marker
        return markerPos
    else
        " Other type of lines
        return -1
    endif
endfu


" FoldLevel for PNOTE files
"
" The fold level is based on the column of the first section marker divided
" by the current tab width. Tab markers are identified by the function
" Pnote_getSectionColumn()
"
" If no section marker is found in the current line, it will look for one in
" the previous lines divided. In such cases it will be assumed that the
" current line is one level deeper (+1) within the first previous section
" line.
"
function! Pnote_getFoldLevel(pos)
    " Get section marker position
    let sectionColumn = -1
    let lineNum = a:pos + 1
    while sectionColumn == -1 && lineNum != 1
        let lineNum -= 1
        let sectionColumn = Pnote_getSectionColumn(lineNum)
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

ftplugin/pnote.vim	[[[1
42
" ------------------------------------------------------------------------------
"
" File: Power Note for Vim
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
" Version:
" History:
"   (see plugin/pnote.vim)
"

if exists ("b:did_ftplugin_pnote")
   finish
endif
let b:did_ftplugin_pnote = 1

setlocal foldexpr=Pnote_getFoldLevel(v:lnum)
setlocal fdm=expr
setlocal fdi=";"
setlocal nowrap
setlocal autoindent
setlocal tw=80
setlocal formatoptions=tcqn
setlocal formatlistpat=^\\s*\\*\\s*
"setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*

" Yank and reformat current line
nmap <Leader>y Y:call Pnote_YankCode()<CR>

" Yank and reformat [current] selection
vmap <Leader>y y:call Pnote_YankCode()<CR>

syntax/pnote.vim	[[[1
62
"
" File: Power Note Syntax
"
" Description:
" This syntax file comes along a Vim script that should be at
" ftplugin/pnote.vim  Go there for more info on Power Note
"
" Author: Francisco Garcia Rodriguez <public@francisco-garcia.net>
"
" Licence: Copyright (C) 2010 Francisco Garcia Rodriguez
"
" This program is free software: you can redistribute it and/or
" modify it under the terms of the GNU General Public License.
" See http://www.gnu.org/licenses/gpl.html
" 
" This program is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
" GNU General Public License for more details.
" 
" http://www.gnu.org/licenses/gpl.html
"
" Version:
" History:
"   (see plugin/pnote.vim)
"

if exists("b:current_syntax")
  finish
endif
let b:current_syntax = "pnote"

syn keyword	confTodo	contained TODO FIXME XXX 
syn match	confComment	"^#\s.*" contains=confTodo
syn region	confString	start=+"+ skip=+\\\\\|\\"+ end=+"+ oneline
syn region	confString	start=+'+ skip=+\\\\\|\\'+ end=+'+ oneline
syn match       KEYWORD         "#\w\+#"hs=s+1,he=e-1
syn match       SUB_COMMENT     "\s\+\(#\|--\)\s.*"hs=s+1
syn match       ANOTATION       "\s\+;\s.*"
syn match       COMMAND         "\s*$\s[^#]*" contains=Comment
syn match	LIST	        "^\s\+\*"
syn match	BIBLIO	        "\[\w\+\]"

"syn region	block	start=+^#+ end=+^\s*$+ contains=inside,confComment,ANOTATION
"syn region	inside	start=+^ + skip=+$\+ + end=+^\s*$+ contained fold 

"syn region	inside	start=+^ + end=+^\s*$+ fold 

" Define the default highlighting.
" Only used when an item doesn't have highlighting yet
hi def link confComment	Comment
hi def link confTodo	Todo
hi def link confString	String
hi SUB_COMMENT guifg=darkcyan 
hi ANOTATION guifg=lightgreen
hi COMMAND guifg=lightcyan
hi LIST guifg=magenta
hi BIBLIO guifg=magenta
hi KEYWORD guifg=lightred 


" vim: ts=8 sw=2
