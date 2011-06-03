" Vim syntax file " Language:	    LDraw files model files (including parts and multi-part" documents)
" Maintainer:	Jack Hawk <jack_lego_manaic@yahoo.com>
" Filenames:    *.ldr, *.mpd, *.dat
" Last Change:	5th July 2002
" Web Page:     N/A
" Notes:        This syntax file is for use with LDraw (Lego Draw) CAD files.
"               Information about this suite of applications can be found at
"               http://www.ldraw.org.
"
"               This is my first attempt at a syntax file, but as far as I
"               know it is complete for the LDraw file formats.
"
"               Jack Hawk

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" LDraw Meta-Commands (from http://www.ldraw.org/reference/specs/fileformat)
syn keyword ldrawMetaCommand            MODEL TITLE STEP WRITE CLEAR PAUSE SAVE contained
syn keyword ldrawMetaCommand            COMMENT PART LINE TRIANGLE QUADRILATERAL  contained
syn keyword ldrawMetaCommand            CONDITIONAL-LINE contained

" LDraw Meta-Commands (other)
syn keyword ldrawMetaCommand            ROTATION CONFIG CENTER contained

" LDraw files are case insensitive (other than Meta-Commands, according to ldraw.org)
syn case ignore

" LDraw line type info
syn match ldrawComment                  /^\s*0.*$/ contains=ldrawMetaCommand skipwhite

syn match ldrawLineType                 /^\s*[1-5]/ nextgroup=ldrawColor skipwhite
syn match ldrawColor                    /\d\+/ contained nextgroup=ldrawCoord1 skipwhite

syn match ldrawCoord1                   /\([\-\.0-9]\+\s*\)\{3\}/ contained nextgroup=ldrawCoord2 skipwhite
syn match ldrawCoord2                   /\([\-\.0-9]\+\s*\)\{3\}/ contained nextgroup=ldrawCoord3 skipwhite
syn match ldrawCoord3                   /\([\-\.0-9]\+\s*\)\{3\}/ contained nextgroup=ldrawCoord4 skipwhite
syn match ldrawCoord4                   /\([\-\.0-9]\+\s*\)\{3\}/ contained

command -nargs=+ HiLink hi def link <args>

" simply used syntax types at will for coloring, aside from Comments
HiLink ldrawComment                           Comment
HiLink ldrawLineType                          Statement
HiLink ldrawColor                             Identifier 
HiLink ldrawMetaCommand                       PreProc

HiLink ldrawCoord1                            Constant 
HiLink ldrawCoord2                            Special
HiLink ldrawCoord3                            Constant 
HiLink ldrawCoord4                            Special

delcommand HiLink

let b:current_syntax = "ldraw"

" vim: ts=8
