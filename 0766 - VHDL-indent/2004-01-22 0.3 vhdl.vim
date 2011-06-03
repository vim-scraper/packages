"*****************************************************************************
"
"  Filename:      vhdl.vim - Vim indent file
"
"  Language:      VHDL
"
"  Description:   Vim VHDL indent file. Should indent VHDL code, so it has 
"                 more structured look. Also useful when seeking for mistakes
"                 like missing "end if".
"
"  Use:           For UNIX: This files should be copied into one of
"                    "$VIMRUNTIME/indent" directories. If indent directory
"                    does not exist, it can be created in one of the paths
"                    from message that shows up when ":echo $VIMRUNTIME" Vim
"                    command is executed. The line "filetype plugin on" must
"                    exist in vimrc file so Vim indenting is enabled. 
"
"                    For example, indent file could be here:
"                    "~/.vim/indent/vhdl.vim"
"
"
"  Authors:       N. J. Heo, Janez Stangelj
"
"  Last Change:   January the 21-th, 2004 
"
"*****************************************************************************


"*****************************************************************************
"
"  INITIALIZATION:
"
"*****************************************************************************

" Only do this when not done yet for this buffer
if exists("b:did_indent")
   finish
endif
let b:did_indent = 1

" Set which function evaluates local indent expression.
set indentexpr=VhdlGetIndent(v:lnum)

" ?
"setlocal indentkeys&

" Reindenting at: 
setlocal indentkeys+==~),=~(,=~;,=~then,=~begin,=~is,=~=>,=~units,=~record,=~:
setlocal indentkeys+==~when,=~loop,=~block,=~component,=~process,=~use,=~for
setlocal indentkeys+==~else,=~elseif,=~select,=~end


" Don't need C predprocessor indenting "#", "{" and "}" indenting.
setlocal indentkeys-=0{,0},0#


" Only define the function once.
if exists("*VhdlGetIndent")
   finish
endif


"*****************************************************************************
"
" FUNCTIONS:
"
"*****************************************************************************

"*****************************************************************************
"
" Function name: VhdlGetIndent(l_num)
"
" What function should do: It should calculate number of spaces before first
"     word in line, depending on VHDL syntax specification.
"
" Global variables: -
"
"*****************************************************************************
fun VhdlGetIndent(l_num)
   "
   " Define constants.
   "
   let s:PREPROC = '^\s*library\s'
   let s:PREDPROC_OR_COMMENT = '^\s*\(library\s\|--\)'
   let s:COMMENT = '^\s*--'
   "
   " Assign zero indent to lines containing word "library",
   "     preceded by one or more whitespaces, with anything following it.
   "
   let s:this_line = getline(a:l_num)
   if s:this_line =~? s:PREPROC
      return 0
   endif
   "
   " If this line is comment from the beginning, find next non-commenting
   " line.
   "
   let s:l_num_temp = a:l_num
   while s:l_num_temp > 0 && s:this_line =~? s:COMMENT
      let s:l_num_temp = nextnonblank( s:l_num_temp + 1 )
      let s:this_line = getline(s:l_num_temp)
   endwhile
   "
   " Find a non-blank line above the current line. Skip over preprocessor 
   " directive "library" and over lines begining with comment "--".
   "
   let s:l_num_temp = a:l_num
   let s:l_num_temp = prevnonblank( s:l_num_temp - 1 )
   let s:previous_line = getline( s:l_num_temp )
   while s:l_num_temp > 0 && s:previous_line =~? s:PREDPROC_OR_COMMENT
      let s:l_num_temp = prevnonblank( s:l_num_temp - 1 )
      let s:previous_line = getline( s:l_num_temp )
   endwhile
   "
   " When the start of the file reached, use zero indent and return 0 (as 
   "     the value of s:ind variable).
   "
   " Otherwise indent line (assign s:l_num_temp) and save returned value to 
   " s:ind variable.
   "
   if s:l_num_temp == 0
      return 0
   else
      let s:ind = indent(s:l_num_temp)
   endif


   " 
   " Legend of Vim's regular expressions used so frequently they aren't 
   " mentioned in  comment: 
   " ^         -  Start of the line. 
   " $         -  Last letter in line.
   " \s        -  White space letter.
   " \s*       -  Any number of white space letters, zero or more.
   " .*        -  Any number of letters, no matter which letters.
   " \<word\>  -  single word, not necessary separated by spaces.
   " ^\s*      -  Any number of white spaces from the beginning of line.
   " \s*$      -  Any number of white spaces before end of line.
   "
   " To test following regular expressions open some vhdl file with Vim and
   " type search command "/" then copy regular expression text ( text that is
   " written between ' and ' marks into command line and press enter.
   " You will see if search found any matching patterns.
   "


   "
   "
   " Count opening clauses. Increment s:ind for count * &sw number.
   "
   "
   " Search for "(" character in s:previous_line, get first position of '('
   "
   let s:start_position = match(s:previous_line,'(',0)
   "
   " Get s:comment_position.
   "
   let s:comment_position = match(s:previous_line,"--",0)
   "
   " Loop until all "(" clauses counted.
   "
   while (s:start_position != -1 && ((s:comment_position == -1) || (s:comment_position > s:start_position)))
      "
      " Increment s:ind by &sw, increment last returned match position by one.
      " Search for "(" character in s:previous_line starting from previously
      "     returned s:start_position.
      "
      let s:ind = s:ind + &sw
      let s:start_position = match(s:previous_line,'(',s:start_position + 1)
   endwhile


   "
   " Increment s:ind when previous line contains "type", "is range" and this 
   " line contains "units" word.
   "
   if (s:previous_line =~? '^\s*type\s.*is\srange\s' && s:this_line =~? '^\s*units\(\s*$\|\s*--\)')
      let s:ind = (s:ind + &sw)
   endif
   "
   " Add one shift width to s:ind variable, when previous line contains:
   "
   "     only "begin" word in the whole line, 
   "     single "loop" word in the line,
   "     single "record" word in the whole line, 
   "     single "units"in the whole line, 
   "     single "else" word in the whole line, 
   "
   "     "process" at the beginning of the line, 
   "     "for" at the beginning of the line,
   "     "component" at the beginning of the line
   "
   "     "when" + "=>" words,
   "     "while" + "loop" words,
   "     "for" + "in" + "loop" words,
   "     "is" at the end of the line, 
   "     "then" at the end of the line,
   "     "block" at the end of the line,
   "     "<=" + "after" + "when" words + "else" at the end of the line.
   "
   if (s:previous_line =~? '^\s*\(\(begin\|units\|record\|else\|loop\)\|\(when\|while\|for\s.*\sin\|with\)\s.*\(\s\(loop\|select\)\|=>\)\)\(\s*$\|\s*--\)\|\(\sis\|block\|then\|<=.*\safter\s.*\swhen.*\selse\)\(\s*$\|\s*--\)\|^\s*\(for\|component\|process\)\s')
      let s:ind = s:ind + &sw
   endif


   "
   " Subtract shift width to s:ind variable:
   "
   " When this line contains "end" or "else" word from the beginning of 
   " the line.
   "
   " When previous line contains "after" + ";" words.
   "
   " Or when previous line contains ";" word at the end of the line and this 
   " line contains only "begin" word; or "elsif" or "when" word, at the start 
   " of the line.
   "
   " It also substracts s:ind when previous line contains "block" word at the
   " end of the line or "architecture" or "process" at the beginning of the 
   " line. Together with this line containging only "begin" word.
   "
   " Or previous line contains "when others" + "=>" words and this line
   " contains only "end case;" words.
   "
   if (s:this_line =~? '^\s*\(end\s\|else\)' || s:previous_line =~? '\s\(after\|when\)\s.*;\(\s*$\|\s*--\)' || (s:previous_line =~? '\(;\|^\s*when\sothers\s*=>\|block\)\(\s*$\|\s*--\)\|^\s*\(process\s\|architecture\)' && s:this_line =~? '^\s*\(\(begin\|end\scase;\)\(\s*$\|\s*--\)\|\(elsif\|when\)\s\)'))
      let s:ind = s:ind - &sw
   endif
   "
   " Subtract shift width again to s:ind variable:
   "
   " When previous line contains ";" at the end of the line and this line
   " contains "end case;" words.
   "
   " Or if previous line is "end units;".
   "
   " Or in case previous line is "end record;".
   "
   if ((s:previous_line =~? ';\(\s*$\|\s*--\)' && s:this_line =~? '^\s*end\scase;\(\s*$\|\s*--\)') || s:previous_line =~? '^\s*\(end\sunits;\|end\srecord;\)\(\s*$\|\s*--\)')
      let s:ind = s:ind - &sw
   endif


   "
   "
   " Count number of closing clauses. Decrement s:ind for count * &sw number.
   "
   "
   " Initialize s:start_position to zero
   "
   let s:start_position = 0
   "
   " Search for ")" character in s:previous_line.
   "
   let s:start_position = match(s:previous_line,')',s:start_position)
   "
   " Get s:comment_position.
   "
   let s:comment_position = match(s:previous_line,"--",0)
   "
   " Loop until all ")" clauses counted.
   "
   while (s:start_position != -1 && ((s:comment_position == -1) || (s:comment_position > s:start_position)))
      "
      " Decrement s:ind by &sw, increment last returned match position by one.
      " Search for ")" character in s:previous_line starting from previously
      "     returned s:start_position.
      "
      let s:ind = s:ind - &sw
      let s:start_position = match(s:previous_line,')',s:start_position + 1)

   endwhile


   "
   " return indent number
   "
   return s:ind

endfun

