"*****************************************************************************
"
"  Filename:      vhdl.vim - Vim indent file
"
"  Language:      VHDL
"
"  Description:   Vim VHDL indent file. Should indent VHDL code, so it has 
"                 structured look.
"
"  Authors:       N. J. Heo, Janez Stangelj
"
"  Last Change:   November the 6-th, 2003 
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
setlocal indentexpr=VhdlGetIndent(v:lnum)

" ?
"setlocal indentkeys&

" Reindenting at: 
setlocal indentkeys+==~),=~(,=~;,=~then,=~begin,=~is,=~=>,=~units,=~record,=~:
setlocal indentkeys+==~when,=~loop,=~block,=~component,=~process,=~use,=~for
setlocal indentkeys+==~else,=~elseif,=~select


" Don't need C predprocessor indenting "#", "{" and "}" indenting.
setlocal indentkeys-==0{,0},0#


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
" Function name: VhdlGetIndent(lnum)
"
" What function should do: It should calculate number of spaces before first
"     word in line, depending on VHDL syntax specification.
"
" Global variables: -
"
"*****************************************************************************
fun VhdlGetIndent(lnum)
   "
	" Assign zero indent to lines containing word "library",
   "     preceded by one or more whitespaces, with anything following it.
   "
	let s:this_line = getline(a:lnum)
	let s:PREPROC = '^\s*library\s'
	if s:this_line =~? s:PREPROC
		return 0
	endif
   "
	" Find a non-blank line above the current line.
	" Skip over preprocessor directive "library".
   "
	let s:lnum = a:lnum
	while s:lnum > 0
		let s:lnum = prevnonblank(s:lnum - 1)
		let s:previous_line = getline(s:lnum)
		if s:previous_line !~? s:PREPROC
			break
		endif
	endwhile
   "
	" When the start of the file reached, use zero indent and return 0 (as 
   "     the value of s:ind variable).
   "
   " Otherwise indent line (assign s:lnum) and save returned value to s:ind 
   "     variable.
   "
	if s:lnum == 0
		return 0
   else
      let s:ind = indent(s:lnum)
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
   " Initialize s:start_position to zero
   "
   let s:start_position = 0
   "
   " Search for "(" character in s:previous_line.
   "
   let s:start_position = match(s:previous_line,'(',s:start_position)
   "
   " Loop until all "(" clauses counted.
   "
   while s:start_position != -1
      "
      " Increment s:ind by &sw, increment last returned match position by one.
      " Search for "(" character in s:previous_line starting from previously
      "     returned s:start_position.
      "
      let s:ind = s:ind + &sw
      let s:start_position = s:start_position + 1
      let s:start_position = match(s:previous_line,'(',s:start_position)

   endwhile


   "
   " Increment s:ind when previous line contains "type", "is range" and this 
   " line contains "units" word.
   "
   if s:previous_line =~? '^\s*type\s.*is\srange\s' && s:this_line =~? '^\s*units\s*$'
      let s:ind = s:ind + &sw
   endif
   "
	" Add one shift width to s:ind variable, when previous line contains:
   "
   "     only "begin" word in the whole line, 
   "     "process" at the beginning of the line, 
   "     "is" at the end of the line, 
   "     single "record" word in the whole line, 
   "     single "units"word in the whole line, 
   "     "else" word as the first word in line, 
   "     "elsif" + "then" words, 
   "     "if" + "then" words, 
   "     "elsif" + "then" words, 
   "     "when" + "=>" words,
   "     single "loop" word in the line,
   "     "while" + "loop" words,
   "     "for" + "in" + "loop" words,
   "     "for" word at the beginning of the line,
   "     "block" word at the end of the line,
   "     "component" in the beginning of the line
   "     "<=" + "after" + "when" words + "else" at the end of the line.
   "
	if s:previous_line =~? '^\s*\(\(begin\|units\|record\|else\|loop\)\|\(elsif\|if\|when\|while\|for\s.*\sin\|with\)\s.*\(\s\(then\|loop\|select\)\|=>\)\)\s*$\|\(\sis\|block\|<=.*\safter\s.*\swhen.*\selse\)\s*$\|^\s*\(for\|component\|process\)\s'
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
	if s:this_line =~? '^\s*\(end\s\|else\)' || s:previous_line =~? '\s\(after\|when\)\s.*;\s*$' || (s:previous_line =~? '\(;\|^\s*when\sothers\s*=>\|block\)\s*$\|^\s*\(process\s\|architecture\)' && s:this_line =~? '^\s*\(\(begin\|end\scase;\)\s*$\|\(elsif\|when\)\s\)')
      let s:ind = s:ind - &sw
	endif
   if 
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
   if (s:previous_line =~? ';\s*$' && s:this_line =~? '^\s*end\scase;\s*$') || s:previous_line =~? '^\s*\(end\sunits;\|end\srecord;\)\s*$'
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
   " Loop until all ")" clauses counted.
   "
   while s:start_position != -1
      "
      " Decrement s:ind by &sw, increment last returned match position by one.
      " Search for ")" character in s:previous_line starting from previously
      "     returned s:start_position.
      "
      let s:ind = s:ind - &sw
      let s:start_position = s:start_position + 1
      let s:start_position = match(s:previous_line,')',s:start_position)

   endwhile


   "
   " return indent number
   "
   return s:ind

endfun

