" Vim indent file
" Language:	sql (keywords based on Sybase Adaptive Server Anywhere)
" Maintainer:	David Fishburn <fishburn@sybase.com>
" Based on:	Ada.vim  - Neil Bird
" Last Change:  2002 Aug 23
" Version:	1.0
" Look for the latest version at http://vim.sourceforge.net/
"

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
   finish
endif
let b:did_indent = 1

setlocal indentkeys-=0{
setlocal indentkeys-=0}
setlocal indentkeys-=:
setlocal indentkeys-=0#
setlocal indentkeys-=e

" This indicates formatting should take place when one of these 
" expressions is used.  These expressions would normally be something
" you would type at the BEGINNING of a line
" SQL is generally case insensitive, so this files assumes that
" These keywords are something that would trigger an indent LEFT, not
" an indent right, since the SQLBlockStart is used for those keywords
setlocal indentkeys+==~end,=~else,=~elseif,=~elsif,0=~when,0=)

" GetSQLIndent is executed whenever one of the expressions 
" in the indentkeys is typed
setlocal indentexpr=GetSQLIndent()

" Only define the functions once.
if exists("*GetSQLIndent")
   finish
endif

" List of all the statements that start a new block.
" These are typically words that start a line.
let s:SQLBlockStart = '^\s*\(if\|else\|elseif\|elsif\|while\|loop\|do\|begin\|case\|when\)\>'
let s:SQLBlockEnd = '^\s*\(end\)\>'
let s:SQLComment = "\\v^(\"[^\"]*\"|'.'|[^\"']){-}\\zs\\s*--.*"


" The indent level is also based on unmatched paranethesis
" If a line has an extra "(" increase the indent
" If a line has an extra ")" decrease the indent
function s:CountUnbalancedParan( line, paran_to_check )
   let l = a:line
   let lp = substitute(l, '[^(]', '', 'g')
   let l = a:line
   let rp = substitute(l, '[^)]', '', 'g')

   if a:paran_to_check =~ ')'
      " echom 'CountUnbalancedParan ) returning: ' . (strlen(rp) - strlen(lp))
      return (strlen(rp) - strlen(lp))
   elseif a:paran_to_check =~ '('
      " echom 'CountUnbalancedParan ( returning: ' . (strlen(lp) - strlen(rp))
      return (strlen(lp) - strlen(rp))
   else
      " echom 'CountUnbalancedParan unknown paran to check: ' . a:paran_to_check
      return 0
   endif
endfunction

" Unindent commands based on previous indent level
function s:CheckToUnindentCommand( prev_indent, prev_lnum, num_levels )
   let lnum = a:prev_lnum
   let line = getline(lnum)
   let ends = 0
   let ind = a:prev_indent
   let remaining_indent_level = a:num_levels
   while lnum > 0
      " Find previous line with a lower indent level
      " If command is an if or while, do not unindent the line
      " Example
      " if (
      "    ) then
      "    leave it indented
      "
      " create table T1 (
      "    id integer
      "    )
      " indent should fall back in this case
      "    
      if indent(lnum) < a:prev_indent
         let remaining_indent_level = remaining_indent_level - 1

         " if line has an if or while clause, leave indent the same
         if getline(lnum) =~? '\(if\|while\)\>'
            if remaining_indent_level == 0
               " echom 'CheckToUnindentCommand - ind: ' . ind
               return ind
            endif
         else
            let ind = ind - &sw

            if remaining_indent_level == 0
               " echom 'CheckToUnindentCommand - ind: ' . ind
               return ind
            endif
         endif
      endif

      let lnum = prevnonblank(lnum - 1)
      " Get previous non-blank/non-comment-only line
      while 1
	 let line = getline(lnum)
	 let line = substitute( line, s:SQLComment, '', '' )
	 if line !~ '^\s*$'
	    break
	 endif
	 let lnum = prevnonblank(lnum - 1)
	 if lnum <= 0
	    return a:prev_indent
	 endif
      endwhile
   endwhile

   " Fallback - just move back one
   return a:prev_indent - &sw
endfunction

" Find correct indent of a new line based upon the previous line
function GetSQLIndent()
   " echom 'GetSQLIndent start indent: ' . indent(v:lnum) . '  line: ' . getline(v:lnum)
   " Find a non-blank line above the current line.
   let lnum = prevnonblank(v:lnum - 1)
   let ind = indent(lnum)

   " echom 'GetSQLIndent - indent ' . ind . '  line: ' . getline(".")
   " Get previous non-blank/non-comment-only/non-cpp line
   while 1
      let line = substitute( getline(lnum), s:SQLComment, '', '' )
      if line !~ '^\s*$'
         " echom 'previous non blank - break: ' . line
         break
      endif
      let lnum = prevnonblank(lnum - 1)
      if lnum <= 0
         return ind
      endif
   endwhile

   " echom 'PREVIOUS INDENT: ' . indent(lnum) . '  LINE: ' . getline(lnum) 
   "
   " This is the line you just hit return on
   " Based on this line, we can determine how much to indent the new
   " line, which is a new blank line
   " Get default indent (from prev. line)
   let ind = indent(lnum)

   " Now check what's on the previous line
   if line =~? s:SQLBlockStart
      " Move indent in
      let ind = ind + &sw
      " echom 'prevl - SQLBlockStart - indent ' . ind . '  line: ' . line
   " elseif line =~? s:SQLBlockEnd
      " Move indent out
      " let ind = ind - &sw
      " echom 'prevl - SQLBlockEnd - indent ' . ind . '  line: ' . line
   elseif line =~ '[()]'
      if line =~ '('
         let num_unmatched_left = s:CountUnbalancedParan( line, '(' )
      else
         let num_unmatched_left = 0
      endif
      if line =~ ')'
         let num_unmatched_right  = s:CountUnbalancedParan( line, ')' )
      else
         let num_unmatched_right  = s:CountUnbalancedParan( line, ')' )
      endif
      if num_unmatched_left > 0
         " There is a open left paranethesis 
         " increase indent
         let ind = ind + ( &sw * num_unmatched_left )
      elseif num_unmatched_right > 0
         " if it is an unbalanced paranethesis only unindent if
         " it was part of a command (ie create table(..)  )
         " instead of part of an if (ie if (....) then) which should
         " maintain the indent level
         let ind = s:CheckToUnindentCommand( ind, lnum, num_unmatched_right )
      " else
      "   echom 'prevl - () balanced - indent ' . ind .
      endif
   endif


   " echom 'CURRENT INDENT: ' . ind . '  LINE: '  . getline(v:lnum)
  
   " This is a new blank line since we just typed a carriage return
   " Check current line; search for simplistic matching start-of-block
   let line = getline(v:lnum)

   if line =~? '^\s*els'
      " Any line when you type else will automatically back up one 
      " ident level  (ie else, elseif, elsif)
      let ind = ind - &sw
      " echom 'curr - else - indent ' . ind
   elseif line =~? '^\s*end\>'
      " General case for end
      let ind = ind - &sw
      " echom 'curr - end - indent ' . ind
   elseif line =~? '^\s*when\>'
      let ind = ind - &sw
   " else
     " echom 'CURR fall through: '.ind.' : '.line
   endif

   " echom 'final - indent ' . ind
   return ind
endfunction

" vim: set sw=3 sts=3 :
