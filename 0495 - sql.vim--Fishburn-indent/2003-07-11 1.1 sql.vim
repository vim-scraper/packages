" Vim indent file
" Language:    sql (keywords based on Sybase Adaptive Server Anywhere)
" Maintainer:  David Fishburn <fishburn@sybase.com>
" Based on:    Ada.vim  - Neil Bird
" Last Change: Sun May 25 2003 11:24:02 PM
" Version:     1.1
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

" The indent level is also based on unmatched paranethesis
" If a line has an extra "(" increase the indent
" If a line has an extra ")" decrease the indent
function s:CountUnbalancedParan( line, paran_to_check )
   let l = a:line
   let lp = substitute(l, '[^(]', '', 'g')
   let l = a:line
   let rp = substitute(l, '[^)]', '', 'g')

   if a:paran_to_check =~ ')'
      " echom 'CountUnbalancedParan ) returning: ' .
      " \ (strlen(rp) - strlen(lp))
      return (strlen(rp) - strlen(lp))
   elseif a:paran_to_check =~ '('
      " echom 'CountUnbalancedParan ( returning: ' .
      " \ (strlen(lp) - strlen(rp))
      return (strlen(lp) - strlen(rp))
   else
      " echom 'CountUnbalancedParan unknown paran to check: ' .
      " \ a:paran_to_check
      return 0
   endif
endfunction

" Unindent commands based on previous indent level
function s:CheckToIgnoreRightParan( prev_lnum, num_levels )
   let lnum = a:prev_lnum
   let line = getline(lnum)
   let ends = 0
   let num_right_paran = a:num_levels
   let ignore_paran = 0
   let vircol = 1

   while num_right_paran > 0
      silent! exec 'norm! '.lnum."G\<bar>".vircol."\<bar>"
      let right_paran = search( ')', 'W' )
      if right_paran != lnum
         " This should not happen since there should be at least
         " num_right_paran matches for this line
         break
      endif
      let vircol      = virtcol(".")

      " if getline(".") =~ '^)'
      let matching_paran = searchpair('(', '', ')', 'bW',
                \ 'synIDattr(synID(line("."), col("."), 0), "name") 
                \           =~? "comment"')

      if matching_paran < 1
         " No match found
         " echom 'CTIRP - no match found, ignoring'
         break
      endif

      if matching_paran == lnum 
         " This was not an unmatched parantenses, start the search again
         " again after this column
         " echom 'CTIRP - same line match, ignoring'
         continue
      endif

      " echom 'CTIRP - match: ' . line(".") . '  ' . getline(".")

      if getline(matching_paran) =~? '\(if\|while\)\>'
         " echom 'CTIRP - if/while ignored: ' . line(".") . '  ' . getline(".")
         let ignore_paran = ignore_paran + 1
      endif

      " One match found, decrease and check for further matches
      let num_right_paran = num_right_paran - 1

   endwhile

   " Fallback - just move back one
   " return a:prev_indent - &sw
   return ignore_paran
endfunction

" Find correct indent of a new line based upon the previous line
function GetSQLIndent()
   " Find a non-blank line above the current line.
   " let lnum = prevnonblank(v:lnum - 1)
   let lnum = v:lnum 
   let ind = indent(lnum)

   " Get previous non-blank/non-comment-only/non-cpp line
   while 1
      let lnum = prevnonblank(lnum - 1)
      if lnum <= 0
         return ind
      endif

      " Check the syntax attribute of this line for the first non-white 
      " space character (match(line("."), '\S'))
      if synIDattr(synID(lnum, match(getline(lnum), '\S')+1, 0), "name") 
                \           =~? "comment" 
         continue
      endif

      let line = getline(lnum)
      if line !~ '^\s*$'
         " echom 'previous non blank - break: ' . line
         break
      endif
   endwhile

   " echom 'PREVIOUS INDENT: ' . indent(lnum) . '  LINE: ' . getline(lnum) 
   
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
   elseif line =~ '[()]'
      if line =~ '('
         let num_unmatched_left = s:CountUnbalancedParan( line, '(' )
      else
         let num_unmatched_left = 0
      endif
      if line =~ ')'
         let num_unmatched_right  = s:CountUnbalancedParan( line, ')' )
      else
         let num_unmatched_right  = 0
         " let num_unmatched_right  = s:CountUnbalancedParan( line, ')' )
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
         let ignore = s:CheckToIgnoreRightParan( lnum, num_unmatched_right )
         " echom 'prevl - ) unbalanced - CTIRP - ignore: ' . ignore

         if line =~ '^\s*)'
            let ignore = ignore + 1
            " echom 'prevl - begins ) unbalanced ignore: ' . ignore
         endif

         if (num_unmatched_right - ignore) > 0
            let ind = ind - ( &sw * (num_unmatched_right - ignore) )
         endif

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
      " elseif line =~ '^\s*)\s*;\?\s*$'
      " elseif line =~ '^\s*)'
   elseif line =~ '^\s*)'
      let num_unmatched_right  = s:CountUnbalancedParan( line, ')' )
      let ignore = s:CheckToIgnoreRightParan( v:lnum, num_unmatched_right )
      " If the line ends in a ), then reduce the indent
      " This catches items like:
      " CREATE TABLE T1(
      "    c1 int, 
      "    c2 int
      "    );
      " But we do not want to unindent a line like:
      " IF ( c1 = 1 
      " AND  c2 = 3 ) THEN
      " let num_unmatched_right  = s:CountUnbalancedParan( line, ')' )
      " if num_unmatched_right > 0
      " elseif strpart( line, strlen(line)-1, 1 ) =~ ')'
      " let ind = ind - &sw
      if line =~ '^\s*)'
         " let ignore = ignore + 1
         " echom 'curr - begins ) unbalanced ignore: ' . ignore
      endif

      if (num_unmatched_right - ignore) > 0
         let ind = ind - ( &sw * (num_unmatched_right - ignore) )
      endif
      " endif
   endif

   " echom 'final - indent ' . ind
   return ind
endfunction

" vim: set sw=3 sts=3 :
