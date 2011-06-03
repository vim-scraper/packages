" Vim indent file
" Language:    SQL 
" Maintainer:  David Fishburn <fishburn at ianywhere dot com>
" Last Change: Fri Jun 04 2004 12:04:19
" Version:     1.3
" Download:    http://vim.sourceforge.net/script.php?script_id=495

" Notes:
"    Indenting keywords are based on Oracle and Sybase Adaptive Server
"    Anywhere (ASA).  Test indenting was done with ASA stored procedures and
"    fuctions and Oracle packages which contain stored procedures and
"    functions.
"    This has not been tested against Microsoft SQL Server and Sybase
"    Adaptive Server Enterprise which use the Transact-SQL syntax.  
"    That syntax does not have end tags for IF's, which makes indenting
"    more difficult.
"
" Known Issues:
"    The Oracle MERGE statement does not have an end tag associated with
"    it, this can leave the indent hanging to the right one too many.

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
" IS is excluded, since it is difficult to determine when the 
" ending block is (especially for procedures/functions).
let s:SQLBlockStart = '^\s*\%('.
         \ 'if\|else\|elseif\|elsif\|'.
         \ 'while\|loop\|do\|'.
         \ 'begin\|'.
         \ 'case\|when\|merge\|exception'.
         \ '\)\>'
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
                  \ 'IsColComment(line("."), col("."))')

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

   " Based on the keyword provided, loop through previous non empty
   " non comment lines to find the statement that initated the keyword.
   " Return its indent level
   "    CASE ..
   "    WHEN ...
   " Should return indent level of CASE
   "    EXCEPTION ..
   "    WHEN ...
   "         something;
   "    WHEN ...
   " Should return indent level of exception.
   function s:GetStmtStarterIndent( keyword, curr_lnum )
      let lnum  = a:curr_lnum

      " Default - reduce indent by 1
      let ind = indent(a:curr_lnum) - &sw

      if a:keyword =~? 'end'
         exec 'normal! ^'
         let stmts = '^\s*\%('.
                  \ '\<begin\>\|' .
                  \ '\%(\%(\<end\s\+\)\@<!\<loop\>\)\|' .
                  \ '\%(\%(\<end\s\+\)\@<!\<case\>\)\|' .
                  \ '\%(\%(\<end\s\+\)\@<!\<for\>\)\|' .
                  \ '\%(\%(\<end\s\+\)\@<!\<if\>\)'.
                  \ '\)'
         let matching_lnum = searchpair(stmts, '', '\<end\>\zs', 'bW',
                  \ 'IsColComment(line("."), col(".")) == 1')
         exec 'normal! $'
         if matching_lnum > 0 && matching_lnum < a:curr_lnum
            let ind = indent(matching_lnum)
         endif
      elseif a:keyword =~? 'when'
         exec 'normal! ^'
         let matching_lnum = searchpair(
                  \ '\%(\<end\s\+\)\@<!\<case\>\|\<exception\>\|\<merge\>', 
                  \ '', 
                  \ '\%(\%(\<when\s\+others\>\)\|\%(\<end\s\+case\>\)\)', 
                  \ 'bW',
                  \ 'IsColComment(line("."), col(".")) == 1')
         exec 'normal! $'
         if matching_lnum > 0 && matching_lnum < a:curr_lnum
            let ind = indent(matching_lnum)
         else
            let ind = indent(a:curr_lnum)
         endif
      endif

      return ind
   endfunction


   " Check if the line is a comment
   function IsLineComment(lnum)
      let rc = synIDattr(
               \ synID(a:lnum, 
               \     match(getline(a:lnum), '\S')+1, 0)
               \ , "name") 
               \ =~? "comment" 

      return rc
   endfunction


   " Check if the column is a comment
   function IsColComment(lnum, cnum)
      let rc = synIDattr(synID(a:lnum, a:cnum, 0), "name") 
               \           =~? "comment" 

      return rc
   endfunction


   " Find correct indent of a new line based upon the previous line
   function GetSQLIndent()
      let lnum = v:lnum 
      let ind = indent(lnum)

      " If the current line is a comment, leave the indent as is
      if IsLineComment(lnum) == 1
         return ind
      endif

      " Get previous non-blank/non-comment-only/non-cpp line
      while 1
         let lnum = prevnonblank(lnum - 1)
         if lnum <= 0
            return ind
         endif

         " Check the syntax attribute of this line for the first non-white 
         " space character (match(line("."), '\S'))
         if IsLineComment(lnum) == 1
            continue
         endif

         let prevline = getline(lnum)
         if prevline !~ '^\s*$'
            " echom 'previous non blank - break: ' . prevline
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
      if prevline =~? s:SQLBlockStart
         " Move indent in
         let ind = ind + &sw
         " echom 'prevl - SQLBlockStart - indent ' . ind . '  line: ' . prevline
      elseif prevline =~ '[()]'
         if prevline =~ '('
            let num_unmatched_left = s:CountUnbalancedParan( prevline, '(' )
         else
            let num_unmatched_left = 0
         endif
         if prevline =~ ')'
            let num_unmatched_right  = s:CountUnbalancedParan( prevline, ')' )
         else
            let num_unmatched_right  = 0
            " let num_unmatched_right  = s:CountUnbalancedParan( prevline, ')' )
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

            if prevline =~ '^\s*)'
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
         let ind = s:GetStmtStarterIndent('end', v:lnum)
         " General case for end
         " let ind = ind - &sw
         " echom 'curr - end - indent ' . ind
      elseif line =~? '^\s*when\>'
         let ind = s:GetStmtStarterIndent('when', v:lnum)
         " If the WHEN clause is used with a MERGE or EXCEPTION
         " clause, do not change the indent level, since these
         " statements do not have a corresponding END statement.
         " if stmt_starter =~? 'case'
         "    let ind = ind - &sw
         " endif
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

   " vim: set sw=4 sts=3 :
