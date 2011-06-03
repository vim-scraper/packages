" showbrace.vim
" Author: Tatu Tarvainen (ttarvain@mail.student.oulu.fi)
" Version: 0.1

if (exists("*ShowBraceMatch"))
  delfunction ShowBraceMatch
endif
 
" Function that shows the line that has the matching curlybrace
function ShowBraceMatch()
   let s:line = searchpair('{', '', '}', "nbW")
   if (s:line > 0)
      " Get the line without leading whitespace
      let s:data = substitute(getline(s:line), "[ \t]*", "", "")

      " Is this K&R style?
      if (match(s:data, "\{") > 0)
         " yes it is... show the line as it is
         echo "Match on line ". s:line . ": ".s:data
      else
         " no, show the previous nonempty line
         let s:line1 = prevnonblank(s:line - 1)
         if (s:line1 > 0)
            echo "Match on line ". s:line1 . ": ".substitute(getline(s:line1), "[ \t]*", "", "") . " ... {"
         else
            echo "No match."
         endif
      endif
   else
      echo "No match."
   endif
endfunction

inoremap <buffer> } }<Esc>:cal ShowBraceMatch()<Cr>a

set noshowmode
