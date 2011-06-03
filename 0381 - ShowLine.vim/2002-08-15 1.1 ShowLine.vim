" Display the given line(s) from the current file in the command area (i.e.,
" echo), using that line's syntax highlighting (i.e., WYSIWYG).
"
" If no line number is given, display the current line.
"
" Regardless of how many line numbers are given, only the first &cmdheight
" lines are shown (i.e., don't cause scrolling, and a "more" message).
"
" $Header: /usr/home/gary/.vim/autoload/RCS/ShowLine.vim,v 1.1 2002/08/15 20:03:36 gary Exp $

function! ShowLine(...)
     " This makes sure we start (subsequent) echo's on the first line in the
     " command-line area.
     "
     echo ''

     let cmd        = ''
     let prev_group = ' x '	" Something that won't match any syntax group name.

     if a:0 == 0
	  call ShowLine(line("."))
	  return
     endif

     let argn = 1
     let show = 0
     while argn <= a:0 "{
	  if a:{argn} > 0
	       let show = show + 1
	  endif
	  let argn = argn + 1
     endwhile "}
     if &cmdheight < show && show <= 5
	  let &cmdheight = show
     endif

     let argn  = 1
     let shown = 0
     while argn <= a:0 "{
	  if a:{argn} > 0 "{
	       if shown > 0
		    let cmd = cmd . '\n'
	       endif

	       let shown  = shown + 1
	       let length = strlen(getline(a:{argn}))
	       let column = 1

	       if length == 0
		    let cmd = cmd . 'echon "'
	       endif

	       while column <= length "{
		    let group = synIDattr(synID(a:{argn}, column, 1), 'name')
		    if group != prev_group
			 if cmd != ''
			      let cmd = cmd . '"|'
			 endif
			 let cmd = cmd . 'echohl ' . (group == '' ? 'NONE' : group) . '|echon "'
			 let prev_group = group
		    endif
		    let char = strpart(getline(a:{argn}), column - 1, 1)
		    if char == '"' || char == "\\"
			 let char = '\' . char
		    endif
		    let cmd = cmd . char
		    let column = column + 1
	       endwhile "}

	       if shown == &cmdheight
		    break
	       endif
	  endif "}

	  let argn = argn + 1
     endwhile "}

     let cmd = cmd . '"|echohl NONE'
     "DEBUG call input('CMD='.cmd)
     exe cmd
endfunction
