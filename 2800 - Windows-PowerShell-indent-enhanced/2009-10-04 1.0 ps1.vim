" Vim indent file
" Language:	PowerShell
" Maintainer:	Lior Elia - http://blogs.msdn.com/lior
" Last Change:	2009 Oct 4

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal cindent cinoptions& cinoptions+=+0

" Set the function to do the work.
setlocal indentexpr=GetPsIndent()

let b:undo_indent = "set cin< cino< indentkeys< indentexpr<"

" Only define the function once.
if exists("*GetPsIndent")
  finish
endif

function! SkipPsComments(startline)
  let lnum = a:startline
  while lnum > 1
    let lnum = prevnonblank(lnum)
    if getline(lnum) =~ '^\s*#'
      let lnum = lnum - 1
    else
      break
    endif
  endwhile
  return lnum
endfunction

function GetPsIndent()

  " PowerShell is almost like C; use the built-in C indenting and then correct the comment/precompiler duality.
  let theIndent = cindent(v:lnum)

  " comment lines are mishandled as precompiler directives ('#') so fix that.
  if getline(v:lnum) =~ '^\s*#'
    let v:lnum = SkipPsComments(v:lnum-1)
    let theIndent = cindent(v:lnum)
    if getline(v:lnum) =~ '{$'
      " indent once more, because we're in a beginning of a block
      let theIndent = theIndent + &sw
    endif
  endif

  return theIndent
endfunction

