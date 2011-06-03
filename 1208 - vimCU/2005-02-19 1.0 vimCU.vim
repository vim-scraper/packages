"***********************************************************************

  " Comment / UnComment
  "
  " Comment differently single line comments and multiple line comments.
  " Strings that begins and ends comments are contained in variables:
  " b:beginOfCommentSingle, b:endOfCommentSingle, b:beginOfCommentMulti,
  " b:endOfCommentMulti; these variables have to be defined in order to
  " use Comment and UnComment. The variable b:CommentType specify which
  " comment are available, this variable contains one or more of the
  " following flag 's', 'm'; 's' means that single line comments are
  " available, 'm' means that multiple line comments are available. If
  " 'm' is not set, multiple line comments are threaded line by line as
  " a single comment.

function! SingleLineComment(lineno)
  let line = getline(a:lineno)
  let line = substitute(line, "^", b:beginOfCommentSingle, "")
  let line = substitute(line, "$", b:endOfCommentSingle, "")
  call setline(a:lineno, line)
endfunction

function! MultiLineComment(line1, line2)
  if match(b:CommentType, "m") == -1  " multilne comments disabled
    let i = a:line1
    while i <= a:line2  " comment each line by itself
      call SingleLineComment(i)
      let i = i+1
    endwhile
  else  " multiline comments enabled
    call append(a:line2, b:endOfCommentMulti)
    call append(a:line1 - 1, b:beginOfCommentMulti)
  endif
endfunction

function! SingleLineUnComment(lineno)
  let line = getline(a:lineno)
  let line = substitute(line, "^".escape(b:beginOfCommentSingle, "*"), "", "")
  let line = substitute(line, escape(b:endOfCommentSingle, "*")."$", "", "")
  call setline(a:lineno, line)
endfunction

function! MultiLineUnComment(line1, line2)
  if match(b:CommentType, "m") == -1  " multilne comments disabled
    let i = a:line1
    while i <= a:line2  " uncomment each line by itself
      call SingleLineUnComment(i)
      let i = i+1
    endwhile
  else  " multiline comments enabled
    execute a:line2."d"
    execute a:line1."d"
  endif
endfunction

function! Comment(line1, line2)   " comment part of text
  if (a:line2 - a:line1) == 0 " single line comment
    call SingleLineComment(a:line1)
  else  " multi line comment
    call MultiLineComment(a:line1, a:line2)
  endif
endfunction

function! UnComment(line1, line2)   " uncomment part of text
  if (a:line2 - a:line1) == 0 " single line comment
    call SingleLineUnComment(a:line1)
  else  " multi line comment
    call MultiLineUnComment(a:line1, a:line2)
  endif
endfunction

    " assign function to user defined command Commend and UnComment
command! -range Comment call Comment(<line1>, <line2>)
command! -range UnComment call UnComment(<line1>, <line2>)

map ,# :Comment<RETURN>     " comment lines, see below
map ,3 :UnComment<RETURN>   " uncomment lines, see below

  " default settings
let b:CommentType = "s"   " only single line comments enabled by default
let b:beginOfCommentSingle = "# "
let b:endOfCommentSingle = ""

