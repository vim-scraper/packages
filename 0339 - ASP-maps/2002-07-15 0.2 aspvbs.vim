" Comma (,) prefixes a ASP Objects, properties 
" and methods abbreviation
inoremap <buffer> ,% <%<CR>%><Esc>O
inoremap <buffer> ,r Response.
inoremap <buffer> ,R Request.
inoremap <buffer> ,s Server.
inoremap <buffer> ,b Buffer = 
inoremap <buffer> ,S Session("X")<Esc>FXs
inoremap <buffer> ,c CreateObject("X")<Esc>FXs
inoremap <buffer> ,d Redirect("X")<Esc>FXs
inoremap <buffer> ,w Write "X"<Esc>FXs
inoremap <buffer> ,f Form("X")<Esc>FXs
inoremap <buffer> ,q QueryString("X")<Esc>FXs


" Colon (:) prefixes a FLOW abbreviation

inoremap <buffer> :f For <CR>Next<Esc>kA
inoremap <buffer> :n Next
inoremap <buffer> :i If X then<CR>End If<Esc>kFXs
inoremap <buffer> :e End If
inoremap <buffer> :s Select Case <CR>End Select<Esc>kA
inoremap <buffer> :F Function <CR>End Function<Esc>kA
inoremap <buffer> :S Sub <CR>End Sub<Esc>kA

" Ctrl+f prefixes VBScript FUNCTIONS
inoremap <buffer> <C-f>a Array(X)<Esc>FXs
inoremap <buffer> <C-f>t Trim(X)<Esc>FXs
inoremap <buffer> <C-f>r Replace(X)<Esc>FXs
inoremap <buffer> <C-f>u UBound(X)<Esc>FXs
inoremap <buffer> <C-f>dd Day(X)<Esc>FXs
inoremap <buffer> <C-f>dm Month(X)<Esc>FXs
inoremap <buffer> <C-f>dy Year(X)<Esc>FXs
inoremap <buffer> <C-f>da DateAdd(X)<Esc>FXs
inoremap <buffer> <C-f>df DateDiff(X)<Esc>FXs
inoremap <buffer> <C-f>fd FormatDateTime(X)<Esc>FXs
inoremap <buffer> <C-f>fn FormatNumber(X)<Esc>FXs
inoremap <buffer> <C-f>ia IsArray(X)<Esc>FXs
inoremap <buffer> <C-f>id IsDate(X)<Esc>FXs
inoremap <buffer> <C-f>in IsNumeric(X)<Esc>FXs
inoremap <buffer> <C-f>cd CDate(X)<Esc>FXs
inoremap <buffer> <C-f>ci CInt(X)<Esc>FXs
inoremap <buffer> <C-f>cb CBool(X)<Esc>FXs

" Ctrl+f prefixes and wraps VBScript FUNCTIONS 
" around visually selected text
vnoremap <buffer> <C-f>t sTrim()<Esc>P
vnoremap <buffer> <C-f>r sReplace()<Esc>P 
vnoremap <buffer> <C-f>u sUBound()<Esc>P 
vnoremap <buffer> <C-f>dd sDay()<Esc>P
vnoremap <buffer> <C-f>dm sMonth()<Esc>P
vnoremap <buffer> <C-f>dy sYear()<Esc>P
vnoremap <buffer> <C-f>da sDateAdd()<Esc>P
vnoremap <buffer> <C-f>df sDateDiff()<Esc>P
vnoremap <buffer> <C-f>fd sFormatDateTime()<Esc>P
vnoremap <buffer> <C-f>fn sFormatNumber()<Esc>P
vnoremap <buffer> <C-f>ia sIsArray()<Esc>P
vnoremap <buffer> <C-f>id sIsDate()<Esc>P
vnoremap <buffer> <C-f>in sIsNumeric()<Esc>P
vnoremap <buffer> <C-f>cd sCDate()<Esc>P
vnoremap <buffer> <C-f>ci sCInt()<Esc>P
vnoremap <buffer> <C-f>cb sCBool()<Esc>P

