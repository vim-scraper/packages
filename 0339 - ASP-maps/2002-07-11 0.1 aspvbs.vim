" Comma (,) prefixes a KEYWORD abbreviation
inoremap <buffer> ,r Response.
inoremap <buffer> ,R Request.
inoremap <buffer> ,s Server.
inoremap <buffer> ,b Buffer = 
inoremap <buffer> ,% <%<CR>%><Esc>O
inoremap <buffer> ,S Session("X")<Esc>FXs
inoremap <buffer> ,c CreateObject("X")<Esc>FXs
inoremap <buffer> ,d Redirect("X")<Esc>FXs
inoremap <buffer> ,w Write "X"<Esc>FXs
inoremap <buffer> ,f Form("X")<Esc>FXs
inoremap <buffer> ,q QueryString("X")<Esc>FXs


" Colon (:) prefixes a FLOW abbreviation

inoremap <buffer> :f For X<CR>Next<Esc>kFXs
inoremap <buffer> :n Next
inoremap <buffer> :i If X then<CR>End If<Esc>kFXs
inoremap <buffer> :e End If
inoremap <buffer> :s Select Case X<CR>End Select<Esc>kFXs
inoremap <buffer> :F Function X<CR>End Function<Esc>kFXs
inoremap <buffer> :S Sub X<CR>End Sub<Esc>kFXs

" Ctrl+f prefixes VBScript FUNCTIONS
inoremap <buffer> <C-f>a Array(X)<Esc>FXs
inoremap <buffer> <C-f>t Trim(X)<Esc>FXs
inoremap <buffer> <C-f>r Replace(X)<Esc>FXs
inoremap <buffer> <C-f>u UBound(X)<Esc>FXs
inoremap <buffer> <C-f>dD Date()
inoremap <buffer> <C-f>dt Time()
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

