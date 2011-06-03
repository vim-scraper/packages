" ASP.vim - version 1.0 - Macros and menus for ASP documents
"
" Copyright 2004 by TramTram www.sobotka.sk
"  Permission to copy this document or make derivative works is granted
"  without condition.
"
" Functions ASPCommentSelection and ASPUnCommentSelection were copied from 
" python.vim script and the comment string was changed. 
" Some macros were copied from aspvbs.vim and improved.
"
" | ASP menu:
" |------------------------------|
" |	Work with ASP comments.		 |		 
" | Vizual mode only.			 |	
" |------------------------------|
" | ASP Objects					 |
" | ...							 |
" | ...							 |
" |								 |
" |------------------------------|
" | ASP Functions                |
" | ...							 |
" | ...							 |
" |								 |
" |------------------------------|
" | ASP Control flow statements  |
" | ...							 |
" | ...							 |
" |								 |
" |------------------------------|
"
" Almost each keyword may be accessed by its short cut (and by the menu if using a gui)
" in each of 3 modes.  For most tags, these rules apply:
"
" Insert Mode:
" Inserts the keyword at the current cursor position and move cursor to place
" when is assumptioned your typing.
"
" Normal Mode:
" Inserts the keyword at the current cursor position.
"
" Visual Mode:
" For keywords having parenthesis, selected text is placed into the
" pharenthesis.
"
" Option Set autointend is strongly recommended.
"
" ***************************************************************************** 


" Cast Functions
map <F5>cb iCBool()<Esc>i
imap <F5>cb CBool()<Esc>i
vmap <F5>cb <Esc>`>a)<Esc>`<iCBool(
")
map <F5>cd iCDate()<Esc>i
imap <F5>cd CDate()<Esc>i
vmap <F5>cd <Esc>`>a)<Esc>`<iCDate(
")
map <F5>ci iCInt()<Esc>i
imap <F5>ci CInt()<Esc>i
vmap <F5>ci <Esc>`>a)<Esc>`<iCInt(
")
map <F5>cl iCLng()<Esc>i
imap <F5>cl CLng()<Esc>i
vmap <F5>cl <Esc>`>a)<Esc>`<iCLng(
")
map <F5>cs iCStr()<Esc>i
imap <F5>cS CSng()<Esc>i
vmap <F5>cS <Esc>`>a)<Esc>`<iCSng(
")
map <F5>cS iCSng()<Esc>i
imap <F5>cs CStr()<Esc>i
vmap <F5>cs <Esc>`>a)<Esc>`<iCStr(
")
map <F5>cB iCByte()<Esc>i
imap <F5>cB CByte()<Esc>i
vmap <F5>cB <Esc>`>a)<Esc>`<iCByte(
")
map <F5>cD iCDbl()<Esc>i
imap <F5>cD CDbl()<Esc>i
vmap <F5>cD <Esc>`>a)<Esc>`<iCDbl(
")


"Is type functions 
map <F5>ia iIsArray()<Esc>i
map <F5>id iIsDate()<Esc>i
map <F5>ie iIsEmpty()<Esc>i
map <F5>il iIsNull()<Esc>i
map <F5>in iIsNumeric()<Esc>i
map <F5>io iIsObject()<Esc>i

imap <F5>ia IsArray()<Esc>i
vmap <F5>ia <Esc>`>a)<Esc>`<iIsArray(
imap <F5>id IsDate()<Esc>i
vmap <F5>id <Esc>`>a)<Esc>`<iIsDate(
imap <F5>ie IsEmpty()<Esc>i
vmap <F5>ie <Esc>`>a)<Esc>`<iIsEmpty(
imap <F5>il IsNull()<Esc>i
vmap <F5>il <Esc>`>a)<Esc>`<iIsNull(
imap <F5>in IsNumeric()<Esc>i
vmap <F5>in <Esc>`>a)<Esc>`<iIsNumeric(
imap <F5>io IsObject()<Esc>i
vmap <F5>io <Esc>`>a)<Esc>`<iIsObject(
")


" Formatting Functions
map <F5>fmd iFormatDateTime()<Esc>i
map <F5>fmc iFormatCurrency()<Esc>i
map <F5>fmn iFormatNumber()<Esc>i
map <F5>fmp iFormatPercent()<Esc>i

imap <F5>fmd FormatDateTime()<Esc>i
vmap <F5>fmd <Esc>`>a)<Esc>`<iFormatDateTime(
imap <F5>fmc FormatCurrency()<Esc>i
vmap <F5>fmc <Esc>`>a)<Esc>`<iFormatCurrency(
imap <F5>fmn FormatNumber()<Esc>i
vmap <F5>fmn <Esc>`>a)<Esc>`<iFormatNumber(
imap <F5>fmp FormatPercent()<Esc>i
vmap <F5>fmp <Esc>`>a)<Esc>`<iFormatPercent(
")


" Date and Time related functions
map <F5>dd iDay()<Esc>i
map <F5>dm iMonth()<Esc>i
map <F5>dy iYear()<Esc>i
map <F5>da iDateAdd()<Esc>i
map <F5>df iDateDiff()<Esc>i
map <F5>dv iDateValue()<Esc>i
map <F5>dp iDatePart()<Esc>i
map <F5>ds iDateSerial()<Esc>i
map <F5>dh iHour()<Esc>i
map <F5>dM iMinute()<Esc>i
map <F5>dS iSecond()<Esc>i
")


" Date and Time related functions
imap <F5>dd Day()<Esc>i
vmap <F5>dd <Esc>`>a)<Esc>`<iDay(
imap <F5>dm Month()<Esc>i
vmap <F5>dm <Esc>`>a)<Esc>`<iMonth(
imap <F5>dy Year()<Esc>i
vmap <F5>dy <Esc>`>a)<Esc>`<iYear(
imap <F5>da DateAdd()<Esc>i
vmap <F5>da <Esc>`>a)<Esc>`<iDateAdd(
imap <F5>df DateDiff()<Esc>i
vmap <F5>df <Esc>`>a)<Esc>`<iDateDiff(
imap <F5>dv DateValue()<Esc>i
vmap <F5>dv <Esc>`>a)<Esc>`<iDateValue(
imap <F5>dp DatePart()<Esc>i
vmap <F5>dp <Esc>`>a)<Esc>`<iDatePart(
imap <F5>ds DateSerial()<Esc>i
vmap <F5>ds <Esc>`>a)<Esc>`<iDateSerial(
imap <F5>dh Hour()<Esc>i
vmap <F5>dh <Esc>`>a)<Esc>`<iHour(
imap <F5>dM Minute()<Esc>i
vmap <F5>dM <Esc>`>a)<Esc>`<iMinute(
imap <F5>dS Second()<Esc>i
vmap <F5>dS <Esc>`>a)<Esc>`<iSecond(
")


"Strings 
map <F5>t iTrim()<Esc>i
imap <F5>t Trim()<Esc>i
vmap <F5>t <Esc>`>a)<Esc>`<iTrim(

map <F5>l iLeft(,)<Esc>1hi
imap <F5>l Left(,)<Esc>1hi
vmap <F5>l <Esc>`>a)<Esc>`<iLeft(

map <F5>ri iRight(,)<Esc>1hi
imap <F5>ri Right(,)<Esc>1hi
vmap <F5>ri <Esc>`>a)<Esc>`<iRight(

map <F5>re iReplace(,,)<Esc>2hi
imap <F5>re Replace(,,)<Esc>2hi
vmap <F5>re <Esc>`>a)<Esc>`<iReplace(
")


"Arrays
map <F5>a iArray()<Esc>i
imap <F5>a Array()<Esc>i
vmap <F5>a <Esc>`>a)<Esc>`<iArray(

map <F5>u iUbound()<Esc>i
imap <F5>u Ubound()<Esc>i
vmap <F5>u <Esc>`>a)<Esc>`<iUbound(
")


"Flow control
map <F5>su iSub  <CR><CR><Home>End Sub<Esc>2ki
imap <F5>su Sub  <CR><CR><Home>End Sub<Esc>2ki
vmap <F5>su <Esc>`>a<Home>End Sub<Esc>`<iSub  <CR><Esc>i
map <F5>fn iFunction  <CR>	Dim<CR><CR><Home>End Function<Esc>3ki
imap <F5>fn Function  <CR>	Dim<CR><CR><Home>End Function<Esc>3ki
vmap <F5>fn <Esc>`>a<Home>End Function<Esc>`<iFunction  <CR>Dim<CR><CR><Esc>i
map <F5>fnh iFunction  <CR>	Dim html<CR><CR>=html<CR><Home>End Function<Esc>4ki
imap <F5>fnh Function  <CR>	Dim html<CR><CR>=html<CR><Home>End Function<Esc>4ki
map <F5>fns iFunction  <CR>	Dim sql<CR>	sql=""<CR><CR><Home>End Function<Esc>4ki
imap <F5>fns Function  <CR>	Dim sql<CR>	sql=""<CR><CR><Home>End Function<Esc>4ki
map <F5>if iIf  Then<CR><CR>End If<Esc>2k$FT1hi
imap <F5>if If  Then<CR><CR>End If<Esc>2k$FT1hi
vmap <F5>if <Esc>`>aEnd If<Esc>`<iIf  Then<CR><Esc>
map <F5>ife iIf  Then<CR><CR>Else<CR><CR>End If<Esc>4k$FT1hi
imap <F5>ife If  Then<CR><CR>Else<CR><CR>End If<Esc>4k$FT1hi
vmap <F5>ife <Esc>`>aElse<CR><CR>End If<Esc>`<iIf  Then<CR><Esc>i
map <F5>for iFor  =  To <CR><CR>Next<Esc>2k$F=1hi
imap <F5>for For  =  To <CR><CR>Next<Esc>2k$F=1hi
vmap <F5>for <Esc>`>aNext<Esc>`<iFor  =  To <CR><Esc>
map <F5>fore iFor Each  In <CR><CR>Next<Esc>2k$FI1hi
imap <F5>fore For Each  In <CR><CR>Next<Esc>2k$FI1hi
vmap <F5>fore <Esc>`>aNext<Esc>`<iFor Each  In <CR><Esc>i
map <F5>se iSelect Case  <CR>	Case <CR>	Case Else <CR>End Select<Esc>3k$i
imap <F5>se Select Case  <CR>	Case <CR>	Case Else <CR>End Select<Esc>3k$i
map <F5>wh iDo While ()<CR><CR>Loop<Esc>2k$i
imap <F5>wh Do While ()<CR><CR>Loop<Esc>2k$i
vmap <F5>wh <Esc>`>aLoop<Esc>`<iDo While ()<CR><Esc>
map <F5>do iDo<CR><CR>While ()<Esc>i
imap <F5>do Do<CR><CR>While ()<Esc>i
vmap <F5>do <Esc>`>aWhile ()<Esc>`<iDo<CR><Esc>i

"Application Object
"nnoremenu ASP.Application.Contents<Tab><F5>ac iApplication.Contents()= <Esc>i
map <F5>ac iApplication.Contents()= <Esc>i
imap <F5>ac Application.Contents()= <Esc>i
map <F5>ar iApplication.Contents.Remove <Esc>i
imap <F5>ar Application.Contents.Remove <Esc>i
map <F5>ara iApplication.Contents.RemoveAll()<Esc>i
imap <F5>ara Application.Contents.RemoveAll()<Esc>i
map <F5>al iApplication.Lock <Esc>i
imap <F5>al Application.Lock <Esc>i
map <F5>au iApplication.UnLock <Esc>i
imap <F5>au Application.UnLock <Esc>i

")
"Request Object	
map <F5>rc iRequest.Cookies()<Esc>i
imap <F5>rc Request.Cookies()<Esc>i
vmap <F5>rc <Esc>`>a)<Esc>`<iRequest.Cookies(<Esc>l
map <F5>rv iRequest.ServerVariables()<Esc>i
imap <F5>rv Request.ServerVariables()<Esc>i
vmap <F5>rv <Esc>`>a)<Esc>`<iRequest.ServerVariables(<Esc>l
map <F5>rs iRequest.QueryString()<Esc>i
imap <F5>rs Request.QueryString()<Esc>i
vmap <F5>rs <Esc>`>a)<Esc>`<iRequest.QueryString(<Esc>l
map <F5>rf iRequest.Form()<Esc>i
imap <F5>rf Request.Form()<Esc>i
vmap <F5>rf <Esc>`>a)<Esc>`<iRequest.Form(<Esc>l

")
"Response Object	
map <F5>pc iResponse.Cookies()<Esc>i
imap <F5>pc Response.Cookies()<Esc>i
vmap <F5>pc <Esc>`>a)<Esc>`<iResponse.Cookies(<Esc>l
map <F5>pcc iResponse.CacheControl()<Esc>i
imap <F5>pcc Response.CacheControl()<Esc>i
map <F5>pbw iResponse.BinaryWrite  <Esc>i
imap <F5>pbw Response.BinaryWrite  <Esc>i
map <F5>pb iResponse.Buffer=<Esc>i
imap <F5>pb Response.Buffer=<Esc>i
map <F5>pe iResponse.End<Esc>i
imap <F5>pe Response.End<Esc>i
map <F5>px iResponse.Expires= <Esc>i
imap <F5>px Response.Expires= <Esc>i
map <F5>pr iResponse.Redirect  <Esc>i
imap <F5>pr Response.Redirect  <Esc>i
map <F5>pw iResponse.Write  <Esc>i
imap <F5>pw Response.Write  <Esc>i 
map <F5>rw iResponse.Write  <Esc>i
imap <F5>rw Response.Write  <Esc>i 

")
"Server Object	
map <F5>sst iServer.ScriptTimeout= <Esc>i
imap <F5>sst Server.ScriptTimeout= <Esc>i
map <F5>sco iServer.CreateObject()<Esc>i
imap <F5>sco Server.CreateObject()<Esc>i
vmap <F5>sco <Esc>`>a)<Esc>`<iServer.CreateObject(<Esc>l
map <F5>se iServer.Execute()<Esc>i
imap <F5>se Server.Execute()<Esc>i
vmap <F5>se <Esc>`>a)<Esc>`<iServer.Execute(<Esc>l
map <F5>st iServer.Transfer()<Esc>i
imap <F5>st Server.Transfer()<Esc>i
vmap <F5>st <Esc>`>a)<Esc>`<iServer.Transfer(<Esc>l
map <F5>she iServer.HTMLEncode()<Esc>i
imap <F5>she Server.HTMLEncode()<Esc>i
vmap <F5>she <Esc>`>a)<Esc>`<iServer.HTMLEncode(<Esc>l
map <F5>sue iServer.URLEncode()<Esc>i
imap <F5>sue Server.URLEncode()<Esc>i
vmap <F5>sue <Esc>`>a)<Esc>`<iServer.URLEncode(<Esc>l
map <F5>smp iServer.MapPath()<Esc>i
imap <F5>smp Server.MapPath()<Esc>i
vmap <F5>smp <Esc>`>a)<Esc>`<iServer.MapPath(<Esc>l

")
"Session Object
map <F5>sc iSession.Contents()= <Esc>i
imap <F5>sc Session.Contents()= <Esc>i
map <F5>sr iSession.Contents.Remove <Esc>i
imap <F5>sr Session.Contents.Remove <Esc>i
map <F5>sra iSession.Contents.RemoveAll()<Esc>i
imap <F5>sra Session.Contents.RemoveAll()<Esc>i
map <F5>sa iSession.Abadon<Esc>i
imap <F5>sa Session.Abadon<Esc>i
map <F5>sto iSession.Timeout<Esc>i
imap <F5>sto Session.Timeout<Esc>i

"ASP comments
vmap <F5>co :call ASPCommentSelection()<CR>
vmap <F5>uc :call ASPUncommentSelection()<CR>

"GIU
if has("gui")
"Application Object
 vnoremenu ASP.Comment\ Selection<Tab><F5>co 
    \:call ASPCommentSelection()<CR>
 vnoremenu ASP.Uncomment\ Selection<Tab><F5>uc 
    \:call ASPUncommentSelection()<CR>

 :amenu ASP.-Sep0-	:
 
 nnoremenu ASP.Application.Contents<Tab><F5>ac iApplication.Contents()= <Esc>i
 inoremenu ASP.Application.Contents<Tab><F5>ac Application.Contents()= <Esc>i
 nnoremenu ASP.Application.ContentsRemove<Tab><F5>ar iApplication.Contents.Remove() <Esc>i
 inoremenu ASP.Application.ContentsRemove<Tab><F5>ar Application.Contents.Remove() <Esc>i
 nnoremenu ASP.Application.ContentsRemoveAll<Tab><F5>ara iApplication.Contents.RemoveAll()<Esc>i
 inoremenu ASP.Application.ContentsRemoveAll<Tab><F5>ara Application.Contents.RemoveAll()<Esc>i
 nnoremenu ASP.Application.Lock<Tab><F5>al iApplication.Lock <Esc>i
 inoremenu ASP.Application.Lock<Tab><F5>al Application.Lock <Esc>i
 nnoremenu ASP.Application.UnLock<Tab><F5>au iApplication.UnLock <Esc>i
 inoremenu ASP.Application.UnLock<Tab><F5>au Application.UnLock <Esc>i

"Request Object	
 nnoremenu ASP.Request.Cookies<Tab><F5>rc iRequest.Cookies()<Esc>i
 inoremenu ASP.Request.Cookies<Tab><F5>rc Request.Cookies()<Esc>i
 vnoremenu ASP.Request.Cookies<Tab><F5>rc <Esc>`>a)<Esc>`<iRequest.Cookies(<Esc>l
 nnoremenu ASP.Request.Form<Tab><F5>rf iRequest.Form()<Esc>i
 inoremenu ASP.Request.Form<Tab><F5>rf Request.Form()<Esc>i
 vnoremenu ASP.Request.Form<Tab><F5>rf <Esc>`>a)<Esc>`<iRequest.Form(<Esc>l
 nnoremenu ASP.Request.QueryString<Tab><F5>rs iRequest.QueryString()<Esc>i
 inoremenu ASP.Request.QueryString<Tab><F5>rs Request.QueryString()<Esc>i	  
 vnoremenu ASP.Request.QueryString<Tab><F5>rs <Esc>`>a)<Esc>`<iRequest.QueryString(<Esc>l
 nnoremenu ASP.Request.ServerVariables<Tab><F5>rv iRequest.ServerVariables()<Esc>i
 inoremenu ASP.Request.ServerVariables<Tab><F5>rv Request.ServerVariables()<Esc>i
 vnoremenu ASP.Request.ServerVariables<Tab><F5>rv <Esc>`>a)<Esc>`<iRequest.ServerVariables(<Esc>l

"Response Object
 nnoremenu ASP.Response.BinaryWrite<Tab><F5>pbw iResponse.BinaryWrite  <Esc>i
 inoremenu ASP.Response.BinaryWrite<Tab><F5>pbw Response.BinaryWrite  <Esc>i
 nnoremenu ASP.Response.Buffer<Tab><F5>pb iResponse.Buffer= <Esc>i
 inoremenu ASP.Response.Buffer<Tab><F5>pb Response.Buffer= <Esc>i
 nnoremenu ASP.Response.CacheControl<Tab><F5>pcc iResponse.CacheControl= <Esc>i
 inoremenu ASP.Response.CacheControl<Tab><F5>pcc Response.CacheControl= <Esc>i
 nnoremenu ASP.Response.Cookies<Tab><F5>pc iResponse.Cookies()<Esc>i
 inoremenu ASP.Response.Cookies<Tab><F5>pc Response.Cookies()<Esc>i
 vnoremenu ASP.Response.Cookies<Tab><F5>pc <Esc>`>a)<Esc>`<iResponse.Cookies(<Esc>l
 nnoremenu ASP.Response.End<Tab><F5>pe iResponse.End()<Esc>i
 inoremenu ASP.Response.End<Tab><F5>pe Response.End()<Esc>i
 nnoremenu ASP.Response.Expires<Tab><F5>px iResponse.Expires= <Esc>i
 inoremenu ASP.Response.Expires<Tab><F5>px Response.Expires= <Esc>i
 nnoremenu ASP.Response.Redirect<Tab><F5>pr iResponse.Redirect  <Esc>i
 inoremenu ASP.Response.Redirect<Tab><F5>pr Response.Redirect  <Esc>i
 nnoremenu ASP.Response.Write<Tab><F5>pw iResponse.Write()<Esc>i
 inoremenu ASP.Response.Write<Tab><F5>pw Response.Write()<Esc>i
")
"Session Object
 nnoremenu ASP.Session.Abadon<Tab><F5>sa iSession.Abadon<Esc>i
 inoremenu ASP.Session.Abadon<Tab><F5>sa Session.Abadon<Esc>i
 nnoremenu ASP.Session.Contents<Tab><F5>sc iSession.Contents()= <Esc>i
 inoremenu ASP.Session.Contents<Tab><F5>sc Session.Contents()= <Esc>i
 nnoremenu ASP.Session.ContentsRemove<Tab><F5>sr iSession.Contents.Remove() <Esc>i
 inoremenu ASP.Session.ContentsRemove<Tab><F5>sr Session.Contents.Remove() <Esc>i
 nnoremenu ASP.Session.ContentsRemoveAll<Tab><F5>sra iSession.Contents.RemoveAll()<Esc>i
 inoremenu ASP.Session.ContentsRemoveAll<Tab><F5>sra Session.Contents.RemoveAll()<Esc>i
 nnoremenu ASP.Session.Timeout<Tab><F5>sto iSession.Timeout<Esc>i
 inoremenu ASP.Session.Timeout<Tab><F5>sto Session.Timeout<Esc>i

") 
"Server Object
 nnoremenu ASP.Server.CreateObject<Tab><F5>sco iServer.CreateObject()<Esc>i
 inoremenu ASP.Server.CreateObject<Tab><F5>sco Server.CreateObject()<Esc>i
 vnoremenu ASP.Server.CreateObject<Tab><F5>sco <Esc>`>a)<Esc>`<iServer.CreateObject(<Esc>l
 nnoremenu ASP.Server.Execute<Tab><F5>se iServer.Execute()<Esc>i
 inoremenu ASP.Server.Execute<Tab><F5>se Server.Execute()<Esc>i
 vnoremenu ASP.Server.Execute<Tab><F5>se <Esc>`>a)<Esc>`<iServer.Execute(<Esc>l
 nnoremenu ASP.Server.HTMLEncode<Tab><F5>she iServer.HTMLEncode()<Esc>i
 inoremenu ASP.Server.HTMLEncode<Tab><F5>she Server.HTMLEncode()<Esc>i
 vnoremenu ASP.Server.HTMLEncode<Tab><F5>she <Esc>`>a)<Esc>`<iServer.HTMLEncode(<Esc>l
 nnoremenu ASP.Server.MapPath<Tab><F5>smp iServer.MapPath()<Esc>i
 inoremenu ASP.Server.MapPath<Tab><F5>smp Server.MapPath()<Esc>i
 vnoremenu ASP.Server.MapPath<Tab><F5>smp <Esc>`>a)<Esc>`<iServer.MapPath(<Esc>l
 nnoremenu ASP.Server.ScriptTimeout<Tab><F5>sst iServer.ScriptTimeout= <Esc>i
 inoremenu ASP.Server.ScriptTimeout<Tab><F5>sst Server.ScriptTimeout= <Esc>i
 nnoremenu ASP.Server.Transfer<Tab><F5>st iServer.Transfer()<Esc>i
 inoremenu ASP.Server.Transfer<Tab><F5>st Server.Transfer()<Esc>i
 vnoremenu ASP.Server.Transfer<Tab><F5>st <Esc>`>a)<Esc>`<iServer.Transfer(<Esc>l
 nnoremenu ASP.Server.URLEncode<Tab><F5>sue iServer.URLEncode()<Esc>i
 inoremenu ASP.Server.URLEncode<Tab><F5>sue Server.URLEncode()<Esc>i
 vnoremenu ASP.Server.URLEncode<Tab><F5>sue <Esc>`>a)<Esc>`<iServer.URLEncode(<Esc>l

:amenu ASP.-Sep1-	:
")
"Arrays
nnoremenu ASP.Array.Array<Tab><F5>a iArray()<Esc>i
inoremenu ASP.Array.Array<Tab><F5>a Array()<Esc>i
vnoremenu ASP.Array.Array<Tab><F5>a <Esc>`>a)<Esc>`<iArray(<Esc>l
nnoremenu ASP.Array.Ubound<Tab><F5>u iUbound()<Esc>i
inoremenu ASP.Array.Ubound<Tab><F5>u Ubound()<Esc>i
vnoremenu ASP.Array.Ubound<Tab><F5>u <Esc>`>a)<Esc>`<iUbound(<Esc>l
")
"Cast functions
nnoremenu ASP.Cast.CBool<Tab><F5>cb iCBool()<Esc>i
inoremenu ASP.Cast.CBool<Tab><F5>cb CBool()<Esc>i
vnoremenu ASP.Cast.CBool<Tab><F5>cb <Esc>`>a)<Esc>`<iCBool(<Esc>l
nnoremenu ASP.Cast.CDate<Tab><F5>cd iCDate()<Esc>i
inoremenu ASP.Cast.CDate<Tab><F5>cd CDate()<Esc>i
vnoremenu ASP.Cast.CDate<Tab><F5>cd <Esc>`>a)<Esc>`<iCDate(
nnoremenu ASP.Cast.CInt<Tab><F5>ci iCInt()<Esc>i
inoremenu ASP.Cast.CInt<Tab><F5>ci CInt()<Esc>i
vnoremenu ASP.Cast.CInt<Tab><F5>ci <Esc>`>a)<Esc>`<iCInt(<Esc>l
nnoremenu ASP.Cast.CLng<Tab><F5>cl iCLng()<Esc>i
inoremenu ASP.Cast.CLng<Tab><F5>cl CLng()<Esc>i
vnoremenu ASP.Cast.CLng<Tab><F5>cl <Esc>`>a)<Esc>`<iCLng(<Esc>l
nnoremenu ASP.Cast.CStr<Tab><F5>cs iCStr()<Esc>i
inoremenu ASP.Cast.CStr<Tab><F5>cs CStr()<Esc>i
vnoremenu ASP.Cast.CStr<Tab><F5>cs <Esc>`>a)<Esc>`<iCStr(<Esc>l
nnoremenu ASP.Cast.CSng<Tab><F5>cS iCSng()<Esc>i
inoremenu ASP.Cast.CSng<Tab><F5>cS CSng()<Esc>i
vnoremenu ASP.Cast.CSng<Tab><F5>cS <Esc>`>a)<Esc>`<iCSng(<Esc>l
nnoremenu ASP.Cast.CByte<Tab><F5>cB iCByte()<Esc>i
inoremenu ASP.Cast.CByte<Tab><F5>cB CByte()<Esc>i
vnoremenu ASP.Cast.CByte<Tab><F5>cB <Esc>`>a)<Esc>`<iCByte(<Esc>l
nnoremenu ASP.Cast.CDbl<Tab><F5>cD iCDbl()<Esc>i
inoremenu ASP.Cast.CDbl<Tab><F5>cD CDbl()<Esc>i
vnoremenu ASP.Cast.CDbl<Tab><F5>cD <Esc>`>a)<Esc>`<iCDbl(<Esc>l
")
" Date and Time related functions
nnoremenu ASP.DateTime.Day<Tab><F5>dd iDay()<Esc>i
inoremenu ASP.DateTime.Day<Tab><F5>dd Day()<Esc>i
vnoremenu ASP.DateTime.Day<Tab><F5>dd <Esc>`>a)<Esc>`<iDay(<Esc>l
nnoremenu ASP.DateTime.Month<Tab><F5>dm iMonth()<Esc>i
inoremenu ASP.DateTime.Month<Tab><F5>dm Month()<Esc>i
vnoremenu ASP.DateTime.Month<Tab><F5>dm <Esc>`>a)<Esc>`<iMonth(<Esc>l
nnoremenu ASP.DateTime.Year<Tab><F5>dy iYear()<Esc>i
inoremenu ASP.DateTime.Year<Tab><F5>dy Year()<Esc>i
vnoremenu ASP.DateTime.Year<Tab><F5>dy <Esc>`>a)<Esc>`<iYear(<Esc>l
nnoremenu ASP.DateTime.DateAdd<Tab><F5>da iDateAdd()<Esc>i
inoremenu ASP.DateTime.DateAdd<Tab><F5>da DateAdd()<Esc>i
vnoremenu ASP.DateTime.DateAdd<Tab><F5>da <Esc>`>a)<Esc>`<iDateAdd(<Esc>l
nnoremenu ASP.DateTime.DateDiff<Tab><F5>df iDateDiff()<Esc>i
inoremenu ASP.DateTime.DateDiff<Tab><F5>df DateDiff()<Esc>i
vnoremenu ASP.DateTime.DateDiff<Tab><F5>df <Esc>`>a)<Esc>`<iDateDiff(<Esc>l
nnoremenu ASP.DateTime.DateValue<Tab><F5>dv iDateValue()<Esc>i
inoremenu ASP.DateTime.DateValue<Tab><F5>dv DateValue()<Esc>i
vnoremenu ASP.DateTime.DateValue<Tab><F5>dv <Esc>`>a)<Esc>`<iDateValue(<Esc>l
nnoremenu ASP.DateTime.DatePart<Tab><F5>dp iDatePart()<Esc>i
inoremenu ASP.DateTime.DatePart<Tab><F5>dp DatePart()<Esc>i
vnoremenu ASP.DateTime.DatePart<Tab><F5>dp <Esc>`>a)<Esc>`<iDatePart(<Esc>l
nnoremenu ASP.DateTime.DateSerial<Tab><F5>ds iDateSerial()<Esc>i
inoremenu ASP.DateTime.DateSerial<Tab><F5>ds DateSerial()<Esc>i
vnoremenu ASP.DateTime.DateSerial<Tab><F5>ds <Esc>`>a)<Esc>`<iDateSerial(<Esc>l
nnoremenu ASP.DateTime.Hour<Tab><F5>dh iHour()<Esc>i
inoremenu ASP.DateTime.Hour<Tab><F5>dh Hour()<Esc>i
vnoremenu ASP.DateTime.Hour<Tab><F5>dh <Esc>`>a)<Esc>`<iHour(<Esc>l
nnoremenu ASP.DateTime.Minute<Tab><F5>dM iMinute()<Esc>i
inoremenu ASP.DateTime.Minute<Tab><F5>dM Minute()<Esc>i
vnoremenu ASP.DateTime.Minute<Tab><F5>dM <Esc>`>a)<Esc>`<iMinute(<Esc>l
nnoremenu ASP.DateTime.Second<Tab><F5>dS iSecond()<Esc>i
inoremenu ASP.DateTime.Second<Tab><F5>dS Second()<Esc>i
vnoremenu ASP.DateTime.Second<Tab><F5>dS <Esc>`>a)<Esc>`<iSecond(<Esc>l
")
" Formatting Functions
nnoremenu ASP.Format.FormatDateTime<Tab><F5>fmd iFormatDateTime()<Esc>i
inoremenu ASP.Format.FormatDateTime<Tab><F5>fmd FormatDateTime()<Esc>i
vnoremenu ASP.Format.FormatDateTime<Tab><F5>fmd <Esc>`>a)<Esc>`<iFormatDateTime(<Esc>l
nnoremenu ASP.Format.FormatCurrency<Tab><F5>fmc iFormatCurrency()<Esc>i
inoremenu ASP.Format.FormatCurrency<Tab><F5>fmc FormatCurrency()<Esc>i
vnoremenu ASP.Format.FormatCurrency<Tab><F5>fmc <Esc>`>a)<Esc>`<iFormatCurrency(<Esc>l
nnoremenu ASP.Format.FormatNumber<Tab><F5>fmn iFormatNumber()<Esc>i
inoremenu ASP.Format.FormatNumber<Tab><F5>fmn FormatNumber()<Esc>i
vnoremenu ASP.Format.FormatNumber<Tab><F5>fmn <Esc>`>a)<Esc>`<iFormatNumber(<Esc>l
nnoremenu ASP.Format.FormatPercent<Tab><F5>fmp iFormatPercent()<Esc>i
inoremenu ASP.Format.FormatPercent<Tab><F5>fmp FormatPercent()<Esc>i
vnoremenu ASP.Format.FormatPercent<Tab><F5>fmp <Esc>`>a)<Esc>`<iFormatPercent(<Esc>l

")
"Is type functions
nnoremenu ASP.IsType.IsArray<Tab><F5>ia iIsArray()<Esc>i
inoremenu ASP.IsType.IsArray<Tab><F5>ia IsArray()<Esc>i
vnoremenu ASP.IsType.IsArray<Tab><F5>ia <Esc>`>a)<Esc>`<iIsArray(<Esc>l
nnoremenu ASP.IsType.IsDate<Tab><F5>id iIsDate()<Esc>i
inoremenu ASP.IsType.IsDate<Tab><F5>id IsDate()<Esc>i
vnoremenu ASP.IsType.IsDate<Tab><F5>id <Esc>`>a)<Esc>`<iIsDate(<Esc>l
nnoremenu ASP.IsType.IsEmpty<Tab><F5>ie iIsEmpty()<Esc>i
inoremenu ASP.IsType.IsEmpty<Tab><F5>ie IsEmpty()<Esc>i
vnoremenu ASP.IsType.IsEmpty<Tab><F5>ie <Esc>`>a)<Esc>`<iIsEmpty(<Esc>l
nnoremenu ASP.IsType.IsNull<Tab><F5>il iIsNull()<Esc>i
inoremenu ASP.IsType.IsNull<Tab><F5>il IsNull()<Esc>i
vnoremenu ASP.IsType.IsNull<Tab><F5>il <Esc>`>a)<Esc>`<iIsNull(<Esc>l
nnoremenu ASP.IsType.IsNumeric<Tab><F5>in iIsNumeric()<Esc>i
inoremenu ASP.IsType.IsNumeric<Tab><F5>in IsNumeric()<Esc>i
vnoremenu ASP.IsType.IsNumeric<Tab><F5>in <Esc>`>a)<Esc>`<iIsNumeric(<Esc>l
nnoremenu ASP.IsType.IsObject<Tab><F5>io iIsObject()<Esc>i
inoremenu ASP.IsType.IsObject<Tab><F5>io IsObject()<Esc>i
vnoremenu ASP.IsType.IsObject<Tab><F5>io <Esc>`>a)<Esc>`<iIsObject(<Esc>l

")
"Strings 
nnoremenu ASP.String.Trim<Tab><F5>t iTrim()<Esc>i
inoremenu ASP.String.Trim<Tab><F5>t Trim()<Esc>i
vnoremenu ASP.String.Trim<Tab><F5>t <Esc>`>a)<Esc>`<iTrim(<Esc>l
nnoremenu ASP.String.Left<Tab><F5>l iLeft(,)<Esc>i
inoremenu ASP.String.Left<Tab><F5>l Left(,)<Esc>i
vnoremenu ASP.String.Left<Tab><F5>l <Esc>`>a)<Esc>`<iLeft(<Esc>l
nnoremenu ASP.String.Right<Tab><F5>ri iRight(,)<Esc>i
inoremenu ASP.String.Right<Tab><F5>ri Right(,)<Esc>i
vnoremenu ASP.String.Right<Tab><F5>ri <Esc>`>a)<Esc>`<iRight(<Esc>l
nnoremenu ASP.String.Replace<Tab><F5>re iReplace(,,)<Esc>i
inoremenu ASP.String.Replace<Tab><F5>re Replace(,,)<Esc>i
vnoremenu ASP.String.Replace<Tab><F5>re <Esc>`>a)<Esc>`<iReplace(<Esc>l

:amenu ASP.-Sep2-	:

")
"Keywords 
 nnoremenu ASP.Do<Tab><F5>do iDo<CR><CR>While ()<Esc>i
 inoremenu ASP.Do<Tab><F5>do Do<CR><CR>While ()<Esc>i
 vnoremenu ASP.Do<Tab><F5>do <Esc>`>aWhile ()<Esc>`<iDo<CR><Esc>i
 nnoremenu ASP.For<Tab><F5>for iFor  =  To <CR><CR>Next<Esc>2k$F=1hi
 inoremenu ASP.For<Tab><F5>for For  =  To <CR><CR>Next<Esc>2k$F=1hi
 vnoremenu ASP.For<Tab><F5>for <Esc>`>aNext<Esc>`<iFor  =  To <CR><Esc>2k$F=1hi
 nnoremenu ASP.ForEach<Tab><F5>fore iFor Each  In <CR><CR>Next<Esc>2k$FI1hi
 inoremenu ASP.ForEach<Tab><F5>fore For Each  In <CR><CR>Next<Esc>2k$FI1hi
 vnoremenu ASP.ForEach<Tab><F5>fore <Esc>`>aNext<Esc>`<iFor Each  In <CR><Esc>i
 nnoremenu ASP.Function<Tab><F5>fn iFunction  <CR>	Dim<CR><CR><Home>End Function<Esc>3ki
 inoremenu ASP.Function<Tab><F5>fn Function  <CR>	Dim<CR><CR><Home>End Function<Esc>3ki
 vnoremenu ASP.Function<Tab><F5>fn <Esc>`>a<Home>End Function<Esc>`<iFunction  <CR>Dim<CR><CR><Esc>i
 nnoremenu ASP.If<Tab><F5>if iIf  Then<CR><CR>End If<Esc>2k$FT1hi
 inoremenu ASP.If<Tab><F5>if If  Then<CR><CR>End If<Esc>2k$FT1hi
 vnoremenu ASP.If<Tab><F5>if <Esc>`>aEnd If<Esc>`<iIf  Then<CR><Esc>2k$FT1hi
 nnoremenu ASP.IfElse<Tab><F5>ife iIf  Then<CR><CR>Else<CR><CR>End If<Esc>4k$FT1hi
 inoremenu ASP.IfElse<Tab><F5>ife If  Then<CR><CR>Else<CR><CR>End If<Esc>4k$FT1hi
 vnoremenu ASP.IfElse<Tab><F5>ife <Esc>`>aElse<CR><CR>End If<Esc>`<iIf  Then<CR><Esc>i
 nnoremenu ASP.Select<Tab><F5>se iSelect Case  <CR>	Case <CR>	Case Else <CR>End Select<Esc>3k$i
 inoremenu ASP.Select<Tab><F5>se Select Case  <CR>	Case <CR>	Case Else <CR>End Select<Esc>3k$i
 nnoremenu ASP.Sub<Tab><F5>su iSub  <CR><CR><Home>End Sub<Esc>2ki
 inoremenu ASP.Sub<Tab><F5>su Sub  <CR><CR><Home>End Sub<Esc>2ki
 vnoremenu ASP.Sub<Tab><F5>su <Esc>`>a<Home>End Sub<Esc>`<iSub  <CR><Esc>i
 nnoremenu ASP.While<Tab><F5>wh iDo While ()<CR><CR>Loop<Esc>2k$i
 inoremenu ASP.While<Tab><F5>wh Do While ()<CR><CR>Loop<Esc>2k$i
 vnoremenu ASP.While<Tab><F5>wh <Esc>`>aLoop<Esc>`<iDo While ()<CR><Esc>2k$i
endif

" Comment out selected lines
" commentString is inserted in non-empty lines, and should be aligned with
" the block
function! ASPCommentSelection()  range
  let commentString = "'"
  let cl = a:firstline
  let ind = 1000    " I hope nobody use so long lines! :)

  " Look for smallest indent
  while (cl <= a:lastline)
    if strlen(getline(cl))
      let cind = indent(cl)
      let ind = ((ind < cind) ? ind : cind)
    endif
    let cl = cl + 1
  endwhile
  if (ind == 1000)
    let ind = 1
  else
    let ind = ind + 1
  endif

  let cl = a:firstline
  execute ":".cl
  " Insert commentString in each non-empty line, in column ind
  while (cl <= a:lastline)
    if strlen(getline(cl))
      execute "normal ".ind."|i".commentString
    endif
    execute "normal \<Down>"
    let cl = cl + 1
  endwhile
endfunction

" Uncomment selected lines
function! ASPUncommentSelection()  range
  " commentString could be different than the one from CommentSelection()
  " For example, this could be "# \\="
  let commentString = "'"
  let cl = a:firstline
  while (cl <= a:lastline)
    let ul = substitute(getline(cl),
             \"\\(\\s*\\)".commentString."\\(.*\\)$", "\\1\\2", "")
    call setline(cl, ul)
    let cl = cl + 1
  endwhile
endfunction

