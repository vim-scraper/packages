" HTML Commands.
" Author: Michael Geddes <michaelrgeddes@optushome.com.au>
" Version: 2.2
" Please feel free to use and modify all or part of this script.
" I would appreciate being acknowledged in any derived scripts, and would 
" appreciate any updates/modifications.

" Define htmlcmd_NOHEADER to not include <m-0> style
" Define htmlcmd_BF for some of Benji's commands that haven't been adopted quite yet.
" Define htmlcmd_NOAUTOCLOSE to not map > to auto-close open tags.
" &usersign is your username for modification

" This code allows htmlcmd to behave as a FT plugin and as a buffoptions.vim
" style plugin.
if !exists('DoingSOURCE')
  if exists("b:did_ftplugin")
	finish
  endif
  if exists('*ReadFileTypeMap')
	SO <sfile>
	finish
  endif
  " Else we assume we are behaving as a plugin
endif

function! s:HTMLCloseAll( line1, column1, line2, column2, cline,ccolumn)
  if g:htmlCmd_debug| call input( a:line1.':'.a:column1.':'.a:line2.':'.a:column2) |endif
  let ic=&ic
  set ic
  let cursor=-1
  let foundcursor=0
  if(a:line1==a:line2)
	let txt=strpart(getline(a:line1),a:column1, a:column2-a:column1)
	if a:line1==a:cline
	  let cursor=a:ccolumn-a:column1
	  let foundcursor=1
	endif
  else
	let txt=strpart(getline(a:line1),a:column1,65535)
	let here=a:line1+1
	let cursor=0
	while(here < a:line2)
	  let ll=getline(here)
	  if !foundcursor
		if here==a:cline
		  let foundcursor=1
		  let cursor=cursor+a:ccolumn
		else
		  let cursor=cursor+strlen(ll)
		endif
	  endif
	  let txt=txt.ll
	  let here=here+1
	endwhile
	let txt=txt.strpart(getline(a:line2),0,a:column2)
  endif
  
  let pastcursor=0
  let stillopen=''
  let badlist=''
  let mx='<\s*\(/\=\)\(\k\+\)\>[^>]\{-}\(/\=\)>'
"  if g:htmlCmd_debug| call input( txt) |endif
  let indextotal =0
  while txt != ""
	let index=match(txt,mx)
	if index < 0
	  break
	endif
	let match=matchstr(txt,mx)
	if g:htmlCmd_debug| echo '-----'.'^.\{'.index.'}'.mx |endif
	let lenpart=index+strlen(match)
	let indextotal=indextotal+lenpart

	let txt=strpart(txt,lenpart,65535)
	if foundcursor && !pastcursor && indextotal >= cursor
		let pastcursor=1
		let openbeforecursor=stillopen
	endif
	let type=substitute(match,mx,'\1','') 
	let seq=substitute(match,mx,'\2','') 
	let endtype=substitute(match,mx,'\3','') 
	if endtype==''
	  if g:htmlCmd_debug| echo stillopen  |endif
	  if type=='/'
		if stillopen !~? '\<'.seq.'\>,'
		  let badlist=badlist.'</'.seq.'>,'
		  "break
		elseif pastcursor && openbeforecursor ==? stillopen
		  if g:htmlCmd_debug|echo 'END:'.seq.':'.stillopen|endif
		  let stillopen=substitute(stillopen,'\c\<'.seq.'\>,.\{-}$','','i')
		  break
		else
		  let my='\(^.\{-}\)\<'.seq.'\>,'
		  let badopen=substitute(matchstr(stillopen,my),my,'\1','')
		  let badlist=badlist.substitute(badopen,'\<\k\+\>','<&>','g')
		  let stillopen=substitute(stillopen,my,'','i')
		endif
	  else
		let stillopen=seq.','.stillopen
	  endif
	endif
  endwhile
  let stillopen=substitute(stillopen,'\<\k\+\>','</&>','g')
  let stillopen=substitute(stillopen,',','','g')
  
  if badlist!=''
	echo "Badly formed tags :".substitute(badlist,',$','','')
  endif

  let &ic=ic
  if g:htmlCmd_debug| call input(stillopen ) |endif
  return stillopen
endfun

if !exists("g:htmlCmd_debug")
  let g:htmlCmd_debug=0
endif

function! s:FindBack(option)
  return s:Find__Back(a:option,1)
endfun

function! s:Find__Fwd()
  let mx='<\s*\(/\=\)\(\k\+\)\>[^>]*>'

  let curline=line('.')
  let curcol=col('.')
  let text=getline(curline)
  let text=strpart(text,curcol,65535)
  while 1
	
  endwhile

endfun

function! s:Find__Back(option, insertit)
  let ws=&ws
  let ic=&ic
  let sc=&sc
  set nows
  set ic
  set nosc

  let stillopen=""
  let find='<\(/\=\)'.a:option.'\>[^>]*>'
  let here=line('.')
  let text=strpart(getline('.'),0,col('.')+1)
  let end=""
  let closed=""
  while 1
	let t=''
	let i=matchend(text,find)
	if g:htmlCmd_debug | call input(text.':'.i) |endif
	let column=0
	let str=0
	while i >= 0 
	  let str=match(text,find)
	  if g:htmlCmd_debug | call input(column.':'.text.':'.str) |endif
	  let t=matchstr(text,find)
	  let text=strpart(text,i,1024)
	  let i=matchend(text,find)
	  if i >=0 
		let column=column+str
	  endif
	endwhile
	let column=column+str
	if g:htmlCmd_debug| call input('<'.str.'>'.column) |endif
	  
	if t != ''
	  let t=substitute(t,find,'\1','')
	  if t != '/'
		let end='/'
		if ! a:insertit
		  let column = column+2 
		endif
		let closed=s:HTMLCloseAll(here, column, line('.'),col('.')+1,0,0)
	  endif
	  break
	endif
	let here=here-1
	if here<0 
	  break
	endif
	let text=getline(here)
  endwhile

  if end=="" && a:insertit
	let openit='<'.a:option.'>'
  else
	let openit=""
  endif
  let &sc=sc
  let &ic=ic
  let &ws=ws
  return closed.openit
endfun


if has('menu')
	amenu H&TML.GenerateTOC :call GenerateTOC()<cr>
endif
function! GenerateTOC(...)
  let types=""
  if a:0 > 0 
	let types=a:1
  endif
  let line=1
  let found=0
  let TOC=0
  let end=line('$')
  while line<end
	if getline(line)=~'<!--TOC-->'
	  let TOC=line
	  if getline(line+1) !~ "<!--TOC-->"
		exe (line+1).',/<!--ENDTOC-->/-1 d'
	  endif
	  let found=1
	  break
	elseif getline(line) =~ 'Table [oO]f Contents'
	  let TOC=line
	endif
	let line=line+1
  endwhile
  if TOC ==0
	exe "norm /\\C<BODY\<CR>/>\<CR>a\<CR><h1>Table of Contents</h1>\<cr>\<ESC>"
	let TOC=line('.')-1
  endif
  if !found
	if g:htmlCmd_debug| echo '------' |endif
	call append(TOC,"<!--TOC-->")
	let TOC=TOC+1
	call append(TOC,"<!--ENDTOC-->")
	let end=end+2
  endif
  let line=TOC
  let sTOC=""
  let firstlevel=0
  let lastlevel=0
  while line < end
	let hx='\(<[hH]\([1-5]\)>\)\s*\(.\{-}\)\(</[hH]\2\)'
	let here=getline(line)
	let part=matchstr(here,hx)
	if part != ""
	  let lvl=substitute(part,hx,'\2','')
	  let txt=substitute(part,hx,'\3','')
	  let xrf='<[aA] name=\s*\([^>]*\)>\(.\{-}\)</a>'
	  if txt!~xrf
		let refname=substitute(substitute(txt,'\W','','g'),'<[^>]*>','','g')
"		let refname=substitute(refname,'&\k\+\>;\=','','g')
		call setline(line,substitute(here,hx,'\1<a name='.refname.'>'.escape(txt,'\&' ) . '</a>\4',''))
	  else
		let refname=substitute(matchstr(txt,xrf),xrf,'\1','')
		let txt=substitute(matchstr(txt,xrf),xrf,'\2','')
	  endif 
	  let ty=types[lvl-1]
	  if ty!=""
		let ty=' type="'.ty.'"'
	  endif
	  if lastlevel==0
		let firstlevel=lvl-1
		let lastlevel=lvl
		let sTOC=sTOC."\<cr><ul".ty.">"
	  elseif lvl <= firstlevel
		break
	  elseif lastlevel < lvl
		while lastlevel < lvl
		  let lastlevel=lastlevel+1
		  let sTOC=sTOC."\<cr><ul".ty.">"
		endwhile
	  elseif lastlevel > lvl
		while lastlevel > lvl
		  let sTOC=sTOC."\<cr></ul>"
		  let lastlevel=lastlevel-1
		endwhile
	  endif
	  let sTOC=sTOC."\<cr>".'<li><a href="#'.refname.'">'.txt.'</a></li>'
	endif
	let line=line+1
  endwhile
  while lastlevel > firstlevel
	let sTOC=sTOC."\<cr></ul>"
	let lastlevel=lastlevel-1
  endwhile
  exe TOC
  exe 'norm o'.sTOC."\<esc>"
endfun

if v:version < 600
fun! s:Surround(begin,end) range
  exe "norm `>a".a:end."\<esc>"
  exe "norm `<i".a:begin."\<esc>"
endfun
else
fun! s:Surround(begin,end) range
  exe "norm `>a".a:end."\<esc>`<i".a:begin."\<esc>"
endfun
endif
fun! s:SurroundHTM(tag) range
  call s:Surround('<'.a:tag.'>','</'.a:tag.'>')
endfun


"FileTypes: html xml
vmap <buffer> <s-â> <m-s-b>

imap <buffer> <m-s-b> <c-r>=<SID>FindBack('big')<CR>
imap <buffer> <m-b> <c-r>=<SID>FindBack('b')<CR>
imap <buffer> <m-m> <c-r>=<SID>FindBack('i')<CR>
imap <buffer> <m-u> <c-r>=<SID>FindBack('u')<CR>
imap <buffer> <m-p> <c-r>=<SID>FindBack('p')<CR>
imap <buffer> <m-k> <c-r>=<SID>FindBack('kbd')<CR>
imap <buffer> <m-d> <c-r>=<SID>FindBack('td')<cr>
vmap <buffer> <m-s-b> :call <SID>SurroundHTM('big')<CR>
vmap <buffer> <m-b> :call <SID>SurroundHTM('b')<CR>
vmap <buffer> <m-m> :call <SID>SurroundHTM('i')<CR>
vmap <buffer> <m-u> :call <SID>SurroundHTM('u')<CR>
vmap <buffer> <m-p> :call <SID>SurroundHTM('p')<CR>
vmap <buffer> <m-k> :call <SID>SurroundHTM('kbd')<CR>
vmap <buffer> <m-d> :call <SID>SurroundHTM('td')<CR>
nmap <buffer> <m-s-b> i<m-s-b>
nmap <buffer> <m-b> i<m-b>
nmap <buffer> <m-m> i<m-m>
nmap <buffer> <m-u> i<m-u>
nmap <buffer> <m-p> i<m-p>
"nmap<buffer>  <m-k> i<m-k>
nmap <buffer> <m-d> i<m-d>

imap <buffer> <m-8> <li></li><esc>F<i
nmap <buffer> <m-8> i<m-8>
vmap <buffer> <m-8> :call <SID>SurroundHTM('li')<cr>

imap <buffer> <m-.> <ol><CR></ol><esc>F<O<TAB>
nmap <buffer> <m-.> o<m-.>
imap <buffer> <m-,> <lt>ul><cr></ul><esc>F<O<TAB>
nmap <buffer> <m-,> o<m-,>

if !exists('htmlcmd_NOHEADER')
imap <buffer> <m-0> <c-r>=substitute(g_html_htmlheader,'%VERSION%',strpart(v:version,0,1).'.'.strpart(v:version,1,1),'')<cr>
endif
vmap <buffer> <m-1> :call <SID>SurroundHTM('h1')<cr>
vmap <buffer> <m-2> :call <SID>SurroundHTM('h2')<cr>
vmap <buffer> <m-3> :call <SID>SurroundHTM('h3')<cr>
vmap <buffer> <m-4> :call <SID>SurroundHTM('h4')<cr>
vmap <buffer> <m-5> :call <SID>SurroundHTM('h5')<cr>
vmap <buffer> <m-6> :call <SID>SurroundHTM('h6')<cr>
imap <buffer> <m-1> <c-r>=<SID>FindBack('h1')<cr>
imap <buffer> <m-2> <c-r>=<SID>FindBack('h2')<cr>
imap <buffer> <m-3> <c-r>=<SID>FindBack('h3')<cr>
imap <buffer> <m-4> <c-r>=<SID>FindBack('h4')<cr>
imap <buffer> <m-5> <c-r>=<SID>FindBack('h5')<cr>
imap <buffer> <m-6> <c-r>=<SID>FindBack('h6')<cr>
nmap <buffer> <m-1> i<m-1>
nmap <buffer> <m-2> i<m-2>
nmap <buffer> <m-3> i<m-3>
nmap <buffer> <m-4> i<m-4>
nmap <buffer> <m-5> i<m-5>
nmap <buffer> <m-6> i<m-6>

imap <buffer> <c-cr> <esc>o
imap <buffer> <s-cr> <lt>br/><CR>
imap <buffer> <m--> <lt>hr/>

imap <buffer> &<space> &nbsp;
imap <buffer> && &amp;
imap <buffer> &< &lt;
imap <buffer> &> &gt;
if 0
imap <buffer> &'< &lsquo;
imap <buffer> &'> &rsquo;
imap <buffer> &"< &ldquo;
imap <buffer> &"> &rdquo;
endif

fun! s:HTMLQuote( type)
  if col('.') ==1 || getline('.')[col('.')-2] =~'\s' 
	  return '&l'.a:type.';'
  else
	  return '&r'.a:type.';'
  endif  
endfun
imap <buffer> &' <c-r>=<SID>HTMLQuote('squo')<CR>
imap <buffer> &" <c-r>=<SID>HTMLQuote('dquo')<CR>

"fun! HTMLFindQuote( quotename )
"  if a:quotename = "'"
"	  let q='squo'
"  elseif a:quotename ='"'
"	  let q='dquo'
"  else
"	return "no"
"  endif
"  let oldline=line('.')
"  let line=search( '&\([lr]\)'.q, 'bW' ) > 0
"  if line>0
"	let type = substitu
"  let type=
"  endif
"endfun

"imap <m-'>

if exists("htmlcmd_BF")
  imap <buffer> <m-<> &lt;
  imap <buffer> <m->> &gt;
  imap <buffer> <m-&> &amp;
  imap <buffer> <m-l> <a href=""><Left><Left>
  imap <buffer> <m-n> <a name=""><Left><Left>
endif

" Add a html link to the location stored in the windows clipboard
" (external link)                                                               
" Add internal link to loc. in reg 0

nmap ,al "='<a href="'.@*.'" target="_top"></a>'<cr>p
nmap ,aL "='<li><a href="'.@*.'" target="_top"></a></li>'<cr>p
nmap ,l "='<a href="#'.@".'"></a>'<cr>p
"nmap ,yf :let @*='file:///'.expand('%:p:gs+\+/+')<cr>
nmap ,yf :let @*=<SID>URLPathOf(expand('%:p'))<cr>

fun! s:URLPathOf( filename)
  let file=s:UNCPathOf(a:filename)
  if file =~ '^//'
	  return 'file:'.file
  else
	  return 'file:///'.file
  endif
endfun
if has('win32')
  fun! s:UNCPathOf( filename )
	let shares=system('net share')
	let shares=substitute(shares,"\n[^ \n]\\+[$]\\s[^\n]\\+","",'g')
	let newname=''
	let current=fnamemodify(a:filename,':gs+/+\\+')
	let do_it=1
	while do_it
	  let tx=fnamemodify(current, ':h')
	  if tx==current
		let do_it=0
	  else
		let current=tx
		let mx= '\c'."\n".'\([^ 	'."\n".']\+\)\s\+'.escape(current,'$\.&').'\s\+'
		let match=matchstr(shares,mx)
		if match != ""
		  let sharename=substitute(match,mx,'\1','')
		  let drivepart=substitute(strpart(a:filename, strlen(current)),'\\','/','g')
		  if drivepart!~ '^/'
			let drivepart='/'.drivepart
		  endif
		  return '//'.$COMPUTERNAME.'/'.sharename.drivepart
		endif
	  endif
	endwhile
	return a:filename
  endfun
else
  fun! s:UNCPathOf( filename )
	return filename
  endfun
endif

fun! s:EndTag()
	let l=line(".")
	let c=col(".")
	let txt=strpart(getline(l),c)
	if txt[0]=='/'
		return s:CloseLast(1)
"		return s:Find__Back( matchstr(txt,'\k\+') ,0 )
	endif
	return ''
endfun

fun! s:CloseLast( returnall )
	let reA='\v\<(/=<\k+>)([^/>"]+|"[^"]*")*[/>]'
	let reMatch='\c\v\<(<\k+>)\>.{-}\</<\1>\>'
	if &syntax == 'html'
		let reSingles='br|hr'
	else
		let reSingles='--'
	endif
	let reIncompleteStart='<.*$'
	let reIncompleteEnd='^.\{-}>'

	let l=line('.')
	let txt=strpart(getline(l),0,col('.')-1)

	" Search Forwards for the next enclosing
	let fl=line('.')
	let ftxt=strpart(getline(fl),col('.')-1)
	let ahead=''
	while fl < line('$')
		let i=0
		while 1
			let j=matchend(ftxt,reA, i)
			if j==-1
				break
			else
				if ftxt[j-1]=='>'
					let word=substitute(matchstr(ftxt,reA,i),reA,'\1','')
					if word !~ '\c\v<('.reSingles.')>'
						let ahead=ahead.'<'.word.'>'
					endif
					let i=j
				else
					let i=j+1
				endif
			endif
		endwhile

		let ahead=substitute(ahead, reMatch, '', 'g')
		if ahead =~ '</'
			break
		endif

		let fl=fl+1
		let ftxt=matchstr(ftxt,reIncompleteStart,i).getline(fl)
	endwhile

	let closedahead= matchstr(ahead, '\v^\</\zs<\k+>\ze\>')

	" Search backwards for the unclosed
	let left=''
	while l > 1
		let i=0
		let lleft=''
		while 1
			let j=matchend(txt,reA, i)
			if j==-1
				break
			else
				if txt[j-1]=='>'
					let word=substitute(matchstr(txt,reA,i),reA,'\1','')
					if word !~ '\v\c<('.reSingles.')>'
						let lleft=lleft.'<'.word.'>'
					endif
					let i=j
				else
					let i=j+1
				endif
			endif
		endwhile
		let left=substitute(lleft.left, reMatch, '', 'g')
		if left =~ '<[^/]'
			if ! a:returnall || matchend(left,'\c\v\<'.closedahead.'\>')== strlen(left)
				break
			endif
		endif

		let l=l-1
		let iend=matchend(txt,reIncompleteEnd)
		let first=matchend(txt,reA)
		if iend>=0 && txt[iend-1] == '>' && (first < 0  || ( iend < first ))
			let txt=getline(l).strpart(txt,0,iend)
		else
			let txt=getline(l)
		endif
	endwhile

	let left = substitute(left,'\v^.*\</<\k+>\>', '', 'g')

	let left=substitute(left,'\c^.*<'.closedahead.'>','','')

	if a:returnall
		let ret=''
		while left != ''
			let ret=matchstr(left,'^<[^>]*>').ret
			let left=substitute(left,'^<[^>]*>','','')
		endwhile
		return substitute(ret,'\v<\k+>','/&','g')
	else
		return substitute(matchstr(left,'\v<\k+>\>$'),'\<','</','')
	endif
endfun

imap <buffer> <m-;> <c-r>=<SID>CloseLast(0)<CR>
imap <buffer> <m-s-;> <c-r>=<SID>CloseLast(1)<CR>
imap <buffer> º <m-s-;>

"FileTypes: html,xml
" Close everything that hasn't been closed
if ! exists('htmlcmd_NOAUTOCLOSE')
inoremap <buffer> > ><c-o>mz<c-o>F<<c-r>=<SID>EndTag()<CR><c-o>f><right>
endif

" Close everything
imap <buffer> ¾ <m->>
imap <buffer> <m->> <c-r>=<SID>HTMLCloseAll(0,0,line('$')+1,0,line('.'),col('.'))<cr>

if !exists("usersign")
  let usersign=$USERNAME
endif

if !exists('htmlcmd_NOHEADER')
	aug htmlwrite
	" In earlier versions, a failed ex // didn't cause an error
	if v:version < 600
		au! BufWrite *.html /\(\<Last Modified:.*\)/mark z | exe 'call setline(line("'."'".'z"),substitute(getline("'."'".'z"),'."'".'\<\d\+\s*\a\a\a\s*\d\+\s\+\d\+:\d\+\>[^<]*\s*'."'".',"'.strftime("%d %b %Y %H:%M").' by '.usersign.'",""))'
	else
		au! BufWrite *.html sil! /\(\<Last Modified:\)/mark z | exe 'call setline(line("'."'".'z"),substitute(getline("'."'".'z"),'."'".'\(Last Modified: \)\<\d\+\s*\a\a\a\s*\d\+\s\+\d\+:\d\+\>[^<]*\s*'."'".',"\\1'.strftime("%d %b %Y %H:%M").' by '.usersign.'",""))'
	endif
	aug END
	let g_html_gocursor="\<esc>?#CURSOR#\<cr>cf#"
	let g_html_htmlheader="
				\<HTML>\<CR>
				\<HEAD>\<CR>
				\<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=windows-1252\"/>\<CR>
				\<META NAME=\"Generator\" CONTENT=\"Vim %VERSION%\"/>\<CR>
				\<META NAME=\"ProgID\" CONTENT=\"Vim.Application\"/>\<CR>
				\<TITLE>#CURSOR#</TITLE>\<CR>
				\</HEAD>\<CR>
				\<BODY TEXT=\"#000f90\" LINK=\"#3030ff\" VLINK=\"#202020\" >\<CR>\<CR>
				\<!-- Trailer -->\<CR>
				\<hr/>\<CR>
				\<table width=\"100%\"><tr width=\"100%\"><td width=\"50%\">\<CR>
				\<small>Last Modified: 00 Aaa 0000 00:00 by Nobody</small></td>\<CR>
				\<td align=right width=50%><small>Powered by <b>Vim</b></small></td></tr>\<CR>
				\</table>\<cr>
				\</BODY></HTML>\<CR>
				\".g_html_gocursor
endif

