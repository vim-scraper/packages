" HTML Commands.
" Author: Michael Geddes <michaelrgeddes@optushome.com.au>
" Version: 2.5
" Please feel free to use and modify all or part of this script.
" I would appreciate being acknowledged in any derived scripts, and would 
" appreciate any updates/modifications.

" Define htmlcmd_NOHEADER to not include <m-0> style
" Define htmlcmd_BF for some of Benji's commands that haven't been adopted quite yet.
" Define htmlcmd_AUTOCLOSE to map > to auto-close open tags.
" usersign is your username for modification

" :GenTOC  -- Generate Table Of Contents
" :GenTOC * -- Generate Table Of Contents - prompting for levels
"
" History:
"   2.5: Added more menus - made them useful for detaching, and able to
"   customise the name.

" This code allows htmlcmd to behave as a FT plugin and as a buffoptions.vim
" style plugin.
if !exists('DoingSOURCE')
  if exists('*ReadFileTypeMap')
	SO <sfile>
	finish
  endif
  " Else we assume we are behaving as a plugin
endif


if !exists( 'html_menu_leader')
    let html_menu_leader='HTM&L'
endif
if !exists( 'html_menu_pos')
    let html_menu_pos='45'
endif
if !exists( 'html_font_menu_leader')
    let html_font_menu_leader='.&Font'
endif
if !exists( 'html_font_menu_pos')
    let html_font_menu_pos='.10'
endif
if !exists( 'html_defs_menu_leader')
    let html_defs_menu_leader='.&Defs'
endif
if !exists( 'html_defs_menu_pos')
    let html_defs_menu_pos='.20'
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

" - The main function that handles toggling of HTML commands. It searches
"   backwards for the last token of the specified type, and returns its
"   counterpart. It also closes all nested tags that have been opened. 

function! s:FindBack(option)
  return s:Find__Back(a:option,1)
endfun

"function! s:Find__Fwd()
"  let mx='<\s*\(/\=\)\(\k\+\)\>[^>]*>'
"
"  let curline=line('.')
"  let curcol=col('.')
"  let text=getline(curline)
"  let text=strpart(text,curcol,65535)
"  while 1
"	
"  endwhile
"
"endfun

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

let s:bmenu_oneshot=0
if has('menu') && exists(':Bmenu') && BufferOneShot('htmlcmd')
    exe FindBufferSID()
    let s:bmenu_oneshot=1
endif

fun! s:MapAndMenuStyle( rootpos, rootmenu, key, menu, html)
    exe "imap <buffer> ".a:key." <c-r>=<SID>FindBack('".a:html."')<CR>"
    exe "vmap <buffer> ".a:key." :call <SID>SurroundHTM('".a:html."')<CR>"
    exe "nmap <buffer> ".a:key." i<c-r>=<SID>FindBack('".a:html."')<CR>"

    if has('menu')
        if !exists( 's:menu_number' )|let s:menu_number =10|else|let s:menu_number=s:menu_number+10|endif
        let menuname=a:rootpos.'.'.s:menu_number.' '.a:rootmenu.'.'.a:menu
        if exists(':Bmenu')
            if s:bmenu_oneshot " Only do it once
                " exe FindBufferSID()
                exe "Bimenu ".menuname." <c-r>=<SID>FindBack('".a:html."')<CR>"
                exe "Bvmenu ".menuname." :call <SID>SurroundHTM('".a:html."')<CR>"
                exe "Bnmenu ".menuname." i<c-r>=<SID>FindBack('".a:html."')<CR>"
            endif
        else
            exe "imenu ".menuname." <c-r>=<SID>FindBack('".a:html."')<CR>"
            exe "vmenu ".menuname." :call <SID>SurroundHTM('".a:html."')<CR>"
            exe "nmenu ".menuname." i<c-r>=<SID>FindBack('".a:html."')<CR>"
        endif
    endif
endfun
let s:menu_number=10

let s:menu_number=10
let s:menpos=html_menu_pos.html_font_menu_pos
let s:menlead=html_menu_leader.html_font_menu_leader
if has('menu')
	if exists(':Bmenu')
		if s:bmenu_oneshot
            exe 'amenu '.s:menpos.'.200 '.s:menlead.'.-1- <nul>'
            exe 'amenu '.html_menu_pos.'.25 '.html_menu_leader.'.-1- <nul>'
			exe 'Bamenu '.html_menu_pos.'.100 '.html_menu_leader.'.&Generate\ TOC :GenTOC *<CR>'
			exe 'Bamenu '.html_menu_pos.'.110 '.html_menu_leader.'.&Refresh\ TOC :GenTOC<cr>'
		endif
	else
        exe 'amenu '.html_menu_pos.'.100 '.html_menu_leader.'.&Generate\ TOC :GenTOC *<CR>'
        exe 'amenu '.html_menu_pos.'.110 '.html_menu_leader.'.&Refresh\ TOC :GenTOC<cr>'
	endif
endif

call s:MapAndMenuStyle(s:menpos,s:menlead,'<m-b>', '&Bold', 'b')
call s:MapAndMenuStyle(s:menpos,s:menlead,'<m-m>', '&Italic', 'i')
call s:MapAndMenuStyle(s:menpos,s:menlead,'<m-u>', '&Underline', 'u')
call s:MapAndMenuStyle(s:menpos,s:menlead,'<m-p>', '&Para', 'p')
call s:MapAndMenuStyle(s:menpos,s:menlead,'<c-k>', '&Keyboard', 'kbd')
call s:MapAndMenuStyle(s:menpos,s:menlead,'<m-s-b>', 'Big', 'big')

let s:menpos=html_menu_pos.html_defs_menu_pos
let s:menlead=html_menu_leader.html_defs_menu_leader
call s:MapAndMenuStyle(s:menpos,s:menlead,'<m-d>', '&Table Definition', 'td')

let s:menpos=html_menu_pos.html_defs_menu_pos
let s:menlead=html_menu_leader.html_defs_menu_leader
call s:MapAndMenuStyle(s:menpos,s:menlead,'<m-1>', 'Level\ &1', 'h1')
call s:MapAndMenuStyle(s:menpos,s:menlead,'<m-2>', 'Level\ &2', 'h2')
call s:MapAndMenuStyle(s:menpos,s:menlead,'<m-3>', 'Level\ &3', 'h3')
call s:MapAndMenuStyle(s:menpos,s:menlead,'<m-4>', 'Level\ &4', 'h4')
call s:MapAndMenuStyle(s:menpos,s:menlead,'<m-5>', 'Level\ &5', 'h5')
call s:MapAndMenuStyle(s:menpos,s:menlead,'<m-6>', 'Level\ &6', 'h6')

com! -nargs=? GenTOC call GenerateTOC(<q-args>)

function! GenerateTOC(...)
  let types=""
  let do_prompt=0
  if a:0 > 0 
  	if a:1=='*'
		let do_prompt=1
	else
		let types=a:1
	endif
  endif
  let line=1
  let found=0
  let TOC=0
  let end=line('$')
  let mtoc='<!--TOC\(-\([^->]*\)\)\=-->'
  while line<end
	if getline(line)=~mtoc
	  let TOC=line
	  if getline(line+1) !~ mtoc
		if types==''
		" Get the types originally used.
			let types=substitute(matchstr(getline(line),mtoc),mtoc,'\2','')
		endif
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
  if g:htmlCmd_debug| echo '------' |endif
  if do_prompt
  	let txt=((types=='')?'default':types)
  	let txt=inputdialog('Level Modes {1|a|i} :', txt)
	if txt==''
		return 0
	elseif txt=='default'
		let types=''
	else
		let types=txt
	endif
  endif
  if found
	exe TOC.',/<!--ENDTOC-->/ d'
	let TOC=TOC-1
  endif
  call append(TOC, "<!--TOC".((types=='')? '' : ('-'.types)).'-->')
  let TOC=TOC+1
  call append(TOC,"<!--ENDTOC-->")
  let end=line('$')
 
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

imap <buffer> <m-8> <li></li><esc>F<i
nmap <buffer> <m-8> i<m-8>
vmap <buffer> <m-8> :call <SID>SurroundHTM('li')<cr>

imap <buffer> <m-.> <ol><CR></ol><esc>F<O<TAB>
nmap <buffer> <m-.> o<m-.>
imap <buffer> <m-,> <lt>ul><cr></ul><esc>F<O<TAB>
nmap <buffer> <m-,> o<m-,>

if !exists('htmlcmd_NOHEADER')
imap <buffer> <m-0> <c-r>=substitute(g_html_htmlheader,'%VERSION%',(v:version/100).'.'.(v:version%100),'')<cr>
endif

imap <buffer> <c-cr> <esc>o
imap <buffer> <s-cr> <lt>br<space>/><CR>
imap <buffer> <m--> <lt>hr<space>/>

if 0
imap <buffer> &'< &lsquo;
imap <buffer> &'> &rsquo;
imap <buffer> &"< &ldquo;
imap <buffer> &"> &rdquo;
endif

if !exists('b:htmlcmd_map_symbols_to_numbers')
  let b:htmlcmd_map_symbols_to_numbers=(&filetype=='xml')
endif

" See http://evolt.org/article/ala/17/21234/
fun! s:HTMLSymbolMap( type)
  if !b:htmlcmd_map_symbols_to_numbers
    return a:type
  elseif (a:type =='lsquo')
    return '#8216'
  elseif (a:type =='rsquo')
    return '#8217'
  elseif (a:type =='ldquo')
    return '#8220'
  elseif (a:type =='rdquo')
    return '#8221'
  elseif (a:type == 'mdash')
    return '#8212'
  else
    return a:type
  endif
endfun

fun! s:HTMLQuote( type)
  if col('.') ==1 || getline('.')[col('.')-2] =~'\s\|>' 
	  return '&'.s:HTMLSymbolMap('l'.a:type).';'
  else
	  return '&'.s:HTMLSymbolMap('r'.a:type).';'
  endif  
endfun
imap <buffer> &' <c-r>=<SID>HTMLQuote('squo')<CR>
imap <buffer> &" <c-r>=<SID>HTMLQuote('dquo')<CR>
imap <buffer> &- &<c-r>=<SID>HTMLSymbolMap('mdash')<CR>;
imap <buffer> &<space> &<c-r>=<SID>HTMLSymbolMap('nbsp')<CR>;
imap <buffer> && &<c-r>=<SID>HTMLSymbolMap('amp')<CR>;
imap <buffer> &< &<c-r>=<SID>HTMLSymbolMap('lt')<CR>;
imap <buffer> &> &<c-r>=<SID>HTMLSymbolMap('gt')<CR>;

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

nmap <buffer> <localleader>al "='<a href="'.@*.'" target="_top"></a>'<cr>p
nmap <buffer> <localleader>aL "='<li><a href="'.@*.'" target="_top"></a></li>'<cr>p
nmap <buffer> <localleader>l "='<a href="#'.@".'"></a>'<cr>p
"nmap ,yf :let @*='file:///'.expand('%:p:gs+\+/+')<cr>
nmap <buffer> <localleader>yf :let @*=<SID>URLPathOf(expand('%:p'))<cr>

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


" Main function number for handling closing of HTML commands 
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
if exists('htmlcmd_AUTOCLOSE')
inoremap <buffer> > ><c-o>mz<c-o>F<<c-r>=<SID>EndTag()<CR><c-o>f><right>
endif

" Close everything
if has('win32')
    imap <buffer> ¾ <m->>
endif
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
				\<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\<CR>
				\<html>\<CR>
				\<head>\<CR>
				\<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\" />\<CR>
				\<meta name=\"Generator\" content=\"Vim %VERSION%\" />\<CR>
				\<meta name=\"ProgID\" content=\"Vim.Application\" />\<CR>
				\<title>#CURSOR#</title>\<CR>
				\</head>\<CR>
				\<body text=\"#000f90\" link=\"#3030ff\" vlink=\"#202020\" bgcolor=\"#e0f0ff\">\<CR>\<CR>
				\<!-- Trailer -->\<CR>
				\<hr />\<CR>
				\<table width=\"100%\"><tr>\<CR>
				\<td width=\"50%\"><small>Last Modified: 00 Aaa 0000 00:00 by Nobody</small></td>\<CR>
				\<td align=\"right\" width=\"50%\"><small>Powered by <b>Vim</b></small></td>\<CR>
				\</tr></table>\<CR>
				\</body>\<CR>
				\</html>\<CR>
				\".g_html_gocursor
endif

