" preview.vim:	    Preview the current file in selected browser
" Last Change:	    18th Oct 2003
" Author:	    Mark Woodward <markwoodward@bigpond.com>
" Version:	    0.1
" 
" Description:	    Pressing <S-F12> will prompt you for the browser you 
"		    would like to preview the current file in. Browsers can 
"		    be added by modifying the appropriately commented lines 
"		    in the script.  
"		    
"			** make sure to modify the current paths **
"			   ** for browsers on your system.**
"
"		    There is also the option to include only files of a 
"		    specific type. These are .php, .php3, .jsp, .asp, 
"		    .html, .htm, and .vim at the moment but others can easily 
"		    be added.
"		
" Setup:	    set the path(s) to the browser(s) relevant to your system.
"		    Add any additional browsers you'd like included
"		    Add any additional extensions you'd like included
"		    
" To Use:	    Currently <S-F12> in either normal or insert modes.		    
	    
"-------------------------------------------------------------------------

if exists("loaded_preview")
    finish
endif

let loaded_preview = 1
let s:save_cpo = &cpo
set cpo&vim

if !hasmapto('<Plug>PreviewBrowseFile')
    nmap <unique> <S-F12>   <Plug>PreviewBrowseFile
    imap <unique> <S-F12>   <Plug>PreviewBrowseFile
endif
noremap	 <unique> <silent> <script> 
	    \ <Plug>PreviewBrowseFile  :call<SID>BrowseFile()<cr>
inoremap <unique> <silent> <script> 
	    \ <Plug>PreviewBrowseFile  <c-o>:call<SID>BrowseFile()<cr>

"----------------------------------------------------------------
function s:BrowseFile()

    " set the path to the browsers you'd like to use. Add additional
    " browsers to the list below, setting the appropriate paths.
    " If a browser is included in your system path, you can if you 
    " wish only include the exe file
    " eg let s:moz = 'mozilla.exe'
    " 
    let s:ie  = 'C:\Program Files\Internet Explorer\iexplore.exe'
    let s:moz = 'd:\Moz\mozilla.exe'
    let s:nn4  = 'e:\nn4\program\netscape.exe'
    let s:opera = 'd:\opera7\opera.exe'

    " Modify the prompt and the if/elseif/endif statement to 
    " remove or include browsers.
    " eg "open in which browser? [m/e/n/o/k]"
    " elseif s:browser ==? "k"
    "	let s:browser = s:konquerer
    " 
    " If any other browsers handle path separators like nn4
    " you should include them in any if(s:browser == s:nn4)
    " statements: if(s:browser == s:nn4 || s:browser == s:?)
    
    let s:browser = input("open in which browser? [m/e/n/o]")
    if s:browser ==? "m"
	let s:browser = s:moz
    elseif s:browser ==? "e"
	let s:browser = s:ie
    elseif s:browser ==? "n"
	let s:browser = s:nn4
    elseif s:browser ==? "o"
	let s:browser = s:opera
    endif

    " the full path to the current file, it's extension and length.
    let s:ptf = expand("%:p")
    let s:ext = expand("%:e")
    let s:ptf_len = strlen(s:ptf)

    " the 'active' path.
    if(s:browser == s:nn4)
	let s:lhost = 'http://localhost/'
    else
	let s:lhost='http:\\localhost\'
    endif

    " The actual path to the base directory of the server.
    " (Where does http://localhost/ actually point to?)
    let s:aps='C:\Program Files\Apache Group\Apache\htdocs\'
    let s:aps_len = strlen(s:aps)

    " if the full path to the current file starts with the
    " actual path to the server, replace it with http:\\localhost\.
    " 
    " eg:
    " current file: C:\Program Files\Apache Group\Apache\htdocs\test.php
    "        s:aps: C:\Program Files\Apache Group\Apache\htdocs\
    "  will become: http:\\localhost\test.php
    "
    " otherwise, the file is as returned using expand("%:p")
    "
    " eg:
    " d:\mysite\index.html   stays as is.
    " 
    if(strpart(s:ptf,0,s:aps_len) == s:aps)
	let s:ptf = s:lhost . strpart(s:ptf,s:aps_len,s:ptf_len - s:aps_len)
	if s:browser == s:nn4
	    let s:ptf = substitute(s:ptf,"\\","/","g")
	endif
    endif

    " I'm not sure this needs to be included but..
    " only open recognised file types. 
    if	  s:ext ==? ("php") || s:ext ==? ("php3") ||
	\ s:ext ==? ("html")|| s:ext ==? ("htm")  ||
	\ s:ext ==? ("jsp") || s:ext ==? ("asp")  ||
	\ s:ext ==? ("vim")
	execute ":sil! !start " s:browser . " " . s:ptf
    else
	echo "cant open files of type: '" . s:ext . "' in browser"
    endif
endfunction
