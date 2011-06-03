"This vim script can be used when editing xml files.
"
" source it, or runtime it from ftplugin/xml.vim.
"
" Usage:
"
"    when inserting type:
"
"    word,,,
"
"    This gets immediately translated to
"    <para>
"    </para>
"    If word is not alone on a line the xml element is kept on one line.
"	<para><emphasis>hello</emphasis></para>
"
"	Does not work with xml elements with just one character (such as p in
"	html) 
"
" Bart van Deenen , www.vandeenensupport.com
function! Make_element()
	if match(getline('.'),'^\s*'.@".'\s*$') == -1
		"the deleted word was not alone on the line
		 let @w = "i<ea></pa>F<i"
	else
		"the deleted word was on it's own on the line
		 let @w = "i<ea></pa>kA"
	endif
endfunction

"include colon(58) for namespaces in xsl for instance
setlocal iskeyword=@,48-57,_,192-255,58
imap <buffer>  ,,, <Esc>bye:call Make_element()<enter>@w

