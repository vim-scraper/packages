" zopedav.vim: (global plugin) Handles file transfer with WebDAV, adapted for zope
" Last Change:	Aug 28, 2008
" Maintainer:	Sébastien Migniot, <smigniot AT hotmail.com>
" Version:	2
" License:	Vim License  (see vim's :help license)
" =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

" ---------------------------------------------------------------------
" Configuration {{{
let s:webdav_executable = 'curl'
let s:webdav_get_suffix = ''
let s:webdav_put_suffix = ''
" }}}
" ---------------------------------------------------------------------
" Return a suffix {{{
"
" Look for a named variable in
" the buffer namespace first,
" the global namespace then and
" else in the script namespace
"
function! <SID>WebDAVGetSuffix(method)
	" 1. Build variable name
	let l:name = 'webdav_' . a:method . '_suffix'
	" 2. Try buffer namespace
	if exists('b:' . l:name)
		return eval('b:' . l:name)
	" 3. Try global namespace
	elseif exists('g:' . l:name)
		return eval('g:' . l:name)
	" 4. Use script namespace
	else
		return eval('s:' . l:name)
	endif
endfunction
" ---------------------------------------------------------------------
" Compute a target url {{{
"
" Assume url starts with a protocol and a colon
" for instance webdav://somehost/some/path/filename
"
" Return that url starting with 'http', or with
" 'https' if the protocol starts with an 's'
" and append suffix
"
" For instance swebdav://somehost:483/some/path/
" Returns https://somehost:483/some/path/suffix
"
function! <SID>WebDAVURL(url, suffix)
	" 1. Detect protocol colon
	let l:index = stridx(a:url, ':')
	" 2. Isolate content
	let l:content = substitute(strpart(a:url, l:index),'\\','/','g')
	" 4. Detect starting s
	if a:url =~ '^s'
		let l:prefix = 'https'
	else
		let l:prefix = 'http'
	endif
	" 5. Build url
	return l:prefix . l:content . a:suffix
endfunction
" ---------------------------------------------------------------------
" Read a webdav url content {{{
"
" Assume url starts with a protocol and a colon
" for instance webdav://somehost/some/path/filename
" and retrieves the content of the url, replacing
" the protocol and appending webdav_get_suffix
"
function! <SID>WebDAVGet(url)
	" 1. Create a tempfile if required
	if !exists('b:webdav_tempfile')
		let b:webdav_tempfile = tempname()
	endif
	" 2. Compute target URL
	let l:target_url = <SID>WebDAVURL(a:url, <SID>WebDAVGetSuffix('get'))
	" 3. Run WebDAV GET command
	silent exe "!" . s:webdav_executable . ' -o "' .b:webdav_tempfile . '" "' . l:target_url . '"'
	" 4. Insert the tempfile
	exe "0read " . b:webdav_tempfile
	" 5. Fixup last blank line
	$delete
endfunction
" }}}
" ---------------------------------------------------------------------
" Write a webdav url content {{{
"
" Assume url starts with the webdav_protocol
" for instance webdav://somehost/some/path/filename
" and stores the content of the url, replacing
" the protocol by webdav_target_protocol and appending
" webdav_put_suffix
"
function! <SID>WebDAVPut(url)
	" 1. Save current buffer
	exe "write! " . b:webdav_tempfile
	set nomodified
	" 2. Compute target URL
	let l:target_url = <SID>WebDAVURL(a:url, <SID>WebDAVGetSuffix('put'))
	" 3. Run WebDAV PUT command
	silent exe "!" . s:webdav_executable . ' -T "' . b:webdav_tempfile. '" "' . l:target_url . '"'
endfunction
" }}}
" ---------------------------------------------------------------------
" Set zope webdav suffixes {{{
"
" Zope editing uses /document_src to get a non
" published source document and no suffix for
" source uploading
"
function! <SID>WebDAVZopeSuffixes()
	let b:webdav_get_suffix = '/document_src'
	let b:webdav_put_suffix = ''
endfunction
" ---------------------------------------------------------------------
" WebDAV commands {{{
"
command! -nargs=0 WebDAVZopeSuffixes call <SID>WebDAVZopeSuffixes()
command! -nargs=1 WebDAVGet call <SID>WebDAVGet(<f-args>)
command! -nargs=1 WebDAVPut call <SID>WebDAVPut(<f-args>)
" }}}
" ---------------------------------------------------------------------
" Auto commands for Zope {{{
"
" Handles transparent reading and writing
" of Zope WebDAV urls
"
if version >= 600
	augroup Zope
	au!
	au BufReadCmd zope://* exe "doau BufReadPre ".expand("<afile>")|exe "WebDAVZopeSuffixes"|exe "WebDAVGet ".expand("<afile>")|exe "doau BufReadPost ".expand("<afile>")
	au BufReadCmd szope://* exe "doau BufReadPre ".expand("<afile>")|exe "WebDAVZopeSuffixes"|exe "WebDAVGet ".expand("<afile>")|exe "doau BufReadPost ".expand("<afile>")
	au FileReadCmd zope://* exe "doau BufReadPre ".expand("<afile>")|exe "WebDAVZopeSuffixes"|exe "WebDAVGet ".expand("<afile>")|exe "doau BufReadPost ".expand("<afile>")
	au FileReadCmd szope://* exe "doau BufReadPre ".expand("<afile>")|exe "WebDAVZopeSuffixes"|exe "WebDAVGet ".expand("<afile>")|exe "doau BufReadPost ".expand("<afile>")
	au BufWriteCmd zope://* exe "WebDAVPut ".expand("<afile>")
	au BufWriteCmd szope://* exe "WebDAVPut ".expand("<afile>")
endif
" }}}
" ---------------------------------------------------------------------
" Auto commands for WebDAV {{{
"
" Handles transparent reading and writing of WebDAV urls
"
if version >= 600
	augroup WebDAV
	au!
	au BufReadCmd webdav://* exe "doau BufReadPre ".expand("<afile>")|exe "WebDAVGet ".expand("<afile>")|exe "doau BufReadPost ".expand("<afile>")
	au BufReadCmd swebdav://* exe "doau BufReadPre ".expand("<afile>")|exe "WebDAVGet ".expand("<afile>")|exe "doau BufReadPost ".expand("<afile>")
	au FileReadCmd webdav://* exe "doau BufReadPre ".expand("<afile>")|exe "WebDAVGet ".expand("<afile>")|exe "doau BufReadPost ".expand("<afile>")
	au FileReadCmd swebdav://* exe "doau BufReadPre ".expand("<afile>")|exe "WebDAVGet ".expand("<afile>")|exe "doau BufReadPost ".expand("<afile>")
	au BufWriteCmd webdav://* exe "WebDAVPut ".expand("<afile>")
	au BufWriteCmd swebdav://* exe "WebDAVPut ".expand("<afile>")
endif
" }}}



