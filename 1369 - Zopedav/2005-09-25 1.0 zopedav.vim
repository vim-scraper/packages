" zopedav.vim: (global plugin) Handles file transfer with zope using WebDAV
" Last Change:	Sep 25, 2005
" Maintainer:	Sébastien Migniot, <smigniot AT hotmail.com>
" Version:	1
" License:	Vim License  (see vim's :help license)
" =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

" ---------------------------------------------------------------------
" The WebDAV executable {{{
let s:webdav_executable = "cadaver"
" }}}
" ---------------------------------------------------------------------
" WebDAV url parser {{{
"
" splits an url into base component
" protocol://host:port/dirpath/filename
function! <SID>ZopeCompute(url)
    if !exists("b:tempfile")
        let b:tempfile = tempname()
    endif
    
    let s:protocol = match(a:url, "^[a-zA-Z]*\\zs://")
    let s:fullhost = match(a:url, "/", s:protocol+3)
    let s:dirpath  = match(a:url, "[^/]*$", s:fullhost+1)
    let s:filename = strpart(a:url, s:dirpath)
    let s:dirpath  = strpart(a:url, s:fullhost, s:dirpath-s:fullhost-1)
    let s:fullhost = strpart(a:url, s:protocol+3, s:fullhost-s:protocol-3)
    let s:protocol = strpart(a:url, 0, s:protocol)
    let s:port     = match(s:fullhost, ":")
    let s:host     = strpart(s:fullhost, 0, s:port)
    let s:port     = strpart(s:fullhost, s:port+1)
    if strlen(s:host) == 0
        let s:host = s:port
        let s:port = "80"
    endif

    let s:webdav_command_line = s:webdav_executable . " http://" . s:host . ":" . s:port . s:dirpath
endfunction
" }}}
" ---------------------------------------------------------------------
" Run a cadaver script {{{
"
" write a script file for cadaver
" and run cmd against it
function! <SID>ZopeScript(lines)
    if !exists("b:scriptfile")
        let b:scriptfile = tempname()
    endif

    exe "new " . b:scriptfile
    if !append('.', a:lines)
        silent exe "write!"
        bdelete!
        silent exe "!" . s:webdav_command_line . " < " . b:scriptfile
    endif
endfunction
" }}}
" ---------------------------------------------------------------------
" Edit a Zope WebDAV document source {{{
"
" splits the url and
" retrieve document_src
function! <SID>ZopeGet(url)
    call <SID>ZopeCompute(a:url)
    let tempfile = b:tempfile
    call <SID>ZopeScript("get " . s:filename . "/document_src " . b:tempfile)
    let scriptfile = b:scriptfile
    silent exe "edit! " . b:tempfile
    silent exe "file " . a:url
    let b:tempfile = tempfile
    let b:scriptfile = scriptfile
endfunction
" }}}
" ---------------------------------------------------------------------
" Write a Zope WebDAV document {{{
"
" splits the url and
" write filename
function! <SID>ZopePut(url)
    call <SID>ZopeCompute(a:url)
    let tempfile = b:tempfile
    silent exe "write! " . b:tempfile
    call <SID>ZopeScript("put " . b:tempfile . " " . s:filename)
    let scriptfile = b:scriptfile
    silent exe "file " . a:url
    let b:scriptfile = scriptfile
    let b:tempfile = tempfile
endfunction
" }}}
" ---------------------------------------------------------------------
" Zope WebDAV commands {{{
command -nargs=1 ZopeGet call <SID>ZopeGet(<f-args>)
command -nargs=1 ZopePut call <SID>ZopePut(<f-args>)
" }}}
" ---------------------------------------------------------------------
" Auto commands for Zope {{{
"
" Handles transparent reading and writing
" of Zope WebDAV urls
if version >= 600
 augroup Zope
 au!
 au BufReadCmd zope://* exe "doau BufReadPre ".expand("<afile>")|exe "ZopeGet ".expand("<afile>")|exe "doau BufReadPost ".expand("<afile>")
 au FileReadCmd zope://* exe "doau BufReadPre ".expand("<afile>")|exe "ZopeGet ".expand("<afile>")|exe "doau BufReadPost ".expand("<afile>")
 au BufWriteCmd zope://* exe "ZopePut ".expand("<afile>")
endif
" }}}

