"{{{1 Documents
"
" File:     webpreview.vim
" Description:
"   This plugin make you can dynmic page in customed browser.Just use ",bd" to preview the
"   edited file in Default configed browser, and use ",bs" to preview in Second browser.
"   It can identify the web server automatically. It also support to preview the file on the
"   remote ftp server. It also support a simple web favorite, use ",bl" to list your
"   favorites and ",bf" to goto the favorite.
" Version:  0.20
" Date:     2008-01-11
" Author:   weiye (zhqm03@gmail.com)
"
" Install:
"   Put webpreview.vim in youre dirctory $vim/vimfiles/plugin
" Usage:
"   1. put ' let g:loaded_webpreview = 1 ' into your _vimrc will stop to load the plugin.
"   2. config the browser list and web server list in your _vimrc(windows) or .vimrc (unix)
"   
" Config Example 1
" """"""""""""""""""""""""
" let g:Default_WebBrowser='c:\Program Files\Maxthonsecond\Maxthon.exe'
" let g:Second_WebBrowser=''
" let g:Default_WebDomain='http://weiye.cn'
" let g:Default_WebRoot='D:/www'
" 
" Config Example 2
" """"""""""""""""""""""""
" let g:WebList = [
"            \ {'domain' : 'http://*.blog.weiye.cn','rootdir' : 'D:/www/blog/*'},
"            \ {'domain' : 'http://blog.weiye.cn','rootdir' : 'D:/www/blog'},
"            \ {'domain' : 'http://weiye.cn','rootdir' : 'D:/www'},
"            \ {'domain' : 'http://aa.bb.cn','rootdir' : 'ftp://user@default9second.168.202.110/www/xx'}
"            \ ]

" let g:BrowserList = {
"            \'default' : 'c:\Program Files\Maxthonsecond\Maxthon.exe' ,
"            \'second'  : 'c:\Program Files\Mozilla Firefox\firefox.exe' ,
"            \'ie'      : 'c:\Program Files\Internet Explorer\iexplore.exe' ,
"            \'ff'      : 'c:\Program Files\Mozilla Firefox\firefox.exe' 
"            \}

" 3. There have been two default mappings
"   
"    noremap <silent> ,bd :silent call g:Browser('default',expand('%:p'))<cr>
"    noremap <silent> ,bs :silent call g:Browser('second',expand('%:p'))<cr>
"    noremap <silent> ,bl :call g:ShowWebFavorites()<cr>
"    noremap <silent> ,bf :call g:GotoFavorite()<cr>
"
" You can also make your own mappings.
"
" History:
" version 0.2 : 2008-01-11
"   1.Support file path with signs '\', and support web rootdir with signs '\','~','.'
"   2.Support a wildcard '*' in webrootdir and domain
"   3.Hide the DOS window completely when call the browser
"   4.Add a simple web favoriates function, and improve the performence when file start with 'http://'
" version 0.1 : 2008-01-10
"   1.Support browser list, file will be open in a new tab for browsers which support tab page.
"   2.Support site list. support remote ftp server and nesting dirctory. Site root with the
"   subdirctory should be placed before the site root with the parent directory.
"  
"
"   
"
" Todo:
" 1. support complex filename,for example include chinese chars or space. 
" 2. get default browser from system or registry when not configed.
" =========================================================================================== 
" {{{1 Source Code


if exists('g:loaded_webpreview') && g:loaded_webpreview == 1
    finish
endif

let g:loaded_webpreview = 1

"if v:version < 700
"     echohl ErrorMsg | echomsg "webbrowser.vim needs vim version >= 7.0!" | echohl None
"     finish
"endif

if !exists("g:Default_WebBrowser")
    if has("win32") || has("win64")
        let g:Default_WebBrowser='c:\Program Files\Internet Explorer\iexplore.exe'
    elseif has('unix')
        let g:Default_WebBrowser = system("which firefox")
    endif
endif

if !exists("g:Second_WebBrowser")
    if has('unix')
        let g:Second_WebBrowser=system("which opera")
    else
        let g:Second_WebBrowser='c:\Program Files\Mozilla Firefox\firefox.exe'
    endif
endif

if !exists("g:Default_WebDomain")
    let g:Default_WebDomain= 'http://localhost'
endif

if !exists("g:Default_WebRoot")
    " space shouldn't exist in path 
    if has('unix')
        let g:Default_WebRoot = "/var/www"
    else 
        let g:Default_WebRoot= 'c:/Program Files/Apache/htdocs'
    endif
endif

if !exists('g:BrowserList')
    let g:BrowserList = {
                \    'default': g:Default_WebBrowser,
                \    'second': g:Second_WebBrowser,
                \ }
else
    if ! exists('g:BrowserList["default"]')
        let g:BrowserList['default']=g:Default_WebBrowser
    endif
    if ! exists('g:BrowserList["second"]')
        let g:BrowserList['second'] = g:Second_WebBrowser
    endif
endif

if !exists("g:WebList")
    let g:WebList=[{'domain' : g:Default_WebDomain , 'rootdir' : g:Default_WebRoot}]
endif

if !exists("g:WebFavoriteList")
    let g:WebFavoriteList= [
                    \{'name' : 'vim', 'url' : "http://vim.sf.net"},
                    \{'name' : 'vimtips', 'url' : "http://vim.wikia.com/wiki/Main_Page"},
                    \{'name' : 'plugins' , 'url' : "http://vim.sf.net/search.php"}
                \]
endif

function! g:ShowWebFavorites()
        let i = 0
        for fav in g:WebFavoriteList
            let i += 1
            echo i . "  " . fav.name
        endfor
endfunction

function! g:GotoFavorite()
    let name = input("Go To favorite:")
    if name == ''
        let name = 0
    endif
    if name =~ '^[0-9]$'
        let url = g:WebFavoriteList[name]['url']
    else
        for fav in g:WebFavoriteList
            if fav.name =~ name
                let url = fav.url
            endif
        endfor
    endif
    if url == ''
        echohl ErrorMsg | echomsg "no such favorites exists" | echohl None
    else
        silent call g:Browser('default',url)
    endif

endfunction

function! s:GetBrowser(browser)
    if exists("g:BrowserList['" . a:browser ."']")
        return g:BrowserList[a:browser]
    else
        echo "the browser " . a:browser . "is not exists."
    endif
endfunction

" Get the url of the file
" for windows ignore the case of chars in file path
" there must be no chinese chars in the file path

function! s:GetUrl(file)
    let file= substitute(a:file,'\\','/','g')
    if has('win32') || has('win64')
        let file=tolower(file)
    endif
    if file =~ '^http://.\+'
        let url = file
        return url
    endif
    for web in g:WebList
        if has('win32') || has('win64') 
            let domain=tolower(web.domain)
            let rootdir=tolower(web.rootdir)
        endif
        let rootdir=escape(rootdir,'.~')
        let rootdir=substitute(rootdir,'\\','/','g')
        let rootdir=substitute(rootdir,'\*','\\\(\[\^\/\]\*\\\)','g')
        let rootdir=substitute(rootdir,'/$','','')
        let domain=substitute(domain,'\*','\\1','g')
        let domain=substitute(domain,'/$','','')
        
        let url=substitute(file,rootdir,domain,'')
        if url != file
            return url
        endif
    endfor

    if file =~ '^\(ftp:\|ssh:\|file:\)//.\+'
        let url = file
    elseif file =~ '^www[0-9]\?\.'
        let url = "http://" . file
    elseif file =~ '^ftp[0-9]\?\.'
        let url = "ftp://" . file
    elseif file =~ '^[a-z]:\(\\\|/\)[-.[:alnum:]_~]\+' || file =~ '^/[-.[:alnum:]_~]\+'
        let url = "file://" .file
    else
        let url=file
    endif

    return url
endfunction

function! s:GetCmd(browserPath,url)
        let browserPath = substitute(a:browserPath,'\\','/','g')
        if(browserPath =~ '\cfirefox')
            return '"' . browserPath . '" -remote openURL(' . a:url . ',new-tab)'
        elseif(browserPath =~ '\copera')
            return '"' . browserPath . '" -remote openURL(' . a:url . ',new-page)'
        else
            return '"'.browserPath . '" ' . a:url
        endif
endfunction

function! g:Browser(browser,file) 
    let browser = s:GetBrowser(a:browser)
    let url = s:GetUrl(a:file)
    let cmd = s:GetCmd(browser,url)
    exec "!start " . cmd
endfunction

" Mappings
noremap <silent> ,bd :silent call g:Browser('default',expand('%:p'))<cr>
noremap <silent> ,bs :silent call g:Browser('second',expand('%:p'))<cr>
noremap <silent> ,bl :call g:ShowWebFavorites()<cr>
noremap <silent> ,bf :call g:GotoFavorite()<cr>

" vim: set ft=vim foldmethod=marker sw=4 ts=8:
