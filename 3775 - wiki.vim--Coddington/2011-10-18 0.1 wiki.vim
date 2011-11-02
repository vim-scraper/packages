augroup wiki
    autocmd!
    autocmd BufNewFile wiki:*   call s:WikiPageGET(expand("%:t<amatch>"))
    autocmd BufWriteCmd wiki:*  call s:WikiPagePOST(expand("%:t<amatch>"))
augroup END

let s:user = 'user'
let s:pass = 'gorgonzola'
let s:server = 'wiki.hostname.com'
let s:host_header = 'wiki-basic.hostname.com'

function! s:WikiPageGET(page)
    let s:title = matchstr(a:page, 'wiki:\zs.*\ze')
    set filetype=mediawiki

    let s:curlGET ='curl -k -s '
    let s:curlGET .= '-H"host: '.s:host_header.'" '
    let s:curlGET .= '"https://'.s:user.':'.s:pass.'@'.s:server.'/api.php?'
    let s:curlGET .= 'titles='.s:title.'&action=query&prop=revisions&rvprop=content&format=xml"'
    let s:wpGET = system(s:curlGET)

    let s:wpTextList = split(matchstr(s:wpGET, '\c<revisions><rev>\zs.*\ze</rev></revisions'), '\n')

    silent 0,$delete
    call setline(1, s:wpTextList)

    silent! %s/&lt;/</g
    silent! %s/&gt;/>/g
    silent! %s/&quot;/"/g
    silent! %s/&amp;/\&/g
endfunction

function! s:WikiPagePOST(page)
    let s:title = matchstr(a:page, 'wiki:\zs.*\ze')

    let s:cookies = tempname()

    let s:curlGET_token ='curl -k -s -c'.s:cookies.' '
    let s:curlGET_token .= '-H"host: '.s:host_header.'" '
    let s:curlGET_token .= '"https://'.s:user.':'.s:pass.'@'.s:server.'/api.php?'
    let s:curlGET_token .= 'titles='.s:title.'&action=query&rvprop=content&prop=info&intoken=edit&format=xml"'
    let s:wpGET_token = system(s:curlGET_token)
    let s:wpToken = matchstr(s:wpGET_token, '\c.*edittoken="\zs.*\ze" />')

    let s:text = tempname()
    exe '1,$write! ' s:text

    let s:curlPOST ='curl -k -s -b'.s:cookies.' '
    let s:curlPOST .= '-H"host: '.s:host_header.'" '
    let s:curlPOST .= '"https://'.s:user.':'.s:pass.'@'.s:server.'/api.php?'
    let s:curlPOST .= 'title='.s:title.'&action=edit&format=xml" '
    let s:curlPOST .= '--data-urlencode ''token='.s:wpToken.''' '
    let s:curlPOST .= '--data-urlencode ''text@'.s:text.''' '
    let s:wpPOST = system(s:curlPOST)
    echo s:wpPOST

    set nomodified
endfunction
