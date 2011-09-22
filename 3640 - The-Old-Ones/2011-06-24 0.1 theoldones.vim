" The Old Ones -- Alerts you to old buffers.
"
" Description:
"
"   This script lists following buffers:
"       a) Empty (nameless and had not been viewed nor been saved)
"       b) Not Referred (had been opened in read-only usage)
"       c) Long Time No Save (had been modified and is not saved yet)
"   that are left long time.
"   And this script lets you close a) and/or b).
"
" Maintainer: Shuhei Kubota <kubota.shuhei+vim@gmail.com>
"
" Usage:
"   1. (optional) set g:TheOldOnes_Threshold to what you feel "It's long time"
"      default: 60 (60 min.)
"   2. execute  :TheOldOnes
"
"

if !exists('g:TheOldOnes_Threshold')
    let g:TheOldOnes_Threshold = 60 "in minute
endif


let s:COMMENT_EMPTYBUFFER = 'Empty'
let s:COMMENT_NOTREFERRED = 'Not Referred'
let s:COMMENT_NOTSAVED    = 'Long Time No Save'

command!  TheOldOnes  :call <SID>Execute()

augroup TheOldOnes
    autocmd!

    autocmd  BufEnter  *  call <SID>OnBufEnter()
    autocmd  BufLeave  *  call <SID>OnBufLeave()

    autocmd  BufRead   *  call <SID>OnBufRead()
    autocmd  BufWrite  *  call <SID>OnBufWrite()
augroup END


function! s:Execute()
    let oldones = [] "of {nr:bufnr, name:expand('#'.nr.':t'), path:expand('#'.nr.':p:h'), tl:timeline, comment:comment}
    for b in range(1, bufnr('$'))
        let v = getbufvar(b, 'TheOldOnes_lastViewed')
        let m = getbufvar(b, 'TheOldOnes_lastModified')
        let s = getbufvar(b, 'TheOldOnes_lastSaved')

        if v == '' | continue | endif
        if m == '' | let m = 0 | endif
        if s == '' | let s = 0 | endif

        let tl = s:GetTimeline(v, m, s)
        let comment = s:GetComment(b, tl)

        if comment != ''
            call add(oldones, {'nr':b, 'name':expand('#'.b.':t'), 'path':expand('#'.b.':p:h'), 'tl':tl, 'comment':comment})
        endif
    endfor

    call sort(oldones, 's:OldonesCompare')

    let nowcomment = ''
    for o in oldones
        if o.comment != nowcomment
            let nowcomment = o.comment
            echom nowcomment . ':'
        endif

        echom '    ' . o.name . ' [' . o.nr . ']  ' . o.path
    endfor
    
    echom ' '
    echo 'a) close all ' . s:COMMENT_EMPTYBUFFER ', b) close all ' . s:COMMENT_NOTREFERRED . ', c) both'
    echo '[abc]'

    let ans = tolower(nr2char(getchar()))
    if ans == 'a' || ans == 'c'
        for o in oldones
            if o.comment == s:COMMENT_EMPTYBUFFER
                execute 'silent bdelete! ' . o.nr
            endif
        endfor
    endif
    if ans == 'b' || ans == 'c'
        for o in oldones
            if o.comment == s:COMMENT_NOTREFERRED
                execute 'silent bdelete! ' . o.nr
            endif
        endfor
    endif
endfunction

function! s:OldonesCompare(v1, v2)
    let v1c = s:CommentRenk(a:v1.comment)
    let v2c = s:CommentRenk(a:v2.comment)

    if v1c != v2c
        return v2c - v1c
    elseif a:v1.path == a:v2.path
        return 0
    elseif a:v1.path > a:v2.path
        return 1
    else
        return -1
    endif
endfunction

function! s:CommentRenk(c)
    let result = 0
    if a:c == s:COMMENT_EMPTYBUFFER
        return 3
    elseif a:c == s:COMMENT_NOTREFERRED
        return 2
    elseif a:c == s:COMMENT_NOTSAVED
        return 1
    endif
    return result
endfunction

function! s:OnBufEnter()
    let b:TheOldOnes_lastViewed = localtime()
endfunction

function! s:OnBufLeave()
    let last_time = 0
    let ut = undotree()
    if has_key(ut, 'seq_last')
        let seq_last = ut.seq_last
        let ee = copy(ut.entries)
        call filter(ee, 'v:val.seq == ' . seq_last)
        if len(ee) > 0
            let last_time = ee[0].time
        endif
    endif
    if last_time == 0 && exists('b:TheOldOnes_lastModified')
        unlet b:TheOldOnes_lastModified
    else
        let b:TheOldOnes_lastModified = last_time
    endif
endfunction

function! s:OnBufRead()
    if exists('b:TheOldOnes_lastSaved')    | unlet b:TheOldOnes_lastSaved    | endif
    if exists('b:TheOldOnes_lastModified') | unlet b:TheOldOnes_lastModified | endif

    call s:OnBufEnter()
endfunction

function! s:OnBufWrite()
    let b:TheOldOnes_lastSaved = localtime()
endfunction


function! s:GetTimeline(v, m, s)
    let now = localtime()
    let gaman = g:TheOldOnes_Threshold * 60 "in second

    let timeline = ''

    let vms = [{'name':'v', 'time':a:v, 'status':'v'}
                \ , {'name':'m', 'time':a:m, 'status':'m'}
                \ , {'name':'s', 'time':a:s, 'status':'s'}]
    call filter(vms, 'v:val.time != 0')
    call sort(vms, 's:TlCompare')
    for elem in vms
        if elem.time + gaman < now
            let elem.status = toupper(elem.status)
        endif
        let timeline .= elem.status
    endfor
    return timeline
endfunction

function! s:GetComment(b, tl)
    if getbufvar(a:b, '&modified') == 1
        if stridx(a:tl, 'M') != -1 
            " had been modified and is not saved yet
            return s:COMMENT_NOTSAVED
        endif

    elseif bufname(a:b) == '' && match(a:tl, '\v[mMsS]') == -1
        " nameless and had not been viewed nor been saved
        return s:COMMENT_EMPTYBUFFER

    elseif stridx(a:tl, 'V') != -1
        " had been opened in read-only usage
        return s:COMMENT_NOTREFERRED
    endif

    return ''
endfunction

function! s:TlCompare(v1, v2)
    return a:v2.time - a:v1.time
endfunction

" vim: set et ft=vim sts=4 sw=4 ts=4 tw=78 : 
