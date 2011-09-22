"Author:  zzsu (vimtexhappy@gmail.com)
"         Buffer list in statusline
"         2011-02-14 07:07:48 v4.0
"License: Copyright (c) 2001-2009, zzsu
"         GNU General Public License version 2 for more details.

if exists('loaded_buf_it') || &cp
    finish
endif

let g:showInStatusbar = 1

autocmd VimEnter,BufNew,BufEnter,BufWritePost,VimResized * call UpdateStatus()
autocmd InsertLeave * call BufEcho()

noremap  <m-[>      :call BufPrevPart()<cr>
noremap  <m-]>      :call BufNextPart()<cr>
noremap  <m-space>  :call BufEcho()<cr>
noremap  <leader>bo :call BufOnly()<cr>

let g:bufBStr = ""
let g:bufNStr= ""
let g:bufAStr= ""
let g:statusbarKeepWidth = 20
hi NowBuf term=bold ctermfg=Cyan guifg=green guibg=blue gui=bold

if exists("g:showInStatusbar")
    if !exists("g:statusbarUsrDef") || g:statusbarUsrDef == 0
        set statusline=%m\{%{&ff}:%{&fenc}:%Y}\ %{g:bufBStr}%#NowBuf#%{g:bufNStr}%#StatusLine#%{g:bufAStr}%<%=%l,%c,%P,%L%<
    endif
endif

let s:bufNowPartIdx = 0
let s:bufs = {}
let s:bufnrs = {}
let s:bufPartStrList = []

function! BufUnMap()
    for i in keys(s:bufs)
        if i < 10
            exec "silent! unmap <M-".i.">"
        else
            exec "silent! unmap <M-".i/10."><M-".i%10.">"
        endif
        exec "silent! unmap <leader>".i
    endfor
endfun

function! BufMap()
    for i in keys(s:bufs)
        if i < 10
            exec "silent! noremap <M-".i."> :call BufChange(".i.")<CR>"
        else
            exec "silent! noremap <M-".i/10."><M-".i%10."> :call BufChange(".i.")<CR>"
        endif
        exec "silent! noremap <leader>".i." :call BufSplit(".i.")<CR>"
    endfor
endfunction

function! BufOnly()
    let i = 1
    while(i <= bufnr('$'))
        if buflisted(i) && getbufvar(i, "&modifiable")
                    \   && (bufwinnr(i) != winnr())
            exec 'bw'.i
        endif
        let i = i + 1
    endwhile
    call UpdateStatus()
endfun

function! BufChange(idx)
    exec 'b! '.s:bufnrs[a:idx]
endfunction

function! BufSplit(idx)
    exec 'sb! '.s:bufnrs[a:idx]
endfunction

function! BufNextPart()
    let s:bufNowPartIdx += 1
    if s:bufNowPartIdx >= len(s:bufPartStrList)
        let s:bufNowPartIdx = 0
    endif
    call UpdateBufPartStr()
endfunction

function! BufPrevPart()
    let s:bufNowPartIdx -= 1
    if s:bufNowPartIdx < 0
        let s:bufNowPartIdx = len(s:bufPartStrList)-1
    endif
    call UpdateBufPartStr()
endfunction

function! UpdateBufPartStr()
    let [g:bufBStr, g:bufNStr, g:bufAStr] = s:bufPartStrList[s:bufNowPartIdx]
    if s:bufNowPartIdx > 0
        let g:bufBStr = '<<'.g:bufBStr
    endif
    if s:bufNowPartIdx < len(s:bufPartStrList)-1
        let g:bufAStr = g:bufAStr.'>>'
    endif
    call BufEcho()
endfunction

function! UpdateStatus()
    call BufUnMap()
    let s:bufs = {}
    let s:bufNowPartIdx = 0
    let s:bufnrs = {}
    let s:bufPartStrList = []
    let idx = 1
    let i = 1
    while(i <= bufnr('$'))
        if buflisted(i) && getbufvar(i, "&modifiable")
            let bufName  =  idx."-"
            let bufName .= fnamemodify(bufname(i), ":t")
            let bufName .= getbufvar(i, "&modified")? "+":''
            let bufName .= " "
            let s:bufs[idx] = bufName
            let s:bufnrs[idx] = i
            let idx += 1
        endif
        let i += 1
    endwhile

    if empty(s:bufs)
        return
    endif

    let widthForBufStr = winwidth(0) - g:statusbarKeepWidth
    let [POSB,POSN,POSA] = [0, 1, 2]
    let [strB,strN,strA] = ["", "", ""]
    let strPos = POSB
    let partIdx = 0
    for i in sort(keys(s:bufs))
        let bufName = s:bufs[i]
        if bufnr("%") == s:bufnrs[i]
            let strPos = POSN
        endif
        if strPos == POSB
            let strB .= bufName
        elseif strPos == POSN
            let strN .= bufName
            let strPos = POSA
            let s:bufNowPartIdx = partIdx
        elseif strPos == POSA
            let strA .= bufName
        endif
        if len(strB.strN.strA.bufName) > widthForBufStr
            call add(s:bufPartStrList, [strB, strN, strA])
            let [strB,strN,strA] = ["", "", ""]
            let partIdx += 1
        endif
    endfor
    if strB != "" || strN != "" || strA != ""
        call add(s:bufPartStrList, [strB, strN, strA])
    endif

    call UpdateBufPartStr()
    call BufMap()
    call BufEcho()
endfunction

function! BufEcho()
    redraw
    let msg = g:bufBStr.'['.g:bufNStr.']'.g:bufAStr
    echo msg
endfunction
