"Author:  Fvw (vimtexhappy@gmail.com)
"         Buffer list in statusline
"         2009-01-02 23:57:48
"License: Copyright (c) 2001-2009, Fvw
"         GNU General Public License version 2 for more details.

autocmd VimEnter,BufNew,BufEnter,BufWritePost * call UpdateStatus()
"if version >= 700
"autocmd InsertLeave,VimResized * call UpdateStatus()
"end
for i in range(0,9,1)
    exec "silent! noremap <M-".i."> :call BufChange(".i.")<CR>"
    exec "silent! noremap <leader>".i." :call BufSplit(".i.")<CR>"
endfor

hi NowBuf term=bold ctermfg=Cyan guifg=green guibg=blue gui=bold
set statusline=%m\{%{&ff}:%{&fenc}:%Y}\ %{g:bufBef}%#NowBuf#%{g:bufCur}%#StatusLine#%{g:bufAft}%=%l,%c,%P,%L%<

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
    if !empty(get(s:bufList, a:idx, []))
        exec 'b! '.s:bufList[a:idx][0]
    endif
endfunction
function! BufSplit(idx)
    if !empty(get(s:bufList, a:idx, []))
        exec 'sb! '.s:bufList[a:idx][0]
    endif
endfunction

function! UpdateStatus()
    let s:bufList = []

    let idxCur = -1
    let [i,idx] = [1, 0]
    while(i <= bufnr('$'))
        if buflisted(i) && getbufvar(i, "&modifiable")
            let buf  =  idx."-"
            "let buf .= fnamemodify(bufname(i), ":t")."(".i.")"
            let buf .= fnamemodify(bufname(i), ":t")
            let buf .= getbufvar(i, "&modified")? "+":''
            let buf .= " "
            if bufwinnr(i) == winnr()
                let idxCur = idx
            endif
            call add(s:bufList, [i, buf])
            let idx += 1
        endif
        let i += 1
    endwhile

    let [g:bufBef, g:bufCur, g:bufAft] = ['','','']
    if empty(s:bufList)
        return
    endif

    if idxCur != -1
        let g:bufCur = s:bufList[idxCur][1]
    endif

    let i = idxCur - 1
    let widthForBef = winwidth(0) - len(g:bufCur) - 20
    while i >= 0
        let try = s:bufList[i][1].g:bufBef
        if len(try) > widthForBef
            let g:bufBef = '<-'.g:bufBef
            break
        else
            let g:bufBef = try
        endif
        let i -= 1
    endwhile

    let i = idxCur + 1
    let len = len(s:bufList)
    while i < len
        let g:bufAft .= s:bufList[i][1]
        let i += 1
    endwhile

endfunction
