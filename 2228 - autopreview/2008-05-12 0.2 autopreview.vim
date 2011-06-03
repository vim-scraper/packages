" Description:
"   Vim plugin for previewing definitions of functions or variables on
"   the cursor automatically like Source Insight, it's great conveniet
"   when grok source codes.
"
" Maintainer: Yang Baohua <yangbaohua[At]gmail.com>
" Last Change: 2008 May 09
" Version: v0.2
"
" Acknowledgements:
"   Thanks to Vim-help mail list
"   thinelephant[At]newsmth
"   Dieken[At]newsmth
"
" Usage:
"   Drop this file to your plugin directory of vim.
"
"   Set g:AutoPreview_enabled to 1 or 0 and g:AutoPreview_allowed_filetypes
"   in your vimrc, here is the default setting:
"
"     let g:AutoPreview_enabled = 0
"     let g:AutoPreview_allowed_filetypes = ["c", "cpp", "java"]
"
"   The file type of current buffer can be checked with `:echo &ft`, and
"   you can call `:AutoPreviewToggle` command to enable or disable this
"   plugin at runtime.
"
"   You'd better set 'updatetime' option to 2000 or even less for good
"   responsibility, see `:help updatetime` for details.
"
" ChangeLog:
"   2008-05-07  Yang Baohua <yangbaohua@gmail.com>
"       * initial inspiration and implementation, release v0.1
"
"   2008-05-09  Liu Yubao <yubao.liu@gmail.com>
"       * cleanup, optimize and enhance, release v0.2


if exists ("loaded_autopreview") || !has("autocmd") || !exists(":filetype")
    finish
endif
let loaded_autopreview = 1


if !exists("g:AutoPreview_enabled")
    let g:AutoPreview_enabled = 0
endif
if !exists("g:AutoPreview_allowed_filetypes")
    let g:AutoPreview_allowed_filetypes = ["c", "cpp", "java"]
endif

command! -nargs=0 -bar AutoPreviewToggle :call s:AutoPreviewToggle()

if g:AutoPreview_enabled
    augroup AutoPreview
        au! FileType * :call s:SetCursorHoldAutoCmd()
    augroup END
endif


func s:AutoPreviewToggle()
    if g:AutoPreview_enabled
        silent! exe "pclose"
        silent! :au! AutoPreview
    else 
        silent! call s:PreviewWord()
        augroup AutoPreview
            au! FileType * :call s:SetCursorHoldAutoCmd()
            let i = 1
            let n = bufnr("$")
            while (i <= n)
                if buflisted(i) && index(g:AutoPreview_allowed_filetypes,
                            \            getbufvar(i, "&ft")) >= 0 &&
                            \   !getbufvar(i, "&previewwindow")
                    exe "au! CursorHold <buffer=" . i . "> nested :call s:PreviewWord()"
                    exe "au! CursorHoldI <buffer=" . i . "> nested :call s:PreviewWord()"
                endif
                let i = i + 1
            endwhile
        augroup END
    endif
    let g:AutoPreview_enabled = !g:AutoPreview_enabled
endfunc


func s:SetCursorHoldAutoCmd()
    if &previewwindow
        return
    endif

    augroup AutoPreview
        if index(g:AutoPreview_allowed_filetypes, &ft) >= 0
            " auto refresh the ptag window
            au! CursorHold <buffer> nested :call s:PreviewWord()
            au! CursorHoldI <buffer> nested :call s:PreviewWord()
            ":echo "set autocmd for " . &ft . " in " . expand("%")
        else
            au! CursorHold <buffer>
            au! CursorHoldI <buffer>
            ":echo "unset autocmd for " . &ft . " in " . expand("%")
        endif
    augroup END
endfunc


func s:PreviewWord()
    let w = expand("<cword>")     " get the word under cursor
    if w =~ '\a'                  " if the word contains a letter
        silent! exe "ptag " . w
    endif
endfun

