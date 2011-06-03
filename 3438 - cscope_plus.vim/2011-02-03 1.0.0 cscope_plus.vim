" Vim global plugin for binding custom indent file per project.
" Last Change:	2011 Feb 03
" Maintainer:	Konstantin Lepa <konstantin.lepa@gmail.com>
" License:      MIT
" Version:      1.0.0
"
" Changes {{{
" 1.0.0 2011-02-03
"   Initial upload.
"}}}
"

if exists("b:loaded_cscope_plus")
    finish
endif
let b:loaded_cscope_plus = 1

if !exists("g:cscope_plus_disable")
    let g:cscope_plus_disable = 0
endif

if !exists("*s:AddCscopeConnection")
    function s:AddCscopeConnection()
        setlocal nocsverb
        if filereadable(expand("%:p:h") . "/cscope.out")
            let b:CSCOPE_PLUS_connection = expand("%:p:h") . "/cscope.out"
            exec "cs add " . b:CSCOPE_PLUS_connection
        elseif filereadable("cscope.out")
            let b:CSCOPE_PLUS_connection = "cscope.out"
            exec "cs add " . b:CSCOPE_PLUS_connection
        elseif exists("$CSCOPE_DB") && filereadable("$CSCOPE_DB")
            let b:CSCOPE_PLUS_connection = $CSCOPE_DB
            exec "cs add " . b:CSCOPE_PLUS_connection
        endif
        setlocal csverb
    endfunction
endif

if !exists("*s:RemoveCscopeConnection")
    function s:RemoveCscopeConnection()
        setlocal nocsverb
        if exists("b:CSCOPE_PLUS_connection")
            exec "cs kill " . b:CSCOPE_PLUS_connection
        endif
        setlocal csverb
    endfunction
endif

if has("cscope") && g:cscope_plus_disable == 0
	setlocal csto=0
	setlocal cst

    nmap <buffer> <C-_>s :cs find s <C-R>=expand("<cword>")<CR><CR>
    nmap <buffer> <C-_>g :cs find g <C-R>=expand("<cword>")<CR><CR>
    nmap <buffer> <C-_>c :cs find c <C-R>=expand("<cword>")<CR><CR>
    nmap <buffer> <C-_>t :cs find t <C-R>=expand("<cword>")<CR><CR>
    nmap <buffer> <C-_>e :cs find e <C-R>=expand("<cword>")<CR><CR>
    nmap <buffer> <C-_>f :cs find f <C-R>=expand("<cfile>")<CR><CR>
    nmap <buffer> <C-_>i :cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
    nmap <buffer> <C-_>d :cs find d <C-R>=expand("<cword>")<CR><CR>

    nmap <buffer> <C-_><C-_>s :scs find s <C-R>=expand("<cword>")<CR><CR>
    nmap <buffer> <C-_><C-_>g :scs find g <C-R>=expand("<cword>")<CR><CR>
    nmap <buffer> <C-_><C-_>c :scs find c <C-R>=expand("<cword>")<CR><CR>
    nmap <buffer> <C-_><C-_>t :scs find t <C-R>=expand("<cword>")<CR><CR>
    nmap <buffer> <C-_><C-_>e :scs find e <C-R>=expand("<cword>")<CR><CR>
    nmap <buffer> <C-_><C-_>f :scs find f <C-R>=expand("<cfile>")<CR><CR>
    nmap <buffer> <C-_><C-_>i :scs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
    nmap <buffer> <C-_><C-_>d :scs find d <C-R>=expand("<cword>")<CR><CR>

    au BufWinEnter <buffer> call s:AddCscopeConnection()
    au BufWinLeave <buffer> call s:RemoveCscopeConnection()
endif

