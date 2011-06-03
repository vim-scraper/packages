" You can change the colours to ones you like here
let s:pastel_guibg = '#121212'
let s:pastel_ctermbg = 233

" +++ Make it obvious which mode we are in
set laststatus=2 " always show status line

" Makes it VERY obvious if you are in insert mode or not :)
if version >= 700

    function EnterPastel()
        redir => current | silent highlight Normal | redir END
        let current = substitute(current, '\<xxx\>',"  ", "")
        " Real vim (not gvim) leaves this here
        let current = substitute(current, '\<cleared\>',"  ", "")

        " Weird junk char at start
        let current = matchstr(current, '\(Normal.*\)')
        redir => background | silent set background | redir END
        let background = matchstr(background, '\(background=.*\)')

        " As pointed out by Dave Kirby, gvim puts font info into there which
        " breaks things. This also revealed a lot of other things to me which
        " aren't working with gvim, i use it in -v mode and didn't realise so
        " I have tried to make it more GUI user friendly.
        let current_font = matchstr(current,'font=.*$')
        if len(current_font) > 5
            let s:gfn = strpart(current_font,5)
        else
            let s:gfn = ''
        endif

        let current = substitute(current,'font=.*$',"", "")
        let s:highlight_normal = current

        let args = split(current, "")
        call filter(args, 'v:val !~ "guibg"')
        call filter(args, 'v:val !~ "ctermbg"')
        let pastel_normal = 'highlight '.join(args).' ctermbg='.s:pastel_ctermbg.' guibg='.s:pastel_guibg 

        if exists("g:colors_name")
            let colors_name = g:colors_name
            unlet g:colors_name
        endif
        
        if exists("g:syntax_cmd")
            let syntax_cmd = g:syntax_cmd
        endif
        let g:syntax_cmd = "Who you lookin at kid?"

        exec pastel_normal
        exec 'set gfn="'.s:gfn.'"'
        exec 'set '.background

        if exists("colors_name")
            let g:colors_name = colors_name
        endif

        unlet g:syntax_cmd
        if exists("syntax_cmd")
            let g:syntax_cmd = syntax_cmd
        endif 
    endfunction

    function LeavePastel()
        redir => background | silent set background | redir END
        let background = matchstr(background, '\(background=.*\)')
        if exists("g:colors_name")
            let colors_name = g:colors_name
            unlet g:colors_name
        endif
        
        if exists("g:syntax_cmd")
            let syntax_cmd = g:syntax_cmd
        endif
        let g:syntax_cmd = "Who you lookin at kid?"

        highlight clear Normal

        if s:highlight_normal !~ "ctermbg="
            " Thanks godlygeek for this one
            let s:highlight_normal = s:highlight_normal." ctermbg=NONE"
        endif

        if s:highlight_normal !~ "guibg"
            let s:highlight_normal = s:highlight_normal." guibg=NONE"
        endif

        exec 'highlight '.s:highlight_normal
        exec 'set gfn="'.s:gfn.'"'
        exec 'set '.background

        if exists("colors_name")
            let g:colors_name = colors_name
        endif

        unlet g:syntax_cmd
        if exists("syntax_cmd")
            let g:syntax_cmd = syntax_cmd
        endif 
    endfunction

au InsertEnter * hi StatusLine term=reverse ctermfg=DarkRed ctermbg=7 guibg=black
au InsertLeave * hi StatusLine term=reverse ctermfg=7 ctermbg=0 guibg=black
au InsertEnter * call EnterPastel()
au InsertLeave * call LeavePastel()

endif
