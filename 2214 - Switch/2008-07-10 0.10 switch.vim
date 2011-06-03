"        name: Switch
"     summary: Quickly toggle boolean options between 'on' and 'off'
"     version: 0.10.0
"     license: GPL
"    requires: Vim 7+
"   script id: 2214
" last change: 2008 Jul 09
"    rev days: 3                \A/
"     creator: Tomas RC
"     project: Univrc
"       email: univrc@gmail.com
"        site: http://univrc.org/
"
"         use: # get help            : <C-Q><Space> or <C-Q><Tab>
"              # switch option       : <C-Q>{key}

" {{{1 [comments]                 
    " {{{2 [changes]
        " {{{3 \A/ 2008 Jul 07            
"   Andy Wokula <anwoku@yahoo.de>
"
" Added: Added <C-Q><Tab>, which is more intuitive.  If I look for
" completions in the command-line I always try Tab and never Space.
"
" Added: accept a choice also when the user forgets to release CTRL, first
" try the lower then the upper case letter; doesn't work always, e.g. Ctrl-C
" always breaks, Ctrl-I is like <Tab>; where possible, conversion is done:
" Ctrl-@ -> "J".  Another problem: unwanted setting of "m" if Enter is
" pressed, therefore convert Ctrl-M (Enter) to " ".  Conversion to " "
" ignores a flag.
"
" Added: there are three places where options are remembered:
" - the s:sys dictionary
" - s:defaults (string) to remember defaults (does never change)
" - g:switch_options (string) for user settings (to modify or replace
"   defaults)
"
" Added: interface mapping <Plug>SwitchOption in case the user doesn't
" like Ctrl-Q
"
" Added: after printing help with <C-Q><Space>, the screen is not cleared if
" the user continues with Command-line mode (by typing ":" or Ctrl-U to
" clear ":switch").  Very useful to edit the g:switch_options variable, or
" to get help on an option.  And it's more close to the behavior of other
" Vim commands.  In this situation, both keys take precedence over <C-Q>: or
" <C-Q>u mappings.
"
" Added: after printing help with <C-Q><Space>, another <C-Q> redraws and
" shows the ":switch" prompt (instead of trying to set the "q" option).
"
" Added: the user can press <F1> to get help on an option; e.g. to get help
" on 'expandtab' he can press <C-Q><F1>e or <C-Q><Space><F1>e or
" <C-Q><F1><Space><F1>e; pressing <F1> two times in a row quits.  Ok, now we
" have a really big messy function <SID>Switch().
"
" Changed: massive change to the set of available default options
" - removed flags: D:digraph, E:expandtab, f:foldenable, F:rightleft,
"   g:ignorecase, G:magic, K:writebackup, N:icon, O:confirm, o:readonly,
"   t:title, U:autoindent, u:secure, V:revins, v:visualbell
" - changed flags: a:showmatch->a:autoindent, c:cindent->c:ignorecase,
"   e:errorbells->e:expandtab, r:ruler->r:readonly
"
" Changed: now sets the local value of an option (not the global value;
" :setlocal sets the global value anyway if there is no local value)
"
" Changed: include the key that was rejected in the message
"
" Changed: now uses the full screen width to show available options (several
" columns)
"
" Changed: After pressing <C-Q><Space>, don't require another <C-Q>, instead
" show the :switch prompt immediately
"
" Fix: test for non-existant option names caused an error
"
" Fix: the escape key shouldn't give an error (also keys with empty return
" char)
"
" ... some more ...
        " }}}

    " {{{2 [customization]            
"
" g:switch_options          (string), default: "".  Its value can have three
"                           different meanings:
"
"   :let g:switch_options = ""
"
"       Use the default set of options.
"
"   :let g:switch_options = "., e:expandtab, k"
"
"       With ".," at the begin, start with the default set.  Following
"       entries either add/replace "key:option" pairs or remove keys if the
"       ":option" part is missing.  Separator is "," followed by optional
"       white space.  The example makes <C-Q>e toggle 'expandtab' and <C-Q>k
"       raise an error.  (bad example, "e" already is in the defaults).
"
"   :let g:switch_options = "e:expandtab, k"
"
"       Map <C-Q>e as the only available key.  "k" is garbage here.
"
" g:switch_help_header      (boolean), default 1.  If on, show header lines.
"
" g:switch_help_leftalign   (boolean), default 0.  If off, make displaying
"                           the options look more like centering (about 1/3
"                           of white space before, 2/3 after).
"}}}

" {{{1 [script init]              
if exists('loaded_switch')
    finish
endif
let loaded_switch = 1

if v:version < 700 || &cp
    echomsg "Switch: you need at least Vim 7.0 and 'nocp' set"
    finish
endif

" {{{1 [user config]              
if !exists("g:switch_options")
    let g:switch_options = ""
endif

if !exists("g:switch_help_header")
    let g:switch_help_header = 1
endif

if !exists("g:switch_help_leftalign")
    let g:switch_help_leftalign = 0
endif

" {{{1 [mappings]                 
if !hasmapto("<Plug>SwitchOption")
    nmap <silent> <C-Q> <Plug>SwitchOption
endif
nnoremap <Plug>SwitchOption :<C-U>call <SID>Switch()<CR>

" {{{1 [config]                   

" start set of options:
let s:defaults =
            \ "A:autochdir, B:scrollbind, C:cursorcolumn, H:hidden"
            \.", I:infercase, L:cursorline, M:showmode, P:wrapscan"
            \.", R:autoread, S:showcmd, W:autowrite, a:autoindent"
            \.", b:linebreak, c:ignorecase, d:diff, e:expandtab, h:hlsearch"
            \.", i:incsearch, j:joinspaces, k:backup, l:list, m:modifiable"
            \.", n:number, p:paste, r:readonly, s:spell, w:wrap, z:lazyredraw"
" remember the previous g:switch_options value to avoid redundant updates:
let s:old_options = "~(_8(I)"

" {{{1 [functions]                
func! s:GetUserSettings() "{{{
    " if !exists("g:switch_options")
    "     let g:switch_options = ""
    " endif
    if s:old_options == g:switch_options
        return
    endif
    let s:old_options = g:switch_options
    let s:sys = {}
    let modify = g:switch_options[0:1] == ".,"
    if modify || g:switch_options == ""
        for flagmap in split(s:defaults, ',\s*')
            let s:sys[flagmap[0]] = flagmap[2:]
        endfor
        if modify
            let g:switch_options = matchstr(g:switch_options, '^\.,\s*\zs.*')
        endif
    endif
    if g:switch_options != ""
        for flagmap in split(g:switch_options, ',\s*')
            if strlen(flagmap) >= 4
                let s:sys[flagmap[0]] = flagmap[2:]
            else
                sil! unlet s:sys[flagmap[0]]
            endif
        endfor
    endif
    if modify
        let g:switch_options = s:old_options
    endif
endfunc "}}}
func! Switch_PrintOptions() "{{{
    call s:GetUserSettings()

    let nentries = len(s:sys)
    let maxoptlen = max(map(values(s:sys),'strlen(v:val)'))
    let colwidth = 2 + 1 + 3 + maxoptlen + 2
    let ncolumns = (&columns-1) / colwidth
    if ncolumns > 0
        let nlines = nentries / ncolumns + (nentries % ncolumns > 0)
        if g:switch_help_leftalign
            let prewidth = 0
        else
            if nlines > 1
                let ncolumns = nentries / nlines + (nentries % nlines > 0)
                let prewidth = ((&columns-1) - ncolumns*colwidth) / 3
            else
                " the old setting, slightly wrong
                let prewidth = (&columns-1) % colwidth / 3
            endif
        endif
        let prespaces = repeat(" ", prewidth)
        let fmtstr = "  %s : %.". maxoptlen. "s  "
    else
        let ncolumns = 1
        let nlines = nentries
        let prespaces = ""
        let fmtstr = "%s : %.". maxoptlen. "s"
    endif

    if g:switch_help_header
        let headline = prespaces. "  Dictionary of boolean options  "
        if strlen(headline) <= &columns - 1
            echo prespaces. "  Get help: Press F1 {flag}"
            echo "\n"
        else
            let headline = prespaces. "  Options  "
        endif
        echohl PreProc
        echo headline
        echohl none
        echo "\n"
    endif

    let list = []
    for entry in items(s:sys)
        call add(list, [entry[1], entry[0]])
    endfor
    call sort(list)

    let filler = repeat(" ", maxoptlen)
    let lidx = 0
    while lidx < nlines
        let line = prespaces
        let cidx = lidx
        while cidx < nentries
            let entry = list[cidx]
            let line .= printf(fmtstr, entry[1], entry[0].filler)
            let cidx += nlines
        endwhile
        echo line
        let lidx += 1
    endwhile
    echo "\n"
    " not a bug: it is normal if not all columns are used
endfunc "}}}
func! <SID>Switch(...) "{{{
    " a:1 -- (boolean) redraw after showing help (internal use only)
    let recursive = a:0>=1 && a:1
    echo ":switch "
    let gotchar = getchar()
    let showhelp = gotchar=="\<F1>"
    let flag = nr2char(gotchar)
    if recursive
        if flag =~ "[:\<C-U>]"
            call feedkeys(":")
            return
        elseif flag =~ '\s'
            redraw
            return
        elseif flag == "\<C-Q>"
            redraw
            call feedkeys("\<Plug>SwitchOption")
            return
        endif
    endif
    if showhelp
        if !recursive
            redraw
        endif
        echo ":switch help "
        let flag = nr2char(getchar())
        if recursive && flag =~ '\s'
            redraw
            return
        endif
    endif
    if flag == "" || flag == "\e"
        redraw
        return
    elseif flag =~ '\s'
        call Switch_PrintOptions()
        call <SID>Switch(1)
        return
    endif
    call s:DoOption(flag, showhelp)
endfunc "}}}
func! s:DoOption(flag, showhelp) "{{{
    let flag = a:flag
    call s:GetUserSettings()
    try
        let sav_dy = &dy
        set display-=uhex
        " strtrans() depends on 'display'
        let optname = ""
        if has_key(s:sys, flag)
            let optname = s:sys[flag]
        else
            let tryctrl = tr(strtrans(flag),"@M","J ")
            if strlen(tryctrl)==2 && tryctrl[0] == "^"
                let flag = tolower(tryctrl[1])
                if has_key(s:sys, flag)
                    let optname = s:sys[flag]
                else
                    let flag = toupper(tryctrl[1])
                    if has_key(s:sys, flag)
                        let optname = s:sys[flag]
                    else
                        let flag = tolower(flag)
                    endif
                endif
            endif
        endif
        if flag == " "
            return
        endif
        redraw
        if optname == ""
            echohl WarningMsg
            echomsg 'Switch: Key "'.strtrans(flag).'" not found in the boolean options dictionary!'
            echohl none
            return
        endif
        if exists("+". optname)
            if a:showhelp
                exec "help '". s:sys[flag]."'"
            else
                exec "setlocal" optname."!" optname."?"
            endif
        else
            echohl WarningMsg
            echo "Switch: Not a working option: '". optname. "'"
            echohl none
            return 1
        endif
    finally
        let &dy = sav_dy
    endtry
endfunc "}}}

" {{{1 [modeline]                 
" vim:set fen fdm=marker fdl=0 et: ;;edswitch
