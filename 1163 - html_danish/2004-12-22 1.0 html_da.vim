" Vim html_danish plugin.
" Version: 1.1
" 
" Description:
" When loading a html file, this plugin replaces all
" HTML-coded danish characters (like &aring;) with the 
" normal representation (å).
"
" Is based on the html_umlaute by Timo Teifel <timo at teifel-net dot de>.
" and the html_porteguese by Rubens Marins <rubens at windstar dot com dot br>
" Mantainer: Sune Vuorela <sune at pusling dot com>

" Usage:
" It does everything automatically. When reading a file,
" it replaces (&aring;, &AElig; etc) with the corresponding 
" character encoding.
" When saving, it replaces the special Characters with the
" html-Code, undoing it after the write, to keep the chars
" if you keep working with the file.
"
" Installation:
" Save this file in vim plugin dir ( /usr/share/vim/vim61/plugin) or 
" ~/.vim/plugin/html_danish.vim.
"
" Licence: GPL 
" 
" Changelog:
" v1.0 23/12/2004
"  - initial release

" do this only once per buffer:
if exists("b:loaded_html_danish")
    finish
endif
let b:loaded_html_danish = 1

if has("autocmd")
augroup html_danish
    au!
    au FileType      html,php			call s:Html2Char()
    au BufWrite      *.html,*.htm,*.php		call s:Char2Html()
    au BufWritePost  *.html,*.htm,*.php		call s:Html2Char()
augroup END
endif


" functions need to be sourced only once per session
if exists("s:loaded_html_danish_functions")
  finish
endif
let s:loaded_html_danish_functions = 1

function s:Html2Char()
    " remember cursor position:
	let s:line = line(".")
	let s:column = col(".")
    " if more than 'report' substitutions have been done, vim 
    " displays it.
    let s:save_report = &report
    set report=99999
    "Danish special characters
    %s/&aelig;/æ/eIg
    %s/&AElig;/Æ/eIg
    %s/&oslash;/ø/eIg
    %s/&Oslash;/Ø/eIg
    %s/&aring;/å/eIg
    %s/&Aring;/Å/eIg
    let &report=s:save_report
    unlet s:save_report
    call cursor(s:line,s:column)
    unlet s:line
    unlet s:column
endfunction

function s:Char2Html()
	let s:line = line(".")
	let s:column = col(".")
    let s:save_report = &report
    set report=99999
    " Danish special characters
    %s/æ/\&aelig;/eIg
    %s/Æ/\&AElig;/eIg
    %s/ø/\&oslash;/eIg
    %s/Ø/\&Oslash;/eIg
    %s/å/\&aring;/eIg
    %s/Å/\&Aring;/eIg
    let &report=s:save_report
    unlet s:save_report
    call cursor(s:line,s:column)
    unlet s:line
    unlet s:column
endfunction

