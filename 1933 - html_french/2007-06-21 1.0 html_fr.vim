" Vim html_french plugin.
" Version: 1.0
" 
" Description:
" When loading a html file, this plugin replaces all
" HTML-coded French characters (like &eacute;) with the 
" normal representation ().
"
" ripped from the Danish version by sune vuorela, based on the html_umlaute by Timo Teifel <timo at teifel-net dot de>.
" and the html_portuguese by Rubens Marins <rubens at windstar dot com dot br>
" Maintainer: Roger Pilkey <rpilkey@gmail.com>

" Usage:
" It does everything automatically. When reading a file,
" it replaces (&eacute; , &agrave; etc) with the corresponding 
" character encoding.
" When saving, it replaces the special Characters with the
" html-Code, undoing it after the write, to keep the chars
" if you keep working with the file.
"
" Installation:
" Save this file in vim plugin dir ( /usr/share/vim/vim61/plugin) or 
" ~/.vim/plugin/html_french.vim.
"
" Licence: GPL 
" 
" Changelog:
" v1.0 
"  - initial release

" do this only once per buffer:
if exists("b:loaded_html_french")
    finish
endif
let b:loaded_html_french = 1

if has("autocmd")
augroup html_french
    au!
    au FileType      html,php			call s:Html2Char()
    au BufWrite      *.shtml,*.html,*.htm,*.php		call s:Char2Html()
    au BufWritePost  *.shtml,*.html,*.htm,*.php		call s:Html2Char()
augroup END
endif


" functions need to be sourced only once per session
if exists("s:loaded_html_french_functions")
  finish
endif
let s:loaded_html_french_functions = 1

function s:Html2Char()
    " remember cursor position:
	let s:line = line(".")
	let s:column = col(".")
    " if more than 'report' substitutions have been done, vim 
    " displays it.
    let s:save_report = &report
    set report=99999
    "french special characters

    %s/&Agrave;/�/eIg
    %s/&Acirc;/�/eIg
    %s/&Auml;/�/eIg
    %s/&Ccedil;/�/eIg
    %s/&Eacute;/�/eIg
    %s/&Egrave;/�/eIg
    %s/&Euml;/�/eIg
    %s/&Ecirc;/�/eIg
    %s/&Iuml;/�/eIg
    %s/&Ocirc;/�/eIg
    %s/&Ucirc;/�/eIg
    %s/&OELig;/�/eIg

    %s/&agrave;/�/eIg
    %s/&acirc;/�/eIg
    %s/&auml;/�/eIg
    %s/&ccedil;/�/eIg
    %s/&eacute;/�/eIg
    %s/&egrave;/�/eIg
    %s/&ecirc;/�/eIg
    %s/&euml;/�/eIg
    %s/&iuml;/�/eIg
    %s/&ocirc;/�/eIg
    %s/&ucirc;/�/eIg
    %s/&oelig;/�/eIg


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
    " french special characters
	
    %s/�/\&Agrave;/eIg
    %s/�/\&Acirc;/eIg
    %s/�/\&Auml;/eIg
    %s/�/\&Ccedil;/eIg
    %s/�/\&Eacute;/eIg
    %s/�/\&Egrave;/eIg
    %s/�/\&Ecirc;/eIg
    %s/�/\&Euml;/eIg
    %s/�/\&Iuml;/eIg
    %s/�/\&Ocirc;/eIg
    %s/�/\&Ucirc;/eIg
    %s/�/\&OElig;/eIg

    %s/�/\&agrave;/eIg
    %s/�/\&acirc;/eIg
    %s/�/\&auml;/eIg
    %s/�/\&ccedil;/eIg
    %s/�/\&eacute;/eIg
    %s/�/\&egrave;/eIg
    %s/�/\&ecirc;/eIg
    %s/�/\&euml;/eIg
    %s/�/\&iuml;/eIg
    %s/�/\&ocirc;/eIg
    %s/�/\&ucirc;/eIg
    %s/�/\&oelig;/eIg


    let &report=s:save_report
    unlet s:save_report
    call cursor(s:line,s:column)
    unlet s:line
    unlet s:column
endfunction

