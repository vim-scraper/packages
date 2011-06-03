" Vim html_french plugin.
" Version: 1.3
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

    %s/&Acirc;/Â/eIg
    %s/&Agrave;/À/eIg
    %s/&Auml;/Ä/eIg
    %s/&Ccedil;/Ç/eIg
    %s/&Eacute;/É/eIg
    %s/&Ecirc;/Ê/eIg
    %s/&Egrave;/È/eIg
    %s/&Euml;/Ë/eIg
    %s/&Icirc;/Î/eIg
    %s/&Iuml;/Ï/eIg
    %s/&Ocirc;/Ô/eIg
    %s/&Ucirc;/Û/eIg
    %s/&Ugrave;/Ù/eIg
    %s/&OELig;/Œ/eIg

    %s/&acirc;/â/eIg
    %s/&agrave;/à/eIg
    %s/&auml;/ä/eIg
    %s/&ccedil;/ç/eIg
    %s/&eacute;/é/eIg
    %s/&ecirc;/ê/eIg
    %s/&egrave;/è/eIg
    %s/&euml;/ë/eIg
    %s/&icirc;/î/eIg
    %s/&iuml;/ï/eIg
    %s/&ocirc;/ô/eIg
    %s/&ucirc;/û/eIg
    %s/&ugrave;/ù/eIg
    %s/&oelig;/œ/eIg
    %s/&laquo;/«/eIg
    %s/&raquo;/»/eIg


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

    %s/Â/\&Acirc;/eIg
    %s/À/\&Agrave;/eIg
    %s/Ä/\&Auml;/eIg
    %s/Ç/\&Ccedil;/eIg
    %s/É/\&Eacute;/eIg
    %s/Ê/\&Ecirc;/eIg
    %s/È/\&Egrave;/eIg
    %s/Ë/\&Euml;/eIg
    %s/Î/\&Icirc;/eIg
    %s/Ï/\&Iuml;/eIg
    %s/Ô/\&Ocirc;/eIg
    %s/Û/\&Ucirc;/eIg
    %s/Ù/\&Ugrave;/eIg
    %s/Œ/\&OElig;/eIg

    %s/â/\&acirc;/eIg
    %s/à/\&agrave;/eIg
    %s/ä/\&auml;/eIg
    %s/ç/\&ccedil;/eIg
    %s/é/\&eacute;/eIg
    %s/ê/\&ecirc;/eIg
    %s/è/\&egrave;/eIg
    %s/ë/\&euml;/eIg
    %s/î/\&icirc;/eIg
    %s/ï/\&iuml;/eIg
    %s/ô/\&ocirc;/eIg
    %s/û/\&ucirc;/eIg
    %s/ù/\&ugrave;/eIg
    %s/œ/\&oelig;/eIg
    %s/«/\&laquo;/eIg
    %s/»/\&raquo;/eIg


    let &report=s:save_report
    unlet s:save_report
    call cursor(s:line,s:column)
    unlet s:line
    unlet s:column
endfunction

