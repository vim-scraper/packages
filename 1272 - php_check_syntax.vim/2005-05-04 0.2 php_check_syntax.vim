" php_check_syntax.vim -- Check php syntax when saving or reading a file
" @Author:      Thomas Link (samul@web.de)
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     04-Mai-2005.
" @Last Change: 04-Mai-2005.
" @Revision:    0.2.21

if exists("g:php_check_syntax")
    finish
endif
let g:php_check_syntax = 1

if !exists("g:php_check_syntax_cmd")
    let g:php_check_syntax_cmd='php -l'
endif

function! PhpSyntaxCheck()
    if &filetype == 'php'
        let t  = @t
        let mp = &makeprg
        " let sp = &shellpipe
        let ef = &errorformat
        try
            let &makeprg = g:php_check_syntax_cmd
            " set shellpipe=> 				
            " set errorformat=<b>%*[^<]</b>:\ \ %m\ in\ <b>%f</b>\ on\ line\ <b>%l</b><br\ />
            set errorformat=%*[^:]:\ %m\ in\ %f\ on\ line\ %l
            silent make %
            redir @t
            silent clist
            redir END
            if @t =~ 'No syntax errors detected in '
                cclose
            else
                copen
            endif
        finally
            let @t = t
            let &makeprg     = mp
            " let &shellpipe   = sp
            let &errorformat = ef
        endtry
    endif
endf

" noremap <buffer> <C-F5> :call PhpSyntaxCheck()<cr>
" inoremap <buffer> <C-F5> <esc>:call PhpSyntaxCheck()<cr> 

autocmd BufRead,BufWritePost *.php call PhpSyntaxCheck()

finish
Installation:
Save this file either in ~/.vim/ftplugin/php/ or ~/.vim/plugin/.

Description:
This ftplugin is based on Klaus Horsten's php_console (vimscript #779). In 
opposition to php_console, it only checks the syntax when opening or closing a 
file.

If you use this as a ftplugin, the script will miss the first BufRead event 
as this is when it's being loaded.

If php isn't in your path, set the g:php_check_syntax_cmd variable 
(including the -l switch).

