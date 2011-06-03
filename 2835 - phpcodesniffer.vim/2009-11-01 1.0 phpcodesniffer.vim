" phpcodesniffer.vim -- Revisar los estándares de programación con
" PhpCodeSniffer 
" @Author:      Eduardo Magrané , basado en phpchecksyntax de Thomas Link (samul@web.de)
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     31-Oct-2009.
" @Last Change: .
" @Revision:    0.2.21
" @todo Aceptar parámetro de standard que se desea

if exists("g:php_check_codesniffer")
   finish
endif

let g:php_check_codesniffer = 1

if !exists("g:php_check_codesniffer_cmd")
    let g:php_check_codesniffer_cmd='phpcs --standard=Zend --report=emacs'
endif

function! PhpCodeSniffer()
    if &filetype == 'php'
        let t  = @t
        let mp = &makeprg
        let sp = &shellpipe
        let ef = &errorformat
        try
            let &makeprg = g:php_check_codesniffer_cmd
            set shellpipe=> 				
            " set errorformat=%*[^<]:\ \ %m\ in\ %f\ on\ line\ %l
            "set errorformat=%*[^:]:\ %m\ in\ %f\ on\ line\ %l
            set errorformat=%f:%l:%c:\ %m
            silent make %
            redir @t
            silent clist
            redir END
            if @t =~ 'No se han detectado errores '
                cclose
            else
                copen
            endif
        finally
            let @t = t
            let &makeprg     = mp
            let &shellpipe   = sp
            let &errorformat = ef
        endtry
    endif
endf

noremap   :call PhpCodeSniffer()
inoremap   :call PhpCodeSniffer() 

"autocmd BufRead,BufWritePost *.php call PhpCodeSniffer()

finish
Instalación:
Guardar el fichero en la carpeta ~/.vim/plugins

Descripción:
Revisar los estándares de programación sobre el fichero que estamos trabajando.

Este script esta basado sobre el script de Thomas Link (samul@web.de) (phpchecksyntax)

Para lanzar el script debes teclear Control y F5.
