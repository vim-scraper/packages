"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Name:		    pdftotext
" Description:	Use the pdftotext program to filter .pdf files into plain text
" Author:	    Marc Prud'hommeaux <mwp1@cornell.edu>
" Last Change:	18 May, 2004
"
" Licence:	    This program is free software; you can redistribute it
"               and/or modify it under the terms of the GNU General Public
"               License. See http://www.gnu.org/copyleft/gpl.txt
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" run pdftotext to read PDF files
autocmd BufReadPost,FileReadPost *.pdf call s:readpdf()
    
fun s:readpdf()
    if (!executable("pdftotext"))
        echo "Error: pdftotext not installed or not in path"
        return
    endif

    let tmp = tempname()
    " invoke: pdftotext sourcefile.pdf tempfile
    call system ("pdftotext '" . escape (expand("<afile>"), "'") . "' " . tmp)
    setlocal nobin
	execute "silent '[-1r " . tmp
    " clean up the temporary file
    call delete(tmp)

    " make the buffer unwritable: we don't want to clobber the PDF file!
    set nowrite
endfun

