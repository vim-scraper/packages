" MultiEnc.vim: Script to autodetect multiple encodings
" Author: Wu Yongwei <wuyongwei@gmail.com>
" Licence: LGPL
" $Revision: 1.3 $
" $Date: 2007/02/25 13:19:37 $
"
" Tested with gVim 7.0 under Windows XP

if $MULTIENC_TELLENC == ''
  let $MULTIENC_TELLENC='tellenc'
endif

if !exists('g:multienc_auto_patterns')
  let g:multienc_auto_patterns='*.txt'
endif

if !exists('g:multienc_html_patterns')
  let g:multienc_html_patterns='*.htm{l\=}'
endif

if !exists('g:legacy_encoding')
  if &encoding !~? '^utf' && &encoding !~? '^ucs'
    let g:legacy_encoding=&encoding
  elseif &fileencodings =~? '^ucs-bom,utf-8,[^,]\+'
    let g:legacy_encoding=matchstr(&fileencodings, '^ucs-bom,utf-8,\zs[^,]\+')
  endif
  if !exists('g:legacy_encoding') || g:legacy_encoding == 'default'
    echomsg 'Cannot determine legacy encoding; detecting encoding of' .
          \ ' files with non-ASCII characters may not work.'
    let g:legacy_encoding=''
  endif
endif

let g:multienc_disable_autodetection=0

function! CheckFileEncoding()
  if &modified && &fileencoding != ''
    if g:multienc_disable_autodetection < 2
      exec 'e! ++enc=' . &fileencoding
      syntax on
    elseif exists('s:multienc_manual_enc')
      let &fileencoding=s:multienc_manual_enc
      set nomodified
    endif
  endif
endfunction

function! EditManualEncoding(enc, ...)
  if a:0 > 1
    echoerr 'Only one file name should be supplied'
    return
  endif
  if a:0 == 1
    let filename=' ' . a:1
  else
    let filename=''
  endif
  try
    let g:multienc_disable_autodetection=2
    let s:multienc_manual_enc=a:enc
    exec 'e ++enc=' . a:enc . filename
  finally
    let g:multienc_disable_autodetection=0
    unlet s:multienc_manual_enc
  endtry
endfunction

function! EditAutoEncoding(...)
  if g:multienc_disable_autodetection || !has('iconv')
    return
  endif
  if a:0 > 1
    echoerr 'Only one file name should be supplied'
    return
  endif
  if a:0 == 1
    let filename=iconv(a:1, &encoding, g:legacy_encoding)
    let filename_e=' ' . a:1
  else
    let filename=iconv(expand('%:p'), &encoding, g:legacy_encoding)
    let filename_e=''
  endif
  if a:0 == 1
    try
      let g:multienc_disable_autodetection=1
      exec 'e' . filename_e
    finally
      let g:multienc_disable_autodetection=0
    endtry
  endif
  let result=system($MULTIENC_TELLENC . ' "' . filename . '"')
  let result=substitute(result, '\n$', '', '')
  if v:shell_error != 0
    echo iconv(result, g:legacy_encoding, &encoding)
    return
  endif
  if has('win32') || has('win64')
    if result =~ '^gb'
      let result='cp936'
    endif
  endif
  if result != &fileencoding
    if result == 'binary'
      echo 'Binary file'
      sleep 2
    elseif result == 'unknown'
      echo 'Unknown encoding'
      sleep 2
    else
      try
        let g:multienc_disable_autodetection=1
        exec 'e ++enc=' . result . filename_e
      finally
        let g:multienc_disable_autodetection=0
      endtry
    endif
  endif
endfunction

function! ConvertHtmlEncoding(encoding)
  if a:encoding ==? 'gb2312'
    return 'cp936'            " GB2312 imprecisely means CP936 in HTML
  elseif a:encoding ==? 'iso-8859-1'
    return 'latin1'           " The canonical encoding name in Vim
  elseif a:encoding ==? 'utf8'
    return 'utf-8'            " Other encoding aliases should follow here
  else
    return a:encoding
  endif
endfunction

function! DetectHtmlEncoding()
  if g:multienc_disable_autodetection
    return
  endif
  normal m`
  normal gg
  if search('\c<meta http-equiv=\("\?\)Content-Type\1 content="text/html; charset=[-A-Za-z0-9_]\+">') != 0
    let reg_bak=@"
    normal y$
    let charset=matchstr(@", 'text/html; charset=\zs[-A-Za-z0-9_]\+')
    let charset=ConvertHtmlEncoding(charset)
    normal ``
    let @"=reg_bak
    if &fileencodings == ''
      let auto_encodings=',' . &encoding . ','
    else
      let auto_encodings=',' . &fileencodings . ','
    endif
    if charset !=? &fileencoding &&
       \(auto_encodings =~ ',' . &fileencoding . ',' || &fileencoding == '')
      try
        let g:multienc_disable_autodetection=1
        silent! exec 'e ++enc=' . charset
      finally
        let g:multienc_disable_autodetection=0
      endtry
    endif
  else
    normal ``
    call EditAutoEncoding()
  endif
endfunction

command! -nargs=* -complete=file EditAutoEncoding call
                               \ EditAutoEncoding(<f-args>)
command! -nargs=+ -complete=file EditManualEncoding call
                               \ EditManualEncoding(<f-args>)

exec 'au BufRead ' . g:multienc_auto_patterns .
      \' nested call EditAutoEncoding()'
exec 'au BufRead ' . g:multienc_html_patterns .
      \' nested call DetectHtmlEncoding()'

" Detect file encoding based on modeline
au BufWinEnter * call CheckFileEncoding()

" vim:expandtab shiftwidth=2 textwidth=76
