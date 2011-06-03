" This VIM sript automates download list generation
" for GetRight, when You have a list of URL's

if !exists( "DestDir" )
  let DestDir = 'D:\DownLoads\'
endif

function! MakeList()
gl /^/ put =''
gl /^$/ - copy -
gl /^$/ -- s/^/URL: /
exe "gl /^$/ - s?^.*\/?File: ".escape( g:DestDir, '\' )."?"
gl /^$/ -s/$/.GetRight/
endfunction

function! UnHref()
  let iii = &ic
  set ignorecase
  let ptrn = 'href=".\{-}["#]'
  let l = line('.')
  let ll = getline( l )
  while ( ll =~? ptrn )
    let ss = match( ll, ptrn )
    let ee = matchend( ll, ptrn )
    call append( l, strpart( ll, ss + 6, ee - ss - 7 ) )
    " truncate line 
    let ll = strpart( ll, ee, strlen( ll ) - ee ) 
  endwhile
  execute l.' delete'
  let &ic = iii
endfunction

:nnoremap <F4> :let DestDir = ''<Left>
:imap <F4> <C-O><F4>

:nnoremap <F6> :%call UnHref()<Return>
:imap <F6> <C-O><F6>

:nnoremap <F7> :call MakeList()<Return>
:imap <F7> <C-O><F7>

:echo "F4 - :let DestDir = '' ~ sets the destination directory"
:echo 'F6 - :%call UnHref()   ~ removes everything execpt URLs'
:echo 'F7 - :call MakeList()  ~ converts URLs to GetRight list'

