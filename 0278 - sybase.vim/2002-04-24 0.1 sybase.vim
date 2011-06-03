
" SETUP DB USER AND PASSWORD!!!!!
let g:sqshcom_path = "sqsh -w1000 "
let g:sqshcom_common_commands = ""


function! AE_setDatabase( database )

  let g:sqshcom_db = ""
  let g:sqshcom_userid = ""
  let g:sqshcom_passwd = ""

  if a:database == "DBA"
    let g:sqshcom_db = "FLANGE"
    let g:sqshcom_userid = "WIBBLE"
    let g:sqshcom_passwd = "WOBBLE"
  endif

  if g:sqshcom_db == ""
    echo "Invalid database"   
  endif
endfunction

call AE_setDatabase("DBA")

function! AE_changeDatabase()
  let database = inputdialog("Enter db value (DBA)")
  if (database == "")
    echo "No change made .. please enter a database"
  endif
  call AE_setDatabase(database)

endfunction

function! AE_configureOutputWindow() 
  set ts=8 buftype=nofile nowrap sidescroll=5 listchars+=precedes:<,extends:>
  normal $G
  "Trim empty lines
  while getline(".") == ""
    normal dd
  endwhile
  normal 1G
  let l:newheight = line("$")
  if l:newheight < winheight(0)
    exe "resize " . l:newheight
  endif
endfunction 

function! AE_execQuery( sql_query ) 
  new
  let l:tmpfile = tempname() . ".sql"
  let l:oldo = @o
  let @o="i" . g:sqshcom_common_commands . a:sql_query
  let l:pos = match( @o, ";$" )
  if l:pos < 0
    let @o=@o . ";"
  endif
  let @o=@o . "\n"
  normal @o
  let @o=l:oldo
  exe "silent write " . l:tmpfile
  close
  new
  let l:cmd = g:sqshcom_path . "-U " . g:sqshcom_userid . " -P " . g:sqshcom_passwd . " -S " . g:sqshcom_db
  let l:cmd = l:cmd . " -i" . l:tmpfile
  exe "1,$!" . l:cmd
  call AE_configureOutputWindow()
  call delete( l:tmpfile )
endfunction 

function! AE_execLiteralQuery( sql_query ) 
  let l:query = a:sql_query

  let l:idx = stridx( l:query, "\n" )
  while l:idx >= 0
    let l:query = strpart( l:query, 0, l:idx ) . " " . strpart( l:query, l:idx+1 )
    let l:idx = stridx( l:query, "\n" )
  endwhile

  call AE_execQuery( l:query )
endfunction 

function! AE_execQueryUnderCursor() 
  exe "silent norm! ?\\c[^.]*\\<\\(select\\|update\\|delete\\)\\>\nv/;\nh\"zy"
  noh
  call AE_execLiteralQuery( @z )
endfunction 


" visual mode mappings 1
map <F8> :call AE_changeDatabase () <CR>
vmap <F8> "zy:call AE_execLiteralQuery( @z )<CR>

cabbrev select Select
cabbrev update Update
cabbrev db     DB
cabbrev sql    SQL
