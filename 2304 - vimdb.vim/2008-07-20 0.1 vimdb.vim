" Description: Vim plugin to simulate a simple database
" Last Change: 2008 Jul 20
" Author:      Benjamin Schnitzler <benschni at yahoo dot de>
" License:     This file is placed in the public domain.
"
" WARNING: Look at Warning at line 17
" Recommendation:
" Place the following command into your .vimrc for a better overview:
" au BufRead,BufNewFile *.vim set foldmarker=func,endfunc
" Look_also_at: Settings and Mapping made locally for Database in
" Function StartDb(); beware of a lot of local autocommands
"
" TODO:
" Write TODO


"(very) Global Settings and WARNING:
"virtualedit is necessary for some special characters which have not
"bytelength. It may cause Problems with other scripts.
"If you plan to unset it, you have to change the cursor command used 
"in RecordbufMove() and RecordbufMoveI() a bit.
set virtualedit=all

"Autocommands
au BufRead,BufNewFile                *.vimdb   call StartDb()
au CursorMoved,CursorMovedI,BufEnter *.vimdb   call TableChange()
au BufLeave                          *.vimdb   call LeaveDb()

au BufEnter,BufRead                  recordbuf call RecordbufMove()
au BufEnter,BufRead                  recordbuf call EnterRecordbuf()
au CursorMoved                       recordbuf call RecordbufMove()
au InsertEnter                       recordbuf call RecordbufEnterI()
au CursorMovedI                      recordbuf call RecordbufMoveI()
au InsertLeave                       recordbuf call RecordbufLeaveI()


func ToggleAutocmd()
  if s:AUTO == 1
    let s:AUTO = 0
    au! CursorMoved,CursorMovedI,BufEnter *.vimdb  
    au! BufRead,BufNewFile                *.vimdb  
    au! BufLeave                          *.vimdb
    au! BufEnter,BufRead                  recordbuf
    au! InsertEnter                       recordbuf
    au! CursorMoved                       recordbuf
    au! CursorMovedI                      recordbuf
    au! InsertLeave                       recordbuf
  elseif s:AUTO == 0
    au CursorMoved,CursorMovedI,BufEnter *.vimdb   call TableChange()
    au BufRead,BufNewFile                *.vimdb   call StartDb()
    au BufLeave                          *.vimdb   call LeaveDb()
    au BufEnter,BufRead                  recordbuf call EnterRecordbuf()
    au BufEnter,BufRead                  recordbuf call RecordbufMove()
    au InsertEnter                       recordbuf call RecordbufEnterI()
    au CursorMoved                       recordbuf call RecordbufMove()
    au CursorMovedI                      recordbuf call RecordbufMoveI()
    au InsertLeave                       recordbuf call RecordbufLeaveI()
    let s:AUTO = 1
  endif
endfunc


"Autocommand Functions for *.vimdb
func StartDb()

  let s:AUTO = 1

  let s:DBFILE = bufname("")
  let s:DBPOS = getpos(".")

  let s:COLLIST = []

  let s:DBHEAD = []
  let s:LONGEST = 0
  let s:BLONGEST = 0
  call RefreshHeader()

  setlocal tw=0
  setlocal bufhidden=hide


  badd recordbuf
  bnext

  setlocal tw=0
  setlocal buftype=nofile
  setlocal bufhidden=hide
  setlocal noswapfile

  map o :call AddTableCol("o")<CR>
  map O :call AddTableCol("O")<CR>

  execute 'vsplit ' . s:DBFILE
endfunc

func TableChange()
  let s:DBPOS = getpos('.')

    b! recordbuf
    call RefreshRecord()
    execute ':b ' . s:DBFILE

  call setpos('.',s:DBPOS)
endfunc

func LeaveDb()
  let s:DBPOS = getpos('.')
endfunc


"Autocommand Functions for recordbuf
func EnterRecordbuf()
  if !bufloaded(s:DBFILE)
    q
    call ToggleAutocmd()
    return
  endif

  let s:RECPOS = getpos('.')
  let s:RECBUF = getline('.')
  let s:RECLINES = line('$')
endfunc

func RecordbufMove()
  if virtcol('.') <= s:LONGEST+1
    call cursor(0,0,s:LONGEST+1)
    "call cursor(0,s:BLONGEST+1)
  endif
  if virtcol('.') <= s:LONGEST+1
    execute "s/.*$/" . escape(s:DBHEAD[line('.')-1],'\/') . " -"
    call cursor(0,0,s:LONGEST+1)
    "call cursor(0,s:BLONGEST+1)
  endif
endfunc

func RecordbufEnterI()
  let s:ADDCOL = "false"
  let s:RECPOS = getpos('.')
  let s:RECBUF = getline('.')
  let s:RECLINES = line('$')
endfunc

func RecordbufMoveI()
  if line('$') < s:RECLINES
    call RefreshRecord()
    call setpos('.',s:RECPOS)
  endif
  if virtcol('.') <= s:LONGEST+1 || line('.') != s:RECPOS[1]
    call setpos('.',s:RECPOS)
    execute "s/.*$/" . escape(s:RECBUF,'\/')
    call setpos('.',s:RECPOS)
  endif
endfunc

func RecordbufLeaveI()
  call ChangeTableEntry()
  if s:DBPOS[1] == 1
    call RefreshHeader()
  endif
endfunc


"Functions primarily accessed by autocommand Functions
func RefreshHeader()
  let s:DBHEAD = split(getbufline(s:DBFILE,1)[0],'\s*<H>\s*', 1)
  let LEN = len(s:DBHEAD)
  let s:DBHEAD[LEN-1] = DelTrailingSpaces(s:DBHEAD[LEN-1])

  let s:LONGEST = 0
  let s:BLONGEST = 0
  for ITEM in s:DBHEAD
    let ILEN = Strlen(ITEM)
    if ILEN > s:LONGEST
      let s:LONGEST = ILEN
      let s:BLONGEST = strlen(ITEM . " ")
    endif
  endfor

  let INDEX = 0
  while INDEX < len(s:DBHEAD)
     let LEN = s:LONGEST - Strlen(s:DBHEAD[INDEX])
     while LEN > 0
       let s:DBHEAD[INDEX] = s:DBHEAD[INDEX] . " "
       let LEN = LEN - 1
     endwhile
     let INDEX = INDEX + 1
  endwhile
endfunc

func RefreshRecord()
  call ToggleAutocmd()

  let LISTE=split(getbufline(s:DBFILE, s:DBPOS[1])[0],'\s*<H>\s*',1)

  %d _
  let INDEX = 0
  while INDEX < len(s:DBHEAD)
    let ENTRY = s:DBHEAD[INDEX] . " " . get(LISTE,INDEX,"-")
    call append(line('$'),ENTRY)
    let INDEX = INDEX + 1
  endwhile
  move 0
  d _

  call ToggleAutocmd()
endfunc

func ChangeTableEntry()
  call ToggleAutocmd()

  let RECENTRY = DelTrailingSpaces( strpart(getline('.'),s:BLONGEST) )
  let RECENTRYL = Strlen(RECENTRY)

  let RECLINE = line('.')
  let TCOL = RECLINE

  execute ':b ' . s:DBFILE
  let RECENTRY = RECENTRY == "" ? "-" : RECENTRY

  call InsertTableCell(RECENTRY, s:DBPOS[1], TCOL-1)
  call AdjustColWidth( TCOL )

  b! recordbuf

  call ToggleAutocmd()
endfunc


"AddTableCol Functions
func AddTableCol( MODE )
  call ToggleAutocmd()
  let s:AUTO = 2

  let PREPEND = a:MODE ==# "O"

  call append(line('.') - PREPEND,"")
  call cursor(line('.') + 1 - PREPEND*2, 0)
  startinsert

  let s:RECPOS = getpos('.')
  let s:RECLINES = s:RECLINES + 1

  call InsertTableCol(s:RECPOS[1] - 1)

  call setpos('.',s:RECPOS)

  au CursorMovedI recordbuf call RecordbufMoveI_AddTableCol()
  au InsertLeave  recordbuf call RecordbufLeaveI_AddTableCol()
endfunc

func InsertTableCol( COL )
  let BUFFER = bufname("")
  execute "b! " . s:DBFILE

  call insert(s:DBHEAD,"-",a:COL)
  let LEN = s:LONGEST - Strlen(s:DBHEAD[a:COL])
  while LEN > 0
    let s:DBHEAD[a:COL] = s:DBHEAD[a:COL] . " "
    let LEN = LEN - 1
  endwhile

  let SEPERATOR = a:COL == len(s:DBHEAD)-1 ? "<H>-" : "-<H>"
  execute "%s/^\\(\\(.\\{-}\\(<H>\\|$\\)\\)\\{" . a:COL . "}\\)\\(.\\{-}\\)\\(<H>\\|$\\)/\\1" . SEPERATOR . "\\4\\5"

  execute "b! " . BUFFER
endfunc

func RecordbufMoveI_AddTableCol()
  if line('$') < s:RECLINES
    call RefreshRecord()
    call setpos('.',s:RECPOS)
    s/^.*$//
  endif
  if line('.') != s:RECPOS[1]
    call RefreshRecord()
    call setpos('.',s:RECPOS)
    s/^.*$//
  endif
endfunc

func RecordbufLeaveI_AddTableCol()
  let NEWCOL = line('.')
  let NEWCOLE = getline('.')
  let s:DBHEAD[NEWCOL-1] = DelTrailingSpaces(NEWCOLE)
  let s:DBHEAD[NEWCOL-1] = s:DBHEAD[NEWCOL-1] == "" ? "-" : s:DBHEAD[NEWCOL-1]

  call InsertTableCell( s:DBHEAD[NEWCOL-1], 1, NEWCOL-1 )

  call RefreshHeader()
  call RefreshRecord()
  call AdjustColWidth( NEWCOL )

  au! CursorMovedI recordbuf
  au! InsertLeave  recordbuf
  let s:AUTO = 0
  call ToggleAutocmd()
endfunc

"CutTableCol Functions
func CutTableCol( MODE )
  call ToggleAutocmd()
  let s.AUTO = 2

  d

  let s.AUTO = 0
  call ToggleAutocmd()
endfunc


"Other Utilities
func CreateColList( COLLIST, COL )
  call extend( a:COLLIST, getbufline(s:DBFILE,1,"$") )
  let LONGEST = 0

  let INDEX = 0
  while INDEX < len(a:COLLIST)
    let RECORD = split(a:COLLIST[INDEX],'\s*<H>\s*')
    let a:COLLIST[INDEX] = get(RECORD,a:COL-1,"-")

    let TMPLEN = Strlen(a:COLLIST[INDEX])
    if LONGEST < TMPLEN
      let LONGEST = TMPLEN
    endif
    let INDEX += 1
  endwhile

  return LONGEST
endfunc

func InsertTableCell( ENTRY, LINE, COL )
  let BUFFER = bufname("")
  execute "b " . s:DBFILE

  let ENTRY = escape(a:ENTRY, '\/')
  execute a:LINE . "s/^\\(\\(.\\{-}<H>\\)\\{" . a:COL . "}\\).\\{-}\\(<H>\\|$\\)/\\1" . ENTRY . "\\3"

  execute "b " . BUFFER
endfunc

func AdjustColWidth( COL )
  let BUFFER = bufname("")
  execute "b " . s:DBFILE

  let COLLIST = []
  let COLWIDTH = CreateColList( COLLIST, a:COL )

  let TLINE = 1
  while TLINE <= line("$")
    while Strlen(COLLIST[TLINE-1]) < COLWIDTH
      let COLLIST[TLINE-1] = COLLIST[TLINE-1] . " "
    endwhile

    call InsertTableCell(COLLIST[TLINE-1], TLINE, a:COL-1)

    let TLINE = TLINE + 1
  endwhile

  execute "b " . BUFFER
endfunc

func Strlen( STRING )
  return strlen(substitute(a:STRING, ".", "x", "g"))
endfunc

func DelTrailingSpaces( STRING )
  return substitute(a:STRING, '^\(\S*\)\s*$', '\1', '' )
endfunc
