" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/editsrec.vim	[[[1
188
" Vim Global Plugin for Editing .srec Files
" Maintainer: Alexander Fleck
"            <alexander.fleck@gmx.net>
" License:    This File is placed in the Public Domain.
" Revision | Date [DD.MM.YY] | Changes
" 00.01.00 |       05.07.09  | 01. Revision
" 00.02.00 |       29.03.10  | Fun added, MakeSrecS5()

if exists("loaded_editsrec")
  finish
endif
let loaded_editsrec = 1

let s:save_cpo = &cpo
set cpo&vim

" BC = ByteCount
if !hasmapto('<Plug>EditSrecLineBC')
  map <unique> <Leader>lb <Plug>EditSrecLineBC
endif
noremap <unique> <script> <Plug>EditSrecLineBC <SID>AutoLineBC
noremap <SID>AutoLineBC <Esc>:call <SID>AutoLineBC()<CR>
if !hasmapto('<Plug>EditSrecPartBC')
  map <unique> <Leader>pb <Plug>EditSrecPartBC
endif
noremap <unique> <script> <Plug>EditSrecPartBC <SID>AutoPartBC
noremap <SID>AutoPartBC <Esc>:call <SID>AutoPartBC()<CR>

" AD = ADdress
if !hasmapto('<Plug>EditSrecLineAD')
  map <unique> <Leader>la <Plug>EditSrecLineAD
endif
noremap <unique> <script> <Plug>EditSrecLineAD <SID>AutoLineAD
noremap <SID>AutoLineAD <Esc>:call <SID>AutoLineAD()<CR>
if !hasmapto('<Plug>EditSrecPartAD')
  map <unique> <Leader>pa <Plug>EditSrecPartAD
endif
noremap <unique> <script> <Plug>EditSrecPartAD <SID>AutoPartAD
noremap <SID>AutoPartAD <Esc>:call <SID>AutoPartAD()<CR>

" DA = DAta
if !hasmapto('<Plug>EditSrecLineDA')
  map <unique> <Leader>ld <Plug>EditSrecLineDA
endif
noremap <unique> <script> <Plug>EditSrecLineDA <SID>AutoLineDA
noremap <SID>AutoLineDA <Esc>:call <SID>AutoLineDA()<CR>
if !hasmapto('<Plug>EditSrecPartDA')
  map <unique> <Leader>pd <Plug>EditSrecPartDA
endif
noremap <unique> <script> <Plug>EditSrecPartDA <SID>AutoPartDA
noremap <SID>AutoPartDA <Esc>:call <SID>AutoPartDA()<CR>

" CS = CheckSum
if !hasmapto('<Plug>EditSrecLineCS')
  map <unique> <Leader>lc <Plug>EditSrecLineCS
endif
noremap <unique> <script> <Plug>EditSrecLineCS <SID>AutoLineCS
noremap <SID>AutoLineCS <Esc>:call <SID>AutoLineCS()<CR>
if !hasmapto('<Plug>EditSrecPartCS')
  map <unique> <Leader>pc <Plug>EditSrecPartCS
endif
noremap <unique> <script> <Plug>EditSrecPartCS <SID>AutoPartCS
noremap <SID>AutoPartCS <Esc>:call <SID>AutoPartCS()<CR>

" M5 = MakeS5
if !hasmapto('<Plug>MakeSrecLineS5')
  map <unique> <Leader>m5 <Plug>MakeSrecLineS5
endif
noremap <unique> <script> <Plug>MakeSrecLineS5 <SID>MakeSrecS5
noremap <SID>MakeSrecS5 <Esc>:call <SID>MakeSrecS5()<CR>

" obsolete Mappings
"imap <F5>    <Esc>:call <SID>AutoLineBC()<CR>a
"imap <F6>    <Esc>:call <SID>AutoLineAD()<CR>a
"imap <F7>    <Esc>:call <SID>AutoLineDA()<CR>a
"imap <F8>    <Esc>:call <SID>AutoLineCS()<CR>a
"imap <C-F5>  <Esc>:call <SID>AutoPartBC()<CR>a
"imap <C-F6>  <Esc>:call <SID>AutoPartAD()<CR>a
"imap <C-F7>  <Esc>:call <SID>AutoPartDA()<CR>a
"imap <C-F8>  <Esc>:call <SID>AutoPartCS()<CR>a

" create Line from ByteCount
fun s:AutoLineBC()
  let s:ln = getline(".")
  
  let s:ln = s:ln . libsrec#CrBC(s:ln)
  let s:ln = s:ln . libsrec#CrAD(s:ln)
  let s:ln = s:ln . libsrec#CrDA(s:ln)
  let s:ln = s:ln . libsrec#CrCS(s:ln)
  
  call setline(".", s:ln)
  
  unlet s:ln
endfun

" create only ByteCount
fun s:AutoPartBC()
  let s:ln = getline(".")
  
  let s:ln = s:ln . libsrec#CrBC(s:ln)
  
  call setline(".", s:ln)
  
  unlet s:ln
endfun

" create Line from ADdress
fun s:AutoLineAD()
  let s:ln = getline(".")
  
  let s:ln = s:ln . libsrec#CrAD(s:ln)
  let s:ln = s:ln . libsrec#CrDA(s:ln)
  let s:ln = s:ln . libsrec#CrCS(s:ln)
  
  call setline(".", s:ln)
  
  unlet s:ln
endfun

" create only ADdress
fun s:AutoPartAD()
  let s:ln = getline(".")
  
  let s:ln = s:ln . libsrec#CrAD(s:ln)
  
  call setline(".", s:ln)
  
  unlet s:ln
endfun

" create Line from DAta
fun s:AutoLineDA()
  let s:ln = getline(".")
  
  let s:ln = s:ln . libsrec#CrDA(s:ln)
  let s:ln = s:ln . libsrec#CrCS(s:ln)
  
  call setline(".", s:ln)
  
  unlet s:ln
endfun

" create only DAta
fun s:AutoPartDA()
  let s:ln = getline(".")
  
  let s:ln = s:ln . libsrec#CrDA(s:ln)
  
  call setline(".", s:ln)
  
  unlet s:ln
endfun

" create Line from CheckSum
fun s:AutoLineCS()
  let s:ln = getline(".")
  
  let s:ln = s:ln . libsrec#CrCS(s:ln)
  
  call setline(".", s:ln)
  
  unlet s:ln
endfun

" create only CheckSum
fun s:AutoPartCS()
  let s:ln = getline(".")
  
  let s:ln = s:ln . libsrec#CrCS(s:ln)
  
  call setline(".", s:ln)
  
  unlet s:ln
endfun

" make S5 record
fun s:MakeSrecS5()
  let s:ln = getline(".")
  
  let s:ln = s:ln . libsrec#MkS5(s:ln)
  
  call setline(".", s:ln)
  
  unlet s:ln
endfun

let &cpo = s:save_cpo

plugin/editsrec_test.txt	[[[1
356
" Vim Test Cases for Editing .srec Files
" Maintainer: Alexander Fleck
"            <alexander.fleck@gmx.net>
" License:    This File is placed in the Public Domain.
" Revision | Date [DD.MM.YY] | Changes
" 00.01.00 |       05.07.09  | 01. Revision
" 00.01.10 |       29.03.10  | BugFix, in libsrec#CrCS()
"          |                 | leading Zeros in Result
" 00.02.00 |       29.03.10  | Fun added

" This File contains Test Cases for the Plugin 'editsrec.vim'
"                               and its Library 'libsrec.vim'.

TestCases for ByteCount
Line -----------------------------
insert a new Line and type "S0"
type "<Esc><Leader>lb"
the following Line will be created
S00F0000FFFFFFFFFFFFFFFFFFFFFFFFFC
insert a new Line and type "S1"
type "<Esc><Leader>lb"
the following Line will be created
S10F0000FFFFFFFFFFFFFFFFFFFFFFFFFC
insert a new Line and type "S2"
type "<Esc><Leader>lb"
the following Line will be created
S20F000000FFFFFFFFFFFFFFFFFFFFFFFB
insert a new Line and type "S3"
type "<Esc><Leader>lb"
the following Line will be created
S30F00000000FFFFFFFFFFFFFFFFFFFFFA
insert a new Line and type "S4"
type "<Esc><Leader>lb"
Nothing will be created
insert a new Line and type "S5"
type "<Esc><Leader>lb"
the following Line will be created
S5030000FC
insert a new Line and type "S6"
type "<Esc><Leader>lb"
Nothing will be created
insert a new Line and type "S7"
type "<Esc><Leader>lb"
the following Line will be created
S70500000000FA
insert a new Line and type "S8"
type "<Esc><Leader>lb"
the following Line will be created
S804000000FB
insert a new Line and type "S9"
type "<Esc><Leader>lb"
the following Line will be created
S9030000FC

Part -----------------------------
insert a new Line and type "S0"
type "<Esc><Leader>pb"
the following Line will be created
S00F
insert a new Line and type "S1"
type "<Esc><Leader>pb"
the following Line will be created
S10F
insert a new Line and type "S2"
type "<Esc><Leader>pb"
the following Line will be created
S20F
insert a new Line and type "S3"
type "<Esc><Leader>pb"
the following Line will be created
S30F
insert a new Line and type "S4"
type "<Esc><Leader>pb"
Nothing will be created
insert a new Line and type "S5"
type "<Esc><Leader>pb"
the following Line will be created
S503
insert a new Line and type "S6"
type "<Esc><Leader>pb"
Nothing will be created
insert a new Line and type "S7"
type "<Esc><Leader>pb"
the following Line will be created
S705
insert a new Line and type "S8"
type "<Esc><Leader>pb"
the following Line will be created
S804
insert a new Line and type "S9"
type "<Esc><Leader>pb"
the following Line will be created
S903


TestCases for ADdress
Line -----------------------------
insert a new Line and type "S00F"
type "<Esc><Leader>la"
the following Line will be created
S00F0000FFFFFFFFFFFFFFFFFFFFFFFFFC
insert a new Line and type "S10F"
type "<Esc><Leader>la"
the following Line will be created
S10F0000FFFFFFFFFFFFFFFFFFFFFFFFFC
insert a new Line and type "S20F"
type "<Esc><Leader>la"
the following Line will be created
S20F000000FFFFFFFFFFFFFFFFFFFFFFFB
insert a new Line and type "S30F"
type "<Esc><Leader>la"
the following Line will be created
S30F00000000FFFFFFFFFFFFFFFFFFFFFA
insert a new Line and type "S40F"
type "<Esc><Leader>la"
Nothing will be created
insert a new Line and type "S503"
type "<Esc><Leader>la"
the following Line will be created
S5030000FC
insert a new Line and type "S60F"
type "<Esc><Leader>la"
Nothing will be created
insert a new Line and type "S705"
type "<Esc><Leader>la"
the following Line will be created
S70500000000FA
insert a new Line and type "S804"
type "<Esc><Leader>la"
the following Line will be created
S804000000FB
insert a new Line and type "S903"
type "<Esc><Leader>la"
the following Line will be created
S9030000FC

Part -----------------------------
insert a new Line and type "S00F"
type "<Esc><Leader>pa"
the following Line will be created
S00F0000
insert a new Line and type "S10F"
type "<Esc><Leader>pa"
the following Line will be created
S10F0000
insert a new Line and type "S20F"
type "<Esc><Leader>pa"
the following Line will be created
S20F000000
insert a new Line and type "S30F"
type "<Esc><Leader>pa"
the following Line will be created
S30F00000000
insert a new Line and type "S40F"
type "<Esc><Leader>pa"
Nothing will be created
insert a new Line and type "S503"
type "<Esc><Leader>pa"
the following Line will be created
S5030000
insert a new Line and type "S60F"
type "<Esc><Leader>pa"
Nothing will be created
insert a new Line and type "S705"
type "<Esc><Leader>pa"
the following Line will be created
S70500000000
insert a new Line and type "S804"
type "<Esc><Leader>pa"
the following Line will be created
S804000000
insert a new Line and type "S903"
type "<Esc><Leader>pa"
the following Line will be created
S9030000


TestCases for DAta
Line -----------------------------
insert a new Line and type "S00F0000"
type "<Esc><Leader>ld"
the following Line will be created
S00F0000FFFFFFFFFFFFFFFFFFFFFFFFFC
insert a new Line and type "S10F0000"
type "<Esc><Leader>ld"
the following Line will be created
S10F0000FFFFFFFFFFFFFFFFFFFFFFFFFC
insert a new Line and type "S20F000000"
type "<Esc><Leader>ld"
the following Line will be created
S20F000000FFFFFFFFFFFFFFFFFFFFFFFB
insert a new Line and type "S30F00000000"
type "<Esc><Leader>ld"
the following Line will be created
S30F00000000FFFFFFFFFFFFFFFFFFFFFA
insert a new Line and type "S40F0000"
type "<Esc><Leader>ld"
Nothing will be created
insert a new Line and type "S5030000"
type "<Esc><Leader>ld"
the following Line will be created
S5030000FC
insert a new Line and type "S60F0000"
type "<Esc><Leader>ld"
Nothing will be created
insert a new Line and type "S70500000000"
type "<Esc><Leader>ld"
the following Line will be created
S70500000000FA
insert a new Line and type "S804000000"
type "<Esc><Leader>ld"
the following Line will be created
S804000000FB
insert a new Line and type "S9030000"
type "<Esc><Leader>ld"
the following Line will be created
S9030000FC

Part -----------------------------
insert a new Line and type "S00F0000"
type "<Esc><Leader>pd"
the following Line will be created
S00F0000FFFFFFFFFFFFFFFFFFFFFFFF
insert a new Line and type "S10F0000"
type "<Esc><Leader>pd"
the following Line will be created
S00F0000FFFFFFFFFFFFFFFFFFFFFFFF
insert a new Line and type "S20F000000"
type "<Esc><Leader>pd"
the following Line will be created
S00F000000FFFFFFFFFFFFFFFFFFFFFF
insert a new Line and type "S30F00000000"
type "<Esc><Leader>pd"
the following Line will be created
S00F00000000FFFFFFFFFFFFFFFFFFFF
insert a new Line and type "S40F0000"
type "<Esc><Leader>pd"
Nothing will be created
insert a new Line and type "S5030000"
type "<Esc><Leader>pd"
the following Line will be created
S5030000
insert a new Line and type "S60F0000"
type "<Esc><Leader>pd"
Nothing will be created
insert a new Line and type "S70500000000"
type "<Esc><Leader>pd"
the following Line will be created
S70500000000
insert a new Line and type "S804000000"
type "<Esc><Leader>pd"
the following Line will be created
S804000000
insert a new Line and type "S9030000"
type "<Esc><Leader>pd"
the following Line will be created
S9030000


TestCases for CheckSum
Line -----------------------------
insert a new Line and type "S00F0000FFFFFFFFFFFFFFFFFFFFFFFF"
type "<Esc><Leader>lc"
the following Line will be created
S00F0000FFFFFFFFFFFFFFFFFFFFFFFFFC
insert a new Line and type "S10F0000FFFFFFFFFFFFFFFFFFFFFFFF"
type "<Esc><Leader>lc"
the following Line will be created
S10F0000FFFFFFFFFFFFFFFFFFFFFFFFFC
insert a new Line and type "S20F000000FFFFFFFFFFFFFFFFFFFFFF"
type "<Esc><Leader>lc"
the following Line will be created
S20F000000FFFFFFFFFFFFFFFFFFFFFFFB
insert a new Line and type "S30F00000000FFFFFFFFFFFFFFFFFFFF"
type "<Esc><Leader>lc"
the following Line will be created
S30F00000000FFFFFFFFFFFFFFFFFFFFFA
insert a new Line and type "S40F0000"
type "<Esc><Leader>lc"
Nothing will be created
insert a new Line and type "S5030000"
type "<Esc><Leader>lc"
the following Line will be created
S5030000FC
insert a new Line and type "S60F0000"
type "<Esc><Leader>lc"
Nothing will be created
insert a new Line and type
type "<Esc><Leader>lc"
the following Line will be created
S70500000000FA
insert a new Line and type "S804000000"
type "<Esc><Leader>lc"
the following Line will be created
S804000000FB
insert a new Line and type "S9030000"
type "<Esc><Leader>lc"
the following Line will be created
S9030000FC

Part -----------------------------
insert a new Line and type "S00F0000FFFFFFFFFFFFFFFFFFFFFFFF"
type "<Esc><Leader>pc"
the following Line will be created
S00F0000FFFFFFFFFFFFFFFFFFFFFFFFFC
insert a new Line and type "S10F0000FFFFFFFFFFFFFFFFFFFFFFFF"
type "<Esc><Leader>pc"
the following Line will be created
S10F0000FFFFFFFFFFFFFFFFFFFFFFFFFC
insert a new Line and type "S20F000000FFFFFFFFFFFFFFFFFFFFFF"
type "<Esc><Leader>pc"
the following Line will be created
S20F000000FFFFFFFFFFFFFFFFFFFFFFFB
insert a new Line and type "S30F00000000FFFFFFFFFFFFFFFFFFFF"
type "<Esc><Leader>pc"
the following Line will be created
S30F00000000FFFFFFFFFFFFFFFFFFFFFA
insert a new Line and type "S40F0000"
type "<Esc><Leader>pc"
Nothing will be created
insert a new Line and type "S5030000"
type "<Esc><Leader>pc"
the following Line will be created
S5030000FC
insert a new Line and type "S60F0000"
type "<Esc><Leader>pc"
Nothing will be created
insert a new Line and type "S70500000000"
type "<Esc><Leader>pc"
the following Line will be created
S70500000000FA
insert a new Line and type "S804000000"
type "<Esc><Leader>pc"
the following Line will be created
S804000000FB
insert a new Line and type "S9030000"
type "<Esc><Leader>pc"
the following Line will be created
S9030000FC

TestCase for leading Zeros -------
insert a new Line and type "S30F00000000000000000000000000EE"
type "<Esc><Leader>lc"
the following Line will be created
S30F00000000000000000000000000EE02

TestCase for Making S5 Record ----
insert
S30F00000000000000000000000000EE02
S30F00000000000000000000000000EE02
S30F00000000000000000000000000EE02
insert a new Line
type "<Esc><Leader>m5"
the following Line will be created
S5030003FC

autoload/libsrec.vim	[[[1
211
" Vim Library for Editing .srec Files
" Maintainer: Alexander Fleck
"            <alexander.fleck@gmx.net>
" License:    This File is placed in the Public Domain.
" Revision | Date [DD.MM.YY] | Changes
" 00.01.00 |       05.07.09  | 01. Revision
" 00.01.10 |       29.03.10  | BugFix, in libsrec#CrCS()
"          |                 | leading Zeros in Result
" 00.02.00 |       29.03.10  | Fun added, MkS5()

" create ByteCount
fun libsrec#CrBC(line)
  let s:byco = ""
  
  " Dictionary with Assumptions
  let s:dict = { 0: '0F',
               \ 1: '0F',
               \ 2: '0F',
               \ 3: '0F',
               \ 4: ''  ,
               \ 5: '03',
               \ 6: ''  ,
               \ 7: '05',
               \ 8: '04',
               \ 9: '03' }
  
  let s:byco = s:dict[a:line[1]]
  
  unlet s:dict
  "unlet s:byco
  return s:byco
endfun

" create ADdress
fun libsrec#CrAD(line)
  let s:adrs = ""
  " Number of Bytes for Address
  let s:adby = ""
  " Dictionary
  let s:dict = { 0: 2,
               \ 1: 2,
               \ 2: 3,
               \ 3: 4,
               \ 4: 0,
               \ 5: 2,
               \ 6: 0,
               \ 7: 4,
               \ 8: 3,
               \ 9: 2 }
  
  let s:adby = s:dict[a:line[1]]
  
  let s:n = 0
  while 1
    let s:n = s:n + 1
    if s:n > s:adby
      break
    endif
    let s:adrs = s:adrs . "00"
    continue
  endwhile
  unlet s:n
  
  unlet s:dict
  unlet s:adby
  "unlet s:adrs
  return s:adrs
endfun

" create DAta
fun libsrec#CrDA(line)
  let s:data = ""
  " Number of Bytes for Data
  let s:daby = ""
  
  " get ByteCount from Line
  let s:bchx = "0x" . a:line[2] . a:line[3]
  " Dictionary
  let s:dict = { 0: s:bchx - 3,
               \ 1: s:bchx - 3,
               \ 2: s:bchx - 4,
               \ 3: s:bchx - 5,
               \ 4: 0         ,
               \ 5: 0         ,
               \ 6: 0         ,
               \ 7: 0         ,
               \ 8: 0         ,
               \ 9: 0          }
  
  let s:daby = s:dict[a:line[1]]
  
  let s:n = 0
  while 1
    let s:n = s:n + 1
    if s:n > s:daby
      break
    endif
    let s:data = s:data . "FF"
    continue
  endwhile
  unlet s:n
  
  unlet s:dict
  unlet s:bchx
  unlet s:daby
  "unlet s:data
  return s:data
endfun

" create CheckSum
fun libsrec#CrCS(line)
  " CheckSum as a String
  let s:chsu = ""
  
  " get ByteCount from Line
  let s:bchx = "0x" . a:line[2] . a:line[3]
  
  if ((a:line[1] == "4") || (a:line[1] == "6"))
    " there' s no S4 or S6
  else
    " CheckSum as a Value
    let s:csby = 0
    
    " add the Byte Values
    let s:n = 0
    while 1
      let s:n = s:n + 1
      if s:n > s:bchx
        break
      endif
      let s:csby = s:csby + ("0x" . a:line[2*s:n] . a:line[2*s:n+1])
      continue
    endwhile
    unlet s:n
    
    " CheckSum is:
    let s:csby = 255 - (s:csby % 256)
    
    " convert to Hex Value,
    "            Hex String without "0x"
    let s:hxva = "0123456789ABCDEF"
    while s:csby
      let s:chsu = s:hxva[s:csby % 16] . s:chsu
      let s:csby = s:csby        / 16
    endwhile
    
    " add missing Zeros
    while strlen(s:chsu) < 2
      let s:chsu = "0" . s:chsu
    endwhile
    " Exception Handling
    if strlen(s:chsu) > 2
      let s:chsu = "CS"
    endif
    
    unlet s:hxva
    unlet s:csby
  endif
  
  unlet s:bchx
  "unlet s:chsu
  return s:chsu
endfun

" make S5 record
fun libsrec#MkS5(line)
  let s:srec = ""
  let s:line = ""
  let s:srco = 0
  let s:srad = ""
  
  let s:srec = s:srec . "S5" . "03"
  
  let s:n = 1
  while (line2byte(s:n) != line2byte("."))
    let s:line = getline(s:n)
    if ((s:line[0] == "S") && ((s:line[1] == "1") || (s:line[1] == "2") || (s:line[1] == "3")))
      let s:srco = s:srco + 1
    endif
    let s:n = s:n + 1
  endwhile
  unlet s:n
  
  " convert to Hex Value,
  "            Hex String without "0x"
  let s:hxva = "0123456789ABCDEF"
  while s:srco
    let s:srad = s:hxva[s:srco % 16] . s:srad
    let s:srco = s:srco        / 16
  endwhile
  
  " add missing Zeros
  while strlen(s:srad) < 4
    let s:srad = "0" . s:srad
  endwhile
  " Exception Handling
  if strlen(s:srad) > 4
    let s:srad = "AAAA"
  endif
  
  unlet s:hxva
  
  let s:srec = s:srec . s:srad . libsrec#CrCS(s:srec)
  
  unlet s:srad
  unlet s:srco
  unlet s:line
  "unlet s:srec
  return s:srec
endfun

doc/editsrec.txt	[[[1
41
" Vim Global Plugin for Editing .srec Files, doc-File
" Maintainer: Alexander Fleck
"            <alexander.fleck@gmx.net>
" License:    This File is placed in the Public Domain.
" Revision | Date [DD.MM.YY] | Changes
" 00.01.00 |       05.07.09  | 01. Revision
" 00.02.00 |       29.03.10  | Description added

*editsrec.txt* Global Plugin for Editing .srec Files

This Plugin consists of the following Files:
/plugin/editsrec.vim
/plugin/editsrec_test.txt
/autoload/libsrec.vim
and
/doc/editsrec.txt
, this File.

The Purpose of this Plugin is to edit .srec-Files.
Currently it only allows Editing Line by Line, 
                but Handling of Files will be added later.

Commands | Description                       |
---------|-----------------------------------|
lb       | build a line from ByteCount       |
pb       | build only the ByteCount          |
---------|-----------------------------------|
la       | build a line from ADdress         |
pa       | build only the ADdress            |
---------|-----------------------------------|
ld       | build a line from DAta            |
pd       | build only the DAta               |
---------|-----------------------------------|
lc       | build a line from CheckSum        |
pc       | build only the CheckSum           |
---------|-----------------------------------|
m5       | make an S5 record on a blank line |
         |                                   |

see *editsrec_test.txt* for examples

