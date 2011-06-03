" Vim filetype plugin file
" Language:	S-record
" Maintainer:	ZhaoRuFei <slimzhao@21cn.com>
" Last Change:	2004 May 31
" License:	This file is placed in the public domain.

" Only do this when not done yet for this buffer
if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

let b:undo_ftplugin = "setl et< fo< com< commentstring<"

" Decode the S0 record
function! <SID>Interprete()
  let b:s0_line = getline('.')
  let b:data_count= ("0x". strpart(b:s0_line, 2, 2))+0
  "Contain 1 byte checksum, 1 byte DataCount(Contain checksum)
  let b:line_len = strlen(b:s0_line)/2
  if b:data_count != (b:line_len-1) && b:data_count != (b:line_len-2)
    echo "The data count of this record(line ". line('.'). ") is wrong!"
    return
  elseif b:data_count == b:line_len-1
    echo "This record looks have no checksum, press <C-K> to generate checksum for it"
    return
  else
    "Check checksum
    let b:right_cs = CheckSum(0)
    if b:right_cs != strpart(b:s0_line, strlen(b:s0_line)-2)
      echo "The Check sum is wrong!, should be: 0x". b:right_cs. ",Press <C-K> to correct it"
    endif
  endif
  "Currently only parse S0 record
  if getline('.') !~ '^S0'
    return
  endif

  let b:mname = strpart( b:s0_line, 8)
  let b:mname = strpart( b:mname, 0, strlen(b:mname)-2)
  let b:mname = substitute(b:mname,'[0-9a-fA-F]\{2}','\=(nr2char("0x".submatch(0)))', 'g')
  echo "Info: " . b:mname
  "RecType, DataCount, Address, Mname, ver, rev

endfunction


"a worker function, invoked by others
func! Nr2Hex(nr)
  let n = a:nr
  let r = ""
  while n
    let r = '0123456789ABCDEF'[n % 16] . r
    let n = n / 16
  endwhile
  return r
endfunc
"Re-calculate the check sum, Assume there's no checksum at current line
"arg1 == 0, return CheckSum, don't modify, arg1 == 1, modify, and return;
"arg1 =~ [^01], nothing
function! CheckSum(arg1)
  if a:arg1 != 0 && a:arg1 != 1
    return
  endif

  let b:curr_line = strpart(getline('.'), 2)
  let b:curr_line = strpart(b:curr_line, 0)
  "Number of bytes to Sum
  let n = strlen(b:curr_line) / 2
  let b:count = ('0x'. strpart(b:curr_line, 0, 2)) + 0
  "CheckSum
  let sum = 0
  let j = 0
  let b:n_trim_sum = n
  "Current line hasn't checksum at present
  if b:count+1 == strlen(b:curr_line)/2
    let b:n_trim_sum = b:n_trim_sum-1
  elseif b:count != strlen(b:curr_line)/2
    return "BAD_DATACOUNT"
  endif

  while j < b:n_trim_sum
    let sum = "0x". strpart(b:curr_line, j*2, 2) + sum
    let j = j+1
  endwhile
  "echo '_' . curr_line . '_'
  "echo 255-sum%256
  let sum_chars = Nr2Hex(255-sum%256)
  if strlen(sum_chars) == 1
    let sum_chars = "0". sum_chars
  elseif strlen(sum_chars) == 0
    "echo sum. ":" . b:curr_line. ":". n
    let sum_chars = "00"
  endif
  "There's no checksum at present, 2: CheckSum(1byte) + DataCount(1byte)
  if a:arg1 == 1
    if n == b:count
      s/$/\= sum_chars/
    else
      s/..$/\=sum_chars/
    endif
  endif
  return sum_chars
endfunction

"add *.srec/*.s3 to the possible fileext
au! CursorHold *.s19,*.s28,*.37,*.srec,*.s3 call <SID>Interprete()
nnoremap <C-K> :silent call CheckSum(1)<CR>
