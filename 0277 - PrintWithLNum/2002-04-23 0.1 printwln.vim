" Vim utility file
" Language:     vim
" Maintainer:   Colin Keith <vim@ckeith.clara.net>
" Last Change:  2002 Apr 23
" Location:     http://vim.sf.net/

" Written in response to the vim mailing list mail:
"
" From: He WanQing-a2536c <a2536c@motorola.com>
" Date: Tue, 23 Apr 2002 20:53:20 +0800
"
" How can I print the code from vim (Win32 version)
" with the line number also printed?
"

if !exists(':Printwln')
  command -n=? -complete=dir Printwln :call s:PrintWithLNum('<a>')
endif


function! s:PrintWithLNum(...)
   " Basically sprintf'ing...
   let i=strlen(line('$'))+1
   let fmt = ''
   while(i>0)
     let fmt = fmt . ' '
     let i = i - 1
   endwhile
   :%s/^/\= s:GetLNum(fmt, line('.')) /

   " Now we have line numbers, do we actually want to print it?
   if exists('a:1')  && a:1
     :hardcopy
     :undo
   endif
:endfunction

" Couldn't get this to work inline :(
function! s:GetLNum(fmt, linenum)
  return substitute(a:fmt, ' \{'.strlen(a:linenum).'\}$', a:linenum, '').' '
endfunction

