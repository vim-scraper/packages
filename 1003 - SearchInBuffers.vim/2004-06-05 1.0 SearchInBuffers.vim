"                          :SIB - Search in buffers
"                          ------------------------
"                        (C) 2004 Francesco Bradascio
"
"                       http://fbradasc.altervista.org
"                          mailto:fbradasc@yahoo.it
"
" DESCRIPTION
"
"     This plugin allow you to search the current search pattern in all the
"     buffers currently opened into VIM.
"
" INSTALLATION
"
"     Copy this file into the VIM plugin directory.
"
" USAGE
"
"     Just do a search in whatsoever way into the current buffer and then
"     run :SIB to process the same search in all the other open buffers.
"
"     Use the quickfix commands (:cn, :cp, ...) to navigate through the
"     patterns found, :cclose to close the found's list, that's all.
"     Do ':help quickfix.txt' for more details.
"
" COPYING POLICY
"
"     This library is free software; you can redistribute it and/or
"     modify it under the terms of the GNU Lesser General Public
"     License as published by the Free Software Foundation; either
"     version 2.1 of the License, or (at your option) any later version.
" 
"     This library is distributed in the hope that it will be useful,
"     but WITHOUT ANY WARRANTY; without even the implied warranty of
"     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
"     Lesser General Public License for more details.
" 
"     You should have received a copy of the GNU Lesser General Public
"     License along with this library; if not, write to the Free Software
"     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
"
function! s:RunSearchInBuffers()

  let current = bufnr("%")
  let files   = bufnr("$")
  let i       = 1
  let n       = 1
  let found   = 0
  let tmpfile = tempname()

  cclose

  while i <= files      " loop over all files in buffer list
    if bufexists(i) && buflisted(i)
      silent exe "buffer" i
      "
      " start at the last char in the file and wrap for the
      " first search to find match at start of file
      "
      normal G$
      let flags = "w"
      silent let lineno = search(@/, flags)
      while lineno > 0
        "
        " appending to the tmp file the result of the search
        "
        exe "redir! >> " . tmpfile
        silent echo expand('%') . ":" . lineno . ":" . getline(lineno)
        redir END
        "
        " proceed with the search
        "
        let flags = "W"
        silent let lineno = search(@/, flags)
        "
        " notify that something was found
        "
        let found = 1
      endwhile
      let n = n + 1
    endif
    let i = i + 1
  endwhile

  if found != 0     " if something was found show the list of items found
    let old_efm = &efm
    set efm=%f:%\\s%#%l:%m
    execute "silent! cfile " . tmpfile
    let &efm = old_efm
    botright copen
    "
    " jump to the first item found
    "
    cc
    "
    " remove the tmp file
    "
    call delete(tmpfile)
  else              " nothing was found, restore the current buffer
    silent exe "buffer" current
  endif

endfunction
command! -nargs=0 SIB call s:RunSearchInBuffers()
