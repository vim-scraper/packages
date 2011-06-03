" Copy right : T.N.Satish. Send all your comments to the following address.
" tnsatish < a t > g m a i l < d o t > c o m
" This can be redistributed under BSD license.
au TabEnter * call ShowTab()

function! ShowTab()
  silent! aunmenu T&ab
  let in = 1
  for i in range(tabpagenr('$'))
    let xyz = in + 10
    let name = 'an 120.'. xyz . ' T&ab.&' . in . '\.\ ' . Krishna(TabLabel(in)) . ' :tabnext ' . in . '<CR>' 
    let in = in + 1
    exe name
  endfor
endfunction

function! TabLabel(n)
  let buflist = tabpagebuflist(a:n)
  let winnr = tabpagewinnr(a:n)
  return bufname(buflist[winnr - 1])
endfunction

func! BKrishna(fname)
  let name = a:fname
  if g:bmenu_max_pathlen < 5
    let name = ""
  else
    let len = strlen(name)
    if len > g:bmenu_max_pathlen
      let amountl = (g:bmenu_max_pathlen / 2) - 2
      let amountr = g:bmenu_max_pathlen - amountl - 3
      let pattern = '^\(.\{,' . amountl . '}\).\{-}\(.\{,' . amountr . '}\)$'
      let left = substitute(name, pattern, '\1', '')
      let right = substitute(name, pattern, '\2', '')
      if strlen(left) + strlen(right) < len
        let name = left . '...' . right
      endif
    endif
  endif
  return name
endfunc

func! Krishna(fname)
  let name = a:fname
  if name == ''
    if !exists("g:menutrans_no_file")
      let g:menutrans_no_file = "[No file]"
    endif
    let name = g:menutrans_no_file
  else
    let name = fnamemodify(name, ':p:~')
  endif
  " detach file name and separate it out:
  let name2 = fnamemodify(name, ':t')
  let name1 = BKrishna(fnamemodify(name,':h'))
  let name = name2 . "\t" . name1
  let name = escape(name, "\\. \t|")
  let name = substitute(name, "&", "&&", "g")
  let name = substitute(name, "\n", "^@", "g")
  return name
endfunc

" Hare Krishna Hare Krishna Krishna Krishna Hare Hare
" Hare Rama Hare Rama Rama Rama Hare Hare
