" Replacement for Vim's default buffers menu to add status display
" Now adds a + before modified buffers, * before the current buffer.
"
" Note that Vim doesn't seem to have an autocmd event for a buffer
" becoming modified, so the menu won't show this change until something
" else causes it to refresh.
"
" Original code stolen from Vim 7.0-164
" Changes by Thomas Matysik <thomas@matysik.co.nz>
"
" This is version 1.0 of this script (2007-09-02)

if exists("did_install_paradons_buffers_menu")
  " Only install it once
  finish
endif
let did_install_paradons_buffers_menu = 1

if !has("gui_running")
  " Only load if we're in GUI mode
  finish
endif

if !exists("no_buffers_menu")

" Buffer list menu -- Setup functions & actions

" wait with building the menu until after loading 'session' files. Makes
" startup faster.
let s:bmenu_wait = 1

if !exists("bmenu_priority")
  let bmenu_priority = 60
endif

func! s:BMAdd()
  if s:bmenu_wait == 0
    " when adding too many buffers, redraw in short format
    if s:bmenu_count == &menuitems && s:bmenu_short == 0
      call s:BMShow()
    else
      call <SID>BMFilename(expand("<afile>"), expand("<abuf>"))
      let s:bmenu_count = s:bmenu_count + 1
    endif
  endif
endfunc

func! s:BMRemove()
  if s:bmenu_wait == 0
    let name = expand("<afile>")
    if isdirectory(name)
      return
    endif
    let munge = <SID>BMMunge(name, expand("<abuf>"))

    if s:bmenu_short == 0
      exe 'silent! aun &Buffers.' . munge
    else
      exe 'silent! aun &Buffers.' . <SID>BMHash2(munge) . munge
    endif
    let s:bmenu_count = s:bmenu_count - 1
  endif
endfunc

" Create the buffer menu (delete an existing one first).
func! s:BMShow(...)
  let s:bmenu_wait = 1
  let s:bmenu_short = 1
  let s:bmenu_count = 0
  "
  " get new priority, if exists
  if a:0 == 1
    let g:bmenu_priority = a:1
  endif

  " remove old menu, if exists; keep one entry to avoid a torn off menu to
  " disappear.
  silent! unmenu &Buffers
  exe 'noremenu ' . g:bmenu_priority . ".1 &Buffers.Dummy l"
  silent! unmenu! &Buffers

  " create new menu; set 'cpo' to include the <CR>
  let cpo_save = &cpo
  set cpo&vim
  exe 'an <silent> ' . g:bmenu_priority . ".2 &Buffers.&Refresh\\ menu :call <SID>BMShow()<CR>"
  exe 'an ' . g:bmenu_priority . ".4 &Buffers.&Delete :confirm bd<CR>"
  exe 'an ' . g:bmenu_priority . ".6 &Buffers.&Alternate :confirm b #<CR>"
  exe 'an ' . g:bmenu_priority . ".7 &Buffers.&Next :confirm bnext<CR>"
  exe 'an ' . g:bmenu_priority . ".8 &Buffers.&Previous :confirm bprev<CR>"
  exe 'an ' . g:bmenu_priority . ".9 &Buffers.-SEP- :"
  let &cpo = cpo_save
  unmenu &Buffers.Dummy

  " figure out how many buffers there are
  let buf = 1
  while buf <= bufnr('$')
    if bufexists(buf) && !isdirectory(bufname(buf)) && buflisted(buf)
					    \ && !getbufvar(buf, "&bufsecret")
      let s:bmenu_count = s:bmenu_count + 1
    endif
    let buf = buf + 1
  endwhile
  if s:bmenu_count <= &menuitems
    let s:bmenu_short = 0
  endif

  " iterate through buffer list, adding each buffer to the menu:
  let buf = 1
  while buf <= bufnr('$')
    if bufexists(buf) && !isdirectory(bufname(buf)) && buflisted(buf)
					    \ && !getbufvar(buf, "&bufsecret")
      call <SID>BMFilename(bufname(buf), buf)
    endif
    let buf = buf + 1
  endwhile
  let s:bmenu_wait = 0
  aug buffer_list
  au!
  au BufCreate,BufFilePost * call <SID>BMAdd()
  au BufDelete,BufFilePre * call <SID>BMRemove()
  aug END
endfunc

func! s:BMHash(name)
  " Make name all upper case, so that chars are between 32 and 96
  let nm = substitute(a:name, ".*", '\U\0', "")
  if has("ebcdic")
    " HACK: Replace all non alphabetics with 'Z'
    "       Just to make it work for now.
    let nm = substitute(nm, "[^A-Z]", 'Z', "g")
    let sp = char2nr('A') - 1
  else
    let sp = char2nr(' ')
  endif
  " convert first six chars into a number for sorting:
  return (char2nr(nm[0]) - sp) * 0x800000 + (char2nr(nm[1]) - sp) * 0x20000 + (char2nr(nm[2]) - sp) * 0x1000 + (char2nr(nm[3]) - sp) * 0x80 + (char2nr(nm[4]) - sp) * 0x20 + (char2nr(nm[5]) - sp)
endfunc

func! s:BMHash2(name)
  let nm = substitute(a:name, ".", '\L\0', "")
  " Not exactly right for EBCDIC...
  if nm[0] < 'a' || nm[0] > 'z'
    return '&others.'
  elseif nm[0] <= 'd'
    return '&abcd.'
  elseif nm[0] <= 'h'
    return '&efgh.'
  elseif nm[0] <= 'l'
    return '&ijkl.'
  elseif nm[0] <= 'p'
    return '&mnop.'
  elseif nm[0] <= 't'
    return '&qrst.'
  else
    return '&u-z.'
  endif
endfunc

" insert a buffer name into the buffer menu:
func! s:BMFilename(name, num)
  if isdirectory(a:name)
    return
  endif
  if getbufvar(a:num, "&modified") == 1
    let mymod = "+"
  else
    let mymod = '\ \ '
  endif
  if bufnr("%") == a:num
    let mycur = "*"
  else
    let mycur = '\ \ '
  endif
  let munge = <SID>BMMunge(a:name, a:num)
  let hash = <SID>BMHash(munge)
  let statusmunge = mycur . mymod . munge
  if s:bmenu_short == 0
    let name = 'an ' . g:bmenu_priority . '.' . hash . ' &Buffers.' . statusmunge
  else
    let name = 'an ' . g:bmenu_priority . '.' . hash . '.' . hash . ' &Buffers.' . <SID>BMHash2(munge) . statusmunge
  endif
  " set 'cpo' to include the <CR>
  let cpo_save = &cpo
  set cpo&vim
  exe name . ' :confirm b' . a:num . '<CR>'
  let &cpo = cpo_save
endfunc

" Truncate a long path to fit it in a menu item.
if !exists("g:bmenu_max_pathlen")
  let g:bmenu_max_pathlen = 35
endif
func! s:BMTruncName(fname)
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

func! s:BMMunge(fname, bnum)
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
  if a:bnum >= 0
    let name2 = name2 . ' (' . a:bnum . ')'
  endif
  let name = name2 . "\t" . <SID>BMTruncName(fnamemodify(name,':h'))
  let name = escape(name, "\\. \t|")
  let name = substitute(name, "&", "&&", "g")
  let name = substitute(name, "\n", "^@", "g")
  return name
endfunc

" When just starting Vim, load the buffer menu later
if has("vim_starting")
  augroup LoadBufferMenu
    au! VimEnter * if !exists("no_buffers_menu") | call <SID>BMShow() | endif
    au  VimEnter * au! LoadBufferMenu
  augroup END
else
  call <SID>BMShow()
endif

autocmd VimEnter,BufWinEnter,BufEnter,BufNew,WinEnter * call <SID>BMShow()

endif " !exists("no_buffers_menu")
" vim: set sw=2 :
