"
"Vimorator, 2010-2011, Sergey Rezvanov, rezwyi(at)gmail.com
"
if v:version < 700
  finish
endif

"loaded
if exists("g:vimorator_loaded") && g:vimorator_loaded
	finish
endif
let g:vimorator_loaded = 1

"tns_admin
if !exists("g:vimorator_tns_admin") || g:vimorator_tns_admin == ""
  let g:vimorator_tns_admin = $TNS_ADMIN
endif

"global hotkey
if !exists("g:vimorator_hotkey") || g:vimorator_hotkey == ""
	let g:vimorator_hotkey = "<F2>"
endif
exe "nnoremap <unique>" g:vimorator_hotkey ":call Vimorator()<cr>"
exe "cnoremap <unique>" g:vimorator_hotkey "<Esc>"

function Vimorator()
  call s:init()
  call s:open()
endfunc

function s:init()
  set nolazyredraw

  let s:cursor_bg = synIDattr(hlID("Cursor"), "bg")
	let s:cursor_fg = synIDattr(hlID("Cursor"), "fg")
  let s:original_cmdh = &cmdheight

	hi Cursor guibg=NONE guifg=NONE

	cnoremap k k<cr>:call <SID>open()<cr>
	cnoremap j j<cr>:call <SID>open()<cr>
  cmap <up> k
  cmap <down> j

  call s:parse_tnsnames()
  call s:set_cmdh(s:sn_list_length + 1)
  exe "redraw"

endfunc

function s:kill()
	cunmap k
	cunmap j
  cunmap <up>
  cunmap <down>

  exe "hi Cursor guibg=" . 
      \s:cursor_bg . " guifg=".((s:cursor_fg == "") ? "NONE" : s:cursor_fg)

  call s:set_cmdh(s:original_cmdh) 

endfunc

function s:open()
	if !exists("s:current_sn_idx") || (s:current_sn_idx >= s:sn_list_length) || (s:current_sn_idx < 0)
		let s:current_sn_idx = 0
	endif

	if s:sn_list_length < 1
		call s:kill()
    echoh WarningMsg | echo "Vimorator [warning]: No service name parsed..." | echoh None
		return
	endif

  for l:i in range(s:sn_list_length)
    if l:i != s:current_sn_idx
      echo "  <" . (l:i + 1) . "> " . s:sn_list[l:i]
    else
      echoh DiffText | echo "> <" . (l:i + 1) . "> " . s:sn_list[l:i] | echoh None
    endif
  endfor

  let l:pkey = input("Choose service name:", " ")

  "key press handling
	if l:pkey =~ "j$"
		if s:current_sn_idx == s:sn_list_length - 1
			let s:current_sn_idx = 0
		else
			let s:current_sn_idx += 1
		endif

	elseif l:pkey =~ "k$"
		if s:current_sn_idx == 0
			let s:current_sn_idx = s:sn_list_length - 1
		else
			let s:current_sn_idx -= 1
		endif

  "enter
  elseif l:pkey != "" && l:pkey =~ "^ $"
    if !exists("g:vimorator_user") || g:vimorator_user == ""
      let g:vimorator_user = input("Username: ", "")
    endif

    if !exists("g:vimorator_pass") || g:vimorator_pass == ""
      let g:vimorator_pass = inputsecret("Password: ", "")
    endif

    exe "!sqlplus " . g:vimorator_user . "/" .  g:vimorator_pass . 
        \"@" . s:sn_list[s:current_sn_idx] . " @%"

    call s:kill()
    return

  "others (e.g. Esc)
  else
    call s:kill()
    return

	endif

  call s:set_cmdh(s:sn_list_length + 1)

endfunc

function s:parse_tnsnames()
  let l:regexp = '\(^\|)\) *[a-z0-9_-]\{1,\}'

  let s:sn_list = []
  let s:sn_list_length = 0

  for l:one_line in readfile(g:vimorator_tns_admin . "tnsnames.ora")
    while l:one_line =~ l:regexp
      let l:one_match = matchstr(l:one_line, l:regexp)
      let l:one_line = substitute(l:one_line, l:regexp, "", "")

      let s:sn_list_length += 1 
      call add(s:sn_list, substitute(tolower(l:one_match), "[ =)]", "", "g"))

    endwhile

  endfor

endfunc

function s:set_cmdh(height)
	if a:height > &lines - winnr('$') * (&winminheight + 1) - 1
		call s:kill()
    echoh WarningMsg | echo "Vimorator [fatal]: Too big service name list..." | echoh None
	else
		exe "set cmdheight=" . a:height
	endif
endfunc
