" An IRC client plugin for Vim
" Maintainer: Madoka Machitani <madokam@zag.att.ne.jp>
" Created: Tue, 24 February 2004
" Last Change: Tue, 09 Mar 2004 21:37:02 +0900 (JST)
" License: Distributed under the same terms as Vim itself
"
" Credits:
"   ircII		the basics of IRC
"   KoRoN		creator of a BBS viewer for Vim, Chalice (very popular
"			among Japanese vim community)
"   Ilya Sher		an idea for mini-buffers for cmdline editing
"
" Features:
"   * real-time message receiving with user interaction (many of the normal
"     mode commands available)
"   * multiple servers/channels connectivity
"
"   A Drawback:
"     VimIRC achieves real-time message reception by implementing its own main
"     loop.  Therefore, while you are out of it, VimIRC has no way to get new
"     messages.  So my recommendation is, use a shortcut which creates
"     a VimIRC-dedicated instance of Vim, where you don't do normal editing
"     sessions.
"
" Requirements:
"   * Vim 6.2 or later with perl interface enabled
"   * Perl (preferably 5.8 or later if you want multibyte feature, which is
"     not implemented yet)
"
" Options:
"   let g:plugin_vimirc_nick		nickname
"   let g:plugin_vimirc_user		username
"   let g:plugin_vimirc_realname	full name
"   let g:plugin_vimirc_umode		user mode set upon logon (not
"					working?)
"   let g:plugin_vimirc_server		your favorite IRC server. in a format:
"					  irc.foobar.com:6667
"					(default is irc.freenode.net)
"   let g:plugin_vimirc_partmsg		message sent with QUIT/PART
"
"   Runtime Options:
"     You can pass the following options to the command :VimIRC, overriding
"     the vim variables above
"
"	-n nickname
"	-u username
"	-s server:port
"
"	and
"
"	--realname="full name"
"
" Startup:
"   Type
"     :VimIRC<CR>
"
"   You will be prompted for several user information if you have not set
"   variables listed above.
"
" Usage:
"   Normal mode:  This is a pseudo normal mode.  Just try several normal-mode
"		  commands as usual.  Some are available, and some are not.
"
"		  Type "i" or "I" to enter the command-line mode (mnemonic:
"		  enter "I"RC-command mode.  Or else, "i"nsert command).
"
"		  Type <Ctrl-C> to get out of control and freely move
"		  around/do ex commands.
"
"		  Hit <Space> to re-enter the normal (online) mode.
"
"		  Special cases:
"
"		  * In a channels list window (which opens up with /list
"		    command), you can type "o" to sort the list, "O" to
"		    reverse it (I took these mappings from Mutt the e-mail
"		    client).
"		    Hitting <CR> will prompt you whether to join the channel
"		    where the cursor is.
"
"   Command mode: This is just a normal buffer opened at the bottom of the
"		  screen.  Enter IRC commands here.  Hitting <CR>, both in
"		  insert and normal mode, will send the cursor line instantly
"		  either as a command or a message.
"
"		  Every IRC command starts with "/".  E.g.: /join #vim,#c
"
"		  Line without a leading slash will be sent as a message,
"		  normaly to the current channel.
"
"		  Type /help<CR> to see the list of available commands.  It's
"		  far from complete, though.
"
" Quit:
"   Type
"     /quit<CR>
"   in the IRC command line to disconnect with the current server.
"
"   Or to totally exit from the script, type
"     :VimIRCQuit<CR>
"   in the VIM command line.
"
" TODOs:
"   * multibyte support
"   * authentication:		just add one line to send PASS
"   * ctcp (partially done)
"   * flood protection
"   * netsplit detection
"   * logging
"   * scripting (?)
"   * help
"   * etc. etc.
"
"   Done:
"   - command-line history
"   - separate listing of channels
"     with sorting facilities
"

if exists('g:loaded_vimirc') || &compatible
  finish
endif

let s:save_cpoptions = &cpoptions
set cpoptions&

let s:version = '0.5.1'
let s:client = 'VimIRC '.s:version
" Set this to zero when releasing, which I'll occasionally forget, for sure
let s:debug = 0

if !s:debug
  let g:loaded_vimirc = 1
endif

"
" Start/Exit
"

command! -nargs=* VimIRC :call s:StartVimIRC(<q-args>)

function! s:ObtainUserInfo(args)
  " Maybe called more than once
  let retval = !strlen(a:args) && (exists('s:nick') && exists('s:user')
	\					      && exists('s:realname'))
  if !retval
    let s:nick = s:StrMatched(a:args, '-n\s*\(\S\+\)', '\1')
    if !strlen(s:nick)
      let s:nick = s:GetVimVar('g:plugin_vimirc_nick')
      if !strlen(s:nick)
	let s:nick = expand('$IRCNICK')
	if s:nick ==# '$IRCNICK'
	  let s:nick = s:Input('Enter your nickname')
	endif
      endif
    endif

    let s:user = s:StrMatched(a:args, '-u\s*\(\S\+\)', '\1')
    if !strlen(s:user)
      let s:user = s:GetVimVar('g:plugin_vimirc_user')
      if !strlen(s:user)
	let s:user = expand('$USER')
	if s:user ==# '$USER'
	  let s:user = s:Input('Enter your username')
	endif
      endif
    endif

    let s:realname = s:StrMatched(a:args,
	  \"--real\\%(name\\)\\==\\(['\"]\\)\\(.\\{-\\}\\)\\1", '\2')
    if !strlen(s:realname)
      let s:realname = s:GetVimVar('g:plugin_vimirc_realname')
      if !strlen(s:realname)
	let s:realname = expand('$NAME')
	if s:realname ==# '$NAME'
	  let s:realname = expand('$IRCNAME')
	  if s:realname ==# '$IRCNAME'
	    let s:realname = s:Input('Enter your full name')
	  endif
	endif
      endif
    endif

    let s:umode = s:StrMatched(a:args, '-m\s*\(\S\+\)', '\1')
    if !strlen(s:umode)
      let s:umode = s:GetVimVar('g:plugin_vimirc_umode')
      if !strlen(s:umode)
	let s:umode = expand('$IRCUMODE')
	if s:umode ==# '$IRCUMODE'
	  let s:umode = 0
	endif
      endif
    endif
  endif

  let retval = (strlen(s:nick) && strlen(s:user) && strlen(s:realname))
  if retval
    let s:server = s:StrMatched(a:args, '-s\s*\(\S\+\)', '\1')
    if !strlen(s:server)
      let s:server = s:GetVimVar('g:plugin_vimirc_server')
      if !strlen(s:server)
	let s:server = 'irc.freenode.net:6667'
      endif
    endif
  else
    unlet s:nick s:user s:realname s:umode
  endif

  return retval
endfunction

function! s:InitVars()
  if exists('s:bufname_prefix') " already inited
    return
  endif

  map <SID>xx <SID>xx
  let s:sid = substitute(maparg('<SID>xx'), 'xx$', '', '')
  unmap <SID>xx

  let s:bufname_prefix	= '_VimIRC_'
  let s:bufname_server	= s:bufname_prefix.'SERVER_'
  let s:bufname_list	= s:bufname_prefix.'LIST_'
  let s:bufname_channel = s:bufname_prefix.'CHANNEL_'
  let s:bufname_names	= s:bufname_prefix.'NAMES_'
  let s:bufname_command = s:bufname_prefix.'COMMAND_'

  " User options below

  " Set your favorite farewell message
  let s:partmsg = s:GetVimVar('g:plugin_vimirc_partmsg')
  if !strlen(s:partmsg)
    let s:partmsg = (s:debug ? 'Testing ' : '').s:client.' (IRC client for Vim)'
  endif
  " Prepend a leading colon
  let s:partmsg = substitute(s:partmsg, '^[^:]', ':&', '')

endfunction

function! s:SetGlobVars()
  let s:eadirection = &eadirection
  set eadirection=ver
  let s:equalalways = &equalalways
  set equalalways
  let s:lazyredraw = &lazyredraw
  set lazyredraw
  let s:showbreak = &showbreak
  let &showbreak = '      '
  let s:statusline = &statusline
  let &statusline = '%{'.s:sid.'GetStatus()}%=%l/%L'
  let s:titlestring = &titlestring
  let s:winminheight = &winminheight
  set winminheight=1
  let s:winwidth = &winwidth
  set winwidth=10
endfunction

function! s:ResetGlobVars()
  let &eadirection = s:eadirection
  let &equalalways = s:equalalways
  let &lazyredraw = s:lazyredraw
  let &showbreak = s:showbreak
  let &statusline = s:statusline
  let &titlestring = s:titlestring
  let &winminheight = s:winminheight
  let &winwidth = s:winwidth
endfunction

function! s:StartVimIRC(...)
  if !has('perl')
    echoerr "To use this, you have to build vim with perl interface. Exiting."
    return
  endif

  if exists('s:opened') && s:opened
    return
  endif

  call s:InitVars()
  if !s:ObtainUserInfo(a:0 ? a:1 : '')
    return
  endif

  call s:SetGlobVars()
  call s:DoCommands()
  call s:DoAutocmds()

  call s:SetEncoding()
  call s:PerlIRC()

  call s:Server(s:server)
  let s:opened = 1

  call s:MainLoop()
endfunction

function! s:QuitVimIRC()
  call s:UndoCommands()
  call s:UndoAutocmds()
  call s:ResetGlobVars()
  call s:QuitServers()
  call s:CloseVimIRC()
  let s:opened = 0
endfunction

function! s:DoCommands()
  delcommand VimIRC
  command! VimIRCQuit :call s:QuitVimIRC()
endfunction

function! s:UndoCommands()
  delcommand VimIRCQuit
  command! -nargs=* VimIRC :call s:StartVimIRC()
endfunction

function! s:DoAutocmds()
  augroup VimIRC
    autocmd!
    " NOTE: Cannot use CursorHold to auto re-enter the loop: getchar() won't
    "	    get a char since key inputs will never be waited after that event.
    execute 'autocmd CursorHold' s:bufname_prefix.'* call s:OfflineMsg()'
  augroup END
endfunction

function! s:UndoAutocmds()
  augroup VimIRC
    autocmd! CursorHold
    "autocmd! BufLeave
  augroup END
endfunction

function! s:MainLoop()
  if !(exists('s:opened') && s:opened && s:IsSockOpen())
    return
  endif

  echo ""
  " TODO: I really want to write this loop in perl, but how could I detect
  " interrupt then?
  while 1
    try
      let key = getchar(0)
      " FIXME: Hitting <C-Tab> or something might raise E132
      if ''.key != '0'
	call s:HandleKey(key)
	continue
      endif
      if !s:RecvData()
	break
      endif
    catch /^IMGONNA/
      " Get out of the loop
      " NOTE: You cannot see new messages posted while posting
      break
    catch /^Vim:Interrupt$/
      match none
      if s:IsBufCommand()
	startinsert!
      endif
      break
    endtry
  endwhile
endfunction

"
" Buffer manipulation
"

" I'm using buffer numbers to access buffers: accessing by name will soon fail
" if user changes directory or something.
" NOTE: I removed the `server' argument from the functions below, just for
"	ease of typing (esp. on the perl's side).

function! s:GetBufNum(bufname)
  let bufnum = -1
  let varname = 's:bufnum_'.a:bufname
  if exists('{varname}')
    if bufloaded({varname})
      let bufnum = {varname}
    else
      unlet {varname}
    endif
  endif
  return bufnum
endfunction

function! s:GetBufNum_Server()
  return s:GetBufNum(s:GenBufName_Server(s:server))
endfunction

function! s:GetBufNum_List()
  return s:GetBufNum(s:GenBufName_List(s:server))
endfunction

function! s:GetBufNum_Channel(channel)
  return s:GetBufNum(s:GenBufName_Channel(s:server, a:channel))
endfunction

function! s:GetBufNum_Names(channel)
  return s:GetBufNum(s:GenBufName_Names(s:server, a:channel))
endfunction

function! s:GetBufNum_Command(channel)
  return s:GetBufNum(s:GenBufName_Command(s:server, a:channel))
endfunction

function! s:SetBufNum(bufname, bufnum)
  let s:bufnum_{a:bufname} = a:bufnum
endfunction

function! s:DeleteBufNum(bufnum)
  unlet! s:bufnum_{bufname(a:bufnum)}
endfunction

function! s:GenBufName_Server(server)
  return s:bufname_server.a:server
endfunction

function! s:GenBufName_List(server)
  return s:bufname_list.a:server
endfunction

function! s:GenBufName_Channel(server, channel)
  return s:bufname_channel.a:server.s:GetChannelSafe(a:channel)
endfunction

function! s:GenBufName_Names(server, channel)
  return s:bufname_names.a:server.s:GetChannelSafe(a:channel)
endfunction

function! s:GenBufName_Command(server, channel)
  return s:bufname_command.a:server.s:GetChannelSafe(a:channel)
endfunction

function! s:GetChannelSafe(channel)
  return escape(tolower(a:channel), '#')
endfunction

function! s:IsBufIRC(...)
  return !match(bufname(a:0 && a:1 ? a:1 : '%'), s:bufname_prefix)
endfunction

function! s:IsBufServer(...)
  return !match(bufname(a:0 && a:1 ? a:1 : '%'), s:bufname_server)
endfunction

function! s:IsBufList(...)
  return !match(bufname(a:0 && a:1 ? a:1 : '%'), s:bufname_list)
endfunction

function! s:IsBufChannel(...)
  return !match(bufname(a:0 && a:1 ? a:1 : '%'), s:bufname_channel)
endfunction

function! s:IsBufCommand(...)
  return !match(bufname(a:0 && a:1 ? a:1 : '%'), s:bufname_command)
endfunction

"
" Opening buffers
"

function! s:OpenBuf(comd, buffer)
  " Avoid "not enough room" error
  let winminheight  = &winminheight
  let winminwidth   = &winminwidth
  set winminheight=0
  set winminwidth=0
  silent execute a:comd a:buffer
  let &winminheight = winminheight
  let &winminwidth  = winminwidth
endfunction

function! s:OpenBuf_Server()
  let bufnum = s:GetBufNum_Server()
  if bufnum >= 0
    if s:SelectWindow(bufnum) < 0
      call s:OpenBuf('split', '+'.bufnum.'buffer')
    endif
  else
    let bufname = s:GenBufName_Server(s:server)
    call s:OpenBuf((s:GetVimVar('s:opened') ? 'split' : 'edit!'), bufname)
    call s:InitBuf_Server(bufname)
  endif

  unlet! b:dead
  silent! wincmd J
endfunction

function! s:OpenBuf_List()
  let bufnum = s:GetBufNum_List()
  " Open it next to the server window
  call s:SelectWindow(s:GetBufNum_Server())
  if bufnum >= 0
    if s:SelectWindow(bufnum) < 0
      call s:OpenBuf('vertical split', '+'.bufnum.'buffer')
    endif
  else
    let bufname = s:GenBufName_List(s:server)
    call s:OpenBuf('vertical split', bufname)
    call s:InitBuf_List(bufname)
  endif
endfunction

function! s:OpenBuf_Channel(channel)
  let bufnum  = s:GetBufNum_Channel(a:channel)
  " TODO: Height should be configurable
  let command = 'botright split'
  if bufnum >= 0
    if s:SelectWindow(bufnum) < 0
      call s:OpenBuf(command, '+'.bufnum.'buffer')
    endif
  else
    let bufname = s:GenBufName_Channel(s:server, a:channel)
    call s:OpenBuf(command, bufname)
    call s:InitBuf_Channel(bufname, a:channel)
  endif
  if &l:winfixheight
    let &l:winfixheight = !&l:winfixheight
  endif
endfunction

function! s:OpenBuf_Names(channel)
  let bufnum  = s:GetBufNum_Names(a:channel)
  let command = 'vertical belowright 11split'
  call s:SelectWindow(s:GetBufNum_Channel(a:channel))
  if bufnum >= 0
    if s:SelectWindow(bufnum) < 0
      call s:OpenBuf(command, '+'.bufnum.'buffer')
    endif
  else
    let bufname = s:GenBufName_Names(s:server, a:channel)
    call s:OpenBuf(command, bufname)
    call s:InitBuf_Names(bufname, a:channel)
  endif
endfunction

function! s:OpenBuf_Command()
  if !(exists('s:opened') && s:opened && s:IsBufIRC())
    return
  endif

  " Set the current server appropriately, so the command/message will be sent
  " to the one user intended
  call s:SetCurrentServer(b:server)

  let channel = s:GetVimVar('b:channel')
  let bufnum  = s:GetBufNum_Command(channel)
  let command = 'botright 1split'
  if bufnum >= 0
    if s:SelectWindow(bufnum) < 0
      call s:OpenBuf(command, '+'.bufnum.'buffer')
    endif
  else
    let bufname = s:GenBufName_Command(s:server, channel)
    call s:OpenBuf(command, bufname)
    call s:InitBuf_Command(bufname, channel)
  endif
  call s:SetCommandMode(channel)

  match none
  if strlen(getline('$'))
    call append('$', '')
  endif
  $
  startinsert
endfunction

function! s:DoSettings()
  setlocal bufhidden=hide
  setlocal buftype=nofile
  setlocal nolist
  setlocal noswapfile
  setlocal nonumber
  nnoremap <buffer> <silent> i	      :call <SID>OpenBuf_Command()<CR>
  nnoremap <buffer> <silent> I	      :call <SID>OpenBuf_Command()<CR>
  nnoremap <buffer> <silent> <Space>  :call <SID>MainLoop()<CR>
endfunction

function! s:DoHilite()
  " NOTE: I'm really bad at syntax highlighting.  It's horrible
  " NOTE: Do not overdo.  It'll slow things down
  syntax match VimIRCUserHead display "^\S\+\%( \S\+:\)\=" contains=@VimIRCUserName
  syntax match VimIRCTime display "^\d\d:\d\d" containedin=VimIRCUserHead contained
  syntax match VimIRCBullet display "[*!]" containedin=VimIRCUserHead contained
  " User names
  syntax cluster VimIRCUserName contains=VimIRCUserMessage,VimIRCUserNotice,VimIRCUserAction,VimIRCUserQuery
  syntax match VimIRCUserMessage  display "<\S\+>" contained
  syntax match VimIRCUserNotice	  display "\[\S\+\]" contained
  syntax match VimIRCUserAction	  display "\*\S\+\*" contained
  syntax match VimIRCUserQuery	  display "?\S\+?" containedin=VimIRCUserHead contained

  highlight link VimIRCTime	    String
  highlight link VimIRCUserHead	    PreProc
  highlight link VimIRCBullet	    WarningMsg
  highlight link VimIRCUserMessage  Identifier
  highlight link VimIRCUserNotice   Statement
  highlight link VimIRCUserAction   WarningMsg
  highlight link VimIRCUserQuery    Question
endfunction

function! s:DoHilite_Server()
  call s:DoHilite()
  syntax region VimIRCUnderline matchgroup=VimIRCIgnore start="" end=""

  highlight link VimIRCUnderline    Underlined
  highlight link VimIRCIgnore	    Ignore
endfunction

function! s:DoHilite_List()
  syntax match VimIRCChannel "^[&#+!]\S\+ \d\+" contains=VimIRCChanMember
  syntax match VimIRCChanMember "\<\d\+\>" contained

  highlight link VimIRCChannel	    Identifier
  highlight link VimIRCChanMember   Number
endfunction

function! s:DoHilite_Channel()
  call s:DoHilite()
  syntax match VimIRCUserEnter display "->" contained containedin=VimIRCUserHead
  syntax match VimIRCUserExit display "<[-=]" contained containedin=VimIRCUserHead
  syntax match VimIRCUserPriv display "[@+]" contained containedin=@VimIRCUserName

  highlight link VimIRCUserEnter  DiffChange
  highlight link VimIRCUserExit	  DiffDelete
  highlight link VimIRCUserPriv	  Statement
endfunction

function! s:DoHilite_Names()
  syntax match VimIRCNamesChop "^@"
  syntax match VimIRCNamesVoice "^+"

  highlight link VimIRCNamesChop  Identifier
  "highlight link VimIRCNamesChop  Statement
  highlight link VimIRCNamesVoice Statement
endfunction

function! s:InitBuf_Server(bufname)
  let b:server	= s:server
  let b:umode	= ''
  let b:title	= '  '.s:nick.' @ '.s:server
  call s:DoSettings()
  call s:DoHilite_Server()
  call s:SetBufNum(a:bufname, bufnr('%'))
endfunction

function! s:InitBuf_List(bufname)
  let b:server	= s:server
  let b:title	= '  List of channels @ '.s:server
  call s:DoSettings()
  setlocal nowrap
  nnoremap <buffer> <silent> <CR> :call <SID>JoinChannel()<CR>
  " I take Mutt's keys
  nnoremap <buffer> <silent> o	  :call <SID>SortSelect()<CR>
  nnoremap <buffer> <silent> O	  :call <SID>SortReverse()<CR>
  call s:DoHilite_List()
  call s:SetBufNum(a:bufname, bufnr('%'))
endfunction

function! s:InitBuf_Channel(bufname, channel)
  let b:server	= s:server
  let b:channel = a:channel
  let b:cmode	= ''
  let b:topic	= ''
  let b:title	= '  '.a:channel.' @ '.s:server

  call s:DoSettings()
  call s:DoHilite_Channel()
  call s:SetBufNum(a:bufname, bufnr('%'))
endfunction

function! s:InitBuf_Names(bufname, channel)
  let b:server	= s:server
  let b:channel = a:channel
  let b:title	= a:channel
  call s:DoSettings()
  call s:DoHilite_Names()
  setlocal nowrap
  call s:SetBufNum(a:bufname, bufnr('%'))
endfunction

function! s:InitBuf_Command(bufname, channel)
  let b:query = ''
  let b:server	= s:server
  let b:channel = a:channel
  call s:DoSettings()
  "setlocal bufhidden=delete
  "setlocal winfixheight
  setlocal nowrap
  nnoremap <buffer> <silent> <CR> :call <SID>SendingCommand()<CR>
  inoremap <buffer> <silent> <CR> <Esc>:call <SID>SendingCommand()<CR>
  nunmap <buffer> i
  nunmap <buffer> I
  " Do not allow opening a new line
  nmap <buffer> o <Nop>
  nmap <buffer> O <Nop>
  " Joining lines either
  nmap <buffer> J <Nop>
  " TODO: gq stuffs
  call s:SetBufNum(a:bufname, bufnr('%'))
endfunction

"
" And closing
"

function! s:CloseServer()
  " Close other related windows before closeing the server window
  call s:CloseCommand(1)
  call s:CloseList()

  let v:errmsg = ''
  while s:SelectWindow(s:GetBufNum_Server()) >= 0
    silent! close
    if strlen(v:errmsg)
      break
    endif
  endwhile
endfunction

function! s:CloseList()
  let bufnum = s:GetBufNum_List()
  if bufnum >= 0
    while s:SelectWindow(bufnum) >= 0
      silent! close
    endwhile
  endif
endfunction

function! s:CloseChannel(channel)
  let bufnum = s:GetBufNum_Channel(a:channel)
  if bufnum >= 0
    while s:SelectWindow(bufnum) >= 0
      silent! close
    endwhile
  endif

  let bufnum = s:GetBufNum_Names(a:channel)
  if bufnum >= 0
    while s:SelectWindow(bufnum) >= 0
      silent! close
    endwhile
  endif
  redraw
endfunction

function! s:CloseCommand(force)
  if exists('s:channel')
    let bufnum = s:GetBufNum_Command(s:channel)
    if bufnum >= 0 && s:SelectWindow(bufnum) >= 0
      call s:PreBufModify()
      call s:BufTrim()
      " Remove duplicates, if any
      if line('$') > 1
	call s:ExecuteSafe('keepjumps', 'normal! G$')
	while search('^\V'.getline('$').'\$', 'w') && line('.') != line('$')
	  delete _
	  .-1
	endwhile
      endif
      call s:PostBufModify()
      if a:force || !strlen(b:query)	" don't close if in query mode
	while s:SelectWindow(bufnum) >= 0
	  silent! close
	endwhile
	" Move the cursor back onto the channel where command mode was
	" triggered.
	if strlen(s:channel)
	  call s:SelectWindow(s:GetBufNum_Channel(s:channel))
	endif
      endif
      redraw
    endif
  endif
endfunction

function! s:CloseVimIRC()
  wincmd b
  let v:errmsg = ''
  while 1
    if !match(bufname('%'), s:bufname_prefix)
      silent! close
      if strlen(v:errmsg)
	enew
	break
      endif
    else
      let winnum = winnr()
      wincmd W
      if winnr() == winnum
	break
      endif
    endif
  endwhile
endfunction

"
" Providing some user interaction
"

function! s:HandleKey(key)
  let char   = nr2char(a:key)
  let oldwin = winnr()
  let search = (char =~# '[/?Nn]')

  match none
  " TODO: Make some mappings user-configurable
  " TODO: Characterwise motions?
  if char =~# '[:]'
    " TODO: How can we enter ex command mode from script?  Requires a new vim
    "	    command (startex or something)?
    throw 'IMGONNAEX'
  elseif char =~# '[iI]'
    call s:OpenBuf_Command()
    throw 'IMGONNAPOST'
  elseif char =~# '[/?]'
    call s:SearchWord(char)
  elseif char == "\<CR>"
    if s:IsBufCommand()
      call s:SendingCommand()
    elseif s:IsBufList()
      call s:JoinChannel()
      echo ""
    else
      execute 'normal!' char
    endif
  elseif char =~# '[oO]'
    if s:IsBufList()
      call s:Sort{char ==# 'o' ? 'Select' : 'Reverse'}()
    endif
  elseif char =~# '[bp]'	" scroll backward
    execute 'normal!' nr2char(2)
  elseif char =~# '[ ]'		" scroll forward
    execute 'normal!' nr2char(6)
  elseif char == "\<C-B>" || char == "\<C-F>"
	\ || char == "\<C-D>" || char == "\<C-U>"
	\ || char == "\<C-E>" || char == "\<C-Y>"
	\ || char == "\<C-L>" || char == "\<C-P>"
	\ || char =~# '[$+-0GHLMNjkn^]'
    " One char commands
    silent! execute 'normal!' char
  elseif char =~# '[`gmz]'
    " Commands which take a second char
    execute 'normal!' char.nr2char(getchar())
  elseif (char + 0) || char == "\<C-W>"
    " Accept things like "15G", "<C-W>2k", etc.
    let comd = char
    while 1
      let key  = getchar()
      let comd = comd.nr2char(key)
      " Continue if it is a number
      if !(key >= 48 && key <= 57)
	break
      endif
    endwhile
    silent! execute 'normal!' comd
  endif
  " Discard excessive keytypes (Chalice)
  while getchar(0)|endwhile

  if winnr() != oldwin
    call s:UpdateTitleBar()
  endif
  call s:Hilite{search ? 'Column' : 'Line'}('.')
  redraw
endfunction

function! s:SendingCommand()
  " TODO: Do confirmation (preferably optionally)
  if !s:IsBufCommand()
    return
  endif

  call s:SetCurrentServer(b:server)
  " The variable used for closing the command-line window.  Ugly, isn't it?
  let s:channel = b:channel

  let line = s:StrTrim(getline('.'))
  let comd = ''
  let args = ''

  if line[0] == '/'
    let rx = '^/\(\S\+\)\%(\s\+\(.\+\)\)\=$'
    if line =~ rx  " could this fail?  I don't know
      " TODO: Allow abbreviated forms of commands
      let comd = toupper(substitute(line, rx, '\1', ''))
      let args = substitute(line, rx, '\2', '')

      if comd =~# '^\%(MSG\|QUERY\)$'
	if comd ==# 'QUERY'
	  " Just entering/quitting query mode, not sending.
	  " NOTE: I'm reusing the command window for querying.  Conversations
	  " will be displayed in the current server window.
	  if strlen(args)
	    let b:query = args
	    call s:SetCommandMode(b:channel)
	    call append('$', '')
	    $
	    startinsert
	    return
	  else
	    let b:query = ''
	  endif
	endif
	let comd = 'PRIVMSG'
      elseif comd =~# '^\%(ME\)$'
	let comd = 'ACTION'
	let args = (strlen(b:query) ? b:query : b:channel).' '.args
      endif

      if strlen(args)
	" This is only for removing a leading colon which user (unnecessarily)
	" appended to the MESSAGE and adding back!  Silly and redundant
	"
	" Commands with the following form:
	"   COMMAND [MESSAGE]
	if comd =~# '^\%(AWAY\|QUIT\|WALLOPS\)$'
	  let rx = '^:\=\(.\+\)$'
	  if args =~ rx
	    let args = substitute(args, rx, ':\1', '')
	  endif
	elseif comd =~# '^\%(USERHOST\|ISON\)$'
	  " User might delimit targets with commas
	  let args = s:StrCompress(substitute(args, ',', ' ', 'g'))
	else
	  let rx = ''
	  if comd =~# '^\%(PART\|TOPIC\|PRIVMSG\|NOTICE\|SQU\%(ERY\|IT\)\|KILL\|ACTION\)$'
	    " COMMAND TARGET [MESSAGE]
	    let rx = '^\(\S\+\)\s\+:\=\(.\+\)$'
	  elseif comd =~# '^\%(KICK\)$'
	    " COMMAND CHANNEL USER [MESSAGE]
	    let rx = '^\(\S\+\s\+\S\+\)\s\+:\=\(.\+\)$'
	  endif
	  if strlen(rx) && args =~ rx
	    let args = substitute(args, rx, '\1', '').' :'.substitute(args, rx,
		  \						      '\2', '')
	  endif
	endif
      endif

      if comd =~# '^\%(NAMES\)$'
	" When receiving a bunch of lines, visit the server window beforehand:
	" avoid the flicker caused by cursor movement between server and
	" channel windows
	call s:SelectWindow(s:GetBufNum_Server())
      endif

      if exists('*s:Send_{comd}')
	call s:Send_{comd}(comd, args)
      elseif comd ==# 'HELP'
	call s:PrintHelp()
      elseif comd ==# 'SERVER'
	call s:Server(args)
      else
	call s:SendCommand(comd, args)
      endif
    endif
  elseif strlen(line)
    let target = strlen(b:query) ? b:query : b:channel
    if strlen(target)
      " Send PRIVMSG to the current channel
      call s:SendMessage('PRIVMSG', target.' :'.line)
    else
      echomsg 'You are not on a channel.'
    endif
  endif

  call s:CloseCommand(0) " might have already been closed before we reach here
  unlet! s:channel
  if comd !=# 'AWAY'  " don't scroll down when awaying, to keep the context
    call s:ExecuteSafe('keepjumps', 'normal! Gzb')
  endif
  redraw
  call s:MainLoop()
endfunction

function! s:JoinChannel()
  let chan = matchstr(getline('.'), '^[&#+!]\S\+')
  if strlen(chan) && s:GetConf_YN("Join the channel ".chan."?")
    call s:SetCurrentServer(b:server)
    call s:Send_JOIN('JOIN', chan)  " first parameter can be any string i think
  endif
endfunction

function! s:SearchWord(comd)
  let word = input(a:comd)
  if strlen(word)
    let @/ = word
  endif
  silent! execute 'normal!' (a:comd == '/' ? 'n' : 'N')
endfunction

function! s:OfflineMsg()
  echo s:IsSockOpen() ? s:IsBufCommand()
	\		? 'Hitting <CR> will send out the current line'
	\		: 'Hit <Space> to get online'
	\	      : 'Do /SERVER command to get connected'
endfunction

function! s:PrintHelp()
  try
    echohl Title
    echo " VimIRC Help\n\n"
    echohl None
    echo " Available commands:\n\n"
    echo "/server host:port"
    echo "\tTry to connect to a new server.  Omitting an argument will prompt"
    echo "\tyou for a new server anyway."
    echo "/quit message"
    echo "\tDisconnect with the current server.  Message is optional."
    echo "/join channel(s)"
    echo "\tJoins specified channels. \"channels\" is a list of one or more of"
    echo "\tchannel names, separated with commas."
    echo "/msg target message"
    echo "\tSends a message to a nick/channel."
    echo "message"
    echo "\tSends a message to the current channel or the user currently"
    echo "\tquerying with."
    echo "/query nick"
    echo "\tStart a query session with a user."
    echo "/query"
    echo "\tClose it."
    echo "/action target message"
    echo "\tSends a message to a nick/channel, pretending something."
    echo "/me message"
    echo "\tSends a message to the current channel/query target, pretending"
    echo "\tsomething."
    echo "/part channel(s) message"
    echo "\tExit from the specified channels.  The last argument is optional."
    echo "\tIf you also omit channels, you'll exit from the current channel."
    echo "\n"
    echohl MoreMsg
    echo "Hit any key to continue"
    call getchar()
  finally
    echohl None
    redraw!
  endtry
endfunction

function! s:NotifyNewEntry(...)
  " If the bottom line is already visible, or just forced to do so,
  if a:0 && a:1 || (line('.') + (winheight(0) - (winline() - 1)) >= line('$'))
    " Scroll down
    call s:ExecuteSafe('keepjumps', 'normal! G')
    call s:HiliteLine('.')
  else
    " And if not, do not scroll.  User might want to stay there to read old
    " messages
    call s:Beep(1)
    " Which redraw is better?  Simple redrawing might cause a flicker (if the
    " update was on a different buffer than the active one), which should be
    " eye-catchy
    if 1
      redraw
    else
      call s:RedrawStatus() " show the correct line number
    endif
  endif
endfunction

function! s:UpdateTitleBar()
  let &titlestring = s:client.': '.(s:IsBufIRC()
	\			    ? b:server.' '.(s:IsBufChannel()
	\					    ? b:channel.': '.b:topic
	\					    : '')
	\			    : fnamemodify(expand('%'), ':p'))
endfunction

function! s:GetStatus()
  return exists('b:title') ? b:title  : bufname('%')
endfunction

function! s:SetUserMode(umode)
  let bufnum = s:GetBufNum_Server()
  if bufnum >= 0
    call setbufvar(bufnum, 'umode', a:umode)
    call setbufvar(bufnum, 'title', '  '.s:nick." [".a:umode.'] @ '.s:server)
    call s:RedrawStatus()
    call s:UpdateTitleBar()
  endif
endfunction

function! s:SetChannelTopic(channel, topic)
  let bufnum = s:GetBufNum_Channel(a:channel)
  if bufnum >= 0
    call setbufvar(bufnum, 'topic', a:topic)
    call s:UpdateTitleBar()
  endif
endfunction

function! s:SetChannelMode(channel, cmode)
  let bufnum = s:GetBufNum_Channel(a:channel)
  if bufnum >= 0
    call setbufvar(bufnum, 'cmode', a:cmode)
    call setbufvar(bufnum, 'title', '  '.a:channel." [".a:cmode.'] @ '.s:server)
    call s:RedrawStatus(1)
  endif
endfunction

function! s:SetCommandMode(channel)
  let bufnum = s:GetBufNum_Command(a:channel)
  if bufnum >= 0
    let query = getbufvar(bufnum, 'query')
    call setbufvar(bufnum, 'title', '  '.(strlen(query)
	  \				  ? 'Querying '.query
	  \				  : 'Posting to '.
	  \(strlen(a:channel) ? a:channel.' @ ' : '').s:server))
    call s:RedrawStatus()
  endif
endfunction

"
" Misc. utility functions
"

function! s:SelectWindow(bufnum)
  let winnum = -1
  if a:bufnum
    let winnum = bufwinnr(a:bufnum)
    if winnum >= 0 && winnum != winnr()
      execute winnum.'wincmd w'
    endif
  endif
  return winnum
endfunction

function! s:Beep(times)
  if !a:times
    return
  endif

  try
    let line = line('.')
    let col  = col('.')
    let errorbells = &errorbells
    let visualbell = &visualbell
    set errorbells
    set novisualbell

    let i = 0
    normal! 0
    while i < a:times
      normal! h
      let i = i + 1
      if (a:times - i)  " do not sleep for the last time
	sleep 250 m
      endif
    endwhile
  finally
    let &errorbells = errorbells
    let &visualbell = visualbell
    call cursor(line, col)
  endtry
endfunction

function! s:ExecuteSafe(prefix, comd)
  execute (exists(':'.a:prefix) == 2 ? a:prefix : '') a:comd
endfunction

function! s:HiliteColumn(bogus, ...)
  execute 'match' (a:0 ? a:1 : 'Cursor') '/\%#\k*/'
endfunction

function! s:HiliteLine(lnum, ...)
  execute 'match' (a:0 ? a:1 : 'Cursor') '/^.*\%'.(a:lnum
	\					    ? a:lnum
	\					    : line(a:lnum)).'l.*$/'
endfunction

function! s:GetTime(short, ...)
  return strftime((a:short ? '%H:%M' : '%Y/%m/%d %H:%M:%S'), (a:0 && a:1 ? a:1 : localtime()))
endfunction

function! s:RedrawStatus(...)
  if exists(':redrawstatus')
    execute 'redrawstatus'.(a:0 && a:1 ? '!' : '')
  endif
endfunction

function! s:PreBufModify()
  let s:save_undolevels = &undolevels
  set undolevels=-1
  setlocal modifiable
endfunction

function! s:PostBufModify()
  if exists('s:save_undolevels')
    let &undolevels = s:save_undolevels
    unlet s:save_undolevels
  endif
  "setlocal nomodifiable
endfunction

function! s:Input(msg, ...)
  return s:StrCompress(input(a:msg.': ', (a:0 ? a:1 : '')))
endfunction

function! s:IsBufEmpty()
  return !(line('$') > 1 || strlen(getline(1)))
endfunction

function! s:GetVimVar(varname)
  return exists('{a:varname}') ? {a:varname} : ''
endfunction

function! s:GetConf_YN(msg)
  echo a:msg.' (y/n): '
  return (nr2char(getchar()) ==? 'y')
endfunction

function! s:StrMatched(str, pat, sub)
  " A wrapper function to substitute().  First extract an interesting part
  " upon which we perform matching, so that only necessary string (sub) will
  " be obtained.  An empty string will be returned on failure.
  " I took this clever trick from Chalice.
  return substitute(matchstr(a:str, a:pat), a:pat, a:sub, '')
endfunction

" Remove unnecessary spaces in a string
function! s:StrTrim(str)
  return substitute(a:str, '\%(^\s\+\|\s\+$\)', '', 'g')
endfunction

function! s:StrCompress(str)
  return substitute(s:StrTrim(a:str), '\s\{2,\}', ' ', 'g')
endfunction

function! s:BufClear()
  silent %delete _
endfunction

function! s:BufTrim()
  while search('^\s*$', 'w') && line('$') > 1
    delete _
  endwhile
endfunction

"
" And the Perl part
"

if has('perl')
function! s:SetEncoding()
  let encs = ''
  if &encoding ==# 'cp932'
    let encs = '7bit-jis'
  endif
  if !strlen(encs)
    return
  endif

  perl <<EOP
{
  use Encode;
  use Encode::Guess;

  $Encoding = VIM::Eval('&encoding');
  Encode::Guess->set_suspects(split(/,/, scalar(VIM::Eval('l:encs'))));
}
EOP
endfunction

function! s:SortSelect()
  if !s:IsBufList()
    return
  endif

  let choice= confirm("Sort by what?", "&channel\n&member\n&topic")
  if !choice
    return
  endif

  if choice == 1
    let cmp = 'channel'
  elseif choice == 2
    let cmp = 'member'
  elseif choice == 3
    let cmp = 'topic'
  endif

  let oldline = getline('.')
  call s:PreBufModify()
  call s:SortList(cmp, (s:GetVimVar('b:sortdir') + 0))
  call s:PostBufModify()
  call search('^\V'.oldline.'\$', 'w')
endfunction

function! s:SortReverse()
  if !s:IsBufList()
    return
  endif

  let lnum = line('.')
  call s:PreBufModify()
  perl $curbuf->Set(1, reverse($curbuf->Get(1 .. $curbuf->Count())))
  call s:PostBufModify()
  let b:sortdir = !(s:GetVimVar('b:sortdir'))
  execute (line('$') - lnum + 1)
endfunction

function! s:SortList(cmp, dir)
  perl <<EOP
{
  my $cmp = VIM::Eval('a:cmp');
  my $dir = VIM::Eval('a:dir');
  my @lns = $curbuf->Get(1 .. $curbuf->Count());

  if ($cmp eq 'channel')
    {
      @lns = map { $_->[0] }
	      sort {  if ($dir) { lc($b->[1]) cmp lc($a->[1]) }
		      else	{ lc($a->[1]) cmp lc($b->[1]) } }
		map { [ $_, /^(\S+).*$/ ] } @lns;
    }
  elsif ($cmp eq 'member')
    {
      @lns = map { $_->[0] }
	      sort {  if ($dir) { $b->[1] <=> $a->[1] }
		      else	{ $a->[1] <=> $b->[1] } }
		map { [ $_, /^\S+\s+(\d+).*$/ ] } @lns;
    }
  else
    {
      @lns = map { $_->[0] }
	      sort {  if ($dir) { $b->[1] cmp $a->[1] }
		      else	{ $a->[1] cmp $b->[1] } }
		map { [ $_, /^\S+\s+\d+\s*:(.*)$/ ] } @lns;
    }
  $curbuf->Set(1, @lns);
}
EOP
endfunction

function! s:Send_ACTION(comd, args)
  perl <<EOP
{
  my ($chan, $mesg) = (VIM::Eval('a:args') =~ /^(\S+) :(.+)$/);
  send_ctcp(1, $chan, \$mesg);

  unless (is_channel($chan))
    {
      $chan = '';
    }
  add_line($chan, "*$Current_Server->{'nick'}*: $mesg");
}
EOP
endfunction

function! s:Send_JOIN(comd, args)
  " TODO: What to do with 'JOIN 0'?  Especially when it is not supported by
  "	  the server?
  perl <<EOP
{
  my $chans = VIM::Eval('a:args');

  foreach my $chan (split(/,/, $chans))
    {
      if (is_channel($chan))
	{
	  VIM::DoCommand("call s:OpenBuf_Channel(\"$chan\")");
	  unless (get_channel($chan))	# not joined yet
	    {
	      my $cchan = $chan;
	      if (1 && $Current_Server->{'encoding'})
		{
		  Encode::from_to($cchan, $Encoding,
						$Current_Server->{'encoding'});
		}
	      send_msg("JOIN %s", $cchan);
	      add_line($chan, "*: Now talking in $chan");
	      if (VIM::Eval('!strlen(getline(1))'))
		{
		  $curbuf->Delete(1);
		}
	      add_channel($chan);
	    }
	}
    }
}
EOP
  " I don't like to call CloseCommand() in various parts, but I just want to
  " stay here (the last opened channel)
  let bufnum = bufnr('%')
  call s:CloseCommand(1)
  call s:SelectWindow(bufnum)
endfunction

function! s:Send_LIST(comd, args)
  " MEMO: Some servers don't send 321 RPL_LISTSTART, so open a list buffer
  " befor we send the command
  call s:OpenBuf_List()
  call s:PreBufModify()
  call s:BufClear()
  call s:PostBufModify()
  call s:SendCommand(a:comd, a:args)
endfunction

function! s:Send_NICK(comd, args)
  let args = a:args
  if !strlen(a:args)
    let args = s:Input('Enter a new nickname')
    if !strlen(args)
      return
    endif
  endif

  let s:nick = args
  perl <<EOP
{
  $Current_Server->{'nick'} = VIM::Eval('s:nick');
}
EOP
  call s:SetUserMode('')
  call s:SendCommand(a:comd, s:nick)
endfunction

function! s:Send_PART(comd, args)
  let args = strlen(a:args) ? a:args : (exists('b:channel')
	\				? b:channel.' '.s:partmsg : '')
  if !strlen(args)
    return
  endif

  perl <<EOP
{
  my ($chans, $mesg) = (VIM::Eval('l:args') =~ /^(\S+)(?: (.+))?$/);

  # TODO: Do quoting and encoding
  foreach my $chan (split(/,/, $chans))
    {
      if (is_channel($chan))
	{
	  send_msg("PART %s %s", $chan, $mesg);
	}
    }
}
EOP
endfunction

function! s:Send_QUIT(comd, args)
  let mesg = strlen(a:args) ? a:args : s:partmsg
  let bufnum = s:GetBufNum_Server()
  if bufnum >= 0
    " Mark this buffer as dead so that the next /SERVER invocation will close it
    call setbufvar(bufnum, 'dead', 1)
  endif

  perl <<EOP
{
  my $mesg = VIM::Eval('l:mesg');

  while (my $chan = each(%{$Current_Server->{'chans'}}))
    {
      VIM::DoCommand("call s:CloseChannel(\"$chan\")");
    }
  delete($Current_Server->{'chans'});

  # $mesg itself already contains a leading colon
  send_msg("QUIT %s", $mesg);
}
EOP
  call s:CloseServer()
endfunction

function! s:Send_NOTICE(comd, args)
  call s:SendMessage(a:comd, a:args)
endfunction

function! s:Send_PRIVMSG(comd, args)
  call s:SendMessage(a:comd, a:args)
endfunction

function! s:SendMessage(comd, args)
  if !strlen(a:args)
    return
  endif

  perl <<EOP
{
  my $comd = VIM::Eval('a:comd');

  if (my ($chan, $mesg) = (VIM::Eval('a:args') =~ /^(\S+) :(.+)$/))
    {
      send_msg("%s %s :%s", ($comd ? $comd : 'PRIVMSG'), $chan, $mesg);
      unless (is_channel($chan))
	{
	  $chan = '';
	}
      if ($comd eq 'PRIVMSG')
	{
	  add_line($chan, "<$Current_Server->{'nick'}>: $mesg");
	}
      else
	{
	  add_line($chan, "[$Current_Server->{'nick'}]: $mesg");
	}
    }
}
EOP
endfunction

function! s:SendCommand(comd, args)
  perl <<EOP
{
  my $comd = VIM::Eval('a:comd');
  my $args = VIM::Eval('a:args');

  send_msg("%s".($args ? " %s" : ""), $comd, $args);
}
EOP
endfunction

function! s:SetCurrentServer(server)
  perl <<EOP
{
  my $server = VIM::Eval('a:server');
  if (defined($Servers{$server}))
    {
      $Current_Server = $Servers{$server};
      VIM::DoCommand('let s:server = a:server');
    }
}
EOP
endfunction

" Quit all connected servers at once
function! s:QuitServers()
  perl <<EOP
{
  while (my $server = each(%Servers))
    {
      $Current_Server = $Servers{$server};
      if ($Current_Server->{'conn'})
	{
	  send_msg("QUIT");
	}
    }
}
EOP
  call s:ResetPerlVars()
endfunction

function! s:Server(server)
  let server = a:server
  let port = 0
  if !strlen(a:server)
    let server = s:Input('Enter server name', s:GetVimVar('b:server'))
    if !strlen(server)
      return
    endif
  endif

  let rx = '^\(\S\+\):\(\d\+\)$'
  if server =~ rx
    let port = substitute(server, rx, '\2', '')
    let server = substitute(server, rx, '\1', '')
  else
    let port = s:Input('Specify port number', 6667)
  endif
  if !(strlen(server) && strlen(port))
    return
  endif

  call s:CloseCommand(1)
  if server !=# s:GetVimVar('s:server')
    let s:server = server
  endif

  " Close the currently open server window (only if it is marked disconnected)
  let close = s:IsBufIRC() && exists('b:dead') && b:server !=# server
	\     ? bufnr('%') : 0
  call s:OpenBuf_Server()

  if close
    while s:SelectWindow(close) >= 0
      close
    endwhile
  endif

  call s:PreBufModify()
  call {s:IsBufEmpty() ? 'setline' : 'append'}('$', s:GetTime(1).
	\' * Connecting to '.server.'...')
  call s:PostBufModify()
  $

  perl <<EOP
{
  my $server= VIM::Eval('l:server');
  my $port  = VIM::Eval('l:port') + 0;

  my $nick  = VIM::Eval('s:nick');
  my $user  = VIM::Eval('s:user');
  my $realname= VIM::Eval('s:realname');
  my $umode = VIM::Eval('s:umode');

  if ($port <= 0)
    {
      $port = 6667;
    }

  unless (defined($Servers{$server})
	  && $Servers{$server}->{'conn'}
	  && $Servers{$server}->{'port'} == $port)
    {
      my $sock = establish_connection($server, $port);

      $Servers{$server} = { server    => $server,
			    port      => $port,
			    sock      => $sock,
			    conn      => 0,
			    nick      => $nick,
			    away      => 0,
			    motd      => 0,
			    chans     => undef,
			    encode    => undef,
			    lastbuf   => undef	};

      $Current_Server = $Srevres{$sock->peerhost()} = $Servers{$server};

      unless (defined($Sockets))
	{
	  use IO::Select;
	  $Sockets = IO::Select->new();
	}
      $Sockets->add($Current_Server->{'sock'});

      send_msg("NICK %s", $nick);
      send_msg("USER %s %s * :%s", $user, $umode, $realname);
    }
}
EOP
endfunction

function! s:RecvData()
  let retval = 1

  perl <<EOP
{
  my ($r) = IO::Select->select($Sockets, undef, undef, 0);

  foreach my $sock (@{$r})
    {
      my ($buffer, @lines);

      $Current_Server = $Srevres{$sock->peerhost()};
      VIM::DoCommand("let s:server = \"$Current_Server->{'server'}\"");

      sysread($sock, $buffer, (1024 * 2));
      unless ($buffer)
	{
	  $Sockets->remove($sock);
	  set_connected(0);

	  unless ($Sockets->count())
	    {
	      VIM::DoCommand('let retval = 0');
	      last;
	    }
	  next;
	}

      if ($Current_Server->{'lastbuf'})
	{
	  $buffer = $Current_Server->{'lastbuf'}.$buffer;
	  $Current_Server->{'lastbuf'} = undef;
	}

      @lines = split(/\r?\n/, $buffer);
      if (substr($buffer, -1) ne "\n")
	{
	  # Data obtained partially. Save the last line for later use
	  $Current_Server->{'lastbuf'} = pop(@lines);
	}

      foreach my $line (@lines)
	{
	  if ($Encoding)
	    {
	      unless ($Current_Server->{'motd'} || $Current_Server->{'encode'})
		{
		  get_encode(\$line);
		}
	      if ($Current_Server->{'encode'})
		{
		  Encode::from_to($line, $Current_Server->{'encode'},
								  $Encoding);
		}
	    }
	  parse_line(\$line);
	}
    }
}
EOP
  return retval
endfunction

function! s:IsSockOpen()
  perl <<EOP
{
  if (defined($Sockets))
    {
      VIM::DoCommand('return '.$Sockets->count());
    }
}
EOP
  return 0
endfunction

function! s:ResetPerlVars()
  perl <<EOP
{
  undef %Servers;
  undef %Srevres;
  undef $Current_Server;
  undef $Sockets;
}
EOP
endfunction

function! s:PerlIRC()
  " Don't use strict
  perl <<EOP

our %Servers;		# servers information with sock object for each
our %Srevres;		# table for IP-address to server object mappings
our $Current_Server;	# reference referring an element of %Servers
our $From_Server;	# simple string value of the last sender's name@host
our $Sockets;		# IO::Select object

our $USER_CHOP		= 0x01;
our $USER_VOICE		= 0x02;

our $Encoding;

sub vim_getvar
{
  my $var = shift;
  return VIM::Eval("exists('$var')") ? scalar(VIM::Eval("$var")) : undef;
}

sub vim_printf
{
  my $format = shift;
  my $mesg = sprintf($format, @_);
  VIM::Msg($mesg);
}

sub send_msg
{
  my $format = shift;
  my $sock = $Current_Server->{'sock'};

  if (defined($sock) && $sock->connected())
    {
      printf($sock "$format\r\n", @_);
    }
}

sub set_connected
{
  my $conn = shift;

  $Current_Server->{'conn'} = $conn;
  unless ($conn)
    {
      $Current_Server->{'motd'} = 0;
      delete($Current_Server->{'sock'});
    }
}

sub establish_connection
{
  use IO::Socket;

  my ($host, $port) = @_;
  my $sock = IO::Socket::INET->new( PeerAddr  => $host,
				    PeerPort  => $port,
				    Proto     => 'tcp',
				    Timeout   => 10);
  unless ($sock)
    {
      die "$@";
    }

  return $sock;
}

sub get_encode
{
  my $line  = shift;
  my $encode= Encode::Guess->guess(${$line});

  if (ref($encode) && $encode->name() !~ /^(?:ascii|utf8)$/)
    {
      $Current_Server->{'encode'} = $encode->name();
    }
}

sub is_channel
{
  # TODO: This might not be enough
  return (shift =~ /^[&#+!]/);
}

sub is_me
{
  return (shift eq $Current_Server->{'nick'});
}

if (0)
  {
    # TODO: Follow the rules described in:
    #   "REVISED AND UPDATED CTCP SPECIFICATION"
    #   Dated Fri, 12 Aug 94 00:21:54 edt
    #   By ben@gnu.ai.mit.edu et al.
    sub quote_low
    {
      my $line = shift;
    }

    sub dequote_low
    {
    }

    sub quote_ctcp
    {
      my $line = shift;
      ${$line} = "\x01${$line}\x01";
    }

    sub dequote_ctcp
    {
    }
  }

sub send_ctcp
{
  my ($query, $to, $mesg) = @_;
  send_msg("%s %s :\x01%s\x01", ($query ? 'PRIVMSG' : 'NOTICE'), $to, ${$mesg});
}

sub process_ctcp
{
  my ($from, $pref, $chan, $mesg) = @_;

  while (${$mesg} =~ s/\x01(.*?)\x01//)
    {
      # TODO: flood-protection codes here
      if (my ($comd, $args) = ($1 =~ /^(\S+)(?:\s*(.+))?$/))
	{
	  if ($comd eq 'ACTION')
	    {
	      add_line($chan, "*$pref$from*: $args");
	    }
	  else
	    {
	      add_line($chan, "?$pref$from?: $comd $args");
	      if ($comd eq 'ECHO' || $comd eq 'PING')
		{
		  send_ctcp(0, $from, \"$comd $args");
		}
	      elsif ($comd eq 'TIME')
		{
		  my $time = VIM::Eval('s:GetTime(0)');
		  send_ctcp(0, $from, \"$comd :$time");
		}
	      elsif ($comd eq 'VERSION')
		{
		  my $client = VIM::Eval('s:client');
		  send_ctcp(0, $from, \"$comd :$client");
		}
	      else
		{
		  # TODO:
		  send_ctcp(0, $from, \"ERRMSG $comd :unknown query");
		}
	    }
	}
    }

  return length(${$mesg});
}

sub add_channel
{
  my $chan = lc(shift);

  unless (exists($Current_Server->{'chans'}->{$chan}))
    {
      $Current_Server->{'chans'}->{$chan} = { umode => 0,
					      cmode => 0,
					      nicks => {} };
    }
}

sub get_channel
{
  my $chan = lc(shift);
  return $Current_Server->{'chans'}->{$chan};
}

sub get_nicks
{
  my $cref = get_channel(shift);
  return $cref->{'nicks'};
}

sub delete_channel
{
  my $chan = lc(shift);

  if (exists($Current_Server->{'chans'}->{$chan}))
    {
      delete($Current_Server->{'chans'}->{$chan});
    }
  VIM::DoCommand("call s:CloseChannel(\"$chan\")");
}

sub get_nickprefix
{
  my $mode = shift;
  my $pref = '';

  if ($mode & $USER_CHOP)
    {
      $pref .= '@';
    }
  if ($mode & $USER_VOICE)
    {
      $pref .= '+';
    }
  return $pref;
}

sub add_nick
{
  my ($nick, $mode, $chan) = @_;
  my $cref = get_channel($chan);

  if ($nick eq $Current_Server->{'nick'})
    {
      $cref->{'umode'} = $mode;
    }
  $cref->{'nicks'}->{$nick} = $mode;
}

sub rename_nick
{
  my ($old, $new, $chan) = @_;
  my $nref = get_nicks($chan);

  if (exists($nref->{$old}))
    {
      my $mode = $nref->{$old};
      delete_nick($old, $chan);
      add_nick($new, $mode, $chan);
      return 1;
    }
  return 0;
}

sub delete_nick
{
  my ($nick, $chan) = @_;
  my $nref = get_nicks($chan);

  if (exists($nref->{$nick}))
    {
      delete($nref->{$nick});
      return 1;
    }
  return 0;
}

sub list_nicks
{
  my $chan = lc(shift);
  my $nref = get_nicks($chan);
  my @nicks= map { get_nickprefix($nref->{$_}).$_ } keys(%{$nref});

  # Use bufnum since window number can change
  my $bnum = VIM::Eval("bufnr('%')");
  VIM::DoCommand("call s:OpenBuf_Names(\"$chan\")");
  VIM::DoCommand('call s:PreBufModify()');
  $curbuf->Delete(1, $curbuf->Count());

  foreach my $nick (sort { lc($b) cmp lc($a) } @nicks)
    {
      $curbuf->Append(0, $nick);
    }

  $curbuf->Delete($curbuf->Count());
  VIM::DoCommand('call s:PostBufModify()');
  VIM::DoCommand("call s:SelectWindow($bnum)");
  VIM::DoCommand('redraw');
}

sub add_line
{
  my $chan = lc(shift);
  my $line = ref($_[0]) ? $_[0] : \$_[0];
  my $bnum = is_channel($chan) ? VIM::Eval("s:GetBufNum_Channel(\"$chan\")")
			       : VIM::Eval("s:GetBufNum_Server()");
  my $wnum = VIM::Eval('winnr()');  # remember the current window

  # TODO: Data for hidden channels will be discarded, which is not desirable
  if (VIM::Eval("s:SelectWindow($bnum) >= 0"))
    {
      # I think Vim's strftime is much faster than perl's equivalent
      my $time = VIM::Eval('s:GetTime(1)');

      VIM::DoCommand('call s:PreBufModify()');
      $curbuf->Append($curbuf->Count(), "$time ${$line}");
      unless ($Current_Server->{'away'})
	{
	  # Shouldn't scroll down nor beep while you're away
	  VIM::DoCommand('call s:NotifyNewEntry()');
	}
      VIM::DoCommand('call s:PostBufModify()');
    }
  VIM::DoCommand("${wnum}wincmd w");
  VIM::DoCommand('redraw');
}

sub parse_number
{
  my ($from, $comd, $args) = @_;
  my ($nick, $mesg) = (${$args} =~ /^(\S+) :?(.*)$/);

  if (0)
    {
      vim_printf("from=%s comd=%s args=%s", $from, $comd, ${$args});
    }

  if ($comd == 001)	# RPL_WELCOME
    {
      set_connected(1);
      add_line('', $mesg);
    }
  elsif ($comd == 002)	# RPL_YOURHOST
    {
      add_line('', $mesg);
    }
  elsif ($comd == 003)	# RPL_CREATED
    {
      add_line('', $mesg);
    }
  elsif ($comd == 004)	# RPL_MYINFO
    {
      add_line('', $mesg);
    }
  elsif ($comd == 005)	# RPL_BOUNCE
    {
      # Most servers do not seem to use this code as what RFC suggests:
      # instead, they use it to indicate what options they have set, e.g., the
      # maximum length of nick
      # TODO: Make use of those options?
      add_line('', $mesg);
    }
  elsif ($comd == 221)	# RPL_UMODEIS
    {
      if ($mesg =~ /^(\S+)/)
	{
	  add_line('', "*: $nick sets mode: $1");
	  VIM::DoCommand("call s:SetUserMode(\"$1\")");
	}
    }
  elsif ($comd >= 250 && $comd <= 259)  # RPL_LUSERCLIENT etc.
    {
      add_line('', $mesg);
    }
  elsif ($comd == 265 || $comd == 266)
    {
      add_line('', $mesg);
    }
  elsif ($comd == 301)	# RPL_AWAY
    {
      if (my ($nick, $mesg) = ($mesg =~ /^(\S+) :(.*)$/))
	{
	  add_line('', "$nick is away: $mesg");
	}
    }
  elsif ($comd == 303)	# RPL_ISON
    {
      add_line('', "ISON: $mesg");
    }
  elsif ($comd == 305 || $comd == 306)	# RPL_UNAWAY/RPL_NOWAWAY
    {
      $Current_Server->{'away'} = ($comd == 306);
      add_line('', $mesg);
    }
  elsif ($comd == 311 || $comd == 314)	# RPL_WHOISUSER
    {
      if (my ($nick, $user, $host) = ($mesg =~ /^(\S+) (\S+) (.*)$/))
	{
	  add_line('', "$nick ".($comd == 311 ? "i" : "wa").
							"s ${user}\@${host}");
	}
    }
  elsif ($comd == 312)	# RPL_WHOISSERVER
    {
      if (my ($nick, $server) = ($mesg =~ /^(\S+) (.*)$/))
	{
	  add_line('', "$nick using $server");
	}
    }
  elsif ($comd == 317)	# RPL_WHOISIDLE
    {
      if (my ($nick, $idle, $signon) = ($mesg =~ /^(\S+) (\d+)(?: (\d+))?/))
	{
	  my $idlestr = "$idle seconds";
	  if ($idle >= 60)
	    {
	      my $min = sprintf("%d", $idle / 60);
	      my $sec = $idle % 60;
	      $idlestr = "$min mins $sec secs";
	    }
	  add_line('', "$nick has been idle for $idlestr".($signon ?
			    ', signed on '.VIM::Eval("s:GetTime(0, $signon)")
								   : ''));
	}
    }
  elsif ($comd == 318 || $comd == 369)	# RPL_ENDOFWHOIS
    {
      if (my ($nick, $mesg) = ($mesg =~ /^(\S+) :?(.*)$/))
	{
	  add_line('', "$nick $mesg");
	}
    }
  elsif ($comd == 319)	# RPL_WHOISCHANNELS
    {
      if (my ($nick, $chan) = ($mesg =~ /^(\S+) :(.*)$/))
	{
	  add_line('', "$nick on $chan");
	}
    }
  elsif ($comd == 320)	#
    {
      if (my ($nick, $mesg) = ($mesg =~ /^(\S+) :(.*)$/))
	{
	  add_line('', "$nick $mesg");
	}
    }
  elsif ($comd == 321)	# RPL_LISTSTART
    {
      add_line('', '*: Listing channels...');
    }
  elsif ($comd == 322)	# RPL_LIST
    {
      # TODO: It's too unefficient to do this all the time.
      VIM::DoCommand('call s:SelectWindow(s:GetBufNum_List())');
      $curbuf->Append($curbuf->Count(), $mesg);
    }
  elsif ($comd == 323)	# RPL_LISTEND
    {
      VIM::DoCommand('call s:BufTrim()|call s:HiliteLine(".")');
      add_line('', $mesg);
    }
  elsif ($comd == 324)	# RPL_CHANNELMODEIS
    {
      if (my ($chan, $mode) = ($mesg =~ /^(\S+) :?(\S+)/))
	{
	  VIM::DoCommand("call s:SetChannelMode(\"$chan\", \"$mode\")");
	}
    }
  elsif ($comd == 329)
    {
      if (my ($chan, $time) = ($mesg =~ /^(\S+) (\d+)$/))
	{
	  $time = VIM::Eval("s:GetTime(0, $time)");
	  add_line($chan, "*: $chan came into existence on $time");
	}
    }
  elsif ($comd == 331)	# RPL_NOTOPIC
    {
      if (my ($chan, $mesg) = ($mesg =~ /^(\S+) :?(.*)$/))
	{
	  add_line($chan, "*: $mesg");
	}
    }
  elsif ($comd == 332)	# RPL_TOPIC
    {
      if (my ($chan, $topic) = ($mesg =~ /^(\S+) :(.*)$/))
	{
	  add_line($chan, "*: Topic for $chan:");
	  add_line($chan, "*: $topic");
	  VIM::DoCommand("call s:SetChannelTopic(\"$chan\", \"$topic\")");
	}
    }
  elsif ($comd == 333)
    {
      if (my ($chan, $nick, $time) = ($mesg =~ /^(\S+) (\S+) (\d+)$/))
	{
	  add_line($chan, "*: Topic set by $nick at ".
			  VIM::Eval("s:GetTime(0, $time)"));
	}
    }
  elsif ($comd == 353)	# RPL_NAMREPLY
    {
      my ($type, $chan, $nicks) = ($mesg =~ /^(.) (\S+) :(.*)$/);

      if (get_channel($chan))
	{
	  foreach my $nick (split(/ /, $nicks))
	    {
	      my $mode = 0;
	      if ($nick =~ s/^@//)
		{
		  $mode |= $USER_CHOP;
		}
	      if ($nick =~ s/^\+//)
		{
		  $mode |= $USER_VOICE;
		}
	      add_nick($nick, $mode, $chan);
	    }
	}
      else
	{
	  add_line('', "*: Names for $chan: $nicks");
	}
    }
  elsif ($comd == 366)	# RPL_ENDOFNAMES
    {
      my ($chan) = ($mesg =~ /^(\S+)/);

      if (get_channel($chan))
	{
	  list_nicks($chan);
	}
      else
	{
	  add_line('', "*: End of names");
	}
    }
  elsif ($comd == 372)	# RPL_MOTD
    {
      add_line('', $mesg);
    }
  elsif ($comd == 375)	# RPL_MOTDSTART
    {
      add_line('', $mesg);
    }
  elsif ($comd == 376)	# RPL_ENDOFMOTD
    {
      add_line('', $mesg);
      unless ($Current_Server->{'motd'})
	{
	  $Current_Server->{'motd'} = 1;
	  # Auto-obtain the user mode string upon first connection, after
	  # displaying MOTD
	  send_msg("MODE %s", $Current_Server->{'nick'});
	}
    }
  elsif ($comd == 391)	# RPL_TIME
    {
      add_line('', $mesg);
    }
  elsif ($comd >= 431 && $comd <= 433)	# ERR_NICKNAMEINUSE etc.
    {
      add_line('', $mesg);
      unless ($Current_Server->{'conn'})
	{
	  VIM::DoCommand("call s:Send_NICK('NICK', '')");
	}
    }
  else
    {
      add_line('', "$comd: $mesg");
    }
}

sub parse_join
{
  my ($from, $args) = @_;

  if (my ($chan) = (${$args} =~ /^:?(\S+)/))
    {
      add_nick($from, 0, $chan);
      add_line($chan, "->: Enter $from");
      if ($from eq $Current_Server->{'nick'})
	{
	  # If it is you who is entering, get the channel mode.
	  send_msg("MODE %s", $chan);
	}
      list_nicks($chan);
    }
}

sub parse_kick
{
  my ($from, $args) = @_;

  if (my ($chan, $nick, $mesg) = (${$args} =~ /^(\S+) (\S+) :(.*)$/))
    {

      delete_nick($nick, $chan);

      if (is_me($nick))
	{
	  add_line('', "!: You have been kicked out of channel $chan by $from ($mesg)");
	  delete_channel($chan);
	}
      else
	{
	  add_line($chan, "!: $from kicks out $nick ($mesg)");
	  list_nicks($chan);
	}
    }
}

sub parse_mode
{
  my ($from, $args) = @_;

  if (my ($chan, $mode) = (${$args} =~ /^(\S+) :(.*?)\s*$/))
    {
      if (is_channel($chan))
	{
	  my $cref = get_channel($chan);
	  my $nref = get_nicks($chan);

	  while ($mode =~ /([-+])(.)(?:\s+([^-+]\S+))?/g)
	    {
	      my $add = ($1 eq '+');
	      my $nick;
	      my $val;

	      for ($2)
		{
		  # TODO: Handle channel modes
		  if (/o/i)
		    {
		      $nick = $3;
		      $val  = $USER_CHOP;
		      last;
		    }
		  if (/v/)
		    {
		      $nick = $3;
		      $val  = $USER_VOICE;
		      last;
		    }
		}

	      if ($nick)
		{
		  if ($add)
		    {
		      $nref->{$nick} |= $val;
		      if (is_me($nick))
			{
			  $cref->{'umode'} |= $val;
			}
		    }
		  else
		    {
		      $nref->{$nick} &= ~$val;
		      if (is_me($nick))
			{
			  $cref->{'umode'} &= ~$val;
			}
		    }
		  list_nicks($chan);
		}
	      else
		{
		  if ($add)
		    {
		      $cref->{'cmode'} |= $val;
		    }
		  else
		    {
		      $cref->{'cmode'} &= ~$val;
		    }
		}
	    }

	  add_line($chan, "*: $from sets new mode: $mode");
	  if (0)
	    {
	      VIM::DoCommand("call s:SetChannelMode(\"$chan\", \"$mode\")");
	    }
	}
      else
	{
	  # Should be about yourself, right?
	  add_line('', "*: $chan sets mode: $mode");
	  VIM::DoCommand("call s:SetUserMode(\"$mode\")");
	}
    }
}

sub parse_nick
{
  my ($from, $args) = @_;

  if (my ($nick) = (${$args} =~ /^:(.*)$/))
    {
      if (is_me($from))
	{
	  add_line('', "*: New nick $nick approved");
	}
      while (my $chan = each(%{$Current_Server->{'chans'}}))
	{
	  if (rename_nick($from, $nick, $chan))
	    {
	      add_line($chan, "*: $from is now known as $nick");
	      list_nicks($chan);
	    }
	}
    }
}

sub parse_notice
{
  my ($from, $args) = @_;

  if (my ($chan, $mesg) = (${$args} =~ /^(\S+) :?(.*)$/))
    {
      my $pref;
      if (is_channel($chan))
	{
	  my $nref = get_nicks($chan);
	  $pref = get_nickprefix($nref->{$from});
	}
      else
	{
	  $chan = '';
	}
      add_line($chan, "[$pref$from]: $mesg");
    }
}

sub parse_part
{
  my ($from, $args) = @_;
  my ($chan, $mesg) = (${$args} =~ /^(\S+) :(.*)$/);

  if (delete_nick($from, $chan))
    {
      add_line($chan, "<-: Exit $from ($mesg)");

      if (is_me($from))
	{
	  delete_channel($chan);
	}
      else
	{
	  list_nicks($chan);
	}
    }
}

sub parse_ping
{
  my $args = shift;

  send_msg("PONG :%s", ${$args});
  add_line('', "Ping? Pong!");
}

sub parse_privmsg
{
  my ($from, $args) = @_;

  if (my ($chan, $mesg) = (${$args} =~ /^(\S+) :(.*)$/))
    {
      my $pref;
      if (is_channel($chan))
	{
	  my $nref = get_nicks($chan);
	  $pref = get_nickprefix($nref->{$from});
	}
      else
	{
	  $chan = '';
	}
      # Handle CTCP messages first
      if (process_ctcp($from, $pref, $chan, \$mesg))
	{
	  add_line($chan, "<$pref$from>: $mesg");
	}
    }
}

sub parse_quit
{
  my ($from, $args) = @_;
  my ($mesg) = (${$args} =~ /^:(.*)$/);

  while (my $chan = each(%{$Current_Server->{'chans'}}))
    {
      if (delete_nick($from, $chan))
	{
	  add_line($chan, "<=: Exit $from ($mesg)");
	  list_nicks($chan);
	}
    }
}

sub parse_topic
{
  my ($from, $args) = @_;

  if (my ($chan, $topic) = (${$args} =~ /^(\S+) :(.*)$/))
    {
      add_line($chan, "*: $from sets new topic: $topic");
      VIM::DoCommand("call s:SetChannelTopic(\"$chan\", \"$topic\")");
    }
}

sub parse_wallops
{
  my ($from, $args) = @_;

  if (my ($mesg) = (${$args} =~ /^:(.*)$/))
    {
      add_line('', "!$from!: $mesg");
    }
}

sub parse_line
{
  my $line = shift;

  if (my ($from, $comd, $args) = (${$line} =~ /^:(\S+) (\S+) (.*)$/))
    {
      ($from, $From_Server) = ($from =~ /^([^!]+)(?:!(\S+))?$/);

      if ($comd + 0)
	{
	  parse_number($from, $comd, \$args);
	}
      else
	{
	  $comd = lc($comd);

	  if (defined(&{'parse_'.$comd}))
	    {
	      &{'parse_'.$comd}($from, \$args);
	    }
	  else
	    {
	      add_line('', $line);
	    }
	}
    }
  else
    {
      ($comd, $args) = (${$line} =~ /^(\S+) :?(.*)$/);

      $comd = lc($comd);
      if ($comd && defined(&{'parse_'.$comd}))
	{
	  &{'parse_'.$comd}(\$args);
	}
      else
	{
	  add_line('', $line);
	}
    }
}

EOP
endfunction
if s:debug
  call s:PerlIRC()
endif
endif

let &cpoptions = s:save_cpoptions
unlet s:save_cpoptions

" vim:ts=8:sts=2:sw=2:fdm=indent:
