" Vim language File
" Language:      mcabber history file
" Maintainger:   Thomas Ba
" Version:       0.5a1
" Last Change:   2011-09-05
" URL:           http://www.vim.org/scripts/script.php?script_id=3726
"
" Version 0.5
"   • Handle Status changes
" Version 0.4
"   • Fixed: show own kicks/bans correct
"   • Fixed: Handling for kicks/bans + reason
" Version 0.3:
"   • Fix for non MUC

if !exists("main_syntax")
  if version < 600 
    syntax clear
  elseif exists("b:current_syntax")
    finish
  endif
  let main_syntax = 'mcabberlog'
endif

" Highlight this Words (in Messages):
" Uncomment to enable!
" syn keyword mcabberDongle thomasba

" Message"header"
syn match mcabberStatusChange "^S[OFDNAI_] \d\{8}T\d\d:\d\d:\d\dZ \d\d\d .*$" contains=mcabberDate,mcabberTime
syn match mcabberHeader "^MI \d\{8}T\d\d:\d\d:\d\dZ \d\d\d" nextgroup=@mcabberMsgs contains=mcabberDate,mcabberTime skipwhite
syn match mcabberHeader "^MR \d\{8}T\d\d:\d\d:\d\dZ \d\d\d" nextgroup=@mcabberNicks contains=mcabberDate,mcabberTime skipwhite
syn match mcabberHeaderSend "^MS \d\{8}T\d\d:\d\d:\d\dZ \d\d\d" nextgroup=mcabberNicks contains=mcabberDate,mcabberTime skipwhite

" Time and Date
syn match mcabberDate "\d\{8\}" skipwhite contained
syn match mcabberTime "\d\d:\d\d:\d\d" skipwhite contained

" Servermessages, kicks, joins...
syn region mcabberMsgRegion start="[^<]" end="$" contained contains=@mcabberMsgs
syn match mcabberMsg     "\(\~ \|The topic\|You have\|.* has set the topic to\).*" contained
syn match mcabberMsgNick ".* \(has joined\|has left\|has left: .*\|is now known as .*\|ha\(s\|ve\) been .*\(\nReason: .*\)\?\)$" contained
syn match mcabberMsgTilde "\~ .*$" contained skipwhite

" Message nicks
syn region mcabberNickRegion start="<" end=">" contained contains=@mcabberNicks
syn match mcabberNick "<[^>]*>\( /me\)\?" contained 

" Cluster
syn cluster mcabberMsgs contains=mcabberMsg,mcabberMsgNick
syn cluster mcabberNicks contains=mcabberNick,mcabberMsgTilde
syn cluster mcabberRegions contains=@mcabberNicks,@mcabberMsgs

" Mail und URL handling
syn match mcabberURL /\\\@<!\<\(http\|https\|ftp\|file\|irc\):\/\/[^| \t]*\(\w\|\/\)/
syn match mcabberEmail /[\\.:]\@<!\(\<\|<\)\w\(\w\|[._-]\)*@\(\w\|[.-]\)*\w>\?[0-9A-Za-z_]\.\(\w\|[.-]\)*\w>\?[0-9A-Za-z_]\@!/


" And link the colors
" Main header section
hi link mcabberHeader Comment
hi link mcabberHeaderSend LineNr
" Time and Date
hi link mcabberTime Number
hi link mcabberDate Type
" The nicknames
hi link mcabberNick Statement
" Mail and URL highlighting
hi link mcabberURL String
hi link mcabberEmail String
" Status messages, leaves, nick change
hi link mcabberMsg Comment
hi link mcabberMsgNick Comment
hi link mcabberStatus Comment
hi link mcabberStatusChange Comment
" Keywords to highlight (set above)
hi link mcabberDongle Todo
" MR ... 000 ~ ...
hi link mcabberMsgTilde Comment


let b:current_syntax = "mcabberlog"

if main_syntax == 'mcabberlog'
	unlet main_syntax
endif
