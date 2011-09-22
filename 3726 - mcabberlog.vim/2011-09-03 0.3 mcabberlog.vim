" Vim language File
" Language:      mcabber history file
" Maintainger:   Thomas Ba
" Version:       0.3
" Last Change:   2011-09-03
" URL:           http://thomasba.de/page/blog/21/vim_colors_for_mcabber_logfiles
"
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
" syn keyword mcabberDongle thomasba

" Nachrichten"header"
syn match mcabberHeader "^\(MR\|MI\) [12][90][0-9]\{2\}[01][0-9][0-3][0-9]T[0-2][0-9]:[0-5][0-9]:[0-5][0-9]Z [0-9]\{3\}" nextgroup=@mcabberRegions contains=mcabberDate,mcabberTime skipwhite
syn match mcabberDate "[12][90][0-9]\{2\}[01][0-9][0-3][0-9]" skipwhite contained
syn match mcabberTime "[0-2][0-9]:[0-5][0-9]:[0-5][0-9]" skipwhite contained

" For normal chats:
syn match mcabberHeaderSend "^MS [12][90][0-9]\{2\}[01][0-9][0-3][0-9]T[0-2][0-9]:[0-5][0-9]:[0-5][0-9]Z [0-9]\{3\}" nextgroup=@mcabberRegions contains=mcabberDate,mcabberTime skipwhite

" Servermessages, kicks, joins...
syn region mcabberMsgRegion start="[^< ]" end="$" contained contains=@mcabberMsg
syn match mcabberMsg     "\(\~ \|The topic\|You have\|.* has set the topic to\).*" contained
syn match mcabberMsgNick ".* \(has joined\|has left\|is now known as .*\)$" contained

" Message nicks
syn region mcabberNickRegion start="<" end=">" contained contains=@mcabberNicks
syn match mcabberNick "<[^>]*>\( /me\)\?" contained skipwhite

" Cluster
syn cluster mcabberMsgs contains=mcabberMsg,mcabberMsgNick
syn cluster mcabberNicks contains=mcabberNick
syn cluster mcabberRegions contains=@mcabberNicks,@mcabberMsgs

" Mail und URL handling
syn match mcabberURL /\\\@<!\<\(http\|https\|ftp\|file\|irc\):\/\/[^| \t]*\(\w\|\/\)/
syn match mcabberEmail /[\\.:]\@<!\(\<\|<\)\w\(\w\|[.-]\)*@\(\w\|[.-]\)*\w>\?[0-9A-Za-z_]\.\(\w\|[.-]\)*\w>\?[0-9A-Za-z_]\@!/

" And link the colors
hi link mcabberHeader Comment
hi link mcabberHeaderSend LineNr
hi link mcabberTime Number
hi link mcabberDate Type
hi link mcabberNick Statement
hi link mcabberURL String
hi link mcabberEmail String
hi link mcabberMsg Comment
hi link mcabberMsgNick Comment
hi link mcabberDongle Todo

let b:current_syntax = "mcabberlog"

if main_syntax == 'mcabberlog'
	unlet main_syntax
endif
