" Vim color scheme
"
" Name:         blackboard.vim
" Maintainer:   Ben Wyrosdick <ben.wyrosdick@gmail.com> 
" Last Change:  1 July 2008
" License:      public domain
" Version:      1.0

set background=dark
hi clear
if exists("syntax_on")
   syntax reset
endif

let g:colors_name = "blackboard"

if has("gui_running")
  highlight Normal guifg=White   guibg=#0B1022
  highlight Cursor guifg=Black   guibg=Yellow
  highlight CursorLine guibg=#191E2F
  highlight LineNr guibg=#323232 guifg=#888888
  highlight Folded guifg=White

  highlight Comment guifg=#AEAEAE
  highlight Constant guifg=#CAFE1E
  highlight Keyword guifg=#FFDE00
  highlight rubyClass guifg=#FFDE00
  highlight rubyDefine guifg=#FFDE00
  highlight String guifg=#00D42D
  highlight rubyStringDelimiter guifg=#00D42D
  highlight railsMethod guifg=#A4A7D1
  highlight rubyInclude guifg=#FFDE00
  highlight rubyAttribute guifg=#FFDE00
  highlight rubyInstanceVariable guifg=White
  highlight rubyString guifg=#00D42D
  highlight Type guifg=#A4A7DD
  highlight rubyConstant guifg=#A4A7DD
  highlight rubySymbol guifg=#CAFE1E gui=NONE
  highlight Identifier guifg=#00D42D gui=NONE
  highlight Function guifg=#FF5600 gui=NONE
  highlight Search guibg=White

  "highlight Define guifg=#FF6600
  "highlight rubyInterpolation guifg=White
  "highlight rubyPseudoVariable guifg=White
  "highlight Include guifg=#FFCC00 gui=NONE
  "highlight Statement guifg=#FF6600 gui=NONE
end
