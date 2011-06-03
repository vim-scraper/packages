" toggle_word.vim
" Date: Sat Dec 30 14:02:35 EET 2006
" Author: Alexandru Ungur <alexandru@rb.no-ip.biz>
" Version: 1.0
" Summary: toggles words, ie. turn "yes" into "no", "false" into "true", etc.
" Requires: Vim compiled with Ruby support
"
" Description:
" Toggles words and preserves case. The words will get converted like this:
"
"   on -> off
"   On -> Off
"   ON -> OFF
" 
" any other case gets converted to lowercase, e.g.:
"
"   tRUe -> false
"
" Inspired from vimscript #1676
" All I really wanted besides what that script did, was the ability to
" preserve the case of words.
"
" Install:
" Copy it to your "$HOME/[._]vim/plugin" folder
"
" Usage:
" Once installed it will give you a ToggleWord() command and will map it by
" default to ",t". All you need to do is position the cursor over the word
" you want to toggle and press ,t in normal or visual mode.

function! ToggleWord()
ruby <<
class String
  def upcase?() self == self.upcase end
  def downcase?() self == self.downcase end
  def capitalize?() self == self.capitalize end
end

# Use only lowercase words
# Word case will be adjusted automatically
$pairs = [
  ['all', 'none'],
  ['allow', 'deny'],
  ['define', 'undef'],
  ['in', 'out'], 
  ['left', 'right'], 
  ['min', 'max'],
  ['on', 'off'], 
  ['top', 'bottom'], 
  ['true', 'false'], 
  ['up', 'down'], 
  ['visible', 'hidden'], 
  ['yes', 'no'],
  ['vim', 'emacs']
]

def toggle_word(s)
  pair = $pairs.select{|p| p.include?(s.downcase)}.flatten
  return nil if pair.empty?
  antiword = pair[pair.index(s.downcase) ^ 1]
  case
  when s.upcase? then return antiword.upcase
  when s.downcase? then return antiword.downcase
  when s.capitalize? then return antiword.capitalize
  else return antiword
  end
end

tword = toggle_word(VIM::evaluate("expand('<cword>')"))
VIM::command("normal ciw" << tword) unless tword.nil?
.
endfunction

command! ToggleWord :call ToggleWord() <CR>
nmap ,t :call ToggleWord()<CR>
vmap ,t <ESC>:call ToggleWord()<CR>
