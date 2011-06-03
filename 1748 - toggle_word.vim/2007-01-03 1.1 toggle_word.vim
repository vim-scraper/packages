" toggle_word.vim
" Date: Sat Dec 30 14:02:35 EET 2006
" Author: Alexandru Ungur <alexandru@rb.no-ip.biz>
" Version: 1.1
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
" preserve the case of words, but without specifying multiple pairs as
" in [on off], [On Off], etc.
"
" Install:
" Copy it to your "$HOME/[._]vim/plugin" folder
"
" Usage:
" Once installed it will give you a ToggleWord() command and will map it by
" default to ",t". All you need to do is position the cursor over the word
" you want to toggle and press ,t in normal or visual mode.
"
" Changelog:
" v1.1 Sat Dec 30 17:59:50 EET 2006
" Simplified the dictionary format. Now you don't have to worry about quotes,
" commas and square brackets anymore, it's all "just text".

function! ToggleWord()
ruby <<
class String
  def upcase?() self == self.upcase end
  def downcase?() self == self.downcase end
  def capitalize?() self == self.capitalize end
end

require 'enumerator'
# Use only lowercase words, word case will be detected automatically.
# You can optionally put more pairs on the same line to group them logically,
# but they will still be treated like separate pairs.
$pairs = *%w[
  on off yes no true false 
  online offline
  in out left right top bottom up down
  all none 
  allow deny accept reject
  min max hi lo high low
  hidden visible show hide 
  open close
  vim emacs
].enum_slice(2)

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
