" toggle_word.vim
" Date: Sat Dec 30 14:02:35 EET 2006
" Author: Alexandru Ungur <alexandru@globalterrasoft.ro>
" Version: 1.2
" Summary: toggles words, ie. turn "yes" into "no", "false" into "true", etc.
" Requires: Vim compiled with Ruby support
"
" Description:
" Toggles words and preserves case, e.g. on -> off; On -> Off; ON -> OFF
"
" Inspired from vimscript #1676 ; all I really wanted besides what that script 
" did, was the ability to preserve the case of words, but without specifying 
" multiple pairs as in [on off], [On Off], etc.
"
" Suggestions and comments are highly welcome :-)
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
"
" v1.2 Fri Jan  5 17:27:01 EET 2007
" Refactored the code a little bit.

function! ToggleWord()
ruby << RUBY_DONE
require 'enumerator'
class String
  @@pairs = *%w[
    on off yes no true false
    online offline
    in out left right top bottom up down
    all none
    allow deny accept reject
    min max hi lo high low
    hidden visible show hide
    open close
    + - < > 1 0
    vim emacs
  ].enum_slice(2)

  def toggle_word
    pair = @@pairs.select{|p| p.include?(self.downcase)}.flatten
    wordcase = %w[upcase downcase capitalize].detect {|c| self.send(c) == self} || "downcase"
    pair.empty? ? nil : pair[pair.index(self.downcase) ^ 1].send(wordcase)
  end
end

tword = VIM::evaluate("expand('<cword>')").toggle_word
VIM::command("normal ciw#{tword}") unless tword.nil?
RUBY_DONE
endfunction

command! ToggleWord :call ToggleWord() <CR>
nmap ,t :call ToggleWord()<CR>
vmap ,t <ESC>:call ToggleWord()<CR>
