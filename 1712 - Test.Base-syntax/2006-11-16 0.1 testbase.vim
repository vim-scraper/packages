" Vim syntax file
" Language:	TestBase
" Maintainer:	Yuichi Tateno ( hotchpotch at gmail@@nospam@@.com )
" Version: 0.1
" ScreenShot: http://f.hatena.ne.jp/images/fotolife/s/secondlife/20061117/20061117163415.png
" Description: testbase.vim is Test::Base spec syntax hilighting.
" Instration:
"   ~/.vim/ftdetect/testbase.vim
"      au BufNewFile,BufRead *_test.rb set filetype=ruby | unlet b:current_syntax | syn include @rubyTestBase syntax/testbase.vim | syn region rubyTestBase matchgroup=rubyData start="^__END__$" keepend end="\%$" contains=@rubyTestBase
"      au BufNewFile,BufRead *.t set filetype=perl | unlet b:current_syntax | syn include @perlTestBase syntax/testbase.vim | syn region perlTestBase matchgroup=perlData start="^__\(DATA\|END\)__$" keepend end="\%$" contains=@perlTestBase
" ----------------------------------------------------------------------------
"
if !exists("main_syntax")
  if version < 600
    syntax clear
  elseif exists("b:current_syntax")
  finish
endif
  let main_syntax = 'testbase'
endif

syntax region  testbaseBlockStartLine matchgroup=testbaseBlockStartDelim start=+^===+ matchgroup=testbaseBlockStartName end=+$+
syntax region  testbaseBlockLine matchgroup=testbaseBlockSubDelim start=+^---+ end=+$+ contains=@testbase_blockparams oneline

syn cluster testbase_blockparams contains=testbase_blockname,testbase_filter,testbase_data

syn match testbase_data ":.*"hs=s+1 contained
syn match testbase_filter "[^: \t]\+" contained nextgroup=testbase_filter,testbase_data skipwhite skipempty
syn match testbase_blockname "[^: \t]\+" contained nextgroup=testbase_filter,testbase_data skipwhite skipempty

if version >= 508 || !exists("did_testbase_syntax_inits")
  if version < 508
    let did_testbase_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink testbaseBlockStartName String
  HiLink testbaseBlockStartLine testbaseBlockStartName
  HiLink testbaseBlockStartDelim Statement

  HiLink testbaseBlockLine None
  HiLink testbaseBlockSubDelim Statement

  HiLink testbase_blockname Type
  HiLink testbase_filter Function
  HiLink testbase_data String

  delcommand HiLink
endif

let b:current_syntax = "testbase"
if main_syntax == 'testbase'
  unlet main_syntax
endif

