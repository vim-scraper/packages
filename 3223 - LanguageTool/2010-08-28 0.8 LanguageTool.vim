" LanguageTool: Grammar checker for Vim using LanguageTool.
" Maintainer:   Dominique Pellé <dominique.pelle@gmail.com>
" Screenshots:  http://dominique.pelle.free.fr/pic/LanguageToolVimPlugin_en.png
"               http://dominique.pelle.free.fr/pic/LanguageToolVimPlugin_fr.png
" Last Change:  2010/08/28
" Version:      0.8
" 
" Long Description:
"
" This plugin integrates the LanguageTool grammar checker into Vim.
" Current version of LanguageTool can check grammar in many languages: 
" en, de, pl, fr, es, it, nl, lt, uk, ru, sk, sl, sv, ro, is, gl, ca, da,
" ml, be. See also http://www.languagetool.org/ for more information
" about LanguageTool.
"
" The script defines 2 commands:
"
" * Use  :LanguageToolCheck  to check grammar in current buffer.
"   This will check for grammatical mistakes in text of current buffer 
"   and highlight the errors.  It also open a new scratch window with the
"   list of grammatical errors with further explanations for each error.
"
" * Use  :LanguageToolClear  to remove highlighting of grammatical errors
"   and close the scratch window containing the list of errors.
"
" See screenshots of grammar checking in English and French at:
"   http://dominique.pelle.free.fr/pic/LanguageToolVimPlugin_en.png
"   http://dominique.pelle.free.fr/pic/LanguageToolVimPlugin_fr.png
"
" You can customize this plugin by setting the following variables in 
" your ~/.vimrc (plugin already sets default values).
"
"   g:languagetool_jar  
"       This variable specifies the location of the LanguageTool java
"       grammar checker program.
"       Default is: $HOME/JLanguageTool/dist/LanguageTool.jar
"
"   g:languagetool_disable_rules
"       This variable specifies checker rules which are disabled.
"       Each disabled rule must be comma separated
"       Default is: WHITESPACE_RULE,EN_QUOTES
"
"   g:languagetool_win_height
"       This variable specifies the height of the scratch window which 
"       contains all grammatical mistakes with some explanations. You 
"       can use a negative value to disable opening the scratch window.
"       Default is: 14
"
" Language is selected automatically from the Vim 'spelllang' option.
" Character encoding is selected automatically from the Vim 'fenc' option.
"
" Install Details:
"
" Copy this plugin script LanguageTool.vim in $HOME/.vim/plugin/.
"
" You also need to install the Java LanguageTool program in order to use
" this plugin. LanguageTool can be downloaded and built as follows:
"
" $ cvs -z3 \
" -d:pserver:anonymous@languagetool.cvs.sourceforge.net:/cvsroot/languagetool \
" co -P JLanguageTool
" $ cd JLanguageTool
" $ ant
"
" This should build JLanguageTool/dist/LanguageTool.jar.
"
" You then need to set up g:languagetool_jar in your ~/.vimrc with
" the location of the LanguageTool.jar file.
"
" License: The VIM LICENSE applies to LanguageTool.vim plugin
" (see ":help copyright" except use "LanguageTool.vim" instead of "Vim").
"
if &cp || exists("g:loaded_languagetool")
 finish
endif
let g:loaded_languagetool = "1"

" Set up configuration.
function s:LanguageToolSetUp()
  let s:languagetool_jar = exists("g:languagetool_jar")
  \ ? g:languagetool_jar
  \ : $HOME . '/JLanguageTool/dist/LanguageTool.jar'
  let s:languagetool_disable_rules = exists("g:languagetool_disable_rules")
  \ ? g:languagetool_jar
  \ : 'WHITESPACE_RULE,EN_QUOTES'
  let s:languagetool_win_height = exists("g:languagetool_win_height")
  \ ? g:languagetool_win_height
  \ : 14
  let s:languagetool_encoding = &fenc

  " Only pick the first 2 letters of spelllang, so "en_us" for example
  " is transformed into "en".
  let s:languagetool_lang = (&spelllang)[:1]
  if !filereadable(s:languagetool_jar)
    echomsg "LanguageTool cannot be found at: " . s:languagetool_jar
    echomsg "You need to install LanguageTool and/or set up s:languagetool_jar"
    finish
  endif
endfunction

" This function performs grammatical check of text in the current buffer.
" It highlights grammar mistakes in current buffer and opens a scratch
" window with all errors found.
function s:LanguageToolCheck()
  let l:save_cursor = getpos('.')
  call s:LanguageToolSetUp()
  call s:LanguageToolClear()
  sil %y
  new
  sil put!

  " LanguageTool somehow gives incorrect line/column numbers when 
  " reading from stdin so we need to use a temporary file to get 
  " correct results.
  let l:tmpfilename = tempname()
  silent exe "w!" . l:tmpfilename

  let l:languagetool_cmd = 'java'
  \ . ' -jar '  . s:languagetool_jar 
  \ . ' -l '    . s:languagetool_lang
  \ . ' -c '    . s:languagetool_encoding
  \ . ' -d '    . s:languagetool_disable_rules
  \ . ' --api ' . l:tmpfilename

  exe '%!' . l:languagetool_cmd
  call delete(l:tmpfilename)

  " Loop on all errors in XML output of LanguageTool and
  " collect information about all errors in list l:errors
  let l:errors = []
  while search('^<error ', 'eW') > 0
    let l:l  = getline('.')
    let l:l1 = matchlist(l,  'fromy=\"\(\d\+\)\" '
    \ .                      'fromx=\"\(\d\+\)\" '
    \ .                        'toy=\"\(\d\+\)\" '
    \ .                        'tox=\"\(\d\+\)\" ')
    let l:l2 = matchlist(l, 'ruleId=\"\(\w\+\)\" '
    \ .                        'msg=\"\(.*\)\" '
    \ .               'replacements=\"\(.*\)\" '
    \ .                    'context=\"\(.*\)\" '
    \ .              'contextoffset=\"\(\d\+\)\" '
    \ .                'errorlength=\"\(\d\+\)\"')
    let l:error = l:l1[1:4] + l:l2[1:6]

    " Make line/column number start at 1 rather than 0.
    let l:error[0] += 1  
    let l:error[1] += 1  
    let l:error[2] += 1
    let l:error[3] += 1
    call add(l:errors, l:error)
  endwhile

  if s:languagetool_win_height >= 0
    " Reformat the output of LanguageTool (XML is not human friendly) and
    " set up syntax highlighting in the buffer which shows all errors.
    %d
    call append(0, '# ' . l:languagetool_cmd)
    set bt=nofile
    setlocal nospell
    syn clear
    syn match LanguageToolCmd   '\%1l.*'
    syn match LanguageToolLabel '^\(Pos\|Rule\|Context\|Message\|Correction\):'
    syn match LanguageToolErrorCount '^Error:\s\+\d\+.\d\+'
    let l:i = 0
    for l:error in l:errors
      call append('$', 'Error:      ' 
      \ . (l:i + 1) . '/' . len(l:errors)
      \ . ' ('  . l:error[4] . ')'
      \ . ' @ ' . l:error[0] . 'L '
      \ .         l:error[1] . 'C ')
      call append('$', 'Message:    ' . l:error[5])
      call append('$', 'Context:    ' . l:error[7])

      exe "syn match LanguageToolError '"
      \ . '\%'  . line('$') . 'l\%9c'
      \ . '.\{' . (4 + l:error[8]) . '}\zs'
      \ . '.\{' . (l:error[9]) . "}'"
      call append('$', 'Correction: ' . l:error[6])
      call append('$', '')
      let l:i += 1
    endfor
    exe "norm z" . s:languagetool_win_height . "\<CR>"
    0
    let g:ErrorBuffer = bufnr('%')
    exe "norm \<C-W>\<C-P>"
  else
    " Negative s:languagetool_win_height -> no scratch window.
    bd!
    unlet! g:ErrorBuffer 
  endif

  " Also highlight errors in original buffer.
  for l:error in l:errors
    let l:re = l:error[7][byteidx(l:error[7], l:error[8])
    \                    :byteidx(l:error[7], l:error[8] + l:error[9] - 1)]
    " This substitute allows matching when error spans multiple lines.
    let l:re = '\%' . l:error[0] . 'l' . substitute(l:re, ' ', '\\_\\s', 'g')
    exe "syn match LanguageToolError '" . l:re . "'"
  endfor
endfunction

" This function clears syntax highlighting created by LanguageTool plugin
" and removes the scratch window containing grammatical errors.
function s:LanguageToolClear()
  syn clear LanguageToolError
  if exists('g:ErrorBuffer') && bufexists(g:ErrorBuffer)
    exe "bd " . g:ErrorBuffer
    unlet g:ErrorBuffer
  endif
endfunction

hi def link LanguageToolCmd          Comment
hi def link LanguageToolLabel        Label
hi def link LanguageToolError        Error
hi def link LanguageToolErrorCount   Title

com! -nargs=0 LanguageToolCheck :call s:LanguageToolCheck()
com! -nargs=0 LanguageToolClear :call s:LanguageToolClear()
