" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
autoload/unite/filters/converter_default.vim	[[[1
63
"=============================================================================
" FILE: converter_default.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 22 Apr 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#filters#converter_default#define()"{{{
  return s:converter
endfunction"}}}

let s:converter = {
      \ 'name' : 'converter_default',
      \ 'description' : 'default converter',
      \}

function! s:converter.filter(candidates, context)"{{{
  let l:candidates = a:candidates
  for l:default in s:default_converters
    let l:filter = unite#get_filters(l:default)
    if !empty(l:filter)
      let l:candidates = l:filter.filter(l:candidates, a:context)
    endif
  endfor

  return l:candidates
endfunction"}}}


let s:default_converters = ['converter_nothing']
function! unite#filters#converter_default#get()"{{{
  return s:default_converters
endfunction"}}}
function! unite#filters#converter_default#use(converters)"{{{
  let s:default_converters = a:converters
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/filters/converter_nothing.vim	[[[1
47
"=============================================================================
" FILE: converter_nothing.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 22 Apr 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#filters#converter_nothing#define()"{{{
  return s:converter
endfunction"}}}

let s:converter = {
      \ 'name' : 'converter_nothing',
      \ 'description' : 'nothing converter',
      \}

function! s:converter.filter(candidates, context)"{{{
  " Nothing.
  return a:candidates
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/filters/converter_relative_abbr.vim	[[[1
68
"=============================================================================
" FILE: converter_relative_abbr.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 02 Aug 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#filters#converter_relative_abbr#define()"{{{
  return s:converter
endfunction"}}}

let s:converter = {
      \ 'name' : 'converter_relative_abbr',
      \ 'description' : 'relative path abbr converter',
      \}

function! s:converter.filter(candidates, context)"{{{
  try
    let l:directory = unite#util#substitute_path_separator(getcwd())
    if has_key(a:context, 'source__directory')
      let l:old_dir = l:directory
      let l:directory = substitute(a:context.source__directory, '*', '', 'g')

      if l:directory !=# l:old_dir
        lcd `=l:directory`
      endif
    endif

    for candidate in a:candidates
      let candidate.abbr = unite#util#substitute_path_separator(
            \ fnamemodify(candidate.word, ':~:.'))
    endfor
  finally
    if has_key(a:context, 'source__directory')
          \ && l:directory !=# l:old_dir
      lcd `=l:old_dir`
    endif
  endtry

  return a:candidates
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/filters/converter_relative_word.vim	[[[1
68
"=============================================================================
" FILE: converter_relative_word.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 02 Aug 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#filters#converter_relative_word#define()"{{{
  return s:converter
endfunction"}}}

let s:converter = {
      \ 'name' : 'converter_relative_word',
      \ 'description' : 'relative path word converter',
      \}

function! s:converter.filter(candidates, context)"{{{
  try
    let l:directory = unite#util#substitute_path_separator(getcwd())
    if has_key(a:context, 'source__directory')
      let l:old_dir = l:directory
      let l:directory = substitute(a:context.source__directory, '*', '', 'g')

      if l:directory !=# l:old_dir
        lcd `=l:directory`
      endif
    endif

    for candidate in a:candidates
      let candidate.word = unite#util#substitute_path_separator(
            \ fnamemodify(candidate.word, ':.'))
    endfor
  finally
    if has_key(a:context, 'source__directory')
          \ && l:directory !=# l:old_dir
      lcd `=l:old_dir`
    endif
  endtry

  return a:candidates
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/filters/default.vim	[[[1
46
"=============================================================================
" FILE: default.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 22 Apr 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#filters#default#define()"{{{
  " Dummy.
  return []
endfunction"}}}

let s:default = ['matcher_default', 'sorter_default', 'converter_default']
function! unite#filters#default#get()"{{{
  return s:default
endfunction"}}}
function! unite#filters#default#use(filters)"{{{
  let s:default = a:filters
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/filters/matcher_default.vim	[[[1
63
"=============================================================================
" FILE: matcher_default.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 22 Apr 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#filters#matcher_default#define()"{{{
  return s:matcher
endfunction"}}}

let s:matcher = {
      \ 'name' : 'matcher_default',
      \ 'description' : 'default matcher',
      \}

function! s:matcher.filter(candidates, context)"{{{
  let l:candidates = a:candidates
  for l:default in s:default_matchers
    let l:filter = unite#get_filters(l:default)
    if !empty(l:filter)
      let l:candidates = l:filter.filter(l:candidates, a:context)
    endif
  endfor

  return l:candidates
endfunction"}}}


let s:default_matchers = ['matcher_glob']
function! unite#filters#matcher_default#get()"{{{
  return s:default_matchers
endfunction"}}}
function! unite#filters#matcher_default#use(matchers)"{{{
  let s:default_matchers = a:matchers
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/filters/matcher_glob.vim	[[[1
76
"=============================================================================
" FILE: matcher_glob.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 08 Aug 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#filters#matcher_glob#define()"{{{
  return s:matcher
endfunction"}}}

let s:matcher = {
      \ 'name' : 'matcher_glob',
      \ 'description' : 'glob matcher',
      \}

function! s:matcher.filter(candidates, context)"{{{
  if a:context.input == ''
    return a:candidates
  endif

  let l:candidates = a:candidates
  for l:input in split(a:context.input, '\\\@<! ')
    let l:input = substitute(l:input, '\\ ', ' ', 'g')

    if l:input =~ '^!'
      if l:input == '!'
        continue
      endif

      " Exclusion.
      let l:input = unite#escape_match(l:input)
      let l:expr = 'v:val.word !~ ' . string(l:input[1:])
    elseif l:input =~ '\\\@<!\*'
      " Wildcard.
      let l:input = unite#escape_match(l:input)
      let l:expr = 'v:val.word =~ ' . string(l:input)
    else
      let l:input = substitute(l:input, '\\\(.\)', '\1', 'g')
      let l:expr = &ignorecase ?
            \ printf('stridx(tolower(v:val.word), %s) != -1', string(tolower(l:input))) :
            \ printf('stridx(v:val.word, %s) != -1', string(l:input))
    endif

    let l:candidates = filter(copy(l:candidates), l:expr)
  endfor

  return l:candidates
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/filters/matcher_regexp.vim	[[[1
79
"=============================================================================
" FILE: matcher_regexp.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 08 Aug 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#filters#matcher_regexp#define()"{{{
  return s:matcher
endfunction"}}}

let s:matcher = {
      \ 'name' : 'matcher_regexp',
      \ 'description' : 'regular expression matcher',
      \}

function! s:matcher.filter(candidates, context)"{{{
  if a:context.input == ''
    return a:candidates
  endif

  let l:candidates = a:candidates
  for l:input in split(a:context.input, '\\\@<! ')
    if l:input =~ '^!'
      if l:input == '!'
        continue
      endif
      " Exclusion match.
      try
        let l:candidates = filter(copy(l:candidates),
              \ 'v:val.word !~ ' . string(l:input[1:]))
      catch
      endtry
    elseif l:input !~ '[~\\.^$[\]*]'
      " Optimized filter.
      let l:input = substitute(l:input, '\\\(.\)', '\1', 'g')
      let l:expr = &ignorecase ?
            \ printf('stridx(tolower(v:val.word), %s) != -1', string(tolower(l:input))) :
            \ printf('stridx(v:val.word, %s) != -1', string(l:input))

      let l:candidates = filter(copy(l:candidates), l:expr)
    else
      try
        let l:candidates = filter(copy(l:candidates),
              \ 'v:val.word =~ ' . string(l:input))
      catch
      endtry
    endif
  endfor

  return l:candidates
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/filters/sorter_default.vim	[[[1
63
"=============================================================================
" FILE: sorter_default.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 22 Apr 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#filters#sorter_default#define()"{{{
  return s:sorter
endfunction"}}}

let s:sorter = {
      \ 'name' : 'sorter_default',
      \ 'description' : 'default sorter',
      \}

function! s:sorter.filter(candidates, context)"{{{
  let l:candidates = a:candidates
  for l:default in s:default_sorters
    let l:filter = unite#get_filters(l:default)
    if !empty(l:filter)
      let l:candidates = l:filter.filter(l:candidates, a:context)
    endif
  endfor

  return l:candidates
endfunction"}}}


let s:default_sorters = ['sorter_nothing']
function! unite#filters#sorter_default#get()"{{{
  return s:default_sorters
endfunction"}}}
function! unite#filters#sorter_default#use(sorters)"{{{
  let s:default_sorters = a:sorters
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/filters/sorter_nothing.vim	[[[1
47
"=============================================================================
" FILE: sorter_nothing.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 22 Apr 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#filters#sorter_nothing#define()"{{{
  return s:sorter
endfunction"}}}

let s:sorter = {
      \ 'name' : 'sorter_nothing',
      \ 'description' : 'nothing sorter',
      \}

function! s:sorter.filter(candidates, context)"{{{
  " Nothing.
  return a:candidates
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/filters/sorter_word.vim	[[[1
46
"=============================================================================
" FILE: sorter_word.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 22 Apr 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#filters#sorter_word#define()"{{{
  return s:sorter
endfunction"}}}

let s:sorter = {
      \ 'name' : 'sorter_word',
      \ 'description' : 'sort by word order',
      \}

function! s:sorter.filter(candidates, context)"{{{
  return unite#util#sort_by(a:candidates, 'v:val.word')
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/kinds/buffer.vim	[[[1
183
"=============================================================================
" FILE: buffer.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 31 Jul 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#kinds#buffer#define()"{{{
  return s:kind
endfunction"}}}

let s:kind = {
      \ 'name' : 'buffer',
      \ 'default_action' : 'open',
      \ 'action_table': {},
      \ 'parents': ['file'],
      \}

" Actions"{{{
let s:kind.action_table.open = {
      \ 'description' : 'open buffer',
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.open.func(candidates)"{{{
  for l:candidate in a:candidates
    execute 'buffer' l:candidate.action__buffer_nr
  endfor
endfunction"}}}

let s:kind.action_table.delete = {
      \ 'description' : 'delete from buffer list',
      \ 'is_invalidate_cache' : 1,
      \ 'is_quit' : 0,
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.delete.func(candidates)"{{{
  for l:candidate in a:candidates
    call s:delete('bdelete', l:candidate)
  endfor
endfunction"}}}

let s:kind.action_table.fdelete = {
      \ 'description' : 'force delete from buffer list',
      \ 'is_invalidate_cache' : 1,
      \ 'is_quit' : 0,
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.fdelete.func(candidates)"{{{
  for l:candidate in a:candidates
    call s:delete('bdelete!', l:candidate)
  endfor
endfunction"}}}

let s:kind.action_table.wipeout = {
      \ 'description' : 'wipeout from buffer list',
      \ 'is_invalidate_cache' : 1,
      \ 'is_quit' : 0,
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.wipeout.func(candidates)"{{{
  for l:candidate in a:candidates
    call s:delete('bwipeout', l:candidate)
  endfor
endfunction"}}}

let s:kind.action_table.unload = {
      \ 'description' : 'unload from buffer list',
      \ 'is_invalidate_cache' : 1,
      \ 'is_quit' : 0,
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.unload.func(candidates)"{{{
  for l:candidate in a:candidates
    call s:delete('unload', l:candidate)
  endfor
endfunction"}}}

let s:kind.action_table.preview = {
      \ 'description' : 'preview buffer',
      \ 'is_quit' : 0,
      \ }
function! s:kind.action_table.preview.func(candidate)"{{{
  pedit `=a:candidate.action__path`

  let l:filetype = getbufvar(a:candidate.action__buffer_nr, '&filetype')
  if l:filetype != ''
    let l:winnr = winnr()
    execute bufwinnr(a:candidate.action__buffer_nr) . 'wincmd w'
    execute 'setfiletype' l:filetype
    execute l:winnr . 'wincmd w'
  endif
endfunction"}}}

let s:kind.action_table.rename = {
      \ 'description' : 'rename buffers',
      \ 'is_invalidate_cache' : 1,
      \ 'is_quit' : 0,
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.rename.func(candidates)"{{{
  for l:candidate in a:candidates
    let l:old_buffer_name = bufname(l:candidate.action__buffer_nr)
    let l:buffer_name = input(printf('New buffer name: %s -> ', l:old_buffer_name), l:old_buffer_name)
    if l:buffer_name != '' && l:buffer_name !=# l:old_buffer_name
      let l:bufnr = bufnr('%')
      execute 'buffer' l:candidate.action__buffer_nr
      saveas! `=l:buffer_name`
      call delete(l:candidate.action__path)
      execute 'buffer' l:bufnr
    endif
  endfor
endfunction"}}}
"}}}

" Misc
function! s:delete(delete_command, candidate)"{{{
  " Not to close window, move to alternate buffer.
  let l:winnr = 1
  while l:winnr <= winnr('$')
    if winbufnr(l:winnr) == a:candidate.action__buffer_nr
      execute l:winnr . 'wincmd w'
      call s:alternate_buffer()
      wincmd p
    endif

    let l:winnr += 1
  endwhile

  execute a:candidate.action__buffer_nr a:delete_command
endfunction"}}}
function! s:alternate_buffer()"{{{
  if bufnr('%') != bufnr('#') && buflisted(bufnr('#'))
    buffer #
  else
    let l:cnt = 0
    let l:pos = 1
    let l:current = 0
    while l:pos <= bufnr('$')
      if buflisted(l:pos)
        if l:pos == bufnr('%')
          let l:current = l:cnt
        endif

        let l:cnt += 1
      endif

      let l:pos += 1
    endwhile

    if l:current > l:cnt / 2
      bprevious
    else
      bnext
    endif
  endif
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/kinds/cdable.vim	[[[1
144
"=============================================================================
" FILE: cdable.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 07 Jun 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#kinds#cdable#define()"{{{
  return s:kind
endfunction"}}}

let s:kind = {
      \ 'name' : 'cdable',
      \ 'action_table' : {},
      \ 'alias_table' : { 'edit' : 'narrow' },
      \}

" Actions"{{{
let s:kind.action_table.cd = {
      \ 'description' : 'change current directory',
      \ }
function! s:kind.action_table.cd.func(candidate)"{{{
  if &filetype ==# 'vimfiler'
    call vimfiler#internal_commands#cd(a:candidate.action__directory)
  elseif &filetype ==# 'vimshell'
    call vimshell#switch_shell(0, a:candidate.action__directory)
  endif

  if a:candidate.action__directory != ''
    execute g:unite_cd_command '`=a:candidate.action__directory`'
  endif
endfunction"}}}

let s:kind.action_table.lcd = {
      \ 'description' : 'change window local current directory',
      \ }
function! s:kind.action_table.lcd.func(candidate)"{{{
  if &filetype ==# 'vimfiler'
    call vimfiler#internal_commands#cd(a:candidate.action__directory)
  elseif &filetype ==# 'vimshell'
    call vimshell#switch_shell(0, a:candidate.action__directory)
  endif

  if a:candidate.action__directory != ''
    execute g:unite_cd_command '`=a:candidate.action__directory`'
  endif
endfunction"}}}

let s:kind.action_table.project_cd = {
      \ 'description' : 'change current directory to project directory',
      \ }
function! s:kind.action_table.project_cd.func(candidate)"{{{
  if a:candidate.action__directory == ''
    " Ignore.
    return
  endif

  let l:directory = unite#util#path2project_directory(a:candidate.action__directory)

  if isdirectory(l:directory)
    let l:candidate = copy(a:candidate)
    let l:candidate.action__directory = l:directory
    call s:kind.action_table.cd.func(l:candidate)
  endif
endfunction"}}}

let s:kind.action_table.narrow = {
      \ 'description' : 'narrowing candidates by directory name',
      \ 'is_quit' : 0,
      \ }
function! s:kind.action_table.narrow.func(candidate)"{{{
  if a:candidate.word =~ '^\.\.\?/'
    let l:word = a:candidate.word
  else
    let l:word = a:candidate.action__directory
  endif

  if l:word !~ '[\\/]$'
    let l:word .= '/'
  endif

  call unite#mappings#narrowing(l:word)
endfunction"}}}

if exists(':VimShell')
  let s:kind.action_table.vimshell = {
        \ 'description' : 'open vimshell buffer here',
        \ }
  function! s:kind.action_table.vimshell.func(candidate)"{{{
    VimShellCreate `=a:candidate.action__directory`
  endfunction"}}}
endif
if exists(':VimShellTab')
  let s:kind.action_table.tabvimshell = {
        \ 'description' : 'tabopen vimshell buffer here',
        \ }
  function! s:kind.action_table.tabvimshell.func(candidate)"{{{
    VimShellTab `=a:candidate.action__directory`
  endfunction"}}}
endif
if exists(':VimFiler')
  let s:kind.action_table.vimfiler = {
        \ 'description' : 'open vimfiler buffer here',
        \ }
  function! s:kind.action_table.vimfiler.func(candidate)"{{{
    VimFilerCreate `=a:candidate.action__directory`
  endfunction"}}}
endif
if exists(':VimFilerTab')
  let s:kind.action_table.tabvimfiler = {
        \ 'description' : 'tabopen vimfiler buffer here',
        \ }
  function! s:kind.action_table.tabvimfiler.func(candidate)"{{{
    VimFilerTab `=a:candidate.action__directory`
  endfunction"}}}
endif
"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/kinds/command.vim	[[[1
66
"=============================================================================
" FILE: command.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 25 Jun 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#kinds#command#define()"{{{
  return s:kind
endfunction"}}}

let s:kind = {
      \ 'name' : 'command',
      \ 'default_action' : 'execute',
      \ 'action_table': {},
      \ 'alias_table' : { 'ex' : 'nop' },
      \}

" Actions"{{{
let s:kind.action_table.execute = {
      \ 'description' : 'execute command',
      \ }
function! s:kind.action_table.execute.func(candidate)"{{{
  " Add history.
  let l:type = has_key(a:candidate, 'action__type') ? a:candidate.action__type : ':'
  call histadd(l:type, a:candidate.action__command)
  if l:type ==# '/'
    let @/ = a:candidate.action__command
  endif

  execute l:type.a:candidate.action__command
endfunction"}}}
let s:kind.action_table.edit = {
      \ 'description' : 'edit command',
      \ }
function! s:kind.action_table.edit.func(candidate)"{{{
  call feedkeys(':' . a:candidate.action__command, 'n')
endfunction"}}}
"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/kinds/common.vim	[[[1
90
"=============================================================================
" FILE: common.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 22 Apr 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#kinds#common#define()"{{{
  return s:kind
endfunction"}}}

let s:kind = {
      \ 'name' : 'common',
      \ 'default_action' : 'nop',
      \ 'action_table': {},
      \ 'parents': [],
      \}

" Actions"{{{
let s:kind.action_table.nop = {
      \ 'description' : 'no operation',
      \ }
function! s:kind.action_table.nop.func(candidate)"{{{
endfunction"}}}

let s:kind.action_table.yank = {
      \ 'description' : 'yank text',
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.yank.func(candidates)"{{{
  let @" = join(map(copy(a:candidates), 'v:val.word'), "\n")
  if has('clipboard')
    let @* = @"
  endif
endfunction"}}}

let s:kind.action_table.yank_escape = {
      \ 'description' : 'yank escaped text',
      \ }
function! s:kind.action_table.yank_escape.func(candidate)"{{{
  let @" = escape(a:candidate.word, " *?[{`$\\%#\"|!<>")
endfunction"}}}

let s:kind.action_table.ex = {
      \ 'description' : 'insert candidates into command line',
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.ex.func(candidates)"{{{
  " Result is ':| {candidate}', here '|' means the cursor position.
  call feedkeys(printf(": %s\<C-b>", join(map(map(copy(a:candidates), 'v:val.word'), 'escape(v:val, " *?[{`$\\%#\"|!<>")'))), 'n')
endfunction"}}}

let s:kind.action_table.insert = {
      \ 'description' : 'insert word',
      \ }
function! s:kind.action_table.insert.func(candidate)"{{{
  " Paste.
  let l:old_reg = @"
  let @" = a:candidate.word
  normal! ""p
  let @" = l:old_reg
endfunction"}}}
"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/kinds/completion.vim	[[[1
90
"=============================================================================
" FILE: completion.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 10 Jul 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#kinds#completion#define()"{{{
  return s:kind
endfunction"}}}

let s:kind = {
      \ 'name' : 'completion',
      \ 'default_action' : 'insert',
      \ 'action_table': {},
      \}

" Actions"{{{
let s:kind.action_table.insert = {
      \ 'description' : 'insert word',
      \ }
function! s:kind.action_table.insert.func(candidate)"{{{
  let l:col = a:candidate.action__complete_pos
  let l:cur_text = matchstr(getline('.'), '^.*\%' . l:col . 'c.')
  let l:word = a:candidate.action__complete_word

  " Insert word.
  let l:context_col = unite#get_current_unite().context.col
  let l:next_line = l:context_col < col('$') ?
        \ getline('.')[l:context_col-1 :] : ''
  let l:next_line = getline('.')[l:context_col :]
  call setline(line('.'), split(l:cur_text . l:word . l:next_line, '\n\|\r\n'))
  let l:pos = getpos('.')
  let l:pos[2] = len(l:cur_text)+len(l:word)+1
  call setpos('.', l:pos)
  let l:next_col = len(l:cur_text)+len(l:word)+1

  if l:next_col < col('$')
    startinsert
  else
    startinsert!
  endif
endfunction"}}}

let s:kind.action_table.preview = {
      \ 'description' : 'preview word in echo area',
      \ 'is_quit' : 0,
      \ }
function! s:kind.action_table.preview.func(candidate)"{{{
  echo ''
  redraw

  let complete_info = has_key(a:candidate, 'action__complete_info') ?
        \ a:candidate.action__complete_info :
        \ has_key(a:candidate, 'action__complete_info_lazy') ?
        \ a:candidate.action__complete_info_lazy() :
        \ ''
  if complete_info != ''
    let S = vital#of('unite').import('Data.String')
    echo join(S.wrap(complete_info)[: &cmdheight-1], "\n")
  endif
endfunction"}}}
"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/kinds/directory.vim	[[[1
47
"=============================================================================
" FILE: directory.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 31 Oct 2010
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#kinds#directory#define()"{{{
  return s:kind
endfunction"}}}

let s:kind = {
      \ 'name' : 'directory',
      \ 'default_action' : 'narrow',
      \ 'action_table': {},
      \ 'parents': ['file'],
      \}

" Actions"{{{
"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/kinds/file.vim	[[[1
103
"=============================================================================
" FILE: file.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 10 Aug 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#kinds#file#define()"{{{
  return s:kind
endfunction"}}}

let s:kind = {
      \ 'name' : 'file',
      \ 'default_action' : 'open',
      \ 'action_table' : {},
      \ 'parents' : ['openable', 'cdable', 'uri'],
      \}

" Actions"{{{
let s:kind.action_table.open = {
      \ 'description' : 'open files',
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.open.func(candidates)"{{{
  for l:candidate in a:candidates
    call s:execute_command('edit', l:candidate)
  endfor
endfunction"}}}

let s:kind.action_table.preview = {
      \ 'description' : 'preview file',
      \ 'is_quit' : 0,
      \ }
function! s:kind.action_table.preview.func(candidate)"{{{
  if filereadable(a:candidate.action__path)
    call s:execute_command('pedit', a:candidate)
  endif
endfunction"}}}

let s:kind.action_table.mkdir = {
      \ 'description' : 'make this directory or parents directory',
      \ 'is_quit' : 0,
      \ 'is_invalidate_cache' : 1,
      \ }
function! s:kind.action_table.mkdir.func(candidate)"{{{
  if !filereadable(a:candidate.action__path)
    call mkdir(iconv(a:candidate.action__path, &encoding, &termencoding), 'p')
  endif
endfunction"}}}

let s:kind.action_table.rename = {
      \ 'description' : 'rename files',
      \ 'is_invalidate_cache' : 1,
      \ 'is_quit' : 0,
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.rename.func(candidates)"{{{
  for l:candidate in a:candidates
    let l:filename = input(printf('New buffer name: %s -> ', l:candidate.action__path), l:candidate.action__path)
    if l:filename != '' && l:filename !=# l:candidate.action__path
      call rename(l:candidate.action__path, l:filename)
    endif
  endfor
endfunction"}}}
"}}}

function! s:execute_command(command, candidate)"{{{
  let l:dir = unite#util#path2directory(a:candidate.action__path)
  " Auto make directory.
  if !isdirectory(l:dir) &&
        \ input(printf('"%s" does not exist. Create? [y/N]', l:dir)) =~? '^y\%[es]$'
    call mkdir(iconv(l:dir, &encoding, &termencoding), 'p')
  endif

  silent call unite#util#smart_execute_command(a:command, a:candidate.action__path)
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/kinds/jump_list.vim	[[[1
215
"=============================================================================
" FILE: jump_list.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 31 Jul 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

" Variables  "{{{
if !exists('g:unite_kind_jump_list_after_jump_scroll')
  let g:unite_kind_jump_list_after_jump_scroll = 25
else
  let g:unite_kind_jump_list_after_jump_scroll =
        \ min([max([0, g:unite_kind_jump_list_after_jump_scroll]), 100])
endif
"}}}

function! unite#kinds#jump_list#define()"{{{
  return s:kind
endfunction"}}}

let s:kind = {
      \ 'name' : 'jump_list',
      \ 'default_action' : 'open',
      \ 'action_table': {},
      \ 'parents': ['openable'],
      \}

" Actions"{{{
let s:kind.action_table.open = {
      \ 'description' : 'jump to this position',
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.open.func(candidates)"{{{
  for l:candidate in a:candidates
    if bufnr(unite#util#escape_file_searching(l:candidate.action__path)) != bufnr('%')
      if has_key(l:candidate, 'action__buffer_nr')
        execute 'buffer' l:candidate.action__buffer_nr
      else
        edit `=l:candidate.action__path`
      endif
    endif
    call s:jump(l:candidate, 0)

    " Open folds.
    normal! zv
    call s:adjust_scroll(s:best_winline())
  endfor
endfunction"}}}

let s:kind.action_table.preview = {
      \ 'description' : 'preview this position',
      \ 'is_quit' : 0,
      \ }
function! s:kind.action_table.preview.func(candidate)"{{{
  pedit +call\ s:jump(a:candidate,1) `=a:candidate.action__path`
  if has_key(a:candidate, 'action__buffer_nr')
    let l:filetype = getbufvar(a:candidate.action__buffer_nr, '&filetype')
    if l:filetype != ''
      let l:winnr = winnr()
      execute bufwinnr(a:candidate.action__buffer_nr) . 'wincmd w'
      execute 'setfiletype' l:filetype
      execute l:winnr . 'wincmd w'
    endif
  endif
endfunction"}}}

if globpath(&runtimepath, 'autoload/qfreplace.vim') != ''
  let s:kind.action_table.replace = {
        \ 'description' : 'replace with qfreplace',
        \ 'is_selectable' : 1,
        \ }
  function! s:kind.action_table.replace.func(candidates)"{{{
    let l:qflist = []
    for candidate in a:candidates
      if has_key(candidate, 'action__line')
            \ && has_key(candidate, 'action__text')
        call add(l:qflist, {
              \ 'filename' : candidate.action__path,
              \ 'lnum' : candidate.action__line,
              \ 'text' : candidate.action__text,
              \ })
      endif
    endfor

    if !empty(l:qflist)
      call setqflist(l:qflist)
      call qfreplace#start('')
    endif
  endfunction"}}}
endif
"}}}

" Misc.
function! s:jump(candidate, is_highlight)"{{{
  if !has_key(a:candidate, 'action__line') && !has_key(a:candidate, 'action__pattern')
    " Move to head.
    0
    return
  endif

  if has_key(a:candidate, 'action__line')
        \ && a:candidate.action__line != ''
        \ && a:candidate.action__line !~ '^\d\+$'
    call unite#print_error('unite: jump_list: Invalid action__line format.')
    return
  endif

  if !has_key(a:candidate, 'action__pattern')
    " Jump to the line number.
    let l:col = has_key(a:candidate, 'action__col') ?
          \ a:candidate.action__col : 0
    call cursor(a:candidate.action__line, l:col)
    call s:open_current_line(a:is_highlight)
    return
  endif

  let l:pattern = a:candidate.action__pattern

  " Jump by search().
  let l:source = unite#get_sources(a:candidate.source)
  if !(has_key(a:candidate, 'action__signature') && has_key(l:source, 'calc_signature'))
    " Not found signature.
    if has_key(a:candidate, 'action__line')
          \ && a:candidate.action__line != ''
          \ && getline(a:candidate.action__line) =~# l:pattern
      execute a:candidate.action__line
    else
      call search(l:pattern, 'w')
    endif

    call s:open_current_line(a:is_highlight)
    return
  endif

  call search(l:pattern, 'w')

  let l:lnum_prev = line('.')
  call search(l:pattern, 'w')
  let l:lnum = line('.')
  if l:lnum != l:lnum_prev
    " Detected same pattern lines!!
    let l:start_lnum = l:lnum
    while l:source.calc_signature(l:lnum) !=# a:candidate.action__signature
      call search(l:pattern, 'w')
      let l:lnum = line('.')
      if l:lnum == l:start_lnum
        " Not found.
        call unite#print_error("unite: jump_list: Target position is not found.")
        0
        return
      endif
    endwhile
  endif

  call s:open_current_line(a:is_highlight)
endfunction"}}}

function! s:best_winline()"{{{
  return max([1, winheight(0) * g:unite_kind_jump_list_after_jump_scroll / 100])
endfunction"}}}

function! s:adjust_scroll(best_winline)"{{{
  normal! zt
  let l:save_cursor = getpos('.')
  let l:winl = 1
  " Scroll the cursor line down.
  while l:winl <= a:best_winline
    let l:winl_prev = l:winl
    execute "normal! \<C-y>"
    let l:winl = winline()
    if l:winl == l:winl_prev
      break
    end
    let l:winl_prev = l:winl
  endwhile
  if l:winl > a:best_winline
    execute "normal! \<C-e>"
  endif
  call setpos('.', l:save_cursor)
endfunction"}}}

function! s:open_current_line(is_highlight)"{{{
  normal! zv
  normal! zz
  if a:is_highlight
    execute 'match Search /\%'.line('.').'l/'
  endif
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/kinds/openable.vim	[[[1
146
"=============================================================================
" FILE: openable.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 06 Jul 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

" Variables  "{{{
call unite#util#set_default('g:unite_kind_openable_persist_open_blink_time', '250m')
"}}}
function! unite#kinds#openable#define()"{{{
  return s:kind
endfunction"}}}

let s:kind = {
      \ 'name' : 'openable',
      \ 'action_table': {},
      \}

" Actions"{{{
let s:kind.action_table.tabopen = {
      \ 'description' : 'tabopen items',
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.tabopen.func(candidates)"{{{
  for l:candidate in a:candidates
    tabnew
    call unite#take_action('open', l:candidate)
  endfor
endfunction"}}}

let s:kind.action_table.split = {
      \ 'description' : 'horizontal split open items',
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.split.func(candidates)"{{{
  for l:candidate in a:candidates
    split
    call unite#take_action('open', l:candidate)
  endfor
endfunction"}}}

let s:kind.action_table.vsplit = {
      \ 'description' : 'vertical split open items',
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.vsplit.func(candidates)"{{{
  for l:candidate in a:candidates
    vsplit
    call unite#take_action('open', l:candidate)
  endfor
endfunction"}}}

let s:kind.action_table.left = {
      \ 'description' : 'vertical left split items',
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.left.func(candidates)"{{{
  for l:candidate in a:candidates
    leftabove vsplit
    call unite#take_action('open', l:candidate)
  endfor
endfunction"}}}

let s:kind.action_table.right = {
      \ 'description' : 'vertical right split open items',
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.right.func(candidates)"{{{
  for l:candidate in a:candidates
    rightbelow vsplit
    call unite#take_action('open', l:candidate)
  endfor
endfunction"}}}

let s:kind.action_table.above = {
      \ 'description' : 'horizontal above split open items',
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.above.func(candidates)"{{{
  for l:candidate in a:candidates
    leftabove split
    call unite#take_action('open', l:candidate)
  endfor
endfunction"}}}

let s:kind.action_table.below = {
      \ 'description' : 'horizontal below split open items',
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.below.func(candidates)"{{{
  for l:candidate in a:candidates
    rightbelow split
    call unite#take_action('open', l:candidate)
  endfor
endfunction"}}}

let s:kind.action_table.persist_open = {
      \ 'description' : 'persistent open',
      \ 'is_quit'     : 0,
      \ }
function! s:kind.action_table.persist_open.func(candidate)"{{{
  if winnr('#') <= 0
    new
    wincmd p
  endif

  wincmd p
  call unite#take_action('open', a:candidate)
  if g:unite_kind_openable_persist_open_blink_time != ''
    normal! V
    redraw!
    execute 'sleep ' . g:unite_kind_openable_persist_open_blink_time
    execute "normal! \<ESC>"
  endif
  wincmd p
endfunction"}}}

"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/kinds/source.vim	[[[1
61
"=============================================================================
" FILE: source.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 03 Aug 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#kinds#source#define()"{{{
  return s:kind
endfunction"}}}

let s:kind = {
      \ 'name' : 'source',
      \ 'default_action' : 'start',
      \ 'action_table': {},
      \}

" Actions"{{{
let s:kind.action_table.start = {
      \ 'description' : 'start source',
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.start.func(candidates)"{{{
  let l:context = unite#get_context()
  let l:context.input = ''
  let l:context.auto_preview = 0
  let l:context.default_action = 'default'

  call unite#start(map(copy(a:candidates),
        \ 'has_key(v:val, "action__source_args") ?'
        \  . 'insert(v:val.action__source_args, v:val.action__source_name) :'
        \  . 'v:val.action__source_name'), l:context)
endfunction"}}}
"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/kinds/tab.vim	[[[1
91
"=============================================================================
" FILE: tab.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 25 Jun 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#kinds#tab#define()"{{{
  return s:kind
endfunction"}}}

let s:kind = {
      \ 'name' : 'tab',
      \ 'default_action' : 'open',
      \ 'action_table': {},
      \ 'alias_table': { 'edit' : 'rename' },
      \}

" Actions"{{{
let s:kind.action_table.open = {
      \ 'description' : 'open this tab',
      \ }
function! s:kind.action_table.open.func(candidate)"{{{
  execute 'tabnext' a:candidate.action__tab_nr
endfunction"}}}

let s:kind.action_table.delete = {
      \ 'description' : 'delete tabs',
      \ 'is_selectable' : 1,
      \ 'is_invalidate_cache' : 1,
      \ 'is_quit' : 0,
      \ }
function! s:kind.action_table.delete.func(candidates)"{{{
  for l:candidate in sort(a:candidates, 's:compare')
    execute 'tabclose' l:candidate.action__tab_nr
  endfor
endfunction"}}}

if exists('*gettabvar')
  " Enable cd action.
  let s:kind.parents = ['cdable']

  let s:kind.action_table.rename = {
      \ 'description' : 'rename tabs',
      \ 'is_selectable' : 1,
      \ 'is_invalidate_cache' : 1,
      \ 'is_quit' : 0,
        \ }
  function! s:kind.action_table.rename.func(candidates)"{{{
    for l:candidate in a:candidates
      let l:old_title = gettabvar(l:candidate.action__tab_nr, 'title')
      let l:title = input(printf('New title: %s -> ', l:old_title), l:old_title)
      if l:title != '' && l:title !=# l:old_title
        call settabvar(l:candidate.action__tab_nr, 'title', l:title)
      endif
    endfor
  endfunction"}}}
endif
"}}}

" Misc
function! s:compare(candidate_a, candidate_b)"{{{
  return a:candidate_b.action__tab_nr - a:candidate_a.action__tab_nr
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/kinds/uri.vim	[[[1
57
"=============================================================================
" FILE: uri.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 10 Aug 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#kinds#uri#define()"{{{
  return s:kind
endfunction"}}}

let s:System = vital#of('unite').import('System.File')

let s:kind = {
      \ 'name' : 'uri',
      \ 'default_action' : 'start',
      \ 'action_table' : {},
      \}

" Actions"{{{
let s:kind.action_table.start = {
      \ 'description' : 'open files with associated program',
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.start.func(candidates)"{{{
  for l:candidate in a:candidates
    call s:System.open(l:candidate.action__path)
  endfor
endfunction"}}}
"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/kinds/window.vim	[[[1
76
"=============================================================================
" FILE: window.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 22 Apr 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#kinds#window#define()"{{{
  return s:kind
endfunction"}}}

let s:kind = {
      \ 'name' : 'window',
      \ 'default_action' : 'open',
      \ 'action_table': {},
      \ 'parents' : ['cdable'],
      \}

" Actions"{{{
let s:kind.action_table.open = {
      \ 'description' : 'move to this window',
      \ }
function! s:kind.action_table.open.func(candidate)"{{{
  execute a:candidate.action__window_nr.'wincmd w'
endfunction"}}}

let s:kind.action_table.only = {
      \ 'description' : 'only this window',
      \ }
function! s:kind.action_table.only.func(candidate)"{{{
  execute a:candidate.action__window_nr.'wincmd w'
  only
endfunction"}}}

let s:kind.action_table.delete = {
      \ 'description' : 'delete windows',
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.delete.func(candidates)"{{{
  for l:candidate in sort(a:candidates, 's:compare')
    close
  endfor
endfunction"}}}
"}}}

" Misc
function! s:compare(candidate_a, candidate_b)"{{{
  return a:candidate_b.action__window_nr - a:candidate_a.action__window_nr
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/kinds/word.vim	[[[1
46
"=============================================================================
" FILE: word.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 22 Apr 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#kinds#word#define()"{{{
  return s:kind
endfunction"}}}

let s:kind = {
      \ 'name' : 'word',
      \ 'default_action' : 'insert',
      \ 'action_table': {},
      \}

" Actions"{{{
"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/mappings.vim	[[[1
736
"=============================================================================
" FILE: mappings.vim
" AUTHOR: Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 13 Aug 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

" Define default mappings.
function! unite#mappings#define_default_mappings()"{{{
  " Plugin keymappings"{{{
  nnoremap <silent><buffer> <Plug>(unite_exit)  :<C-u>call <SID>exit()<CR>
  nnoremap <silent><buffer> <Plug>(unite_choose_action)  :<C-u>call <SID>choose_action()<CR>
  nnoremap <expr><buffer> <Plug>(unite_insert_enter)  <SID>insert_enter('i')
  nnoremap <expr><buffer> <Plug>(unite_insert_head)   <SID>insert_enter('0'.(len(unite#get_current_unite().prompt)-1).'li')
  nnoremap <expr><buffer> <Plug>(unite_append_enter)  <SID>insert_enter('a')
  nnoremap <expr><buffer> <Plug>(unite_append_end)    <SID>insert_enter('A')
  nnoremap <silent><buffer> <Plug>(unite_toggle_mark_current_candidate)  :<C-u>call <SID>toggle_mark()<CR>
  nnoremap <silent><buffer> <Plug>(unite_redraw)  :<C-u>call <SID>redraw()<CR>
  nnoremap <silent><buffer> <Plug>(unite_rotate_next_source)  :<C-u>call <SID>rotate_source(1)<CR>
  nnoremap <silent><buffer> <Plug>(unite_rotate_previous_source)  :<C-u>call <SID>rotate_source(0)<CR>
  nnoremap <silent><buffer> <Plug>(unite_print_candidate)  :<C-u>call <SID>print_candidate()<CR>
  nnoremap <buffer><expr> <Plug>(unite_cursor_top)  unite#get_current_unite().prompt_linenr.'G0z.'
  nnoremap <buffer><expr> <Plug>(unite_loop_cursor_down)  <SID>loop_cursor_down()
  nnoremap <buffer><expr> <Plug>(unite_loop_cursor_up)  <SID>loop_cursor_up()
  nnoremap <silent><buffer> <Plug>(unite_quick_match_default_action)  :<C-u>call <SID>quick_match()<CR>
  nnoremap <silent><buffer> <Plug>(unite_input_directory)   :<C-u>call <SID>input_directory()<CR>
  nnoremap <silent><buffer><expr> <Plug>(unite_do_default_action)   unite#do_action(unite#get_current_unite().context.default_action)
  nnoremap <silent><buffer> <Plug>(unite_delete_backward_path)  :<C-u>call <SID>normal_delete_backward_path()<CR>
  nnoremap <silent><buffer> <Plug>(unite_restart)  :<C-u>call <SID>restart()<CR>
  nnoremap <buffer><silent> <Plug>(unite_toggle_mark_all_candidates)  :<C-u>call <SID>toggle_mark_candidates(0, len(unite#get_unite_candidates()) - 1)<CR>
  nnoremap <buffer><silent> <Plug>(unite_toggle_transpose_window)  :<C-u>call <SID>toggle_transpose_window()<CR>
  nnoremap <buffer><silent> <Plug>(unite_toggle_auto_preview)  :<C-u>call <SID>toggle_auto_preview()<CR>
  nnoremap <buffer><silent> <Plug>(unite_narrowing_path)  :<C-u>call <SID>narrowing_path()<CR>
  nnoremap <buffer><silent> <Plug>(unite_narrowing_input_history)  :<C-u>call <SID>narrowing_input_history()<CR>

  vnoremap <buffer><silent> <Plug>(unite_toggle_mark_selected_candidates)  :<C-u>call <SID>toggle_mark_candidates(getpos("'<")[1] - unite#get_current_unite().prompt_linenr-1, getpos("'>")[1] - unite#get_current_unite().prompt_linenr - 1)<CR>

  inoremap <silent><buffer> <Plug>(unite_exit)  <ESC>:<C-u>call <SID>exit()<CR>
  inoremap <silent><expr><buffer> <Plug>(unite_insert_leave)
        \ (line('.') <= unite#get_current_unite().prompt_linenr) ?
        \ "\<ESC>0".(unite#get_current_unite().prompt_linenr+1)."G" : "\<ESC>0"
  inoremap <silent><expr><buffer> <Plug>(unite_delete_backward_char)
        \ col('.') <= (len(unite#get_current_unite().prompt)+1) ?
        \ "\<C-o>:\<C-u>call \<SID>exit()\<Cr>" : "\<C-h>"
  inoremap <expr><buffer> <Plug>(unite_delete_backward_line)
        \ repeat("\<C-h>", col('.')-(len(unite#get_current_unite().prompt)+1))
  inoremap <expr><buffer> <Plug>(unite_delete_backward_word)
        \ col('.') <= (len(unite#get_current_unite().prompt)+1) ? '' : "\<C-w>"
  inoremap <expr><buffer> <Plug>(unite_delete_backward_path)
        \ col('.') <= (len(unite#get_current_unite().prompt)+1) ? '' : <SID>delete_backward_path()
  inoremap <expr><buffer> <Plug>(unite_select_next_line)
        \ pumvisible() ? "\<C-n>" : <SID>loop_cursor_down()
  inoremap <expr><buffer> <Plug>(unite_select_previous_line)
        \ pumvisible() ? "\<C-p>" : <SID>loop_cursor_up()
  inoremap <expr><buffer> <Plug>(unite_select_next_page)
        \ pumvisible() ? "\<PageDown>" : repeat("\<Down>", winheight(0))
  inoremap <expr><buffer> <Plug>(unite_select_previous_page)
        \ pumvisible() ? "\<PageUp>" : repeat("\<Up>", winheight(0))
  inoremap <silent><buffer> <Plug>(unite_toggle_mark_current_candidate)  <C-o>:<C-u>call <SID>toggle_mark()<CR>
  inoremap <silent><buffer> <Plug>(unite_choose_action)  <C-o>:<C-u>call <SID>choose_action()<CR>
  inoremap <silent><buffer> <Plug>(unite_move_head)  <C-o>:<C-u>call <SID>insert_head()<CR>
  inoremap <silent><buffer> <Plug>(unite_quick_match_default_action)  <C-o>:<C-u>call <SID>quick_match()<CR>
  inoremap <silent><buffer> <Plug>(unite_input_directory)   <C-o>:<C-u>call <SID>input_directory()<CR>
  inoremap <silent><buffer><expr> <Plug>(unite_do_default_action)   unite#do_action(unite#get_current_unite().context.default_action)
  inoremap <buffer><silent> <Plug>(unite_toggle_transpose_window)  <C-o>:<C-u>call <SID>toggle_transpose_window()<CR>
  inoremap <buffer><silent> <Plug>(unite_toggle_auto_preview)  <C-o>:<C-u>call <SID>toggle_auto_preview()<CR>
  inoremap <buffer><silent> <Plug>(unite_narrowing_path)  <C-o>:<C-u>call <SID>narrowing_path()<CR>
  inoremap <buffer><silent> <Plug>(unite_narrowing_input_history)  <C-o>:<C-u>call <SID>narrowing_input_history()<CR>
  "}}}

  if exists('g:unite_no_default_keymappings') && g:unite_no_default_keymappings
    return
  endif

  " Normal mode key-mappings.
  nmap <buffer> i         <Plug>(unite_insert_enter)
  nmap <buffer> I         <Plug>(unite_insert_head)
  nmap <buffer> a         <Plug>(unite_append_enter)
  nmap <buffer> A         <Plug>(unite_append_end)
  nmap <buffer> q         <Plug>(unite_exit)
  nmap <buffer> <CR>      <Plug>(unite_do_default_action)
  nmap <buffer> <Space>   <Plug>(unite_toggle_mark_current_candidate)
  nmap <buffer> <Tab>     <Plug>(unite_choose_action)
  nmap <buffer> <C-n>     <Plug>(unite_rotate_next_source)
  nmap <buffer> <C-p>     <Plug>(unite_rotate_previous_source)
  nmap <buffer> <C-g>     <Plug>(unite_print_candidate)
  nmap <buffer> <C-l>     <Plug>(unite_redraw)
  nmap <buffer> gg        <Plug>(unite_cursor_top)
  nmap <buffer> j         <Plug>(unite_loop_cursor_down)
  nmap <buffer> <Down>         <Plug>(unite_loop_cursor_down)
  nmap <buffer> k         <Plug>(unite_loop_cursor_up)
  nmap <buffer> <Up>         <Plug>(unite_loop_cursor_up)
  nmap <buffer> <C-h>     <Plug>(unite_delete_backward_path)
  nmap <buffer> <C-r>     <Plug>(unite_restart)
  nmap <buffer> *         <Plug>(unite_toggle_mark_all_candidates)

  nnoremap <silent><buffer><expr> d   unite#smart_map('d', unite#do_action('delete'))
  nnoremap <silent><buffer><expr> b   unite#smart_map('b', unite#do_action('bookmark'))
  nnoremap <silent><buffer><expr> e   unite#smart_map('e', unite#do_action('edit'))
  nnoremap <silent><buffer><expr> p   unite#do_action('preview')
  nmap <silent><buffer><expr> x       unite#smart_map('x', "\<Plug>(unite_quick_match_default_action)")

  " Visual mode key-mappings.
  xmap <buffer> <Space>   <Plug>(unite_toggle_mark_selected_candidates)

  " Insert mode key-mappings.
  imap <buffer> <TAB>     <Plug>(unite_choose_action)
  imap <buffer> <C-n>     <Plug>(unite_select_next_line)
  imap <buffer> <Down>     <Plug>(unite_select_next_line)
  imap <buffer> <C-p>     <Plug>(unite_select_previous_line)
  imap <buffer> <Up>     <Plug>(unite_select_previous_line)
  imap <buffer> <C-f>     <Plug>(unite_select_next_page)
  imap <buffer> <C-b>     <Plug>(unite_select_previous_page)
  imap <buffer> <CR>      <Plug>(unite_do_default_action)
  imap <buffer> <C-h>     <Plug>(unite_delete_backward_char)
  imap <buffer> <BS>      <Plug>(unite_delete_backward_char)
  imap <buffer> <C-u>     <Plug>(unite_delete_backward_line)
  imap <buffer> <C-w>     <Plug>(unite_delete_backward_word)
  imap <buffer> <C-a>     <Plug>(unite_move_head)
  imap <buffer> <Home>    <Plug>(unite_move_head)

  inoremap <silent><buffer><expr> d         unite#smart_map('d', unite#do_action('delete'))
  inoremap <silent><buffer><expr> e         unite#smart_map('e', unite#do_action('edit'))
  imap <silent><buffer><expr> <Space>       unite#smart_map(' ', "\<Plug>(unite_toggle_mark_current_candidate)")
  imap <silent><buffer><expr> x             unite#smart_map('x', "\<Plug>(unite_quick_match_default_action)")
endfunction"}}}

function! unite#mappings#narrowing(word)"{{{
  setlocal modifiable
  let l:unite = unite#get_current_unite()
  let l:unite.input = escape(a:word, ' *')
  call setline(unite#get_current_unite().prompt_linenr, unite#get_current_unite().prompt . unite#get_current_unite().input)
  call unite#redraw()
  if l:unite.is_insert
    execute unite#get_current_unite().prompt_linenr
    startinsert!
  else
    execute unite#get_current_unite().prompt_linenr
    normal! 0z.
  endif
endfunction"}}}
function! unite#mappings#do_action(action_name, ...)"{{{
  let l:candidates = a:0 > 0 ? a:1 : unite#get_marked_candidates()

  let l:unite = unite#get_current_unite()
  if empty(l:candidates)
    let l:num = (line('.') <= l:unite.prompt_linenr) ? 0 :
          \ (line('.') - (l:unite.prompt_linenr + 1))
    if type(l:num) == type(0)
      if line('$') - (l:unite.prompt_linenr + 1) < l:num
        " Ignore.
        return
      endif

      let l:candidates = [ unite#get_current_candidate() ]
    else
      let l:candidates = [ l:num ]
    endif
  endif

  call filter(l:candidates, '!v:val.is_dummy')
  if empty(l:candidates)
    return
  endif

  " Clear mark flag.
  for l:candidate in l:candidates
    let l:candidate.unite__is_marked = 0
  endfor

  let l:action_tables = s:get_action_table(a:action_name, l:candidates)

  let l:context = l:unite.context

  " Execute action.
  let l:is_redraw = 0
  let l:is_quit = 0
  for l:table in l:action_tables
    " Check quit flag.
    if l:table.action.is_quit
      call unite#quit_session()
      let l:is_quit = 1
    endif

    call l:table.action.func(l:table.candidates)

    " Check invalidate cache flag.
    if l:table.action.is_invalidate_cache
      for l:source_name in l:table.source_names
        call unite#invalidate_cache(l:source_name)
      endfor

      let l:is_redraw = 1
    endif
  endfor

  if l:is_redraw
    call unite#force_redraw()
    normal! zz
  endif
endfunction"}}}

function! s:get_action_table(action_name, candidates)"{{{
  let l:action_tables = []
  let Self = unite#get_self_functions()[-1]
  for l:candidate in a:candidates
    let l:action_table = unite#get_action_table(l:candidate.source, l:candidate.kind, Self)

    let l:action_name =
          \ a:action_name ==# 'default' ?
          \ unite#get_default_action(l:candidate.source, l:candidate.kind)
          \ : a:action_name

    if !has_key(l:action_table, l:action_name)
      call unite#util#print_error(l:candidate.abbr . '(' . l:candidate.source . ')')
      call unite#util#print_error('No such action : ' . l:action_name)
      return []
    endif

    let l:action = l:action_table[l:action_name]

    " Check selectable flag.
    if !l:action.is_selectable && len(a:candidates) > 1
      call unite#util#print_error(l:candidate.abbr . '(' . l:candidate.source . ')')
      call unite#util#print_error('Not selectable action : ' . l:action_name)
      return []
    endif

    let l:found = 0
    for l:table in l:action_tables
      if l:action == l:table.action
        " Add list.
        call add(l:table.candidates, l:candidate)
        call add(l:table.source_names, l:candidate.source)
        let l:found = 1
        break
      endif
    endfor

    if !l:found
      " Add action table.
      call add(l:action_tables, {
            \ 'action' : l:action,
            \ 'source_names' : [l:candidate.source],
            \ 'candidates' : (!l:action.is_selectable ? l:candidate : [l:candidate]),
            \ })
    endif
  endfor

  return l:action_tables
endfunction"}}}
function! s:get_actions(candidates)"{{{
  let Self = unite#get_self_functions()[-1]
  let l:actions = unite#get_action_table(a:candidates[0].source, a:candidates[0].kind, Self)
  if len(a:candidates) > 1
    for l:candidate in a:candidates
      let l:action_table = unite#get_action_table(l:candidate.source, l:candidate.kind, Self)
      " Filtering unique items and check selectable flag.
      call filter(l:actions, 'has_key(l:action_table, v:key)
            \ && l:action_table[v:key].is_selectable')
    endfor
  endif

  return l:actions
endfunction"}}}

" key-mappings functions.
function! s:exit()"{{{
  call unite#force_quit_session()
endfunction"}}}
function! s:restart()"{{{
  let l:unite = unite#get_current_unite()
  let l:context = l:unite.context
  let l:sources = map(deepcopy(l:unite.sources), 'empty(v:val.args) ? v:val.name : [v:val.name, v:val.args]')
  call unite#force_quit_session()
  call unite#start(l:sources, l:context)
endfunction"}}}
function! s:delete_backward_path()"{{{
  let l:unite    = unite#get_current_unite()
  let l:prompt   = l:unite.prompt
  let l:input    = getline(l:unite.prompt_linenr)[len(l:prompt):]
  let l:startcol = match(l:input, '[^/]*.$') + 1 + len(l:prompt)
  let l:endcol   = virtcol('.')
  return repeat("\<C-h>", (l:startcol < l:endcol ? l:endcol - l:startcol : 0))
endfunction"}}}
function! s:normal_delete_backward_path()"{{{
  let l:modifiable_save = &l:modifiable
  setlocal modifiable
  call setline(unite#get_current_unite().prompt_linenr,
        \ substitute(getline(unite#get_current_unite().prompt_linenr)[len(unite#get_current_unite().prompt):],
        \                 '[^/]*.$', '', ''))
  call unite#redraw()
  let &l:modifiable = l:modifiable_save
endfunction"}}}
function! s:toggle_mark()"{{{
  let l:candidate = unite#get_current_candidate()
  let l:candidate.unite__is_marked = !l:candidate.unite__is_marked
  let l:candidate.unite__marked_time = localtime()

  let l:prompt_linenr = unite#get_current_unite().prompt_linenr
  if line('.') <= l:prompt_linenr
    call cursor(l:prompt_linenr+1, 0)
  endif
  call unite#redraw_line()

  normal! j
endfunction"}}}
function! s:toggle_mark_candidates(start, end)"{{{
  if a:start < 0 || a:end >= len(unite#get_unite_candidates())
    " Ignore.
    return
  endif

  let l:cnt = a:start
  while l:cnt <= a:end
    let l:candidate = unite#get_unite_candidates()[l:cnt]
    let l:candidate.unite__is_marked = !l:candidate.unite__is_marked
    let l:candidate.unite__marked_time = localtime()

    call unite#redraw_line(l:cnt + unite#get_current_unite().prompt_linenr+1)

    let l:cnt += 1
  endwhile
endfunction"}}}
function! s:choose_action()"{{{
  let l:unite = unite#get_current_unite()
  if line('$') < (l:unite.prompt_linenr+1)
        \ || l:unite.context.temporary
    " Ignore.
    return
  endif

  let l:candidates = unite#get_marked_candidates()
  if empty(l:candidates)
    let l:candidates = [ unite#get_current_candidate() ]
  endif

  call filter(l:candidates, '!v:val.is_dummy')
  if empty(l:candidates)
    return
  endif

  call unite#define_source(s:source_action)

  call unite#start_temporary([['action'] + l:candidates], {}, 'action')
endfunction"}}}
function! s:insert_enter(key)"{{{
  setlocal modifiable
  return a:key
endfunction"}}}
function! s:insert_head()"{{{
  let l:pos = getpos('.')
  let l:pos[2] = len(unite#get_current_unite().prompt)+1
  call setpos('.', l:pos)
  call s:insert_enter(col('.'))
endfunction"}}}
function! s:redraw()"{{{
  call unite#clear_message()

  let l:unite = unite#get_current_unite()
  call unite#force_redraw()
endfunction"}}}
function! s:rotate_source(is_next)"{{{
  let l:unite = unite#get_current_unite()

  for l:source in unite#loaded_sources_list()
    let l:unite.sources = a:is_next ?
          \ add(l:unite.sources[1:], l:unite.sources[0]) :
          \ insert(l:unite.sources[: -2], l:unite.sources[-1])

    if !empty(l:unite.sources[0].unite__candidates)
      break
    endif
  endfor

  call unite#redraw_status()
  call unite#redraw_candidates()
endfunction"}}}
function! s:print_candidate()"{{{
  if line('.') <= unite#get_current_unite().prompt_linenr
    " Ignore.
    return
  endif

  let l:candidate = unite#get_current_candidate()
  echo l:candidate.word
endfunction"}}}
function! s:insert_selected_candidate()"{{{
  if line('.') <= unite#get_current_unite().prompt_linenr
    " Ignore.
    return
  endif

  let l:candidate = unite#get_current_candidate()
  call unite#mappings#narrowing(l:candidate.word)
endfunction"}}}
function! s:quick_match()"{{{
  let l:unite = unite#get_current_unite()

  if line('$') < (l:unite.prompt_linenr+1)
    call unite#util#print_error('Candidate is nothing.')
    return
  elseif !empty(unite#get_marked_candidates())
    call unite#util#print_error('Marked candidates is detected.')
    return
  endif

  call unite#quick_match_redraw()

  if mode() !~# '^c'
    echo 'Input quick match key: '
  endif
  let l:char = ''

  while l:char == ''
    let l:char = nr2char(getchar())
  endwhile

  redraw
  echo ''

  call unite#redraw_candidates()

  if has_key(g:unite_quick_match_table, l:char)
        \ && g:unite_quick_match_table[l:char] < len(l:unite.candidates)
    call unite#mappings#do_action(l:unite.context.default_action,
          \ [ l:unite.candidates[g:unite_quick_match_table[l:char]] ])
  else
    call unite#util#print_error('Canceled.')
  endif
endfunction"}}}
function! s:input_directory()"{{{
  let l:path = unite#substitute_path_separator(input('Input narrowing directory: ', unite#get_input(), 'dir'))
  let l:path = l:path.(l:path == '' || l:path =~ '/$' ? '' : '/')
  call unite#mappings#narrowing(l:path)
endfunction"}}}
function! s:loop_cursor_down()"{{{
  let l:is_insert = mode() ==# 'i'
  let l:prompt_linenr = unite#get_current_unite().prompt_linenr

  if line('.') == line('$')
    " Loop.
    if l:is_insert
      return "\<C-Home>\<End>".repeat("\<Down>", l:prompt_linenr-1)."\<End>"
    else
      return l:prompt_linenr.'G0z.'
    endif
  endif

  let l:num = (line('.') <= l:prompt_linenr) ? 0 :
        \ (line('.') - (l:prompt_linenr + 1))
  let l:count = 1

  while 1
    let l:candidate = get(unite#get_unite_candidates(), l:num + l:count, {})
    if !empty(l:candidate) && l:candidate.is_dummy
      let l:count += 1
      continue
    endif

    break
  endwhile

  if l:is_insert && line('.') == l:prompt_linenr
    let l:count += 1
  endif

  if l:is_insert
    return "\<Home>" . repeat("\<Down>", l:count)
  else
    return '0' . repeat('j', l:count)
  endif
endfunction"}}}
function! s:loop_cursor_up()"{{{
  let l:is_insert = mode() ==# 'i'
  let l:prompt_linenr = unite#get_current_unite().prompt_linenr

  if line('.') <= l:prompt_linenr
    " Loop.
    if l:is_insert
      return "\<C-End>\<Home>"
    else
      return 'G'
    endif
  endif

  let l:num = (line('.') <= l:prompt_linenr) ? 0 :
        \ (line('.') - (l:prompt_linenr + 1))

  let l:count = 1

  if l:is_insert && line('.') == l:prompt_linenr + 2
    let l:count += 1
  endif

  while 1
    let l:candidate = get(unite#get_unite_candidates(), l:num - l:count, {})
    if l:num >= l:count && !empty(l:candidate) && l:candidate.is_dummy
      let l:count += 1
      continue
    endif

    break
  endwhile

  if l:num < 0
    if l:is_insert
      return "\<C-Home>\<End>".repeat("\<Down>", l:prompt_linenr)."\<Home>"
    else
      return l:prompt_linenr.'G0z.'
    endif
  endif

  if l:is_insert
    if line('.') <= l:prompt_linenr + 2
      return repeat("\<Up>", l:count) . "\<End>"
    else
      return "\<Home>" . repeat("\<Up>", l:count)
    endif
  else
    return '0' . repeat('k', l:count)
  endif
endfunction"}}}
function! s:toggle_transpose_window()"{{{
  " Toggle vertical/horizontal view.
  let l:context = unite#get_context()
  let l:direction = l:context.vertical ?
        \ (l:context.direction ==# 'topleft' ? 'K' : 'J') :
        \ (l:context.direction ==# 'topleft' ? 'H' : 'L')

  execute 'silent wincmd ' . l:direction

  let l:context.vertical = !l:context.vertical
endfunction"}}}
function! s:toggle_auto_preview()"{{{
  let l:context = unite#get_context()
  let l:context.auto_preview = !l:context.auto_preview

  if !l:context.auto_preview
        \ && !unite#get_current_unite().has_preview_window
    " Close preview window.
    pclose!
  endif
endfunction"}}}
function! s:narrowing_path()"{{{
  if line('.') <= unite#get_current_unite().prompt_linenr
    " Ignore.
    return
  endif

  let l:candidate = unite#get_current_candidate()
  call unite#mappings#narrowing(has_key(l:candidate, 'action__path')? l:candidate.action__path : l:candidate.word)
endfunction"}}}
function! s:narrowing_input_history()"{{{
  let l:unite = unite#get_current_unite()

  call unite#define_source(s:source_input)

  call unite#start_temporary(['history/input'],
        \ { 'old_source_names_string' : unite#loaded_source_names_string() },
        \ 'history/input')
endfunction"}}}

function! unite#mappings#complete_actions(arglead, cmdline, cursorpos)"{{{
  return filter(keys(s:actions), printf('stridx(v:val, %s) == 0', string(a:arglead)))
endfunction"}}}

" Unite action source."{{{
let s:source_action = {
      \ 'name' : 'action',
      \ 'description' : 'candidates from unite action',
      \ 'action_table' : {},
      \ 'hooks' : {},
      \ 'default_action' : 'do',
      \ 'syntax' : 'uniteSource__Action',
      \}

function! s:source_action.hooks.on_close(args, context)"{{{
  call unite#undef_source('action')
endfunction"}}}
function! s:source_action.hooks.on_syntax(args, context)"{{{
  syntax match uniteSource__ActionDescriptionLine / -- .*$/ contained containedin=uniteSource__Action
  syntax match uniteSource__ActionDescription /.*$/ contained containedin=uniteSource__ActionDescriptionLine
  syntax match uniteSource__ActionMarker / -- / contained containedin=uniteSource__ActionDescriptionLine
  highlight default link uniteSource__ActionMarker Special
  highlight default link uniteSource__ActionDescription Comment
endfunction"}}}

function! s:source_action.gather_candidates(args, context)"{{{
  let l:candidates = copy(a:args)

  " Print candidates.
  call unite#print_message(map(copy(l:candidates), '"[action] candidates: ".v:val.abbr."(".v:val.source.")"'))

  " Process Alias.
  let l:actions = s:get_actions(l:candidates)
  let l:alias_table = unite#get_alias_table(
        \ l:candidates[0].source, l:candidates[0].kind)
  for [l:alias_name, l:action_name] in items(l:alias_table)
    if has_key(l:actions, l:alias_name)
      let l:actions[l:action_name] = copy(l:actions[l:action_name])
      let l:actions[l:action_name].name = l:alias_name
    endif
  endfor

  " Uniq.
  let l:uniq_actions = {}
  for l:action in values(l:actions)
    if !has_key(l:action, l:action.name)
      let l:uniq_actions[l:action.name] = l:action
    endif
  endfor

  let l:max = max(map(values(l:uniq_actions), 'len(v:val.name)'))

  return sort(map(values(l:uniq_actions), '{
        \   "word": v:val.name,
        \   "abbr": printf("%-' . l:max . 's -- %s", v:val.name, v:val.description),
        \   "kind": "common",
        \   "source__candidates": l:candidates,
        \   "action__action": l:actions[v:val.name],
        \ }'), 's:compare_word')
endfunction"}}}

function! s:compare_word(i1, i2)
  return (a:i1.word ># a:i2.word) ? 1 : -1
endfunction

" Actions"{{{
let s:action_table = {}

let s:action_table.do = {
      \ 'description' : 'do action',
      \ }
function! s:action_table.do.func(candidate)"{{{
  call unite#mappings#do_action(a:candidate.word, a:candidate.source__candidates)
endfunction"}}}

let s:source_action.action_table['*'] = s:action_table

unlet s:action_table
"}}}
"}}}

" Unite history/input source."{{{
let s:source_input = {
      \ 'name' : 'history/input',
      \ 'description' : 'candidates from unite input history',
      \ 'action_table' : {},
      \ 'hooks' : {},
      \ 'default_action' : 'narrow',
      \ 'syntax' : 'uniteSource__Action',
      \}

function! s:source_input.hooks.on_close(args, context)"{{{
  call unite#undef_source('history/input')
endfunction"}}}

function! s:source_input.gather_candidates(args, context)"{{{
  let l:context = unite#get_context()
  let l:inputs = unite#get_buffer_name_option(
        \ l:context.old_buffer_info[0].buffer_name, 'unite__inputs')
  let l:key = l:context.old_source_names_string
  if !has_key(l:inputs, l:key)
    return []
  endif

  return map(copy(l:inputs[l:key]), '{
        \ "word" : v:val
        \ }')
endfunction"}}}

" Actions"{{{
let s:action_table = {}

let s:action_table.narrow = {
      \ 'description' : 'narrow by history',
      \ 'is_quit' : 0,
      \ }
function! s:action_table.narrow.func(candidate)"{{{
  call unite#force_quit_session()
  call unite#mappings#narrowing(a:candidate.word)
endfunction"}}}

let s:action_table.delete = {
      \ 'description' : 'delete from input history',
      \ 'is_selectable' : 1,
      \ 'is_quit' : 0,
      \ 'is_invalidate_cache' : 1,
      \ }
function! s:action_table.delete.func(candidates)"{{{
  let l:context = unite#get_context()
  let l:inputs = unite#get_buffer_name_option(
        \ l:context.old_buffer_info[0].buffer_name, 'unite__inputs')
  let l:key = l:context.old_source_names_string
  if !has_key(l:inputs, l:key)
    return
  endif

  for l:candidate in a:candidates
    call filter(l:inputs[l:key], 'v:val !=# l:candidate.word')
  endfor
endfunction"}}}

let s:source_input.action_table['*'] = s:action_table

unlet s:action_table
"}}}
"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/sources/bookmark.vim	[[[1
188
"=============================================================================
" FILE: bookmark.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 14 May 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

" Variables  "{{{
" The version of bookmark file format.
let s:VERSION = '0.1.0'

let s:bookmark_file_mtime = 0  " the last modified time of the bookmark file.

" [ [ name, full_path, linenr, search pattern ], ... ]
let s:bookmark_files = []

call unite#util#set_default('g:unite_source_bookmark_file',  g:unite_data_directory . '/.bookmark')
"}}}

function! unite#sources#bookmark#define()"{{{
  return s:source
endfunction"}}}
function! unite#sources#bookmark#_append(filename)"{{{
  if a:filename == ''
    " Append the current buffer to the bookmark list.
    let l:path = expand('%:p')
    let l:linenr = line('.')
    let l:pattern = '^' . escape(getline('.'), '~"\.^*$[]') . '$'
  else
    let l:path = fnamemodify(a:filename, ':p')
    let l:linenr = ''
    let l:pattern = ''
  endif

  let l:filename = (a:filename == '' ? expand('%') : a:filename)
  if bufexists(l:filename)
    let l:filetype = getbufvar(l:path, '&filetype')

    " Detect vimfiler and vimshell.
    if l:filetype ==# 'vimfiler'
      let l:path = getbufvar(l:path, 'vimfiler').current_dir
    elseif l:filetype ==# 'vimshell'
      let l:path = getbufvar(l:path, 'vimshell').save_dir
    endif
  endif

  let l:path = unite#substitute_path_separator(l:path)
  if !s:is_exists_path(path)
    return
  endif

  redraw
  echo a:filename
  let l:name = input('Please input bookmark name : ')

  call s:load()
  call insert(s:bookmark_files, [l:name, l:path, l:linenr, l:pattern])
  call s:save()
endfunction"}}}

let s:source = {
      \ 'name' : 'bookmark',
      \ 'description' : 'candidates from bookmark list',
      \ 'action_table' : {},
      \}

function! s:source.gather_candidates(args, context)"{{{
  call s:load()
  return map(copy(s:bookmark_files), '{
        \ "abbr" : (v:val[0] != "" ? "[" . v:val[0] . "] " : "") .  
        \          (fnamemodify(v:val[1], ":~:.") != "" ? fnamemodify(v:val[1], ":~:.") : v:val[1]),
        \ "word" : v:val[1],
        \ "kind" : (isdirectory(v:val[1]) ? "directory" : "jump_list"),
        \ "source_bookmark_name" : v:val[0],
        \ "action__path" : v:val[1],
        \ "action__line" : v:val[2],
        \ "action__pattern" : v:val[3],
        \ "action__directory" : unite#path2directory(v:val[1]),
        \   }')
endfunction"}}}

" Actions"{{{
let s:action_table = {}

let s:action_table.delete = {
      \ 'description' : 'delete from bookmark list',
      \ 'is_invalidate_cache' : 1,
      \ 'is_quit' : 0,
      \ 'is_selectable' : 1,
      \ }
function! s:action_table.delete.func(candidates)"{{{
  for l:candidate in a:candidates
    call filter(s:bookmark_files, 'string(v:val) !=# ' .
        \ string(string([l:candidate.source_bookmark_name, l:candidate.action__path, l:candidate.action__line, l:candidate.action__pattern])))
  endfor

  call s:save()
endfunction"}}}

let s:source.action_table['*'] = s:action_table
unlet! s:action_table
"}}}

" Add custom action table."{{{
let s:file_bookmark_action = {
      \ 'description' : 'append files to bookmark list',
      \ }
function! s:file_bookmark_action.func(candidate)"{{{
  " Add to bookmark.
  call unite#sources#bookmark#_append(a:candidate.action__path)
endfunction"}}}

let s:buffer_bookmark_action = {
      \ 'description' : 'append buffers to bookmark list',
      \ }
function! s:buffer_bookmark_action.func(candidate)"{{{
  let l:filetype = getbufvar(a:candidate.action__buffer_nr, '&filetype')
  if l:filetype ==# 'vimfiler'
    let l:filename = getbufvar(a:candidate.action__buffer_nr, 'vimfiler').current_dir
  elseif l:filetype ==# 'vimshell'
    let l:filename = getbufvar(a:candidate.action__buffer_nr, 'vimshell').save_dir
  else
    let l:filename = a:candidate.action__path
  endif

  " Add to bookmark.
  call unite#sources#bookmark#_append(l:filename)
endfunction"}}}

call unite#custom_action('file', 'bookmark', s:file_bookmark_action)
call unite#custom_action('buffer', 'bookmark', s:buffer_bookmark_action)
unlet! s:file_bookmark_action
unlet! s:buffer_bookmark_action
"}}}

" Misc
function! s:save()  "{{{
  call writefile([s:VERSION] + map(copy(s:bookmark_files), 'join(v:val, "\t")'),
  \              g:unite_source_bookmark_file)
  let s:bookmark_file_mtime = getftime(g:unite_source_bookmark_file)
endfunction"}}}
function! s:load()  "{{{
  if filereadable(g:unite_source_bookmark_file)
  \  && s:bookmark_file_mtime != getftime(g:unite_source_bookmark_file)
    let [ver; s:bookmark_files] = readfile(g:unite_source_bookmark_file)
    if ver !=# s:VERSION
      echohl WarningMsg
      echomsg 'Sorry, the version of bookmark file is old.  Clears the bookmark list.'
      echohl None
      let s:bookmark_files = []
      return
    endif
    let s:bookmark_files =
    \   filter(map(s:bookmark_files,
    \              'split(v:val, "\t", 1)'), 's:is_exists_path(v:val[1])')
    let s:bookmark_file_mtime = getftime(g:unite_source_bookmark_file)
  endif
endfunction"}}}
function! s:is_exists_path(path)  "{{{
  return isdirectory(a:path) || filereadable(a:path)
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/sources/buffer.vim	[[[1
217
"=============================================================================
" FILE: buffer.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 10 Jul 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

" Variables  "{{{
let s:buffer_list = {}
"}}}

function! unite#sources#buffer#define()"{{{
  return [s:source_buffer_all, s:source_buffer_tab]
endfunction"}}}
function! unite#sources#buffer#_append()"{{{
  " Append the current buffer.
  let l:bufnr = bufnr('%')
  let s:buffer_list[l:bufnr] = {
        \ 'action__buffer_nr' : l:bufnr, 'source__time' : localtime(),
        \ }

  if !exists('t:unite_buffer_dictionary')
    let t:unite_buffer_dictionary = {}
  endif

  if exists('*gettabvar')
    " Delete same buffer in other tab pages.
    for l:tabnr in range(1, tabpagenr('$'))
      let l:buffer_dict = gettabvar(l:tabnr, 'unite_buffer_dictionary')
      if type(l:buffer_dict) == type({}) && has_key(l:buffer_dict, l:bufnr)
        call remove(l:buffer_dict, l:bufnr)
      endif
      unlet l:buffer_dict
    endfor
  endif

  let t:unite_buffer_dictionary[l:bufnr] = 1
endfunction"}}}

let s:source_buffer_all = {
      \ 'name' : 'buffer',
      \ 'description' : 'candidates from buffer list',
      \ 'syntax' : 'uniteSource__Buffer',
      \ 'hooks' : {},
      \}

function! s:source_buffer_all.hooks.on_init(args, context)"{{{
  let a:context.source__buffer_list = s:get_buffer_list()
endfunction"}}}
function! s:source_buffer_all.hooks.on_syntax(args, context)"{{{
  syntax match uniteSource__Buffer_Directory /\[.*\]/ contained containedin=uniteSource__Buffer
  highlight default link uniteSource__Buffer_Directory PreProc
endfunction"}}}

function! s:source_buffer_all.gather_candidates(args, context)"{{{
  if a:context.is_redraw
    " Recaching.
    let a:context.source__buffer_list = s:get_buffer_list()
  endif

  let l:candidates = map(copy(a:context.source__buffer_list), '{
        \ "word" : s:make_word(v:val.action__buffer_nr),
        \ "abbr" : s:make_abbr(v:val.action__buffer_nr),
        \ "kind" : "buffer",
        \ "action__path" : unite#substitute_path_separator(bufname(v:val.action__buffer_nr)),
        \ "action__buffer_nr" : v:val.action__buffer_nr,
        \ "action__directory" : s:get_directory(v:val.action__buffer_nr),
        \}')

  return l:candidates
endfunction"}}}

let s:source_buffer_tab = {
      \ 'name' : 'buffer_tab',
      \ 'description' : 'candidates from buffer list in current tab',
      \ 'syntax' : 'uniteSource__BufferTab',
      \ 'hooks' : {},
      \}

function! s:source_buffer_tab.hooks.on_init(args, context)"{{{
  let a:context.source__buffer_list = s:get_buffer_list()
endfunction"}}}
function! s:source_buffer_tab.hooks.on_syntax(args, context)"{{{
  syntax match uniteSource__BufferTab_Directory /\[.*\]/ containedin=uniteSource__BufferTab
  highlight default link uniteSource__BufferTab_Directory PreProc
endfunction"}}}

function! s:source_buffer_tab.gather_candidates(args, context)"{{{
  if a:context.is_redraw
    " Recaching.
    let a:context.source__buffer_list = s:get_buffer_list()
  endif

  if !exists('t:unite_buffer_dictionary')
    let t:unite_buffer_dictionary = {}
  endif

  let l:list = filter(copy(a:context.source__buffer_list), 'has_key(t:unite_buffer_dictionary, v:val.action__buffer_nr)')

  let l:candidates = map(l:list, '{
        \ "word" : s:make_word(v:val.action__buffer_nr),
        \ "abbr" : s:make_abbr(v:val.action__buffer_nr),
        \ "kind" : "buffer",
        \ "action__path" : unite#substitute_path_separator(bufname(v:val.action__buffer_nr)),
        \ "action__buffer_nr" : v:val.action__buffer_nr,
        \ "action__directory" : s:get_directory(v:val.action__buffer_nr),
        \}')

  return l:candidates
endfunction"}}}

" Misc
function! s:make_word(bufnr)"{{{
  let l:filetype = getbufvar(a:bufnr, '&filetype')
  if l:filetype ==# 'vimfiler'
    let l:path = getbufvar(a:bufnr, 'vimfiler').current_dir
    let l:path = printf('*vimfiler* [%s]', unite#substitute_path_separator(simplify(l:path)))
  elseif l:filetype ==# 'vimshell'
    let l:vimshell = getbufvar(a:bufnr, 'vimshell')
    let l:path = printf('*vimshell*: [%s]',
          \ unite#substitute_path_separator(simplify(l:vimshell.save_dir)))
  else
    let l:path = unite#substitute_path_separator(simplify(bufname(a:bufnr)))
  endif

  return l:path
endfunction"}}}
function! s:make_abbr(bufnr)"{{{
  let l:filetype = getbufvar(a:bufnr, '&filetype')
  if l:filetype ==# 'vimfiler'
    let l:path = getbufvar(a:bufnr, 'vimfiler').current_dir
    let l:path = printf('*vimfiler* [%s]', unite#substitute_path_separator(simplify(l:path)))
  elseif l:filetype ==# 'vimshell'
    let l:vimshell = getbufvar(a:bufnr, 'vimshell')
    let l:path = printf('*vimshell*: %s [%s]',
          \ (has_key(l:vimshell, 'cmdline') ? l:vimshell.cmdline : ''),
          \ unite#substitute_path_separator(simplify(l:vimshell.save_dir)))
  else
    let l:path = fnamemodify(bufname(a:bufnr), ':~:.') . (getbufvar(a:bufnr, '&modified') ? '[+]' : '')
    let l:path = unite#substitute_path_separator(simplify(l:path))
  endif

  return l:path
endfunction"}}}
function! s:compare(candidate_a, candidate_b)"{{{
  return a:candidate_b.source__time - a:candidate_a.source__time
endfunction"}}}
function! s:get_directory(bufnr)"{{{
  let l:filetype = getbufvar(a:bufnr, '&filetype')
  if l:filetype ==# 'vimfiler'
    let l:dir = getbufvar(a:bufnr, 'vimfiler').current_dir
  elseif l:filetype ==# 'vimshell'
    let l:dir = getbufvar(a:bufnr, 'vimshell').save_dir
  else
    let l:path = unite#substitute_path_separator(bufname(a:bufnr))
    let l:dir = unite#path2directory(l:path)
  endif

  return l:dir
endfunction"}}}
function! s:get_buffer_list()"{{{
  " Make buffer list.
  let l:list = []
  let l:bufnr = 1
  while l:bufnr <= bufnr('$')
    if buflisted(l:bufnr) && l:bufnr != bufnr('%')
      if has_key(s:buffer_list, l:bufnr)
        call add(l:list, s:buffer_list[l:bufnr])
      else
        call add(l:list,
              \ { 'action__buffer_nr' : l:bufnr, 'source__time' : 0 })
      endif
    endif
    let l:bufnr += 1
  endwhile

  call sort(l:list, 's:compare')

  if buflisted(bufnr('%'))
    " Add current buffer.
    if has_key(s:buffer_list, bufnr('%'))
      call add(l:list, s:buffer_list[bufnr('%')])
    else
      call add(l:list,
            \ { 'action__buffer_nr' : bufnr('%'), 'source__time' : 0 })
    endif
  endif

  return l:list
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/sources/change.vim	[[[1
80
"=============================================================================
" FILE: changes.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 23 Jul 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

" Variables  "{{{
"}}}

function! unite#sources#change#define()"{{{
  return s:source
endfunction"}}}

let s:source = {
      \ 'name' : 'change',
      \ 'description' : 'candidates from changes',
      \ 'hooks' : {},
      \ }

let s:cached_result = []
function! s:source.hooks.on_init(args, context)"{{{
  " Get changes list.
  redir => l:redir
  silent! changes
  redir END

  let l:result = []
  let l:max_width = (winwidth(0) - 5)
  for change in split(l:redir, '\n')[1:]
    let l:list = split(change)
    if len(l:list) < 4
      continue
    endif

    let [l:linenr, l:col, l:text] = [l:list[1], l:list[2]+1, join(l:list[3:])]

    call add(l:result, {
          \ 'word' : unite#util#truncate_smart(printf('%4d-%-3d  %s', l:linenr, l:col, l:text),
          \           l:max_width, l:max_width/3, '..'),
          \ 'kind' : 'jump_list',
          \ 'action__path' : unite#util#substitute_path_separator(fnamemodify(expand('%'), ':p')),
          \ 'action__buffer_nr' : bufnr('%'),
          \ 'action__line' : l:linenr,
          \ 'action__col' : l:col,
          \ })
  endfor

  let a:context.source__result = l:result
endfunction"}}}
function! s:source.gather_candidates(args, context)"{{{
  return a:context.source__result
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/sources/command.vim	[[[1
133
"=============================================================================
" FILE: command.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 11 Jul 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

" Variables  "{{{
"}}}

function! unite#sources#command#define()"{{{
  return s:source
endfunction"}}}

let s:source = {
      \ 'name' : 'command',
      \ 'description' : 'candidates from Ex command',
      \ 'default_action' : 'edit',
      \ 'max_candidates' : 30,
      \ }

let s:cached_result = []
function! s:source.gather_candidates(args, context)"{{{
  if !a:context.is_redraw && !empty(s:cached_result)
    return s:cached_result
  endif

  " Get command list.
  redir => l:result
  silent! command
  redir END

  let s:cached_result = []
  for line in split(l:result, '\n')[1:]
    let l:word = matchstr(line, '\a\w*')

    " Analyze prototype.
    let l:end = matchend(line, '\a\w*')
    let l:args = matchstr(line, '[[:digit:]?+*]', l:end)
    if l:args != '0'
      let l:prototype = matchstr(line, '\a\w*', l:end)

      if l:prototype == ''
        let l:prototype = 'arg'
      endif

      if l:args == '*'
        let l:prototype = '[' . l:prototype . '] ...'
      elseif l:args == '?'
        let l:prototype = '[' . l:prototype . ']'
      elseif l:args == '+'
        let l:prototype = l:prototype . ' ...'
      endif
    else
      let l:prototype = ''
    endif

    call add(s:cached_result, {
          \ 'word' : l:word,
          \ 'abbr' : printf('%-16s %s', l:word, l:prototype),
          \ 'kind' : 'command',
          \ 'action__command' : l:word,
          \})
  endfor
  let s:cached_result += s:caching_from_neocomplcache_dict()

  return s:cached_result
endfunction"}}}
function! s:source.change_candidates(args, context)"{{{
  let l:dummy = substitute(a:context.input, '[*\\]', '', 'g')
  if len(split(l:dummy)) > 1
    " Add dummy result.
    return [{
          \ 'word' : l:dummy,
          \ 'abbr' : printf('[new command] %s', l:dummy),
          \ 'kind' : 'command',
          \ 'source' : 'command',
          \ 'action__command' : l:dummy,
          \}]
  endif

  return []
endfunction"}}}

function! s:caching_from_neocomplcache_dict()"{{{
  let l:dict_files = split(globpath(&runtimepath, 'autoload/neocomplcache/sources/vim_complete/commands.dict'), '\n')
  if empty(l:dict_files)
    return []
  endif

  let l:keyword_pattern =
        \'^\%(-\h\w*\%(=\%(\h\w*\|[01*?+%]\)\?\)\?\|<\h[[:alnum:]_-]*>\?\|\h[[:alnum:]_:#\[]*\%([!\]]\+\|()\?\)\?\)'
  let l:keyword_list = []
  for line in readfile(l:dict_files[0])
    let l:word = substitute(matchstr(line, l:keyword_pattern), '[\[\]]', '', 'g')
    call add(l:keyword_list, {
          \ 'word' : l:word,
          \ 'abbr' : line,
          \ 'kind' : 'command',
          \ 'source' : 'command',
          \ 'action__command' : l:word,
          \})
  endfor

  return l:keyword_list
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/sources/directory_mru.vim	[[[1
187
"=============================================================================
" FILE: directory_mru.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 02 Aug 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

" Variables  "{{{
" The version of MRU file format.
let s:VERSION = '0.2.0'

" [[full_path, localtime()], ... ]
let s:mru_dirs = []

let s:mru_file_mtime = 0  " the last modified time of the mru file.

call unite#util#set_default('g:unite_source_directory_mru_time_format', '(%c) ')
call unite#util#set_default('g:unite_source_directory_mru_file',  g:unite_data_directory . '/.directory_mru')
call unite#util#set_default('g:unite_source_directory_mru_limit', 100)
call unite#util#set_default('g:unite_source_directory_mru_ignore_pattern',
      \'\%(^\|/\)\.\%(hg\|git\|bzr\|svn\)\%($\|/\)\|^\%(\\\\\|/mnt/\|/media/\|/Volumes/\)')
"}}}

function! unite#sources#directory_mru#define()"{{{
  return s:source
endfunction"}}}
function! unite#sources#directory_mru#_append()"{{{
  let l:filetype = getbufvar(bufnr('%'), '&filetype')
  if l:filetype ==# 'vimfiler'
    let l:path = getbufvar(bufnr('%'), 'vimfiler').current_dir
  elseif l:filetype ==# 'vimshell'
    let l:path = getbufvar(bufnr('%'), 'vimshell').save_dir
  else
    let l:path = getcwd()
  endif

  let l:path = unite#util#substitute_path_separator(simplify(resolve(l:path)))
  " Chomp last /.
  let l:path = substitute(l:path, '/$', '', '')

  " Append the current buffer to the mru list.
  if !isdirectory(path) || &l:buftype =~ 'help'
  \   || (g:unite_source_directory_mru_ignore_pattern != ''
  \      && l:path =~# g:unite_source_directory_mru_ignore_pattern)
    return
  endif

  call s:load()

  let l:save_ignorecase = &ignorecase
  let &ignorecase = unite#is_win()

  call insert(filter(s:mru_dirs, 'v:val.action__path != l:path'),
  \           s:convert2dictionary([l:path, localtime()]))

  let &ignorecase = l:save_ignorecase

  if g:unite_source_directory_mru_limit > len(s:mru_dirs)
    let s:mru_dirs = s:mru_dirs[ : g:unite_source_directory_mru_limit - 1]
  endif

  call s:save()
endfunction"}}}

let s:source = {
      \ 'name' : 'directory_mru',
      \ 'description' : 'candidates from directory MRU list',
      \ 'max_candidates' : 30,
      \ 'hooks' : {},
      \ 'action_table' : {},
      \ 'syntax' : 'uniteSource__DirectoryMru',
      \}

function! s:source.hooks.on_syntax(args, context)"{{{
  syntax match uniteSource__DirectoryMru_Time /(.*)/ contained containedin=uniteSource__DirectoryMru
  highlight default link uniteSource__DirectoryMru_Time Statement
endfunction"}}}
function! s:source.hooks.on_post_filter(args, context)"{{{
  for l:mru in a:context.candidates
    let l:relative_path = unite#util#substitute_path_separator(fnamemodify(l:mru.action__path, ':~:.'))
    if l:relative_path == ''
      let l:relative_path = l:mru.action__path
    endif
    if l:relative_path !~ '/$'
      let l:relative_path .= '/'
    endif

    let l:mru.abbr = strftime(g:unite_source_directory_mru_time_format, l:mru.source__time)
          \ . l:relative_path
  endfor
endfunction"}}}

function! s:source.gather_candidates(args, context)"{{{
  call s:load()
  return s:mru_dirs
endfunction"}}}

" Actions"{{{
let s:action_table = {}

let s:action_table.delete = {
      \ 'description' : 'delete from directory_mru list',
      \ 'is_invalidate_cache' : 1,
      \ 'is_quit' : 0,
      \ 'is_selectable' : 1,
      \ }
function! s:action_table.delete.func(candidates)"{{{
  for l:candidate in a:candidates
    call filter(s:mru_dirs, 'v:val.action__path !=# l:candidate.action__path')
  endfor

  call s:save()
endfunction"}}}

let s:source.action_table.directory = s:action_table
"}}}

" Misc
function! s:save()  "{{{
  call writefile([s:VERSION] + map(copy(s:mru_dirs), 'join(s:convert2list(v:val), "\t")'),
  \              g:unite_source_directory_mru_file)
  let s:mru_file_mtime = getftime(g:unite_source_directory_mru_file)
endfunction"}}}
function! s:load()  "{{{
  if filereadable(g:unite_source_directory_mru_file)
  \  && s:mru_file_mtime != getftime(g:unite_source_directory_mru_file)
    let [ver; s:mru_dirs] = readfile(g:unite_source_directory_mru_file)

    if ver !=# s:VERSION
      call unite#util#print_error('Sorry, the version of MRU file is old.  Clears the MRU list.')
      let s:mru_dirs = []
      return
    endif

    try
      let s:mru_dirs = map(s:mru_dirs[: g:unite_source_directory_mru_limit - 1],
            \              's:convert2dictionary(split(v:val, "\t"))')
    catch
      call unite#util#print_error('Sorry, MRU file is invalid.  Clears the MRU list.')
      let s:mru_dirs = []
      return
    endtry

    let s:mru_dirs = filter(s:mru_dirs, 'isdirectory(v:val.action__path)')

    let s:mru_file_mtime = getftime(g:unite_source_directory_mru_file)
  endif
endfunction"}}}
function! s:convert2dictionary(list)  "{{{
  return {
        \ 'word' : unite#util#substitute_path_separator(a:list[0]),
        \ 'kind' : 'directory',
        \ 'source__time' : a:list[1],
        \ 'action__path' : unite#util#substitute_path_separator(a:list[0]),
        \ 'action__directory' : unite#util#substitute_path_separator(a:list[0]),
        \   }
endfunction"}}}
function! s:convert2list(dict)  "{{{
  return [ a:dict.action__path, a:dict.source__time ]
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/sources/file.vim	[[[1
155
"=============================================================================
" FILE: file.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 05 Aug 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

" Variables  "{{{
call unite#util#set_default('g:unite_source_file_ignore_pattern',
      \'^\%(/\|\a\+:/\)$\|\%(^\|/\)\.\.\?$\|\~$\|\.\%(o|exe|dll|bak|sw[po]\)$')
"}}}

function! unite#sources#file#define()"{{{
  return s:source
endfunction"}}}

let s:source = {
      \ 'name' : 'file',
      \ 'description' : 'candidates from file list',
      \}

function! s:source.change_candidates(args, context)"{{{
  if !has_key(a:context, 'source__cache') || a:context.is_redraw
        \ || a:context.is_invalidate
    " Initialize cache.
    let a:context.source__cache = {}
  endif

  let l:input_list = filter(split(a:context.input,
        \                     '\\\@<! ', 1), 'v:val !~ "!"')
  let l:input = empty(l:input_list) ? '' : l:input_list[0]
  let l:input = substitute(substitute(a:context.input, '\\ ', ' ', 'g'), '^\a\+:\zs\*/', '/', '')

  if l:input !~ '^\%(/\|\a\+:/\)' && get(a:args, 0) != ''
    let l:input = a:args[0] . '/' .  l:input
  endif
  let l:is_relative_path = l:input !~ '^\%(/\|\a\+:/\)' && get(a:args, 0) == ''

  " Substitute *. -> .* .
  let l:input = substitute(l:input, '\*\.', '.*', 'g')

  if l:input !~ '\*' && unite#is_win() && getftype(l:input) == 'link'
    " Resolve link.
    let l:input = resolve(l:input)
  endif

  " Glob by directory name.
  let l:input = substitute(l:input, '[^/.]*$', '', '')
  let l:glob = l:input . (l:input =~ '\*$' ? '' : '*')
  if !has_key(a:context.source__cache, l:glob)
    let l:files = split(unite#util#substitute_path_separator(
          \ glob(l:glob)), '\n')

    if g:unite_source_file_ignore_pattern != ''
      call filter(l:files, 'v:val !~ ' . string(g:unite_source_file_ignore_pattern))
    endif

    let a:context.source__cache[l:glob] =
          \ map(sort(l:files, 's:compare_file'), 's:create_dict(v:val, l:is_relative_path)')
  endif

  let l:candidates = a:context.source__cache[l:glob]

  if a:context.input != ''
    let l:newfile = substitute(a:context.input, '[*\\]', '', 'g')
    if !filereadable(l:newfile) && !isdirectory(l:newfile)
      " Add newfile candidate.
      let l:candidates = copy(l:candidates) +
            \ [s:create_dict(l:newfile, l:is_relative_path)]
    endif

    if l:input !~ '^\%(/\|\a\+:/\)$'
      let l:parent = substitute(l:input, '[*\\]\|\.[^/]*$', '', 'g')

      if a:context.input =~ '\.$' && isdirectory(l:parent . '..')
        " Add .. directory.
        let l:candidates = [s:create_dict(l:parent . '..', l:is_relative_path)]
              \ + copy(l:candidates)
      endif
    endif
  endif

  return l:candidates
endfunction"}}}
function! s:create_dict(file, is_relative_path)"{{{
  let l:dict = {
        \ 'word' : a:file,
        \ 'abbr' : a:file, 'source' : 'file',
        \ 'action__path' : unite#util#substitute_path_separator(fnamemodify(a:file, ':p')),
        \}
  let l:dict.action__directory = a:is_relative_path ?
        \ unite#util#substitute_path_separator(
        \    fnamemodify(unite#util#path2directory(a:file), ':.')) :
        \ unite#util#path2directory(l:dict.action__path)

  if isdirectory(a:file)
    if a:file !~ '^\%(/\|\a\+:/\)$'
      let l:dict.abbr .= '/'
    endif

    let l:dict.kind = 'directory'
  else
    if !filereadable(a:file)
      " New file.
      let l:dict.abbr = '[new file]' . a:file
    endif

    let l:dict.kind = 'file'
  endif

  return l:dict
endfunction"}}}
function! s:compare_file(a, b)"{{{
  return isdirectory(a:b) - isdirectory(a:a)
endfunction"}}}

" Add custom action table."{{{
let s:cdable_action_file = {
      \ 'description' : 'open this directory by file source',
      \}

function! s:cdable_action_file.func(candidate)
  call unite#start([['file', a:candidate.action__directory]])
endfunction

call unite#custom_action('cdable', 'file', s:cdable_action_file)
unlet! s:cdable_action_file
"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/sources/file_mru.vim	[[[1
182
"=============================================================================
" FILE: file_mru.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 02 Aug 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

" Variables  "{{{
" The version of MRU file format.
let s:VERSION = '0.2.0'

" [[full_path, localtime()], ... ]
let s:mru_files = []

let s:mru_file_mtime = 0  " the last modified time of the mru file.

call unite#util#set_default('g:unite_source_file_mru_time_format', '(%c) ')
call unite#util#set_default('g:unite_source_file_mru_filename_format', ':~:.')
call unite#util#set_default('g:unite_source_file_mru_file',  g:unite_data_directory . '/.file_mru')
call unite#util#set_default('g:unite_source_file_mru_limit', 100)
call unite#util#set_default('g:unite_source_file_mru_ignore_pattern',
      \'\~$\|\.\%(o|exe|dll|bak|sw[po]\)$\|\%(^\|/\)\.\%(hg\|git\|bzr\|svn\)\%($\|/\)\|^\%(\\\\\|/mnt/\|/media/\|/Volumes/\)')
"}}}

function! unite#sources#file_mru#define()"{{{
  return s:source
endfunction"}}}
function! unite#sources#file_mru#_append()"{{{
  let l:path = unite#util#substitute_path_separator(
        \ simplify(resolve(expand('%:p'))))

  " Append the current buffer to the mru list.
  if !s:is_exists_path(path) || &l:buftype =~ 'help'
  \   || (g:unite_source_file_mru_ignore_pattern != ''
  \      && l:path =~# g:unite_source_file_mru_ignore_pattern)
    return
  endif

  call s:load()

  let l:save_ignorecase = &ignorecase
  let &ignorecase = unite#is_win()

  call insert(filter(s:mru_files, 'v:val.action__path != l:path'),
  \           s:convert2dictionary([l:path, localtime()]))

  let &ignorecase = l:save_ignorecase

  if g:unite_source_file_mru_limit > len(s:mru_files)
    let s:mru_files = s:mru_files[ : g:unite_source_file_mru_limit - 1]
  endif

  call s:save()
endfunction"}}}

let s:source = {
      \ 'name' : 'file_mru',
      \ 'description' : 'candidates from file MRU list',
      \ 'max_candidates' : 30,
      \ 'hooks' : {},
      \ 'action_table' : {},
      \ 'syntax' : 'uniteSource__FileMru',
      \}

function! s:source.hooks.on_syntax(args, context)"{{{
  syntax match uniteSource__FileMru_Time /(.*)/ contained containedin=uniteSource__FileMru
  highlight default link uniteSource__FileMru_Time Statement
endfunction"}}}
function! s:source.hooks.on_post_filter(args, context)"{{{
  for l:mru in a:context.candidates
    let l:path = (g:unite_source_file_mru_filename_format == '') ?
          \ l:mru.action__path :
          \ unite#util#substitute_path_separator(
          \     fnamemodify(l:mru.action__path, g:unite_source_file_mru_filename_format))
    if l:path == ''
      let l:path = l:mru.action__path
    endif
    let l:mru.abbr = (g:unite_source_file_mru_time_format == '' ? '' :
          \ strftime(g:unite_source_file_mru_time_format, l:mru.source__time)) .l:path
  endfor
endfunction"}}}

function! s:source.gather_candidates(args, context)"{{{
  call s:load()

  return s:mru_files
endfunction"}}}

" Actions"{{{
let s:action_table = {}

let s:action_table.delete = {
      \ 'description' : 'delete from file_mru list',
      \ 'is_invalidate_cache' : 1,
      \ 'is_quit' : 0,
      \ 'is_selectable' : 1,
      \ }
function! s:action_table.delete.func(candidates)"{{{
  for l:candidate in a:candidates
    call filter(s:mru_files, 'v:val.action__path !=# l:candidate.action__path')
  endfor

  call s:save()
endfunction"}}}

let s:source.action_table['*'] = s:action_table
"}}}

" Misc
function! s:save()  "{{{
  call writefile([s:VERSION] + map(copy(s:mru_files), 'join(s:convert2list(v:val), "\t")'),
  \              g:unite_source_file_mru_file)
  let s:mru_file_mtime = getftime(g:unite_source_file_mru_file)
endfunction"}}}
function! s:load()  "{{{
  if filereadable(g:unite_source_file_mru_file)
  \  && s:mru_file_mtime != getftime(g:unite_source_file_mru_file)
    let [ver; s:mru_files] = readfile(g:unite_source_file_mru_file)

    if ver !=# s:VERSION
      call unite#util#print_error('Sorry, the version of MRU file is old.  Clears the MRU list.')
      let s:mru_files = []
      return
    endif

    try
      let s:mru_files = map(s:mru_files[: g:unite_source_file_mru_limit - 1],
            \              's:convert2dictionary(split(v:val, "\t"))')
    catch
      call unite#util#print_error('Sorry, MRU file is invalid.  Clears the MRU list.')
      let s:mru_files = []
      return
    endtry

    let s:mru_files = filter(s:mru_files, 's:is_exists_path(v:val.action__path)')

    let s:mru_file_mtime = getftime(g:unite_source_file_mru_file)
  endif
endfunction"}}}
function! s:is_exists_path(path)  "{{{
  return getftype(a:path) != ''
endfunction"}}}
function! s:convert2dictionary(list)  "{{{
  let l:path = unite#util#substitute_path_separator(a:list[0])
  return {
        \ 'word' : l:path,
        \ 'kind' : (isdirectory(l:path) ? 'directory' : 'file'),
        \ 'source__time' : a:list[1],
        \ 'action__path' : l:path,
        \ 'action__directory' : unite#util#path2directory(l:path),
        \   }
endfunction"}}}
function! s:convert2list(dict)  "{{{
  return [ a:dict.action__path, a:dict.source__time ]
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/sources/file_point.vim	[[[1
73
"=============================================================================
" FILE: file_point.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 10 Aug 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#sources#file_point#define()"{{{
  return s:source
endfunction"}}}

let s:source = {
      \ 'name' : 'file_point',
      \ 'description' : 'file candidate from cursor point',
      \ 'hooks' : {},
      \}
function! s:source.hooks.on_init(args, context)"{{{
  let l:filename_pattern = '[[:alnum:];/?:@&=+$,_.!~*''|()-]\+'
  let l:filename = expand(matchstr(getline('.')[: col('.')-1], l:filename_pattern . '$')
        \ . matchstr(getline('.')[col('.') :], '^'.l:filename_pattern))
  let a:context.source__filename =
        \ (l:filename =~ '^\%(https\?\|ftp\)://') ?
        \ l:filename : fnamemodify(l:filename, ':p')
endfunction"}}}

function! s:source.gather_candidates(args, context)"{{{
  if a:context.source__filename =~ '^\%(https\?\|ftp\)://'
    " URI.
    return [{
          \   'word' : a:context.source__filename,
          \   'kind' : 'uri',
          \   'action__path' : a:context.source__filename,
          \ }]
  elseif filereadable(a:context.source__filename)
    return [{
          \   'word' : a:context.source__filename,
          \   'kind' : 'file',
          \   'action__path' : a:context.source__filename,
          \   'action__directory' : unite#util#path2directory(
          \               a:context.source__filename),
          \ }]
  else
    " File not found.
    return []
  endif
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/sources/file_rec.vim	[[[1
320
"=============================================================================
" FILE: file_rec.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 07 Aug 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

" Variables  "{{{
call unite#util#set_default('g:unite_source_file_rec_ignore_pattern',
      \'\%(^\|/\)\.$\|\~$\|\.\%(o|exe|dll|bak|sw[po]\)$\|\%(^\|/\)\.\%(hg\|git\|bzr\|svn\)\%($\|/\)')
call unite#util#set_default('g:unite_source_file_rec_min_cache_files', 50)
"}}}

function! unite#sources#file_rec#define()"{{{
  return [ s:source_rec ]
        \ + [ executable('ls') && unite#util#has_vimproc() ? s:source_async : {} ]
endfunction"}}}

let s:continuation = {}

" Source rec.
let s:source_rec = {
      \ 'name' : 'file_rec',
      \ 'description' : 'candidates from directory by recursive',
      \ 'hooks' : {},
      \ 'max_candidates' : 50,
      \ }

function! s:source_rec.gather_candidates(args, context)"{{{
  let l:directory = s:get_path(a:args, a:context)

  call unite#print_message('[file_rec] directory: ' . l:directory)

  call s:init_continuation(a:context, l:directory)

  let l:continuation = s:continuation[l:directory]

  let a:context.source__directory = l:directory

  if empty(l:continuation.rest) || l:continuation.end
    " Disable async.
    call unite#print_message('[file_rec] Directory traverse was completed.')
    let a:context.is_async = 0
  endif

  return l:continuation.files
endfunction"}}}

function! s:source_rec.async_gather_candidates(args, context)"{{{
  let l:continuation = s:continuation[a:context.source__directory]

  let [l:continuation.rest, l:files] = s:get_files(l:continuation.rest, 1, 20)

  if empty(l:continuation.rest)
    call unite#print_message('[file_rec] Directory traverse was completed.')

    " Disable async.
    let a:context.is_async = 0
    let l:continuation.end = 1
  endif

  let l:candidates = map(l:files, '{
        \ "word" : v:val, "action__path" : v:val,
        \ }')

  let l:continuation.files += l:candidates

  return l:candidates
endfunction"}}}

function! s:source_rec.hooks.on_post_filter(args, context)"{{{
  call s:on_post_filter(a:args, a:context)
endfunction"}}}

" Source async.
let s:source_async = {
      \ 'name' : 'file_rec/async',
      \ 'description' : 'asyncronous candidates from directory by recursive',
      \ 'hooks' : {},
      \ 'max_candidates' : 50,
      \ }

function! s:source_async.gather_candidates(args, context)"{{{
  let l:directory = s:get_path(a:args, a:context)

  call unite#print_message('[file_rec/async] directory: ' . l:directory)

  call s:init_continuation(a:context, l:directory)

  let l:continuation = s:continuation[l:directory]

  let a:context.source__directory = l:directory

  if empty(l:continuation.rest) || l:continuation.end
    " Disable async.
    call unite#print_message('[file_rec/async] Directory traverse was completed.')
    let a:context.is_async = 0

    return l:continuation.files
  endif

  let a:context.source__proc = vimproc#pgroup_open('ls -R1 '
        \ . escape(l:directory, ' '))

  " Close handles.
  call a:context.source__proc.stdin.close()
  call a:context.source__proc.stderr.close()

  return []
endfunction"}}}

function! s:source_async.async_gather_candidates(args, context)"{{{
  let l:continuation = s:continuation[a:context.source__directory]

  let l:stdout = a:context.source__proc.stdout
  if l:stdout.eof
    " Disable async.
    call unite#print_message('[file_rec] Directory traverse was completed.')
    let a:context.is_async = 0
    let l:continuation.end = 1
  endif

  let l:candidates = []
  for l:line in map(l:stdout.read_lines(-1, 300),
        \ 'iconv(v:val, &termencoding, &encoding)')
    if l:line =~ ':$'
      " Directory name.
      let l:continuation.directory = l:line[: -2]
      if l:continuation.directory !~ '/$'
        let l:continuation.directory .= '/'
      endif
    elseif l:line != ''
          \ && (g:unite_source_file_rec_ignore_pattern == ''
          \      || l:line !~ g:unite_source_file_rec_ignore_pattern)
      call add(l:candidates, {
            \ 'word' : l:continuation.directory . l:line,
            \ 'action__path' : l:continuation.directory . l:line,
            \ })
    endif
  endfor

  let l:continuation.files += l:candidates

  return l:candidates
endfunction"}}}

function! s:source_async.hooks.on_close(args, context) "{{{
  if has_key(a:context, 'source__proc')
    call a:context.source__proc.waitpid()
  endif
endfunction "}}}
function! s:source_async.hooks.on_post_filter(args, context)"{{{
  call s:on_post_filter(a:args, a:context)
endfunction"}}}

" Add custom action table."{{{
let s:cdable_action_rec = {
      \ 'description' : 'open this directory by file_rec source',
      \}

function! s:cdable_action_rec.func(candidate)
  call unite#start([['file_rec', a:candidate.action__directory]])
endfunction

let s:cdable_action_rec_async = {
      \ 'description' : 'open this directory by file_rec/async source',
      \}

function! s:cdable_action_rec_async.func(candidate)
  call unite#start([['file_rec/async', a:candidate.action__directory]])
endfunction

call unite#custom_action('cdable', 'rec', s:cdable_action_rec)
call unite#custom_action('cdable', 'rec/async', s:cdable_action_rec_async)
unlet! s:cdable_action_rec
unlet! s:cdable_action_rec_async
"}}}

" Misc.
function! s:get_path(args, context)"{{{
  let l:directory = get(a:args, 0, '')
  if l:directory == ''
    let l:directory = isdirectory(a:context.input) ?
          \ a:context.input : getcwd()
  endif

  return unite#util#substitute_path_separator(
        \ substitute(fnamemodify(l:directory, ':p'), '^\~',
        \ unite#util#substitute_path_separator($HOME), ''))
endfunction"}}}
function! s:get_files(files, level, max_len)"{{{
  let l:continuation_files = []
  let l:ret_files = []
  let l:files_index = 0
  let l:ret_files_len = 0
  for l:file in a:files
    let l:files_index += 1

    if l:file =~ '/\.\+$'
          \ || (g:unite_source_file_rec_ignore_pattern != '' &&
          \     l:file =~ g:unite_source_file_rec_ignore_pattern)
          \ || isdirectory(l:file) && getftype(l:file) ==# 'link'
      continue
    endif

    if isdirectory(l:file)
      if l:file != '/' && l:file =~ '/$'
        let l:file = l:file[: -2]
      endif

      let l:child_index = 0
      let l:childs = split(unite#util#substitute_path_separator(glob(l:file . '/*')), '\n')
            \ + split(unite#util#substitute_path_separator(glob(l:file . '/.*')), '\n')
      for l:child in l:childs
        let l:child_index += 1

        if l:child =~ '/\.\+$'
              \ ||(g:unite_source_file_rec_ignore_pattern != '' &&
              \     l:child =~ g:unite_source_file_rec_ignore_pattern)
              \ || isdirectory(l:child) && getftype(l:child) ==# 'link'
          continue
        endif

        if isdirectory(l:child)
          if a:level < 5 && l:ret_files_len < a:max_len
            let [l:continuation_files_child, l:ret_files_child] =
                  \ s:get_files([l:child], a:level + 1, a:max_len - l:ret_files_len)
            let l:continuation_files += l:continuation_files_child
            let l:ret_files += l:ret_files_child
          else
            call add(l:continuation_files, l:child)
          endif
        else
          call add(l:ret_files, l:child)

          let l:ret_files_len += 1

          if l:ret_files_len > a:max_len
            let l:continuation_files += l:childs[l:child_index :]
            break
          endif
        endif
      endfor
    else
      call add(l:ret_files, l:file)
      let l:ret_files_len += 1
    endif

    if l:ret_files_len > a:max_len
      break
    endif
  endfor

  let l:continuation_files += a:files[l:files_index :]
  return [l:continuation_files, map(l:ret_files,
        \ 'unite#util#substitute_path_separator(fnamemodify(v:val, ":p"))')]
endfunction"}}}
function! s:on_post_filter(args, context)"{{{
  let l:is_relative_path =
        \ a:context.source__directory == unite#util#substitute_path_separator(getcwd())

  if !l:is_relative_path
    let l:cwd = getcwd()
    lcd `=a:context.source__directory`
  endif

  for l:candidate in a:context.candidates
    let l:candidate.kind = 'file'
    let l:candidate.abbr = unite#util#substitute_path_separator(
          \ fnamemodify(l:candidate.action__path, ':.'))
          \ . (isdirectory(l:candidate.action__path) ? '/' : '')
    let l:candidate.action__directory = l:is_relative_path ?
          \ l:candidate.abbr :
          \ unite#util#path2directory(l:candidate.action__path)
  endfor

  if !l:is_relative_path
    lcd `=l:cwd`
  endif
endfunction"}}}
function! s:init_continuation(context, directory)"{{{
  if a:context.is_redraw
        \ || !has_key(s:continuation, a:directory)
        \ || len(s:continuation[a:directory].files)
        \      < g:unite_source_file_rec_min_cache_files
    let a:context.is_async = 1

    let s:continuation[a:directory] = {
          \ 'files' : [], 'rest' : [a:directory],
          \ 'directory' : a:directory, 'end' : 0,
          \ }
  endif
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/sources/grep.vim	[[[1
179
"=============================================================================
" FILE: grep.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu at gmail.com>
"          Tomohiro Nishimura <tomohiro68 at gmail.com>
" Last Modified: 10 Aug 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

" Variables  "{{{
call unite#util#set_default('g:unite_source_grep_command', 'grep')
call unite#util#set_default('g:unite_source_grep_default_opts', '-Hn')
call unite#util#set_default('g:unite_source_grep_recursive_opt', '-R')
call unite#util#set_default('g:unite_source_grep_max_candidates', 100)
"}}}

" Actions "{{{
let s:action_grep_file = {
  \   'description': 'grep this files',
  \   'is_quit': 1,
  \   'is_invalidate_cache': 1,
  \   'is_selectable': 1,
  \ }
function! s:action_grep_file.func(candidates) "{{{
  call unite#start([['grep', map(copy(a:candidates), 'v:val.action__path')]])
endfunction "}}}

let s:action_grep_directory = {
  \   'description': 'grep this directories',
  \   'is_quit': 1,
  \   'is_invalidate_cache': 1,
  \   'is_selectable': 1,
  \ }
function! s:action_grep_directory.func(candidates) "{{{
  call unite#start([['grep', map(copy(a:candidates), 'v:val.action__directory'), g:unite_source_grep_recursive_opt]])
endfunction "}}}
if executable(g:unite_source_grep_command) && unite#util#has_vimproc()
  call unite#custom_action('file,buffer', 'grep', s:action_grep_file)
  call unite#custom_action('file,buffer', 'grep_directory', s:action_grep_directory)
endif
" }}}

function! unite#sources#grep#define() "{{{
  if !exists('*unite#version') || unite#version() <= 100
    echoerr 'Your unite.vim is too old.'
    echoerr 'Please install unite.vim Ver.1.1 or above.'
    return []
  endif

  return executable(g:unite_source_grep_command) && unite#util#has_vimproc() ? s:grep_source : []
endfunction "}}}

let s:grep_source = {
      \ 'name': 'grep',
      \ 'max_candidates': g:unite_source_grep_max_candidates,
      \ 'hooks' : {},
      \ 'syntax' : 'uniteSource__Grep',
      \ 'filters' : ['matcher_regexp', 'sorter_default', 'converter_default'],
      \ }

function! s:grep_source.hooks.on_init(args, context) "{{{
  let l:target  = get(a:args, 0, '')
  if type(l:target) != type([])
    if l:target == ''
      let l:target = input('Target: ', '**', 'file')
    endif

    if l:target == '%' || l:target == '#'
      let l:target = unite#util#escape_file_searching(bufname(l:target))
    elseif l:target ==# '$buffers'
      let l:target = join(map(filter(range(1, bufnr('$')), 'buflisted(v:val)'),
            \ 'unite#util#escape_file_searching(bufname(v:val))'))
    elseif l:target == '**'
      " Optimized.
      let l:target = '* ' . g:unite_source_grep_recursive_opt
    endif

    let a:context.source__target = [l:target]
  else
    let a:context.source__target = l:target
  endif

  let a:context.source__extra_opts = get(a:args, 1, '')

  let a:context.source__input = get(a:args, 2, '')
  if a:context.source__input == ''
    let a:context.source__input = input('Pattern: ')
  endif

  call unite#print_message('[grep] Target: ' . join(a:context.source__target))
  call unite#print_message('[grep] Pattern: ' . a:context.source__input)
endfunction"}}}
function! s:grep_source.hooks.on_syntax(args, context)"{{{
  syntax case ignore
  execute 'syntax match uniteSource__GrepPattern /:.*\zs'
        \ . substitute(a:context.source__input, '\([/\\]\)', '\\\1', 'g')
        \ . '/ contained containedin=uniteSource__Grep'
  highlight default link uniteSource__GrepPattern Search
endfunction"}}}
function! s:grep_source.hooks.on_close(args, context) "{{{
  if has_key(a:context, 'source__proc')
    call a:context.source__proc.waitpid()
  endif
endfunction "}}}

function! s:grep_source.gather_candidates(args, context) "{{{
  if empty(a:context.source__target)
        \ || a:context.source__input == ''
    let a:context.is_async = 0
    call unite#print_message('[grep] Completed.')
    return []
  endif

  if a:context.is_redraw
    call unite#print_message('[grep] Target: ' . join(a:context.source__target))
    call unite#print_message('[grep] Pattern: ' . a:context.source__input)
    let a:context.is_async = 1
  endif

  let l:cmdline = printf('%s %s ''%s'' %s %s',
    \   g:unite_source_grep_command,
    \   g:unite_source_grep_default_opts,
    \   substitute(a:context.source__input, "'", "''", 'g'),
    \   join(a:context.source__target),
    \   a:context.source__extra_opts)
  call unite#print_message('[grep] Command-line: ' . l:cmdline)
  let a:context.source__proc = vimproc#pgroup_open(l:cmdline)
  " let a:context.source__proc = vimproc#popen3(l:cmdline)

  " Close handles.
  call a:context.source__proc.stdin.close()
  call a:context.source__proc.stderr.close()

  return []
endfunction "}}}

function! s:grep_source.async_gather_candidates(args, context) "{{{
  let l:stdout = a:context.source__proc.stdout
  if l:stdout.eof
    " Disable async.
    call unite#print_message('[grep] Completed.')
    let a:context.is_async = 0
  endif

  let l:candidates = map(filter(map(l:stdout.read_lines(-1, 300),
        \ 'iconv(v:val, &termencoding, &encoding)'),
    \  'v:val =~ "^.\\+:.\\+:.\\+$"'),
    \ '[v:val, split(v:val[2:], ":")]')

  return map(l:candidates,
    \ '{
    \   "word": v:val[0],
    \   "kind": "jump_list",
    \   "action__path": unite#util#substitute_path_separator(
    \                   fnamemodify(v:val[0][:1].v:val[1][0], ":p")),
    \   "action__line": v:val[1][1],
    \   "action__text": join(v:val[1][2:], ":"),
    \ }')
endfunction "}}}

" vim: foldmethod=marker
autoload/unite/sources/jump.vim	[[[1
106
"=============================================================================
" FILE: jump.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 23 Jul 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

" Variables  "{{{
"}}}

function! unite#sources#jump#define()"{{{
  return s:source
endfunction"}}}

let s:source = {
      \ 'name' : 'jump',
      \ 'description' : 'candidates from jumps',
      \ }

let s:cached_result = []
function! s:source.gather_candidates(args, context)"{{{
  " Get jumps list.
  redir => l:redir
  silent! jumps
  redir END

  let l:result = []
  let l:max_path = (winwidth(0) - 5) / 2
  let l:max_text = (winwidth(0) - 5) - l:max_path
  for jump in split(l:redir, '\n')[1:]
    let l:list = split(jump)
    if len(l:list) < 4
      continue
    endif

    let [l:linenr, l:col, l:file_text] = [l:list[1], l:list[2]+1, join(l:list[3:])]
    let l:lines = getbufline(l:file_text, l:linenr)
    let l:path = l:file_text
    let l:bufnr = bufnr(l:file_text)
    if empty(l:lines)
      if getline(l:linenr) ==# l:file_text
        let l:lines = [l:file_text]
        let l:path = bufname('%')
        let l:bufnr = bufnr('%')
      elseif filereadable(l:path)
        let l:bufnr = 0
        let l:lines = ['buffer unloaded']
      else
        " Skip.
        continue
      endif
    endif

    if getbufvar(l:bufnr, '&filetype') ==# 'unite'
      " Skip unite buffer.
      continue
    endif

    let l:text = get(l:lines, 0, '')

    let l:dict = {
          \ 'word' : unite#util#truncate_smart(printf('%s:%d-%d  ', l:path, l:linenr, l:col),
          \           l:max_path, l:max_path/3, '..') .
          \          unite#util#truncate_smart(l:text, l:max_text, l:max_text/3, '..'),
          \ 'kind' : 'jump_list',
          \ 'action__path' : unite#util#substitute_path_separator(fnamemodify(expand(l:path), ':p')),
          \ 'action__line' : l:linenr,
          \ 'action__col' : l:col,
          \ }

    if l:bufnr > 0
      let l:dict.action__buffer_nr = l:bufnr
    endif

    call add(l:result, l:dict)
  endfor

  return reverse(l:result)
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/sources/jump_point.vim	[[[1
82
"=============================================================================
" FILE: jump_point.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 10 Aug 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#sources#jump_point#define()"{{{
  return s:source
endfunction"}}}

let s:source = {
      \ 'name' : 'jump_point',
      \ 'description' : 'candidates from cursor point',
      \ 'hooks' : {},
      \}
function! s:source.hooks.on_init(args, context)"{{{
  let l:line = substitute(getline('.'), '^!!!\|!!!$', '', 'g')
  let a:context.source__lines =
        \ (l:line =~ '^\f\+:') ?  [l:line] : []
endfunction"}}}

function! s:source.gather_candidates(args, context)"{{{
  let l:candidates = []

  for [word, list] in map(a:context.source__lines,
        \ '[v:val, split(v:val[2:], ":")]')
    let l:candidate = {
        \   'word': word,
        \   'kind': 'jump_list',
        \ }
    if len(word) == 1 && unite#util#is_win()
      let l:candidate.word = word . list[0]
      let list = list[1:]
    endif

    let l:candidate.action__path = unite#util#substitute_path_separator(
          \ fnamemodify(word[:1].list[0], ':p'))

    if len(list) >= 1 && list[1] =~ '^\d\+$'
      let l:candidate.action__line = list[1]
      if len(list) >= 2 && list[2] =~ '^\d\+$'
        let l:candidate.action__col = list[2]
      endif
    else
      let l:candidate.action__text = join(list[1:], ':')
      let l:candidate.action__pattern =
            \ unite#escape_match(l:candidate.action__text)
    endif

    call add(l:candidates, l:candidate)
  endfor

  return l:candidates
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/sources/line.vim	[[[1
140
"=============================================================================
" FILE: line.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu at gmail.com>
"          t9md <taqumd at gmail.com>
" Last Modified: 10 Aug 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

" original verion is http://d.hatena.ne.jp/thinca/20101105/1288896674

call unite#util#set_default('g:unite_source_line_enable_highlight', 1)
call unite#util#set_default('g:unite_source_line_search_word_highlight', 'Search')

let s:unite_source = {
            \ 'name' : 'line',
            \ 'syntax' : 'uniteSource__Line',
            \ 'hooks' : {},
            \ 'max_candidates': 100,
            \ 'filters' :
            \    ['matcher_regexp', 'sorter_default', 'converter_default'],
            \ }

function! s:unite_source.hooks.on_init(args, context) "{{{
    execute 'highlight default link uniteSource__Line_target ' . g:unite_source_line_search_word_highlight
    syntax case ignore
    let a:context.source__path = (&l:buftype =~ 'nofile') ?
                \ expand('%:p') : bufname('%')
    let a:context.source__bufnr = bufnr('%')
    let a:context.source__linenr = line('.')
endfunction"}}}
function! s:unite_source.hooks.on_syntax(args, context) "{{{
    call s:hl_refresh(a:context)
endfunction"}}}

function! s:hl_refresh(context)
    syntax clear uniteSource__Line_target
    syntax case ignore
    if a:context.input == '' || !g:unite_source_line_enable_highlight
        return
    endif

    for word in split(a:context.input, '\\\@<! ')
        execute "syntax match uniteSource__Line_target '"
          \ . unite#escape_match(word)
          \ . "' contained containedin=uniteSource__Line"
    endfor
endfunction

let s:supported_search_direction = ['forward', 'backward', 'all']
function! s:unite_source.gather_candidates(args, context)
    let direction = get(a:args, 0, '')
    if direction == ''
        let direction = 'all'
    endif

    if index(s:supported_search_direction, direction) == -1
        let direction = 'all'
    endif

    if direction !=# 'all'
        call unite#print_message('[line] direction: ' . direction)
    endif

    let [start, end] =
                \ direction ==# 'forward' ?
                \ [a:context.source__linenr, '$'] :
                \ direction ==# 'backward' ?
                \ [1, a:context.source__linenr] :
                \ [1, '$']

    let lines = map(getbufline(a:context.source__bufnr, start, end),
                \ '{"nr": v:key+start, "val": v:val }')
    let a:context.source__format = '%' . strlen(len(lines)) . 'd: %s'

    return map(lines, '{
                \   "word": v:val.val,
                \   "action__line": v:val.nr,
                \   "action__text": v:val.val
                \ }')
endfunction

function! s:unite_source.hooks.on_post_filter(args, context)
    call s:hl_refresh(a:context)

    for l:candidate in a:context.candidates
        let l:candidate.kind = "jump_list"
        let l:candidate.abbr = printf(a:context.source__format,
                    \ l:candidate.action__line, l:candidate.action__text)
        let l:candidate.action__buffer_nr = a:context.source__bufnr
        let l:candidate.action__path = a:context.source__path
    endfor
endfunction
function! s:on_post_filter(args, context)"{{{
  let l:is_relative_path =
        \ a:context.source__directory == unite#util#substitute_path_separator(getcwd())

  if !l:is_relative_path
    let l:cwd = getcwd()
    lcd `=a:context.source__directory`
  endif

  for l:candidate in a:context.candidates
    let l:candidate.kind = 'file'
    let l:candidate.abbr = unite#util#substitute_path_separator(
          \ fnamemodify(l:candidate.action__path, ':.'))
          \ . (isdirectory(l:candidate.action__path) ? '/' : '')
    let l:candidate.action__directory = l:is_relative_path ?
          \ l:candidate.abbr :
          \ unite#util#path2directory(l:candidate.action__path)
  endfor

  if !l:is_relative_path
    lcd `=l:cwd`
  endif
endfunction"}}}

function! unite#sources#line#define() "{{{
  return s:unite_source
endfunction "}}}

" vim: expandtab:ts=4:sts=4:sw=4
autoload/unite/sources/mapping.vim	[[[1
73
"=============================================================================
" FILE: mapping.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 27 Apr 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

" Variables  "{{{
"}}}

function! unite#sources#mapping#define()"{{{
  return s:source
endfunction"}}}

let s:source = {
      \ 'name' : 'mapping',
      \ 'description' : 'candidates from Vim mappings',
      \ 'max_candidates' : 30,
      \ 'hooks' : {},
      \ }

let s:cached_result = []
function! s:source.hooks.on_init(args, context)"{{{
  " Get mapping list.
  redir => l:redir
  silent! nmap
  redir END

  let s:cached_result = []
  for line in split(l:redir, '\n')
    let l:map = matchstr(line, '^\a*\s*\zs\S\+')
    if l:map !~ '^<' || l:map =~ '^<SNR>'
      continue
    endif
    let l:map = substitute(l:map, '\(<.*>\)', '\\\1', 'g')

    call add(s:cached_result, {
          \ 'word' : l:line,
          \ 'kind' : 'command',
          \ 'action__command' : 'execute "normal ' . l:map . '"',
          \ })
  endfor
endfunction"}}}
function! s:source.gather_candidates(args, context)"{{{
  return s:cached_result
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/sources/output.vim	[[[1
62
"=============================================================================
" FILE: output.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 11 Jul 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

" Variables  "{{{
"}}}

function! unite#sources#output#define()"{{{
  return s:source
endfunction"}}}

let s:source = {
      \ 'name' : 'output',
      \ 'description' : 'candidates from Vim command output',
      \ 'default_action' : 'yank',
      \ }

function! s:source.gather_candidates(args, context)"{{{
  let l:command = get(a:args, 0)
  if l:command == ''
    let l:command = input('Please input Vim command: ', '', 'command')
  endif

  redir => l:result
  silent execute l:command
  redir END

  return map(split(l:result, '\r\n\|\n'), '{
        \ "word" : v:val,
        \ "kind" : "word",
        \ }')
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/sources/register.vim	[[[1
77
"=============================================================================
" FILE: register.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 03 Jun 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#sources#register#define()"{{{
  return s:source
endfunction"}}}

let s:source = {
      \ 'name' : 'register',
      \ 'description' : 'candidates from register',
      \}

function! s:source.gather_candidates(args, context)"{{{
  let l:candidates = []

  let l:max_width = winwidth(0) - 5
  let l:registers = [['"', @"],
        \ ['0', @0], ['1', @1], ['2', @2], ['3', @3], ['4', @4],
        \ ['5', @5], ['6', @6], ['7', @7], ['8', @8], ['9', @9],
        \ ['a', @a], ['b', @b], ['c', @c], ['d', @d], ['e', @e],
        \ ['f', @f], ['g', @g], ['h', @h], ['i', @i], ['j', @j],
        \ ['k', @k], ['l', @l], ['m', @m], ['n', @n], ['o', @o],
        \ ['p', @p], ['q', @q], ['r', @r], ['s', @s], ['t', @t],
        \ ['u', @u], ['v', @v], ['w', @w], ['x', @x], ['y', @y], ['z', @z],
        \ ['-', @-], ['*', @*], ['+', @+], ['.', @.], [':', @:],
        \ ['%', @%], ['#', @#], ['/', @/], ['=', @=],
        \ ]
  if exists('g:yanktmp_file') && filereadable(g:yanktmp_file)
    call add(l:registers, ['yanktmp', join(readfile(g:yanktmp_file, "b"), "\n")])
  endif

  for [l:reg, l:register] in l:registers
    if l:register != ''
      let l:abbr = substitute(l:register[ : l:max_width], '\t', '>---', 'g')
      let l:abbr = substitute(l:abbr, '\r\?\n', '\\n', 'g')

      call add(l:candidates, {
            \ 'word' : l:register,
            \ 'abbr' : printf('%-7s - %s', l:reg, l:abbr),
            \ 'kind' : 'word',
            \ })
    endif
  endfor

  return l:candidates
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/sources/resume.vim	[[[1
66
"=============================================================================
" FILE: resume.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 02 Jul 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#sources#resume#define()"{{{
  return s:source
endfunction"}}}

let s:source = {
      \ 'name' : 'resume',
      \ 'description' : 'candidates from resume list',
      \}

function! s:source.gather_candidates(args, context)"{{{
  let a:context.source__buffer_list = filter(range(1, bufnr('$')),
        \ 'getbufvar(v:val, "&filetype") ==# "unite" && !getbufvar(v:val, "unite").context.temporary')

  let l:max_width = max(map(copy(a:context.source__buffer_list),
        \ 'len(getbufvar(v:val, "unite").buffer_name)'))
  let l:candidates = map(copy(a:context.source__buffer_list), '{
        \ "word" : getbufvar(v:val, "unite").buffer_name,
        \ "abbr" : printf("%-".l:max_width."s : "
        \          . join(map(copy(getbufvar(v:val, "unite").sources), "v:val.name"), ", "),
        \            getbufvar(v:val, "unite").buffer_name),
        \ "kind" : "command",
        \ "action__command" : "UniteResume " . getbufvar(v:val, "unite").buffer_name,
        \ "source__time" : getbufvar(v:val, "unite").access_time,
        \}')

  return sort(l:candidates, 's:compare')
endfunction"}}}

" Misc.
function! s:compare(candidate_a, candidate_b)"{{{
  return a:candidate_b.source__time - a:candidate_a.source__time
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/sources/source.vim	[[[1
59
"=============================================================================
" FILE: source.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 03 Aug 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#sources#source#define()"{{{
  return s:source
endfunction"}}}

let s:source = {
      \ 'name' : 'source',
      \ 'description' : 'candidates from sources list',
      \ 'action_table' : {},
      \ 'default_action' : 'start',
      \}

function! s:source.gather_candidates(args, context)"{{{
  return map(sort(values(unite#get_sources()), 's:compare_sources'), '{
        \ "word" : v:val.name,
        \ "abbr" : unite#util#truncate(v:val.name, 25) . (v:val.description != "" ? " -- " . v:val.description : ""),
        \ "kind" : "source",
        \ "action__source_name" : v:val.name,
        \ "action__source_args" : [],
        \}')
endfunction"}}}

function! s:compare_sources(source_a, source_b) "{{{
  return a:source_a.name ==# a:source_b.name ? 0 :
  \      a:source_a.name >#  a:source_b.name ? 1 : -1
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/sources/tab.vim	[[[1
126
"=============================================================================
" FILE: tab.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 17 Jul 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#sources#tab#define()"{{{
  return s:source
endfunction"}}}
function! unite#sources#tab#_append()"{{{
  if exists('*gettabvar')
    " Save tab access time.
    let t:unite_tab_access_time = localtime()
  endif
endfunction"}}}

let s:source = {
      \ 'name' : 'tab',
      \ 'description' : 'candidates from tab list',
      \}

function! s:source.gather_candidates(args, context)"{{{
  let l:list = range(1, tabpagenr('$'))
  unlet l:list[tabpagenr()-1]
  if exists('*gettabvar')
    call sort(l:list, 's:compare')
  endif
  let l:arg = get(a:args, 0, '')
  if l:arg !=# 'no-current'
    " Add current tab.
    call add(l:list, tabpagenr())
  endif

  let l:candidates = []
  for i in l:list
    let l:bufnrs = tabpagebuflist(i)
    let l:bufnr = l:bufnrs[tabpagewinnr(i) - 1]  " Get current window buffer in tabs.

    let l:bufname = unite#substitute_path_separator(fnamemodify((i == tabpagenr() ? bufname('#') : bufname(l:bufnr)), ':p'))
    if l:bufname == ''
      let l:bufname = '[No Name]'
    endif

    if exists('*gettabvar')
      " Use gettabvar().
      let l:title = gettabvar(i, 'title')
      if l:title != ''
        let l:title = '[' . l:title . ']'
      endif

      let l:cwd = unite#substitute_path_separator((i == tabpagenr() ? getcwd() : gettabvar(i, 'cwd')))
      if l:cwd !~ '/$'
        let l:cwd .= '/'
      endif
    else
      let l:title = ''
      let l:cwd = ''
    endif

    let l:abbr = i . ': ' . l:title
    if l:cwd != ''
      if stridx(l:bufname, l:cwd) == 0
        let l:bufname = l:bufname[len(l:cwd) :]
      endif
      let l:abbr .= l:bufname

      let l:abbr .= '(' . substitute(l:cwd, '.\zs/$', '', '') . ')'
    else
      let l:abbr .= l:bufname
    endif

    let l:wincount = tabpagewinnr(i, '$')
    if i == tabpagenr()
      let l:wincount -= 1
    endif
    if l:wincount > 1
      let l:abbr .= '{' . l:wincount . '}'
    endif
    let l:abbr .= getbufvar(bufnr('%'), '&modified') ? '[+]' : ''

    let l:word = exists('*gettabvar') && gettabvar(i, 'title') != '' ? gettabvar(i, 'title') : l:bufname

    call add(l:candidates, {
          \ 'word' : l:word,
          \ 'abbr' : l:abbr,
          \ 'kind' : 'tab',
          \ 'action__tab_nr' : i,
          \ 'action__directory' : l:cwd,
          \ })
  endfor

  return l:candidates
endfunction"}}}

" Misc
function! s:compare(candidate_a, candidate_b)"{{{
  return gettabvar(a:candidate_b, 'unite_tab_access_time') - gettabvar(a:candidate_a, 'unite_tab_access_time')
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/sources/window.vim	[[[1
103
"=============================================================================
" FILE: window.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 17 Jul 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#sources#window#define()"{{{
  return s:source
endfunction"}}}
function! unite#sources#window#_append()"{{{
  if &filetype == 'unite'
    " Ignore unite window.
    return
  endif

  " Save unite window information.
  let w:unite_window = {
        \ 'time' : localtime(),
        \ 'cwd' : getcwd(),
        \}
endfunction"}}}

let s:source = {
      \ 'name' : 'window',
      \ 'description' : 'candidates from window list',
      \ 'hooks' : {},
      \}

function! s:source.hooks.on_init(args, context)"{{{
  let l:list = range(1, winnr('$'))
  for i in l:list
    " Set default value.
    if type(getwinvar(i, 'unite_window')) == type('')
      call setwinvar(i, 'unite_window', {
            \ 'time' : 0,
            \ 'cwd' : getcwd(),
            \ })
    endif
  endfor

  unlet l:list[winnr()-1]
  call sort(l:list, 's:compare')
  let l:arg = get(a:args, 0, '')
  if l:arg !=# 'no-current'
    " Add current window.
    call add(l:list, winnr())
  endif

  let a:context.source__candidates = []
  for i in l:list
    let l:window = getwinvar(i, 'unite_window')
    let l:bufname = bufname(winbufnr(i))
    if empty(l:bufname)
      let l:bufname = '[No Name]'
    endif

    call add(a:context.source__candidates, {
          \ 'word' : l:bufname,
          \ 'abbr' : printf('[%d/%d] %s %s(%s)', i, winnr('$'),
          \      (i == winnr() ? '%' : i == winnr('#') ? '#' : ' '),
          \      l:bufname, l:window.cwd),
          \ 'kind' : 'window',
          \ 'action__window_nr' : i,
          \ 'action__directory' : l:window.cwd,
          \ })
  endfor
endfunction"}}}
function! s:source.gather_candidates(args, context)"{{{
  return a:context.source__candidates
endfunction"}}}

" Misc
function! s:compare(candidate_a, candidate_b)"{{{
  return getwinvar(a:candidate_b, 'unite_window').time - getwinvar(a:candidate_a, 'unite_window').time
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/unite/util.vim	[[[1
78
let s:save_cpo = &cpo
set cpo&vim

let s:V = vital#of('unite')
call s:V.load('Data.List')
function! unite#util#truncate_smart(...)
  return call(s:V.truncate_smart, a:000)
endfunction
function! unite#util#truncate(...)
  return call(s:V.truncate, a:000)
endfunction
function! unite#util#strchars(...)
  return call(s:V.strchars, a:000)
endfunction
function! unite#util#strwidthpart(...)
  return call(s:V.strwidthpart, a:000)
endfunction
function! unite#util#strwidthpart_reverse(...)
  return call(s:V.strwidthpart_reverse, a:000)
endfunction
function! unite#util#wcswidth(...)
  return call(s:V.wcswidth, a:000)
endfunction
function! unite#util#wcswidth(...)
  return call(s:V.wcswidth, a:000)
endfunction
function! unite#util#is_win(...)
  return call(s:V.is_windows, a:000)
endfunction
function! unite#util#is_mac(...)
  return call(s:V.is_mac, a:000)
endfunction
function! unite#util#print_error(...)
  return call(s:V.print_error, a:000)
endfunction
function! unite#util#smart_execute_command(...)
  return call(s:V.smart_execute_command, a:000)
endfunction
function! unite#util#escape_file_searching(...)
  return call(s:V.escape_file_searching, a:000)
endfunction
function! unite#util#escape_pattern(...)
  return call(s:V.escape_pattern, a:000)
endfunction
function! unite#util#set_default(...)
  return call(s:V.set_default, a:000)
endfunction
function! unite#util#set_dictionary_helper(...)
  return call(s:V.set_dictionary_helper, a:000)
endfunction
function! unite#util#substitute_path_separator(...)
  return call(s:V.substitute_path_separator, a:000)
endfunction
function! unite#util#path2directory(...)
  return call(s:V.path2directory, a:000)
endfunction
function! unite#util#path2project_directory(...)
  return call(s:V.path2project_directory, a:000)
endfunction
function! unite#util#has_vimproc(...)
  return call(s:V.has_vimproc, a:000)
endfunction
function! unite#util#system(...)
  return call(s:V.system, a:000)
endfunction
function! unite#util#get_last_status(...)
  return call(s:V.get_last_status, a:000)
endfunction
function! unite#util#sort_by(...)
  return call(s:V.Data.List.sort_by, a:000)
endfunction
function! unite#util#uniq(...)
  return call(s:V.Data.List.uniq, a:000)
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo

autoload/unite/vital.vim	[[[1
66
let s:save_cpo = &cpo
set cpo&vim

function! unite#vital#truncate_smart(...)
  return call('vital#_39a315#truncate_smart', a:000)
endfunction
function! unite#vital#truncate(...)
  return call('vital#_39a315#truncate', a:000)
endfunction
function! unite#vital#strchars(...)
  return call('vital#_39a315#strchars', a:000)
endfunction
function! unite#vital#strwidthpart(...)
  return call('vital#_39a315#strwidthpart', a:000)
endfunction
function! unite#vital#strwidthpart_reverse(...)
  return call('vital#_39a315#strwidthpart_reverse', a:000)
endfunction
function! unite#vital#wcswidth(...)
  return call('vital#_39a315#wcswidth', a:000)
endfunction
function! unite#vital#wcswidth(...)
  return call('vital#_39a315#wcswidth', a:000)
endfunction
function! unite#vital#is_win(...)
  return call('vital#_39a315#is_win', a:000)
endfunction
function! unite#vital#print_error(...)
  return call('vital#_39a315#print_error', a:000)
endfunction
function! unite#vital#smart_execute_command(...)
  return call('vital#_39a315#smart_execute_command', a:000)
endfunction
function! unite#vital#escape_file_searching(...)
  return call('vital#_39a315#escape_file_searching', a:000)
endfunction
function! unite#vital#escape_pattern(...)
  return call('vital#_39a315#escape_pattern', a:000)
endfunction
function! unite#vital#set_default(...)
  return call('vital#_39a315#set_default', a:000)
endfunction
function! unite#vital#set_dictionary_helper(...)
  return call('vital#_39a315#set_dictionary_helper', a:000)
endfunction
function! unite#vital#substitute_path_separator(...)
  return call('vital#_39a315#substitute_path_separator', a:000)
endfunction
function! unite#vital#path2directory(...)
  return call('vital#_39a315#path2directory', a:000)
endfunction
function! unite#vital#path2project_directory(...)
  return call('vital#_39a315#path2project_directory', a:000)
endfunction
function! unite#vital#has_vimproc(...)
  return call('vital#_39a315#has_vimproc', a:000)
endfunction
function! unite#vital#system(...)
  return call('vital#_39a315#system', a:000)
endfunction
function! unite#vital#get_last_status(...)
  return call('vital#_39a315#get_last_status', a:000)
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo
autoload/unite.vim	[[[1
1794
"=============================================================================
" FILE: unite.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 13 Aug 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
" Version: 2.2, for Vim 7.0
"=============================================================================

let s:save_cpo = &cpo
set cpo&vim

function! unite#version()"{{{
  return str2nr(printf('%02d%02d%03d', 2, 2, 0))
endfunction"}}}

" User functions."{{{
function! unite#get_substitute_pattern(buffer_name)"{{{
  let l:buffer_name = (a:buffer_name == '' ? 'default' : a:buffer_name)

  return has_key(s:buffer_name_options, l:buffer_name) ?
        \ s:buffer_name_options[l:buffer_name].substitute_patterns : ''
endfunction"}}}
function! unite#set_substitute_pattern(buffer_name, pattern, subst, ...)"{{{
  let l:buffer_name = (a:buffer_name == '' ? 'default' : a:buffer_name)

  for key in split(l:buffer_name, ',')
    let l:substitute_patterns = has_key(s:buffer_name_options, key) ?
          \ unite#get_buffer_name_option(key, 'substitute_patterns') : {}

    if has_key(l:substitute_patterns, a:pattern)
          \ && a:pattern == ''
      call remove(l:substitute_patterns, a:pattern)
    else
      let l:substitute_patterns[a:pattern] = {
            \ 'pattern' : a:pattern,
            \ 'subst' : a:subst, 'priority' : (a:0 > 0 ? a:1 : 0),
            \ }
    endif

    call unite#set_buffer_name_option(key, 'substitute_patterns', l:substitute_patterns)
  endfor
endfunction"}}}
function! unite#set_buffer_name_option(buffer_name, option_name, value)"{{{
  let l:buffer_name = (a:buffer_name == '' ? 'default' : a:buffer_name)

  for key in split(l:buffer_name, ',')
    if !has_key(s:buffer_name_options, key)
      let s:buffer_name_options[key] = {}
    endif

    let s:buffer_name_options[key][a:option_name] = a:value
  endfor
endfunction"}}}
function! unite#get_buffer_name_option(buffer_name, option_name)"{{{
  let l:buffer_name = (a:buffer_name == '' ? 'default' : a:buffer_name)

  return s:buffer_name_options[a:buffer_name][a:option_name]
endfunction"}}}
function! unite#custom_filters(source_name, filters)"{{{
  let l:filters = type(a:filters) == type([]) ?
        \ a:filters : [a:filters]
  for key in split(a:source_name, ',')
    let s:custom.filters[key] = l:filters
  endfor
endfunction"}}}
function! unite#custom_alias(kind, name, action)"{{{
  for key in split(a:kind, ',')
    if !has_key(s:custom.aliases, key)
      let s:custom.aliases[key] = {}
    endif

    let s:custom.aliases[key][a:name] = a:action
  endfor
endfunction"}}}
function! unite#custom_default_action(kind, default_action)"{{{
  for key in split(a:kind, ',')
    let s:custom.default_actions[key] = a:default_action
  endfor
endfunction"}}}
function! unite#custom_action(kind, name, action)"{{{
  for key in split(a:kind, ',')
    if !has_key(s:custom.actions, key)
      let s:custom.actions[key] = {}
    endif
    let s:custom.actions[key][a:name] = a:action
  endfor
endfunction"}}}
function! unite#custom_max_candidates(source_name, max)"{{{
  for key in split(a:source_name, ',')
    let s:custom.max_candidates[key] = a:max
  endfor
endfunction"}}}
function! unite#undef_custom_action(kind, name)"{{{
  for key in split(a:kind, ',')
    if has_key(s:custom.actions, key)
      call remove(s:custom.actions, key)
    endif
  endfor
endfunction"}}}

function! unite#define_source(source)"{{{
  if type(a:source) == type([])
    for l:source in a:source
      let s:dynamic.sources[l:source.name] = l:source
    endfor
  else
    let s:dynamic.sources[a:source.name] = a:source
  endif
endfunction"}}}
function! unite#define_kind(kind)"{{{
  if type(a:kind) == type([])
    for l:kind in a:kind
      let s:dynamic.kinds[l:kind.name] = l:kind
    endfor
  else
    let s:dynamic.kinds[a:kind.name] = a:kind
  endif
endfunction"}}}
function! unite#define_filter(filter)"{{{
  if type(a:filter) == type([])
    for l:filter in a:filter
      let s:dynamic.filters[l:filter.name] = l:filter
    endfor
  else
    let s:dynamic.filters[a:filter.name] = a:filter
  endif
endfunction"}}}
function! unite#undef_source(name)"{{{
  if has_key(s:dynamic.sources, a:name)
    call remove(s:dynamic.sources, a:name)
  endif
endfunction"}}}
function! unite#undef_kind(name)"{{{
  if has_key(s:dynamic.kinds, a:name)
    call remove(s:dynamic.kinds, a:name)
  endif
endfunction"}}}
function! unite#undef_filter(name)"{{{
  if has_key(s:dynamic.filters, a:name)
    call remove(s:dynamic.filters, a:name)
  endif
endfunction"}}}

function! unite#do_action(action)
  return printf("%s:\<C-u>call unite#mappings#do_action(%s)\<CR>",
        \             (mode() ==# 'i' ? "\<C-o>" : ''), string(a:action))
endfunction
function! unite#smart_map(narrow_map, select_map)"{{{
  return (line('.') <= unite#get_current_unite().prompt_linenr && empty(unite#get_marked_candidates())) ? a:narrow_map : a:select_map
endfunction"}}}

function! unite#take_action(action_name, candidate)"{{{
  call s:take_action(a:action_name, a:candidate, 0)
endfunction"}}}
function! unite#take_parents_action(action_name, candidate, extend_candidate)"{{{
  call s:take_action(a:action_name, extend(deepcopy(a:candidate), a:extend_candidate), 1)
endfunction"}}}
"}}}

" Constants"{{{
let s:FALSE = 0
let s:TRUE = !s:FALSE

let s:LNUM_STATUS = 1
"}}}

" Variables  "{{{
" buffer number of the unite buffer
let s:last_unite_bufnr = -1
let s:current_unite = {}
let s:unite_cached_message = []
let s:is_initialized_unite_buffer = 0

let s:static = {}

let s:dynamic = {}
let s:dynamic.sources = {}
let s:dynamic.kinds = {}
let s:dynamic.filters = {}

let s:custom = {}
let s:custom.actions = {}
let s:custom.default_actions = {}
let s:custom.aliases = {}
let s:custom.filters = {}
let s:custom.source = {}
let s:custom.max_candidates = {}

let s:buffer_name_options = {}
call unite#set_substitute_pattern('files', '^\~',
      \ substitute(unite#util#substitute_path_separator($HOME), ' ', '\\\\ ', 'g'), -100)
call unite#set_substitute_pattern('files', '[^~.*]\ze/', '\0*', 100)
call unite#set_substitute_pattern('files', '/\ze[^~.*]', '/*', 100)
call unite#set_substitute_pattern('files', '\.', '*.', 1000)
call unite#set_buffer_name_option('files', 'smartcase', 0)
call unite#set_buffer_name_option('files', 'ignorecase', 1)

let s:unite_options = [
      \ '-buffer-name=', '-input=', '-prompt=',
      \ '-default-action=', '-start-insert','-no-start-insert', '-no-quit',
      \ '-winwidth=', '-winheight=',
      \ '-immediately', '-auto-preview', '-complete',
      \ '-vertical', '-horizontal', '-direction=',
      \ '-verbose', '-auto-resize',
      \]
"}}}

" Core functions."{{{
function! unite#get_kinds(...)"{{{
  let l:unite = unite#get_current_unite()
  return a:0 == 0 ? l:unite.kinds : get(l:unite.kinds, a:1, {})
endfunction"}}}
function! unite#get_sources(...)"{{{
  let l:all_sources = s:initialize_sources()
  return a:0 == 0 ? l:all_sources : get(l:all_sources, a:1, {})
endfunction"}}}
function! unite#get_filters(...)"{{{
  let l:all_filters = s:initialize_filters()
  return a:0 == 0 ? l:all_filters : get(l:all_filters, a:1, {})
endfunction"}}}
"}}}

" Helper functions."{{{
function! unite#is_win()"{{{
  return unite#util#is_win()
endfunction"}}}
function! unite#loaded_source_names()"{{{
  return map(copy(unite#loaded_sources_list()), 'v:val.name')
endfunction"}}}
function! unite#loaded_source_names_string()"{{{
  return join(unite#loaded_source_names())
endfunction"}}}
function! unite#loaded_source_names_with_args()"{{{
  return map(copy(unite#loaded_sources_list()), 'join(insert(filter(copy(v:val.args), "type(v:val) < 1"), v:val.name), ":")')
endfunction"}}}
function! unite#loaded_sources_list()"{{{
  return s:get_loaded_sources()
endfunction"}}}
function! unite#get_unite_candidates()"{{{
  return unite#get_current_unite().candidates
endfunction"}}}
function! unite#get_current_candidate(...)"{{{
  let l:linenr = a:0 > 1? a:1 : line('.')
  let l:num = l:linenr <= unite#get_current_unite().prompt_linenr ?
        \ 0 : l:linenr - (unite#get_current_unite().prompt_linenr+1)

  return get(unite#get_unite_candidates(), l:num, {})
endfunction"}}}
function! unite#get_context()"{{{
  return unite#get_current_unite().context
endfunction"}}}
" function! unite#get_action_table(source_name, kind_name, self_func, [is_parent_action])
function! unite#get_action_table(source_name, kind_name, self_func, ...)"{{{
  let l:kind = unite#get_kinds(a:kind_name)
  let l:source = unite#get_sources(a:source_name)
  if empty(l:source)
    call unite#print_error('source "' . a:source_name . '" is not found.')
    return {}
  endif

  let l:is_parents_action = a:0 > 0 ? a:1 : 0

  let l:action_table = {}

  let l:source_kind = 'source/'.a:source_name.'/'.a:kind_name
  let l:source_kind_wild = 'source/'.a:source_name.'/*'

  if !l:is_parents_action
    " Source/kind custom actions.
    if has_key(s:custom.actions, l:source_kind)
      let l:action_table = s:extend_actions(a:self_func, l:action_table,
            \ s:custom.actions[l:source_kind], 'custom/'.l:source.name.'/'.l:kind.name)
    endif

    " Source/kind actions.
    if has_key(l:source.action_table, a:kind_name)
      let l:action_table = s:extend_actions(a:self_func, l:action_table,
            \ l:source.action_table[a:kind_name], l:source.name.'/'.l:kind.name)
    endif

    " Source/* custom actions.
    if has_key(s:custom.actions, l:source_kind_wild)
      let l:action_table = s:extend_actions(a:self_func, l:action_table,
            \ s:custom.actions[l:source_kind_wild], 'custom/source/'.l:source.name)
    endif

    " Source/* actions.
    if has_key(l:source.action_table, '*')
      let l:action_table = s:extend_actions(a:self_func, l:action_table,
            \ l:source.action_table['*'], 'source/'.l:source.name)
    endif

    " Kind custom actions.
    if has_key(s:custom.actions, a:kind_name)
      let l:action_table = s:extend_actions(a:self_func, l:action_table,
            \ s:custom.actions[a:kind_name], 'custom/'.l:kind.name)
    endif

    " Kind actions.
    let l:action_table = s:extend_actions(a:self_func, l:action_table,
          \ l:kind.action_table, l:kind.name)
  endif

  " Parents actions.
  for l:parent in l:kind.parents
    let l:action_table = s:extend_actions(a:self_func, l:action_table,
          \ unite#get_action_table(a:source_name, l:parent, a:self_func))
  endfor

  if !l:is_parents_action
    " Kind aliases.
    call s:filter_alias_action(l:action_table, l:kind.alias_table,
          \ l:kind.name)

    " Kind custom aliases.
    if has_key(s:custom.aliases, a:kind_name)
      call s:filter_alias_action(l:action_table, s:custom.aliases[a:kind_name],
            \ 'custom/'.l:kind.name)
    endif

    " Source/* aliases.
    if has_key(l:source.alias_table, '*')
      call s:filter_alias_action(l:action_table, l:source.alias_table['*'],
            \ 'source/'.l:source.name)
    endif

    " Source/* custom aliases.
    if has_key(s:custom.aliases, l:source_kind_wild)
      call s:filter_alias_action(l:action_table, s:custom.aliases[l:source_kind_wild],
            \ 'custom/source/'.l:source.name)
    endif

    " Source/kind aliases.
    if has_key(s:custom.aliases, l:source_kind)
      call s:filter_alias_action(l:action_table, s:custom.aliases[l:source_kind],
            \ 'source/'.l:source.name.'/'.l:kind.name)
    endif

    " Source/kind custom aliases.
    if has_key(l:source.alias_table, a:kind_name)
      call s:filter_alias_action(l:action_table, l:source.alias_table[a:kind_name],
            \ 'custom/source/'.l:source.name.'/'.l:kind.name)
    endif
  endif

  " Set default parameters.
  for [l:action_name, l:action] in items(l:action_table)
    if !has_key(l:action, 'name')
      let l:action.name = l:action_name
    endif
    if !has_key(l:action, 'description')
      let l:action.description = ''
    endif
    if !has_key(l:action, 'is_quit')
      let l:action.is_quit = 1
    endif
    if !has_key(l:action, 'is_selectable')
      let l:action.is_selectable = 0
    endif
    if !has_key(l:action, 'is_invalidate_cache')
      let l:action.is_invalidate_cache = 0
    endif
  endfor

  " Filtering nop action.
  return filter(l:action_table, 'v:key !=# "nop"')
endfunction"}}}
function! unite#get_alias_table(source_name, kind_name)"{{{
  let l:kind = unite#get_kinds(a:kind_name)
  let l:source = unite#get_sources(a:source_name)

  let l:table = l:kind.alias_table

  let l:source_kind = 'source/'.a:source_name.'/'.a:kind_name
  let l:source_kind_wild = 'source/'.a:source_name.'/*'

  " Kind custom aliases.
  if has_key(s:custom.aliases, a:kind_name)
    let l:table = extend(l:table, s:custom.aliases[a:kind_name])
  endif

  " Source/* aliases.
  if has_key(l:source.alias_table, '*')
    let l:table = extend(l:table, l:source.alias_table['*'])
  endif

  " Source/* custom aliases.
  if has_key(s:custom.aliases, l:source_kind_wild)
    let l:table = extend(l:table, s:custom.aliases[l:source_kind_wild])
  endif

  " Source/kind aliases.
  if has_key(s:custom.aliases, l:source_kind)
    let l:table = extend(l:table, s:custom.aliases[l:source_kind])
  endif

  " Source/kind custom aliases.
  if has_key(l:source.alias_table, a:kind_name)
    let l:table = extend(l:table, l:source.alias_table[a:kind_name])
  endif

  return l:table
endfunction"}}}
function! unite#get_default_action(source_name, kind_name)"{{{
  let l:source = unite#get_sources(a:source_name)

  let l:source_kind = 'source/'.a:source_name.'/'.a:kind_name
  let l:source_kind_wild = 'source/'.a:source_name.'/*'

  " Source/kind custom default actions.
  if has_key(s:custom.default_actions, l:source_kind)
    return s:custom.default_actions[l:source_kind]
  endif

  " Source custom default actions.
  if has_key(l:source.default_action, a:kind_name)
    return l:source.default_action[a:kind_name]
  endif

  " Source/* custom default actions.
  if has_key(s:custom.default_actions, l:source_kind_wild)
    return s:custom.default_actions[l:source_kind_wild]
  endif

  " Source/* default actions.
  if has_key(l:source.default_action, '*')
    return l:source.default_action['*']
  endif

  " Kind custom default actions.
  if has_key(s:custom.default_actions, a:kind_name)
    return s:custom.default_actions[a:kind_name]
  endif

  " Kind default actions.
  return unite#get_kinds(a:kind_name).default_action
endfunction"}}}
function! unite#escape_match(str)"{{{
  return substitute(substitute(escape(a:str, '~\.^$[]'), '\*\@<!\*', '[^/]*', 'g'), '\*\*\+', '.*', 'g')
endfunction"}}}
function! unite#complete_source(arglead, cmdline, cursorpos)"{{{
  if empty(s:static)
    " Initialize load.
    call s:load_default_scripts()
  endif

  let l:sources = extend(copy(s:static.sources), s:dynamic.sources)
  return filter(sort(keys(l:sources))+s:unite_options, 'stridx(v:val, a:arglead) == 0')
endfunction"}}}
function! unite#complete_buffer(arglead, cmdline, cursorpos)"{{{
  let l:buffer_list = map(filter(range(1, bufnr('$')), '
        \ getbufvar(v:val, "&filetype") ==# "unite" &&
        \ !getbufvar(v:val, "unite").context.temporary'),
        \ 'getbufvar(v:val, "unite").buffer_name')

  return filter(l:buffer_list, printf('stridx(v:val, %s) == 0', string(a:arglead)))
endfunction"}}}
function! unite#invalidate_cache(source_name)  "{{{
  for l:source in unite#get_current_unite().sources
    if l:source.name ==# a:source_name
      let l:source.unite__is_invalidate = 1
    endif
  endfor
endfunction"}}}
function! unite#force_redraw() "{{{
  call s:redraw(1)
endfunction"}}}
function! unite#redraw() "{{{
  call s:redraw(0)
endfunction"}}}
function! unite#redraw_line(...) "{{{
  let l:linenr = a:0 > 0 ? a:1 : line('.')
  if l:linenr <= unite#get_current_unite().prompt_linenr || &filetype !=# 'unite'
    " Ignore.
    return
  endif

  let l:modifiable_save = &l:modifiable
  setlocal modifiable

  let l:candidate = unite#get_unite_candidates()[l:linenr - (unite#get_current_unite().prompt_linenr+1)]
  call setline(l:linenr, s:convert_lines([l:candidate])[0])

  let &l:modifiable = l:modifiable_save
endfunction"}}}
function! unite#quick_match_redraw() "{{{
  let l:modifiable_save = &l:modifiable
  setlocal modifiable

  call setline(unite#get_current_unite().prompt_linenr+1, s:convert_quick_match_lines(unite#get_current_unite().candidates))
  redraw

  let &l:modifiable = l:modifiable_save
endfunction"}}}
function! unite#redraw_status() "{{{
  let l:modifiable_save = &l:modifiable
  setlocal modifiable

  call setline(s:LNUM_STATUS, 'Sources: ' . join(unite#loaded_source_names_with_args(), ', '))

  let &l:modifiable = l:modifiable_save
endfunction"}}}
function! unite#redraw_candidates() "{{{
  let l:candidates = unite#gather_candidates()

  let l:modifiable_save = &l:modifiable
  setlocal modifiable

  let l:lines = s:convert_lines(l:candidates)
  if len(l:lines) < len(unite#get_current_unite().candidates)
    let l:pos = getpos('.')
    silent! execute (unite#get_current_unite().prompt_linenr+1).',$delete _'
    execute 'normal!' "1z\<Enter>"
    call setpos('.', l:pos)
  endif
  call setline(unite#get_current_unite().prompt_linenr+1, l:lines)

  let &l:modifiable = l:modifiable_save

  let l:unite = unite#get_current_unite()
  let l:unite.candidates = l:candidates

  if l:unite.context.auto_resize
        \ && l:unite.prompt_linenr + len(l:candidates)
        \      < l:unite.context.winheight
    " Auto resize.
    let l:pos = getpos('.')
    execute 'resize' l:unite.prompt_linenr + len(l:candidates)
    execute 'normal!' "1z\<Enter>"
    call setpos('.', l:pos)
  endif
endfunction"}}}
function! unite#get_marked_candidates() "{{{
  return unite#util#sort_by(filter(copy(unite#get_unite_candidates()),
        \ 'v:val.unite__is_marked'), 'v:val.unite__marked_time')
endfunction"}}}
function! unite#get_input()"{{{
  let l:unite = unite#get_current_unite()
  " Prompt check.
  if stridx(getline(l:unite.prompt_linenr), l:unite.prompt) != 0
    let l:modifiable_save = &l:modifiable
    setlocal modifiable

    " Restore prompt.
    call setline(l:unite.prompt_linenr, l:unite.prompt
          \ . getline(l:unite.prompt_linenr))

    let &l:modifiable = l:modifiable_save
  endif

  return getline(l:unite.prompt_linenr)[len(l:unite.prompt):]
endfunction"}}}
function! unite#get_options()"{{{
  return s:unite_options
endfunction"}}}
function! unite#get_self_functions()"{{{
  return split(matchstr(expand('<sfile>'), '^function \zs.*$'), '\.\.')[: -2]
endfunction"}}}
function! unite#gather_candidates()"{{{
  let l:candidates = []
  for l:source in unite#loaded_sources_list()
    let l:candidates += l:source.unite__candidates
  endfor

  " Post filter.
  let l:unite = unite#get_current_unite()
  for l:filter_name in unite#get_buffer_name_option(
        \ l:unite.buffer_name, 'filters')
    if has_key(l:unite.filters, l:filter_name)
      let l:candidates =
            \ l:unite.filters[l:filter_name].filter(l:candidates, l:unite.context)
    endif
  endfor

  return l:candidates
endfunction"}}}
function! unite#get_current_unite() "{{{
  return exists('b:unite') && s:is_initialized_unite_buffer ? b:unite : s:current_unite
endfunction"}}}

" Utils.
function! unite#print_error(message)"{{{
  let l:message = type(a:message) == type([]) ?
        \ l:message : [a:message]
  for l:mes in l:message
    call unite#print_message('!!!'.l:mes.'!!!')

    echohl WarningMsg | echomsg l:mes | echohl None
  endfor
endfunction"}}}
function! unite#print_message(message)"{{{
  if &filetype ==# 'unite'
    call s:print_buffer(a:message)
  else
    call add(s:unite_cached_message, a:message)
  endif
endfunction"}}}
function! unite#clear_message()"{{{
  if &filetype ==# 'unite'
    let l:unite = unite#get_current_unite()
    if l:unite.prompt_linenr > 2
      let l:modifiable_save = &l:modifiable
      setlocal modifiable

      let l:pos = getpos('.')
      silent! execute '2,'.(l:unite.prompt_linenr-1).'delete _'
      let l:pos[1] -= l:unite.prompt_linenr-2
      call setpos('.', l:pos)
      normal! zb
      if mode() ==# 'i' && l:pos[2] == col('$')
        startinsert!
      endif

      let l:unite.prompt_linenr = 2

      let &l:modifiable = l:modifiable_save
      call s:on_cursor_moved()

      if exists('b:current_syntax') && b:current_syntax ==# 'unite'
        syntax clear uniteInputLine
        execute 'syntax match uniteInputLine'
              \ '/\%'.l:unite.prompt_linenr.'l.*/'
              \ 'contains=uniteInputPrompt,uniteInputPromptError,uniteInputSpecial'
      endif
    endif
  endif
  let s:unite_cached_message = []
endfunction"}}}
function! unite#substitute_path_separator(path)"{{{
  return unite#util#substitute_path_separator(a:path)
endfunction"}}}
function! unite#path2directory(path)"{{{
  return unite#util#path2directory(a:path)
endfunction"}}}
function! s:print_buffer(message)"{{{
  if &filetype ==# 'unite'
    let l:modifiable_save = &l:modifiable
    setlocal modifiable

    let l:unite = unite#get_current_unite()
    let l:pos = getpos('.')
    call append(l:unite.prompt_linenr-1, a:message)
    let l:len = type(a:message) == type([]) ?
          \ len(a:message) : 1
    let l:unite.prompt_linenr += l:len

    let l:pos[1] += l:len
    call setpos('.', l:pos)
    normal! zb
    if mode() ==# 'i' && l:pos[2] == col('$')
      startinsert!
    endif

    let &l:modifiable = l:modifiable_save
    call s:on_cursor_moved()

    if exists('b:current_syntax') && b:current_syntax ==# 'unite'
      syntax clear uniteInputLine
      execute 'syntax match uniteInputLine'
            \ '/\%'.l:unite.prompt_linenr.'l.*/'
            \ 'contains=uniteInputPrompt,uniteInputPromptError,uniteInputSpecial'
    endif
  endif
endfunction"}}}
"}}}

" Command functions.
function! unite#start(sources, ...)"{{{
  " Check command line window.
  if s:is_cmdwin()
    echoerr 'Command line buffer is detected!'
    echoerr 'Please close command line buffer.'
    return
  endif

  let l:context = a:0 >= 1 ? a:1 : {}
  call s:initialize_context(l:context)

  let s:is_initialized_unite_buffer = 0

  try
    call s:initialize_current_unite(a:sources, l:context)
  catch /^Invalid source/
    return
  endtry

  " Caching.
  let s:current_unite.last_input = l:context.input
  let s:current_unite.input = l:context.input
  call s:recache_candidates(l:context.input, l:context.is_redraw)

  if l:context.immediately
    let l:candidates = unite#gather_candidates()

    " Immediately action.
    if empty(l:candidates)
      " Ignore.
      let s:is_initialized_unite_buffer = 1
      return
    elseif len(l:candidates) == 1
      " Default action.
      call unite#mappings#do_action(l:context.default_action, [l:candidates[0]])
      let s:is_initialized_unite_buffer = 1
      return
    endif
  endif

  call s:initialize_unite_buffer()

  let s:is_initialized_unite_buffer = 1

  let l:unite = unite#get_current_unite()

  setlocal modifiable

  silent % delete _
  call unite#redraw_status()
  call setline(l:unite.prompt_linenr, l:unite.prompt . l:unite.context.input)
  for message in s:unite_cached_message
    call s:print_buffer(message)
    unlet message
  endfor
  call unite#redraw_candidates()

  if l:unite.context.start_insert || l:unite.context.complete
    let l:unite.is_insert = 1

    execute l:unite.prompt_linenr
    normal! zb

    startinsert!
  else
    let l:positions = unite#get_buffer_name_option(l:unite.buffer_name, 'unite__save_pos')
    let l:key = unite#loaded_source_names_string()
    let l:is_restore = l:unite.context.input == '' &&
          \ has_key(l:positions, l:key)
    if l:is_restore
      " Restore position.
      call setpos('.', l:positions[l:key].pos)
    endif
    let l:candidate = has_key(l:positions, l:key) ?
          \ l:positions[l:key].candidate : {}

    let l:unite.is_insert = 0

    if !l:is_restore ||
          \ l:candidate != unite#get_current_candidate(l:unite.prompt_linenr+1)
      execute (l:unite.prompt_linenr+1)
      normal! zb
    endif
    normal! 0
  endif
endfunction"}}}
function! unite#start_temporary(sources, new_context, buffer_name)"{{{
  " Get current context.
  let l:context = deepcopy(unite#get_context())
  let l:context.old_buffer_info = insert(l:context.old_buffer_info,
        \ { 'buffer_name' : unite#get_current_unite().buffer_name,
        \   'pos' : getpos('.'), })

  let l:context.buffer_name = a:buffer_name
  let l:context.temporary = 1
  let l:context.input = ''
  let l:context.auto_preview = 0
  let l:context.default_action = 'default'

  " Overwrite context.
  let l:context = extend(l:context, a:new_context)

  call unite#force_quit_session()
  call unite#start(a:sources, l:context)
endfunction"}}}
function! unite#resume(buffer_name)"{{{
  " Check command line window.
  if s:is_cmdwin()
    echoerr 'Command line buffer is detected!'
    echoerr 'Please close command line buffer.'
    return
  endif

  if a:buffer_name == ''
    " Use last unite buffer.
    if !bufexists(s:last_unite_bufnr)
      call unite#util#print_error('No unite buffer.')
      return
    endif

    let l:bufnr = s:last_unite_bufnr
  else
    let l:buffer_dict = {}
    for l:unite in map(filter(range(1, bufnr('$')),
          \ 'getbufvar(v:val, "&filetype") ==# "unite" && !getbufvar(v:val, "unite").context.temporary'),
          \ 'getbufvar(v:val, "unite")')
      let l:buffer_dict[l:unite.buffer_name] = l:unite.bufnr
    endfor

    if !has_key(l:buffer_dict, a:buffer_name)
      call unite#util#print_error('Invalid buffer name : ' . a:buffer_name)
      return
    endif
    let l:bufnr = l:buffer_dict[a:buffer_name]
  endif

  let l:winnr = winnr()
  let l:win_rest_cmd = winrestcmd()

  let l:unite = getbufvar(l:bufnr, 'unite')
  let l:context = l:unite.context

  call s:switch_unite_buffer(bufname(l:bufnr), l:context)

  " Set parameters.
  let l:unite = unite#get_current_unite()
  let l:unite.winnr = l:winnr
  let l:unite.win_rest_cmd = l:win_rest_cmd
  let l:unite.redrawtime_save = &redrawtime
  let l:unite.access_time = localtime()

  let s:current_unite = l:unite

  if g:unite_enable_start_insert
    let l:unite.is_insert = 1

    execute l:unite.prompt_linenr
    normal! zb

    startinsert!
  else
    let l:positions = unite#get_buffer_name_option(l:unite.buffer_name, 'unite__save_pos')
    let l:key = unite#loaded_source_names_string()
    let l:is_restore = has_key(l:positions, l:key)
    let l:candidate = unite#get_current_candidate()

    if l:is_restore
      " Restore position.
      call setpos('.', l:positions[l:key].pos)
    endif

    let l:unite.is_insert = 0

    if !l:is_restore
          \ || l:candidate != unite#get_current_candidate()
      execute (l:unite.prompt_linenr+1)
    endif
    normal! 0zb
  endif
endfunction"}}}
function! s:initialize_context(context)"{{{
  if !has_key(a:context, 'input')
    let a:context.input = ''
  endif
  if !has_key(a:context, 'start_insert')
    let a:context.start_insert = g:unite_enable_start_insert
  endif
  if has_key(a:context, 'no_start_insert')
        \ && a:context.no_start_insert
    " Disable start insert.
    let a:context.start_insert = 0
  endif
  if !has_key(a:context, 'complete')
    let a:context.complete = 0
  endif
  if !has_key(a:context, 'col')
    let a:context.col = col('.')
  endif
  if !has_key(a:context, 'no_quit')
    let a:context.no_quit = 0
  endif
  if !has_key(a:context, 'buffer_name')
    let a:context.buffer_name = 'default'
  endif
  if !has_key(a:context, 'prompt')
    let a:context.prompt = '> '
  endif
  if !has_key(a:context, 'default_action')
    let a:context.default_action = 'default'
  endif
  if !has_key(a:context, 'winwidth')
    let a:context.winwidth = g:unite_winwidth
  endif
  if !has_key(a:context, 'winheight')
    let a:context.winheight = g:unite_winheight
  endif
  if !has_key(a:context, 'immediately')
    let a:context.immediately = 0
  endif
  if !has_key(a:context, 'auto_preview')
    let a:context.auto_preview = 0
  endif
  if !has_key(a:context, 'vertical')
    let a:context.vertical = g:unite_enable_split_vertically
  endif
  if has_key(a:context, 'horizontal')
    " Disable vertically.
    let a:context.vertical = 0
  endif
  if !has_key(a:context, 'direction')
    let a:context.direction = g:unite_split_rule
  endif
  if !has_key(a:context, 'temporary')
    let a:context.temporary = 0
  endif
  if !has_key(a:context, 'verbose')
    let a:context.verbose = 0
  endif
  if !has_key(a:context, 'auto_resize')
    let a:context.auto_resize = 0
  endif
  if !has_key(a:context, 'old_buffer_info')
    let a:context.old_buffer_info = []
  endif
  let a:context.is_redraw = 0
endfunction"}}}

function! unite#force_quit_session()  "{{{
  call s:quit_session(1)

  let l:context = unite#get_context()
  if l:context.temporary
    call s:resume_from_temporary(l:context)
  endif
endfunction"}}}
function! unite#quit_session()  "{{{
  call s:quit_session(0)

  let l:context = unite#get_context()
  if l:context.temporary
    call s:resume_from_temporary(l:context)
  endif
endfunction"}}}
function! s:quit_session(is_force)  "{{{
  if &filetype !=# 'unite'
    return
  endif

  " Save unite value.
  let s:current_unite = b:unite
  let l:unite = s:current_unite
  let l:context = l:unite.context

  let l:key = unite#loaded_source_names_string()

  " Save position.
  let l:positions = unite#get_buffer_name_option(
        \ l:unite.buffer_name, 'unite__save_pos')
  let l:positions[l:key] = {
        \ 'pos' : getpos('.'),
        \ 'candidate' : unite#get_current_candidate(),
        \ }

  if l:context.input != ''
    " Save input.
    let l:inputs = unite#get_buffer_name_option(
          \ l:unite.buffer_name, 'unite__inputs')
    if !has_key(l:inputs, l:key)
      let l:inputs[l:key] = []
    endif
    call insert(filter(l:inputs[l:key],
          \ 'v:val !=# l:unite.context.input'), l:context.input)
  endif

  if winnr('$') != 1
    if !a:is_force && l:context.no_quit
      if winnr('#') > 0
        wincmd p
      endif
    else
      let l:bufname = bufname('%')
      noautocmd close!
      execute l:unite.winnr . 'wincmd w'
      call s:on_buf_unload(l:bufname)
    endif
  endif

  if l:context.complete
    if l:context.col < col('$')
      startinsert
    else
      startinsert!
    endif
  else
    stopinsert
    redraw!
  endif
endfunction"}}}
function! s:resume_from_temporary(context)  "{{{
  " Resume unite buffer.
  let l:buffer_info = a:context.old_buffer_info[0]
  call unite#resume(l:buffer_info.buffer_name)
  call setpos('.', l:buffer_info.pos)
  let a:context.old_buffer_info = a:context.old_buffer_info[1:]
endfunction"}}}

function! s:load_default_scripts()"{{{
  " Gathering all sources and kind name.
  let s:static.sources = {}
  let s:static.kinds = {}
  let s:static.filters = {}

  for l:key in ['sources', 'kinds', 'filters']
    for l:name in map(split(globpath(&runtimepath, 'autoload/unite/' . l:key . '/*.vim'), '\n'),
          \ 'fnamemodify(v:val, ":t:r")')

      let l:define = {'unite#' . l:key . '#' . l:name . '#define'}()
      for l:dict in (type(l:define) == type([]) ? l:define : [l:define])
        if !empty(l:dict) && !has_key(s:static[l:key], l:dict.name)
          let s:static[l:key][l:dict.name] = l:dict
        endif
      endfor
      unlet l:define
    endfor
  endfor
endfunction"}}}
function! s:initialize_loaded_sources(sources, context)"{{{
  let l:all_sources = s:initialize_sources()
  let l:sources = []

  let l:number = 0
  for [l:source_name, l:args] in map(a:sources, 'type(v:val) == type([]) ? [v:val[0], v:val[1:]] : [v:val, []]')
    if !has_key(l:all_sources, l:source_name)
      call unite#util#print_error('Invalid source name "' . l:source_name . '" is detected.')
      throw 'Invalid source'
    endif

    let l:source = deepcopy(l:all_sources[l:source_name])
    let l:source.args = l:args
    let l:source.unite__is_invalidate = 1

    let l:source.unite__context = deepcopy(a:context)
    let l:source.unite__context.is_async =
          \ has_key(l:source, 'async_gather_candidates')
    let l:source.unite__context.source = l:source
    let l:source.unite__candidates = []
    let l:source.unite__cached_candidates = []
    let l:source.unite__cached_change_candidates = []
    let l:source.unite__number = l:number
    let l:number += 1

    call add(l:sources, l:source)
  endfor

  return l:sources
endfunction"}}}
function! s:initialize_sources()"{{{
  if empty(s:static)
    " Initialize load.
    call s:load_default_scripts()
  endif

  let l:sources = extend(deepcopy(s:static.sources), deepcopy(s:dynamic.sources))

  for l:source in values(filter(copy(l:sources), '!has_key(v:val, "is_initialized")'))
    let l:source.is_initialized = 1

    if !has_key(l:source, 'is_volatile')
      let l:source.is_volatile = 0
    endif
    if !has_key(l:source, 'required_pattern_length')
      let l:source.required_pattern_length = 0
    endif
    if !has_key(l:source, 'action_table')
      let l:source.action_table = {}
    endif
    if !has_key(l:source, 'default_action')
      let l:source.default_action = {}
    elseif type(l:source.default_action) == type('')
      " Syntax sugar.
      let l:source.default_action = { '*' : l:source.default_action }
    endif
    if !has_key(l:source, 'alias_table')
      let l:source.alias_table = {}
    endif
    if !has_key(l:source, 'hooks')
      let l:source.hooks = {}
    endif
    if !has_key(l:source, 'description')
      let l:source.description = ''
    endif
    if !has_key(l:source, 'syntax')
      let l:source.syntax = ''
    endif
    if l:source.is_volatile
          \ && !has_key(l:source, 'change_candidates')
      let l:source.change_candidates = l:source.gather_candidates
      call remove(l:source, 'gather_candidates')
    endif

    let l:source.filters =
          \ has_key(s:custom.filters, l:source.name) ?
          \ s:custom.filters[l:source.name] :
          \ has_key(l:source, 'filters') ?
          \ l:source.filters :
          \ unite#filters#default#get()
    let l:source.max_candidates =
          \ has_key(s:custom.max_candidates, l:source.name) ?
          \ s:custom.max_candidates[l:source.name] :
          \ has_key(l:source, 'max_candidates') ?
          \ l:source.max_candidates :
          \ 0
  endfor

  return l:sources
endfunction"}}}
function! s:initialize_kinds()"{{{
  let l:kinds = extend(copy(s:static.kinds), s:dynamic.kinds)
  for l:kind in values(filter(copy(l:kinds), '!has_key(v:val, "is_initialized")'))
    let l:kind.is_initialized = 1
    if !has_key(l:kind, 'alias_table')
      let l:kind.alias_table = {}
    endif
    if !has_key(l:kind, 'parents')
      let l:kind.parents = ['common']
    endif
  endfor

  return l:kinds
endfunction"}}}
function! s:initialize_filters()"{{{
  return extend(copy(s:static.filters), s:dynamic.filters)
endfunction"}}}
function! s:initialize_buffer_name_options(buffer_name)"{{{
  if !has_key(s:buffer_name_options, a:buffer_name)
    let s:buffer_name_options[a:buffer_name] = {}
  endif
  let l:setting = s:buffer_name_options[a:buffer_name]
  if !has_key(l:setting, 'substitute_patterns')
    let l:setting.substitute_patterns = {}
  endif
  if !has_key(l:setting, 'filters')
    let l:setting.filters = []
  endif
  if !has_key(l:setting, 'ignorecase')
    let l:setting.ignorecase = &ignorecase
  endif
  if !has_key(l:setting, 'smartcase')
    let l:setting.smartcase = &smartcase
  endif
  if !has_key(l:setting, 'unite__save_pos')
    let l:setting.unite__save_pos = {}
  endif
  if !has_key(l:setting, 'unite__inputs')
    let l:setting.unite__inputs = {}
  endif
endfunction"}}}

function! s:recache_candidates(input, is_force)"{{{
  let l:unite = unite#get_current_unite()

  " Save options.
  let l:ignorecase_save = &ignorecase

  if unite#get_buffer_name_option(l:unite.buffer_name, 'smartcase') && a:input =~ '\u'
    let &ignorecase = 0
  else
    let &ignorecase = unite#get_buffer_name_option(l:unite.buffer_name, 'ignorecase')
  endif

  let l:input = s:get_substitute_input(a:input)
  let l:input_len = unite#util#strchars(l:input)

  let l:context = l:unite.context
  let l:context.input = l:input
  let l:context.is_redraw = a:is_force
  let l:filtered_count = 0

  for l:source in unite#loaded_sources_list()
    " Check required pattern length.
    if l:input_len < l:source.required_pattern_length
      continue
    endif

    " Set context.
    let l:source.unite__context.input = l:context.input
    let l:source.unite__context.is_redraw = l:context.is_redraw
    let l:source.unite__context.is_invalidate = l:source.unite__is_invalidate

    if l:context.is_redraw || l:source.unite__is_invalidate
      " Recaching.
      let l:source.unite__cached_candidates = []

      if has_key(l:source, 'gather_candidates')
        let l:source.unite__cached_candidates +=
              \ copy(l:source.gather_candidates(l:source.args, l:source.unite__context))
      endif
    endif

    if l:source.unite__context.is_async
      let l:source.unite__cached_candidates +=
            \ l:source.async_gather_candidates(l:source.args, l:source.unite__context)
    endif

    " Update async state.
    let l:unite.is_async =
          \ len(filter(copy(l:unite.sources), 'v:val.unite__context.is_async')) > 0

    if has_key(l:source, 'change_candidates')
          \ && (l:context.is_redraw || l:source.unite__is_invalidate
          \      || a:input !=# l:unite.last_input)
      " Recaching.
      let l:source.unite__cached_change_candidates =
            \ l:source.change_candidates(l:source.args, l:source.unite__context)
    endif

    let l:source_candidates = l:source.unite__cached_candidates
          \ + l:source.unite__cached_change_candidates

    let l:custom_source = has_key(s:custom.source, l:source.name) ?
          \ s:custom.source[l:source.name] : {}

    " Filter.
    for l:filter_name in has_key(l:custom_source, 'filters') ?
          \ l:custom_source.filters : l:source.filters
      if has_key(l:unite.filters, l:filter_name)
        let l:source_candidates =
              \ l:unite.filters[l:filter_name].filter(l:source_candidates, l:source.unite__context)
      endif
    endfor

    if l:source.max_candidates != 0
          \ && len(l:source_candidates) > l:source.max_candidates
      " Filtering too many candidates.
      let l:source_candidates = l:source_candidates[: l:source.max_candidates - 1]

      if l:context.verbose && l:filtered_count < &cmdheight
        echohl WarningMsg | echomsg printf('[%s] Filtering too many candidates.', l:source.name) | echohl None
        let l:filtered_count += 1
      endif
    endif

    " Call post_filter hook.
    let l:source.unite__context.candidates = l:source_candidates
    call s:call_hook([l:source], 'on_post_filter')

    for l:candidate in l:source_candidates
      if !has_key(l:candidate, 'abbr')
        let l:candidate.abbr = l:candidate.word
      endif
      if !has_key(l:candidate, 'kind')
        let l:candidate.kind = 'common'
      endif
      if !has_key(l:candidate, 'source')
        let l:candidate.source = l:source.name
      endif
      if !has_key(l:candidate, 'is_dummy')
        let l:candidate.is_dummy = 0
      endif
      if !has_key(l:candidate, 'unite__is_marked')
        let l:candidate.unite__is_marked = 0
      endif
    endfor

    let l:source.unite__candidates = l:source_candidates
    let l:source.unite__is_invalidate = 0
  endfor

  let &ignorecase = l:ignorecase_save
endfunction"}}}
function! s:convert_quick_match_lines(candidates)"{{{
  let l:unite = unite#get_current_unite()
  let [l:max_width, l:max_source_name] = s:adjustments(winwidth(0)-2, l:unite.max_source_name, 5)
  if l:unite.max_source_name == 0
    let l:max_width -= 1
  endif

  let l:candidates = []

  " Create key table.
  let l:keys = {}
  for [l:key, l:number] in items(g:unite_quick_match_table)
    let l:keys[l:number] = l:key . ': '
  endfor

  " Add number.
  let l:num = 0
  for l:candidate in a:candidates
    call add(l:candidates,
          \ (has_key(l:keys, l:num) ? l:keys[l:num] : '   ')
          \ . (l:unite.max_source_name == 0 ? ' ' :
          \    unite#util#truncate(l:candidate.source, l:max_source_name))
          \ . unite#util#truncate_smart(l:candidate.abbr, l:max_width, l:max_width/3, '..'))
    let l:num += 1
  endfor

  return l:candidates
endfunction"}}}
function! s:convert_lines(candidates)"{{{
  let l:unite = unite#get_current_unite()
  let [l:max_width, l:max_source_name] = s:adjustments(winwidth(0)-2, l:unite.max_source_name, 2)
  if l:unite.max_source_name == 0
    let l:max_width -= 1
  endif

  return map(copy(a:candidates),
        \ '(v:val.unite__is_marked ? "*  " : "-  ")
        \ . (l:unite.max_source_name == 0 ? " " : unite#util#truncate(v:val.source, l:max_source_name))
        \ . unite#util#truncate_smart(v:val.abbr, ' . l:max_width .  ', l:max_width/3, "..")')
endfunction"}}}

function! s:initialize_current_unite(sources, context)"{{{
  let s:unite_cached_message = []

  let l:context = a:context

  if getbufvar(bufnr('%'), '&filetype') ==# 'unite'
    if unite#get_current_unite().buffer_name ==# l:context.buffer_name
      " Quit unite buffer.
      call unite#force_quit_session()

      if l:context.input == ''
        " Get input text.
        let l:context.input = unite#get_input()
      endif
    endif
  endif

  " The current buffer is initialized.
  let l:buffer_name = unite#is_win() ? '[unite]' : '*unite*'
  let l:buffer_name .= ' - ' . l:context.buffer_name

  let l:winnr = winnr()
  let l:win_rest_cmd = winrestcmd()

  " Check sources.
  let l:sources = s:initialize_loaded_sources(a:sources, a:context)

  " Call initialize functions.
  call s:call_hook(l:sources, 'on_init')

  " Set parameters.
  let l:unite = {}
  let l:unite.winnr = l:winnr
  let l:unite.win_rest_cmd = l:win_rest_cmd
  let l:unite.context = l:context
  let l:unite.candidates = []
  let l:unite.sources = l:sources
  let l:unite.kinds = s:initialize_kinds()
  let l:unite.filters = s:initialize_filters()
  let l:unite.buffer_name = (l:context.buffer_name == '') ? 'default' : l:context.buffer_name
  let l:unite.buffer_options =
        \ s:initialize_buffer_name_options(l:unite.buffer_name)
  let l:unite.real_buffer_name = l:buffer_name
  let l:unite.prompt = l:context.prompt
  let l:unite.input = l:context.input
  let l:unite.last_input = l:context.input
  let l:unite.sidescrolloff_save = &sidescrolloff
  let l:unite.prompt_linenr = 2
  let l:unite.max_source_name = len(a:sources) > 1 ?
        \ max(map(copy(a:sources), 'len(v:val[0])')) + 2 : 0
  let l:unite.is_async =
        \ len(filter(copy(l:sources), 'v:val.unite__context.is_async')) > 0
  let l:unite.access_time = localtime()
  let l:unite.is_finalized = 0

  " Preview windows check.
  let l:unite.has_preview_window =
   \ len(filter(range(1, winnr('$')), 'getwinvar(v:val, "&previewwindow")')) > 0

  let s:current_unite = l:unite
endfunction"}}}
function! s:initialize_unite_buffer()"{{{
  let l:is_bufexists = bufexists(s:current_unite.real_buffer_name)
  call s:switch_unite_buffer(s:current_unite.real_buffer_name, s:current_unite.context)

  let b:unite = s:current_unite
  let l:unite = unite#get_current_unite()

  if !l:unite.context.temporary
    let s:last_unite_bufnr = bufnr('%')
  endif
  let l:unite.bufnr = bufnr('%')

  " Note: If unite buffer initialize is incomplete, &modified or &wrap.
  if !l:is_bufexists || &modified || &wrap
    " Basic settings.
    setlocal bufhidden=hide
    setlocal buftype=nofile
    setlocal nolist
    setlocal nobuflisted
    setlocal noswapfile
    setlocal noreadonly
    setlocal nofoldenable
    setlocal nomodeline
    setlocal nonumber
    setlocal nowrap
    setlocal foldcolumn=0
    setlocal iskeyword+=-,+,\\,!,~
    match
    if has('conceal')
      setlocal conceallevel=3
      setlocal concealcursor=n
    endif
    if exists('+cursorcolumn')
      setlocal nocursorcolumn
    endif
    if exists('+colorcolumn')
      setlocal colorcolumn=0
    endif
    setlocal nocursorline

    " Autocommands.
    augroup plugin-unite
      autocmd InsertEnter <buffer>  call s:on_insert_enter()
      autocmd InsertLeave <buffer>  call s:on_insert_leave()
      autocmd CursorHoldI <buffer>  call s:on_cursor_hold_i()
      autocmd CursorHold <buffer>  call s:on_cursor_hold()
      autocmd CursorMoved,CursorMovedI <buffer>  call s:on_cursor_moved()
      autocmd BufUnload,BufHidden <buffer>  call s:on_buf_unload(expand('<afile>'))
    augroup END

    call unite#mappings#define_default_mappings()

    if exists(':NeoComplCacheLock')
      " Lock neocomplcache.
      NeoComplCacheLock
    endif
  endif

  if exists('&redrawtime')
    " Save redrawtime
    let l:unite.redrawtime_save = &redrawtime
    let &redrawtime = 100
  endif

  " User's initialization.
  setlocal nomodifiable
  set sidescrolloff=0
  setfiletype unite

  if exists('b:current_syntax') && b:current_syntax ==# 'unite'
    " Set highlight.
    let l:match_prompt = escape(l:unite.prompt, '\/*~.^$[]')
    syntax clear uniteInputPrompt
    execute 'syntax match uniteInputPrompt' '/^'.l:match_prompt.'/ contained'

    syntax clear uniteCandidateSourceName
    if l:unite.max_source_name > 0
      syntax match uniteCandidateSourceName /\%4c[[:alnum:]_\/-]\+/ contained
    else
      syntax match uniteCandidateSourceName /^- / contained
    endif
    let l:source_padding = 3
    execute 'syntax match uniteCandidateAbbr' '/\%'.(l:unite.max_source_name+l:source_padding).'c.*/ contained'

    execute 'highlight default link uniteCandidateAbbr'  g:unite_abbr_highlight

    " Set syntax.
    for l:source in l:unite.sources
      if l:source.syntax != ''
        let l:name = len(l:unite.sources) > 1 ? l:source.name : ''

        execute 'syntax match' l:source.syntax '/\%'.(l:unite.max_source_name+l:source_padding).'c.*/ contained'

        execute 'highlight default link' l:source.syntax g:unite_abbr_highlight

        execute printf('syntax region %s start="^- %s" end="$" contains=%s%s',
              \ 'uniteSourceLine__'.l:source.syntax, (l:name == '' ? '' : l:name . '\>'), (l:name == '' ? '' : 'uniteSourceNames,'), l:source.syntax
              \ )

        call s:call_hook([l:source], 'on_syntax')
      endif
    endfor
  endif
endfunction"}}}
function! s:switch_unite_buffer(buffer_name, context)"{{{
  " Search unite window.
  " Note: must escape file-pattern.
  if bufwinnr(unite#util#escape_file_searching(a:buffer_name)) > 0
    silent execute bufwinnr(unite#util#escape_file_searching(a:buffer_name)) 'wincmd w'
  else
    " Split window.
    execute a:context.direction (bufexists(a:buffer_name) ?
          \ ((a:context.vertical) ? 'vsplit' : 'split') :
          \ ((a:context.vertical) ? 'vnew' : 'new'))

    if bufexists(a:buffer_name)
      " Search buffer name.
      let l:bufnr = 1
      let l:max = bufnr('$')
      while l:bufnr <= l:max
        if bufname(l:bufnr) ==# a:buffer_name
          silent execute l:bufnr 'buffer'
          break
        endif

        let l:bufnr += 1
      endwhile
    else
      silent! file `=a:buffer_name`
    endif
  endif

  if winnr('$') != 1
    if a:context.vertical
      execute 'vertical resize' a:context.winwidth
    else
      execute 'resize' a:context.winheight
    endif
  endif
endfunction"}}}

function! s:redraw(is_force) "{{{
  if &filetype !=# 'unite'
    return
  endif

  if a:is_force
    call unite#clear_message()
  endif

  let l:unite = unite#get_current_unite()
  let l:input = unite#get_input()
  if !a:is_force && l:input ==# l:unite.last_input
        \ && !l:unite.is_async
    return
  endif

  let l:unite.context.is_redraw = a:is_force

  " Recaching.
  call s:recache_candidates(l:input, a:is_force)

  let l:unite.last_input = l:input

  " Redraw.
  call unite#redraw_candidates()
  let l:unite.context.is_redraw = 0
endfunction"}}}

" Autocmd events.
function! s:on_insert_enter()  "{{{
  let l:unite = unite#get_current_unite()
  let l:unite.is_insert = 1
  setlocal modifiable

  if line('.') != l:unite.prompt_linenr
        \ || col('.') <= len(l:unite.prompt)
    execute l:unite.prompt_linenr
    normal! zb
    startinsert!
  endif

  if &updatetime > g:unite_update_time
    let l:unite = unite#get_current_unite()
    let l:unite.update_time_save = &updatetime
    let &updatetime = g:unite_update_time
  endif
endfunction"}}}
function! s:on_insert_leave()  "{{{
  let l:unite = unite#get_current_unite()

  if line('.') == l:unite.prompt_linenr
    " Redraw.
    call unite#redraw()
  else
    normal! 0
  endif

  let l:unite.is_insert = 0

  setlocal nomodifiable

  if has_key(l:unite, 'update_time_save')
        \ && &updatetime < l:unite.update_time_save
    let &updatetime = l:unite.update_time_save
  endif
endfunction"}}}
function! s:on_cursor_hold_i()  "{{{
  let l:prompt_linenr = unite#get_current_unite().prompt_linenr
  if line('.') == l:prompt_linenr
    " Redraw.
    call unite#redraw()

    execute 'match' (line('.') <= l:prompt_linenr ?
          \ line('$') <= l:prompt_linenr ?
          \ 'UniteError /\%'.l:prompt_linenr.'l/' :
          \ g:unite_cursor_line_highlight.' /\%'.(l:prompt_linenr+1).'l/' :
          \ g:unite_cursor_line_highlight.' /\%'.line('.').'l/')

    " Prompt check.
    if col('.') <= len(unite#get_current_unite().prompt)
      startinsert!
    endif
  endif

  if unite#get_current_unite().is_async
    " Ignore key sequences.
    call feedkeys("\<C-r>\<ESC>", 'n')
  endif
endfunction"}}}
function! s:on_cursor_hold()  "{{{
  " Redraw.
  call unite#redraw()

  if unite#get_current_unite().is_async
    " Ignore key sequences.
    call feedkeys("g\<ESC>", 'n')
  endif
endfunction"}}}
function! s:on_cursor_moved()  "{{{
  if &filetype !=# 'unite'
    return
  endif

  let l:prompt_linenr = unite#get_current_unite().prompt_linenr

  setlocal nocursorline

  execute 'setlocal' line('.') == l:prompt_linenr ?
        \ 'modifiable' : 'nomodifiable'

  execute 'match' (line('.') <= l:prompt_linenr ?
        \ line('$') <= l:prompt_linenr ?
        \ 'UniteError /\%'.l:prompt_linenr.'l/' :
        \ g:unite_cursor_line_highlight.' /\%'.(l:prompt_linenr+1).'l/' :
        \ g:unite_cursor_line_highlight.' /\%'.line('.').'l/')

  if unite#get_current_unite().context.auto_preview
    if !unite#get_current_unite().has_preview_window
      pclose!
    endif

    call unite#mappings#do_action('preview')

    " Restore window size.
    let l:context = unite#get_context()
    if winnr('$') != 1
      if l:context.vertical
        if winwidth(winnr()) != l:context.winwidth
          execute 'vertical resize' l:context.winwidth
        endif
      elseif winheight(winnr()) != l:context.winwidth
        execute 'resize' l:context.winheight
      endif
    endif
  endif
endfunction"}}}
function! s:on_buf_unload(bufname)  "{{{
  " Save unite value.
  let s:current_unite = getbufvar(a:bufname, 'unite')
  let l:unite = s:current_unite

  if l:unite.is_finalized
    return
  endif

  " Restore options.
  if exists('&redrawtime')
    let &redrawtime = l:unite.redrawtime_save
  endif
  let &sidescrolloff = l:unite.sidescrolloff_save

  match

  if !l:unite.has_preview_window
    " Close preview window.
    pclose!
  endif

  if winnr('$') != 1
    execute l:unite.win_rest_cmd
  endif

  " Call finalize functions.
  call s:call_hook(unite#loaded_sources_list(), 'on_close')
  let l:unite.is_finalized = 1
endfunction"}}}

" Internal helper functions."{{{
function! s:adjustments(currentwinwidth, the_max_source_name, size)"{{{
  let l:max_width = a:currentwinwidth - a:the_max_source_name - a:size
  if l:max_width < 20
    return [a:currentwinwidth - a:size, 0]
  else
    return [l:max_width, a:the_max_source_name]
  endif
endfunction"}}}
function! s:extend_actions(self_func, action_table1, action_table2, ...)"{{{
  let l:filterd_table = s:filter_self_func(a:action_table2, a:self_func)

  if a:0 > 0
    for l:action in values(l:filterd_table)
      let l:action.from = a:1
    endfor
  endif

  return extend(a:action_table1, l:filterd_table, 'keep')
endfunction"}}}
function! s:filter_alias_action(action_table, alias_table, from)"{{{
  for [l:alias_name, l:alias_action] in items(a:alias_table)
    if l:alias_action ==# 'nop'
      if has_key(a:action_table, l:alias_name)
        " Delete nop action.
        call remove(a:action_table, l:alias_name)
      endif
    else
      let a:action_table[l:alias_name] = a:action_table[l:alias_action]
      let a:action_table[l:alias_name].from = a:from
    endif
  endfor
endfunction"}}}
function! s:filter_self_func(action_table, self_func)"{{{
  return filter(copy(a:action_table), printf("string(v:val.func) !=# \"function('%s')\"", a:self_func))
endfunction"}}}
function! s:take_action(action_name, candidate, is_parent_action)"{{{
  let l:candidate_head = type(a:candidate) == type([]) ?
        \ a:candidate[0] : a:candidate

  let l:action_table = unite#get_action_table(
        \ l:candidate_head.source, l:candidate_head.kind,
        \ unite#get_self_functions()[-3], a:is_parent_action)

  let l:action_name =
        \ a:action_name ==# 'default' ?
        \ unite#get_default_action(l:candidate_head.source, l:candidate_head.kind)
        \ : a:action_name

  if !has_key(l:action_table, a:action_name)
    " throw 'no such action ' . a:action_name
    return
  endif

  let l:action = l:action_table[a:action_name]
  " Convert candidates.
  call l:action.func(
        \ (l:action.is_selectable && type(a:candidate) != type([])) ?
        \ [a:candidate] : a:candidate)
endfunction"}}}
function! s:get_loaded_sources(...)"{{{
  let l:unite = unite#get_current_unite()
  return a:0 == 0 ? l:unite.sources : get(filter(copy(l:unite.sources), 'v:val.name ==# a:1'), 0, {})
endfunction"}}}
function! s:get_substitute_input(input)"{{{
  let l:input = a:input

  let l:unite = unite#get_current_unite()
  let l:substitute_patterns =
        \ unite#get_buffer_name_option(l:unite.buffer_name, 'substitute_patterns')
  if l:unite.input != '' && stridx(l:input, l:unite.input) == 0
    " Substitute after input.
    let l:input_save = l:input
    let l:subst = l:input_save[len(l:unite.input) :]
    let l:input = l:input_save[: len(l:unite.input)-1]
  else
    " Substitute all input.
    let l:subst = l:input
    let l:input = ''
  endif

  for l:pattern in reverse(unite#util#sort_by(values(l:substitute_patterns),
        \ 'v:val.priority'))
    let l:subst = substitute(l:subst, l:pattern.pattern, l:pattern.subst, 'g')
  endfor

  let l:input .= l:subst

  return l:input
endfunction"}}}
function! s:call_hook(sources, hook_name)"{{{
  for l:source in a:sources
    if has_key(l:source.hooks, a:hook_name)
      call call(l:source.hooks[a:hook_name], [l:source.args, l:source.unite__context], l:source.hooks)
    endif
  endfor
endfunction"}}}
function! s:is_cmdwin()"{{{
  silent! noautocmd wincmd p
  silent! noautocmd wincmd p
  return v:errmsg =~ '^E11:'
endfunction"}}}
"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: foldmethod=marker
autoload/vital/_a11647/data/list.vim	[[[1
96
" Utilities for list.

let s:save_cpo = &cpo
set cpo&vim

" Removes duplicates from a list.
" FIXME: string only.
function! s:uniq(list)  "{{{
  let i = 0
  let seen = {}
  while i < len(a:list)
    if has_key(seen, '_' . a:list[i])
      call remove(a:list, i)
    else
      " Avoid empty string for key of dictionary.
      let seen['_' . a:list[i]] = 1
      let i += 1
    endif
  endwhile
  return a:list
endfunction "}}}

" Concatenates a list of lists.
" XXX: Should we verify the input?
function! s:concat(list)  "{{{
  let list = []
  for Value in a:list
    let list += Value
  endfor
  return list
endfunction "}}}

" Flattens a list.
function! s:flatten(list)  "{{{
  let list = []
  for Value in a:list
    if type(Value) == type([])
      let list += s:flatten(Value)
    else
      call add(list, Value)
    endif
    unlet! Value
  endfor
  return list
endfunction "}}}

" Sorts a list with expression to compare each two values.
" a:a and a:b can be used in {expr}.
function! s:sort(list, expr)  "{{{
  if type(a:expr) == type(function('function'))
    return sort(a:list, a:expr)
  endif
  let s:expr = a:expr
  return sort(a:list, 's:_compare')
endfunction "}}}

function! s:_compare(a, b)  " {{{
  return eval(s:expr)
endfunction "}}}

" TODO: Use sort()'s {dict} argument which was introduced by 7.3.224
" Sorts a list using a set of keys generated by mapping the values in the list
" through the given expr.
" v:val is used in {expr}
function! s:sort_by(list, expr)  " {{{
  let pairs = map(a:list, printf('[v:val, %s]', a:expr))
  return map(s:sort(pairs,
  \      'a:a[1] ==# a:b[1] ? 0 : a:a[1] ># a:b[1] ? 1 : -1'), 'v:val[0]')
endfunction "}}}

" Returns List of character sequence between [a:from, a:to]
" e.g.: s:char_range('a', 'c') returns ['a', 'b', 'c']
function! s:char_range(from, to) " {{{
  return map(
  \   range(char2nr(a:from), char2nr(a:to)),
  \   'nr2char(v:val)'
  \)
endfunction "}}}

" Returns true if a:list has a:Value.
" Returns false otherwise.
function! s:has(list, Value) "{{{
  return index(a:list, a:Value) isnot -1
endfunction "}}}

" Returns true if a:list[a:index] exists.
" Returns false otherwise.
" NOTE: Returns false when a:index is negative number.
function! s:has_index(list, index) "{{{
    " Return true when negative index?
    " let index = a:index >= 0 ? a:index : len(a:list) + a:index
    return 0 <= a:index && a:index < len(a:list)
endfunction "}}}


let &cpo = s:save_cpo
autoload/vital/_a11647/data/ordered_set.vim	[[[1
91
" vim:foldmethod=marker:fen:
scriptencoding utf-8

" Saving 'cpoptions' {{{
let s:save_cpo = &cpo
set cpo&vim
" }}}


function! s:version() "{{{
    return '0.0.15'
endfunction "}}}

function! s:new(...) "{{{
    let obj = deepcopy(s:ordered_set)
    if a:0
    \   && type(a:1) == type({})
    \   && has_key(a:1, 'Fn_identifier')
        let obj.Fn_identifier = a:1.Fn_identifier
    endif
    return obj
endfunction "}}}


let s:ordered_set = {
\   '_list': [],
\   '_dict': {},
\   '_origin_pos': 0,
\   'Fn_identifier': 'string',
\}

function s:ordered_set.push(elem) "{{{
    let id = call(self.Fn_identifier, [a:elem])
    if !has_key(self._dict, id)
        let self._dict[id] = len(self._list) - self._origin_pos
        call add(self._list, a:elem)
    endif
endfunction "}}}

function! s:ordered_set.unshift(elem) "{{{
    let id = call(self.Fn_identifier, [a:elem])
    if !has_key(self._dict, id)
        let self._origin_pos += 1
        let self._dict[id] = -self._origin_pos
        call insert(self._list, a:elem)
    endif
endfunction "}}}

function! s:ordered_set.empty() "{{{
    return empty(self._list)
endfunction "}}}

function! s:ordered_set.size() "{{{
    return len(self._list)
endfunction "}}}

function! s:ordered_set.to_list() "{{{
    return copy(self._list)
endfunction "}}}

function! s:ordered_set.has(elem) "{{{
    let id = call(self.Fn_identifier, [a:elem])
    return has_key(self._dict, id)
endfunction "}}}

function! s:ordered_set.has_id(id) "{{{
    return has_key(self._dict, a:id)
endfunction "}}}

function! s:ordered_set.clear() "{{{
    let self._list = []
    let self._dict  = {}
    let self._origin_pos = 0
endfunction "}}}

function! s:ordered_set.remove(elem) "{{{
    let id = call(self.Fn_identifier, [a:elem])
    if has_key(self._dict, id)
        let idx = self._origin_pos + self._dict[id]
        unlet self._list[idx]
        if idx < self._origin_pos
            let self._origin_pos -= 1
        endif
        unlet self._dict[id]
    endif
endfunction "}}}


" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
" }}}
autoload/vital/_a11647/data/string.vim	[[[1
114
" Utilities for string.

let s:save_cpo = &cpo
set cpo&vim
let s:V = vital#{expand('<sfile>:h:h:t:r')}#new()

" Substitute a:from => a:to by string.
" To substitute by pattern, use substitute() instead.
" Test: https://gist.github.com/984296
function! s:replace(str, from, to)
    if a:str ==# '' || a:from ==# ''
        return a:str
    endif
    let str = a:str
    let idx = stridx(str, a:from)
    while idx !=# -1
        let left  = idx ==# 0 ? '' : str[: idx - 1]
        let right = str[idx + strlen(a:from) :]
        let str = left . a:to . right
        let idx = stridx(str, a:from)
    endwhile
    return str
endfunction

" Substitute a:from => a:to only once.
" cf. s:replace()
" Test: https://gist.github.com/984296
function! s:replace_once(str, from, to)
    if a:str ==# '' || a:from ==# ''
        return a:str
    endif
    let idx = stridx(a:str, a:from)
    if idx ==# -1
        return a:str
    else
        let left  = idx ==# 0 ? '' : a:str[: idx - 1]
        let right = a:str[idx + strlen(a:from) :]
        return left . a:to . right
    endif
endfunction

" Split to two elements of List. ([left, right])
" e.g.: s:split_leftright("neocomplcache", "compl") returns ["neo", "cache"]
" Test: https://gist.github.com/984356
function! s:split_leftright(haystack, needle)
    let ERROR = ['', '']
    if a:haystack ==# '' || a:needle ==# ''
        return ERROR
    endif
    let idx = stridx(a:haystack, a:needle)
    if idx ==# -1
        return ERROR
    endif
    let left  = idx ==# 0 ? '' : a:haystack[: idx - 1]
    let right = a:haystack[idx + strlen(a:needle) :]
    return [left, right]
endfunction

" Returns the number of character in a:str.
" NOTE: This returns proper value
" even if a:str contains multibyte character(s).
" s:strchars(str) {{{
if exists('*strchars')
    " TODO: Why can't I write like this?
    " let s:strchars = function('strchars')
    function! s:strchars(str)
        return strchars(a:str)
    endfunction
else
    function! s:strchars(str)
        return strlen(substitute(copy(a:str), '.', 'x', 'g'))
    endfunction
endif "}}}

" Remove last character from a:str.
" NOTE: This returns proper value
" even if a:str contains multibyte character(s).
function! s:chop(str) "{{{
    return substitute(a:str, '.$', '', '')
endfunction "}}}

" wrap() and its internal functions
" * _split_by_wcswitdh_once()
" * _split_by_wcswitdh()
" * _concat()
" * wrap()
"
" NOTE _concat() is just a copy of Data.List.concat().
" FIXME don't repeat yourself
function! s:_split_by_wcswitdh_once(body, x)
  return [
        \ s:V.strwidthpart(a:body, a:x),
        \ s:V.strwidthpart_reverse(a:body, s:V.wcswidth(a:body) - a:x)]
endfunction

function! s:_split_by_wcswitdh(body, x)
  let memo = []
  let body = a:body
  while s:V.wcswidth(body) > a:x
    let [tmp, body] = s:_split_by_wcswitdh_once(body, a:x)
    call add(memo, tmp)
  endwhile
  call add(memo, body)
  return memo
endfunction

function! s:wrap(str)
  let L = s:V.import('Data.List')
  return L.concat(
        \ map(split(a:str, '\r\?\n'), 's:_split_by_wcswitdh(v:val, &columns - 1)'))
endfunction


let &cpo = s:save_cpo
autoload/vital/_a11647/functor.vim	[[[1
77
" "Callable thing" in vital.

let s:save_cpo = &cpo
set cpo&vim


" [Callable Object] is one of the following values:
" - function name (String)
" - Funcref value
" - callable object
"
" [Functor] is a Dictionary which has the key "do" of Funcref value.
" Please note that `Functor.wrap([Callable Object]).do` is always Funcref value.
" So you can always call .do() method without checking return value of `Functor.wrap()`.
" e.g.: `Functor.wrap("").do()`


" The same arguments as call()
" but first argument is [Callable Object].
function! s:call(callable, args, ...)
    let functor = s:wrap(a:callable)
    return call(functor.do, a:args, (a:0 ? a:1 : functor))
endfunction

" Convert [Callable Object] to [Functor].
" NOTE: `s:wrap(callable).do` must be Funcref value.
let s:TYPE_STRING  = type("")
let s:TYPE_FUNCREF = type(function('tr'))
let s:TYPE_DICT    = type({})
function! s:wrap(callable)
    if type(a:callable) ==# s:TYPE_FUNCREF
        return {'do': a:callable}
    elseif type(a:callable) ==# s:TYPE_STRING
        return {'do': function(a:callable)}
    elseif type(a:callable) ==# s:TYPE_DICT
    \   && has_key(a:callable, 'do')
        if type(a:callable.do) ==# s:TYPE_FUNCREF
            return a:callable
        elseif type(a:callable.do) ==# s:TYPE_STRING
            return extend(a:callable, {
            \   'do': function(a:callable),
            \}, 'force')
        endif
    endif
    throw 'vital: Functor.wrap(): '
    \   . 'a:callable is not callable!'
endfunction

" Bind a:this to a:callable's `self`.
function! s:bind(callable, this)
    let this = copy(a:this)
    let this.do = s:wrap(a:callable).do
    return this
endfunction

" Curry a:callable's 1st argument with a:v.
function! s:curry(callable, v)
    return {
    \   'do': s:localfunc('__curry_stub', s:__sid()),
    \   '__functor': s:wrap(a:callable),
    \   '__value': a:v,
    \}
endfunction
function! s:__curry_stub(...) dict
    return s:call(self.__functor, [self.__value] + a:000)
endfunction
function! s:__sid()
    return matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze___sid$')
endfunction

" Convert script-local function to globally callable function.
function! s:localfunc(funcname, sid)
    return function(printf('<SNR>%d_%s', a:sid, a:funcname))
endfunction


let &cpo = s:save_cpo
autoload/vital/_a11647/mapping.vim	[[[1
147
" Utilities for keymapping.

let s:save_cpo = &cpo
set cpo&vim



" TODO:
" - parsing functions
" - s:split_to_keys() in arpeggio.vim
" - s:key2char() in eskk.vim
" - support maparg()'s {dict}
" - builder object: .lhs(), .rhs(), .modes(), ...
" - move functions from eskk.vim(autoload/eskk/map.vim), emap.vim(autoload/emap.vim)

" Variable name convention:
" maparg: Dictionary which maparg() returns when {dict} is true.
" dict: it differs a little from `maparg` above. it contains more keys like "unique", etc.
" chars: String that each character means option. e.g., "b" (which means <buffer>)
" raw: String that option passing to :map command's argument. e.g., "<buffer>"
" mode: a character which means current mode. see s:get_all_modes() for avaiable modes.
" lhs: :help {lhs}
" rhs: :help {rhs}


" Conversion of options: chars <-> dict <-> raw
" To convert `chars` to `raw`, it must convert to `dict` at first.



function! s:options_dict2raw(dict)
    return
    \   (get(a:dict, 'expr')     ? '<expr>' : '')
    \   . (get(a:dict, 'buffer') ? '<buffer>' : '')
    \   . (get(a:dict, 'silent') ? '<silent>' : '')
    \   . (get(a:dict, 'script') ? '<script>' : '')
    \   . (get(a:dict, 'unique') ? '<unique>' : '')
endfunction

function! s:options_dict2chars(dict)
    return
    \   (get(a:dict, 'expr')      ? 'e' : '')
    \   . (get(a:dict, 'buffer')  ? 'b' : '')
    \   . (get(a:dict, 'silent')  ? 's' : '')
    \   . (get(a:dict, 'script')  ? 'S' : '')
    \   . (get(a:dict, 'unique')  ? 'u' : '')
    \   . (get(a:dict, 'noremap') ? ''  : 'r')
endfunction

function! s:options_chars2raw(chars)
    return s:options_dict2raw(s:options_chars2dict(a:chars))
endfunction

function! s:options_chars2dict(chars)
    return {
    \   'expr': (stridx(a:chars, 'e') isnot -1),
    \   'buffer': (stridx(a:chars, 'b') isnot -1),
    \   'silent' : (stridx(a:chars, 's') isnot -1),
    \   'script' : (stridx(a:chars, 'S') isnot -1),
    \   'unique': (stridx(a:chars, 'u') isnot -1),
    \   'noremap': (stridx(a:chars, 'r') is -1),
    \}
endfunction



function! s:execute_map_command(mode, dict, lhs, rhs)
    " s:get_map_command() may return empty string for invalid arguments.
    " But :execute '' does not do anything.
    execute s:get_map_command(a:mode, a:dict, a:lhs, a:rhs)
endfunction

function! s:get_map_command(...)
    return call('s:__get_map_command', ['map'] + a:000)
endfunction

function! s:execute_abbr_command(mode, dict, lhs, rhs)
    " s:get_abbr_command() may return empty string for invalid arguments.
    " But :execute '' does not do anything.
    execute s:get_abbr_command(a:mode, a:dict, a:lhs, a:rhs)
endfunction

function! s:get_abbr_command(...)
    return call('s:__get_map_command', ['abbr'] + a:000)
endfunction

function! s:execute_unmap_command(mode, dict, lhs)
    " s:get_unmap_command() may return empty string for invalid arguments.
    " But :execute '' does not do anything.
    execute s:get_unmap_command(a:mode, a:dict, a:lhs)
endfunction

function! s:__get_map_command(type, mode, dict, lhs, rhs)
    if type(a:dict) != type({})
    \   || !s:is_mode_char(a:mode)
    \   || a:lhs ==# ''
    \   || a:rhs ==# ''
        return ''
    endif

    let noremap = get(a:dict, 'noremap', 0)
    return join([
    \   a:mode . (noremap ? 'nore' : '') . a:type,
    \   s:options_dict2raw(a:dict),
    \   a:lhs,
    \   a:rhs,
    \])
endfunction

function! s:get_unmap_command(...)
    return call('s:__get_unmap_command', ['unmap'] + a:000)
endfunction

function! s:get_unabbr_command(...)
    return call('s:__get_unmap_command', ['unabbr'] + a:000)
endfunction

function! s:__get_unmap_command(type, mode, dict, lhs)
    if type(a:dict) != type({})
    \   || !s:is_mode_char(a:mode)
    \   || a:lhs ==# ''
        return ''
    endif

    return join([
    \   a:mode . a:type,
    \   s:options_dict2raw(a:dict),
    \   a:lhs,
    \])
endfunction


function! s:get_all_modes()
    return 'nvoiclxs'
endfunction

function! s:get_all_modes_list()
    return split(s:get_all_modes(), '\zs')
endfunction

function! s:is_mode_char(char)
    return a:char =~# '^['.s:get_all_modes().']$'
endfunction



let &cpo = s:save_cpo
autoload/vital/_a11647/prelude.vim	[[[1
336

" glob() wrapper which returns List.
function! s:glob(...)
    let R = call('glob', a:000)
    return split(R, '\n')
endfunction
" globpath() wrapper which returns List.
function! s:globpath(...)
    let R = call('globpath', a:000)
    return split(R, '\n')
endfunction

" Wrapper functions for type().
let [
\   s:__TYPE_NUMBER,
\   s:__TYPE_STRING,
\   s:__TYPE_FUNCREF,
\   s:__TYPE_LIST,
\   s:__TYPE_DICT,
\   s:__TYPE_FLOAT
\] = [
\   type(3),
\   type(""),
\   type(function('tr')),
\   type([]),
\   type({}),
\   type(3.14159)
\]
" Number or Float
function! s:is_numeric(Value)
    let _ = type(a:Value)
    return _ ==# s:__TYPE_NUMBER
    \   || _ ==# s:__TYPE_FLOAT
endfunction
" Number
function! s:is_integer(Value)
    return type(a:Value) ==# s:__TYPE_NUMBER
endfunction
function! s:is_number(Value)
    return type(a:Value) ==# s:__TYPE_NUMBER
endfunction
" Float
function! s:is_float(Value)
    return type(a:Value) ==# s:__TYPE_FLOAT
endfunction
" String
function! s:is_string(Value)
    return type(a:Value) ==# s:__TYPE_STRING
endfunction
" Funcref
function! s:is_funcref(Value)
    return type(a:Value) ==# s:__TYPE_FUNCREF
endfunction
" List
function! s:is_list(Value)
    return type(a:Value) ==# s:__TYPE_LIST
endfunction
" Dictionary
function! s:is_dict(Value)
    return type(a:Value) ==# s:__TYPE_DICT
endfunction

function! s:truncate_smart(str, max, footer_width, separator)"{{{
  let width = s:wcswidth(a:str)
  if width <= a:max
    let ret = a:str
  else
    let header_width = a:max - s:wcswidth(a:separator) - a:footer_width
    let ret = s:strwidthpart(a:str, header_width) . a:separator
          \ . s:strwidthpart_reverse(a:str, a:footer_width)
  endif

  return s:truncate(ret, a:max)
endfunction"}}}

function! s:truncate(str, width)"{{{
  " Original function is from mattn.
  " http://github.com/mattn/googlereader-vim/tree/master

  if a:str =~# '^[\x00-\x7f]*$'
    return len(a:str) < a:width ?
          \ printf('%-'.a:width.'s', a:str) : strpart(a:str, 0, a:width)
  endif

  let ret = a:str
  let width = s:wcswidth(a:str)
  if width > a:width
    let ret = s:strwidthpart(ret, a:width)
    let width = s:wcswidth(ret)
  endif

  if width < a:width
    let ret .= repeat(' ', a:width - width)
  endif

  return ret
endfunction"}}}

function! s:strchars(str)"{{{
  return len(substitute(a:str, '.', 'x', 'g'))
endfunction"}}}

function! s:strwidthpart(str, width)"{{{
  let ret = a:str
  let width = s:wcswidth(a:str)
  while width > a:width
    let char = matchstr(ret, '.$')
    let ret = ret[: -1 - len(char)]
    let width -= s:wcswidth(char)
  endwhile

  return ret
endfunction"}}}
function! s:strwidthpart_reverse(str, width)"{{{
  let ret = a:str
  let width = s:wcswidth(a:str)
  while width > a:width
    let char = matchstr(ret, '^.')
    let ret = ret[len(char) :]
    let width -= s:wcswidth(char)
  endwhile

  return ret
endfunction"}}}

if v:version >= 703
  " Use builtin function.
  function! s:wcswidth(str)"{{{
    return strwidth(a:str)
  endfunction"}}}
else
  function! s:wcswidth(str)"{{{
    if a:str =~# '^[\x00-\x7f]*$'
      return strlen(a:str)
    end

    let mx_first = '^\(.\)'
    let str = a:str
    let width = 0
    while 1
      let ucs = char2nr(substitute(str, mx_first, '\1', ''))
      if ucs == 0
        break
      endif
      let width += s:_wcwidth(ucs)
      let str = substitute(str, mx_first, '', '')
    endwhile
    return width
  endfunction"}}}

  " UTF-8 only.
  function! s:_wcwidth(ucs)"{{{
    let ucs = a:ucs
    if (ucs >= 0x1100
          \  && (ucs <= 0x115f
          \  || ucs == 0x2329
          \  || ucs == 0x232a
          \  || (ucs >= 0x2e80 && ucs <= 0xa4cf
          \      && ucs != 0x303f)
          \  || (ucs >= 0xac00 && ucs <= 0xd7a3)
          \  || (ucs >= 0xf900 && ucs <= 0xfaff)
          \  || (ucs >= 0xfe30 && ucs <= 0xfe6f)
          \  || (ucs >= 0xff00 && ucs <= 0xff60)
          \  || (ucs >= 0xffe0 && ucs <= 0xffe6)
          \  || (ucs >= 0x20000 && ucs <= 0x2fffd)
          \  || (ucs >= 0x30000 && ucs <= 0x3fffd)
          \  ))
      return 2
    endif
    return 1
  endfunction"}}}
endif

let s:is_windows = has('win16') || has('win32') || has('win64')
let s:is_cygwin = has('win32unix')
let s:is_mac = !s:is_windows && (has('mac') || has('macunix') || has('gui_macvim') || system('uname') =~? '^darwin')
function! s:is_windows()"{{{
  return s:is_windows
endfunction"}}}
function! s:is_cygwin()"{{{
  return s:is_cygwin
endfunction"}}}
function! s:is_mac()"{{{
  return s:is_mac
endfunction"}}}

function! s:print_error(message)"{{{
  echohl ErrorMsg
  for m in split(a:message, "\n")
    echomsg m
  endfor
  echohl None
endfunction"}}}

function! s:smart_execute_command(action, word)"{{{
  execute a:action . ' ' . (a:word == '' ? '' : '`=a:word`')
endfunction"}}}

function! s:escape_file_searching(buffer_name)"{{{
  return escape(a:buffer_name, '*[]?{},')
endfunction"}}}
function! s:escape_pattern(str)"{{{
  return escape(a:str, '~"\.^$[]*')
endfunction"}}}
" iconv() wrapper for safety.
function! s:iconv(expr, from, to)
  if a:from == '' || a:to == '' || a:from ==? a:to
    return a:expr
  endif
  let result = iconv(a:expr, a:from, a:to)
  return result != '' ? result : a:expr
endfunction
" Like builtin getchar() but returns string always.
function! s:getchar(...)
  let c = call('getchar', a:000)
  return type(c) == type(0) ? nr2char(c) : c
endfunction
" Like builtin getchar() but returns string always.
" and do inputsave()/inputrestore() before/after getchar().
function! s:getchar_safe(...)
  let c = s:input_helper('getchar', a:000)
  return type(c) == type("") ? c : nr2char(c)
endfunction
" Like builtin getchar() but
" do inputsave()/inputrestore() before/after input().
function! s:input_safe(...)
    return s:input_helper('input', a:000)
endfunction
" Do inputsave()/inputrestore() before/after calling a:funcname.
function! s:input_helper(funcname, args)
    let success = 0
    if inputsave() !=# success
        throw 'inputsave() failed'
    endif
    try
        return call(a:funcname, a:args)
    finally
        if inputrestore() !=# success
            throw 'inputrestore() failed'
        endif
    endtry
endfunction

function! s:set_default(var, val)  "{{{
  if !exists(a:var) || type({a:var}) != type(a:val)
    let {a:var} = a:val
  endif
endfunction"}}}
function! s:set_dictionary_helper(variable, keys, pattern)"{{{
  for key in split(a:keys, ',')
    if !has_key(a:variable, key)
      let a:variable[key] = a:pattern
    endif
  endfor
endfunction"}}}
function! s:substitute_path_separator(path)"{{{
  return s:is_windows ? substitute(a:path, '\\', '/', 'g') : a:path
endfunction"}}}
function! s:path2directory(path)"{{{
  return s:substitute_path_separator(isdirectory(a:path) ? a:path : fnamemodify(a:path, ':p:h'))
endfunction"}}}
function! s:path2project_directory(path)"{{{
  let l:search_directory = s:path2directory(a:path)
  let l:directory = ''

  " Search VCS directory.
  for d in ['.git', '.bzr', '.hg']
    let d = finddir(d, s:escape_file_searching(l:search_directory) . ';')
    if d != ''
      let l:directory = fnamemodify(d, ':p:h:h')
      break
    endif
  endfor

  " Search project file.
  if l:directory == ''
    for d in ['build.xml', 'prj.el', '.project', 'pom.xml', 'Makefile', 'configure', 'Rakefile', 'NAnt.build', 'tags', 'gtags']
      let d = findfile(d, s:escape_file_searching(l:search_directory) . ';')
      if d != ''
        let l:directory = fnamemodify(d, ':p:h')
        break
      endif
    endfor
  endif

  if l:directory == ''
    " Search /src/ directory.
    let l:base = s:substitute_path_separator(l:search_directory)
    if l:base =~# '/src/'
      let l:directory = l:base[: strridx(l:base, '/src/') + 3]
    endif
  endif

  if l:directory == ''
    let l:directory = l:search_directory
  endif

  return s:substitute_path_separator(l:directory)
endfunction"}}}
" Check vimproc."{{{
let s:exists_vimproc = globpath(&rtp, 'autoload/vimproc.vim') != ''
"}}}
function! s:has_vimproc()"{{{
  return s:exists_vimproc
endfunction"}}}
function! s:system(str, ...)"{{{
  let l:command = a:str
  let l:input = a:0 >= 1 ? a:1 : ''
  if &termencoding != '' && &termencoding != &encoding
    let l:command = s:iconv(l:command, &encoding, &termencoding)
    let l:input = s:iconv(l:input, &encoding, &termencoding)
  endif

  if a:0 == 0
    let l:output = s:has_vimproc() ?
          \ vimproc#system(l:command) : system(l:command)
  elseif a:0 == 1
    let l:output = s:has_vimproc() ?
          \ vimproc#system(l:command, l:input) : system(l:command, l:input)
  else
    " ignores 3rd argument unless you have vimproc.
    let l:output = s:has_vimproc() ?
          \ vimproc#system(l:command, l:input, a:2) : system(l:command, l:input)
  endif

  if &termencoding != '' && &termencoding != &encoding
    let l:output = s:iconv(l:output, &termencoding, &encoding)
  endif

  return l:output
endfunction"}}}
function! s:get_last_status()"{{{
  return s:has_vimproc() ?
        \ vimproc#get_last_status() : v:shell_error
endfunction"}}}
" vim: foldmethod=marker
autoload/vital/_a11647/system/file.vim	[[[1
123
" Utilities for file copy/move/mkdir/etc.

let s:save_cpo = &cpo
set cpo&vim

let s:is_windows = has('win16') || has('win32') || has('win64')
let s:is_cygwin = has('win32unix')
let s:is_mac = !s:is_windows && (has('mac') || has('macunix') || has('gui_macvim') || system('uname') =~? '^darwin')

" Open a file.
function! s:open(filename) "{{{
  let filename = iconv(fnamemodify(a:filename, ':p'),
        \ &encoding, &termencoding)

  " Detect desktop environment.
  if s:is_windows
    " For URI only.
    silent execute '!start rundll32 url.dll,FileProtocolHandler' l:filename
  elseif s:is_cygwin
    " Cygwin.
    call system(printf('%s ''%s''', 'cygstart', l:filename))
  elseif executable('xdg-open')
    " Linux.
    call system(printf('%s ''%s'' &', 'xdg-open', l:filename))
  elseif exists('$KDE_FULL_SESSION') && $KDE_FULL_SESSION ==# 'true'
    " KDE.
    call system(printf('%s ''%s'' &', 'kioclien exec', l:filename))
  elseif exists('$GNOME_DESKTOP_SESSION_ID')
    " GNOME.
    call system(printf('%s ''%s'' &', 'gnome-open', l:filename))
  elseif executable('exo-open')
    " Xfce.
    call system(printf('%s ''%s'' &', 'exo-open', l:filename))
  elseif s:is_mac && executable('open')
    " Mac OS.
    call system(printf('%s ''%s'' &', 'open', l:filename))
  else
    " Give up.
    throw 'Not supported.'
  endif
endfunction "}}}


" Move a file.
" Dispatch s:move_file_exe() or s:move_file_pure().
function! s:move_file(src, dest) "{{{
    if executable('mv')
        return s:move_file_exe(a:src, a:dest)
    else
        return s:move_file_pure(a:src, a:dest)
    endif
endfunction "}}}

" Move a file.
" Implemented by 'mv' executable.
" TODO: Support non-*nix like system.
function! s:move_file_exe(src, dest)
    if !executable('mv') | return 0 | endif
    silent execute '!mv' shellescape(a:src) shellescape(a:dest)
    if v:shell_error
        return 0
    endif
    return 1
endfunction

" Move a file.
" Implemented by pure vimscript.
function! s:move_file_pure(src, dest) "{{{
    let copy_success = s:copy_file(a:src, a:dest)
    let remove_success = delete(a:src) == 0
    if copy_success && remove_success
        return 1
    else
        return 0
    endif
endfunction "}}}

" Copy a file.
" Dispatch s:copy_file_exe() or s:copy_file_pure().
function! s:copy_file(src, dest) "{{{
    if executable('cp')
        return s:copy_file_exe(a:src, a:dest)
    else
        return s:copy_file_pure(a:src, a:dest)
    endif
endfunction "}}}

" Copy a file.
" Implemented by 'cp' executable.
" TODO: Support non-*nix like system.
function! s:copy_file_exe(src, dest)
    if !executable('cp') | return 0 | endif
    silent execute '!cp' shellescape(a:src) shellescape(a:dest)
    if v:shell_error
        return 0
    endif
    return 1
endfunction

" Copy a file.
" Implemented by pure vimscript.
function! s:copy_file_pure(src, dest) "{{{
    let ret = writefile(readfile(a:src, "b"), a:dest, "b")
    if ret == -1
        return 0
    endif
    return 1
endfunction "}}}

" mkdir() but does not throw an exception.
" Returns true if success.
" Returns false if failure.
function! s:mkdir_nothrow(...) "{{{
    try
        call call('mkdir', a:000)
        return 1
    catch
        return 0
    endtry
endfunction "}}}


let &cpo = s:save_cpo
autoload/vital/_a11647/system/filepath.vim	[[[1
90
" You should check the following related builtin functions.
" fnamemodify()
" resolve()
" simplify()

let s:save_cpo = &cpo
set cpo&vim

let s:path_sep_pattern = (exists('+shellslash') ? '[\\/]' : '/') . '\+'
let s:is_windows = has('win16') || has('win32') || has('win64')
let s:is_cygwin = has('win32unix')
let s:is_mac = !s:is_windows && (has('mac') || has('macunix') || has('gui_macvim') || system('uname') =~? '^darwin')


" Get the path separator.
function! s:separator()
  return !exists('+shellslash') || &shellslash ? '/' : '\'
endfunction

" Convert all path separators to "/".
function! s:unify_separator(path)
  return substitute(a:path, s:path_sep_pattern, '/', 'g')
endfunction

" Split the path with path separator.
" Note that this includes the drive letter of MS Windows.
function! s:split(path)
  return split(a:path, s:path_sep_pattern)
endfunction

" Join the paths.
" join('foo', 'bar')            => 'foo/bar'
" join('foo/', 'bar')           => 'foo/bar'
" join('/foo/', ['bar', 'buz/']) => '/foo/bar/buz/'
function! s:join(...)
  let sep = s:separator()
  let path = ''
  for part in a:000
    let path .= sep .
    \ (type(part) is type([]) ? call('s:join', part) :
    \                           part)
    unlet part
  endfor
  return substitute(path[1 :], s:path_sep_pattern, sep, 'g')
endfunction

" Check if the path is absolute path.
if s:is_windows
  function! s:is_absolute(path)
    return a:path =~? '^[a-z]:[/\]'
  endfunction
else
  function! s:is_absolute(path)
    return a:path[0] ==# '/'
  endfunction
endif

" Return the parent directory of the path.
" NOTE: fnamemodify(path, ':h') does not return the parent directory
" when path[-1] is the separator.
function! s:dirname(path)
  let path = a:path
  let orig = a:path

  let path = s:remove_last_separator(path)
  if path == ''
    return orig    " root directory
  endif

  let path = fnamemodify(path, ':h')
  return path
endfunction

" Remove the separator at the end of a:path.
function! s:remove_last_separator(path)
  let sep = s:separator()
  let pat = (sep == '\' ? '\\' : '/') . '\+$'
  return substitute(a:path, pat, '', '')
endfunction


" Return true if filesystem ignores alphabetic case of a filename.
" Return false otherwise.
let s:is_case_tolerant = s:is_windows || s:is_cygwin || s:is_mac
function! s:is_case_tolerant()
  return s:is_case_tolerant
endfunction


let &cpo = s:save_cpo
autoload/vital/_a11647.vim	[[[1
114
let s:base_dir = expand('<sfile>:r')
let s:self_version = expand('<sfile>:t:r')

let s:loaded = {}

function! s:import(name, ...)
  let module = s:_import(a:name, s:_scripts())
  if a:0 && type(a:1) == type({})
    call extend(a:1, module, 'keep')
  endif
  return module
endfunction

function! s:load(...) dict
  let scripts = s:_scripts()
  for name in a:000
    let target = split(name, '\W\+')
    let dict = self
    while 2 <= len(target)
      let ns = remove(target, 0)
      if !has_key(dict, ns)
        let dict[ns] = {}
      endif
      if type(dict[ns]) == type({})
        let dict = dict[ns]
      else
        let target = []
      endif
    endwhile

    if !empty(target) && !has_key(dict, target[0])
      let dict[target[0]] = s:_import(name, scripts)
    endif
  endfor
  return self
endfunction

function! s:_import(name, scripts)
  if type(a:name) == type(0)
    return s:_build_module(a:name)
  endif
  if a:name =~# '^[^A-Z]' || a:name =~# '\W[^A-Z]'
    throw 'vital: module name must start with capital letter: ' . a:name
  endif
  let target = a:name == '' ? '' : '/' . substitute(a:name, '\W\+', '/', 'g')
  let target = substitute(target, '\l\zs\ze\u', '_', 'g') " OrderedSet -> Ordered_Set
  let target = substitute(target, '[/_]\zs\u', '\l\0', 'g') " Ordered_Set -> ordered_set
  let target = s:base_dir . target . '.vim'
  let sid = get(a:scripts, s:_unify_path(target), 0)
  if !sid
    try
      source `=target`
    catch /^Vim\%((\a\+)\)\?:E484/
      throw 'vital: module not found: ' . a:name
    endtry
    let sid = len(a:scripts) + 1  " We expect that the file newly read is +1.
  endif
  return s:_build_module(sid)
endfunction

function! s:_scripts()
  let scripts = {}
  for line in split(s:_redir('scriptnames'), "\n")
    let list = matchlist(line, '^\s*\(\d\+\):\s\+\(.\+\)\s*$')
    if !empty(list)
      let scripts[s:_unify_path(list[2])] = list[1] - 0
    endif
  endfor
  return scripts
endfunction

function! s:_unify_path(path)
  return fnamemodify(resolve(a:path), ':p:gs?\\\+?/?')
endfunction

function! s:_build_module(sid)
  if has_key(s:loaded, a:sid)
    return copy(s:loaded[a:sid])
  endif
  let prefix = '<SNR>' . a:sid . '_'
  let funcs = s:_redir('function')
  let filter_pat = '^\s*function ' . prefix
  let map_pat = prefix . '\zs\w\+'
  let functions = map(filter(split(funcs, "\n"), 'v:val =~# filter_pat'),
  \          'matchstr(v:val, map_pat)')

  let module = {}
  for func in functions
    let module[func] = function(prefix . func)
  endfor
  if has_key(module, '_vital_loaded')
    try
      call module._vital_loaded(vital#{s:self_version}#new())
    catch
      " FIXME: Show the error message for debug.
    endtry
  endif
  call filter(module, 'v:key =~# "^\\a"')
  let s:loaded[a:sid] = module
  return copy(module)
endfunction

function! s:_redir(cmd)
  redir => res
    silent! execute a:cmd
  redir END
  return res
endfunction

function! vital#{s:self_version}#new()
  let V = s:import('')
  call V.import('Prelude', V)
  return V
endfunction
autoload/vital/unite.vital	[[[1
1
a11647
autoload/vital.vim	[[[1
12
function! vital#of(name)
  let files = globpath(&runtimepath, 'autoload/vital/' . a:name . '.vital')
  let file = split(files, "\n")
  if empty(file)
    throw 'vital: version file not found: ' . a:name
  endif
  let ver = readfile(file[0], 'b')
  if empty(ver)
    throw 'vital: invalid version file: ' . a:name
  endif
  return vital#_{ver[0]}#new()
endfunction
doc/unite.jax	[[[1
1980
*unite.txt*	すべてのsourceを統合する

Version: 2.2
Author : Shougo <Shougo.Matsu@gmail.com>
Japanese Documentation Author: naoina
License: MIT license  {{{
    Permission is hereby granted, free of charge, to any person obtaining
    a copy of this software and associated documentation files (the
    "Software"), to deal in the Software without restriction, including
    without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to
    permit persons to whom the Software is furnished to do so, subject to
    the following conditions:
    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
    OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}}}

目次						*unite-contents*

概要			|unite-introduction|
使い方			|unite-usage|
インストール		|unite-install|
インターフェース	|unite-interface|
  コマンド		  |unite-commands|
  変数			  |unite-variables|
    sourceの変数	    |unite-sources-variables|
  キーマッピング	  |unite-key-mappings|
  関数			  |unite-functions|
  source		  |unite-sources|
  kind			  |unite-kinds|
  アクション		  |unite-actions|
  filter		  |unite-filters|
sourceの作成		|unite-create-source|
kindの作成		|unite-create-kind|
filterの作成		|unite-create-filter|
設定例			|unite-examples|
ユーザーのsource	|unite-user-sources|
更新履歴		|unite-changelog|

==============================================================================
概要						*unite-introduction*

*unite* (または *unite.vim* ) はファイル、バッファ、最近使用したファイル、レジスタといった、任意の情
報源からの検索、表示を行う。表示された候補に対して、候補のkindごとに定義された
アクションを実行することができる。

似たプラグインとして、|fuzzyfinder|や|ku|があるが、|unite| はVimの補完インタフ
ェースを使用しないということ、sourceの統合を行うことが違いとして挙げられる。

==============================================================================
使い方						*unite-usage*

	sourceとしてファイル及び、バッファを指定して起動する場合
>
	:Unite file buffer
<

	絞込みテキストとしてhogeを入力した状態で起動する場合
>
	:Unite -input=hoge file
<

uniteを起動すると、デフォルトの状態ではウィンドウが水平に分割され、デフォルトで
は上側にuniteのバッファが開かれる。例えば
>
	:Unite file
<
として起動した場合、カレントディレクトリのファイルの一覧が表示されているはずで
ある。その状態で j、k、で候補を選択できるので、適当に候補を選び、Enter を押せ
ば選択した候補が新しいバッファで開かれる。これは選択した候補のkindがファイルで
あった場合の、デフォルトのアクションである。候補を選択して <Tab> を押下すると
候補に対するアクションが選択できる。アクションについては|unite-action|を参照。

候補が多い場合などはキーワードによって絞り込むことができる。前回と同じように
起動した後に、i を押下するとカーソルが2行目の > の右側に移る。これで絞込みテ
キストが入力できる。この状態で文字を入力すると、1文字入力するごとに候補が絞り
込まれていく。また * (ワイルドカード)を入力すると任意の文字で絞り込むことがで
きる。例えば
>
	*hoge
<
上記は hoge、ahoge、foohoge などにマッチする。また、ワイルドカードを2つ続けて
入力すると、ディレクトリを再帰的に指定できる。例えば
>
	**/foo
<
上記は hoge/foo、hoge/fuga/foo などにマッチする。ただし上記コマンドは実際には
後述する|unite-source-file_rec|を用いた方が良い。

絞り込み文字列はスペース区切りで複数指定できる。例えば
>
	hoge piyo
<
上記は hogeにマッチかつ、piyoにマッチする候補にマッチする。

除外パターンを!で記述できる。例えば
>
	hoge !piyo
<
上記は hogeにマッチするが、piyoにマッチする候補は除外される。

	-buffer-nameオプションにfilesを設定すると、/区切りでワイルドカードが自
	動的に付加される。これはファイルをuniteで選択する際に便利である。
>
	:Unite -buffer-name=files file
<
その他の操作については|unite_default_key_mappings|を参照。

参考：ujihisa氏の作成したScreencast
http://www.ustream.tv/recorded/11240673

==============================================================================
インストール					*unite-install*

配布ファイルをVimスクリプトのディレクトリへインストールする。
(普通は ~/.vim/ またはWindowsの場合、 $HOME/vimfiles)

インストールに成功すると、sourceを引数に指定して、|:Unite|コマンドでuniteを実
行することができる。しかしながら毎回このコマンドを実行するのは非常に手間である
ため、何かしらのキーマッピングを定義すると良いだろう。

==============================================================================
インターフェース				*unite-interface*

------------------------------------------------------------------------------
コマンド 					*unite-commands*

:Unite [{options}] {sources}			*:Unite*
		{sources} で指定したsourceでの候補ウィンドウを表示する。初期の
		絞込みテキストは空。{sources} はスペースで区切って複数指定でき
		る。また、{sources} の指定順序がsourceの候補の表示順序となる。
		sourceについては|unite-source|を参照。
		このコマンドは前回の位置を保存する。

		現在uniteバッファに居る場合、絞り込みテキストは保存される。

		source名の後ろに:で区切った文字列のリストを引数として渡すことが
		できる。引数に含まれる:と\は\でエスケープしなければならな
		い。::は引数の省略を表す。引数がどう解釈されるかは、sourceに
		よって異なる。

		例：
		"file:hoge:piyo": source fileに与える引数は
		                  ["hoge", "piyo"]である。
		"file:hoge\:piyo": source fileに与える引数は
		                  ["hoge:piyo"]である。
		"file:hoge::piyo": source fileに与える引数は
		                  ["hoge", "", "piyo"]である。

						*unite-options*
		{options}とはuniteバッファに与えるオプションである。
		オプションは、次のパラメータを取る。値にスペースが含まれるとき
		は、\でエスケープしなければならない。

						*unite-options-buffer-name*
		-buffer-name={buffer-name}
		uniteのバッファ名を指定する。同じ目的を持つuniteバッファに同じ
		バッファ名を付けることで、|unite#set_substitute_pattern()|など
		の設定を共通化することができる。指定しないと、'default'が使われ
		る。

						*unite-options-input*
		-input={input-text}
		初期の絞込みテキストを指定する。指定しないと、''が使われる。

						*unite-options-prompt*
		-prompt={prompt-text}
		プロンプトを指定する。指定しないと、'> 'が使われる。

						*unite-options-default-action*
		-default-action={default-action}
		デフォルトアクションを指定する。指定しないと、'default'が使われ
		る。

						*unite-options-start-insert*
						*unite-options-no-start-insert*
		-start-insert
		初期状態が絞り込みモードになる。

		-no-start-insert
		初期状態がNormal modeになる。

		両方のオプションを指定しないと、初期状態は
		|g:unite_enable_start_insert|に依存する。
		両方のオプションが指定されたときの動作は未定義である。

						*unite-options-no-quit*
		-no-quit
		アクションを実行しても、uniteバッファを閉じない。指定しないと、
		"is_quit"なアクションを実行するとuniteバッファは閉じられる。

						*unite-options-winwidth*
		-winwidth={window-width}
		uniteバッファの幅を指定する。指定しないと、|g:unite_winwidth|が
		使われる。

						*unite-options-winheight*
		-winheight={window-height}
		uniteバッファの高さを指定する。指定しないと、
		|g:unite_winheight|が使われる。

						*unite-options-immediately*
		-immediately
		候補が一つしかないとき、即座にアクションを実行する。候補がな
		いときにはuniteバッファを開かない。

						*unite-options-auto-preview*
		-auto-preview
		候補を選択した際、自動的に"preview"アクションを実行する。

						*unite-options-completion*
		-completion
		uniteの補完インタフェースを使用する。|unite-options-col|を同
		時に設定しなければならない。

						*unite-options-col*
		-col={column-number}
		uniteを開始した位置を指定する。

						*unite-options-vertical*
		-vertical
		uniteバッファを垂直分割する。

						*unite-options-auto-resize*
		-auto-resize
		候補数に応じて、uniteバッファを自動的にリサイズする。

						*unite-options-horizontal*
		-horizontal
		uniteバッファを水平分割する。
		
		両方のオプションを指定しないと、初期状態は
		|g:unite_enable_split_vertically|に依存する。両方のオプションが
		指定されたときの動作は未定義である。

						*unite-options-direction*
		-direction={direction}
		uniteバッファの分割方法を指定する。デフォルト値は
		|g:unite_split_rule|と同じである。

						*unite-options-verbose*
		-verbose
		冗長な警告メッセージを出すようにする。

:UniteWithCurrentDir [{options}] {sources}	*:UniteWithCurrentDir*
		初期の絞込みテキストが現在のディレクトリである他は|:Unite|と同
		一である。

:UniteWithBufferDir [{options}] {sources}	*:UniteWithBufferDir*
		初期の絞込みテキストが現在のバッファのディレクトリである他は
		|:Unite|と同一である。

:UniteWithCursorWord [{options}] {sources}	*:UniteWithCursorWord*
		初期の絞込みテキストが現在のカーソル位置文字列である他は
		|:Unite|と同一である。

:UniteWithInput [{options}] {sources}		*:UniteWithInput*
		初期の絞込みテキストをユーザーが入力する他は|:Unite|と同一であ
		る。

:UniteWithInputDirectory [{options}] {sources}	
						*:UniteWithInputDirectory*
		|:UniteWithInput|と同じであるが、入力する文字列はデイレクトリと
		みなされ、自動的に変換される。

:UniteResume [{buffer-name}]			*:UniteResume*
		以前開いた{buffer-name}という名前のuniteバッファを再利用する。
		{buffer-name}を省略すると、以前使用したバッファを再利用する。
		絞込みテキストや候補はそのままの状態となる。

sourceのコマンド				*unite-sources-commands*

:UniteBookmarkAdd [{file}]			*:UniteBookmarkAdd*
		ブックマークリストにファイルを追加する。引数が指定されなかった
		場合、現在のファイルの現在位置が記録される。

------------------------------------------------------------------------------
変数						*unite-variables*

g:unite_update_time				*g:unite_update_time*
		絞込みテキストを入力するごとに表示される候補の更新間隔を制御す
		る。単位はミリ秒。
		
		初期値は200である。

g:unite_enable_start_insert			*g:unite_enable_start_insert*
		uniteを実行したあとすぐに、絞込みテキストの入力モードにするかを
		制御する。1ならば有効になる。
		
		初期値は0である。

g:unite_split_rule				*g:unite_split_rule*
		uniteのウィンドウを生成する際、画面分割の位置ルールを指定する。
		
		初期値は"topleft"である。

g:unite_enable_split_vertically			*g:unite_enable_split_vertically*
		uniteのウィンドウを垂直分割にするかどうかを制御する。1ならば垂
		直分割にする。
		
		初期値は0なので、水平分割である。

g:unite_winheight				*g:unite_winheight*
		uniteのウィンドウが水平分割されたときの高さを指定する。垂直分割
		の場合は無視される。
		
		初期値は20である。

g:unite_winwidth				*g:unite_winwidth*
		uniteのウィンドウが垂直分割されたときの幅を指定する。水平分割の
		場合は無視される。
		
		初期値は90である。

g:unite_cd_command				*g:unite_cd_command*
		cdアクションで実行されるVimのコマンドを指定する。
		|`=|を解釈するコマンドでなければならない。
		
		初期値は"cd"である。

g:unite_lcd_command				*g:unite_lcd_command*
		lcdアクションで実行されるVimのコマンドを指定する。
		|`=|を解釈するコマンドでなければならない。
		
		初期値は"lcd"である。

g:unite_cursor_line_highlight			*g:unite_cursor_line_highlight*
		カーソル行のハイライトを指定する。
		
		初期値は"PmenuSel"である。

g:unite_abbr_highlight				*g:unite_abbr_highlight*
		候補の短縮文字列のハイライトを指定する。
		
		初期値は"Normal"である。

g:unite_quick_match_table			*g:unite_quick_match_table*
		入力文字と対応する、クイックマッチリストの補完候補のテーブルで
		ある。
		
		初期値は複雑なので、plugin/unite.vimを参照せよ。

g:unite_data_directory				*g:unite_data_directory*
		uniteやそのsourceが内部で使用する設定ファイルを書き出すディレク
		トリを指定する。ここで指定したディレクトリが実際に存在しない場
		合、自動的に作成される。例えばfile_mruのsourceは最近使用したフ
		ァイルの情報をこの下に保存する。
		
		初期値は'~/.unite'の絶対パスである。

g:unite_no_default_keymappings			*g:unite_no_default_keymappings*
		この変数の値を1にすれば、uniteが予め用意しているキーマッピング
		を設定しない。混乱するので、特に理由がない限り、通常は有効にす
		べきではない。
		
		この変数はユーザーが自分で定義しない限り存在しない。

sourceの変数					*unite-sources-variables*

g:unite_source_file_ignore_pattern		*g:unite_source_file_ignore_pattern*
		|unite-source-file|の候補に表示しないファイルの正規表現パター
		ンを指定する。マッチングはファイルのフルパスに対して行われる。
		この変数が空文字列以外であれば、指定した正規表現で結果をフィル
		タリングする。

		初期値は autoload/unite/sources/file.vim を参照。

g:unite_source_file_mru_time_format		*g:unite_source_file_mru_time_format*
		|unite-source-file_mru|の最終アクセス時間の表示フォーマット
		を指定する。フォーマットは|strftime()|で指定できるものと同じ
		である。
		
		初期値は"(%c) "である。

g:unite_source_file_mru_filename_format		*g:unite_source_file_mru_filename_format*
		|unite-source-file_mru|のファイル名の表示フォーマットを指定
		する。フォーマットは|fnamemodify()|で指定できるものと同じで
		ある。もしこの変数の値が空だと、表示スピードが高速化される。

		初期値は":~:."である。

g:unite_source_file_mru_file			*g:unite_source_file_mru_file*
		最近使用したファイルの情報を書き出すファイルを指定する。
		
		初期値は |g:unite_data_directory| . '/.file_mru' である。

g:unite_source_file_mru_limit			*g:unite_source_file_mru_limit*
		最近使用したファイルの最大保存件数を指定する。
		
		初期値は100である。

g:unite_source_file_mru_ignore_pattern		*g:unite_source_file_mru_ignore_pattern*
		|unite-source-file_mru|の候補に表示しないファイルの正規表現
		パターンを指定する。マッチングはファイルのフルパスに対して行
		われる。この変数が空文字列以外であれば、指定した正規表現で結
		果をフィルタリングする。正規表現の大文字・小文字は明確に区別さ
		れる。
		
		初期値は autoload/unite/sources/file_mru.vim を参照。

g:unite_source_directory_mru_time_format	*g:unite_source_directory_mru_time_format*
g:unite_source_directory_mru_directory		*g:unite_source_directory_mru_directory*
g:unite_source_directory_mru_limit		*g:unite_source_directory_mru_limit*
g:unite_source_directory_mru_ignore_pattern	*g:unite_source_directory_mru_ignore_pattern*
		これらは対象が|unite-source-directory_mru|であること以外は
		|unite-source-file_mru|の各変数と同じ仕様である。

g:unite_source_bookmark_file			*g:unite_source_bookmark_file*
		|unite-source-bookmark|がブックマークを書き出すファイルを指
		定する。
		
		初期値は |g:unite_data_directory| . '/.bookmark' である。

g:unite_source_file_rec_ignore_pattern		*g:unite_source_file_rec_ignore_pattern*
		|unite-source-file_rec|の候補に表示しないファイルの正規表現
		パターンを指定する。マッチングはファイルのフルパスに対して行
		われる。この変数が空文字列以外であれば、指定した正規表現で結
		果をフィルタリングする。正規表現の大文字・小文字は明確に区別さ
		れる。
		
		初期値は autoload/unite/sources/file_rec.vim を参照。

g:unite_source_file_rec_min_cache_files		*g:unite_source_file_rec_min_cache_files*
		|unite-source-file_rec|が内部でキャッシュする最小のファイル
		数を指定する。ディレクトリ中のファイル数が、ここで指定したファ
		イル数よりも小さい場合はキャッシュされない。

		初期値は50である。

g:unite_source_grep_command			*g:unite_source_grep_command*
		"grep"コマンドを指定する。

		初期値は"grep"である。

g:unite_source_grep_recursive_opt		*g:unite_source_grep_recursive_opt*
		"grep"の再帰検索オプションを指定する。

		初期値は"-R"である。

g:unite_source_grep_default_opts		*g:unite_source_grep_default_opts*
		"grep"に与えるデフォルトのオプションを指定する。
		Note: "grep"の出力は下記のパターンに一致しなければならない。
		filename:number:pattern

		初期値は"-Hn"である。

g:unite_source_grep_max_candidates		*g:unite_source_grep_max_candidates*
		|unite-source-grep|の候補の最大数を指定する。

		初期値は100である。

g:unite_source_line_enable_highlight		*g:unite_source_line_enable_highlight*
		検索キーワードをハイライトするか制御する。

		Default value is 1.

g:unite_source_line_search_word_highlight	*g:unite_source_line_search_word_highlight*
		|g:unite_source_line_enable_highlight|が真のとき、検索文字
		列のハイライトを設定する。

		初期値は"Search"である。

kindの変数					*unite-kinds-variables*

g:unite_kind_jump_list_after_jump_scroll	*g:unite_kind_jump_list_after_jump_scroll*
		|unite-kind-jump_list|において、ジャンプ後のカーソルの位置（ス
		クロール）を調整するための数値。最小値は 0 でウィンドウ最上を
		意味し、最大値は 100 でウィンドウ最下を意味する。

		数値	意味		同等のコマンド
		--------------------------------------
		0	ウィンドウ最上	normal! |z<CR>|
		50	ウィンドウ中央	normal! |z.|
		100	ウィンドウ最下	normal! |z-|

		初期値は25である。

g:unite_kind_openable_persist_open_blink_time	*g:unite_kind_openable_persist_open_blink_time*
		|unite-kind-openable|において、"persist_open"後にカーソル行
		を点滅させる時間。

		初期値は"250m"である。

------------------------------------------------------------------------------
キーマッピング 					*unite-key-mappings*

ノーマルモードマッピング

<Plug>(unite_exit)				*<Plug>(unite_exit)*
		uniteを終了する。

<Plug>(unite_restart)				*<Plug>(unite_restart)*
		unitを再起動する。

<Plug>(unite_do_default_action)			*<Plug>(unite_do_default_action)*
		選択している候補に対してデフォルトのアクションを実行する。アク
		ションは候補のkindごとに定義されている。kindについては
		|unite-kind|を参照。デフォルトのアクションについては
		|unite-default-action|を参照。

<Plug>(unite_choose_action)			*<Plug>(unite_choose_action)*
		uniteインタフェースにより、選択している候補に対して実行するアク
		ションを選択する。アクションは候補のkindごとに定義されている。
		複数の候補が選択されている場合は共通なアクションのみ選択ができ
		る。kindについては|unite-kind|を参照。

<Plug>(unite_insert_enter)			*<Plug>(unite_insert_enter)*
		カーソル位置から絞込みテキスト入力を開始する。ただし、カーソル
		がプロンプト行にいない場合、カーソルが自動的にプロンプト行へと
		移動する。

<Plug>(unite_insert_head)			*<Plug>(unite_insert_head)*
		カーソルを先頭に移動して絞込みテキスト入力を開始する。ただし、
		カーソルがプロンプト行にいない場合、カーソルが自動的にプロンプ
		ト行へと移動する。

<Plug>(unite_append_enter)			*<Plug>(unite_append_enter)*
		カーソルの右側から絞込みテキスト入力を開始する。ただし、カー
		ソルがプロンプト行にいない場合、カーソルが自動的にプロンプト行
		へと移動する。

<Plug>(unite_append_end)			*<Plug>(unite_append_end)*
		カーソルを行末に移動して絞込みテキストの入力を開始する。ただ
		し、カーソルがプロンプト行にいない場合、カーソルが自動的にプロ
		ンプト行へと移動する。

<Plug>(unite_toggle_mark_current_candidate)	*<Plug>(unite_toggle_mark_current_candidate)*
		カレント行の候補のマークを反転させる。複数の候補をマークすれば、
		複数の候補に対して一気にアクションを実行することができる。

<Plug>(unite_toggle_mark_all_candidates)	*<Plug>(unite_toggle_mark_all_candidates)*
		全ての候補のマークを反転させる。

<Plug>(unite_redraw)				*<Plug>(unite_redraw)*
		|g:unite_update_time|で制御される更新間隔を待たずに、すぐに表
		示を更新する。uniteのキャッシュ更新に使われる。

<Plug>(unite_rotate_next_source)		*<Plug>(unite_rotate_next_source)*
		sourceの順番を順方向に並び変える。

<Plug>(unite_rotate_previous_source)		*<Plug>(unite_rotate_previous_source)*
		sourceの順番を逆方向に並び変える。

<Plug>(unite_print_candidate)			*<Plug>(unite_print_candidate)*
		選択している候補に対するアクションの対象を表示する。例えば、選
		択している候補のkindが|word|の場合は、対象の内容が表示される。

<Plug>(unite_cursor_top)			*<Plug>(unite_cursor_top)*
		uniteバッファの一番上へ移動する。

<Plug>(unite_loop_cursor_down)			*<Plug>(unite_loop_cursor_down)*
		次の行に移動。ただしバッファの最後にいる場合はループする。

<Plug>(unite_loop_cursor_up)			*<Plug>(unite_loop_cursor_up)*
		前の行に移動。ただしバッファの一番上にいる場合はループする。

<Plug>(unite_quick_match_default_action)	*<Plug>(unite_quick_match_default_action)*
		クイックマッチを行い、選択した候補のデフォルトアクションを実行
		する。マークしている候補がある場合は無効となる。

<Plug>(unite_input_directory)			*<Plug>(unite_input_directory)*
		ディレクトリ名を入力して絞り込みを行う。

<Plug>(unite_delete_backward_path)		*<Plug>(unite_delete_backward_path)*
		一つ上のパスまで削除する。
		|i_<Plug>(unite_delete_backward_path)|を参照せよ。

<Plug>(unite_toggle_transpose_window)		*<Plug>(unite_toggle_transpose_window)*
		uniteバッファの分割方向を変更する。

<Plug>(unite_narrowing_path)			*<Plug>(unite_narrowing_path)*
		候補のパスもしくはwordにより絞り込みを行う。

<Plug>(unite_narrowing_input_history)		*<Plug>(unite_narrowing_input_history)*
		バッファの入力履歴により絞り込みを行う。

<Plug>(unite_toggle_auto_preview)		*<Plug>(unite_toggle_auto_preview)*
		uniteバッファのauto previewモードをトグルする。

インサートモードマッピング

<Plug>(unite_exit)				*i_<Plug>(unite_exit)*
		uniteを終了する。

<Plug>(unite_insert_leave)			*i_<Plug>(unite_insert_leave)*
		ノーマルモードに移行する。最初の候補にカーソルを動かす。

<Plug>(unite_delete_backward_char)		*i_<Plug>(unite_delete_backward_char)*
		カーソル直前の1文字を消す。文字が入力されていない場合はuniteを
		終了する。

<Plug>(unite_delete_backward_line)		*i_<Plug>(unite_delete_backward_line)*
		カーソルから行の先頭までの文字をすべて削除する。

<Plug>(unite_delete_backward_word)		*i_<Plug>(unite_delete_backward_word)*
		カーソル直前の単語を削除する。

<Plug>(unite_delete_backward_path)		*i_<Plug>(unite_delete_backward_path)*
		一つ上のパスまで削除する。たとえば >
		/home/Shougo/Desktop
<		や >
		/home/Shougo/Desktop/
<		の時に<Plug>(unite_delete_backward_path)すると >
		/home/Shougo
<		になる。ファイルパスを操作する際に有効。

<Plug>(unite_select_next_line)			*i_<Plug>(unite_select_next_line)*
		次の候補に移動。ただし候補の最後にいる場合はループする。

<Plug>(unite_select_previous_line)		*i_<Plug>(unite_select_previous_line)*
		前の候補に移動。ただし候補の先頭にいる場合はループする。

<Plug>(unite_select_next_page)			*i_<Plug>(unite_select_next_page)*
		次の候補ページを表示。

<Plug>(unite_select_previous_page)		*i_<Plug>(unite_select_previous_page)*
		前の候補ページを表示。

<Plug>(unite_do_default_action)			*i_<Plug>(unite_do_default_action)*
		|<Plug>(unite_do_default_action)|と同じ。

<Plug>(unite_toggle_mark_current_candidate)	*i_<Plug>(unite_toggle_mark_current_candidate)*
		|<Plug>(unite_toggle_mark_current_candidate)|と同じ。

<Plug>(unite_choose_action)			*i_<Plug>(unite_choose_action)*
		|<Plug>(unite_choose_action)|と同じ。

<Plug>(unite_move_head)				*i_<Plug>(unite_move_head)*
		カーソルを行の先頭に移動する。

<Plug>(unite_quick_match_default_action)	*i_<Plug>(unite_quick_match_default_action)*
		|<Plug>(unite_quick_match_default_action)|と同じ。

<Plug>(unite_input_directory)			*i_<Plug>(unite_input_directory)*
		|<Plug>(unite_input_directory)|と同じ。

<Plug>(unite_toggle_transpose_window)		*i_<Plug>(unite_toggle_transpose_window)*
		|<Plug>(unite_toggle_transpose_window)|と同じ。

<Plug>(unite_narrowing_path)			*i_<Plug>(unite_narrowing_path)*
		|<Plug>(unite_narrowing_path)|と同じ。

<Plug>(unite_narrowing_input_history)		*i_<Plug>(unite_narrowing_input_history)*
		|<Plug>(unite_narrowing_input_history)|と同じ。

<Plug>(unite_toggle_auto_preview)		*i_<Plug>(unite_toggle_auto_preview)*
		|<Plug>(unite_toggle_auto_preview)|と同じ。

ビジュアルモードマッピング

<Plug>(unite_toggle_mark_selected_candidates)	*v_<Plug>(unite_toggle_selected_candidates)*
		ビジュアル選択している候補に対して、マークのオンオフを切り替え
		る。

						*unite_default_key_mappings*
デフォルトキーマッピング

ノーマルモードマッピング
{lhs}		{rhs}
--------	-----------------------------
i		|<Plug>(unite_insert_enter)|
I		|<Plug>(unite_insert_head)|
a		|<Plug>(unite_append_enter)|
A		|<Plug>(unite_append_end)|
q		|<Plug>(unite_exit)|
<C-r>		|<Plug>(unite_restart)|
<Space>		|<Plug>(unite_toggle_mark_current_candidate)|
*		|<Plug>(unite_toggle_mark_all_candidates)|
<Tab>		|<Plug>(unite_choose_action)|
<C-n>		|<Plug>(unite_rotate_next_source)|
<C-p>		|<Plug>(unite_rotate_previous_source)|
<C-g>		|<Plug>(unite_print_candidate)|
<C-l>		|<Plug>(unite_redraw)|
<C-h>		|<Plug>(unite_delete_backward_path)|
gg		|<Plug>(unite_cursor_top)|
j		|<Plug>(unite_loop_cursor_down)|
<Down>		|<Plug>(unite_loop_cursor_down)|
k		|<Plug>(unite_loop_cursor_up)|
<Up>		|<Plug>(unite_loop_cursor_up)|
<CR>		候補を選択している場合は default アクションの実行
d		候補を選択している場合は delete アクションの実行
b		候補を選択している場合は bookmark アクションの実行
e		候補を選択している場合は edit アクションの実行
p		preview アクションの実行
x		候補を選択している場合は|<Plug>(unite_quick_match_default_action)|

インサートモードマッピング
{lhs}		{rhs}
--------	-----------------------------
<Tab>		|i_<Plug>(unite_choose_action)|
<C-n>		|i_<Plug>(unite_select_next_line)|
<Down>		|i_<Plug>(unite_select_next_line)|
<C-p>		|i_<Plug>(unite_select_previous_line)|
<Up>		|i_<Plug>(unite_select_previous_line)|
<C-f>		|i_<Plug>(unite_select_next_page)|
<C-b>		|i_<Plug>(unite_select_previous_page)|
<CR>		|i_<Plug>(unite_do_default_action)|
<C-h>		|i_<Plug>(unite_delete_backward_char)|
<BS>		|i_<Plug>(unite_delete_backward_char)|
<C-u>		|i_<Plug>(unite_delete_backward_line)|
<C-w>		|i_<Plug>(unite_delete_backward_word)|
<C-a>		|i_<Plug>(unite_move_head)|
<Home>		|i_<Plug>(unite_move_head)|
e		候補を選択している場合は edit アクションの実行
d		候補を選択している場合は delete アクションの実行
<Space>		候補を選択している場合は|i_<Plug>(unite_toggle_mark_current_candidate)|
x		候補を選択している場合は|i_<Plug>(unite_quick_match_default_action)|

ビジュアルモードマッピング
{lhs}		{rhs}
--------	-----------------------------
<Space>		|v_<Plug>(unite_toggle_mark_selected_candidates)|

==============================================================================
関数						*unite-functions*

CORE						*unite-functions-core*

unite#get_kinds([{kind-name}])			*unite#get_kinds()*
			{kind-name}に指定されたkindを取得する。kindが存在しな
			い場合は空のディクショナリを返す。{kind-name}を省略し
			た場合、キーがkind名、値がそれぞれのkindとしたディク
			ショナリを返す。

			この戻り値を変更してはいけない。

unite#get_sources([{source-name}])		*unite#get_sources()*
			{source-name}に指定されたsourceを取得する。sourceが存
			在しない場合は空のディクショナリを返す。{source-name}
			を省略した場合、キーがsource名、値がそれぞれのsourceと
			したディクショナリを返す。

			この戻り値を変更してはいけない。

CUSTOMS						*unite-functions-customs*

unite#start({sources}, [, {context}])				*unite#start()*
		uniteバッファを新しく生成する。現在uniteバッファに居る場合、絞
		り込みテキストは保存される。

		{sources}はリストであり、それぞれ
		の要素は{source-name}または、[{source-name}, {args},...]とい
		う形式である。{args}には{source-name}に渡す引数を文字列形式で
		複数指定できる。

		{context}については、 |unite-notation-{context}|
		を参照せよ。値を省略すると、デフォルト値が使用される。

unite#get_context()						*unite#get_context()*
		現在のuniteバッファのコンテキスト情報を得る。
		|unite#custom_action()|などで、|unite#start()|を内部で呼び出す
		際に使用する。

unite#do_action({action-name})					*unite#do_action()*
		マークしている候補に対して{action-name}アクションを実行す
		るためのキーシーケンスを返す。この関数はuniteが起動している時に
		のみ動作する。{action-name}が存在しない、もしくは不正な実行だっ
		た場合は実行時エラーとなる。

		ユーザー側で特定のアクションを実行するためのマッピングを定義す
		る場合に便利である。

		{action-name}に"default"を指定すると、デフォルトのアクションが
		実行される。

		候補がマークされていない場合は、現在の行もしくは、一番上の候補
		に対してアクションを実行することになる。

		inoremap <buffer><expr>やnnoremap <buffer><expr>で通常用いる。
		例:
>
		nnoremap <silent><buffer><expr> <C-k> unite#do_action('preview')
>
unite#smart_map({narrow-map}, {select-map})			*unite#smart_map()*
		絞り込み時と選択時のマッピングを設定し、文脈によって切り換える
		キーシーケンスを返す。|unite#do_action()|と組み合わせて使用する
		と良い。

		inoremap <buffer><expr>やnnoremap <buffer><expr>で通常用いる。
		例:
>
		inoremap <buffer><expr> ' unite#smart_map("'", unite#do_action('preview'))
<
unite#set_substitute_pattern({buffer-name}, {pattern}, {subst} [, {priority}])
						*unite#set_substitute_pattern()*
		uniteのバッファ名{buffer-name}における絞込みテキストの置換パタ
		ーンを指定する。{buffer-name}を""とすると、"default"となり、
		|:Unite|で-buffer-name=を省略したときに使われるbuffer-nameと同
		じになる。{buffer-name}は","区切りで複数指定ができる。
		{pattern}は置換対象の正規表現、{subst}は置換する文字列である。
		同じ{pattern}を複数回指定すると、設定は上書きされる。
		{subst}を""とすると、{pattern}は無効化される。{priority}とは置
		換処理の優先度である。{pattern}は{priority}でソートされた上で、
		{priority}が大きい順に適用される。{priority}を省略すると0にな
		る。先に処理されるべき{pattern}は{priority}を大きくすると良い。
		ちなみに、uniteバッファの初期文字列は置換の対象とならない。
		
		この関数を使うと、曖昧マッチを模倣することができる。
>
		call unite#set_substitute_pattern('files', '[[:alnum:]]', '*\0', 100)
		call unite#set_substitute_pattern('files', '[[:alnum:]]', ' \0', 100)
<
		上の設定は/を越えずに曖昧検索をするが、下の場合は/を越えて曖昧
		検索することができる。
		
		初期値は次のように設定されていて、buffer_nameがfilesのバッファ
		において、~を$HOME、/は部分マッチできるように、ワイルドカードを
		付加するようになっている。
>
		call unite#set_substitute_pattern('files', '^\~',
		\ substitute(unite#util#substitute_path_separator($HOME), ' ', '\\\\ ', 'g'), -100)
		call unite#set_substitute_pattern('files', '[^~.*]\ze/', '\0*', 100)
		call unite#set_substitute_pattern('files', '/\ze[^~.*]', '/*', 100)
<
unite#get_substitute_pattern({buffer-name})
						*unite#get_substitute_pattern()*
		uniteのバッファ名{buffer-name}における絞込みテキストの置換パタ
		ーンを取得する。{buffer-name}に置換パターンが定義されていない
		と、エラーになる。デバッグ用である。

unite#set_buffer_name_option({buffer-name}, {option-name}, {value})
						*unite#set_buffer_name_option()*
		uniteのバッファ名{buffer-name}における{option-name}のオプショ
		ンを{value}に設定する。
		
		現在、次のオプション名が有効である。
		
		substitute_patterns		(辞書)
		置換文字列のパターンを指定する。辞書のkeyは"pattern",
		"subst", "priority"である。詳しい仕様は
		|unite#set_substitute_pattern()|を参照せよ。
		
		filters				(リスト)
		filter名のリストを指定する。ここで指定したfilterは、source毎
		に呼び出される普通のfilterとは異なり、全候補を結合した後に呼
		び出される。
		
		ignorecase			(数値)
		uniteバッファで一時的にセットされる'ignorecase'オプションの値を
		決定する。省略すると、'ignorecase'の値と同じになる。
		ただし、buffer_nameが"files"の場合は初期値が"1"になっている。
		
		smartcase			(数値)
		大文字が入力されているときに、'ignorecase'オプションの値を無視
		するかどうか決定する。省略すると、'smartcase'の値と同じになる。
		ただし、buffer_nameが"files"の場合は初期値が"0"になっている。

unite#custom_default_action({kind}, {default-action})
						*unite#custom_default_action()*
		{kind}に対するデフォルトアクションを{default-action}に置き換え
		る。{kind}は","区切りで複数指定ができる。
		例:
>
		call unite#custom_default_action('file', 'tabopen')
<
unite#custom_action({kind}, {name}, {action})
						*unite#custom_action()*
		{kind}に対し、{name}という名前の{action}を付け加える。
		{kind}は","区切りで複数指定ができる。
		例:
>
		let my_tabopen = {
		\ 'is_selectable' : 1,
		\ }
		function! my_tabopen.func(candidates)
		  call unite#take_action('tabopen', a:candidates)
		
		  let l:dir = isdirectory(a:candidate.word) ? a:candidate.word : fnamemodify(a:candidate.word, ':p:h')
		  execute g:unite_lcd_command '`=l:dir`'
		endfunction
		call unite#custom_action('file,buffer', 'tabopen', my_tabopen)
		unlet my_tabopen
<
unite#undef_custom_action({kind}, {name})			*unite#undef_custom_action()*
		|unite#custom_action()|で追加した、{kind}の{name}というactionを削
		除する。{kind}は","区切りで複数指定ができる。
		存在しないときは無視される。

unite#custom_alias({kind}, {name}, {action})
						*unite#custom_alias()*
		{kind}に対する{action}の別名である、{name}というアクションを定
		義する。アクションに短縮名を付けたい場合に便利。
		{kind}は","区切りで複数指定ができる。
		{action}を"nop"とすると、そのアクションは無効化される。
		例:
>
		call unite#custom_alias('file', 'h', 'left')
<
unite#custom_filters({source-name}, {filters})	*unite#custom_filters()*
		{source-name}のfilterを{filters}に変更する。
		{source-name}は","区切りで複数指定ができる。
		{filters}は使用するfilterの名前、もしくは名前のリストである。
		filterは指定したリストの順番で呼び出される。
		{filters}は"matcher", "sorter", "converter"の名前のリストで
		指定することに注意しなければならない。特にmatcherの指定を忘
		れると、絞り込みができなくなるであろう。

unite#custom_max_candidates({source-name}, {max})
						*unite#custom_max_candidates()*
		{source-name}のmax_candidatesを{max}に変更する。
		{source-name}は","区切りで複数指定ができる。
		{max}に0を指定した場合、そのsourceの候補は全て表示される。

unite#take_action({action-name}, {candidate})
						*unite#take_action()*
		{candidate}に対し、{action-name}という名前のアクションを実行する。
		主に、|unite#custom_action()|などで用いる。
		is_selectableなアクションの場合は{candidate}がリストに自動変換
		される。

unite#take_parents_action({action-name}, {candidate}, {extend-candidate})
						*unite#take_parents_action()*
		|unite#take_action()|と同じだが、{candidate}に
		{extend-candidate}が合成された上で、parentsのアクションテーブル
		のみが検索される。親のアクションを再利用したいときに用いる。

unite#define_source({source})			*unite#define_source()*
		{source}を動的に追加する。sourceの仕様については、
		|unite-create-source|を参照せよ。
		すでにその名前のsourceが存在する場合は上書きされる。

unite#define_kind({kind})			*unite#define_kind()*
		{kind}を動的に追加する。kindの仕様については、
		|unite-create-kind|を参照せよ。
		すでにその名前のkindが存在する場合は上書きされる。

unite#define_filter({filter})			*unite#define_filter()*
		{filter}を動的に追加する。filterの仕様については、
		|unite-create-filter|を参照せよ。
		すでにその名前のfilterが存在する場合は上書きされる。

unite#undef_source({name})			*unite#undef_source()*
		|unite#define_source()|で追加した{name}というsourceを削除
		する。
		存在しないときは無視される。

unite#undef_kind({name})			*unite#undef_kind()*
		|unite#define_kind()|で追加した{name}というkindを削除する。
		存在しないときは無視される。

unite#undef_filter({name})			*unite#undef_filter()*
		|unite#define_filter()|で追加した{name}というfilterを
		削除する。
		存在しないときは無視される。

unite#filters#default#use({filters})		*unite#filters#default#use(()*
		|unite-filter-default|が使用するデフォルトのfilterを
		{filters}に変更する。{filters}には、filter名のリストを指定す
		る。

unite#filters#matcher_default#use({matchers})	*unite#filters#matcher_default#use(()*
		|unite-filter-matcher_default|が使用するデフォルトのmatcher
		を{matchers}に変更する。{matchers}には、matcher名のリストを
		指定する。

unite#filters#sorter_default#use({sorters})	*unite#filters#sorter_default#use(()*
		|unite-filter-sorter_default|が使用するデフォルトのsorterを
		{sorters}に変更する。{sorters}には、sorter名のリストを指定す
		る。

						*unite#filters#converter_default#use(()*
unite#filters#converter_default#use({converters})
		|unite-filter-converter_default|が使用するデフォルトの
		converterを{converters}に変更する。{converters}には、
		converter名のリストを指定する。

==============================================================================
source						*unite-sources*

ここでは、標準で実装されているsourceの仕様を解説する。

						*unite-source-file*
file		入力されたファイルを候補とする。
		Note: デフォルトでは、"../"といった親ディレクトリや隠しファイル
		は表示されない。絞り込み文字列に.を入力すると表示される。

						*unite-source-file_mru*
file_mru	最近使用したファイルを候補とする。使用した順番に整列される。

						*unite-source-directory_mru*
directory_mru	最近使用したカレントディレクトリを候補とする。使用した順番に整
		列される。

						*unite-source-file_rec*
file_rec	入力された絞り込みテキストのディレクトリまたは、現在ディレクト
		リ直下のファイルすべてを候補とする。ディレクトリや隠しファイル
		は候補から除外される。候補が多すぎる場合はフリーズするかもしれ
		ない。候補はsource内でキャッシュされるため、キャッシュを更新
		するためには、|<Plug>(unite_redraw)|を用いる。

		source引数：
		1. 絞り込み文字列

						*unite-source-file_rec/async*
file_rec/async	|unite-source-file_rec|と同じだが、候補の取得を完全に非同期
		で行う。
		
		Note: このsourceはvimprocと外部lsコマンドを必要としている。
		http://github.com/Shougo/vimproc/tree/master

		source引数：
		1. 絞り込み文字列

						*unite-source-buffer*
buffer		開いているバッファを候補とする。バッファは使用した順番に整列さ
		れる。

						*unite-source-buffer_tab*
buffer_tab	現在のタブで開いたバッファを候補とする。バッファは使用した順番
		に整列される。

						*unite-source-tab*
tab		開いているタブを候補とする。t:cwdをカレントディレクトリ、
		t:titleをタブのタイトルとして認識する。ただし、|gettabvar()|が
		使用可能である必要がある。

		もしt:titleが存在していた場合、絞り込みに使われる("word"として
		使用される。)存在していなかったら、代わりにタブの中での現在の
		バッファ名が使われる。

		source引数：
		1. no-currentを指定すると、カレントタブを除外する。

						*unite-source-register*
register	レジスタに保存されている文字列を候補とする。
		|yanktmp|がインストールされている場合、それも仮想レジスタとして
		参照できる。

						*unite-source-bookmark*
bookmark	ブックマークされたファイルやディレクトリを候補とする。

						*unite-source-source*
source		uniteのsource名そのものを候補とする。

		選択されたsource名を|unite#start()|で起動する。contextは現在
		のuniteバッファのものを受け継ぐ。

						*unite-source-window*
window		開いているウィンドウを候補とする。ウィンドウは移動した順番に
		整列される。

		source引数：
		1. no-currentを指定すると、カレントウインドウを除外する。

						*unite-source-output*
output		実行したVimコマンドの出力を候補とする。

		source引数：
		1. 実行するVimコマンド

						*unite-source-command*
command		VimのExコマンドを候補とする。

						*unite-source-mapping*
mapping		ユーザーが定義したマッピングを候補とする。

						*unite-source-grep*
grep		"grep"コマンドを実行した結果を候補とする。
		Note: このsourceはSixeight氏によって作成された。

		Note: このsourceはvimprocを必要としている。
		http://github.com/Shougo/vimproc/tree/master

		source引数：
		1. ターゲットとなるディレクトリ名
		2. "grep"に与えるオプション
		3. 絞り込みパターン

		候補の最大数：|g:unite_source_grep_max_candidates|

		Example:
>
	:Unite grep:~/.vim/autoload/unite/sources:-iR:file
<
		次のような特別なターゲットも認識する。
		%         : カレントバッファ名
		#         : 裏バッファ名
		$buffers  : 全てのバッファ名

						*unite-source-line*
line		カレントバッファの行を候補とする。
		Note: このsourceはt9md氏によって作成された。

		source引数：
		1. 検索方向。 "all", "forward", "backward"のうちどれかを指定
		する。省略すると"all"となる。

		候補の最大数：100

		Example:
>
	nnoremap <silent> /  :<C-u>Unite -buffer-name=search line -start-insert -no-quit<CR>
<
						*unite-source-resume*
resume		|:UniteResume|の引数となるuniteバッファを候補とする。uniteバッ
		ファは使用した順番に並び換えられる。

						*unite-source-jump*
jump		|:jumps|の結果を候補とする。

						*unite-source-change*
change		|:changes|の結果を候補とする。

						*unite-source-jump_point*
jump_point	現在行の"file:line"形式をジャンプ先として、候補とする。
		|vimshell|の出力に対して利用すると便利である。

						*unite-source-file_point*
file_point	現在行のカーソル位置にあるファイル名やURIを候補とする。
		|vimshell|の出力に対して利用すると便利である。

==============================================================================
kind						*unite-kinds*

ここでは、標準で実装されているkindの仕様と、内部で使用する属性について解説す
る。

						*unite-kind-common*
common		共通アクション
		ほとんどのkindはcommonを暗黙的に継承する。
		必須となるキーはwordのみ。

						*unite-kind-openable*
openable	オープン可能インタフェース
		必須となるキーは存在しないが、これを継承したkindにはopenアク
		ションが必須である。

						*unite-kind-cdable*
cdable		cd可能インタフェース

			action__directory	(文字列)		(必須)
				対象となるディレクトリ

						*unite-kind-file*
file		ファイル
		このkindはcdable, openable, uriを継承しているため、それらのkind
		が使用する属性も必要である。

			action__path		(文字列)		(必須)
				対象となるファイルのパス

						*unite-kind-buffer*
buffer		バッファ
		このkindはfileを継承しているため、それらのkindが
		使用する属性も必要である。

			action__buffer_nr	(文字列)		(必須)
				対象となるバッファの番号

						*unite-kind-tab*
tab		タブ
		|gettabvar()|が使用可能な場合、このkindはcdableを継承するた
		め、それらのkindが使用する属性も必要である。

			action__tab_nr		(文字列)		(必須)
				対象となるタブの番号

						*unite-kind-directory*
directory	ディレクトリ
		このkindはfileを継承しているため、それらのkindが
		使用する属性も必要である。

						*unite-kind-word*
word		挿入可能文字列

			word			(文字列)		(必須)
				挿入する文字列

						*unite-kind-jump_list*
jump_list	ジャンプリスト
		このkindはopenableを継承しているため、それらのkindが
		使用する属性も必要である。

			action__path		(文字列)		(必須)
				ジャンプするファイルのパス

			action__buffer_nr	(文字列)		(任意)
				ジャンプするバッファのバッファ番号

			action__line		(数値)			(任意)
				ジャンプするファイルの行番号

			action__col		(数値)			(任意)
				ジャンプするファイルの桁番号

			action__pattern		(文字列)		(任意)
				ファイルを開いた後に検索するパターン

			action__text		(文字列)		(任意)
				ジャンプする行の内容

			action__signature	(文字列)		(任意)
				action__patternに設定するパターンや
				action__lineだけではジャンプ位置の一意性を保証
				できない場合に、同じパターンにマッチする行どう
				しを区別するための一意な文字列

		action__signatureとcalc_signature()関数

		action__signatureを設定するsourceは、バッファの行番号から
		signatureを計算するためのcalc_signature()関数を定義しなければ
		ならない。
		calc_signature()関数は{lnum}を引数に取り、signatureとなる文字
		列を返す。
		{lnum}は行番号である。
		jump_listはこの関数を呼び出してsignatureの比較を行う。

		以下にその例を示す。
>
		function! s:source.calc_signature(lnum)
		  let range = 2
		  let from = max([1, a:lnum - range])
		  let to   = min([a:lnum + range, line('$')])
		  return join(getline(from, to))
		endfunction
<
						*unite-kind-command*
command		VimのExコマンド

			action__command		(文字列)		(必須)
				実行するコマンド

			action__type		(文字列)		(任意)
				実行するコマンドの型。省略すると":"となる。
				":"または"/"のみ指定できる。指定できる型につ
				いての説明は、|histadd()|を参照せよ。

						*unite-kind-window*
window		ウィンドウ
		このkindはcdableを継承するため、それらのkindが使用する属性も
		必要である。

			action__window_nr	(文字列)		(必須)
				対象となるウィンドウの番号

						*unite-kind-completion*
completion	補完

			action__complete_word	(文字列)	(必須)
				補完する文字列

			action__complete_pos	(数値)	(必須)
				補完する位置

			action__complete_info	(数値)	(任意)
				補完候補の追加情報

						*unite-kind-source*
source		unite.vimのsource

			action__source_name	(文字列)	(必須)
				実行するsourceの名前

			action__source_name	(リスト)	(任意)
				実行するsourceの引数

						*unite-kind-uri*
uri		ファイルやhttpといったプロトコル

			action__path		(文字列)		(必須)
				ファイルのパス

==============================================================================
filter						*unite-filter*

ここでは、標準で実装されているfilterの仕様と、内部で使用する属性について解説
する。

filterには大まかに分けて、"matcher", "sorter", "converter"の三種類が存在する。
matcherは入力に合う候補を取り出し、sorterは候補の並べ換えを行い、converterは
候補の中身を変更する。converterが候補のabbrを変更するものなら、候補の表示が変わ
ることになる。
filterはデフォルトで、|unite-filter-default|にあるものが使用される。
だが、|unite#custom_filters()|でsource毎に変更することも可能である。
filterは指定したリストの順番で呼び出されるので注意せよ。

						*unite-filter-default*
default		特にfilterが指定されていないときに呼び出される、デフォルトの
		filter。このfilterは登録用のダミーである。
		初期状態では、["matcher_glob", "sorter_nothing",
		"converter_nothing"]のfilterを呼び出す。
		|unite#filters#default#use()|を呼び出すことで、デフォルトの
		filterを変更することができる。

						*unite-filter-matcher_default*
matcher_default	|unite-filter-default|により呼び出される、デフォルトのmatcher。
		初期状態では、["matcher_glob"]のmatcherを呼び出す。
		|unite#filters#matcher_default#use()|を呼び出すことで、デフォルトの
		matcherを変更することができる。

						*unite-filter-matcher_glob*
matcher_glob	ユーザーが入力したglobパターンにより候補をフィルタリングする
		matcher。"*"をワイルドカードとして認識する。"!"は否定となる。
		絞り込みにはwordを使用する。

						*unite-filter-matcher_regexp*
matcher_regexp	ユーザーが入力したパターンを正規表現として、候補をフィルタリ
		ングするmatcher。"!"は否定となる。絞り込みにはwordを使用する。

						*unite-filter-sorter_default*
sorter_default	|unite-filter-default|により呼び出される、デフォルトのsorter。
		初期状態では、["sorter_nothing"]のsorterを呼び出す。
		|unite#filters#sorter_default#use()|を呼び出すことで、デフォルトの
		sorterを変更することができる。

						*unite-filter-sorter_nothing*
sorter_nothing	何もsortをしないsorter。source側のsortがそのまま反映される。

						*unite-filter-sorter_word*
sorter_word	wordを比較するsorter。

						*unite-filter-converter_default*
converter_default
		|unite-filter-default|により呼び出される、デフォルトのconverter。
		初期状態では、["converter_nothing"]のconverterを呼び出す。
		|unite#filters#converter_default#use()|を呼び出すことで、デフォルトの
		converterを変更することができる。

						*unite-filter-converter_nothing*
converter_nothing
		候補を何も変更しないconverter。

						*unite-filter-converter_relative_word*
converter_relative_word
		候補のwordを相対パスに変換するconverter。wordはmatcherにおいて、
		絞り込みに使用される。そのため、このconverterを使用する場合は、
		matcherの前に指定しなければならない。
		context情報に、source__directoryが含まれている場合は、そのパ
		スを基準として相対パスに変換する。
>
		call unite#custom_filters('file_rec',
		\ ['converter_relative_word', 'matcher_default',
		\  'sorter_default', 'converter_relative_abbr'])
<

						*unite-filter-converter_relative_abbr*
converter_relative_abbr
		候補のabbrをwordの相対パスに変換するconverter。仕様は
		|unite-filter-converter_relative_word|とほぼ同じである。

==============================================================================
アクション					*unite-actions*

kind別アクション

common					*unite-action-common*
全てのkindに共通なインタフェースを定義している。
内部では、candidate.wordを使う。
	nop		何もしない
	yank		候補文字列をヤンクする
	yank_escape	エスケープした候補文字列をヤンクする
	ex		候補をエスケープして、コマンドラインに入力した状態にする
	insert		現在のバッファに文字列を挿入する

openable					*unite-action-openable*
オープン可能ファイルのインタフェースを定義している。内部でopenアクションを呼
び出すので、openアクションは継承したkind側が個別に定義する必要がある。
	tabopen		ファイルをタブで開く
	split		ウィンドウを水平分割してファイルを開く
	vsplit		ウィンドウを垂直分割してファイルを開く
	left		ウィンドウを垂直分割して左側にファイルを開く
	right		ウィンドウを垂直分割して右側にファイルを開く
	above		ウィンドウを水平分割して上側にファイルを開く
	below		ウィンドウを水平分割して下側にファイルを開く
	persist_open	ファイルを裏ウィンドウに開く。uniteバッファは閉じな
			い。

cdable					*unite-action-cdable*
cd可能ファイルのインタフェースを定義している。
	cd		カレントディレクトリを変更
	lcd		現在のウィンドウのカレントディレクトリを変更
	project_cd	プロジェクトのディレクトリを探し出し、そこにカレントデ
			ィレクトリを変更
	narrow		ディレクトリ名で候補を絞り込む
	vimshell	そのディレクトリで|vimshell|を起動する。これ
			は|vimshell|がインストールされている場合にのみ有効なア
			クションである。
	vimfiler	そのディレクトリで|vimfiler|を起動する。これは
			|vimfiler|がインストールされている場合にのみ有効なア
			クションである。
	tabvimfiler	そのディレクトリで|:VimFilerTab|を実行する。これは
			|vimfiler|がインストールされている場合にのみ有効な
			アクションである。
	rec		そのディレクトリで|unite-source-file_rec|を起動する。
	file		そのディレクトリで|unite-source-file|を起動する。

file						*unite-action-file*
ファイルはすべて別のバッファに開かれる。
このkindは|unite-action-openable|と|unite-action-cdable|のアクションを継承している。
	open		ファイルを開く
	preview		プレビューウィンドウにファイルを開く
	bookmark	候補をブックマークに追加する
	mkdir		ファイル名をディレクトリ名としてディレクトリを作成する。
			ファイルが存在する場合は無効。
	rename		ファイル名を変更する
	grep		ファイルをgrepする
	grep_directory	ディレクトリをgrepする

buffer						*unite-action-buffer*
このkindは|unite-action-file|のアクションを継承している。
	delete		バッファを|:bdelete|
	fdelete		バッファを|:bdelete!|
	wipeout		バッファを|:bwipeout|
	unload		バッファを|:bunload|
	bookmark	候補をブックマークに追加する
	rename		バッファのファイル名を変更する

tab						*unite-action-tab*
このkindは|gettabvar()|が存在するときのみ、|unite-action-cdable|のアクションを
継承する。
	open		タブを表示
	delete		タブを閉じる
	
	次のアクションは|gettabvar()|およびt:cwdが存在する必要がある。
	rename		タブのタイトルを変更

directory					*unite-action-directory*
このkindは|unite-action-file|のアクションを継承している。独自のアクションは存在
しない。対象がdirectoryのとき、default_actionを変更したい場合に使われる。

word						*unite-action-word*
このkindに独自のアクションは存在しない。対象がwordのとき、default_actionを変更
したい場合に使われる。

jump_list					*unite-action-jump_list*
このkindは|unite-action-openable|のアクションを継承している。ここではjump_list独自
のアクションについて解説する。
	open		その候補の場所にジャンプする
	preview		その候補の場所をプレビューする
	replace		選択した候補を|qfreplace|プラグインで置換する。
			候補にaction__textが必要である。

command						*unite-action-command*
	execute		コマンドを実行する
	edit		コマンドをコマンドラインに入力する

window						*unite-action-window*
このkindは|unite-action-cdable|のアクションを継承する。
	open		ウィンドウへ移動
	delete		ウィンドウを閉じる
	only		そのウィンドウ以外すべて閉じる

completion					*unite-action-completion*
	insert		候補の挿入
	preview		候補の情報を表示

uri						*unite-action-uri*
	start		ファイルを関連付けで開く

source別アクション

file_mru					*unite-action-file_mru*
	delete		最近使用したファイルの候補から削除

directory_mru					*unite-action-directory_mru*
	delete		最近使用したディレクトリの候補から削除

bookmark					*unite-action-bookmark*
	delete		ブックマークファイルの候補から削除

						*unite-default-action*
デフォルトアクション

kind		アクション
{kind}		{action}
----------	----------
file		open
buffer		open
tab		open
directory	narrow
word		insert
jump_list	open

==============================================================================
sourceの作成					*unite-create-source*

autoload/unite/sources/に*.vimのファイルを置いておくと、自動的に読み込まれる。
その際、unite#sources#{source_name}#define()が呼ばれ、戻り値がsourceと解釈される。
戻り値はリストでもよく、その場合はsourceのリストと解釈される。
sourceを追加したくない場合は、空リストを返せば良い。
独自のsourceを動的に追加するためには、|unite#define_source()|を使う。

------------------------------------------------------------------------------
sourceの属性					*unite-source-attributes*

						*unite-source-attribute-name*
name			文字列		(必須)
			sourceの名前。以下の文字で構成しなければならない。
			文字:
			- a-z
			- 0-9
			- _
			- /

			例:
			- "buffer", "virw/git" と "file_mru" は正当。
			- "BadOne", "!@#$%^&*()_[]{}-|" と "" は
			  sourceの名前としては使えない。

						*unite-source-attribute-gather_candidates*
gather_candidates	関数		(ほぼ必須)
			uniteが候補を収集する際に一度だけ呼ばれる。ただし、
			|<Plug>(unite_redraw)|による再描画の際にも呼ばれる。
			sourceが|unite-source-attribute-is_volatile|の場合は、
			|unite-source-attribute-change_candidates|が定義され
			ていない限り、入力文字列が変化すると呼ばれる。
			この関数は{args}と{context}を引数に取り、{candidate}の
			リストを返す。
			{args}は|:Unite|コマンドを実行された際にsourceに渡され
			た引数リスト、{context}はsourceが呼ばれたときのコンテキ
			スト情報である。引数が省略されると、その引数には""が
			渡される。よって、引数が空文字列であっても、うまく対
			応しなければならない。引数は|get()|を用いてチェック
			するようにするべきである。
			{context}については|unite-notation-{context}|、
			{candidate}については|unite-notation-{candidate}|を参照
			せよ。
			この属性は基本的に必須だが、他の属性を用いて候補を取得
			できる場合には省略することができる。
			この属性で収集された候補はuniteバッファによりキャッ
			シュされる。キャッシュはuniteバッファが閉じられるま
			で有効だが、uniteバッファが閉じられると
			|:UniteResume|を使わない限り破棄される。uniteバッファ
			の寿命によらず永続的にキャッシュしたい場合は、source
			側でキャッシュを保存しておく必要がある。

						*unite-source-attribute-change_candidates*
change_candidates	関数		(任意)
			uniteが候補を収集する際に、入力文字列が変化すると呼ばれ
			る。入力文字列から候補を生成するときに便利である。
			この属性で追加する候補は、
			|unite-source-attribute-gather_candidates|によって
			キャッシュした候補に追加される。
			この関数は{args}と{context}を引数に取り、{candidate}の
			リストを返す。
			引数や戻り値の仕様は
			|unite-source-attribute-gather_candidates|と同一であ
			る。

						*unite-source-attribute-async_gather_candidates*
async_gather_candidates	関数		(任意)
			uniteが候補を収集する際に、非同期に呼ばれる。時間のかか
			る処理を細切れで行う際に便利である。
			デフォルトのタイミングは、|g:unite_update_time|である。
			この関数は{args}と{context}を引数に取り、{candidate}の
			リストを返す。
			引数や戻り値の仕様は
			|unite-source-attribute-gather_candidates|と同一であ
			る。

						*unite-source-attribute-hooks*
hooks			辞書		(任意)
			uniteへのフック関数を指定する。キーはフックする位置、値
			は呼び出す関数へのリファレンスである。
			標準では、次のようなフックが定義されている。

			on_init			*unite-source-attribute-hooks-on_init*
			|:Unite|系のコマンドを実行した際や|unite#start()|を呼ん
			だ際に、uniteバッファに移る前に呼ばれる。|:UniteResume|
			では呼ばれない。
			この関数は{args}と{context}を引数に取る。
			この関数が呼ばれた際、まだuniteバッファは初期化されてい
			ないので呼び出す関数には注意しなければならない。

			on_syntax		*unite-source-attribute-hooks-on_syntax*
			uniteバッファが初期化され、source毎のシンタックスが
			設定された後に呼ばれる。
			|unite-source-attribute-syntax|が設定されていない場
			合は呼び出されない。
			この関数は{args}と{context}を引数に取る。
			source毎のハイライト設定はここで行う。

			ハイライトの設定例：
>
			syntax match uniteSource_FileMru_Time /(.*)/ contained containedin=uniteSource_FileMru
			highlight default link uniteSource_FileMru_Time Statement
<
			|unite-source-attribute-syntax|で設定したsyntax名を
			containedinで指定することに注意しなければならない。

			on_close		*unite-source-attribute-hooks-on_close*
			<Plug>(unite_exit)を実行したときや、コマンドの実行後に
			uniteバッファが閉じられた後に呼ばれる。
			この関数は{args}と{context}を引数に取る。

			on_post_filter		*unite-source-attribute-hooks-on_post_filter*
			filterが呼ばれ、候補が絞り込まれた後に呼ばれる。
			パフォーマンスの影響を避けるため、絞り込まれた後に属性
			をセットする際に使用する。
			この関数は{args}と{context}を引数に取る。

						*unite-source-attribute-action_table*
action_table		辞書		(任意)
			source独自のアクションテーブルを追加するときに使用する。
			キーはアクションテーブルを追加するkindで、値は追加す
			るアクションテーブルである。それぞれのアクションテーブ
			ルの仕様は、|unite-kind-attribute-action_table|と同じで
			ある。つまり、source側では候補のkind毎に
			|unite-kind-attribute-action_table|を定義できる。
			例えば、
>
			'action_table' : { 'buffer' : foo_action_table }
<
			の場合、このsourceのkind "buffer"では、foo_action_table
			が使用される。
			キーのkindに"*"を設定すると、どんなkindにもマッチするよ
			うになる。候補のkindが一種類しかない場合に便利である。
			もしこれが与えられない場合、空となる。

						*unite-source-attribute-default_action*
default_action		辞書		(任意)
			source独自の標準的なアクションを追加するときに使用する。
			キーはアクションテーブルを追加するkindで、値は標準ア
			クションである。これを省略した場合はkindの
			default_actionを使用する。
			default_actionは文字列形式にすることもできる。
			その場合、'default_action' : { '*' : action_name }と指
			定したことと同じとなる。

						*unite-source-attribute-alias_table*
alias_table		辞書		(任意)
			source独自のエイリアステーブルを追加するときに使用す
			る。キーはエイリアステーブルを追加するkindで、値は追
			加するエイリアステーブルである。キーに"*"を設定する
			と、どんなkindにもマッチするようになる。
			エイリアステーブルの詳細については、
			|unite-kind-attribute-action_table|を参照せよ。もし
			これが与えられない場合、空となる。

						*unite-source-attribute-max_candidates*
max_candidates		数値		(任意)
			候補の最大数。
			この属性は任意である。もしこれが与えられない場合、0がデ
			フォルト値として使われる。この場合、最大数の制限はない。

						*unite-source-attribute-required_pattern_length*
required_pattern_length	数値		(任意)
			候補を収集するのに必要な絞り込み文字列の長さ。
			この属性は任意である。もしこれが与えられない場合、0がデ
			フォルト値として使われる。この場合、常に候補が収集され
			る。

						*unite-source-attribute-is_volatile*
is_volatile		数値		(任意)
			入力が変化するごとに、sourceが毎回候補を再計算するかど
			うか。
			この属性は任意である。もしこれが与えられない場合、0がデ
			フォルト値として使われる。この場合、候補はuniteバッファ
			によりキャッシュされ、毎回
			|unite-source-attribute-gather_candidates|が呼ばれるこ
			とはない。ただし、このキャッシュはそのuniteバッファが
			閉じられるまで有効である。もっと長い時間キャッシュした
			い場合はsourceが自前でキャッシュするほかない。

						*unite-source-attribute-description*
description		文字列		(任意)
			sourceを説明する文字列。省略すると""になる。
			|unite-source-source|において表示に使われる。

						*unite-source-attribute-syntax*
syntax			文字列		(任意)
			source内で使用するsyntax名。ここで設定したsyntax名は
			uniteによって自動的に定義される。省略すると""になる。
			source独自のハイライトを設定する際に使用される。
			他のsyntaxと被らないようにするため、
			"uniteSource__(source名)"とする慣習となっている。
			ハイライトの設定自体は、
			|unite-source-attribute-hooks-on_syntax|を使用しなけ
			ればならない。

						*unite-source-attribute-filters*
filters			リスト		(任意)
			そのsourceが使用するfilterのリスト。filterの仕様は、
			|unite-filter-attributes|を参照せよ。これが省略されると
			|unite-filter-default|が使用される。

						*unite-source-attribute-source__*
source__		不定		(任意)
			source毎の独自属性。unite側の属性と被らないように、
			source__プレフィクスを追加しなければならない。

NOTATION					*unite-notation*

{context}					*unite-notation-{context}*
			コンテキスト情報を与える辞書変数。主に、次の情報を持つ。
			グローバルなコンテキスト情報は|unite#get_context()|で取
			得できる。コンテキスト情報は、source毎に独立して持って
			いるため、ここにsource__で始まるキーを格納することで、
			source独自の情報を格納することができる。

			input			(文字列)
				ユーザーの入力文字列。

			buffer_name		(文字列)
				uniteバッファの名前。

			prompt			(文字列)
				uniteバッファのプロンプト文字列。

			is_insert		(数値)
				uniteバッファが挿入モードから呼び出されたかどう
				か。

			immediately		(数値)
				uniteバッファが|unite-options-immediately|によ
				って呼び出されたかどうか。

			is_redraw		(数値)
				|<Plug>(unite_redraw)|で呼び出されたかどうか。
				ユーザーが明示的にキャッシュを破棄しているので、
				自前でキャッシュを管理するsourceにとっては重
				要な情報となる。アクションの実行により、キャッ
				シュが無効化された場合も1になる。

			is_invalidate		(数値)
				アクションの実行により、キャッシュが無効化さ
				れたかどうか。uniteバッファの初期化のときに
				も1になる。

			is_async		(数値)
				sourceが非同期に候補を収集するかどうか。
				|unite-source-attribute-async_gather_candidates|
				が存在する場合、1となる。これをsource内で明示的
				に0にセットすると今後非同期では呼ばれなくなる。

			source			(辞書)
				現在候補を収集しているsourceの情報。

			candidates		(リスト)
				フィルタリング終了後の候補。
				|unite-source-attribute-hooks-on_post_filter|内
				でのみ有効。

			source__{name}		(不定)		(任意)
				source独自の付加情報。source側で自由に使用でき
				る。複数回同じsourceを呼び出せるようにするため
				s:変数ではなく、ここに情報を格納することが推
				奨される。

{candidate}					*unite-notation-{candidate}*
			候補を表す辞書変数。次の情報を持つ。

			word			(文字列)
				画面に表示される候補を表す文字列。候補の絞り込
				みに使われる。

			abbr			(文字列)	(任意)
				画面に表示される候補を表す文字列。wordより優先
				されるが、候補の絞り込みには使われない。

			source			(文字列)	(任意)
				候補が所属するsource名。省略された場合、自動
				的にセットされる。

			kind			(文字列)	(任意)
				候補が所属するkind名。省略すると"common"となる。

			is_dummy		(数値)		(任意)
				ダミー候補であるかどうか。省略すると0となる。
				ダミー候補の場合、アクションや移動時に無視さ
				れる。

			source__{name}		(不定)		(任意)
				source独自の付加情報。source側で自由に使用でき
				る。

			action__{name}		(不定)		(任意)
				actionが使用する付加情報。例えば、
				"action__path"はそのファイルが存在するパスを指
				定する。実行するactionによって、その仕様は異な
				る。標準で定義されるkindの仕様については、
				|unite-kind|を参照せよ。

==============================================================================
kindの作成					*unite-create-kind*

autoload/unite/kinds/に*.vimのファイルを置いておくと、自動的に読み込まれる。
その際、unite#kinds#{kind_name}#define()が呼ばれ、戻り値がkindと解釈される。
戻り値はリストでもよく、その場合はkindのリストと解釈される。
kindを追加したくない場合は、空リストを返せば良い。
独自のkindを動的に追加するためには、|unite#define_kind()|を使う。

------------------------------------------------------------------------------
kindの属性					*unite-kind-attributes*

						*unite-kind-attribute-name*
name			文字列		(必須)
			kindの名前。以下の文字で構成しなければならない。
			ただし、"source"と"common"は予約されている。
			文字:
			- a-z
			- 0-9
			- _
			- /

			例:
			- "buffer", "virw/git" と "file_mru" は正当。
			- "BadOne", "!@#$%^&*()_[]{}-|" と "" は
			  kindの名前としては使えない。

						*unite-kind-attribute-default_action*
default_action		文字列		(必須)
			|<Plug>(unite_do_default_action)|を実行した際に呼ばれる
			標準的なアクションの名前を指定する。これを省略した場合
			は実行時にエラーになるが、それを逆手に取り、継承に使う
			だけのインタフェースアクションではこれを定義しないとい
			う手がある。

						*unite-kind-attribute-action_table*
action_table		辞書		(必須)
			アクションテーブルを指定する。
			アクションテーブルは辞書となっていて、キーとして、アク
			ションの名前を取る。値として、次の辞書情報を持つ。
			ただしアクションの名前として、"default"と"nop"は予約さ
			れている。ユーザーは使用できない。
			
			func			(関数)
				アクションの実行時に呼び出される関数。
				{candidate}を引数に取る。アクションの
				is_selectableが1の場合、{candidate}には候補のリ
				ストが渡される。{candidate}はキャッシュされてい
				るので、中身を変更してはならない。変更する場合
				は、|deepcopy()|を使うこと。

			description		(文字列)	(任意)
				アクションを説明する文字列。省略すると""になる。

			is_quit			(数値)		(任意)
				アクション実行前にuniteバッファを終了するかどう
				か。省略すると1になり、uniteバッファを終了する。

			is_selectable		(数値)		(任意)
				このアクションが候補を複数選択可能した際に実行
				可能かどうか。省略すると0になり、複数選択してい
				るとエラーとなる。

			is_invalidate_cache	(数値)		(任意)
				このアクションを実行した際にキャッシュを無効化
				するかどうか。省略すると0になり、キャッシュは無
				効化されない。

						*unite-kind-attribute-alias_table*
alias_table		辞書		(任意)
			エイリアステーブルを指定する。
			エイリアステーブルは辞書となっていて、キーとして、アク
			ションの名前を取る。値として、エイリアスとなるアクショ
			ン名を持つ。アクションに短縮名を付けたいときに便利で
			ある。値を"nop"とすると、そのアクションを無効化でき
			る。

						*unite-kind-attribute-parents*
parents			リスト		(任意)
			アクションを継承するkindの名前をリスト形式で指定する。
			省略すると、["common"]となる。アクションの検索について
			は、リストの順番で辿っていき、後から見つかったものによ
			って上書きされる。アクションは親のkindを辿って再帰的に
			検索されるので、無限ループに注意しなければならない。

------------------------------------------------------------------------------
sourceに特殊化されたkind			*unite-implicit-kind-for-a-source*

uniteはある特定のsourceによって生成されたkindを認識し、アクションを特殊化す
ることができる。特殊化されたkind名は、"source/{name}/{kind}"となる。{name}には
source名を指定する。{kind}を"*"とすると、どんなkindにもマッチする。

例えば、source "file"において"delete"アクションを追加するには以下のようにする。
>
	call unite#custom_action('source/file/*', 'delete', function('...'))
<
------------------------------------------------------------------------------
actionの解決順序				*unite-action-resolution-order*

例えば、source "file"によって生成された、kind "file"(これは"openable",
"cdable"を継承している)の候補に対してアクションを実行した場合、アクションの
解決順序は次のようになる。

(1) Custom action table for kind "source/file/file".
(2) Default action table for kind "source/file/file".
(3) Custom action table for kind "source/file/*".
(4) Default action table for kind "source/file/*".
(5) Custom action table for kind "file".
(6) Default action table for kind "file".
(7) Custom action table for kind "openable".
(8) Default action table for kind "openable".
(9) Custom action table for kind "cdable".
(10) Default action table for kind "cdable".
(11) Custom action table for kind "common".
(12) Default action table for kind "common".

==============================================================================
filterの作成					*unite-create-filter*

autoload/unite/filters/に*.vimのファイルを置いておくと、自動的に読み込まれる。
その際、unite#filters#{filter_name}#define()が呼ばれ、戻り値がfilterと解釈される。
戻り値はリストでもよく、その場合はfilterのリストと解釈される。
filterを追加したくない場合は、空リストを返せば良い。
独自のfilterを動的に追加するためには、|unite#define_filter()|を使う。

------------------------------------------------------------------------------
filterの属性					*unite-filter-attributes*

						*unite-filter-attribute-name*
name			文字列		(必須)
			filterの名前。

						*unite-filter-attribute-filter*
filter			関数		(必須)
			uniteが候補を収集した後、フィルタリングする際に呼ばれる。
			この関数は{candidates}と{context}を引数に取り、
			{candidate}のリストを返す。
			{candidate}はsourceが
			|unite-source-attribute-gather_candidates|で収集した
			候補リスト、{context}はsourceが呼ばれたときのコンテ
			キスト情報である。
			{context}については|unite-notation-{context}|、
			{candidate}については|unite-notation-{candidate}|を参照
			せよ。

						*unite-filter-attribute-description*
description		文字列		(任意)
			filterを説明する文字列。省略すると""になる。

==============================================================================
設定例						*unite-examples*
>
	
	" The prefix key.
	nnoremap    [unite]   <Nop>
	nmap    f [unite]
	
	nnoremap <silent> [unite]c  :<C-u>UniteWithCurrentDir -buffer-name=files buffer file_mru bookmark file<CR>
	nnoremap <silent> [unite]b  :<C-u>UniteWithBufferDir -buffer-name=files -prompt=%\  buffer file_mru bookmark file<CR>
	nnoremap <silent> [unite]r  :<C-u>Unite -buffer-name=register register<CR>
	nnoremap <silent> [unite]o  :<C-u>Unite outline<CR>
	nnoremap  [unite]f  :<C-u>Unite source<CR>
	
	autocmd FileType unite call s:unite_my_settings()
	function! s:unite_my_settings()"{{{
	  " Overwrite settings.
	
	  nmap <buffer> <ESC>      <Plug>(unite_exit)
	  imap <buffer> jj      <Plug>(unite_insert_leave)
	  "imap <buffer> <C-w>     <Plug>(unite_delete_backward_path)
	
	  " <C-l>: manual neocomplcache completion.
	  inoremap <buffer> <C-l>  <C-x><C-u><C-p><Down>
	
	  " Start insert.
	  "let g:unite_enable_start_insert = 1
	endfunction"}}}
	
	let g:unite_source_file_mru_limit = 200
	let g:unite_cursor_line_highlight = 'TabLineSel'
	let g:unite_abbr_highlight = 'TabLine'
	
	" For optimize.
	let g:unite_source_file_mru_filename_format = ''
	
>
==============================================================================
ユーザーのsource				*unite-user-sources*

作者以外の手により作成されたsourceについては、Wikiのページを参照せよ。
https://github.com/Shougo/unite.vim/wiki/unite-plugins

==============================================================================
更新履歴					*unite-changelog*

doc/unite.txtを参照せよ。

==============================================================================
vim:tw=78:ts=8:ft=help:norl:noet:fen:fdl=0:
doc/unite.txt	[[[1
2910
*unite.txt*	Unite all sources

Version: 2.2
Author : Shougo <Shougo.Matsu@gmail.com>
Documentation Author: ujihisa <ujihisa at gmail com>
License: MIT license  {{{
    Permission is hereby granted, free of charge, to any person obtaining
    a copy of this software and associated documentation files (the
    "Software"), to deal in the Software without restriction, including
    without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to
    permit persons to whom the Software is furnished to do so, subject to
    the following conditions:
    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
    OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}}}

CONTENTS					*unite-contents*

Introduction		|unite-introduction|
Usage			|unite-usage|
Install			|unite-install|
Interface		|unite-interface|
  Commands		  |unite-commands|
  Variables		  |unite-variables|
    Sources variables	    |unite-sources-variables|
  Key mappings		  |unite-key-mappings|
  Functions		  |unite-functions|
  Sources		  |unite-sources|
  Kinds			  |unite-kinds|
  Actions		  |unite-actions|
  Filters		  |unite-filters|
Create source		|unite-create-source|
Create kind		|unite-create-kind|
Create filter		|unite-create-filter|
Configulation Examples	|unite-examples|
User defined source	|unite-user-sources|
Changelog		|unite-changelog|

==============================================================================
INTRODUCTION					*unite-introduction*

*unite* or *unite.vim* searches and displays information from arbitrary sources like files,
buffers, recently used files or registers. You can run one of defined action
on a target displayed.

The differences between |unite| and similar plugins like |fuzzyfinder| or |ku|
are that |unite| doesn't use the built-in completion interface of Vim, and
integrates sources at the same time.

==============================================================================
USAGE						*unite-usage*

	In case when you run with files and buffers as the source
>
	:Unite file buffer
<

	In case when you run with the initial input value foo
>
	:Unite -input=foo file
<

The unite you started splits the window horizontally as default, opening on
the top of the Vim.  For example,
>
	:Unite file
<
lists up the files of the current directory.  You may choose one of the
candidates by moving j or k, and typing Enter opens the candidate in a new
buffer.  That's the default action for candidates of which kind is file.  You
may also select an action with <Tab> on a candidate.  See also |unite-action|
about the actions.

You can narrow down the candidates with a keyword.  After opening a unite
window, the cursor goes on the right side of > in the 2nd line by typing i.
Then you can input a keyword to narrow down the candidates.  Each single
characters you type narrows down the candidates.  You also can use a wild
card * as an arbitrary character sequence.  For example
>
	*hisa
<
matches hisa, ujihisa, or ujihisahisa.  Furthermore, two consequence wild cards
matches directory recursively.  For example
>
	**/foo
<
matches bar/foo or buzz/bar/foo.  Note that you should consider using
|file_rec| that will be described bellow in most cases.

You may specify multiple keywords to narrow down, separating by spaces.
>
	foo bar
<
That matches candidates that match both foo and bar.

You may specify negative conditions with !.
>
	foo !bar
<
That matches foo but candidates that match bar will be rejected.

	You may add wild cards automatically with / if you specify files on
	-buffer-name option.  That's handy in case you select file with unite.
>
	:Unite -buffer-name=files file
<
See also |unite_default_key_mappings| for other actions.

Screencast is available. Thanks ujihisa!
http://www.ustream.tv/recorded/11240673

==============================================================================
INSTALL						*unite-install*

Install the distributed files into Vim script directory which is usually
~/.vim/, or $HOME/vimfiles on Windows.

In future VimJolts the Vim plugin package manager will support unite, you can
install unite just by `jolt install unite`.

You may run unite with |:Unite| command with source as parameters if you
succeeded in installing unite.  However, it's pain in ass to run the command
explicitly every time, so I recommend you to set a key mapping for the
command.

==============================================================================
INTERFACE					*unite-interface*

------------------------------------------------------------------------------
COMMANDS 					*unite-commands*

:Unite [{options}] {sources}			*:Unite*
		Shows the candidate window of {sources} with empty initial
		narrowing text.  You may specify multiple {sources} with
		spaces as a separator; the order of the {sources} will be the
		order of candidates.  See |unite-source| also for sources.
		This command restores previous position.

		In case you are already on unite buffer, the narrowing text
		will be stored.

		You may give a list of strings, separating with ":", after
		the name of sources. You must escape ":" and "\" with "\"
		for parameters themselves. "::" is abbreviation argument. It
		depends on the sources how the parameters will be interpreted.

		Examples:
		"file:foo:bar": the parameters of source file are
		                ["foo", "bar"].
		"file:foo\:bar": the parameters of source file are
		                ["foo:bar"].
		"file:foo::baz": the parameters of source file are
		                ["foo", "", "bar"].

						*unite-options*
		{options} are options for a unite buffer.  You may give the
		following parameters for a option; you must escape with \ when
		it contains spaces.

						*unite-options-buffer-name*
		-buffer-name={buffer-name}
		Specifies a buffer name. You can share a configuration of
		functions like |unite#set_substitute_pattern()| by giving a
		buffer name for unite buffers that have same purpose.
		The default buffer name is 'default'.

						*unite-options-input*
		-input={input-text}
		Specifies an initial narrowing text. The default value is ''.

						*unite-options-prompt*
		-prompt={prompt-text}
		Specifies a prompt. The default value is '> '.

						*unite-options-default-action*
		-default-action={default-action}
		Specifies a default action. The default value is 'default'.

						*unite-options-start-insert*
						*unite-options-no-start-insert*
		-start-insert
		Opens unite buffer with narrowing mode.

		-no-start-insert
		Opens unite buffer with normal mode.

		When both options are undefined, this will depend on
		|g:unite_enable_start_insert| option.
		The behavior when both options are defined is undefined.

						*unite-options-no-quit*
		-no-quit
		Doesn't close the unite buffer after firing an action.  Unless
		you specify it, a unite buffer will be closed when you
		selected an action which is "is_quit".

						*unite-options-winwidth*
		-winwidth={window-width}
		Specifies the width of a unite buffer.  Unless you specify
		it, |g:unite_winwidth| will be used.

						*unite-options-winheight*
		-winheight={window-height}
		Specifies the height of a unite buffer.  Unless you specify
		it, |g:unite_winheight| will be used.

						*unite-options-immediately*
		-immediately
		If the number of candidates is exactly one, it runs default
		action immediately. If candidate is empty, it doesn't open
		unite buffer.

						*unite-options-auto-preview*
		-auto-preview
		When you selected candidate, it runs "preview" action
		automatically.

						*unite-options-completion*
		-completion
		Uses unite completion interface. |unite-options-col| is also
		required.

						*unite-options-col*
		-col={column-number}
		Specifies the called unite buffer position.

						*unite-options-vertical*
		-vertical
		Splits unite window vertically.

						*unite-options-horizontal*
		-horizontal
		Splits unite window horizontally.
		
		When both options are undefined, this will depend on
		|g:unite_enable_split_vertically| option.
		The behavior when both options are defined is undefined.

						*unite-options-direction*
		-direction={direction}
		Defines split position rule. The default value is same to
		|g:unite_split_rule|.

						*unite-options-verbose*
		-verbose
		Print verbose warning messages.

						*unite-options-auto-resize*
		-auto-resize
		Auto resize unite buffer height by candidates number.

:UniteWithCurrentDir [{options}] {sources}	*:UniteWithCurrentDir*
		Equivalent to |:Unite| except that the initial narrowing text
		is the current directory.

:UniteWithBufferDir [{options}] {sources}	*:UniteWithBufferDir*
		Equivalent to |:Unite| except that the initial narrowing text
		is the buffer's directory.

:UniteWithCursorWord [{options}] {sources}	*:UniteWithCursorWord*
		Equivalent to |:Unite| except that the initial narrowing text
		is the word on the cursor.

:UniteResume [{buffer-name}]			*:UniteResume*
		Reuses the unite buffer previously opened named {buffer-name}.
		Reuses the last unite buffer you used if you skipped
		specifying {buffer-name}.  Narrowing texts or candidates are
		same to them.

A command of source				*:unite-sources-command*

:UniteBookmarkAdd [{file}]			*:UniteBookmarkAdd*
		Adds the file into the bookmark list. Unless you specify the
		parameter, the current position of the current file will be
		stored.

------------------------------------------------------------------------------
VARIABLES 					*unite-variables*

g:unite_update_time				*g:unite_update_time*
		Update time interval of candidates for each input of narrowing
		text.  In Msec.

		Default value is 200.

g:unite_enable_start_insert			*g:unite_enable_start_insert*
		If this variable is 1, unite buffer will be Insert Mode
		immediately.

		Default value is 0.

g:unite_split_rule				*g:unite_split_rule*
		Defines split position rule.

		Default value is "topleft".

g:unite_enable_split_vertically			*g:unite_enable_split_vertically*
		If this option is 1, splits unite window vertically.

		Default value is 0; unite window will be splat horizontally.

g:unite_winheight				*g:unite_winheight*
		The height of unite window when it's split horizontally.  It's
		ignored in splitting vertically.

		The default value is 20.

g:unite_winwidth				*g:unite_winwidth*
		The width of unite window when it's split vertically.  It's
		ignored in splitting horizontally.

		The default value is 90.

g:unite_cd_command				*g:unite_cd_command*
		Specifies the Vim command for cd action.
		This command must interpret |`=|.

		The default value is "cd".

g:unite_lcd_command				*g:unite_lcd_command*
		Specifies the Vim command for lcd action.
		This command must interpret |`=|.

		The default value is "lcd".

g:unite_cursor_line_highlight			*g:unite_cursor_line_highlight*
		Specifies the cursor line highlight.
		
		The default value is "PmenuSel".

g:unite_abbr_highlight				*g:unite_abbr_highlight*
		Specifies candidates abbr highlight.
		
		The default value is "Normal".

g:unite_quick_match_table			*g:unite_quick_match_table*
		The table of completion candidates of quick match list,
		corresponding the narrowing text.

		The default value is complex; so see plugin/unite.vim.

g:unite_data_directory				*g:unite_data_directory*
		Specifies directories for configurations internally used in
		unite itself or its sources.  If the directory doesn't exist
		the directory will be automatically generated.  For example source
		of file_mru saves the information of the most recent used
		files on the directory.
		
		Default value is expand('~/.unite'); the absolute path of it.

g:unite_no_default_keymappings			*g:unite_no_default_keymappings*
		If it's 1, unite doesn't map any default key mappings.  You
		shouldn't enable this option without any strong reasons.

		This variable doesn't exist unless you define explicitly.

SOURCES VARIABLES 				*unite-sources-variables*

g:unite_source_file_ignore_pattern		*g:unite_source_file_ignore_pattern*
		Specifies a regex pattern for ignoring some specific
		candidates in |unite-source-file|.  This matches on the full
		path of each files.  If the variable isn't empty string, unite
		filters with the regex pattern on the results.

		Refer autoload/unite/sources/file.vim about the default value.

g:unite_source_file_mru_time_format		*g:unite_source_file_mru_time_format*
		Specifies the output format of the last access time of
		|unite-source-file_mru|.  The format is same to |strftime()|.

		Default value is "(%c) ".

g:unite_source_file_mru_filename_format		*g:unite_source_file_mru_filename_format*
		Specifies the output format of the filename of
		|unite-source-file_mru|.  The format is same to
		|fnamemodify()|.  If this variable is empty, drawing speed
		is faster.

		Default value is ":~:.".

g:unite_source_file_mru_file			*g:unite_source_file_mru_file*
		Specifies the file to write the information of most recent
		used files.

		Default value is |g:unite_data_directory|; '/.file_mru'

g:unite_source_file_mru_limit			*g:unite_source_file_mru_limit*
		The maximum number of most recent files to save.

		Default value is 100.

g:unite_source_file_mru_ignore_pattern		*g:unite_source_file_mru_ignore_pattern*
		Specifies the regexp pattern to ignore candidates of
		|unite-source-file_mru|.  This applies on the full path of
		each files.  Unless this variable value is an empty string,
		Unite filters out the result with the regexp.  Regardless of
		|g:unite_enable_ignore_case| value, it's case sensitive.

		Refer autoload/unite/sources/file_mru.vim about the default
		value.

g:unite_source_directory_mru_time_format	*g:unite_source_directory_mru_time_format*
g:unite_source_directory_mru_directory		*g:unite_source_directory_mru_directory*
g:unite_source_directory_mru_limit		*g:unite_source_directory_mru_limit*
g:unite_source_directory_mru_ignore_pattern	*g:unite_source_directory_mru_ignore_pattern*
		They are same to |unite-source-file_mru| except for the
		targets are |unite-source-directory_mru|.

g:unite_source_bookmark_file			*g:unite_source_bookmark_file*
		Specifies the file that |unite-source-bookmark| writes its
		bookmarks.

		Default value is |g:unite_data_directory|; '/.bookmark'. 

g:unite_source_file_rec_ignore_pattern		*g:unite_source_file_rec_ignore_pattern*
		Specifies the regexp pattern to ignore candidates of
		|unite-source-file_rec|.  This applies on the full path of
		each files.  Unless this variable value is an empty string,
		Unite filters out the result with the regexp.  Regardless of
		|g:unite_enable_ignore_case| value, it's case sensitive.

		Refer autoload/unite/sources/file_rec.vim about the default
		value.

g:unite_source_file_rec_min_cache_files		*g:unite_source_file_rec_min_cache_files*
		Specifies the minimum number of files that
		|unite-source-file_rec| caches.  It doesn't cache if the
		number of files is less than this value.

		Default value is 50.

g:unite_source_grep_command			*g:unite_source_grep_command*
		Set grep command.

		Default value is "grep".

g:unite_source_grep_recursive_opt		*g:unite_source_grep_recursive_opt*
		Set grep recursive option.

		Default value is "-R".

g:unite_source_grep_default_opts		*g:unite_source_grep_default_opts*
		Set grep default options.
		Note: grep output must be this pattern.
		filename:number:pattern
>
		let g:unite_source_grep_default_opts = '-iRHn'
<
		Default value is "-Hn".

g:unite_source_grep_max_candidates		*g:unite_source_grep_max_candidates*
		Set |unite-source-grep| max candidates number.

		Default value is 100.

g:unite_source_line_enable_highlight		*g:unite_source_line_enable_highlight*
		Control whether search keyword is highlighted in unite
		buffer.

		Default value is 1.

g:unite_source_line_search_word_highlight	*g:unite_source_line_search_word_highlight*
		Highlight setting applied to search keyword when
		|g:unite_source_enable_highlight| is true.

		Default value is "Search".

KINDS VARIABLES					*unite-kinds-variables*

g:unite_kind_jump_list_after_jump_scroll	*g:unite_kind_jump_list_after_jump_scroll*
		A number for adjusting the location of the cursor after the
		jump by |unite-kind-jump_list|.  The minimum is 0 which means
		the top of the window and the maximum is 100 which means the
		bottom of the window.

		value	meaning		equivalent command
		--------------------------------------
		0	Window top	normal! |z<CR>|
		50	Window centre	normal! |z.|
		100	Window bottom	normal! |z-|

		Default is 25.

g:unite_kind_openable_persist_open_blink_time	*g:unite_kind_openable_persist_open_blink_time*
		A number of blink time after "persist_open" action by |unite-kind-openable|.

		Default value is "250m"

------------------------------------------------------------------------------
KEY MAPPINGS 					*unite-key-mappings*

Normal mode mappings.

<Plug>(unite_exit)				*<Plug>(unite_exit)*
		Exits unite.

<Plug>(unite_restart)				*<Plug>(unite_restart)*
		Restarts unite.

<Plug>(unite_do_default_action)			*<Plug>(unite_do_default_action)*
		Runs the default action of the default candidates.  The kinds
		of each candidates have their own defined actions.  See also
		|unite-kind| about kinds.  Refer |unite-default-action| about
		default actions.

<Plug>(unite_choose_action)			*<Plug>(unite_choose_action)*
		Runs the default action of the selected candidates.  The kinds
		of each candidates have their own defined actions.  Refer
		|unite-kind| about kinds.

<Plug>(unite_insert_enter)			*<Plug>(unite_insert_enter)*
		Starts inputting narrowing text by the cursor position.  In
		case when the cursor is not on prompt line, this moves the
		cursor into the prompt line automatically.

<Plug>(unite_insert_head)			*<Plug>(unite_insert_head)*
		Starts inputting narrowing text by the head of the line.  In
		case when the cursor is not on prompt line, this moves the
		cursor into the prompt line automatically.

<Plug>(unite_append_enter)			*<Plug>(unite_append_enter)*
		Starts inputting narrowing text by the head of the line.  In
		case when the cursor is not on prompt line, this moves the
		cursor into the prompt line automatically.

<Plug>(unite_append_end)			*<Plug>(unite_append_end)*
		Starts inputting narrowing text by the end of the line.  In
		case when the cursor is not on prompt line, this moves the
		cursor into the prompt line automatically.

<Plug>(unite_toggle_mark_current_candidate)	*<Plug>(unite_toggle_mark_current_candidate)*
		Toggles the mark of the candidates in the current line.  You may
		run an action on multiple candidates at the same time by
		marking multiple candidates.

<Plug>(unite_toggle_mark_current_candidate)	*<Plug>(unite_toggle_mark_all_candidates)*
		Toggles the mark of the candidates in the all lines.

<Plug>(unite_redraw)				*<Plug>(unite_redraw)*
		Without waiting for the update time defined in 
		|g:unite_update_time|, Unite updates its view immediately.
		This is also used internally for updating the cache.

<Plug>(unite_rotate_next_source)		*<Plug>(unite_rotate_next_source)*
		Changes the order of source normally.

<Plug>(unite_rotate_previous_source)		*<Plug>(unite_rotate_previous_source)*
		Changes the order of source reversely.

<Plug>(unite_print_candidate)			*<Plug>(unite_print_candidate)*
		Shows the target of the action of the selected candidate.  For
		example this shows the content of the candidate when the
		kind of the candidate you selected is |word|.

<Plug>(unite_cursor_top)			*<Plug>(unite_cursor_top)*
		Moves the cursor to the top of the Unite buffer.

<Plug>(unite_loop_cursor_down)			*<Plug>(unite_loop_cursor_down)*
		Goes to the next line.  Goes up to top when you are on bottom.

<Plug>(unite_loop_cursor_up)			*<Plug>(unite_loop_cursor_up)*
		Goes to the previous line.  Goes bottom when you are on top.

<Plug>(unite_quick_match_default_action)	*<Plug>(unite_quick_match_default_action)*
		Runs the default action of the selected candidate with using
		quick match.   This doesn't work when there are marked
		candidates.

<Plug>(unite_input_directory)			*<Plug>(unite_input_directory)*
		Narrows with inputting directory name.

<Plug>(unite_delete_backward_path)		*<Plug>(unite_delete_backward_path)*
		Deletes a path upward. Refer to |i_<Plug>(unite_delete_backward_path)|.

<Plug>(unite_toggle_transpose_window)		*<Plug>(unite_toggle_transpose_window)*
		Change the unite buffer's split direction.

<Plug>(unite_narrowing_path)			*<Plug>(unite_narrowing_path)*
		Narrowing candidates by candidate path(or word).

<Plug>(unite_narrowing_input_history)		*<Plug>(unite_narrowing_input_history)*
		Narrowing candidates by input history.

<Plug>(unite_toggle_auto_preview)		*<Plug>(unite_toggle_auto_preview)*
		Toggle the unite buffer's auto preview mode.

Insert mode mappings.

<Plug>(unite_exit)				*i_<Plug>(unite_exit)*
		Exits Unite.

<Plug>(unite_insert_leave)			*i_<Plug>(unite_insert_leave)*
		Changes the mode into Normal mode and move cursor to first
		candidate line.

<Plug>(unite_delete_backward_char)		*i_<Plug>(unite_delete_backward_char)*
		Deletes a char just before the cursor, otherwise quits the Unite.

<Plug>(unite_delete_backward_line)		*i_<Plug>(unite_delete_backward_line)*
		Deletes all chars after the cursor until the end of the line.

<Plug>(unite_delete_backward_word)		*i_<Plug>(unite_delete_backward_word)*
		Deletes a word just before the cursor.

<Plug>(unite_delete_backward_path)		*i_<Plug>(unite_delete_backward_path)*
		Deletes a path upward. For example doing
		<Plug>(unite_delete_backward_path) on >
		/Users/ujihisa/Desktop
<		or >
		/Users/ujihisa/Desktop/
<		this changes into >
		/Users/ujihisa
<		This is handy for changing file paths.

<Plug>(unite_select_next_line)			*i_<Plug>(unite_select_next_line)*
		Goes to the next candidate.  Goes to the top from bottom.

<Plug>(unite_select_previous_line)		*i_<Plug>(unite_select_previous_line)*
		Goes to the previous candidate.  Goes to the bottom from top.

<Plug>(unite_select_next_page)			*i_<Plug>(unite_select_next_page)*
		Shows the next candidate page.

<Plug>(unite_select_previous_page)		*i_<Plug>(unite_select_previous_page)*
		Shows the previous candidate page.

<Plug>(unite_do_default_action)			*i_<Plug>(unite_do_default_action)*
		Same to |<Plug>(unite_do_default_action)|.

<Plug>(unite_toggle_mark_current_candidate)	*i_<Plug>(unite_toggle_mark_current_candidate)*
		Same to |<Plug>(unite_toggle_mark_current_candidate)|.

<Plug>(unite_choose_action)			*i_<Plug>(unite_choose_action)*
		Same to |<Plug>(unite_choose_action)|.

<Plug>(unite_move_head)				*i_<Plug>(unite_move_head)*
		Goes to the top of the line.

<Plug>(unite_quick_match_default_action)	*i_<Plug>(unite_quick_match_default_action)*
		Same to |<Plug>(unite_quick_match_default_action)|.

<Plug>(unite_input_directory)			*i_<Plug>(unite_input_directory)*
		Same to |<Plug>(unite_input_directory)|.

<Plug>(unite_toggle_selected_candidates)	*i_<Plug>(unite_toggle_transpose_window)*
		Same to |<Plug>(unite_toggle_transpose_window)|.

<Plug>(unite_narrowing_path)			*i_<Plug>(unite_narrowing_path)*
		Same to |<Plug>(unite_narrowing_path)|.

<Plug>(unite_narrowing_input_history)		*i_<Plug>(unite_narrowing_input_history)*
		Same to |<Plug>(unite_narrowing_input_history)|.

<Plug>(unite_toggle_auto_preview)	*i_<Plug>(unite_toggle_auto_preview)*
		Same to |<Plug>(unite_toggle_auto_preview)|.

Visual mode mappings.

<Plug>(unite_toggle_mark_selected_candidates)	*v_<Plug>(unite_toggle_selected_candidates)*
		Toggle marks in visual selected candidates.

						*unite_default_key_mappings*
Following keymappings are the default keymappings.

Normal mode mappings.
{lhs}		{rhs}
--------	-----------------------------
i		|<Plug>(unite_insert_enter)|
I		|<Plug>(unite_insert_head)|
a		|<Plug>(unite_append_enter)|
A		|<Plug>(unite_append_end)|
q		|<Plug>(unite_exit)|
<C-r>		|<Plug>(unite_restart)|
<Space>		|<Plug>(unite_toggle_mark_current_candidate)|
*		|<Plug>(unite_toggle_mark_all_candidates)|
<Tab>		|<Plug>(unite_choose_action)|
<C-n>		|<Plug>(unite_rotate_next_source)|
<C-p>		|<Plug>(unite_rotate_previous_source)|
<C-g>		|<Plug>(unite_print_candidate)|
<C-l>		|<Plug>(unite_redraw)|
<C-h>		|<Plug>(unite_delete_backward_path)|
gg		|<Plug>(unite_cursor_top)|
j		|<Plug>(unite_loop_cursor_down)|
<Down>		|<Plug>(unite_loop_cursor_down)|
k		|<Plug>(unite_loop_cursor_up)|
<Up>		|<Plug>(unite_loop_cursor_up)|
<CR>		In case when you selected a candidate, runs default action
d		In case when you selected a candidate, runs delete action
b		In case when you selected a candidate, runs bookmark action
e		In case when you selected a candidate, runs narrow action
p		runs preview action
x		In case when you selected a candidate, runs
		|<Plug>(unite_quick_match_default_action)|

Insert mode mappings.
{lhs}		{rhs}
--------	-----------------------------
<Tab>		|i_<Plug>(unite_choose_action)|
<C-n>		|i_<Plug>(unite_select_next_line)|
<Down>		|i_<Plug>(unite_select_next_line)|
<C-p>		|i_<Plug>(unite_select_previous_line)|
<Up>		|i_<Plug>(unite_select_previous_line)|
<C-f>		|i_<Plug>(unite_select_next_page)|
<C-b>		|i_<Plug>(unite_select_previous_page)|
<CR>		|i_<Plug>(unite_do_default_action)|
<C-h>		|i_<Plug>(unite_delete_backward_char)|
<BS>		|i_<Plug>(unite_delete_backward_char)|
<C-u>		|i_<Plug>(unite_delete_backward_line)|
<C-w>		|i_<Plug>(unite_delete_backward_word)|
<C-a>		|i_<Plug>(unite_move_head)|
<Home>		|i_<Plug>(unite_move_head)|
e		In case when you selected a candidate, runs narrow action
d		In case when you selected a candidate, runs delete action
<Space>		In case when you selected a candidate,
		|i_<Plug>(unite_toggle_mark_current_candidate)|
x		In case when you selected a candidate,
		|i_<Plug>(unite_quick_match_default_action)|

Visual mode mappings.
{lhs}		{rhs}
--------	-----------------------------
<Space>		|v_<Plug>(unite_toggle_mark_selected_candidates)|

==============================================================================
FUNCTIONS					*unite-functions*

CORE						*unite-functions-core*

unite#get_kinds([{kind-name}])			*unite#get_kinds()*
			Gets the kinds of {kind-name}.  Unless they exist this
			returns an empty dictionary.  This returns a
			dictionary which keys are kind names and values are
			the kinds when you skipped giving {kind-name}.

			Changing the return value is not allowed.

unite#get_sources([{source-name}])		*unite#get_sources()*
			Gets the source of {source-name}. Unless they exist
			this returns an empty dictionary.  This returns a
			dictionary which keys are source names and values are
			the sources when you skipped giving {source-name}.

			Changing the return value is not allowed.

CUSTOMS						*unite-functions-customs*

unite#start({sources}, [, {context}])		*unite#start()*
		Creates a new Unite buffer.  In case when you are already on a
		Unite buffer, the narrowing text will be preserved.

		{sources} is a list which elements are formatted as
		{source-name} or [{source-name}, {args}, ...].  You may
		specify multiple string arguments in {args} for {source-name}.

		Refer |unite-notation-{context}| about {context}.  If you
		skipped it will use the default value.

unite#get_context()				*unite#get_context()*
		Gets the context information of the current Unite buffer.
		This is used by functions like |unite#custom_action()| to call
		|unite#start()| internally.

unite#do_action({action-name})			*unite#do_action()*
		Returns the key sequence for running {action-name} action for
		the marked candidates.  This function works only when Unite is
		already activated.  This causes a runtime error if
		{action-name} doesn't exist or the action is invalid.

		This is handy for defining a key mapping to run an action by
		yourself.

		This runs the default action when you specify "default" on
		{action-name}.

		This runs an action on the candidates of the current line or
		the top of the candidates when none of the candidates are marked.

		This is usually used as inoremap <buffer><expr> or
		nnoremap <buffer><expr>.  For example,
>
		nnoremap <silent><buffer><expr> <C-k> unite#do_action('preview')
>
unite#smart_map({narrow-map}, {select-map})	*unite#smart_map()*
		Returns the key sequence which works both modes of narrowing
		and selecting with respect to the given narrow-map and select-map.
		Use this with |unite#do_action()|.  This will be used with
		inoremap <buffer><expr> or nnoremap <buffer><expr> usually.
		Example:
>
		inoremap <buffer><expr> ' unite#smart_map("'", unite#do_action('preview'))
<
unite#set_substitute_pattern({buffer-name}, {pattern}, {subst} [, {priority}])
						*unite#set_substitute_pattern()*
		Specifies a replace pattern of narrowing text for a Unite
		buffer which name is {buffer-name}.  "" is regarded as
		"default" and is equivalent to the case when |:Unite| without
		-buffer-name= option.  You may specify multiple {buffer-name}
		with breaking with ",".  {pattern} is the replace target
		regexp and {subst} is the substitute string.  If you specify a
		same {pattern} again, the setting will just be updated.  You
		may defeat {pattern} with giving {subst} as "".  {priority}
		prioritizes how this replaces.  If you skipped giving
		{priority} it'll be 0.  Give bigger number for a {pattern}
		which must be done earlier.  Note that the initial text of
		Unite buffer will not be replaced with these values.

		You may mimic ambiguous matching with using this function.
>
		call unite#set_substitute_pattern('files', '[[:alnum:]]', '*\0', 100)
		call unite#set_substitute_pattern('files', '[[:alnum:]]', ' \0', 100)
<
		The former does ambiguous search within / while the latter
		does over the /.

		The initial value is defined as the following; on a buffer
		which buffer_name is files, it adds a wildcard in order to
		match ~ as $HOME and to match partially /.
>
		call unite#set_substitute_pattern('files', '^\~',
		\ substitute(unite#util#substitute_path_separator($HOME), ' ', '\\\\ ', 'g'), -100)
		call unite#set_substitute_pattern('files', '[^~.*]\ze/', '\0*', 100)
		call unite#set_substitute_pattern('files', '/\ze[^~.*]', '/*', 100)
<
unite#get_substitute_pattern({buffer-name})
						*unite#get_substitute_pattern()*
		Gets the substitute pattern for narrowing text of a Unite buffer
		name {buffer-name}.  This causes an error if the substitute
		pattern of {buffer-name}.  This is for debugging.

unite#set_buffer_name_option({buffer-name}, {option-name}, {value})
						*unite#set_buffer_name_option()*
		Set {buffer-name} specialized {option-name} to {value}.
		
		This options is available:
		
		substitute_patterns		(Dictionary)
		
		filters				(List)
		
		ignorecase			(Number)
		
		smartcase			(Number)

unite#custom_default_action({kind}, {default-action})
						*unite#custom_default_action()*
		Changes the default action of {kind} into {default-action}.
		You may specify multiple {kind} with separating ",".  For
		example:
>
		call unite#custom_default_action('file', 'tabopen')
<
unite#custom_action({kind}, {name}, {action})
						*unite#custom_action()*
		Adds an {action} which name is {name} for {kind}.
		You may specify multiple {kind} with separating ",".  For
		example:
>
		let my_tabopen = {
		\ 'is_selectable' : 1,
		\ }
		function! my_tabopen.func(candidates)
		  call unite#take_action('tabopen', a:candidates)
		
		  let l:dir = isdirectory(a:candidate.word) ? a:candidate.word : fnamemodify(a:candidate.word, ':p:h')
		  execute g:unite_lcd_command '`=l:dir`'
		endfunction
		call unite#custom_action('file,buffer', 'tabopen', my_tabopen)
		unlet my_tabopen
<
unite#undef_custom_action({kind}, {name})	*unite#undef_custom_action()*
		Deletes an action which name is {name} of {kind} that you
		added with using |unite#custom_action()|.  You may specify
		multiple {kind} with separating ",".  This function doesn't do
		anything if the function doesn't exist.

unite#custom_alias({kind}, {name}, {action})
						*unite#custom_alias()*
		Define action {kind} is another name of {name} in {kind}.  You
		may specify multiple {kind} with separating ",".  If {action}
		is "nop", such action is disabled.
		example:
>
		call unite#custom_alias('file', 'h', 'left')
<
unite#custom_filters({source-name}, {filters})	*unite#custom_filters()*
		Changes the filters of {source-name} into {filters}. You
		may specify multiple sources with separating "," in
		{source-name}.  {filters} is a name of filter or a list of
		the names.

unite#custom_max_candidates({source-name}, {max})
						*unite#custom_max_candidates()*
		Changes the max candidates of {source-name} into {max}. You
		may specify multiple sources with separating "," in
		{source-name}. If {max} is 0, all candidates is displayed.

unite#take_action({action-name}, {candidate})
						*unite#take_action()*
		Runs an action {action-name} against {candidate}.  This will
		be mainly used in |unite#custom_action()|.  When the action is
		is_selectable, the {candidate} will be automatically converted
		into a list.

unite#take_parents_action({action-name}, {candidate}, {extend-candidate})
						*unite#take_parents_action()*
		Same to |unite#take_action()| but searches the parents' action
		table with combining {extend-candidate} on {candidate}.  This
		is handy for reusing parents' actions.

unite#define_source({source})			*unite#define_source()*
		Adds {source} dynamically.  See also |unite-create-source|
		about the detail of source.  If a source which name is same
		exists, that will be overwritten.

unite#define_kind({kind})			*unite#define_kind()*
		Adds {kind} dynamically.  See also |unite-create-kind|
		about the detail of kind.  If a kind which name is same
		exists, that will be overwritten.

unite#define_filter({filter})			*unite#define_filter()*
		Adds {filter} dynamically.  See also |unite-create-filter|
		about the detail of filter.  If a filter which name is same
		exists, that will be overwritten.

unite#undef_source({name})			*unite#undef_source()*
		Removes the source which name is {name} that was added by
		|unite#define_source()|.  If such a source doesn't exist, this
		function doesn't do anything.

unite#undef_kind({name})			*unite#undef_kind()*
		Removes the kind which name is {name} that was added by
		|unite#define_kind()|.  If such a kind doesn't exist, this
		function doesn't do anything.

unite#undef_filter({name})			*unite#undef_filter()*
		Removes the filter which name is {name} that was added by
		|unite#define_filter()|.  If such a filter doesn't exist,
		this function doesn't do anything.

==============================================================================
SOURCES						*unite-sources*

						*unite-source-file*
file		Nominates an input file as a candidate.
		Note: This source doesn't nominates parent file(Example: "../")
		or hidden files(Example: ".gitignore").  If you want to open
		this files, please input ".".

						*unite-source-file_mru*
file_mru	Nominates files you used recently as candidates, ordering
		by time series.

						*unite-source-directory_mru*
directory_mru	Nominates directories you used recently as candidates,
		ordering by time series.

						*unite-source-file_rec*
file_rec	Nominates all directory or file names of input narrowing text
		under the current directly as candidates.  This may get Vim
		frozen when there are too many candidates. The candidates is
		cached by file_rec source. If you clear cache, use
		|<Plug>(unite_redraw)| keymapping.

		Source arguments:
		1. the narrowing text.

						*unite-source-file_rec/async*
file_rec/async	Same as |unite-source-file_rec|. But get files by asynchronously.
		
		Note: This source requires vimproc and external "ls" command.
		Please install.
		http://github.com/Shougo/vimproc/tree/master

		Source arguments:
		1. the narrowing text.

						*unite-source-buffer*
buffer		Nominates opened buffers as candidates, ordering by time
		series.

						*unite-source-buffer_tab*
buffer_tab	Nominates opened buffers only in the current tab as
		candidates, ordering by time series.

						*unite-source-tab*
tab		Nominates opened tabs as candidates, regarding t:cwd as the
		current directory and t:title as the title of the tab.  This
		requires |gettabvar()|.

		If t:title exists, this will be used as "word" for narrowing.
		Otherwise, the buffer name of the current tab will be used.

		Source arguments:
		1. "no-current" remove current tab from candidates.

						*unite-source-register*
register	Nominates the strings stored in registers as candidates.

						*unite-source-bookmark*
bookmark	Nominates files or directories you bookmarked as candidates.

						*unite-source-source*
source		Nominates Unite source names themselves as candidates.

		Runs |unite#start()| with the selected source name, using the
		current Unite buffer context.

						*unite-source-window*
window		Nominates opened windows as candidates, ordering by time
		series.

		Source arguments:
		1. "no-current" remove current window from candidates.

						*unite-source-output*
output		Nominates executed Vim command as candidates.

		Source arguments:
		1. Vim command.

						*unite-source-command*
command		Nominates Vim Ex commands as candidates.

						*unite-source-mapping*
mapping		Nominates Vim mappings as candidates.

						*unite-source-grep*
grep		Nominates "grep" command output as candidates.
		Note: This source is created by Sixeight.

		Note: This source requires vimproc.  Please install.
		http://github.com/Shougo/vimproc/tree/master

		Source arguments:
		1. the target directory.
		2. "grep" options.
		3. the narrowing pattern.
		Max candidates: |g:unite_source_grep_max_candidates|

		Example:
>
	:Unite grep:~/.vim/autoload/unite/sources:-iR:file
<
		Special Target:
		%         : Current buffer name
		#         : Alternate buffer name
		$buffers  : All buffer names

						*unite-source-line*
line		Nominates current buffer lines as candidates.
		Note: This source is created by t9md.

		Source arguments:
		1. the search direction. "all" or "forward" or "backward".
		Max candidates: 100

		Example:
>
	nnoremap <silent> /  :<C-u>Unite -buffer-name=search line -start-insert -no-quit<CR>
<
						*unite-source-resume*
resume		Nominates unite buffers as candidates, ordering by time
		series.

						*unite-source-jump*
jump		Nominates results of |:jumps| command as candidates.

						*unite-source-change*
change		Nominates results of |:changes| command as candidates.

						*unite-source-jump_point*
jump_point	Nominates current line of "file:line" format as candidates.
		This source is useful for |vimshell| outputs.

						*unite-source-file_point*
file_point	Nominates cursor word of filename or URI as candidates.
		This source is useful for |vimshell| outputs.

==============================================================================
KINDS						*unite-kinds*

						*unite-kind-common*
common		A kind for common actions.  Almost all kinds inherit this
		common implicitly. This only requires word key.

						*unite-kind-openable*
openable	An interface that can open.  This doesn't require any keys,
		but a kind that inherits this requires open action.

						*unite-kind-cdable*
cdable		An interface that can cd.

			action__directory	(String)	(Required)
				The target directory

						*unite-kind-file*
file		An interface for files.  This kind is inheriting cdable, uri
		and openable, so this requires kinds that they require.

			action__path		(String)	(Required)
				The path of the target directory

						*unite-kind-buffer*
buffer		An interface for buffers.  This kind is inheriting file, so
		this requires keys that it requires.

			action__buffer_nr	(String		(Required)
				The number of the target buffer

						*unite-kind-tab*
tab		An interface for tabs.  If you can use |gettabvar()|, since this
		kind inherits cdable, this requires keys that those kind
		require.

			action__tab_nr		(String)	(Required)
				The number of the tab

						*unite-kind-directory*
directory	An interface for directories.  This kind is inheriting file,
		this requires keys it requires.

						*unite-kind-word*
word		A String that you can insert

			word			(String)	(Required)
				The string you want to insert

						*unite-kind-jump_list*
jump_list	An interface for jump lists.  This kind is inheriting
		openable, so this requires that it requires.

			action__path		(String)	(Required)
				The path of the file that you'll jump into.

			action__buffer_nr	(String)	(Required)
				The buffer number of the buffer that you'll
				jump into.

			action__line		(Number)	(Optional)
				The line number in the file you'll jump into.

			action__col		(Number)	(Optional)
				The column number in the file you'll jump
				into.

			action__pattern		(String)	(Optional)
				The search pattern that you'll search after
				you open the file.

			action__signature	(String)	(Optional)
				In case you cannot assume the uniqueness where
				you'll jump into only by the pattern of
				action__pattern and action__line, a unique
				String to distinguish lines that match same
				pattern.

		About action__signature and calc_signature() function

		A source which specifies action__signature must define
		cals_signatre() function for calculating the signature by the
		line number of the buffer.  calc_signature() receives {lnum}
		as the first argument and returns a signature in String, where
		{lnum} is a line number.  jump_list compares signatures with
		calling this function.

		The below is an example.
>
		function! s:source.calc_signature(lnum)
		  let range = 2
		  let from = max([1, a:lnum - range])
		  let to   = min([a:lnum + range, line('$')])
		  return join(getline(from, to))
		endfunction
<
						*unite-kind-command*
command		An interface for Ex commands of Vim

			action__command		(String)	(Required)
				The command to run

						*unite-kind-window*
window		An interface for Windows of Vim
		This kind is inheriting cdable, so this requires keys that it
		requires.

			action__window_nr	(String)	(Required)
				The target window number

						*unite-kind-completion*
completion	An interface for completion

			action__complete_word	(String)	(Required)
				The completion word

			action__complete_pos	(Number)	(Required)
				The completion position

			action__complete_info	(String)	(Optional)
				The completion information

						*unite-kind-source*
source		unite.vim source

			action__source_name	(String)	(Required)
				The source name

			action__source_name	(List)		(Optional)
				The source arguments

						*unite-kind-uri*
uri		Files and protocols.

			action__path		(String)	(Required)
				The file path.

==============================================================================
FILTERS						*unite-filters*

Todo
==============================================================================
ACTIONS						*unite-actions*

Actions of each kinds

common					*unite-action-common*
Defines the common interface for all kinds.  This uses condidate.word
internally.
	nop		Do nothing
	yank		Yank the candidate text
	yank_escape	Yank the escaped candidate text
	ex		Input the escaped candidate text into command line
	insert		Input the candidate text into the current buffer

openable					*unite-action-openable*
Defines an interface for files you can open.  This requires an inheriting kind
to define open action.
	tabopen		Open the file in a new tab
	split		Open the file, splitting horizontally
	vsplit		Open the file, splitting vertically
	left		Open the file in the left, splitting vertically
	right		Open the file in the right, splitting vertically
	above		Open the file in the top, splitting horizontally
	below		Open the file in the bottom, splitting horizontally
	persist_open	Open the file in alternate window. Don't close unite
			window.

cdable					*unite-action-cdable*
Defines an interface for files you can move to with cd command.
	cd		Change the current directory.
	lcd		Change the current directory of the current window.
	project_cd	Look for the project directory, and changes the
			current directory there.
	narrow		Narrow down candidates by the directory name
	vimshell	Run |vimshell| on the directory.  This is available
			only when you already installed |vimshell|.
	tabvimshell	Run |:VimShellTab| on the directory.  This is
			available only when you already installed |vimshell|.
	vimfiler	Run |vimfiler| on the directory.  This is available
			only when you already installed |vimfiler|.
	tabvimfiler	Run |:VimFilerTab| on the directory.  This is
			available only when you already installed |vimfiler|.
	rec		Run |unite-source-file_rec| on the directory.
	file		Run |unite-source-file| on the directory.

file						*unite-action-file*
Opens a file into a new buffer.  This kind extends |unite-action-openable| and
|unite-action-cdable|.
	open		Open the file
	preview		Open the file into preview window
	bookmark	Add the file into your bookmark
	mkdir		Make directory. If exists file, this action is invalid.
	rename		Change the file name.
	grep		grep files
	grep_directory	grep directories

buffer						*unite-action-buffer*
This kind extends |unite-action-file|.
	delete		|:bdelete| the buffer
	fdelete		|:bdelete!| the buffer
	wipeout		|:bwipeout| the buffer
	unload		|:bunload| the buffer
	bookmark	Add the candidate into your bookmark
	rename		Change the buffer name and file name.

tab						*unite-action-tab*
This kind extends actions of |unite-action-cdable| only when |gettabvar()|
exists.
	open		Show the tab
	delete		Close the tab
	
	The following action requires |gettabvar()| and t:cwd.
	rename		Change the title of the tab

directory					*unite-action-directory*
This kind extends actions of |unite-action-file|.  This doesn't have any
additional actions.  You may want to use this to change the default_action
when the target is a directory.

word						*unite-action-word*
This kind doesn't have any additional actions.  You may want to use this to
change the default_action when the target is a word.

jump_list					*unite-action-jump_list*
This kind extends actions of |unite-action-openable|.  Let me explain about
the additional actions defined in this.
	open		Jump to the location of the candidate
	preview		Preview around the location of the candidate
	replace		Replace selected candidates with |qfreplace| plugin

command						*unite-action-command*
	execute		Execute the command
	edit		Input the command into command line

window						*unite-action-window*
This kind extends ctions of |unite-action-cdable|.
	open		Move to the window
	delete		Close the window
	only		Close all windows besides the window

completion					*unite-action-completion*
	insert		Insert the candidate
	preview		Show the information of the candidate

uri						*unite-action-uri*
	start		Open file with associated program.

Actions of each sources

file_mru					*unite-action-file_mru*
	delete		Delete from most recent used file candidates

directory_mru					*unite-action-directory_mru*
	delete		Delete from most recent used directory candidates

bookmark					*unite-action-bookmark*
	delete		Delete from bookmark file candidates

						*unite-default-action*
Default actions

kind		action
{kind}		{action}
----------	----------
file		open
buffer		open
tab		open
directory	narrow
word		insert
jump_list	open

==============================================================================
CREATE SOURCE					*unite-create-source*

Todo

------------------------------------------------------------------------------
SOURCE ATTRIBUTES				*unite-source-attributes*

						*unite-source-attribute-name*
name			String		(Required)
			The name of a source. It must consist of the following
			characters:
			- a-z
			- 0-9
			- _
			- /

			For example:
			- "buffer" , "file_mru" and "virw/git" are valid.
			- "BadOne", "!@#$%^&*()_[]{}-|" and "" are invalid.

						*unite-source-attribute-gather_candidates*
gather_candidates	Function	(Required)
			Todo

						*unite-source-attribute-change_candidates*
change_candidates	Function	(Optional)
			Todo

						*unite-source-attribute-async_gather_candidates*
async_gather_candidates	Function	(Optional)
			Todo

						*unite-source-attribute-hooks*
hooks			Dictionary		(Optional)
			Todo

			on_init			*unite-source-attribute-hooks-on_init*
			Todo

			on_syntax		*unite-source-attribute-hooks-on_syntax*
			Todo

			Example:
>
			syntax match uniteSource_FileMru_Time /(.*)/ contained containedin=uniteSource_FileMru
			highlight default link uniteSource_FileMru_Time Statement
<
			Todo

			on_close		*unite-source-attribute-hooks-on_close*
			Todo

			on_post_filter		*unite-source-attribute-hooks-on_post_filter*
			Todo

						*unite-source-attribute-action_table*
action_table		Dictionary		(Optional)
			Todo
>
			'action_table' : { 'buffer' : foo_action_table }
<
			Todo

						*unite-source-attribute-default_action*
default_action		Dictionary		(Optional)
			Todo

						*unite-source-attribute-alias_table*
alias_table		Dictionary		(Optional)
			Todo

						*unite-source-attribute-max_candidates*
max_candidates		Number		(Optional)
			The maximum number of candidates.

			This attribute is optional; if it is not given, 0 is
			used as the default value.  This means maximum number
			is infinity.

						*unite-source-attribute-required_pattern_length*
required_pattern_length	Number		(Optional)
			Todo

						*unite-source-attribute-is_volatile*
is_volatile		Number		(Optional)
			Todo

						*unite-source-attribute-description*
description		String		(Optional)
			Todo

						*unite-source-attribute-syntax*
syntax			String		(Optional)
			Todo

						*unite-source-attribute-filters*
filters			List		(Optional)
			Todo

						*unite-source-attribute-source__*
source__		Unknown		(Optional)
			Todo

NOTATION					*unite-notation*

{context}					*unite-notation-{context}*
			Todo

			input			(String)
				The input string of unite buffer.

			buffer_name		(String)
				The name of unite buffer.

			prompt			(String)
				The prompt string of unite buffer.

			is_insert		(Number)
				Todo

			immediately		(Number)
				Todo

			is_redraw		(Number)
				Todo

			is_invalidate		(Number)
				Todo

			is_async		(Number)
				Todo

			source			(Dictionary)
				Todo

			candidates		(List)
				Todo

			source__{name}		(Unknown)		(Optional)
				Todo

{candidate}					*unite-notation-{candidate}*
			Todo

			word			(String)
				Todo

			abbr			(String)	(Optional)
				Todo

			source			(String)	(Optional)
				Todo

			kind			(String)	(Optional)
				Todo

			is_dummy		(Number)	(Optional)
				Todo

			source__{name}		(Unknown)	(Optional)
				Todo

			action__{name}		(Unknown)	(Optional)
				Todo

==============================================================================
CREATE KIND					*unite-create-kind*

Todo

------------------------------------------------------------------------------
KIND ATTRIBUTES					*unite-kind-attributes*

						*unite-kind-attribute-name*
name			String		(Required)
			The name of a kind. It must consist of the following
			characters:
			- a-z
			- 0-9
			- _
			- /

			For example:
			- "buffer" , "file_mru" and "virw/git" are valid.
			- "BadOne", "!@#$%^&*()_[]{}-|" and "" are invalid.

						*unite-kind-attribute-default_action*
default_action		String		(Required)
			Todo

						*unite-kind-attribute-action_table*
action_table		Dictionary		(Required)
			Todo
			
			func			(Function)
				Todo

			description		(String)	(Optional)
				Todo

			is_quit			(Number)		(Optional)
				Todo

			is_selectable		(Number)		(Optional)
				Todo

			is_invalidate_cache	(Number)		(Optional)
				Todo

						*unite-kind-attribute-alias_table*
alias_table		Dictionary		(Optional)
			Todo

						*unite-kind-attribute-parents*
parents			List		(Optional)
			Todo

------------------------------------------------------------------------------
IMPLICIT KIND					*unite-implicit-kind-for-a-source*

Todo
>
	call unite#custom_action('source/file/*', 'delete', function('...'))
<
------------------------------------------------------------------------------
ACTION RESOLUTION ORDER				*unite-action-resolution-order*

Todo

(1) Custom action table for kind "source/file/file".
(2) Default action table for kind "source/file/file".
(3) Custom action table for kind "source/file/*".
(4) Default action table for kind "source/file/*".
(5) Custom action table for kind "file".
(6) Default action table for kind "file".
(7) Custom action table for kind "openable".
(8) Default action table for kind "openable".
(9) Custom action table for kind "cdable".
(10) Default action table for kind "cdable".
(11) Custom action table for kind "common".
(12) Default action table for kind "common".

==============================================================================
CREATE FILTER					*unite-create-filter*

Todo

------------------------------------------------------------------------------
FILTER ATTRIBUTES				*unite-filter-attributes*

						*unite-filter-attribute-name*
name			String		(Required)
			The filter name.

						*unite-filter-attribute-filter*
filter			Function		(Required)
			Todo

						*unite-filter-attribute-description*
description		String		(Optional)
			Todo

==============================================================================
EXAMPLES					*unite-examples*
>
	
	" The prefix key.
	nnoremap    [unite]   <Nop>
	nmap    f [unite]
	
	nnoremap <silent> [unite]c  :<C-u>UniteWithCurrentDir -buffer-name=files buffer file_mru bookmark file<CR>
	nnoremap <silent> [unite]b  :<C-u>UniteWithBufferDir -buffer-name=files -prompt=%\  buffer file_mru bookmark file<CR>
	nnoremap <silent> [unite]r  :<C-u>Unite -buffer-name=register register<CR>
	nnoremap <silent> [unite]o  :<C-u>Unite outline<CR>
	nnoremap  [unite]f  :<C-u>Unite source<CR>
	
	" Start insert.
	"let g:unite_enable_start_insert = 1
	
	autocmd FileType unite call s:unite_my_settings()
	function! s:unite_my_settings()"{{{
	  " Overwrite settings.
	
	  nmap <buffer> <ESC>      <Plug>(unite_exit)
	  imap <buffer> jj      <Plug>(unite_insert_leave)
	  "imap <buffer> <C-w>     <Plug>(unite_delete_backward_path)
	
	  " <C-l>: manual neocomplcache completion.
	  inoremap <buffer> <C-l>  <C-x><C-u><C-p><Down>
	endfunction"}}}
	
	let g:unite_source_file_mru_limit = 200
	let g:unite_cursor_line_highlight = 'TabLineSel'
	let g:unite_abbr_highlight = 'TabLine'
	
	" For optimize.
	let g:unite_source_file_mru_filename_format = ''
	
>
==============================================================================
USER DEFINED SOURCES				*unite-user-sources*

See Wiki page(Japanese).
https://github.com/Shougo/unite.vim/wiki/unite-plugins

==============================================================================
CHANGELOG					*unite-changelog*

2011-08-13
- Fixed recache candidates.
- Improved <Plug>(unite_insert_leave) keymapping.
- Fixed documentation.
- Released ver.2.2.

2011-08-12
- Fixed change_candidates().

2011-08-11
- Improved toggle mark.

2011-08-10
- Optimized source line.
- Added jump_point source.
- Added file_point source and uri kind.
- Kind file inherited uri kind.
- Improved command-line window detection.
- Fixed for cursorline.
- Fixed for colorcolumn.
- Changed default prompt.
- Improved autocmd.
- Improved cursor down/up behavior.
- Initialize ignorecase and smartcase value in files buffer.

2011-08-09
- Improved context source.

2011-08-08
- Improved matcher.

2011-08-06
- Fixed file_rec path problem.

2011-08-05
- Improved quick match.
- Fixed file_rec/async source.

2011-08-04
- Fixed windows restore.
- Added <Plug>(unite_toggle_auto_preview) keymapping.
- Fixed file_rec relative path.
- Improved cursor line.
- Optimized unite-file.
- Asynchronous file_rec source(experimental).
- Improved file_rec.
- Added file_rec/async source.
- Added <Plug>(unite_narrowing_input_history) keymapping.

2011-08-03
- Improved relative converters.
- Added source kind.
- Improved g:unite_abbr_highlight default value.
- Improved documentation.

2011-08-02
- Improved mru filtering.

2011-08-01
- Improved unite-grep source.
- Added converter_relative_abbr and converter_relative_word.
- Improved converter_relative.
- Improved quit session.
- Improved file_rec source.

2011-07-31
- Fixed preview problems.
- Fixed arguments parse.

2011-07-23
- Added jump source.
- Added change source.

2011-07-21
- Set max candidates in line source.
- Improved matcher.
- Improved restore position.

2011-07-20
- Fixed source args parse.
- Fixed filter description.
- Fixed filters.
- Improved matcher_regexp filter.

2011-07-17
- Fixed resource leak in grep source.
- Use new vimproc function.
- Improved async.
- Fixed args problem.
- Supported null args.
- Implemented abbreviation argument.

2011-07-16
- Improved redraw.

2011-07-15
- Fixed jump_list kind.

2011-07-13
- Added -auto-resize option.
- Fixed unite#clear_message().
- Improved file_rec source.

2011-07-12
- Fixed on_close call timing.

2011-07-11
- Fixed fatal mru bug.
- Fixed unite#start() description.
- Implemented default_action syntax sugar.
- Improved source action_table description.

2011-07-10
- Fixed narrowing problem.
- Improved buffer and file_mru source abbr.
- Improved delete backword path keymapping.
- Fixed buffer source error.
- Fixed preview action in completion kind.

2011-07-09
- Improved command line buffer detect.
- Improved substitute pattern.
- Deleted <ESC> default mapping.
- Improved on InsertEnter.

2011-07-08
- Added unite#custom_max_candidates().
- Fixed s:print_buffer().
- Detect command line buffer.
- Improved mru.

2011-07-07
- Improved unite#print_message() and unite#clear_message() behavior.
- Improved grep source behavior.

2011-07-06
- Ver.2.2 development is started.
- Fixed file actions.

------------------------------------------------------------------------------
ChangeLog unite.vim Ver.2.1:

2011-07-05
- Released unite.vim Ver.2.1.

2011-07-03
- Improved insert enter mappings.
- Renamed lines source to line.

2011-07-02
- Added matcher_regexp filter.
- Changed lines and grep filters.
- Added resume source.

2011-06-30
- Added direction argument in source lines.
- Improved file_rec source.
- Added g:unite_source_file_rec_min_cache_files option.

2011-06-28
- Added <Plug>(unite_toggle_selected_candidates) keymapping.
- Improved unite buffer initialize.
- Added <Plug>(unite_narrowing_path) keymapping.
- Supported redraw in source lines.
- Improved unite buffer initialize.

2011-06-26
- Improved autocmd.
- Improved matcher.
- Improved error highlight.
- Improved preview action in jump_list.
- Added lines source.
- Fixed for syntax case.
- Added persist_open action.

2011-06-25
- Added rename action.
- Deleted search pattern.
- Fixed initialize unite bug.
- Added grep source(Thanks Sixeight).
- Improved unite-grep.
- Added pattern highlight in unite-grep.

2011-06-24
- Don't follow symbolic link in file_rec source.

2011-06-20
- Improved unite -immediately behavior.
- Fixed path separator problem in unite-file.

2011-06-19
- Improved detect error in mru.

2011-06-18
- Fixed hlsearch.

2011-06-17
- Fixed caching bug.
- Improved s:recache_candidates().

2011-06-15
- Fixed unite-buffer-name problem.

2011-06-13
- Fixed switch_unite_buffer().

2011-06-12
- Fixed quit unite buffer problem.

2011-06-11
- Improved kind tab.

2011-06-10
- Added -verbose option.

2011-06-09
- Improved unite#print_error().

2011-06-07
- Improved "e" mapping.
- Changed insert mode mappings.

2011-06-04
- Added <Plug>(unite_toggle_mark_all_candidates) keymapping.
- Deleted l default keymapping.

2011-06-03
- Fixed unite-register freeze.
- Fixed initialize error.
- Added file action in cdable kind.
- Improved narrow action.

2011-06-02
- Added replace action in jump_list kind.
- Improved redraw.

2011-06-01
- Fixed recaching.
- Fixed choose_action.

2011-05-31
- Improved redraw flag.
- Fixed is_redraw flag.

2011-05-21
- Improved file_mru.

2011-05-16
- Fixed directory_mru error.

2011-05-15
- Improved alias in choose action.

2011-05-14
- Fixed kind command.
- Fixed preview action.
- Updated vital.vim.
- Fixed choose_action().

2011-05-08
- Fixed :UniteResume error.
- Improved cursor move behavior.
- Ver.2.1 development is started.

2011-05-06
- Fixed syntax error.
- Fixed example.
- Improved g:unite_enable_start_insert option.
- Added .gitignore file.
- Improved file source discription.

2011-05-01
- Ver.2.0 is released.

------------------------------------------------------------------------------
ChangeLog unite.vim Ver.2.0:

2011-04-28
- Fixed unite source.

2011-04-27
- Refactored a bit.

2011-04-26
- Implemented position restore.
- Improved auto preview behavior.
- Implemented mapping source.

2011-04-25
- Fixed file_mru error.

2011-04-22
- Set cpoptions.
- Improved file_rec source.

2011-04-21
- Fixed redraw candidates bug.

2011-04-20
- Fixed default action bug.

2011-04-17
- Improved auto preview.
- Ignore invalid action.
- Improved source file.
- Improved loop cursor down/up.
- Implemented dummy candidates.

2011-04-16
- Fixed auto preview bug.

2011-04-15
- Supported yanktmp in register source.

2011-04-13
- Added ignorecase and smartcase buffer_name options.
- Fixed help.

2011-04-12
- Fixed quick match bug.
- Fixed choose action.
- Improved redraw.
- Improved source file dummy candidates.

2011-04-09
- Improved file_rec source.
- Caching file_rec source.
- Fixed narrowing action.

2011-04-07
- Fixed hlsearch bug.
- Fixed sidescrolloff problem.
- Added is_async context flag.

2011-04-04
- Fixed quick match error.
- Improved default substitute pattern.

2011-04-03
- Improved unite action.

2011-04-02
- Fixed :UniteResume bug.
- Improved unite action.
- Fixed choose action.

2011-04-01
- Fixed get action table bug.
- Fixed priority sort bug.
- Fixed narrowing.
- Improved file source.
- Implemented auto mkdir().
- Implemented mkdir action.

2011-03-31
- Improved sort.
- Optimized initialization.
- Improved truncate.
- Added post_filter hook.
- Improved file_mru source.
- Improved directory_mru source.
- Fixed choose_action documentation.
- Add '..' directory in unite source.

2011-03-29
- Implemented unite action source.
- Changed unite#loaded_source_names_with_args().
- Added sorter_word.
- Improved redraw.
- Fixed highlight settings.

2011-03-25
- Implemented vimfiler and tabvimfiler actions.
- Improved documentation.
- Fixed highlight.
- Changed on_close timing.

2011-03-23
- Fixed kind completion bug.

2011-03-22
- Fixed unite#custom_filters() bug.
- Ommitable candidates source name.

2011-03-17
- Implemented unite#clear_message().
- Added README.
- Implemented source highlight.

2011-03-16
- Added matcher_default, sorter_default, converter_default.

2011-03-13
- Improved delete buffer action.
- Fixed unite#custom_filters() error.
- Fixed help typo.
- Renamed unite#custom_filter() to unite#custom_filters().
- Added filter help.
- Added unite#set_buffer_name_option() help.

2011-03-10
- Implemented filters.
- Refactored buffer name options.
- Implemented buffer name filters.
- Fixed version number.

2011-03-02
- Improved file_rec path.
- Improved buffer abbr.

2011-02-25
- Fixed context bug.
- Fixed narrowing.
- Added unite-source-attribute-source__ description.
- Changed available_xxx API names to get_xxx.

2011-02-24
- Improved unite#print_error() and unite#print_message().
- Implemented input_gather_candidates().
- Fixed narrowing bug.
- Changed input_gather_candidates() to change_candidates().
- gather_candidates attribute is optional.
- Changed file_rec message format.
- Improved syntax.

2011-02-23
- Fixed auto_preview.
- Fixed set search pattern.
- Deleted highlight off.

2011-02-22
- Fixed narrowing bugs.

2011-02-21
- Fixed fnamemodify().
- Improved traverse message in file_rec source.
- Fixed file_rec path problem.

2011-02-18
- Fixed source file path problem.

2011-02-16
- Fixed source register max width.
- Improved syntax.

2011-02-14
- Added -vertical, -horizontal, -direction options.

2011-02-13
- Implemented preview window check.
- Improved initialize sources.

2011-02-11
- Implemented sorter and matcher.
- Implemented unite#print_message().

2011-02-10
- Changed volatile source.
- Added default matcher.
- Added from field in action.

2011-02-09
- Improved file_rec source.
- Added g:unite_source_file_rec_ignore_pattern option.

2011-02-08
- Fixed file_rec index.

2011-02-07
- Implemented asynchronous get candidates.
- Improved file_rec.

2011-02-06
- Fixed redraw buffer source bug.
- Added <Plug>(unite_restart) keymapping.

2011-02-05
- Fixed unite buffer name.

2011-02-04
- Fixed is_insert error.
- Fixed completion kind.
- Improved completion kind.

2011-02-03
- Fixed filtering bug.
- Added completion kind.
- Added col and completion option.

2011-02-02
- Improved syntax.
- Improved highlight off.

2011-02-01
- Don't save directory in file_mru.
- Fixed unite-command.
- Implemented -auto-preview option.

2011-01-31
- Fixed substitute input.
- Fixed buffer source.

2011-01-30
- Added no-current option in window and tab source.
- Added <Plug>(unite_delete_backward_path) keymapping.
- Improved normal mode redraw.
- Refactored.
- Added -immediately option.

2011-01-28
- Improved unite-register.
- Improved narrowing behavior.

2011-01-27
- Improved jump_list.

2011-01-26
- Deleted :NeoComplCacheCachingBuffer.
- Improved quick match message.
- Chomp last /.

2011-01-24
- Added --no-start-insert option.

2011-01-21
- Fixed unite-register freeze bug.

2011-01-20
- Fixed is_redraw bug.

2011-01-19
- Added unite#version().
- Fixed context.
- Improved initialize.

2011-01-17
- Improved unite-command.
- Improved command narrow action.
- Use strdisplaywidth().
- Add history in kind command.
- Fixed :NeoComplCacheCachingBuffer error.
- Improved context.
- Improved rotate sources.
- Improved args.
- Improved syntax.

2011-01-16
- Improved command source.
- Fixed jump_list escape.

2011-01-15
- Improved command action.
- Added command source.

2011-01-14
- Fixed escape jump_list.
- Fixed unite#get_default_action().
- Improved unite-output.
- Added edit action in command kind.

2011-01-11
- Changed :UniteWithInputDirectory behavior.
- Deleted unused tag.

2011-01-10
- Changed yank action.
- Added output source.
- Fixed help typo.
- Improved source arguments description.

2011-01-09
- Ver.1.1 development is started.
- Improved insert leave.
- Supported neocomplcache manual completion.
- Added g:unite_cursor_line_highlight and g:unite_abbr_highlight options.

------------------------------------------------------------------------------
ChangeLog unite.vim Ver.1.0:

2011-01-08
- Optimized file_mru.
- Added g:unite_source_file_mru_filename_format option.

2010-12-27
- Improved autocmd timing.

2010-12-24
- Fixed window sort.

2010-12-23
- Fixed unite window error.
- Improved unite window.

2010-12-19
- Fixed directory_mru.
- Fixed buffer_tab source error.
- Added kind window.
- Fixed buffer bug.

2010-12-16
- Fixed unite source compare.

2010-12-14
- Gather no buflisted buffers in source buffer.
- silent mappings.
- Improved tab source.

2010-12-09
- Added unite.vim tag.

2010-12-08
- Changed cd action behavior.

2010-12-06
- Improved narrowing.

2010-12-05
- Fixed kind/cdable cd and lcd action bugs.
- Added screencast.
- Improved project_cd.

2010-12-03
- Improved help tags.
- Fixed <Plug>(unite_move_head).

2010-12-02
- Fixed select actions.

2010-11-25
- Improved util functions.
- Added wiki page.
- Implemented source description.

2010-11-24
- Fixed file_rec.
- Added user sources.
- Improved open action of jump_list kind.
- Added yank_escape action of common kind.
- Added unite#util#has_vimproc() function.
- Refactored internal functions.

2010-11-22
- Fixed bookmark error.
- Improved jump_list.
- Fixed unite#util#system().
- Added unite#util#get_last_status().
- Changed hooks specification.
- Improved source default_action.
- Fixed rotate source error.

2010-11-21
- Added unite#util#system().
- Added context winnr.
- Added unite-functions-core description.
- Added unite-alias description.
- Improved unite kind descriptions.
- Changed core functions.
- Improved file_rec.
- Added g:unite_source_file_rec_max_depth option.
- Added source source.
- Fixed unite#start() bug.

2010-11-20
- Improved jump_list.

2010-11-19
- Changed file_rec specification.
- Fixed source file.
- Improved narrowing.
- Added mark source description.

2010-11-18
- Improved narrowing.
- Added insert action in common kind.
- Added g:unite_kind_jump_list_search_range variable.
- Improved jump_list.
- Fixed required_pattern_length bug (Thanks ujihisa!)

2010-11-17
- Improved view.
- Don't make directory in dummy file.
- Fixed option parse.
- Added user sources description.

2010-11-16
- Improved Japanese documentation.
- Some Improvements.
- Implemented project_cd action.
- Improved file_mru.
- Implemented directory_mru.
- Improved highlight abbr.
- Improved util functions.
- Fixed tags.
- Improved autoload.
- Improved file and file_rec sources.
- Make directory in dummy file.

2010-11-15
- Escape buffer name.
- Fixed on_init timing.

2010-11-14
- Added <Plug>(unite_rotate_previous_source) and <Plug>(unite_rotate_next_source).
- Improved input parse.
- Added <Plug>(unite_input_directory) keymappings.
- Added on_init and on_close attributes.

2010-11-13
- Fixed :bwipeout command.

2010-11-12
- Improved :UniteWithInputDirectory.
- Improved file actions.

2010-11-11
- Fixed :UniteResume.
- Improved examples.
- Added is_redraw in context.
- Candidates key kind is an option.
- Fixed highlight error.
- Added :UniteWithInputDirectory command.

2010-11-10
- Fixed unite#custom_action().
- Fixed required_pattern_length.
- Fixed buffer kind.
- Added wipeout, unload action in buffer kind.
- Deleted fopen action in file kind.

2010-11-09
- Improved display source name.
- Added command kind.
- Changed unite#take_action().
- Fixed inifinite loop problem.

2010-11-08
- Fixed :UniteWithInput.
- Added outline source description.
- Added unite help settings.

2010-11-07
- Improved analyse options.
- Changed unite#take_action().
- Implemented source/* action_table.
- Improved japanese documentation.
- Implemented alias_table.
- Improved fold method.

2010-11-06
- Improved switch to unite buffer.
- Added -no-quit option.
- Improved drawing lines.
- Added -winwidth and -winheight options.

2010-11-05
- Improved syntax highlight.
- Setlocal nolist.
- Open folds when jump_list open.
- Improved keyword filter.
- Improved buffer filtering.
- Improved drawing candidates.
- Fixed help tags.
- Improved default quick match table.
- Added description in action_table.

2010-11-04
- Improved g:unite_enable_start_insert.
- Improved unite#resume().
- Save search pattern.
- Added kind name description.

2010-11-03
- Added unite#do_action().
- Changed mappings.
- Fixed file_mru error.
- Deleted do action mappings.
- Fixed unite#do_action() description.

2010-11-02
- Improved <Plug>(unite_select_next_line) and <Plug>(unite_select_previous_line).
- Improved prompt check.
- Fixed default parameters set.
- Fixed unite#take_action() error.

2010-11-01
- Improved redraw.
- Fixed parents.

2010-10-31
- Fixed buffer action error.
- Improved buffer actions.
- Changed unite#start() specification.
- Improved common parents.
- Improved unite#take_action().
- Added unite#take_parents_action().
- Added cdable source.
- Improved Japanese documentation.
- Added rec action in cdable.
- Improved tab source.
- Added g:unite_winwidth option.
- Improved match line.

2010-10-30
- Action specification is changed dramatically.
- Improved substitute path separator.
- Added unite#undef_custom_action().
- Source bookmark uses unite#custom_action().
- :Unite command analyses source arguments.
- Improved Japanese documentation.
- Set hlsearch option.
- Added unite#start() and unite#get_context().
- Implemented extend kinds.
- Improved file_rec.

2010-10-29
- Improved selectable action(experimental).
- Improved tab action.
- Fixed unite#take_action().

2010-10-28
- Improved util.vim.
- Sorted marked candidates.

2010-10-26
- Added g:unite_winheight option.
- Improved modifiable.
- Improved file_mru abbr.
- Improved file_mru load.
- Improved unite_tab.
- Added rename action in kind tab.
- Fixed filtering bug.

2010-10-25
- Fixed unite#invalidate_cache() error.

2010-10-24
- Improved file_mru.
- Added tab source.

2010-10-23
- Fixed jump_list.

2010-10-22
- Fixed escape pattern.
- Added bookmark action in buffer.
- Improved file_mru.

2010-10-21
- Improved prompt check.
- Added unite#define_source() and unite#define_kind() and unite#undef_source() and unite#undef_kind().
- Added unite-user-sources.
- Improved Japanese help.
- Added tabvimshell action in directory kind.
- Improved jump_list preview action.

2010-10-20
- Improved do_action().
- Fixed source name syntax.
- Added i_<Plug>(unite_do_delete_action) mapping.
- Added -start-insert option.

2010-10-19
- Improved source name completion.

2010-10-16
- Deleted g:unite_enable_quick_match_mappings option.
- Added <Plug>(unite_quick_match_default_action) mapping.
- Improved quick match.
- Improved unite#mappings#do_action() description.
- Added g:unite_quick_match_table option.
- Implemented unite#custom_alias().
- Improved unite#set_substitute_pattern() and unite#custom_alias().
- Implemented unite#custom_default_action().
- Implemented unite#custom_action().
- Fixed buffer append error.
- Improved unite#take_action().

2010-10-15
- Improved <Plug>(unite_select_next_line) and <Plug>(unite_select_previous_line).
- Implemented default-action option.
- Improved completion.
- Displays candidates number.
- Implemented quick match.
- Added g:unite_enable_quick_match_mappings option.
- Added cursor key support.
- Deleted <ESC> mapping.

2010-10-14
- Fixed substitute pattern.
- Improved dummy file.
- Implemented unite#get_substitute_pattern().
- Fixed :Unite completion.
- Improved buffer_tab.
- Improved s:load_default_sources_and_kinds() loading.

2010-10-12
- Fixed get directory error.
- Fixed s:load_default_sources_and_kinds() error.
- Added buffer_tab source.
- Improved buffer sort.

2010-10-11
- Fixed modifiable problem in insert mode.
- Fixed lcd in unite#quit_session().
- Improved <Plug>(unite_delete_backward_path) mapping.
- Improved s:load_default_sources_and_kinds().

2010-10-10
- Changed g:unite_update_time default value.
- Allowed source name contained /.
- Allowed sources#define to list sources.
- Added <Plug>(unite_delete_backward_path) mapping.

2010-10-09
- Improved unite#mappings#smart_map().
- Deleted <S-Tab> default mapping.
- Improved g:unite_source_file_mru_time_format default pattern.
- Improved default mappings.
- Modifiable unite buffer.
- Fixed register source error.
- Improved prompt highlight.
- Improved Japanese description.
- Parse prompt.
- Implemented restore prompt.
- Improved <Plug>(unite_delete_backward_char).
- Fixed syntax clear.
- Fixed :UniteWithInput.

2010-10-08
- Fixed dummy candidates bug.
- Improved buffer narrowing.
- Added common kind.
- Improved unite#mappings#do_action().
- Improved choose action.
- Improved mapping description.
- Changed source action specification.
- Improved cd action.
- Improved dummy.
- Improved English description.
- Added g:unite_cd_command and g:unite_lcd_command options.
- Changed gather_candidates() specification.
- Changed the name of unite#mappings#smart_imap to unite#mappings#smart_map

2010-10-07
- Improved :UniteResume.
- Fixed file_mru word.
- Optimized register source.
- Don't chase link.
- Fixed filtering bug.
- Fixed dummy candidates bug.

2010-10-06
- Improved unite#set_substitute_pattern().
- Deleted / mappings.
- Improved description.
- Improved globing pattern.
- List current buffer.
- Added vimshell action in buffer.
- Improved default substitute patterns.
- Improved / substitute pattern.
- Fixed substitute pattern.
- Added :UniteResume.

2010-10-04
- Fixed Japanese description.
- Deleted :UniteFilemruSweep command.

2010-10-02
- Added file_rec source.
- Improved file_rec source.
- Added :UniteWithInput command.
- Improved get input text.
- Improved invalid source detection.
- Detect home directory in file_rec.

2010-10-01
- Improved iskeyword.

2010-09-30
- Improved g:unite_source_file_ignore_pattern.
- Changed g:unite_temporary_directory into g:unite_data_directory.
- Improved / mapping.
- Fixed globing.
- Deleted setlocal number.
- Fixed / mapping.

2010-09-29
- Added openable kind.
- Print candidates when <Plug>(unite_choose_action).
- Changed / mapping.
- Implemented unite#mappings#smart_imap().

2010-09-28
- Optimized redraw.
- Added <Plug>(unite_do_narrow_action).
- Reverted / behavior.
- Added narrow action in buffer.
- Fixed glob().
- Fixed force redraw bug.
- Added cd and lcd action in buffer.
- Fixed modifiable bug.

2010-09-27
- Fixed file glob.
- Fixed abbr in file_mru.
- Fixed doted file search.
- Improved g:unite_source_file_ignore_pattern.

2010-09-26
- Fixed ignorecase bug.
- Deleted obsolute mappings.
- Improved prompt.
- Implemented input text save.
- Optimized file_mru.
- Added <Plug>(unite_cursor_top) and <Plug>(unite_loop_cursor_down) and <Plug>(unite_loop_cursor_up) mappings.
- Improved documentation.
- Fixed vimshell error.
- Improved glob.

2010-09-24
- Improved <Plug>(unite_select_next_line) and <Plug>(unite_previous_line) behavior.
- Fixed documentation typo.
- Added unite#set_substitute_pattern().
- Deleted g:unite_substitute_patterns.

2010-09-23
- Fixed filtering bug.
- Added the unite-functions description into Japanese document.

2010-09-21
- Ver.1.0 development is started.
- Fixed unite buffer restore bug.

------------------------------------------------------------------------------
ChangeLog unite.vim Ver.0.5
2010-09-21
- Ver.0.5 is released.
- Fixed japanese documentation.
- Improved unite buffer detect.

2010-09-20
- Added :UniteWithCursorWord command.
- Improved initialization.
- Added Japanese help (Thanks naoina!)
- Added <Plug>(unite_toggle_mark_selected_candidates) keymapping.
- Changed some keymappings name.
- Refactored file_mru.
- Added bookmark source.
- Added jump_list kind.
- Added <Plug>(unite_do_bookmark_action)
- Detect vimfiler and vimshell in bookmark.
- Improved buffer search.
- Fixed i_<Plug>(unite_exit).
- Fixed help tags.

2010-09-19
- Fixed wildcard bug.
- Improved buffer check.
- Improved split.

2010-09-18
- Display modified flag in source buffer.
- Fixed multiple unite buffer bug.
- Refactored.
- Fixed :UniteWithBufferDir and :UniteWithCurrentDir.
- Improved multiple unite buffer.
- Set nomodeline.
- Fixed redraw bug.
- Changed g:unite_update_time default value.

2010-09-17
- Implemented escape ' '.
- Added split actions in kind file.
- Added cd and lcd actions in kind file.
- Added ex action in kind file.
- Improved filtering.
- Added vimshell action in kind directory.
- Improved mru format.
- Implemented delete action in file_mru.
- Improved action table.

2010-09-16
- Added <Plug>(unite_move_head) keymapping.
- Deleted h keymappings.
- Added sources variables description.
- Supported ** pattern.
- Added g:unite_source_file_ignore_pattern option.
- Improved g:unite_source_file_mru_ignore_pattern option.
- Implemented option parse.
- Revised examples.
- Improved directory kind.
- Added file kind actions.
- Added preview keymapping.

2010-09-15
- Improved complete sources.
- Implemented choose action.
- Resolve link file.
- Improved restore window.

2010-09-14
- Added dummy candidate in file source.
- Changed cur_text as input.
- Changed unite#start() arguments.

2010-09-12
- Fixed filtering.
- Added source register.
- Added kind word.

2010-09-10
- Ver.0.5 development is started.
- Implemented kind.
- Improved caching.

------------------------------------------------------------------------------
ChangeLog unite.vim Ver.0.1
2010-09-10
- Improved UniteWithBufferDir and UniteWithCurrentDir.

2010-09-09
- Check redrawtime.
- Implemented ! exclude pattern.
- Added g:unite_enable_start_insert option.
- Improved file_mru.
- Fixed filtering bug.
- Detect invalid source name.
- Implemented maximum candidates attribute.
- Added g:unite_split_rule and g:unite_enable_split_vertically options.
- Implemented required_pattern_length attribute.

2010-09-05
- Fixed filtering bug.
- Improved close buffer.
- Improved truncate.
- Improved mappings.

2010-09-04
- Implemented exclude pattern.
- Improved syntax.

2010-08-30
- Improved UniteWithBufferDir.

2010-08-28
- Fixed / mapping.
- Fixed startinsert error.
- Fixed mappings.
- Fixed initialization bug.
- Implemented caching candidates.
- Improved caching behavior.
- Improved <Plug>(unite_enter).
- Added UniteWithCurrentDir.
- Added UniteWithBufferDir.
- Improved <Plug>(unite_do_selected_candidate).

2010-08-27
- Improved <Enter> mappings.

2010-08-25
- Added examples.

2010-08-19
- Added wildcard syntax.

2010-08-18
- Added <Plug>(unite_print_candidate) mapping.
- Improved truncate string.
- Fixed escape bug.
- Implemented insert mode completion.
- Improved truncate max.
- Improved unite_enter.
- Implemented <Plug>(unite_insert_leave) mapping.

2010-08-13
- Fixed highlight bug.
- Fixed buffer filtering bug.
- Fixed load error.

2010-08-12
- Improved source buffer.
- Improved mappings.
- Improved filter.
- Added g:unite_enable_ignore_case and g:unite_enable_smart_case option.

2010-08-10
- Improved keymappings.
- Improved InsertLeave behavior.
- Improved substitute tilde.
- Added g:unite_substitute_patterns option.

2010-08-08
- Fixed nomodifiable error.
- Improved redraw.
- Implemented previous/next source.

2010-08-07
- Implemented buffer delete.
- Improved action table.
- Implemented marks.
- Improved buffer settings.

2010-08-06
- Improved buffer abbr.
- Renamed unite#buf_leave().
- Improved buffer sort.
- Improved cursor position.
- Improved buffer leave.

2010-08-05
- Fixed buffer switch.
- Improved match behavior.

2010-08-04
- Improved menu.
- Improved syntax.
- Improved redrawtime.
- Improved file source.
- Fixed open bugs.

2010-08-03
- Changed mru file format.
- Changed mappings name.
- Added highlight match.

2010-08-02
- Improved mappings name.

2010-08-01
- Updated file_mru.
- Improved mappings.

2010-07-31
- Added documentation.
- Added syntax file.
- Implemented buffer source.
- Added file source.
- Added file_mru source.
- Some improvements.

==============================================================================
vim:tw=78:ts=8:ft=help:norl:noet:fen:fdl=0:
plugin/unite/bookmark.vim	[[[1
42
"=============================================================================
" FILE: bookmark.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 20 Aug 2010
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

if exists('g:loaded_unite_source_bookmark')
  finish
endif

let s:save_cpo = &cpo
set cpo&vim

command! -nargs=? -complete=file UniteBookmarkAdd call unite#sources#bookmark#_append(<q-args>)

let g:loaded_unite_source_bookmark = 1

let &cpo = s:save_cpo
unlet s:save_cpo

" __END__
" vim: foldmethod=marker
plugin/unite/buffer.vim	[[[1
45
"=============================================================================
" FILE: buffer.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 06 Aug 2010
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

if exists('g:loaded_unite_source_buffer')
  finish
endif

let s:save_cpo = &cpo
set cpo&vim

augroup plugin-unite-source-buffer
  autocmd!
  autocmd BufEnter,BufWinEnter,BufFilePost * call unite#sources#buffer#_append()
augroup END

let g:loaded_unite_source_buffer = 1

let &cpo = s:save_cpo
unlet s:save_cpo

" __END__
" vim: foldmethod=marker
plugin/unite/directory_mru.vim	[[[1
45
"=============================================================================
" FILE: directory_mru.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 16 Oct 2010
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

if exists('g:loaded_unite_source_directory_mru')
  finish
endif

let s:save_cpo = &cpo
set cpo&vim

augroup plugin-unite-source-directory_mru
  autocmd!
  autocmd BufLeave,BufWinLeave,BufFilePost * call unite#sources#directory_mru#_append()
augroup END

let g:loaded_unite_source_directory_mru = 1

let &cpo = s:save_cpo
unlet s:save_cpo

" __END__
" vim: foldmethod=marker
plugin/unite/file_mru.vim	[[[1
45
"=============================================================================
" FILE: file_mru.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 05 Oct 2010
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

if exists('g:loaded_unite_source_file_mru')
  finish
endif

let s:save_cpo = &cpo
set cpo&vim

augroup plugin-unite-source-file_mru
  autocmd!
  autocmd BufEnter,BufWinEnter,BufFilePost * call unite#sources#file_mru#_append()
augroup END

let g:loaded_unite_source_file_mru = 1

let &cpo = s:save_cpo
unlet s:save_cpo

" __END__
" vim: foldmethod=marker
plugin/unite/tab.vim	[[[1
45
"=============================================================================
" FILE: tab.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 24 Oct 2010
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

if exists('g:loaded_unite_source_tab')
  finish
endif

let s:save_cpo = &cpo
set cpo&vim

augroup plugin-unite-source-tab
  autocmd!
  autocmd TabEnter * call unite#sources#tab#_append()
augroup END

let g:loaded_unite_source_tab = 1

let &cpo = s:save_cpo
unlet s:save_cpo

" __END__
" vim: foldmethod=marker
plugin/unite/window.vim	[[[1
45
"=============================================================================
" FILE: window.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 22 Apr 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

if exists('g:loaded_unite_source_window')
  finish
endif

let s:save_cpo = &cpo
set cpo&vim

augroup plugin-unite-source-window
  autocmd!
  autocmd WinEnter,BufWinEnter * call unite#sources#window#_append()
augroup END

let g:loaded_unite_source_window = 1

let &cpo = s:save_cpo
unlet s:save_cpo

" __END__
" vim: foldmethod=marker
plugin/unite.vim	[[[1
196
"=============================================================================
" FILE: unite.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 10 Aug 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
" Version: 2.2, for Vim 7.0
"=============================================================================

if exists('g:loaded_unite')
  finish
endif

let s:save_cpo = &cpo
set cpo&vim

" Global options definition."{{{
if !exists('g:unite_update_time')
  let g:unite_update_time = 200
endif
if !exists('g:unite_enable_start_insert')
  let g:unite_enable_start_insert = 0
endif
if !exists('g:unite_enable_ignore_case')
  let g:unite_enable_ignore_case = &ignorecase
endif
if !exists('g:unite_enable_smart_case')
  let g:unite_enable_smart_case = &infercase
endif
if !exists('g:unite_split_rule')
  let g:unite_split_rule = 'topleft'
endif
if !exists('g:unite_enable_split_vertically')
  let g:unite_enable_split_vertically = 0
endif
if !exists('g:unite_winheight')
  let g:unite_winheight = 20
endif
if !exists('g:unite_winwidth')
  let g:unite_winwidth = 90
endif
if !exists('g:unite_quick_match_table')
  let g:unite_quick_match_table = {
        \'a' : 1, 's' : 2, 'd' : 3, 'f' : 4, 'g' : 5, 'h' : 6, 'j' : 7, 'k' : 8, 'l' : 9, ';' : 10,
        \'q' : 11, 'w' : 12, 'e' : 13, 'r' : 14, 't' : 15, 'y' : 16, 'u' : 17, 'i' : 18, 'o' : 19, 'p' : 20,
        \'1' : 21, '2' : 22, '3' : 23, '4' : 24, '5' : 25, '6' : 26, '7' : 27, '8' : 28, '9' : 29, '0' : 30,
        \}
endif
if !exists('g:unite_cd_command')
  let g:unite_cd_command = 'cd'
endif
if !exists('g:unite_lcd_command')
  let g:unite_lcd_command = 'lcd'
endif
if !exists('g:unite_abbr_highlight')
  let g:unite_abbr_highlight = 'Normal'
endif
if !exists('g:unite_cursor_line_highlight')
  let g:unite_cursor_line_highlight = 'PmenuSel'
endif
if !exists('g:unite_data_directory')
  let g:unite_data_directory = expand('~/.unite')
endif
if !isdirectory(fnamemodify(g:unite_data_directory, ':p'))
  call mkdir(iconv(fnamemodify(g:unite_data_directory, ':p'), &encoding, &termencoding), 'p')
endif
"}}}

" Wrapper command.
command! -nargs=+ -complete=customlist,unite#complete_source Unite call s:call_unite_empty(<q-args>)
function! s:call_unite_empty(args)"{{{
  let [l:args, l:options] = s:parse_options(a:args)
  call unite#start(l:args, l:options)
endfunction"}}}

command! -nargs=+ -complete=customlist,unite#complete_source UniteWithCurrentDir call s:call_unite_current_dir(<q-args>)
function! s:call_unite_current_dir(args)"{{{
  let [l:args, l:options] = s:parse_options(a:args)
  if !has_key(l:options, 'input')
    let l:path = &filetype ==# 'vimfiler' ? b:vimfiler.current_dir : unite#substitute_path_separator(fnamemodify(getcwd(), ':p'))
    if l:path !~ '/$'
      let l:path .= '/'
    endif
    let l:options.input = escape(l:path, ' ')
  endif

  call unite#start(l:args, l:options)
endfunction"}}}

command! -nargs=+ -complete=customlist,unite#complete_source UniteWithBufferDir call s:call_unite_buffer_dir(<q-args>)
function! s:call_unite_buffer_dir(args)"{{{
  let [l:args, l:options] = s:parse_options(a:args)
  if !has_key(l:options, 'input')
    let l:path = &filetype ==# 'vimfiler' ? b:vimfiler.current_dir : unite#substitute_path_separator(fnamemodify(bufname('%'), ':p:h'))
    if l:path !~ '/$'
      let l:path .= '/'
    endif
    let l:options.input = escape(l:path, ' ')
  endif

  call unite#start(l:args, l:options)
endfunction"}}}

command! -nargs=+ -complete=customlist,unite#complete_source UniteWithCursorWord call s:call_unite_cursor_word(<q-args>)
function! s:call_unite_cursor_word(args)"{{{
  let [l:args, l:options] = s:parse_options(a:args)
  if !has_key(l:options, 'input')
    let l:options.input = expand('<cword>')
  endif

  call unite#start(l:args, l:options)
endfunction"}}}

command! -nargs=+ -complete=customlist,unite#complete_source UniteWithInput call s:call_unite_input(<q-args>)
function! s:call_unite_input(args)"{{{
  let [l:args, l:options] = s:parse_options(a:args)
  if !has_key(l:options, 'input')
    let l:options.input = escape(input('Input narrowing text: ', ''), ' ')
  endif

  call unite#start(l:args, l:options)
endfunction"}}}

command! -nargs=+ -complete=customlist,unite#complete_source UniteWithInputDirectory call s:call_unite_input_directory(<q-args>)
function! s:call_unite_input_directory(args)"{{{
  let [l:args, l:options] = s:parse_options(a:args)
  if !has_key(l:options, 'input')
    let l:path = unite#substitute_path_separator(input('Input narrowing directory: ', '', 'dir'))
    if isdirectory(l:path) && l:path !~ '/$'
      let l:path .= '/'
    endif
    let l:options.input = l:path
  endif

  call unite#start(l:args, l:options)
endfunction"}}}

function! s:parse_options(args)"{{{
  let l:args = []
  let l:options = {}
  for l:arg in split(a:args, '\%(\\\@<!\s\)\+')
    let l:arg = substitute(l:arg, '\\\( \)', '\1', 'g')

    let l:found = 0
    for l:option in unite#get_options()
      if stridx(l:arg, l:option) == 0
        let l:key = substitute(substitute(l:option, '-', '_', 'g'), '=$', '', '')[1:]
        let l:options[l:key] = (l:option =~ '=$') ?
              \ l:arg[len(l:option) :] : 1

        let l:found = 1
        break
      endif
    endfor

    if !l:found
      " Add source name.
      let l:source_name = matchstr(l:arg, '^[^:]*')
      let l:source_arg = l:arg[len(l:source_name)+1 :]
      let l:source_args = l:source_arg  == '' ? [] :
            \  map(split(l:source_arg, '\\\@<!:', 1),
            \      'substitute(v:val, ''\\\(.\)'', "\\1", "g")')
      call add(l:args, insert(l:source_args, l:source_name))
    endif
  endfor

  return [l:args, l:options]
endfunction"}}}

command! -nargs=? -complete=customlist,unite#complete_buffer UniteResume call unite#resume(<q-args>)

let g:loaded_unite = 1

let &cpo = s:save_cpo
unlet s:save_cpo

" __END__
" vim: foldmethod=marker
syntax/unite.vim	[[[1
95
"=============================================================================
" FILE: syntax/unite.vim
" AUTHOR: Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 06 Aug 2011.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

if version < 700
  syntax clear
elseif exists('b:current_syntax')
  finish
endif

let s:save_cpo = &cpo
set cpo&vim

syntax match uniteStatusLine /\%1l.*/
      \  contains=uniteSourcePrompt,uniteSeparator,uniteSourceNames,uniteSourceArgs
syntax match uniteSourcePrompt /^Sources/ contained nextgroup=uniteSourceSeparator
syntax match uniteSeparator /:/ contained nextgroup=uniteSourceNames
syntax match uniteSourceNames / [[:alnum:]_\/-]\+/ contained nextgroup=uniteSourceArgs
syntax match uniteMessage /^\[.*\].*$/  contains=uniteMessageSource
syntax match uniteMessageSource /^\[.*\]/ contained
syntax match uniteSourceArgs /:\S\+/ contained

syntax match uniteInputLine /\%2l.*/
      \ contains=uniteInputPrompt,uniteInputPromptError,uniteInputSpecial

syntax match uniteQuickMatchLine /^.:.*/
      \ contains=uniteQuickMatchTrigger,uniteCandidateSourceName,uniteCandidateAbbr
syntax match uniteMarkedLine /^\*.*/
syntax match uniteNonMarkedLine /^- .*/
      \ contains=uniteCandidateMarker,uniteCandidateSourceName,uniteCandidateAbbr
syntax match uniteCandidateMarker /^- / contained
syntax match uniteQuickMatchTrigger /^.:/ contained

syntax region   uniteError   start=+!!!+ end=+!!!+ contains=uniteErrorHidden oneline
if has('conceal')
  " Supported conceal features.
  syntax match   uniteErrorHidden            '!!!' contained conceal
else
  syntax match   uniteErrorHidden            '!!!' contained
endif

highlight default link uniteSourcePrompt  Statement
highlight default link uniteSeparator  NONE
highlight default link uniteSourceNames  Type
highlight default link uniteSourceArgs  Function
highlight default link uniteMessage Comment
highlight default link uniteMessageSource Constant

highlight default link uniteQuickMatchTrigger  Special
highlight default link uniteMarkedLine  Statement
highlight default link uniteCandidateSourceName  Type
highlight default link uniteCandidateMarker  Special

" The following definitions are for <Plug>(unite-choose-action).
highlight default link uniteChooseAction  NONE
highlight default link uniteChooseCandidate  NONE
highlight default link uniteChooseKey  SpecialKey
highlight default link uniteChooseMessage  NONE
highlight default link uniteChoosePrompt  uniteSourcePrompt
highlight default link uniteChooseSource  uniteSourceNames

highlight default link uniteInputPrompt  Identifier
highlight default link uniteInputPromptError  Error
highlight default link uniteInputSpecial  Special

highlight default link uniteError Error
highlight default link uniteErrorHidden Ignore

let b:current_syntax = 'unite'

let &cpo = s:save_cpo
unlet s:save_cpo

