" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
autoload/unite/kinds/buffer.vim	[[[1
103
"=============================================================================
" FILE: buffer.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 13 Nov 2010
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
"}}}

" Misc
function! s:delete(delete_command, candidate)"{{{
  execute a:candidate.action__buffer_nr a:delete_command
endfunction"}}}

" vim: foldmethod=marker
autoload/unite/kinds/cdable.vim	[[[1
115
"=============================================================================
" FILE: cdable.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 08 Dec 2010
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

function! unite#kinds#cdable#define()"{{{
  return s:kind
endfunction"}}}

let s:kind = {
      \ 'name' : 'cdable',
      \ 'action_table' : {},
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
  let l:word = a:candidate.action__directory . (a:candidate.action__directory =~ '[\\/]$' ? '' : '/')
  if l:word =~ '^/\|\a\+:[/\\]'
    let l:word = unite#util#substitute_path_separator(fnamemodify(l:word, ':.'))
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
"}}}

" vim: foldmethod=marker
autoload/unite/kinds/command.vim	[[[1
46
"=============================================================================
" FILE: command.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 09 Nov 2010
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

function! unite#kinds#command#define()"{{{
  return s:kind
endfunction"}}}

let s:kind = {
      \ 'name' : 'command',
      \ 'default_action' : 'execute',
      \ 'action_table': {},
      \}

" Actions"{{{
let s:kind.action_table.execute = {
      \ 'description' : 'execute command',
      \ }
function! s:kind.action_table.execute.func(candidate)"{{{
  execute a:candidate.action__command
endfunction"}}}
"}}}

" vim: foldmethod=marker
autoload/unite/kinds/common.vim	[[[1
93
"=============================================================================
" FILE: common.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 24 Nov 2010
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
      \ }
function! s:kind.action_table.yank.func(candidate)"{{{
  let @" = a:candidate.word
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
  let [l:old_col, l:old_max_col] = [col('.'), col('$')]

  " Paste.
  let l:old_reg = @"
  let @" = a:candidate.word
  normal! ""p
  let @" = l:old_reg

  if unite#get_context().is_insert
    PP! [l:old_col+len(a:candidate.word), l:old_max_col]
    if l:old_col+1 >= l:old_max_col
      startinsert!
    else
      let l:pos = getpos('.')
      let l:pos[2] += len(a:candidate.word)
      call setpos('.', l:pos)
    endif
  endif
endfunction"}}}
"}}}

" vim: foldmethod=marker
autoload/unite/kinds/directory.vim	[[[1
41
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

" vim: foldmethod=marker
autoload/unite/kinds/file.vim	[[[1
128
"=============================================================================
" FILE: file.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 17 Nov 2010
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

function! unite#kinds#file#define()"{{{
  return s:kind
endfunction"}}}

let s:kind = {
      \ 'name' : 'file',
      \ 'default_action' : 'open',
      \ 'action_table' : {},
      \ 'parents' : ['openable', 'cdable'],
      \}

" Actions"{{{
let s:kind.action_table.open = {
      \ 'description' : 'open files',
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.open.func(candidates)"{{{
  for l:candidate in a:candidates
    call unite#util#smart_execute_command('edit', l:candidate.action__path)
  endfor
endfunction"}}}

let s:kind.action_table.preview = {
      \ 'description' : 'preview file or buffer',
      \ 'is_quit' : 0,
      \ }
function! s:kind.action_table.preview.func(candidate)"{{{
  call unite#util#smart_execute_command('pedit', l:candidate.action__path)
endfunction"}}}

let s:kind.action_table.tabopen = {
      \ 'description' : 'tabopen files or buffers',
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.tabopen.func(candidates)"{{{
  for l:candidate in a:candidates
    call unite#util#smart_execute_command('tabedit', l:candidate.action__path)
  endfor
endfunction"}}}

let s:kind.action_table.split = {
      \ 'description' : 'horizontal split open files or buffers',
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.split.func(candidates)"{{{
  for l:candidate in a:candidates
    call unite#util#smart_execute_command('split', l:candidate.action__path)
  endfor
endfunction"}}}

let s:kind.action_table.vsplit = {
      \ 'description' : 'vertical split open files or buffers',
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.vsplit.func(candidates)"{{{
  for l:candidate in a:candidates
    call unite#util#smart_execute_command('vsplit', l:candidate.action__path)
  endfor
endfunction"}}}

let s:kind.action_table.left = {
      \ 'description' : 'vertical left split files or buffers',
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.left.func(candidates)"{{{
  for l:candidate in a:candidates
    call unite#util#smart_execute_command('leftabove vsplit', l:candidate.action__path)
  endfor
endfunction"}}}

let s:kind.action_table.right = {
      \ 'description' : 'vertical right split open files or buffers',
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.right.func(candidates)"{{{
  for l:candidate in a:candidates
    call unite#util#smart_execute_command('rightbelow vsplit', l:candidate.action__path)
  endfor
endfunction"}}}

let s:kind.action_table.above = {
      \ 'description' : 'horizontal above split open files or buffers',
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.above.func(candidates)"{{{
  for l:candidate in a:candidates
    call unite#util#smart_execute_command('leftabove split', l:candidate.action__path)
  endfor
endfunction"}}}

let s:kind.action_table.below = {
      \ 'description' : 'horizontal below split open files or buffers',
      \ 'is_selectable' : 1,
      \ }
function! s:kind.action_table.below.func(candidates)"{{{
  for l:candidate in a:candidates
    call unite#util#smart_execute_command('rightbelow split', l:candidate.action__path)
  endfor
endfunction"}}}
"}}}

" vim: foldmethod=marker
autoload/unite/kinds/jump_list.vim	[[[1
145
"=============================================================================
" FILE: jump_list.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 22 Nov 2010
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
      edit `=l:candidate.action__path`
    endif
    call s:jump(l:candidate)

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
  pedit +call\ s:jump(a:candidate) `=a:candidate.action__path`
endfunction"}}}
"}}}

" Misc.
function! s:jump(candidate)"{{{
  if !has_key(a:candidate, 'action__line') && !has_key(a:candidate, 'action__pattern')
    " Move to head.
    0
    return
  endif

  if !has_key(a:candidate, 'action__pattern')
    " Jump to the line number.
    execute a:candidate.action__line
    return
  endif

  " Jump by search().
  let l:source = unite#available_sources(a:candidate.source)
  if !(has_key(a:candidate, 'action__signature') && has_key(l:source, 'calc_signature'))
    " Not found signature.
    if getline(a:candidate.action__line) =~# a:candidate.action__pattern
      execute a:candidate.action__line
    else
      call search(a:candidate.action__pattern, 'w')
    endif
    return
  endif

  call search(a:candidate.action__pattern, 'w')

  let l:lnum_prev = line('.')
  call search(a:candidate.action__pattern, 'w')
  let l:lnum = line('.')
  if l:lnum != l:lnum_prev
    " Detected same pattern lines!!
    let l:start_lnum = l:lnum
    while l:source.calc_signature(l:lnum) !=# a:candidate.action__signature
      call search(a:candidate.action__pattern, 'w')
      let l:lnum = line('.')
      if l:lnum == l:start_lnum
        " Not found.
        call unite#print_error("unite: jump_list: Target position is not found.")
        0
        return
      endif
    endwhile
  endif
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

" vim: foldmethod=marker
autoload/unite/kinds/openable.vim	[[[1
116
"=============================================================================
" FILE: openable.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 01 Nov 2010
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
"}}}


" vim: foldmethod=marker
autoload/unite/kinds/tab.vim	[[[1
84
"=============================================================================
" FILE: tab.vim
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

function! unite#kinds#tab#define()"{{{
  return s:kind
endfunction"}}}

let s:kind = {
      \ 'name' : 'tab',
      \ 'default_action' : 'open',
      \ 'action_table': {},
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
      if l:title != ''
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

" vim: foldmethod=marker
autoload/unite/kinds/window.vim	[[[1
70
"=============================================================================
" FILE: window.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 27 Dec 2010.
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

" vim: foldmethod=marker
autoload/unite/kinds/word.vim	[[[1
40
"=============================================================================
" FILE: word.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 18 Nov 2010
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

" vim: foldmethod=marker
autoload/unite/mappings.vim	[[[1
453
"=============================================================================
" FILE: mappings.vim
" AUTHOR: Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 14 Dec 2010
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

" Define default mappings.
function! unite#mappings#define_default_mappings()"{{{
  " Plugin keymappings"{{{
  nnoremap <silent><buffer> <Plug>(unite_exit)  :<C-u>call <SID>exit()<CR>
  nnoremap <silent><buffer> <Plug>(unite_choose_action)  :<C-u>call <SID>choose_action()<CR>
  nnoremap <silent><buffer> <Plug>(unite_insert_enter)  :<C-u>call <SID>insert_enter()<CR>
  nnoremap <silent><buffer> <Plug>(unite_insert_head)  :<C-u>call <SID>insert_head()<CR>
  nnoremap <silent><buffer> <Plug>(unite_append_enter)  :<C-u>call <SID>append_enter()<CR>
  nnoremap <silent><buffer> <Plug>(unite_append_end)  :<C-u>call <SID>append_end()<CR>
  nnoremap <silent><buffer> <Plug>(unite_toggle_mark_current_candidate)  :<C-u>call <SID>toggle_mark()<CR>
  nnoremap <silent><buffer> <Plug>(unite_redraw)  :<C-u>call <SID>redraw()<CR>
  nnoremap <silent><buffer> <Plug>(unite_rotate_next_source)  :<C-u>call <SID>rotate_source(1)<CR>
  nnoremap <silent><buffer> <Plug>(unite_rotate_previous_source)  :<C-u>call <SID>rotate_source(0)<CR>
  nnoremap <silent><buffer> <Plug>(unite_print_candidate)  :<C-u>call <SID>print_candidate()<CR>
  nnoremap <buffer><expr> <Plug>(unite_cursor_top)  b:unite.prompt_linenr.'G0z.'
  nnoremap <buffer><expr> <Plug>(unite_loop_cursor_down)  (line('.') == line('$'))? b:unite.prompt_linenr.'G0z.' : 'j'
  nnoremap <buffer><expr> <Plug>(unite_loop_cursor_up)  (line('.') <= b:unite.prompt_linenr)? 'G' : 'k'
  nnoremap <silent><buffer> <Plug>(unite_quick_match_default_action)  :<C-u>call <SID>quick_match()<CR>
  nnoremap <silent><buffer> <Plug>(unite_input_directory)   :<C-u>call <SID>input_directory()<CR>
  nnoremap <silent><buffer><expr> <Plug>(unite_do_default_action)   unite#do_action(b:unite.context.default_action)

  vnoremap <buffer><silent> <Plug>(unite_toggle_mark_selected_candidates)  :<C-u>call <SID>toggle_mark_candidates(getpos("'<")[1], getpos("'>")[1])<CR>

  inoremap <silent><buffer> <Plug>(unite_exit)  <ESC>:<C-u>call <SID>exit()<CR>
  inoremap <buffer><expr> <Plug>(unite_insert_leave)  unite#smart_map("\<ESC>j0", "\<ESC>0")
  inoremap <silent><expr><buffer> <Plug>(unite_delete_backward_char)  col('.') <= (len(b:unite.prompt)+1) ? "\<C-o>:\<C-u>call \<SID>exit()\<Cr>" : "\<C-h>"
  inoremap <expr><buffer> <Plug>(unite_delete_backward_line)  repeat("\<C-h>", col('.')-(len(b:unite.prompt)+1))
  inoremap <expr><buffer> <Plug>(unite_delete_backward_word)  col('.') <= (len(b:unite.prompt)+1) ? '' : "\<C-w>"
  inoremap <expr><buffer> <Plug>(unite_delete_backward_path)  col('.') <= (len(b:unite.prompt)+1) ? '' : <SID>delete_backward_path()
  inoremap <expr><buffer> <Plug>(unite_select_next_line)  pumvisible() ? "\<C-n>" : line('.') == line('$') ? "\<C-Home>\<End>".repeat("\<Down>", b:unite.prompt_linenr-1)
        \ : line('.') == b:unite.prompt_linenr ? "\<Home>\<Down>\<Down>" : "\<Home>\<Down>"
  inoremap <expr><buffer> <Plug>(unite_select_previous_line)  pumvisible() ? "\<C-p>" : line('.') == b:unite.prompt_linenr ? "\<C-End>\<Home>"
        \ : line('.') == (b:unite.prompt_linenr+2) ? "\<End>\<Up>\<Up>" : "\<Home>\<Up>"
  inoremap <expr><buffer> <Plug>(unite_select_next_page)  pumvisible() ? "\<PageDown>" : repeat("\<Down>", winheight(0))
  inoremap <expr><buffer> <Plug>(unite_select_previous_page)  pumvisible() ? "\<PageUp>" : repeat("\<Up>", winheight(0))
  inoremap <silent><buffer> <Plug>(unite_toggle_mark_current_candidate)  <C-o>:<C-u>call <SID>toggle_mark()<CR>
  inoremap <silent><buffer> <Plug>(unite_choose_action)  <C-o>:<C-u>call <SID>choose_action()<CR>
  inoremap <silent><buffer> <Plug>(unite_move_head)  <C-o>:<C-u>call <SID>insert_head()<CR>
  inoremap <silent><buffer> <Plug>(unite_quick_match_default_action)  <C-o>:<C-u>call <SID>quick_match()<CR>
  inoremap <silent><buffer> <Plug>(unite_input_directory)   <C-o>:<C-u>call <SID>input_directory()<CR>
  inoremap <silent><buffer><expr> <Plug>(unite_do_default_action)   unite#do_action(b:unite.context.default_action)
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

  nnoremap <silent><buffer><expr> d   unite#smart_map('d', unite#do_action('delete'))
  nnoremap <silent><buffer><expr> b   unite#smart_map('b', unite#do_action('bookmark'))
  nnoremap <silent><buffer><expr> e   unite#smart_map('e', unite#do_action('narrow'))
  nnoremap <silent><buffer><expr> l   unite#smart_map('l', unite#do_action(b:unite.context.default_action))
  nnoremap <silent><buffer><expr> p   unite#smart_map('p', unite#do_action('preview'))
  nmap <silent><buffer><expr> x       unite#smart_map('x', "\<Plug>(unite_quick_match_default_action)")

  " Visual mode key-mappings.
  xmap <buffer> <Space>   <Plug>(unite_toggle_mark_selected_candidates)

  " Insert mode key-mappings.
  imap <buffer> <ESC>     <Plug>(unite_insert_leave)
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
  inoremap <silent><buffer><expr> /         unite#smart_map('/', unite#do_action('narrow'))
  imap <silent><buffer><expr> <Space>       unite#smart_map(' ', "\<Plug>(unite_toggle_mark_current_candidate)")
  imap <silent><buffer><expr> x             unite#smart_map('x', "\<Plug>(unite_quick_match_default_action)")
endfunction"}}}

function! unite#mappings#narrowing(word)"{{{
  setlocal modifiable
  let b:unite.input = escape(a:word, ' *')
  call setline(b:unite.prompt_linenr, b:unite.prompt . b:unite.input)
  execute b:unite.prompt_linenr
  startinsert!
endfunction"}}}
function! unite#mappings#do_action(action_name, ...)"{{{
  let l:candidates = unite#get_marked_candidates()

  if empty(l:candidates)
    let l:num = a:0 > 0 ? a:1 : line('.') <= b:unite.prompt_linenr ? 0 : line('.') - (b:unite.prompt_linenr+1)

    if line('$')-(b:unite.prompt_linenr+1) < l:num
      " Ignore.
      return
    endif

    let l:candidates = [ unite#get_unite_candidates()[l:num] ]
  endif

  " Check action.
  let l:action_tables = []
  let Self = unite#get_self_functions()[-1]
  for l:candidate in l:candidates
    let l:action_table = unite#get_action_table(l:candidate.source, l:candidate.kind, Self)

    let l:action_name =
          \ a:action_name ==# 'default' ?
          \ unite#get_default_action(l:candidate.source, l:candidate.kind)
          \ : a:action_name

    if !has_key(l:action_table, l:action_name)
      call unite#util#print_error(l:candidate.abbr . '(' . l:candidate.source . ')')
      call unite#util#print_error('No such action : ' . l:action_name)
      return
    endif

    let l:action = l:action_table[l:action_name]

    " Check selectable flag.
    if !l:action.is_selectable && len(l:candidates) > 1
      call unite#util#print_error(l:candidate.abbr . '(' . l:candidate.source . ')')
      call unite#util#print_error('Not selectable action : ' . l:action_name)
      return
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

  " Execute action.
  let l:is_redraw = 0
  for l:table in l:action_tables
    " Check quit flag.
    if l:table.action.is_quit
      call unite#quit_session()
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
  endif
endfunction"}}}

" key-mappings functions.
function! s:exit()"{{{
  call unite#force_quit_session()
endfunction"}}}
function! s:delete_backward_path()"{{{
  let l:input = getline(b:unite.prompt_linenr)[len(b:unite.prompt):]
  return repeat("\<C-h>", len(matchstr(l:input, '[^/]*.$')))
endfunction"}}}
function! s:toggle_mark()"{{{
  if line('.') <= b:unite.prompt_linenr
    " Ignore.
    return
  endif

  let l:candidate = unite#get_unite_candidates()[line('.') - (b:unite.prompt_linenr+1)]
  let l:candidate.unite__is_marked = !l:candidate.unite__is_marked
  let l:candidate.unite__marked_time = localtime()
  call unite#redraw_line()

  normal! j
endfunction"}}}
function! s:toggle_mark_candidates(start, end)"{{{
  if a:start <= b:unite.prompt_linenr
    " Ignore.
    return
  endif

  let l:cnt = a:start
  while l:cnt <= a:end
    let l:candidate = unite#get_unite_candidates()[l:cnt - (b:unite.prompt_linenr+1)]
    let l:candidate.unite__is_marked = !l:candidate.unite__is_marked
    let l:candidate.unite__marked_time = localtime()

    call unite#redraw_line(l:cnt)

    let l:cnt += 1
  endwhile
endfunction"}}}
function! s:choose_action()"{{{
  if line('$') < (b:unite.prompt_linenr+1)
    " Ignore.
    return
  endif

  let l:candidates = unite#get_marked_candidates()
  if empty(l:candidates)
    let l:num = line('.') <= b:unite.prompt_linenr ? 0 : line('.') - (b:unite.prompt_linenr+1)

    let l:candidates = [ unite#get_unite_candidates()[l:num] ]
  endif

  echohl Statement | echo 'Candidates:' | echohl None

  let Self = unite#get_self_functions()[-1]
  let s:actions = unite#get_action_table(l:candidates[0].source, l:candidates[0].kind, Self)
  if len(l:candidates) > 1
    for l:candidate in l:candidates
      let l:action_table = unite#get_action_table(l:candidate.source, l:candidate.kind, Self)
      " Filtering unique items and check selectable flag.
      call filter(s:actions, 'has_key(l:action_table, v:key)
            \ && l:action_table[v:key].is_selectable')
    endfor
  endif

  if empty(s:actions)
    call unite#util#print_error('No actions.')
    return
  endif

  " Print candidates.
  for l:candidate in l:candidates
    " Print candidates.
    echo l:candidate.abbr . '('
    echohl Type | echon l:candidate.source | echohl None
    echon ')'
  endfor

  " Print action names.
  let l:max = max(map(keys(s:actions), 'len(v:val)'))
  for [l:action_name, l:action] in items(s:actions)
    echohl Identifier
    echo unite#util#truncate(l:action_name, l:max)
    if l:action.description != ''
      echohl Special | echon ' -- '
      echohl Comment
      echon l:action.description
    endif
  endfor
  echohl None

  " Choose action.
  let l:input = ''
  while 1
    let l:input = input('What action? ', l:input, 'customlist,unite#mappings#complete_actions')

    if l:input == ''
      " Cancel.
      return
    endif

    " Check action candidates.
    let l:actions = filter(keys(s:actions), printf('stridx(v:val, %s) == 0', string(l:input)))
    if empty(l:actions)
      echohl Error | echo 'Invalid action.' | echohl None
    elseif len(l:actions) > 1
      if has_key(s:actions, l:input)
        let l:selected_action = l:input
        break
      endif

      echohl Error | echo 'Too match action.' | echohl None
    else
      let l:selected_action = l:actions[0]
      break
    endif

    echo ''
  endwhile

  " Execute action.
  call unite#mappings#do_action(l:selected_action)
endfunction"}}}
function! s:insert_enter()"{{{
  if line('.') != b:unite.prompt_linenr
    execute b:unite.prompt_linenr
    startinsert!
  elseif col('.') <= len(b:unite.prompt)+1
    startinsert
    let l:pos = getpos('.')
    let l:pos[2] = len(b:unite.prompt)+1
    call setpos('.', l:pos)
  else
    startinsert
  endif
endfunction"}}}
function! s:insert_head()"{{{
  let l:pos = getpos('.')
  let l:pos[2] = len(b:unite.prompt)+1
  call setpos('.', l:pos)
  call s:insert_enter()
endfunction"}}}
function! s:append_enter()"{{{
  call s:insert_enter()
  if col('.')+1 == col('$')
    startinsert!
  elseif col('$') != len(b:unite.prompt)+1
    normal! l
  endif
endfunction"}}}
function! s:append_end()"{{{
  call s:insert_enter()
  startinsert!
endfunction"}}}
function! s:redraw()"{{{
  let b:unite.context.is_redraw = 1
  call unite#force_redraw()
  let b:unite.context.is_redraw = 0
endfunction"}}}
function! s:rotate_source(is_next)"{{{
  let l:max = len(unite#loaded_sources_list()) - 1
  for l:source in unite#loaded_sources_list()
    let l:source.unite__number = a:is_next ?
          \ (l:source.unite__number - 1) : (l:source.unite__number + 1)
    if l:source.unite__number < 0
      let l:source.unite__number = l:max
    elseif l:source.unite__number > l:max
      let l:source.unite__number = 0
    endif
  endfor

  call unite#redraw_status()
  call unite#redraw_candidates()
endfunction"}}}
function! s:print_candidate()"{{{
  if line('.') <= b:unite.prompt_linenr
    " Ignore.
    return
  endif

  let l:candidate = unite#get_unite_candidates()[line('.') - (b:unite.prompt_linenr+1)]
  echo l:candidate.word
endfunction"}}}
function! s:insert_selected_candidate()"{{{
  if line('.') <= b:unite.prompt_linenr
    " Ignore.
    return
  endif

  let l:candidate = unite#get_unite_candidates()[line('.') - (b:unite.prompt_linenr+1)]
  call unite#mappings#narrowing(l:candidate.word)
endfunction"}}}
function! s:quick_match()"{{{
  if line('$') < (b:unite.prompt_linenr+1)
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

  call unite#force_redraw()

  if has_key(g:unite_quick_match_table, l:char)
        \ && g:unite_quick_match_table[l:char] < len(b:unite.candidates)
    call unite#mappings#do_action(b:unite.context.default_action,
          \ g:unite_quick_match_table[l:char])
  else
    call unite#util#print_error('Invalid quick match key.')
  endif
endfunction"}}}
function! s:input_directory()"{{{
  let l:path = unite#substitute_path_separator(input('Input narrowing directory: ', unite#get_input(), 'dir'))
  let l:path = l:path.(l:path == '' || l:path =~ '/$' ? '' : '/')
  call unite#mappings#narrowing(l:path)
endfunction"}}}

function! unite#mappings#complete_actions(arglead, cmdline, cursorpos)"{{{
  return filter(keys(s:actions), printf('stridx(v:val, %s) == 0', string(a:arglead)))
endfunction"}}}

" vim: foldmethod=marker
autoload/unite/sources/bookmark.vim	[[[1
184
"=============================================================================
" FILE: bookmark.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 22 Nov 2010
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
        \ "source" : "bookmark",
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


" vim: foldmethod=marker
autoload/unite/sources/buffer.vim	[[[1
166
"=============================================================================
" FILE: buffer.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 19 Dec 2010.
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
      \}

function! s:source_buffer_all.gather_candidates(args, context)"{{{
  let l:list = s:get_buffer_list()

  let l:candidates = map(l:list, '{
        \ "word" : s:make_abbr(v:val.action__buffer_nr),
        \ "kind" : "buffer",
        \ "source" : "buffer",
        \ "action__path" : unite#substitute_path_separator(bufname(v:val.action__buffer_nr)),
        \ "action__buffer_nr" : v:val.action__buffer_nr,
        \ "action__directory" : s:get_directory(v:val.action__buffer_nr),
        \}')

  return l:candidates
endfunction"}}}

let s:source_buffer_tab = {
      \ 'name' : 'buffer_tab',
      \ 'description' : 'candidates from buffer list in current tab',
      \}

function! s:source_buffer_tab.gather_candidates(args, context)"{{{
  if !exists('t:unite_buffer_dictionary')
    let t:unite_buffer_dictionary = {}
  endif

  let l:list = filter(s:get_buffer_list(), 'has_key(t:unite_buffer_dictionary, v:val.action__buffer_nr)')

  let l:candidates = map(l:list, '{
        \ "word" : s:make_abbr(v:val.action__buffer_nr),
        \ "kind" : "buffer",
        \ "source" : "buffer_tab",
        \ "action__path" : unite#substitute_path_separator(bufname(v:val.action__buffer_nr)),
        \ "action__buffer_nr" : v:val.action__buffer_nr,
        \ "action__directory" : s:get_directory(v:val.action__buffer_nr),
        \}')

  return l:candidates
endfunction"}}}

" Misc
function! s:make_abbr(bufnr)"{{{
  let l:filetype = getbufvar(a:bufnr, '&filetype')
  if l:filetype ==# 'vimfiler'
    let l:path = getbufvar(a:bufnr, 'vimfiler').current_dir
    let l:path = '*vimfiler* - ' . unite#substitute_path_separator(simplify(l:path))
  elseif l:filetype ==# 'vimshell'
    let l:path = getbufvar(a:bufnr, 'vimshell').save_dir
    let l:path = '*vimshell* - ' . unite#substitute_path_separator(simplify(l:path))
  else
    let l:path = bufname(a:bufnr) . (getbufvar(a:bufnr, '&modified') ? '[+]' : '')
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
    if buflisted(l:bufnr) && l:bufnr != bufnr('#')
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

  if buflisted(bufnr('#'))
    " Add current buffer.
    if has_key(s:buffer_list, bufnr('#'))
      call add(l:list, s:buffer_list[bufnr('#')])
    else
      call add(l:list,
            \ { 'action__buffer_nr' : bufnr('#'), 'source__time' : 0 })
    endif
  endif

  return l:list
endfunction"}}}

" vim: foldmethod=marker
autoload/unite/sources/directory_mru.vim	[[[1
163
"=============================================================================
" FILE: directory_mru.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 19 Dec 2010.
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
" The version of MRU file format.
let s:VERSION = '0.2.0'

" [[full_path, localtime()], ... ]
let s:mru_dirs = []

let s:mru_file_mtime = 0  " the last modified time of the mru file.

call unite#util#set_default('g:unite_source_directory_mru_time_format', '(%c)')
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

  let l:path = unite#util#substitute_path_separator(simplify(l:path))
  " Chomp last /.
  let l:path = substitute(l:path, '/$', '', '')

  " Append the current buffer to the mru list.
  if !isdirectory(path) || &l:buftype =~ 'help'
  \   || (g:unite_source_directory_mru_ignore_pattern != ''
  \      && l:path =~# g:unite_source_directory_mru_ignore_pattern)
    return
  endif

  call s:load()
  call insert(filter(s:mru_dirs, 'v:val.action__path !=# l:path'),
  \           s:convert2dictionary([l:path, localtime()]))

  if g:unite_source_directory_mru_limit > 0
    unlet s:mru_dirs[g:unite_source_directory_mru_limit]
  endif

  call s:save()
endfunction"}}}

let s:source = {
      \ 'name' : 'directory_mru',
      \ 'description' : 'candidates from directory MRU list',
      \ 'max_candidates' : 30,
      \ 'action_table' : {},
      \}

function! s:source.gather_candidates(args, context)"{{{
  call s:load()

  for l:mru in s:mru_dirs
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

    let s:mru_dirs =
    \   map(s:mru_dirs[: g:unite_source_directory_mru_limit - 1],
    \              's:convert2dictionary(split(v:val, "\t"))')
    call filter(s:mru_dirs, 'isdirectory(v:val.action__path)')

    let s:mru_file_mtime = getftime(g:unite_source_directory_mru_file)
  endif
endfunction"}}}
function! s:convert2dictionary(list)  "{{{
  return {
        \ 'word' : unite#util#substitute_path_separator(a:list[0]),
        \ 'source' : 'directory_mru',
        \ 'kind' : 'directory',
        \ 'source__time' : a:list[1],
        \ 'action__path' : unite#util#substitute_path_separator(a:list[0]),
        \ 'action__directory' : unite#util#substitute_path_separator(a:list[0]),
        \   }
endfunction"}}}
function! s:convert2list(dict)  "{{{
  return [ a:dict.action__path, a:dict.source__time ]
endfunction"}}}

" vim: foldmethod=marker
autoload/unite/sources/file.vim	[[[1
101
"=============================================================================
" FILE: file.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 06 Dec 2010
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
call unite#util#set_default('g:unite_source_file_ignore_pattern', 
      \'\%(^\|/\)\.$\|\~$\|\.\%(o|exe|dll|bak|sw[po]\)$')
"}}}

function! unite#sources#file#define()"{{{
  return s:source
endfunction"}}}

let s:source = {
      \ 'name' : 'file',
      \ 'description' : 'candidates from file list',
      \ 'is_volatile' : 1,
      \}

function! s:source.gather_candidates(args, context)"{{{
  let l:input = substitute(substitute(a:context.input, '\\ ', ' ', 'g'), '^\a\+:\zs\*/', '/', '')

  " Substitute *. -> .* .
  let l:input = substitute(l:input, '\*\.', '.*', 'g')

  if l:input !~ '\*' && unite#is_win() && getftype(l:input) == 'link'
    " Resolve link.
    let l:input = resolve(l:input)
  endif

  " Glob by directory name.
  let l:input = substitute(l:input, '[^/.]*$', '', '')
  let l:candidates = split(unite#substitute_path_separator(glob(l:input . (l:input =~ '\*$' ? '' : '*'))), '\n')

  if a:context.input != ''
    let l:dummy = substitute(a:context.input, '[*\\]', '', 'g')
    if (!filereadable(l:dummy) && !isdirectory(l:dummy) && isdirectory(fnamemodify(l:dummy, ':h')))
          \ || l:dummy =~ '^\%(/\|\a\+:/\)$'
      " Add dummy candidate.
      call add(l:candidates, l:dummy)
    endif
  endif

  if g:unite_source_file_ignore_pattern != ''
    call filter(l:candidates, 'v:val !~ ' . string(g:unite_source_file_ignore_pattern))
  endif

  let l:candidates_dir = []
  let l:candidates_file = []
  for l:file in l:candidates
    let l:dict = {
          \ 'word' : l:file, 'abbr' : l:file, 'source' : 'file', 'action__path' : l:file,
          \ 'action__directory' : unite#path2directory(l:file),
          \}

    if isdirectory(l:file)
      if l:file !~ '^\%(/\|\a\+:/\)$'
        let l:dict.abbr .= '/'
      endif

      let l:dict.kind = 'directory'

      call add(l:candidates_dir, l:dict)
    else
      if !filereadable(l:file)
        " Dummy.
        let l:dict.abbr = '[new file]' . l:file
      endif

      let l:dict.kind = 'file'

      call add(l:candidates_file, l:dict)
    endif
  endfor

  return l:candidates_dir + l:candidates_file
endfunction"}}}

" vim: foldmethod=marker
autoload/unite/sources/file_mru.vim	[[[1
163
"=============================================================================
" FILE: file_mru.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 08 Jan 2011.
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
" The version of MRU file format.
let s:VERSION = '0.2.0'

" [[full_path, localtime()], ... ]
let s:mru_files = []

let s:mru_file_mtime = 0  " the last modified time of the mru file.

call unite#util#set_default('g:unite_source_file_mru_time_format', '(%c)')
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
  let l:filetype = getbufvar(bufnr('%'), '&filetype')
  if l:filetype ==# 'vimfiler'
    let l:path = getbufvar(bufnr('%'), 'vimfiler').current_dir
  elseif l:filetype ==# 'vimshell'
    let l:path = getbufvar(bufnr('%'), 'vimshell').save_dir
  else
    let l:path = expand('%:p')
  endif
  let l:path = unite#util#substitute_path_separator(simplify(l:path))

  " Append the current buffer to the mru list.
  if !s:is_exists_path(path) || &l:buftype =~ 'help'
  \   || (g:unite_source_file_mru_ignore_pattern != ''
  \      && l:path =~# g:unite_source_file_mru_ignore_pattern)
    return
  endif

  call s:load()
  call insert(filter(s:mru_files, 'v:val.action__path !=# l:path'),
  \           s:convert2dictionary([l:path, localtime()]))

  if g:unite_source_file_mru_limit > 0
    unlet s:mru_files[g:unite_source_file_mru_limit]
  endif

  call s:save()
endfunction"}}}

let s:source = {
      \ 'name' : 'file_mru',
      \ 'description' : 'candidates from file MRU list',
      \ 'max_candidates' : 30,
      \ 'action_table' : {},
      \}

function! s:source.gather_candidates(args, context)"{{{
  call s:load()

  " Create abbr.
  for l:mru in s:mru_files
    let l:path = (g:unite_source_file_mru_filename_format == '') ? '' :
          \ unite#util#substitute_path_separator(fnamemodify(l:mru.action__path, g:unite_source_file_mru_filename_format))
    if l:path == ''
      let l:path = l:mru.action__path
    endif

    let l:mru.abbr = strftime(g:unite_source_file_mru_time_format, l:mru.source__time) . l:path
  endfor

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

    let s:mru_files =
    \   map(s:mru_files[: g:unite_source_file_mru_limit - 1],
    \              's:convert2dictionary(split(v:val, "\t"))')
    call filter(s:mru_files, 's:is_exists_path(v:val.action__path)')

    let s:mru_file_mtime = getftime(g:unite_source_file_mru_file)
  endif
endfunction"}}}
function! s:is_exists_path(path)  "{{{
  return isdirectory(a:path) || filereadable(a:path)
endfunction"}}}
function! s:convert2dictionary(list)  "{{{
  let l:path = unite#util#substitute_path_separator(a:list[0])
  return {
        \ 'word' : l:path,
        \ 'source' : 'file_mru',
        \ 'kind' : (isdirectory(l:path) ? 'directory' : 'file'),
        \ 'source__time' : a:list[1],
        \ 'action__path' : l:path,
        \ 'action__directory' : unite#util#path2directory(l:path),
        \   }
endfunction"}}}
function! s:convert2list(dict)  "{{{
  return [ a:dict.action__path, a:dict.source__time ]
endfunction"}}}

" vim: foldmethod=marker
autoload/unite/sources/file_rec.vim	[[[1
108
"=============================================================================
" FILE: file_rec.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 24 Nov 2010
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
if !exists('g:unite_source_file_rec_max_depth')
  let g:unite_source_file_rec_max_depth = 10
endif
"}}}

function! unite#sources#file_rec#define()"{{{
  return s:source
endfunction"}}}

let s:source = {
      \ 'name' : 'file_rec',
      \ 'description' : 'candidates from directory by recursive',
      \ 'max_candidates' : 30,
      \ }

function! s:source.gather_candidates(args, context)"{{{
  if !empty(a:args)
    let l:directory = unite#substitute_path_separator(a:args[0])
  elseif isdirectory(a:context.input)
    let l:directory = a:context.input
  else
    let l:directory = unite#substitute_path_separator(getcwd())
  endif

  if l:directory =~ '/$'
    let l:directory = l:directory[: -2]
  endif

  let s:start_time = has('reltime') ? reltime() : 0
  let l:candidates = s:get_files(1, l:directory, [])

  if g:unite_source_file_ignore_pattern != ''
    call filter(l:candidates, 'v:val !~ ' . string(g:unite_source_file_ignore_pattern))
  endif

  " Convert to relative path.
  call map(l:candidates, 'fnamemodify(v:val, ":.")')

  return map(l:candidates, '{
        \ "word" : v:val,
        \ "source" : "file_rec",
        \ "kind" : "file",
        \ "action__path" : unite#util#substitute_path_separator(fnamemodify(v:val, ":p")),
        \ "action__directory" : unite#util#path2directory(fnamemodify(v:val, ":p")),
        \ }')
endfunction"}}}

" Add custom action table."{{{
let s:cdable_action_rec = {
      \ 'description' : 'open this directory by file_rec',
      \}

function! s:cdable_action_rec.func(candidate)
  call unite#start([['file_rec', a:candidate.action__directory]])
endfunction

call unite#custom_action('cdable', 'rec', s:cdable_action_rec)
unlet! s:cdable_action_rec
"}}}

function! s:get_files(depth, directory, files)"{{{
  if a:depth > g:unite_source_file_rec_max_depth
        \ || (has('reltime') && str2nr(split(reltimestr(reltime(s:start_time)))[0]) >= 2)
    return []
  endif

  let l:directory_files = split(unite#substitute_path_separator(glob(a:directory . '/*')), '\n')
  let l:files = a:files
  for l:file in l:directory_files
    if isdirectory(l:file)
      " Get files in a directory.
      let l:files += s:get_files(a:depth + 1, l:file, [])
    else
      call add(l:files, l:file)
    endif
  endfor

  return l:files
endfunction"}}}

" vim: foldmethod=marker
autoload/unite/sources/register.vim	[[[1
54
"=============================================================================
" FILE: register.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 18 Nov 2010
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

function! unite#sources#register#define()"{{{
  return s:source
endfunction"}}}

let s:source = {
      \ 'name' : 'register',
      \ 'description' : 'candidates from register',
      \}

function! s:source.gather_candidates(args, context)"{{{
  let l:candidates = []

  let l:max_width = winwidth(0) - 40
  for l:reg in ['"', '*', '+', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

    let l:register = eval('@' . l:reg)
    call add(l:candidates, {
          \ 'word' : l:register,
          \ 'abbr' : printf('register%s - %s', l:reg, l:register[: l:max_width]),
          \ 'source' : 'register',
          \ 'kind' : 'word',
          \ })
  endfor

  return l:candidates
endfunction"}}}

" vim: foldmethod=marker
autoload/unite/sources/source.vim	[[[1
65
"=============================================================================
" FILE: source.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 16 Dec 2010.
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

function! unite#sources#source#define()"{{{
  return s:source
endfunction"}}}

let s:source = {
      \ 'name' : 'source',
      \ 'description' : 'candidates from sources list',
      \ 'action_table' : {},
      \ 'default_action' : { 'common' : 'start' },
      \}

function! s:source.gather_candidates(args, context)"{{{
  return map(sort(values(unite#available_sources()), 's:compare_sources'), '{
        \ "word" : v:val.name,
        \ "abbr" : unite#util#truncate(v:val.name, 25) . (v:val.description != "" ? " -- " . v:val.description : ""),
        \ "source" : "source",
        \ "action__source_name" : v:val.name,
        \}')
endfunction"}}}

" Actions"{{{
let s:action_table = {}

let s:action_table.start = {
      \ 'description' : 'start source',
      \ 'is_selectable' : 1,
      \ }
function! s:action_table.start.func(candidates)"{{{
  call unite#start(map(copy(a:candidates), 'v:val.action__source_name'), unite#get_context())
endfunction"}}}

let s:source.action_table['*'] = s:action_table
"}}}

function! s:compare_sources(source_a, source_b) "{{{
  return a:source_a.name > a:source_b.name
endfunction"}}}

" vim: foldmethod=marker
autoload/unite/sources/tab.vim	[[[1
118
"=============================================================================
" FILE: tab.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 14 Dec 2010.
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
  " Add current tab.
  call add(l:list, tabpagenr())

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
          \ 'source' : 'tab',
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

" vim: foldmethod=marker
autoload/unite/sources/window.vim	[[[1
99
"=============================================================================
" FILE: window.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 27 Dec 2010.
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

  if winnr() != 0
    unlet l:list[winnr()-1]
  endif
  call sort(l:list, 's:compare')
  if winnr() != 0
    " Add previous window.
    call add(l:list, winnr())
  endif

  let s:candidates = []
  for i in l:list
    let l:window = getwinvar(i, 'unite_window')
    let l:bufname = bufname(winbufnr(i))
    if empty(l:bufname)
      let l:bufname = '[No Name]'
    endif

    call add(s:candidates, {
          \ 'word' : l:bufname,
          \ 'abbr' : printf('[%d/%d] %s %s(%s)', i, winnr('$'),
          \      (i == winnr() ? '%' : i == winnr('#') ? '#' : ' '),
          \      l:bufname, l:window.cwd),
          \ 'kind' : 'window',
          \ 'source' : 'window',
          \ 'action__window_nr' : i,
          \ 'action__directory' : l:window.cwd,
          \ })
  endfor
endfunction"}}}
function! s:source.gather_candidates(args, context)"{{{
  return s:candidates
endfunction"}}}

" Misc
function! s:compare(candidate_a, candidate_b)"{{{
  return getwinvar(a:candidate_b, 'unite_window').time - getwinvar(a:candidate_a, 'unite_window').time
endfunction"}}}

" vim: foldmethod=marker
autoload/unite/util.vim	[[[1
246
"=============================================================================
" FILE: util.vim
" AUTHOR: Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 08 Jan 2011.
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

" Original function is from mattn.
" http://github.com/mattn/googlereader-vim/tree/master

function! unite#util#truncate_smart(str, max, footer_width, separator)"{{{
  let width = unite#util#wcswidth(a:str)
  if width <= a:max
    let ret = a:str
  else
    let header_width = a:max - unite#util#wcswidth(a:separator) - a:footer_width
    let ret = unite#util#strwidthpart(a:str, header_width) . a:separator
          \ . unite#util#strwidthpart_reverse(a:str, a:footer_width)
  endif
   
  return unite#util#truncate(ret, a:max)
endfunction"}}}

function! unite#util#truncate(str, width)"{{{
  let ret = a:str
  let width = unite#util#wcswidth(a:str)
  if width > a:width
    let ret = unite#util#strwidthpart(ret, a:width)
    let width = unite#util#wcswidth(ret)
  endif

  if width < a:width
    let ret .= repeat(' ', a:width - width)
  endif

  return ret
endfunction"}}}

function! unite#util#strchars(str)"{{{
  return len(substitute(a:str, '.', 'x', 'g'))
endfunction"}}}

  function! unite#util#strwidthpart(str, width)"{{{
    let ret = a:str
    let width = unite#util#wcswidth(a:str)
    while width > a:width
      let char = matchstr(ret, '.$')
      let ret = ret[: -1 - len(char)]
      let width -= s:wcwidth(char)
    endwhile

    return ret
  endfunction"}}}
  function! unite#util#strwidthpart_reverse(str, width)"{{{
    let ret = a:str
    let width = unite#util#wcswidth(a:str)
    while width > a:width
      let char = matchstr(ret, '^.')
      let ret = ret[len(char) :]
      let width -= s:wcwidth(char)
    endwhile

    return ret
  endfunction"}}}

if v:version >= 703
  " Use builtin function.
  function! unite#util#wcswidth(str)"{{{
    return strwidth(a:str)
  endfunction"}}}
  function! s:wcwidth(str)"{{{
    return strwidth(a:str)
  endfunction"}}}
else
  function! unite#util#wcswidth(str)"{{{
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
      let width += s:wcwidth(ucs)
      let str = substitute(str, mx_first, '', '')
    endwhile
    return width
  endfunction"}}}

  " UTF-8 only.
  function! s:wcwidth(ucs)"{{{
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

function! unite#util#is_win()"{{{
  return has('win16') || has('win32') || has('win64')
endfunction"}}}

function! unite#util#print_error(message)"{{{
  echohl WarningMsg | echomsg a:message | echohl None
endfunction"}}}

function! unite#util#smart_execute_command(action, word)
  execute a:action . ' ' . (a:word == '' ? '' : '`=a:word`')
endfunction

function! unite#util#escape_file_searching(buffer_name)"{{{
  return escape(a:buffer_name, '*[]?{},')
endfunction"}}}

function! unite#util#set_default(var, val)  "{{{
  if !exists(a:var) || type({a:var}) != type(a:val)
    let {a:var} = a:val
  endif
endfunction"}}}
function! unite#util#set_dictionary_helper(variable, keys, pattern)"{{{
  for key in split(a:keys, ',')
    if !has_key(a:variable, key)
      let a:variable[key] = a:pattern
    endif
  endfor
endfunction"}}}
function! unite#util#substitute_path_separator(path)"{{{
  return unite#util#is_win() ? substitute(a:path, '\\', '/', 'g') : a:path
endfunction"}}}
function! unite#util#path2directory(path)"{{{
  return unite#util#substitute_path_separator(isdirectory(a:path) ? a:path : fnamemodify(a:path, ':p:h'))
endfunction"}}}
function! unite#util#path2project_directory(path)"{{{
  let l:search_directory = unite#util#path2directory(a:path)
  let l:directory = ''

  " Search VCS directory.
  for d in ['.git', '.bzr', '.hg']
    let d = finddir(d, unite#util#escape_file_searching(l:search_directory) . ';')
    if d != ''
      let l:directory = fnamemodify(d, ':p:h:h')
      break
    endif
  endfor

  " Search project file.
  if l:directory == ''
    for d in ['build.xml', 'prj.el', '.project', 'pom.xml', 'Makefile', 'configure', 'Rakefile', 'NAnt.build', 'tags', 'gtags']
      let d = findfile(d, unite#util#escape_file_searching(l:search_directory) . ';')
      if d != ''
        let l:directory = fnamemodify(d, ':p:h')
        break
      endif
    endfor
  endif

  if l:directory == ''
    " Search /src/ directory.
    let l:base = unite#substitute_path_separator(l:search_directory)
    if l:base =~# '/src/'
      let l:directory = l:base[: strridx(l:base, '/src/') + 3]
    endif
  endif

  if l:directory == ''
    let l:directory = l:search_directory
  endif

  return unite#substitute_path_separator(l:directory)
endfunction"}}}

" Check vimproc."{{{
try
  let s:exists_vimproc_version = vimproc#version()
catch
  let s:exists_vimproc_version = 0
endtry
"}}}
function! unite#util#has_vimproc()"{{{
  return s:exists_vimproc_version
endfunction"}}}
function! unite#util#system(str, ...)"{{{
  let l:command = a:str
  let l:input = a:0 >= 1 ? a:1 : ''
  if &termencoding != '' && &termencoding != &encoding
    let l:command = iconv(l:command, &encoding, &termencoding)
    let l:input = iconv(l:input, &encoding, &termencoding)
  endif

  if a:0 == 0
    let l:output = unite#util#has_vimproc() ?
          \ vimproc#system(l:command) : system(l:command)
  else
    let l:output = unite#util#has_vimproc() ?
          \ vimproc#system(l:command, l:input) : system(l:command, l:input)
  endif

  if &termencoding != '' && &termencoding != &encoding
    let l:output = iconv(l:output, &termencoding, &encoding)
  endif

  return l:output
endfunction"}}}
function! unite#util#get_last_status()"{{{
  return unite#util#has_vimproc() ?
        \ vimproc#get_last_status() : v:shell_error
endfunction"}}}

" vim: foldmethod=marker
autoload/unite.vim	[[[1
1133
"=============================================================================
" FILE: unite.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 24 Nov 2010
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
" Version: 1.0, for Vim 7.0
"=============================================================================

" User functions."{{{
function! unite#get_substitute_pattern(buffer_name)"{{{
  return s:substitute_pattern[a:buffer_name]
endfunction"}}}
function! unite#set_substitute_pattern(buffer_name, pattern, subst, ...)"{{{
  let l:priority = a:0 > 0 ? a:1 : 0
  let l:buffer_name = (a:buffer_name == '' ? 'default' : a:buffer_name)

  for key in split(l:buffer_name, ',')
    if !has_key(s:substitute_pattern, key)
      let s:substitute_pattern[key] = {}
    endif

    if has_key(s:substitute_pattern[key], a:pattern)
          \ && a:pattern == ''
      call remove(s:substitute_pattern[key], a:pattern)
    else
      let s:substitute_pattern[key][a:pattern] = {
            \ 'pattern' : a:pattern,
            \ 'subst' : a:subst, 'priority' : l:priority
            \ }
    endif
  endfor
endfunction"}}}
function! unite#custom_alias(kind, name, action)"{{{
  for key in split(a:kind, ',')
    if !has_key(s:custom_aliases, key)
      let s:custom_aliases[key] = {}
    endif

    let s:custom_aliases[key][a:name] = a:action
  endfor
endfunction"}}}
function! unite#custom_default_action(kind, default_action)"{{{
  for key in split(a:kind, ',')
    let s:custom_default_actions[key] = a:default_action
  endfor
endfunction"}}}
function! unite#custom_action(kind, name, action)"{{{
  for key in split(a:kind, ',')
    if !has_key(s:custom_actions, key)
      let s:custom_actions[key] = {}
    endif
    let s:custom_actions[key][a:name] = a:action
  endfor
endfunction"}}}
function! unite#undef_custom_action(kind, name)"{{{
  for key in split(a:kind, ',')
    if has_key(s:custom_actions, key)
      call remove(s:custom_actions, key)
    endif
  endfor
endfunction"}}}

function! unite#define_source(source)"{{{
  if type(a:source) == type([])
    for l:source in a:source
      let s:custom_sources[l:source.name] = l:source
    endfor
  else
    let s:custom_sources[a:source.name] = a:source
  endif
endfunction"}}}
function! unite#define_kind(kind)"{{{
  if type(a:kind) == type([])
    for l:kind in a:kind
      let s:custom_kinds[l:kind.name] = l:kind
    endfor
  else
    let s:custom_kinds[a:kind.name] = a:kind
  endif
endfunction"}}}
function! unite#undef_source(name)"{{{
  if has_key(s:custom_sources, a:name)
    call remove(s:custom_sources, a:name)
  endif
endfunction"}}}
function! unite#undef_kind(name)"{{{
  if has_key(s:custom_kind, a:name)
    call remove(s:custom_kind, a:name)
  endif
endfunction"}}}

function! unite#do_action(action)
  return printf("%s:\<C-u>call unite#mappings#do_action(%s)\<CR>",
        \             (mode() ==# 'i' ? "\<ESC>" : ''), string(a:action))
endfunction
function! unite#smart_map(narrow_map, select_map)"{{{
  return (line('.') <= b:unite.prompt_linenr && empty(unite#get_marked_candidates())) ? a:narrow_map : a:select_map
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
let s:unite = {}

let s:default_sources = {}
let s:default_kinds = {}

let s:custom_sources = {}
let s:custom_kinds = {}

let s:custom_actions = {}
let s:custom_default_actions = {}
let s:custom_aliases = {}

let s:substitute_pattern = {}
call unite#set_substitute_pattern('files', '^\~', substitute(substitute($HOME, '\\', '/', 'g'), ' ', '\\\\ ', 'g'), -100)
call unite#set_substitute_pattern('files', '[^~.*]\zs/', '*/*', 100)

let s:unite_options = [
      \ '-buffer-name=', '-input=', '-prompt=',
      \ '-default-action=', '-start-insert', '-no-quit',
      \ '-winwidth=', '-winheight=',
      \]
"}}}

" Core functions."{{{
function! unite#available_kinds(...)"{{{
  let l:unite = s:get_unite()
  return a:0 == 0 ? l:unite.kinds : get(l:unite.kinds, a:1, {})
endfunction"}}}
function! unite#available_sources(...)"{{{
  let l:all_sources = s:initialize_sources()
  return a:0 == 0 ? l:all_sources : get(l:all_sources, a:1, {})
endfunction"}}}
"}}}

" Helper functions."{{{
function! unite#is_win()"{{{
  return unite#util#is_win()
endfunction"}}}
function! unite#loaded_source_names()"{{{
  return map(unite#loaded_sources_list(), 'v:val.name')
endfunction"}}}
function! unite#loaded_sources_list()"{{{
  return sort(values(s:get_loaded_sources()), 's:compare_sources')
endfunction"}}}
function! unite#get_unite_candidates()"{{{
  return s:get_unite().candidates
endfunction"}}}
function! unite#get_context()"{{{
  return s:get_unite().context
endfunction"}}}
" function! unite#get_action_table(source_name, kind_name, self_func, [is_parent_action])
function! unite#get_action_table(source_name, kind_name, self_func, ...)"{{{
  let l:kind = unite#available_kinds(a:kind_name)
  let l:source = s:get_loaded_sources(a:source_name)
  let l:is_parents_action = a:0 > 0 ? a:1 : 0

  let l:action_table = {}

  let l:source_kind = 'source/'.a:source_name.'/'.a:kind_name
  let l:source_kind_wild = 'source/'.a:source_name.'/*'

  if !l:is_parents_action
    " Source/kind custom actions.
    if has_key(s:custom_actions, l:source_kind)
      let l:action_table = s:extend_actions(a:self_func, l:action_table,
            \ s:custom_actions[l:source_kind])
    endif

    " Source/kind actions.
    if has_key(l:source.action_table, a:kind_name)
      let l:action_table = s:extend_actions(a:self_func, l:action_table,
            \ l:source.action_table[a:kind_name])
    endif

    " Source/* custom actions.
    if has_key(s:custom_actions, l:source_kind_wild)
      let l:action_table = s:extend_actions(a:self_func, l:action_table,
            \ s:custom_actions[l:source_kind_wild])
    endif

    " Source/* actions.
    if has_key(l:source.action_table, '*')
      let l:action_table = s:extend_actions(a:self_func, l:action_table,
            \ l:source.action_table['*'])
    endif

    " Kind custom actions.
    if has_key(s:custom_actions, a:kind_name)
      let l:action_table = s:extend_actions(a:self_func, l:action_table,
            \ s:custom_actions[a:kind_name])
    endif

    " Kind actions.
    let l:action_table = s:extend_actions(a:self_func, l:action_table,
          \ l:kind.action_table)
  endif

  " Parents actions.
  for l:parent in l:kind.parents
    let l:action_table = s:extend_actions(a:self_func, l:action_table,
          \ unite#get_action_table(a:source_name, l:parent, a:self_func))
  endfor

  if !l:is_parents_action
    " Kind aliases.
    call s:filter_alias_action(l:action_table, l:kind.alias_table)

    " Kind custom aliases.
    if has_key(s:custom_aliases, a:kind_name)
      call s:filter_alias_action(l:action_table, s:custom_aliases[a:kind_name])
    endif

    " Source/* aliases.
    if has_key(l:source.alias_table, '*')
      call s:filter_alias_action(l:action_table, l:source.alias_table['*'])
    endif

    " Source/* custom aliases.
    if has_key(s:custom_aliases, l:source_kind_wild)
      call s:filter_alias_action(l:action_table, s:custom_aliases[l:source_kind_wild])
    endif

    " Source/kind aliases.
    if has_key(s:custom_aliases, l:source_kind)
      call s:filter_alias_action(l:action_table, s:custom_aliases[l:source_kind])
    endif

    " Source/kind custom aliases.
    if has_key(l:source.alias_table, a:kind_name)
      call s:filter_alias_action(l:action_table, l:source.alias_table[a:kind_name])
    endif
  endif

  " Set default parameters.
  for l:action in values(l:action_table)
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
function! unite#get_default_action(source_name, kind_name)"{{{
  let l:source = s:get_loaded_sources(a:source_name)

  if has_key(s:custom_default_actions, a:source_name.'/'.a:kind_name)
    " Source/kind custom actions.
    return s:custom_default_actions[a:source_name.'/'.a:kind_name]
  elseif has_key(l:source.default_action, a:kind_name)
    " Source custom default actions.
    return l:source.default_action[a:kind_name]
  elseif has_key(s:custom_default_actions, a:kind_name)
    " Kind custom default actions.
    return s:custom_default_actions[a:kind_name]
  else
    " Kind default actions.
    return unite#available_kinds(a:kind_name).default_action
  endif
endfunction"}}}
function! unite#escape_match(str)"{{{
  return substitute(substitute(escape(a:str, '~"\.^$[]'), '\*\@<!\*', '[^/]*', 'g'), '\*\*\+', '.*', 'g')
endfunction"}}}
function! unite#complete_source(arglead, cmdline, cursorpos)"{{{
  if empty(s:default_sources)
    " Initialize load.
    call s:load_default_sources_and_kinds()
  endif

  let l:sources = extend(copy(s:default_sources), s:custom_sources)
  return filter(keys(l:sources)+s:unite_options, 'stridx(v:val, a:arglead) == 0')
endfunction"}}}
function! unite#complete_buffer(arglead, cmdline, cursorpos)"{{{
  let l:buffer_list = map(filter(range(1, bufnr('$')), 'getbufvar(v:val, "&filetype") ==# "unite"'), 'getbufvar(v:val, "unite").buffer_name')

  return filter(l:buffer_list, printf('stridx(v:val, %s) == 0', string(a:arglead)))
endfunction"}}}
function! unite#invalidate_cache(source_name)  "{{{
  let l:unite = s:get_unite()

  if has_key(l:unite.sources, a:source_name)
    let l:unite.sources[a:source_name].unite__is_invalidate = 1
  endif
endfunction"}}}
function! unite#force_redraw() "{{{
  call s:redraw(1)
endfunction"}}}
function! unite#redraw() "{{{
  call s:redraw(0)
endfunction"}}}
function! unite#redraw_line(...) "{{{
  let l:linenr = a:0 > 0 ? a:1 : line('.')
  if l:linenr <= b:unite.prompt_linenr || &filetype !=# 'unite'
    " Ignore.
    return
  endif

  let l:modifiable_save = &l:modifiable
  setlocal modifiable

  let l:candidate = unite#get_unite_candidates()[l:linenr - (b:unite.prompt_linenr+1)]
  call setline(l:linenr, s:convert_line(l:candidate))

  let &l:modifiable = l:modifiable_save
endfunction"}}}
function! unite#quick_match_redraw() "{{{
  let l:modifiable_save = &l:modifiable
  setlocal modifiable

  call setline(b:unite.prompt_linenr+1, s:convert_quick_match_lines(b:unite.candidates))
  redraw

  let &l:modifiable = l:modifiable_save
endfunction"}}}
function! unite#redraw_status() "{{{
  let l:modifiable_save = &l:modifiable
  setlocal modifiable

  call setline(s:LNUM_STATUS, 'Sources: ' . join(map(copy(unite#loaded_sources_list()), 'v:val.name'), ', '))

  let &l:modifiable = l:modifiable_save
endfunction"}}}
function! unite#redraw_candidates() "{{{
  let l:candidates = unite#gather_candidates()

  let l:modifiable_save = &l:modifiable
  setlocal modifiable

  let l:lines = s:convert_lines(l:candidates)
  if len(l:lines) < len(b:unite.candidates)
    if mode() !=# 'i' && line('.') == b:unite.prompt_linenr
      silent! execute (b:unite.prompt_linenr+1).',$delete _'
      startinsert!
    else
      let l:pos = getpos('.')
      silent! execute (b:unite.prompt_linenr+1).',$delete _'
      call setpos('.', l:pos)
    endif
  endif
  call setline(b:unite.prompt_linenr+1, l:lines)

  let &l:modifiable = l:modifiable_save
  let b:unite.candidates = l:candidates
endfunction"}}}
function! unite#get_marked_candidates() "{{{
  return sort(filter(copy(unite#get_unite_candidates()), 'v:val.unite__is_marked'), 's:compare_marked_candidates')
endfunction"}}}
function! unite#keyword_filter(list, input)"{{{
  for l:input in split(a:input, '\\\@<! ')
    let l:input = substitute(l:input, '\\ ', ' ', 'g')

    if l:input =~ '^!'
      " Exclusion.
      let l:input = unite#escape_match(l:input)
      call filter(a:list, 'v:val.word !~ ' . string(l:input[1:]))
    elseif l:input =~ '\\\@<!\*'
      " Wildcard.
      let l:input = unite#escape_match(l:input)
      call filter(a:list, 'v:val.word =~ ' . string(l:input))
    else
      let l:input = substitute(l:input, '\\\(.\)', '\1', 'g')
      if &ignorecase
        let l:expr = printf('stridx(tolower(v:val.word), %s) != -1', string(tolower(l:input)))
      else
        let l:expr = printf('stridx(v:val.word, %s) != -1', string(l:input))
      endif

      call filter(a:list, l:expr)
    endif
  endfor

  return a:list
endfunction"}}}
function! unite#get_input()"{{{
  " Prompt check.
  if stridx(getline(b:unite.prompt_linenr), b:unite.prompt) != 0
    " Restore prompt.
    call setline(b:unite.prompt_linenr, b:unite.prompt . getline(b:unite.prompt_linenr))
  endif

  return getline(b:unite.prompt_linenr)[len(b:unite.prompt):]
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
    let l:candidates += b:unite.sources_candidates[l:source.name]
  endfor

  return l:candidates
endfunction"}}}

" Utils.
function! unite#print_error(message)"{{{
  echohl WarningMsg | echomsg a:message | echohl None
endfunction"}}}
function! unite#substitute_path_separator(path)"{{{
  return unite#util#substitute_path_separator(a:path)
endfunction"}}}
function! unite#path2directory(path)"{{{
  return unite#util#path2directory(a:path)
endfunction"}}}
"}}}

" Command functions.
function! unite#start(sources, ...)"{{{
  if empty(s:default_sources)
    " Initialize load.
    call s:load_default_sources_and_kinds()
  endif

  " Save context.
  let l:context = a:0 >= 1 ? a:1 : {}
  if !has_key(l:context, 'input')
    let l:context.input = ''
  endif
  if !has_key(l:context, 'start_insert')
    let l:context.start_insert = 0
  endif
  if !has_key(l:context, 'is_insert')
    let l:context.is_insert = 0
  endif
  if !has_key(l:context, 'no_quit')
    let l:context.no_quit = 0
  endif
  if !has_key(l:context, 'buffer_name')
    let l:context.buffer_name = ''
  endif
  if !has_key(l:context, 'prompt')
    let l:context.prompt = '>'
  endif
  if !has_key(l:context, 'default_action')
    let l:context.default_action = 'default'
  endif
  if !has_key(l:context, 'winwidth')
    let l:context.winwidth = g:unite_winwidth
  endif
  if !has_key(l:context, 'winheight')
    let l:context.winheight = g:unite_winheight
  endif
  let l:context.is_redraw = 0

  try
    call s:initialize_unite_buffer(a:sources, l:context)
  catch /^Invalid source/
    return
  endtry

  setlocal modifiable

  silent % delete _
  call unite#redraw_status()
  call setline(b:unite.prompt_linenr, b:unite.prompt . b:unite.context.input)

  call unite#force_redraw()

  if g:unite_enable_start_insert
        \ || b:unite.context.start_insert || b:unite.context.is_insert
    execute b:unite.prompt_linenr
    normal! 0z.
    startinsert!
  else
    execute (b:unite.prompt_linenr+1)
    normal! 0z.
  endif

  setlocal nomodifiable
endfunction"}}}
function! unite#resume(buffer_name)"{{{
  if a:buffer_name == ''
    " Use last unite buffer.
    if !bufexists(s:last_unite_bufnr)
      call unite#util#print_error('No unite buffer.')
      return
    endif

    let l:bufnr = s:last_unite_bufnr
  else
    let l:buffer_dict = {}
    for l:unite in map(filter(range(1, bufnr('$')), 'getbufvar(v:val, "&filetype") ==# "unite"'), 'getbufvar(v:val, "unite")')
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

  call s:switch_unite_buffer(bufname(l:bufnr), getbufvar(l:bufnr, 'unite').context)

  " Set parameters.
  let b:unite.winnr = l:winnr
  let b:unite.win_rest_cmd = l:win_rest_cmd
  let b:unite.redrawtime_save = &redrawtime
  let b:unite.hlsearch_save = &hlsearch
  let b:unite.search_pattern_save = @/

  let s:unite = b:unite

  setlocal modifiable

  if g:unite_enable_start_insert
        \ || b:unite.context.start_insert || b:unite.context.is_insert
    execute b:unite.prompt_linenr
    normal! 0z.
    startinsert!
  else
    execute (b:unite.prompt_linenr+1)
    normal! 0z.
  endif

  setlocal nomodifiable
endfunction"}}}

function! unite#force_quit_session()  "{{{
  call s:quit_session(1)
endfunction"}}}
function! unite#quit_session()  "{{{
  call s:quit_session(0)
endfunction"}}}
function! s:quit_session(is_force)  "{{{
  if &filetype !=# 'unite'
    return
  endif

  " Save unite value.
  let s:unite = b:unite

  " Highlight off.
  let @/ = s:unite.search_pattern_save

  " Restore options.
  if exists('&redrawtime')
    let &redrawtime = s:unite.redrawtime_save
  endif
  let &hlsearch = s:unite.hlsearch_save

  nohlsearch

  " Close preview window.
  pclose

  " Call finalize functions.
  for l:source in unite#loaded_sources_list()
    if has_key(l:source.hooks, 'on_close')
      call l:source.hooks.on_close(l:source.args, s:unite.context)
    endif
  endfor

  if winnr('$') != 1
    if !a:is_force && s:unite.context.no_quit
      if winnr('#') > 0
        wincmd p
      endif
    else
      close
      execute s:unite.winnr . 'wincmd w'

      if winnr('$') != 1
        execute s:unite.win_rest_cmd
      endif
    endif
  endif

  if !s:unite.context.is_insert
    stopinsert
    redraw!
  endif
endfunction"}}}

function! s:load_default_sources_and_kinds()"{{{
  " Gathering all sources and kind name.
  let s:default_sources = {}
  let s:default_kinds = {}

  for l:name in map(split(globpath(&runtimepath, 'autoload/unite/sources/*.vim'), '\n'),
        \ 'fnamemodify(v:val, ":t:r")')

    if type({'unite#sources#' . l:name . '#define'}()) == type([])
      for l:source in {'unite#sources#' . l:name . '#define'}()
        if !has_key(s:default_sources, l:source.name)
          let s:default_sources[l:source.name] = l:source
        endif
      endfor
    else
      let l:source = {'unite#sources#' . l:name . '#define'}()

      if !has_key(s:default_sources, l:source.name)
        let s:default_sources[l:source.name] = l:source
      endif
    endif
  endfor

  for l:name in map(split(globpath(&runtimepath, 'autoload/unite/kinds/*.vim'), '\n'),
        \ 'fnamemodify(v:val, ":t:r")')

    if type({'unite#kinds#' . l:name . '#define'}()) == type([])
      for l:kind in {'unite#kinds#' . l:name . '#define'}()
        if !has_key(s:default_kinds, l:kind.name)
          let s:default_kinds[l:kind.name] = l:kind
        endif
      endfor
    else
      let l:kind = {'unite#kinds#' . l:name . '#define'}()

      if !has_key(s:default_kinds, l:kind.name)
        let s:default_kinds[l:kind.name] = l:kind
      endif
    endif
  endfor
endfunction"}}}
function! s:initialize_loaded_sources(sources)"{{{
  let l:all_sources = s:initialize_sources()
  let l:sources = {}

  let l:number = 0
  for [l:source_name, l:args] in map(a:sources, 'type(v:val) == type([]) ? [v:val[0], v:val[1:]] : [v:val, []]')
    if !has_key(l:all_sources, l:source_name)
      call unite#util#print_error('Invalid source name "' . l:source_name . '" is detected.')
      throw 'Invalid source'
    endif

    let l:source = l:all_sources[l:source_name]
    let l:source.args = l:args
    let l:source.unite__is_invalidate = 1

    let l:source.unite__number = l:number
    let l:number += 1

    let l:sources[l:source_name] = l:source
  endfor

  return l:sources
endfunction"}}}
function! s:initialize_sources()"{{{
  let l:all_sources = extend(copy(s:default_sources), s:custom_sources)

  for l:source in values(l:all_sources)
    if !has_key(l:source, 'is_volatile')
      let l:source.is_volatile = 0
    endif
    if !has_key(l:source, 'max_candidates')
      let l:source.max_candidates = 0
    endif
    if !has_key(l:source, 'required_pattern_length')
      let l:source.required_pattern_length = 0
    endif
    if !has_key(l:source, 'action_table')
      let l:source.action_table = {}
    endif
    if !has_key(l:source, 'default_action')
      let l:source.default_action = {}
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
  endfor

  return l:all_sources
endfunction"}}}
function! s:initialize_kinds()"{{{
  let l:kinds = extend(copy(s:default_kinds), s:custom_kinds)
  for l:kind in values(l:kinds)
    if !has_key(l:kind, 'alias_table')
      let l:kind.alias_table = {}
    endif
    if !has_key(l:kind, 'parents')
      let l:kind.parents = ['common']
    endif
  endfor

  return l:kinds
endfunction"}}}
function! s:recache_candidates(input, context)"{{{
  let l:context = a:context
  let l:input_list = filter(split(a:input, '\\\@<! ', 1), 'v:val !~ "!"')
  let l:context.input = empty(l:input_list) ? '' : l:input_list[0]
  let l:input_len = unite#util#strchars(l:context.input)

  for l:source in unite#loaded_sources_list()
    " Check required pattern length.
    if l:input_len < l:source.required_pattern_length
      let b:unite.sources_candidates[l:source.name] = []
      continue
    endif

    if l:source.is_volatile || l:context.is_force || l:source.unite__is_invalidate
      let l:context.source = l:source
      let l:source_candidates = copy(l:source.gather_candidates(l:source.args, l:context))
      let l:source.unite__is_invalidate = 0

      if !l:source.is_volatile
        " Recaching.
        let b:unite.cached_candidates[l:source.name] = l:source_candidates
      endif
    else
      let l:source_candidates = copy(b:unite.cached_candidates[l:source.name])
    endif

    if a:input != ''
      call unite#keyword_filter(l:source_candidates, a:input)
    endif

    if l:source.max_candidates != 0
      " Filtering too many candidates.
      let l:source_candidates = l:source_candidates[: l:source.max_candidates - 1]
    endif

    for l:candidate in l:source_candidates
      if !has_key(l:candidate, 'abbr')
        let l:candidate.abbr = l:candidate.word
      endif
      if !has_key(l:candidate, 'kind')
        let l:candidate.kind = 'common'
      endif

      " Initialize.
      let l:candidate.unite__is_marked = 0
    endfor

    let b:unite.sources_candidates[l:source.name] = l:source_candidates
  endfor
endfunction"}}}
function! s:convert_quick_match_lines(candidates)"{{{
  let [l:max_width, l:max_source_name] = s:adjustments(winwidth(0), b:unite.max_source_name, 5)
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
          \ . unite#util#truncate(l:candidate.source, l:max_source_name)
          \ . unite#util#truncate_smart(l:candidate.abbr, l:max_width, l:max_width/3, '..'))
    let l:num += 1
  endfor

  return l:candidates
endfunction"}}}
function! s:convert_lines(candidates)"{{{
  let [l:max_width, l:max_source_name] = s:adjustments(winwidth(0), b:unite.max_source_name, 2)

  return map(copy(a:candidates),
        \ '(v:val.unite__is_marked ? "* " : "- ")
        \ . unite#util#truncate(v:val.source, l:max_source_name)
        \ . unite#util#truncate_smart(v:val.abbr, ' . l:max_width .  ', l:max_width/3, "..")')
endfunction"}}}
function! s:convert_line(candidate)"{{{
  let [l:max_width, l:max_source_name] = s:adjustments(winwidth(0), b:unite.max_source_name, 2)

  return (a:candidate.unite__is_marked ? '* ' : '- ')
        \ . unite#util#truncate(a:candidate.source, l:max_source_name)
        \ . unite#util#truncate_smart(a:candidate.abbr, l:max_width, l:max_width/3, '..')
endfunction"}}}

function! s:initialize_unite_buffer(sources, context)"{{{
  " Check sources.
  let l:sources = s:initialize_loaded_sources(a:sources)

  let l:context = a:context

  if getbufvar(bufnr('%'), '&filetype') ==# 'unite'
    if l:context.input == ''
          \ && b:unite.buffer_name ==# l:context.buffer_name
      " Get input text.
      let l:context.input = unite#get_input()
    endif

    " Quit unite buffer.
    call unite#quit_session()
  endif

  " The current buffer is initialized.
  let l:buffer_name = unite#is_win() ? '[unite]' : '*unite*'
  if l:context.buffer_name != ''
    let l:buffer_name .= ' - ' . l:context.buffer_name
  endif

  let l:winnr = winnr()
  let l:win_rest_cmd = winrestcmd()

  " Call initialize functions.
  for l:source in values(l:sources)
    if has_key(l:source.hooks, 'on_init')
      call l:source.hooks.on_init(l:source.args, l:context)
    endif
  endfor

  call s:switch_unite_buffer(l:buffer_name, a:context)

  " Set parameters.
  let b:unite = {}
  let b:unite.winnr = l:winnr
  let b:unite.win_rest_cmd = l:win_rest_cmd
  let b:unite.context = l:context
  let b:unite.candidates = []
  let b:unite.sources = l:sources
  let b:unite.kinds = s:initialize_kinds()
  let b:unite.buffer_name = (l:context.buffer_name == '') ? 'default' : l:context.buffer_name
  let b:unite.prompt = l:context.prompt
  let b:unite.input = l:context.input
  let b:unite.last_input = l:context.input
  let b:unite.bufnr = bufnr('%')
  let b:unite.hlsearch_save = &hlsearch
  let b:unite.search_pattern_save = @/
  let b:unite.prompt_linenr = 2
  let b:unite.max_source_name = max(map(copy(a:sources), 'len(v:val[0])')) + 2
  let b:unite.cached_candidates = {}
  let b:unite.sources_candidates = {}

  let s:unite = b:unite

  let s:last_unite_bufnr = bufnr('%')

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
  set hlsearch

  " Autocommands.
  augroup plugin-unite
    autocmd InsertEnter <buffer>  call s:on_insert_enter()
    autocmd InsertLeave <buffer>  call s:on_insert_leave()
    autocmd CursorHoldI <buffer>  call s:on_cursor_hold_i()
    autocmd CursorHold <buffer>  call s:on_cursor_hold()
    autocmd CursorMoved,CursorMovedI <buffer>  call s:on_cursor_moved()
  augroup END

  call unite#mappings#define_default_mappings()

  if exists(':NeoComplCacheLock')
    " Lock neocomplcache.
    NeoComplCacheLock
  endif

  if exists('&redrawtime')
    " Save redrawtime
    let b:unite.redrawtime_save = &redrawtime
    let &redrawtime = 100
  endif

  " User's initialization.
  setlocal nomodifiable
  setfiletype unite

  if exists('b:current_syntax') && b:current_syntax == 'unite'
    " Set highlight.
    let l:match_prompt = escape(b:unite.prompt, '\/*~.^$[]')
    syntax clear uniteInputPrompt
    execute 'syntax match uniteInputPrompt' '/^'.l:match_prompt.'/ contained'

    execute 'syntax match uniteCandidateAbbr' '/\%'.(b:unite.max_source_name+2).'c.*/ contained'
  endif
endfunction"}}}
function! s:switch_unite_buffer(buffer_name, context)"{{{
  " Search unite window.
  " Note: must escape file-pattern.
  if bufwinnr(unite#util#escape_file_searching(a:buffer_name)) > 0
    silent execute bufwinnr(unite#util#escape_file_searching(a:buffer_name)) 'wincmd w'
  else
    " Split window.
    execute g:unite_split_rule
          \ g:unite_enable_split_vertically ?
          \        (bufexists(a:buffer_name) ? 'vsplit' : 'vnew')
          \      : (bufexists(a:buffer_name) ? 'split' : 'new')
    if bufexists(a:buffer_name)
      " Search buffer name.
      let l:bufnr = 1
      let l:max = bufnr('$')
      while l:bufnr <= l:max
        if bufname(l:bufnr) ==# a:buffer_name
          silent execute l:bufnr 'buffer'
        endif

        let l:bufnr += 1
      endwhile
    else
      silent! file `=a:buffer_name`
    endif
  endif

  if g:unite_enable_split_vertically
    execute 'vertical resize' a:context.winwidth
  else
    execute 'resize' a:context.winheight
  endif
endfunction"}}}

function! s:redraw(is_force) "{{{
  if &filetype !=# 'unite'
    return
  endif

  let l:input = unite#get_input()
  if !a:is_force && l:input ==# b:unite.last_input
    return
  endif

  " Highlight off.
  let @/ = ''

  let b:unite.last_input = l:input

  " Save options.
  let l:ignorecase_save = &ignorecase

  if g:unite_enable_smart_case && l:input =~ '\u'
    let &ignorecase = 0
  else
    let &ignorecase = g:unite_enable_ignore_case
  endif

  if has_key(s:substitute_pattern, b:unite.buffer_name)
    if b:unite.input != '' && stridx(l:input, b:unite.input) == 0
      " Substitute after input.
      let l:input_save = l:input
      let l:subst = l:input_save[len(b:unite.input) :]
      let l:input = l:input_save[: len(b:unite.input)-1]
    else
      " Substitute all input.
      let l:subst = l:input
      let l:input = ''
    endif

    for l:pattern in sort(values(s:substitute_pattern[b:unite.buffer_name]), 's:compare_substitute_patterns')
      let l:subst = substitute(l:subst, l:pattern.pattern, l:pattern.subst, 'g')
    endfor

    let l:input .= l:subst
  endif

  let l:context = b:unite.context
  let l:context.is_force = a:is_force

  " Recaching.
  call s:recache_candidates(l:input, l:context)

  let &ignorecase = l:ignorecase_save

  " Redraw.
  call unite#redraw_candidates()
endfunction"}}}

" Autocmd events.
function! s:on_insert_enter()  "{{{
  if &updatetime > g:unite_update_time
    let b:unite.update_time_save = &updatetime
    let &updatetime = g:unite_update_time
  endif

  setlocal modifiable
endfunction"}}}
function! s:on_insert_leave()  "{{{
  if line('.') == b:unite.prompt_linenr
    " Redraw.
    call unite#redraw()
  endif

  if has_key(b:unite, 'update_time_save') && &updatetime < b:unite.update_time_save
    let &updatetime = b:unite.update_time_save
  endif

  setlocal nomodifiable
endfunction"}}}
function! s:on_cursor_hold_i()  "{{{
  if line('.') == b:unite.prompt_linenr
    " Redraw.
    call unite#redraw()

    " Prompt check.
    if col('.') <= len(b:unite.prompt)
      startinsert!
    endif
  endif
endfunction"}}}
function! s:on_cursor_hold()  "{{{
  if line('.') == b:unite.prompt_linenr
    " Redraw.
    call unite#redraw()
  endif
endfunction"}}}
function! s:on_cursor_moved()  "{{{
  execute 'setlocal' line('.') == b:unite.prompt_linenr ? 'modifiable' : 'nomodifiable'
  execute 'match' (line('.') <= b:unite.prompt_linenr ? line('$') <= b:unite.prompt_linenr ?
        \ 'Error /\%'.b:unite.prompt_linenr.'l/' : 'PmenuSel /\%'.(b:unite.prompt_linenr+1).'l/' : 'PmenuSel /\%'.line('.').'l/')
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
function! s:get_unite() "{{{
  return exists('b:unite') ? b:unite : s:unite
endfunction"}}}
function! s:compare_sources(source_a, source_b) "{{{
  return a:source_a.unite__number - a:source_b.unite__number
endfunction"}}}
function! s:compare_substitute_patterns(pattern_a, pattern_b)"{{{
  return a:pattern_b.priority - a:pattern_a.priority
endfunction"}}}
function! s:compare_marked_candidates(candidate_a, candidate_b)"{{{
  return a:candidate_a.unite__marked_time - a:candidate_b.unite__marked_time
endfunction"}}}
function! s:extend_actions(self_func, action_table1, action_table2)"{{{
  return extend(a:action_table1, s:filter_self_func(a:action_table2, a:self_func), 'keep')
endfunction"}}}
function! s:filter_alias_action(action_table, alias_table)"{{{
  for [l:alias_name, l:alias_action] in items(a:alias_table)
    if l:alias_action ==# 'nop'
      if has_key(a:action_table, l:alias_name)
        " Delete nop action.
        call remove(a:action_table, l:alias_name)
      endif
    else
      let a:action_table[l:alias_name] = a:action_table[l:alias_action]
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
    throw 'no such action ' . a:action_name
  endif

  let l:action = l:action_table[a:action_name]
  " Convert candidates.
  call l:action.func(
        \ (l:action.is_selectable && type(a:candidate) != type([])) ?
        \ [a:candidate] : a:candidate)
endfunction"}}}
function! s:get_loaded_sources(...)"{{{
  let l:unite = s:get_unite()
  return a:0 == 0 ? l:unite.sources : get(l:unite.sources, a:1, {})
endfunction"}}}
"}}}

" vim: foldmethod=marker
doc/unite.jax	[[[1
1380
*unite.txt*	すべてのsourceを統合する

Version: 1.0
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
  source		  |unite-source|
  kind			  |unite-kind|
  アクション		  |unite-action|
設定例			|unite-examples|
sourceの作成		|unite-create-source|
kindの作成		|unite-create-kind|
ユーザーのsource	|unite-user-sources|
ToDo			|unite-todo|
既知のバグ			|unite-bugs|
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

		現在uniteバッファに居る場合、絞り込みテキストは保存される。

		source名の後ろに:で区切った文字列のリストを引数として渡すことが
		できる。引数に含まれる:と\は\でエスケープしなければならない。引
		数がどう解釈されるかは、sourceによって異なる。

		例：
		"file:hoge:piyo": source fileに与える引数は[hoge, piyo]である。
		"file:hoge\:piyo": source fileに与える引数は[hoge:piyo]である。

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
		プロンプトを指定する。指定しないと、'>'が使われる。

						*unite-options-default-action*
		-default-action={default-action}
		デフォルトアクションを指定する。指定しないと、'default'が使われ
		る。

						*unite-options-start-insert*
		-start-insert
		初期状態が絞り込みモードになる。指定しないと、初期状態はNormal
		modeになる。

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

g:unite_enable_ignore_case			*g:unite_enable_ignore_case*
		大文字・小文字を区別するかを制御する。1ならば区別しない。
		
		初期値は'ignorecase'オプションと同じである。

g:unite_enable_smart_case			*g:unite_enable_smart_case*
		入力に大文字が含まれている場合は、大文字・小文字を区別する。1
		ならば有効になる。
		
		初期値は'infercase'オプションと同じである。

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
		|unite-source-file|, |unite-source-file_rec|の候補に表示しな
		いファイルの正規表現パターンを指定する。マッチングはファイル
		のフルパスに対して行われる。この変数が空文字列以外であれば、
		指定した正規表現で結果をフィルタリングする。正規表現の大文字・
		小文字の区別は'ignorecase'の設定次第である。

		初期値は autoload/unite/sources/file.vim を参照。

g:unite_source_file_mru_time_format		*g:unite_source_file_mru_time_format*
		|unite-source-file_mru|の最終アクセス時間の表示フォーマット
		を指定する。フォーマットは|strftime()|で指定できるものと同じ
		である。
		
		初期値は"(%c)"である。

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
		果をフィルタリングする。正規表現の大文字・小文字は
		'ignorecase'の設定に関わらず、明確に区別される。
		
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

g:unite_source_file_rec_max_depth		*g:unite_source_file_rec_max_depth*
		|unite-source-file_rec|が検索するディレクトリ深さの最大値を
		指定する。
		
		初期値は10である。

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

------------------------------------------------------------------------------
キーマッピング 					*unite-key-mappings*

ノーマルモードマッピング

<Plug>(unite_exit)				*<Plug>(unite_exit)*
		uniteを終了する。

<Plug>(unite_do_default_action)			*<Plug>(unite_do_default_action)*
		選択している候補に対してデフォルトのアクションを実行する。アク
		ションは候補のkindごとに定義されている。kindについては
		|unite-kind|を参照。デフォルトのアクションについては
		|unite-default-action|を参照。

<Plug>(unite_choose_action)			*<Plug>(unite_choose_action)*
		選択している候補に対して実行するアクションを選択する。アクショ
		ンは候補のkindごとに定義されている。複数の候補が選択されている
		場合は共通なアクションのみ選択ができる。kindについては
		|unite-kind|を参照。

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

インサートモードマッピング

<Plug>(unite_exit)				*i_<Plug>(unite_exit)*
		uniteを終了する。

<Plug>(unite_insert_leave)			*i_<Plug>(unite_insert_leave)*
		ノーマルモードに移行する。

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
<Space>		|<Plug>(unite_toggle_mark_current_candidate)|
<Tab>		|<Plug>(unite_choose_action)|
<C-n>		|<Plug>(unite_rotate_next_source)|
<C-p>		|<Plug>(unite_rotate_previous_source)|
<C-g>		|<Plug>(unite_print_candidate)|
<C-l>		|<Plug>(unite_redraw)|
gg		|<Plug>(unite_cursor_top)|
j		|<Plug>(unite_loop_cursor_down)|
<Down>		|<Plug>(unite_loop_cursor_down)|
k		|<Plug>(unite_loop_cursor_up)|
<Up>		|<Plug>(unite_loop_cursor_up)|
<CR>		候補を選択している場合は default アクションの実行
l		候補を選択している場合は default アクションの実行
d		候補を選択している場合は delete アクションの実行
b		候補を選択している場合は bookmark アクションの実行
e		候補を選択している場合は narrow アクションの実行
p		候補を選択している場合は preview アクションの実行
x		候補を選択している場合は|<Plug>(unite_quick_match_default_action)|

インサートモードマッピング
{lhs}		{rhs}
--------	-----------------------------
<ESC>		|i_<Plug>(unite_insert_leave)|
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
/		候補を選択している場合は narrow アクションの実行
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

unite#available_kinds([{kind-name}])		*unite#available_kinds()*
			{kind-name}に指定されたkindを取得する。kindが存在しな
			い場合は空のディクショナリを返す。{kind-name}を省略し
			た場合、キーがkind名、値がそれぞれのkindとしたディク
			ショナリを返す。

			この戻り値を変更してはいけない。

unite#available_sources([{source-name}])	*unite#available_sources()*
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
		の要素は{source-name}または、[{source-name}, [{args},...]]とい
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
		call unite#set_substitute_pattern('files', '^\~', substitute(substitute($HOME, '\\', '/', 'g'), ' ', '\\\\ ', 'g'), -100)
		call unite#set_substitute_pattern('files', '[^~.*]\zs/', '*/*', 100)
<
unite#get_substitute_pattern({buffer-name})
						*unite#get_substitute_pattern()*
		uniteのバッファ名{buffer-name}における絞込みテキストの置換パタ
		ーンを取得する。{buffer-name}に置換パターンが定義されていない
		と、エラーになる。デバッグ用である。

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

unite#undef_source({name})			*unite#undef_source()*
		|unite#define_source()|で追加した{name}というsourceを削除する。
		存在しないときは無視される。

unite#undef_kind({name})			*unite#undef_kind()*
		|unite#define_kind()|で追加した{name}というkindを削除する。
		存在しないときは無視される。

==============================================================================
source						*unite-source*

ここでは、標準で実装されているsourceの仕様を解説する。

						*unite-source-file*
file		入力されたファイルを候補とする。

						*unite-source-file_mru*
file_mru	最近使用したファイルを候補とする。使用した順番に整列される。

						*unite-source-directory_mru*
directory_mru	最近使用したカレントディレクトリを候補とする。使用した順番に整
		列される。

						*unite-source-file_rec*
file_rec	入力された絞り込みテキストのディレクトリまたは、現在ディレクト
		リ直下のファイルすべてを候補とする。ディレクトリや隠しファイル
		は候補から除外される。候補が多すぎる場合はフリーズするかもしれ
		ない。

		sourceの引数として、絞り込み文字列を取る。

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

						*unite-source-register*
register	レジスタに保存されている文字列を候補とする。

						*unite-source-bookmark*
bookmark	ブックマークされたファイルやディレクトリを候補とする。

						*unite-source-source*
source		uniteのsource名そのものを候補とする。

		選択されたsource名を|unite#start()|で起動する。contextは現在
		のuniteバッファのものを受け継ぐ。

						*unite-source-window*
window		開いているウィンドウを候補とする。ウィンドウは移動した順番に
		整列される。

==============================================================================
kind						*unite-kind*

ここでは、標準で実装されているkindの仕様と、内部で使用する属性について解説する。

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
		このkindはcdable, openableを継承しているため、それらのkindが
		使用する属性も必要である。

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

			action__line		(数値)			(任意)
				ジャンプするファイルの行番号

			action__pattern		(文字列)		(任意)
				ファイルを開いた後に検索するパターン

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

						*unite-kind-window*
window		ウィンドウ
		このkindはcdableを継承するため、それらのkindが使用する属性も
		必要である。

			action__window_nr	(文字列)		(必須)
				対象となるウィンドウの番号

==============================================================================
アクション					*unite-action*

|<Plug>(unite_choose_action)|で選択するときは、アクション名を補完したり、曖昧で
なければ先頭文字で省略することができる。

kind別アクション

common					*unite-action-common*
全てのkindに共通なインタフェースを定義している。
内部では、candidate.wordを使う。
	nop		何もしない
	yank		ヤンクする
	yank_escape	エスケープした候補文字列をヤンクする
	ex		候補をコマンドラインに入力した状態にする
	insert		現在のバッファに文字列を挿入する

openable					*unite-action-openable*
オープン可能ファイルのインタフェースを定義している。
内部でopenアクションを呼び出すので、openアクションは継承したkind側が個別に定義
する必要がある。
	tabopen		ファイルをタブで開く
	split		ウィンドウを水平分割してファイルを開く
	vsplit		ウィンドウを垂直分割してファイルを開く
	left		ウィンドウを垂直分割して左側にファイルを開く
	right		ウィンドウを垂直分割して右側にファイルを開く
	above		ウィンドウを水平分割して上側にファイルを開く
	below		ウィンドウを水平分割して下側にファイルを開く

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
	tabvimshell	そのディレクトリで|:VimShellTab|を実行する。これは
			|vimshell|がインストールされている場合にのみ有効な
			アクションである。
	rec		そのディレクトリで|unite-source-file_rec|を起動する。

file						*unite-action-file*
ファイルはすべて別のバッファに開かれる。
このkindは|unite-action-openable|と|unite-action-cdable|のアクションを継承している。
	open		ファイルを開く
	preview		プレビューウィンドウにファイルを開く
	bookmark	候補をブックマークに追加する

buffer						*unite-action-buffer*
このkindは|unite-action-file|のアクションを継承している。
	delete		バッファを|:bdelete|
	fdelete		バッファを|:bdelete!|
	wipeout		バッファを|:bwipeout|
	unload		バッファを|:bunload|
	bookmark	候補をブックマークに追加する

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

command						*unite-action-command*
	execute		コマンドを実行する

window						*unite-action-window*
このkindは|unite-action-cdable|のアクションを継承する。
	open		ウィンドウへ移動
	delete		ウィンドウを閉じる
	only		そのウィンドウ以外すべて閉じる

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

autoload/unite/sources//に*.vimのファイルを置いておくと、自動的に読み込まれる。
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
gather_candidates	関数		(必須)
			uniteが候補を収集する際に呼ばれる。
			この関数は{args}と{context}を引数に取り、{candidate}の
			リストを返す。
			{args}は|:Unite|コマンドを実行された際にsourceに渡され
			た引数リスト、{context}はsourceが呼ばれたときのコンテキ
			スト情報である。
			{context}については|unite-notation-{context}|、
			{candidate}については|unite-notation-{candidate}|を参照
			せよ。

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

			on_close		*unite-source-attribute-hooks-on_close*
			<Plug>(unite_exit)を実行したときや、コマンドの実行後に
			uniteバッファが閉じられる前に呼ばれる。
			この関数は{args}と{context}を引数に取る。
			この関数が呼ばれたとしても、まだuniteバッファが残ってい
			る可能性があるので、内部で使用する変数は解放しないほう
			がよい。

						*unite-source-attribute-action_table*
action_table		辞書		(任意)
			source独自のアクションテーブルを追加するときに使用する。
			キーはアクションテーブルを追加するkindで、値は追加するアクションテーブルである。
			キーに"*"を設定すると、どんなkindにもマッチするようになる。
			アクションテーブルの詳細については、|unite-kind-attribute-action_table|
			を参照せよ。もしこれが与えられない場合、空となる。

						*unite-source-attribute-default_action*
default_action		文字列		(任意)
			source独自の標準的なアクションを追加するときに使用する。
			キーはアクションテーブルを追加するkindで、値は標準ア
			クションである。これを省略した場合はkindの
			default_actionを使用する。

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

description		文字列		(任意)
			sourceを説明する文字列。省略すると""になる。
			|unite-source-source|において表示に使われる。

NOTATION					*unite-notation*

{context}					*unite-notation-{context}*
			コンテキスト情報を与える辞書変数。主に、次の情報を持つ。
			|unite#get_context()|で取得できる。システムで共有さ
			れているので、これを書き換えてはいけない。

			input			(文字列)
				ユーザーの入力文字列。

			buffer_name		(文字列)
				uniteバッファの名前。

			prompt			(文字列)
				uniteバッファのプロンプト文字列。

			is_insert		(数値)
				uniteバッファが挿入モードから呼び出されたかどう
				か。

			is_redraw		(数値)
				<Plug>(unite_redraw)で呼び出されたかどうか。
				ユーザーが明示的にキャッシュを破棄しているので、
				自前でキャッシュを管理するsourceにとっては重
				要な情報となる。

			source			(辞書)
				現在候補を収集しているsourceの情報。

			winnr			(数値)
				uniteを呼び出したウィンドウの番号。

{candidate}					*unite-notation-{candidate}*
			候補を表す辞書変数。次の情報を持つ。

			word			(文字列)
				画面に表示される候補を表す文字列。候補の絞り込
				みに使われる。

			abbr			(文字列)	(任意)
				画面に表示される候補を表す文字列。wordより優先
				されるが、候補の絞り込みには使われない。

			source			(文字列)
				候補が所属するsource名。

			kind			(文字列)	(任意)
				候補が所属するkind名。省略すると"common"となる。

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

|unite-action-table|も参照せよ。

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
	
	  " Start insert.
	  "let g:unite_enable_start_insert = 1
	endfunction"}}}
	
	let g:unite_source_file_mru_limit = 200
	
>
==============================================================================
ユーザーのsource				*unite-user-sources*

作者以外の手により作成されたsourceについては、Wikiのページを参照せよ。
https://github.com/Shougo/unite.vim/wiki/unite-plugins

==============================================================================
TODO						*unite-todo*

==============================================================================
既知のバグ					*unite-bugs*

==============================================================================
更新履歴					*unite-changelog*

doc/unite.txtを参照せよ。

==============================================================================
vim:tw=78:ts=8:ft=help:norl:noet:fen:fdl=0:
doc/unite.txt	[[[1
1674
*unite.txt*	Unite all sources

Version: 1.0
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
  Functions		  |unite-functins|
  source		  |unite-source|
  kind			  |unite-kind|
  Actions		  |unite-action|
Configulation Examples	|unite-examples|
Create source		|unite-create-source|
Create kind		|unite-create-kind|
User defined source	|unite-user-sources|
ToDo			|unite-todo|
Bugs			|unite-bugs|
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

		In case you are already on unite buffer, the narrowing text
		will be stored.

		You may give a list of strings, separating with :, after the
		name of sources. You must escape : and \ with \ for parameters
		themselves.  It depends on the sources how the parameters will
		be interpreted.

		Examples:
		"file:foo:bar": the parameters of source file are [foo, bar].
		"file:foo\:bar": the parameters of source file are [foo:bar].

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
		Specifies a prompt. The default value is '>'.

						*unite-options-default-action*
		-default-action={default-action}
		Specifies a default action. The default value is 'default'.

						*unite-options-start-insert*
		-start-insert
		Opens unite with narrowing mode. Otherwise the initial mode
		will be Normal mode.

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

:UniteWithCurrentDir [{options}] {sources}	*:UniteWithCurrentDir*
		Equivalent to |:Unite| except that the initial narrowing text
		is the current directory.

:UniteWithBufferDir [{options}] {sources}	*:UniteWithBufferDir*
		Equivalent to |:Unite| except that the initial narrowing text
		is the buffer's directory.

:UniteWithCursorWord [{options}] {sources}	*:UniteWithCursorWord*
		Equivalent to |:Unite| except that the initial narrowing text
		is the word on the cursor.

:UniteResume [{buffer-name}]name		*:UniteResume*
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

g:unite_enable_ignore_case			*g:unite_enable_ignore_case*
		Ignores case when the value is 1.

		Default value is same to 'ignorecase' option.

g:unite_enable_smart_case			*g:unite_enable_smart_case*
		Distinguish capitals from smalls when the narrowing text has a
		capital letter if this variable is 1.
		
		Default value is same to 'infercase'.

g:unite_split_rule				*g:unite_split_rule*
		Defines split position rule.

		Default value is "topleft".

g:unite_enable_split_vertically				*g:unite_enable_split_vertically*
		If this option is 1, splits unite window vertically.

		Default value is 0; unite window will be splat horizontally.

g:unite_winheight					*g:unite_winheight*
		The height of unite window when it's split horizontally.  It's
		ignored in splitting vertically.

		The default value is 20.

g:unite_winwidth					*g:unite_winwidth*
		The width of unite window when it's split vertically.  It's
		ignored in splitting horizontally.

		The default value is 90.

g:unite_cd_command					*g:unite_cd_command*
		Specifies the Vim command for cd action.
		This command must interpret |`=|.

		The default value is "cd".

g:unite_lcd_command					*g:unite_lcd_command*
		Specifies the Vim command for lcd action.
		This command must interpret |`=|.

		The default value is "lcd".

g:unite_quick_match_table				*g:unite_quick_match_table*
		The table of completion candidates of quick mtch list,
		corresponding the narrowing text.

		The default value is complex; so see plugin/unite.vim.

g:unite_data_directory					*g:unite_data_directory*
		Specifies directories for configurations internally used in
		unite itself or its sources.  If the directory doesn't exist
		the directory will be automatically generated.  For example source
		of file_mru saves the information of the most recent used
		files on the directory.
		
		Default value is expand('~/.unite'); the absolute path of it.

g:unite_no_default_keymappings			*g:unite_no_default_keymappings*
		If it's 1, unite doesn't map any defauilt key mappings.  You
		shouldn't enable this option without any strong reasons.

		This variable doesn't exist unless you define explicitly.

SOURCES VARIABLES 					*unite-sources-variables*

g:unite_source_file_ignore_pattern		*g:unite_source_file_ignore_pattern*
		Specifies a regex pattern for ignoring some specific
		candidates in which source is file.  This matches on the full
		path of each files.  If the variable isn't empty string, unite
		filters with the regex pattern on the results.  It depends on
		'ignorecase' to distinguish cases or not.

		Refer autoload/unite/sources/file.vim about the default value.

g:unite_source_file_mru_time_format		*g:unite_source_file_mru_time_format*
		Specifies the output format of the last access time of
		|unite-source-file_mru|.  The format is same to |strftime()|.

		Default value is "(%c)".

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
		'ignorecase' value, it's case sensitive.

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

g:unite_source_file_rec_max_depth		*g:unite_source_file_rec_max_depth*
		Specifies the maximum directory depth that 
		|unite-source-file_rec| searches.

		Default is 10.

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

------------------------------------------------------------------------------
KEY MAPPINGS 					*unite-key-mappings*

Normal mode mappings.

<Plug>(unite_exit)				*<Plug>(unite_exit)*
		Exits unite.

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
		Toggles the mark of the candidate in the current line.  You may
		run an action on multiple candidates at the same time by
		marking multiple candidates.

<Plug>(unite_redraw)				*<Plug>(unite_redraw)*
		Without waiting for the update time defined in 
		|g:unite_update_time|, Unite updates its view immediately.
		This is also used internally for updating the cache.

<Plug>(unite_rotate_next_source)		*<Plug>(unite_rotate_next_source)*
		Changes the order of source normally.

<Plug>(unite_rotate_previous_source)		*<Plug>(unite_rotate_previous_source)*
		Changes the order of source reversely.

<Plug>(unite_print_candidate)			*<Plug>(unite_print_candidate)*
		Shows the target of the action of the selected candiate.  For
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

Insert mode mappings.

<Plug>(unite_exit)				*i_<Plug>(unite_exit)*
		Exits Unite.

<Plug>(unite_insert_leave)			*i_<Plug>(unite_insert_leave)*
		Changes the mode into Normal mode.

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
<Space>		|<Plug>(unite_toggle_mark_current_candidate)|
<Tab>		|<Plug>(unite_choose_action)|
<C-n>		|<Plug>(unite_rotate_next_source)|
<C-p>		|<Plug>(unite_rotate_previous_source)|
<C-g>		|<Plug>(unite_print_candidate)|
<C-l>		|<Plug>(unite_redraw)|
gg		|<Plug>(unite_cursor_top)|
j		|<Plug>(unite_loop_cursor_down)|
<Down>		|<Plug>(unite_loop_cursor_down)|
k		|<Plug>(unite_loop_cursor_up)|
<Up>		|<Plug>(unite_loop_cursor_up)|
<CR>		In case when you selected a candidate, runs default action
l		In case when you selected a candidate, runs default action
d		In case when you selected a candidate, runs delete action
b		In case when you selected a candidate, runs bookmark action
e		In case when you selected a candidate, runs narrow action
p		In case when you selected a candidate, runs preview action
x		In case when you selected a candidate, runs
		|<Plug>(unite_quick_match_default_action)|

Insert mode mappings.
{lhs}		{rhs}
--------	-----------------------------
<ESC>		|i_<Plug>(unite_insert_leave)|
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
/		In case when you selected a candidate, runs narrow action
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

unite#available_kinds([{kind-name}])		*unite#available_kinds()*
			Gets the kinds of {kind-name}.  Unless they exist this
			returns an empty dictionary.  This returns a
			dictionary which keys are kind names and values are
			the kinds when you skipped giving {kind-name}.

			Changing the return value is not allowed.

unite#available_sources([{source-name}])	*unite#available_sources()*
			Gets the source of {source-name}. Unless they exist
			this returns an empty dictionary.  This returns a
			dictionary which keys are source names and values are
			the sources when you skipped giving {source-name}.

			Changing the return value is not allowed.

CUSTOMS						*unite-functions-customs*

unite#start({sources}, [, {context}])				*unite#start()*
		Creates a new Unite buffer.  In case when you are already on a
		Unite buffer, the narrowing text will be preserved.

		{sources} is a list which elements are formatted as
		{source-name} or [{source-name}, [{args}, ...]].  You may
		specify multiple string arguments in {args} for {source-name}.


		Refer |unite-notation-{context}| about {context}.  If you
		skipped it will use the default value.

unite#get_context()						*unite#get_context()*
		Gets the context information of the current Unite buffer.
		This is used by functions like |unite#custom_action()| to call
		|unite#start()| internally.

unite#do_action({action-name})					*unite#do_action()*
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
unite#smart_map({narrow-map}, {select-map})			*unite#smart_map()*
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
		call unite#set_substitute_pattern('files', '^\~', substitute(substitute($HOME, '\\', '/', 'g'), ' ', '\\\\ ', 'g'), -100)
		call unite#set_substitute_pattern('files', '[^~.*]\zs/', '*/*', 100)
<
unite#get_substitute_pattern({buffer-name})
						*unite#get_substitute_pattern()*
		Gets the substitute pattern for narrowing text of a Unite buffer
		name {buffer-name}.  This causes an error if the substitute
		pattern of {buffer-name}.  This is for debugging.

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
unite#undef_custom_action({kind}, {name})			*unite#undef_custom_action()*
		Deletes an action which name is {name} of {kind} that you
		added with using |unite#custom_action()|.  You may specify
		multiple {kind} with separating ",".  This function doesn't do
		anything if the function doesn't exist.

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
unite#take_action({action-name}, {candidate})
						*unite#take_action()*
		Runs an action {action-name} against {candidate}.  This will
		be mainly used in |unite#custom_action()|.  When the action is
		is_selectable, the {candidate} will be automatically converted
		into a list.

unite#take_parents_action({action-name}, {candidate}, {extend-candidate})
						*unite#take_parents_action()*
		Same to |unite#take_action()| but searches the parents' action
		table with combining {extend-candiate} on {candidate}.  This
		is handy for reusing parents' actions.

unite#define_source({source})			*unite#define_source()*
		Adds {source} dinamically.  See also |unite-create-source|
		about the detail of source.  If a source which name is same,
		that will be overwritten.

unite#define_kind({kind})			*unite#define_kind()*
		Adds {kind} dinamically.  See also |unite-create-kind|
		about the detail of kind.  If a kind which name is same,
		that will be overwritten.

unite#undef_source({name})			*unite#undef_source()*
		Removes the source which name is {name} that was added by
		|unite#define_source()|.  If such a source doesn't exist, this
		function doesn't do anything.

unite#undef_kind({name})			*unite#undef_kind()*
		Removes the kind which name is {name} that was added by
		|unite#define_kind()|.  If such a kind doesn't exist, this
		function doesn't do anything.

==============================================================================
SOURCE						*unite-source*

						*unite-source-file*
file		Nominates an input file as a candidate.

						*unite-source-file_mru*
file_mru	Nominates files you used recently as candidates, ordering
		by time series.

						*unite-source-directory_mru*
directory_mru	Nominates directories you used recently as candidates,
		ordering by time series.

						*unite-source-file_rec*
file_rec	Nominates all directory or file names of input narrowing text
		under the current directly as candidates.  This may get Vim
		frozen when there are too many candidates.

		You may pass the narrowing text as an argument of sourc e.

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

						*unite-source-register*
register	Nominates the strings stored in registers as candidates.

						*unite-source-bookmark*
bookmark	Nominates files or directories you bookmarked as candidates.

						*unite-source-source*
source		Nominates Unite source names themselves as candidates.

		Runs |unite#start()| with the selected source name, using the
		current Unite buffer context.

						*unite-source-window*
window		Nominates opend windows as candidates, ordering by time
		series.

==============================================================================
KIND						*unite-kind*

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
file		An inteface for files.  This kind is inheriting cdable and
		openable, so this requries kinds that they require.

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
		this requires keys it requries.

						*unite-kind-word*
word		A String that you can insert

			word			(String)	(Required)
				The string you want to insert

						*unite-kind-jump_list*
jump_list	An interface for jump lists.  This kind is inheriting
		openable, so this requires that it requires.

			action__path		(String)	(Required)
				The path of the file that you'll jump into.

			action__line		(Number)	(Optional)
				The line number in the file you'll jump into.

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

==============================================================================
ACTIONS						*unite-action*

==============================================================================
CREATE SOURCE					*unite-create-source*

------------------------------------------------------------------------------
SOURCE ATTRIBUTES				*unite-source-attributes*

						*unite-source-attribute-name*
name			string		(requried)
			The name of a source. It must consist of the following
			characters:
			- a-z
			- 0-9
			- _
			- /

			For example:
			- "buffer" , "file_mru" and "virw/git" are valid.
			- "BadOne", "!@#$%^&*()_[]{}-|" and "" are
			  not valid.

						*unite-source-attribute-max_candidates*
max_candidates		number		(optional)
			The maximum number of candidates.

			This attribute is optional; if it is not given, 0 is
			used as the default value.  This means maximum number
			is infinity.

						*unite-source-attribute-required_pattern_length*
required_pattern_length	number		(optional)
			The required pattern length to collect candidates.

			This attribute is optional; if it is not given, 0 is
			used as the default value. This means that always
			collect candidates.

==============================================================================
CREATE KIND					*unite-create-kind*

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
	
	autocmd FileType unite call s:unite_my_settings()
	function! s:unite_my_settings()"{{{
	  " Overwrite settings.
	
	  nmap <buffer> <ESC>      <Plug>(unite_exit)
	  imap <buffer> jj      <Plug>(unite_insert_leave)
	  "imap <buffer> <C-w>     <Plug>(unite_delete_backward_path)
	
	  " Start insert.
	  "let g:unite_enable_start_insert = 1
	endfunction"}}}
	
	let g:unite_source_file_mru_limit = 200
	
>
==============================================================================
USER DEFINED SOURCES				*unite-user-sources*

See Wiki page(Japanese).
https://github.com/Shougo/unite.vim/wiki/unite-plugins

==============================================================================
TODO						*unite-todo*

==============================================================================
BUGS						*unite-bugs*

==============================================================================
CHANGELOG					*unite-changelog*

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
Ver.0.5
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
Ver.0.1
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
36
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

command! -nargs=? -complete=file UniteBookmarkAdd call unite#sources#bookmark#_append(<q-args>)

let g:loaded_unite_source_bookmark = 1

" __END__
" vim: foldmethod=marker
plugin/unite/buffer.vim	[[[1
39
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

augroup plugin-unite-source-buffer
  autocmd!
  autocmd BufEnter,BufWinEnter,BufFilePost * call unite#sources#buffer#_append()
augroup END

let g:loaded_unite_source_buffer = 1

" __END__
" vim: foldmethod=marker
plugin/unite/directory_mru.vim	[[[1
39
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

augroup plugin-unite-source-directory_mru
  autocmd!
  autocmd BufLeave,BufWinLeave,BufFilePost * call unite#sources#directory_mru#_append()
augroup END

let g:loaded_unite_source_directory_mru = 1

" __END__
" vim: foldmethod=marker
plugin/unite/file_mru.vim	[[[1
39
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

augroup plugin-unite-source-file_mru
  autocmd!
  autocmd BufEnter,BufWinEnter,BufFilePost * call unite#sources#file_mru#_append()
augroup END

let g:loaded_unite_source_file_mru = 1

" __END__
" vim: foldmethod=marker
plugin/unite/tab.vim	[[[1
39
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

augroup plugin-unite-source-tab
  autocmd!
  autocmd TabEnter * call unite#sources#tab#_append()
augroup END

let g:loaded_unite_source_tab = 1

" __END__
" vim: foldmethod=marker
plugin/unite/window.vim	[[[1
39
"=============================================================================
" FILE: window.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 27 Dec 2010.
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

augroup plugin-unite-source-window
  autocmd!
  autocmd WinEnter,BufWinEnter * call unite#sources#window#_append()
augroup END

let g:loaded_unite_source_window = 1

" __END__
" vim: foldmethod=marker
plugin/unite.vim	[[[1
182
"=============================================================================
" FILE: unite.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 17 Nov 2010
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
" Version: 1.0, for Vim 7.0
"=============================================================================

if exists('g:loaded_unite')
  finish
endif

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
if !exists('g:unite_data_directory')
  let g:unite_data_directory = expand('~/.unite')
endif
if !isdirectory(fnamemodify(g:unite_data_directory, ':p'))
  call mkdir(fnamemodify(g:unite_data_directory, ':p'), 'p')
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
    if l:path !~ '/$'
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
      let l:source_args = map(split(l:arg[len(l:source_name) :], '\\\@<!:'),
            \ 'substitute(v:val, ''\\\(.\)'', "\\1", "g")')
      call add(l:args, insert(l:source_args, l:source_name))
    endif
  endfor

  return [l:args, l:options]
endfunction"}}}

command! -nargs=? -complete=customlist,unite#complete_buffer UniteResume call unite#resume(<q-args>)

let g:loaded_unite = 1

" __END__
" vim: foldmethod=marker
syntax/unite.vim	[[[1
66
"=============================================================================
" FILE: syntax/unite.vim
" AUTHOR: Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 09 Nov 2010
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

syntax match uniteStatusLine /\%1l.*/
\            contains=uniteSourcePrompt,uniteSourceSeparator,uniteSourceNames
syntax match uniteSourcePrompt /^Sources/ contained nextgroup=uniteSourceSeparator
syntax match uniteSourceSeparator /: / contained nextgroup=uniteSourceNames
syntax match uniteSourceNames /[a-z_/-]\+/ contained

syntax match uniteInputLine /\%2l.*/ contains=uniteInputPrompt,uniteInputPromptError,uniteInputSpecial
syntax match uniteInputSpecial /\\\@<![*!,]/ contained

syntax match uniteMarkedLine /^\*.*/
syntax match uniteNonMarkedLine /^-.*/     contains=uniteCandidateSourceName,uniteCandidateAbbr
syntax match uniteCandidateSourceName /^- \zs[a-z_/-]\+/ contained

highlight default link uniteSourceNames  Type
highlight default link uniteSourcePrompt  PreProc
highlight default link uniteSourceSeparator  NONE

highlight default link uniteMarkedLine  Statement
highlight default link uniteCandidateSourceName  Type
highlight default link uniteCandidateAbbr  Pmenu

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

let b:current_syntax = 'unite'
test/00-unite-source.vim	[[[1
67
" vim:foldmethod=marker:fen:
scriptencoding utf-8

" Saving 'cpoptions' {{{
let s:save_cpo = &cpo
set cpo&vim
" }}}


function! s:run()
  let l:kind = {
      \ 'name' : 'hoge',
      \ 'default_action' : 'open',
      \ 'action_table': {},
        \ }
  let l:kind.action_table.open = {
        \ 'is_selectable' : 1, 
        \ }
  function! l:kind.action_table.open.func(candidate)
    echo 'hoge'
  endfunction
  
  Ok unite#define_kind(l:kind) == 0, "defined kind"
  
  let l:source = {
        \ 'name' : 'hoge',
        \ 'is_volatile' : 1,
        \}
  function! l:source.gather_candidates(args, context)"{{{
    " Add dummy candidate.
    let l:candidates = [ a:context.input ]

    call map(l:candidates, '{
          \ "word" : v:val,
          \ "source" : "hoge",
          \ "kind" : "hoge",
          \}')

    if g:unite_source_file_ignore_pattern != ''
      call filter(l:candidates, 'v:val.word !~ ' . string(g:unite_source_file_ignore_pattern))
    endif

    return l:candidates
  endfunction"}}}
  
  Ok unite#define_source(l:source) == 0, "defind source"

  let candidate = {
  \   'ku__source': unite#available_sources('hoge'),
  \   'word': 'EMPRESS',
  \ }

  silent! let _ = unite#take_action('*choose*', candidate)
  Like _ 'no such action'

  Ok unite#undef_kind(l:kind.name) == 0, "undef kind"
  Ok unite#undef_source(l:source.name) == 0, "undef source"
  
endfunction

call s:run()
Done


" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
" }}}
