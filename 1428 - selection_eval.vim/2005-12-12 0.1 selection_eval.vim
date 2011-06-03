"Synopsis: Evaluate the statements which you select with visual mode.
"
"
"Commands: vmap <c-e><c-r> evaluate with using ruby.
"          vmap <c-e><c-l> evaluate with using perl.
"          vmap <c-e><c-p> evaluate with using python.
"          vmap <c-e><c-s> evaluate with using shell(bash)
"          vmap <c-e><c-v> evaluate with using vim.
"
"
" if you don't use the default, you put the folloing your .vimrc.
"
"   let g:selection_eval_using_default = 0
"
"
"Usage: 1. press <ctrl> + v
"       2. select the snippets with visual mode.
"           cf) echo "hello world"
"       3. press <c-e><c-v> or :EV
"          this will evaluate by vim.
"       4. you get the result the another buffer named __RESULT__.
"       5. when you press the <e-c>c or :ECL, the result buffer(__RESULT__)
"          is closed.
"
"
"Reference: dbext.vim
"
"
"Written By: Shunsuke Tsutsui(shunsuke@luckpoint.jp)
"
"
"$LastChangedDate: 2005-12-12 23:37:18 +0900 (Mon, 12 Dec 2005) $


if !exists('g:selection_eval_using_default')
  let g:selection_eval_using_default = 1
endif

if g:selection_eval_using_default
  command! -range ER :call s:GetResultWithTempfile("ruby")
  command! -range EL :call s:GetResultWithTempfile("perl")
  command! -range EP :call s:GetResultWithTempfile("python")
  command! -range ES :call s:GetResultWithTempfile("bash")
  command! -range EV :call s:GetResultForVimScript()
  command!        ECL :call s:ResultBufClose()

  vmap <silent> <c-e><c-r> :ER<cr>
  vmap <silent> <c-e><c-l> :EL<cr>
  vmap <silent> <c-e><c-p> :EP<cr>
  vmap <silent> <c-e><c-s> :ES<cr>
  vmap <silent> <c-e><c-v> :EV<cr>
endif


fun! s:GetResultForVimScript()
    let tempfile = s:VisualBlockToTempFile() 
    let result = system("source " . tempfile)
    call s:AddToResultBuffer(result)
endfun


fun! s:GetResultWithTempfile(cmd)
    let tempfile = s:VisualBlockToTempFile() 
    let result = system(a:cmd . " " . tempfile)

    call s:AddToResultBuffer(result)
endfun


fun! s:TranslateByWord(cmd)
  let word = expand("<cword>")
  let result = system(a:cmd . " " . word)

  call s:AddToResultBuffer(result)
endfun

fun! s:Translate(cmd)
  let tempfile = s:VisualBlockToTempFile()
  let result = system(a:cmd . " " . tempfile)

  call s:AddToResultBuffer(result)
endfun


fun! s:GetResultWithCompile(compile_cmd, in, out)
    call s:VisualBlockToTempFileWithExt(a:in) 
    let cmd = a:compile_cmd . " -o " . a:out . " " . a:in
    let result = system(cmd)

    if v:shell_error != 0
      "error
      call s:AddToResultBuffer(cmd . "\n" . result)
      return
    endif

    let result1 = system(a:out)
    call s:AddToResultBuffer(result . "\n" . result1)
endfun


fun! s:GetResultWithRegister(cmd)
    let reg_a = @a
    exe "redir @a"
    let result = system(a:cmd)
    echo result
    redir END

    call s:AddToResultBuffer(@a)
    let @a = reg_a
endfun

fun! s:VisualBlockToTempFileWithExt(tempfile)
    let code = s:GetVisualBlock()
    if has('win32')
        exe "silent redir! > /cygwin" . a:tempfile
    else
        exe "silent redir! > " . a:tempfile
    endif
    silent echo code
    redir END
endfun

fun! s:VisualBlockToTempFile()
    let tempfile = tempname()
    let code = s:GetVisualBlock()
    exe "silent redir > " . tempfile
    silent echo code
    redir END

    return tempfile
endfun


fun! s:GetVisualBlock() range
    let save = @"
    silent normal gvy
    let vis_cmd = @"
    let @" = save
    return vis_cmd
endfun 


" TODO: more flexiblity
"
fun! s:ResultBufName()
    let res_buf_name = "__Result__"
    return res_buf_name
endfun


fun! s:ResultBufClose()
    let cur_winnr = winnr()
    let res_buf_name = s:ResultBufName()
    let buf_exists = bufexists(bufnr(res_buf_name))
    let res_buf_nbr = bufnr(res_buf_name)

    if buf_exists != 0 && bufwinnr(res_buf_nbr) != -1
        exec bufwinnr(res_buf_nbr) . "wincmd w"
        close
    endif
    exec cur_winnr."wincmd w"
endfun


fun! s:AddToResultBuffer(output)
    " Store current window number so we can return to it
    let cur_winnr = winnr()
    let res_buf_name = s:ResultBufName()
    " Retieve this value before we switch buffers
    let l:buffer_lines = 8

    " Do not use bufexists(res_buf_name), since it uses a fully qualified
    " path name to search for the buffer, which in effect opens multiple
    " buffers called "Result" if the files that you are executing the
    " commands from are in different directories.
    let buf_exists = bufexists(bufnr(res_buf_name))
    let res_buf_nbr = bufnr(res_buf_name)

    if buf_exists == 0
        " Create the new buffer
        silent exec 'belowright ' . l:buffer_lines . 'new ' . res_buf_name
    else
        if bufwinnr(res_buf_nbr) == -1
            " if the buffer is not visible, wipe it out and recreate it,
            " this will position us in the new buffer
            exec 'bwipeout! ' . res_buf_nbr
            silent exec 'bot ' . l:buffer_lines . 'new ' . res_buf_name
        else
            " If the buffer is visible, switch to it
            exec bufwinnr(res_buf_nbr) . "wincmd w"
        endif
    endif
    setlocal modified
    " Create a buffer mapping to clo this window
    nnoremap <buffer> q :clo<cr>
    
    " Delete all the lines prior to this run
    %d

    if strlen(a:output) > 0
        " Add to end of buffer
        silent! exec "normal! G"
        silent! exec "put = a:output"
    endif

    " Since this is a small window, remove any blanks lines
    silent %g/^\s*$/d
    " Fix the ^M characters, if any
    silent execute "%s/\<C-M>\\+$//e"
    " Dont allow modifications, and do not wrap the text, since
    " the data may be lined up for columns
    setlocal nomodified
    setlocal nowrap
    " Go to top of output
    norm gg
    " Return to original window
    exec cur_winnr."wincmd w"

    return
endfun
