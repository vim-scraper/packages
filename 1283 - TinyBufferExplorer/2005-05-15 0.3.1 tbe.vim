" Tiny Buffer Explorer
"
" Version: 0.3.1
" Description:
"
"   A tiny(1 file) and relatively useful buffer explorer plugin. This requiers
"   small space, provides quick accesses to other buffers, enables deleting one
"   or more buffers, and allows you to customize the format of buffer status.
"
" Last Change: 15-May-2005.
" Maintainer: Shuhei Kubota <shu_brief@yahoo.co.jp>
"
" Usage:
"
"   [Commands]
"
"   :TinyBufferExplorer
"
"       shows a buffer explorer window.
"
"   [Options(Variables)]
"
"   Note that the right-hand value is a default value.
"
"   g:TBE_showMethod = 'new'
"
"       This variable specifies the way the new window is created.
"
"       e.g. 'vertical new'
"
"   g:TBE_windowWidth = 30
"
"       This affects if g:DOT_newMethod is VERTICAL.
"
"   g:TBE_lineFormat = '{number} | {%}{h} {+} {name}   {dir}'
"
"       A format of a buffer. For more specifications, execute
"       "/!exists('g:TBE_lineFormat')" and read what is written there.
"
"   [Key Mappings]
"
"       <Enter>, <C-J>:
"           jumps to the node.
"
"       v, V, <C-V>:
"           selects by buffer(line). Selected buffers are targets for deletion.
"       d:
"           deletes a buffer. If the buffers selected, they are all deleted.
"
"       <Esc>: 
"           escapes from the buffer list window.
"       q:
"           escapes from the buffer list window.


if !exists('g:TBE_showMethod')
    let g:TBE_showMethod = 'new'
endif

if !exists('g:TBE_windowWidth')
    let g:TBE_windowWidth = 30
endif

if !exists('g:TBE_lineFormat')
    let g:TBE_lineFormat = '{number} | {%}{h} {+} {name}   {dir}'
    "let g:TBE_lineFormat = '{number} | {%}{h} {+} {path}'
    " 
    " format elements:
    " {number} : buffer number
    "
    " {path}   : full path of buffer
    "
    " {dir}    : directory in which buffer is
    "
    " {name}   : buffer name without directory
    "
    " {%}      : '%' for current buffer,
    "            '#' for alternative,
    "            ' ' for others
    "
    " {h}      : 'h' for hidden buffer,
    "            'a' for active
    "
    " {+}      : '+' for modified,
    "            ' ' for others
endif


command!  TinyBufferExplorer  call <SID>List()


let s:TBE_buffName = 'Tiny Buffer Explorer'


" set key mappings of TBE buffer's
function! s:SetKeyMappings()
    " leaving to a buffer under the cursor
    noremap  <silent> <buffer>  <C-J>    :call <SID>JumpToBuffer(line('.') - 1)<CR>
    noremap  <silent> <buffer>  <Enter>  :call <SID>JumpToBuffer(line('.') - 1)<CR>

    " deleting buffers
    noremap  <silent> <buffer>  d  :call <SID>DeleteBuffer()<CR>

    " force line selection
    noremap  <silent> <buffer>  v      V
    noremap  <silent> <buffer>  <C-V>  V

    " escaping from TBE buffer
    noremap  <silent> <buffer>  <Esc>  :bdelete<CR>
    noremap  <silent> <buffer>  q      :bdelete<CR>
endfunction


function! s:SetSyntax()
    "syntax  match  DiffChange  '.*+.*' 
endfunction


function! s:List()
    let s:currBuffNum = bufnr('%')
    let s:altBuffNum = bufnr('#')

    call s:CollectBuffers()
    call s:ShowBufferWindow()
endfunction


function! s:CollectBuffers()
    let s:buffCount = 0

    let buffLast = bufnr('$') + 1
    let i = 1

    while i < buffLast
        if buflisted(i) && (bufname(i) != s:TBE_buffName)
            call s:AddBuffer(i)
        endif

        let i = i + 1
    endwhile
endfunction


function! s:ShowBufferWindow()
    call s:OpenTempWindow(s:TBE_buffName, g:TBE_showMethod)
    setlocal modifiable noreadonly

    call s:SetKeyMappings()
    call s:SetSyntax()

    " make sure of lines
    %delete
    let i = 0
    if 1 < s:buffCount
        execute 'normal ' . (s:buffCount - 1) . "o\<Esc>"
    endif

    let maxNumLen = s:GetMaxColumnWidth('buffNum')
    let maxNameLen = s:GetMaxColumnWidth('buffName')
    let maxDirLen = s:GetMaxColumnWidth('buffDir')
    let maxPathLen = s:GetMaxColumnWidth('buffPath')

    let currBuffLine = 0
    let i = 0
    while i < s:buffCount
        if getbufvar(s:buffNum{i}, '&modified')
            let modChar = '+'
        else
            let modChar = ' '
        endif

        if s:buffNum{i} == s:currBuffNum
            let buffChar = '%'
            let currBuffLine = i + 1
        elseif s:buffNum{i} == s:altBuffNum
            let buffChar = '#'
            if currBuffLine == 0
                let currBuffLine = i + 1
            endif
        else
            let buffChar = ' '
        endif

        if bufwinnr(s:buffNum{i}) == -1
            let hiddenChar = 'h'
        else
            let hiddenChar = 'a'
        endif

        let line = g:TBE_lineFormat
        let buffNum = s:buffNum{i} . s:RepeatString(' ', maxNumLen - strlen(s:buffNum{i}))
        let buffName = s:buffName{i} . s:RepeatString(' ', maxNameLen - strlen(s:buffName{i}))
        let buffDir = s:buffDir{i} . s:RepeatString(' ', maxDirLen - strlen(s:buffDir{i}))
        let buffPath = s:buffPath{i} . s:RepeatString(' ', maxPathLen - strlen(s:buffPath{i}))

        let line = substitute(line, '{number}', buffNum, '')
        let line = substitute(line, '{name}', escape(buffName, ' \'), '')
        let line = substitute(line, '{dir}', escape(buffDir, ' \'), '')
        let line = substitute(line, '{path}', escape(buffPath, ' \'), '')
        let line = substitute(line, '{+}', modChar, '')
        let line = substitute(line, '{%}', buffChar, '')
        let line = substitute(line, '{h}', hiddenChar, '')

        call setline(i + 1, line)

        let i = i + 1
    endwhile

    if stridx(g:TBE_showMethod, 'v') == -1
        execute s:buffCount . 'wincmd _'
    else
        execute g:TBE_windowWidth . 'wincmd |'
    endif

    if currBuffLine == 0
        currBuffLine = 1
    endif
    normal gg
    execute currBuffLine
    call cursor(currBuffLine, 1)

    setlocal nomodifiable readonly

endfunction


function! s:JumpToBuffer(index)
    bdelete
    call s:OpenWindow(s:buffNum{a:index}, 'new', 0)
endfunction


function! s:AddBuffer(buffNum)
    let buffPath = expand('#' . a:buffNum . ':p')
    let buffDir = expand('#' . a:buffNum . ':p:h')
    let buffName = expand('#' . a:buffNum . ':p:t')

    " find index to insert a new element
    let index = s:getWhereToInsert(buffPath)

    " move succeeding elements to forward
    let i = s:buffCount
    while index < i
        let s:buffNum{i} = s:buffNum{i - 1}
        let s:buffPath{i} = s:buffPath{i - 1}
        let s:buffDir{i} = s:buffDir{i - 1}
        let s:buffName{i} = s:buffName{i - 1}

        let i = i - 1
    endwhile

    " insert the new element
    let s:buffNum{index} = a:buffNum
    let s:buffPath{index} = buffPath
    let s:buffDir{index} = buffDir
    let s:buffName{index} = buffName


    let s:buffCount = s:buffCount + 1

    return index
endfunction


function! s:getWhereToInsert(buffPath)
    let i = 0
    while i < s:buffCount
        if (tolower(a:buffPath) < tolower(s:buffPath{i}))
            break
        endif

        let i = i + 1
    endwhile

    return i
endfunction


function! s:DeleteBuffer() range
    let msg = 'Are you sure to delete '
    if a:firstline == a:lastline
        let msg = msg . '`' . s:buffName{a:firstline - 1} . "'"
    else
        let msg = msg . (a:lastline - a:firstline + 1) . ' buffers'
    endif
    let msg = msg . '? [y/N]'

    echo msg

    if char2nr(tolower(nr2char(getchar()))) == char2nr('y')
        setlocal modifiable

        " delete buffers
        let i = a:firstline - 1
        while i < a:lastline
            execute 'bdelete! ' . s:buffNum{i}

            let i = i + 1
        endwhile

        " delete lines corresponding with deleted buffers
        silent execute a:firstline.','.a:lastline . 'delete'

        " delete 's:buffXxx{i}'
        call s:CollectBuffers()

        if (s:buffCount == 0)
            normal q
        else
            setlocal nomodifiable
        endif
    endif
    " clear the prev echo
    normal :<Esc>
endfunction


function! s:OpenTempWindow(buffName, newMethod)
    let window = s:OpenWindow(a:buffName, a:newMethod, 1)
    setlocal nowrap nonumber buftype=nofile bufhidden=delete noswapfile
    return window
endfunction


function! s:OpenWindow(buffName, method, forceNewWindow)
    let window = bufwinnr(a:buffName)

    if window != -1
        execute window . 'wincmd w'
        return window
    endif

    if a:forceNewWindow
        execute a:method . ' ' . a:buffName
    else
        if type(a:buffName) == 0 " Number
            execute 'buffer ' . a:buffName
        else
            execute 'edit ' . a:buffName
        endif
    endif

    return winnr()
endfunction


function! s:GetMaxColumnWidth(column)
    let maxLen = 0

    let i = 0
    while i < s:buffCount
        let len = strlen(s:{a:column}{i})
        if maxLen < len
            let maxLen = len
        endif

        let i = i + 1
    endwhile

    return maxLen
endfunction


function! s:RepeatString(str, times)
    let result = ''

    let i = 0
    while i < a:times
        let result = result . a:str

        let i = i + 1
    endwhile

    return result
endfunction

" vim:ts=4:sts=4:sw=4:tw=80:et:
