" File: right_align.vim
" Author: Alexey Radkov
" Description: A function to set right indentation, it can be useful in insert
"              mode along with ^T, ^D and ^F
" Usage:
"   Command :RightAlign to align current line to a right border, the optional
"   argument indicates that position of the cursor must be kept
"   Global variable g:RightBorder will be used as the right border value, if
"   not set then value of textwidth option will be used instead
"   Recommended mappings are
"       imap <silent> <C-b>  <Plug>RightAlign
"       nmap <silent> <C-m>b :RightAlign<CR>


if exists('right_align_plugin')
    finish
endif

let right_align_plugin = 1

if exists('g:RightBorder')
    let s:RightBorder = g:RightBorder
else
    let s:RightBorder = &textwidth
endif

function! <SID>right_align(right_border, ...)
    if getline('.') =~ '^[[:blank:]]*$'
        return ''
    endif
    let indent_cmd = ''
    let move_left = 0
    let tab_width = &et ? &sw : 1
    let keep_cursor = a:0 > 0 && a:1 == 'kc'
    let save_cursor = getpos('.')
    let move_cursor = 0
    let line_length = virtcol('$') - 1
    let line_diff = a:right_border - line_length
    let move_count = line_diff / &sw
    normal ^
    let start_pos = virtcol('.') - 1
    let start_shift = start_pos % &sw
    let end_shift = line_diff % &sw
    call setpos('.', save_cursor)
    " restore cursor position after undo (see Tip 1595 in vim.wikia.com)
    if keep_cursor
        normal ix
        normal x
    endif
    if &shiftround && start_shift > 0
        if line_diff >= 0
            if &sw - start_shift <= line_diff % &sw
                let move_count += 1
                let move_cursor = start_shift - tab_width
            else
                if line_diff < &sw
                    let move_left = 1
                    let move_count = 1
                    let move_cursor = start_shift - 2 * tab_width
                else
                    let move_cursor = -start_shift
                endif
            endif
        else
            if start_shift < abs(end_shift)
                let move_count -= 1
            endif
        endif
    else
        let start_shift = 0
    endif
    if line_diff < 0 && ( start_shift != 0 || end_shift != 0 )
        let move_count -= 1
        let move_cursor -= start_shift
        if start_shift != 0
            let move_cursor += tab_width
        endif
    endif
    let move_cursor += move_count * tab_width
    if move_cursor < 0 && start_pos < -move_cursor
        let move_cursor = -start_pos
    endif
    let save_cursor[2] += move_cursor
    if line_diff < 0
        let move_left = 1
        let move_count = -move_count
    endif
    for i in range(1, move_count)
        let indent_cmd .= move_left ? '<' : '>'
    endfor
    if !empty('indent_cmd')
        exe indent_cmd
    endif
    if keep_cursor
        call setpos('.', save_cursor)
    endif
    return ''
endfunction

command! -nargs=* RightAlign    call s:right_align(s:RightBorder, <f-args>)

imap <silent> <Plug>RightAlign  <C-r>=<SID>right_align(g:RightBorder, 'kc')<CR>

