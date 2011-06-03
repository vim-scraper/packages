" Author: Cooper
" Version: 1.0
" Mail: yegong1985@gmail.com
" Last Change: 2008-01-30 02:05:12

if !exists("g:use_timestamp")
    let g:use_timestamp = 1
endif

au BufWritePre *vimrc,*.vim       call Timestamp('"', '')
au BufWritePre *.cpp,*.cc,*.hpp   call Timestamp('//', '')
au BufWritePre *.java             call Timestamp('//', '')
au BufWritePre *.c,*.h            call Timestamp('/*', '*/')
au BufWritePre Makefile           call Timestamp('#', '')
au BufWritePre *.rb,*.py          call Timestamp('#', '')

function! Timestamp(left, right)
    if (!g:use_timestamp)
	return
    endif
    if (a:left != '')
	let left = a:left.' '
    else
	return
    endif
    if (a:right != '')
	let right = ' '.a:right
    else
	let right = ''
    endif
    let regex_char = '\([\*\\\/]\)'
    let s_left = substitute(left, regex_char, '\\\1', 'g')
    let s_right = substitute(right, regex_char, '\\\1', 'g')
    let time_format = '\d\{4}-\d\{2}-\d\{2} \d\{2}:\d\{2}:\d\{2}'
    let pattern = '\('.s_left.'Last Change:\s\+\)'.time_format.'\('.s_right.'\)'

    let row = search('\%^\_.\{-}\(^\zs'.pattern.'$\)', 'n')
    let now = strftime('%Y-%m-%d %H:%M:%S')
    if row != 0 "find
        let new_row_str =substitute(getline(row), '^'.pattern , '\1'.now.'\2', '')
        call setline(row, new_row_str)
    else "didn't find
        normal m`
        silent! :1
        normal O
	let new_row_str = left . 'Last Change: ' . now . right
        call setline(1, new_row_str)
        normal ``
    endif
endfunction

