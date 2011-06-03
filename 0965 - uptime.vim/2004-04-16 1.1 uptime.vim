" Name:          uptime (global plugin)
" Version:       1.1
" Author:        Ciaran McCreesh <ciaranm at gentoo.org>
" Updates:       http://dev.gentoo.org/~ciaranm/vim/
" Purpose:       Display vim uptime
"
" License:       You may redistribute this plugin under the same terms as Vim
"                itself.
"
" Usage:         :Uptime
"
" Requirements:  Untested on Vim versions below 6.2
"
" ChangeLog:
"     v1.1 (20040417)
"         * pretty times
"
"     v1.0 (20040417)
"         * initial release

" when the plugin is sourced, record the time
let s:start_time = localtime()

" how long have we been up?
function! Uptime()
    let l:current_time = localtime()
    let l:uptime_in_seconds = l:current_time - s:start_time

    " get days, hours, minutes, seconds
    let l:m_s = (l:uptime_in_seconds) % 60
    let l:m_m = (l:uptime_in_seconds / 60) % 60
    let l:m_h = (l:uptime_in_seconds / (60 * 60)) % 24
    let l:m_d = (l:uptime_in_seconds / (60 * 60 * 24))

    " make it look purdy
    let l:d = 0
    let l:msg = v:progname . strftime(" has been up for ")

    if l:d || (l:m_d > 0)
        let l:msg = l:msg . l:m_d . (l:m_d == 1 ? " day, " : " days, ")
        let l:d = 1
    endif

    if l:d || (l:m_h > 0)
        let l:msg = l:msg . l:m_h . (l:m_h == 1 ? " hour, " : " hours, ")
        let l:d = 1
    endif

    if l:d || (l:m_m > 0)
        let l:msg = l:msg . l:m_m . (l:m_m == 1 ? " minute" : " minutes")
                    \ . " and "
        let l:d = 1
    endif

    let l:msg = l:msg . l:m_s . (l:m_s == 1 ? " second " : " seconds ")

    " echo it
    echo l:msg
endfun

command! -nargs=0 Uptime :call Uptime()

" vim: set tw=80 ts=4 et :
