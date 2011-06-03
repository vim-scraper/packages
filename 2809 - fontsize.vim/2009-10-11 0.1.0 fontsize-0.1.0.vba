" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/fontsize.vim	[[[1
66
" Plugin for modifying guifont size.
" Maintainer:   Michael Henry (vim at drmikehenry.com)
" License:      This file is placed in the public domain.

if exists("loaded_fontsize")
    finish
endif
let loaded_fontsize = 1

" Save 'cpoptions' and set Vim default to enable line continuations.
let s:save_cpoptions = &cpoptions
set cpoptions&vim

if ! hasmapto("<Plug>FontsizeInc", "n")
    nmap <silent> <Leader>++  <Plug>FontsizeInc
    nmap <silent> <Leader>==  <Plug>FontsizeInc
endif

if ! hasmapto("<Plug>FontsizeDec", "n")
    nmap <silent> <Leader>--  <Plug>FontsizeDec
endif

if ! hasmapto("<Plug>FontsizeDefault", "n")
    nmap <silent> <Leader>00  <Plug>FontsizeDefault
endif

" Mappings using <SID>m_ are inspired by the bufmru.vim plugin.
" The concept is to enter a "mode" via an initial mapping.  Once
" in this mode, some mode-specific keystrokes now behave as if they
" were mapped.  After 'timeoutlen' milliseconds have elapsed, the
" new "mode" times out and the new "mappings" are effectively disabled.
"
" This emulation of a "mode" is accomplished via a clever techinque
" wherein each operation terminates with a partial mapping to <SID>m_.
" Each new keystroke completes a mapping that itself terminates with
" <SID>m_, keeping an extensible chain of mappings going as long as
" they arrive before 'timeoutlen' milliseconds elapses between keystrokes.

nmap <silent> <Plug>FontsizeInc       <SID>m_+
nmap <silent> <Plug>FontsizeDec       <SID>m_-
nmap <silent> <Plug>FontsizeDefault   <SID>m_0

nmap <silent> <SID>m_+        <SID>inc<SID>m_
nmap <silent> <SID>m_=        <SID>inc<SID>m_
nmap <silent> <SID>m_-        <SID>dec<SID>m_
nmap <silent> <SID>m_0        <SID>default<SID>m_
nmap <silent> <SID>m_!        <SID>setDefault<SID>m_
nmap <silent> <SID>m_q        <SID>quit
nmap <silent> <SID>m_<SPACE>  <SID>quit
nmap <silent> <SID>m_<CR>     <SID>quit
nmap <silent> <SID>m_         <SID>quit

nnoremap <silent> <SID>inc         :call fontsize#inc()<CR>
nnoremap <silent> <SID>dec         :call fontsize#dec()<CR>
nnoremap <silent> <SID>default     :call fontsize#default()<CR>
nnoremap <silent> <SID>setDefault  :call fontsize#setDefault()<CR>
nnoremap <silent> <SID>quit        :call fontsize#quit()<CR>

command! FontsizeInc        call fontsize#inc()
command! FontsizeDec        call fontsize#dec()
command! FontsizeDefault    call fontsize#default()
command! FontsizeSetDefault call fontsize#setDefault()

" Restore saved 'cpoptions'.
let cpoptions = s:save_cpoptions
" vim: sts=4 sw=4 tw=80 et ai:
autoload/fontsize.vim	[[[1
65
" Autoload portion of plugin/fontsize.vim.
" Maintainer:   Michael Henry (vim at drmikehenry.com)
" License:      This file is placed in the public domain.

function! fontsize#get()
    let s:font = &guifont
    if has("gui_win32")
        let size = substitute(s:font, '.*:h\(\d\+\):.*', '\1', '')
    elseif has("gui_gtk2")
        let size = substitute(s:font, '.*\D\(\d\+\)', '\1', '')
    else
        let size = "0"
    endif
    " Convert to integer.
    return size + 0
endfunction

function! fontsize#set(size)
    let s:font = &guifont
    if has("gui_win32")
        let newFont = substitute(s:font, '\(.*:h\)\d\+\(:.*\)', 
                    \ '\1' . a:size . '\2', '')
    elseif has("gui_gtk2")
        let newFont = substitute(s:font, '\(.*\D\)\d\+', '\1' . a:size, '')
    else
        let newFont = &guifont
    endif
    let &guifont = newFont
endfunction

function! fontsize#display()
    redraw
    sleep 100m
    echo &guifont . " (+/= - 0 ! q CR SP)"
endfunction

function! fontsize#quit()
    echo &guifont . " (Done)"
endfunction

function! fontsize#inc()
    call fontsize#set(fontsize#get() + 1)
    call fontsize#display()
endfunction

function! fontsize#dec()
    let oldSize = fontsize#get()
    if oldSize > 1
        call fontsize#set(oldSize - 1)
    endif
    call fontsize#display()
endfunction

let fontsize#defaultSize = fontsize#get()

function! fontsize#default()
    call fontsize#set(g:fontsize#defaultSize)
    call fontsize#display()
endfunction

function! fontsize#setDefault()
    let g:fontsize#defaultSize = fontsize#get()
endfunction

" vim: sts=4 sw=4 tw=80 et ai:
doc/fontsize.txt	[[[1
104
*fontsize.txt*   Plugin for modifying guifont size

*fontsize*       Version 0.1.0          Last Change:  October 11, 2009

==============================================================================
1.  Introduction                                    |fontsize-intro|
2.  Installation                                    |fontsize-installation|
3.  Customization                                   |fontsize-customization|
4.  Credits                                         |fontsize-credits|
5.  Distribution                                    |fontsize-distribution|

==============================================================================
1.  Introduction                                    *fontsize-intro*

This plugin provides convenient mappings for changing the font size in Gvim.

  <Leader>++    Increment font size
  <Leader>==    Increment font size
  <Leader>--    Decrement font size
  <Leader>00    Reset to default font size

The above mappings initiate a "font size" mode in which the following
additional individual keys become active:

  +          Increment font size
  =          Increment font size
  -          Decrement font size
  0          Revert to default font size
  !          Save current size as new default
  q          Quit "font size" mode
  <SPACE>    Quit "font size" mode
  <CR>       Quit "font size" mode

Other keys pressed will exit "font size" mode and perform their normal
function.

In addition, "font size" mode will automatically timeout after |timeoutlen|
milliseconds have elapsed without a keypress, because "font size" mode is
based on mappings.

The default value of |timeoutlen| is 1000 milliseconds (1 second), which might
be too fast.  I'm using the following in my |vimrc|: >

  " Slow down mapping timeout from default 1000 milliseconds.
  set timeoutlen=3000

===============================================================================
2.  Installation                                    *fontsize-installation*

Must be installed using the Vimball plugin, found here:
http://vim.sourceforge.net/scripts/script.php?script_id=1502

Open fontsize-x.y.z.vba with Vim: >

  vim fontsize-x.y.z.vba

With the Vimball open in Vim, extract the files with the :source command: >

  :source %

===============================================================================
3.  Customization                                   *fontsize-customization*

You may change the mappings that initiate "font size" mode by creating
your own mappings in your |vimrc|file.  For example, use these mappings
to use single characters instead of doubled ones:

  nmap <silent> \+  <Plug>FontsizeInc
  nmap <silent> \=  <Plug>FontsizeInc
  nmap <silent> \-  <Plug>FontsizeDec
  nmap <silent> \0  <Plug>FontsizeDefault

===============================================================================
4.  Credits                                         |fontsize-credits|

Author: Michael Henry <vim at drmikehenry.com>

Thanks to all the tireless posters on the Vim mailing lists.  I have
benefitted greatly from the detailed and helpful postings contributed daily
by the helpful Vim community.

Thanks also to Andy Wokula, author of the bufmru plugin
(http://www.vim.org/scripts/script.php?script_id=2346),
for writing a clever plugin from which I learned to use
chained keymaps to implement "modes".

===============================================================================
5.  Distribution                                    |fontsize-distribution|

- Ensure version and date are correct at top of doc/fontsize.txt.
- Visually select the following lines:

plugin/fontsize.vim
autoload/fontsize.vim
doc/fontsize.txt

- Create Vimball based on version number (e.g., 0.1.0) as follows: >

  :MkVimball! fontsize-0.1.0

- Distribute fontsize-0.1.0.vba.

===============================================================================
vim:sts=2:et:ai:tw=78:fo=tcq2:ft=help:
