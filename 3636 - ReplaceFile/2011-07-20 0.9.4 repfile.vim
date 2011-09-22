" repfile.vim: file replacer

" Doc

" Installation: put repfile.vim in your .vim/plugin/ and restart vim
" If you'd like to remap the function call, put following in your .vimrc
"       let g:RepCall="yourmapping"

" Description
" Utility for save files under new names in vim with automatically cleaning up the old file's buffer and hard disk version.

" Usage:
" Call the function by one of the methods below. Insert the desired new /optional/newpath/NewName and press <Enter>.

    "- the current file is saved under /optional/path/NewName
    "- a buffer for NewName opens
    "- the old file's buffer is closed
    "- the old file is deleted (if was written to disk)

" Method 1 (preferred):
"    <leader>R
" brings up the function call with the current file's full path.

" Method 2:
"    :Repfile
" will operate from Vim's CWD, not with the buffers directory.

" Mapping:
" The default mapping is:
"    <leader>R

" If you want to change that, put
"    let g:RepCall="yourmapping"
" in your vimrc.

" check if you set a custom mapping for function call
if !exists("g:RepCall")
    let g:RepCall="<leader>R"
endif

" map the function to default mapping or to mapping from .vimrc
exec "map " . g:RepCall . " :RepFile <c-r>=expand('%:p:h')<cr>/"

function! RepFile(newfile)
    " set vars
    let oldfile=expand("%")
    let newfile=a:newfile

    silent execute 'w ' . newfile

    " check for BufKill support:
    if !exists('g:BufKillCommandWhenLastBufferKilled')
        bw
    else
        " delete buffer but keep it's window (BufKill feature)
        BW
    endif

    " check if the current buffer holds a file from disk, if so, delete it.
    if filereadable(oldfile) == 1
        silent execute '!rm ' . oldfile
    endif

    " open the new instance of file
    silent execute 'e ' . newfile
endfunction

" beautifies the cmd-line function call
command! -bang -nargs=* -complete=file RepFile call RepFile(<q-args>)
