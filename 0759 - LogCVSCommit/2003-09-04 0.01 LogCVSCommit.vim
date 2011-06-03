" Script to log CVS commits
"
" Travis Hume
" Thu Sep  4 15:10:57 2003
" v0.01
"
" I use this bit of vimscript to edit CVS commit messages.
" The message, including the affected files, is timestamped and
" written to a log file.  The log entry format looks like:
" ________________________________________________________________________________
" Thu Sep  4 09:30:44 PDT 2003
" ----------------------------
" ISSUE00007620 -- resolved
" Modified Files:
"  Tag: TEST_11_I6
" 	grounddb/create/update_gcsapp_schema.sql 
" 	grounddb/create/gcsapp/table/ans.sql
"
"
" You'll need to set a shell environment variable like this:
" export CVSEDITOR='gvim -f +LogCVSCommit'

" change this to point to your own log file
let log_file="~/cvs.log"

command! -complete=command LogCVSCommit call LogCVSCommit()

function! LogCVSCommit()
    augroup LogCVS
    autocmd LogCVS BufUnload * call FinishLogCVSCommit()

    silent! g/CVS: \(-\+\|\|Enter Log.\+\|Committing in.\+\)$/d
    silent! %s/^CVS: //

    normal 1GO
endfunction

function! FinishLogCVSCommit()
    %yank
    new +set\ buftype=nofile
    call append(0, "________________________________________________________________________________")
    call append(1, strftime("%c"))
    call append(2, "------------------------")
    3put
    $del
    execute "write! >> ".g:log_file

    autocmd! LogCVS
    augroup! LogCVS
endfunction

