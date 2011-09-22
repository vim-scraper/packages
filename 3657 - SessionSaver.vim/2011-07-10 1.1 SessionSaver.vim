"sessionSaver.vim: saves session automatically before leaving vim
"Prajjwal Devkota -- July 10, 2011
"If vim is not running with a session name, it prompts the user for one, x
"prevents saving the session.
"you can map a call to SaveSession if you want to save a session without
"waiting to exit vim, e.g.: 
"map <C-h>ss call SaveSession()<CR>

"Set this variable to wherever you want to save your sessions
let g:sessionDir="$HOME/.vim/sessions"

"I have a slightly more complicated version of the following function in my .bashrc
"So that I can directly load a session without specifying the full path:
"vis() {
"   vi -S $HOME/.vim/sessions/$1
"}
"alias vid='vis default'

"No need to modify below this line

"List all files in session directory
function! ListSessions()
    
    let outStr = "Session files in session directory ".g:sessionDir.":\n"
    
    for sessFileName in split(glob(g:sessionDir."/*"),'\n')

        let pathSplit = split(sessFileName, '/')
        let outStr = outStr.pathSplit[len(pathSplit) - 1]."\n"
        "echo pathSplit[len(pathSplit) - 1]

    endfor
    echo outStr

endfunction

"Get session name from user
function! GetSessNameFromUser()

    let sessionName = "?"

    while ( sessionName == "?")

        let sessionName = input("Session Name: (<enter>: default; x: cancel ?:list existing sessions)")

        if ( sessionName == "?")
            call ListSessions()
        endif

    endwhile

    if (sessionName == "")
        let sessionName = "default"
    endif

    return sessionName

endfunction

"Save current session
function! SaveSession()

    if (v:this_session != "")

        let sessionName=v:this_session
        let saveCmd="mksession! ".sessionName

    else

        let saveSessionName = GetSessNameFromUser()

        if (saveSessionName != "x")
            let saveSessionPath=g:sessionDir."/".saveSessionName
            let saveCmd="mksession! ".saveSessionPath
        else
            let saveCmd = "echo 'Not Saving Session'"
        endif

    endif


    exec saveCmd

endfunction

"Load existing session into vim
function! LoadSession()

    if (v:this_session != "")
        echo "Already have a session: ".v:this_session." loaded... make sure everything loads fine."
    endif

    let loadSessionName = GetSessNameFromUser()
    if (loadSessionName != "x")
        echo "Loading session ".loadSessionName
        let cmd="source ".g:sessionDir."/".loadSessionName
        exec cmd
    endif
endfunction

"called during startup of vim to allow user choice of loading session
function AutoLoadSession()

    if ((argc() == 0) && (v:this_session == ""))
        call LoadSession()
    endif

endfunction

"call AutoLoadSession if necessary when starting up vim
au VimEnter * call AutoLoadSession()

"call SaveSession before exiting vim
autocmd VimLeavePre * call SaveSession()
