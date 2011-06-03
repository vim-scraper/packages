" perforce.vim: Interface with p4 command.
" Author: Hari Krishna <hari_vim@yahoo.com>
" Last Modified: 08-Mar-2002 @ 15:39:26
" Created:       not sure, but sometime before 20-Apr-2001
" Requires: Vim-6.0 or higher, genutils.vim(1.0.15), multvals.vim(2.1.2)
" Version: 1.1.8
" Usage: 
"   Adds commands and menus (if enabled) to execute perforce commands. There
"     are commands defined for most used perforce commands such as 'edit' (PE),
"     'opened' (PO) etc. The generic PF command lets you execute any arbitrary
"     perforce command, and opens a new window with the output of that
"     command. But don't execute any commands that require you to enter input
"     (such as 'submit'). There are separate commands defined for such
"     operations, such as PSubmit for 'submit' and PC for 'client'.
"   Most commands take variable number of arguments. Some commands that
"     require a filename default to current file if you didn't pass any. If
"     you need to specify a version number, then protect the '#' symbol with
"     a backslash.
"   When you are in a list view, such as "PF labels" or "PO", you can
"     press <Enter> to view the current item in a preview window and O to edit
"     it. You can also press D to delete the current item, when it is
"     applicable. You can also use the PDescribeItem, PEditItem, or the
"     PDeleteItem commands.
"   Set the g:p4Client, g:p4User, g:p4Port and g:p4CodelineRoot variables in
"     your .vimrc, or some defaults will be chosen. 
"   If you want to switch to a different perforce server, or just switch to a
"     different client, without leaving Vim or needing to change any environment
"     variables, then you can use the PSwitch command (or the Settings menu).
"     You can set the most used configurations as comma separated list of
"     "{port} {client} {user}" to the g:p4Presets variable and specify the
"     index (starting from 0) to this command.
"   When you want to change any of the script options without restarting Vim,
"     you can use the PFInitialize to reinitialize the script from the
"     environmental variables.  You can e.g., disable menus by setting the
"     value of g:p4EnableMenu to 0 and run PFInitialize.
"   You can also set g:p4DefaultOptions to options that are specified in the
"     "p4 help usage", so that these options are always passed to p4 command.
"   To enable menus, set the g:p4EnableMenu and/or g:p4EnablePopupMenu as per
"     your taste. You can also set the g:p4UseExpandedMenu to enable more
"     complete menus, on the lines of p4Win. By default, a basic menu is
"     created. There are also g:p4EnablePopupMenu and g:p4UseExpandedPopupMenu
"     options to create the Perforce PopUp menu group.
"   If you are manually sourcing the scripts either from vimrc or from
"     commandline, make sure to source multvals.vim and genutils.vim first.
"
" Environment: 
"   Adds
"       PE (edit), PR (revert), PA (add), PD (diff), PD2 (diff2),
"       PP (print), PG (get), PO (opened), PH (help),
"       PDelete (delete), PLock (lock), PSubmit (submit), PUnlock (unlock),
"       PClient (client), PUser (user), PBranch (branch), PLabel (label),
"       PSync (sync), PJob (job), PResolve (resolve), PChange (change),
"       PDepot (depot),
"       PF & PFRaw (for generic command execution),
"       E (command to open a file from a different codeline),
"       PSwitch (command to switch between different port/client/user settings),
"       PRefreshActivePane (to refresh the current p4 window),
"       PDescribeItem, PEditItem and PDeleteItem to operate on the list views.
"     commands. 
"   Adds 
"       O (for open/edit), <CR> and <2-LeftMouse> (for describe), D (for delete)
"     normal-mode mappings, only in the relevant p4 windows.
"   Adds 
"       Perforce
"     menu group in the main and Popup menus if enabled.
"   Depends on
"       g:p4CmdPath, g:p4CodelineRoot, g:p4Client, g:p4User, g:p4Port,
"       g:p4DefaultOptions, g:p4UseGUIDialogs, g:p4EnableMenu,
"       g:p4EnablePopupMenu, g:p4PromptToCheckout, g:p4UseExpandedMenu,
"       g:p4UseExpandedPopupMenu
"     Environmental variables. 
"     
"
" TODO:
"   How can I support interactive resolves? Will it be worth doing it? 
"   Support more operations on the displayed files (like a file browser).
"   Better file history viewer, including diff operations.
"   Define a syntax file for perforce syntax. May be one for the help window
"     too to highlight the perforce help keywords.
"   How can I avoid protecting the # on the command line while giving the
"     version number ???
"   How can I avoid prompting for checkout when the current vim session is in
"     view mode (-R option) ???
"   Allow file names to be passed into PSubmit command. 
"   The PSwitch can't change the p4CodelineRoot, it is best to read this from
"     'info' after reinitialization.
"   Set some decent options to edit text, such as ts=8. 
"   The script is not much intelligent wrt to obtaining the settings from p4.
"     E.g., it assumes that the local directory name is same as the branch
"     name.
"   You can't pass in arguments to p4 while using the commands such as PUser.
"   Shouldn't enable delete on the filelog and opened windows. 
"   Something to enable/disable and switch between basic and expanded menus
"     will be good.
"   The list specific menus should be disabled unless you are in that window. 
"   Unified approach to set settings for both preview and edit modes. Now I
"     have to check for outputType == 1 and do a wincmd p, once before and
"     another after.
"   The other commands such as files that list out files, should also have the
"     same features as the opened command. 
"   Verify that the autocommands are not leaking.

if exists("loaded_perforce")
  finish
endif
let loaded_perforce=1


" We need these scripts at the time of initialization itself.
if !exists("loaded_genutils")
  runtime plugin/genutils.vim
endif
if !exists("loaded_multvals")
  runtime plugin/multvals.vim
endif


command! -nargs=0 PFInitialize :call <SID>Initialize()

function! s:Initialize()

if exists("g:p4CmdPath")
  let s:p4CmdPath = g:p4CmdPath
  unlet g:p4CmdPath
elseif !exists("s:p4CmdPath")
  let s:p4CmdPath = "p4"
endif

if exists("g:p4CodelineRoot")
  let s:codelineRoot=g:p4CodelineRoot
  unlet g:p4CodelineRoot
elseif !exists("s:codelineRoot")
  let s:codelineRoot=fnamemodify(".", ":p")
endif

if exists("g:p4Client")
  let s:p4Client = g:p4Client
  unlet g:p4Client
elseif !exists("s:p4Client")
  let s:p4Client = $P4CLIENT
endif

if exists("g:p4User")
  let s:p4User = g:p4User
  unlet g:p4User
elseif !exists("s:p4User")
  if OnMS() && exists("$USERNAME")
    let s:p4User = $USERNAME
  elseif exists("$LOGNAME")
    let s:p4User = $LOGNAME
  endif
endif

if exists("g:p4Port")
  let s:p4Port = g:p4Port
  unlet g:p4Port
elseif !exists("s:p4Port")
  let s:p4Port = $P4PORT
endif

if exists("g:p4Presets")
  let s:p4Presets = g:p4Presets
  unlet g:p4Presets
elseif !exists("s:p4Presets")
  let s:p4Presets = ""
endif

if exists("g:p4DefaultOptions")
  let s:p4DefaultOptions = g:p4DefaultOptions
  unlet g:p4DefaultOptions
elseif !exists("s:p4DefaultOptions")
  let s:p4DefaultOptions = ""
endif

" Normally, we use consolve dialogs even in gvim, which has the advantage of
"   having an history and expression register. But if you rather prefer GUI
"   dialogs, then set this variable.
if exists("g:p4UseGUIDialogs")
  let s:useDialogs = g:p4UseGUIDialogs
  unlet g:p4UseGUIDialogs
elseif !exists("s:useDialogs")
  let s:useDialogs = 0
endif

if exists("g:p4PromptToCheckout")
  let s:promptToCheckout = g:p4PromptToCheckout
  unlet g:p4PromptToCheckout
elseif !exists("s:promptToCheckout")
  let s:promptToCheckout = 1
endif

" If the client, user and port are available, and if they are not specified in
" the p4DefaultOptions, then add them.
let s:defaultOptions = s:p4DefaultOptions
if s:p4Client != "" && (match(s:p4DefaultOptions, '-c\>') == -1)
  let s:defaultOptions = s:defaultOptions . " -c " . s:p4Client
endif
if s:p4User != "" && (match(s:p4DefaultOptions, '-u\>') == -1)
  let s:defaultOptions = s:defaultOptions . " -u " . s:p4User
endif
if s:p4Port != "" && (match(s:p4DefaultOptions, '-p\>') == -1)
  let s:defaultOptions = s:defaultOptions . " -p " . s:p4Port
endif

""" The following are some shortcut commands.

" Equivalent to: p4 print %
command! -nargs=* -complete=file PP :call <SID>PPrint(0, <f-args>)
" Equivalent to: p4 diff %. You can pass in arguments to diff and a filename.
command! -nargs=* -complete=file PD :call <SID>PDiff(0, <f-args>)
" Equivalent to: p4 edit %
command! -nargs=* -complete=file PE :call <SID>PEdit(2, <f-args>)
" Equivalent to: p4 add %
command! -nargs=* -complete=file PA :call <SID>PAdd(2, <f-args>)
" Equivalent to: p4 delete %
command! -nargs=* -complete=file PDelete :call <SID>PDelete(2, <f-args>)
" Equivalent to: p4 lock %
command! -nargs=* -complete=file PLock :call <SID>PLock(2, <f-args>)
" Equivalent to: p4 unlock %
command! -nargs=* -complete=file PUnlock :call <SID>PUnlock(2, <f-args>)
" Equivalent to: p4 revert %
command! -nargs=* -complete=file PR :call <SID>PRevert(2, <f-args>)
" Equivalent to: p4 get/sync %
command! -nargs=* -complete=file PG :call <SID>PSync(<f-args>)
command! -nargs=* -complete=file PSync :call <SID>PSync(2, <f-args>)
" Equivalent to: p4 opened
command! -nargs=* PO :call <SID>POpened(0, <f-args>)
" Equivalent to: p4 diff2 %. You will be prompted for the two revisions. You
"   can pass in arguments diff2, but not a filename. The current filename is
"   always assumed.
command! -nargs=* PD2 :call <SID>PDiff2(0, <f-args>)
" Same as: p4 help. You can drill down the help by pressing <Enter>
command! -nargs=* PH :call <SID>PHelp(0, <f-args>)

""" The following support some p4 operations that normally involve some
"""   interaction with the user (they are more than just shortcuts).

" Same as: p4 change. You can edit and save change spec. by using :W command.
command! -nargs=* PChange :call <SID>PChange(0, <f-args>)
" Same as: p4 branch. You can edit and save branch spec. by using :W command.
command! -nargs=* PBranch :call <SID>PBranch(0, <f-args>)
" Same as: p4 label. You can edit and save label spec. by using :W command.
command! -nargs=* PLabel :call <SID>PLabel(0, <f-args>)
" Same as: p4 client. You can edit and save client spec. by using :W command.
command! -nargs=* PClient :call <SID>PClient(0, <f-args>)
" Same as: p4 user. You can edit and save user spec. by using :W command.
command! -nargs=* PUser :call <SID>PUser(0, <f-args>)
" Same as: p4 job. You can edit and save job spec. by using :W command.
command! -nargs=* PJob :call <SID>PJob(0, <f-args>)
" Same as: p4 depot. You can edit and save depot spec. by using :W command.
command! -nargs=* PDepot :call <SID>PDepot(0, <f-args>)
" Generates a template for p4 submit. You can edit and submit using the :W
"   command.
command! -nargs=0 PSubmit :call <SID>PSubmit(0)
" Currently just a shortcut for "p4 resolve", but hope to implement something
" better.
command! -nargs=* PResolve :call <SID>PResolve(0, <f-args>)

""" Other utility commands.

" E <codeline> [files: default %]
" You can open a file that you are viewing from a different codeline by using
" this command. You can specify more than one file in which case the first one
" is still opened, but the remaining files are just added to the buffer list.
command! -nargs=* -complete=file E :call <SID>PFOpenAltFile(<f-args>)
" No args: Print presets and prompt user to select a preset.
" Number: Select that numbered preset (index starts from 0). 
" Usage: PSwitch port [client] [user]
command! -nargs=* PSwitch :call <SID>PSwitch(<f-args>)
" Refresh the active pane.
command! -nargs=0 PRefreshActivePane :call <SID>PRefreshActivePane()
" You can specify any p4 command using this command. The command is executed
" and the output is placed in a new window. But don't run the commands that
" require user input such as "p4 submit".
command! -nargs=* -complete=file PF :call <SID>PFIF(0, 0, 0, <f-args>)
" Same as PF, but a raw output is generated.
command! -nargs=* -complete=file PFRaw :call <SID>PFRaw(0, <f-args>)
" Write the current file contents as input into the specified p4 command. You
" can specify a range.
command! -nargs=* -complete=file -range=% PW
    \ :<line1>,<line2>call <SID>PW(<f-args>)
command! -nargs=* W :echohl WarningMsg |
    \ echo "Use PW if you want to write current buffer into a perforce command"
    \ | echohl NONE

let s:changesExpr  = "matchstr(getline(\".\"), " . "'" . '^Change \zs\d\+\ze ' . "'" . ")"
let s:branchesExpr = "matchstr(getline(\".\"), " . "'" . '^Branch \zs[^ ]\+\ze ' . "'" . ")"
let s:labelsExpr   = "matchstr(getline(\".\"), " . "'" . '^Label \zs[^ ]\+\ze ' . "'" . ")"
let s:clientsExpr  = "matchstr(getline(\".\"), " . "'" . '^Client \zs[^ ]\+\ze ' . "'" . ")"
let s:usersExpr    = "matchstr(getline(\".\"), " . "'" . '^[^ ]\+\ze <[^@>]\+@[^>]\+> ([^)]\+)' . "'" . ")"
let s:jobsExpr     = "matchstr(getline(\".\"), " . "'" . '^[^ ]\+\ze on ' . "'" . ")"
let s:depotsExpr   = "matchstr(getline(\".\"), " . "'" . '^Depot \zs[^ ]\+\ze ' . "'" . ")"
let s:openedExpr   = "s:ConvertToLocalPath(expand('<cfile>'))"
let s:filelogExpr  = "s:GetCurrentDepotFile()"

" If an explicit handler is defined, then it will override the default rule of
" finding the command with the singular form.
let s:filelogItemHandler = "s:PPrint"
let s:openedItemHandler = "s:OpenFile"



function! s:CreateMenu(sub, expanded)

  if ! a:expanded
    let fileGroup = '.'
  else
    let fileGroup = '.&File.'
  endif
  exec 'amenu <silent> ' . a:sub . '&Perforce' . fileGroup . '&Add :PA<CR>'
  exec 'amenu <silent> ' . a:sub . '&Perforce' . fileGroup . 'S&ync :PSync<CR>'
  exec 'amenu <silent> ' . a:sub . '&Perforce' . fileGroup . '&Edit :PE<CR>'
  exec 'amenu <silent> ' . a:sub . '&Perforce' . fileGroup . '-Sep1- :'
  exec 'amenu <silent> ' . a:sub . '&Perforce' . fileGroup .
      \ '&Delete :PDelete<CR>'
  exec 'amenu <silent> ' . a:sub . '&Perforce' . fileGroup . '&Revert :PR<CR>'
  exec 'amenu <silent> ' . a:sub . '&Perforce' . fileGroup . '-Sep2- :'
  exec 'amenu <silent> ' . a:sub . '&Perforce' . fileGroup . 'Loc&k :PLock<CR>'
  exec 'amenu <silent> ' . a:sub . '&Perforce' . fileGroup .
      \ 'U&nlock :PUnlock<CR>'
  exec 'amenu <silent> ' . a:sub . '&Perforce' . fileGroup . '-Sep3- :'
  exec 'amenu <silent> ' . a:sub . '&Perforce' . fileGroup . '&Diff :PD<CR>'
  exec 'amenu <silent> ' . a:sub . '&Perforce' . fileGroup . 'Diff&2 :PD2<CR>'
  exec 'amenu <silent> ' . a:sub . '&Perforce' . fileGroup .
      \ 'Revision\ &History :PF filelog %<CR>'
  exec 'amenu <silent> ' . a:sub . '&Perforce' . fileGroup . '&Print :PP<CR>'
  exec 'amenu <silent> ' . a:sub . '&Perforce' . fileGroup . '-Sep4- :'
  " TODO: Properties.
  if a:expanded
    exec 'amenu <silent> ' . a:sub . '&Perforce.&File.' .
          \ 'Resol&ve.Accept\ &Their\ Changes<Tab>resolve\ -at ' .
          \ ':PResolve -at<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&File.' .
          \ 'Resol&ve.Accept\ &Your\ Changes<Tab>resolve\ -ay :PResolve -ay<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&File.' .
          \ 'Resol&ve.&Automatic\ Resolve<Tab>resolve\ -am :PResolve -am<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&File.' .
          \ 'Resol&ve.&Safe\ Resolve<Tab>resolve\ -as :PResolve -as<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&File.' .
          \ 'Resol&ve.&Force\ Resolve<Tab>resolve\ -af :PResolve -af<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&File.' .
          \ 'Resol&ve.S&how\ Integrations<Tab>resolve\ -n :PResolve -n<CR>'
  endif

  if ! a:expanded
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Opened\ Files :PO<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Refresh\ Active\ Pane :PRefreshActivePane<CR>'
  exec 'amenu <silent> ' . a:sub . '&Perforce.-Sep5- :'
  else
    exec 'amenu <silent> ' . a:sub .
        \ '&Perforce.&View.&BranchSpecs :PF branches<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&View.&Changelist.' .
        \ '&Pending\ Changelists :PF changes -s pending<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&View.&Changelist.' .
        \ '&Submitted\ Changelists :PF changes -s submitted<CR>'
    exec 'amenu <silent> ' . a:sub .
        \ '&Perforce.&View.Cl&ientSpecs :PF clients<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&View.&Jobs :PF jobs<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&View.&Labels :PF labels<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&View.&Users :PF users<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&View.&Depots :PF depots<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&View.&Opened\ Files :PO<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&View.&Refresh\ Active\ Pane :PRefreshActivePane<CR>'
  endif

  if a:expanded
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Settings.' .
        \ '&Switch\ Port\ Client\ User :call <SID>SwitchPortClientUser()<CR>'
    let nSets = MvNumberOfElements(s:p4Presets, ',')
    if nSets > 0
      let index = 0
      while index < nSets
        let nextSet = MvElementAt(s:p4Presets, ',', index)
        exec 'amenu <silent> ' . a:sub . '&Perforce.&Settings.&' . index . '\ '
            \ . escape(nextSet, ' ') . ' :PSwitch ' . index . '<CR>'
        let index = index + 1
      endwhile
    endif
  endif

  if ! a:expanded
    exec 'amenu <silent> ' . a:sub .
        \ '&Perforce.New\ &Submission\ Template :PSubmit<CR>'
  else
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Changelist.&New :PChange<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Changelist.' .
        \ '&Edit\ Current\ Changelist :silent! exec "PEditItem"<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Changelist.' .
        \ 'Descri&be\ Current\ Changelist :silent! exec "PDescribeItem"<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Changelist.' .
        \ '&Delete\ Current\ Changelist :silent! exec "PDeleteItem"<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Changelist.' .
        \ 'New\ &Submission\ Template :PSubmit<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Changelist.-Sep- :'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Changelist.' .
        \ 'View\ &Pending\ Changelists :PF changes -s pending<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Changelist.' .
        \ '&View\ Submitted\ Changelists :PF changes -s submitted<CR>'
  endif

  if ! a:expanded
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Branch :PBranch<CR>'
  else
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Branch.&New :PBranch<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Branch.' .
        \ '&Edit\ Current\ BranchSpec :silent! exec "PEditItem"<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Branch.' .
        \ 'Descri&be\ Current\ BranchSpec :silent! exec "PDescribeItem"<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Branch.' .
        \ '&Delete\ Current\ BranchSpec :silent! exec "PDeleteItem"<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Branch.-Sep- :'
    exec 'amenu <silent> ' . a:sub .
        \ '&Perforce.&Branch.&View\ BranchSpecs :PF branches<CR>'
  endif

  if ! a:expanded
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Label :PLabel<CR>'
  else
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Label.&New :PLabel<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Label.' .
        \ '&Edit\ Current\ LabelSpec :silent! exec "PEditItem"<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Label.' .
        \ 'Descri&be\ Current\ LabelSpec :silent! exec "PDescribeItem"<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Label.' .
        \ '&Delete\ Current\ LabelSpec :silent! exec "PDeleteItem"<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Label.-Sep- :'
    exec 'amenu <silent> ' . a:sub .
        \ '&Perforce.&Label.&View\ Labels :PF labels<CR>'
  endif

  if ! a:expanded
    exec 'amenu <silent> ' . a:sub . '&Perforce.Cl&ient :PClient<CR>'
  else
    exec 'amenu <silent> ' . a:sub .
        \ '&Perforce.Cl&ient.&New :call s:NewClient()<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.Cl&ient.' .
        \ '&Edit\ Current\ ClientSpec :silent! exec "PEditItem"<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.Cl&ient.' .
        \ 'Descri&be\ Current\ ClientSpec :silent! exec "PDescribeItem"<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.Cl&ient.' .
        \ '&Delete\ Current\ ClientSpec :silent! exec "PDeleteItem"<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.' .
        \ 'Cl&ient.&Edit\ ' . escape(s:p4Client, ' ') . ' :PClient<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.Cl&ient.-Sep- :'
    " TODO: Switch to a client.
    exec 'amenu <silent> ' . a:sub .
        \ '&Perforce.Cl&ient.&View\ ClientSpecs :PF clients<CR>'
  endif

  if ! a:expanded
    exec 'amenu <silent> ' . a:sub . '&Perforce.&User :PUser<CR>'
  else
    exec 'amenu <silent> ' . a:sub .
        \ '&Perforce.&User.&New :call s:NewUser()<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&User.' .
        \ '&Edit\ Current\ UserSpec :silent! exec "PEditItem"<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&User.' .
        \ 'Descri&be\ Current\ UserSpec :silent! exec "PDescribeItem"<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&User.' .
        \ '&Delete\ Current\ UserSpec :silent! exec "PDeleteItem"<CR>'
    exec 'amenu <silent> ' . a:sub .
        \ '&Perforce.&User.&Edit\ ' . escape(s:p4User, ' ') . ' :PSU<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&User.-Sep- :'
    " TODO: Switch to a user.
    exec 'amenu <silent> ' . a:sub .
        \ '&Perforce.&User.&View\ Users :PF users<CR>'
  endif

  if ! a:expanded
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Job :PJob<CR>'
  else
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Job.&New :PJob<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Job.' .
        \ '&Edit\ Current\ JobSpec :silent! exec "PEditItem"<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Job.' .
        \ 'Descri&be\ Current\ JobSpec :silent! exec "PDescribeItem"<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Job.' .
        \ '&Delete\ Current\ JobSpec :silent! exec "PDeleteItem"<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Job.-Sep- :'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Job.&View\ Jobs :PF jobs<CR>'
  endif

  if a:expanded
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Depot.&New :PDepot<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Depot.' .
        \ '&Edit\ Current\ DepotSpec :silent! exec "PEditItem"<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Depot.' .
        \ 'Descri&be\ Current\ DepotSpec :silent! exec "PDescribeItem"<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Depot.' .
        \ '&Delete\ Current\ DepotSpec :silent! exec "PDeleteItem"<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Depot.-Sep- :'
    exec 'amenu <silent> ' . a:sub .
        \ '&Perforce.&Depot.&View\ Depots :PF depots<CR>'
  endif

  if ! a:expanded
    exec 'amenu <silent> ' . a:sub .
        \ '&Perforce.Open\ Current\ File\ From\ A&nother\ Codeline :E<CR>'
  else
    exec 'amenu <silent> ' . a:sub .
        \ '&Perforce.&Tools.Open\ File\ From\ A&nother\ Codeline :E<CR>'
  endif

  exec 'amenu <silent> ' . a:sub . '&Perforce.-Sep4- :'
  exec 'amenu <silent> ' . a:sub . '&Perforce.Re-Initial&ze :PFInitialize<CR>'
  if ! a:expanded
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Help :PH<CR>'
  else
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Help.&General :PH<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Help.&Simple :PH simple<CR>'
    exec 'amenu <silent> ' . a:sub .
        \ '&Perforce.&Help.&Commands :PH commands<CR>'
    exec 'amenu <silent> ' . a:sub .
        \ '&Perforce.&Help.&Environment :PH environment<CR>'
    exec 'amenu <silent> ' . a:sub .
        \ '&Perforce.&Help.&Filetypes :PH filetypes<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Help.&Jobview :PH jobview<CR>'
    exec 'amenu <silent> ' . a:sub .
        \ '&Perforce.&Help.&Revisions :PH revisions<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Help.&Usage :PH usage<CR>'
    exec 'amenu <silent> ' . a:sub . '&Perforce.&Help.&Views :PH views<CR>'
  endif
endfunction

"
" Add menu entries if user wants.
"

if exists("g:p4UseExpandedMenu")
  let s:useExpandedMenu = g:p4UseExpandedMenu
  unlet g:p4UseExpandedMenu
elseif !exists("s:useExpandedMenu")
  let s:useExpandedMenu = 0
endif

if exists("g:p4UseExpandedPopupMenu")
  let s:useExpandedPopupMenu = g:p4UseExpandedPopupMenu
  unlet g:p4UseExpandedPopupMenu
elseif !exists("s:useExpandedPopupMenu")
  let s:useExpandedPopupMenu = 0
endif

if exists("g:p4EnableMenu")
  let s:enableMenu = g:p4EnableMenu
  unlet g:p4EnableMenu
elseif !exists("s:enableMenu")
  let s:enableMenu = 0
endif

silent! unmenu Perforce
silent! unmenu! Perforce
if s:enableMenu
  call s:CreateMenu('', s:useExpandedMenu)
endif

if exists("g:p4EnablePopupMenu")
  let s:enablePopupMenu = g:p4EnablePopupMenu
  unlet g:p4EnablePopupMenu
elseif !exists("s:enablePopupMenu")
  let s:enablePopupMenu = 0
endif

silent! unmenu PopUp.Perforce
silent! unmenu! PopUp.Perforce
if s:enablePopupMenu
  call s:CreateMenu('PopUp.', s:useExpandedPopupMenu)
endif

aug P4
au!
if s:promptToCheckout
  au FileChangedRO * nested :call <SID>CheckOutFile()
endif
aug END


" Initialize some script variables.
function! s:ResetP4Vars()
  let s:p4Options = ""
  let s:p4Command = ""
  let s:p4Arguments = ""
  let s:p4LastArg = ""
  let s:p4WinName = ""
endfunction
call s:ResetP4Vars()

" Determine the script id.
function! s:MyScriptId()
  map <SID>xx <SID>xx
  let s:sid = maparg("<SID>xx")
  unmap <SID>xx
  return substitute(s:sid, "xx$", "", "")
endfunction
let s:myScriptId = s:MyScriptId()
delfunction s:MyScriptId

" On cygwin bash, p4 sometimes gets confused with the PWD env variable???
if OnMS() && match(&shell, '\<bash\>') != -1
  let s:p4CommandPrefix = "unset PWD && "
else
  let s:p4CommandPrefix = ""
endif

let s:p4KnownCmds = "add,admin,branch,branches,change,changes,client,clients," .
    \ "counter,counters,delete,depot,depots,describe,diff,diff2,dirs,edit," .
    \ "filelog,files,fix,fixes,flush,fstat,group,groups,have,help,info," .
    \ "integrate,integrated,job,jobs,jobspec,label,labels,labelsync,lock," .
    \ "logger,obliterate,opened,passwd,print,protect,rename,reopen,resolve," .
    \ "resolved,revert,review,reviews,set,submit,sync,triggers,typemap," .
    \ "unlock,user,users,verify,where,"

let s:p4SubmitTemplate = "Change:\tnew\n\n" .
    \ "Client:\t" . s:p4Client . "\n\n" .
    \ "User:\t" . s:p4User . "\n\n" .
    \ "Status:\tnew\n\n" .
    \ "Description:\n\t<enter description here>\n\n" .
    \ "Files:\n"

" Delete unnecessary stuff.
delfunction s:CreateMenu

endfunction " s:Initialize
call s:Initialize()


function! s:PPrint(outputType, ...)
  " TODO: Temp. work-around for delete doesn't make sense on file history.
  if a:0 > 0 && a:1 == "-d"
    return
  endif

  exec g:makeArgumentString
  exec "call s:PFIF(1, a:outputType, 1, 'print', " . argumentString . ")"

  " For those that create a new window, take cursor to the pattern.
  if a:outputType == 0 || a:outputType == 1
    if a:outputType == 1
      wincmd p
    endif

    let &ft=s:GuessFileType(w:pfCurFileName)

    if a:outputType == 1
      wincmd p
    endif
  endif
endfunction


function! s:POpened(outputType, ...)
  exec g:makeArgumentString
  exec "call s:PFIF(1, a:outputType, 0, 'opened', " . argumentString . ")"
  set ft=vim " This seems to show some decent syntax highlighting.
  " For now, assume that a new window is created and we are in the new window.
  exec "setlocal includeexpr=" . s:myScriptId . "ConvertToLocalPath(v:fname)"
endfunction


function! s:PDiff(outputType, ...)
  exec g:makeArgumentString
  exec "call s:PFIF(1, a:outputType, 1, 'diff', " . argumentString . ")"
  set ft=diff
endfunction


function! s:PEdit(outputType, ...)
  exec g:makeArgumentString
  let _autoread = &autoread
  set autoread
  exec "call s:PFIF(1, " . a:outputType . ", 1, 'edit', " . argumentString . ")"
  checktime
  let &autoread = _autoread
endfunction


function! s:PAdd(outputType, ...)
  exec g:makeArgumentString
  exec "call s:PFIF(1, " . a:outputType . ", 1, 'add', " . argumentString . ")"
endfunction


function! s:PDelete(outputType, ...)
  exec g:makeArgumentString
  exec "call s:PFIF(1, " . a:outputType . ", 1, 'delete', " . argumentString .
      \ ")"
endfunction


function! s:PLock(outputType, ...)
  exec g:makeArgumentString
  exec "call s:PFIF(1, " . a:outputType . ", 1, 'lock', " . argumentString . ")"
endfunction


function! s:PUnlock(outputType, ...)
  exec g:makeArgumentString
  exec "call s:PFIF(1, " . a:outputType . ", 1, 'unlock', " . argumentString .
      \ ")"
endfunction


function! s:PRevert(outputType, ...)
  exec g:makeArgumentString
  let _autoread = &autoread
  set autoread
  exec "call s:PFIF(1, " . a:outputType . ", 1, 'revert', " . argumentString .
      \ ")"
  checktime
  let &autoread = _autoread
endfunction


function! s:PSync(outputType, ...)
  exec g:makeArgumentString
  let _autoread = &autoread
  set autoread
  exec "call s:PFIF(1, " . a:outputType . ", 1, 'sync', " . argumentString . ")"
  checktime
  let &autoread = _autoread
endfunction


function! s:PDiff2(outputType, ...)
  if ((a:0 == 0) || (((a:0 == 1) && (a:1 == ""))))
    let argumentString = ""
    let file = expand("%")
  else
    exec g:makeArgumentString
    " Get the last argument as a file and remove it from argumentString.
    exec "let file = a:" . a:0
    let argumentString = MvRemoveElement(argumentString, ',', (a:0 - 1))
  endif
  let ver1 = s:PromptFor(0, s:useDialogs, "Version1? ", '')
  let ver2 = s:PromptFor(0, s:useDialogs, "Version2? ", '')
  let argumentString = MvAddElement(argumentString, ',',
        \ " '" . file . '#' . ver1 . "'")
  let argumentString = MvAddElement(argumentString, ',',
        \ " '" . file . '#' . ver2 . "'")
  exec "call s:PFIF(1, a:outputType, 0, 'diff2', " . argumentString . ")"
  set ft=diff
endfunction


" Open a file from an alternative codeline.
" First argument is expected to be codeline, and the remaining arguments are
" expected to be filenames. 
function! s:PFOpenAltFile(...)
  if a:0 == 0
    " Prompt for codeline.
    let codeline = s:PromptFor(0, s:useDialogs,
        \ "Enter the alternative codeline: ", '')
    if codeline == ""
      echohl Error | echo "Codeline required." | echohl NONE
      return
    endif
  elseif a:0 == 1
    let codeline = a:1
  endif
  " If the filenanme argument is mising, then assume it is for the current file.
  if a:0 < 2
    let argumentString = "'" . expand("%") . "'"
  else
    exec g:makeArgumentString
  endif

  exec "let altFileNames = s:PFGetAltFiles(codeline, " . argumentString . ")"
  if a:0 == 1
    let n = 1
  else
    let n = MvNumberOfElements(altFileNames, ';')
  endif
  if n == 1
    execute ":edit " . altFileNames
  else
    call MvIterCreate(altFileNames, ';', "Perforce")
    while MvIterHasNext("Perforce")
      execute ":badd " . MvIterNext("Perforce")
    endwhile
    call MvIterDestroy("Perforce")
    execute ":edit " . MvElementAt(altFileNames, ";", 0)
  endif
endfunction


" Interactively change the port/client/user.
function! s:SwitchPortClientUser()
  let p4Port = s:PromptFor(0, s:useDialogs, "Port: ", s:p4Port)
  let p4Client = s:PromptFor(0, s:useDialogs, "Client: ", s:p4Client)
  let p4User = s:PromptFor(0, s:useDialogs, "User: ", s:p4User)
  call s:PSwitch(p4Port, p4Client, p4User)
endfunction


" No args: Print presets and prompt user to select a preset.
" Number: Select that numbered preset. 
" port [client] [user]: Set the specified settings.
function! s:PSwitch(...)
  if a:0 == 0 || a:1 == ""
    let nArgs = 0
  else
    let nArgs = a:0
  endif

  let nSets = MvNumberOfElements(s:p4Presets, ',')
  if nArgs == 0
    if nSets == 0
      echohl ERROR | echo "No sets to select from." | echohl None
      return
    endif

    let selectedSetting = MvPromptForElement(s:p4Presets, ',', 0,
        \ "Select the setting: ", -1, s:useDialogs)
    call s:PSwitchHelper(selectedSetting)
    return
  else
    if match(a:1, '^\d\+') == 0
      let index = a:1 + 0
      if index >= nSets
        echohl ERROR | echo "Not that many sets." | echohl None
        return
      endif
      let selectedSetting = MvElementAt(s:p4Presets, ',', index)
      call s:PSwitchHelper(selectedSetting)
      return
    else
      let g:p4Port = a:1
      if nArgs > 1
        let g:p4Client = a:2
      endif
      if nArgs > 2
        let g:p4User = a:3
      endif
    endif
    call s:Initialize()
  endif
endfunction


function! s:PSwitchHelper(settingStr)
  if a:settingStr != ""
    let settingStr = substitute(a:settingStr, '\s\+', "','", 'g')
    let settingStr = substitute(settingStr, '^', "'", '')
    let settingStr = substitute(settingStr, '$', "'", '')
    exec 'call s:PSwitch(' . settingStr . ')'
  endif
endfunction


function! s:PChange(outputType, ...)
  exec g:makeArgumentString
  exec "call s:PInteractiveCommand(a:outputType, 'change', 0,
    \ '^<enter description here>\\|^Description:', " . argumentString . ")"
endfunction


function! s:PBranch(outputType, ...)
  exec g:makeArgumentString
  exec "call s:PInteractiveCommand(a:outputType, 'branch', 1, '^View:', " .
      \ argumentString . ")"
endfunction


function! s:PLabel(outputType, ...)
  exec g:makeArgumentString
  exec "call s:PInteractiveCommand(a:outputType, 'label', 1, '^View:', " .
      \ argumentString . ")"
endfunction


function! s:NewClient()
  let clientName = s:PromptFor(0, s:useDialogs, "Client name: ", '')
  if clientName == ""
    echohl Error | echo "Client name required." | echohl NONE
    return
  endif
  call s:PClient(0, clientName)
endfunction


function! s:PClient(outputType, ...)
  exec g:makeArgumentString
  exec "call s:PInteractiveCommand(a:outputType, 'client', 0, '^View:', " .
      \ argumentString . ")"
endfunction


function! s:PJob(outputType, ...)
  exec g:makeArgumentString
  exec "call s:PInteractiveCommand(a:outputType, 'job', 0, '^Job:', " .
      \ argumentString . ")"
endfunction


function! s:NewUser()
  let userName = s:PromptFor(0, s:useDialogs, "User name: ", '')
  if userName == ""
    echohl Error | echo "Client name required." | echohl NONE
    return
  endif
  call s:PUser(0, userName)
endfunction


function! s:PUser(outputType, ...)
  exec g:makeArgumentString
  exec "call s:PInteractiveCommand(a:outputType, 'user', 0, '^User:', " .
      \ argumentString . ")"
endfunction


function! s:PDepot(outputType, ...)
  exec g:makeArgumentString
  exec "call s:PInteractiveCommand(a:outputType, 'depot', 1, '^Description:', "
      \ . argumentString . ")"
endfunction


function! s:PResolve(outputType, ...)
  exec g:makeArgumentString
  exec "call s:ParseOptions('resolve', " . argumentString . ")"

  if (match(s:p4Arguments, '-a[fmsty]\>') == -1) &&
      \ (match(s:p4Arguments, '-n\>') == -1)
    echohl Error | echo "Interactive resolve not implemented yet." | echohl None
    return
  endif
  call s:PFImpl(a:outputType, s:p4CommandPrefix, 0, "")
endfunction


" Create a template for submit.
function! s:PSubmit(outputType)
  call s:ResetP4Vars()
  let s:p4Command = "submit"
  let s:p4WinName = "P4 submit"

  call s:PFImpl(a:outputType, "", 2, s:p4SubmitTemplate)
  $
  -mark t
  $call s:PW("opened")
  let _saveSearch = @/
  let @/ = "^"
  silent! 't+1,$s//\t/
  let @/ = _saveSearch
  command! -buffer -nargs=* W :1,$call <SID>PW("submit", "-i", <f-args>)
  if search('<enter description here>', 'w') != 0
    normal! zz
  endif
  redraw | echo "When done, submit the change by using the :W command. " .
      \ "Undo if you see an error."
endfunction


function! s:PInteractiveCommand(outputType, commandName, argRequired, pattern,
    \ ...)

  exec g:makeArgumentString
  exec "call s:ParseOptions('" . a:commandName . "', " . argumentString . ")"

  " Check not sufficient if the user gives other arguments and forgets to give
  "   name.
  if s:p4LastArg == "" || s:p4Arguments == ""
    if a:argRequired
      let s:p4Arguments = s:PromptFor(0, s:useDialogs,
          \ "Enter the " . a:commandName . " name: ", '')
      if s:p4Arguments == ""
        echohl Error | echo substitute(a:commandName, "^.", '\U&', '') .
            \ " name required." | echohl NONE
        return
      endif
    endif
  endif

  let interactive = 0
  if match(s:p4Arguments, '-d\>') == -1
    let s:p4Arguments = "-o " . s:p4Arguments
    if a:outputType == 0 " Only if in the edit mode.
      let interactive = 1
    endif
  endif

  if s:PFImpl(a:outputType, s:p4CommandPrefix, 0, "") != 0
    return
  endif

  if interactive
    setlocal modifiable
    exec 'command! -buffer -nargs=* W :1,$call <SID>PW("' . a:commandName .
          \ '", "-i", <f-args>)'
    redraw | echo "When done, save " . a:commandName .
          \ " spec by using the :W command. Undo if you see an error."
  endif

  " For those that create a new window, take cursor to the pattern.
  if a:outputType == 0 || a:outputType == 1
    if a:outputType == 1
      wincmd p
    endif

    if search(a:pattern, 'w') != 0
      normal! zz
    endif

    if a:outputType == 1
      wincmd p
    endif
  endif
endfunction


function! s:PHelp(outputType, ...)
  "exec g:makeArgumentString
  "exec "call s:ParseOptions('help', " . argumentString . ")"
  " Work-around to avoid processing the argument to help.
  call s:ResetP4Vars()
  let s:p4Command = 'help' 
  if a:0 > 0
    let s:p4Arguments = a:1
  endif
  " Use simple window name for all the help commands.
  let s:p4WinName = 'P4 help'

  if s:PFImpl(a:outputType, s:p4CommandPrefix, 0, "") != 0
    return
  endif

  call s:SetupSelectHelp()
  redraw | echo "Press <Enter> or K to drilldown on perforce help keywords."
endfunction


function! s:CheckOutFile()
  if filereadable(expand("%")) && ! filewritable(expand("%"))
    let option = confirm("Readonly file, do you want to checkout from perforce?"
          \, "&Yes\n&No", 1, "Question")
    if option == 1
      call s:PEdit()
    endif
    edit!
  endif
endfunction


" Filter contents through p4.
function! s:PW(...) range
  exec g:makeArgumentString
  exec "call s:ParseOptions(" . argumentString . ")"

  setlocal modifiable
  call s:PFImpl(4, a:firstline . ',' . a:lastline . '!' . s:p4CommandPrefix, 1,
        \ "")
endfunction


function! s:PFIF(scriptOrigin, outputType, defaultToCurrentFile, ...)
  exec g:makeArgumentString
  exec "call s:ParseOptions(" . argumentString . ")"

  if ! a:scriptOrigin
    let redirect = s:getCommandHandler(s:p4Command)
    if redirect != ""
      let argumentString = MvRemoveElement(argumentString, ", ",
            \ "'" . s:p4Command . "'")
      exec "call " . redirect . "(" . a:outputType . ", " . argumentString . ")"
      return
    endif
  endif

  if a:defaultToCurrentFile && s:p4LastArg == ""
    let s:p4Arguments = MvAddElement(s:p4Arguments, ' ', expand("%"))
  endif

  if s:PFImpl(a:outputType, s:p4CommandPrefix, 0, "") != 0
    return
  endif

  " If the above call created a new window, then only setup SetupSelectItem.
  if a:outputType == 0 || a:outputType == 1
    " If this command has a handler for the individual items, then enable the
    " item selection commands.
    if s:getCommandItemHandler(s:p4Command) != ""
      call s:SetupSelectItem()
    endif
  endif
  if s:p4Command == "filelog"
    set ft=vim " This seems to show some decent syntax highlighting for now.
  endif
endfunction


function! s:LookupValue(key, type)
  exec 'return s:' . a:key . '_' . a:type
endfunction


" Generate raw output into a new window.
function! s:PFRaw(outputType, ...)
  exec g:makeArgumentString
  exec "call s:ParseOptions(" . argumentString . ")"

  call s:PFImpl(a:outputType, s:p4CommandPrefix, 0, "")
endfunction


function! s:SelectHelp()
  call s:ResetP4Vars()
  let s:p4Command = "help"
  let s:p4Arguments = expand("<cword>")
  let s:p4WinName = "P4 help"

  setlocal modifiable
  call s:PFImpl(4, '1,$!' . s:p4CommandPrefix, 1, "")
endfunction


" Handler for opened command.
" TODO: The variable number of arguments is a temp. work-around to avoid error
" if user mistakenly trys to execute delete on the opened list.
function! s:OpenFile(outputType, fileName, ...)
  " TODO: Temp. work-around for delete doesn't make sense on opened files.
  if a:fileName == "-d"
    return
  endif

  if filereadable(a:fileName)
    if a:outputType == 0
      exec "split " . a:fileName
    else
      pclose
      exec "pedit " . a:fileName
    endif
  else
    call s:PPrint(a:outputType, a:fileName)
  endif
endfunction


function! s:getCommandHandler(command)
  let handler = 's:P' . substitute(a:command,'.','\U&','')
  if exists('*' . handler)
    return handler
  else
    return ""
  endif
endfunction


function! s:getCommandItemHandler(command)
  let handlerCmd = ""
  if exists("s:" . a:command . "ItemHandler")
    exec "let handlerCmd = s:" . a:command . "ItemHandler"
  elseif match(a:command, 'e\?s$') != -1
    let handlerCmd = substitute(a:command, 'e\?s$', '', '')
    let handlerCmd = 's:P' . substitute(handlerCmd,'.','\U&','')
  endif
  return handlerCmd
endfunction


function! s:SelectCurrentItem(outputType, advance)
  let curItem = s:GetCurrentItem()
  if curItem != ""
    let commandHandler = s:getCommandItemHandler(b:p4Command)
    if commandHandler != ""
      exec 'call ' . commandHandler . '(' . a:outputType . ', ' .
            \ "'" . curItem . "'" . ')'
    endif
  elseif a:advance
    normal <CR>
  endif
endfunction


function! s:GetCurrentItem()
  if exists("b:p4Command") && exists("s:" . b:p4Command . "Expr")
    exec "let expr = s:" . b:p4Command . "Expr"
    if expr == ""
      return
    endif
    exec "return " expr
  endif
  return ""
endfunction


function! s:DeleteCurrentItem()
  let curItem = s:GetCurrentItem()
  if curItem != ""
    let answer = confirm("Are you sure you want to delete " . curItem . "?",
        \ "&Yes\n&No", 2, "Question")
    if answer == 1
      let options = "'-d', '-f', "
      let command = substitute(b:p4Command, 'e\?s$', '', '')
      exec 'call ' . s:getCommandItemHandler(command) . '(2, ' . options . "'" .
          \ curItem . "'" . ')'
    endif
  endif
endfunction


function! s:SetupSelectItem()
  nnoremap <silent> <buffer> D :PDeleteItem<CR>
  nnoremap <silent> <buffer> O :PEditItem<CR>
  nnoremap <silent> <buffer> <CR> :call <SID>SelectCurrentItem(1, 1)<CR>
  nnoremap <silent> <buffer> <2-LeftMouse> :PDescribeItem<CR>
  command! -buffer -nargs=0 PDescribeItem :call <SID>SelectCurrentItem(1, 0)
  command! -buffer -nargs=0 PEditItem :call <SID>SelectCurrentItem(0, 0)
  command! -buffer -nargs=0 PDeleteItem :call <SID>DeleteCurrentItem()
endfunction


function! s:SetupSelectHelp()
  nnoremap <silent> <buffer> <CR> :call <SID>SelectHelp()<CR>
  nnoremap <silent> <buffer> K :call <SID>SelectHelp()<CR>
  nnoremap <silent> <buffer> <2-LeftMouse> :call <SID>SelectHelp()<CR>
  nnoremap <silent> <buffer> <C-O> u
  nnoremap <silent> <buffer> <Tab> <C-R>
endfunction


"
" Infrastructure.
"


" TODO: avoid processing help argument.
" Assumes that the arguments are already parsed and are ready to be used in
"   the script variables.
" Low level interface with the p4 command.
" outputType:
"   0 - Execute p4 and place the output in a new window.
"   1 - Same as above, but use preview window.
"   2 - Execute p4 and show the output in a dialog for confirmation.
"   3 - Execute p4 and echo the output.
"   4 - Execute p4 but discard output.
" commandType:
"   0 - Execute p4 using system().
"   1 - Execute p4 as a filter for the current window contents. Use 
"         commandPrefix to restrict the filter range.
"   2 - Don't execute p4. The output is already passed in. 
" Returns non-zero error-code on failure. 
function! s:PFImpl(outputType, commandPrefix, commandType, output)
  let _report = &report
  set report=99999

  " Otherwise it may causes some trouble for cygwin BASH.
  let fullCmd = a:commandPrefix . s:p4CmdPath . ' ' . s:defaultOptions . ' ' .
          \ s:p4Options . ' ' . s:p4Command . ' ' . s:p4Arguments
  let g:p4FullCmd = fullCmd " Debug.
  " save the name of the current file.
  let pfCurFileName = expand("%")

  let error = 0
  if a:commandType == 0
    " If it placing the output in a new window, then we shouldn't use system()
    "   for efficiency reasons.
    if a:outputType != 0
      " Assume the shellredir is set correctly to capture the error messages.
      let output = system(fullCmd)

      let error = s:CheckShellError(output)
    else
      let output = ""
    endif
  elseif a:commandType == 1
    silent! exec fullCmd
    let output = ""

    let error = s:CheckShellError(output)
  elseif a:commandType == 2
    let output = a:output
  endif

  if error == 0
    let newWindowCreated = 0
    " If the output has to be shown in a dialog, bringup a dialog with the
    "   output, otherwise show it in a new window.
    if a:outputType == 0
      split
      let w:pfCurFileName = pfCurFileName

      exec ":edit " . s:p4WinName

      setlocal modifiable
      if a:commandType == 0
        exec ".!" . fullCmd
        let error = s:CheckShellError(output)
      else
        put! =output
      endif
      if v:shell_error == 0
        call s:PFSetupBuf(s:p4WinName)
        let newWindowCreated = 1
      endif
    elseif a:outputType == 1
      pclose
      exec ":pedit " . s:p4WinName
      wincmd p
      let w:pfCurFileName = pfCurFileName

      setlocal modifiable
      if a:commandType == 0
        exec ".!" . fullCmd
        let error = s:CheckShellError(output)
      else
        put! =output
      endif
      if v:shell_error == 0
        call s:PFSetupBuf(s:p4WinName)
        wincmd p
        let newWindowCreated = 1
      endif
    elseif a:outputType == 2
      call confirm(output, "OK", 1, "Info")
    elseif a:outputType == 3
      echo output
    elseif a:outputType == 4
      " Do nothing.
    endif
    if newWindowCreated
      if a:outputType == 1
        wincmd p
      endif
      let b:p4Command = s:p4Command
      let b:p4FullCmd = fullCmd
      if a:outputType == 1
        wincmd p
      endif
    endif
  endif
  let &report = _report
  return error
endfunction


function! s:CheckShellError(output)
  let output = a:output
  if v:shell_error != 0
    let output = "There was an error executing external p4 command.\n" . output
    if output == ""
      let output = output . "\nSet the shellredir option correctly to be able" .
          \ "to capture the error message."
    endif
    call confirm(output, "OK", 1, "Error")
  endif
  return v:shell_error
endfunction

" Parses the arguments into 4 parts, "options to p4", "p4 command",
" "options to p4 command", "actual arguments". Also generates the window name.
function! s:ParseOptions(...)
  call s:ResetP4Vars()
  let s:p4WinName = "P4"

  if ((a:0 == 0) || (((a:0 == 1) && (a:1 == ""))))
    return
  endif

  let i = 1
  while i <= a:0
    exec "let curArg = a:" . i
    let winArg = curArg
    if match(curArg, '^-') < 0
      if s:p4Command == "" && MvContainsElement(s:p4KnownCmds, ',', curArg)
        let s:p4Command = curArg
      else
        " Don't clean the filenames that are depot specs.
        if match(winArg, '^//depot/') == -1
          let winArg = CleanupFileName(winArg)
        endif
        let winArg = escape(winArg, '#')

        " Most probably a filename, cook it.
        let curArg = winArg
        if match(curArg, '^//') == 0 && match(curArg, '^//depot/') == -1
          let curArg = strpart(curArg, 1, strlen(curArg) - 1)
        endif
        let curArg = escape(curArg, " \t") " Escape white space.
        let curArg = escape(curArg, "\\") " For bash???
        let s:p4LastArg = curArg

        let s:p4Arguments = s:p4Arguments . ' ' . curArg
      endif
    else
      if s:p4Command == ""
        let s:p4Options = s:p4Options . ' ' . curArg
      else
        let s:p4Arguments = s:p4Arguments . ' ' . curArg
      endif
    endif
    let s:p4WinName = s:p4WinName . ' ' . winArg
    let i = i + 1
  endwhile
endfunction


function! s:GuessFileType(fileName)
  let fileExt = fnamemodify(a:fileName, ":e")
  let fileExt = matchstr(fileExt, '\w\+')
  return fileExt
endfunction


function! s:ConvertToLocalPath(depotName)
  let fileName = a:depotName
  if match(a:depotName, '^//depot/') == 0 ||
        \ match(a:depotName, '^//'. s:p4Client . '/') == 0
    let fileName = s:codelineRoot . substitute(fileName, '^//[^/]\+', '', '')
  endif
  let fileName = substitute(fileName, '#[^#]\+$', '', '')
  return fileName
endfunction


" Requires at least 2 arguments. 
" Returns a list of alternative filenames. 
function! s:PFGetAltFiles(codeline, ...)
  if a:0 == 0 || a:1 == ""
    return ""
  endif

  let altCodeLine = a:codeline

  let i = 1
  let altFiles = ""
  let root=CleanupFileName(s:codelineRoot) . "/"
  while i <= a:0
    exec "let fileName = a:" . i
    let fileName=CleanupFileName(fnamemodify(fileName, ":p"))
    " We have only one slash after the cleanup is done.
    if match(fileName, '^/depot/') == 0 ||
          \ match(fileName, '^/'. s:p4Client . '/') == 0
      let fileName = root . substitute(fileName, '^//[^/]\+/', '', '')
    elseif match(fileName, '^/') == -1
      if match(fileName, '^[a-zA-Z]:') < 0 && !OnMS()
        let fileName=getcwd() . "/" . fileName
      endif
    endif
    " One final cleanup, just in case.
    let fileName=CleanupFileName(fileName)

    let altFiles = MvAddElement(altFiles, ';',
          \ substitute(fileName, root . '[^/]\+', root . altCodeLine, ""))
    let i = i + 1
  endwhile
  " Remove the last separator, so the the list is ready to be used for 1
  " element case.
  let altFiles = strpart(altFiles, 0, strlen(altFiles) - 1) 
  return altFiles
endfunction


function! s:GetCurrentDepotFile()
  " Local submissions.
  let fileName = ""
  let line = getline(".")
  if match(line, '\.\.\. #\d\+ .*') != -1
    let fileVer = matchstr(line, '\d\+')
    let fileName = w:pfCurFileName . "#" . fileVer
  " Branches, integrations etc.
  elseif match(line, '//depot/.*#\d\+') != -1 ||
          \ match(line, '^//'. s:p4Client . '/.*#\d\+') != -1
    let fileName = matchstr(line, '//[^/]\+/[^#]*#\d\+')
  endif
  return fileName
endfunction


function! s:PFSetupBuf(p4WinName)
" call input("PFfilelogSetup")
  call SetupScratchBuffer()
  setlocal nomodified
  setlocal nomodifiable
  call s:PFSetupBufAutoClean(a:p4WinName)
endfunction


" Arrange an autocommand such that the buffer is automatically deleted when the
"  window is quit. Delete the autocommand itself when done.
function! s:PFSetupBufAutoClean(p4WinName)
  exec "au BufWinLeave " . escape(a:p4WinName, ' ') .
        \ " :call <SID>PFExecBufClean('" . a:p4WinName . "')"
endfunction


" Find and delete the buffer. Delete the autocommand itself after that.
function! s:PFExecBufClean(p4WinName)
  let bufNo = FindBufferForName(a:p4WinName)
  if bufNo == -1
    " Should not happen
    echoerr "perforce.vim: Internal ERROR detected. Please report this message."
    return
  endif
  let _report=&report
  set report=99999
  exec "silent! bwipeout! ". bufNo
  let &report=_report
  exec "au! BufWinLeave " . escape(a:p4WinName, ' ')
endfunction


function! s:PromptFor(loop, useDialogs, msg, default)
  let result = ""
  while result == ""
    if a:useDialogs
      let result = inputdialog(a:msg, a:default)
    else
      let result = input(a:msg, a:default)
    endif
    if ! a:loop
      break
    endif
  endwhile
  return result
endfunction


function! s:PRefreshActivePane()
  if exists("b:p4FullCmd")
    let _modifiable = &l:modifiable
    setlocal modifiable
    exec "1,$!" . b:p4FullCmd
    let &l:modifiable=_modifiable
  endif
endfunction
