
"Name: Buffer reminder Remix version
"Version: 0.6
"Autor: Vakulenko Sergiy
"Description: I rewrite this plugin because prev. version of plugin not work properly with tabs/windows.
"I recommend use vim tab concept with this plugin.

"Sorry for this "soft" type of documentation. Next time i will create vim doc file.
"Also you can mail me if you have supplementary questions options.
"
"


"-------------------------------------------
"VARIABLES DESCRIPTION:
"-------------------------------------------

"-------------------------------------------
"Variable: g:BuffReminder_enablePlugin (bool)
"-------------------------------------------
"Description: - by default - plugin is disabled. To make it work set BuffReminder_enablePlugin:
"let g:BuffReminder_enablePlugin = 1

"-------------------------------------------
"Variable: g:BuffReminderRMX_OpenFirstTabByDefault (bool)
"-------------------------------------------
"Description: - open first tab by default

"-------------------------------------------
"Variable: g:BuffReminderRMX_ProjectFiles ( [] )
"-------------------------------------------
"Description: this variable is list of project files. Need to add
"they to this list to make them treater by the right way (this file's will be
"opened with 'Project ' <project_file>; also to skip project split 
"Example:
"let g:BuffReminderRMX_ProjectFiles          = ['C:\programs\gvim\vim_extention\projects\work_machine.projectfile'
                                            "\, 'C:\programs\gvim\vim_extention\projects\home_machine.projectfile']

"-------------------------------------------
"Variable: g:BufReminderRMX_ignoreFilesList ( [] )
"-------------------------------------------
"Description: check if buff_name in ignore file list, and if yes, skip it

"-------------------------------------------
"Variable: g:BuffReminderRMX_Disable_Hidden ( bool )
"-------------------------------------------
"Description: if this option is enabled - all 'hidden' buffers (which isn't
"showed in windows) will be skipped in save procedure

"-------------------------------------------
"Variable: g:BuffReminderRMX_Default_SplitMode ( string ('split' or 'vsplit' ) )
"-------------------------------------------
"Description: when page contain many split window with differents
"possitions/size - on save - we can't decide what split mode was used. That why, the 'simplest' solution was to define window default split mode.
"todo: add algorithm which decide split option according to all windows columns/rows size in tab.

"-------------------------------------------
"Variable: g:BuffReminder_persistency_file ( string ('split' or 'vsplit' ) )
"-------------------------------------------
"Description: this is path and name of persistency file. You can it redefine if default isn't good for you.


if v:version < 700 || &diff "skip load plugin if diff mode
    finish
endif

if g:BuffReminder_enablePlugin == 0 "plugin not enabled
    finish
endif




"//- SET GLOBAL VARIABLES -------------------------------------------------------------------
let g:BuffReminder_skip_NoNameBuffer        = 1

if !exists('g:BuffReminderRMX_OpenFirstTabByDefault')
    let g:BuffReminderRMX_OpenFirstTabByDefault = 0
endif


if !exists('g:BufReminderRMX_ignoreFilesList')
    let g:BufReminderRMX_ignoreFilesList        = []
endif

if !exists('g:BuffReminderRMX_ProjectFiles')
    let g:BuffReminderRMX_ProjectFiles        = []
endif

if !exists('g:BuffReminderRMX_Disable_Hidden')
    let g:BuffReminderRMX_Disable_Hidden        = 0
endif

if !exists('g:BuffReminderRMX_Default_SplitMode')
    let g:BuffReminderRMX_Default_SplitMode        = 'split'
endif

if !exists('g:BuffReminder_persistency_file')
    let bufRemiderPersFile = 'vim_bufReminder.cfg'
    let bifRemiderPersPath = ''

    if has('win32')
        let bifRemiderPersPath = $USERPROFILE . '\'
    else
        if has('unix') || has('macunix')
            let bifRemiderPersPath = $HOME . '/'
        else
            let bifRemiderPersPath = $VIM .  '/'
        endif
    endif
    let g:BuffReminder_persistency_file = bifRemiderPersPath . bufRemiderPersFile
endif


let g:buf_info_lst = []
let g:buf_default_view_pos = [1,1]


"//- FUNCTIONS DECLARATION -------------------------------------------------------------------
func! BufReminderRMX_GetActualfBuffInfo()

    for i in range(tabpagenr('$')) "iterate tabs

        let isEditBufferWasOpened = 0
        "get buffers of tab. Important: index of each iterations is WINDOW INDEX!!!
        let buff_index_list = tabpagebuflist(i+1) 
        "Decho("tabid=" + (i+1))
        "exe 'OpenTabsSilent ' . (i+1)
        exe 'tabnext ' . (i+1)
        if !empty(buff_index_list) "is list empty ?

            let buf_info_list = []
            let win_index = 1 "windows index 0 == 1, that why starts from 1
            let winrestcmd_str = ''
            for buf_id in buff_index_list

                if g:BuffReminder_skip_NoNameBuffer "not save NoName buffers
                    if empty(bufname(buf_id))
                        continue
                    endif
                endif
                let buff_info = {}
                let buff_info['buf_name']   = fnamemodify(bufname(buf_id), ':p') "get full path to buff/file name 
                let buff_info['win_id']     = win_index
                let buff_info['win_width']  = winwidth(win_index)
                let buff_info['win_height'] = winheight(win_index)
                "let buff_info['tab_id']     = (i+1)

                    let buff_info['split_opt'] = 'edit'

                    if BufReminderRMX_isItemInList(g:BuffReminderRMX_ProjectFiles, buff_info['buf_name'])
                        let buff_info['split_opt']  = 'project'
                    else

                        if isEditBufferWasOpened
                            if winwidth(win_index) < &columns
                                let buff_info['split_opt']  = 'vsplit'
                            else
                                let buff_info['split_opt']  = 'split'
                            endif

                        endif
                        let isEditBufferWasOpened = 1
                    endif

                call add( buf_info_list, buff_info ) "add item in global list with buff info

                "generate for buff resize str
                let winrestcmd_str .= BufReminderRMX_winrestcmd(win_index, buff_info['win_height'], buff_info['win_width'])

                let win_index  += 1 
            endfor

            let tab_id = (i+1)
            call BufReminderRMX_SaveElementInList(tab_id, buf_info_list, winrestcmd_str)

        endif

    endfor

endfunc

func! BufReminderRMX_getTab0_CurrentTabPos()
    return '0' . ' Last tab/win(' . tabpagenr() . ',' .  winnr() . ')'
endfunc

func! BufReminderRMX_getHiddenBuffers()
    let buff_info_list = []

    "here we collect hidden buffers, 
    for buffr in range(1, bufnr('$'))
        if BufHidden( buffr ) && buflisted( buffr )
            let buff_info = {}
            let buff_info['buf_name']   = fnamemodify(bufname(buffr), ':p') "get full path to buff/file name 
            let buff_info['win_id']     = 0
            let buff_info['win_width']  = 0
            let buff_info['win_height'] = 0
            let buff_info['split_opt']      = 'hide'
            call add(buff_info_list, buff_info)
        endif
    endfor

    return buff_info_list
endfunc

"//---------------------------------------------------------------------------------
func! BufReminderRMX_saveHiddenBuffers()
"//---------------------------------------------------------------------------------

    "let's save curr pos in tab 0, because this number not used in vim native buff list
    let tab0_Pos = BufReminderRMX_getTab0_CurrentTabPos()
    let buff_info_list = []

    if g:BuffReminderRMX_Disable_Hidden == 0 "save hidden buffers ?
        let buff_info_list = BufReminderRMX_getHiddenBuffers()
    endif

    "if !empty(buff_info_list)
    call BufReminderRMX_SaveElementInList(tab0_Pos, buff_info_list, '')
    "endif

endfunc

"custom function for window index"
func! BufReminderRMX_winrestcmd(win_index, height, width)
    let str  = ''
    let str .=           a:win_index . 'resize ' . a:height . '|'
    let str .= 'vert ' . a:win_index . 'resize ' . a:width .  '|'
    return str
endfunc

func! BufReminderRMX_isItemInList(lst, buff_name)
    if !empty(a:lst) &&  index( a:lst, a:buff_name ) != -1
        return 1
    endif
    return 0
endfunc
func! BufReminderRMX_BuffNameFilter(buff_name)

    let ret_value = -1

    "open it with 'Project.vim' if buff_name in 'project' files list
    if BufReminderRMX_isItemInList(g:BuffReminderRMX_ProjectFiles, a:buff_name)
        exe 'Project ' .  a:buff_name
        exe 'wincmd w'
        let ret_value = 1
        "Decho("here#1")
    endif

    "check if buff_name in ignore file list, and if yes, skip it
    if BufReminderRMX_isItemInList(g:BufReminderRMX_ignoreFilesList, a:buff_name)
        let ret_value = 1
        "Decho("here#2")
    endif

    return ret_value

endfunc

func! BufReminderRMX_OpenBuffersInList()
    "g:buf_info_lst[0]['tab_info']
    for buf_and_tab_info in g:buf_info_lst "buff info list

        if buf_and_tab_info['tab_id'] > 1 "create new tab if this is not first tab and not zero(hidden buffers)
            exe 'tabnew'
        endif

        for buff_info in buf_and_tab_info['buff_info']

            if BufReminderRMX_BuffNameFilter(buff_info['buf_name']) != -1
                continue
            endif

            call BufReminderRMX_OpenBuffer(buff_info['buf_name'], buff_info['split_opt'], buff_info['win_id'])
        endfor

        if !empty(buf_and_tab_info['tab_wins_info']) "some of viewport's can be empty, like for hidden buffers
            exe buf_and_tab_info['tab_wins_info']
        endif

    endfor
    "finally, go to first window (default)
    exe '1wincmd w'

    "if g:BuffReminderRMX_OpenFirstTabByDefault "go to first tab
        "exe 'tabnext 1'
    "endif
endfunc

func! BufReminderRMX_SaveElementInList(tab_id, buff_info, tab_wins_info)
    let buf_and_tab_info = {}
    let buf_and_tab_info['tab_id']         = a:tab_id
    let buf_and_tab_info['buff_info']      = a:buff_info
    let buf_and_tab_info['tab_wins_info']  = a:tab_wins_info

    call add(g:buf_info_lst, buf_and_tab_info)
endfunc

func! BufReminderRMX_LoadPersistency() "function to save buffers persistency information

    if filereadable(g:BuffReminder_persistency_file) "check if persistency file exist

        "Decho('here#1')
        let FileLinesList  = readfile(g:BuffReminder_persistency_file)
        call BufReminderRMX_RemovePersFile()

        if !empty(FileLinesList)
            "Decho('here#2')

            let regex_persistency = 'buff_info:\([a-zA-Z0-9: \\_\/\.-]\+\)\s\([a-z_]\+\)\s\(\d\+\)\s\(\d\+\)\s\(\d\+\)'

            let tab_id = 0
            let buf_info_list = []
            let tab_wins_info = ''
            for line in FileLinesList
                if match(line, 'tab_id:') != -1

                    let tab_id  = substitute(line,'tab_id:\(\d\+\)\(\sLast\stab\/win(\(\d\+\),\(\d\+\))\)\?','\1','')

                    "this case if we want to reload view ( tab, win positions ) 
                    if match( line, 'Last\stab' ) != -1 && g:BuffReminderRMX_OpenFirstTabByDefault == 0
                        let g:buf_default_view_pos[0] = substitute(line,'tab_id:\(\d\+\)\(\sLast\stab\/win(\(\d\+\),\(\d\+\))\)\?','\3','') "tab
                        let g:buf_default_view_pos[1] = substitute(line,'tab_id:\(\d\+\)\(\sLast\stab\/win(\(\d\+\),\(\d\+\))\)\?','\4','') "win
                    endif

                elseif match(line, 'buff_info:') != -1
                    let buff_info = {}
                    let buff_info['buf_name']   = substitute(line, regex_persistency, '\1','')
                    let buff_info['split_opt']  = substitute(line, regex_persistency, '\2','')
                    let buff_info['win_id']     = substitute(line, regex_persistency, '\3','')
                    let buff_info['win_width']  = substitute(line, regex_persistency, '\4','')
                    let buff_info['win_height'] = substitute(line, regex_persistency, '\5','')
                    call add(buf_info_list, buff_info)

                elseif match(line, 'viewport:') != -1
                    let tab_wins_info = substitute(line,'viewport:','','')

                elseif match(line, '---end of tab data---') != -1
                    call BufReminderRMX_SaveElementInList(tab_id, buf_info_list, tab_wins_info)
                    let buf_info_list = []
                endif
            endfor

        endif

    endif

endfunc

func! BufReminderRMX_RemovePersFile()
    if filereadable(g:BuffReminder_persistency_file) "remove previus persistency file, if it exist
        call delete(g:BuffReminder_persistency_file)
    endif
endfunc

func! BufReminderRMX_SavePersistency() "function to save buffers persistency information

    let file_lines_list = []

    for buf_and_tab_info in g:buf_info_lst

        call add (file_lines_list,   'tab_id:' . buf_and_tab_info['tab_id'] )

        for buf_info in buf_and_tab_info['buff_info']
            let pers_line   =  buf_info['buf_name']   . ' '
            let pers_line  .=  buf_info['split_opt']  . ' '
            let pers_line  .=  buf_info['win_id']     . ' '
            let pers_line  .=  buf_info['win_width']  . ' '
            let pers_line  .=  buf_info['win_height']
            call add (file_lines_list, 'buff_info:' . pers_line)
        endfor
        call add (file_lines_list,   'viewport:' . buf_and_tab_info['tab_wins_info'])
        call add (file_lines_list,   '---end of tab data---')
    endfor

    if !empty(g:buf_info_lst)
        call writefile(file_lines_list, g:BuffReminder_persistency_file)
    endif

endfunc

func BufReminderRMX_OpenBuffer(fName, mode, win_id)

    if a:mode == 'edit'
        exe 'edit ' a:fName

    elseif a:mode == 'split' || a:mode == 'vsplit'
        exe g:BuffReminderRMX_Default_SplitMode
        "exe a:mode
        "move to window with win_id
        exe a:win_id 'wincmd w' 
        exe 'edit ' a:fName

    elseif a:mode == 'hide'
        exe 'edit ' a:fName
        "exe 'hide' "hide buffer ( h flag )
        "
    else
        Decho("BufReminderRMX_OpenBuffer error unknown options! opt=" . a:mode)

    endif

endfunc

fun! BufReminderRMX_reloadView()
    exe 'tabnext ' . g:buf_default_view_pos[0]
    exe g:buf_default_view_pos[1] . 'wincmd w'
endfunc

func! BufReminderRMX_SaveEvent()
    call BufReminderRMX_saveHiddenBuffers() "get all hiden buffers
    call BufReminderRMX_GetActualfBuffInfo() "get information about current opened buffers
    call BufReminderRMX_RemovePersFile()
    call BufReminderRMX_SavePersistency()
endfunc

func! BufReminderRMX_LoadEvent()
    call BufReminderRMX_LoadPersistency()
    call BufReminderRMX_OpenBuffersInList()
    call BufRemionderRMX_ClearList()
    call BufReminderRMX_reloadView()
endfunc

func! BufRemionderRMX_ClearList()
    let g:buf_info_lst = [] "clear list
endfunc


function BufHidden(bufnr)
    return empty(filter(map(range(1, tabpagenr('$')),
                \'tabpagebuflist(v:val)'),
                \'index(v:val, a:bufnr)!=-1'))
endfunction

"//- EVENT DECLARATION -------------------------------------------------------------------
autocmd VimLeavePre *        call BufReminderRMX_SaveEvent()
autocmd VimEnter    * nested call BufReminderRMX_LoadEvent()
"//---------------------------------------------------------------------------------

