" VIM goodies for openroad
" Version 1.0
" Tested on Vim 6.1 / Windows NT4 / OpenROAD 4.1 MR (using pre-MR help file)
" Paul Humphrey 
" Wellington Underwriting plc
" 25/7/02

" NB insert this file in your plugins directory

:ab #b /***************************************************************************
:ab #e ****************************************************************************/
:ab #i #    include
:ab #d #    define
:ab pr/ PROCEDURE
:ab me/ METHOD
:ab t/  THEN
:ab w/  WHERE
:ab r/  RETURN
:ab rg/ RETURNING
:ab be/ BEGIN
:ab de/ DECLARE
:ab ed/ ENDDECLARE
:ab in/ INITIALIZE
:ab cf/ CALLFRAME
:ab cp/ CALLPROC
:ab se/ SELECT
:ab up/ UPDATE
:ab ei/ ELSEIF
:ab ef/ ENDFOR;
:ab ew/ ENDWHILE;
:ab ec/ ENDCASE;
:ab el/ ENDLOOP
:ab co/ CONTINUE
:ab cs/ CALL SYSTEM
:ab sd/ SELECT DISTINCT
:ab i/  INSERT INTO
:ab re/ REPEATED
:ab c/  COMMIT;
:ab ro/ ROLLBACK;
:ab ha/ HAVING
:ab gr/ GROUP BY
:ab or/ ORDER BY
:ab v/  varchar(
:ab V/  VARCHAR(
:ab i1/ INTEGER1
:ab i2/ INTEGER2
:ab i4/ INTEGER4
:ab sm/ SMALLINT
:ab f4/ FLOAT4
:ab f8/ FLOAT8
:ab st/ StringObject
:ab cv/ ConcatVarchar(text =
:ab ar/ Array of
:ab dn/ default NULL;
:ab nn/ not NULL
:ab o/  Object
:ab cu/ CurObject
:ab tf/ TableField
:ab en/ EntryField
:ab cl/ ChoiceList
:ab op/ OptionField
:ab fo/ FieldObject

" Colours for fold indicators
hi folded guifg=black gui=bold

" Locate keyword in openroad help - doesn't work with 4.1 MR help file (no 4GL keywords)
map K ! winhlp32 -k<cword> f:\apps32\devapps\or41\ingres\files\english\openroad.hlp<CR>

" find first occurrence in file of current word 
map <F2> *:1<CR>n

" CTRL-SPACE: keyword completion
map  <C-space> <C-n>
map!  <C-space> <C-n>
map  <C-S-space> <C-p>
map!  <C-S-space> <C-p>

" Open folds
map <F3> zR

" Close Folds
map <F4> zM

" Build GOTO menu
map <F5> :$,$call CreateGoToMenu()<CR>

" Rebuild GoTo menus
map <F6> :call RebuildGoToMenus()<CR>

function! RebuildGoToMenus()
    let l = line(".")
    silent! aunmenu Methods
    silent! aunmenu Procedures
    silent! aunmenu Events
    $,$call CreateGoToMenu()
    exe ":".l
endfun

"create menu entries for OpenROAD Procedures, methods & event blocks

function! CreateGoToMenu()
    let i = 1
    let str = ""
    let hasprocmenu = 0
    let hasmethodmenu = 0
    let haseventmenu = 0
    let g:PH_mn = 10     " item number for current entry on main menu
    let g:PH_property = "55."
    let g:PH_more = ".&"
    let g:PH_cnt = 1

    while ( i <= a:lastline )
        let str = getline(i)
 
        if ( MatchAndAdd(i, str, "procedure", "Procedures") == 1 && hasprocmenu == 0 )
            let hasprocmenu = 1
        endif

        if ( MatchAndAdd(i, str, "method", "Methods") == 1 && hasmethodmenu == 0 )
            let hasmethodmenu = 1
        endif

        if ( MatchAndAdd(i, str, "on ", "Events") == 1 && haseventmenu == 0 )
            let haseventmenu = 1
        endif


        let i = i + 1
    endwhile

    if ( hasmethodmenu == 1 )
        tearoff Methods
    endif
    
    if ( hasprocmenu == 1 )
        tearoff Procedures
    endif

    if ( haseventmenu == 1 )
        tearoff Events
    endif
     exe ":1"
endfun

function! MatchAndAdd(line, str, pat, title)
    let pat = "^".a:pat
    let menuflag = 0
    let pos = match(a:str, pat)
    let len = strlen(a:pat)

    if ( pos == 0 )
        let Name = escape(strpart(a:str, pos + len), " .")

        if ( g:PH_mn > 300 )
            let g:PH_property = g:PH_property . "320."
            let g:PH_more = g:PH_more . "More---->.&"
            let g:PH_mn = 10
        endif            

        silent exe "an ".g:PH_property.g:PH_mn." &".a:title.g:PH_more.g:PH_cnt ."-->".Name ":".a:line."<CR>"
        let g:PH_mn = g:PH_mn + 10
        let g:PH_cnt = g:PH_cnt + 1
        let menuflag = 1
    endif

" indicate whether we have created a menu entry    
    return menuflag
endfun

" Fold BEGIN/END blocks - this can be confused by CASE statements
set foldmethod=marker
"set foldmarker=BEGIN,END;

" start with folds open
normal zR

" This will create the shortcut menus automatically when a file is opened
if has("autocmd")
    augroup openroad
        au!
        autocmd  BufReadPost,FileReadPost W*.e* :$,$call CreateGoToMenu()
    augroup END
endif " has("autocmd")


