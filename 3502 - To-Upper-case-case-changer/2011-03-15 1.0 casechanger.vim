

" User manual: 
" For use this script on VB.
" First , add the following line to your vimrc:
" let g:case_basic_source='IF;ELSE;THEN;DO;WHILE;FOR;CALL;'
" Second , add the folloing line to your vimrc
" We use .vb as the language extension name.
" let g:case_changer_pat="*.vb"
"
" Now , while your input if in your .vb file , I will change it to IF ...
"
" Use this script for other language .
" First , add source
" let g:case_{&ft}_source='WHILE;UNTIL;UNLESS...'
" Second , add file extension:
" let g:case_changer_pat="*.vb;*.rb;*.py"
"
" Author : Fuchuan Wang
" mail   : fuchuanwang@gmail.com
"
" version 1.1 change the case while Enter pressed.
" version 1.0 Init
"
if exists('load_casechanger') || &cp
    finish
endif
let load_casechanger = 1

if !exists('g:case_changer_pat')
    let g:case_changer_pat=''
endif

func! s:ChangeLastLine() 
    let lnum = line('.')
    if lnum <= 1
	return
    endif
    let lnum -= 1
    let str = getline(lnum)
    let synname= synIDattr(synIDtrans(synID(lnum,col('$'),1)),"name")
    if synname == "Comment" || synname=="String" || synname=="Constant"
        return
    endif

    let word = substitute(str,'^.*\(\<.*\>\)$','\1','')
    let alen = strlen(str)
    let len = strlen(word)
    for item in s:case_{&ft}_words
        if word ==? item
	    if alen > len 
                call setline(lnum, str[0 : alen -1- len ].item)
	    else
		call setline(lnum,item)
	    endif
        endif
    endfor
endf
func! CaseChanger()
    let str = getline('.')
    if !exists('s:case_{&ft}_words')
        if !exists('g:case_{&ft}_source')
            return
        endif
        let s:case_{&ft}_words=split(g:case_{&ft}_source,';')
    endif

    if str=~ '^\s*$'
	    call s:ChangeLastLine()
    endif

    let icol = col('.')

    if str[icol-2 : icol-1] =~ "[a-zA-Z]"
        return
    endif

    let synname= synIDattr(synIDtrans(synID(line('.'),icol-2,1)),"name")
    if synname == "Comment" || synname=="String" || synname=="Constant"
        return
    endif

    let str2 = str[0 : icol-2]
    let word = substitute(str2,'^.*\(\<.*\>\)\s$','\1','')
    let len = strlen(word)
    for item in s:case_{&ft}_words
        if word ==? item
            if strlen(str2)==len+1
                call setline(line('.'), item.str[icol-2 : -1])
            else
                call setline(line('.'), str[0 : icol-3 - len ].item.str[icol-2 : -1])
            endif
        endif
    endfor
endf

func! CaseChangerInstall()
    au! CursorMovedI <buffer> call CaseChanger()
endf
" add auto commands
for item in split(g:case_changer_pat,';')
    exec 'au BufEnter '.item.'  call CaseChangerInstall()'
endfor
