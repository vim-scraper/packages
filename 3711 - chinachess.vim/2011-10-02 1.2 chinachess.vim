"             chinachess vim
"             version 1.2
"             author  jumping
"             email www.ping@gmail.com

syn match red '܇\|�R\|�h\|��\|��\|��\|��'
syn match black '��\|��\|��\|ʿ\|��\|��\|��'
syn match hejie '��\|��'
set background=light
highlight black     gui=bold guifg=Black guibg=LightGray
highlight red       gui=bold guifg=Red guibg=LightGray
highlight hejie     gui=italic guifg=LightGreen guibg=LightGray
highlight Normal    gui=None guifg=Black guibg=LightGray
highlight Cursor        gui=None guibg=cyan3 guifg=White

"ParseChessFile {{{
function ParseChessFile()
    normal gg
    let step = 1
    let curCol = line(".") 
    let curLine = getline(".")
    let rxChessMove = '\(˧\|܇\|�R\|�h\|��\|��\|��\|��\|��\|��\|��\|ʿ\|��\|��\|��\|��\|��\|��\|��\|��\|��\|��\|��\|��\|��\|��\|��\|��\|��\|��\|��\|��\|һ\|ƽ\|��\|��\|ǰ\|��\)\{4\}'  
    let rxKeyLine = '^\s*\[.*\]'
    let rxComLine = '^\s*{\|^\s*('
    let lastLine = line("$")
    while curCol <= lastLine
        " result line
        if curLine =~ '^\s*\d-\d'
            if curLine =~ "^\s*1-0"
                let b:result = "��ʤ"
            else
                if curLine =~ "^\s*0-1"
                    let b:result = "��ʤ"
                else
                    let b:result = "�;�"
                endif
            endif
        endif

        " key line
        if curLine =~ rxKeyLine
            normal j
            let curLine = getline(".")
            let curCol = line(".")
            continue
        endif

        " comment line
        if curLine =~ rxComLine
            if curLine =~ '^\s*{'
                call searchpair('{','','}')
            else
                call searchpair('(','',')')
            endif
            normal j
            let curLine = getline(".")
            let curCol = line(".")
            continue
        endif

        " space line
        if curLine =~ '^\s*$'
            normal j
            let curLine = getline(".")
            let curCol = line(".")
            continue
        endif

       let posCol =match(curLine,rxChessMove, 0)  
       while posCol >= 0
           call add(b:chessStep, strpart(curLine, posCol, 8))
           let posCol =match(curLine,rxChessMove,posCol +8)   
           let step += 1
       endwhile

       let temp = line(".")
       normal j
       let curLine = getline(".")
       let curCol = line(".")
       if temp == curCol
           break
       endif
    endwhile        
endfunction
"}}}

let s:line = []
call add(s:line,"  ��   ��   ��   ��   ��   ��   ��   ��   �� ")
call add(s:line," [��]-[��]-[��]-[ʿ]-[��]-[ʿ]-[��]-[��]-[��]")
call add(s:line,"  |    |    |    | �v | �u |    |    |    |  ")
call add(s:line,"  |----+----+----+----+----+----+----+----|  ")
call add(s:line,"  |    |    |    | �u | �v |    |    |    |  ")
call add(s:line,"  |---[��]--+----+----+----+----+---[��]--|  ")
call add(s:line,"  |    |    |    |    |    |    |    |    |  ")
call add(s:line," [��]--+---[��]--+---[��]--+---[��]--+---[��]")
call add(s:line,"  |    |    |    |    |    |    |    |    |  ")
call add(s:line,"  |---------------------------------------|  ")
call add(s:line,"  |        ��                  ��         |  ")
call add(s:line,"  |---------------------------------------|  ")
call add(s:line,"  |    |    |    |    |    |    |    |    |  ")
call add(s:line," (��)--+---(��)--+---(��)--+---(��)--+---(��)")
call add(s:line,"  |    |    |    |    |    |    |    |    |  ")
call add(s:line,"  |---(�h)--+----+----+----+----+---(�h)--|  ")
call add(s:line,"  |    |    |    | �v | �u |    |    |    |  ")
call add(s:line,"  |----+----+----+----+----+----+----+----|  ")
call add(s:line,"  |    |    |    | �u | �v |    |    |    |  ")
call add(s:line," (܇)-(�R)-(��)-(��)-(��)-(��)-(��)-(�R)-(܇)")
call add(s:line,"  ��   ��   ��   ��   ��   ��   ��   ��   һ ")

let s:actionP = "ƽ"
let s:actionJ = "��"
let s:actionT = "��"
let s:postionQ = "ǰ"
let s:postionH = "��"

let s:redChessK = "��" 
let s:redChessA = "��" 
let s:redChessB = "��" 
let s:redChessN = "�R" 
let s:redChessR = "܇" 
let s:redChessC = "�h" 
let s:redChessP = "��" 

let s:blackChessK = "��" 
let s:blackChessA = "ʿ" 
let s:blackChessB = "��" 
let s:blackChessN = "��" 
let s:blackChessR = "��" 
let s:blackChessC = "��" 
let s:blackChessP = "��" 

let b:chessStep = []
let b:step = 0
let b:result = "��"

call ParseChessFile()


"DrawChessboard {{{
function DrawChessboard()
    set lines=22
    set columns=45
    normal ggVGd
    for s:item in s:line 
        exec "normal o".s:item
    endfor
    normal ggdd
endfunction
"}}}

"DrawNext {{{
function DrawNext()
    normal gg
    if b:step >= len(b:chessStep)
        echo b:result
        return
    else 
        echo b:chessStep[b:step]
    endif
    call MoveChess(b:chessStep[b:step])
    let b:step +=1
endfunction
"}}}

"MoveChess {{{
function MoveChess(chessStep)
    let s:curChess = strpart(a:chessStep,0,2)
    let s:curStep =strpart(a:chessStep,2,2)
    if stridx(s:line[0],s:curStep) == -1 && stridx(s:line[20],s:curStep) == -1 
        let s:curChess = s:curStep
        let s:curStep = strpart(a:chessStep,0,2)
    endif 
    let s:action = strpart(a:chessStep,4,2)
    let s:nextStep = strpart(a:chessStep,6,2)
    if b:step%2 == 0
        if s:curChess == '��'
            let s:curChess = '�h'
        endif
        if s:curChess == '��'
            let s:curChess = '�R'
        endif
        if s:curChess == '��'
            let s:curChess = '܇'
        endif
        if s:curChess == 'ʿ'
            let s:curChess = '��'
        endif
        if s:curChess == '˧'
            let s:curChess = '��'
        endif
    endif
    "�õ���ǰ����λ��   
    let [s:lnum, s:col] = searchpos(s:curChess,'n')
    if s:lnum ==0 && s:col ==0
        echo b:result
        return
    endif


    let [s:lnum1, s:col1] = searchpos(s:curStep,'n')
    if s:lnum1 ==0 && s:col1 ==0
        " curStep �� ǰ ���� �� �����Ӷ�λ 
        if s:curStep == s:postionQ && b:step%2 !=0
            call setpos('.',[0,s:lnum,s:col,0])
            let [s:lnum, s:col] = searchpos(s:curChess,'n')
        endif
        if s:curStep == s:postionH && b:step%2 ==0
            call setpos('.',[0,s:lnum,s:col,0])
            let [s:lnum, s:col] = searchpos(s:curChess,'n')
        endif
        let s:col1 = s:col "�ҵ���ǰ����
    else
        " ���壬��һ�У��������ˣ�����������������
        if s:lnum == 2 && b:step%2 != 0 && s:action == s:actionT
            call setpos('.',[0,s:lnum,s:col,0])
            let [s:lnum, s:col] = searchpos(s:curChess,'n')
        endif

        "���壬��16 �У������ǽ�������� �� ������������������
        if s:lnum == 16 && b:step%2 == 0 && s:action == s:actionJ && s:curChess == s:redChessA
            call setpos('.',[0,s:lnum,s:col,0])
            let [s:lnum, s:col] = searchpos(s:curChess,'n')
        endif

    endif
    while s:col1 != s:col 
        call setpos('.',[0,s:lnum,s:col,0])
        let [s:lnum, s:col] = searchpos(s:curChess,'n')
    endwhile

    call setpos('.',[0,s:lnum,s:col-1,0])
    "�����ǰ����
    if s:lnum == 20 || s:lnum == 2 || s:lnum == 10 || s:lnum == 12
        if s:col == 3
            exe ':substitute/\%'.(s:col-1).'c.*\%'.(s:col+3).'c/ +--' 
        else
            if s:col == 43
                exe ':substitute/\%'.(s:col-1).'c.*\%'.(s:col+3).'c/-+  ' 
            else
                exe ':substitute/\%'.(s:col-1).'c.*\%'.(s:col+3).'c/----' 
            endif
        endif
    else
        if s:col == 3 
            exe ':substitute/\%'.(s:col-1).'c.*\%'.(s:col+3).'c/ |--' 
        else
            if s:col == 43
                exe ':substitute/\%'.(s:col-1).'c.*\%'.(s:col+3).'c/-|  ' 
            else
                exe ':substitute/\%'.(s:col-1).'c.*\%'.(s:col+3).'c/-+--' 
            endif
        endif
    endif
        
    normal gg
    let s:pos = s:GetNextPos(s:curChess,s:action,s:nextStep)
    call setpos('.',s:pos)

    " ���ƶ��������
    normal h
    if b:step%2 ==0
        let curLine = getline('.')
        exe ':substitute/\%'.(s:pos[2]-1).'c.*\%'.(s:pos[2]+3).'c/('.s:curChess.')' 
    else
        exe ':substitute/\%'.(s:pos[2]-1).'c.*\%'.(s:pos[2]+3).'c/['.s:curChess.']' 
    endif
    call setpos('.',s:pos)
    normal h

endfunction
"}}}

" GetNextPos {{{
function s:GetNextPos(chess,action,next)
    let [s:lnum2,s:col2] = searchpos(a:next,'n')
    let ret = [0,s:lnum,s:col,0]
    "�� ��
    if a:chess == s:redChessK || a:chess == s:redChessP
         if a:action == s:actionJ
             let ret[1] -= 2
         endif
         if a:action == s:actionT
             let ret[1] += 2
         endif
         if a:action == s:actionP
             let ret[2] = s:col2
         endif
    endif
    "black
    if a:chess == s:blackChessK || a:chess == s:blackChessP
         if a:action == s:actionJ
             let ret[1] += 2
         endif
         if a:action == s:actionT
             let ret[1] -= 2
         endif
         if a:action == s:actionP
             let ret[2] = s:col2
         endif
    endif

    "��
    if a:chess == s:redChessA
        let ret[2] = s:col2
         if a:action == s:actionJ
             let ret[1] -= 2 
         endif
         if a:action == s:actionT
             let ret[1] += 2
         endif
    endif
    "black
    if a:chess == s:blackChessA
        let ret[2] = s:col2
         if a:action == s:actionJ
             let ret[1] += 2 
         endif
         if a:action == s:actionT
             let ret[1] -= 2
         endif
    endif

    "��
    if a:chess == s:redChessB
        let ret[2] = s:col2
         if a:action == s:actionJ
             let ret[1] -= 4 
         endif
         if a:action == s:actionT
             let ret[1] += 4
         endif
    endif
    "black
    if a:chess == s:blackChessB
        let ret[2] = s:col2
         if a:action == s:actionJ
             let ret[1] += 4 
         endif
         if a:action == s:actionT
             let ret[1] -= 4
         endif
    endif

    "�R
    if a:chess == s:redChessN
        let ret[2] = s:col2
         if a:action == s:actionJ
             if abs(s:col1-s:col2) == 10
                 let ret[1] -= 2
             else
                 let ret[1] -= 4
             endif 
         endif
         if a:action == s:actionT
             if abs(s:col1-s:col2) == 10
                 let ret[1] += 2
             else
                 let ret[1] += 4
             endif 
         endif
    endif
    "black
    if a:chess == s:blackChessN
        let ret[2] = s:col2
         if a:action == s:actionJ
             if abs(s:col1-s:col2) == 10
                 let ret[1] += 2
             else
                 let ret[1] += 4
             endif 
         endif
         if a:action == s:actionT
             if abs(s:col1-s:col2) == 10
                 let ret[1] -= 2
             else
                 let ret[1] -= 4
             endif 
         endif
    endif

    "܇ �h
    if a:chess == s:redChessR || a:chess == s:redChessC
         if a:next == 'һ' 
             let offset = 2
         endif
         if a:next == '��' 
             let offset = 4
         endif
         if a:next == '��' 
             let offset = 6
         endif
         if a:next == '��' 
             let offset = 8
         endif
         if a:next == '��' 
             let offset = 10
         endif
         if a:next == '��' 
             let offset = 12
         endif
         if a:next == '��' 
             let offset = 14
         endif
         if a:next == '��' 
             let offset = 16
         endif
         if a:next == '��' 
             let offset = 18
         endif
         if a:action == s:actionJ
             let ret[1] -= offset 
         endif
         if a:action == s:actionT
             let ret[1] += offset
         endif
         if a:action == s:actionP
             let ret[2] = s:col2
         endif
    endif
    "black
    if a:chess == s:blackChessR || a:chess == s:blackChessC
         if a:next == '��' 
             let offset = 2
         endif
         if a:next == '��' 
             let offset = 4
         endif
         if a:next == '��' 
             let offset = 6
         endif
         if a:next == '��' 
             let offset = 8
         endif
         if a:next == '��' 
             let offset = 10
         endif
         if a:next == '��' 
             let offset = 12
         endif
         if a:next == '��' 
             let offset = 14
         endif
         if a:next == '��' 
             let offset = 16
         endif
         if a:next == '��' 
             let offset = 18
         endif
         if a:action == s:actionJ
             let ret[1] += offset 
         endif
         if a:action == s:actionT
             let ret[1] -= offset
         endif
         if a:action == s:actionP
             let ret[2] = s:col2
         endif
    endif
    return ret
endfunction
"}}}

"key map {{{
map <silent><A-n>  :call DrawNext()<CR>
map <silent><A-d>  :call DrawChessboard()<CR>
map <silent><A-p>  :let b:step -=1<cr>:normal u<cr>
map <silent><A-q>  :q!<CR>
"}}}
