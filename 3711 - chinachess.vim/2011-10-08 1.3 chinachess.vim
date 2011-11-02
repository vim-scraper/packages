"             chinachess vim
"             version 1.2
"             author  jumping
"             email www.ping@gmail.com

" ��ɫ���� {{{
syn match red '܇\|�R\|�h\|��\|��\|��\|��\|(\|)'
syn match black '��\|��\|��\|ʿ\|��\|��\|��\|\[\|\]'
syn match gray '��\|��\|+\|-\||\|x\|#\|�v\|�u'
set background=light
highlight black     gui=bold guifg=Black guibg=LightGray
highlight red       gui=bold guifg=Red guibg=LightGray
highlight gray      gui=None guifg=DarkGray guibg=LightGray
highlight Normal    gui=None guifg=Black guibg=LightGray
hi clear MatchParen
highlight MatchParen gui=None guibg=Gray
highlight Cursor    gui=None guifg=LightGray guibg=Blue
"}}}

"��ʼ���� {{{
let s:line = []
call add(s:line,"  ��   ��   ��   ��   ��   ��   ��   ��   �� ")
call add(s:line,"  +---------------------------------------+  ")
"call add(s:line," [��]-[��]-[��]-[ʿ]-[��]-[ʿ]-[��]-[��]-[��]")
call add(s:line,"  |    |    |    | �v | �u |    |    |    |  ")
call add(s:line,"  |----+----+----+----x----+----+----+----|  ")
call add(s:line,"  |    |    |    | �u | �v |    |    |    |  ")
call add(s:line,"  |----#----+----+----+----+----+----#----|  ")
"call add(s:line,"  |---[��]--+----+----+----+----+---[��]--|  ")
call add(s:line,"  |    |    |    |    |    |    |    |    |  ")
call add(s:line,"  |----+----#----+----#----+----#----+----|  ")
"call add(s:line," [��]--+---[��]--+---[��]--+---[��]--+---[��]")
call add(s:line,"  |    |    |    |    |    |    |    |    |  ")
call add(s:line,"  |---------------------------------------|  ")
call add(s:line,"  |        ��                  ��         |  ")
call add(s:line,"  |---------------------------------------|  ")
call add(s:line,"  |    |    |    |    |    |    |    |    |  ")
call add(s:line,"  |----+----#----+----#----+----#----+----|  ")
"call add(s:line," (��)--+---(��)--+---(��)--+---(��)--+---(��)")
call add(s:line,"  |    |    |    |    |    |    |    |    |  ")
call add(s:line,"  |----#----+----+----+----+----+----#----|  ")
"call add(s:line,"  |---(�h)--+----+----+----+----+---(�h)--|  ")
call add(s:line,"  |    |    |    | �v | �u |    |    |    |  ")
call add(s:line,"  |----+----+----+----x----+----+----+----|  ")
call add(s:line,"  |    |    |    | �u | �v |    |    |    |  ")
call add(s:line,"  +---------------------------------------+  ")
"call add(s:line," (܇)-(�R)-(��)-(��)-(��)-(��)-(��)-(�R)-(܇)")
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
let s:curChessInfo=[0,0,'']

let b:chessStep = []
let b:step = 0
let b:result = "��"
let b:FEN = ""
"}}}

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
            if curLine =~ '^\s*[\s*FEN'
                let b:FEN = substitute(curLine, '.*"\(.*\)",*', '\1', "")
            endif
            if curLine =~ '^\s*[\s*Result'
                let b:result = substitute(curLine, '.*"\(.*\)".*', '\1', "")

                if b:result == "1-0"
                    let b:result = "��ʤ"
                else
                    if b:result == "0-1"
                        let b:result = "��ʤ"
                    else
                        let b:result = "�;�"
                    endif
                endif
                
            endif
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
            let temp = line(".")
            normal j
            let curLine = getline(".")
            let curCol = line(".")
            if temp == curCol
                break
            else
                continue
            endif
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

"ClearChess {{{
function ClearChess(line,col)
    
    call setpos('.',[0,a:line,a:col-1,0])
    "�����ǰ����
    if a:line == 20 || a:line == 2          
        "��һ�� �� ���һ��
        if a:col == 3
            exe ':substitute/\%'.(a:col-1).'c.*\%'.(a:col+3).'c/ +--' 
        else
            if a:col == 43
                exe ':substitute/\%'.(a:col-1).'c.*\%'.(a:col+3).'c/-+  ' 
            else
                exe ':substitute/\%'.(a:col-1).'c.*\%'.(a:col+3).'c/----' 
            endif
        endif

    else

        if a:line == 10 || a:line == 12
        "���Աߵ�����
            if a:col == 3
                exe ':substitute/\%'.(a:col-1).'c.*\%'.(a:col+3).'c/ |--' 
            else
                if a:col == 43
                    exe ':substitute/\%'.(a:col-1).'c.*\%'.(a:col+3).'c/-|  ' 
                else
                    exe ':substitute/\%'.(a:col-1).'c.*\%'.(a:col+3).'c/----' 
                endif
            endif

        else
            if (a:line == 4 || a:line == 18 )&& a:col ==23
                exe ':substitute/\%'.(a:col-1).'c.*\%'.(a:col+3).'c/-x--' 
            else
                if ((a:line == 6 || a:line == 16) && (a:col == 8 || a:col == 38))||((a:line == 8 || a:line == 14) && (a:col == 13 || a:col == 23 || a:col == 33))
                    exe ':substitute/\%'.(a:col-1).'c.*\%'.(a:col+3).'c/-#--' 
                else

                    " ��������
                    if a:col == 3 
                        exe ':substitute/\%'.(a:col-1).'c.*\%'.(a:col+3).'c/ |--' 
                    else
                        if a:col == 43
                            exe ':substitute/\%'.(a:col-1).'c.*\%'.(a:col+3).'c/-|  ' 
                        else
                            exe ':substitute/\%'.(a:col-1).'c.*\%'.(a:col+3).'c/-+--' 
                        endif
                    endif
                endif
            endif

        endif
    endif
endfunction
"}}}

"DrawChessOnBoard {{{
function DrawChessOnBoard(chess,line,col)
    if a:chess == 'r'
        let chess = '['.s:blackChessR.']'
    endif
    if a:chess == 'n'
        let chess = '['.s:blackChessN.']'
    endif
    if a:chess == 'b'
        let chess = '['.s:blackChessB.']'
    endif
    if a:chess == 'a'
        let chess = '['.s:blackChessA.']'
    endif
    if a:chess == 'k'
        let chess = '['.s:blackChessK.']'
    endif
    if a:chess == 'c'
        let chess = '['.s:blackChessC.']'
    endif
    if a:chess == 'p'
        let chess = '['.s:blackChessP.']'
    endif
    if a:chess == 'R'
        let chess = '('.s:redChessR.')'
    endif
    if a:chess == 'N'
        let chess = '('.s:redChessN.')'
    endif
    if a:chess == 'B'
        let chess = '('.s:redChessB.')'
    endif
    if a:chess == 'A'
        let chess = '('.s:redChessA.')'
    endif
    if a:chess == 'K'
        let chess = '('.s:redChessK.')'
    endif
    if a:chess == 'C'
        let chess = '('.s:redChessC.')'
    endif
    if a:chess == 'P'
        let chess = '('.s:redChessP.')'
    endif
    let s:line[a:line*2+1] = substitute(s:line[a:line*2+1],"\\%".(a:col*5+2)."c.*\\%".(a:col*5+6)."c", chess,"")

endfunction 
"}}}

"DrawChessboard {{{
function DrawChessboard()
    call ParseChessFile()
    set lines=22
    set columns=45
    normal ggVGd
    if b:FEN == ""
        let b:FEN = 'rnbakabnr/9/1c5c1/p1p1p1p1p/9/9/P1P1P1P1P/1C5C1/9/RNBAKABNR'
    endif

    if b:FEN != ""
        let i=0
        let col = 0
        let line = 0
        while i < strlen(b:FEN)
            if b:FEN[i] == '/'
                let line += 1
                let col = 0
                let i += 1
                continue
            else
                if b:FEN[i] == ' '
                    break
                endif

                if b:FEN[i] <= '9' && b:FEN[i] >= '1'
                    let col += b:FEN[i]
                    let i += 1
                    continue
                endif
                
            endif
            call DrawChessOnBoard(b:FEN[i],line,col)
            let col += 1
            let i += 1
        endwhile
    endif

    for s:item in s:line 
        exec "normal o".s:item
    endfor
    normal ggdd
endfunction
"}}}

"DrawNext {{{
function DrawNext()
    if len(b:chessStep) == 0
        return
    endif
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

    call ClearChess(s:lnum,s:col) 

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
    "normal h

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

"DrawPreview  {{{
function DrawPreview()
    if b:step > 0
        let b:step -= 1
        normal u
    endif
endfunction
"}}}

"GetCurChess {{{
function GetCurChess()
    let [temp1,s:curChessInfo[0],s:curChessInfo[1],temp2] = getpos(".")
    let s:curChessInfo[2] = strpart(getline("."),s:curChessInfo[1]-2,4)
    echo s:curChessInfo
endfunction
"}}}

"MoveCurChess {{{
function MoveCurChess()
    let temp = getpos(".")
    call ClearChess(s:curChessInfo[0],s:curChessInfo[1])

    call setpos('.',temp)
    exe ':substitute/\%'.(temp[2]-1).'c.*\%'.(temp[2]+3).'c/'.s:curChessInfo[2]
    call setpos('.',temp)
endfunction
"}}}

"CursorMove {{{
function CursorMove(dir)
    let temp = getpos(".")
    if a:dir == "up"
        let temp[1] -= 2
    endif
    if a:dir == "down"
        let temp[1] += 2
    endif
    if a:dir == "left"
        let temp[2] -= 5
    endif
    if a:dir == "right"
        let temp[2] += 5
    endif
    let temp[1] = (temp[1]-2)/2*2 +2
    let temp[2] = (temp[2]-3)/5*5 +3
    if temp[1]<2
        let temp[1] = 2
    endif
    if temp[1]>20
        let temp[1] = 20
    endif
    if temp[2] <3
        let temp[2] =3
    endif
    if temp[2] >43
        let temp[2] =43
    endif
    call setpos(".",temp)

endfunction
"}}}

"key map {{{
map <silent><A-n>  :call DrawNext()<CR>
map <silent><A-d>  :call DrawChessboard()<CR>
map <silent><A-p>  :call DrawPreview()<CR>
map <silent><A-q>  :q!<CR>
map <silent><SPACE> :call  GetCurChess()<CR>
nmap <silent><CR> :call MoveCurChess()<CR>
nmap <silent><up> :call CursorMove("up")<CR>
nmap <silent><down> :call CursorMove("down")<CR>
nmap <silent><left> :call CursorMove("left")<CR>
nmap <silent><right> :call CursorMove("right")<CR>
"}}}

" vim: fdm=marker 
