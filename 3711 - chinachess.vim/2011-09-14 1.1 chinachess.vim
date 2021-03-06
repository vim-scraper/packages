" version 1.1
syn match red '車\|馬\|砲\|仕\|相\|兵\|帥'
syn match black '车\|马\|炮\|士\|象\|卒\|将'
syn match hejie '河\|界'
set background=light
highlight black     gui=bold guifg=Black guibg=LightGray
highlight red       gui=bold guifg=Red guibg=LightGray
highlight hejie     gui=italic guifg=LightGreen guibg=LightGray
highlight Normal    gui=None guifg=Black guibg=LightGray
highlight Cursor        gui=None guibg=cyan3 guifg=White

let s:line = []
call add(s:line,"  １   ２   ３   ４   ５   ６   ７   ８   ９ ")
call add(s:line," [车]-[马]-[象]-[士]-[将]-[士]-[象]-[马]-[车]")
call add(s:line,"  |    |    |    | ╲ | ╱ |    |    |    |  ")
call add(s:line,"  |----+----+----+----+----+----+----+----|  ")
call add(s:line,"  |    |    |    | ╱ | ╲ |    |    |    |  ")
call add(s:line,"  |---[炮]--+----+----+----+----+---[炮]--|  ")
call add(s:line,"  |    |    |    |    |    |    |    |    |  ")
call add(s:line," [卒]--+---[卒]--+---[卒]--+---[卒]--+---[卒]")
call add(s:line,"  |    |    |    |    |    |    |    |    |  ")
call add(s:line,"  |---------------------------------------|  ")
call add(s:line,"  |        河                  界         |  ")
call add(s:line,"  |---------------------------------------|  ")
call add(s:line,"  |    |    |    |    |    |    |    |    |  ")
call add(s:line," (兵)--+---(兵)--+---(兵)--+---(兵)--+---(兵)")
call add(s:line,"  |    |    |    |    |    |    |    |    |  ")
call add(s:line,"  |---(砲)--+----+----+----+----+---(砲)--|  ")
call add(s:line,"  |    |    |    | ╲ | ╱ |    |    |    |  ")
call add(s:line,"  |----+----+----+----+----+----+----+----|  ")
call add(s:line,"  |    |    |    | ╱ | ╲ |    |    |    |  ")
call add(s:line," (車)-(馬)-(相)-(仕)-(帥)-(仕)-(相)-(馬)-(車)")
call add(s:line,"  九   八   七   六   五   四   三   二   一 ")

let s:actionP = "平"
let s:actionJ = "进"
let s:actionT = "退"
let s:postionQ = "前"
let s:postionH = "后"

let s:redChessK = "帥" 
let s:redChessA = "仕" 
let s:redChessB = "相" 
let s:redChessN = "馬" 
let s:redChessR = "車" 
let s:redChessC = "砲" 
let s:redChessP = "兵" 

let s:blackChessK = "将" 
let s:blackChessA = "士" 
let s:blackChessB = "象" 
let s:blackChessN = "马" 
let s:blackChessR = "车" 
let s:blackChessC = "炮" 
let s:blackChessP = "卒" 

let s:curLine = getline(".")
let b:step = 0
while( match( s:curLine, "^[") != -1  )
    if stridx(s:curLine,"Result") != -1
        let b:result=matchstr(s:curLine,'\(Result "\)\@<=.*"\@=')
        if b:result == "1-0"
            let b:result = "红胜"
        else
            if b:result == "0-1"
                let b:result = "黑胜"
            else
                let b:result = "和局"
            endif
        endif

    endif
    normal j
    let s:curLine= getline(".")
endwhile

let s:lnum = 1
let s:col = 1
let step = 1
let b:chessStep = []
while s:lnum!=0 || s:col != 0
let [s:lnum, s:col] = searchpos('\('.step.'\.\)\@<=\s','n')
if s:col != 0
    call add(b:chessStep, strpart(getline(s:lnum),s:col,8))
    call add(b:chessStep, strpart(getline(s:lnum),s:col+10,8))
    let step += 1
endif
endwhile

map <silent><A-n>  :call DrawNext()<CR>
map <silent><A-d>  :call DrawChessboard()<CR>
map <silent><A-p>  :let b:step -=1<cr>:normal u<cr>

"----------------------------------------------------------------
function DrawChessboard()
    set lines=22
    set columns=45
    normal ggVGd
    for s:item in s:line 
        exec "normal o".s:item
    endfor
    normal ggdd
endfunction

"--------------------------------------------------------------------------------
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
        if s:curChess == '炮'
            let s:curChess = '砲'
        endif
        if s:curChess == '马'
            let s:curChess = '馬'
        endif
        if s:curChess == '车'
            let s:curChess = '車'
        endif
        if s:curChess == '士'
            let s:curChess = '仕'
        endif
        if s:curChess == '帅'
            let s:curChess = '帥'
        endif
    endif
    "得到当前棋子位置   
    let [s:lnum, s:col] = searchpos(s:curChess,'n')
    if s:lnum ==0 && s:col ==0
        echo b:result
        return
    endif

    let [s:lnum1, s:col1] = searchpos(s:curStep,'n')
    if s:lnum1 ==0 && s:col1 ==0
        if s:curStep == s:postionQ && b:step%2 !=0
            call setpos('.',[0,s:lnum,s:col,0])
            let [s:lnum, s:col] = searchpos(s:curChess,'n')
        endif
        if s:curStep == s:postionH && b:step%2 ==0
            call setpos('.',[0,s:lnum,s:col,0])
            let [s:lnum, s:col] = searchpos(s:curChess,'n')
        endif
        let s:col1 = s:col "找到当前棋子
    endif
    while s:col1 != s:col 
        call setpos('.',[0,s:lnum,s:col,0])
        let [s:lnum, s:col] = searchpos(s:curChess,'n')
    endwhile

    call setpos('.',[0,s:lnum,s:col-1,0])
    "清除当前棋子
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

function s:GetNextPos(chess,action,next)
    let [s:lnum2,s:col2] = searchpos(a:next,'n')
    let ret = [0,s:lnum,s:col,0]
    "帥 兵
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

    "仕
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

    "相
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

    "馬
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

    "車 砲
    if a:chess == s:redChessR || a:chess == s:redChessC
         if a:next == '一' 
             let offset = 2
         endif
         if a:next == '二' 
             let offset = 4
         endif
         if a:next == '三' 
             let offset = 6
         endif
         if a:next == '四' 
             let offset = 8
         endif
         if a:next == '五' 
             let offset = 10
         endif
         if a:next == '六' 
             let offset = 12
         endif
         if a:next == '七' 
             let offset = 14
         endif
         if a:next == '八' 
             let offset = 16
         endif
         if a:next == '九' 
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
         if a:next == '１' 
             let offset = 2
         endif
         if a:next == '２' 
             let offset = 4
         endif
         if a:next == '３' 
             let offset = 6
         endif
         if a:next == '４' 
             let offset = 8
         endif
         if a:next == '５' 
             let offset = 10
         endif
         if a:next == '６' 
             let offset = 12
         endif
         if a:next == '７' 
             let offset = 14
         endif
         if a:next == '８' 
             let offset = 16
         endif
         if a:next == '９' 
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
