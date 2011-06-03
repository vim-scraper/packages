"Date: Feb 9th 2009
"Version: 1.00
"bugs to: arunke@yahoo.com
"
" The mastermind game implemented using vim scripting language.
"
" The idea of the game is to guess a N-digit code. Correct position
" and correct number would get a 'X' and incorrect position, but
" correct number would get a 'x'.
" 
" Note: Position of X/x in the guess hints are random.
"
" Key Mappings:
"     <keys 1-9> : Enters respective key
"        <enter> : Press this, when done with the current line
"        <space> : Skip to enter next number
"    <backspace> : Skip to enter previous number
"          <c-r> : Restart the game
"          <c-n> : Change the number of digits to guess (1-9)
"          <c-g> : Change the number of guesses (1-20)
"              q : To quit the game
"              b : Boss Key (switches b/w first buffer and game)
"
if version < 700
    echo "Sorry! This game requires vim version at least 7.00"
    finish
endif

set nocp

fun! MM_HighLight()
    exe 'syn match m1      "\%>'.s:msg_lines.'l\%<32c1"'
    exe 'syn match m2      "\%>'.s:msg_lines.'l\%<32c2"'
    exe 'syn match m3      "\%>'.s:msg_lines.'l\%<32c3"'
    exe 'syn match m4      "\%>'.s:msg_lines.'l\%<32c4"'
    exe 'syn match m5      "\%>'.s:msg_lines.'l\%<32c5"'
    exe 'syn match m6      "\%>'.s:msg_lines.'l\%<32c6"'
    exe 'syn match m7      "\%>'.s:msg_lines.'l\%<32c7"'
    exe 'syn match m8      "\%>'.s:msg_lines.'l\%<32c8"'
    exe 'syn match m9      "\%>'.s:msg_lines.'l\%<32c9"'
    exe 'syn match mexact  "\%>'.s:msg_lines.'l\%<32cX"'
    exe 'syn match malmost "\%>'.s:msg_lines.'l\%<32cx"'
    exe 'syn match mb      "\%>'.s:msg_lines.'l\%<32c|"'
    exe 'syn match md      "\%>'.s:msg_lines.'l\%<32c-"'
    exe 'syn match mp      "\%>'.s:msg_lines.'l\%<32c+"'
    exe 'syn match mT      "\%>'.s:msg_lines.'l\%<32cT"'
    hi m1      guifg=black ctermfg=black ctermbg=1 guibg=red
    hi m2      guifg=black ctermfg=black ctermbg=2 guibg=blue
    hi m3      guifg=black ctermfg=black ctermbg=3 guibg=yellow
    hi m4      guifg=black ctermfg=black ctermbg=4 guibg=white
    hi m5      guifg=black ctermfg=black ctermbg=5 guibg=magenta
    hi m6      guifg=black ctermfg=black ctermbg=6 guibg=green
    hi m7      guifg=black ctermfg=black ctermbg=7 guibg=grey
    hi m8      guifg=black ctermfg=white ctermbg=1 guibg=orange
    hi m9      guifg=black ctermfg=white ctermbg=2 guibg=darkgreen
    hi mexact  guifg=green ctermfg=2
    hi malmost guifg=red ctermfg=1
    hi mb      guifg=blue  ctermfg=4
    hi md      guifg=blue  ctermfg=4
    hi mp      guifg=blue  ctermfg=4
    hi mT      guifg=blue  ctermfg=4
    hi Normal  guibg=black guifg=grey65
    hi NonText guibg=black guifg=grey65
endf

fun! MM_Exit()
    exe 'set virtualedit='.b:ve
    exe 'highlight Normal guibg='.b:NormBGC.' guifg='.b:NormFGC
    exe 'highlight NonText guibg='.b:NonTxtBGC.' guifg='.b:NonTxtFGC
    unlet s:mastermind_on
    if (b:b_map != '')
        exe "nn <silent> b ".b:b_map
    else
        unmap b
    endif
    bw!
endf

fun! MM_Setup()
    new
    let b:ve=&virtualedit
    let b:b_map=maparg('b', 'n')
    let b:NormBGC=synIDattr(synIDtrans(hlID("Normal")), "bg")
    let b:NormFGC=synIDattr(synIDtrans(hlID("Normal")), "fg")
    let b:NonTxtBGC=synIDattr(synIDtrans(hlID("NonText")), "bg")
    let b:NonTxtFGC=synIDattr(synIDtrans(hlID("NonText")), "fg")
    set virtualedit=all buftype=nofile bufhidden=hide noswapfile
    sil! file <<mastermind>>

    let s:mastermind_on=bufnr('%')
    let s:Ncols=5
    let s:Nguesses=12
    let s:StartULine=4+s:msg_lines
    let s:StartUCol=11
    let s:Cheat=0

    call MM_HighLight()

    "let i=0
    "while(i<128)
    "    sil! exe "nn <buffer> ".nr2char(i).
    "        \   " :echo 'Invalid key ".nr2char(i)."'<cr>"
    "    let i=i+1
    "endwhile
    for k in range(1,9)
        exe 'nn <buffer> <silent> '.k.' :call MM_ProcessCol('.k.')<cr>'
    endfor
    nn <buffer> <silent> <space> :call MM_ProcessCol('sp')<cr>
    nn <buffer> <silent> <c-h> :call MM_ProcessCol('bs')<cr>
    nn <buffer> <silent> <bs> :call MM_ProcessCol('bs')<cr>
    nn <buffer> <silent> <cr> :call MM_ProcessEnter()<cr>
    nn <buffer> <silent> <c-r> :call MM_Start('restart')<cr>
    nn <buffer> <silent> <c-n> :call MM_Start('ncols')<cr>
    nn <buffer> <silent> <c-g> :call MM_Start('nguesses')<cr>
    nn <buffer> <silent> <c-k> :call MM_Start('cheat')<cr>
    nn <buffer> <silent> h :echo MM_mappings<cr>
    nn <buffer> <silent> q :call MM_Exit()<cr>
    nn <buffer> <silent> <c-q> :call MM_Exit()<cr>
    nn <silent> b :call MM_BossKey()<cr>
endf

fun! MM_BossKey()
    set hidden
    if s:mastermind_on == bufnr('%')
        exe "norm! mM1\<c-^>"
    else
        exe "norm! `M"
    endif
endf

fun! MM_Init()
    let s:DoneRound=0
    let s:CurLine=1     "MM_DrawU modifies this
    let s:CurCol=1      "MM_DrawU modifies this
    let s:StartCol=1    "MM_DrawU modifies this
    let s:AllCols=['1', '2', '3', '4', '5', '6', '7', '8', '9']
    let s:GameCols=[]
    let s:LastIpLine=[]
    for i in range(0, s:Ncols-1)
        call add(s:GameCols, s:AllCols[i])
    endfor
    let s:Code=[]
    for i in range(0, s:Ncols-1)
        let rbase=(reltime()[1]/1024)+(reltime()[1]*1024)
        call add(s:Code, s:GameCols[rbase%s:Ncols])
    endfor
endf

fun! MM_Mesg(off, msg, save)
    let msg=a:msg
    if a:save|exe "norm! m`"|endif
    if msg == ''
        let msg=repeat(' ', (80-34+1))
    endif
    exe (s:StartULine+a:off)."norm! 34|R".msg."\<Esc>"
    if a:save|exe "norm! ``"|endif
    set nomodified
endf

fun! MM_ProcessEnter()
    if s:DoneRound| return| endif
    let s:CurIpLine=split(substitute(getline(s:CurLine), '|', '', 'g'), '\W\+')
    if len(s:CurIpLine) < s:Ncols|return|endif
    if s:CurIpLine == s:LastIpLine|return|endif
    let s:LastIpLine=s:CurIpLine
    let res=""
    let RemIp=[]
    let RemCode=[]
    for i in range(0, s:Ncols-1)
        if (s:CurIpLine[i] == s:Code[i])
            let res=res.'X'
        else
            call add(RemIp, s:CurIpLine[i])
            call add(RemCode, s:Code[i])
        endif
    endfor
    for i in range(0, len(RemIp)-1)
        let ind=index(RemCode, RemIp[i])
        if ind >= 0
            let res=res.'x'
            call remove(RemCode, ind)
        endif
    endfor
    exe "norm! ".s:CurLine."G^h".s:Ncols."hR".res
    if (len(RemIp) == 0)
        "exe s:StartULine."norm! 34|RCongrats!\<Esc>"
        call MM_Mesg(0, "Congrats!", 0)
        let s:DoneRound=1
    else
        if s:CurLine == s:StartULine + 1
            call MM_Mesg(0, "Better luck next time!", 0)
            call MM_Mesg(1, "Correct code was: ".join(s:Code, ', '), 0)
            let s:DoneRound=1
        else
            let s:CurLine=s:CurLine-1
            let s:CurCol=s:StartCol
            exe s:CurLine."norm!".s:CurCol."|R".join(s:CurIpLine, ' ')."\<esc>"
            exe "norm!".s:CurCol."|"
        endif
    endif
    se nomodified
endf

fun! MM_ProcessCol(col)
    if s:DoneRound| return| endif
    let col=a:col
    exe s:CurLine
    if (col =~ '\d')
        exe "norm!".s:CurCol."|r".col
    endif
    if (col =~ 'bs')
        let s:CurCol=(s:CurCol == s:StartCol) ? s:LastCol : s:CurCol-2
    else "space/numbers
        let s:CurCol=(s:CurCol == s:LastCol) ? s:StartCol : s:CurCol+2
    endif
    exe "norm!".s:CurCol."|"
    set nomodified
endf

let s:welcome_msg="
\          !!! Welcome to Mastermind, the game of guessing !!!\n
\\n
\You have to guess the correct N-digit code. Correct position\n
\and correct number would get a 'X' and incorrect position, but\n
\correct number would get a 'x'.\n
\\n
\Note: Position of X/x in the guess hints are random.\n
\\n
\Press 'h' key for key mappings\n
\"

let MM_mappings="
\Key Mappings:\n
\    <keys 1-9> : Enters respective key\n
\       <enter> : Press this, when done with the current line\n
\       <space> : Skip to enter next number\n
\          <bs> : Skip to enter previous number\n
\         <c-r> : Restart the game\n
\         <c-n> : Change the number of digits to guess (1-9)\n
\         <c-g> : Change the number of guesses (1-20)\n
\             q : To quit the game\n
\             b : (underscore) Boss Key (switches b/w first buffer and game)\n
\"

let s:msg_lines=strlen(substitute(s:welcome_msg, '.\{-}\n', 'x', 'g'))

fun! MM_DrawU(startL, startC, Ncols, Nguesses)
    exe '%d'
    pu =substitute(s:welcome_msg, 'N-digit', s:Ncols.'-digit', '')
    let startL=a:startL
    let startC=a:startC
    let Ncols=a:Ncols
    let Nlines=a:Nguesses+1
    while line('$') < startL
        $pu _
    endwhile
    exe "norm!".startC."|rT".(startC+Ncols*2+1)."|R T\<esc>"
    "fyi! virtualedit is all
    while line('$') < startL+Nlines-1
        exe "$pu _|norm!".startC."|r|".(startC+Ncols*2+1)."|R |\<esc>"
    endwhile
    let s:CurLine=line('.') "Game starts on this line
    norm! ^ll
    let s:StartCol=col('.')   "Game starts on this col
    let s:CurCol=s:StartCol
    norm! $hh
    let s:LastCol=col('.')
    $pu _
    exe "norm!".startC."|R+".repeat('-', Ncols*2+1)."+\<esc>k^llm`"
    set nomodified
endf

fun! MM_Start(arg)
    if (a:arg == 'start')
        call MM_Setup()
    elseif (a:arg == 'cheat')
        let s:Cheat=s:Cheat ? 0 : 1
        if s:Cheat
            call MM_Mesg(0, "Code is: ".join(s:Code, ', '), 1)
        else
            call MM_Mesg(0, "", 1)
        endif
        return
    elseif (a:arg == 'ncols')
        let n=input('Enter desired number of columns(1-9): ')
        if (n !~ '^[1-9]')
            call MM_Mesg(0, "Invalid number", 0)
            redraw|sleep 500m
            call MM_Mesg(0, "", 0)
            norm ``
            return
        endif
        let s:Ncols=n
    elseif (a:arg == 'nguesses')
        let n=input('Enter desired number of allowable guesses (max 20): ')
        if n !~ '^[0-9]\+$' || n > 20
            call MM_Mesg(0, "Invalid number", 0)
            redraw|sleep 500m
            call MM_Mesg(0, "", 0)
            norm ``
            return
        endif
        let s:Nguesses=n
    endif
    call MM_Init()
    call MM_DrawU(s:StartULine, s:StartUCol, s:Ncols, s:Nguesses)
endf

nn <Leader>mm :call MM_Start('start')<cr>
