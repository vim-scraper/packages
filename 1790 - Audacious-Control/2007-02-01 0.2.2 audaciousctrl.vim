" Plugin:       audaciousctrl.vim
" LastChange:   Feb 1, 2007
" Version:      0.2.2
" Author:       Amir Mohammad Saied <amirsaied AT gmail DOT com>
" License:      Public Domain
"
" Usage:
" Just put this file in your ~/.vim/plugin/
" type:
"   \as To get the current song info
"   \an To jump to the next song in the playlist
"   \ap To jump to the previous song in the playlist
"   or :Audacious help for full help
"
" Notes:
" This plugin (idea and concepts) are derived from a great XMMS control Vim
" Plugin, written by `Andrew Rodionoff'.
"
" Changelog:
"  0.2.2
"   - BUGFIX: Fixing a typo which caused deletion of the last item in playlist
"   (Thanks to Milad Rastian)
"  0.2.1
"   - Quit (shutting down Audacious) added.
"   - Centering the playing song in playlist window.
"   - Playlist repeat control added.
"   - BUGFIX: Fixing a nasty error while calling :Audacious with no arguments
"     (Thanks to Milad Rastian)

if v:version < 602 
    finish
endif

if executable("audtool") < 1
    finish
endif

fun! s:Cmd_next()
    call system("audtool --playlist-advance")
    sleep 10m
    call s:Cmd_fetchCurrentSongInfo()
endfun

fun! s:Cmd_prev()
    call system("audtool --playlist-reverse")
    sleep 10m
    call s:Cmd_fetchCurrentSongInfo()
endfun

fun! s:Cmd_pause()
    call system("audtool --playback-pause")
endfun

fun! s:Cmd_play()
    call system("audtool --playback-play")
endfun

fun! s:Cmd_stop()
    call system("audtool --playback-stop")
endfun

fun! s:Cmd_quit()
    call system("audtool --shutdown")
endfun

fun! s:Cmd_toggleShuffle()
    let l:shuffleStatus = substitute(system("audtool --playlist-shuffle-status"), "\n", "", "")

    if l:shuffleStatus == "off"
        echo "Shuffle: ON"
        call system("audtool --playlist-shuffle-toggle")
    else
        echo "Shuffle: OFF"
        call system("audtool --playlist-shuffle-toggle")
    endif
endfun

fun! s:Cmd_toggleRepeat()
    let l:repeatStatus = substitute(system("audtool --playlist-repeat-status"), "\n", "", "")

    if l:repeatStatus == "off"
        echo "Repeat: ON"
        call system("audtool --playlist-repeat-toggle")
    else
        echo "Repeat: OFF"
        call system("audtool --playlist-repeat-toggle")
    endif
endfun

fun! s:Cmd_volume(volume)
    if a:volume == ""
        echo "usage: [+|-]percent"
        return
    endif
    
    call system("audtool --set-volume ".a:volume)
    sleep 15m
    let l:changedVolume = substitute(system("audtool --get-volume"), "\n", "", "")
    echo "Volume: ".l:changedVolume." (".a:volume."%)"
endfun

fun! s:Cmd_fetchCurrentSongInfo()
    let l:position   = substitute(system("audtool --current-song-output-length"), "\n", "", "")
    let l:songLength = substitute(system("audtool --current-song-length"), "\n", "", "")
    let l:plposition = substitute(system("audtool --playlist-position"), "\n", "", "")
    let l:songTitle  = substitute(system("audtool --current-song "), "\n", "", "")
    let l:finalOutput= l:plposition.". ".l:songTitle." [".l:position."/".l:songLength."]"

    if strlen(l:finalOutput) >= &columns
        echo strpart(l:finalOutput, 0, 55)
    else
        echo finalOutput
    endif
endfun

fun! s:Cmd_help()
    echo 'Audacious handlers'
    echo '---------'
    echo ':Audacious playlist,              Show playlist, Press <Enter> to play song on cursor'
    echo ':Audacious next, \an              Next Song'
    echo ':Audacious pause                  Pause'
    echo ':Audacious play                   Play Audacious'
    echo ':Audacious prev, \ap              Previous Song'
    echo ':Audacious toggleShuffle          Toggle Shuffle'
    echo ':Audacious toggleRepeat           Toggle Repeat'
    echo ':Audacious song, \as              Song Number, Title and duration'
    echo ':Audacious stop                   Stop'
    echo ':Audacious setvol[ [+|-]percent], Change volume by [+|-] percent'
    echo ':Audacious quit                   Shuts down Audacious'
    return
endfun

fun! s:Audacious(...)
    let l:cmd = ""
    if a:0
        let l:cmd = a:1
    endif

    if l:cmd == "song"
        return s:Cmd_fetchCurrentSongInfo()
    elseif l:cmd == "playlist"
        return s:Audacious_openPlayList()
    elseif l:cmd == "setvol"
        if a:0 == 2
            return s:Cmd_volume(a:2)
        else
            return s:Cmd_volume("")
        endif
    elseif exists("*s:Cmd_{l:cmd}")
        return s:Cmd_{l:cmd}()
    else
        echo 'usage: Audacious <command>'
        echo 'use Audacious help to get a listing of available commands.'
    endif
endfun

fun! s:Audacious_openPlayList()
    call foreground()
    let l:audaciousWin = bufwinnr('##AUDACIOUS##')
    if l:audaciousWin == -1
        split \#\#AUDACIOUS\#\#
    else
        exec l:audaciousWin . 'wincmd w'
    endif
    call s:Audacious_songSelector()
    redraw
endfun

fun! s:Audacious_songSelector()
    setlocal ma noswf noro bh=wipe nowrap
    silent %d _
    0insert
# Press <Enter> to play song
.
    syn match Comment '^#.*'
    silent r!audtool --playlist-display | sed -e '1d' | sed -e '/^Total length/d'
    setlocal nomodifiable nomodified ro nobuflisted
    mapclear <buffer>
    map <silent> <buffer> <Return> :call <SID>Audacious_goToSong()<CR>
    call s:Audacious_centerSong()
endfun

fun! s:Audacious_goToSong()
    let l:nr = matchstr(getline('.'), '^ *[0-9]\+')
    if l:nr != ''
        call system('audtool playlist-jump ' . l:nr)
        sleep 15m
        call s:Cmd_fetchCurrentSongInfo()
    endif
endfun

fun! s:Audacious_centerSong()
    let l:playListPosition = substitute(system("audtool --playlist-position"), "\n", "", "") + 1
    exe 'normal ' . l:playListPosition . 'zz'
endfun

fun! Audacious_Complete(A,L,P)
    return "help\nnext\npause\nplay\nplaylist\nprev\nquit\nsetvol\nsong\nstop\ntoggleRepeat\ntoggleShuffle"
endfun

command! -nargs=* -complete=custom,Audacious_Complete Audacious :call s:Audacious(<f-args>)

map <silent> <unique> <Leader>an :Audacious next<CR>
map <silent> <unique> <Leader>ap :Audacious prev<CR>
map <silent> <unique> <Leader>as :Audacious song<CR>

amenu <silent> &Audacious.&Playlist<TAB>:Audacious\ playlist    :Audacious playlist<CR>
amenu <silent> &Audacious.&Current\ Song<TAB>:Audacious\ song   :Audacious song<CR>
amenu <silent> &Audacious.&Next<TAB>:Audacious\ next            :Audacious next<CR>
amenu <silent> &Audacious.&Previous<TAB>:Audacious\ previous    :Audacious prev<CR>
amenu <silent> &Audacious.Play<TAB>:Audacious\ play             :Audacious play<CR>
amenu <silent> &Audacious.Pause<TAB>:Audacious\ pause           :Audacious pause<CR>
amenu <silent> &Audacious.Stop<TAB>:Audacious\ stop             :Audacious stop<CR>
amenu <silent> &Audacious.Volume.-50 :Audacious setvol -50<CR>
amenu <silent> &Audacious.Volume.-40 :Audacious setvol -40<CR>
amenu <silent> &Audacious.Volume.-30 :Audacious setvol -30<CR>
amenu <silent> &Audacious.Volume.-20 :Audacious setvol -20<CR>
amenu <silent> &Audacious.Volume.-10 :Audacious setvol -10<CR>
amenu <silent> &Audacious.Volume.+10 :Audacious setvol +10<CR>
amenu <silent> &Audacious.Volume.+20 :Audacious setvol +20<CR>
amenu <silent> &Audacious.Volume.+30 :Audacious setvol +30<CR>
amenu <silent> &Audacious.Volume.+40 :Audacious setvol +40<CR>
amenu <silent> &Audacious.Volume.+50 :Audacious setvol +50<CR>
amenu <silent> &Audacious.&Help<TAB>:Audacious\ help             :Audacious help<CR>
