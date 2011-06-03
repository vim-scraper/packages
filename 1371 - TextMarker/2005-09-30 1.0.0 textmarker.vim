" Vim support file to colorize single words
"
" Maintainer:    Andreas Thoelke
" Last Change:   2005 Sep. 20
" Version:       1.0.0
" --------------------------------------------------------------------------------
" Install:       Copy this file into the plugin directory so it will be automatically
"                sourced.
"                Copy the files: 
"                    lightBlue.bmp
"                    lightCyan.bmp
"                    lightGreen.bmp
"                    lightMagenta.bmp
"                    lightRed.bmp
"                    lightYellow.bmp
"                    white.bmp
"                into the directory bitmaps so they will be placed into the toolbar
"
" Usage:         Place the cursor over the word you want to mark and press the TextMarker
"                button in the toolbar or place the mouse cursor over the word you want
"                to mark and use the right mouse button menue or place the mouse cursor
"                over the word you want to mark and press the middle (wheel) mouse button.
"                Using the middle mouse button will colorize the words in different colors
"                for each mouse click.  "
"
" This TextMarker feature was tested with gvim 6.3 and Win2K/WinXP. May be there
" are some configurations necessary for different mouse models ... At least the
" "set mousemodel=popup" should be used.
" --------------------------------------------------------------------------------

" counting words in bigger files takes too much time
let gtmMaxLinesToCount = 5000

let gtmColorIndex = "1"
let gtmColor_1 = "gTextMarker_1"
let gtmColor_2 = "gTextMarker_2"
let gtmColor_3 = "gTextMarker_3"
let gtmColor_4 = "gTextMarker_4"
let gtmColor_5 = "gTextMarker_5"
let gtmColor_6 = "gTextMarker_6"
let gtmColorIndexMax = 6

hi gTextMarker_1    ctermfg=black   ctermbg=lightgreen     guifg=black  guibg=lightgreen
hi gTextMarker_2    ctermfg=black   ctermbg=lightred       guifg=black  guibg=lightred
hi gTextMarker_3    ctermfg=black   ctermbg=lightblue      guifg=black  guibg=lightblue
hi gTextMarker_4    ctermfg=black   ctermbg=lightmagenta   guifg=black  guibg=lightmagenta
hi gTextMarker_5    ctermfg=black   ctermbg=lightcyan      guifg=black  guibg=lightcyan
hi gTextMarker_6    ctermfg=grey    ctermbg=yellow         guifg=grey40 guibg=lightyellow

amenu ToolBar.LightGreen        :call TextMarker(gtmColor_1)<cr>
amenu ToolBar.LightRed          :call TextMarker(gtmColor_2)<cr>
amenu ToolBar.LightBlue         :call TextMarker(gtmColor_3)<cr>
amenu ToolBar.LightMagenta      :call TextMarker(gtmColor_4)<cr>
amenu ToolBar.LightCyan         :call TextMarker(gtmColor_5)<cr>
amenu ToolBar.LightYellow       :call TextMarker(gtmColor_6)<cr>
amenu <silent> ToolBar.White    :syntax on<cr>:let gtmColorIndex = "1"<cr>

" using <leftmouse> to put the cursor to the actual mouse position
:menu 1.05 PopUp.Text\ Marker.Light\ Green              <leftmouse>:call TextMarker(gtmColor_1)<cr> 
:menu 1.05 PopUp.Text\ Marker.Light\ Red                <leftmouse>:call TextMarker(gtmColor_2)<cr> 
:menu 1.05 PopUp.Text\ Marker.Light\ Blue               <leftmouse>:call TextMarker(gtmColor_3)<cr> 
:menu 1.05 PopUp.Text\ Marker.Light\ Magenta            <leftmouse>:call TextMarker(gtmColor_4)<cr> 
:menu 1.05 PopUp.Text\ Marker.Light\ Cyan               <leftmouse>:call TextMarker(gtmColor_5)<cr> 
:menu 1.05 PopUp.Text\ Marker.Light\ Yellow             <leftmouse>:call TextMarker(gtmColor_6)<cr> 
:menu 1.05 PopUp.Text\ Marker.-Sep-                     :
:menu <silent> 1.05 PopUp.Text\ Marker.Remove\ Marks    :syntax on<cr>:let gtmColorIndex = "1"<cr>
:menu 1.05 PopUp.-Sep-                                  :

map <middlemouse> <leftmouse>:call MouseTextMarker()<cr>

func! WordOccurrence(searchfor)
    let n = 1
    let cnt = 0
    " search case sensitive and whole words only
    let searchforic = "\\<" . a:searchfor . "\\>\\C"
    let line = getline(n)
    let lastline = line("$")
    if lastline < g:gtmMaxLinesToCount
        while n <= lastline
            let pos = match(line, searchforic, 0)
            while pos != -1
                let cnt = cnt + 1
                let pos = match(line, searchforic, pos + 1)
            endwhile
            let n = n + 1
            let line = getline(n)
        endwhile
        if cnt == 0
            echo "\"" . a:searchfor . "\" not found"
        else
            echo "\"" . a:searchfor . "\" marked ". cnt . " time(s)"
        endif
    else
        echo "too many lines to count \"" . a:searchfor . "\""
    endif
endfunc

func! TextMarker(color)
    let searchfor = expand("<cword>")
    if strlen(searchfor) == 0 || match(searchfor, "[a-z,A-Z,0-9]", 0) == -1
        echohl ErrorMsg
        echo "Please position cursor to the word to be marked"
        echohl None
    else
        execute "syntax keyword " . a:color . " " . searchfor . ' containedin=ALL'
        call WordOccurrence(searchfor)
    endif
endfunc

func! MouseTextMarker()
    call TextMarker(g:gtmColor_{g:gtmColorIndex})
    let g:gtmColorIndex = g:gtmColorIndex + 1 
    if g:gtmColorIndex > g:gtmColorIndexMax
        let g:gtmColorIndex = "1"
    endif
endfunc

