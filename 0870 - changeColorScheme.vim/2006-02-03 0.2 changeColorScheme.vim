"------------------------------------------------------------------------------
" File: changeColorScheme.vim 
"------------------------------------------------------------------------------
" Got idea from following tips and scripts 
" vimtip #341, vimtip #358, vimscript #668, vimscript #109 
"------------------------------------------------------------------------------
" Author: Hosup Chung <hosup.chung@gmail.com>
"
" Created: 2003 December 31
" Last Updated: 2006 February 3 
"
" Version: 0.2
" 0.2: Updated RandomColorScheme()
"
" 0.12: I finally tested this plugin on OS X and Linux
"       When searching color schemes, substituting eol didn't work on Linux.
"       I'm using "\n" instead of '\n' now, and it seems working.
"
" 0.11: fixed some typo in Usage description 
"
" 0.1:  initial upload 
"------------------------------------------------------------------------------
" Install:
" Copy this script in your plugin directory
"------------------------------------------------------------------------------
" Usage:
" When this script is loaded, it will populate an array with each of color 
" scheme's file path.  You can then load different color schemes using 
" NextColorScheme(), PreviousColorScheme() or RandomColorScheme(). 
"
" There are 3 main functions 
"   1. You can either call them directly
"      :call NextColorScheme()
"      :call PreviousColorScheme() 
"      :call RandomColorScheme()
"
"   2. You can map and save them in your [._]gvimrc
"      map <F12>   :call NextColorScheme()<CR>
"      map <S-F12> :call PreviousColorScheme()<CR>
"      map <C-F12> :call RandomColorScheme()<CR>
"
"   3. You can also start each vim session with random color scheme by 
"      adding following line in your [._]gvimrc
"      call RandomColorScheme()
"
"------------------------------------------------------------------------------
" Tip: 
" You can change your rulerformat in your [._]vimrc to display the name of 
" current color scheme on status line.
"
" First, add this line to display status information 
"     set ruler 
"
" And add %{g:colors_name} in rulerformat to display name of current color 
"  scheme. For example,
"     set rulerformat=%55(:%{g:colors_name}:\ %5l,%-6(%c%V%)\ %P%) 
"
" However, you will see an error message if you didn't load color scheme at
" startup. So you might want to add %{GetColorSyntaxName()} instead.
"     set rulerformat=%55(:%{GetColorSyntaxName()}:\ %5l,%-6(%c%V%)\ %P%) 
"
" GetColorSyntaxName() function is included in this script. It returns
" the value of g:colors_name if it exists, otherwise an empty string. If you
" are using a console version, then you might have to copy
" GetColorSyntaxName() into .vimrc, because I think the plugin files get
" loaded after evaluating .vimrc.
"------------------------------------------------------------------------------

if !exists("g:change_color_scheme")

    " You can pick any sep character that will mark between array elements.
    " However, because each array element is a file path, it would be better 
    " to pick an invalid file character.
    let s:sep='?'

    " I got idea from lightWeightArray.vim, and came up ElementAt.
    " This function expect an array which element is separated by sep character
    " and returns the index element from given array or -1 for not found
    function! ElementAt (array, sep, index)
        " if array is empty or index is negative then return -1
        if strlen(a:array) == 0 || a:index < 0
            return -1
        endif

        let current_pos = 0 	" current character position within array
        let i = 0		" current array position

        " Search the array element on a:index position.
        while i != a:index 
            let current_pos = match(a:array, a:sep, current_pos)
            if current_pos == -1
                return -1	" couldn't find it
            endif

            let current_pos = current_pos + 1
            let i = i + 1
        endwhile
         
	" then find where the current array element ends 
        let array_element_endpos = match(a:array, a:sep, current_pos)
        if array_element_endpos == -1
	    " must be the last array element
	    let array_element_endpos = strlen(a:array) 
	endif

        " return the color scheme file path in a:index position
        return strpart(a:array,current_pos,(array_element_endpos-current_pos))
    endfunction  " ElementAt

    " If g:colors_name is defined, return the name of current color syntax name.
    " Otherwise return an empty string
    function! GetColorSyntaxName()
        if exists('g:colors_name')
            return g:colors_name
        else
            return ''
        endif	
    endfunction  "GetColorSyntaxName

    " Use this function if you want to use different fonts for different scheme.
    " SetGuiFont don't do anything by default. You can uncomment if..else..endif 
    " block and change font name and size for your platform.
    function! SetGuiFont()
        " Color schemes that has dark background should defined 'background' 
        " option as 'dark' in its scheme. You can use this option to set 
        " different fonts when color scheme has dark background.
        " I found Anonymous font from on code2html script description page,
        " and it looks good (only on dark background though). 
        " http://www.ms-studio.com/FontSales/anonymous.html
        " if &background == "dark"
        "    set guifont=Anonymous:h9:cANSI
        " else
        "    set guifont=Courier_New:h10:cANSI
        " endif

        " You can also use different font for different color scheme.
        " if g:colors_name == "aqua"
        "    set guifont=Courier_New:h12:cANSI
        " elseif g:colors_name == "default" 
        "    set guifont=Terminal:h12:cANSI
        " endif
    endfunction   " SetGuiFont

    let g:change_color_scheme="0.2"
endif

" get all color scheme file path and save it to s:color_schemes. It will be 
" treated as a string array which elements are separated by sep character. 
let s:color_schemes = substitute(globpath(&runtimepath,"colors/*.vim"), "\n", s:sep, 'g')
let s:total_schemes = 0
let s:scheme_index = 0

" calculate the total number of color schemes by counting sep character from 
" color_schemes. Unless there's no color scheme, total number of color scheme
" is 1 bigger than number of sep characters
if (strlen(s:color_schemes) > 0)
    let found = 0
    while found != -1
        let found = match(s:color_schemes, s:sep, found+1)
        let s:total_schemes = s:total_schemes + 1
    endwhile
endif

" load next color schemes.
function! NextColorScheme()
    let s:scheme_index = s:scheme_index + 1
    call LoadColorScheme()
endfunction

" load previous color schemes.
function! PreviousColorScheme()
    let s:scheme_index = s:scheme_index - 1
    call LoadColorScheme()
endfunction

" load randomly chosen color scheme
" Vim still doesn't have a function that returns millisecons. As a result,
" it's difficult to create a random number. My previous attempt was to call
" localtime(). Since it's only returns seconds, calling RandomColorScheme()
" continuosly is just same as NextColorScheme() except the first call.
" Now I found another function getfsize(), which returns the file size. I
" dont dare say adding localtime() and getfsize() generates random numbers, 
" but it's better than previous one. And it looks random enough for me ^_^
"------------------------------------------------------------------------------
function! RandomColorScheme()
    let s:current_scheme_fsize = getfsize(ElementAt(s:color_schemes, s:sep, s:scheme_index))
    " set a random scheme_index from (0 ... total_schemes-1).
    let s:scheme_index = (localtime()+s:current_scheme_fsize) % s:total_schemes
    call LoadColorScheme()
endfunction

" load a color scheme 
function! LoadColorScheme()
    " quit if there's no color scheme
    if s:total_schemes == 0
    	    return 0
    endif

    " wrap around scheme_index for either direction
    if s:scheme_index < 0 
        let s:scheme_index = s:total_schemes-1
    elseif s:scheme_index >= s:total_schemes
        let s:scheme_index = 0
    endif

    " ElementAt returns the name of color scheme on scheme_index position in 
    " color_schemes array. Then we will load (source) the scheme.
    exe "source " ElementAt(s:color_schemes, s:sep, s:scheme_index)

    call SetGuiFont()
endfunction
