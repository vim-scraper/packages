"cshelper.vim - plugin
"This just highlights the highlighting groups of the colorscheme you are
"editing in the current buffer -- so that you can see what you are doing.
"
"Version 1.3
"Last change July 28 2006
"Walter Hutchins
"
":CShelper   - Do color scheme file in buffer, then do CShelperHL
"
":CShelperHL - update highlight from current buffer
"
"Multiple/disjointed instances of a highlighting type are not welcome.
"
command CShelper call <SID>CShelper()
command CShelperHL call <SID>CShelperHL()

function <SID>CShelper()
" if match(in_this_file, 'colors_name\s*=\s*"' . color_scheme_name . '"') != -1
silent! /colors_name\s*=\s*["']\(.\{-}\)["']/
let a=getline(".")
let m4=""
let m1=match(a, "\\c[\"']")
if m1 != -1
    let m2=match(a, "\\c[\"']",m1 + 1)
    if m2 != -1
        let m3=m2 - m1
        let m4=strpart(a, m1 + 1, m3 - 1)
        silent! exec 'color '.tolower(m4)
    endif
endif
if !exists("g:colors_name") || m4 != g:colors_name
    echo "No valid colorscheme name found in current buffer!"
    return
endif
call <SID>CShelperHL(1)
endfunction

function <SID>CShelperHL(...)
"At this point, we shall trust that you have a valid colorscheme file
"loaded in the buffer.
" save global options and registers
let hidden      = &hidden
let lazyredraw  = &lazyredraw
let more          = &more
let report      = &report
let shortmess   = &shortmess
let wrapscan    = &wrapscan
let register_a  = @a
let register_b  = @b
let register_se = @/
set hidden lazyredraw nomore report=99999 shortmess=aoOstTW wrapscan
%yank a
let a=line(".") "Everybody remember where we parked.
silent g/^\s*$/d
silent g/^\s*"/d
silent g/^\s*[h][i]\S*\s*clear/d
silent g/^\s*[h][i]\S*\s*link/d
silent g/^\s*[h][i].*\(\n\)\s\+\(term\|cterm\|gui\)/j
silent g/^\s*[^h][^i]/d
%s/^\s*[h][i]\S*\s*//e
" precede syntax command
% substitute /\s\+/ /g
% substitute /^[^ ]\+/syn keyword & &/
" execute syntax commands
syntax clear
% yank b
silent @b
% substitute /^syn keyword \(\S\+\) //
%s/^/hi /
% yank b
silent @b
1,$d
put! =@a
"1
"/^$/d
" restore global options and registers
let &hidden      = hidden
let &lazyredraw  = lazyredraw
let &more        = more
let &report      = report
let &shortmess   = shortmess
let &wrapscan    = wrapscan
let @a           = register_a
let @b           = register_b
0 append
.
if a:0 == 1 && a:1 == 1
   /colors_name\s*=\s*["']\(.\{-}\)["']/
else
   exec a
endif
" restore last search pattern
call histdel("search", -1)
let @/ = register_se
set nomodified
endfunction
