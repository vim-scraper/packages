" Simple script to help create colorscheme files.
" Travis Hume
" Tue 02 Apr 2002 05:21:04 PM PST
" v0.06
"
" v0.01 - initial release
" v0.02 - bugfixes
" v0.03 - allow scheme naming on commandline
" v0.04 - protect font names with ''
" v0.05 - add comments describing how I create and modify colorschemes
" v0.06 - add support for Mkcolorscheme! [scheme name] -- if the ! is
"         included then the file ~/.vim/colors/<scheme-name>.vim will
"         automatically be written if possible.  Only works with
"         has("unix") for now.
"
" 1. Drop this file into your plugin directory
" 2. Configure your colorscheme just the way you want it
" 3. Run Mkcolorscheme[!] [scheme-name]
" 4. Review the generated file
" 5. Save the scheme to your colors dir with a scheme-name.vim filename
"
" I reckon there are problems with this somewhere, but it seems to work
" for a quick hack.
"
" Helpful command for colorscheme writing.  The first 3 or these commands
" are invaluable for setting up a new or changing an existing colorscheme.
" 1. Open a file with syntax highlighting defined.
" 2. Find a color you'd like to change and position the cursor on it
" 3. :GetSyntax will give you the name of the highlight group to change
"    :GetSpecificSyntax will give you the filetype specific group
" 4. Modify the group (:hi <group-name> guifg=<color1> guibg=<color2>
" 5. Rinse and repeat for each group you'd like changed
" 6. :Mkcolorscheme <scheme name> will create a colorscheme file for you.
"
" command! -complete=command GetFgColor echo synIDattr(synIDtrans(synID(line("."), col("."), 1)), "fg")
" command! -complete=command GetBgColor echo synIDattr(synIDtrans(synID(line("."), col("."), 1)), "bg")
" command! -complete=command GetSyntax echo synIDattr(synIDtrans(synID(line("."), col("."), 1)), "name")
" command! -complete=command GetSpecificSyntax echo synIDattr(synID(line("."), col("."), 1), "name")

if exists( "Mkcolorscheme_defined" )
    finish
endif
let Mkcolorscheme_defined=1


command! -complete=command -nargs=? -bang Mkcolorscheme call Mkcolorscheme( "<bang>", <f-args> )
function! Mkcolorscheme( ... )
    " grag the current highlight defs
    redir @"
    silent highlight
    redir END

    " create a new scratch buffer
    new
    set buftype=nofile bufhidden=hide noswapfile ft=vim

    " paste the defs and tweak them a bit
    normal ""p
    silent! g/^$\| links /d             " delete empty and links lines
    silent! %s/ xxx / /                 " remove the xxx's
    silent! %s/^/highlight /            " add highlight commands
    silent! %s/font=\(.*\)/font='\1'/   " some fonts have spaces in their names
                                        " so we need to protect the font name

    if a:0 == 0
        let getName = 1
        let doWrite = 0
    elseif a:0 == 1
        if a:1 == "!"
            let getName = 1
            let doWrite = 1
        else
            let name = a:1
            let getName = 1
            let doWrite = 0
        endif
    elseif a:0 == 2
        if a:1 == "!"
            let name = a:2
            let getName = 0
            let doWrite = 1
        else
            let name = a:1
            let getName = 0
            let doWrite = 0
        endif
    endif

    if getName == 1
        let name = input( "scheme name? " )
    endif


    " append some stuff at the beginning to set the value of background
    " and clear the current highlighting.  Taken from colorscheme scripts
    " distributed with Vim v6.0av
    call append( 0, "set background=".&background )
    call append( 1, "highlight clear" )
    call append( 2, "if exists( \"syntax_on\" )" )
    call append( 3, "    syntax reset" )
    call append( 4, "endif" )
    call append( 5, "let g:colors_name=\"".name."\"" )
    call append( 6, "" )

    let colors_dir = expand("$HOME")."/.vim/colors"
    if doWrite == 1 && has("unix") && filewritable( colors_dir ) == 2
        execute "w! ".colors_dir."/".name.".vim"
        bd
    endif
endfunction

