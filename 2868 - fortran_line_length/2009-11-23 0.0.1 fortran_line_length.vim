" -*- vim -*-
" FILE: fortran_line_length.vim
"
" DESCRIPTION:
" According to the file extension of a FORTRAN source file,
" this plugin sets a valid line length.
" Any characters occurring after this column are considered as
" error, and are matched so by highlighting.
"
" This plugin is run automatically. But, if you want to
" use any other valid size for some reason, the commands
" are available.
"
" LAST MODIFICATION: 2009-11-23
" (C) Copyright 2009 Caglar Toklu <caglartoklu@gmail.com>
" Maintained by Caglar Toklu <caglartoklu@gmail.com>
" VERSION: 0.0.1
"
" CHANGELOG:
" - 0.0.1, 2009-11-23
"   - First version.
"
" INSTALLATION:
" Copy to the Vim plugin directory.
" Although it is specifically for FORTRAN, this plugin
" should be in the plugin directory, not ftplugin, so that
" it can remove the matching for other file types.
"
" USAGE:
" All commands defined by this plugin starts with 'FORTRAN'.
" So, you can type :FORTRAN and press tab to see the available values.
"
" REFERENCES:
" http://gnu.huihoo.org/gcc/gcc-3.3.6/g77/Fortran-Dialect-Options.html
" http://docs.sun.com/app/docs/doc/805-4939/6j4m0vn6l?a=view
"
" COMMANDS:
" :FORTRANLengthAccordingToExtension
"    Sets the valid line length according to the file extension.
"    This command is applied by default.
" :FORTRANStandardLength72
"    The columns occurring after the 72nd character are marked.
" :FORTRANCardImageLength80
"    The columns occurring after the 80th character are marked.
" :FORTRANExtendedLength132
"    The columns occurring after the 132nd character are marked.
" :FORTRANRemoveMatching
"    Removes the matching set by this plugin.



" Define the commands.
command! -nargs=0 FORTRANLengthAccordingToExtension :
            \ call SetLineLenghtAccordingToFileExtension()
command! -nargs=0 FORTRANStandardLength72          :
            \ call SetLineLengthForFortranStandard()
command! -nargs=0 FORTRANCardImageLength80         :
            \ call SetLineLengthForFortranCardImage()
command! -nargs=0 FORTRANExtendedLength132         :
            \ call SetLineLengthForFortranExtended()
command! -nargs=0 FORTRANRemoveMatching            :
            \ call RemoveFortranMatching()



function! SetLineLenghtAccordingToFileExtension()
    " Checks the file extension, and it sets the valid
    " line size according to that.
    " This function is called by default.
    let current_file_extension = expand("%:e")

    call RemoveFortranMatching()
    exec "hi FortranLineMatch guifg=#FF0000"

    if current_file_extension == "f"
        call SetLineLengthForFortranStandard()
    elseif current_file_extension == "f77"
        call SetLineLengthForFortranStandard()
    elseif current_file_extension == "for"
        call SetLineLengthForFortranStandard()
    elseif current_file_extension == "f90"
        call SetLineLengthForFortranExtended()
    elseif current_file_extension == "f95"
        call SetLineLengthForFortranExtended()
    elseif current_file_extension == "f03"
        call SetLineLengthForFortranExtended()
    elseif current_file_extension == "f08"
        call SetLineLengthForFortranExtended()
    endif
endfunction


function! SetLineLengthForFortranStandard()
    " Sets the standard fixed format, 72 characters.
    call RemoveFortranMatching()
    exec "hi FortranLineMatch guifg=#FF0000"
    let g:fortranMatched = matchadd('FortranLineMatch', '\%>72v.\+')
endfunction


function! SetLineLengthForFortranCardImage()
    " Sets the card image format, 80 characters.
    call RemoveFortranMatching()
    exec "hi FortranLineMatch guifg=#FF0000"
    let g:fortranMatched = matchadd('FortranLineMatch', '\%>80v.\+')
endfunction


function! SetLineLengthForFortranExtended()
    " Sets the modern, extended format, 132 characters.
    call RemoveFortranMatching()
    exec "hi FortranLineMatch guifg=#FF0000"
    let g:fortranMatched = matchadd('FortranLineMatch', '\%>132v.\+')
endfunction


function! RemoveFortranMatching()
    " Removes all the matching set by this plugin.
    if exists("g:fortranMatched")
        if g:fortranMatched > 0
            call matchdelete(g:fortranMatched)
            let g:fortranMatched = 0
        endif
    else
    endif
endfunction


" Set the valid line length by default.
" It also removes the matching for other file types.
au BufEnter * call SetLineLenghtAccordingToFileExtension()

