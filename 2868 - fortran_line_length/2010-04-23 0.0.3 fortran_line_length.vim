" -*- vim -*-
" FILE: fortran_line_length.vim
" Vim plugin
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
" For a screenshot, see
" http://caglartoklu.blogspot.com/2009/12/fortran-valid-line-length-plugin-for.html
"
" LAST MODIFICATION: 2010-04-23
" (C) Copyright 2009 Caglar Toklu <caglartoklu[aat]gmail.com>
" Maintained by Caglar Toklu <caglartoklu[aat]gmail.com>
" VERSION: 0.0.3
"
" CHANGELOG:
" - 0.0.3, 2010-04-23
"   - FIX: Loosing the match ID after removing the match in another buffer.
"   - Priority can be customized.
"   - Group name can be customized if necessary.
"   - Script will not load itself if it has been loaded before.
"   - Script is now respecting the script ID to avoid clashes with others.
" - 0.0.2, 2009-12-07
"   - The display format for match is now read from the global variable
"     g:FORTRANMatchDisplayFormat instead of hard coded settings.
"     Now you can adapt it to your favorite colorscheme within VIMRC
"     without modifiying the plugin itself.
"   - The file is saved in UNIX file format instead of Windows.
" - 0.0.1, 2009-11-23
"   - First version.
"
" INSTALLATION:
" Copy to the Vim plugin directory.
" Although it is specifically for FORTRAN, this plugin
" should be in the plugin directory, not ftplugin, so that
" it can remove the matching for other file types.
"
" OPTIONS:
" These are options that can be customized from your VIMRC:
" - g:FORTRANMatchDisplayFormat
"   The format/color of the matched characters.
"   The default is:
"   let g:FORTRANMatchDisplayFormat = 'guifg=#FF0000'
" - g:let g:FORTRANMatchGroup
"   The name of the match group. Changing this is generally unnecessary,
"   do it if another plugin uses the same group name.
"   The default is:
"   g:let g:FORTRANMatchGroup = 'FortranLineMatch'
" - g:let g:FORTRANMatchPriority
"   The priority, Vim's default is 10.
"   If some other highlighting group shadows this one, or this one
"   is shadowing another one, you can change it.
"   Higher number gives higher priority.
"   The default is:
"   g:let g:FORTRANMatchPriority = 10
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
"
" TODO:
" * Use the same ID as previous one, if exists, since Vim increments
"   them by one whenever a new one is added.
"   :help matchadd


if exists("g:loaded_fortran_line_length") || &cp
    " If it already loaded, do not load it again.
    finish
endif


let g:loaded_fortran_line_length = 1


" Define the commands.
command! -nargs=0 FORTRANLengthAccordingToExtension :
            \ call s:SetLineLenghtAccordingToFileExtension()
command! -nargs=0 FORTRANStandardLength72          :
            \ call s:SetLineLengthForFortranStandard()
command! -nargs=0 FORTRANCardImageLength80         :
            \ call s:SetLineLengthForFortranCardImage()
command! -nargs=0 FORTRANExtendedLength132         :
            \ call s:SetLineLengthForFortranExtended()
command! -nargs=0 FORTRANRemoveMatching            :
            \ call s:RemoveFortranMatching()


function! s:SetFortranLineLengthSettings()
    " Sets the default options for the plugin.

    " Set the match display format.
    if !exists('g:FORTRANMatchDisplayFormat')
        let g:FORTRANMatchDisplayFormat = 'guifg=#FF0000'
    endif

    " Set the group name for match.
    " It is recommended not to change this.
    if !exists('g:FORTRANMatchGroup')
        let g:FORTRANMatchGroup = 'FortranLineMatch'
    endif

    " Define the highlighting only once.
    exec 'hi ' . g:FORTRANMatchGroup . ' ' . g:FORTRANMatchDisplayFormat

    " Set the priority for matching.
    " Higher values gives more priority.
    " Default value is 10. See
    " :help matchadd
    if !exists('g:FORTRANMatchPriority')
        let g:FORTRANMatchPriority = 10
    endif
endfunction


function! s:IsKnownFileExtension()
    " Returns 1 if the file extension is one of the defined
    " ones, 0 otherwise.
    let current_file_extension = expand('%:e')
    let known_extensions = split("f f77 for f90 f95 f03 f08")
    let known = count(known_extensions, current_file_extension)
    return known
endfunction


function! s:SetLineLenghtAccordingToFileExtension()
    " Checks the file extension, and it sets the valid
    " line size according to that.
    " This function is called by default.
    let current_file_extension = expand('%:e')

    if s:IsKnownFileExtension()
        " Set the matching if and only if the file extension is known,
        " and one of the predefined FORTRAN extensions.
        if current_file_extension == 'f'
            call s:SetLineLengthForFortranStandard()
        elseif current_file_extension == 'f77'
            call s:SetLineLengthForFortranStandard()
        elseif current_file_extension == 'for'
            call s:SetLineLengthForFortranStandard()
        elseif current_file_extension == 'f90'
            call s:SetLineLengthForFortranExtended()
        elseif current_file_extension == 'f95'
            call s:SetLineLengthForFortranExtended()
        elseif current_file_extension == 'f03'
            call s:SetLineLengthForFortranExtended()
        elseif current_file_extension == 'f08'
            call s:SetLineLengthForFortranExtended()
        endif
    else
        " Do not try to apply the match highlighting from an unknown
        " file extension.
        " Remove all the matches applied by this plugin.
        call s:RemoveFortranMatching()
    endif
endfunction


function! s:SetLineLengthForFortranStandard()
    " Sets the standard fixed format, 72 characters.
    call s:RemoveFortranMatching()
    let g:fortranMatched = matchadd(g:FORTRANMatchGroup, '\%>72v.\+', g:FORTRANMatchPriority)
endfunction


function! s:SetLineLengthForFortranCardImage()
    " Sets the card image format, 80 characters.
    call s:RemoveFortranMatching()
    let g:fortranMatched = matchadd(g:FORTRANMatchGroup, '\%>80v.\+', g:FORTRANMatchPriority)
endfunction


function! s:SetLineLengthForFortranExtended()
    " Sets the modern, extended format, 132 characters.
    call s:RemoveFortranMatching()
    let g:fortranMatched = matchadd(g:FORTRANMatchGroup, '\%>132v.\+', g:FORTRANMatchPriority)
endfunction


function! s:RemoveFortranMatching()
    " Removes all the matching set by this plugin.
    "
    " First, get the list of all matches.
    " [{'group': 'MyGroup1', 'pattern': 'some1',
    " 'priority': 10, 'id': 1}, {'group': 'MyGroup2',
    " 'pattern': 'some2', 'priority': 10, 'id': 2}]
    let ms = getmatches()
    " Then, traverse the list, and remove all the matches with the group
    " g:FORTRANMatchGroup.
    for m1 in ms
        let mgrp = m1['group']
        let mptn = m1['pattern']
        let mpri = m1['priority']
        let mid = m1['id']
        if mgrp == g:FORTRANMatchGroup
            call matchdelete(mid)
            " let x = input(mid)
            " let g:fortranLastMatchId = mid
        endif
    endfor
endfunction

" Define the settings once.
call s:SetFortranLineLengthSettings()

" Set the valid line length by default.
" It also removes the matching for other file types.
au BufEnter * call s:SetLineLenghtAccordingToFileExtension()
