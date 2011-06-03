" Copyright (C) 2007
" Michael Kapelko <kornerr@gmail.com>
"
" We grant permission to use, copy modify, distribute, and sell this
" software for any purpose without fee, provided that the above copyright
" notice and this text are not removed. We make no guarantee about the
" suitability of this software for any purpose and we are not liable
" for any damages resulting from its use. Further, we are under no
" obligation to maintain or extend this software. It is provided on an
" "as is" basis without any expressed or implied warranty.

" If we call "Acpp" when editing "/path/to/file/src/ae/cmd/File.cpp", VIM
" opens complementary "/path/to/file/include/ae/cmd/File.h" file, and
" vice versa.
" That is, it searches for the last occurrence of "/src/" or "/include/" and
" changes this along with the file extension (h to cpp, and cpp to h).
" NOTE: This works with ONLY single occurrence of "/src/" or "/include/"
" If current full path doesn't contain "/src/" or "/include/" and/or does not
" end with "h" or "cpp" then "Acpp" opens a file named "0"

function GetComplementaryFileName(fileName)
    let fileName = a:fileName
    let retFN = ''
    let id = strridx(fileName, '/src/')
    " If we found '/include/' change it to '/src/'
    if id == -1
        let id = strridx(fileName, '/include/')
        let retFN = strpart(fileName, 0, id)
        let retFN = retFN . '/src/'
        let retFN = retFN . strpart(fileName, id + 9)
        " We assume that it was a header file and change 'h' extention to 'cpp'
        let retFN = strpart(retFN, 0, strlen(retFN) - 1)
        let retFN = retFN . 'cpp'
    " If we found '/src/' change it to '/include/'
    else
        let retFN = strpart(fileName, 0, id)
        let retFN = retFN . '/include/'
        let retFN = retFN . strpart(fileName, id + 5)
        " We assume that it was a source file and change 'cpp' extention to 'h'
        let retFN = strpart(retFN, 0, strlen(retFN) - 3)
        let retFN = retFN . 'h'
    endif
    return retFN
endfunction

function CheckForSrcIncludeAndExt(fileName)
    let fileName = a:fileName
    let id = strridx(fileName, '.')
    let ext = strpart(fileName, id + 1)
    if ext == 'h' || ext == 'cpp'
        let idSrc = strridx(fileName, '/src/')
        let idInclude = strridx(fileName, '/include/')
        if idSrc != -1 || idInclude != -1
            return 1
        else
            return 0
        endif
    else
        return 0
endfunction

function ACppFunction(fileName)
    if CheckForSrcIncludeAndExt(a:fileName)
        return GetComplementaryFileName(a:fileName)
    else
        return 0
endfunction

command Acpp :exec "e " . ACppFunction(expand("%:p"))

