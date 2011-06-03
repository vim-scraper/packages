" Relativize : Sets the local working Directory to be relative to file 
"
" Script Info and Documentation  {{{
"=============================================================================
"    Copyright: Copyright (C) 2008 Michael Brown
"      License: The MIT License
"               
"               Permission is hereby granted, free of charge, to any person obtaining
"               a copy of this software and associated documentation files
"               (the "Software"), to deal in the Software without restriction,
"               including without limitation the rights to use, copy, modify,
"               merge, publish, distribute, sublicense, and/or sell copies of the
"               Software, and to permit persons to whom the Software is furnished
"               to do so, subject to the following conditions:
"               
"               The above copyright notice and this permission notice shall be included
"               in all copies or substantial portions of the Software.
"               
"               THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"               OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"               MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"               IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"               CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"               TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"               SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" Name Of File: relativize.vim
"  Description: Sets the local Working directory to be relative to file argument
"   Maintainer: Michael Brown <michael <at> ascetinteractive.com>
"  Last Change: 
"          URL: 
"      Version: 0.1
"
"        Usage: 
"
"               Project with a tags file example:
"
"               Open a long full path name from some error log
"
"               vi /some/full/path/to/project/current_project/lib/module/file.ext
"               ...
"
"               :Relativize tags<cr>
"
"               if you type :pwd you should get
"
"               /some/full/path/to/project/current_project
"
"               assuming a tags file has been created in the current_project
"               file in the past.
"
"               So hopefully this means tag jumps will work and relative
"               include paths can be accessed via gf.
"
"               This could also be useful for other landmark files on the
"               system (eg. index.html, tags, README). In the above example
"               using the actual file.ext as the argument will make the
"               working directory the module directory
"
"               The function will prioritise the deepest tags file.
"
"
"         Bugs:
"
"               - Will probably not work with windows paths in current form.
"               It will probably cause an infinite loop you will need to
"               Ctrl-C out of.
"        To Do: 
"
"
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
com! -nargs=1 Relativize call MakeRelativeTo (<f-args>)

fun! MakeRelativeTo (relfilename )

    if match (a:relfilename, '[^0-9A-Za-z_\.]') != -1
        echo "Argument contains invalid characters"
        return 0
    endif

    let myfile = expand('%')

    echo match (myfile,'/')
    echo myfile

    if match (myfile,'/') != 0
        echo ("File is Already Relative")
        return 0
    endif

    let tagfile = substitute(myfile,'\/[^\/]*$','/' . a:relfilename , '')
    if (filereadable (tagfile))
        echo 'Working Directory set to: ' . tagfile
        execute 'lcd '  . substitute(tagfile,a:relfilename . '$','', '')
        return 1
    else
    endif

    while match (tagfile, '\/')  != -1 && tagfile != '/'. a:relfilename

        let tagfile = substitute(tagfile,'\/[^\/]*\/'.a:relfilename.'$','/'. a:relfilename , '')

        if (filereadable (tagfile))
            echo 'Working Directory set to: ' . tagfile
            execute 'lcd '  . substitute(tagfile,a:relfilename . '$','', '')
            return 1
        else
        endif
    endwhile

    echo 'Could not find a tags file in parent directories'
    return 0
endfun
