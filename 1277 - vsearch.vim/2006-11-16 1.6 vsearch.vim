"*********************************
"*** Author : Arun Easi        ***
"*** E-mail : arunke@yahoo.com ***
"*********************************
"
"Version: 1.6
"
" Script to perform vertical search for patterns
" Usage:
"  :[range]VS <pattern>
"       - Use @: to search for further hits
"       - Range default: whole file
"       - Search starts from cursor
"
" Pre-requisites:
"   :set nocp cpo+=<
"
" NOTE: Marks "g" and "h" are used by the script :( (lazy to avoid that)
"
" Change Log:
"===============================================================================
" Version   Description                                             Date
"-------------------------------------------------------------------------------
"   1.0    First submit :)                                         May 08, '05
"   1.5    Speed Enhancements                                      May 10, '05
"   1.5    Fixed "E475: invalid argument: f" problem with vim7     Nov 16, '06
"===============================================================================
"
com! -nargs=1 -range VS :call Vsearch(<f-args>, <line1>, <line2>)
fun! Vsearch(patt, fl, ll)
    let patt=a:patt
    let fl=a:fl|let ll=a:ll
    if (fl == ll)|let fl=1|let ll=line('$')|endif
    let cl=fl

    norm! mgMmh`g
    let chr=strpart(patt, 0, 1)
    while (cl <= ll)
        if (search(chr, 'W') == 0) || (line('.') > ll)
            echo "No hit from here to <EOR>"
            exe "norm! `h`g"
            return
        endif

        let c=col('.')
        let spatt='\%#'.substitute(patt, '.\ze[^$]', '&.*\\n.*\\%'.c.'c', 'g')
        if (search(spatt) != 0)
            return
        endif
    endwhile
endf
