"Author : Arun Easi
"Date   : May 8th, 2005
"Version: 1.5

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
com! -nargs=1 -range VS :call Vsearch(<f-args>, <line1>, <line2>)
fun! Vsearch(patt, fl, ll)
    let patt=a:patt
    let fl=a:fl|let ll=a:ll
    if (fl == ll)|let fl=1|let ll=line('$')|endif
    let cl=fl

    norm! mgMmh`g
    let chr=strpart(patt, 0, 1)
    while (cl <= ll)
        if (search(chr, 'Wf') == 0) || (line('.') > ll)
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

