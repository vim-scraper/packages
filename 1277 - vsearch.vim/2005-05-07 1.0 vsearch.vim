"Author : Arun Easi
"Date   : May 7th, 2005
"Version: 1.0

" Script to perform vertical search for patterns
" Usage:
"  1) .,$VS <pattern> [Preferred approach, shows multiple occurence in a line]
"      [ Use @: to search for further hits]
"  2)  [range]VS <pattern> [Only first occurence in a line shown]
"
" Pre-requisites:
"   :set nocp cpo+=<
"
com! -nargs=1 -range VS :call Vsearch(<f-args>, <line1>, <line2>)
fun! Vsearch(patt, fl, ll)
    let patt=a:patt
    "let fl=line('.')|let ll=line('$')
    let fl=a:fl|let ll=a:ll
    let cl=fl

    let i=col('.')
    while (cl <= ll)
        if (cl != fl)|let i=0|endif
        let len=strlen(getline(cl))
        while (i <= len)
            let i=i+1
            let spatt='\%'.cl.'l'.'\%'.i.'c'.substitute(patt, '.\ze[^$]',
                                        \'&.*\\n.*\\%'.i.'c', 'g')
            if (search(spatt, 'w') != 0)
                return
            endif
        endwhile
        let cl=cl+1
    endwhile
    " EOR : End Of Range given
    echo "No hit from here to <EOR>"
endf

