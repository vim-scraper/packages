" This file contains various goodies for dealing with DNA and protein
" sequences, alignment and other stuff. I keep everything in one single file
" simply because it's easyer to deal with/maintain.
" Alexandru Tudor Constantinescu,  02/14/2005


" ===============================================================================
" this function tries to get the reverse-complement of a certain DNA sequence
" you have to select a block of text in advance
" the script is crude, in that it assumes you have only ATCG
" capitalization does not get screwed up
" at this moment 12/07/2004, the whole LINE (i.e. not only part of line) gets
" changed, irrespective of what you select.
" Tim Chase and William Nater 12/12/2004 
" since ignorecase gives problems (i.e. capitalization is lost)

" The replacement should be done by selecting a block of text (beware that the
" WHOLE line will get changed!!) and then issuing the commmand:
" :RC

command! -n=* -range RC :call RC_Tim(<line1>,<line2>)
" and also define a Menu entry for it
vmenu 40.480 Tools.DNA.RevComp	:RC<CR>

function! RC_Tim(l1, l2)
   let l:str = getline(a:l1)
   let l:len = strlen(l:str)
   let l:ignorecs = &l:ic
   let &l:ic = 0
   exe a:l1.",".a:l2."j!"
   exe a:l1."s/.*/\\=Rev(submatch(0))/"
   exe a:l1."s/\\c[agct]/\\=\"ATGCatgc\"[match(\"TACGtacg\", submatch(0))]/ge"
   exe a:l1."s/.\\{".&tw."\\}\\zs/\\r/g"
   let &l:ic = l:ignorecs
endfunction

fun! Rev(result)
   let l:i = strlen(a:result) - 1
   let l:result = ''
   while (l:i > -1)
	  let l:result = l:result.a:result[l:i]
	  let l:i = l:i - 1
   endwhile
   return l:result
endfun 


" ===============================================================================
" This lines just tell you how much AT and GC you have in a *line*
nmenu 40.481 Tools.DNA.Count\ AT			:set report=0<CR>:set ignorecase<CR>:s/[at]/&/g<CR>
nmenu 40.482 Tools.DNA.Count\ GC			:set report=0<CR>:set ignorecase<CR>:s/[cg]/&/g<CR>

" ===============================================================================
" This function counts the AT and GC on the WHOLE line (and only one line)
" It reports the AT/GC contents and the calculated Tm based on 4*GC+2*AT
" Alexandru Tudor Constantinescu, 12/14/2004 
function! Count_bases() 
   let l:string_length = strlen(substitute(getline("."), ".*", "&", "g"))
   let l:at = l:string_length - strlen(substitute(getline("."), "\\c[at]", "", "g"))
   let l:gc = l:string_length - strlen(substitute(getline("."), "\\c[cg]", "", "g"))
   let l:tm = 2 * l:at + 4 * l:gc
   execute "normal o"."AT=".l:at" CG=".l:gc"Tm=".l:tm
endfunction

" and also define a Menu entry for it
menu 40.483 Tools.DNA.Count+Tm :call Count_bases()<CR>

" ===============================================================================
"This is a set of commands for changing the appearance of PS files generated by ClustalX
"Changes title to a smaller font
"I have problems with title overwriting the date, so I lower the date on the same level with
"the Page numbering
"Finally I change the consensus to something more printer-friendly and save the file
"Alexandru Tudor Constantinescu  09/24/2004 
function! Clustal_ps_brushup()
   :%s/Times-Bold findfont 14/Times-Bold findfont 12/
   :%s/532 524 moveto/532 510 moveto/
   :%s/0.3 0.3 0.3 setrgbcolor/0.5 0.5 0.5 setrgbcolor/
   :w
endfunction
" and also define a Menu entry for it
nmenu 40.490 Tools.Cleanup.ClustalX\ PS\ files :call Clustal_ps_brushup()<CR>

" ===============================================================================
"This is a set of commands for making the ABI FASTA files
"easyier to get into ClustalX
"It removes the leading ' AC_' and the pattern _H09.ab1...
function! ABI_cleanup()
   :%s/ AC_//
   :%s/_[A-Z][0-9][0-9].*//
   :wn
endfunction
" and also define Menu entries for it
nmenu 40.491 Tools.Cleanup.ABI\ file :call ABI_cleanup()<CR>
nmenu 40.492 Tools.Cleanup.ABI\ fileS :bufdo call ABI_cleanup()<CR>


" ===============================================================================
" VNTI writes a lot of garbage in the GeneBank file. Get rid of it!
nmenu 40.491 Tools.Cleanup.VNTI\ GB\ files :g/^COMMENT\\|informax\\|vntifk/d<CR>

" ===============================================================================
" Assembly Lign needs files in plain text with no headers whatsoever
function AssLign_prepare()
  bufdo exec 'normal 1Gdd' | :wn!
  rewind
endfunction
nmenu 40.493 Tools.Cleanup.For\ AssemblyLign	:call AssLign_prepare()<CR>
