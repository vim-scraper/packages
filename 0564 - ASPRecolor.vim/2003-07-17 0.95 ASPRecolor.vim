" ASPRecolor.vim:   In ASPVBS buffers, give JS, ASP, and HTML sections different guibg colors
" javascript 
" Description:
"   gives unique backgrounds to server-side ASP/VBSCRIPT and client-side
"   Javascript regions. Technique could be applicable to other kinds of files
"   that embed other syntax types.
" Last Change:      July 17, 2003
"      2003-07-17       New function RecolorCluster() to walk a syntax cluster
"                       and recolor each member item.
" Maintainer:       Ross Presser <rpresser@imtek.com>
" Version:          0.95
"
" Usage:
"   Either source this from your vimrc, or just drop it in a plugins/ folder.
"   When active, buffers that get filetype ASPVBS automatically get different
"   guibg colors for the HTML regions, the Javascript regions, and the
"   ASP/VBScript regions.  See compare.asp.html for a comparison.
"
"   Requires a gui, the ASPVBS ftplugin, and 
"   the MULTVALS.vim plugin <http://www.vim.org/scripts/script.php?script_id=171>

" don't load twice.
if exists("loaded_recolor")
    finish
endif
let loaded_recolor = 1

" Define these two colors to suit your color scheme.  These go well with the
" "dawn" colorscheme that I am currently using.
let s:color1='LightGray' " used for javascript areas
let s:color2='#D0D0FF' " used for ASP areas. Sort of blue.

" This function takes a specific hlgroup, presumably linked to another
" hlgroup, and retrieves the complete highlight definition, then redefines the
" highlight with a guibg color; obviously it isn't linked afterwards.
"
" (In most cases, what we're being passed is actually a syntax group name, not
" a highlight group name, and there might not be a same-named highlight group;
" so we can't recolor it. That's what the if strlen(@z) is for.)

function! s:Recolor(hlgroup,bgcolor)
"   echo a:hlgroup . ',' . a:bgcolor
    let name2=synIDattr(synIDtrans(hlID(a:hlgroup)), "name")
    redir @z
       silent exec 'hi ' . name2
    redir END
    " if there was a highlight definition, then we'll recolor it.
    if strlen(@z) > 0
        let newhl = 'hi ' . a:hlgroup . strpart(@z, stridx(@z, 'xxx')+3) . ' guibg=' . a:bgcolor
        silent exec newhl
    endif
endfunction

" This function processes each of the syntax groups found in a syntax cluster
" and recolors them; however, subclusters are ignored.

function! s:RecolorCluster(hlcluster,bgcolor)

    redir @z 
        silent exec "syn list " . a:hlcluster
    redir END
    let clus = substitute(@z, "^.*=", "", "")
    let clus = substitute(clus, "", "", "g")
    let clus = substitute(clus, " ", "", "g")
    "echo "[[" . clus . "]] len=" . strlen(clus)
    call MvIterCreate(clus, ",", "Cluster")
    while MvIterHasNext("Cluster")
        let synitem = MvIterNext("Cluster")
        if strlen( synitem ) == 0
            " for some bizarre reason, MvIterHasNext is returning true
            " twenty times for the ASPVBS group, so we get 20 zero-length
            " things here.  We'll just ignore it for now
            "echo "Zero length iterate??"
        elseif strpart( synitem, 0, 1 ) != "@"
            "echo "Recoloring <" . synitem . ">"
            cal s:Recolor( synitem , a:bgcolor )
        endif
    endwhile
    call MvIterDestroy("Cluster")
endfunction

" This function recolors each of the syntax items that are defined for ASPVBS
" types.  Javascript items are colored with s:color1, and ASP/VBScript items
" with s:color2.
function! s:RecolorASPVBS()


  " AspVBS items
  cal s:RecolorCluster('@AspVBScriptTop',s:color2)
  exec "hi AspVBScriptInsideHtmlTags guibg=" . s:color2
  
  " javascript items
  cal s:RecolorCluster('@htmlJavascript',s:color1)

" I also like the enclosing <% and %> delimiters to be colored.  Unfortunately
" they are only implicitly defined by a region, and they are linked to
" Delimiter, which is used by lots of other highlights.  So we redefine them
" with our own new Delimiter.
  syn match ASPDelimiter "$^"
  syn region  AspVBScriptInsideHtmlTags keepend matchgroup=ASPDelimiter start=+<%=\=+ end=+%>+ contains=@AspVBScriptTop
  syn region  AspVBScriptInsideHtmlTags keepend matchgroup=ASPDelimiter start=+<script\s\+language="\=vbscript"\=[^>]*\s\+runatserver[^>]*>+ end=+</script>+ contains=@AspVBScriptTop

  " Ordinary delimiter is linked to Special, so we will link to it too -- and
  " then immediately recolor it.
  hi link ASPDelimiter Special
  cal s:Recolor('ASPDelimiter',s:color2)

endfunction

augroup recolor
  au FileType aspvbs call s:RecolorASPVBS()|syn sync fromstart
augroup end
