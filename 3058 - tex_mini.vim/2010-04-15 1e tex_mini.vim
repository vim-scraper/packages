" File:     tex_mini.vim
" Author:   Pouya D. Tafti
" Date:     15 Apr. 2010
" Version:  1e
" Licence:  IDCLMA (I don't care leave me alone) ;)
"
" Notice:   
"   Portions inspired by the tutorial available at
"   <http://www.ibm.com/developerworks/linux/library/l-vim-script-3/>.
"   This is my first vim plugin; it works for me, but you may be disappointed.
"   There is absolutely no warranty; use at your own risk, etc.
"
" Installation: 
"   One possibility is to copy the file to .vim/ftplugin/ and add the following
"   line to your .vimrc:
"   au BufRead,BufNewfile *.tex runtime ftplugin/tex_mini.vim
"
" Usage:    
"   In insert mode, pressing <tab> after any of the keywords listed below
"   "completes" it by replacing it with the corresponding left and right
"   completions (feel free to modify/add your own).
"
"   In visual mode, pressing <tab> after having selected some text asks for
"   left and right commands (same as the keywords used in insert mode), and
"   then sandwiches the selected text between corresponding left and right
"   completions.
"
" Bugs:     
"   Unknown (but probably many)
            

if exists("b:did_localmltex") || &cp
    finish
endif

let b:did_localmltex = 1
let s:keepcpo = &cpo
set cpo&vim

"imap <buffer> <tab> <c-r>=LtxTabWrapper()<cr>
inoremap <buffer> <tab> <c-r>=LtxTabWrapper('n','')<cr>
vnoremap <buffer> <tab> "zd:<c-u>exec "normal i" . LtxTabWrapper('v',@z)<cr>

let s:null = ""
let s:completions = []

if !exists("*LtxAddCompletion")
fun! LtxAddCompletion(left,right,lcomp,rcomp)
    call insert(s:completions, [a:left, a:right, a:lcomp, a:rcomp])
endfun
endif

" environments
call LtxAddCompletion(  'eq', s:null, "\\begin{equation}\n\<tab>", "\n\<c-u>\\end{equation}\n")
call LtxAddCompletion(  'eq\*', s:null, "\\begin{equation\*}\n\<tab>", "\n\<c-u>\\end{equation\*}\n")
call LtxAddCompletion(  'mult', s:null, "\\begin{multline}\n\<tab>", "\n\<c-u>\\end{multline}\n")
call LtxAddCompletion(  'mult\*', s:null, "\\begin{multline\*}\n\<tab>", "\n\<c-u>\\end{multline\*}\n")
call LtxAddCompletion(  'al', s:null, "\\begin{align}\n\<tab>", "\n\<c-u>\\end{align}\n")
call LtxAddCompletion(  'al\*', s:null, "\\begin{align\*}\n\<tab>", "\n\<c-u>\\end{align\*}\n")
call LtxAddCompletion(  'alat', s:null, "\\begin{alignat}[", "]\n\n\<c-u>\\end{align}\n")
call LtxAddCompletion(  'alat\*', s:null, "\\begin{alignat\*}[", "]\n\n\<c-u>\\end{align\*}\n")
call LtxAddCompletion(  'cas', s:null, "\\begin{cases}\n\<tab>", "\n\<c-u>\\end{cases}\n")
call LtxAddCompletion(  'itm', s:null, "\\begin{itemize}\n\<tab>\\item ", "\n\<c-u>\\end{itemize}\n")
call LtxAddCompletion(  'enum', s:null, "\\begin{enumerate}\n\<tab>\\item ", "\n\<c-u>\\end{enumerate}\n")

" macros
call LtxAddCompletion(  'chap', s:null, "\\chapter\{", "\}")
call LtxAddCompletion(  'sec', s:null, "\\section\{", "\}")
call LtxAddCompletion(  'sub', s:null, "\\subsection\{", "\}")
call LtxAddCompletion(  'sub2', s:null, "\\subsubsection\{", "\}")
call LtxAddCompletion(  'lab', s:null, "\\label\{", "\}")

" font commands
call LtxAddCompletion(  'bf', s:null, "\{\\bfseries ", "\}")
call LtxAddCompletion(  'it', s:null, "\{\\itshape ", "\\/\}")
call LtxAddCompletion(  'sl', s:null, "\{\\slshape ", "\\/\}")
call LtxAddCompletion(  'sc', s:null, "\{\\scshape ", "\}")
call LtxAddCompletion(  'tt', s:null, "\{\\ttfamily ", "\}")

" greek letters
call LtxAddCompletion(  '\\a', s:null, "\\alpha", s:null)
call LtxAddCompletion(  '\\b', s:null, "\\beta", s:null)
call LtxAddCompletion(  '\\g', s:null, "\\gamma", s:null)
call LtxAddCompletion(  '\\G', s:null, "\\Gamma", s:null)
call LtxAddCompletion(  '\\d', s:null, "\\delta", s:null)
call LtxAddCompletion(  '\\D', s:null, "\\Delta", s:null)
call LtxAddCompletion(  '\\e', s:null, "\\epsilon", s:null)
call LtxAddCompletion(  '\\e2', s:null, "\\varepsilon", s:null)
call LtxAddCompletion(  '\\z', s:null, "\\zeta", s:null)
call LtxAddCompletion(  '\\h', s:null, "\\eta", s:null)
call LtxAddCompletion(  '\\th', s:null, "\\theta", s:null)
call LtxAddCompletion(  '\\Th', s:null, "\\Theta", s:null)
call LtxAddCompletion(  '\\i', s:null, "\\iota", s:null)
call LtxAddCompletion(  '\\k', s:null, "\\kappa", s:null)
call LtxAddCompletion(  '\\l', s:null, "\\lambda", s:null)
call LtxAddCompletion(  '\\L', s:null, "\\Lambda", s:null)
call LtxAddCompletion(  '\\m', s:null, "\\mu", s:null)
call LtxAddCompletion(  '\\n', s:null, "\\nu", s:null)
call LtxAddCompletion(  '\\x', s:null, "\\xi", s:null)
call LtxAddCompletion(  '\\X', s:null, "\\Xi", s:null)
call LtxAddCompletion(  '\\p', s:null, "\\pi", s:null)
call LtxAddCompletion(  '\\P', s:null, "\\Pi", s:null)
call LtxAddCompletion(  '\\r', s:null, "\\rho", s:null)
call LtxAddCompletion(  '\\s', s:null, "\\sigma", s:null)
call LtxAddCompletion(  '\\s2', s:null, "\\varsigma", s:null)
call LtxAddCompletion(  '\\S', s:null, "\\Sigma", s:null)
call LtxAddCompletion(  '\\t', s:null, "\\tau", s:null)
call LtxAddCompletion(  '\\u', s:null, "\\upsilon", s:null)
call LtxAddCompletion(  '\\ph', s:null, "\\phi", s:null)
call LtxAddCompletion(  '\\Ph', s:null, "\\Phi", s:null)
call LtxAddCompletion(  '\\c', s:null, "\\chi", s:null)
call LtxAddCompletion(  '\\C', s:null, "\\Chi", s:null)
call LtxAddCompletion(  '\\ps', s:null, "\\psi", s:null)
call LtxAddCompletion(  '\\Ps', s:null, "\\Psi", s:null)
call LtxAddCompletion(  '\\o', s:null, "\\omega", s:null)
call LtxAddCompletion(  '\\o2', s:null, "\\varomega", s:null)
call LtxAddCompletion(  '\\O', s:null, "\\Omega", s:null)

" delimiters
call LtxAddCompletion(  '$', s:null, "$", "$")
call LtxAddCompletion(  '{', s:null, "\{", "\}")
call LtxAddCompletion(  '{2', s:null, "\\\{", "\\\}")
call LtxAddCompletion(  '<', s:null, "\\langle", "\\rangle")

call LtxAddCompletion(  'le', s:null, "\\left", "\\right")

call LtxAddCompletion(  '(l', s:null, "\\left(", "\\right)")
call LtxAddCompletion(  '[l', s:null, "\\left[", "\\right]")
call LtxAddCompletion(  '|l', s:null, "\\left|", "\\right|")
call LtxAddCompletion(  '|2l', s:null, "\\left\\|", "\\right\\|")
call LtxAddCompletion(  '{2l', s:null, "\\left\\\{", "\\right\\\}")
call LtxAddCompletion(  '<l', s:null, "\\left\\langle", "\\right\\rangle")

call LtxAddCompletion(  '(b', s:null, "\\bigl(", "\\bigr)")
call LtxAddCompletion(  '[b', s:null, "\\bigl[", "\\bigr]")
call LtxAddCompletion(  '|b', s:null, "\\bigl|", "\\bigr|")
call LtxAddCompletion(  '|2b', s:null, "\\bigl\\|", "\\bigr\\|")
call LtxAddCompletion(  '{2b', s:null, "\\bigl\\\{", "\\bigr\\\}")
call LtxAddCompletion(  '<b', s:null, "\\bigl\\langle", "\\bigr\\rangle")

call LtxAddCompletion(  '(B', s:null, "\\Bigl(", "\\Bigr)")
call LtxAddCompletion(  '[B', s:null, "\\Bigl[", "\\Bigr]")
call LtxAddCompletion(  '|B', s:null, "\\Bigl|", "\\Bigr|")
call LtxAddCompletion(  '|2B', s:null, "\\Bigl\\|", "\\Bigr\\|")
call LtxAddCompletion(  '{2B', s:null, "\\Bigl\\\{", "\\Bigr\\\}")
call LtxAddCompletion(  '<B', s:null, "\\Bigl\\langle", "\\Bigr\\rangle")

" misc math
call LtxAddCompletion(  '\\', s:null, "\\backslash", s:null)
call LtxAddCompletion(  '_', s:null, "_\{", "\}")
call LtxAddCompletion(  '\^', s:null, "\^\{", "\}")
call LtxAddCompletion(  'frac', s:null, "\\frac\{", "\}\{\}")

" ================================
" end of user configurable section
" ================================

if !exists("*LtxTabWrapper")
fun! LtxTabWrapper(cmode,txt)
    let cmdlist = []
    let txt = s:null

    if a:cmode == 'v'
        let cmdlist = split(input('mltex cmd (left/right):'),'/')
    else
        " from <http://www.vim.org/scripts/script.php?script_id=93>
        let thiscol = col('.')
        let thisline = getline('.')
        if !thiscol || thisline[thiscol-1-1] !~ '\S'
            return "\<tab>"
        endif
    endif
    let lcmd = get(cmdlist,0,s:null)
    let rcmd = get(cmdlist,1,s:null)

    " run completion
    return LtxComplete(lcmd,rcmd,a:txt,a:cmode)
endfun
endif

if !exists("*LtxComplete")
fun! LtxComplete(lcmd,rcmd,txt,cmode)
    let thiscol = col('.')
    let thisline = getline('.')
    
    if a:cmode == 'v' " visual mode: apply commands to selection
        for [left, right, lcomp, rcomp] in s:completions
            if left != a:lcmd || right != a:rcmd
                continue
            endif

            " sandwich text b/w lcomp and rcomp
            exec "normal i" . lcomp . a:txt
            let savecursor = getpos(".")
            let savecursor[2] += 1
            exec "normal a" . rcomp
            call setpos('.', savecursor)

            return s:null
        endfor
    endif

    " special pattern to match at cursor position
    let here = '\%' . (thiscol) . 'c'
    for [left, right, lcomp, rcomp] in s:completions
        let pattern = left . here . right
        if thisline =~ pattern
            " delete pattern and insert comp
            let savecursor = getpos(".")
            let repl=substitute(thisline,pattern,'_LTXCURSOR_',"")
            let savecursor[2] = match(repl,'_LTXCURSOR_') + 1
            let thisline=substitute(repl,'_LTXCURSOR_','',"")
            call setline(".",thisline)
            call setpos('.', savecursor)
            exec "normal i" . lcomp 
            let savecursor = getpos(".")
            let savecursor[2] += 1
            exec "normal a" . rcomp 
            call setpos('.', savecursor)
            

            return s:null
        endif
    endfor
    return "\<tab>"
endfun
endif
