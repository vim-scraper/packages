""""""""""""""""""""""""""""""""" Reference to Author
let g:BASH_AuthorName   = 'Your Name'
let g:BASH_AuthorRef    = 'UTILS'
let g:BASH_Company      = 'Your Company'
let g:BASH_Product      = 'Scripts Development'
let g:BASH_Email        = 'your-email@yourISP.com'

let g:C_AuthorName      = g:BASH_AuthorName
let g:C_AuthorRef       = g:BASH_AuthorRef
let g:C_Email           = g:BASH_Email
let g:C_Company         = g:BASH_Company
let g:C_Product         = g:BASH_Product
let g:WinMergeDir=''

let g:clipbrdDefaultReg = 'c'
let g:mycol = 1
let g:game = 0
let g:CutList=[]
let g:browsefilter="*.c\*.cpp\*.h\*.vbs\*.bat\*.txt\n"
let g:HiMtchBrktOn= 1
let g:HiMtchBrktOn= 1
let g:ActivateGUIMenu=1
let g:marker=0
let g:FoldLines=0
let g:startmark=0
let g:color=''
let g:sienna_style = 'dark'
let g:sienna_style = 'light' 

set guioptions+=v
set t_Co=256
if exists("loaded_myvim") || &cp
    finish
endif

"""""""""""""""""""""" Commands VIM
command! -nargs=* ColCopy    call ColCopy(<f-args>)
command! -nargs=* Rm         call s:RmSpace(<f-args>)
command!          BrowExpl   call s:BrowWithWinExplFunc()
command!          Run        !start c:\WINDOWS\system32\cmd.exe /c %<CR>
command!          Ru         !dir<CR>
command!          PS         call Play()<CR>
command! -nargs=* Cmp        call s:Compile()
command! -nargs=* Rpl        call s:Rpl(<f-args>)
command! -nargs=* C          call s:AdvCmmdNew(<f-args>)
command! -nargs=* Rp         call s:Rp(<f-args>)
command! -nargs=* Rpi        call s:Rpi(<f-args>)
command! -nargs=* Srch       call s:Srch(<f-args>)
command! -nargs=* Srh        call s:Srch(<f-args>)
command! -nargs=* Srhi       call s:Srchi(<f-args>)
command! -nargs=* Sr         call s:Srch(<f-args>)
command! -nargs=* Ed         call s:Ed(<f-args>)
command! -nargs=* E          call s:Ed(<f-args>)
command! -nargs=* MyFunc     call s:MyFunc(<f-args>)
command! -nargs=* CPDUM      call s:CopyFileDummy(<f-args>)
command! -nargs=* VSS        call s:VSS(<f-args>)
command! -nargs=* SS         call s:VSS(<f-args>)
command!          Brow       call s:VbFunction()
command! -nargs=* Cln        call s:Clean(<f-args>)
command! -nargs=* Clean      call s:Clean(<f-args>)
command! -nargs=* HELPWORDS  call HelpWords(<f-args>)
command! -nargs=* HW         call HelpWords(<f-args>)

command! -nargs=* Rectcp     call s:RectangleCopy(<f-args>)
command! -nargs=* Cut        call s:RectangleCopy(<f-args>)
command! -nargs=* Pas        call s:RectPaste(<f-args>)
command! -nargs=* Jstfy      call s:Justify(<f-args>)
command! -nargs=* Add        call s:Add(<f-args>)

"""""""""""""""""""""" End of Plugin Commands VIM

""""""""""""""""""""""""""""""""""" Global variables
let moria_fontface = 'mixed'
let loaded_myvim = 1

""""""""""""""""""""""""""""""""""" Key Mapping

map  <TAB> i    <ESC><HOME>
:nnoremap gf <C-W>gf
:nmap ,s :source $VIM/_vimrc
:nmap ,v :e $VIM/_vimrc

nmap <silent> <s-down>   <c-w>j
nmap <silent> <s-up>     <c-w>k
nmap <s-right>             :tabnext<CR>
nmap <s-left>              :tabprev<CR>

silent nnoremap <silent> <c-f> :call ActivateGUIMenu()<CR>
silent nnoremap <silent> <c-c> :call StartMarkForRectangleCopy()<CR>
silent nnoremap <silent> <c-y> :call PasteMarkForRectangleCopy()<CR>

"silent map <silent> <a--> :sil tabnew $VIM\vim71\vimrc<CR> :sil tabnew $VIM\vim71\plugin\tips<CR>  :sil tabnew $VIM\vim71\plugin\myvim.vim<CR>
silent map <silent> <a-->   :sil tabnew c:\windows\system32\drivers\etc\hosts<CR>  :sil tabnew $VIM\vim71\plugin\myvim.vim<CR>

:nmap <silent> <f5>    :call StartMarkNormalCopy(0)<CR>
" Just Copies other lines are intact
:nmap <silent> <s-f5>  :call StartMarkNormalCopy(1)<CR>
:nmap <silent> <f6>  :call StartMarkNormalCopy(1)<CR>
" Copies while deleting lines

"Compiles the code if VIM knows compile option for particular type of file
"Checks out current file from version control system

silent map <silent> <s-f12>  :SS chk<CR>
silent map <silent> <s-f11>  :SS chi<CR>
silent map <silent> <s-f10>  :Cmp<CR>

" File compilation/CheckIn/CheckOut sequence 
silent map <silent> <f12>  :SS chk<CR>
silent map <silent> <f11>  :SS chi<CR>
silent map <silent> <f10>  :Cmp<CR>

" Picking random color sequence
silent map <silent> <a-c>  :call PickRandomColor()<CR>

" Ease of use for selecting project
silent map <silent> <f1>   :call SelectProject(0)<CR>
silent map <silent> <s-f1> :call SelectProject(1)<CR>
nmap       <silent> <s-pagedown>   :q!<CR>
nmap       <silent> <s-down>    <c-w>j
nmap       <silent> <s-up>      <c-w>k
"nmap      <silent> <c-f1>         :call C_CommentTemplates("function")<CR> 
nnoremap   <silent> <c-down>    <c-w>+
nnoremap   <silent> <c-up>      <c-w>-
nmap     <silent> <s-left>       :tabp<CR>
nmap     <silent> <s-right>      :tabn<CR>


" Folding and unfolding of lines
silent map <silent> <f9> :call FoldLines()<CR>
silent map <silent> <s-f2> zD<CR>
silent map <silent> <s-f3> zE<CR>

" Games
silent map <silent> <s-f9> :call Play()<CR>

" Paste all result
:nmap <silent> <f8>    "zp
silent nnoremap <silent> <c-p> "zp

" Searching/Replacement options
map        s  *
silent map S  *           :promptrepl<CR>
nnoremap   <silent> <F3>  :Grep<CR>

noremap  <silent> <s-end>  :!start make clean <CR>
silent map <S-l>   1000x
silent map <S-h>   1000X
map <F2> :w!<CR><ESC>

setlocal makeprg=make
map <TAB> i    <ESC><HOME>
vmap <F1> "vy
"map z  *
"map f               :call FoldLines()<CR>
"map f  %
map u :u<CR>

""""""""""  VIM Options
set autoread    " read open files again when changed outside Vim
":g/{/ ,/}/- s/\n\+/\r/g
":v/\S/d
set go+=a
set lcs+=tab:»\         " 187 followed by a space (032) (and no, it
set ic
set ignorecase
set smartcase
setlocal noswapfile
set noswapfile
set ar

" Changes the path to current directory
au BufEnter *   execute ":lcd " . expand("%:p:h") 
au GUIEnter * simalt ~x

"set statusline=%<%f%<%{FileTime()}%<%h%m%r%=%-30.(l=%03l,c=%02c%V,L=%L%)\%h%m%r%=%-30(,BfNm=%n%Y%)\%P\*%=%{CurTime()}
"set statusline=%<%f%<%{FileTime()}%<%h%m%r%=%-0.(l=%03l,c=%02c%V,L=%L%)\%h%m%r%=%{CurTime()}
"set statusline=%<%f%<%<%h%m%r%=%(B=%02B,Off=%o,l=%03l,c=%02c%V,L=%L%)\%h%m%r%=%{CurTime()}
set statusline=%<%f%<%<%h%m%r%=%(l=%l,c=%c%V,B=%02B,(%L%),off=%o)\%h%m%r%=%{CurTime()}
set backupdir=d:\
"shortcuts
set guioptions -=T
set guioptions -=m
set mouse=a
set autoread                          " read open files again when changed outside Vim
"set gcr=a:blinkon0
set gcr=o:hor50-Cursor
set gcr=sm:block-Cursor-blinkwait175-blinkoff150-blinkon175
set autowrite                         " write a modified buffer on each :next , ...
set browsedir=current
set ttyfast
set nocompatible    " Use Vim defaults (much better!)
set lazyredraw          " vks
set laststatus=2        " vks
"set guioptions-=m       " vks
set bs=2       " allow backspacing over everything in insert mode
"set cp
set ai            " always set autoindenting on
"set bg=light
"set bg=1
"set backup        " keep a backup file
set viminfo='20,\"50    " read/write a .viminfo file, don't store more
            " than 50 lines of registers
set history=250000     " keep 250 lines of command line history
set ruler        " show the cursor position all the time
set rulerformat=%15(%c%V\ %p%%%)
set vb
set backspace=indent,start

""""""""""" Color Scheme and Font
"set bg=dark
"set bg=light
"colo smp
"let g:color='smp_new'
"colo smp_new
"let g:color='sienna'
"set bg=torte
colo synic
let g:color='synic'
"colo print_bw
"let g:color='print_bw'
"set guifont=Courier_New:h12:cANSI
"set guifont=Bitstream_Vera_Sans_Mono:h14:cANSI
"set guifont=Courier_New:h14:cANSI
"set guifont=Bitstream_Vera_Sans_Mono:h14:cANSI
"set guifont=Courier_New:h16:cANSI
"set guifont=Bitstream_Vera_Sans_Mono:h14:cANSI
set guifont=Consolas:h14:cANSI
set guifont=Bitstream_Vera_Sans_Mono:h14:cANSI
set guifont=Bitstream_Vera_Sans_Mono:h12:cANSI
set guifont=Console9_437:h11:cOEM
set guifont=Consolas:h14:cANSI
set guifont=Courier_New:h14:cANSI
au BufWinLeave * mkview
au BufWinEnter * silent loadview
"set guifont=Bitstream_Vera_Sans_Mono:h12:cANSI

"""""""""""""" Other VIM Options
" toggle on/off
let $flist = 'flist.txt'
       if filereadable( $flist )
           source flist.txt
       else
"           echo 'file does not exist'
       endif

let $conf = 'c:/vim'
if filereadable( $conf )
      source $conf
endif
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Only do this part when compiled with support for autocommands.
if has("autocmd")

" In text files, always limit the width of text to 78 characters
"autocmd BufRead *.txt set tw=78

 augroup cprog
  " Remove all cprog autocommands
  au!

  " When starting to edit a file:
  "   For C and C++ files set formatting of comments and set C-indenting on.
  "   For other files switch it off.
  "   Don't change the order, it's important that the line with * comes first.
  autocmd FileType *         set formatoptions=tcql nocindent comments&
  autocmd FileType c,cpp     set formatoptions=croql cindent comments=sr:/*,mb:*,el:*/,://
  autocmd FileType make      set formatoptions=croql cindent comments=:#
  autocmd FileType DOSBATCH  set formatoptions=croql cindent comments=:REM 
  autocmd FileType VB        set formatoptions=croql cindent comments=:' 

  "autocmd FileReadPost     *.txt exe('Clean 1')
  "autocmd FileReadPre      *.txt exe('r d:\c')
  augroup END

 augroup END

 " This is disabled, because it changes the jumplist.  Can't use CTRL-O to go
 " back to positions in previous files more than once.
 if 0
  " When editing a file, always jump to the last cursor position.
  " This must be after the uncompress commands.
   autocmd BufReadPost * if line("'\"") && line("'\"") <= line("$") | exe "normal `\"" | endif
 endif

endif " has("autocmd"

set tabstop=4
set shiftwidth=4
set expandtab 
set list listchars=tab:»·,trail:· 
set softtabstop=4   " makes the spaces feel like real tabs 
au FileType make setlocal noexpandtab

set laststatus=2        "show status line for multiple buffers only

let g:src='d:\work\"Vitronic Scripts"\src'
let g:dst_src='f:\Test_2006\src'

let g:src_pic='d:\pics\tmp4'
let g:dst_pic='d:\pics'

let g:dst_ip1_picc='\\' . $CAPTURE1 . '\d\pics'
let g:dst_ip2_picc='\\' . $CAPTURE2 . '\d\pics'
let g:dst_ip2_picc='\\' . $CAPTURE3 . '\d\pics'

let g:dst_ip1_picd='\\' . $DECODE1 . '\d\pics'
let g:dst_ip2_picd='\\' . $DECODE2 . '\d\pics'
let g:dst_ip2_picd='\\' . $DECODE3 . '\d\pics'


" The function Nr2Hex() returns the Hex string of a number.
func s:Nr2Hex(nr)
  let n = a:nr
  let r = ""
  while n
    let r = '0123456789ABCDEF'[n % 16] . r
    let n = n / 16
  endwhile
  return r
endfunc

func s:Clean(...)

   if  a:0 == 0
        echo 'Clean 0  Help'
        echo 'Clean 1  Delete trailing space and tabs'
        echo 'Clean 2  Delete multiple empty lines'
        echo 'Clean 3  Delete all empty lines between {}'
        echo 'Clean 4  Delete all empty lines'
        echo 'Clean 5  Remove all tabs'
        echo 'Clean 7  Replace last search'
        echo 'Clean 8  Remove multiple lines and tabs spaces'
      return
   elsif a:0 == 1
      let re=a:1
   else
       let re=''
       let k = 0
       while k < a:0
            let re = re . ' ' . a:000[k]
            let x=a:000[k]
            echo x
    if x=='1'
        let s=line('.')
        exe 'sil 1,$s/		 /    /g'
        let cmmnd='sil %s#\s*\r\?$##'
        exe cmmnd
        exe ':' . s
    elseif x=='2'
        let s=line('.')
        let cmmnd='%s/^\n\+/\r/'
        exe cmmnd
        exe ':' . s
    elseif x=='3'
        let s=line('.')
        let cmmnd='%s/^/\=strpart(line('.')."        ",0,&ts)'
        exe cmmnd
        exe ':' . s
    elseif x=='4'
        let s=line('.')
        let cmmnd='v/\S/d'
        exe cmmnd
        exe ':' . s
    elseif x=='5'
        "Delete empty lines but only between {...}
        let s=line('.')
        let cmmnd='g/{/ ,/}/- s/\n\+/\r/g'
        exe cmmnd
        exe ':' . s
    elseif x=='6'
        let s=line('.')
        exe 'sil 1,$s/		 /    /g'
        let cmmnd='sil %s#\s*\r\?$##'
        exe cmmnd
        exe ':' . s
    elseif x=='7'
        ":%s/<c-r>//<new string>/g 
    elseif x=='8'
        let s=line('.')
        exe 'sil 1,$s/		 /    /g'
        let cmmnd='sil %s#\s*\r\?$##'
        exe cmmnd
        exe ':' . s
        let s=line('.')
        let cmmnd='%s/^\n\+/\r/'
        exe cmmnd
        exe ':' . s
    else
        echo x .' Unknown operation'
    endif
            let k=k+1
       endw
   endif

endfunc


" The function String2Hex() converts each character in a string to a two
" character Hex string.
func! s:String2Hex(str)
  let out = ''
  let ix = 0
  while ix < strlen(a:str)
    let out = out . s:Nr2Hex(char2nr(a:str[ix]))
    let ix = ix + 1
  endwhile
  return out
endfunc

" The function TrimString() removes white spaces
func! s:TrimString(str)
  let out = ''
  let ix = 0
  while ix < strlen(a:str)
    if(a:str[ix]==' ')
    else
    let out = out . a:str[ix]
    endif
    let ix = ix + 1
  endwhile
  return out
endfunc

func! s:Stuff(str,justif,fillchar,len)
  let out = a:str
  let intendedlen  = a:len
  let left_or_right = a:justif
  let fillch=a:fillchar

  while intendedlen > strlen(out)
    if left_or_right > 0 
        let out = out . fillch
    else
        let out = fillch . out
    endif
  endwhile
  return out
endfunc

function! s:RmSpace(...)

    if a:cmd==0
        echo 'Rm 0  Help'
        echo 'Rm 1  Delete trailing space and tabs'
        echo 'Rm 2  Delete multiple empty lines'
        echo 'Rm 3  Delete all empty lines between {}'
        echo 'Rm 4  Delete all empty lines'
        echo 'Rm 5  Remove all tabs'
        echo 'Rm 7  Replace last search'
    elseif a:cmd==1
        let s=line('.')
        exe 'sil 1,$s/		 /    /g'
        let cmmnd='sil %s#\s*\r\?$##'
        exe cmmnd
        exe ':' . s
    elseif a:cmd==2
        let s=line('.')
        let cmmnd='%s/^\n\+/\r/'
        exe cmmnd
        exe ':' . s
    elseif a:cmd==3
        let s=line('.')
        let cmmnd='%s/^/\=strpart(line('.')."        ",0,&ts)'
        exe cmmnd
        exe ':' . s
    elseif a:cmd==4
        let s=line('.')
        let cmmnd='v/\S/d'
        exe cmmnd
        exe ':' . s
    elseif a:cmd==5
        "Delete empty lines but only between {...}
        let s=line('.')
        let cmmnd='g/{/ ,/}/- s/\n\+/\r/g'
        exe cmmnd
        exe ':' . s
    elseif a:cmd==6
        let s=line('.')
        exe 'sil 1,$s/		 /    /g'
        let cmmnd='sil %s#\s*\r\?$##'
        exe cmmnd
        exe ':' . s
    elseif a:cmd==7
        ":%s/<c-r>//<new string>/g 
    else
        echo 'Rm 0  Help'
        echo 'Rm 1  Delete trailing space and tabs'
        echo 'Rm 2  Delete multiple empty lines'
        echo 'Rm 3  Delete all empty lines between {}'
        echo 'Rm 4  Delete all empty lines'
        echo 'Rm 5  Remove all tabs'
    endif
endfunction

func! ColCopy(...)

   let x1=0
   let y1=0
   let x2=0
   let y2=0
   let k=0
   let re=''

   if a:0 > 0
       while k < a:0
            let re = re . ' ' . a:000[k]
            let k=k+1
       endw
       echo re
   endif
   if ((k==0) || (k==4))
   else
     echo "minimum 4 or 0 arguments"
     return
   endif
   if((x2<x1) || (y2<y1))
     echo "Invalid x and y cordinates"
     echo "x2>x1 and y2>y1"
     echo "example: Cut 2(x1) 3(y1) 4(x2) 5(y2)"
     return
   endif

    if(k==4) 
        let s=line('.')
        let start=line("1")
        let end=line("$")
        let longtestlen=0

        while (start <= end)
            let len = strlen(getline(start))

            if(len > longtestlen)
                let longtestlen=len
            endif

            let start = start + 1
        endwhile

        let beautymargin=longtestlen+1
        let start=line("1")

        while (start <= end)
            let str = getline(start)
            let src_op1=getline(start)
            let src_op2=getline(start)
            "let len=strlen(x)

            let src_op1=s:Stuff(src_op1,1," ",beautymargin)
            let result_string=(src_op1 . src_op2)

            call setline(start,result_string)
            let start = start + 1
            endwhile
            exe ':' . s
    else
        "let start=line(".") " From current line
        let s=line('.')
        let start=line("1")
        let end=line("$")
        let longtestlen=0

        while (start <= end)
            let len = strlen(getline(start))

            if(len > longtestlen)
                let longtestlen=len
            endif

            let start = start + 1
        endwhile

        let beautymargin=longtestlen+1
        let start=line("1")

        while (start <= end)
            let str = getline(start)
            let src_op1=getline(start)
            let src_op2=getline(start)
            "let len=strlen(x)

            let src_op1=s:Stuff(src_op1,1," ",beautymargin)
            let result_string=(src_op1 . src_op2)

            call setline(start,result_string)
            let start = start + 1
            endwhile
            exe ':' . s
    endif

endfunc

function! s:RunDir()

  let path='dir /od/b' . ' c:\'
  let dir=system(path)

  for var in dir
      echo x
  endfor

endfunction

function! s:GetNthItemFromList(list, n) 
   let itemStart = 0
   let itemEnd = -1
   let pos = 0
   let item = ""
   let i = 0
   while (i != a:n)
      let itemStart = itemEnd + 1
      let itemEnd = match(a:list, ",", itemStart)
      let i = i + 1
      if (itemEnd == -1)
         if (i == a:n)
            let itemEnd = strlen(a:list)
         endif
         break
      endif
   endwhile 
   if (itemEnd != -1) 
      let item = strpart(a:list, itemStart, itemEnd - itemStart)
   endif
   return item 
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:MyFunc(cmd)

    let success=0
    let ext=tolower(expand("%:e"))
    let fname=tolower(expand('%<'))
    if ext=='bat'
        exe 'Rm 1'
        exe 'Rm 2'
    elseif ext=='c'
        exe 'Rm 1'
        exe 'Rm 2'
        exe 'Rm 3'
    elseif ext=='ini'
        exe 'Rm 1'
        exe 'Rm 2'
    elseif ext=='h'
        exe 'Rm 1'
        exe 'Rm 2'
        exe 'Rm 3'
    elseif ext=='txt'
        exe 'Rm 1'
        exe 'Rm 2'
        exe 'Rm 3'
    elseif ext=='vbs'
        exe 'Rm 1'
        exe 'Rm 2'
        exe 'Rm 3'
        let success=5
    elseif ext=='cpp'
        exe 'Rm 1'
        exe 'Rm 2'
        exe 'Rm 3'
    elseif ext=='mak'
    elseif ext=='cfg'
        exe 'Rm 1'
        exe 'Rm 2'
    elseif fname=='vim'
    elseif fname=='makefile'
    else
        exe 'Rm 1'
        exe 'Rm 2'
        echo 'File type unknown'
    endif

    if success==1
        echo "Program compiled"
    elseif success==2
        echo "Build Started"
    elseif success==3
        echo "VIM file sourced"
    elseif success==4
        echo "Make completed"
    elseif success==5
        echo "Tab removed"
    else
        echo 'Unknown option'
    endif


endfunction
""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:Compile()

    let success=0
    let ext=tolower(expand("%:e"))
    let fname=tolower(expand('%<'))
    if ext=='bat'
        "exe 'sil 1,$s/		 /    /g'
        "exe 'Rm 1'
        "exe 'sil !c:\sw\bin\quickbfc.exe % ' . '../\exe/\' . fname . '.exe'
        "exe 'sil !start c:\sw\quickbfc\quickbfc.exe % ' . '..\exe\' . fname . '.exe'
        exe 'sil !start c:\sw\quickbfc\quickbfc.exe % ' . fname . '.exe'
        "exe 'sil !start c:\sw\quickbfc\quickbfc.exe %'
        let success=1
    elseif ext=='c'
        exe 'sil 1,$s/		 /    /g'
        exe 'sil !gcc ' . fname . '.' . ext . ' -o ' . fname . '.exe 2>error'
        let success=1
    elseif ext=='ini'
        exe 'Rm 1'
        let success=1
    elseif ext=='h'
        let s=line('.')
        exe 'sil 1,$s/		 /    /g'
        exe 'Rm 1'
        exe ':' . s
        let success=5
    elseif ext=='txt'
        let s=line('.')
        exe 'sil 1,$s/		 /    /g'
        exe 'Rm 1'
        exe ':' . s
        let success=5
    elseif ext=='vbs'
        let s=line('.')
        exe 'sil 1,$s/		 /    /g'
        exe 'Rm 1'
        exe ':' . s
        let success=5
    elseif ext=='cpp'
        "exe 'sil !g++ ' . fname . '.' . ext . ' -o ' . fname . '.exe 2>error'
        let s=line('.')
        exe 'sil 1,$s/		 /    /g'
        exe 'Rm 1'
        exe ':' . s
        let success=5
    elseif ext=='dem'
        let cmd= 'sil !start C:\sw\gnuplot\bin\pgnuplot.exe  ' . expand("%")
        exe cmd
        let success=2
    elseif ext=='mak'
        let cmd= 'sil !start nmake -f ' . expand("%")
        exe cmd
        let success=2
    elseif ext=='cfg'
        if fname=='fedex_scripts'
            let cmd= 'sil !del /q D:\work\"Vitronic Scripts"\pcinstall\temp\*.*'
            exe cmd
            let cmd= 'sil !del D:\work\"Vitronic Scripts"\pcinstall\Onefile\' . fname . '.exe'
            exe cmd
            exe 'sil !start c:\sw\pcinst\qbuild.exe ' . 'cfg="D:\work\Vitronic Scripts\pcinstall\' . fname . '.cfg" ' . 'rebuild="1" builddir="D:\work\Vitronic Scripts\pcinstall" type="1" ver="1" show="0"'
        elseif fname=='fedex_pics'
            let cmd= 'sil !del /q D:\work\"Vitronic Scripts"\pcinstall\temp\*.*'
            exe cmd
            let cmd= 'sil !del D:\work\"Vitronic Scripts"\pcinstall\Onefile\' . fname . '.exe'
            exe cmd
            exe 'sil !start c:\sw\pcinst\qbuild.exe ' . 'cfg="D:\work\Vitronic Scripts\pcinstall\' . fname . '.cfg" ' . 'rebuild="1" builddir="D:\work\Vitronic Scripts\pcinstall" type="1" ver="1" show="0"'
        elseif fname=='dhl_scripts'
            let cmd= 'sil !del /q D:\work\"Vitronic Scripts"\pcinstall\temp\*.*'
            exe cmd
            let cmd= 'sil !del D:\work\"Vitronic Scripts"\pcinstall\Onefile\' . fname . '.exe'
            exe cmd
            exe 'sil !start c:\sw\pcinst\qbuild.exe ' . 'cfg="D:\work\Vitronic Scripts\pcinstall\' . fname . '.cfg" ' . 'rebuild="1" builddir="D:\work\Vitronic Scripts\pcinstall" type="1" ver="1" show="0"'
        elseif fname=='target_scripts'
            let cmd= 'sil !del /q D:\work\"Vitronic Scripts"\pcinstall\temp\*.*'
            exe cmd
            let cmd= 'sil !del D:\work\"Vitronic Scripts"\pcinstall\Onefile\' . fname . '.exe'
            exe cmd
            exe 'sil !start c:\sw\pcinst\qbuild.exe ' . 'cfg="D:\work\Vitronic Scripts\pcinstall\' . fname . '.cfg" ' . 'rebuild="1" builddir="D:\work\Vitronic Scripts\pcinstall" type="1" ver="1" show="0"'
        elseif fname=='comark_scripts'
            let cmd= 'sil !del /q D:\work\"Vitronic Scripts"\pcinstall\temp\*.*'
            exe cmd
            let cmd= 'sil !del D:\work\"Vitronic Scripts"\pcinstall\Onefile\' . fname . '.exe'
            exe cmd
            exe 'sil !start c:\sw\pcinst\qbuild.exe ' . 'cfg="D:\work\Vitronic Scripts\pcinstall\' . fname . '.cfg" ' . 'rebuild="1" builddir="D:\work\Vitronic Scripts\pcinstall" type="1" ver="1" show="0"'
        elseif fname=='target_scripts'
        elseif fname=='fedex_upgrade'
            let cmd= 'sil !del /q D:\work\"Vitronic Scripts"\pcinstall\temp\*.*'
            exe cmd
            let cmd= 'sil !del D:\work\"Vitronic Scripts"\pcinstall\Onefile\' . fname . '.exe'
            exe cmd
            exe 'sil !start c:\sw\pcinst\qbuild.exe ' . 'cfg="D:\work\Vitronic Scripts\pcinstall\' . fname . '.cfg" ' . 'rebuild="1" builddir="D:\work\Vitronic Scripts\pcinstall" type="1" ver="1" show="0"'
        elseif fname=='dhl'
            let cmd= 'sil !del /q D:\work\"Vitronic Scripts"\pcinstall\temp\*.*'
            exe cmd
            let cmd= 'sil !del D:\work\"DHL IMAGING CD"\PcInstall\Onefile\' . fname . '.exe'
            exe cmd
            exe 'sil !start c:\sw\pcinst\qbuild.exe ' . 'cfg="D:\work\DHL IMAGING CD\pcinstall\' . fname . '.cfg" ' . 'rebuild="1" builddir="D:\work\DHL IMAGING CD\pcinstall" type="1" ver="1" show="0"'
        elseif fname=='dhl_022207_upgrade'
            let cmd= 'sil !del /q D:\work\"Vitronic Scripts"\pcinstall\temp\*.*'
            exe cmd
            let cmd= 'sil !del D:\work\"Vitronic Scripts"\"DHL IMAGING CD"\PcInstall\Onefile\' . fname . '.exe'
            exe cmd
            exe 'sil !start c:\sw\pcinst\qbuild.exe ' . 'cfg="D:\work\DHL IMAGING CD\pcinstall\' . fname . '.cfg" ' . 'rebuild="1" builddir="D:\work\DHL IMAGING CD\pcinstall" type="1" ver="1" show="0"'
        elseif fname=='dhl_upgrade'
            let cmd= 'sil !del /q D:\work\"Vitronic Scripts"\pcinstall\temp\*.*'
            exe cmd
            let cmd= 'sil !del D:\work\"Vitronic Scripts"\"DHL IMAGING CD"\PcInstall\Onefile\' . fname . '.exe'
            exe cmd
            exe 'sil !start c:\sw\pcinst\qbuild.exe ' . 'cfg="D:\work\DHL IMAGING CD\pcinstall\' . fname . '.cfg" ' . 'rebuild="1" builddir="D:\work\DHL IMAGING CD\pcinstall" type="1" ver="1" show="0"'
            else
            let cmd= 'sil !del /q D:\work\"Vitronic Scripts"\pcinstall\temp\*.*'
            exe cmd
            let cmd= 'sil !del D:\work\PcInstall\Onefile\' . fname . '.exe'
            exe cmd
            exe 'sil !start c:\sw\pcinst\qbuild.exe ' . 'cfg="D:\work\PcInstall\' . fname . '.cfg" ' . 'rebuild="1" builddir="D:\work\PcInstall" type="1" ver="1" show="0"'
        endif
        let success=2
    elseif fname=='vim'
        source %
        let success=3
    elseif fname=='makefile'
        exe 'sil !make ' . fname . ' ' . '2>error'
        let success=4
    else
        echo 'Unknown option'
    endif

    if success==1
        echo "Program compiled"
    elseif success==2
        echo "Build Started"
    elseif success==3
        echo "VIM file sourced"
    elseif success==4
        echo "Make completed"
    elseif success==5
        echo "Tab removed"
    else
        echo 'Unknown option'
    endif


endfunction
""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:VSS(...)

   if  a:0 == 0
        echo 'Help:'
        echo 'chk CheckOut File'
        echo 'chi CheckIn File'
        echo 'get CheckIn File'
        echo 'undo checkOut File'
        echo 'add checkOut File'
        echo 'his history of File'
        echo 'del delete file from project'
        echo 'getall'
   return
   elsif a:0 == 1
      let re=a:1
   else
       let re=''
       let k = 0
       while k < a:0
            let re = re . ' ' . a:000[k]
            let k=k+1
       endw
   endif
   let cmd=s:TrimString(re)
   echo cmd

    let ext=tolower(expand("%:e"))
    let fname=tolower(expand('%<'))
    let filename=fname . '.' . ext

    let tdir=system('cd')
    let tdir=substitute(tdir, '\n','', 'g')
    let tdir=substitute(tdir, 'D','', 'ic')
    let tdir=substitute(tdir, ':','', 'ic')
    let tdir=substitute(tdir, '\\','', 'ic')

    let tdir=substitute(tdir, 'w','', 'ic')
    let tdir=substitute(tdir, 'o','', 'ic')
    let tdir=substitute(tdir, 'r','', 'ic')
    let tdir=substitute(tdir, 'k','', 'ic')

    let tdir= '"$\' . 'vitronic' . tdir . '"'
    let path='sil !ss project ' . tdir
    "let path='sil !ss cp ' . tdir
    "echo path
    "let tdir=substitute(tdir, '\\','//', 'g')
    exe path

    if cmd=='cho'
        let sscmd='sil !ss checkout ' . filename
        exe sscmd
    elseif cmd=='chk'
        let sscmd='sil !ss checkout ' . filename
        exe sscmd
    elseif cmd=='get'
        let sscmd='sil !ss get ' . filename
        exe sscmd
    elseif cmd=='chi'
        let  sscmd='sil !ss checkin ' . filename
        exe  sscmd
    elseif cmd=='und'
        let sscmd='sil !ss undocheckout ' . filename
        exe sscmd
    elseif cmd=='undo'
        let sscmd='sil !ss undocheckout ' . filename
        exe sscmd
    elseif cmd=='add'
        let sscmd='sil !ss undocheckout ' . filename
        exe sscmd
    elseif cmd=='diff'
        let sscmd='!ss diff ' . filename
        exe sscmd
    elseif cmd=='his'
        let sscmd='!ss history ' . filename
        exe sscmd
    elseif cmd=='getall'
        let sscmd='sil !ss get *.*'
        exe sscmd
    elseif cmd=='del'
        let sscmd='sil !ss delete filename'
        exe sscmd
    elseif cmd=='chk'
        let sscmd='sil !ss checkout ' . filename
        exe sscmd
    elseif cmd=='get'
        let sscmd='sil !ss get ' . file
        exe sscmd
    else
        echo 'Unknown Command'
        echo 'chk CheckOut File'
        echo 'chi CheckIn File'
        echo 'get CheckIn File'
        echo 'undo checkOut File'
        echo 'add checkOut File'
        echo 'his history of File'
        echo 'del delete file from project'
        echo 'getall'
        return
    endif
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""
fu! FileTime()
        let Font=&guifont
        let ext=tolower(expand("%:e"))
        let fname=tolower(expand('%<'))
        let filename=fname . '.' . ext
        let msg=strftime("(Modified %b,%d %y %H:%M:%S)",getftime(filename))
        let msg=msg . ' ' . Font . ' ' . g:color
        return msg
endf

fu! CurTime()
        let ftime=strftime("%b,%d %y %H:%M:%S")
        return ftime
endf

function! StartMarkNormalCopy(x)

    let y=a:x

    if g:startmark==0
        if y==0
            exe('ma s')
            let g:linenum = line('.')
            echo 'line marked at=' . g:linenum . ', move curser press f6(to yank) and < f8 to paste>'
            :nmap <silent> <f6> "zy's
            let g:startmark=2
        else
            exe('ma s')
            let g:linenum = line('.')
            echo 'line marked at=' . g:linenum . ', move curser press f6(to cut) and < f8 to paste>'
            :nmap <silent> <f6>  "zd's
            let g:startmark=3
        endif
        let g:startmark=0
    else
      if g:startmark==2
          SendKeys(f7)
          if y==1 Then
             echo 'wrong key pressed,start again'
             let g:startmark=0
          else
              :nmap <silent> <f5>  :call StartMarkNormalCopy(0)<CR>
              echo 'action completed(yank)'
             let g:startmark=0
          endif
      elseif g:startmark==3
          if y==0 Then
             echo 'wrong key pressed,start again'
             let g:startmark=0
          else
              :nmap <silent> <f6>  :call StartMarkNormalCopy(1)<CR>
              echo 'action completed(cut)'
             let g:startmark=0
          endif
      endif

        "let g:linenum = line('.') - g:linenum
        "echo 'Lines yanked=' . g:linenum . 'f8(paste)'

    endif
endfunction

function! ActivateGUIMenu()
    if(g:ActivateGUIMenu)
        let g:ActivateGUIMenu=0
        "echo 'Menu ON'
        set guioptions +=T
        set guioptions +=m
        set noic
        set noautochdir
        if g:color==''
            colo smp
        endif
        set nu
        echo FileTime()
    else
        let g:ActivateGUIMenu=1
        "echo 'Menu OFF'
        echo FileTime()
        set guioptions -=T
        set guioptions -=m
        set ic
        set autochdir
        set nonu
    endif
endfunction

""""""""""""""""""""""
"function! s:BrowWithWinExplFunc(ExploreDir)
function! s:BrowWithWinExplFunc()
    let tdir=system('cd')
    let cmds='sil !cmd /c start ' . $VIM . '\vim71\vbscript\explorer.vbs ' . tdir
    exe cmds
endfunction

""""""""""""""""""""""

function! s:VbFunction()
    let tdir=system('cd')
    let cmds='sil !cmd /c start ' . $VIM . '\vim71\vbscript\selectfile.vbs ' . tdir
    exe cmds
endfunction

""""""""""""""""""""""
function! s:MyTemplate()
    let tdir=system('cd')
    let cmds='sil !cmd /c start ' . $VIM . '\vim71\vbscript\selectfile.vbs ' . tdir
    exe cmds
endfunction

""""""""""""""""""""""
fun! HelpWords(...)

" /\c\(.\|.\n\)*w1\&\(.\|.\n\)*w2\&\(.\|.\n\)*w3
" -- searched words (w1,w2,w3) in any order within same paragraph
   if  a:0 == 0
      echo "Missing arguments"
      return
   elsif a:0 == 1
      let re=a:1
   else
       let re='\c'
       let k = 0
       while k < a:0
            let re = re . '\(.\|.\n\)*' . a:000[k]
            if k != a:0 - 1
                let re = re . '\&'
            endif
            let k=k+1
       endw
   endif
   " let @/ = re
   redraw

   " regexp of :helpgrep does not modify @/
   " if we want to debug HELPWORDS, we want o leave
   " regexp used in global var
   let g:helpwords_re=re
   let g:helpwords_argc=a:0
   let g:helpwords_cmd="helpgrep " . re
   "echo "argc=".a:0

   echo ": helpgrep " . re
   exe "helpgrep " . re
endfun
""""""""""""""""""""""
fun!  s:Srchi(...)

endfun
""""""""""""""""""""""
fun!  s:Srch(...)
   if  a:0 == 0
      echo "Missing arguments"
      return
   elsif a:0 == 1
      let re=a:1
   else
       let re=''
       let k = 0
       while k < a:0
            if k==0
                let re = re . '/' . a:000[k]
            else
                let re = re . '\|' . a:000[k]
            endif
            let k=k+1
       endw
   endif

   let ext=tolower(expand("%:e"))
   let fname=tolower(expand('%<'))
   let re=s:TrimString(re)
   echo re

   "exe re

endfun

""""""""""""""""""""""
fun! s:Rpi(...)

   if  a:0 == 0
      echo "Missing arguments"
      return
   elsif a:0 == 1
      let re=a:1
   else
       let re=''
       let k = 0
       while k < a:0
            let re = re . ' ' . a:000[k]
            let k=k+1
       endw
   endif

   let ext=tolower(expand("%:e"))
   let fname=tolower(expand('%<'))
   let re=s:TrimString(re)

   exe '.,$s//' . re . '/gc"'

endfun

""""""""""""""""""""""
fun! s:Rp(...)

   if  a:0 == 0
      echo "Missing arguments"
      return
   elsif a:0 == 1
      let re=a:1
   else
       let re=''
       let k = 0
       while k < a:0
            let re = re . ' ' . a:000[k]
            let k=k+1
       endw
   endif

   let ext=tolower(expand("%:e"))
   let fname=tolower(expand('%<'))
   let re=s:TrimString(re)

   exe '.,$s//' . re . '/igc"'

endfun

""""""""""""""""""""""
fun! s:Rpl(...)

   if  a:0 == 0
      echo "Missing arguments"
      return
   elsif a:0 == 1
      let re=a:1
   else
       let re=''
       let k = 0
       while k < a:0
            let re = re . ' ' . a:000[k]
            let k=k+1
       endw
   endif

   let ext=tolower(expand("%:e"))
   let fname=tolower(expand('%<'))
   let re=s:TrimString(re)

   exe '%s//' . re . '/igc"'

endfun
""""""""""""""""""""""
fun! s:Ed(...)

   if  a:0 == 0
      echo "Missing arguments"
      return
   elsif a:0 == 1
      let re=a:1
   else
       let re=''
       let k = 0
       while k < a:0
            let re = re . ' ' . a:000[k]
            let k=k+1
       endw
   endif
":%s//joe/igc                : Substitute what you last searched for *N*
":%s/~/sue/igc               : Substitute your last replacement string for *N*
   let ext=tolower(expand("%:e"))
   let fname=tolower(expand('%<'))
   let re=s:TrimString(re)

   if(re=='wordpad')
       exe 'sil !start "C:\Program Files\Windows NT\Accessories\wordpad.exe" ' . fname . '.' . ext
   elseif(re=='notepad')
       exe 'sil !start c:\Windows\system32\notepad.exe ' . fname . '.' . ext
   elseif(re=="emacs")
       exe 'sil !start cmd /c start C:\sw\emacs\bin\emacs.exe ' . fname . '.' . ext
   endif

   if(re=='w')
       exe 'sil !start "C:\Program Files\Windows NT\Accessories\wordpad.exe" ' . fname . '.' . ext
   elseif(re=='n')
       exe 'sil !start c:\Windows\system32\notepad.exe ' . fname . '.' . ext
   elseif(re=="e")
       exe 'sil !start cmd /c start C:\sw\emacs\bin\emacs.exe ' . fname . '.' . ext
   endif

   return

    if type(re) == type(0)
        echo "type is number"
    elseif type(re) == type("")
        echo "type is string"
    elseif type(re) == type(function("tr"))
        echo "type is function"
    elseif type(re) == type([])
        echo "type is bracket"
    elseif type(re) == type({})
        echo "type is curly bracket"
    endif

endfun
""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:ExtractNumber(num,whichnumber)
    let x=a:num
    let y=a:whichnumber

    let x1=x/1000
    let tmp=(x%1000)
    let x=tmp
    let x2=x/100
    let tmp=(x%100)
    let x=tmp
    let x3=x/10
    let tmp=(x%10)
    let x4=tmp

    if(y==4) 
        return x4
    elseif(y==3)
        return x3
    elseif(y==2)
        return x2
    elseif(y==1)
        return x1
    endif

endfunction

function! FindCow(n1,n2)
    let x=a:n1
    let y=a:n2


    if((s:CheckNumber(x))==1)
        echo "Only one digit per number"
        return
    endif
    if((s:CheckNumber(y))==1)
        echo "Only one digit per number"
        return
    endif

    let x1=s:ExtractNumber(x,1)
    let x2=s:ExtractNumber(x,2)
    let x3=s:ExtractNumber(x,3)
    let x4=s:ExtractNumber(x,4)

    let y1=s:ExtractNumber(y,1)
    let y2=s:ExtractNumber(y,2)
    let y3=s:ExtractNumber(y,3)
    let y4=s:ExtractNumber(y,4)


    let cow=0

    if((x1==y2) || (x1==y3) || (x1==y4))
        let cow=cow+1
    endif
    if((x2==y1) || (x2==y3) || (x2==y4))
        let cow=cow+1
    endif
    if((x3==y1) || (x3==y2) || (x3==y4))
        let cow=cow+1
    endif
    if((x4==y1) || (x4==y2) || (x4==y3))
        let cow=cow+1
    endif

    return cow

endfun
function! FindBull(n1,n2)
    let x=a:n1
    let y=a:n2


    if((s:CheckNumber(x))==1)
        echo "Only one digit per number"
        return
    endif
    if((s:CheckNumber(y))==1)
        echo "Only one digit per number"
        return
    endif

    let x1=s:ExtractNumber(x,1)
    let x2=s:ExtractNumber(x,2)
    let x3=s:ExtractNumber(x,3)
    let x4=s:ExtractNumber(x,4)

    let y1=s:ExtractNumber(y,1)
    let y2=s:ExtractNumber(y,2)
    let y3=s:ExtractNumber(y,3)
    let y4=s:ExtractNumber(y,4)


    let bull=0

    if(x1==y1)
        let bull=bull+1
    endif
    if(x2==y2)
        let bull=bull+1
    endif
    if(x3==y3)
        let bull=bull+1
    endif
    if(x4==y4)
        let bull=bull+1
    endif

    return bull

endfun

function PickRandomColor()
  let mycolors = split(globpath(&rtp,"**/colors/*.vim"),"\n")
  let random=localtime()
  let color= mycolors[random % len(mycolors)]
  exe 'so ' . color
  "exe 'echo ' . mycolors[random % len(mycolors)]
  unlet mycolors
  let   g:color=color
  echo g:color
endfunction

function Rand(max_num)
    let xx=a:max_num
    let num=(localtime()*localtime())%xx
    while(1==s:CheckNumber(num))
        let num=(localtime()*localtime())%xx
    endwhile
    if(num<0) 
        let num=(-1)*num
    endif
    "echo num
    return num
endfunction

function FindBullCow(num1,num2,bull,cow)

    let g:bull=0
    let g:cow=0

endfunction

function Play()

    let g:NumberList=[]
    let g:totalnum=0
    let g:NumberList=range(0123,9876)
    let g:bull=0
    let g:cow=0

    if g:game==1
        call P1()
        return
    endif
    let g:game = 1

    exe('%d')
    let x=Rand(9876)
    let x=s:Stuff(x,0,"0",4)

    let bull = input("Input bull(" . x . '):')
    while bull==''
        let bull = input("Input bull(" . x . '):')
    endwhile

    let cow  = input("Input cow(" . x . '):')
    while cow==''
        let cow  = input("Input cow(" . x . '):')
    endwhile

    while startnum < endnum
        if(0==s:CheckNumber(startnum))
            if(FindBull(startnum,x)==bull) && (FindCow(startnum,x)==cow)
                let  result_string=s:Stuff(startnum,0,"0",4)
                call add(g:NumberList,startnum)
                call setline(startl,result_string . '=>(' . x . ') '. ' Bull=' . bull . ' Cow=' . cow)
                let startl=startl+1
            endif
        endif
        let startnum=startnum+1
    endwhile
    call setline(startl,'----------------------------------')
    let g:totalnum=startl
    exe('w!')

endfunction

function PlayOld()

    let startnum=0123
    let endnum=9877
    let startl=1
    let g:NumberList=[]
    let g:totalnum=0

    if g:game==1
        call P1()
        return
    endif
    let g:game = 1

    exe('%d')
    let x=Rand(9876)
    let x=s:Stuff(x,0,"0",4)

    let bull = input("Input bull(" . x . '):')
    while bull==''
        let bull = input("Input bull(" . x . '):')
    endwhile

    let cow  = input("Input cow(" . x . '):')
    while cow==''
        let cow  = input("Input cow(" . x . '):')
    endwhile

    while startnum < endnum
        if(0==s:CheckNumber(startnum))
            if(FindBull(startnum,x)==bull) && (FindCow(startnum,x)==cow)
                let  result_string=s:Stuff(startnum,0,"0",4)
                call add(g:NumberList,startnum)
                call setline(startl,result_string . '=>(' . x . ') '. ' Bull=' . bull . ' Cow=' . cow)
                let startl=startl+1
            endif
        endif
        let startnum=startnum+1
    endwhile
    call setline(startl,'----------------------------------')
    let g:totalnum=startl
    exe('w!')

endfunction

function P1()

    let x=Rand(9876)

    let bull = input("Input bull(" . x . '):')
    while bull==''
        let bull = input("Input bull(" . x . '):')
    endwhile

    let cow  = input("Input cow(" . x . '):')
    while cow==''
        let cow  = input("Input cow(" . x . '):')
    endwhile

    let startl=1
    let index=0
    let matched=0
    exe('%d')

    for var in g:NumberList
        if(FindBull(var,x)==bull) && (FindCow(var,x)==cow)
        else
            let rem=remove(g:NumberList,index)
        endif
        let index=index+1
    endfor

    let index=0

    for var in g:NumberList
        call setline(matched+1,var . '=>' . ' Cow=' . cow . ' Bull=' . bull)
        let matched=matched+1
        let matched_num=var
        let index=index+1
    endfor

    call setline(matched+1,'----------------------------------')

    exe('w!')


    if(matched==1)
        echo 'Matched : ' . matched_num
        g:totalnum=0
        let g:game=0
    else
        echo "Total matches " . matched
    endif

endfunction


function s:CheckNumber(num)
    let x=a:num

    let x1=x/1000
    let tmp=(x%1000)
    let x=tmp
    let x2=x/100
    let tmp=(x%100)
    let x=tmp
    let x3=x/10
    let tmp=(x%10)
    let x4=tmp

    if((x1==x2) ||  (x1==x3) ||  (x1==x4) ||  (x2==x1) ||  (x2==x3) ||  (x2==x4) ||  (x3==x1) ||  (x3==x2) ||  (x3==x4) ||  (x4==x1) ||  (x4==x2) ||  (x4==x3))
    return 1
    else
    return 0
    endif

endfunction


""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:AdvCmmdNew(...)

   let xdir=''

   if  a:0 == 0
        echo 'Help:'
        echo 'C Run AdvCmmdNew Command'
        echo 'C dir <Dir>'
        echo 'C file <Dir>'
        echo 'C dos'
        echo 'C init'
        echo 'C copy'
        echo 'C touch'
        echo 'C split'
        let xdir='"' . system('cd') . '"'
        let g:WinMergeDir=xdir
        let cmds='sil !cmd /c start c:\windows\system32\cscript.exe ' . $VIM . '\vim71\vbscript\setvar.vbs ' . xdir
        exe cmds
        return
   elseif a:0 == 1
      let cmd=a:1
   else
       let k = 0
       while k < a:0
            if k==0
                let cmd=a:000[k]
            elseif k==1
                let xdir=a:000[k]
                let xdir='"' . xdir . '"'
                let g:WinMergeDir=xdir
                let cmds='sil !cmd /c start c:\windows\system32\cscript.exe ' . $VIM . '\vim71\vbscript\setvar.vbs ' . xdir
                exe cmds
            endif
            let k=k+1
       endw
   endif

   let cmd=s:TrimString(cmd)

  if cmd=='d'
    let cmd='dir'
  elseif cmd=='f'
    let cmd='file'
  elseif cmd=='cp'
    let cmd='copy'
  elseif cmd=='c'
    let cmd='current'
  elseif cmd=='t'
    let cmd='touch'
  elseif cmd=='i'
    let cmd='init'
  elseif cmd=='e'
    let cmd='explorer'
  elseif cmd=='s'
    let cmd='split'
  endif

   if cmd=='file' || cmd=='dir' || cmd=='copy' || cmd=='touch' || cmd=='current' || cmd=='explorer' || cmd=='split'
       if xdir==''
          if g:WinMergeDir==''

              if (g:WinMergeDir=='')
                  let xdir=$TMPVAR
                  let xdir='"' . xdir . '"'
                  let g:WinMergeDir=xdir
              endif

              if (xdir=='')
                  echo 'please specify a directory too'
                  return
              endif
          else
              let xdir=g:WinMergeDir
          endif
       endif
   endif

    let xdir=substitute(xdir, '\n','','ic')

    if cmd=='dos'
       let cmd="silent !start C:\\Windows\\system32\\cmd /c cmd"
       exe  cmd
    elseif cmd=='explorer'
        let tdir=system('cd')
        let vidir=$VIM
        let tdir=substitute(tdir, '\','\\', 'ic')
        let vidir=substitute(vidir, '\','\\','ic')
        let tdir='"' . tdir . '"'
        let cmds="silent !start " . vidir . "\\vim71\\bin\\a43.exe " .  tdir
        "echo cmds
        exe cmds
    elseif cmd=='init'
        let tdir=system('cd')
        let cmds='sil !cmd /c start c:\windows\system32\cscript.exe ' . $VIM . '\vim71\vbscript\selectpath.vbs ' . tdir
        exe cmds
    elseif cmd=='current'
        echo xdir
    elseif cmd=='touch'
        call s:STouch(xdir)
    elseif cmd=='dir'
       let ext=tolower(expand("%:e"))
       let fname=tolower(expand('%<'))
       let filename=fname . '.' . ext
       let tdir=substitute(system('cd'), '\n','','ic')
       let tdir='"' . tdir . '"'
       "let xdir='"' . xdir . '"'
       let vidir=$VIM
       let xdir=substitute(xdir, '\','\\', 'ic')
       let tdir=substitute(tdir, '\','\\', 'ic')
       let vidir=substitute(vidir, '\','\\','ic')
       "let cmd="silent !start " . vidir . "\\vim71\\bin\\winmerge.exe " .  tdir  . " " . xdir
       "let cmd=substitute(cmd, '\n','','ic')
       let cmds="silent !start " . vidir . "\\vim71\\bin\\winmerge.exe " .  tdir  . " " . xdir
       let cmds=substitute(cmds, '\n','','ic')
       exe cmds
    elseif cmd=='file'
       let ext=tolower(expand("%:e"))
       let fname=tolower(expand('%<'))
       let filename=fname . '.' . ext
       let tdir=substitute(system('cd'), '\n','','ic')
       let xdir=xdir
       let vidir=$VIM
       let xdir=substitute(xdir, '\','\\', 'ic')
       let tdir=substitute(tdir, '\','\\', 'ic')
       let tdir='"' . tdir . '"'
       let vidir=substitute(vidir, '\','\\','ic')
       let cmds="silent !start " . vidir . "\\vim71\\bin\\examdiff.exe " .  tdir  . " " . xdir . "\\" . filename . '"'
       let cmds=substitute(cmds, '\n','','ic')
       exe cmds

    elseif cmd=='split'
       let ext=tolower(expand("%:e"))
       let fname=tolower(expand('%<'))
       let filename=fname . '.' . ext
       let tdir=substitute(system('cd'), '\n','','ic')
       let xdir=xdir
       let vidir=$VIM
       let xdir=substitute(xdir, '\','\\', 'ic')
       let tdir=substitute(tdir, '\','\\', 'ic')
       let tdir='"' . tdir . '"'
       let vidir=substitute(vidir, '\','\\','ic')
       let cmds="silent sp " . xdir . "\\" . filename . '"'
       let cmds=substitute(cmds, '\n','','ic')
       let cmds=substitute(cmds, '"','','gc')
       exe cmds
    elseif cmd=='copy'

       let ext=tolower(expand("%:e"))
       let fname=tolower(expand('%<'))
       let filename=fname . '.' . ext

       let tdir=substitute(system('cd'), '\n','','ic')
       let xdir=xdir
       let vidir=$VIM
       let xdir=substitute(xdir, '\','\\', 'ic')
       let tdir=substitute(tdir, '\','\\', 'ic')
       let vidir=substitute(vidir, '\','\\','ic')
       let cmds="silent !start xcopy /y  " . '"' .  xdir . "\\" . filename .'"' . ' "' . tdir . "\\" . filename . '"'
       let cmds=substitute(cmds, '\n','','ic')
       exe cmds
       let cmds='e ' . filename
       exe 'e ' . filename
       exe cmds
       "echo cmd

    else
        echo 'Help:'
        echo 'C Run AdvCmmdNew Command'
        echo 'C dir <Dir>'
        echo 'C file <Dir>'
        echo 'C dos'
        echo 'C init'
        echo 'C copy'
        echo 'C split'
        return
    endif
    "echo xdir

endfunction

function! s:STouch(path)
"
        let path=a:path

        let cpath='dir /od/b ' . path
        let dircontent=substitute(system(cpath), '','','ic')
        let pdir=split(dircontent, '\n')

        let path=substitute(path, '"','','g')

        for var in pdir
          let object='"' . path . '\' . '"' . var . '"' . '"'
          let obj= path . '\' . var
          if isdirectory(obj)
               "echo 'Dir exists '  . object
               call s:STouch(object)
          else
               "echo 'File readable ' . object
               let vidir=$VIM
               let vidir=substitute(vidir, '\','\\','ic')
               let cmd="silent !start " . vidir . "\\vim71\\bin\\touch.exe " . obj
               "echo cmd
               exe cmd
         endif
        endfor

endfunction
""""""""""""""""""""""""""""""""""""""""""""""""""
"highlight Cursor   guifg=white  guibg=red
"highlight iCursor  guifg=white  guibg=green
"set guicursor=n-v-c:block-Cursor
"set guicursor+=i:ver100-iCursor
"set guicursor+=n-v-c:blinkon0-Cursor
"set guicursor+=i:blinkwait20-iCursor 
""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:CurPos(...)
  if a:0 > 0
    let b:saveve = &virtualedit
    let b:savesiso = &sidescrolloff
    set virtualedit=all
    set sidescrolloff=0
    let b:curline = a:1
    let b:curcol = a:2
    let b:curvcol = virtcol(".")
    let b:curwcol = wincol()
    normal g0miM
    let b:midline = line(".")
    call cursor(b:curline, b:curcol)
    let &virtualedit = b:saveve
    let &sidescrolloff = b:savesiso
  else
    set virtualedit=all
    set sidescrolloff=0
    call cursor(b:midline, 0)
    normal zz0
    let nw = wincol() - 1
    if b:curvcol != b:curwcol - nw
      normal `izs
      let s = wincol() - nw - 1
      if s != 0
        execute "normal ".s."zl"
      endif
    endif
    call cursor(b:curline, b:curcol)
    let &virtualedit = b:saveve
    let &sidescrolloff = b:savesiso
    unlet b:saveve b:savesiso b:curline b:curcol b:curvcol b:curwcol b:midline
  endif
endfunction 
""""""""""""""""""""""""""""""""""""""""""""""""""
fun! s:RectangleCopy(...)

   let x=0

   let x1=0
   let y1=0
   let x2=0
   let y2=0
   let maxlen=0

   if a:0 < 4
    echo "Example: Rectcp <StartLine> <StartCol> <EndLine> <EndCol>"
    return
   endif
   if  a:0 == 0
      let x=1
   elseif a:0 == 1
      let re=a:1
   else
       let re=''
       let k = 0
       while k < a:0
            let re = re . ' ' . a:000[k]
            if k==0
                let startl=a:000[k]
                "echo k . ' ' . startl
            elseif k==1
                let startc=a:000[k]-1
                "echo k . ' ' . startc
            elseif k==2
                let endl=a:000[k]+1
                "echo k . ' ' . endl
            elseif k==3
                let endc=a:000[k]
                "echo k . ' ' . endc
            endif

            let k=k+1
       endw
   endif
        "call cursor(endl,endc)
        "sleep 500m
        "s:CurPos(startl,startc)
        "s:CurPos(endl,endc)
        """""""""""""""""""""""""""""""""""""""""""""""""""""
        let sc=startc
        let ec=endc
        let counter1=endl-startl
        let xinit=0
        let yinit=0
        let g:CutList=[]

        while counter1
            "echo counter1
            let counter2=endc-startc
            while counter2
                if xinit==0
                    let @l=getline(startl)[startc]
                    let x=getline(startl)[startc]
                    let linestr=x
                    "let @b= '(' . startl . ',' . startc .')' . x
                    let startc=startc+1
                    let xinit=1
                else
                    let @L=getline(startl)[startc]
                    let x=getline(startl)[startc]
                    let linestr=linestr . x
                    "let @B= '(' . startl . ',' . startc .')' . x
                    let startc=startc+1
                endif
                let counter2=endc-startc
            endw

            call add(g:CutList,linestr)
            let linestr=''
            let @L= "\n"
            let startl=startl+1
            let counter1=endl-startl
            let startc=sc
            let endc=ec
       endw
        """""""""""""""""""""""""""""""""""""""""""""""""""""


     let @*=@l
     echo "Command executed successfully and result is stored in 'l' register user Ctrl-V to paste"

endfun


""""""""""""""""""""""
fun! s:RectPaste(...)

   let x=0

   let x1=0
   let y1=0
   let x2=0
   let y2=0
   let startc=0

   if a:0 > 2
    echo "Example: Pas <StartLine> <StartCol> or Pas <StartLine>"
    return
   endif
   " Extract variable arguments
   if  a:0 == 0
      let x=1
      echo "Example: Pas <StartLine> <StartCol> or Pas <StartLine>"
      return
   elseif a:0 == 1
      let re=a:1
      let startl=a:1
   else
       let re=''
       let k = 0
       while k < a:0
            let re = re . ' ' . a:000[k]
            if k==0
                let startl=a:000[k]
            elseif k==1
                let startc=a:000[k]
            endif

            let k=k+1
       endw
   endif

        let mrk=startl
        let xinit=0

   " Determine max length of string
        let maxlen=0
        for var in g:CutList
          let result_string=strpart(getline(startl),0)

          let lng=strlen(result_string)
          if (lng > maxlen)
            let maxlen=lng
          endif
          let startl=startl+1
        endfor

        let startl=mrk

   " Now paste it in each line
        for var in g:CutList
              "Stuff empty space to look more beautifull
              let result_string=strpart(getline(startl),0)
              if startc==0
                  let result_string=s:Stuff(result_string,1," ",maxlen+1)
              else
                  let result_string=s:Stuff(result_string,0," ",maxlen+1)
              endif
              let result_string=result_string . ' ' . var
              call setline(startl,result_string)
              let startl=startl+1
        endfor

        return
endfun

""""""""""""""""""""""
fun! s:Justify(...)

   let justification=0

   " Extract variable arguments
   if  a:0 == 0
      let x=1
   elseif a:0 == 1
      let re=a:1
      let justification=1
   else
       let re=''
       let k = 0
       while k < a:0
            let re = re . ' ' . a:000[k]
            if k==0
                let justification=1
            endif

            let k=k+1
       endw
   endif

   let str=s:TrimString(re)

   if str=="l" 
    justification=0
   else
    justification=1
   endif

        let xinit=0

        let startl=line("1")
        let end=line("$")
   " Determine max length of string
        let maxlen=0

        while (startl <= end)
            let len = strlen(getline(startl))

            if(len > maxlen)
                let maxlen=len
            endif

            let startl = startl + 1
        endwhile

        let startl=line("1")
        let end=line("$")

        while (startl <= end)

          let result_string=strpart(getline(startl),0)
          if justification==0
              let result_string=s:Stuff(result_string,1," ",maxlen+1)
          else
              let result_string=s:Stuff(result_string,0," ",maxlen+1)
          endif
          call setline(startl,result_string)
          let startl = startl + 1

        endwhile

        return
endfun


""""""""""""""""""""""""""""""""""""""""""""
fun! StartMarkForRectangleCopy()

    if g:marker==0 || g:marker==2
        let g:startl=line('.')
        let g:startc=col('.')-1
        "echo "Pressed ctrl-c now move curser and press ctrl-c for rectangle copy"
        echo "Pressed ctrl-c now move curser and press ctrl-c for rectangle copy position marked [" .  g:startl . " " . g:startc . "]"
        let g:marker=1
    elseif g:marker==1
        let g:endl=line('.')+1
        let g:marker=2
        let g:endc =col('.')

        let startc=g:startc
        let startl=g:startl
        let endc=g:endc
        let endl=g:endl

        let sc=startc
        let ec=endc
        let counter1=endl-startl
        let xinit=0
        let yinit=0
        let g:CutList=[]

        while counter1
            "echo counter1
            let counter2=endc-startc
            while counter2
                if xinit==0
                    let @l=getline(startl)[startc]
                    let x=getline(startl)[startc]
                    let linestr=x
                    "let @b= '(' . startl . ',' . startc .')' . x
                    let startc=startc+1
                    let xinit=1
                else
                    let @L=getline(startl)[startc]
                    let x=getline(startl)[startc]
                    let linestr=linestr . x
                    "let @B= '(' . startl . ',' . startc .')' . x
                    let startc=startc+1
                endif
                let counter2=endc-startc
            endw

                call add(g:CutList,linestr)
                let linestr=''
                let @L= "\n"
                let startl=startl+1
                let counter1=endl-startl
                let startc=sc
                let endc=ec
           endw

            let @*=@l
            "echo "Rectangle copy command executed successfully and result can be retrived using ctrl-v,ctrl-y for vertical paste"
            echo "command executed,retrive using ctrl-v,ctrl-y for vertical paste " .  "start [" .  g:startl . " " . g:startc . "]" . "end [" . g:endl . " " . g:endc . "]"
            let g:marker=2
    else
    endif
endfun
""""""""""""""""""""""""""""""""""""""""""""
fun! PasteMarkForRectangleCopy()

   if(g:marker==2)
   let x=0

   let x1=0
   let y1=0
   let x2=0
   let y2=0
   let startc=0
   let startl=line('.')


        let mrk=startl
        let xinit=0

   " Determine max length of string
        let maxlen=0
        for var in g:CutList
          let result_string=strpart(getline(startl),0)

          let lng=strlen(result_string)
          if (lng > maxlen)
            let maxlen=lng
          endif
          let startl=startl+1
        endfor

        let startl=mrk

   " Now paste it in each line
        for var in g:CutList
              "Stuff empty space to look more beautifull
              let result_string=strpart(getline(startl),0)
              if startc==0
                  let result_string=s:Stuff(result_string,1," ",maxlen+1)
              else
                  let result_string=s:Stuff(result_string,0," ",maxlen+1)
              endif
              let result_string=result_string . ' ' . var
              call setline(startl,result_string)
              let startl=startl+1
        endfor

        echo "paste of command performed"
      else

        echo "Please perform cut and paste step by step"
      endif

        return
endfun
""""""""""""""""""""""""""""""""""""""""""""
fun! FoldLines()

   if(g:FoldLines!=0)
        let startl=g:FoldLines
        let endl=line('.')
        if startl > endl
            exe(endl . ',' . startl . 'fo')
        else
            exe(startl . ',' . endl . 'fo')
        endif
        echo "Folding command executed successfully between lines " . startl . '  and ' . endl
        let g:FoldLines=0
    else
        let g:FoldLines=line('.')
        echo "Line marked " . g:FoldLines .  " for folding now move curser and press f9 again to fold "
    endif


endfun
""""""""""""""""""""""""""""""""""""""""""""
function Mynumber(arg)
    echo line(".") . " " . a:arg
endfunction
"1,5call Mynumber(getline("."))
""""""""""""""""""""""""""""""""""""""""""""
function Table(title, ...)
   echo a:title
   for s in a:000
     echo ' ' . s
   endfor
endfunction

function SelectProject(type)

   let x=a:type

   let choice = confirm("Select project", "&DHL\n&Fedex\nFedex &Pics\nDHL &HK\nPC &Build\n&Vitronic\n&Scripts Documentation\n&Lab Work", 3)

   if choice==0
       let choice=3
   endif

   if choice == 1
       let path=escape('d:\work\vitronic scripts\dhl', ' \')
       let path='d:\work\vitronic scripts\dhl'
       exe 'cd ' . path
       if x==1
           exe 'sil tabnew  ' . 'pgmenu.ini'
       else
           exe 'e ' . 'pgmenu.ini'
       endif
   elseif choice == 2
       let path=escape('d:\work\vitronic scripts\fedex', ' \')
       let path='d:\work\vitronic scripts\fedex'
       exe 'cd ' . path
       if x==1
           exe 'sil tabnew ' . 'pgmenu.ini'
       else
           exe 'e ' . 'pgmenu.ini'
       endif
   elseif choice == 3
       let path=escape('d:\work\vitronic scripts\picsscripts\vbsrc', ' \')
       let path='d:\work\vitronic scripts\picsscripts\vbsrc'
       exe 'cd ' . path
       if x==1
           exe 'sil tabnew ' . 'commandhandler.vbs'
       else
           exe 'e ' . 'commandhandler.vbs'
       endif
   elseif choice == 4
       "let path=escape('d:\work\vitronic scripts\comark_del', ' \')
       let path='d:\work\vitronic scripts\comark_del'
       exe 'cd ' . path
       if x==1
           exe 'sil tabnew ' . 'pgmenu.ini'
       else
           exe 'e ' . 'pgmenu.ini'
       endif
   elseif choice == 5
       let path=escape('d:\work\vitronic scripts\PcInstall', ' \')
       let path='d:\work\vitronic scripts\PcInstall'
       exe 'cd ' . path
       if x==1
           exe 'sil tabnew ' . 'fedex_pics.cfg'
       else
           exe 'e ' . 'fedex_pics.cfg'
       endif
   elseif choice == 6
        let $flist = 'd:\vitronic\scripts\gettunnelips.exe'
       if filereadable( $flist )
           let path='d:\vitronic\scripts'
       else
           let path='c:\vitronic\scripts'
       endif
       exe 'cd ' . path
       if x==1
           exe 'sil tabnew ' . 'rundaily.vbs'
       else
           exe 'e ' . 'rundaily.vbs'
       endif
   elseif choice == 7
       "let path=escape('d:\work\vitronic scripts\docs', ' \')
       let path='d:\work\"vitronic scripts"\docs'
       exe '!' . path . '\' . 'ScriptsDocumentation_ver1.01.doc'
   elseif choice == 8
       let path=escape('d:\labwork', ' \')
       let path='d:\labwork'
       exe 'cd ' . path
       if x==1
           exe 'sil tabnew ' . 'commandhandler.vbs'
       else
           exe 'e ' . 'commandhandler.vbs'
       endif
   else
       echo choice
       echo "No default project defined"
   endif
endfunction



function AddBuffer(FileExt, ...)

  let tdir=system('cd')
  let tdir=substitute(tdir, '\n','', 'g')
  let s=a:FileExt
  let s=tdir . '\*.' . s
  let cntdir=system('s')
   for s in a:000
     let s=substitute(s,'\n','', 'g')
     let s=tdir . '\*.' . s
     let cntdir=cntdir . system('s')
     echo cntdir
   endfor
   echo cntdir

endfunction


highlight Cursor guibg=Green guifg=NONE
highlight lCursor guibg=Cyan guifg=NONE
