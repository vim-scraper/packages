map  
map!  
set nocp tw=72 noet
syn on

" ~~ 
"      Man on cross [singing]: ``Always look on the bright side of
"                              life...'' [whistling]...
"                -- Monty Python::The life of Brian(``Erik Idle'')
" 
"     /||  Adam Seyfarth <http://members.home.net/adam.seyfarth/>  ||\
"    /«||  <mailto:cloud@users.sf.net>   <http://www.majik3d.org>  ||»\
"    \«||  <http://tuxaqfh.sf.net>        <http://tuxkart.sf.net>  ||»/
"     \||  <http://vim.sf.net>           <http://www.majik3d.org>  ||/

map <Leader>s :call AddSig()<CR>
func! AddSig()
     sp ~/.fortunes
     normal d/^-- $/
     exe 'norm Gp'
     wq
     exe 'norm Gop'
     exe 'norm Go'
     r ~/.sig
endfunc

map <Leader>r G?^-- $?2kdG:call AddSig()<CR>
autocmd FileType mail norm ggO
