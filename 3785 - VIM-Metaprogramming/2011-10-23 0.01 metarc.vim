"Meta VIMRC
new metarc.vim
?^"METASTART
,$ w! R.vim
new R.vim
silent! 0,$s/\vFunc\[(.+)\]\{(.+)\}/fun!\1\r\2\rendfun\r/g
silent! 0,$s/\vKey([^=]+)\=(.*)\{(.+)\}/\2noremap <\1> \3<CR>\r/g
w! R.vim
so R.vim
close
close
finish	
"METASTART
so $VIMRUNTIME/mswin.vim  
set nu ru ai ar sta cin noswf nowrap ts=4 gfn=Courier\ New:h9
syn on
Func[Test()]{return 3}
KeyC-F1={:so %}
KeyF2={o<C-R>="#include "}
KeyF3={0i//}
KeyF4={^xx}
KeyF5={yyp} 
KeyF9={:!g++ % -o %<.exe && %<.exe }
KeyF10={:!g++ % -o %<.exe && %<.exe < %<.in}

