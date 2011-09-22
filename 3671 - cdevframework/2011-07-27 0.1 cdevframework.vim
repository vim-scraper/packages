
set sessionoptions+=globals

map <C-W>s :call OpenFile()<CR>

func! OpenFile()
    if !exists("g:BasePathSrc")
        let g:SrcExt = input("Source extension? ", ".cc")
        let g:HdrExt = input("Header extension? ", ".h")
        let g:BasePathSrc = input("Basepath for " . g:SrcExt . "? ", "", "file")
        let g:BasePathHdr = input("Basepath for " . g:HdrExt . "? ", "", "file")
    endif
    let fname = input("File? ", "", "file")
    exe "norm \<C-W>b\<C-W>_"
    exec "sp"
    exec "e " . g:BasePathSrc . "/" . fname . g:SrcExt
    exec "vsp " . g:BasePathHdr . "/" . fname . g:HdrExt
    exe "norm \<C-W>_"
endfunc
