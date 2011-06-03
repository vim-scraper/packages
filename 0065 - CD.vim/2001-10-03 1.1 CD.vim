fu! CD()
    let bufdir = substitute(bufname("%"),"[/\\\\][^/\\\\]\\{-}$","","")
    execute ":cd " . bufdir
endf
com! CD call CD ()
