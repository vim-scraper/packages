
" user filetype file
if exists("did_load_filetypes")
  finish
endif

augroup filetypedetect

au! BufRead,BufNewFile *.applescript,*.as	setf applescript
au! BufRead,BufNewFile *.plt,.gnuplot		setf gnuplot

augroup END

