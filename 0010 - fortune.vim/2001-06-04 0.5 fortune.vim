" ZZ..................Add a fortune and advance to the next one then quit editor
" :sp ~/.fortunes<CR>.open a window on ~/.fortunes
" d/^--/<CR>..........delete until the next line starting with "--"
" Gp..................Go to the end and put the just deleted text there
" :wq<CR>.............Write the ~/.fortunes file and close the window
" GA<CR><Esc>.........go to the end Add an empty line
" p...................put the fortune text
" :x<CR>..............quit the editor

map ZZ :sp ~/.fortunes<CR>d/^--/<CR>Gp:wq<CR>GA<CR><Esc>p:x<CR>
