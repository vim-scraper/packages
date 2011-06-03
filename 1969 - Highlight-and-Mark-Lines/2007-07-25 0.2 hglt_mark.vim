"Inspired from other scripters. Thanks.
"
"This utility was created because of needs as others. My job requires log
"mining. Sometimes I need to highlight specific lines and moving between 
"them for comparison. Most of my colleagues use UltraEdit which has this
"capability. 
"
"What does this utility do?
"It highlights and marks the line under cursor. Then you'll be able to move
"between these marks easily. Unfortunately it is just limited to 3 marks because 
"my lack of coding knowledge.
"
"Step 1 - Just highlight and mark a line with CTRL+F2, CTRL+F3 or CTRL+F4. 
"Step 2 - Then use F2, F3 and F4 to move between these marks. 
"Step 3 - Use SHIFT+CTRL+F2, SHIFT+CTRL+F3 and SHIFT+CTRL+F4 to reset the related mark and highlight.
"Step 4 - use cm in normal mode to reset all highlights and marks
"
"It is that simple.

"HIGHLIGHT AND MARK LINES AND MOVE BETWEEN THEM
"Highlight groups
highlight my1 guibg=maroon guifg=white
highlight my2 guibg=cyan guifg=navy
highlight my3 guibg=green guifg=white
highlight my4 guibg=red guifg=white

"clean up used marks
delmarks x y z

"highlight and mark current line
nmap <C-F2> :exe   'match my1 /\%' . line(".") . 'l.*/'<CR>\|:mark x<CR>
nmap <C-F3> :exe  '2match my2 /\%' . line(".") . 'l.*/'<CR>\|:mark y<CR>
nmap <C-F4> :exe  '3match my3 /\%' . line(".") . 'l.*/'<CR>\|:mark z<CR>

"Maps to jump to the marked lines 
nmap <F2> g'x
nmap <F3> g'y
nmap <F4> g'z

"clear marks and highlights one by one
nmap <C-S-F2>  :match\|:delmarks x<CR>
nmap <C-S-F3> :2match\|:delmarks y<CR>
nmap <C-S-F4> :3match\|:delmarks z<CR>

"clear all marks and highlights 
nmap cm :match\|:2match\|:3match\|:delmarks x y z<CR>

