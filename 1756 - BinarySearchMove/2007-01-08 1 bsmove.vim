
" Move a cursor with binary search principle - each step is twice shorter than previous
" left/right movement are bound to H and L
" up/down to <Leader>u and <Leader>d 


" minimal length of binary search step, if step is less that the threshold, we move only at one char or line
let g:bsmovethre = 2 

" mapping
map H :call BSMoveLineGoLeft()<CR>
map L :call BSMoveLineGoRight()<CR>
map <Leader>u :call BSMoveBufGoUp()<CR>
map <Leader>d :call BSMoveBufGoDown()<CR>



function! BSMoveLineCenter()
	call cursor(0, b:bscolmin + (b:bscolmax - b:bscolmin) / 2)
	let b:bscolcur = col(".")
endfunc

function! BSMoveLineGoLeft()
	let pos = col(".")

	if !exists("b:bscolcur") || b:bscolcur != pos
		let b:bscolmin = 1
	endif

	let b:bscolmax = pos
	if (b:bscolmax - b:bscolmin) > g:bsmovethre
		call BSMoveLineCenter()
	else
		execute "normal h"
		let b:bscolcur = col(".")
	endif
endfunc

function! BSMoveLineGoRight()
	let pos = col(".")

	if !exists("b:bscolcur") || b:bscolcur != pos
		let b:bscolmax = col("$")
	endif

	let b:bscolmin = pos
	if (b:bscolmax - b:bscolmin) > g:bsmovethre
		call BSMoveLineCenter()
	else
		execute "normal l"
		let b:bscolcur = col(".")
	endif
endfunc


function! BSMoveBufCenter()
	call cursor(b:bslinemin + (b:bslinemax - b:bslinemin) / 2, 0)
	let b:bslinecur = line(".")
endfunc

function! BSMoveBufGoUp()
	let pos = line(".")

	if !exists("b:bslinecur") || b:bslinecur != pos
		let b:bslinemin = 1
	endif

	let b:bslinemax = pos
	if (b:bslinemax - b:bslinemin) > g:bsmovethre
		call BSMoveBufCenter()
	else
		execute "normal k"
		let b:bslinecur = line(".")
	endif
endfunc

function! BSMoveBufGoDown()
	let pos = line(".")

	if !exists("b:bslinecur") || b:bslinecur != pos
		let b:bslinemax = line("$")
	endif

	let b:bslinemin = pos
	if (b:bslinemax - b:bslinemin) > g:bsmovethre
		call BSMoveBufCenter()
	else
		execute "normal j"
		let b:bslinecur = line(".")
	endif
endfunc

