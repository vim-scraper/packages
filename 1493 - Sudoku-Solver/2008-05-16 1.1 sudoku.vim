" ==============================================================
" Sudoku solver plugin
" Version: 1.1
" License: Vim license. See :help license
" Language: Vim script
" Maintainer: Po Shan Cheah <morton@mortonfox.com>
" Last updated: May 16, 2008
"
" GetLatestVimScripts: 1493 1 sudoku.vim
" ==============================================================
" 
" Sudoku is a puzzle where you have to fill all the blank spaces with
" digits from 1 to 9 such that no row, column, or 3x3 block of cells have
" any digits repeated.
" 
" Enter the puzzle into a buffer like this:
" 
"     8xx 69x xx2
"     91x xxx xxx
"     5xx xx8 xx7
" 
"     xxx 2x9 xx6
"     xxx 8xx xx3
"     2xx 3x4 xxx
" 
"     3xx 78x xx9
"     xxx xxx xx5
"     4xx x5x x28
"
" Then visually select the puzzle and invoke the macro binding ,s

" Vim 7.0 required
if version < 700
    finish
endif

if exists("loaded_sudoku")
    finish
endif
let loaded_sudoku = 1

let s:save_cpo = &cpoptions
set cpoptions&vim

let s:sudoku_winname = "Sudoku_".localtime()

" Switch to the Sudoku window if there is already one or open a new window for
" Sudoku.
function! s:sudoku_win()
    let sudoku_bufnr = bufwinnr('^'.s:sudoku_winname.'$')
    if sudoku_bufnr > 0
	execute sudoku_bufnr . "wincmd w"
    else
	execute "new " . s:sudoku_winname
	setlocal noswapfile
	setlocal buftype=nofile
	setlocal bufhidden=delete 
	setlocal foldcolumn=0
	setlocal nobuflisted
	setlocal nospell
    endif
endfunction

" Open a special window for Sudoku solver or reuse the existing Sudoku
" window.
function! s:sudoku_newwin()
    call s:sudoku_win()

    " Clear the entire buffer.
    " Need to use 'silent' or a 'No lines in buffer' message will appear.
    " Delete to the blackhole register "_ so that we don't affect registers.
    silent %delete _
endfunction

" Append text to the Sudoku window.
function! s:sudoku_addline(text)
    call s:sudoku_win()
    call append(line('$'), a:text)
    normal G
endfunction

" Display the board, representing blank spaces as underscores.
function! s:Print_board(board)
    for i in range(0, 8)
	call s:sudoku_addline(join(map(copy(a:board[i]), 'v:val == 0 ? "_" : v:val'), " "))
    endfor
    redraw
endfunction

" Display an error message with error highlighting.
function! s:Error_msg(msg)
    redraw
    echohl ErrorMsg
    echomsg a:msg
    echohl None
endfunction

" Return a list of numbers that could go into square row, col on the board.
function! s:Get_possible(board, row, col)
    let used = {}

    " Check row and column.
    for i in range(0, 8)
	let used[a:board[a:row][i]] = 1
	let used[a:board[i][a:col]] = 1
    endfor

    let blockrow = a:row - a:row % 3
    let blockcol = a:col - a:col % 3

    " Check the 3x3 block containing this square.
    for i in range(blockrow, blockrow+2)
	for item in a:board[i][blockcol : blockcol+2]
	    let used[item] = 1
	endfor
    endfor

    let possible = []
    for i in range(1, 9)
	if !has_key(used, i)
	    let possible += [i]
	endif
    endfor
    return possible
endfunction

" Check the board to see if a solution is possible at all. This function
" looks for obvious problems where the same number occurs in the puzzle in
" a row, column, or block.
function! s:Check_board(board)
    " Check rows.
    for i in range(0, 8)
	let used = {}
	for j in range(0, 8)
	    if a:board[i][j] && has_key(used, a:board[i][j])
		call s:Error_msg("Duplicate number ".a:board[i][j]." in row ".(i+1))
		return 0
	    endif
	    let used[a:board[i][j]] = 1
	endfor
    endfor

    " Check columns.
    for j in range(0, 8)
	let used = {}
	for i in range(0, 8)
	    if a:board[i][j] && has_key(used, a:board[i][j])
		call s:Error_msg("Duplicate number ".a:board[i][j]." in column ".(j+1))
		return 0
	    endif
	    let used[a:board[i][j]] = 1
	endfor
    endfor

    " Check blocks.
    for bi in range(0, 2)
	for bj in range(0, 2)
	    let used = {}
	    for i in range(bi * 3, bi * 3 + 2)
		for j in range(bj * 3, bj * 3 + 2)
		    if a:board[i][j] && has_key(used, a:board[i][j])
			call s:Error_msg("Duplicate number ".a:board[i][j]." in block ".(bi+1).",".(bj+1))
			return 0
		    endif
		    let used[a:board[i][j]] = 1
		endfor
	    endfor
	endfor
    endfor

    return 1
endfunction

" Recursive function to search for a solution by exhaustive search.
function! s:Try_board(board, row, col)
    if a:row > 8 || a:col > 8
	call s:sudoku_addline("")
	call s:sudoku_addline("Success")
	call s:Print_board(a:board)
	return 1
    endif

    let s:nodecount += 1

    " Advance to next row, col with wraparound.
    let nextrow = a:row
    let nextcol = a:col + 1
    if nextcol > 8
	let nextrow = a:row + 1
	let nextcol = 0
    endif

    " Skip over squares that already have numbers.
    if a:board[a:row][a:col] != 0
	return s:Try_board(a:board, nextrow, nextcol)
    endif

    for cell in s:Get_possible(a:board, a:row, a:col)
	let a:board[a:row][a:col] = cell
	if s:Try_board(a:board, nextrow, nextcol)
	    return 1
	endif
    endfor
    let a:board[a:row][a:col] = 0
endfunction

" Parse board info from the visual selection.
function! s:Parse_board(firstln, lastln)
    let rowcount = 0
    let board = []
    for lineno in range(a:firstln, a:lastln)
	" Strip blanks from the line.
	let boardline = substitute(getline(lineno), '\s\+', '', 'g')
	if strlen(boardline) > 0
	    if strlen(boardline) < 9
		call s:Error_msg("Line " . lineno . " '" . boardline . "' is too short.")
		return []
	    endif
	    let rowcount += 1
	    " Get the first 9 characters on the line.
	    " Split into a list of characters.
	    " Convert to numbers.
	    let board += [map(split(strpart(boardline, 0, 9), '\zs'), 'v:val + 0')]
	endif
    endfor
    if rowcount < 9
	call s:Error_msg("Not enough rows. Only " . rowcount . " rows found.")
	return []
    endif
    return board
endfunction

" Sudoku solver main function.
function! s:Sudoku_solver() range
    " Parse the input.
    let board = s:Parse_board(a:firstline, a:lastline)
    if board == []
	return
    endif

    " Check the puzzle board for obvious problems.
    if s:Check_board(board) == 0
	return
    endif

    " Open a window split to display the results.
    call s:sudoku_newwin()
    call s:sudoku_addline("Puzzle to be solved:")
    call s:Print_board(board)

    redraw
    echo "Solving Sudoku puzzle..."

    let s:nodecount = 0
    let result = s:Try_board(board, 0, 0)
    if result == 0
	call s:sudoku_addline("No solution")
    endif

    call s:sudoku_addline("")
    call s:sudoku_addline(s:nodecount." nodes examined")

    redraw
    echo "Sudoku solver finished."
endfunction

if !hasmapto('<Plug>SudokuSolver', 'v')
    vmap <unique> ,s <Plug>SudokuSolver
endif
vnoremap <unique> <script> <Plug>SudokuSolver  <SID>SudokuSolver

vnoremap <SID>SudokuSolver :call <SID>Sudoku_solver()<cr>

let &cpoptions = s:save_cpo

" vim:fo=cqro tw=75 com=\:\" sw=4
