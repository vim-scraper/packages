" Sudoku solver plugin
" Last Change: March 13, 2006
" Maintainer: Po Shan Cheah (vim@cheah.endjunk.com)
" Version: 1.0
" 
" This is a puzzle where you have to fill all the blank spaces with digits
" from 1 to 9 such that no row, column, or 3x3 block of cells have any
" digits repeated.
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

" Display the board, representing blank spaces as underscores.
function! <SID>Print_board(board)
    for i in range(0, 8)
	echo join(map(copy(a:board[i]), 'v:val == 0 ? "_" : v:val'), " ")
    endfor
endfunction

" Return a list of numbers that could go into square row, col on the board.
function! <SID>Get_possible(board, row, col)
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

" Recursive function to search for a solution by exhaustive search.
function! <SID>Try_board(board, row, col)
    if a:row > 8 || a:col > 8
	echo "Success"
	call <SID>Print_board(a:board)
	return 1
    endif

    let s:nodecount += 1

    let nextrow = a:row
    let nextcol = a:col + 1
    if nextcol > 8
	let nextrow = a:row + 1
	let nextcol = 0
    endif

    " Skip over squares that already have numbers.
    if a:board[a:row][a:col] != 0
	return <SID>Try_board(a:board, nextrow, nextcol)
    endif

    for cell in <SID>Get_possible(a:board, a:row, a:col)
	let a:board[a:row][a:col] = cell
	if <SID>Try_board(a:board, nextrow, nextcol)
	    return 1
	endif
    endfor
    let a:board[a:row][a:col] = 0
endfunction

" Parse board info from the visual selection.
function! <SID>Parse_board(firstln, lastln)
    let rowcount = 0
    let board = []
    for lineno in range(a:firstln, a:lastln)
	" Strip blanks from the line.
	let boardline = substitute(getline(lineno), '\s\+', '', 'g')
	if strlen(boardline) > 0
	    if strlen(boardline) < 9
		echoerr "Line " . lineno . " '" . boardline . "' is too short."
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
	echoerr "Not enough rows. Only " . rowcount . " rows found."
	return []
    endif
    return board
endfunction

" Sudoku solver main function.
function! <SID>Sudoku_solver() range
    let board = <SID>Parse_board(a:firstline, a:lastline)
    if board == []
	return
    endif
    echo "Puzzle to be solved:"
    call <SID>Print_board(board)
    let s:nodecount = 0
    call <SID>Try_board(board, 0, 0)
    echo s:nodecount . " nodes examined"
endfunction

if !hasmapto('<Plug>SudokuSolver', 'v')
    vmap <unique> ,s <Plug>SudokuSolver
endif
vnoremap <unique> <script> <Plug>SudokuSolver  <SID>SudokuSolver

vnoremap <SID>SudokuSolver :call <SID>Sudoku_solver()<cr>

let &cpoptions = s:save_cpo

" vim:fo=cqro tw=75 com=\:\" sw=4
