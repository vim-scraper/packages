""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" keywords.vim -- functions to move and quote keyword text
" 
" Author: Anders Thøgersen
" Email: NaOnSdPeArMslt@gmail.com -- remove the capitol letters
" Last Change: 28-May-2005
" Version:     0.7
"
" Licence: This program is public domain; 
"
" Download From:
" http://www.vim.org/scripts/script.php?script_id=1292
" 
" Description:
" 
" This scripts provides two functionalities that use the iskeyword setting of
" the current buffer.
" 
" The following mappings are provided for moving keywords in normal mode:
"
"     <C-Up>   : exchange the current keyword with the previous one.
"     <C-Down> : exchange the current keyword with the next one.
"
" These mappings can be changed by changing the value of the following variables. 
"
"     let g:keywords_MoveWordL = '<C-Up>'
"     let g:keywords_MoveWordR = '<C-Down>'
"
" Also several mappings for adding/deleting "quotes" to/from a keyword or a
" visually selected area. The mappings are as follows:
"
"    qw, q"  : double quote        
"    qs, q'  : single quote      
"    qe, q`  : quote execute      
"    q[, q]  : quote with square brackets      
"    q{, q}  : quote with brackets
"    q(, q)  : quote with parantheses      
"    q<, q>  : quote with xml style tag.
"
" If you wish to provide a different map leader than 'q' use this variable:
"    
"    let g:keywords_MapLeader = 'q'
"
" There is one normal mode mapping for removing quotes from a keyword:
"
"    wq  :  removes non iskeyword material around the current keyword.
" 
" Consider sending an email if you have suggestions or comments :-)
" 	

if exists('loaded_keywords_script') || &cp
  finish
endif
let loaded_keywords_script = 1

if !exists("g:keywords_MoveWordL")
	let g:keywords_MoveWordR = '<C-Up>'
endif
if !exists("g:keywords_MoveWordL")
	let g:keywords_MoveWordL = '<C-Down>'
endif
if !exists("g:keywords_MapLeader")
	let g:keywords_MapLeader = 'q'
endif

let b:keywords_isk       = 'a-zA-Z0-9_'

" {{{1 Quote(l, r) : quote a word konsisting of letters from iskeyword
fun! <SID>KeywordsQuote(lquote, rquote)
	normal mz
	exe 's/\(\k*\%#\k*\)/' . a:lquote . '\1' . a:rquote . '/'
	normal `zl
endfun

" {{{1 QuoteVisual(l, r)
function! <SID>KeywordsQuoteVisual(lquote, rquote)
	let save = @"

	silent normal gvy
	let @" = a:lquote . @" . a:rquote
	silent normal gvp

	let @" = save
endfunction

" {{{1 UnQuote 
fun! <SID>KeywordsUnQuote()
	normal mz
	exe 's/[^' . b:keywords_isk . ']\(\k*\%#\k*\)[^' . b:keywords_isk . ']/\1/'
	normal `zh
endfun

" {{{1 IskeywordChars : output a regex for matching a keyword
fun! <SID>KeywordsIskeywordChars()
	let isk   = escape(&iskeyword, "^$.\\/")
	let chars = substitute(isk, '\(\d\+\)-\(\d\+\)', '\=nr2char(submatch(1))."-".nr2char(submatch(2))', 'g')
	let chars = substitute(chars, ",-,", ',', "g") " cheat!
	let chars = substitute(chars, ",a-z,", ',', "g") " cheat!
	let chars = substitute(chars, ",A-Z,", ',', "g") " cheat!
	let chars = substitute(chars, ",", "", "g") . 'a-zA-Z'
	return chars
endfun

" {{{1 MoveWordsSetup augroup
augroup KeywordsMoveWordsSetup
	au!
	autocmd BufReadPost * if &modifiable 
	autocmd BufReadPost * let b:keywords_isk = <SID>KeywordsIskeywordChars()
	autocmd BufReadPost * exe 'nnoremap <silent> <buffer> '. g:keywords_MoveWordR .' "_yiw?\k\+\_[^'. b:keywords_isk .']\+\%#<CR>:s/\(\%#\k\+\)\(\_[^'. b:keywords_isk .']\+\)\(\k\+\)/\3\2\1/e<CR><C-o>'
	autocmd BufReadPost * exe 'nnoremap <silent> <buffer> '. g:keywords_MoveWordL .' "_yiw:s/\(\%#\k\+\)\(\_[^'. b:keywords_isk .']\+\)\(\k\+\)/\3\2\1/e<CR><C-o>/\k\+\_[^'. b:keywords_isk .']\+<CR>'
	autocmd BufReadPost * endif
augroup END

" {{{1 Default mappings
" KeywordsAddMap(key, left, right) : Add mappings for visual and normal modes
fun! <SID>KeywordsAddMap(key, left, right)
	let lmapping = 'noremap <silent> <unique> ' . g:keywords_MapLeader . a:key . ' :call <SID>' 
	let rmapping = '("' . a:left . '", "' . a:right . '")<cr>'
	exe 'n' . lmapping . 'KeywordsQuote'       . rmapping
	exe 'v' . lmapping . 'KeywordsQuoteVisual' . rmapping
endfun

" {{{1 The mappings
call <SID>KeywordsAddMap('w', "\\\"", "\\\"")
call <SID>KeywordsAddMap('s',  "'", "'")
call <SID>KeywordsAddMap('e',  "`", "`") " quote execute
call <SID>KeywordsAddMap('m',  "`", "'") " m4 quotes

call <SID>KeywordsAddMap("'"    , "'"    , "'")
call <SID>KeywordsAddMap('\\\"' , "\\\"" , "\\\"")
call <SID>KeywordsAddMap('`'    , "`"    , "`")

call <SID>KeywordsAddMap(']',  "[", "]")
call <SID>KeywordsAddMap('}',  "(", ")")
call <SID>KeywordsAddMap(')',  "{", "}")
call <SID>KeywordsAddMap('>',  "<", ">")

call <SID>KeywordsAddMap('[',  "[", "]")
call <SID>KeywordsAddMap('{',  "(", ")")
call <SID>KeywordsAddMap('(',  "{", "}") 
call <SID>KeywordsAddMap('\<',  "\<", "\>") " for tags

nnoremap <silent> wq :call <SID>KeywordsUnQuote()<CR>

