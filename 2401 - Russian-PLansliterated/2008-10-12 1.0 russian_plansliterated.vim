" ---------------------------------------------------------------------
" russian_plansliterated.vim
"  Author:	Krzysztof Goj
"  Date:	24 Sep 2008
"  About:
"  This plug-in allows you to write transliterated Russian (Cyrillic alphabet)
"  in insert and replace mode. Other modes are unaffected and just work (which
"  makes it superior to relying on GTK's input methods).
"
"  Transliteration is heavily biased towards Polish orthography and phonetics,
"  eg. "ч" is spelled as "cz" (not "ch"), "Я" is "Ja", not 'Ya', etc. This is
"  why it's called PLansliterated :) It's quite trivial to turn this plug-in
"  into english transliteration, CZransliteration, FRansliteration, etc. If
"  you do so for your 'source' language of choice, feel free to post your work
"  on vim.org or just drop me a line.
"
"  Usage:
"  Turn transliteration on and off by :RussianOn, :RussianOff or :ToggleRussian
"  commands. You can also use CTRL+q (both in insert and normal mode) for
"  toggling.
"
"  Most of characters should be quite intuitive (at least if you're Polish).
"  I decided to make both "e" and "je" represent Cyrillic "е".
"  Use "e'" to get 'э'. "'" stands for "ь" and "`" is for "ъ". Prefix with
"  tilde to get uppercase variants of твердый and мягкий знак, eg. "~`" for "Ъ".
"  
"  If you need several Latin letters for one uppercase Cyrillic, you can use
"  both all-uppercase, or only the first one capitalised, like "SZCZ" or "Szcz" for "Щ".
"
" Set g:cursor_follows_alphabet to make cursor color change when alphabet is
" switched.
"
"  Use the source when in doubt.
"
"  Blablabla:
"  Have fun using this plug-in!
"  Comments are welcome.
"  Пака!

if &cp || exists("g:loaded_russian_plansliterated")
 if &verbose
  echo "russian_plansliterated is not vi-compatible; not loaded (you need to set nocp)"
 endif
 finish
endif

let g:is_russian_on=0
let g:loaded_russian_plansliterated=1

if !exists("g:cursor_follows_alphabet")
let g:cursor_follows_alphabet=0
endif

command! RussianOn call RussianOn()
command! RussianOff call RussianOff()
command! ToggleRussian call ToggleRussian()
inoremap <C-q> <C-r>=ToggleRussian()<CR>
nnoremap <C-q> :ToggleRussian<CR>


function! ToggleRussian()
    if g:is_russian_on
        RussianOff
    else
        RussianOn
    endif
    return ''
endfunction

function! RussianOn()
    let g:is_russian_on=1
    if g:cursor_follows_alphabet
        hi Cursor guibg=red
    endif
    inoremap A А
    inoremap a а
    inoremap B Б
    inoremap b б
    inoremap W В
    inoremap w в
    inoremap V В
    inoremap v в
    inoremap G Г
    inoremap g г
    inoremap D Д
    inoremap d д
    inoremap E Е
    inoremap e е
    inoremap Je Е
    inoremap JE Е
    inoremap je е
    inoremap Jo Ё
    inoremap JO Ё
    inoremap jo ё
    inoremap Z З
    inoremap z з
    inoremap Ż Ж
    inoremap ż ж
    inoremap E' З
    inoremap e' з
    inoremap I И
    inoremap i и
    inoremap J Й
    inoremap j й
    inoremap K К
    inoremap k к
    inoremap L Л
    inoremap l л
    inoremap Ł Л
    inoremap ł л
    inoremap M М
    inoremap m м
    inoremap N Н
    inoremap n н
    inoremap O О
    inoremap o о
    inoremap P П
    inoremap p п
    inoremap R Р
    inoremap r р
    inoremap S С
    inoremap s с
    inoremap T Т
    inoremap t т
    inoremap U У
    inoremap u у
    inoremap F Ф
    inoremap f ф
    inoremap H Х
    inoremap h х
    inoremap X Х
    inoremap x х
    inoremap C Ц
    inoremap c ц
    inoremap Cz Ч
    inoremap CZ Ч
    inoremap cz ч
    inoremap Sz Ш
    inoremap SZ Ш
    inoremap sz ш
    inoremap SZCZ Щ
    inoremap Szcz Щ
    inoremap szcz щ
    inoremap ~` Ъ
    inoremap ` ъ
    inoremap Y Ы
    inoremap y ы
    inoremap ~' Ь
    inoremap ' ь
    inoremap E' Э
    inoremap e' э
    inoremap JU Ю
    inoremap Ju Ю
    inoremap ju ю
    inoremap JA Я
    inoremap Ja Я
    inoremap ja я
endfunction

function! RussianOff()
    let g:is_russian_on=0
    if g:cursor_follows_alphabet
        hi Cursor guibg=green
    endif
    silent! iunmap A
    silent! iunmap a
    silent! iunmap B
    silent! iunmap b
    silent! iunmap W
    silent! iunmap w
    silent! iunmap V
    silent! iunmap v
    silent! iunmap G
    silent! iunmap g
    silent! iunmap D
    silent! iunmap d
    silent! iunmap E
    silent! iunmap e
    silent! iunmap JO
    silent! iunmap Jo
    silent! iunmap jo
    silent! iunmap JE
    silent! iunmap Je
    silent! iunmap je
    silent! iunmap Z
    silent! iunmap z
    silent! iunmap Ż
    silent! iunmap ż
    silent! iunmap E'
    silent! iunmap e'
    silent! iunmap I
    silent! iunmap i
    silent! iunmap J
    silent! iunmap j
    silent! iunmap K
    silent! iunmap k
    silent! iunmap L
    silent! iunmap l
    silent! iunmap Ł
    silent! iunmap ł
    silent! iunmap M
    silent! iunmap m
    silent! iunmap N
    silent! iunmap n
    silent! iunmap O
    silent! iunmap o
    silent! iunmap P
    silent! iunmap p
    silent! iunmap R
    silent! iunmap r
    silent! iunmap S
    silent! iunmap s
    silent! iunmap T
    silent! iunmap t
    silent! iunmap U
    silent! iunmap u
    silent! iunmap F
    silent! iunmap f
    silent! iunmap H
    silent! iunmap h
    silent! iunmap X
    silent! iunmap x
    silent! iunmap C
    silent! iunmap c
    silent! iunmap CZ
    silent! iunmap Cz
    silent! iunmap cz
    silent! iunmap Sz
    silent! iunmap SZ
    silent! iunmap sz
    silent! iunmap SZCZ
    silent! iunmap Szcz
    silent! iunmap szcz
    silent! iunmap ~`
    silent! iunmap `
    silent! iunmap Y
    silent! iunmap y
    silent! iunmap ~'
    silent! iunmap '
    silent! iunmap E'
    silent! iunmap e'
    silent! iunmap JU
    silent! iunmap Ju
    silent! iunmap ju
    silent! iunmap JA
    silent! iunmap Ja
    silent! iunmap ja
endfunction
