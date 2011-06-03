" Vim filetype plugin file
" Language:     Falcon
" Author:       Alexander FÃ¦rÃ¸y <oliver.steven@gmail.com>
" Copyright:    Copyright (c) 2009 Steven Oliver
" License:      You may redistribute this under the same terms as Vim itself

if &compatible || v:version < 700
    finish
endif

au BufNewFile,BufRead *.fal set filetype=falcon

" vim: set et ts=4 :

