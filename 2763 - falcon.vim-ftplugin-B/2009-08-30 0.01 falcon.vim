" Vim filetype plugin file
" Language:     Falcon
" Author:       Steven Oliver <oliver.steven@gmail.com>
" Copyright:    Copyright (c) 2009 Steven Oliver
" License:      You may redistribute this under the same terms as Vim itself

if &compatible || v:version < 700
    finish
endif

runtime! ftplugin/sh.vim

setlocal tabstop=4
setlocal shiftwidth=4
setlocal expandtab
setlocal fileencoding=utf-8

" vim: set et ts=4 :

