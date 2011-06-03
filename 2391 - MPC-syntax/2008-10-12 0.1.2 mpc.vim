" Vim syntax file
" Language:     mpc (Makefile, Project, and Workspace Creator)
" Maintainer:   Like Ma <LikeMartnMa@gmail.com>
" Last Change:  2008 Oct 4
" Summary: Syntax Highlight for MakeProjectCreator files
" Description: Syntax Highlight for Makefile, Project, and Workspace Creator (http://www.ociweb.com/products/mpc).
"
" Install:
"   Put it into syntax directory and add mpc filetype 
"   au BufNewFile,BufRead *.m\(wb\|wc\|pb\|pc\) setf mpc
"   in your filetype.vim
"
" Changes:
"   0.1
"       Only supports types of .mwc, .mwb, .mpc and.mpb;
"

if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

syn keyword mpcStructure	feature workspace project verbatim specific conditional Source_Files Header_Files Inline_Files Template_Files Documentation_Files Resource_Files Define_Custom static IDL_Files CIDL_Files Pkgconfig_Files
syn keyword mpcKeyword		after automatic avoids command commandflags custom_only dependent dynamicflags dllout exename includes inline_pre_extension inputext install libout keyword libpaths libs lit_libs header_pre_extension macros output_option pch_header pch_postrule pch_source pre_extension postcommand pure_libs postbuid recurse requires sharedname source_pre_extension staticflags staticname tagchecks tagname template_pre_extension version 
syn match mpcComment		"//.*"

hi def link mpcComment		Comment
hi def link mpcStructure	Structure
hi def link mpcKeyword		Keyword

let b:current_syntax = "mpc"

let &cpo = s:cpo_save
unlet s:cpo_save
