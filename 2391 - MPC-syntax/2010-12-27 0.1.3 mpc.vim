" Vim syntax file
" Language:     mpc (Makefile, Project, and Workspace Creator)
" Maintainer:   Like Ma <LikeMartnMa@gmail.com>
" Last Change:  2010 Dec 28
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

syn keyword mpcStructure	expand feature workspace project verbatim specific conditional Source_Files Header_Files Inline_Files Template_Files Documentation_Files Resource_Files Define_Custom static IDL_Files CIDL_Files Pkgconfig_Files
syn keyword mpcKeyword		after automatic automatic_in automatic_out avoids command commandflags custom_only dependent dllout documentation_outputext documentation_pre_dirname documentation_pre_extension documentation_pre_filename dynamicflags exename exeout generic_outputext generic_pre_dirname generic_pre_extension generic_pre_filename header_outputext header_pre_dirname header_pre_extension header_pre_filename includes inline_outputext inline_pre_dirname inline_pre_extension inline_pre_filename inputext install libout libpath libpaths libs lit_libs macros managed no_pch output_follows_input output_option pch_header pch_postrule pch_source postbuid postbuild postclean postcommand prebuild pre_dirname pre_extension pre_filename pure_libs recurse recursive_includes recursive_libpaths requires resource_outputext resource_pre_dirname resource_pre_extension resource_pre_filename sharedname source_outputext source_pre_dirname source_pre_extension source_pre_filename staticflags staticname tagchecks tagname template_outputext template_pre_dirname template_pre_extension template_pre_filename version webapp
syn match mpcComment		"//.*"

hi def link mpcComment		Comment
hi def link mpcStructure	Structure
hi def link mpcKeyword		Keyword

let b:current_syntax = "mpc"

let &cpo = s:cpo_save
unlet s:cpo_save
