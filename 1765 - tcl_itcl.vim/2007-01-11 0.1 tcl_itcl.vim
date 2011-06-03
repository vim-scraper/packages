" Vim syntax file for Tcl/tk language
" Language:	Tcl - extension itcl
" Maintained:	SM Smithfield <m_smithfield@yahoo.com>
" Last Change:	11-29-06
" Filenames:    *.tcl
" Version:      0.1
" GetLatestVimScripts: xxxx 1 :AutoInstall: tcl_itcl.vim


syn keyword tclPrimary contained class skipwhite nextgroup=tclProcDef
syn keyword tclKeyword contained public private protected delete
syn keyword tclPrimary contained method
" variable
syn keyword tclPrimary contained constructor skipwhite nextgroup=tclProcArgs
syn keyword tclPrimary contained destructor skipwhite nextgroup=tclFoldBraces
syn keyword tclPrimary contained body skipwhite nextgroup=tclProcName
