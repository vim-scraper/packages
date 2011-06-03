" Vim syntax file for Tcl/tk language
" Language:	Tcl - extension togl
" Maintained:	SM Smithfield <m_smithfield@yahoo.com>
" Last Change:	1-11-07
" Filenames:    *.tcl
" Version:      0.1
" GetLatestVimScripts: 1766 1 :AutoInstall: syntax/tcl_togl.vim

syn keyword tkWidget contained togl skipwhite nextgroup=tkWidgetPredicate
syn keyword tkWidgetOpts contained createproc reshapeproc displayproc double depth depthsize privatecmap auxbuffers rgba redsize greensize bluesize alpha alphasize stencil stencilsize overlay stereo time sharelist sharecontext ident indirect pixelformat accum accumredsize accumgreensize accumbluesize accumalphasize setgrid
syn keyword tclSecondary contained postredisplay swapbuffers
