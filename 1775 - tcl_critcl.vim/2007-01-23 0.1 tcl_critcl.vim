" Vim syntax file for Tcl/tk language
" Language:	Tcl - extension critcl
" Maintained:	SM Smithfield <m_smithfield@yahoo.com>
" Last Change:	11-13-06
" Filenames:    *.tcl
" Version:      0.1
" GetLatestVimScripts: xxxx 1 :AutoInstall: tcl_critcl.vim

syn match tclNamespace contained "\(::\)\=critcl::" nextgroup=tclCritclCmd
hi link tclCritclCmd tclKeyword

syn keyword tclCritclCmd contained cinit ccode cdata cproc ccommand csources cheaders clibraries skipwhite nextgroup=tclPred
syn keyword tclCritclCmd contained failed done tk cache tsources platform compiling sharedlibext scripting  skipwhite nextgroup=tclPred
syn keyword tclCritclCmd contained config skipwhite nextgroup=tclPred

syn keyword tclCritclCmd contained cinit skipwhite nextgroup=tclCBraces
syn keyword tclCritclCmd contained ccode skipwhite nextgroup=tclCBraces
syn keyword tclCritclCmd contained ccommand skipwhite nextgroup=tclCCommandName
syn match tclCCommandName contained "\S\+" skipwhite nextgroup=tclCCommandArgs
syn region tclCCommandArgs contained extend keepend start=+{+ end=+}+ skipwhite nextgroup=tclCBraces
hi link tclCCommandName tclBookends

" syn match tclPrimary contained "::critcl::cinit" skipwhite nextgroup=tclCBraces
" syn match tclPrimary contained "::critcl::ccode" skipwhite nextgroup=tclCBraces
" syn match tclPrimary contained "::critcl::ccommand" skipwhite nextgroup=tclCCommandName
" syn match tclCCommandName contained "\S\+" skipwhite nextgroup=tclCCommandArgs
" syn region tclCCommandArgs contained extend keepend matchgroup=tclBookends start=+{+ end=+}+ skipwhite nextgroup=tclCBraces
" syn region tclCCommandArgs contained extend keepend start=+{+ end=+}+ skipwhite nextgroup=tclCBraces
" hi link tclCCommandName tclBookends

syn keyword tclCritclCmd contained config skipwhite nextgroup=tclCritclConfigCmds
syn keyword tclCritclConfigCmds contained keepsrc option outdir I L force appinit tk language lines combine skipwhite nextgroup=@tclStuff
hi link tclCritclConfigCmds tclEnsemble

syn keyword tclCritclCmd contained cproc skipwhite nextgroup=tclCProcName
syn match tclCProcName contained "\S\+" skipwhite nextgroup=tclCProcAdefs
syn region tclCProcAdefs contained extend keepend start=+{+ end=+}+ skipwhite nextgroup=tclCProcRtype
syn match tclCProcRtype contained "\S\+" skipwhite nextgroup=tclCBraces
hi link tclCProcName tclBookends

" Link To C:
if exists("b:current_syntax")
    unlet b:current_syntax
endif
syn include @tclCCode syntax/c.vim
syn region tclCBraces contained extend keepend matchgroup=tclBookends start=+{+ms=s end=+}+me=e contains=@tclCCode,tclCBraces
" syn region tclCBraces contained extend keepend start=+{+ end=+}+ contains=@tclCCode,tclCBraces

" c.vim defines several ALLBUTS, add tcl/tk stuff - it's important!
syn cluster cParenGroup	    add=tcl.*
syn cluster cDefineGroup    add=tcl.*
syn cluster cPreProcGroup   add=tcl.*
syn cluster cMultiGroup	    add=tcl.*
syn cluster cParenGroup	    add=tk.*
syn cluster cDefineGroup    add=tk.*
syn cluster cPreProcGroup   add=tk.*
syn cluster cMultiGroup	    add=tk.*


" syn match tclPrimary contained "::critcl::csources"
" syn match tclPrimary contained "::critcl::cheaders"
" syn match tclPrimary contained "::critcl::clibraries"
" syn match tclPrimary contained "::critcl::failed"
" syn match tclPrimary contained "::critcl::done"
" syn match tclPrimary contained "::critcl::tk"
" syn match tclPrimary contained "::critcl::cache"
" syn match tclPrimary contained "::critcl::tsources"
" syn match tclPrimary contained "::critcl::platform"
" syn match tclPrimary contained "::critcl::compiling"
" syn match tclPrimary contained "::critcl::sharedlibext"
" syn match tclPrimary contained "::critcl::scripting"
" syn match tclPrimary contained "::critcl::cdata"

