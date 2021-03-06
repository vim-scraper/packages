" Vim syntax file for Tcl/tk language
" Language:	Tcl - extension snit
" Maintained:	SM Smithfield <m_smithfield@yahoo.com>
" Last Change:	11-19-06
" Filenames:    *.tcl
" Version:      0.1

syn match tclNamespace contained "\(::\)\=snit::" nextgroup=tclSnitCmd
hi link tclSnitCmd tclKeyword

" plain vanilla keywords
syn keyword tclKeyword contained mytypemethod myvar mytypevar myproc mymethod
syn keyword tclKeyword contained widgetclass typevariable expose
syn keyword tclKeyword contained hulltype
syn keyword tclKeyword contained varname typevarname codename
" def script
syn keyword tclSnitCmd contained widget skipwhite nextgroup=tclProcDef
syn keyword tclSnitCmd contained widgetadaptor skipwhite nextgroup=tclProcDef
syn keyword tclSnitCmd contained type skipwhite nextgroup=tclProcDef
syn keyword tclPrimary contained oncget skipwhite nextgroup=tclProcDef
" name args script
syn keyword tclPrimary contained onconfigure typemethod method skipwhite nextgroup=tclProcName
syn keyword tclSnitCmd contained macro skipwhite nextgroup=tclProcName
syn keyword tclSnitCmd contained compile skipwhite nextgroup=tclProcName
" type name args script
syn keyword tclSnitCmd contained method skipwhite nextgroup=tclProcType
syn keyword tclSnitCmd contained typemethod skipwhite nextgroup=tclProcType
" args script
syn keyword tclPrimary contained constructor skipwhite nextgroup=tclProcArgs
" script
syn keyword tclPrimary contained destructor typeconstructor skipwhite nextgroup=tclFoldBraces


" typevariable = an example
syn keyword tclPrimary contained typevariable variable skipwhite nextgroup=tclProcTypevarPred
syn region tclProcTypevarPred contained start=+.+ skip=+\\$+ end=+}\|]\|;\|$+ contains=tclProcTypevarOptsGroup,@tclStuff
syn match tclProcTypevarOptsGroup contained "-\a\+" contains=tclProcTypevarOpts
syn keyword tclProcTypevarOpts contained array
hi link tclProcTypevarOpts tclOption

syn keyword tclPrimary contained delegate skipwhite nextgroup=tclSnitDelegateCmds
syn keyword tclSnitDelegateCmds contained typemethod method skipwhite nextgroup=tclSnitDelegateMethodPred
syn keyword tclSnitDelegateCmds contained option skipwhite nextgroup=tclSnitDelegateOptionPred
syn region tclSnitDelegateMethodPred contained start=+.+ skip=+\\$+ end=+}\|]\|;\|$+ contains=@tclStuff,tclSnitDelegateMethodPrepositions
syn keyword tclSnitDelegateMethodPrepositions contained to as using except
syn region tclSnitDelegateOptionPred contained start=+.+ skip=+\\$+ end=+}\|]\|;\|$+ contains=@tclStuff,tclSnitDelegateOptionPrepositions
syn keyword tclSnitDelegateOptionPrepositions contained to as except
hi link tclSnitDelegateOptionPrepositions tclEnsemble
hi link tclSnitDelegateMethodPrepositions tclEnsemble
hi link tclSnitDelegateCmds tclSubcommand

syn keyword tclPrimary contained typecomponent component skipwhite nextgroup=tclSnitComponent
syn match tclSnitComponent contained "\S\+" skipwhite nextgroup=tclSnitComponentPred
syn region tclSnitComponentPred contained start=+.+ skip=+\\$+ end=+}\|]\|;\|$+ contains=@tclStuff,tclSnitComponentOptsGroup
syn match tclSnitComponentOptsGroup contained "-\a\+" contains=tclSnitComponentOpts
syn keyword tclSnitComponentOpts contained public inherit
hi link tclSnitComponentOpts tclOption

syn keyword tclPrimary contained install skipwhite nextgroup=tclSnitInstallName
syn match tclSnitInstallName contained "\S\+" skipwhite nextgroup=tclSnitInstallNameCmds
syn keyword tclSnitInstallNameCmds contained using skipwhite nextgroup=tclStuff
hi link tclSnitInstallNameCmds tclSubcommand

syn keyword tclPrimary contained installhull skipwhite nextgroup=tclSnitInstallHullCmds
syn keyword tclSnitInstallHullCmds contained using
hi link tclSnitInstallHullCmds tclSubcommand

syn keyword tclPrimary contained pragma skipwhite nextgroup=tclSnitPragmaPred
syn region tclSnitPragmaPred contained start=+.+ skip=+\\$+ end=+}\|]\|;\|$+ contains=@tclStuff,tclSnitPragmaOptsGroup
syn match tclSnitPragmaOptsGroup contained "-\a\+" contains=tclSnitPragmaOpts
syn keyword tclSnitPragmaOpts contained hastypeinfo hastypedestroy hasinstance canreplace hastypeinfo hastypedestroy hastypemethods hasinstances hasinfo simpledispatch
hi link tclSnitPragmaOpts tclOption

syn keyword tclSecondary contained configurelist destroy method option typemethod

" [recipe {
    " major tclSnit
    " keywords tclSecondary info
    " predicate cmds
    " commands type vars typevars typemethods options methods
" }]
"
" baked and imported
syn keyword tclSecondary contained info skipwhite nextgroup=tclSnitInfoSUBInfoPred
syn region tclSnitInfoSUBInfoPred contained excludenl keepend start=+.+ skip=+\\$+ end=+}\|]\|;\|$+ contains=tclSnitInfoSUBInfoCmds,@tclStuff
syn keyword tclSnitInfoSUBInfoCmds contained type vars typevars typemethods options methods skipwhite nextgroup=tclSnitInfoSUBInfoCmdsTypePred
syn region tclSnitInfoSUBInfoCmdsTypePred contained excludenl keepend start=+.+ skip=+\\$+ end=+}\|]\|;\|$+ contains=,@tclStuff

" XXX words can CONFLICT with Tcl
" option - same word, different usage
" variable - changes options
" configure - can step on tk configure !watch out! 
