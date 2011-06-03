" VIM Syntax file
" Language: C# (C Sharp)
" Author:	Dan Seitz and Nathan Sharfi

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when syntax has already been created
if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

"ambiguous: new(storageclass, operator), using(statement, namespace)
" TODO: get a #region fold in here (I tried)
syn region	csCodeBlockFold					start="{" end="}" transparent fold
set foldlevel=2
set foldmethod=syntax
syn sync fromstart
" Comment and XML Documentation
syn match	csSpecialVal			"\\x\x\{1,4}\|\\u\x\{4}\|\\U\x\{8}\|\\0\o\{3}"
syn match	csRegexSpecialVal		"\\[abtrvfne]\|\\\o\{3}\|\\c\u\|\\x\x\{2}\|\\u\x\{4}"
syn match	csSpecialChar			"\\['\"?\\abefnrtv]"
syn match	csConsoleIndex			"{\(.\+?\)}" contained
syn region	csLineComment			start="//" end="$"
syn region	csCommentC				start="/\*" end="\*/"
syn match	csFloat					"\d\{1,}\.\d\{0,}[dDfm]\|\d\{1,}[dDm]\|\d\{1,}\.\d\{1,}"
syn match	csNumber				"0x\x\{1,}\|\d\{1,}" contains=csFloat
syn match	csXMLDocComment			+///.*+ contains=csXMLDocTag
syn region	csXMLDocTag				transparent contained start=+<+ end=+>+ contains=xmlDocTagName
syn keyword csXMLDocTagName			transparent contained c code example exception include 
syn keyword csXMLDocTagName			transparent contained permission remarks returns see seealso
syn keyword csXMLDocTagName			transparent contained list para param paramref summary value
syn keyword csConstant				null
syn region	csString				start=+"+	end=+"+	skip=+\\"+ contains=csSpecialChar,csConsoleIndex,csSpecialVal
syn region	csVerbatimString		start=+@"+	end=+"+	skip=+""+
syn region	csChar					start=+'+	end=+'+ skip=+\'+ contains=csSpecialChar,csSpecialVal
" number stuff...not yet
syn keyword csBoolean				true false
" float stuff...not yet
" identifier, function not here yet
syn keyword csStatement				goto break continue return
syn keyword csStatement				lock checked
syn keyword csConditional			if else switch
syn keyword csRepeat				while do for foreach
syn keyword csLabel					case default
" operator: not here yet
syn keyword csKeyword				this base
syn keyword csException				try throw catch finally

syn	region	csRegion				start="^\s*#\s*\(region\>\|endregion\>\|\)" skip="\\$" end="$" end="//"me=s-1 fold contains=csCommentC
syn keyword csPreproc				#region #endregion #line #warning #error
syn keyword csDefine				#define #undef
syn keyword csPreCondit				#if #elif #endif #else 


syn keyword csType					bool byte char decimal double float string object
syn keyword csType					int long sbyte short uint ulong ushort void
syn keyword csStructStorageClass	abstract sealed
syn keyword csMemberStorageClass	abstract virtual override static
syn keyword csStructure				struct enum class namespace interface delegate event
syn keyword csStructAccess			public internal
syn keyword csMemberAccess			public protected private readonly
syn keyword csTodo					contained TODO FIXME BUG BUGBUG HACK XXX
syn cluster csCommentGroup			contains=csTodo
" needs csAttribute

if version >= 508 || !exists("did_cs_syn_init")
	if version < 508
		let did_cs_syn_init = 1
		command -nargs=+ HiLink hi link <args>
	else
		command -nargs=+ HiLink hi def link <args>
	endif

	HiLink	csFloat					Float
	HiLink	csNumber				Number
	HiLink	csSpecialVal			csSpecialChar
	HiLink	csConsoleIndex			csSpecialChar
	HiLink	csSpecialChar			SpecialChar
	HiLink	csLineComment			Comment
	HiLink	csCommentC				Comment
	HiLink	csXMLDocComment			Comment
	HiLink	csConstant				Constant
	HiLink	csString				String
	HiLink	csVerbatimString		String
	HiLink	csChar					Character
	HiLink	csBoolean				Boolean
	HiLink	csStatement				Statement
	HiLink	csConditional			Conditional
	HiLink	csRepeat				Repeat
	HiLink	csLabel					Label
	HiLink	csKeyword				Keyword
	HiLink	csException				Exception
	HiLink	csPreproc				Preproc
	HiLink	csRegion				Preproc
	HiLink	csDefine				Define
	HiLink	csPreCondit				PreCondit
	HiLink	csType					Type
	HiLink	csStructure				Structure
	HiLink	csStructStorageClass	StorageClass
	HiLink	csMemberStorageClass	StorageClass
	HiLink	csMemberAccess			AccessLevel
	HiLink	csStructAccess			AccessLevel
	HiLink	csAttribute				Attribute
delcommand HiLink
endif

let b:current_syntax = "cs"
