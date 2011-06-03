" Vim syntax file
" Language:     sip
" Maintainer:   Jonathan Gardner <jgardner@jonathangardner.net>
" URL:          http://riverbankcomputing.co.uk
" Remark:       
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

runtime! syntax/cpp.vim
unlet b:current_syntax
syn include @Cpp syntax/cpp.vim
unlet b:current_syntax
syn include @Python syntax/python.vim
unlet b:current_syntax
syn include @Make syntax/make.vim
unlet b:current_syntax

syn keyword sipKeyword Version PyMethods PyNumberMethods PySequenceMethods
syn keyword SipKeyword PyMappingMethods
syn keyword SipKeyword __abs__ __add__ __and__ __call__ __cmp__
syn keyword SipKeyword __coerce__ __contains__ __div__ __divmod__
syn keyword SipKeyword __float__ __getitem__ __getslice__ __hash__
syn keyword SipKeyword __hex__ __iadd__ __iand__ __idiv__ __ilshift__
syn keyword SipKeyword __imod__ __imul__ __int__ __invert__ __ior__
syn keyword SipKeyword __ipow__ __irshift__ __isub__ __ixor__ __len__
syn keyword SipKeyword __long__ __lshift__ __mod__ __mul__ __neg__
syn keyword SipKeyword __nonzero__ __oct__ __or__ __pos__ __pow__
syn keyword SipKeyword __repr__ __richcompare__ __rshift__ __setitem__
syn keyword SipKeyword __setslice__ __str__ __sub__ __xor__

syn match sipSpecial "\$[$CSOcomP]"

syn keyword sipConstant sipAcquireLock sipArgs sipArgsParsed sipBadSetType
syn keyword sipConstant sipBadVirtualResultType sipCallHook sipCheckNone
syn keyword sipConstant sipCondAcquireLock sipCondReleaseLock sipConnectRx
syn keyword sipConstant sipConvertFromBool sipConvertFromVoidPtr sipConvertRx
syn keyword sipConstant sipConvertToCpp sipConvertToVoidPtr sipDisconnectRx
syn keyword sipConstant sipEmitSignal sipEvalMethod sipFree sipGetComplexCppPtr
syn keyword sipConstant sipGetCppPtr sipGetRx sipGetThisWrapper
syn keyword sipConstant sipIsSubClassInstance sipMalloc sipMapCppToSelf
syn keyword sipConstant sipParseArgs sipReleaseLock

syn match sipKeyword "^%Module"
syn match sipKeyword "^%End$"
syn match sipKeyword "^%Include"
syn match sipKeyword "^%Import"
syn match sipKeyword "^%Version"
syn match sipKeyword "^%PrimaryVersions"
syn match sipKeyword "^%ExposeFunction$"

syn region sipIf matchgroup=sipKeyword start=+^%If+ end=+^%End$+ contains=TOP fold keepend extend transparent

syn region sipCppCode matchgroup=sipKeyword start="^%\(Member\|C++\|Virtual\|Variable\)Code$" end=+^%End$+ contains=@Cpp,sipSpecial fold keepend extend transparent

syn region sipPythonCode matchgroup=sipKeyword start="^%\(Pre\)\=PythonCode$" end=+^%End$+ contains=@Python,sipSpecial fold keepend extend transparent

syn region sipMakefile matchgroup=sipKeyword start="^%Makefile$" end=+^%End$+ contains=@Make,sipSpecial fold keepend extend transparent

syn region sipHeaderCode matchgroup=sipKeyword start=+^%\(Exported\)\=HeaderCode$+ end=+^%End$+ contains=cInclude,sipKeyword,sipSpecial fold keepend extend transparent

syn region sipCopying matchgroup=sipKeyword start=+^%Copying$+ end=+^%End$+ contains=sipKeyword,sipSpecial fold keepend extend transparent

syn region sipDoc matchgroup=sipKeyword start=+^%\(Exported\)\=Doc$+ end=+^%End$+ contains=sipKeyword,sipSpecial fold keepend extend transparent

syn region sipVersionCode matchgroup=sipKeyword start=+^%VersionCode$+ end=+^%End$+ contains=sipKeyword,sipSpecial fold keepend extend transparent


if version >= 508 || !exists("did_sip_syntax_inits")
  if version < 508
    let did_sip_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink sipSpecial Special
  HiLink sipKeyword Keyword
  HiLink sipConstant  Constant
  delcommand HiLink
endif

let b:current_syntax = "sip"

" vim: ts=2 sw=2 sts=2 tw=0 et
