" DTrace D script syntax file. To avoid confusion with the D programming
" language, I call this script dtrace.vim instead of d.vim.
" Language: D script as described in "Solaris Dynamic Tracing Guide",
"           http://docs.sun.com/app/docs/doc/817-6223
" Version: 1.1
" Last Change: 2008/03/19
" Maintainer: Nicolas Weber <nicolasweber@gmx.de>

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Read the C syntax to start with
if version < 600
  so <sfile>:p:h/c.vim
else
  runtime! syntax/c.vim
  unlet b:current_syntax
endif

setlocal iskeyword+=@,$

syn clear cCommentL  " dtrace doesn't support // style comments

" Probe descriptors need explicit matches, so that keywords in probe
" descriptors don't show up as errors
syn match dtraceStatement "^\S\{-}:\S\{-}:\S\{-}:\S\{-}$"

syn region dtracePredicate start=+/+ end=+/+
  "contains=ALLBUT,dtraceOption  " this lets the region contain too much stuff

" Pragmas.
" dtrace seems not to support whitespace before or after the '='.  dtrace
" supports only one option per #pragma, and no continuations of #pragma over
" several lines with '\'.
" Note that dtrace treats units (Hz etc) as case-insenstive, we allow only
" sane unit capitalization in this script (ie 'ns', 'us', 'ms', 's' have to be
" small, Hertz can be 'Hz' or 'hz')
" XXX: "cpu" is always highlighted as builtin var, not as option

"   auto or manual: bufresize
syn match dtraceOption contained "bufresize=\%(auto\|manual\)\s*$"

"   scalar: cpu jstackframes jstackstrsize nspec stackframes stackindent ustackframes
syn match dtraceOption contained "\%(cpu\|jstackframes\|jstackstrsize\|nspec\|stackframes\|stackindent\|ustackframes\)=\d\+\s*$"

"   size: aggsize bufsize dynvarsize specsize strsize 
"   size defaults to something if no unit is given (ie., having no unit is ok)
syn match dtraceOption contained "\%(aggsize\|bufsize\|dynvarsize\|specsize\|strsize\)=\d\+\%(k\|m\|g\|t\|K\|M\|G\|T\)\=\s*$"

"   time: aggrate cleanrate statusrate switchrate
"   time defaults to hz if no unit is given
syn match dtraceOption contained "\%(aggrate\|cleanrate\|statusrate\|switchrate\)=\d\+\%(hz\|Hz\|ns\|us\|ms\|s\)\=\s*$"

"   No type: defaultargs destructive flowindent grabanon quiet
syn match dtraceOption contained "\%(defaultargs\|destructive\|flowindent\|grabanon\|quiet\)\s*$"


" Turn reserved but unspecified keywords into errors
syn keyword dtraceReservedKeyword auto break case continue counter default do
syn keyword dtraceReservedKeyword else for goto if import probe provider
syn keyword dtraceReservedKeyword register restrict return static switch while

" Add dtrace-specific stuff
syn keyword dtraceOperator   sizeof offsetof stringof xlate
syn keyword dtraceStatement  self inline xlate translator

" Builtin variables
syn keyword dtraceIdentifier arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 
syn keyword dtraceIdentifier args caller chip cpu curcpu curlwpsinfo curpsinfo
syn keyword dtraceIdentifier curthread cwd epid errno execname gid id ipl lgrp
syn keyword dtraceIdentifier pid ppid probefunc probemod probename probeprov
syn keyword dtraceIdentifier pset root stackdepth tid timestamp uid uregs
syn keyword dtraceIdentifier vtimestamp walltimestamp
syn keyword dtraceIdentifier ustackdepth

" Macro Variables
syn match dtraceConstant     "$[0-9]\+"
syn keyword dtraceConstant    $egid $euid $gid $pgid $ppid
syn keyword dtraceConstant    $projid $sid $target $taskid $uid

" Data Recording Actions
syn keyword dtraceFunction   trace tracemem printf printa stack ustack jstack

" Process Destructive Actions
syn keyword dtraceFunction   stop raise copyout copyoutstr system

" Kernel Destructive Actions
syn keyword dtraceFunction   breakpoint panic chill

" Special Actions
syn keyword dtraceFunction   speculate commit discard exit

" Subroutines
syn keyword dtraceFunction   alloca basename bcopy cleanpath copyin copyinstr
syn keyword dtraceFunction   copyinto dirname msgdsize msgsize mutex_owned
syn keyword dtraceFunction   mutex_owner mutex_type_adaptive progenyof
syn keyword dtraceFunction   rand rw_iswriter rw_write_held speculation
syn keyword dtraceFunction   strjoin strlen

" Aggregating Functions
syn keyword dtraceAggregatingFunction count sum avg min max lquantize quantize

syn keyword dtraceType int8_t int16_t int32_t int64_t intptr_t
syn keyword dtraceType uint8_t uint16_t uint32_t uint64_t uintptr_t
syn keyword dtraceType string
syn keyword dtraceType pid_t id_t


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_triangle_syn_inits")
  if version < 508
    let did_dtrace_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink dtraceReservedKeyword Error

  HiLink dtracePredicate String
  HiLink dtraceStatement Statement
  HiLink dtraceConstant Constant
  HiLink dtraceIdentifier Identifier
  HiLink dtraceAggregatingFunction dtraceFunction
  HiLink dtraceFunction Function
  HiLink dtraceType Type
  HiLink dtraceOperator Operator
  HiLink dtraceComment Comment
  HiLink dtraceNumber Number
  HiLink dtraceOption Identifier

  delcommand HiLink
endif

let b:current_syntax = "dtrace"
