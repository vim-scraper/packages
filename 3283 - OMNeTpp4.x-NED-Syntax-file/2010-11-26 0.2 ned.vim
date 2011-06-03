" Vim syntax file
" Language:	Omnet++ NED
" Maintainer:	Simon <carmark.xue@gmail.com> 
" URL:			http://carmark.javaeye.com 
" Last Change:	November 28, 2005
" Version:      1.1
" Released under the terms of the GNU/GPL licence v2
"
" syntax file for NED files by OMNeT++ 4.1
"
" The following parameters are available for truning ned
" syntax highligting, with default given:
"
" unlet ned_fold
" unlet ned_fold_blocks
" let ned_nofold_packages=1
" let ned_nofold_subs=1

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syntax case match

syn match nedValidNumber        "\d\+" nextgroup=nedValidTimeUnits skipwhite
syn match nedValidNumber        "\d\+\.\d\+" nextgroup=nedValidTimeUnits skipwhite
syn match nedValidNumber        "\d*\.\d\+" nextgroup=nedValidTimeUnits skipwhite

syn match nedValidTimeUnits     "ns" 
syn match nedValidTimeUnits     "m"
syn match nedValidTimeUnits     "ms"
syn match nedValidTimeUnits     "s"
syn match nedValidTimeUnits     "h"
syn match nedValidTimeUnits     "d"

syn keyword nedConditional      if contained
syn keyword nedConditional		else contained
syn keyword nedInclude          import 
syn keyword nedInclude			package 
syn keyword nedRepeat           for contained 
syn keyword nedRepeat			do contained
syn keyword nedType             string int bool xml char volatile double
syn keyword nedBoolean          true false
syn keyword nedTodo				TODO FIXME TBD XXX contained

syn match nedIdentifier         "[a-zA-Z_][0-9a-zA-Z_]*"


syn keyword nedChannel          channel contained
syn keyword nedChannelOptions   delay error datarate
syn keyword nedSimple           simple contained
syn keyword nedModule           module contained
syn keyword nedNetwork          network contained
syn keyword nedMessage			message contained

syn match nedSubDefinition		"submodules:"he=e-1 contained
syn match nedSubDefinition		"parameters:"he=e-1 contained
syn match nedSubDefinition		"types:"he=e-1 contained
syn match nedSubDefinition		"gates:"he=e-1 contained
syn match nedSubDefinition		"connections:"he=e-1 contained

syn match nedProperties         "@display" 
syn match nedProperties		 	"@unit"
syn match nedProperties			"@class"
syn match nedProperties			"@namespace"
syn match nedProperties			"@prompt"
syn match nedProperties			"@loose"
syn match nedProperties			"@directIn"

syn match nedGateOptions        "in:"he=e-1 " 
syn match nedGateOptions        "out:"he=e-1 " 
syn match nedGateOptions		"inout:"he=e-1 "add new gate
syn match nedComment            "\/\/.*" contains=nedTodo

syn region nedString		    start=/"/ skip=/\\"/ end=/"/
syn keyword nedReservedWords     gatesizes nocheck ref ancestor like input extends 

"--KeyWords For Functions
"--function for category "conversion"
syn match nedFunctions		"double\s*("he=e-1
syn match nedFunctions		"int\s*("he=e-1
syn match nedFunctions		"string\s*("he=e-1
"--function for category "math"
syn keyword nedFunctions		acos asin atan atan2 ceil cos exp fabs floor fmod hypot log log10 max min pow sin sqrt tan
"--function for category "ned"
syn keyword nedFunctions		ancestorIndex fullName fullPath parentIndex
"--function for category "random/continuous"
syn keyword nedFunctions		beta cauchy chi_square erlang_k exponential gamma_d lognormal normal pareto_shifted student_t triang truncnormal uniform weibull
"--function for category "random/discrete"
syn keyword nedFunctions		bernoulli binomial geometric intuniform negbinomial poisson
"--function for category "strings"
syn match nedFunctions		"contains"
syn keyword nedFunctions		choose endsWith expand indexOf length replace replaceFirst startsWith substring substringAfter substringAfterLast substringBefore substringBeforeLast tail toLower toUpper trim
"--function for category "units"
syn keyword nedFunctions		convertUnit	dropUnit replaceUnit unitOf

"
" Folding
if exists("ned_fold")
	syn cluster nedNoFold contains=nedValidNumber,nedValidTimeUnits,nedConditional,nedInclude,nedRepeat,nedType,nedBoolean,nedIdentifier,nedChannel,nedChannelOptions,nedSimple,nedModule,nedNetwork,nedMessage,nedSubDefinition,nedModuleOptions,nedGateOptions,nedCommentExe,nedComment,nedString,nedReservedWords,nedFunctions
	if !exists("ned_nofold_packages")
		syn region nedPackageFold start="^package \S\+;\s*\(\/\/.*\)\=$" end="^1;\s*\(\/\/.*\)\=$" end="\n\+package"me=s-1 transparent fold keepend contains=ALL
	endif

	if !exists("ned_nofold_subs")
		syn region nedSimpleFold	start="^\z(\s*\)\<simple\>.*[^}]$"	end="^\z1}\s*\(\/\/.*\)\=$" transparent fold keepend extend contains=ALL
		syn region nedModuleFold     start="^\z(\s*\)\<module\>.*[^};]$" end="^\z1}\s*\(\/\/.*\)\=$" transparent fold keepend contains=ALL
		syn region nedNetworkFold     start="^\z(\s*\)\<network\>.*[^};]$" end="^\z1}\s*\(\/\/.*\)\=$" transparent fold keepend contains=ALL
		syn region nedMessageFold     start="^\z(\s*\)\<message\>.*[^};]$" end="^\z1}\s*\(\/\/.*\)\=$" transparent fold keepend contains=ALL
		syn region nedChannelFold     start="^\z(\s*\)\<channel\>.*[^};]$" end="^\z1}\s*\(\/\/.*\)\=$" transparent fold keepend contains=ALL
	endif
 
	if exists("ned_fold_blocks")
		syn region nedBlockFold start="^\z(\s*\)\(if\|elsif\|unless\|for\|while\|until\)\s*(.*)\(\s*{\)\=\s*\(\/\/.*\)\=$" start="^\z(\s*\)foreach\s*\(\(my\|our\)\=\s*\S\+\ s*\)\=(.*)\(\s*{\)\=\s*\(\/\/.*\)\=$" end="^\z1}\s*;\=\(\/\/.*\)\=$" transparent fold keepend contains=ALL
		syn region nedBlockFold start="^\z(\s*\)\(do\|else\)\(\s*{\)\=\s*\(\/\/.*\)\=$" end="^\z1}\s*while" end="^\z1}\s*;\=\(\/\/.*\)\=$" transparent fold keepend contains=ALL
   endif
 
   setlocal foldmethod=syntax
   syn sync fromstart
else
" fromstart above seems to set minlines even if perl_fold is not set.
   syn sync minlines=0
endif
"

if version >= 508 || !exists("did_ned_syn_inits")
    if version < 508
      let did_ned_syn_inits = 1
      command -nargs=+ HiLink hi link <args>
    else
      command -nargs=+ HiLink hi def link <args>
    endif
    HiLink nedComment                     Comment
    HiLink nedConditional                 Conditional
    HiLink nedRepeat                      Repeat
    HiLink nedIdentifier                  Identifier
    HiLink nedValidTimeUnits              Constant
    HiLink nedValidNumber                 Number
    HiLink nedInclude                     Include
    HiLink nedType                        Type
    HiLink nedBoolean                     Boolean
    HiLink nedChannel                     Keyword
    HiLink nedChannelOptions              Keyword
    HiLink nedSimple                      Keyword
    HiLink nedSubDefinition               Keyword
    HiLink nedGateOptions                 Keyword
    HiLink nedString                      String
    HiLink nedModule                      Keyword
    HiLink nedSubModules                  Keyword
    HiLink nedModuleOptions               Keyword
    HiLink nedNetwork                     Keyword
    HiLink nedEndNetwork                  Keyword
    HiLink nedReservedWords               Keyword
    HiLink nedFunctions                   Keyword
	HiLink nedTodo						  Todo
	HiLink nedProperties				  Keyword
    delcommand HiLink
endif

let b:current_syntax = "ned"
