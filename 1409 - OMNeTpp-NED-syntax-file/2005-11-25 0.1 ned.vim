" Vim syntax file
" Language:	Omnet++ NED
" Maintainer:	Timo Teifel <timo@teifel-net.de>
" URL:          www.teifel.net/projects/vim
" Last Change:	2005 Nov 25
" Version:      0.1
" Released under the terms of the GNU/GPL licence v2
"
" Very basic syntax file for NED files by OMNeT++
" does not know any regions/areas yet. Maybe I will implement
" this when I learned how to do it ;)

if version < 600
  syntax clear
endif

syn match nedValidTimeUnits     "[0-9]ns"hs=s+1 
syn match nedValidTimeUnits     "[0-9]m"hs=s+1
syn match nedValidTimeUnits     "[0-9]ms"hs=s+1
syn match nedValidTimeUnits     "[0-9]s"hs=s+1 
syn match nedValidTimeUnits     "[0-9]h"hs=s+1
syn match nedValidTimeUnits     "[0-9]d"hs=s+1

syn keyword nedConditional      if
syn keyword nedInclude          import
syn keyword nedRepeat           for do endfor
syn keyword nedType             string numeric bool xml char const
syn keyword nedBoolean          true false
syn match nedIdentifier         "[a-zA-Z_][0-9a-zA-Z_]*"
syn match nedComment            "\/\/.*"

syn keyword nedChannel          channel endchannel
syn keyword nedChannelOptions   delay error datarate
syn keyword nedSimple           simple endsimple
syn keyword nedModule           module endmodule
syn match nedSubModules         "submodules:"he=e-1
syn match nedModuleOptions      "display:"he=e-1

syn keyword nedNetwork          network endnetwork

syn match nedSimpleDefinition   "parameters:"he=e-1
syn match nedSimpleDefinition   "gates:"he=e-1

syn match nedGateOptions        "in:"he=e-1
syn match nedGateOptions        "out:"he=e-1

syn region nedString		start=/"/ end=/"/
syn keyword nedReservedWords    connections gatesizes nocheck ref ancestor like input  xmldoc

hi def link nedComment                     Comment
hi def link nedConditional                 Conditional
hi def link nedRepeat                      Repeat
hi def link nedIdentifier                  Identifier
hi def link nedValidTimeUnits              Constant
hi def link nedInclude                     Include
hi def link nedType                        Type
hi def link nedBoolean                     Boolean
hi def link nedChannel                     Keyword
hi def link nedChannelOptions              Keyword
hi def link nedSimple                      Keyword
hi def link nedSimpleDefinition            Keyword
hi def link nedGateOptions                 Keyword
hi def link nedString                      String
hi def link nedModule                      Keyword
hi def link nedSubModules                  Keyword
hi def link nedModuleOptions               Keyword
hi def link nedNetwork                     Keyword
hi def link nedEndNetwork                  Keyword
hi def link nedReservedWords               Keyword
