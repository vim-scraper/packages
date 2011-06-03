" JDL syntax file
" Language:          Job Description Language
" Author:            Roberto Bonvallet <rbonvall@cern.ch>
" Creation:          20060727
" Last Modification: 20060811

" Reference: www.infn.it/workload-grid/docs/DataGrid-01-TEN-0102-0_2-Document.pdf

syntax case ignore

syntax keyword jdlTypePredicate      IsUndefined IsError IsString IsList
syntax keyword jdlTypePredicate      IsClassad IsBoolean IsAbsTime IsRelTime
syntax keyword jdlListMembership     Member IsMember
syntax keyword jdlTimeQueries        CurrentTime TimeZoneOffset DayTime
syntax keyword jdlTimeConstruction   MakeDate MakeAbsTime MakeRelTime
syntax keyword jdlAbsTimeComp        GetYear GetMonth GetDayOfYear GetDayOfMonth
syntax keyword jdlAbsTimeComp        GetYearOfWeek GetHours GetMinutes GetSeconds
syntax keyword jdlRelTimeComp        GetDays GetHours GetMinutes GetSeconds
syntax keyword jdlTimeConversion     InDays InHours InMinutes InSeconds
syntax keyword jdlStringFunction     StrCat ToUpper ToLower SubStr RegExp
syntax keyword jdlTypeConversion     Int Real String Bool AbsTime RelTime
syntax keyword jdlMathFunction       Floor Ceil Round

syntax keyword jdlGangmatchFunction  AnyMatch WhichMatch AllMatch

syntax keyword jdlAttribute  Type JobType Executable Arguments
syntax keyword jdlAttribute  StdInput StdOutput StdError
syntax keyword jdlAttribute  InputSandbox OutputSandbox
syntax keyword jdlAttribute  Environment InputData DataAccessProtocol OutputSE
syntax keyword jdlAttribute  OutputData OutputFile StorageElement
syntax keyword jdlAttribute  LogicalFileName VirtualOrganisation RetryCount
syntax keyword jdlAttribute  MyProxyServer HLRLocation NodeNumber JobSteps
syntax keyword jdlAttribute  CurrentStep ListenerPort Requirements Rank
syntax keyword jdlAttribute  FuzzyRank 
syntax keyword jdlAttribute  ReplicaCatalog

syntax keyword jdlLiteral    undefined error false true

syntax keyword jdlReference  self other 

"syntax match   lplOperator  "[|+-/*^&=()]"
"syntax match   lplIndex           "\w*" contained contains=lplNumber
"syntax match   lplIndexBrackets   "[{].\{-}[}]" contained contains=lplIndex

"syntax match jdlPrimaryOperator 
syntax match jdlOperator "[-+!~]"
syntax match jdlOperator "[*/%]"
syntax match jdlOperator "[<>]"
syntax match jdlOperator "<<"
syntax match jdlOperator ">>>\="
syntax match jdlOperator "[!=]="
syntax keyword jdlOperator  is isnt
syntax match jdlOperator "[&^|]"
syntax match jdlOperator "&&"
syntax match jdlOperator "||"
syntax match jdlOperator "[?:]"

syntax region jdlString start=/"/ end=/"/ contains=jdlEscaped
syntax match  jdlEscaped /\\./ contained

syntax match  jdlDecimalInteger /\<\(0\|[1-9][0-9]*\)\>/
syntax match  jdlOctalInteger   /\<0[0-7]\+\>/        contains=jdlOctalZero
syntax match  jdlHexInteger     /\<0x[0-9a-fA-F]\+\>/ contains=jdlHexZeroX
syntax match  jdlOctalZero      /\<0/  contained
syntax match  jdlHexZeroX       /\<0x/ contained
syntax match  jdlReal           /\d*[.]\d*\([eE]\d\+\|[BKMGT]\)\=/ 
" TODO: 
"syntax match  jdlAbsoluteTime
"syntax match  jdlRelativeTime

syntax match jdlEndOfLine       /;[ 	]*$/ contains=jdlWrongWhitespace
syntax match jdlWrongWhitespace /[ 	]*/ contained

syntax match jdlLineContinuation /\\$/

syntax region jdlComment start=!/[*]! end=![*]/!
syntax region jdlComment start=!//!  end=/$/
syntax region jdlComment start=/#/   end=/$/


highlight def link jdlTypePredicate     Function
highlight def link jdlListMembership	Function
highlight def link jdlTimeQueries	Function
highlight def link jdlTimeConstruction	Function
highlight def link jdlAbsTimeComp	Function
highlight def link jdlRelTimeComp	Function
highlight def link jdlTimeConversion	Function
highlight def link jdlStringFunction	Function
highlight def link jdlTypeConversion	Function
highlight def link jdlMathFunction	Function
highlight def link jdlLiteral	        Constant
highlight def link jdlReference	        Identifier
highlight def link jdlAttribute 	Keyword
highlight def link jdlOperator  	Operator
highlight def link jdlString    	Constant
highlight def link jdlEscaped   	Special
highlight def link jdlWrongWhitespace   Error
highlight def link jdlLineContinuation  Special
highlight def link jdlDecimalInteger    Constant
highlight def link jdlOctalInteger      Constant
highlight def link jdlHexInteger        Constant
highlight def link jdlOctalZero         Special
highlight def link jdlHexZeroX          Special
highlight def link jdlReal              Constant
highlight def link jdlComment   	Comment

let b:current_syntax = "jdl"
