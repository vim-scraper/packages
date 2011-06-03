" Vim syntax file
" Language:	Business Object Formulas Language
" Maintainer:	Mike Meirsman <meirsman@ultim8team.com>
" Last Change:	Fri, 16 Nov 2006

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Functions
syn keyword BOFunction Abs ApplicationValue Asc Average BlockNumber Ceil
syn keyword BOFunction Char ColumnNumber Concatenation Cos Count CountAll
syn keyword BOFunction CurrentDate CurrentTime CurrentUser DataProvider
syn keyword BOFunction DataProviderSQL DataProviderType DayName DayNumberOfMonth
syn keyword BOFunction DayNumberOfWeek DayNumberOfYear DaysBetween DocumentAuthor
syn keyword BOFunction DocumentDate DocumentName DocumentPartiallyRefreshed
syn keyword BOFunction DocumentTime DrillFilters EuroConvertFrom EuroConvertTo
syn keyword BOFunction EuroFromRoundErr EuroToRoundErr Even Exp Fact Fill Floor
syn keyword BOFunction FormatDate FormatNumber GetProfileNumber GetProfileString
syn keyword BOFunction GlobalFilters InitCap IsDate IsError IsLogical IsNull
syn keyword BOFunction IsNumber IsString IsTime LastDayOfMonth LastDayOfWeek
syn keyword BOFunction LastExecutionDate LastExecutionTime LastPrintDate Left
syn keyword BOFunction LeftPad LeftTrim Length LineNumber Ln Log Log10 Lower
syn keyword BOFunction Match Max Median Min Mod Month MonthNumberOfYear
syn keyword BOFunction MonthsBetween MultiCube NameOf NoFilter NumberOfDataProviders
syn keyword BOFunction NumberOfPages NumberOfRows Odd Page PageInSection Pos
syn keyword BOFunction Power Previous Product Quarter Rank RelativeDate Replace
syn keyword BOFunction Right RightPad RightTrim Round RowIndex RunningAverage
syn keyword BOFunction RunningCount RunningMax RunningMin RunningProduct RunningSum
syn keyword BOFunction SectionNumber ServerValue Sign Sin Sqrt StdDev StdDevP
syn keyword BOFunction SubStr Sum Tan ToDate ToNumber Trim Truncate UniverseName
syn keyword BOFunction Upper UserResponse Var VarP Week WordCap Year

" Operators
syn match BOMathsOperator "-\|=\|[<>+\*/%]\|Between\|InList\|And\|Or\|Not"
syn match BOContextOperator "ForAll\|ForEach\|In[^\(List\)]\|Where"

" If-Then-Else Statement
" If, Then and Else are considered as operators for Business Object
syn match BOIfThenElseOperator "If\|Then\|Else"

" Numbers
syn match BONumber "\<\d\+\(\.\d*\)\=\>"

" Variables
syn region BOVariable start=+<\S+ end=+>+

" Strings
syn region BOString start=+"+ end=+"+


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_BOF_syntax_inits")
  if version < 508
    let did_ATE_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink BOFunction		Function
  HiLink BOMathsOperator	Operator
  HiLink BOContextOperator	Operator
  HiLink BOIfThenElseOperator	Conditional
  HiLink BONumber		Number
  HiLink BOVariable		Identifier
  HiLink BOString		String
  
  delcommand HiLink
endif

let b:current_syntax = "bof"

" vim:ts=8
