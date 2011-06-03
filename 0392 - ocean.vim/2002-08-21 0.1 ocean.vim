" Vim syntax file
" Language:	OCEAN
" Maintainer:	Ahmed Nabil "anabil@ieee.org"
" Last Change:  29/04/2002	
" Comments:   	This file provides syntax hilighting for Open Command Environment for Analysis(OCEAN).
"		OCEAN is a scripting language used in EDA tools from Cadence Design Systems
"		that simulates analog and digital circuits at the differential equation level.
"
" This is based on spice.vim by Noam Halevy

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" ocean syntax is case INsensitive
syn case match

syn keyword	oceanTodo	contained TODO

syn keyword oceanSimulation 	ac analysis appendPath createFinalNetlist createNetlist
syn keyword oceanSimulation 	dc definitionFile delete desVar design envOption forcenode
syn keyword oceanSimulation 	ic includeFile modelFile nodeset noise ocnDisplay off option
syn keyword oceanSimulation 	path prependPath restore resultsDir run save saveOption setup simulator
syn keyword oceanSimulation 	stimulusFile store temp tran

syn keyword oceanDataAccess 	dataTypes getData i noiseSummary ocnHelp ocnPrint openResults outputParams
syn keyword oceanDataAccess 	outputs phaseNoise pv report resultParam results selectResult selectResults sp
syn keyword oceanDataAccess 	sweepNames sweepValues v vswr zm zref

syn keyword oceanPlotting 	addSubWindowTitle addSubwindow addTitle addWaveLabel addWindowLabel
syn keyword oceanPlotting 	clearAll clearSubwindow currentSubwindow currentWindow dbCompressionPlot
syn keyword oceanPlotting 	deleteSubwindow deleteWaveform displayMode graphicsOff 
syn keyword oceanPlotting 	graphicsOn hardCopy hardCopyOptions ip3Plot newWindow plot plotStyle
syn keyword oceanPlotting 	removeLabel xLimit yLimit

syn keyword oceanAliases 	iim im ip ir vdb vim vm vp vr

syn keyword oceanBuiltInFunctions 	abs acos add1 asin atan average b1f bandwidth
syn keyword oceanBuiltInFunctions 	cPwrContour cReflContour clip compression compressionVRI
syn keyword oceanBuiltInFunctions 	compressionVRICurves conjugate convolve cos cross db10
syn keyword oceanBuiltInFunctions 	db20 dbm delay deriv dft exp flip fourEval frequency ga gac
syn keyword oceanBuiltInFunctions 	gainBwProd gainMargin gmax gmin gmsg gmux gp gpc groupDelay 
syn keyword oceanBuiltInFunctions 	gt harmonic harmonicFreqList harmonicList iinteg imag integ
syn keyword oceanBuiltInFunctions 	ipn ipnVRI ipnVRICurves kf linRg ln log log10
syn keyword oceanBuiltInFunctions 	logRg lsb lshift mag max min mod nc overshoot peakToPeak phase
syn keyword oceanBuiltInFunctions 	phaseDeg phaseDegUnwrapped phaseMargin phaseRad phaseRadUnwrapped
syn keyword oceanBuiltInFunctions 	pow psd psdbb random real riseTime rms rmsNoise root round rshift sample
syn keyword oceanBuiltInFunctions 	settlingTime sin slewRate spectralPower sqrt srandom ssb sub1 tan tangent
syn keyword oceanBuiltInFunctions 	thd value xmax xmin xval ymax ymin

syn keyword oceanAdvanced 	cornerDesVar cornerMeas cornerRun cornerRunTemp correlationTable dataFilter
syn keyword oceanAdvanced 	histogram iterVsValue monteCarlo monteCorrelate monteDisplay monteExpr 
syn keyword oceanAdvanced 	monteOutputs monteResults monteRun monteSelectResults optimizeAlgoControl
syn keyword oceanAdvanced 	optimizeGoal optimizePlotOption optimizeRun optimizeVar paramAnalysis 
syn keyword oceanAdvanced 	paramRun residual scatterPlot selectProcess specLimits yield

syn keyword oceanDistributed 	deleteJob digitalHostMode digitalHostName hostMode hostName killJob
syn keyword oceanDistributed	monitor remoteDir resumeJob suspendJob wait

syn keyword oceanConstructs 	case cond for foreach if unless when while

syn keyword oceanFileFunctions 	close fprintf fscanf gets infile load newline outfile printf println

" Numbers, all with engineering suffixes and optional units
"==========================================================
"floating point number, with dot, optional exponent
syn match oceanNumber  "\<[0-9]\+\.[0-9]*\(e[-+]\=[0-9]\+\)\=\(meg\=\|[afpnumkgGK]\)\="
"floating point number, starting with a dot, optional exponent
syn match oceanNumber  "\.[0-9]\+\(e[-+]\=[0-9]\+\)\=\(meg\=\|[afpnumkgGK]\)\="
"integer number with optional exponent
syn match oceanNumber  "\<[0-9]\+\(e[-+]\=[0-9]\+\)\=\(meg\=\|[afpnumkgGK]\)\="

" Misc
"=====
syn match   oceanWrapLineOperator       "\\$"
syn match   oceanWrapLineOperator       "^+"

syn match   oceanIgnore			"\ \ " 
syn match   oceanIgnore			"\ \ \ " 
syn match   oceanIgnore			"\ \ \ \ "

syn match   oceanStatement      "^ \=\.\I\+"
syn region  oceanString	start=+L\="+ skip=+\\\\\|\\"+ end=+"+

syn region oceanComment start=";" end="$" contains=oceanTodo


" Errors
"=======
syn match oceanParenError ")"

" Matching pairs of parentheses
"==========================================
syn region  oceanParen transparent matchgroup=oceanOperator start="(" end=")" contains=ALLBUT,oceanParenError
"syn region  oceanSinglequote start=/'[a-z]/ end=/\ /me=s-1 contains=ALLBUT,oceanParenError oneline
"syn region  oceanOption start=/?[a-z]/ end=/\ /me=s-1 contains=ALLBUT,oceanParenError oneline

" Syncs
" =====
syn sync minlines=50

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_ocean_syntax_inits")
  if version < 508
    let did_ocean_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink oceanTodo	        Todo
  HiLink oceanWrapLineOperator 	oceanOperator
  HiLink oceanSinglequote      	oceanExpr
  HiLink oceanExpr             	Function
  HiLink oceanParenError       	Error
  HiLink oceanStatement        	Statement
  HiLink oceanNumber           	Number
  HiLink oceanComment          	Comment
  HiLink oceanOperator         	Operator
  HiLink oceanString           	String
  HiLink oceanOption         	oceanKeyword 
  HiLink oceanKeyword          	Type 
  HiLink oceanIgnore	       	Ignore
  HiLink oceanSimulation	oceanStatement		
  HiLink oceanDataAccess	oceanStatement		
  HiLink oceanPlotting		oceanStatement		
  HiLink oceanAliases		oceanStatement		
  HiLink oceanBuiltInFunctions	oceanStatement		
  HiLink oceanAdvanced		oceanStatement		
  HiLink oceanDistributed	oceanStatement		
  HiLink oceanConstructs	oceanStatement		
  HiLink oceanFileFunctions	oceanStatement		

  delcommand HiLink
endif

let b:current_syntax = "ocean"

" insert the following to $VIM/syntax/scripts.vim
" to autodetect HSpice netlists and text listing output:
"
" " Spice netlists and text listings
" elseif getline(1) =~ 'ocean\>' || getline("$") =~ '^\.end'
"   so <sfile>:p:h/ocean.vim

" vim: ts=8
