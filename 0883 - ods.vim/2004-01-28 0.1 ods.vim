" Vim syntax file 
" Language:	ODS Toolbox rule language
" Previous Maintainer:	
" Current  Maintainer:
" Last Change:	
" Remove any old syntax stuff hanging around
syn clear

syn case match

syn keyword odsStatement 	after before changed close enter extevent finish
syn keyword odsStatement 	key leave on select start wsievent

syn keyword odsType 		anyvalue attribute boolean class enum event float integer index
syn keyword odsType 		method object record pointer scope string type void

syn keyword odsFunction 	applyformat atof atoi beep create destroy exit 
syn keyword odsFunction 	fail first ftoa ftoi getvalue inherited itoa itof
syn keyword odsFunction 	length load parsepath print querybox
syn keyword odsFunction 	save second setinherit setvalue stop strcmp stringpos substring
syn keyword odsFunction 	tolower toupper trace trimstr typeof updatescreen

" ODS rule extenders
syn keyword odsFunction 	ODSCanvasFunc ATC_Func_Unknown ODSAssignPresToViewport
syn keyword odsFunction 	ODSLinkObject ODSUnlinkObject ODSCompareObjects ODSGetCurrentTime
syn keyword odsFunction 	ODSCountVisibleTracks ODSReadJepData ODSGetRascalLoadErrors
syn keyword odsFunction 	ODSGetJeppesenLoadErrors ODSReadRascalData ODSFinishSharedLoading
syn keyword odsFunction 	ODSDestroy ODSCalcDistanceAndBearing ODSLoadMap ODSDumpMap
syn keyword odsFunction 	ODSDeleteObjectsOfLayer ODSGetNrOfPresInLayer ODSGetPresOfLayer
syn keyword odsFunction 	ODSAssignPres ODSGetModelID ODSGetPresID ODSSetAttr
syn keyword odsFunction 	ODSLockViewport ODSUnlockViewport ODSCreatePres ODSPick
syn keyword odsFunction 	ODSGetFirstObject ODSGetFirstObjectOfClass ODSGetNextObject
syn keyword odsFunction 	ODSGetNextObjectOfClass ODSCreateTransformation ODSSetIntensity
syn keyword odsFunction 	ODSGetIntensity ODSSetRgb ODSGetRgb ODSBlinkColor ODSBlinkColorOff
syn keyword odsFunction 	ODSOvercolor1 ODSOvercolor2 ODSCreate ODSCreateService
syn keyword odsFunction 	ODSDestroyService ODSGetDMID ODSDelayUnlock ODSFlush
syn keyword odsFunction 	ODSOverrideTranslation ODSClearTranslations
syn keyword odsFunction 	WIRE_DM_ClientOpen WIRE_DM_ClientMessage
syn keyword odsFunction 	WIRE_DM_ClientClose WIRE_DM_ClientPing ODSSetCurrentTrafo
syn keyword odsFunction 	DM_CreateDirectory OIMGetObjects OIMGetAllObjects
syn keyword odsFunction 	OIMGetObjectWithIndex OIMGetNumOfObjects OIMGetSizeOfObjList
syn keyword odsFunction 	OIMObjListDelete ODSUpdateDisplay ODSFontSize ODSLockViewports
syn keyword odsFunction 	ODSUnlockViewports DM_ShellExec

syn keyword odsConditional 	if then case endcase else elseif endif
syn keyword odsRepeat 		do until for to downto endfor while endwhile

syn keyword odsObject		aircraftpresentation areapresentation baseobject 
syn keyword odsObject		ccxcontrol checkbox compassrose conflict conflictviewport
syn keyword odsObject		dialog edittext extdflight flight flightlegpresentation 
syn keyword odsObject		graphicobject groupbox labelpresentation image 
syn keyword odsObject		linedlabelpresentation listbox 
syn keyword odsObject		mapobject mappoint menubox menuitem menusep messagebox module 
syn keyword odsObject		observer plot point pointpresentation polyline
syn keyword odsObject		polysegmentpresentation poptext pushbutton 
syn keyword odsObject		radiobutton rangemarker rascalcontrol rectangle rubberband 
syn keyword odsObject		scrollbar statictext stereotrafo symbolpresentation 
syn keyword odsObject		tablefield trackpresentation 
syn keyword odsObject		verticaltrackpresentation verticalviewport viewport weather window

syn keyword odsImpExp           import export

syn region odsString		start=+"+ end=+"+
syn region odsComment		start="//" end="$"
syn region odsComment		start="!!" end="$"
syn region odsComment		start="/\*" end="\*/"
syn region odsFold		start="{" end="}" transparent fold

"syn sync lines=250
syn sync fromstart

  hi link odsAcces			Statement
  hi link odsByte			Number
  hi link odsComment			Comment
  hi link odsConditional		Conditional
  hi link odsFunction			Function
  hi link odsLabel			Label
  hi link odsMatrixDelimiter		Identifier
  hi link odsModifier			Type
  hi link odsNumber			Number
  hi link odsOperator			Operator
  hi link odsRepeat			Repeat
  hi link odsStatement			Statement
  hi link odsString			String
  hi link odsStructure			Structure
  hi link odsTodo			Todo
  hi link odsType			Type
  hi link odsImpExp                     Include
  hi link odsUnclassified		Statement
  hi link odsObject			Statement
  
hi Folded guifg=DarkGray guibg=Black

let b:current_syntax = "ods"

