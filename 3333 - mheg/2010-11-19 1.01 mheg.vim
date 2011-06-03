" Vim syntax file
" Language: MHEG-5 DVB
" Maintainer: Paul Banks (http://paulbanks.org/)
" Latest Revision: 20th November 2010
"

if exists("b:current_syntax")
    finish
endif

syn case ignore

syn region mhg_regionBlock start='{' end='}' fold transparent

syn keyword mhgRoot Application Scene

syn keyword mhgSceneAttr InputEventReg SceneCS AspectRatio MovingCursor NextScenes

syn keyword mhgApplicationAttr OnSpawnCloseDown OnRestart

syn keyword mhgDefAttr CharacterSet BackgroundColour TextCHook TextColour Font FontAttributes InterchgPrgHook StreamCHook BitmapCHook LineArtCHook ButtonRefColour HighlightRefColour SliderRefColour

syn keyword mhgGroupAttr StdID StdVersion ObjectInfo OnStartUp OnCloseDown Items

syn keyword mhgItems ResidentPrg RemotePrg InterchgPrg Palette Font CursorShape BooleanVar IntegerVar OStringVar ObjectRefVar ContentRefVar Link Stream Bitmap LineArt DynamicLineArt Rectangle Hotspot SwitchButton PushButton Text EntryField HyperText Slider TokenGroup ListGroup

syn keyword mhgRemotePrgAttr ConnectionTag

syn keyword mhgIngrAttr InitiallyActive CHook OrigContent Shared

syn keyword mhgLinkAttr EventSource EventType LinkEffect EventData 

syn keyword mhgEventTypeConst IsAvailable ContentAvailable IsDeleted IsRunning IsStopped UserInput AnchorFired TimerFired AsynchStopped InteractionCompleted TokenMovedFrom TokenMovedTo StreamEvent StreamPlaying StreamStopped CounterTrigger HighlightOn HighlightOff CursorEnter CursorLeave IsSelected IsDeselected TestEvent FirstItemPresented LastItemPresented HeadItems TailItems ItemSelected ItemDeselected EntryFieldFull EngineEvent FocusMoved SliderValueChanged

syn keyword mhgActions Activate Add AddItem Append BringToFront Call CallActionSlot Clear Clone CloseConnection Deactivate DelItem Deselect DeselectItem Divide DrawArc DrawLine DrawOval DrawPolygon DrawPolyline DrawRectangle DrawSector Fork GetAvailabilityStatus GetBoxSize GetCellItem GetCursorPosition GetEngineSupport GetEntryPoint GetFillColour GetFirstItem GetHighlightStatus GetInteractionStatus GetItemStatus GetLabel GetLastAnchorFired GetLineColour GetLineStyle GetLineWidth GetListItem GetListSize GetOverwriteMode GetPortion GetPosition GetRunningStatus GetSelectionStatus GetSliderValue GetTextContent GetTextData GetTokenPosition GetVolume Launch LockScreen Modulo Move MoveTo Multiply OpenConnection Preload PutBefore PutBehind Quit ReadPersistent Run ScaleBitmap ScaleVideo ScrollItems Select SelectItem SendEvent SendToBack SetBoxSize SetCachePriority SetCounterEndPosition SetCounterPosition SetCounterTrigger SetCursorPosition SetCursorShape SetData SetEntryPoint SetFillColour SetFirstItem SetFontRef SetHighlightStatus SetInteractionStatus SetLabel SetLineColour SetLineStyle SetLineWidth SetOverwriteMode SetPaletteRef SetPortion SetPosition SetSliderValue SetSpeed SetTimer SetTransparency SetVariable SetVolume Spawn Step Stop StorePersistent Subtract TestVariable Toggle ToggleItem TransitionTo Unload UnlockScreen SetBackgroundColour SetCellPosition SetInputReg SetTextColour SetFontAttributes SetVideoDecodeOffset GetVideoDecodeOffset GetFocusPosition SetFocusPosition SetBitmapDecodeOffset GetBitmapDecodeOffset SetSliderParameters

syn keyword mhgTokenGroupAttr TokenGroupItems NoTokenActionSlots
syn keyword mhgTokenGroupItemAttr ActionSlots

syn keyword mhgVariableAttr OrigValue ObjectRef ContentRef

syn keyword mhgVisibleAttr OrigBoxSize OrigPosition OrigPaletteRef

syn keyword mhgTextAttr OrigFont HJustification VJustification LineOrientation StartCorner TextWrapping

syn keyword mhgProgramAttr Name InitiallyAvailable

syn keyword mhgTextHJustEnum start end centre justified
syn keyword mhgTextLineOrientEnum vertical horizontal
syn keyword mhgTextStartCornerEnum upper-left upper-right lower-left lower-right

syn keyword mhgLineArtBodyAttr BBBox OrigLineWidth OrigLineStyle OrigRefLineColour OrigRefFillColour

syn keyword mhgOper NewAbsoluteColour NewColourIndex NewCCPriority NewContentSize GBoolean GInteger GOctetString GObjectRef GContentRef NewRefContent IndirectRef

syn keyword mhgBool true false

syn match mhgNumber '\d\+'
syn region mhgString start='\'' end='\''

syn match mhgComment '\/\/.*$' 

hi def link mhgComment Comment
hi def link mhgActions Statement
hi def link mhgRoot PreProc
hi def link mhgItems Type
hi def link mhgTokenGroupAttr Identifier
hi def link mhgLineArtBodyAttr Identifier
hi def link mhgProgramAttr Identifier
hi def link mhgTokenGroupItemAttr Identifier
hi def link mhgSceneAttr Identifier
hi def link mhgTextAttr Identifier
hi def link mhgVariableAttr Identifier
hi def link mhgVisibleAttr Identifier
hi def link mhgDefAttr Identifier
hi def link mhgLinkAttr Identifier
hi def link mhgGroupAttr Identifier
hi def link mhgRemotePrgAttr Identifier
hi def link mhgIngrAttr Identifier
hi def link mhgEventTypeConst Constant
hi def link mhgTextHJustEnum Constant
hi def link mhgOper PreProc
hi def link mhgTextLineOrientEnum Constant
hi def link mhgTextStartCornerEnum Constant
hi def link mhgBool Constant

hi def link mhgNumber Number
hi def link mhgString String


let b:current_syntax = "mheg"

