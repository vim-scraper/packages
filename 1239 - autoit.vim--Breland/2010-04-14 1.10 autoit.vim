" Vim syntax file
"
" Language:		AutoIt v3 (http://www.autoitscript.com/autoit3/)
" Maintainer:	Jared Breland <jbreland@legroom.net>
" Authored By:	Riccardo Casini <ric@libero.it>
" URL:			http://www.vim.org/scripts/script.php?script_id=1239
" ChangeLog:	Please visit the script URL for detailed change information
" 	v1.10 04/14/10 by Jared Breland <jbreland@legroom.net>
" 		updated for AutoIt 3.3.6.0
" 	v1.9 05/17/09 by Jared Breland <jbreland@legroom.net>
" 		updated for AutoIt 3.3.0.0
" 		added the MANY new constants and UDFs added in recent versions
" 	v1.8 10/09/08 by Jared Breland <jbreland@legroom.net>
" 		updated for AutoIt 3.2.12.1
" 	v1.7 02/24/08 by Jared Breland <jbreland@legroom.net>
" 		updated for AutoIt 3.2.10.0
" 	v1.6 06/10/07 by Jared Breland <jbreland@legroom.net>
" 		updated for AutoIt 3.2.4.9
" 	v1.5 01/10/07 by Jared Breland <jbreland@legroom.net>
" 		updated for AutoIt 3.2.2.0
" 		updated constant and option strings to recognize ' and " quotes
" 		more cleanup for inclusion in Vim distribution
" 	v1.4 08/27/06 by Jared Breland <jbreland@legroom.net>
" 		update for AutoIt 3.2.0.1
" 		cleanup for inclusion in Vim distribution
" 	v1.3 05/13/06 by Jared Breland <jbreland@legroom.net>
" 		update for AutoIt 3.1.1.123-beta
" 		added Styles section
" 		added Constants section
" 		added Send Key section
" 		changed variable formatting to match PHP style
" 		(to better distinguish between user vars and built-ins)
"	v1.2 10/07/05 by Jared Breland <jbreland@legroom.net>
" 		update for AutoIt 3.1.1.78-beta
" 		added Options section
" 	v1.1 03/15/05 by Jared Breland <jbreland@legroom.net>
" 		updated for AutoIt 3.1.0

" AutoIt is not case dependent
syn case ignore

" Definitions for AutoIt reserved keywords
syn keyword autoitKeyword Default False True
syn keyword autoitKeyword Const Dim Global Local ReDim Static
syn keyword autoitKeyword If Else ElseIf Then EndIf
syn keyword autoitKeyword Select Switch Case EndSelect EndSwitch
syn keyword autoitKeyword Enum For In To Step Next
syn keyword autoitKeyword With While EndWith Wend Do Until
syn keyword autoitKeyword ContinueCase ContinueLoop ExitLoop Exit

" inside script inclusion and global options
syn match autoitIncluded display contained "<[^>]*>"
syn match autoitInclude	display "^\s*#\s*include\>\s*["<]"
	\ contains=autoitIncluded,autoitString
syn match autoitInclude "^\s*#include-once\>"
syn match autoitInclude "^\s*#NoAutoIt3Execute\>"
syn match autoitInclude "^\s*#NoTrayIcon\>"
syn match autoitInclude "^\s*#OnAutoItStartRegister\>"
syn match autoitInclude "^\s*#RequireAdmin\>"

" user-defined functions
syn keyword autoitKeyword Func ByRef EndFunc Return

" built-in functions
" environment management
syn keyword autoitFunction ClipGet ClipPut EnvGet EnvSet EnvUpdate MemGetStats
" file, directory, and disk management
syn keyword autoitFunction ConsoleRead ConsoleWrite ConsoleWriteError
syn keyword autoitFunction DirCopy DirCreate DirGetSize DirMove DirRemove
syn keyword autoitFunction DriveGetDrive DriveGetFileSystem DriveGetLabel
	\ DriveGetSerial DriveGetType DriveMapAdd DriveMapDel DriveMapGet
	\ DriveSetLabel DriveSpaceFree DriveSpaceTotal DriveStatus
syn keyword autoitFunction FileChangeDir FileClose FileCopy FileCreateNTFSLink
	\ FileCreateShortcut FileDelete FileExists FileFindFirstFile
	\ FileFindNextFile FileFlush FileGetAttrib FileGetEncoding FileGetLongName
	\ FileGetPos FileGetShortcut FileGetShortName FileGetSize FileGetTime
	\ FileGetVersion FileInstall FileMove FileOpen FileOpenDialog FileRead
	\ FileReadLine FileRecycle FileRecycleEmpty FileSaveDialog FileSelectFolder
	\ FileSetAttrib FileSetPos FileSetTime FileWrite FileWriteLine
syn keyword autoitFunction IniDelete IniRead IniReadSection IniReadSectionNames
	\ IniRenameSection IniWrite IniWriteSection
syn keyword autoitFunction StderrRead StdioClose StdinWrite StdoutRead
" graphic and sound
syn keyword autoitFunction Beep PixelChecksum PixelGetColor PixelSearch
	\ SoundPlay SoundSetWaveVolume
" gui reference
syn keyword autoitFunction GUICreate GUIDelete GUICtrlGetHandle GUICtrlGetState
	\ GUICtrlRead GUICtrlRecvMsg GUICtrlSendMsg GUICtrlSendToDummy
	\ GUIGetCursorInfo GUIGetMsg GUIGetStyle GUIRegisterMsg GUIStartGroup GUISwitch
syn keyword autoitFunction GUICtrlCreateAvi GUICtrlCreateButton
	\ GUICtrlCreateCheckbox GUICtrlCreateCombo GUICtrlCreateContextMenu
	\ GUICtrlCreateDate GUICtrlCreateDummy GUICtrlCreateEdit
	\ GUICtrlCreateGraphic GUICtrlCreateGroup GUICtrlCreateIcon
	\ GUICtrlCreateInput GUICtrlCreateLabel GUICtrlCreateList
	\ GUICtrlCreateListView GUICtrlCreateListViewItem GUICtrlCreateMenu
	\ GUICtrlCreateMenuItem GUICtrlCreateMonthCal GUICtrlCreateObj
	\ GUICtrlCreatePic GUICtrlCreateProgress GUICtrlCreateRadio
	\ GUICtrlCreateSlider GUICtrlCreateTab GUICtrlCreateTabItem
	\ GUICtrlCreateTreeView GUICtrlCreateTreeViewItem
	\ GUICtrlCreateUpDown GUICtrlDelete
syn keyword autoitFunction GUICtrlRegisterListViewSort GUISetAccelerators
	\ GUICtrlSetBkColor GUICtrlSetColor GUICtrlSetCursor GUICtrlSetData
	\ GUICtrlSetDefBkColor GUICtrlSetDefColor GUICtrlSetFont GUICtrlSetGraphic
	\ GUICtrlSetImage GUICtrlSetLimit GUICtrlSetOnEvent GUICtrlSetPos
	\ GUICtrlSetResizing GUICtrlSetState GUICtrlSetStyle GUICtrlSetTip
syn keyword autoitFunction GUISetBkColor GUISetCoord GUISetCursor GUISetFont
	\ GUISetHelp GUISetIcon GUISetOnEvent GUISetState
" keyboard control
syn keyword autoitFunction HotKeySet Send SendKeepActive
" math
syn keyword autoitFunction Abs ACos ASin ATan BitAND BitNOT BitOR BitRotate
	\ BitShift BitXOR Cos Ceiling Exp Floor Log Mod Random Round Sin Sqrt
	\ SRandom Tan
" message boxes and dialogs
syn keyword autoitFunction InputBox MsgBox ProgressOff ProgressOn ProgressSet
	\ SplashImageOn SplashOff SplashTextOn ToolTip
" miscellaneous
syn keyword autoitFunction AutoItSetOption AutoItWinGetTitle AutoItWinSetTitle
	\ BlockInput Break Call CDTray Execute Opt SetError SetExtended VarGetType
" mouse control
syn keyword autoitFunction MouseClick MouseClickDrag MouseDown MouseGetCursor
	\ MouseGetPos MouseMove MouseUp MouseWheel
" network
syn keyword autoitFunction FtpSetProxy HttpSetProxy HttpSetUserAgent InetClose
	\ InetGet InetGetInfo InetGetSize InetRead Ping TCPAccept TCPCloseSocket
	\ TCPConnect TCPListen TCPNameToIp TCPRecv TCPSend TCPShutDown TCPStartup
	\ UDPBind UDPCloseSocket UDPOpen UDPRecv UDPSend UDPShutdown UDPStartup
" obj/com reference
syn keyword autoitFunction ObjCreate ObjEvent ObjGet ObjName
" process management
syn keyword autoitFunction DllCall DllCallbackFree DllCallbackGetPtr
	\ DllCallbackRegister DllClose DllOpen DllStructCreate DllStructGetData
	\ DllStructGetPtr DllStructGetSize DllStructSetData ProcessClose
	\ ProcessExists ProcessGetStats ProcessSetPriority ProcessList ProcessWait
	\ ProcessWaitClose Run RunAs RunAsWait RunWait ShellExecute ShellExecuteWait Shutdown
	" removed from 3.2.0 docs - PluginClose PluginOpen
" registry management
syn keyword autoitFunction RegDelete RegEnumKey RegEnumVal RegRead RegWrite
" string management
syn keyword autoitFunction StringAddCR StringCompare StringFormat
	\ StringFromASCIIArray StringInStr StringIsAlNum StringIsAlpha
	\ StringIsASCII StringIsDigit StringIsFloat StringIsInt StringIsLower
	\ StringIsSpace StringIsUpper StringIsXDigit StringLeft StringLen
	\ StringLower StringMid StringRegExp StringRegExpReplace StringReplace
	\ StringRight StringStripCR StringStripWS StringToASCIIArray StringTrimLeft
	\ StringTrimRight StringUpper
" timer and delay
syn keyword autoitFunction Sleep TimerInit TimerDiff
" tray
syn keyword autoitFunction TrayCreateItem TrayCreateMenu TrayItemDelete
	\ TrayItemGetHandle TrayItemGetState TrayItemGetText TrayItemSetOnEvent
	\ TrayItemSetState TrayItemSetText TrayGetMsg TraySetClick TraySetIcon
	\ TraySetOnEvent TraySetPauseIcon TraySetState TraySetToolTip TrayTip
" variables and conversions
syn keyword autoitFunction Asc AscW Assign Binary BinaryLen BinaryMid
	\ BinaryToString Chr ChrW Dec Eval Hex HWnd Int IsAdmin IsArray IsBinary
	\ IsBool IsDeclared IsDllStruct IsFloat IsHWnd IsInt IsKeyword IsNumber
	\ IsObj IsString Number String StringToBinary UBound
" window management
syn keyword autoitFunction WinActivate WinActive WinClose WinExists WinFlash
	\ WinGetCaretPos WinGetClassList WinGetClientSize WinGetHandle WinGetPos
	\ WinGetProcess WinGetState WinGetText WinGetTitle WinKill WinList
	\ WinMenuSelectItem WinMinimizeAll WinMinimizeAllUndo WinMove
	\ WinSetOnTop WinSetState WinSetTitle WinSetTrans WinWait WinWaitActive
	\ WinWaitClose WinWaitNotActive
syn keyword autoitFunction ControlClick ControlCommand ControlDisable
	\ ControlEnable ControlFocus ControlGetFocus ControlGetHandle
	\ ControlGetPos ControlGetText ControlHide ControlListView ControlMove
	\ ControlSend ControlSetText ControlShow ControlTreeView StatusBarGetText

" user defined functions
" array
syn keyword autoitFunction _ArrayAdd _ArrayBinarySearch _ArrayCombinations
	\ _ArrayConcatenate _ArrayDelete _ArrayDisplay _ArrayFindAll _ArrayInsert
	\ _ArrayMax _ArrayMaxIndex _ArrayMin _ArrayMinIndex _ArrayPermute _ArrayPop
	\ _ArrayPush _ArrayReverse _ArraySearch _ArraySort _ArraySwap _ArrayToClip
	\ _ArrayToString _ArrayTrim _ArrayUnique
" clipboard
syn keyword autoitFunction _ClipBoard_ChangeChain _ClipBoard_Close
	\ _ClipBoard_CountFormats _ClipBoard_Empty _ClipBoard_EnumFormats
	\ _ClipBoard_FormatStr _ClipBoard_GetData _ClipBoard_GetDataEx
	\ _ClipBoard_GetFormatName _ClipBoard_GetOpenWindow _ClipBoard_GetOwner
	\ _ClipBoard_GetPriorityFormat _ClipBoard_SequenceNumber
	\ _ClipBoard_GetViewer _ClipBoard_IsFormatAvailable _ClipBoard_Open
	\ _ClipBoard_Open _ClipBoard_RegisterFormat _ClipBoard_SetData
	\ _ClipBoard_SetDataEx _ClipBoard_SetViewer
" color
syn keyword autoitFunction _ColorConvertHSLtoRGB _ColorConvertRGBtoHSL
	\ _ColorgetBlue _ColorGetGreen _ColorGetRed
" crypt
syn keyword autoitFunction _Crypt_Startup _Crypt_Shutdown _Crypt_DecryptData
	\ _Crypt_DecryptFile _Crypt_DeriveKey _Crypt_DestroyKey _Crypt_EncryptData
	\ _Crypt_EncryptFile _Crypt_HashData _Crypt_HashFile
" date
syn keyword autoitFunction _Date_Time_CompareFileName
	\ _Date_Time_DOSDateTimeToArray _Date_Time_DOSDateTimeToFileTime
	\ _Date_Time_DOSDateTimeToStr _Date_Time_DOSDateToArray
	\ _Date_Time_DOSDateToStr _Date_Time_DOSTimeToArray _Date_Time_DOSTimeToStr
	\ _Date_Time_EncodeFileTime _Date_Time_EncodeSystemTime
	\ _Date_Time_FileTimeToArray _Date_Time_FileTimeToDOSDateTime
	\ _Date_Time_FileTimeToLocalFileTime _Date_Time_FileTimeToStr
	\ _Date_Time_FileTimeToStr _Date_Time_FileTimeToSystemTime
	\ _Date_Time_GetFileTime _Date_Time_GetLocalTime
	\ _Date_Time_GetSystemTimeAdjustment _Date_Time_GetSystemTimeAsFileTime
	\ _Date_Time_GetSystemTimes _Date_Time_GetTickCount
	\ _Date_Time_GetTimeZoneInformation _Date_Time_LocalFileTimeToFileTime
	\ _Date_Time_SetFileTime _Date_Time_SetLocalTime _Date_Time_SetSystemTime
	\ _Date_Time_SetSystemTimeAdjustment _Date_Time_SetTimeZoneInformation
	\ _Date_Time_SystemTimeToArray _Date_Time_SystemTimeToDateStr
	\ _Date_Time_SystemTimeToDateTimeStr _Date_Time_SystemTimeToFileTime
	\ _Date_Time_SystemTimeToTimeStr _Date_Time_SystemTimeToTzSpecificLocalTime
	\ _Date_Time_TzSpecificLocalTimeToSystemTime _DateAdd _DateDayOfWeek
	\ _DateDaysInMonth _DateDiff _DateIsLeapYear _DateIsValid _DateTimeFormat
	\ _DateTimeSplit _DateToDayOfWeek _ToDayOfWeekISO _DateToDayValue
	\ _DateToMonth _DayValueToDate _Now _NowCalc _NowCalcDate _NowDate _NowTime
	\ _SetDate _SetTime _TicksToTime _TimeToTicks _WeekNumberISO
" debug
syn keyword autoitFunction _Assert _DebugBugReportEnv _DebugOut _DebugSetup
" eventlog
syn keyword autoitFunction _EventLog__Backup _EventLog__Clear _EventLog__Close
	\ _EventLog__Count _EventLog__DeregisterSource _EventLog__Full
	\ _EventLog__Notify _EventLog__Oldest _EventLog__Open _EventLog__OpenBackup
	\ _EventLog__Read _EventLog__RegisterSource _EventLog__Report
" excel
syn keyword autoitFunction _ExcelBookAttach _ExcelBookClose _ExcelBookNew
	\ _ExcelBookOpen _ExcelBookSave _ExcelBookSaveAs _ExcelColumnDelete
	\ _ExcelColumnInsert _ExcelFontSetProperties _ExcelHorizontalAlignSet
	\ _ExcelHyperlinkInsert _ExcelNumberFormat _ExcelReadArray _ExcelReadCell
	\ _ExcelReadSheetToArray _ExcelRowDelete _ExcelRowInsert _ExcelSheetActivate
	\ _ExcelSheetAddNew _ExcelSheetDelete _ExcelSheetList _ExcelSheetMove
	\ _ExcelSheetNameGet _ExcelSheetNameSet _ExcelWriteArray _ExcelWriteCell
	\ _ExcelWriteFormula _ExcelWriteSheetFromArray
" file
syn keyword autoitFunction _FileCountLines _FileCreate _FileListToArray
	\ _FilePrint _FileReadToArray _FileWriteFromArray _FileWriteLog
	\ _FileWriteToLine _PathFull _PathGetRelative _PathMake _PathSplit
	\ _ReplaceStringInFile _TempFile
" ftpex
syn keyword autoitFunction _FTP_Close _FTP_Command _FTP_Connect _FTP_DecodeInternetStatus
	\ _FTP_DirCreate _FTP_DirDelete _FTP_DirGetCurrent _FTP_DirPutContents
	\ _FTP_DirSetCurrent _FTP_FileClose _FTP_FileDelete _FTP_FileGet
	\ _FTP_FileGetSize _FTP_FileOpen _FTP_FilePut _FTP_FileRead _FTP_FileRename
	\ _FTP_FileTimeLoHiToStr _FTP_FindFileClose _FTP_FindFileFirst
	\ _FTP_FindFileNext _FTP_GetLastResponseInfo _FTP_ListToArray
	\ _FTP_ListToArray2D _FTP_ListToArrayEx _FTP_Open _FTP_ProgressDownload
	\ _FTP_ProgressUpload _FTP_SetStatusCallback
" gdiplus
syn keyword autoitFunction _GDIPlus_ArrowCapCreate _GDIPlus_ArrowCapDispose
	\ _GDIPlus_ArrowCapGetFillState _GDIPlus_ArrowCapGetHeight
	\ _GDIPlus_ArrowCapGetMiddleInset _GDIPlus_ArrowCapGetWidth
	\ _GDIPlus_ArrowCapSetFillState _GDIPlus_ArrowCapSetHeight
	\ _GDIPlus_ArrowCapSetMiddleInset _GDIPlus_ArrowCapSetWidth
	\ _GDIPlus_BitmapCloneArea _GDIPlus_BitmapCreateFromFile
	\ _GDIPlus_BitmapCreateFromGraphics _GDIPlus_BitmapCreateFromHBITMAP
	\ _GDIPlus_BitmapCreateHBITMAPFromBitmap _GDIPlus_BitmapDispose
	\ _GDIPlus_BitmapLockBits _GDIPlus_BitmapUnlockBits _GDIPlus_BrushClone
	\ _GDIPlus_BrushCreateSolid _GDIPlus_BrushDispose
	\ _GDIPlus_BrushGetSolidColor _GDIPlus_BrushGetType
	\ _GDIPlus_BrushSetSolidColor _GDIPlus_CustomLineCapDispose
	\ _GDIPlus_Decoders _GDIPlus_DecodersGetCount _GDIPlus_DecodersGetSize
	\ _GDIPlus_DrawImagePoints _GDIPlus_Encoders _GDIPlus_EncodersGetCLSID
	\ _GDIPlus_EncodersGetCount _GDIPlus_EncodersGetParamList
	\ _GDIPlus_EncodersGetParamListSize _GDIPlus_EncodersGetSize
	\ _GDIPlus_FontCreate _GDIPlus_FontDispose _GDIPlus_FontFamilyCreate
	\ _GDIPlus_FontFamilyDispose _GDIPlus_GraphicsClear
	\ _GDIPlus_GraphicsCreateFromHDC _GDIPlus_GraphicsCreateFromHWND
	\ _GDIPlus_GraphicsDispose _GDIPlus_GraphicsDrawArc
	\ _GDIPlus_GraphicsDrawBezier _GDIPlus_GraphicsDrawClosedCurve
	\ _GDIPlus_GraphicsDrawCurve _GDIPlus_GraphicsDrawEllipse
	\ _GDIPlus_GraphicsDrawImage _GDIPlus_GraphicsDrawImageRect
	\ _GDIPlus_GraphicsDrawImageRectRect _GDIPlus_GraphicsDrawLine
	\ _GDIPlus_GraphicsDrawPie _GDIPlus_GraphicsDrawPolygon
	\ _GDIPlus_GraphicsDrawRect _GDIPlus_GraphicsDrawString
	\ _GDIPlus_GraphicsDrawStringEx _GDIPlus_GraphicsFillClosedCurve
	\ _GDIPlus_GraphicsFillEllipse _GDIPlus_GraphicsFillPie
	\ _GDIPlus_GraphicsFillPolygon _GDIPlus_GraphicsFillRect
	\ _GDIPlus_GraphicsGetDC _GDIPlus_GraphicsGetSmoothingMode
	\ _GDIPlus_GraphicsMeasureString _GDIPlus_GraphicsReleaseDC
	\ _GDIPlus_GraphicsSetSmoothingMode _GDIPlus_GraphicsSetTransform
	\ _GDIPlus_ImageDispose _GDIPlus_ImageGetFlags
	\ _GDIPlus_ImageGetGraphicsContext _GDIPlus_ImageGetHeight
	\ _GDIPlus_ImageGetHorizontalResolution _GDIPlus_ImageGetPixelFormat
	\ _GDIPlus_ImageGetRawFormat _GDIPlus_ImageGetType
	\ _GDIPlus_ImageGetVerticalResolution _GDIPlus_ImageGetWidth
	\ _GDIPlus_ImageLoadFromFile _GDIPlus_ImageSaveToFile
	\ _GDIPlus_ImageSaveToFileEx _GDIPlus_MatrixCreate _GDIPlus_MatrixDispose
	\ _GDIPlus_MatrixRotate _GDIPlus_MatrixScale _GDIPlus_MatrixTranslate
	\ _GDIPlus_ParamAdd _GDIPlus_ParamInit _GDIPlus_PenCreate
	\ _GDIPlus_PenDispose _GDIPlus_PenGetAlignment _GDIPlus_PenGetColor
	\ _GDIPlus_PenGetCustomEndCap _GDIPlus_PenGetDashCap
	\ _GDIPlus_PenGetDashStyle _GDIPlus_PenGetEndCap _GDIPlus_PenGetWidth
	\ _GDIPlus_PenSetAlignment _GDIPlus_PenSetColor _GDIPlus_PenSetCustomEndCap
	\ _GDIPlus_PenSetDashCap _GDIPlus_PenSetDashStyle _GDIPlus_PenSetEndCap
	\ _GDIPlus_PenSetWidth _GDIPlus_RectFCreate _GDIPlus_Shutdown
	\ _GDIPlus_Startup _GDIPlus_StringFormatCreate _GDIPlus_StringFormatDispose
	\ _GDIPlus_StringFormatSetAlign
" guiavi
syn keyword autoitFunction _GUICtrlAVI_Close _GUICtrlAVI_Create\
	\ _GUICtrlAVI_Destroy _GUICtrlAVI_IsPlaying _GUICtrlAVI_Open
	\ _GUICtrlAVI_OpenEx _GUICtrlAVI_Play _GUICtrlAVI_Seek _GUICtrlAVI_Show
	\ _GUICtrlAVI_Stop
" guibutton
syn keyword autoitFunction _GUICtrlButton_Click _GUICtrlButton_Create
	\ _GUICtrlButton_Destroy _GUICtrlButton_Enable _GUICtrlButton_GetCheck
	\ _GUICtrlButton_GetFocus _GUICtrlButton_GetIdealSize
	\ _GUICtrlButton_GetImage _GUICtrlButton_GetImageList _GUICtrlButton_GetNote
	\ _GUICtrlButton_GetNoteLength _GUICtrlButton_GetSplitInfo
	\ _GUICtrlButton_GetState _GUICtrlButton_GetText
	\ _GUICtrlButton_GetTextMargin _GUICtrlButton_SetCheck
	\ _GUICtrlButton_SetDontClick _GUICtrlButton_SetFocus
	\ _GUICtrlButton_SetImage _GUICtrlButton_SetImageList _GUICtrlButton_SetNote
	\ _GUICtrlButton_SetShield _GUICtrlButton_SetSize
	\ _GUICtrlButton_SetSplitInfo _GUICtrlButton_SetState
	\ _GUICtrlButton_SetStyle _GUICtrlButton_SetText
	\ _GUICtrlButton_SetTextMargin _GUICtrlButton_Show
" guicombobox
syn keyword autoitFunction _GUICtrlComboBox_AddDir _GUICtrlComboBox_AddString
	\ _GUICtrlComboBox_AutoComplete _GUICtrlComboBox_BeginUpdate
	\ _GUICtrlComboBox_Create _GUICtrlComboBox_DeleteString
	\ _GUICtrlComboBox_Destroy _GUICtrlComboBox_EndUpdate
	\ _GUICtrlComboBox_FindString _GUICtrlComboBox_FindStringExact
	\ _GUICtrlComboBox_GetComboBoxInfo _GUICtrlComboBox_GetCount
	\ _GUICtrlComboBox_GetCueBanner _GUICtrlComboBox_GetCurSel
	\ _GUICtrlComboBox_GetDroppedControlRect
	\ _GUICtrlComboBox_GetDroppedControlRectEx _GUICtrlComboBox_GetDroppedState
	\ _GUICtrlComboBox_GetDroppedWidth _GUICtrlComboBox_GetEditSel
	\ _GUICtrlComboBox_GetEditText _GUICtrlComboBox_GetExtendedUI
	\ _GUICtrlComboBox_GetHorizontalExtent _GUICtrlComboBox_GetItemHeight
	\ _GUICtrlComboBox_GetLBText _GUICtrlComboBox_GetLBTextLen
	\ _GUICtrlComboBox_GetList _GUICtrlComboBox_GetListArray
	\ _GUICtrlComboBox_GetLocale _GUICtrlComboBox_GetLocaleCountry
	\ _GUICtrlComboBox_GetLocaleLang _GUICtrlComboBox_GetLocalePrimLang
	\ _GUICtrlComboBox_GetLocaleSubLang _GUICtrlComboBox_GetMinVisible
	\ _GUICtrlComboBox_GetTopIndex _GUICtrlComboBox_InitStorage
	\ _GUICtrlComboBox_InsertString _GUICtrlComboBox_LimitText
	\ _GUICtrlComboBox_ReplaceEditSel _GUICtrlComboBox_ResetContent
	\ _GUICtrlComboBox_SelectString _GUICtrlComboBox_SetCueBanner
	\ _GUICtrlComboBox_SetCurSel _GUICtrlComboBox_SetDroppedWidth
	\ _GUICtrlComboBox_SetEditSel _GUICtrlComboBox_SetEditText
	\ _GUICtrlComboBox_SetExtendedUI _GUICtrlComboBox_SetHorizontalExtent
	\ _GUICtrlComboBox_SetItemHeight _GUICtrlComboBox_SetMinVisible
	\ _GUICtrlComboBox_SetTopIndex _GUICtrlComboBox_ShowDropDown
" guicomboboxex
syn keyword autoitFunction _GUICtrlComboBoxEx_AddDir
	\ _GUICtrlComboBoxEx_AddString _GUICtrlComboBoxEx_BeginUpdate
	\ _GUICtrlComboBoxEx_Create _GUICtrlComboBoxEx_CreateSolidBitMap
	\ _GUICtrlComboBoxEx_DeleteString _GUICtrlComboBoxEx_Destroy
	\ _GUICtrlComboBoxEx_EndUpdate _GUICtrlComboBoxEx_FindStringExact
	\ _GUICtrlComboBoxEx_GetComboBoxInfo _GUICtrlComboBoxEx_GetComboControl
	\ _GUICtrlComboBoxEx_GetCount _GUICtrlComboBoxEx_GetCurSel
	\ _GUICtrlComboBoxEx_GetDroppedControlRect
	\ _GUICtrlComboBoxEx_GetDroppedControlRectEx
	\ _GUICtrlComboBoxEx_GetDroppedState _GUICtrlComboBoxEx_GetDroppedWidth
	\ _GUICtrlComboBoxEx_GetEditControl _GUICtrlComboBoxEx_GetEditSel
	\ _GUICtrlComboBoxEx_GetEditText _GUICtrlComboBoxEx_GetExtendedStyle
	\ _GUICtrlComboBoxEx_GetExtendedUI _GUICtrlComboBoxEx_GetImageList
	\ _GUICtrlComboBoxEx_GetItem _GUICtrlComboBoxEx_GetItemEx
	\ _GUICtrlComboBoxEx_GetItemHeight _GUICtrlComboBoxEx_GetItemImage
	\ _GUICtrlComboBoxEx_GetItemIndent _GUICtrlComboBoxEx_GetItemOverlayImage
	\ _GUICtrlComboBoxEx_GetItemParam _GUICtrlComboBoxEx_GetItemSelectedImage
	\ _GUICtrlComboBoxEx_GetItemText _GUICtrlComboBoxEx_GetItemTextLen
	\ _GUICtrlComboBoxEx_GetList _GUICtrlComboBoxEx_GetListArray
	\ _GUICtrlComboBoxEx_GetLocale _GUICtrlComboBoxEx_GetLocaleCountry
	\ _GUICtrlComboBoxEx_GetLocaleLang _GUICtrlComboBoxEx_GetLocalePrimLang
	\ _GUICtrlComboBoxEx_GetLocaleSubLang _GUICtrlComboBoxEx_GetMinVisible
	\ _GUICtrlComboBoxEx_GetTopIndex _GUICtrlComboBoxEx_GetUnicode
	\ _GUICtrlComboBoxEx_InitStorage _GUICtrlComboBoxEx_InsertString
	\ _GUICtrlComboBoxEx_LimitText _GUICtrlComboBoxEx_ReplaceEditSel
	\ _GUICtrlComboBoxEx_ResetContent _GUICtrlComboBoxEx_SetCurSel
	\ _GUICtrlComboBoxEx_SetDroppedWidth _GUICtrlComboBoxEx_SetEditSel
	\ _GUICtrlComboBoxEx_SetEditText _GUICtrlComboBoxEx_SetExtendedStyle
	\ _GUICtrlComboBoxEx_SetExtendedUI _GUICtrlComboBoxEx_SetImageList
	\ _GUICtrlComboBoxEx_SetItem _GUICtrlComboBoxEx_SetItemEx
	\ _GUICtrlComboBoxEx_SetItemHeight _GUICtrlComboBoxEx_SetItemImage
	\ _GUICtrlComboBoxEx_SetItemIndent _GUICtrlComboBoxEx_SetItemOverlayImage
	\ _GUICtrlComboBoxEx_SetItemParam _GUICtrlComboBoxEx_SetItemSelectedImage
	\ _GUICtrlComboBoxEx_SetMinVisible _GUICtrlComboBoxEx_SetTopIndex
	\ _GUICtrlComboBoxEx_SetUnicode _GUICtrlComboBoxEx_ShowDropDown
" guidatetimepicker
syn keyword autoitFunction _GUICtrlDTP_Create _GUICtrlDTP_Destroy
	\ _GUICtrlDTP_GetMCColor _GUICtrlDTP_GetMCFont _GUICtrlDTP_GetMonthCal
	\ _GUICtrlDTP_GetRange _GUICtrlDTP_GetRangeEx _GUICtrlDTP_GetSystemTime
	\ _GUICtrlDTP_GetSystemTimeEx _GUICtrlDTP_SetFormat _GUICtrlDTP_SetMCColor
	\ _GUICtrlDTP_SetMCFont _GUICtrlDTP_SetRange _GUICtrlDTP_SetRangeEx
	\ _GUICtrlDTP_SetSystemTime _GUICtrlDTP_SetSystemTimeEx
" guiedit
syn keyword autoitFunction _GUICtrlEdit_AppendText _GUICtrlEdit_BeginUpdate
	\ _GUICtrlEdit_CanUndo _GUICtrlEdit_CharFromPos _GUICtrlEdit_Create
	\ _GUICtrlEdit_Destroy _GUICtrlEdit_EmptyUndoBuffer _GUICtrlEdit_EndUpdate
	\ _GUICtrlEdit_Find _GUICtrlEdit_FmtLines _GUICtrlEdit_GetFirstVisibleLine
	\ _GUICtrlEdit_GetLimitText _GUICtrlEdit_GetLine _GUICtrlEdit_GetLineCount
	\ _GUICtrlEdit_GetMargins _GUICtrlEdit_GetModify
	\ _GUICtrlEdit_GetPasswordChar _GUICtrlEdit_GetRECT _GUICtrlEdit_GetRECTEx
	\ _GUICtrlEdit_GetSel _GUICtrlEdit_GetText _GUICtrlEdit_GetTextLen
	\ _GUICtrlEdit_HideBalloonTip _GUICtrlEdit_InsertText
	\ _GUICtrlEdit_LineFromChar _GUICtrlEdit_LineIndex _GUICtrlEdit_LineLength
	\ _GUICtrlEdit_LineScroll _GUICtrlEdit_PosFromChar _GUICtrlEdit_ReplaceSel
	\ _GUICtrlEdit_Scroll _GUICtrlEdit_SetLimitText _GUICtrlEdit_SetMargins
	\ _GUICtrlEdit_SetModify _GUICtrlEdit_SetPasswordChar
	\ _GUICtrlEdit_SetReadOnly _GUICtrlEdit_SetRECT _GUICtrlEdit_SetRECTEx
	\ _GUICtrlEdit_SetRECTNP _GUICtrlEdit_SetRectNPEx _GUICtrlEdit_SetSel
	\ _GUICtrlEdit_SetTabStops _GUICtrlEdit_SetText _GUICtrlEdit_ShowBalloonTip
	\ _GUICtrlEdit_Undo
" guiheader
syn keyword autoitFunction _GUICtrlHeader_AddItem _GUICtrlHeader_ClearFilter
	\ _GUICtrlHeader_ClearFilterAll _GUICtrlHeader_Create
	\ _GUICtrlHeader_CreateDragImage _GUICtrlHeader_DeleteItem
	\ _GUICtrlHeader_Destroy _GUICtrlHeader_EditFilter
	\ _GUICtrlHeader_GetBitmapMargin _GUICtrlHeader_GetImageList
	\ _GUICtrlHeader_GetItem _GUICtrlHeader_GetItemAlign
	\ _GUICtrlHeader_GetItemBitmap _GUICtrlHeader_GetItemCount
	\ _GUICtrlHeader_GetItemDisplay _GUICtrlHeader_GetItemFlags
	\ _GUICtrlHeader_GetItemFormat _GUICtrlHeader_GetItemImage
	\ _GUICtrlHeader_GetItemOrder _GUICtrlHeader_GetItemParam
	\ _GUICtrlHeader_GetItemRect _GUICtrlHeader_GetItemRectEx
	\ _GUICtrlHeader_GetItemText _GUICtrlHeader_GetItemWidth
	\ _GUICtrlHeader_GetOrderArray _GUICtrlHeader_GetUnicodeFormat
	\ _GUICtrlHeader_HitTest _GUICtrlHeader_InsertItem _GUICtrlHeader_Layout
	\ _GUICtrlHeader_OrderToIndex _GUICtrlHeader_SetBitmapMargin
	\ _GUICtrlHeader_SetFilterChangeTimeout _GUICtrlHeader_SetHotDivider
	\ _GUICtrlHeader_SetImageList _GUICtrlHeader_SetItem
	\ _GUICtrlHeader_SetItemAlign _GUICtrlHeader_SetItemBitmap
	\ _GUICtrlHeader_SetItemDisplay _GUICtrlHeader_SetItemFlags
	\ _GUICtrlHeader_SetItemFormat _GUICtrlHeader_SetItemImage
	\ _GUICtrlHeader_SetItemOrder _GUICtrlHeader_SetItemParam
	\ _GUICtrlHeader_SetItemText _GUICtrlHeader_SetItemWidth
	\ _GUICtrlHeader_SetOrderArray _GUICtrlHeader_SetUnicodeFormat
" guiimagelist
syn keyword autoitFunction _GUIImageList_Add _GUIImageList_AddBitmap
	\ _GUIImageList_AddIcon _GUIImageList_AddMasked _GUIImageList_BeginDrag
	\ _GUIImageList_Copy _GUIImageList_Create _GUIImageList_Destroy
	\ _GUIImageList_DestroyIcon _GUIImageList_DragEnter _GUIImageList_DragLeave
	\ _GUIImageList_DragMove _GUIImageList_Draw _GUIImageList_DrawEx
	\ _GUIImageList_Duplicate _GUIImageList_EndDrag _GUIImageList_GetBkColor
	\ _GUIImageList_GetIcon _GUIImageList_GetIconHeight
	\ _GUIImageList_GetIconSize _GUIImageList_GetIconSizeEx
	\ _GUIImageList_GetIconWidth _GUIImageList_GetImageCount
	\ _GUIImageList_GetImageInfoEx _GUIImageList_Remove
	\ _GUIImageList_ReplaceIcon _GUIImageList_SetBkColor
	\ _GUIImageList_SetIconSize _GUIImageList_SetImageCount _GUIImageList_Swap
" guiipaddress
syn keyword autoitFunction _GUICtrlIpAddress_ClearAddress
	\ _GUICtrlIpAddress_Create _GUICtrlIpAddress_Destroy _GUICtrlIpAddress_Get
	\ _GUICtrlIpAddress_GetArray _GUICtrlIpAddress_GetEx
	\ _GUICtrlIpAddress_IsBlank _GUICtrlIpAddress_Set _GUICtrlIpAddress_SetArray
	\ _GUICtrlIpAddress_SetEx _GUICtrlIpAddress_SetFocus
	\ _GUICtrlIpAddress_SetFont _GUICtrlIpAddress_SetRange
	\ _GUICtrlIpAddress_ShowHide
" guilistbox
syn keyword autoitFunction _GUICtrlListBox_AddFile _GUICtrlListBox_AddString
	\ _GUICtrlListBox_BeginUpdate _GUICtrlListBox_ClickItem
	\ _GUICtrlListBox_Create _GUICtrlListBox_DeleteString
	\ _GUICtrlListBox_Destroy _GUICtrlListBox_Dir _GUICtrlListBox_EndUpdate
	\ _GUICtrlListBox_FindInText _GUICtrlListBox_FindString
	\ _GUICtrlListBox_GetAnchorIndex _GUICtrlListBox_GetCaretIndex
	\ _GUICtrlListBox_GetCount _GUICtrlListBox_GetCurSel
	\ _GUICtrlListBox_GetHorizontalExtent _GUICtrlListBox_GetItemData
	\ _GUICtrlListBox_GetItemHeight _GUICtrlListBox_GetItemRect
	\ _GUICtrlListBox_GetItemRectEx _GUICtrlListBox_GetListBoxInfo
	\ _GUICtrlListBox_GetLocale _GUICtrlListBox_GetLocaleCountry
	\ _GUICtrlListBox_GetLocaleLang _GUICtrlListBox_GetLocalePrimLang
	\ _GUICtrlListBox_GetLocaleSubLang _GUICtrlListBox_GetSel
	\ _GUICtrlListBox_GetSelCount _GUICtrlListBox_GetSelItems
	\ _GUICtrlListBox_GetSelItemsText _GUICtrlListBox_GetText
	\ _GUICtrlListBox_GetTextLen _GUICtrlListBox_GetTopIndex
	\ _GUICtrlListBox_InitStorage _GUICtrlListBox_InsertString
	\ _GUICtrlListBox_ItemFromPoint _GUICtrlListBox_ReplaceString
	\ _GUICtrlListBox_ResetContent _GUICtrlListBox_SelectString
	\ _GUICtrlListBox_SelItemRange _GUICtrlListBox_SelItemRangeEx
	\ _GUICtrlListBox_SetAnchorIndex _GUICtrlListBox_SetCaretIndex
	\ _GUICtrlListBox_SetColumnWidth _GUICtrlListBox_SetCurSel
	\ _GUICtrlListBox_SetHorizontalExtent _GUICtrlListBox_SetItemData
	\ _GUICtrlListBox_SetItemHeight _GUICtrlListBox_SetLocale
	\ _GUICtrlListBox_SetSel _GUICtrlListBox_SetTabStops
	\ _GUICtrlListBox_SetTopIndex _GUICtrlListBox_Sort
	\ _GUICtrlListBox_SwapString _GUICtrlListBox_UpdateHScroll
" guilistview
syn keyword autoitFunction _GUICtrlListView_AddArray _GUICtrlListView_AddColumn
	\ _GUICtrlListView_AddItem _GUICtrlListView_AddSubItem
	\ _GUICtrlListView_ApproximateViewHeight
	\ _GUICtrlListView_ApproximateViewRect _GUICtrlListView_ApproximateViewWidth
	\ _GUICtrlListView_Arrange _GUICtrlListView_BeginUpdate
	\ _GUICtrlListView_CancelEditLabel _GUICtrlListView_ClickItem
	\ _GUICtrlListView_CopyItems _GUICtrlListView_Create
	\ _GUICtrlListView_CreateDragImage _GUICtrlListView_CreateSolidBitMap
	\ _GUICtrlListView_DeleteAllItems _GUICtrlListView_DeleteColumn
	\ _GUICtrlListView_DeleteItem _GUICtrlListView_DeleteItemsSelected
	\ _GUICtrlListView_Destroy _GUICtrlListView_DrawDragImage
	\ _GUICtrlListView_EditLabel _GUICtrlListView_EnableGroupView
	\ _GUICtrlListView_EndUpdate _GUICtrlListView_EnsureVisible
	\ _GUICtrlListView_FindInText _GUICtrlListView_FindItem
	\ _GUICtrlListView_FindNearest _GUICtrlListView_FindParam
	\ _GUICtrlListView_FindText _GUICtrlListView_GetBkColor
	\ _GUICtrlListView_GetBkImage _GUICtrlListView_GetCallbackMask
	\ _GUICtrlListView_GetColumn _GUICtrlListView_GetColumnCount
	\ _GUICtrlListView_GetColumnOrder _GUICtrlListView_GetColumnOrderArray
	\ _GUICtrlListView_GetColumnWidth _GUICtrlListView_GetCounterPage
	\ _GUICtrlListView_GetEditControl _GUICtrlListView_GetExtendedListViewStyle
	\ _GUICtrlListView_GetFocusedGroup _GUICtrlListView_GetGroupCount
	\ _GUICtrlListView_GetGroupInfo _GUICtrlListView_GetGroupInfoByIndex
	\ _GUICtrlListView_GetGroupRect _GUICtrlListView_GetGroupViewEnabled
	\ _GUICtrlListView_GetHeader _GUICtrlListView_GetHotCursor
	\ _GUICtrlListView_GetHotItem _GUICtrlListView_GetHoverTime
	\ _GUICtrlListView_GetImageList _GUICtrlListView_GetISearchString
	\ _GUICtrlListView_GetItem _GUICtrlListView_GetItemChecked
	\ _GUICtrlListView_GetItemCount _GUICtrlListView_GetItemCut
	\ _GUICtrlListView_GetItemDropHilited _GUICtrlListView_GetItemEx
	\ _GUICtrlListView_GetItemFocused _GUICtrlListView_GetItemGroupID
	\ _GUICtrlListView_GetItemImage _GUICtrlListView_GetItemIndent
	\ _GUICtrlListView_GetItemParam _GUICtrlListView_GetItemPosition 
	\ _GUICtrlListView_GetItemPositionX _GUICtrlListView_GetItemPositionY
	\ _GUICtrlListView_GetItemRect _GUICtrlListView_GetItemRectEx
	\ _GUICtrlListView_GetItemSelected _GUICtrlListView_GetItemSpacing
	\ _GUICtrlListView_GetItemSpacingX _GUICtrlListView_GetItemSpacingY
	\ _GUICtrlListView_GetItemState _GUICtrlListView_GetItemStateImage
	\ _GUICtrlListView_GetItemText _GUICtrlListView_GetItemTextArray
	\ _GUICtrlListView_GetItemTextString _GUICtrlListView_GetNextItem
	\ _GUICtrlListView_GetNumberOfWorkAreas _GUICtrlListView_GetOrigin
	\ _GUICtrlListView_GetOriginX _GUICtrlListView_GetOriginY
	\ _GUICtrlListView_GetOutlineColor _GUICtrlListView_GetSelectedColumn
	\ _GUICtrlListView_GetSelectedCount _GUICtrlListView_GetSelectedIndices
	\ _GUICtrlListView_GetSelectionMark _GUICtrlListView_GetStringWidth
	\ _GUICtrlListView_GetSubItemRect _GUICtrlListView_GetTextBkColor
	\ _GUICtrlListView_GetTextColor _GUICtrlListView_GetToolTips
	\ _GUICtrlListView_GetTopIndex _GUICtrlListView_GetUnicodeFormat
	\ _GUICtrlListView_GetView _GUICtrlListView_GetViewDetails
	\ _GUICtrlListView_GetViewLarge _GUICtrlListView_GetViewList
	\ _GUICtrlListView_GetViewRect _GUICtrlListView_GetViewSmall
	\ _GUICtrlListView_GetViewTile _GUICtrlListView_HideColumn
	\ _GUICtrlListView_HitTest _GUICtrlListView_InsertColumn
	\ _GUICtrlListView_InsertGroup _GUICtrlListView_InsertItem
	\ _GUICtrlListView_JustifyColumn _GUICtrlListView_MapIDToIndex
	\ _GUICtrlListView_MapIndexToID _GUICtrlListView_RedrawItems
	\ _GUICtrlListView_RegisterSortCallBack _GUICtrlListView_RemoveAllGroups
	\ _GUICtrlListView_RemoveGroup _GUICtrlListView_Scroll
	\ _GUICtrlListView_SetBkColor _GUICtrlListView_SetBkImage
	\ _GUICtrlListView_SetCallBackMask _GUICtrlListView_SetColumn
	\ _GUICtrlListView_SetColumnOrder _GUICtrlListView_SetColumnOrderArray
	\ _GUICtrlListView_SetColumnWidth _GUICtrlListView_SetExtendedListViewStyle
	\ _GUICtrlListView_SetGroupInfo _GUICtrlListView_SetHotItem
	\ _GUICtrlListView_SetHoverTime _GUICtrlListView_SetIconSpacing
	\ _GUICtrlListView_SetImageList _GUICtrlListView_SetItem
	\ _GUICtrlListView_SetItemChecked _GUICtrlListView_SetItemCount
	\ _GUICtrlListView_SetItemCut _GUICtrlListView_SetItemDropHilited
	\ _GUICtrlListView_SetItemEx _GUICtrlListView_SetItemFocused
	\ _GUICtrlListView_SetItemGroupID _GUICtrlListView_SetItemImage
	\ _GUICtrlListView_SetItemIndent _GUICtrlListView_SetItemParam
	\ _GUICtrlListView_SetItemPosition _GUICtrlListView_SetItemPosition32
	\ _GUICtrlListView_SetItemSelected _GUICtrlListView_SetItemState
	\ _GUICtrlListView_SetItemStateImage _GUICtrlListView_SetItemText
	\ _GUICtrlListView_SetOutlineColor _GUICtrlListView_SetSelectedColumn
	\ _GUICtrlListView_SetSelectionMark _GUICtrlListView_SetTextBkColor
	\ _GUICtrlListView_SetTextColor _GUICtrlListView_SetToolTips
	\ _GUICtrlListView_SetUnicodeFormat _GUICtrlListView_SetView
	\ _GUICtrlListView_SetWorkAreas _GUICtrlListView_SimpleSort
	\ _GUICtrlListView_SortItems _GUICtrlListView_SubItemHitTest
	\ _GUICtrlListView_UnRegisterSortCallBack
" guimenu
syn keyword autoitFunction _GUICtrlMenu_AddMenuItem _GUICtrlMenu_AppendMenu
	\ _GUICtrlMenu_CheckMenuItem _GUICtrlMenu_CheckRadioItem
	\ _GUICtrlMenu_CreateMenu _GUICtrlMenu_CreatePopup _GUICtrlMenu_DeleteMenu
	\ _GUICtrlMenu_DestroyMenu _GUICtrlMenu_DrawMenuBar
	\ _GUICtrlMenu_EnableMenuItem _GUICtrlMenu_FindItem _GUICtrlMenu_FindParent
	\ _GUICtrlMenu_GetItemBmp _GUICtrlMenu_GetItemBmpChecked
	\ _GUICtrlMenu_GetItemBmpUnchecked _GUICtrlMenu_GetItemChecked
	\ _GUICtrlMenu_GetItemCount _GUICtrlMenu_GetItemData
	\ _GUICtrlMenu_GetItemDefault _GUICtrlMenu_GetItemDisabled
	\ _GUICtrlMenu_GetItemEnabled _GUICtrlMenu_GetItemGrayed
	\ _GUICtrlMenu_GetItemHighlighted _GUICtrlMenu_GetItemID
	\ _GUICtrlMenu_GetItemInfo _GUICtrlMenu_GetItemRect
	\ _GUICtrlMenu_GetItemRectEx _GUICtrlMenu_GetItemState
	\ _GUICtrlMenu_GetItemStateEx _GUICtrlMenu_GetItemSubMenu
	\ _GUICtrlMenu_GetItemText _GUICtrlMenu_GetItemType _GUICtrlMenu_GetMenu
	\ _GUICtrlMenu_GetMenuBackground _GUICtrlMenu_GetMenuBarInfo
	\ _GUICtrlMenu_GetMenuContextHelpID _GUICtrlMenu_GetMenuData
	\ _GUICtrlMenu_GetMenuDefaultItem _GUICtrlMenu_GetMenuHeight
	\ _GUICtrlMenu_GetMenuInfo _GUICtrlMenu_GetMenuStyle
	\ _GUICtrlMenu_GetSystemMenu _GUICtrlMenu_InsertMenuItem
	\ _GUICtrlMenu_InsertMenuItemEx _GUICtrlMenu_IsMenu _GUICtrlMenu_LoadMenu
	\ _GUICtrlMenu_MapAccelerator _GUICtrlMenu_MenuItemFromPoint
	\ _GUICtrlMenu_RemoveMenu _GUICtrlMenu_SetItemBitmaps
	\ _GUICtrlMenu_SetItemBmp _GUICtrlMenu_SetItemBmpChecked
	\ _GUICtrlMenu_SetItemBmpUnchecked _GUICtrlMenu_SetItemChecked
	\ _GUICtrlMenu_SetItemData _GUICtrlMenu_SetItemDefault
	\ _GUICtrlMenu_SetItemDisabled _GUICtrlMenu_SetItemEnabled
	\ _GUICtrlMenu_SetItemGrayed _GUICtrlMenu_SetItemHighlighted
	\ _GUICtrlMenu_SetItemID _GUICtrlMenu_SetItemInfo _GUICtrlMenu_SetItemState
	\ _GUICtrlMenu_SetItemSubMenu _GUICtrlMenu_SetItemText
	\ _GUICtrlMenu_SetItemType _GUICtrlMenu_SetMenu
	\ _GUICtrlMenu_SetMenuBackground _GUICtrlMenu_SetMenuContextHelpID
	\ _GUICtrlMenu_SetMenuData _GUICtrlMenu_SetMenuDefaultItem
	\ _GUICtrlMenu_SetMenuHeight _GUICtrlMenu_SetMenuInfo
	\ _GUICtrlMenu_SetMenuStyle _GUICtrlMenu_TrackPopupMenu
" guimonthcal
syn keyword autoitFunction _GUICtrlMonthCal_Create _GUICtrlMonthCal_Destroy
	\ _GUICtrlMonthCal_GetCalendarBorder _GUICtrlMonthCal_GetCalendarCount
	\ _GUICtrlMonthCal_GetColor _GUICtrlMonthCal_GetColorArray
	\ _GUICtrlMonthCal_GetCurSel _GUICtrlMonthCal_GetCurSelStr
	\ _GUICtrlMonthCal_GetFirstDOW _GUICtrlMonthCal_GetFirstDOWStr
	\ _GUICtrlMonthCal_GetMaxSelCount _GUICtrlMonthCal_GetMaxTodayWidth
	\ _GUICtrlMonthCal_GetMinReqHeight _GUICtrlMonthCal_GetMinReqRect
	\ _GUICtrlMonthCal_GetMinReqRectArray _GUICtrlMonthCal_GetMinReqWidth
	\ _GUICtrlMonthCal_GetMonthDelta _GUICtrlMonthCal_GetMonthRange
	\ _GUICtrlMonthCal_GetMonthRangeMax _GUICtrlMonthCal_GetMonthRangeMaxStr
	\ _GUICtrlMonthCal_GetMonthRangeMin _GUICtrlMonthCal_GetMonthRangeMinStr
	\ _GUICtrlMonthCal_GetMonthRangeSpan _GUICtrlMonthCal_GetRange
	\ _GUICtrlMonthCal_GetRangeMax _GUICtrlMonthCal_GetRangeMaxStr
	\ _GUICtrlMonthCal_GetRangeMin _GUICtrlMonthCal_GetRangeMinStr
	\ _GUICtrlMonthCal_GetSelRange _GUICtrlMonthCal_GetSelRangeMax
	\ _GUICtrlMonthCal_GetSelRangeMaxStr _GUICtrlMonthCal_GetSelRangeMin
	\ _GUICtrlMonthCal_GetSelRangeMinStr _GUICtrlMonthCal_GetToday
	\ _GUICtrlMonthCal_GetTodayStr _GUICtrlMonthCal_GetUnicodeFormat
	\ _GUICtrlMonthCal_HitTest _GUICtrlMonthCal_SetCalendarBorder
	\ _GUICtrlMonthCal_SetColor _GUICtrlMonthCal_SetCurSel
	\ _GUICtrlMonthCal_SetDayState _GUICtrlMonthCal_SetFirstDOW
	\ _GUICtrlMonthCal_SetMaxSelCount _GUICtrlMonthCal_SetMonthDelta
	\ _GUICtrlMonthCal_SetRange _GUICtrlMonthCal_SetSelRange
	\ _GUICtrlMonthCal_SetToday _GUICtrlMonthCal_SetUnicodeFormat
" guirebar
syn keyword autoitFunction _GUICtrlRebar_AddBand _GUICtrlRebar_AddToolBarBand
	\ _GUICtrlRebar_BeginDrag _GUICtrlRebar_Create _GUICtrlRebar_DeleteBand
	\ _GUICtrlRebar_Destroy _GUICtrlRebar_DragMove _GUICtrlRebar_EndDrag
	\ _GUICtrlRebar_GetBandBackColor _GUICtrlRebar_GetBandBorders
	\ _GUICtrlRebar_GetBandBordersEx _GUICtrlRebar_GetBandChildHandle
	\ _GUICtrlRebar_GetBandChildSize _GUICtrlRebar_GetBandCount
	\ _GUICtrlRebar_GetBandForeColor _GUICtrlRebar_GetBandHeaderSize
	\ _GUICtrlRebar_GetBandID _GUICtrlRebar_GetBandIdealSize
	\ _GUICtrlRebar_GetBandLength _GUICtrlRebar_GetBandLParam
	\ _GUICtrlRebar_GetBandMargins _GUICtrlRebar_GetBandMarginsEx
	\ _GUICtrlRebar_GetBandRect _GUICtrlRebar_GetBandRectEx
	\ _GUICtrlRebar_GetBandStyle _GUICtrlRebar_GetBandStyleBreak
	\ _GUICtrlRebar_GetBandStyleChildEdge _GUICtrlRebar_GetBandStyleFixedBMP
	\ _GUICtrlRebar_GetBandStyleFixedSize
	\ _GUICtrlRebar_GetBandStyleGripperAlways _GUICtrlRebar_GetBandStyleHidden
	\ _GUICtrlRebar_GetBandStyleHideTitle _GUICtrlRebar_GetBandStyleNoGripper
	\ _GUICtrlRebar_GetBandStyleTopAlign _GUICtrlRebar_GetBandStyleUseChevron
	\ _GUICtrlRebar_GetBandStyleVariableHeight _GUICtrlRebar_GetBandText
	\ _GUICtrlRebar_GetBarHeight _GUICtrlRebar_GetBarInfo
	\ _GUICtrlRebar_GetBKColor _GUICtrlRebar_GetColorScheme
	\ _GUICtrlRebar_GetRowCount _GUICtrlRebar_GetRowHeight
	\ _GUICtrlRebar_GetTextColor _GUICtrlRebar_GetToolTips
	\ _GUICtrlRebar_GetUnicodeFormat _GUICtrlRebar_HitTest
	\ _GUICtrlRebar_IDToIndex _GUICtrlRebar_MaximizeBand
	\ _GUICtrlRebar_MinimizeBand _GUICtrlRebar_MoveBand
	\ _GUICtrlRebar_SetBandBackColor _GUICtrlRebar_SetBandForeColor
	\ _GUICtrlRebar_SetBandHeaderSize _GUICtrlRebar_SetBandID
	\ _GUICtrlRebar_SetBandIdealSize _GUICtrlRebar_SetBandLength
	\ _GUICtrlRebar_SetBandLParam _GUICtrlRebar_SetBandStyle
	\ _GUICtrlRebar_SetBandStyleBreak _GUICtrlRebar_SetBandStyleChildEdge
	\ _GUICtrlRebar_SetBandStyleFixedBMP _GUICtrlRebar_SetBandStyleFixedSize
	\ _GUICtrlRebar_SetBandStyleGripperAlways _GUICtrlRebar_SetBandStyleHidden
	\ _GUICtrlRebar_SetBandStyleHideTitle _GUICtrlRebar_SetBandStyleNoGripper
	\ _GUICtrlRebar_SetBandStyleTopAlign _GUICtrlRebar_SetBandStyleUseChevron
	\ _GUICtrlRebar_SetBandStyleVariableHeight _GUICtrlRebar_SetBandText
	\ _GUICtrlRebar_SetBarInfo _GUICtrlRebar_SetBKColor
	\ _GUICtrlRebar_SetColorScheme _GUICtrlRebar_SetTextColor
	\ _GUICtrlRebar_SetToolTips _GUICtrlRebar_SetUnicodeFormat
	\ _GUICtrlRebar_ShowBand
" guiscrollbars
syn keyword autoitFunction _GUIScrollBars_EnableScrollBar
	\ _GUIScrollBars_GetScrollBarInfoEx _GUIScrollBars_GetScrollBarRect
	\ _GUIScrollBars_GetScrollBarRGState _GUIScrollBars_GetScrollBarXYLineButton
	\ _GUIScrollBars_GetScrollBarXYThumbButton
	\ _GUIScrollBars_GetScrollBarXYThumbTop _GUIScrollBars_GetScrollInfo
	\ _GUIScrollBars_GetScrollInfoEx _GUIScrollBars_GetScollInfoMax
	\ _GUIScrollBars_GetScrollInfoMin _GUIScrollBars_GetScrollInfoPage
	\ _GUIScrollBars_GetScrollInfoPos _GUIScrollBars_GetScrollInfoTrackPos
	\ _GUIScrollBars_GetScrollPos _GUIScrollBars_GetScrollRange
	\ _GUIScrollBars_Init _GUIScrollBars_ScrollWindow
	\ _GUIScrollBars_SetScrollInfo _GUIScrollBars_SetScrollInfoMax
	\ _GUIScrollBars_SetScrollInfoMin _GUIScrollBars_SecScrollInfoPage
	\ _GUIScrollBars_SetScrollInfoPos _GUIScrollBars_SetScrollRange
	\ _GUIScrollBars_ShowScrollBar
" guislider
syn keyword autoitFunction _GUICtrlSlider_ClearSel _GUICtrlSlider_ClearTics
	\ _GUICtrlSlider_Create _GUICtrlSlider_Destroy _GUICtrlSlider_GetBuddy
	\ _GUICtrlSlider_GetChannelRect _GUICtrlSlider_GetChannelRectEx
	\ _GUICtrlSlider_GetLineSize _GUICtrlSlider_GetLogicalTics
	\ _GUICtrlSlider_GetNumTics _GUICtrlSlider_GetPageSize
	\ _GUICtrlSlider_GetPos _GUICtrlSlider_GetRange _GUICtrlSlider_GetRangeMax
	\ _GUICtrlSlider_GetRangeMin _GUICtrlSlider_GetSel _GUICtrlSlider_GetSelEnd
	\ _GUICtrlSlider_GetSelStart _GUICtrlSlider_GetThumbLength
	\ _GUICtrlSlider_GetThumbRect _GUICtrlSlider_GetThumbRectEx
	\ _GUICtrlSlider_GetTic _GUICtrlSlider_GetTicPos _GUICtrlSlider_GetToolTips
	\ _GUICtrlSlider_GetUnicodeFormat _GUICtrlSlider_SetBuddy
	\ _GUICtrlSlider_SetLineSize _GUICtrlSlider_SetPageSize
	\ _GUICtrlSlider_SetPos _GUICtrlSlider_SetRange _GUICtrlSlider_SetRangeMax
	\ _GUICtrlSlider_SetRangeMin _GUICtrlSlider_SetSel _GUICtrlSlider_SetSelEnd
	\ _GUICtrlSlider_SetSelStart _GUICtrlSlider_SetThumbLength
	\ _GUICtrlSlider_SetTic _GUICtrlSlider_SetTicFreq _GUICtrlSlider_SetTipSide
	\ _GUICtrlSlider_SetToolTips _GUICtrlSlider_SetUnicodeFormat
" guistatusbar
syn keyword autoitFunction _GUICtrlStatusBar_Create _GUICtrlStatusBar_Destroy
	\ _GUICtrlStatusBar_EmbedControl _GUICtrlStatusBar_GetBorders
	\ _GUICtrlStatusBar_GetBordersHorz _GUICtrlStatusBar_GetBordersRect
	\ _GUICtrlStatusBar_GetBordersVert _GUICtrlStatusBar_GetCount
	\ _GUICtrlStatusBar_GetHeight _GUICtrlStatusBar_GetIcon
	\ _GUICtrlStatusBar_GetParts _GUICtrlStatusBar_GetRect
	\ _GUICtrlStatusBar_GetRectEx _GUICtrlStatusBar_GetText
	\ _GUICtrlStatusBar_GetTextFlags _GUICtrlStatusBar_GetTextLength
	\ _GUICtrlStatusBar_GetTextLengthEx _GUICtrlStatusBar_GetTipText
	\ _GUICtrlStatusBar_GetUnicodeFormat _GUICtrlStatusBar_GetWidth
	\ _GUICtrlStatusBar_IsSimple _GUICtrlStatusBar_Resize
	\ _GUICtrlStatusBar_SetBkColor _GUICtrlStatusBar_SetIcon
	\ _GUICtrlStatusBar_SetMinHeight _GUICtrlStatusBar_SetParts
	\ _GUICtrlStatusBar_SetSimple _GUICtrlStatusBar_SetText
	\ _GUICtrlStatusBar_SetTipText _GUICtrlStatusBar_SetUnicodeFormat
	\ _GUICtrlStatusBar_ShowHide
" guitab
syn keyword autoitFunction _GUICtrlTab_ClickTab _GUICtrlTab_Create
	\ _GUICtrlTab_DeleteAllItems _GUICtrlTab_DeleteItem _GUICtrlTab_DeselectAll
	\ _GUICtrlTab_Destroy _GUICtrlTab_FindTab _GUICtrlTab_GetCurFocus
	\ _GUICtrlTab_GetCurSel _GUICtrlTab_GetDisplayRect
	\ _GUICtrlTab_GetDisplayRectEx _GUICtrlTab_GetExtendedStyle
	\ _GUICtrlTab_GetImageList _GUICtrlTab_GetItem _GUICtrlTab_GetItemCount
	\ _GUICtrlTab_GetItemImage _GUICtrlTab_GetItemParam _GUICtrlTab_GetItemRect
	\ _GUICtrlTab_GetItemRectEx _GUICtrlTab_GetItemState _GUICtrlTab_GetItemText
	\ _GUICtrlTab_GetRowCount _GUICtrlTab_GetToolTips
	\ _GUICtrlTab_GetUnicodeFormat _GUICtrlTab_HighlightItem _GUICtrlTab_HitTest
	\ _GUICtrlTab_InsertItem _GUICtrlTab_RemoveImage _GUICtrlTab_SetCurFocus
	\ _GUICtrlTab_SetCurSel _GUICtrlTab_SetExtendedStyle
	\ _GUICtrlTab_SetImageList _GUICtrlTab_SetItem _GUICtrlTab_SetItemImage
	\ _GUICtrlTab_SetItemParam _GUICtrlTab_SetItemSize _GUICtrlTab_SetItemState
	\ _GUICtrlTab_SetItemText _GUICtrlTab_SetMinTabWidth _GUICtrlTab_SetPadding
	\ _GUICtrlTab_SetToolTips _GUICtrlTab_SetUnicodeFormat
" guitoolbar
syn keyword autoitFunction _GUICtrlToolbar_AddBitmap _GUICtrlToolbar_AddButton
	\ _GUICtrlToolbar_AddButtonSep _GUICtrlToolbar_AddString
	\ _GUICtrlToolbar_ButtonCount _GUICtrlToolbar_CheckButton
	\ _GUICtrlToolbar_ClickAccel _GUICtrlToolbar_ClickButton
	\ _GUICtrlToolbar_ClickIndex _GUICtrlToolbar_CommandToIndex
	\ _GUICtrlToolbar_Create _GUICtrlToolbar_Customize
	\ _GUICtrlToolbar_DeleteButton _GUICtrlToolbar_Destroy
	\ _GUICtrlToolbar_EnableButton _GUICtrlToolbar_FindToolbar
	\ _GUICtrlToolbar_GetAnchorHighlight _GUICtrlToolbar_GetBitmapFlags
	\ _GUICtrlToolbar_GetButtonBitmap _GUICtrlToolbar_GetButtonInfo
	\ _GUICtrlToolbar_GetButtonInfoEx _GUICtrlToolbar_GetButtonParam
	\ _GUICtrlToolbar_GetButtonRect _GUICtrlToolbar_GetButtonRectEx
	\ _GUICtrlToolbar_GetButtonSize _GUICtrlToolbar_GetButtonState
	\ _GUICtrlToolbar_GetButtonStyle _GUICtrlToolbar_GetButtonText
	\ _GUICtrlToolbar_GetColorScheme _GUICtrlToolbar_GetDisabledImageList
	\ _GUICtrlToolbar_GetExtendedStyle _GUICtrlToolbar_GetHotImageList
	\ _GUICtrlToolbar_GetHotItem _GUICtrlToolbar_GetImageList
	\ _GUICtrlToolbar_GetInsertMark _GUICtrlToolbar_GetInsertMarkColor
	\ _GUICtrlToolbar_GetMaxSize _GUICtrlToolbar_GetMetrics
	\ _GUICtrlToolbar_GetPadding _GUICtrlToolbar_GetRows
	\ _GUICtrlToolbar_GetString _GUICtrlToolbar_GetStyle
	\ _GUICtrlToolbar_GetStyleAltDrag _GUICtrlToolbar_GetStyleCustomErase
	\ _GUICtrlToolbar_GetStyleFlat _GUICtrlToolbar_GetStyleList
	\ _GUICtrlToolbar_GetStyleRegisterDrop _GUICtrlToolbar_GetStyleToolTips
	\ _GUICtrlToolbar_GetStyleTransparent _GUICtrlToolbar_GetStyleWrapable
	\ _GUICtrlToolbar_GetTextRows _GUICtrlToolbar_GetToolTips
	\ _GUICtrlToolbar_GetUnicodeFormat _GUICtrlToolbar_HideButton
	\ _GUICtrlToolbar_HighlightButton _GUICtrlToolbar_HitTest
	\ _GUICtrlToolbar_IndexToCommand _GUICtrlToolbar_InsertButton
	\ _GUICtrlToolbar_InsertMarkHitTest _GUICtrlToolbar_IsButtonChecked
	\ _GUICtrlToolbar_IsButtonEnabled _GUICtrlToolbar_IsButtonHidden
	\ _GUICtrlToolbar_IsButtonHighlighted _GUICtrlToolbar_IsButtonIndeterminate
	\ _GUICtrlToolbar_IsButtonPressed _GUICtrlToolbar_LoadBitmap
	\ _GUICtrlToolbar_LoadImages _GUICtrlToolbar_MapAccelerator
	\ _GUICtrlToolbar_MoveButton _GUICtrlToolbar_PressButton
	\ _GUICtrlToolbar_SetAnchorHighlight _GUICtrlToolbar_SetBitmapSize
	\ _GUICtrlToolbar_SetButtonBitMap _GUICtrlToolbar_SetButtonInfo
	\ _GUICtrlToolbar_SetButtonInfoEx _GUICtrlToolbar_SetButtonParam
	\ _GUICtrlToolbar_SetButtonSize _GUICtrlToolbar_SetButtonState
	\ _GUICtrlToolbar_SetButtonStyle _GUICtrlToolbar_SetButtonText
	\ _GUICtrlToolbar_SetButtonWidth _GUICtrlToolbar_SetCmdID
	\ _GUICtrlToolbar_SetColorScheme _GUICtrlToolbar_SetDisabledImageList
	\ _GUICtrlToolbar_SetDrawTextFlags _GUICtrlToolbar_SetExtendedStyle
	\ _GUICtrlToolbar_SetHotImageList _GUICtrlToolbar_SetHotItem
	\ _GUICtrlToolbar_SetImageList _GUICtrlToolbar_SetIndent
	\ _GUICtrlToolbar_SetIndeterminate _GUICtrlToolbar_SetInsertMark
	\ _GUICtrlToolbar_SetInsertMarkColor _GUICtrlToolbar_SetMaxTextRows
	\ _GUICtrlToolbar_SetMetrics _GUICtrlToolbar_SetPadding
	\ _GUICtrlToolbar_SetParent _GUICtrlToolbar_SetRows
	\ _GUICtrlToolbar_SetStyle _GUICtrlToolbar_SetStyleAltDrag
	\ _GUICtrlToolbar_SetStyleCustomErase _GUICtrlToolbar_SetStyleFlat
	\ _GUICtrlToolbar_SetStyleList _GUICtrlToolbar_SetStyleRegisterDrop
	\ _GUICtrlToolbar_SetStyleToolTips _GUICtrlToolbar_SetStyleTransparent
	\ _GUICtrlToolbar_SetStyleWrapable _GUICtrlToolbar_SetToolTips
	\ _GUICtrlToolbar_SetUnicodeFormat _GUICtrlToolbar_SetWindowTheme
" guitooltip
syn keyword autoitFunction _GUIToolTip_Activate _GUIToolTip_AddTool
	\ _GUIToolTip_AdjustRect _GUIToolTip_BitsToTTE _GUIToolTip_Create
	\ _GUIToolTip_DelTool _GUIToolTip_Destroy _GUIToolTip_EnumTools
	\ _GUIToolTip_GetBubbleHeight _GUIToolTip_GetBubbleSize
	\ _GUIToolTip_GetBubbleWidth _GUIToolTip_GetCurrentTool
	\ _GUIToolTip_GetDelayTime _GUIToolTip_GetMargin _GUIToolTip_GetMarginEx
	\ _GUIToolTip_GetMaxTipWidth _GUIToolTip_GetText _GUIToolTip_GetTipTextColor
	\ _GUIToolTip_GetTitleBitMap _GUIToolTip_GetTitleText
	\ _GUIToolTip_GetToolCount _GUIToolTip_GetToolInfo _GUIToolTip_HitTest
	\ _GUIToolTip_NewToolRect _GUIToolTip_Pop _GUIToolTip_PopUp
	\ _GUIToolTip_SetDelayTime _GUIToolTip_SetMargin _GUIToolTip_SetMaxTipWidth
	\ _GUIToolTip_SetTipBkColor _GUIToolTip_SetTipTextColor
	\ _GUIToolTip_SetTitle _GUIToolTip_SetToolInfo _GUIToolTip_SetWindowTheme
	\ _GUIToolTip_ToolExists _GUIToolTip_ToolToArray _GUIToolTip_TrackActivate
	\ _GUIToolTip_TrackPosition _GUIToolTip_TTFToBits _GUIToolTip_Update
	\ _GUIToolTip_UpdateTipText
" guitreeview
syn keyword autoitFunction _GUICtrlTreeView_Add _GUICtrlTreeView_AddChild
	\ _GUICtrlTreeView_AddChildFirst _GUICtrlTreeView_AddFirst
	\ _GUICtrlTreeView_BeginUpdate _GUICtrlTreeView_ClickItem
	\ _GUICtrlTreeView_Create _GUICtrlTreeView_CreateDragImage
	\ _GUICtrlTreeView_CreateSolidBitMap _GUICtrlTreeView_Delete
	\ _GUICtrlTreeView_DeleteAll _GUICtrlTreeView_DeleteChildren
	\ _GUICtrlTreeView_Destroy _GUICtrlTreeView_DisplayRect
	\ _GUICtrlTreeView_DisplayRectEx _GUICtrlTreeView_EditText
	\ _GUICtrlTreeView_EndEdit _GUICtrlTreeView_EndUpdate
	\ _GUICtrlTreeView_EnsureVisible _GUICtrlTreeView_Expand
	\ _GUICtrlTreeView_ExpandedOnce _GUICtrlTreeView_FindItem
	\ _GUICtrlTreeView_FindItemEx _GUICtrlTreeView_GetBkColor
	\ _GUICtrlTreeView_GetBold _GUICtrlTreeView_GetChecked
	\ _GUICtrlTreeView_GetChildCount _GUICtrlTreeView_GetChildren
	\ _GUICtrlTreeView_GetCount _GUICtrlTreeView_GetCut
	\ _GUICtrlTreeView_GetDropTarget _GUICtrlTreeView_GetEditControl
	\ _GUICtrlTreeView_GetExpanded _GUICtrlTreeView_GetFirstChild
	\ _GUICtrlTreeView_GetFirstItem _GUICtrlTreeView_GetFirstVisible
	\ _GUICtrlTreeView_GetFocused _GUICtrlTreeView_GetHeight
	\ _GUICtrlTreeView_GetImageIndex _GUICtrlTreeView_GetImageListIconHandle
	\ _GUICtrlTreeView_GetIndent _GUICtrlTreeView_GetInsertMarkColor
	\ _GUICtrlTreeView_GetISearchString _GUICtrlTreeView_GetItemByIndex
	\ _GUICtrlTreeView_GetItemHandle _GUICtrlTreeView_GetItemParam
	\ _GUICtrlTreeView_GetLastChild _GUICtrlTreeView_GetLineColor
	\ _GUICtrlTreeView_GetNext _GUICtrlTreeView_GetNextChild
	\ _GUICtrlTreeView_GetNextSibling _GUICtrlTreeView_GetNextVisible
	\ _GUICtrlTreeView_GetNormalImageList _GUICtrlTreeView_GetParentHandle
	\ _GUICtrlTreeView_GetParentParam _GUICtrlTreeView_GetPrev
	\ _GUICtrlTreeView_GetPrevChild _GUICtrlTreeView_GetPrevSibling
	\ _GUICtrlTreeView_GetPrevVisible _GUICtrlTreeView_GetScrollTime
	\ _GUICtrlTreeView_GetSelected _GUICtrlTreeView_GetSelectedImageIndex
	\ _GUICtrlTreeView_GetSelection _GUICtrlTreeView_GetSiblingCount
	\ _GUICtrlTreeView_GetState _GUICtrlTreeView_GetStateImageIndex
	\ _GUICtrlTreeView_GetStateImageList _GUICtrlTreeView_GetText
	\ _GUICtrlTreeView_GetTextColor _GUICtrlTreeView_GetToolTips
	\ _GUICtrlTreeView_GetTree _GUICtrlTreeView_GetUnicodeFormat
	\ _GUICtrlTreeView_GetVisible _GUICtrlTreeView_GetVisibleCount
	\ _GUICtrlTreeView_HitTest _GUICtrlTreeView_HitTestEx
	\ _GUICtrlTreeView_HitTestItem _GUICtrlTreeView_Index
	\ _GUICtrlTreeView_InsertItem _GUICtrlTreeView_IsFirstItem
	\ _GUICtrlTreeView_IsParent _GUICtrlTreeView_Level
	\ _GUICtrlTreeView_SelectItem _GUICtrlTreeView_SelectItemByIndex
	\ _GUICtrlTreeView_SetBkColor _GUICtrlTreeView_SetBold
	\ _GUICtrlTreeView_SetChecked _GUICtrlTreeView_SetCheckedByIndex
	\ _GUICtrlTreeView_SetChildren _GUICtrlTreeView_SetCut
	\ _GUICtrlTreeView_SetDropTarget _GUICtrlTreeView_SetFocused
	\ _GUICtrlTreeView_SetHeight _GUICtrlTreeView_SetIcon
	\ _GUICtrlTreeView_SetImageIndex _GUICtrlTreeView_SetIndent
	\ _GUICtrlTreeView_SetInsertMark _GUICtrlTreeView_SetInsertMarkColor
	\ _GUICtrlTreeView_SetItemHeight _GUICtrlTreeView_SetItemParam
	\ _GUICtrlTreeView_SetLineColor _GUICtrlTreeView_SetNormalImageList
	\ _GUICtrlTreeView_SetScrollTime _GUICtrlTreeView_SetSelected
	\ _GUICtrlTreeView_SetSelectedImageIndex _GUICtrlTreeView_SetState
	\ _GUICtrlTreeView_SetStateImageIndex _GUICtrlTreeView_SetStateImageList
	\ _GUICtrlTreeView_SetText _GUICtrlTreeView_SetTextColor
	\ _GUICtrlTreeView_SetToolTips _GUICtrlTreeView_SetUnicodeFormat
	\ _GUICtrlTreeView_Sort
" ie
syn keyword autoitFunction _IE_Example _IE_Introduction _IE_VersionInfo
	\ _IEAction _IEAttach _IEBodyReadHTML _IEBodyReadText _IEBodyWriteHTML
	\ _IECreate _IECreateEmbedded _IEDocGetObj _IEDocInsertHTML
	\ _IEDocInsertText _IEDocReadHTML _IEDocWriteHTML
	\ _IEErrorHandlerDeRegister _IEErrorHandlerRegister _IEErrorNotify
	\ _IEFormElementCheckboxSelect _IEFormElementGetCollection
	\ _IEFormElementGetObjByName _IEFormElementGetValue
	\ _IEFormElementOptionSelect _IEFormElementRadioSelect
	\ _IEFormElementSetValue _IEFormGetCollection _IEFormGetObjByName
	\ _IEFormImageClick _IEFormReset _IEFormSubmit _IEFrameGetCollection
	\ _IEFrameGetObjByName _IEGetObjByName _IEHeadInsertEventScript
	\ _IEImgClick _IEImgGetCollection _IEIsFrameSet _IELinkClickByIndex
	\ _IELinkClickByText _IELinkGetCollection _IELoadWait _IELoadWaitTimeout
	\ _IENavigate _IEPropertyGet _IEPropertySet _IEQuit
	\ _IETableGetCollection _IETableWriteToArray _IETagNameAllGetCollection
	\  _IETagNameGetCollection
" inet
syn keyword autoitFunction _GetIP _INetExplorerCapable _INetGetSource _INetMail
	\ _INetSmtpMail _TCPIpToName
" math
syn keyword autoitFunction _Degree _MathCheckDiv _Max _Min _Radian
" mem
syn keyword autoitFunction _MemGlobalAlloc _MemGlobalFree _MemGlobalLock
	\ _MemGlobalSize _MemGlobalUnlock _MemMoveMemory _MemMsgBox _MemShowError
	\ _MemVirtualAlloc _MemVirtualAllocEx _MemVirtualFree _MemVirtualFreeEx
" miscellaneous
syn keyword autoitFunction _ChooseColor _ChooseFont _ClipPutFile _Iif
	\ _IsPressed _MouseTrap _SendMessage _Singleton _VersionCompare
" namedpipes
syn keyword autoitFunction _NamedPipes_CallNamedPipe
	\ _NamedPipes_ConnectNamedPipe _NamedPipes_CreateNamedPipe
	\ _NamedPipes_CreatePipe _NamedPipes_DisconnectNamedPipe
	\ _NamedPipes_GetNamedPipeHandleState _NamedPipes_GetNamedPipeInfo
	\ _NamedPipes_PeekNamedPipe _NamedPipes_SetNamedPipeHandleState
	\ _NamedPipes_TransactNamedPipe _NamedPipes_WaitNamedPipe
" netshare
syn keyword autoitFunction _Net_Share_ConnectionEnum _Net_Share_FileClose
	\ _Net_Share_FileEnum _Net_Share_FileGetInfo _Net_Share_PermStr
	\ _Net_Share_ResourceStr _Net_Share_SessionDel _Net_Share_SessionEnum
	\ _Net_Share_SessionGetInfo _Net_Share_ShareAdd _Net_Share_ShareCheck
	\ _Net_Share_ShareDel _Net_Share_ShareEnum _Net_Share_ShareGetInfo
	\ _Net_Share_ShareSetInfo _Net_Share_StatisticsGetSvr
	\ _Net_Share_StatisticsGetWrk
" process
syn keyword autoitFunction _ProcessGetName _ProcessGetPriority _RunDOS
" screencapture
syn keyword autoitFunction _ScreenCapture_Capture _ScreenCapture_CaptureWnd
	\ _ScreenCapture_SaveImage _ScreenCapture_SetBMPFormat
	\ _ScreenCapture_SetJPGQuality _ScreenCapture_SetTIFColorDepth
	\ _ScreenCapture_SetTIFCompression
" security
syn keyword autoitFunction _Security__AdjustTokenPrivileges
	\ _Security__GetAccountSid _Security__GetLengthSid
	\ _Security__GetTokenInformation _Security__ImpersonateSelf
	\ _Security__IsValidSid _Security__LookupAccountName
	\ _Security__LookupAccountSid _Security__LookupPrivilegeValue
	\ _Security__OpenProcessToken _Security__OpenThreadToken
	\ _Security__OpenThreadTokenEx _Security__SetPrivilege
	\ _Security__SidToStringSid _Security__SidTypeStr
	\ _Security__StringSidToSid
" sendmessage
syn keyword autoitFunction _SendMessage _SendMessageA
" sound
syn keyword autoitFunction _SoundClose _SoundLength _SoundOpen _SoundPause
	\ _SoundPlay _SoundPos _SoundResume _SoundSeek _SoundStatus _SoundStop
" sqlite
syn keyword autoitFunction _SQLite_Changes _SQLite_Close
	\ _SQLite_Display2DResult _SQLite_Encode _SQLite_ErrCode _SQLite_ErrMsg
	\ _SQLite_Escape _SQLite_Exec _SQLite_FetchData _SQLite_FetchNames
	\ _SQLite_GetTable _SQLite_GetTable2D _SQLite_LastInsertRowID
	\ _SQLite_LibVersion _SQLite_Open _SQLite_Query _SQLite_QueryFinalize
	\ _SQLite_QueryReset _SQLite_QuerySingleRow _SQLite_SafeMode
	\ _SQLite_SetTimeout _SQLite_Shutdown _SQLite_SQLiteExe _SQLite_Startup
	\ _SQLite_TotalChanges
" string
syn keyword autoitFunction _HexToString _StringBetween _StringEncrypt
	\ _StringExplode _StringInsert _StringProper _StringRepeat _StringReverse
	\ _StringToHex
" timers
syn keyword autoitFunction _Timer_Diff _Timer_GetIdleTime _Timer_GetTimerID _Timer_Init _Timer_KillAllTimers _Timer_KillTimer _Timer_SetTimer
" visa
syn keyword autoitFunction _viClose _viExecCommand _viFindGpib _viGpibBusReset
	\ _viGTL _viOpen _viSetAttribute _viSetTimeout
" winapi
syn keyword autoitFunction _WinAPI_AttachConsole _WinAPI_AttachThreadInput
	\ _WinAPI_Beep _WinAPI_BitBlt _WinAPI_CallNextHookEx _WinAPI_CallWindowProc
	\ _WinAPI_ClientToScreen _WinAPI_CloseHandle _WinAPI_CombineRgn
	\ _WinAPI_CommDlgExtendedError _WinAPI_CopyIcon _WinAPI_CreateBitmap
	\ _WinAPI_CreateCompatibleBitmap _WinAPI_CreateCompatibleDC
	\ _WinAPI_CreateEvent _WinAPI_CreateFile _WinAPI_CreateFont
	\ _WinAPI_CreateFontIndirect _WinAPI_CreatePen _WinAPI_CreateProcess
	\ _WinAPI_CreateRectRgn _WinAPI_CreateRoundRectRgn
	\ _WinAPI_CreateSolidBitmap _WinAPI_CreateSolidBrush _WinAPI_CreateWindowEx
	\ _WinAPI_DefWindowProc _WinAPI_DeleteDC _WinAPI_DeleteObject
	\ _WinAPI_DestroyIcon _WinAPI_DestroyWindow _WinAPI_DrawEdge
	\ _WinAPI_DrawFrameControl _WinAPI_DrawIcon _WinAPI_DrawIconEx
	\ _WinAPI_DrawLine _WinAPI_DrawText _WinAPI_EnableWindow
	\ _WinAPI_EnumDisplayDevices _WinAPI_EnumWindows _WinAPI_EnumWindowsPopup
	\ _WinAPI_EnumWindowsTop _WinAPI_ExpandEnvironmentStrings
	\ _WinAPI_ExtractIconEx _WinAPI_FatalAppExit _WinAPI_FillRect
	\ _WinAPI_FindExecutable _WinAPI_FindWindow _WinAPI_FlashWindow
	\ _WinAPI_FlashWindowEx _WinAPI_FloatToInt _WinAPI_FlushFileBuffers
	\ _WinAPI_FormatMessage _WinAPI_FrameRect _WinAPI_FreeLibrary
	\ _WinAPI_GetAncestor _WinAPI_GetAsyncKeyState _WinAPI_GetBkMode
	\ _WinAPI_GetClassName _WinAPI_GetClientHeight _WinAPI_GetClientRect
	\ _WinAPI_GetClientWidth _WinAPI_GetCurrentProcess
	\ _WinAPI_GetCurrentProcessID _WinAPI_GetCurrentThread
	\ _WinAPI_GetCurrentThreadId _WinAPI_GetCursorInfo _WinAPI_GetDC
	\ _WinAPI_GetDesktopWindow _WinAPI_GetDeviceCaps _WinAPI_GetDIBits
	\ _WinAPI_GetDlgCtrlID _WinAPI_GetDlgItem _WinAPI_GetFileSizeEx
	\ _WinAPI_GetFocus _WinAPI_GetForegroundWindow _WinAPI_GetGuiResources
	\ _WinAPI_GetIconInfo _WinAPI_GetLastError _WinAPI_GetLastErrorMessage
	\ _WinAPI_GetLayeredWindowAttributes _WinAPI_GetModuleHandle
	\ _WinAPI_GetMousePos _WinAPI_GetMousePosX _WinAPI_GetMousePosY
	\ _WinAPI_GetObject _WinAPI_GetOpenFileName _WinAPI_GetOverlappedResult
	\ _WinAPI_GetParent _WinAPI_GetProcessAffinityMask _WinAPI_GetSaveFileName
	\ _WinAPI_GetStdHandle _WinAPI_GetStockObject _WinAPI_GetSysColor
	\ _WinAPI_GetSysColorBrush _WinAPI_GetSystemMetrics
	\ _WinAPI_GetTextExtentPoint32 _WinAPI_GetWindow _WinAPI_GetWindowDC
	\ _WinAPI_GetWindowHeight _WinAPI_GetWindowLong _WinAPI_GetWindowPlacement
	\ _WinAPI_GetWindowRect _WinAPI_GetWindowText
	\ _WinAPI_GetWindowThreadProcessId _WinAPI_GetWindowWidth
	\ _WinAPI_GetXYFromPoint _WinAPI_GlobalMemStatus _WinAPI_GUIDFromString
	\ _WinAPI_GUIDFromStringEx _WinAPI_HiWord _WinAPI_InProcess
	\ _WinAPI_IntToFloat _WinAPI_InvalidateRect _WinAPI_IsClassName
	\ _WinAPI_IsWindow _WinAPI_IsWindowVisible _WinAPI_LineTo _WinAPI_LoadBitmap
	\ _WinAPI_LoadImage _WinAPI_LoadLibrary _WinAPI_LoadLibraryEx
	\ _WinAPI_LoadShell32Icon _WinAPI_LoadString _WinAPI_LocalFree
	\ _WinAPI_LoWord _WinAPI_MakeDWord _WinAPI_MAKELANGID _WinAPI_MAKELCID
	\ _WinAPI_MakeLong _WinAPI_MessageBeep _WinAPI_Mouse_Event _WinAPI_MoveTo
	\ _WinAPI_MoveWindow _WinAPI_MsgBox _WinAPI_MulDiv
	\ _WinAPI_MultiByteToWideChar _WinAPI_MultiByteToWideCharEx
	\ _WinAPI_OpenProcess _WinAPI_PathFindOnPath _WinAPI_PointFromRect
	\ _WinAPI_PostMessage _WinAPI_PrimaryLangId _WinAPI_PtInRect
	\ _WinAPI_ReadFile _WinAPI_ReadProcessMemory _WinAPI_RectIsEmpty
	\ _WinAPI_RedrawWindow _WinAPI_RegisterWindowMessage
	\ _WinAPI_ReleaseCapture _WinAPI_ReleaseDC _WinAPI_ScreenToClient
	\ _WinAPI_SelectObject _WinAPI_SetBkColor _WinAPI_SetBkMode
	\ _WinAPI_SetCapture _WinAPI_SetCursor _WinAPI_SetDefaultPrinter
	\ _WinAPI_SetDIBits _WinAPI_SetEndOfFile _WinAPI_SetEvent
	\ _WinAPI_SetFilePointer _WinAPI_SetFocus _WinAPI_SetFont
	\ _WinAPI_SetHandleInformation _WinAPI_SetLastError
	\ _WinAPI_SetLayeredWindowAttributes _WinAPI_SetParent
	\ _WinAPI_SetProcessAffinityMask _WinAPI_SetSysColors _WinAPI_SetTextColor
	\ _WinAPI_SetWindowLong _WinAPI_SetWindowPlacement _WinAPI_SetWindowPos
	\ _WinAPI_SetWindowRgn _WinAPI_SetWindowsHookEx _WinAPI_SetWindowText
	\ _WinAPI_ShowCursor _WinAPI_ShowError _WinAPI_ShowMsg _WinAPI_ShowWindow
	\ _WinAPI_StringFromGUID _WinAPI_SubLangId _WinAPI_SystemParametersInfo
	\ _WinAPI_TwipsPerPixelX _WinAPI_TwipsPerPixelY _WinAPI_UnhookWindowsHookEx
	\ _WinAPI_UpdateLayeredWindow _WinAPI_UpdateWindow _WinAPI_ValidateClassName
	\ _WinAPI_WaitForInputIdle _WinAPI_WaitForMultipleObjects
	\ _WinAPI_WaitForSingleObject _WinAPI_WideCharToMultiByte
	\ _WinAPI_WindowFromPoint _WinAPI_WriteConsole _WinAPI_WriteFile
	\ _WinAPI_WriteProcessMemory
" winnet
syn keyword autoitFunction _WinNet_AddConnection _WinNet_AddConnection2
	\ _WinNet_AddConnection3 _WinNet_CancelConnection _WinNet_CancelConnection2
	\ _WinNet_CloseEnum _WinNet_ConnectionDialog _WinNet_ConnectionDialog1
	\ _WinNet_DisconnectDialog _WinNet_DisconnectDialog1 _WinNet_EnumResource
	\ _WinNet_GetConnection _WinNet_GetConnectionPerformance
	\ _WinNet_GetLastError _WinNet_GetNetworkInformation _WinNet_GetProviderName
	\ _WinNet_GetResourceInformation _WinNet_GetResourceParent
	\ _WinNet_GetUniversalName _WinNet_GetUser _WinNet_OpenEnum
	\ _WinNet_RestoreConnection _WinNet_UseConnection
" word
syn keyword autoitFunction 	_Word_VersionInfo _WordAttach _WordCreate
	\ _WordDocAdd _WordDocAddLink _WordDocAddPicture _WordDocClose
	\ _WordDocFindReplace _WordDocGetCollection _WordDocLinkGetCollection
	\ _WordDocOpen _WordDocPrint _WordDocPropertyGet _WordDocPropertySet
	\ _WordDocSave _WordDocSaveAs _WordErrorHandlerDeRegister
	\ _WordErrorHandlerRegister _WordErrorNotify _WordMacroRun _WordPropertyGet
	\ _WordPropertySet _WordQuit

" read-only macros
syn match autoitBuiltin "@AppData\(Common\)\=Dir"
syn match autoitBuiltin "@AutoItExe"
syn match autoitBuiltin "@AutoItPID"
syn match autoitBuiltin "@AutoItVersion"
syn match autoitBuiltin "@AutoItUnicode"
syn match autoitBuiltin "@AutoItX64"
syn match autoitBuiltin "@COM_EventObj"
syn match autoitBuiltin "@CommonFilesDir"
syn match autoitBuiltin "@Compiled"
syn match autoitBuiltin "@ComputerName"
syn match autoitBuiltin "@ComSpec"
syn match autoitBuiltin "@CPUArch"
syn match autoitBuiltin "@CR\(LF\)\="
syn match autoitBuiltin "@Desktop\(Common\)\=Dir"
syn match autoitBuiltin "@DesktopDepth"
syn match autoitBuiltin "@DesktopHeight"
syn match autoitBuiltin "@DesktopRefresh"
syn match autoitBuiltin "@DesktopWidth"
syn match autoitBuiltin "@DocumentsCommonDir"
syn match autoitBuiltin "@Error"
syn match autoitBuiltin "@ExitCode"
syn match autoitBuiltin "@ExitMethod"
syn match autoitBuiltin "@Extended"
syn match autoitBuiltin "@Favorites\(Common\)\=Dir"
syn match autoitBuiltin "@GUI_CtrlId"
syn match autoitBuiltin "@GUI_CtrlHandle"
syn match autoitBuiltin "@GUI_DragId"
syn match autoitBuiltin "@GUI_DragFile"
syn match autoitBuiltin "@GUI_DropId"
syn match autoitBuiltin "@GUI_WinHandle"
syn match autoitBuiltin "@HomeDrive"
syn match autoitBuiltin "@HomePath"
syn match autoitBuiltin "@HomeShare"
syn match autoitBuiltin "@HOUR"
syn match autoitBuiltin "@HotKeyPressed"
syn match autoitBuiltin "@IPAddress[1234]"
syn match autoitBuiltin "@KBLayout"
syn match autoitBuiltin "@LF"
syn match autoitBuiltin "@Logon\(DNS\)\=Domain"
syn match autoitBuiltin "@LogonServer"
syn match autoitBuiltin "@MDAY"
syn match autoitBuiltin "@MIN"
syn match autoitBuiltin "@MON"
syn match autoitBuiltin "@MSEC"
syn match autoitBuiltin "@MyDocumentsDir"
syn match autoitBuiltin "@NumParams"
syn match autoitBuiltin "@OSArch"
syn match autoitBuiltin "@OSBuild"
syn match autoitBuiltin "@OSLang"
syn match autoitBuiltin "@OSServicePack"
syn match autoitBuiltin "@OSTYPE"
syn match autoitBuiltin "@OSVersion"
syn match autoitBuiltin "@ProgramFilesDir"
syn match autoitBuiltin "@Programs\(Common\)\=Dir"
syn match autoitBuiltin "@ScriptDir"
syn match autoitBuiltin "@ScriptFullPath"
syn match autoitBuiltin "@ScriptLineNumber"
syn match autoitBuiltin "@ScriptName"
syn match autoitBuiltin "@SEC"
syn match autoitBuiltin "@StartMenu\(Common\)\=Dir"
syn match autoitBuiltin "@Startup\(Common\)\=Dir"
syn match autoitBuiltin "@SW_DISABLE"
syn match autoitBuiltin "@SW_ENABLE"
syn match autoitBuiltin "@SW_HIDE"
syn match autoitBuiltin "@SW_LOCK"
syn match autoitBuiltin "@SW_MAXIMIZE"
syn match autoitBuiltin "@SW_MINIMIZE"
syn match autoitBuiltin "@SW_RESTORE"
syn match autoitBuiltin "@SW_SHOW"
syn match autoitBuiltin "@SW_SHOWDEFAULT"
syn match autoitBuiltin "@SW_SHOWMAXIMIZED"
syn match autoitBuiltin "@SW_SHOWMINIMIZED"
syn match autoitBuiltin "@SW_SHOWMINNOACTIVE"
syn match autoitBuiltin "@SW_SHOWNA"
syn match autoitBuiltin "@SW_SHOWNOACTIVATE"
syn match autoitBuiltin "@SW_SHOWNORMAL"
syn match autoitBuiltin "@SW_UNLOCK"
syn match autoitBuiltin "@SystemDir"
syn match autoitBuiltin "@TAB"
syn match autoitBuiltin "@TempDir"
syn match autoitBuiltin "@TRAY_ID"
syn match autoitBuiltin "@TrayIconFlashing"
syn match autoitBuiltin "@TrayIconVisible"
syn match autoitBuiltin "@UserProfileDir"
syn match autoitBuiltin "@UserName"
syn match autoitBuiltin "@WDAY"
syn match autoitBuiltin "@WindowsDir"
syn match autoitBuiltin "@WorkingDir"
syn match autoitBuiltin "@YDAY"
syn match autoitBuiltin "@YEAR"

"comments and commenting-out
syn match autoitComment ";.*"
"in this way also #ce alone will be highlighted
syn match autoitCommDelimiter "^\s*#comments-start\>"
syn match autoitCommDelimiter "^\s*#cs\>"
syn match autoitCommDelimiter "^\s*#comments-end\>"
syn match autoitCommDelimiter "^\s*#ce\>"
syn region autoitComment
	\ matchgroup=autoitCommDelimiter
	\ start="^\s*#comments-start\>" start="^\s*#cs\>"
	\ end="^\s*#comments-end\>" end="^\s*#ce\>"

"one character operators
syn match autoitOperator "[-+*/&^=<>][^-+*/&^=<>]"me=e-1
"two characters operators
syn match autoitOperator "==[^=]"me=e-1
syn match autoitOperator "<>"
syn match autoitOperator "<="
syn match autoitOperator ">="
syn match autoitOperator "+="
syn match autoitOperator "-="
syn match autoitOperator "*="
syn match autoitOperator "/="
syn match autoitOperator "&="
syn keyword autoitOperator NOT AND OR

syn match autoitParen "(\|)"
syn match autoitBracket "\[\|\]"
syn match autoitComma ","

"numbers must come after operator '-'
"decimal numbers without a dot
syn match autoitNumber "-\=\<\d\+\>"
"hexadecimal numbers without a dot
syn match autoitNumber "-\=\<0x\x\+\>"
"floating point number with dot (inside or at end)

syn match autoitNumber "-\=\<\d\+\.\d*\>"
"floating point number, starting with a dot
syn match autoitNumber "-\=\<\.\d\+\>"
"scientific notation numbers without dots
syn match autoitNumber "-\=\<\d\+e[-+]\=\d\+\>"
"scientific notation numbers with dots
syn match autoitNumber "-\=\<\(\(\d\+\.\d*\)\|\(\.\d\+\)\)\(e[-+]\=\d\+\)\=\>"

"string constants
"we want the escaped quotes marked in red
syn match autoitDoubledSingles +''+ contained
syn match autoitDoubledDoubles +""+ contained
"we want the continuation character marked in red
"(also at the top level, not just contained)
syn match autoitCont "_$"

" send key list - must be defined before autoitStrings
syn match autoitSend "{!}" contained
syn match autoitSend "{#}" contained
syn match autoitSend "{+}" contained
syn match autoitSend "{^}" contained
syn match autoitSend "{{}" contained
syn match autoitSend "{}}" contained
syn match autoitSend "{SPACE}" contained
syn match autoitSend "{ENTER}" contained
syn match autoitSend "{ALT}" contained
syn match autoitSend "{BACKSPACE}" contained
syn match autoitSend "{BS}" contained
syn match autoitSend "{DELETE}" contained
syn match autoitSend "{DEL}" contained
syn match autoitSend "{UP}" contained
syn match autoitSend "{DOWN}" contained
syn match autoitSend "{LEFT}" contained
syn match autoitSend "{RIGHT}" contained
syn match autoitSend "{HOME}" contained
syn match autoitSend "{END}" contained
syn match autoitSend "{ESCAPE}" contained
syn match autoitSend "{ESC}" contained
syn match autoitSend "{INSERT}" contained
syn match autoitSend "{INS}" contained
syn match autoitSend "{PGUP}" contained
syn match autoitSend "{PGDN}" contained
syn match autoitSend "{F1}" contained
syn match autoitSend "{F2}" contained
syn match autoitSend "{F3}" contained
syn match autoitSend "{F4}" contained
syn match autoitSend "{F5}" contained
syn match autoitSend "{F6}" contained
syn match autoitSend "{F7}" contained
syn match autoitSend "{F8}" contained
syn match autoitSend "{F9}" contained
syn match autoitSend "{F10}" contained
syn match autoitSend "{F11}" contained
syn match autoitSend "{F12}" contained
syn match autoitSend "{TAB}" contained
syn match autoitSend "{PRINTSCREEN}" contained
syn match autoitSend "{LWIN}" contained
syn match autoitSend "{RWIN}" contained
syn match autoitSend "{NUMLOCK}" contained
syn match autoitSend "{BREAK}" contained
syn match autoitSend "{PAUSE}" contained
syn match autoitSend "{CAPSLOCK}" contained
syn match autoitSend "{NUMPAD0}" contained
syn match autoitSend "{NUMPAD1}" contained
syn match autoitSend "{NUMPAD2}" contained
syn match autoitSend "{NUMPAD3}" contained
syn match autoitSend "{NUMPAD4}" contained
syn match autoitSend "{NUMPAD5}" contained
syn match autoitSend "{NUMPAD6}" contained
syn match autoitSend "{NUMPAD7}" contained
syn match autoitSend "{NUMPAD8}" contained
syn match autoitSend "{NUMPAD9}" contained
syn match autoitSend "{NUMPADMULT}" contained
syn match autoitSend "{NUMPADADD}" contained
syn match autoitSend "{NUMPADSUB}" contained
syn match autoitSend "{NUMPADDIV}" contained
syn match autoitSend "{NUMPADDOT}" contained
syn match autoitSend "{NUMPADENTER}" contained
syn match autoitSend "{APPSKEY}" contained
syn match autoitSend "{LALT}" contained
syn match autoitSend "{RALT}" contained
syn match autoitSend "{LCTRL}" contained
syn match autoitSend "{RCTRL}" contained
syn match autoitSend "{LSHIFT}" contained
syn match autoitSend "{RSHIFT}" contained
syn match autoitSend "{SLEEP}" contained
syn match autoitSend "{ALTDOWN}" contained
syn match autoitSend "{SHIFTDOWN}" contained
syn match autoitSend "{CTRLDOWN}" contained
syn match autoitSend "{LWINDOWN}" contained
syn match autoitSend "{RWINDOWN}" contained
syn match autoitSend "{ASC \d\d\d\d}" contained
syn match autoitSend "{BROWSER_BACK}" contained
syn match autoitSend "{BROWSER_FORWARD}" contained
syn match autoitSend "{BROWSER_REFRESH}" contained
syn match autoitSend "{BROWSER_STOP}" contained
syn match autoitSend "{BROWSER_SEARCH}" contained
syn match autoitSend "{BROWSER_FAVORITES}" contained
syn match autoitSend "{BROWSER_HOME}" contained
syn match autoitSend "{VOLUME_MUTE}" contained
syn match autoitSend "{VOLUME_DOWN}" contained
syn match autoitSend "{VOLUME_UP}" contained
syn match autoitSend "{MEDIA_NEXT}" contained
syn match autoitSend "{MEDIA_PREV}" contained
syn match autoitSend "{MEDIA_STOP}" contained
syn match autoitSend "{MEDIA_PLAY_PAUSE}" contained
syn match autoitSend "{LAUNCH_MAIL}" contained
syn match autoitSend "{LAUNCH_MEDIA}" contained
syn match autoitSend "{LAUNCH_APP1}" contained
syn match autoitSend "{LAUNCH_APP2}" contained

"this was tricky!
"we use an oneline region, instead of a match, in order to use skip=
"matchgroup= so start and end quotes are not considered as au3Doubled
"contained
syn region autoitString oneline contains=autoitSend matchgroup=autoitQuote start=+"+
	\ end=+"+ end=+_\n\{1}.*"+
	\ contains=autoitCont,autoitDoubledDoubles skip=+""+
syn region autoitString oneline matchgroup=autoitQuote start=+'+
	\ end=+'+ end=+_\n\{1}.*'+
	\ contains=autoitCont,autoitDoubledSingles skip=+''+

syn match autoitVarSelector "\$"	contained display
syn match autoitVariable "$\w\+" contains=autoitVarSelector

" options - must be defined after autoitStrings
syn match autoitOption "\([\"\']\)CaretCoordMode\1"
syn match autoitOption "\([\"\']\)ExpandEnvStrings\1"
syn match autoitOption "\([\"\']\)ExpandVarStrings\1"
syn match autoitOption "\([\"\']\)FtpBinaryMode\1"
syn match autoitOption "\([\"\']\)GUICloseOnEsc\1"
syn match autoitOption "\([\"\']\)GUICoordMode\1"
syn match autoitOption "\([\"\']\)GUIDataSeparatorChar\1"
syn match autoitOption "\([\"\']\)GUIOnEventMode\1"
syn match autoitOption "\([\"\']\)GUIResizeMode\1"
syn match autoitOption "\([\"\']\)GUIEventCompatibilityMode\1"
syn match autoitOption "\([\"\']\)MouseClickDelay\1"
syn match autoitOption "\([\"\']\)MouseClickDownDelay\1"
syn match autoitOption "\([\"\']\)MouseClickDragDelay\1"
syn match autoitOption "\([\"\']\)MouseCoordMode\1"
syn match autoitOption "\([\"\']\)MustDeclareVars\1"
syn match autoitOption "\([\"\']\)OnExitFunc\1"
syn match autoitOption "\([\"\']\)PixelCoordMode\1"
syn match autoitOption "\([\"\']\)SendAttachMode\1"
syn match autoitOption "\([\"\']\)SendCapslockMode\1"
syn match autoitOption "\([\"\']\)SendKeyDelay\1"
syn match autoitOption "\([\"\']\)SendKeyDownDelay\1"
syn match autoitOption "\([\"\']\)TCPTimeout\1"
syn match autoitOption "\([\"\']\)TrayAutoPause\1"
syn match autoitOption "\([\"\']\)TrayIconDebug\1"
syn match autoitOption "\([\"\']\)TrayIconHide\1"
syn match autoitOption "\([\"\']\)TrayMenuMode\1"
syn match autoitOption "\([\"\']\)TrayOnEventMode\1"
syn match autoitOption "\([\"\']\)WinDetectHiddenText\1"
syn match autoitOption "\([\"\']\)WinSearchChildren\1"
syn match autoitOption "\([\"\']\)WinTextMatchMode\1"
syn match autoitOption "\([\"\']\)WinTitleMatchMode\1"
syn match autoitOption "\([\"\']\)WinWaitDelay\1"

" styles - must be defined after autoitVariable
" common
syn match autoitStyle "\$WS_BORDER"
syn match autoitStyle "\$WS_POPUP"
syn match autoitStyle "\$WS_CAPTION"
syn match autoitStyle "\$WS_CLIPCHILDREN"
syn match autoitStyle "\$WS_CLIPSIBLINGS"
syn match autoitStyle "\$WS_DISABLED"
syn match autoitStyle "\$WS_DLGFRAME"
syn match autoitStyle "\$WS_HSCROLL"
syn match autoitStyle "\$WS_MAXIMIZE"
syn match autoitStyle "\$WS_MAXIMIZEBOX"
syn match autoitStyle "\$WS_MINIMIZE"
syn match autoitStyle "\$WS_MINIMIZEBOX"
syn match autoitStyle "\$WS_OVERLAPPED"
syn match autoitStyle "\$WS_OVERLAPPEDWINDOW"
syn match autoitStyle "\$WS_POPUPWINDOW"
syn match autoitStyle "\$WS_SIZEBOX"
syn match autoitStyle "\$WS_SYSMENU"
syn match autoitStyle "\$WS_THICKFRAME"
syn match autoitStyle "\$WS_VSCROLL"
syn match autoitStyle "\$WS_VISIBLE"
syn match autoitStyle "\$WS_CHILD"
syn match autoitStyle "\$WS_GROUP"
syn match autoitStyle "\$WS_TABSTOP"
syn match autoitStyle "\$DS_MODALFRAME"
syn match autoitStyle "\$DS_SETFOREGROUND"
syn match autoitStyle "\$DS_CONTEXTHELP"
" common extended
syn match autoitStyle "\$WS_EX_ACCEPTFILES"
syn match autoitStyle "\$WS_EX_APPWINDOW"
syn match autoitStyle "\$WS_EX_CLIENTEDGE"
syn match autoitStyle "\$WS_EX_CONTEXTHELP"
syn match autoitStyle "\$WS_EX_DLGMODALFRAME"
syn match autoitStyle "\$WS_EX_MDICHILD"
syn match autoitStyle "\$WS_EX_OVERLAPPEDWINDOW"
syn match autoitStyle "\$WS_EX_STATICEDGE"
syn match autoitStyle "\$WS_EX_TOPMOST"
syn match autoitStyle "\$WS_EX_TRANSPARENT"
syn match autoitStyle "\$WS_EX_TOOLWINDOW"
syn match autoitStyle "\$WS_EX_WINDOWEDGE"
syn match autoitStyle "\$WS_EX_LAYERED"
syn match autoitStyle "\$GUI_WS_EX_PARENTDRAG"
" checkbox
syn match autoitStyle "\$BS_3STATE"
syn match autoitStyle "\$BS_AUTO3STATE"
syn match autoitStyle "\$BS_AUTOCHECKBOX"
syn match autoitStyle "\$BS_CHECKBOX"
syn match autoitStyle "\$BS_LEFT"
syn match autoitStyle "\$BS_PUSHLIKE"
syn match autoitStyle "\$BS_RIGHT"
syn match autoitStyle "\$BS_RIGHTBUTTON"
syn match autoitStyle "\$BS_GROUPBOX"
syn match autoitStyle "\$BS_AUTORADIOBUTTON"
" push button
syn match autoitStyle "\$BS_BOTTOM"
syn match autoitStyle "\$BS_CENTER"
syn match autoitStyle "\$BS_DEFPUSHBUTTON"
syn match autoitStyle "\$BS_MULTILINE"
syn match autoitStyle "\$BS_TOP"
syn match autoitStyle "\$BS_VCENTER"
syn match autoitStyle "\$BS_ICON"
syn match autoitStyle "\$BS_BITMAP"
syn match autoitStyle "\$BS_FLAT"
" combo
syn match autoitStyle "\$CBS_AUTOHSCROLL"
syn match autoitStyle "\$CBS_DISABLENOSCROLL"
syn match autoitStyle "\$CBS_DROPDOWN"
syn match autoitStyle "\$CBS_DROPDOWNLIST"
syn match autoitStyle "\$CBS_LOWERCASE"
syn match autoitStyle "\$CBS_NOINTEGRALHEIGHT"
syn match autoitStyle "\$CBS_OEMCONVERT"
syn match autoitStyle "\$CBS_SIMPLE"
syn match autoitStyle "\$CBS_SORT"
syn match autoitStyle "\$CBS_UPPERCASE"
" list
syn match autoitStyle "\$LBS_DISABLENOSCROLL"
syn match autoitStyle "\$LBS_NOINTEGRALHEIGHT"
syn match autoitStyle "\$LBS_NOSEL"
syn match autoitStyle "\$LBS_NOTIFY"
syn match autoitStyle "\$LBS_SORT"
syn match autoitStyle "\$LBS_STANDARD"
syn match autoitStyle "\$LBS_USETABSTOPS"
" edit/input
syn match autoitStyle "\$ES_AUTOHSCROLL"
syn match autoitStyle "\$ES_AUTOVSCROLL"
syn match autoitStyle "\$ES_CENTER"
syn match autoitStyle "\$ES_LOWERCASE"
syn match autoitStyle "\$ES_NOHIDESEL"
syn match autoitStyle "\$ES_NUMBER"
syn match autoitStyle "\$ES_OEMCONVERT"
syn match autoitStyle "\$ES_MULTILINE"
syn match autoitStyle "\$ES_PASSWORD"
syn match autoitStyle "\$ES_READONLY"
syn match autoitStyle "\$ES_RIGHT"
syn match autoitStyle "\$ES_UPPERCASE"
syn match autoitStyle "\$ES_WANTRETURN"
" progress bar
syn match autoitStyle "\$PBS_SMOOTH"
syn match autoitStyle "\$PBS_VERTICAL"
" up-down
syn match autoitStyle "\$UDS_ALIGNLEFT"
syn match autoitStyle "\$UDS_ALIGNRIGHT"
syn match autoitStyle "\$UDS_ARROWKEYS"
syn match autoitStyle "\$UDS_HORZ"
syn match autoitStyle "\$UDS_NOTHOUSANDS"
syn match autoitStyle "\$UDS_WRAP"
" label/static
syn match autoitStyle "\$SS_BLACKFRAME"
syn match autoitStyle "\$SS_BLACKRECT"
syn match autoitStyle "\$SS_CENTER"
syn match autoitStyle "\$SS_CENTERIMAGE"
syn match autoitStyle "\$SS_ETCHEDFRAME"
syn match autoitStyle "\$SS_ETCHEDHORZ"
syn match autoitStyle "\$SS_ETCHEDVERT"
syn match autoitStyle "\$SS_GRAYFRAME"
syn match autoitStyle "\$SS_GRAYRECT"
syn match autoitStyle "\$SS_LEFT"
syn match autoitStyle "\$SS_LEFTNOWORDWRAP"
syn match autoitStyle "\$SS_NOPREFIX"
syn match autoitStyle "\$SS_NOTIFY"
syn match autoitStyle "\$SS_RIGHT"
syn match autoitStyle "\$SS_RIGHTJUST"
syn match autoitStyle "\$SS_SIMPLE"
syn match autoitStyle "\$SS_SUNKEN"
syn match autoitStyle "\$SS_WHITEFRAME"
syn match autoitStyle "\$SS_WHITERECT"
" tab
syn match autoitStyle "\$TCS_SCROLLOPPOSITE"
syn match autoitStyle "\$TCS_BOTTOM"
syn match autoitStyle "\$TCS_RIGHT"
syn match autoitStyle "\$TCS_MULTISELECT"
syn match autoitStyle "\$TCS_FLATBUTTONS"
syn match autoitStyle "\$TCS_FORCEICONLEFT"
syn match autoitStyle "\$TCS_FORCELABELLEFT"
syn match autoitStyle "\$TCS_HOTTRACK"
syn match autoitStyle "\$TCS_VERTICAL"
syn match autoitStyle "\$TCS_TABS"
syn match autoitStyle "\$TCS_BUTTONS"
syn match autoitStyle "\$TCS_SINGLELINE"
syn match autoitStyle "\$TCS_MULTILINE"
syn match autoitStyle "\$TCS_RIGHTJUSTIFY"
syn match autoitStyle "\$TCS_FIXEDWIDTH"
syn match autoitStyle "\$TCS_RAGGEDRIGHT"
syn match autoitStyle "\$TCS_FOCUSONBUTTONDOWN"
syn match autoitStyle "\$TCS_OWNERDRAWFIXED"
syn match autoitStyle "\$TCS_TOOLTIPS"
syn match autoitStyle "\$TCS_FOCUSNEVER"
" avi clip
syn match autoitStyle "\$ACS_AUTOPLAY"
syn match autoitStyle "\$ACS_CENTER"
syn match autoitStyle "\$ACS_TRANSPARENT"
syn match autoitStyle "\$ACS_NONTRANSPARENT"
" date
syn match autoitStyle "\$DTS_UPDOWN"
syn match autoitStyle "\$DTS_SHOWNONE"
syn match autoitStyle "\$DTS_LONGDATEFORMAT"
syn match autoitStyle "\$DTS_TIMEFORMAT"
syn match autoitStyle "\$DTS_RIGHTALIGN"
syn match autoitStyle "\$DTS_SHORTDATEFORMAT"
" monthcal
syn match autoitStyle "\$MCS_NOTODAY"
syn match autoitStyle "\$MCS_NOTODAYCIRCLE"
syn match autoitStyle "\$MCS_WEEKNUMBERS"
" treeview
syn match autoitStyle "\$TVS_HASBUTTONS"
syn match autoitStyle "\$TVS_HASLINES"
syn match autoitStyle "\$TVS_LINESATROOT"
syn match autoitStyle "\$TVS_DISABLEDRAGDROP"
syn match autoitStyle "\$TVS_SHOWSELALWAYS"
syn match autoitStyle "\$TVS_RTLREADING"
syn match autoitStyle "\$TVS_NOTOOLTIPS"
syn match autoitStyle "\$TVS_CHECKBOXES"
syn match autoitStyle "\$TVS_TRACKSELECT"
syn match autoitStyle "\$TVS_SINGLEEXPAND"
syn match autoitStyle "\$TVS_FULLROWSELECT"
syn match autoitStyle "\$TVS_NOSCROLL"
syn match autoitStyle "\$TVS_NONEVENHEIGHT"
" slider
syn match autoitStyle "\$TBS_AUTOTICKS"
syn match autoitStyle "\$TBS_BOTH"
syn match autoitStyle "\$TBS_BOTTOM"
syn match autoitStyle "\$TBS_HORZ"
syn match autoitStyle "\$TBS_VERT"
syn match autoitStyle "\$TBS_NOTHUMB"
syn match autoitStyle "\$TBS_NOTICKS"
syn match autoitStyle "\$TBS_LEFT"
syn match autoitStyle "\$TBS_RIGHT"
syn match autoitStyle "\$TBS_TOP"
" listview
syn match autoitStyle "\$LVS_ICON"
syn match autoitStyle "\$LVS_REPORT"
syn match autoitStyle "\$LVS_SMALLICON"
syn match autoitStyle "\$LVS_LIST"
syn match autoitStyle "\$LVS_EDITLABELS"
syn match autoitStyle "\$LVS_NOCOLUMNHEADER"
syn match autoitStyle "\$LVS_NOSORTHEADER"
syn match autoitStyle "\$LVS_SINGLESEL"
syn match autoitStyle "\$LVS_SHOWSELALWAYS"
syn match autoitStyle "\$LVS_SORTASCENDING"
syn match autoitStyle "\$LVS_SORTDESCENDING"
" listview extended
syn match autoitStyle "\$LVS_EX_FULLROWSELECT"
syn match autoitStyle "\$LVS_EX_GRIDLINES"
syn match autoitStyle "\$LVS_EX_HEADERDRAGDROP"
syn match autoitStyle "\$LVS_EX_TRACKSELECT"
syn match autoitStyle "\$LVS_EX_CHECKBOXES"
syn match autoitStyle "\$LVS_EX_BORDERSELECT"
syn match autoitStyle "\$LVS_EX_DOUBLEBUFFER"
syn match autoitStyle "\$LVS_EX_FLATSB"
syn match autoitStyle "\$LVS_EX_MULTIWORKAREAS"
syn match autoitStyle "\$LVS_EX_SNAPTOGRID"
syn match autoitStyle "\$LVS_EX_SUBITEMIMAGES"

" constants - must be defined after autoitVariable - excludes styles
" constants - autoit options
syn match autoitConst "\$OPT_COORDSRELATIVE"
syn match autoitConst "\$OPT_COORDSABSOLUTE"
syn match autoitConst "\$OPT_COORDSCLIENT"
syn match autoitConst "\$OPT_ERRORSILENT"
syn match autoitConst "\$OPT_ERRORFATAL"
syn match autoitConst "\$OPT_CAPSNOSTORE"
syn match autoitConst "\$OPT_CAPSSTORE"
syn match autoitConst "\$OPT_MATCHSTART"
syn match autoitConst "\$OPT_MATCHANY"
syn match autoitConst "\$OPT_MATCHEXACT"
syn match autoitConst "\$OPT_MATCHADVANCED"
" constants - common control styles
syn match autoitConst "\$CCS_TOP"
syn match autoitConst "\$CCS_NOMOVEY"
syn match autoitConst "\$CCS_BOTTOM"
syn match autoitConst "\$CCS_NORESIZE"
syn match autoitConst "\$CCS_NOPARENTALIGN"
syn match autoitConst "\$CCS_NOHILITE"
syn match autoitConst "\$CCS_ADJUSTABLE"
syn match autoitConst "\$CCS_NODIVIDER"
syn match autoitConst "\$CCS_VERT"
syn match autoitConst "\$CCS_LEFT"
syn match autoitConst "\$CCS_NOMOVEX"
syn match autoitConst "\$CCS_RIGHT"
" constants - drawiconex
syn match autoitConst "\$DI_MASK"
syn match autoitConst "\$DI_IMAGE"
syn match autoitConst "\$DI_NORMAL"
syn match autoitConst "\$DI_COMPAT"
syn match autoitConst "\$DI_DEFAULTSIZE"
syn match autoitConst "\$DI_NOMIRROT"
" constants - enumdisplaydevice
syn match autoitConst "\$DISPLAY_DEVICE_ATTACHED_TO_DESKTOP"
syn match autoitConst "\$DISPLAY_DEVICE_MULTI_DRIVER"
syn match autoitConst "\$DISPLAY_DEVICE_PRIMARY_DEVICE"
syn match autoitConst "\$DISPLAY_DEVICE_MIRRORING_DRIVER"
syn match autoitConst "\$DISPLAY_DEVICE_VGA_COMPATIBLE"
syn match autoitConst "\$DISPLAY_DEVICE_REMOVABLE"
syn match autoitConst "\$DISPLAY_DEVICE_DISCONNECT"
syn match autoitConst "\$DISPLAY_DEVICE_REMOTE"
syn match autoitConst "\$DISPLAY_DEVICE_MODESPRUNED"
" constants - dir
syn match autoitConst "\$DDL_ARCHIVE"
syn match autoitConst "\$DDL_DIRECTORY"
syn match autoitConst "\$DDL_DRIVES"
syn match autoitConst "\$DDL_EXCLUSIVE"
syn match autoitConst "\$DDL_HIDDEN"
syn match autoitConst "\$DDL_READONLY"
syn match autoitConst "\$DDL_READWRITE"
syn match autoitConst "\$DDL_SYSTEM"
" constants - file copy
syn match autoitConst "\$FC_NOOVERWRITE"
syn match autoitConst "\$FC_OVERWRITE"
" constants - file time
syn match autoitConst "\$FT_MODIFIED"
syn match autoitConst "\$FT_CREATED"
syn match autoitConst "\$FT_ACCESSED"
" constants - file open
syn match autoitConst "\$FO_READ"
syn match autoitConst "\$FO_APPEND"
syn match autoitConst "\$FO_OVERWRITE"
syn match autoitConst "\$FO_BINARY"
syn match autoitConst "\$FO_UNICODE"
syn match autoitConst "\$FO_UTF16_LE"
syn match autoitConst "\$FO_UTF16_BE"
syn match autoitConst "\$FO_UTF8"
" constants - file read
syn match autoitConst "\$EOF"
" constants - file dialog
syn match autoitConst "\$FD_FILEMUSTEXIST"
syn match autoitConst "\$FD_PATHMUSTEXIST"
syn match autoitConst "\$FD_MULTISELECT"
syn match autoitConst "\$FD_PROMPTCREATENEW"
syn match autoitConst "\$FD_PROMPTOVERWRITE"
" constants - file miscellaneous
syn match autoitConst "\$CREATE_NEW"
syn match autoitConst "\$CREATE_ALWAYS"
syn match autoitConst "\$OPEN_EXISTING"
syn match autoitConst "\$OPEN_ALWAYS"
syn match autoitConst "\$TRUNCATE_EXISTING"
syn match autoitConst "\$INVALID_SET_FILE_POINTER"
" constants - file starting point
syn match autoitConst "\$FILE_BEGIN"
syn match autoitConst "\$FILE_CURRENT"
syn match autoitConst "\$FILE_END"
syn match autoitConst "\$FILE_ATTRIBUTE_READONLY"
syn match autoitConst "\$FILE_ATTRIBUTE_HIDDEN"
syn match autoitConst "\$FILE_ATTRIBUTE_SYSTEM"
syn match autoitConst "\$FILE_ATTRIBUTE_DIRECTORY"
syn match autoitConst "\$FILE_ATTRIBUTE_ARCHIVE"
syn match autoitConst "\$FILE_ATTRIBUTE_DEVICE"
syn match autoitConst "\$FILE_ATTRIBUTE_NORMAL"
syn match autoitConst "\$FILE_ATTRIBUTE_TEMPORARY"
syn match autoitConst "\$FILE_ATTRIBUTE_SPARSE_FILE"
syn match autoitConst "\$FILE_ATTRIBUTE_REPARSE_POINT"
syn match autoitConst "\$FILE_ATTRIBUTE_COMPRESSED"
syn match autoitConst "\$FILE_ATTRIBUTE_OFFLINE"
syn match autoitConst "\$FILE_ATTRIBUTE_NOT_CONTENT_INDEXED"
syn match autoitConst "\$FILE_ATTRIBUTE_ENCRYPTED"
syn match autoitConst "\$FILE_SHARE_READ"
syn match autoitConst "\$FILE_SHARE_WRITE"
syn match autoitConst "\$FILE_SHARE_DELETE"
syn match autoitConst "\$GENERIC_ALL"
syn match autoitConst "\$GENERIC_EXECUTE"
syn match autoitConst "\$GENERIC_WRITE"
syn match autoitConst "\$GENERIC_READ"
" constants - flashwindowex
syn match autoitConst "\$FLASHW_CAPTION"
syn match autoitConst "\$FLASHW_TRAY"
syn match autoitConst "\$FLASHW_TIMER"
syn match autoitConst "\$FLASHW_TIMERNOFG"
" constants - formatmessage
syn match autoitConst "\$FORMAT_MESSAGE_ALLOCATE_BUFFER"
syn match autoitConst "\$FORMAT_MESSAGE_IGNORE_INSERTS"
syn match autoitConst "\$FORMAT_MESSAGE_FROM_STRING"
syn match autoitConst "\$FORMAT_MESSAGE_FROM_HMODULE"
syn match autoitConst "\$FORMAT_MESSAGE_FROM_SYSTEM"
syn match autoitConst "\$FORMAT_MESSAGE_ARGUMENT_ARRAY"
" constants - getwindows
syn match autoitConst "\$GW_HWNDFIRST"
syn match autoitConst "\$GW_HWNDLAST"
syn match autoitConst "\$GW_HWNDNEXT"
syn match autoitConst "\$GW_HWNDPREV"
syn match autoitConst "\$GW_OWNER"
syn match autoitConst "\$GW_CHILD"
" constants - getwindowlong
syn match autoitConst "\$GWL_WNDPROC"
syn match autoitConst "\$GWL_HINSTANCE"
syn match autoitConst "\$GWL_HWNDPARENT"
syn match autoitConst "\$GWL_ID"
syn match autoitConst "\$GWL_STYLE"
syn match autoitConst "\$GWL_EXSTYLE"
syn match autoitConst "\$GWL_USERDATA"
" constants - standard icon index
syn match autoitConst "\$STD_CUT"
syn match autoitConst "\$STD_COPY"
syn match autoitConst "\$STD_PASTE"
syn match autoitConst "\$STD_UNDO"
syn match autoitConst "\$STD_REDOW"
syn match autoitConst "\$STD_DELETE"
syn match autoitConst "\$STD_FILENEW"
syn match autoitConst "\$STD_FILEOPEN"
syn match autoitConst "\$STD_FILESAVE"
syn match autoitConst "\$STD_PRINTPRE"
syn match autoitConst "\$STD_PROPERTIES"
syn match autoitConst "\$STD_HELP"
syn match autoitConst "\$STD_FIND"
syn match autoitConst "\$STD_REPLACE"
syn match autoitConst "\$STD_PRINT"
" constants - image load
syn match autoitConst "\$LR_DEFAULTCOLOR"
syn match autoitConst "\$LR_MONOCHROME"
syn match autoitConst "\$LR_COLOR"
syn match autoitConst "\$LR_COPYRETURNORG"
syn match autoitConst "\$LR_COPYDELETEORG"
syn match autoitConst "\$LR_LOADFROMFILE"
syn match autoitConst "\$LR_LOADTRANSPARENT"
syn match autoitConst "\$LR_DEFAULTSIZE"
syn match autoitConst "\$LR_VGACOLOR"
syn match autoitConst "\$LR_LOADMAP3DCOLORS"
syn match autoitConst "\$LR_CREATEDIBSECTION"
syn match autoitConst "\$LR_COPYFROMRESOURCE"
syn match autoitConst "\$LR_SHARED"
" constants - image type
syn match autoitConst "\$IMAGE_BITMAP"
syn match autoitConst "\$IMAGE_ICON"
syn match autoitConst "\$IMAGE_CURSOR"
" constants - keyboard
syn match autoitConst "\$KB_SENDSPECIAL"
syn match autoitConst "\$KB_SENDRAW"
syn match autoitConst "\$KB_CAPSOFF"
syn match autoitConst "\$KB_CAPSON"
" constants - loadlibraryex
syn match autoitConst "\$DONT_RESOLVE_DLL_REFERENCES"
syn match autoitConst "\$LOAD_LIBRARY_AS_DATAFILE"
syn match autoitConst "\$LOAD_WITH_ALTERED_SEARCH_PATH"
" constants - reserved ids for system objects
syn match autoitConst "\$OBJID_WINDOW"
syn match autoitConst "\$OBJID_SYSMENU"
syn match autoitConst "\$OBJID_TITLEBAR"
syn match autoitConst "\$OBJID_MENU"
syn match autoitConst "\$OBJID_SIZEGRIP"
syn match autoitConst "\$OBJID_CARET"
syn match autoitConst "\$OBJID_CURSOR"
syn match autoitConst "\$OBJID_ALERT"
syn match autoitConst "\$OBJID_SOUND"
" constants - virtual keys
syn match autoitConst "\$VK_DOWN"
syn match autoitConst "\$VK_END"
syn match autoitConst "\$VK_HOME"
syn match autoitConst "\$VK_LEFT"
syn match autoitConst "\$VK_NEXT"
syn match autoitConst "\$VK_PRIOR"
syn match autoitConst "\$VK_RIGHT"
syn match autoitConst "\$VK_UP"
" constants - message box
syn match autoitConst "\$MB_OK"
syn match autoitConst "\$MB_OKCANCEL"
syn match autoitConst "\$MB_ABORTRETRYIGNORE"
syn match autoitConst "\$MB_YESNOCANCEL"
syn match autoitConst "\$MB_YESNO"
syn match autoitConst "\$MB_RETRYCANCEL"
syn match autoitConst "\$MB_ICONHAND"
syn match autoitConst "\$MB_ICONQUESTION"
syn match autoitConst "\$MB_ICONEXCLAMATION"
syn match autoitConst "\$MB_ICONASTERISK"
syn match autoitConst "\$MB_DEFBUTTON1"
syn match autoitConst "\$MB_DEFBUTTON2"
syn match autoitConst "\$MB_DEFBUTTON3"
syn match autoitConst "\$MB_APPLMODAL"
syn match autoitConst "\$MB_SYSTEMMODAL"
syn match autoitConst "\$MB_TASKMODAL"
syn match autoitConst "\$MB_TOPMOST"
syn match autoitConst "\$MB_RIGHTJUSTIFIED"
syn match autoitConst "\$IDTIMEOUT"
syn match autoitConst "\$IDOK"
syn match autoitConst "\$IDCANCEL"
syn match autoitConst "\$IDABORT"
syn match autoitConst "\$IDRETRY"
syn match autoitConst "\$IDIGNORE"
syn match autoitConst "\$IDYES"
syn match autoitConst "\$IDNO"
syn match autoitConst "\$IDTRYAGAIN"
syn match autoitConst "\$IDCONTINUE"
" constants - progress and splash
syn match autoitConst "\$DLG_NOTITLE"
syn match autoitConst "\$DLG_NOTONTOP"
syn match autoitConst "\$DLG_TEXTLEFT"
syn match autoitConst "\$DLG_TEXTRIGHT"
syn match autoitConst "\$DLG_MOVEABLE"
syn match autoitConst "\$DLG_TEXTVCENTER"
" constants - tray tip
syn match autoitConst "\$TIP_ICONNONE"
syn match autoitConst "\$TIP_ICONASTERISK"
syn match autoitConst "\$TIP_ICONEXCLAMATION"
syn match autoitConst "\$TIP_ICONHAND"
syn match autoitConst "\$TIP_NOSOUND"
" constants - mouse
syn match autoitConst "\$IDC_UNKNOWN"
syn match autoitConst "\$IDC_APPSTARTING"
syn match autoitConst "\$IDC_ARROW"
syn match autoitConst "\$IDC_CROSS"
syn match autoitConst "\$IDC_HAND"
syn match autoitConst "\$IDC_HELP"
syn match autoitConst "\$IDC_IBEAM"
syn match autoitConst "\$IDC_ICON"
syn match autoitConst "\$IDC_NO"
syn match autoitConst "\$IDC_SIZE"
syn match autoitConst "\$IDC_SIZEALL"
syn match autoitConst "\$IDC_SIZENESW"
syn match autoitConst "\$IDC_SIZENS"
syn match autoitConst "\$IDC_SIZENWSE"
syn match autoitConst "\$IDC_SIZEWE"
syn match autoitConst "\$IDC_UPARROW"
syn match autoitConst "\$IDC_WAIT"
syn match autoitConst "\$IDI_APPLICATION"
syn match autoitConst "\$IDI_ASTERISK"
syn match autoitConst "\$IDI_EXCLAMATION"
syn match autoitConst "\$IDI_HAND"
syn match autoitConst "\$IDI_QUESTION"
syn match autoitConst "\$IDI_WINLOGO"
" constants - process
syn match autoitConst "\$SD_LOGOFF"
syn match autoitConst "\$SD_SHUTDOWN"
syn match autoitConst "\$SD_REBOOT"
syn match autoitConst "\$SD_FORCE"
syn match autoitConst "\$SD_POWERDOWN"
" constants - open process
syn match autoitConst "\$PROCESS_TERMINATE"
syn match autoitConst "\$PROCESS_CREATE_THREAD"
syn match autoitConst "\$PROCESS_SET_SESSIONID"
syn match autoitConst "\$PROCESS_VM_OPERATION"
syn match autoitConst "\$PROCESS_VM_READ"
syn match autoitConst "\$PROCESS_VM_WRITE"
syn match autoitConst "\$PROCESS_DUP_HANDLE"
syn match autoitConst "\$PROCESS_CREATE_PROCESS"
syn match autoitConst "\$PROCESS_SET_QUOTA"
syn match autoitConst "\$PROCESS_SET_INFORMATION"
syn match autoitConst "\$PROCESS_QUERY_INFORMATION"
syn match autoitConst "\$PROCESS_SUSPEND_RESUME"
syn match autoitConst "\$PROCESS_ALL_ACCESS"
" constants - string
syn match autoitConst "\$STR_NOCASESENSE"
syn match autoitConst "\$STR_CASESENSE"
syn match autoitConst "\STR_STRIPLEADING"
syn match autoitConst "\$STR_STRIPTRAILING"
syn match autoitConst "\$STR_STRIPSPACES"
syn match autoitConst "\$STR_STRIPALL"
" constants - token
syn match autoitConst "\$TOKEN_ASSIGN_PRIMARY"
syn match autoitConst "\$TOKEN_DUPLICATE"
syn match autoitConst "\$TOKEN_IMPERSONATE"
syn match autoitConst "\$TOKEN_QUERY"
syn match autoitConst "\$TOKEN_QUERY_SOURCE"
syn match autoitConst "\$TOKEN_ADJUST_PRIVILEGES"
syn match autoitConst "\$TOKEN_ADJUST_GROUPS"
syn match autoitConst "\$TOKEN_ADJUST_DEFAULT"
syn match autoitConst "\$TOKEN_ADJUST_SESSIONID"
" constants - tray
syn match autoitConst "\$TRAY_ITEM_EXIT"
syn match autoitConst "\$TRAY_ITEM_PAUSE"
syn match autoitConst "\$TRAY_ITEM_FIRST"
syn match autoitConst "\$TRAY_CHECKED"
syn match autoitConst "\$TRAY_UNCHECKED"
syn match autoitConst "\$TRAY_ENABLE"
syn match autoitConst "\$TRAY_DISABLE"
syn match autoitConst "\$TRAY_FOCUS"
syn match autoitConst "\$TRAY_DEFAULT"
syn match autoitConst "\$TRAY_EVENT_SHOWICON"
syn match autoitConst "\$TRAY_EVENT_HIDEICON"
syn match autoitConst "\$TRAY_EVENT_FLASHICON"
syn match autoitConst "\$TRAY_EVENT_NOFLASHICON"
syn match autoitConst "\$TRAY_EVENT_PRIMARYDOWN"
syn match autoitConst "\$TRAY_EVENT_PRIMARYUP"
syn match autoitConst "\$TRAY_EVENT_SECONDARYDOWN"
syn match autoitConst "\$TRAY_EVENT_SECONDARYUP"
syn match autoitConst "\$TRAY_EVENT_MOUSEOVER"
syn match autoitConst "\$TRAY_EVENT_MOUSEOUT"
syn match autoitConst "\$TRAY_EVENT_PRIMARYDOUBLE"
syn match autoitConst "\$TRAY_EVENT_SECONDARYDOUBLE"
" constants - run
syn match autoitConst "\$STDIN_CHILD"
syn match autoitConst "\$STDOUT_CHILD"
syn match autoitConst "\$STDERR_CHILD"
syn match autoitConst "\$STDERR_MERGED"
syn match autoitConst "\$STDIO_INHERIT_PARENT"
syn match autoitConst "\$RUN_CREATE_NEW_CONSOLE"
" constants - color, rgb
syn match autoitConst "\$COLOR_AQUA"
syn match autoitConst "\$COLOR_BLACK"
syn match autoitConst "\$COLOR_BLUE"
syn match autoitConst "\$COLOR_CREAM"
syn match autoitConst "\$COLOR_FUCHSIA"
syn match autoitConst "\$COLOR_GRAY"
syn match autoitConst "\$COLOR_GREEN"
syn match autoitConst "\$COLOR_LIME"
syn match autoitConst "\$COLOR_MAROON"
syn match autoitConst "\$COLOR_MEDBLUE"
syn match autoitConst "\$COLOR_MEDGRAY"
syn match autoitConst "\$COLOR_MONEYGREEN"
syn match autoitConst "\$COLOR_NAVY"
syn match autoitConst "\$COLOR_OLIVE"
syn match autoitConst "\$COLOR_PURPLE"
syn match autoitConst "\$COLOR_RED"
syn match autoitConst "\$COLOR_SILVER"
syn match autoitConst "\$COLOR_SKYBLUE"
syn match autoitConst "\$COLOR_TEAL"
syn match autoitConst "\$COLOR_WHITE"
syn match autoitConst "\$COLOR_YELLOW"
syn match autoitConst "\$CLR_NONE"
" constants - color, bgr
syn match autoitConst "\$CLR_AQUA"
syn match autoitConst "\$CLR_BLACK"
syn match autoitConst "\$CLR_BLUE"
syn match autoitConst "\$CLR_CREAM"
syn match autoitConst "\$CLR_DEFAULT"
syn match autoitConst "\$CLR_FUCHSIA"
syn match autoitConst "\$CLR_GRAY"
syn match autoitConst "\$CLR_GREEN"
syn match autoitConst "\$CLR_LIME"
syn match autoitConst "\$CLR_MAROON"
syn match autoitConst "\$CLR_MEDBLUE"
syn match autoitConst "\$CLR_MEDGRAY"
syn match autoitConst "\$CLR_MONEYGREEN"
syn match autoitConst "\$CLR_NAVY"
syn match autoitConst "\$CLR_OLIVE"
syn match autoitConst "\$CLR_PURPLE"
syn match autoitConst "\$CLR_RED"
syn match autoitConst "\$CLR_SILVER"
syn match autoitConst "\$CLR_SKYBLUE"
syn match autoitConst "\$CLR_TEAL"
syn match autoitConst "\$CLR_WHITE"
syn match autoitConst "\$CLR_YELLOW"
" constants - color, dialog
syn match autoitConst "\$CC_ANYCOLOR"
syn match autoitConst "\$CC_FULLOPEN"
syn match autoitConst "\$CC_RGBINIT"
" constants - mouse event
syn match autoitConst "\$MOUSEEVENTF_ABSOLUTE"
syn match autoitConst "\$MOUSEEVENTF_MOVE"
syn match autoitConst "\$MOUSEEVENTF_LEFTDOWN"
syn match autoitConst "\$MOUSEEVENTF_LEFTUP"
syn match autoitConst "\$MOUSEEVENTF_RIGHTDOWN"
syn match autoitConst "\$MOUSEEVENTF_RIGHTUP"
syn match autoitConst "\$MOUSEEVENTF_MIDDLEDOWN"
syn match autoitConst "\$MOUSEEVENTF_MIDDLEUP"
syn match autoitConst "\$MOUSEEVENTF_WHEEL"
syn match autoitConst "\$MOUSEEVENTF_XDOWN"
syn match autoitConst "\$MOUSEEVENTF_XUP"
" constants - reg value type
syn match autoitConst "\$REG_NONE"
syn match autoitConst "\$REG_SZ"
syn match autoitConst "\$REG_EXPAND_SZ"
syn match autoitConst "\$REG_BINARY"
syn match autoitConst "\$REG_DWORD"
syn match autoitConst "\$REG_DWORD_BIG_ENDIAN"
syn match autoitConst "\$REG_LINK"
syn match autoitConst "\$REG_MULTI_SZ"
syn match autoitConst "\$REG_RESOURCE_LIST"
syn match autoitConst "\$REG_FULL_RESOURCE_DESCRIPTOR"
syn match autoitConst "\$REG_RESOURCE_REQUIREMENTS_LIST"
" constants - z order
syn match autoitConst "\$HWND_BOTTOM"
syn match autoitConst "\$HWND_NOTOPMOST"
syn match autoitConst "\$HWND_TOP"
syn match autoitConst "\$HWND_TOPMOST"
" constants - setwindowpos
syn match autoitConst "\$SWP_NOSIZE"
syn match autoitConst "\$SWP_NOMOVE"
syn match autoitConst "\$SWP_NOZORDER"
syn match autoitConst "\$SWP_NOREDRAW"
syn match autoitConst "\$SWP_NOACTIVATE"
syn match autoitConst "\$SWP_FRAMECHANGED"
syn match autoitConst "\$SWP_DRAWFRAME"
syn match autoitConst "\$SWP_SHOWWINDOW"
syn match autoitConst "\$SWP_HIDEWINDOW"
syn match autoitConst "\$SWP_NOCOPYBITS"
syn match autoitConst "\$SWP_NOOWNERZORDER"
syn match autoitConst "\$SWP_NOREPOSITION"
syn match autoitConst "\$SWP_NOSENDCHANGING"
syn match autoitConst "\$SWP_DEFERERASE"
syn match autoitConst "\$SWP_ASYNCWINDOWPOS"
" constants - language identifiers
syn match autoitConst "\$LANG_AFRIKAANS"
syn match autoitConst "\$LANG_ALBANIAN"
syn match autoitConst "\$LANG_ARABIC"
syn match autoitConst "\$LANG_ARMENIAN"
syn match autoitConst "\$LANG_ASSAMESE"
syn match autoitConst "\$LANG_AZERI"
syn match autoitConst "\$LANG_BASQUE"
syn match autoitConst "\$LANG_BELARUSIAN"
syn match autoitConst "\$LANG_BENGALI"
syn match autoitConst "\$LANG_BULGARIAN"
syn match autoitConst "\$LANG_CATALAN"
syn match autoitConst "\$LANG_CHINESE"
syn match autoitConst "\$LANG_CROATIAN"
syn match autoitConst "\$LANG_CZECH"
syn match autoitConst "\$LANG_DANISH"
syn match autoitConst "\$LANG_DUTCH"
syn match autoitConst "\$LANG_ENGLISH"
syn match autoitConst "\$LANG_ESTONIAN"
syn match autoitConst "\$LANG_FAEROESE"
syn match autoitConst "\$LANG_FARSI"
syn match autoitConst "\$LANG_FINNISH"
syn match autoitConst "\$LANG_FRENCH"
syn match autoitConst "\$LANG_GEORGIAN"
syn match autoitConst "\$LANG_GERMAN"
syn match autoitConst "\$LANG_GREEK"
syn match autoitConst "\$LANG_GUJARATI"
syn match autoitConst "\$LANG_HEBREW"
syn match autoitConst "\$LANG_HINDI"
syn match autoitConst "\$LANG_HUNGARIAN"
syn match autoitConst "\$LANG_ICELANDIC"
syn match autoitConst "\$LANG_INDONESIAN"
syn match autoitConst "\$LANG_ITALIAN"
syn match autoitConst "\$LANG_JAPANESE"
syn match autoitConst "\$LANG_KANNADA"
syn match autoitConst "\$LANG_KASHMIRI"
syn match autoitConst "\$LANG_KAZAK"
syn match autoitConst "\$LANG_KONKANI"
syn match autoitConst "\$LANG_KOREAN"
syn match autoitConst "\$LANG_LATVIAN"
syn match autoitConst "\$LANG_LITHUANIAN"
syn match autoitConst "\$LANG_MACEDONIAN"
syn match autoitConst "\$LANG_MALAY"
syn match autoitConst "\$LANG_MALAYALAM"
syn match autoitConst "\$LANG_MANIPURI"
syn match autoitConst "\$LANG_MARATHI"
syn match autoitConst "\$LANG_NEPALI"
syn match autoitConst "\$LANG_NEUTRAL"
syn match autoitConst "\$LANG_NORWEGIAN"
syn match autoitConst "\$LANG_ORIYA"
syn match autoitConst "\$LANG_POLISH"
syn match autoitConst "\$LANG_PORTUGUESE"
syn match autoitConst "\$LANG_PUNJABI"
syn match autoitConst "\$LANG_ROMANIAN"
syn match autoitConst "\$LANG_RUSSIAN"
syn match autoitConst "\$LANG_SANSKRIT"
syn match autoitConst "\$LANG_SERBIAN"
syn match autoitConst "\$LANG_SINDHI"
syn match autoitConst "\$LANG_SLOVAK"
syn match autoitConst "\$LANG_SLOVENIAN"
syn match autoitConst "\$LANG_SPANISH"
syn match autoitConst "\$LANG_SWAHILI"
syn match autoitConst "\$LANG_SWEDISH"
syn match autoitConst "\$LANG_TAMIL"
syn match autoitConst "\$LANG_TATAR"
syn match autoitConst "\$LANG_TELUGU"
syn match autoitConst "\$LANG_THAI"
syn match autoitConst "\$LANG_TURKISH"
syn match autoitConst "\$LANG_UKRANIAN"
syn match autoitConst "\$LANG_URDU"
syn match autoitConst "\$LANG_UZBEK"
syn match autoitConst "\$LANG_VIETNAMESE"
" constants - sublanguage identifiers
syn match autoitConst "\$SUBLANG_ARABIC_ALGERIA"
syn match autoitConst "\$SUBLANG_ARABIC_BAHRAIN"
syn match autoitConst "\$SUBLANG_ARABIC_EGYPT"
syn match autoitConst "\$SUBLANG_ARABIC_IRAQ"
syn match autoitConst "\$SUBLANG_ARABIC_JORDAN"
syn match autoitConst "\$SUBLANG_ARABIC_KUWAIT"
syn match autoitConst "\$SUBLANG_ARABIC_LEBANON"
syn match autoitConst "\$SUBLANG_ARABIC_LIBYA"
syn match autoitConst "\$SUBLANG_ARABIC_MOROCCO"
syn match autoitConst "\$SUBLANG_ARABIC_OMAN"
syn match autoitConst "\$SUBLANG_ARABIC_QUATAR"
syn match autoitConst "\$SUBLANG_ARABIC_SAUDIA_ARABIA"
syn match autoitConst "\$SUBLANG_ARABIC_SYRIA"
syn match autoitConst "\$SUBLANG_ARABIC_TUNISIA"
syn match autoitConst "\$SUBLANG_ARABIC_UAE"
syn match autoitConst "\$SUBLANG_ARABIC_YEMEN"
syn match autoitConst "\$SUBLANG_AZERI_CYRILLIC"
syn match autoitConst "\$SUBLANG_AZERI_LATIN"
syn match autoitConst "\$SUBLANG_CHINESE_HONGKONG"
syn match autoitConst "\$SUBLANG_CHINESE_MACAU"
syn match autoitConst "\$SUBLANG_CHINESE_SIMPLIFIED"
syn match autoitConst "\$SUBLANG_CHINESE_SINGAPORE"
syn match autoitConst "\$SUBLANG_CHINESE_TRADITIONAL"
syn match autoitConst "\$SUBLANG_DEFAULT"
syn match autoitConst "\$SUBLANG_DUTCH"
syn match autoitConst "\$SUBLANG_DUTCH_BELGIAN"
syn match autoitConst "\$SUBLANG_ENGLISH_AUS"
syn match autoitConst "\$SUBLANG_ENGLISH_BELIZE"
syn match autoitConst "\$SUBLANG_ENGLISH_CAN"
syn match autoitConst "\$SUBLANG_ENGLISH_CARIBBEAN"
syn match autoitConst "\$SUBLANG_ENGLISH_EIRE"
syn match autoitConst "\$SUBLANG_ENGLISH_JAMAICA"
syn match autoitConst "\$SUBLANG_ENGLISH_NZ"
syn match autoitConst "\$SUBLANG_ENGLISH_PHILLIPPINES"
syn match autoitConst "\$SUBLANG_ENGLISH_SOUTH_AFRICA"
syn match autoitConst "\$SUBLANG_ENGLISH_TRINIDAD"
syn match autoitConst "\$SUBLANG_ENGLISH_UK"
syn match autoitConst "\$SUBLANG_ENGLISH_US"
syn match autoitConst "\$SUBLANG_ENGLISH_ZIMBABWE"
syn match autoitConst "\$SUBLANG_FRENCH"
syn match autoitConst "\$SUBLANG_FRENCH_BELGIAN"
syn match autoitConst "\$SUBLANG_FRENCH_CANADIAN"
syn match autoitConst "\$SUBLANG_FRENCH_LUXEMBOURG"
syn match autoitConst "\$SUBLANG_FRENCH_MONACO"
syn match autoitConst "\$SUBLANG_FRENCH_SWISS"
syn match autoitConst "\$SUBLANG_GERMAN"
syn match autoitConst "\$SUBLANG_GERMAN_AUSTRIAN"
syn match autoitConst "\$SUBLANG_GERMAN_LIECHTENSTEIN"
syn match autoitConst "\$SUBLANG_GERMAN_LUXEMBOURG"
syn match autoitConst "\$SUBLANG_GERMAN_SWISS"
syn match autoitConst "\$SUBLANG_ITALIAN"
syn match autoitConst "\$SUBLANG_ITALIAN_SWISS"
syn match autoitConst "\$SUBLANG_KASHMIRI_INDIA"
syn match autoitConst "\$SUBLANG_KOREAN"
syn match autoitConst "\$SUBLANG_LITHUANIAN"
syn match autoitConst "\$SUBLANG_MALAY_BRUNEI_DARUSSALAM"
syn match autoitConst "\$SUBLANG_MALAY_MALAYSIA"
syn match autoitConst "\$SUBLANG_NEPALI_INDIA"
syn match autoitConst "\$SUBLANG_NEUTRAL"
syn match autoitConst "\$SUBLANG_NORWEGIAN_BOKMAL"
syn match autoitConst "\$SUBLANG_NORWEGIAN_NYNORSK"
syn match autoitConst "\$SUBLANG_PORTUGUESE"
syn match autoitConst "\$SUBLANG_PORTUGUESE_BRAZILIAN"
syn match autoitConst "\$SUBLANG_SERBIAN_CYRILLIC"
syn match autoitConst "\$SUBLANG_SERBIAN_LATIN"
syn match autoitConst "\$SUBLANG_SPANISH"
syn match autoitConst "\$SUBLANG_SPANISH_ARGENTINA"
syn match autoitConst "\$SUBLANG_SPANISH_BOLIVIA"
syn match autoitConst "\$SUBLANG_SPANISH_CHILE"
syn match autoitConst "\$SUBLANG_SPANISH_COLUMBIA"
syn match autoitConst "\$SUBLANG_SPANISH_COSTA_RICA"
syn match autoitConst "\$SUBLANG_SPANISH_DOMINICAN_REPUBLIC"
syn match autoitConst "\$SUBLANG_SPANISH_ECUADOR"
syn match autoitConst "\$SUBLANG_SPANISH_EL_SALVADOR"
syn match autoitConst "\$SUBLANG_SPANISH_GUATEMALA"
syn match autoitConst "\$SUBLANG_SPANISH_HONDURAS"
syn match autoitConst "\$SUBLANG_SPANISH_MEXICAN"
syn match autoitConst "\$SUBLANG_SPANISH_MODERN"
syn match autoitConst "\$SUBLANG_SPANISH_NICARAGUA"
syn match autoitConst "\$SUBLANG_SPANISH_PANAMA"
syn match autoitConst "\$SUBLANG_SPANISH_PARAGUAY"
syn match autoitConst "\$SUBLANG_SPANISH_PERU"
syn match autoitConst "\$SUBLANG_PUERTO_RICO"
syn match autoitConst "\$SUBLANG_PUERTO_URUGUAY"
syn match autoitConst "\$SUBLANG_PUERTO_VENEZUELA"
syn match autoitConst "\$SUBLANG_SWEDISH"
syn match autoitConst "\$SUBLANG_SWEDISH_FINLAND"
syn match autoitConst "\$SUBLANG_SYS_DEFAULT"
syn match autoitConst "\$SUBLANG_URDU_INDIA"
syn match autoitConst "\$SUBLANG_URDU_PAKISTAN"
syn match autoitConst "\$SUBLANG_UZBEK_CYRILLIC"
" constants - sorting IDs
syn match autoitConst "\$SORT_DEFAULT"
syn match autoitConst "\$SORT_JAPANESE_XJIS"
syn match autoitConst "\$SORT_JAPANESE_UNICODE"
syn match autoitConst "\$SORT_CHINESE_BIG5"
syn match autoitConst "\$SORT_CHINESE_PRCP"
syn match autoitConst "\$SORT_CHINESE_UNICODE"
syn match autoitConst "\$SORT_CHINESE_PRC"
syn match autoitConst "\$SORT_KOREAN_KSC"
syn match autoitConst "\$SORT_KOREAN_UNICODE"
syn match autoitConst "\$SORT_GERMAN_PHONE_BOOK"
syn match autoitConst "\$SORT_HUNGARIAN_DEFAULT"
syn match autoitConst "\$SORT_HUNGARIAN_TECHNICAL"
syn match autoitConst "\$SORT_GEORGIAN_TRADITIONAL"
syn match autoitConst "\$SORT_GEORGIAN_MODERN"
" guiconstants - events and messages
syn match autoitConst "\$GUI_EVENT_CLOSE"
syn match autoitConst "\$GUI_EVENT_MINIMIZE"
syn match autoitConst "\$GUI_EVENT_RESTORE"
syn match autoitConst "\$GUI_EVENT_MAXIMIZE"
syn match autoitConst "\$GUI_EVENT_PRIMARYDOWN"
syn match autoitConst "\$GUI_EVENT_PRIMARYUP"
syn match autoitConst "\$GUI_EVENT_SECONDARYDOWN"
syn match autoitConst "\$GUI_EVENT_SECONDARYUP"
syn match autoitConst "\$GUI_EVENT_MOUSEMOVE"
syn match autoitConst "\$GUI_EVENT_RESIZED"
syn match autoitConst "\$GUI_EVENT_DROPPED"
syn match autoitConst "\$GUI_RUNDEFMSG"
" guiconstants - state
syn match autoitConst "\$GUI_AVISTOP"
syn match autoitConst "\$GUI_AVISTART"
syn match autoitConst "\$GUI_AVICLOSE"
syn match autoitConst "\$GUI_CHECKED"
syn match autoitConst "\$GUI_INDETERMINATE"
syn match autoitConst "\$GUI_UNCHECKED"
syn match autoitConst "\$GUI_DROPACCEPTED"
syn match autoitConst "\$GUI_NODROPACCEPTED"
syn match autoitConst "\$GUI_DROPNOTACCEPTED"
syn match autoitConst "\$GUI_ACCEPTFILES"
syn match autoitConst "\$GUI_SHOW"
syn match autoitConst "\$GUI_HIDE"
syn match autoitConst "\$GUI_ENABLE"
syn match autoitConst "\$GUI_DISABLE"
syn match autoitConst "\$GUI_FOCUS"
syn match autoitConst "\$GUI_NOFOCUS"
syn match autoitConst "\$GUI_DEFBUTTON"
syn match autoitConst "\$GUI_EXPAND"
syn match autoitConst "\$GUI_ONTOP"
" guiconstants - font
syn match autoitConst "\$GUI_FONTITALIC"
syn match autoitConst "\$GUI_FONTUNDER"
syn match autoitConst "\$GUI_FONTSTRIKE"
" guiconstants - resizing
syn match autoitConst "\$GUI_DOCKAUTO"
syn match autoitConst "\$GUI_DOCKLEFT"
syn match autoitConst "\$GUI_DOCKRIGHT"
syn match autoitConst "\$GUI_DOCKHCENTER"
syn match autoitConst "\$GUI_DOCKTOP"
syn match autoitConst "\$GUI_DOCKBOTTOM"
syn match autoitConst "\$GUI_DOCKVCENTER"
syn match autoitConst "\$GUI_DOCKWIDTH"
syn match autoitConst "\$GUI_DOCKHEIGHT"
syn match autoitConst "\$GUI_DOCKSIZE"
syn match autoitConst "\$GUI_DOCKMENUBAR"
syn match autoitConst "\$GUI_DOCKSTATEBAR"
syn match autoitConst "\$GUI_DOCKALL"
syn match autoitConst "\$GUI_DOCKBORDERS"
" guiconstants - graphic
syn match autoitConst "\$GUI_GR_CLOSE"
syn match autoitConst "\$GUI_GR_LINE"
syn match autoitConst "\$GUI_GR_BEZIER"
syn match autoitConst "\$GUI_GR_MOVE"
syn match autoitConst "\$GUI_GR_COLOR"
syn match autoitConst "\$GUI_GR_RECT"
syn match autoitConst "\$GUI_GR_ELLIPSE"
syn match autoitConst "\$GUI_GR_PIE"
syn match autoitConst "\$GUI_GR_DOT"
syn match autoitConst "\$GUI_GR_PIXEL"
syn match autoitConst "\$GUI_GR_HINT"
syn match autoitConst "\$GUI_GR_REFRESH"
syn match autoitConst "\$GUI_GR_PENSIZE"
syn match autoitConst "\$GUI_GR_NOBKCOLOR"
" guiconstants - control default styles
syn match autoitConst "\$GUI_SS_DEFAULT_AVI"
syn match autoitConst "\$GUI_SS_DEFAULT_BUTTON"
syn match autoitConst "\$GUI_SS_DEFAULT_CHECKBOX"
syn match autoitConst "\$GUI_SS_DEFAULT_COMBO"
syn match autoitConst "\$GUI_SS_DEFAULT_DATE"
syn match autoitConst "\$GUI_SS_DEFAULT_EDIT"
syn match autoitConst "\$GUI_SS_DEFAULT_GRAPHIC"
syn match autoitConst "\$GUI_SS_DEFAULT_GROUP"
syn match autoitConst "\$GUI_SS_DEFAULT_ICON"
syn match autoitConst "\$GUI_SS_DEFAULT_INPUT"
syn match autoitConst "\$GUI_SS_DEFAULT_LABEL"
syn match autoitConst "\$GUI_SS_DEFAULT_LIST"
syn match autoitConst "\$GUI_SS_DEFAULT_LISTVIEW"
syn match autoitConst "\$GUI_SS_DEFAULT_MONTHCAL"
syn match autoitConst "\$GUI_SS_DEFAULT_PIC"
syn match autoitConst "\$GUI_SS_DEFAULT_PROGRESS"
syn match autoitConst "\$GUI_SS_DEFAULT_RADIO"
syn match autoitConst "\$GUI_SS_DEFAULT_SLIDER"
syn match autoitConst "\$GUI_SS_DEFAULT_TAB"
syn match autoitConst "\$GUI_SS_DEFAULT_TREEVIEW"
syn match autoitConst "\$GUI_SS_DEFAULT_UPDOWN"
syn match autoitConst "\$GUI_SS_DEFAULT_GUI"
" guiconstants - background color special flags
syn match autoitConst "\$GUI_BKCOLOR_DEFAULT"
syn match autoitConst "\$GUI_BKCOLOR_LV_ALTERNATE"
syn match autoitConst "\$GUI_BKCOLOR_TRANSPARENT"

" registry constants
syn match autoitConst "\([\"\']\)REG_BINARY\1"
syn match autoitConst "\([\"\']\)REG_SZ\1"
syn match autoitConst "\([\"\']\)REG_MULTI_SZ\1"
syn match autoitConst "\([\"\']\)REG_EXPAND_SZ\1"
syn match autoitConst "\([\"\']\)REG_DWORD\1"

" Define the default highlighting.
" Unused colors: Underlined, Ignore, Error, Todo
hi def link autoitFunction Statement  " yellow/yellow
hi def link autoitKeyword Statement
hi def link autoitOperator Operator
hi def link autoitVarSelector Operator
hi def link autoitComment	Comment  " cyan/blue
hi def link autoitParen Comment
hi def link autoitComma Comment
hi def link autoitBracket Comment
hi def link autoitNumber Constant " magenta/red
hi def link autoitString Constant
hi def link autoitQuote Constant
hi def link autoitIncluded Constant
hi def link autoitCont Special  " red/orange
hi def link autoitDoubledSingles Special
hi def link autoitDoubledDoubles Special
hi def link autoitCommDelimiter PreProc  " blue/magenta
hi def link autoitInclude PreProc
hi def link autoitVariable Identifier  " cyan/cyan
hi def link autoitBuiltin Type  " green/green
hi def link autoitOption Type
hi def link autoitStyle Type
hi def link autoitConst Type
hi def link autoitSend Type
syn sync minlines=50
