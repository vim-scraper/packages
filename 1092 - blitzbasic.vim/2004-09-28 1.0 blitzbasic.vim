" Vim syntax file
" Language:	BlitzBASIC
" Maintainer:	Elias Pschernig <elias@users.sf.net>
" Last Change:	Tue Sep 28 14:43:16 CEST 2004

" Based on basic.vim.

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Keywords list auto-fetched from the BB site with this script:
" #!/usr/bin/python
" import re
" import urllib2
" url = """http://blitzbasic.com/bpdocs/command_list_2d_a-z.php"""
" for letter in "ABCDEFGHIJKLMNOPQRSTUVWXYZ":
"    site = urllib2.urlopen (url + "?show=%s" % letter)
"    contents = site.read ()
"    regexp = '<li><a href="command.php\\?name=(.*?)&ref=2d_a-z">'
"    names = re.compile (regexp).findall (contents)
"    print "syn keyword BBCommand\t",
"    for name in names
"        print name,
"    print
syn keyword BBCommand   Abs AcceptTCPStream ACos After And AppTitle Asc ASin ATan ATan2 AutoMidHandle AutoSuspend AvailVidMem
syn keyword BBCommand   BackBuffer BankSize Before Bin
syn keyword BBCommand   CallDLL Case Ceil ChangeDir ChannelPan ChannelPitch ChannelPlaying ChannelVolume Chr CloseDir CloseFile CloseMovie CloseTCPServer CloseTCPStream CloseUDPStream Cls ClsColor Color ColorBlue ColorGreen ColorRed CommandLine Const CopyBank CopyFile CopyImage CopyPixel CopyPixelFast CopyRect CopyStream Cos CountGfxDrivers CountGFXModes CountHostIPs CreateBank CreateDir CreateImage CreateNetPlayer CreateProcess CreateTCPServer CreateTimer CreateUDPStream CurrentDate CurrentDir CurrentTime
syn keyword BBCommand   Data DebugLog Default Delay Delete DeleteDir DeleteFile DeleteNetPlayer DesktopBuffer Dim DottedIP DrawBlock DrawBlockRect DrawImage DrawImageRect DrawMovie
syn keyword BBCommand   Each Else Else If ElseIf End End Function End If End Select End Type EndGraphics EndIf Eof ExecFile Exit Exp
syn keyword BBCommand   False Field FilePos FileSize FileType First Flip Float Floor FlushJoy FlushKeys FlushMouse FontHeight FontName FontSize FontStyle FontWidth For Forever FreeBank FreeFont FreeImage FreeSound FreeTimer FrontBuffer Function
syn keyword BBCommand   GammaBlue GammaGreen GammaRed GetColor GetEnv GetKey GetMouse GfxDriverName GFXModeDepth GfxModeExists GfxModeFormat GFXModeHeight GFXModeWidth Global Gosub Goto GrabImage Graphics GraphicsBuffer GraphicsDepth GraphicsFormat GraphicsHeight GraphicsWidth
syn keyword BBCommand   HandleImage Hex HidePointer HostIP HostNetGame
syn keyword BBCommand   If ImageBuffer ImageHeight ImageRectCollide ImageRectOverlap ImagesCollide ImagesOverlap ImageWidth ImageXHandle ImageYHandle Include Input Insert Instr Int
syn keyword BBCommand   JoinNetGame JoyDown JoyHat JoyHit JoyPitch JoyRoll JoyType JoyU JoyUDir JoyV JoyVDir JoyX JoyXDir JoyY JoyYaw JoyYDir JoyZ JoyZDir
syn keyword BBCommand   KeyDown KeyHit KeyWait
syn keyword BBCommand   Last Left Len Line LoadAnimImage LoadBuffer LoadFont LoadImage LoadSound Local LockBuffer LockedFormat LockedPitch LockedPixels Log Log10 LoopSound Lower LSet
syn keyword BBCommand   MaskImage Mid MidHandle Millisecs Mod MoreFiles MouseDown MouseHit MouseX MouseXSpeed MouseY MouseYSpeed MouseZ MouseZSpeed MoveMouse MovieHeight MoviePlaying MovieWidth
syn keyword BBCommand   NetMsgData NetMsgFrom NetMsgTo NetMsgType NetPlayerLocal NetPlayerName New Next NextFile Not Null
syn keyword BBCommand   OpenFile OpenMovie OpenTCPStream Or Origin Oval
syn keyword BBCommand   PauseChannel PauseTimer PeekByte PeekFloat PeekInt PeekShort Pi PlayCDTrack PlayMusic PlaySound Plot PokeByte PokeFloat PokeInt PokeShort Print
syn keyword BBCommand   QueryObject
syn keyword BBCommand   Rand Read ReadAvail ReadByte ReadBytes ReadDir ReadFile ReadFloat ReadInt ReadLine ReadPixel ReadPixelFast ReadShort ReadString Rect RectsOverlap RecvNetMsg RecvUDPMsg Repeat Replace ResetTimer ResizeBank ResizeImage Restore ResumeChannel ResumeTimer Return Right Rnd RndSeed RotateImage RSet RuntimeError
syn keyword BBCommand   Sar SaveBuffer SaveImage ScaleImage ScanLine SeedRnd SeekFile Select SendNetMsg SendUDPMsg SetBuffer SetEnv SetFont SetGamma SetGfxDriver Sgn Shl ShowPointer Shr Sin SoundPan SoundPitch SoundVolume Sqr StartNetGame Step Stop StopChannel StopNetGame Str String StringHeight StringWidth SystemProperty
syn keyword BBCommand   Tan TCPStreamIP TCPStreamPort TCPTimeouts Text TFormFilter TFormImage Then TileBlock TileImage TimerTicks To TotalVidMem Trim True Type
syn keyword BBCommand   UDPMsgIP UDPMsgPort UDPStreamIP UDPStreamPort UDPTimeouts UnlockBuffer Until UpdateGamma Upper
syn keyword BBCommand   Viewport VWait
syn keyword BBCommand   WaitKey WaitMouse WaitTimer Wend While Write WriteByte WriteBytes WriteFile WriteFloat WriteInt WriteLine WritePixel WritePixelFast WriteShort WriteString
syn keyword BBCommand   Xor

syn keyword basicTodo contained	TODO

"integer number, or floating point number without a dot.
syn match  basicNumber		"\<\d\+\>"
"floating point number, with dot
syn match  basicNumber		"\<\d\+\.\d*\>"
"floating point number, starting with a dot
syn match  basicNumber		"\.\d\+\>"

" String and Character constants
syn match   basicSpecial contained "\\\d\d\d\|\\."
syn region  basicString		  start=+"+  skip=+\\\\\|\\"+  end=+"+  contains=basicSpecial

syn region  basicComment	start=";" end="$" contains=basicTodo
syn region  basicComment	start="^[ \t]*'" end="$" contains=basicTodo
syn match   basicLabel		"^\.[a-zA-Z0-9]*$"
syn match   basicTypeSpecifier  "[a-zA-Z0-9][\$%&!#]"ms=s+1

"syn sync ccomment basicComment
" syn match   basicMathsOperator "[<>+\*^/\\=-]"
syn match   basicMathsOperator   "-\|=\|[:<>+\*^/\\]\|AND\|OR"

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_basic_syntax_inits")
  if version < 508
    let did_basic_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink basicLabel		Label
  HiLink basicConditional	Conditional
  HiLink basicRepeat		Repeat
  HiLink basicLineNumber	Comment
  HiLink basicNumber		Number
  HiLink basicError		Error
  HiLink BBCommand              Statement
  HiLink basicString		String
  HiLink basicComment		Comment
  HiLink basicSpecial		Special
  HiLink basicTodo		Todo
  HiLink basicFunction		Identifier
  HiLink basicTypeSpecifier	Type
  HiLink basicFilenumber	basicTypeSpecifier
  "hi basicMathsOperator term=bold cterm=bold gui=bold

  delcommand HiLink
endif

let b:current_syntax = "blitzbasic"

" vim: ts=8
