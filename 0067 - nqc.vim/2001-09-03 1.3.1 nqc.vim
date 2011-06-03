"###############################################################################################
"
"     Filename:  nqc.vim
"
"  Description:  gvim-menus for NQC (Not Quite C) ,  Version 2.3r1 .
"                NQC is a C-like language for programmimg the LEGO MINDSTORMS RCX-Controller.
"                The NQC language is described in:
"
"                    NQC Programmers's Guide, Version 2.3r1, by Dave Baum
"                    ( http://www.enteract.com/~dbaum/ )
"
"                These menus turn gvim into an IDE for the RCX-programming:
"                 - insertion of NQC statements, function calls, comments, RCX constants
"                 - download and start programs and firmware to RCX
"                 - upload the datalog from RCX
"                 - erase programs and datalogs in RCX
"
"                The default style of the GNU indent programm is used for the 
"                representation of the NQC statements.
"
"      Version:  1.3.1 / LINUX
"     Revision:  03.09.2001
"       Author:  Dr.-Ing. Fritz Mehner - MFH Iserlohn
"        Email:  mehner@mfh-iserlohn.de
"      Created:  28.07.2001 - 15:40:43
"
"        Usage:  (1) Personalize nqc.vim  (section below).
"                (2) Rename  nqc.vim  to  .nqc.vim  and put it in your home directory.  
"                (3) Load  .nqc.vim  manually with the :so command:
"                      :so ~/.nqc.vim
"                    or better place this command in your file .gvimrc .
"               
"                The register z is used in many places.
"
"###############################################################################################
"               Personalization 
"-------------------------------------------------------------------------------------
"  Use my personalization as an example.
"
let NQCVIM_AuthorName      = "Fritz Mehner"
let NQCVIM_AuthorSign      = "fm"
"
"  RCX-Firmware - path and filename :
"
let NQCVIM_RCX_Firmware    = "~/bin/firm0309.lgo"
"
"###############################################################################################
"               Key Mappings
"-------------------------------------------------------------------------------------
"  This is for convenience only. Comment out the following maps if you dislike them.
"  If enabled, there may be conflicts with predefined key bindings of your window manager.
"
"   F2          write buffer to file without confirmation (update)
"   F3          file open dialog
"   Alt-F9      write buffer and compile
"
map	<F2>	    	:update<CR>
map	<F3>	    	:browse confirm e<CR>
map	<M-F9>    	:w<CR><Esc>:!nqc %<CR>
"
"###############################################################################################
"
"===============================================================================================
"----- Menu : NQC-Comments ---------------------------------------------------------------------
"===============================================================================================
"  MENU ENTRY
" -----------------
"  Line End Comment               
"  Frame Comment     
"  Function Description    
"  File Prologue          
"  Author+Date+Time      
"  Date+Time
"  Date
"  comment out a highlighted block of code
"  uncomment   a highlighted block of code
"===============================================================================================
"
imenu  NQC-&Comments.&Line\ End\ Comment               <Tab><Tab><Tab>// 
imenu  NQC-&Comments.&Frame\ Comment                   <Esc>:call NQCVIM_CommentFrame()          <CR>jA
imenu  NQC-&Comments.task\/function\/sub\ &Description <Esc>:call NQCVIM_CommentFunction()       <CR>:/Name<CR>A
imenu  NQC-&Comments.File\ &Prologue                   <Esc>:call NQCVIM_CommentFilePrologue()   <CR>:/Description<CR>A
imenu  NQC-&Comments.-SEP1-                            :
imenu  NQC-&Comments.\/\/\ Date\ Time\ &Author         <Esc>$<Esc>:call NQCVIM_CommentDateTimeAuthor() <CR>kJA
imenu  NQC-&Comments.Date\ &Time                       <Esc>:call NQCVIM_CommentDateTime()             <CR>kJi
imenu  NQC-&Comments.Date                              <Esc>:call NQCVIM_CommentDate()                 <CR>kJi
imenu  NQC-&Comments.-SEP2-                            :
vmenu  NQC-&Comments.&code->comment                    <Esc>:'<,'>s/^/\/\//<CR>
vmenu  NQC-&Comments.c&omment->code                    <Esc>:'<,'>s/^\/\///<CR>
"
"===============================================================================================
"----- Menu : NQC-Statements -------------------------------------------------------------------
"===============================================================================================
"  MENU ENTRY
" -----------------
"  if{}
"  if{}else{}
"  for{}
"  while{}
"  do{}while
"  repeat{}
"  switch
"  case
"  task
"  inline function
"  subroutine
"  #include "..."
"  #define
"  #ifndef..#def..#endif
"  #ifdef..#endif
" -----------------
"  Inserting at the end of the line preserves indentation.
"===============================================================================================
"
imenu  NQC-St&atements.&if\ \{\ \}                 <Esc>:let @z="if (  )\n\t{\n\t\n\t}\n"                     <CR>"z]p<Esc>f(la
imenu  NQC-St&atements.if\ \{\ \}\ &else\ \{\ \}   <Esc>:let @z="if (  )\n\t{\n\t\n\t}\nelse\n\t{\n\t\n\t}\n" <CR>"z]p<Esc>f(la
imenu  NQC-St&atements.&for\ \{\ \}                <Esc>:let @z="for ( ; ;  )\n\t{\n\t\n\t}\n"                <CR>"z]p<Esc>f;i
imenu  NQC-St&atements.&while\ \{\ \}              <Esc>:let @z="while (  )\n\t{\n\t\n\t}\n"                  <CR>"z]p<Esc>f(la
imenu  NQC-St&atements.&do\ \{\ \}\ while          <Esc>:call NQCVIM_DoWhile()                                <CR>"z]p<Esc>:/while <CR>f(la
imenu  NQC-St&atements.&repeat\ \{\ \}             <Esc>:let @z="repeat (  )\n\t{\n\t\n\t}\n"                 <CR>"z]p<Esc>f(la
imenu  NQC-St&atements.&switch                     <Esc>:call NQCVIM_CodeSwitch()                             <CR>"z]p<Esc>f(la
imenu  NQC-St&atements.&case                       <Esc>:call NQCVIM_CodeCase()                               <CR>"z]p<Esc>f:i
imenu  NQC-St&atements.-SEP1-                      :
imenu  NQC-St&atements.&task                       <Esc>:call NQCVIM_CodeTask()   <CR>
imenu  NQC-St&atements.in&line\ function           <Esc>:call NQCVIM_CodeInlineFunction()   <CR>
imenu  NQC-St&atements.s&ubroutine                 <Esc>:call NQCVIM_CodeSubroutine()   <CR>
imenu  NQC-St&atements.-SEP2-                      :
imenu  NQC-St&atements.#include\ \"\.\.\.\"        <Esc>:let @z="#include\t\".nqh\""                          <CR>"zp<Esc>F.i
imenu  NQC-St&atements.&#define                    <Esc>:let @z="#define\t\t\t\t// "                          <CR>"zp<Esc>4F<Tab>a
imenu  NQC-St&atements.#if&ndef\.\.#def\.\.#endif  <Esc>:call NQCVIM_CodeIfndef()                             <CR>
imenu  NQC-St&atements.#ifdef\.\.#endif            #ifdef<Tab><CR><CR><CR>#endif<Esc>3kA
"
"===============================================================================================
"----- Menu : RCX-Functions --------------------------------------------------------------------
"===============================================================================================
"
"----- outputs ----------------------------------------------------------------------------
imenu RCX-Functions.outputs.Float\ (outputs)                     Float();<Esc>F(a
imenu RCX-Functions.outputs.Fwd\ (outputs)                       Fwd();<Esc>F(a
imenu RCX-Functions.outputs.Off\ (outputs)                       Off();<Esc>F(a
imenu RCX-Functions.outputs.On\ (outputs)                        On();<Esc>F(a
imenu RCX-Functions.outputs.OnFor\ (outputs,time)                OnFor(,);<Esc>F(a
imenu RCX-Functions.outputs.OnFwd\ (outputs)                     OnFwd();<Esc>F(a
imenu RCX-Functions.outputs.OnRev\ (outputs)                     OnRev();<Esc>F(a
imenu RCX-Functions.outputs.OutputStatus\ (n)                    OutputStatus();<Esc>F(a
imenu RCX-Functions.outputs.Rev\ (outputs)                       Rev();<Esc>F(a
imenu RCX-Functions.outputs.SetDirection\ (outputs,dir)          SetDirection(,);<Esc>F(a
imenu RCX-Functions.outputs.SetOutput\ (outputs,mode)            SetOutput(,);<Esc>F(a
imenu RCX-Functions.outputs.SetPower\ (outputs,power)            SetPower(,);<Esc>F(a
imenu RCX-Functions.outputs.Toggle\ (outputs)                    Toggle();<Esc>F(a
"----- sensor types, modes, information	---------------------------------------------------
imenu RCX-Functions.sensors.ClearSensor\ (sensor)                ClearSensor();<Esc>F(a
imenu RCX-Functions.sensors.SensorMode\ (n)                      SensorMode();<Esc>F(a
imenu RCX-Functions.sensors.SensorType\ (n)                      SensorType();<Esc>F(a
imenu RCX-Functions.sensors.SensorValueBool\ (n)                 SensorValueBool();<Esc>F(a
imenu RCX-Functions.sensors.SensorValueRaw\ (n)                  SensorValueRaw();<Esc>F(a
imenu RCX-Functions.sensors.SensorValue\ (n)                     SensorValue();<Esc>F(a
imenu RCX-Functions.sensors.SetSensor\ (sensor,config)           SetSensor(,);<Esc>F(a
imenu RCX-Functions.sensors.SetSensorMode\ (sensor,mode)         SetSensorMode(,);<Esc>F(a
imenu RCX-Functions.sensors.SetSensorType\ (sensor,type)         SetSensorType(,);<Esc>F(a
"----- timers and counters ----------------------------------------------------------------
imenu RCX-Functions.timers\ counters.ClearTimer\ (n)             ClearTimer();<Esc>F(a
imenu RCX-Functions.timers\ counters.Timer\ (n)                  Timer();<Esc>F(a
imenu RCX-Functions.timers\ counters.Counter\ (n)                Timer();<Esc>F(a
"----- sounds -----------------------------------------------------------------------------
imenu RCX-Functions.sounds.PlaySound\ (sound)                    PlaySound();<Esc>F(a
imenu RCX-Functions.sounds.PlayTone\ (freq,duration)             PlayTone(,);<Esc>F(a
"----- LCD display ------------------------------------------------------------------------
imenu RCX-Functions.display.SelectDisplay\ (mode)                SelectDisplay();<Esc>F(a
"----- messages ---------------------------------------------------------------------------
imenu RCX-Functions.messages.ClearMessage\ (\ )                  ClearMessage();
imenu RCX-Functions.messages.Message\ (\ )                       Message();
imenu RCX-Functions.messages.SendMessage\ (message)              SendMessage();<Esc>F(a
imenu RCX-Functions.messages.SetTxPower\ (power)                 SetTxPower();<Esc>F(a
"----- general ----------------------------------------------------------------------------
imenu RCX-Functions.general.Program\ (\ )                        Program();
imenu RCX-Functions.general.Random\ (n)                          Random();<Esc>F(a
imenu RCX-Functions.general.SetSleepTime\ (minutes)              SetSleepTime();<Esc>F(a
imenu RCX-Functions.general.SetWatch\ (hours,minutes)            SetWatch(,);<Esc>F(a
imenu RCX-Functions.general.SleepNow\ (\ )                       SleepNow();
imenu RCX-Functions.general.StopAllTasks\ (\ )                   StopAllTasks();
imenu RCX-Functions.general.Wait\ (time)                         Wait();<Esc>F(a
imenu RCX-Functions.general.Watch\ (\n)                          Watch();<Esc>F(a
"----- datalog ----------------------------------------------------------------------------
imenu RCX-Functions.datalog.AddToDatalog\ (value)                AddToDatalog();<Esc>F(a
imenu RCX-Functions.datalog.CreateDatalog\ (size)                CreateDatalog();<Esc>F(a
imenu RCX-Functions.datalog.UploadDatalog\ (start,count)         UploadDatalog(,);<Esc>F(a
"
"
"===============================================================================================
"----- Menu : RCX-Constants --------------------------------------------------------------------
"===============================================================================================
"
"----- display ----------------------------------------------------------------------------
imenu RCX-Constants.display.DISPLAY_OUT_A           DISPLAY_OUT_A                   
imenu RCX-Constants.display.DISPLAY_OUT_B           DISPLAY_OUT_B
imenu RCX-Constants.display.DISPLAY_OUT_C           DISPLAY_OUT_C
imenu RCX-Constants.display.DISPLAY_SENSOR_1        DISPLAY_SENSOR_1
imenu RCX-Constants.display.DISPLAY_SENSOR_2        DISPLAY_SENSOR_2
imenu RCX-Constants.display.DISPLAY_SENSOR_3        DISPLAY_SENSOR_3
imenu RCX-Constants.display.DISPLAY_WATCH           DISPLAY_WATCH
"----- output  ----------------------------------------------------------------------------
imenu RCX-Constants.output.OUT_A                    OUT_A
imenu RCX-Constants.output.OUT_B                    OUT_B
imenu RCX-Constants.output.OUT_C                    OUT_C
imenu RCX-Constants.output.OUT_FLOAT                OUT_FLOAT
imenu RCX-Constants.output.OUT_FULL                 OUT_FULL
imenu RCX-Constants.output.OUT_FWD                  OUT_FWD
imenu RCX-Constants.output.OUT_HALF                 OUT_HALF
imenu RCX-Constants.output.OUT_LOW                  OUT_LOW
imenu RCX-Constants.output.OUT_OFF                  OUT_OFF
imenu RCX-Constants.output.OUT_ON                   OUT_ON
imenu RCX-Constants.output.OUT_REV                  OUT_REV
imenu RCX-Constants.output.OUT_TOGGLE               OUT_TOGGLE
"----- sensor  ----------------------------------------------------------------------------
imenu RCX-Constants.sensor.SENSOR_1                 SENSOR_1
imenu RCX-Constants.sensor.SENSOR_2                 SENSOR_2
imenu RCX-Constants.sensor.SENSOR_3                 SENSOR_3
imenu RCX-Constants.sensor.SENSOR_CELSIUS           SENSOR_CELSIUS
imenu RCX-Constants.sensor.SENSOR_EDGE              SENSOR_EDGE
imenu RCX-Constants.sensor.SENSOR_FAHRENHEIT        SENSOR_FAHRENHEIT
imenu RCX-Constants.sensor.SENSOR_LIGHT             SENSOR_LIGHT
imenu RCX-Constants.sensor.SENSOR_MODE_BOOL         SENSOR_MODE_BOOL
imenu RCX-Constants.sensor.SENSOR_MODE_CELSIUS      SENSOR_MODE_CELSIUS
imenu RCX-Constants.sensor.SENSOR_MODE_EDGE         SENSOR_MODE_EDGE
imenu RCX-Constants.sensor.SENSOR_MODE_FAHRENHEIT   SENSOR_MODE_FAHRENHEIT
imenu RCX-Constants.sensor.SENSOR_MODE_PERCENT      SENSOR_MODE_PERCENT
imenu RCX-Constants.sensor.SENSOR_MODE_PULSE        SENSOR_MODE_PULSE
imenu RCX-Constants.sensor.SENSOR_MODE_RAW          SENSOR_MODE_RAW
imenu RCX-Constants.sensor.SENSOR_MODE_ROTATION     SENSOR_MODE_ROTATION
imenu RCX-Constants.sensor.SENSOR_PULSE             SENSOR_PULSE
imenu RCX-Constants.sensor.SENSOR_ROTATION          SENSOR_ROTATION
imenu RCX-Constants.sensor.SENSOR_TOUCH             SENSOR_TOUCH
imenu RCX-Constants.sensor.SENSOR_TYPE_LIGHT        SENSOR_TYPE_LIGHT
imenu RCX-Constants.sensor.SENSOR_TYPE_NONE         SENSOR_TYPE_NONE
imenu RCX-Constants.sensor.SENSOR_TYPE_ROTATION     SENSOR_TYPE_ROTATION
imenu RCX-Constants.sensor.SENSOR_TYPE_TEMPERATURE  SENSOR_TYPE_TEMPERATURE
imenu RCX-Constants.sensor.SENSOR_TYPE_TOUCH        SENSOR_TYPE_TOUCH
"----- sound   ----------------------------------------------------------------------------
imenu RCX-Constants.sound.SOUND_CLICK               SOUND_CLICK
imenu RCX-Constants.sound.SOUND_DOUBLE_BEEP         SOUND_DOUBLE_BEEP
imenu RCX-Constants.sound.SOUND_DOWN                SOUND_DOWN
imenu RCX-Constants.sound.SOUND_FAST_UP             SOUND_FAST_UP
imenu RCX-Constants.sound.SOUND_LOW_BEEP            SOUND_LOW_BEEP
imenu RCX-Constants.sound.SOUND_UP                  SOUND_UP
"----- misc    ----------------------------------------------------------------------------
imenu RCX-Constants.TX_POWER_HI                     TX_POWER_HI
imenu RCX-Constants.TX_POWER_LO                     TX_POWER_LO       
"
"===============================================================================================
"----- Menu : NQC-Run  -------------------------------------------------------------------------
"===============================================================================================
"
"  MENU ENTRY
" -----------------
"  save buffer to file and compile the file
"  download program 1 ... 5 to RCX
"  download program 1 ... 5 to RCX and run it
"  download firmware to RCX
"  upload a datalog from RCX
"  erase programs and datalogs in RCX
"
"===============================================================================================
"
amenu  NQC-&Run.save\ and\ &compile\ \ \<Alt\>\<F9\>       <C-C>:w<CR><Esc>:!nqc %<CR>
amenu  NQC-&Run.-SEP1-                                     :
amenu  NQC-&Run.download\ program\ 1\ to\ RCX              <C-C>:w<CR><Esc>:!nqc -d -pgm 1 %<CR>
amenu  NQC-&Run.download\ program\ 2\ to\ RCX              <C-C>:w<CR><Esc>:!nqc -d -pgm 2 %<CR>
amenu  NQC-&Run.download\ program\ 3\ to\ RCX              <C-C>:w<CR><Esc>:!nqc -d -pgm 3 %<CR>
amenu  NQC-&Run.download\ program\ 4\ to\ RCX              <C-C>:w<CR><Esc>:!nqc -d -pgm 4 %<CR>
amenu  NQC-&Run.download\ program\ 5\ to\ RCX              <C-C>:w<CR><Esc>:!nqc -d -pgm 5 %<CR>
amenu  NQC-&Run.-SEP2-                                     :
amenu  NQC-&Run.download\ program\ 1\ to\ RCX\ and\ Run    <C-C>:w<CR><Esc>:!nqc -d -pgm 1 % -run<CR>
amenu  NQC-&Run.download\ program\ 2\ to\ RCX\ and\ Run    <C-C>:w<CR><Esc>:!nqc -d -pgm 2 % -run<CR>
amenu  NQC-&Run.download\ program\ 3\ to\ RCX\ and\ Run    <C-C>:w<CR><Esc>:!nqc -d -pgm 3 % -run<CR>
amenu  NQC-&Run.download\ program\ 4\ to\ RCX\ and\ Run    <C-C>:w<CR><Esc>:!nqc -d -pgm 4 % -run<CR>
amenu  NQC-&Run.download\ program\ 5\ to\ RCX\ and\ Run    <C-C>:w<CR><Esc>:!nqc -d -pgm 5 % -run<CR>
amenu  NQC-&Run.-SEP3-                                     :
amenu  NQC-&Run.download\ firmware\ to\ RCX                <C-C>:let @z="!nqc -firmware ".g:NQCVIM_RCX_Firmware<CR><Esc>:@z<CR>
amenu  NQC-&Run.upload\ datalog\ into\ buffer              <C-C>:r !nqc -datalog 2>/dev/null <CR>
amenu  NQC-&Run.erase\ programs\ and\ datalogs\ from\ RCX  <C-C>:! nqc -clear<CR>
"
"===============================================================================================
"----- vim Functions ---------------------------------------------------------------------------
"===============================================================================================
"
"------------------------------------------------------------------------------
"  NQC-Comments : Frame Comment
"------------------------------------------------------------------------------
function! NQCVIM_CommentFrame ()
  let @z=   "//----------------------------------------------------------------------\n"
  let @z=@z."//  \n"
  let @z=@z."//----------------------------------------------------------------------\n"
  put z
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Comments : Function  Comment
"------------------------------------------------------------------------------
function! NQCVIM_CommentFunction ()
  let @z=    "//\n"
  let @z= @z."//=====================================================================================\n"
  let @z= @z."//\n"
  let @z= @z."//        Name:  \n"
  let @z= @z."//\n"
  let @z= @z."// Description:  \n"
  let @z= @z."//\n"
  let @z= @z."//- PARAMETER -------------------------------------------------------------------------\n"
  let @z= @z."//     Mode   Type            Name            Description\n"
  let @z= @z."//-------------------------------------------------------------------------------------\n"
  let @z= @z."//       in:  \n"
  let @z= @z."//   in-out:  \n"
  let @z= @z."//      out:  \n"
  let @z= @z."//-------------------------------------------------------------------------------------\n"
  let @z= @z."//   Author:  ".g:NQCVIM_AuthorName."\n"
  let @z= @z."//     Date:  ".strftime("%x - %X")."\n"
  let @z= @z."//=====================================================================================\n"
  let @z= @z."//\n"
  put z
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Comments : File Prologue
"------------------------------------------------------------------------------
function! NQCVIM_CommentFilePrologue ()

		let	File	= expand("%:t")								" name of the file in the current buffer 
    let @z=    "//\n"
    let @z= @z."//=====================================================================================\n"
    let @z= @z."//\n"
    let @z= @z."//       Filename:\t".File."\n"
    let @z= @z."//    Description:\t\n"
    let @z= @z."//\n"
    let @z= @z."//       Compiler:\tnqc\n"
    let @z= @z."//         Author:\t".g:NQCVIM_AuthorName."\n"
    let @z= @z."//        Created:\t".strftime("%x - %X")."\n"
    let @z= @z."//=====================================================================================\n"
    let @z= @z."//\n\n"
    put! z
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Comments : Author+Date+Time 
"  NQC-Comments : Date+Time 
"  NQC-Comments : Date
"------------------------------------------------------------------------------
function! NQCVIM_CommentDateTimeAuthor ()
  put = '//% '.strftime(\"%x - %X\").' ('.g:NQCVIM_AuthorSign.')' 
endfunction
"
function! NQCVIM_CommentDateTime ()
  put = strftime(\"%x - %X\")
endfunction
"
function! NQCVIM_CommentDate ()
  put = strftime(\"%x\")
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Statements : do-while
"------------------------------------------------------------------------------
"
function! NQCVIM_DoWhile ()
	let @z=    "do\n\t{\n\t\n\t}\nwhile (  );" 
  let @z= @z."\t\t\t\t// -----  end do-while  -----\n"
endfunction
"
"------------------------------------------------------------------------------
"  Statements : switch
"  Statements : case
"------------------------------------------------------------------------------
"
let NQCVIM_CaseStatement = "\tcase :\t\n\t\tbreak;\n\n"
"
function! NQCVIM_CodeSwitch ()
  let @z= "switch (  )\n\t{\n\n"
	
	let loopcount=4                   " default number of cases
	while loopcount>0
    let @z= @z.g:NQCVIM_CaseStatement
	  let loopcount=loopcount-1
	endwhile
	
	let @z= @z."\tdefault:\t\n\t}"
  let @z= @z."\t\t\t\t// -----  end switch  -----\n"
endfunction
"
function! NQCVIM_CodeCase ()
    let @z= g:NQCVIM_CaseStatement
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Statements : #ifndef
"------------------------------------------------------------------------------
function! NQCVIM_CodeIfndef (...)
	if a:0 == 0
		let	identifier=input("(uppercase) identifier (default <FILENAME>_H ) : ")
		if identifier==""
			let	identifier	= expand("%:t:r")."_H"
			let	identifier	= substitute(identifier,".*", '\U\0', "" )	" to uppercase
		endif
	else
		let identifier=a:1
	endif

  let @z=    "#ifndef\t".identifier."\n"
	let @z= @z."#define\t".identifier."\t\t// \n\n\n\n"
	let @z= @z."#endif\t\t\t// ----------  ifndef ".identifier."  ----------\n"
	put z
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Statements : task
"------------------------------------------------------------------------------
function! NQCVIM_CodeTask ()
	let	identifier=input("task name (default main ) : ")
	if identifier==""
		let	identifier	= "main"
	endif
  let @z=    "task\n".identifier."\t(  )\n{\n\n\n\treturn ;\n}"
  let @z= @z."\t\t\t\t// ----------  end of task ".identifier."  ----------" 
	  put z
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Statements : inline function
"------------------------------------------------------------------------------
function! NQCVIM_CodeInlineFunction ()
	let	identifier=input("inline function name (default func ) : ")
	if identifier==""
		let	identifier	= "func"
	endif
  let @z=    "void\n".identifier."\t(  )\n{\n\n\n\treturn ;\n}"
  let @z= @z."\t\t\t\t// ----------  end of inline function ".identifier."  ----------" 
	  put z
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Statements : subroutine
"------------------------------------------------------------------------------
function! NQCVIM_CodeSubroutine ()
	let	identifier=input("subroutine name (default subr ) : ")
	if identifier==""
		let	identifier	= "subr"
	endif
  let @z=    "sub\n".identifier."\t(  )\n{\n\n\n\treturn ;\n}"
  let @z= @z."\t\t\t\t// ----------  end of subroutine ".identifier."  ----------" 
	  put z
endfunction
"
"=====================================================================================
"
