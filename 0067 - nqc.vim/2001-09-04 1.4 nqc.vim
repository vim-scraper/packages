"###############################################################################################
"
"     Filename:  nqc.vim
"
"  Description:  gvim-menus for NQC (Not Quite C) ,  Version 2.3r1 .
"
"                NQC stands for Not Quite C, and is a simple language for programmimg
"                several LEGO MINDSTORMS products:   RCX, CyberMaster, Scout and RCX2.
"                The language is described in:
"                  NQC Programmers's Guide, Version 2.3r1, by Dave Baum
"                  ( http://www.enteract.com/~dbaum/ )
"
"                These menus turn gvim into an IDE for NQC programming:
"
"                 - insertion of NQC statements, API function calls, API constants and comments
"                 - download and start programs
"                 - download firmware
"                 - upload the RCX datalog into the current buffer
"                 - erase programs and datalogs
"                 - configurable for RCX, RCX2, CyberMaster, Scout
"
"      Version:  1.4 - LINUX / UNIX
"     Revision:  04.09.2001
"       Author:  Dr.-Ing. Fritz Mehner 
"        Email:  mehner@mfh-iserlohn.de
"      Created:  28.07.2001 - 15:40:43
"
"        Usage:  (1) Configure  nqc.vim  (section below).
"                (2) Rename  nqc.vim  to  .nqc.vim  and put it in your home directory.  
"                (3) Load  .nqc.vim  manually with the :so command:
"                      :so ~/.nqc.vim
"                    or better place this command in your file .gvimrc .
"               
"                The register z is used in many places.
"
"###############################################################################################
"               Configuration
"-------------------------------------------------------------------------------------
"
"  Use my personalization as an example.
"
let NQCVIM_AuthorName      = "Fritz Mehner"
let NQCVIM_AuthorSign      = "fm"
"
"  RCX-Firmware, full path and filename :
"
let NQCVIM_RCX_Firmware    = "~/bin/firm0309.lgo"
"
"  Supported robot type:  0 = no / 1 =yes ; multiple choices are possible 
"
let  NQCVIM_rcx            = 1
let  NQCVIM_rcx_2          = 0
let  NQCVIM_cybermaster    = 0
let  NQCVIM_scout          = 0
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
map  <F2>        :update<CR>
map  <F3>        :browse confirm e<CR>
map  <M-F9>      :w<CR><Esc>:!nqc %<CR>
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
"----- Menu : API-Functions --------------------------------------------------------------------
"===============================================================================================
"
"----- outputs ----------------------------------------------------------------------------
imenu API-Functions.outputs.Float\ (outputs)                     Float();<Esc>F(a
imenu API-Functions.outputs.Fwd\ (outputs)                       Fwd();<Esc>F(a
imenu API-Functions.outputs.Off\ (outputs)                       Off();<Esc>F(a
imenu API-Functions.outputs.On\ (outputs)                        On();<Esc>F(a
imenu API-Functions.outputs.OnFor\ (outputs,time)                OnFor(,);<Esc>F(a
imenu API-Functions.outputs.OnFwd\ (outputs)                     OnFwd();<Esc>F(a
imenu API-Functions.outputs.OnRev\ (outputs)                     OnRev();<Esc>F(a
imenu API-Functions.outputs.OutputStatus\ (n)                    OutputStatus();<Esc>F(a
imenu API-Functions.outputs.Rev\ (outputs)                       Rev();<Esc>F(a
imenu API-Functions.outputs.SetDirection\ (outputs,dir)          SetDirection(,);<Esc>F(a
imenu API-Functions.outputs.SetOutput\ (outputs,mode)            SetOutput(,);<Esc>F(a
imenu API-Functions.outputs.SetPower\ (outputs,power)            SetPower(,);<Esc>F(a
imenu API-Functions.outputs.Toggle\ (outputs)                    Toggle();<Esc>F(a
"----- sensor types, modes, information ---------------------------------------------------
imenu API-Functions.sensors.ClearSensor\ (sensor)                ClearSensor();<Esc>F(a
if NQCVIM_rcx != 0 || NQCVIM_cybermaster != 0
  imenu API-Functions.sensors.SensorMode\ (n)                    SensorMode();<Esc>F(a
endif
imenu API-Functions.sensors.SensorType\ (n)                      SensorType();<Esc>F(a
if NQCVIM_rcx != 0
  imenu API-Functions.sensors.SensorValueBool\ (n)               SensorValueBool();<Esc>F(a
endif
if NQCVIM_rcx != 0 || NQCVIM_scout != 0
  imenu API-Functions.sensors.SensorValueRaw\ (n)                SensorValueRaw();<Esc>F(a
endif
imenu API-Functions.sensors.SensorValue\ (n)                     SensorValue();<Esc>F(a
if NQCVIM_scout != 0
  imenu API-Functions.sensors.SetSensorLowerLimit\ (value)       SetSensorLowerLimit();<Esc>F(a
  imenu API-Functions.sensors.SetSensorUpperLimit\ (value)       SetSensorUpperLimit();<Esc>F(a
  imenu API-Functions.sensors.SetSensorHysteresis\ (value)       SetSensorHysteresis();<Esc>F(a
  imenu API-Functions.sensors.CalibrateSensor\ (\ )              CalibrateSensor();
endif
if NQCVIM_rcx != 0
  imenu API-Functions.sensors.SetSensor\ (sensor,config)         SetSensor(,);<Esc>F(a
endif
if NQCVIM_rcx != 0 || NQCVIM_cybermaster != 0
  imenu API-Functions.sensors.SetSensorMode\ (sensor,mode)       SetSensorMode(,);<Esc>F(a
endif
if NQCVIM_rcx != 0
  imenu API-Functions.sensors.SetSensorType\ (sensor,type)       SetSensorType(,);<Esc>F(a
endif
"----- timers and counters ----------------------------------------------------------------
imenu API-Functions.timers\ counters.ClearTimer\ (n)             ClearTimer();<Esc>F(a
imenu API-Functions.timers\ counters.Timer\ (n)                  Timer();<Esc>F(a
if NQCVIM_rcx_2 != 0
  imenu API-Functions.timers\ counters.FastTimer\ (n)            FastTimer();<Esc>F(a
  imenu API-Functions.timers\ counters.SetTimer\ (n,value)       SetTimer(,);<Esc>F(a
endif
if NQCVIM_rcx != 0 || NQCVIM_scout != 0
  imenu API-Functions.timers\ counters.Counter\ (n)              Timer();<Esc>F(a
endif
if NQCVIM_rcx_2 != 0 || NQCVIM_scout != 0
  imenu API-Functions.timers\ counters.ClearCounter\ (n)         ClearCounter();<Esc>F(a
  imenu API-Functions.timers\ counters.DecCounter\ (n)           DecCounter();<Esc>F(a
  imenu API-Functions.timers\ counters.IncCounter\ (n)           IncCounter();<Esc>F(a
endif
"----- sounds -----------------------------------------------------------------------------
if NQCVIM_rcx_2 != 0
  imenu API-Functions.sounds.ClearSound\ (\n)                    ClearSound();
endif
if NQCVIM_rcx_2 != 0 || NQCVIM_scout != 0
  imenu API-Functions.sounds.MuteSound\ (\n)                     MuteSound();
endif
imenu API-Functions.sounds.PlaySound\ (sound)                    PlaySound();<Esc>F(a
imenu API-Functions.sounds.PlayTone\ (freq,duration)             PlayTone(,);<Esc>F(a
if NQCVIM_rcx_2 != 0 || NQCVIM_scout != 0
  imenu API-Functions.sounds.UnmuteSound\ (\n)                   UnmuteSound();
endif
if NQCVIM_scout != 0
  imenu API-Functions.sounds.SelectSound\ (group)                SelectSound();<Esc>F(a
endif
"----- LCD display ------------------------------------------------------------------------
if NQCVIM_rcx != 0
  imenu API-Functions.display.SelectDisplay\ (mode)                  SelectDisplay();<Esc>F(a
endif
if NQCVIM_rcx_2 != 0
  imenu API-Functions.display.SelectUserDisplay\ (value,precision)   SelectUserDisplay(,);<Esc>F(a
endif
"----- messages ---------------------------------------------------------------------------
if NQCVIM_rcx != 0 || NQCVIM_scout != 0
  imenu API-Functions.messages.ClearMessage\ (\ )                ClearMessage();
  imenu API-Functions.messages.Message\ (\ )                     Message();
  imenu API-Functions.messages.SendMessage\ (message)            SendMessage();<Esc>F(a
  imenu API-Functions.messages.SetTxPower\ (power)               SetTxPower();<Esc>F(a
endif
"----- general ----------------------------------------------------------------------------
imenu API-Functions.general.Random\ (n)                          Random();<Esc>F(a
imenu API-Functions.general.SetSleepTime\ (minutes)              SetSleepTime();<Esc>F(a
if NQCVIM_rcx_2 != 0
  imenu API-Functions.general.SetRandomSeed\ (n)                 SetRandomSeed();<Esc>F(a
endif
imenu API-Functions.general.SleepNow\ (\ )                       SleepNow();
imenu API-Functions.general.StopAllTasks\ (\ )                   StopAllTasks();
imenu API-Functions.general.Wait\ (time)                         Wait();<Esc>F(a

"----- RCX features -----------------------------------------------------------------------
if NQCVIM_rcx != 0
  imenu API-Functions.RCX\ features.Program\ (\ )                Program();
  imenu API-Functions.RCX\ features.SetWatch\ (hours,minutes)    SetWatch(,);<Esc>F(a
  imenu API-Functions.RCX\ features.Watch\ (\n)                  Watch();<Esc>F(a
endif
if NQCVIM_rcx_2 != 0
  imenu API-Functions.RCX\ features.SelectProgram\ (n)           SelectProgram();<Esc>F(a
  imenu API-Functions.RCX\ features.BatteryLevel\ (\ )           BatteryLevel();
  imenu API-Functions.RCX\ features.FirmwareVersion\ (\ )        FirmwareVersion();
endif
"----- SCOUT features -----------------------------------------------------------------------
if NQCVIM_scout != 0
  imenu API-Functions.Scout\ features.EventFeedback\ (\ )                           EventFeedback();
  imenu API-Functions.Scout\ features.ScoutRules\ (n)                               ScoutRules();<Esc>F(a
  imenu API-Functions.Scout\ features.SetEventFeedback\ (events)                    SetEventFeedback();<Esc>F(a
  imenu API-Functions.Scout\ features.SetLight\ (mode)                              SetLight();<Esc>F(a
  imenu API-Functions.Scout\ features.SetScoutRules\ (motion,touch,light,time,fx)   SetScoutRules(,,,,);<Esc>F(a
  imenu API-Functions.Scout\ features.SetScoutMode\ (mode)                          SetScoutMode();<Esc>F(a
endif
"----- CYBERMASTER features -----------------------------------------------------------------------
if NQCVIM_cybermaster != 0
  imenu API-Functions.cybermaster\ features.Drive\ (motor0,motor1)                  Drive(,);<Esc>F(a
  imenu API-Functions.cybermaster\ features.OnWait\ (motors,time)                   OnWait(,);<Esc>F(a
  imenu API-Functions.cybermaster\ features.OnWaitDifferent\ (motors,n0,n1,n2,time) OnWaitDifferent(,,,,);<Esc>F(a
  imenu API-Functions.cybermaster\ features.ClearTachoCounter\ (motors)             ClearTachoCounter();<Esc>F(a
  imenu API-Functions.cybermaster\ features.TachoCount\ (n)                         TachoCount();<Esc>F(a
  imenu API-Functions.cybermaster\ features.TachoSpeed\ (n)                         TachoSpeed();<Esc>F(a
  imenu API-Functions.cybermaster\ features.ExternalMotorRunning\ (\n)              ExternalMotorRunning();
  imenu API-Functions.cybermaster\ features.AGC\ (\ )                               AGC();
endif
"----- datalog ----------------------------------------------------------------------------
if NQCVIM_rcx != 0
  imenu API-Functions.datalog.AddToDatalog\ (value)              AddToDatalog();<Esc>F(a
  imenu API-Functions.datalog.CreateDatalog\ (size)              CreateDatalog();<Esc>F(a
  imenu API-Functions.datalog.UploadDatalog\ (start,count)       UploadDatalog(,);<Esc>F(a
endif
"----- global control  --------------------------------------------------------------------
if NQCVIM_rcx_2 != 0 || NQCVIM_scout != 0
  imenu API-Functions.global\ control.SetGlobalOutput\ (outputs,mode)            SetGlobalOutput(,);<Esc>F(a
  imenu API-Functions.global\ control.SetGlobalDirection\ (outputs,mode)         SetGlobalDirection(,);<Esc>F(a
  imenu API-Functions.global\ control.SetMaxPower\ (outputs,power)               SetMaxPower(,);<Esc>F(a
  imenu API-Functions.global\ control.GlobalOutputStatus\ (n)                    GlobalOutputStatus();<Esc>F(a
endif
"----- serial  ----------------------------------------------------------------------------
if NQCVIM_rcx_2 != 0
  imenu API-Functions.serial.SetSerialComm\ (settings)           SetSerialComm();<Esc>F(a
  imenu API-Functions.serial.SetSerialPacket\ (settings)         SetSerialPacket();<Esc>F(a
  imenu API-Functions.serial.SetSerialData\ (n,value)            SetSerialData(,);<Esc>F(a
  imenu API-Functions.serial.SerialData\ (n)                     SerialData();<Esc>F(a
  imenu API-Functions.serial.SendSerial\ (start,count)           SendSerial(,);<Esc>F(a
endif
"----- VLL  --------------------------------------------------------------------------------
if NQCVIM_scout != 0
  imenu API-Functions.VLL.SendVLL\ (value)                       SendVLL();<Esc>F(a
endif
"----- access control ----------------------------------------------------------------------
if NQCVIM_rcx_2 != 0 || NQCVIM_scout != 0
  imenu API-Functions.access\ control.SetPriority\ (p)           SetPriority();<Esc>F(a
endif
"----- RCX2 events -------------------------------------------------------------------------
if NQCVIM_rcx_2 != 0
	imenu API-Functions.RCX2\ events.CalibrateEvent\ (event,lower,upper,hyst)    CalibrateEvent();<Esc>F(a          
	imenu API-Functions.RCX2\ events.ClearAllEvents\ (\ )                        ClearAllEvents();
	imenu API-Functions.RCX2\ events.ClearEvent(event)                           ClearEvent();<Esc>F(a
	imenu API-Functions.RCX2\ events.ClickCounter\ (event)                       ClickCounter();<Esc>F(a
	imenu API-Functions.RCX2\ events.ClickTime\ (event)                          ClickTime();<Esc>F(a
	imenu API-Functions.RCX2\ events.EventState\ (event)                         EventState();<Esc>F(a
	imenu API-Functions.RCX2\ events.Hysteresis\ (event)                         Hysteresis();<Esc>F(a
	imenu API-Functions.RCX2\ events.LowerLimit\ (event)                         LowerLimit();<Esc>F(a
	imenu API-Functions.RCX2\ events.SetClickCounter\ (event,value)              SetClickCounter();<Esc>F(a
	imenu API-Functions.RCX2\ events.SetClickTime\ (event,value)                 SetClickTime();<Esc>F(a
	imenu API-Functions.RCX2\ events.SetEvent\ (event,source,type)               SetEvent();<Esc>F(a
	imenu API-Functions.RCX2\ events.SetHysteresis\ (event,value)                SetHysteresis();<Esc>F(a
	imenu API-Functions.RCX2\ events.SetLowerLimit\ (event,limit)                SetLowerLimit();<Esc>F(a
	imenu API-Functions.RCX2\ events.SetUpperLimit\ (event,limit)                SetUpperLimit();<Esc>F(a
	imenu API-Functions.RCX2\ events.UpperLimit\ (event)                         UpperLimit();<Esc>F(a
  imenu API-Functions.RCX2\ events.ActiveEvents\ (task)                        ActiveEvents();<Esc>F(a
  imenu API-Functions.RCX2\ events.CurrentEvents\ (\ )                         CurrentEvents();
  imenu API-Functions.RCX2\ events.Events\ (events)                            Events();<Esc>F(a
endif
"----- Scout events ------------------------------------------------------------------------
if NQCVIM_scout != 0
  imenu API-Functions.Scout\ events.ActiveEvents\ (task)                ActiveEvents();<Esc>F(a
  imenu API-Functions.Scout\ events.Events\ (events)                    Events();<Esc>F(a
  imenu API-Functions.Scout\ events.SetSensorClickTime\ (value)         SetSensorClickTime();<Esc>F(a
  imenu API-Functions.Scout\ events.SetCounterLimit\ (n,value)          SetCounterLimit(,);<Esc>F(a
  imenu API-Functions.Scout\ events.SetTimerLimit\ (n,value)            SetTimerLimit(,);<Esc>F(a
endif
"
"
"===============================================================================================
"----- Menu : API-Constants --------------------------------------------------------------------
"===============================================================================================
"
"----- access control ---------------------------------------------------------------------
if NQCVIM_rcx_2 != 0 || NQCVIM_scout != 0
	imenu API-Constants.access\ control.ACQUIRE_OUT_A   ACQUIRE_OUT_A
	imenu API-Constants.access\ control.ACQUIRE_OUT_B   ACQUIRE_OUT_B
	imenu API-Constants.access\ control.ACQUIRE_OUT_C   ACQUIRE_OUT_C
	imenu API-Constants.access\ control.ACQUIRE_SOUND   ACQUIRE_SOUND
endif
if NQCVIM_rcx_2 != 0
	imenu API-Constants.access\ control.ACQUIRE_USER_1  ACQUIRE_USER_1
	imenu API-Constants.access\ control.ACQUIRE_USER_2  ACQUIRE_USER_2
	imenu API-Constants.access\ control.ACQUIRE_USER_3  ACQUIRE_USER_3
	imenu API-Constants.access\ control.ACQUIRE_USER_4  ACQUIRE_USER_4
endif
"----- display ----------------------------------------------------------------------------
if NQCVIM_rcx != 0 || NQCVIM_rcx_2 != 0
	imenu API-Constants.display.DISPLAY_OUT_A           DISPLAY_OUT_A                   
	imenu API-Constants.display.DISPLAY_OUT_B           DISPLAY_OUT_B
	imenu API-Constants.display.DISPLAY_OUT_C           DISPLAY_OUT_C
	imenu API-Constants.display.DISPLAY_SENSOR_1        DISPLAY_SENSOR_1
	imenu API-Constants.display.DISPLAY_SENSOR_2        DISPLAY_SENSOR_2
	imenu API-Constants.display.DISPLAY_SENSOR_3        DISPLAY_SENSOR_3
	imenu API-Constants.display.DISPLAY_WATCH           DISPLAY_WATCH
endif
if NQCVIM_rcx_2 != 0
	imenu API-Constants.display.DISPLAY_USER            DISPLAY_USER
endif
"----- output  ----------------------------------------------------------------------------
imenu API-Constants.output.OUT_A                    OUT_A
imenu API-Constants.output.OUT_B                    OUT_B
imenu API-Constants.output.OUT_C                    OUT_C
imenu API-Constants.output.OUT_FLOAT                OUT_FLOAT
imenu API-Constants.output.OUT_FULL                 OUT_FULL
imenu API-Constants.output.OUT_FWD                  OUT_FWD
imenu API-Constants.output.OUT_HALF                 OUT_HALF
imenu API-Constants.output.OUT_LOW                  OUT_LOW
imenu API-Constants.output.OUT_OFF                  OUT_OFF
imenu API-Constants.output.OUT_ON                   OUT_ON
imenu API-Constants.output.OUT_REV                  OUT_REV
imenu API-Constants.output.OUT_TOGGLE               OUT_TOGGLE
"----- sensor  ----------------------------------------------------------------------------
if NQCVIM_rcx_2 != 0
  imenu API-Constants.serial.SERIAL_COMM_DEFAULT      SERIAL_COMM_DEFAULT
  imenu API-Constants.serial.SERIAL_COMM_4800         SERIAL_COMM_4800 
  imenu API-Constants.serial.SERIAL_COMM_DUTY25       SERIAL_COMM_DUTY25
  imenu API-Constants.serial.SERIAL_COMM_76KHZ        SERIAL_COMM_76KHZ
endif
"----- sensor  ----------------------------------------------------------------------------
if NQCVIM_rcx != 0 || NQCVIM_scout != 0
  imenu API-Constants.sensor.SENSOR_1                 SENSOR_1
  imenu API-Constants.sensor.SENSOR_2                 SENSOR_2
  imenu API-Constants.sensor.SENSOR_3                 SENSOR_3
  imenu API-Constants.sensor.SENSOR_CELSIUS           SENSOR_CELSIUS
  imenu API-Constants.sensor.SENSOR_EDGE              SENSOR_EDGE
  imenu API-Constants.sensor.SENSOR_FAHRENHEIT        SENSOR_FAHRENHEIT
  imenu API-Constants.sensor.SENSOR_LIGHT             SENSOR_LIGHT
  imenu API-Constants.sensor.SENSOR_MODE_BOOL         SENSOR_MODE_BOOL
  imenu API-Constants.sensor.SENSOR_MODE_CELSIUS      SENSOR_MODE_CELSIUS
  imenu API-Constants.sensor.SENSOR_MODE_EDGE         SENSOR_MODE_EDGE
  imenu API-Constants.sensor.SENSOR_MODE_FAHRENHEIT   SENSOR_MODE_FAHRENHEIT
  imenu API-Constants.sensor.SENSOR_MODE_PERCENT      SENSOR_MODE_PERCENT
  imenu API-Constants.sensor.SENSOR_MODE_PULSE        SENSOR_MODE_PULSE
  imenu API-Constants.sensor.SENSOR_MODE_RAW          SENSOR_MODE_RAW
  imenu API-Constants.sensor.SENSOR_MODE_ROTATION     SENSOR_MODE_ROTATION
  imenu API-Constants.sensor.SENSOR_PULSE             SENSOR_PULSE
  imenu API-Constants.sensor.SENSOR_ROTATION          SENSOR_ROTATION
  imenu API-Constants.sensor.SENSOR_TOUCH             SENSOR_TOUCH
  imenu API-Constants.sensor.SENSOR_TYPE_LIGHT        SENSOR_TYPE_LIGHT
  imenu API-Constants.sensor.SENSOR_TYPE_NONE         SENSOR_TYPE_NONE
  imenu API-Constants.sensor.SENSOR_TYPE_ROTATION     SENSOR_TYPE_ROTATION
  imenu API-Constants.sensor.SENSOR_TYPE_TEMPERATURE  SENSOR_TYPE_TEMPERATURE
  imenu API-Constants.sensor.SENSOR_TYPE_TOUCH        SENSOR_TYPE_TOUCH
endif
"----- sound   ----------------------------------------------------------------------------
imenu API-Constants.sound.SOUND_CLICK               SOUND_CLICK
imenu API-Constants.sound.SOUND_DOUBLE_BEEP         SOUND_DOUBLE_BEEP
imenu API-Constants.sound.SOUND_DOWN                SOUND_DOWN
imenu API-Constants.sound.SOUND_FAST_UP             SOUND_FAST_UP
imenu API-Constants.sound.SOUND_LOW_BEEP            SOUND_LOW_BEEP
imenu API-Constants.sound.SOUND_UP                  SOUND_UP
"----- misc    ----------------------------------------------------------------------------
imenu API-Constants.TX_POWER_HI                     TX_POWER_HI
imenu API-Constants.TX_POWER_LO                     TX_POWER_LO       
"
"----- Scout events -----------------------------------------------------------------------
if NQCVIM_scout != 0
  imenu API-Constants.Scout\ events.EVENT_1_PRESSED             EVENT_1_PRESSED
  imenu API-Constants.Scout\ events.EVENT_2_PRESSED             EVENT_2_PRESSED
  imenu API-Constants.Scout\ events.EVENT_1_RELEASED            EVENT_1_RELEASED
  imenu API-Constants.Scout\ events.EVENT_2_RELEASED            EVENT_2_RELEASED
	imenu API-Constants.Scout\ events.EVENT_LIGHT_HIGH            EVENT_LIGHT_HIGH  
	imenu API-Constants.Scout\ events.EVENT_LIGHT_NORMAL          EVENT_LIGHT_NORMAL  
	imenu API-Constants.Scout\ events.EVENT_LIGHT_LOW             EVENT_LIGHT_LOW   
	imenu API-Constants.Scout\ events.EVENT_LIGHT_CLICK           EVENT_LIGHT_CLICK  
	imenu API-Constants.Scout\ events.EVENT_LIGHT_DOUBLECLICK     EVENT_LIGHT_DOUBLECLICK  
	imenu API-Constants.Scout\ events.EVENT_COUNTER_0             EVENT_COUNTER_0   
	imenu API-Constants.Scout\ events.EVENT_COUNTER_1             EVENT_COUNTER_1   
	imenu API-Constants.Scout\ events.EVENT_TIMER_0               EVENT_TIMER_0 
	imenu API-Constants.Scout\ events.EVENT_TIMER_1               EVENT_TIMER_1 
	imenu API-Constants.Scout\ events.EVENT_TIMER_2               EVENT_TIMER_2 
	imenu API-Constants.Scout\ events.EVENT_MESSAGE               EVENT_MESSAGE 
endif
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
if NQCVIM_rcx != 0 || NQCVIM_rcx_2
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
endif
if NQCVIM_scout != 0
	amenu  NQC-&Run.download\ program\ to\ Scout               <C-C>:w<CR><Esc>:!nqc -d -TScout %<CR>
endif
if NQCVIM_cybermaster != 0
	amenu  NQC-&Run.download\ program\ to\ CyberMaster         <C-C>:w<CR><Esc>:!nqc -d -TCM %<CR>
endif
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
  let @z=    "//=====================================================================================\n"
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
  put z
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Comments : File Prologue
"------------------------------------------------------------------------------
function! NQCVIM_CommentFilePrologue ()

		let	File	= expand("%:t")								" name of the file in the current buffer 
    let @z=    "//=====================================================================================\n"
    let @z= @z."//\n"
    let @z= @z."//       Filename:\t".File."\n"
    let @z= @z."//    Description:\t\n"
    let @z= @z."//\n"
    let @z= @z."//       Compiler:\tnqc\n"
    let @z= @z."//         Author:\t".g:NQCVIM_AuthorName."\n"
    let @z= @z."//        Created:\t".strftime("%x - %X")."\n"
    let @z= @z."//=====================================================================================\n"
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
