"###############################################################################################
"
"     Filename:  nqc.vim
"
"  Description:  gvim-menus for NQC (Not Quite C),  Version 2.3r1 .
"
"                NQC stands for Not Quite C, and is a simple language for programmimg
"                several LEGO MINDSTORMS products:  RCX, CyberMaster, Scout and RCX2.
"                The language is described in:
"                  NQC Programmers's Guide, Version 2.3r1, by Dave Baum
"                  ( http://www.enteract.com/~dbaum/ )
"
"                nqc.vim turns gvim into an IDE for NQC programming:
"                 - insertion of NQC statements, API function calls, API constants and comments
"                 - download and start programs
"                 - download firmware
"                 - upload the RCX datalog into the current buffer
"                 - erase programs and datalogs
"                 - configurable for RCX, RCX2, CyberMaster, Scout
"
"      Version:  1.6.3 - LINUX / UNIX
"     Revision:  02.01.2002
"       Author:  Dr.-Ing. Fritz Mehner
"        Email:  mehner@mfh-iserlohn.de
"      Created:  28.07.2001
"
"        Usage:  (1.0) Configure  nqc.vim  (section Configuration below).
"
"                (2.1) Load  nqc.vim  manually into VIM with the 'so' command:
"                      :so ~/<any directory>/nqc.vim
"
"                      or better
"                (2.2) Load nqc.vim on startup (VIM versions below 6) :
"                      - Rename  nqc.vim  to  .nqc.vim  and move it to your home directory.
"                      - place the following command in your file .gvimrc :
"                        :so ~/.nqc.vim
"                      or
"                (2.3) Load nqc.vim on startup (VIM version 6.0 and higher) :
"                      - move this file to the directory ~/.vim/plugin/
"                
"                You will find the menu entry "Load NQC extensions" in the Tools memu.
"                The menu entry changes now to "Unload NQC extensions" .
"  
"        Hints:  The register z is used in many places.
"
"###############################################################################################
"               Configuration     (Use my configuration as an example)
"-----------------------------------------------------------------------------------------------
"
"  Personalization  (full name, email, ... ; used in comments) :
"
let NQC_AuthorName      = "Fritz Mehner"
let NQC_AuthorRef       = "Mn"
"
" ---------------------------------------------------------------------
"
"  RCX-Firmware (RCX 1.5, RCX 2.0, ... ); full path and filename :
"
let NQC_RCX_Firmware    = "~/bin/firm0328.lgo"
"
" ---------------------------------------------------------------------
"
"  Choose a target :
"
"    RCX   :  Mindstorms with RCX
"    RCX2  :  Mindstorms with RCX 2
"    Scout :  Scout
"    CM    :  Cybermaster
"
let NQC_Target     =  "RCX2"
"
" ---------------------------------------------------------------------
"
"  Specify serial port, case matters (file permission 777) :
"  /dev/ttyS0  =  COM1
"  /dev/ttyS1  =  COM2
"
let NQC_Portname   =  "/dev/ttyS0"
"
"###############################################################################################
"
function!	NQC_InitMenu ()
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
imenu  NQC-&Comments.&Frame\ Comment                   <Esc>:call NQC_CommentFrame()          <CR>jA
imenu  NQC-&Comments.task\/function\/sub\ &Description <Esc>:call NQC_CommentFunction()       <CR>:/Name<CR>A
imenu  NQC-&Comments.File\ &Prologue                   <Esc>:call NQC_CommentFilePrologue()   <CR>:/Description<CR>A
imenu  NQC-&Comments.-SEP1-                            :
imenu  NQC-&Comments.\/\/\ Date\ Time\ &Author         <Esc>$<Esc>:call NQC_CommentDateTimeAuthor() <CR>kJA
imenu  NQC-&Comments.Date\ &Time                       <Esc>:call NQC_CommentDateTime()             <CR>kJi
imenu  NQC-&Comments.Date                              <Esc>:call NQC_CommentDate()                 <CR>kJi
imenu  NQC-&Comments.-SEP2-                            :
vmenu  NQC-&Comments.&code->comment                    <Esc>:'<,'>s/^/\/\//<CR>
vmenu  NQC-&Comments.c&omment->code                    <Esc>:'<,'>s/^\/\///<CR>
"
"===============================================================================================
"----- Menu : NQC-Statements -------------------------------------------------------------------
"===============================================================================================
"      Inserting at the end of a line preserves indentation.
"-----------------------------------------------------------------------------------------------
"
imenu  NQC-St&atements.&if\ \{\ \}                 <Esc>:let @z="if (  )\n{\n\t\n}\n"                 <CR>"z]p<Esc>f(la
imenu  NQC-St&atements.if\ \{\ \}\ &else\ \{\ \}   <Esc>:let @z="if (  )\n{\n\t\n}\nelse\n{\n\t\n}\n" <CR>"z]p<Esc>f(la
imenu  NQC-St&atements.&for\ \{\ \}                <Esc>:let @z="for ( ; ;  )\n{\n\t\n}\n"            <CR>"z]p<Esc>f;i
imenu  NQC-St&atements.&while\ \{\ \}              <Esc>:let @z="while (  )\n{\n\t\n}\n"              <CR>"z]p<Esc>f(la
imenu  NQC-St&atements.&do\ \{\ \}\ while          <Esc>:call NQC_DoWhile()                           <CR>"z]p<Esc>:/while <CR>f(la
imenu  NQC-St&atements.&repeat\ \{\ \}             <Esc>:let @z="repeat (  )\n{\n\t\n}\n"             <CR>"z]p<Esc>f(la
imenu  NQC-St&atements.&until                      <Esc>:let @z="until (  );\n"                       <CR>"z]p<Esc>f(la
imenu  NQC-St&atements.&switch                     <Esc>:call NQC_CodeSwitch()                        <CR>"z]p<Esc>f(la
imenu  NQC-St&atements.&case                       <Esc>:call NQC_CodeCase()                          <CR>"z]p<Esc>f:i
imenu  NQC-St&atements.brea&k                      <Esc>:let @z="break;\n"                            <CR>"z]p<Esc>A
imenu  NQC-St&atements.c&ontinue                   <Esc>:let @z="continue;\n"                         <CR>"z]p<Esc>A
imenu  NQC-St&atements.st&art                      <Esc>:let @z="start\t;\n"                          <CR>"z]p<Esc>f;i
imenu  NQC-St&atements.-SEP1-                      :
imenu  NQC-St&atements.&task                       <Esc>:call NQC_CodeTask()<CR>
imenu  NQC-St&atements.in&line\ function           <Esc>:call NQC_CodeInlineFunction()<CR>
imenu  NQC-St&atements.su&broutine                 <Esc>:call NQC_CodeSubroutine()<CR>
imenu  NQC-St&atements.-SEP2-                      :
imenu  NQC-St&atements.#include\ \"\.\.\.\"        <Esc>:let @z="#include\t\".nqh\""                          <CR>"zp<Esc>F.i
imenu  NQC-St&atements.&#define                    <Esc>:let @z="#define\t\t\t\t// "                          <CR>"zp<Esc>4F<Tab>a
imenu  NQC-St&atements.#if&ndef\.\.#def\.\.#endif  <Esc>:call NQC_CodeIfndef()<CR>
imenu  NQC-St&atements.#ifdef\.\.#endif            #ifdef<Tab><CR><CR><CR>#endif<Esc>3kA
if g:NQC_Target=="RCX2" || g:NQC_Target=="Scout"
  imenu  NQC-St&atements.-SEP3-                      :
  imenu  NQC-St&atements.ac&quire                    <Esc>:call NQC_Acquire()<CR>f(a
  imenu  NQC-St&atements.&monitor                    <Esc>:call NQC_Monitor()<CR>f(a
  imenu  NQC-St&atements.&catch\ \(\ \)              <Esc>:call NQC_Catch()<CR>f(a
endif
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
imenu API-Functions.outputs.OutputStatus\ (n)                    OutputStatus()<Esc>F(a
imenu API-Functions.outputs.Rev\ (outputs)                       Rev();<Esc>F(a
imenu API-Functions.outputs.SetDirection\ (outputs,dir)          SetDirection(,);<Esc>F(a
imenu API-Functions.outputs.SetOutput\ (outputs,mode)            SetOutput(,);<Esc>F(a
imenu API-Functions.outputs.SetPower\ (outputs,power)            SetPower(,);<Esc>F(a
imenu API-Functions.outputs.Toggle\ (outputs)                    Toggle();<Esc>F(a
"----- sensor types, modes, information ---------------------------------------------------
imenu API-Functions.sensors.ClearSensor\ (sensor)                ClearSensor();<Esc>F(a
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2" || g:NQC_Target=="CM"
  imenu API-Functions.sensors.SensorMode\ (n)                    SensorMode();<Esc>F(a
endif
imenu API-Functions.sensors.SensorType\ (n)                      SensorType();<Esc>F(a
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2"
  imenu API-Functions.sensors.SensorValueBool\ (n)               SensorValueBool()<Esc>F(a
endif
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2" || g:NQC_Target=="Scout"
  imenu API-Functions.sensors.SensorValueRaw\ (n)                SensorValueRaw()<Esc>F(a
endif
imenu API-Functions.sensors.SensorValue\ (n)                     SensorValue()<Esc>F(a
if g:NQC_Target=="Scout"
  imenu API-Functions.sensors.SetSensorLowerLimit\ (value)       SetSensorLowerLimit();<Esc>F(a
  imenu API-Functions.sensors.SetSensorUpperLimit\ (value)       SetSensorUpperLimit();<Esc>F(a
  imenu API-Functions.sensors.SetSensorHysteresis\ (value)       SetSensorHysteresis();<Esc>F(a
  imenu API-Functions.sensors.CalibrateSensor\ (\ )              CalibrateSensor();
endif
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2"
  imenu API-Functions.sensors.SetSensor\ (sensor,config)         SetSensor(,);<Esc>F(a
endif
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2" || g:NQC_Target=="CM"
  imenu API-Functions.sensors.SetSensorMode\ (sensor,mode)       SetSensorMode(,);<Esc>F(a
endif
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2"
  imenu API-Functions.sensors.SetSensorType\ (sensor,type)       SetSensorType(,)<Esc>F(a
endif
"----- timers and counters ----------------------------------------------------------------
imenu API-Functions.timers\ counters.ClearTimer\ (n)             ClearTimer();<Esc>F(a
imenu API-Functions.timers\ counters.Timer\ (n)                  Timer()<Esc>F(a
if g:NQC_Target=="RCX2"
  imenu API-Functions.timers\ counters.FastTimer\ (n)            FastTimer()<Esc>F(a
  imenu API-Functions.timers\ counters.SetTimer\ (n,value)       SetTimer(,);<Esc>F(a
endif
if g:NQC_Target=="RCX2" || g:NQC_Target=="Scout"
  imenu API-Functions.timers\ counters.Counter\ (n)              Counter()<Esc>F(a
  imenu API-Functions.timers\ counters.ClearCounter\ (n)         ClearCounter();<Esc>F(a
  imenu API-Functions.timers\ counters.DecCounter\ (n)           DecCounter();<Esc>F(a
  imenu API-Functions.timers\ counters.IncCounter\ (n)           IncCounter();<Esc>F(a
endif
"----- sounds -----------------------------------------------------------------------------
if g:NQC_Target=="RCX2"
  imenu API-Functions.sounds.ClearSound\ (\n)                    ClearSound();
endif
imenu API-Functions.sounds.PlaySound\ (sound)                    PlaySound();<Esc>F(a
imenu API-Functions.sounds.PlayTone\ (freq,duration)             PlayTone(,);<Esc>F(a
if g:NQC_Target=="RCX2" || g:NQC_Target=="Scout"
  imenu API-Functions.sounds.MuteSound\ (\n)                     MuteSound();
  imenu API-Functions.sounds.UnmuteSound\ (\n)                   UnmuteSound();
endif
if g:NQC_Target=="Scout"
  imenu API-Functions.sounds.SelectSound\ (group)                SelectSound();<Esc>F(a
endif
"----- LCD display ------------------------------------------------------------------------
if g:NQC_Target=="RCX" ||  g:NQC_Target=="RCX2"
  imenu API-Functions.display.SelectDisplay\ (mode)                  SelectDisplay();<Esc>F(a
endif
if g:NQC_Target=="RCX2"
  imenu API-Functions.display.SetUserDisplay\ (value,precision)      SetUserDisplay(,);<Esc>F(a
endif
"----- messages ---------------------------------------------------------------------------
if g:NQC_Target=="RCX" ||  g:NQC_Target=="RCX2" || g:NQC_Target=="Scout"
  imenu API-Functions.messages.ClearMessage\ (\ )                ClearMessage();
  imenu API-Functions.messages.Message\ (\ )                     Message()
  imenu API-Functions.messages.SendMessage\ (message)            SendMessage();<Esc>F(a
  imenu API-Functions.messages.SetTxPower\ (power)               SetTxPower();<Esc>F(a
endif
"----- general ----------------------------------------------------------------------------
imenu API-Functions.general.Random\ (n)                          Random()<Esc>F(a
imenu API-Functions.general.SetSleepTime\ (minutes)              SetSleepTime();<Esc>F(a
if g:NQC_Target=="RCX2"
  imenu API-Functions.general.SetRandomSeed\ (n)                 SetRandomSeed();<Esc>F(a
endif
imenu API-Functions.general.SleepNow\ (\ )                       SleepNow();
imenu API-Functions.general.StopAllTasks\ (\ )                   StopAllTasks();
imenu API-Functions.general.Wait\ (time)                         Wait();<Esc>F(a

"----- RCX features -----------------------------------------------------------------------
if g:NQC_Target=="RCX" ||  g:NQC_Target=="RCX2"
  imenu API-Functions.RCX\ features.Program\ (\ )                Program()
  imenu API-Functions.RCX\ features.SetWatch\ (hours,minutes)    SetWatch(,);<Esc>F(a
  imenu API-Functions.RCX\ features.Watch\ (\n)                  Watch()
endif
if g:NQC_Target=="RCX2"
  imenu API-Functions.RCX\ features.BatteryLevel\ (\ )           BatteryLevel()
  imenu API-Functions.RCX\ features.FirmwareVersion\ (\ )        FirmwareVersion()
  imenu API-Functions.RCX\ features.SelectProgram\ (n)           SelectProgram();<Esc>F(a
endif
"----- SCOUT features -----------------------------------------------------------------------
if g:NQC_Target=="Scout"
  imenu API-Functions.Scout\ features.EventFeedback\ (\ )                           EventFeedback()
  imenu API-Functions.Scout\ features.ScoutRules\ (n)                               ScoutRules()<Esc>F(a
  imenu API-Functions.Scout\ features.SetEventFeedback\ (events)                    SetEventFeedback();<Esc>F(a
  imenu API-Functions.Scout\ features.SetLight\ (mode)                              SetLight();<Esc>F(a
  imenu API-Functions.Scout\ features.SetScoutRules\ (motion,touch,light,time,fx)   SetScoutRules(,,,,);<Esc>F(a
  imenu API-Functions.Scout\ features.SetScoutMode\ (mode)                          SetScoutMode();<Esc>F(a
endif
"----- CYBERMASTER features -----------------------------------------------------------------------
if g:NQC_Target=="CM"
  imenu API-Functions.cybermaster\ features.Drive\ (motor0,motor1)                  Drive(,);<Esc>F(a
  imenu API-Functions.cybermaster\ features.OnWait\ (motors,time)                   OnWait(,);<Esc>F(a
  imenu API-Functions.cybermaster\ features.OnWaitDifferent\ (motors,n0,n1,n2,time) OnWaitDifferent(,,,,);<Esc>F(a
  imenu API-Functions.cybermaster\ features.ClearTachoCounter\ (motors)             ClearTachoCounter();<Esc>F(a
  imenu API-Functions.cybermaster\ features.TachoCount\ (n)                         TachoCount()<Esc>F(a
  imenu API-Functions.cybermaster\ features.TachoSpeed\ (n)                         TachoSpeed()<Esc>F(a
  imenu API-Functions.cybermaster\ features.ExternalMotorRunning\ (\n)              ExternalMotorRunning()
  imenu API-Functions.cybermaster\ features.AGC\ (\ )                               AGC()
endif
"----- datalog ----------------------------------------------------------------------------
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2"
  imenu API-Functions.datalog.AddToDatalog\ (value)              AddToDatalog();<Esc>F(a
  imenu API-Functions.datalog.CreateDatalog\ (size)              CreateDatalog();<Esc>F(a
  imenu API-Functions.datalog.UploadDatalog\ (start,count)       UploadDatalog(,);<Esc>F(a
endif
"----- global control  --------------------------------------------------------------------
if g:NQC_Target=="RCX2" || g:NQC_Target=="Scout"
  imenu API-Functions.global\ control.SetGlobalOutput\ (outputs,mode)            SetGlobalOutput(,);<Esc>F(a
  imenu API-Functions.global\ control.SetGlobalDirection\ (outputs,mode)         SetGlobalDirection(,);<Esc>F(a
  imenu API-Functions.global\ control.SetMaxPower\ (outputs,power)               SetMaxPower(,);<Esc>F(a
  imenu API-Functions.global\ control.GlobalOutputStatus\ (n)                    GlobalOutputStatus()<Esc>F(a
endif
"----- serial  ----------------------------------------------------------------------------
if g:NQC_Target=="RCX2"
  imenu API-Functions.serial.InternalMessage\ (message)          InternalMessage();<Esc>F(a
  imenu API-Functions.serial.SetSerialComm\ (settings)           SetSerialComm();<Esc>F(a
  imenu API-Functions.serial.SetSerialPacket\ (settings)         SetSerialPacket();<Esc>F(a
  imenu API-Functions.serial.SetSerialData\ (n,value)            SetSerialData(,);<Esc>F(a
  imenu API-Functions.serial.SerialComm\ (\ )                    SerialComm()
  imenu API-Functions.serial.SerialData\ (n)                     SerialData()<Esc>F(a
  imenu API-Functions.serial.SerialPacket\ (\ )                  SerialPacket()
  imenu API-Functions.serial.SendSerial\ (start,count)           SendSerial(,);<Esc>F(a
endif
"----- VLL  --------------------------------------------------------------------------------
if g:NQC_Target=="Scout"
  imenu API-Functions.VLL.SendVLL\ (value)                       SendVLL();<Esc>F(a
endif
"----- access control ----------------------------------------------------------------------
if g:NQC_Target=="RCX2" || g:NQC_Target=="Scout"
  imenu API-Functions.access\ control.SetPriority\ (p)           SetPriority();<Esc>F(a
endif
"----- RCX2 events -------------------------------------------------------------------------
if g:NQC_Target=="RCX2"
  imenu API-Functions.RCX2\ events.CalibrateEvent\ (event,lower,upper,hyst)    CalibrateEvent(,,,);<Esc>F(a
  imenu API-Functions.RCX2\ events.ClearAllEvents\ (\ )                        ClearAllEvents()
  imenu API-Functions.RCX2\ events.ClearEvent\ (event)                         ClearEvent()<Esc>F(a
  imenu API-Functions.RCX2\ events.ClickCounter\ (event)                       ClickCounter()<Esc>F(a
  imenu API-Functions.RCX2\ events.ClickTime\ (event)                          ClickTime()<Esc>F(a
  imenu API-Functions.RCX2\ events.Event\ (event)                              Event();<Esc>F(a
  imenu API-Functions.RCX2\ events.EventState\ (event)                         EventState()<Esc>F(a
  imenu API-Functions.RCX2\ events.Hysteresis\ (event)                         Hysteresis()<Esc>F(a
  imenu API-Functions.RCX2\ events.LowerLimit\ (event)                         LowerLimit()<Esc>F(a
  imenu API-Functions.RCX2\ events.SetClickCounter\ (event,value)              SetClickCounter();<Esc>F(a
  imenu API-Functions.RCX2\ events.SetClickTime\ (event,value)                 SetClickTime();<Esc>F(a
  imenu API-Functions.RCX2\ events.SetEvent\ (event,source,type)               SetEvent();<Esc>F(a
  imenu API-Functions.RCX2\ events.SetHysteresis\ (event,value)                SetHysteresis();<Esc>F(a
  imenu API-Functions.RCX2\ events.SetLowerLimit\ (event,limit)                SetLowerLimit();<Esc>F(a
  imenu API-Functions.RCX2\ events.SetUpperLimit\ (event,limit)                SetUpperLimit();<Esc>F(a
  imenu API-Functions.RCX2\ events.UpperLimit\ (event)                         UpperLimit()<Esc>F(a
  imenu API-Functions.RCX2\ events.ActiveEvents\ (task)                        ActiveEvents()<Esc>F(a
  imenu API-Functions.RCX2\ events.CurrentEvents\ (\ )                         CurrentEvents();
  imenu API-Functions.RCX2\ events.Events\ (events)                            Events()<Esc>F(a
endif
"----- Scout events ------------------------------------------------------------------------
if g:NQC_Target=="Scout"
  imenu API-Functions.Scout\ events.ActiveEvents\ (task)                ActiveEvents();<Esc>F(a
  imenu API-Functions.Scout\ events.CounterLimit\ (n)                   CounterLimit()<Esc>F(a
  imenu API-Functions.Scout\ events.Events\ (events)                    Events();<Esc>F(a
  imenu API-Functions.Scout\ events.SetSensorClickTime\ (time)          SetSensorClickTime();<Esc>F(a
  imenu API-Functions.Scout\ events.SetCounterLimit\ (n,value)          SetCounterLimit(,);<Esc>F(a
  imenu API-Functions.Scout\ events.SetTimerLimit\ (n,value)            SetTimerLimit(,);<Esc>F(a
  imenu API-Functions.Scout\ events.TimerLimit\ (n)                     TimerLimit()<Esc>F(a
endif
"
"
"===============================================================================================
"----- Menu : API-Constants --------------------------------------------------------------------
"===============================================================================================
"
"----- access control ---------------------------------------------------------------------
if g:NQC_Target=="RCX2" || g:NQC_Target=="Scout"
  imenu API-Constants.access\ control.ACQUIRE_OUT_A   ACQUIRE_OUT_A
  imenu API-Constants.access\ control.ACQUIRE_OUT_B   ACQUIRE_OUT_B
  imenu API-Constants.access\ control.ACQUIRE_OUT_C   ACQUIRE_OUT_C
  imenu API-Constants.access\ control.ACQUIRE_SOUND   ACQUIRE_SOUND
endif
if g:NQC_Target=="RCX2"
  imenu API-Constants.access\ control.ACQUIRE_USER_1  ACQUIRE_USER_1
  imenu API-Constants.access\ control.ACQUIRE_USER_2  ACQUIRE_USER_2
  imenu API-Constants.access\ control.ACQUIRE_USER_3  ACQUIRE_USER_3
  imenu API-Constants.access\ control.ACQUIRE_USER_4  ACQUIRE_USER_4
endif
"----- display ----------------------------------------------------------------------------
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2"
  imenu API-Constants.display.DISPLAY_OUT_A           DISPLAY_OUT_A
  imenu API-Constants.display.DISPLAY_OUT_B           DISPLAY_OUT_B
  imenu API-Constants.display.DISPLAY_OUT_C           DISPLAY_OUT_C
  imenu API-Constants.display.DISPLAY_SENSOR_1        DISPLAY_SENSOR_1
  imenu API-Constants.display.DISPLAY_SENSOR_2        DISPLAY_SENSOR_2
  imenu API-Constants.display.DISPLAY_SENSOR_3        DISPLAY_SENSOR_3
  imenu API-Constants.display.DISPLAY_WATCH           DISPLAY_WATCH
endif
if g:NQC_Target=="RCX2"
  imenu API-Constants.display.DISPLAY_USER            DISPLAY_USER
endif
"----- output  ----------------------------------------------------------------------------
imenu API-Constants.output.OUT_A                      OUT_A
imenu API-Constants.output.OUT_B                      OUT_B
imenu API-Constants.output.OUT_C                      OUT_C
imenu API-Constants.output.OUT_FLOAT                  OUT_FLOAT
imenu API-Constants.output.OUT_FULL                   OUT_FULL
imenu API-Constants.output.OUT_FWD                    OUT_FWD
imenu API-Constants.output.OUT_HALF                   OUT_HALF
imenu API-Constants.output.OUT_LOW                    OUT_LOW
imenu API-Constants.output.OUT_OFF                    OUT_OFF
imenu API-Constants.output.OUT_ON                     OUT_ON
imenu API-Constants.output.OUT_REV                    OUT_REV
imenu API-Constants.output.OUT_TOGGLE                 OUT_TOGGLE
if g:NQC_Target=="CM"
  imenu API-Constants.output.OUT_L                    OUT_L
  imenu API-Constants.output.OUT_R                    OUT_R
  imenu API-Constants.output.OUT_X                    OUT_X
endif
"----- sensor  ----------------------------------------------------------------------------
if g:NQC_Target=="RCX2"
  imenu API-Constants.serial.SERIAL_COMM_DEFAULT      SERIAL_COMM_DEFAULT
  imenu API-Constants.serial.SERIAL_COMM_4800         SERIAL_COMM_4800
  imenu API-Constants.serial.SERIAL_COMM_DUTY25       SERIAL_COMM_DUTY25
  imenu API-Constants.serial.SERIAL_COMM_76KHZ        SERIAL_COMM_76KHZ
  imenu API-Constants.serial.SERIAL_PACKET_DEFAULT    SERIAL_PACKET_DEFAULT
  imenu API-Constants.serial.SERIAL_PACKET_PREAMBLE   SERIAL_PACKET_PREAMBLE
  imenu API-Constants.serial.SERIAL_PACKET_NEGATED    SERIAL_PACKET_NEGATED
  imenu API-Constants.serial.SERIAL_PACKET_CHECKSUM   SERIAL_PACKET_CHECKSUM
  imenu API-Constants.serial.SERIAL_PACKET_RCX        SERIAL_PACKET_RCX
endif
"----- sensor  ----------------------------------------------------------------------------
  imenu API-Constants.sensor.SENSOR_1                 SENSOR_1
  imenu API-Constants.sensor.SENSOR_2                 SENSOR_2
  imenu API-Constants.sensor.SENSOR_3                 SENSOR_3
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2"
  imenu API-Constants.sensor.SENSOR_CELSIUS           SENSOR_CELSIUS
  imenu API-Constants.sensor.SENSOR_EDGE              SENSOR_EDGE
  imenu API-Constants.sensor.SENSOR_FAHRENHEIT        SENSOR_FAHRENHEIT
  imenu API-Constants.sensor.SENSOR_LIGHT             SENSOR_LIGHT
  imenu API-Constants.sensor.SENSOR_PULSE             SENSOR_PULSE
  imenu API-Constants.sensor.SENSOR_ROTATION          SENSOR_ROTATION
  imenu API-Constants.sensor.SENSOR_TOUCH             SENSOR_TOUCH
  imenu API-Constants.sensor.SENSOR_MODE_CELSIUS      SENSOR_MODE_CELSIUS
  imenu API-Constants.sensor.SENSOR_MODE_FAHRENHEIT   SENSOR_MODE_FAHRENHEIT
  imenu API-Constants.sensor.SENSOR_MODE_ROTATION     SENSOR_MODE_ROTATION
endif
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2" ||  g:NQC_Target=="CM"
  imenu API-Constants.sensor.SENSOR_MODE_BOOL         SENSOR_MODE_BOOL
  imenu API-Constants.sensor.SENSOR_MODE_EDGE         SENSOR_MODE_EDGE
  imenu API-Constants.sensor.SENSOR_MODE_PERCENT      SENSOR_MODE_PERCENT
  imenu API-Constants.sensor.SENSOR_MODE_PULSE        SENSOR_MODE_PULSE
  imenu API-Constants.sensor.SENSOR_MODE_RAW          SENSOR_MODE_RAW
endif
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2"
  imenu API-Constants.sensor.SENSOR_TYPE_LIGHT        SENSOR_TYPE_LIGHT
  imenu API-Constants.sensor.SENSOR_TYPE_NONE         SENSOR_TYPE_NONE
  imenu API-Constants.sensor.SENSOR_TYPE_ROTATION     SENSOR_TYPE_ROTATION
  imenu API-Constants.sensor.SENSOR_TYPE_TEMPERATURE  SENSOR_TYPE_TEMPERATURE
  imenu API-Constants.sensor.SENSOR_TYPE_TOUCH        SENSOR_TYPE_TOUCH
endif
if g:NQC_Target=="CM"
  imenu API-Constants.sensor.SENSOR_L                 SENSOR_L
  imenu API-Constants.sensor.SENSOR_M                 SENSOR_M
  imenu API-Constants.sensor.SENSOR_R                 SENSOR_R
endif
"----- sound   ----------------------------------------------------------------------------
imenu API-Constants.sound.SOUND_CLICK                 SOUND_CLICK
imenu API-Constants.sound.SOUND_DOUBLE_BEEP           SOUND_DOUBLE_BEEP
imenu API-Constants.sound.SOUND_DOWN                  SOUND_DOWN
imenu API-Constants.sound.SOUND_FAST_UP               SOUND_FAST_UP
imenu API-Constants.sound.SOUND_LOW_BEEP              SOUND_LOW_BEEP
imenu API-Constants.sound.SOUND_UP                    SOUND_UP
"----- RCX2 events ------------------------------------------------------------------------
if g:NQC_Target=="RCX2"
  imenu API-Constants.RCX2\ events.EVENT_TYPE_PRESSED       EVENT_TYPE_PRESSED
  imenu API-Constants.RCX2\ events.EVENT_TYPE_RELEASED      EVENT_TYPE_RELEASED
  imenu API-Constants.RCX2\ events.EVENT_TYPE_PULSE         EVENT_TYPE_PULSE
  imenu API-Constants.RCX2\ events.EVENT_TYPE_EDGE          EVENT_TYPE_EDGE
  imenu API-Constants.RCX2\ events.EVENT_TYPE_FASTCHANGE    EVENT_TYPE_FASTCHANGE
  imenu API-Constants.RCX2\ events.EVENT_TYPE_LOW           EVENT_TYPE_LOW
  imenu API-Constants.RCX2\ events.EVENT_TYPE_NORMAL        EVENT_TYPE_NORMAL
  imenu API-Constants.RCX2\ events.EVENT_TYPE_HIGH          EVENT_TYPE_HIGH
  imenu API-Constants.RCX2\ events.EVENT_TYPE_CLICK         EVENT_TYPE_CLICK
  imenu API-Constants.RCX2\ events.EVENT_TYPE_DOUBLECLICK   EVENT_TYPE_DOUBLECLICK
  imenu API-Constants.RCX2\ events.EVENT_TYPE_MESSAGE       EVENT_TYPE_MESSAGE
endif
"----- Scout events -----------------------------------------------------------------------
if g:NQC_Target=="Scout"
  imenu API-Constants.Scout\ events.EVENT_1_PRESSED         EVENT_1_PRESSED
  imenu API-Constants.Scout\ events.EVENT_2_PRESSED         EVENT_2_PRESSED
  imenu API-Constants.Scout\ events.EVENT_1_RELEASED        EVENT_1_RELEASED
  imenu API-Constants.Scout\ events.EVENT_2_RELEASED        EVENT_2_RELEASED
  imenu API-Constants.Scout\ events.EVENT_LIGHT_HIGH        EVENT_LIGHT_HIGH
  imenu API-Constants.Scout\ events.EVENT_LIGHT_NORMAL      EVENT_LIGHT_NORMAL
  imenu API-Constants.Scout\ events.EVENT_LIGHT_LOW         EVENT_LIGHT_LOW
  imenu API-Constants.Scout\ events.EVENT_LIGHT_CLICK       EVENT_LIGHT_CLICK
  imenu API-Constants.Scout\ events.EVENT_LIGHT_DOUBLECLICK EVENT_LIGHT_DOUBLECLICK
  imenu API-Constants.Scout\ events.EVENT_COUNTER_0         EVENT_COUNTER_0
  imenu API-Constants.Scout\ events.EVENT_COUNTER_1         EVENT_COUNTER_1
  imenu API-Constants.Scout\ events.EVENT_TIMER_0           EVENT_TIMER_0
  imenu API-Constants.Scout\ events.EVENT_TIMER_1           EVENT_TIMER_1
  imenu API-Constants.Scout\ events.EVENT_TIMER_2           EVENT_TIMER_2
  imenu API-Constants.Scout\ events.EVENT_MESSAGE           EVENT_MESSAGE
  imenu API-Constants.LIGHT_ON                              LIGHT_ON
  imenu API-Constants.LIGHT_OFF                             LIGHT_OFF
  imenu API-Constants.SCOUT_MODE_POWER                      SCOUT_MODE_POWER
  imenu API-Constants.SCOUT_MODE_STANDALONE                 SCOUT_MODE_STANDALONE
endif
"----- misc    ----------------------------------------------------------------------------
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2" ||  g:NQC_Target=="Scout"
  imenu API-Constants.TX_POWER_HI                           TX_POWER_HI
  imenu API-Constants.TX_POWER_LO                           TX_POWER_LO
endif
"
"
"===============================================================================================
"----- Menu : NQC-Run  -------------------------------------------------------------------------
"===============================================================================================
"
amenu  NQC-&Run.save\ and\ &compile                         <C-C>:call NQC_SaveCompile ()<CR>
amenu  NQC-&Run.-SEP1-                                      :
if g:NQC_Target=="RCX" ||  g:NQC_Target=="RCX2"
  amenu  NQC-&Run.download\ program\ 1\ to\ RCX             <C-C>:call NQC_CompDown (1)<CR>
  amenu  NQC-&Run.download\ program\ 2\ to\ RCX             <C-C>:call NQC_CompDown (2)<CR>
  amenu  NQC-&Run.download\ program\ 3\ to\ RCX             <C-C>:call NQC_CompDown (3)<CR>
  amenu  NQC-&Run.download\ program\ 4\ to\ RCX             <C-C>:call NQC_CompDown (4)<CR>
  amenu  NQC-&Run.download\ program\ 5\ to\ RCX             <C-C>:call NQC_CompDown (5)<CR>
  amenu  NQC-&Run.-SEP2-                                    :
  amenu  NQC-&Run.download\ program\ 1\ to\ RCX\ and\ Run   <C-C>:call NQC_CompDownRun (1)<CR>
  amenu  NQC-&Run.download\ program\ 2\ to\ RCX\ and\ Run   <C-C>:call NQC_CompDownRun (2)<CR>
  amenu  NQC-&Run.download\ program\ 3\ to\ RCX\ and\ Run   <C-C>:call NQC_CompDownRun (3)<CR>
  amenu  NQC-&Run.download\ program\ 4\ to\ RCX\ and\ Run   <C-C>:call NQC_CompDownRun (4)<CR>
  amenu  NQC-&Run.download\ program\ 5\ to\ RCX\ and\ Run   <C-C>:call NQC_CompDownRun (5)<CR>
  amenu  NQC-&Run.-SEP3-                                    :
  amenu  NQC-&Run.download\ firmware\ to\ RCX               <C-C>:call NQC_DLoadFirmware()<CR>
  amenu  NQC-&Run.upload\ datalog\ into\ buffer             <C-C>:call NQC_DatalogUpload()<CR>
  amenu  NQC-&Run.erase\ programs\ and\ datalogs\ from\ RCX <C-C>:call NQC_DatalogClear()<CR>
endif
if g:NQC_Target=="Scout"
  amenu  NQC-&Run.download\ program\ to\ Scout              <C-C>call NQC_CompDownRun(0)<CR>
endif
if g:NQC_Target=="CM"
  amenu  NQC-&Run.download\ program\ to\ CyberMaster        <C-C>call NQC_CompDownRun(0)<CR>
endif
"
endfunction			" function NQC_InitMenu
"
"===============================================================================================
"----- vim Functions ---------------------------------------------------------------------------
"===============================================================================================
"
"------------------------------------------------------------------------------
"  NQC-Comments : Frame Comment
"------------------------------------------------------------------------------
function! NQC_CommentFrame ()
  let @z=   "//----------------------------------------------------------------------\n"
  let @z=@z."//  \n"
  let @z=@z."//----------------------------------------------------------------------\n"
  put z
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Comments : Function  Comment
"------------------------------------------------------------------------------
function! NQC_CommentFunction ()
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
  let @z= @z."//   Author:  ".g:NQC_AuthorName."\n"
  let @z= @z."//     Date:  ".strftime("%x - %X")."\n"
  let @z= @z."//=====================================================================================\n"
  put z
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Comments : File Prologue
"------------------------------------------------------------------------------
function! NQC_CommentFilePrologue ()

    let File = expand("%:t")                  " name of the file in the current buffer
    let @z=    "//=====================================================================================\n"
    let @z= @z."//\n"
    let @z= @z."//       Filename:\t".File."\n"
    let @z= @z."//    Description:\t\n"
    let @z= @z."//\n"
    let @z= @z."//       Compiler:\tnqc\n"
    let @z= @z."//         Author:\t".g:NQC_AuthorName."\n"
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
function! NQC_CommentDateTimeAuthor ()
  put = '//% '.strftime(\"%x - %X\").' ('.g:NQC_AuthorRef.')'
endfunction
"
function! NQC_CommentDateTime ()
  put = strftime(\"%x - %X\")
endfunction
"
function! NQC_CommentDate ()
  put = strftime(\"%x\")
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Statements : do-while
"------------------------------------------------------------------------------
"
function! NQC_DoWhile ()
  let @z=    "do\n{\n\t\n}\nwhile (  );"
  let @z= @z."\t\t\t\t// -----  end do-while  -----\n"
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Statements : switch
"  NQC-Statements : case
"------------------------------------------------------------------------------
"
let NQC_CaseStatement = "case :\t\n\tbreak;\n\n"
"
function! NQC_CodeSwitch ()
  let @z= "switch (  )\n{\n\n"

  let loopcount=4                   " default number of cases
  while loopcount>0
    let @z= @z.g:NQC_CaseStatement
    let loopcount=loopcount-1
  endwhile

  let @z= @z."\tdefault:\t\n\t\tbreak;\n}"
  let @z= @z."\t\t\t\t// -----  end switch  -----\n"
endfunction
"
function! NQC_CodeCase ()
    let @z= g:NQC_CaseStatement
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Statements : #ifndef
"------------------------------------------------------------------------------
function! NQC_CodeIfndef (...)
	let defaultcond	= toupper(expand("%:r"))."_INC"
	let	identifier=inputdialog("(uppercase) condition for #ifndef", defaultcond )
	if identifier != ""
		let @z=    "#ifndef  ".identifier."\n"
		let @z= @z."#define  ".identifier."\n\n\n"
		let @z= @z."#endif   // ----- #ifndef ".identifier."  -----\n"
		put z
	endif
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Statements : task
"------------------------------------------------------------------------------
function! NQC_CodeTask ()
  let identifier=inputdialog("task name", "main" )
  if identifier==""
    let identifier = "main"
  endif
  let @z=    "task\n".identifier."\t(  )\n{\n\n\n\treturn ;\n}"
  let @z= @z."\t\t\t\t// ----------  end of task ".identifier."  ----------"
    put z
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Statements : inline function
"------------------------------------------------------------------------------
function! NQC_CodeInlineFunction ()
  let identifier=inputdialog("inline function name", "func")
  if identifier==""
    let identifier = "func"
  endif
  let @z=    "void\n".identifier."\t(  )\n{\n\n\n\treturn ;\n}"
  let @z= @z."\t\t\t\t// ----------  end of inline function ".identifier."  ----------"
    put z
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Statements : subroutine
"------------------------------------------------------------------------------
function! NQC_CodeSubroutine ()
  let identifier=inputdialog("subroutine name", "subr")
  if identifier==""
    let identifier = "subr"
  endif
  let @z=    "sub\n".identifier."\t(  )\n{\n\n\n\treturn ;\n}"
  let @z= @z."\t\t\t\t// ----------  end of subroutine ".identifier."  ----------"
    put z
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Statements : acquire / monitor / catch
"------------------------------------------------------------------------------
function! NQC_Acquire ()
  let @z=    "acquire ()\n{\n\t\n}\t\t\t// -----  end acquire  -----\n"
  let @z= @z."catch\n{\n\t\n}\t\t\t// -----  end catch  -----\n"
  put z
endfunction
"
function! NQC_Monitor ()
  let @z=    "monitor ()\n{\n\t\n}\t\t\t// -----  end monitor  -----\n"
  let @z= @z."catch\n{\n\t\n}\t\t\t// -----  end catch  -----\n"
  put z
endfunction
"
function! NQC_Catch ()
  let @z=    "catch ()\n{\n\t\n}\t\t\t// -----  end catch  -----\n"
  put z
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Statements : save and compile
"------------------------------------------------------------------------------
function! NQC_SaveCompile ()
  let @z= "update | !nqc -T".g:NQC_Target." %"
  exec @z
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Statements : compile, download
"  NQC-Statements : compile, download and run
"------------------------------------------------------------------------------
function! NQC_CompDown (n)
  let @z= "!nqc -T".g:NQC_Target." -S".g:NQC_Portname." -d -pgm ".a:n." %"
  exec @z
endfunction
"
function! NQC_CompDownRun (n)
  if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2"
    let @z= "!nqc -T".g:NQC_Target." -S".g:NQC_Portname." -d -pgm ".a:n." % -run"
  else
    let @z= "!nqc -T".g:NQC_Target." -S".g:NQC_Portname." -d %"
  endif
  exec @z
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Statements : download firmware
"------------------------------------------------------------------------------
function! NQC_DLoadFirmware ()
  let @z= "!nqc  -S".g:NQC_Portname." -firmware ".g:NQC_RCX_Firmware
  exec @z
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Statements : datalog upload / clear
"------------------------------------------------------------------------------
function! NQC_DatalogUpload ()
  put = '// -----  datalog : '.strftime(\"%x - %X\").'  -----'
  let @z= "read !nqc -S".g:NQC_Portname." -datalog 2>/dev/null"
  exec @z
endfunction
"
function! NQC_DatalogClear ()
  let @z= "!nqc -S".g:NQC_Portname." -clear"
  exec @z
endfunction
"
"------------------------------------------------------------------------------
"	 Create the load/unload entry in the GVIM tool menu, depending on 
"	 which script is already loaded
"------------------------------------------------------------------------------
"
let s:NQC_Active = -1														" state variable controlling the NQC-menus
"
function! NQC_CreateUnLoadMenuEntries ()
"
	" NQC is now active and was former inactive -> 
	" Insert Tools.Unload and remove Tools.Load Menu
	if  s:NQC_Active == 1
		aunmenu Tools.Load\ NQC\ Extensions
		amenu   &Tools.Unload\ NQC\ Extensions  	<C-C>:call NQC_Handle()<CR>
	else
		" NQC is now inactive and was former active or in initial state -1 
		if s:NQC_Active == 0
			" Remove Tools.Unload if NQC was former inactive
			aunmenu Tools.Unload\ NQC\ Extensions
		else
			" Set initial state NQC_Active=-1 to inactive state NQC_Active=0
			" This protects from removing Tools.Unload during initialization after
			" loading this script
			let s:NQC_Active = 0
			" Insert Tools.Load
		endif
		amenu &Tools.Load\ NQC\ Extensions <C-C>:call NQC_Handle()<CR>
	endif
	"
"
endfunction
"
"------------------------------------------------------------------------------
"  Loads or unloads NQC extensions menus
"------------------------------------------------------------------------------
function! NQC_Handle ()
	if s:NQC_Active == 0
		:call NQC_InitMenu()
		let s:NQC_Active = 1
	else
		aunmenu NQC-Comments
		aunmenu NQC-Statements
		aunmenu API-Functions
		aunmenu API-Constants
		aunmenu NQC-Run
		let s:NQC_Active = 0
	endif
	
	call NQC_CreateUnLoadMenuEntries ()
endfunction
"
"------------------------------------------------------------------------------
" 
call NQC_CreateUnLoadMenuEntries()			" create the menu entry in the GVIM tool menu
"
"call NQC_Handle()												" show menu 
"
"=====================================================================================
