"###############################################################################################
"
"     Filename:  nqc.vim
"
"  Description:  gvim-menus for NQC (Not Quite C),  Version 2.3r1 or newer.
"
"                NQC stands for Not Quite C, and is a C-like language for programmimg
"                several LEGO MINDSTORMS products:  RCX, CyberMaster, Scout and RCX2.
"                The language is described in:
"                  NQC Programmers's Guide, Version 2.3r1, by Dave Baum
"                  ( http://www.baumfamily.org/nqc/ )
"
"                nqc.vim turns gvim into an IDE for NQC programming:
"
"    Features:   - insert various types of comments
"                - insert complete but empty statements (e.g. 'if {} else {}' )
"                - insert often used code snippets (e.g. function definition, #ifndef #def #endif, ... )
"                - insert API-constants and calls for API-functions
"                - read, write, maintain your own code snippets in a separate directory
"                - download and start programs
"                - download firmware
"                - upload the RCX datalog into a new buffer
"                - show or print datalog plot (gnuplot)
"                - erase programs and datalogs
"                - configurable for RCX, RCX2, CyberMaster, Scout
"
"       Author:  Dr.-Ing. Fritz Mehner
"        Email:  mehner@fh-swh.de
"
"        Usage:  (1  ) Configure  nqc.vim 
"                      There are some technical and personal details which should be configured
"                      (see section Configuration  below; use my configuration as an example).
"
"                (2.1) Load  nqc.vim  manually into VIM with the 'so' command:
"                      :so <any directory>/nqc.vim
"
"                      or better
"
"                (2.2) Load nqc.vim on startup (VIM version 6.0 and higher) :
"                      - move this file to the directory ~/.vim/plugin/
"                
"                You will find the menu entry "Load NQC Support" in the Tools menu.
"                After loading the menus this menu entry changes to "Unload NQC Support".
"  
"         Note:  The register z is used in many places.
"
let s:NQC_Version = "1.9"              " version number of this script; do not change

"     Revision:  07.02.2003
"     
"      Created:  23.10.2002
"
"    Copyright:  Copyright (C) 2002-2003 Dr.-Ing. Fritz Mehner  (mehner@fh-swf.de)
"
"                This program is free software; you can redistribute it and/or modify
"                it under the terms of the GNU General Public License as published by
"                the Free Software Foundation; either version 2 of the License, or
"                (at your option) any later version.
"
"                This program is distributed in the hope that it will be useful,
"                but WITHOUT ANY WARRANTY; without even the implied warranty of
"                MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"                GNU General Public License for more details.
"
"                You should have received a copy of the GNU General Public License
"                along with this program; if not, write to the Free Software
"                Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
"  
"###############################################################################################
"               
"               Configuration     (Use my configuration as an example)
"
"-----------------------------------------------------------------------------------------------
"
"  Personalization  (full name, email, ... ; used in comments) :
"
let s:NQC_AuthorName     = "Fritz Mehner"
let s:NQC_AuthorRef      = "Mn"
"
"  RCX-Firmware (RCX 1.5, RCX 2.0, ... ); full path and filename :
"
let NQC_RCX_Firmware     = $HOME."/bin/firm0328.lgo"
"
"  Choose a target :
"
"    RCX   :  Mindstorms with RCX
"    RCX2  :  Mindstorms with RCX 2
"    Scout :  Scout
"    CM    :  Cybermaster
"
let NQC_Target           = "RCX2"
"
"  Specify serial port, case matters (file permission 777) :
"  /dev/ttyS0  =  COM1
"  /dev/ttyS1  =  COM2
"
let NQC_Portname         = "/dev/ttyS0"
"
"
let s:NQC_ShowMenues     = "no"							" show menues immediately after loading (yes/no)
"                                           
"
let s:NQC_Printer        = "lpr"						" printer command
"                                           
"
" The menu entries for code snippet support will not appear 
" if the following string is empty 

let s:NQC_CodeSnippets   = $HOME."/.vim/codesnippets-nqc"   " NQC code snippets
"                                       
let	s:Tools_menu_name    = "Tools"          " this variable contains the name of the Tools-menu
                                            " if the original VIM-menus are translated;
												    		            " for german menus : "Werkzeuge"
"
"-----------------------------------------------------------------------------------------------
"  End of Configuration Section 
"###############################################################################################
"
function!	NQC_InitMenu ()
"
"===============================================================================================
"----- Menu : Key Mappings ---------------------------------------------------------------------
"===============================================================================================
"  The following key mappings are for convenience only. 
"  Comment out the mappings if you dislike them.
"  If enabled, there may be conflicts with predefined key bindings of your window manager.
"-----------------------------------------------------------------------------------------------
"       F2   update/save file without confirmation
"       F3   file open dialog
"       F9   save and compile 
"
noremap  <F2>    :update<CR>
noremap  <F3>    :browse confirm e<CR>
noremap  <F9>    :call NQC_SaveCompile()<CR>
"
inoremap  <F2>    <Esc>:update<CR>
inoremap  <F3>    <Esc>:browse confirm e<CR>
inoremap  <F9>    <Esc>:call NQC_SaveCompile()<CR>
"
"
"----- for developement only -------------------------------------------------------------------
"
   noremap   <F12>       :write<CR><Esc>:so %<CR><Esc>:call NQC_Handle()<CR><Esc>:call NQC_Handle()<CR><Esc>:call NQC_Handle()<CR>
  inoremap   <F12>  <Esc>:write<CR><Esc>:so %<CR><Esc>:call NQC_Handle()<CR><Esc>:call NQC_Handle()<CR><Esc>:call NQC_Handle()<CR>
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
amenu  NQC-&Comments.&Line\ End\ Comment               <Esc><Esc>A<Tab><Tab><Tab>// 
amenu  NQC-&Comments.&Frame\ Comment                   <Esc><Esc>:call NQC_CommentFrame()          <CR>jA
amenu  NQC-&Comments.task\/function\/sub\ &Description <Esc><Esc>:call NQC_CommentFunction()       <CR>:/NAME<CR>A
amenu  NQC-&Comments.File\ &Prologue                   <Esc><Esc>:call NQC_CommentFilePrologue()   <CR>:/DESCRIPTION<CR>A
amenu  NQC-&Comments.-SEP1-                            :
amenu  NQC-&Comments.\/\/\ Date\ Time\ &Author         <Esc><Esc>$<Esc>:call NQC_CommentDateTimeAuthor() <CR>kJA
vmenu  NQC-&Comments.&code->comment                    <Esc><Esc>:'<,'>s/^/\/\//<CR><Esc>:nohlsearch<CR>
vmenu  NQC-&Comments.c&omment->code                    <Esc><Esc>:'<,'>s/^\/\///<CR><Esc>:nohlsearch<CR>
amenu  NQC-&Comments.-SEP2-                            :
amenu  NQC-&Comments.&Date                             <Esc><Esc>:let @z=strftime("%x")     <CR>"zpa
amenu  NQC-&Comments.Date\ &Time                       <Esc><Esc>:let @z=strftime("%x - %X")<CR>"zpa
"
"===============================================================================================
"----- Menu : NQC-Statements -------------------------------------------------------------------
"===============================================================================================
"      Inserting at the end of a line preserves indentation.
"-----------------------------------------------------------------------------------------------
"
imenu  NQC-St&atements.&if                         <Esc>:let @z="if (  )\n\t\n"                       <CR>"z]p<Esc>f(la
imenu  NQC-St&atements.if\ &else                   <Esc>:let @z="if (  )\n\t\nelse\n\t\n"             <CR>"z]p<Esc>f(la
imenu  NQC-St&atements.i&f\ \{\ \}                 <Esc>:let @z="if (  )\n{\n\t\n}\n"                 <CR>"z]p<Esc>f(la
imenu  NQC-St&atements.if\ \{\ \}\ e&lse\ \{\ \}   <Esc>:let @z="if (  )\n{\n\t\n}\nelse\n{\n\t\n}\n" <CR>"z]p<Esc>f(la
imenu  NQC-St&atements.f&or                        <Esc>:let @z="for ( ; ;  )\n"                      <CR>"z]p<Esc>f;i
imenu  NQC-St&atements.fo&r\ \{\ \}                <Esc>:let @z="for ( ; ;  )\n{\n\t\n}\n"            <CR>"z]p<Esc>f;i
"	
imenu  NQC-St&atements.&while\ \{\ \}              <Esc>:let @z="while (  )\n{\n\t\n}\n"              <CR>"z]p<Esc>f(la
imenu  NQC-St&atements.&do\ \{\ \}\ while          <Esc>:call NQC_DoWhile()                           <CR>"z]p<Esc>:/while <CR>f(la
imenu  NQC-St&atements.re&peat\ \{\ \}             <Esc>:let @z="repeat (  )\n{\n\t\n}\n"             <CR>"z]p<Esc>f(la
imenu  NQC-St&atements.&until                      <Esc>:let @z="until (  );\n"                       <CR>"z]p<Esc>f(la
imenu  NQC-St&atements.&switch                     <Esc>:call NQC_CodeSwitch()                        <CR>"z]p<Esc>f(la
imenu  NQC-St&atements.&case                       <Esc>:call NQC_CodeCase()                          <CR>"z]p<Esc>f:i
imenu  NQC-St&atements.brea&k                      <Esc>:let @z="break;\n"                            <CR>"z]p<Esc>A
imenu  NQC-St&atements.continue                    <Esc>:let @z="continue;\n"                         <CR>"z]p<Esc>A
imenu  NQC-St&atements.st&art                      <Esc>:let @z="start\t;\n"                          <CR>"z]p<Esc>f;i
imenu  NQC-St&atements.-SEP1-                      :
imenu  NQC-St&atements.&task                       <Esc>:call NQC_CodeTask()<CR>
imenu  NQC-St&atements.function                    <Esc>:call NQC_CodeInlineFunction()<CR>
imenu  NQC-St&atements.su&broutine                 <Esc>:call NQC_CodeSubroutine()<CR>
imenu  NQC-St&atements.-SEP2-                      :
imenu  NQC-St&atements.#include\ \"\.\.\.\"        <Esc>:let @z="#include\t\".nqh\""                          <CR>"zp<Esc>F.i
imenu  NQC-St&atements.&#define                    <Esc>:let @z="#define\t\t\t\t// "                          <CR>"zp<Esc>4F<Tab>a
imenu  NQC-St&atements.#ifndef\.\.#def\.\.#endif   <Esc>:call NQC_CodeIfndef()<CR>
imenu  NQC-St&atements.#ifdef\.\.#endif            #ifdef<Tab><CR><CR><CR>#endif<Esc>3kA
if g:NQC_Target=="RCX2" || g:NQC_Target=="Scout"
  imenu  NQC-St&atements.-SEP3-                      :
  imenu  NQC-St&atements.ac&quire                    <Esc>:call NQC_Acquire()<CR>f(a
  imenu  NQC-St&atements.&monitor                    <Esc>:call NQC_Monitor()<CR>f(a
  imenu  NQC-St&atements.catc&h\ \(\ \)              <Esc>:call NQC_Catch()<CR>f(a
endif
if s:NQC_CodeSnippets != ""
	imenu NQC-St&atements.-SEP6-                         :
	amenu  NQC-St&atements.read\ code\ snippet        <C-C>:call NQC_CodeSnippet("r")<CR>
	amenu  NQC-St&atements.write\ code\ snippet       <C-C>:call NQC_CodeSnippet("w")<CR>
	vmenu  NQC-St&atements.write\ code\ snippet       <C-C>:call NQC_CodeSnippet("wv")<CR>
	amenu  NQC-St&atements.edit\ code\ snippet        <C-C>:call NQC_CodeSnippet("e")<CR>
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
"----- Menu : Datalog  -------------------------------------------------------------------------
"===============================================================================================
"
  amenu  &Datalog.&upload\ datalog\ into\ buffer             <C-C>:call NQC_DatalogUpload()<CR>
  amenu  &Datalog.-SEP1-                                     :
  amenu  &Datalog.show\ y-&plot\                             <C-C>:call NQC_DatalogPlot(1)<CR>
  amenu  &Datalog.show\ &x-y-plot\                           <C-C>:call NQC_DatalogPlot(2)<CR>
  amenu  &Datalog.&save\ plot\ (<filenam>\.ps)               <C-C>:call NQC_DatalogPlot(10)<CR>
  amenu  &Datalog.p&rint\ plot\                              <C-C>:call NQC_DatalogPlot(11)<CR>
  amenu  &Datalog.plot\ &type.&impulses                      <C-C>:call NQC_DatalogPlotType("impulses")<CR>
  amenu  &Datalog.plot\ &type.&lines                         <C-C>:call NQC_DatalogPlotType("lines")<CR>
  amenu  &Datalog.plot\ &type.l&ines+points                  <C-C>:call NQC_DatalogPlotType("linespoints")<CR>
  amenu  &Datalog.plot\ &type.&points                        <C-C>:call NQC_DatalogPlotType("points")<CR>
  amenu  &Datalog.plot\ &type.&steps                         <C-C>:call NQC_DatalogPlotType("steps")<CR>
  amenu  &Datalog.plot\ t&itle                               <C-C>:call NQC_DatalogPlotTitle()<CR>
  amenu  &Datalog.-SEP2-                                     :
  amenu  &Datalog.&erase\ programs\ and\ datalogs\ from\ RCX <C-C>:call NQC_DatalogClear()<CR>
"
"===============================================================================================
"----- Menu : NQC-Run  -------------------------------------------------------------------------
"===============================================================================================
"
amenu  NQC-&Run.save\ and\ &compile\ \<F9\>                 <C-C>:call NQC_SaveCompile ()<CR>
amenu  NQC-&Run.-SEP1-                                      :
if g:NQC_Target=="RCX" ||  g:NQC_Target=="RCX2"
  amenu  NQC-&Run.download\ program\ &1\ to\ RCX             <C-C>:call NQC_CompDown (1)<CR>
  amenu  NQC-&Run.download\ program\ &2\ to\ RCX             <C-C>:call NQC_CompDown (2)<CR>
  amenu  NQC-&Run.download\ program\ &3\ to\ RCX             <C-C>:call NQC_CompDown (3)<CR>
  amenu  NQC-&Run.download\ program\ &4\ to\ RCX             <C-C>:call NQC_CompDown (4)<CR>
  amenu  NQC-&Run.download\ program\ &5\ to\ RCX             <C-C>:call NQC_CompDown (5)<CR>
  amenu  NQC-&Run.-SEP2-                                    :
  amenu  NQC-&Run.download\ program\ 1\ to\ RCX\ and\ Run   <C-C>:call NQC_CompDownRun (1)<CR>
  amenu  NQC-&Run.download\ program\ 2\ to\ RCX\ and\ Run   <C-C>:call NQC_CompDownRun (2)<CR>
  amenu  NQC-&Run.download\ program\ 3\ to\ RCX\ and\ Run   <C-C>:call NQC_CompDownRun (3)<CR>
  amenu  NQC-&Run.download\ program\ 4\ to\ RCX\ and\ Run   <C-C>:call NQC_CompDownRun (4)<CR>
  amenu  NQC-&Run.download\ program\ 5\ to\ RCX\ and\ Run   <C-C>:call NQC_CompDownRun (5)<CR>
  amenu  NQC-&Run.-SEP3-                                    :
  amenu  NQC-&Run.&run\ current\ program                     <C-C>:call NQC_RunCurrent ()<CR>
	imenu  NQC-&Run.-SEP4-                                    :
	amenu  NQC-&Run.&hardcopy\ buffer\ to\ FILENAME\.ps       <C-C>:call Hardcopy("n")<CR>
	vmenu  NQC-&Run.hard&copy\ highlighted\ part\ to\ FILENAME\.part\.ps   <C-C>:call Hardcopy("v")<CR>
  amenu  NQC-&Run.-SEP5-                                    :
  amenu  NQC-&Run.&erase\ programs\ and\ datalogs\ from\ RCX <C-C>:call NQC_DatalogClear()<CR>
  amenu  NQC-&Run.download\ &firmware\ to\ RCX               <C-C>:call NQC_DLoadFirmware("normal")<CR>
endif
if g:NQC_Target=="Scout"
  amenu  NQC-&Run.&download\ program\ to\ Scout              <C-C>call NQC_CompDownRun(0)<CR>
endif
if g:NQC_Target=="CM"
  amenu  NQC-&Run.&download\ program\ to\ CyberMaster        <C-C>call NQC_CompDownRun(0)<CR>
endif
	imenu  NQC-&Run.-SEP6-                                :
	amenu  NQC-&Run.&settings                                  <C-C>:call NQC_Settings()<CR>

	amenu  NQC-&Run.&about\ nqc\.vim                           <C-C>:call NQC_Version()<CR>
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
  let @z= @z."//        NAME:  \n"
  let @z= @z."//\n"
  let @z= @z."// DESCRIPTION:  \n"
  let @z= @z."//\n"
  let @z= @z."//- PARAMETER -------------------------------------------------------------------------\n"
  let @z= @z."//     Mode   Type            Name            Description\n"
  let @z= @z."//-------------------------------------------------------------------------------------\n"
  let @z= @z."//       in:  \n"
  let @z= @z."//   in-out:  \n"
  let @z= @z."//      out:  \n"
  let @z= @z."//-------------------------------------------------------------------------------------\n"
  let @z= @z."//   AUTHOR:  ".s:NQC_AuthorName."\n"
  let @z= @z."//  CREATED:  ".strftime("%x - %X")."\n"
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
    let @z= @z."//       FILENAME:\t".File."\n"
    let @z= @z."//    DESCRIPTION:\t\n"
    let @z= @z."//\n"
    let @z= @z."//       COMPILER:\tnqc\n"
    let @z= @z."//         AUTHOR:\t".s:NQC_AuthorName."\n"
    let @z= @z."//        CREATED:\t".strftime("%x - %X")."\n"
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
  put = '//% '.strftime(\"%x - %X\").' ('.s:NQC_AuthorRef.')'
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
"  NQC-Statements : function
"------------------------------------------------------------------------------
function! NQC_CodeInlineFunction ()
  let identifier=inputdialog("function name", "func")
  if identifier==""
    let identifier = "func"
  endif
  let @z=    "void\n".identifier."\t(  )\n{\n\n\n\treturn ;\n}"
  let @z= @z."\t\t\t\t// ----------  end of function ".identifier."  ----------"
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
"  NQC-Statements : run current program
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
function! NQC_RunCurrent ()
  if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2"
    let @z= "!nqc -T".g:NQC_Target." -S".g:NQC_Portname." -run"
  endif
  exec @z
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Statements : download firmware
"------------------------------------------------------------------------------
function! NQC_DLoadFirmware (n)
	if a:n=="fast"
		let @z= "!nqc  -S".g:NQC_Portname." -near -firmfast ".g:NQC_RCX_Firmware
	else
		let @z= "!nqc  -S".g:NQC_Portname." -firmware ".g:NQC_RCX_Firmware
	endif
	exec @z
endfunction
"
"------------------------------------------------------------------------------
"  Datalog : variables
"------------------------------------------------------------------------------
let s:NQC_Plot_Type				= "steps"						" Gnuplot style parameter
let s:NQC_Plot_Dataformat	= "x"								" x-plot / x-y-plot
let s:NQC_Plot_Title			= "RCX-Datalog"			" Gnuplot title

"------------------------------------------------------------------------------
"  Datalog : set plot style
"------------------------------------------------------------------------------
function! NQC_DatalogPlotType(type)
	let s:NQC_Plot_Type=a:type
endfunction
"
"------------------------------------------------------------------------------
"  Datalog : set plot title
"------------------------------------------------------------------------------
function! NQC_DatalogPlotTitle()
	let s:NQC_Plot_Title	= inputdialog("Plot Title",  s:NQC_Plot_Title )
endfunction

"------------------------------------------------------------------------------
"  Datalog : datalog upload 
"  1. line initially empty; will be deleted
"------------------------------------------------------------------------------
function! NQC_DatalogUpload ()

	let	filename=browse(0,"open a new datalaog file", ".", "datalog" )
	if filename != ""

		if filereadable(filename)
			exe ":!rm ".filename."  2>/dev/null"  
		endif

		exe ":25vnew ".filename
		exe ":read !nqc -S".g:NQC_Portname." -datalog 2>/dev/null"
		set noswapfile
		exe ":1,1d"

	endif

endfunction
"
"------------------------------------------------------------------------------
"  Datalog : datalog plot
"  n =  1 : show plot      (gnuplot, terminal X11)
"  n =  2 : show x-y-plot  (gnuplot, terminal X11)
"  n = 10 : make hardcopy  (gnuplot, terminal postscript)
"  n = 11 : print plot     (gnuplot, terminal postscript)
"------------------------------------------------------------------------------
function! NQC_DatalogPlot (n)

	if system("which gnuplot 2>/dev/null")==""
		echo "**  program gnuplot is not available  **"
		return
	endif

	let	Sou				= expand("%:p")					" name of the file in the current buffer
	let	tempfile	= tempname()
	let	timestamp	= Sou.'  *  '.strftime("%x - %X").'  *  '.s:NQC_AuthorName
	:execute  "silent :!echo \"set grid \" > ".tempfile
	if a:n==10 || a:n==11
		:execute  "silent :!echo \"set terminal postscript\" >> ".tempfile
	endif
	:execute  "silent:!echo \"set nokey \" >> ".tempfile
	:execute  "silent:!echo \"set timestamp \'".timestamp."\'\" >> ".tempfile
	:execute  "silent:!echo \"set title \'".s:NQC_Plot_Title."\'\" >> ".tempfile
	:execute  "silent:!echo \"plot '-' with ".s:NQC_Plot_Type."\" >> ".tempfile
	"
	"---------- y-plot ------------------------------------------
	if a:n==1
		let s:NQC_Plot_Dataformat	= "x"
		:execute  "silent:!cat % >> ".tempfile
		:execute  "silent:!gnuplot -persist ".tempfile." &"
		:echo "**  quit gnuplot with 'q'  **"
	endif
	"
	"---------- x-y-plot ----------------------------------------
	if a:n==2
		let s:NQC_Plot_Dataformat	= "xy"
		:%s/\(.\+\)\n\(.\+\)/\1 \2/								 " group 2 lines in a x-y-pair
		:execute  "silent:!cat % >> ".tempfile
		:u
		:execute  "silent:!gnuplot -persist ".tempfile." &"
		:echo "**  quit gnuplot with 'q'  **"
	endif
	"
	"---------- generate postscript -----------------------------
	if a:n==10
		if s:NQC_Plot_Dataformat=="xy"
			:%s/\(.\+\)\n\(.\+\)/\1 \2/								 " group 2 lines in a x-y-pair
		endif
		:execute  "silent:!cat % >> ".tempfile
		:execute  "silent:!gnuplot  ".tempfile." > ".Sou.".ps &"
		if s:NQC_Plot_Dataformat=="xy"
			:u
		endif
	endif
	"
	"---------- print postscript --------------------------------
	if a:n==11
		if s:NQC_Plot_Dataformat=="xy"
			:%s/\(.\+\)\n\(.\+\)/\1 \2/								 " group 2 lines in a x-y-pair
		endif
		:execute  "silent:!cat % >> ".tempfile
		:execute  "silent:!gnuplot  ".tempfile." | ".s:NQC_Printer." &"
		if s:NQC_Plot_Dataformat=="xy"
			:u
		endif
	endif

endfunction
"
"------------------------------------------------------------------------------
"  Datalog : erase programs / clear datalog 
"------------------------------------------------------------------------------
function! NQC_DatalogClear ()
  let @z= "!nqc -S".g:NQC_Portname." -clear"
  exec @z
endfunction
"
"
"------------------------------------------------------------------------------
"  NQC-Statements : read / edit code snippet
"------------------------------------------------------------------------------
function! NQC_CodeSnippet(arg1)
	if isdirectory(s:NQC_CodeSnippets)
		"
		" read snippet file, put content below current line
		" 
		if a:arg1 == "r"
			let	l:snippetfile=browse(0,"read a code snippet",s:NQC_CodeSnippets,"")
			if l:snippetfile != ""
				:execute "read ".l:snippetfile
			endif
		endif
		"
		" update current buffer / split window / edit snippet file
		" 
		if a:arg1 == "e"
			let	l:snippetfile=browse(0,"edit a code snippet",s:NQC_CodeSnippets,"")
			if l:snippetfile != ""
				:execute "update! | split | edit ".l:snippetfile
			endif
		endif
		"
		" write whole buffer into snippet file 
		" 
		if a:arg1 == "w"
			let	l:snippetfile=browse(0,"write a code snippet",s:NQC_CodeSnippets,"")
			if l:snippetfile != ""
				:execute ":write! ".l:snippetfile
			endif
		endif
		"
		" write marked area into snippet file 
		" 
		if a:arg1 == "wv"
			let	l:snippetfile=browse(0,"write a code snippet",s:NQC_CodeSnippets,"")
			if l:snippetfile != ""
				:execute ":*write! ".l:snippetfile
			endif
		endif

	else
		echohl ErrorMsg
		echo "code snippet directory ".s:NQC_CodeSnippets." does not exist"
		echohl None
	endif
endfunction
"
"------------------------------------------------------------------------------
"  run : hardcopy
"------------------------------------------------------------------------------
function! Hardcopy (arg1)
	let	Sou		= expand("%")								" name of the file in the current buffer
	" ----- normal mode ----------------
	if a:arg1=="n"
		exe	"hardcopy > ".Sou.".ps"		
	endif
	" ----- visual mode ----------------
	if a:arg1=="v"
		exe	"*hardcopy > ".Sou.".part.ps"		
	endif
endfunction
"
"------------------------------------------------------------------------------
"  run : settings
"------------------------------------------------------------------------------
function! NQC_Settings ()
	let settings =          "nqc.vim settings:\n"
	let settings = settings."\n"
	let settings = settings."target :  ".g:NQC_Target."\n"
	let settings = settings."serial port :  ".g:NQC_Portname."\n"
	let settings = settings."firmware :  ".g:NQC_RCX_Firmware."\n"
	let settings = settings."\nhot keys:        \n"
	let settings = settings."F2  :  update (save) file      \n"
	let settings = settings."F3  :  file open dialog        \n"
  let settings = settings."F9  :  save and compile buffer\n"
	let settings = settings."\nMake changes in file \"".expand("%:p")."\"\n"
	let dummy=confirm( settings, "ok", 1, "Info" )
endfunction
"
"------------------------------------------------------------------------------
"  run : about
"------------------------------------------------------------------------------
function! NQC_Version ()
	let dummy=confirm("NQC-Support, Version ".s:NQC_Version."\nDr. Fritz Mehner\nmehner@fh-swf.de", "ok" )
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
	" protect the following submenu names against interpolation by using single qoutes (Mn)
	"
	if  s:NQC_Active == 1
		exe "aunmenu ".s:Tools_menu_name.'.Load\ NQC\ Support'
		exe "amenu   &".s:Tools_menu_name.'.Unload\ NQC\ Support  	<C-C>:call NQC_Handle()<CR>'
	else
		" NQC is now inactive and was former active or in initial state -1 
		if s:NQC_Active == 0
			" Remove Tools.Unload if NQC was former inactive
			exe "aunmenu ".s:Tools_menu_name.'.Unload\ NQC\ Support'
		else
			" Set initial state NQC_Active=-1 to inactive state NQC_Active=0
			" This protects from removing Tools.Unload during initialization after
			" loading this script
			let s:NQC_Active = 0
			" Insert Tools.Load
		endif
		exe "amenu &".s:Tools_menu_name.'.Load\ NQC\ Support <C-C>:call NQC_Handle()<CR>'
	endif
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
		aunmenu Datalog
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
if s:NQC_ShowMenues == "yes"
	call NQC_Handle()											" load the menus
endif
"
"=====================================================================================
