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
let s:NQC_Version = "2.0"              " version number of this script; do not change

"     Revision:  26.04.2003
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
"       F9   save and compile 
"
noremap  <F9>    :call NQC_SaveCompile()<CR>
"
inoremap  <F9>    <Esc>:call NQC_SaveCompile()<CR>
"
"
"----- for developement only -------------------------------------------------------------------
"
"   noremap   <F12>       :write<CR><Esc>:so %<CR><Esc>:call NQC_Handle()<CR><Esc>:call NQC_Handle()<CR><Esc>:call NQC_Handle()<CR>
"  inoremap   <F12>  <Esc>:write<CR><Esc>:so %<CR><Esc>:call NQC_Handle()<CR><Esc>:call NQC_Handle()<CR><Esc>:call NQC_Handle()<CR>
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
"
 menu  NQC-&Comments.&Date                      i<C-R>=strftime("%x")<CR>
imenu  NQC-&Comments.&Date                       <C-R>=strftime("%x")<CR>
 menu  NQC-&Comments.Date\ &Time                i<C-R>=strftime("%x %X %Z")<CR>
imenu  NQC-&Comments.Date\ &Time                 <C-R>=strftime("%x %X %Z")<CR>
"
"===============================================================================================
"----- Menu : NQC-Statements -------------------------------------------------------------------
"===============================================================================================
"      Inserting at the end of a line preserves indentation.
"-----------------------------------------------------------------------------------------------
"
amenu  NQC-St&atements.&if                         <Esc><Esc>oif (  )<Esc>F(la
amenu  NQC-St&atements.if\ &else                   <Esc><Esc>oif (  )<CR>else<Esc>1kf)hi
amenu  NQC-St&atements.i&f\ \{\ \}                 <Esc><Esc>oif (  )<CR>{<CR>}<Esc>2kf(la
amenu  NQC-St&atements.if\ \{\ \}\ e&lse\ \{\ \}   <Esc><Esc>oif (  )<CR>{<CR>}<CR>else<CR>{<CR>}<Esc>5kf(la
amenu  NQC-St&atements.f&or                        <Esc><Esc>ofor ( ; ;  )<Esc>2F;i
amenu  NQC-St&atements.fo&r\ \{\ \}                <Esc><Esc>ofor ( ; ;  )<CR>{<CR>}<Esc>2kf;i
"	
amenu  NQC-St&atements.&while\ \{\ \}              <Esc><Esc>owhile (  )<CR>{<CR>}<Esc>2kf(la
amenu  NQC-St&atements.&do\ \{\ \}\ while          <Esc><Esc>:call NQC_DoWhile()<CR><Esc>3jf(la
amenu  NQC-St&atements.re&peat\ \{\ \}             <Esc><Esc>orepeat (  )<CR>{<CR>}<CR><Esc>3kf(la
amenu  NQC-St&atements.&until                      <Esc><Esc>ountil (  );<Esc>F(la
amenu  NQC-St&atements.&switch                     <Esc><Esc>:call NQC_CodeSwitch()<Esc>f(la
amenu  NQC-St&atements.&case                       <Esc><Esc>ocase 0:<Tab><CR>break;<CR><Esc>2kf0s
amenu  NQC-St&atements.brea&k                      <Esc><Esc>obreak;<Esc>a
amenu  NQC-St&atements.continue                    <Esc><Esc>ocontinue;<Esc>a
amenu  NQC-St&atements.st&art                      <Esc><Esc>ostart<Tab>;<CR><Esc>kf;i
"	
amenu  NQC-St&atements.-SEP1-                      :
amenu  <silent> NQC-St&atements.&task              <Esc><Esc>:call NQC_CodeTask()<CR>3jA
amenu  <silent> NQC-St&atements.function           <Esc><Esc>:call NQC_CodeInlineFunction()<CR>3jA
amenu  <silent> NQC-St&atements.su&broutine        <Esc><Esc>:call NQC_CodeSubroutine()<CR>3jA
"	
amenu  NQC-St&atements.-SEP2-                      :
amenu  NQC-St&atements.#include\ \"\.\.\.\"        <Esc><Esc>o#include<Tab>".nqh"<Esc>F.i
amenu  NQC-St&atements.&#define                    <Esc><Esc>o#define<Tab><Tab><Tab><Tab>//<Space><Esc>4F<Tab>a
amenu  NQC-St&atements.#ifndef\.\.#def\.\.#endif   <Esc><Esc>:call NQC_CodeIfndef()<CR>2ji
amenu  NQC-St&atements.#ifdef\.\.#endif            <Esc><Esc>o#ifdef<Tab><CR><CR><CR>#endif<Esc>3kA
"	
if g:NQC_Target=="RCX2" || g:NQC_Target=="Scout"
  amenu  NQC-St&atements.-SEP3-                      :
  amenu  NQC-St&atements.ac&quire                    <Esc><Esc>:call NQC_Acquire()<CR>f(a
  amenu  NQC-St&atements.&monitor                    <Esc><Esc>:call NQC_Monitor()<CR>f(a
  amenu  NQC-St&atements.catc&h\ \(\ \)              <Esc><Esc>:call NQC_Catch()<CR>f(a
endif
if s:NQC_CodeSnippets != ""
"	
  imenu           NQC-St&atements.-SEP6-                     :
  amenu  <silent> NQC-St&atements.read\ code\ snippet        <C-C>:call NQC_CodeSnippet("r")<CR>
  amenu  <silent> NQC-St&atements.write\ code\ snippet       <C-C>:call NQC_CodeSnippet("w")<CR>
  vmenu  <silent> NQC-St&atements.write\ code\ snippet       <C-C>:call NQC_CodeSnippet("wv")<CR>
  amenu  <silent> NQC-St&atements.edit\ code\ snippet        <C-C>:call NQC_CodeSnippet("e")<CR>
endif
"
"===============================================================================================
"----- Menu : API-Functions --------------------------------------------------------------------
"===============================================================================================
"
"----- outputs ----------------------------------------------------------------------------
 menu API-Functions.outputs.Float\ (outputs)                     aFloat();<Esc>F(a
 menu API-Functions.outputs.Fwd\ (outputs)                       aFwd();<Esc>F(a
 menu API-Functions.outputs.Off\ (outputs)                       aOff();<Esc>F(a
 menu API-Functions.outputs.On\ (outputs)                        aOn();<Esc>F(a
 menu API-Functions.outputs.OnFor\ (outputs,time)                aOnFor(,);<Esc>F(a
 menu API-Functions.outputs.OnFwd\ (outputs)                     aOnFwd();<Esc>F(a
 menu API-Functions.outputs.OnRev\ (outputs)                     aOnRev();<Esc>F(a
 menu API-Functions.outputs.OutputStatus\ (n)                    aOutputStatus()<Esc>F(a
 menu API-Functions.outputs.Rev\ (outputs)                       aRev();<Esc>F(a
 menu API-Functions.outputs.SetDirection\ (outputs,dir)          aSetDirection(,);<Esc>F(a
 menu API-Functions.outputs.SetOutput\ (outputs,mode)            aSetOutput(,);<Esc>F(a
 menu API-Functions.outputs.SetPower\ (outputs,power)            aSetPower(,);<Esc>F(a
 menu API-Functions.outputs.Toggle\ (outputs)                    aToggle();<Esc>F(a
"
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
"
"----- sensor types, modes, information ---------------------------------------------------
imenu API-Functions.sensors.ClearSensor\ (sensor)                ClearSensor();<Esc>F(a
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2" || g:NQC_Target=="CM"
   menu API-Functions.sensors.SensorMode\ (n)                    aSensorMode();<Esc>F(a
  imenu API-Functions.sensors.SensorMode\ (n)                     SensorMode();<Esc>F(a
endif
 menu API-Functions.sensors.SensorType\ (n)                      aSensorType();<Esc>F(a
imenu API-Functions.sensors.SensorType\ (n)                       SensorType();<Esc>F(a
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2"
   menu API-Functions.sensors.SensorValueBool\ (n)               aSensorValueBool()<Esc>F(a
  imenu API-Functions.sensors.SensorValueBool\ (n)                SensorValueBool()<Esc>F(a
endif
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2" || g:NQC_Target=="Scout"
   menu API-Functions.sensors.SensorValueRaw\ (n)                aSensorValueRaw()<Esc>F(a
  imenu API-Functions.sensors.SensorValueRaw\ (n)                 SensorValueRaw()<Esc>F(a
endif
 menu API-Functions.sensors.SensorValue\ (n)                     aSensorValue()<Esc>F(a
imenu API-Functions.sensors.SensorValue\ (n)                      SensorValue()<Esc>F(a
if g:NQC_Target=="Scout"
   menu API-Functions.sensors.SetSensorLowerLimit\ (value)       aSetSensorLowerLimit();<Esc>F(a
   menu API-Functions.sensors.SetSensorUpperLimit\ (value)       aSetSensorUpperLimit();<Esc>F(a
   menu API-Functions.sensors.SetSensorHysteresis\ (value)       aSetSensorHysteresis();<Esc>F(a
   menu API-Functions.sensors.CalibrateSensor\ (\ )              aCalibrateSensor();
  imenu API-Functions.sensors.SetSensorLowerLimit\ (value)        SetSensorLowerLimit();<Esc>F(a
  imenu API-Functions.sensors.SetSensorUpperLimit\ (value)        SetSensorUpperLimit();<Esc>F(a
  imenu API-Functions.sensors.SetSensorHysteresis\ (value)        SetSensorHysteresis();<Esc>F(a
  imenu API-Functions.sensors.CalibrateSensor\ (\ )               CalibrateSensor();
endif
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2"
   menu API-Functions.sensors.SetSensor\ (sensor,config)         aSetSensor(,);<Esc>F(a
  imenu API-Functions.sensors.SetSensor\ (sensor,config)          SetSensor(,);<Esc>F(a
endif
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2" || g:NQC_Target=="CM"
   menu API-Functions.sensors.SetSensorMode\ (sensor,mode)      aSetSensorMode(,);<Esc>F(a
  imenu API-Functions.sensors.SetSensorMode\ (sensor,mode)       SetSensorMode(,);<Esc>F(a
endif
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2"
   menu API-Functions.sensors.SetSensorType\ (sensor,type)      aSetSensorType(,)<Esc>F(a
  imenu API-Functions.sensors.SetSensorType\ (sensor,type)       SetSensorType(,)<Esc>F(a
endif
"----- timers and counters ----------------------------------------------------------------
 menu API-Functions.timers\ counters.ClearTimer\ (n)            aClearTimer();<Esc>F(a
 menu API-Functions.timers\ counters.Timer\ (n)                 aTimer()<Esc>F(a
imenu API-Functions.timers\ counters.ClearTimer\ (n)             ClearTimer();<Esc>F(a
imenu API-Functions.timers\ counters.Timer\ (n)                  Timer()<Esc>F(a
if g:NQC_Target=="RCX2"
   menu API-Functions.timers\ counters.FastTimer\ (n)           aFastTimer()<Esc>F(a
   menu API-Functions.timers\ counters.SetTimer\ (n,value)      aSetTimer(,);<Esc>F(a
  imenu API-Functions.timers\ counters.FastTimer\ (n)            FastTimer()<Esc>F(a
  imenu API-Functions.timers\ counters.SetTimer\ (n,value)       SetTimer(,);<Esc>F(a
endif
if g:NQC_Target=="RCX2" || g:NQC_Target=="Scout"
   menu API-Functions.timers\ counters.Counter\ (n)             aCounter()<Esc>F(a
   menu API-Functions.timers\ counters.ClearCounter\ (n)        aClearCounter();<Esc>F(a
   menu API-Functions.timers\ counters.DecCounter\ (n)          aDecCounter();<Esc>F(a
   menu API-Functions.timers\ counters.IncCounter\ (n)          aIncCounter();<Esc>F(a
  imenu API-Functions.timers\ counters.Counter\ (n)              Counter()<Esc>F(a
  imenu API-Functions.timers\ counters.ClearCounter\ (n)         ClearCounter();<Esc>F(a
  imenu API-Functions.timers\ counters.DecCounter\ (n)           DecCounter();<Esc>F(a
  imenu API-Functions.timers\ counters.IncCounter\ (n)           IncCounter();<Esc>F(a
endif
"----- sounds -----------------------------------------------------------------------------
if g:NQC_Target=="RCX2"
   menu API-Functions.sounds.ClearSound\ (\n)                   aClearSound();
  imenu API-Functions.sounds.ClearSound\ (\n)                    ClearSound();
endif
 menu API-Functions.sounds.PlaySound\ (sound)                   aPlaySound();<Esc>F(a
 menu API-Functions.sounds.PlayTone\ (freq,duration)            aPlayTone(,);<Esc>F(a
imenu API-Functions.sounds.PlaySound\ (sound)                    PlaySound();<Esc>F(a
imenu API-Functions.sounds.PlayTone\ (freq,duration)             PlayTone(,);<Esc>F(a
if g:NQC_Target=="RCX2" || g:NQC_Target=="Scout"
   menu API-Functions.sounds.MuteSound\ (\n)                    aMuteSound();
   menu API-Functions.sounds.UnmuteSound\ (\n)                  aUnmuteSound();
  imenu API-Functions.sounds.MuteSound\ (\n)                     MuteSound();
  imenu API-Functions.sounds.UnmuteSound\ (\n)                   UnmuteSound();
endif
if g:NQC_Target=="Scout"
   menu API-Functions.sounds.SelectSound\ (group)               aSelectSound();<Esc>F(a
  imenu API-Functions.sounds.SelectSound\ (group)                SelectSound();<Esc>F(a
endif
"----- LCD display ------------------------------------------------------------------------
if g:NQC_Target=="RCX" ||  g:NQC_Target=="RCX2"
   menu API-Functions.display.SelectDisplay\ (mode)             aSelectDisplay();<Esc>F(a
  imenu API-Functions.display.SelectDisplay\ (mode)              SelectDisplay();<Esc>F(a
endif
if g:NQC_Target=="RCX2"
   menu API-Functions.display.SetUserDisplay\ (value,precision) aSetUserDisplay(,);<Esc>F(a
  imenu API-Functions.display.SetUserDisplay\ (value,precision)  SetUserDisplay(,);<Esc>F(a
endif
"----- messages ---------------------------------------------------------------------------
if g:NQC_Target=="RCX" ||  g:NQC_Target=="RCX2" || g:NQC_Target=="Scout"
   menu API-Functions.messages.ClearMessage\ (\ )               aClearMessage();
   menu API-Functions.messages.Message\ (\ )                    aMessage()
   menu API-Functions.messages.SendMessage\ (message)           aSendMessage();<Esc>F(a
   menu API-Functions.messages.SetTxPower\ (power)              aSetTxPower();<Esc>F(a
  imenu API-Functions.messages.ClearMessage\ (\ )                ClearMessage();
  imenu API-Functions.messages.Message\ (\ )                     Message()
  imenu API-Functions.messages.SendMessage\ (message)            SendMessage();<Esc>F(a
  imenu API-Functions.messages.SetTxPower\ (power)               SetTxPower();<Esc>F(a
endif
"----- general ----------------------------------------------------------------------------
 menu API-Functions.general.Random\ (n)                         aRandom()<Esc>F(a
 menu API-Functions.general.SetSleepTime\ (minutes)             aSetSleepTime();<Esc>F(a
imenu API-Functions.general.Random\ (n)                          Random()<Esc>F(a
imenu API-Functions.general.SetSleepTime\ (minutes)              SetSleepTime();<Esc>F(a
if g:NQC_Target=="RCX2"
   menu API-Functions.general.SetRandomSeed\ (n)                aSetRandomSeed();<Esc>F(a
  imenu API-Functions.general.SetRandomSeed\ (n)                 SetRandomSeed();<Esc>F(a
endif
 menu API-Functions.general.SleepNow\ (\ )                      aSleepNow();
 menu API-Functions.general.StopAllTasks\ (\ )                  aStopAllTasks();
 menu API-Functions.general.Wait\ (time)                        aWait();<Esc>F(a
imenu API-Functions.general.SleepNow\ (\ )                       SleepNow();
imenu API-Functions.general.StopAllTasks\ (\ )                   StopAllTasks();
imenu API-Functions.general.Wait\ (time)                         Wait();<Esc>F(a

"----- RCX features -----------------------------------------------------------------------
if g:NQC_Target=="RCX" ||  g:NQC_Target=="RCX2"
   menu API-Functions.RCX\ features.Program\ (\ )               aProgram()
   menu API-Functions.RCX\ features.SetWatch\ (hours,minutes)   aSetWatch(,);<Esc>F(a
   menu API-Functions.RCX\ features.Watch\ (\n)                 aWatch()
  imenu API-Functions.RCX\ features.Program\ (\ )                Program()
  imenu API-Functions.RCX\ features.SetWatch\ (hours,minutes)    SetWatch(,);<Esc>F(a
  imenu API-Functions.RCX\ features.Watch\ (\n)                  Watch()
endif
if g:NQC_Target=="RCX2"
   menu API-Functions.RCX\ features.BatteryLevel\ (\ )          aBatteryLevel()
   menu API-Functions.RCX\ features.FirmwareVersion\ (\ )       aFirmwareVersion()
   menu API-Functions.RCX\ features.SelectProgram\ (n)          aSelectProgram();<Esc>F(a
  imenu API-Functions.RCX\ features.BatteryLevel\ (\ )           BatteryLevel()
  imenu API-Functions.RCX\ features.FirmwareVersion\ (\ )        FirmwareVersion()
  imenu API-Functions.RCX\ features.SelectProgram\ (n)           SelectProgram();<Esc>F(a
endif
"----- SCOUT features -----------------------------------------------------------------------
if g:NQC_Target=="Scout"
   menu API-Functions.Scout\ features.EventFeedback\ (\ )                           aEventFeedback()
   menu API-Functions.Scout\ features.ScoutRules\ (n)                               aScoutRules()<Esc>F(a
   menu API-Functions.Scout\ features.SetEventFeedback\ (events)                    aSetEventFeedback();<Esc>F(a
   menu API-Functions.Scout\ features.SetLight\ (mode)                              aSetLight();<Esc>F(a
   menu API-Functions.Scout\ features.SetScoutRules\ (motion,touch,light,time,fx)   aSetScoutRules(,,,,);<Esc>F(a
   menu API-Functions.Scout\ features.SetScoutMode\ (mode)                          aSetScoutMode();<Esc>F(a
  imenu API-Functions.Scout\ features.EventFeedback\ (\ )                            EventFeedback()
  imenu API-Functions.Scout\ features.ScoutRules\ (n)                                ScoutRules()<Esc>F(a
  imenu API-Functions.Scout\ features.SetEventFeedback\ (events)                     SetEventFeedback();<Esc>F(a
  imenu API-Functions.Scout\ features.SetLight\ (mode)                               SetLight();<Esc>F(a
  imenu API-Functions.Scout\ features.SetScoutRules\ (motion,touch,light,time,fx)    SetScoutRules(,,,,);<Esc>F(a
  imenu API-Functions.Scout\ features.SetScoutMode\ (mode)                           SetScoutMode();<Esc>F(a
endif
"----- CYBERMASTER features -----------------------------------------------------------------------
if g:NQC_Target=="CM"
   menu API-Functions.cybermaster\ features.Drive\ (motor0,motor1)                  aDrive(,);<Esc>F(a
   menu API-Functions.cybermaster\ features.OnWait\ (motors,time)                   aOnWait(,);<Esc>F(a
   menu API-Functions.cybermaster\ features.OnWaitDifferent\ (motors,n0,n1,n2,time) aOnWaitDifferent(,,,,);<Esc>F(a
   menu API-Functions.cybermaster\ features.ClearTachoCounter\ (motors)             aClearTachoCounter();<Esc>F(a
   menu API-Functions.cybermaster\ features.TachoCount\ (n)                         aTachoCount()<Esc>F(a
   menu API-Functions.cybermaster\ features.TachoSpeed\ (n)                         aTachoSpeed()<Esc>F(a
   menu API-Functions.cybermaster\ features.ExternalMotorRunning\ (\n)              aExternalMotorRunning()
   menu API-Functions.cybermaster\ features.AGC\ (\ )                               aAGC()
  imenu API-Functions.cybermaster\ features.Drive\ (motor0,motor1)                   Drive(,);<Esc>F(a
  imenu API-Functions.cybermaster\ features.OnWait\ (motors,time)                    OnWait(,);<Esc>F(a
  imenu API-Functions.cybermaster\ features.OnWaitDifferent\ (motors,n0,n1,n2,time)  OnWaitDifferent(,,,,);<Esc>F(a
  imenu API-Functions.cybermaster\ features.ClearTachoCounter\ (motors)              ClearTachoCounter();<Esc>F(a
  imenu API-Functions.cybermaster\ features.TachoCount\ (n)                          TachoCount()<Esc>F(a
  imenu API-Functions.cybermaster\ features.TachoSpeed\ (n)                          TachoSpeed()<Esc>F(a
  imenu API-Functions.cybermaster\ features.ExternalMotorRunning\ (\n)               ExternalMotorRunning()
  imenu API-Functions.cybermaster\ features.AGC\ (\ )                                AGC()
endif
"----- datalog ----------------------------------------------------------------------------
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2"
   menu API-Functions.datalog.AddToDatalog\ (value)              aAddToDatalog();<Esc>F(a
   menu API-Functions.datalog.CreateDatalog\ (size)              aCreateDatalog();<Esc>F(a
   menu API-Functions.datalog.UploadDatalog\ (start,count)       aUploadDatalog(,);<Esc>F(a
  imenu API-Functions.datalog.AddToDatalog\ (value)               AddToDatalog();<Esc>F(a
  imenu API-Functions.datalog.CreateDatalog\ (size)               CreateDatalog();<Esc>F(a
  imenu API-Functions.datalog.UploadDatalog\ (start,count)        UploadDatalog(,);<Esc>F(a
endif
"----- global control  --------------------------------------------------------------------
if g:NQC_Target=="RCX2" || g:NQC_Target=="Scout"
   menu API-Functions.global\ control.SetGlobalOutput\ (outputs,mode)            aSetGlobalOutput(,);<Esc>F(a
   menu API-Functions.global\ control.SetGlobalDirection\ (outputs,mode)         aSetGlobalDirection(,);<Esc>F(a
   menu API-Functions.global\ control.SetMaxPower\ (outputs,power)               aSetMaxPower(,);<Esc>F(a
   menu API-Functions.global\ control.GlobalOutputStatus\ (n)                    aGlobalOutputStatus()<Esc>F(a
  imenu API-Functions.global\ control.SetGlobalOutput\ (outputs,mode)             SetGlobalOutput(,);<Esc>F(a
  imenu API-Functions.global\ control.SetGlobalDirection\ (outputs,mode)          SetGlobalDirection(,);<Esc>F(a
  imenu API-Functions.global\ control.SetMaxPower\ (outputs,power)                SetMaxPower(,);<Esc>F(a
  imenu API-Functions.global\ control.GlobalOutputStatus\ (n)                     GlobalOutputStatus()<Esc>F(a
endif
"----- serial  ----------------------------------------------------------------------------
if g:NQC_Target=="RCX2"
   menu API-Functions.serial.InternalMessage\ (message)          aInternalMessage();<Esc>F(a
   menu API-Functions.serial.SetSerialComm\ (settings)           aSetSerialComm();<Esc>F(a
   menu API-Functions.serial.SetSerialPacket\ (settings)         aSetSerialPacket();<Esc>F(a
   menu API-Functions.serial.SetSerialData\ (n,value)            aSetSerialData(,);<Esc>F(a
   menu API-Functions.serial.SerialComm\ (\ )                    aSerialComm()
   menu API-Functions.serial.SerialData\ (n)                     aSerialData()<Esc>F(a
   menu API-Functions.serial.SerialPacket\ (\ )                  aSerialPacket()
   menu API-Functions.serial.SendSerial\ (start,count)           aSendSerial(,);<Esc>F(a
  imenu API-Functions.serial.InternalMessage\ (message)           InternalMessage();<Esc>F(a
  imenu API-Functions.serial.SetSerialComm\ (settings)            SetSerialComm();<Esc>F(a
  imenu API-Functions.serial.SetSerialPacket\ (settings)          SetSerialPacket();<Esc>F(a
  imenu API-Functions.serial.SetSerialData\ (n,value)             SetSerialData(,);<Esc>F(a
  imenu API-Functions.serial.SerialComm\ (\ )                     SerialComm()
  imenu API-Functions.serial.SerialData\ (n)                      SerialData()<Esc>F(a
  imenu API-Functions.serial.SerialPacket\ (\ )                   SerialPacket()
  imenu API-Functions.serial.SendSerial\ (start,count)            SendSerial(,);<Esc>F(a
endif
"----- VLL  --------------------------------------------------------------------------------
if g:NQC_Target=="Scout"
   menu API-Functions.VLL.SendVLL\ (value)                      aSendVLL();<Esc>F(a
  imenu API-Functions.VLL.SendVLL\ (value)                       SendVLL();<Esc>F(a
endif
"----- access control ----------------------------------------------------------------------
if g:NQC_Target=="RCX2" || g:NQC_Target=="Scout"
   menu API-Functions.access\ control.SetPriority\ (p)          aSetPriority();<Esc>F(a
  imenu API-Functions.access\ control.SetPriority\ (p)           SetPriority();<Esc>F(a
endif
"----- RCX2 events -------------------------------------------------------------------------
if g:NQC_Target=="RCX2"
   menu API-Functions.RCX2\ events.CalibrateEvent\ (event,lower,upper,hyst)    aCalibrateEvent(,,,);<Esc>F(a
   menu API-Functions.RCX2\ events.ClearAllEvents\ (\ )                        aClearAllEvents()
   menu API-Functions.RCX2\ events.ClearEvent\ (event)                         aClearEvent()<Esc>F(a
   menu API-Functions.RCX2\ events.ClickCounter\ (event)                       aClickCounter()<Esc>F(a
   menu API-Functions.RCX2\ events.ClickTime\ (event)                          aClickTime()<Esc>F(a
   menu API-Functions.RCX2\ events.Event\ (event)                              aEvent();<Esc>F(a
   menu API-Functions.RCX2\ events.EventState\ (event)                         aEventState()<Esc>F(a
   menu API-Functions.RCX2\ events.Hysteresis\ (event)                         aHysteresis()<Esc>F(a
   menu API-Functions.RCX2\ events.LowerLimit\ (event)                         aLowerLimit()<Esc>F(a
   menu API-Functions.RCX2\ events.SetClickCounter\ (event,value)              aSetClickCounter();<Esc>F(a
   menu API-Functions.RCX2\ events.SetClickTime\ (event,value)                 aSetClickTime();<Esc>F(a
   menu API-Functions.RCX2\ events.SetEvent\ (event,source,type)               aSetEvent();<Esc>F(a
   menu API-Functions.RCX2\ events.SetHysteresis\ (event,value)                aSetHysteresis();<Esc>F(a
   menu API-Functions.RCX2\ events.SetLowerLimit\ (event,limit)                aSetLowerLimit();<Esc>F(a
   menu API-Functions.RCX2\ events.SetUpperLimit\ (event,limit)                aSetUpperLimit();<Esc>F(a
   menu API-Functions.RCX2\ events.UpperLimit\ (event)                         aUpperLimit()<Esc>F(a
   menu API-Functions.RCX2\ events.ActiveEvents\ (task)                        aActiveEvents()<Esc>F(a
   menu API-Functions.RCX2\ events.CurrentEvents\ (\ )                         aCurrentEvents();
   menu API-Functions.RCX2\ events.Events\ (events)                            aEvents()<Esc>F(a
	"
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
   menu API-Functions.Scout\ events.ActiveEvents\ (task)                aActiveEvents();<Esc>F(a
   menu API-Functions.Scout\ events.CounterLimit\ (n)                   aCounterLimit()<Esc>F(a
   menu API-Functions.Scout\ events.Events\ (events)                    aEvents();<Esc>F(a
   menu API-Functions.Scout\ events.SetSensorClickTime\ (time)          aSetSensorClickTime();<Esc>F(a
   menu API-Functions.Scout\ events.SetCounterLimit\ (n,value)          aSetCounterLimit(,);<Esc>F(a
   menu API-Functions.Scout\ events.SetTimerLimit\ (n,value)            aSetTimerLimit(,);<Esc>F(a
   menu API-Functions.Scout\ events.TimerLimit\ (n)                     aTimerLimit()<Esc>F(a
  imenu API-Functions.Scout\ events.ActiveEvents\ (task)                 ActiveEvents();<Esc>F(a
  imenu API-Functions.Scout\ events.CounterLimit\ (n)                    CounterLimit()<Esc>F(a
  imenu API-Functions.Scout\ events.Events\ (events)                     Events();<Esc>F(a
  imenu API-Functions.Scout\ events.SetSensorClickTime\ (time)           SetSensorClickTime();<Esc>F(a
  imenu API-Functions.Scout\ events.SetCounterLimit\ (n,value)           SetCounterLimit(,);<Esc>F(a
  imenu API-Functions.Scout\ events.SetTimerLimit\ (n,value)             SetTimerLimit(,);<Esc>F(a
  imenu API-Functions.Scout\ events.TimerLimit\ (n)                      TimerLimit()<Esc>F(a
endif
"
"
"===============================================================================================
"----- Menu : API-Constants --------------------------------------------------------------------
"===============================================================================================
"
"----- access control ---------------------------------------------------------------------
if g:NQC_Target=="RCX2" || g:NQC_Target=="Scout"
   menu API-Constants.access\ control.ACQUIRE_OUT_A   aACQUIRE_OUT_A
   menu API-Constants.access\ control.ACQUIRE_OUT_B   aACQUIRE_OUT_B
   menu API-Constants.access\ control.ACQUIRE_OUT_C   aACQUIRE_OUT_C
   menu API-Constants.access\ control.ACQUIRE_SOUND   aACQUIRE_SOUND
  imenu API-Constants.access\ control.ACQUIRE_OUT_A    ACQUIRE_OUT_A
  imenu API-Constants.access\ control.ACQUIRE_OUT_B    ACQUIRE_OUT_B
  imenu API-Constants.access\ control.ACQUIRE_OUT_C    ACQUIRE_OUT_C
  imenu API-Constants.access\ control.ACQUIRE_SOUND    ACQUIRE_SOUND
endif
if g:NQC_Target=="RCX2"
   menu API-Constants.access\ control.ACQUIRE_USER_1  aACQUIRE_USER_1
   menu API-Constants.access\ control.ACQUIRE_USER_2  aACQUIRE_USER_2
   menu API-Constants.access\ control.ACQUIRE_USER_3  aACQUIRE_USER_3
   menu API-Constants.access\ control.ACQUIRE_USER_4  aACQUIRE_USER_4
  imenu API-Constants.access\ control.ACQUIRE_USER_1   ACQUIRE_USER_1
  imenu API-Constants.access\ control.ACQUIRE_USER_2   ACQUIRE_USER_2
  imenu API-Constants.access\ control.ACQUIRE_USER_3   ACQUIRE_USER_3
  imenu API-Constants.access\ control.ACQUIRE_USER_4   ACQUIRE_USER_4
endif
"----- display ----------------------------------------------------------------------------
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2"
   menu API-Constants.display.DISPLAY_OUT_A           aDISPLAY_OUT_A
   menu API-Constants.display.DISPLAY_OUT_B           aDISPLAY_OUT_B
   menu API-Constants.display.DISPLAY_OUT_C           aDISPLAY_OUT_C
   menu API-Constants.display.DISPLAY_SENSOR_1        aDISPLAY_SENSOR_1
   menu API-Constants.display.DISPLAY_SENSOR_2        aDISPLAY_SENSOR_2
   menu API-Constants.display.DISPLAY_SENSOR_3        aDISPLAY_SENSOR_3
   menu API-Constants.display.DISPLAY_WATCH           aDISPLAY_WATCH
  imenu API-Constants.display.DISPLAY_OUT_A            DISPLAY_OUT_A
  imenu API-Constants.display.DISPLAY_OUT_B            DISPLAY_OUT_B
  imenu API-Constants.display.DISPLAY_OUT_C            DISPLAY_OUT_C
  imenu API-Constants.display.DISPLAY_SENSOR_1         DISPLAY_SENSOR_1
  imenu API-Constants.display.DISPLAY_SENSOR_2         DISPLAY_SENSOR_2
  imenu API-Constants.display.DISPLAY_SENSOR_3         DISPLAY_SENSOR_3
  imenu API-Constants.display.DISPLAY_WATCH            DISPLAY_WATCH
endif
if g:NQC_Target=="RCX2"
   menu API-Constants.display.DISPLAY_USER            aDISPLAY_USER
  imenu API-Constants.display.DISPLAY_USER             DISPLAY_USER
endif
"----- output  ----------------------------------------------------------------------------
 menu API-Constants.output.OUT_A                      aOUT_A
 menu API-Constants.output.OUT_B                      aOUT_B
 menu API-Constants.output.OUT_C                      aOUT_C
 menu API-Constants.output.OUT_FLOAT                  aOUT_FLOAT
 menu API-Constants.output.OUT_FULL                   aOUT_FULL
 menu API-Constants.output.OUT_FWD                    aOUT_FWD
 menu API-Constants.output.OUT_HALF                   aOUT_HALF
 menu API-Constants.output.OUT_LOW                    aOUT_LOW
 menu API-Constants.output.OUT_OFF                    aOUT_OFF
 menu API-Constants.output.OUT_ON                     aOUT_ON
 menu API-Constants.output.OUT_REV                    aOUT_REV
 menu API-Constants.output.OUT_TOGGLE                 aOUT_TOGGLE
imenu API-Constants.output.OUT_A                       OUT_A
imenu API-Constants.output.OUT_B                       OUT_B
imenu API-Constants.output.OUT_C                       OUT_C
imenu API-Constants.output.OUT_FLOAT                   OUT_FLOAT
imenu API-Constants.output.OUT_FULL                    OUT_FULL
imenu API-Constants.output.OUT_FWD                     OUT_FWD
imenu API-Constants.output.OUT_HALF                    OUT_HALF
imenu API-Constants.output.OUT_LOW                     OUT_LOW
imenu API-Constants.output.OUT_OFF                     OUT_OFF
imenu API-Constants.output.OUT_ON                      OUT_ON
imenu API-Constants.output.OUT_REV                     OUT_REV
imenu API-Constants.output.OUT_TOGGLE                  OUT_TOGGLE
if g:NQC_Target=="CM"
   menu API-Constants.output.OUT_L                    aOUT_L
   menu API-Constants.output.OUT_R                    aOUT_R
   menu API-Constants.output.OUT_X                    aOUT_X
  imenu API-Constants.output.OUT_L                     OUT_L
  imenu API-Constants.output.OUT_R                     OUT_R
  imenu API-Constants.output.OUT_X                     OUT_X
endif
"----- sensor  ----------------------------------------------------------------------------
if g:NQC_Target=="RCX2"
   menu API-Constants.serial.SERIAL_COMM_DEFAULT      aSERIAL_COMM_DEFAULT
   menu API-Constants.serial.SERIAL_COMM_4800         aSERIAL_COMM_4800
   menu API-Constants.serial.SERIAL_COMM_DUTY25       aSERIAL_COMM_DUTY25
   menu API-Constants.serial.SERIAL_COMM_76KHZ        aSERIAL_COMM_76KHZ
   menu API-Constants.serial.SERIAL_PACKET_DEFAULT    aSERIAL_PACKET_DEFAULT
   menu API-Constants.serial.SERIAL_PACKET_PREAMBLE   aSERIAL_PACKET_PREAMBLE
   menu API-Constants.serial.SERIAL_PACKET_NEGATED    aSERIAL_PACKET_NEGATED
   menu API-Constants.serial.SERIAL_PACKET_CHECKSUM   aSERIAL_PACKET_CHECKSUM
   menu API-Constants.serial.SERIAL_PACKET_RCX        aSERIAL_PACKET_RCX
  imenu API-Constants.serial.SERIAL_COMM_DEFAULT       SERIAL_COMM_DEFAULT
  imenu API-Constants.serial.SERIAL_COMM_4800          SERIAL_COMM_4800
  imenu API-Constants.serial.SERIAL_COMM_DUTY25        SERIAL_COMM_DUTY25
  imenu API-Constants.serial.SERIAL_COMM_76KHZ         SERIAL_COMM_76KHZ
  imenu API-Constants.serial.SERIAL_PACKET_DEFAULT     SERIAL_PACKET_DEFAULT
  imenu API-Constants.serial.SERIAL_PACKET_PREAMBLE    SERIAL_PACKET_PREAMBLE
  imenu API-Constants.serial.SERIAL_PACKET_NEGATED     SERIAL_PACKET_NEGATED
  imenu API-Constants.serial.SERIAL_PACKET_CHECKSUM    SERIAL_PACKET_CHECKSUM
  imenu API-Constants.serial.SERIAL_PACKET_RCX         SERIAL_PACKET_RCX
endif
"----- sensor  ----------------------------------------------------------------------------
   menu API-Constants.sensor.SENSOR_1                 aSENSOR_1
   menu API-Constants.sensor.SENSOR_2                 aSENSOR_2
   menu API-Constants.sensor.SENSOR_3                 aSENSOR_3
  imenu API-Constants.sensor.SENSOR_1                  SENSOR_1
  imenu API-Constants.sensor.SENSOR_2                  SENSOR_2
  imenu API-Constants.sensor.SENSOR_3                  SENSOR_3
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2"
   menu API-Constants.sensor.SENSOR_CELSIUS           aSENSOR_CELSIUS
   menu API-Constants.sensor.SENSOR_EDGE              aSENSOR_EDGE
   menu API-Constants.sensor.SENSOR_FAHRENHEIT        aSENSOR_FAHRENHEIT
   menu API-Constants.sensor.SENSOR_LIGHT             aSENSOR_LIGHT
   menu API-Constants.sensor.SENSOR_PULSE             aSENSOR_PULSE
   menu API-Constants.sensor.SENSOR_ROTATION          aSENSOR_ROTATION
   menu API-Constants.sensor.SENSOR_TOUCH             aSENSOR_TOUCH
   menu API-Constants.sensor.SENSOR_MODE_CELSIUS      aSENSOR_MODE_CELSIUS
   menu API-Constants.sensor.SENSOR_MODE_FAHRENHEIT   aSENSOR_MODE_FAHRENHEIT
   menu API-Constants.sensor.SENSOR_MODE_ROTATION     aSENSOR_MODE_ROTATION
  imenu API-Constants.sensor.SENSOR_CELSIUS            SENSOR_CELSIUS
  imenu API-Constants.sensor.SENSOR_EDGE               SENSOR_EDGE
  imenu API-Constants.sensor.SENSOR_FAHRENHEIT         SENSOR_FAHRENHEIT
  imenu API-Constants.sensor.SENSOR_LIGHT              SENSOR_LIGHT
  imenu API-Constants.sensor.SENSOR_PULSE              SENSOR_PULSE
  imenu API-Constants.sensor.SENSOR_ROTATION           SENSOR_ROTATION
  imenu API-Constants.sensor.SENSOR_TOUCH              SENSOR_TOUCH
  imenu API-Constants.sensor.SENSOR_MODE_CELSIUS       SENSOR_MODE_CELSIUS
  imenu API-Constants.sensor.SENSOR_MODE_FAHRENHEIT    SENSOR_MODE_FAHRENHEIT
  imenu API-Constants.sensor.SENSOR_MODE_ROTATION      SENSOR_MODE_ROTATION
endif
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2" ||  g:NQC_Target=="CM"
   menu API-Constants.sensor.SENSOR_MODE_BOOL         aSENSOR_MODE_BOOL
   menu API-Constants.sensor.SENSOR_MODE_EDGE         aSENSOR_MODE_EDGE
   menu API-Constants.sensor.SENSOR_MODE_PERCENT      aSENSOR_MODE_PERCENT
   menu API-Constants.sensor.SENSOR_MODE_PULSE        aSENSOR_MODE_PULSE
   menu API-Constants.sensor.SENSOR_MODE_RAW          aSENSOR_MODE_RAW
  imenu API-Constants.sensor.SENSOR_MODE_BOOL          SENSOR_MODE_BOOL
  imenu API-Constants.sensor.SENSOR_MODE_EDGE          SENSOR_MODE_EDGE
  imenu API-Constants.sensor.SENSOR_MODE_PERCENT       SENSOR_MODE_PERCENT
  imenu API-Constants.sensor.SENSOR_MODE_PULSE         SENSOR_MODE_PULSE
  imenu API-Constants.sensor.SENSOR_MODE_RAW           SENSOR_MODE_RAW
endif
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2"
   menu API-Constants.sensor.SENSOR_TYPE_LIGHT        aSENSOR_TYPE_LIGHT
   menu API-Constants.sensor.SENSOR_TYPE_NONE         aSENSOR_TYPE_NONE
   menu API-Constants.sensor.SENSOR_TYPE_ROTATION     aSENSOR_TYPE_ROTATION
   menu API-Constants.sensor.SENSOR_TYPE_TEMPERATURE  aSENSOR_TYPE_TEMPERATURE
   menu API-Constants.sensor.SENSOR_TYPE_TOUCH        aSENSOR_TYPE_TOUCH
  imenu API-Constants.sensor.SENSOR_TYPE_LIGHT         SENSOR_TYPE_LIGHT
  imenu API-Constants.sensor.SENSOR_TYPE_NONE          SENSOR_TYPE_NONE
  imenu API-Constants.sensor.SENSOR_TYPE_ROTATION      SENSOR_TYPE_ROTATION
  imenu API-Constants.sensor.SENSOR_TYPE_TEMPERATURE   SENSOR_TYPE_TEMPERATURE
  imenu API-Constants.sensor.SENSOR_TYPE_TOUCH         SENSOR_TYPE_TOUCH
endif
if g:NQC_Target=="CM"
   menu API-Constants.sensor.SENSOR_L                 aSENSOR_L
   menu API-Constants.sensor.SENSOR_M                 aSENSOR_M
   menu API-Constants.sensor.SENSOR_R                 aSENSOR_R
  imenu API-Constants.sensor.SENSOR_L                  SENSOR_L
  imenu API-Constants.sensor.SENSOR_M                  SENSOR_M
  imenu API-Constants.sensor.SENSOR_R                  SENSOR_R
endif
"----- sound   ----------------------------------------------------------------------------
 menu API-Constants.sound.SOUND_CLICK                 aSOUND_CLICK
 menu API-Constants.sound.SOUND_DOUBLE_BEEP           aSOUND_DOUBLE_BEEP
 menu API-Constants.sound.SOUND_DOWN                  aSOUND_DOWN
 menu API-Constants.sound.SOUND_FAST_UP               aSOUND_FAST_UP
 menu API-Constants.sound.SOUND_LOW_BEEP              aSOUND_LOW_BEEP
 menu API-Constants.sound.SOUND_UP                    aSOUND_UP
imenu API-Constants.sound.SOUND_CLICK                  SOUND_CLICK
imenu API-Constants.sound.SOUND_DOUBLE_BEEP            SOUND_DOUBLE_BEEP
imenu API-Constants.sound.SOUND_DOWN                   SOUND_DOWN
imenu API-Constants.sound.SOUND_FAST_UP                SOUND_FAST_UP
imenu API-Constants.sound.SOUND_LOW_BEEP               SOUND_LOW_BEEP
imenu API-Constants.sound.SOUND_UP                     SOUND_UP
"----- RCX2 events ------------------------------------------------------------------------
if g:NQC_Target=="RCX2"
   menu API-Constants.RCX2\ events.EVENT_TYPE_PRESSED       aEVENT_TYPE_PRESSED
   menu API-Constants.RCX2\ events.EVENT_TYPE_RELEASED      aEVENT_TYPE_RELEASED
   menu API-Constants.RCX2\ events.EVENT_TYPE_PULSE         aEVENT_TYPE_PULSE
   menu API-Constants.RCX2\ events.EVENT_TYPE_EDGE          aEVENT_TYPE_EDGE
   menu API-Constants.RCX2\ events.EVENT_TYPE_FASTCHANGE    aEVENT_TYPE_FASTCHANGE
   menu API-Constants.RCX2\ events.EVENT_TYPE_LOW           aEVENT_TYPE_LOW
   menu API-Constants.RCX2\ events.EVENT_TYPE_NORMAL        aEVENT_TYPE_NORMAL
   menu API-Constants.RCX2\ events.EVENT_TYPE_HIGH          aEVENT_TYPE_HIGH
   menu API-Constants.RCX2\ events.EVENT_TYPE_CLICK         aEVENT_TYPE_CLICK
   menu API-Constants.RCX2\ events.EVENT_TYPE_DOUBLECLICK   aEVENT_TYPE_DOUBLECLICK
   menu API-Constants.RCX2\ events.EVENT_TYPE_MESSAGE       aEVENT_TYPE_MESSAGE
  imenu API-Constants.RCX2\ events.EVENT_TYPE_PRESSED        EVENT_TYPE_PRESSED
  imenu API-Constants.RCX2\ events.EVENT_TYPE_RELEASED       EVENT_TYPE_RELEASED
  imenu API-Constants.RCX2\ events.EVENT_TYPE_PULSE          EVENT_TYPE_PULSE
  imenu API-Constants.RCX2\ events.EVENT_TYPE_EDGE           EVENT_TYPE_EDGE
  imenu API-Constants.RCX2\ events.EVENT_TYPE_FASTCHANGE     EVENT_TYPE_FASTCHANGE
  imenu API-Constants.RCX2\ events.EVENT_TYPE_LOW            EVENT_TYPE_LOW
  imenu API-Constants.RCX2\ events.EVENT_TYPE_NORMAL         EVENT_TYPE_NORMAL
  imenu API-Constants.RCX2\ events.EVENT_TYPE_HIGH           EVENT_TYPE_HIGH
  imenu API-Constants.RCX2\ events.EVENT_TYPE_CLICK          EVENT_TYPE_CLICK
  imenu API-Constants.RCX2\ events.EVENT_TYPE_DOUBLECLICK    EVENT_TYPE_DOUBLECLICK
  imenu API-Constants.RCX2\ events.EVENT_TYPE_MESSAGE        EVENT_TYPE_MESSAGE
endif
"----- Scout events -----------------------------------------------------------------------
if g:NQC_Target=="Scout"
   menu API-Constants.Scout\ events.EVENT_1_PRESSED         aEVENT_1_PRESSED
   menu API-Constants.Scout\ events.EVENT_2_PRESSED         aEVENT_2_PRESSED
   menu API-Constants.Scout\ events.EVENT_1_RELEASED        aEVENT_1_RELEASED
   menu API-Constants.Scout\ events.EVENT_2_RELEASED        aEVENT_2_RELEASED
   menu API-Constants.Scout\ events.EVENT_LIGHT_HIGH        aEVENT_LIGHT_HIGH
   menu API-Constants.Scout\ events.EVENT_LIGHT_NORMAL      aEVENT_LIGHT_NORMAL
   menu API-Constants.Scout\ events.EVENT_LIGHT_LOW         aEVENT_LIGHT_LOW
   menu API-Constants.Scout\ events.EVENT_LIGHT_CLICK       aEVENT_LIGHT_CLICK
   menu API-Constants.Scout\ events.EVENT_LIGHT_DOUBLECLICK aEVENT_LIGHT_DOUBLECLICK
   menu API-Constants.Scout\ events.EVENT_COUNTER_0         aEVENT_COUNTER_0
   menu API-Constants.Scout\ events.EVENT_COUNTER_1         aEVENT_COUNTER_1
   menu API-Constants.Scout\ events.EVENT_TIMER_0           aEVENT_TIMER_0
   menu API-Constants.Scout\ events.EVENT_TIMER_1           aEVENT_TIMER_1
   menu API-Constants.Scout\ events.EVENT_TIMER_2           aEVENT_TIMER_2
   menu API-Constants.Scout\ events.EVENT_MESSAGE           aEVENT_MESSAGE
  imenu API-Constants.Scout\ events.EVENT_1_PRESSED          EVENT_1_PRESSED
  imenu API-Constants.Scout\ events.EVENT_2_PRESSED          EVENT_2_PRESSED
  imenu API-Constants.Scout\ events.EVENT_1_RELEASED         EVENT_1_RELEASED
  imenu API-Constants.Scout\ events.EVENT_2_RELEASED         EVENT_2_RELEASED
  imenu API-Constants.Scout\ events.EVENT_LIGHT_HIGH         EVENT_LIGHT_HIGH
  imenu API-Constants.Scout\ events.EVENT_LIGHT_NORMAL       EVENT_LIGHT_NORMAL
  imenu API-Constants.Scout\ events.EVENT_LIGHT_LOW          EVENT_LIGHT_LOW
  imenu API-Constants.Scout\ events.EVENT_LIGHT_CLICK        EVENT_LIGHT_CLICK
  imenu API-Constants.Scout\ events.EVENT_LIGHT_DOUBLECLICK  EVENT_LIGHT_DOUBLECLICK
  imenu API-Constants.Scout\ events.EVENT_COUNTER_0          EVENT_COUNTER_0
  imenu API-Constants.Scout\ events.EVENT_COUNTER_1          EVENT_COUNTER_1
  imenu API-Constants.Scout\ events.EVENT_TIMER_0            EVENT_TIMER_0
  imenu API-Constants.Scout\ events.EVENT_TIMER_1            EVENT_TIMER_1
  imenu API-Constants.Scout\ events.EVENT_TIMER_2            EVENT_TIMER_2
  imenu API-Constants.Scout\ events.EVENT_MESSAGE            EVENT_MESSAGE
	"
   menu API-Constants.LIGHT_ON                              aLIGHT_ON
   menu API-Constants.LIGHT_OFF                             aLIGHT_OFF
   menu API-Constants.SCOUT_MODE_POWER                      aSCOUT_MODE_POWER
   menu API-Constants.SCOUT_MODE_STANDALONE                 aSCOUT_MODE_STANDALONE
  imenu API-Constants.LIGHT_ON                               LIGHT_ON
  imenu API-Constants.LIGHT_OFF                              LIGHT_OFF
  imenu API-Constants.SCOUT_MODE_POWER                       SCOUT_MODE_POWER
  imenu API-Constants.SCOUT_MODE_STANDALONE                  SCOUT_MODE_STANDALONE
endif
"----- misc    ----------------------------------------------------------------------------
if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2" ||  g:NQC_Target=="Scout"
   menu API-Constants.TX_POWER_HI                           aTX_POWER_HI
   menu API-Constants.TX_POWER_LO                           aTX_POWER_LO
  imenu API-Constants.TX_POWER_HI                            TX_POWER_HI
  imenu API-Constants.TX_POWER_LO                            TX_POWER_LO
endif
"
"
"===============================================================================================
"----- Menu : Datalog  -------------------------------------------------------------------------
"===============================================================================================
"
  amenu  <silent> &Datalog.&upload\ datalog\ into\ buffer             <C-C>:call NQC_DatalogUpload()<CR>
  amenu  <silent> &Datalog.-SEP1-                                     :
  amenu  <silent> &Datalog.show\ y-&plot\                             <C-C>:call NQC_DatalogPlot(1)<CR>
  amenu  <silent> &Datalog.show\ &x-y-plot\                           <C-C>:call NQC_DatalogPlot(2)<CR>
  amenu  <silent> &Datalog.&save\ plot\ (<filenam>\.ps)               <C-C>:call NQC_DatalogPlot(10)<CR>
  amenu  <silent> &Datalog.p&rint\ plot\                              <C-C>:call NQC_DatalogPlot(11)<CR>
  amenu  <silent> &Datalog.plot\ &type.&impulses                      <C-C>:call NQC_DatalogPlotType("impulses")<CR>
  amenu  <silent> &Datalog.plot\ &type.&lines                         <C-C>:call NQC_DatalogPlotType("lines")<CR>
  amenu  <silent> &Datalog.plot\ &type.l&ines+points                  <C-C>:call NQC_DatalogPlotType("linespoints")<CR>
  amenu  <silent> &Datalog.plot\ &type.&points                        <C-C>:call NQC_DatalogPlotType("points")<CR>
  amenu  <silent> &Datalog.plot\ &type.&steps                         <C-C>:call NQC_DatalogPlotType("steps")<CR>
  amenu  <silent> &Datalog.plot\ t&itle                               <C-C>:call NQC_DatalogPlotTitle()<CR>
  amenu  <silent> &Datalog.-SEP2-                                     :
  amenu  <silent> &Datalog.&erase\ programs\ and\ datalogs\ from\ RCX <C-C>:call NQC_DatalogClear()<CR>
"
"===============================================================================================
"----- Menu : NQC-Run  -------------------------------------------------------------------------
"===============================================================================================
"
amenu  NQC-&Run.save\ and\ &compile\ \<F9\>                 <C-C>:call NQC_SaveCompile ()<CR>
amenu  NQC-&Run.-SEP1-                                      :
if g:NQC_Target=="RCX" ||  g:NQC_Target=="RCX2"
  amenu  NQC-&Run.download\ program\ &1\ to\ RCX            <C-C>:call NQC_CompDown (1)<CR>
  amenu  NQC-&Run.download\ program\ &2\ to\ RCX            <C-C>:call NQC_CompDown (2)<CR>
  amenu  NQC-&Run.download\ program\ &3\ to\ RCX            <C-C>:call NQC_CompDown (3)<CR>
  amenu  NQC-&Run.download\ program\ &4\ to\ RCX            <C-C>:call NQC_CompDown (4)<CR>
  amenu  NQC-&Run.download\ program\ &5\ to\ RCX            <C-C>:call NQC_CompDown (5)<CR>
  amenu  NQC-&Run.-SEP2-                                    :
  amenu  NQC-&Run.download\ program\ 1\ to\ RCX\ and\ Run   <C-C>:call NQC_CompDownRun (1)<CR>
  amenu  NQC-&Run.download\ program\ 2\ to\ RCX\ and\ Run   <C-C>:call NQC_CompDownRun (2)<CR>
  amenu  NQC-&Run.download\ program\ 3\ to\ RCX\ and\ Run   <C-C>:call NQC_CompDownRun (3)<CR>
  amenu  NQC-&Run.download\ program\ 4\ to\ RCX\ and\ Run   <C-C>:call NQC_CompDownRun (4)<CR>
  amenu  NQC-&Run.download\ program\ 5\ to\ RCX\ and\ Run   <C-C>:call NQC_CompDownRun (5)<CR>
  amenu  NQC-&Run.-SEP3-                                    :
  amenu  NQC-&Run.&run\ current\ program                    <C-C>:call NQC_RunCurrent ()<CR>
  imenu  NQC-&Run.-SEP4-                                    :
  amenu  <silent> NQC-&Run.&hardcopy\ buffer\ to\ FILENAME\.ps       <C-C>:call Hardcopy("n")<CR>
  vmenu  <silent> NQC-&Run.hard&copy\ highlighted\ part\ to\ FILENAME\.part\.ps   <C-C>:call Hardcopy("v")<CR>
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
	amenu  <silent> NQC-&Run.&settings                         <C-C>:call NQC_Settings()<CR>

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
  let zz=   "//----------------------------------------------------------------------\n"
  let zz=zz."//  \n"
  let zz=zz."//----------------------------------------------------------------------\n"
  put =zz
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Comments : Function  Comment
"------------------------------------------------------------------------------
function! NQC_CommentFunction ()
  let zz=    "//=====================================================================================\n"
  let zz= zz."//\n"
  let zz= zz."//        NAME:  \n"
  let zz= zz."//\n"
  let zz= zz."// DESCRIPTION:  \n"
  let zz= zz."//\n"
  let zz= zz."//- PARAMETER -------------------------------------------------------------------------\n"
  let zz= zz."//     Mode   Type            Name            Description\n"
  let zz= zz."//-------------------------------------------------------------------------------------\n"
  let zz= zz."//       in:  \n"
  let zz= zz."//   in-out:  \n"
  let zz= zz."//      out:  \n"
  let zz= zz."//-------------------------------------------------------------------------------------\n"
  let zz= zz."//   AUTHOR:  ".s:NQC_AuthorName."\n"
  let zz= zz."//  CREATED:  ".strftime("%x - %X")."\n"
  let zz= zz."//=====================================================================================\n"
  put =zz
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Comments : File Prologue
"------------------------------------------------------------------------------
function! NQC_CommentFilePrologue ()
	let File = expand("%:t")                  " name of the file in the current buffer
	let zz=    "//=====================================================================================\n"
	let zz= zz."//\n"
	let zz= zz."//       FILENAME:\t".File."\n"
	let zz= zz."//    DESCRIPTION:\t\n"
	let zz= zz."//\n"
	let zz= zz."//       COMPILER:\tnqc\n"
	let zz= zz."//         AUTHOR:\t".s:NQC_AuthorName."\n"
	let zz= zz."//        CREATED:\t".strftime("%x - %X")."\n"
	let zz= zz."//=====================================================================================\n"
	put! =zz
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
  let zz=    "do\n{\n}\nwhile (  );"
  let zz= zz."\t\t\t\t// -----  end do-while  -----\n"
	put =zz
	normal  =3+
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Statements : switch
"------------------------------------------------------------------------------
"
function! NQC_CodeSwitch ()
  let zz= "switch (  )\n{\n"
	let zz= zz."case 0:\t\n\t\tbreak;\n\n"
	let zz= zz."case 0:\t\n\t\tbreak;\n\n"
	let zz= zz."case 0:\t\n\t\tbreak;\n\n"
	let zz= zz."case 0:\t\n\t\tbreak;\n\n"
	let zz= zz."default:\t\n\t\tbreak;\n}"
  let zz= zz."\t\t\t\t//  -----  end switch  -----\n"
	put =zz	
	" indent 
	normal  =16+
	" delete case labels
	exe ":.,+12s/0//"
	-11
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Statements : #ifndef
"------------------------------------------------------------------------------
function! NQC_CodeIfndef (...)
	let defaultcond	= toupper(expand("%:r"))."_INC"
	let	identifier=inputdialog("(uppercase) condition for #ifndef", defaultcond )
	if identifier != ""
		let zz=    "#ifndef  ".identifier."\n"
		let zz= zz."#define  ".identifier."\n\n\n"
		let zz= zz."#endif   // ----- #ifndef ".identifier."  -----\n"
		put =zz
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
  let zz=    "task\n".identifier."\t(  )\n{\n\t\n\treturn ;\n}"
  let zz= zz."\t\t\t\t// ----------  end of task ".identifier."  ----------"
  put =zz
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
	let zz=    "void\n".identifier."\t(  )\n{\n\t\n\treturn ;\n}"
	let zz= zz."\t\t\t\t// ----------  end of function ".identifier."  ----------"
	put =zz
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
	let zz=    "sub\n".identifier."\t(  )\n{\n\t\n\treturn ;\n}"
	let zz= zz."\t\t\t\t// ----------  end of subroutine ".identifier."  ----------"
	put =zz
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Statements : acquire / monitor / catch
"------------------------------------------------------------------------------
function! NQC_Acquire ()
  let zz=    "acquire ()\n{\n}\t\t\t// -----  end acquire  -----\n"
  let zz= zz."catch\n{\n}\t\t\t// -----  end catch  -----\n"
  put =zz
	normal  =5+
endfunction
"
function! NQC_Monitor ()
  let zz=    "monitor ()\n{\n}\t\t\t// -----  end monitor  -----\n"
  let zz= zz."catch\n{\n}\t\t\t// -----  end catch  -----\n"
  put =zz
	normal  =5+
endfunction
"
function! NQC_Catch ()
  let zz=    "catch ()\n{\n}\t\t\t// -----  end catch  -----\n"
  put =zz
	normal  =2+
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Statements : save and compile
"------------------------------------------------------------------------------
function! NQC_SaveCompile ()
  let zz= "update | !nqc -T".g:NQC_Target." %"
  exec zz
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Statements : compile, download
"  NQC-Statements : compile, download and run
"  NQC-Statements : run current program
"------------------------------------------------------------------------------
function! NQC_CompDown (n)
  let zz= "!nqc -T".g:NQC_Target." -S".g:NQC_Portname." -d -pgm ".a:n." %"
  exec zz
endfunction
"
function! NQC_CompDownRun (n)
  if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2"
    let zz= "!nqc -T".g:NQC_Target." -S".g:NQC_Portname." -d -pgm ".a:n." % -run"
  else
    let zz= "!nqc -T".g:NQC_Target." -S".g:NQC_Portname." -d %"
  endif
  exec zz
endfunction
"
function! NQC_RunCurrent ()
  if g:NQC_Target=="RCX" || g:NQC_Target=="RCX2"
    let zz= "!nqc -T".g:NQC_Target." -S".g:NQC_Portname." -run"
  endif
  exec zz
endfunction
"
"------------------------------------------------------------------------------
"  NQC-Statements : download firmware
"------------------------------------------------------------------------------
function! NQC_DLoadFirmware (n)
	if a:n=="fast"
		let zz= "!nqc  -S".g:NQC_Portname." -near -firmfast ".g:NQC_RCX_Firmware
	else
		let zz= "!nqc  -S".g:NQC_Portname." -firmware ".g:NQC_RCX_Firmware
	endif
	exec zz
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
  let zz= "!nqc -S".g:NQC_Portname." -clear"
  exec zz
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
		if a:arg1 == "r"
			let	l:snippetfile=browse(0,"read a code snippet",s:NQC_CodeSnippets,"")
			if filereadable(l:snippetfile)
				let	length= line("$")
				:execute "read ".l:snippetfile
				let	length= line("$")-length-1
				if length>=0
					silent exe "normal =".length."+"
				endif
			endif
		endif
		"
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
  let settings = settings."F9  :  save and compile buffer\n"
  let settings = settings."\nMake changes in file nqc.vim \n"
  let settings = settings."----------------------------------------------------------------------------------------\n"
  let settings = settings."NQC-Support, Version ".s:NQC_Version."  /  Dr.-Ing. Fritz Mehner  /  mehner@fh-swf.de\n"
  let dummy=confirm( settings, "ok", 1, "Info" )
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
		:aunmenu &Tools.Load\ NQC\ Support
		exe 'amenu <silent> 40.1140   &Tools.Unload\ NQC\ Support  	<C-C>:call NQC_Handle()<CR>'
	else
		" NQC is now inactive and was former active or in initial state -1 
		if s:NQC_Active == 0
			" Remove Tools.Unload if NQC was former inactive
			:aunmenu &Tools.Unload\ NQC\ Support
		else
			" Set initial state NQC_Active=-1 to inactive state NQC_Active=0
			" This protects from removing Tools.Unload during initialization after
			" loading this script
			let s:NQC_Active = 0
			" Insert Tools.Load
		endif
		exe 'amenu <silent> 40.1000 &Tools.-SEP100- : '
		exe 'amenu <silent> 40.1140 &Tools.Load\ NQC\ Support <C-C>:call NQC_Handle()<CR>'
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
