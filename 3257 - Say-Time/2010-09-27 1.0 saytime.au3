#include <Date.au3>

$talk = ObjCreate("SAPI.SpVoice")

$nDonotspeakunlesszero = 0

While True
   
    Sleep(1000)

    $tTime = _Date_Time_GetSystemTime()
    $aTime = _Date_Time_SystemTimeToArray($tTime)
    $sTime = ""

    If $nDonotspeakunlesszero>0 Then
	; make sure there is a wait to timeout the "not speaking" bit
        $nDonotspeakunlesszero = $nDonotspeakunlesszero-1
    EndIf

    If ($aTime[4]=0 OR $aTime[4]=30) And $nDonotspeakunlesszero=0 Then

        Switch $aTime[7]
        Case 0
            $sTime = $sTime & "Sunday "
        Case 1
            $sTime = $sTime & "Monday "
        Case 2
            $sTime = $sTime & "Tuesday "
        Case 3
            $sTime = $sTime & "Wednesday "
        Case 4
            $sTime = $sTime & "Thursday "
        Case 5
            $sTime = $sTime & "Friday "
        Case 6
            $sTime = $sTime & "Saturday "
        EndSwitch

        $sTime = $sTime & $aTime[1]

        Switch $aTime[0]
        Case 1
            $sTime = $sTime & " January "
        Case 2
            $sTime = $sTime & " February "
        Case 3
            $sTime = $sTime & " March "
        Case 4
            $sTime = $sTime & " April "
        Case 5
            $sTime = $sTime & " May "
        Case 6
            $sTime = $sTime & " June "
        Case 7
            $sTime = $sTime & " July "
        Case 8
            $sTime = $sTime & " August "
        Case 9
            $sTime = $sTime & " September "
        Case 10
            $sTime = $sTime & " October "
        Case 11
            $sTime = $sTime & " November "
        Case 12
            $sTime = $sTime & " December "
        EndSwitch

        $talk.Speak($sTime)
        Sleep(1300)

        $sTime = ""

        ; hour add extra zero
        If $aTime[3]<=9 Then
            $sTime = $sTime & "0"
        EndIf
        ; common hour
        $sTime = $sTime & $aTime[3]

        ; say it
        $talk.Speak($sTime)

        Sleep(500)

        $sTime = ""

        ; minute add extra zero
        If $aTime[4]<=9 Then
            $sTime = "0"
        EndIf
        ; common minute
        $sTime = $sTime & $aTime[4]

        ; say it
        $talk.Speak($sTime)

        ; but avoid saying it again
        $nDonotspeakunlesszero = 120

    EndIf

Wend

