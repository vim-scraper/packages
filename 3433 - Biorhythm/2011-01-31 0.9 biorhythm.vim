"	Biorhythm
"	More information refer to http://en.wikipedia.org/wiki/Biorhythm
"
"	Maintainer:	Bi Ran(biran0079@gmail.com)
"	Usage:		edit your data of birth
"			(s:birthday variable, in format [year,month,day])
"			type	:Bio
"	This is simply for fun
"	No guarrantee to any prediction this script makes
"
"	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
"	!!!   ENTER YOUR BIRTHDAY BELOW  !!!
"	!!!   In [year,month,day] format !!!
"	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
let s:birthday = [1989,5,13]
"
"
"

function! s:IsLeapYear(y)
	return a:y % 400==0 || (a:y % 100 != 0 && a:y % 4 == 0)
endfunction

"time from 1970-1-1 to y-m-d, in days
function! s:GetDay(birthday)
	let y = a:birthday[0]
	let m = a:birthday[1]
	let d = a:birthday[2]
	let res=0
	let daysPerMonth=[0,31,28,31,30,31,30,31,31,30,31,30,31]
	for i in range(1970,y - 1)
		if s:IsLeapYear(i)
			let res += 366
		else
			let res += 365 
		endif
	endfor
	for i in range(1,m-1)
		let res+=daysPerMonth[i]
	endfor
	if s:IsLeapYear(y) && m > 2
		let res+=1
	endif
	let res+=d
	return res
endfunction

function! s:DisplayBiorhythm()
	let pi=3.1415926
	let t=localtime()/3600/24 - s:GetDay(s:birthday)
	let physical		= sin( 2 * pi * t / 23 )
	let emotional		= sin( 2 * pi * t / 28 )
	let intellectual	= sin( 2 * pi * t / 33 )
	let intuitive		= sin( 2 * pi * t / 38 )

	echo printf("physical:	%4d%%",float2nr(100 * physical))
	echo printf("emotional:	%4d%%",float2nr(100 * emotional))
	echo printf("intellectual:	%4d%%",float2nr(100 * intellectual))
	echo printf("intuitive:	%4d%%",float2nr(100 * intuitive))
endfunction

command! Bio call s:DisplayBiorhythm()


