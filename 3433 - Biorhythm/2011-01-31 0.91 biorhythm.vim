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

function! s:Year2Sec(y)
	if s:IsLeapYear(a:y)
		return 366*24*3600
	else
		return 365*24*3600
	endif
endfunction

function! s:Month2Day(y,m)
	let daysPerMonth=[0,31,28,31,30,31,30,31,31,30,31,30,31]
	if a:m != 2
		let d=daysPerMonth[a:m]
	else
		let d=s:IsLeapYear(a:y) ? 29 : 28
	endif
	return d
endfunction

function! s:Month2Sec(y,m)
	return s:Month2Day(a:y,a:m)*24*3600
endfunction

function! s:GetDayDiff(birthday,today)
	let y1 = a:birthday[0]
	let m1 = a:birthday[1]
	let d1 = a:birthday[2]
	let y2 = a:today[0]
	let m2 = a:today[1]
	let d2 = a:today[2]
	let res=0
	for i in range(y1,y2 - 1)
		if s:IsLeapYear(i)
			let res += 366
		else
			let res += 365 
		endif
	endfor
	for i in range(1,m2-1)
		let res += s:Month2Day(y2,i)
	endfor
	let res+=d2
	for i in range(1,m1-1)
		let res-=s:Month2Day(y1,i)
	endfor
	let res-=d1
	return res
endfunction


"in [year,month,day] form
function! s:GetTodayDate()
	let secPerDay=24*3600
	let y=1970
	let m=1
	let d=1
	let t=localtime()
	while t > s:Year2Sec(y)
		let t-=s:Year2Sec(y)
		let y+=1
	endwhile
	while t > s:Month2Sec(y,m)
		let t-=s:Month2Sec(y,m)
		let m+=1
	endwhile
	while t > 24*3600
		let d+=1
		let t-=24*3600
	endwhile
	return [y,m,d]
endfunction

function! s:DisplayBiorhythm()
	let pi=3.1415926
	let t=s:GetDayDiff(s:birthday,s:GetTodayDate())
	let physical		= sin( 2 * pi * t / 23)
	let emotional		= sin( 2 * pi * t / 28 )
	let intellectual	= sin( 2 * pi * t / 33 )
	let intuitive		= sin( 2 * pi * t / 38 )

	echo printf("physical:	%4d%%",float2nr(100 * physical))
	echo printf("emotional:	%4d%%",float2nr(100 * emotional))
	echo printf("intellectual:	%4d%%",float2nr(100 * intellectual))
	echo printf("intuitive:	%4d%%",float2nr(100 * intuitive))
endfunction

command! Bio call s:DisplayBiorhythm()


