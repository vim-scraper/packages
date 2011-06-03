: let score=1
: let votes=8



" s:n1  = How many  1-point  vots we have
" s:nm1 = How many -1-points vots we have
" score = the total score
" votes = the number of votes
" We must have this relation:
" votes = s:n4 + s:n1 + s:nm1
" From this, we have
" s:nm1 = votes - ( s:n4 + s:n1 ) 

" We build a function for displaying statistics
: function s:NiceDisplay (str1, num2)
: 	echo a:str1 
: 	let s:counter = 0
: 	while s:counter < a:num2
: 		sleep 70m
: 		echon "."
: 		let s:counter = s:counter + 1
: 	endwhile
:endfunction "endNiceDisplay


" Here we initialize
: let s:n4 = 0 
: let s:nm1 = 0

: while s:n4 <= votes 
:	let s:n1 = 0 
: 	while s:n1 <= votes
: 		let s:nm1 = votes - ( s:n4 + s:n1 )
: 		let sscore = s:n4 * 4 + s:n1 - s:nm1
: 		if sscore == score
:			if s:nm1 > 0
:			let s:n4percent = 50 * s:n4 / votes 
:			let s:n1percent = 50 * s:n1 / votes 
:			let s:nm1percent = 50 * s:nm1 / votes
:			if s:n4 < 10  
: 				call s:NiceDisplay (" 4-points votes"."= ".s:n4." ", s:n4percent) 
:			else
: 				call s:NiceDisplay (" 4-points votes"."=".s:n4." ", s:n4percent) 
:			endif " s:n4percent
:			if s:n1 < 10  
: 				call s:NiceDisplay (" 1-point  votes"."= ".s:n1." ", s:n1percent) 
:			else
: 				call s:NiceDisplay (" 1-point  votes"."=".s:n1." ", s:n1percent) 
:			endif " s:n1percent
:			if s:nm1 < 10  
:	 			call s:NiceDisplay ("-1-point  votes"."= ".s:nm1." ", s:nm1percent) 
:			else
:	 			call s:NiceDisplay ("-1-point  votes"."=".s:nm1." ", s:nm1percent) 
:			endif " s:n1mpercent
:			let s:n4 = votes * 2
: 			break
: 		endif
: 		endif
: 		let s:n1 = s:n1 + 1
: 		endwhile " s:n1 loop
: 	let s:n4 = s:n4 + 1
: endwhile " s:n4 loop
