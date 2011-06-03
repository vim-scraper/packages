" +-----------------------------------------------------------------------------+
" | CHANGING COLOUR SCRIPT                                                      |
" +-----------------------------------------------------------------------------+
" | START                                                                       |
" +-----------------------------------------------------------------------------+
" | REVISONS:                                                                   |
" | MON 11TH JAN 2010:   12.1                                                   |
" |                      Changed possible annoying effect to 'off' by default.  |
" | SUN 10TH JAN 2010:   12.0                                                   |
" |                      Cosmetic improvements.                                 |
" |                      11.9                                                   |
" |                      Cosmetic improvements.                                 |
" |                      11.8                                                   |
" |                      Cosmetic improvements.                                 |
" |                      11.7                                                   |
" |                      Further tweaks around Constant and Identifier.         |
" |                      11.6                                                   |
" |                      Further tweaks around Constant and Identifier.         |
" |                      11.5                                                   |
" |                      Blended in elements better, improved constrasts more.  |
" |                      11.4                                                   |
" |                      Lots of improvements to the contrasts, made cursorline |
" |                      blend nicely with line number's colour.                |
" |                      11.3                                                   |
" |                      Improved some elements that had somehow become         |
" |                      invisible like Folded. There should be no elements     |
" |                      that are invisible now.                                |
" | FRI  8TH JAN 2010:   11.2                                                   |
" |                      Tweaked all the elements, removed some glitches.       |
" | MON 21ST DEC 2009:   11.1                                                   |
" |                      Tweaked visibility of Identifier and Statement.        |
" | WED 16TH DEC 2009:   11.0                                                   |
" |                      Prettied-up Identifier and Statement. Still some       |
" |                      non-vital glitches like sometimes the search/replace   |
" |                      prompt is invisible, (fg==bg), folded lines text       |
" |                      invisible (fg=bg) (which may not be so bad) and        |
" |                      cursorline's colour is not always very distinct from   |
" |                      general background (which may not be a problem if      |
" |                      you don't use it.) But generally getting there.        |
" | SAT 5th DEC 2009:    10.9                                                   |
" |                      Now the Identifier's green is starting to work out nice|
" |                      and particularly with the new green in 'Normal' which  |
" |                      complements with that of Identifier's.                 |
" |                      10.8                                                   |
" |                      Not claiming this is perfect but seems to be working   |
" |                      for me, Identifier's green now feels a bit smoother.   |
" |                      10.7                                                   |
" |                      Made the Identier's green a bit nicer, at dark and     |
" |                      light backgrounds.                                     |
" |                      10.6                                                   |
" |                      Got rid at last of the awful pink colour for the       |
" |                      Normal, at dark ranges. Now it's green, and hence a    |
" |                      bit more usable.                                       |
" | FRI 4th DEC 2009:    10.5                                                   |
" |                      Identifier vis 'fix' still too agreessive, tweaked.    |
" |                      10.4                                                   |
" |                      Made the Identifier 'fix' visibility less agressive.   |
" | WED 18TH OCT 2009:   10.3                                                   |
" |                      Trouble-shoot some visibilty problems associated with  |
" |                      syntax elements Pmenu and CursorLine.                  |
" |                      10.2                                                   |
" |                      Further tuning-up.                                     |
" | TUE 17TH OCT 2009:   10.1                                                   |
" |                      Just a quick update. Had to tune-up the white-peak as  |
" |                      it was staying on a bit too long.                      |
" |                      10.0                                                   |
" |                      On the 10.0 version, perhaps the best version. Normal  |
" |                      now blends more smoothly than ever at darker           |
" |                      background ranges, and the background colour at the    |
" |                      lighter ranges now peaks up to a pure white before     |
" |                      coming down again to a tint which can be a shade of    |
" |                      either light-green, cyan, rose. The contrast is great, |
" |                      the visibility is great, and the smoothness is great.  |
" |                      If you haven't tried this before you'll really be      |
" |                      amazed.                                                |
" | WED 27TH MAY 2009: o VER 1.00                                               |
" +-----------------------------------------------------------------------------+

" +------------------------------------------------------------------------------+
" | The following static variable provide a way for the script to check if its   |
" | time that the muscle function should call vim's 'highlight' command to cause |
" | all the syntax elements colouring to change. A combination of this and       |
" | 'changefreq' (below) is used to determine final yes/no (it is time or not)   |
" +------------------------------------------------------------------------------+
let s:oldactontime=-9999

" +------------------------------------------------------------------------------+
" | This variable is used to control how often the script deems it's time to     |
" | 'progress' the colour (also see above). The higher the value the less often  |
" | it will do so, the lower the more often it will do so.  2880 is equivalnet   |
" | to every minute, 5760 every two minutes.                                     |
" +------------------------------------------------------------------------------+
let g:changefreq=2880

" +------------------------------------------------------------------------------+
" | The following variable is the size of the area below and above that zone that|
" | makes the text colour 'darken' to avoid clashing with the background. It     |
" | lasts around 2 minutes and during this time the text colour stays exactly    |
" | unmodified but the background is tweaked up or down to 'ease' visibility     |
" | while not drastically changing anything yet. This happens in that area known |
" | as the 'danger' area. The bigger this value the bigger that 'ease' period    |
" | is, with 7200 being around 2 minutes above and below 'danger' area.          |
" +------------------------------------------------------------------------------+
let g:easeArea=8200

"debug
"let g:mytime=16000
"let g:myhour=0

" +------------------------------------------------------------------------------+
" | The following routine causes a non-printing keypress to be generated when    |
" | the cursor has been idle, hence causing a kind of 'timer' function.  Replaces|
" | previous 'cusor-movement' based one that hacked around with the cursor but   |
" | causes too many problems. Courtesey of Yukihiro Nakadaira. Source:-          |
" | http://old.nabble.com/timer-revisited-td8816391.html.                        |
" +------------------------------------------------------------------------------+
let g:K_IGNORE = "\x80\xFD\x35"   " internal key code that is ignored
autocmd CursorHold * call Timer()
function! Timer()
  call feedkeys(g:K_IGNORE)
endfunction 

" +------------------------------------------------------------------------------+
" | Main RGBEl function, used to work out amount to offset RGB value by to avoid |
" | it clashing with the background colour.                                      |
" | Return value is modified or otherwise value (not modified if no clash).      |
" +------------------------------------------------------------------------------+
:function RGBEl2(RGBEl,actBgr,dangerBgr,senDar,senLig,adjust)
:	if a:actBgr>=a:dangerBgr-a:senDar && a:actBgr<=a:dangerBgr+a:senLig
:		let whatdoyoucallit=a:dangerBgr-a:actBgr
:		if whatdoyoucallit<0
:			let whatdoyoucallit=-whatdoyoucallit
:		endif
:		let whatdoyoucallit=whatdoyoucallit/130
:		if whatdoyoucallit>255
:			let whatdoyoucallit=255
:		endif
:		let whatdoyoucallit=-whatdoyoucallit+255
:		let whatdoyoucallit=whatdoyoucallit*a:adjust
:		let whatdoyoucallit=whatdoyoucallit/800
:		let whatdoyoucallit=whatdoyoucallit+65
:		let adjustedValue=a:RGBEl-whatdoyoucallit
:	else
:		let adjustedValue=a:RGBEl
:	endif
:	let adjustedValue=adjustedValue-g:whiteadd
:	if adjustedValue<0
:		let adjustedValue=0
:	endif
:	if adjustedValue>255
:		let adjustedValue=255
:	endif
:	return adjustedValue
:endfunction

" +------------------------------------------------------------------------------+
" | Main RGBEl for Normal (like RGBEl2 but brightens, not darkens - Normal is    |
" | a bit trickier because it is also where the general background is set        |
" +------------------------------------------------------------------------------+
:function RGBEl2a(RGBEl,actBgr,dangerBgr,senDar,senLig,loadj,hiadj)
:	let localEase = 0
:	if a:actBgr>=a:dangerBgr-a:senDar && a:actBgr<=a:dangerBgr+a:senLig
:		let        progressFrom=a:dangerBgr-a:senDar
:		let        progressLoHi=a:actBgr-progressFrom
:		let            diffLoHi=(a:dangerBgr+a:senLig)-(a:dangerBgr-a:senDar)
:		let     progressPerThou=progressLoHi/(diffLoHi/1000)
:		let     ourinterestDiff=a:hiadj-a:loadj
:		let     weareScaleRatio=1000/ourinterestDiff
:		let            interest=progressPerThou/weareScaleRatio
:		let           interest2=interest+a:loadj
:		let       adjustedValue=a:RGBEl+interest2
:	elseif a:actBgr>=a:dangerBgr-a:senDar-localEase && a:actBgr<a:dangerBgr-a:senDar
:		let        progressFrom=a:dangerBgr-a:senDar-localEase
:		let        progressLoHi=a:actBgr-progressFrom
:		let            diffLoHi=(a:dangerBgr-a:senDar)-(a:dangerBgr-a:senDar-localEase)
:		let     progressPerThou=progressLoHi/(diffLoHi/1000)
:		let     ourinterestDiff=a:loadj
:		let     weareScaleRatio=1000/ourinterestDiff
:		let            interest=progressPerThou/weareScaleRatio
:		let           interest2=interest
:		let       adjustedValue=a:RGBEl+interest2
:	else
:		let adjustedValue=a:RGBEl
:	endif
:	let adjustedValue=adjustedValue-g:whiteadd
:	if adjustedValue<0
:		let adjustedValue=0
:	endif
:	if adjustedValue>255
:		let adjustedValue=255
:	endif
:	return adjustedValue
:endfunction

" +------------------------------------------------------------------------------+
" | Special RGBEl function for cases that needed special care. It provides       |
" | more control over the text's high or low lighting in the danger visibility   |
" | zone.                                                                        |
" +------------------------------------------------------------------------------+
:function RGBEl2b(RGBEl,actBgr,dangerBgr,senDar,senLig,adjust1,adjust2,adjust3)
:	if a:actBgr>=a:dangerBgr-a:senDar && a:actBgr<=a:dangerBgr+a:senLig
:		let        progressFrom=a:dangerBgr-a:senDar
:		let        progressLoHi=a:actBgr-progressFrom
:		let            diffLoHi=(a:dangerBgr+a:senLig)-(a:dangerBgr-a:senDar)
:		let     progressPerThou=progressLoHi/(diffLoHi/1000)
:		if progressPerThou<500
:			let     ourinterestDiff=a:adjust2-a:adjust1
:			let     weareScaleRatio=1000/ourinterestDiff
:			let            interest=progressPerThou/weareScaleRatio
:			let           interest2=interest+a:adjust1
:			let      adjustedAdjust=interest2
:		else
:			let     ourinterestDiff=a:adjust3-a:adjust2
:			let     weareScaleRatio=1000/ourinterestDiff
:			let            interest=progressPerThou/weareScaleRatio
:			let           interest2=interest+a:adjust2
:			let      adjustedAdjust=interest2
:		endif
:		let whatdoyoucallit=a:dangerBgr-a:actBgr
:		if whatdoyoucallit<0
:			let whatdoyoucallit=-whatdoyoucallit
:		endif
:		let whatdoyoucallit=whatdoyoucallit/130
:		if whatdoyoucallit>255
:			let whatdoyoucallit=255
:		endif
:		let whatdoyoucallit=-whatdoyoucallit+255
:		let whatdoyoucallit=whatdoyoucallit*adjustedAdjust
:		let whatdoyoucallit=whatdoyoucallit/800
:		let whatdoyoucallit=whatdoyoucallit+65
:		let adjustedValue=a:RGBEl-whatdoyoucallit
:	else
:		let adjustedValue=a:RGBEl
:	endif
:	let adjustedValue=adjustedValue-g:whiteadd
:	if adjustedValue<0
:		let adjustedValue=0
:	endif
:	if adjustedValue>255
:		let adjustedValue=255
:	endif
:	return adjustedValue
:endfunction

" +------------------------------------------------------------------------------+
" | RGBEl function for cursor to work out amount to offset RGB component to stop |
" | it from clashing with the background colour.                                 |
" | Return value is modified or otherwise value (not modified if no clash).      |
" +------------------------------------------------------------------------------+
:function RGBEl3(RGBEl,actBgr,dangerBgr,adj)
:	let diff=a:actBgr-a:dangerBgr
:	if diff<0
:		let diff=-diff
:	endif
:	if diff<8000
:		let adjustedValue=a:RGBEl-a:adj
:		if adjustedValue<0
:			let adjustedValue=0
:		endif
:	else
:		let adjustedValue=a:RGBEl
:	endif
:	return adjustedValue
:endfunction

" +------------------------------------------------------------------------------+
" | RGBEl function used to work out offsetting for RGB components pertaining to  |
" | a background, i.e. the bit that says guibg= of the vim highlight command.    |
" | Background is handled different to foreground so it needs another function.  |
" | You can tell this RGBEl function what to do with the background if RGB value |
" | is *just* below the 'danger' general background, and above it. In each case  |
" | you can tell it to brighten or darken the passed RGB value. (darkAdj,        |
" | lghtAdj params.) Positive values, (e.g. 40) add brightness, nagative values  |
" | remove it. Special cases 99 and -99 adds does this in a 'default' measure.   |
" | You can also tell the function what to do if the RGB value is right inside   |
" | the danger zone; not to be confused with darkAdj & lghtAdj that mean the     |
" | two end tips outside of the danger area. This bit is the danger area itself, |
" | the low-visisibility area, the 'danger zone'. (dangerZoneAdj) It works the   |
" | same, a positive value causes background to brighten, a negative to darken.  |
" | Like darkAdj & lghtAdj, you can also specify default 'brighten' or 'darken', |
" | 99 or -99 respectively, but if you're not happy with the default just fine   |
" | tune it exactly as you would like it to look exactly as you do with darkAdj  |
" | & lghtAdj. Use this if you find using the normal foreground text colour      |
" | modification by itself (RGBEl2 function) doesn't cut it. Text still looks    |
" | blurry over a certain background colour even after you've adjusted the danger|
" | adjustment parameters available in RGBEl2. Normally I found darkening text   |
" | with RGBEl2 adjustment params makes the text 'visible' over danger zone but  |
" | in some cases it wasn't up to it, so I added this param: 'dangerZoneAdj'.    |
" | This allows you to 'fudge' the background colour up and down as desired until|
" | you're happy with the result.                                                |
" | Return value is either the up or down-shifted RGB background element if the  |
" | element falls just outside the 'danger' boundary, a shifted-up RGB element   |
" | if the value is fully inside the danger boundary (and you set dangerZoneAdj) |
" | or simply the same as you pass if the value you pass is outside the danger   |
" | zone AND the outer boundary ring of the 'danger zone'.                       |
" +------------------------------------------------------------------------------+
:function RGBEl4(RGBEl,actBgr,dangerBgr,senDar,senLig,darkAdjLo,darkAdjHi,lghtAdjLo,lghtAdjHi,dangerZoneAdj)
:	let darkAdjLo=a:darkAdjLo
:	let darkAdjHi=a:darkAdjHi
:	let lghtAdjLo=a:lghtAdjLo
:	let lghtAdjHi=a:lghtAdjHi
:	if a:darkAdjLo==99
:		let darkAdjLo=11
:	endif
:	if a:darkAdjLo==-99
:		let darkAdjLo=-11
:	endif
:	if a:darkAdjHi==99
:		let darkAdjHi=11
:	endif
:	if a:darkAdjHi==-99
:		let darkAdjHi=-11
:	endif
:	if a:lghtAdjLo==99
:		let lghtAdjLo=11
:	endif
:	if a:lghtAdjLo==-99
:		let lghtAdjLo=-11
:	endif
:	if a:lghtAdjHi==99
:		let lghtAdjHi=11
:	endif
:	if a:lghtAdjHi==-99
:		let lghtAdjHi=-11
:	endif
:	let dangerZoneAdj=a:dangerZoneAdj
:	if a:dangerZoneAdj==99
:		let dangerZoneAdj=15
:	endif
:	if a:dangerZoneAdj==-99
:		let dangerZoneAdj=-15
:	endif
:	let adjustedValue=a:RGBEl
:	if a:actBgr>=a:dangerBgr-a:senDar-g:easeArea && a:actBgr<a:dangerBgr-a:senDar
:		let        progressFrom=a:dangerBgr-a:senDar-g:easeArea
:		let        progressLoHi=a:actBgr-progressFrom
:		let            diffLoHi=darkAdjHi-darkAdjLo
:		let  scaledProgressLoHi=diffLoHi*progressLoHi
:		let       adjustedValue=a:RGBEl+darkAdjLo+(scaledProgressLoHi/g:easeArea)
:	endif
:	if a:actBgr>a:dangerBgr+a:senLig && a:actBgr<=a:dangerBgr+a:senLig+g:easeArea
:		let        progressFrom=a:dangerBgr+a:senLig
:		let        progressLoHi=a:actBgr-progressFrom
:		let            diffLoHi=lghtAdjHi-lghtAdjLo
:		let  scaledProgressLoHi=diffLoHi*progressLoHi
:		let       adjustedValue=a:RGBEl+lghtAdjLo+(scaledProgressLoHi/g:easeArea)
:	endif
:	if a:actBgr>=(a:dangerBgr-a:senDar) && a:actBgr<=(a:dangerBgr+a:senLig) && dangerZoneAdj
:		let adjustedValue=adjustedValue+dangerZoneAdj
:	endif
:	if adjustedValue<0
:		let adjustedValue=0
:	endif
:	if adjustedValue>255
:		let adjustedValue=255
:	endif
:	return adjustedValue
:endfunction

" +------------------------------------------------------------------------------+
" | Special case of RGBEl function used particularly for the Normal highlight    |
" | element which obviously needs to be stronger because it's background cannot  |
" | be 'shifted' in bad visibility cases because Normal also happens to be the   |
" | the general background vim uses.                                             |
" +------------------------------------------------------------------------------+
:function RGBEl5(RGBEl,actBgr,dangerBgr,senDar,senLig)
:	if a:actBgr>=a:dangerBgr-a:senDar && a:actBgr<=a:dangerBgr+a:senLig
:		let adjustedValue=255
:	else
:		let adjustedValue=a:RGBEl
:	endif
:	if a:actBgr>=(a:dangerBgr+a:senLig)-21000 && a:actBgr<=a:dangerBgr+a:senLig
:		let adjustedValue=0
:	endif
:	if adjustedValue<0
:		let adjustedValue=0
:	endif
:	if adjustedValue>255
:		let adjustedValue=255
:	endif
:	return adjustedValue
:endfunction 

" +------------------------------------------------------------------------------+
" | This is a simple cut-off function disallowing values <0 and >255             |
" +------------------------------------------------------------------------------+
:function RGBEl6(RGBEl)
:	let result=a:RGBEl
:	if a:RGBEl<0
:		let result=0
:	endif
:	if a:RGBEl>255
:		let result=255
:	endif
:	return result
:endfunction

" +------------------------------------------------------------------------------+
" | This variable allows highlight to be inverted, i.e higher time = darker      |
" +------------------------------------------------------------------------------+
let highLowLightToggle=0

" +------------------------------------------------------------------------------+
" | Muscle function, calls vim highlight command for each element based on the   |
" | time into the current hour.                                                  |
" +------------------------------------------------------------------------------+
:function SetHighLight(nightorday)
:	let todaysec=((localtime()%(60*60)))*24
:	if exists("g:mytime")
:		let todaysec=g:mytime
:	endif
: 	let nightorday=a:nightorday
:	if nightorday==1
:		if todaysec<43199
:			let todaysec=-todaysec*2+86399
:			let dusk=0
:		else
:			let todaysec=(todaysec-43200)*2
:			let dusk=1
:		endif
:	else
:		if todaysec<43199
:			let todaysec=todaysec*2
:			let dusk=0
:		else
:			let todaysec=-todaysec*2+172799
:			let dusk=1
:		endif
:	endif
:	if exists("g:myhour")
:		let myhour=g:myhour
:	else
:		let myhour=(localtime()/(60*60))%3
:	endif
:	if (myhour==0 && dusk==0 && nightorday==0) || (myhour==2 && dusk==1 && nightorday==1)
:		let adjBG1=(todaysec<67000)?todaysec/380:(todaysec-43200)/271+96
:		let adjBG1A=(todaysec<67000)?todaysec/380:(todaysec-43200)/271+96
:		let adjBG2=(todaysec<67000)?todaysec/420:(todaysec-43200)/271+80
:	endif
:	if (myhour==0 && dusk==1 && nightorday==0) || (myhour==0 && dusk==0 && nightorday==1)
:		let adjBG1=(todaysec<67000)?todaysec/420:(todaysec-43200)/271+80
:		let adjBG1A=(todaysec<67000)?todaysec/380:(todaysec-43200)/271+96
:		let adjBG2=(todaysec<67000)?todaysec/420:(todaysec-43200)/271+80
:	endif
:	if (myhour==1 && dusk==0 && nightorday==0) || (myhour==0 && dusk==1 && nightorday==1)
:		let adjBG1=(todaysec<67000)?todaysec/380:(todaysec-43200)/271+96
:		let adjBG1A=(todaysec<67000)?todaysec/420:(todaysec-43200)/271+80
:		let adjBG2=(todaysec<67000)?todaysec/420:(todaysec-43200)/271+80
:	endif
:	if (myhour==1 && dusk==1 && nightorday==0) || (myhour==1 && dusk==0 && nightorday==1)
:		let adjBG1=(todaysec<67000)?todaysec/420:(todaysec-43200)/271+80
:		let adjBG1A=(todaysec<67000)?todaysec/420:(todaysec-43200)/271+80
:		let adjBG2=(todaysec<67000)?todaysec/380:(todaysec-43200)/271+96
:	endif
:	if (myhour==2 && dusk==0 && nightorday==0) || (myhour==1 && dusk==1 && nightorday==1)
:		let adjBG1=(todaysec<67000)?todaysec/420:(todaysec-43200)/271+80
:		let adjBG1A=(todaysec<67000)?todaysec/380:(todaysec-43200)/271+96
:		let adjBG2=(todaysec<67000)?todaysec/380:(todaysec-43200)/271+96
:	endif
:	if (myhour==2 && dusk==1 && nightorday==0) || (myhour==2 && dusk==0 && nightorday==1)
:		let adjBG1=(todaysec<67000)?todaysec/380:(todaysec-43200)/271+96
:		let adjBG1A=(todaysec<67000)?todaysec/420:(todaysec-43200)/271+80
:		let adjBG2=(todaysec<67000)?todaysec/380:(todaysec-43200)/271+96
:	endif
:	let g:whiteadd=(todaysec-60000)/850
:	if g:whiteadd>0
:		let adjBG1=adjBG1+g:whiteadd
:		if adjBG1>255
:			let adjBG1=255
:		endif
:		let adjBG1A=adjBG1A+g:whiteadd
:		if adjBG1A>255
:			let adjBG1A=255
:		endif
:		let adjBG2=adjBG2+g:whiteadd
:		if adjBG2>255
:			let adjBG2=255
:		endif
:	else
:		let g:whiteadd=0
:	endif
:	let adjBG3=(adjBG1-32>=32)?adjBG1-6-(g:whiteadd/3):adjBG1+9-(g:whiteadd/3)
:	let adjBG4=(adjBG1A-32>=32)?adjBG1A-6-(g:whiteadd/3):adjBG1A+9-(g:whiteadd/3)
:	let adjBG5a=(adjBG2-32>=32)?adjBG2-6-(g:whiteadd/3):adjBG2+9-(g:whiteadd/3)
:       let hA=printf("highlight Normal guibg=#%02x%02x%02x",					adjBG1,adjBG1A,adjBG2)
:	let adj1=	RGBEl2(adjBG1+40,							todaysec,80000,5500,6400,60)
:	let adj2=	RGBEl2(adjBG1+40,							todaysec,80000,5500,6400,60)
:	let adj3=	RGBEl2(adjBG1+40,							todaysec,80000,5500,6400,60)
:       let hA1=printf("highlight Folded guibg=#%02x%02x%02x guifg=#%02x%02x%02x",		adjBG1,adjBG1,adjBG4,adj1,adj2,adj3)
:       let hA2=printf("highlight CursorLine guibg=#%02x%02x%02x",				adjBG3,adjBG4,adjBG5a) 
:       let hA3=printf("highlight NonText guibg=#%02x%02x%02x guifg=#%02x%02x%02x",		adjBG3,adjBG1,adjBG1,adjBG3,adjBG1,adjBG1)  
:       let hA4=printf("highlight LineNr guibg=#%02x%02x%02x",					adjBG3,adjBG4,adjBG5a)
:	let adj1=	RGBEl4(adjBG1-30,							todaysec,0,0,10000,20,20,40,20,40)
:	let adj2=	RGBEl4(adjBG1A-10,							todaysec,0,0,10000,20,20,40,20,40)
:	let adj3=	RGBEl4(adjBG2+10,							todaysec,0,0,10000,20,20,40,20,40)
:       let hA5=printf("highlight Search guibg=#%02x%02x%02x",					adj1,adj2,adj3) 
:	let adj1=	RGBEl2(adjBG1,								todaysec,86399,4000,1,40)
:	let adj2=	RGBEl2(adjBG1A+30,							todaysec,86399,4000,1,40)
:	let adj3=	RGBEl2(adjBG2,								todaysec,86399,4000,1,40)
:	let hA6=printf("highlight DiffAdd guibg=#%02x%02x%02x",					adj1,adj2,adj3)
:	let adj1=	RGBEl2(adjBG1+30,							todaysec,86399,4000,1,40)
:	let adj2=	RGBEl2(adjBG1A,								todaysec,86399,4000,1,40)
:	let adj3=	RGBEl2(adjBG2,								todaysec,86399,4000,1,40)
:	let hA7=printf("highlight DiffDelete guibg=#%02x%02x%02x",				adj1,adj2,adj3)
:	let adj1=	RGBEl2(adjBG1+30,							todaysec,86399,4000,1,40)
:	let adj2=	RGBEl2(adjBG1A+30,							todaysec,86399,4000,1,40)
:	let adj3=	RGBEl2(adjBG2,								todaysec,86399,4000,1,40)
:	let hA8=printf("highlight DiffChange guibg=#%02x%02x%02x",				adj1,adj2,adj3)
:	let adj1=	RGBEl2(adjBG1,								todaysec,86399,4000,1,40)
:	let adj2=	RGBEl2(adjBG1A,								todaysec,86399,4000,1,40)
:	let adj3=	RGBEl2(adjBG2+30,							todaysec,86399,4000,1,40)
:	let hA9=printf("highlight DiffText guibg=#%02x%02x%02x",				adj1,adj2,adj3)
:	let adj1	=RGBEl2a((-todaysec+86400)/338/4+160,					todaysec,50000,9000,16000,-73,-42)
:	let adj2	=RGBEl2a((-todaysec+86400)/338/4+76,					todaysec,50000,9000,16000,-73,-42)
:	let adj3	=RGBEl2a((-todaysec+86400)/338/4+23,					todaysec,50000,9000,16000,-73,-42)
:	let adj4	=RGBEl4(adjBG1,								todaysec,50000,9000,16000,-4,-11,-2,0,0)
:	let adj5	=RGBEl4(adjBG1A,							todaysec,50000,9000,16000,-4,-11,-2,0,0)
:	let adj6	=RGBEl4(adjBG2,								todaysec,50000,9000,16000,-4,-11,-2,0,0)
:	let hB=printf("highlight Statement guifg=#%02x%02x%02x guibg=#%02x%02x%02x",		adj1,adj2,adj3,adj4,adj5,adj6)
:	let adjBG5=(todaysec<43200)?todaysec/338/2:todaysec/450+63
:	let hB1=printf("highlight VertSplit guifg=#%02x%02x%02x",				adjBG3,adjBG3,adjBG5)
:	let adj1=	RGBEl2((-todaysec+86400)/338/2+40,					todaysec,44000,8000,20000,100)
:	let adj2=	RGBEl2((-todaysec+86400)/338/2+54,					todaysec,44000,8000,20000,100)
:	let adj3=	RGBEl2((-todaysec+86400)/338/2+80,					todaysec,44000,8000,20000,100)
:       let hB2=printf("highlight LineNr guifg=#%02x%02x%02x",					adj1,adj2,adj3)  
:	let adj1=	RGBEl2a((-todaysec+86400)/400/2+27,					todaysec,46500,28000,16000,-117,-50)
:	let adj2=	RGBEl2a((-todaysec+86400)/400/2+110,					todaysec,46500,28000,16000,-117,-50)
:	let adj4=	RGBEl4(adjBG1,								todaysec,46500,28000,16000,-5,-10,-3,-2,4)
:	let adj5=	RGBEl4(adjBG1A,								todaysec,46500,28000,16000,-5,-10,-3,-2,4)
:	let adj6=	RGBEl4(adjBG2,								todaysec,46500,28000,16000,-5,-10,-3,-2,4)
:	let hC=printf("highlight Constant guifg=#%02x%02x%02x guibg=#%02x%02x%02x",		adj1,adj1,adj2,adj4,adj5,adj6)
:	let hC1=printf("highlight JavaScriptValue guifg=#%02x%02x%02x guibg=#%02x%02x%02x",	adj1,adj1,adj2,adj4,adj5,adj6)
:	let adj1=	RGBEl2a((-todaysec+86400)/338/2+110,					todaysec,35000,9600,51400,-137,30)
:	let adj2=	RGBEl2a((-todaysec+86400)/338/2+76,					todaysec,35000,0100,51400,-137,30)
:	let adj3=	RGBEl2a((-todaysec+86400)/338/2,					todaysec,35000,0100,51400,-137,30)
:	let hD=printf("highlight Normal guifg=#%02x%02x%02x gui=NONE",				adj1,adj2,adj3)
:	let adj1=	RGBEl2a((-todaysec+86400)/365/2+66,					todaysec,57000,16000,15000,-115,-45)
:	let adj2=	RGBEl2a((-todaysec+86400)/365/2+97,					todaysec,57000,16000,15000,-115,-45)
:	let adj3=	RGBEl2a((-todaysec+86400)/355/2,					todaysec,57000,16000,15000,-115,-45)
:	let adj4=	RGBEl4(adjBG1-5,							todaysec,57000,16000,18000,-2,-8,0,1,2)
:	let adj5=	RGBEl4(adjBG1A-5,							todaysec,57000,16000,18000,-2,-8,0,1,2)
:	let adj6=	RGBEl4(adjBG2-5,							todaysec,57000,16000,18000,-2,-8,0,1,2)
:	let hE=printf("highlight Identifier guifg=#%02x%02x%02x guibg=#%02x%02x%02x",		adj1,adj2,adj3,adj4,adj5,adj6) 
:	let adj1=	RGBEl2((-todaysec+86400)/355/2+97,					todaysec,43000,8000,16000,50)
:	let adj2=	RGBEl2((-todaysec+86400)/355/2+0,					todaysec,43000,8000,16000,50)
:	let adj3=	RGBEl2((-todaysec+86400)/355/2+137,					todaysec,43000,8000,16000,50)
:	let adj4=	RGBEl4(adjBG1,								todaysec,43000,8000,16000,-99,-99,0,0,0)
:	let adj5=	RGBEl4(adjBG1A,								todaysec,43000,8000,16000,-99,-99,0,0,0)
:	let adj6=	RGBEl4(adjBG2,								todaysec,43000,8000,16000,-99,-99,0,0,0)
:	let hF=printf("highlight PreProc guifg=#%02x%02x%02x gui=bold guibg=#%02x%02x%02x",	adj1,adj2,adj3,adj4,adj5,adj6)
:	let adj1=	RGBEl2((-todaysec+86400)/600/4+187,					todaysec,57000,16000,15000,68)
:	let adj2=	RGBEl2((-todaysec+86400)/600/4+95,					todaysec,57000,16000,15000,68)
:	let adj3=	RGBEl2((-todaysec+86400)/600/4+155,					todaysec,57000,16000,15000,68)
:	let adj4=	RGBEl4(adjBG1,								todaysec,57000,16000,15000,-2,-5,-6,-2,0)
:	let adj5=	RGBEl4(adjBG1A,								todaysec,57000,16000,15000,-2,-5,-6,-2,0)
:	let adj6=	RGBEl4(adjBG2,								todaysec,57000,16000,15000,-2,-5,-6,-2,0)
:	let hG=printf("highlight Special guifg=#%02x%02x%02x gui=bold guibg=#%02x%02x%02x",		adj1,adj2,adj3,adj4,adj5,adj6) 
:	let hG1=printf("highlight JavaScriptParens guifg=#%02x%02x%02x gui=bold guibg=#%02x%02x%02x",	adj1,adj2,adj3,adj4,adj5,adj6) 
:	let adj1=	RGBEl2((-todaysec+86400)/338/2+120,					todaysec,47000,3000,14000,64)
:	let adj2=	RGBEl2((-todaysec+86400)/338/2+10,					todaysec,47000,3000,14000,64)
:	let adj3=	RGBEl2((-todaysec+86400)/338/2+80,					todaysec,47000,3000,14000,64)
:	let adj4=	RGBEl4(adjBG1,								todaysec,47000,3000,14000,-99,-99,-99,-99,99)
:	let adj5=	RGBEl4(adjBG1A,								todaysec,47000,3000,14000,-99,-99,-99,-99,99)
:	let adj6=	RGBEl4(adjBG2,								todaysec,47000,3000,14000,-99,-99,-99,-99,99)
:       let hH=printf("highlight Title guifg=#%02x%02x%02x guibg=#%02x%02x%02x",		adj1,adj2,adj3,adj4,adj5,adj6) 
:	let adj1=	RGBEl2b((-todaysec+86400)/338/4+110,					todaysec,50000,5000,22000,40,0,-300)
:	let adj2=	RGBEl2b((-todaysec+86400)/338/4+110,					todaysec,50000,5000,22000,40,0,-300)
:	let adj3=	RGBEl2b((-todaysec+86400)/338/4+110,					todaysec,50000,5000,22000,40,0,-300)
:	let adj4=	RGBEl4(adjBG1,								todaysec,50000,5000,22000,-5,-18,0,0,-3)
:	let adj5=	RGBEl4(adjBG1A,								todaysec,50000,5000,22000,-5,-18,0,0,-3)
:	let adj6=	RGBEl4(adjBG2,								todaysec,50000,5000,22000,-5,-18,0,0,-3)
:	let hI=printf("highlight Comment guifg=#%02x%02x%02x guibg=#%02x%02x%02x",		adj1,adj2,adj3,adj4,adj5,adj6)
:	let hI1=printf("highlight htmlComment guifg=#%02x%02x%02x guibg=#%02x%02x%02x",		adj1,adj2,adj3,adj4,adj5,adj6)
:	let hI2=printf("highlight htmlCommentPart guifg=#%02x%02x%02x guibg=#%02x%02x%02x",	adj1,adj2,adj3,adj4,adj5,adj6)
:	let adj1=	RGBEl6(todaysec/338+70							)
:	let adj2=	RGBEl6(todaysec/338+30							)
:	let adj3=	RGBEl6(todaysec/338-100							)
:	let adj4=	RGBEl2a((-todaysec+86400)/338/2+70,					todaysec,35000,15000,14000,60,120)
:	let adj5=	RGBEl2a((-todaysec+86400)/338/2+60,					todaysec,35000,15000,14000,60,120)
:	let adj6=	RGBEl2a((-todaysec+86400)/338/2+0,					todaysec,35000,15000,14000,60,120)
:	let hJ=printf("highlight StatusLine guibg=#%02x%02x%02x guifg=#%02x%02x%02x gui=bold",	adj1,adj2,adj3,adj4,adj5,adj6)
:	let adj1=	RGBEl6(todaysec/338+70							)
:	let adj2=	RGBEl6(todaysec/338+60							)
:	let adj3=	RGBEl6(todaysec/338-100							)
:	let adj4=	RGBEl2a((-todaysec+86400)/338/2+70,					todaysec,20000,10000,14000,40,120)
:	let adj5=	RGBEl2a((-todaysec+86400)/338/2+0,					todaysec,20000,10000,14000,40,120)
:	let adj6=	RGBEl2a((-todaysec+86400)/338/2+0,					todaysec,20000,10000,14000,40,120)
:	let hK=printf("highlight StatusLineNC guibg=#%02x%02x%02x guifg=#%02x%02x%02x gui=bold",adj1,adj2,adj3,adj4,adj5,adj6)
:	let adj1=	RGBEl2((-todaysec+86400)/338/2,						todaysec,37000,27000,20000,40)
:	let adj2=	RGBEl2((-todaysec+86400)/338/2+20,					todaysec,37000,27000,20000,40)
:	let adj3=	RGBEl2((-todaysec+86400)/338/2+80,					todaysec,37000,27000,20000,40)
:	let adjBG6=(adjBG5-32>=0)?adjBG5-32:0
:	let hM=printf("highlight PMenu guibg=#%02x%02x%02x",					adjBG3,adjBG3,adjBG1)
:	let hN="highlight PMenuSel guibg=Yellow guifg=Blue"
:	let adj1=	RGBEl2(adjBG1+40,							todaysec,86399,4000,1,40)
:	let adj2=	RGBEl2(adjBG1A+15,							todaysec,86399,4000,1,40)
:	let adj3=	RGBEl2(adjBG2+10,							todaysec,86399,4000,1,40)
:	let hL=printf("highlight Visual guibg=#%02x%02x%02x",					adj1,adj2,adj3)
:	let adj1=	RGBEl2((-todaysec+86400)/338/2+150,					todaysec,60000,8000,13000,40)
:	let adj2=	RGBEl2((-todaysec+86400)/338/2+120,					todaysec,60000,8000,13000,40)
:	let adj3=	RGBEl2((-todaysec+86400)/338/2+0,					todaysec,60000,8000,13000,40)
:	let hO=printf("highlight Type guifg=#%02x%02x%02x",					adj1,adj2,adj3)
:	let adj1=RGBEl3(255,									todaysec,80000,0)
:	let adj2=RGBEl3(255,									todaysec,80000,255)
:	let adj3=RGBEl3(0,									todaysec,80000,0)
:	let hP=printf("highlight Cursor guibg=#%02x%02x%02x",					adj1,adj2,adj3)
:	let hP1=printf("highlight MatchParen guibg=#%02x%02x%02x",				adj1,adj2,adj3)
:	let adj1=	RGBEl2((-todaysec+86400)/338/2+100,					todaysec,44000,10000,26000,40)
:	let adj2=	RGBEl2((-todaysec+86400)/338/2+0,					todaysec,44000,10000,26000,40)
:	let adj3=	RGBEl2((-todaysec+86400)/338/2+180,					todaysec,44000,10000,26000,40)
:	let adj4=	RGBEl4(adjBG1,								todaysec,44000,10000,26000,-99,-99,-99,-99,99)
:	let adj5=	RGBEl4(adjBG1A,								todaysec,44000,10000,26000,-99,-99,-99,-99,99)
:	let adj6=	RGBEl4(adjBG2,								todaysec,44000,10000,26000,-99,-99,-99,-99,99)
:	let hQ=printf("highlight htmlLink guifg=#%02x%02x%02x guibg=#%02x%02x%02x",		adj1,adj2,adj3,adj4,adj5,adj6)
:	let adj1=	RGBEl2((-todaysec+86400)/338/2+220,					todaysec,77000,10000,26000,70)
:	let adj2=	RGBEl2((-todaysec+86400)/338/2+220,					todaysec,77000,10000,26000,70)
:	let adj3=	RGBEl2((-todaysec+86400)/338/2+0,					todaysec,77000,10000,26000,70)
:	let adj4=	RGBEl4(adjBG1,								todaysec,77000,10000,26000,-99,-99,-99,-99,99)
:	let adj5=	RGBEl4(adjBG1A,								todaysec,77000,10000,26000,-99,-99,-99,-99,99)
:	let adj6=	RGBEl4(adjBG2,								todaysec,77000,10000,26000,-99,-99,-99,-99,99)
:	let hR=printf("highlight Question guifg=#%02x%02x%02x guibg=#%02x%02x%02x",		adj1,adj2,adj3,adj4,adj5,adj6)
:	let hR1=printf("highlight MoreMsg guifg=#%02x%02x%02x guibg=#%02x%02x%02x",		adj1,adj2,adj3,adj4,adj5,adj6)
:	let adj1=	RGBEl2((-todaysec+86400)/338/2+100,					todaysec,66000,8000,8000,95)
:	let adj2=	RGBEl2((-todaysec+86400)/338/2+160,					todaysec,66000,8000,8000,95)
:	let adj3=	RGBEl2((-todaysec+86400)/338/2+0,					todaysec,66000,8000,8000,95)
:	let adj4=	RGBEl4(adjBG1,								todaysec,66000,8000,8000,-5,-5,10,3,5)
:	let adj5=	RGBEl4(adjBG1A,								todaysec,66000,8000,8000,-5,-5,10,3,5)
:	let adj6=	RGBEl4(adjBG2,								todaysec,66000,8000,8000,-5,-5,10,3,5)
:	let hS=printf("highlight Directory guifg=#%02x%02x%02x guibg=#%02x%02x%02x",		adj1,adj2,adj3,adj4,adj5,adj6)
:	if todaysec/g:changefreq!=s:oldactontime/g:changefreq || exists("g:mytime")
:		let s:oldactontime=todaysec
:		execute hA
:		execute hA1
:		execute hA2
:		execute hA3
:		execute hA4
:		execute hA5
:		execute hA6
:		execute hA7
:		execute hA8
:		execute hA9
:		execute hB
:		execute hB1
:		execute hB2
:		execute hC
:		execute hC1
:		execute hD
:		execute hE
:		execute hF
:		execute hG
:		execute hG1
:		execute hH
:		execute hI
:		execute hI1
:		execute hI2
:		execute hJ
:		execute hK
:		execute hL
:		execute hM
:		execute hN
:		execute hO
:		execute hP
:		execute hP1
:		execute hQ
:		execute hR
:		execute hR1
:		execute hS
:	endif
:	redraw
:	if exists("g:mytime") || exists("g:myhour")
:		echo "WARNING: debug is *on*"
:	endif
:endfunction       

" +------------------------------------------------------------------------------+
" | Wrapper function takes into account 'invert' global variable, used for doing |
" | an 'invert' effect when you enter/leave Insert. In case this was annoying    |
" | I left this off, but if you wanted to try it out simply change the second    |
" | call to SetHighLight() so that it calls it with 1 rather than 0.             |
" +------------------------------------------------------------------------------+
:function ExtraSetHighLight()
:	if g:highLowLightToggle==0
:		call SetHighLight(0)
:	else
:		call SetHighLight(0)
:	endif
:endfunction

au CursorHold * call ExtraSetHighLight()
au CursorHoldI * call ExtraSetHighLight()

" +------------------------------------------------------------------------------+
" | The following lines provide a invert when you go into and out of insert      |
" | mode. If you don't like this effect, just comment these two lines out!       |
" +------------------------------------------------------------------------------+
au InsertEnter * let g:highLowLightToggle=1 | call ExtraSetHighLight()
au InsertLeave * let g:highLowLightToggle=0 | call ExtraSetHighLight()

" +-----------------------------------------------------------------------------+
" | END                                                                         |
" +-----------------------------------------------------------------------------+
" | CHANGING COLOUR SCRIPT                                                      |
" +-----------------------------------------------------------------------------+

