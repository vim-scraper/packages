" +-----------------------------------------------------------------------------+
" | CHANGING COLOUR SCRIPT                                                      |
" +-----------------------------------------------------------------------------+
" | START                                                                       |
" +-----------------------------------------------------------------------------+
" | REVISONS:                                                                   |
" | THU 24TH JAN 2010:   13.9                                                   |
" |                      Made CursorColumn be the same as CursorLine, so if you |
" |                      happen to make use of CursorColumn at least it blends  |
" |                      in nicely with the colour scheme.                      |
" |                      $sParent = 'my mum'; <-- now look nice and bright      |
" | SUN 24TH JAN 2010:   13.8                                                   |
" |                      Strengthened Constant, and Normal so things like       |
" |                      like function names (e.g. CalculateScore()) look       |
" |                      bright and sharp everywhere now, and Constants like    |
" |                      $sParent = 'my mum'; <-- now look nice and bright      |
" |                      everywhere too.                                        |
" |                      13.7                                                   |
" |                      Strengthened PreProc, which gives colour to keywords   |
" |                      like 'function' and 'include_once' in php, and         |
" |                      'function' in other languages like ActionScript 2.0.   |
" | THU 21ST JAN 2010:   13.6                                                   |
" |                      Made Constant backgrounds a bit more helpful, so look- |
" |                      ing at your 'Hello mum' and 58, 99, and #ee will now be|
" |                      a lot more fun in all languages supported by vim.      |
" |                      13.5                                                   |
" |                      Made Statement a bit more focussed-looking at darker   |
" |                      backgrounds. This is typically a keyword such a return,|
" |                      for, if.. so it's pretty important that it be sharp.   |
" | WED 20TH JAN 2010:   13.4                                                   |
" |                      Did a spot of fine-tuning with Special, this is typic- |
" |                      ally the braces '{' and '}' in most langs, like PHP, C.|
" | TUE 19TH JAN 2010:   13.3                                                   |
" |                      Made Constant a bit brighter so it stands out better.  |
" | SUN 17TH JAN 2010:   13.2                                                   |
" |                      Sharpened-up PreProc. This is the 'function' keyword   |
" |                      in e.g. PHP & ActionScript. Was a bit fuzzy in certain |
" |                      areas.                                                 |
" | THU 14TH JAN 2010:   13.1                                                   |
" |                      Had one final fight with Type to make it visible and   |
" |                      ironed-out some bugs in Identifier.                    |
" |                      13.0                                                   |
" |                      Cosmetic improvements to Identifier.                   |
" |                      12.9                                                   |
" |                      Think finally ironed-out visibility issues with Type.  |
" |                      12.8                                                   |
" |                      Still a few teething visibility issues with Type.      |
" |                      Did a few more tweaks. Had got a bit carried away.     |
" |                      12.7                                                   |
" |                      Type still hard too see. Corrected this a bit more.    |
" |                      ...                                                    |
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

function ScaleToRange(a,aMin,aMax,rMin,rMax)
:	let aLocal=a:a-a:aMin
:	let rangeA=a:aMax-a:aMin
:	let rangeR=a:rMax-a:rMin
:	let divAR=rangeA/rangeR
:	return aLocal/divAR+a:rMin
endfunction

" +------------------------------------------------------------------------------+
" | Main RGBEl for Normal (like RGBEl2 but brightens, not darkens - Normal is    |
" | a bit trickier because it is also where the general background is set        |
" +------------------------------------------------------------------------------+
:function RGBEl2a(RGBEl,actBgr,dangerBgr,senDar,senLig,loadj,hiadj,lotail1,lotail2)
:	let adjustedValue=a:RGBEl
:	if a:actBgr>=a:dangerBgr-a:senDar && a:actBgr<=a:dangerBgr+a:senLig
:		let adjustedValue=adjustedValue+ScaleToRange(a:actBgr,a:dangerBgr-a:senDar,a:dangerBgr+a:senLig,a:loadj,a:hiadj)
:	endif
:	if a:actBgr>=0 && a:actBgr<=a:dangerBgr-a:senDar
:		let adjustedValue=adjustedValue+ScaleToRange(a:actBgr,0,a:dangerBgr-a:senDar,a:lotail1,a:lotail2)
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
:		let adjustedValue=adjustedValue+ScaleToRange(a:actBgr,a:dangerBgr-a:senDar-g:easeArea,a:dangerBgr-a:senDar,lghtAdjLo,lghtAdjHi)
:	endif
:	if a:actBgr>a:dangerBgr+a:senLig && a:actBgr<=a:dangerBgr+a:senLig+g:easeArea
:		let adjustedValue=adjustedValue+ScaleToRange(a:actBgr,a:dangerBgr+a:senLig,a:dangerBgr+a:senLig+g:easeArea,lghtAdjLo,lghtAdjHi)
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
" | Like REGEl4 but adding an additional control for fine-tuning the background  |
" | at various ranges (instead of one flat rate.)                                |
" +------------------------------------------------------------------------------+
:function RGBEl4a(RGBEl,actBgr,dangerBgr,senDar,senLig,darkAdjLo,darkAdjHi,lghtAdjLo,lghtAdjHi,dangerZoneAdj1,dangerZoneAdj2,ligdown1,ligdown2,ligup1,ligup2)
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
:	let dangerZoneAdj1=a:dangerZoneAdj1
:	if a:dangerZoneAdj1==99
:		let dangerZoneAdj1=15
:	endif
:	if a:dangerZoneAdj1==-99
:		let dangerZoneAdj1=-15
:	endif
:	let dangerZoneAdj2=a:dangerZoneAdj2
:	if a:dangerZoneAdj2==99
:		let dangerZoneAdj2=15
:	endif
:	if a:dangerZoneAdj2==-99
:		let dangerZoneAdj2=-15
:	endif
:	let adjustedValue=a:RGBEl
:	if a:actBgr>=a:dangerBgr-a:senDar-g:easeArea && a:actBgr<a:dangerBgr-a:senDar
:		let adjustedValue=adjustedValue+ScaleToRange(a:actBgr,a:dangerBgr-a:senDar-g:easeArea,a:dangerBgr-a:senDar,darkAdjLo,darkAdjHi)
:	endif
:	if a:actBgr>=0 && a:actBgr<a:dangerBgr-a:senDar-g:easeArea
:		let adjustedValue=adjustedValue+ScaleToRange(a:actBgr,0,a:dangerBgr-a:senDar-g:easeArea,a:ligdown1,a:ligdown2)
:	endif
:	if a:actBgr>a:dangerBgr+a:senLig && a:actBgr<=a:dangerBgr+a:senLig+g:easeArea
:		let adjustedValue=adjustedValue+ScaleToRange(a:actBgr,a:dangerBgr+a:senLig,a:dangerBgr+a:senLig+g:easeArea,lghtAdjLo,lghtAdjHi)
:	endif
:	if a:actBgr>a:dangerBgr+a:senLig+g:easeArea && a:actBgr<=86400
:		let adjustedValue=adjustedValue+ScaleToRange(a:actBgr,a:dangerBgr+a:senLig+g:easeArea,86400,a:ligup1,a:ligup2)
:	endif
:	if a:actBgr>=(a:dangerBgr-a:senDar) && a:actBgr<=(a:dangerBgr+a:senLig) && (dangerZoneAdj1 || dangerZoneAdj2)
:		let adjustedValue=adjustedValue+ScaleToRange(a:actBgr,a:dangerBgr-a:senDar,a:dangerBgr+a:senLig,dangerZoneAdj1,dangerZoneAdj2)
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
:       let hA2=printf("highlight CursorColumn guibg=#%02x%02x%02x",				adjBG3,adjBG4,adjBG5a) 
:       let hA2a=printf("highlight CursorLine guibg=#%02x%02x%02x",				adjBG3,adjBG4,adjBG5a) 
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
:	let adj1	=RGBEl2a((-todaysec+86400)/338/4+160,					todaysec,57000,22000,15000,-100,-42,0,12)
:	let adj2	=RGBEl2a((-todaysec+86400)/338/4+76,					todaysec,57000,22000,15000,-100,-42,0,12)
:	let adj3	=RGBEl2a((-todaysec+86400)/338/4+23,					todaysec,57000,22000,15000,-100,-42,0,12)
:	let adj4	=RGBEl4(adjBG1,								todaysec,57000,22000,15000,-4,-11,-2,0,0)
:	let adj5	=RGBEl4(adjBG1A,							todaysec,57000,22000,15000,-4,-11,-2,0,0)
:	let adj6	=RGBEl4(adjBG2,								todaysec,57000,22000,15000,-4,-11,-2,0,0)
:	let hB=printf("highlight Statement guifg=#%02x%02x%02x guibg=#%02x%02x%02x",		adj1,adj2,adj3,adj4,adj5,adj6)
:	let adjBG5=(todaysec<43200)?todaysec/338/2:todaysec/450+63
:	let hB1=printf("highlight VertSplit guifg=#%02x%02x%02x",				adjBG3,adjBG3,adjBG5)
:	let adj1=	RGBEl2((-todaysec+86400)/338/2+40,					todaysec,44000,8000,20000,100)
:	let adj2=	RGBEl2((-todaysec+86400)/338/2+54,					todaysec,44000,8000,20000,100)
:	let adj3=	RGBEl2((-todaysec+86400)/338/2+80,					todaysec,44000,8000,20000,100)
:       let hB2=printf("highlight LineNr guifg=#%02x%02x%02x",					adj1,adj2,adj3)  
:	let adj1=	RGBEl2a((-todaysec+86400)/400/2+27,					todaysec,46500,28000,16000,-110,-45,0,10)
:	let adj2=	RGBEl2a((-todaysec+86400)/400/2+110,					todaysec,46500,28000,16000,-110,-45,0,10)
:	let adj4=	RGBEl4a(adjBG1,								todaysec,46500,28000,16000,-5,-10,-3,-2,26,2,25,6,-4,-13)
:	let adj5=	RGBEl4a(adjBG1A,							todaysec,46500,28000,16000,-5,-10,-3,-2,26,2,25,6,-4,-13)
:	let adj6=	RGBEl4a(adjBG2,								todaysec,46500,28000,16000,-5,-10,-3,-2,26,2,25,6,-4,-13)
:	let hC=printf("highlight Constant guifg=#%02x%02x%02x guibg=#%02x%02x%02x",		adj1,adj1,adj2,adj4,adj5,adj6)
:	let hC1=printf("highlight JavaScriptValue guifg=#%02x%02x%02x guibg=#%02x%02x%02x",	adj1,adj1,adj2,adj4,adj5,adj6)
:	let adj1=	RGBEl2a((-todaysec+86400)/338/2+110,					todaysec,35000,9600,51400,-155,30,0,17)
:	let adj2=	RGBEl2a((-todaysec+86400)/338/2+76,					todaysec,35000,0100,51400,-155,30,0,17)
:	let adj3=	RGBEl2a((-todaysec+86400)/338/2,					todaysec,35000,0100,51400,-155,30,0,17)
:	let hD=printf("highlight Normal guifg=#%02x%02x%02x gui=NONE",				adj1,adj2,adj3)
:	let adj1=	RGBEl2a((-todaysec+86400)/365/2+66,					todaysec,57000,22000,15000,-122,-30,-6,0)
:	let adj2=	RGBEl2a((-todaysec+86400)/365/2+97,					todaysec,57000,22000,15000,-122,-30,-6,0)
:	let adj3=	RGBEl2a((-todaysec+86400)/355/2,					todaysec,57000,22000,15000,-122,-30,-6,0)
:	let adj4=	RGBEl4a(adjBG1,								todaysec,57000,22000,15000,-8,-12,5,0,7,3,6,-8,-2,-2)
:	let adj5=	RGBEl4a(adjBG1A,							todaysec,57000,22000,15000,-8,-12,5,0,7,3,6,-8,-2,-2)
:	let adj6=	RGBEl4a(adjBG2,								todaysec,57000,22000,15000,-8,-12,5,0,7,3,6,-8,-2,-2)
:	let hE=printf("highlight Identifier guifg=#%02x%02x%02x guibg=#%02x%02x%02x",		adj1,adj2,adj3,adj4,adj5,adj6) 
:	let adj1=	RGBEl2a((-todaysec+86400)/355/2+97,					todaysec,43000,14000,19000,-130,-75,0,0)
:	let adj2=	RGBEl2a((-todaysec+86400)/355/2+0,					todaysec,43000,14000,19000,-130,-75,0,0)
:	let adj3=	RGBEl2a((-todaysec+86400)/355/2+137,					todaysec,43000,14000,19000,-130,-75,0,0)
:	let adj4=	RGBEl4(adjBG1,								todaysec,43000,14000,19000,-99,-99,0,0,0)
:	let adj5=	RGBEl4(adjBG1A,								todaysec,43000,14000,19000,-99,-99,0,0,0)
:	let adj6=	RGBEl4(adjBG2,								todaysec,43000,14000,19000,-99,-99,0,0,0)
:	let hF=printf("highlight PreProc guifg=#%02x%02x%02x gui=bold guibg=#%02x%02x%02x",	adj1,adj2,adj3,adj4,adj5,adj6)
:	let adj1=	RGBEl2a((-todaysec+86400)/600/4+187,					todaysec,57000,22000,15000,-96,-68,0,0)
:	let adj2=	RGBEl2a((-todaysec+86400)/600/4+95,					todaysec,57000,22000,15000,-96,-68,0,0)
:	let adj3=	RGBEl2a((-todaysec+86400)/600/4+155,					todaysec,57000,22000,15000,-96,-68,0,0)
:	let adj4=	RGBEl4(adjBG1,								todaysec,57000,22000,15000,-2,-5,-5,-2,0)
:	let adj5=	RGBEl4(adjBG1A,								todaysec,57000,22000,15000,-2,-5,-5,-2,0)
:	let adj6=	RGBEl4(adjBG2,								todaysec,57000,22000,15000,-2,-5,-5,-2,0)
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
:	let adj4=	RGBEl2a((-todaysec+86400)/338/2+70,					todaysec,35000,15000,14000,60,120,0,0)
:	let adj5=	RGBEl2a((-todaysec+86400)/338/2+60,					todaysec,35000,15000,14000,60,120,0,0)
:	let adj6=	RGBEl2a((-todaysec+86400)/338/2+0,					todaysec,35000,15000,14000,60,120,0,0)
:	let hJ=printf("highlight StatusLine guibg=#%02x%02x%02x guifg=#%02x%02x%02x gui=bold",	adj1,adj2,adj3,adj4,adj5,adj6)
:	let adj1=	RGBEl6(todaysec/338+70							)
:	let adj2=	RGBEl6(todaysec/338+60							)
:	let adj3=	RGBEl6(todaysec/338-100							)
:	let adj4=	RGBEl2a((-todaysec+86400)/338/2+70,					todaysec,20000,10000,14000,40,120,0,0)
:	let adj5=	RGBEl2a((-todaysec+86400)/338/2+0,					todaysec,20000,10000,14000,40,120,0,0)
:	let adj6=	RGBEl2a((-todaysec+86400)/338/2+0,					todaysec,20000,10000,14000,40,120,0,0)
:	let hK=printf("highlight StatusLineNC guibg=#%02x%02x%02x guifg=#%02x%02x%02x gui=bold",adj1,adj2,adj3,adj4,adj5,adj6)
:	let adj1=	RGBEl2((-todaysec+86400)/338/2,						todaysec,37000,27000,20000,40)
:	let adj2=	RGBEl2((-todaysec+86400)/338/2+20,					todaysec,37000,27000,20000,40)
:	let adj3=	RGBEl2((-todaysec+86400)/338/2+80,					todaysec,37000,27000,20000,40)
:	let adjBG6=(adjBG5-32>=0)?adjBG5-32:0
:	let hM=printf("highlight PMenu guibg=#%02x%02x%02x",					adjBG3,adjBG3,adjBG1)
:	let hN="highlight PMenuSel guibg=Yellow guifg=Blue"
:	let adj1=	RGBEl2(adjBG1+40,							todaysec,86399,6000,1,30)
:	let adj2=	RGBEl2(adjBG1A+15,							todaysec,86399,6000,1,30)
:	let adj3=	RGBEl2(adjBG2+10,							todaysec,86399,6000,1,30)
:	let hL=printf("highlight Visual guibg=#%02x%02x%02x",					adj1,adj2,adj3)
:	let adj1=	RGBEl2a((-todaysec+86400)/338/2+150,					todaysec,60000,11000,13000,-90,-45,-50,-20)
:	let adj2=	RGBEl2a((-todaysec+86400)/338/2+120,					todaysec,60000,11000,13000,-90,-45,-50,-20)
:	let adj3=	RGBEl2a((-todaysec+86400)/338/2+0,					todaysec,60000,11000,13000,-90,-45,-50,-20)
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
:		execute hA2a
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

