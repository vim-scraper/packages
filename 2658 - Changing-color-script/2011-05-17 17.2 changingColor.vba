" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/changingColor.vim	[[[1
669
" +-----------------------------------------------------------------------------+
" | CHANGING COLOR SCRIPT                                                       |
" +-----------------------------------------------------------------------------+
" | Changes syntax highlight colors gradually every little while to reflect the |
" | passing by of the hour.  Note: this script needs the Timer script in order  |
" | to work properly, Timer should be installed alongside.  Use Vimball if you  |
" | are not sure, as Vimball installs Timer.                                    |
" +-----------------------------------------------------------------------------+
" | START                                                                       |
" +-----------------------------------------------------------------------------+
" | TUE  17TH MAY 2011:  17.2                                                   |
" |                      Made foreground NonText color same as that of Constant |
" |                      (prettier, not like that of ugly Normal, as I had b4)  |
" | TUE  17TH MAY 2011:  17.1                                                   |
" |                      You can now see the NonText character, before I had    |
" |                      it so that you couldn't                                |
" |                      17.0                                                   |
" |                      Stopped Question and MoreMsg becoming invisible;       |
" |                      visibility tweaks to NonText.                          |
" | TUE  26TH APR 2011:  16.9                                                   |
" |                      Brightened up Special an Identifier under low light    |
" | MON  25TH APR 2011:  16.8                                                   |
" |                      Unconfused mixup with fg and bg for Search             |
" |                      and Visual, i got fg and bg wrong way.  Fixed and seems|
" |                      ok now.                                                |
" | SUN  24TH APR 2011:  16.7                                                   |
" |                      Collated temporary variables into one single variable  |
" |                      and used this.  Tweaked poor contrast of Search and    |
" |                      Visual syntax elements.  Made code formatting more     |
" |                      normal.                                                |
" | WED  19TH JAN 2011:  16.6                                                   |
" |                      Removed what appears to be a glitch at lighter end of  |
" |                      the background spectrum.  This affected selections     |
" |                      of text done at the very lightest background, they     |
" |                      were invisible.  It now works as it should.            |
" | MON   3RD NOV 2010:  16.5                                                   |
" |                      Revved up the visibility of PreProc. PreProc is the    |
" |                      'function' keyword in php so it now brightens nicer    |
" |                      and doesn't become hard-to-see. Similarly with         |
" |                      Identifier, and Special.  Identifier in PHP obviously  |
" |                      means the variables. Not sure if its used in other     |
" |                      languages but at least if you use PHP it now looks nice|
" |                      and strong.                                            |
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
" | to every minute, 5760 every two minutes, 4320 is ~ every 1.5 minutes         |
" +------------------------------------------------------------------------------+
let g:changefreq=720

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
" | Main Rgb function, used to work out amount to offset Rgb value by to avoid |
" | it clashing with the background colour.                                      |
" | Return value is modified or otherwise value (not modified if no clash).      |
" +------------------------------------------------------------------------------+
:function Rgb2(Rgb, actBgr, dangerBgr, senDar, senLig, adjust)
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
:		let adjustedValue=a:Rgb-whatdoyoucallit
:	else
:		let adjustedValue=a:Rgb
:	endif
:	if adjustedValue<0
:		let adjustedValue=0
:	endif
:	if adjustedValue>255
:		let adjustedValue=255
:	endif
:	return adjustedValue
:endfunction

" takes a within range aMin,aMin outputs a mapped to rMin, rMin
function ScaleToRange(a,aMin,aMax,rMin,rMax)
:	let aLocal=a:a-a:aMin
:	let rangeA=a:aMax-a:aMin
:	let rangeR=a:rMax-a:rMin
:	let divAR=rangeA/rangeR
:	return aLocal/divAR+a:rMin
endfunction

" +------------------------------------------------------------------------------+
" | Main Rgb for Normal (like Rgb2 but brightens, not darkens - Normal is    |
" | a bit trickier because it is also where the general background is set        |
" +------------------------------------------------------------------------------+
:function Rgb2a(Rgb,actBgr,dangerBgr,senDar,senLig,loadj,hiadj,lotail1,lotail2)
:	let adjustedValue=a:Rgb
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
" | Special Rgb function for cases that needed special care. It provides       |
" | more control over the text's high or low lighting in the danger visibility   |
" | zone.                                                                        |
" +------------------------------------------------------------------------------+
:function Rgb2b(Rgb,actBgr,dangerBgr,senDar,senLig,adjust1,adjust2,adjust3)
:	if a:actBgr>=a:dangerBgr-a:senDar && a:actBgr<=a:dangerBgr+a:senLig
:		let        progressFrom=a:dangerBgr-a:senDar
:		let        progressLoHi=a:actBgr-progressFrom
:		let            diffLoHi=(a:dangerBgr+a:senLig)-(a:dangerBgr-a:senDar)
:		let     progressPerThou=progressLoHi/(diffLoHi/1000)
:		if progressPerThou<500
:			let adjustedAdjust=ScaleToRange(a:actBgr,a:dangerBgr-a:senDar,((a:dangerBgr-a:senDar)+(a:dangerBgr+a:senLig))/2,a:adjust1,a:adjust2)
:		else
:			let adjustedAdjust=ScaleToRange(a:actBgr,((a:dangerBgr-a:senDar)+(a:dangerBgr+a:senLig))/2,a:dangerBgr+a:senLig,a:adjust2,a:adjust3)
:		endif
:		let proximity=a:dangerBgr-a:actBgr
:		if proximity<0
:			let proximity=-proximity
:		endif
:		let proximity=proximity/130
:		if proximity>255
:			let proximity=255
:		endif
:		let proximity=-proximity+255
:		let proximity=proximity*adjustedAdjust
:		let proximity=proximity/800
:		let proximity=proximity+65
:		let adjustedValue=a:Rgb-proximity
:	else
:		let adjustedValue=a:Rgb
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
" | Rgb function for cursor to work out amount to offset Rgb component to stop |
" | it from clashing with the background colour.                                 |
" | Return value is modified or otherwise value (not modified if no clash).      |
" +------------------------------------------------------------------------------+
:function Rgb3(Rgb,actBgr,dangerBgr,adj)
:	let diff=a:actBgr-a:dangerBgr
:	if diff<0
:		let diff=-diff
:	endif
:	if diff<12000
:		let adjustedValue=a:Rgb-a:adj
:		if adjustedValue<0
:			let adjustedValue=0
:		endif
:	else
:		let adjustedValue=a:Rgb
:	endif
:	return adjustedValue
:endfunction

" +------------------------------------------------------------------------------+
" | Rgb function used to work out offsetting for Rgb components pertaining to  |
" | a background, i.e. the bit that says guibg= of the vim highlight command.    |
" | Background is handled different to foreground so it needs another function.  |
" | You can tell this Rgb function what to do with the background if Rgb value |
" | is *just* below the 'danger' general background, and above it. In each case  |
" | you can tell it to brighten or darken the passed Rgb value. (darkAdj,        |
" | lghtAdj params.) Positive values, (e.g. 40) add brightness, nagative values  |
" | remove it. Special cases 99 and -99 adds does this in a 'default' measure.   |
" | You can also tell the function what to do if the Rgb value is right inside   |
" | the danger zone; not to be confused with darkAdj & lghtAdj that mean the     |
" | two end tips outside of the danger area. This bit is the danger area itself, |
" | the low-visisibility area, the 'danger zone'. (dangerZoneAdj) It works the   |
" | same, a positive value causes background to brighten, a negative to darken.  |
" | Like darkAdj & lghtAdj, you can also specify default 'brighten' or 'darken', |
" | 99 or -99 respectively, but if you're not happy with the default just fine   |
" | tune it exactly as you would like it to look exactly as you do with darkAdj  |
" | & lghtAdj. Use this if you find using the normal foreground text colour      |
" | modification by itself (Rgb2 function) doesn't cut it. Text still looks    |
" | blurry over a certain background colour even after you've adjusted the danger|
" | adjustment parameters available in Rgb2. Normally I found darkening text   |
" | with Rgb2 adjustment params makes the text 'visible' over danger zone but  |
" | in some cases it wasn't up to it, so I added this param: 'dangerZoneAdj'.    |
" | This allows you to 'fudge' the background colour up and down as desired until|
" | you're happy with the result.                                                |
" | Return value is either the up or down-shifted Rgb background element if the  |
" | element falls just outside the 'danger' boundary, a shifted-up Rgb element   |
" | if the value is fully inside the danger boundary (and you set dangerZoneAdj) |
" | or simply the same as you pass if the value you pass is outside the danger   |
" | zone AND the outer boundary ring of the 'danger zone'.                       |
" +------------------------------------------------------------------------------+
:function Rgb4(Rgb,actBgr,dangerBgr,senDar,senLig,darkAdjLo,darkAdjHi,lghtAdjLo,lghtAdjHi,dangerZoneAdj)
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
:	let adjustedValue=a:Rgb
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
:function Rgb4a(Rgb,actBgr,dangerBgr,senDar,senLig,darkAdjLo,darkAdjHi,lghtAdjLo,lghtAdjHi,dangerZoneAdj1,dangerZoneAdj2,ligdown1,ligdown2,ligup1,ligup2)
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
:	let adjustedValue=a:Rgb
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
" | Special case of Rgb function used particularly for the Normal highlight    |
" | element which obviously needs to be stronger because it's background cannot  |
" | be 'shifted' in bad visibility cases because Normal also happens to be the   |
" | the general background vim uses.                                             |
" +------------------------------------------------------------------------------+
:function Rgb5(Rgb,actBgr,dangerBgr,senDar,senLig)
:	if a:actBgr>=a:dangerBgr-a:senDar && a:actBgr<=a:dangerBgr+a:senLig
:		let adjustedValue=255
:	else
:		let adjustedValue=a:Rgb
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
:function Rgb6(Rgb)
:	let result=a:Rgb
:	if a:Rgb<0
:		let result=0
:	endif
:	if a:Rgb>255
:		let result=255
:	endif
:	return result
:endfunction

" +------------------------------------------------------------------------------+
" | Muscle function, calls vim highlight command for each element based on the   |
" | time into the current hour.                                                  |
" +------------------------------------------------------------------------------+
:function SetHighLight()
:	let todaysec=(((localtime()+1800)%(60*60)))*24
:	if exists("g:mytime")
:		let todaysec=g:mytime
:	endif
:	if s:oldactontime/g:changefreq==todaysec/g:changefreq
:		return
:	endif
:	let s:oldactontime=todaysec
:	if todaysec<43199
:		let todaysec=todaysec*2
:		let dusk=0
:	else
:		let todaysec=-todaysec*2+172799
:		let dusk=1
:	endif
:	if exists("g:myhour")
:		let myhour=g:myhour
:	else
:		let myhour=(localtime()/(60*60))%3
:	endif
:	if (myhour==0 && dusk==0)
:		let adjBG1=(todaysec<67000)?todaysec/380:(todaysec-43200)/271+96
:		let adjBG1A=(todaysec<67000)?todaysec/380:(todaysec-43200)/271+96
:		let adjBG2=(todaysec<67000)?todaysec/420:(todaysec-43200)/271+80
:	endif
:	if (myhour==0 && dusk==1)
:		let adjBG1=(todaysec<67000)?todaysec/420:(todaysec-43200)/271+80
:		let adjBG1A=(todaysec<67000)?todaysec/380:(todaysec-43200)/271+96
:		let adjBG2=(todaysec<67000)?todaysec/420:(todaysec-43200)/271+80
:	endif
:	if (myhour==1 && dusk==0)
:		let adjBG1=(todaysec<67000)?todaysec/380:(todaysec-43200)/271+96
:		let adjBG1A=(todaysec<67000)?todaysec/420:(todaysec-43200)/271+80
:		let adjBG2=(todaysec<67000)?todaysec/420:(todaysec-43200)/271+80
:	endif
:	if (myhour==1 && dusk==1)
:		let adjBG1=(todaysec<67000)?todaysec/420:(todaysec-43200)/271+80
:		let adjBG1A=(todaysec<67000)?todaysec/420:(todaysec-43200)/271+80
:		let adjBG2=(todaysec<67000)?todaysec/380:(todaysec-43200)/271+96
:	endif
:	if (myhour==2 && dusk==0)
:		let adjBG1=(todaysec<67000)?todaysec/420:(todaysec-43200)/271+80
:		let adjBG1A=(todaysec<67000)?todaysec/380:(todaysec-43200)/271+96
:		let adjBG2=(todaysec<67000)?todaysec/380:(todaysec-43200)/271+96
:	endif
:	if (myhour==2 && dusk==1)
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
:	let temp1 = adjBG1-(g:whiteadd/3)
:	let temp2 = adjBG1A-(g:whiteadd/3)
:	let temp3 = adjBG2-(g:whiteadd/3)
:	let adjBG3 = (todaysec>=18500)?temp1-ScaleToRange(adjBG1,54,255,4,17):adjBG1+ScaleToRange(adjBG1,0,54,32,12)
:	let adjBG4 = (todaysec>=18500)?temp2-ScaleToRange(adjBG1A,54,255,4,17):adjBG1A+ScaleToRange(adjBG1A,0,54,32,26)
:	let adjBG5a = (todaysec>=18500)?temp3-ScaleToRange(adjBG2,54,255,4,17):adjBG2+ScaleToRange(adjBG2,0,54,32,26)
:       let highlCmd = printf("highlight Normal guibg=#%02x%02x%02x", adjBG1,adjBG1A,adjBG2)
:	execute highlCmd
:	let adj1 = Rgb2(adjBG1+40, todaysec, 80000, 5500, 6400, 60)
:	let adj2 = Rgb2(adjBG1+40, todaysec, 80000, 5500, 6400, 60)
:	let adj3 = Rgb2(adjBG1+40, todaysec, 80000, 5500, 6400, 60)
:       let highlCmd = printf("highlight Folded guibg=#%02x%02x%02x", adjBG3, adjBG4, adjBG5a)
:	execute highlCmd
:       let highlCmd = printf("highlight CursorColumn guibg=#%02x%02x%02x", adjBG3, adjBG4, adjBG5a) 
:	execute highlCmd
:       let highlCmd = printf("highlight CursorLine guibg=#%02x%02x%02x", adjBG3,adjBG4,adjBG5a) 
:	execute highlCmd
:       let highlCmd = printf("highlight LineNr guibg=#%02x%02x%02x", adjBG3, adjBG4, adjBG5a)
:	execute highlCmd
:       let highlCmd = printf("highlight FoldColumn guibg=#%02x%02x%02x", adjBG3, adjBG4, adjBG5a)
:	execute highlCmd
:	let adj1 = Rgb2(adjBG1,	todaysec, 86399, 4000, 1, 40)
:	let adj2 = Rgb2(adjBG1A+30, todaysec,86399,4000,1,40)
:	let adj3 = Rgb2(adjBG2,	todaysec,86399,4000,1,40)
:	let highlCmd = printf("highlight DiffAdd guibg=#%02x%02x%02x", adj1, adj2, adj3)
:	execute highlCmd
:	let adj1 = Rgb2(adjBG1+30, todaysec, 86399, 4000, 1, 40)
:	let adj2 = Rgb2(adjBG1A, todaysec, 86399, 4000, 1, 40)
:	let adj3 = Rgb2(adjBG2,	todaysec, 86399, 4000, 1, 40)
:	let highlCmd = printf("highlight DiffDelete guibg=#%02x%02x%02x", adj1, adj2, adj3)
:	execute highlCmd
:	let adj1 = Rgb2(adjBG1+30, todaysec, 86399, 4000, 1, 40)
:	let adj2 = Rgb2(adjBG1A+30, todaysec, 86399,4000,1,40)
:	let adj3 = Rgb2(adjBG2, todaysec, 86399, 4000, 1, 40)
:	let highlCmd = printf("highlight DiffChange guibg=#%02x%02x%02x", adj1,adj2,adj3)
:	execute highlCmd
:	let adj1 = Rgb2(adjBG1,	todaysec, 86399, 4000, 1, 40)
:	let adj2 = Rgb2(adjBG1A, todaysec, 86399, 4000, 1, 40)
:	let adj3 = Rgb2(adjBG2+30, todaysec, 86399, 4000, 1, 40)
:	let highlCmd = printf("highlight DiffText guibg=#%02x%02x%02x", adj1, adj2, adj3)
:	execute highlCmd
:	let adj1 = Rgb2a((-todaysec+86400)/338/4+160, todaysec, 57000, 16000, 23000, -100, -42, 0, 12)
:	let adj2 = Rgb2a((-todaysec+86400)/338/4+76, todaysec, 57000, 16000, 23000, -100, -42, 0, 12)
:	let adj3 = Rgb2a((-todaysec+86400)/338/4+23, todaysec, 57000, 16000, 23000, -100, -42, 0, 12)
:	let adj4 = Rgb4(adjBG1, todaysec, 57000, 16000, 15000, -4, -11, -2, 0, 0)
:	let adj5 = Rgb4(adjBG1A, todaysec, 57000, 16000, 15000, -4, -11, -2, 0, 0)
:	let adj6 = Rgb4(adjBG2, todaysec, 57000, 16000, 15000, -4, -11, -2, 0, 0)
:	let highlCmd = printf("highlight Statement guifg=#%02x%02x%02x guibg=#%02x%02x%02x", adj1, adj2, adj3, adj4, adj5, adj6)
:	execute highlCmd
:	let adjBG5 = (todaysec<43200)?todaysec/338/2:todaysec/450+63
:	let highlCmd = printf("highlight VertSplit guifg=#%02x%02x%02x", adjBG3, adjBG3, adjBG5)
:	execute highlCmd
:	let adj1 = Rgb2((-todaysec+86400)/338/2+40, todaysec, 44000, 8000, 20000, 100)
:	let adj2 = Rgb2((-todaysec+86400)/338/2+54, todaysec, 44000,8000,20000,100)
:	let adj3 = Rgb2((-todaysec+86400)/338/2+80, todaysec, 44000, 8000, 20000, 100)
:       let highlCmd = printf("highlight LineNr guifg=#%02x%02x%02x", adj1, adj2, adj3)  
:	execute highlCmd
:       let highlCmd = printf("highlight FoldColumn guifg=#%02x%02x%02x", adj1, adj2, adj3)  
:	execute highlCmd
:       let highlCmd = printf("highlight Folded guifg=#%02x%02x%02x", adj1, adj2, adj3)  
:	execute highlCmd
:	let adj1 = Rgb2a((-todaysec+86400)/400/2+27, todaysec, 57000, 22000, 14500, -140, -45, 0, 25)
:	let adj2 = Rgb2a((-todaysec+86400)/400/2+110, todaysec, 57000, 22000, 14500, -140, -45, 0, 25)
:	let adj4 = Rgb4a(adjBG1, todaysec, 57000, 22000, 14500, -26, -28, -3, -2, 8, 2, 25, -20, -4, -13)
:	let adj5 = Rgb4a(adjBG1A, todaysec, 57000, 22000, 14500, -26, -28, -3, -2, 8, 2, 25, -20, -4, -13)
:	let adj6 = Rgb4a(adjBG2, todaysec, 57000, 22000, 14500, -26, -28, -3, -2, 8, 2, 25, -20, -4, -13)
:	let highlCmd = printf("highlight Constant guifg=#%02x%02x%02x guibg=#%02x%02x%02x", adj1, adj1, adj2, adj4, adj5, adj6)
:	execute highlCmd
:       let highlCmd = printf("highlight NonText guibg=#%02x%02x%02x guifg=#%02x%02x%02x", adjBG3, adjBG1, adjBG1, adj1, adj1, adj2)  
:	execute highlCmd
:	let highlCmd=printf("highlight JavaScriptValue guifg=#%02x%02x%02x guibg=#%02x%02x%02x", adj1, adj1, adj2, adj4, adj5, adj6)
:	execute highlCmd
:	let adj1 = Rgb2a((-todaysec+86400)/338/2+110, todaysec, 35000, 9600, 40000, -285, 30, 0, 17)
:	let adj2 = Rgb2a((-todaysec+86400)/338/2+76, todaysec, 35000, 0100, 40000, -285, 30, 0, 17)
:	let adj3 = Rgb2a((-todaysec+86400)/338/2, todaysec, 35000, 0100, 40000, -285, 30, 0, 17)
:	let adj4 = Rgb4(adjBG1-30, todaysec, 0, 0, 10000, 20, 20, 40, 20, 40)
:	let adj5 = Rgb4(adjBG1A-10, todaysec, 0, 0, 10000, 20, 20, 40, 20, 40)
:	let adj6 = Rgb4(adjBG2+10, todaysec, 0, 0, 10000, 20, 20, 40, 20, 40)
:	let highlCmd = printf("highlight Normal guifg=#%02x%02x%02x gui=NONE", adj1, adj2, adj3)
:	execute highlCmd
:       let highlCmd=printf("highlight Search guifg=#%02x%02x%02x guibg=#%02x%02x%02x", adj1, adj2, adj3, adj4, adj5, adj6) 
:	execute highlCmd
:	let adj4 = Rgb4(adjBG1, todaysec, 77000, 10000, 26000, -99, -99, -99, -99, 99)
:	let adj5 = Rgb4(adjBG1A, todaysec, 77000,10000,26000,-99,-99,-99,-99,99)
:	let adj6 = Rgb4(adjBG2, todaysec, 77000, 10000, 26000, -99, -99, -99, -99, 99)
:	let highlCmd = printf("highlight Question guifg=#%02x%02x%02x guibg=#%02x%02x%02x", adj1, adj2, adj3, adj4, adj5, adj6)
:	execute highlCmd
:	let highlCmd=printf("highlight MoreMsg guifg=#%02x%02x%02x guibg=#%02x%02x%02x", adj1, adj2, adj3, adj4, adj5, adj6)
:	execute highlCmd
:	let adj4 = Rgb2(adjBG1+40, todaysec, 86399, 6000, 1, 30)
:	let adj5 = Rgb2(adjBG1A+15, todaysec, 86399, 6000, 1, 30)
:	let adj6 = Rgb2(adjBG2+10, todaysec, 86399, 6000, 1, 30)
:	let highlCmd = printf("highlight Visual guifg=#%02x%02x%02x guibg=#%02x%02x%02x", adj1, adj2, adj3, adj4, adj5, adj6)
:	execute highlCmd
:	let adj1 = Rgb2a((-todaysec+86400)/365/2+66, todaysec, 57000, 10000, 23000, -168, -30, -6, 33)
:	let adj2 = Rgb2a((-todaysec+86400)/365/2+97, todaysec, 57000, 10000, 23000, -168, -30, -6, 33)
:	let adj3 = Rgb2a((-todaysec+86400)/365/2, todaysec, 57000, 10000, 23000, -168, -30, -6, 33)
:	let adj4 = Rgb4a(adjBG1, todaysec, 57000, 10000, 15000, -8, -12, 5, 0, 7,3,6,-8,-2,-2)
:	let adj5 = Rgb4a(adjBG1A, todaysec,57000,10000,15000,-8,-12,5,0,7,3,6,-8,-2,-2)
:	let adj6 = Rgb4a(adjBG2, todaysec,57000,10000,15000,-8,-12,5,0,7,3,6,-8,-2,-2)
:	let highlCmd = printf("highlight Identifier guifg=#%02x%02x%02x guibg=#%02x%02x%02x", adj1, adj2, adj3, adj4, adj5, adj6) 
:	execute highlCmd
:	let adj1 = Rgb2a((-todaysec+86400)/355/2+97, todaysec, 43000, 2000, 19000, -130, -75, 0, 30)
:	let adj2 = Rgb2a((-todaysec+86400)/355/2+0, todaysec, 43000, 2000, 19000, -130, -75, 0, 30)
:	let adj3 = Rgb2a((-todaysec+86400)/355/2+137, todaysec, 43000, 2000, 19000, -130, -75, 0, 30)
:	let adj4 = Rgb4(adjBG1, todaysec, 43000, 2000, 19000, -99, -99, 0, 0, 0)
:	let adj5 = Rgb4(adjBG1A, todaysec, 43000, 2000, 19000, -99, -99, 0, 0, 0)
:	let adj6 = Rgb4(adjBG2,	 todaysec, 43000, 2000, 19000, -99, -99, 0, 0, 0)
:	let highlCmd = printf("highlight PreProc guifg=#%02x%02x%02x gui=bold guibg=#%02x%02x%02x",	adj1,adj2,adj3,adj4,adj5,adj6)
:	execute highlCmd
:	let adj1 = Rgb2a((-todaysec+86400)/600/4+187, todaysec, 57000, 10000, 23000, -156, -68, 0, 10)
:	let adj2 = Rgb2a((-todaysec+86400)/600/4+95, todaysec, 57000, 10000, 23000, -156, -68, 0, 10)
:	let adj3 = Rgb2a((-todaysec+86400)/600/4+155, todaysec, 57000, 10000, 23000, -156, -68, 0, 10)
:	let adj4 = Rgb4(adjBG1, todaysec, 57000, 10000, 15000, -2, -5, -5, -2, 0)
:	let adj5 = Rgb4(adjBG1A, todaysec, 57000, 10000, 15000, -2, -5, -5, -2, 0)
:	let adj6 = Rgb4(adjBG2, todaysec, 57000, 10000, 15000, -2, -5, -5, -2, 0)
:	let highlCmd = printf("highlight Special guifg=#%02x%02x%02x gui=bold guibg=#%02x%02x%02x", adj1, adj2, adj3, adj4, adj5, adj6) 
:	execute highlCmd
:	let highlCmd = printf("highlight JavaScriptParens guifg=#%02x%02x%02x gui=bold guibg=#%02x%02x%02x", adj1, adj2, adj3, adj4, adj5, adj6) 
:	execute highlCmd
:	let adj1 = Rgb2((-todaysec+86400)/338/2+120, todaysec, 47000, 3000, 14000, 64)
:	let adj2 = Rgb2((-todaysec+86400)/338/2+10, todaysec, 47000, 3000, 14000, 64)
:	let adj3 = Rgb2((-todaysec+86400)/338/2+80, todaysec, 47000, 3000, 14000, 64)
:	let adj4 = Rgb4(adjBG1, todaysec, 47000, 3000, 14000, -99, -99, -99, -99, 99)
:	let adj5 = Rgb4(adjBG1A, todaysec, 47000, 3000, 14000, -99, -99, -99, -99, 99)
:	let adj6 = Rgb4(adjBG2, todaysec, 47000, 3000, 14000, -99, -99, -99, -99, 99)
:       let highlCmd = printf("highlight Title guifg=#%02x%02x%02x guibg=#%02x%02x%02x", adj1, adj2, adj3, adj4, adj5, adj6) 
:	execute highlCmd
:	let adj1 = Rgb2b((-todaysec+86400)/338/4+110, todaysec, 50000, 12000, 22000, 220, 140, -300)
:	let adj2 = Rgb2b((-todaysec+86400)/338/4+110, todaysec, 50000, 12000, 22000, 220, 140, -300)
:	let adj3 = Rgb2b((-todaysec+86400)/338/4+110, todaysec, 50000, 12000, 22000, 220, 140, -300)
:	let adj4 = Rgb4(adjBG1,	todaysec, 50000, 12000, 22000, -8, -18, 0, 0, -3)
:	let adj5 = Rgb4(adjBG1A, todaysec, 50000, 12000, 22000, -8, -18, 0, 0, -3)
:	let adj6 = Rgb4(adjBG2,	todaysec, 50000, 12000, 22000, -8, -18, 0, 0, -3)
:	let highlCmd = printf("highlight Comment guifg=#%02x%02x%02x guibg=#%02x%02x%02x", adj1, adj2, adj3, adj4, adj5, adj6)
:	execute highlCmd
:	let highlCmd = printf("highlight htmlComment guifg=#%02x%02x%02x guibg=#%02x%02x%02x", adj1, adj2, adj3, adj4, adj5, adj6)
:	execute highlCmd
:	let highlCmd = printf("highlight htmlCommentPart guifg=#%02x%02x%02x guibg=#%02x%02x%02x", adj1, adj2, adj3, adj4, adj5, adj6)
:	execute highlCmd
:	let adj1 = Rgb6(todaysec/338+70)
:	let adj2 = Rgb6(todaysec/338+30)
:	let adj3 = Rgb6(todaysec/338-100)
:	let adj4 = Rgb2a((-todaysec+86400)/338/2+70, todaysec, 35000, 15000, 14000, 60, 120, 0, 0)
:	let adj5 = Rgb2a((-todaysec+86400)/338/2+60, todaysec, 35000, 15000, 14000, 60, 120, 0, 0)
:	let adj6 = Rgb2a((-todaysec+86400)/338/2+0, todaysec, 35000, 15000, 14000, 60, 120, 0, 0)
:	let highlCmd = printf("highlight StatusLine guibg=#%02x%02x%02x guifg=#%02x%02x%02x gui=bold", adj1, adj2, adj3, adj4, adj5, adj6)
:	execute highlCmd
:	let adj1 = Rgb6(todaysec/338+70)
:	let adj2 = Rgb6(todaysec/338+60)
:	let adj3 = Rgb6(todaysec/338-100)
:	let adj4 = Rgb2a((-todaysec+86400)/338/2+70, todaysec, 20000, 10000, 14000, 40, 120, 0, 0)
:	let adj5 = Rgb2a((-todaysec+86400)/338/2+0, todaysec, 20000, 10000, 14000, 40, 120, 0, 0)
:	let adj6 = Rgb2a((-todaysec+86400)/338/2+0, todaysec, 20000, 10000, 14000, 40, 120, 0, 0)
:	let highlCmd = printf("highlight StatusLineNC guibg=#%02x%02x%02x guifg=#%02x%02x%02x gui=bold", adj1, adj2, adj3, adj4, adj5, adj6)
:	execute highlCmd
:	let adj1 = Rgb2((-todaysec+86400)/338/2, todaysec, 37000, 27000, 20000, 40)
:	let adj2 = Rgb2((-todaysec+86400)/338/2+20, todaysec, 37000, 27000, 20000, 40)
:	let adj3 = Rgb2((-todaysec+86400)/338/2+80, todaysec, 37000, 27000, 20000, 40)
:	let adjBG6 = (adjBG5-32>=0)?adjBG5-32:0
:	let highlCmd = printf("highlight PMenu guibg=#%02x%02x%02x", adjBG3, adjBG3, adjBG1)
:	execute highlCmd
:	let highlCmd = "highlight PMenuSel guibg=Yellow guifg=Blue"
:	execute highlCmd
:	let adj1 = Rgb2a((-todaysec+86400)/338/2+150, todaysec, 60000, 11000, 13000, -90, -45, -50, -20)
:	let adj2 = Rgb2a((-todaysec+86400)/338/2+120, todaysec, 60000, 11000, 13000, -90, -45, -50, -20)
:	let adj3 = Rgb2a((-todaysec+86400)/338/2+0, todaysec, 60000, 11000, 13000, -90, -45, -50, -20)
:	let highlCmd = printf("highlight Type guifg=#%02x%02x%02x", adj1, adj2, adj3)
:	execute highlCmd
:	let adj1 = Rgb3(255, todaysec, 80000, 0)
:	let adj2 = Rgb3(255, todaysec, 80000, 255)
:	let adj3 = Rgb3(0, todaysec, 80000, 0)
:	let highlCmd = printf("highlight Cursor guibg=#%02x%02x%02x", adj1, adj2, adj3)
:	execute highlCmd
:	let highlCmd = printf("highlight MatchParen guibg=#%02x%02x%02x", adj1, adj2, adj3)
:	execute highlCmd
:	let adj1 = Rgb2((-todaysec+86400)/338/2+100, todaysec, 44000, 10000, 26000, 40)
:	let adj2 = Rgb2((-todaysec+86400)/338/2+0, todaysec, 44000, 10000, 26000, 40)
:	let adj3 = Rgb2((-todaysec+86400)/338/2+180, todaysec, 44000, 10000, 26000, 40)
:	let adj4 = Rgb4(adjBG1,	todaysec, 44000, 10000, 26000, -99, -99, -99, -99, 99)
:	let adj5 = Rgb4(adjBG1A, todaysec, 44000, 10000, 26000, -99, -99, -99, -99, 99)
:	let adj6 = Rgb4(adjBG2, todaysec, 44000, 10000, 26000, -99, -99, -99, -99, 99)
:	let highlCmd = printf("highlight htmlLink guifg=#%02x%02x%02x guibg=#%02x%02x%02x", adj1, adj2, adj3, adj4, adj5, adj6)
:	execute highlCmd
:	let adj1 = Rgb2((-todaysec+86400)/338/2+100, todaysec, 66000, 8000, 8000, 95)
:	let adj2 = Rgb2((-todaysec+86400)/338/2+160, todaysec, 66000, 8000, 8000, 95)
:	let adj3 = Rgb2((-todaysec+86400)/338/2+0, todaysec, 66000, 8000, 8000, 95)
:	let adj4 = Rgb4(adjBG1,	todaysec, 66000, 8000, 8000, -5, -5, 10, 3, 5)
:	let adj5 = Rgb4(adjBG1A, todaysec, 66000, 8000, 8000, -5, -5, 10, 3, 5)
:	let adj6 = Rgb4(adjBG2,	todaysec, 66000, 8000, 8000, -5, -5, 10, 3, 5)
:	let highlCmd = printf("highlight Directory guifg=#%02x%02x%02x guibg=#%02x%02x%02x", adj1, adj2, adj3, adj4, adj5, adj6)
:	execute highlCmd
:	redraw
:endfunction       

au CursorHold * call SetHighLight()
au CursorHoldI * call SetHighLight()
plugin/timer.vim	[[[1
19
" +-----------------------------------------------------------------------------+
" | TIMER                                                                       |
" +-----------------------------------------------------------------------------+
" | START                                                                       |
" +-----------------------------------------------------------------------------+
" | The following 'script' does a sort of a 'timer' function like there is on   |
" | other programming environments in VIM.  It relies on updatetime.  If the    |
" | updatetime is set to 4000 for example it will wait and then execute after   |
" | 4 seconds.                                                                  |
" +-----------------------------------------------------------------------------+
" | REVISONS:                                                                   |
" | SUN 19TH SEP 2010:   1.0                                                    |
" |                      Initial revision.                                      |
" +-----------------------------------------------------------------------------+

autocmd CursorHold * call Timer()
function! Timer()
  call feedkeys("f\e")
endfunction 
