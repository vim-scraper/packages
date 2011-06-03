" +-----------------------------------------------------------------------------+
" | CHANGING COLOUR SCRIPT                                                      |
" +-----------------------------------------------------------------------------+
" | START                                                                       |
" +-----------------------------------------------------------------------------+
" | REVISONS:                                                                   |
" | WED 8TH JUL 2009:  o VER 3.3                                                |
" |                    . made Constant look very pretty at lighter background   |
" |                      tones by adding dab of shadow, tuned-up as far as i    |
" |                      could the Constant visibility at lower-light           |
" |                      background and darker part of the clash background     |
" |                      area - where background makes text's colour hardest to |
" |                      to read.                                               |
" | TUE 7TH JUL 2009   o VER 3.2                                                |
" |                    . removed a lot of the 'glary' backlighting by making    |
" |                      the text color get darker                              |
" |                    . greatly improved visibility of Constant at darker      |
" |                      background colours without distorting the background   |
" |                      too much.                                              |
" |                    . greatly simplified the way RGBEl2 works, hence making  |
" |                      it easier to estimate how much adjustment text needs   |
" | MON 6TH JUL 2009:  o VER 3.1                                                |
" |                    . made the backlighting of Statement and Constant less   |
" |                      bright, it was overdone                                |
" | SUN 5TH JUL 2009:  o VER 3.0                                                |
" |                    . finally made Constant backlighting so that it          |
" |                      perfectly kicks in at darker background colours        |
" |                      backgrounds                                            |
" |                    . improved visibility around clash-background and just   |
" |                      below the clash-background area of Comment             |
" |                    o VER 2.9                                                |
" |                    . made backlighting for Statement in the clash-background|
" |                      area less bright.                                      |
" | SAT 4TH JUL 2009:  o VER 2.8                                                |
" |                    . darkened the back-darkening of Constant at the low end |
" |                      of the background spectrum, as it looks a bit fuzzy    |
" |                      otherwise.                                             |
" |                    o VER 2.7                                                |
" |                    . made a slight correction to the backlighting of the    |
" |                      Constant that was looking way to bright at the high    |
" |                      end of the background spectrum                         |
" |                    o VER 2.6                                                |
" |                    . extensively tuned-up the interplay of Statement,       |
" |                      Identifier, and specially Constant re. the changing    |
" |                      background colour. C64 fans will love some of the      |
" |                      funky colours at the darker end of the background      |
" |                      spectrum.                                              |
" | FRI 3RD JUL 2009:  o VER 2.5                                                |
" |                    . darkened background backlighting at the dark end of    |
" |                      Constant because it was too hard to read               |
" |                    . darkened background backlighting at the dark end of    |
" |                      Identifier as it was a little fuzzy                    |
" |                    o VER 2.4                                                |
" |                    . made statement backlighting effect less agressive      |
" |                    . made the Constant text colour blue, not brown, as there|
" |                      were too many other browns and it got repetitive       |
" |                    . added a dab more backlighting to Identifier and        |
" |                      darkened the text colour a shade                       |
" |                    . reversed the order of the revision history comments    |
" | THU 2ND JUL 2009   o VER 2.3                                                |
" |                    . made the Constant text colour more gradual change as it|
" |                      goes over the 'danger' area. Previously it went to jet |
" |                      black suddenly as the background went from dark to     |
" |                      light. It looked really silly. Now the colour is a     |
" |                      more nice dark-brown colour, not jet-black.  Tuned-in  |
" |                      the background 'down'-lighting of Constant at the dark |
" |                      boundary of the danger zone so it is exactly as        |
" |                      dark as it needs to be no more, and tuned-in the       |
" |                      actually in-the-danger-zone' background adjustment as  |
" |                      well so it brings out the new non-jet-black text colour|
" |                    o VER 2.2                                                |
" |                    . made the Constant background up-lighting a bit         |
" |                      stronger as it was previously overcorrected and made   |
" |                      *too* weak (oops!), and adjusted the 'dangerzone'      |
" |                      background lighting of the same element to be not quite|
" |                      as bright, so that it looks more tastefully done       |
" |                    o VER 2.1                                                |
" |                    . corrected slightly over-zealous background brightening |
" |                      in the lighter background of Constant highlight elem.  |
" |                    . improved the clarity of the explantion of what the     |
" |                      RGBEl4 function does                                   |
" |                    . removed variable s:oldhA that had originally been part |
" |                      of calculation for the timing of the change of colours,|
" |                      that gradually got replaced by the more accurate       |
" |                      s:oldactontime                                         |
" |                    . added a proper description of what s:oldactontime does |
" |                    . improved clarity of description of what g:changefreq   |
" |                      does                                                   |
" | SAT 13TH JUN 2009  o VER 2.0                                                |
" |                    . made standard change freq. 1 min, not too often to     |
" |                      annoy but often enough to remind you of time is passing|
" |                    o VER 1.9                                                |
" |                    . made it very easy for you to set your own preferred    |
" |                      frequency for the script to change the colour          |
" | FRI 12TH JUN 2009: o VER 1.8                                                |
" |		       . removed 'stopinsert' from au CursorHoldI               |
" |		         unintentionally left in                                |
" |		       . added comment to show how to remove invert effect      |
" | THU 11TH JUN 2009: o VER 1.7                                                |
" |		       . corrected slightly glary Identifier at low light       |
" | THU 4TH JUN 2009:  o VER 1.6                                                |
" |                    . brightened Constant and Identifier bgr. a tiny amount  |
" | WED 3RD JUN 2009:  o VER 1.5                                                |
" |                    . removed duplicate htmlComment* pair                    |
" |                    . improved comment to a nice balanced-out grey           |
" |                    o VER 1.4                                                |
" |                    . improve way bckgrnds. enhanced, corrected some mistakes|
" | TUE 2ND JUN 2009:  o VER 1.3                                                |
" |                    . added pretty frames around function descriptions       |
" |                    . created new entry for Directory element in grassy green|
" |                    . corrected bug: lo-lights not perfectly synch. wit. text|
" | TUE 2ND JUN 2009:  o VER 1.2                                                |
" |                    . Corrected Search element background was perm. yellow   |
" |                    . Considerably reduced clutter, shortened names, etc     |
" | MON 1ST JUN 2009:  o VER 1.1                                                |
" |                    . Put comment markers to ensure whole script include-oops|
" |                    . Corrected Question of element potential                |
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
" | it will do so, the lower the more often it will do so.  480 is equivalent to |
" | every 20 seconds. (86400, the total number of seconds in a day, squeezed down|
" | to one hours, divided by 60 and then by 3 .. so it's like one hour divided by|
" | 60 and then by 3 = 20 seconds. Obviously if you wanted the colour to change  |
" | every minute then you'd do 86400 divided by 60. The script originally worked |
" | by changing colour throughout the day so that's why it's still using this    |
" | peculiar number 86400 as the 'time range'. You could play with this number   |
" | until you find one that works out best for you. E.g. every thirty seconds    |
" | (86400/60/2=720). Every two minutes (86400/6/5=2880). Every 2.5 minutes      |
" | (86400/6/4=3600). Every 1 minute (86400/60=1440).                            |
" +------------------------------------------------------------------------------+
let g:changefreq=1440

"debug
"let g:mytime=40000
"let g:mysenDar=10000
"let g:mysenLig=24000
"let g:myadjust=64

" +------------------------------------------------------------------------------+
" | Main RGBEl function, used to work out amount to offset RGB value by to avoid |
" | it clashing with the background colour.                                      |
" | Return value is modified or otherwise value (not modified if no clash).      |
" +------------------------------------------------------------------------------+
:function RGBEl2(RGBEl,actBgr,dangerBgr,senDar,senLig,adjust,debug)
:	if a:debug==1
:		return a:RGBEl
:	endif
:	if exists("g:mysenDar") && a:debug!=2
:		let senDar=g:mysenDar
:	else
:		let senDar=a:senDar
:	endif
:	if exists("g:mysenLig") && a:debug!=2
:		let senLig=g:mysenLig
:	else
:		let senLig=a:senLig
:	endif
:	if exists("g:myadjust") && a:debug!=2
:		let adjust=g:myadjust
:	else
:		let adjust=a:adjust
:	endif
:	if exists("g:mydangerBgr") && a:debug!=2
:		let dangerBgr=g:mydangerBgr
:	else
:		let dangerBgr=a:dangerBgr
:	endif
:	if a:actBgr>=dangerBgr-senDar && a:actBgr<=dangerBgr+senLig
:		let whatdoyoucallit=dangerBgr-a:actBgr
:		if whatdoyoucallit<0
:			let whatdoyoucallit=-whatdoyoucallit
:		endif
:		let whatdoyoucallit=whatdoyoucallit/130
:		if whatdoyoucallit>255
:			let whatdoyoucallit=255
:		endif
:		let whatdoyoucallit=-whatdoyoucallit+255
:		let whatdoyoucallit=whatdoyoucallit*adjust
:		let whatdoyoucallit=whatdoyoucallit/800
:		let whatdoyoucallit=whatdoyoucallit+65
:		let adjustedValue=a:RGBEl-whatdoyoucallit
:	else
:		let adjustedValue=a:RGBEl
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
" | RGBEl function for cursor to work out amount to offset RGB component to stop |
" | it from clashing with the background colour.                                 |
" | Return value is modified or otherwise value (not modified if no clash).      |
" +------------------------------------------------------------------------------+
:function RGBEl3(RGBEl,actBgr,dangerBgr)
:	let diff=a:actBgr-a:dangerBgr
:	if diff<0
:		let diff=-diff
:	endif
:	if diff<8000
:		let adjustedValue=a:RGBEl-128
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
:function RGBEl4(RGBEl,actBgr,dangerBgr,senDar,senLig,darkAdj,lghtAdj,dangerZoneAdj,debug)
:	if a:debug==1
:		return a:RGBEl
:	endif
:	if exists("g:mysenDar") && a:debug!=2
:		let senDar=g:mysenDar
:	else
:		let senDar=a:senDar
:	endif
:	if exists("g:mysenLig") && a:debug!=2
:		let senLig=g:mysenLig
:	else
:		let senLig=a:senLig
:	endif
:	if exists("g:mydangerBgr") && a:debug!=2
:		let dangerBgr=g:mydangerBgr
:	else
:		let dangerBgr=a:dangerBgr
:	endif
:	let darkAdj=a:darkAdj
:	let lghtAdj=a:lghtAdj
:	if a:darkAdj==99
:		let darkAdj=11
:	endif
:	if a:darkAdj==-99
:		let darkAdj=-11
:	endif
:	if a:lghtAdj==99
:		let lghtAdj=11
:	endif
:	if a:lghtAdj==-99
:		let lghtAdj=-11
:	endif
:	let dangerZoneAdj=a:dangerZoneAdj
:	if a:dangerZoneAdj==99
:		let dangerZoneAdj=15
:	endif
:	if a:dangerZoneAdj==-99
:		let dangerZoneAdj=-15
:	endif
:	let adjustedValue=a:RGBEl
:	if a:actBgr>=dangerBgr-senDar-7200 && a:actBgr<dangerBgr-senDar
:		let adjustedValue=a:RGBEl+darkAdj
:	endif
:	if a:actBgr>dangerBgr+senLig && a:actBgr<=dangerBgr+senLig+7200
:		let adjustedValue=a:RGBEl+lghtAdj
:	endif
:	if a:actBgr>=(dangerBgr-senDar) && a:actBgr<=(dangerBgr+senLig) && dangerZoneAdj
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
:function RGBEl5(RGBEl,actBgr,dangerBgr,senDar,senLig,debug)
:	if a:debug==1
:		return a:RGBEl
:	endif
:	if exists("g:mysenDar") && a:debug!=2
:		let senDar=g:mysenDar
:	else
:		let senDar=a:senDar
:	endif
:	if exists("g:mysenLig") && a:debug!=2
:		let senLig=g:mysenLig
:	else
:		let senLig=a:senLig
:	endif
:	if exists("g:mydangerBgr") && a:debug!=2
:		let dangerBgr=g:mydangerBgr
:	else
:		let dangerBgr=a:dangerBgr
:	endif
:	if a:actBgr>=dangerBgr-senDar && a:actBgr<=dangerBgr+senLig
:		let adjustedValue=255
:	else
:		let adjustedValue=a:RGBEl
:	endif
:	if a:actBgr>=(dangerBgr+senLig)-21000 && a:actBgr<=dangerBgr+senLig
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
:	else
: 		if a:nightorday==1
:			let todaysec=-todaysec+86400
:		else
:			let todaysec=todaysec
:		endif
:		if (localtime()/60/60%2==1)
:			let todaysec=-todaysec+86400
:		else
:			let todaysec=todaysec
:		endif
:	endif
:	let adjBG1=(todaysec<43200)?todaysec/450:(todaysec-43200)/271+96
:	let adjBG2=(todaysec<43200)?todaysec/338:(todaysec-43200)/676+127
:	let adjBG3=(adjBG1-32>=32)?adjBG1-32:32
:	let adjBG4=(adjBG1-32>=32)?adjBG1-32:32
:       let hA=printf("highlight Normal guibg=#%02x%02x%02x",					adjBG1,adjBG1,adjBG2)
:       let hA1=printf("highlight Folded guibg=#%02x%02x%02x guifg=#%02x%02x%02x",		adjBG1,adjBG1,adjBG4,adjBG1,adjBG1,adjBG4)
:       let hA2=printf("highlight CursorLine guibg=#%02x%02x%02x",				adjBG3,adjBG3,adjBG1) 
:       let hA3=printf("highlight NonText guibg=#%02x%02x%02x guifg=#%02x%02x%02x",		adjBG3,adjBG1,adjBG1,adjBG3,adjBG1,adjBG1)  
:       let hA4=printf("highlight LineNr guibg=#%02x%02x%02x",					adjBG1,adjBG3,adjBG1)
:       let hA5=printf("highlight Search guibg=#%02x%02x%02x",					adjBG3,adjBG3,adjBG1) 
:	let adj1=	RGBEl2(adjBG1,								todaysec,86399,4000,1,40,2)
:	let adj2=	RGBEl2(adjBG1+30,							todaysec,86399,4000,1,40,2)
:	let adj3=	RGBEl2(adjBG2,								todaysec,86399,4000,1,40,2)
:	let hA6=printf("highlight DiffAdd guibg=#%02x%02x%02x",					adj1,adj2,adj3)
:	let adj1=	RGBEl2(adjBG1+30,							todaysec,86399,4000,1,40,2)
:	let adj2=	RGBEl2(adjBG1,								todaysec,86399,4000,1,40,2)
:	let adj3=	RGBEl2(adjBG2,								todaysec,86399,4000,1,40,2)
:	let hA7=printf("highlight DiffDelete guibg=#%02x%02x%02x",				adj1,adj2,adj3)
:	let adj1=	RGBEl2(adjBG1+30,							todaysec,86399,4000,1,40,2)
:	let adj2=	RGBEl2(adjBG1+30,							todaysec,86399,4000,1,40,2)
:	let adj3=	RGBEl2(adjBG2,								todaysec,86399,4000,1,40,2)
:	let hA8=printf("highlight DiffChange guibg=#%02x%02x%02x",				adj1,adj2,adj3)
:	let adj1=	RGBEl2(adjBG1,								todaysec,86399,4000,1,40,2)
:	let adj2=	RGBEl2(adjBG1,								todaysec,86399,4000,1,40,2)
:	let adj3=	RGBEl2(adjBG2+30,							todaysec,86399,4000,1,40,2)
:	let hA9=printf("highlight DiffText guibg=#%02x%02x%02x",				adj1,adj2,adj3)
:	let adj1	=RGBEl2((-todaysec+86400)/338/4+160,					todaysec,50000,5000,16000,100,2)
:	let adj2	=RGBEl2((-todaysec+86400)/338/4+76,					todaysec,50000,5000,16000,100,2)
:	let adj3	=RGBEl2((-todaysec+86400)/338/4+23,					todaysec,50000,5000,16000,100,2)
:	let adj4	=RGBEl4(adjBG1,								todaysec,50000,5000,16000,-17,2,-5,2)
:	let adj5	=RGBEl4(adjBG1,								todaysec,50000,5000,16000,-17,2,-5,2)
:	let adj6	=RGBEl4(adjBG2,								todaysec,50000,5000,16000,-17,2,-5,2)
:	let hB=printf("highlight Statement guifg=#%02x%02x%02x guibg=#%02x%02x%02x",		adj1,adj2,adj3,adj4,adj5,adj6)
:	let adjBG5=(todaysec<43200)?todaysec/338/2:todaysec/450+63
:	let hB1=printf("highlight VertSplit guifg=#%02x%02x%02x",				adjBG3,adjBG3,adjBG5)
:	let adj1=	RGBEl2((-todaysec+86400)/338/2+110,					todaysec,56000,10000,22000,40,2)
:	let adj2=	RGBEl2((-todaysec+86400)/338/2+64,					todaysec,56000,10000,22000,40,2)
:	let adj3=	RGBEl2((-todaysec+86400)/338/2,						todaysec,56000,10000,22000,40,2)
:       let hB2=printf("highlight LineNr guifg=#%02x%02x%02x",					adj1,adj2,adj3)  
:	let adj1=	RGBEl2((-todaysec+86400)/250/2+0,					todaysec,46500,17000,14000,112,2)
:	let adj2=	RGBEl2((-todaysec+86400)/250/2+76,					todaysec,46500,17000,14000,112,2)
:	let adj4=	RGBEl4(adjBG1,								todaysec,46500,17000,14000,-6,-2,4,2)
:	let adj5=	RGBEl4(adjBG2,								todaysec,46500,17000,14000,-6,-2,4,2)
:	let hC=printf("highlight Constant guifg=#%02x%02x%02x guibg=#%02x%02x%02x",		adj1,adj1,adj2,adj4,adj4,adj5)
:	let adj1=	RGBEl5((-todaysec+86400)/338/2+110,					todaysec,50000,27000,29000,2)
:	let adj2=	RGBEl5((-todaysec+86400)/338/2+64,					todaysec,50000,27000,29000,2)
:	let adj3=	RGBEl5((-todaysec+86400)/338/2,						todaysec,50000,27000,29000,2)
:	let hD=printf("highlight Normal guifg=#%02x%02x%02x gui=NONE",				adj1,adj2,adj3)
:	let adj1=	RGBEl2((-todaysec+86400)/338/2+18,					todaysec,57000,10000,24000,100,2)
:	let adj2=	RGBEl2((-todaysec+86400)/338/2+127,					todaysec,57000,10000,24000,100,2)
:	let adj3=	RGBEl2((-todaysec+86400)/338/2,						todaysec,57000,10000,24000,100,2)
:	let adj4=	RGBEl4(adjBG1,								todaysec,57000,10000,24000,-8,-99,9,2)
:	let adj5=	RGBEl4(adjBG1,								todaysec,57000,10000,24000,-8,-99,9,2)
:	let adj6=	RGBEl4(adjBG2,								todaysec,57000,10000,24000,-8,-99,9,2)
:	let hE=printf("highlight Identifier guifg=#%02x%02x%02x guibg=#%02x%02x%02x",		adj1,adj2,adj3,adj4,adj5,adj6) 
:	let adj1=	RGBEl2((-todaysec+86400)/338/2+100,					todaysec,43000,5000,16000,39,2)
:	let adj2=	RGBEl2((-todaysec+86400)/338/2+0,					todaysec,43000,5000,16000,39,2)
:	let adj3=	RGBEl2((-todaysec+86400)/338/2+140,					todaysec,43000,5000,16000,39,2)
:	let adj4=	RGBEl4(adjBG1,								todaysec,43000,5000,16000,-99,99,99,2)
:	let adj5=	RGBEl4(adjBG1,								todaysec,43000,5000,16000,-99,99,99,2)
:	let adj6=	RGBEl4(adjBG2,								todaysec,43000,5000,16000,-99,99,99,2)
:	let hF=printf("highlight PreProc guifg=#%02x%02x%02x gui=bold guibg=#%02x%02x%02x",	adj1,adj2,adj3,adj4,adj5,adj6)
:	let adj1=	RGBEl2((-todaysec+86400)/338/4+192,					todaysec,60500,14000,19000,40,2)
:	let adj2=	RGBEl2((-todaysec+86400)/338/4+100,					todaysec,60500,14000,19000,40,2)
:	let adj3=	RGBEl2((-todaysec+86400)/338/4+160,					todaysec,60500,14000,19000,40,2)
:	let adj4=	RGBEl4(adjBG1,								todaysec,60500,14000,19000,-99,-99,0,2)
:	let adj5=	RGBEl4(adjBG1,								todaysec,60500,14000,19000,-99,-99,0,2)
:	let adj6=	RGBEl4(adjBG2,								todaysec,60500,14000,19000,-99,-99,0,2)
:	let hG=printf("highlight Special guifg=#%02x%02x%02x gui=bold guibg=#%02x%02x%02x",	adj1,adj2,adj3,adj4,adj5,adj6) 
:	let adj1=	RGBEl2((-todaysec+86400)/338/2+120,					todaysec,47000,3000,14000,64,2)
:	let adj2=	RGBEl2((-todaysec+86400)/338/2+10,					todaysec,47000,3000,14000,64,2)
:	let adj3=	RGBEl2((-todaysec+86400)/338/2+80,					todaysec,47000,3000,14000,64,2)
:	let adj4=	RGBEl4(adjBG1,								todaysec,47000,3000,14000,-99,-99,99,2)
:	let adj5=	RGBEl4(adjBG1,								todaysec,47000,3000,14000,-99,-99,99,2)
:	let adj6=	RGBEl4(adjBG2,								todaysec,47000,3000,14000,-99,-99,99,2)
:       let hH=printf("highlight Title guifg=#%02x%02x%02x guibg=#%02x%02x%02x",		adj1,adj2,adj3,adj4,adj5,adj6) 
:	let adj1=	RGBEl2((-todaysec+86400)/338/2+60,					todaysec,50000,3000,13000,31,0)
:	let adj2=	RGBEl2((-todaysec+86400)/338/2+60,					todaysec,50000,3000,13000,31,0)
:	let adj3=	RGBEl2((-todaysec+86400)/338/2+60,					todaysec,50000,3000,13000,31,0)
:	let adj4=	RGBEl4(adjBG1,								todaysec,50000,3000,13000,-21,-99,99,0)
:	let adj5=	RGBEl4(adjBG1,								todaysec,50000,3000,13000,-21,-99,99,0)
:	let adj6=	RGBEl4(adjBG2,								todaysec,50000,3000,13000,-21,-99,99,0)
:	let hI=printf("highlight Comment guifg=#%02x%02x%02x guibg=#%02x%02x%02x",		adj1,adj2,adj3,adj4,adj5,adj6)
:	let hI1=printf("highlight htmlComment guifg=#%02x%02x%02x guibg=#%02x%02x%02x",		adj1,adj2,adj3,adj4,adj5,adj6)
:	let hI2=printf("highlight htmlCommentPart guifg=#%02x%02x%02x guibg=#%02x%02x%02x",	adj1,adj2,adj3,adj4,adj5,adj6)
:	let adj1=	RGBEl2(todaysec/338+70,							todaysec,14000,12000,12000,80,2)
:	let adj2=	RGBEl2(todaysec/338+30,							todaysec,14000,12000,12000,80,2)
:	let adj3=	RGBEl2(todaysec/338-100,						todaysec,14000,12000,12000,80,2)
:	let adj4=	RGBEl2((-todaysec+86400)/338/2+70,					todaysec,16000,17000,17000,120,2)
:	let adj5=	RGBEl2((-todaysec+86400)/338/2+60,					todaysec,16000,17000,17000,120,2)
:	let adj6=	RGBEl2((-todaysec+86400)/338/2+0,					todaysec,16000,17000,17000,120,2)
:	let hJ=printf("highlight StatusLine guibg=#%02x%02x%02x guifg=#%02x%02x%02x gui=bold",	adj1,adj2,adj3,adj4,adj5,adj6)
:	let adj1=	RGBEl2(todaysec/338+70,							todaysec,99999,0,0,0,2)
:	let adj2=	RGBEl2(todaysec/338+60,							todaysec,99999,0,0,0,2)
:	let adj3=	RGBEl2(todaysec/338-100,						todaysec,99999,0,0,0,2)
:	let adj4=	RGBEl2((-todaysec+86400)/338/2+70,					todaysec,14000,12000,29000,120,2)
:	let adj5=	RGBEl2((-todaysec+86400)/338/2+0,					todaysec,14000,12000,29000,120,2)
:	let adj6=	RGBEl2((-todaysec+86400)/338/2+0,					todaysec,14000,12000,29000,120,2)
:	let hK=printf("highlight StatusLineNC guibg=#%02x%02x%02x guifg=#%02x%02x%02x gui=bold",adj1,adj2,adj3,adj4,adj5,adj6)
:	let adj1=	RGBEl2((-todaysec+86400)/338/2,						todaysec,37000,27000,20000,40,2)
:	let adj2=	RGBEl2((-todaysec+86400)/338/2+20,					todaysec,37000,27000,20000,40,2)
:	let adj3=	RGBEl2((-todaysec+86400)/338/2+80,					todaysec,37000,27000,20000,40,2)
:	let adjBG6=(adjBG5-32>=0)?adjBG5-32:0
:	let hM=printf("highlight PMenu guibg=#%02x%02x%02x",					adjBG6,adjBG6,adjBG6)
:	let hN="highlight PMenuSel guibg=Yellow guifg=Blue"
:	let adj1=	RGBEl2(adjBG1+50,							todaysec,86399,4000,1,40,2)
:	let adj2=	RGBEl2(adjBG1+40,							todaysec,86399,4000,1,40,2)
:	let adj3=	RGBEl2(adjBG2,								todaysec,86399,4000,1,40,2)
:	let hL=printf("highlight Visual guibg=#%02x%02x%02x",					adj1,adj2,adj3)
:	let adj1=	RGBEl2((-todaysec+86400)/338/2+150,					todaysec,60000,8000,13000,40,2)
:	let adj2=	RGBEl2((-todaysec+86400)/338/2+120,					todaysec,60000,8000,13000,40,2)
:	let adj3=	RGBEl2((-todaysec+86400)/338/2+0,					todaysec,60000,8000,13000,40,2)
:	let hO=printf("highlight Type guifg=#%02x%02x%02x",					adj1,adj2,adj3)
:	let adj1=RGBEl3(255,todaysec,85000)
:	let adj2=RGBEl3(0,todaysec,85000)
:	let hP=printf("highlight Cursor guibg=#%02x%02x%02x",					adj1,adj1,adj2)
:	let hP1=printf("highlight MatchParen guibg=#%02x%02x%02x",				adj1,adj1,adj2)
:	let adj1=	RGBEl2((-todaysec+86400)/338/2+100,					todaysec,44000,10000,26000,40,2)
:	let adj2=	RGBEl2((-todaysec+86400)/338/2+0,					todaysec,44000,10000,26000,40,2)
:	let adj3=	RGBEl2((-todaysec+86400)/338/2+180,					todaysec,44000,10000,26000,40,2)
:	let adj4=	RGBEl4(adjBG1,								todaysec,44000,10000,26000,-99,-99,99,2)
:	let adj5=	RGBEl4(adjBG1,								todaysec,44000,10000,26000,-99,-99,99,2)
:	let adj6=	RGBEl4(adjBG2,								todaysec,44000,10000,26000,-99,-99,99,2)
:	let hQ=printf("highlight htmlLink guifg=#%02x%02x%02x guibg=#%02x%02x%02x",		adj1,adj2,adj3,adj4,adj5,adj6)
:	let adj1=	RGBEl2((-todaysec+86400)/338/2+220,					todaysec,77000,10000,26000,70,2)
:	let adj2=	RGBEl2((-todaysec+86400)/338/2+220,					todaysec,77000,10000,26000,70,2)
:	let adj3=	RGBEl2((-todaysec+86400)/338/2+0,					todaysec,77000,10000,26000,70,2)
:	let adj4=	RGBEl4(adjBG1,								todaysec,77000,10000,26000,-99,-99,99,2)
:	let adj5=	RGBEl4(adjBG1,								todaysec,77000,10000,26000,-99,-99,99,2)
:	let adj6=	RGBEl4(adjBG2,								todaysec,77000,10000,26000,-99,-99,99,2)
:	let hR=printf("highlight Question guifg=#%02x%02x%02x guibg=#%02x%02x%02x",		adj1,adj2,adj3,adj4,adj5,adj6)
:	let hR1=printf("highlight MoreMsg guifg=#%02x%02x%02x guibg=#%02x%02x%02x",		adj1,adj2,adj3,adj4,adj5,adj6)
:	let adj1=	RGBEl2((-todaysec+86400)/338/2+100,					todaysec,63000,27000,7000,40,2)
:	let adj2=	RGBEl2((-todaysec+86400)/338/2+160,					todaysec,63000,27000,7000,40,2)
:	let adj3=	RGBEl2((-todaysec+86400)/338/2+0,					todaysec,63000,27000,7000,40,2)
:	let adj4=	RGBEl4(adjBG1,								todaysec,63000,27000,7000,-99,-99,0,2)
:	let adj5=	RGBEl4(adjBG1,								todaysec,63000,27000,7000,-99,-99,0,2)
:	let adj6=	RGBEl4(adjBG2,								todaysec,63000,27000,7000,-99,-99,0,2)
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
:		execute hD
:		execute hE
:		execute hF
:		execute hG
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
:	if todaysec>=69120 || todaysec<=17280
:	echo strftime("%c")
:	endif
:endfunction       

" +------------------------------------------------------------------------------+
" | Wrapper function takes into account 'invert' global variable, used when      |
" | doing 'invert' colours behave light-dark instead of dark-light.              |
" | If you thought this effect was annoying you could you could modify this      |
" | function so it always calls muscle function with 0.                          |
" +------------------------------------------------------------------------------+
:function ExtraSetHighLight()
:	if g:highLowLightToggle==0
:		call SetHighLight(0)
:	else
:		call SetHighLight(1)
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

