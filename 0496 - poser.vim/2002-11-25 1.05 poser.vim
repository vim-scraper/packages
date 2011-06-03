" Vim syntax file
" Language:	Poser
" Maintainer:	Sean Cannon <dodger@dodger.org>
" Last Change:	2002 November 24
" Location:	http://www.dodger.org/friv/syntax/perl.vim
"
" Please download most recent version first before mailing
" any comments.

" Remove any old syntax stuff that was loaded (5.x) or quit when a syntax file
" was already loaded (6.x).
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" All keywords

syn match poserGetStringRes /GetStringRes(\d\+,\d\+)/
syn region  poserString start=+"+  end=+"+ contains=poserGetStringRes
syn keyword poserBlock	version movieInfo prop actor light camera figure doc illustrationParms renderDefaults material channels deltas alternateGeom inkyChain fgStrokeParms bgStrokeParms edgeStrokeParms keys geomCustom
syn keyword poserDeltaInfo Figure d
syn keyword poserKeyword figureResFile setGeomHandlerOffset
syn keyword poserVersionNumber number
syn keyword poserMovieInfo numFrames loopStart loopEnd currentFrame framesPerSec outputWidth outputHeight outputFramesPerSec outputStartFrame outputEndFrame useCustomFramesPerSec renderStyle outputType res movieName bgMovieOn
syn keyword poserObjectInfo storageOffset geomResource objFileGeom geomHandlerGeom lightType name bend dynamicsLock hidden addToMenu castsShadow includeInDepthCue parent objFile defaultGeomName endPoint origin orientation displayOrigin displayMode customMaterial locked port cameraModel depthMapSize depthCamera doShadow lightOn numbVerts numbTVerts numbTSets numbElems numbSets v vt g f usemtl
syn keyword poserFigureInfo root defaultPick displayOn allowsBending figureType origFigureType canonType conforming
syn keyword poserDocInfo dimensions screenPlace useLimits headGuides horizon vanishingLines hipShoulder groundDisplay depthCue boxesAlways bgPicOn handLockOn loopInterpolation quatIntrpolation doBalance fastTracking groundShadows bendBodies fgColor bgColor shadowColor paperTexture addActor addLight addCamera rootActor rightCamera leftCamera posingCamera faceCamera rHandCamera lHandCamera dollyCamera topCamera frontCamera mainCamera auxCamera useCamera pickActor backgroundFile
syn keyword poserIlloParmsInfo	combineGradient thresholdGradient combineColor overBlack autoDensity normalSegCutOff bgStyle useUVspace autoSpacing colorBlend liteFactor1 liteFactor2 liteFactor3
syn keyword poserRenderInfo Kd Ks Ns antialiasing textureStrength bumpStrength useTexture useBump castShadows renderOver toNewWindow newWinWidth newWinHeight newWinDPI
syn keyword poserChannelID rotateX rotateY rotateZ scale scaleX scaleY scaleZ kdRed kdGreen kdBlue kdIntensity propagatingScale propagatingScaleX propagatingScaleY propagatingScaleZ translateX translateY translateZ depthMapStrength depthMapSize fical hither valueParm xOffsetA xOffsetB yOffsetA yOffsetB zOffsetA zOffsetB twistX twistY twistZ jointX jointY jointZ geomChan curve liteFalloffStart liteFalloffEnd liteAttenStart liteAttenEnd targetGeom
syn keyword poserChannelInfo uniqueInterp initValue hidden forceLimits min max trackingScale interpStyleLocked angles smoothZones otherActor matrixActor center startPt endPt doBulge posBulgeLeft posBulgeRight negBulgeLeft negBulgeRight jointMult calcWeights staticValue deltaAddDelta indexes numbDeltas
syn keyword poserMaterialInfo KdColor KaColor KsColor Texture NsExponent tMin tMax tExpo bumpStrength ksIgnoreTexture reflectThruLights reflectThruKd textureMap bumpMap reflectionMap transparencyMap ReflectionColor reflectionStrength TextureColor
syn keyword poserIKChainInfo addLink goal linkWeight
syn keyword poserStrokeParms lineLength density minRadius maxRadius lineRandomness strokeHeadLength strokeTailLength colorRandom crossHatch opacity totalNormCutOff colorSegCutOff britenessSegCutOff britenessLoSegCutOff coloredLines brushStyle
syn keyword poserKeysInfo static k sl spl sm
syn keyword poserBoolean on off flipped
syn keyword poserSpecial xrot yrot zrot NO_MAP USEPARENT UNIVERSE NULL
syn keyword poserConstructor addChild weld valueOpDeltaAdd sphereMatsRaw
syn match poserVector "/\d\+"
syn match poserBraces "[{}]"
syn match poserRef "\w\+:\d\+"
syn match poserRef "\w\+ \w\+:\d\+"
syn match poserRef "\w\+ \w\+ \w\+:\d\+"
syn match poserFile "\s:\S\+$"
syn match poserInt "\s-*\d\+"
syn match poserInt "e[+-]\d\+"
syn match poserFloat "-*\.\d\+"
syn match poserFloat "-*\d\+\.\d\+"
syn match poserFloat "-*\d\+\.\d\+e+\d\+"


if version >= 508 || !exists("did_perl_syn_inits")
  if version < 508
    let did_perl_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  " The default highlighting.
  HiLink poserRef		Special
  HiLink poserBraces		Function
  HiLink poserFile		PreProc
  HiLink poserString		String
  HiLink poserInt		Number
  HiLink poserFloat		Number
  HiLink poserVector		Number
  HiLink poserBlock		Repeat
  HiLink poserKeyword		Function
  HiLink poserVersionNumber	Function
  HiLink poserMovieInfo		Function
  HiLink poserObjectInfo	Function
  HiLink poserFigureInfo	Function
  HiLink poserDocInfo		Function
  HiLink poserIlloParmsInfo	Function
  HiLink poserRenderInfo	Function
  HiLink poserDeltaInfo		Function
  HiLink poserChannelID		Conditional
  HiLink poserChannelInfo	Function
  HiLink poserMaterialInfo	Function
  HiLink poserIKChainInfo	Function
  HiLink poserStrokeParms	Function
  HiLink poserBoolean		Boolean
  HiLink poserBraces		Function
  HiLink poserFile		Special
  HiLink poserGetStringRes	Type
  HiLink poserKeysInfo		Function
  HiLink poserSpecial		Type
  HiLink poserConstructor	Type
  delcommand HiLink
endif

" Syncing to speed up processing
"
let b:current_syntax = "poser"

" vim: ts=8
