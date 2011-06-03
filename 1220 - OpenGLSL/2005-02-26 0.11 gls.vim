" Vim syntax file
" Language:	OpenGL Shader Language
" Maintainer:	Adam Butcher <adam@jessamine.co.uk>
" Version:	0.11
" Last change:	2005 February 26
" Note : Based on cg.vim Rev Lebaredian

" I use:
"   autocmd BufNewFile,BufRead *.gls,*.shd,*.ogl set filetype=gls cindent comments=sr:/*,mbl:*,ex:*/,://
" to autoset this syntax

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Read the C syntax to start with
if version < 600
  so <sfile>:p:h/c.vim
else
  runtime! syntax/c.vim
  unlet b:current_syntax
endif

" types
syn keyword shdType	float int void bool
syn keyword shdType	mat2 mat3 mat4
syn keyword shdType	vec2 vec3 vec4 ivec2 ivec3 ivec4 bvec2 bvec3 bvec4
syn keyword shdType	sampler1D sampler2D sampler3D
syn keyword shdType	samplerCube sampler1DShadow sampler2DShadow

" semantics
syn keyword shdSemantic	attribute
syn keyword shdSemantic	in out inout

" other keywords
syn keyword shdStorageClass	const uniform varying
syn keyword shdStructure	struct 
syn keyword shdBoolean	true false
syn keyword shdStatement	discard

" built-ins
syn keyword shdBuiltins		gl_Position gl_PointSize gl_ClipVertex
syn keyword shdBuiltins		gl_FragCoord gl_FrontFacing gl_FragColor
syn keyword shdBuiltins		gl_FragData gl_FragDepth
syn keyword shdBuiltins		gl_Color gl_SecondarColor gl_Normal gl_Vertex
syn keyword shdBuiltins		gl_MultiTexCoord0
syn keyword shdBuiltins		gl_MultiTexCoord1
syn keyword shdBuiltins		gl_MultiTexCoord2
syn keyword shdBuiltins		gl_MultiTexCoord3
syn keyword shdBuiltins		gl_MultiTexCoord4
syn keyword shdBuiltins		gl_MultiTexCoord5
syn keyword shdBuiltins		gl_MultiTexCoord6
syn keyword shdBuiltins		gl_MultiTexCoord7
syn keyword shdBuiltins		gl_FogCoord

syn keyword shdConstants	gl_MaxLights gl_MaxClipPlanes
syn keyword shdConstants	gl_MaxTextureUnits gl_MaxTextureCoords
syn keyword shdConstants	gl_MaxVertexAttribs
syn keyword shdConstants	gl_MaxVaryingFloats
syn keyword shdConstants	gl_MaxVertexTextureImageUnits
syn keyword shdConstants	gl_MaxCombinedTextureImageUnits
syn keyword shdConstants	gl_MaxTextureImageUnits
syn keyword shdConstants	gl_MaxVertexUniformComponents
syn keyword shdConstants	gl_MaxFragmentUniformComponents
syn keyword shdConstants	gl_MaxDrawBuffers

syn keyword shdState		gl_ModelViewMatrix
syn keyword shdState		gl_ProjectionMatrix
syn keyword shdState		gl_ModelViewProjectionMatrix
syn keyword shdState		gl_TextureMatrix

syn keyword shdState		gl_NormalMatrix

syn keyword shdState		gl_ModelViewMatrixInverse
syn keyword shdState		gl_ProjectionMatrixInverse
syn keyword shdState		gl_ModelViewProjectionMatrixInverse
syn keyword shdState		gl_TextureMatrixInverse

syn keyword shdState		gl_ModelViewMatrixTranspose
syn keyword shdState		gl_ProjectionMatrixTranspose
syn keyword shdState		gl_ModelViewProjectionMatrixTranspose
syn keyword shdState		gl_TextureMatrixTranspose

syn keyword shdState		gl_ModelViewMatrixInverseTranspose
syn keyword shdState		gl_ProjectionMatrixInverseTranspose
syn keyword shdState		gl_ModelViewProjectionMatrixInverseTranspose
syn keyword shdState		gl_TextureMatrixInverseTranspose

syn keyword shdBuiltins		gl_NormalScale

syn keyword shdBuiltins		gl_ClipPlane

syn keyword shdBuiltins		gl_DepthRangeParameters gl_DepthRange
syn keyword shdMembers		near far diff

syn keyword shdBuiltins		gl_PointParameters gl_Point
syn keyword shdMembers		size sizeMin sizeMax fadeThresholdSize
syn keyword shdMembers		distanceConstantAttenuation
syn keyword shdMembers		distanceLinearAttenuation
syn keyword shdMembers		distanceQuadraticAttenuation

syn keyword shdBuiltins		gl_MaterialParameters gl_FrontMaterial gl_BackMaterial
syn keyword shdMembers		emission ambient diffuse specular shininess

syn keyword shdBuiltins		gl_LightSourceParameters gl_LightSource
syn keyword shdMembers		ambient diffuse specular position halfVector
syn keyword shdMembers		spotDirection spotExponent spotCutoff spotCosCutoff
syn keyword shdMembers		constantAttenuation linearAttenuation quadraticAttenuation

syn keyword shdBuiltins		gl_LightModelParameters gl_LightModel
syn keyword shdMembers		ambient

syn keyword shdBuiltins		gl_LightModelProducts gl_FrontLightModelProduct gl_BackLightModelProduct
syn keyword shdMembers		sceneColor

syn keyword shdBuiltins		gl_LightProducts gl_FrontLightProduct gl_BackLightProduct
syn keyword shdMembers		ambient diffuse specular

syn keyword shdBuiltins		gl_TextureEnvColor
syn keyword shdBuiltins		gl_EyePlaneS
syn keyword shdBuiltins		gl_EyePlaneT
syn keyword shdBuiltins		gl_EyePlaneR
syn keyword shdBuiltins		gl_EyePlaneQ
syn keyword shdBuiltins		gl_ObjectPlaneS
syn keyword shdBuiltins		gl_ObjectPlaneT
syn keyword shdBuiltins		gl_ObjectPlaneR
syn keyword shdBuiltins		gl_ObjectPlaneQ

syn keyword shdBuiltins		gl_FogParameters gl_Fog
syn keyword shdMembers		color density start end scale

"Varying variable builtins
syn keyword shdBuiltins gl_FrontColor gl_BackColor gl_FrontSecondaryColor gl_BackSecondaryColor gl_TexCoord gl_FogFragCoord
syn keyword shdBuiltins gl_Color gl_SecondaryColor gl_TexCoord gl_FogFragCoord

"Function builtins
syn keyword shdFunctions	radians degrees sin cos tan asin acos atan 
syn keyword shdFunctions	pow exp log exp2 log2 sqrt inversesqrt
syn keyword shdFunctions	abs sign floor ciel fract mod min max clamp mix
syn keyword shdFunctions	step smoothstep 
syn keyword shdFunctions	length distance dot cross normalize ftransform
syn keyword shdFunctions	faceforward reflect refract matrixCompMult
"vector funcs
syn keyword shdFunctions	lessThan lessThanEqual greaterThan greaterThanEqual
syn keyword shdFunctions	equal notEqual any all not
"texture funcs
syn keyword shdFunctions	texture1D texture1DProj texture1DLod texture1DProjLod
syn keyword shdFunctions	texture2D texture2DProj texture2DLod texture2DProjLod
syn keyword shdFunctions	texture3D texture3DProj texture3DLod texture3DProjLod
syn keyword shdFunctions	textureCube textureCubeLod
syn keyword shdFunctions	shadow1D shadow1DProj shadow1DLod shadow1DProjLod
syn keyword shdFunctions	shadow2D shadow2DProj shadow2DLod shadow2DProjLod
"fragment funcs
syn keyword shdFunctions	dFdx dFdy fwidth
"noise funcs
syn keyword shdFunctions	noise1 noise2 noise3 noise4

" The minimum and maximum operators in GNU C++
syn match shdMinMax "[<>]?"

syn match shdGroup "\[[^\]]*\]"

syn match shdSwizzle "\<[wxyz]\{1,4}\>"
syn match shdSwizzle "\<[rgba]\{1,4}\>"
syn match shdSwizzle "\<[stpq]\{1,4}\>"

" Default highlighting
if version >= 508 || !exists("did_shd_syntax_inits")
  if version < 508
    let did_shd_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink shdType		        Type
  HiLink shdStorageClass	        StorageClass
  HiLink shdStructure		Structure
  HiLink shdSemantic		Constant
  HiLink shdBoolean		Boolean
  HiLink shdGroup		PreProc
  HiLink shdStatement		Statement
  HiLink shdBuiltins		Special
  HiLink shdState		Constant
  HiLink shdMembers		PreProc
  HiLink shdSwizzle		PreProc
  HiLink shdFunctions		Function
  delcommand HiLink
endif

let b:current_syntax = "cpp"

" vim: ts=8
