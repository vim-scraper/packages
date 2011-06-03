" Vim syntax file
" Language:	Cg
" Maintainer:	Rev Lebaredian <revl@nvidia.com>
" Version:	.1
" Last change:	2003 July 9
" Note : Adapted from cpp.vim written by Ken Shan

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

" Cg built-in types
syn keyword cgType	float1 float2 float3 float4
syn keyword cgType	float1x1 float2x1 float3x1 float4x1
syn keyword cgType	float1x2 float2x2 float3x2 float4x2
syn keyword cgType	float1x3 float2x3 float3x3 float4x3
syn keyword cgType	float1x4 float2x4 float3x4 float4x4

syn keyword cgType	int int1 int2 int3 int4
syn keyword cgType	int1x1 inint3x1 int4x1
syn keyword cgType	int1x2 int2x2 int3x2 int4x2
syn keyword cgType	int1x3 int2x3 int3x3 int4x3
syn keyword cgType	int1x4 int2x4 int3x4 int4x4

syn keyword cgType	half half1 half2 half3 half4
syn keyword cgType	half1x1 half2x1 half3x1 half4x1
syn keyword cgType	half1x2 half2x2 half3x2 half4x2
syn keyword cgType	half1x3 half2x3 half3x3 half4x3
syn keyword cgType	half1x4 half2x4 half3x4 half4x4

syn keyword cgType	sampler sampler1D sampler2D sampler3D 
syn keyword cgType      samplerRECT samplerCUBE

syn keyword cgType	fixed fixed1 fixed2 fixed3 fixed4
syn keyword cgType	fixed1x1 fixed2x1 fixed3x1 fixed4x1
syn keyword cgType	fixed1x2 fixed2x2 fixed3x2 fixed4x2
syn keyword cgType	fixed1x3 fixed2x3 fixed3x3 fixed4x3
syn keyword cgType	fixed1x4 fixed2x4 fixed3x4 fixed4x4

syn keyword cgType	bool bool1 bool2 bool3 bool4
syn keyword cgType	bool1x1 bool2x1 bool3x1 bool4x1
syn keyword cgType	bool1x2 bool2x2 bool3x2 bool4x2
syn keyword cgType	bool1x3 bool2x3 bool3x3 bool4x3
syn keyword cgType	bool1x4 bool2x4 bool3x4 bool4x4

" Varying semantics
syn keyword cgSemantic	POSITION POSITION0 POSITION1 POSITION2 POSITION3
syn keyword cgSemantic	POSITION4 POSITION5 POSITION6 POSITION7 POSITION8
syn keyword cgSemantic	POSITION9 POSITION10 POSITION11 POSITION12 POSITION13
syn keyword cgSemantic	POSITION14 POSITION15

syn keyword cgSemantic	ATTR ATTR0 ATTR1 ATTR2 ATTR3
syn keyword cgSemantic	ATTR4 ATTR5 ATTR6 ATTR7 ATTR8
syn keyword cgSemantic	ATTR9 ATTR10 ATTR11 ATTR12 ATTR13
syn keyword cgSemantic	ATTR14 ATTR15

syn keyword cgSemantic	TEX TEX0 TEX1 TEX2 TEX3
syn keyword cgSemantic	TEX4 TEX5 TEX6 TEX7 

syn keyword cgSemantic	HPOS COL0 COL1 COL2 COL3 PSIZ WPOS

syn keyword cgSemantic	DIFFUSE DIFFUSE0

syn keyword cgSemantic	TANGENT4 TANGENT5 TANGENT6 TANGENT7 TANGENT8
syn keyword cgSemantic	TANGENT9 TANGENT10 TANGENT11 TANGENT12 TANGENT13
syn keyword cgSemantic	TANGENT14 TANGENT15

syn keyword cgSemantic	SPECULAR SPECULAR0

syn keyword cgSemantic	BLENDINDICES BLENDINDICES0 BLENDINDICES1 
syn keyword cgSemantic  BLENDINDICES2 BLENDINDICES3
syn keyword cgSemantic	BLENDINDICES4 BLENDINDICES5 BLENDINDICES6 
syn keyword cgSemantic  BLENDINDICES7 BLENDINDICES8
syn keyword cgSemantic	BLENDINDICES9 BLENDINDICES10 BLENDINDICES11 
syn keyword cgSemantic  BLENDINDICES12 BLENDINDICES13
syn keyword cgSemantic	BLENDINDICES14 BLENDINDICES15

syn keyword cgSemantic	COLOR COLOR0 COLOR1 COLOR2 COLOR3
syn keyword cgSemantic	COLOR4 COLOR5 COLOR6 COLOR7 COLOR8
syn keyword cgSemantic	COLOR9 COLOR10 COLOR11 COLOR12 COLOR13
syn keyword cgSemantic	COLOR14 COLOR15

syn keyword cgSemantic	PSIZE PSIZE0 PSIZE1 PSIZE2 PSIZE3
syn keyword cgSemantic	PSIZE4 PSIZE5 PSIZE6 PSIZE7 PSIZE8
syn keyword cgSemantic	PSIZE9 PSIZE10 PSIZE11 PSIZE12 PSIZE13
syn keyword cgSemantic	PSIZE14 PSIZE15

syn keyword cgSemantic	BINORMAL BINORMAL0 BINORMAL1 BINORMAL2 BINORMAL3
syn keyword cgSemantic	BINORMAL4 BINORMAL5 BINORMAL6 BINORMAL7 BINORMAL8
syn keyword cgSemantic	BINORMAL9 BINORMAL10 BINORMAL11 BINORMAL12 BINORMAL13
syn keyword cgSemantic	BINORMAL14 BINORMAL15

syn keyword cgSemantic	FOG FOG0 FOG1 FOG2 FOG3
syn keyword cgSemantic	FOG4 FOG5 FOG6 FOG7 FOG8
syn keyword cgSemantic	FOG9 FOG10 FOG11 FOG12 FOG13
syn keyword cgSemantic	FOG14 FOG15

syn keyword cgSemantic	DEPTH DEPTH0 DEPTH1 DEPTH2 DEPTH3
syn keyword cgSemantic	DEPTH4 DEPTH5 DEPTH6 DEPTH7 DEPTH8
syn keyword cgSemantic	DEPTH9 DEPTH10 DEPTH11 DEPTH12 DEPTH13
syn keyword cgSemantic	DEPTH14 DEPTH15

syn keyword cgSemantic	SAMPLE SAMPLE0 SAMPLE1 SAMPLE2 SAMPLE3
syn keyword cgSemantic	SAMPLE4 SAMPLE5 SAMPLE6 SAMPLE7 SAMPLE8
syn keyword cgSemantic	SAMPLE9 SAMPLE10 SAMPLE11 SAMPLE12 SAMPLE13
syn keyword cgSemantic	SAMPLE14 SAMPLE15

syn keyword cgSemantic	BLENDWEIGHT BLENDWEIGHT0 BLENDWEIGHT1 POSITION2 
syn keyword cgSemantic  BLENDWEIGHT3
syn keyword cgSemantic	BLENDWEIGHT4 BLENDWEIGHT5 BLENDWEIGHT6 BLENDWEIGHT7 
syn keyword cgSemantic  BLENDWEIGHT8
syn keyword cgSemantic	BLENDWEIGHT9 BLENDWEIGHT10 BLENDWEIGHT11 
syn keyword cgSemantic  BLENDWEIGHT12 BLENDWEIGHT13
syn keyword cgSemantic	BLENDWEIGHT14 BLENDWEIGHT15

syn keyword cgSemantic	NORMAL NORMAL0 NORMAL1 NORMAL2 NORMAL3
syn keyword cgSemantic	NORMAL4 NORMAL5 NORMAL6 NORMAL7 NORMAL8
syn keyword cgSemantic	NORMAL9 NORMAL10 NORMAL11 NORMAL12 NORMAL13
syn keyword cgSemantic	NORMAL14 NORMAL15

syn keyword cgSemantic	FOGCOORD

syn keyword cgSemantic	TEXCOORD TEXCOORD0 TEXCOORD1 TEXCOORD2 TEXCOORD3
syn keyword cgSemantic	TEXCOORD4 TEXCOORD5 TEXCOORD6 TEXCOORD7 TEXCOORD8
syn keyword cgSemantic	TEXCOORD9 TEXCOORD10 TEXCOORD11 TEXCOORD12 TEXCOORD13
syn keyword cgSemantic	TEXCOORD14 TEXCOORD15

syn keyword cgSemantic	COMBINER_CONST0 COMBINER_CONST1
syn keyword cgSemantic	COMBINER_STAGE_CONST0 COMBINER_STAGE_CONST1
syn keyword cgSemantic	OFFSET_TEXTURE_MATRIX OFFSET_TEXTURE_SCALE
syn keyword cgSemantic	OFFSET_TEXTURE_BIAS

" other keywords
syn keyword cgStorageClass	uniform varying
syn keyword cgStructure	interface 
syn keyword cgBoolean	true false

" The minimum and maximum operators in GNU C++
syn match cgMinMax "[<>]?"

" Default highlighting
if version >= 508 || !exists("did_cg_syntax_inits")
  if version < 508
    let did_cg_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink cgType		        Type
  HiLink cgStorageClass	        StorageClass
  HiLink cgStructure		Structure
  HiLink cgSemantic		Constant
  HiLink cgBoolean		Boolean
  delcommand HiLink
endif

let b:current_syntax = "cpp"

" vim: ts=8
