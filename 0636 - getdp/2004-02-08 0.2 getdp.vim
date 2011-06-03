" Vim syntax file
" Language: GetDP
" Author: Patricio Toledo <patoledo@ing.uchile.cl>
" Last Change: dom may 11 18:01:00 CLT 2003

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn sync maxlines=200
syn sync minlines=40

syn keyword getdpBasisFunction BF_CurlGroupOfPerpendicularEdge
syn keyword getdpBasisFunction BF_dGlobal BF_NodeX BF_NodeY BF_NodeZ
syn keyword getdpBasisFunction BF_DivPerpendicularFacet BF_Region
syn keyword getdpBasisFunction BF_GradNode BF_CurlEdge BF_DivFacet
syn keyword getdpBasisFunction BF_GroupOfEdges BF_CurlGroupOfEdges
syn keyword getdpBasisFunction BF_GroupOfNodes BF_GradGroupOfNodes
syn keyword getdpBasisFunction BF_GroupOfPerpendicularEdge
syn keyword getdpBasisFunction BF_Node BF_Edge BF_Facet BF_Volume
syn keyword getdpBasisFunction BF_PerpendicularEdge BF_CurlPerpendicularEdge
syn keyword getdpBasisFunction BF_PerpendicularFacet
syn keyword getdpBasisFunction BF_RegionX BF_RegionY BF_RegionZ BF_Global
syn keyword getdpBasisFunction BF_Zero BF_One AliasOf AssociatedWith
syn keyword getdpConstraint Assign Init AssignFromResolution
syn keyword getdpConstraint InitFromResolution Network Link LinkCplx 
syn keyword getdpDefine DefineConstant DefineGroup DefineFunction
syn keyword getdpElement Line Triangle Quadrangle Tetrahedron Hexahedron
syn keyword getdpElement Prism Pyramid Point
syn keyword getdpFormulation Dof Dt DtDof DtDt DtDtDof JacNL NeverDt
syn keyword getdpFormulation FemEquation FemEquation Galerkin deRham Local
syn keyword getdpFormulation Global Integral LocalQuantity Symmetry
syn keyword getdpFunction Complex Re Vector Tensor TensorV TensorSym TensorDiag
syn keyword getdpFunction CompX CompY CompZ 
syn keyword getdpFunction CompXX CompXY CompYZ
syn keyword getdpFunction CompYX CompYY CompYZ
syn keyword getdpFunction CompZX CompZY CompZZ 
syn keyword getdpFunction List ListAlt StrCat
syn keyword getdpFunctionSpace Form0 Form1 Form2 Form3 Form1P Form2P Scalar 
syn keyword getdpFunctionSpace SubSpace 
syn keyword getdpFunction X Y Z XYZ 
syn keyword getdpGreen Laplace GradLaplace Helmholtz GradHelmholtz
syn keyword getdpGroup DualEdgesOf DualFacetsOf DualVolumesOf
syn keyword getdpGroup EdgesOfTreeIn FacetsOfTreeIn 
syn keyword getdpGroup FacetsOfVolumesOf ElementsOf GroupsOfNodesOf 
syn keyword getdpGroup GroupsOfEdgesOf GroupsOfEdgesOnNodesOf 
syn keyword getdpGroup Region Global NodesOf EdgesOf 
syn keyword getdpInclude Include
syn keyword getdpIntegration Gauss GaussLegendre
syn keyword getdpJacobian VolAxiRectShell VolAxiSquRectShell
syn keyword getdpJacobian VolAxiSphShell VolAxiSquSphShell VolRectShell
syn keyword getdpJacobian Vol Sur Lin VolAxi SurAxi VolAxiSqu VolSphShell
syn keyword getdpMath Atan2 Sinh Cosh Tanh Fabs Fmod Cross Hypot Norm
syn keyword getdpMathFunction Exp Log Log10 Sqrt Sin Asin Cos Acos Atan 
syn keyword getdpMathFunction F_Period SquNorm
syn keyword getdpMathFunction Unit Transpose TTrace F_Cos_wt_p F_Sin_wt_p
syn keyword getdpMiscFunction dInterpolationAkima Order 
syn keyword getdpMiscFunction dInterpolationLinear InterpolationAkima
syn keyword getdpMiscFunction F_CompElementNum InterpolationLinear
syn keyword getdpMiscFunction Printf Normal NormalSource 
syn keyword getdpName BasisFunction Entity Quantity Equation Operation
syn keyword getdpName NameOfCoef NameOfConstraint NameOfFormulation
syn keyword getdpName NameOfMesh NameOfPostProcessing NameOfSpace 
syn keyword getdpName NameOfSystem System 
syn keyword getdpName Name Type Case NameOfResolution NameOfBasisFunction
syn keyword getdpObject Group Function Constraint FunctionSpace
syn keyword getdpObject Jacobian Integration Formulation Resolution
syn keyword getdpObject PostProcessing PostOperation
syn keyword getdpPostOperation ChangeOfValues GmshGmshParsed Table
syn keyword getdpPostOperation Depth Skin Smoothing HarmonicToTime Dimension
syn keyword getdpPostOperation OnElementsOf OnRegion OnGlobal OnSection
syn keyword getdpPostOperation OnGrid OnPoint OnLine OnPlane OnBox File
syn keyword getdpPostOperation Sort Iso NoNewLine ChangeOfCoordinates
syn keyword getdpPostOperation TimeStep Frequency Format Adapt Target Value
syn keyword getdpPostOperation TimeTable Gnuplot Adaptation
syn keyword getdpPostProcessing Local Integral 
syn keyword getdpResolution FourierTransform TimeLoopTheta TimeLoopNewmark
syn keyword getdpResolution GenerateSeparate Update InitSolution
syn keyword getdpResolution Generate Solve GenerateJac SolveJac
syn keyword getdpResolution IterativeLoop
syn keyword getdpResolution SaveSolution SaveSolutions TransferSolution
syn keyword getdpResolution SystemCommand If Else Print Lanczos
syn keyword getdpResolution TransferInitSolution SetTime SetFrequency
syn keyword getdpSpec Not All

syn match getdpArgument "\$\(\d\|\w\)\+"
syn match getdpArithmeticOperator "[-+%]" contained
syn match getdpArithmeticOperator "[\/\|\*]" contained
syn match getdpColons "[,;:]"
syn match getdpError "[}\])]" 
syn match getdpInclude "#include\s\+"
syn match getdpLogicalOperator "[||\|&&]"
syn match getdpLogicalOperator "[!]="
syn match getdpNumber "\<\d\+\(\.\d*\)\=\([edED][-+]\=\d\+\)\=[ij]\=\>"
syn match getdpNumber "\.\d\+\([edED][-+]\=\d\+\)\=[ij]\=\>"
syn match getdpNumber "\<\d\+[ij]\=\>"
syn match getdpRegister "\#\d"
syn match getdpRelationalOperator "[<>]=\="

syn region getdpComment matchgroup=Comment start="\/\*" end="\*\/" contains=getdpString,getdpNumber
syn region getdpComment start="//" end="$"
syn region getdpMatch matchgroup=Identifier start="(" end=")" contains=ALL fold 
syn region getdpMatch matchgroup=Identifier start="{" end="}" contains=ALL fold
syn region getdpMatch matchgroup=Identifier start="\[" end="]" contains=ALL fold
syn region getdpString start=+"+ end=+"+ oneline

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet

if version >= 508 || !exists("did_getdp_syntax_inits")
   if version < 508
     let did_getdp_syntax_inits = 1
     command -nargs=+ HiLink hi link <args>
   else
     command -nargs=+ HiLink hi def link <args>
   endif
   HiLink getdpArgument String
   HiLink getdpArithmeticOperator Operator
   HiLink getdpBasisFunction Type
   HiLink getdpColons Special
   HiLink getdpComment Comment
   HiLink getdpConstant Operator
   HiLink getdpConstraint Type
   HiLink getdpDefine Type
   HiLink getdpElement Type
   HiLink getdpError Error
   HiLink getdpFormulation Type
   HiLink getdpFunction Function
   HiLink getdpFunctionSpace Type
   HiLink getdpGreen Type
   HiLink getdpGroup Type
   HiLink getdpInclude Include 
   HiLink getdpIntegration Type
   HiLink getdpJacobian Type
   HiLink getdpLogicalOperator Operator
   HiLink getdpMath Function
   HiLink getdpMathFunction Function
   HiLink getdpMiscFunction Function
   HiLink getdpName Operator
   HiLink getdpNumber Number
   HiLink getdpObject SpecialKey
   HiLink getdpPostOperation Type
   HiLink getdpPostProcessing Type
   HiLink getdpRegister String
   HiLink getdpRelationalOperator Operator
   HiLink getdpRepeat Repeat
   HiLink getdpResolution Type
   HiLink getdpSpec Underlined
   HiLink getdpString String
   delcommand HiLink
endif

let b:current_syntax = "getdp"
