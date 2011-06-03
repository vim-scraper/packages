:sy clear
:sy on
:sy sync ccomment
:sy region gdlComment start=/\/\*/ end=/\*\//
:sy match gdlComment /\/\/.*/
:hi link gdlComment Comment
:sy case match
:sy match gdlKey /\(graph\|edge\|node\|region\|backedge\|\(left\|right\|\)\(bent\|\)nearedge\):\s*{/he=e-1
:sy match gdlErr /\(foldnode\.\|foldedge\.\)\a*/hs=s+9
:sy match gdlErr /\(node\.\|edge\.\)\a*/hs=s+5
:sy match gdlOpt "\(text\|border\)color *:" skipwhite skipempty nextgroup=gdlEColor
:sy match gdlOpt "\(arrow\|backarrow\|\)color *:" skipwhite skipempty nextgroup=gdlEColor
:sy match gdlOpt "\(arrow\|backarrow\|\)style *:" skipwhite skipempty nextgroup=gdlOldArrow,gdlALQuote
:sy match gdlOpt "\(line\|border\)style *:" skipwhite skipempty nextgroup=gdlELine
:sy match gdlOpt "shape *:" skipwhite skipempty nextgroup=gdlEShape
:sy match gdlOpt "color *:" skipwhite skipempty nextgroup=gdlEColor
:sy match gdlVar /\(foldnode\.\|node\.\)color *:/he=e-1 skipwhite skipempty nextgroup=gdlEColor
:sy match gdlVar /\(foldnode\.\|node\.\)\(textcolor\|bordercolor\|color\) *:/he=e-1 skipwhite skipempty nextgroup=gdlEColor
:sy match gdlVar /\(foldedge\.\|edge\.\)\(arrowcolor\|backarrowcolor\|color\) *:/he=e-1 skipwhite skipempty nextgroup=gdlEColor
:sy match gdlVar /\(foldedge\.\|edge\.\)\(arrow\|backarrow\|\)style *:/he=e-1 skipwhite skipempty nextgroup=gdlOldArrow,gdlALQuote
:sy match gdlVar /\(foldedge\.\|edge\.\)linestyle *:/he=e-1 skipwhite skipempty nextgroup=gdlELine
:sy match gdlVar /\(foldnode\.\|node\.\)borderstyle *:/he=e-1 skipwhite skipempty nextgroup=gdlELine
:sy match gdlVar /\(foldnode\.\|node\.\)shape *:/he=e-1 skipwhite skipempty nextgroup=gdlEShape
:sy match gdlReq "\(source\|target\)\(name\|\) *:" skipwhite skipempty nextgroup=gdlTitle
:sy match gdlReq "title *:" skipwhite skipempty nextgroup=gdlTitle
:sy match gdlOpt "label *:" skipwhite skipempty nextgroup=gdlString
:sy match gdlOpt "fontname *:" skipwhite skipempty nextgroup=gdlLQuote
:sy match gdlOpt "iconfile *:" skipwhite skipempty nextgroup=gdlPath
:sy match gdlVar /\(foldnode\.\|node\.\|foldedge\.\|edge\.\)label *:/he=e-1 skipwhite skipempty nextgroup=gdlString
:sy match gdlVar /\(foldnode\.\|node\.\|foldedge\.\|edge\.\)fontname *:/he=e-1 skipwhite skipempty nextgroup=gdlLQuote
:sy match gdlVar /\(foldnode\.\|node\.\)iconfile *:/he=e-1 skipwhite skipempty nextgroup=gdlPath
:sy match gdlOpt /infoname\(1\|2\|3\) *:/ skipwhite skipempty nextgroup=gdlString
:sy match gdlOpt "info\(1\|2\) *:" skipwhite skipempty nextgroup=gdlString
:sy match gdlOpt "info3 *:" skipwhite skipempty nextgroup=gdlInfo
:sy match gdlOpt "spreadlevel *:" skipwhite skipempty nextgroup=gdlEInt
:sy match gdlOpt "\(level\|vertical_\=order\) *:" skipwhite skipempty nextgroup=gdlEInt
:sy match gdlOpt "horizontal_\=order *:" skipwhite skipempty nextgroup=gdlEInt
:sy match gdlOpt "\(height\|width\|borderwidth\|margin\|stretch\|shrink\) *:" skipwhite skipempty nextgroup=gdlEInt
:sy match gdlOpt "\(arrowsize\|backarrowsize\|thickness\|priority\) *:" skipwhite skipempty nextgroup=gdlEInt
:sy match gdlVar /\(foldnode\.\|node\.\)info[123] *:/he=e-1 skipwhite skipempty nextgroup=gdlString
:sy match gdlVar /\(foldnode\.\|node\.\)\(level\|vertical_\=order\) *:/he=e-1 skipwhite skipempty nextgroup=gdlEInt
:sy match gdlVar /\(foldnode\.\|node\.\|foldedge\.\|edge\.\)horizontal_\=order *:/he=e-1 skipwhite skipempty nextgroup=gdlEInt
:sy match gdlVar /\(foldnode\.\|node\.\)\(height\|width\|borderwidth\|margin\|stretch\|shrink\) *:/he=e-1 skipwhite skipempty nextgroup=gdlEInt
:sy match gdlVar /\(foldedge\.\|edge\.\)\(arrowsize\|backarrowsize\|thickness\|priority\) *:/he=e-1 skipwhite skipempty nextgroup=gdlEInt
:sy match gdlOpt "scaling *:" skipwhite skipempty nextgroup=gdlScale,gdlEFloat
:sy match gdlVar /\(foldnode\.\|node\.\)scaling *:/he=e-1 skipwhite skipempty nextgroup=gdlEFloat
:sy match gdlOpt "\(gravity\|tempfactor\|treefactor\) *:" skipwhite skipempty nextgroup=gdlEFloat
:sy match gdlOpt "crossing_\=optimization *:" skipwhite skipempty nextgroup=gdlEBool
:sy match gdlOpt "crossing_\=phase2 *:" skipwhite skipempty nextgroup=gdlEBool
:sy match gdlOpt "\(dirty_edge_\|display_edge_\|displayedge\|late_edge_\|subgraph_\=\)labels *:" skipwhite skipempty nextgroup=gdlEBool
:sy match gdlOpt "s\=manhatt\(a\|e\)n_\=edges *:" skipwhite skipempty nextgroup=gdlEBool
:sy match gdlOpt "\(nodes\|near_\=edges\|edges\|splines\) *:" skipwhite skipempty nextgroup=gdlEBool
:sy match gdlOpt "equal_y_dist *:" skipwhite skipempty nextgroup=gdlEBool
:sy match gdlOpt "equal_\=ydist *:" skipwhite skipempty nextgroup=gdlEBool
:sy match gdlOpt "\(fast_\=\|\)icons *:" skipwhite skipempty nextgroup=gdlEBool
:sy match gdlOpt "fine_\=tuning *:" skipwhite skipempty nextgroup=gdlEBool
:sy match gdlOpt "\(f\=straight_\=\|priority_\)phase *:" skipwhite skipempty nextgroup=gdlEBool
:sy match gdlOpt "ignore_\=singles *:" skipwhite skipempty nextgroup=gdlEBool
:sy match gdlOpt "\(in\|out\|\)port_\=sharing *:" skipwhite skipempty nextgroup=gdlEBool
:sy match gdlOpt "linear_\=segments *:" skipwhite skipempty nextgroup=gdlEBool
:sy match gdlOpt "layout_\=\(up\|down\|near\|spline\)factor *:" skipwhite skipempty nextgroup=gdlEInt
:sy match gdlOpt "splinefactor *:" skipwhite skipempty nextgroup=gdlEInt
:sy match gdlVar /\(foldedge\.\|edge\.\)class *:/he=e-1 skipwhite skipempty nextgroup=gdlIdNr
:sy match gdlOpt "view *:" skipwhite skipempty nextgroup=gdlFish
:sy match gdlOpt "class *:" skipwhite skipempty nextgroup=gdlIdNr
:sy match gdlOpt "anchor *:" skipwhite skipempty nextgroup=gdlEInt
:sy match gdlOpt "hidden *:" skipwhite skipempty nextgroup=gdlIdNr
:sy match gdlOpt "iconcolors *:" skipwhite skipempty nextgroup=gdlEInt
:sy match gdlOpt "stat\(us\|e\) *:" skipwhite skipempty nextgroup=gdlEState
:sy match gdlOpt "layout_\=algorithm *:" skipwhite skipempty nextgroup=gdlEAlg
:sy match gdlOpt "orientation *:" skipwhite skipempty nextgroup=gdlEOrient
:sy match gdlOpt "magnetic_field\(1\|2\) *:" skipwhite skipempty nextgroup=gdlEMagn
:sy match gdlVar /colorentry *\d\d* *:/hs=s+10,he=e-1 skipwhite skipempty nextgroup=gdlERGB
:sy match gdlVar /classname *\d\d* *:/hs=s+9,he=e-1 skipwhite skipempty nextgroup=gdlString
:sy match gdlOpt "arrow_\=mode *:" skipwhite skipempty nextgroup=gdlAMode
:sy match gdlOpt "textmode *:" skipwhite skipempty nextgroup=gdlTMode
:sy match gdlVar /\(foldnode\.\|node\.\)textmode *:/he=e-1 skipwhite skipempty nextgroup=gdlTMode
:sy match gdlOpt "node_\=alignment *:" skipwhite skipempty nextgroup=gdlEAlign
:sy match gdlOpt "crossing_\=weight *:" skipwhite skipempty nextgroup=gdlWeight
:sy match gdlOpt "\(attraction\|repulsion\) *:" skipwhite skipempty nextgroup=gdlEInt
:sy match gdlOpt "random\(factor\|impulse\|rounds\) *:" skipwhite skipempty nextgroup=gdlEInt
:sy match gdlOpt "temp\(scheme\|treshold\|min\|max\) *:" skipwhite skipempty nextgroup=gdlEInt
:sy match gdlOpt "magnetic_force\(1\|2\) *:" skipwhite skipempty nextgroup=gdlEInt
:sy match gdlOpt "\(xlraster\|xlspace\) *:" skipwhite skipempty nextgroup=gdlEInt
:sy match gdlOpt "\(xspace\|xbase\|xmax\|xraster\|x\) *:" skipwhite skipempty nextgroup=gdlEInt
:sy match gdlOpt "\(yspace\|ybase\|ymax\|yraster\|y\) *:" skipwhite skipempty nextgroup=gdlEInt
:sy match gdlOpt "\(a\|b\|c\|fd\|p\|r\|s\)\(max\) *:" skipwhite skipempty nextgroup=gdlEInt
:sy match gdlOpt "\(c\|p\|r\)\(min\) *:" skipwhite skipempty nextgroup=gdlEInt
:sy region gdlEColor start=/\S/ end=/\(\n\| \|}\)/he=e-1 contains=gdlColor contained
:sy region gdlERGB start=/\S/ end=/\(\n\| \|}\)/he=e-1 contains=gdlRGB contained
:sy match gdlEArrow /\(\a\| \)*/ contains=gdlArrow nextgroup=gdlRQuote contained
:sy region gdlEShape start=/\S/ end=/\(\n\| \|}\)/he=e-1 contains=gdlShape contained
:sy region gdlEState start=/\S/ end=/\(\n\| \|}\)/he=e-1 contains=gdlState contained
:sy region gdlEAlg start=/\S/ end=/\(\n\| \|}\)/he=e-1 contains=gdlAlg contained
:sy region gdlELine start=/\S/ end=/\(\n\| \|}\)/he=e-1 contains=gdlLine contained
:sy region gdlEInt start=/\S/ end=/\(\n\| \|}\)/he=e-1 contains=gdlInt contained
:sy region gdlEFloat start=/\S/ end=/\(\n\| \|}\)/he=e-1 contains=gdlFloat contained
:sy region gdlEOrient start=/\S/ end=/\(\n\| \|}\)/he=e-1 contains=gdlOrient contained
:sy region gdlEMagn start=/\S/ end=/\(\n\| \|}\)/he=e-1 contains=gdlMagn contained
:sy region gdlEAlign start=/\S/ end=/\(\n\| \|}\)/he=e-1 contains=gdlAlign contained
:sy region gdlEBool start=/\S/ end=/\(\n\| \|}\)/he=e-1 contains=gdlBool contained
:sy match gdlColor "\d\d*" contained
:sy keyword gdlColor white blue red green yellow magenta cyan pink orange black gold lilac turquoise aquamarine contained
:sy keyword gdlColor darkgrey darkgray darkblue darkred darkgreen darkyellow darkmagenta darkcyan khaki purple orchid contained
:sy keyword gdlColor lightgrey lightgray lightblue lightred lightgreen lightyellow lightmagenta lightcyan yellowgreen contained
:sy match gdlRGB "\d\d* \+\d\d* \+\d\d*" contained
:sy keyword gdlShape box triangle circle ellipse hexagon rhomb rhomboid trapeze contained
:sy keyword gdlShape uptrapeze trapezoid uptrapezoid lparallelogram rparallelogram contained
:sy keyword gdlStyle solid line none contained
:sy keyword gdlState unfolded folded boxed clustered wrapped exclusive white contained
:sy keyword gdlAlg normal tree forcedir dfs minbackward contained
:sy keyword gdlAlg maxdepth maxdepthslow maxindegree maxoutdegree maxdegree contained
:sy keyword gdlAlg mindepth mindepthslow minindegree minoutdegree mindegree contained
:sy match gdlArrow /\(none\|solid\|line\|filled\|half\|circle\|rhomb\|box\|diamond\|slash\|dslash\| \|_\)/ contained
:sy keyword gdlOldArrow none solid line contained
:sy keyword gdlLine solid continuous dashed dotted double triple invisible contained
:sy keyword gdlOrient toptobottom bottomtotop lefttoright righttoleft contained
:sy keyword gdlOrient top_to_bottom bottom_to_top left_to_right right_to_left contained
:sy keyword gdlMagn toptobottom bottomtotop lefttoright righttoleft contained
:sy keyword gdlMagn top_to_bottom bottom_to_top left_to_right right_to_left contained
:sy keyword gdlMagn polar polcircular circular orthogonal no contained
:sy keyword gdlAMode free fixed contained
:sy keyword gdlTMode center left_justify right_justify contained
:sy keyword gdlWeight medianbary barymedian median bary contained
:sy keyword gdlAlign center top bottom contained
:sy match gdlIdNr "\d\d*" contained
:sy match gdlInt /-\=\d\d*/ contained
:sy match gdlFloat /\d\d*\.\d\d*/ contained
:sy match gdlScale /maxspect/ contained
:sy region gdlTitle start=/"/ skip=/\\"/ end=/"/ contains=gdlChar contained
:sy region gdlString start=/"/ skip=/\\"/ end=/"/ contains=gdlChar contained
:sy region gdlInfo start=/"/ skip=/\\"/ end=/"/ contains=gdlHref,gdlChar contained
:sy region gdlPath start=/"/ skip=/\\"/ end=/"/ contains=gdlSlash contained
:sy match gdlChar "\\\(n\|t\|b\|a\)" contained
:sy match gdlChar "\\fi\(0\|1\|2\)\d\d" contained
:sy match gdlChar "\\f\(u\|I\|b\|B\|n\|\d\d\)" contained
:sy match gdlHref /\(href:\|target:\|onClick:\|onMouseOut:\|onMouseOver:\|onMouseDown:\|onMouseUp:\|onMouseMove\)/ contained
:sy match gdlSlash "\(\\\|\/\)" contained
:sy match gdlLQuote /"/ nextgroup=gdlEFName contained
:sy match gdlEFName /\a*/ contains=gdlFName nextgroup=gdlEFSize contained
:sy match gdlFName "\(\(tim\|ncen\)\(R\|BI\|I\|B\)\|\(cour\|helv\)\(R\|BO\|O\|B\)\|symb\)" nextgroup=gdlEFSize contained
:sy match gdlEFSize /\d*/ contains=gdlFSize nextgroup=gdlRQuote contained
:sy match gdlFSize "\(08\|10\|12\|14\|18\|24\)" nextgroup=gdlRQuote contained
:sy match gdlALQuote /"/ nextgroup=gdlEArrow contained
:sy match gdlRQuote /"/ contained
:sy match gdlFish /\(f\|d\|\)\(p\|c\)fish/ contained
:sy keyword gdlBool yes no contained
:hi link gdlKey Statement
:hi link gdlErr Error
:hi link gdlEAlg Error
:hi link gdlEAlign Error
:hi link gdlEArrow Error
:hi link gdlEBool Error
:hi link gdlEColor Error
:hi link gdlEFloat Error
:hi link gdlEFName Error
:hi link gdlEFSize Error
:hi link gdlEInt Error
:hi link gdlELine Error
:hi link gdlEMagn Error
:hi link gdlEOrient Error
:hi link gdlERGB Error
:hi link gdlEShape Error
:hi link gdlEState Error
:hi link gdlString String
:hi link gdlInfo String
:hi link gdlPath String
:hi link gdlChar Special
:hi link gdlHref Special
:hi link gdlSlash Special
:hi link gdlBool Constant
:hi link gdlInt Constant
:hi link gdlFloat Constant
:hi link gdlRGB Constant
:hi link gdlReq Normal
:hi link gdlOpt Normal
:hi link gdlColor Identifier
:hi link gdlIdNr Identifier
:hi link gdlVar Identifier
:hi link gdlTitle Identifier
:hi link gdlArrow Type
:hi link gdlOldArrow Type
:hi link gdlAlg Type
:hi link gdlAlign Type
:hi link gdlAMode Type
:hi link gdlALQuote Type
:hi link gdlARQuote Type
:hi link gdlFish Type
:hi link gdlFName Type
:hi link gdlFSize Type
:hi link gdlLine Type
:hi link gdlLQuote Type
:hi link gdlMagn Type
:hi link gdlOrient Type
:hi link gdlRQuote Type
:hi link gdlScale Type
:hi link gdlShape Type
:hi link gdlState Type
:hi link gdlStyle Type
:hi link gdlTMode Type
:hi link gdlWeight Type
