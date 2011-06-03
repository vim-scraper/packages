" +{{{------------------------------------+::|||[[ ]]|||::+---------------------------------------+
" | MacroMedia Flash MX Vim Syntax File                                                           |
" | Maintainer : Timothy Aldrich                                                                  |
" | Version    : 0.01                                                                             |
" +---------------------------------------+::|||[[ ]]|||::+------------------------------------}}}+
" |                                                                                               |
" +{{{------------------------------------+::|||[[ ]]|||::+---------------------------------------+
" | CHANGELOG                                                                                     |
" | 20030123 : initial write                                                                      |
" +---------------------------------------+::|||[[ ]]|||::+------------------------------------}}}+

if exists("b:current_syntax")
        syntax clear
        let b:current_syntax = "as"
endif



" Symbolic operators 
syntax keyword ASOperator       -- ++ !  != !== % %= & && &= () - * *= , .  ?: /  /= ^ ^= \| \|\| \|= ~ + += < << <<= <= <> = -= == === > >= >> >>= >>> >>>= . 
" all other operators
syntax keyword ASOperator       and eq ge gt le lt ne not or 

syntax keyword ASConstant       BACKSPACE CAPSLOCK CONTROL DELETEKEY DOWN END ENTER ESCAPE HOME INSERT LEFT LN2 LN10 LOG2E LOG10E MAX_VALUE MIN_VALUE NEGATIVE_INFINITY PGDN PGUP PI RIGHT SHIFT SPACE SQRT1_2 SQRT2 TAB UP UTC 

syntax region ASString         start=+\"+           end=+\"+


" TODO  separate keywords from class methods
" syntax keyword ASKeyword        

syntax keyword ASConditional    else if 


syntax keyword ASBoolean        false true 

syntax keyword ASFunction       add addItem  addItemAt addListener addProperty align _alpha appendChild apply applyChanges arrow asfunction attachMovie attachSound attributes autosize 

syntax keyword ASFunction       background backgroundColor backgroundDisabled  beginFill beginGradientFill blockIndent bold border borderColor bottomScroll bullet Button

syntax keyword ASFunction       callee caller capabilities ceil charAt charCodeAt check childNodes clear clearInterval cloneNode close concat connect constructor contentType ccntinue cos createElement createEmptyMovieClip createTextField createTextNode _currentframe curveTo 

syntax keyword ASFunction       darkshadow default delete docTypeDecl _droptarget duplicateMovieClip duration 

syntax keyword ASFunction       endinitclip embedFonts enabled endFill escape eval evaluate exp 

syntax keyword ASFunction       face FCheckBox FComboBox firstChild FListBox floor focusEnabled _focusrect font foregroundDisabled FPushButton FRadioButton _framesloaded fromCharCode fscommand FScrollBar FScrollPane FStyleFormat function 

syntax keyword ASFunction       get getAscii getBeginIndex getBounds getBytesLoaded getBytesTotal getCaretIndex getCode getData getDate getDay getDepth getEnabled getEndIndex getFocus getFontList getFullYear getHours getItemAt getLabel getLength getMilliseconds getMinutes getMonth getNewTextFormet getPan getPaneHeight getPaneWidth getProperty getRowCount getRGB getScrollContent getScrollPosition getSeconds getSelectedIndex getSelectedIndices getSelectedItem getSelectedItems getSelectMultiple getState getTextExtent getTextFormat getTime getTimer getTimezoneOffset getTransform getURL getUTCDate getUTCDay getUTCFullYear getUTCHours getUTCMilliseconds getUTCMinutes getUTCMonth getUTCSeconds getValue getVersion getVolume getYear _global globalStyleFormat globalToLocal gotoAndPlay gotoAndStop 

syntax keyword ASFunction       hasAccessibility hasAudio hasAudioEncoder hasMP3 hasVideoEncoder hasChildNodes height _height hide highlight highlight3D _highquality hitArea hitTest hscroll html htmlText 

syntax keyword ASFunction       ifFrameLoaded ignoreWhite  indent indexOf insertBefore install instanceof isActive isDown isFinite isNaN isToggled italic 

syntax keyword ASFunction       join 

syntax keyword ASFunction       language lastChild lastIndexOf leading leftMargin length level lineStyle lineTo list load loaded loadMovie loadMovieNum loadScrollContent loadSound loadVariables loadVariablesNum LoadVars localToGlobal 

syntax keyword ASFunction       manufacturer Math maxChars maxhscroll maxscroll mbchr mblength mbord mbsubstring method Mouse moveTo MovieClip multiline 

syntax keyword ASFunction       _name NaN new newline nextFrame nextScene nextSibling nodeName nodeType nodeValue null 

syntax keyword ASFunction       on onClipEvent onClose onChanged onConnect onData onDragOut onDragOver onEnterFrame onKeyDown onKeyUp onKillFocus onLoad onMouseDown onMouseMove onMouseUp onPress onRelease onReleaseOutisde onResize onRollOut onRollOver onScroller onSetFocus onSort onSoundComplete onUnload onXML ord os 

syntax keyword ASFunction       _parent parentNode parseFloat parseInt parseXML password  pixelAspectRatio play pop position POSITIVE_INFINITY pow prevFrame previousSibling prevScene print printAsBitmap printAsBitmapNum printNum __proto__ push 

syntax keyword ASFunction       _quality 

syntax keyword ASFunction       radioDot refreshPane registerClass registerSkinElement removeAll removeItemAt removeListener removeMovieClip removeNode removeTextField replaceItemAt replaceSel resolutionX resolutionY restrict reverse rightMargin _root _rotation 

syntax keyword ASFunction       scaleMode screenColor screenDPI screenResolution.x screenResolution.y scroll scrollTrack selectable selection Selection selectionDisabled selectionUnfocused send sendAndLoad set variable setAutoHideScrollBar setChangeHandler setClickHandler setData setDataProvider setDate setDragContent setEditable setEnabled setFocus setFullYear setGroupName setHorizontal setHours setHScroll setInterval setItemSymbol setLabel setLabelPlacement setLargeScroll setMask setMilliseconds setMinutes setMonth setNewTextFormat setPan setProperty setRGB setRowCount setScrollContent setScrollPosition setScrollProperties setScrollTarget setSeconds setSelectedIndex setSelectedIndices setSelection setSelectMultiple setSize setSmallScroll setState setStyleProperty setTextFormat setTime setTransform setUTCDate setUTCFullYear setUTCHours setUTCMilliseconds setUTCMinutes setUTCMonth setUTCSeconds setValue setVolume setVScroll setWidth setYear shadow shift show size slice sort sortItemsBy Sound _soundbuftime splice split sqrt start startDrag status stop stopAllSounds stopDrag substr substring super swapDepths System 

syntax keyword ASFunction       tabChildren tabEnabled tabIndex tabStops target _target targetPath tellTarget text textAlign textBold textColor textDisabled TextField textFont TextFormat textHeight textIndent textItalic textLeftMargin textRightMargin textSelected textSize textUnderline textWidth this toggleHighQuality toLowerCase toString _totalframes toUpperCase trace trackAsMenu type typeof 

syntax keyword ASFunction       undefined underline unescape uninstall unloadMovie unLoadMovieNum unshift unwatch updateAfterEvent url _url useHandCursor 

syntax keyword ASFunction       valueOf version _visible 

syntax keyword ASFunction       watch width _width wordwrap 

syntax keyword ASFunction       _x XML xmlDecl XMLSocket _xmouse _xscale 

syntax keyword ASFunction       _y _ymouse _yscale 

syntax keyword ASStatement     abs acos asin atan atan2 log min max round random sin tan with var variable void 

syntax keyword ASStatement     break call return  

syntax keyword ASRepeat        do while for switch 

syntax keyword ASLabel         case goto #initclip #endinitclip

syntax match ASInclude        "^\s*#include"

syntax keyword ASType          Array Arguments Boolean Color Date int chr Key Number Object String 

syntax keyword ASTodo          TODO NOTE XXX


syntax region ASComment     start="/\*"         end="\*/"
syntax region ASComment     start="//"          end="$"
syntax region ASString      start=+"+           end=+"+

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_as_syn_inits")
  if version < 508
    let did_as_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink ASBoolean               Boolean
  HiLink ASComment               Comment
  HiLink ASConstant              Constant
  HiLink ASConditional           Conditional
  HiLink ASKeyword               Keyword
  HiLink ASInclude               Include
  HiLink ASLabel                 Label
  HiLink ASOperator              Operator
  HiLink ASString                String
  HiLink ASFunction              Function
  HiLink ASStatement             Statement
  HiLink ASTodo                  Todo
  HiLink ASType                  Type
  
  delcommand HiLink
endif

