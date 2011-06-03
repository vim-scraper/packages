" Vim syntax file
" Language:     TikiWiki
" Maintainer:   Olivier Teuliere <ipkiss@via.ecp.fr>
" Version: 1.0
" Last Change:  2006 Aug 29

" Syntax file to use a wiki-editor with VIM (for example using the Mozex
" plugin for Firefox: http://mozex.mozdev.org/

" Variables to control the highlighting:
" tikiwiki_no_wikiword      Wiki words (LikeThis) are not highlighted
" tikiwiki_no_rendering     The bold/italic/underline attributes are not
"                           rendered directly on the text
" tikiwiki_my_rendering     The default mappings for rendering are ignored,
"                           you can use your own ones (similar to html.vim)

" Quit when a syntax file was already loaded
if exists("b:current_syntax")
    finish
endif

syn case match

" Plugins
syn keyword tikiwikiPluginName      contained AGENTINFO ALINK ANAME ARTICLE ARTICLES ATTACH AVATAR BACKLINKS BOX CATEGORY CATORPHANS CATPATH CENTER CODE COPYRIGHT COUNTDOWN DIV DL EXAMPLE FANCYTABLE FLASH FORMULA GAUGE JABBER LANG MAP MINIQUIZ MODULE MONO OBJECTHITS PLUGINMANAGER POLL QUOTE REGEX RSS SF SHEET SHOWPAGES SORT SPLIT SQL SUB THUMB TITLESEARCH TRACKER TRACKERLIST TRANSLATED USERCOUNT VERSIONS WANTEDPAGES
syn match   tikiwikiPluginArrows    contained /=>\|,/
syn match   tikiwikiPluginUnknown   contained /[A-Z]\+/
syn match   tikiwikiPluginBraces    contained /[{}()]/
syn match   tikiwikiPluginAttrName  contained /\w\+=>/me=e-2
syn match   tikiwikiPluginAttrValue contained /=>[^,)]\+/hs=s+2 contains=tikiwikiPluginArrows
syn match   tikiwikiPluginBegin     contained /{[A-Z]\+(\([a-z]\+=>"\?\w\+"\?\(,[a-z]\+=>"\?\w\+"\?\)*\)\?)}/ contains=tikiwikiPluginBraces,tikiwikiPluginName,tikiwikiPluginAttrName,tikiwikiPluginAttrValue,tikiwikiPluginArrows
syn match   tikiwikiPluginEnd       contained /{[A-Z]\+}/ contains=tikiwikiPluginBraces,tikiwikiPluginName,tikiwikiPluginUnknown
syn region  tikiwikiPlugin          start=/{\z([A-Z]\+\)(\([a-z]\+=>"\?\w\+"\?\(,[a-z]\+=>"\?\w\+"\?\)*\)\?)}/ end=/{\z1}/ keepend contains=@tikiwikiText,tikiwikiPlugin,tikiwikiPluginBegin,tikiwikiPluginEnd


" Color groups
syn match   tikiwikiColorError      contained /.\+/
syn case ignore
" List from http://doc.tikiwiki.org/tiki-index.php?page=Wiki-Syntax%20Text
syn keyword tikiwikiColorNumber     contained AliceBlue AntiqueWhite Aqua Aquamarine Azure Beige Bisque Black BlanchedAlmond Blue BlueViolet Brown BurlyWood CadetBlue Chartreuse Chocolate Coral CornflowerBlue Cornsilk Crimson Cyan DarkBlue DarkCyan DarkGoldenRod DarkGray DarkGreen DarkKhaki DarkMagenta DarkOliveGreen Darkorange DarkOrchid DarkRed DarkSalmon DarkSeaGreen DarkSlateBlue DarkSlateGray DarkTurquoise DarkViolet DeepPink DeepSkyBlue DimGray DodgerBlue Feldspar FireBrick FloralWhite ForestGreen Fuchsia Gainsboro GhostWhite Gold GoldenRod Gray Green GreenYellow HoneyDew HotPink IndianRed Indigo Ivory Khaki Lavender LavenderBlush LawnGreen LemonChiffon LightBlue LightCoral LightCyan LightGoldenRodYellow LightGrey LightGreen LightPink LightSalmon LightSeaGreen LightSkyBlue LightSlateBlue LightSlateGray LightSteelBlue LightYellow Lime LimeGreen Linen Magenta Maroon MediumAquaMarine MediumBlue MediumOrchid MediumPurple MediumSeaGreen MediumSlateBlue MediumSpringGreen MediumTurquoise MediumVioletRed MidnightBlue MintCream MistyRose Moccasin NavajoWhite Navy OldLace Olive OliveDrab Orange OrangeRed Orchid PaleGoldenRod PaleGreen PaleTurquoise PaleVioletRed PapayaWhip PeachPuff Peru Pink Plum PowderBlue Purple Red RosyBrown RoyalBlue SaddleBrown Salmon SandyBrown SeaGreen SeaShell Sienna Silver SkyBlue SlateBlue SlateGray Snow SpringGreen SteelBlue Tan Teal Thistle Tomato Turquoise Violet VioletRed Wheat White WhiteSmoke Yellow YellowGreen
syn case match
syn match   tikiwikiColorNumber     contained /\#[a-fA-F0-9]\{6}/
syn region  tikiwikiColorBegin      matchgroup=tikiwikiMarker contained start=/\~\~/ end=/:/ oneline keepend contains=tikiwikiColorError,tikiwikiColorNumber
syn region  tikiwikiColor           start=/\~\~.\{-}:/ matchgroup=tikiwikiMarker end=/\~\~/ keepend contains=@tikiwikiText,tikiwikiColorBegin


" Generic stuff
syn match   tikiwikiLine            /^----$/
if !exists("tikiwiki_no_wikiword")
    syn match   tikiwikiLinkWord    /\<\([A-Z][a-z0-9_-]\+\)\{2}[A-Za-z0-9-]*\>/
endif
syn region  tikiwikiLink            start=/(([^(]/hs=s+2 end=/))/he=e-2
syn region  tikiwikiLink            start=/\[[^\[]/hs=s+1 end=/\]/he=e-1
syn region  tikiwikiLink            start=/[^\[]\[[^\[]/hs=s+2,ms=s+1 end=/\]/he=e-1
syn match   tikiwikiNoLink          /))[a-zA-Z0-9_-]\+((/
syn match   tikiwikiColorNameIgn    /\#[a-fA-F0-9]\+/
syn match   tikiwikiToc             /{toc}\|{maketoc}\|{cookie}/
syn match   tikiwikiImageAttrName   contained /\w\+=/me=e-1
syn match   tikiwikiImageAttrValue  contained /=[^ \t}]\+/hs=s+1
syn region  tikiwikiImageAttrValue  contained start=/="/hs=s+1 end=/"/ extend
syn region  tikiwikiImage           start=/{img\s\+src=/ end=/}/ keepend contains=tikiwikiImageAttrName,tikiwikiImageAttrValue
syn region  tikiwikiImage           start=/{picture\s\+file=/ end=/}/ keepend contains=tikiwikiImageAttrName,tikiwikiImageAttrValue
syn match   tikiwikiDirection       /{l2r}\|{r2l}\|{rm}\|{lm}/

syn match   tikiwikiIndent          /%%%/
syn match   tikiwikiListItem        /^\(\*\|\#\|+\)\+/
syn region  tikiwikiListItemDef     matchgroup=tikiwikiListItem start=/^;/ end=/:/ contains=@tikiwikiText
syn match   tikiwikiMarker          "\~\/\?hs\~"
syn match   tikiwikiMarker          /\~\d\+\~/
syn match   tikiwikiTablecolumn     contained /|/
syn region  tikiwikiTable           start=/||/ end=/||/ contains=@tikiwikiText,tikiwikiTableColumn keepend

syn region  tikiwikiTitleBar        matchgroup=tikiwikiChars start=/-=/ end=/=-/ oneline
syn region  tikiwikiHeading         matchgroup=tikiwikiChars start=/^!\+[+-]\?/ end=/$/ oneline keepend contains=@tikiwikiText
syn region  tikiwikiCenter          matchgroup=tikiwikiChars start=/::/ end=/::/ oneline contains=@tikiwikiText
syn region  tikiwikiBox             matchgroup=tikiwikiChars start=/\^/ end=/\^/ contains=@tikiwikiText,tikiwikiHeading
syn region  tikiwikiMonospaced      matchgroup=tikiwikiChars start=/-+/ end=/+-/ contains=@tikiwikiText

syn region  tikiwikiNoProcess       matchgroup=tikiwikiMarker start=/\~np\~/ end=/\~\/np\~/ keepend
syn region  tikiwikiNoProcess       matchgroup=tikiwikiMarker start=/\~pp\~/ end=/\~\/pp\~/ keepend
syn region  tikiwikiNoProcess       start=/{CODE(.\{-})}/ matchgroup=tikiwikiPluginEnd end=/{CODE}/ keepend contains=tikiwikiPluginBegin

syn cluster tikiwikiText            contains=tikiwikiBold,tikiwikiItalic,tikiwikiUnderline,tikiwikiListItem,tikiwikiListItemDef,tikiwikiMarker,tikiwikiBox,tikiwikiCenter,tikiwikiIndent,tikiwikiNoProcess,tikiwikiMonospaced,tikiwikiColor,tikiwikiLink,tikiwikiColorNameIgn,tikiwikiImage,tikiwikiDirection,tikiwikiLinkWord,tikiwikiNoLink

if !exists("tikiwiki_no_rendering")
    syn region tikiwikiBold                 matchgroup=tikiwikiChars start=/__/  end=/__/  contains=@tikiwikiText,tikiwikiBoldItalic,tikiwikiBoldUnderline
    syn region tikiwikiItalic               matchgroup=tikiwikiChars start=/''/  end=/''/  contains=@tikiwikiText,tikiwikiItalicBold,tikiwikiItalicUnderline
    syn region tikiwikiUnderline            matchgroup=tikiwikiChars start=/===/ end=/===/ contains=@tikiwikiText,tikiwikiUnderlineBold,tikiwikiUnderlineItalic

    syn region tikiwikiBoldItalic           matchgroup=tikiwikiChars contained start=/''/  end=/''/  contains=@tikiwikiText,tikiwikiBoldItalicUnderline
    syn region tikiwikiBoldUnderline        matchgroup=tikiwikiChars contained start=/===/ end=/===/ contains=@tikiwikiText,tikiwikiBoldUnderlineItalic
    syn region tikiwikiBoldItalicUnderline  matchgroup=tikiwikiChars contained start=/===/ end=/===/ contains=@tikiwikiText,tikiwikiUnderlineBoldItalic,tikiwikiUnderlineItalicBold
    syn region tikiwikiBoldUnderlineItalic  matchgroup=tikiwikiChars contained start=/''/  end=/''/  contains=@tikiwikiText,tikiwikiItalicBoldUnderline,tikiwikiItalicUnderlineBold

    syn region tikiwikiItalicBold           matchgroup=tikiwikiChars contained start=/__/  end=/__/  contains=@tikiwikiText,tikiwikiItalicBoldUnderline
    syn region tikiwikiItalicUnderline      matchgroup=tikiwikiChars contained start=/===/ end=/===/ contains=@tikiwikiText,tikiwikiItalicUnderlineBold
    syn region tikiwikiItalicBoldUnderline  matchgroup=tikiwikiChars contained start=/===/ end=/===/ contains=@tikiwikiText,tikiwikiUnderlineBoldItalic,tikiwikiUnderlineItalicBold
    syn region tikiwikiItalicUnderlineBold  matchgroup=tikiwikiChars contained start=/__/  end=/__/  contains=@tikiwikiText,tikiwikiBoldItalicUnderline,tikiwikiBoldUnderlineItalic

    syn region tikiwikiUnderlineBold        matchgroup=tikiwikiChars contained start=/__/  end=/__/  contains=@tikiwikiText,tikiwikiUnderlineBoldItalic
    syn region tikiwikiUnderlineItalic      matchgroup=tikiwikiChars contained start=/''/  end=/''/  contains=@tikiwikiText,tikiwikiUnderlineItalicBold
    syn region tikiwikiUnderlineBoldItalic  matchgroup=tikiwikiChars contained start=/''/  end=/''/  contains=@tikiwikiText,tikiwikiItalicBoldUnderline,tikiwikiItalicUnderlineBold
    syn region tikiwikiUnderlineItalicBold  matchgroup=tikiwikiChars contained start=/__/  end=/__/  contains=@tikiwikiText,tikiwikiBoldItalicUnderline,tikiwikiBoldUnderlineItalic
endif

" Define the default highlighting.
" Only used when an item doesn't have highlighting yet
hi def link tikiwikiIndent           PreProc
hi def link tikiwikiHeading          Title
hi def link tikiwikiColorError       Error
hi def link tikiwikiColorNumber      String
hi def link tikiwikiPluginName       Keyword
hi def link tikiwikiPluginEnd        Keyword
hi def link tikiwikiPluginAttrName   Type
hi def link tikiwikiPluginAttrValue  String
"hi def link tikiwikiPluginArrows     Identifier
hi def link tikiwikiPluginBraces     Keyword
hi def link tikiwikiImageAttrName    Type
hi def link tikiwikiImageAttrValue   String
hi def link tikiwikiImage            Keyword
hi def link tikiwikiTitleBar         Type
hi def link tikiwikiChars            Special
hi def link tikiwikiListItem         String
hi def link tikiwikiMarker           Keyword
hi def link tikiwikiToc              Keyword
hi def link tikiwikiDirection        Keyword
hi def link tikiwikiTableColumn      Identifier
hi def link tikiwikiLink             Underlined
hi def link tikiwikiLinkWord         Underlined
hi def link tikiwikiLine             PreProc
if !exists("tikiwiki_no_rendering")
    hi def link tikiwikiBoldUnderlineItalic tikiwikiBoldItalicUnderline
    hi def link tikiwikiUnderlineBold       tikiwikiBoldUnderline
    hi def link tikiwikiUnderlineItalicBold tikiwikiBoldItalicUnderline
    hi def link tikiwikiUnderlineBoldItalic tikiwikiBoldItalicUnderline
    hi def link tikiwikiUnderlineItalic     tikiwikiItalicUnderline
    hi def link tikiwikiItalicBold          tikiwikiBoldItalic
    hi def link tikiwikiItalicBoldUnderline tikiwikiBoldItalicUnderline
    hi def link tikiwikiItalicUnderlineBold tikiwikiBoldItalicUnderline
    if !exists("tikiwiki_my_rendering")
        hi def tikiwikiBold                 term=bold cterm=bold gui=bold
        hi def tikiwikiItalic               term=italic cterm=italic gui=italic
        hi def tikiwikiUnderline            term=underline cterm=underline gui=underline
        hi def tikiwikiBoldItalic           term=bold,italic cterm=bold,italic gui=bold,italic
        hi def tikiwikiBoldUnderline        term=bold,underline cterm=bold,underline gui=bold,underline
        hi def tikiwikiItalicUnderline      term=italic,underline cterm=italic,underline gui=italic,underline
        hi def tikiwikiBoldItalicUnderline  term=bold,italic,underline cterm=bold,italic,underline gui=bold,italic,underline
    endif
endif

let b:current_syntax = "tikiwiki"

