" Vim syntax file
" Language:      ColdFusion <http://livedocs.macromedia.com/coldfusion/7/>
" Maintainer:    Steven N. Severinghaus <sns@severinghaus.org>
" Last Modified: 2007-02-23
" Version:       0.1.2
"
" Based on the original version by Jeff Lanzarotta, expanded to include
" new tags, attributes, and functions in CFMX and beyond.

" Quit if syntax file is already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Inherit syntax rules from the standard HTML syntax file
if version < 600
  source <sfile>:p:h/html.vim
else
  runtime! syntax/html.vim
endif

" Tags
syn keyword cfTagName contained cfabort cfapplet cfapplication cfargument cfassociate
syn keyword cfTagName contained cfbreak cfcache cfcalendar cfcase cfcatch
syn keyword cfTagName contained cfchart cfchartdata cfchartseries cfcol cfcollection
syn keyword cfTagName contained cfcomponent cfcontent cfcookie cfdefaultcase cfdirectory
syn keyword cfTagName contained cfdocument cfdocumentitem cfdocumentsection cfdump cfelse
syn keyword cfTagName contained cfelseif cferror cfexecute cfexit cffile cfflush cfform
syn keyword cfTagName contained cfformgroup cfformitem cfftp cffunction cfgraph cfgraphdata
syn keyword cfTagName contained cfgrid cfgridcolumn cfgridrow cfgridupdate cfheader
syn keyword cfTagName contained cfhtmlhead cfhttp cfhttpparam cfif cfimport
syn keyword cfTagName contained cfinclude cfindex cfinput cfinsert cfinvoke cfinvokeargument
syn keyword cfTagName contained cfldap cflocation cflock cflog cflogin cfloginuser cflogout
syn keyword cfTagName contained cfloop cfmail cfmailparam cfmailpart cfmodule
syn keyword cfTagName contained cfNTauthenticate cfobject cfobjectcache cfoutput cfparam
syn keyword cfTagName contained cfpop cfprocessingdirective cfprocparam cfprocresult
syn keyword cfTagName contained cfproperty cfquery cfqueryparam cfregistry cfreport
syn keyword cfTagName contained cfreportparam cfrethrow cfreturn cfsavecontent cfschedule
syn keyword cfTagName contained cfscript cfsearch cfselect cfservlet cfservletparam cfset
syn keyword cfTagName contained cfsetting cfsilent cfslider cfstoredproc cfswitch cftable
syn keyword cfTagName contained cftextarea cftextinput cfthrow cftimer cftrace cftransaction
syn keyword cfTagName contained cftree cftreeitem cftry cfupdate cfwddx cfxml

" Tag attributes
syn keyword cfArg contained accept access accessible action addnewline addtoken
syn keyword cfArg contained agentname align appendkey appletsource applicationtimeout
syn keyword cfArg contained archive arguments attachmentpath attributecollection
syn keyword cfArg contained attributes backgroundvisible basetag bgcolor bindingname
syn keyword cfArg contained blockfactor body bold border branch cachedafter cachedwithin
syn keyword cfArg contained cc cfsqltype chartheight chartwidth checked class
syn keyword cfArg contained clientmanagement clientstorage codebase colheaderalign
syn keyword cfArg contained colheaderbold colheaderfont colheaderfontsize
syn keyword cfArg contained colheaderitalic colheaders collection colorlist colspacing
syn keyword cfArg contained columns completepath component connection context criteria
syn keyword cfArg contained custom1 custom2 data dataalign databackgroundcolor
syn keyword cfArg contained datacollection datasource daynames dbname dbserver dbtype
syn keyword cfArg contained dbvarname debug default delete deletebutton deletefile
syn keyword cfArg contained delimiter description destination detail directory display
syn keyword cfArg contained displayname dn domain enablecab enablecfoutputonly enabled
syn keyword cfArg contained encryption enctype enddate endrange endtime entry errorcode
syn keyword cfArg contained exception expand expires expireurl expression extendedinfo
syn keyword cfArg contained extends extensions external file filefield filename filter
syn keyword cfArg contained fixnewline font fontbold fontembed fontitalic fontsize
syn keyword cfArg contained foregroundcolor format formfields formula from grid
syn keyword cfArg contained griddataalign gridlines groovecolor group header headeralign
syn keyword cfArg contained headerbold headerfont headerfontsize headeritalic
syn keyword cfArg contained headerlines height highlighthref hint href hrefkey hscroll
syn keyword cfArg contained hspace htmltable id img imgopen imgstyle index input insert
syn keyword cfArg contained insertbutton interval isolation italic item itemcolumn key
syn keyword cfArg contained keyonly label labelformat language list loginstorage
syn keyword cfArg contained mailerid mailto marginbottom marginleft marginright
syn keyword cfArg contained margintop markersize markerstyle mask maxlength maxrows
syn keyword cfArg contained message messagenumber method mimeattach mimetype mode
syn keyword cfArg contained monthnames multiple name nameconflict namespace newdirectory
syn keyword cfArg contained notsupported null numberformat onblur onchange onerror
syn keyword cfArg contained onfocus onload onreset onsubmit onvalidate operation orderby
syn keyword cfArg contained orientation output outputfile overwrite ownerpassword
syn keyword cfArg contained pageheight pagetype pagewidth paintstyle param_1 param_2
syn keyword cfArg contained param_3 param_4 param_5 param_6 param_7 param_8 param_9
syn keyword cfArg contained parrent passthrough password path permissions picturebar
syn keyword cfArg contained pieslicestyle port porttypename prefix preloader
syn keyword cfArg contained preservedata procedure protocol provider providerdsn
syn keyword cfArg contained proxybypass proxyserver publish query queryasroot range
syn keyword cfArg contained recurse refreshlabel report requesttimeout required reset
syn keyword cfArg contained resoleurl resultset retrycount returncode returntype
syn keyword cfArg contained returnvariable rotated rowheaderalign rowheaderbold
syn keyword cfArg contained rowheaderfont rowheaderfontsize rowheaderitalic rowheaders
syn keyword cfArg contained rowheaderwidth rowheight scale scalefrom scaleto scope
syn keyword cfArg contained scriptprotect scriptsrc secure securitycontext select
syn keyword cfArg contained selectcolor selected selecteddate selectedindex selectmode
syn keyword cfArg contained seriescolor serieslabel seriesplacement server
syn keyword cfArg contained serviceportname sessionmanagement sessiontimeout
syn keyword cfArg contained setclientcookies setcookie setdomaincookies show3d
syn keyword cfArg contained showborder showdebugoutput showerror showlegend showmarkers
syn keyword cfArg contained showxgridlines showygridlines size skin sort
syn keyword cfArg contained sortascendingbutton sortdescendingbutton sortxaxis source
syn keyword cfArg contained sql src srcfile start startdate startrange startrow
syn keyword cfArg contained starttime step stoponerror style subject tablename
syn keyword cfArg contained tableowner tablequalifier taglib target task template text
syn keyword cfArg contained textcolor textqualifier throwonfailure throwontimeout
syn keyword cfArg contained timeout timespan tipbgcolor tipstyle title to tooltip
syn keyword cfArg contained toplevelvariable type unit url urlpath username userpassword
syn keyword cfArg contained usetimezoneinfo validate value valuecolumn var variable
syn keyword cfArg contained visible vscroll vspace width wmode wsdlfile xaxistitle
syn keyword cfArg contained xaxistype xoffset yaxistitle yaxistype yoffset

" Functions
syn keyword cfFunctionName contained ACos ASin Abs AddSOAPRequestHeader AddSOAPResponseHeader
syn keyword cfFunctionName contained ArrayAppend ArrayAvg ArrayClear ArrayDeleteAt ArrayInsertAt
syn keyword cfFunctionName contained ArrayIsEmpty ArrayLen ArrayMax ArrayMin ArrayNew
syn keyword cfFunctionName contained ArrayPrepend ArrayResize ArraySet ArraySort ArraySum
syn keyword cfFunctionName contained ArraySwap ArrayToList Asc Atn AuthenticatedContext
syn keyword cfFunctionName contained AuthenticatedUser BinaryDecode BinaryEncode BitAnd
syn keyword cfFunctionName contained BitMaskClear BitMaskRead BitMaskSet BitNot BitOr BitSHLN
syn keyword cfFunctionName contained BitSHRN BitXor CJustify Ceiling CharsetDecode CharsetEncode
syn keyword cfFunctionName contained Chr Compare CompareNoCase Cos CreateDate CreateDateTime
syn keyword cfFunctionName contained CreateODBCDate CreateODBCDateTime CreateODBCTime
syn keyword cfFunctionName contained CreateObject CreateTime CreateTimeSpan CreateUUID DE DateAdd
syn keyword cfFunctionName contained DateCompare DateConvert DateDiff DateFormat DatePart Day
syn keyword cfFunctionName contained DayOfWeek DayOfWeekAsString DayOfYear DaysInMonth DaysInYear
syn keyword cfFunctionName contained DecimalFormat DecrementValue Decrypt DecryptBinary
syn keyword cfFunctionName contained DeleteClientVariable DirectoryExists DollarFormat Duplicate
syn keyword cfFunctionName contained Encrypt EncryptBinary Evaluate Exp ExpandPath FileExists
syn keyword cfFunctionName contained Find FindNoCase FindOneOf FirstDayOfMonth Fix FormatBaseN
syn keyword cfFunctionName contained GenerateSecretKey GetAuthUser GetBaseTagData GetBaseTagList
syn keyword cfFunctionName contained GetBaseTemplatePath GetClientVariablesList GetContextRoot
syn keyword cfFunctionName contained GetCurrentTemplatePath GetDirectoryFromPath GetEncoding
syn keyword cfFunctionName contained GetException GetFileFromPath GetFunctionList
syn keyword cfFunctionName contained GetGatewayHelper GetHttpRequestData GetHttpTimeString
syn keyword cfFunctionName contained GetK2ServerDocCount GetK2ServerDocCountLimit GetLocalHostIP
syn keyword cfFunctionName contained GetLocale GetLocaleDisplayName GetMetaData GetMetricData
syn keyword cfFunctionName contained GetPageContext GetProfileSections GetProfileString
syn keyword cfFunctionName contained GetSOAPRequest GetSOAPRequestHeader GetSOAPResponse
syn keyword cfFunctionName contained GetSOAPResponseHeader GetTempDirectory GetTempFile
syn keyword cfFunctionName contained GetTemplatePath GetTickCount GetTimeZoneInfo GetToken
syn keyword cfFunctionName contained HTMLCodeFormat HTMLEditFormat Hash Hour IIf IncrementValue
syn keyword cfFunctionName contained InputBaseN Insert Int IsArray IsAuthenticated IsAuthorized
syn keyword cfFunctionName contained IsBinary IsBoolean IsCustomFunction IsDate IsDebugMode
syn keyword cfFunctionName contained IsDefined IsK2ServerABroker IsK2ServerDocCountExceeded
syn keyword cfFunctionName contained IsK2ServerOnline IsLeapYear IsLocalHost IsNumeric
syn keyword cfFunctionName contained IsNumericDate IsObject IsProtected IsQuery IsSOAPRequest
syn keyword cfFunctionName contained IsSimpleValue IsStruct IsUserInRole IsValid IsWDDX IsXML
syn keyword cfFunctionName contained IsXmlAttribute IsXmlDoc IsXmlElem IsXmlNode IsXmlRoot
syn keyword cfFunctionName contained JSStringFormat JavaCast LCase LJustify LSCurrencyFormat
syn keyword cfFunctionName contained LSDateFormat LSEuroCurrencyFormat LSIsCurrency LSIsDate
syn keyword cfFunctionName contained LSIsNumeric LSNumberFormat LSParseCurrency LSParseDateTime
syn keyword cfFunctionName contained LSParseEuroCurrency LSParseNumber LSTimeFormat LTrim Left
syn keyword cfFunctionName contained Len ListAppend ListChangeDelims ListContains
syn keyword cfFunctionName contained ListContainsNoCase ListDeleteAt ListFind ListFindNoCase
syn keyword cfFunctionName contained ListFirst ListGetAt ListInsertAt ListLast ListLen
syn keyword cfFunctionName contained ListPrepend ListQualify ListRest ListSetAt ListSort
syn keyword cfFunctionName contained ListToArray ListValueCount ListValueCountNoCase Log Log10
syn keyword cfFunctionName contained Max Mid Min Minute Month MonthAsString Now NumberFormat
syn keyword cfFunctionName contained ParagraphFormat ParameterExists ParseDateTime Pi
syn keyword cfFunctionName contained PreserveSingleQuotes Quarter QueryAddColumn QueryAddRow
syn keyword cfFunctionName contained QueryNew QuerySetCell QuotedValueList REFind REFindNoCase
syn keyword cfFunctionName contained REReplace REReplaceNoCase RJustify RTrim Rand RandRange
syn keyword cfFunctionName contained Randomize ReleaseComObject RemoveChars RepeatString Replace
syn keyword cfFunctionName contained ReplaceList ReplaceNoCase Reverse Right Round Second
syn keyword cfFunctionName contained SendGatewayMessage SetEncoding SetLocale SetProfileString
syn keyword cfFunctionName contained SetVariable Sgn Sin SpanExcluding SpanIncluding Sqr StripCR
syn keyword cfFunctionName contained StructAppend StructClear StructCopy StructCount StructDelete
syn keyword cfFunctionName contained StructFind StructFindKey StructFindValue StructGet
syn keyword cfFunctionName contained StructInsert StructIsEmpty StructKeyArray StructKeyExists
syn keyword cfFunctionName contained StructKeyList StructNew StructSort StructUpdate Tan
syn keyword cfFunctionName contained TimeFormat ToBase64 ToBinary ToScript ToString Trim UCase
syn keyword cfFunctionName contained URLDecode URLEncodedFormat URLSessionFormat Val ValueList
syn keyword cfFunctionName contained Week Wrap WriteOutput XmlChildPos XmlElemNew XmlFormat
syn keyword cfFunctionName contained XmlGetNodeType XmlNew XmlParse XmlSearch XmlTransform
syn keyword cfFunctionName contained XmlValidate Year YesNoFormat

syn keyword cfDeprecated contained cfauthenticate cfimpersonate
syn keyword cfDeprecated contained GetK2ServerDocCount GetK2ServerDocCountLimit GetTemplatePath
syn keyword cfDeprecated contained IsK2ServerABroker IsK2ServerDocCountExceeded IsK2ServerOnline
syn keyword cfDeprecated contained ParameterExists

syn cluster htmlTagNameCluster add=cfTagName
syn cluster htmlArgCluster add=cfArg,cfFunctionName

syn region cfFunctionRegion start='#' end='#' contains=cfFunctionName

command -nargs=+ HiLink hi def link <args>

HiLink	cfTagName	Statement
HiLink	cfArg		Type
HiLink	cfFunctionName	Function
HiLink	cfDeprecated	Error

delcommand HiLink

let b:current_syntax = "cf"

"EOF vim: tw=78:ft=vim:ts=8

