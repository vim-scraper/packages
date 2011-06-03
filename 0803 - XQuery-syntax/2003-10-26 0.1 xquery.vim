" Vim syntax file
" Language:	XQuery
" Author:	Jean-Marc Vanel <http://jmvanel.free.fr/>
" Last Change:	sam oct 25 10:36:59 CEST 2003
" Filenames:	*.xq
" URL:		http://jmvanel.free.fr/vim/xquery.vim
" $Id: xquery.vim,v 1.1 2003/10/26 21:00:12 jmv Exp jmv $

" REFERENCES:
"   [1] http://www.w3.org/TR/xquery/

" Quit when a syntax file was already loaded
if exists("b:current_syntax")
    finish
endif

runtime syntax/xml.vim

syn case match
syn	keyword	xqueryStatement for let where ascending descending empty greatest empty least collation some every in in satisfies typeswitch default return case as return if then else or and instance of treat as castable as cast as to div idiv mod union intersect except validate validate global validate context validate context global eq ne lt le gt ge is isnot child descendant attribute self following parent ancestor preceding document element element namespace attribute attribute pi pi comment text declare xmlspace preserve strip declare default collation declare declare namespace declare default element declare default function namespace declare function as external as empty item element nillable attribute comment text node declare validation import schema at namespace default element

highlight link	xqueryStatement	Statement

"floating point number, with dot, optional exponent
syn match	cFloat		"\d\+\.\d*\(e[-+]\=\d\+\)\=[fl]\="
"floating point number, starting with a dot, optional exponent
syn match	cFloat		"\.\d\+\(e[-+]\=\d\+\)\=[fl]\=\>"
"floating point number, without dot, with exponent
syn match	cFloat		"\d\+e[-+]\=\d\+[fl]\=\>"
syn match	cNumber		"0x\x\+\(u\=l\{0,2}\|ll\=u\)\>"
syn match	cNumber		 "\d\+"
highlight link	cNumber	Number
highlight link	cFloat	Number

syn region	xqComment	start='(:' excludenl end=':)'
highlight link	xqComment	Comment
syntax match	xqVariable	"$\w\+"
highlight link	xqVariable	Identifier
" Redefine the default XML highlighting:
hi link	xmlTag		Structure
hi link	xmlTagName	Structure
hi link	xmlEndTag	Structure

syntax match	xqSeparator	",\|;"
highlight link	xqSeparator	Operator

syn region	xqCode	transparent start='{' excludenl end='}' contains=xmlRegionBis,xqComment,xqueryStatement,xmlString,xqSeparator,cNumber,xqVariable keepend extend

syn region xmlRegionBis start=+<\z([^ /!?<>"']\+\)+ skip=+<!--\_.\{-}-->+ end=+</\z1\_\s\{-}>+ end=+/>+ fold contains=xmlTag,xmlEndTag,xmlCdata,xmlRegionBis,xmlComment,xmlEntity,xmlProcessing,xqCode keepend extend

syn region	List	transparent start='(' excludenl end=')' contains=xqCode,xmlRegion,xqComment,xqSeparator,xqueryStatement keepend extend


