" Vim syntax file
" Language:     Table of Contents (CdrDAO)
" Filenames:    *.toc
" Maintainer:   Florian octo Forster  <octo@verplant.org>
" URL:          http://verplant.org/toc.vim
" Last Change:  2003 Mar 28 - initial version

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn region   tocComment start="//" end="$" contains=tocComment,tocTodo
syn keyword  tocTodo contained TODO FIXME XXX

syn region   tocString    start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match    tocNumber contained /\<[0-9]\>/
syn match    tocTime /\<\d\d\?:[0-5]\?\d:\([0-6]\?\d\|7[0-4]\)\>/
syn match    tocTime /\s0\(\s\|$\)/

syn keyword  tocCDBegin    CD_DA CD_ROM CD_ROM_XA

syn match    tocTrackBegin /TRACK \(AUDIO\|MODE[12]\(_RAW\)\?\)/
syn match    tocTrackBegin /TRACK MODE2_FORM\([12]\|_MIX\)/

syn keyword  tocTrackTags  CATALOG
syn keyword  tocTrackTags  TWO_CHANNEL_AUDIO FOUR_CHANNEL_AUDIO
syn keyword  tocTrackTags  ISRC SILENCE ZERO START PREGAP INDEX
syn keyword  tocTrackTags  FILE AUDIOFILE DATAFILE
syn match    tocTrackTags  /\(NO\s\+\)\?\(COPY\|PRE_EMPHASIS\)/

syn keyword  tocLanguageTags  contained TITLE PERFORMER SONGWRITER COMPOSER ARRANGER
syn keyword  tocLanguageTags  contained MESSAGE DISC_ID GENRE TOC_INFO1 TOC_INFO2
syn keyword  tocLanguageTags  contained UPC_EAN ISRC SIZE_INFO

syn match    tocLanguageNum   contained /\d/

syn keyword  toc_CD_TEXT      contained CD_TEXT
syn keyword  toc_LANGUAGE     contained LANGUAGE
syn keyword  toc_LANGUAGE_MAP contained LANGUAGE_MAP

syn match    tocLanguageConstruct    contained /LANGUAGE\s\+\d\s\+{/ transparent contains=toc_LANGUAGE,tocLanguageNum
syn match    tocLanguageMapConstruct contained /\d\s*:\s\+/          transparent contains=tocLanguageNum nextgroup=tocNumber


syn region   tocCDText      start="CD_TEXT\s\+{"      end="}" keepend contains=toc_CD_TEXT,tocLanguage,tocLanguageMap
syn region   tocLanguage    start=/LANGUAGE\s\+\d\s\+{/ end="}" contained extend keepend contains=tocLanguageConstruct,tocLanguageTags,tocString
syn region   tocLanguageMap start="LANGUAGE_MAP\s\+{" end="}" contained extend keepend contains=toc_LANGUAGE_MAP,tocLanguageMapConstruct

" Synchronization
syn sync minlines=50
syn sync maxlines=500

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_toc_syntax_inits")
  if version < 508
    let did_toc_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink tocComment      Comment
  HiLink tocTodo         Todo

  HiLink tocCDBegin      Tag
  HiLink tocTrackBegin   Type

  HiLink toc_CD_TEXT      Function
  HiLink toc_LANGUAGE     Function
  HiLink toc_LANGUAGE_MAP Function

  HiLink tocTrackTags     Keyword
  HiLink tocLanguageTags  Keyword

  HiLink tocLanguageNum   Define
  HiLink tocLanguageAny   String

  HiLink tocString        String
  HiLink tocTime          Number
  HiLink tocNumber        Number
  delcommand HiLink
endif

let b:current_syntax = "toc"

" vim: ts=28
