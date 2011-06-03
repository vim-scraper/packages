" Vim syntax file
" Language:	SQL+, SQL, PL/SQL (Oracle 9i)
" Last Change:	2003 Aug 25
" Maintainer: Johannes Gritsch <jo@linuxification.at>
" Version: V0.2.1

" original file by Paul Moore <gustav@morpheus.demon.co.uk>
" Adapted to Oracle 9iR2 by Johannes Gritsch <jo@linuxification.at>

if exists("b:current_syntax")
  finish
endif

" This script supports highlighting of SQL+ commands, pure SQL commands and keywords
" and PL/SQL Blocks - the latter are just handled as a uniform block without further
" syntax highlighting

syn case ignore

" SQL+:
syn match sqlPlus	"^@" nextgroup=sqlPlusLine
syn match sqlPlus	"^@@" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*a(ppend)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*acc(ept)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*archive>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*attribute>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*br(eak)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*bti(tle)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*c(hange)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*cl(ear)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*col(umn)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*comp(ute)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*conn(ect)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*copy>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*def(ine)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*del>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*desc(ribe)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*dis(connect)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*ed(it)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*execute>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*get>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*i(nput)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*l(ist)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*passw(ord)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*pau(se)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*pri(nt)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*pro(mpt)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*recover>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*repf(ooter)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*reph(eader)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*sav(e)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*set>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*sho(w)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*shutdown>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*spo(ol)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*sta(rt)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*startup>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*store>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*tit(le)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*undef(ine)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*var(iable)?>" nextgroup=sqlPlusLine
syn match sqlPlus	"\v^\s*whenever>" nextgroup=sqlPlusLine

syn match sqlVar contained	"&[a-z0-9_]+"
syn match sqlBind contained	":[a-z0-9_]+"

" actually null is neither keyword nor reserved, but ...
syn keyword sqlSpecial contained	null 	

" The SQL reserved words, defined as keywords.
syn keyword sqlIdentifier contained	abs acos add_months ascii asciistr asin atan atan2 avg bfilename bin_to_num bitand cast ceil chartorowid chr coalesce compose concat convert corr cos cosh count covar_pop covar_samp cube cume_dist current_date current_timestamp dbtimezone decode decompose dense_rank depth deref dump empty_blob empty_clob existsnode exp extract extractvalue first first_value floor from_tz greatest group_id grouping grouping_id hextoraw initcap instr lag last last_day last_value lead least length ln localtimestamp log lower lpad ltrim make_ref max min mod months_between nchr new_time next_day nls_charset_decl_len nls_charset_id nls_charset_name nls_initcap nls_lower nlssort nls_upper ntile nullif numtodsinterval numtoyminterval nvl nvl2 path percent_rank percentile_cont percentile_disc power rank ratio_to_report rawtohex rawtonhex ref reftohex regr_ replace rollup round row_number rowidtochar rowidtonchar rpad rtrim sessiontimezone sign sin sinh soundex sqrt stddev stddev_pop stddev_samp substr sum sys_connect_by_path sys_context sys_dburigen sys_extract_utc sys_guid sys_typeid sys_xmlagg sys_xmlgen systimestamp tan tanh to_char to_clob to_date to_dsinterval to_lob to_multi_byte to_nchar to_nclob to_number to_single_byte to_timestamp to_timestamp_tz to_yminterval translate treat trim trunc tz_offset unistr updatexml upper userenv value var_pop var_samp variance vsize width_bucket xmlagg xmlcolattval xmlconcat xmlelement xmlforest xmlsequence xmltransform

syn keyword sqlKeyword contained	access add alter as asc audit by check cluster column comment compress connect create current default desc distinct drop else exclusive file for from group having identified immediate increment insert index initial into is level lock maxextents mode modify noaudit nocompress nowait of offline on online option over order partition pctfree privileges public resource row rowid rownum rows session set share size start successful synonym sysdate table then to trigger uid unique user validate values view whenever where 

syn keyword sqlOperator contained	all and any between exists in intersect like minus not or out some prior union
syn match sqlOperator contained	"\v(left|right|full)\s+outer\s+join>"
syn match sqlOperator contained	"\v(natural|cross)=\s+join>"
syn match sqlOperator contained	"\v\|\|"

syn keyword sqlType contained	char character date decimal float integer long mlslabel number raw smallint varchar varchar2 varray

"Statements begin with (one or several) keyword(s) 
" alter statements
syn match sqlStatement "\v<alter\s+(cluster|database|dimension|function|index|indextype|java|materialized\s+view(\s+log)?|operator|outline|package|procedure|profile|resource\s+cost|role|rollback\s+segment|sequence|session|system|table|tablespace|trigger|type|user|view)>" nextgroup=sqlLine skipempty

syn keyword sqlStatement analyze nextgroup=sqlLine skipempty
syn keyword sqlStatement audit nextgroup=sqlLine skipempty
syn match sqlStatement "\v<associate\s+statistics>" nextgroup=sqlLine skipempty
syn keyword sqlStatement call nextgroup=sqlLine skipempty
syn keyword sqlStatement comment nextgroup=sqlLine skipempty
syn keyword sqlStatement commit nextgroup=sqlEnd,sqlLine skipempty

"create statements
syn match sqlStatement "\v<create\s+(cluster|context|controlfile|database(\s+link)?|dimension|directory|((unique|bitmap)\s+)?index|indextype|java|library|materialized\s+view(\s+log)?|operator|outline|s?pfile|profile|role|rollback\s+segment|schema|sequence|(global\s+temporary\s+)?table|((temporary|undo)\s+)?tablespace|e(\s+body)?|user)>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+(or\s+replace\s+)?(synonym|trigger|view)>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+(or\s+replace\s+)?(function|procedure|package(\s+body)?)>" nextgroup=plSqlHeader skipempty

syn keyword sqlStatement delete nextgroup=sqlLine skipempty
syn match sqlStatement "\v<disassociate\s+statistics>" nextgroup=sqlLine skipempty

" drop statements
syn match sqlStatement "\v<drop\s+(cluster|context|database\s+link|dimension|directory|function|procedure|package|index|indextype|java|library|materialized\s+view(\s+log)?|operator|outline|profile|role|rollback\s+segment|sequence|synonym|table|tablespace|trigger|type(\s+body)?|user|view)>" nextgroup=sqlLine skipempty

syn match sqlStatement "\v<explain\s+plan>" nextgroup=sqlLine skipempty
syn keyword sqlStatement grant nextgroup=sqlLine skipempty
syn match sqlStatement "\v<insert\s+(all|into)>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<lock\s+table>" nextgroup=sqlLine skipempty
syn keyword sqlStatement merge nextgroup=sqlLine skipempty
syn keyword sqlStatement noaudit nextgroup=sqlLine skipempty
syn keyword sqlStatement rename nextgroup=sqlLine skipempty
syn keyword sqlStatement revoke nextgroup=sqlLine skipempty
syn keyword sqlStatement rollback nextgroup=sqlEnd,sqlLine skipempty
syn keyword sqlStatement savepoint nextgroup=sqlLine skipempty
syn keyword sqlStatement select nextgroup=sqlLine skipempty
syn match sqlStatement "\v<set\s+constraint(s)?>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<set\s+role>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<set\s+transaction>" nextgroup=sqlLine skipempty
syn keyword sqlStatement truncate nextgroup=sqlLine skipempty
syn keyword sqlStatement update nextgroup=sqlLine skipempty
syn keyword sqlStatement with nextgroup=sqlLine skipempty

" Strings and characters:
syn region sqlString contained	start=+"+  skip=+\\\\\|\\"+  end=+"+ contains=sqlVar
syn region sqlString contained	start=+'+  skip=+\\\\\|\\'+  end=+'+ contains=sqlVar

" Numbers:
syn match sqlNumber contained	"-\=\<\d*\.\=[0-9_]\[kKmM]\=>"

" Comments:
syn region sqlComment    start="/\*"  end="\*/" contains=sqlTodo
syn match sqlComment	"--.*$" contains=sqlTodo
syn match sqlComment	"rem\>.*$" contains=sqlTodo

syn keyword sqlSelect select contained 
syn region sqlLine contained start="\v\z(.)" end="\v\;|^\/$|\%$"  contains=sqlOperator,sqlIdentifier,sqlKeyword,sqlVar,sqlBind,sqlString,sqlNumber,sqlSpecial,sqlType,sqlSelect
syn match sqlEnd 	"\v\;|\n/"	contained 

"SQL+ line (after trailing keyword)
syn region sqlPlusLine	start="." end="$" skip="-\s*$" contained

"PL/SQL
syn region plSql	start="\v<declare>|<procedure>|<function>" end="begin" nextgroup=plSqlBody skipempty
syn region plSqlBody	start="." end="end" contains=plSql,plSqlBlock contained
syn region plSqlHeader  start="." end="begin" contained nextgroup=plSqlBody skipempty
syn region plSqlBlock	start="begin" end="end" contains=plSql,plSqlBlock
syn sync ccomment sqlComment
 syn sync linebreaks=100

" Todo.
syn keyword sqlTodo TODO FIXME XXX DEBUG NOTE
highlight plSql term=bold ctermfg=Black ctermbg=lightcyan guifg=Black guibg=lightcyan

" Define the default highlighting.
" only when an item doesn't have highlighting yet
if !exists("did_sql_syn_inits")
  command -nargs=+ HiLink hi def link <args>
  HiLink plSqlBlock	plSql
  HiLink plSqlHeader	plSql
  HiLink plSqlBody	plSql
  HiLink sqlComment	Comment
  HiLink sqlKeyword	sqlSpecial
  HiLink sqlNumber	Number
  HiLink sqlLine	col_white_darkcyan
  HiLink sqlOperator	sqlSpecial
  HiLink sqlSpecial	Special
  HiLink sqlStatement	Statement
  HiLink sqlSelect	Statement
  HiLink sqlString	String
  HiLink sqlType	Type
  HiLink sqlTodo	Todo
  HiLink sqlIdentifier  Identifier
  HiLink sqlPlus	PreProc
  HiLink sqlPlusLine	Comment
  HiLink sqlVar		NonText
  HiLink sqlBind	NonText
  delcommand HiLink
endif

let b:current_syntax = "sql"
