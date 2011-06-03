" Vim syntax file
" Language:	SQL+, SQL, PL/SQL (Oracle 9i)
" Last Change:	2003 Aug 9
" Maintainer: Johannes Gritsch <jo@linuxification.at>
" Version: V0.2

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
syn match sqlPlus	"^\s*a\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*acc\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*accept\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*append\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*archive\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*attribute\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*br\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*break\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*bti\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*btitle\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*c\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*change\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*cl\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*clear\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*col\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*column\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*comp\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*compute\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*conn\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*connect\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*copy\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*def\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*define\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*del\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*desc\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*describe\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*dis\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*disconnect\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*ed\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*edit\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*execute\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*get\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*i\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*input\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*l\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*list\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*passw\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*password\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*pau\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*pause\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*pri\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*print\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*pro\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*prompt\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*recover\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*repf\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*repfooter\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*reph\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*repheader\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*sav\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*save\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*set\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*sho\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*show\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*shutdown\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*spo\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*spool\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*sta\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*start\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*startup\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*store\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*tit\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*title\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*undef\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*undefine\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*var\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*variable\>" nextgroup=sqlPlusLine
syn match sqlPlus	"^\s*whenever\>" nextgroup=sqlPlusLine

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

syn match sqlStatement "\v<alter[ \n]+database>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+cluster>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+database>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+dimension>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+function>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+index>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+indextype>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+java>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+materialized\s+view>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+materialized\s+view\s+log>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+operator>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+outline>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+package>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+procedure>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+profile>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+resource\s+cost>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+role>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+rollback\s+segment>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+sequence>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+session>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+system>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+table>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+tablespace>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+trigger>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+type>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+user>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<alter\s+view>" nextgroup=sqlLine skipempty
syn keyword sqlStatement analyze nextgroup=sqlLine skipempty
syn keyword sqlStatement audit nextgroup=sqlLine skipempty
syn match sqlStatement "\v<associate\s+statistics>" nextgroup=sqlLine skipempty
syn keyword sqlStatement call nextgroup=sqlLine skipempty
syn keyword sqlStatement comment nextgroup=sqlLine skipempty
syn keyword sqlStatement commit nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+cluster>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+context>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+controlfile>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+database>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+database\s+link>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+dimension>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+directory>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+(or\s+replace\s+)?function>" nextgroup=plSqlHeader skipempty
syn match sqlStatement "\v<create\s+index>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+indextype>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+java>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+library>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+materialized\s+view>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+materialized view\s+log>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+operator>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+outline>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+(or\s+replace\s+)?package(\s+body)?>" nextgroup=plSqlHeader skipempty
syn match sqlStatement "\v<create\s+pfile>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+(or\s+replace\s+)?procedure>" nextgroup=plSqlHeader skipempty
syn match sqlStatement "\v<create\s+profile>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+role>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+rollback\s+segment>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+schema>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+sequence>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+spfile>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+(or\s+replace\s+)?synonym>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+table>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+((temporary|undo)\s+)?tablespace>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+(or\s+replace\s+)?trigger>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+type>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create type\s+body>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+user>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<create\s+(or\s+replace\s+)?view>" nextgroup=sqlLine skipempty
syn keyword sqlStatement delete nextgroup=sqlLine skipempty
syn match sqlStatement "\v<disassociate\s+statistics>" nextgroup=sqlLine skipempty
"syn keyword sqlStatement drop
syn match sqlStatement "\v<drop\s+cluster>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+context>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+database\s+link>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+dimension>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+directory>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+function>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+index>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+indextype>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+java>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+library>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+materialized\s+view>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+materialized view\s+log>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+operator>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+outline>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+package>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+procedure>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+profile>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+role>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+rollback\s+segment>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+sequence>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+synonym>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+table>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+tablespace>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+trigger>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+type>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+type\s+body>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+user>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<drop\s+view>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<explain\s+plan>" nextgroup=sqlLine skipempty
syn keyword sqlStatement grant nextgroup=sqlLine skipempty
syn match sqlStatement "\v<insert\s+(all|into)>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<lock\s+table>" nextgroup=sqlLine skipempty
syn keyword sqlStatement merge nextgroup=sqlLine skipempty
syn keyword sqlStatement noaudit nextgroup=sqlLine skipempty
syn keyword sqlStatement rename nextgroup=sqlLine skipempty
syn keyword sqlStatement revoke nextgroup=sqlLine skipempty
syn keyword sqlStatement rollback nextgroup=sqlLine skipempty
syn keyword sqlStatement savepoint nextgroup=sqlLine skipempty
syn keyword sqlStatement select nextgroup=sqlLine skipempty
syn match sqlStatement "\v<set\s+constraint(s)=>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<set\s+role>" nextgroup=sqlLine skipempty
syn match sqlStatement "\v<set\s+transaction>" nextgroup=sqlLine skipempty
syn keyword sqlStatement truncate nextgroup=sqlLine skipempty
syn keyword sqlStatement update nextgroup=sqlLine skipempty
syn keyword sqlStatement with nextgroup=sqlLine skipempty

" Strings and characters:
syn region sqlString contained	start=+"+  skip=+\\\\\|\\"+  end=+"+ contains=sqlVar
syn region sqlString contained	start=+'+  skip=+\\\\\|\\'+  end=+'+ contains=sqlVar

" Numbers:
syn match sqlNumber contained	"-\=\<\d*\.\=[0-9_]\>"

" Comments:
syn region sqlComment    start="/\*"  end="\*/" contains=sqlTodo
syn match sqlComment	"--.*$" contains=sqlTodo
syn match sqlComment	"rem\>.*$" contains=sqlTodo

syn keyword sqlSelect select contained 
syn region sqlLine contained start="\v\z(.)" end="\v\;|^\/$|\%$"  contains=sqlOperator,sqlIdentifier,sqlKeyword,sqlVar,sqlBind,sqlString,sqlNumber,sqlSpecial,sqlType,sqlSelect

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
