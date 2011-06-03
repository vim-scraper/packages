" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/range_date.vim	[[[1
106
" range_date.vim: some auxiliary functions to work with ranges and dates
" @Author:      <bastian at mathes dot jp>
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2008-03-24

" Range_int(start, stop[, step[, format]])
function! Range_int(start,stop,...)
	let l:step = a:0 >= 1 ? a:1 : 1
	let l:format = a:0 >= 2 ? a:2 : "%d"
	let l:counter = a:start
	let l:erg = []
	while l:counter <= a:stop
		call add(l:erg,printf(l:format,l:counter))
		let l:counter = l:counter + l:step
	endwhile
	return l:erg
endfunction

" range by ascii value
function! Range_char(start,stop)
	let l:counter = char2nr(a:start)
	let l:erg = []
	while l:counter <= char2nr(a:stop)
		call add(l:erg,nr2char(l:counter))
		let l:counter = l:counter + 1 
	endwhile
	return l:erg
endfunction

" Range_month(start, stop[, format])
function! Range_month(start,stop,...)
	let l:month = strpart(a:start,strlen(a:start)-2) + 0
	let l:year = strpart(a:start,0,strlen(a:start)-2) + 0
	let l:month_stop = strpart(a:stop,strlen(a:stop)-2) + 0
	let l:year_stop = strpart(a:stop,0,strlen(a:stop)-2) + 0
	let l:format = a:0 >=1 ? a:1 : "YYYYMM"
	let l:erg = []
	while l:year < l:year_stop || l:month <= l:month_stop
		call add(l:erg,Format_date(l:year.printf("%02d",l:month),l:format))
		let l:month = l:month == 12 ? 1 : l:month + 1
		let l:year = l:month == 1 ? l:year + 1 : l:year
	endwhile
	return l:erg
endfunction

" Range_date(start,stop[, format])
function! Range_date(start,stop,...)
	let l:jd = Gd2jd(a:start)
	let l:jd_stop = Gd2jd(a:stop)
	let l:format = a:0 >= 1 ? a:1 : "YYYYMMDD"
	let l:erg = []
	while l:jd <= l:jd_stop
		call add(l:erg,Format_date(Jd2gd(l:jd),l:format))
		let l:jd = l:jd + 1
	endwhile
	return l:erg
endfunction

" convert gregorian date (YYYYMMDD) to julian data (integer)
function! Gd2jd(gregorian)
	let l:y=strpart(a:gregorian,0,4)
	let l:m=strpart(a:gregorian,4,2)
	let l:d=strpart(a:gregorian,6,2)
	return l:d-32075+1461*(l:y+4800+(l:m-14)/12)/4+367*(l:m-2-(l:m-14)/12*12)/12-3*((l:y+4900+(l:m-14)/12)/100)/4
endfunction

" convert julian date (integer) to gregorian date (YYYYMMDD)
function! Jd2gd(julian)
	let l:L    = a:julian+68569
	let l:N    = 4*l:L/146097
	let l:L    = l:L-(146097*l:N+3)/4
	let l:I    = 4000*(l:L+1)/1461001
	let l:L    = l:L-1461*l:I/4+31
	let l:J    = 80*l:L/2447
	let l:DD   = l:L-2447*l:J/80
	let l:L    = l:J/11
	let l:MM   = l:J+2-12*l:L
	let l:YYYY = 100*(l:N-49)+l:I+l:L
	return l:YYYY.printf('%02d',l:MM).printf('%02d',l:DD)
endfunction

" date=YYYY[MM[DD[HH[MI[SS]]]]]]
" format=any string containing the characters above
function! Format_date(date,format)
	let l:erg = a:format
	if strlen(a:date) >= 4
		let l:erg = substitute(l:erg,'YYYY',strpart(a:date,0,4),'')
		let l:erg = substitute(l:erg,'YY',strpart(a:date,2,2),'')
		if strlen(a:date) >= 6
			let l:erg = substitute(l:erg,'MM',strpart(a:date,4,2),'')
			if strlen(a:date) >= 8
				let l:erg = substitute(l:erg,'DD',strpart(a:date,6,2),'')
				if strlen(a:date) >= 10
					let l:erg = substitute(l:erg,'HH',strpart(a:date,8,2),'')
					if strlen(a:date) >= 12
						let l:erg = substitute(l:erg,'MI',strpart(a:date,10,2),'')
						if strlen(a:date) >= 14
							let l:erg = substitute(l:erg,'SS',strpart(a:date,12,2),'')
						endif
					endif
				endif	
			endif
		endif
	endif
	return l:erg
endfunction
doc/range_date.txt	[[[1
108
*range_date.txt*  auxiliary functions to work with ranges and dates

1. Overview                                     *range_date-overview*

This script provides auxiliary functions to work with ranges and dates. 
Four functions return ranges as arrays:

Range_int (|range_date-Range_int|) - returns an array of numbers
Range_char (|range_date-Range_char|) - returns an array of characters
Range_month (|range_date-Range_month|) - returns an array of month
Range_date (|range_date-Range_date|) - returns an array of dates

Two functions convert (gregorian) dates in the form YYYYMMDD into julian dates 
(days since 01.01.4713 v.Chr.) to calculate with dates:

Gd2jd (|range_date-Gd2jd|) - converts gregorian date string (YYYYMMDD) to julian date
Jd2gd (|range_date-Jd2gd|) - converts julian date to gregorian date string (YYYYMMDD)

One functions formats a date given in the form YYYYMMDD or YYYYMM:

Format_date (|range_date-Format_date|) - Returns the given date in the given format


2. Range_int                                    *range_date-Range_int*

Signature: Range_int(start, stop[, step[, format]])

Returns an array containing the numbers from 'start' to 'stop'. Optionally a 'step' width 
can be provided (e.g. use only every second number) and a 'format' (in the form used by printf 
can be given). Note that a step has to be provided if the format is provided.

Example: Range_int(1,29,2,"%02d")
Result: ['01', '03', '05', '07', '09', '11', '13', '15', '17', '19', '21', '23', '25', '27', '29']


3. Range_char                                   *range_date-Range_char*

Signature: Range_char(start, stop)

Returns an array containing the characters from 'start' to 'stop'. Actually the ASCII value is 
incremented in a loop.

Example: Range_char('a','h')
Result: ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']

Example: Range_char('A','c')
Result: ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '[', '\', ']', '^', '_', '`', 'a', 'b', 'c']


4. Range_month                                  *range_date-Range_month*

Signature: Range_month(start, stop[, format])

Returns an array with months from 'start' to 'stop' (both given as strings in the format YYYYMM) 
in the format 'format' (used by |range_date-Format_date|). 

Example: Range_month('200710','200803','MM.YYYY')
Result: ['10.2007', '11.2007', '12.2007', '01.2008', '02.2008', '03.2008']


5. Range_date                                   *range_date-Range_date*

Signature: Range_month(start, stop[, format])

Returns an array with dates from 'start' to 'stop' (both given as strings in the format YYYYMMDD)
in the format 'format' (used by |range_date-Format_date|).

Example: Range_date('20071229','20080103','DD.MM.YYYY')
Result: ['29.12.2007', '30.12.2007', '31.12.2007', '01.01.2008', '02.01.2008', '03.01.2008']


6. Gd2jd                                        *range_date-Gd2jd*

Signature: Gd2jd(gregorian_date)

Returns the julian date that corresponds to the given 'gregorian_date' (which has to be
a string in the format YYYYMMDD). This will only work for dates from 1901 to 2099.

Example: Gd2jd('20080325')
Result: 2454551


7. Jd2gd                                        *range_date-Jd2gd*

Signature: Jd2gd(julian_date)

Returns the gregorian date (in the format YYYYMMMDD) that corresponds to the given 'julian_date'. 
This will only work for dates from 1901 to 2099.

Example: Jd2gd(2454551)
Result: '20080325'


8. Format_date                                  *range_date-Format_date*

Signature: Format_date(date, format)

Returns the date 'date' in the format 'format'. 'date' is a string of the format
YYYY[MM[DD[HH[MI[SS]]]]], e.g. 2008, 20080325, 200803252312 usw.

'format' is a string containing a combination of the literals YY, YYYY, MM, DD, HH, MI, SS, 
e.g. "DD.MM.YYYY" or "the date DD.MM in the year YYYY"

Example: Format_date('20080325','DD.MM.YY')
Result: '25.03.08'

Example: Format_date('20080325','Today is the day no. DD in month no. MM of the year YY')
Result: 'Today is the day no. 25 in month no. 03 of the year 08'
plugin/vimgen.vim	[[[1
169
" vimgen.vim -- Generate text from data and template
" @Author:      <bastian at mathes dot jp>
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2008-03-24

" define new command
com! -range -nargs=0 Vimgen <line1>,<line2>call Vimgen()

" start tag character, default «
if !exists("g:vgen_sc")
	let g:vgen_sc = '«'
endif

" end tag character, default »
if !exists("g:vgen_ec")
	let g:vgen_ec = '»'
endif

" loop character, default !
if !exists("g:vgen_lc")
	let g:vgen_lc = '-'
endif

if !exists("g:vgen_sfc")
	let g:vgen_sfc = "!"
endif

" delimiter
if !exists("g:vgen_delim")
	let g:vgen_delim = "----vimgen----"
endif

" entry point, starts the generation
function! Vimgen() range
	" initialize variables
	let l:line = a:firstline
	let l:is_data = 1
	let l:data_list = []
	let l:template_list = []
	let g:vgen_data = {}
	let g:vgen_vars = []
	let g:vgen_iter = {}

  " read data and template
	while (l:line <= a:lastline)
		if getline(l:line) =~ '^\s*$' && l:is_data
			let l:is_data = 0
		elseif l:is_data
			call add(l:data_list,getline(l:line))
		else
			call add(l:template_list,getline(l:line))
		endif
		let l:line = l:line+1
	endwhile

	" Decho "data_list -> ".string(l:data_list)
	" Decho "template_list -> ".string(l:template_list)

	" do the work
	let g:vgen_data = eval(join(l:data_list,""))
	" Decho "data -> ".string(g:vgen_data)
	let l:output = s:transform_template(join(l:template_list,"\n"))

	" output
	call append(a:lastline, split(g:vgen_delim . "\n" . l:output,"\n"))
endfunction

" this method does the real work
function! s:transform_template(template)
	let l:continue = 1 
	let l:index = 0
	let l:old_index = 0
	let l:script = ""
	let l:template = ""
	let l:guards = []

	while l:continue == 1
		" handle text until next g:vgen_sc
		let l:old_index = l:index
		let l:index = stridx(a:template,g:vgen_sc,l:old_index)
		if l:index == -1
			let l:index = strlen(a:template)
			let l:continue = 0
		endif
		if l:index != l:old_index 
			" handle guards
			for l:guard in l:guards
				let l:script = l:script . "if g:vgen_iter['". l:guard ."'] > 1 | "
			endfor
			let l:script = l:script . "let l:template = l:template . \"" . strpart(a:template,l:old_index,l:index-l:old_index) . "\" | "
			for l:guard in l:guards
				let l:script = l:script . "endif | "
			endfor
		endif
		" handle next command
		if l:index != strlen(a:template)
			let l:old_index = l:index
			let l:index = stridx(a:template,g:vgen_ec,l:old_index)
			if l:index == -1
				throw "incomplete command at the end of the template"		
			endif
			let l:command = strpart(a:template, l:old_index+strlen(g:vgen_sc), l:index-l:old_index-strlen(g:vgen_ec))
			if l:command =~ '^'.g:vgen_lc.'$'
			  " end of loop
			  let l:script = l:script . "endfor" . " | "
				call s:unbind_variable()
			elseif l:command =~ '^'.g:vgen_lc
				" begin of loop
				let l:var_str = substitute(l:command,'^'.g:vgen_lc,'','')
				let l:var = s:get_variable(l:var_str)
				let l:new_var = s:bind_variable(l:var_str,l:var)
				let l:script = l:script . "let g:vgen_iter['". l:var_str ."'] = 0 | for " . l:new_var . " in " . l:var . " | let g:vgen_iter['". l:var_str ."'] = g:vgen_iter['". l:var_str ."'] + 1 | "
			elseif l:command =~ '^'.g:vgen_sfc.'$'
				" end of block not to be printed in first iteration
				call remove(l:guards,0)
			elseif l:command =~ '^'.g:vgen_sfc
				" beginning of block not to be printed in first iteration
				let l:var_str = substitute(l:command,'^'.g:vgen_sfc,'','')
				call insert(l:guards,l:var_str)
			else
				" variable
				let l:var = s:get_variable(l:command)
				let l:script = l:script . "let l:template = l:template . " . l:var . " | "
			endif
			let l:index = l:index+strlen(g:vgen_ec)
		endif
	endwhile

  let l:script = substitute(l:script,' | $','','')      " last delimiter
	" Decho "data -> ".string(g:vgen_data)
	" Decho "script -> ".l:script
	execute l:script
	return l:template
endfunction

" get the vimscript equivalent to the variable in the current context
function! s:get_variable(var_str)
	" Decho "vars -> ".string(g:vgen_vars)
	let l:erg = []
	for l:var in g:vgen_vars
		if a:var_str =~ '^'.l:var[0] 
			let l:erg = l:var
			break
		endif
	endfor
	if l:erg == []
		return "g:vgen_data.".a:var_str
	else
		return l:erg[1].strpart(a:var_str,strlen(l:erg[0]))
	endif
endfunction

" binds a new variable, i.e. create a new context, i.e. push to the stack
function! s:bind_variable(var_str,var)
	for l:var in g:vgen_vars
		if l:var[0] == a:var_str
			throw "variable " . a:var_str . " already bound"
		endif
	endfor
	let l:erg = substitute(a:var,'^g','l','')
	let l:erg = substitute(l:erg,'\.','_','g')
	call insert(g:vgen_vars,[a:var_str,l:erg])	
	return l:erg	
endfunction

" unbinds a variable, i.e. pop from the stack
function! s:unbind_variable()
	call remove(g:vgen_vars,0)
endfunction 
doc/vimgen.txt	[[[1
146
*vimgen.txt*  vim inline generator / template expander

1. Overview                                     *vimgen-overview*

Vimgen is a simple template system that works inside a VIM buffer. The 
Input is a region of text. Everything until the first empty line in this 
region is considered the data section, everything below the first empty line
is the template section. 

The region of text can either be provided as range, e.g.

:1,10Vimgen

or you can mark the region in visual mode and type :Vimgen

:'>,'<Vimgen

This documentation has the following sections:

Overview - this section                         |vimgen-overview|
Options - configuration parameter               |vimgen-options| 
Data - description of the data section          |vimgen-data|
Template - description of the template section  |vimgen-template|
Examples - examples of vimgen usage             |vimgen-examples|


2. Options                                      *vimgen-options*

The following global variables can be set to influence vimgen's
behaviour:

g:vgen_sc    - start character of vimgen tags, defaults to «
g:vgen_ec    - end character of vimgen tags, defaults to »
g:vgen_lc    - the loop character (see |vimgen-template|), default to - 
g:vgen_sfc   - the "skip first iteration" character (see |vimgen-template|),
               default to !
g:vgen_delim - a text for the line that separates input from output
               default to ----vimgen---- 


3. Data                                         *vimgen-data*

The data section of the input (everything until the first empty line) has to 
be a valid VIM dictionary. Newlines ("\n") but no empty lines can be inserted for 
better readability (they are ignored). The data section is evaluated using VIM's 
eval function, hence it can include VIM functions, e.g. from |range_date.txt|.

Example: {'a':['1','2','3'], 'b':['a','b','c']}
Example: {'day':Range_date('20070615','20070715'), 'year':['2007','2008']}


4. Template                                     *vimgen-template*

The template section of the input (everything below the first empty line) is the
desired output (including newlines etc.) that has to be enriched/multiplied by the 
data using vimgen tags.

There are only three types of tags (for better readability shown with the default
characters, see |vimgen-options|):

«-var»...«-»          this tags loop over the variable 'var' which has to be 
                      a list. Inside the loop «var» references the element of
                      the current iteration

«!var»...«-»          used inside a loop over 'var'. The test/tags that are surrounded
                      by this tag are ignored in the first iteration of the loop. This
                      is useful for enumerations (see |vimgen-examples|)

«var»                 this tag is replaced with the variable 'var', where 'var'
                      either has only one value or the tag is surrounded by 
                      a loop iterating over 'var'. In case of nested dictionaries,
                      a dot notation is used, e.g. a.b.c

5. Examples                                     *vimgen-examples*

{'a':['spring','sommer','fall','winter']}

There are four seasons: «-a»«!a», «!»«a»«-».
----vimgen----
There are four seasons: spring, sommer, fall, winter.

----------------------------------------------------------------------

{'n':{'a':Range_int(1,5,1,"%03d"), 
      'b':Range_char('a','c')}}

«-n.a»«-n.b»«n.b»«n.a»
«-»«-»
----vimgen----
a001
b001
c001
a002
b002
c002
a003
b003
c003
a004
b004
c004
a005
b005
c005

----------------------------------------------------------------------

{'date':Range_month("200712","200803"), 'code':[['foo','0OF'',''OFF2'],['bar','RAB']]}

CREATE TABLE test_table
    (date_id  DATE NOT NULL, id1 NUMBER, id2 NUMBER) 
PARTITION BY RANGE (date_id) SUBPARTITION BY LIST (id1)
(
  «-date»«!date», 
  «!»PARTITION testpart_«date» VALUES LESS THAN (TO_DATE('«date»01', 'YYYYMMDD'))
  ( 
    «-code»«!code»,
    «!»SUBPARTITION testpart_«date»_«code[0]» VALUES ('«code[1]»')«-»
  )«-»
);
----vimgen----
CREATE TABLE test_table
    (date_id  DATE NOT NULL, id1 NUMBER, id2 NUMBER) 
PARTITION BY RANGE (date_id) SUBPARTITION BY LIST (id1)
(
  PARTITION testpart_200712 VALUES LESS THAN (TO_DATE('20071201', 'YYYYMMDD'))
  ( 
    SUBPARTITION testpart_200712_foo VALUES ('0OF','OFF2'),
    SUBPARTITION testpart_200712_bar VALUES ('RAB')
  ), 
  PARTITION testpart_200801 VALUES LESS THAN (TO_DATE('20080101', 'YYYYMMDD'))
  ( 
    SUBPARTITION testpart_200801_foo VALUES ('0OF','OFF2'),
    SUBPARTITION testpart_200801_bar VALUES ('RAB')
  ), 
  PARTITION testpart_200802 VALUES LESS THAN (TO_DATE('20080201', 'YYYYMMDD'))
  ( 
    SUBPARTITION testpart_200802_foo VALUES ('0OF','OFF2'),
    SUBPARTITION testpart_200802_bar VALUES ('RAB')
  ), 
  PARTITION testpart_200803 VALUES LESS THAN (TO_DATE('20080301', 'YYYYMMDD'))
  ( 
    SUBPARTITION testpart_200803_foo VALUES ('0OF','OFF2'),
    SUBPARTITION testpart_200803_bar VALUES ('RAB')
  )
);
