XPTemplate priority=personal

let s:f = g:XPTfuncs()

fun! s:f.build_choice( choices )
    let s:choice = s:f.Build( ( a:choices )[ s:f.V() ] )
    return s:choice
endfunction

fun! s:f.include_choice( choices )
    let s:choice = s:f.Build( '`:' . ( ( a:choices )[ s:f.V() ] ) . ':^' )
    return s:choice
endfunction

XPTvar $TRUE          1
XPTvar $FALSE         0
XPTvar $NULL          NULL
XPTvar $UNDEFINED     NULL
XPTvar $VOID_LINE     /* void */;
XPTvar $BRif \n

XPTinclude
      \ _common/common
      \ _condition/lisp.like

XPT - " (+ x y)
(- `:_numbers:^)

XPT + " (+ x y)
(+ `:_numbers:^)

XPT * " (+ x y)
(* `:_numbers:^)

XPT / " (+ x y)
(/ `:_numbers:^)

XPT = " (= x y)
(= `number-1^ `number-2^` `more...^` `number^` `more...^)

XPT /= " (/= x y)
(/= `number-1^ `number-2^` `more...^` `number^` `more...^)

XPT < " (< x y)
(< `number-1^ `number-2^` `more...^` `number^` `more...^)

XPT <= " (<= x y)
(<= `number-1^ `number-2^` `more...^` `number^` `more...^)

XPT > " (> x y)
(> `number-1^ `number-2^` `more...^` `number^` `more...^)

XPT >= " (>= x y)
(>= `number-1^ `number-2^` `more...^` `number^` `more...^)

XPT abs " (abs number)
(abs `number^)

XPT acons " (acons key datum alist)
(acons `key^ `datum^ `alist^)

XPT and " (and boolean boolean)
(and `boolean-1^` `boolean-2^`:_booleans:^)

XPT append " (append list list)
(append `list^`:_lists:^)

XPT apply " (apply function args...)
(apply `function-object^` `:_args:^)

XPT aref " (aref array subscripts...)
(aref `array^ `subscript^` `subscript...{{^` `subscript^` `subscript...^`}}^)

XPT _args hidden
`...args^` `arg^` `...args^

XPT array-total-size " (array-total-size array)
(array-total-size `array^)

XPT assoc " (assoc item alist)
(assoc `item^ `alist^` `:_key:^)

XPT atom " (atom object)
(atom `object^)

XPT _booleans hidden
` `...booleans^` `boolean^` `...booleans^

XPT boundp " (boundp symbol)
(boundp `symbol^)

XPT car " (car list)
(car `list^)

XPT cdr " (cdr list)
(cdr `list^)

XPT clear-output " (clear-output output-stream)
(clear-output` `output-stream^)

XPT cond " (cond (test result) ...)
(cond
  (`test^ `result^)` `...^
`  (`test^ `result^)` `...^)

XPT cons " (cons object object)
(cons `object-1^ `object-2^)

XPT consp " (consp object)
(consp `object^)

XPT copy-alist " (copy-alist alist)
(copy-alist `alist^)

XPT copy-list " (copy-list list)
(copy-list `list^)

XPT copy-tree " (copy-tree tree)
(copy-tree `tree^)

XPT defconstant " (defconstant name initial-value documentation)
(defconstant `name^ `initial-value^` "`documentation`"^)

XPT defmacro " (defmacro name (args) body)
(defmacro `name^ (`arg^` `...arg^` `arg^` `...arg^)
`  "`documentation`"^
  `body^` `...body^
`  `body^` `...body^)

XPT defun " (defun name (args) body)
(defun `name^ (`arg^` `...arg^` `arg^` `...arg^)
`  "`documentation`"^
  `body^` `...body^
`  `body^` `...body^)

XPT defparameter " (defparameter name initial-value documentation)
(defparameter `name^ `initial-value^` "`documentation`"^)

XPT defvar " (defvar name documentation)
(defvar `name^` `value^` "`documentation`"^)

XPT delete " (delete item sequence)
(delete `item^ `sequence^` `keys...{{^` `:_remove_and_delete_keys:^` `keys...^`}}^)

XPT delete-if synonym=dif|delif " (delete-if test sequence)
(delete-if `test^ `sequence^` `keys...{{^` `:_remove_if_and_delete_if_keys:^` `keys...^`}}^)

XPT delete-if-not synonym=difn|delifn " (delete-if-not test sequence)
(delete-if-not `test^ `sequence^` `keys...{{^` `:_remove_if_and_delete_if_keys:^` `keys...^`}}^)

XPT describe " (describe object stream)
(describe `object^` `stream^)

XPT divide alias=/ " (/ x y)

XPT do " (do ..)
(do
  ((`var^ `init^` `stepper^)` `...more^
`   (`var^ `init^` `stepper^)` `...more^)
  (`end-test^` `...end_form^
`   `end-form^` `...end_form^
   `return-value^)` `...body^
`  `body^` `...body^)

XPT documentation " (documentation x doc-type)
(documentation `x^ `doc-type^)

XPT dolist " (dolist ...)
(dolist (`var^ `list-form^` `result-form^)
  `body^)

XPT dotimes " (dotimes ...)
(dotimes (`index-var^ `upper-limit-form^` `result-form^)
  `body^)

XPT eighth " (eighth list)
(eighth `list^)

XPT elt " (elt sequence index)
(elt `sequence^ `index^)

XPT endp " (endp list)
(endp `list^)

XPT eq " (eq x y)
(eq `object-1^ `object-2^)

XPT equal " (equal x y)
(equal `object-1^ `object-2^)

XPT eql " (eql x y)
(eql `object-1^ `object-2^)

XPT eval " (eval form)
(eval `form^)

XPT fboundp " (fboundp symbol)
(fboundp `symbol^)

XPT fifth " (fifth list)
(fifth `list^)

XPT finish-output " (finish-output output-stream)
(finish-output` `output-stream^)

XPT first " (first list)
(first `list^)

XPT force-output " (force-output output-stream)
(force-output` `output-stream^)

XPT format " (format destination control-string args)
(format `destination^ "`control-string^"` `:_args:^)

XPT _forms hidden
`form^` `...form^
`form^` `...form^

XPT fourth " (fourth list)
(fourth `list^)

XPT fresh-line " (fresh-line object output-stream)
(fresh-line `output-stream^)

XPT funcall " (funcall functional-object args...)
(funcall `functional-object^` `:_args:^)

XPT functionp " (functionp object)
(functionp `object^)

XPT get " (get symbol indicator default)
(get `symbol^ `indicator^` `default^)

XPT getf " (getf plist indicator default)
(getf `plist^ `indicator^` `default^)

XPT gethash " (gethash key hash-table)
(gethash `key^ `hash-table^` `default^)

XPT if " (if .. then .. else)
(if `test^
  `then^
  `else^)

XPT intersection " (intersection set set)
(intersection `set-1^ `set-2^)

XPT _key hidden
`key...{{^ :key `function^`}}^

XPT lambda " (lambda (args) body)
(lambda (`arg^` `...arg^` `arg^` `...arg^)
`  `body^` `...body^
`  `body^` `...body^)

XPT last " (last list)
(last `list^)

XPT length " (length list)
(length `sequence^)

XPT _let_args hidden
`var^ `value^

XPT let " (let (var value) ...)
(let (`(`:_let_args:^)` `...another_var^
`      (`:_let_args:^)` `...another_var^)
  `form^` `...form^
  `form^` `...form^)

XPT letstar " (let* (var value) ...)
(let* (`(`:_let_args:^)` `...another_var^
`       (`:_let_args:^)` `...another_var^)
  `form^` `...form^
  `form^` `...form^)

XPT list " (list object ...)
(list `object^` `...object^` `object^` `...object^)

XPT listp " (listp object...)
(listp `object^)

XPT liststar " (list* object ...)
(list* `object^` `...object^` `object^` `...object^)

XPT _lists hidden
` `...list^` `list^` `...list^

XPT load " (load "filename")
(load "`file^")

XPT macroexpand " (macroexpand form env)
(macroexpand `form^` `env^)

XPT _make_array_adjustable hidden
 :adjustable `t^

XPT _make_array_displaced_index_offset hidden
 :make-array-displaced-index-offset `0^

XPT _make_array_displaced_to hidden
 :displaced-to `array^

XPT _make_array_element_type hidden
 :element-type `t^

XPT _make_array_fill_pointer hidden
 :fill-pointer `t^

XPT _make_array_keys hidden
`keys^
XSET keys=ChooseStr( 'element-type', 'initial-element', 'initial-contents', 'adjustable', 'fill-pointer', 'displaced-to', 'displaced-index-offset' )
XSET keys|post=include_choice( { " element-type" : "_make_array_element_type", " initial-element" : "_make_array_initial_element", " initial-contents" : "_make_array_initial_contents", " adjustable" : "_make_array_adjustable", " fill-pointer" : "_make_array_fill_pointer", " displaced-to" : "_make_array_displaced_to", " displaced-index-offset" : "_make_array_displaced_index_offset" } )

XPT _make_array_initial_contents hidden
 :initial-contents `object^

XPT _make_array_initial_element hidden
 :initial-element `object^

XPT make-array " (make array dimensions...)
(make-array `dimensions^` `keys...{{^` `:_make_array_keys:^` `keys...^`}}^)

XPT _make_hash_table_rehash_size hidden
 :rehash-size `size^

XPT _make_hash_table_size hidden
 :size `size^

XPT _make_hash_table_test hidden
 :test `test^
XSET test=ChooseStr( 'eq', 'eql', 'equal', 'equalp' )
XSET test|post=build_choice( { "eq" : "#'eq", "eql" : "#'eql", "equal" : "#'equal", "equalp" : "#'equalp" } )

XPT _make_hash_table_rehash_threshold hidden
 :rehash-threshold `threshold^

XPT _make_hash_table_keys hidden
`keys^
XSET keys=ChooseStr( 'test', 'size', 'rehash-size', 'rehash-threshold' )
XSET keys|post=include_choice( { " test" : "_make_hash_table_test", " size" : "_make_hash_table_size", " rehash-size" : "_make_hash_table_rehash_size", " rehash-threshold" : "_make_hash_table_rehash_threshold" } )

XPT make-hash-table " (make hash-table ...)
(make-hash-table` `keys...{{^` `:_make_hash_table_keys:^` `keys...^`}}^)

XPT mapc " (mapc function args...)
(mapc `function^` `:_args:^)

XPT mapcan " (mapcan function args...)
(mapcan `function^` `:_args:^)

XPT mapcar " (mapcar function args...)
(mapcar `function^` `:_args:^)

XPT mapcon " (mapcon function args...)
(mapcon `function^` `:_args:^)

XPT mapl " (mapl function args...)
(mapl `function^` `:_args:^)

XPT maplist " (maplist function args...)
(maplist `function^` `:_args:^)

XPT member " (member element list)
(member `element^ `list^)

XPT minus alias=- " (- number number)

XPT multiply alias=* " (* number number)

XPT nconc " (nconc list list)
(nconc `list^`:_lists:^)

XPT nintersection " (nintersection set set)
(nintersection `set-1^ `set-2^)

XPT ninth " (ninth list)
(ninth `list^)

XPT not " (not boolean)
(not `boolean^)

XPT nth " (nth number list)
(nth `number^ `list^)

XPT nthcdr " (nthcdr number list)
(nthcdr `number^ `list^)

XPT nreverse " (nreverse list)
(nreverse `list^)

XPT null " (null object)
(null `object^)

XPT numberp " (numberp object)
(numberp `object^)

XPT _numbers hidden
`number-1^ `number-2^` `...number^` `number^` `...number^

XPT nsublis " (nsublis alist tree)
(nsublis `alist^ `tree^)

XPT nsubst " (nsubst new old tree)
(nsubst `new^ `old^ `tree^)

XPT _objects hidden
` `...object^` `object^` `...object^

XPT _open_element_type hidden
 :element-type `element-type^
XSET element-type=:default

XPT _open_external_format hidden
 :external-format `file-format^
XSET file-format=:default

XPT _open_if_does_not_exist hidden
 :if-does-not-exist `_if_does_not_exist_action^
XSET _if_does_not_exist_action=ChooseStr( ':error', ':create', 'nil' )

XPT _open_if_exists hidden
 :if-exists `_if_exists_action^
XSET _if_exists_action=ChooseStr( ':error', ':new-version', ':rename', ':rename-and-delete', ':overwrite', ':append', ':supersede', 'nil' )

XPT _open_more hidden
` `element-type...{{^`:_open_element_type:^`}}^` `if-exists...{{^`:_open_if_exists:^`}}^` `if-does-not-exist...{{^`:_open_if_does_not_exist:^`}}^` `external-format...{{^`:_open_external_format:^`}}^

XPT open " (open ...)
(open `pathname^ :direction `direction^` `more...{{^`:_open_more:^`}}^)
XSET direction=ChooseStr( ':input', ':output', ':io', ':probe' )

XPT or " (or boolean boolean)
(or `boolean-1^` `boolean-2^`:_booleans:^)

XPT pairlis " (pairlis keys values alist)
(pairlis `keys^ `data^ `alist^)

XPT plus alias=+ " (+ x y)

XPT pop " (pop place)
(pop `place^)

XPT prin1 " (prin1 object output-stream)
(prin1 `object^` `output-stream^)

XPT princ " (princ object output-stream)
(princ `object^` `output-stream^)

XPT print " (print object output-stream)
(print `object^` `output-stream^)

XPT printbase " *PRINT-BASE*
*PRINT-BASE*

XPT printlevel " *PRINT-LEVEL*
*PRINT-LEVEL*

XPT printlength " *PRINT-LENGTH*
*PRINT-LENGTH*

XPT progn " (progn form ...)
(progn
  `form^`
  `...form^`
  `form^`
  `...form^)

XPT push " (push item place)
(push `item^ `place^)

XPT _pushnew_key hidden
 :key `function^

XPT _pushnew_keys hidden
`keys^
XSET keys=ChooseStr( 'key', 'test', 'test-not' )
XSET keys|post=include_choice( { " key" : "_pushnew_key", " test" : "_pushnew_test", " test-not" : "_pushnew_test_not" } )

XPT _pushnew_test hidden
 :test `function^

XPT _pushnew_test_not hidden
 :test-not `function^

XPT pushnew " (pushnew item place)
(pushnew `item^ `place^` `keys...{{^` `:_pushnew_keys:^` `keys...^`}}^)

XPT quote " (quote object)
(quote `object^)

XPT _rassoc_count hidden
 :count `nil^

XPT _rassoc_end hidden
 :end `nil^

XPT _rassoc_from_end hidden
 :from-end `t^

XPT _rassoc_key hidden
 :key `function^

XPT _rassoc_keys hidden
`keys^
XSET keys=ChooseStr( 'key', 'test', 'test-not' )
XSET keys|post=include_choice( { " key" : "_rassoc_key", " test" : "_rassoc_test", " test-not" : "_rassoc_test_not" } )

XPT _rassoc_test hidden
 :test `function^

XPT _rassoc_test_not hidden
 :test-not `function^

XPT _rassoc_if_keys hidden
`keys^
XSET keys=ChooseStr( 'key' )
XSET keys|post=include_choice( { " key" : "_rassoc_key" } )

XPT rassoc " (rassoc item sequence)
(rassoc `item^ `alist^` `keys...{{^` `:_rassoc_keys:^` `keys...^`}}^)

XPT rassoc-if synonym=rasif|rassocif " (rassoc-if test sequence)
(rassoc-if `predicate^ `alist^` `key...{{^`:_rassoc_key:^`}}^)

XPT rassoc-if-not synonym=rasifn|rassocifnot " (rassoc-if-not test sequence)
(rassoc-if-not `predicate^ `alist^` `key...{{^`:_rassoc_key:^`}}^)

XPT _read_options hidden
` `input-stream^` `eof-error-p^` `eof-value^` `recursive-p^

XPT read " (read ...)
(read` `options...{{^`:_read_options:^`}}^)
XSET direction=ChooseStr( ':input', ':output', ':io', ':probe' )

XPT readbase " *READ-BASE*
*READ-BASE*

XPT remf " (remf place indicator)
(remf `place^ `indicator^)

XPT remhash " (remhash key hash-table)
(remhash `key^ `hash-table^)

XPT remprop " (remprop symbol indicator)
(remprop `symbol^ `indicator^)

XPT _remove_and_delete_count hidden
 :count `nil^

XPT _remove_and_delete_end hidden
 :end `nil^

XPT _remove_and_delete_from_end hidden
 :from-end `t^

XPT _remove_and_delete_key hidden
 :key `function^

XPT _remove_and_delete_keys hidden
`keys^
XSET keys=ChooseStr( 'from-end', 'test', 'test-not', 'start', 'end', 'count', 'key' )
XSET keys|post=include_choice( { " from-end" : "_remove_and_delete_from_end", " test" : "_remove_and_delete_test", " test-not" : "_remove_and_delete_test_not", " start" : "_remove_and_delete_start", " end" : "_remove_and_delete_end", " count" : "_remove_and_delete_count", " key" : "_remove_and_delete_key" } )

XPT _remove_and_delete_start hidden
 :start `0^

XPT _remove_and_delete_test hidden
 :test `function^

XPT _remove_and_delete_test_not hidden
 :test-not `function^

XPT _remove_if_and_delete_if_keys hidden
`keys^
XSET keys=ChooseStr( 'from-end', 'start', 'end', 'count', 'key' )
XSET keys|post=include_choice( { " from-end" : "_remove_and_delete_from_end", " start" : "_remove_and_delete_start", " end" : "_remove_and_delete_end", " count" : "_remove_and_delete_count", " key" : "_remove_and_delete_key" } )

XPT remove " (remove item sequence)
(remove `item^ `sequence^` `keys...{{^` `:_remove_and_delete_keys:^` `keys...^`}}^)

XPT remove-if synonym=rif|remif|removeif " (remove-if test sequence)
(remove-if `test^ `sequence^` `keys...{{^` `:_remove_if_and_delete_if_keys:^` `keys...^`}}^)

XPT remove-if-not synonym=rifn|remifn|removeifnot " (remove-if-not test sequence)
(remove-if-not `test^ `sequence^` `keys...{{^` `:_remove_if_and_delete_if_keys:^` `keys...^`}}^)

XPT remprop " (remprop symbol indicator)
(remprop `symbol^ `indicator^)

XPT rest " (rest list)
(rest `list^)

XPT return " (return result)
(return `result^)

XPT reverse " (reverse list)
(reverse `list^)

XPT rplaca " (rplaca destination source)
(rplaca `destination^ `source^)

XPT rplacd " (rplacd destination source)
(rplacd `destination^ `source^)

XPT second " (second list)
(second `list^)

XPT set " (set symbol value)
(set `symbol^ `value^)

XPT setf " (setf place newvalue)
(setf `place^ `newvalue^)

XPT setq " (setq variable form)
(setq `:_var_form:^` `more...{{^ `Include:_var_form^` `more...^`}}^)

XPT seventh " (seventh list)
(seventh `list^)

XPT sixth " (sixth list)
(sixth `list^)

XPT stringp " (stringp object)
(stringp `object^)

XPT sublis " (sublis alist tree)
(sublis `alist^ `tree^)

XPT subst " (subst new old tree)
(subst `new^ `old^ `tree^)

XPT symbolfunction " (symbol-function symbol)
(symbol-function `symbol^)

XPT symbolp " (symbolp object)
(symbolp `object^)

XPT symbolvalue " (symbol-value symbol)
(symbol-value `symbol^)

XPT tenth " (tenth list)
(tenth `list^)

XPT terpri " (terpri object output-stream)
(terpri `output-stream^)

XPT third " (third list)
(third `list^)

XPT typep " (typep object)
(typep `object^)

XPT union " (union set set)
(union `set-1^ `set-2^)

XPT _var_form hidden
`variable^ `form^

XPT vector " (vector object...)
(vector `object^` `object...{{^` `object^` `object...^`}}^)

XPT vector-pop " (vector-pop vector)
(vector-pop `vector^)

XPT vector-push " (vector-push new-element vector)
(vector-push `new-element^ `vector^)

XPT when " (when ...)
(when `test^
  `:_forms:^)

XPT zerop " (zerop object)
(zerop `object^)

