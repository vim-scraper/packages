"
" $Id: php.vim,v 1.3 2002/11/01 00:04:48 culley Exp culley $  
"
" ===================================================================
" I wrote these php macros for my personal use.  They're
" freely-distributable and freely-modifiable.
" 
" If you do make any major additions or changes, or even just
" have a suggestion for improvement, feel free to let me know.
" I'd appreciate any suggestions.
"
" quite a few of the abbreviations ouput PEAR code snippets,
" mostly DB and HTML/Table.  
"
" There is a section of abbreviations at the end of this script
" that *do not* use the php prefix.  These could very well mess
" you up if you aren't used to them...
"
" many of the insert mode abbreviations leave 'x' characters here
" and there as place holders to jump to.  <c-x> or <leader>x will
" take you to the next 'x' I have added this functionality to F7 too
"
" culley@ml1.net 
" someday I will get a web page together...
" ===================================================================

" cindent is helpful for many of the insert mode abbreviations
set cindent
set cinoptions==0
set tags=/my/tags
set fdm=marker
" can be nice to have your html settings present too
so ~/.vim/ftplugin/html.vim
" use that excelent sql script available on vim.org
:DB db_name

"
" ===================================================================
" maps
" ===================================================================
"

" -------------------------------------------------------------------
" many of the insert mode abbreviations leave 'x' characters here and
" there as place holders to jump to.  <c-x> or <leader>x will take you
" to the next 'x'. I am not sure that I like this... might change. 
" -------------------------------------------------------------------
nmap    <leader>x       /x<cr>s
imap    <c-x>           <esc>/x<cr>s
imap    <F7>            <esc>/x<cr>s
nmap    <F7>            <esc>/x<cr>s
nmap    <c-x>           /x<cr>s

" -------------------------------------------------------------------
" when the cursor is on a php variable use these maps to open a new 
" line and insert  print("$variable<br />");
" useful for debugging
" -------------------------------------------------------------------
nmap    <leader>pv yiwoprint("$<esc>pa<br />");
vmap    <leader>pv yoprint("$<esc>pa<br />");

" -------------------------------------------------------------------
" visual map to insert a vim fold in php comments
" -------------------------------------------------------------------
vmap    <leader>fold mz:<ESC>'<O// {{{<ESC>'>o// }}}<ESC>`z
 
" -------------------------------------------------------------------
"  when the cursor is on a php array variable use this map to open a 
"  new line below the cursor and insert an array_push function.
"  Example:
"  $test = array();                 //cursor is on test
"  array_push($test, <cursor>);     //this line will be inserted
"
" -------------------------------------------------------------------
nmap    <leader>ap yiwoarray_push($<esc>pa, X);<esc>FXs
vmap    <leader>ap yoarray_push($<esc>pa, X);<esc>FXs

" -------------------------------------------------------------------
"  when the cursor is on a php array variable use this map to open a 
"  new line below the cursor and insert the extract function
"  Example:
"  $test = array();           //cursor is on test
"  extract($test, EXTR_SKIP); //this line will be inserted
"  EXTR_SKIP -- If there is a collision, don't overwrite the existing 
"               variable
"
" -------------------------------------------------------------------
nmap    <leader>ae yiwoextract($<esc>pa, EXTR_SKIP);<esc>
vmap    <leader>ae yoextract($<esc>pa, EXTR_SKIP);<esc>

" -------------------------------------------------------------------
"  when the cursor is on a php array variable use this map to open a 
"  new line below the cursor and insert a print_r function.
"  Example:
"  $test = array(); //cursor is on test
"  print_r($test);  //this line will be inserted
"
" -------------------------------------------------------------------
nmap    <leader>apr yiwoprint_r($<esc>pa);<esc>
vmap    <leader>apr yoprint_r($<esc>pa);<esc>

"
" ===================================================================
" insert mode abbrieviations:  control structures
" ===================================================================
"

"
" -------------------------------------------------------------------
" phpif Output:
" -------------------------------------------------------------------
"       if ($<cursor>) {
"       }
" -------------------------------------------------------------------
" Usage:  cintent setting should indent properly after opening a line
" -------------------------------------------------------------------
iab phpif if ($X) {<cr>}<esc>?X<cr>s<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpifl (ifl-- if line) Output:
" -------------------------------------------------------------------
"       $<cursor> = (x) ? x : x;       
" -------------------------------------------------------------------
iab phpifl $X = ($x) ? $x : $x;<esc>FXs<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpife (ife-- if else) Output:
" -------------------------------------------------------------------
"       if ($<cursor>) {
"       } else {
"       }
" -------------------------------------------------------------------
iab phpife if ($X) {<cr>} else {<cr>}<esc>?X<cr>s<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpswitch Output:
" -------------------------------------------------------------------
"       switch ($<cursor>) {
"       Case $x:
"           break;
"       }
" -------------------------------------------------------------------
iab phpswitch switch ($X) {<cr>Case $x:<cr>break;<cr>}<esc>?X<cr>s<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpfor Output:
" -------------------------------------------------------------------
"       for($i=0;$i<cursor>;$i++) {
"       }
" -------------------------------------------------------------------
iab phpfor for($i=0;$iX;$i++) {<cr>}<esc>?X<cr>s<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpfore Output:
" -------------------------------------------------------------------
"       foreach($<cursor> as $k => $v) {
"       }
" -------------------------------------------------------------------
iab phpfore foreach($X as $k => $v) {<cr>}<esc>?X<cr>s<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpwhile Output:
" -------------------------------------------------------------------
"       while ($<cursor>) {
"       }
" -------------------------------------------------------------------
iab phpwhile while ($X) {<cr>}<esc>?X<cr>s<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpfunc Output:
" -------------------------------------------------------------------
"       // {{{ function 
"       function <cursor>()
"       {
"
"       }
"       // }}}
" -------------------------------------------------------------------
iab phpfunc // {{{ function <cr>function X()<cr>{<cr><cr>}<cr>// }}}<esc>?X<cr>s<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phphtml Output:
" -------------------------------------------------------------------
"       ?>
"       <cursor>
"       <?php
" -------------------------------------------------------------------
" Usage: cut a hole in the php output for some html
" -------------------------------------------------------------------
iab phphtml ?><cr><?php<esc>O<c-o>:call getchar()<cr>



" ===================================================================
" insert mode abbreviations:  general
" ===================================================================
"

"
" -------------------------------------------------------------------
" phph Output:
" -------------------------------------------------------------------
"       <?php
"       <cursor>
"       ?>
" -------------------------------------------------------------------
iab phph <?php<cr>X<cr>?><esc>?X<cr>s<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpo Output:
" -------------------------------------------------------------------
"       <?= $<cursor> ?>
" -------------------------------------------------------------------
iab phpo <?= $X ?><esc>FXs<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpsql Output:
" -------------------------------------------------------------------
"       $sql = "<cursor>";
" -------------------------------------------------------------------
iab phpsql $sql = "X";<esc>FXs<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpid Output:
" -------------------------------------------------------------------
"       // $Id: php.vim,v 1.3 2002/11/01 00:04:48 culley Exp culley $ <cursor> 
" -------------------------------------------------------------------
iab phpid // $Id: php.vim,v 1.3 2002/11/01 00:04:48 culley Exp culley $

"
" -------------------------------------------------------------------
" phppost Output:
" -------------------------------------------------------------------
"       $_POST["<cursor>"]
" -------------------------------------------------------------------
iab phppost $_POST["X"]<esc>FXs<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpget Output:
" -------------------------------------------------------------------
"       $_GET["<cursor>"]
" -------------------------------------------------------------------
iab phpget $_GET["X"]<esc>FXs<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpsess Output:
" -------------------------------------------------------------------
"       $_SESSION["<cursor>"]
" -------------------------------------------------------------------
iab phpsess $_SESSION["X"]<esc>FXs<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpserver Output:
" -------------------------------------------------------------------
"       $_SERVER["<cursor>"]
" -------------------------------------------------------------------
iab phpserver $_SERVER["X"]<esc>FXs<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpcom Output:
" -------------------------------------------------------------------
"       /**
"       * <cursor>
"       */
" -------------------------------------------------------------------
iab phpcom /**<cr><bs>*<space>X<cr>*/<esc>?X<cr>s<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phploc Output:
" -------------------------------------------------------------------
"       header("Location: <cursor>.php");
" -------------------------------------------------------------------
iab phploc header("Location: X.php");<esc>FXs<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpinc Output:
" -------------------------------------------------------------------
"       include_once("<cursor>");
" -------------------------------------------------------------------
iab phpinc include_once("X");<esc>FXs<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpreq Output:
" -------------------------------------------------------------------
"       require_once("<cursor>");
" -------------------------------------------------------------------
iab phpreq require_once("X");<esc>FXs<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phppm Output:
" -------------------------------------------------------------------
"       preg_match('/^<cursor>$/',$x,$matches);
" -------------------------------------------------------------------
iab phppm preg_match('/^X$/',$x,$matches);<esc>FXs<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phppc Output:
" -------------------------------------------------------------------
"       $dbh =& getConnection();<cursor>
" -------------------------------------------------------------------
" Usage: not going to be very useful to most people.  This calls a
" function that returns a PEAR connection object.  
" -------------------------------------------------------------------
iab phpgc $dbh =& getConnection();<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phplv Output:
" -------------------------------------------------------------------
"       $sql = "select last_value from <cursor>"; 
"       $last_value = $dbh->getOne($sql);
" -------------------------------------------------------------------
" Usage: look up the last value from a postgresql sequence
" -------------------------------------------------------------------
iab phplv $sql = "select last_value from X";<esc>o$last_value = $dbh->getOne($sql);<esc>?X<cr>s<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpq Output:
" -------------------------------------------------------------------
"       $dbh->query($sql);<cursor>
" -------------------------------------------------------------------
" Usage: execute query against a pear DB handle.  see phpsql
" -------------------------------------------------------------------
iab phpq $dbh->query($sql);<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpqr Output:
" -------------------------------------------------------------------
"       $result = $dbh->query($sql);
"
"       while ($row = $result->fetchRow()) {
"           $<cursor> = $row['x'];
"       }
" -------------------------------------------------------------------
" Usage: return output for looping through a pear DB resultset
" -------------------------------------------------------------------
iab phpqr $result = $dbh->query($sql);<cr><cr>while ($row = $result->fetchRow()) {<cr>$X = $row['x'];<cr>}<esc>?X<cr>s<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpgo Output:
" -------------------------------------------------------------------
"       $<cursor> = $dbh->getOne($sql);
" -------------------------------------------------------------------
iab phpgo $X = $dbh->getOne($sql);<esc>FXs<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpga Output:
" -------------------------------------------------------------------
"       $data = $dbh->getAll($sql);
" -------------------------------------------------------------------
iab phpga $data = $dbh->getAll($sql);<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpgassoc Output:
" -------------------------------------------------------------------
"       $data = $dbh->getAssoc($sql);
" -------------------------------------------------------------------
iab phpgassoc $data = $dbh->getAssoc($sql);<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phprow Output:
" -------------------------------------------------------------------
"       $row["<cursor>"];
" -------------------------------------------------------------------
iab phprow $row["X"];<esc>FXs<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpnr Output:
" -------------------------------------------------------------------
"       $result->numRows();<cursor>
" -------------------------------------------------------------------
iab phpnr $result->numRows();<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpes Output:
" -------------------------------------------------------------------
"       echo "$sql<br />";<cursor>
" -------------------------------------------------------------------
" Usage: debugging tool-- see what sql was sent to the database
" -------------------------------------------------------------------
iab phpes echo "$sql<br />";<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpself Output:
" -------------------------------------------------------------------
"       $_SERVER['PHP_SELF']<cursor>
" -------------------------------------------------------------------
iab phpself $_SERVER['PHP_SELF']<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phptbl Output:
" -------------------------------------------------------------------
"       $table1 = new HTML_Table(TBL_ATTRIBUTES);
" -------------------------------------------------------------------
" Usage: create a new PEAR table object replace TBL_ATTRIBUTES 
" with your favorite table attributes.
" -------------------------------------------------------------------
iab phptbl $table1 = new HTML_Table(TBL_ATTRIBUTES);<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phptblr Output:
" -------------------------------------------------------------------
"       $table1->addRow(array(<cursor>));
" -------------------------------------------------------------------
" Usage: I don't use this much but it can be used to add a table row
" usually just cut and paste...
" -------------------------------------------------------------------
iab phptblr $table1->addRow(array(X));<esc>?X<cr>s<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phptbld Output:
" -------------------------------------------------------------------
"       $table1->display();
" -------------------------------------------------------------------
iab phptbld $table1->display();<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phptc Output:
" -------------------------------------------------------------------
"       to_char(<cursor>, 'MM/DD/YYYY') as x
" -------------------------------------------------------------------
" Usage: this is actually a sql abbreviation.  output the postgresql
" date formating function to_char
" -------------------------------------------------------------------
iab phptc to_char(X, 'MM/DD/YYYY') as x<esc>?X<cr>s<c-o>:call getchar()<cr>

"
" -------------------------------------------------------------------
" phpfold Output:
" -------------------------------------------------------------------
"       // {{{ <cursor 
"       // }}}
" -------------------------------------------------------------------
" Usage: creates a vim fold behind php comments.
" -------------------------------------------------------------------
iab phpfold // {{{ X <cr>// }}}<esc>?X<cr>s<c-o>:call getchar()<cr>

"
" ===================================================================
" abbreviations without prefixes-- can mess up your typing quite a bit
" use iabclear before pasting anthing.  Could be expanded ad nauseam
" Expand these abbreiations by hitting the space bar
"
" Not sure if I want to switch these to having the php prefix...
" ===================================================================
"
iab echo echo "";<left><left><c-o>:call getchar()<cr>
iab isset isset($X)<esc>FXs<c-o>:call getchar()<cr>
iab strlen strlen($X)<esc>FXs<c-o>:call getchar()<cr>
iab unset unset($X);<esc>FXs<c-o>:call getchar()<cr>
iab str_replace str_replace("X", "x", $x);<esc>FXs<c-o>:call getchar()<cr>
iab return return $X;<esc>FXs<c-o>:call getchar()<cr>
iab global global $X;<esc>FXs<c-o>:call getchar()<cr>
iab preg_match preg_match('/^X$/',$x,$matches)<esc>Fxs<c-o>:call getchar()<cr>


