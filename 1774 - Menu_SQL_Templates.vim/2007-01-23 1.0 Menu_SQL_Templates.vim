"  Menu_SQL_Templates.vim 
"  Last Change: Tue Jan 23 14:00 GMT
"  Maintainer:  Sanjeev Sapre
"  Version:     1.0 

" triggers
:menu &SQL_Template.Create\ Trigger.Row\ Trigger iCREATE OR REPLACE TRIGGER  ????<Enter>       BEFORE DELETE OR INSERT or UPDATE /* OF <ColumnList> */<Enter>      ON <table_name><Enter>       REFERENCING OLD AS OLD NEW AS NEW<Enter>        FOR EACH ROW<Enter>DECLARE <Enter>   -- ...  <Enter>  /* WHEN (<condition>) */ <Enter>BEGIN <Enter>   -- ...  <Enter>END;
:menu SQL_Template.Create\ Trigger.Statement\ Trigger iCREATE OR REPLACE TRIGGER ????  <Enter>     BEFORE DELETE OR INSERT OR UPDATE <Enter>       ON TABLE ????  <Enter>  REFERENCING OLD AS OLD NEW AS NEW <Enter>DECLARE <Enter>-- ...  <Enter>    BEGIN<Enter>-- ...  <Enter>END; 


" sequence
:menu SQL_Template.Sequence iCREATE SEQUENCE ????  <Enter>    INCREMENT BY 1 <Enter>    START WITH 1 <Enter>    MAXVALUE 99999 MINVALUE 1 <Enter>    CYCLE NOCACHE NOORDER;


" Index
:menu SQL_Template.Index  iCREATE INDEX <index_name> ON <table_name>(<columns>) <Enter>    /*INITRANS 2 <Enter>    MAXTRANS 255 <Enter>    PCTFREE 10 <Enter>    TABLESPACE <tablespace_name> <Enter>    STORAGE( <Enter>    INITIAL 10240 <Enter>    NEXT 10240 <Enter>    PCTINCREASE 50 <Enter>    MINEXTENTS 1 <Enter>    MAXEXTENTS 121)  */;


" Constraints
:menu SQL_Template.Add\ Constraints.Foreign\ Key iALTER TABLE <Table_name> <Enter>    ADD CONSTRAINT <constraint_name> <Enter>    FOREIGN KEY (<col1>,<col2>) <Enter>    REFERENCES < ref_table> (<ref_col1>,<ref_col2>); 
:menu SQL_Template.Add\ Constraints.Check\ Constraint iALTER TABLE <table_name> <Enter>    ADD CONSTRAINT <constraint_name1> CHECK ( <col1> IS NOT NULL) <Enter>    ADD CONSTRAINT <constraint_name2> CHECK ( <condition> );
:menu SQL_Template.Add\ Constraints.NOT\ NULL iALTER TABLE <table_name> <Enter>    MODIFY ( <column_name> CONSTRAINT <constrain_name> NOT NULL );
:menu SQL_Template.Add\ Constraints.Unique iALTER TABLE <table_name> <Enter>    ADD CONSTRAINT <constraint_name> <Enter>    UNIQUE ( <column1>,<column2>);
:menu SQL_Template.Add\ Constraints.Primary\ Key  iALTER TABLE <table_name> <Enter>    ADD CONSTRAINT <constraint_name> <Enter>    PRIMARY KEY (<col1>,<col2>) <Enter>    \/* USING INDEX TABLESPACE <tablespace_name> <Enter>    STORAGE( INITIAL <value> <Enter>    NEXT  <value>  ) */;


" Alter Table
:menu SQL_Template.Alter\ Table.Disable\ Triggers iALTER TABLE <tablename> DISABLE ALL TRIGGERS;
:menu SQL_Template.Alter\ Table.Drop\ Constraint iALTER TABLE <tablename> DROP CONSTRAINT <ConstraintName> ;
:menu SQL_Template.Alter\ Table.Disable\ constraint iALTER TABLE <TableName> DISABLE CONSTRAINT <ConstraintName> ;

"Session Related
:menu SQL_Template.Session.Kill iALTER SYSTEM KILL SESSION '<sid>,<serial#>';
:menu SQL_Template.Session.Set\ NLS_DATE_FORMAT iALTER SESSION SET NLS_DATE_FORMAT = 'YYYY MM DD HH24:MI:SS';
:menu SQL_Template.Session.SQL_TRACE iALTER SESSION SET SQL_TRACE = TRUE;

" Misclaneous
:menu SQL_Template.Locked\ Objects iSELECT owner, object_name FROM all_objects WHERE object_id IN (SELECT object_id FROM v$locked_object);
:menu SQL_Template.User\ Dependencies  iSELECT RPAD(name,20,' ')\|\|RPAD(type,20,' ')\|\| <Enter>    RPAD(referenced_owner,20,' ')\|\| <Enter>    RPAD(referenced_name,20,' ')\|\|referenced_type <enter>    FROM user_dependencies <Enter>    ORDER BY type, name;
:menu SQL_Template.Hierarchical\ Example i-- Emp table in scott<Enter>SELECT LPAD(' ',3*(LEVEL-1)) \|\| NAME hierarchy_list , <Enter>    START WITH empid  = :l_empid <Enter>    FROM emp <Enter>    CONNECT BY PRIOR mgr = empid ;


" Hints
:menu SQL_Template.Hints.Index.Index  i/*+ INDEX (<table_name> <index_name1> <index_name2>) */ -- Use index
:menu SQL_Template.Hints.Index.Fast\ Full i/*+ INDEX_FFS (<table_name> <index_name1> <index_name2>) */ -- Fast full scan
:menu SQL_Template.Hints.Index.Index\ Asc i/*+ INDEX_ASC (<table_name> <index_name1> <index_name2>) */ -- Ascending index
:menu SQL_Template.Hints.Optimizer\ Goal.All\ rows  i/*+ ALL_ROWS */ -- Optimizer goal
:menu SQL_Template.Hints.Optimizer\ Goal.Choose  i/*+ CHOOSE */ -- Optimizer decides
:menu SQL_Template.Hints.Optimizer\ Goal.First\ Row i/*+ FIRST_ROW */ -- Optimizer goal
:menu SQL_Template.Hints.Full\ Table i/*+ FULL(<table_name>) */ -- Full table scan
:menu SQL_Template.Hints.Push\ Query i/*+ PUSH_SUBQ */ -- Evaluate (cheap) subqueries first
:menu SQL_Template.Hints.Ordered i/*+ ORDERED */ -- In order of FROM-Clause
:menu SQL_Template.Hints.Star i/*+ STAR(<table_name> <index_name>) */ -- ORDERED and USE_NL and INDEX
:menu SQL_Template.Hints.And\ Equal i/*+ AND_EQUAL (<table_name> <index_name1> <index_name2> . . .<index_name5>) */ -- Explicitely state indices
:menu SQL_Template.Hints.Hash  i/*+ HASH(table) */ -- Explicit hash scan
:menu SQL_Template.Hints.Merge\ AntiJoin i/*+ MERGE_AJ (<table_name>) */ -- Merge Anti Join
:menu SQL_Template.Hints.Hash\ AntiJoin  i/*+ HASH_AJ(table) */ -- Transforms NOT IN into Anti Join
:menu SQL_Template.Hints.No\ Merge i/*+ NO_MERGE(<table_name1> <table_name2>) */ -- Do NOT use Sorted MERGE
:menu SQL_Template.Hints.RowID i/*+ ROWID (<table_name>) */ -- Access by ROWID
:menu SQL_Template.Hints.Concatenation i/*+ USE_CONCAT */ -- Transform OR into UNION ALL
:menu SQL_Template.Hints.Use\ Merge  i/*+ USE_MERGE(<table_name>) */ -- Use Sorted MERGE
:menu SQL_Template.Hints.Use\ Nested\ Loops  i/*+ USE_NL(<table_name>) */ -- Use NESTED LOOPS
:menu SQL_Template.Hints.Use\ Hash i/*+ USE_HASH(<table_name1> <table_name2>) */ -- Use HASH Join
