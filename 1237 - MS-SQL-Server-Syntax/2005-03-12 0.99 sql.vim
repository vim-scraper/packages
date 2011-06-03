" Vim syntax file
" Language:    Microsoft SQL Server 2000
"
" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

syn match sqlKeyword "\<CONTAINS\>"


syn keyword sqlKeyword ABS ABSOLUTE ACOS ACTION ADA ADD ADMIN AFTER AGGREGATE ALIAS ALL ALLOCATE ALTER AND ANSI_DEFAULTS ANSI_NULL_DFLT_OFF ANSI_NULL_DFLT_ON ANSI_NULLS ANSI_PADDING ANSI_WARNINGS ANY APP_NAME ARE ARITHABORT ARITHIGNORE ARRAY AS ASC ASCII ASIN ASSERTION AT ATAN ATN2 AUTHORIZATION AVG
syn keyword sqlKeyword BACKUP BEFORE BEGIN BETWEEN BIGINT BINARY BIT BIT_LENGTH BLOB BOOLEAN BOTH BREADTH BREAK BROWSE BULK BY
syn keyword sqlKeyword CALL CASCADE CASCADED CASE CAST CATALOG CEILING CHAR CHAR CHAR_LENGTH CHARACTER CHARACTER_LENGTH CHARINDEX CHECK CHECKPOINT CLASS CLOB CLOSE CLUSTERED COALESCE COL_LENGTH COL_NAME COLLATE COLLATION COLLATIONPROPERTY COLUMN COLUMNPROPERTY COMMIT COMPLETION COMPUTE CONCAT_NULL_YIELDS_NULL CONNECT CONNECTION CONSTRAINT CONSTRAINTS CONSTRUCTOR CONTAINSTABLE CONTINUE CONVERT CORRESPONDING COS COT COUNT CREATE CROSS CUBE CURRENT CURRENT_DATE CURRENT_PATH CURRENT_ROLE CURRENT_TIME CURRENT_TIMESTAMP CURRENT_USER CURSOR CURSOR_CLOSE_ON_COMMIT CURSOR_STATUS CYCLE
syn keyword sqlKeyword DATA DATABASE DATABASEPROPERTY DATABASEPROPERTYEX DATE DATEADD DATEDIFF DATEFIRST DATEFORMAT DATELENGTH DATENAME DATEPART DATETIME DAY DB_ID DB_NAME DBCC DEADLOCK_PRIORITY DEALLOCATE DEC DECIMAL DECLARE DEFAULT DEFERRABLE DEFERRED DEGREES DELETE DENY DEPTH DEREF DESC DESCRIBE DESCRIPTOR DESTROY DESTRUCTOR DETERMINISTIC DIAGNOSTICS DICTIONARY DIFFERENCE DISABLE_DEF_CNST_CHK DISCONNECT DISK DISTINCT DISTRIBUTED DOMAIN DOUBLE DROP DUMMY DUMP DYNAMIC
syn keyword sqlKeyword EACH ELSE END END-EXEC END-EXEC EQUALS ERRLVL ESCAPE EVERY EXCEPT EXCEPTION EXEC EXECUTE EXISTS EXIT EXP EXTERNAL EXTRACT
syn keyword sqlKeyword FALSE FETCH FILE FILE_ID FILE_NAME FILEGROUP_ID FILEGROUP_NAME FILEGROUPPROPERTY FILEPROPERTY FILLFACTOR FIPS_FLAGGER FIRST FLOAT FLOOW FMTONLY FOR FORCEPLAN FOREIGN FORMATMESSAGE FORTRAN FOUND FREE FREETEXT FREETEXTTABLE FROM FULL FULLTEXTCATALOGPROPERTY FULLTEXTSERVICEPROPERTY FUNCTION
syn keyword sqlKeyword GENERAL GET GETANSINULL GETDATE GETUTCDATE GLOBAL GO GOTO GRANT GROUP GROUPING
syn keyword sqlKeyword HAS_DBACCESS HAVING HOLDLOCK HOST HOST_ID HOST_NAME HOUR
syn keyword sqlKeyword IDENT_CURRENT IDENT_INCR IDENT_SEED IDENTITY IDENTITY_INSERT IDENTITYCOL IF IGNORE IMAGE IMMEDIATE IMPLICIT_TRANSACTIONS IN INCLUDE INDEX INDEX_COL INDEXKEY_PROPERTY INDEXPROPERTY INDICATOR INITIALIZE INITIALLY INNER INOUT INPUT INSENSITIVE INSERT INT INTEGER INTERSECT INTERVAL INTO IO IS IS_MEMBER IS_SRVROLEMEMBER ISDATE ISNULL ISNUMERIC ISOLATION ITERATE
syn keyword sqlKeyword JOIN
syn keyword sqlKeyword KEY KILL
syn keyword sqlKeyword LANGUAGE LARGE LAST LATERAL LEADING LEFT LEN LESS LEVEL LIKE LIMIT LINENO LOAD LOCAL LOCALTIME LOCALTIMESTAMP LOCATOR LOCK_TIMEOUT LOG LOG10 LOWER LTRIM
syn keyword sqlKeyword MAP MATCH MAX MIN MINUTE MODIFIES MODIFY MODULE MONEY MONTH
syn keyword sqlKeyword NAMES NATIONAL NATURAL NCHAR NCHAR NCLOB NEW NEWID NEXT NO NOCHECK NOCOUNT NOEXEC NONCLUSTERED NONE NOT NTEXT NULL NULLIF NUMERIC NUMERIC_ROUNDABORT NVARCHAR
syn keyword sqlKeyword OBJECT OBJECT_ID OBJECT_NAME OBJECTPROPERTY OCTET_LENGTH OF OFF OFFSETS OLD ON ONLY OPEN OPENDATASOURCE OPENQUERY OPENROWSET OPENXML OPERATION OPTION OR ORDER ORDINALITY OUT OUTER OUTPUT OVER OVERLAPS
syn keyword sqlKeyword PAD PARAMETER PARAMETERS PARSENAME PARSEONLY PARTIAL PASCAL PATH PATINDEX PERCENT PERMISSIONS PI PLAN POSITION POSTFIX POWER PRECISION PREFIX PREORDER PREPARE PRESERVE PRIMARY PRINT PRIOR PRIVILEGES PROC PROCEDURE PROFILE PUBLIC
syn keyword sqlKeyword QUERY_GOVERNOR_COST_LIMIT QUOTED_IDENTIFIER QUOTENAME RADIANS RAISERROR RAND READ READS READTEXT REAL RECONFIGURE RECURSIVE REF REFERENCES REFERENCING RELATIVE REMOTE_PROC_TRANSACTIONS REPLACE REPLICATE REPLICATION RESTORE RESTRICT RESULT RETURN RETURNS REVERSE REVOKE RIGHT ROLE ROLLBACK ROLLUP ROUND ROUTINE ROW ROWCOUNT ROWCOUNT_BIG ROWGUIDCOL ROWS RTRIM RULE
syn keyword sqlKeyword SAVE SAVEPOINT SCHEMA SCOPE SCOPE_IDENTITY SCROLL SEARCH SECOND SECTION SELECT SEQUENCE SERVERPROPERY SESSION SESSION_USER SESSIONPROPERTY SET SETS SETUSER SHOWPLAN_ALL SHOWPLAN_TEXT SHUTDOWN SIGN SIN SIZE SMALLDATETIME SMALLINT SMALLMONEY SOME SOUNDEX SPACE SPECIFIC SPECIFICTYPE SQL SQL_VARIANT SQL_VARIANT_PROPERTY SQLCA SQLCODE SQLERROR SQLEXCEPTION SQLSTATE SQLWARNING SQRT SQUARE START STATE STATEMENT STATIC STATISTICS STATS_DATE STR STRUCTURE STUFF SUBSTRING SUM SUSER_SID SUSER_SNAME SYSTEM_USER TABLE
syn keyword sqlKeyword TAN TEMPORARY TERMINATE TEXT TEXTPTR TEXTSIZE TEXTVALID THAN THEN TIME TIMESTAMP TIMEZONE_HOUR TIMEZONE_MINUTE TINYINT TO TOP TRAILING TRAN TRANSACTION TRANSLATE TRANSLATION TREAT TRIGGER TRIM TRUE TRUNCATE TSEQUAL TYPE_PROPERTY
syn keyword sqlKeyword UNDER UNICODE UNION UNIQUE UNIQUEIDENTIFIER UNKNOWN UNNEST UPDATE UPDATETEXT UPPER USAGE USE USER USER_ID USER_NAME USING
syn keyword sqlKeyword VALUE VALUES VARBINARY VARCHAR VARIABLE VARYING VIEW
syn keyword sqlKeyword WAITFOR WHEN WHENEVER WHERE WHILE WITH WITHOUT WORK WRITE WRITETEXT
syn keyword sqlKeyword XACT_ABORT YEAR ZONE

syn keyword sqlfunction fn_get_sql fn_helpcollations fn_listextendproperty
syn keyword sqlfunction fn_serversharedrives fn_trace_geteventinfo
syn keyword sqlfunction fn_trace_getfilterinfo fn_trace_getinfo fn_trace_gettable
syn keyword sqlfunction fn_virtualfilestats fn_virtualservermodes
syn keyword sqlFunction sp_ActiveDirectory_Obj sp_ActiveDirectory_SCP sp_add_agent_parameter sp_enableagentoffload sp_add_agent_profile sp_enumcustomresolvers sp_add_alert sp_help_jobhistory sp_add_category sp_help_jobschedule sp_add_data_file_recover_suspect_db sp_helpconstraint sp_add_job  sp_help_jobserver sp_add_jobschedule  sp_help_jobstep sp_add_jobserver sp_help_notification sp_add_jobstep  sp_help_operator sp_add_log_file_recover_suspect_db sp_helpextendedproc
syn keyword sqlFunction sp_add_log_shipping_database sp_delete_log_shipping_database sp_add_log_shipping_plan sp_delete_log_shipping_plan sp_add_log_shipping_plan_database sp_delete_log_shipping_plan_database sp_add_log_shipping_primary sp_delete_log_shipping_primary sp_add_log_shipping_secondary sp_delete_log_shipping_secondary sp_add_maintenance_plan sp_delete_maintenance_plan_db sp_add_maintenance_plan_db sp_delete_maintenance_plan_job sp_add_maintenance_plan_job sp_help_maintenance_plan sp_add_notification  sp_help_targetserver sp_add_operator  sp_help_targetservergroup sp_add_targetservergroup sp_helptask
syn keyword sqlFunction sp_add_targetsvrgrp_member sp_manage_jobs_by_login sp_addalias sp_droprolemember sp_addapprole sp_dropserver sp_addarticle sp_enumdsn sp_adddistpublisher  sp_enumfullsubscribers sp_adddistributiondb sp_expired_subscription_cleanup sp_adddistributor sp_generatefilters sp_addextendedproc sp_helpdb sp_addextendedproperty sp_helpdevice sp_addgroup sp_dropsrvrolemember
syn keyword sqlFunction sp_addlinkedserver sp_indexes sp_addlinkedsrvlogin sp_dropuser sp_addlinkedsrvlogin sp_linkedservers sp_addlogin sp_grantdbaccess sp_addmergealternatepublisher sp_getagentoffloadinfo sp_addmergearticle sp_getmergedeletetype sp_addmergefilter sp_get_distributor sp_addmergepublication sp_getqueuedrows
syn keyword sqlFunction sp_addmergepullsubscription sp_getsubscriptiondtspackagename sp_addmergepullsubscription_agent sp_grant_publication_access sp_addmergesubscription sp_help_agent_default sp_addmessage sp_helpfile sp_addpublication  sp_help_agent_parameter sp_addpublication_snapshot sp_help_agent_profile sp_addpublisher70 sp_helparticle
syn keyword sqlFunction sp_addpullsubscription sp_helparticlecolumns sp_addpullsubscription_agent sp_helparticledts sp_addremotelogin sp_grantlogin sp_addrole sp_helpdbfixedrole sp_addrolemember sp_helpgroup sp_addscriptexec sp_helpdistpublisher sp_addserver sp_helplinkedsrvlogin
syn keyword sqlFunction sp_addsrvrolemember sp_helplogins sp_addsubscriber sp_helpdistributiondb sp_addsubscriber_schedule sp_helpdistributor sp_addsubscription sp_helpmergealternatepublisher sp_addsynctriggers sp_helpmergearticle sp_addtabletocontents sp_helpmergearticlecolumn sp_addtask sp_msx_defect sp_addtype sp_helpfilegroup
syn keyword sqlFunction sp_addumpdevice sp_helpindex sp_adduser sp_helpntgroup sp_adjustpublisheridentityrange sp_helpmergearticleconflicts sp_altermessage sp_helplanguage sp_apply_job_to_targets sp_msx_enlist sp_approlepassword sp_helpremotelogin sp_article_validation sp_helpmergeconflictrows sp_articlecolumn sp_helpmergedeleteconflictrows sp_articlefilter  sp_helpmergefilter
syn keyword sqlFunction sp_articlesynctranprocs sp_helpmergepublication sp_articleview sp_helpmergepullsubscription sp_attach_db sp_helpsort sp_attach_single_file_db sp_helpstats sp_attachsubscription sp_helpmergesubscription sp_autostats sp_helpserver sp_bindefault sp_helptext sp_bindrule sp_helptrigger sp_bindsession sp_indexoption
syn keyword sqlFunction sp_browsemergesnapshotfolder sp_help_publication_access sp_browsereplcmds sp_helppullsubscription sp_browsesnapshotfolder sp_helppublication sp_can_tlog_be_applied sp_get_log_shipping_monitor_info sp_catalogs sp_primarykeys sp_certify_removable sp_invalidate_textptr sp_change_agent_parameter sp_helpreplfailovermode
syn keyword sqlFunction sp_change_agent_profile sp_helpreplicationdboption sp_change_monitor_role sp_remove_log_shipping_monitor sp_change_primary_role sp_resolve_logins sp_change_secondary_role sp_update_log_shipping_monitor_info sp_change_subscription_properties sp_reinitpullsubscription sp_change_users_login sp_helpsrvrole sp_changearticle  sp_helpreplicationoption
syn keyword sqlFunction sp_changedbowner sp_helprole sp_changedistpublisher sp_helpsubscriberinfo sp_changedistributiondb sp_helpsubscription sp_changedistributor_password sp_helpsubscription_properties sp_changedistributor_property sp_ivindexhasnullcols sp_changegroup sp_helprolemember sp_changemergearticle sp_link_publication sp_changemergefilter sp_marksubscriptionvalidation
syn keyword sqlFunction sp_changemergepublication sp_mergearticlecolumn sp_changemergepullsubscription sp_mergecleanupmetadata sp_changemergesubscription sp_mergedummyupdate sp_changeobjectowner sp_helprotect sp_changepublication sp_mergesubscription_cleanup sp_changesubscriber sp_publication_validation
syn keyword sqlFunction sp_changesubscriber_schedule sp_refreshsubscriptions sp_changesubscriptiondtsinfo sp_reinitmergepullsubscription sp_changesubstatus sp_reinitmergesubscription sp_check_for_sync_trigger sp_reinitsubscription sp_column_privileges sp_special_columns sp_column_privileges_ex sp_serveroption sp_columns sp_sproc_columns
syn keyword sqlFunction sp_columns_ex sp_table_privileges_ex sp_configure sp_lock sp_copymergesnapshot sp_removedbreplication sp_copysnapshot sp_repladdcolumn sp_copysubscription sp_replcmds sp_create_log_shipping_monitor_account sp_update_log_shipping_plan
syn keyword sqlFunction sp_create_removable sp_monitor sp_createstats sp_procoption sp_cursor_list sp_describe_cursor_columns sp_cycle_errorlog sp_recompile sp_databases  sp_statistics sp_datatype_info sp_refreshview sp_dbcmptlevel sp_releaseapplock sp_dbfixedrolepermission sp_helpsrvrolemember sp_dboption sp_rename sp_dbremove sp_renamedb sp_defaultdb sp_helpuser
syn keyword sqlFunction sp_defaultlanguage sp_MShasdbaccess sp_define_log_shipping_monitor sp_update_log_shipping_plan_database sp_delete_alert  sp_post_msx_operation sp_delete_backuphistory sp_resetstatus sp_delete_category sp_purgehistory sp_delete_job sp_purge_jobhistory sp_delete_jobschedule sp_reassigntask sp_delete_jobserver sp_remove_job_from_targets sp_delete_jobstep sp_resync_targetserver
syn keyword sqlFunction sp_delete_maintenance_plan sp_delete_notification  sp_start_job sp_delete_operator sp_stop_job sp_delete_targetserver sp_update_alert sp_delete_targetservergroup sp_update_category sp_delete_targetsvrgrp_member sp_update_job sp_deletemergeconflictrow sp_replcounters sp_denylogin sp_password sp_depends sp_serveroption sp_describe_cursor sp_describe_cursor_tables
syn keyword sqlFunction sp_detach_db sp_setnetname sp_disableagentoffload sp_repldone sp_drop_agent_parameter sp_repldropcolumn sp_drop_agent_profile sp_replflush sp_dropalias sp_remoteoption sp_dropanonymouseagent sp_replication_agent_checkup sp_dropapprole sp_revokedbaccess sp_droparticle sp_replicationdboption sp_dropdevice sp_settriggerorder sp_dropdistpublisher sp_replqueuemonitor sp_dropdistributiondb sp_replsetoriginator sp_dropdistributor sp_repltrans
syn keyword sqlFunction sp_dropextendedproc sp_spaceused sp_dropextendedproperty sp_tableoption sp_dropgroup sp_revokelogin sp_droplinkedsrvlogin sp_setapprole sp_droplinkedsrvlogin sp_tables_ex sp_droplogin sp_srvrolepermission sp_dropmergealternatepublisher sp_replshowcmds sp_dropmergearticle sp_restoredbreplication
syn keyword sqlFunction sp_dropmergefilter sp_resyncmergesubscription sp_dropmergepublication sp_scriptsubconflicttable sp_dropmergepullsubscription sp_script_synctran_commands sp_dropmergesubscription sp_showrowreplicainfo sp_dropmessage sp_unbindefault sp_droppublication sp_subscription_cleanup sp_droppullsubscription sp_table_validation sp_dropremotelogin sp_validatelogins sp_droprole sp_dropsubscriber sp_update_agent_profile
syn keyword sqlFunction sp_dropsubscription sp_validatemergepublication sp_droptask sp_update_jobschedule sp_droptype sp_unbindrule sp_dropwebtask sp_makewebtask sp_dsninfo sp_validatemergesubscription sp_dumpparamcmd sp_vupgrade_replication sp_enumcodepages sp_runwebtask sp_executesql sp_updateextendedproperty sp_fkeys  sp_stored_procedures
syn keyword sqlFunction sp_foreignkeys sp_fulltext_catalog sp_help_fulltext_catalogs_cursor sp_fulltext_column sp_help_fulltext_columns sp_fulltext_database sp_help_fulltext_columns_cursor sp_fulltext_service sp_help_fulltext_tables sp_fulltext_table sp_help_fulltext_tables_cursor sp_getapplock sp_updatestats sp_getbindtoken sp_validname
syn keyword sqlFunction sp_help sp_who sp_help_alert sp_update_jobstep sp_help_category sp_update_notification sp_help_downloadlist sp_update_operator sp_help_fulltext_catalogs sp_help_job sp_updatetask sp_helphistory sp_update_targetservergroup sp_OACreate sp_OAMethod sp_OADestroy sp_OASetProperty sp_OAGetErrorInfo sp_OAStop
syn keyword sqlFunction sp_OAGetProperty Object Hierarchy Syntax sp_pkeys sp_table_privileges sp_processmail xp_sendmail sp_revoke_publication_access sp_server_info  sp_tables sp_setreplfailovermode sp_trace_create sp_trace_setfilter sp_trace_generateevent sp_trace_setstatus sp_trace_setevent sp_xml_preparedocument sp_xml_removedocument xp_cmdshell  xp_logininfo
syn keyword sqlFunction xp_deletemail xp_startmail xp_enumgroups xp_msver xp_findnextmsg  xp_revokelogin xp_findnextmsg xp_stopmail xp_grantlogin xp_sprintf xp_logevent xp_sqlmaint xp_loginconfig xp_sscanf xp_readmail xp_sqlagent_proxy_accountsyn keyword sqlfunction

syn keyword sqlOption CONNECTIONS CPU_BUSY CURSOR_ROWS DATEFIRST
syn keyword sqlOption DBTS ERROR FETCH_STATUS IDENTIFY IDLE IO_BUSY
syn keyword sqlOption LANGID LANGUAGE LOCK_TIMEOUT MAX_CONNECTIONS
syn keyword sqlOption MAX_PRECISION NESTLEVEL OPTIONS PACK_RECEIVED
syn keyword sqlOption PACK_SENT PACKET_ERRORS PROCID REMSERVER
syn keyword sqlOption ROWCOUNT SERVERNAME SERVICENAME SPID TEXTSIZE
syn keyword sqlOption TIMETICKS TOTAL_ERRORS TOTAL_READ TOTAL_WRITE TRANCOUNT VERSION

syn match sqlType "\<[A-Z]\w\+_Type\>"
syn match sqlIdentifier "@[a-z_]\+\>"

""syn match sqlStatement "\<ALTER \(DATABASE\|FUNCTION\|PROCEDURE\|TABLE\|TRIGGER\|VIEW\)\>"
""syn match sqlStatement "\<BACKUP DATABASE\>"
""syn match sqlStatement "\<BEGIN\( \(DISTRIBUTED\)? TRAN\(SACTION\)?\)?\>"
""syn match sqlStatement "\<BREAK\>"
""syn match sqlStatement "\<BULK INSERT\>"
""syn match sqlStatement "\<CASE\>"
""syn match sqlStatement "\<CLOSE\>"
""syn match sqlStatement "\<COMMIT\( \(TRAN\(SACTION\)?\|WORK\)\)?\>"
""syn match sqlStatement "\<CONTINUE\>"
""syn match sqlStatement "\<CREATE TABLE\>"
""syn match sqlStatement "\<CREATE \(DATABASE\|DEFAULT\|FUNCTION\|INDEX\|PROCEDURE\|RULE\|SCHEMA\|STATISTICS\|TABLE\|TRIGGER\|VIEW\)\>"
""syn match sqlStatement "\<DEALLOCATE\>"
""syn match sqlStatement "\<DECLARE\( CURSOR\)?\>"
""syn match sqlStatement "\<DELETE\>"
""syn match sqlStatement "\<DENY\>"
""syn match sqlStatement "\<DROP \(DATABASE\|DEFAULT\|FUNCTION\|INDEX\|PROCEDURE\|RULE\|SCHEMA\|STATISTICS\|TABLE\|TRIGGER\|VIEW\)\>"
""syn match sqlStatement "\<DUMP\>"
""syn match sqlStatement "\<ELSE\>"
""syn match sqlStatement "\<END\>"
""syn match sqlStatement "\<EXEC\(UTE\)?\>"
""syn match sqlStatement "\<FETCH\>"
""syn match sqlStatement "\<FROM\>"
""syn match sqlStatement "\<GO\>"
""syn match sqlStatement "\<GOTO\>"
""syn match sqlStatement "\<GRANT\>"
""syn match sqlStatement "\<GROUP BY\>"
""syn match sqlStatement "\<IF\>"
""syn match sqlStatement "\<INSERT\( INTO\)\>"
""syn match sqlStatement "\<INTO\>"
""syn match sqlStatement "\<KILL\>"
""syn match sqlStatement "\<LOAD\>"
""syn match sqlStatement "\<OPEN\( GLOBAL\)?\>"
""syn match sqlStatement "\<PRINT\>"
""syn match sqlStatement "\<RECONFIGURE\>"
""syn match sqlStatement "\<RESTORE \(DATABASE\|FILELISTONLY\|HEADERONLY\|LABELONLY\|VERIFYONLY\)\>"
""syn match sqlStatement "\<RETURN\>"
""syn match sqlStatement "\<REVOKE\>"
""syn match sqlStatement "\<ROLLBACK\( \(TRAN\(SACTION\)?\|WORK\)\)?\>"
""syn match sqlStatement "\<SAVE TRAN\(SACTION\)?\>"
""syn match sqlStatement "\<SELECT\>"
""syn match sqlStatement "\<SET\(USER\)?\>"
""syn match sqlStatement "\<SHUTDOWN\>"
""syn match sqlStatement "\<TRUNCATE TABLE\>"
""syn match sqlStatement "\<UNION\>"
""syn match sqlStatement "\<UPDATE\( STATISTICS \)?\>"
""syn match sqlStatement "\<UPDATETEXT\>"
""syn match sqlStatement "\<USE\>"
""syn match sqlStatement "\<WHEN\>"
""syn match sqlStatement "\<WHILE\>"
""syn match sqlStatement "\<WITH\>"
""
" Strings and characters:
syn region sqlString		start=+"+    end=+"+ contains=@Spell
syn region sqlString		start=+'+    end=+'+ contains=@Spell

" Numbers:
syn match sqlNumber		"-\=\<\d*\.\=[0-9_]\>"

" Comments:
syn region sqlDashComment	start=/--/ end=/$/ contains=@Spell
syn region sqlSlashComment	start=/\/\// end=/$/ contains=@Spell
syn region sqlMultiComment	start="/\*" end="\*/" contains=sqlMultiComment,@Spell
syn cluster sqlComment	contains=sqlDashComment,sqlSlashComment,sqlMultiComment,@Spell
syn sync ccomment sqlComment
syn sync ccomment sqlDashComment
syn sync ccomment sqlSlashComment

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_sql_syn_inits")
    if version < 508
        let did_sql_syn_inits = 1
        command -nargs=+ HiLink hi link <args>
    else
        command -nargs=+ HiLink hi def link <args>
    endif

    HiLink sqlDashComment	Comment
    HiLink sqlSlashComment	Comment
    HiLink sqlMultiComment	Comment
    HiLink sqlNumber	        Number
    HiLink sqlOperator	        Operator
    HiLink sqlSpecial	        Special
    HiLink sqlKeyword	        Keyword
    HiLink sqlStatement	        Statement
    HiLink sqlString	        String
    HiLink sqlType	        Type
    HiLink sqlFunction	        Function
    HiLink sqlOption	        PreProc
    HiLink sqlIdentifier        Special

    delcommand HiLink
endif

let b:current_syntax = "sql"

" vim:sw=4
