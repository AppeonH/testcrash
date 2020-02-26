$PBExportHeader$n_taetigkeiten_connectservice.sru
$PBExportComments$DB Connection Service
forward
global type n_taetigkeiten_connectservice from nonvisualobject
end type
end forward

global type n_taetigkeiten_connectservice from nonvisualobject
end type
global n_taetigkeiten_connectservice n_taetigkeiten_connectservice

type variables

end variables

forward prototypes
public function integer of_connectdb ()
public function integer of_disconnectdb ()
public function integer of_getconnectioninfo (ref string as_dbms, ref string as_database, ref string as_userid, ref string as_dbpass, ref string as_logid, ref string as_logpass, ref string as_server, ref string as_dbparm, ref string as_lock, ref string as_autocommit)
end prototypes

public function integer of_connectdb ();//*-----------------------------------------------------------------*/
//*    of_ConnectDB:  Make a connection to the database
//*-----------------------------------------------------------------*/
/*  Actual DB connection */

integer li_rc
string ls_dbms, ls_database, ls_userid, ls_dbpass, ls_logid, ls_logpass
string ls_server, ls_dbparm, ls_lock, ls_autocommit

If of_GetConnectionInfo ( ls_dbms, ls_database, ls_userid, ls_dbpass, ls_logid, ls_logpass, ls_server, ls_dbparm, ls_lock, ls_autocommit ) = 1 Then
	
	SQLCA.DBMS			= ls_dbms
	SQLCA.Database		= ls_database
	SQLCA.UserID		= ls_userid
	SQLCA.DBPass		= ls_dbpass
	SQLCA.LogID			= ls_logid
	SQLCA.LogPass		= ls_logpass
	SQLCA.ServerName	= ls_server
	SQLCA.DBParm		= ls_dbparm
	SQLCA.Lock			= ls_lock
	Choose Case Lower ( ls_autocommit ) 
		Case "1", "true", "on", "yes"
			SQLCA.AutoCommit	= True
		Case "0", "false", "off", "no"
			SQLCA.AutoCommit	= False
		Case Else
			SQLCA.AutoCommit	= False
	End Choose
	
End If


Connect using SQLCA;

If SQLCA.SQLCode <> 0 Then
	MessageBox ("Cannot Connect to Database", SQLCA.SQLErrText )
End If

li_rc = SQLCA.SQLCode

// Modulnamen der Datenbank bekannt machen
DECLARE set_module PROCEDURE FOR dbms_application_info.set_module ('Zeiterfassung', '') USING SQLCA;
EXECUTE set_module;

Return li_rc
end function

public function integer of_disconnectdb ();//*-----------------------------------------------------------------*/
//*    of_DisconnectDB:  Disconnect from the database
//*-----------------------------------------------------------------*/
Disconnect using SQLCA;

/*If SQLCA.SQLCode <> 0 Then
	MessageBox ("Cannot Disconnect to Database", SQLCA.SQLErrText )
End If*/

Return SQLCA.SQLCode
end function

public function integer of_getconnectioninfo (ref string as_dbms, ref string as_database, ref string as_userid, ref string as_dbpass, ref string as_logid, ref string as_logpass, ref string as_server, ref string as_dbparm, ref string as_lock, ref string as_autocommit);
string ls_registrykey, ls_temp
string ls_lastdate, ls_kz


// Zusatzinformationen ermitteln
ls_registrykey = "HKEY_CURRENT_USER\Software\tab\taetigkeiten"
If RegistryGet ( ls_registrykey, "lastDate",		RegString!, ls_lastdate) <> 1 Then
	RegistrySet ( ls_registrykey, "lastDate",		RegString!, string(today(), "dd.mm.yyyy") )
	RegistryGet ( ls_registrykey, "lastDate", 	RegString!, ls_lastdate)
End If

If RegistryGet ( ls_registrykey, "kz_zeit",		RegString!, ls_kz) <> 1 Then
	RegistrySet ( ls_registrykey, "kz_zeit",		RegString!, "m")
	RegistryGet ( ls_registrykey, "kz_zeit",	 	RegString!, ls_kz)
End If


as_dbms = ProfileString (gs_inifile, "Datenbank", 'DBMS', '')
as_Database = ProfileString (gs_inifile, "Datenbank", 'Database', '')
as_LogID = ProfileString (gs_inifile, "Datenbank", 'LogID', '')
as_Server = ProfileString (gs_inifile, "Datenbank", 'ServerName', '')
as_UserID = ProfileString (gs_inifile, "Datenbank", 'UserID', '')
as_DBPass = ProfileString (gs_inifile, "Datenbank", 'DatabasePassword', '')
as_Lock = ProfileString (gs_inifile, "Datenbank", 'Lock', '')
as_DBParm = ProfileString (gs_inifile, "Datenbank", 'DBParm', '')
as_AutoCommit = ProfileString (gs_inifile, "Datenbank", 'AutoCommit', 'false')

as_LogPass = ProfileString (gs_inifile, "Datenbank", 'LogPassword', '')


Return 1

end function

on n_taetigkeiten_connectservice.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_taetigkeiten_connectservice.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

