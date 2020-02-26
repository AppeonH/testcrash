$PBExportHeader$taetigkeiten.sra
$PBExportComments$Application Object
forward
global type taetigkeiten from application
end type
global transaction sqlca
global dynamicdescriptionarea sqlda
global dynamicstagingarea sqlsa
global error error
global message message
end forward

global variables

string gs_username
string gs_modus
string gs_persnr
constant string gs_inifile = "taetigkeiten.ini"


end variables

global type taetigkeiten from application
string appname = "taetigkeiten"
string microhelpdefault = "Bereit"
string displayname = "Zeiterfassung"
string themepath = "C:\Program Files (x86)\Appeon\Shared\PowerBuilder\theme190"
string themename = "Do Not Use Themes"
long richtextedittype = 0
long richtexteditversion = 0
string richtexteditkey = ""
end type
global taetigkeiten taetigkeiten

forward prototypes
public function boolean of_register (string path, string value)
protected function string of_readusername ()
protected function string of_readpersnr ()
end prototypes

public function boolean of_register (string path, string value);boolean lb_da
string ls_subkeylist[]
long ll_i, ll_arraysize

// steht Datenbank in der Registry?
if RegistryKeys(path, ls_subkeylist) = -1 then
	messagebox("Fehler", "Registry konnte nicht ausgelesen werden")
end if

lb_da = false
ll_arraysize = UpperBound(ls_subkeylist)
FOR ll_i=1 TO ll_arraysize
	if ls_subkeylist[ll_i] = value then
		lb_da = true
		exit
	end if
NEXT

return lb_da
end function

protected function string of_readusername ();
integer		li_rc
OLEObject	lole_winntsysteminfo
string ls_username

//ContextKeyword lcxk_base
//string ls_values[]
//
//GetApplication().GetContextService ("Keyword", lcxk_base)
//lcxk_base.GetContextKeywords ("NWUSERNAME", ls_values)
//IF Upperbound (ls_values) > 0 THEN
//   ls_username = ls_values[1]
//END IF


// Versuch, Username aus registry zu lesen
RegistryGet ("HKEY_CURRENT_USER\Volatile Environment", "NWUSERNAME", RegString!, ls_username)


// Alternativ: Aus Windows-SystemInfo
IF IsNull (ls_username) OR ls_username = "" THEN
	try
		lole_winntsysteminfo = CREATE OLEObject
		li_rc = lole_winntsysteminfo.ConnectToNewObject ("WinNTSystemInfo")
		IF li_rc = 0 THEN
			ls_username = String (lole_winntsysteminfo.username)
			lole_winntsysteminfo.DisconnectObject ()
		END IF
	catch (throwable e)
		// do nothing
	finally
		Destroy lole_winntsysteminfo
	end try
END IF

IF IsNull (ls_username) OR ls_username = "" THEN
	MessageBox ("Fehler", "Der Username konnte nicht ermittelt werden!~r~nWenden Sie sich bitte an die DV-Technik!")
END IF

return lower (ls_username)

end function

protected function string of_readpersnr ();
integer li_rc
string ls_username, ls_persnr
OLEObject lole_user, lole_sysinfo


TRY
	// Connect to ADSI
	lole_sysinfo = CREATE oleobject
	li_rc = lole_sysinfo.ConnectToNewObject ("ADSystemInfo")		
	IF li_rc = 0 THEN	
		// Username im AD-Format
		ls_username = lole_sysinfo.username	
		
		// Connect to Active Directory
		lole_user = CREATE oleobject
		li_rc = lole_user.ConnectToObject ("LDAP://" + ls_username)	
		IF li_rc = 0 THEN	
			ls_persnr = string (lole_user.extensionAttribute4)
	//		ls_persnr = string (lole_user.employeeID)
		END IF
	END IF
	
CATCH (throwable e)
	// ignore
FINALLY
	IF IsValid (lole_sysinfo) THEN lole_sysinfo.DisconnectObject ()
	IF IsValid (lole_user) THEN lole_user.DisconnectObject ()
	DESTROY lole_sysinfo
	DESTROY lole_user
END TRY

return ls_persnr

end function

on taetigkeiten.create
appname="taetigkeiten"
message=create message
sqlca=create transaction
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create error
end on

on taetigkeiten.destroy
destroy(sqlca)
destroy(sqlda)
destroy(sqlsa)
destroy(error)
destroy(message)
end on

event open;
string ls_tk, ls_modus, ls_window, ls_temp, ls_pass
long ll_pos
n_taetigkeiten_connectservice lnv_connectserv
window lw_main



IF IsNull (commandline) THEN commandline = ""


// Username aus Registry
gs_username = of_readusername ()
IF IsNull (gs_username) OR gs_username = "" THEN
	MessageBox ("Fehler", "Ihr Benutzername konnte nicht ermittelt werden!~r~nBitte wenden Sie sich an die DV-Technik!")
	return
END IF

// Personalnummer aus ActiveDirectory
gs_persnr = of_readpersnr ()


// TEST
//commandline = "/modus=WF1"
//commandline = "/modus=RW"
//commandline = "/modus=FB"
//commandline = "/modus=FM"
//commandline = "/modus=EDV"
//commandline = "/modus=VST"
//commandline = "/modus=IRESF"

// Anwendungsmodus
ll_pos = Pos(lower(commandline), "/modus=")
IF ll_pos > 0 THEN
	ls_modus = Trim (Mid (commandline, ll_pos + 7))
	ll_pos = Pos (ls_modus, " ")
	IF ll_pos = 0 THEN ll_pos = Len (ls_modus) + 1
	gs_modus = Left (ls_modus, ll_pos - 1)
END IF


lnv_connectserv = CREATE n_taetigkeiten_connectservice
If lnv_connectserv.of_ConnectDB ( ) <> 0 Then return
Destroy lnv_connectserv


IF gs_modus = "" THEN
	// Bearbeitet der User die EDV-Liste ?
	select edv_tk
	into :ls_tk
	from intdba.mitarbeiter
	where username = :gs_username
	using sqlca;
	
	IF IsNull (ls_tk) THEN ls_tk = ""
	IF upper (ls_tk) = 'X' THEN
		gs_modus = "EDV"
	ELSE
		gs_modus = "FB"
	END IF
END IF


// nach 30 Minuten ohne Aktion wird die Anwendung geschlossen
Idle (1800)


// Hauptfenster (abhängig vom Modus)
ls_window = "w_taetigkeiten_" + gs_modus
IF IsNull (FindClassDefinition (ls_window)) THEN
	MessageBox ("Achtung", "Fehlerhafter Modus!~r~nBitte informieren Sie die DV-Technik!")
	return
END IF
Open (lw_main, ls_window)

end event

event close;
n_taetigkeiten_connectservice lnv_connectserv

lnv_connectserv = Create n_taetigkeiten_connectservice
lnv_connectserv.of_DisconnectDB ( )
Destroy lnv_connectserv

// kein Errorlevel
Message.LongParm = 0

end event

event idle;
Open (w_idle)

end event

