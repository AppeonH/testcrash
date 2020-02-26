$PBExportHeader$w_taetigkeiten_basis.srw
$PBExportComments$Basisfenster Tätigkeitenerfassung
forward
global type w_taetigkeiten_basis from window
end type
type dw_zeitdaten from datawindow within w_taetigkeiten_basis
end type
type dw_info from datawindow within w_taetigkeiten_basis
end type
type ds_schluessel from datastore within w_taetigkeiten_basis
end type
end forward

shared variables

end variables

global type w_taetigkeiten_basis from window
integer x = 2002
integer y = 500
integer width = 3118
integer height = 2668
boolean titlebar = true
string title = "Tätigkeiten"
string menuname = "m_taetigkeiten_fb"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 29347791
string icon = "AppIcon!"
event dfscl_print ( )
event dfscl_ws_saldo ( decimal adc_saldo )
event dfscl_ws_error ( string as_text )
event dfscl_ws_daten ( blob ablob_result )
dw_zeitdaten dw_zeitdaten
dw_info dw_info
ds_schluessel ds_schluessel
end type
global w_taetigkeiten_basis w_taetigkeiten_basis

type prototypes

Function longlong GetTickCount() Library "kernel32.dll" Alias for "GetTickCount64"

end prototypes

type variables

protected string is_username    // kann abweichend von gs_username sein!!!

protected string is_schl_values

end variables

forward prototypes
protected function integer of_setlastdate (date datum)
protected function integer of_setlastkst (string as_kst)
public function string of_getlastkst ()
public function date of_getlastdate ()
public function integer of_getzeitraum (ref date adt_von, ref date adt_bis)
protected function string of_remove_keyvalue (string as_values, string as_key)
protected subroutine of_modify_schluessel (date adt_tag, datawindow adw_single)
public function integer of_load_zeitdaten (date adt_datum)
end prototypes

event dfscl_ws_saldo(decimal adc_saldo);
// Ergebnis des Zeitdaten-Saldos

IF dw_info.RowCount () > 0 THEN
	dw_info.SetItem (1, "saldo", adc_saldo)
END IF

end event

event dfscl_ws_error(string as_text);
// Fehler ignorieren

end event

event dfscl_ws_daten(blob ablob_result);
TRY
	dw_zeitdaten.Reset()
	dw_zeitdaten.SetFullState (ablob_result)
	dw_zeitdaten.Event ue_show (TRUE)
CATCH (throwable e)
	// ignore
END TRY

end event

protected function integer of_setlastdate (date datum);
IF gs_username = is_username THEN
	return RegistrySet("HKEY_CURRENT_USER\Software\tab\taetigkeiten", "lastDate", RegString!, string(datum, "dd.mm.yyyy"))
END IF

return 0

end function

protected function integer of_setlastkst (string as_kst);
IF gs_username = is_username THEN
	return RegistrySet("HKEY_CURRENT_USER\Software\tab\taetigkeiten", "lastKST", RegString!, as_kst)
END IF

return 0


end function

public function string of_getlastkst ();
string ls_kst

IF gs_username = is_username THEN
	RegistryGet ("HKEY_CURRENT_USER\Software\tab\taetigkeiten", "lastKST", RegString!, ls_kst)
END IF

return ls_kst

end function

public function date of_getlastdate ();
string ls_date

IF gs_username = is_username THEN
	RegistryGet ("HKEY_CURRENT_USER\Software\tab\taetigkeiten", "lastDate", RegString!, ls_date)
END IF

return date (ls_date)

end function

public function integer of_getzeitraum (ref date adt_von, ref date adt_bis);
return -1

end function

protected function string of_remove_keyvalue (string as_values, string as_key);
long ll_pose, ll_poss, ll_temp

// aus den Wertepaaren in as_values den Schlüssel as_key entfernen
ll_pose = Pos (as_values, "~t" + as_key)
IF ll_pose > 0 THEN
	ll_temp = 0
	DO
		ll_poss = Pos (Left (as_values, ll_pose), "/", ll_temp + 1)
		IF ll_poss > 0 THEN
			ll_temp = ll_poss
		ELSE
			ll_poss = ll_temp
			EXIT
		END IF
	LOOP WHILE TRUE
	
	as_values = Left (as_values, ll_poss - 1) + Mid (as_values, ll_pose + 2)
	IF Left (as_values, 1) = "/" THEN as_values = Mid (as_values, 2)
END IF

return as_values

end function

protected subroutine of_modify_schluessel (date adt_tag, datawindow adw_single);
// passt die mögliche Schlüsselauswahl an den gewählten Tag an

long ll_row, ll_rows, ll_i, ll_upper, ll_pos
string ls_values, ls_schluessel, ls_schl_remove, ls_v[], ls_null
boolean lb_schluessel_int
n_cst_string lnv_string


SetNull (ls_null)

IF IsNull (adt_tag) THEN
	ds_schluessel.SetFilter ("1=2")
ELSE
	ds_schluessel.SetFilter ("(date (von) <= " + string (adt_tag, "yyyy-mm-dd") + &
									") and (IsNull(bis) or date (bis) >= " + &
									string (adt_tag, "yyyy-mm-dd") + ")")
END IF
ds_schluessel.Filter () 
ll_rows = ds_schluessel.RowCount()

IF adw_single.RowCount () > 0 THEN
	ls_schluessel = adw_single.GetItemString (1, "schluessel")
END IF

// haben wir eine Spalte für den internen Schlüssel?
ls_values = adw_single.Describe ("schluessel_int.id")
lb_schluessel_int = NOT (ls_values = "?" OR ls_values = "!")


// wir gehen durch alle Schlüssel im String und entfernen die, die nicht im DS sind
ls_values = is_schl_values

ll_upper = lnv_string.of_parsetoarray (is_schl_values, "/", ls_v[])
FOR ll_i = 1 TO ll_upper
	// den Key aus dem Werte-Paar holen
	ll_pos = Pos (ls_v[ll_i], "~t")
	IF ll_pos > 0 THEN
		ls_schl_remove = Mid (ls_v[ll_i], ll_pos + 1)
		ll_row = ds_schluessel.Find ("schluessel='" + ls_schl_remove + "'", 1, ll_rows)
		IF ll_row < 1 THEN
			// Schlüssel entfernen; ggf. Auswahl entfernen (auf NULL setzen)
			IF ls_schl_remove = ls_schluessel THEN
				adw_single.SetItem (1, "schluessel", ls_null)
				IF lb_schluessel_int THEN adw_single.SetItem (1, "schluessel_int", ls_null)
			END IF
			
			ls_values = of_remove_keyvalue (ls_values, ls_schl_remove)
		END IF
	END IF
NEXT

IF lb_schluessel_int THEN adw_single.Modify ("schluessel_int.values='" + ls_values + "'")
adw_single.Modify ("schluessel.values='" + ls_values + "'")

end subroutine

public function integer of_load_zeitdaten (date adt_datum);
return 1
end function

on w_taetigkeiten_basis.create
if this.MenuName = "m_taetigkeiten_fb" then this.MenuID = create m_taetigkeiten_fb
this.dw_zeitdaten=create dw_zeitdaten
this.dw_info=create dw_info
this.ds_schluessel=create ds_schluessel
this.Control[]={this.dw_zeitdaten,&
this.dw_info,&
this.ds_schluessel}
end on

on w_taetigkeiten_basis.destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_zeitdaten)
destroy(this.dw_info)
destroy(this.ds_schluessel)
end on

event open;


ds_schluessel.Retrieve (gs_modus)

// Initialisierung: Username des aktuellen Users
is_username = gs_username

// wurde per Parameter ein anderer Username mitgegeben?
IF Message.StringParm <> "" AND NOT IsNull (Message.StringParm) THEN
	is_username = Message.StringParm
	this.title += " für " + is_username
	this.backcolor = rgb (255, 0, 0)
END IF

end event

type dw_zeitdaten from datawindow within w_taetigkeiten_basis
event ue_show ( boolean ab_show )
boolean visible = false
integer x = 1842
integer y = 120
integer width = 1225
integer height = 2352
integer taborder = 20
string title = "none"
string dataobject = "d_zeitdaten"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event ue_show(boolean ab_show);
this.visible = ab_show

end event

type dw_info from datawindow within w_taetigkeiten_basis
integer x = 9
integer y = 4
integer width = 3063
integer height = 104
integer taborder = 10
string title = "none"
string dataobject = "d_tk_info"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event constructor;
datetime ldtm_start
string ls_vbs, ls_return
n_cst_datetime lnv_datetime
OLEObject lole_wsh


SetNull (ldtm_start)

// Versuch der Auswertung des EventLogs, da GetTickCount die Zeit seit dem letzten Re-Boot zählt. Das Aufwachen aus Hibernate zählt da nicht.
// Ausführen von VBS aus Powerbuilder heraus: siehe https://www.rgagnon.com/pbdetails/pb-0252.html
TRY
	lole_wsh = CREATE OLEObject
	lole_wsh.ConnectToNewObject("MSScriptControl.ScriptControl")
	lole_wsh.Language = "vbscript"
	ls_vbs = 'Function getTodayMinWindowsStart()~r~n'
	ls_vbs += 'Set objWMIService = GetObject("winmgmts:{impersonationLevel=impersonate}!\\.\root\cimv2")~r~n'
	ls_vbs += 'Set colLoggedEvents = objWMIService.ExecQuery ("Select * from Win32_NTLogEvent Where Logfile = ~'System~' '
	ls_vbs += 'and ((EventCode=27 and SourceName=~'Microsoft-Windows-Kernel-Boot~')  or (EventCode=12 and SourceName=~'Microsoft-Windows-Kernel-General~')) '
	ls_vbs += 'and TimeWritten>=~'' + string (today(), "YYYYMMDD") + '000000.000000-000~'")~r~n'
	ls_vbs += 'mintime = "29990000000000.000000-000"~r~n'
	ls_vbs += 'For Each objEvent in colLoggedEvents~r~n'
	ls_vbs += '	if (objEvent.TimeWritten < mintime) then~r~n'
	ls_vbs += '		mintime = objEvent.TimeWritten~r~n'
	ls_vbs += '	end if~r~n'
	ls_vbs += 'Next~r~n'
	ls_vbs += 'getTodayMinWindowsStart = mintime~r~n'
	ls_vbs += 'End Function'
	 
	lole_wsh.AddCode(ls_vbs)
	ls_return = lole_wsh.Eval("getTodayMinWindowsStart")
CATCH (throwable e)
	ls_return = ""
FINALLY
	lole_wsh.DisconnectObject()
	DESTROY lole_wsh
END TRY

IF IsNull (ls_return) OR Trim (ls_return) = "" OR Left (ls_return, 4) = "2999" THEN
	// irgendetwas lief falsch
ELSE
	ldtm_start = DateTime (Date (string (left (ls_return, 8), "@@@@-@@-@@")), Time (string (mid (ls_return, 9, 6), "@@:@@:@@")))
	ldtm_start = lnv_datetime.of_utc2local(ldtm_start)
END IF



IF IsNull (ldtm_start) THEN
	// alternativ: letzter Windows-Start (unter Windows 10 zählt der letzte "richtige" Start; das Aufwachen aus dem Ruhemodus zählt NICHT als Start
	ldtm_start = lnv_datetime.of_RelativeDateTime (DateTime (Today(), Now()), -GetTickCount() / 1000)
END IF

InsertRow (0)
SetItem (1, "starttime", ldtm_start)

end event

type ds_schluessel from datastore within w_taetigkeiten_basis descriptor "pb_nvo" = "true" 
string dataobject = "d_schluessel_gueltig"
end type

on ds_schluessel.create
call super::create
TriggerEvent( this, "constructor" )
end on

on ds_schluessel.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

event constructor;setTransObject(sqlca)

end event

