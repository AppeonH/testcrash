$PBExportHeader$w_taetigkeiten_edv.srw
$PBExportComments$Tätigkeitenerfassung EDV
forward
global type w_taetigkeiten_edv from w_taetigkeiten_basis
end type
type uo_zeitraum from u_auswahl_zeitraum within w_taetigkeiten_edv
end type
type cb_new_tag from commandbutton within w_taetigkeiten_edv
end type
type dw_tk_liste_grid from datawindow within w_taetigkeiten_edv
end type
type cb_wechsel from commandbutton within w_taetigkeiten_edv
end type
type cb_new_umlage from commandbutton within w_taetigkeiten_edv
end type
type cb_delete from commandbutton within w_taetigkeiten_edv
end type
type cb_ok from commandbutton within w_taetigkeiten_edv
end type
type dw_tk_single from datawindow within w_taetigkeiten_edv
end type
type dw_tk_liste_cross from datawindow within w_taetigkeiten_edv
end type
type dw_tk_uhr from datawindow within w_taetigkeiten_edv
end type
end forward

shared variables

end variables

global type w_taetigkeiten_edv from w_taetigkeiten_basis
integer width = 3081
integer height = 2096
string menuname = "m_taetigkeiten_edv"
event dfscl_print ( )
event dfscl_export ( )
uo_zeitraum uo_zeitraum
cb_new_tag cb_new_tag
dw_tk_liste_grid dw_tk_liste_grid
cb_wechsel cb_wechsel
cb_new_umlage cb_new_umlage
cb_delete cb_delete
cb_ok cb_ok
dw_tk_single dw_tk_single
dw_tk_liste_cross dw_tk_liste_cross
dw_tk_uhr dw_tk_uhr
end type
global w_taetigkeiten_edv w_taetigkeiten_edv

type variables

protected boolean ib_readonly = FALSE

end variables

forward prototypes
protected subroutine of_retrieve ()
protected function string of_getkz_zeit ()
protected function integer of_setkz_zeit (string as_data)
protected function integer wf_calc_zeit ()
protected subroutine wf_delete ()
protected function long wf_new_tag ()
protected function long wf_new_umlage ()
protected function long wf_rowchanged (long al_row)
protected function integer wf_update ()
protected function integer wf_update_zeit ()
public function integer of_getzeitraum (ref date adt_von, ref date adt_bis)
protected subroutine wf_setreadonly (boolean ab_readonly)
end prototypes

event dfscl_print();
// Drucken

SetPointer (HourGlass!)

IF dw_tk_liste_cross.visible THEN
	dw_tk_liste_cross.Print ()
ELSE
	dw_tk_liste_grid.Print ()
END IF

end event

event dfscl_export();
// Export

SetPointer (HourGlass!)

IF dw_tk_liste_cross.visible THEN
	dw_tk_liste_cross.SaveAs ("", PDF!, TRUE)
ELSE
	dw_tk_liste_grid.SaveAs ("", PDF!, TRUE)
END IF

end event

protected subroutine of_retrieve ();
string ls_maxpos, ls_minpos
boolean lb_setpos = FALSE
date ld_von, ld_bis


SetPointer (Hourglass!)

IF dw_tk_liste_cross.Describe ("DataWindow.HorizontalScrollSplit") <> "0" THEN
	ls_minpos = dw_tk_liste_cross.Describe ("DataWindow.HorizontalScrollSplit")
	ls_maxpos = dw_tk_liste_cross.Describe ("DataWindow.HorizontalScrollMaximum2")
	lb_setpos = (ls_maxpos = dw_tk_liste_cross.Describe ("DataWindow.HorizontalScrollPosition2"))
END IF

uo_zeitraum.of_GetZeitraum (ld_von, ld_bis)
dw_tk_liste_cross.retrieve (is_username, Datetime (ld_von), Datetime (ld_bis))
dw_tk_liste_grid.retrieve (is_username, Datetime (ld_von), Datetime (ld_bis))

IF lb_setpos THEN
	dw_tk_liste_cross.Modify ("DataWindow.HorizontalScrollSplit=" + ls_minpos)
	ls_maxpos = dw_tk_liste_cross.Describe ("DataWindow.HorizontalScrollMaximum2")
	dw_tk_liste_cross.Modify ("DataWindow.HorizontalScrollPosition2=" + ls_maxpos)
END IF

end subroutine

protected function string of_getkz_zeit ();
string ls_data

IF gs_username = is_username THEN
	RegistryGet ("HKEY_CURRENT_USER\Software\tab\taetigkeiten", "kz_zeit", RegString!, ls_data)
END IF

return ls_data

end function

protected function integer of_setkz_zeit (string as_data);
IF gs_username = is_username THEN
	return RegistrySet ("HKEY_CURRENT_USER\Software\tab\taetigkeiten", "kz_zeit", RegString!, as_data)
END IF
return 0


end function

protected function integer wf_calc_zeit ();string ls_zeit 
integer li_return=1
integer li_pos
long ll_zeit

//Überprüfen der Zeiteingabe
dw_tk_single.AcceptText()
if dw_tk_single.getRow() > 0 then
	ls_zeit=Trim (dw_tk_single.GetItemString(dw_tk_single.getRow(), "zeit_str"))
	if isNull(ls_zeit) OR ls_zeit = "" then //leere Eingabe
		li_return=-1  //Fehler
	else
		li_pos = Pos (ls_zeit, ":")
		if li_pos > 0 then  //Format hh:mm
			//Umrechnen in einen Long-Wert
			if li_pos = 1 or li_pos > len (ls_zeit) - 2 then 
				li_return=-2   //falsche Eingabe
			elseif IsNumber (Left (ls_zeit, li_pos - 1)) and IsNumber (Mid (ls_zeit, li_pos + 1)) then
				ll_zeit = Long (Left (ls_zeit, li_pos - 1)) * 60 + Long (Mid (ls_zeit, li_pos + 1)) 
			else 
				li_return=-2   //falsche Eingabe
			end if
			
		else // Format  mmmmm	
			if NOT IsNumber (ls_zeit) then 
				li_return=-2    //falsche Eingabe
			else
				ll_zeit=Long(ls_zeit)    //Umrechnen in einen Long-Wert
			end if
		end if
		if li_return > 0 then
			if ll_zeit > 1440 AND is_username = gs_username then  //Mehr 24 Stunden (außer bei Erfassung für jemand anderen)
				li_return=-3
			elseif ll_zeit > 99999 AND is_username = gs_username then  //nicht größer als 5 Stellen (Größe der DB-Spalte)
				li_return=-4
			elseif ll_zeit <= 0 then
				li_return=-1
			else 
				dw_tk_single.SetItem(dw_tk_single.getRow(), "zeit", ll_zeit)
			end if		
		end if
	end if
end if

return li_return
end function

protected subroutine wf_delete ();long ll_row
integer li_response
string ls_von
datetime ldtm_tag


if dw_tk_single.rowCount() < 1 then return

// prüfen, ob Datum in geöffneter Periode
ldtm_tag = dw_tk_single.GetItemDateTime (1, "tag")

SELECT wert
INTO :ls_von
FROM intdba.steuerung
WHERE schluessel = 'BUCHPER_AB'
USING sqlca;

IF Date (ldtm_tag) < DATE (ls_von) THEN
	MessageBox (This.Title, "Das angegebene Datum liegt außerhalb der geöffneten Buchungsperiode!~r~n" + &
						"Buchungen vor dem " + ls_von + " sind nicht möglich!~r~n" + &
						"Falls Sie rückwirkend Änderungen vornehmen möchten, wenden Sie sich bitte an das Controlling.", StopSign!)
	return
END IF


li_response = MessageBox (this.Title, "Wirklich löschen?", Question!, YesNo!, 2)
IF li_response = 1 THEN
	ll_row = dw_tk_liste_grid.getRow()
	setRedraw(false)
	
	ldtm_tag = dw_tk_single.getItemDateTime(dw_tk_single.getrow(), "tag")
	dw_tk_single.DeleteRow(0)
	
	IF wf_update() <> 1 THEN return
	
	if ll_row > 0 THEN 
		IF ll_row > dw_tk_liste_grid.RowCount () then ll_row = dw_tk_liste_grid.RowCount ()
		dw_tk_liste_grid.scrollToRow(ll_row)
	END IF
	setRedraw(true)
end if

dw_tk_liste_grid.setFocus()

end subroutine

protected function long wf_new_tag ();long ll_row
string ls_kz

// neue Zeile einfügen
dw_tk_uhr.Reset ()

ll_row = dw_tk_uhr.insertRow(0)
if ll_row > 0 then
	// Vorbelegung: Name, Datum
	dw_tk_uhr.setItem(ll_row, "username", is_username)
	ls_kz = of_getkz_zeit()
	dw_tk_uhr.setItem(ll_row, "zeit_kz", ls_kz)
	dw_tk_uhr.setItemStatus(ll_row, 0, Primary!, NotModified!)
end if

return 1

end function

protected function long wf_new_umlage ();long ll_row, ll_restzeit, ll_tagrow, ll_zeit
datetime ldtm_date
string ls_kst, ls_kst_alt

dw_tk_single.object.t_lang.text = ""

// Tag merken für den Fall, dass es keinen Eintrag in tkuhr gibt
if dw_tk_single.rowCount() > 0 then	
	ldtm_date = dw_tk_single.getItemDateTime(1, "tag")
	ls_kst_alt = dw_tk_single.getItemString(1, "von_kst")
end if

// neue Zeile einfügen
dw_tk_single.reset()
ll_row = dw_tk_single.insertRow(0)
dw_tk_single.setColumn("zeit_str")

if ll_row > 0 then
	dw_tk_single.scrollToRow(ll_row)
	
	if dw_tk_uhr.rowCount() < 1 then 
		// neuen Tag einfügen
		wf_new_tag()
		// Datum setzen
		ll_tagrow = dw_tk_uhr.getRow()
		dw_tk_uhr.setItem(ll_tagrow, "tag", ldtm_date)
		dw_tk_uhr.setItemStatus(ll_tagrow, 0, Primary!, NotModified!)
	else
		ldtm_date = dw_tk_uhr.getItemDateTime(1, "tag")
	end if
	
	// Vorbelegung: Name, Datum, Schlüssel
	dw_tk_single.setItem (ll_row, "username", is_username)
	dw_tk_single.setItem (ll_row, "schluessel", "p")
	dw_tk_single.setItem (ll_row, "schluessel_int", "p")
	
	// Vorbelegung: Kostenstelle
	select orgnr 
	into :ls_kst
	from intdba.mitarbeiter
	where username = :is_username
	using sqlca;
	
	if sqlca.sqlcode <> 0 then
		Messagebox ("Fehler", "Sie sind nicht als Mitarbeiter eingerichtet!~r~nDas Programm wird beendet!")
		HALT CLOSE
	end if
	
	IF IsNull (ls_kst) THEN
		// keine KST vordefiniert -> Auswahl anbieten
		dw_tk_single.Modify ("von_kst.protect='0' von_kst.background.mode=0")
		
		IF ls_kst_alt <> "" AND NOT IsNull (ls_kst_alt) THEN
			ls_kst = ls_kst_alt
		ELSE
			ls_kst = of_getlastkst ()
		END IF
	ELSEIF (NOT IsNumber (ls_kst)) AND ls_kst <> 'EXTERN' THEN
		// keine konkrete KST vordefiniert -> keine Auswahl anbieten
		dw_tk_single.Modify ("von_kst.protect='0' von_kst.background.mode=0")
		
		IF ls_kst_alt <> "" AND NOT IsNull (ls_kst_alt) THEN
			ls_kst = ls_kst_alt
		ELSE
			ls_kst = of_getlastkst ()
		END IF
	ELSE
		// eindeutige KST vordefiniert -> keine Auswahl anbieten
		dw_tk_single.Modify ("von_kst.protect='1' von_kst.background.mode=1")
	END IF
	
	dw_tk_single.setItem (ll_row, "von_kst", ls_kst)
	
	// Tag und Zeit übernehmen
	dw_tk_uhr.acceptText()
	if dw_tk_uhr.rowCount() > 0 then
		dw_tk_single.setItem(ll_row, "tag", ldtm_date)
		
		of_modify_schluessel (Date (ldtm_date), dw_tk_single)
		
		of_load_zeitdaten (Date (ldtm_date))
		
		// Restzeit ermitteln und vorbelegen
		select nvl (sum(zeit), 0)
		into :ll_zeit
		from intdba.tk 
		where username = :is_username
		and tag = :ldtm_date
		using sqlca;
		
		ll_restzeit = dw_tk_uhr.getItemNumber(1, "min") - ll_zeit
		dw_tk_single.setItem(ll_row, "zeit", ll_restzeit)
		dw_tk_single.setItem(ll_row, "zeit_str", string(ll_restzeit))
	end if
	
	dw_tk_single.setItemStatus(ll_row, 0, Primary!, NotModified!)
end if

dw_tk_single.Modify ("t_lang.Text=''")
dw_tk_single.setFocus()

return 1

end function

protected function long wf_rowchanged (long al_row);
long ll_rows, ll_tagrow
datetime ldtm_tag
string ls_user
decimal {0} ldc_id

if dw_tk_liste_grid.visible then
	dw_tk_liste_grid.selectRow (0, false)
	
	if al_row > 0 then
		dw_tk_liste_grid.selectRow(al_row, true)
	
		ldc_id = dw_tk_liste_grid.getItemDecimal(al_row, "id")
		ldtm_tag = dw_tk_liste_grid.getItemDateTime(al_row, "tag")
		ls_user = dw_tk_liste_grid.getItemString(al_row, "username")

		of_load_zeitdaten (Date (ldtm_tag))		

		ll_rows = dw_tk_single.retrieve (ldc_id)
		ll_rows = dw_tk_uhr.retrieve (ldtm_tag, ls_user)
		
		if ll_rows < 1 then
			// neuen Tag einfügen
			wf_new_tag()
			// Datum setzen
			ll_tagrow = dw_tk_uhr.getRow()
			dw_tk_uhr.setItem(ll_tagrow, "tag", ldtm_tag)
			dw_tk_uhr.setItemStatus(ll_tagrow, 0, Primary!, NotModified!)
		end if
		of_modify_schluessel (Date (ldtm_tag), dw_tk_single)
	end if	
	dw_tk_single.object.t_lang.text = ''
	dw_tk_liste_grid.setFocus()
end if

return 1

end function

protected function integer wf_update ();date ld_tag, ld_tag_orig
date ld_von, ld_bis
datetime ldtm_tag
long ll_count
decimal {0} ldc_id
string ls_von, ls_schluessel, ls_auftrag, ls_umlage, ls_temp

IF dw_tk_single.AcceptText() = 1 and dw_tk_uhr.AcceptText() = 1 THEN
	IF dw_tk_single.RowCount () > 0 THEN
		// prüfen, ob Datum in geöffneter Periode
		ld_tag = Date (dw_tk_single.GetItemDateTime (1, "tag"))
		ld_tag_orig = Date (dw_tk_single.GetItemDateTime (1, "tag", Primary!, TRUE))
		
		IF ld_tag > Today() THEN
			MessageBox (This.Title, "Das angegebene Datum liegt in der Zukunft!", StopSign!)
			return -1
		END IF
		
		SELECT wert
		INTO :ls_von
		FROM intdba.steuerung
		WHERE schluessel = 'BUCHPER_AB'
		USING sqlca;
	
		IF ld_tag < DATE (ls_von) OR ld_tag_orig < DATE (ls_von) THEN
			MessageBox (This.Title, "Das angegebene Datum liegt außerhalb der geöffneten Buchungsperiode!~r~n" + &
								"Buchungen vor dem " + ls_von + " sind nicht möglich!~r~n" + &
								"Falls Sie rückwirkend Änderungen vornehmen möchten, wenden Sie sich bitte an das Controlling.", StopSign!)
			return -1
		END IF
		
		// Zusatz darf nicht NULL sein
		ls_temp = dw_tk_single.GetItemString (1, "zusatz")
		IF IsNull (ls_temp) OR ls_temp = "" THEN
			dw_tk_single.SetItem (1, "zusatz", " ")
		END IF
		
		// prüfen, dass ausgewählte Umlage auch an dem Tag gültig ist
		ls_schluessel = dw_tk_single.GetItemString (1, "schluessel")
		ldtm_tag = DateTime (ld_tag)
		ls_umlage = dw_tk_single.GetItemString (1, "u_p")
		
		IF ls_schluessel = 't' THEN
			SELECT count (*)
			INTO :ll_count
			FROM intdba.ts_umlage
			WHERE umlage = :ls_umlage
			  AND :ldtm_tag BETWEEN von AND NVL (bis, TO_DATE ('2900-01-01', 'YYYY-MM-DD'))
			USING sqlca;
		ELSE
			SELECT count (*)
			INTO :ll_count
			FROM intdba.umlage
			WHERE schluessel = :ls_schluessel
			  AND umlage = :ls_umlage
			  AND :ldtm_tag BETWEEN von AND NVL (bis, TO_DATE ('2900-01-01', 'YYYY-MM-DD'))
			USING sqlca;
		END IF
		
		IF ll_count < 1 THEN
			ls_auftrag = dw_tk_single.GetItemString (1, "kz_auftrag")
			IF ls_auftrag = 'X' AND NOT IsNull (ls_auftrag) THEN
				MessageBox (This.Title, "Für den ausgewählten Auftrag ist eine Umlage erfasst, die am Abrechnungstag nicht mehr gültig ist!~r~n" + &
									"Ändern Sie für den Auftrag die Umlage, bevor Sie Abrechnungen vornehmen!", StopSign!)
			ELSEIF ls_auftrag = 'J' AND NOT IsNull (ls_auftrag) THEN
				MessageBox (This.Title, "Für das ausgewählte JIRA-Ticket ist eine Umlage erfasst, die am Abrechnungstag nicht mehr gültig ist!~r~n" + &
									"Ändern Sie für das Ticket die Umlage, bevor Sie Abrechnungen vornehmen!", StopSign!)
			ELSE
				MessageBox (This.Title, "Die ausgewählte Umlage ist am Abrechnungstag nicht gültig!~r~n" + &
									"Wählen Sie eine andere Umlage!", StopSign!)
			END IF
			return -1
		END IF
		
		// prüfen, dass ausgewählte eigene KST auch an dem Tag gültig ist
		// (nicht für Dummy-KST EXTERN, denn die gibt es nicht)
		ls_umlage = dw_tk_single.GetItemString (1, "von_kst")
		IF ls_umlage <> "EXTERN" THEN
			SELECT count (*)
			INTO :ll_count
			FROM intdba.umlage
			WHERE schluessel = 'k'
			  AND umlage = :ls_umlage
			  AND :ldtm_tag BETWEEN von AND NVL (bis, TO_DATE ('2900-01-01', 'YYYY-MM-DD'))
			USING sqlca;
			
			IF ll_count < 1 THEN
				MessageBox (This.Title, "Ihre Kostenstelle ist am Abrechnungstag nicht gültig!", StopSign!)
				return -1
			END IF
		END IF
		
		IF IsNull (dw_tk_single.GetItemNumber (1, "id")) THEN
			select intdba.seq_tk_id.nextval
			into :ldc_id
			from dual
			using sqlca;
			IF SQLCA.SQLCode <> 0 THEN
				MessageBox(This.Title, "Ermitteln der ID fehlgeschlagen.  Code: " + &
					String(SQLCA.SQLCode) + " - " + SQLCA.SQLErrText + ".", &
					StopSign!)
				ROLLBACK USING SQLCA;
				return -1
			END IF	
			dw_tk_single.SetItem (1, "id", ldc_id)
		END IF
	END IF

	// speichern
	IF dw_tk_single.Update() = 1 and wf_update_zeit() = 1 THEN	
		COMMIT USING SQLCA;

		IF SQLCA.SQLCode <> 0 THEN
			MessageBox(This.Title,"Die Daten konnten nicht gespeichert werden.  Code: " + &
				String(SQLCA.SQLCode) + " - " + SQLCA.SQLErrText + ".", &
				StopSign!)
			ROLLBACK USING SQLCA;
		END IF	
		
		if dw_tk_uhr.rowCount() > 0 then 
			of_setkz_zeit(dw_tk_uhr.getItemString(1, "zeit_kz"))
			of_setlastdate(Date (dw_tk_uhr.getItemDateTime(1, "tag")))
			ld_tag = Date (dw_tk_uhr.getItemDateTime(1, "tag"))
			
			uo_zeitraum.of_GetZeitraum (ld_von, ld_bis)
			if ld_tag < ld_von or ld_tag > ld_bis then
				messagebox("Hinweis", "Der " + string(ld_tag, "dd.mm.yyyy") + " liegt nicht im gewählten Zeitfenster vom " + &
				string(ld_von, "dd.mm.yyyy") + " bis " + string(ld_bis, "dd.mm.yyyy") + "~n" + &
				"Um den gerade gespeicherten Eintrag anzuzeigen, wählen Sie einen geeigneten Zeitraum.")
			end if
		end if
		if dw_tk_single.rowCount() > 0 then 
			of_setlastkst (dw_tk_single.getItemString(1, "von_kst"))
		end if
		
		of_retrieve ()
		
	ELSE
		ROLLBACK USING SQLCA;
		return -1
	END IF	
ELSE
	dw_tk_single.SetFocus()
	return -1
END IF		

return 1
end function

protected function integer wf_update_zeit ();
IF dw_tk_uhr.AcceptText() = 1 THEN

	if dw_tk_uhr.getItemString(dw_tk_uhr.getrow(), "zeit_kz") = 'm' or &
			ib_readonly then
		dw_tk_uhr.setItemStatus(dw_tk_uhr.getrow(), 0, Primary!, NotModified!)
	end if

	IF dw_tk_uhr.Update() = 1 THEN
		COMMIT USING SQLCA;

		IF SQLCA.SQLCode <> 0 THEN
			MessageBox(This.Title,"Die Daten konnten nicht gespeichert werden.  Code: " + &
				String(SQLCA.SQLCode) + " - " + SQLCA.SQLErrText + ".", &
				StopSign!)
			ROLLBACK USING SQLCA;
		END IF	
		
	ELSE
		ROLLBACK USING SQLCA;
	END IF	
END IF

return 1
end function

public function integer of_getzeitraum (ref date adt_von, ref date adt_bis);
uo_zeitraum.of_GetZeitraum (adt_von, adt_bis)

return 1

end function

protected subroutine wf_setreadonly (boolean ab_readonly);
ib_readonly = ab_readonly

IF ib_readonly THEN
	dw_tk_uhr.Modify("kz_readonly.Expression='1'")
ELSE
	dw_tk_uhr.Modify("kz_readonly.Expression='0'")
END IF

end subroutine

on w_taetigkeiten_edv.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_taetigkeiten_edv" then this.MenuID = create m_taetigkeiten_edv
this.uo_zeitraum=create uo_zeitraum
this.cb_new_tag=create cb_new_tag
this.dw_tk_liste_grid=create dw_tk_liste_grid
this.cb_wechsel=create cb_wechsel
this.cb_new_umlage=create cb_new_umlage
this.cb_delete=create cb_delete
this.cb_ok=create cb_ok
this.dw_tk_single=create dw_tk_single
this.dw_tk_liste_cross=create dw_tk_liste_cross
this.dw_tk_uhr=create dw_tk_uhr
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_zeitraum
this.Control[iCurrent+2]=this.cb_new_tag
this.Control[iCurrent+3]=this.dw_tk_liste_grid
this.Control[iCurrent+4]=this.cb_wechsel
this.Control[iCurrent+5]=this.cb_new_umlage
this.Control[iCurrent+6]=this.cb_delete
this.Control[iCurrent+7]=this.cb_ok
this.Control[iCurrent+8]=this.dw_tk_single
this.Control[iCurrent+9]=this.dw_tk_liste_cross
this.Control[iCurrent+10]=this.dw_tk_uhr
end on

on w_taetigkeiten_edv.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.uo_zeitraum)
destroy(this.cb_new_tag)
destroy(this.dw_tk_liste_grid)
destroy(this.cb_wechsel)
destroy(this.cb_new_umlage)
destroy(this.cb_delete)
destroy(this.cb_ok)
destroy(this.dw_tk_single)
destroy(this.dw_tk_liste_cross)
destroy(this.dw_tk_uhr)
end on

event open;call super::open;
string ls_maxpos
n_cst_datetime lnv_datetime


// Berechtigungsprüfung für Menü
MenuID.dynamic of_checkrights()


is_schl_values = dw_tk_single.Describe ("schluessel_int.values")


dw_tk_liste_grid.visible = false

uo_zeitraum.of_SetZeitraum (Date (Year(today()), Month(today()), 1), &
									lnv_datetime.of_LastDayOfMonth (today()))

of_retrieve ()

wf_rowchanged(dw_tk_liste_grid.rowCount())

dw_tk_liste_cross.Modify ("DataWindow.HorizontalScrollSplit=280")
ls_maxpos = dw_tk_liste_cross.Describe ("DataWindow.HorizontalScrollMaximum2")
dw_tk_liste_cross.Modify ("DataWindow.HorizontalScrollPosition2=" + ls_maxpos)

cb_new_tag.TriggerEvent(Clicked!)

end event

event resize;long ll_wsw, ll_wsh, ll_wzeit


ll_wsw = this.workspacewidth()
ll_wsh = this.workspaceheight()

SetRedraw (FALSE)

IF dw_zeitdaten.visible THEN
	ll_wzeit = dw_zeitdaten.width
END IF

dw_tk_liste_cross.resize (ll_wsw - 40 - ll_wzeit, ll_wsh - dw_tk_liste_cross.Y - 130)
dw_tk_liste_grid.resize (ll_wsw - 40 - ll_wzeit, ll_wsh - dw_tk_liste_grid.Y - 130)

dw_tk_uhr.Width = ll_wsw - 70 - ll_wzeit
dw_tk_single.Width = ll_wsw - 70 - ll_wzeit
dw_info.Width = ll_wsw - 70 - ll_wzeit
dw_zeitdaten.height = ll_wsh - dw_zeitdaten.Y - 130
dw_zeitdaten.X = dw_tk_uhr.X + ll_wsw - 40 - ll_wzeit

uo_zeitraum.X = ll_wsw - uo_zeitraum.width - 31 - ll_wzeit
cb_wechsel.X = ll_wsw - uo_zeitraum.width - cb_wechsel.width - 261 - ll_wzeit

SetRedraw (TRUE)
end event

event closequery;integer li_response

IF dw_tk_single.AcceptText() = 1 THEN
	if dw_tk_single.ModifiedCount() <> 0 then
		li_response = MessageBox(this.Title, &
		              "Änderungen speichern ?" , Question!, YesNo!, 2)
		IF li_response = 1 THEN	cb_ok.Event clicked ()
	end if
end if

end event

type dw_zeitdaten from w_taetigkeiten_basis`dw_zeitdaten within w_taetigkeiten_edv
integer x = 3040
integer y = 0
integer height = 1900
end type

event dw_zeitdaten::ue_show;call super::ue_show;
IF ab_show THEN
	IF parent.width < this.X + 100 THEN
		parent.width = this.X + this.width
	END IF
ELSE
	parent.width = this.X
END IF

end event

type dw_info from w_taetigkeiten_basis`dw_info within w_taetigkeiten_edv
integer x = 32
integer y = 8
integer width = 2971
end type

type ds_schluessel from w_taetigkeiten_basis`ds_schluessel within w_taetigkeiten_edv
end type

type uo_zeitraum from u_auswahl_zeitraum within w_taetigkeiten_edv
event destroy ( )
integer x = 1536
integer y = 608
integer width = 1458
integer taborder = 70
end type

on uo_zeitraum.destroy
call u_auswahl_zeitraum::destroy
end on

event modified;call super::modified;
of_retrieve ()

end event

type cb_new_tag from commandbutton within w_taetigkeiten_edv
integer x = 41
integer y = 140
integer width = 361
integer height = 84
integer taborder = 10
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "&Neuer Tag"
end type

event clicked;long ll_row
date ldt_new

// neuen Tag einfügen
wf_new_tag()
// Datum aus Registry lesen und setzen
ll_row = dw_tk_uhr.getRow()

ldt_new = of_getlastdate()
IF year (ldt_new) < 2000 THEN ldt_new = today()
ldt_new = relativedate (ldt_new, 1)
DO WHILE DayNumber (ldt_new) = 1 OR DayNumber (ldt_new) = 7
	ldt_new = relativedate (ldt_new, 1)
LOOP
IF ldt_new > Today() THEN ldt_new = Today()

dw_tk_uhr.setItem(ll_row, "tag", DateTime (ldt_new))
dw_tk_uhr.setItemStatus(ll_row, 0, Primary!, NotModified!)

dw_tk_uhr.Event itemchanged (ll_row, dw_tk_uhr.object.tag, string (DateTime (ldt_new)))

// neue Umlage einfügen
wf_new_umlage()

end event

type dw_tk_liste_grid from datawindow within w_taetigkeiten_edv
boolean visible = false
integer x = 18
integer y = 728
integer width = 3003
integer height = 1172
integer taborder = 100
string title = "none"
string dataobject = "d_tk_liste_grid"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;//long ll_len
//string ls_dwo

if row > 0 then	
	wf_rowchanged(row)
//else
//	ls_dwo = string(dwo.Name)
//	ll_len = len(ls_dwo)
//	ls_dwo = left(ls_dwo, ll_len - 2)
//	dw_tk_liste_grid.SetSort(ls_dwo)
//	dw_tk_liste_grid.Sort ()
end if

dw_tk_liste_grid.setFocus()

end event

event rbuttondown;string ls_null

setNull(ls_null)
setFilter(ls_null)
Filter()

end event

event rowfocuschanged;wf_rowchanged(currentrow)

end event

event constructor;setTransObject(sqlca)
end event

type cb_wechsel from commandbutton within w_taetigkeiten_edv
integer x = 1051
integer y = 624
integer width = 361
integer height = 84
integer taborder = 70
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "&Einzeln"
end type

event clicked;
parent.SetRedraw (FALSE)
dw_tk_liste_grid.visible = not dw_tk_liste_grid.visible
dw_tk_liste_cross.visible = not dw_tk_liste_cross.visible

cb_delete.visible = dw_tk_liste_grid.visible
cb_new_tag.visible = dw_tk_liste_cross.visible
cb_new_umlage.visible = dw_tk_liste_cross.visible

of_retrieve ()
if dw_tk_liste_grid.visible then
	this.text = "Üb&ersicht"
	wf_setreadonly(TRUE)

	wf_rowchanged(dw_tk_liste_grid.rowCount())
else
	this.text = "&Einzeln"
	wf_setreadonly(FALSE)
	wf_new_umlage()
end if

parent.SetRedraw (TRUE)

end event

type cb_new_umlage from commandbutton within w_taetigkeiten_edv
integer x = 41
integer y = 424
integer width = 361
integer height = 84
integer taborder = 30
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "Neue &Umlage"
end type

event clicked;wf_new_umlage()


end event

type cb_delete from commandbutton within w_taetigkeiten_edv
boolean visible = false
integer x = 41
integer y = 424
integer width = 361
integer height = 84
integer taborder = 50
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "&Löschen"
end type

event clicked;wf_delete()

end event

event constructor;this.visible = false
end event

type cb_ok from commandbutton within w_taetigkeiten_edv
integer x = 41
integer y = 628
integer width = 361
integer height = 84
integer taborder = 60
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "&Speichern"
boolean default = true
end type

event clicked;string ls_umlage, ls_kst, ls_schluessel, ls_zusatz, ls_msg
date ldt_tag

if dw_tk_single.enabled then
	CHOOSE CASE wf_calc_zeit()
		CASE -1
			MessageBox("Unvollständige Eingabe", "Bei Zeit muss ein positiver Wert eingegeben werden!")
			dw_tk_single.SetFocus()
			dw_tk_single.SetColumn("zeit_str")
			
		CASE -2
			MessageBox("Fehlerhafte Eingabe", "Die Zeit muss in Minuten oder im Format hh:mm eingegeben werden!")	
			dw_tk_single.SetFocus()
			dw_tk_single.SetColumn("zeit_str")
			
		CASE -3
			MessageBox("Fehlerhafte Eingabe", "Bitte nicht mehr als 24 Stunden eingeben!")
			dw_tk_single.SetFocus()
			dw_tk_single.SetColumn("zeit_str")
			
		CASE -4
			MessageBox("Fehlerhafte Eingabe", "Bitte maximal 99999 Minuten (ca. 1666 Stunden) eingeben!")
			dw_tk_single.SetFocus()
			dw_tk_single.SetColumn("zeit_str")
			
		CASE ELSE
			IF dw_tk_single.RowCount () > 0 THEN
				ldt_tag = Date (dw_tk_single.getItemDateTime(1, "tag"))
				ls_schluessel = dw_tk_single.getItemString(1, "schluessel_int")
				ls_umlage = dw_tk_single.getItemString(1, "u_p")
				ls_zusatz = dw_tk_single.getItemString(1, "zusatz")
				ls_kst = dw_tk_single.getItemString(1, "von_kst")
				if isNull(ls_umlage) or trim (ls_umlage) = "" then
					dw_tk_single.setFocus()
					ls_msg = ""
					CHOOSE CASE ls_schluessel
						CASE "p"
							dw_tk_single.setColumn("u_p")
						CASE "b"
							dw_tk_single.setColumn("u_b")
						CASE "j"
							dw_tk_single.setColumn("jira")
							ls_msg = "~r~nIn dem JIRA-Ticket ist die Umlage nicht oder unvollständig angegeben!"
						CASE "a"
							dw_tk_single.setColumn("auftrag")
							ls_msg = "~r~nIn dem Auftrag ist keine Umlage angegeben!"
						CASE "k"
							dw_tk_single.setColumn("u_k")
						CASE "s"
							dw_tk_single.setColumn("u_s")
						CASE "t"
							dw_tk_single.setColumn("u_t")
						CASE "e"
							dw_tk_single.setColumn("u_e")
						CASE ELSE
							dw_tk_single.setColumn("schluessel_int")
					END CHOOSE
					Messagebox("Unvollständige Eingabe", "Die Umlage muss angegeben werden!" + ls_msg)
					
				elseif (ls_schluessel = "p" OR ls_schluessel = 'b') AND (IsNull (ls_zusatz) OR Trim (ls_zusatz) = '') then
					Messagebox("Unvollständige Eingabe", "Wählen Sie bitte zur Umlage auch die Art der Tätigkeit aus!")
	
				elseif IsNull (ls_kst) OR ls_kst = "" then
					Messagebox("Unvollständige Eingabe", "Geben Sie bitte Ihre Kostenstelle an!")
	
				elseif IsNull (ldt_tag) then
					Messagebox("Unvollständige Eingabe", "Der Tag muss angegeben werden!")
					
				elseif ldt_tag < 2002-01-01 or ldt_tag > Today() then
					Messagebox("Fehlerhafte Eingabe", "Der angegebene Tag liegt vor 2002 oder in der Zukunft!")
					
				else
					IF wf_update() <> 1 THEN return
					if dw_tk_liste_grid.visible then
						wf_rowchanged(dw_tk_liste_grid.rowCount())
					end if
					wf_new_umlage()
				end if
			end if
	END CHOOSE
end if

end event

type dw_tk_single from datawindow within w_taetigkeiten_edv
event dropdown pbm_dwndropdown
integer x = 32
integer y = 324
integer width = 2971
integer height = 284
integer taborder = 40
string title = "none"
string dataobject = "d_tk_single"
boolean vscrollbar = true
boolean border = false
boolean livescroll = true
end type

event dropdown;
long ll_row
string ls_colname, ls_umlage
datetime ldtm_tag
DataWindowChild ldwc_current

ls_colname = GetColumnName ()
IF Left (ls_colname, 2) = "u_" OR ls_colname = "von_kst" THEN
	// Filter auf Buchungsdatum setzen
	IF GetChild (ls_colname, ldwc_current) <> 1 THEN return
	
	ldtm_tag = GetItemDateTime (1, "tag")
	ls_umlage = GetItemString (1, ls_colname)
	ldwc_current.SetFilter ("(date (von) <= " + string (ldtm_tag, "yyyy-mm-dd") + &
									") and (IsNull(bis) or date (bis) >= " + &
									string (ldtm_tag, "yyyy-mm-dd") + ")")
	ldwc_current.Filter () 
	
	IF NOT IsNull (ls_umlage) THEN
		ll_row = ldwc_current.Find ("umlage='" + ls_umlage + "'", 1, ldwc_current.RowCount())
		IF ll_row < 1 THEN
			SetNull (ls_umlage)
			SetItem (1, ls_colname, ls_umlage)
			Modify ("t_lang.text=''")
		ELSE
			ldwc_current.SetRow (ll_row)
		END IF
	END IF
	
ELSEIF ls_colname = "auftrag" THEN
	// Filter auf Buchungsdatum setzen
	IF GetChild (ls_colname, ldwc_current) <> 1 THEN return
	
	ldtm_tag = GetItemDateTime (1, "tag")
	ls_umlage = GetItemString (1, ls_colname)
	ldwc_current.SetFilter ("isnull (erledigt) or (date (erledigt) >= relativedate (" + &
									string (ldtm_tag, "yyyy-mm-dd") + ", -8))")
	ldwc_current.Filter ()
	
	IF NOT IsNull (ls_umlage) THEN
		ll_row = ldwc_current.Find ("nr='" + ls_umlage + "'", 1, ldwc_current.RowCount())
		IF ll_row > 0 THEN ldwc_current.SetRow (ll_row)
	END IF

ELSEIF ls_colname = "jira" THEN
	// Filter auf Buchungsdatum setzen
	IF GetChild (ls_colname, ldwc_current) <> 1 THEN return
	
	ldtm_tag = GetItemDateTime (1, "tag")
	ls_umlage = GetItemString (1, ls_colname)
	ldwc_current.SetFilter (" kz_erledigt <> 'X' or (updated >= relativedate(" + &
									string (ldtm_tag, "yyyy-mm-dd") + ", -8 ))")
	ldwc_current.Filter ()
	
	IF NOT IsNull (ls_umlage) THEN
		ll_row = ldwc_current.Find ("ticket='" + ls_umlage + "'", 1, ldwc_current.RowCount())
		IF ll_row > 0 THEN ldwc_current.SetRow (ll_row)
	END IF
END IF

end event

event itemchanged;
DataWindowChild child_dddw
string ls_umlage, ls_schluessel, ls_old, ls_thema


if dwo.name = "u_p" or dwo.name = "u_t" then 
	IF GetChild ("zusatz", child_dddw) = 1 THEN
		child_dddw.SetTransObject (SQLCA)
		child_dddw.Retrieve (gs_modus, data)
	END IF
	
	setItem(row, "zusatz", "Realisierung")
end if

CHOOSE CASE dwo.name
	CASE "auftrag" // Aufträge
		ls_old = GetItemString (row, "auftrag")
		getChild (string(dwo.name), child_dddw)
		
		// Setzen Umlage und Schlüssel
		ls_umlage = child_dddw.getItemstring (child_dddw.getrow(), "umlage")
		ls_schluessel = child_dddw.getItemstring (child_dddw.getrow(), "schluessel")
		
		// Prüfen: Aufträge für Techn. Projekte nicht als Auftrag abrechenbar
		IF ls_schluessel = 'p' THEN
			MessageBox ("Achtung", "Dieser Auftrag betrifft das technische Projekt " + ls_umlage + &
											"!~r~nRechnen Sie Ihren Aufwand bitte direkt auf das Projekt ab!")
			SetItem (row, "auftrag", ls_old)
			return 1
		END IF
		
		SetItem (row, "u_p", ls_umlage)
		SetItem (row, "schluessel", ls_schluessel)

		Modify ("t_lang.text = '" + child_dddw.getItemstring (child_dddw.getrow(), "thema") + "'")
		
	CASE "jira" // JIRA-Tickets
		ls_old = GetItemString (row, "jira")
		getChild (string(dwo.name), child_dddw)
		
		// Setzen Umlage und Schlüssel
		ls_umlage = child_dddw.getItemstring (child_dddw.getrow(), "umlage")
		ls_schluessel = child_dddw.getItemstring (child_dddw.getrow(), "schluessel")
		
		// Prüfen: Aufträge für Techn. Projekte nicht als Auftrag abrechenbar
		IF ls_schluessel = 'p' THEN
			MessageBox ("Achtung", "Dieses Ticket betrifft das Projekt " + ls_umlage + &
											"!~r~nRechnen Sie Ihren Aufwand bitte direkt auf das Projekt ab!")
			SetItem (row, "jira", ls_old)
			return 1
		END IF
		
		SetItem (row, "u_p", ls_umlage)
		SetItem (row, "schluessel", ls_schluessel)

		Modify ("t_lang.text = '" + child_dddw.getItemstring (child_dddw.getrow(), "summary") + "'")
		
	CASE "u_s", "u_k", "u_p", "u_t", "u_e", "u_b"
		getChild (string(dwo.name), child_dddw)
		Modify ("t_lang.text = '" + child_dddw.getItemstring(child_dddw.getrow(), "lang") + "'")
		
END CHOOSE

if dwo.name = "schluessel_int" then
	IF GetChild ("zusatz", child_dddw) = 1 THEN
		child_dddw.SetTransObject (SQLCA)
		child_dddw.Retrieve (gs_modus, "")
	END IF
	
	if data = 'a' then
		setItem(row, "schluessel", "")
		setItem(row, "kz_auftrag", "X")
	elseif data = 'j' then
		setItem(row, "schluessel", "")
		setItem(row, "kz_auftrag", "J")
	else
		setItem(row, "schluessel", data)
		setItem(row, "kz_auftrag", " ")
	end if
	setItem(row, "u_p", "")
	setItem(row, "zusatz", "")
	setItem(row, "auftrag", "")
	setItem(row, "jira", "")
	modify("t_lang.text = ''")
end if

end event

event dberror;
if sqldbcode = 1 then 
	MessageBox("Doppelte Eingabe", "Zwei Tätigkeiten mit gleicher Umlage und gleichem Datum dürfen nicht eingegeben werden") 
	return 1
end if

end event

event losefocus;post acceptText()
end event

event constructor;
DataWindowChild ldwc_current

setTransObject(sqlca)

IF GetChild ("u_e", ldwc_current) = 1 THEN
	ldwc_current.SetTransObject (SQLCA)
	ldwc_current.Retrieve (gs_modus)
END IF

IF GetChild ("u_s", ldwc_current) = 1 THEN
	ldwc_current.SetTransObject (SQLCA)
	ldwc_current.Retrieve (gs_modus)
END IF

IF GetChild ("u_p", ldwc_current) = 1 THEN
	ldwc_current.SetTransObject (SQLCA)
	ldwc_current.Retrieve (gs_modus)
END IF

IF GetChild ("u_k", ldwc_current) = 1 THEN
	ldwc_current.SetTransObject (SQLCA)
	ldwc_current.Retrieve (gs_modus)
END IF

IF GetChild ("von_kst", ldwc_current) = 1 THEN
	ldwc_current.SetTransObject (SQLCA)
	ldwc_current.Retrieve (gs_username)
END IF

IF GetChild ("zusatz", ldwc_current) = 1 THEN
	ldwc_current.SetTransObject (SQLCA)
	ldwc_current.Retrieve (gs_modus, "")
END IF

IF GetChild ("jira", ldwc_current) = 1 THEN
	ldwc_current.SetTransObject (SQLCA)
	ldwc_current.Retrieve (gs_username, " ")
END IF


end event

event itemerror;
IF dwo.name = "auftrag" OR dwo.name = "jira" THEN return 3

end event

event buttonclicked;
DataWindowChild ldwc_current


IF dwo.name = "b_extended_jira" THEN
	// erweiterte Liste der JIRA-Tickets verwenden
	Modify ("b_extended_jira.visible='0'")
	IF GetChild ("jira", ldwc_current) = 1 THEN
		ldwc_current.SetTransObject (SQLCA)
		ldwc_current.Retrieve (gs_username, "X")
	END IF
END IF

end event

type dw_tk_liste_cross from datawindow within w_taetigkeiten_edv
integer x = 18
integer y = 728
integer width = 3003
integer height = 1172
integer taborder = 90
string title = "none"
string dataobject = "d_tk_liste_cross"
boolean hscrollbar = true
boolean vscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event constructor;setTransObject(sqlca)
end event

type dw_tk_uhr from datawindow within w_taetigkeiten_edv
integer x = 32
integer y = 112
integer width = 2971
integer height = 212
integer taborder = 20
string title = "none"
string dataobject = "d_tk_uhr"
boolean vscrollbar = true
boolean border = false
boolean livescroll = true
end type

event losefocus;
long ll_zeit, ll_rest
datetime ldtm_tag

acceptText()

if dw_tk_single.RowCount () > 0 then
	if IsNull (dw_tk_single.GetItemString (1, "zeit_str")) then
		ldtm_tag = dw_tk_uhr.getItemDateTime(1, "tag")
		// Restzeit ermitteln und vorbelegen
		select nvl (sum(zeit), 0)
		into :ll_zeit
		from intdba.tk 
		where username = :is_username
		and tag = :ldtm_tag
		using sqlca;
		
		ll_rest = dw_tk_uhr.getItemNumber(1, "min") - ll_zeit
		dw_tk_single.setItem(1, "zeit", ll_rest)
		dw_tk_single.setItem(1, "zeit_str", string(ll_rest))
		
	end if
end if

post acceptText()  // noch mal, damit Auswahl aus Kalender sofort wirkt

end event

event itemchanged;
long ll_pause, ll_count
datetime ldtm_tag
time lt_von, lt_bis


ldtm_tag = getItemDateTime (row, "tag")


if dwo.name = "tag" then 
	ldtm_tag = DateTime (Date (left (data, 10)))
	if dw_tk_single.RowCount () > 0 THEN dw_tk_single.setItem(1, "tag", ldtm_tag)
	
	of_modify_schluessel (Date (ldtm_tag), dw_tk_single)
	
	of_load_zeitdaten (Date (ldtm_tag))
	
	// sind für diesen Tag schon Daten erfasst?
	// dann diese laden und RowStatus ändern
	SELECT count (*)
	INTO :ll_count
	FROM intdba.tkuhr
	WHERE tag = :ldtm_tag
	  AND username = :is_username
	USING SQLCA;
	
	IF ll_count > 0 THEN this.Retrieve (ldtm_tag, is_username)
end if

lt_von = Time (getItemDateTime(row, "von"))
lt_bis = Time (getItemDateTime(row, "bis"))
ll_pause = getItemNumber(row, "pause")	
if isnull(ll_pause) then ll_pause = 0

if dwo.name = "von" then 
	lt_von = Time (mid(data, 11))
	
elseif dwo.name = "bis" then 
	lt_bis = Time (mid(data, 11))

elseif dwo.name = "pause" then 
	ll_pause = long(data)
	if isnull(ll_pause) then ll_pause = 0
end if

if dwo.name <> "zeit_kz" then
	setItem(row, "min", ((secondsafter(lt_von, lt_bis) / 60) - ll_pause))
end if

end event

event constructor;setTransObject(sqlca)
end event

