$PBExportHeader$w_taetigkeiten_iresf.srw
$PBExportComments$Tätigkeitenerfassung für Fachbereich IR (ESF-Prüfstelle)
forward
global type w_taetigkeiten_iresf from w_taetigkeiten_basis
end type
type uo_zeitraum from u_auswahl_zeitraum within w_taetigkeiten_iresf
end type
type cb_wechsel from commandbutton within w_taetigkeiten_iresf
end type
type cb_ok from commandbutton within w_taetigkeiten_iresf
end type
type dw_tk_single from datawindow within w_taetigkeiten_iresf
end type
type dw_tk_liste_cross from datawindow within w_taetigkeiten_iresf
end type
type dw_tk_liste_grid from datawindow within w_taetigkeiten_iresf
end type
type cb_delete from commandbutton within w_taetigkeiten_iresf
end type
type cb_new_umlage from commandbutton within w_taetigkeiten_iresf
end type
end forward

shared variables

end variables

global type w_taetigkeiten_iresf from w_taetigkeiten_basis
integer width = 3045
integer height = 2308
string menuname = "m_taetigkeiten_iresf"
event dfscl_print ( )
uo_zeitraum uo_zeitraum
cb_wechsel cb_wechsel
cb_ok cb_ok
dw_tk_single dw_tk_single
dw_tk_liste_cross dw_tk_liste_cross
dw_tk_liste_grid dw_tk_liste_grid
cb_delete cb_delete
cb_new_umlage cb_new_umlage
end type
global w_taetigkeiten_iresf w_taetigkeiten_iresf

type variables

end variables

forward prototypes
protected subroutine of_retrieve ()
protected subroutine wf_delete ()
protected function long wf_new_umlage ()
protected function long wf_rowchanged (long al_row)
protected function integer wf_update ()
public function integer of_getzeitraum (ref date adt_von, ref date adt_bis)
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

protected subroutine of_retrieve ();
string ls_maxpos
boolean lb_setpos = FALSE
date ld_von, ld_bis


SetPointer (Hourglass!)

IF dw_tk_liste_cross.Describe ("DataWindow.HorizontalScrollSplit") <> "0" THEN
	ls_maxpos = dw_tk_liste_cross.Describe ("DataWindow.HorizontalScrollMaximum2")
	lb_setpos = (ls_maxpos = dw_tk_liste_cross.Describe ("DataWindow.HorizontalScrollPosition2"))
END IF

uo_zeitraum.of_GetZeitraum (ld_von, ld_bis)
dw_tk_liste_cross.retrieve (is_username, DateTime (ld_von), DateTime (ld_bis))
dw_tk_liste_grid.retrieve (is_username, DateTime (ld_von), DateTime (ld_bis))

IF lb_setpos THEN
	ls_maxpos = dw_tk_liste_cross.Describe ("DataWindow.HorizontalScrollMaximum2")
	dw_tk_liste_cross.Modify ("DataWindow.HorizontalScrollPosition2=" + ls_maxpos)
END IF

end subroutine

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
WHERE schluessel = 'BUCHPER_AB_IR'
USING sqlca;

IF DATE (ldtm_tag) < DATE (ls_von) THEN
	MessageBox (This.Title, "Das angegebene Datum liegt außerhalb der von der Revision geöffneten Buchungsperiode!~r~n" + &
						"Buchungen vor dem " + ls_von + " sind nicht möglich!~r~n" + &
						"Falls Sie rückwirkend Änderungen vornehmen möchten, wenden Sie sich bitte an den Verantwortlichen der Revision.", StopSign!)
	return
END IF


SELECT wert
INTO :ls_von
FROM intdba.steuerung
WHERE schluessel = 'BUCHPER_AB'
USING sqlca;

IF Date (ldtm_tag) < DATE (ls_von) THEN
	MessageBox (This.Title, "Das angegebene Datum liegt außerhalb der vom CO geöffneten Buchungsperiode!~r~n" + &
						"Buchungen vor dem " + ls_von + " sind nicht möglich!~r~n" + &
						"Falls Sie rückwirkend Änderungen vornehmen möchten, wenden Sie sich bitte an das Controlling.", StopSign!)
	return
END IF

li_response = MessageBox(this.Title, "Wirklich löschen?", Question!, YesNo!, 2)
IF li_response = 1 THEN
	ll_row = dw_tk_liste_grid.getRow()
	setRedraw(false)
	
	ldtm_tag = dw_tk_single.getItemDatetime(dw_tk_single.getrow(), "tag")
	dw_tk_single.DeleteRow(0)
	
	IF wf_update() <> 1 THEN return
	
	if ll_row > 0 then dw_tk_liste_grid.scrollToRow(ll_row)
	setRedraw(true)
end if

dw_tk_liste_grid.setFocus()

end subroutine

protected function long wf_new_umlage ();long ll_row
date ld_date
string ls_kst

dw_tk_single.object.t_lang.text = ""

if dw_tk_single.rowCount () > 0 then
	ld_date = Date (dw_tk_single.getItemDateTime(1, "tag"))
	ls_kst = dw_tk_single.getItemString(1, "von_kst")
else
	ld_date = of_getlastdate()
	IF year (ld_date) < 2000 THEN ld_date = today()
	ld_date = relativedate (ld_date, 1)
	DO WHILE DayNumber (ld_date) = 1 OR DayNumber (ld_date) = 7
		ld_date = relativedate (ld_date, 1)
	LOOP
	IF ld_date > Today() THEN ld_date = Today()
	
	ls_kst = of_getlastkst ()
end if

// neue Zeile einfügen
dw_tk_single.Reset ()
ll_row = dw_tk_single.insertRow (0)
dw_tk_single.setColumn ("zeit_h")

if ll_row > 0 then
	dw_tk_single.scrollToRow(ll_row)

	of_modify_schluessel (ld_date, dw_tk_single)
	
	// Vorbelegung: Name, Datum, Schlüssel, Tag
	dw_tk_single.setItem (ll_row, "username", is_username)
	dw_tk_single.setItem (ll_row, "schluessel", "f")
	dw_tk_single.setItem (ll_row, "tag", DateTime (ld_date))
	dw_tk_single.setItem (ll_row, "von_kst", ls_kst)
	
	dw_tk_single.setItemStatus (ll_row, 0, Primary!, NotModified!)
	of_load_zeitdaten (ld_date)
end if

dw_tk_single.Modify ("t_lang.Text=''")
dw_tk_single.setFocus()

return 1

end function

protected function long wf_rowchanged (long al_row);
long ll_rows
decimal {0} ldc_id
date ldt_tag

if dw_tk_liste_grid.visible then
	dw_tk_liste_grid.selectRow (0, false)
	
	if al_row > 0 then
		dw_tk_liste_grid.selectRow(al_row, true)
	
		ldc_id = dw_tk_liste_grid.getItemDecimal(al_row, "id")
		
		ll_rows = dw_tk_single.retrieve (ldc_id)
		
		ldt_tag = Date (dw_tk_liste_grid.getItemDateTime(al_row, "tag"))
		of_load_zeitdaten (ldt_tag)
		of_modify_schluessel (ldt_tag, dw_tk_single)
	end if
	
	dw_tk_single.object.t_lang.text = ''
	dw_tk_liste_grid.setFocus()
end if

return 1

end function

protected function integer wf_update ();
decimal {0} ldc_id
string ls_kst, ls_von, ls_schluessel, ls_umlage
date ld_tag, ld_tag_orig
date ld_von, ld_bis
long ll_count
datetime ldtm_tag
integer li_rc


IF dw_tk_single.AcceptText() = 1 THEN
	IF dw_tk_single.RowCount () > 0 THEN
		// prüfen, ob Datum in geöffneter Periode
		ld_tag = Date (dw_tk_single.GetItemDateTime (1, "tag"))
		ld_tag_orig = Date (dw_tk_single.GetItemDateTime (1, "tag", Primary!, TRUE))
		
		SELECT wert
		INTO :ls_von
		FROM intdba.steuerung
		WHERE schluessel = 'BUCHPER_AB_IR'
		USING sqlca;
		
		IF ld_tag < DATE (ls_von) OR ld_tag_orig < DATE (ls_von) THEN
			MessageBox (This.Title, "Das angegebene Datum liegt außerhalb der von der Revision geöffneten Buchungsperiode!~r~n" + &
								"Buchungen vor dem " + ls_von + " sind nicht möglich!~r~n" + &
								"Falls Sie rückwirkend Änderungen vornehmen möchten, wenden Sie sich bitte an den Verantwortlichen der Revision.", StopSign!)
			return -1
		END IF
		
		
		SELECT wert
		INTO :ls_von
		FROM intdba.steuerung
		WHERE schluessel = 'BUCHPER_AB'
		USING SQLCA;

		IF ld_tag < DATE (ls_von) OR ld_tag_orig < DATE (ls_von) THEN
			MessageBox (This.Title, "Das angegebene Datum liegt außerhalb der vom CO geöffneten Buchungsperiode!~r~n" + &
								"Buchungen vor dem " + ls_von + " sind nicht möglich!~r~n" + &
								"Falls Sie rückwirkend Änderungen vornehmen möchten, wenden Sie sich bitte an das Controlling.", StopSign!)
			return -1
		END IF

		// Zusätze dürfen nicht NULL sein
		IF IsNull (dw_tk_single.GetItemString (1, "zusatz")) THEN
			dw_tk_single.SetItem (1, "zusatz", " ")
		END IF
		
		IF IsNull (dw_tk_single.GetItemString (1, "zusatz1")) THEN
			dw_tk_single.SetItem (1, "zusatz1", " ")
		END IF
		
		IF IsNull (dw_tk_single.GetItemString (1, "zusatz2")) THEN
			dw_tk_single.SetItem (1, "zusatz2", " ")
		END IF
		
		// prüfen, dass ausgewählte Umlage auch an dem Tag gültig ist
		ldtm_tag = DateTime (ld_tag)
		ls_schluessel = dw_tk_single.GetItemString (1, "schluessel")
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
			MessageBox (This.Title, "Die ausgewählte Umlage ist am Abrechnungstag nicht gültig!~r~n" + &
								"Wählen Sie eine andere Umlage!", StopSign!)
			return -1
		END IF
		
		
		// prüfen, dass ausgewählte eigene KST auch an dem Tag gültig ist
		ls_umlage = dw_tk_single.GetItemString (1, "von_kst")
		SELECT count (*)
		INTO :ll_count
		FROM intdba.umlage
		WHERE schluessel = 'k'
		  AND umlage = :ls_umlage
		  AND :ldtm_tag BETWEEN von AND NVL (bis, TO_DATE ('2900-01-01', 'YYYY-MM-DD'))
		USING sqlca;
		
		IF ll_count < 1 THEN
			MessageBox (This.Title, "Ihre Kostenstelle ist am Abrechnungstag nicht gültig!~r~n" + &
								"Wählen Sie eine andere Kostenstelle!", StopSign!)
			return -1
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
	
	li_rc = dw_tk_single.Update()
	IF li_rc = 1 THEN	
		COMMIT USING SQLCA;

		IF SQLCA.SQLCode <> 0 THEN
			MessageBox(This.Title,"Die Daten konnten nicht gespeichert werden.  Code: " + &
				String(SQLCA.SQLCode) + " - " + SQLCA.SQLErrText + ".", &
				StopSign!)
			ROLLBACK USING SQLCA;
		END IF	
		
		if dw_tk_single.rowCount() > 0 then 
			ld_tag = Date (dw_tk_single.getItemDateTime (1, "tag"))
			ls_kst = dw_tk_single.getItemString(1, "von_kst")
			
			of_setlastdate (ld_tag)
			of_setlastkst (ls_kst)
			
			uo_zeitraum.of_GetZeitraum (ld_von, ld_bis)
			if ld_tag < ld_von or ld_tag > ld_bis then
				messagebox("Hinweis", "Der " + string(ld_tag, "dd.mm.yyyy") + " liegt nicht im gewählten Zeitfenster vom " + &
				string(ld_von, "dd.mm.yyyy") + " bis " + string(ld_bis, "dd.mm.yyyy") + "~n" + &
				"Um den gerade gespeicherten Eintrag anzuzeigen, wählen Sie am besten >Alle Daten<")
			end if
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

public function integer of_getzeitraum (ref date adt_von, ref date adt_bis);
uo_zeitraum.of_GetZeitraum (adt_von, adt_bis)

return 1

end function

on w_taetigkeiten_iresf.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_taetigkeiten_iresf" then this.MenuID = create m_taetigkeiten_iresf
this.uo_zeitraum=create uo_zeitraum
this.cb_wechsel=create cb_wechsel
this.cb_ok=create cb_ok
this.dw_tk_single=create dw_tk_single
this.dw_tk_liste_cross=create dw_tk_liste_cross
this.dw_tk_liste_grid=create dw_tk_liste_grid
this.cb_delete=create cb_delete
this.cb_new_umlage=create cb_new_umlage
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_zeitraum
this.Control[iCurrent+2]=this.cb_wechsel
this.Control[iCurrent+3]=this.cb_ok
this.Control[iCurrent+4]=this.dw_tk_single
this.Control[iCurrent+5]=this.dw_tk_liste_cross
this.Control[iCurrent+6]=this.dw_tk_liste_grid
this.Control[iCurrent+7]=this.cb_delete
this.Control[iCurrent+8]=this.cb_new_umlage
end on

on w_taetigkeiten_iresf.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.uo_zeitraum)
destroy(this.cb_wechsel)
destroy(this.cb_ok)
destroy(this.dw_tk_single)
destroy(this.dw_tk_liste_cross)
destroy(this.dw_tk_liste_grid)
destroy(this.cb_delete)
destroy(this.cb_new_umlage)
end on

event open;call super::open;
string ls_maxpos
n_cst_datetime lnv_datetime


// Berechtigungsprüfung für Menü
MenuID.dynamic of_checkrights()

is_schl_values = dw_tk_single.Describe ("schluessel.values")

dw_tk_liste_grid.visible = false

uo_zeitraum.of_SetZeitraum (Date (Year(today()), Month(today()), 1), &
									lnv_datetime.of_LastDayOfMonth (today()))

of_retrieve ()

wf_rowchanged (dw_tk_liste_grid.rowCount())

dw_tk_liste_cross.Modify ("DataWindow.HorizontalScrollSplit=280")
ls_maxpos = dw_tk_liste_cross.Describe ("DataWindow.HorizontalScrollMaximum2")
dw_tk_liste_cross.Modify ("DataWindow.HorizontalScrollPosition2=" + ls_maxpos)

cb_new_umlage.TriggerEvent (Clicked!)

end event

event resize;long ll_wsw, ll_wsh, ll_wzeit


ll_wsw = newwidth
ll_wsh = newheight

SetRedraw (FALSE)

IF dw_zeitdaten.visible THEN
	ll_wzeit = dw_zeitdaten.width
END IF

dw_tk_liste_cross.resize (ll_wsw - 40 - ll_wzeit, ll_wsh - dw_tk_liste_cross.Y - 130)
dw_tk_liste_grid.resize (ll_wsw - 40 - ll_wzeit, ll_wsh - dw_tk_liste_grid.Y - 130)

dw_tk_single.width = ll_wsw - 100 - ll_wzeit
dw_info.width = ll_wsw - 100 - ll_wzeit
dw_zeitdaten.height = ll_wsh - dw_zeitdaten.Y - 130
dw_zeitdaten.X = dw_tk_single.X + ll_wsw - 40 - ll_wzeit

uo_zeitraum.X = ll_wsw - uo_zeitraum.width - 31 - ll_wzeit
cb_wechsel.X = ll_wsw - uo_zeitraum.width - cb_wechsel.width - 261 - ll_wzeit
SetRedraw (TRUE)

end event

event closequery;integer li_response

IF dw_tk_single.AcceptText() = 1 THEN
	li_response = dw_tk_single.ModifiedCount()
	if dw_tk_single.ModifiedCount() <> 0 then
		li_response = MessageBox(this.Title, &
		              "Änderungen speichern ?" , Question!, YesNo!, 2)
		IF li_response = 1 THEN	cb_ok.Event clicked ()
	end if
end if

end event

type dw_zeitdaten from w_taetigkeiten_basis`dw_zeitdaten within w_taetigkeiten_iresf
integer x = 2994
integer y = 0
integer height = 2116
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

type dw_info from w_taetigkeiten_basis`dw_info within w_taetigkeiten_iresf
integer x = 50
integer width = 2921
end type

type ds_schluessel from w_taetigkeiten_basis`ds_schluessel within w_taetigkeiten_iresf
end type

type uo_zeitraum from u_auswahl_zeitraum within w_taetigkeiten_iresf
event destroy ( )
integer x = 1495
integer y = 472
integer taborder = 80
end type

on uo_zeitraum.destroy
call u_auswahl_zeitraum::destroy
end on

event modified;call super::modified;
of_retrieve ()

end event

type cb_wechsel from commandbutton within w_taetigkeiten_iresf
integer x = 567
integer y = 488
integer width = 361
integer height = 84
integer taborder = 70
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "&Einzeln"
end type

event clicked;
parent.SetRedraw (FALSE)
dw_tk_liste_grid.visible = not(dw_tk_liste_grid.visible)
dw_tk_liste_cross.visible = not(dw_tk_liste_cross.visible)

cb_delete.visible = dw_tk_liste_grid.visible
cb_new_umlage.visible = dw_tk_liste_cross.visible

of_retrieve ()
if dw_tk_liste_grid.visible then
	this.text = "Üb&ersicht"

	wf_rowchanged(dw_tk_liste_grid.rowCount())
else
	this.text = "&Einzeln"
	wf_new_umlage()
end if
parent.SetRedraw (TRUE)

end event

type cb_ok from commandbutton within w_taetigkeiten_iresf
integer x = 91
integer y = 488
integer width = 361
integer height = 84
integer taborder = 60
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "&Speichern"
boolean default = true
end type

event clicked;
string ls_schluessel, ls_umlage, ls_kst, ls_bem, ls_zusatz, ls_zusatz1
date ldt_tag
long ll_zeit

if dw_tk_single.enabled then
	IF dw_tk_single.RowCount () > 0 THEN
		ls_schluessel = dw_tk_single.getItemString(1, "schluessel")
		ls_umlage = dw_tk_single.getItemString(1, "u_p")
		ls_kst = dw_tk_single.getItemString(1, "von_kst")
		ls_zusatz = dw_tk_single.getItemString(1, "zusatz")
		ls_zusatz1 = dw_tk_single.getItemString(1, "zusatz1")
		ls_bem = dw_tk_single.getItemString(1, "bemerkung")
		ldt_tag = Date (dw_tk_single.getItemDateTime(1, "tag"))
		ll_zeit = dw_tk_single.getItemNumber(1, "zeit")
		
		if isNull(ls_umlage) or trim (ls_umlage) = "" then
			dw_tk_single.setFocus()
			CHOOSE CASE ls_schluessel
				CASE "p"
					dw_tk_single.setColumn("u_p")
				CASE "b"
					dw_tk_single.setColumn("u_b")
				CASE "k"
					dw_tk_single.setColumn("u_k")
				CASE "s"
					dw_tk_single.setColumn("u_s")
			END CHOOSE
			Messagebox("Unvollständige Eingabe", "Die Umlage muss angegeben werden!")

		elseif (ls_schluessel = "p" OR ls_schluessel = 'b') AND (IsNull (ls_zusatz) OR Trim (ls_zusatz) = '') then
			Messagebox("Unvollständige Eingabe", "Wählen Sie bitte zur Umlage auch die Art der Tätigkeit aus!")

		elseif IsNull (ls_kst) OR ls_kst = "" then
			Messagebox("Unvollständige Eingabe", "Geben Sie bitte Ihre Kostenstelle an!")

		elseif IsNull (ldt_tag) then
			Messagebox("Unvollständige Eingabe", "Der Tag muss angegeben werden!")
			
		elseif ldt_tag < 2016-01-01 then
			Messagebox("Fehlerhafte Eingabe", "Der angegebene Tag liegt vor 2016!")
			
		elseif ldt_tag > Today() AND NOT (ls_schluessel = "s" AND (ls_zusatz1 = "Urlaub" OR ls_zusatz1 = "Krankheit")) then
			Messagebox("Fehlerhafte Eingabe", "Der angegebene Tag liegt in der Zukunft!")
			
		elseif IsNull (ll_zeit) then
			Messagebox("Unvollständige Eingabe", "Die Arbeitszeit muss angegeben werden!")
			dw_tk_single.setColumn("zeit_h")
			dw_tk_single.SetFocus ()
		
		elseif (ls_schluessel = 'k' OR ls_schluessel = 's') AND (IsNull (ls_zusatz1) OR Trim (ls_zusatz1) = "") then
			Messagebox("Unvollständige Eingabe", "Wählen Sie bitte eine Tätigkeit aus!")

		elseif IsNull (ls_bem) OR ls_bem = "" then
			Messagebox("Unvollständige Eingabe", "Geben Sie bitte eine Bemerkung an!")

		else
			IF wf_update() <> 1 THEN return
			if dw_tk_liste_grid.visible then
				wf_rowchanged(dw_tk_liste_grid.rowCount())
			end if
			wf_new_umlage()
		end if
	end if
end if

end event

type dw_tk_single from datawindow within w_taetigkeiten_iresf
event dropdown pbm_dwndropdown
integer x = 50
integer y = 104
integer width = 2921
integer height = 364
integer taborder = 40
string title = "none"
string dataobject = "d_tk_single_iresf"
boolean border = false
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
			IF Left (ls_colname, 2) = "u_" THEN Modify ("t_lang.text=''")
		ELSE
			ldwc_current.SetRow (ll_row)
		END IF
	END IF
	
END IF

end event

event itemchanged;
DataWindowChild child_dddw
decimal {2} ldc_zeit, ldc_zeit_neu
long ll_row

if dwo.name = "zeit_h" then
	SetItem (row, "zeit", Dec (data) * 60)
	
	IF Mod (Round (Dec (data), 5), 0.5) <> 0 THEN
		MessageBox ("Hinweis", "Bitte erfassen Sie die Zeit auf halbe Stunden genau!")
	END IF
	return  // kein auto-tab

elseif dwo.name = "u_p" then
	IF GetChild ("zusatz_p", child_dddw) = 1 THEN
		child_dddw.SetTransObject (SQLCA)
		child_dddw.Retrieve (gs_modus, data)
	END IF
	
	// Vorbelegung des Zusatz mit "Testkonzept/ Funktionstest"
	// und Anzeige des Textes
	setItem (row, "zusatz", "Testkonzept/ Funktionstest")
	
	IF getChild (string(dwo.name), child_dddw) > 0 THEN
		Modify ("t_lang.text = '" + child_dddw.getItemstring(child_dddw.getrow(), "lang") + "'")
	END IF

elseif dwo.name = "u_b" then
	// Anzeige des Textes
	IF getChild (string(dwo.name), child_dddw) > 0 THEN
		Modify ("t_lang.text = '" + child_dddw.getItemstring(child_dddw.getrow(), "lang") + "'")
	END IF

elseif dwo.name = "u_k" then
	// Vorbelegung des Zusatz mit " "
	// und Anzeige des Textes
	setItem (row, "zusatz", " ")

	IF GetChild ("zusatz_k", child_dddw) = 1 THEN
		child_dddw.SetTransObject (SQLCA)
		child_dddw.Retrieve (data)
	END IF
	
	IF getChild (string(dwo.name), child_dddw) > 0 THEN
		Modify ("t_lang.text = '" + child_dddw.getItemstring(child_dddw.getrow(), "lang") + "'")
	END IF

elseif dwo.name = "schluessel" then
	IF GetChild ("zusatz_s", child_dddw) = 1 THEN
		child_dddw.SetTransObject (SQLCA)
		child_dddw.Retrieve (gs_modus, "")
	END IF
	
	setItem (row, "zusatz", "")
	setItem (row, "zusatz1", " ")
	setItem (row, "zusatz2", " ")

	IF data = "s" THEN
		setItem (row, "u_p", "INTERN")
		Modify ("t_lang.text = 'erscheint nicht im CO (nur für internen Gebrauch)'")
	ELSE
		setItem (row, "u_p", "")
		Modify ("t_lang.text = ''")
	END IF
	
elseif dwo.name = "zusatz1_s" then
	// manche dieser Auswahlen (Schlüssel enthält ein "*") sollen mit 7,8 Stunden vorbelegt werden
	IF GetChild ("zusatz1_s", child_dddw) = 1 THEN
		ll_row = child_dddw.Find ("wert = '" + data + "'", 1, child_dddw.RowCount())
		IF ll_row > 0 THEN
			IF Pos (child_dddw.GetItemString (ll_row, "kat_key"), "*") > 0 THEN
				ldc_zeit = GetItemNumber (1, "zeit_h")
				ldc_zeit_neu = 7.8
				IF ldc_zeit <> ldc_zeit_neu OR IsNull (ldc_zeit) THEN
					IF MessageBox ("Hinweis", "Soll die Zeit auf " + string (ldc_zeit_neu, "0.0") + " Stunden gesetzt werden?", Question!, YesNo!, 1) = 1 THEN
						SetItem (1, "zeit", ldc_zeit_neu * 60)
						SetItem (1, "zeit_h", ldc_zeit_neu)
					END IF			
				END IF			
			END IF
		END IF
	END IF

elseif dwo.name = "tag" then
	of_modify_schluessel (Date (left (data, 10)), dw_tk_single)
	of_load_zeitdaten (Date (left (data, 10)))
end if

end event

event dberror;if sqldbcode = 1 then
	MessageBox("Doppelte Eingabe", "Zwei Tätigkeiten mit gleicher Umlage und gleichem Datum dürfen nicht eingegeben werden") 
	return 1
end if

end event

event losefocus;post acceptText()
end event

event constructor;
DataWindowChild ldwc_current

setTransObject(sqlca)

IF GetChild ("u_p", ldwc_current) = 1 THEN
	ldwc_current.SetTransObject (SQLCA)
	ldwc_current.Retrieve (gs_modus)
END IF

IF GetChild ("u_s", ldwc_current) = 1 THEN
	ldwc_current.SetTransObject (SQLCA)
	ldwc_current.Retrieve (gs_modus)
END IF

IF GetChild ("zusatz_p", ldwc_current) = 1 THEN
	ldwc_current.SetTransObject (SQLCA)
	ldwc_current.Retrieve (gs_modus, "")
END IF

IF GetChild ("von_kst", ldwc_current) = 1 THEN
	ldwc_current.SetTransObject (SQLCA)
	ldwc_current.Retrieve (gs_modus)
END IF

end event

type dw_tk_liste_cross from datawindow within w_taetigkeiten_iresf
integer x = 18
integer y = 592
integer width = 2898
integer height = 896
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

type dw_tk_liste_grid from datawindow within w_taetigkeiten_iresf
boolean visible = false
integer x = 18
integer y = 592
integer width = 2825
integer height = 896
integer taborder = 100
string title = "none"
string dataobject = "d_tk_liste_iresf"
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
//	dw_tk_liste_grid.setSort (ls_dwo)
//	dw_tk_liste_grid.Sort ()
end if

dw_tk_liste_grid.setFocus()

end event

event rowfocuschanged;wf_rowchanged(currentrow)

end event

event constructor;setTransObject(sqlca)
end event

type cb_delete from commandbutton within w_taetigkeiten_iresf
integer x = 2569
integer y = 84
integer width = 361
integer height = 84
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "&Löschen"
end type

event clicked;wf_delete()

end event

event constructor;this.visible = false
end event

type cb_new_umlage from commandbutton within w_taetigkeiten_iresf
integer x = 2181
integer y = 84
integer width = 361
integer height = 84
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "Neue &Umlage"
end type

event clicked;wf_new_umlage()


end event

