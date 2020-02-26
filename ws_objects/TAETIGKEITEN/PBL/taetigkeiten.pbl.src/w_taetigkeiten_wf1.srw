$PBExportHeader$w_taetigkeiten_wf1.srw
$PBExportComments$Tätigkeitenerfassung für WF1
forward
global type w_taetigkeiten_wf1 from w_taetigkeiten_basis
end type
type cb_ok from commandbutton within w_taetigkeiten_wf1
end type
type dw_tk_single from datawindow within w_taetigkeiten_wf1
end type
type dw_tk_liste from datawindow within w_taetigkeiten_wf1
end type
type cb_new_umlage from commandbutton within w_taetigkeiten_wf1
end type
end forward

shared variables

end variables

global type w_taetigkeiten_wf1 from w_taetigkeiten_basis
integer width = 3022
integer height = 1984
string menuname = "m_taetigkeiten_wf1"
event dfscl_print ( )
cb_ok cb_ok
dw_tk_single dw_tk_single
dw_tk_liste dw_tk_liste
cb_new_umlage cb_new_umlage
end type
global w_taetigkeiten_wf1 w_taetigkeiten_wf1

type variables

date id_von
protected boolean ib_loading = FALSE

end variables

forward prototypes
protected subroutine of_retrieve ()
protected subroutine wf_delete ()
protected function long wf_new_umlage ()
protected function long wf_rowchanged (long al_row)
protected function integer wf_update ()
end prototypes

event dfscl_print();
// Drucken

SetPointer (HourGlass!)

dw_tk_liste.Print ()

end event

protected subroutine of_retrieve ();
SetPointer (Hourglass!)

dw_tk_liste.retrieve (is_username, DateTime (id_von))

end subroutine

protected subroutine wf_delete ();long ll_row
integer li_response
string ls_von
datetime ldtm_tag

if dw_tk_single.rowCount() < 1 then return
	
// prüfen, ob Datum in geöffneter Periode
ldtm_tag = dw_tk_single.GetItemDateTime (1, "tag")

// Prüfung Sperrung CO
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

// Prüfung Sperrung WF1
SELECT wert
INTO :ls_von
FROM intdba.steuerung
WHERE schluessel = 'BUCHPER_AB_WF1'
USING sqlca;

IF Date (ldtm_tag) < DATE (ls_von) THEN
	MessageBox (This.Title, "Das angegebene Datum liegt außerhalb der von der WF Zuschuss geöffneten Buchungsperiode!~r~n" + &
						"Buchungen vor dem " + ls_von + " sind nicht möglich!~r~n" + &
						"Falls Sie rückwirkend Änderungen vornehmen möchten, wenden Sie sich bitte an den Verantwortlichen der WF Zuschuss.", StopSign!)
	return
END IF

li_response = MessageBox (this.Title, "Möchten Sie den Datensatz wirklich löschen?", Question!, YesNo!, 2)
IF li_response = 1 THEN
	ll_row = dw_tk_liste.getRow()
	setRedraw(false)
	
	ldtm_tag = dw_tk_single.getItemDatetime(dw_tk_single.getrow(), "tag")
	dw_tk_single.DeleteRow(0)
	
	IF wf_update() <> 1 THEN return
	
	wf_new_umlage ()
	setRedraw(true)
end if

dw_tk_liste.setFocus()

end subroutine

protected function long wf_new_umlage ();long ll_row
date ld_date
string ls_kst

if dw_tk_single.ModifiedCount() <> 0 then
	IF MessageBox ("Achtung", "Ihre Änderungen wurden noch nicht gespeichert und gehen verloren!", &
						StopSign!, OkCancel!, 2) <> 1 THEN return 1
end if

dw_tk_liste.SelectRow (0, FALSE)

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

id_von = ld_date

// neue Zeile einfügen
dw_tk_single.Reset ()
dw_tk_single.Modify ("zusatz_f.visible='0' zusatz1.visible='0' zusatz2.visible='0' zusatz_p.visible='0' zusatz_b.visible='0'")

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
	dw_tk_single.setItem (ll_row, "u_p", "")
	dw_tk_single.setItem (ll_row, "zusatz_p", "")
	dw_tk_single.setItem (ll_row, "zusatz1", " ")
	dw_tk_single.setItem (ll_row, "zusatz2", " ")
	
	dw_tk_single.setItemStatus (ll_row, 0, Primary!, NotModified!)
	
	dw_tk_single.SetColumn ("u_f")
	of_load_zeitdaten (ld_date)
end if

dw_tk_single.setFocus()

return 1

end function

protected function long wf_rowchanged (long al_row);
long ll_rows
decimal {0} ldc_id

dw_tk_liste.selectRow (0, false)

if al_row > 0 then
	dw_tk_liste.selectRow(al_row, true)

	ldc_id = dw_tk_liste.getItemDecimal(al_row, "id")
	
	dw_tk_single.Modify ("zusatz_f.visible='0' zusatz1.visible='0' zusatz2.visible='0' zusatz_p.visible='0' zusatz_b.visible='0'")
	ll_rows = dw_tk_single.retrieve (ldc_id)
	
	IF ll_rows > 0 THEN
		// Sichtbarkeit der Zusatz-Felder
		ib_loading = TRUE
		dw_tk_single.Event itemchanged (1, dw_tk_single.object.__get_attribute ("schluessel", TRUE), dw_tk_single.GetItemString (1, "schluessel"))
		CHOOSE CASE dw_tk_single.GetItemString (1, "schluessel") 
			CASE "p"
				dw_tk_single.Event itemchanged (1, dw_tk_single.object.__get_attribute ("u_p", TRUE), dw_tk_single.GetItemString (1, "u_p"))
			CASE "b"
				dw_tk_single.Event itemchanged (1, dw_tk_single.object.__get_attribute ("u_b", TRUE), dw_tk_single.GetItemString (1, "u_b"))
			CASE ELSE
				dw_tk_single.Event itemchanged (1, dw_tk_single.object.__get_attribute ("u_f", TRUE), dw_tk_single.GetItemString (1, "u_f"))
				dw_tk_single.Event itemchanged (1, dw_tk_single.object.__get_attribute ("zusatz_f", TRUE), dw_tk_single.GetItemString (1, "zusatz_f"))
				dw_tk_single.Event itemchanged (1, dw_tk_single.object.__get_attribute ("zusatz1", TRUE), dw_tk_single.GetItemString (1, "zusatz1"))
		END CHOOSE
		ib_loading = FALSE
	END IF
end if

dw_tk_liste.setFocus()

return 1

end function

protected function integer wf_update ();
string ls_kst, ls_von, ls_schluessel, ls_umlage, ls_msg
date ld_tag, ld_tag_orig
long ll_count, ll_sum, ll_maxzeit
datetime ldtm_tag
integer li_rc
decimal {0} ldc_id

IF dw_tk_single.AcceptText() = 1 THEN
	IF dw_tk_single.RowCount () > 0 THEN
		// prüfen, ob Datum in geöffneter Periode
		ld_tag = Date (dw_tk_single.GetItemDateTime (1, "tag"))
		ld_tag_orig = Date (dw_tk_single.GetItemDateTime (1, "tag", Primary!, TRUE))
		
		IF ld_tag > Today() THEN
			MessageBox (This.Title, "Das angegebene Datum liegt in der Zukunft!", StopSign!)
			return -1
		END IF
		
		// Prüfung Sperrung CO
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

		// Prüfung Sperrung WF1
		SELECT wert
		INTO :ls_von
		FROM intdba.steuerung
		WHERE schluessel = 'BUCHPER_AB_WF1'
		USING sqlca;
		
		IF ld_tag < DATE (ls_von) OR ld_tag_orig < DATE (ls_von) THEN
			MessageBox (This.Title, "Das angegebene Datum liegt außerhalb der von der WF Zuschuss geöffneten Buchungsperiode!~r~n" + &
								"Buchungen vor dem " + ls_von + " sind nicht möglich!~r~n" + &
								"Falls Sie rückwirkend Änderungen vornehmen möchten, wenden Sie sich bitte an den Verantwortlichen der WF Zuschuss.", StopSign!)
			return -1
		END IF


		// Zusatz darf nicht NULL sein
		IF IsNull (dw_tk_single.GetItemString (1, "zusatz")) THEN
			dw_tk_single.SetItem (1, "zusatz", " ")
		END IF
		
		// prüfen, dass ausgewählte Umlage auch an dem Tag gültig ist
		ldtm_tag = DateTime (ld_tag)
		ls_schluessel = dw_tk_single.GetItemString (1, "schluessel")
		ls_umlage = dw_tk_single.GetItemString (1, "u_p")
		
		SELECT count (*)
		INTO :ll_count
		FROM intdba.umlage
		WHERE schluessel = :ls_schluessel
		  AND umlage = :ls_umlage
		  AND :ldtm_tag BETWEEN von AND NVL (bis, TO_DATE ('2900-01-01', 'YYYY-MM-DD'))
		USING sqlca;
		
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
	
	li_rc = dw_tk_single.Update (TRUE, FALSE)
	IF li_rc = 1 THEN	
		
		// gespeicherten Stand für Prüfung laden
		dw_tk_liste.SetRedraw (FALSE)
		dw_tk_liste.post SetRedraw (TRUE)
		of_retrieve ()
		
		// Zeit prüfen
		ll_maxzeit = 10
		ll_sum = 0
		
		IF dw_tk_liste.RowCount () > 0 THEN
			ll_sum = dw_tk_liste.GetItemNumber (1, "sum_zeit")
			
			IF dw_tk_liste.Find ("zusatz = 'Unternehmensbesuche außerhalb Artikel 4' or (zusatz1='Artikel 4 (2) - Vor-Ort-Kontrolle' and zusatz2 = 'Durchführung')", 1, dw_tk_liste.RowCount ()) > 0 THEN
				ll_maxzeit = 12
				ls_msg = "für Vor-Ort-Kontrollen/Unternehmensbesuche "
			END IF
		END IF
		
		IF ll_sum > ll_maxzeit THEN
			// zuviel eingegeben -> Speicherung ungültig
			ROLLBACK USING SQLCA;
			of_retrieve ()
			
			MessageBox ("Achtung", "Die zulässige Höchstarbeitszeit " + ls_msg + "beträgt " + string (ll_maxzeit, "0") + &
									"h/Tag. Bitte reduzieren Sie Ihre Eingabe!", Information!, Ok!)
									
			return -1
		END IF
		
		// Speicherung gültig
		COMMIT USING SQLCA;

		IF SQLCA.SQLCode <> 0 THEN
			MessageBox(This.Title,"Die Daten konnten nicht gespeichert werden.  Code: " + &
				String(SQLCA.SQLCode) + " - " + SQLCA.SQLErrText + ".", &
				StopSign!)
			ROLLBACK USING SQLCA;
		END IF	
		
		dw_tk_single.ResetUpdate ()
		
		if dw_tk_single.rowCount() > 0 then 
			ld_tag = Date (dw_tk_single.getItemDateTime (1, "tag"))
			ls_kst = dw_tk_single.getItemString(1, "von_kst")
			
			of_setlastdate (ld_tag)
			of_setlastkst (ls_kst)
		end if
		
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

on w_taetigkeiten_wf1.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_taetigkeiten_wf1" then this.MenuID = create m_taetigkeiten_wf1
this.cb_ok=create cb_ok
this.dw_tk_single=create dw_tk_single
this.dw_tk_liste=create dw_tk_liste
this.cb_new_umlage=create cb_new_umlage
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.dw_tk_single
this.Control[iCurrent+3]=this.dw_tk_liste
this.Control[iCurrent+4]=this.cb_new_umlage
end on

on w_taetigkeiten_wf1.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.dw_tk_single)
destroy(this.dw_tk_liste)
destroy(this.cb_new_umlage)
end on

event open;call super::open;
// Berechtigungsprüfung für Menü
MenuID.dynamic of_checkrights()

is_schl_values = dw_tk_single.Describe ("schluessel.values")

wf_new_umlage()
of_retrieve ()

end event

event resize;long ll_wsw, ll_wsh, ll_wzeit

ll_wsw = newwidth
ll_wsh = newheight

SetRedraw (FALSE)

IF dw_zeitdaten.visible THEN
	ll_wzeit = dw_zeitdaten.width
END IF

dw_tk_liste.resize (ll_wsw - 40 - ll_wzeit, ll_wsh - dw_tk_liste.Y - 130)
dw_tk_single.width = dw_tk_liste.width
dw_info.width = dw_tk_liste.width
dw_zeitdaten.height = ll_wsh - dw_zeitdaten.Y - 130
dw_zeitdaten.X = dw_tk_single.X + ll_wsw - 40 - ll_wzeit

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

type dw_zeitdaten from w_taetigkeiten_basis`dw_zeitdaten within w_taetigkeiten_wf1
integer x = 2967
integer y = 0
integer height = 1796
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

type dw_info from w_taetigkeiten_basis`dw_info within w_taetigkeiten_wf1
integer x = 18
integer width = 2825
end type

type ds_schluessel from w_taetigkeiten_basis`ds_schluessel within w_taetigkeiten_wf1
end type

type cb_ok from commandbutton within w_taetigkeiten_wf1
integer x = 1271
integer y = 780
integer width = 407
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
long ll_zeit
string ls_schluessel, ls_umlage, ls_kst, ls_zusatz1, ls_zusatz2, ls_zusatz
date ldt_tag

IF dw_tk_single.AcceptText () <> 1 THEN return

IF dw_tk_single.RowCount () > 0 THEN
	ls_schluessel = dw_tk_single.getItemString(1, "schluessel")
	ls_umlage = dw_tk_single.getItemString(1, "u_p")
	ls_zusatz1 = dw_tk_single.getItemString(1, "zusatz1")
	ls_zusatz2 = dw_tk_single.getItemString(1, "zusatz2")
	ls_zusatz = dw_tk_single.getItemString(1, "zusatz")
	ls_kst = dw_tk_single.getItemString(1, "von_kst")
	ldt_tag = Date (dw_tk_single.getItemDateTime(1, "tag"))
	ll_zeit = dw_tk_single.getItemNumber(1, "zeit")
	
	if isNull(ls_umlage) or trim (ls_umlage) = "" then
		dw_tk_single.setFocus()
		CHOOSE CASE ls_schluessel
			CASE "p"
				dw_tk_single.setColumn("u_p")
			CASE "b"
				dw_tk_single.setColumn("u_b")
			CASE "f"
				dw_tk_single.setColumn("u_f")
		END CHOOSE
		Messagebox("Unvollständige Eingabe", "Die Umlage muss angegeben werden!")
		dw_tk_single.SetFocus ()
		return
		
	elseif (ls_schluessel = "p" OR ls_schluessel = 'b') AND (IsNull (ls_zusatz) OR Trim (ls_zusatz) = '') then
		Messagebox("Unvollständige Eingabe", "Wählen Sie bitte zur Umlage auch die Art der Tätigkeit aus!")

	elseif IsNull (ls_kst) OR ls_kst = "" then
		Messagebox("Unvollständige Eingabe", "Geben Sie bitte Ihre Kostenstelle an!")
		dw_tk_single.setColumn("von_kst")
		dw_tk_single.SetFocus ()
		return

	elseif IsNull (ldt_tag) then
		Messagebox("Unvollständige Eingabe", "Der Tag muss angegeben werden!")
		dw_tk_single.setColumn("tag")
		dw_tk_single.SetFocus ()
		return
		
	elseif ldt_tag < 2002-01-01 or ldt_tag > Today() then
		Messagebox("Fehlerhafte Eingabe", "Der angegebene Tag liegt vor 2002 oder in der Zukunft!")
		dw_tk_single.setColumn("tag")
		dw_tk_single.SetFocus ()
		return
		
	elseif IsNull (ll_zeit) then
		Messagebox("Unvollständige Eingabe", "Die Arbeitszeit muss angegeben werden!")
		dw_tk_single.setColumn("zeit_h")
		dw_tk_single.SetFocus ()
		return
	
	elseif dw_tk_single.getItemString(1, "zusatz1") = 'Stichprobenauswahl' and &
				ls_umlage <> 'WF1_GAG' then
		Messagebox("Fehlerhafte Eingabe", "Die Stichprobenauswahl kann momentan nur für GA ausgewählt werden!")
		dw_tk_single.setColumn("zusatz1")
		dw_tk_single.SetFocus ()
		return

	end if
	
	// Prüfung, ob alle verfügbaren Zusatzfelder gefüllt sind
	if ls_schluessel = 'f' then
		// Zusatz nur wenn sichtbar
		if (IsNull (ls_zusatz) or trim (ls_zusatz) = '') and dw_tk_single.object.zusatz_f.visible = '1' then
			Messagebox ("Unvollständige Eingabe", "Bitte machen Sie Angaben zur Tätigkeit!")
			dw_tk_single.setColumn("zusatz_f")
			dw_tk_single.SetFocus ()
			return
		end if
		// Zusatz1 nur wenn sichtbar
		if (IsNull (ls_zusatz1) or trim (ls_zusatz1) = '') and dw_tk_single.object.zusatz1.visible = '1' then
			Messagebox ("Unvollständige Eingabe", "Bitte vervollständigen Sie alle Angaben zur Tätigkeit!")
			dw_tk_single.setColumn("zusatz1")
			dw_tk_single.SetFocus ()
			return
		end if
		// Zusatz2 nur wenn sichtbar
		if (IsNull (ls_zusatz2) or trim (ls_zusatz2) = '') and dw_tk_single.object.zusatz2.visible = '1' then
			Messagebox ("Unvollständige Eingabe", "Bitte vervollständigen Sie alle Angaben zur Tätigkeit!")
			dw_tk_single.setColumn("zusatz2")
			dw_tk_single.SetFocus ()
			return
		end if
	end if
	
	// alle Vorab-Prüfungen i.O.
	// -> Update
	IF wf_update() <> 1 THEN return
	
	post wf_new_umlage()
end if

end event

type dw_tk_single from datawindow within w_taetigkeiten_wf1
event dropdown pbm_dwndropdown
integer x = 18
integer y = 104
integer width = 2939
integer height = 652
integer taborder = 40
string title = "none"
string dataobject = "d_tk_single_wf1"
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
	
	IF ls_umlage <> "" AND NOT IsNull (ls_umlage) THEN
		ll_row = ldwc_current.Find ("umlage='" + ls_umlage + "'", 1, ldwc_current.RowCount())
		IF ll_row < 1 THEN
			SetNull (ls_umlage)
			SetItem (1, ls_colname, ls_umlage)
		ELSE
			ldwc_current.SetRow (ll_row)
		END IF
	END IF
	
END IF

end event

event itemchanged;
DataWindowChild child_dddw
long ll_row


if dwo.name = "tag" then
	id_von = Date (left (data, 10))

	of_modify_schluessel (id_von, dw_tk_single)
	of_load_zeitdaten (id_von)
	
	of_Retrieve()
	return  // kein auto-tab
	
elseif dwo.name = "zeit_h" then
	SetItem (row, "zeit", Dec (data) * 60)
	return  // kein auto-tab

elseif dwo.name = "u_p" or dwo.name = "u_f" or dwo.name = "u_b" then
	// Umlage auswählen
	// und Anzeige des Textes
	if dwo.name = "u_p" then 
		IF GetChild ("zusatz_p", child_dddw) = 1 THEN
			child_dddw.SetTransObject (SQLCA)
			child_dddw.Retrieve (gs_modus, data)
		END IF
		
		// Vorbelegung des Zusatz mit "Testkonzept/ Funktionstest"
		IF NOT ib_loading THEN
			setItem (row, "zusatz_p", "Testkonzept/ Funktionstest")
		END IF
		Modify ("zusatz_f.visible='0' zusatz_p.visible='1' zusatz_b.visible='0'")
		
	elseif dwo.name = "u_b" then 
		Modify ("zusatz_f.visible='0' zusatz_p.visible='0' zusatz_b.visible='1'")
		
	elseif dwo.name = "u_f" then
		Modify ("zusatz_p.visible='0' zusatz_b.visible='0'")
		
		IF NOT ib_loading THEN
			setItem (row, "zusatz_f", " ")
			setItem (row, "zusatz1", " ")
			setItem (row, "zusatz2", " ")
		END IF
		
		IF getChild ("zusatz_f", child_dddw) > 0 THEN
			Modify ("zusatz1.visible='0'")
			Modify ("zusatz2.visible='0'")
			child_dddw.SetTransObject (SQLCA)
			IF child_dddw.Retrieve (gs_modus, 3, data) > 0 THEN
				ll_row = child_dddw.Find ("vorbeleg='J'", 1, child_dddw.RowCount())
				IF ll_row > 0 AND NOT ib_loading THEN
					SetItem (row, "zusatz_f", child_dddw.GetItemString (ll_row, "unter"))
					Post Event itemchanged (row, this.object.zusatz_f, child_dddw.GetItemString (ll_row, "unter"))
				END IF
				Modify ("zusatz_f.visible='1'")
			ELSE
				Modify ("zusatz_f.visible='0'")
			END IF
		END IF
	end if
	

elseif dwo.name = "zusatz_f" then
	IF NOT ib_loading THEN
		setItem (row, "zusatz1", " ")
		setItem (row, "zusatz2", " ")
	END IF
	
	IF getChild ("zusatz1", child_dddw) > 0 THEN
		Modify ("zusatz2.visible='0'")
		child_dddw.SetTransObject (SQLCA)
		IF child_dddw.Retrieve (gs_modus, 4, data) > 0 THEN
			ll_row = child_dddw.Find ("vorbeleg='J'", 1, child_dddw.RowCount())
			IF ll_row > 0 AND NOT ib_loading THEN
				SetItem (row, "zusatz1", child_dddw.GetItemString (ll_row, "unter"))
				Post Event itemchanged (row, this.object.zusatz1, child_dddw.GetItemString (ll_row, "unter"))
			END IF
			Modify ("zusatz1.visible='1'")
		ELSE
			Modify ("zusatz1.visible='0'")
		END IF
	END IF
	
elseif dwo.name = "zusatz1" then
	IF NOT ib_loading THEN
		setItem (row, "zusatz2", " ")
	END IF
	
	IF getChild ("zusatz2", child_dddw) > 0 THEN
		child_dddw.SetTransObject (SQLCA)
		IF child_dddw.Retrieve (gs_modus, 5, data) > 0 THEN
			ll_row = child_dddw.Find ("vorbeleg='J'", 1, child_dddw.RowCount())
			IF ll_row > 0 AND NOT ib_loading THEN
				SetItem (row, "zusatz2", child_dddw.GetItemString (ll_row, "unter"))
			END IF
			Modify ("zusatz2.visible='1'")
		ELSE
			Modify ("zusatz2.visible='0'")
		END IF
	END IF

elseif dwo.name = "schluessel" then
	Modify ("zusatz_p.visible='0'")
	Modify ("zusatz_b.visible='0'")
	Modify ("zusatz_f.visible='0'")
	Modify ("zusatz1.visible='0'")
	Modify ("zusatz2.visible='0'")
	
	IF NOT ib_loading THEN
		setItem (row, "u_p", "")
		setItem (row, "zusatz_p", "")
		setItem (row, "zusatz1", " ")
		setItem (row, "zusatz2", " ")
	END IF
end if

Post Send(Handle(this), 256, 9, Long(0, 0))

end event

event dberror;if sqldbcode = 1 then
	MessageBox("Doppelte Eingabe", "Zwei Tätigkeiten mit gleicher Umlage und gleichem Datum dürfen nicht eingegeben werden") 
	return 1
end if

end event

event constructor;
DataWindowChild ldwc_current


setTransObject(sqlca)

IF GetChild ("u_f", ldwc_current) = 1 THEN
	ldwc_current.SetTransObject (SQLCA)
	ldwc_current.Retrieve (gs_modus)
END IF

IF GetChild ("u_p", ldwc_current) = 1 THEN
	ldwc_current.SetTransObject (SQLCA)
	ldwc_current.Retrieve (gs_modus)
END IF

IF GetChild ("von_kst", ldwc_current) = 1 THEN
	ldwc_current.SetTransObject (SQLCA)
	ldwc_current.Retrieve (gs_modus)
END IF

IF GetChild ("zusatz_p", ldwc_current) = 1 THEN
	ldwc_current.SetTransObject (SQLCA)
	ldwc_current.Retrieve (gs_modus, "")
END IF

end event

event losefocus;
post Accepttext()

end event

type dw_tk_liste from datawindow within w_taetigkeiten_wf1
integer x = 18
integer y = 892
integer width = 2939
integer height = 896
integer taborder = 100
string title = "none"
string dataobject = "d_tk_liste_wf1"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event constructor;setTransObject(sqlca)
end event

event buttonclicking;
IF dw_tk_single.ModifiedCount () > 0 THEN
	IF MessageBox ("Achtung", "Ihre Änderungen wurden noch nicht gespeichert und gehen verloren!", &
						StopSign!, OkCancel!, 2) <> 1 THEN return 1
END IF

wf_rowchanged (row)

IF dwo.name = "b_delete" THEN
	// Löschen
	wf_delete ()
END IF

return 1

end event

type cb_new_umlage from commandbutton within w_taetigkeiten_wf1
integer x = 2085
integer y = 112
integer width = 407
integer height = 84
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "Neue &Tätigkeit"
end type

event clicked;wf_new_umlage()


end event

