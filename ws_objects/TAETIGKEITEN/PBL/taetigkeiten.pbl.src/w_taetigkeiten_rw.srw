$PBExportHeader$w_taetigkeiten_rw.srw
$PBExportComments$Tätigkeitenerfassung für Fachbereich Rechnungswesen
forward
global type w_taetigkeiten_rw from w_taetigkeiten_basis
end type
type uo_zeitraum from u_auswahl_zeitraum within w_taetigkeiten_rw
end type
type cb_wechsel from commandbutton within w_taetigkeiten_rw
end type
type cb_ok from commandbutton within w_taetigkeiten_rw
end type
type dw_tk_single from datawindow within w_taetigkeiten_rw
end type
type dw_tk_liste_cross from datawindow within w_taetigkeiten_rw
end type
type dw_tk_liste_grid from datawindow within w_taetigkeiten_rw
end type
type cb_new_umlage from commandbutton within w_taetigkeiten_rw
end type
type cb_delete from commandbutton within w_taetigkeiten_rw
end type
end forward

shared variables

end variables

global type w_taetigkeiten_rw from w_taetigkeiten_basis
integer width = 3008
integer height = 2128
string menuname = "m_taetigkeiten_rw"
event dfscl_print ( )
uo_zeitraum uo_zeitraum
cb_wechsel cb_wechsel
cb_ok cb_ok
dw_tk_single dw_tk_single
dw_tk_liste_cross dw_tk_liste_cross
dw_tk_liste_grid dw_tk_liste_grid
cb_new_umlage cb_new_umlage
cb_delete cb_delete
end type
global w_taetigkeiten_rw w_taetigkeiten_rw

type variables

end variables

forward prototypes
protected subroutine of_retrieve ()
protected function integer wf_calc_zeit ()
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
	
	ldtm_tag = dw_tk_single.getItemDatetime(dw_tk_single.getrow(), "tag")
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
dw_tk_single.setColumn ("zeit_str")

if ll_row > 0 then
	dw_tk_single.scrollToRow(ll_row)

	of_modify_schluessel (ld_date, dw_tk_single)
	
	// Vorbelegung: Name, Datum, Schlüssel, Tag
	dw_tk_single.setItem (ll_row, "username", is_username)
	dw_tk_single.setItem (ll_row, "schluessel", "p")
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
string ls_kst, ls_von, ls_schluessel, ls_umlage
date ld_tag, ld_tag_orig
date ld_von, ld_bis
long ll_count
datetime ldtm_tag
integer li_rc
decimal {0} ldc_id


IF dw_tk_single.AcceptText() = 1 THEN
	IF dw_tk_single.RowCount () > 0 THEN
		// prüfen, ob Datum in geöffneter Periode
		ld_tag = Date (dw_tk_single.GetItemDateTime (1, "tag"))
		ld_tag_orig = Date (dw_tk_single.GetItemDateTime (1, "tag", Primary!, TRUE))
		
		SELECT wert
		INTO :ls_von
		FROM intdba.steuerung
		WHERE schluessel = 'BUCHPER_AB'
		USING SQLCA;

		IF ld_tag < DATE (ls_von) OR ld_tag_orig < DATE (ls_von) THEN
			MessageBox (This.Title, "Das angegebene Datum liegt außerhalb der geöffneten Buchungsperiode!~r~n" + &
								"Buchungen vor dem " + ls_von + " sind nicht möglich!~r~n" + &
								"Falls Sie rückwirkend Änderungen vornehmen möchten, wenden Sie sich bitte an das Controlling.", StopSign!)
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

on w_taetigkeiten_rw.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_taetigkeiten_rw" then this.MenuID = create m_taetigkeiten_rw
this.uo_zeitraum=create uo_zeitraum
this.cb_wechsel=create cb_wechsel
this.cb_ok=create cb_ok
this.dw_tk_single=create dw_tk_single
this.dw_tk_liste_cross=create dw_tk_liste_cross
this.dw_tk_liste_grid=create dw_tk_liste_grid
this.cb_new_umlage=create cb_new_umlage
this.cb_delete=create cb_delete
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_zeitraum
this.Control[iCurrent+2]=this.cb_wechsel
this.Control[iCurrent+3]=this.cb_ok
this.Control[iCurrent+4]=this.dw_tk_single
this.Control[iCurrent+5]=this.dw_tk_liste_cross
this.Control[iCurrent+6]=this.dw_tk_liste_grid
this.Control[iCurrent+7]=this.cb_new_umlage
this.Control[iCurrent+8]=this.cb_delete
end on

on w_taetigkeiten_rw.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.uo_zeitraum)
destroy(this.cb_wechsel)
destroy(this.cb_ok)
destroy(this.dw_tk_single)
destroy(this.dw_tk_liste_cross)
destroy(this.dw_tk_liste_grid)
destroy(this.cb_new_umlage)
destroy(this.cb_delete)
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

type dw_zeitdaten from w_taetigkeiten_basis`dw_zeitdaten within w_taetigkeiten_rw
integer x = 2967
integer y = 0
integer height = 1940
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

type dw_info from w_taetigkeiten_basis`dw_info within w_taetigkeiten_rw
integer x = 41
integer width = 2898
end type

type ds_schluessel from w_taetigkeiten_basis`ds_schluessel within w_taetigkeiten_rw
end type

type uo_zeitraum from u_auswahl_zeitraum within w_taetigkeiten_rw
event destroy ( )
integer x = 1431
integer y = 484
integer taborder = 80
end type

on uo_zeitraum.destroy
call u_auswahl_zeitraum::destroy
end on

event modified;call super::modified;
of_retrieve ()

end event

type cb_wechsel from commandbutton within w_taetigkeiten_rw
integer x = 567
integer y = 500
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

type cb_ok from commandbutton within w_taetigkeiten_rw
integer x = 91
integer y = 500
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
string ls_umlage, ls_kst, ls_schluessel, ls_zusatz
date ldt_tag
datetime ldtm_tag
long ll_sum

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
				ls_schluessel = dw_tk_single.getItemString(1, "schluessel")
				ls_umlage = dw_tk_single.getItemString(1, "u_p")
				ls_zusatz = dw_tk_single.getItemString(1, "zusatz")
				ls_kst = dw_tk_single.getItemString(1, "von_kst")
				ldt_tag = Date (dw_tk_single.getItemDateTime(1, "tag"))
				if isNull(ls_umlage) or trim (ls_umlage) = "" then
					dw_tk_single.setFocus()
					CHOOSE CASE ls_schluessel
						CASE "p"
							dw_tk_single.setColumn("u_p")
						CASE "b"
							dw_tk_single.setColumn("u_b")
						CASE "k"
							dw_tk_single.setColumn("u_k")
						CASE "e"
							dw_tk_single.setColumn("u_e")
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
					
				elseif ldt_tag < 2002-01-01 then
					Messagebox("Fehlerhafte Eingabe", "Der angegebene Tag liegt vor 2002!")
					
				elseif ldt_tag > Today() and ls_umlage <> "URLAUB" and ls_umlage <> "GLEITZEIT" and ls_umlage <> "SCHULUNG" then
					Messagebox("Fehlerhafte Eingabe", "Der angegebene Tag liegt vor 2002 oder in der Zukunft!")
					
				else
					IF wf_update() <> 1 THEN return
					if dw_tk_liste_grid.visible then
						wf_rowchanged(dw_tk_liste_grid.rowCount())
					end if
					
					ldtm_tag = datetime (ldt_tag)
					SELECT sum (zeit)
					INTO :ll_sum
					FROM intdba.tk
					WHERE tag = :ldtm_tag
					AND username = :is_username
					USING SQLCA; 
					IF ll_sum > 600 THEN
						MessageBox ("Achtung", "Die zulässige Höchstarbeitszeit beträgt " + &
												"10 h/Tag. Liegt für Ihre Überschreitung die erforderliche Genehmigung vor?~r~n" + &
												"Wenn nicht, dann reduzieren Sie bitte Ihre Eingabe!", Information!, Ok!)
					END IF
					
					wf_new_umlage()
				end if
			end if
	END CHOOSE
end if

end event

type dw_tk_single from datawindow within w_taetigkeiten_rw
event dropdown pbm_dwndropdown
integer x = 41
integer y = 104
integer width = 2898
integer height = 376
integer taborder = 40
string title = "none"
string dataobject = "d_tk_single_rw"
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
date ldt_tag

if dwo.name = "tag" then
	IF data <> "" THEN
		ldt_tag = Date (left (data, 10))
		
		of_modify_schluessel (ldt_tag, dw_tk_single)
		of_load_zeitdaten (ldt_tag)
		
		IF DayNumber (ldt_tag) = 1 OR DayNumber (ldt_tag) = 7 THEN
			MessageBox ("Achtung", "Sie geben eine Arbeitszeit am Wochenende ein! Liegt dafür die erforderliche Genehmigung vor?")
		END IF

	END IF
	
elseif dwo.name = "u_p" or dwo.name = "u_e" or dwo.name = "u_k" or dwo.name = "u_s" or dwo.name = "u_b"  then
	// Vorbelegung des Projekt-Zusatz mit "Testkonzept/ Funktionstest"
	if dwo.name = "u_p" then 
		IF GetChild ("zusatz_p", child_dddw) = 1 THEN
			child_dddw.SetTransObject (SQLCA)
			child_dddw.Retrieve (gs_modus, data)
		END IF
		
		setItem (row, "zusatz", "Testkonzept/ Funktionstest")
	end if
	
	// Anzeige des Textes
	IF getChild (string(dwo.name), child_dddw) > 0 THEN
		Modify ("t_lang.text = '" + child_dddw.getItemstring(child_dddw.getrow(), "lang") + "'")
	END IF

elseif dwo.name = "zusatz" then
	// Anzeige des Textes
	IF getChild (string(dwo.name), child_dddw) > 0 THEN
		Modify ("t_zusatz_lang.text = '" + child_dddw.getItemstring(child_dddw.getrow(), "text") + "'")
	END IF
	
elseif dwo.name = "schluessel" then
	IF GetChild ("zusatz_p", child_dddw) = 1 THEN
		child_dddw.SetTransObject (SQLCA)
		child_dddw.Retrieve (gs_modus, "")
	END IF
	
	setItem (row, "zusatz", "")
	setItem (row, "u_p", "")
	Modify ("t_lang.text = ''")
	Modify ("t_zusatz_lang.text = ''")
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

IF GetChild ("u_e", ldwc_current) = 1 THEN
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

IF GetChild ("u_s", ldwc_current) = 1 THEN
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

type dw_tk_liste_cross from datawindow within w_taetigkeiten_rw
integer x = 18
integer y = 604
integer width = 2935
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

type dw_tk_liste_grid from datawindow within w_taetigkeiten_rw
boolean visible = false
integer x = 18
integer y = 604
integer width = 2935
integer height = 896
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
//	dw_tk_liste_grid.setSort (ls_dwo)
//	dw_tk_liste_grid.Sort ()
end if

dw_tk_liste_grid.setFocus()

end event

event rowfocuschanged;wf_rowchanged(currentrow)

end event

event constructor;setTransObject(sqlca)
end event

type cb_new_umlage from commandbutton within w_taetigkeiten_rw
integer x = 2135
integer y = 88
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

type cb_delete from commandbutton within w_taetigkeiten_rw
boolean visible = false
integer x = 2528
integer y = 88
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

