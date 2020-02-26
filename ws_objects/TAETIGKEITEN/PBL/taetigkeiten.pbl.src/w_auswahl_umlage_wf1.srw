$PBExportHeader$w_auswahl_umlage_wf1.srw
$PBExportComments$Auswahl Umlage WF1
forward
global type w_auswahl_umlage_wf1 from window
end type
type dw_schluessel from datawindow within w_auswahl_umlage_wf1
end type
type st_ebene from statictext within w_auswahl_umlage_wf1
end type
type cb_prev from commandbutton within w_auswahl_umlage_wf1
end type
type cb_next from commandbutton within w_auswahl_umlage_wf1
end type
type dw_zusatz2 from datawindow within w_auswahl_umlage_wf1
end type
type dw_zusatz1 from datawindow within w_auswahl_umlage_wf1
end type
type dw_zusatz from datawindow within w_auswahl_umlage_wf1
end type
type st_info from statictext within w_auswahl_umlage_wf1
end type
type dw_umlage from datawindow within w_auswahl_umlage_wf1
end type
type cb_ok from commandbutton within w_auswahl_umlage_wf1
end type
type cb_cancel from commandbutton within w_auswahl_umlage_wf1
end type
end forward

shared variables

end variables

global type w_auswahl_umlage_wf1 from window
integer x = 201
integer y = 200
integer width = 1275
integer height = 1416
boolean titlebar = true
string title = "Auswahl Tätigkeit"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 29347791
string icon = "AppIcon!"
boolean center = true
event dfscl_print ( )
dw_schluessel dw_schluessel
st_ebene st_ebene
cb_prev cb_prev
cb_next cb_next
dw_zusatz2 dw_zusatz2
dw_zusatz1 dw_zusatz1
dw_zusatz dw_zusatz
st_info st_info
dw_umlage dw_umlage
cb_ok cb_ok
cb_cancel cb_cancel
end type
global w_auswahl_umlage_wf1 w_auswahl_umlage_wf1

type variables

private n_cst_auswertwf1_attrib inv_attrib
private boolean ib_first[4] = {TRUE, TRUE, TRUE, TRUE}

end variables

forward prototypes
protected function long of_retrieve (integer ai_ebene)
end prototypes

protected function long of_retrieve (integer ai_ebene);
long ll_rows
string ls_sql, ls_where, ls_schl, ls_auswahl[], ls_auswahl1[], ls_aktuell[]
long ll_pos, ll_i, ll_upper, ll_j, ll_upper2

IF ai_ebene = 0 THEN
	// Schlüssel -> hier kein Retrieve, nur Vorbelegung
	ll_rows = dw_schluessel.RowCount ()
	ll_upper = UpperBound (inv_attrib.is_schluessel)
	FOR ll_i = 1 TO ll_upper
		ll_pos = dw_schluessel.Find ("schluessel = '" + inv_attrib.is_schluessel[ll_i] + "'", 1, ll_rows) 
		IF ll_pos > 0 THEN dw_schluessel.SelectRow (ll_pos, TRUE)
	NEXT
	
ELSEIF ai_ebene = 1 THEN
	IF ib_first[1] THEN
		ib_first[1] = FALSE
		ls_aktuell = inv_attrib.is_umlagen
	ELSE
		ls_aktuell = dw_umlage.object.umlage.selected
	END IF
	
	dw_umlage.dataobject = dw_umlage.dataobject  // Original-SQL zurücksetzen
	
	ls_auswahl = dw_schluessel.object.schluessel.selected
	IF UpperBound (ls_auswahl) < 1 THEN 
		dw_umlage.Reset ()
		return 0
	END IF

	ll_upper = UpperBound (ls_auswahl)
	FOR ll_i = 1 TO ll_upper
		IF ls_schl <> "" THEN ls_schl += ", "
		ls_schl += "'" + string (ls_auswahl[ll_i]) + "'"
	NEXT
	
	
	dw_umlage.SetTransObject (SQLCA)
	
	// WHERE-Bedingung an Zeitraum anpassen
	ls_sql = dw_umlage.GetSQLSelect ()
	
	ll_pos = Pos (ls_sql, "'%SCHLÜSSEL%'")
	DO WHILE ll_pos > 0
		ls_sql = Left (ls_sql, ll_pos - 1) + ls_schl + Mid (ls_sql, ll_pos + 13)
		ll_pos = Pos (ls_sql, "'%SCHLÜSSEL%'")
	LOOP
	
	ll_upper = UpperBound (inv_attrib.ii_jahre)
	ll_upper2 = UpperBound (inv_attrib.ii_monate)
	FOR ll_i = 1 TO ll_upper
		FOR ll_j = 1 TO ll_upper2
			IF ls_where <> "" THEN ls_WHERE += " OR "
			ls_where += "'" + String (inv_attrib.ii_jahre[ll_i], "0000") + String (inv_attrib.ii_monate[ll_j], "00") + "' BETWEEN to_char (von, 'YYYYMM') and nvl (to_char (bis, 'YYYYMM'), '999912')"
		NEXT
	NEXT
	
	IF ls_where <> "" AND NOT IsNull (ls_where) THEN
		ll_pos = Pos (ls_sql, "1=1")
		DO WHILE ll_pos > 0
			ls_sql = Left (ls_sql, ll_pos - 1) + ls_where + Mid (ls_sql, ll_pos + 3)
			ll_pos = Pos (ls_sql, "1=1")
		LOOP
	END IF
	
	dw_umlage.SetSQLSelect (ls_sql)
	
	dw_umlage.SetTransObject (SQLCA)
	ll_rows = dw_umlage.Retrieve ()

	ll_upper = UpperBound (ls_aktuell)
	FOR ll_i = 1 TO ll_upper
		ll_pos = dw_umlage.Find ("umlage = '" + ls_aktuell[ll_i] + "'", 1, ll_rows) 
		IF ll_pos > 0 THEN dw_umlage.SelectRow (ll_pos, TRUE)
	NEXT

	// abhängige DWs neu laden, damit da nicht alte ungültige Auswahlen stehen bleiben
	of_retrieve (2)

ELSEIF ai_ebene = 2 THEN
	IF ib_first[2] THEN
		ib_first[2] = FALSE
		ls_aktuell = inv_attrib.is_zusatz
	ELSE
		ls_aktuell = dw_zusatz.object.unter.selected
	END IF
	
	ls_auswahl = dw_umlage.object.umlage.selected
	ls_auswahl1 = dw_umlage.object.schluessel.selected
	IF UpperBound (ls_auswahl) < 1 THEN 
		dw_zusatz.Reset ()
		return 0
	END IF
	
	dw_zusatz.SetTransObject (SQLCA)
	ll_rows = dw_zusatz.Retrieve (gs_modus, 3, ls_auswahl, ls_auswahl1)

	ll_upper = UpperBound (ls_aktuell)
	FOR ll_i = 1 TO ll_upper
		ll_pos = dw_zusatz.Find ("unter = '" + ls_aktuell[ll_i] + "'", 1, ll_rows) 
		IF ll_pos > 0 THEN dw_zusatz.SelectRow (ll_pos, TRUE)
	NEXT
	
	// abhängige DWs neu laden, damit da nicht alte ungültige Auswahlen stehen bleiben
	of_retrieve (3)
	
ELSEIF ai_ebene = 3 THEN
	IF ib_first[3] THEN
		ib_first[3] = FALSE
		ls_aktuell = inv_attrib.is_zusatz1
	ELSE
		ls_aktuell = dw_zusatz1.object.unter.selected
	END IF
	
	ls_auswahl = dw_zusatz.object.unter.selected
	IF UpperBound (ls_auswahl) < 1 THEN 
		dw_zusatz1.Reset ()
		return 0
	END IF
	
	dw_zusatz1.SetTransObject (SQLCA)
	ll_rows = dw_zusatz1.Retrieve (gs_modus, 4, ls_auswahl)

	ll_upper = UpperBound (ls_aktuell)
	FOR ll_i = 1 TO ll_upper
		ll_pos = dw_zusatz1.Find ("unter = '" + ls_aktuell[ll_i] + "'", 1, ll_rows) 
		IF ll_pos > 0 THEN dw_zusatz1.SelectRow (ll_pos, TRUE)
	NEXT

	// abhängige DWs neu laden, damit da nicht alte ungültige Auswahlen stehen bleiben
	of_retrieve (4)
	
ELSEIF ai_ebene = 4 THEN
	IF ib_first[4] THEN
		ib_first[4] = FALSE
		ls_aktuell = inv_attrib.is_zusatz2
	ELSE
		ls_aktuell = dw_zusatz2.object.unter.selected
	END IF
	
	ls_auswahl = dw_zusatz1.object.unter.selected
	IF UpperBound (ls_auswahl) < 1 THEN 
		dw_zusatz2.Reset ()
		return 0
	END IF
	
	dw_zusatz2.SetTransObject (SQLCA)
	ll_rows = dw_zusatz2.Retrieve (gs_modus, 5, ls_auswahl)

	ll_upper = UpperBound (ls_aktuell)
	FOR ll_i = 1 TO ll_upper
		ll_pos = dw_zusatz2.Find ("unter = '" + ls_aktuell[ll_i] + "'", 1, ll_rows) 
		IF ll_pos > 0 THEN dw_zusatz2.SelectRow (ll_pos, TRUE)
	NEXT
END IF

return ll_rows

end function

on w_auswahl_umlage_wf1.create
this.dw_schluessel=create dw_schluessel
this.st_ebene=create st_ebene
this.cb_prev=create cb_prev
this.cb_next=create cb_next
this.dw_zusatz2=create dw_zusatz2
this.dw_zusatz1=create dw_zusatz1
this.dw_zusatz=create dw_zusatz
this.st_info=create st_info
this.dw_umlage=create dw_umlage
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.Control[]={this.dw_schluessel,&
this.st_ebene,&
this.cb_prev,&
this.cb_next,&
this.dw_zusatz2,&
this.dw_zusatz1,&
this.dw_zusatz,&
this.st_info,&
this.dw_umlage,&
this.cb_ok,&
this.cb_cancel}
end on

on w_auswahl_umlage_wf1.destroy
destroy(this.dw_schluessel)
destroy(this.st_ebene)
destroy(this.cb_prev)
destroy(this.cb_next)
destroy(this.dw_zusatz2)
destroy(this.dw_zusatz1)
destroy(this.dw_zusatz)
destroy(this.st_info)
destroy(this.dw_umlage)
destroy(this.cb_ok)
destroy(this.cb_cancel)
end on

event open;
inv_attrib = Message.PowerObjectParm

of_retrieve (0)

end event

type dw_schluessel from datawindow within w_auswahl_umlage_wf1
event mousemove pbm_dwnmousemove
integer x = 5
integer y = 4
integer width = 1248
integer height = 992
integer taborder = 10
string title = "none"
string dataobject = "d_auswahl_schluessel_wf1"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event mousemove;
if row < 1 then return

IF KeyDown (KeyLeftButton!) THEN
	SelectRow (row, TRUE)
ELSEIF KeyDown (KeyRightButton!) THEN
	SelectRow (row, FALSE)
END IF


end event

event clicked;
if row < 1 then return

SelectRow (row, TRUE)

end event

event rbuttondown;
if row < 1 then return

SelectRow (row, FALSE)

end event

type st_ebene from statictext within w_auswahl_umlage_wf1
integer x = 503
integer y = 1100
integer width = 219
integer height = 48
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
long textcolor = 33554432
long backcolor = 29347791
string text = "Ebene"
alignment alignment = center!
boolean focusrectangle = false
end type

type cb_prev from commandbutton within w_auswahl_umlage_wf1
boolean visible = false
integer x = 288
integer y = 1088
integer width = 210
integer height = 76
integer taborder = 30
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "<<"
end type

event clicked;
IF dw_umlage.visible THEN
	dw_umlage.visible = FALSE
	dw_schluessel.visible = TRUE
	cb_prev.visible = FALSE
ELSEIF dw_zusatz.visible THEN
	dw_zusatz.visible = FALSE
	dw_umlage.visible = TRUE
ELSEIF dw_zusatz1.visible THEN
	dw_zusatz1.visible = FALSE
	dw_zusatz.visible = TRUE
ELSEIF dw_zusatz2.visible THEN
	dw_zusatz.visible = FALSE
	dw_zusatz1.visible = TRUE
	cb_next.visible = TRUE
END IF

end event

type cb_next from commandbutton within w_auswahl_umlage_wf1
integer x = 731
integer y = 1088
integer width = 210
integer height = 76
integer taborder = 30
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = ">>"
end type

event clicked;
IF dw_schluessel.visible THEN
	IF of_retrieve (1) < 1 THEN
		MessageBox ("Hinweis", "Für Ihre Auswahl stehen keine Detailinformationen zur Verfügung!")
		return
	END IF
	dw_schluessel.visible = FALSE
	dw_umlage.visible = TRUE
	cb_prev.visible = TRUE
ELSEIF dw_umlage.visible THEN
	IF of_retrieve (2) < 1 THEN
		MessageBox ("Hinweis", "Für Ihre Auswahl stehen keine Detailinformationen zur Verfügung!")
		return
	END IF
	dw_umlage.visible = FALSE
	dw_zusatz.visible = TRUE
ELSEIF dw_zusatz.visible THEN
	IF of_retrieve (3) < 1 THEN
		MessageBox ("Hinweis", "Für Ihre Auswahl stehen keine Detailinformationen zur Verfügung!")
		return
	END IF
	dw_zusatz.visible = FALSE
	dw_zusatz1.visible = TRUE
ELSEIF dw_zusatz1.visible THEN
	IF of_retrieve (4) < 1 THEN
		MessageBox ("Hinweis", "Für Ihre Auswahl stehen keine Detailinformationen zur Verfügung!")
		return
	END IF
	dw_zusatz1.visible = FALSE
	dw_zusatz2.visible = TRUE
	cb_next.visible = FALSE
END IF

end event

type dw_zusatz2 from datawindow within w_auswahl_umlage_wf1
event mousemove pbm_dwnmousemove
boolean visible = false
integer x = 5
integer y = 4
integer width = 1248
integer height = 992
integer taborder = 20
string title = "none"
string dataobject = "d_auswahl_zusatz1_wf1"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event mousemove;
if row < 1 then return

IF KeyDown (KeyLeftButton!) THEN
	SelectRow (row, TRUE)
ELSEIF KeyDown (KeyRightButton!) THEN
	SelectRow (row, FALSE)
END IF


end event

event clicked;
if row < 1 then return

SelectRow (row, TRUE)

end event

event rbuttondown;
if row < 1 then return

SelectRow (row, FALSE)

end event

type dw_zusatz1 from datawindow within w_auswahl_umlage_wf1
event mousemove pbm_dwnmousemove
boolean visible = false
integer x = 5
integer y = 4
integer width = 1248
integer height = 992
integer taborder = 20
string title = "none"
string dataobject = "d_auswahl_zusatz1_wf1"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event mousemove;
if row < 1 then return

IF KeyDown (KeyLeftButton!) THEN
	SelectRow (row, TRUE)
ELSEIF KeyDown (KeyRightButton!) THEN
	SelectRow (row, FALSE)
END IF


end event

event clicked;
if row < 1 then return

SelectRow (row, TRUE)

end event

event rbuttondown;
if row < 1 then return

SelectRow (row, FALSE)

end event

type dw_zusatz from datawindow within w_auswahl_umlage_wf1
event mousemove pbm_dwnmousemove
boolean visible = false
integer x = 5
integer y = 4
integer width = 1248
integer height = 992
integer taborder = 10
string title = "none"
string dataobject = "d_auswahl_zusatz_wf1"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event mousemove;
if row < 1 then return

IF KeyDown (KeyLeftButton!) THEN
	SelectRow (row, TRUE)
ELSEIF KeyDown (KeyRightButton!) THEN
	SelectRow (row, FALSE)
END IF


end event

event clicked;
if row < 1 then return

SelectRow (row, TRUE)

end event

event rbuttondown;
if row < 1 then return

SelectRow (row, FALSE)

end event

type st_info from statictext within w_auswahl_umlage_wf1
integer x = 5
integer y = 1012
integer width = 1262
integer height = 56
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
long textcolor = 33554432
long backcolor = 29347791
string text = "Linksklick: Auswahl / Rechtsklick: Auswahl aufheben"
alignment alignment = center!
boolean focusrectangle = false
end type

type dw_umlage from datawindow within w_auswahl_umlage_wf1
event mousemove pbm_dwnmousemove
boolean visible = false
integer x = 5
integer y = 4
integer width = 1248
integer height = 992
integer taborder = 10
string title = "none"
string dataobject = "d_auswahl_umlage_wf1"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event mousemove;
if row < 1 then return

IF KeyDown (KeyLeftButton!) THEN
	SelectRow (row, TRUE)
ELSEIF KeyDown (KeyRightButton!) THEN
	SelectRow (row, FALSE)
END IF


end event

event clicked;
if row < 1 then return

SelectRow (row, TRUE)

end event

event rbuttondown;
if row < 1 then return

SelectRow (row, FALSE)

end event

type cb_ok from commandbutton within w_auswahl_umlage_wf1
integer x = 146
integer y = 1220
integer width = 375
integer height = 88
integer taborder = 90
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "&OK"
boolean default = true
end type

event clicked;
string ls_schluessel []

ls_schluessel = dw_schluessel.object.schluessel.selected
IF UpperBound (ls_schluessel) < 1 THEN
	MessageBox ("Fehler", "Wählen Sie mindestens aus, ob Sie für Projekte, Beratungen und/oder Förderprogramme Daten auswerten möchten!")
	return
END IF

inv_attrib.is_schluessel = ls_schluessel
inv_attrib.is_umlagen = dw_umlage.object.umlage.selected
inv_attrib.is_zusatz = dw_zusatz.object.unter.selected
inv_attrib.is_zusatz1 = dw_zusatz1.object.unter.selected
inv_attrib.is_zusatz2 = dw_zusatz2.object.unter.selected

CloseWithReturn (parent, inv_attrib)

end event

type cb_cancel from commandbutton within w_auswahl_umlage_wf1
integer x = 695
integer y = 1220
integer width = 375
integer height = 88
boolean bringtotop = true
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "&Abbrechen"
boolean cancel = true
end type

event clicked;close(parent)

end event

