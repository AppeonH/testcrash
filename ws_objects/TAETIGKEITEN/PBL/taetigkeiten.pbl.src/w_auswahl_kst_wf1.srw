$PBExportHeader$w_auswahl_kst_wf1.srw
$PBExportComments$Auswahl KST WF1
forward
global type w_auswahl_kst_wf1 from window
end type
type st_2 from statictext within w_auswahl_kst_wf1
end type
type dw_kst from datawindow within w_auswahl_kst_wf1
end type
type cb_ok from commandbutton within w_auswahl_kst_wf1
end type
type cb_cancel from commandbutton within w_auswahl_kst_wf1
end type
end forward

shared variables

end variables

global type w_auswahl_kst_wf1 from window
integer x = 201
integer y = 200
integer width = 1454
integer height = 1584
boolean titlebar = true
string title = "Auswahl Kostenstelle"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 29347791
string icon = "AppIcon!"
boolean center = true
event dfscl_print ( )
st_2 st_2
dw_kst dw_kst
cb_ok cb_ok
cb_cancel cb_cancel
end type
global w_auswahl_kst_wf1 w_auswahl_kst_wf1

type variables

private n_cst_auswertwf1_attrib inv_attrib

end variables

on w_auswahl_kst_wf1.create
this.st_2=create st_2
this.dw_kst=create dw_kst
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.Control[]={this.st_2,&
this.dw_kst,&
this.cb_ok,&
this.cb_cancel}
end on

on w_auswahl_kst_wf1.destroy
destroy(this.st_2)
destroy(this.dw_kst)
destroy(this.cb_ok)
destroy(this.cb_cancel)
end on

event open;
string ls_sql, ls_where, ls_auswahl[]
long ll_pos, ll_i, ll_upper, ll_row, ll_rows

inv_attrib = Message.PowerObjectParm

IF inv_attrib.ib_bezug THEN
	this.title += " als Bezugsgröße"
	ls_auswahl = inv_attrib.is_bezugkst
ELSE
	ls_auswahl = inv_attrib.is_vonkst
END IF

dw_kst.SetTransObject (SQLCA)

// WHERE-Bedingung an Zeitraum anpassen
// für Zeitraum der Einfachheit wegen nur Jahre berücksichtigen
ls_sql = dw_kst.GetSQLSelect ()

ll_upper = UpperBound (inv_attrib.ii_jahre)
ll_rows = UpperBound (inv_attrib.ii_monate)
FOR ll_i = 1 TO ll_upper
	FOR ll_row = 1 TO ll_rows
		IF ls_where <> "" THEN ls_WHERE += " OR "
		ls_where += "'" + String (inv_attrib.ii_jahre[ll_i], "0000") + "/" + String (inv_attrib.ii_monate[ll_row], "00") + &
						"' BETWEEN to_char (von, 'YYYY/MM') and nvl (to_char (bis, 'YYYY/MM'), '9999/99')"
	NEXT
NEXT
IF ls_where <> "" AND NOT IsNull (ls_where) THEN
	ll_pos = Pos (ls_sql, "1=1")
	DO WHILE ll_pos > 0
		ls_sql = Replace (ls_sql, ll_pos, 3, ls_where)
		ll_pos = Pos (ls_sql, "1=1")
	LOOP
END IF

// Benutzername für Berechtigung einsetzen
ll_pos = Pos (ls_sql, "%USERNAME%")
DO WHILE ll_pos > 0
	ls_sql = Replace (ls_sql, ll_pos, 10, gs_username)
	ll_pos = Pos (ls_sql, "%USERNAME%")
LOOP

dw_kst.SetSQLSelect (ls_sql)

dw_kst.SetTransObject (SQLCA)
ll_rows = dw_kst.Retrieve ()

ll_upper = UpperBound (ls_auswahl)
FOR ll_i = 1 TO ll_upper
	ll_row = dw_kst.Find ("umlage='" + ls_auswahl[ll_i] + "'", 1, ll_rows)
	IF ll_row > 0 THEN dw_kst.SelectRow (ll_row, TRUE)
NEXT

end event

type st_2 from statictext within w_auswahl_kst_wf1
integer x = 78
integer y = 1312
integer width = 1303
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

type dw_kst from datawindow within w_auswahl_kst_wf1
event mousemove pbm_dwnmousemove
integer x = 5
integer y = 4
integer width = 1426
integer height = 1300
integer taborder = 10
string title = "none"
string dataobject = "d_auswahl_kst_wf1"
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

type cb_ok from commandbutton within w_auswahl_kst_wf1
integer x = 251
integer y = 1400
integer width = 375
integer height = 88
integer taborder = 20
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "&OK"
boolean default = true
end type

event clicked;
string ls_kst []

ls_kst = dw_kst.object.umlage.selected

IF UpperBound (ls_kst) < 1 THEN
	MessageBox ("Fehler", "Wählen Sie mindestens eine Kostenstelle aus!")
	return
END IF

IF inv_attrib.ib_bezug THEN
	inv_attrib.is_bezugkst = ls_kst
ELSE
	inv_attrib.is_vonkst = ls_kst
END IF

CloseWithReturn (parent, inv_attrib)

end event

type cb_cancel from commandbutton within w_auswahl_kst_wf1
integer x = 800
integer y = 1400
integer width = 375
integer height = 88
integer taborder = 30
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

