$PBExportHeader$w_auswahl_zeitraum_wf1.srw
$PBExportComments$Auswahl Zeiträume WF1
forward
global type w_auswahl_zeitraum_wf1 from window
end type
type st_info from statictext within w_auswahl_zeitraum_wf1
end type
type cb_cancel from commandbutton within w_auswahl_zeitraum_wf1
end type
type cb_ok from commandbutton within w_auswahl_zeitraum_wf1
end type
type dw_monat from datawindow within w_auswahl_zeitraum_wf1
end type
type st_1 from statictext within w_auswahl_zeitraum_wf1
end type
type st_jahr from statictext within w_auswahl_zeitraum_wf1
end type
type dw_jahr from datawindow within w_auswahl_zeitraum_wf1
end type
end forward

global type w_auswahl_zeitraum_wf1 from window
integer width = 1056
integer height = 1528
boolean titlebar = true
string title = "Auswahl Zeitraum"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 29347791
string icon = "AppIcon!"
boolean center = true
st_info st_info
cb_cancel cb_cancel
cb_ok cb_ok
dw_monat dw_monat
st_1 st_1
st_jahr st_jahr
dw_jahr dw_jahr
end type
global w_auswahl_zeitraum_wf1 w_auswahl_zeitraum_wf1

type variables

protected n_cst_auswertwf1_attrib inv_attrib

end variables

on w_auswahl_zeitraum_wf1.create
this.st_info=create st_info
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.dw_monat=create dw_monat
this.st_1=create st_1
this.st_jahr=create st_jahr
this.dw_jahr=create dw_jahr
this.Control[]={this.st_info,&
this.cb_cancel,&
this.cb_ok,&
this.dw_monat,&
this.st_1,&
this.st_jahr,&
this.dw_jahr}
end on

on w_auswahl_zeitraum_wf1.destroy
destroy(this.st_info)
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.dw_monat)
destroy(this.st_1)
destroy(this.st_jahr)
destroy(this.dw_jahr)
end on

event open;
long ll_i, ll_upper, ll_rows, ll_row
string ls_sql

inv_attrib = Message.PowerObjectParm

IF inv_attrib.is_username = "" THEN
	dw_jahr.DataObject = "d_auswahl_jahr"
END IF

SetPointer (HourGlass!)
dw_jahr.SetTransObject (SQLCA)
IF inv_attrib.is_username <> "" THEN
	ll_rows = dw_jahr.Retrieve (inv_attrib.is_username)
ELSE
	ls_sql = dw_jahr.GetSQLSelect ()
	dw_jahr.SetSQLSelect (ls_sql + " WHERE (von_kst LIKE '3%' or von_kst LIKE '8%')")
	
	ll_rows = dw_jahr.Retrieve ()
END IF

ll_upper = UpperBound (inv_attrib.ii_jahre)
FOR ll_i = 1 TO ll_upper
	ll_row = dw_jahr.Find ("jahr=" + string (inv_attrib.ii_jahre[ll_i], "0"), 1, ll_rows)
	IF ll_row > 0 THEN 
		dw_jahr.SelectRow (ll_row, TRUE)
		IF NOT inv_attrib.ib_zeitmehrfach THEN EXIT
	END IF
NEXT

ll_upper = UpperBound (inv_attrib.ii_monate)
FOR ll_i = 1 TO ll_upper
	ll_row = dw_monat.Find ("monat=" + string (inv_attrib.ii_monate[ll_i], "0"), 1, 12)
	IF ll_row > 0 THEN 
		dw_monat.SelectRow (ll_row, TRUE)
		IF NOT inv_attrib.ib_zeitmehrfach THEN EXIT
	END IF
NEXT

end event

type st_info from statictext within w_auswahl_zeitraum_wf1
integer y = 1248
integer width = 1061
integer height = 56
integer textsize = -7
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
long textcolor = 33554432
long backcolor = 29347791
string text = "Linksklick: Auswahl / Rechtsklick: Auswahl aufheben"
alignment alignment = center!
boolean focusrectangle = false
end type

type cb_cancel from commandbutton within w_auswahl_zeitraum_wf1
integer x = 539
integer y = 1320
integer width = 361
integer height = 100
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "A&bbrechen"
boolean cancel = true
end type

event clicked;
Close (parent)

end event

type cb_ok from commandbutton within w_auswahl_zeitraum_wf1
integer x = 155
integer y = 1320
integer width = 343
integer height = 100
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "&OK"
boolean default = true
end type

event clicked;
integer li_jahre [], li_monate []

li_jahre = dw_jahr.object.jahr.selected
li_monate = dw_monat.object.monat.selected

IF UpperBound (li_jahre) < 1 OR UpperBound (li_monate) < 1 THEN
	MessageBox ("Fehler", "Wählen Sie mindestens ein Jahr und einen Monat aus!")
	return
END IF

inv_attrib.ii_jahre = li_jahre
inv_attrib.ii_monate = li_monate

CloseWithReturn (parent, inv_attrib)

end event

type dw_monat from datawindow within w_auswahl_zeitraum_wf1
event mousemove pbm_dwnmousemove
integer x = 535
integer y = 84
integer width = 480
integer height = 1156
integer taborder = 20
string title = "none"
string dataobject = "d_auswahl_monat"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event mousemove;
if row < 1 then return

IF KeyDown (KeyLeftButton!) THEN
	IF NOT inv_attrib.ib_zeitmehrfach THEN SelectRow (0, FALSE)
	SelectRow (row, TRUE)
ELSEIF KeyDown (KeyRightButton!) THEN
	SelectRow (row, FALSE)
END IF


end event

event clicked;
if row < 1 then return

IF NOT inv_attrib.ib_zeitmehrfach THEN SelectRow (0, FALSE)
SelectRow (row, TRUE)

end event

event rbuttondown;
if row < 1 then return

SelectRow (row, FALSE)

end event

type st_1 from statictext within w_auswahl_zeitraum_wf1
integer x = 535
integer y = 20
integer width = 402
integer height = 64
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
long textcolor = 33554432
long backcolor = 29347791
string text = "Monat"
boolean focusrectangle = false
end type

type st_jahr from statictext within w_auswahl_zeitraum_wf1
integer x = 23
integer y = 20
integer width = 402
integer height = 64
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
long textcolor = 33554432
long backcolor = 29347791
string text = "Jahr"
boolean focusrectangle = false
end type

type dw_jahr from datawindow within w_auswahl_zeitraum_wf1
event mousemove pbm_dwnmousemove
integer x = 23
integer y = 84
integer width = 480
integer height = 1156
integer taborder = 10
string title = "none"
string dataobject = "d_auswahl_jahr_user"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event mousemove;
if row < 1 then return

IF KeyDown (KeyLeftButton!) THEN
	IF NOT inv_attrib.ib_zeitmehrfach THEN SelectRow (0, FALSE)
	SelectRow (row, TRUE)
ELSEIF KeyDown (KeyRightButton!) THEN
	SelectRow (row, FALSE)
END IF


end event

event clicked;
if row < 1 then return

IF NOT inv_attrib.ib_zeitmehrfach THEN SelectRow (0, FALSE)
SelectRow (row, TRUE)

end event

event rbuttondown;
if row < 1 then return

SelectRow (row, FALSE)

end event

