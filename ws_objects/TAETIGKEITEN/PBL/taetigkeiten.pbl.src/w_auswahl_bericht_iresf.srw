$PBExportHeader$w_auswahl_bericht_iresf.srw
$PBExportComments$Auswahl Bericht IR-ESF
forward
global type w_auswahl_bericht_iresf from window
end type
type cb_cancel from commandbutton within w_auswahl_bericht_iresf
end type
type cb_ok from commandbutton within w_auswahl_bericht_iresf
end type
type dw_bericht from datawindow within w_auswahl_bericht_iresf
end type
end forward

global type w_auswahl_bericht_iresf from window
integer width = 1070
integer height = 1528
boolean titlebar = true
string title = "Auswahl Bericht"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 29347791
string icon = "AppIcon!"
boolean center = true
cb_cancel cb_cancel
cb_ok cb_ok
dw_bericht dw_bericht
end type
global w_auswahl_bericht_iresf w_auswahl_bericht_iresf

type variables

public s_auswert istr_auswert

end variables

on w_auswahl_bericht_iresf.create
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.dw_bericht=create dw_bericht
this.Control[]={this.cb_cancel,&
this.cb_ok,&
this.dw_bericht}
end on

on w_auswahl_bericht_iresf.destroy
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.dw_bericht)
end on

event open;
long ll_rows, ll_row


istr_auswert = Message.PowerObjectParm

SetPointer (HourGlass!)
dw_bericht.SetTransObject (SQLCA)
ll_rows = dw_bericht.Retrieve ()

ll_row = dw_bericht.Find ("ueber = '" + istr_auswert.is_kst + "' and unter = '" + istr_auswert.is_projekt + "'", 1, ll_rows)
IF ll_row > 0 THEN 
	dw_bericht.SelectRow (ll_row, TRUE)
	dw_bericht.Expand (ll_row, 1)
	dw_bericht.ScrollToRow (ll_row)
END IF

end event

type cb_cancel from commandbutton within w_auswahl_bericht_iresf
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

type cb_ok from commandbutton within w_auswahl_bericht_iresf
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
long ll_row


ll_row = dw_bericht.GetSelectedRow (0)
IF ll_row < 1 THEN
	MessageBox ("Achtung", "Bitte wählen Sie einen Bericht aus!")
	return
END IF


istr_auswert.is_kst = dw_bericht.GetItemString (ll_row, "ueber")
istr_auswert.is_projekt = dw_bericht.GetItemString (ll_row, "unter")

CloseWithReturn (parent, istr_auswert)

end event

type dw_bericht from datawindow within w_auswahl_bericht_iresf
event mousemove pbm_dwnmousemove
integer x = 23
integer y = 20
integer width = 1019
integer height = 1276
integer taborder = 10
string title = "none"
string dataobject = "d_auswahl_bericht_iresf"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;IF row > 0 THEN
	SelectRow (0, FALSE)
	SelectRow (row, TRUE)
END IF

end event

