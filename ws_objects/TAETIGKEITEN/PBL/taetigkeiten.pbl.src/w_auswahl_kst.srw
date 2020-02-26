$PBExportHeader$w_auswahl_kst.srw
$PBExportComments$Auswahl KST
forward
global type w_auswahl_kst from window
end type
type dw_kst from datawindow within w_auswahl_kst
end type
type cb_ok from commandbutton within w_auswahl_kst
end type
type cb_cancel from commandbutton within w_auswahl_kst
end type
end forward

shared variables

end variables

global type w_auswahl_kst from window
integer x = 201
integer y = 200
integer width = 1477
integer height = 356
boolean titlebar = true
string title = "Auswahl Umlage"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 29347791
string icon = "AppIcon!"
event dfscl_print ( )
dw_kst dw_kst
cb_ok cb_ok
cb_cancel cb_cancel
end type
global w_auswahl_kst w_auswahl_kst

type variables

private s_auswert istr_auswert

end variables

on w_auswahl_kst.create
this.dw_kst=create dw_kst
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.Control[]={this.dw_kst,&
this.cb_ok,&
this.cb_cancel}
end on

on w_auswahl_kst.destroy
destroy(this.dw_kst)
destroy(this.cb_ok)
destroy(this.cb_cancel)
end on

event open;
istr_auswert = Message.PowerObjectParm
SetNull (Message.PowerObjectParm)

dw_kst.SetItem (1, "kst", istr_auswert.is_kst)

end event

type dw_kst from datawindow within w_auswahl_kst
integer x = 5
integer y = 20
integer width = 1463
integer height = 116
integer taborder = 10
string title = "none"
string dataobject = "d_auswahl_kst"
boolean border = false
end type

event constructor;
SetTransObject (SQLCA)
InsertRow (0)

end event

type cb_ok from commandbutton within w_auswahl_kst
integer x = 261
integer y = 156
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
string ls_return

IF dw_kst.Accepttext() <> 1 THEN return

ls_return = dw_kst.GetItemString (1, "kst")
IF IsNull (ls_return) THEN
	MessageBox ("Achtung", "Bitte wählen Sie eine Umlage aus oder geben Sie einen LIKE-Ausdruck an!")
	return
END IF

istr_auswert.is_kst = ls_return

CloseWithReturn (parent, istr_auswert)

end event

type cb_cancel from commandbutton within w_auswahl_kst
integer x = 809
integer y = 156
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

