$PBExportHeader$w_auswahl_zeitraum.srw
$PBExportComments$Auswahl Zeitraum
forward
global type w_auswahl_zeitraum from window
end type
type uo_zeitraum from u_auswahl_zeitraum within w_auswahl_zeitraum
end type
type cb_ok from commandbutton within w_auswahl_zeitraum
end type
type cb_cancel from commandbutton within w_auswahl_zeitraum
end type
end forward

shared variables

end variables

global type w_auswahl_zeitraum from window
integer x = 201
integer y = 200
integer width = 1445
integer height = 364
boolean titlebar = true
string title = "Auswahl Zeitraum"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 29347791
string icon = "AppIcon!"
event dfscl_print ( )
uo_zeitraum uo_zeitraum
cb_ok cb_ok
cb_cancel cb_cancel
end type
global w_auswahl_zeitraum w_auswahl_zeitraum

type variables
public s_auswert istr_auswert

end variables

on w_auswahl_zeitraum.create
this.uo_zeitraum=create uo_zeitraum
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.Control[]={this.uo_zeitraum,&
this.cb_ok,&
this.cb_cancel}
end on

on w_auswahl_zeitraum.destroy
destroy(this.uo_zeitraum)
destroy(this.cb_ok)
destroy(this.cb_cancel)
end on

event open;
istr_auswert = Message.PowerObjectParm
SetNull (Message.PowerObjectParm)

uo_zeitraum.of_SetZeitraum (istr_auswert.id_von, istr_auswert.id_bis)

end event

type uo_zeitraum from u_auswahl_zeitraum within w_auswahl_zeitraum
event destroy ( )
integer x = 14
integer y = 8
integer taborder = 20
end type

on uo_zeitraum.destroy
call u_auswahl_zeitraum::destroy
end on

type cb_ok from commandbutton within w_auswahl_zeitraum
integer x = 238
integer y = 156
integer width = 375
integer height = 88
integer taborder = 20
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "&OK"
boolean default = true
end type

event clicked;
uo_zeitraum.of_GetZeitraum (istr_auswert.id_von, istr_auswert.id_bis)

CloseWithReturn (parent, istr_auswert)

end event

type cb_cancel from commandbutton within w_auswahl_zeitraum
integer x = 786
integer y = 156
integer width = 375
integer height = 88
integer taborder = 30
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "&Abbrechen"
boolean cancel = true
end type

event clicked;close(parent)
end event

