$PBExportHeader$w_auswahl.srw
$PBExportComments$Auswahl aus einer Liste
forward
global type w_auswahl from window
end type
type cb_cancel from commandbutton within w_auswahl
end type
type cb_ok from commandbutton within w_auswahl
end type
type dw_auswahl from datawindow within w_auswahl
end type
end forward

global type w_auswahl from window
integer width = 2542
integer height = 1364
boolean titlebar = true
string title = "Auswahl"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 29347791
string icon = "AppIcon!"
boolean center = true
cb_cancel cb_cancel
cb_ok cb_ok
dw_auswahl dw_auswahl
end type
global w_auswahl w_auswahl

type variables

protected s_auswahl istr_auswahl

end variables

on w_auswahl.create
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.dw_auswahl=create dw_auswahl
this.Control[]={this.cb_cancel,&
this.cb_ok,&
this.dw_auswahl}
end on

on w_auswahl.destroy
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.dw_auswahl)
end on

event open;
istr_auswahl = Message.PowerObjectParm

dw_auswahl.DataObject = istr_auswahl.dataobject
dw_auswahl.SetTransObject (SQLCA)
dw_auswahl.Retrieve (gs_modus)

end event

type cb_cancel from commandbutton within w_auswahl
integer x = 1330
integer y = 1136
integer width = 402
integer height = 112
integer taborder = 30
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "Abbrechen"
boolean cancel = true
end type

event clicked;
CloseWithReturn (parent, "")

end event

type cb_ok from commandbutton within w_auswahl
integer x = 672
integer y = 1136
integer width = 402
integer height = 112
integer taborder = 20
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "Übernehmen"
boolean default = true
end type

event clicked;
long ll_row
string ls_return

ll_row = dw_auswahl.GetSelectedRow (0)
IF ll_row < 1 THEN
	MessageBox ("Hinweis", "Wählen Sie bitte eine Zeile aus!")
	return
END IF

ls_return = dw_auswahl.GetItemString (ll_row, istr_auswahl.return_column)

CloseWithReturn (parent, ls_return)

end event

type dw_auswahl from datawindow within w_auswahl
integer x = 23
integer y = 24
integer width = 2487
integer height = 1080
integer taborder = 10
string title = "none"
boolean hscrollbar = true
boolean vscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;
IF row > 0 THEN
	SelectRow (0, FALSE)
	SelectRow (row, TRUE)
END IF

end event

event doubleclicked;
cb_ok.Event clicked ()

end event

