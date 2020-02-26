$PBExportHeader$w_pflege_beratungsmandate.srw
$PBExportComments$Pflege der Beratungsmandate
forward
global type w_pflege_beratungsmandate from window
end type
type dw_daten from datawindow within w_pflege_beratungsmandate
end type
type cb_close from commandbutton within w_pflege_beratungsmandate
end type
end forward

global type w_pflege_beratungsmandate from window
integer width = 4283
integer height = 1656
boolean titlebar = true
string title = "Beratungsmandate"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 29347791
string icon = "AppIcon!"
boolean center = true
dw_daten dw_daten
cb_close cb_close
end type
global w_pflege_beratungsmandate w_pflege_beratungsmandate

type variables

end variables

on w_pflege_beratungsmandate.create
this.dw_daten=create dw_daten
this.cb_close=create cb_close
this.Control[]={this.dw_daten,&
this.cb_close}
end on

on w_pflege_beratungsmandate.destroy
destroy(this.dw_daten)
destroy(this.cb_close)
end on

event open;long ll_count


// Berechtigung
select count (*) 
into :ll_count
from intdba.org_recht 
where username = :gs_username 
AND art = 'TK_CUST'
and recht = 'cust_berat'
using sqlca; 

IF ll_count < 1 THEN
	MessageBox ("Hinweis", "Sie sind nicht berechtigt, diese Daten zu bearbeiten!")
	close (this)
	return
END IF

dw_daten.SetTransObject (SQLCA)
dw_daten.Retrieve ()

end event

type dw_daten from datawindow within w_pflege_beratungsmandate
integer x = 18
integer y = 16
integer width = 4238
integer height = 1412
integer taborder = 10
string title = "none"
string dataobject = "d_beratungsprojekte"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event buttonclicked;
CHOOSE CASE lower (dwo.name)
	CASE "b_add"
		// siehe buttonclicking event
		
	CASE "b_edit"
		// siehe auch doubleclicked event
		IF row < 1 THEN return
		OpenWithParm (w_pflege_beratungsprojekt, GetItemString (row, "umlage"))
		IF Message.DoubleParm = 1 THEN post Retrieve ()
END CHOOSE

end event

event buttonclicking;
CHOOSE CASE lower (dwo.name)
	CASE "b_add"
		OpenWithParm (w_pflege_beratungsprojekt, "")
		IF Message.DoubleParm = 1 THEN post Retrieve ()
		return 1 // wir wollen die Default-Aktion hier nicht ausführen
		
	CASE "b_edit"
		// siehe buttonclicked event
END CHOOSE

end event

event doubleclicked;
IF row < 1 THEN return
OpenWithParm (w_pflege_beratungsprojekt, GetItemString (row, "umlage"))
IF Message.DoubleParm = 1 THEN post Retrieve ()

end event

type cb_close from commandbutton within w_pflege_beratungsmandate
integer x = 1934
integer y = 1456
integer width = 402
integer height = 100
integer taborder = 30
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "S&chließen"
boolean cancel = true
end type

event clicked;
close (parent)

end event

