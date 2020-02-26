$PBExportHeader$w_idle.srw
$PBExportComments$Fenster nach Ablauf Idle-Zeit
forward
global type w_idle from window
end type
type cb_close from commandbutton within w_idle
end type
type st_4 from statictext within w_idle
end type
type st_time from statictext within w_idle
end type
type st_3 from statictext within w_idle
end type
type st_2 from statictext within w_idle
end type
type st_1 from statictext within w_idle
end type
type st_info from statictext within w_idle
end type
type cb_ok from commandbutton within w_idle
end type
end forward

global type w_idle from window
integer x = 873
integer y = 428
integer width = 1070
integer height = 752
boolean titlebar = true
string title = "Über"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 79416533
string icon = "AppIcon!"
cb_close cb_close
st_4 st_4
st_time st_time
st_3 st_3
st_2 st_2
st_1 st_1
st_info st_info
cb_ok cb_ok
end type
global w_idle w_idle

type variables

private long il_time = 60

end variables

on w_idle.create
this.cb_close=create cb_close
this.st_4=create st_4
this.st_time=create st_time
this.st_3=create st_3
this.st_2=create st_2
this.st_1=create st_1
this.st_info=create st_info
this.cb_ok=create cb_ok
this.Control[]={this.cb_close,&
this.st_4,&
this.st_time,&
this.st_3,&
this.st_2,&
this.st_1,&
this.st_info,&
this.cb_ok}
end on

on w_idle.destroy
destroy(this.cb_close)
destroy(this.st_4)
destroy(this.st_time)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.st_info)
destroy(this.cb_ok)
end on

event open;
Timer (1)

st_time.Text = String (il_time, "0")

end event

event timer;
il_time --

IF il_time < 0 THEN 
	close (this)
	HALT CLOSE
	return
END IF

st_time.Text = String (il_time, "0")

end event

type cb_close from commandbutton within w_idle
integer x = 576
integer y = 540
integer width = 334
integer height = 88
integer taborder = 10
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "Abbrechen"
end type

event clicked;//*-----------------------------------------------------------------*/
//*    clicked:  Close "About" Window
//*-----------------------------------------------------------------*/
Close ( Parent )
end event

type st_4 from statictext within w_idle
integer x = 18
integer y = 396
integer width = 1019
integer height = 72
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
long backcolor = 79416533
string text = "Änderungen werden nicht gespeichert!"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_time from statictext within w_idle
integer x = 101
integer y = 256
integer width = 389
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
long backcolor = 79416533
string text = "60"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_3 from statictext within w_idle
integer x = 18
integer y = 316
integer width = 1019
integer height = 72
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
long backcolor = 79416533
string text = "geschlossen."
alignment alignment = center!
boolean focusrectangle = false
end type

type st_2 from statictext within w_idle
integer x = 507
integer y = 256
integer width = 475
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
long backcolor = 79416533
string text = "Sekunden"
boolean focusrectangle = false
end type

type st_1 from statictext within w_idle
integer x = 18
integer y = 196
integer width = 1019
integer height = 72
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
long backcolor = 79416533
string text = "Die Anwendung wird automatisch in"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_info from statictext within w_idle
integer x = 18
integer y = 16
integer width = 1019
integer height = 120
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
long backcolor = 79416533
string text = "Sie haben das Zeiterfassungsprogramm seit einiger Zeit nicht mehr verwendet"
alignment alignment = center!
boolean focusrectangle = false
end type

type cb_ok from commandbutton within w_idle
integer x = 146
integer y = 540
integer width = 334
integer height = 88
integer taborder = 10
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "Beenden"
end type

event clicked;
close (parent)
HALT CLOSE

end event

