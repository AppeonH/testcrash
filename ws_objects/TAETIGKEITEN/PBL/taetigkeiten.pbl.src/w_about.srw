$PBExportHeader$w_about.srw
$PBExportComments$About Fenster
forward
global type w_about from window
end type
type st_problem from statictext within w_about
end type
type p_bild from picture within w_about
end type
type st_vers from statictext within w_about
end type
type cb_ok from commandbutton within w_about
end type
type shl_mail from statichyperlink within w_about
end type
end forward

global type w_about from window
integer x = 873
integer y = 428
integer width = 1166
integer height = 904
boolean titlebar = true
string title = "Über"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 79416533
string icon = "AppIcon!"
st_problem st_problem
p_bild p_bild
st_vers st_vers
cb_ok cb_ok
shl_mail shl_mail
end type
global w_about w_about

on w_about.create
this.st_problem=create st_problem
this.p_bild=create p_bild
this.st_vers=create st_vers
this.cb_ok=create cb_ok
this.shl_mail=create shl_mail
this.Control[]={this.st_problem,&
this.p_bild,&
this.st_vers,&
this.cb_ok,&
this.shl_mail}
end on

on w_about.destroy
destroy(this.st_problem)
destroy(this.p_bild)
destroy(this.st_vers)
destroy(this.cb_ok)
destroy(this.shl_mail)
end on

type st_problem from statictext within w_about
integer x = 169
integer y = 524
integer width = 805
integer height = 76
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
long backcolor = 79416533
string text = "Bei Fragen und Problemen:"
alignment alignment = center!
boolean focusrectangle = false
end type

type p_bild from picture within w_about
integer x = 366
integer y = 140
integer width = 411
integer height = 360
boolean originalsize = true
string picturename = "..\ress\about.gif"
boolean focusrectangle = false
end type

type st_vers from statictext within w_about
integer x = 233
integer y = 64
integer width = 672
integer height = 72
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
long backcolor = 79416533
string text = "Zeiterfassung Version 14.2"
alignment alignment = center!
boolean focusrectangle = false
end type

type cb_ok from commandbutton within w_about
integer x = 407
integer y = 700
integer width = 334
integer height = 88
integer taborder = 10
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "OK"
boolean default = true
end type

event clicked;//*-----------------------------------------------------------------*/
//*    clicked:  Close "About" Window
//*-----------------------------------------------------------------*/
Close ( Parent )
end event

type shl_mail from statichyperlink within w_about
integer x = 187
integer y = 588
integer width = 773
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
boolean underline = true
string pointer = "HyperLink!"
long textcolor = 16711680
long backcolor = 79416533
string text = "rene.ullrich@aufbaubank.de"
alignment alignment = center!
boolean focusrectangle = false
string url = "mailto:rene.ullrich@aufbaubank.de"
end type

