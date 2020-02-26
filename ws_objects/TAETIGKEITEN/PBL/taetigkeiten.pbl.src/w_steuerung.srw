$PBExportHeader$w_steuerung.srw
$PBExportComments$Eingabe von Steuerungsdaten (Customizing)
forward
global type w_steuerung from window
end type
type st_1 from statictext within w_steuerung
end type
type em_von from editmask within w_steuerung
end type
type cb_close from commandbutton within w_steuerung
end type
type cb_save from commandbutton within w_steuerung
end type
end forward

global type w_steuerung from window
integer width = 1093
integer height = 436
boolean titlebar = true
string title = "Customizing"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 29347791
string icon = "AppIcon!"
boolean center = true
st_1 st_1
em_von em_von
cb_close cb_close
cb_save cb_save
end type
global w_steuerung w_steuerung

on w_steuerung.create
this.st_1=create st_1
this.em_von=create em_von
this.cb_close=create cb_close
this.cb_save=create cb_save
this.Control[]={this.st_1,&
this.em_von,&
this.cb_close,&
this.cb_save}
end on

on w_steuerung.destroy
destroy(this.st_1)
destroy(this.em_von)
destroy(this.cb_close)
destroy(this.cb_save)
end on

event open;
string ls_von
long ll_count

// Berechtigung
select count (*) 
into :ll_count
from intdba.org_recht 
where username = :gs_username 
AND art = 'TK_CUST'
and recht = 'cust'
using sqlca; 

IF ll_count < 1 THEN
	MessageBox ("Hinweis", "Sie sind nicht berechtigt, diese Daten zu bearbeiten!")
	close (this)
	return
END IF

select wert
into :ls_von
from intdba.steuerung
where schluessel = 'BUCHPER_AB'
using sqlca;

em_von.Text = ls_von

end event

type st_1 from statictext within w_steuerung
integer y = 20
integer width = 1079
integer height = 60
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
long textcolor = 33554432
long backcolor = 553648127
string text = "Ab welchem Tag sind Buchungen möglich?"
alignment alignment = center!
boolean focusrectangle = false
end type

type em_von from editmask within w_steuerung
integer x = 379
integer y = 92
integer width = 325
integer height = 88
integer taborder = 10
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd.mm.yyyy"
end type

type cb_close from commandbutton within w_steuerung
integer x = 571
integer y = 212
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

type cb_save from commandbutton within w_steuerung
integer x = 87
integer y = 212
integer width = 402
integer height = 100
integer taborder = 20
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "&Speichern"
boolean default = true
end type

event clicked;
date ld_von
string ls_von

IF em_von.GetData (ld_von) <> 1 THEN
	MessageBox ("Fehler", "Geben Sie bitte ein gültiges Datum ein!")
	return
END IF

ls_von = String (ld_von, "dd.mm.yyyy")

update intdba.steuerung
set wert = :ls_von
where schluessel = 'BUCHPER_AB'
USING SQLCA;

commit;

close (parent)

end event

