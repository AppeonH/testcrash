$PBExportHeader$w_steuerung_wf1.srw
$PBExportComments$Eingabe von Steuerungsdaten (Customizing) für WF1
forward
global type w_steuerung_wf1 from window
end type
type st_1 from statictext within w_steuerung_wf1
end type
type em_von from editmask within w_steuerung_wf1
end type
type cb_close from commandbutton within w_steuerung_wf1
end type
type cb_save from commandbutton within w_steuerung_wf1
end type
end forward

global type w_steuerung_wf1 from window
integer width = 1088
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
global w_steuerung_wf1 w_steuerung_wf1

on w_steuerung_wf1.create
this.st_1=create st_1
this.em_von=create em_von
this.cb_close=create cb_close
this.cb_save=create cb_save
this.Control[]={this.st_1,&
this.em_von,&
this.cb_close,&
this.cb_save}
end on

on w_steuerung_wf1.destroy
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
and recht = 'cust_WF1'
using sqlca; 

IF ll_count < 1 THEN
	MessageBox ("Hinweis", "Sie sind nicht berechtigt, diese Daten zu bearbeiten!")
	close (this)
	return
END IF

select wert
into :ls_von
from intdba.steuerung
where schluessel = 'BUCHPER_AB_WF1'
using sqlca;

em_von.Text = ls_von

end event

type st_1 from statictext within w_steuerung_wf1
integer y = 16
integer width = 1083
integer height = 64
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

type em_von from editmask within w_steuerung_wf1
integer x = 370
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

type cb_close from commandbutton within w_steuerung_wf1
integer x = 558
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

type cb_save from commandbutton within w_steuerung_wf1
integer x = 73
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
where schluessel = 'BUCHPER_AB_WF1'
USING SQLCA;

commit;

close (parent)

end event

