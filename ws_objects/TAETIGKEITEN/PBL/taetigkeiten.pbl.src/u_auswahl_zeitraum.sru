$PBExportHeader$u_auswahl_zeitraum.sru
$PBExportComments$Baustein für Zeitraum-Auswahl
forward
global type u_auswahl_zeitraum from userobject
end type
type st_strich from statictext within u_auswahl_zeitraum
end type
type ddlb_zeitraum from dropdownlistbox within u_auswahl_zeitraum
end type
type st_von from statictext within u_auswahl_zeitraum
end type
type st_bis from statictext within u_auswahl_zeitraum
end type
end forward

global type u_auswahl_zeitraum from userobject
integer width = 1399
integer height = 108
long backcolor = 553648127
string text = "none"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
event modified ( date ad_von,  date ad_bis )
st_strich st_strich
ddlb_zeitraum ddlb_zeitraum
st_von st_von
st_bis st_bis
end type
global u_auswahl_zeitraum u_auswahl_zeitraum

type variables

private date id_von
private date id_bis

end variables

forward prototypes
public function integer of_setzeitraum (date ad_von, date ad_bis)
public subroutine of_getzeitraum (ref date ad_von, ref date ad_bis)
protected subroutine of_standardzeit (integer index)
end prototypes

event modified(date ad_von, date ad_bis);
// Event wird getriggert, wenn sich die Auswahl des Zeitraum (auch durch Eingabe) geändert hat

end event

public function integer of_setzeitraum (date ad_von, date ad_bis);
IF IsNull (ad_von) OR IsNull (ad_bis) OR ad_bis < ad_von THEN return -1

id_von = ad_von
id_bis = ad_bis

ddlb_zeitraum.SelectItem (0)
ddlb_zeitraum.Text = "Zeitraum auswählen"
st_von.text = string (id_von, "dd.mm.yyyy")
st_bis.text = string (id_bis, "dd.mm.yyyy")

return 1

end function

public subroutine of_getzeitraum (ref date ad_von, ref date ad_bis);
ad_von = id_von
ad_bis = id_bis

end subroutine

protected subroutine of_standardzeit (integer index);datetime ldtm_von, ldtm_bis
date ld_date
n_cst_datetime lnv_datetime


ld_date = today()

CHOOSE CASE index
	CASE 1
		select min(tag) 
		into :ldtm_von 
		from intdba.tk
		using sqlca;
		
		select max(tag) 
		into :ldtm_bis 
		from intdba.tk
		using sqlca;
		
		if ldtm_bis < DateTime (Date (today())) then ldtm_bis = DateTime (Date (today()))
		id_von = Date (ldtm_von)
		id_bis = Date (ldtm_bis)
		
	CASE 2
		id_von = Date (Year(ld_date), Month(ld_date), 1)
		id_bis = lnv_datetime.of_LastDayOfMonth (ld_date)
		
	CASE 3
		id_von = Date (Year(ld_date), 1, 1)
		id_bis = Date (Year(ld_date), 12, 31)
		
	CASE 4
		id_von = lnv_datetime.of_RelativeMonth (ld_date, -1)
		id_von = Date (Year(id_von), Month(id_von), 1)
		id_bis = lnv_datetime.of_LastDayOfMonth (id_von)
		
	CASE 5
		id_von = Date (Year(ld_date) - 1, 1, 1)
		id_bis = Date (Year(ld_date) - 1, 12, 31)
		
	CASE 6
		id_von = RelativeDate (ld_date, -30)
		id_bis = ld_date
		
	CASE 7
		id_von = lnv_datetime.of_RelativeMonth (ld_date, -3)
		id_bis = ld_date
		
	CASE 8
		id_von = lnv_datetime.of_RelativeMonth (ld_date, -6)
		id_bis = ld_date
		
	CASE 9
		id_von = lnv_datetime.of_RelativeMonth (ld_date, -12)
		id_bis = ld_date
		
END CHOOSE

st_von.text = string (id_von, "dd.mm.yyyy")
st_bis.text = string (id_bis, "dd.mm.yyyy")

Event modified (id_von, id_bis)

end subroutine

on u_auswahl_zeitraum.create
this.st_strich=create st_strich
this.ddlb_zeitraum=create ddlb_zeitraum
this.st_von=create st_von
this.st_bis=create st_bis
this.Control[]={this.st_strich,&
this.ddlb_zeitraum,&
this.st_von,&
this.st_bis}
end on

on u_auswahl_zeitraum.destroy
destroy(this.st_strich)
destroy(this.ddlb_zeitraum)
destroy(this.st_von)
destroy(this.st_bis)
end on

event constructor;
of_setzeitraum (id_von, id_bis)

end event

type st_strich from statictext within u_auswahl_zeitraum
integer x = 969
integer y = 24
integer width = 59
integer height = 60
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
long textcolor = 33554432
long backcolor = 553648127
string text = "-"
alignment alignment = center!
long bordercolor = 33554432
boolean focusrectangle = false
end type

type ddlb_zeitraum from dropdownlistbox within u_auswahl_zeitraum
string tag = "Zeitraum auswäh&len"
integer x = 9
integer y = 8
integer width = 594
integer height = 720
integer taborder = 10
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
long textcolor = 33554432
string text = "Zeitraum auswählen"
boolean allowedit = true
boolean autohscroll = true
boolean sorted = false
string item[] = {"Alle Daten","Laufender Monat","Laufendes Jahr","Vormonat","Vorjahr","Letzte 30 Tage","Letzte 3 Monate","Letzte 6 Monate","Letzte 12 Monate"}
integer accelerator = 108
borderstyle borderstyle = stylelowered!
end type

event modified;long ll_pos
datetime ldtm_von, ldtm_bis
string ls_text, ls_von, ls_bis
string ls_msg = "Wählen Sie bitte einen Zeitraum aus oder~r~n" + &
					"geben Sie ein Datum (TT.MM.JJJJ), ~r~n" + &
					"einen Monat (MM/JJJJ), ein Jahr (JJJJ) oder~r~n" + &
					"eine Zeitspanne (zwei Werte durch - getrennt) an!"
n_cst_datetime lnv_datetime


ls_text = this.Text
IF Trim (ls_text) = "" THEN ls_text = this.Item [1]
ll_pos = this.FindItem (ls_text, 0)

IF ll_pos > 0 THEN
	this.SelectItem (ll_pos)
	of_standardzeit (ll_pos)
	return
END IF

ll_pos = Pos (ls_text, "-")
IF ll_pos > 0 THEN
	// Angabe Zeitspanne (von/bis)
	// von und bis können ein Datum, Monat oder Jahr sein
	ls_von = Trim (Left (ls_text, ll_pos - 1))
	ls_bis = Trim (Mid (ls_text, ll_pos + 1))
	
	// Mindestens eines der Werte muss vorhanden sein
	IF ls_von = "" AND ls_bis = "" THEN
		MessageBox ("Achtung", ls_msg)
		return
	END IF
	
	// VON
	IF ls_von = "" THEN 
		select min(tag) 
		into :ldtm_von 
		from intdba.tk
		using sqlca;
		
	ELSEIF IsDate (ls_von) THEN
		// Angabe ist ein Datum
		ldtm_von = DateTime (Date (ls_von))
		
	ELSEIF Pos (ls_von, "/") > 0  THEN
		// Angabe ist ein Monat (MM/JJJJ)
		ll_pos = Pos (ls_von, "/")
		ls_von = "01." + Replace (ls_von, ll_pos, 1, ".")
		IF IsDate (ls_von) THEN
			ldtm_von = DateTime (Date (ls_von))
		ELSE
			MessageBox ("Achtung", ls_msg)
			return
		END IF
		
	ELSEIF Long (ls_von) >= 1990 and Long (ls_von) <= 2099 THEN
		// Angabe ist ein Jahr (JJJJ)
		ldtm_von = DateTime (Date (Long (ls_von), 1, 1))
		
	ELSE
		MessageBox ("Achtung", ls_msg)
		return
	END IF

	
	// BIS
	IF ls_bis = "" THEN 
		select max(tag) 
		into :ldtm_bis 
		from intdba.tk
		using sqlca;
		
		if ldtm_bis < Datetime (Date (today())) then ldtm_bis = Datetime (Date (today()))
		
	ELSEIF IsDate (ls_bis) THEN
		// Angabe ist ein Datum
		ldtm_bis = DateTime (Date (ls_bis))
		
	ELSEIF Pos (ls_bis, "/") > 0  THEN
		// Angabe ist ein Monat (MM/JJJJ)
		ll_pos = Pos (ls_bis, "/")
		ls_bis = "1." + Replace (ls_bis, ll_pos, 1, ".")
		IF IsDate (ls_bis) THEN
			ldtm_bis = DateTime (lnv_datetime.of_LastDayOfMonth (Date (ls_bis)))
		ELSE
			MessageBox ("Achtung", ls_msg)
			return
		END IF
		
	ELSEIF Long (ls_bis) >= 1990 and Long (ls_bis) <= 2099 THEN
		// Angabe ist ein Jahr (JJJJ)
		ldtm_bis = DateTime (Date (Long (ls_bis), 12, 31))
		
	ELSE
		MessageBox ("Achtung", ls_msg)
		return
	END IF
	
ELSEIF IsDate (ls_text) THEN
	// Angabe ist ein Datum
	ldtm_von = DateTime (Date (ls_text))
	ldtm_bis = ldtm_von
	
ELSEIF Pos (ls_text, "/") > 0  THEN
	// Angabe ist ein Monat (MM/JJJJ)
	ls_text = Trim (ls_text)
	ll_pos = Pos (ls_text, "/")
	ls_text = "01." + Replace (ls_text, ll_pos, 1, ".")
	IF IsDate (ls_text) THEN
		ldtm_von = DateTime (Date (ls_text))
		ldtm_bis = DateTime (lnv_datetime.of_LastDayOfMonth (Date (ldtm_von)))
	ELSE
		MessageBox ("Achtung", ls_msg)
		return
	END IF
	
ELSEIF Long (ls_text) >= 1990 and Long (ls_text) <= 2099 THEN
	// Angabe ist ein Jahr (JJJJ)
	ldtm_von = DateTime (Date (Long (ls_text), 1, 1))
	ldtm_bis = DateTime (Date (Long (ls_text), 12, 31))
	
ELSE
	MessageBox ("Achtung", ls_msg)
	return
END IF

id_von = Date (ldtm_von)
id_bis = Date (ldtm_bis)

st_von.text = string (id_von, "dd.mm.yyyy")
st_bis.text = string (id_bis, "dd.mm.yyyy")

parent.Event modified (id_von, id_bis)

end event

type st_von from statictext within u_auswahl_zeitraum
integer x = 622
integer y = 24
integer width = 343
integer height = 60
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
long textcolor = 33554432
long backcolor = 553648127
string text = "von"
alignment alignment = right!
long bordercolor = 33554432
boolean focusrectangle = false
end type

type st_bis from statictext within u_auswahl_zeitraum
integer x = 1033
integer y = 24
integer width = 343
integer height = 60
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
long textcolor = 33554432
long backcolor = 553648127
string text = "bis"
long bordercolor = 33554432
boolean focusrectangle = false
end type

