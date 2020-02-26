$PBExportHeader$w_pflege_beratungsprojekt.srw
$PBExportComments$Pflege eines Beratungsprojektes
forward
global type w_pflege_beratungsprojekt from window
end type
type dw_zusatz from datawindow within w_pflege_beratungsprojekt
end type
type dw_umlage from datawindow within w_pflege_beratungsprojekt
end type
type cb_close from commandbutton within w_pflege_beratungsprojekt
end type
type cb_save from commandbutton within w_pflege_beratungsprojekt
end type
end forward

global type w_pflege_beratungsprojekt from window
integer width = 2606
integer height = 968
boolean titlebar = true
string title = "Bearbeitung Beratungsmandat"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 29347791
string icon = "AppIcon!"
boolean center = true
dw_zusatz dw_zusatz
dw_umlage dw_umlage
cb_close cb_close
cb_save cb_save
end type
global w_pflege_beratungsprojekt w_pflege_beratungsprojekt

type variables

end variables

on w_pflege_beratungsprojekt.create
this.dw_zusatz=create dw_zusatz
this.dw_umlage=create dw_umlage
this.cb_close=create cb_close
this.cb_save=create cb_save
this.Control[]={this.dw_zusatz,&
this.dw_umlage,&
this.cb_close,&
this.cb_save}
end on

on w_pflege_beratungsprojekt.destroy
destroy(this.dw_zusatz)
destroy(this.dw_umlage)
destroy(this.cb_close)
destroy(this.cb_save)
end on

event open;
string ls_umlage
long ll_count


ls_umlage = message.StringParm


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


dw_umlage.SetTransObject (SQLCA)
dw_zusatz.SetTransObject (SQLCA)

IF IsNull (ls_umlage) OR ls_umlage = "" THEN
	dw_umlage.InsertRow (0)
	dw_zusatz.InsertRow (0)
ELSE
	IF dw_umlage.Retrieve (ls_umlage) > 0 THEN
		IF dw_zusatz.Retrieve (ls_umlage) = 0 THEN
			dw_zusatz.InsertRow (0)
		END IF
	END IF
END IF

end event

event closequery;integer li_response

dw_umlage.AcceptText()
dw_zusatz.AcceptText()
if dw_umlage.ModifiedCount() + dw_zusatz.ModifiedCount() <> 0 then
	li_response = MessageBox(this.Title, &
					  "Änderungen speichern ?" , Question!, YesNo!, 2)
	IF li_response = 1 THEN	cb_save.Event clicked ()
end if

end event

type dw_zusatz from datawindow within w_pflege_beratungsprojekt
integer x = 18
integer y = 408
integer width = 2565
integer height = 332
integer taborder = 20
string title = "none"
string dataobject = "d_pflege_beratungsprojekt_zusatz"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_umlage from datawindow within w_pflege_beratungsprojekt
integer x = 18
integer y = 16
integer width = 2565
integer height = 400
integer taborder = 10
string title = "none"
string dataobject = "d_pflege_beratungsprojekt_umlage"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type cb_close from commandbutton within w_pflege_beratungsprojekt
integer x = 1294
integer y = 764
integer width = 402
integer height = 100
integer taborder = 40
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

type cb_save from commandbutton within w_pflege_beratungsprojekt
integer x = 809
integer y = 764
integer width = 402
integer height = 100
integer taborder = 30
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "&Speichern"
boolean default = true
end type

event clicked;
// Validierung

long ll_row, ll_rows
string ls_umlage
decimal {0} ldc_key
datetime ldtm_von, ldtm_bis, ldtm_temp


IF dw_umlage.Accepttext () <> 1 THEN return
IF dw_zusatz.Accepttext () <> 1 THEN return

ls_umlage = dw_umlage.GetItemString (1, "umlage")

// Validierung
IF IsNull (dw_umlage.GetItemString (1, "lang")) THEN
	MessageBox ("Fehler", "Bitte geben Sie den Namen der Kommune an!")
	return
END IF

ldtm_von = dw_umlage.GetItemDateTime (1, "von")
ldtm_bis = dw_umlage.GetItemDateTime (1, "bis")
IF IsNull (ldtm_von) THEN
	MessageBox ("Fehler", "Bitte geben Sie den Beginn des Gültigkeitszeitraums an!")
	return
END IF

IF NOT IsNull (ls_umlage) THEN
	// wenn die Umlage schon existiert, müssen wir prüfen, dass es keine Buchungen außerhalb des Gültigkeitszeitraums gibt
	SELECT MIN (tag)
	INTO :ldtm_temp
	FROM intdba.tk
	WHERE schluessel = 'b'
	   AND umlage = :ls_umlage
	   AND tag < :ldtm_von
	USING SQLCA;
	
	IF SQLCA.SQLCode <> 0 THEN
		MessageBox ("Fehler", "Gültigkeitsdatum des Umlage-Schlüssel konnte nicht geprüft werden!")
		return
	END IF
	
	IF NOT IsNull (ldtm_temp) THEN
		MessageBox ("Fehler", "Der Gültigkeitsbeginn des Mandats ist zu spät, da die früheste Buchung bereits für den " + string (ldtm_temp, "dd.mm.yyyy") + " erfasst wurde!")
		return
	END IF

	IF NOT IsNull (ldtm_bis) THEN
		SELECT MIN (tag)
		INTO :ldtm_temp
		FROM intdba.tk
		WHERE schluessel = 'b'
			AND umlage = :ls_umlage
			AND tag > :ldtm_bis
		USING SQLCA;
		
		IF SQLCA.SQLCode <> 0 THEN
			MessageBox ("Fehler", "Gültigkeitsdatum des Umlage-Schlüssel konnte nicht geprüft werden!")
			return
		END IF
		
		IF NOT IsNull (ldtm_temp) THEN
			MessageBox ("Fehler", "Das Gültigkeitsende des Mandats ist zu früh, da die späteste Buchung bereits für den " + string (ldtm_temp, "dd.mm.yyyy") + " erfasst wurde!")
			return
		END IF
	END IF
END IF


// für neue Umlage den Schlüssel generieren
IF IsNull (ls_umlage) THEN
	ls_umlage = dw_umlage.GetItemString (1, "ktr")
	IF IsNull (ls_umlage) THEN
		MessageBox ("Fehler", "Bitte wählen Sie einen Kostenträger aus!")
		return
	END IF
	
	SELECT intdba.SEQ_MANDAT_BERATUNG.NEXTVAL 
	INTO :ldc_key
	FROM DUAL
	USING SQLCA;
	
	IF SQLCA.SQLCode <> 0 THEN
		MessageBox ("Fehler", "Umlage-Schlüssel konnte nicht generiert werden!")
		return
	END IF
	
	ls_umlage = string (ldc_key, "00000") + ls_umlage
	dw_umlage.SetItem (1, "umlage", ls_umlage)
	dw_zusatz.SetItem (1, "umlage", ls_umlage)
END IF


// Speichern
IF dw_umlage.Update(TRUE, FALSE) = 1 THEN
	IF dw_zusatz.Update(TRUE, FALSE) = 1 THEN
		COMMIT USING SQLCA;
		IF SQLCA.SQLCode <> 0 THEN
			MessageBox ("Fehler", "Commit fehlgeschlagen!")
			return
		END IF
		
		dw_umlage.ResetUpdate()
		dw_zusatz.ResetUpdate()
		
		CloseWithReturn (parent, 1)
		
	ELSE
		ROLLBACK USING SQLCA;
	
		MessageBox ("Fehler", "Das Speichern ist fehlgeschlagen!")
		return
	END IF
	
ELSE
	ROLLBACK USING SQLCA;

	MessageBox ("Fehler", "Das Speichern ist fehlgeschlagen!")
	return
END IF

end event

