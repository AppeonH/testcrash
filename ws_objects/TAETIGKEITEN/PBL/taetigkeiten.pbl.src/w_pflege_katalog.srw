$PBExportHeader$w_pflege_katalog.srw
$PBExportComments$Pflege eines Katalogs
forward
global type w_pflege_katalog from window
end type
type dw_daten from datawindow within w_pflege_katalog
end type
type cb_close from commandbutton within w_pflege_katalog
end type
type cb_save from commandbutton within w_pflege_katalog
end type
end forward

global type w_pflege_katalog from window
integer width = 2080
integer height = 1656
boolean titlebar = true
string title = "Pflege Katalog"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 29347791
string icon = "AppIcon!"
boolean center = true
dw_daten dw_daten
cb_close cb_close
cb_save cb_save
end type
global w_pflege_katalog w_pflege_katalog

type variables

n_cst_pflege_katalog_attrib inv_attrib

end variables

on w_pflege_katalog.create
this.dw_daten=create dw_daten
this.cb_close=create cb_close
this.cb_save=create cb_save
this.Control[]={this.dw_daten,&
this.cb_close,&
this.cb_save}
end on

on w_pflege_katalog.destroy
destroy(this.dw_daten)
destroy(this.cb_close)
destroy(this.cb_save)
end on

event open;
long ll_count


inv_attrib = message.powerobjectparm


IF inv_attrib.is_kat_typ = "KB_LPAKET" OR inv_attrib.is_kat_typ = "KB_JOBS" THEN
	// Berechtigung (Kataloge für Kommunale Beratung)
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
END IF

this.title = inv_attrib.is_title
dw_daten.dataobject = inv_attrib.is_dataobject

dw_daten.SetTransObject (SQLCA)
dw_daten.Retrieve (inv_attrib.is_kat_typ)

end event

event closequery;integer li_response

dw_daten.AcceptText()
if dw_daten.ModifiedCount() + dw_daten.DeletedCount() <> 0 then
	li_response = MessageBox(this.Title, &
					  "Änderungen speichern ?" , Question!, YesNo!, 2)
	IF li_response = 1 THEN	cb_save.Event clicked ()
end if

end event

type dw_daten from datawindow within w_pflege_katalog
event key pbm_dwnkey
integer x = 18
integer y = 16
integer width = 2034
integer height = 1412
integer taborder = 10
string title = "none"
string dataobject = "d_pflege_katalog_nokey"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event key;
IF key = KeyUpArrow! AND keyflags = 2 THEN
	post Event buttonclicked (GetRow(), 0, this.object.__get_attribute ("b_up", TRUE))
ELSEIF key = KeyDownArrow!  AND keyflags = 2 THEN
	post Event buttonclicked (GetRow(), 0, this.object.__get_attribute ("b_down", TRUE))
END IF

end event

event buttonclicked;
long ll_max, ll_o

CHOOSE CASE lower (dwo.name)
	CASE "b_add"
		IF ActionReturnCode <= 0 THEN return

		SetItem (ActionReturnCode, "kat_typ", inv_attrib.is_kat_typ)
		
		ll_max = GetItemNumber (1, "max_order")
		IF IsNull (ll_max) THEN ll_max = 0
		SetItem (ActionReturnCode, "order", ll_max + 1)
		
	CASE "b_up"
		IF row < 2 THEN return
		
		ll_o = GetItemNumber (row - 1, "order")
		SetItem (row - 1, "order", GetItemNumber (row, "order"))
		SetItem (row, "order", ll_o)
		Sort ()
		ScrollToRow (row - 1)
		
	CASE "b_down"
		IF row < 1 or row >= RowCount() THEN return
		
		ll_o = GetItemNumber (row + 1, "order")
		SetItem (row + 1, "order", GetItemNumber (row, "order"))
		SetItem (row, "order", ll_o)
		Sort ()
		ScrollToRow (row + 1)
		
END CHOOSE

end event

type cb_close from commandbutton within w_pflege_katalog
integer x = 1083
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

type cb_save from commandbutton within w_pflege_katalog
integer x = 599
integer y = 1456
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
// Validierung

long ll_row, ll_rows
decimal {0} ldc_key


IF dw_daten.Accepttext () <> 1 THEN return

ll_rows = dw_daten.RowCount()

IF inv_attrib.ib_autokey THEN
	// fehlende Schlüssel setzen
	FOR ll_row = 1 TO ll_rows
		IF IsNull (dw_daten.GetItemString (ll_row, "kat_key")) THEN
			
			SELECT intdba.SEQ_KATALOG_KEY.NEXTVAL 
			INTO :ldc_key
			FROM DUAL
			USING SQLCA;
			
			IF SQLCA.SQLCode <> 0 THEN
				MessageBox ("Fehler", "Katalog-Schlüssel konnte nicht generiert werden!")
				return
			END IF
			
			dw_daten.SetItem (ll_row, "kat_key", String (ldc_key, "0"))
		END IF
	NEXT
END IF

ll_row = dw_daten.Find ("IsNull (kat_value) or IsNull (kat_key)", 1, ll_rows)
IF ll_row > 0 THEN
	dw_daten.ScrollToRow (ll_row)
	MessageBox ("Fehler", "Bitte erfassen Sie für alle Zeilen die Daten!")
	dw_daten.SetFocus()
END IF

// Speichern
IF dw_daten.Update() = 1 THEN
	COMMIT USING SQLCA;
	IF SQLCA.SQLCode <> 0 THEN
		MessageBox ("Fehler", "Commit fehlgeschlagen!")
		return
	END IF
ELSE
	ROLLBACK USING SQLCA;

	MessageBox ("Fehler", "Das Speichern ist fehlgeschlagen!")
	return
END IF

end event

