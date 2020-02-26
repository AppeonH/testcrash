$PBExportHeader$w_rechnungsaufteilung.srw
$PBExportComments$Aufteilung Rechnungsbetrag
forward
global type w_rechnungsaufteilung from window
end type
type cbx_check from checkbox within w_rechnungsaufteilung
end type
type dw_user from datawindow within w_rechnungsaufteilung
end type
type em_betrag from editmask within w_rechnungsaufteilung
end type
type st_betrag from statictext within w_rechnungsaufteilung
end type
type st_1 from statictext within w_rechnungsaufteilung
end type
type cb_ok from commandbutton within w_rechnungsaufteilung
end type
type cb_cancel from commandbutton within w_rechnungsaufteilung
end type
type s_logfont from structure within w_rechnungsaufteilung
end type
type s_nonclientmetrics from structure within w_rechnungsaufteilung
end type
end forward

type s_logfont from structure
	long		lfheight
	long		lfwidth
	long		lfescapement
	long		lforientation
	long		lfweight
	character		lfitalic
	character		lfunderline
	character		lfstrikeout
	character		lfcharset
	character		lfoutprecision
	character		lfclipprecision
	character		lfquality
	character		lfpitchandfamily
	character		lffacename[32]
end type

type s_nonclientmetrics from structure
	unsignedlong		cbsize
	integer		iborderwidth
	integer		iscrollwidth
	integer		iscrollheight
	integer		icaptionwidth
	integer		icaptionheight
	s_logfont		lfcaptionfont
	integer		ismcaptionwidth
	integer		ismcaptionheight
	s_logfont		lfsmcaptionfont
	integer		imenuwidth
	integer		imenuheight
	s_logfont		lfmenufont
	s_logfont		lfstatusfont
	s_logfont		lfmessagefont
	integer		ipaddedborderwidth
end type

global type w_rechnungsaufteilung from window
integer x = 201
integer y = 200
integer width = 1303
integer height = 692
boolean titlebar = true
string title = "Aufteilung Rechnungsbetrag"
windowtype windowtype = response!
long backcolor = 29347791
string icon = "AppIcon!"
event dfscl_print ( )
cbx_check cbx_check
dw_user dw_user
em_betrag em_betrag
st_betrag st_betrag
st_1 st_1
cb_ok cb_ok
cb_cancel cb_cancel
end type
global w_rechnungsaufteilung w_rechnungsaufteilung

type prototypes
Function long GetWindowLong (long hWindow, integer nIndex) Library "user32.dll" ALIAS FOR GetWindowLongA
Function long SetWindowLong (long hWindow, integer nIndex, long dwNewLong) Library "user32.dll" ALIAS FOR SetWindowLongA
FUNCTION ulong GetSystemMenu (ulong hWnd, BOOLEAN bRevert)  Library "USER32"
FUNCTION boolean InsertMenu (ulong HMenu, uint uPosition, uint uFlags, ulong uIDNewItem, string lpNewItem) LIBRARY "user32.dll" ALIAS FOR "InsertMenuA;Ansi"
FUNCTION boolean DrawMenuBar (ulong hWnd) LIBRARY "user32.dll"
FUNCTION boolean SetWindowPos (ulong hWnd, ulong hWinInsertAfter, int newx, int newy, int newwidth, int newheight, long flags) LIBRARY "user32.dll"
Function boolean SystemParametersInfoW_NC(uint wActon, uint wParam, REF s_nonclientmetrics lst_metrics, uint fUpdateProfile) Library "USER32.DLL" Alias For "SystemParametersInfoW"

end prototypes

type variables

private w_auswertung_edv iw_auswert

end variables

forward prototypes
protected function long of_getncmetrics ()
protected function integer of_make_resizeable ()
end prototypes

protected function long of_getncmetrics ();Constant uint SPI_GETNONCLIENTMETRICS = 41
s_nonclientmetrics s_ncm
uint lui_size
long ll_pixels

lui_size = 504 //344 //340
s_ncm.cbSize=lui_size

IF SystemParametersInfoW_NC (SPI_GETNONCLIENTMETRICS, lui_size, s_ncm, 0) THEN
	IF s_ncm.iborderwidth > 0 THEN
		ll_pixels = s_ncm.iborderwidth - 1
	ELSE
		ll_pixels = 0
	END IF

	RETURN ll_pixels
END IF

RETURN -1

end function

protected function integer of_make_resizeable ();//A few constants we'll need for the Windows API calls
CONSTANT int 	GWL_STYLE  			= -16

//CONSTANT ulong	WS_SYSMENU 			= 524288
CONSTANT ulong WS_THICKFRAME 		= 262144
CONSTANT ulong WS_MINIMIZEBOX		= 65536
//CONSTANT ulong WS_MAXIMIZEBOX		= 131072

CONSTANT uint 	SC_RESTORE      	= 61728
CONSTANT uint 	SC_MAXIMIZE     	= 61488
//CONSTANT uint 	SC_MINIMIZE     	= 61472
CONSTANT uint	SC_SIZE				= 61440

//CONSTANT uint 	MF_BYCOMMAND 		= 0
CONSTANT uint	MF_STRING			= 0
CONSTANT uint	MF_BYPOSITION		= 1024

CONSTANT uint	SWP_NOSIZE			= 1
CONSTANT uint	SWP_NOMOVE			= 2
CONSTANT uint	SWP_NOZORDER		= 4
CONSTANT uint	SWP_FRAMECHANGED	= 32

ulong 	lul_style, lul_hMenu, lul_hWnd, lul_newstyle
long ll_areo_border


TRY
	lul_hWnd = Handle (this)
	
	//Get the current window style
	lul_style = GetWindowLong (handle(this), gwl_style)
	
	IF this.ControlMenu THEN
		//You have to include the MINIMIZEBOX attribute to get the controls to show
		lul_newstyle = lul_style + WS_THICKFRAME + WS_MINIMIZEBOX
	ELSE
		lul_newstyle = lul_style + WS_THICKFRAME
	END IF
	
	IF lul_style <> 0 THEN
		IF SetWindowLong ( lul_hWnd, gwl_style, lul_newstyle ) <> 0 THEN
			IF this.ControlMenu THEN
				//Get a handle to the system menu
				lul_hMenu = GetSystemMenu( lul_hWnd, FALSE )
				IF lul_hMenu > 0 THEN
					InsertMenu( lul_hMenu, 1, MF_BYPOSITION + MF_STRING, SC_MAXIMIZE, "Maximieren") ;
					InsertMenu( lul_hMenu, 1, MF_BYPOSITION + MF_STRING, SC_RESTORE, "Wiederherstellen") ;
					//The Size menu option has to be added to allow the resize gripper to work
					//if there is a control menu
					InsertMenu( lul_hMenu, 1, MF_BYPOSITION + MF_STRING, SC_SIZE, "Größe ändern" ) ;
					DrawMenuBar( lul_hWnd )
				END IF
			END IF
			//Force a repaint
			SetWindowPos ( lul_hWnd, 0, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOZORDER + SWP_FRAMECHANGED )
			
			// Fenster muss durch den Rand jetzt etwas größer werden
			ll_areo_border = of_getncmetrics ()
			
			if ll_areo_border >= 0 then
				this.width += 2 * PixelsToUnits (ll_areo_border, XPixelsToUnits! )
				this.height += 2 * PixelsToUnits (ll_areo_border, YPixelsToUnits! )
			end if
			
			this.SetRedraw (TRUE)
		END IF
	END IF

CATCH (Throwable e)
	return -1
END TRY

return 1

end function

on w_rechnungsaufteilung.create
this.cbx_check=create cbx_check
this.dw_user=create dw_user
this.em_betrag=create em_betrag
this.st_betrag=create st_betrag
this.st_1=create st_1
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.Control[]={this.cbx_check,&
this.dw_user,&
this.em_betrag,&
this.st_betrag,&
this.st_1,&
this.cb_ok,&
this.cb_cancel}
end on

on w_rechnungsaufteilung.destroy
destroy(this.cbx_check)
destroy(this.dw_user)
destroy(this.em_betrag)
destroy(this.st_betrag)
destroy(this.st_1)
destroy(this.cb_ok)
destroy(this.cb_cancel)
end on

event open;
decimal {2} ldc_betrag
long ll_row, ll_rows
string ls_filter


iw_auswert = Message.PowerObjectParm

// aktuell gesetzten Betrag ermitteln
IF iw_auswert.dw_auswertung.RowCount () > 0 THEN
	ldc_betrag = iw_auswert.dw_auswertung.GetItemNumber (1, "betrag_vorgabe")
	IF IsNull (ldc_betrag) THEN ldc_betrag = 0
END IF

em_betrag.Text = String (ldc_betrag, em_betrag.Mask)


dw_user.SetTransObject (SQLCA)
ll_rows = dw_user.Retrieve (DateTime (iw_auswert.istr_auswert.id_von), &
									DateTime (iw_auswert.istr_auswert.id_bis))
IF ll_rows < 1 THEN
	MessageBox ("Achtung", "Betragsaufteilung ist nicht möglich, da keine Daten vorhanden!")
	close (this)
	return
END IF


ls_filter = iw_auswert.dw_auswertung.Describe ("DataWindow.Table.Filter")
IF ls_filter = "?" THEN
	// alle auswählen
	dw_user.SelectRow (0, TRUE)
	cbx_check.checked = TRUE

ELSE
	FOR ll_row = 1 TO ll_rows
		IF Pos (ls_filter, "'" + dw_user.GetItemString (ll_row, "username") + "'") > 0 THEN
			dw_user.SelectRow (ll_row, TRUE)
		END IF
	NEXT
	IF dw_user.GetSelectedRow (0) = 0 THEN
		cbx_check.checked = FALSE
	ELSE
		IF Long (dw_user.Describe ("Evaluate ('sum (if (isSelected(), 1, 0))', 0)")) = dw_user.RowCount () THEN
			cbx_check.checked = TRUE
		ELSE
			cbx_check.thirdstate = TRUE
		END IF
	END IF
END IF

of_make_resizeable()

end event

event resize;
long ll_wsw, ll_wsh


ll_wsw = this.workspacewidth()
ll_wsh = this.workspaceheight()

SetRedraw (FALSE)

dw_user.Resize (ll_wsw - em_betrag.width - 60, ll_wsh - dw_user.Y - 30)
em_betrag.X = ll_wsw - em_betrag.width - 30
st_betrag.X = ll_wsw - em_betrag.width - 30
cb_ok.X = em_betrag.X + 37
cb_cancel.X = cb_ok.X

SetRedraw (TRUE)

end event

type cbx_check from checkbox within w_rechnungsaufteilung
integer x = 530
integer width = 288
integer height = 72
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
long textcolor = 33554432
long backcolor = 29347791
string text = "Auswahl"
boolean threestate = true
end type

event clicked;
IF this.ThirdState THEN
	// der dritte Zustand kann nicht manuell gesetzt werden
	this.ThirdState = FALSE
	this.Checked = FALSE
END IF

IF this.Checked THEN
	dw_user.SelectRow (0, TRUE)
ELSE
	dw_user.SelectRow (0, FALSE)
END IF

end event

type dw_user from datawindow within w_rechnungsaufteilung
integer x = 5
integer y = 72
integer width = 805
integer height = 524
integer taborder = 10
string title = "none"
string dataobject = "d_auswahl_user_extern"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;
IF row > 0 THEN
	SelectRow (row, NOT isSelected (row))
END IF

IF dw_user.GetSelectedRow (0) = 0 THEN
	cbx_check.checked = FALSE
	cbx_check.thirdstate = FALSE
ELSE
	IF Long (dw_user.Describe ("Evaluate ('sum (if (isSelected(), 1, 0))', 0)")) = dw_user.RowCount () THEN
		cbx_check.thirdstate = FALSE
		cbx_check.checked = TRUE
	ELSE
		cbx_check.checked = FALSE
		cbx_check.thirdstate = TRUE
	END IF
END IF

end event

type em_betrag from editmask within w_rechnungsaufteilung
integer x = 832
integer y = 72
integer width = 439
integer height = 80
integer taborder = 20
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
long textcolor = 33554432
string text = "none"
alignment alignment = right!
integer accelerator = 114
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = decimalmask!
string mask = "###,##0.00"
end type

type st_betrag from statictext within w_rechnungsaufteilung
integer x = 837
integer y = 8
integer width = 448
integer height = 60
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
long textcolor = 33554432
long backcolor = 29347791
string text = "&Rechnungsbetrag"
boolean focusrectangle = false
end type

type st_1 from statictext within w_rechnungsaufteilung
integer x = 18
integer y = 8
integer width = 498
integer height = 56
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
long textcolor = 33554432
long backcolor = 29347791
string text = "&Filter der Mitarbeiter"
boolean focusrectangle = false
end type

type cb_ok from commandbutton within w_rechnungsaufteilung
integer x = 869
integer y = 196
integer width = 375
integer height = 88
integer taborder = 30
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "&OK"
boolean default = true
end type

event clicked;
decimal {2} ldc_betrag, ldc_last, ldc_curr
long ll_i
string ls_filter, ls_user[]
string ls_text, ls_curr, ls_last


IF em_betrag.GetData (ldc_betrag) <> 1 THEN return


IF dw_user.GetSelectedRow (0) < 1 THEN
	MessageBox ("Hinweis", "Bitte wählen Sie mindestens einen Mitarbeiter aus!")
	return
END IF

ls_user = dw_user.object.username.selected
FOR ll_i = 1 TO UpperBound (ls_user)
	IF ll_i > 1 THEN ls_filter += ", "
	ls_filter += "'" + ls_user[ll_i] + "'"
NEXT


// ausführen
iw_auswert.dw_auswertung.Modify ("betrag_vorgabe.Expression='number(~~'" + string (ldc_betrag) + "~~')'")
iw_auswert.dw_auswertung.SetFilter ("username IN (" + ls_filter + ")")
iw_auswert.dw_auswertung.Filter()
iw_auswert.dw_auswertung.GroupCalc()


// Daten in Zwischenablage zusammenfassen
ll_i = 0
ls_text = ""
ls_last = ""
ldc_last = 0
DO
	ll_i = iw_auswert.dw_auswertung.FindGroupChange (ll_i + 1, 3)
	IF ll_i < 1 THEN EXIT
	
	ldc_curr = iw_auswert.dw_auswertung.GetItemNumber (ll_i, "sum_betrag_umlage")
	ls_curr = iw_auswert.dw_auswertung.Describe ("Evaluate ('LookUpDisplay (schluessel)', " + string (ll_i) + " )")
	ls_curr += Space (15 - Len (ls_curr))
	ls_curr += iw_auswert.dw_auswertung.GetItemString (ll_i, "umlage")
	ls_curr += Space (25 - Len (ls_curr))
	
	IF ls_curr <> ls_last AND ldc_last <> 0 THEN
		ls_text += ls_last + Space (10 - Len (string (ldc_last, "#,##0.00"))) + string (ldc_last, "#,##0.00") + "~r~n"
		ldc_last = 0
	END IF
	
	ls_last = ls_curr
	ldc_last += ldc_curr
	
	// Originalbetrag verringern, um am Ende Rundungsdifferenzen aufzudecken
	ldc_betrag -= ldc_curr
LOOP WHILE TRUE

// den letzten Eintrag noch übernehmen und da die Rundungsdifferenzen dazurechnen
ls_text += ls_last + Space (10 - Len (string (ldc_last, "#,##0.00"))) + string (ldc_last + ldc_betrag, "#,##0.00") + "~r~n"

Clipboard (ls_text)

MessageBox ("Hinweis", "Die Daten wurden in die Zwischenablage kopiert.")

Close (parent)

end event

type cb_cancel from commandbutton within w_rechnungsaufteilung
integer x = 869
integer y = 320
integer width = 375
integer height = 88
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Segoe UI"
string text = "&Abbrechen"
boolean cancel = true
end type

event clicked;close(parent)

end event

