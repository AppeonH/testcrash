$PBExportHeader$w_auswertung_wf1.srw
$PBExportComments$Auswertungen WF1
forward
global type w_auswertung_wf1 from window
end type
type dw_auswertung from datawindow within w_auswertung_wf1
end type
end forward

global type w_auswertung_wf1 from window
integer x = 50
integer y = 52
integer width = 3730
integer height = 2176
boolean titlebar = true
string title = "Auswertungen"
string menuname = "m_auswertung_wf1"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 29347791
string icon = "AppIcon!"
dw_auswertung dw_auswertung
end type
global w_auswertung_wf1 w_auswertung_wf1

type variables

public n_cst_auswertwf1_attrib inv_attrib
public privatewrite boolean ib_loading = FALSE
end variables

forward prototypes
public function long of_retrieve ()
end prototypes

public function long of_retrieve ();
long ll_row, ll_i, ll_upper, ll_pos
string ls_text, ls_syntax
string ls_sql, ls_where, ls_wherekst, ls_temp
date ldt_von, ldt_bis
decimal {0} ldc_bezug
n_cst_string lnv_string
m_auswertung_wf1 lm_menu
DataWindowChild ldwc_current
datastore lds_temp
datawindow ldw_kopf

SetPointer (HourGlass!)

IF inv_attrib.is_auswertung = "d_auswert_user_wf1" THEN
	// kein SetTransObject vorher !!!
	
	// jetzt nicht schließen
	ib_loading = TRUE
	
	IF dw_auswertung.GetChild ("dw_detail", ldwc_current) = 1 THEN
		ldwc_current.SetTransObject (SQLCA)
		ll_row = ldwc_current.retrieve (inv_attrib.is_username, inv_attrib.ii_jahre, inv_attrib.ii_monate)
	END IF
	IF dw_auswertung.GetChild ("dw_sum", ldwc_current) = 1 THEN
		ldwc_current.SetTransObject (SQLCA)
		ll_row = ldwc_current.retrieve (inv_attrib.is_username, inv_attrib.ii_jahre, inv_attrib.ii_monate)
	END IF
	
	ll_upper = UpperBound (inv_attrib.ii_monate)
	FOR ll_i = 1 TO ll_upper
		IF ll_i <> 1 THEN ls_text += ", "
		ls_text += string (inv_attrib.ii_monate [ll_i], "0")
	NEXT
	ls_text += " / "
	ll_upper = UpperBound (inv_attrib.ii_jahre)
	FOR ll_i = ll_upper TO 1 STEP -1
		IF ll_i <> ll_upper THEN ls_text += ", "
		ls_text += string (inv_attrib.ii_jahre [ll_i], "0")
	NEXT
	
	dw_auswertung.object.t_username.text = inv_attrib.is_username
	dw_auswertung.object.t_zeitraum.text = ls_text

	// jetzt schließen möglich
	ib_loading = FALSE
	

ELSEIF inv_attrib.is_auswertung = "d_auswert_stunden_wf1" OR &
			inv_attrib.is_auswertung = "d_auswert_prozent_wf1" THEN
	// kein SetTransObject vorher !!!
	dw_auswertung.DataObject = dw_auswertung.DataObject  // SQL-Reset

	// temporäres DW für die Kriterien
	// DS geht nicht, da dort keine dynamische Ermittlung der Detail-Höhe möglich
	OpenUserObject (ldw_kopf)
	ldw_kopf.visible = FALSE
	ldw_kopf.DataObject = "d_auswert_wf1_kopf"
	ldw_kopf.InsertRow (0)


	// Monate
	ls_temp = ""
	ll_upper = UpperBound (inv_attrib.ii_monate)
	FOR ll_i = 1 TO ll_upper
		IF ls_temp <> "" THEN ls_temp += ", "
		ls_temp += string (inv_attrib.ii_monate [ll_i], "0")
	NEXT
	ls_text = ls_temp + " / "

	IF ls_temp <> "" THEN
		IF ls_where <> "" THEN ls_where += "~r~n AND "
		ls_where += "TO_CHAR (tag, 'MM') IN (" + ls_temp + ")"
	END IF
	
	// Jahre
	ls_temp = ""
	ll_upper = UpperBound (inv_attrib.ii_jahre)
	FOR ll_i = ll_upper TO 1 STEP -1
		IF ls_temp <> "" THEN ls_temp += ", "
		ls_temp += string (inv_attrib.ii_jahre [ll_i], "0")
	NEXT
	ls_text += ls_temp

	IF ls_temp <> "" THEN
		IF ls_where <> "" THEN ls_where += "~r~n AND "
		ls_where += "TO_CHAR (tag, 'YYYY') IN (" + ls_temp + ")"
	END IF

	ldw_kopf.SetItem (1, "zeit", ls_text)
	
	// (von-)Kostenstellen
	ls_temp = ""
	ls_text = ""
	ll_upper = UpperBound (inv_attrib.is_vonkst)
	FOR ll_i = 1 TO ll_upper
		IF ls_temp <> "" THEN ls_temp += " OR "
		ls_temp += "(REGEXP_LIKE (VON_KST, '" + inv_attrib.is_vonkst[ll_i] + "'))"
		IF ls_text <> "" THEN ls_text += ", "
		ls_text += lnv_string.of_globalreplace (inv_attrib.is_vonkst[ll_i], "%", "*")
	NEXT

	IF ls_temp <> "" THEN 
		ls_wherekst = "(" + ls_temp + ")"
	ELSE
		ls_text = "(alle)"
	END IF

	ldw_kopf.SetItem (1, "kst", ls_text)


	// Schlüssel
	ls_temp = ""
	ls_text = ""
	ll_upper = UpperBound (inv_attrib.is_schluessel)
	FOR ll_i = 1 TO ll_upper
		IF ls_temp <> "" THEN ls_temp += ", "
		ls_temp += "'" + inv_attrib.is_schluessel[ll_i] + "'"
		
		IF ls_text <> "" THEN ls_text += "/ "
		IF inv_attrib.is_schluessel[ll_i] = "f" THEN
			ls_text += "Förderprogramme "
		ELSEIF inv_attrib.is_schluessel[ll_i] = "p" THEN
			ls_text += "Projekte "
		ELSEIF inv_attrib.is_schluessel[ll_i] = "b" THEN
			ls_text += "Kommunale Beratungen "
		ELSE
			ls_text += inv_attrib.is_schluessel[ll_i] + " "
		END IF
	NEXT

	IF ls_temp <> "" THEN
		IF ls_where <> "" THEN ls_where += "~r~n AND "
		ls_where += "SCHLUESSEL IN (" + ls_temp + ")"
	END IF
	
	// Umlagen
	ls_temp = ""
	//	ls_text = ""
	ll_upper = UpperBound (inv_attrib.is_umlagen)
	FOR ll_i = 1 TO ll_upper
		IF ls_temp <> "" THEN ls_temp += ", "
		ls_temp += "'" + inv_attrib.is_umlagen[ll_i] + "'"
		IF ll_i > 1 THEN ls_text += ", "
		ls_text += inv_attrib.is_umlagen[ll_i]
	NEXT

	IF ls_temp <> "" THEN
		IF ls_where <> "" THEN ls_where += "~r~n AND "
		ls_where += "UMLAGE IN (" + ls_temp + ")"
	END IF
	IF ls_text = "" THEN ls_text = "(alle)"
	
	ldw_kopf.SetItem (1, "prog", ls_text)
	
	
	// Zusatz
	ls_temp = ""
	ls_text = ""
	ll_upper = UpperBound (inv_attrib.is_zusatz)
	FOR ll_i = 1 TO ll_upper
		IF ls_temp <> "" THEN ls_temp += ", "
		IF inv_attrib.is_zusatz[ll_i] = "(ohne)" THEN
			ls_temp += "' '"
		ELSE
			ls_temp += "'" + inv_attrib.is_zusatz[ll_i] + "'"
		END IF
		IF ls_text <> "" THEN ls_text += ", "
		ls_text += inv_attrib.is_zusatz[ll_i]
	NEXT

	IF ls_temp <> "" THEN
		IF ls_where <> "" THEN ls_where += "~r~n AND "
		ls_where += "ZUSATZ IN (" + ls_temp + ")"
	ELSE
		ls_text = "(alle)"
	END IF
	
	ldw_kopf.SetItem (1, "e3", ls_text)

	
	// Zusatz1
	ls_temp = ""
	ls_text = ""
	ll_upper = UpperBound (inv_attrib.is_zusatz1)
	FOR ll_i = 1 TO ll_upper
		IF ls_temp <> "" THEN ls_temp += ", "
		IF inv_attrib.is_zusatz1[ll_i] = "(ohne)" THEN
			ls_temp += "' '"
		ELSE
			ls_temp += "'" + inv_attrib.is_zusatz1[ll_i] + "'"
		END IF
		IF ls_text <> "" THEN ls_text += ", "
		ls_text += inv_attrib.is_zusatz1[ll_i]
	NEXT

	IF ls_temp <> "" THEN
		IF ls_where <> "" THEN ls_where += "~r~n AND "
		ls_where += "ZUSATZ1 IN (" + ls_temp + ")"
	ELSE
		ls_text = "(alle)"
	END IF
	
	ldw_kopf.SetItem (1, "e4", ls_text)

	
	// Zusatz2
	ls_temp = ""
	ls_text = ""
	ll_upper = UpperBound (inv_attrib.is_zusatz2)
	FOR ll_i = 1 TO ll_upper
		IF ls_temp <> "" THEN ls_temp += ", "
		IF inv_attrib.is_zusatz2[ll_i] = "(ohne)" THEN
			ls_temp += "' '"
		ELSE
			ls_temp += "'" + inv_attrib.is_zusatz2[ll_i] + "'"
		END IF
		IF ls_text <> "" THEN ls_text += ", "
		ls_text += inv_attrib.is_zusatz2[ll_i]
	NEXT

	IF ls_temp <> "" THEN
		IF ls_where <> "" THEN ls_where += " AND "
		ls_where += "ZUSATZ2 IN (" + ls_temp + ")"
	ELSE
		ls_text = "(alle)"
	END IF
	
	ldw_kopf.SetItem (1, "e5", ls_text)

	
	IF inv_attrib.is_auswertung = "d_auswert_prozent_wf1" THEN
		// Bezugsgröße
		ls_temp = ""
		ls_text = ""
		ll_upper = UpperBound (inv_attrib.is_bezugkst)
		FOR ll_i = 1 TO ll_upper
			IF ls_temp <> "" THEN ls_temp += " OR "
			ls_temp += "(REGEXP_LIKE (VON_KST, '" + inv_attrib.is_bezugkst[ll_i] + "'))"
			IF ls_text <> "" THEN ls_text += ", "
			ls_text += lnv_string.of_globalreplace (inv_attrib.is_bezugkst[ll_i], "%", "*")
		NEXT
	
		IF ls_temp = "" THEN ls_text = "(alle)"
	
		ldw_kopf.SetItem (1, "bezug", ls_text)

		IF ls_temp <> "" THEN
			ls_temp = "(" + ls_temp + ")"
			IF ls_where <> "" THEN ls_temp = "~r~n AND " + ls_temp
			ls_temp = ls_where + ls_temp
		END IF
		
		lds_temp = CREATE datastore
		lds_temp.DataObject = "d_auswert_prozent_wf1_bezug"
		IF ls_temp <> "" THEN		
			ls_sql = lds_temp.GetSQLSelect ()
			ll_pos = Pos (ls_sql, "1=1")
			IF ll_pos > 0 THEN
				ls_sql = Replace (ls_sql, ll_pos, 3, ls_temp)
			END IF
			lds_temp.SetSQLSelect (ls_sql)
		END IF
		lds_temp.SetTransObject (SQLCA)
		IF lds_temp.Retrieve () > 0 THEN
			ldc_bezug = lds_temp.GetItemDecimal (1, "std")
		END IF
		DESTROY lds_temp

		IF ldc_bezug = 0 OR IsNull (ldc_bezug) THEN
			CloseUserObject (ldw_kopf)
			MessageBox ("Achtung", "Die Auswertung kann nicht durchgeführt werden, da die von Ihnen angegebene Bezugsgröße den Wert 0 hat!~r~nBitte wählen Sie eine andere Bezugsgröße aus!")
			return 0
		END IF
	END IF

	// jetzt noch KST-Bedingung ergänzen
	IF ls_wherekst <> "" THEN
		IF ls_where <> "" THEN ls_where += "~r~n AND "
		ls_where += ls_wherekst
	END IF


	// Höhe des Headers anpassen
	lm_menu = this.MenuID
	IF lm_menu.m_file.m_kriterienkopf.checked THEN
		// Kriterien im Header-Band
		ll_i = Long (ldw_kopf.Describe ("Evaluate ('rowheight()', 1)"))
		ll_i += long (dw_auswertung.object.dw_kopf_header.Y)
		
		dw_auswertung.Modify ("destroy dw_kopf_detail")
		ls_temp = "dw_kopf_header"
		
	ELSE
		// Kriterien im Detail-Band
		ll_i = long (dw_auswertung.object.dw_kopf_header.Y)
		
		dw_auswertung.Modify ("destroy dw_kopf_header")
		ls_temp = "dw_kopf_detail"
	END IF
	
	dw_auswertung.object.DataWindow.Header.Height = string (ll_i)

	// jetzt Syntax des angepassten DW holen und damit DW neu erstellen
	// Grund: PB errechnet Höhe des Detail-Bereichs der Reports nicht neu,
	//        wenn Höhe des Headers angepasst wird
	ls_syntax = dw_auswertung.object.DataWindow.Syntax
	dw_auswertung.Create (ls_syntax)
	dw_auswertung.Move (0, 0)

	// Kriterien in Kopf übernehmen
	SetPointer (HourGlass!)
	IF dw_auswertung.GetChild (ls_temp, ldwc_current) = 1 THEN
		ldw_kopf.RowsCopy (1, 1, Primary!, ldwc_current, 1, Primary!)
	END IF
	
	CloseUserObject (ldw_kopf)
	

	// jetzt nicht schließen
	ib_loading = TRUE
	
	SetPointer (HourGlass!)
	IF dw_auswertung.GetChild ("dw_1", ldwc_current) = 1 THEN
		ldwc_current.SetTransObject (SQLCA)
		
		ls_sql = ldwc_current.GetSQLSelect ()
		// WHERE-Bedingung
		IF ls_where <> "" THEN		
			ll_pos = Pos (ls_sql, "1=1")
			IF ll_pos > 0 THEN
				ls_sql = Replace (ls_sql, ll_pos, 3, ls_where)
			END IF
		END IF
		// Bezugsgröße
		ll_pos = Pos (ls_sql, "0000.0000")
		IF ll_pos > 0 THEN
			ls_sql = Replace (ls_sql, ll_pos, 9, String (ldc_bezug, "0"))
		END IF
		ldwc_current.SetSQLSelect (ls_sql)
		ll_row = ldwc_current.retrieve ()
	END IF
	SetPointer (HourGlass!)
	IF dw_auswertung.GetChild ("dw_2", ldwc_current) = 1 THEN
		ldwc_current.SetTransObject (SQLCA)
		
		ls_sql = ldwc_current.GetSQLSelect ()
		// WHERE-Bedingung
		IF ls_where <> "" THEN		
			ll_pos = Pos (ls_sql, "1=1")
			IF ll_pos > 0 THEN
				ls_sql = Replace (ls_sql, ll_pos, 3, ls_where)
			END IF
		END IF		
		// Bezugsgröße
		ll_pos = Pos (ls_sql, "0000.0000")
		IF ll_pos > 0 THEN
			ls_sql = Replace (ls_sql, ll_pos, 9, String (ldc_bezug, "0"))
		END IF
		ldwc_current.SetSQLSelect (ls_sql)
		ll_row = ldwc_current.retrieve ()
	END IF
	SetPointer (HourGlass!)
	IF dw_auswertung.GetChild ("dw_3", ldwc_current) = 1 THEN
		ldwc_current.SetTransObject (SQLCA)
		
		ls_sql = ldwc_current.GetSQLSelect ()
		// WHERE-Bedingung
		IF ls_where <> "" THEN		
			ll_pos = Pos (ls_sql, "1=1")
			IF ll_pos > 0 THEN
				ls_sql = Replace (ls_sql, ll_pos, 3, ls_where)
			END IF
		END IF		
		// Bezugsgröße
		ll_pos = Pos (ls_sql, "0000.0000")
		IF ll_pos > 0 THEN
			ls_sql = Replace (ls_sql, ll_pos, 9, String (ldc_bezug, "0"))
		END IF
		ldwc_current.SetSQLSelect (ls_sql)
		ll_row = ldwc_current.retrieve ()
	END IF

	// jetzt ist Schließen wieder möglich
	ib_loading = FALSE

ELSEIF inv_attrib.is_auswertung = "d_plausi_username_wf1" THEN
	ldt_von = Date (inv_attrib.ii_jahre[1], inv_attrib.ii_monate[1], 1)
	ldt_bis = RelativeDate (ldt_von, 32)
	ldt_bis = RelativeDate (Date (Year (ldt_bis), Month (ldt_bis), 1), -1)
	
	dw_auswertung.SetTransObject (SQLCA)
	ll_row = dw_auswertung.retrieve (Datetime (ldt_von), Datetime (ldt_bis), gs_username)
	
ELSE
	dw_auswertung.SetTransObject (SQLCA)
	ll_row = dw_auswertung.retrieve (inv_attrib.ii_jahre, inv_attrib.ii_monate)
END IF

SetPointer (Arrow!)

return ll_row

end function

event open;
inv_attrib = Message.PowerObjectParm

this.title = inv_attrib.is_title
dw_auswertung.dataobject = inv_attrib.is_auswertung

MenuId.dynamic of_checkrights()

post of_retrieve ()


end event

on w_auswertung_wf1.create
if this.MenuName = "m_auswertung_wf1" then this.MenuID = create m_auswertung_wf1
this.dw_auswertung=create dw_auswertung
this.Control[]={this.dw_auswertung}
end on

on w_auswertung_wf1.destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_auswertung)
end on

event resize;
dw_auswertung.resize (this.workspacewidth(), this.workspaceheight() - 100)

end event

event closequery;
// beim Laden nicht schließen (sonst Absturz)

IF ib_loading THEN return 1

return 0

end event

type dw_auswertung from datawindow within w_auswertung_wf1
event type integer pfc_print ( )
integer width = 3685
integer height = 1988
integer taborder = 10
string title = "none"
boolean hscrollbar = true
boolean vscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event type integer pfc_print();//////////////////////////////////////////////////////////////////////////////
//	Event:			pfc_print
//	Arguments:		None
//	Returns:			Integer - 1 if it succeeds and -1 if an error occurs
//	Description:		Opens the print dialog to allow user to change print settings,
//						and then prints the DataWindow.
//////////////////////////////////////////////////////////////////////////////
//	Rev. History		Version
//						5.0			Initial version
//						5.0.01		Modified script to avoid 64K segment problem with 16bit machine code executables
//						5.0.04		Destroy local datastore prior to returning in error condition.
//						8.0			Return code of pfc_printdlg has changed for Cancel Action.  Changed
//							 		code to test on success rather than failure.
//						8.0			Set Printer chosen in print dialog
// 						10.0 		Use new overloaded function dw.Print(canceldlg, showPrintDlg) which could display Print Dialog,
//////////////////////////////////////////////////////////////////////////////
/*
 * Open Source PowerBuilder Foundation Class Libraries
 *
 * Copyright (c) 2004-2005, All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted in accordance with the GNU Lesser General
 * Public License Version 2.1, February 1999
 *
 * http://www.gnu.org/copyleft/lesser.html
 *
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals and was originally based on software copyright (c) 
 * 1996-2004 Sybase, Inc. http://www.sybase.com.  For more
 * information on the Open Source PowerBuilder Foundation Class
 * Libraries see http://pfc.codexchange.sybase.com
*/
//////////////////////////////////////////////////////////////////////////////
//boolean	lb_rowselection
integer	li_rc
long		ll_s, ll_selected[]
long		ll_selectedcount
long		ll_cnt
datastore				lds_selection

// Print selection
if this.Object.DataWindow.Print.Page.Range = "selection" then
	// Get selected count
//	lb_rowselection = IsValid (inv_RowSelect)
//	if not lb_rowselection then of_SetRowSelect (true)
//	ll_selectedcount = inv_RowSelect.of_SelectedCount (ll_selected)
//	if not lb_rowselection then of_SetRowSelect (false)
	DO
		ll_s = GetSelectedRow ( ll_s )
		IF ll_s > 0 THEN
			ll_selectedcount++
			ll_selected[ll_selectedcount] = ll_s
		END IF
	LOOP WHILE ll_s > 0

	if ll_selectedcount > 0 then
		// Create a datastore to print selected rows
		lds_selection = create datastore
		lds_selection.dataobject = this.DataObject

		// First discard any data in the dataobject
		lds_selection.Reset()

		// Copy selected rows
		for ll_cnt = 1 to ll_selectedcount
			if this.RowsCopy (ll_selected[ll_cnt], ll_selected[ll_cnt], primary!, &
				lds_selection, 2147483647, primary!) < 0 then
				destroy lds_selection
				return -1
			end if
		next

	end if
end if

// Print
if IsValid (lds_selection) then
	li_rc = lds_selection.Print (true, true)
	destroy lds_selection
else
	li_rc = this.Print (true, true)
end if

return li_rc
end event

event clicked;long ll_selected

ll_selected = this.getSelectedRow(0)
this.selectRow(ll_selected, false)

if row > 0 then
	this.selectRow(row, true)
end if

end event

event doubleclicked;
m_auswertung_wf1 lm_menu
string ls_proj, ls_schluessel
string ls_dwo


if (row > 0) and (dw_auswertung.dataobject = 'd_auswahl_projekt_fb') then
	ls_proj = getItemString(row, "umlage")
	ls_schluessel = getItemString(row, "schluessel")
	
	ls_dwo = dw_auswertung.tag
	
	// Projektauswertung nur nach Kostenstelle oder nach Mitarbeitern
	// ist abhängig von der Berechtigung oder Projektleitung
	IF GetItemString (row, "variante") = 'pd' THEN
		ls_dwo += "_detail"
	END IF
	
	dw_auswertung.dataobject = ls_dwo

	parent.MenuId.dynamic of_checkrights()

	dw_auswertung.setTransObject(sqlca)
	dw_auswertung.retrieve(ls_proj, ls_schluessel)
	
elseif dwo.name = "zeitraum" or Pos (dwo.tag, "zeitraum") > 0 then
	lm_menu = parent.Menuid
	IF lm_menu.m_file.m_zeitraum.enabled AND lm_menu.m_file.m_zeitraum.visible THEN
		lm_menu.m_file.m_zeitraum.Event clicked ()
	END IF

elseif dwo.name = "kstnr" or Pos (dwo.tag, "kstnr") > 0 then
	lm_menu = parent.Menuid
	IF lm_menu.m_file.m_kst.enabled AND lm_menu.m_file.m_kst.visible THEN
		lm_menu.m_file.m_kst.Event clicked ()
	END IF
end if


end event

